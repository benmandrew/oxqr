#!/usr/bin/env bash
# Prepare / restore a Linux host for reliable single-core benchmarking.
#
#   sudo bench/setup-isolation.sh apply   [CORE]
#   sudo bench/setup-isolation.sh restore [CORE]
#        bench/setup-isolation.sh apply --dry-run [CORE]   # preview, no root, no writes
#
# CORE defaults to $BENCH_CPU or the last online CPU.
# --dry-run (-n) prints every change it would make and touches nothing (so it
# needs no root); run it first on a live server to see the exact writes.
#
# "apply" does everything that does NOT need a reboot:
#   1. pins the target core's frequency governor to 'performance'
#   2. disables turbo/boost (fixes the clock)
#   3. offlines the target core's SMT sibling (kills hyperthread contention)
#   4. stops irqbalance and steers IRQs off the target core
#   5. cgroup v2: shrinks system.slice/user.slice/init.scope to every core
#      EXCEPT the target, so all services *and Docker containers* are evicted
#      from it (containers inherit the slice's cpuset; effective cpus are
#      capped by the parent, so even --cpuset-cpus can't re-enter the core).
# It then prints the one reboot-only step (isolcpus/nohz_full) for maximum
# isolation. "restore" undoes 1-5.
set -euo pipefail

# --- Argument parsing (flag may appear anywhere) ---------------------------
DRY=0
_args=()
for a in "$@"; do
    case "$a" in
    --dry-run | -n) DRY=1 ;;
    *) _args+=("$a") ;;
    esac
done
set -- ${_args[@]+"${_args[@]}"}

[ "$(uname -s)" = "Linux" ] || {
    echo "Linux only." >&2
    exit 1
}
if [ "$DRY" != "1" ] && [ "$(id -u)" != "0" ]; then
    echo "Run as root: sudo $0 $*   (or add --dry-run to preview without root)" >&2
    exit 1
fi

action="${1:-}"
ncpu="$(nproc --all)"
last=$((ncpu - 1))
core="${2:-${BENCH_CPU:-$last}}"

sib_file="/sys/devices/system/cpu/cpu${core}/topology/thread_siblings_list"
sibling=""
[ -r "$sib_file" ] && sibling="$(tr ',' '\n' <"$sib_file" | grep -vx "$core" | head -1 || true)"

# --- Mutation primitives (honour DRY) --------------------------------------
# write_file VALUE FILE — write to a sysfs/procfs node, or print the intent.
write_file() {
    local val="$1" file="$2"
    if [ "$DRY" = "1" ]; then
        printf '  [dry-run] echo %s > %s\n' "$val" "$file"
        return 0
    fi
    if [ -w "$file" ]; then
        echo "$val" >"$file" 2>/dev/null || echo "  (write failed: $file)"
    else
        echo "  (not writable, skipped: $file)"
    fi
}

# run CMD... — execute a command, or print the intent.
run() {
    if [ "$DRY" = "1" ]; then
        printf '  [dry-run] %s\n' "$*"
        return 0
    fi
    "$@"
}

# Comma list of every online CPU except the target (and its offlined sibling).
others_list() {
    local c out=()
    for c in $(seq 0 "$last"); do
        [ "$c" = "$core" ] && continue
        [ -n "$sibling" ] && [ "$c" = "$sibling" ] && continue
        out+=("$c")
    done
    (
        IFS=','
        echo "${out[*]}"
    )
}

cgroup_v2() { [ "$(stat -fc %T /sys/fs/cgroup 2>/dev/null)" = "cgroup2fs" ]; }

set_governor() {
    local g="$1" f
    for f in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; do
        [ -e "$f" ] || continue
        write_file "$g" "$f"
    done
}

set_turbo() {
    # $1 = "off" | "on"
    local want="$1" val
    if [ -e /sys/devices/system/cpu/intel_pstate/no_turbo ]; then
        [ "$want" = "off" ] && val=1 || val=0
        write_file "$val" /sys/devices/system/cpu/intel_pstate/no_turbo
    elif [ -e /sys/devices/system/cpu/cpufreq/boost ]; then
        [ "$want" = "off" ] && val=0 || val=1
        write_file "$val" /sys/devices/system/cpu/cpufreq/boost
    else
        echo "  (no turbo/boost knob on this host)"
    fi
}

set_sibling_online() {
    local state="$1" f="/sys/devices/system/cpu/cpu${sibling}/online"
    [ -n "$sibling" ] || return 0
    [ -e "$f" ] || return 0
    write_file "$state" "$f"
}

steer_irqs() {
    # Best-effort: set each IRQ's affinity to the 'others' set. Some IRQs are
    # pinned by the kernel and will refuse the write; that's expected.
    local others="$1" irq count=0
    if [ "$DRY" = "1" ]; then
        for irq in /proc/irq/*/smp_affinity_list; do [ -e "$irq" ] && count=$((count + 1)); done
        printf '  [dry-run] echo %s > /proc/irq/*/smp_affinity_list  (%d IRQs, best-effort)\n' "$others" "$count"
        return 0
    fi
    for irq in /proc/irq/*/smp_affinity_list; do
        [ -e "$irq" ] || continue
        echo "$others" >"$irq" 2>/dev/null || true
    done
}

confine_slices() {
    # cgroup v2: cap the top-level slices to $1 (the 'others' set), or reset to
    # the full range with $1="0-$last".
    local cpus="$1" slice
    cgroup_v2 || {
        echo "  (cgroup v1 or no unified hierarchy — skipping cgroup confinement; rely on isolcpus instead.)"
        return 0
    }
    for slice in system.slice user.slice init.scope; do
        run systemctl set-property --runtime "$slice" AllowedCPUs="$cpus"
    done
}

others="$(others_list)"
[ "$DRY" = "1" ] && echo "== DRY RUN: no changes will be made =="

case "$action" in
apply)
    echo "Isolating core ${core}${sibling:+ (SMT sibling ${sibling})} on a ${ncpu}-CPU host."
    echo "1. governor -> performance"
    set_governor performance
    echo "2. turbo/boost -> off"
    set_turbo off
    echo "3. offline SMT sibling ${sibling:-<none>}"
    set_sibling_online 0
    echo "4. stop irqbalance + steer IRQs to {${others}}"
    if systemctl list-unit-files irqbalance.service >/dev/null 2>&1 &&
        [ -n "$(systemctl list-unit-files irqbalance.service 2>/dev/null | grep -E '^irqbalance\.service')" ]; then
        run systemctl stop irqbalance
        run systemctl mask irqbalance
    else
        echo "  (irqbalance not installed — nothing to stop)"
    fi
    steer_irqs "$others"
    echo "5. evict services + Docker from core ${core} (cgroup AllowedCPUs={${others}})"
    confine_slices "$others"
    echo
    echo "Runtime isolation applied (resets on reboot). For MAXIMUM isolation,"
    echo "add this to /etc/default/grub GRUB_CMDLINE_LINUX_DEFAULT, then"
    echo "'sudo update-grub && sudo reboot':"
    echo
    echo "    isolcpus=${core}${sibling:+,${sibling}} nohz_full=${core}${sibling:+,${sibling}} rcu_nocbs=${core}${sibling:+,${sibling}}"
    echo
    echo "Verify with: bench/check-env.sh ${core}"
    ;;
restore)
    echo "Restoring core ${core} and host defaults."
    echo "1. governor -> schedutil (fallback ondemand)"
    if grep -q schedutil "/sys/devices/system/cpu/cpu${core}/cpufreq/scaling_available_governors" 2>/dev/null; then
        set_governor schedutil
    else
        set_governor ondemand
    fi
    echo "2. turbo/boost -> on"
    set_turbo on
    echo "3. online SMT sibling ${sibling:-<none>}"
    set_sibling_online 1
    echo "4. re-enable irqbalance"
    if systemctl list-unit-files irqbalance.service >/dev/null 2>&1 &&
        [ -n "$(systemctl list-unit-files irqbalance.service 2>/dev/null | grep -E '^irqbalance\.service')" ]; then
        run systemctl unmask irqbalance
        run systemctl start irqbalance
    else
        echo "  (irqbalance not installed — nothing to re-enable)"
    fi
    echo "5. reset slice AllowedCPUs to 0-${last}"
    confine_slices "0-${last}"
    echo
    echo "Note: isolcpus/nohz_full (if you added them to GRUB) persist until you"
    echo "remove them from /etc/default/grub and 'sudo update-grub && sudo reboot'."
    ;;
*)
    echo "usage: sudo $0 {apply|restore} [--dry-run] [CORE]" >&2
    exit 1
    ;;
esac
