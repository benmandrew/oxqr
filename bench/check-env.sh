#!/usr/bin/env bash
# Pre-flight check for reliable benchmarking on Linux (Ubuntu).
# Verifies the target core is isolated, the frequency is pinned, and nothing
# (including Docker) can interfere. Reports [ OK ] / [WARN] with fix hints.
#
# Usage: bench/check-env.sh [CORE]      (CORE defaults to $BENCH_CPU or the last online CPU)
# Exit status: 0 if no warnings, 1 otherwise.
set -euo pipefail

warns=0
ok() { printf '  [ OK ] %s\n' "$1"; }
warn() {
    printf '  [WARN] %s\n' "$1"
    warns=$((warns + 1))
}
have() { command -v "$1" >/dev/null 2>&1; }

# in_cpulist CORE "0-2,4,6-7" -> exit 0 if CORE is a member of the list.
in_cpulist() {
    local want="$1" list="$2" part lo hi
    IFS=',' read -ra _parts <<<"$list"
    for part in "${_parts[@]}"; do
        if [[ "$part" == *-* ]]; then
            lo="${part%-*}"
            hi="${part#*-}"
            if [ "$want" -ge "$lo" ] && [ "$want" -le "$hi" ]; then return 0; fi
        elif [ "$part" = "$want" ]; then
            return 0
        fi
    done
    return 1
}

if [ "$(uname -s)" != "Linux" ]; then
    echo "This checklist is Linux-specific (uname=$(uname -s))."
    exit 0
fi

ncpu="$(nproc --all)"
last=$((ncpu - 1))
core="${1:-${BENCH_CPU:-$last}}"
sib_file="/sys/devices/system/cpu/cpu${core}/topology/thread_siblings_list"
sibling=""
if [ -r "$sib_file" ]; then
    sibling="$(tr ',' '\n' <"$sib_file" | grep -vx "$core" | head -1 || true)"
fi

echo "Benchmark environment check (Linux)"
echo "  CPUs: ${ncpu}  |  target core: ${core}${sibling:+  (SMT sibling: ${sibling})}"
echo

# --- Frequency governor is 'performance' on the target ---------------------
gov_file="/sys/devices/system/cpu/cpu${core}/cpufreq/scaling_governor"
if [ -r "$gov_file" ]; then
    gov="$(cat "$gov_file")"
    if [ "$gov" = "performance" ]; then
        ok "Governor on core ${core} is 'performance'."
    else
        warn "Governor on core ${core} is '${gov}', not 'performance'. Run: sudo bench/setup-isolation.sh apply ${core}"
    fi
else
    warn "No cpufreq for core ${core} (VM without governor control?). Frequency may drift."
fi

# --- Turbo / boost disabled (fixes the clock) ------------------------------
if [ -r /sys/devices/system/cpu/intel_pstate/no_turbo ]; then
    if [ "$(cat /sys/devices/system/cpu/intel_pstate/no_turbo)" = "1" ]; then
        ok "Intel turbo disabled."
    else
        warn "Intel turbo is ON — clock will vary. setup-isolation.sh disables it."
    fi
elif [ -r /sys/devices/system/cpu/cpufreq/boost ]; then
    if [ "$(cat /sys/devices/system/cpu/cpufreq/boost)" = "0" ]; then
        ok "CPU boost disabled."
    else
        warn "CPU boost is ON — clock will vary. setup-isolation.sh disables it."
    fi
else
    ok "No turbo/boost knob exposed (nothing to disable)."
fi

# --- SMT sibling offline (no hyperthread contention on the physical core) ---
if [ -n "$sibling" ]; then
    online_file="/sys/devices/system/cpu/cpu${sibling}/online"
    if [ -r "$online_file" ] && [ "$(cat "$online_file")" = "0" ]; then
        ok "SMT sibling (core ${sibling}) is offline."
    else
        warn "SMT sibling core ${sibling} is online — it shares the physical core and adds jitter. setup-isolation.sh offlines it."
    fi
else
    ok "No SMT sibling for core ${core} (SMT off or single-thread core)."
fi

# --- Scheduler isolation: isolcpus OR cgroup confinement -------------------
cmdline="$(cat /proc/cmdline)"
isolated_boot=0
case " $cmdline " in
*isolcpus=*)
    if tr ' ' '\n' <<<"$cmdline" | grep -oE 'isolcpus=[^ ]*' | grep -qE "(^|[=,])${core}([,]|$)"; then
        isolated_boot=1
    fi
    ;;
esac
eff="/sys/fs/cgroup/system.slice/cpuset.cpus.effective"
cgroup_confined=0
if [ -r "$eff" ]; then
    # Confined if the target core is NOT in system.slice's effective cpu set.
    if ! in_cpulist "$core" "$(cat "$eff")"; then
        cgroup_confined=1
    fi
fi
if [ "$isolated_boot" = "1" ]; then
    ok "Core ${core} is isolated via isolcpus (kernel scheduler won't place tasks here)."
elif [ "$cgroup_confined" = "1" ]; then
    ok "Core ${core} is freed via cgroup: system.slice (and Docker under it) can't use it."
else
    warn "Core ${core} is NOT isolated. Other processes/containers may run on it. Run: sudo bench/setup-isolation.sh apply ${core}  (or add isolcpus=${core} to GRUB)"
fi

# --- nohz_full / rcu_nocbs (tickless — removes the 1000 Hz scheduler tick) --
if grep -qE "nohz_full=[^ ]*(^|[=,])${core}([,]|$)" /proc/cmdline; then
    ok "Core ${core} is tickless (nohz_full)."
else
    warn "Core ${core} not in nohz_full — the periodic scheduler tick still fires here. Add nohz_full=${core} rcu_nocbs=${core} to GRUB for the cleanest tail."
fi

# --- irqbalance off + no IRQs targeting the core ---------------------------
if have systemctl && systemctl is-active --quiet irqbalance 2>/dev/null; then
    warn "irqbalance is running — it will route interrupts onto core ${core}. setup-isolation.sh stops it and moves IRQs away."
else
    ok "irqbalance not active."
fi

# --- Docker present? remind about cpuset ----------------------------------
if have docker && docker info >/dev/null 2>&1; then
    if [ "$isolated_boot" = "1" ] || [ "$cgroup_confined" = "1" ]; then
        ok "Docker present, but core ${core} is protected from it."
    else
        warn "Docker is running and core ${core} is unprotected. Confine containers (see README) or apply isolation."
    fi
fi

# --- ASLR (optional determinism) ------------------------------------------
if [ -r /proc/sys/kernel/randomize_va_space ] && [ "$(cat /proc/sys/kernel/randomize_va_space)" != "0" ]; then
    printf '  [info] ASLR on (fine to leave; disable with sysctl kernel.randomize_va_space=0 for byte-identical layout).\n'
fi

echo
if [ "$warns" -eq 0 ]; then
    echo "Ready. Run: BENCH_CPU=${core} bench/run-bench.sh 15"
    exit 0
else
    echo "$warns warning(s). Fix with: sudo bench/setup-isolation.sh apply ${core}   (then re-check). See bench/README.md."
    exit 1
fi
