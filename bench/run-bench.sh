#!/usr/bin/env bash
# Run the latency-distribution benchmark N times (each a fresh process),
# pinned to an isolated core, then aggregate to a median dist.csv + spread
# report.
#
# Usage:  BENCH_CPU=<core> bench/run-bench.sh [N_TRIALS]     (default 15)
#   or:   bench/run-bench.sh [N_TRIALS] [CORE]
#
# Set BENCH_RT=1 to run each trial under SCHED_FIFO (chrt) for the lowest
# preemption on the isolated core (needs root or CAP_SYS_NICE).
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

trials="${1:-15}"
ncpu="$(nproc --all)"
core="${2:-${BENCH_CPU:-$((ncpu - 1))}}"
exe="_build/default/bench/bench_dist.exe"
trial_dir="bench/trials"

command -v taskset >/dev/null || {
    echo "taskset not found (install util-linux)." >&2
    exit 1
}

echo "== environment check (core ${core}) =="
if ! bench/check-env.sh "$core"; then
    echo
    echo "Environment not fully isolated (warnings above). Continuing in 5s; Ctrl-C to abort."
    sleep 5 || true
fi

echo
echo "== build (release profile) =="
dune build --profile release bench/bench_dist.exe

mkdir -p "$trial_dir"
rm -f "$trial_dir"/trial_*.csv

# Pin to the isolated core; optionally raise to real-time priority.
pin=(taskset -c "$core")
if [ "${BENCH_RT:-0}" = "1" ]; then
    if command -v chrt >/dev/null; then
        pin=(chrt -f 1 taskset -c "$core")
    else
        echo "BENCH_RT=1 but chrt missing; falling back to taskset only."
    fi
fi

# Under the no-reboot cgroup-isolation method (setup-isolation.sh), core $core is
# removed from system.slice/user.slice, so a bare `taskset -c $core` from this
# shell fails with EINVAL: the cgroup's effective cpuset is intersected with the
# affinity mask, and $core is no longer in it. Launch each trial in a fresh
# top-level slice instead — a new slice is unconfined, so its effective CPU set
# still includes $core — and cap it to the core with AllowedCPUs. This also works
# under the isolcpus method. The system-level scope needs root.
if command -v systemd-run >/dev/null; then
    sudo_cmd=()
    [ "$(id -u)" = "0" ] || sudo_cmd=(sudo)
    runner=("${sudo_cmd[@]}" systemd-run --scope --quiet \
        --slice=oxqrbench.slice -p AllowedCPUs="$core" "${pin[@]}")
else
    echo "systemd-run not found; using bare ${pin[*]} (valid only under isolcpus, not cgroup isolation)."
    runner=("${pin[@]}")
fi

echo
echo "== running $trials trials on core $core (${runner[*]} $exe) =="
for i in $(seq 1 "$trials"); do
    out="$trial_dir/trial_$(printf '%02d' "$i").csv"
    printf 'trial %2d/%s -> %s\n' "$i" "$trials" "$out"
    "${runner[@]}" "$exe" >"$out"
done

echo
echo "== aggregate =="
python3 bench/aggregate.py "$trial_dir"/trial_*.csv

echo
echo "Wrote bench/dist.csv (median across $trials trials on isolated core $core)."
echo "Next: python bench/plot_dist.py"
