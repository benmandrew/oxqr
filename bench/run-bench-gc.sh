#!/usr/bin/env bash
# Run the GC-attribution/threshold benchmark (bench_gc.exe) N times (each a
# fresh process), pinned to an isolated core, then aggregate to median
# threshold.txt/gc_attribution.txt plus a run-to-run spread report. Mirrors
# run-bench.sh's repeat-trial methodology (used for bench_dist.exe) so
# threshold.txt/gc_attribution.txt get the same reproducibility evidence
# dist.csv already has, instead of being a single unrepeated run.
#
# Usage:  BENCH_CPU=<core> bench/run-bench-gc.sh [N_TRIALS]     (default 15)
#   or:   bench/run-bench-gc.sh [N_TRIALS] [CORE]
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

trials="${1:-15}"
ncpu="$(nproc --all)"
core="${2:-${BENCH_CPU:-$((ncpu - 1))}}"
exe="_build/default/bench/bench_gc.exe"
trial_dir="bench/gc_trials"

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
dune build --profile release bench/bench_gc.exe

rm -rf "$trial_dir"
mkdir -p "$trial_dir"

# Pin to the isolated core; optionally raise to real-time priority. Same
# rationale as run-bench.sh: under cgroup-isolation, launch each trial in a
# fresh top-level slice via systemd-run so its effective CPU set still
# includes the isolated core.
pin=(taskset -c "$core")
if [ "${BENCH_RT:-0}" = "1" ]; then
    if command -v chrt >/dev/null; then
        pin=(chrt -f 1 taskset -c "$core")
    else
        echo "BENCH_RT=1 but chrt missing; falling back to taskset only."
    fi
fi

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
    out="$trial_dir/trial_$(printf '%02d' "$i")"
    mkdir -p "$out"
    printf 'trial %2d/%s -> %s\n' "$i" "$trials" "$out"
    "${runner[@]}" "$exe" "$repo_root/$out" >"$out/run.log" 2>&1
done

echo
echo "== aggregate =="
python3 bench/aggregate_gc.py "$trial_dir"/trial_*

echo
echo "Wrote bench/threshold.txt, bench/gc_attribution.txt (median across $trials trials on isolated core $core)."
echo "Run-to-run spread: bench/gc_spread.txt"
