#!/usr/bin/env bash
# One-shot data collection for the article (REPORT.md). Runs everything on the
# isolated core, in order, and leaves the deliverables in bench/. Intended to be
# launched detached (it takes hours) from inside `nix develop` so dune + the
# OxCaml toolchain are on PATH.
#
#   BENCH_CPU=7 nix develop --command bench/collect.sh [N_TRIALS]
#
# NOT set -e: an optional step failing (plot, throughput) must not abort the
# whole multi-hour run. Each step reports its own status instead.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

trials="${1:-30}"
core="${BENCH_CPU:-7}"
export BENCH_CPU="$core"

ts() { date -Is; }
step() { echo; echo "=== [$(ts)] $* ==="; }

# Pin a standalone exe to the isolated core the same way run-bench.sh does:
# a fresh top-level slice (unconfined) capped to the core, so it can claim it
# even though system/user.slice have it removed.
scope() {
    sudo systemd-run --scope --quiet --slice=oxqrbench.slice \
        -p AllowedCPUs="$core" taskset -c "$core" "$@"
}

echo "=== OxQR collection started $(ts) | core=$core | trials=$trials ==="

# --- Re-assert isolation (idempotent; cheap insurance for a long detached run).
step "[iso] re-apply isolation on core $core"
sudo -E bench/setup-isolation.sh apply "$core" || echo "  (setup-isolation returned non-zero; continuing)"

# --- 0. Provenance (REPORT section 0) ---------------------------------------
step "[0] machine.txt"
{
    uname -a
    echo
    lscpu
    echo
    echo "cmdline: $(cat /proc/cmdline)"
    echo "governor(core $core): $(cat /sys/devices/system/cpu/cpu${core}/cpufreq/scaling_governor 2>&1)"
    echo "isolation method: cgroup AllowedCPUs (no reboot); SMT sibling offlined"
    echo "note: nohz_full NOT set (reboot-only); the single residual check-env WARN"
    echo "--- check-env ---"
    bench/check-env.sh "$core"
} >bench/machine.txt 2>&1
echo "wrote bench/machine.txt"

# --- 1. Core dataset: N trials (REPORT section 1) ---------------------------
step "[1] run-bench.sh $trials  ->  dist.csv, spread.txt, trials/"
bench/run-bench.sh "$trials" | tee bench/spread.txt
echo "run-bench exit: ${PIPESTATUS[0]}"

# --- 5. Quick-look plot (REPORT section 5) ----------------------------------
step "[5] plot_dist.py -> dist.png"
python bench/plot_dist.py || echo "  (plot failed; dist.csv still usable)"

# --- 4. Throughput / landmark cycles (REPORT section 4) ---------------------
step "[4] bench.exe landmark cycles -> throughput.txt"
dune build --profile release bench/bench.exe &&
    scope ./_build/default/bench/bench.exe >bench/throughput.txt 2>&1 &&
    echo "wrote bench/throughput.txt" || echo "  (bench.exe failed)"

# --- 3. GC attribution + threshold (REPORT section 3) -----------------------
step "[3] run-bench-gc.sh $trials -> threshold.txt, gc_attribution.txt, gc_spread.txt"
bench/run-bench-gc.sh "$trials" || echo "  (bench_gc trials failed)"

step "done"
echo "Deliverables in bench/:"
ls -o bench/machine.txt bench/dist.csv bench/spread.txt bench/dist.png \
    bench/throughput.txt bench/threshold.txt bench/gc_attribution.txt bench/gc_spread.txt 2>&1
echo "trials collected: $(ls bench/trials/trial_*.csv 2>/dev/null | wc -l)"
echo "gc trials collected: $(ls -d bench/gc_trials/trial_* 2>/dev/null | wc -l)"
echo "=== OxQR collection finished $(ts) ==="
