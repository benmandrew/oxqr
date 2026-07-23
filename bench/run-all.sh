#!/usr/bin/env bash
# Single entry point for the quick "did my change help or hurt" dev loop:
# builds and runs every benchmark once (no core isolation, no sudo, portable
# to any dev machine including macOS) and regenerates every plot/report that
# reads from bench/'s data files.
#
# This is NOT the rigorous, publication-grade methodology used for the
# figures cited in the article: unpinned runs on a shared/boosting machine
# are not reproducible above p99 (see bench/README.md). For that, use the
# isolated-core, repeated-trial scripts instead:
#   bench/run-bench.sh / bench/run-bench-gc.sh   (Linux, needs sudo)
#   bench/collect.sh                              (runs all of the above)
#
# Usage: bench/run-all.sh
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

ts() { date -Is; }
step() { echo; echo "=== [$(ts)] $* ==="; }

step "build (release profile)"
dune build --profile release \
    bench/bench_dist.exe bench/bench.exe bench/bench_gc.exe

step "bench_dist.exe -> dist.csv"
./_build/default/bench/bench_dist.exe >bench/dist.csv &&
    echo "wrote bench/dist.csv" || echo "  (bench_dist failed)"

step "bench.exe -> throughput.txt (landmark cycles)"
./_build/default/bench/bench.exe >bench/throughput.txt 2>&1 &&
    echo "wrote bench/throughput.txt" || echo "  (bench failed)"

step "bench_gc.exe -> threshold.txt, gc_attribution.txt"
./_build/default/bench/bench_gc.exe "$repo_root/bench" ||
    echo "  (bench_gc failed)"

step "plots + reports"
python3 bench/plot_dist.py || echo "  (plot_dist failed; needs dist.csv + matplotlib)"
python3 bench/plot_ratio.py || echo "  (plot_ratio failed; needs dist.csv + matplotlib)"
python3 bench/p50_parity.py || echo "  (p50_parity failed; needs dist.csv)"

step "done"
echo "Deliverables in bench/:"
ls -o bench/dist.csv bench/dist.png bench/ratio.png bench/p50_parity.txt \
    bench/throughput.txt bench/threshold.txt bench/gc_attribution.txt 2>&1
