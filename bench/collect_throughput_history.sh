#!/usr/bin/env bash
# Per-commit throughput history across the perf-commit series (EXPERIMENTS.md
# Experiment 1, Blockers A/B/C). Builds and runs bench.exe at the baseline and
# each of the five `perf:` commits, in an isolated `git worktree` per commit
# (the primary checkout is never touched), pinned to the isolated core, for
# N trials each. Raw per-trial landmark dumps land in
# bench/throughput_trials/; bench/aggregate_throughput.py turns those into
# the commit x landmark -> median (CV%) table.
#
#   BENCH_CPU=7 nix develop --command bench/collect_throughput_history.sh [N_TRIALS]
#
# Assumes isolation is already applied (bench/setup-isolation.sh apply) --
# unlike collect.sh this is meant to be run as a standalone follow-up pass,
# so it only checks the environment rather than re-applying isolation.
#
# NOT set -e: a build/run failure on one historical commit (older commits may
# not build cleanly under the current toolchain -- see EXPERIMENTS.md's
# "Pitfall" note) must not abort the whole multi-hour run; it's reported and
# that commit is skipped.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

trials="${1:-15}"
core="${BENCH_CPU:-7}"
raw_dir="bench/throughput_trials"

# label:commit-ish, in order. "baseline" is the parent of the first perf
# commit, per EXPERIMENTS.md Experiment 1.
commits=(
    "baseline:a087189^"
    "a087189:a087189"
    "3e1a912:3e1a912"
    "6478790:6478790"
    "8549c9f:8549c9f"
    "d5155d1:d5155d1"
)

ts() { date -Is; }
step() { echo; echo "=== [$(ts)] $* ==="; }

scope() {
    sudo systemd-run --scope --quiet --slice=oxqrbench.slice \
        -p AllowedCPUs="$core" taskset -c "$core" "$@"
}

echo "=== throughput-history collection started $(ts) | core=$core | trials=$trials ==="

step "environment check (core $core)"
bench/check-env.sh "$core" || echo "  (continuing despite warnings -- see bench/README.md)"

mkdir -p "$raw_dir"
rm -f "$raw_dir"/*_trial_*.txt "$raw_dir"/*_build.log

worktree_base="$(mktemp -d)"
trap 'rm -rf "$worktree_base"' EXIT

built=()
failed=()

for entry in "${commits[@]}"; do
    label="${entry%%:*}"
    sha="${entry#*:}"
    wt="$worktree_base/$label"

    step "$label ($sha)"

    if ! git worktree add --detach --quiet "$wt" "$sha"; then
        echo "  git worktree add failed for $sha -- skipping"
        failed+=("$label")
        continue
    fi

    build_log="$raw_dir/${label}_build.log"
    if ! (cd "$wt" && dune build --profile release bench/bench.exe) >"$build_log" 2>&1; then
        echo "  BUILD FAILED for $label ($sha) -- see $build_log, skipping runs"
        echo "  (per EXPERIMENTS.md's Pitfall: if this needed changed build flags to"
        echo "   fix, note that explicitly -- it breaks cross-commit comparability)"
        git worktree remove --force "$wt"
        failed+=("$label")
        continue
    fi

    exe="$wt/_build/default/bench/bench.exe"
    for i in $(seq 1 "$trials"); do
        out="$raw_dir/${label}_trial_$(printf '%02d' "$i").txt"
        printf '  trial %2d/%s -> %s\n' "$i" "$trials" "$out"
        scope "$exe" >"$out" 2>&1
    done

    git worktree remove --force "$wt"
    built+=("$label")
done

step "done"
echo "Built and ran: ${built[*]:-<none>}"
if [ "${#failed[@]}" -gt 0 ]; then
    echo "Failed (skipped): ${failed[*]}"
fi

if [ "${#built[@]}" -ge 2 ]; then
    step "aggregate"
    python3 bench/aggregate_throughput.py "$raw_dir"
else
    echo "Fewer than 2 commits built -- skipping aggregation; inspect $raw_dir/*_build.log"
fi

echo "=== throughput-history collection finished $(ts) ==="
