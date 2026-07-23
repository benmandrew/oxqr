#!/usr/bin/env bash
# Per-commit throughput history across the perf-commit series (EXPERIMENTS.md
# Experiment 1, Blockers A/B/C). Builds and runs bench.exe at the baseline and
# each of the five `perf:` commits, in an isolated `git worktree` per commit
# (the primary checkout is never touched), pinned to the isolated core. Raw
# per-trial landmark dumps land in bench/throughput_trials/;
# bench/aggregate_throughput.py turns those into the commit x landmark ->
# median (CV%) table.
#
#   # sequential (default): N trials per commit, one commit at a time
#   BENCH_CPU=7 nix develop --command bench/collect_throughput_history.sh [N_TRIALS]
#
#   # round-robin: build every point once, then R rounds of 1 trial/point,
#   # order reshuffled each round so machine drift spreads across commits
#   # instead of aliasing onto whichever commit ran last.
#   ROUNDS=300 BENCH_CPU=7 nix develop --command bench/collect_throughput_history.sh
#
# Why only these 6 points: the 5 `perf:` commits are the ONLY commits that touch
# lib/ in the a087189^..d5155d1 window, so they already give full throughput
# attribution -- widening coverage to the docs/bench commits would re-measure
# byte-identical lib. Instead we spend the budget on rounds (statistical depth)
# plus a control point (harness-noise check).
#
# Env knobs:
#   ROUNDS           >0 => round-robin over R rounds; unset/0 => sequential (default)
#   SHUFFLE          round-robin only; 1 (default) reshuffles order each round, 0 keeps it fixed
#   CONTROL_COMMITS  space-separated "sha" or "label:sha" control points; default is one
#                    docs-only commit (4f2acd5, README-only) whose lib/ is identical to
#                    8549c9f, so its throughput MUST match 8549c9f -- any gap is harness
#                    non-determinism, not a real delta. Set CONTROL_COMMITS="" to disable.
#   BENCH_CPU        isolated core to pin to (default 7)
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
rounds="${ROUNDS:-0}"
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

# Append control point(s): docs-only commits whose lib/ is byte-identical to a
# perf commit (its "twin"), so their measured throughput must match that twin.
# A mismatch flags harness non-determinism rather than a code change -- a cheap
# built-in noise check. Default: one control (4f2acd5, README-only; twin=8549c9f).
control_default="ctl-4f2acd5:4f2acd5"
IFS=' ' read -r -a controls <<< "${CONTROL_COMMITS-$control_default}"
for c in "${controls[@]:-}"; do
    [ -z "$c" ] && continue
    case "$c" in
        *:*) commits+=("$c") ;;
        *)   commits+=("ctl-$c:$c") ;;
    esac
done

ts() { date -Is; }
step() { echo; echo "=== [$(ts)] $* ==="; }

scope() {
    sudo systemd-run --scope --quiet --slice=oxqrbench.slice \
        -p AllowedCPUs="$core" taskset -c "$core" "$@"
}

# Fail fast if the opam switch is not shared across worktrees. Each commit is
# benchmarked in a fresh `git worktree`, which starts with no _opam of its own.
# The per-worktree `dune build` only reuses prebuilt deps if OPAM_SWITCH_PREFIX
# is already exported (locked at repo root when we entered `nix develop`) and
# points at a switch where the deps are installed -- then every worktree build
# inherits it. If instead opam re-resolves per worktree, it finds no switch and
# rebuilds the ENTIRE dependency switch from scratch (~15+ min/commit), turning
# a ~10 min run into ~2 h. This is a hard error, not a skippable one: it would
# silently blow the time budget on every commit, so abort before the first.
preflight_switch_check() {
    local prefix="${OPAM_SWITCH_PREFIX:-}"
    if [ -z "$prefix" ]; then
        echo "  FAIL: OPAM_SWITCH_PREFIX is not set/exported." >&2
        echo "  Run this inside a single 'nix develop --command' (or after" >&2
        echo "  'eval \$(opam env)') so worktree builds inherit the switch." >&2
        return 1
    fi
    # landmarks is bench.exe's only non-oxqr dep (see bench/dune). If it isn't
    # already in the switch, the first worktree build installs the whole tree.
    if [ ! -d "$prefix/lib/landmarks" ]; then
        echo "  FAIL: deps not installed in switch $prefix (no lib/landmarks)." >&2
        echo "  Each worktree would rebuild the switch (~15+ min/commit)." >&2
        echo "  Fix: opam install . --deps-only --with-test   then re-run." >&2
        return 1
    fi
    echo "  OK: switch $prefix is exported with deps installed;" \
         "all 6 worktrees share it (no per-commit switch rebuild)."
}

echo "=== throughput-history collection started $(ts) | core=$core | trials=$trials ==="

step "preflight: shared opam switch"
preflight_switch_check || { echo "Aborting (see bench/README.md)." >&2; exit 1; }

step "environment check (core $core)"
bench/check-env.sh "$core" || echo "  (continuing despite warnings -- see bench/README.md)"

mkdir -p "$raw_dir"
rm -f "$raw_dir"/*_trial_*.txt "$raw_dir"/*_build.log

worktree_base="$(mktemp -d)"
trap 'rm -rf "$worktree_base"; git worktree prune 2>/dev/null' EXIT

built=()
failed=()

# Build one point in its own worktree under the release profile. On success,
# prints the exe path to stdout (nothing else goes to stdout, so callers can
# capture it); all diagnostics go to stderr. On failure, removes the worktree
# and returns nonzero. Shared by both the sequential and round-robin modes.
build_point() {
    local label="$1" sha="$2"
    local wt="$worktree_base/$label"
    if ! git worktree add --detach --quiet "$wt" "$sha"; then
        echo "  git worktree add failed for $sha -- skipping" >&2
        return 1
    fi
    local build_log="$raw_dir/${label}_build.log"
    if ! (cd "$wt" && dune build --profile release bench/bench.exe) >"$build_log" 2>&1; then
        echo "  BUILD FAILED for $label ($sha) -- see $build_log, skipping runs" >&2
        echo "  (per EXPERIMENTS.md's Pitfall: if this needed changed build flags to" >&2
        echo "   fix, note that explicitly -- it breaks cross-commit comparability)" >&2
        git worktree remove --force "$wt"
        return 1
    fi
    echo "$wt/_build/default/bench/bench.exe"
}

# Sequential mode: for each point, build it, take all N trials, tear it down,
# then move to the next. One worktree alive at a time.
run_sequential() {
    local entry label sha exe i out
    for entry in "${commits[@]}"; do
        label="${entry%%:*}"; sha="${entry#*:}"
        step "$label ($sha)"
        if ! exe="$(build_point "$label" "$sha")"; then failed+=("$label"); continue; fi
        for i in $(seq 1 "$trials"); do
            out="$raw_dir/${label}_trial_$(printf '%02d' "$i").txt"
            printf '  trial %2d/%s -> %s\n' "$i" "$trials" "$out"
            scope "$exe" >"$out" 2>&1
        done
        git worktree remove --force "$worktree_base/$label"
        built+=("$label")
    done
}

# Round-robin mode: build every point once (worktrees kept alive), then run
# R rounds of one trial per point, reshuffling the order each round so slow
# machine drift is spread evenly across commits rather than confounded with
# whichever commit happened to run last.
run_roundrobin() {
    local labels=() exes=() entry label sha exe
    step "build all points once"
    for entry in "${commits[@]}"; do
        label="${entry%%:*}"; sha="${entry#*:}"
        echo "  building $label ($sha) ..."
        if ! exe="$(build_point "$label" "$sha")"; then failed+=("$label"); continue; fi
        labels+=("$label"); exes+=("$exe"); built+=("$label")
        echo "  built $label -> $exe"
    done
    if [ "${#labels[@]}" -lt 2 ]; then return; fi

    step "round-robin: $rounds rounds x ${#labels[@]} points (1 trial/point/round)"
    local last=$(( ${#labels[@]} - 1 )) r i order out
    for r in $(seq 1 "$rounds"); do
        if [ "${SHUFFLE:-1}" = 1 ] && command -v shuf >/dev/null; then
            order=$(seq 0 "$last" | shuf)
        else
            order=$(seq 0 "$last")
        fi
        for i in $order; do
            out="$raw_dir/${labels[$i]}_trial_$(printf '%03d' "$r").txt"
            scope "${exes[$i]}" >"$out" 2>&1
        done
        printf '  round %3d/%s done\n' "$r" "$rounds"
    done

    for label in "${labels[@]}"; do
        git worktree remove --force "$worktree_base/$label"
    done
}

if [ "$rounds" -gt 0 ] 2>/dev/null; then
    echo "mode: round-robin ($rounds rounds, SHUFFLE=${SHUFFLE:-1})"
    run_roundrobin
else
    echo "mode: sequential ($trials trials/commit)"
    run_sequential
fi

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
