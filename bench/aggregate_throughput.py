#!/usr/bin/env python3
"""Aggregate the raw per-commit landmark dumps produced by
collect_throughput_history.sh into a commit x landmark -> median cycles (CV%)
table (EXPERIMENTS.md Experiment 1 / Blockers A, B, C).

Each trial file is the combined stdout+stderr of one bench.exe run: it
contains a `Landmark.Graph.output` dump whose "Aggregated table" section is
semicolon-delimited, e.g.:

    Name;           Filename;    Calls;     Time; Allocated bytes; ...
    generate_qr;  bench/bench.ml:11;   120000;    3.06G; 272958352; ...

"Time" is human-formatted with a K/M/G suffix (3 significant figures); that
precision loss is negligible next to run-to-run CV. The landmark *set* is not
assumed to be stable across commits -- e.g. `place_data` and
`apply_mask_pattern` are separate landmarks before commit 6478790 and merge
into `place_data_and_apply_mask` from 6478790 onward. Every landmark name
seen anywhere is reported for every commit (absent = blank cell).

Usage: aggregate_throughput.py THROUGHPUT_TRIALS_DIR
  Writes bench/throughput_history.csv (long format) and
  bench/throughput_history.txt (pivoted human-readable table + summary),
  and prints the latter to stdout.

Expects trial files named "<label>_trial_NN.txt" as written by
collect_throughput_history.sh, where <label> is one of the commit labels in
that script's `commits` array (in order: baseline, then each perf commit).
"""
import csv
import os
import re
import statistics
import sys
from collections import defaultdict

# Must match the `commits` array in collect_throughput_history.sh, in order.
COMMIT_LABELS = ["baseline", "a087189", "3e1a912", "6478790", "8549c9f", "d5155d1"]

# Docs-only commits whose lib/ is byte-identical to an earlier perf commit
# (see collect_throughput_history.sh's CONTROL_COMMITS default) -- their
# measured throughput MUST match that twin; any gap is harness
# non-determinism, not a code change. Update if the shell script's default
# control point changes.
CONTROL_TWINS = {"ctl-4f2acd5": "8549c9f"}

SUFFIX = {"": 1, "K": 1e3, "M": 1e6, "G": 1e9}
TIME_RE = re.compile(r"^([\d.]+)([KMG]?)$")

# ANSI colour codes appear in the call-graph section (not the aggregated
# table) but strip them defensively in case a trial captured stdout too.
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")


def parse_time(s):
    m = TIME_RE.match(s.strip())
    if not m:
        return None
    value, suffix = m.groups()
    return float(value) * SUFFIX[suffix]


def parse_trial(path):
    """Return {landmark_name: cycles} from one bench.exe trial dump."""
    text = ANSI_RE.sub("", open(path, encoding="utf-8", errors="replace").read())
    lines = text.splitlines()
    out = {}
    in_table = False
    for line in lines:
        if line.strip().startswith("Name;") and "Time" in line:
            in_table = True
            continue
        if not in_table:
            continue
        if not line.strip() or ";" not in line:
            break
        cols = [c.strip() for c in line.split(";")]
        if len(cols) < 4:
            continue
        name, time_s = cols[0], cols[3]
        if name in ("ROOT",):
            continue
        cycles = parse_time(time_s)
        if cycles is not None:
            out[name] = cycles
    return out


def collect(trial_dir):
    """Return {label: {landmark: [cycles, ...]}}."""
    by_label = defaultdict(lambda: defaultdict(list))
    for fname in sorted(os.listdir(trial_dir)):
        m = re.match(r"^(.*)_trial_\d+\.txt$", fname)
        if not m:
            continue
        label = m.group(1)
        parsed = parse_trial(os.path.join(trial_dir, fname))
        if not parsed:
            print(f"warning: no landmark rows parsed from {fname}", file=sys.stderr)
            continue
        for name, cycles in parsed.items():
            by_label[label][name].append(cycles)
    return by_label


def median_cv(values):
    med = statistics.median(values)
    cv = (statistics.pstdev(values) / med * 100) if med else 0.0
    return med, cv


def main(argv):
    if len(argv) != 2:
        sys.exit("usage: aggregate_throughput.py THROUGHPUT_TRIALS_DIR")
    trial_dir = argv[1]
    by_label = collect(trial_dir)
    if not by_label:
        sys.exit(f"no trial files (*_trial_NN.txt) found in {trial_dir}")

    extra_labels = sorted(l for l in by_label if l not in COMMIT_LABELS)
    labels = [l for l in COMMIT_LABELS if l in by_label] + extra_labels
    missing = [l for l in COMMIT_LABELS if l not in by_label]
    landmarks = sorted({name for d in by_label.values() for name in d})

    here = os.path.dirname(os.path.abspath(__file__))

    # Long-format CSV: one row per (commit, landmark).
    csv_path = os.path.join(here, "throughput_history.csv")
    with open(csv_path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["commit", "landmark", "n_trials", "median_cycles", "cv_pct"])
        for label in labels:
            for name in landmarks:
                vals = by_label[label].get(name)
                if not vals:
                    continue
                med, cv = median_cv(vals)
                w.writerow([label, name, len(vals), f"{med:.0f}", f"{cv:.1f}"])

    lines = []
    lines.append(f"Per-commit landmark cycles, {len(labels)} commit(s), from {trial_dir}")
    if missing:
        lines.append(f"MISSING (no trial files found): {', '.join(missing)}")
    lines.append("")

    # Pivoted table: rows = landmark, cols = commit label.
    col_w = max(12, max((len(l) for l in labels), default=0) + 2)
    header = f"{'landmark':<32}" + "".join(f"{l:>{col_w}}" for l in labels)
    lines.append(header)
    for name in landmarks:
        row = f"{name:<32}"
        for label in labels:
            vals = by_label[label].get(name)
            if vals:
                med, cv = median_cv(vals)
                cell = f"{med/1e6:.0f}M({cv:.0f}%)"
            else:
                cell = "-"
            row += f"{cell:>{col_w}}"
        lines.append(row)
    lines.append("")

    # generate_qr cumulative story (Blockers B, C).
    def gq(label):
        vals = by_label.get(label, {}).get("generate_qr")
        return median_cv(vals) if vals else None

    base = gq("baseline")
    end = gq("8549c9f")
    d5 = gq("d5155d1")
    if base and end:
        ratio = base[0] / end[0]
        lines.append(
            f"generate_qr: baseline {base[0]/1e9:.2f}G ({base[1]:.0f}% CV) -> "
            f"8549c9f {end[0]/1e9:.2f}G ({end[1]:.0f}% CV), cumulative {ratio:.2f}x"
        )
        lines.append(
            f"  draft asserts ~4.03x -- {'CONFIRMED' if abs(ratio - 4.03) / 4.03 < 0.05 else 'MISMATCH, re-check'}"
        )
    else:
        lines.append("generate_qr: baseline/8549c9f data missing, cannot compute cumulative ratio")

    if end and d5:
        delta = end[0] / d5[0]
        cum_with_d5 = (base[0] / d5[0]) if base else None
        lines.append(
            f"generate_qr: 8549c9f {end[0]/1e9:.2f}G -> d5155d1 {d5[0]/1e9:.2f}G, "
            f"delta {delta:.2f}x"
        )
        if cum_with_d5:
            lines.append(f"  cumulative baseline->d5155d1 if folded in: {cum_with_d5:.2f}x")
    else:
        lines.append("generate_qr: 8549c9f/d5155d1 data missing, cannot compute d5155d1 delta")

    lines.append("")
    for ctl_label, twin in CONTROL_TWINS.items():
        if ctl_label not in by_label:
            continue
        if twin not in by_label:
            lines.append(f"control {ctl_label} (twin {twin}): twin data missing, cannot check")
            continue
        shared = sorted(set(by_label[ctl_label]) & set(by_label[twin]))
        gaps = []
        for name in shared:
            m_ctl, _ = median_cv(by_label[ctl_label][name])
            m_twin, _ = median_cv(by_label[twin][name])
            gap_pct = abs(m_ctl - m_twin) / m_twin * 100 if m_twin else 0.0
            gaps.append((name, m_ctl, m_twin, gap_pct))
        lines.append(f"control check: {ctl_label} vs twin {twin} (byte-identical lib/, must match)")
        if gaps:
            worst = max(gaps, key=lambda g: g[3])
            lines.append(
                f"  max landmark gap: {worst[0]} {worst[3]:.2f}% "
                f"({ctl_label}={worst[1]/1e6:.1f}M, {twin}={worst[2]/1e6:.1f}M)"
            )
            verdict = (
                "OK (harness noise looks clean)"
                if worst[3] < 3.0
                else "WARN -- gap exceeds 3%, treat cross-commit deltas near this size with suspicion"
            )
            lines.append(f"  {verdict}")
        else:
            lines.append("  no shared landmarks to compare")

    lines.append("")
    lines.append(
        "Landmark identity is NOT stable across commits (e.g. place_data + "
        "apply_mask_pattern merge into place_data_and_apply_mask at 6478790) "
        "-- diff the pivoted table above manually against each commit message "
        "to compute the per-commit 'landmark(s) targeted' ratios EXPERIMENTS.md "
        "asks for; this script deliberately does not guess a merge mapping."
    )

    out_text = "\n".join(lines)
    txt_path = os.path.join(here, "throughput_history.txt")
    with open(txt_path, "w") as f:
        f.write(out_text + "\n")
    print(out_text)
    print()
    print(f"wrote {csv_path}")
    print(f"wrote {txt_path}")


if __name__ == "__main__":
    main(sys.argv)
