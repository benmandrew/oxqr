#!/usr/bin/env python3
"""Aggregate several bench_gc trial directories into median threshold.txt /
gc_attribution.txt files plus a run-to-run spread report (mirrors aggregate.py's
role for bench_dist trials).

Each trial directory is one fresh `bench_gc.exe` process's output: a
`threshold.txt` and a `gc_attribution.txt`, each a `#`-commented header
followed by a CSV body (see bench_gc.ml). For every (version, column) we take
the median across trials as the published value, and report CV%/min-max for
the measured columns so the reader can see how reproducible each number was.

Usage: aggregate_gc.py trial_dir_1 trial_dir_2 ...
  Writes bench/threshold.txt, bench/gc_attribution.txt (medians) and
  bench/gc_spread.txt (spread report), and prints the latter to stdout.
"""
import csv
import os
import statistics
import sys

THRESHOLD_FILE = "threshold.txt"
ATTRIBUTION_FILE = "gc_attribution.txt"

# Columns whose values are measured (i.e. can vary trial-to-trial); the rest
# (version, and threshold.txt's width/buf_bytes/buf_words) are derived purely
# from the version number and are identical across trials by construction.
THRESHOLD_MEASURED = ["major_words_per_call", "minor_words_per_call", "major_collections"]
THRESHOLD_INT_COLS = {"version", "width", "buf_bytes", "buf_words", "major_collections"}

ATTRIBUTION_MEASURED = [
    "gc_rate", "tail_gc_rate",
    "p50_all", "p999_all", "p50_nogc", "p999_nogc", "p50_gc", "p999_gc",
]
ATTRIBUTION_INT_COLS = {
    "version", "p50_all", "p999_all", "p50_nogc", "p999_nogc", "p50_gc", "p999_gc",
}


def split_header_and_rows(path):
    """Return (comment_lines, [row dict, ...]) from one bench_gc report file."""
    comments = []
    body_lines = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("#"):
                comments.append(line.rstrip("\n"))
            elif line.strip():
                body_lines.append(line)
    rows = list(csv.DictReader(body_lines))
    return comments, rows


def aggregate_file(trial_dirs, filename, measured_cols):
    """Return (comments, [median row dict, ...], {version: {col: (median, cv)}})."""
    comments = []
    by_version = {}  # version -> {col: [values across trials]}
    order = []
    for d in trial_dirs:
        c, rows = split_header_and_rows(os.path.join(d, filename))
        if not comments:
            comments = c
        for row in rows:
            v = row["version"]
            if v not in by_version:
                by_version[v] = {col: [] for col in row}
                order.append(v)
            for col, val in row.items():
                by_version[v][col].append(float(val))

    median_rows = []
    spread = {}
    for v in order:
        cols = by_version[v]
        out = {}
        spread[v] = {}
        for col, vals in cols.items():
            med = statistics.median(vals)
            out[col] = med
            if col in measured_cols:
                cv = (statistics.pstdev(vals) / med * 100) if med else 0.0
                spread[v][col] = (med, min(vals), max(vals), cv)
        median_rows.append(out)
    return comments, median_rows, spread


def write_report(path, comments, rows, int_cols, float_decimals):
    if not rows:
        return
    header = list(rows[0].keys())
    with open(path, "w", encoding="utf-8") as f:
        for line in comments:
            f.write(line + "\n")
        w = csv.DictWriter(f, fieldnames=header)
        w.writeheader()
        for row in rows:
            formatted = {
                col: (
                    str(int(round(row[col])))
                    if col in int_cols
                    else f"{row[col]:.{float_decimals}f}"
                )
                for col in header
            }
            w.writerow(formatted)


def main(argv):
    trial_dirs = argv[1:]
    if len(trial_dirs) < 2:
        sys.exit("need at least 2 trial dirs; got %d" % len(trial_dirs))
    for d in trial_dirs:
        if not os.path.isdir(d):
            sys.exit("not a directory: %s" % d)

    here = os.path.dirname(os.path.abspath(__file__))
    n = len(trial_dirs)

    t_comments, t_rows, t_spread = aggregate_file(
        trial_dirs, THRESHOLD_FILE, THRESHOLD_MEASURED
    )
    t_comments.append(f"# Median across {n} trials; see gc_spread.txt for run-to-run CV%.")
    write_report(os.path.join(here, THRESHOLD_FILE), t_comments, t_rows, THRESHOLD_INT_COLS, 1)

    a_comments, a_rows, a_spread = aggregate_file(
        trial_dirs, ATTRIBUTION_FILE, ATTRIBUTION_MEASURED
    )
    a_comments.append(f"# Median across {n} trials; see gc_spread.txt for run-to-run CV%.")
    write_report(os.path.join(here, ATTRIBUTION_FILE), a_comments, a_rows, ATTRIBUTION_INT_COLS, 4)

    lines = []
    lines.append(f"run-to-run spread across {n} trials (bench_gc.exe, threshold.txt):")
    for v, cols in t_spread.items():
        for col in THRESHOLD_MEASURED:
            if col not in cols:
                continue
            med, lo, hi, cv = cols[col]
            lines.append(f"  v{v:>2} {col:<22} {med:>10.2f} [{lo:.2f}..{hi:.2f}] {cv:>5.1f}%")

    lines.append("")
    lines.append(f"run-to-run spread across {n} trials (bench_gc.exe, gc_attribution.txt):")
    for v, cols in a_spread.items():
        for col in ATTRIBUTION_MEASURED:
            if col not in cols:
                continue
            med, lo, hi, cv = cols[col]
            lines.append(f"  v{v:>2} {col:<14} {med:>12.2f} [{lo:.2f}..{hi:.2f}] {cv:>5.1f}%")

    lines.append("")
    lines.append(
        "If CV% on p999_gc/p999_nogc is still large (say >20%), the machine is "
        "still noisy: re-check bench/README.md setup, or raise the trial count."
    )

    out_text = "\n".join(lines)
    spread_path = os.path.join(here, "gc_spread.txt")
    with open(spread_path, "w") as f:
        f.write(out_text + "\n")
    print(out_text)
    print()
    print(f"wrote {os.path.join(here, THRESHOLD_FILE)}")
    print(f"wrote {os.path.join(here, ATTRIBUTION_FILE)}")
    print(f"wrote {spread_path}")


if __name__ == "__main__":
    main(sys.argv)
