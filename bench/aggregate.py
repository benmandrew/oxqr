#!/usr/bin/env python3
"""Aggregate several bench_dist trial CSVs into a median dist.csv plus a
run-to-run spread report.

Each trial CSV has one row per QR version with numeric columns (see bench_dist.ml).
For every (version, column) we take the median across trials as the published value.
For the tail columns we also report the min-max band and coefficient of variation so
the reader can see how reproducible each number actually was.

Usage: aggregate.py trial_01.csv trial_02.csv ...   (writes bench/dist.csv)
"""
import csv
import os
import statistics
import sys

TAIL_COLS = ["heap_p999", "stack_p999", "heap_p99", "stack_p99"]


def load(path):
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def main(argv):
    paths = argv[1:]
    if len(paths) < 2:
        sys.exit("need at least 2 trial CSVs; got %d" % len(paths))

    trials = [load(p) for p in paths]
    header = list(trials[0][0].keys())
    n_versions = len(trials[0])
    # Sanity: every trial must have the same shape.
    for p, t in zip(paths, trials):
        if len(t) != n_versions or list(t[0].keys()) != header:
            sys.exit("shape mismatch in %s" % p)

    out_rows = []
    for row_idx in range(n_versions):
        out = {}
        for col in header:
            vals = [float(t[row_idx][col]) for t in trials]
            out[col] = int(round(statistics.median(vals)))
        out_rows.append(out)

    out_path = os.path.join(os.path.dirname(__file__), "dist.csv")
    with open(out_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=header)
        w.writeheader()
        w.writerows(out_rows)

    # Spread report: for a handful of versions, show tail reproducibility.
    print("run-to-run spread across %d trials (ns):" % len(trials))
    hdr = f"{'ver':>4} " + " ".join(f"{c:>22}" for c in TAIL_COLS)
    print(hdr)
    print(f"{'':>4} " + " ".join(f"{'median [min..max] cv%':>22}" for _ in TAIL_COLS))
    show = {1, 5, 7, 8, 9, 11, 15, 20, 30, 40}
    for row_idx in range(n_versions):
        ver = int(trials[0][row_idx]["version"])
        if ver not in show:
            continue
        cells = []
        for col in TAIL_COLS:
            vals = [float(t[row_idx][col]) for t in trials]
            med = statistics.median(vals)
            lo, hi = min(vals), max(vals)
            cv = (statistics.pstdev(vals) / med * 100) if med else 0.0
            cells.append(f"{med:>7.0f} [{lo:.0f}..{hi:.0f}] {cv:>4.0f}%")
        print(f"{ver:>4} " + " ".join(f"{c:>22}" for c in cells))

    print()
    print("If cv%% on the tail columns is large (say >20%%), the machine is still noisy:")
    print("re-check bench/README.md setup, or raise the trial count.")


if __name__ == "__main__":
    main(sys.argv)
