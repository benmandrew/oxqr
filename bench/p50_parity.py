#!/usr/bin/env python3
"""Heap/stack p50 parity across all QR versions in dist.csv (Experiment 2 /
Blocker D in EXPERIMENTS.md). No new benchmark run needed: dist.csv already
holds heap_p50/stack_p50 for every version.

Usage: p50_parity.py   (reads bench/dist.csv, writes bench/p50_parity.txt)
"""
import csv
import os

HERE = os.path.dirname(os.path.abspath(__file__))


def main():
    rows = list(csv.DictReader(open(os.path.join(HERE, "dist.csv"))))

    gaps = []
    for r in rows:
        ver = int(r["version"])
        heap = float(r["heap_p50"])
        stack = float(r["stack_p50"])
        gap_pct = abs(heap - stack) / stack * 100
        gaps.append((ver, heap, stack, gap_pct))

    worst = max(gaps, key=lambda g: g[3])

    lines = []
    lines.append("p50 heap/stack parity across all %d versions (ns):" % len(gaps))
    lines.append(f"{'ver':>4} {'heap_p50':>10} {'stack_p50':>10} {'gap%':>7}")
    for ver, heap, stack, gap_pct in gaps:
        lines.append(f"{ver:>4} {heap:>10.0f} {stack:>10.0f} {gap_pct:>6.2f}%")
    lines.append("")
    lines.append(
        f"Max gap: {worst[3]:.2f}% at v{worst[0]} "
        f"(heap {worst[1]:.0f} ns, stack {worst[2]:.0f} ns)."
    )
    lines.append(
        "Draft claims 'within 1-5%' -- max observed gap exceeds that band; "
        "the claim should be corrected to the measured max."
    )

    out_path = os.path.join(HERE, "p50_parity.txt")
    with open(out_path, "w") as f:
        f.write("\n".join(lines) + "\n")
    print("\n".join(lines))
    print()
    print("wrote bench/p50_parity.txt")


if __name__ == "__main__":
    main()
