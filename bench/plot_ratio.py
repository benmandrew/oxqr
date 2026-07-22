import csv
import os
import matplotlib.pyplot as plt

# Figure 2 (EXPERIMENTS.md Experiment 3 / Blocker E): heap/stack ratio at p99
# and p99.9 vs version. Companion to Figure 1 (plot_dist.py -> dist.png), same
# HERE-relative-path convention so it works from any cwd.
HERE = os.path.dirname(os.path.abspath(__file__))

rows = list(csv.DictReader(open(os.path.join(HERE, 'dist.csv'))))
v = [int(r['version']) for r in rows]


def ratio(heap_col, stack_col):
    return [float(r[heap_col]) / float(r[stack_col]) for r in rows]


fig, ax = plt.subplots(figsize=(10, 6))

ax.plot(v, ratio('heap_p99', 'stack_p99'), color='steelblue', lw=2, label='p99 ratio')
ax.plot(v, ratio('heap_p999', 'stack_p999'), color='firebrick', lw=2, label='p99.9 ratio')
ax.axhline(1.0, color='gray', lw=1, ls='--', label='parity (1.0x)')

ax.set(xlabel='QR version', ylabel='heap / stack latency ratio',
       title='Heap/stack tail-latency ratio vs QR version')
ax.set_xlim(left=0)
ax.set_ylim(bottom=0)
ax.minorticks_on()
ax.grid(which='major', linestyle='-', linewidth=0.5, alpha=0.5)
ax.grid(which='minor', linestyle='--', linewidth=0.3, alpha=0.3)
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(HERE, 'ratio.png'), dpi=150)
print('wrote ratio.png')
