import csv
import os
import matplotlib.pyplot as plt

# Resolve data/output next to this script so it works from any cwd (README and
# bench/collect.sh invoke it as `python bench/plot_dist.py` from the repo root).
HERE = os.path.dirname(os.path.abspath(__file__))

rows = list(csv.DictReader(open(os.path.join(HERE, 'dist.csv'))))
v    = [int(r['version']) for r in rows]

def col(name):
    return [float(r[name]) / 1000 for r in rows]

fig, ax = plt.subplots(figsize=(10, 6))

ax.plot(v, col('heap_p50'),  color='steelblue',  lw=2,            label='heap p50')
ax.plot(v, col('heap_p99'),  color='steelblue',  lw=1.5, ls='--', label='heap p99')
ax.plot(v, col('heap_p999'), color='steelblue',  lw=1,   ls=':',  label='heap p99.9')
ax.fill_between(v, col('heap_p50'), col('heap_p999'), color='steelblue',  alpha=0.10)

ax.plot(v, col('stack_p50'),  color='darkorange', lw=2,            label='stack p50')
ax.plot(v, col('stack_p99'),  color='darkorange', lw=1.5, ls='--', label='stack p99')
ax.plot(v, col('stack_p999'), color='darkorange', lw=1,   ls=':',  label='stack p99.9')
ax.fill_between(v, col('stack_p50'), col('stack_p999'), color='darkorange', alpha=0.10)

ax.set(xlabel='QR version', ylabel='Latency (µs)',
       title='Latency distribution vs QR version (ECL L, 100 000 samples per path)')
ax.set_xlim(left=0)
ax.set_ylim(bottom=0)
ax.minorticks_on()
ax.grid(which='major', linestyle='-',  linewidth=0.5, alpha=0.5)
ax.grid(which='minor', linestyle='--', linewidth=0.3, alpha=0.3)
ax.legend(ncol=2)
plt.tight_layout()
plt.savefig(os.path.join(HERE, 'dist.png'), dpi=150)
print('wrote dist.png')
