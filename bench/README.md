# Benchmarking OxQR (Linux)

This directory holds the throughput and latency benchmarks. **Tail-latency
numbers (p99, p99.9) are only meaningful on an isolated core, at a pinned
frequency, with every published figure taken as the median of several runs.** A
single run on a shared, boosting host is not reproducible — we measured p99.9
swinging 1.5–3× between otherwise-identical runs before isolation.

The host here also runs other services in Docker, so the setup below does two
jobs: it pins the benchmark to one core **and** evicts everything else —
including the containers — from that core.

## TL;DR

```bash
export BENCH_CPU=7                            # the core to dedicate (see "Choosing a core")
bench/setup-isolation.sh apply --dry-run 7   # preview every change (no root, no writes)
sudo bench/setup-isolation.sh apply 7        # isolate core 7, evict services + Docker
bench/check-env.sh 7                           # confirm it's clean
bench/run-bench.sh 15                           # 15 pinned trials -> dist.csv + spread report
python bench/plot_dist.py                       # Figure 1: p50/p99/p99.9 vs version
python bench/plot_ratio.py                      # Figure 2: heap/stack ratio vs version
python bench/p50_parity.py                      # p50 heap/stack gap report (no run needed once dist.csv exists)
sudo bench/setup-isolation.sh restore 7      # give the core back when done
```

On a server that's serving traffic, **always `--dry-run` first**: it needs no
root, writes nothing, and prints the exact `echo … > …` and `systemctl …`
commands it would run, so you can eyeball the blast radius before committing.

## Why isolation matters

Latency percentiles above p99 are dominated by the machine, not the code. On
Linux you can actually control the machine (unlike a laptop), so the noise floor
is yours to set:

- **Another process lands on your core** — a Docker container, a cron job, a
  kernel thread — and you get a tail outlier. Fix: pin the benchmark to a core
  and keep everything else off it.
- **The clock moves** under turbo/boost and DVFS, so identical work takes
  different times. Fix: `performance` governor + turbo disabled → a fixed
  frequency.
- **The SMT sibling** on the same physical core steals execution resources. Fix:
  offline the sibling.
- **The scheduler tick** (250–1000 Hz) and **IRQs** interrupt your core. Fix:
  `nohz_full` + steer IRQs away.

`setup-isolation.sh` does all of this. What each layer buys you:

| Layer | Knob | Reboot? | Stops |
| --- | --- | --- | --- |
| Frequency | `performance` governor + turbo off | no | clock drift |
| Physical core | offline SMT sibling | no | hyperthread contention |
| Interrupts | stop `irqbalance` + IRQ affinity | no | IRQ jitter |
| Scheduler (soft) | cgroup `AllowedCPUs` | no | services + **Docker** on your core |
| Scheduler (hard) | `isolcpus` | **yes** | any task + most kernel threads |
| Tick | `nohz_full` + `rcu_nocbs` | **yes** | the periodic scheduler tick |

Unlike macOS, the timer here (`clock_gettime(CLOCK_MONOTONIC)`) has true
nanosecond resolution, so low percentiles are not quantised.

**Sample count** still only sets *within-run* precision: p99.9 needs ~100
samples in the tail (`n·(1−p)`), so `n = 100_000` gives a ±5% within-run CI and
more buys little; `n < 10_000` is noisy *and biased low*. Reproducibility comes
from repeated runs, which `run-bench.sh` handles.

## Choosing a core

Don't use core 0 (it handles a lot of kernel/IRQ work). Pick a high-numbered
core, and know its SMT sibling so you can account for both:

```bash
lscpu -e                                                   # CPU -> CORE -> SOCKET map
cat /sys/devices/system/cpu/cpu7/topology/thread_siblings_list   # e.g. "3,7"
```

If `lscpu -e` shows core 7 pairs with core 3 as SMT siblings, isolate 7 and let
the setup offline 3. Set `BENCH_CPU` (and pass the same number to the scripts).

## Keeping Docker (and everything else) off the core

Two mechanisms, use either or both:

**1. cgroup `AllowedCPUs` (no reboot — what `setup-isolation.sh apply` does).**
On cgroup v2, a child's usable CPUs are capped by its parent. The script shrinks
`system.slice`, `user.slice`, and `init.scope` to every core *except* the target.
Docker runs under `system.slice`, so all current and future containers inherit
the reduced set — **even `docker run --cpuset-cpus=7` cannot re-enter core 7**,
because the effective set is intersected with the parent's. This evicts the
containers without restarting them.

If you'd rather scope it per-container, the complementary knob is on Docker:

```bash
docker run --cpuset-cpus=0-6 ...        # new containers avoid core 7
# for a running container:
docker update --cpuset-cpus=0-6 <name>
# or set the daemon default in /etc/docker/daemon.json: {"cpuset-cpus": "0-6"}
```

**2. `isolcpus` (one reboot — the strongest).** Removes the core from the
scheduler's default affinity mask entirely, so nothing — containers, services,
*and* most kernel threads — is placed there unless it explicitly asks. Add to
`/etc/default/grub`:

```
GRUB_CMDLINE_LINUX_DEFAULT="... isolcpus=7,3 nohz_full=7,3 rcu_nocbs=7,3"
```

then `sudo update-grub && sudo reboot`. `setup-isolation.sh apply` prints this
line pre-filled for your chosen core + sibling. Use `isolcpus` for a machine you
benchmark on regularly; use the cgroup route when you can't reboot a server
that's serving traffic.

The benchmark then explicitly *claims* the freed core with `taskset -c 7`
(`run-bench.sh` does this), since isolated cores are opt-in.

## Running

```bash
BENCH_CPU=7 bench/run-bench.sh 15
```

`run-bench.sh` runs `check-env.sh`, builds the release profile, runs `bench_dist`
`N` times **pinned to the core** (`taskset -c`, and `chrt -f 1` if you set
`BENCH_RT=1`), writes one CSV per trial under `bench/trials/`, and calls
`aggregate.py` to produce:

- `bench/dist.csv` — the **median** of each column across trials (drop-in for
  `plot_dist.py`).
- a printed spread report: per-version median, min–max band, and coefficient of
  variation for the tail columns, so you can see how reproducible each figure was.
  If the tail CV is still high (>~20%), something is leaking onto the core —
  re-run `check-env.sh` and look for the remaining WARN.

Commit `dist.csv` together with the trial count, core id, and machine spec; a
percentile without its provenance is not a measurement.

## Scripts

| Script | Purpose |
| --- | --- |
| `check-env.sh [CORE]` | Read-only pre-flight: governor, turbo, SMT, isolation, ticks, IRQs, Docker. |
| `setup-isolation.sh apply\|restore [CORE]` | Apply/undo the runtime isolation; prints the GRUB line for the reboot-only bits. Needs `sudo`. |
| `run-bench.sh [N] [CORE]` | Pinned N-trial run + aggregation. |
| `aggregate.py trial_*.csv` | Median `dist.csv` + spread report. |
| `plot_dist.py` | Figure 1: p50/p99/p99.9 heap+stack vs version -> `dist.png`. |
| `plot_ratio.py` | Figure 2: heap/stack ratio at p99/p99.9 vs version -> `ratio.png`. |
| `p50_parity.py` | p50 heap/stack gap per version, from the existing `dist.csv` -> `p50_parity.txt`. |
| `collect_throughput_history.sh [N] [CORE]` | Per-commit landmark-cycle history across the perf-commit series (builds+runs each checkout in an isolated `git worktree`) -> raw trial dumps in `throughput_trials/`. |
| `aggregate_throughput.py DIR` | Median cycles + CV% per (commit, landmark) from `collect_throughput_history.sh`'s raw dumps -> `throughput_history.csv`/`.txt`. |
