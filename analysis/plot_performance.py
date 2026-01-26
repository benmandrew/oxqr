#!/usr/bin/env python3
"""
QR Generation Tail Event Analyzer
"""

import pandas as pd
import matplotlib.pyplot as plt
import argparse

def load_timing_data(filename):
    try:
        df = pd.read_csv(filename)
        return df
    except FileNotFoundError:
        print(f"Error: Could not find file {filename}")
        return None

def plot_histogram(df, title, ax, color='blue', xlim=None):
    durations = df['duration_seconds'] * 1000  # Convert to milliseconds
    tail_events = df[df['tail_event'] == 1]['duration_seconds'] * 1000
    n_bins = min(50, len(durations) // 20)
    _counts, bins, _patches = ax.hist(durations, bins=n_bins, alpha=0.7, color=color, edgecolor='black')
    if len(tail_events) > 0:
        ax.hist(tail_events, bins=bins, alpha=0.9, color='red', edgecolor='darkred', label='Tail Events')
    mean_ms = durations.mean()
    std_ms = durations.std()
    p95_ms = durations.quantile(0.95)
    p99_ms = durations.quantile(0.99)
    p99_9_ms = durations.quantile(0.999)
    tail_count = len(tail_events)
    tail_percentage = (tail_count / len(durations)) * 100
    stats_text = f'Mean: {mean_ms:.3f}ms\nStd: {std_ms:.3f}ms\nP95: {p95_ms:.3f}ms\nP99: {p99_ms:.3f}ms\nP99.9: {p99_9_ms:.3f}ms\nTail Events: {tail_count} ({tail_percentage:.2f}%)'
    ax.text(0.98, 0.98, stats_text, transform=ax.transAxes, fontsize=10,
            verticalalignment='top', horizontalalignment='right',
            bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))
    ax.set_title(title, fontsize=14, fontweight='bold')
    ax.set_xlabel('Duration (milliseconds)', fontsize=12)
    ax.set_ylabel('Frequency', fontsize=12)
    if xlim is not None:
        ax.set_xlim(0, xlim)
    ax.grid(True, alpha=0.3)
    if len(tail_events) > 0:
        ax.legend()

def plot_time_series(df, title, ax, color='blue', ylim=None):
    durations_ms = df['duration_seconds'] * 1000
    tail_mask = df['tail_event'] == 1
    ax.plot(df['iteration'], durations_ms, color=color, alpha=0.6, linewidth=0.5)
    ax.scatter(df['iteration'], durations_ms, c=color, s=1, alpha=0.7)
    if tail_mask.any():
        tail_iterations = df[tail_mask]['iteration']
        tail_durations = durations_ms[tail_mask]
        ax.scatter(tail_iterations, tail_durations, c='red', s=20, alpha=0.9, 
                  label='Tail Events', zorder=5)
    ax.set_title(f'{title} - Time Series', fontsize=14, fontweight='bold')
    ax.set_xlabel('Iteration', fontsize=12)
    ax.set_ylabel('Duration (milliseconds)', fontsize=12)
    if ylim is not None:
        ax.set_ylim(0, ylim)
    ax.grid(True, alpha=0.3)
    if tail_mask.any():
        ax.legend()

def create_comparison_plot(stack_df, heap_df):
    # Calculate consistent axis limits across both datasets
    stack_durations_ms = stack_df['duration_seconds'] * 1000
    heap_durations_ms = heap_df['duration_seconds'] * 1000

    # Worst-case analysis
    stack_worst_idx = stack_durations_ms.idxmax()
    stack_worst_val = stack_durations_ms.max()
    heap_worst_idx = heap_durations_ms.idxmax()
    heap_worst_val = heap_durations_ms.max()

    print(f"Stack-based worst-case: {stack_worst_val:.3f} ms (iteration {stack_df['iteration'][stack_worst_idx]})")
    print(f"Heap-based worst-case:  {heap_worst_val:.3f} ms (iteration {heap_df['iteration'][heap_worst_idx]})")

    # For time series: find max duration across both datasets
    max_duration = max(stack_durations_ms.max(), heap_durations_ms.max())

    # For histograms: find max duration for x-axis consistency
    max_hist_duration = max_duration

    fig = plt.figure(figsize=(16, 12))
    gs = fig.add_gridspec(3, 2, height_ratios=[2, 2, 1], hspace=0.3, wspace=0.3)

    # Histograms
    ax1 = fig.add_subplot(gs[0, 0])
    ax2 = fig.add_subplot(gs[0, 1])
    plot_histogram(stack_df, 'Stack-based QR Generation', ax1, 'steelblue', xlim=max_hist_duration)
    plot_histogram(heap_df, 'Heap-based QR Generation', ax2, 'darkgreen', xlim=max_hist_duration)

    # Time series
    ax3 = fig.add_subplot(gs[1, 0])
    ax4 = fig.add_subplot(gs[1, 1])
    plot_time_series(stack_df, 'Stack-based', ax3, 'steelblue', ylim=max_duration)
    plot_time_series(heap_df, 'Heap-based', ax4, 'darkgreen', ylim=max_duration)

    # Add worst-case annotation to the bottom row
    ax5 = fig.add_subplot(gs[2, :])
    ax5.axis('off')
    worst_text = (f"Stack-based worst-case: {stack_worst_val:.3f} ms (iteration {stack_df['iteration'][stack_worst_idx]})\n"
                 f"Heap-based worst-case:  {heap_worst_val:.3f} ms (iteration {heap_df['iteration'][heap_worst_idx]})")
    ax5.text(0.5, 0.5, worst_text, fontsize=14, ha='center', va='center', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.7))

    plt.suptitle('QR Generation Performance Analysis: Stack vs Heap', fontsize=16, fontweight='bold')
    return fig

def main():
    parser = argparse.ArgumentParser(description='Analyze QR generation tail events')
    parser.add_argument('--stack-file', default='generate_qr_stack_timings.csv',
                       help='CSV file with stack-based timing data')
    parser.add_argument('--heap-file', default='generate_qr_heap_timings.csv',
                       help='CSV file with heap-based timing data')
    parser.add_argument('--output', default='qr_tail_analysis.png',
                       help='Output filename for the plot')
    parser.add_argument('--show', action='store_true',
                       help='Show the plot interactively')
    args = parser.parse_args()
    print("Loading timing data...")
    stack_df = load_timing_data(args.stack_file)
    heap_df = load_timing_data(args.heap_file)
    if stack_df is None or heap_df is None:
        print("Error: Could not load required data files.")
        print(f"Expected files: {args.stack_file}, {args.heap_file}")
        print("Run the tail_events OCaml program first to generate CSV data.")
        return 1
    print(f"Loaded {len(stack_df)} stack measurements and {len(heap_df)} heap measurements")
    print("Creating plots...")
    fig = create_comparison_plot(stack_df, heap_df)
    fig.savefig(args.output, dpi=300, bbox_inches='tight')
    print(f"Plot saved as {args.output}")
    if args.show:
        plt.show()
    return 0

if __name__ == '__main__':
    exit(main())
