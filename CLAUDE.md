# OxQR

Zero-alloc QR code generator written in OxCaml (Jane Street's OCaml with allocation-control extensions). Runs natively as a CLI tool and in the browser via `js_of_ocaml`.

## Repository layout

```
lib/        Core library (oxqr)
  config.ml         Version/ECL config tables
  encoding.ml       Top-level encode + Arena scratch allocator
  qr.ml             QR matrix construction (place patterns, data, mask)
  reed_solomon.ml   Reed-Solomon error correction
  bitbuf.ml         Bit-level buffer
  svg.ml            SVG renderer

bin/        Executables
  main.ml           Native CLI (dune exec oxqr)
  web.ml            js_of_ocaml entry point for the browser demo
  verify.ml         Runtime-events heap-allocation verifier

test/       Inline expect tests (ppx_expect)
  test_qr.ml
  test_encoding.ml

bench/      Benchmarks
  bench.ml          Landmark-based profiling
  bench_versions.ml Throughput across QR versions (outputs versions.csv)
  bench_dist.ml     Latency distribution (outputs dist.csv)
  plot_versions.py  Plot versions.csv → versions.png
  plot_dist.py      Plot dist.csv → dist.png

web/        Browser demo (HTML/CSS/JS + compiled wasm/js)
docs/       Built web assets (served statically)
```

## Build system

Dune 3.20, OxCaml switch `5.2.0+ox`.

```bash
# One-time switch setup (takes time)
opam update --all
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
eval $(opam env --switch 5.2.0+ox)

# Install deps and build
opam install . --deps-only --with-test
dune build
```

## Common commands

```bash
# Run CLI
dune exec oxqr -- --ecl M --format ascii "HELLO WORLD"

# Run tests
dune test

# Release build (enables -O3)
dune build --profile release

# Run benchmarks
dune exec bench/bench_versions.exe          # version-scaling throughput
dune exec bench/bench_dist.exe              # latency distribution
python bench/plot_versions.py               # generate versions.png
python bench/plot_dist.py                   # generate dist.png

# Landmark profiling (bench.ml)
dune exec bench/bench.exe

# Browser demo — build JS then serve
dune build bin/web.bc.js
python -m http.server 8000
# Open http://localhost:8000/web

# Allocation verifier (checks zero-alloc at runtime)
dune exec bin/verify.exe
```

## Key design constraints

- **Zero-alloc**: `generate_qr_stack` and all `[@@zero_alloc]`-annotated functions must not heap-allocate. The OxCaml compiler enforces this at compile time.
- **Arena**: All scratch memory is pre-allocated via `Encoding.Arena.create`. Pass the arena into `encode`/`generate_qr_stack`; it can be reused across multiple QR generations.
- **`local_` modifier**: OxCaml stack-allocation syntax. `local_ t` values may not escape their scope.
- **Release profile**: Always benchmark with `--profile release` to get `-O3` optimisations.

## Testing

Tests are inline expect tests using `ppx_expect`. Run with `dune test`. Promotable output lives alongside the source in `test/`.

## Benchmarking workflow

1. Edit benchmark in `bench/bench_versions.ml` or `bench/bench_dist.ml`.
2. `dune exec bench/bench_<name>.exe > bench/<name>.csv` (or let the executable write the file).
3. `python bench/plot_<name>.py` to regenerate the PNG.
4. Commit CSVs and PNGs together with the benchmark source change.
