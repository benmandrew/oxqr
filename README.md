# OxQR

QR code generator using OxCaml, a set of compiler extensions to OCaml from Jane Street. These give the programmer control over how data is allocated in the program, for example preferring stack to heap allocation to reduce pressure on the garbage collector.

The QR code generation is *zero-alloc*, which means that the generation does not allocate any memory on the heap, only on the stack. Scratch memory used during the generation is pre-allocated on the heap beforehand in an arena, and can be reused for the generation of multiple QR codes. This behaviour is enforced by the compiler using the `[@zero_alloc]` annotation on functions.

This program can be run natively as a CLI tool, or in the browser by transpiling to JavaScript with [`js_of_ocaml`](https://github.com/ocsigen/js_of_ocaml).

![Screenshot](doc/screenshot.png)

### Building

#### With Nix

If you have Nix with flakes enabled, `nix develop` provisions the full toolchain (OxCaml compiler, dune, opam, python) in one step — no manual opam switch setup needed:

```bash
nix develop --extra-experimental-features "nix-command flakes"
dune build
```

The first run builds the OxCaml compiler from source, which takes a while; subsequent runs reuse the `_opam` switch created in the project directory.

#### Without Nix

Get a local copy of OxCaml as an opam switch.

```bash
# Update to make sure you get the latest version of all packages
opam update --all
# This may take some time to install
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
eval $(opam env --switch 5.2.0+ox)
```

Then, install the project dependencies and build.

```bash
opam install . --deps-only --with-test
dune build
```

You can then either run the CLI tool, or start a local web server to interact through a browser at [http://localhost:8000/web](http://localhost:8000/web).

```bash
dune exec oxqr -- --ecl M --format ascii "HELLO WORLD"
# Or, build the JS bundle then serve it:
dune build bin/web.bc.js
python -m http.server 8000
```

### Testing

Tests are inline expect tests using `ppx_expect`.

```bash
dune test
```

### Benchmarking

```bash
# Latency distribution across QR versions
dune exec bench/bench_dist.exe
python bench/plot_dist.py   # generates bench/dist.png
```

### Links

- Get OxCaml (https://oxcaml.org/get-oxcaml/)
- OxCaml tutorial at ICFP'25 (https://gavinleroy.com/oxcaml-tutorial-icfp25/#0)
- QR Code Tutorial (https://www.thonky.com/qr-code-tutorial/)
