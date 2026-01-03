# OxQR

QR code generator using OxCaml, a set of compiler extensions to OCaml from Jane Street. These give the programmer control over how data is allocated in the program, for example preferring stack to heap allocation to reduce pressure on the garbage collector.

The QR code generation is *zero-alloc*, which means that the generation does not allocate any memory on the heap, only on the stack. Scratch memory used during the generation is pre-allocated on the heap beforehand in an arena, and can be reused for the generation of multiple QR codes. This behaviour is enforced by the compiler using the `[@zero_alloc]` annotation on functions.

This program can be run natively as a CLI tool, or in the browser by transpiling to JavaScript with [`js_of_ocaml`](https://github.com/ocsigen/js_of_ocaml).

![Screenshot](doc/screenshot.png)

### Building

First, get a local copy of OxCaml as an opam switch.

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

You can then either run the CLI tool, or start a local web server to interact through a browser.

```bash
dune exec oxqr -- --ecl M --format ascii "HELLO WORLD"
# Or...
python -m http.server -d web 8000
```




### Links

- Get OxCaml (https://oxcaml.org/get-oxcaml/)
- OxCaml tutorial at ICFP'25 (https://gavinleroy.com/oxcaml-tutorial-icfp25/#0)
- QR Code Tutorial (https://www.thonky.com/qr-code-tutorial/)
