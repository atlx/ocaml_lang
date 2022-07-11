# ocaml_lang

A unnamed compiler written in OCaml. Currently just getting started!

## Updating & upgrading packages

```bash
$ opam update # Update repositories.
$ opam upgrade # Upgrade packages.
```

## Requirements

[llvm](https://llvm.org/index.html) v13.0.0

## Installing LLVM

The best way to install the proper LLVM version without any problems is to download the source code and build it yourself. If you already have a different LLVM version install, proceed with the same steps. OPAM is smart enough to differentiate between multiple installed versions.

```bash
$ cd llvm-13.0.0.src
$ mkdir _build
$ cd _build
$ cmake ..
$ sudo cmake --build . --target install -j2 # Build and install LLVM.
```
