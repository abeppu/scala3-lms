This repository contains an in-progress Scala 3 port of the original
Lightweight Modular Staging (LMS) core library.

LMS is a runtime code-generation approach for building high-performance code
generators and embedded compilers in Scala. The original Scala 2 LMS source is
available at https://github.com/TiarkRompf/virtualization-lms-core.

## Current Status

The Scala 3 port builds and the covered Scala runtime-compile path is tested
with `sbt test`.

The main Scala 3 change is local virtualization: staged code must opt in with
the `@virt` macro annotation. `@virt` rewrites supported control flow and
operators after Scala 3 typer, replacing the old Scala 2 compiler-plugin
virtualization model.

Supported areas include:

- Scala code generation and quoted runtime compilation for the current core DSL.
- Staged `if`, `while`, selected `match`, `try/catch/finally`, `throw`, and
  `return` forms.
- Staged mutable variables, with explicit type annotations in user code, e.g.
  `var x: Var[Int] = 1`.
- Primitive boolean, equality, ordering, string, integer, long, float, and
  double operations covered by the test suite.
- Manifest-backed type reification for nested applied types, arrays, and LMS
  `Variable[T]` shapes used by runtime compilation.
- Scala 3 adaptations of the tutorial examples listed in `doc/PLAN.md`.

Known deferred areas:

- C, CUDA, and OpenCL backends.
- Query/compiler/backend-heavy tutorial chapters.
- Full staged extractor-pattern parity, because Scala 3 typer runs before the
  macro can rewrite staged scrutinees.
- Arbitrary staged exception-object behavior beyond the currently supported
  `getMessage`, `getCause`, and `toString` operations.

See `doc/PLAN.md` for the working checklist and `doc/DECISIONS.md` for design
constraints that should be preserved.

## Build

1. Install SBT.
2. Run `sbt test` to compile and run the test suite.
3. Run `sbt publish-local` to install LMS-Core for use in other projects.

## Background

- LMS website: http://scala-lms.github.io
- LMS paper: http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf

## License

Copyright 2010-2016, EPFL and collaborators. Licensed under the revised BSD
License.
