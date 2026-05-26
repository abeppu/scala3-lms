
## Current status checklist

- [x] Builds on Scala 3.7.3 and test suite is green (`sbt test`, 80 passing)
- [x] Trim warning noise (unchecked/feature/deprecation) to make regressions visible
  - [x] Remove debug `report.info` logging from `MacroVirtualization` rewrites
- [ ] Harden virtualization macro (Var handling, trailing units, avoid brittle string matching)
  - [x] Fix effectful staged `if` values flowing into later generic numeric/operator call sites
    - [ ] Remaining cleanup: simplify the macro-side IR shape so runtime compile does not need to normalize symbolic constants in that path
  - [ ] Add support for `match` / pattern matching on staged values
    - [x] First pass: staged scrutinee with literal/stable-id/alternative cases, wildcard fallback, guards, and simple binders/aliases
    - [x] Initial typed-pattern support for staged wildcard type tests over `Rep[Any]`
    - [x] Add typed binders and typed aliases over staged `Rep[Any]` scrutinees
    - [ ] Remaining gaps: extractor patterns and full host-match preservation for richer pattern trees
  - [ ] Add support for `try` / `catch` / `finally`, plus `throw` / `return`
    - [x] Add a first-pass staged `try/catch` lowering for unguarded catch clauses without exception-value use
    - [x] Lower `throw new Exception(msg)` and `throw new IllegalArgumentException(msg)` into staged exception IR
    - [ ] Remaining gaps: `finally`, broader `throw` syntax virtualization, `return`, guarded catch cases, and catch binders whose values are referenced in the handler
  - [ ] Audit and extend operator coverage beyond the currently hard-coded boolean/arithmetic/equality/ordering/string-index cases
    - [x] Add staged `Int` support for `%`, `&`, `|`, `^`, `<<`, `>>`, `>>>`, and unary `~`
  - [ ] Decide how to handle destructuring / pattern-bound local definitions inside `@virt` blocks
- [ ] Improve type reification (nested type params, generic code staging)
- [ ] Restore in-process eval and non-Scala backends (C/CUDA/OpenCL)
  - [x] Re-enable the `RegexpMatcherTest` host-compile assertions once `StagingCompile` can evaluate virtualized `Var` flows under Scala 3
- [ ] Port and validate original Scala 2 LMS examples under @virt
  - [x] Adapt the Scala-only tutorial examples that map cleanly to the current port (`start`, `ack`, `dynvar`, `shonan`, `automata`, `stencil`, `scanner`)
  - [ ] Decide whether to reintroduce a `CompileScala`-style path or otherwise redesign `eval.scala` for Scala 3
  - [ ] Defer the query/compiler/backend-heavy tutorial chapters (`query*`, `linq`, `03_compiler`, `04_atwork`, scanner C lowering) until the non-Scala backend/runtime story is back in scope
- [ ] Document new design decisions in `DECISIONS.md` as they land

## Hacks

The fact that trees will need to typecheck _before_ macro rewriting makes
rewriting control flow a bit more annoying. We can kind of cheat by having
an implicit conversion method from `Rep[Bool]` to `Bool` because we'll
rewrite it anyway, but it will make field accesses and especially pattern
matching very difficult.

- [x] Replace string-based matching on `Rep`/`Var` with `TypeRepr`-level checks

## Notes from lms-clean

`lms-clean` seems to define a much more general virtualization mechanism
(probably due to porting code from the old scala-virtualized). We should skip
that and generate the code much more directly.

## Compatibility

- [x] Vendor EmbeddedControls
- [x] Alias `Manifest` and `RefinedManifest` to `ClassTag[T]`

## Manual fixes

- [ ] Unwind tree-smashed `using` insertions (-rewrite suggested but broken)
- [ ] Replace the manual `x.toDouble` workaround with a cleaner solution
