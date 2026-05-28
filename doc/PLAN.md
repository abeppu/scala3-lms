
## Current status checklist

- [x] Builds on Scala 3.7.3 and test suite is green (`sbt test`, 91 passing)
- [x] Trim warning noise (unchecked/feature/deprecation) to make regressions visible
  - [x] Remove debug `report.info` logging from `MacroVirtualization` rewrites
- [ ] Harden virtualization macro (Var handling, trailing units, avoid brittle string matching)
  - [x] Fix effectful staged `if` values flowing into later generic numeric/operator call sites
    - [x] Remove the runtime-compile `Const(Sym)` recovery path; current macro/IR construction now keeps staged-if results as real staged symbols
  - [ ] Add support for `match` / pattern matching on staged values
    - [x] First pass: staged scrutinee with literal/stable-id/alternative cases, wildcard fallback, guards, and simple binders/aliases
    - [x] Initial typed-pattern support for staged wildcard type tests over `Rep[Any]`
    - [x] Add typed binders and typed aliases over staged `Rep[Any]` scrutinees
    - [x] Preserve richer host-only extractor matches when `@virt` code stays on bare Scala scrutinees
    - [ ] Remaining staged gaps: extractor patterns on staged scrutinees
  - [ ] Add support for `try` / `catch` / `finally`, plus `throw` / `return`
    - [x] Add a first-pass staged `try/catch` lowering for unguarded catch clauses without exception-value use
    - [x] Lower `throw new ThrowableSubclass(msg)` into staged exception IR for Throwable subclasses with single-String constructors
    - [x] Support `throw new ThrowableSubclass()` no-argument constructor syntax by lowering it with an empty staged message
    - [x] Support staged catch guards and typed/bound catch patterns when the binder itself is unused
    - [x] Support staged `finally` blocks in the Scala backend/runtime-compile path
    - [x] Support staged `return` for early exits in the Scala backend/runtime-compile path
    - [ ] Remaining gaps: richer `throw` constructor shapes beyond zero-arg/single-String Throwable constructors and catch binders whose values are referenced in guards or handlers
  - [ ] Audit and extend operator coverage beyond the currently hard-coded boolean/arithmetic/equality/ordering/string-index cases
    - [x] Add staged `Int` support for `%`, `&`, `|`, `^`, `<<`, `>>`, `>>>`, and unary `~`
    - [x] Fill primitive `Float`/`Double` arithmetic surface/runtime gaps for staged `+`, `-`, `*`, `/`
    - [x] Fill primitive `Long` arithmetic surface/runtime/codegen gaps for staged `+`, `-`, `*`, `/`
  - [x] Keep destructuring / pattern-bound local definitions host-side when the scrutinee is a host container, while preserving staged elements bound by the pattern
- [ ] Improve type reification (nested type params, generic code staging)
  - [x] Preserve manifest-backed applied type arguments in `Typ.asTypeRepr`, including nested generics and array element types
  - [ ] Decide whether path-dependent LMS types such as `Variable[T]` need direct `TypeRepr` reification, or should remain represented through their element/result operations
- [ ] Restore in-process eval and non-Scala backends (C/CUDA/OpenCL)
  - [x] Re-enable the `RegexpMatcherTest` host-compile assertions once `StagingCompile` can evaluate virtualized `Var` flows under Scala 3
- [ ] Port and validate original Scala 2 LMS examples under @virt
  - [x] Adapt the Scala-only tutorial examples that map cleanly to the current port (`start`, `ack`, `dynvar`, `shonan`, `automata`, `stencil`, `scanner`)
  - [ ] Decide whether to reintroduce a `CompileScala`-style path or otherwise redesign `eval.scala` for Scala 3
  - [ ] Defer the query/compiler/backend-heavy tutorial chapters (`query*`, `linq`, `03_compiler`, `04_atwork`, scanner C lowering) until the non-Scala backend/runtime story is back in scope

### Legacy tutorial parity tracker (`/legacy-lms-tutorials/src/test/scala/lms/tutorial`)

- [x] `start.scala` -> Scala 3 equivalent in tree
- [x] `ack.scala` -> Scala 3 equivalent in tree
- [x] `dynvar.scala` -> Scala 3 equivalent in tree
- [x] `shonan.scala` -> Scala 3 equivalent in tree
- [x] `automata.scala` -> Scala 3 equivalent in tree
- [x] `stencil.scala` -> Scala 3 equivalent in tree
- [x] `scanner.scala` -> Scala 3 equivalent in tree
- [x] `regex.scala` -> Scala 3 equivalent in tree
- [x] `01_overview.scala` -> Scala 3 equivalent in tree
- [ ] `02_basics.scala` -> add Scala 3 equivalent once tutorial framing is settled
- [ ] `dslapi.scala` -> add Scala 3 equivalent after remaining macro/operator gaps are closed
- [ ] `fft.scala` -> add Scala 3 equivalent once backend/runtime constraints are confirmed for this example
- [ ] `eval.scala` -> blocked on Scala 3 in-process eval design decision (`CompileScala`-style replacement)
- [ ] `index.scala` -> add Scala 3 equivalent once chapter aggregation/runner structure is decided
- [ ] `linq.scalax` -> deferred until query/linq staging story is back in scope
- [ ] `query.scala` -> deferred until query staging + non-Scala backend path is back in scope
- [ ] `query_unstaged.scala` -> deferred with query chapter
- [ ] `query_staged0.scala` -> deferred with query chapter
- [ ] `query_staged.scala` -> deferred with query chapter
- [ ] `query_live.scala` -> deferred with query chapter
- [ ] `query_live_steps.scala` -> deferred with query chapter
- [ ] `query_optc.scala` -> deferred until non-Scala backend/runtime story is back in scope
- [ ] `shonan_live.scala` -> add Scala 3 equivalent once live tutorial workflow is defined
- [ ] `03_compiler.scala` -> deferred until compiler/backend-heavy chapter is back in scope
- [ ] `04_atwork.scala` -> deferred until compiler/backend-heavy chapter is back in scope
- [ ] `scannerlib.scala` -> add Scala 3 equivalent when scanner internals chapter is explicitly in scope
- [ ] `utils.scala` -> port only helper pieces needed by the chosen Scala 3 tutorial set
- [x] Document new design decisions in `DECISIONS.md` as they land

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
- [x] Replace the manual `x.toDouble` workaround with a cleaner solution
