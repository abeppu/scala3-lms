
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
    - [ ] Blocked: extractor patterns on staged scrutinees that require pre-typer extractor typing the current `Rep` surface cannot satisfy
    - [x] Harden staged extractor lowering internals for Scala 3 `Unapply` tree shapes (`unapply(scrutinee)` call construction and nested-condition plumbing)
  - [ ] Add support for `try` / `catch` / `finally`, plus `throw` / `return`
    - [x] Add a first-pass staged `try/catch` lowering for unguarded catch clauses without exception-value use
    - [x] Lower `throw new ThrowableSubclass(msg)` into staged exception IR for Throwable subclasses with single-String constructors
    - [x] Support `throw new ThrowableSubclass()` no-argument constructor syntax by lowering it with an empty staged message
    - [x] Support staged catch guards and typed/bound catch patterns when the binder itself is unused
    - [x] Support staged `finally` blocks in the Scala backend/runtime-compile path
    - [x] Support staged `return` for early exits in the Scala backend/runtime-compile path
    - [x] Support `throw new ThrowableSubclass(msg, cause)` where `cause` is a constructor-form Throwable (message or no-arg constructor)
    - [x] Support `throw new ThrowableSubclass(cause)` where `cause` is a constructor-form Throwable (message or no-arg constructor)
    - [x] Support constructor-form Throwable causes stored in local vals for `(String, Throwable)` and `Throwable`-only constructors
    - [x] Preserve host-only catch-binder guard/handler usage inside `@virt` methods when no staged `try/catch` virtualization is needed
    - [ ] Remaining gap: catch binders whose values are referenced in staged guards or handlers require exception-object representation in staged catch IR
  - [ ] Audit and extend operator coverage beyond the currently hard-coded boolean/arithmetic/equality/ordering/string-index cases
    - [x] Add staged `Int` support for `%`, `&`, `|`, `^`, `<<`, `>>`, `>>>`, and unary `~`
    - [x] Fill primitive `Float`/`Double` arithmetic surface/runtime gaps for staged `+`, `-`, `*`, `/`
    - [x] Fill primitive `Long` arithmetic surface/runtime/codegen gaps for staged `+`, `-`, `*`, `/`
    - [x] Fill primitive `Long` bitwise/shift surface/runtime/codegen gaps for staged `%`, `&`, `|`, `^`, `<<`, `>>`, `>>>`
    - [x] Add primitive `Long` unary bitwise-not (`~`) surface/runtime/codegen support
    - [x] Add primitive `Long` numeric conversions for staged `toFloat` and `toDouble`
    - [x] Add runtime-compile support for primitive parse nodes, primitive constants, and double conversion nodes already covered by Scala codegen
  - [x] Keep destructuring / pattern-bound local definitions host-side when the scrutinee is a host container, while preserving staged elements bound by the pattern
- [x] Improve type reification (nested type params, generic code staging)
  - [x] Preserve manifest-backed applied type arguments in `Typ.asTypeRepr`, including nested generics and array element types
  - [x] Reify path-dependent LMS `Variable[T]` directly in `Typ.asTypeRepr`, including `Array[Variable[T]]` wrappers
  - [x] Support nested `Array[Variable[T]]` type reification without runtime TODOs
- [ ] Restore in-process eval and non-Scala backends (C/CUDA/OpenCL)
  - [x] Re-enable the `RegexpMatcherTest` host-compile assertions once `StagingCompile` can evaluate virtualized `Var` flows under Scala 3
  - [ ] Deferred: non-Scala backends (C/CUDA/OpenCL) until the docker image/toolchain is explicitly in scope
- [ ] Port and validate original Scala 2 LMS examples under @virt
  - [x] Adapt the Scala-only tutorial examples that map cleanly to the current port (`start`, `ack`, `dynvar`, `shonan`, `automata`, `stencil`, `scanner`)
  - [x] Redesign `eval.scala` for Scala 3 around the current `DslCompile` runtime-compile path (no `CompileScala` reintroduction)
  - [ ] Deferred: query/compiler/backend-heavy tutorial chapters (`query*`, `linq`, `03_compiler`, `04_atwork`, scanner C lowering) until the non-Scala backend/runtime story is back in scope

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
- [x] `02_basics.scala` -> Scala 3 equivalent in tree
- [x] `dslapi.scala` -> Scala 3 custom-DSL extension equivalent in tree (Scala backend path)
- [x] `fft.scala` -> Scala 3 equivalent in tree (Scala backend/runtime-compile path)
- [x] `eval.scala` -> Scala 3 evaluator-specialization equivalent in tree (`DslCompile` runtime path)
- [x] `index.scala` -> Scala 3 equivalent tutorial catalog in tree
- [ ] `linq.scalax` -> deferred until query/linq staging story is back in scope
- [ ] `query.scala` -> deferred until query staging + non-Scala backend path is back in scope
- [ ] `query_unstaged.scala` -> deferred with query chapter
- [ ] `query_staged0.scala` -> deferred with query chapter
- [ ] `query_staged.scala` -> deferred with query chapter
- [ ] `query_live.scala` -> deferred with query chapter
- [ ] `query_live_steps.scala` -> deferred with query chapter
- [ ] `query_optc.scala` -> deferred until non-Scala backend/runtime story is back in scope
- [x] `shonan_live.scala` -> Scala 3 live-style staged matrix-vector example in tree
- [ ] `03_compiler.scala` -> deferred until compiler/backend-heavy chapter is back in scope
- [ ] `04_atwork.scala` -> deferred until compiler/backend-heavy chapter is back in scope
- [x] `scannerlib.scala` -> Scala 3 scanner internals equivalent in tree
- [x] `utils.scala` -> ported helper pieces used by current Scala 3 tutorial/test workflow (`dataFilePath`, `checkOut`, `exec`)
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

- [x] Unwind tree-smashed `using` insertions (-rewrite suggested but broken)
- [x] Replace the manual `x.toDouble` workaround with a cleaner solution
