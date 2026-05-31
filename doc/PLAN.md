
## Current status checklist

- [x] Builds on Scala 3.7.3 and test suite is green (`sbt test`, 171 passing)
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
    - [x] Document the Scala-typer boundary for extractor syntax over staged scrutinees and the supported staged-match subset
    - [x] Add a staged match-combinator API for cases that Scala pattern syntax cannot typecheck before macro expansion
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
    - [x] Add staged catch-binder representation for supported exception members, starting with `e.getMessage`
    - [x] Lower catch-binder `getMessage` references in staged guards and handlers through the new catch IR
    - [x] Add regression coverage for staged guards/handlers that depend on catch-binder messages
    - [ ] Decide whether full staged exception objects are needed after message-only support lands
      - [x] Define a real staged exception-object representation, likely `Rep[Throwable]`-like catch binder symbols that are scoped to each `ReifiedCatch`
      - [x] Add IR nodes for selected exception members beyond `getMessage`, starting with `getCause` and `toString`
      - [ ] Decide whether `getClass`/type-test operations should be explicit exception ops or reuse the existing staged cast/type-test surface
      - [x] Teach Scala codegen and runtime compile to bind the caught exception object itself, not only a derived message string
      - [x] Update scheduling/bound-symbol handling so pure computations depending on catch binders stay inside guard/handler catch scopes
      - [ ] Blocked: passing catch binders or catch-derived staged members through ordinary helper methods needs a macro inlining/source-shape strategy; direct supported member operations remain the safe path
      - [x] Fix runtime-compile preservation of nested staged `try/catch` inside catch handlers; nested catch-binder regression now keeps the inner throw inside its catch
      - [ ] Keep arbitrary host-side exception mutation, stack trace inspection, suppressed exceptions, and backend-specific exception semantics out of scope unless a concrete tutorial/test requires them
      - [ ] Add focused regressions for fallback behavior around unsupported exception helper-call/member-operation shapes
  - [x] Audit and extend operator coverage beyond the currently hard-coded boolean/arithmetic/equality/ordering/string-index cases
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
  - [ ] Bring up minimal C backend smoke coverage before attempting full query codegen
    - [ ] Add a Scala 3 `DslDriverC` equivalent that emits C source and can optionally compile/run it when a C compiler is available
    - [ ] Add C source golden tests for tiny staged snippets: arithmetic, `if`, `while`, mutable vars, arrays, strings/printing
    - [ ] Validate that the existing `CCodegen`/`CLikeCodegen` and `CGen*` traits still compile and preserve expected semantics after the Scala 3 port
    - [ ] Keep CUDA/OpenCL deferred until the C backend path is exercised and the container toolchain is explicitly in scope
- [ ] Port and validate original Scala 2 LMS examples under @virt
  - [x] Adapt the Scala-only tutorial examples that map cleanly to the current port (`start`, `ack`, `dynvar`, `shonan`, `automata`, `stencil`, `scanner`)
  - [x] Redesign `eval.scala` for Scala 3 around the current `DslCompile` runtime-compile path (no `CompileScala` reintroduction)
  - [x] Port `linq.scalax` first as a Scala-backend tutorial, before taking on the C/query path
    - [x] Add an initial Scala 3 LINQ `rangeFromNames` slice using a typed schema facade, staged lists, and DB traversal normalization
    - [x] Port the source to Scala 3 syntax and `@virt` usage
      - [x] Convert `rangeFromNames` from explicit `list_flatMap`/`list_map` calls to a Scala `for` comprehension
      - [x] Avoid Scala 3's covariant `Rep[List[A]]` implicit-conversion widening by adding direct `Rep[List[A]]` extension methods, so plain `a <- ageFromName(start)` keeps `a: Rep[Int]`
      - [x] Port the legacy `satisfies` higher-order predicate query shape
      - [x] Port the legacy dynamic predicate-AST examples (`Above`/`Below`/`And`/`Or`/`Not`) as host-side matches producing staged predicates
      - [x] Port the legacy nested `expertise("abstract")` query over corporate departments/employees/tasks
      - [x] Port the legacy `nestedOrg` and `expertise2("abstract")` higher-order nested-query shape
    - [x] Restore or adapt the required `StructOps`/structural-record surface for `Record { val ... }`, anonymous record construction, and field projection
      - [x] Replace the temporary typed `Name` case-class facade with an explicit staged `record("field" -> value)` constructor that emits anonymous `new TutorialLinqSchema.Record { val ... }` Scala code
      - [x] Add staged field projection for the current `name` record field
      - [x] Extend the record facade to multi-field records with typed `name` and `age` projections plus generated-source coverage
      - [x] Decide to keep the explicit `record(...)` facade for Scala 3 tutorial parity; direct legacy `Record { val ... }` source syntax remains deferred with rationale in `DECISIONS.md`
    - [x] Add the full staged `List` surface needed by LINQ (`map`, `flatMap`, `filter`, `++`, `isEmpty`, `ListNew`, `ListConcat`) to the active tutorial DSL path
      - [x] Add LINQ regression coverage for staged `++` / `ListConcat`
      - [x] Add LINQ regression coverage for staged `.isEmpty` / `ListIsEmpty`
      - [x] Add LINQ regression coverage for explicit staged `map`, `filter`, and `List(...)` construction
    - [x] Port the LINQ-specific IR and normalization rewrites: `Database`, `DBFor`, `Fun`, `dbfor`, and the staged `ifThenElse` normalization cases
      - [x] Generalize the hardcoded `People` table node into typed table projection IR so `DBFor` can normalize multiple database-backed tables
      - [x] Add the legacy-style `differences` query over `couples` and `people` as multi-table `DBFor` regression coverage
      - [x] Add regression coverage for higher-order query predicates and host-selected dynamic predicate trees
      - [x] Add nested `DBFor`/`isEmpty` regression coverage for `expertise`
      - [x] Add nested record/list-field regression coverage for `nestedOrg` and higher-order `all`/`contains` query composition
    - [x] Add Scala codegen for `Database`, `DBFor`, and generated record construction
      - [x] Add Scala codegen coverage for generic table projection and multi-table `DBFor`
    - [x] Add a regression for the current Scala 3 `rangeFromNames` generated output and host result; tighten toward legacy structural-record output as parity improves
  - [ ] Port query/compiler/backend-heavy tutorial chapters after LINQ and C smoke coverage
    - [ ] Port `query_unstaged.scala` as the host baseline and SQL parser/AST reference
    - [ ] Port `query_staged0.scala` and `query_staged.scala` for Scala source generation before switching to C
    - [ ] Port scanner lowering (`ScannerLowerExp`, `CGenScannerLower`) for C-level file/input access
    - [ ] Port `query_optc.scala` once the C driver and scanner lowering are in place
    - [ ] Add staged query tests in phases: AST parity, Scala generated source, Scala output CSV, C generated source, then C output CSV when the local toolchain supports it
    - [ ] Keep `query_live.scala`, `query_live_steps.scala`, `03_compiler.scala`, and `04_atwork.scala` deferred until the core query/C path is stable

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
- [x] `linq.scalax` -> Scala 3 Scala-backend equivalent in tree through nested LINQ examples, using explicit staged records
- [ ] `query.scala` -> planned after LINQ and C smoke coverage
- [ ] `query_unstaged.scala` -> planned as query host baseline
- [ ] `query_staged0.scala` -> planned as first Scala-backend query compiler
- [ ] `query_staged.scala` -> planned after `query_staged0`
- [ ] `query_live.scala` -> deferred until core query/C path is stable
- [ ] `query_live_steps.scala` -> deferred until core query/C path is stable
- [ ] `query_optc.scala` -> planned after C driver smoke tests and scanner lowering
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
