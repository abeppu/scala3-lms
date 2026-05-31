# Design Decisions

This file records implementation decisions specific to the Scala 3 LMS port.
It is not a changelog; it should capture constraints and tradeoffs that future
work needs to preserve.

## Local Macro Virtualization

Scala 3 support uses local `@virt` macro rewriting instead of recreating the
Scala 2 compiler-plugin virtualization model. The macro rewrites only annotated
definitions after typer and classifies terms by type as staged `Rep[T]`, staged
mutable `Var[T]`, or host `T`.

Plain host expressions should remain host expressions. Rewrites switch to LMS
combinators only when a staged value participates, because broad rewrites after
typer can easily break valid host code inside `@virt` definitions.

## Staged Control Flow

Staged `if`, `while`, `match`, `try/catch/finally`, `throw`, and `return` are
implemented selectively through the existing LMS IR and runtime-compile path.
Unsupported syntax should fail with a targeted macro error rather than silently
falling back to host semantics.

Staged `if` results must be real staged expressions (`Sym`/`Rep`), not constants
wrapping staged symbols. Runtime compile no longer accepts `Const(Sym(...))` as a
recovery path; if that shape reappears, fix the macro/IR construction instead.

## Pattern Matching Boundary

A `match` is staged only when its scrutinee is directly staged (`Rep`/`Var`).
Host matches and local destructuring stay host-side even when the host container
contains staged values. This lets code such as tuple or `Option` destructuring
bind staged elements without forcing staged extractor-pattern support.

Current staged scrutinee support covers literals, stable ids, alternatives,
wildcards, guards, simple binders/aliases, and typed binders over `Rep[Any]`.
The macro can lower some extractor-shaped trees once Scala has already typed
them, but extractor syntax over a staged scrutinee is still constrained by
Scala 3 typer running before `@virt`. If a pattern requires the pre-macro
`Rep[T]` surface to satisfy an extractor's expected scrutinee type, the code
will fail before the virtualization macro can rewrite it.

For that reason, extractor-pattern parity should not be pursued only by adding
more macro cases. The staged path needs either a source shape that typechecks
against `Rep` before rewriting or an explicit staged match/combinator API whose
case tests and projections are already expressed as LMS terms. The initial
`stagedMatch` helper takes explicit staged tests, value cases, and type cases;
it composes existing `__ifThenElse`, equality, and cast/type-test operations
rather than introducing separate match IR.

## Exceptions

Staged exceptions are represented by exception class name plus staged message in
the existing `ThrowException` IR. `throw new ThrowableSubclass(msg)` and
`throw new ThrowableSubclass()` lower to that representation; no-arg throws use
an empty staged message.

Catch cases carry an exception class name plus optional staged guard and handler
blocks. They can bind the caught exception itself as a scoped staged
`Rep[Throwable]` for supported member operations. Current member coverage is
`getMessage`, `getCause`, and `toString`; these are emitted through explicit
exception IR so guard/handler computations stay inside the catch scope in Scala
codegen and runtime compile.

The staged exception object is intentionally not an arbitrary mutable host
`Throwable`. Unsupported operations such as stack-trace mutation, suppressed
exceptions, or backend-specific exception behavior should continue to fail in
the macro until a concrete tutorial or regression requires them.

## Type Reification

`Typ.asTypeRepr` must preserve manifest-backed applied type arguments. Runtime
compile uses this to build quoted parameter, local, and block result types, so
erasing nested type arguments is not acceptable.

Arrays are reified from their element `Typ` rather than directly from JVM array
runtime classes so primitive arrays and generic element arrays produce Scala
`Array[T]` type representations. Path-dependent LMS types such as `Variable[T]`
do not currently have direct `TypeRepr` reification; prefer representing them
through their element/result operations unless a concrete use case requires more.

## Primitive Operators

Operator coverage is intentionally incremental. The macro and primitive surface
API should support operators only where there is matching LMS IR plus runtime
compile support.

Current Scala runtime-compile coverage includes staged `Int` arithmetic,
bitwise, shifts, and unary bitwise-not; staged `Float`, `Double`, and `Long`
arithmetic for `+`, `-`, `*`, and `/`; and existing boolean/equality/ordering
rewrites. New operators should be added with focused regression tests that
exercise runtime compile, not only generated source emission.

## Examples And Backends

Scala-only tutorial examples are the current validation target. C/CUDA/OpenCL
and compiler/backend-heavy tutorial chapters remain deferred until the runtime
and non-Scala backend story is explicit again.
