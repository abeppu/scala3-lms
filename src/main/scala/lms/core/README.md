### Why virtualize comparisons in the macro?

Scala already provides `given` conversions (for example, the `OrderingOpsCls` wrappers in LMS) so that any `Rep[T]` receiver automatically exposes `<`, `<=`, `>`, and `>=`. That covers the common “both sides are already staged” case without any help from macros.

Where things fall apart is when one side of the comparison is a host value (for instance `5 < repX` or `repX < 10`). In those situations the compiler resolves the expression to the standard library operators long before the LMS conversions get a chance to run, so no IR nodes are created. The legacy scala-virtualized plugin side-stepped this by rewriting every comparison into `ordering_lt(lhs, rhs)` regardless of which side was staged.

The new `@virt` macro follows that approach because:

1. **Completeness:** Every combination of `Rep`, `Var`, and bare operands behaves the same. Relying on conversions would require symmetric extensions (both `Rep[T]`→`Rep[T]` and `T`→`Rep[T]` for every primitive) and still be brittle whenever type inference picks an unexpected overload.
2. **Error reporting:** The macro can surface a single “no `__whileDo` in scope” style error instead of letting the compiler fall through to a confusing “value `<` is not a member of Rep[T]” message.
3. **Centralization:** All virtualization logic (equality, ordering, boolean combinators, loops) lives in one place. That makes it easier to keep behavior aligned with the original LMS frontend and reason about staging boundaries.

We considered leaning more heavily on implicit conversions or extension methods. They would reduce macro surface area and avoid TreeMap rewrites, but only if we mirrored scala-virtualized’s behavior with a comprehensive set of bidirectional conversions. That quickly becomes more code than the macro approach and still fails for syntactic constructs (`if`, `while`, guards) that require AST-level access. For now we keep the macro machinery for ordering, but we can revisit once enough extensions exist to cover the awkward mixed cases.
