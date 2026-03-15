# Design decisions

- Virtualization uses macro annotations (`@virt`) to rewrite control flow, comparisons, and mixed Rep/Var/host operands rather than relying on implicit conversions, to mirror scala-virtualized behavior and keep staging complete.
- Compatibility with the legacy LMS surface is preserved by vendoring EmbeddedControls and aliasing `Manifest`/`RefinedManifest` to `ClassTag`, keeping older examples source-compatible where possible.
- Staged mutable variables are represented with `VarCell` at runtime and require explicit type annotations in user code to satisfy Scala 3 macro typing; this keeps macro rewriting predictable while deferring a cleaner Var story to later work.
- Code generation currently targets Scala only; non-Scala backends are deferred until the Scala 3 port stabilizes.
- This file is the running log for future decisions—append new bullets here as changes are made.
- [Decision] LSP tooling (Metals) is treated as a helper only when available in the active IDE process; CLI discovery outside the workspace is not required for this task flow.
- [Decision] Avoid explicit homedir scanning for tooling discovery; rely on the current IDE-integrated language-server session or explicit user-provided binaries.
