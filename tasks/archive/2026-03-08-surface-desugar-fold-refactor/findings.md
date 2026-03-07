# Findings

- `MLF.Frontend.Desugar` is still explicit recursion and remains the cleanest tree-only recursion-schemes candidate.
- Direct row1 desugaring-contract tests are still missing; current row1 confidence is indirect via normalization, constraint-generation, and pipeline tests.
- `Expr` currently has recursion-schemes support for source types only, not for surface expressions.
- The new direct desugaring guards passed against the pre-refactor implementation, which confirmed this was a structural cleanup rather than a latent row1 semantic fix.
- Exporting `SurfaceExprF` from `MLF.Frontend.Syntax` was necessary so `MLF.Frontend.Desugar` could use the manual base-functor constructors in its local algebra.
