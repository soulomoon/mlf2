# Progress Log

## 2026-03-08
- Initialized task folder for the row1 guard-first desugar refactor.
- Confirmed the repo is clean before edits.
- Confirmed `MLF.Frontend.Desugar` still uses explicit recursion and there is no dedicated `FrontendDesugarSpec` yet.
- Added `test/FrontendDesugarSpec.hs` and wired it into `mlf2.cabal` and `test/Main.hs`.
- Verified the new direct row1 desugaring guards against the existing explicit-recursive implementation before refactoring.
- Added `SurfaceExprF` plus `Recursive`/`Corecursive` instances for `Expr 'Surface ty`, then refactored `desugarSurface` to `cata`.
- Targeted verification passes:
  - `MLF.Frontend.Desugar` -> PASS (`4 examples, 0 failures`)
  - `desugars annotated lambda parameters via let` -> PASS (`1 example, 0 failures`)
  - `ELet with EAnn RHS does not create explicit-scheme instantiation structure` -> PASS (`1 example, 0 failures`)
  - `row1 closeout guard|checked-authoritative` -> PASS (`2 examples, 0 failures`)
- Full verification: `cabal build all && cabal test` -> PASS (`974 examples, 0 failures`).
