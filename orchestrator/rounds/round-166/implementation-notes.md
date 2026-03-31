# Round 166 — Item 7: Golden Test Expansion — Implementation Notes

## What was created

- **`test/GoldenSpec.hs`** — New spec module with 8 golden tests:
  - 5 xMLF pretty-print golden tests: identity, church-true, poly-let, simple-app, choose
  - 3 constraint graph summary golden tests: identity, poly-let, choose
  - `goldenTest` helper with `GOLDEN_ACCEPT=1` workflow support
  - `constraintSummary` helper for deterministic constraint graph serialization
  - `{- Note [Golden test workflow] -}` documenting the accept workflow

- **8 golden files** under `test/golden/`:
  - `xmlf-identity.golden`, `xmlf-church-true.golden`, `xmlf-poly-let.golden`, `xmlf-simple-app.golden`, `xmlf-choose.golden`
  - `constraint-identity.golden`, `constraint-poly-let.golden`, `constraint-choose.golden`

- **Wiring**: `GoldenSpec` added to `mlf2.cabal` `other-modules` and `test/Main.hs`

## Test count change

- Before: 1288 tests
- After: 1296 tests (+8)

## Adjustments from the plan

1. **rank2-app → simple-app**: The plan's `rank2AppExpr` (`(λf. let a = f 1 in f True) (λx. x)`) fails the pipeline type-checker with `TCArgumentMismatch (TBase "Int") (TBase "Bool")`. This is expected — the expression requires the lambda parameter `f` to have a polymorphic type, but without an annotation the pipeline infers a monomorphic type for lambda-bound variables. Replaced with `simpleAppExpr = EApp (ELam "x" (EVar "x")) (ELit (LInt 42))` — identity applied to a literal, which exercises application without requiring rank-2 inference.

2. **No inline `takeDirectory`**: Used `System.FilePath (takeDirectory)` from the `filepath` package already in `build-depends`, per the plan's preferred approach.

## Verification

- `cabal build all && cabal test`: All 1296 tests pass, 0 failures
- No `-Wall` warnings from `GoldenSpec`
- No production code modified
- No new dependencies added
