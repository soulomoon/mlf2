# Design

## Overview
Add targeted Hspec examples that exercise explicit forall annotations in both constraint generation and elaboration. The tests focus on three edge cases: annotated let-bound variables, explicit forall round-tripping, and forall-in-bound preservation.

## Test placement
- Constraint-shape check for REQ-1 in `test/ConstraintGenSpec.hs` under "Annotated Terms" or "Annotation Edge Cases".
- Elaboration-level checks for REQ-2 and REQ-3 in `test/ElaborationSpec.hs` under paper-alignment or annotation sections.

## Detailed approach

### REQ-1: Single TyExp for annotated let-bound variable
- Build the expression:
  - `ELet "id" (ELam "x" (EVar "x")) (EAnn (EVar "id") ann)`
  - `ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))`
- Use `inferConstraintGraphDefault` to get `ConstraintResult`.
- Find the annotation edge by traversing `crAnnotated` and extracting the `InstEdgeId` from the `AAnn` node.
- Assert:
  - The left side of that inst edge is a `TyExp` whose body is **not** another `TyExp`.
  - This ensures only one expansion node sits between the annotated occurrence and the scheme root.

### REQ-2: Explicit forall round-trip
- Build the expression:
  - `ELet "id" (ELam "x" (EVar "x")) (EAnn (EVar "id") ann)`
  - same `ann` as above.
- Run `requirePipeline` and assert the resulting type is alpha-equivalent to the expected explicit forall scheme.

### REQ-3: Forall-in-bound preservation
- Build the expression:
  - `EAnn (ELam "x" (EVar "x")) ann`
  - `ann = STForall "a" (Just (STForall "b" Nothing (STArrow (STVar "b") (STVar "b")))) (STArrow (STVar "a") (STVar "a"))`
- Run `requirePipeline` and assert the elaborated type retains the forall in the bound.

## Error handling
- Tests should use existing helpers (`expectRight`, `requirePipeline`, `shouldAlphaEqType`) and produce clear failure messages.

## Verification
- `cabal test --test-show-details=direct --test-option=--match --test-option="explicit forall annotation"`
