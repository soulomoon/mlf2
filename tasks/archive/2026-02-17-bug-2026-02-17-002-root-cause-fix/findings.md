# Findings: BUG-2026-02-17-002

## Baseline evidence
- Direct repro (outside sentinel) produced:
  - `Phase 7 (type checking): TCLetTypeMismatch (TForall ... (TForall "t0" ...)) (TForall ... )`.
- Traced elaboration showed:
  - let scheme for `c` and elaborated RHS diverged under applied variant;
  - app recovery on literal arguments selected `InstElim` fallback instead of `InstApp`, causing bottomization.

## Pattern comparison
- Working A6 parity variant (`ELet ... (EAnn (EVar "c") ann)`) stayed green but still exposed the same shape-sensitive fallback paths.
- Applied variant (`EApp (EApp (EAnn (EVar "c") ann) 1) 2`) was sensitive to:
  - annotated-lambda RHS classification in `ALetF`;
  - non-variable argument handling in `AAppF` fun-inst recovery.

## Validated root causes
- `ALetF` did not treat `AAnn (ALam ...)` as lambda RHS, so lambda fallback logic was skipped where needed.
- `AAppF` only considered variable-origin arguments for `InstApp` recovery; literals remained on `InstElim` fallback and bottomized.

## Result after fix
- Reproducer now elaborates/typechecks to `Int` for both unchecked and checked pipelines.
- Regression test converted from sentinel to strict success assertion.
