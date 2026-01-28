# Design Document

## Overview
Implement Phase 7 as two new components:
- `MLF.Elab.TypeCheck` for xMLF typing (Figure 4 in `papers/xmlf.txt`), and
- `MLF.Elab.Reduce` for small-step reduction (Figure 5).

The typechecker validates Phase 6 output and reuses `applyInstantiation` to
interpret instantiation witnesses. The reducer provides a reference semantics
for elaborated terms, scoped to a single evaluation strategy.

## Architecture
- Typechecking is syntax-directed and returns `Either TypeCheckError ElabType`.
- Instantiation checking uses `applyInstantiation` on the inferred type of the
  term being instantiated; invalid instantiations are errors.
- Reduction is small-step with explicit evaluation contexts; we start with
  call-by-value (CBV) as the default strategy.
- A pipeline entry point `runPipelineElabChecked` (or similar) runs Phase 1-6
  then typechecks the elaborated term and returns `(term, type)`.

## Components and Interfaces
- `src/MLF/Elab/TypeCheck.hs`
  - `typeCheck :: ElabTerm -> Either TypeCheckError ElabType`
  - `typeCheckWithEnv :: Env -> ElabTerm -> Either TypeCheckError ElabType`
  - `checkInstantiation :: ElabType -> Instantiation -> Either TypeCheckError ElabType`
- `src/MLF/Elab/Reduce.hs`
  - `step :: ElabTerm -> Maybe ElabTerm`
  - `normalize :: ElabTerm -> ElabTerm`
  - `isValue :: ElabTerm -> Bool`
- `src/MLF/Elab/Pipeline.hs`
  - Re-export Phase 7 entry points.
- `src-public/MLF/Pipeline.hs` and `src-public/MLF/API.hs`
  - Optional public API exposure for typecheck/reduce helpers.

## Data Models
- Introduce `TypeCheckError` and `ReduceError` (if needed) in
  `src/MLF/Elab/Types.hs` or dedicated modules.
- Optional `EvalStrategy = CallByValue | CallByName` enum if strategy
  selection is desired. Initial implementation can hardcode CBV.

## Error Handling
- `TypeCheckError` distinguishes variable lookup, instantiation, and
  application mismatch cases.
- Reduction uses `Maybe` or `Either ReduceError` to signal stuck terms or
  invalid reduction attempts.

## Testing Strategy
- Unit tests for each typing rule: var, lam, app, let, type abs, instantiation.
- Reduction tests for:
  - beta reduction `(\x. e) v`,
  - instantiation reduction on `ETyInst` over type abstractions,
  - context stepping (reduce inside applications in CBV order).
- Property/regression: for a small fixed set of terms, `typeCheck t` equals
  `typeCheck (normalize t)` (type preservation sanity check).
