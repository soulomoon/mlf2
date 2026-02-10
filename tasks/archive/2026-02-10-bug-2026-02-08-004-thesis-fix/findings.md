# Findings — BUG-2026-02-08-004 Thesis-Exact Fix

## 2026-02-10 Initial Context
- Dedicated BUG-2026-02-08-004 sentinel existed and passed by asserting rejection behavior.
- Full suite was green before flipping this sentinel to thesis-expected success.
- Closely related passing case already existed in `test/ElaborationSpec.hs` (`annotated lambda parameter should accept a polymorphic argument via κσ (US-004)`).

## Root-Cause Findings
- After switching sentinel to thesis-green `Int`, the failing payload was:
  - `TCInstantiationError ... InstElim expects forall, got (Int -> Int) -> Int`.
- This showed that `AApp` elaboration could preserve witness-derived `InstApp` even when the elaborated function term was already monomorphic (non-`∀`).
- A second failure after partial fix was:
  - `TCArgumentMismatch (Int -> Int) (a -> a)`.
- This exposed a second seam: polymorphic-argument repair for variable arguments only inferred from syntactic `ELam` and missed typed-arrow shapes after function-side instantiation.

## Fix Findings
- In `MLF.Elab.Elaborate` `AApp`:
  - Added `funInstByFunType`: keep `InstApp` only when `typeCheckWithEnv` reports `TForall{}` for the function term.
  - Added `fAppForArgInference` path and extended `argInstFromFun` for variable args (`Just v`) to infer from `typeCheckWithEnv tcEnv fAppForArgInference` when it yields `TArrow paramTy _`.
- This preserved thesis intent (κσ-style acceptance for annotated lambda params) while preventing invalid `InstElim` chains on monomorphic arrows.

## Verification Findings
- `BUG-2026-02-08-004` sentinel now passes as thesis-green (`Int`) in unchecked + checked pipeline.
- `BUG-2026-02-06-002` strict/thesis/focused regressions all remain green.
- Full gate passes: `cabal build all && cabal test` => `604 examples, 0 failures`.
