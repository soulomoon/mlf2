# Task Plan — BUG-2026-02-08-004 Thesis-Exact Fix

## Goal
Make BUG-2026-02-08-004 pass under checked-authoritative pipeline with thesis-faithful behavior, then update sentinel/docs/bug tracker.

## Scope
- Flip dedicated sentinel from expected rejection to expected success (`Int`).
- Diagnose root cause in elaboration/type-checking path for nested let + annotated-lambda application.
- Implement minimal thesis-faithful fix.
- Validate targeted gates + full suite.
- Update docs (`direction-matrix` closure and `Bugs.md`) and task records.

## Phases
1. Baseline + red test lock (complete)
2. Root-cause diagnosis (complete)
3. Implement fix (complete)
4. Verification (complete)
5. Docs/bug tracker updates (complete)

## Decisions
- Kept the fix local to `MLF.Elab.Elaborate` `AApp` to avoid broad behavior churn.
- Preserved existing `BUG-2026-02-06-002` hard-gate coverage and revalidated those focused regressions.
- Treated the checked-authoritative type checker as final arbiter and shaped elaboration instantiation steps accordingly.

## Errors Encountered
| Date | Error | Context | Resolution |
|------|-------|---------|------------|
| 2026-02-10 | `TCInstantiationError ... InstElim expects forall, got (Int -> Int) -> Int` | After flipping BUG-004 sentinel to thesis-green | Added function-side `InstApp` guard in `AApp` based on `typeCheckWithEnv` of function term; collapse to `InstId` when not `TForall`. |
| 2026-02-10 | `TCArgumentMismatch (Int -> Int) (a -> a)` | After first guard-only patch | Extended polymorphic argument-instantiation inference for variable args to use typechecked `fApp` arrow (`TArrow paramTy _`) path. |
