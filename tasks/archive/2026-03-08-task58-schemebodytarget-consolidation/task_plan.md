# Task Plan: Task 58 item 3 — `schemeBodyTarget` consolidation

## Metadata
- Date: 2026-03-08
- Status: completed
- Skills: using-superpowers, brainstorming, planning-with-files, haskell-pro

## Goal
Consolidate the duplicated `schemeBodyTarget` logic between `MLF.Elab.Elaborate` and `MLF.Elab.Run.Scope` only if the richer run-scope semantics can be adopted without changing intended elaboration behavior.

## Phases
1. Inspect the two implementations and all call sites. [completed]
2. Decide the safe consolidation shape and document it. [completed]
3. Add/adjust guard tests for the chosen semantics. [completed]
4. Implement the consolidation with minimal code movement. [completed]
5. Run targeted tests, then broader verification if needed. [completed]

## Decisions
- Treat `MLF.Elab.Run.Scope.schemeBodyTarget` as the richer candidate implementation.
- Keep the richer `schemeBodyTarget` for thesis `S′`-style subterm translation, but route `Elaborate.generalizeAtNode` through a separate owner-local `generalizeTargetNode` helper after nested-let / alias regressions showed that named-node generalization still needs the older `S`-style bound descent.
- Preserve single ownership in `MLF.Elab.Run.Scope`; do not restore any local helper in `MLF.Elab.Elaborate`.

## Errors
- `cabal build all && cabal test` initially failed with `TCLetTypeMismatch` in nested-let / alias regressions after `generalizeAtNode` adopted the richer `schemeBodyTarget` directly.
- Root cause: the richer `S′`-style helper is correct for subterm translation, but named-node generalization still needs the older `S`-style bound/body descent.
