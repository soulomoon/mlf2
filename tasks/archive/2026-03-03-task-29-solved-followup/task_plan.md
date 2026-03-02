# Task Plan: Task 29 Solved Follow-up Implementation

## Goal
Implement Task 29 follow-up items from `TODO.md` using an agent-team workflow while preserving thesis-faithful behavior and keeping the build/test gate green.

## Phases
- [x] Phase 1: Create shared `Solved -> PresolutionView` adapter and replace duplicated adapters. (status: completed)
- [x] Phase 2: Remove runtime `Solved.mkTestSolved` dependency from planner/reify paths. (status: completed)
- [x] Phase 3: Tighten solved/base mapping fallback handling (`gaSolvedToBase`) with explicit typed handling and guards. (status: completed)
- [x] Phase 4: Add solved-boundary guard tests (canonical/original invariants, O15 empty translation, result-type primary/fallback equivalence). (status: completed)
- [x] Phase 5: Realign thesis obligations/claims anchors to runtime modules and sync docs. (status: completed)
- [x] Phase 6: Full validation gate and task closeout packaging. (status: completed)

## Decisions
- Execute with subagent-driven-development loops: implementer -> spec reviewer -> code-quality reviewer per phase.
- Keep behavior changes minimal and evidence-backed; prefer compatibility-preserving refactors first.
- Place the shared `Solved -> PresolutionView` adapter at the presolution boundary (`MLF.Constraint.Presolution.View`) only; runtime call sites import this boundary module directly.
- Guard the migration with semantic adapter-equivalence assertions (solved query parity) in `PipelineSpec`, not source-text matching.
- Add `Solved.fromConstraintAndUf` as the production-safe direct constructor and keep `mkTestSolved` as a compatibility alias for existing tests.
- Model `gaSolvedToBase` lookup outcomes explicitly as `mapped | same-domain | missing` and route all `resolveContext` fallback branches through this typed classifier.
- For Phase 4 guard tests, import `MLF.Constraint.Presolution.Plan.Context` directly from `mlf2-internal` instead of re-exporting `SolvedToBaseResolution`/`resolveGaSolvedToBase` from `MLF.Constraint.Presolution`.
- Post-Phase-4 review-fix pass uses a fresh parallel agent team for independent medium findings (`SolvedSpec` constructor guard quality and `ElaborationSpec` GA/result-type guard realism), then runs the full build/test gate before proceeding.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `pvCanonical` not in scope in `MLF.Elab.Run.Pipeline` after import tightening | 1 | Restored `PresolutionView(..)` import in `Pipeline.hs` so field selectors remain in scope. |
| `ghc-pkg` package.conf creation conflict while running multiple `cabal test` commands in parallel | 1 | Re-ran test commands sequentially; no persistent build issue. |
| New Phase 4 guards failed to compile because `MLF.Constraint.Presolution.Plan.Context` and `MLF.Elab.Run.ResultType` were hidden from `mlf2-test` | 1 | Exposed both modules from private `mlf2-internal` in `mlf2.cabal`; tests compile and pass. |
| Review-fix merge introduced an `ElaborationSpec` warning (`foldl'` redundant import) | 1 | Removed the redundant `foldl'` import and re-ran targeted + full validation. |
