# Task Plan: 2026-02-25 Equivalence-Class Abstraction

## Goal
Execute `docs/plans/2026-02-25-equivalence-class-abstraction-plan.md` using the subagent-driven-development workflow (implementer -> spec review -> code-quality review per task) while keeping thesis-faithful behavior and full regression safety.

## Scope
- Plan source: `docs/plans/2026-02-25-equivalence-class-abstraction-plan.md`
- Required workflow: `subagent-driven-development`
- Repository process constraints from `AGENTS.md` (task folder logs, TODO/Bugs/docs updates, full validation)

## Current Baseline (at start)
- Working tree is dirty before this execution:
  - `src/MLF/Constraint/Solved.hs`
  - `test/ConstraintGenSpec.hs`
  - `test/ElaborationSpec.hs`
  - `test/PipelineSpec.hs`
  - `test/Presolution/ExpansionSpec.hs`
  - `test/SpecUtil.hs`
- `tasks/todo/` did not exist; created for this task.
- Plan milestones enumerate Tasks 1-17.

## Phases
- [complete] Phase 1: Baseline audit + task extraction + planning files initialization
- [complete] Phase 2: Execute Milestone 1 tasks (1-2) with subagent/review gates
- [complete] Phase 3: Execute Milestone 2 tasks (3-7) with subagent/review gates
- [complete] Phase 4: Execute Milestone 3 tasks (8-10) with subagent/review gates
- [complete] Phase 5: Execute Milestone 4 tasks (11-13) with subagent/review gates
- [complete] Phase 6: Execute Milestone 5 tasks (14-17) with subagent/review gates
- [complete] Phase 7: Full validation (`cabal build all && cabal test`) + docs/trackers sync + archive task

## Task Checklist (from plan)
- [x] Task 1: Create `MLF.Constraint.Solved` module with Phase 1 API
- [x] Task 2: Add `Solved` unit tests
- [x] Task 3: Migrate `Elaborate.hs`
- [x] Task 4: Migrate `Phi/Translate.hs`
- [x] Task 5: Migrate `Omega.hs`
- [x] Task 6: Migrate test infrastructure
- [x] Task 7: Hide `SolveResult` fields
- [x] Task 8: Add equivalence-class fields/backends to `Solved`
- [x] Task 9: Build pre-rewrite snapshot construction
- [x] Task 10: Add equivalence-class query tests
- [x] Task 11: Wire equivalence backend into solve output
- [x] Task 12: Add backend comparison test harness
- [x] Task 13: Remove `applyUFConstraint` from primary output
- [x] Task 14: Rework OpWeaken handling in Omega
- [x] Task 15: Rebuild IdentityBridge on `Solved`
- [x] Task 16: Remove `unionFind` escape hatch
- [x] Task 17: Update deviation documentation

## Decisions
- Treat this as continuation work (pre-existing uncommitted changes), not a clean-room implementation.
- Keep strict gate order per task: spec compliance review must pass before code-quality review.
- Use one implementation subagent at a time to avoid workspace conflicts.
- Accept minor Task 6 quality findings as non-blocking for Task 7 kickoff:
  - `mkSolved` broad export may weaken opacity in production API.
  - Repeated `Solved.toSolveResult` bridges remain in some tests.
- Accept internal module split (`MLF.Constraint.Solve.Internal`) as Task 7 implementation detail:
  - Keeps constructor/field access confined to `Solve` + `Solved`.
  - Preserves abstract `SolveResult` export to consumers.
- Accept Task 8 follow-up hardening (`c3ec0a3`) as required quality gate:
  - `EquivBackend` canonicalization now chase-based/cycle-safe.
  - Equiv original/snapshot queries no longer silently fallback to legacy views.
- Task 9 snapshot exposure design:
  - Added `solveUnifyWithSnapshot` in `MLF.Constraint.Solve` returning `SolveOutput` with both the legacy `SolveResult` and a `SolveSnapshot`.
  - `SolveSnapshot` captures `(UF at pre-applyUFConstraint point, preRewrite constraint)` where `preRewrite` is frozen immediately before `applyUFConstraint`.
  - `solveUnify` remains unchanged semantically by delegating to `solveUnifyWithSnapshot` and returning `soResult`.
- Accept Task 9 follow-up consolidation (`aba07a0`) as required quality gate:
  - Shared `rewriteConstraintWithUF` is now the single rewrite path for both solve output and snapshot construction.
  - Added direct snapshot regression tests in `SolveSpec` and `Constraint/SolvedSpec`.
- Task 10 unit test shape:
  - Added explicit EquivBackend assertions in `Constraint/SolvedSpec` using `fromPreRewriteState` for `classMembers`, `wasOriginalBinder`, `originalNode`, and `originalBindParent`.
  - Refactored the snapshot comparison fixture into explicit legacy-oracle tests for `canonical` and `lookupNode`.
- Task 11 wiring strategy:
  - Kept `solveUnify` return type stable (`SolveResult`) for compatibility.
  - Wired the runtime pipeline and test helpers through `solveUnifyWithSnapshot` + `Solved.fromSolveOutput` so the staged equivalence backend becomes the primary `Solved` construction path.
  - Kept legacy bridges (`fromSolveResult`, `toSolveResult`) for compatibility where APIs still require `SolveResult`.
- Task 13 compile-recovery strategy:
  - `Solved.fromSolveOutput` must remain snapshot-driven and not read `soResult.srConstraint`.
  - Snapshot replay is centralized in `solveResultFromSnapshot`; both solve output and solved reconstruction use this shared path.
  - `fromSolveOutput` / `fromPreRewriteState` are typed (`Either SolveError`) to avoid partial constructors and preserve explicit error handling.
  - Keep `applyUFConstraint`-based rewriting in compatibility replay (`solveResultFromSnapshot`) for legacy `SolveResult` consumers; staged solved construction remains snapshot-authoritative.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Implementer subagent stalled without status | 1 | Interrupted and dispatched replacement implementer with tighter scope |
| Replacement implementer returned partial completion (no tests/commit) | 2 | Dispatched focused compile-fix subagent and then resumed same subagent for full tests + commit |
| Task 7 implementer stalled mid-migration without report | 1 | Interrupted, inspected partial diffs, and dispatched a recovery implementer that completed Task 7 |
| Task 8 build failed: `TyForall` constructor not in scope in `Solved.hs` | 1 | Imported `TyNode(..)` in `MLF.Constraint.Solved` |
| Task 8 build failed: `NodeId` constructor not in scope in `Solved.hs` | 1 | Imported `NodeId(..)` in `MLF.Constraint.Solved` |
| Task 9 build failed: ambiguous `ssUnionFind` record field (clashed with `SolverState`) | 1 | Renamed new snapshot fields to `snapUnionFind` / `snapPreRewriteConstraint` |
| Task 9 spec review flagged snapshot UF point mismatch | 1 | Changed `snapUnionFind` to capture pre-rewrite UF (`uf`) instead of post-subst `uf'` (`ccc687a`) |
| Task 9 quality review flagged rewrite duplication and missing snapshot tests | 1 | Introduced shared `rewriteConstraintWithUF` and added snapshot regression tests (`aba07a0`) |
| Task 11 review dispatch hit agent thread limit (`max 6`) | 1 | Closed completed implementer agent and retried reviewer dispatch successfully |
| Task 13 compile break: `Solved.hs` imported non-exported `rewriteEliminatedBinders` | 1 | Removed stale import and restored snapshot replay path via `fromPreRewriteState`; full test suite now green |
| Task 13 quality review flagged partial constructors (`error`) and silent fallback | 1 | Changed `fromSolveOutput`/`fromPreRewriteState` to return `Either SolveError` and updated pipeline/tests to propagate errors explicitly |
| Task 15 follow-up regression: alias-target `OpWeaken` fell back to `ε` after first binder-identity patch | 1 | Added explicit class-fallback rank tier in `IdentityBridge.lookupBinderIndex` and revalidated both targeted regressions plus full gate |

## 2026-02-26 follow-up (binder identity collapse in merged classes)

- RED:
  - Added failing regression `preserves raw binder identity before class-member fallback` in `test/Phi/IdentityBridgeSpec.hs`.
  - Failure reproduced: `lookupBinderIndex` returned `Just 0` for both binders in one solved class (`b2` should resolve to `Just 1`).
- Root cause:
  - `IdentityBridge.lookupBinderIndex` treated class-expanded keys as exact-match inputs, so binders sharing a class became indistinguishable and defaulted to lowest spine index.
- GREEN:
  - Split source-key usage into exact keys (no class fallback) vs class-fallback keys.
  - Ranking now runs: exact identity match -> class fallback (only if target has no exact keys) -> canonical alias fallback.
- Regression safety:
  - Re-ran existing alias-target weaken test to ensure `OpWeaken` recovery remained intact.
  - Full gate pass: `cabal build all && cabal test` (`825 examples, 0 failures`).

## 2026-02-26 Task 17 closure (deviation/claims doc sync)

- Audited `docs/thesis-deviations.yaml`:
  - `DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP` is already removed.
  - No remaining deviations are rooted in solved-node identity loss.
- Audited `docs/thesis-claims.yaml`:
  - no claim status remained partial for this resolved deviation class;
  - `CLM-PHI-CORRECTNESS` already has `deviations: []`.
- Verification:
  - `./scripts/check-thesis-claims.sh` -> PASS (`21 claims, 4 deviations, no orphans`).
  - `cabal build all && cabal test` -> PASS (`827 examples, 0 failures`).
