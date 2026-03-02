# Task Plan: Eliminate Solved Indirection

## Goal
Replace production `Solved`-based elaboration/planning boundaries with `PresolutionView` built directly from presolution data, then reduce `MLF.Constraint.Solved` to compatibility/test-only surface.

## Phases
1. [completed] Establish isolated worktree baseline and verify clean build/test.
2. [completed] Gather migration surface using parallel agent teams (Phi/Omega, Reify/Generalize, Run/Pipeline).
3. [completed] Write detailed agent-team implementation plan in `docs/plans/2026-03-02-eliminate-solved-indirection-agent-team-implementation-plan.md`.
4. [completed] Execute Wave 0 (core `PresolutionView` + parity guards).
5. [completed] Execute Wave 1 in parallel (Phi/Omega, Reify/Generalize, Pipeline/Run).
6. [completed] Execute Wave 2 (Presolution/Plan closure migration + Solved cleanup + docs/changelog).

### Active Workstream (Team D)
- [completed] Task 3: migrate `ElabEnv` boundary from `eeSolved` to `eePresolutionView`.
- [completed] Task 6: remove solved-builder pipeline path and migrate result-type inputs away from `rtcSolved`.

### Active Workstream (Team B)
- [completed] Task 4: migrate Phi/Omega query path to `PresolutionView` in `Context`/`Omega`/`IdentityBridge`, with Translate adapters preserving existing call-site semantics.

### Active Workstream (Team C)
- [completed] Task 5: migrate Reify + Generalize canonical-constraint dependencies to `PresolutionView` inputs in owned modules.

### Active Workstream (Team E)
- [completed] Task 7: migrated `PresolutionPlanBuilder` closure to `PresolutionView` and updated planner/generalize call chain.
- [completed] Task 8: removed production-only `Solved.fromPresolutionResult` builder surface and added guard coverage.
- [completed] Task 9: added elaboration-path solved-import hygiene guard for public/entrypoint modules and updated docs/changelog/task logs.

## Decisions
- Use `PresolutionView` as the single production query boundary for elaboration/planning.
- Keep migration test-first with source-level guard tests plus behavior regressions.
- Use clustered agent ownership to minimize merge conflicts and shorten turnaround.
- Team D scope stays within owned modules and guard tests; no cross-team cleanup outside the listed ownership set.
- For Task 9 hygiene, enforce the solved-import ban at public/entrypoint elaboration modules (`MLF.Elab.Run`, `MLF.Elab.Pipeline`, `MLF.API`, `MLF.Pipeline`) rather than all internal elaboration modules, because current architecture still keeps compatibility solved reads in internal run/result-type modules.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| Initial explorer agent batch stalled and was interrupted without payload | 1 | Re-dispatched narrower-scope agents; captured completed outputs from 3 clusters and backfilled remaining scope with direct local scans. |
| Agent thread cap reached after interrupted threads | 1 | Closed stale threads, then resumed dispatch with reduced concurrency. |
| Hspec `--match` pattern with OR (`a\|b\|c`) matched 0 examples in this suite | 1 | Kept required commands for auditability, then ran equivalent per-test and per-suite matches individually to verify real pass/fail state. |
| Parallel `cabal test` invocations raced on `dist-newstyle/.../package.conf.inplace` | 1 | Re-ran test commands sequentially; all targeted checks passed. |
| Importing `PresolutionView` in `Reify/*` through `MLF.Constraint.Presolution` (and then `...View`) created a module cycle via `Presolution.Base -> Plan -> ReifyPlan -> Reify.TypeOps` | 1 | Reverted Reify-side view imports and kept explicit `PresolutionView` propagation in `Elab.Run.Generalize*` where no cycle exists. |
| Requested verification commands currently fail before tests run because unrelated in-flight edits in `src/MLF/Elab/Phi/Omega.hs` do not compile | 1 | Left unrelated Team B files untouched; captured blocker output in progress log and reported verification as blocked by external compile errors. |
| Required Team B guard red-run blocked by existing module cycle (`Presolution.Plan.BinderPlan.Build` -> `Reify.Core` -> `Presolution.Plan` ...) | 1 | Logged blocker and proceeded with source migration; re-run verification commands after edits to confirm whether blocker persists. |
| Task 7 first implementation rebuilt solved-compat state via `Solved.fromPreRewriteState` and regressed `Paper alignment baselines` with `InvalidBindingTree ... node ... not in constraint` | 1 | Replaced that reconstruction with `Solved.mkTestSolved` over `pvCanonicalConstraint` plus live-node sanitized `pvCanonicalMap`; regression slices returned green. |

## Completion / Closure (2026-03-02)
- Gate A (target slices for Tasks 7-9): PASS.
- Gate B (guard/hygiene verification): PASS.
- Gate C (`cabal build all && cabal test`): PASS (`905 examples, 0 failures`).
- Task records archived to `tasks/archive/2026-03-02-eliminate-solved-indirection` on 2026-03-02.
