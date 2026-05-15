# Round 232 Review

Date: 2026-05-15
Round: `round-232`
Milestone: `milestone-3`
Direction: `direction-3a-phase-indexed-constraint`
Extracted item: `item-3a-phase4-raw-bridge-retirement`
Base branch: `master`
Branch: `orchestrator/round-232-phase-indexed-constraint`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The live round lineage matches the assigned worktree and branch: `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`, and `active_rounds[0] = round-232` at stage `review`.

- Command: `git status --short`
  Result: pass. The implementation payload is limited to Phase-4 presolution owners and direct regression guards under [src/MLF/Constraint/Presolution/Base.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Base.hs:157), [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:82), [src/MLF/Constraint/Presolution/StateAccess.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/StateAccess.hs:118), [test/PresolutionFacadeSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/PresolutionFacadeSpec.hs:49), and [test/RepoGuardSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/RepoGuardSpec.hs:65), plus the round-local reviewer artifacts. `orchestrator/state.json` is also dirty, but only as controller-owned stage bookkeeping and is not treated as round payload.

- Command: `python3 - <<'PY' ... PY`
  Result: pass. `selection-record.json` and `round-plan-record.json` both match the active `roadmap_id`, `roadmap_revision`, and `roadmap_dir`, and the active bundle resolves the required closeout anchors `milestone-3`, `milestone-3-completion`, and `roadmap-history-completed-rounds` in [orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap-view.json](/Volumes/src/mlf4/orchestrator/worktrees/round-232/orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap-view.json:35).

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- src/MLF/Constraint/Solve src/MLF/Backend src-public docs/architecture.md AGENTS.md mlf2.cabal test/Main.hs`
  Result: pass. No files changed under solve, backend, public API, durable docs, or module-registration surfaces, so solve-backend/private erasure stayed out of scope and the round introduced no public API widening or registration churn.

- Command: `rg -n "computePresolution|solveUnify ::|data Constraint \\(p :: Phase\\)|toRawConstraintForLegacy|castConstraint|presolutionInProgressRawBridge|newtype PresolutionM p a|data PresolutionState p = PresolutionState|type PresolutionPhaseOf m :: Phase|getConstraintAndCanonical :: PresolutionM p|putConstraintAndUnionFind :: Constraint p|mkInitialPresolutionState :: Constraint 'Acyclic -> PresolutionState 'Acyclic|toPresolvedConstraint :: Constraint 'Acyclic -> Constraint 'Presolved" src/MLF/Constraint/Types/Graph.hs src/MLF/Constraint/Normalize.hs src/MLF/Constraint/Acyclicity.hs src/MLF/Constraint/Presolution/Driver.hs src/MLF/Constraint/Presolution/Base.hs src/MLF/Constraint/Presolution/StateAccess.hs src/MLF/Constraint/Solve.hs test/PresolutionFacadeSpec.hs test/RepoGuardSpec.hs`
  Result: pass. Integrated HEAD satisfies the milestone-3 completion signal: `Constraint (p :: Phase)` is defined at [src/MLF/Constraint/Types/Graph.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Types/Graph.hs:129); `normalize`, `checkAcyclicity`, `computePresolution`, and `solveUnify` advance `Raw -> Normalized -> Acyclic -> Presolved -> Solved` at [src/MLF/Constraint/Normalize.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Normalize.hs:102), [src/MLF/Constraint/Acyclicity.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Acyclicity.hs:118), [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:83), and [src/MLF/Constraint/Solve.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Solve.hs:41). The presolution state foundation is phase-indexed at [src/MLF/Constraint/Presolution/Base.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Base.hs:157) and [src/MLF/Constraint/Presolution/Base.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Base.hs:294), while shared access helpers are phase-indexed at [src/MLF/Constraint/Presolution/StateAccess.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/StateAccess.hs:118). The driver now carries `Constraint 'Acyclic` into working state and emits `Constraint 'Presolved` without a raw bridge at [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:86) and [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:479).

- Command: `rg -n "presolutionInProgressRawBridge|psConstraint :: Constraint 'Raw|getConstraintAndCanonical :: PresolutionM 'Raw|getConstraintAndUnionFind :: PresolutionM 'Raw|putConstraintAndUnionFind :: Constraint 'Raw|castConstraint|toRawConstraintForLegacy" src/MLF/Constraint/Presolution src/MLF/Constraint/Types/Graph.hs test/PresolutionFacadeSpec.hs test/RepoGuardSpec.hs`
  Result: pass. No production match remains for the retired raw bridge or broad raw-only presolution signatures; the only hits are the negative regression assertions in [test/PresolutionFacadeSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/PresolutionFacadeSpec.hs:49) and [test/RepoGuardSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/RepoGuardSpec.hs:65). Broad graph escape hatches stay retired, and the remaining phase erasure helper `rephaseConstraint` stays private to [src/MLF/Constraint/Types/Graph.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Types/Graph.hs:193) rather than reappearing as a public cast surface.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target rebuilt successfully after the presolution state migration.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`
  Result: pass. The focused presolution facade slice passed with `8 examples, 0 failures`, including the typed Phase-4 boundary checks in [test/PresolutionFacadeSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/PresolutionFacadeSpec.hs:49).

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`
  Result: pass. The focused repository-guard slice passed with `26 examples, 0 failures`, including the raw-bridge regression guard in [test/RepoGuardSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/RepoGuardSpec.hs:65).

- Command: `cabal build all && cabal test`
  Result: pass. Full behavior-changing gate passed with `2567 examples, 0 failures` in `352.4238` seconds.

## Plan Compliance

- `Parameterize the presolution state foundation (PresolutionState, PresolutionM, and MonadPresolution) over the in-progress graph phase so getConstraint and modifyConstraint no longer require Constraint 'Raw`: met. [src/MLF/Constraint/Presolution/Base.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Base.hs:157) now defines `PresolutionState p`, [src/MLF/Constraint/Presolution/Base.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Base.hs:294) defines `PresolutionM p`, and [src/MLF/Constraint/Presolution/Base.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Base.hs:334) lifts `MonadPresolution` through the phase parameter and the transformer stack.

- `Update MLF.Constraint.Presolution.StateAccess, MLF.Constraint.Presolution.Ops, and the presolution submodules that consume them to use the new typed state surface without changing the current acyclic-to-presolved behavior`: met. Shared access helpers now consume `PresolutionM p` and `Constraint p` at [src/MLF/Constraint/Presolution/StateAccess.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/StateAccess.hs:118), and the presolution loop/finalization path remains `Constraint 'Acyclic -> Constraint 'Presolved` at [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:83) and [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:144).

- `Rework MLF.Constraint.Presolution.Driver initialization and finalization so the Phase 4 entrypoint carries Constraint 'Acyclic into the working state and emits Constraint 'Presolved without presolutionInProgressRawBridge`: met. The raw bridge note and helper are gone, `mkInitialPresolutionState` now stores the incoming `Constraint 'Acyclic` directly at [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:479), and finalization emits `prConstraint :: Constraint 'Presolved` via `toPresolvedConstraint` at [src/MLF/Constraint/Presolution/Driver.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Presolution/Driver.hs:144).

- `Keep only narrowly named, owner-local phase erasure if any remaining helper still requires it after the migration, and make that justification explicit near the helper instead of reintroducing a broad graph escape hatch`: met. Production code does not reintroduce `castConstraint`, `toRawConstraintForLegacy`, or a presolution raw bridge; the only remaining phase-rebuild helper is the private `rephaseConstraint` in [src/MLF/Constraint/Types/Graph.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/src/MLF/Constraint/Types/Graph.hs:193), behind directional public helpers only.

- `Tighten focused guards around the Phase 4 boundary in test/PresolutionFacadeSpec.hs, test/RepoGuardSpec.hs, and any touched helper specs so future regressions cannot silently restore a raw presolution bridge`: met. [test/PresolutionFacadeSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/PresolutionFacadeSpec.hs:49) asserts the typed Phase-4 driver/base/state-access boundary, and [test/RepoGuardSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-232/test/RepoGuardSpec.hs:65) fails if raw-only signatures or the raw bridge return.

- `Run focused presolution/type-level validation first, then the full cabal build all && cabal test gate required for milestone-3 behavior-changing work`: met. `cabal build mlf2-test`, the focused `MLF.Constraint.Presolution facade` and `Repository guardrails` slices, and the full `cabal build all && cabal test` gate all passed.

## Decision

**APPROVED**

## Evidence

- The round stays inside the selected `milestone-3` / `direction-3a-phase-indexed-constraint` scope from [orchestrator/rounds/round-232/plan.md](/Volumes/src/mlf4/orchestrator/worktrees/round-232/orchestrator/rounds/round-232/plan.md:1). The code diff is confined to presolution owner modules plus direct regression tests; `src-public/`, solve/backend surfaces, registration files, and durable docs are unchanged.

- The minimum round-specific requirements are all satisfied at integrated HEAD. Phase 4 no longer depends on `presolutionInProgressRawBridge`; `PresolutionState`, `PresolutionM`, `MonadPresolution`, and `StateAccess` are phase-indexed; the driver carries `Constraint 'Acyclic` into working state and emits `Constraint 'Presolved`; broad graph escape hatches do not return in presolution; solve-backend/private erasure stayed out of scope; focused regression guards cover the raw-bridge failure mode; no public API widening occurred; and the full gate is green.

- Milestone-3 can move from `pending` to `done`, not merely `in-progress`. The roadmap’s completion signal in [orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap.md](/Volumes/src/mlf4/orchestrator/worktrees/round-232/orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap.md:155) requires the phase-indexed `Constraint`, directional phase entrypoints, retired broad casts, owner-local remaining erasure only, and a full green gate. The earlier rounds already closed the public phase-kind and singleton foundation, and this round removes the last Phase-4 raw-only gap while keeping roadmap meaning unchanged. Status-only closeout is therefore lawful against the existing anchors in [orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap-view.json](/Volumes/src/mlf4/orchestrator/worktrees/round-232/orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap-view.json:35).
