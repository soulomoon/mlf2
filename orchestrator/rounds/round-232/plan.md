### Selected Extraction
- Milestone: Phase-Indexed Constraint Type
- Milestone id: milestone-3
- Direction id: direction-3a-phase-indexed-constraint
- Extracted item id: item-3a-phase4-raw-bridge-retirement
- Roadmap id: 2026-05-05-00-type-level-safety-singletons-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001

### Goal
Retire the remaining Phase 4 raw bridge by carrying a phase-indexed constraint through presolution state and helper layers, while keeping already-private solve-backend erasure out of scope.

### Approach
Current HEAD already exposes the public milestone-3 boundaries: `Constraint` is phase-indexed, `normalize` / `checkAcyclicity` / `computePresolution` / `solveUnify` use directional phase signatures, and the generic graph cast surface is retired. The remaining milestone-3 gap is internal to Phase 4: `PresolutionState`, `MonadPresolution`, `StateAccess`, and `presolutionInProgressRawBridge` still pin the in-progress graph to `Constraint 'Raw`. This round should migrate that Phase 4 foundation to typed state, remove the private raw bridge if the helper migration succeeds, and leave only explicitly justified owner-local erasure outside the presolution path.

### Steps
1. Parameterize the presolution state foundation (`PresolutionState`, `PresolutionM`, and `MonadPresolution`) over the in-progress graph phase so `getConstraint` and `modifyConstraint` no longer require `Constraint 'Raw`.
2. Update `MLF.Constraint.Presolution.StateAccess`, `MLF.Constraint.Presolution.Ops`, and the presolution submodules that consume them to use the new typed state surface without changing the current acyclic-to-presolved behavior.
3. Rework `MLF.Constraint.Presolution.Driver` initialization and finalization so the Phase 4 entrypoint carries `Constraint 'Acyclic` into the working state and emits `Constraint 'Presolved` without `presolutionInProgressRawBridge`.
4. Keep only narrowly named, owner-local phase erasure if any remaining helper still requires it after the migration, and make that justification explicit near the helper instead of reintroducing a broad graph escape hatch.
5. Tighten focused guards around the Phase 4 boundary in `test/PresolutionFacadeSpec.hs`, `test/RepoGuardSpec.hs`, and any touched helper specs so future regressions cannot silently restore a raw presolution bridge.
6. Run focused presolution/type-level validation first, then the full `cabal build all && cabal test` gate required for milestone-3 behavior-changing work.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`
- Manual source check that production Phase 4 no longer depends on `presolutionInProgressRawBridge` or broad `Constraint 'Raw` state access outside explicit owner-local seams
- `cabal build all && cabal test`
