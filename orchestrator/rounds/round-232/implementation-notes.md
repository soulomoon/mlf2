### Changes Made
- `src/MLF/Constraint/Presolution/Base.hs`: parameterized `PresolutionState` and `PresolutionM` over the in-progress `Phase`, added the `PresolutionPhaseOf` associated type to `MonadPresolution`, and lifted the class through `ReaderT`/`StateT` so presolution code can carry `Constraint p` directly.
- `src/MLF/Constraint/Presolution/Driver.hs`: initialized Phase 4 from `Constraint 'Acyclic`, removed the private `presolutionInProgressRawBridge`, and finalized the loop by converting the resulting in-progress graph to `Constraint 'Presolved`.
- `src/MLF/Constraint/Presolution/StateAccess.hs`: retargeted the shared access helpers to `PresolutionM p` so canonicalization, union-find, and constraint reads/writes stay phase-indexed instead of forcing `'Raw`.
- `src/MLF/Constraint/Presolution/Ops.hs`: generalized presolution state operations to the active phase so fresh-node, binding, and union-find helpers continue to work without reintroducing a raw bridge.
- `src/MLF/Constraint/Presolution/Copy.hs`: updated copy helpers to consume the phase-indexed presolution interfaces rather than a raw-only state.
- `src/MLF/Constraint/Presolution/Expansion.hs`: updated expansion helpers to operate through the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/ForallIntro.hs`: updated forall-introduction helpers to operate through the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/Materialization.hs`: updated materialization helpers to operate through the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/Rewrite.hs`: parameterized `RebuildBindParentsEnv` over the source phase so bind-parent reconstruction no longer assumes a raw graph.
- `src/MLF/Constraint/Presolution/Unify.hs`: updated presolution unification helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/Validation.hs`: updated validation helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/Witness.hs`: updated witness construction helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`: updated witness normalization helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/EdgeUnify.hs`: threaded the new phase-indexed presolution monad through the thin EdgeUnify facade.
- `src/MLF/Constraint/Presolution/EdgeUnify/State.hs`: generalized EdgeUnify state helpers to the active presolution phase.
- `src/MLF/Constraint/Presolution/EdgeUnify/Omega.hs`: generalized omega execution helpers to the active presolution phase.
- `src/MLF/Constraint/Presolution/EdgeUnify/Unify.hs`: generalized edge-unification helpers to the active presolution phase.
- `src/MLF/Constraint/Presolution/EdgeProcessing.hs`: threaded the phase-indexed presolution state through the edge-processing loop.
- `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`: updated interpreter helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`: updated planner helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/EdgeProcessing/Solve.hs`: updated edge-processing solve helpers to consume the phase-indexed presolution interfaces.
- `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`: updated edge-processing unification helpers to consume the phase-indexed presolution interfaces.
- `test/PresolutionFacadeSpec.hs`: replaced the old raw-bridge source assertions with guards for `PresolutionState p`, `PresolutionM p`, typed `StateAccess`, and the `Constraint 'Acyclic -> Constraint 'Presolved` Driver boundary.
- `test/RepoGuardSpec.hs`: added a repository guard that fails if the Phase 4 raw bridge or raw-typed presolution state/access signatures return.
- `test/Thesis/ObligationPropertySpec.hs`: updated the local empty presolution-state helper to the now-parameterized `PresolutionState 'Raw`.
- `orchestrator/rounds/round-232/implementation-notes.md`: recorded the round change summary and validation evidence.

### Tests
- `cabal build mlf2-test`: rebuilt the focused test target after the phase-indexed presolution migration.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`: verified the presolution facade guards, including the new typed Phase 4 boundary assertions.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`: verified the repository guardrails, including the new raw-bridge regression guard.
- `rg -n "presolutionInProgressRawBridge|psConstraint :: Constraint 'Raw|getConstraintAndCanonical :: PresolutionM 'Raw|getConstraintAndUnionFind :: PresolutionM 'Raw|putConstraintAndUnionFind :: Constraint 'Raw" src/MLF/Constraint/Presolution src/MLF/Constraint/Presolution/**/*.hs`: returned no matches, confirming the retired raw bridge/state signatures are absent from the presolution pipeline.
- `git diff --check`: passed.
- `cabal build all`: passed.
- `cabal test`: passed (`2567 examples, 0 failures`).

### Notes
This round stays within the settled non-cyclic-graph architecture: Phase 4 now keeps `Constraint 'Acyclic` in working state and emits `Constraint 'Presolved` only at the Driver finalization boundary.

Solve-backend and private erasure work remained untouched, per plan scope.
