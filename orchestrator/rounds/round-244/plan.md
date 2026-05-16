### Selected Extraction
- Milestone: Snapshot Finalization Seam
- Milestone id: `milestone-6`
- Direction id: `direction-6a-snapshot-finalization-seam`
- Extracted item id: `round-244-finalize-internal-test-support-split`
- Roadmap id: `2026-05-16-00-architecture-deepening-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001`

### Goal
Keep `MLF.Constraint.Finalize` as the production **Snapshot Finalization** seam for finalized `PresolutionView` artifacts and solved handles, while moving exposed stepwise helpers and solved-to-view fixture mechanics behind internal or test-support modules. This round should not create a broad raw-view compatibility home.

### Approach
Current HEAD evidence:

- `src/MLF/Constraint/Finalize.hs` exports stepwise helpers: `stepSanitizeSnapshotUf`, `stepCanonicalizeConstraint`, `stepPruneSolvedBindParents`, and `stepValidateSolvedStrict`.
- `presolutionViewFromSolved` has no production users under `src/`, `src-public/`, or `app`; it is used by tests and research specs as a solved-to-view fixture adapter.
- `stepPruneSolvedBindParents` has no production users outside `MLF.Constraint.Finalize`; current uses are tests and research specs.
- `stepSanitizeSnapshotUf` has one production caller, `src/MLF/Constraint/Presolution/Plan/Env.hs`, which needs a sanitized canonical-map projection from a `PresolutionView`.
- Existing guards already assert that `MLF.Constraint.Presolution.View` does not expose `fromSolved` or raw-view legacy adapters, and that `Finalize` does not expose view-to-solved reconstruction.

Use a serial implementation. Add narrow owner-local modules instead of fan-out:

- Add `src/MLF/Constraint/Finalize/Internal.hs` for private Snapshot Finalization mechanics used by `MLF.Constraint.Finalize` and its test-support module.
- Add `src/MLF/Constraint/Finalize/TestSupport.hs` for test-only solved-to-view fixture helpers, including the existing solved-to-view adapter and prune helper needed by tests.
- Keep production `MLF.Constraint.Finalize` focused on construction entrypoints such as `presolutionViewFromSnapshot`, `finalizePresolutionViewFromSnapshot`, `validateCanonicalSnapshotStrict`, `finalizeSolvedFromSnapshot`, and `finalizeSolvedForConstraint`; remove public `step*` exports and remove `presolutionViewFromSolved` from the production facade unless a production caller is found during implementation.
- Move the `Presolution.Plan.Env` dependency off `Finalize.stepSanitizeSnapshotUf` by adding one narrow read-model accessor in `MLF.Constraint.Presolution.View`, for example `sanitizedViewCanonicalMap :: PresolutionView p -> IntMap NodeId`. This accessor must stay a read-model projection, not a raw-view compatibility adapter.
- Update tests/research specs to import `MLF.Constraint.Finalize.TestSupport` for `presolutionViewFromSolved` and the prune helper, while retaining `MLF.Constraint.Finalize` only for production construction entrypoints.
- Update `mlf2.cabal` for any new modules. Keep `MLF.Constraint.Finalize.Internal` in `other-modules`; expose `MLF.Constraint.Finalize.TestSupport` only if tests need it through the internal library, matching existing `MLF.Constraint.Solved.TestSupport` practice.
- Extend `test/RepoGuardSpec.hs` so production `src/`, `src-public/`, and `app` code cannot import `MLF.Constraint.Finalize.TestSupport`, and so `MLF.Constraint.Finalize.Internal` is allowed only to `src/MLF/Constraint/Finalize.hs` and the Finalize test-support module.
- Strengthen `test/PresolutionFacadeSpec.hs` or `test/Constraint/SolvedSpec.hs` with guards that the production `MLF.Constraint.Finalize` facade no longer exposes `stepSanitizeSnapshotUf`, `stepCanonicalizeConstraint`, `stepPruneSolvedBindParents`, `stepValidateSolvedStrict`, or `presolutionViewFromSolved`; add a positive guard that the solved-to-view test helper lives in `MLF.Constraint.Finalize.TestSupport`.
- Update `docs/architecture.md` only for the changed ownership table: `MLF.Constraint.Finalize` remains the production **Snapshot Finalization** construction authority, while `MLF.Constraint.Finalize.TestSupport` owns solved-to-view fixture mechanics for tests. Keep `MLF.Constraint.Presolution.View` described as the read-model surface.

Do not widen `src-public/`, do not add new compatibility adapters, and do not move solved/view bridges to `Presolution.View` except for the one narrow canonical-map read-model accessor described above.

### Steps
1. Re-check the current references with:
   - `rg -n "presolutionViewFromSolved|stepSanitizeSnapshotUf|stepCanonicalizeConstraint|stepPruneSolvedBindParents|stepValidateSolvedStrict" src src-public app test`
   - `rg -n "MLF.Constraint.Finalize|MLF.Constraint.Finalize.TestSupport|MLF.Constraint.Solved.Internal" src src-public app test mlf2.cabal`
2. Create `MLF.Constraint.Finalize.Internal` and move private implementations for snapshot sanitizing, canonicalizing, solved pruning, strict solved validation, and solved-to-view record construction there.
3. Refactor `MLF.Constraint.Finalize` to delegate to the internal module while exporting only production Snapshot Finalization construction entrypoints.
4. Add `MLF.Constraint.Finalize.TestSupport` with test-only exports for solved-to-view fixtures and any prune helper still required by tests.
5. Replace test/research imports and local aliases that currently call `Finalize.presolutionViewFromSolved` or `Finalize.stepPruneSolvedBindParents` with the new test-support module.
6. Add the narrow `Presolution.View` canonical-map accessor needed by `Presolution.Plan.Env`, then update `src/MLF/Constraint/Presolution/Plan/Env.hs` to stop importing `MLF.Constraint.Finalize`.
7. Update `mlf2.cabal`, guard tests, and `docs/architecture.md` for the new ownership split.
8. Run focused validation, then the full behavior-changing gate.

### Verification
- `git diff --check`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Solved"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repo guard"'`
- `cabal build all && cabal test`

Manual review checks:

- `src/MLF/Constraint/Finalize.hs` no longer exports public step helpers or solved-to-view fixture adapters.
- No production file under `src/`, `src-public/`, or `app` imports `MLF.Constraint.Finalize.TestSupport`.
- `MLF.Constraint.Presolution.View` still has no `fromSolved`, `toRawPresolutionViewForLegacy`, or broad raw-view adapter.
- `docs/architecture.md` describes the new Finalize/TestSupport split without claiming `Solved` deletion or reopening Legacy Surface Retirement.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They must conform to their schemas; `selection-record.json` remains the machine authority for selected lineage and scheduler fields.
