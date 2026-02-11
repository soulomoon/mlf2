# Progress Log — Master 4-Phase Typed Two-Pass Edge DSL

## 2026-02-10

### Session initialization

- Confirmed desired direction via brainstorming:
  - High-change refactor allowed.
  - Primary metric: stronger type-guided architecture.
  - Rollout mode: phased.
  - Phase 1 focus: typed edge-processing DSL.
  - Explicit policy: strict paper-shaped representation (`TyExp-left` residual inst edges) and Phase-4 fail-fast assertion.

### Planning artifacts created

- Added master implementation plan:
  - `/Users/ares/.config/superpowers/worktrees/mlf4/codex/2026-02-09-new-worktree/docs/plans/2026-02-10-master-4-phase-typed-two-pass-edge-dsl-implementation-plan.md`

- Added task tracking folder and files:
  - `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md`
  - `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md`
  - `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md`

### Next step

- Start execution in a separate session using `@superpowers:executing-plans` against the master 4-phase plan.

## 2026-02-11

### Phase 1 execution started

- Read plan, AGENTS.md, and all task tracking files.
- Explored codebase: Normalize.hs, EdgeProcessing.hs, NormalizeSpec.hs, EdgeTraceSpec.hs, Types/Graph.hs, test/Main.hs, mlf2.cabal, SpecUtil.hs, Expansion.hs, Base.hs.
- Verified clean baseline: `cabal build all && cabal test` → 606 examples, 0 failures.

### Task 1: Add failing tests for paper-shaped residual inst edges

- Added 3 tests to `NormalizeSpec.hs` under `describe "Paper-shaped residual inst edges"`:
  - "wraps residual Var <= Var edges with TyExp-left form"
  - "wraps residual type-error edges too"
  - "allocates distinct ExpVarIds for synthesized wrappers"
- Added `IntSet` and `lookupNodeMaybe` imports.
- Ran tests: all 3 FAIL as expected (residual edges not wrapped).

### Task 2: Implement residual-edge wrapping in normalization

- Extended `NormalizeState` with `nsNextExpVar :: !Int`.
- Added `maxExpVarKeyOrMinus1` to seed ExpVarId counter.
- Added `freshExpVarIdNorm` helper.
- Added `enforcePaperShapedInstEdges` and `wrapInstEdgeLeft` functions.
- Initially placed wrapping inside `normalizeLoop` — caused "mixed reflexive" test to fail (wrapping prevented grafting in next iteration).
- Moved wrapping to post-loop: `execState (normalizeLoop >> enforcePaperShapedInstEdges)`.
- Paper-shaped tests: 3/3 PASS.

### BLOCKER: TyExp wrapping breaks 7 downstream tests

- Full test suite: 609 examples, 7 failures.
- All failures in pipeline/elaboration phases (not presolution).
- Root cause: synthesized TyExp wrappers route edges through the TyExp branch in `processInstEdge`, which uses `decideMinimalExpansion` + `unifyStructure` instead of `solveNonExpInstantiation`. These have different semantics.
- Elaboration error: `PhiInvariantError "PhiReorder: missing order key for binders"`.
- Logged blocker in findings.md with 4 resolution options.

### BLOCKER RESOLVED: Chose Option 4 (atomic rollout)

- User chose Option 4 with Option 1 semantics folded in.
- Reverted Task 2 wrapping implementation from `Normalize.hs` (kept Task 1 tests).
- Verified clean baseline: 609 examples, 3 failures (only the 3 intentionally-failing Task 1 tests).
- Rewrote master plan with atomic rollout sequence:
  - Phase 1: Typed plan core + dual-mode planner (`ExpansionMode` / `LegacyDirectMode`)
  - Phase 2: Interpreter + integration (preserving both code paths)
  - Phase 3: Atomic wrapping + fail-fast (gated by equivalence tests on the 7 known-failing cases)
  - Phase 4: Error model, matrix tests, docs
- Updated task_plan.md phases and error log.
- Updated findings.md with resolution.
- Master plan file updated: `docs/plans/2026-02-10-master-4-phase-typed-two-pass-edge-dsl-implementation-plan.md`

### Task 2: Create typed plan module (Plan.hs)

- Created `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs`:
  - `EdgePlanMode`: `ExpansionMode` | `LegacyDirectMode`
  - `EdgeStage`: `StageResolved` (DataKinds-promoted)
  - `EdgePlan (s :: EdgeStage)` GADT with `EdgePlanResolved` constructor
  - Fields: `eprEdge`, `eprLeftNode`, `eprRightNode`, `eprLeftCanonical`, `eprRightCanonical`, `eprMode`, `eprAllowTrivial`, `eprSuppressWeaken`
  - Helpers: `edgePlanStage`, `edgePlanMode`, `edgePlanEdge`, `mkEmptyResolvedPlan`
- Created `test/Presolution/EdgePlannerSpec.hs` with 4 smoke tests.
- Wired into `mlf2.cabal` (exposed-modules) and `test/Main.hs`.
- All 4 tests PASS. Committed: `e44023f`.

### Task 3: Implement planner module (Planner.hs)

- Created `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`:
  - `planEdge :: InstEdge -> PresolutionM (EdgePlan 'StageResolved)`
  - Reads canonicalized nodes via `getNode` / `getCanonicalNode`
  - Classifies: `TyExp` left → `ExpansionMode`, otherwise → `LegacyDirectMode`
  - Threads `cLetEdges` → `eprAllowTrivial`, `cAnnEdges` → `eprSuppressWeaken`
- Added 4 planner behavior tests to `EdgePlannerSpec.hs`:
  - "returns ExpansionMode when left node is TyExp"
  - "returns LegacyDirectMode when left node is non-TyExp"
  - "threads let-edge flag into allowTrivial"
  - "threads ann-edge flag into suppressWeaken"
- All 4 tests PASS. Committed: `409b7e4`.

### Phase 1 verification

- Full suite: 617 examples, 3 failures (only the 3 expected Task 1 NormalizeSpec tests).
- Phase 1 complete.

### Phase 3 execution (Tasks 7–8) started

- Confirmed prior Phase 2 commits exist (`b094220`, `0c2ab7a`, `7d9c2ba`) and loaded the master plan + task tracking files.
- Found local pre-existing edits in `Normalize.hs` and `Interpreter.hs`; restored both to Phase 2 baseline before Task 7 verification.

### Task 7: Add and verify equivalence gate tests

- Added explicit gate block in `test/PipelineSpec.hs` under `describe "Phase 3 atomic wrapping equivalence gates"` covering 7 required cases:
  - make-let-mismatch no-name-leak
  - let-c1-apply-bool sentinel matrix
  - let-c1-apply-bool strict target matrix
  - checked-authoritative invariant
  - thesis target unchecked/checked Int
  - `\\y. let id = (\\x. x) in id y` shape gate
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 atomic wrapping equivalence gates"'`
  - Result: 7 examples, 0 failures (PASS on pre-wrapping baseline).

### Task 8: Implement wrapping + legacy-semantics port (blocked)

- Implemented in `src/MLF/Constraint/Normalize.hs`:
  - `NormalizeState` gains `nsNextExpVar`
  - added `maxExpVarKeyOrMinus1`, `freshExpVarIdNorm`
  - added post-loop `enforcePaperShapedInstEdges` + `wrapInstEdgeLeft`
  - inherited wrapper binding parent from wrapped body
- Implemented in `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`:
  - added synthesized-wrapper expansion path dispatch
  - synthesized wrappers set `ExpIdentity`, record witness/trace, and call `solveNonExpInstantiation body target`
  - real expansion path retained for forall-bodied TyExp
- Verification attempt:
  - `cabal test ... --match "Phase 3 atomic wrapping equivalence gates"`
  - Result: 7/7 FAIL with `SchemeFreeVars` / `TCTypeAbsVarInScope`

### Root-cause investigation (systematic-debugging)

- Compared BUG-2026-02-06-002 pipeline internals against clean Phase 2 commit `0c2ab7a` using a detached comparison worktree (`/tmp/mlf4_phase2_compare`).
- Key evidence:
  - Baseline presolution records non-identity expansions (`ExpInstantiate ...`) and non-empty witness steps for key edges.
  - Current Task 8 branch records only `ExpIdentity` + empty witness steps.
- Conclusion:
  - Task 8 implementation still changes expansion semantics in ways that violate elaboration invariants.
  - Phase 3 cannot proceed to Task 9 until this semantic drift is resolved.


### Phase 3 blocker fix (continuation)

- Reproduced gate failures on current Task 8 branch and confirmed blocker symptoms (`SchemeFreeVars`, `TCTypeAbsVarInScope`, then `PhiReorder` ordering failures).
- Performed baseline-vs-current differential debugging against commit `0c2ab7a` in a temporary worktree to compare normalized edges, expansions, witnesses, and traces on BUG-2026-02-06-002.
- Implemented fix set:
  - `src/MLF/Constraint/Normalize.hs`: allocate synthesized-wrapper expansion variables from negative-ID space.
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`: detect synthesized wrappers by `ExpVarId < 0` and preserve real expansion path for frontend TyExp edges.
  - `src/MLF/Elab/Phi/Omega.hs`: fallback to full order-key map when narrowed binder-key map is incomplete during Phi reorder.
- Verification sequence:
  - Gate check: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 atomic wrapping equivalence gates"'` => 7/7 pass.
  - Full validation: `cabal build all && cabal test` => 626 examples, 0 failures.
- Status update: Phase 3 Task 8 complete/unblocked; proceed to Task 9 (planner fail-fast) next.

### Task 9: Enforce planner fail-fast on non-TyExp left edges

- Followed TDD sequence:
  - Added failing test in `test/Presolution/EdgePlannerSpec.hs` asserting planner rejects non-`TyExp` left edge with error containing `expected TyExp-left`.
  - Verified RED via:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails fast when left node is non-TyExp"'`
    - Result before code change: FAIL (planner still accepted non-`TyExp` edge).
- Implemented planner fail-fast in `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`:
  - Imported `throwError` and `PresolutionError(..)`.
  - Replaced non-`TyExp` mode classification with `InternalError` fail-fast.
- Updated tests to match new phase invariant:
  - `test/Presolution/EdgePlannerSpec.hs`:
    - replaced legacy-mode expectation with fail-fast expectation
    - converted let/ann flag-threading tests to use `TyExp`-left edges
  - `test/Presolution/EdgeInterpreterSpec.hs`:
    - replaced legacy-direct success case with planner-rejects-non-`TyExp` assertion
- Verification:
  - targeted checks pass:
    - `--match "fails fast when left node is non-TyExp"`
    - `--match "returns ExpansionMode when left node is TyExp"`
  - full regression pass:
    - `cabal build all && cabal test` => 626 examples, 0 failures.

### Task 10: Remove legacy-direct execution path

- Implemented Task 10 runtime/model cleanup:
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
    - removed mode dispatch and deleted `executeLegacyDirectPath`.
    - `executeEdgePlan` now executes expansion path directly and asserts TyExp-left invariant.
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs`
    - removed `LegacyDirectMode`; plan mode is now single-constructor `ExpansionMode`.
    - removed `edgePlanMode` helper (tests now use `eprMode`).
  - `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
    - removed obsolete re-export/import of `solveNonExpInstantiation`.
- Updated tests to align with Phase-3/4 invariants:
  - `test/Presolution/EdgePlannerSpec.hs`: removed legacy-mode classification assertions; validated `ExpansionMode` via `eprMode`.
  - `test/Presolution/EdgeInterpreterSpec.hs`: replaced legacy-mode-tag dispatch test with TyExp dispatch invariance (`dispatches TyExp plans independent of mode tag`).
- Verification:
  - `cabal build all && cabal test` => 626 examples, 0 failures.
- Status update: Phase 3 (Tasks 7–10) complete; ready to begin Phase 4 Task 11.

### Task 11: Add phase-tagged errors (`PlanError` / `ExecError`)

- TDD RED step:
  - Updated `test/Presolution/EdgePlannerSpec.hs` to expect `PlanError (InternalError ...)` on non-`TyExp` planner fail-fast.
  - Verified RED via `cabal test mlf2-test --test-show-details=direct --test-options='--match "planner tag"'` (compile failure: `PlanError` constructor missing).
- GREEN implementation:
  - `src/MLF/Constraint/Presolution/Base.hs`: added `PlanError PresolutionError` and `ExecError PresolutionError` constructors.
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`: wrapped fail-fast invariant error in `PlanError`.
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`: wrapped interpreter execution with `ExecError` tagging (`catchError ... toExecError`).
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "planner tag"'` => 1 example, 0 failures.

### Task 12: Add regression matrix tests

- Added matrix-style regression coverage in requested suites:
  - `test/Presolution/ExpansionSpec.hs`: constructor coverage for identity / instantiate / forall-intro / compose.
  - `test/Presolution/EdgeTraceSpec.hs`: identity-path trace regression (`ExpIdentity`, empty binder-arg list).
  - `test/Presolution/WitnessSpec.hs`: compose expansion retains interleaved intro + omega witness steps.
  - `test/PipelineSpec.hs`: annotation-edge weaken suppression while preserving expansion assignment presence.
- Targeted verification:
  - `--match "Phase 4 regression matrix"` => 3 examples, 0 failures.
  - `--match "identity expansion with empty binder-argument trace"` => 1 example, 0 failures.
- Full Task 12 regression gate:
  - `cabal build all && cabal test` => 630 examples, 0 failures.

### Task 13: Documentation and tracking sync

- Synced phase-4 state in project docs and task logs:
  - `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`
  - `tasks/.../task_plan.md`, `tasks/.../findings.md`, `tasks/.../progress.md`

### Task 14: Full verification gate and scope check

- Targeted plan commands executed:
  - `--match NormalizeSpec`, `--match EdgePlanner`, `--match EdgeInterpreter` (no matching examples under current Hspec labels; all commands completed successfully).
- Full verification:
  - `cabal build all && cabal test` => 630 examples, 0 failures.
- Diff-scope check:
  - `git status --short` captured intended Phase 3/4 touched files (plus pre-existing staged context from earlier approved phases).
- Status update: Phase 4 complete.

### Phase 5: Abstraction polish (type-level contracts + ID boundary)

- Added follow-up Phase 5 scope per user request: planner/interpreter invariants + `ExpVarId` boundary + plan payload simplification.
- Refactored `EdgePlan` model:
  - Removed `EdgePlanMode` and `eprMode`.
  - Added `ResolvedTyExp` payload to encode TyExp-left shape directly in resolved plans.
- Reworked planner fail-fast error path:
  - Introduced structured invariant constructor `ExpectedTyExpLeftInPlanner` in `PresolutionError`.
  - Planner now emits `PlanError (ExpectedTyExpLeftInPlanner edgeId leftNode)`.
- Encapsulated synthesized wrapper `ExpVarId` management:
  - New module: `MLF.Constraint.Types.SynthesizedExpVar`.
  - `Normalize` now uses `SynthExpVarSupply` with `initSynthExpVarSupply` / `takeSynthExpVar`.
  - Interpreter wrapper detection now uses shared `isSynthesizedExpVar`.
- Updated/realigned tests:
  - `EdgePlannerSpec`: removed mode assertions, added refined TyExp payload assertions and structured invariant checks.
  - `EdgeInterpreterSpec`: updated helper-plan construction to `ResolvedTyExp` and renamed synthesized-wrapper dispatch check.
- Verification sequence:
  - `cabal build all` (green)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types"'` (7 examples, 0 failures)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge interpreter"'` (3 examples, 0 failures)
- Final full verification:
  - `cabal build all && cabal test` => 630 examples, 0 failures.

### Phase 6: Unified expansion execution, remove wrapper bridge

- User requested: eliminate wrapper-specific interpreter execution branch while preserving wrapper semantics, then delete bridge code.
- Added characterization test before refactor:
  - `EdgeInterpreterSpec`: `keeps synthesized-wrapper expansions at ExpIdentity against forall targets`.
- Refactored `Interpreter`:
  - Removed separate `executeSynthesizedWrapperPath` and `executeRealExpansionPath` split.
  - Introduced one unified execution function (`executeUnifiedExpansionPath`) with localized wrapper handling for expansion/unification choice.
  - Preserved wrapper behavior: synthesized wrappers force `ExpIdentity` and solve `(body <= target)` via `solveNonExpInstantiation`.
- Verification sequence:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps synthesized-wrapper expansions at ExpIdentity against forall targets"'` => 1 example, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge interpreter"'` => 4 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 atomic wrapping equivalence gates"'` => 7 examples, 0 failures.
- Final full verification after bridge removal:
  - `cabal build all && cabal test` => 631 examples, 0 failures.

### Post-phase cleanup: remove `EdgeStage`

- User requested simplification after thesis alignment review: drop `EdgeStage` phantom index unless it enforces real transitions.
- Implemented:
  - `Plan.hs`: removed `EdgeStage`, `edgePlanStage`, and phantom-indexed `EdgePlan (s :: EdgeStage)`.
  - `Planner.hs`: `planEdge :: InstEdge -> PresolutionM EdgePlan`.
  - `Interpreter.hs`: `executeEdgePlan :: EdgePlan -> PresolutionM ()`.
  - `EdgePlannerSpec.hs`: replaced stage-tag assertion with concrete resolved-payload assertion.
- Verification:
  - `cabal build mlf2-test` => pass.
  - `cabal build all && cabal test` => 631 examples, 0 failures.
