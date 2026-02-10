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
