# Task Plan — Master 6-Phase Typed Two-Pass Edge DSL + Bridge Removal

## Metadata

- **Task ID:** `2026-02-10-master-4-phase-typed-two-pass-edge-dsl`
- **Created:** `2026-02-10`
- **Status:** `COMPLETE`
- **Primary Plan:** `/Users/ares/.config/superpowers/worktrees/mlf4/codex/2026-02-09-new-worktree/docs/plans/2026-02-10-master-4-phase-typed-two-pass-edge-dsl-implementation-plan.md`

## Objective

Implement a thesis-faithful, type-guided two-pass presolution edge pipeline:

1. Phase 2 normalizes all residual instantiation edges into `TyExp <= τ` form.
2. Phase 4 hard-fails if any instantiation edge enters presolution with non-`TyExp` left.
3. Edge processing migrates to typed plan + interpreter (two-pass architecture).
4. Verification/docs are updated with explicit thesis-faithfulness evidence.
5. Follow-up abstraction polish tightens type-level contracts and wrapper-ID boundaries.
6. Remove wrapper-specific interpreter bridge while keeping wrapper semantics equivalent.

## Phases (revised 2026-02-11 — atomic rollout + polish + bridge removal)

- [x] **Phase 1 — Typed Plan Core + Dual-Mode Planner**
  - [x] Task 1: Add failing tests for paper-shaped residual inst edges (3 tests in NormalizeSpec.hs).
  - [x] Task 2: Create typed plan module with `EdgePlanMode` discriminator (`ExpansionMode` / `LegacyDirectMode`).
  - [x] Task 3: Implement planner that classifies edges into the correct mode.

- [x] **Phase 2 — Interpreter + Integration**
  - [x] Task 4: Implement interpreter with dual execution paths.
  - [x] Task 5: Refactor `processInstEdge` to plan→execute (preserving both code paths).
  - [x] Task 6: Remove obsolete inline branch logic.

- [x] **Phase 3 — Atomic Wrapping + Fail-Fast (gated)**
  - [x] Task 7: Add equivalence gate tests for 7 known-failing pipeline cases.
  - [x] Task 8: Implement residual-edge wrapping + preserve legacy semantics for synthesized wrappers.
  - [x] Task 9: Enforce Phase-4 fail-fast on non-TyExp left edges in planner.
  - [x] Task 10: Remove legacy-direct execution path (dead code after wrapping).

- [x] **Phase 4 — Error model, matrix tests, docs**
  - [x] Task 11: Add phase-tagged error wrappers (`PlanError` / `ExecError`).
  - [x] Task 12: Add regression matrix tests.
  - [x] Task 13: Documentation and task-tracking updates.
  - [x] Task 14: Full verification gate and completion handoff.

- [x] **Phase 5 — Abstraction polish (type-level invariants + ID boundary)**
  - [x] Task 15: Remove `eprMode` and encode TyExp-left edges directly in `EdgePlan` payload.
  - [x] Task 16: Replace planner invariant string error with structured constructor (`ExpectedTyExpLeftInPlanner`).
  - [x] Task 17: Encapsulate synthesized wrapper `ExpVarId` allocation/checks behind `MLF.Constraint.Types.SynthesizedExpVar`.
  - [x] Task 18: Update tests/docs and rerun full verification gate.


- [x] **Phase 6 — Unified expansion execution (bridge removal)**
  - [x] Task 19: Add synthesized-wrapper characterization test(s) for identity-preserving behavior.
  - [x] Task 20: Fold wrapper handling into the unified expansion execution path.
  - [x] Task 21: Delete wrapper-specific bridge function/branch in interpreter.
  - [x] Task 22: Re-verify Phase-3 gates + full test suite and sync docs.

## Constraints / Guardrails

- Keep behavior paper-faithful to `papers/these-finale-english.txt` (per repo policy).
- Prefer minimal semantic churn while introducing typed architecture.
- Preserve external entrypoints and compatibility surface unless explicitly approved.
- Keep builds warning-free (`-Wall`) and avoid partial/unsafe fallback logic.

## Error Log

- **2026-02-11 — BLOCKER: TyExp wrapping breaks 7 downstream tests.**
  - Task 2 implementation (enforcePaperShapedInstEdges) causes 7 pipeline/elaboration failures.
  - Root cause: presolution TyExp branch (`decideMinimalExpansion` + `unifyStructure`) is not semantically equivalent to the non-TyExp branch (`solveNonExpInstantiation`).
  - Elaboration error: `PhiInvariantError "PhiReorder: missing order key for binders"`.
  - See `findings.md` for resolution options.
  - **Resolution:** Chose Option 4 (atomic rollout) with Option 1 semantics folded in. Reverted Task 2 wrapping. Reordered plan to build planner/interpreter with dual-mode execution first, then land wrapping + fail-fast atomically once equivalence is proven. Master plan updated 2026-02-11.

- **2026-02-11 — BLOCKER: Phase 3 Task 8 still breaks 7 gate cases after wrapping + dual-path port.**
  - Reintroduced post-loop normalization wrapping (`enforcePaperShapedInstEdges`) and interpreter synthesized-wrapper path to `solveNonExpInstantiation`.
  - Task 7 gate tests pass pre-wrapping, but fail post-Task 8 with:
    - `Phase 6 (elaboration): SchemeFreeVars (NodeId {getNodeId = 9}) ["t9"]`
    - `Phase 7 (type checking): TCTypeAbsVarInScope "t4"`
  - Instrumentation/evidence on the BUG-2026-02-06-002 expression:
    - Baseline (commit `0c2ab7a`) records non-identity expansions and non-empty witness/trace steps on key edges.
    - Current Task 8 branch records only `ExpIdentity` + empty witness steps for those edges.
  - This indicates the wrapping rollout is still changing semantic behavior for expansion-bearing edges beyond legacy non-`TyExp` cases.

- **2026-02-11 — BLOCKER RESOLVED: Phase 3 wrapping equivalence restored.**
  - Root cause A: synthesized-wrapper detection used TyExp body shape, which misclassified frontend TyExp edges.
  - Root cause B: Phi reorder hard-failed when narrowed binder order keys omitted copied scheme binders under wrapped runs.
  - Fix: synthesized wrappers now use reserved negative `ExpVarId` values; interpreter dispatches wrapper path on `ExpVarId < 0`.
  - Fix: `PhiReorder` now falls back to full order-key map when binder-local keys are incomplete.
  - Verification: gate suite `Phase 3 atomic wrapping equivalence gates` passes (7/7), and full validation passes (`cabal build all && cabal test` => 626 examples, 0 failures).

- **2026-02-11 — Phase 3 complete: Task 10 removed legacy-direct execution path.**
  - Interpreter now executes TyExp expansion semantics only; non-`TyExp` plans raise an internal invariant error.
  - Plan model is single-mode (`ExpansionMode` only), and `EdgeProcessing` no longer re-exports legacy-direct solve entrypoints.
  - Verification: `cabal build all && cabal test` => 626 examples, 0 failures.

- **2026-02-11 — Phase 4 complete: phase-tagged errors + matrix + verification gate landed.**
  - Added `PlanError` / `ExecError` wrappers in presolution error model and tagged planner/interpreter boundaries.
  - Added Phase 4 regression matrix coverage in `ExpansionSpec`, `EdgeTraceSpec`, `WitnessSpec`, and `PipelineSpec` (including annotation-edge weaken suppression checks).
  - Verification: targeted planner-tag check passes, matrix-focused checks pass, and full validation is green (`cabal build all && cabal test` => 630 examples, 0 failures).


- **2026-02-11 — Phase 5 complete: abstraction polish landed.**
  - Plan payload now stores refined `ResolvedTyExp` (no `eprMode`) so resolved plans encode TyExp-left shape directly.
  - Planner fail-fast now uses structured invariant error `ExpectedTyExpLeftInPlanner` (wrapped by `PlanError`) instead of stringly `InternalError` payloads.
  - Synthesized wrapper `ExpVarId` allocation/check logic moved behind `MLF.Constraint.Types.SynthesizedExpVar` (`initSynthExpVarSupply`, `takeSynthExpVar`, `isSynthesizedExpVar`).
  - Verification: targeted EdgePlanner/EdgeInterpreter suites pass and full gate remains green (`cabal build all && cabal test`).


- **2026-02-11 — Phase 6 complete: wrapper bridge removed under unified execution.**
  - Added characterization regression for synthesized wrappers against forall targets (`ExpIdentity` preservation).
  - Interpreter now executes one expansion-oriented function for all TyExp-left plans; synthesized wrappers are handled in-place (no separate bridge function).
  - Preserved wrapper semantics: forced `ExpIdentity`, direct body-target instantiation solving, and unchanged witness/trace recording shape.
  - Verification: `Edge interpreter` matcher (4/4), `Phase 3 atomic wrapping equivalence gates` (7/7), and full gate (`cabal build all && cabal test` => 631 examples, 0 failures).

- **2026-02-11 — Post-phase cleanup: removed `EdgeStage` phantom index.**
  - Simplified `EdgePlan` from `EdgePlan (s :: EdgeStage)` to a concrete resolved plan record.
  - Removed `edgePlanStage`/`StageResolved` scaffolding and updated planner/interpreter signatures to use `EdgePlan` directly.
  - Updated `EdgePlannerSpec` to assert concrete plan payload fields instead of stage tags.
  - Verification: `cabal build mlf2-test` and full gate (`cabal build all && cabal test` => 631 examples, 0 failures).
