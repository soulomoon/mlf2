# Task Plan — Master 4-Phase Typed Two-Pass Edge DSL

## Metadata

- **Task ID:** `2026-02-10-master-4-phase-typed-two-pass-edge-dsl`
- **Created:** `2026-02-10`
- **Status:** `IN_PROGRESS`
- **Primary Plan:** `/Users/ares/.config/superpowers/worktrees/mlf4/codex/2026-02-09-new-worktree/docs/plans/2026-02-10-master-4-phase-typed-two-pass-edge-dsl-implementation-plan.md`

## Objective

Implement a thesis-faithful, type-guided two-pass presolution edge pipeline:

1. Phase 2 normalizes all residual instantiation edges into `TyExp <= τ` form.
2. Phase 4 hard-fails if any instantiation edge enters presolution with non-`TyExp` left.
3. Edge processing migrates to typed plan + interpreter (two-pass architecture).
4. Verification/docs are updated with explicit thesis-faithfulness evidence.

## Phases (revised 2026-02-11 — atomic rollout)

- [x] **Phase 1 — Typed Plan Core + Dual-Mode Planner**
  - [x] Task 1: Add failing tests for paper-shaped residual inst edges (3 tests in NormalizeSpec.hs).
  - [x] Task 2: Create typed plan module with `EdgePlanMode` discriminator (`ExpansionMode` / `LegacyDirectMode`).
  - [x] Task 3: Implement planner that classifies edges into the correct mode.

- [ ] **Phase 2 — Interpreter + Integration**
  - [ ] Task 4: Implement interpreter with dual execution paths.
  - [ ] Task 5: Refactor `processInstEdge` to plan→execute (preserving both code paths).
  - [ ] Task 6: Remove obsolete inline branch logic.

- [ ] **Phase 3 — Atomic Wrapping + Fail-Fast (gated)**
  - [ ] Task 7: Add equivalence gate tests for 7 known-failing pipeline cases.
  - [ ] Task 8: Implement residual-edge wrapping + port legacy semantics into expansion path.
  - [ ] Task 9: Enforce Phase-4 fail-fast on non-TyExp left edges in planner.
  - [ ] Task 10: Remove legacy-direct execution path (dead code after wrapping).

- [ ] **Phase 4 — Error model, matrix tests, docs**
  - [ ] Task 11: Add phase-tagged error wrappers (`PlanError` / `ExecError`).
  - [ ] Task 12: Add regression matrix tests.
  - [ ] Task 13: Documentation and task-tracking updates.
  - [ ] Task 14: Full verification gate and completion handoff.

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

