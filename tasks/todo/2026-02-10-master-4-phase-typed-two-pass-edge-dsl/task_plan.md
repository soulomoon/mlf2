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

## Phases

- [ ] **Phase 1 — Invariant Foundation**
  - Paper-shaped residual edge wrapping in normalization.
  - Phase-4 fail-fast assertion for non-`TyExp` left edges.

- [ ] **Phase 2 — Typed Plan Core**
  - Add stage-indexed edge-plan types.
  - Add planner module and tests.

- [ ] **Phase 3 — Interpreter + Integration**
  - Add interpreter module.
  - Refactor `processInstEdge` into plan→execute orchestration.

- [ ] **Phase 4 — Error model, matrix tests, docs**
  - Add planner/executor error tagging.
  - Add regression matrix.
  - Run full verification (`cabal build all && cabal test`).

## Constraints / Guardrails

- Keep behavior paper-faithful to `papers/these-finale-english.txt` (per repo policy).
- Prefer minimal semantic churn while introducing typed architecture.
- Preserve external entrypoints and compatibility surface unless explicitly approved.
- Keep builds warning-free (`-Wall`) and avoid partial/unsafe fallback logic.

## Error Log

- _No execution errors recorded yet._

