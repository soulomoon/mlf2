# Master 4-Phase Typed Two-Pass Edge DSL Implementation Plan (Historical / Superseded)

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.
>
> **Status (2026-02-11):** Superseded. Retained for execution history only.
>
> **Do not execute this file as the current runbook.** Use current tracking/docs instead:
> - `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md`
> - `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md`
> - `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md`
> - `implementation_notes.md`
> - `TODO.md`

**Goal:** Build a thesis-faithful, type-guided two-pass presolution edge-processing architecture where Phase 2 guarantees paper-shaped `TyExp ≤ τ` instantiation edges and Phase 4 executes only typed, legal edge plans.

**Architecture (revised 2026-02-11):** Implement in four phases using an **atomic rollout** strategy. Build the planner/interpreter infrastructure first with a dual-mode execution split (`expansion-semantics` for TyExp edges, `legacy-direct-semantics` for non-TyExp edges). Then land wrapping + fail-fast atomically once equivalence is proven. This avoids the blocker where wrapping alone reroutes edges through semantically different code paths.

**Blocker context:** Naive wrapping of all residual inst edges with TyExp breaks 7 pipeline/elaboration tests because `processInstEdge`'s TyExp branch (`decideMinimalExpansion` + `unifyStructure`) is not equivalent to the non-TyExp branch (`solveNonExpInstantiation`). The non-TyExp branch has special binding-permission and scheme-root logic. See `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md` for details.

**Tech Stack:** Haskell (GHC2021 + selected extensions for typed plans), Cabal, Hspec, existing presolution/normalization modules.

---

## Current Architecture Snapshot (authoritative)

- `Normalize` enforces paper-shaped residual edges and allocates synthesized wrapper IDs through `SynthExpVarSupply` (no ad hoc `nsNextExpVar` counter path).
- `EdgePlan` is a concrete resolved record (no `EdgeStage`, no `edgePlanStage`, no `EdgePlanMode` discriminator).
- `Planner` fail-fast is structured: `PlanError (ExpectedTyExpLeftInPlanner edgeId leftNode)`.
- `Interpreter` uses one unified TyExp execution path; synthesized wrappers are detected via `isSynthesizedExpVar` and still use `solveNonExpInstantiation` for body-target semantics.
- Presolution phase boundary errors are tagged as `PlanError` / `ExecError`.
- Verification baseline after cleanup: `cabal build all && cabal test` => `631 examples, 0 failures`.

---

## Historical Timeline (completed)

This section replaces the old imperative task runbook with a compact execution record.

### Phase 1 (completed)

- Added initial paper-shape gate coverage in `NormalizeSpec` for residual-inst wrapping behavior.
- Introduced planner/interpreter module split and early type-model scaffolding.
- End-of-phase status: implementation progressed, but dual-path model later proved transitional.

### Phase 2 (completed)

- Routed edge processing through planner then interpreter (`processInstEdge` orchestration).
- Extracted shared solve helpers and removed redundant inline edge-processing glue.
- End-of-phase status: two-pass architecture in place; regression baseline remained stable.

### Phase 3 (completed; blockers resolved)

- Added equivalence gate suite for known failing pipeline/elaboration cases.
- Landed residual-edge wrapping in normalization with synthesized wrapper handling.
- Resolved blocker family by:
  - reserving synthesized wrapper IDs in a negative `ExpVarId` space,
  - routing wrapper semantics explicitly on synthesized-ID detection,
  - fixing binder-order fallback in Phi reordering.
- Enforced planner fail-fast for non-`TyExp` left edges and removed legacy-direct runtime mode split.

### Phase 4 (completed)

- Added phase-tagged presolution errors (`PlanError` / `ExecError`).
- Added regression matrix coverage across expansion, trace, witness, and pipeline suites.
- End-of-phase status: full validation green.

### Phase 5 (completed)

- Tightened abstraction contracts:
  - removed runtime mode tagging from plans,
  - introduced refined TyExp payloads,
  - replaced stringly invariant failures with `ExpectedTyExpLeftInPlanner`,
  - centralized synthesized wrapper ID allocation/checking in `MLF.Constraint.Types.SynthesizedExpVar`.

### Phase 6 (completed)

- Removed wrapper-specific interpreter bridge function.
- Consolidated to one unified TyExp execution family while preserving wrapper identity semantics (`ExpIdentity` retention + direct body-target solve semantics).
- Added characterization regression for synthesized-wrapper/forall target behavior.

### Post-phase cleanup (completed)

- Removed `EdgeStage` and `edgePlanStage` after confirming the index no longer encoded distinct runtime states.
- `EdgePlan` is now a concrete resolved plan record with simplified planner/interpreter signatures.

---

## Verification Outcome

- Final validation command: `cabal build all && cabal test`
- Final status: `631 examples, 0 failures`

---

## Canonical Execution Record

For detailed chronology, command logs, and blocker analysis, use:

- `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md`
- `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md`
- `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md`
