# PRD: Split Large Modules (ResultType, Target Planning, Presolution Specs)

## Introduction / Overview

The codebase is phase-structured, paper-faithful to `papers/these-finale-english.txt`, and has a strong regression suite. However, a small set of remaining “large modules” still concentrate multiple responsibilities, making review and future paper-alignment work riskier than necessary.

Current large internal modules (line counts as of 2026-02-02):
- `src/MLF/Elab/Run/ResultType.hs` (~960 LOC)
- `src/MLF/Constraint/Presolution/Plan/Target.hs` (~930 LOC)
- `src/MLF/Elab/Phi/Omega.hs` (~844 LOC)
- `src/MLF/Reify/Core.hs` (~834 LOC)
- `src/MLF/Constraint/Presolution/Plan.hs` (~810 LOC)
- `src/MLF/Constraint/Normalize.hs` (~760 LOC)
- `src/MLF/Constraint/Solve.hs` (~965 LOC)

The largest test module is also hard to navigate:
- `test/PresolutionSpec.hs` (~2900 LOC)

This project performs a refactor-only cleanup:
- split targeted mega-modules into cohesive submodules with stable facade modules,
- split oversized spec files into focused `*Spec` modules,
- keep behavior identical unless an intentional deviation is documented and tested.

## Goals

- Reduce the size and responsibility concentration of the most problematic modules:
  - Turn `MLF.Elab.Run.ResultType` into a thin facade, with implementation split across `MLF.Elab.Run.ResultType.*`.
  - Turn `MLF.Constraint.Presolution.Plan.Target` into a thin facade, with implementation split across `MLF.Constraint.Presolution.Plan.Target.*`.
- Improve test maintainability by splitting `test/PresolutionSpec.hs` into smaller spec modules without changing test behavior.
- Keep builds warning-free (`-Wall`) and preserve public API behavior.
- Make future work on remaining paper-faithfulness deltas (Cσ, ϕR integration, stricter Φ validation) safer by isolating the relevant code paths.

## User Stories

### US-001: ResultType scaffolding + util extraction
**Description:** As a developer, I want `MLF.Elab.Run.ResultType` split into submodules so the annotation/fallback paths and shared helpers are easier to reason about and modify safely.

**Acceptance Criteria:**
- [ ] Create `src/MLF/Elab/Run/ResultType/Util.hs` and move pure helpers (e.g. “contains forall in bounds” checks, implicit-forall instantiation, annotation stripping/edge collection) there.
- [ ] Keep `src/MLF/Elab/Run/ResultType.hs` as a facade that re-exports the public surface (`ResultTypeContext`, `computeResultTypeFromAnn`, `computeResultTypeFallback`) and delegates to submodules.
- [ ] Update `mlf2.cabal` (`library mlf2-internal` `other-modules`) to include the new module(s).
- [ ] `cabal build all` passes with `-Wall` (no new warnings).
- [ ] `cabal test` passes.

### US-002: ResultType annotation-path extraction
**Description:** As a developer, I want the annotation-edge result-type computation isolated so changes to annotation handling don’t require editing a ~1k LOC module.

**Acceptance Criteria:**
- [ ] Create `src/MLF/Elab/Run/ResultType/Ann.hs` containing the full implementation of `computeResultTypeFromAnn`.
- [ ] `MLF.Elab.Run.ResultType` becomes a facade delegating to `ResultType.Ann`.
- [ ] No semantic change: existing tests that exercise annotations continue to pass.
- [ ] `cabal build all && cabal test` passes.

### US-003: ResultType fallback-path extraction
**Description:** As a developer, I want the fallback result-type computation isolated so it can evolve independently of the annotation path.

**Acceptance Criteria:**
- [ ] Create `src/MLF/Elab/Run/ResultType/Fallback.hs` containing the full implementation of `computeResultTypeFallback`.
- [ ] `MLF.Elab.Run.ResultType` delegates to `ResultType.Fallback`.
- [ ] No semantic change: existing pipeline/elaboration tests pass.
- [ ] `cabal build all && cabal test` passes.

### US-004: Target planning: extract TargetPlan
**Description:** As a developer, I want `MLF.Constraint.Presolution.Plan.Target` split so each plan (Target/Gamma/TypeRoot) is maintained in its own module.

**Acceptance Criteria:**
- [ ] Create `src/MLF/Constraint/Presolution/Plan/Target/TargetPlan.hs` and move `TargetPlanInput`, `TargetPlan`, and `buildTargetPlan` there.
- [ ] Keep `src/MLF/Constraint/Presolution/Plan/Target.hs` as a facade that re-exports the full public API and delegates to submodules.
- [ ] Update `mlf2.cabal` (`library mlf2-internal` `other-modules`) to include the new module(s).
- [ ] `cabal build all && cabal test` passes.

### US-005: Target planning: extract GammaPlan
**Description:** As a developer, I want Gamma planning in a dedicated module so “Γ construction” concerns are separated from target-bound analysis.

**Acceptance Criteria:**
- [ ] Create `src/MLF/Constraint/Presolution/Plan/Target/GammaPlan.hs` and move `GammaPlanInput`, `GammaPlan`, and `buildGammaPlan` there.
- [ ] `MLF.Constraint.Presolution.Plan.Target` remains the stable facade.
- [ ] `cabal build all && cabal test` passes.

### US-006: Target planning: extract TypeRootPlan
**Description:** As a developer, I want the “type root” planning logic isolated so it can be evolved (or tested) without touching Target/Gamma logic.

**Acceptance Criteria:**
- [ ] Create `src/MLF/Constraint/Presolution/Plan/Target/TypeRootPlan.hs` and move `TypeRootPlanInput`, `TypeRootPlan`, and `buildTypeRootPlan` there.
- [ ] `MLF.Constraint.Presolution.Plan.Target` remains the stable facade.
- [ ] `cabal build all && cabal test` passes.

### US-007: Split PresolutionSpec into smaller spec modules
**Description:** As a developer, I want `test/PresolutionSpec.hs` split into smaller spec modules so it is easier to extend coverage without merge conflicts or navigation friction.

**Acceptance Criteria:**
- [ ] Split `test/PresolutionSpec.hs` into multiple modules under `test/Presolution/*Spec.hs` (naming up to implementer), each exposing `spec :: Spec`.
- [ ] Update `test/Main.hs` to import and run the new specs (and remove/retain the old PresolutionSpec facade as needed).
- [ ] Update `mlf2.cabal` `test-suite mlf2-test` `other-modules` to include the new test modules.
- [ ] No behavior change: total presolution test coverage and assertions remain the same.
- [ ] `cabal test` passes.

## Functional Requirements

1. **FR-1 (No behavior changes):** Refactors are structural only unless explicitly documented and tested as a paper-faithfulness fix.
2. **FR-2 (Stable facades):** Existing importing modules should continue to import the same top-level modules (`MLF.Elab.Run.ResultType`, `MLF.Constraint.Presolution.Plan.Target`).
3. **FR-3 (Cabal hygiene):** Any new module must be added to `mlf2.cabal` (`other-modules` or `exposed-modules` as appropriate).
4. **FR-4 (Validation):** The repo validation command must pass at each story end:
   - `cabal build all && cabal test`

## Non-Goals (Out of Scope)

- Implementing remaining paper-faithfulness deltas (Cσ, ϕR integration, stricter Φ validation).
- Performance optimizations or algorithmic changes (unless required to preserve behavior during refactor).
- This task does not *aim* to change the public API, but breaking changes are allowed if needed. If any public surface changes occur, they must be documented (CHANGELOG) and tests updated.

## Technical Considerations

- **Avoid import cycles:** Keep new submodules dependency-light; keep the existing top-level module as a facade that only imports/re-exports.
- **Keep exports explicit:** Maintain explicit export lists in new modules to avoid unintended symbol leakage and to keep `-Wall` clean.
- **Cabal lists are strict:** Remember to add new modules to:
  - `library mlf2-internal` `other-modules`
  - `test-suite mlf2-test` `other-modules` (for new test modules)
- **Paper-faithfulness:** `papers/these-finale-english.txt` remains the source of truth; this refactor should not “simplify away” paper-shaped invariants.

## Success Metrics

- `src/MLF/Elab/Run/ResultType.hs` reduced to a thin facade (minimal local logic; delegates to submodules).
- `src/MLF/Constraint/Presolution/Plan/Target.hs` reduced to a thin facade (minimal local logic; delegates to submodules).
- Presolution specs are split so each file is focused and navigable (no hard LOC target).
- No regressions: `cabal build all && cabal test` passes at the end of each user story.

## Open Questions

- OQ-1: Scope decision: keep this task limited to ResultType + Target planning + tests (do not include `src/MLF/Elab/Phi/Omega.hs`).
- OQ-2: Module size targets: decision is “no LOC targets”; the driver is cohesion + stability, not line counts.
