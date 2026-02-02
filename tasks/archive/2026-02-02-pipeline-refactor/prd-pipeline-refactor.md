# PRD: Pipeline Refactor (Typed Errors, Explicit Tracing, Canonicalizer, Module Splits)

## Introduction / Overview

The current MLF implementation is phase-structured and well-tested, but several “mega-modules” and repeated plumbing patterns (canonicalization, redirects, copy maps, tracing/debug toggles) make it difficult to safely evolve toward full paper-faithfulness (`papers/these-finale-english.txt`) and to implement remaining deltas (e.g., constructor types `Cσ`, ϕR integration, stricter Φ translatability validation).

This project refactors the codebase to:
- make errors/configuration explicit and typed across the end-to-end pipeline,
- remove the last `unsafePerformIO`-based global tracing configuration,
- centralize canonicalization + redirect chasing into a first-class abstraction,
- split mega-modules into smaller, cohesive modules,
- simplify/pick one presolution “foundation layer” style,
- split `MLF.Constraint.Types` into stable submodules (breaking change allowed).

This is a refactor-first effort: preserve inference/elaboration behavior unless an intentional deviation is documented and tested.

## Goals

- Eliminate `unsafePerformIO` and `globalTraceConfig`; tracing becomes explicitly-configured and testable.
- Reduce mega-modules by splitting: no single `src/**/*.hs` module over **~400 LOC** after the refactor (excluding intentionally-monolithic modules with an explicit justification note).
- Reduce duplicated “plumbing” patterns (canonicalization/redirects/copy maps/debug toggles), replacing them with a small number of well-tested helpers.
- Make remaining paper-faithfulness deltas easier to implement by isolating the relevant code paths (especially Φ translation and presolution planning).

## User Stories

### US-001: Introduce typed pipeline errors
**Description:** As a developer, I want `runPipelineElab` to return structured, phase-tagged errors so debugging and downstream tooling is easier than `Either String`.

**Acceptance Criteria:**
- [ ] Add a new error type (e.g., `PipelineError`) that can represent Phase 1–7 failures with constructors that preserve the original error types.
- [ ] Internal pipeline (`MLF.Elab.Run.Pipeline`) returns `Either PipelineError …`.
- [ ] Public API may be breaking: update `src-public/MLF/API.hs` and `src-public/MLF/Pipeline.hs` to expose the typed error (or intentionally keep `Either String` wrappers, documented).
- [ ] `cabal build all` passes with `-Wall` (no new warnings).
- [ ] `cabal test` passes.

### US-002: Add explicit pipeline configuration with tracing (no globals)
**Description:** As a developer, I want an explicit `PipelineConfig` (including `TraceConfig`) threaded through the pipeline so behavior is deterministic and doesn’t depend on global ambient state.

**Acceptance Criteria:**
- [ ] Introduce `PipelineConfig` containing at least `TraceConfig` (and any other needed toggles currently spread across booleans).
- [ ] Update pipeline entry points to accept `PipelineConfig` (breaking change OK).
- [ ] Configuration parsing from env remains possible but happens only at IO boundaries (e.g., `app/Main.hs`), not via globals.
- [ ] `cabal build all` and `cabal test` pass.

### US-003: Remove `globalTraceConfig` and all `unsafePerformIO` uses
**Description:** As a developer, I want tracing/debugging to be purely explicit so we can reason about evaluation order and eliminate global side effects.

**Acceptance Criteria:**
- [ ] `src/MLF/Util/Trace.hs` no longer imports or uses `System.IO.Unsafe`.
- [ ] No usage of `globalTraceConfig` remains in `src/` or `src-public/`.
- [ ] Provide a minimal migration pattern (doc note or module comment) showing how to obtain `TraceConfig` from env in `IO` and pass it down.
- [ ] `rg -n "unsafePerformIO|globalTraceConfig" src src-public` returns no matches.
- [ ] `cabal test` passes.

### US-004: Canonicalization becomes a first-class abstraction
**Description:** As a developer, I want a single “canonicalizer” abstraction (UF + redirects + optional base↔solved mapping) so we stop re-implementing NodeId/copy-map plumbing across modules.

**Acceptance Criteria:**
- [ ] Add a new module (name TBD) that provides a canonicalization API used by pipeline + elaboration.
- [ ] Consolidate redirect chasing + UF canonicalization into one reusable function/object.
- [ ] Add tests/properties for:
  - [ ] idempotence (canonicalize twice == once),
  - [ ] redirect-cycle safety (terminates; stable result),
  - [ ] witness/trace canonicalization consistency (canonicalizing ops/steps matches canonicalizing referenced nodes).
- [ ] Migrate `MLF.Elab.Run.Util` canonicalization helpers to use the new abstraction (or remove them).
- [ ] `cabal test` passes.

### US-005: Refactor pipeline copy-map / provenance handling to use the canonicalizer
**Description:** As a developer, I want the copy-map inversion, “adopt node”, and interior/copy provenance handling centralized so `MLF.Elab.Run.Pipeline` becomes smaller and easier to validate.

**Acceptance Criteria:**
- [ ] Extract copy-map / provenance construction logic out of `src/MLF/Elab/Run/Pipeline.hs` into a cohesive module with explicit inputs/outputs.
- [ ] Pipeline wiring uses the canonicalizer abstraction; no bespoke “chase redirects then UF” logic remains in `Run.Pipeline`.
- [ ] Add a focused test (or expand an existing one) that asserts provenance/copy-map behavior on at least one non-trivial program (e.g., let-polymorphism with instantiation copies).
- [ ] `cabal test` passes.

### US-006: Split `MLF.Elab.Phi` into cohesive submodules
**Description:** As a developer, I want Φ translation split into smaller modules with explicit environments so future paper-faithfulness changes (Φ/Σ/ϕR) are localized and reviewable.

**Acceptance Criteria:**
- [ ] Split `src/MLF/Elab/Phi.hs` into `src/MLF/Elab/Phi/*.hs` submodules (exact naming up to implementer).
- [ ] Keep a small top-level `MLF.Elab.Phi` facade with the current public entry points (`phiFromEdgeWitness`, `phiFromEdgeWitnessWithTrace`, etc.), or intentionally change API and document.
- [ ] Ensure the Reader-based `PhiEnv`/`PhiM` is the primary factoring mechanism (no giant single closure with hundreds of local defs).
- [ ] `cabal test` passes.

### US-007: Split `MLF.Elab.Run.Generalize` into cohesive submodules
**Description:** As a developer, I want generalization “phase” logic split so it is easier to reason about binder selection, scheme roots, and binding parents alignment.

**Acceptance Criteria:**
- [ ] Split `src/MLF/Elab/Run/Generalize.hs` into `src/MLF/Elab/Run/Generalize/*.hs` submodules.
- [ ] Keep `MLF.Elab.Run.Generalize` as a thin orchestrator re-exporting the main functions.
- [ ] No behavioral change: existing generalization/elaboration tests continue to pass.
- [ ] `cabal test` passes.

### US-008: Split `MLF.Constraint.Presolution.Plan.BinderPlan` into smaller modules
**Description:** As a developer, I want binder planning split so ordering/selection/alias decisions can evolve independently and be tested in isolation.

**Acceptance Criteria:**
- [ ] Split `src/MLF/Constraint/Presolution/Plan/BinderPlan.hs` into `src/MLF/Constraint/Presolution/Plan/BinderPlan/*.hs` submodules.
- [ ] Keep an exported facade that preserves the module’s current exports (or intentionally change exports and update callers).
- [ ] Add/adjust tests if needed to keep binder-plan behavior stable (ideally no changes).
- [ ] `cabal test` passes.

### US-009: Converge presolution “foundation” layers (pick one idiom)
**Description:** As a developer, I want presolution to use one consistent access/ops abstraction so contributors don’t have to choose between overlapping layers (`MonadPresolution`, `StateAccess`, `Ops`).

**Acceptance Criteria:**
- [ ] Decide and document the intended “foundation” style (typeclass-based vs Reader-based env vs explicit functions).
- [ ] Remove or significantly shrink redundant layers (no three competing APIs for the same operations).
- [ ] Ensure strict state usage remains consistent (avoid introducing space leaks).
- [ ] `cabal test` passes.

### US-010: Split `MLF.Constraint.Types` into stable submodules (breaking change)
**Description:** As a developer, I want `MLF.Constraint.Types` decomposed so modules can depend on smaller concepts (graph vs witness vs presolution types), reducing import collisions and recompilation churn.

**Acceptance Criteria:**
- [ ] Introduce new submodules (example layout; final naming up to implementer):
  - `MLF.Constraint.Types.Graph` (NodeId/NodeRef/TyNode/Constraint/BindParents/Gen nodes)
  - `MLF.Constraint.Types.Witness` (Expansion, EdgeWitness, InstanceOp/Step/Witness)
  - `MLF.Constraint.Types.Presolution` (Presolution/SolverState/DepGraph if still needed)
- [ ] Update imports across `src/`, `src-public/`, and `test/` to use the new modules.
- [ ] Update `mlf2.cabal` module lists accordingly (private library + public library + test suite).
- [ ] Decide whether to keep `MLF.Constraint.Types` as a compatibility re-export; if removed, document the breaking change in `CHANGELOG.md`.
- [ ] `cabal test` passes.

### US-011: Update public API + versioning for breaking changes
**Description:** As a maintainer, I want the cabal package version and public modules updated to reflect breaking API changes introduced by this refactor.

**Acceptance Criteria:**
- [ ] Bump `mlf2.cabal` version appropriately (breaking changes OK; choose a coherent bump).
- [ ] Update `CHANGELOG.md` with a “Breaking changes” section describing key API/module moves (especially `MLF.Constraint.Types` split and pipeline entry point signature changes).
- [ ] Ensure `src-public/MLF/API.hs` and `src-public/MLF/Pipeline.hs` compile and reflect the new surface area.
- [ ] `cabal build all && cabal test` passes.

## Functional Requirements

1. **FR-1:** Pipeline must provide a typed error value that preserves the original phase error information.
2. **FR-2:** Tracing must be configurable without globals; all tracing flags flow from `PipelineConfig` (or equivalent) passed explicitly.
3. **FR-3:** The codebase must contain zero `unsafePerformIO` usages after completion.
4. **FR-4:** Canonicalization/redirect chasing must be centralized into a single abstraction with tests.
5. **FR-5:** `MLF.Elab.Phi`, `MLF.Elab.Run.Generalize`, and `MLF.Constraint.Presolution.Plan.BinderPlan` must be split into smaller cohesive modules.
6. **FR-6:** `MLF.Constraint.Types` must be split into submodules; public and test builds must be updated accordingly.
7. **FR-7:** `cabal build all && cabal test` must pass at the end of each user story (or at minimum at the end of each epic; prefer per-story).

## Non-Goals (Out of Scope)

- No intentional changes to inference/elaboration semantics (unless explicitly documented as a paper-faithfulness fix with tests).
- No new surface language features, parser changes, or UI/CLI work beyond minimal wiring for configuration.
- No performance-focused rewrite unless it naturally falls out of refactoring (any perf work should be a separate PRD).
- Not implementing the remaining paper deltas themselves (Cσ/ϕR/Φ stricter checks); this refactor exists to make those changes cheaper and safer.

## Design Considerations

- Keep module export lists explicit (match existing style).
- Prefer types/newtypes to encode invariants (e.g., canonical ids vs raw ids) where it reduces ambiguity.
- Minimize import collisions: avoid re-exporting “kitchen sink” modules unless they are explicitly marked as compatibility layers.
- When shrinking mega-modules, keep the top-level module as a stable facade where practical to reduce churn.

## Technical Considerations

- **Risk: import cycles during splits.** Mitigation: introduce new modules first, migrate callers incrementally, and keep “Core” modules dependency-light.
- **Risk: accidental semantic change.** Mitigation: keep refactors behavior-preserving; use existing Hspec/QuickCheck suite and add targeted regression tests where plumbing moves.
- **Cabal module lists:** Splitting modules requires updating:
  - `library mlf2-internal` (`exposed-modules` / `other-modules`)
  - `library` (public)
  - `test-suite mlf2-test` (`other-modules` if new test modules are added)
- **Tracing:** environment-variable parsing can remain (e.g., `traceConfigFromEnv`) but must be invoked at IO edges only.
- **Paper-faithfulness:** Treat `papers/these-finale-english.txt` as source of truth; document any required deviations during refactor.

## Success Metrics

- **SM-1 (Module size):** No single Haskell module under `src/` exceeds ~400 LOC (exceptions require an explicit module note explaining why).
- **SM-2 (Plumbing reduction):** Canonicalization/redirect/copy-map plumbing is centralized (fewer ad-hoc helpers; measurable by removing duplicated canonicalization utilities and reducing bespoke code in `MLF.Elab.Run.Pipeline`).
- **SM-3 (Paper delta readiness):** Φ translation and presolution planning are organized so future work on Cσ/ϕR/Φ invariants can be done within targeted submodules without editing pipeline glue.

## Open Questions

- OQ-1: Exact threshold for “mega-module” size (400 LOC target vs 500 LOC) and whether any module should be exempt.
- OQ-2: Final shape of the public API: should `src-public/MLF/API.hs` expose typed pipeline errors/config, or keep “easy mode” wrappers?
- OQ-3: `MLF.Constraint.Types` split: do we keep a compatibility `MLF.Constraint.Types` re-export module or remove it entirely?
- OQ-4: Do we want to formalize invariants (e.g., “canonical ids only”) via newtypes in this refactor, or keep it as pure conventions?

