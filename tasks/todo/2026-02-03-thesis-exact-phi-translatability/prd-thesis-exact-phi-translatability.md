# PRD: Thesis-Exact Φ Translatability (Eliminate “Skip” Cases)

## Introduction / Overview

Today Φ (Ω→instantiation translation) can silently ignore instance operations that violate the thesis’ “translatable presolution / normalized witness” preconditions.

Concrete example: in `src/MLF/Elab/Phi/Omega.hs` the `OpRaise` case checks membership in `I(r)` (`EdgeTrace.etInterior`) and currently **skips** the operation when the target is outside the interior (see the `interiorSet` check around line ~482).

The thesis (`papers/these-finale-english.txt` §15.2.10 and §15.3.4 / Fig. 15.3.4) assumes that witness operations for *translatable* presolutions only transform nodes transitively flexibly bound to the expansion root, hence admit computation contexts. Encountering operations outside `I(r)` (or without a computation context) indicates a **non-translatable** presolution and must be rejected, not silently ignored.

This PRD makes Φ translation thesis-exact by:
- enforcing normalized-witness invariants at presolution time using exact `I(r)`,
- refusing to translate invalid witnesses in Φ (defense in depth),
- requiring `EdgeTrace` for Φ so interior/context checks are meaningful.

## Goals

- Fail fast on non-translatable witnesses instead of skipping operations.
- Align normalization/validation with thesis Fig. 15.3.4 (conditions 1–5) using exact `I(r)`.
- Ensure Φ translation is deterministic and uses computation contexts only where defined.
- Provide regression tests that demonstrate the old “skip” behavior is eliminated.

## User Stories

### US-001: Presolution rejects non-translatable witnesses
**Description:** As a maintainer, I want presolution to reject any edge witness whose normalized Ω operations violate Fig. 15.3.4 invariants, so Φ never receives a non-translatable witness.

**Acceptance Criteria:**
- [ ] For every edge witness produced by presolution, normalization is performed with `interior = I(r)` in the **same node space as the witness ops** (no “interior inflation” via `copiedNodes`).
- [ ] After normalization, `validateNormalizedWitness` succeeds on the resulting Ω ops for that edge.
- [ ] If validation fails, presolution returns a structured error (not `InternalError`) that includes `EdgeId` and the specific `OmegaNormalizeError` (e.g. `OpOutsideInterior`, `RaiseNotUnderRoot`, `MalformedRaiseMerge`).
- [ ] `cabal build all` remains warning-free.

### US-002: Witness normalization uses exact interior in the correct node space
**Description:** As a maintainer, I want witness normalization to compute `I(r)` in the same node-id space as normalized steps, so “copied” nodes do not accidentally bypass translatability checks.

**Acceptance Criteria:**
- [ ] In `MLF.Constraint.Presolution.WitnessNorm`, replace `interiorNorm = interiorExact ∪ copiedNodes(copyMap)` with an **interior rewrite** that maps `interiorExact` through the same `copyMap` rewrite used on the steps (i.e. `interiorRewrite = { rewriteNode n | n ∈ interiorExact }`).
- [ ] `stripExteriorOps` and `validateNormalizedWitness` are evaluated with this rewritten interior.
- [ ] Add/adjust a presolution-level spec that reproduces the previous failure mode (an op on a copied/exterior node) and now fails with the new structured error.

### US-003: Φ translation refuses invalid operations (no “skip” paths)
**Description:** As a maintainer, I want Φ translation to reject any witness op that violates translatability/normalization preconditions, rather than silently dropping it.

**Acceptance Criteria:**
- [ ] In `MLF.Elab.Phi.Omega`, remove the `OpRaise` branch that calls `go ... rest` when the target is outside `interiorSet`; replace it with an error (use `ElabError.ValidationFailed` or `InstantiationError`) that states the edge id, the op, and the reason (“outside I(r)” / “not transitively flexibly bound” / “no computation context”).
- [ ] Apply the same strictness to any other op cases that currently skip due to non-binder targets, missing interior membership, etc., **except** for the thesis-sanctioned identity cases on rigid nodes (Fig. 15.3.4 “operations on a rigid node translate to ε”).
- [ ] Existing “missing context” failures (e.g. `OpRaise (non-spine): missing context`) are surfaced as translatability/validation errors, not masked.

### US-004: Φ requires EdgeTrace (no best-effort mode)
**Description:** As a maintainer, I want Φ translation to require `EdgeTrace` so that interior membership and computation-context checks are meaningful and paper-faithful.

**Acceptance Criteria:**
- [ ] If an edge has a witness but no trace entry, elaboration fails with an explicit error (not `InstId`).
- [ ] The preferred call path is `phiFromEdgeWitnessWithTrace ... (Just trace) ...`; the trace-less `phiFromEdgeWitness` is either removed or only used in tests with explicit non-strict configuration (documented).
- [ ] Add a regression test where `edgeWitnesses` contains an entry but `edgeTraces` does not, and confirm elaboration errors.

### US-005: Regression tests for “skip” elimination
**Description:** As a maintainer, I want tests that pin the thesis-exact behavior so future refactors can’t reintroduce silent skips.

**Acceptance Criteria:**
- [ ] Add an Hspec regression in `test/ElaborationSpec.hs` (or a new focused spec) that constructs an `EdgeWitness` with an `OpRaise` outside `I(r)` and asserts Φ/elaboration fails.
- [ ] Add/extend a presolution spec (likely `test/Presolution/WitnessSpec.hs` or `test/Presolution/EdgeTraceSpec.hs`) that ensures normalization+validation rejects ops outside the exact rewritten interior.
- [ ] `cabal test` passes.

## Functional Requirements

- FR-1: Presolution must validate normalized witnesses against exact `I(r)` (thesis “translatable presolution” + Fig. 15.3.4 invariants).
- FR-2: Presolution must not treat `copiedNodes(copyMap)` as automatically inside `I(r)`.
- FR-3: Φ translation must throw on any op outside `I(r)` unless it is a rigid-node identity case.
- FR-4: Φ translation must throw when a computation context for an op target cannot be computed.
- FR-5: Φ translation must require `EdgeTrace` for any edge witness it translates.

## Non-Goals

- Not implementing new witness emission logic beyond existing presolution output (e.g. “arbitrary Raise ops not emitted yet”).
- Not changing solver/unification behavior, only validation/translation strictness.
- Not optimizing Φ translation performance.

## Design Considerations

- Errors should be actionable: include `EdgeId`, op constructor, and relevant node ids.
- Favor reusing existing `validateNormalizedWitness` / `OmegaNormalizeError` vocabulary to match the thesis conditions.

## Technical Considerations

- `EdgeTrace.etInterior` is documented as “exact, from the binding tree” (not expanded); keep it exact.
- Fix node-space mismatch by rewriting the interior set through the same copyMap used on steps.
- Prefer defense in depth:
  - Presolution rejects invalid witnesses early.
  - Φ re-validates (cheap) and errors even if presolution missed something.
- Keep builds warning-free (`-Wall`) by ensuring any new error constructors are handled exhaustively.

## Success Metrics

- A witness op outside `I(r)` produces a clear error at presolution time and again at Φ time (if somehow reached), never a silent skip.
- All new/updated tests pass under `cabal test`.

## Open Questions

- Should invalid witnesses be reported as `ElabError.ValidationFailed` vs `ElabError.InstantiationError`?
  - Default for this PRD: `ValidationFailed` for translatability violations; `InstantiationError` reserved for internal inconsistencies like binder-length mismatches.
- Should presolution collect all invalid edges before failing, or fail-fast on the first?
  - Default for this PRD: fail-fast; can be extended later.
