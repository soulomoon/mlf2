# PRD: Thesis‑Exact Quantifier Reordering (ϕR / Σ(g))

## Introduction/Overview
The thesis defines a quantifier‑reordering computation ϕR (Def. 15.3.4) used to reconcile binder orders between `Typ(a′)` and `Typexp(a′)` when they differ only by quantifier order. In the repo, this shows up as the Σ(g) prefix in the per‑instantiation‑edge translation:

> Φ(e) = Σ(g); Φχe(Ω)  (see `papers/xmlf.txt` §3.4 and thesis §15.3.4–§15.3.6)

Current behavior is incomplete: Φ translation only triggers binder reordering when the witness contains `OpRaise` (via `needsPrec` in `src/MLF/Elab/Phi/Omega.hs`). This misses the thesis case where `Typ(a′)` and `Typexp(a′)` differ but Ω has no Raise steps, yielding a non‑paper‑faithful instantiation.

This PRD refactors Φ translation to make ϕR/Σ(g) application thesis‑exact: compute and apply reordering when `Typ` vs `Typexp` differ by binder order, independent of Ω contents, and **fail fast** when reordering cannot be computed deterministically (non‑translatable presolution).

## Goals
- Apply thesis ϕR/Σ(g) reordering independent of Ω ops (no Raise‑only gate).
- Make the reordering computation deterministic and paper‑aligned (using <P / ≺ ordering and bound dependencies).
- Fail fast with a clear elaboration error when the reordering prerequisites are not met (missing <P order keys, dependency cycle, missing binder identities).
- Add regression tests that cover “reorder needed without Raise” and the new fail‑fast behavior.
- Update documentation to reflect that ϕR integration is no longer a known gap.

## User Stories

### US-001: Apply Σ/ϕR reordering in Φ translation independent of Raise
**Description:** As a maintainer, I want Φ translation to compute and apply Σ(g) even when Ω has no Raise operations, so the elaboration matches thesis Def. 15.3.4 and Fig. 15.3.5.

**Acceptance Criteria:**
- [ ] Remove the `needsPrec` gate in `src/MLF/Elab/Phi/Omega.hs` so reordering is not conditional on `OpRaise`.
- [ ] Φ translation always attempts binder reordering at the start of `phiWithSchemeOmega`, producing `InstId` when no reorder is needed.
- [ ] The emitted instantiation preserves existing behavior: `Φ(e)` remains “Σ prefix; then Φχe(Ω)”.
- [ ] `cabal build all` passes.

### US-002: Deterministic, fail-fast Σ/ϕR computation
**Description:** As a maintainer, I want the Σ/ϕR computation to be deterministic and to fail fast when it cannot be computed, so non‑translatable presolutions do not silently produce paper‑incorrect instantiations.

**Acceptance Criteria:**
- [ ] Σ computation validates binder spine bookkeeping (binder list length matches ids list).
- [ ] Σ computation requires concrete binder identities (no `Nothing` binder ids) when a reorder is attempted.
- [ ] Σ computation requires <P order keys for every binder identity under the chosen root; missing keys produce a clear `ElabError`.
- [ ] Bound dependencies are respected: if a binder appears free in another binder’s bound, it must appear earlier (topo‑sorted with cycle detection).
- [ ] Any missing order key / dependency cycle returns `Left` (fail fast), rather than silently returning `InstId`.
- [ ] Errors are surfaced as `ElabError` (likely `InstantiationError` with a stable prefix like `PhiReorder:`), so callers see the failure in Phase 6.
- [ ] `cabal build all` passes warning-free (`-Wall`).

### US-003: Regression tests for reorder-without-Raise + fail-fast cases
**Description:** As a contributor, I want tests that lock in thesis‑exact ϕR integration so future refactors do not reintroduce the Raise‑only behavior.

**Acceptance Criteria:**
- [ ] Un‑pend and implement `test/ElaborationSpec.hs` “applies Σ reordering even without Raise when Typ/Typexp differ (gap)”.
- [ ] Add a test showing: empty Ω (no ops) can still produce a non‑identity instantiation because binder order differs.
- [ ] Add a test showing: missing <P order key for a binder causes Φ translation to fail fast with a clear error.
- [ ] (Optional but recommended) Add a test showing: when binder order already matches, Σ is `InstId`.
- [ ] `cabal test --test-show-details=direct` passes.

### US-004: Documentation updates for paper-faithfulness
**Description:** As a maintainer, I want docs to reflect that ϕR/Σ(g) is now integrated so the repo’s paper-alignment status remains accurate.

**Acceptance Criteria:**
- [ ] Update `implementation_notes.md` to remove “ϕR integration beyond Raise-triggered reordering” from the known gaps section and to describe the new behavior with thesis references.
- [ ] Update `docs/paper-map.md` to mention Σ(g) is applied during Φ translation (while `MLF.Elab.Sigma` provides the commutation building blocks).
- [ ] Update `.kiro/specs/paper-faithfulness-remaining-deltas/requirements.md` Requirement 2 evidence/status to reflect the new integration (no longer “partial”).

## Functional Requirements
- FR-1: Φ translation must compute a Σ/ϕR prefix instantiation even if Ω is empty or contains no Raise operations.
- FR-2: Σ/ϕR computation must be deterministic and consistent with the thesis’ <P ordering and bound-dependency constraints.
- FR-3: When Σ/ϕR cannot be computed deterministically (missing order keys, missing binder identities, dependency cycle), elaboration must fail fast with a clear `ElabError`.
- FR-4: The reordering must be composed as a prefix instantiation before interpreting Ω steps.
- FR-5: Add tests that cover reorder-without-Raise and fail-fast cases.

## Non‑Goals (Out of Scope)
- Changing presolution’s expansion materialization pipeline or witness normalization strategy beyond what is needed for Σ/ϕR integration.
- Reworking Φ translatability validation globally (rigid/non‑interior op handling) except insofar as Σ/ϕR itself must be fail-fast/deterministic.
- Adding new surface syntax or changing the frontend typing rules.

## Design Considerations
- Keep the Σ/ϕR computation local to Φ translation (`src/MLF/Elab/Phi/Omega.hs`) to match the paper structure `Σ(g); Φχe(Ω)`.
- Prefer reusing the existing adjacent-swap machinery (`bubbleReorderTo`) for reordering, but drive it from paper-relevant order keys (<P) and explicit dependency constraints.
- Keep errors explicit and stable (prefix-tagged `InstantiationError` messages) unless a dedicated `ElabError` constructor is clearly warranted.

## Technical Considerations
- Root for <P order keys:
  - Prefer the expansion-root node from traces when available (`EdgeTrace.etRoot`), otherwise fall back to the witness root (`EdgeWitness.ewRoot`).
- Order keys:
  - Use `MLF.Util.OrderKey.orderKeysFromRootWithExtra` as in current Φ translation to include structural children plus binding-edge children and bounds when computing reachability and <P ordering.
- Dependency constraints:
  - Bound dependencies should be computed from `freeTypeVarsList` of each binder’s bound to ensure the reordered quantifier spine remains well-scoped.
- Keep builds warning-free (`-Wall`): avoid partial patterns and ensure error paths are explicit.

## Success Metrics
- The “reorder needed without Raise” test is no longer pending and passes.
- Φ translation produces the same results as before on existing suites (no regressions), while newly covering the missing thesis case.
- Clear failure messages appear when Σ/ϕR prerequisites are violated (no silent skipping).
- `cabal test --test-show-details=direct` passes.

## Open Questions
- None (fail-fast policy chosen; error form is specified as `ElabError` via `InstantiationError` unless a dedicated constructor becomes necessary).

