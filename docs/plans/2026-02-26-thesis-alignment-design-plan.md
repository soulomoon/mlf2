# Thesis Alignment Design Plan

## Goal

Align the implementation pipeline with the thesis model so that:

1. Graph transformations are performed by presolution/solve operations.
2. Elaboration (especially Phi) consumes translatable-presolution artifacts.
3. Canonicalization is a projection/query concern over solved equivalence classes, not a required graph-rewrite dependency for Phi.

Primary thesis anchors:

- `papers/these-finale-english.txt` §10.3–10.4 (propagation, solved edges, presolutions)
- `papers/these-finale-english.txt` §12.1.3 (SolveConstraint ordering)
- `papers/these-finale-english.txt` §15.3.5–15.3.6 (translation from propagation witness to computation; elaboration of `χp`)

## Context

Current code already executes GraphOps and witness-producing transformations in presolution, but Phi still depends heavily on a post-presolution solved canonical view (`Solved.canonical`, canonical maps, identity bridge fallback paths). This is the main remaining divergence from thesis presentation.

## Design Principles

1. Preserve thesis semantics before refactoring representation details.
2. Keep graph mutation ownership in presolution/solve; elaboration remains read-only.
3. Keep `Solved` opaque, but make it projection-first:
   - Original/presolution graph facts are authoritative for lookup.
   - Canonical ids are derived for identity reconciliation only.
4. Migrate with dual-path verification and no behavior regressions.

## Target Architecture

### 1) Presolution as transformation owner

- Presolution remains responsible for:
  - propagation-like expansion materialization,
  - unification closure interleaving,
  - Omega/edge witness and trace construction,
  - rigidification/translatability checks.
- Output artifacts are sufficient for elaboration:
  - solved/translatable constraint snapshot,
  - per-edge witness + trace + expansion metadata,
  - stable node-identity mapping info.

### 2) Solved as identity projection layer

- `Solved` provides:
  - equivalence-class projection (`canonical`, `classMembers`),
  - read-only node/binding queries,
  - compatibility accessors during migration.
- `Solved` should not force Phi to depend on rewritten canonical graph structure as primary source.

### 3) Phi translation driven by witness domain first

- Phi/Omega translation resolves binder/source identity from `EdgeTrace`/witness domain first.
- Canonical projection is used only to reconcile aliases, not as first-choice source of semantic inputs.
- IdentityBridge keeps deterministic ranking but reduces fallback dependence on canonicalized rewritten artifacts.

## Work Plan

### Phase A: Invariant Harness

Add/extend tests to pin alignment invariants:

- GraphOps and witness ops are executed before elaboration.
- No residual `TyExp`/instantiation work enters Phi path.
- Phi derivation remains reconstructible from edge witness/trace data.
- Existing output term/type behavior remains unchanged.

Exit gate:

- New alignment tests pass.
- Existing test suite remains green.

### Phase B: Solved Projection-First Cut

Refine `Solved` query internals and call sites:

- Make original/presolution-side queries primary for read paths.
- Constrain direct reliance on canonical rewritten graph fields.
- Keep compatibility accessors where needed for transition.

Exit gate:

- No consumer requires canonical rewritten graph as unique source of truth.
- No behavior change in elaboration tests.

### Phase C: Phi/Omega/IdentityBridge Reconciliation

Refactor Phi stack (`Translate`, `Omega`, `IdentityBridge`):

- Prefer witness/trace source-domain resolution.
- Use equivalence-class projection as reconciliation fallback.
- Minimize direct canonical-map-driven lookup flows.

Exit gate:

- Phi-focused tests pass (including tricky weaken/raise/merge scenarios).
- Term/type snapshots unchanged for baseline corpus.

### Phase D: Pipeline Boundary Enforcement

Ensure orchestration remains explicit:

- Presolution performs transformations.
- Pipeline/elaboration performs projection + translation only.
- Remove any residual graph-mutation helper calls from elaboration path.

Exit gate:

- Static call-site audit confirms no elab-path graph mutation.
- Regression tests remain green.

### Phase E: Dual-Path Verification and Cleanup

Temporarily run both paths in checks:

- Current canonical-heavy path vs projection-first path.
- Assert semantic equivalence; investigate any mismatch.

Then:

- Remove legacy fallback branches no longer needed.
- Update docs/deviation trackers.

Exit gate:

- Dual-path mismatch set is empty.
- Legacy path removal does not change outputs.

## Risks and Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Identity mismatches in Phi binder replay | High | Add narrow replay diagnostics and targeted fixture coverage before removing fallback paths |
| Hidden dependency on canonical rewritten bind tree | High | Stage migration with query wrappers and explicit call-site audit |
| Behavior drift in elaborated terms | High | Dual-path compare + golden tests across representative corpus |
| Migration complexity across many modules | Medium | Phase gates with bounded changesets and strict verification per phase |

## Verification Strategy

Primary validation command:

`cabal build all && cabal test`

Additional checks:

1. Phi-focused regression suite for witness translation edge cases.
2. A/B comparison harness for legacy vs projection-first elaboration outcomes.
3. Optional instrumentation logs for canonical/source-id reconciliation in failing cases only.

## Deliverables

1. Updated `Solved` semantics and consumer usage pattern.
2. Refactored Phi/Omega/IdentityBridge identity resolution path.
3. Alignment tests and comparison harness.
4. Updated thesis alignment docs:
   - `docs/thesis-deviations.yaml`
   - `implementation_notes.md`
   - relevant `docs/plans/*` cross-links.


