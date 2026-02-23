# Source-ID Split-Domain Contract Design (Thesis-Exact)

Date: 2026-02-15
Status: Validated design (brainstorming)
Scope: Internal elaboration identity contract only (no public API changes)

## 1. Decision Summary

We will not force a pure source-ID model across all structural/typecheck operations.
Instead, we adopt a split-domain contract that preserves thesis-exact binder provenance while maintaining existing solver/typecheck invariants.

Authoritative domains:
1. Source-ID domain is authoritative for Phi binder semantics (membership, binder order, and witness/trace provenance decisions).
2. Canonical-ID domain is authoritative for structural graph/typecheck semantics (bound lookup, reification, parent lookup, and solved-graph traversal).

Rationale:
1. Presolution provenance artifacts (`EdgeTrace.etBinderArgs`, `EdgeTrace.etCopyMap`, `EdgeWitness` op targets) encode the identity needed by thesis-style Phi/Omega reconstruction.
2. The elaboration/typecheck stack is intentionally canonicalized around solved representatives; rewriting those stores to source IDs would be high-risk and out of scope for BUG-2026-02-14-003 closure.
3. Strictness remains unchanged: no permissive fallback for non-binder targets.

## 2. Contract Invariants

The implementation must enforce the following invariants:

1. Membership invariant:
   - “Is this a scheme binder?” is decided in source-ID space first.
   - Canonical aliases may assist lookup only; they are not membership authority.

2. Structural invariant:
   - Structural graph operations remain canonical-ID keyed.
   - This includes binding-parent queries, type reification lookups, and typecheck validation.

3. Bridging invariant:
   - Every Phi operation that transitions from source semantics to structural semantics must pass through one deterministic bridge.
   - Missing bridge provenance is a hard failure (`PhiTranslatabilityError` or invariant error), not a fallback.

4. Determinism invariant:
   - Tie-break order is stable: trace binder order first, then numeric key.
   - Dedupe and rank behavior must be centralized to avoid module drift.

## 3. Module Architecture

Add one internal helper module:
- `src/MLF/Elab/Phi/IdentityBridge.hs`

Responsibilities:
1. Build bridge context from `EdgeTrace`, `SchemeInfo`, and canonicalizer.
2. Expose shared queries for source-key extraction, binder membership, binder index resolution, and source->canonical projection.
3. Centralize ranking/tie-break logic used by both Translate and Omega.

Refactor targets:
1. `src/MLF/Elab/Phi/Translate.hs`
   - Keep source-ID remap and hydration behavior.
   - Move ranking/candidate logic onto bridge helpers.
   - Ensure traced path enters Omega with source-keyed `siSubst`.

2. `src/MLF/Elab/Phi/Omega.hs`
   - Replace local source/canonical reconciliation helpers with bridge calls.
   - Keep strict non-binder rejection unchanged.

3. `src/MLF/Elab/Run/Util.hs`
   - Preserve current boundary behavior:
     - witness op targets and trace provenance fields remain source IDs,
     - structural context fields stay canonicalized.

Non-goals:
1. No public API changes under `src-public/`.
2. No solver/typechecker store rewrite to source IDs.
3. No compatibility/permissive fallback semantics.

## 4. Data Flow

End-to-end identity flow:
1. Presolution emits source provenance (`etBinderArgs`, `etCopyMap`, witness ops).
2. Elaboration canonicalization preserves source provenance fields and canonicalizes structural fields only.
3. IdentityBridge computes deterministic source-key candidates and source<->canonical projections.
4. Translate remaps/hydrates `SchemeInfo.siSubst` into source-key domain.
5. Omega decides binder membership/index in source domain; structural lookups execute in canonical domain.

Failure model:
1. Target has no source-binder identity -> strict `PhiTranslatabilityError`.
2. Ambiguous binder resolution after deterministic ranking -> strict invariant failure.
3. Source binder accepted but no canonical projection for structural operation -> strict invariant failure with diagnostics.

## 5. Testing and Validation Plan

Targeted tests:
1. `test/CanonicalizerSpec.hs`
   - Assert source-ID preservation for witness ops, `etBinderArgs`, and `etCopyMap`.
   - Assert structural canonicalization (`ewLeft/right/root`, `etRoot`, `etInterior`) still occurs.

2. New bridge-focused tests (proposed):
   - `test/Phi/IdentityBridgeSpec.hs`
   - Deterministic ranking and dedupe.
   - Exact source match preference over alias match.
   - Failure on missing source->canonical projection.

3. `test/ElaborationSpec.hs`
   - Positive regression: under-populated `siSubst` hydrated from trace, translation succeeds.
   - Negative regression: true out-of-scheme non-binder still fails strictly.

Guardrails and gates:
1. `BUG-2026-02-06-002 strict target matrix`
2. `BUG-002-V4`
3. `BUG-004`
4. `tracks instantiation copy maps for named binders`
5. `witness/trace/expansion canonicalization`
6. Full gate: `cabal build all && cabal test` (sequential execution)

## 6. Rollout Sequence

1. Introduce IdentityBridge with behavior-preserving wiring.
2. Switch Translate remap/hydration to bridge-backed ranking.
3. Switch Omega binder membership/index to bridge APIs.
4. Remove duplicated reconciliation helpers.
5. Run targeted anchors, then full gate.
6. Only after full green: update `Bugs.md`, `CHANGELOG.md`, `implementation_notes.md`, `TODO.md` for closure.
