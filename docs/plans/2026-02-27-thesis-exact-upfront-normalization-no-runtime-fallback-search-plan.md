# Thesis-Exact Phi Plan: Upfront Normalization, No Runtime Fallback Search

## Summary
This plan makes Phi replay thesis-exact by moving binder replay resolution to presolution artifacts and removing runtime fallback search in Phi. Runtime elaboration will consume a deterministic replay map and fail fast on contract violations instead of searching class members/hints/names at translation time.

Locked choices:
1. Scope: Phi-only.
2. Failure policy: fail-fast.
3. Delivery: phased gates.
4. No-trace helper policy: strict everywhere (no fallback behavior retained in `phiFromEdgeWitnessNoTrace`).

## Scope and Non-Goals
1. In scope:
- Phi replay bridge and Omega binder resolution.
- Presolution trace artifact contract needed by Phi.
- Phi-focused tests/docs.
2. Out of scope:
- Non-Phi fallback logic in generalization/result-type/elaboration modules.
- Solver algorithm changes beyond presolution artifact validation.
- Frontend normalization changes.

## API and Type Changes
1. Change `EdgeTrace` contract in `src/MLF/Constraint/Presolution/Base.hs`:
- Replace advisory `etBinderReplayHints` with required `etBinderReplayMap :: IntMap NodeId`.
- Semantics: source binder key -> replay binder node chosen during presolution normalization.
2. Update trace canonicalization passthrough in:
- `src/MLF/Constraint/Presolution/Rewrite.hs`
- `src/MLF/Elab/Run/Util.hs`
3. Tighten Phi helper surface in `src/MLF/Elab/Phi/IdentityBridge.hs`:
- Remove class-fallback API exports (`sourceKeysForNodeWithClassFallback`, `sourceKeysForNodeNoClassFallback`).
- Keep witness-domain key derivation only.
4. Strict no-trace behavior in `src/MLF/Elab/Phi/Translate.hs`:
- `phiFromEdgeWitnessNoTrace` fails fast (`MissingEdgeTrace`) under strict mode.
5. Add trace-requiring test helper in `src/MLF/Elab/Phi/TestOnly.hs`:
- Expose a trace-backed helper so positive tests stop depending on no-trace path.

## Phased Implementation

### Gate 1: Artifact Schema Cutover
1. Update `EdgeTrace` field name/semantics in `src/MLF/Constraint/Presolution/Base.hs`.
2. Update all constructors/copying callsites:
- `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`
- `src/MLF/Constraint/Presolution/Rewrite.hs`
- `src/MLF/Elab/Run/Util.hs`
- tests using `EdgeTrace { ... }` literals.
3. Exit criteria:
- Build compiles with no `etBinderReplayHints` references.

### Gate 2: Producer-Side Deterministic Replay Mapping
1. Build deterministic replay map in `src/MLF/Constraint/Presolution/WitnessNorm.hs` before witness finalization.
2. Deterministic mapping algorithm (no runtime fallback dependency):
- Source domain: deduplicated binder keys from `etBinderArgs` in trace order.
- Candidate replay pool: live TyVar binder keys derived from normalized binder domain in the same rewritten space.
- Assignment rule: one pass in trace order, pick first unused candidate from deterministic candidate list; no class-member expansion.
- If any source binder cannot be assigned, throw `WitnessNormalizationError`.
- Enforce injective replay codomain for assigned binders.
3. Add explicit normalization errors in `src/MLF/Constraint/Presolution/WitnessValidation.hs`:
- `ReplayMapIncomplete`
- `ReplayMapNonTyVarTarget`
- `ReplayMapNonInjective`
4. Exit criteria:
- Every normalized edge trace has total, deterministic replay map for its source binder domain.

### Gate 3: Remove Runtime Replay-Map Synthesis in Phi.Translate
1. Replace `computeTraceBinderReplayBridge` in `src/MLF/Elab/Phi/Translate.hs` with strict loading/validation of `etBinderReplayMap`.
2. Delete runtime candidate search paths:
- `replayFromAlias`
- class-fallback source key expansion
- `fallbackHint` and `fallbackRaw` picks.
3. Keep only:
- source-domain set extraction,
- replay-map contract validation against current replay binder domain (`siSubst` keys),
- deterministic pass-through to Omega.
4. Exit criteria:
- No runtime replay-map synthesis remains in Translate.
- Missing or invalid map entries produce fail-fast invariant errors.

### Gate 4: Remove Runtime Fallback Search in Omega and IdentityBridge
1. In `src/MLF/Elab/Phi/Omega.hs`:
- Remove `normalizedReplayCandidates`, `fallbackBinderCandidates`, class-member recovery branch, and name/hint fallback search.
- `resolveTraceBinderTarget` uses `traceBinderReplayMap` directly for trace-source keys.
- Non-root `OpWeaken` either resolves to a binder in current spine and emits `InstElim`, or fails fast.
2. In `src/MLF/Elab/Phi/IdentityBridge.hs`:
- Remove class-member fallback logic from key derivation and binder index ranking.
- `lookupBinderIndex` uses exact/canonical witness-domain matching only.
3. Exit criteria:
- Phi runtime no longer searches equivalence classes to recover replay binders.

### Gate 5: Strict Everywhere for No-Trace Path
1. Make `phiFromEdgeWitnessNoTrace` in `src/MLF/Elab/Phi/Translate.hs` fail fast (`MissingEdgeTrace`).
2. Add a trace-backed test entrypoint in `src/MLF/Elab/Phi/TestOnly.hs` and migrate positive tests off no-trace usage.
3. Migrate no-trace success tests (primarily in `test/ElaborationSpec.hs`) to trace-backed fixtures.
4. Keep explicit no-trace negative tests that assert fail-fast.
5. Exit criteria:
- No positive-path tests depend on no-trace fallback behavior.

### Gate 6: Contract Enforcement + Documentation
1. Add producer-boundary checks in `src/MLF/Constraint/Presolution/Driver.hs`:
- replay-map domain equals trace binder-source domain for non-trivial edges.
- replay-map codomain nodes are live TyVar binders.
2. Update docs:
- `implementation_notes.md`
- `docs/thesis-deviations.yaml`
- `CHANGELOG.md`
- `TODO.md`
3. Exit criteria:
- Docs describe strict upfront replay normalization and removed runtime fallback search.

## Test Plan
1. Unit tests:
- `test/Phi/IdentityBridgeSpec.hs`
- update/remove class-fallback expectations.
2. Presolution normalization tests:
- `test/Presolution/WitnessSpec.hs`
- add replay-map totality/injectivity/fail-fast cases.
3. Phi alignment/integration tests:
- `test/Phi/AlignmentSpec.hs`
- assert production success without class fallback search.
4. Elaboration regressions:
- `test/ElaborationSpec.hs`
- migrate no-trace success fixtures to trace-backed path.
- add explicit missing-trace/missing-map fail-fast assertions.
5. Translatability guard:
- `test/TranslatablePresolutionSpec.hs`
- ensure translatable-presolution validation still passes for baseline corpus.

## Verification Commands
1. Search-based invariants:
- `rg -n "sourceKeysForNodeWithClassFallback|fallbackBinderCandidates|replayFromAlias|fallbackHint|fallbackRaw|etBinderReplayHints" /Volumes/src/mlf4/src /Volumes/src/mlf4/test`
2. Targeted suite:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi|IdentityBridge|Witness normalization|Translatable presolution|OpWeaken|MissingEdgeTrace"'`
3. Full gate:
- `cabal build all && cabal test`

## Acceptance Criteria
1. Production Phi path (`withTrace`) does not perform runtime fallback search for replay binder recovery.
2. Replay mapping required by Phi is produced and validated upfront in presolution artifacts.
3. Missing/invalid replay mapping fails fast with deterministic diagnostics.
4. No-trace entrypoint is strict (no success path that relies on fallback behavior).
5. Full build and test suite pass.

## Assumptions and Defaults
1. "Thesis-exact" here means Def. 15.3.x witness-domain replay semantics with explicit producer contracts and no runtime recovery search in Phi.
2. Non-Phi fallback logic remains unchanged by this plan.
3. Internal API breaks in `EdgeTrace` and Phi test helpers are acceptable.
4. Trace-backed production path is authoritative; no-trace path is strict failure by design.
