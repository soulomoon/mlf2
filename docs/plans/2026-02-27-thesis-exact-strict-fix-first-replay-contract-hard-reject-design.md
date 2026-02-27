# Thesis-Exact Strict Fix First: Replay Contract Hard-Reject + Atomic Cutover

## Summary
Implement an **atomic strict cutover** that removes runtime replay-target repair and enforces thesis-exact replay contracts at the producer boundary first.
The fix makes `EdgeTrace.etBinderReplayMap` authoritative and runtime-validated without name/position/default runtime projection, while keeping the full suite green.

## Locked Decisions
1. Source-space replay targets are **hard rejected** at runtime.
2. Delivery is an **atomic strict cutover** (producer + runtime + tests in one change, full green required).

## Public/Internal Interface and Contract Changes
1. `EdgeTrace` type shape stays the same, but contract tightens:
   - `etBinderArgs` becomes the normalized/active source-binder domain (no stale binder sources).
   - `etBinderReplayMap` remains `sourceKey -> replayBinderNode`, but codomain must be replay-binder key-space (not source-space aliases).
2. Runtime contract in Phi:
   - `computeTraceBinderReplayBridge` in `Translate.hs` performs validation and pass-through only.
   - No runtime source->replay projection.
3. OpRaise strictness:
   - In `Omega.hs`, unresolved trace-source raise targets fail fast again.

## Design Plan

### 1) Producer: Normalize source domain and replay codomain in presolution
1. Update replay-map construction in `WitnessNorm.hs` to compute replay map from normalized artifacts, not mixed source/interior candidates.
2. Build `sourceEntriesInOrder` deterministically from `etBinderArgs`:
   - Deduplicate by source key preserving first occurrence order.
   - Retain source key and its paired arg.
3. Build `targetKeysUsed` from normalized ops (`opsNorm`) by collecting all op target nodes (`OpGraft`, `OpWeaken`, `OpRaise`, `OpMerge`, `OpRaiseMerge`), canonicalized.
4. Define active source domain:
   - If `targetKeysUsed` is non-empty, keep only source entries whose rewritten/canonical source key appears in `targetKeysUsed`.
   - If `targetKeysUsed` is empty, keep all deduped source entries.
5. Build replay binder sequence from final rewritten constraint:
   - Use `Binding.orderedBinders canonical c0 (typeRef (canonical edgeRoot))`.
   - Filter to live `TyVar` binders only.
6. Deterministic assignment:
   - Let `N = length activeSources`.
   - Require `length replayBinders >= N`; else throw `ReplayMapIncomplete` with unmapped source keys.
   - Map active sources to first `N` replay binders in deterministic order.
7. Update trace output:
   - Set `etBinderArgs` to active source entries (preserving source-domain provenance).
   - Set `etBinderReplayMap` to the new map keyed by active source keys with replay-domain targets.
8. Keep injectivity and TyVar-target constraints as hard requirements.

### 2) Producer validation tightening
1. In `WitnessValidation.hs`:
   - Keep existing checks (`ReplayMapIncomplete`, `ReplayMapNonTyVarTarget`, `ReplayMapNonInjective`).
   - Add replay-domain membership check against replay binders derived from edge root ordered binders (same derivation as normalization).
2. In `Driver.hs`:
   - Keep source-domain equals replay-map-domain check.
   - Keep codomain TyVar check.
   - Add explicit codomain replay-domain check and fail with `InternalError` if violated.

### 3) Runtime bridge: remove target repair, keep strict validation
1. In `Translate.hs`, `computeTraceBinderReplayBridge`:
   - Remove `projectReplayTarget` behavior that rewrites invalid targets via source-name, positional mapping, or default target.
   - Validate each replay target is in replay binder domain (raw or canonical alias only).
   - Return map unchanged after validation.
2. Replay binder domain derivation:
   - Primary: scheme quantifier names parsed to `NodeId` from `siScheme`.
   - Fallback: `siSubst` keys only if quantifier parsing unavailable.
   - This avoids mismatches between `siSubst` key-space and quantifier identity list used later in Omega.

### 4) Omega strictness restoration for unresolved raise
1. In `Omega.hs`:
   - Restore fail-fast branch for `OpRaise` when `nNonBottom <|> nExisting` is empty and the target is a trace-binder source.
   - Keep non-trace-source no-op behavior unchanged.

### 5) Test strictness tightening (remove broadened predicates)
1. Re-tighten tests in:
   - `ReduceSpec.hs`
   - `TypeCheckSpec.hs`
   - `ThesisFixDirectionSpec.hs`
2. Assert only the intended strict unresolved-`OpWeaken` failure class for those scenarios.

### 6) New regression tests for producer contract
1. Add presolution tests in `WitnessSpec.hs`:
   - `etBinderReplayMap` codomain keys are replay binders for edge root.
   - `etBinderArgs` domain equals `etBinderReplayMap` domain after normalization.
   - stale/unreferenced source binders are pruned from normalized `etBinderArgs`.
2. Add Phi translation tests in `ElaborationSpec.hs`:
   - malformed source-space replay target now hard-fails (no runtime repair).

### 7) Documentation updates (same change)
1. Update `implementation_notes.md`:
   - state runtime no-repair policy and producer hard contract.
2. Update `CHANGELOG.md`:
   - record strict cutover and replay-domain enforcement.
3. Update `TODO.md`:
   - remove/close related replay fallback TODOs.
4. Update thesis deviation docs if needed:
   - `docs/thesis-deviations.yaml`.

## Test Cases and Scenarios
1. Existing full suite must pass unchanged except where strictness expectation is intentionally tightened.
2. A6 parity/bounded-alias/coercion-heavy pipelines must remain green under strict producer artifacts.
3. BUG-002/BUG-003 matrix cases must not regress.
4. No-trace strict behavior remains (`MissingEdgeTrace`) and stays covered.
5. Search invariants must hold:
   - no `fallbackHint`, `fallbackRaw`, `replayFromAlias`, `sourceKeysForNodeWithClassFallback`, `etBinderReplayHints`.

## Verification Commands
1. `rg -n "sourceKeysForNodeWithClassFallback|fallbackBinderCandidates|replayFromAlias|fallbackHint|fallbackRaw|etBinderReplayHints" /Volumes/src/mlf4/src /Volumes/src/mlf4/test`
2. `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi|IdentityBridge|Witness|OpWeaken|OpRaise|MissingEdgeTrace|A6|BUG-002|BUG-003"'`
3. `cabal build all && cabal test`

## Assumptions and Defaults
1. Thesis-exact strictness has priority over runtime compatibility behavior.
2. Runtime Phi is validation-only for replay-map targets; producer owns key-space normalization.
3. Atomic change is required: no intermediate red branch accepted for this fix.
4. Internal contract/API behavior changes are acceptable as long as full gate is green.
