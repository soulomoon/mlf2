# Phi Replay Bridge Strict Pass-Through Design

## Summary
Make replay-map handling in Φ thesis-exact by treating `EdgeTrace.etBinderReplayMap` as producer-authoritative data and removing runtime replay-target projection in `computeTraceBinderReplayBridge`.

Thesis anchor: `papers/these-finale-english.txt` §15.3.5 (translation from normalized witness/derivation to computation).

## Problem Statement
Current bridge behavior validates replay-map shape, but still includes runtime projection policy (source-name / positional / default target) when consuming replay targets. This keeps a non-thesis runtime repair layer in Φ.

## Goals
1. Runtime bridge is load + validate + pass-through only.
2. Missing/invalid replay targets fail fast with deterministic diagnostics.
3. Preserve existing strict Ω behavior (`OpWeaken` unresolved target is hard failure).
4. Keep full suite green.

## Non-Goals
1. Changing presolution replay-map producer algorithm (already strict and deterministic).
2. Reworking Ω semantics or non-replay Φ behavior.
3. Broader elaboration/refinement outside replay-bridge contract.

## Locked Decisions
1. `etBinderReplayMap` remains the single source of replay-target truth at runtime.
2. Runtime does not synthesize/repair replay targets.
3. Failure policy is fail-fast (`PhiInvariantError`).

## Interface/Contract Impact
### `MLF.Elab.Phi.Translate.computeTraceBinderReplayBridge`
1. Keep source-domain parity checks (`etBinderArgs` domain vs replay-map domain).
2. Keep replay-domain membership checks.
3. Remove runtime fallback projection paths:
- source-name remapping
- positional replay map remapping
- default replay-target fallback
4. Return replay map unchanged after validation.

### Tests
1. Strengthen bridge-level regressions in `test/ElaborationSpec.hs` for malformed replay codomain.
2. Keep strict fail-fast matcher coverage in `TypeCheckSpec`/`ReduceSpec`/`ThesisFixDirectionSpec`.

## High-Level Flow (Target State)
1. Read `EdgeTrace`.
2. Compute trace binder source set from `etBinderArgs`.
3. Validate replay-map domain equality with source set.
4. Validate each replay target is in replay binder domain (raw or canonical alias).
5. Pass through map to Ω.
6. Ω executes strict binder resolution without runtime target repair.

## Risks and Mitigations
1. Risk: false negatives in legacy fixtures that relied on implicit projection.
- Mitigation: explicit regression updates to assert fail-fast class.
2. Risk: reduced diagnostic quality after fallback removal.
- Mitigation: keep edge/source/target/domain fields in invariant error messages.

## Verification Strategy
1. Search invariants:
- no runtime projection helpers in `Translate` replay bridge.
2. Targeted tests:
- replay-map domain/codomain bridge failures.
- strict `OpWeaken` fail-fast slices.
3. Full gate:
- `cabal build all && cabal test`.

## Rollout
1. Implement bridge strict-pass-through.
2. Update focused tests.
3. Run targeted and full verification.
4. Sync docs (`implementation_notes.md`, `CHANGELOG.md`, TODO entry if needed).
