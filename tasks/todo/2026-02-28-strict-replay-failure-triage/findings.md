# Findings: Strict Replay Failure Triage

## Root-Cause Interaction
1. `WitnessNorm` no-replay flow (empty replay-binder domain at edge root) can produce witnesses that no longer line up with replay-scheme binder keys.
2. `Translate` previously kept Omega on `siReplay` in some no-replay duplicate-graft cases where op targets were clearly in source key-space.
3. `Omega` had no-replay weaken rescue for one shape, but needed producer-trace-sensitive handling for singleton `OpWeaken` as well.

Combined effect in failing state:
- Under-instantiation on polymorphic edges (`forall` left unconsumed, `Int -> Int` mismatches).
- Wrong phase-3 gate output (`forall a. a -> Bottom` shape).
- Strict fail-fast mismatch for BUG-002 variants.

## Minimal Effective Change Set
- `Translate`:
  - Widen `useSourceSchemeForOmega` for no-replay duplicate-graft cases by removing annotation-only gating.
- `Omega`:
  - Import `getCopyMapping` (compile fix).
  - Add `isProducerTrace` and extend `simpleNoReplayWeakenPattern` to include singleton `OpWeaken` only on producer traces.
  - Keep no-replay target remap and exact binder index elimination under that narrow pattern.
- `WitnessNorm`:
  - Keep current strict replay-map normalization and no-replay pruning/projection logic unchanged in this fix set.

## Line-Level Targets
- `src/MLF/Elab/Phi/Translate.hs` around lines 420-428 (`useSourceSchemeForOmega`, `siOmega`).
- `src/MLF/Elab/Phi/Omega.hs` around:
  - line 35 import list (`getCopyMapping`)
  - lines 113-133 (`isProducerTrace`, `simpleNoReplayWeakenPattern`)
  - lines 258-268 (`nearestSourceSchemeBinderKey`)
  - lines 693-705 (`resolveTraceBinderTarget` no-replay remap)
  - lines 803-823 (`exactRawIx` weaken elimination path)

## Verification Matrix (all passing)
Target failures:
- `elaborates polymorphic instantiation`
- `O15-EDGE-TRANSLATION ... (id @ Int)`
- `witness instantiation ... (two instantiations)`
- `BUG-002-V1`
- `BUG-002-V3`
- `Phase 3 atomic wrapping equivalence gates`

Protected tests:
- `Driver replay-map boundary validation`
- `A6 parity: bounded alias + coercion-heavy path agrees across unchecked, checked, and typeCheck`
- `BUG-2026-02-17-002`
- `redirected let-use sites keep polymorphic schemes`
- `OpWeaken on an alias target fails fast under strict replay-map resolution`
