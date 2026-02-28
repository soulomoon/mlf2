# Findings: WitnessNorm C2 Analysis

## Context
- Target module: `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Constraint/Presolution/WitnessNorm.hs`.
- Focus region: `replayBinders` / `targetKeysUsed` / `activeSourceEntries` around lines 352-489.

## Discoveries
- Reproduced C2 failure with:
  - `cabal test mlf2-test --test-options='--match "C2: edge traces have non-empty binder args for polymorphic edges"'`
  - Failure is currently an early presolution `Driver` internal error:
    - `edge replay-map codomain target outside replay binder domain`
    - edge `0`, source key `0`, replay target `NodeId 0`.
- Root-cause in current witness normalization flow:
  - For non-polymorphic edges (`null replayBinders`), `activeSourceEntries` currently keeps `activeSourceEntriesBase`.
  - This allows non-empty `etBinderArgs`/`etBinderReplayMap` to survive on edges with empty replay binder domain.
  - `Driver` codomain validation then rejects those replay targets against an empty domain.
- Secondary root-cause for C2 behavior after codomain hard-fail is addressed:
  - On polymorphic edges, `activeSourceEntries` is filtered by `targetKeysUsed` using source-key membership only.
  - `targetKeysUsed` is built from normalized op targets (often replay-domain keys), so source keys can be dropped even when corresponding replay binders are active.
  - This can collapse `activeSourceEntries` to `[]`, yielding empty `etBinderArgs` and causing C2 to fail (even when BUG-002/BUG-003 remain green).
- Why BUG-002/BUG-003 can still pass:
  - BUG tests mostly assert pipeline success/type shape, not non-empty binder args.
  - `BUG-003-PRES` computes `selfBoundMetas` by iterating `etBinderArgs`; empty args vacuously pass that specific check.

## Patch Strategy
- Non-polymorphic edges: clear active source entries (and therefore replay map) so no codomain contract is emitted where replay binder domain is empty.
- Polymorphic edges: key filtering should recognize both source keys and replay-target keys; if filtering over-constrains to empty, fall back to base entries (bounded by replay binder count) to preserve non-empty binder args where appropriate.
