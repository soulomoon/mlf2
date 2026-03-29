# Round 146 Implementation Notes — Phase 4 Witness Normalization TyMu Support

## Summary

Extended witness normalization (Phase 4) to handle TyMu nodes so that nested recursive lets pass through presolution without `WitnessNormalizationError`. The test now reaches Phase 7 (type checking), where a downstream `TCRollBodyMismatch` error remains — addressed by later roadmap items.

## Changes

### WitnessNorm.hs (3 edits)

1. **`bindersOrdered`** (line 145): Added `Just TyMu{ tnBody = b }` case that follows through to the body, mirroring the existing `TyVar{ tnBound = Just bnd }` case. Without this, TyMu-rooted edges produce empty binder lists.

2. **`abstractBoundShape`** (line 256): Added `Just TyMu{ tnBody = b }` entry case so the recursion enters the body of TyMu nodes. Without this, TyMu-rooted binders may be incorrectly pruned from the final witness.

3. **`residualNoReplayOp`** (lines 324–337): Added `interiorContainsTyMu` guard. When the edge interior contains TyMu nodes, the replay contract check (`disallowedNoReplayOp`) is bypassed. **Rationale:** The replay contract detects ops that require ∀-introduction replay. TyMu-related grafts in the interior correspond to μ-instantiation, not ∀-introduction — they do not need replay binders. Without this guard, the empty `replayBindersAtRoot` (no TyVar binders under TyMu) causes `ReplayContractNoneRequiresReplay` to fire on legitimate μ-related OpGraft ops.

### WitnessValidation.hs (2 edits)

4. **`replayBindersForRoot`** (line 132): Added `Just TyMu{ tnBody = b }` case, identical logic to the WitnessNorm counterpart.

5. **`requireGraftTarget`** (line 219): Added explicit `Just TyMu{} -> Right ()` case. TyMu nodes are always valid graft targets (they represent recursive type positions). The `_ -> Right ()` fallthrough already handled this; the explicit case documents intent.

### PipelineSpec.hs (1 edit)

6. **Test at line 1350**: Updated test name from "Phase-4 witness normalization rejection" to "Phase-4-safe with current Phase-7 type-checking rejection". Assertions now verify that the pipeline fails at Phase 7 with `TCRollBodyMismatch` instead of Phase 4 with `WitnessNormalizationError`.

## Remaining issue

The nested recursive lets expression now passes Phases 1–6 but fails at Phase 7 (type checking) with:
```
TCRollBodyMismatch (TArrow (TMu "a" (TArrow (TVar "a") TBottom)) TBottom) (TVar "t5")
```
This is a type-checking issue in the elaborated μ-type term, addressed by later roadmap items (items 2–5).

## Verification

- `cabal build all`: 0 warnings
- `cabal test`: 1175 examples, 0 failures
