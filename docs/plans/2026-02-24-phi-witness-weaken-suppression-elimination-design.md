# Design: Eliminate DEV-PHI-WITNESS-WEAKEN-SUPPRESSION

**Date:** 2026-02-24
**Deviation:** DEV-PHI-WITNESS-WEAKEN-SUPPRESSION
**Thesis ref:** Definition 15.3.4 / Figure 10 (Phi witness translation)
**Goal:** Make witness emission thesis-exact by always emitting OpWeaken, then handling translatability in Omega.

## Problem

Witness emission suppresses OpWeaken generation at two sites:

1. **`classify` guard** (`src/MLF/Constraint/Presolution/Witness.hs:148-151`):
   Suppresses OpWeaken when `suppressWeaken` (suffix of ExpCompose has forall) or `argIsGenBound` (graft arg is scheme root flexibly bound to its own gen node).

2. **`dropWeakenOps` call** (`src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs:73`):
   Blanket-strips all OpWeaken from annotation edges (`cAnnEdges`).

The thesis (Def. 15.3.4 / Figure 10) always emits the paired graft/weaken shape. These guards are load-bearing for translatability in the current Omega pipeline but are not thesis-faithful.

## Approach

**Approach A — Extend Omega:** Remove emission guards, extend Omega to handle extra OpWeaken operations. Emission becomes thesis-pure; translation becomes smart.

## Analysis of Extra OpWeaken Cases

### Case 1: `suppressWeaken` (suffix has forall)

When `ExpCompose [ExpInstantiate args, ExpForall specs]`, the instantiate step currently suppresses OpWeaken because the forall step will introduce fresh binders.

With suppression removed: O phase runs first (adding forall intros to the type), then Omega replays ops including the new OpGraft+OpWeaken pairs. The original binder is still in the quantifier spine. The fused `OpGraft+OpWeaken` pattern (Omega.hs:750) translates to `InstApp argTy` or `InstElim`. The binder is resolvable and the type state is correct.

**Expected Omega behavior:** Existing fused pattern handles this. No Omega changes needed for this case.

### Case 2: `argIsGenBound` (gen-bound alias)

When the graft arg is a scheme root flexibly bound to its own gen node, the binder is unbounded (`mbBound = Nothing`). The OpGraft+OpWeaken pair targets a binder in the scheme being instantiated.

With suppression removed: The fused `OpGraft+OpWeaken` pattern should translate normally — the binder is in the quantifier spine, and `InstApp argTy` where argTy may be polymorphic is valid in MLF.

**Expected Omega behavior:** Existing fused pattern handles this. If argTy reifies to a polymorphic type, `InstApp (∀...)` is well-formed in xMLF.

### Case 3: Annotation edges (`dropWeakenOps`)

Annotation edges currently strip ALL OpWeaken. With weakens restored, Omega sees OpWeaken for every binder in the annotation's scheme.

With suppression removed: `keepBinderKeys` already identifies binders appearing in the target type and skips their OpWeaken. Binders not in the target are eliminated via `InstElim`.

**Expected Omega behavior:** `keepBinderKeys` mechanism handles preservation. Remaining weakens translate to `InstElim` normally. May need to verify `keepBinderKeys` coverage is sufficient for annotation edges.

### Case 4: Unfused OpWeaken (normalization reordering)

When normalization (`reorderWeakenWithEnv`) moves OpWeaken away from its paired OpGraft, the fused pattern won't match. The standalone OpWeaken case (Omega.hs:907) handles this — it checks root, keepBinderKeys, and isBinderNode before emitting InstElim.

**Expected Omega behavior:** Standalone OpWeaken case handles this. The binder must be resolvable via `resolveTraceBinderTarget`.

## Risk Assessment

**Low risk:** Cases 1 and 4 — existing Omega patterns handle these directly.

**Medium risk:** Cases 2 and 3 — may surface edge cases where:
- Gen-bound arg reifies to an unexpected type shape
- Annotation edge binders aren't fully covered by `keepBinderKeys`
- `resolveTraceBinderTarget` can't resolve a previously-suppressed binder

**Mitigation:** TDD approach — write failing tests first for each case, then remove suppression, then fix Omega if needed.

## Files Changed

| File | Change |
|------|--------|
| `src/MLF/Constraint/Presolution/Witness.hs` | Remove `suppressWeaken \|\| argIsGenBound` guard in `classify`; remove `argIsGenBound` helper; simplify `witnessAlg` ExpCompose to not compute suffix flags |
| `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs` | Remove `dropWeakenOps` call in `edgeWitnessPlan`; remove `dropWeakenOps` function; remove `suppressWeaken` parameter |
| `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs` | Remove `suppressWeaken` from `EdgePlan` if threaded through plan |
| `src/MLF/Elab/Phi/Omega.hs` | Extend OpWeaken/OpGraft+OpWeaken handling if any new cases fail |
| `test/Presolution/WitnessSpec.hs` | Update existing suppression test; add thesis-exact emission tests |
| `test/Presolution/EdgePlannerSpec.hs` | Update annotation-edge suppressWeaken test |
| `test/ElaborationSpec.hs` | Add end-to-end tests for previously-suppressed cases |
| `docs/thesis-deviations.yaml` | Retire DEV-PHI-WITNESS-WEAKEN-SUPPRESSION |
| `docs/thesis-claims.yaml` | Remove deviation from CLM-PHI-CORRECTNESS |

## Success Criteria

1. `witnessFromExpansion` always emits OpWeaken for unbounded binders (no conditional guards)
2. `edgeWitnessPlan` does not strip OpWeaken for annotation edges
3. All 781+ existing tests pass
4. New tests verify thesis-exact emission for each previously-suppressed case
5. DEV-PHI-WITNESS-WEAKEN-SUPPRESSION retired from deviation register
6. `./scripts/check-thesis-claims.sh` PASS
7. `./scripts/thesis-conformance-gate.sh` PASS
