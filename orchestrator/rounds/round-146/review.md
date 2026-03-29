# Round 146 Review — Phase 4 Witness Normalization TyMu Support

## Baseline Checks

### 1. Whitespace check: `git diff --check HEAD`

```
$ git -C orchestrator/worktrees/round-146 diff --check HEAD
(no output)
EXIT: 0
```

**PASS** — no whitespace or conflict-marker damage.

### 2. State JSON validation: `python3 -m json.tool orchestrator/state.json`

```
$ python3 -m json.tool orchestrator/state.json >/dev/null
EXIT: 0
```

**PASS** — valid JSON.

### 3. Roadmap bundle resolution

```
$ roadmap_dir="$(python3 -c "...")" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"
Roadmap bundle OK
EXIT: 0
```

**PASS** — all three roadmap files resolve from `orchestrator/roadmaps/2026-03-29-02-iso-recursive-inference-gap-fixes/rev-001/`.

### 4. Full build and test gate: `cabal build all && cabal test`

```
$ cabal build all
Build profile: -w ghc-9.12.2 -O1
Building test suite 'mlf2-test' for mlf2-0.2.0.0...
EXIT: 0

$ cabal test
1175 examples, 0 failures
Test suite mlf2-test: PASS
EXIT: 0
```

**PASS** — zero warnings, zero failures, full 1175-example suite green.

## Item-1 Specific Checks

### 1. Targeted test: `cabal test --test-show-details=direct --test-option='-m' --test-option='nested recursive lets'`

```
Pipeline (Phases 1-5)
  Integration Tests
    Automatic μ-introduction (item-4 edge cases)
      characterizes nested recursive lets as Phase-4-safe with current Phase-7 type-checking rejection [✔]

1 example, 0 failures
Test suite mlf2-test: PASS
```

**PASS** — test runs and passes. The test now asserts Phase 7 `TCRollBodyMismatch` instead of Phase 4 `WitnessNormalizationError`, confirming witness normalization handles TyMu without error.

### 2. No existing witness normalization tests regressed

All Phase 4 witness normalization tests pass (verified by full suite run): property-based tests, graft-weaken canonicalization, RaiseMerge coalescing, Weaken placement, validation checks, and the Phase 4 regression matrix all green.

### 3. TyMu handling in `normalizeInstanceOpsCore` and `validateNormalizedWitness`

Verified by code inspection of the diff:

- `bindersOrdered` (WitnessNorm.hs): `Just TyMu{ tnBody = muBody }` case follows body — ✅
- `abstractBoundShape` (WitnessNorm.hs): `Just TyMu{ tnBody = muBody }` enters recursion via body — ✅
- `residualNoReplayOp` (WitnessNorm.hs): `interiorContainsTyMu` guard bypasses replay contract — ✅
- `replayBindersForRoot` (WitnessValidation.hs): `Just TyMu{ tnBody = muBody }` follows body — ✅
- `requireGraftTarget` (WitnessValidation.hs): `Just TyMu{} -> Right ()` explicit case — ✅

## Plan Adherence

| Plan Step | Description | Implemented | Notes |
|-----------|-------------|-------------|-------|
| 1 | `bindersOrdered` TyMu case | ✅ | Variable name `muBody` vs plan's `b`; logic identical |
| 2 | `replayBindersForRoot` TyMu case | ✅ | Same pattern as Step 1 |
| 3 | `requireGraftTarget` TyMu case | ✅ | Exact match: `Just TyMu{} -> Right ()` |
| 4 | `abstractBoundShape` TyMu case | ✅ | Same entry-to-body pattern |
| 5 | Test upgrade | ⚠️ | Phase 7 characterization, not full success (see below) |
| 6 | Full build gate | ✅ | 1175/0 |
| 7 | Iterative debugging | ✅ | `interiorContainsTyMu` guard added |

### Step 5 assessment

The plan's Step 5 requested full pipeline success. The implementation characterizes the test at Phase 7 (`TCRollBodyMismatch`) instead. The plan's Step 7.3 explicitly anticipated this:

> "Phase 5/6 failures — The test may pass Phase 4 but fail in a later phase. That is expected and acceptable for this round."

Phase 7 is even further downstream than Phase 5/6, meaning Phase 4 witness normalization is fully resolved. The downstream type-checking error is squarely in the scope of later roadmap items (items 2–5). This is acceptable for item-1 scope.

### Extra change assessment

The `interiorContainsTyMu` guard on `residualNoReplayOp` (WitnessNorm.hs lines 322–337) was not in the original Steps 1–4 but is covered by Step 7 (iterative debugging). The implementation notes provide clear rationale: TyMu-related grafts correspond to μ-instantiation, not ∀-introduction, so the empty `replayBindersAtRoot` causes false `ReplayContractNoneRequiresReplay` errors. The guard is well-scoped and targeted.

## Evidence Summary

- **Files changed:** 3 (WitnessNorm.hs, WitnessValidation.hs, PipelineSpec.hs)
- **Net change:** +19 lines added, -3 lines removed
- **Core fix:** 5 TyMu match arms added across 2 modules (Steps 1–4 + extra guard)
- **Test upgrade:** Phase 4 → Phase 7 failure characterization
- **Regression:** None (1175 examples, 0 failures)
- **Build:** Clean, 0 warnings
- **Failure point moved:** Phase 4 `WitnessNormalizationError` → Phase 7 `TCRollBodyMismatch`

## Decision

**APPROVED**

The implementation faithfully addresses the witness normalization TyMu gap (item-1's core scope). All four planned TyMu match arms are correct and follow established patterns. The additional `interiorContainsTyMu` guard is a targeted debugging fix consistent with Step 7. The test now characterizes at Phase 7, which is 3 phases further downstream — the witness normalization is fully resolved. The downstream `TCRollBodyMismatch` is a type-checking issue addressed by later roadmap items. Zero regressions across the full 1175-example suite.
