# Findings: 2026-02-23 Thesis Exact Review

## Baseline Status (2026-02-24)

- **Tests:** 781 examples, 0 failures
- **Conformance gate:** PASS (obligations ledger, claims checker, all anchor slices)
- **Claims checker:** PASS (21 claims, 7 deviations, all cross-links valid, no orphans)
- **Open bugs:** None (`Bugs.md` open section is empty)
- **Active tasks:** Only this review task remains in `tasks/todo/`

## Claims Coverage Summary

All 21 claims have status `defended`. Coverage by chapter:

| Chapter | Claims | Status |
|---------|--------|--------|
| Ch. 7 (Unification) | CLM-UNIFICATION, CLM-GEN-UNIFICATION | defended |
| Ch. 9 (Constraint Gen) | CLM-CGEN-SHAPE, CLM-CGEN-SCOPING | defended |
| Ch. 10 (Expansion/Presolution) | CLM-EXPANSION-MINIMALITY, CLM-PRESOLUTION-PRINCIPALITY | defended |
| Ch. 11 (Local Transforms) | CLM-WITNESS-NORMALIZATION | defended |
| Ch. 12 (Global Algorithm) | CLM-ACYCLICITY, CLM-SOLVER-CORRECTNESS | defended |
| Ch. 14 (xMLF Type System) | 7 claims (preservation, progress, determinism, inst, typing, reduction, inst-application) | defended |
| Ch. 15 (Elaboration) | 5 claims (translatable presolution, phi, sigma, elaboration correctness, witness translation) | defended |

## Obligations Coverage

- 104 obligations across Ch. 4-15, all status `anchored`
- Every obligation has `supports_claims` back-links
- Obligations ledger markdown is drift-free

## Deviations Register

7 deviations, all `implementation-choice` or `proof-gap`, none `open`:

| ID | Impact | Type |
|----|--------|------|
| DEV-WITNESS-NORM-NO-PROOF | semantic-neutral | proof-gap |
| DEV-CONSTRAINT-REPR-SIMPLIFIED | semantic-neutral | implementation-choice |
| DEV-PHASE7-NO-FORMAL-LINK | semantic-neutral | proof-gap |
| DEV-TYEXP-OCCURRENCE-REPR | semantic-neutral | implementation-choice |
| DEV-PHI-WITNESS-WEAKEN-SUPPRESSION | semantic-minor | implementation-choice |
| DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION | semantic-minor | implementation-choice |
| DEV-PHI-STANDALONE-GRAFT-EXTENSION | semantic-minor | implementation-choice |

## Gap Analysis

### No Critical Gaps

The implementation is thesis-faithful across all covered chapters (4-15). No open bugs, no undefended claims, no open deviations.

### Remaining Formalization Debt (Low Priority)

1. **Mechanized proofs (DEV-WITNESS-NORM-NO-PROOF, DEV-PHASE7-NO-FORMAL-LINK):**
   Both are `proof-gap` deviations with `semantic-neutral` impact. Witness normalization and Phase 7 theorems (preservation, progress, determinism) are covered by executable property proxies (QuickCheck, 300 samples each) but lack mechanized proofs (Agda/Liquid Haskell). This is explicitly future work.

2. **Three Phi implementation choices (semantic-minor):**
   - Weaken suppression in witness emission
   - Keep-binder weaken suppression in Omega
   - Standalone graft extension
   All three are load-bearing for translatability. They extend the thesis's canonical paired graft/weaken shape to handle real-world normalized witnesses. Well-tested and documented.

### Test Coverage Observations

- 781 tests total, 0 failures
- Property tests: TypeSoundnessSpec (preservation, progress, determinism, n-step), TranslatablePresolutionSpec, PhiSoundnessSpec, ExpansionMinimalitySpec
- Gate anchors: 15-row witness matrix (R-*), Phase 7 theorems, translatable presolution, Phi soundness, expansion minimality
- Bug regression coverage: BUG-002, BUG-003, BUG-004, BUG-2026-02-17-001/002, BUG-2026-02-20-001 all have deterministic seed-pinned regressions

### Infrastructure Health

- Conformance gate enforces: obligations ledger drift, claims schema/cross-links, 5 anchor slices
- Claims checker validates: schema, cross-links, code path presence, deviation orphans
- All scripts pass cleanly

## Prioritized Roadmap

### P0 — Nothing. No blocking gaps.

### P1 — Nice-to-have improvements (no urgency)

1. **Deviation count sync:** TODO.md says "9 deviations" but register has 7 (2 were cleaned up). Minor doc drift.
2. **implementation_notes.md date:** Defensible exactness work is logged under 2026-02-22 but plan referenced 2026-02-21. Cosmetic.
3. **Test count sync:** TODO.md Task 22 says "765 examples" but current suite has 781. Tests were added since.

### P2 — Future work (explicitly deferred)

1. **Mechanized proofs:** Agda or Liquid Haskell formalization of preservation/progress/witness-normalization.
2. **QuickCheck generator enrichment:** Current property tests use small fixed expressions. Richer generators (deeper nesting, more annotation shapes, bounded-forall patterns) would increase confidence.
3. **Chapter 13 coverage:** No claims or obligations for Chapter 13 (if applicable to the implementation scope).

## Conclusion

The repository is in a strong thesis-faithful state. All 21 claims defended, 104 obligations anchored, 7 deviations documented and accepted, 781 tests passing, conformance gate green. The evidence chain from thesis clause → claim → obligation → code path → test → gate is complete and machine-checked. No action items are blocking.
