# Findings — Fig. 15.3.4 Raise Normalization/Emission Full Alignment

## Key Discoveries

1. Existing witness tests cover many semantics but do not expose the full closure contract as explicit 15 row IDs.
2. Matrix-style naming and explicit expected outcomes are required to convert current coverage into auditable closure evidence.
3. Presolution-path fixtures already exist and can be reused for row-level proof where behavior is already thesis-exact.
4. A strict row-prefix gate works reliably with `--match R-`; complex alternation patterns passed through `cabal test --test-options` can under-select (`0 examples`) depending on CLI parsing.
5. Full matrix closure now exists as 15 unique IDs (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) across `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs`.

## Thesis Alignment Notes

- Primary anchors for this task:
  - `papers/these-finale-english.txt:17757-17760`
  - `papers/these-finale-english.txt:17807-17815`
  - `papers/these-finale-english.txt:17867-17910`

## Open Questions

- Resolved: all 15 rows are now present with explicit expected outcomes and green matrix/full gates.

## Matrix Closure Evidence

- Matrix gate command: `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`
  - Result: `19 examples, 0 failures`
  - Unique matrix rows observed: 15/15
- Full gate command: `cabal build all && cabal test`
  - Result: `608 examples, 0 failures`

### 15-Row Completion

- `R-GRAFT-VALID-01` ✅
- `R-GRAFT-INVALID-02` ✅
- `R-GRAFT-NORM-03` ✅
- `R-WEAKEN-VALID-04` ✅
- `R-WEAKEN-INVALID-05` ✅
- `R-WEAKEN-NORM-06` ✅
- `R-MERGE-VALID-07` ✅
- `R-MERGE-INVALID-08` ✅
- `R-MERGE-NORM-09` ✅
- `R-RAISE-VALID-10` ✅
- `R-RAISE-INVALID-11` ✅
- `R-RAISE-NORM-12` ✅
- `R-RAISEMERGE-VALID-13` ✅
- `R-RAISEMERGE-INVALID-14` ✅
- `R-RAISEMERGE-NORM-15` ✅
