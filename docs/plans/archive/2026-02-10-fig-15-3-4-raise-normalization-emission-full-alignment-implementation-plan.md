# Fig. 15.3.4 Raise Normalization/Emission Full Alignment Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close Task 4 with strict thesis-exact evidence that presolution witness normalization/emission aligns with Fig. 15.3.4 for `Graft`, `Weaken`, `Merge`, `Raise`, and `RaiseMerge`, with no undocumented deviations.

**Architecture:** Use a hybrid matrix-first workflow: each matrix row gets (1) a unit/invariant assertion at witness validation/normalization level and (2) a presolution-path assertion through `computePresolution`/`normalizeEdgeWitnessesM`. Drive all work with strict TDD (RED → GREEN), only touching production code when a matrix row fails for a thesis-valid behavior.

**Tech Stack:** Haskell (`hspec`, `cabal`), modules under `MLF.Constraint.Presolution.*`, tests in `test/Presolution/*`, thesis anchors from `papers/these-finale-english.txt`.

---

## Closure Contract (Strict)

Task 4 is complete only when:
1. All 15 matrix rows below are represented by named tests.
2. Every row has explicit expected outcome (normalized shape or exact error constructor).
3. Matrix-only matcher is green.
4. Full gate is green: `cabal build all && cabal test`.
5. `.kiro` status is synchronized and no contradictory “partial” claims remain for completed rows.

If any row is unreachable, do **not** silently drop it: add a dedicated proof-style test or document a thesis-grounded explicit deviation (expected to be zero under this strict plan).

---

## 15-Row Matrix (Source of Truth)

| Row ID | Op | Intent | Unit Layer | Presolution Layer | Expected |
|---|---|---|---|---|---|
| R-GRAFT-VALID-01 | Graft | valid interior/root target | WitnessSpec | MergeEmissionSpec or WitnessSpec presolution case | accept canonical shape |
| R-GRAFT-INVALID-02 | Graft | invalid/non-translatable target | WitnessSpec | WitnessSpec presolution case | explicit translatability error |
| R-GRAFT-NORM-03 | Graft | canonical ordering in mixed sequence | WitnessSpec | presolution replay fixture | stable normalized order |
| R-WEAKEN-VALID-04 | Weaken | valid weaken semantics | WitnessSpec | presolution replay fixture | accept / expected no-op semantics |
| R-WEAKEN-INVALID-05 | Weaken | delayed-weaken violation | WitnessSpec | WitnessSpec presolution case | `DelayedWeakenViolation` |
| R-WEAKEN-NORM-06 | Weaken | reorder/idempotence | WitnessSpec | presolution replay fixture | deterministic normalized order |
| R-MERGE-VALID-07 | Merge | valid direction/context | WitnessSpec | MergeEmissionSpec | accept |
| R-MERGE-INVALID-08 | Merge | wrong `<P` or rigid mismatch | WitnessSpec | WitnessSpec presolution case | `MergeDirectionInvalid` or `RigidOperandMismatch` |
| R-MERGE-NORM-09 | Merge | no permissive fallback | WitnessSpec | presolution normalization case | strict failure or stable canonical merge |
| R-RAISE-VALID-10 | Raise | transitive-flex interior raise | WitnessSpec | presolution-path fixture | accept |
| R-RAISE-INVALID-11 | Raise | non-transitive-flex raise | WitnessSpec | WitnessSpec + MergeEmissionSpec presolution case | `NotTransitivelyFlexBound` |
| R-RAISE-NORM-12 | Raise | canonical sequencing/coalescing | WitnessSpec | presolution replay fixture | stable normalized shape |
| R-RAISEMERGE-VALID-13 | RaiseMerge | valid exterior-target raise-merge | WitnessSpec | MergeEmissionSpec | accept |
| R-RAISEMERGE-INVALID-14 | RaiseMerge | inside-interior or rigid mismatch invalid | WitnessSpec | WitnessSpec presolution case | `RaiseMergeInsideInterior` / `RigidOperandMismatch` |
| R-RAISEMERGE-NORM-15 | RaiseMerge | `Raise;Merge -> RaiseMerge` idempotence | WitnessSpec | presolution replay fixture | coalesced stable shape |

---

### Task 1: Install matrix naming and harness conventions

**Files:**
- Modify: `test/Presolution/WitnessSpec.hs`
- Modify: `test/Presolution/MergeEmissionSpec.hs`

**Step 1: Add stable row-id prefixes to test names**
- Prefix affected tests with row IDs (`R-...`).

**Step 2: Add missing row stubs as failing tests**
- For rows without coverage, add minimal failing `it` blocks with TODO-free expectations.

**Step 3: Verify RED for newly added rows**
Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "R-(GRAFT|WEAKEN|MERGE|RAISE|RAISEMERGE)"'
```
Expected: one or more failures for uncovered/incorrect behavior.

**Step 4: Commit harness naming/stubs**
```bash
git add test/Presolution/WitnessSpec.hs test/Presolution/MergeEmissionSpec.hs
git commit -m "test: scaffold Fig.15.3.4 15-row witness matrix"
```

---

### Task 2: Close all INVALID rows first (safety-first)

**Files:**
- Modify: `test/Presolution/WitnessSpec.hs`
- Modify: `test/Presolution/MergeEmissionSpec.hs`
- Modify only if needed: `src/MLF/Constraint/Presolution/WitnessValidation.hs`

**Rows:** `R-GRAFT-INVALID-02`, `R-WEAKEN-INVALID-05`, `R-MERGE-INVALID-08`, `R-RAISE-INVALID-11`, `R-RAISEMERGE-INVALID-14`

**Step 1: Add/adjust failing assertions row-by-row**
- One row at a time; assert exact constructor.

**Step 2: Run single-row matcher after each row**
Example:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "R-RAISE-INVALID-11"'
```
Expected: RED before fix, GREEN after minimal fix.

**Step 3: Implement minimal production fixes only for thesis-valid failures**
- Keep rigid-node identity behavior intact.

**Step 4: Run invalid-row aggregate matcher**
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "R-.*-INVALID"'
```
Expected: all invalid rows GREEN.

**Step 5: Commit invalid-row closure**
```bash
git add src/MLF/Constraint/Presolution/WitnessValidation.hs test/Presolution/WitnessSpec.hs test/Presolution/MergeEmissionSpec.hs
git commit -m "fix: enforce strict invalid-case translatability across witness ops"
```

---

### Task 3: Close all VALID rows (acceptance semantics)

**Files:**
- Modify: `test/Presolution/WitnessSpec.hs`
- Modify: `test/Presolution/MergeEmissionSpec.hs`
- Modify only if needed: `src/MLF/Constraint/Presolution/WitnessCanon.hs`
- Modify only if needed: `src/MLF/Constraint/Presolution/WitnessNorm.hs`

**Rows:** `R-GRAFT-VALID-01`, `R-WEAKEN-VALID-04`, `R-MERGE-VALID-07`, `R-RAISE-VALID-10`, `R-RAISEMERGE-VALID-13`

**Step 1: Add unit acceptance fixtures per row**
- Assert accepted output shape (`Right ()` or expected normalized ops).

**Step 2: Add presolution-path acceptance fixtures per row**
- Assert `computePresolution` success and expected witness shape where meaningful.

**Step 3: Run valid-row aggregate matcher**
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "R-.*-VALID"'
```
Expected: all GREEN.

**Step 4: Commit valid-row closure**
```bash
git add test/Presolution/WitnessSpec.hs test/Presolution/MergeEmissionSpec.hs src/MLF/Constraint/Presolution/WitnessCanon.hs src/MLF/Constraint/Presolution/WitnessNorm.hs
git commit -m "test: close Fig.15.3.4 valid-case witness rows"
```

---

### Task 4: Close all NORM rows (canonicalization/idempotence)

**Files:**
- Modify: `test/Presolution/WitnessSpec.hs`
- Modify if needed: `src/MLF/Constraint/Presolution/WitnessCanon.hs`

**Rows:** `R-GRAFT-NORM-03`, `R-WEAKEN-NORM-06`, `R-MERGE-NORM-09`, `R-RAISE-NORM-12`, `R-RAISEMERGE-NORM-15`

**Step 1: Add normalization fixtures row-by-row**
- Include idempotence assertions where applicable.

**Step 2: Add presolution replay assertions for selected rows**
- Ensure normalized shape survives pipeline wiring.

**Step 3: Run norm-row aggregate matcher**
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "R-.*-NORM"'
```
Expected: all GREEN.

**Step 4: Commit normalization-row closure**
```bash
git add test/Presolution/WitnessSpec.hs src/MLF/Constraint/Presolution/WitnessCanon.hs
git commit -m "test: close Fig.15.3.4 normalization rows"
```

---

### Task 5: Matrix closure gate and full verification

**Files:**
- No code changes expected

**Step 1: Run matrix-only gate**
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "R-(GRAFT|WEAKEN|MERGE|RAISE|RAISEMERGE)-(VALID|INVALID|NORM)"'
```
Expected: 15/15 rows GREEN.

**Step 2: Run witness-focused gate**
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "Witness normalization|Merge/RaiseMerge emission|R-"'
```
Expected: GREEN.

**Step 3: Run full project gate**
```bash
cabal build all && cabal test
```
Expected: all tests pass.

---

### Task 6: Documentation/task closure sync (strict)

**Files:**
- Modify: `.kiro/specs/paper-faithfulness-remaining-deltas/requirements.md`
- Modify: `.kiro/specs/paper-faithfulness-remaining-deltas/tasks.md`
- Modify: `TODO.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify canonical tracker: `/Volumes/src/mlf4/Bugs.md`

**Step 1: Mark Task 4 status based on matrix evidence**
- If 15/15 rows green, set Task 4.1 complete with evidence links.

**Step 2: Update requirement statuses to remove stale partial claims**
- Keep statements strictly evidence-backed.

**Step 3: Update changelog and bug tracker entries**
- Include matrix gate command and results.

**Step 4: Commit docs/tracker closure**
```bash
git add .kiro/specs/paper-faithfulness-remaining-deltas/requirements.md .kiro/specs/paper-faithfulness-remaining-deltas/tasks.md TODO.md implementation_notes.md CHANGELOG.md
git commit -m "docs: close Fig.15.3.4 witness-alignment Task 4 with 15-row matrix evidence"
```

---

## Thesis Anchors to cite in tests/docs

- `papers/these-finale-english.txt:17757-17760`
- `papers/these-finale-english.txt:17807-17815`
- `papers/these-finale-english.txt:17867-17910`

---

## Notes for implementer

- Keep changes minimal and thesis-exact; avoid behavior drift outside witness normalization/emission.
- Prefer constructor-level error assertions over string matching.
- Do not weaken rigid-node identity semantics for Raise/Merge/RaiseMerge.
- If any row appears redundant with existing tests, keep row ID and map it to the existing test while preserving matrix completeness.
