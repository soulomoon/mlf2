# Elaboration Input Thesis-Exact Parallel Work Implementation Plan (Refresh)

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Make the Transformation Mechanism Table row `Elaboration input` thesis-exact under the repo's strict criterion (`includes test-only code paths`) by removing solved-typed test-only Phi helper surfaces and migrating tests to `χp`-native helper signatures.

**Architecture:** Production elaboration is already `χp`-native (`PresolutionView` + trace-required `phiFromEdgeWitnessWithTrace`). The remaining mismatch is in `MLF.Elab.Phi.TestOnly`, which still accepts `Solved` and bridges internally via `fromSolved`. This plan retires that solved-typed test-only contract, migrates callsites in `test/ElaborationSpec.hs`, then flips the row only after all guard slices and full suite pass.

**Tech Stack:** Haskell (`cabal`, `hspec`), elaboration modules under `src/MLF/Elab/*`, specs under `test/*`, docs under `docs/notes/` + repo tracking docs.

---

## 0. Current Blocking Surfaces (Re-Audited 2026-03-04)

1. `src/MLF/Elab/Phi/TestOnly.hs`
- `phiFromEdgeWitnessNoTrace`, alias `phiFromEdgeWitness`, and `phiFromEdgeWitnessAutoTrace` still take `Solved`.
- `phiFromEdgeWitnessAutoTrace` still converts via `fromSolved solved`.

2. `test/ElaborationSpec.hs`
- Many callsites still pass solved-typed arguments into `ElabTest.phiFromEdgeWitnessAutoTrace` and `ElabTest.phiFromEdgeWitnessNoTrace`.

3. TMT row status
- `docs/notes/2026-02-27-transformation-mechanism-table.md` row `Elaboration input` remains `Thesis-exact = No` specifically because strict policy includes these test-only solved-typed surfaces.

Thesis anchors for row closure:
- `papers/these-finale-english.txt:14087-14097` (Def. 15.3.12)
- `papers/these-finale-english.txt:14112-14117` (§15.3.6)

---

## 1. Parallel Work Topology

| Team | Mission | File Ownership |
|---|---|---|
| Team A (`guards`) | Strengthen strict guard coverage so solved-typed test-only APIs are explicitly forbidden | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`phi-testonly-api`) | Remove solved-typed signatures from test-only Phi helpers and keep behavior | `src/MLF/Elab/Phi/TestOnly.hs` |
| Team C (`spec-migration`) | Migrate elaboration spec callsites to new chi-native helper signatures | `test/ElaborationSpec.hs` |
| Team D (`docs-closeout`) | Flip TMT row and sync docs/changelog/TODO after green gates | `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md` |
| Team E (`verification`) | Run all required gates and publish final evidence | command execution + task artifacts |

Wave order:
1. Wave 0 (serial): Team A
2. Wave 1 (parallel): Team B + Team C
3. Wave 2 (serial): merge fixups + Team E verification
4. Wave 3 (serial): Team D docs closeout

---

## 2. Merge Gates

### Gate A (after Team A)
Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected:
- RED before API migration (new strict checks should fail initially).

### Gate B (after Team B + Team C integration)
Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected:
- PASS.

### Gate C (quality slices)
Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`

Expected:
- PASS.

### Final Gate
Run:
`cabal build all && cabal test`

Expected:
- PASS (`all examples, 0 failures`).

---

## 3. Task Breakdown (Bite-Sized, Agent-Executable)

### Task 1: Strict Guard Hardening For Test-Only API (Team A)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add failing source-guard markers for `MLF.Elab.Phi.TestOnly` solved-typed signatures**
- Add checks that fail if signatures still contain solved-typed parameters for:
  - `phiFromEdgeWitnessNoTrace`
  - `phiFromEdgeWitness`
  - `phiFromEdgeWitnessAutoTrace`

**Step 2: Run guard slice and confirm RED**
Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
Expected:
- FAIL due to newly added strict marker checks.

**Step 3: Commit**
```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: harden elab-input guard for solved-typed test-only phi APIs"
```

---

### Task 2: Retire Solved-Typed Test-Only Phi API Surface (Team B)

**Files:**
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`

**Step 1: Remove `Solved` from helper signatures**
- Replace solved-typed signatures with chi-native shape:
  - `phiFromEdgeWitnessNoTrace :: TraceConfig -> GeneralizeAtWith -> Maybe SchemeInfo -> EdgeWitness -> Either ...`
  - `phiFromEdgeWitness :: ...` (alias unchanged semantically, no solved arg)
  - `phiFromEdgeWitnessAutoTrace :: TraceConfig -> GeneralizeAtWith -> PresolutionView -> Maybe SchemeInfo -> EdgeWitness -> Either ...`

**Step 2: Remove solved-bridge dependencies**
- Remove `MLF.Constraint.Solved (Solved)` import.
- Remove `fromSolved` dependency and usage.
- Keep synthetic trace construction and fail-fast behavior (`MissingEdgeTrace`) unchanged.

**Step 3: Build library target**
Run:
`cabal build mlf2-test`
Expected:
- Compile errors only in outdated test callsites (owned by Team C), not in Team B module.

**Step 4: Commit**
```bash
git add src/MLF/Elab/Phi/TestOnly.hs
git commit -m "refactor: make phi test-only helpers chi-native and remove solved bridge"
```

---

### Task 3: Migrate Elaboration Spec Calls To Chi-Native Test Helpers (Team C)

**Files:**
- Modify: `test/ElaborationSpec.hs`

**Step 1: Update all `phiFromEdgeWitnessAutoTrace` callsites**
- Old form:
  - `ElabTest.phiFromEdgeWitnessAutoTrace ... (generalizeAtWithActive solved) solved ...`
- New form:
  - `ElabTest.phiFromEdgeWitnessAutoTrace ... (generalizeAtWithActive solved) (presolutionViewFromSolved solved) ...`

**Step 2: Update `phiFromEdgeWitnessNoTrace` callsites**
- Remove solved positional argument.
- Preserve expectation of `MissingEdgeTrace` failures.

**Step 3: Run focused spec slices**
Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "no-trace test entrypoint fails fast"'`
Expected:
- PASS.

**Step 4: Commit**
```bash
git add test/ElaborationSpec.hs
git commit -m "test: migrate elaboration phi test-only helper callsites to chi-native signatures"
```

---

### Task 4: Verification Gatekeeper (Team E)

**Files:**
- No code ownership; command verification and evidence capture.

**Step 1: Run gate bundle**
Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
`cabal build all && cabal test`

**Step 2: Capture exact pass counts in progress notes**
- Record matcher counts and full-suite totals exactly as command output reports.

**Step 3: Commit verification notes (if tracked in task files)**
```bash
git add tasks/todo/<task-folder>/progress.md tasks/todo/<task-folder>/findings.md
git commit -m "chore: record elaboration-input thesis-exact verification evidence"
```

---

### Task 5: Docs + TMT Closeout (Team D)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `Bugs.md` only if a new defect is discovered during migration.

**Step 1: Flip row classification only after all gates are green**
- Update row `Elaboration input` from `No` -> `Yes`.
- Refresh references to current line ranges and revision hash.

**Step 2: Sync implementation/changelog/TODO notes**
- Add concise entries documenting retirement of solved-typed test-only Phi surfaces.
- Add verification evidence references.

**Step 3: Commit**
```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md
git commit -m "docs: mark elaboration-input thesis-exact after strict test-only API retirement"
```

---

## 4. Agent Dispatch Procedure

1. Spawn Team A first and require red-proof on Gate A.
2. Spawn Team B and Team C in parallel with strict non-overlapping ownership:
- Team B owns only `src/MLF/Elab/Phi/TestOnly.hs`.
- Team C owns only `test/ElaborationSpec.hs`.
3. Integrate B+C, then run Gate B/Gate C.
4. Run Team E verification bundle.
5. Run Team D docs closeout only after verification is green.

Execution prompt requirement for each worker:
- Tell each worker they are not alone in the codebase and must ignore unrelated edits.
- Enforce ownership boundaries above.

---

## 5. Success Criteria

1. `MLF.Elab.Phi.TestOnly` no longer exposes solved-typed helper signatures.
2. `test/ElaborationSpec.hs` uses only chi-native helper signatures.
3. `elab-input thesis-exact guard`, `checked-authoritative`, and `Dual-path verification` all pass.
4. `cabal build all && cabal test` passes.
5. TMT row `Elaboration input` is updated to `Thesis-exact = Yes` with current evidence.
