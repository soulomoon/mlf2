# Progress Log: BUG-2026-02-16-009 OpRaise Non-Spine Context

## 2026-02-15
- Initialized task tracking files.
- Next: run pinned failing repro and capture Phi/Omega error details.
- Reproduced pinned failure:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Explicit forall annotation edge cases/explicit forall annotation round-trips on let-bound variables/" --seed 1481579064'`
  - Result: `1 example, 1 failure`
  - Error: `PhiTranslatabilityError ["OpRaise (non-spine): missing computation context", target NodeId 6, orderRoot NodeId 2, idsSynced [Just NodeId 0], bindParent Just (GenRef 1, BindFlex)]`.
- Gathered structured probe evidence via `cabal repl mlf2-test` scripts:
  - `/tmp/inspect_bug009.ghci`: confirmed witness ops + trace copy-map for failing expression.
  - `/tmp/inspect_bug009_context.ghci`: confirmed source target has context while adopted target does not.
  - `/tmp/inspect_bug009_canon.ghci`: confirmed canonicalization does not rewrite `OpRaise 1`; drift occurs in Omega copy-map adoption.
  - `/tmp/inspect_bug004v2_context.ghci`: observed similar source-vs-adopted context split in BUG-004-V2 path.
- Root cause isolated: Omega `OpRaise` non-spine context computation currently depends on unconditionally adopted target (`etCopyMap`) and fails when that target lacks a valid context.
- Implemented minimal root-cause patch in `src/MLF/Elab/Phi/Omega.hs`:
  - Added source/adopted dual-target tracking for `OpRaise`.
  - Added source-target root-context fallback in non-spine branch when adopted-target context computation cannot proceed.
- RED/GREEN verification:
  - RED (before patch): explicit-forall round-trip test failed with `OpRaise (non-spine): missing computation context`.
  - GREEN (after patch): same test passes.
- Regression verification:
  - `BUG-004`, `BUG-002-V4`, strict target matrix, explicit-forall group, and context fallback invariant all pass.
- Full gate run:
  - `cabal build all && cabal test` -> `672 examples, 4 failures` in known open buckets (`001/002/007/008`).
- Synced docs and tracking artifacts:
  - `Bugs.md`: moved `BUG-2026-02-16-009` to Resolved with root cause + regression links.
  - `TODO.md`: added Task 12 summary for BUG-009 fix.
  - `implementation_notes.md`: added BUG-009 fix note.
  - `CHANGELOG.md`: added Unreleased entry for BUG-009.
