# A1 Audit + Closure — Findings

## 2026-02-17
- `TODO.md` currently marks `A1 (P1)` open in both Phase 4 and Audit Backlog sections.
- `.kiro/specs/paper-faithfulness-remaining-deltas/tasks.md` indicates strict `OpRaise` non-spine fallback branches were removed and translatability errors are explicit; this suggests A1 may be implementationally complete but not fully closed in trackers.
- Code audit:
  - `src/MLF/Constraint/Presolution/WitnessCanon.hs` `normalizeInstanceOpsFull` executes `checkMergeDirection` and returns `Left (MergeDirectionInvalid ...)` on malformed merge direction.
  - `src/MLF/Constraint/Presolution/WitnessNorm.hs` propagates normalization failures as `WitnessNormalizationError` without fallback acceptance.
- Existing tests already cover strict behavior:
  - `R-MERGE-NORM-09: normalizeInstanceStepsFull rejects wrong merge direction`.
  - `fails fast with MergeDirectionInvalid via presolution normalization`.
- Verification evidence (2026-02-17):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match R-MERGE-NORM-09'` => PASS (2 examples, 0 failures)
  - `cabal test mlf2-test --test-show-details=direct --test-options="--match=\"fails fast with MergeDirectionInvalid via presolution normalization\""` => PASS (1 example, 0 failures)
  - `cabal build all && cabal test` => PASS
- Closure sync applied:
  - `TODO.md`: both A1 tracker entries marked complete with dated AC closure note.
  - `implementation_notes.md`: added dedicated A1 closure audit section with command evidence.
  - `CHANGELOG.md`: added concise A1 closure audit entry under `Unreleased`.
