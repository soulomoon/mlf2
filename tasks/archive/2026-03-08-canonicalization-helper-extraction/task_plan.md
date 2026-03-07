# Task Plan: Canonicalization Helper Extraction

## Metadata
- Date: 2026-03-08
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro

## Goal
Extract the duplicated canonicalization helpers shared by `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` into a single internal utility module, keep behavior unchanged, and add a focused regression guard against re-duplication.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Inspect duplicated helpers and target home | complete | Reviewed canonicalization modules/tests |
| 2. Add focused duplication guard | complete | Added `Constraint.SolvedSpec` source guard |
| 3. Extract shared helper module | complete | Added `MLF.Constraint.Canonicalization.Shared` and rewired both consumers |
| 4. Run targeted canonicalization tests | complete | Focused dedup/parity/canonicalizer slices passed |
| 5. Full gate and tracker sync | complete | Full gate passed; docs synced; task archived |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
