# Task Plan — 2026-03-04 Elaboration Input Row Thesis/Codebase Review

## Summary
Update the `Elaboration input` row in `docs/notes/2026-02-27-transformation-mechanism-table.md` by re-auditing against `papers/these-finale-english.txt` and current implementation, then determine whether the row is thesis-exact.

## Scope
- Target doc: `docs/notes/2026-02-27-transformation-mechanism-table.md` (row `Elaboration input` only).
- Sources of truth: `papers/these-finale-english.txt` (primary), current code in `src/MLF/Elab/*` and related tests.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Collect current row + thesis evidence | complete | Verified thesis references for Def. 15.3.12 and §15.3.6 and confirmed table row wording matches source intent. |
| 2. Verify implementation evidence | complete | Confirmed chi-native elaboration input path, fail-fast `MissingEdgeTrace`, test-only helper signatures, and guard slices/full gate all pass. |
| 3. Update row if needed + classify thesis-exact | complete | No row-content change required; refreshed doc source revision to current HEAD (`fb82990`). Classification remains thesis-exact under strict policy. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None | - | - |
