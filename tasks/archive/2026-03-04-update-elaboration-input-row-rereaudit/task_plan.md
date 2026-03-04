# Task Plan — 2026-03-04 Update Elaboration Input Row (Re-audit)

## Goal
Update the `Elaboration input` row in `docs/notes/2026-02-27-transformation-mechanism-table.md` by re-auditing against `papers/these-finale-english.txt` and current implementation.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Identify target row and collect thesis references | complete | Row located; Def. 15.3.12 / Fig. 15.3.5 / §15.3.6 extracted (`these-finale-english.txt:14087-14174`). |
| 2. Audit active code path for elaboration input | complete | Confirmed chi-native runtime path and remaining solved-typed legacy/test-debug surfaces in `src/MLF/Elab/*`. |
| 3. Edit table row and references | complete | Updated only `Elaboration input` row wording + references; refreshed source revision. |
| 4. Record findings and verify diff scope | complete | Findings/progress updated; verified intended doc-only change set (plus task tracking files). |

## Decisions
- Restrict edits to documentation unless code changes are explicitly requested.
- Use the thesis as primary source of truth; use `xmlf` only if thesis is silent.
- Preserve table criterion: `thesis exact include test-only code paths`.

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | None so far | 1 | N/A |
