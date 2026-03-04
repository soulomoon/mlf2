# Task Plan — 2026-03-04 Update Elaboration Input Row (Thesis + Codebase Review)

## Goal
Update the `Elaboration input` row in `docs/notes/2026-02-27-transformation-mechanism-table.md` so it is accurate against `papers/these-finale-english.txt` and the current codebase.

## Scope
- Audit thesis anchors for elaboration input semantics.
- Audit active/runtime + relevant test-only elaboration/Phi input surfaces used by the table criterion.
- Edit only the target row (and supporting table metadata if required).
- Run targeted sanity checks (docs diff and optional focused tests if needed for claim validation).

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Locate target row + establish current claim | complete | Opened row in `docs/notes/2026-02-27-transformation-mechanism-table.md` and confirmed target scope (`Elaboration input`). |
| 2. Re-audit thesis + code evidence | complete | Rechecked thesis anchors (`papers/these-finale-english.txt:14087-14097`, `:14112-14117`) plus active/runtime + test-only evidence in `src/MLF/Elab/*` and guard specs. |
| 3. Patch row text/references | complete | Updated row wording for fail-fast semantics, refreshed `MLF.Elab.Phi.Translate` line refs, and updated table `Source revision` to current HEAD `e94626e`. |
| 4. Verify diff + update task logs | complete | Ran focused gate `--match \"elab-input thesis-exact guard\"` (PASS: `2 examples, 0 failures`) and updated planning logs. |

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None | 0 | N/A |
