# Task Plan — 2026-03-04 Update Elaboration Input Row (Live Re-audit)

## Goal
Update the `Elaboration input` row in `docs/notes/2026-02-27-transformation-mechanism-table.md` so it reflects the thesis (`papers/these-finale-english.txt`) and the current implementation.

## Scope
- Audit only the `Elaboration input` row.
- Verify claims against thesis sections on elaboration input (`χp`, propagation witness choice, `T(e)`).
- Verify implementation entrypoints in runtime/test paths.
- Update row text, references, and thesis-exact classification if evidence requires.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Gather thesis/code evidence | complete | Collected thesis refs (`Def. 15.3.12`, `§15.3.6`) and current runtime/test code refs. |
| 2. Decide row classification/content | complete | Under strict criterion (includes test-only paths), row remains `Thesis-exact = No` due to solved-typed `MLF.Elab.Phi.TestOnly` helpers still used in tests. |
| 3. Edit table row + metadata | complete | Updated `Elaboration input` row wording/references and refreshed `Source revision` to current HEAD. |
| 4. Validate and log outcomes | complete | Verified diff scope targets only the requested table doc plus task-log artifacts. |

## Files Modified
- `docs/notes/2026-02-27-transformation-mechanism-table.md`
- `tasks/todo/2026-03-04-update-elaboration-input-row-live-reaudit/task_plan.md`
- `tasks/todo/2026-03-04-update-elaboration-input-row-live-reaudit/findings.md`
- `tasks/todo/2026-03-04-update-elaboration-input-row-live-reaudit/progress.md`

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None | 0 | N/A |
