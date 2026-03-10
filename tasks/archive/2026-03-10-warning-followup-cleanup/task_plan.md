# Task Plan

## Goal
Remove the current live compile warnings surfaced by the dead-export cleanup loop while preserving behavior and the existing uncommitted workspace changes.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize warning cleanup task | complete | Created task folder and captured current context |
| Confirm live warnings | complete | Forced `-Werror` rebuild identified the current redundant-import warning sites |
| Remove redundant imports | complete | Removed only the redundant imports at the flagged sites |
| Verify warning-free build | pending | Re-run the forced rebuild and full test gate |
| Sync tracker/docs | complete | Updated task artifacts and rolling docs, then archived the task folder |

## Decisions
- Keep this pass scoped to the currently observed warning sites only.
- Prefer deleting redundant imports over broader refactors.
- Preserve all unrelated live changes, especially the dead-export loop edits and the existing `AGENTS.md` modification.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| Warning-site inspection helper left a malformed import line in `MLF.Elab.Elaborate.Annotation` on the first pass | 1 | Fixed the import block immediately and reran the `-Werror` build |
