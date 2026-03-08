# Task Plan — Bugs + Archive Drift Audit

## Goal
Audit `Bugs.md` and task-folder references across the repo for stale or broken paths after the recent TODO/task cleanup, and fix any doc drift found.

## Scope
- Check `Bugs.md` for stale statuses or task-folder references.
- Check repo docs for broken `tasks/archive/` / `tasks/todo/` references.
- Apply doc-only cleanup where appropriate.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize drift audit | complete | Tracker created and clean baseline captured |
| Scan Bugs and task refs | complete | Broken concrete refs identified across live docs and recent archives |
| Fix any doc drift | complete | Patched live docs and recent archive self-references |
| Verify repo references | complete | Live docs re-scan now clear except intentional template/example patterns |
| Summarize findings | complete | Audit findings recorded and tracker ready for archive |

## Decisions
- Restrict this pass to doc/tracker consistency; do not broaden into unrelated bugfix work.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None yet | - | - |

## Outcome
- No `Bugs.md` task-path drift was found.
- Broken concrete task-folder refs in live docs were repaired.
- This audit is complete and ready for archive.
