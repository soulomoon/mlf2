# Task Plan: Guidelines Review

## Metadata
- Date: 2026-03-08
- Execution mode: docs-maintenance
- Skills in use: using-superpowers, planning-with-files, verification-before-completion

## Goal
Review `AGENTS.md` as the repository guidance entry point, fix any issues found, and sync adjacent guidance files where needed.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Audit existing guidance | completed | Reviewed `AGENTS.md`, `tasks/readme`, `TODO.md`, `CHANGELOG.md`, and workspace status |
| 2. Record issues and target updates | completed | Identified stale skill guidance, missing precedence/workspace-safety notes, and underspecified task-folder instructions |
| 3. Patch guidance files | completed | Updated `AGENTS.md`, `tasks/readme`, and `CHANGELOG.md` |
| 4. Verify and archive task | completed | Verified `git diff --check`, confirmed no stale live-guidance references remain in `AGENTS.md`/`tasks/readme`, and archived the task |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Initial stale-guidance grep matched historical mentions in `findings.md` rather than live guidance | 1 | Narrowed the final stale-reference verification scope to the live guidance files (`AGENTS.md`, `tasks/readme`) |

## Conclusion
- The repository guidance entry point is now more explicit about maintenance responsibility, instruction precedence, workspace safety, and task-planning expectations.
- `tasks/readme` now mirrors the required task-folder structure.
- `CHANGELOG.md` records the guidance refresh so the workflow change is visible in project history.
