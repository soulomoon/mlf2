# Task Plan

## Goal
Execute the dead-export shortlist in a serial agent-team loop, landing each dead export one by one with red→green verification and synchronized tracking artifacts.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize loop artifacts | in_progress | Create the plan/task/orchestrator files and seed the mechanism queue |
| Row 1 — ChiQuery cleanup | complete | Dead exports removed; guard, chi-first suite, and full gate passed |
| Row 2 — Binding.Validation cleanup | complete | Export removed; binding suite and full gate passed |
| Rows 3-5 — Revalidate and resolve | complete | Rows 3-5 all resolved YES via revalidation + targeted red→green rounds |
| Final verification + archive | complete | Final full gate passed, terminal status recorded, and task folder archived |

## Decisions
- Use the current workspace rather than a new worktree because the user explicitly asked to preserve the live uncommitted state.
- Keep one active mechanism only; no overlapping implementers on different rows.
- Treat rows 3-5 as verifier-owned revalidation rounds before any code mutation.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| Full commit hashes from archived notes were not present locally (`fatal: bad object`) during history review. | 1 | Switched to `git log --all -S...`, `git blame`, and short local commit hashes (`0ffa2ab`, `90176dc`) to recover the relevant `ChiQuery` cleanup history without repeating the failing lookup. |
