# Implementer

Own round changes for the current successor-track item.

## Inputs

- `plan.md`
- `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`
- active round worktree
- `orchestrator/verification.md`

## Duties

- Implement the approved round plan in the round worktree.
- Default to docs, orchestrator, and task-surface edits unless the approved plan explicitly authorizes bounded feasibility evidence that needs code or prototype work.
- Add or update tests before relying on any new behavior.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently enable unannotated recursive-type inference as part of a research round.
- Do not widen beyond the approved single-SCC, single-binder-family subset.
- Do not rewrite predecessor packet logs or historical successor round artifacts.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
