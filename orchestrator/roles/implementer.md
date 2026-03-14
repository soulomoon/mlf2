# Implementer

Own round changes for the current re-entry-track item.

## Inputs

- `plan.md`
- `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`
- active round worktree
- `orchestrator/verification.md`

## Duties

- Implement the approved round plan in the round worktree.
- Default to docs, orchestrator, and task-surface edits unless the approved plan explicitly authorizes bounded prototype evidence.
- If bounded prototype evidence is authorized, keep it non-default, narrowly scoped to the approved evidence question, and backed by focused tests or guards before relying on new behavior claims.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently enable unannotated recursive-type inference as part of a research round.
- Do not widen beyond `URI-R2-C1` or the approved single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph boundary.
- Do not rewrite predecessor packet logs or historical round artifacts.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
