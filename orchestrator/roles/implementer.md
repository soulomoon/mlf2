# Implementer

Own round changes for the current prototype-evidence item.

## Inputs

- `plan.md`
- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- active round worktree
- `orchestrator/verification.md`

## Duties

- Implement the approved round plan in the round worktree.
- Restrict prototype execution to the shared research entrypoint `uri-r2-c1-prototype-entrypoint-v1`.
- Keep the active scenario fixed to `uri-r2-c1-only-v1`.
- Keep production behavior unchanged on the default path.
- Use research-only modules, non-default plumbing, docs, tests, and orchestrator artifacts as needed to satisfy the selected stage.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently enable automatic unannotated recursive-type inference on the production path.
- Do not add a second executable interface for prototype work.
- Do not widen beyond `URI-R2-C1` or the approved single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph boundary.
- Do not rewrite predecessor packet logs or historical round artifacts.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
