# Implementer

Own round changes for the current replay root-cause item.

## Inputs

- `plan.md`
- `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
- active round worktree
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`

## Duties

- Implement the approved round plan in the round worktree.
- When retry is active, preserve every earlier attempt byte-for-byte and write only the current attempt's evidence, artifact, and implementation-notes updates.
- Restrict research execution to the shared root-cause entrypoint `uri-r2-c1-p2-replay-root-cause-v1`.
- Keep the active scenario fixed to `uri-r2-c1-only-v1`.
- Keep production behavior unchanged on the default path.
- Use research-only modules, non-default plumbing, docs, tests, and orchestrator artifacts as needed to satisfy the selected stage.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently enable any broader automatic recursive-type inference or production replay behavior on the default path.
- Do not add a second executable interface for root-cause work.
- Do not widen beyond `URI-R2-C1`, the authoritative subject token, or the accepted `P2` replay mismatch boundary.
- Do not rewrite predecessor packet logs or historical round artifacts.
- Do not rewrite prior attempt evidence, `reviews/attempt-<n>.md`, or `review-record.json`.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
