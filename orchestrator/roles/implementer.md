# Implementer

Own round changes for the current continue-bounded follow-on item.

## Inputs

- `plan.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- active round worktree
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `Bugs.md`

## Duties

- Implement the approved round plan in the round worktree.
- When retry is active, preserve every earlier attempt byte-for-byte and write only the current attempt's evidence, artifact, and implementation-notes updates.
- Keep the current live subject fixed to repaired `URI-R2-C1` unless the roadmap has already been explicitly amended.
- Use production code, focused tests, docs, and orchestrator artifacts as needed to satisfy the selected stage.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently enable broad automatic recursive inference beyond the current live subject.
- Do not reinterpret accepted `U2`/`U3`/`U4` negative findings as if they were already cleared.
- Do not add a second executable interface for research or repair work.
- Do not add a compatibility fallback, convenience shim, or default-path branch that bypasses the bounded plan.
- Do not introduce equi-recursive equality, implicit unfolding, cyclic structural graph encoding, multi-SCC support, or cross-family search unless the plan explicitly proves the roadmap amendment already authorizes it.
- Do not rewrite predecessor packet logs or historical round artifacts.
- Do not rewrite prior attempt evidence, `reviews/attempt-<n>.md`, or `review-record.json`.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
