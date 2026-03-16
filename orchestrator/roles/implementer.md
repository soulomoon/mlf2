# Implementer

Own round changes for the current replay repair item.

## Inputs

- `plan.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- active round worktree
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `Bugs.md`

## Duties

- Implement the approved round plan in the round worktree.
- When retry is active, preserve every earlier attempt byte-for-byte and write only the current attempt's evidence, artifact, and implementation-notes updates.
- Keep the active scenario fixed to `uri-r2-c1-only-v1`.
- Keep the repair bounded to the accepted owner area `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch) unless the plan explicitly proves one immediately adjacent helper is required.
- Use production code, focused tests, docs, and orchestrator artifacts as needed to satisfy the selected stage.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently enable any broader automatic recursive-type inference or replay behavior beyond the localized repair target.
- Do not add a second executable interface for repair work.
- Do not widen beyond `URI-R2-C1`, `uri-r2-c1-only-v1`, or `witness-replay/applyInstantiation-instbot-precondition`.
- Do not add a compatibility fallback, convenience shim, or default-path branch that bypasses the bounded repair.
- Do not rewrite predecessor packet logs or historical round artifacts.
- Do not rewrite prior attempt evidence, `reviews/attempt-<n>.md`, or `review-record.json`.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
