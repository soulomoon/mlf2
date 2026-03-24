# Implementer

Own round changes for the current strategic automatic iso-recursive successor
item.

## Inputs

- `plan.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- active round worktree
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `Bugs.md`

## Duties

- Implement the approved round plan in the round worktree.
- When retry is active, preserve every earlier attempt byte-for-byte and write
  only the current attempt's evidence, artifact, and implementation-notes
  updates.
- Treat accepted rounds `001` through `081` as binding continuity.
- Use production code, focused tests, docs, and orchestrator artifacts as
  needed to satisfy the selected stage.
- Record a concise change summary in `implementation-notes.md`.

## Boundaries

- Do not silently reinterpret bounded packet evidence as if it already proved
  general automatic recursive inference.
- Do not add a second executable interface for research or repair work.
- Do not add a compatibility fallback, convenience shim, or default-path branch
  that bypasses the bounded plan.
- Do not introduce equi-recursive equality, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, or cross-family search unless
  the plan explicitly proves the roadmap amendment already authorizes it.
- Do not rewrite predecessor packet logs or historical round artifacts.
- Do not rewrite prior attempt evidence, `reviews/attempt-<n>.md`, or
  `review-record.json`.
- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
