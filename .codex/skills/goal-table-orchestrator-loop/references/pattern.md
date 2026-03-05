# Common Pattern: Goal Table + Improving Loop

## Artifact Set

1. Mechanism table (`docs/notes/...`)
- Tracks current vs target behavior per mechanism.
- Includes binary gate, evidence, and next action.

2. Orchestrator prompt (`docs/prompts/...`)
- Defines role-separated loop and gate protocol.
- Enforces fixed mechanism order and attempt/round limits.

3. Round execution log (`docs/prompts/...`)
- Captures actual round/attempt outcomes.
- Preserves reasons for `NO` gates and terminal status.

## State Machine

- Sweep: verifier evaluates all mechanisms in order.
- Select: first `NO` mechanism becomes target.
- Iterate: planner -> implementer -> reviewer -> QA -> verifier.
- Advance: continue on all-YES attempt; otherwise replan.
- Stop: `COMPLETED`, `FAILED`, or `MAXIMUMRETRY`.

## Non-negotiable Contracts

- Gate values: exactly `YES` or `NO`.
- One terminal status line only.
- Evidence must cite source-of-truth + code/test anchors.
