# Task Plan

Task: URI-R2-C1 Orchestrator Retry-Subloop Contract
Created: 2026-03-16
Status: complete

## Objective
- Amend the live top-level `orchestrator/` contract so future `URI-R2-C1` rounds can retry `P1` through `P3` within the same round instead of forcing single-shot stage finalization.
- Keep the outer state machine intact while adding explicit reviewer-classified retry semantics, machine-readable retry state, and immutable per-attempt review history.
- Preserve completed rounds `round-016` through `round-019` as historical `contract_version: 1` evidence rather than rewriting accepted artifacts.

## Baseline
- Active task folder: `tasks/todo/2026-03-16-uri-r2-c1-retry-subloop-contract/`
- Live control plane: top-level `orchestrator/`
- Recorded base branch at start: `codex/automatic-recursive-type-inference`
- Recorded machine stage at start: `done`
- Recorded last completed round at start: `round-019`

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Audit current control-plane contract and conflicting attempt semantics | complete | Reviewed state, roadmap, verification contract, role prompts, and the prototype-evidence design spec. |
| 2. Write the retry-subloop amendment spec and task packet | complete | Added a forward-only v2 amendment spec plus task-local planning files. |
| 3. Patch live orchestrator docs, prompts, and machine state to v2 | complete | Added the amendment spec, repo-local retry contract, v2 machine state, and prompt/verification updates. |
| 4. Verify docs/state consistency and archive the packet | complete | Ran docs/state verification, synced adjacent docs, recorded the residual global-skill mismatch, and closed the packet for archive. |

## Decisions
| Decision | Rationale |
| --- | --- |
| Treat the retry model as `contract_version: 2` instead of rewriting v1 artifacts | Historical rounds already closed cleanly under the old rules and should remain immutable evidence. |
| Retry only `P1` through `P3` semantically | `P4` is an aggregate decision gate, not the place to spend retry budget on upstream uncertainty. |
| Keep the outer stage order unchanged | The current control plane and runtime skill already assume `select-task -> plan -> implement -> review -> merge -> update-roadmap -> done`; only retry semantics need widening. |

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None so far | 0 | N/A |
