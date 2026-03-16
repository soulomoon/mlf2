# Task Plan

Task: URI-R2-C1 P2 Replay Repair-Track Orchestrator Scaffold
Created: 2026-03-17
Status: completed

## Objective
- Refresh the live top-level `orchestrator/` so it succeeds the completed `D1` through `D4` diagnostic track with a new bounded repair-track roadmap for the localized `applyInstantiation` / `InstBot` defect.
- Keep the new control plane locked to `URI-R2-C1`, `uri-r2-c1-only-v1`, and `witness-replay/applyInstantiation-instbot-precondition`.
- Stop after the scaffold checkpoint commit; do not start runtime rounds in this task.

## Baseline
- Live control plane before scaffold: top-level `orchestrator/`
- Recorded base branch: `codex/automatic-recursive-type-inference`
- Recorded machine stage at start: `done`
- Recorded last completed round at start: `round-023`
- Controlling predecessor result at start: `D4 = reopen-repair-track`

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Survey existing control plane and predecessor evidence | complete | Read the live roadmap, state, verification contract, role prompts, retry contract, and adjacent repo notes before changing the scaffold. |
| 2. Write repair-track design and task packet | complete | Added a new repair-track design spec plus this task folder for persistent execution notes. |
| 3. Refresh the live orchestrator contract | complete | Replaced the live roadmap, state, verification contract, retry-subloop references, and role prompts with the bounded repair-track successor scaffold. |
| 4. Sync adjacent guidance and checkpoint the scaffold | complete | Updated repo notes, prepared the scaffold checkpoint commit, and stopped without starting any runtime round. |

## Decisions
| Decision | Rationale |
| --- | --- |
| Keep the repair track strictly single-scenario | The accepted diagnostic result only justifies a bounded reopen at the localized `applyInstantiation` / `InstBot` boundary, not a broader replay or regression campaign. |
| Refresh the existing top-level `orchestrator/` in place | Historical round directories and retry semantics already exist; replacing the live control plane in place preserves predecessor evidence while keeping the runtime entrypoint stable. |
| Preserve `contract_version: 2` retry semantics | The repair track still benefits from same-round bounded retries for early stages while keeping the final gate terminal. |

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None | 0 | N/A |
