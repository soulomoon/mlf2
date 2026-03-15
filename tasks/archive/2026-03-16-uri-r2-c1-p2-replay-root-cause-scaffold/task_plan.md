# Task Plan

Task: URI-R2-C1 P2 Replay Root-Cause Orchestrator Scaffold
Created: 2026-03-16
Status: complete

## Objective
- Refresh the live top-level `orchestrator/` so the next lawful run targets a new bounded successor roadmap rooted in the authoritative `P2` replay mismatch rather than the completed prototype-evidence track.
- Preserve completed rounds `round-001` through `round-019` as historical evidence while making the repo clean and ready for the first real `contract_version: 2` retry-capable run.
- Keep the new track research-first: the goal is to decide whether the `P2` replay failure warrants a separate repair track, not to start implementation directly.

## Baseline
- Active task folder: `tasks/todo/2026-03-16-uri-r2-c1-p2-replay-root-cause-scaffold/`
- Live control plane before refresh: completed `URI-R2-C1` prototype-evidence track (`P1` through `P4`)
- Recorded machine state at start: `contract_version: 2`, `stage: done`, `active_round_id: null`, `last_completed_round: round-019`
- Recorded base branch at start: `codex/automatic-recursive-type-inference`

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Confirm the next lawful successor target and anchor evidence | complete | Verified the new track should root in the authoritative `P2` replay mismatch and `P4` hard-stop decision. |
| 2. Write the successor roadmap design and task packet | complete | Added the bounded replay root-cause roadmap design and synced the active task files. |
| 3. Refresh the live control-plane docs/prompts/state for the successor track | complete | Updated the live roadmap, verification contract, retry-subloop doc, role prompts, and adjacent docs. |
| 4. Verify the refreshed control plane and clean the tree | complete | Ran docs/state checks and closed the scaffold packet; final repo baselining happens immediately after. |

## Decisions
| Decision | Rationale |
| --- | --- |
| Use a bounded `P2` replay root-cause track as the successor roadmap | The current finished roadmap already isolated the primary failure to `P2-W` (`partial-replay`), so this is the narrowest truthful next target. |
| Keep the next final decision at `reopen-repair-track | remain-stop` | The successor track should decide whether a separate repair campaign is justified, not skip directly to implementation. |
| Preserve `contract_version: 2` and same-round retries for the first three stages of the new track | The retry contract was added precisely to support stubborn debugging loops like this one. |

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None so far | 0 | N/A |
