# Task Plan

Task: Shared `run-orchestrator-loop` Skill v2 Retry Alignment
Created: 2026-03-16
Status: complete

## Objective
- Update the shared `run-orchestrator-loop` skill and its controller reference docs so they match the repo-local `contract_version: 2` retry-subloop semantics.
- Keep the runtime skill generic enough to remain reusable, while teaching it how to respect repo-local retry state and accepted-but-retryable review outcomes.
- Preserve the controller role boundary: this change is instructional, not a license for the controller to do delegated stage work locally.

## Baseline
- Active task folder: `tasks/todo/2026-03-16-run-orchestrator-loop-skill-v2/`
- Shared skill path: `/Users/ares/.codex/skills/run-orchestrator-loop/`
- Repo-local source of truth for the new behavior:
  - `orchestrator/state.json`
  - `orchestrator/retry-subloop.md`
  - `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Audit the shared skill and reference docs for stale single-shot controller rules | complete | Confirmed the skill only models `review rejection -> plan` and knows nothing about repo-local retry state. |
| 2. Patch the shared skill docs to respect repo-local retry contracts | complete | Updated the skill overview, controller rules, state machine, resume rules, delegation boundaries, and worktree/merge notes. |
| 3. Verify internal consistency and record remaining limits | complete | Re-read the edited skill files, confirmed the new retry vocabulary, and recorded that no fresh delegated round was run. |

## Decisions
| Decision | Rationale |
| --- | --- |
| Keep the shared skill generic and repo-driven | The shared skill should teach the controller to read repo-local retry docs/state instead of hard-coding one repo’s exact filenames beyond the standard `orchestrator/` contract. |
| Model `accepted + retry` explicitly in the controller references | That is the key legal transition added by the repo-local v2 amendment; leaving it out would keep the controller stale. |

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None so far | 0 | N/A |
