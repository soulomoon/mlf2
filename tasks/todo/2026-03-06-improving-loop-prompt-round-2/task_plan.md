# Task Plan

## Goal
Create round-2 prompt artifacts that preserve the improving-loop workflow while adding a mandatory two-researcher handoff before Planner work begins, increasing the maximum implementation attempts per round from 6 to 10, and hardening the retry/planning workflow after review.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Read source docs and confirm requested deltas | completed | Read the round-1 plan and current prompt. |
| 2. Draft round-2 plan and prompt artifacts | completed | Wrote the new plan and prompt files. |
| 3. Verify written artifacts match requested behavior | completed | Checked researcher handoff and 10-attempt references; fixed stale run-folder naming. |
| 4. Apply review-driven workflow hardening | completed | Added contradiction resolution, retry discipline, scope expansion, blocked mode, failed-attempt hygiene, and standalone row-14 mapping. |

## Decisions
- Keep the existing orchestrator shape and mechanism ordering intact.
- Add two pre-planner researcher roles with complementary scopes so the Planner receives structured summaries before producing an implementation plan.
- Change every implementation-attempt limit reference from 6 to 10 to avoid internal contradictions.
- Keep the no-timeout research handoff; improve correctness through stronger planner/retry contracts instead of bounded waiting.

## Errors Encountered
- None so far.
