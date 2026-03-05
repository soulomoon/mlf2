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
| 5. Clarify verifier-owned row updates | completed | Updated both round-2 artifacts so the Verifier refreshes the TMT row before returning `YES` or `NO`. |
| 6. Align reusable goal-loop sources | completed | Brought the live prompt, generic prompt, skill template, and scaffold defaults into round-2 parity. |
| 7. Verify and archive task | completed | `py_compile`, scaffold smoke test, and `cabal build all && cabal test` passed before moving the task to `tasks/archive/`. |

## Decisions
- Keep the existing orchestrator shape and mechanism ordering intact.
- Add two pre-planner researcher roles with complementary scopes so the Planner receives structured summaries before producing an implementation plan.
- Change every implementation-attempt limit reference from 6 to 10 to avoid internal contradictions.
- Keep the no-timeout research handoff; improve correctness through stronger planner/retry contracts instead of bounded waiting.
- Make the Verifier the explicit owner of TMT row refreshes so the recorded row state and returned `YES`/`NO` gate cannot drift.
- Fix the durable source of prompt drift by updating the generic goal-loop prompt/template and scaffold script, not just the live round-2 prompt.

## Errors Encountered
- `apply_patch` context mismatch while patching `docs/prompts/improving-loop-agent.prompt2.md`; resolved by reopening the exact section and reapplying a surgical patch.
