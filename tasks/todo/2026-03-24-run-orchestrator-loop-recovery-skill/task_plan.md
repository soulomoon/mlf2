# Task Plan

Task: Design and update the shared `run-orchestrator-loop` skill to recover from incidental delegation failures
Created: 2026-03-24
Status: complete

## Objective

- Capture the approved resilience redesign for the shared
  `run-orchestrator-loop` skill.
- Base the design on the observed `round-078` `select-task` failure where the
  builtin guider handoff produced no observable `selection.md`.
- Keep the change scoped to the shared skill and its references, not repo-local
  orchestrator contracts.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| 1. Load applicable process skills and current skill context | complete | Loaded `brainstorming`, `writing-skills`, `test-driven-development`, the current shared skill, and its references. |
| 2. Validate the failure case and design direction with the user | complete | The user approved broad controller discretion with a dedicated recovery-investigator subagent, scoped to `run-orchestrator-loop`. |
| 3. Write the design spec and supporting task packet | complete | The spec was written, reviewed, user-approved, and committed. |
| 4. Write and review the implementation plan | complete | The plan was revised through three review passes and approved for implementation. |
| 5. Execute and review the completed shared-skill implementation | complete | Implemented in `/Users/ares/.config/superpowers/worktrees/orchestratorpattern/run-orchestrator-loop-recovery` on branch `codex/run-orchestrator-loop-recovery`; task-level reviews and the final whole-implementation review now pass at `3ddeb19646a9c7bcbf8314e881606bb6da6c9d32`. |

## Decisions

| Decision | Rationale |
| --- | --- |
| Keep the redesign in the shared skill only | The user explicitly chose `run-orchestrator-loop`, not repo-local contract changes. |
| Use the observed `round-078` blockage as the RED case | It is a concrete, recent failure that exposed the missing recovery contract. |
| Prefer broad controller recovery discretion with a dedicated investigator role | The user wants the orchestrator to do whatever is needed to finish the loop, without turning the controller into the blocked stage role. |
| Keep implementation changes limited to the shared skill package under `/Users/ares/src/orchestratorpattern/skills/public/run-orchestrator-loop/` | The approved spec names that package as the only implementation scope. |
| Execute implementation in an isolated worktree instead of the dirty source checkout | The source repo already had unrelated edits, so the shared-skill change needed a clean reviewable branch/worktree. |

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| First spec-review pass found missing ownership/availability details for `recovery-investigator` | 1 | Revised the spec to make the role shared-skill-owned, define the source of truth, define mechanism exhaustion, and add the no-recovery-subagent direct-blockage exception. |
| First plan-review pass found missing `recovery-investigator` inputs/outputs, missing mechanism-switch/exhaustion behavior, and weak resume-invariant verification | 1 | Revised the plan to include the missing contract clauses and strengthened verification checks before rerunning plan review. |
| Second plan-review pass found missing `current_task`/retry-attempt freeze semantics and the explicit observability re-check gate | 1 | Revised the plan to include both requirements and aligned verification with the full spec acceptance criteria before the final review pass. |
| Third plan-review pass found that `recovery-investigator.md` still did not explicitly forbid substantive stage work or acting as the stage reviewer | 1 | Revised the plan to add that boundary and the related verification checks, then surfaced the updated plan directly instead of exceeding the review-loop cap. |
| Task 2 code-quality review found the overview stop condition could still short-circuit recovery before attempting the dedicated investigator path | 1 | Narrowed the overview wording in `SKILL.md` so only failure to launch a qualifying `recovery-investigator` can trigger direct blockage. |
| Task 3 code-quality review found `delegation-boundaries.md` could be read as letting the controller perform recovery investigation directly | 1 | Reworded the boundary so diagnosis stays delegated to the shared-skill-owned `recovery-investigator`, while repair actions remain controller-owned. |
| Task 4 code-quality review found the direct-blockage rule was not fully aligned across `SKILL.md`, `delegation-boundaries.md`, and `resume-rules.md` | 1 | Updated the reference docs so every direct-blockage path requires that no qualifying `recovery-investigator` can launch through any available delegation mechanism. |
| Final whole-implementation review found two residual risks: some failure paths did not explicitly persist blockage before stopping, and `recovery-investigator.md` still left controller-owned writes/repairs under-specified | 1 | Updated all affected docs so precise blockage is recorded in `orchestrator/state.json` before user-facing stop behavior, and made `recovery-investigator` explicitly diagnosis/recommendation-only with controller-owned writes/repairs forbidden. |
