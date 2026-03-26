# Task Plan

Task: Update the shared `run-orchestrator-loop` skill so delegated-stage
stop situations must attempt `recovery-investigator` before the controller
can lawfully stop.
Created: 2026-03-26
Status: complete

## Objective

- Tighten the shared runtime-skill contract around recovery and stopping.
- Preserve the existing distinction between terminal completion, explicit
  user interruption, deterministic controller blockage, and delegated-stage
  failure.
- Make the delegated-stage failure path require a documented
  `recovery-investigator` attempt, or a deterministic recorded reason why no
  qualifying launch was possible.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| 1. Capture the baseline failure case and current skill wording | complete | Use the observed `round-099` stop and the current skill/reference docs as the RED baseline. |
| 2. Edit the shared skill and references to close the recovery-stop loophole | complete | Updated the main skill plus the recovery-related reference docs. |
| 3. Review the docs diff for consistency and summarize the contract change | complete | Confirmed the diff is coherent across the skill and references. |

## Decisions

| Decision | Rationale |
| --- | --- |
| Treat the recent `round-099` guider failure as the baseline pressure scenario | It is a concrete example of the loophole the user wants closed. |
| Update both `SKILL.md` and the recovery-related references | The main skill and its detailed references must stay aligned. |

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| None yet | 0 | None yet |
