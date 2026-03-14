# Task Plan

Task: New roadmap for staged progress toward unannotated iso-recursive type inference
Created: 2026-03-14
Status: IN PROGRESS

## Objective

- Design a new post-orchestrator roadmap that starts from the accepted `ARI-C1` implementation-handoff and stages toward true unannotated iso-recursive type inference.
- Keep the design thesis-aware, explicit about boundaries, separate from the completed successor orchestrator roadmap, and end at a research-backed implementation handoff rather than an implementation attempt.
- Limit the final handoff target to `single-SCC` unannotated iso-recursive inference, not multi-SCC.

## Current Context

- The existing top-level `orchestrator/` roadmap is complete.
- The accepted endpoint is a bounded `ARI-C1` implementation-handoff, not general automatic recursive-type inference.
- The user selected a staged follow-on track rather than an immediate end-to-end unannotated-inference target.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| 1. Context refresh | complete | Reviewed completed successor-orchestrator artifacts and current inference boundary docs |
| 2. Brainstorming and scope clarification | complete | Confirmed staged research-only track, conservative ladder shape, three bridge stages, and single-SCC endpoint |
| 3. Draft design / roadmap proposal | complete | Presented and validated roadmap skeleton, fixed boundaries, milestone breakdown, and fail-closed validation model |
| 4. Write design doc | complete | Saved approved roadmap design to `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md` |
| 5. User review gate | pending | Wait for user approval before any implementation-planning step |

## Open Questions

- None currently. Waiting for user review of the written design spec.

## Errors Encountered

- None yet.
