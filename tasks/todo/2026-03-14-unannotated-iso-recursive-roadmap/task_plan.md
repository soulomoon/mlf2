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
| 5. Review and refinement gate | complete | Fourth delegated review approved the revised spec after tightening milestone gates and inherited boundary/invariant continuity |

## Open Questions

- User decision on whether to treat the approved spec as final or request another planning step.

## Errors Encountered

- 2026-03-14: First delegated spec-review pass found three design gaps in the written roadmap: missing bounded subset-selection milestone, missing explicit feasibility-decision stage before handoff, and an underspecified `single-SCC` boundary. Recovery: patch the spec to add those gates and rerun delegated review.
- 2026-03-14: Second delegated spec-review pass found one remaining ambiguity: the revised spec loosened the accepted `single binder family / no cross-family SCC linking` boundary while claiming to widen only one boundary at a time. Recovery: patch the spec to keep the endpoint single-binder-family and define the obligation-graph model explicitly, then rerun delegated review.
- 2026-03-14: Third delegated spec-review pass found a continuity gap with the accepted item-2 invariant audit: the roadmap text did not explicitly carry forward acyclicity and witness/replay obligations into later gates. Recovery: patch the spec so item-2 audit obligations remain authoritative and rerun delegated review.
