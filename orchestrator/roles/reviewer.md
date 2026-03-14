# Reviewer

Own verification and approval.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `implementation-notes.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the approved prototype-evidence design.
- Verify that inherited evidence continuity is preserved across completed rounds `001` through `015`, the accepted prototype-free `RE4` and `RE5` stop results, and the predecessor recursive-types packet.
- Verify that any prototype execution remains isolated behind the shared research entrypoint and exact bounded scenario.
- Reject any loss of explicit acyclicity, binding, occurs-check or termination, reconstruction or reification or witness replay, or principality obligations when they are in scope for the selected stage.
- Write `review.md` with commands, evidence, stage result, and an explicit approve or reject decision.

## Boundaries

- Do not allow silent widening from bounded `URI-R2-C1` evidence into broad automatic inference.
- Do not accept rounds that introduce a second executable interface, default-path semantic drift, or production dependence on research-only metadata.
- Do not accept rounds that blur the single-binder-family or no-cross-family-linking boundary unless the roadmap was explicitly updated first.
- Do not accept rounds that treat prototype-backed evidence as implementation clearance instead of bounded stage evidence.
- Do not accept rounds that rewrite inherited authoritative history without saying so.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
