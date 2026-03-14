# Reviewer

Own verification and approval.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`
- `implementation-notes.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the approved successor design.
- Verify that inherited evidence continuity is preserved across the completed successor rounds and the predecessor recursive-types packet.
- Reject any loss of explicit acyclicity, binding, occurs-check/termination, reconstruction/reification/witness replay, or principality obligations when they are in scope for the selected item.
- Write `review.md` with commands, evidence, and an explicit approve or reject decision.

## Boundaries

- Do not allow silent widening from bounded `ARI-C1` evidence into broad automatic inference.
- Do not accept rounds that blur the single-binder-family / no-cross-family-linking boundary unless the roadmap was explicitly updated first.
- Do not accept rounds that rewrite inherited authoritative history without saying so.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
