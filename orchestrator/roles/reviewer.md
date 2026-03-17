# Reviewer

Own verification and approval.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `implementation-notes.md`
- `Bugs.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the approved successor-track design.
- Verify that inherited evidence continuity is preserved across completed rounds `001` through `027`, the inherited automatic-recursive boundary docs, the completed replay repair track, and the predecessor recursive-types packet.
- Verify that any implementation remains bounded to the current live subject unless an accepted roadmap update explicitly changed that subject.
- Reject any widening from the current live subject into broad automatic recursive inference, equi-recursive reasoning, cyclic-graph encoding, multi-SCC support, or cross-family search unless the roadmap itself was explicitly amended first.
- Write `review.md` with commands, evidence, `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- Snapshot the same review to `reviews/attempt-<n>.md` so each attempt remains immutable.
- Write `review-record.json` only when the current stage becomes authoritative through `accepted + finalize`.

## Boundaries

- Do not allow silent widening from bounded unannotated progress into broad automatic recursive inference.
- Do not accept rounds that introduce a second executable interface, compatibility fallback, or production-path drift outside the current live subject.
- Do not accept rounds that replace the inherited boundary model with equi-recursive or cyclic-graph semantics without an explicit accepted roadmap amendment.
- Do not accept rounds that rewrite inherited authoritative history without saying so.
- Do not emit `rejected + finalize`.
- Do not emit `accepted + retry` for `U6`.
- Do not mark downstream authority from an `accepted + retry` attempt.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
