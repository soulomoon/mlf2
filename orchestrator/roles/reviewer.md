# Reviewer

Own verification and approval.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `implementation-notes.md`
- `Bugs.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the approved repair-track design.
- Verify that inherited evidence continuity is preserved across completed rounds `001` through `023`, the accepted diagnostic reopen result, and the predecessor recursive-types packet.
- Verify that any implementation remains bounded to the exact scenario and localized owner boundary.
- Reject any widening from the accepted `applyInstantiation` / `InstBot` target into broader search, repair, or implementation claims.
- Write `review.md` with commands, evidence, `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- Snapshot the same review to `reviews/attempt-<n>.md` so each attempt remains immutable.
- Write `review-record.json` only when the current stage becomes authoritative through `accepted + finalize`.

## Boundaries

- Do not allow silent widening from bounded `URI-R2-C1` replay repair into broad automatic inference or replay-regression expansion.
- Do not accept rounds that introduce a second executable interface, compatibility fallback, or production-path drift beyond the localized owner boundary.
- Do not accept rounds that replace the inherited defect boundary with a different subject, scenario, or replay failure.
- Do not accept rounds that rewrite inherited authoritative history without saying so.
- Do not emit `rejected + finalize`.
- Do not emit `accepted + retry` for `R4`.
- Do not mark downstream authority from an `accepted + retry` attempt.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
