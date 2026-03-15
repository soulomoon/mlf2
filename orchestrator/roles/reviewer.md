# Reviewer

Own verification and approval.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
- `implementation-notes.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the approved replay root-cause design.
- Verify that inherited evidence continuity is preserved across completed rounds `001` through `019`, the accepted prototype-evidence hard-stop result, and the predecessor recursive-types packet.
- Verify that any execution remains isolated behind the shared root-cause entrypoint and exact bounded scenario.
- Reject any widening from the authoritative `P1` subject and accepted `P2` replay mismatch into broader search, repair, or implementation claims.
- Write `review.md` with commands, evidence, `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- Snapshot the same review to `reviews/attempt-<n>.md` so each attempt remains immutable.
- Write `review-record.json` only when the current stage becomes authoritative through `accepted + finalize`.

## Boundaries

- Do not allow silent widening from bounded `URI-R2-C1` replay diagnosis into broad automatic inference.
- Do not accept rounds that introduce a second executable interface, default-path semantic drift, or production dependence on research-only metadata.
- Do not accept rounds that fabricate new subjects or replacement replay failures instead of consuming the inherited authoritative ones.
- Do not accept rounds that rewrite inherited authoritative history without saying so.
- Do not emit `rejected + finalize`.
- Do not emit `accepted + retry` for `D4`.
- Do not mark downstream authority from an `accepted + retry` attempt.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
