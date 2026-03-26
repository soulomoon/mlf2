# Round 117 Controller Recovery Note

Date: 2026-03-27
Round: `round-117`
Controller stage before recovery: `review`
Retry attempt: `attempt-2`

## Observed Issue

The retry-delta `plan.md` for `attempt-2` froze
`orchestrator/rounds/round-117/review.md` as an immutable prior-attempt
surface, but the repo-local reviewer contract still requires the reviewer to
write `review.md` for the current review attempt.

## Recovery Investigation Result

The repo-local `recovery-investigator` concluded that the controller cannot
safely continue into `review` as-is. The same round must return to `plan`
first for a narrow artifact-ownership repair:

- remove `review.md` from the immutable prior-attempt set;
- keep `reviews/attempt-1.md` and the other true prior-attempt surfaces
  frozen; and
- leave `review.md` available for the attempt-2 reviewer to write.

## Controller Action

The controller returned `round-117` to `plan` without incrementing the retry
attempt number. No reviewer was launched for `attempt-2` while the
contract-inconsistent plan remained active.
