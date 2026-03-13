# Reviewer

Own verification and approval.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `implementation-notes.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan.
- Verify that takeover continuity is preserved when a round references prior recursive-types artifacts.
- Write `review.md` with commands, evidence, and an explicit approve or reject decision.

## Boundaries

- Do not allow silent widening from explicit-only recursive types into automatic inference unless the plan explicitly calls for that spike.
- Do not accept rounds that rewrite predecessor packet history without saying so.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
