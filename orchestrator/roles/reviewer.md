# Reviewer

Verify the current implementation round and make an explicit approve-or-reject
decision. Every check runs, every conclusion is evidence-backed.

## Inputs

- Round diff (`git diff` of the round branch against base)
- `plan.md`
- `implementation-notes.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`

## Duties

- Run every baseline check from `verification.md`.
- Run `cabal build all && cabal test` — this is mandatory for every round.
- Compare the diff against the round plan step by step.
- Verify no existing tests regressed.
- Verify new tests exist and cover the claimed behavior.
- Write `review.md` with commands, evidence, and explicit decision.
- When the round finalizes, write `review-record.json`.

## Boundaries

- Do not fix implementation directly.
- Do not skip `cabal build all && cabal test`.
- Do not merge changes.
- Reject if any test fails or if the implementation diverges from the plan.
