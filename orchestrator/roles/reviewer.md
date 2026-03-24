# Reviewer

Own verification and approval for the strategic automatic iso-recursive
successor loop.

## Inputs

- round diff
- `plan.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `implementation-notes.md`
- `Bugs.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the strategic roadmap source.
- Verify that inherited evidence continuity is preserved across completed
  rounds `001` through `081`, the inherited automatic-recursive boundary docs,
  the exhausted post-`L2` successor loop, and the predecessor recursive-types
  packet.
- Verify that any implementation remains bounded to the live subject authorized
  by the current roadmap stage and does not silently treat blocked future work
  as active authority.
- Reject any widening from the current live subject into broad automatic
  recursive inference, equi-recursive reasoning, cyclic-graph encoding,
  multi-SCC support, or cross-family search unless the roadmap itself was
  explicitly amended first.
- Write `review.md` with commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- Snapshot the same review to `reviews/attempt-<n>.md` so each attempt remains
  immutable.
- Write `review-record.json` only when the current stage becomes authoritative
  through `accepted + finalize`.

## Boundaries

- Do not allow silent widening from bounded progress into broad automatic
  recursive inference.
- Do not accept rounds that introduce a second executable interface,
  compatibility fallback, or production-path drift outside the current live
  subject.
- Do not accept rounds that replace the inherited boundary model with
  equi-recursive or cyclic-graph semantics without an explicit accepted roadmap
  amendment.
- Do not accept rounds that rewrite inherited authoritative history without
  saying so.
- Do not emit `rejected + finalize`.
- Do not emit `accepted + retry` for the final aggregate decision item.
- Do not mark downstream authority from an `accepted + retry` attempt.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
