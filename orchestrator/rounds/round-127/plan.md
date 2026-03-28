# Round 127 Plan (`item-4` `P1` Successor Gate)

## Objective

Convert the accepted bounded `P1` settlement read into exactly one current
outcome and exactly one immediate handoff.

This round is `attempt-1` with `retry: null`.

## Inputs

- `orchestrator/rounds/round-127/selection.md`
- `docs/plans/2026-03-28-p1-local-recursive-shape-successor-authority-success-bar-and-writable-slice-freeze.md`
- `docs/plans/2026-03-28-post-implementation-p1-local-recursive-shape-settlement-surface-and-exact-repo-impact-read.md`
- `orchestrator/rounds/round-125/review-record.json`
- `orchestrator/rounds/round-126/review-record.json`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`

## Round Type

This is an aggregate docs-only decision gate.
No production or test behavior may change here.

## Task List

1. Select exactly one current outcome.
   - Evaluate whether the accepted record settles the exact frozen packet
     within the current architecture, leaves more bounded `P1` work live, or
     forces a boundary-revision reopen.
   - Reject the non-selected outcomes explicitly.

2. Select exactly one immediate handoff.
   - If the exact packet is settled, choose whether the family stops or opens
     the next bounded current-architecture family.
   - If the next bounded family is opened, name it explicitly.

3. Keep the claim non-widening.
   - Do not convert one exact-packet result into general `P1` family success.
   - Do not claim repo-level readiness.
   - Do not reopen architecture boundaries unless the exact evidence requires
     it.

4. Record the docs-only verification posture.
   - Run controller/docs hygiene checks only.
   - Explicitly skip the full Cabal gate because no code or test file is
     changed in this round.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md && test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md && test -f docs/plans/2026-03-28-post-c1-p2-successor-gate-and-immediate-handoff-decision.md`
- `git diff --check`

## Exit Criteria

This round is complete only when:

- one canonical gate artifact records exactly one item-4 outcome;
- the same artifact records exactly one immediate handoff;
- the rejected alternatives are explained;
- the claim remains exact-packet and non-widening; and
- the round is ready for aggregate review.
