# Merge Preparation (`round-192` / `item-6`)

## Roadmap Identity

- `roadmap_id`: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`
- `roadmap_item_id`: `item-6`

## Squash Commit Title

`Document bounded negative-family and termination-pressure aggregate classification`

## Squash Summary

- Merge the approved docs-only bounded aggregate `item-6` artifact for the
  active general automatic iso-recursive full-inference roadmap.
- The canonical payload is
  `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`,
  with round-local notes in `orchestrator/rounds/round-192/`.
- The approved artifact fixes the admissible evidence ledger to the already
  authoritative representative `N1` / `N2` / `N6` rows only: the
  clear-boundary control plus reject-side nested-`forall` contrast in
  `test/Research/P5ClearBoundarySpec.hs`, the fail-closed local-vs-non-local
  contrasts and bounded same-lane checks in `test/PipelineSpec.hs`, and the
  read-only route / guard anchors in
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.
- The approved aggregate classifies `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, and `N6 termination-pressure` as
  `fail-closed rejection`, recording that the representative ambiguity,
  quantified-boundary, and bounded-termination pressure rows remain reject-side
  and bounded under the unchanged current architecture.
- The approved scope stays aggregate-only and current-architecture-only: it
  keeps the inherited explicit-only / iso-recursive / non-equi-recursive /
  `non-cyclic-graph = unknown` / no-fallback boundary, does not move into
  `item-7`, and does not make any repo-level readiness claim.
- The squash scope must stay honest and bounded to the one docs artifact plus
  round-local notes only. No `src/`, `src-public/`, `app/`, `test/`,
  `mlf2.cabal`, roadmap, or controller-state change belongs to the squash
  substance.
- The tracked `orchestrator/state.json` edit remains controller-owned
  bookkeeping only and stays out of merge substance for this round.

## Predecessor Continuity

- This round updates the bounded item-6 negative-family / termination-pressure
  aggregate evidence and classification record only; it does not rewrite the
  inherited settlement contracts, the March strategic decision gate, any
  post-settlement implementation or hardening record, or any final
  repo-level capability-claim record.
- The controlling predecessor records remain unchanged and authoritative:
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`,
  and
  `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`.
- Accepted `round-179` and `round-180` remain the fixed authority for the
  named item-3 route / guard cluster and the item-4 authoritative surfaces,
  and accepted `round-191` remains the fixed predecessor aggregate that left
  this item-6 negative-family read unresolved.
- The inherited strategic posture therefore stays unchanged:
  `continue within the current architecture` remains the strongest accepted
  global read, `non-cyclic-graph` remains unresolved, and this merge does not
  claim any item-7 architecture settlement or final repo-level readiness
  result.

## Review Confirmation

- `orchestrator/rounds/round-192/review.md` records `Implemented stage result:
  accepted`, `Attempt verdict: accepted`, `Stage action: finalize`, and
  `Reviewer decision: APPROVED`.
- `orchestrator/rounds/round-192/review-record.json` matches the approved
  round identity and records `decision: approved`,
  `attempt_verdict: accepted`, and `stage_action: finalize`.
- The latest review snapshot is
  `orchestrator/rounds/round-192/reviews/attempt-1.md`, it is
  `accepted + finalize`, and it matches `review.md`.

## Merge Readiness

- Merge readiness: confirmed for the approved docs-only bounded aggregate
  payload, provided the squash stays limited to the canonical docs artifact
  plus round-local notes and still excludes `orchestrator/state.json` and any
  roadmap changes.
- Base branch freshness: exact. `HEAD`,
  `orchestrator/round-192-negative-family-campaign`, and
  `codex/automatic-recursive-type-inference` all resolve to
  `ae64788085541fe7b3e8958647e297691999145e` (`ae64788` short,
  `Advance full-inference roadmap after round-191`), and
  `git rev-list --left-right --count
  codex/automatic-recursive-type-inference...HEAD` reports `0 0`.
- The approved payload is therefore the current round worktree patch atop the
  fresh base, not a stale committed branch that needs replay.
- Round `round-192` is ready for squash merge.

## Follow-Up Notes

- Post-merge controller bookkeeping should preserve the roadmap identity above
  unchanged and treat the aggregate classification artifact plus the
  round-local review notes as the authoritative completed outcome for this
  bounded `item-6` aggregate slice.
- Keep later summaries honest about the exact aggregate conclusion reached
  here: `N1`, `N2`, and `N6` stay `fail-closed rejection`;
  `non-cyclic-graph`, `item-7`, and repo-level readiness remain unresolved
  and out of scope for this merge.
