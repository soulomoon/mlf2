# Merge Preparation (`round-090` / `item-2`)

## Squash Commit Title

`Audit same-lane retained-child persistence breakpoint`

## Summary

- Merge the approved docs-only `item-2` packet for the refreshed same-lane
  retained-child stable-visible-persistence successor control plane.
- The canonical artifact
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
  freezes `round-090` / `item-2` / `attempt-2` as one exact-pocket
  breakpoint-audit round only.
- The accepted payload removes the rejected `attempt-1`
  `PipelineSpec.hs:1693-1698` out-of-pocket credit path, reruns the exact
  frozen `let k ... let u ... in u` packet through `runPipelineElab` and
  `runPipelineElabChecked`, and records that both entrypoints fail on that
  same packet in `Phase 6 (elaboration)` with
  `PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]]`.
- The approved round keeps solver admission as the only row
  `satisfied on current evidence`, localizes
  `elaboration handoff / result state` as the first actual continuity
  breakpoint, and marks reification / reconstruction, internal output,
  public output, and reviewer-visible evidence as
  `not credited after earlier breakpoint`.
- The approved round diff remains bounded to the docs/orchestrator `item-2`
  packet: the canonical audit artifact, `selection.md`, `plan.md`,
  `implementation-notes.md`, `review.md`, `review-record.json`,
  `reviews/attempt-1.md`, `reviews/attempt-2.md`, and this merge note. No
  implementation code, tests, public-surface, executable, Cabal, roadmap,
  retry-contract, verification-contract, bug-tracker, repo-root notes, or
  controller-owned state edit belongs to this merge payload.

## Review And Retry Confirmation

- The latest review snapshot is
  `orchestrator/rounds/round-090/reviews/attempt-2.md`; no later attempt
  snapshot exists, and `review.md` matches that snapshot exactly.
- The latest review snapshot is lawful `accepted + finalize` and records:
  - Implemented stage result: `pass`
  - Attempt verdict: `accepted`
  - Stage action: `finalize`
  - Retry reason: `none`
  - Fix hypothesis: `none`
- The authoritative finalization summary matches `review-record.json`
  exactly:
  - `stage_id: "item-2"`
  - `attempt: 2`
  - `attempt_verdict: "accepted"`
  - `stage_result: "pass"`
  - `stage_action: "finalize"`
  - `retry_reason: "none"`
  - `fix_hypothesis: "none"`
  - `status: "authoritative"`
  - `authoritative_attempt: 2`
  - `authoritative_result: "pass"`
  - `artifact_path: "docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md"`
  - `review_snapshot: "orchestrator/rounds/round-090/reviews/attempt-2.md"`
  - `final_outcome: "same-lane-retained-child-first-breakpoint-localized-to-phase-6-elaboration"`
  - `terminal_reason: "none"`
- `orchestrator/retry-subloop.md` allows roadmap item `2` to finalize as
  `accepted + finalize`, and the active controller state already places this
  round at `active_round_id: "round-090"`, `stage: "merge"`,
  `current_task: "item-2"`, `branch: "codex/round-090"`, and
  `active_round_dir: "orchestrator/rounds/round-090"`.
- No further merger-authored retry work is lawful here: `attempt-2` is the
  authoritative accepted snapshot for this bounded docs-only round, and this
  note does not edit controller-owned retry bookkeeping.

## Predecessor Continuity Note

- This round does not reset authority. Completed rounds `round-001` through
  `round-089` remain authoritative historical evidence for the refreshed
  control plane exactly as `orchestrator/roadmap.md` and
  `orchestrator/retry-subloop.md` describe.
- The accepted `round-081` `N14` decision artifact at
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the predecessor continuity anchor for the exact same-lane
  retained-child packet. It still contributes one bounded continuity packet
  only, not proof of broad automatic recursive inference.
- The accepted `round-086` item-5 reconstruction contract at
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  remains the controlling source for the persistence-tuple vocabulary and the
  solver/elaboration/reconstruction/internal-output/public-output/
  reviewer-visible ledger rows that item `2` must audit honestly.
- The accepted `round-088` item-7 architecture decision at
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  remains the immediate bounded successor authority. It selected
  `continue within the current architecture` and one same-lane
  retained-child stable-visible-persistence gate; this round does not revise
  that decision.
- The accepted `round-089` item-1 case freeze at
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
  remains the immediate frozen-tuple authority. This round preserves the
  same family, `boundVarTargetRoot` anchor, one owner-local retained-child
  frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, and
  clear-boundary status only, while updating continuity evidence only by
  localizing the first exact-pocket break to elaboration.
- The inherited baseline contract from
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains binding: automatic recursive-type inference is still unresolved at
  repo level, recursive meaning remains explicit and iso-recursive, and the
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary remains mandatory unless a later accepted roadmap item explicitly
  changes it.
- This round updates bounded persistence continuity evidence, but only in the
  narrow item-2 way the roadmap allows: it confirms solver admission as the
  only currently satisfied row for the frozen pocket and records the earliest
  exact-pocket blocker at `Phase 6 (elaboration)`. It does not revise the
  frozen tuple, upgrade the pocket to accepted `stable visible persistence`,
  reopen `non-cyclic-graph`, or select the next roadmap item.

## Follow-Up Notes

- Post-merge guider/controller work should treat the canonical breakpoint
  audit, `review.md`, and `review-record.json` as the authoritative item-2
  localization for this bounded successor loop.
- Later work must preserve both the frozen item-1 tuple and the accepted
  item-2 read exactly: solver admission only is currently satisfied on
  exact-pocket evidence, elaboration is the first actual breakpoint, and all
  later rows stay uncredited after that earlier break.
- The exact-pocket `PhiTranslatabilityError` remains bounded blocker debt for
  later roadmap work only. This note prepares squash merge; it does not merge
  the round, edit controller-owned state, or select the next roadmap item.

## Ready For Squash Merge

Yes. The latest review snapshot is `accepted + finalize`, the authoritative
review record matches the finalized review snapshot, the active round is
already at `stage: "merge"` for `item-2`, predecessor authority and the
inherited baseline remain unchanged, and the approved payload stays within
one docs-only `item-2` breakpoint-audit round.
