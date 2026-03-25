# Merge Preparation (`round-091` / `item-3`)

## Squash Commit Title

`Clear same-lane retained-child Phase 6 elaboration breakpoint`

## Summary

- Merge the approved bounded `item-3` packet for the refreshed same-lane
  retained-child stable-visible-persistence successor control plane.
- The canonical artifact
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`
  freezes `round-091` / `item-3` / `attempt-1` / `retry: null` as one exact
  Phase-6-elaboration-resolution round only.
- The accepted payload clears the exact item-2 `Phase 6 (elaboration)`
  breakpoint for the frozen packet
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`
  by adding the no-fallback helper
  `expInstantiateArgsToInstNoFallback` in `src/MLF/Elab/Legacy.hs`, using it
  only from `reifyInst` in `src/MLF/Elab/Elaborate/Annotation.hs` for the
  exact `ExpInstantiate` edge-`3` authority path, and adding one exact-edge
  regression plus one exact-packet pipeline regression in
  `test/ElaborationSpec.hs` and `test/PipelineSpec.hs`.
- The approved round keeps the inherited
  `explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary unchanged, leaves controller-owned top-level drift untouched, and
  does not widen into item `4` or item `5`, the alias-bound family,
  neighboring routes, nested-`forall`, replay / `InstBot`, fallback-like
  recovery, or an architecture revision.
- The approved diff remains bounded to the canonical item-3 artifact,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`,
  `selection.md`, `plan.md`, `implementation-notes.md`, `review.md`,
  `review-record.json`, `reviews/attempt-1.md`, and this merge note. No
  other `src/`, `src-public/`, `app/`, `mlf2.cabal`, bug-tracker,
  roadmap-bundle, retry-contract, verification-contract, or
  controller-state edit belongs to this merge payload.

## Review And Retry Confirmation

- The latest review snapshot is
  `orchestrator/rounds/round-091/reviews/attempt-1.md`; it is the only
  snapshot present under `orchestrator/rounds/round-091/reviews/`, and
  `review.md` matches that snapshot exactly.
- The latest review snapshot is lawful `accepted + finalize` and records:
  - Implemented stage result: `pass`
  - Attempt verdict: `accepted`
  - Stage action: `finalize`
  - Retry reason: `none`
  - Fix hypothesis: `none`
- The authoritative finalization summary matches `review-record.json`
  exactly:
  - `stage_id: "item-3"`
  - `attempt: 1`
  - `attempt_verdict: "accepted"`
  - `stage_result: "pass"`
  - `stage_action: "finalize"`
  - `retry_reason: "none"`
  - `fix_hypothesis: "none"`
  - `status: "authoritative"`
  - `authoritative_attempt: 1`
  - `authoritative_result: "pass"`
  - `artifact_path: "docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md"`
  - `review_snapshot: "orchestrator/rounds/round-091/reviews/attempt-1.md"`
  - `final_outcome: "same-lane-retained-child-exact-phase-6-elaboration-breakpoint-cleared"`
  - `terminal_reason: "none"`
- The active retry contract at
  `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/retry-subloop.md`
  allows roadmap item `3` to finalize via `accepted + finalize`, and the
  controller state already places this round at
  `active_round_id: "round-091"`, `stage: "merge"`,
  `current_task: "item-3"`, `branch: "codex/round-091"`,
  `active_round_dir: "orchestrator/rounds/round-091"`, and `retry: null`.
- No same-round retry remains open: `attempt-1` is authoritative and no
  further merger-authored retry work is lawful under the current review
  result.

## Predecessor Continuity Note

- This round does not reset authority. Completed rounds `round-001` through
  `round-090` remain authoritative historical evidence for the refreshed
  control plane exactly as the active roadmap bundle and retry contract
  describe.
- The accepted `round-081` `N14` decision artifact at
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the predecessor continuity anchor for the exact same-lane
  retained-child packet. It still contributes one bounded continuity packet
  only, not proof of broad automatic recursive inference.
- The accepted `round-088` item-7 architecture decision at
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  remains the immediate bounded successor authority. It selected
  `continue within the current architecture` and one same-lane
  retained-child stable-visible-persistence gate; this round clears the
  exact elaboration blocker inside that gate but does not revise the bounded
  successor decision.
- The accepted `round-089` item-1 freeze at
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
  remains the frozen-tuple authority. This round preserves the same family,
  `boundVarTargetRoot` anchor, one owner-local retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, and
  clear-boundary status only.
- The accepted `round-090` item-2 breakpoint audit at
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
  remains the immediate continuity handoff. This round updates bounded
  persistence continuity evidence only by clearing the exact recorded
  `Phase 6 (elaboration)` blocker for the same frozen pocket; it does not
  yet classify end-to-end persistence.
- The accepted `round-086` item-5 reconstruction contract at
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  remains the controlling source for the persistence-tuple vocabulary and
  solver / elaboration / reconstruction / internal-output / public-output /
  reviewer-visible ledger rows. This round improves the elaboration row for
  the frozen pocket but does not upgrade the pocket to accepted
  `stable visible persistence`.
- The inherited baseline contract from
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains binding: automatic recursive-type inference is still unresolved at
  repo level, recursive meaning remains explicit and iso-recursive, and the
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary remains mandatory unless a later accepted roadmap item explicitly
  changes it.
- This round updates bounded persistence authority and continuity evidence,
  but only in the narrow item-3 way the roadmap allows: the exact edge-`3`
  Phase-6 breakpoint is cleared for the frozen same-lane retained-child
  pocket, while item `4` persistence classification and item `5` successor
  decision work remain later stages only.

## Follow-Up Notes

- Post-merge guider/controller work should treat the canonical item-3
  elaboration-resolution artifact, `review.md`, and `review-record.json` as
  the authoritative item-3 record for this bounded successor loop.
- Later work must preserve the frozen item-1 tuple and the accepted item-2
  plus item-3 reads exactly: the same family, anchor, owner-local frame,
  route, and clear-boundary status remain fixed; the exact Phase-6 blocker is
  now cleared; end-to-end persistence classification still belongs to roadmap
  item `4`.
- The accepted verification record for this round already includes the exact
  focused regressions, the bounded `ARI-C1` rerun, and the full
  `cabal build all && cabal test` gate. Later stages should reuse this round
  only as bounded predecessor evidence, not as permission to widen the live
  subject.
- This note prepares squash merge only. It does not merge the round, edit
  controller-owned state, or select the next roadmap item.

## Ready For Squash Merge

Yes. The latest review snapshot is `accepted + finalize`, the authoritative
review record matches the finalized review snapshot, the controller state is
already at `stage: "merge"` for `item-3`, predecessor authority and the
inherited baseline remain unchanged, and the approved payload stays within
one bounded item-3 Phase-6 elaboration-resolution round.
