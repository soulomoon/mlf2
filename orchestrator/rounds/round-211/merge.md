# Merge Preparation (`round-211` / `milestone-2` / `direction-2b`)

## Roadmap Identity

- `roadmap_id`: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- `roadmap_revision`: `rev-014`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-014`
- `roadmap_item_id`: absent in the active `rev-014` state / selection / review lineage for this round
- `milestone_id`: `milestone-2`
- `direction_id`: `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
- `extracted_item_id`: `continue-round-211-selected-same-wrapper-nested-forall-through-broader-authoritative-application-and-let-polymorphism-handoff-seam`

## Squash Commit Title

`Repair same-wrapper nested-forall across authoritative AAppF/ALetF handoff seams`

## Squash Summary

- Merge the approved `milestone-2` / `direction-2b` /
  `continue-round-211-selected-same-wrapper-nested-forall-through-broader-authoritative-application-and-let-polymorphism-handoff-seam`
  slice for the active broader-positive nested-`forall` roadmap.
- The canonical squash substance is the approved implementation/test payload in
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`,
  plus the round-local notes under `orchestrator/rounds/round-211/`.
- The approved outcome stays bounded to the broader authoritative
  application / let-polymorphism handoff seam and the admitted helper-local
  scaffold under
  `funInstRecovered`,
  `argInstFromFun`,
  `fApp`,
  `scheme`,
  `rhsAbs0`, and
  `rhsAbs`.
  It does not reopen `Legacy.hs`, closed continuity surfaces, fallback rescue,
  a second interface, cyclic or multi-SCC search, equi-recursive reasoning,
  or later roadmap work.
- The approved behavior keeps the selected same-wrapper nested-`forall`
  packet green on both authoritative entrypoints, preserves
  checked-authoritative parity and the accepted bug/regression cluster, keeps
  direct let-polymorphism rows green, keeps the nested-let probes fail-fast,
  and preserves the correct semantic `g g` failure.
- Keep the squash scope honest: only the approved implementation/test payload
  plus round-local notes belong to merge substance. Controller-owned
  `orchestrator/state.json`, pointer stubs
  (`orchestrator/roadmap.md`, `orchestrator/verification.md`,
  `orchestrator/retry-subloop.md`), roadmap-publication surfaces, and
  predecessor artifacts stay outside this squash payload.

## Predecessor Continuity

- This round still finalizes the same preserved `round-211` packet under the
  accepted `rev-014` contract; it does not restart the lane on a fresh round
  or rewrite settled predecessor evidence from `round-208`, `round-209`, or
  `round-210`.
- The live read remains the bounded authoritative `AAppF` / `ALetF`
  continuation only. The retained-child clear-boundary lane, prior narrower
  retries, and the rejected `rev-013` flatten-only structural objection stay
  predecessor history rather than reopened merge scope.
- Closed continuity anchors remain unchanged and outside merge substance:
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.

## Review Confirmation

- `orchestrator/rounds/round-211/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`,
  `Stage action: finalize`, and an explicit `APPROVED` decision under the
  active `rev-014` contract.
- `orchestrator/rounds/round-211/review-record.json` matches the same
  roadmap identity and records `decision: approved`,
  `stage_action: finalize`, and `merge_readiness: satisfied`.
- The approved review evidence carries forward the green focused
  20-command matcher matrix, reruns
  `./scripts/thesis-conformance-gate.sh` to PASS, and reruns
  `cabal build all && cabal test` to `1341 examples, 0 failures`.

## Merge Readiness

- Merge readiness: confirmed for the approved payload, provided the squash is
  taken from the current approved worktree patch, kept to the approved
  implementation/test payload plus round-local notes, and continues to exclude
  controller-owned state/pointer surfaces.
- Base branch freshness: exact and current.
  `HEAD`,
  `orchestrator/round-211-repair-same-wrapper-nested-forall-across-authoritative-annotation-and-post-annotation-handoff-seams`,
  `codex/automatic-recursive-type-inference`, and
  `git merge-base HEAD codex/automatic-recursive-type-inference`
  all resolve to `5346a9460acb6953a1dfe3ed37e7db93872510ef`, and
  `git rev-list --left-right --count codex/automatic-recursive-type-inference...HEAD`
  reports `0 0`.
- The approved squash substance remains honest on that fresh base:
  `git diff --cached --name-only` is limited to the approved
  implementation/test payload plus round-local notes under
  `orchestrator/rounds/round-211/`, while controller-owned
  `orchestrator/state.json`,
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, and
  `orchestrator/retry-subloop.md`
  remain outside staged merge substance.
- With freshness exact, approved review evidence current, and merge scope still
  bounded, the round is ready for squash merge without another replay or
  review refresh.

## Follow-Up Notes

- Preserve the roadmap identity above unchanged during later controller
  bookkeeping, including the explicit fact that `roadmap_item_id` is absent
  for this active milestone/direction extraction.
- When squashing, keep the merge substance limited to the approved
  implementation/test payload plus the round-local notes for `round-211`.
- Exclude controller-owned state, pointer stubs, roadmap-publication surfaces,
  and predecessor artifacts from the squash. The authoritative completed
  outcome for this round is the approved implementation/test payload together
  with `round-211`'s implementation, review, and merge notes.
