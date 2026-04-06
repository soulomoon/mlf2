# Merge Preparation (`round-196` / `milestone-2` / `direction-2a`)

## Roadmap Identity

- `roadmap_id`: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001`
- `milestone_id`: `milestone-2`
- `direction_id`: `direction-2a-implement-the-selected-p5-lane`
- `roadmap_item_id`: `implement-the-selected-p5-lane` (the runtime state uses
  `extracted_item_id`; no separate `roadmap_item_id` field is present)
- `extracted_item_id`: `implement-the-selected-p5-lane`

## Squash Commit Title

`Pin the selected P5 retained-child lane to authoritative pipeline tests`

## Squash Summary

- Merge the approved bounded `milestone-2` / `direction-2a` /
  `implement-the-selected-p5-lane` slice for the active P5-and-P2 follow-on
  roadmap.
- The implementation-owned payload is intentionally test-only:
  `test/Research/P5ClearBoundarySpec.hs` adds the
  `sameLaneAliasFrameClearBoundaryExpr` specimen, a same-lane fallback harness,
  and authoritative-entrypoint checks over `runPipelineElab` and
  `runPipelineElabChecked`; `test/PipelineSpec.hs` adds a source guard that
  keeps the selected lane pinned to `boundHasForallFrom`,
  `preserveRetainedChildAliasBoundary`, and
  `preserveRetainedChildAuthoritativeResult`.
- The current worktree status stays honest about scope: the implementation
  diff is those two modified test files; the round-local payload is
  `orchestrator/rounds/round-196/selection.md`,
  `orchestrator/rounds/round-196/plan.md`,
  `orchestrator/rounds/round-196/implementation-notes.md`,
  `orchestrator/rounds/round-196/review.md`,
  `orchestrator/rounds/round-196/review-record.json`, and this
  `orchestrator/rounds/round-196/merge.md`; the tracked
  `orchestrator/state.json` change remains controller-owned bookkeeping outside
  the squash substance.
- The approved result is a bounded milestone-2 evidence slice, not a
  production widening: the current workspace already preserved this exact
  retained-child alias-frame lane on the authoritative pipeline surfaces, so
  this round lands reviewer-visible tests and round-local artifacts rather than
  `src/`, `src-public/`, or Cabal changes.
- Keep the squash scope narrow and honest: no roadmap revision, no controller
  state, no March 28 packet reopen, no `round-151` relitigation, no new `P2`
  lane, and no broader architecture or boundary-pressure claim belong to this
  merge.

## Predecessor Continuity

- Accepted `round-194` froze the exact retained-child guard-cluster lane,
  authoritative success surfaces, and writable slice; accepted `round-195`
  classified that lane as `bounded current-architecture continuation` and made
  this bounded milestone-2 evidence round the next lawful move.
- This round therefore records only one approved follow-on slice for that
  already-frozen lane. It does not reopen the March 28 exact packet, rewrite
  settled predecessor evidence, or widen beyond the retained-child guard
  cluster selected in `round-194` / `round-195`.
- The inherited read stays narrow: the selected lane now has authoritative
  regression coverage, while `nestedForallContrastExpr` remains reject-side
  contrast only and the accepted `round-151` nested-forall `mu`-absorption
  reclassification remains settled correct behavior.

## Review Confirmation

- `orchestrator/rounds/round-196/review.md` records `Decision: APPROVED` and
  confirms the implementation-owned diff is limited to
  `test/PipelineSpec.hs` and `test/Research/P5ClearBoundarySpec.hs`, with the
  focused retained-child checks passing and the full gate passing at
  `1338 examples, 0 failures`.
- `orchestrator/rounds/round-196/review-record.json` matches the same roadmap
  identity and records `decision: approved` for `milestone-2` /
  `direction-2a-implement-the-selected-p5-lane` /
  `implement-the-selected-p5-lane`.
- `orchestrator/rounds/round-196/implementation-notes.md` is consistent with
  the approved review: the round started with a focused RED in the new
  alias-frame fallback probe, repaired the test harness only, and finished with the
  retained-child focused checks plus `cabal build all && cabal test` green.

## Merge Readiness

- Merge readiness: confirmed for the approved bounded milestone-2 payload,
  provided the squash stays limited to the two test files plus the round-local
  artifacts under `orchestrator/rounds/round-196/` and continues to exclude
  controller-owned `orchestrator/state.json`.
- Base branch freshness: confirmed locally.
  `codex/automatic-recursive-type-inference` resolves to
  `f4aea6d2fd0686743fb222de56ae19137ab2efd9`,
  `git merge-base HEAD codex/automatic-recursive-type-inference` resolves to
  the same commit, and
  `git rev-list --left-right --count codex/automatic-recursive-type-inference...HEAD`
  reports `0 0`.
- Base branch freshness: also confirmed against the current remote head.
  `git ls-remote origin refs/heads/codex/automatic-recursive-type-inference`
  resolves to `fde4339b0c8c554ede0d246129bd2c9bf5d4c112`, and
  `git rev-list --left-right --count codex/automatic-recursive-type-inference...fde4339b0c8c554ede0d246129bd2c9bf5d4c112`
  reports `253 0`, so the local base branch already contains the current
  remote tip.
- Round `round-196` is ready for squash merge.

## Follow-Up Notes

- Post-merge controller bookkeeping should preserve the roadmap identity above
  unchanged and treat this round as one approved milestone-2 test-evidence
  settlement for the selected retained-child lane.
- Keep later summaries honest about substance: the implementation-owned change
  is test-only, the round-local artifacts document the approved settlement, and
  no production or public-surface widening was needed to carry this lane on
  the authoritative pipeline entrypoints.
- The merge closes one bounded successor slice only; it does not itself settle
  broader P5 architecture questions or authorize a fresh `P2` campaign.
