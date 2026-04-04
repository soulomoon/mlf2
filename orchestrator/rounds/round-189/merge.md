# Merge Preparation (`round-189` / `item-5`)

## Roadmap Identity

- `roadmap_id`: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`
- `roadmap_item_id`: `item-5`

## Squash Commit Title

`Preserve same-lane octuple-alias clear-boundary authoritative output`

## Squash Summary

- Merge the approved one-packet `item-5` slice for the active general
  automatic iso-recursive full-inference roadmap.
- The canonical payload is the exact
  `sameLaneOctupleAliasFrameClearBoundaryExpr` packet only: the
  `src/MLF/Elab/TermClosure.hs` change that raises
  `hasRetainedChildClearBoundaryWithAliasBudget source term` from `3` to `4`
  while keeping the outer `hasRetainedChildAliasBoundary v body 2 =` seam
  fixed, plus the matching authoritative-entrypoint regression and exact
  `source term 4` mechanism guard in `test/PipelineSpec.hs` and the matching
  research assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
- The approved squash scope remains bounded to exactly those three code/test
  files plus the round-local notes in `orchestrator/rounds/round-189/`; no
  pipeline facades, fallback files, item-3 route / guard ownership, roadmap
  files, or controller state belong to the squash substance.
- The approved scope is one bounded `item-5` packet only:
  `sameLaneOctupleAliasFrameClearBoundaryExpr`.
- This squash payload does not claim broader same-lane family settlement,
  aggregate `P2`-`P6` closure, `item-5` completion, repo-level readiness, or
  nonuple support.
- The reviewer explicitly rechecked a fresh nonuple control and confirmed it
  still fails closed on both authoritative entrypoints, so the merge note must
  stay bounded to the octuple result only.
- No controller-state or roadmap edit belongs to this squash payload. The
  tracked `orchestrator/state.json` change remains controller-owned
  bookkeeping only and stays out of merge substance.

## Review Confirmation

- `orchestrator/rounds/round-189/review.md` records `Attempt verdict:
  accepted` and `Decision: APPROVED`.
- `orchestrator/rounds/round-189/review-record.json` matches the approved
  round identity and records `decision: approved`.
- Controller context for the current merge stage is approved attempt `1`.
- The review evidence confirms the reconstructed red run against the old
  `source term 3` budget, the focused alias-through-octuple authoritative
  reruns, the fresh nonuple fail-closed probe, diff hygiene, and the full
  `cabal build all && cabal test` gate passed with `1331 examples, 0
  failures`.

## Merge Readiness

- Merge readiness: confirmed for the approved bounded payload, provided the
  squash is taken from the current approved worktree diff, keeps scope to the
  exact octuple packet, its three code/test files, and round-local notes, and
  still excludes `orchestrator/state.json`.
- Base branch freshness: exact. `HEAD`,
  `orchestrator/round-189-bounded-positive-family-slice`, and
  `codex/automatic-recursive-type-inference` all resolve to
  `2ebf4bf1148d427836bb535c27d9765bd28d4c00` (`2ebf4bf` short,
  `Advance full-inference roadmap after round-188`), and
  `git merge-base HEAD codex/automatic-recursive-type-inference` returns that
  same commit.
- `git rev-list --left-right --count
  codex/automatic-recursive-type-inference...orchestrator/round-189-bounded-positive-family-slice`
  reports `0 0`, so there is no committed divergence from the current base
  tip. The approved payload is therefore the current round worktree patch atop
  the fresh base, not a stale committed branch that needs replay.

## Follow-Up Notes

- Post-merge controller bookkeeping should preserve the roadmap identity above
  unchanged and treat the bounded `TermClosure` helper change, the exact
  octuple-alias regressions, `implementation-notes.md`, `review.md`,
  `review-record.json`, and this merge note as the authoritative completed
  outcome for this one packet.
- `item-5` remains pending after this merge. Later rounds still need
  additional bounded positive-family slices and the eventual aggregate
  artifact; this round settles only
  `sameLaneOctupleAliasFrameClearBoundaryExpr`.
- Keep later summaries honest about the boundary reached here: the accepted
  octuple packet is merge-ready, the fresh nonuple control still fails closed,
  and the outer alias-boundary seam remains fixed at `2`.
