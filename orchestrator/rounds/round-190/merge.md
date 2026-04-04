# Merge Preparation (`round-190` / `item-5`)

## Roadmap Identity

- `roadmap_id`: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`
- `roadmap_item_id`: `item-5`

## Squash Commit Title

`Preserve same-lane nonuple-alias clear-boundary authoritative output`

## Squash Summary

- Merge the approved one-packet `item-5` slice for the active general
  automatic iso-recursive full-inference roadmap.
- The canonical payload is the exact
  `sameLaneNonupleAliasFrameClearBoundaryExpr` packet only: the
  `src/MLF/Elab/TermClosure.hs` change that raises
  `hasRetainedChildClearBoundaryWithAliasBudget source term` from `4` to `5`
  while keeping the outer `hasRetainedChildAliasBoundary v body 2 =` seam
  fixed, plus the matching authoritative-entrypoint regression and exact
  `source term 5` / no `source term 6` mechanism guard in
  `test/PipelineSpec.hs` and the matching research assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
- The approved packet also narrows the octuple mechanism guard back to the
  shared bounded seam so the selected nonuple guard owns the exact helper-step
  claim for this round only.
- The approved squash scope remains bounded to exactly that packet, those
  three code/test files, and the round-local notes in
  `orchestrator/rounds/round-190/`; no pipeline facades, fallback files,
  item-3 route / guard ownership, roadmap files, or controller state belong to
  the squash substance.
- The approved scope is one bounded `item-5` packet only:
  `sameLaneNonupleAliasFrameClearBoundaryExpr`.
- This squash payload does not claim broader same-lane family settlement,
  aggregate `P2`-`P6` closure, `item-5` completion, repo-level readiness, or
  decuple support.
- The reviewer explicitly rechecked a fresh decuple control and confirmed it
  still fails closed on both authoritative entrypoints, so the merge note must
  stay bounded to the nonuple result only.
- No controller-state or roadmap edit belongs to this squash payload. The
  tracked `orchestrator/state.json` change remains controller-owned
  bookkeeping only and stays out of merge substance.

## Predecessor Continuity

- This round does not update the settlement contract or evidence ledger, the
  global settlement gate, post-settlement implementation or hardening records,
  or the final capability-claim record.
- Those predecessor records remain unchanged and authoritative:
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`,
  and
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`.
- The inherited strategic read therefore stays fixed at the previously
  accepted boundary: explicit-only / iso-recursive / non-equi-recursive /
  `non-cyclic-graph = unknown` / no-fallback, with `continue within the
  current architecture` still the strongest lawful global posture.
- The accepted same-lane public-output continuity chain through rounds
  `094` through `098` remains predecessor truth only for blocker-debt and
  current-architecture classification; this round adds one bounded positive
  packet inside that inherited posture and does not reopen those decisions.

## Review Confirmation

- `orchestrator/rounds/round-190/review.md` records `Implemented stage result:
  accepted`, `Attempt verdict: accepted`, `Stage action: finalize`, and
  `Reviewer decision: APPROVED`, so the latest review snapshot is
  `accepted + finalize`.
- `orchestrator/rounds/round-190/review-record.json` matches the approved
  round identity and records `decision: approved`.
- Controller context for the current merge stage is approved attempt `1`.
- The review evidence confirms the reconstructed red run against the old
  `source term 4` budget, the focused alias-through-nonuple authoritative
  reruns, the fresh decuple fail-closed probe, diff hygiene, and the full
  `cabal build all && cabal test` gate passed with `1335 examples, 0
  failures`.

## Merge Readiness

- Merge readiness: confirmed for the approved bounded payload, provided the
  squash is taken from the current approved worktree diff, keeps scope to the
  exact nonuple packet, its three code/test files, and round-local notes, and
  still excludes `orchestrator/state.json` plus any roadmap changes.
- Base branch freshness: exact. `HEAD`,
  `orchestrator/round-190-bounded-positive-family-slice`, and
  `codex/automatic-recursive-type-inference` all resolve to
  `500de6d791317f6f154a8d9821903fabde708827` (`500de6d` short,
  `Advance full-inference roadmap after round-189`), and
  `git rev-list --left-right --count
  codex/automatic-recursive-type-inference...HEAD` reports `0 0`.
- The approved payload is therefore the current round worktree patch atop the
  fresh base, not a stale committed branch that needs replay.
- Round `round-190` is ready for squash merge.

## Follow-Up Notes

- Post-merge controller bookkeeping should preserve the roadmap identity above
  unchanged and treat the bounded `TermClosure` helper change, the exact
  nonuple-alias regressions, `implementation-notes.md`, `review.md`,
  `review-record.json`, and this merge note as the authoritative completed
  outcome for this one packet.
- `item-5` remains pending after this merge. Later rounds still need
  additional bounded positive-family slices and the eventual aggregate
  artifact; this round settles only
  `sameLaneNonupleAliasFrameClearBoundaryExpr`.
- Keep later summaries honest about the boundary reached here: the accepted
  nonuple packet is merge-ready, the fresh decuple control still fails
  closed, and the outer alias-boundary seam remains fixed at `2`.
