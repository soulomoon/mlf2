# Round 196 - Task Selection

**Selected milestone**: `milestone-2`
**Selected direction**: `direction-2a-implement-the-selected-p5-lane`
**Selected extraction**: `implement-the-selected-p5-lane`
**Current task/title**: `Implement and verify the bounded retained-child guard-cluster P5 lane on the authoritative pipeline surfaces`

**Roadmap identity**:
- roadmap_id: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001`
- milestone_id: `milestone-2`
- direction_id: `direction-2a-implement-the-selected-p5-lane`
- extracted_item_id: `implement-the-selected-p5-lane`

## Why now

`milestone-1` is already done, `milestone-2` is now the lowest-numbered
unfinished milestone in `rev-001`, and `retry` is `null`, so no same-round
retry state displaces normal forward selection.

The active roadmap now explicitly says that accepted `round-194` froze the
exact retained-child guard-cluster `P5` lane, accepted `round-195` classified
that lane as `bounded current-architecture continuation`, and entry into
`milestone-2` is therefore lawful. Its progress notes also name
`direction-2a-implement-the-selected-p5-lane` as the next unfinished move.
That makes this the only honest immediate handoff: `direction-2b` and
`direction-2c` both require milestone-2 execution evidence first, and
`milestone-3` still depends on milestone-2 finishing.

## Current baseline

- Round-196 base commit: `f4aea6d`
- Active roadmap family and revision remain
  `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
  at `rev-001`
- `milestone-1` is complete; `milestone-2` is pending and now owns the next
  lawful move
- No retry state is active (`retry: null`, no `resume_error`)
- The selected lane is still exactly the frozen retained-child guard cluster
  centered on `boundHasForallFrom`,
  `sameLaneLocalRetainedChildTarget`,
  `keepTargetFinal`,
  `targetC`,
  and `preserveRetainedChildAuthoritativeResult`
- The required reviewer-visible success surfaces remain `runPipelineElab` and
  `runPipelineElabChecked` plus the matching internal and public pipeline
  facades
- The roadmap still bounds any writable implementation slice to
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `test/Research/P5ClearBoundarySpec.hs`, and
  `test/PipelineSpec.hs`
- The current repo baseline does not yet include an accepted milestone-2
  implementation or verification result for this exact lane, so any positive
  read still has to be earned on the authoritative surfaces rather than
  assumed from milestone-1's docs-only classification

## Scope

- Keep this round to one bounded milestone-2 implementation/verification
  campaign for the already frozen retained-child guard-cluster `P5` lane only
- Use the milestone-1 freeze and classification as fixed authority inputs; do
  not reopen the March 28 exact packet, the accepted round-151
  reclassification, or prior same-lane predecessor settlements as live debt
- Require concrete authoritative evidence on `runPipelineElab` /
  `runPipelineElabChecked` and the matching internal/public pipeline facades,
  with the round staying honest if the lane still fails closed or instead
  exposes explicit boundary pressure
- Keep any edits inside the roadmap's bounded writable slice and run the full
  `cabal build all && cabal test` gate before claiming milestone-2 execution
  success
- Do not widen scope into a fresh `P2` lane, post-implementation settlement
  publication, repo-level routing, architecture revision, cyclic search,
  multi-SCC behavior, fallback behavior, or a second interface
