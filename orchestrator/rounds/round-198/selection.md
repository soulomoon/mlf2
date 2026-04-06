# Round 198 - Task Selection

**Selected milestone**: `milestone-3`
**Selected direction**: `direction-3a-refresh-the-p5-vs-p2-gap-ledger`
**Selected extraction**: `refresh-the-p5-vs-p2-gap-ledger`
**Current task/title**: `Refresh the exact post-milestone-2 P5-vs-P2 remaining-frontier ledger from the settled P5 baseline`

**Roadmap identity**:
- roadmap_id: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001`
- milestone_id: `milestone-3`
- direction_id: `direction-3a-refresh-the-p5-vs-p2-gap-ledger`
- extracted_item_id: `refresh-the-p5-vs-p2-gap-ledger`

## Why now

`milestone-2` is done, `milestone-3` is the lowest-numbered unfinished
milestone in `rev-001`, and `retry` is `null`, so no same-round retry state
displaces normal forward selection.

The roadmap progress notes now state that
`direction-3a-refresh-the-p5-vs-p2-gap-ledger` is the next concrete
unfinished move after accepted `round-197` published the bounded
post-implementation `P5` settlement surface. The current repo baseline still
lacks the promised docs-only reread that compares that settled `P5` result
against the accepted `P2` packet-specific folklore and names the current
strongest remaining positive-family blocker honestly. That reread has to land
before any bounded `P2` lane freeze in `direction-3b` or any broader
repo-level readiness or boundary-pressure claim.

## Current baseline

- Round-198 base commit: `f0d067f`
- Active roadmap family and revision remain
  `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
  at `rev-001`
- `milestone-1` and `milestone-2` are done; `milestone-3` is in progress
- No retry state is active (`retry: null`, no `resume_error`)
- Accepted `round-197` already fixed the settled `P5` baseline:
  `sameLaneAliasFrameClearBoundaryExpr` has bounded current-architecture
  support on `runPipelineElab` / `runPipelineElabChecked`,
  `nestedForallContrastExpr` remains fail-closed with
  `PhiTranslatabilityError`, and the merged implementation payload stayed
  `test-only`
- The current repo baseline does not yet include an accepted routing artifact
  that refreshes the remaining positive-family ledger across `P5` versus `P2`
- The baseline therefore does not yet justify freezing a new `P2` follow-on
  lane, promoting the accepted exact `C1` packet into family closure, or
  revisiting repo-level readiness or architecture revision

## Scope

- Keep this round docs-only and limited to one aggregate reread of the
  remaining positive-family frontier after the settled `P5` lane
- Compare the accepted post-implementation `P5` settlement surface against the
  already accepted `P2` packet-specific folklore without collapsing them into
  the same claim
- End with one exact remaining-frontier conclusion that says whether `P5`
  still dominates or `P2` is now the next lawful bounded follow-on
- Keep predecessor truth closed: do not reopen the March 28 exact `P5`
  packet, the round-151 reclassification, prior same-lane settlements, or the
  accepted exact `C1` packet as live debt
- Do not widen into implementation, fresh tests, `direction-3b`, repo-level
  readiness, cyclic search, multi-SCC behavior, fallback behavior, or a
  second interface
