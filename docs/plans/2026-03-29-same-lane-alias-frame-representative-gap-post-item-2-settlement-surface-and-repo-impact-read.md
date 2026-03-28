# Same-Lane Alias-Frame Representative-Gap Post-Item-2 Settlement Surface And Repo-Impact Read

Date: 2026-03-29
Round: `round-137`
Roadmap item: `item-3`
Live subject: exact packet `sameLaneAliasFrameClearBoundaryExpr` only
Artifact kind: canonical docs-only aggregate settlement republication

## Settlement Surface

This artifact republishes accepted item-2 authority only. It adds no new
runtime evidence and does not widen beyond the exact packet
`sameLaneAliasFrameClearBoundaryExpr`.

- `sameLaneAliasFrameClearBoundaryExpr` remains a `narrower current-architecture blocker` on `runPipelineElab` and `runPipelineElabChecked`.
- The accepted blocker text carried forward verbatim is:
  `PipelineElabError (PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 34}]"])`

## Authority Chain

- `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md`
  froze the exact second packet, the current-architecture boundary, and the
  item-2 success bar.
- `orchestrator/rounds/round-136/selection.md` selected only the frozen
  packet `sameLaneAliasFrameClearBoundaryExpr` for item `2`.
- `orchestrator/rounds/round-136/implementation-notes.md`,
  `orchestrator/rounds/round-136/review.md`, and
  `orchestrator/rounds/round-136/merge.md` record the accepted and merged
  item-2 classification carried forward here.

## Accepted Provenance

- Focused packet check:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
  accepted as `2 examples, 0 failures`; both authoritative pipeline entry
  points were asserted as the same `PhiTranslatabilityError` blocker above.
- Narrow adjacent guard provenance remained green in the accepted round-136
  review:
  `same-lane retained-child exact packet clears Phase 6 elaboration`,
  `same-lane retained-child exact packet authoritative public output stays forall identity`,
  and
  `keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary`
  each finished `1 example, 0 failures`.
- Full-gate provenance from accepted round-136 review and merge remained:
  `cabal build all && cabal test` finished `1153 examples, 0 failures`.

## Exact Repo-Impact Read

- The current architecture remains blocked for this one second packet on
  `runPipelineElab` and `runPipelineElabChecked`.
- The inherited production baseline remains explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback.
- The broader same-lane retained-child representative-gap family across
  `P3` / `P4` / `P6` remains unresolved.
- The settled first same-lane pocket and the settled exact `P5` packet remain
  predecessor truth only.

## Non-Widening Boundary

This artifact does not settle general `P3` / `P4` / `P6`, does not make a
repo-readiness claim, and does not decide the successor-gate work reserved
for item `4`.
