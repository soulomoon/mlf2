# General Automatic Iso-Recursive Current-Architecture Follow-On Post-Item-2 Settlement Surface And Exact Repo-Impact Read

Date: 2026-04-02
Round: `round-175`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: exact packet `sameLaneDoubleAliasFrameClearBoundaryExpr` only
Artifact kind: canonical docs-only aggregate settlement round

## Stage Contract

This artifact implements roadmap item `3` only for `attempt-1` with
`retry: null`.

This round republishes accepted item-2 evidence only. It contributes:

- no new implementation;
- no new focused reruns;
- no new full `cabal build all && cabal test` gate;
- no new successor decision; and
- no item-4 handoff.

Its job is limited to republishing the accepted post-item-2 `narrow success`
read for `sameLaneDoubleAliasFrameClearBoundaryExpr`, rebinding the merged
provenance for that read to accepted sources that actually record `0f44acd`,
and recording the exact repo-impact read as one settled packet only.

The inherited current architecture remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- `non-equi-recursive = keep`;
- the inherited structural model remains non-cyclic; and
- `no-fallback = keep`.

## Authority Ledger

| Input class | Source | Binding read carried here |
| --- | --- | --- |
| Exact frozen packet and architecture boundary | `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md` | The live subject remains `sameLaneDoubleAliasFrameClearBoundaryExpr` only, and the round stays inside the inherited explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback architecture. |
| Accepted item-2 implementation record | `orchestrator/rounds/round-174/implementation-notes.md` | The exact bounded item-2 result is `Outcome: narrow success.` for the frozen packet only. |
| Accepted item-2 review evidence | `orchestrator/rounds/round-174/review.md` | Focused reruns validated recursive success on both authoritative entrypoints for the frozen packet, and the accepted full gate finished `1306 examples, 0 failures`. |
| Accepted item-2 approval record | `orchestrator/rounds/round-174/review-record.json` | Approval remained bounded to roadmap item `2` under the unchanged roadmap identity and evidence summary. |
| Accepted merged provenance | `orchestrator/roadmaps/2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap/rev-001/roadmap.md` | Completion notes: accepted in `round-174` attempt `2`, merged as commit `0f44acd` (`Implement bounded double-alias retained-child packet`) without upgrading the result into repo-level readiness. |

## Exact Post-Item-2 Narrow-Success Read

The exact post-item-2 narrow-success read republished here is:

- exact packet: `sameLaneDoubleAliasFrameClearBoundaryExpr`
- outcome token: `narrow success`
- authoritative surface `runPipelineElab`: recursive output is preserved
- authoritative surface `runPipelineElabChecked`: recursive output is preserved
- architecture read: the recursive output preservation happens within the
  inherited current architecture; no boundary revision is implied

In substance, the accepted read is:
`sameLaneDoubleAliasFrameClearBoundaryExpr` now preserves recursive output on
both `runPipelineElab` and `runPipelineElabChecked` within the inherited
current architecture.

## Provenance Validation

This settlement surface cites predecessor-only accepted evidence. Round-175
does not claim to have rerun the focused packet checks or the full Cabal gate.

The accepted provenance bound here is:

- focused rerun:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
  accepted in round-174 after the packet fix;
- focused rerun:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "double-alias clear-boundary packet"'`
  accepted in round-174 as the exact frozen packet guard;
- accepted full-gate provenance:
  `cabal build all && cabal test`
  reran successfully in round-174 review and closed with
  `1306 examples, 0 failures`; and
- accepted merged provenance:
  active roadmap item-2 completion notes record the narrow-success packet as
  accepted in `round-174` attempt `2`, merged as commit `0f44acd`
  (`Implement bounded double-alias retained-child packet`).

The provenance chain is therefore:
item-1 freeze -> round-174 implementation notes -> round-174 approved review
and review record -> active roadmap item-2 completion notes recording merged
commit `0f44acd`.

## Exact Repo-Impact Read

The exact repo-impact read settled here is one packet only:

- the inherited architecture remains explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback;
- `sameLaneDoubleAliasFrameClearBoundaryExpr` is now a settled bounded
  `narrow success` packet on the authoritative surfaces
  `runPipelineElab` and `runPipelineElabChecked`; and
- the settlement surface is exact-repo-impact only for that packet and does
  not upgrade the repo to a broader readiness state.

No broader packet family is settled by this artifact.

## Non-Claims

This artifact does not claim:

- broader `P3`, `P4`, or `P6` settlement;
- repo-level readiness;
- that the representative-gap family is now generally closed;
- any item-4 outcome token;
- any successor decision; or
- any handoff.
