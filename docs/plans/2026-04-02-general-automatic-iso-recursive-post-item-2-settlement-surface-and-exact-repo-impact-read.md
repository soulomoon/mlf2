# General Automatic Iso-Recursive Post-Item-2 Settlement Surface And Exact Repo-Impact Read

Date: 2026-04-02
Round: `round-171`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-2`
Retry state: active (`retry.attempt: 2`, `retry.latest_attempt_verdict: "rejected"`, `retry.latest_stage_action: "retry"`, `retry.retry_reason: "provenance gaps"`)
Live subject: exact packet `sameLaneAliasFrameClearBoundaryExpr` only
Artifact kind: canonical docs-only aggregate settlement round

## Stage Contract

This artifact implements roadmap item `3` only for `attempt-2` under the live
retry state (`retry.attempt: 2`, `retry.latest_attempt_verdict: "rejected"`,
`retry.latest_stage_action: "retry"`, `retry.retry_reason: "provenance gaps"`).

This round republishes accepted item-2 evidence only. It contributes:

- no new implementation;
- no new focused reruns;
- no new full `cabal build all && cabal test` gate;
- no new successor decision; and
- no item-4 handoff.

Its job is limited to republishing the accepted post-item-2 narrow-success
read for `sameLaneAliasFrameClearBoundaryExpr`, rebinding the merged-commit
provenance for that read to the accepted source that actually records
`45d765b`, and recording the exact repo-impact read as one settled packet
only.

The inherited current architecture remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- `non-equi-recursive = keep`;
- the inherited structural model remains non-cyclic; and
- `no-fallback = keep`.

## Authority Ledger

| Input class | Source | Binding read carried here |
| --- | --- | --- |
| Exact frozen packet and architecture boundary | `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md` | The live subject remains `sameLaneAliasFrameClearBoundaryExpr` only, and the round stays inside the inherited explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback architecture. |
| Accepted item-2 implementation record | `orchestrator/rounds/round-170/implementation-notes.md` | The exact bounded item-2 result is `Outcome: narrow success.` for the frozen packet only. |
| Accepted item-2 review evidence | `orchestrator/rounds/round-170/review.md` | Focused reruns validated recursive success on both authoritative entrypoints for the frozen packet, the broader retained-child focused guard stayed green, and the accepted full gate finished `1303 examples, 0 failures`. |
| Accepted item-2 approval record | `orchestrator/rounds/round-170/review-record.json` | Approval remained bounded to item `2` under the unchanged roadmap identity and evidence summary. |
| Accepted merged provenance | `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md` | Completion notes: accepted in `round-170`, merged as commit `45d765b` (`Preserve recursive output for frozen alias-frame packet`) without upgrading the result into repo-level readiness. |

## Exact Post-Item-2 Narrow-Success Read

The exact post-item-2 narrow-success read republished here is:

- exact packet: `sameLaneAliasFrameClearBoundaryExpr`
- outcome token: `narrow success`
- authoritative surface `runPipelineElab`: recursive output is preserved
- authoritative surface `runPipelineElabChecked`: recursive output is preserved
- architecture read: the recursive output preservation happens within the
  inherited current architecture; no boundary revision is implied

In substance, the accepted read is:
`sameLaneAliasFrameClearBoundaryExpr` now preserves recursive output on both
`runPipelineElab` and `runPipelineElabChecked` within the inherited current
architecture.

## Provenance Validation

This settlement surface cites predecessor-only accepted evidence. Round-171
does not claim to have rerun the focused packet checks or the full Cabal gate.

The accepted provenance bound here is:

- focused rerun:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
  accepted in round-170 after the packet fix;
- focused rerun:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "alias-frame clear-boundary packet"'`
  accepted in round-170 as the exact frozen packet guard;
- broader retained-child focused guard:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "retained-child"'`
  accepted in round-170 and stayed green;
- accepted full-gate provenance:
  `cabal build all && cabal test`
  reran successfully in round-170 review and closed with
  `1303 examples, 0 failures`; and
- accepted merged provenance:
  active roadmap item-2 completion notes record the narrow-success packet as
  accepted in `round-170`, merged as commit `45d765b`
  (`Preserve recursive output for frozen alias-frame packet`).

The provenance chain is therefore:
item-1 freeze -> round-170 implementation notes -> round-170 approved review
and review record -> active roadmap item-2 completion notes recording merged
commit `45d765b`.

## Exact Repo-Impact Read

The exact repo-impact read settled here is one packet only:

- the inherited architecture remains explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback;
- `sameLaneAliasFrameClearBoundaryExpr` is now a settled bounded
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
