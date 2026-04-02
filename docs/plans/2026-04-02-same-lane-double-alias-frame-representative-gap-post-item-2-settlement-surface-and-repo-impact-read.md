# Same-Lane Double-Alias-Frame Representative-Gap Post-Item-2 Settlement Surface And Repo-Impact Read

Date: 2026-04-02
Round: `round-175`
Roadmap item: `item-3`
Live subject: exact packet `sameLaneDoubleAliasFrameClearBoundaryExpr` only
Artifact kind: canonical docs-only aggregate settlement republication

## Settlement Surface

This artifact republishes accepted item-2 authority only. It adds no new
runtime evidence and does not widen beyond the exact packet
`sameLaneDoubleAliasFrameClearBoundaryExpr`.

- `sameLaneDoubleAliasFrameClearBoundaryExpr` is now a bounded `narrow success`
  packet on `runPipelineElab` and `runPipelineElabChecked`.
- The accepted packet meaning carried forward here is:
  one additional same-lane alias shell at the current `hold` boundary is now
  preserved, so both authoritative entrypoints keep recursive output for this
  exact packet.

## Authority Chain

- `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md`
  froze the exact follow-on packet, inherited boundary, item-2 success bar,
  and fail-closed writable slice.
- `orchestrator/rounds/round-174/selection.md` selected only the frozen packet
  `sameLaneDoubleAliasFrameClearBoundaryExpr` for item `2`.
- `orchestrator/rounds/round-174/implementation-notes.md`,
  `orchestrator/rounds/round-174/review.md`, and
  `orchestrator/rounds/round-174/merge.md` record the accepted and merged
  item-2 result carried forward here.

## Accepted Provenance

- Focused packet check:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
  accepted as `3 examples, 0 failures`; the exact packet now succeeds on both
  authoritative pipeline entrypoints.
- Focused pipeline regression:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "double-alias clear-boundary packet"'`
  accepted as `1 example, 0 failures`.
- Full-gate provenance from accepted round-174 review and merge:
  `cabal build all && cabal test` finished `1306 examples, 0 failures`.

## Exact Repo-Impact Read

- The current architecture now carries one additional settled packet only:
  `sameLaneDoubleAliasFrameClearBoundaryExpr` is a bounded `narrow success` on
  `runPipelineElab` and `runPipelineElabChecked`.
- The inherited production baseline remains explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback.
- The broader same-lane representative-gap family across `P3` / `P4` / `P6`
  remains unresolved.
- This settlement is not a repo-readiness claim, not a boundary revision, and
  not authorization for cyclic search, multi-SCC search, fallback widening,
  equi-recursive reasoning, or a second interface.

## Non-Widening Boundary

This artifact does not settle general `P3` / `P4` / `P6`, does not make a
repo-readiness claim, and does not decide the successor-gate work reserved for
item `4`.
