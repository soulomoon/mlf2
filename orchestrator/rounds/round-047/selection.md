# Round 047 Selection

Date: 2026-03-19
Round: `round-047`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 14: execute the `G2` bounded local-binding `rootHasMultiInst`
`keepTargetFinal` / `targetC` fail-closed implementation slice frozen by `G1`.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `active_round_id: round-047`,
`stage: select-task`, `current_task: null`, and `retry: null`, so no same-round
retry is active and no prior review outcome forces a retry resume ahead of
normal roadmap selection.

`orchestrator/roadmap.md` now marks items 1 through 13 done, leaving item 14
(`G2`) as the lowest-numbered unfinished roadmap entry. Items 15 and 16 depend
on item 14 and are therefore not yet selectable.

The accepted predecessor chain fixes both the timing and the bounded scope for
this round. `orchestrator/rounds/round-046/review-record.json` finalized `G1`
as authoritative with `stage_id: "G1"`, `attempt_verdict: "accepted"`,
`stage_action: "finalize"`, and
`artifact_path: "docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md"`.
That accepted `G1` bind froze exactly one future `G2` slice: the local-binding
`rootHasMultiInst` `keepTargetFinal` / `targetC` fail-closed lane in
`src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to
`src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`.

Running `G2` now preserves the continue-bounded ordering while keeping the live
campaign inside repaired `URI-R2-C1` and the inherited explicit-only /
non-equi-recursive / non-cyclic-graph boundary. This selection does not choose
widened work: `G2` implements only the one accepted `G1`-frozen
`rootHasMultiInst` family and may not widen into `instArgRootMultiBase`,
replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening,
non-local binding widening, equi-recursive reasoning, cyclic structural
encoding, a second executable interface, or compatibility / convenience
fallbacks.

`Bugs.md` keeps replay-path bug `BUG-2026-03-16-001` resolved as continuity
context only and does not authorize replay-lane reopen here. Current repository
status is already non-pristine because controller state is preparing the next
round and the active task packet remains untracked, so selecting the narrow
`G2` bounded implementation step best advances the lowest unfinished roadmap
item without rewriting unrelated work.

## Round Scope Guard

- This round is limited to roadmap item `G2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary
  unless the roadmap is explicitly amended first.
- Implement exactly the `G1`-frozen local-binding `rootHasMultiInst`
  `keepTargetFinal` / `targetC` fail-closed lane in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, with focused bounded coverage only
  in `test/PipelineSpec.hs`.
- Leave `instArgRootMultiBase` explicitly unselected and fail-closed for this
  cycle.
- Do not reopen the accepted `E2` / `E3` same-lane retained-child baseline or
  the accepted `F1` / `F2` / `F3` / `F4` / `G1` chain.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
  `boundVarTarget` widening, non-local binding widening, equi-recursive
  reasoning, cyclic structural encoding, a second executable interface, or a
  compatibility or convenience fallback path in this round.
