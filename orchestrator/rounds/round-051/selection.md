# Round 051 Selection

Date: 2026-03-20
Round: `round-051`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 18: execute the `H2` bounded local-binding
`instArgRootMultiBase` `keepTargetFinal` / `targetC` fail-closed
implementation slice frozen by `H1`.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `active_round_id: "round-051"`,
`active_round_dir: "orchestrator/rounds/round-051"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and `last_completed_round: "round-050"`.
That machine state means there is no same-round retry to resume and no prior
selection to carry forward ahead of normal roadmap ordering.

`orchestrator/roadmap.md` marks items 1 through 17 done. Item 18 (`H2`) is the
lowest-numbered unfinished roadmap entry, and items 19 (`H3`) and 20 (`H4`)
both depend on it, so `H2` is the next lawful selection under the guider
contract.

The accepted predecessor chain fixes both the timing and the exact bounded
scope for this round. `orchestrator/rounds/round-050/review-record.json`
finalized `H1` as authoritative with `stage_id: "H1"`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`artifact_path: "docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md"`.
That accepted `H1` bind froze exactly one future `H2` slice: the remaining
local-binding `rootBindingIsLocalType && instArgRootMultiBase`
`keepTargetFinal` / `targetC` fail-closed lane in
`src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to
`src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`.

The approved
`docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
requires the `H` cycle to stay non-widening inside repaired `URI-R2-C1`, and
the older predecessor packet in
`tasks/todo/2026-03-11-recursive-types-orchestration/findings.md` still binds
the inherited explicit-only recursive baseline: iso-recursive forms remain
explicit, there is no implicit unfolding, and no cyclic graph representation is
authorized. Running `H2` now preserves that same bounded ordering instead of
reopening completed `E`, `F`, or `G` work or widening toward broad automatic
recursive inference.

`Bugs.md` currently has no open bug that overrides this ordering. The resolved
replay-path bug remains continuity context only and does not authorize replay
reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget` widening, non-local
widening, or broad recursive-inference work here. Repository status shows only
the expected controller-state preparation (`M orchestrator/state.json`), so it
does not alter the next lawful roadmap choice.

## Round Scope Guard

- This round is limited to roadmap item `H2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary
  unless the roadmap is explicitly amended first.
- Implement exactly the `H1`-frozen local-binding
  `rootBindingIsLocalType && instArgRootMultiBase` `keepTargetFinal` /
  `targetC` fail-closed lane in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with
  focused bounded coverage only in `test/PipelineSpec.hs`.
- Treat `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
  `boundVarTarget` as inherited context only; do not reopen them as separate
  target families in this cycle.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`, non-local
  binding widening, equi-recursive reasoning, cyclic structural encoding, a
  second executable interface, or a compatibility or convenience fallback path
  in this round.
