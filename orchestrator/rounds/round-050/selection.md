# Round 050 Selection

Date: 2026-03-20
Round: `round-050`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 17: execute the `H1` continue-bounded bind and exact
next-slice target selection for repaired `URI-R2-C1` after the accepted
`G4 = continue-bounded` decision for the local-binding `rootHasMultiInst`
`G2` / `G3` baseline.

## Why This Item Should Run Now

`orchestrator/state.json` now reflects the live opened round:
`active_round_id: "round-050"`, `active_round_dir:
"orchestrator/rounds/round-050"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and
`last_completed_round: "round-049"`. That machine state means the control
plane is already positioned at the fresh `round-050` selection step, with no
same-round retry to resume and no previously selected task to carry forward.

`orchestrator/roadmap.md` marks items 1 through 16 done. Item 17 (`H1`) is
the lowest-numbered unfinished roadmap entry, and items 18 through 20 depend
on it, so `H1` is the next lawful selection under the guider contract.

The accepted predecessor chain fixes both the timing and the bounded scope
for this round. `orchestrator/rounds/round-049/review-record.json`
finalized `G4` as authoritative with `stage_id: "G4"`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`artifact_path: "docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md"`.
That authoritative `G4` record confirms result token `continue-bounded`,
not `widen-approved` and not `stop-blocked`. The approved `H`-cycle design
in
`docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
therefore makes `H1` the lawful entry point for the fresh bounded cycle.

`H1` is also the narrowest bounded next step. Per the roadmap and the
approved `H`-cycle design, it must bind exactly one next `H2` slice under
repaired `URI-R2-C1`: the remaining local-binding `instArgRootMultiBase`
`keepTargetFinal` / `targetC` fail-closed lane in
`src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to
`src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`.
Selecting `H1` now keeps the campaign inside the inherited explicit-only /
non-equi-recursive / non-cyclic-graph boundary and preserves the accepted
negative findings `U2`, `U3`, and `U4` as still binding.

`Bugs.md` currently has no open bug that overrides this ordering. The
resolved replay-path bug remains continuity context only and does not
authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
`boundVarTarget` widening, non-local widening, or broad automatic recursive
inference here. Repository status shows the expected live-round controller
state (` M orchestrator/state.json` and `?? orchestrator/rounds/round-050/`)
rather than a conflicting implementation change, so it does not alter the
next lawful roadmap choice.

## Round Scope Guard

- This round is limited to roadmap item `H1` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph
  boundary unless the roadmap is explicitly amended first.
- Freeze exactly one next bounded non-widening `H2` slice: the remaining
  local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC`
  fail-closed lane in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with
  future ownership limited to
  `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`.
- Treat the accepted `E`, `F`, and `G` cycles as inherited context only; do
  not reopen their selection, implementation, verification, or decision
  records.
- Do not reinterpret accepted `U2` / `U3` / `U4` negative findings as
  clearance.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
  `boundVarTarget` widening, `rootHasMultiInst` reopen, non-local binding
  widening, equi-recursive reasoning, cyclic structural encoding, a second
  executable interface, or a compatibility or convenience fallback path in
  this round.
