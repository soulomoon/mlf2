# Round 053 Selection

Date: 2026-03-20
Round: `round-053`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 20: execute the bounded `H4` next-cycle decision gate for the
accepted `H3`-reverified repaired `URI-R2-C1` local-binding
`rootLocalInstArgMultiBase` slice.

## Why This Item Should Run Now

`orchestrator/state.json` reflects the live opened round:
`active_round_id: "round-053"`, `active_round_dir:
"orchestrator/rounds/round-053"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and `last_completed_round:
"round-052"`. That machine state means there is no same-round retry to
resume and no prior selection to carry forward ahead of normal roadmap
ordering.

`orchestrator/roadmap.md` marks items 1 through 19 done. Item 20 (`H4`) is
the lowest-numbered unfinished roadmap entry, and there is no earlier pending
item left to run first, so `H4` is the next lawful selection under the guider
contract.

The accepted predecessor chain fixes both the timing and the exact bounded
scope for this round. `orchestrator/rounds/round-052/review-record.json`
finalized `H3` as authoritative with `stage_id: "H3"`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`artifact_path:
"docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md"`. That
accepted `H3` evidence reverified exactly one lane: the repaired
`URI-R2-C1` local `rootLocalInstArgMultiBase` / `targetC -> rootFinal`
slice, under read-only `Fallback.hs` / `PipelineSpec.hs` anchors, a fresh
focused `ARI-C1 feasibility characterization (bounded prototype-only)` rerun,
a fresh full-repo gate, predecessor continuity checks, the matched non-local
fail-closed contrast, and preserved `baseTarget` rejection outside the
selected lane. Under the roadmap and the approved `H`-cycle design, `H4` is
therefore the next aggregate decision gate for that already-reverified lane;
it is not a new implementation slice, not a new verification target, and not
an in-round roadmap rewrite.

The approved
`docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
makes `H4` the fourth step in the bounded `H1` through `H4` cycle and
requires this stage to record exactly one bounded next-step result grounded in
the accepted `H3` evidence chain: `continue-bounded`, `widen-approved`, or
`stop-blocked`. Running `H4` now preserves that bounded ordering while keeping
the live subject fixed to repaired `URI-R2-C1` and the inherited
explicit-only / non-equi-recursive / non-cyclic-graph boundary. This
selection does not itself widen the roadmap.

The predecessor packet under
`tasks/todo/2026-03-11-recursive-types-orchestration/` remains boundary
context only. Its accepted mechanism-table state says the explicit-only
acyclic `TyMu` path is complete on current `master` while automatic
recursive-type inference remains disabled, so that predecessor evidence
supports keeping `H4` as a bounded decision gate rather than broadening the
live subject into automatic recursive inference work.

`Bugs.md` still carries open bug `BUG-2026-03-16-001`, but that replay-path
`MLF.Elab.Inst` / `InstBot` defect remains continuity context only and is
outside the accepted `H1` / `H2` / `H3` lane. It does not override roadmap
ordering or authorize replay reopen, non-local widening, `boundVarTarget`
widening, or broader recursive-inference work in this round. Current
repository status shows only the expected controller-state preparation
(`M orchestrator/state.json`) and no conflicting implementation diff, so
there is no repository-status reason to bypass the lowest-numbered unfinished
item.

## Round Scope Guard

- This round is limited to roadmap item `H4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph
  boundary unless the roadmap is explicitly amended first.
- Ground the decision strictly in the accepted `H3` evidence chain
  (`docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md` plus
  `orchestrator/rounds/round-052/review-record.json`) and the accepted `H1`
  / `H2` continuity it carries forward; do not reopen `H1` selection, `H2`
  implementation, or `H3` verification.
- Record exactly one bounded next-step result for the already-reverified
  `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane:
  `continue-bounded`, `widen-approved`, or `stop-blocked`.
- Preserve the frozen `src/MLF/Elab/Run/ResultType/Fallback.hs` /
  `test/PipelineSpec.hs` ownership, the matched non-local fail-closed
  contrast, and preserved `baseTarget` rejection outside the selected lane.
- Treat `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
  `boundVarTarget` as inherited context only; do not reopen them as separate
  target families in this round.
- Do not treat this decision gate as implementation authority. No replay
  reopen, no `MLF.Elab.Inst` / `InstBot` work, no non-local widening, no
  equi-recursive reasoning, no cyclic structural encoding, no second
  executable interface, and no compatibility or convenience fallback path are
  in scope for this round.
