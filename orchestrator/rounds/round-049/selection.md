# Round 049 Selection

Date: 2026-03-19
Round: `round-049`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-016`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016`
- State Snapshot: `orchestrator/rounds/round-049/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 16: execute the bounded `G4` next-cycle decision gate for the
accepted `G3`-reverified repaired `URI-R2-C1` local-binding
`rootHasMultiInst` slice.

## Why This Item Should Run Now

`orchestrator/rounds/round-049/state-snapshot.json` is parked at `active_round_id: "round-049"`,
`stage: "select-task"`, `current_task: null`, and `retry: null`, so there is no
same-round retry to resume and no prior review outcome that overrides normal
roadmap ordering.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/roadmap.md` now marks items 1 through 15 done. Item 16 (`G4`) is
the only remaining pending roadmap entry, so it is the next lawful selection.
There is no lower-numbered unfinished item left to run first.

The accepted predecessor chain also fixes both the timing and the bounded scope
for this round. `orchestrator/rounds/round-048/review-record.json` finalized
`G3` as authoritative with `stage_id: "G3"`, `attempt_verdict: "accepted"`,
`stage_action: "finalize"`, and
`artifact_path: "docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md"`.
That accepted `G3` artifact confirms the exact already-reverified lane for
decision: the repaired `URI-R2-C1` local `rootLocalMultiInst` /
`targetC -> rootFinal` `G2` / `G3` slice. `G4` must aggregate exactly that
accepted evidence chain into one bounded next-step result; it is not a new
implementation slice and not a new verification target.

Running `G4` now preserves the continue-bounded ordering while keeping the live
subject fixed to repaired `URI-R2-C1` and the inherited explicit-only /
non-equi-recursive / non-cyclic-graph boundary. This selection does not choose
widened work. `G4` must decide exactly one bounded result grounded in the
accepted `G3` evidence chain: `continue-bounded`, `widen-approved`, or
`stop-blocked`. It may not reopen `G1` selection, `G2` implementation, `G3`
verification, replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget`
widening, `instArgRootMultiBase`, non-local widening, equi-recursive
reasoning, cyclic structural encoding, a second executable interface, or
compatibility / convenience fallbacks.

`Bugs.md` has no open bug that overrides this ordering; the only listed replay
bug is resolved continuity context and does not authorize replay-lane reopen
here. Current repository status is non-pristine because controller state,
historical untracked round-047 selection/plan copies, and the active task
packet remain present, but that does not change the next lawful roadmap
choice. The final bounded `G4` decision gate is the immediate successor to the
accepted `G3` verification record.

## Round Scope Guard

- This round is limited to roadmap item `G4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary
  unless the roadmap is explicitly amended first.
- Treat the accepted `G3` local `rootLocalMultiInst` / `targetC -> rootFinal`
  lane as inherited aggregate-decision input only; do not reopen or extend it.
- Record exactly one bounded next-step result for that already-reverified lane:
  `continue-bounded`, `widen-approved`, or `stop-blocked`.
- Preserve the frozen `src/MLF/Elab/Run/ResultType/Fallback.hs` /
  `test/PipelineSpec.hs` ownership and leave `instArgRootMultiBase`
  explicitly unselected for this cycle.
- Do not reopen the accepted `E2` / `E3`, `F1` / `F2` / `F3` / `F4`, or
  `G1` / `G2` / `G3` chain except as inherited context.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
  `boundVarTarget` widening, non-local binding widening, equi-recursive
  reasoning, cyclic structural encoding, a second executable interface, or a
  compatibility or convenience fallback path in this round.
