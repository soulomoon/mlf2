# Round 079 Selection

Date: 2026-03-24
Round: `round-079`
Role: guider
Active subject: land one bounded implementation slice for the exact
`N11`-frozen same-lane retained-child packet
Successor lane: selected same-lane local `TypeRef` retained-child
`boundVarTarget -> targetC` packet only, with `schemeBodyTarget` kept as
neighboring boundary context and every broader route still blocked

## Selected Roadmap Item

Roadmap item 12 (`N12`): execute the bounded implementation slice for the
exact `N11`-frozen same-lane retained-child `boundVarTarget -> targetC`
packet.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` fixes the
live controller state at `active_round_id: "round-079"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-079-n12-bounded-implementation-slice"`, `worktree_path:
"/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079"`, and
`last_completed_round: "round-078"`. This is a fresh round-selection state,
not a same-round retry. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
therefore supplies no live retry obligation that overrides ordinary
lowest-unfinished-item selection.

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` marks items
1 through 11 done and item 12 pending. Item 12 is therefore the
lowest-numbered unfinished roadmap item, and its only dependency is item 11.
That dependency is satisfied by accepted `round-078`:
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-078/review-record.json`
finalizes `stage_id: "N11"` with `attempt_verdict: "accepted"`,
`stage_result: "pass"`, `stage_action: "finalize"`, `status:
"authoritative"`, and `final_outcome:
"boundVarTarget-exact-target-bind-established"`.

That accepted `N11` result is exactly the authority item 12 must consume.
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
freezes exactly one bounded live packet: the same-lane local `TypeRef`
retained-child `boundVarTarget -> targetC` route in
`src/MLF/Elab/Run/ResultType/Fallback.hs`, with `boundVarTargetRoot`,
`boundHasForallFrom`, `boundVarTarget`, `keepTargetFinal`, the downstream
`Just v -> v` `targetC` selection, and the retained-child
`PipelineSpec` evidence as the controlling anchors. That accepted artifact
also states that future work still requires a later accepted bounded
implementation slice and later verifier-owned evidence before any broader
claim can be made. Selecting item 12 follows that accepted sequencing
directly.

The pending item-12 completion notes in
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` further fix
the exact bounded job for this round: one accepted minimal code/test slice
must land only for the selected packet in
`src/MLF/Elab/Run/ResultType/Fallback.hs` plus focused retained-child
coverage in `test/PipelineSpec.hs`, keep `boundVarTargetRoot`,
`boundHasForallFrom`, `boundVarTarget`, `keepTargetFinal`, and the
downstream `Just v -> v` `targetC` selection aligned with accepted `N11`,
preserve fail-closed nested-`forall` / nested-owner / nested scheme-root
conditions, keep `schemeBodyTarget` as neighboring boundary context only,
and explicitly preserve every unchanged blocked route. That is bounded
implementation work only; it is not verification, a next-cycle decision, or
authority to widen the live subject.

`/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
matches the same lawful sequence. It records that the fresh post-`N8`
successor lane required one accepted exact bounded target bind before any new
implementation or verification slice could begin. Accepted `round-078`
supplies that missing exact bind for the selected `boundVarTarget` lane, so
item 12 is now the first lawful executable step while item 13 verification
and item 14 next-cycle decision remain downstream and blocked until an
accepted `N12` slice exists.

The inherited baseline and repaired-queue closure remain unchanged.
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
and
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
still preserve the explicit-only / non-equi-recursive /
non-cyclic-graph / no-second-interface / no-fallback boundary and require
fresh accepted authority before any broader preserved route becomes live
work. Selecting verification, replay reopen, `MLF.Elab.Inst`, `InstBot`,
`boundTarget`, `schemeBodyTarget` as a live subject, `rootFinal`, another
fallback family, or a wider solver/pipeline route here would contradict
those accepted boundaries and the accepted `N11` outcome.

`/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` still lists open
`BUG-2026-03-16-001`, but that replay / `InstBot` defect remains predecessor
context only for this round. It does not force a retry and does not
authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, or a wider live
subject. Repository status in the controller root is already non-pristine due
to unrelated task/doc updates plus the controller-owned
`M orchestrator/state.json` change, while the active round worktree on
`codex/round-079-n12-bounded-implementation-slice` shows only that
controller-owned `M orchestrator/state.json` entry. No repository-state
blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `N12` only.
- Keep the round implementation-only and exact-packet-only: land one bounded
  code/test slice for the frozen same-lane retained-child packet, not a
  verification artifact, next-cycle decision, roadmap edit, state edit,
  bug-tracker edit, or predecessor-history rewrite.
- Preserve accepted `N9 = boundVarTarget-planning-subject-selected`,
  `N10 = boundVarTarget-safety-acceptance-contract-established`, and
  `N11 = boundVarTarget-exact-target-bind-established`.
- Limit code/test changes to the selected packet in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/src/MLF/Elab/Run/ResultType/Fallback.hs`
  plus focused retained-child coverage in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/test/PipelineSpec.hs`.
- Keep `boundVarTargetRoot`, `boundHasForallFrom`, `boundVarTarget`,
  `keepTargetFinal`, and the downstream `Just v -> v` `targetC` choice
  aligned with accepted `N11`; keep nested-`forall`, nested-owner, and
  nested scheme-root crossings fail-closed.
- Treat `schemeBodyTarget` as neighboring boundary context only, and treat
  the earlier `baseTarget` lane, the exact accepted non-local
  `baseTarget -> baseC` packet, and the accepted repaired-queue retained-child
  packet as predecessor evidence only.
- Keep verification, replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundTarget`, `schemeBodyTarget` as a live subject,
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/src/MLF/Elab/Run/ResultType/View.hs`,
  every other fallback family, every different solver/pipeline subject,
  cross-family search, implicit unfolding, equi-recursive reasoning or
  equality, cyclic structural graph encoding, graph-cycle exceptions,
  multi-SCC support, second-interface work, and fallback widening blocked
  unless named only as still-blocked future work.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unchanged.

## Blockers

No live controller blocker or retry obligation is present.

Active boundaries that must remain blockers rather than reopened work:

- accepted `N11` authorizes only one bounded implementation slice for the
  exact same-lane retained-child `boundVarTarget -> targetC` packet at this
  step, not verification, next-cycle decision, or widening;
- open `BUG-2026-03-16-001` remains replay-only predecessor context and
  cannot reopen replay, `MLF.Elab.Inst`, or `InstBot` as live work in this
  round; and
- any path that promotes `schemeBodyTarget`, `boundTarget`, `rootFinal`, the
  old `baseTarget` lane, the exact accepted non-local `baseTarget -> baseC`
  packet, the repaired-queue retained-child packet, or any other fallback
  family into still-live authority would block acceptance rather than justify
  expansion.
