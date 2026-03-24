# Round 078 Selection

Date: 2026-03-24
Round: `round-078`
Role: guider
Active subject: freeze one docs-only exact bounded target inside accepted
`N9 = boundVarTarget-planning-subject-selected`
Successor lane: selected retained-child / nested-`forall` /
binding-structure `boundVarTarget` planning subject governed by accepted
`N10`

## Selected Roadmap Item

Roadmap item 11 (`N11`): execute the exact bounded target bind for the
selected `boundVarTarget` planning subject under the accepted `N10`
contract.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` fixes the
live controller state at `active_round_id: "round-078"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-078-n11-exact-target-bind"`, `worktree_path:
"/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-078"`, and
`last_completed_round: "round-077"`. This is a fresh round-selection state,
not a same-round retry. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
therefore supplies no live retry obligation that overrides ordinary
lowest-unfinished-item selection.

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` marks items
1 through 10 done and item 11 pending. Item 11 is therefore the
lowest-numbered unfinished roadmap item, and its only dependency is item 10.
That dependency is satisfied by accepted `round-077`:
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-077/review-record.json`
finalizes `stage_id: "N10"` with `attempt_verdict: "accepted"`,
`stage_result: "pass"`, `stage_action: "finalize"`, `status:
"authoritative"`, and `final_outcome:
"boundVarTarget-safety-acceptance-contract-established"`.

That accepted `N10` result is exactly the authority item 11 must consume.
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-safety-acceptance-contract.md`
states that the selected retained-child / nested-`forall` /
binding-structure `boundVarTarget` lane now has one subject-specific
safety/acceptance contract, that any later exact target bind remains blocked
unless it satisfies every reviewer-visible acceptance criterion, and that the
next lawful work is that docs-only exact bounded target bind rather than
implementation or verification. Selecting item 11 follows that accepted
sequencing directly.

`/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
matches the same boundary. It records the earlier reopened `baseTarget` exact
bind and bounded slice as predecessor evidence only after accepted `N8`,
keeps the long-horizon `N7` row unresolved, and says the next action after
accepted `N10` is to define one exact bounded target for the selected
`boundVarTarget` lane before any implementation or verification slice can
begin. Item 11 is the roadmap entry that performs exactly that bind.

The pending item-11 completion notes in
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` further fix
the exact bounded job for this round: one accepted docs-only artifact must
freeze exactly one bounded target inside the selected retained-child /
nested-`forall` / binding-structure `boundVarTarget` route, satisfy every
accepted `N10` criterion, name the concrete retained-child ownership and
target-selection anchors (`boundVarTargetRoot`, `boundHasForallFrom`,
`boundVarTarget`, `keepTargetFinal`, `targetC`, and the retained-child
`PipelineSpec` evidence), state fail-closed nested-`forall` / nested-owner /
nested scheme-root conditions, keep `schemeBodyTarget` as neighboring
boundary context only, and explicitly preserve every unchanged blocked route.
That is exact-target-bind work only; it is not implementation or verification
authority.

The inherited baseline and repaired-queue closure remain unchanged.
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
and
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
still preserve the explicit-only / non-equi-recursive /
non-cyclic-graph / no-second-interface / no-fallback boundary and require
fresh accepted authority before any broader preserved route becomes live
work. Choosing implementation, verification, replay reopen, or any
non-selected fallback family here would contradict those accepted boundaries
and the accepted `N9` / `N10` outcomes.

`/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` still lists open
`BUG-2026-03-16-001`, but that replay / `InstBot` defect remains predecessor
context only for this round. It does not force a retry and does not authorize
replay reopen, `MLF.Elab.Inst`, `InstBot`, or a wider live subject.
Repository status in the controller root is already non-pristine due
unrelated task/doc updates plus the controller-owned
`M orchestrator/state.json` change, while the active round worktree on
`codex/round-078-n11-exact-target-bind` shows only that controller-owned
`M orchestrator/state.json` entry. No repository-state blocker forces a
different selection.

## Round Scope Guard

- This round is limited to roadmap item `N11` only.
- Keep the round docs-only and exact-target-bind only: write one bounded
  exact target bind artifact, not an implementation slice, verification
  artifact, review output, merge note, roadmap edit, state edit, or
  bug-tracker edit.
- Preserve accepted `N9 = boundVarTarget-planning-subject-selected` and
  `N10 = boundVarTarget-safety-acceptance-contract-established`.
- Treat the earlier `baseTarget` selection, the earlier `N3` contract, the
  exact accepted non-local `baseTarget -> baseC` packet, and the
  repaired-queue retained-child packet as predecessor evidence only.
- `N11` may bind exactly one bounded target only if it satisfies every
  accepted `N10` criterion: name the concrete retained-child ownership and
  target-selection anchors (`boundVarTargetRoot`, `boundHasForallFrom`,
  `boundVarTarget`, `keepTargetFinal`, `targetC`, and the retained-child
  `PipelineSpec` evidence), state concrete fail-closed nested-`forall` /
  nested-owner / nested scheme-root conditions, keep `schemeBodyTarget` as
  neighboring boundary context only, and name explicit out-of-scope
  exclusions for every unchanged blocked route.
- Keep implementation, verification, replay reopen, `MLF.Elab.Inst`,
  `InstBot`, `boundTarget`, `schemeBodyTarget`,
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-078/src/MLF/Elab/Run/ResultType/View.hs`,
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

- accepted `N10` authorizes only a subject-specific docs-first exact target
  bind at this step, not implementation or verification;
- open `BUG-2026-03-16-001` remains replay-only predecessor context and
  cannot reopen replay, `MLF.Elab.Inst`, or `InstBot` as live work in this
  round; and
- any path that silently carries forward the old `baseTarget` lane, the exact
  accepted non-local `baseTarget -> baseC` packet, or the repaired-queue
  retained-child packet as still-live authority, or that promotes
  `schemeBodyTarget`, `boundTarget`, or another fallback family into the live
  subject, would block acceptance rather than justify expansion.
