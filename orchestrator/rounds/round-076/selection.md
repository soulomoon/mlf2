# Round 076 Selection

Date: 2026-03-23
Round: `round-076`
Role: guider
Active subject: choose the next bounded live planning subject inside the
accepted `N8 = reopen-planning-only-successor-lane`
Successor lane: docs-only thesis-backed next live-subject selection without
reopening the accepted non-local `baseTarget -> baseC` packet as live work

## Selected Roadmap Item

Roadmap item 9 (`N9`): execute the thesis-backed next live-subject selection
inside the accepted `N8 = reopen-planning-only-successor-lane`.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` fixes the
live controller state at `active_round_id: "round-076"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-076-n9-next-live-subject-selection"`, `worktree_path:
"/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-076"`, and
`last_completed_round: "round-075"`. That is a fresh round-selection state,
not a same-round retry. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
therefore supplies no live retry obligation that would override ordinary
lowest-unfinished-item selection.

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` marks items
1 through 8 done and item 9 pending. Item 9 depends only on item 8, and that
dependency is satisfied by accepted `round-075`:
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-075/review-record.json`
finalizes `stage_id: "N8"` with `attempt_verdict: "accepted"`,
`stage_result: "pass"`, `stage_action: "finalize"`, `status:
"authoritative"`, and `final_outcome:
"reopen-planning-only-successor-lane"`.

That accepted `N8` result is exactly the authority item 9 must consume.
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
states that the exact accepted non-local `baseTarget -> baseC` packet remains
predecessor evidence only, authorizes exactly one fresh docs-first
successor-planning lane for later bounded next live-subject selection only,
and still does not choose the next live subject, bind an exact next target,
authorize implementation, or authorize verification. Selecting item 9 follows
that accepted sequencing directly.

`/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
matches the same boundary. It records the earlier `N2` / `N3` / `N4` / `N5` /
`N6` chain as predecessor evidence only after accepted `N8`, keeps the
long-horizon `N7` row at `NO`, and says the next lawful work is a later
accepted bounded next-subject selection before any fresh target bind or slice
can begin. Item 9 is the roadmap entry that performs exactly that selection.

The inherited baseline and predecessor closure remain unchanged.
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
and
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
still preserve the explicit-only / non-equi-recursive /
non-cyclic-graph / no-second-interface / no-fallback boundary and require
fresh accepted authority before any broader preserved route becomes live
work. Choosing implementation, verification, replay reopen, or exact target
binding here would contradict those accepted boundaries and the accepted `N8`
outcome.

`/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` still lists open
`BUG-2026-03-16-001`, but that replay / `InstBot` defect remains predecessor
context only for this round. It does not force a same-round retry and does
not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, or a wider live
subject. Repository status shows only the controller-owned
`M orchestrator/state.json` change in the controller root and a clean active
round worktree on `codex/round-076-n9-next-live-subject-selection`, so no
repository-state blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `N9` only.
- Keep the round docs-only and selection-only: write one bounded next
  live-subject selection artifact, not an implementation plan, code change,
  verification run, review output, merge note, roadmap edit, state edit, or
  bug-tracker edit.
- Select exactly one thesis-backed bounded next live subject for the reopened
  planning-only successor lane and explicitly defer every non-selected route.
- Preserve accepted `L1`, `L2`, `N1`, `N2`, `N3`, `N4`, `N5`, `N6`, `N7`, and
  `N8` as binding predecessor evidence.
- Treat the accepted non-local `baseTarget -> baseC` packet as predecessor
  evidence only; do not treat it as still-live implementation or verification
  work.
- Keep exact target binding, implementation, verification, replay reopen,
  `MLF.Elab.Inst`, `InstBot`, accepted local lanes, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`,
  `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/View.hs`,
  every other fallback family, every different solver/pipeline subject,
  cross-family search, equi-recursive reasoning, implicit unfolding, cyclic
  encoding, multi-SCC support, second-interface work, and fallback widening
  blocked unless named only as still-blocked future work.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unchanged.

## Blockers

No live controller blocker or retry obligation is present.

Active boundaries that must remain blockers rather than reopened work:

- accepted `N8 = reopen-planning-only-successor-lane` authorizes only
  docs-first next-subject selection at this step, not exact binding,
  implementation, or verification;
- open `BUG-2026-03-16-001` remains replay-only predecessor context and
  cannot be reopened as live implementation work in this round; and
- any path that silently carries forward the old `baseTarget -> baseC`
  packet as the already-chosen live subject, or otherwise widens beyond one
  bounded docs-only selection, would block acceptance rather than justify
  expansion.
