# Round 054 Selection

Date: 2026-03-20
Round: `round-054`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 21: execute the `I1` continue-bounded bind and exact next-slice
target selection for repaired `URI-R2-C1` after the accepted
`H4 = continue-bounded` decision for the local-binding
`rootLocalInstArgMultiBase` `H2` / `H3` baseline.

## Why This Item Should Run Now

`orchestrator/state.json` already parks the live control plane at
`active_round_id: "round-054"`, `active_round_dir:
"orchestrator/rounds/round-054"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and `last_completed_round:
"round-053"`. That machine state means there is no same-round retry to resume
and no earlier interrupted stage that can lawfully run ahead of fresh roadmap
selection.

`orchestrator/roadmap.md` marks items 1 through 20 done and leaves item 21
(`I1`) as the lowest-numbered unfinished roadmap entry. Under the guider
contract, that makes `I1` the next lawful selection. No later stage may run
first because any successor implementation, verification, decision, or
roadmap-update work depends on `I1` first binding one exact bounded next
slice.

The accepted predecessor chain fixes both the timing and the immediate scope
for this round. `orchestrator/rounds/round-053/review-record.json` finalized
`H4` as authoritative with `stage_id: "H4"`, `attempt_verdict: "accepted"`,
`stage_action: "finalize"`, and `artifact_path:
"docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md"`. That
accepted `H4` artifact records result token `continue-bounded` for the
accepted repaired `URI-R2-C1` local
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane and explicitly says
that any successor work must begin with a fresh bounded exact-target bind
rather than silently widening or reopening accepted prior stages. That makes
`I1` the lawful entry point for the next bounded cycle, not direct production
work and not a silent continuation of the completed `H1` / `H2` / `H3` / `H4`
chain.

The inherited predecessor evidence also keeps the subject boundary narrow. The
completed recursive-types packet under
`tasks/todo/2026-03-11-recursive-types-orchestration/` remains predecessor
context only: explicit recursive types are accepted on the explicit-only path,
while automatic recursive-type inference remains disabled and incomplete. The
accepted `U2`, `U3`, and `U4` negative findings remain binding as inherited
context, so `I1` must stay inside repaired `URI-R2-C1` and the explicit-only /
non-equi-recursive / non-cyclic-graph boundary.

`Bugs.md` in this round worktree still lists open bug `BUG-2026-03-16-001` at
`MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), and the older replay
repair evidence remains relevant predecessor context. But roadmap item 21's
own completion rule explicitly says the accepted `H2` / `H3`
`rootLocalInstArgMultiBase` evidence may not be treated as clearance for
replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget`, non-local
widening, or any cross-family widening. So the open bug does not let this
round skip the bind stage or silently preselect a widened family. The next
lawful step is still `I1` itself: bind exactly one successor slice first, with
its authority stated explicitly in the `I1` artifact rather than inferred from
the completed `H` lane.

Current repository status shows only the expected controller-state preparation
(`M orchestrator/state.json`) and no conflicting implementation diff. That
status does not override roadmap ordering. Selecting the docs-only `I1`
bind/selection step therefore advances the live round without rewriting
accepted history or touching unrelated work.

## Round Scope Guard

- This round is limited to roadmap item `I1` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary
  unless the roadmap is explicitly amended first.
- Treat `I1` as a bind-and-selection stage only: freeze exactly one next
  bounded non-widening successor slice under repaired `URI-R2-C1`, with future
  ownership limited to exactly the files named by the accepted `I1` bind.
- Ground the bind in the accepted `round-053` `H4` decision, the accepted
  `H1` / `H2` / `H3` / `H4` chain, relevant predecessor evidence, and current
  bug/roadmap context; do not reopen `H1`, `H2`, `H3`, or `H4` as live work.
- Do not treat the accepted local `rootLocalInstArgMultiBase` evidence as
  automatic clearance for replay reopen, `MLF.Elab.Inst` / `InstBot`,
  `boundVarTarget`, non-local widening, or any cross-family widening. Any
  chosen successor slice must be justified explicitly inside `I1`.
- Do not treat this round as implementation, verification, merge, or
  roadmap-update authority. No production-code edits, no test edits, no second
  executable interface, no compatibility or convenience fallback path, no
  equi-recursive reasoning, and no cyclic structural encoding belong in this
  selection stage.
