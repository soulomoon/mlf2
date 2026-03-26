# Round 106 Selection

Date: 2026-03-26
Round: `round-106`
Role: guider
Active subject: rev-002 reopened global `non-cyclic-graph` same-family
roadmap after the accepted exact live reopened-subject selection

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-002`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-106` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `3`: establish the safety and acceptance contract for the
selected reopened subject.

## Why This Item Should Run Now

`orchestrator/state.json` now fixes the live controller state for this packet
at `active_round_id: "round-106"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-105"`, and
`retry: null`. No same-round retry object exists, so the retry contract does
not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The active roadmap bundle remains `rev-002`. After accepted `round-105`, its
item list now records items `1` and `2` as `done` and items `3` through `5`
as `pending`. Item `3` depends only on item `2`, which is now satisfied by
the accepted subject-selection artifact
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`
and authoritative review
`orchestrator/rounds/round-105/review-record.json`. Item `4` still depends
on item `3`, and item `5` depends on item `4`. The next lawful move is
therefore the item-3 safety-contract round, not an audit-bind round or a
lane-open-or-stop round.

Accepted `round-105` is the decisive predecessor handoff. Its authoritative
artifact selected exactly one live reopened subject inside the frozen rev-002
boundary:
the same exact same-lane retained-child / public-output continuity pocket
already carried by accepted rows `C2`, `C5`, and `C7`, with non-local `C1`
retained only as contrast context. That accepted selection preserved rev-002
as planning-only and architecture-only, kept rev-001 items `6` through `8`
blocked, and explicitly deferred the safety / acceptance contract to item
`3`.

The rev-002 roadmap text makes the next responsibility explicit. Item `3`
must define the evidence bar for the selected reopened subject, including
what would count as:
`acyclic still sufficient`,
`single-component cyclic-structure successor lane justified`, or
`stop without opening an architecture-amendment lane`.
That contract must preserve `iso-recursive = keep`,
`non-equi-recursive = keep`, and `no-fallback = keep`, and it must keep
multi-SCC search, second interfaces, fallback behavior, and production
implementation blocked. Without item `3`, later item `4` would have no
lawful acceptance bar to bind against, and item `5` would have no lawful
decision schema.

The accepted predecessor chain still constrains that contract. The inherited
baseline remains explicit-only, iso-recursive only, non-equi-recursive,
non-production, and no-fallback. The selected subject remains one same-lane
`C2` / `C5` / `C7` pocket only. `C7` keeps the public-output continuity
pressure visible on the same exact route, while `C1` stays contrast only.
Item `3` therefore must define the safety bar for that exact selected
subject, not re-open subject selection, not broaden the family, and not
imply that any architecture amendment is already justified.

## Round Scope Guard

- This round is limited to roadmap item `3` only.
- The output must be docs-only and must preserve the accepted item-1
  authority freeze and accepted item-2 subject selection, including the rule
  that rev-001 items `6` through `8` remain blocked.
- The output must define the safety and acceptance contract for the one
  selected reopened subject only.
- The output must preserve `iso-recursive = keep`,
  `non-equi-recursive = keep`, and `no-fallback = keep`.
- The output must define reviewer-auditable outcomes for
  `acyclic still sufficient`,
  `single-component cyclic-structure successor lane justified`, or
  `stop without opening an architecture-amendment lane`.
- The output must keep multi-SCC search, second interfaces, fallback
  widening, and production implementation blocked.
- The output must not re-open subject selection, bind the exact audit
  target, or decide whether the lane opens or stops.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the reviewer-auditable safety / acceptance contract does not exist until
  item `3` lands;
- the exact audit target and evaluation surface cannot be bound until
  item `4`; and
- the architecture-amendment-lane decision cannot occur until item `5`.
