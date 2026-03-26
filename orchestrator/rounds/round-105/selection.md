# Round 105 Selection

Date: 2026-03-26
Round: `round-105`
Role: guider
Active subject: rev-002 reopened global `non-cyclic-graph` same-family
roadmap after the accepted item-1 authority and candidate-boundary freeze

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-002`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-105` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `2`: select the exact live reopened subject inside the
candidate boundary.

## Why This Item Should Run Now

`orchestrator/state.json` now fixes the live controller state for this packet
at `active_round_id: "round-105"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-104"`, and
`retry: null`. No same-round retry object exists, so the retry contract does
not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The active roadmap bundle remains `rev-002`. After accepted `round-104`, its
item list now records item `1` as `done` and items `2` through `5` as
`pending`. Item `2` depends only on item `1`, which is now satisfied by the
accepted authority-freeze artifact
`docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
and authoritative review
`orchestrator/rounds/round-104/review-record.json`. Item `3` still depends
on item `2`, item `4` depends on item `3`, and item `5` depends on item `4`.
The next lawful move is therefore the item-2 subject-selection round, not a
safety-contract round, audit-bind round, or lane-open-or-stop round.

Accepted `round-104` is the decisive predecessor handoff. Its authoritative
artifact froze rev-002 as planning-only and architecture-only, preserved
accepted rev-001 truth, explicitly kept rev-001 items `6` through `8`
blocked, and froze exactly one planning-only candidate boundary:
whether one bounded single-component cyclic-structure successor lane is
needed for the strongest admitted retained-child / public-output continuity
pressure, with non-local `C1` continuity available only as contrast context.
That freeze deliberately did not select the exact live reopened subject
inside the boundary. Item `2` exists precisely to make that narrower choice
without widening into multiple subjects or broader family search.

The rev-002 roadmap text makes the selection constraints explicit. Item `2`
must choose exactly one next live architecture-pressure subject inside the
frozen boundary, grounded in the accepted representative matrix. The chosen
subject must stay concrete and bounded to the strongest admitted retained-
child / public-output continuity route, with non-local `C1` continuity as
contrast context only unless the accepted selection says otherwise. The item
must fail closed rather than opening multiple packets, multiple lanes, or
unrelated architecture routes.

The accepted predecessor evidence chain still constrains that choice. The
inherited baseline remains explicit-only, iso-recursive only, non-equi-
recursive, no-fallback, and non-production. The accepted representative
matrix still reads `bounded subset only`, still has zero `stable visible
persistence` rows, still carries `C1`, `C2`, `C5`, and `C7` as blocker debt,
and still leaves `N4` as architecture-pressure context only. The accepted
same-lane retained-child continuity chain keeps the strongest admitted
pressure on the exact `C2` / `C5` / `C7` route, while the accepted item-1
freeze keeps non-local `C1` as contrast only. Item `2` therefore must pick
the exact live reopened subject inside that single same-family boundary,
rather than re-opening whether the boundary exists at all.

## Round Scope Guard

- This round is limited to roadmap item `2` only.
- The output must be docs-only and must preserve the accepted item-1
  authority freeze, including the one frozen candidate boundary and the rule
  that rev-001 items `6` through `8` remain blocked.
- The output must choose exactly one next live architecture-pressure subject
  inside that boundary.
- The output must stay concrete and bounded to the strongest admitted same-
  family retained-child / public-output continuity route, with non-local
  `C1` continuity available only as contrast context unless the accepted
  selection says otherwise.
- The output must fail closed rather than selecting multiple subjects,
  multiple packets, unrelated routes, or broader family search.
- The output must keep equi-recursive semantics, multi-SCC search, second
  interfaces, fallback widening, and production implementation blocked.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the exact live reopened subject has not yet been selected until item `2`
  lands;
- the safety / acceptance contract for the selected subject cannot be
  defined until item `3`;
- the exact audit target and evaluation surface cannot be bound until
  item `4`; and
- the architecture-amendment-lane decision cannot occur until item `5`.
