# Round 104 Selection

Date: 2026-03-26
Round: `round-104`
Role: guider
Active subject: global `non-cyclic-graph` reopened same-family roadmap
revision after the accepted `round-103` rev-001 item-5 decision gate

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-002`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-104` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `1`: freeze the reopened `non-cyclic-graph` revision authority
and candidate boundary.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-104"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-103"`, and
`retry: null`. No live same-round retry object exists, so the retry contract
does not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The live roadmap bundle is now `rev-002`, not `rev-001`. Its item list records
items `1` through `5` as `pending`, and item `1` is the only unfinished item
with no dependency. Item `2` depends on item `1`, item `3` depends on item
`2`, item `4` depends on item `3`, and item `5` depends on item `4`. The next
lawful move is therefore the item-1 freeze, not a narrower subject-selection
round and not any implementation or capability-claim work.

Accepted `round-103` is the decisive predecessor handoff. Its authoritative
review record finalizes rev-001 item `5` with exactly one outcome:
`reopen the non-cyclic-graph revision question`. That accepted result
preserved `iso-recursive = keep`, `non-equi-recursive = keep`, and
`no-fallback = keep`, but it explicitly did not authorize cyclic search,
multi-SCC search, second interfaces, fallback widening, or production
implementation. It also explicitly withdrew the direct handoff from rev-001
item `5` into rev-001 items `6` through `8`. The new revision therefore has
to begin by freezing what the reopened question is allowed to mean before any
later item can lawfully narrow or evaluate it.

The rev-002 roadmap text makes that requirement explicit. This revision is
planning-only and architecture-only. Its job is to reopen the inherited
`non-cyclic-graph` boundary in one bounded way, determine whether one minimal
same-family successor lane should exist, and keep all later work concrete
before any code-changing or capability-claim stage can resume. Item `1` is
the stage that freezes that authority and candidate boundary. Without it,
item `2` could silently widen into multiple live subjects, and later stages
could drift into broader cyclic-search, multi-SCC, second-interface, or
fallback territory that the accepted record still blocks.

The accepted evidence chain also points directly to this freeze. The inherited
baseline contract still keeps the explicit-only / iso-recursive /
non-equi-recursive / non-cyclic-graph / no-fallback boundary live. The March
25 capability contract still requires representative success across `P1`
through `P6` and bounded or fail-closed handling for `N1` through `N6`, with
`N4` reserved for cases that would require cyclic graphs or multi-SCC search.
The full-pipeline contract still makes `stable visible persistence` the only
lawful positive `P6` token. The accepted representative-family matrix still
records zero `stable visible persistence` rows, keeps `C1`, `C2`, `C5`, and
`C7` below repo-level success as blocker debt, and leaves `N4` as the only
live architecture-pressure axis. The same-lane retained-child continuity chain
remains the strongest admitted positive-family pressure, but its exact-pocket
item-5 gate still concluded only `blocker debt remains within the current
architecture` for that one pocket. Rev-002 item `1` is therefore needed to
freeze the reopened global question to one planning-only candidate boundary:
whether one bounded single-component cyclic-structure successor lane is needed
for that strongest admitted retained-child / public-output continuity pressure,
with non-local `C1` continuity available only as contrast context.

`Bugs.md` does not change that ordering. `BUG-2026-03-16-001` remains open
predecessor replay context only, and nothing in the bug tracker authorizes
roadmap reordering, direct implementation work, or widening beyond the bounded
reopened family.

## Round Scope Guard

- This round is limited to roadmap item `1` only.
- The output must be docs-only and must preserve accepted rev-001 truth,
  especially the accepted `round-103` reopen outcome and the rule that
  rev-001 items `6` through `8` stay blocked.
- The output must freeze exactly one planning-only candidate boundary:
  whether one bounded single-component cyclic-structure successor lane is
  needed for the strongest admitted retained-child / public-output continuity
  pressure.
- The output must explicitly keep equi-recursive semantics, multi-SCC search,
  second interfaces, fallback widening, and production implementation blocked.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the reopened revision authority is not yet frozen until item `1` lands;
- the exact live reopened subject cannot be selected until item `2`;
- the safety / acceptance contract for the reopened subject cannot be defined
  until item `3`;
- the exact audit target and evaluation surface cannot be bound until item `4`;
  and
- the architecture-amendment-lane decision cannot occur until item `5`.
