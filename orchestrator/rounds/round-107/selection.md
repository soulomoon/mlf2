# Round 107 Selection

Date: 2026-03-26
Round: `round-107`
Role: guider
Active subject: rev-002 reopened global `non-cyclic-graph` same-family
roadmap after the accepted selected-subject safety contract

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-002`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-107` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `4`: bind the exact architecture-pressure audit target and
evaluation surface for the selected subject.

## Why This Item Should Run Now

`orchestrator/state.json` now fixes the live controller state for this packet
at `active_round_id: "round-107"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-106"`, and
`retry: null`. No same-round retry object exists, so the retry contract does
not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The active roadmap bundle remains `rev-002`. After accepted `round-106`, its
item list now records items `1` through `3` as `done` and items `4` and `5`
as `pending`. Item `4` depends only on item `3`, which is now satisfied by
the accepted contract artifact
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-safety-and-acceptance-contract.md`
and authoritative review
`orchestrator/rounds/round-106/review-record.json`. Item `5` still depends
on item `4`. The next lawful move is therefore the item-4 audit-bind round,
not the final lane-open-or-stop decision round.

Accepted `round-106` is the decisive predecessor handoff. Its authoritative
artifact defined exactly three lawful outcome bars for one selected subject
only and explicitly deferred the exact audit bind to item `4`. The selected
subject remains the same exact same-lane retained-child / public-output
continuity pocket carried by accepted rows `C2`, `C5`, and `C7`, with
non-local `C1` retained only as contrast context. Item `4` exists to freeze
the exact rows, packet, modules, commands, and review-visible output
surfaces that later work may inspect against that contract, without
selecting an outcome and without widening the subject.

The rev-002 roadmap text makes the binding constraints explicit. Item `4`
must freeze the exact accepted rows, packets, modules, and review-visible
output surfaces that later work may inspect for the selected reopened
subject. The bind must stay within one bounded same-family successor lane,
must name exact continuity obligations carried forward from the accepted
`C2` / `C5` / `C7` chain and any accepted `C1` contrast use, and must not
authorize code changes by itself.

The accepted predecessor chain still constrains that bind. The inherited
baseline remains explicit-only, iso-recursive only, non-equi-recursive,
non-production, and no-fallback. The selected subject remains one same-lane
`C2` / `C5` / `C7` pocket only, and item `3` now supplies the exact lawful
outcome bars. Item `4` therefore must bind the exact audit target for that
one selected subject only, not broaden the family, not choose among the
three outcome bars, and not imply that an architecture-amendment lane is
already opened.

## Round Scope Guard

- This round is limited to roadmap item `4` only.
- The output must be docs-only and must preserve accepted items `1` through
  `3`, including the rule that rev-001 items `6` through `8` remain
  blocked.
- The output must bind the exact rows, packet, modules, commands, and
  review-visible output surfaces for the one selected reopened subject only.
- The output must stay within the selected same exact `C2` / `C5` / `C7`
  pocket, with non-local `C1` contrast use only if explicitly bounded.
- The output must not reopen subject selection, choose an item-3 outcome,
  or decide whether item `5` opens a lane or stops.
- The output must preserve `iso-recursive = keep`,
  `non-equi-recursive = keep`, and `no-fallback = keep`.
- The output must keep multi-SCC search, second interfaces, fallback
  widening, and production implementation blocked.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the exact audit target and evaluation surface do not exist until item `4`
  lands; and
- the architecture-amendment-lane decision cannot occur until item `5`.
