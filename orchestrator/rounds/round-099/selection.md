# Round 099 Selection

Date: 2026-03-26
Round: `round-099`
Role: guider
Active subject: global `non-cyclic-graph` settlement and automatic
iso-recursive inference roadmap family after the accepted bounded
same-lane retained-child public-output continuity decision in
`round-098`

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit plus preexisting round-099 recovery
  artifacts (`git status --short` returned
  `M orchestrator/state.json` and `?? orchestrator/rounds/round-099/`)

## Selected Roadmap Item

Roadmap item `1`: freeze the global `non-cyclic-graph` settlement contract
and unresolved family evidence ledger.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-099"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-098"`, and
`retry: null`. The preserved `resume_error` explains why the earlier built-in
guider delegation failed to produce `selection.md`, but it does not create a
same-round retry object and therefore does not change roadmap order. Under the
active retry contract, the default rule still applies: select the
lowest-numbered unfinished roadmap item unless live retry state says
otherwise.

The active roadmap bundle at
`orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`
marks every item `1` through `8` as pending. Item `1` has no dependencies and
is therefore the first lawful next step. Items `2` through `5` depend on this
contract freeze, and items `6` through `8` are explicitly blocked until item
`5` records the global settlement outcome. Selecting anything later would skip
the roadmap order and would violate the stage boundary that keeps work before
item `5` limited to settlement evidence only.

The accepted predecessor chain explains why the new roadmap must restart with
a global settlement-contract freeze instead of jumping into production work.
Accepted `round-098` recorded one exact-pocket outcome only:
`blocker debt remains within the current architecture` for the frozen
same-lane retained-child public-output continuity case. The active roadmap
context makes the limitation explicit: rounds `round-094` through `round-098`
remain authoritative predecessor evidence, but they do not globally settle
`non-cyclic-graph` by themselves. The immediate predecessor decision gate at
`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
also kept `reopen the non-cyclic-graph revision question` unselected for that
one bounded pressure point only, not for the repo-level family matrix.

The general capability contract and strategic predecessor artifacts still
leave repo-level settlement unresolved. The accepted capability corpus in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
fixes representative positive families `P1` through `P6` and required
negative or bounded families including `N1`, `N2`, and `N6`. The accepted
architectural audit in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
still classifies `non-cyclic-graph = unknown` at repo scope. The accepted
representative campaign in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
remains only `bounded subset only`, with zero
`stable visible persistence` rows and unresolved pressure across the
representative family matrix. The accepted full-pipeline contract in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
still requires production-surface continuity across solver, reconstruction,
internal output, and public output before a positive `P6` success read is
lawful. That unresolved strategic posture is exactly what item `1` must freeze
before later evidence rounds can proceed honestly.

The inherited baseline remains unchanged while item `1` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still keeps the explicit-only / iso-recursive / non-equi-recursive /
non-cyclic-graph / no-fallback boundary live. The open
`BUG-2026-03-16-001` entry in `Bugs.md` remains predecessor replay context
only. It does not create a retry obligation here and does not authorize
leaving roadmap order or widening into implementation, cyclic search,
multi-SCC search, second interfaces, fallback paths, or a broad capability
claim.

## Round Scope Guard

- This round is limited to roadmap item `1` only.
- The output must freeze the exact repo-level acceptance bar for
  `non-cyclic-graph = keep` versus
  `reopen the non-cyclic-graph revision question`.
- The output must freeze the unresolved family evidence ledger across the
  representative positive families `P1` through `P6` and the required
  negative or bounded families, preserving the accepted production-surface
  evidence bar.
- The output must name the accepted predecessor evidence that later rounds
  may consume, while making explicit that accepted rounds `round-094`
  through `round-098` are bounded exact-pocket evidence only, not global
  settlement by themselves.
- Do not widen into item `2` / `3` / `4` proof slices, item `5` global
  settlement, production implementation, hardening, capability claims,
  cyclic search, multi-SCC search, second interfaces, or fallback widening.
- Do not edit `orchestrator/state.json`, roadmap machine metadata, or
  production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- controller-owned `resume_error` remains recorded in `orchestrator/state.json`
  from the earlier missing-artifact delegation failure, but the exact round,
  branch, worktree, and stage remain preserved for this resumed
  `select-task` write;
- repo-level `non-cyclic-graph` settlement is still unresolved because the
  accepted record contains bounded predecessor wins and bounded blocker debt,
  not a representative global settlement freeze;
- representative family-matrix evidence is still incomplete at repo scope,
  especially across `P2` through `P6` and the required negative or bounded
  families; and
- `BUG-2026-03-16-001` remains open predecessor replay context only, not
  authority to skip the item-1 settlement-contract freeze.
