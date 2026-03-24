# Round 084 Selection

Date: 2026-03-25
Round: `round-084`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted roadmap item `2`
Successor lane: roadmap item `3` only, generalizing accepted bounded packet
history into a reusable mechanism map

## Selected Roadmap Item

Roadmap item 3: generalize the accepted packet history into a reusable
mechanism map.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/orchestrator/state.json`
fixes the live controller state at `active_round_id: "round-084"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-084-item-3-mechanism-map"`, `active_round_dir:
"orchestrator/rounds/round-084"`, and `last_completed_round: "round-083"`.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/orchestrator/retry-subloop.md`
only overrides roadmap order when `retry` is populated. `retry` is currently
`null`, so this is an ordinary selection step and the normal
lowest-numbered-unfinished-item rule still governs.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/orchestrator/roadmap.md`
marks items `1` and `2` done and items `3` through `7` pending. Item `3`
depends on items `1` and `2`, both of which were accepted in `round-082` and
`round-083`. Every later item depends on item `3` directly or indirectly.
Item `3` is therefore the lowest-numbered unfinished roadmap item and the
next lawful dependency-satisfied successor.

Accepted `round-083` finalized item `2` in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/orchestrator/rounds/round-083/review-record.json`
with `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"architectural-constraint-audit-completed-with-non-cyclic-graph-unknown"`.
The canonical artifact
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
records the bounded item-2 read: `iso-recursive = keep`,
`non-equi-recursive = keep`, `no-fallback = keep`, and
`non-cyclic-graph = unknown`. That accepted audit explicitly says the open
pressure now sits on whether representative `P2`-`P5` families can be
explained inside the inherited acyclic model without packet-specific
exceptions. Item `3` is the first roadmap item that owns exactly that
question.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
names the next strategic move as generalizing from packets to mechanisms:
stop choosing another convenient narrow lane and instead extract reusable
mechanism families such as recursive-shape discovery, binder/owner
placement, target/consumer alignment, local versus non-local propagation,
interaction with polymorphism and instantiation, reconstruction obligations,
and fail-closed ambiguity handling. That is exactly what the live roadmap
translates into item `3`. Running item `4` or later before this mechanism-map
step would pre-empt the missing evidence the accepted item-2 audit said is
still needed before search, reconstruction, coverage, or architecture
decisions can be justified.

The inherited baseline and predecessor continuity still make this bounded
mechanism-map step necessary rather than optional.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, non-cyclic structural encoding, and
no silent fallback widening. The accepted `N14` decision artifact at
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves one exact same-lane retained-child `boundVarTarget -> targetC`
packet as bounded predecessor evidence only, not as proof of general
capability and not as authority to widen architecture. Item `3` must
therefore explain accepted packet history through reusable mechanisms inside
the inherited boundary, or state which obligations remain missing, without
silently revising semantics, representation, interfaces, or fallback
behavior.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-084/Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor implementation context only and does not create a retry
obligation or change roadmap order. Repository status in the active worktree
shows only controller-owned `M orchestrator/state.json` drift. No
repository-state blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `3` only.
- Keep the round docs-only and bounded: extract reusable mechanism families
  from accepted bounded evidence rather than selecting another exact packet.
- Explain how multiple accepted packets do or do not fit a shared mechanism
  map covering recursive-shape discovery, binder/owner placement,
  target/consumer alignment, local versus non-local propagation, interaction
  with polymorphism and instantiation, reconstruction obligations, and
  fail-closed ambiguity handling.
- Use the mechanism map to test the open item-2 pressure point: whether
  representative `P2`-`P5` family pressure appears explainable inside the
  inherited acyclic (`non-cyclic-graph`) model without packet-specific
  exceptions.
- State which mechanism obligations remain missing for the acyclic model, and
  fail closed where the current accepted evidence does not justify a stronger
  claim.
- Cite the inherited baseline contract, the accepted item-2 architectural
  audit, the human strategic roadmap, and the accepted `N14` predecessor
  decision explicitly.
- Do not edit production code, tests, public surfaces, executables, Cabal,
  controller state, roadmap state, retry rules, or review/merge artifacts.
- Do not design search/ambiguity or termination policy, define the full
  reconstruction contract, run coverage campaigns, authorize architecture
  revision, or treat bounded predecessor packets as if they already prove
  general automatic iso-recursive inference.

## Blockers

No live controller blocker or retry obligation is present.

Active blockers that must remain blockers rather than widened work:

- accepted item `2` leaves `non-cyclic-graph` as `unknown`, so this round
  must resolve mechanism-level pressure rather than jump ahead to search or
  architecture decisions;
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains binding unless a later
  accepted roadmap item explicitly changes it; and
- open `BUG-2026-03-16-001` remains predecessor implementation context only,
  not authority to divert this round away from the strategic mechanism-map
  step.
