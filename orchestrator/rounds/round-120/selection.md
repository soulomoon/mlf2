# Round 120 Selection

Date: 2026-03-28
Round: `round-120`
Role: guider
Active subject: bounded `C1` / `P2` authoritative-surface successor lane

## Roadmap Provenance

- Roadmap ID:
  `2026-03-28-00-c1-p2-authoritative-surface-successor-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-28-00-c1-p2-authoritative-surface-successor-roadmap/rev-001`
- Roadmap Item ID: `item-1`
- Selection-time controller state:
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- Selection-time round-visible state:
  `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  clean (`git status --short` returned no output)

## Selected Roadmap Item

Roadmap item `1`: freeze the bounded `C1` / `P2` successor authority, exact
packet, success bar, and writable slice.

## Why This Item Should Run Now

No live retry obligation is present. The controller-visible state records one
new active round for `item-1` with `retry: null`, while the round-visible
state is still the clean scaffold checkpoint for the new roadmap family. In
the active `rev-001` roadmap bundle, every item is currently `[pending]`, so
item `1` is the lowest-numbered unfinished item and the only lawful next
move.

Accepted `round-119` already narrowed the live continuation to the remaining
`C1` / `P2 non-local-propagation authoritative-surface blocker family` only.
That handoff is narrower than a repo-scope matrix refresh and narrower than a
global architecture decision. Before any bounded implementation slice can run,
the family still needs one authoritative freeze that states:

- the exact predecessor authority chain carried forward from the March 14,
  March 25, and March 27 accepted artifacts;
- the exact admitted `C1` packet and route
  `baseTarget -> baseC -> targetC`;
- the exact success bar for this family on the current authoritative
  surfaces; and
- the permitted writable slice for a bounded current-architecture attempt.

Running item `1` now keeps the loop honest. Without this freeze, an
implementation round would still need to guess whether it is allowed to touch
only `Fallback`-adjacent logic, whether it may refresh public pipeline output
surfaces directly, whether one exact-packet improvement counts as lawful
family settlement, and whether `P5` or the settled same-lane
`C2` / `C5` / `C7` pocket may be reopened as collateral work. Item `1`
exists to remove that ambiguity before code changes begin.

## Parallel-Lane Statement

This round is `aggregate-only` and `not lane-parallelizable`.

No parallel lane split is authorized. The selected work is one bounded
authority-freeze artifact that must name one exact packet, one exact success
bar, and one exact writable slice. Splitting that freeze into parallel lanes
would create unnecessary risk of mismatched scope or duplicated authority
claims.

## Round Scope Guard

- This round is limited to roadmap item `1` only.
- The round must freeze the direct predecessor authority chain from the March
  14 baseline contract, the March 25 capability and full-pipeline contracts,
  and the accepted March 27 refreshed matrix plus narrowed successor gate.
- The round must bind the live subject to the exact admitted non-local `C1`
  packet and route `baseTarget -> baseC -> targetC` only.
- The round must keep the settled same-lane `C2` / `C5` / `C7` pocket closed
  as predecessor truth only and must not reopen it as live debt.
- The round must keep `P5` as reject-side context only and must not promote it
  into a second live lane.
- The round must preserve the inherited explicit recursive-annotation
  baseline, `iso-recursive = keep`, `non-equi-recursive = keep`,
  the inherited non-cyclic structural boundary, `no-fallback = keep`,
  one-interface-only, and the blocked boundaries around cyclic search,
  multi-SCC search, fallback widening, and second interfaces unless the
  accepted item-1 artifact explicitly changes them.
- The round must not silently widen into implementation, hardening, rollout,
  repo-level capability claims, or a new architecture gate.

## Blockers

No live retry obligation is present.

Active bounded blockers that remain in play for item `1`:

- the family does not yet have one authoritative freeze naming the exact
  `C1` packet, exact success bar, and exact writable slice;
- the round must not assume that a `Fallback`-only implementation is lawful
  until the freeze says so explicitly;
- the round must not reopen `P5` or the settled same-lane pocket as parallel
  debt; and
- the round must not silently treat existing harnesses or task-local drafts as
  authoritative controller truth before republishing them on round-owned
  surfaces.
