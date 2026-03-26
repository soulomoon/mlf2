# Round 117 Selection

Date: 2026-03-27
Round: `round-117`
Role: guider
Active subject: post-rev-004 repo-scope successor authority, evidence-input,
and non-widening boundary freeze

## Roadmap Provenance

- Roadmap ID:
  `2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001`
- Selection-time controller state:
  `orchestrator/state.json`
- Selection-time round-visible state:
  `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## orchestrator/round-117-freeze-post-rev-004-successor-boundary`
  and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `1`: freeze the post-rev-004 repo-scope successor authority,
evidence inputs, and non-widening boundary.

## Why This Item Should Run Now

No live retry obligation is present: both the controller state and the
round-visible state record `retry: null`. Under the active `rev-001` roadmap,
item `1` is the lowest-numbered unfinished item, so it is the next lawful
move unless a retry contract forces reuse of the same round, which it does
not here.

Accepted `round-116` closed the exact same-lane `C2` / `C5` / `C7` pocket by
recording `stop after bounded settlement` for that one settled predecessor
lane only. The new roadmap family exists because that accepted rev-004 result
changes how the controller must read predecessor evidence at repo scope: the
March 26 global `keep` vs `reopen` gate remains historical evidence, but it is
no longer the live repo-scope read after the repaired exact-pocket result was
carried forward. Before any refreshed representative matrix can be lawfully
republished, the controller first needs one docs-only artifact that freezes
exactly which predecessor chain is authoritative now, which inherited
baseline contracts remain binding, and how current local task packets or
research harnesses may be reused only as non-authoritative planning inputs
until republished in round-owned artifacts.

Item `1` is therefore the right stage now because it establishes the only
lawful repo-scope starting surface for later items:

- it preserves the inherited baseline from
  `2026-03-14-automatic-recursive-inference-baseline-contract.md`,
  the March 25 capability and full-pipeline contracts, and the repo-local
  blocked boundaries around `iso-recursive = keep`,
  `non-equi-recursive = keep`, `no-fallback = keep`,
  no second interface, and no multi-SCC widening;
- it carries forward the accepted rev-003 / rev-004 same-lane settlement
  chain as settled predecessor truth rather than reopened live debt;
- it prevents the old March 26 aggregate reopen gate from being silently
  treated as the current controller truth after the repaired exact-pocket
  outcome; and
- it makes item `2` the next concrete move: republish one refreshed
  representative family-matrix surface on top of a frozen repo-scope
  authority boundary.

## Parallel-Lane Statement

This round is `aggregate-only`. No parallel lane split is authorized for item
`1`.

## Round Scope Guard

- This round is limited to roadmap item `1` only.
- The output must be docs-only and repo-scope-only.
- The output must freeze the authoritative predecessor chain and reuse rules
  for current local packets or research harnesses without treating them as
  controller truth before republication.
- The output must preserve the settled same-lane `C2` / `C5` / `C7` pocket
  closure as predecessor truth, not reopen it as live debt.
- The output must keep `iso-recursive = keep`,
  `non-equi-recursive = keep`, `no-fallback = keep`,
  and the explicit recursive-annotation production baseline unchanged.
- The output must not authorize implementation work, hardening, rollout,
  cyclic search, multi-SCC search, second interfaces, fallback widening, or
  broad capability claims.

## Blockers

No live retry obligation is present.

Active bounded blockers that remain in play for item `1`:

- the round must not silently treat local task packets, research harnesses,
  or historical March 26 aggregate artifacts as current authoritative
  controller truth;
- the round must not reopen the settled same-lane `C2` / `C5` / `C7` pocket
  as live repo-scope debt;
- the round must keep the refreshed repo-scope family docs-first and
  non-widening; and
- the round must leave item `2` as the next concrete refreshed-matrix move,
  rather than skipping ahead to a narrowed successor gate or any
  implementation path.
