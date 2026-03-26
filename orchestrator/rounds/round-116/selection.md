# Round 116 Selection

Date: 2026-03-26
Round: `round-116`
Role: guider
Active subject: rev-004 exact-pocket post-settlement same-family handoff
decision for the same-lane `C2` / `C5` / `C7` pocket

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-004`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-116` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `4`: decide the post-settlement same-family handoff.

## Why This Item Should Run Now

Accepted `round-113` finalized rev-004 item `1` and froze the exact
post-amendment settlement surface and successor boundary. Accepted
`round-114` finalized rev-004 item `2` and published the one bounded
same-pocket post-amendment settlement ledger on the frozen writable path.
Accepted `round-115` then finalized rev-004 item `3` and validated that
ledger against the accepted `round-111` current-result anchor while
preserving predecessor immutability and exact-pocket-only language. With
items `1` through `3` now complete, item `4` is the next lawful move.

The active roadmap bundle remains `rev-004`. Its item list now records items
`1`, `2`, and `3` as `done`, leaving item `4` as the only remaining pending
item in the revision. Under the rev-004 retry contract, item `4` is
aggregate-only: it must record exactly one lawful follow-on outcome for this
same exact pocket only, and it must not widen into code implementation,
hardening, rollout, or broad capability narration.

Current decision read: the stronger lawful outcome is now
`stop after bounded settlement`, not `publish one later same-family successor revision`,
because the bounded exact-pocket debt that motivated rev-004 has now been
discharged:

- the accepted current-result anchor is frozen;
- the new bounded settlement ledger exists on a new rev-004 surface;
- the new ledger has been validated against the accepted current-result
  anchor; and
- the older pre-amendment dossiers now remain intentionally immutable
  historical evidence rather than unresolved writable settlement targets.

No new exact-pocket writable surface remains unsettled. A later same-family
successor revision would therefore duplicate already-settled work or widen
beyond the exact-pocket rev-004 scope.

## Round Scope Guard

- This round is limited to roadmap item `4` only.
- The output must be docs-only and aggregate-only.
- The output must record exactly one lawful item-4 outcome:
  `stop after bounded settlement`; or
  `publish one later same-family successor revision for further bounded same-pocket settlement`.
- The output must preserve `iso-recursive = keep`,
  `non-equi-recursive = keep`, and `no-fallback = keep`.
- The output must preserve exact-pocket-only scope and predecessor
  immutability.
- The output must not authorize code changes, second packets, second
  interfaces, multi-SCC search, fallback widening, rollout, or broad
  capability claims.

## Blockers

No live retry obligation is present.

Active bounded blockers that remain in play for item `4`:

- the decision must stay aggregate-only and exact-pocket-only;
- if a successor revision were published, it would need a new concrete
  exact-pocket settlement debt rather than repetition of already-settled
  surfaces;
- the controller must not stop if item `4` lawfully publishes a successor
  revision with pending work; and
- if the stronger lawful read is stop, the decision must make clear that the
  older pre-amendment dossiers remain historical evidence by design, not
  unresolved debt.
