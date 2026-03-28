# Repo-Scope Refreshed Matrix And Narrowed Blocker Successor Roadmap

## Context

- This roadmap family succeeds the accepted global
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
  family, whose authoritative execution record now extends through
  `orchestrator/rounds/round-116`.
- The live controller resolves the active roadmap family through
  `orchestrator/state.json` fields `roadmap_id`, `roadmap_revision`, and
  `roadmap_dir`; the top-level `orchestrator/roadmap.md`,
  `orchestrator/retry-subloop.md`, and `orchestrator/verification.md` files
  are pointer stubs only.
- Accepted rev-004 item `4` selected
  `stop after bounded settlement` for the exact same-lane
  `C2` / `C5` / `C7` pocket only. That accepted outcome closed the exact-pocket
  settlement lane and did not publish a rev-005 bundle in the prior roadmap
  family.
- The inherited baseline remains unchanged unless a later accepted item in
  this family explicitly changes it: explicit recursive annotations remain the
  production baseline, recursive meaning remains iso-recursive only,
  `non-equi-recursive = keep` remains binding, `no-fallback = keep` remains
  binding, no second interface is authorized, and no multi-SCC search is
  authorized.
- Accepted March 25 and March 26 artifacts remain authoritative predecessor
  evidence only. The March 26 global `keep` vs `reopen` gate remains
  historically true on the evidence it consumed, but this family exists
  because later accepted rev-003 / rev-004 exact-pocket settlement changed the
  same-lane `C2` / `C5` / `C7` read that fed that earlier aggregate gate.
- The exact same-lane `C2` / `C5` / `C7` pocket is settled predecessor truth in
  this family, not the next live debt packet. The next honest question is
  repo-scope again:
  after carrying forward the repaired exact-pocket read, what is the current
  representative matrix and what is the strongest repo-scope posture for the
  remaining blocker set?
- Current local task packets and bounded research harnesses may inform planning
  context, but they are not authoritative controller truth until a round in
  this family republishes any decisive evidence in round-owned artifacts.
- This family is planning/docs-first and repo-scope-first. No code-changing
  round is authorized by default in rev-001.
- Runtime may use parallel subagents within one selected round when bounded
  sub-slices are genuinely independent, maintain disjoint write scopes, and
  isolate build outputs where needed, but the controller still selects exactly
  one roadmap item at a time and expects one authoritative round result.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Freeze the post-rev-004 repo-scope successor authority, evidence inputs, and non-widening boundary
   Depends on:
   Completion notes: complete when an accepted docs-only artifact freezes this
   family's authoritative predecessor chain and current boundary:
   the inherited baseline contract, the March 25 capability and
   full-pipeline contracts, the March 26 matrix/gate as historical evidence,
   the accepted rev-003 / rev-004 same-lane settlement chain as settled
   predecessor truth, and one explicit rule for how current local packets or
   research harnesses may be reused only as non-authoritative planning inputs
   until republished in round-owned artifacts. This item must keep the
   same-lane `C2` / `C5` / `C7` pocket settled, keep multi-SCC search, second
   interfaces, fallback widening, and broad capability claims blocked, and
   make item `2` the next lawful move.

2. [done] Publish and validate one refreshed representative family-matrix settlement surface
   Depends on: item 1
   Completion notes: complete when an accepted artifact republishes the
   repo-scope representative matrix while carrying forward the accepted
   repaired exact-pocket `C2` / `C5` / `C7` read and honestly recording the
   current repo-scope read for the remaining rows, including any freshly
   republished bounded `C1` and `P5` evidence used by the round. The artifact
   must not back-edit the March 26 matrix or gate in place and must keep the
   exact same-lane pocket as settled predecessor truth rather than reopened
   same-pocket debt.
   The same accepted artifact must also validate evidence provenance for any
   fresh `C1` / `P5` carry-forward, preserve historical-artifact immutability,
   and avoid silently widening into a repo-level capability claim.

3. [done] Record one narrowed repo-scope successor gate
   Depends on: items 1, 2
   Completion notes: complete when an accepted aggregate artifact records
   exactly one current repo-scope posture on the refreshed matrix and exactly
   one immediate handoff:
   `non-cyclic-graph = keep`,
   `narrowed unresolved / continue within the current architecture`, or
   `reopen the non-cyclic-graph revision question`.
   The same accepted artifact must then record exactly one handoff:
   stop,
   open one bounded current-architecture successor lane for the remaining
   blocker family work, or
   open one explicit boundary-revision family only if the refreshed record
   proves that is necessary.
   The artifact must distinguish historical March 26 reopen evidence from the
   refreshed current read, keep the settled same-lane pocket closed, preserve
   `iso-recursive = keep`,
   `non-equi-recursive = keep`, and `no-fallback = keep` unless an explicit
   accepted decision changes them, and avoid silent widening into rollout or
   broad capability claims.
