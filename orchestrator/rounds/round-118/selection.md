# Round 118 Selection

Date: 2026-03-27
Round: `round-118`
Role: guider
Active subject: post-rev-004 repo-scope refreshed representative family-matrix
settlement surface

## Roadmap Provenance

- Roadmap ID:
  `2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001`
- Selection-time controller state:
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- Selection-time round-visible state:
  `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state and accepted-roadmap updates only
  (`git status --short --branch` returned
  `## orchestrator/round-118-refresh-representative-family-matrix`,
  `M orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001/roadmap.md`,
  and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `2`: publish and validate one refreshed representative
family-matrix settlement surface.

## Why This Item Should Run Now

No live retry obligation is present: both the controller-visible state and the
round-visible state record `retry: null`. Under the active `rev-001` roadmap,
item `2` is now the lowest-numbered unfinished item, and accepted round-117
explicitly froze item `2` as the next lawful move.

Item `2` must run now because the March 26 aggregate matrix and its
`non-cyclic-graph = keep` vs `reopen` gate remain immutable historical
evidence on pre-amendment inputs, while the accepted rev-003 / rev-004
same-lane `C2` / `C5` / `C7` chain now carries a repaired exact-pocket read
as settled predecessor truth. The controller therefore cannot lawfully jump to
item `3` until one round republishes the current repo-scope matrix on top of
the round-117 authority freeze and shows how the remaining rows read after
that repaired exact-pocket result is carried forward.

This is also the stage that must resolve the difference between planning
context and controller truth. Current task-local drafts, focused research
harnesses, and `BUG-2026-03-16-001` can inform planning, but accepted
round-117 explicitly classifies them as non-authoritative until republished on
round-owned surfaces. Item `2` is therefore the only lawful place to reuse any
fresh bounded `C1` or `P5` evidence: either republish it with provenance
validation and build-isolation discipline, or leave it out. Skipping directly
to item `3` would silently treat local planning material as authoritative
repo-scope evidence.

The refreshed matrix round must keep the settled same-lane pocket closed as
predecessor truth rather than reopened debt, preserve the inherited explicit
baseline plus `iso-recursive = keep`, `non-equi-recursive = keep`,
`no-fallback = keep`, one-interface-only, and no multi-SCC widening, and stop
short of any narrowed successor gate or repo-level capability claim.

## Parallel-Lane Statement

This round is `lane-parallelizable` inside one round only; parallel rounds are
not authorized.

Permitted bounded split if the planner chooses it:

- `C1` provenance lane:
  validate any fresh carry-forward from
  `test/Research/C1AuthoritativeSurfaceSpec.hs`, including serialized or
  isolated reruns and the exact authoritative-surface read, without editing
  the canonical refreshed-matrix artifact directly.
- `P5` provenance lane:
  validate any fresh carry-forward from
  `test/Research/P5ClearBoundarySpec.hs`, including serialized or isolated
  reruns and the exact clear-boundary versus nested-`forall` read, without
  editing the canonical refreshed-matrix artifact directly.
- aggregate synthesis lane:
  single-writer consolidation that carries forward the accepted same-lane
  `C2` / `C5` / `C7` settlement truth plus any republished `C1` / `P5`
  evidence into one authoritative refreshed-matrix artifact and the standard
  round-owned notes.

No parallel lane may reopen or re-audit the settled same-lane
`C2` / `C5` / `C7` pocket as live debt. That pocket is a carried-forward
predecessor input only.

## Round Scope Guard

- This round is limited to roadmap item `2` only.
- The output must republish one refreshed repo-scope representative matrix on
  a new round-owned surface and must not back-edit the March 26 matrix or
  gate in place.
- The same-lane `C2` / `C5` / `C7` pocket must remain settled predecessor
  truth and may appear only as a carried-forward current read, not as a live
  repair lane or a coequal parallel subject.
- Any fresh `C1` or `P5` evidence reused from local task packets or research
  harnesses must be republished with explicit provenance validation and
  serialized or isolated rerun evidence before it counts as controller truth.
- The round must preserve the inherited explicit recursive-annotation
  baseline, `iso-recursive = keep`, `non-equi-recursive = keep`,
  `no-fallback = keep`, one-interface-only, and the blocked boundaries around
  cyclic search, multi-SCC search, fallback widening, and second interfaces.
- The round must not decide item `3`, reopen the `non-cyclic-graph` revision
  question, or claim repo-level capability success.

## Blockers

No live retry obligation is present.

Active bounded blockers that remain in play for item `2`:

- the round must not silently reuse the March 26 representative matrix or the
  March 26 global gate as current repo-scope truth after the repaired
  same-lane read;
- the round must not silently promote task-local drafts, focused research
  harnesses, or bug-context traces into authority without round-owned
  republication and provenance checks;
- the round must keep the settled same-lane `C2` / `C5` / `C7` pocket closed
  and carried forward as predecessor truth only; and
- the round must stop at refreshed-matrix publication and validation, leaving
  the narrowed repo-scope successor gate to item `3`.
