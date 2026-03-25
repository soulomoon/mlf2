# Round 070 Selection

Date: 2026-03-22
Round: `round-070`
Role: guider
Active subject: preserved generic scheme-alias / base-like `baseTarget` planning subject
Successor lane: planning-only lane reopened by accepted `N1` and fixed by accepted `N2`

## Roadmap Provenance

- Roadmap ID: `2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap`
- Roadmap Revision: `rev-003`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-003`
- State Snapshot: `orchestrator/rounds/round-070/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 3 (`N3`): execute the `N3` reopened-loop safety and acceptance
contract for the selected preserved generic scheme-alias / base-like
`baseTarget` planning subject.

## Why This Item Should Run Now

`orchestrator/rounds/round-070/state-snapshot.json` fixes the live controller state at
`active_round_id: "round-070"`, `stage: "select-task"`, `current_task: null`,
`retry: null`, `branch: "codex/round-070-n3-safety-acceptance-contract"`, and
`last_completed_round: "round-069"`. Under `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-003/retry-subloop.md`,
that means there is no same-round retry to resume and no lawful path that
skips fresh roadmap selection before `plan`.

`orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-003/roadmap.md` marks item 1 (`N1`) and item 2 (`N2`) done, with
item 3 (`N3`) now the lowest-numbered unfinished item and items 4 through 7
depending on earlier authority. No live retry state or accepted review artifact
forces a same-round retry instead. `orchestrator/rounds/round-069/review-record.json`
finalized `N2` as `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
`status: "authoritative"`, with `final_outcome: "baseTarget-planning-subject-selected"`.
That accepted `N2` outcome is binding predecessor evidence: the preserved
generic scheme-alias / base-like `baseTarget` route is now the only live
planning subject, and the round must stay bounded to that subject until a later
accepted roadmap item explicitly authorizes more.

Accepted predecessor continuity is therefore fixed. `L1` and `L2` remain
accepted-authoritative closure for repaired `URI-R2-C1`: the queue failed
closed and then finalized as `stop-blocked`, so it remains closed predecessor
evidence only. Accepted `N1` then recorded the only lawful post-`L2` authority
outcome as `reopen-planning-only`, preserving the inherited explicit-only /
non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
boundary unchanged and making the preserved generic scheme-alias / base-like
`baseTarget` route admissible only for later planning selection. Accepted `N2`
completed that selection and did not authorize implementation, verification,
replay reopen, cross-family search, or any other route. `Bugs.md` still lists
open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
read-only predecessor context only; it does not reopen repaired `URI-R2-C1`,
override accepted `N1` / `N2` continuity, or force a retry.

`N3` is therefore the stage that must run now because it is the first pending
item that can lawfully act on the accepted `N2` subject without widening scope.
The roadmap and mechanism table both say the reopened loop still lacks a
verifier-checkable safety and acceptance contract for this selected subject.
That contract must audit alias-bound ownership, bound inlining, binding-flag
reconstruction, threatened properties, and explicit no-go conditions inside the
accepted planning-only lane before `N4` can bind one exact target. Selecting
`N4` now would skip required safety authority; selecting `N5` through `N7`
would assume exact-target and implementation/verification authority that does
not yet exist. Reopening the exhausted repaired `URI-R2-C1` queue would
contradict accepted `L1` / `L2` closure.

Predecessor evidence in
`tasks/todo/2026-03-11-recursive-types-orchestration/mechanism_table.md` and
the inherited baseline contract keeps the repo baseline explicit-only:
automatic recursive-type inference remains unresolved and disabled, recursive
meaning remains iso-recursive rather than equi-recursive, and constraint-graph
representation remains structurally acyclic. `N3` must preserve that inherited
boundary unless a later accepted roadmap item explicitly amends it, and nothing
in the current accepted record authorizes such an amendment here.

## Round Scope Guard

- This round is limited to roadmap item `N3` only.
- Keep the live subject bounded to the selected preserved generic scheme-alias
  / base-like `baseTarget` planning lane fixed by accepted `N2`.
- Treat accepted `L1`, `L2`, `N1`, and `N2` continuity as binding predecessor
  evidence.
- Keep repaired `URI-R2-C1` closed as predecessor evidence only; do not reopen
  the exhausted repaired queue as live work.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unless a later
  accepted roadmap item explicitly amends it.
- `N3` is docs-only safety-contract work: it may define invariant audits,
  acceptance criteria, threatened-property boundaries, and no-go conditions for
  the selected subject, but it must not yet bind the exact `N4` target.
- Do not authorize implementation, verification, replay reopen, solver-wide
  recursive inference, cross-family search, another interface, fallback-path
  widening, roadmap/state edits, bug-tracker edits, or production-code changes
  in this round.
