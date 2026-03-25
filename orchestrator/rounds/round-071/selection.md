# Round 071 Selection

Date: 2026-03-22
Round: `round-071`
Role: guider
Active subject: preserved generic scheme-alias / base-like `baseTarget` planning subject
Successor lane: planning-only lane fixed by accepted `N2` and governed by accepted `N3`

## Roadmap Provenance

- Roadmap ID: `2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap`
- Roadmap Revision: `rev-004`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-004`
- State Snapshot: `orchestrator/rounds/round-071/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 4 (`N4`): execute the `N4` exact bounded target bind for the
reopened loop.

## Why This Item Should Run Now

`orchestrator/rounds/round-071/state-snapshot.json` fixes the live controller state at
`active_round_id: "round-071"`, `stage: "select-task"`, `current_task: null`,
`retry: null`, `branch: "codex/round-071-n4-exact-target-bind"`, and
`last_completed_round: "round-070"`. Under
`orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-004/retry-subloop.md`, `N4` may retry within the same round, but the
live state records no retry object, so there is no same-round retry to resume
and no lawful reason to skip fresh roadmap selection before `plan`.

`orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-004/roadmap.md` marks items 1 through 3 (`N1` through `N3`) done and
item 4 (`N4`) pending, with items 5 through 7 depending on it. That makes
`N4` the lowest-numbered unfinished item and therefore the default next stage
unless accepted review artifacts force a retry instead. They do not.
`orchestrator/rounds/round-070/review-record.json` finalized `N3` as
`attempt_verdict: "accepted"`, `stage_action: "finalize"`,
`retry_reason: "none"`, `status: "authoritative"`, and
`final_outcome: "baseTarget-safety-acceptance-contract-established"`. Accepted
`N3` is therefore binding predecessor evidence, not live retry work.

The accepted planning continuity is now fixed and narrow. Accepted `N1`
reopened only a planning lane, not an implementation or verification lane.
Accepted `N2` selected exactly one live subject inside that lane: the
preserved generic scheme-alias / base-like `baseTarget` route only. Accepted
`N3` then froze the verifier-checkable safety and acceptance contract for that
selected subject, including the threatened-property audit and the admissibility
axes for alias-bound ownership, inverse-translation-safe bound inlining, and
binding-flag reconstruction from structural and variance evidence. Those
accepted artifacts keep repaired `URI-R2-C1` closed as predecessor evidence
only and keep replay reopen, `MLF.Elab.Inst`, `InstBot`, every other fallback
family, every different solver/pipeline subject, and every widening route
blocked.

`tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
matches that state exactly: `N4` remains `NO`, and its next action says that
once `N1` through `N3` are accepted, the loop must bind one exact bounded
slice before any implementation begins. The `N4` completion notes in
`orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-004/roadmap.md` further require that the selected target freeze one
bounded binder-centric slice inside the preserved generic scheme-alias /
base-like `baseTarget` route and prove that it satisfies the accepted `N3`
contract by naming the owner binder, owned bound, exact edge or translation
step, inverse-translation-safe bound-inlining story, and the structural /
variance evidence for binding-flag reconstruction.

Selecting `N5`, `N6`, or `N7` now would unlawfully assume exact-target,
design, implementation, or verification authority that does not yet exist.
Reopening the exhausted repaired `URI-R2-C1` queue would contradict accepted
`L1` / `L2` closure and the explicit successor-lane rules carried forward by
accepted `N1`, `N2`, and `N3`. `Bugs.md` still lists
`BUG-2026-03-16-001`, but that replay / `InstBot` defect remains read-only
predecessor context only and does not authorize replay reopen or force a
different stage. Repository status in the controller root shows only the
controller-owned `M orchestrator/rounds/round-071/state-snapshot.json` edit, which does not change the
selection logic.

`N4` should therefore run now because it is the first pending roadmap item
that can lawfully act on the accepted `N3` contract without widening scope:
it freezes exactly one bounded target inside the already-selected
`baseTarget` lane and keeps every blocked route blocked until a later accepted
item explicitly says otherwise.

## Round Scope Guard

- This round is limited to roadmap item `N4` only.
- Keep the live subject bounded to the preserved generic scheme-alias /
  base-like `baseTarget` planning lane fixed by accepted `N2` and governed by
  the accepted `N3` safety contract.
- Treat accepted `L1`, `L2`, `N1`, `N2`, and `N3` continuity as binding
  predecessor evidence.
- Keep repaired `URI-R2-C1` closed as predecessor evidence only; do not reopen
  the exhausted repaired queue as live work.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unless a later
  accepted roadmap item explicitly amends it.
- `N4` may bind exactly one bounded target only if it satisfies the accepted
  `N3` contract: owner binder, owned bound, exact edge or translation step,
  inverse-translation-safe bound-inlining story, structural / variance
  evidence for binding-flag reconstruction, and explicit out-of-scope
  exclusions.
- Keep replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`,
  every other fallback family, every different solver/pipeline subject,
  cross-family search, equi-recursive reasoning, implicit unfolding, cyclic
  structural encoding, graph-cycle exceptions, multi-SCC support, second
  interface work, and fallback widening out of scope.
- Do not authorize `N5` through `N7`, implementation, verification,
  roadmap/state edits, bug-tracker edits, or production-code changes in this
  round.
