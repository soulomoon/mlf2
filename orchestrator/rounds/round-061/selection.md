# Round 061 Selection

Date: 2026-03-21
Round: `round-061`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 28: execute the bounded `J4` next-cycle decision gate for the
accepted `J3`-reverified repaired `URI-R2-C1` local-binding inst-arg-only
singleton-base slice.

## Why This Item Should Run Now

`orchestrator/state.json` already fixes the live controller state at
`active_round_id: "round-061"`, `active_round_dir:
"orchestrator/rounds/round-061"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and `last_completed_round:
"round-060"`. That means there is no same-round retry to resume, no
interrupted later stage to continue, and no lawful reason to reopen an older
round instead of selecting the next roadmap item now.

`orchestrator/roadmap.md` marks items 1 through 27 done and leaves only item
28 (`J4`) pending. Under the guider contract, the next lawful choice is the
lowest-numbered unfinished item unless live retry state forces a same-round
retry. The live retry state does not: `retry` is `null`, and
`orchestrator/retry-subloop.md` therefore does not authorize replaying or
replanning any prior round. Item 28 now has its dependency satisfied because
item 27 completed in accepted `round-060`.

The accepted predecessor authority makes this successor exact rather than
speculative. `orchestrator/rounds/round-060/review-record.json` finalized
`J3` as authoritative with `stage_id: "J3"`, `attempt: 1`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, canonical artifact
path `docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`, and
all recorded `J3-*` checks passing. The accepted `J3` artifact reverified
exactly one already-accepted `J2` lane only: the local-binding inst-arg-only
singleton-base `rootLocalInstArgSingleBase` / `baseTarget -> baseC` /
same-lane `targetC` path in
`src/MLF/Elab/Run/ResultType/Fallback.hs` with its focused
`ARI-C1 feasibility characterization (bounded prototype-only)` evidence in
`test/PipelineSpec.hs`.

That accepted `J3` evidence chain carries accepted `J1` / `J2` continuity
that remains binding here. `orchestrator/rounds/round-058/review-record.json`
finalized `J1` as authoritative and froze exactly one bounded successor slice:
the adjacent local-binding inst-arg-only singleton-base
`baseTarget -> baseC` lane in `Fallback.hs`, together with its same-lane
`targetC` use, with future ownership limited to `Fallback.hs` and
`test/PipelineSpec.hs`. `orchestrator/rounds/round-059/review-record.json`
then finalized `J2` as authoritative and landed only that frozen slice,
introducing the reviewer-auditable `rootLocalInstArgSingleBase` proof and the
dedicated same-lane `targetC` arm while preserving the completed
`rootLocalSingleBase` lane, the already-accepted scheme-alias/base-like
`baseTarget` route, retained-target behavior, and non-local fail-closed
behavior outside the selected lane. `J4` therefore inherits an already bound,
already implemented, and already reverified exact lane; it is not a lawful
vehicle to reopen `J1`, `J2`, or `J3`.

That accepted `J3` authority also fixes the round boundary that `J4` must now
inherit unchanged. `J3` explicitly did not preempt `J4`, did not reopen
implementation, and did not authorize any production or test edits, any
reopening of accepted `I4`, `J1`, or `J2`, any replay reopen, any
`MLF.Elab.Inst` / `InstBot` / `boundTarget` / `schemeBodyTarget` /
`ResultType.View` work, or any widening beyond repaired `URI-R2-C1`. The
accepted `J3` artifact also restates that accepted `U2 = authority-narrowed`,
`U3 = uniqueness-owner-stable-refuted`, and
`U4 = constructor-acyclic-termination-refuted` remain binding negative
findings, not widening clearance. `J4` therefore must be a closed decision
gate over the accepted `J3` evidence chain, not a reopening or widening
vehicle.

The cycle-design and inherited decision records point to the same next step.
The approved continue-bounded design and accepted `U6` decision both permit
only one bounded non-widening cycle at a time inside repaired `URI-R2-C1`.
For the fourth stage in such a cycle, the lawful role is an aggregate
decision gate that records exactly one next-step result without itself
widening the roadmap. After `J3` has already reverified the selected `J2`
lane and preserved the inherited explicit-only / non-equi-recursive /
non-cyclic-graph / no-second-interface / no-fallback boundary, `J4` is the
required next stage because it records whether the evidence supports
`continue-bounded`, `widen-approved`, or `stop-blocked`.

`/Volumes/src/mlf4/Bugs.md` currently has no open entries. The resolved bugs
remain continuity context only and do not displace roadmap ordering or create
selection authority for replay reopen, non-local widening, or any other
out-of-scope family here.

Current repository status before writing this selection showed only the
controller-state preparation (`M orchestrator/state.json`) plus the
guider-owned round artifact path under `orchestrator/rounds/round-061/` on
branch `codex/round-061-j4-next-cycle-decision`. That status does not change
roadmap order, but it is consistent with selecting `J4` now and shows no
separate implementation diff that would justify a different stage choice.

## Round Scope Guard

- This round is limited to roadmap item `J4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary.
- Ground the decision strictly in the accepted `J3` evidence chain and the
  accepted `I4` / `J1` / `J2` continuity it carries forward; do not reopen
  accepted `I4`, `J1`, `J2`, or `J3`.
- Record exactly one bounded next-step result for the already reverified local
  inst-arg-only singleton-base `rootLocalInstArgSingleBase` /
  `baseTarget -> baseC` / same-lane `targetC` lane:
  `continue-bounded`, `widen-approved`, or `stop-blocked`.
- Preserve the frozen `src/MLF/Elab/Run/ResultType/Fallback.hs` /
  `test/PipelineSpec.hs` ownership, the completed `rootLocalSingleBase` lane,
  and the already-accepted scheme-alias/base-like `baseTarget` route outside
  the selected lane.
- Do not reinterpret accepted `U2` / `U3` / `U4` negative findings as
  widening clearance.
- Treat replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, non-local widening, and every broader
  trigger family as inherited context only and out of scope for this round.
- Do not treat this decision gate as implementation, review, merge, or
  roadmap-update authority. Any successor work after an accepted `J4` result
  must begin with a fresh bounded exact-target bind rather than silently
  widening or reopening accepted prior stages.
