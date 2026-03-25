# Round 065 Selection

Date: 2026-03-21
Round: `round-065`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-032`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-032`
- State Snapshot: `orchestrator/rounds/round-065/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 32: execute the bounded `K4` next-cycle decision gate for the
accepted `K3`-reverified repaired `URI-R2-C1` local-binding empty-candidate /
no-inst-arg scheme-alias / base-like slice.

## Why This Item Should Run Now

`orchestrator/rounds/round-065/state-snapshot.json` already fixes the live controller state at
`active_round_id: "round-065"`, `active_round_dir:
"orchestrator/rounds/round-065"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, `branch:
"codex/round-065-k4-next-cycle-decision"`, and `last_completed_round:
"round-064"`. Under `orchestrator/roles/guider.md` and
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-032/retry-subloop.md`, that means there is no same-round retry to
resume, no interrupted later stage to continue, and no lawful basis to reopen
an older round instead of selecting the next roadmap item for the repaired
live subject.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-032/roadmap.md` marks item 31 (`K3`) done and item 32 (`K4`)
pending. Because the guider must prefer the lowest-numbered unfinished item
when `retry` is `null`, item 32 is the next lawful selection. The roadmap
completion notes for item 32 already define the exact `K4` contract: record
exactly one bounded next-step result for the accepted repaired `URI-R2-C1`
`K2` / `K3`
`rootLocalEmptyCandidateSchemeAliasBaseLike` local empty-candidate /
no-inst-arg scheme-alias / base-like `baseTarget -> baseC` / same-lane
`targetC` lane, while preserving the frozen `Fallback.hs` /
`PipelineSpec.hs` ownership anchors, the completed `rootLocalSingleBase`
lane, the completed `rootLocalInstArgSingleBase` lane, the already-accepted
`rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
lane, and the inherited explicit-only / non-equi-recursive /
non-cyclic-graph / no-second-interface / no-fallback boundary.

The accepted predecessor authority makes `K4` exact rather than speculative.
`orchestrator/rounds/round-064/review-record.json` finalized `K3` as the
authoritative accepted record with `stage_id: "K3"`, `attempt: 1`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, `status:
"authoritative"`, canonical artifact path
`docs/plans/2026-03-21-uri-r2-c1-k3-bounded-verification-gate.md`, and all
recorded `K3-*` checks passing. That accepted `K3` artifact reverified only
the exact `K2` lane frozen by accepted `K1`: the local-binding
empty-candidate / no-inst-arg scheme-alias / base-like
`rootLocalEmptyCandidateSchemeAliasBaseLike` /
`baseTarget -> baseC` / same-lane `targetC` lane in
`src/MLF/Elab/Run/ResultType/Fallback.hs` with focused
`ARI-C1 feasibility characterization (bounded prototype-only)` evidence in
`test/PipelineSpec.hs`, plus a fresh full-repo gate and predecessor
continuity checks. Because `K3` is already accepted and authoritative, the
next lawful stage is the aggregate-only `K4` decision gate, not another bind,
not another implementation slice, and not a roadmap update.

Accepted `J4` / `K1` / `K2` continuity narrows `K4` further.
`orchestrator/rounds/round-061/review-record.json` finalized `J4` as the
authoritative `continue-bounded` decision that required any successor work to
begin with a fresh bounded exact-target bind rather than silent widening or
reopening. `orchestrator/rounds/round-062/review-record.json` then finalized
`K1` as the accepted exact bind for this cycle, freezing exactly one future
successor slice: the local-binding empty-candidate / no-inst-arg
scheme-alias / base-like `baseTarget -> baseC` lane plus its same-lane
`targetC` use, with ownership frozen to `Fallback.hs` and `PipelineSpec.hs`.
`orchestrator/rounds/round-063/review-record.json` then finalized `K2` as the
accepted bounded implementation result for that exact slice only, introducing
the reviewer-auditable
`rootLocalEmptyCandidateSchemeAliasBaseLike` proof and its dedicated same-lane
`targetC` arm while preserving the completed `rootLocalSingleBase` lane, the
completed `rootLocalInstArgSingleBase` lane, and the already-accepted
`rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
lane as inherited continuity only. `K4` therefore inherits an already bound,
already implemented, and already reverified exact lane; it is not a lawful
vehicle to reopen `J4`, `K1`, `K2`, or `K3`.

The immutable predecessor packet under
`tasks/todo/2026-03-11-recursive-types-orchestration/` reinforces the same
boundary rather than widening it. Its `mechanism_table.md`, `task_plan.md`,
and `findings.md` record the accepted campaign outcome that explicit recursive
annotations typecheck through the full pipeline only on the explicit-only
acyclic `TyMu` path, while automatic recursive-type inference remains disabled
and the non-equi-recursive / acyclic boundary remains preserved. That
predecessor evidence is continuity context only for `K4`; it does not clear
automatic inference, does not authorize broader recursive inference, and does
not relax the repaired `URI-R2-C1` continue-bounded boundary.

The broader accepted design lineage points to the same next step.
`docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
defines the bounded cycle shape as bind, implementation, verification, then
next-cycle decision. The accepted `U6` decision in
`docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` still
permits only another bounded non-widening cycle inside repaired `URI-R2-C1`
and the inherited explicit-only / non-equi-recursive / non-cyclic-graph
boundary. Selecting `K4` honors that contract because it is the decision gate
immediately after the accepted `K3` verification stage, and it preserves the
repaired continue-bounded boundary instead of widening or reopening prior
stages.

Repository status preserves that continuity rather than changing selection:
the worktree shows only the existing controller-state modification
`M orchestrator/rounds/round-065/state-snapshot.json`, `/Volumes/src/mlf4/Bugs.md` still has an empty
`## Open` section, and `orchestrator/rounds/round-065/` had no pre-existing
round files before this guider artifact. Those facts do not create authority
to skip the pending decision gate or reorder the roadmap.

## Round Scope Guard

- This round is limited to roadmap item `K4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary.
- Ground the decision strictly in the accepted `K3` evidence chain and the
  accepted `J4` / `K1` / `K2` continuity it carries forward; do not reopen
  accepted `J4`, `K1`, `K2`, or `K3`.
- Record exactly one bounded next-step result for the already reverified local
  empty-candidate / no-inst-arg scheme-alias / base-like
  `rootLocalEmptyCandidateSchemeAliasBaseLike` /
  `baseTarget -> baseC` / same-lane `targetC` lane:
  `continue-bounded`, `widen-approved`, or `stop-blocked`.
- Preserve the frozen `src/MLF/Elab/Run/ResultType/Fallback.hs` /
  `test/PipelineSpec.hs` ownership anchors, the completed
  `rootLocalSingleBase` lane, the completed
  `rootLocalInstArgSingleBase` lane, and the already-accepted
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane as inherited continuity only.
- Treat the predecessor recursive-types packet as immutable continuity
  evidence only; do not reinterpret its explicit-only `TyMu` completion as
  clearance for automatic inference or broader recursive widening.
- Treat accepted `U2` / `U3` / `U4` negative findings as binding. Do not
  reinterpret them as clearance for replay reopen, constructor widening, or
  any broader recursive-inference family.
- Do not reopen `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, replay reopen,
  `MLF.Elab.Inst`, `InstBot`, non-local widening, any broader
  trigger-family widening, a second executable interface, or any
  compatibility, convenience, or default-path fallback in this round.
- Do not treat this decision gate as implementation, review, merge, or
  roadmap-update authority. Any successor work after an accepted `K4` result
  must begin with a fresh bounded exact-target bind rather than silently
  widening or reopening accepted prior stages.
