# Round 048 Selection

Date: 2026-03-19
Round: `round-048`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-015`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015`
- State Snapshot: `orchestrator/rounds/round-048/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 15: execute the `G3` bounded verification and evidence
consolidation gate for the accepted local-binding `rootHasMultiInst` `G2`
slice.

## Why This Item Should Run Now

`orchestrator/rounds/round-048/state-snapshot.json` is parked at `active_round_id: "round-048"`,
`stage: "select-task"`, `current_task: null`, and `retry: null`, so there is no
same-round retry to resume and no prior review outcome that overrides normal
roadmap ordering.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/roadmap.md` marks items 1 through 14 done and leaves item 15
(`G3`) as the lowest-numbered unfinished roadmap entry. Item 16 depends on item
15, so `G4` is not yet selectable.

The accepted predecessor chain makes `G3` the next lawful step. 
`orchestrator/rounds/round-047/review-record.json` finalized `G2` as
authoritative with `stage_id: "G2"`, `attempt_verdict: "accepted"`,
`stage_action: "finalize"`, and
`artifact_path: "docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md"`.
That accepted `G2` artifact fixes the inherited context for this round only:
the reviewer-auditable local proof `rootLocalMultiInst = rootBindingIsLocalType
&& rootHasMultiInst` and the bounded `targetC -> rootFinal` behavior on the
local `TypeRef` lane remain the exact lane to reverify, not a fresh
implementation target and not a widening decision.

Running `G3` now preserves the continue-bounded ordering while keeping the live
subject fixed to repaired `URI-R2-C1` and the inherited explicit-only /
non-equi-recursive / non-cyclic-graph boundary. This round should reverify the
accepted `G2` lane through the focused `ARI-C1 feasibility characterization
(bounded prototype-only)` block, the full-repo `cabal build all && cabal test`
gate, and continuity checks against the read-only `Fallback.hs` /
`PipelineSpec.hs` anchors.

This selection must stay bounded to the accepted `G2` local
`rootLocalMultiInst` / `targetC -> rootFinal` lane as inherited context only.
It does not reopen `G2` implementation, does not select `instArgRootMultiBase`,
does not widen into `boundVarTarget` or any non-local path, and does not
authorize replay reopen, `MLF.Elab.Inst` / `InstBot`, equi-recursive
reasoning, cyclic structural encoding, a second executable interface, or any
compatibility / convenience fallback.

`Bugs.md` contains no open bug that overrides this ordering. The current
repository status is non-pristine because controller artifacts and task-packet
files already exist, but that does not change the next lawful roadmap choice:
the bounded `G3` verification gate is the immediate successor to the accepted
`G2` implementation record.

## Round Scope Guard

- This round is limited to roadmap item `G3` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary
  unless the roadmap is explicitly amended first.
- Treat the accepted `G2` local `rootLocalMultiInst` / `targetC -> rootFinal`
  lane as inherited verification input only; do not reopen or extend it.
- Reverify only the bounded `Fallback.hs` / `PipelineSpec.hs` anchors and the
  accepted focused/full-gate evidence chain for that exact local lane.
- Leave `instArgRootMultiBase` explicitly unselected and fail-closed for this
  cycle.
- Do not reopen the accepted `E2` / `E3`, `F1` / `F2` / `F3` / `F4`, or `G1` /
  `G2` chain except as inherited context.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
  `boundVarTarget` widening, non-local binding widening, equi-recursive
  reasoning, cyclic structural encoding, a second executable interface, or a
  compatibility or convenience fallback path in this round.
