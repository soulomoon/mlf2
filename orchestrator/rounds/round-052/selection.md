# Round 052 Selection

Date: 2026-03-20
Round: `round-052`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-019`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019`
- State Snapshot: `orchestrator/rounds/round-052/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 19: execute the `H3` bounded verification and evidence
consolidation gate for the accepted `H2` slice.

## Why This Item Should Run Now

`orchestrator/rounds/round-052/state-snapshot.json` is parked at `active_round_id: "round-052"`,
`active_round_dir: "orchestrator/rounds/round-052"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and `last_completed_round: "round-051"`.
That machine state means there is no same-round retry to resume and no prior
selection to carry forward ahead of normal roadmap ordering.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/roadmap.md` marks items 1 through 18 done. Item 19 (`H3`) is the
lowest-numbered unfinished roadmap entry, and item 20 (`H4`) depends on it, so
`H3` is the next lawful selection under the guider contract.

The accepted predecessor chain fixes both the timing and the exact bounded
scope for this round. `orchestrator/rounds/round-051/review-record.json`
finalized `H2` as authoritative with `stage_id: "H2"`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`artifact_path: "docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md"`.
That accepted `H2` artifact records the exact lane to reverify: the
reviewer-auditable local proof
`rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`,
the bounded `targetC -> rootFinal` behavior on the local `TypeRef` lane, the
matched non-local fail-closed contrast, and the preserved `baseTarget`
rejection outside the selected lane. `H3` should therefore verify that exact
`H2` lane only; it is not a new implementation slice and not a widening
decision.

The approved
`docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
makes `H3` the third step in the four-item `H` cycle after `H2` and before
`H4`, and `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md` keeps the
future work frozen to the same bounded local-binding `instArgRootMultiBase`
family. Running `H3` now preserves the non-widening ordering inside repaired
`URI-R2-C1` and the inherited explicit-only / non-equi-recursive /
non-cyclic-graph boundary instead of reopening implementation or jumping ahead
to `H4`.

The older predecessor packet under
`tasks/todo/2026-03-11-recursive-types-orchestration/` remains boundary
context only. Its mechanism table records the explicit-only acyclic `TyMu` path
as complete on current `master` while automatic recursive-type inference stays
disabled, so it supports keeping `H3` as a bounded reverification round rather
than broadening the live subject.

`Bugs.md` still contains open bug `BUG-2026-03-16-001`, but it is the
replay-path `MLF.Elab.Inst` / `InstBot` defect that the accepted `H1` / `H2`
chain explicitly leaves out of scope. That open bug therefore does not
override roadmap ordering or authorize replay reopen, `boundVarTarget`
widening, non-local widening, or broader recursive-inference work in this
round. Before this artifact was written, repository status showed only the
expected controller-state preparation (`M orchestrator/rounds/round-052/state-snapshot.json`), so there
is no conflicting in-progress implementation that changes the next lawful
selection.

## Round Scope Guard

- This round is limited to roadmap item `H3` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary
  unless the roadmap is explicitly amended first.
- Treat the accepted `H2` `rootLocalInstArgMultiBase` / `targetC -> rootFinal`
  lane as inherited verification input only; do not reopen or extend
  implementation.
- Reverify only the bounded `Fallback.hs` / `PipelineSpec.hs` anchors, the
  focused `ARI-C1 feasibility characterization (bounded prototype-only)` rerun,
  the full `cabal build all && cabal test` gate, and predecessor continuity
  checks for that exact lane.
- Preserve the matched non-local fail-closed contrast and `baseTarget`
  rejection outside the selected lane.
- Treat `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
  `boundVarTarget` as inherited context only; do not reopen them as separate
  target families in this cycle.
- Do not advance to `H4` before accepted `H3` verification finalizes.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`, non-local
  binding widening, equi-recursive reasoning, cyclic structural encoding, a
  second executable interface, or a compatibility or convenience fallback path
  in this round.
