# Round 039 Selection

Date: 2026-03-18
Round: `round-039`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-006`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-006`
- State Snapshot: `orchestrator/rounds/round-039/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 6: execute `E2` bounded fail-closed retained-child `boundVarTarget` / nested-`forall` implementation slice frozen by `E1`.

## Why This Item Should Run Now

`orchestrator/rounds/round-039/state-snapshot.json` is parked at `active_round_id: round-039`, `stage: select-task`, `current_task: null`, and `retry: null`, so no same-round retry is active and no prior review outcome forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-006/roadmap.md` marks item 1 (`C1`), item 2 (`C2`), item 3 (`C3`), item 4 (`C4`), and item 5 (`E1`) done, leaving item 6 (`E2`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `E2` the next lawful selection.

The accepted `E1` bind artifact in `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md` already froze exactly one next bounded implementation slice: the local-binding-only retained-child `boundVarTarget` / nested-`forall` fail-closed hardening lane in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`. Because that bind is already authoritative, the next round should execute the frozen implementation slice itself rather than reopen selection or jump ahead to verification/decision work that depends on an accepted `E2` implementation result.

This selection keeps the live subject fixed to repaired `URI-R2-C1` and preserves the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary. The accepted `C2` / `C3` local-binding-only `rootBindingIsLocalType` fail-closed baseline remains binding, and `E2` is constrained to the retained-child lane only: a child-derived retained target may survive only when it stays in the same local `TypeRef` lane and remains free of nested `forall` / nested scheme-root ownership as detected by `boundHasForallFrom`.

Accepted predecessor findings still constrain the round. `U2` remains `authority-narrowed`, `U3` remains `uniqueness-owner-stable-refuted`, and `U4` remains `constructor-acyclic-termination-refuted`, so `E2` must remain a bounded fail-closed implementation slice rather than a widening step. `Bugs.md` still carries open replay-path defect `BUG-2026-03-16-001`, but that remains replay-lane continuity context only and does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, or any broader recursive-inference work here.

Current repository status is already non-pristine (`orchestrator/rounds/round-039/state-snapshot.json` modified and `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked). Selecting the narrow `E2` implementation slice respects that state while advancing exactly one roadmap item that is already concretely frozen and bounded by accepted evidence.

## Round Scope Guard

- This round is limited to roadmap item `E2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- Implement only the `E1`-frozen retained-child `boundVarTarget` / nested-`forall` fail-closed slice inside `src/MLF/Elab/Run/ResultType/Fallback.hs`, with focused coverage limited to `test/PipelineSpec.hs`.
- Start from the accepted `C2` / `C3` baseline: `rootBindingIsLocalType` remains a mandatory gate before any retained-target behavior is considered.
- Permit a child-derived retained target only when the retained child stays inside the same local `TypeRef` lane and remains free of nested `forall` / nested scheme-root ownership as detected by `boundHasForallFrom`; otherwise stay fail-closed.
- Add only the bounded focused coverage frozen by `E1`: one retained-child accept case and one matched fail-closed contrast in the existing `ARI-C1 feasibility characterization (bounded prototype-only)` block.
- Leave all other `keepTargetFinal` trigger families (`rootHasMultiInst`, `instArgRootMultiBase`, `rootIsSchemeAlias && rootBoundIsBaseLike`) fail-closed and out of scope.
- Do not reopen replay repair, `MLF.Elab.Inst`, `InstBot`, `E1` selection, `E3` verification, `E4` decision work, equi-recursive reasoning, cyclic structural encoding, cross-family widening, compatibility fallbacks, convenience widening, or a second executable interface.
