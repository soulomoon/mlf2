# Round 040 Selection

Date: 2026-03-19
Round: `round-040`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 7: execute `E3` bounded verification and evidence consolidation gate for the accepted same-lane retained-child `E2` slice.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `active_round_id: round-040`, `stage: select-task`, `current_task: null`, and `retry: null`, so no same-round retry is active and no prior review outcome forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmap.md` marks item 1 (`C1`), item 2 (`C2`), item 3 (`C3`), item 4 (`C4`), item 5 (`E1`), and item 6 (`E2`) done, leaving item 7 (`E3`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `E3` the next lawful selection.

The accepted `E2` artifact in `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md` and the authoritative `orchestrator/rounds/round-039/review-record.json` finalized `E2` as `attempt=2`, `attempt_verdict=accepted`, `stage_action=finalize`, and `status=authoritative`. That accepted record fixed the current bounded subject to one same-lane retained-child local-`TypeRef` slice in `src/MLF/Elab/Run/ResultType/Fallback.hs` with focused `test/PipelineSpec.hs` coverage, while keeping nested-`forall` / nested-owner crossings fail-closed and leaving all other widening lanes excluded. Because `E2` is already authoritative, the next lawful step is to verify and consolidate evidence for that exact slice rather than reopen implementation or jump ahead to the next decision gate.

The accepted `C3` artifact remains the verification model for this cycle, and the accepted `C4` artifact recorded `continue-bounded` as the only lawful next-step token for the repaired `URI-R2-C1` lane without widening the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary. `E3` therefore fits the established cycle shape exactly: reverify the accepted bounded implementation slice under focused evidence, full-repo gate, and predecessor continuity before any `E4` decision is allowed.

Accepted predecessor findings remain binding. `C3`, `C4`, and `E2` all kept the live subject fixed to repaired `URI-R2-C1`; accepted `U2`, `U3`, and `U4` still stand as negative findings, so this round cannot reinterpret them as widening clearance. `Bugs.md` currently has no open entry that authorizes replay reopen or broader recursive-inference selection, and the resolved replay-path bug remains continuity context only. Current repository status is already non-pristine (`orchestrator/state.json` modified and `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked), so selecting the narrow verification gate `E3` advances the lowest unfinished bounded item without expanding scope.

## Round Scope Guard

- This round is limited to roadmap item `E3` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- Verify only the accepted `E2` same-lane retained-child local-`TypeRef` slice and its focused `PipelineSpec` evidence, together with the current full-repo gate and predecessor continuity.
- Preserve the accepted `E2` exclusions: nested-`forall` / nested-owner crossings remain fail-closed, and all other retained-target trigger families stay out of scope.
- Do not reopen `E1` selection, `E2` implementation, replay repair, `MLF.Elab.Inst`, `InstBot`, equi-recursive reasoning, cyclic structural encoding, cross-family widening, compatibility fallbacks, convenience widening, or a second executable interface.
- Do not preempt `E4`; that decision gate depends on an accepted `E3` verification record first.
