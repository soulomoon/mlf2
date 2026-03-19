# Round 043 Selection

Date: 2026-03-19
Round: `round-043`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 10: execute the `F2` bounded local-binding scheme-alias/base-like `keepTargetFinal` / `targetC` fail-closed implementation slice frozen by `F1`.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `active_round_id: round-043`, `stage: select-task`, `current_task: null`, and `retry: null`, so no same-round retry is active and no prior review outcome forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmap.md` marks items 1 through 9 done, leaving item 10 (`F2`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `F2` the next lawful selection.

The accepted predecessor chain fixes both the boundary and the exact implementation lane that must run now. `E2` authoritative `attempt-2` implemented only the bounded same-lane retained-child local-`TypeRef` slice; `E3` authoritative `attempt-1` reverified that exact slice under the focused `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, a fresh full `cabal build all && cabal test` gate, and predecessor continuity checks; `E4` authoritative `attempt-1` finalized `continue-bounded`; and `F1` authoritative `round-042` review finalized the next bind, freezing exactly one `F2` target: the adjacent local-binding `rootIsSchemeAlias && rootBoundIsBaseLike` branch inside `keepTargetFinal` and the downstream `targetC` selection in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with ownership limited to `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`. Because that bind is already accepted, the next lawful step is `F2` implementation, not `F3` verification, `F4` decision work, replay reopen, or any wider target family.

The current boundary remains fixed by the roadmap, retry contract, guider role, and accepted `E2` / `E3` / `E4` / `F1` artifacts. The live subject stays repaired `URI-R2-C1`; the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary still applies; accepted `U2`, `U3`, and `U4` negative findings remain binding; `boundVarTarget` must be treated as absent for this selected slice; and `rootHasMultiInst`, `instArgRootMultiBase`, replay reopen, `MLF.Elab.Inst`, `InstBot`, non-local binding, equi-recursive reasoning, cyclic structural encoding, second-interface work, and broader widening remain out of scope.

`Bugs.md` still carries open replay-path bug `BUG-2026-03-16-001`, but it remains replay-lane continuity context only and does not authorize reopening `MLF.Elab.Inst.applyInstantiation`, `InstBot`, or broader recursive-inference work in this round. Current repository status is already non-pristine (`orchestrator/state.json` modified and `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked), so selecting the narrow bounded `F2` implementation slice best respects existing work while advancing the lowest unfinished roadmap item under the live boundary.

## Round Scope Guard

- This round is limited to roadmap item `F2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary unless the roadmap is explicitly amended first.
- Implement exactly the `F1`-frozen adjacent local-binding scheme-alias/base-like `keepTargetFinal` / `targetC` fail-closed lane in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with bounded coverage only in `test/PipelineSpec.hs`.
- Carry the accepted `E2` / `E3` same-lane retained-child baseline forward as inherited context only; do not reopen that lane as active target work.
- Treat `boundVarTarget` as absent for this selected slice, and keep `rootHasMultiInst` and `instArgRootMultiBase` out of scope.
- Treat accepted `U2` / `U3` / `U4` negative findings as still binding; do not reinterpret them as authority, uniqueness, owner-stability, or constructor-feasibility clearance.
- Do not introduce replay-lane reopen, `MLF.Elab.Inst` / `InstBot` edits, non-local binding work, equi-recursive reasoning, cyclic structural encoding, multi-SCC widening, cross-family widening, compatibility fallbacks, convenience widening paths, or a second executable interface in this round.
