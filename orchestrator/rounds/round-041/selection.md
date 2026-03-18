# Round 041 Selection

Date: 2026-03-19
Round: `round-041`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 8: execute the bounded `E4` next-cycle decision gate for the accepted `E3`-reverified same-lane retained-child repaired `URI-R2-C1` slice.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `active_round_id: round-041`, `stage: select-task`, `current_task: null`, and `retry: null`, so no same-round retry is active and no prior review outcome forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmap.md` marks item 1 (`C1`), item 2 (`C2`), item 3 (`C3`), item 4 (`C4`), item 5 (`E1`), item 6 (`E2`), and item 7 (`E3`) done, leaving item 8 (`E4`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `E4` the next lawful selection.

The accepted predecessor chain already fixes the live bounded subject for this decision. `C4` finalized `continue-bounded` for repaired `URI-R2-C1`; `E1` froze exactly one next bounded target, the same-lane retained-child local-`TypeRef` lane in `src/MLF/Elab/Run/ResultType/Fallback.hs` with focused `test/PipelineSpec.hs` ownership; `E2` authoritative `attempt-2` implemented only that bounded slice; and `E3` authoritative `attempt-1` reverified that exact slice under the focused `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, a fresh `cabal build all && cabal test` gate, and predecessor continuity checks. Because that evidence chain is already accepted and current, the next lawful step is the aggregate-only `E4` decision gate rather than reopening `E1` selection, `E2` implementation, or `E3` verification.

The current boundary remains fixed by the accepted `C4` / `E1` / `E2` / `E3` artifacts and by the guider contract. The live subject stays repaired `URI-R2-C1`; the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary still applies; accepted `U2`, `U3`, and `U4` negative findings remain binding; and the bounded code/test ownership stays confined to the already accepted same-lane retained-child lane in `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`. `E4` must therefore record exactly one bounded next-step result for that already reverified lane only: `continue-bounded`, `widen-approved`, or `stop-blocked`.

`Bugs.md` still carries open replay-path bug `BUG-2026-03-16-001`, but it remains replay-lane continuity context only and does not authorize replay reopen, `MLF.Elab.Inst` / `InstBot` work, or broader automatic recursive inference selection in this round. Current repository status is already non-pristine (`orchestrator/state.json` modified and `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked), so selecting the narrow docs-only aggregate `E4` decision gate best respects existing work while advancing the lowest unfinished bounded roadmap item.

## Round Scope Guard

- This round is limited to roadmap item `E4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary unless the roadmap is explicitly amended first.
- Decide exactly one bounded next-step result for the already reverified same-lane retained-child local-`TypeRef` lane frozen by `E1`, implemented by `E2`, and reverified by `E3`: `continue-bounded`, `widen-approved`, or `stop-blocked`.
- Preserve the accepted bounded ownership and evidence anchors in `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`; do not reopen `E1` selection, `E2` implementation, or `E3` verification.
- Treat accepted `U2`/`U3`/`U4` negative findings as still binding; do not reinterpret them as authority, uniqueness, owner-stability, or constructor-feasibility clearance.
- Do not introduce replay-lane reopen, `MLF.Elab.Inst` / `InstBot` edits, equi-recursive reasoning, cyclic structural encoding, multi-SCC widening, cross-family widening, compatibility fallbacks, convenience widening paths, or a second executable interface in this round.
