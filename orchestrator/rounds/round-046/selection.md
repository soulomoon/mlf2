# Round 046 Selection

Date: 2026-03-19
Round: `round-046`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 13: execute the `G1` continue-bounded bind and exact next-slice target selection for repaired `URI-R2-C1` after the accepted `F4 = continue-bounded` decision for the local-binding scheme-alias/base-like `F2` / `F3` baseline.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `active_round_id: round-046`, `stage: select-task`, `current_task: null`, and `retry: null`, so no same-round retry is active and no prior review outcome forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmap.md` marks items 1 through 12 done, leaving item 13 (`G1`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `G1` the next lawful selection. Items 14 through 16 depend on item 13 and are therefore not yet selectable.

The accepted predecessor chain fixes both the timing and the bounded scope for this round. `orchestrator/rounds/round-045/review-record.json` finalized `F4` as authoritative with `stage_id: "F4"`, `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and `artifact_path: "docs/plans/2026-03-19-uri-r2-c1-f4-next-cycle-decision-gate.md"`. That authoritative `F4` decision recorded result token `continue-bounded`, so the previous bounded cycle is closed and the next lawful step is the next cycle's bind/selection stage `G1`, not `G2` implementation, `G3` verification, `G4` decision, or any widened work.

The roadmap and accepted `F4` decision also constrain what `G1` may do now. The live subject stays repaired `URI-R2-C1`, and `G1` must freeze exactly one next bounded non-widening `G2` slice drawn from at most one still-unopened `keepTargetFinal` trigger family (`rootHasMultiInst` or `instArgRootMultiBase`) in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`. Running `G1` now preserves that continue-bounded ordering while keeping the campaign inside the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.

This selection does not choose widened work. `G1` is a bind-and-selection stage only: it freezes one exact next bounded slice under repaired `URI-R2-C1`, but it does not itself implement, verify, amend the roadmap, reopen accepted `E2` / `E3` or `F1` / `F2` / `F3` / `F4` history, or authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening, non-local binding widening, or broader recursive-inference work.

`Bugs.md` records replay-path bug `BUG-2026-03-16-001` as resolved, but it remains replay-lane continuity context only and does not authorize widening or replay-lane reopen here. The active `round-046` packet is docs-only and non-pristine only because the round-local `G1` artifact and `orchestrator/rounds/round-046/` files are present in the dedicated round worktree; no `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`, `orchestrator/state.json`, or `orchestrator/roadmap.md` edit is required to keep this selection lawful. Selecting the narrow docs-only `G1` bind step therefore still best advances the lowest unfinished roadmap item without rewriting unrelated work.

## Round Scope Guard

- This round is limited to roadmap item `G1` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary unless the roadmap is explicitly amended first.
- Freeze exactly one next bounded non-widening `G2` slice drawn from at most one still-unopened `keepTargetFinal` trigger family (`rootHasMultiInst` or `instArgRootMultiBase`) in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`.
- Do not reopen the accepted `E2` / `E3` same-lane retained-child baseline or the accepted `F1` / `F2` / `F3` / `F4` local-binding scheme-alias/base-like chain.
- Do not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget` widening, non-local binding widening, equi-recursive reasoning, cyclic structural encoding, a second executable interface, or a compatibility or convenience fallback path in this round.
