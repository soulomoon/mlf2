# Unannotated Iso-Recursive Inference Continue-Bounded Follow-On Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed initial successor cycle whose accepted execution record ends at `orchestrator/rounds/round-033`.
- Completed rounds `round-001` through `round-041` remain inherited baseline and predecessor evidence.
- The completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` remains immutable predecessor evidence.
- The authoritative inherited automatic-recursive baseline and predecessor-boundary documents remain:
  - `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
  - `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- The completed initial successor-cycle artifacts that control entry into this follow-on cycle are:
  - `docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`
  - `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`
  - `docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`
  - `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`
  - `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
  - `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- The approved design source for the first follow-on cycle is `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`.
- The accepted first follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`, finalized in `round-034` as the authoritative `C1` bind/selection record.
- The accepted second follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`, finalized in `round-035` as the authoritative `C2` fail-closed local-binding-only retention hardening record.
- The accepted third follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`, finalized in `round-036` as the authoritative `C3` bounded verification/evidence consolidation record for the accepted local-binding-only fail-closed retention slice.
- The accepted fourth follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`, finalized in `round-037` as the authoritative `C4` next-cycle decision record for the verified repaired `URI-R2-C1` local-binding-only fail-closed slice.
- The accepted fifth follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`, finalized in `round-038` as the authoritative `E1` bind/selection record that freezes exactly one retained-child `boundVarTarget` / nested-`forall` fail-closed hardening slice under repaired `URI-R2-C1`.
- The accepted sixth follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`, finalized in `round-039` as the authoritative `E2` bounded retained-child implementation record for the repaired `URI-R2-C1` same-lane local-`TypeRef` / nested-`forall` fail-closed slice.
- The accepted seventh follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`, finalized in `round-040` as the authoritative `E3` bounded verification/evidence consolidation record for the accepted same-lane retained-child `E2` slice.
- The accepted eighth follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`, finalized in `round-041` as the authoritative `E4` next-cycle decision record for the accepted `E3`-reverified same-lane retained-child repaired `URI-R2-C1` slice with result token `continue-bounded`.
- This control plane continues to use `contract_version: 2` retry semantics from `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` and `orchestrator/retry-subloop.md`.
- The long-horizon direction remains fully unannotated iso-recursive-type synthesis in the solver/pipeline, but the live campaign must still move by bounded evidence and bounded implementation slices only.
- The live subject remains fixed to repaired `URI-R2-C1`.
- The accepted `U6` result `continue-bounded` bound the first follow-on cycle to stay non-widening, the accepted `C4` result `continue-bounded` queued one more bounded non-widening cycle under repaired `URI-R2-C1`, the accepted `E1` bind froze that next cycle to exactly one retained-child `boundVarTarget` / nested-`forall` fail-closed hardening slice in `ResultType.Fallback`, the accepted `E2` record captured the bounded same-lane local-`TypeRef` implementation result for that slice, the accepted `E3` record captured the bounded reverification/evidence result for that same slice, and the accepted `E4` result `continue-bounded` now queues one more bounded non-widening cycle that must begin with another exact bind/selection step under repaired `URI-R2-C1`; subsequent work may not reinterpret accepted negative findings as clearance or silently widen the subject/boundary.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Execute the `C1` continue-bounded bind and exact next-slice target selection for the still-bound live subject
   Depends on: completed rounds `round-001` through `round-033`, especially the accepted `U6` decision gate and the accepted `U5` bounded implementation slice
   Completion notes: completed in accepted `round-034`; `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md` is the authoritative `C1` attempt and binds exactly one next bounded non-widening target under repaired `URI-R2-C1` without reinterpreting accepted `U2`/`U3`/`U4` negative findings as clearance.

2. [done] Execute the `C2` bounded fail-closed local-binding-only result-type target-retention hardening slice frozen by `C1`
   Depends on: item 1
   Completion notes: completed in accepted `round-035`; `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md` is the authoritative `C2` attempt and `orchestrator/rounds/round-035/review-record.json` finalized `attempt-2` as the accepted authoritative record for the exact `C1`-frozen `rootBindingIsLocalType` local-`TypeRef` fail-closed hardening slice in `src/MLF/Elab/Run/ResultType/Fallback.hs`, with focused same-case coverage in `test/PipelineSpec.hs` across direct `computeResultTypeFallback`, unchecked `runPipelineElab`, and checked `runPipelineElabChecked`, while keeping wrapper/proxy-shaped retention fail-closed and preserving the repaired `URI-R2-C1` boundary.

3. [done] Execute the `C3` bounded verification and evidence consolidation gate for the accepted local-binding-only fail-closed retention slice
   Depends on: item 2
   Completion notes: completed in accepted `round-036`; `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md` is the authoritative `C3` attempt and `orchestrator/rounds/round-036/review-record.json` finalized `attempt-1` as the accepted authoritative record, confirming that the accepted `C2` `rootBindingIsLocalType` fail-closed `ResultType.Fallback` / `PipelineSpec` slice stays stable under the bounded `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, the current full-repo gate, and predecessor continuity checks, without widening beyond repaired `URI-R2-C1`.

4. [done] Execute the bounded `C4` next-cycle decision gate for the verified repaired `URI-R2-C1` local-binding-only fail-closed slice
   Depends on: item 3
   Completion notes: completed in accepted `round-037`; `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md` is the authoritative `C4` attempt and `orchestrator/rounds/round-037/review-record.json` finalized `attempt-1` as the accepted authoritative record, closing the first follow-on `C1` through `C4` cycle with result token `continue-bounded` for the already-verified repaired `URI-R2-C1` local-binding-only fail-closed lane, without reopening `C1` selection, `C2` implementation, `C3` verification, or widening the live subject/boundary.

5. [done] Execute the `E1` continue-bounded bind and exact next-slice target selection for repaired `URI-R2-C1` after the accepted local-binding-only fail-closed retention baseline
   Depends on: item 4
   Completion notes: completed in accepted `round-038`; `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md` is the authoritative `E1` attempt and `orchestrator/rounds/round-038/review-record.json` finalized `attempt-1` as the accepted authoritative record, binding the queued next cycle to repaired `URI-R2-C1` under accepted `C4 = continue-bounded`, preserving the explicit-only / non-equi-recursive / non-cyclic-graph boundary, and freezing exactly one next bounded non-widening `E2` slice: the local-binding-only retained-child `boundVarTarget` / nested-`forall` fail-closed hardening lane in `src/MLF/Elab/Run/ResultType/Fallback.hs:530-674` with future ownership limited to `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`, without reopening replay repair, `MLF.Elab.Inst`, or reinterpreting accepted `U2` / `U3` / `U4` negative findings as clearance.

6. [done] Execute the `E2` bounded fail-closed retained-child `boundVarTarget` / nested-`forall` implementation slice frozen by `E1`
   Depends on: item 5
   Completion notes: completed in accepted `round-039`; `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md` is the authoritative `E2` attempt and `orchestrator/rounds/round-039/review-record.json` finalized `attempt-2` as the accepted authoritative record, confirming that the exact `E1`-frozen repaired `URI-R2-C1` slice remained bounded to `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`, kept `rootBindingIsLocalType` as the mandatory gate, admitted retained-child survival only for the same canonical local `TypeRef` lane while `boundHasForallFrom` kept nested-`forall` / nested-owner crossings fail-closed, added one behavioral same-lane success example plus one matched fail-closed contrast in the existing bounded `ARI-C1 feasibility characterization (bounded prototype-only)` block, and left all other `keepTargetFinal` trigger families (`rootHasMultiInst`, `instArgRootMultiBase`, `rootIsSchemeAlias && rootBoundIsBaseLike`) and excluded widening lanes unchanged.

7. [done] Execute the `E3` bounded verification and evidence consolidation gate for the accepted same-lane retained-child `E2` slice
   Depends on: item 6
   Completion notes: completed in accepted `round-040`; `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md` is the authoritative `E3` attempt and `orchestrator/rounds/round-040/review-record.json` finalized `attempt-1` as the accepted authoritative record, confirming that the accepted repaired `URI-R2-C1` `E2` same-lane local-`TypeRef` retained-child slice remained stable under a rerun of the bounded `ARI-C1 feasibility characterization (bounded prototype-only)` block, a fresh full-repo `cabal build all && cabal test` gate, predecessor continuity checks, and docs-only diff scope, while preserving the bounded `Fallback.hs` / `PipelineSpec.hs` ownership and fail-closed exclusions without reopening implementation or widening.

8. [done] Execute the bounded `E4` next-cycle decision gate for the accepted `E3`-reverified same-lane retained-child repaired `URI-R2-C1` slice
   Depends on: item 7
   Completion notes: completed in accepted `round-041`; `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md` is the authoritative `E4` attempt and `orchestrator/rounds/round-041/review-record.json` finalized `attempt-1` as the accepted authoritative record, confirming that the accepted repaired `URI-R2-C1` same-lane retained-child local-`TypeRef` lane frozen by `E1`, implemented by `E2`, and reverified by `E3` remains non-widening with result token `continue-bounded`, while preserving the bounded `Fallback.hs` / `PipelineSpec.hs` ownership boundary, the inherited explicit-only / non-equi-recursive / non-cyclic-graph limits, and the rule that `widen-approved` and `stop-blocked` are not lawful on the accepted evidence chain.

9. [pending] Execute the `F1` continue-bounded bind and exact next-slice target selection for repaired `URI-R2-C1` after the accepted same-lane retained-child `E2` / `E3` baseline
   Depends on: item 8
   Completion notes: complete when one accepted `F1` artifact binds the queued next cycle to repaired `URI-R2-C1` under accepted `E4 = continue-bounded`, preserves the explicit-only / non-equi-recursive / non-cyclic-graph boundary, and freezes exactly one next bounded non-widening slice that starts from the accepted same-lane retained-child `boundVarTarget` / nested-`forall` fail-closed baseline without reopening replay repair, `MLF.Elab.Inst`, or reinterpreting accepted `U2` / `U3` / `U4` negative findings or `BUG-2026-03-16-001` continuity context as clearance.

10. [pending] Execute the `F2` bounded fail-closed implementation slice frozen by `F1`
    Depends on: item 9
    Completion notes: complete when the exact `F1`-frozen repaired `URI-R2-C1` slice lands inside its explicitly bound ownership with focused verification and preserved fail-closed behavior outside that slice, without equi-recursive reasoning, cyclic structural graph encoding, replay reopen, multi-SCC widening, cross-family widening, or compatibility/default-path widening.

11. [pending] Execute the `F3` bounded verification and evidence consolidation gate for the accepted `F2` slice
    Depends on: item 10
    Completion notes: complete when the accepted `F2` slice is reverified against the current bounded checks and predecessor continuity, and the authoritative record confirms that the repaired `URI-R2-C1` boundary still holds without widening.

12. [pending] Execute the bounded `F4` next-cycle decision gate for the accepted `F3`-reverified repaired `URI-R2-C1` slice
    Depends on: item 11
    Completion notes: complete when the accepted `F4` artifact and reviewer record name the authoritative `F4` attempt and the round records exactly one bounded next-step result for the already-reverified `F1`-frozen repaired `URI-R2-C1` lane: `continue-bounded`, `widen-approved`, or `stop-blocked`, without reopening `F1` selection, `F2` implementation, or `F3` verification.
