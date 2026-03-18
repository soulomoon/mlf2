# Unannotated Iso-Recursive Inference Continue-Bounded Follow-On Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed initial successor cycle whose accepted execution record ends at `orchestrator/rounds/round-033`.
- Completed rounds `round-001` through `round-034` remain inherited baseline and predecessor evidence.
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
- The approved design source for this follow-on cycle is `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`.
- The accepted first follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`, finalized in `round-034` as the authoritative `C1` bind/selection record.
- The accepted second follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`, finalized in `round-035` as the authoritative `C2` fail-closed local-binding-only retention hardening record.
- The accepted third follow-on bounded-cycle artifact is `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`, finalized in `round-036` as the authoritative `C3` bounded verification/evidence consolidation record for the accepted local-binding-only fail-closed retention slice.
- This control plane continues to use `contract_version: 2` retry semantics from `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` and `orchestrator/retry-subloop.md`.
- The long-horizon direction remains fully unannotated iso-recursive-type synthesis in the solver/pipeline, but the live campaign must still move by bounded evidence and bounded implementation slices only.
- The live subject remains fixed to repaired `URI-R2-C1`.
- The accepted `U6` result `continue-bounded` means the next cycle must stay non-widening: it may continue with another bounded slice, but it may not reinterpret accepted negative findings as clearance or silently widen the subject/boundary.

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

4. [pending] Execute the bounded `C4` next-cycle decision gate for the verified repaired `URI-R2-C1` local-binding-only fail-closed slice
   Depends on: item 3
   Completion notes: complete when `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md` is accepted, the reviewer record names the authoritative `C4` attempt, and the round records exactly one bounded next-step result for the already-verified repaired `URI-R2-C1` local-binding-only fail-closed lane: `continue-bounded`, `widen-approved`, or `stop-blocked`, without reopening `C1` selection, `C2` implementation, `C3` verification, or silently widening the live subject.
