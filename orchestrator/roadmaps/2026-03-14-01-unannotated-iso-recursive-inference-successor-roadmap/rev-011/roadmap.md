# Unannotated Iso-Recursive Inference Successor Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed `URI-R2-C1` replay repair track whose accepted execution record ends at `orchestrator/rounds/round-027`.
- Completed rounds `round-001` through `round-027` remain inherited baseline and predecessor evidence.
- The completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` remains immutable predecessor evidence.
- The authoritative inherited automatic-recursive baseline and narrowing documents are:
  - `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
  - `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- The repaired predecessor result that controls entry into this successor track is:
  - `URI-R2-C1 repair track = repair-accepted`
  - authoritative terminal artifact: `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- The approved design source for this successor track is `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`.
- This control plane continues to use `contract_version: 2` retry semantics from `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` and `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-011/retry-subloop.md`.
- The long-horizon direction is fully unannotated iso-recursive-type synthesis in the solver/pipeline, but the live campaign must move by bounded evidence and bounded implementation slices only.
- The initial live subject stays fixed to the repaired `URI-R2-C1` lane. Later accepted roadmap updates may refine future pending items or append another bounded cycle, but they may not rewrite completed-item truth or silently widen the live subject.
- `U1` is now finalized as accepted in `round-028`; the authoritative reviewer record is `orchestrator/rounds/round-028/review-record.json`.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Execute the `U1` inherited baseline and repaired-subject bind for unannotated iso-recursive inference
   Depends on: completed rounds `round-001` through `round-027`, especially the accepted automatic-recursive baseline/handoff documents and the completed `URI-R2-C1` repair track
   Completion notes: completed in `round-028` (`accepted + finalize`) with authoritative record `orchestrator/rounds/round-028/review-record.json`, accepted artifact `docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`, repaired `URI-R2-C1` explicitly bound as the live subject, and reviewer-visible hard-stop triggers preserved against broad automatic recursive inference.

2. [done] Execute the `U2` provenance-stable unannotated authority clearance for the live subject
   Depends on: item 1
   Completion notes: completed in `round-029` (`accepted + finalize`) with authoritative record `orchestrator/rounds/round-029/review-record.json`, accepted artifact `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`, and bounded result token `authority-narrowed` that keeps the live subject fixed to repaired `URI-R2-C1` without widening.

3. [done] Execute the `U3` uniqueness and owner-stability clearance for the live subject
   Depends on: item 2
   Completion notes: completed in `round-030` (`accepted + finalize`) with authoritative record `orchestrator/rounds/round-030/review-record.json`, accepted artifact `docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`, and bounded result token `uniqueness-owner-stable-refuted` (no heuristic ranking, no subject widening, repaired `URI-R2-C1` still fixed as the live subject).

4. [done] Execute the `U4` constructor-directed / acyclicity / termination clearance for the live subject
   Depends on: item 3
   Completion notes: completed in `round-031` (`accepted + finalize`) with authoritative record `orchestrator/rounds/round-031/review-record.json`, accepted artifact `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`, and bounded result token `constructor-acyclic-termination-refuted` (repaired `URI-R2-C1` remains the fixed live subject; no equi-recursive, cyclic, fallback, or termination-weakened widening authorized).

5. [done] Execute the `U5` bounded solver/pipeline implementation slice for the still-bound live subject under the `U4` refuted result
   Depends on: item 4
   Completion notes: completed in `round-032` (`accepted + finalize`) with authoritative record `orchestrator/rounds/round-032/review-record.json`, accepted artifact `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`, and bounded result token `result-type-pipeline-hardening-slice-landed` (exactly one repaired-`URI-R2-C1` slice in `src/MLF/Elab/Run/ResultType/Fallback.hs` plus focused `test/PipelineSpec.hs` coverage, while preserving the inherited explicit-only / non-equi-recursive / non-cyclic boundary and the accepted `U4` fail-closed non-widening result).

6. [pending] Execute the `U6` end-to-end verification and next-widening decision gate
   Depends on: item 5
   Completion notes: complete when `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` is accepted, the reviewer record names the authoritative `U6` attempt, and the round records exactly one bounded next-step result: `continue-bounded`, `widen-approved`, or `stop-blocked`.
