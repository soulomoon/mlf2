# Automatic Recursive-Type Inference Research Roadmap

## Context

- This top-level `orchestrator/` succeeds the completed recursive-types campaign under `tasks/todo/2026-03-11-recursive-types-orchestration/`.
- Inherited baseline on `codex/automatic-recursive-type-inference`: the prior roadmap through explicit/core/runtime/XMLF/surface/pipeline `TyMu` transport is complete, but automatic recursive-type inference is still unresolved.
- This roadmap tracks only the post-takeover research problem: whether any sound, thesis-defensible subset of automatic recursive-type inference should move beyond the current explicit-only boundary.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Write the inherited-baseline and acceptance contract for automatic recursive-type inference
   Depends on: completed recursive-types campaign under `tasks/todo/2026-03-11-recursive-types-orchestration/`
   Completion notes: completed by accepted round 001 (squash-merged as `154f788`), documented in `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`; it preserves the inherited explicit-only, acyclic `TyMu` baseline, keeps automatic recursive-type inference unresolved, and records evidence gates required before any code-changing spike.

2. [done] Audit thesis and solver invariants threatened by automatic recursive-type inference
   Depends on: item 1
   Completion notes: completed by accepted round 002 (squash-merged as `07b0815`), documented in `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`; the audit captures acyclicity, binding, occurs-check/termination, reconstruction/reification/witness, and principality risks with concrete module obligations while preserving the explicit-only / non-equi-recursive / non-cyclic-graph boundary.

3. [pending] Select one bounded candidate subset and research plan
   Depends on: item 2
   Completion notes: one candidate subset/representation is chosen for evaluation, alternatives are explicitly deferred or rejected, and the verifier-visible success/failure criteria for the spike are written down.

4. [pending] Execute a bounded feasibility spike for the selected subset
   Depends on: item 3
   Completion notes: the spike produces concrete evidence, docs, and if needed a narrowly-scoped prototype showing whether the chosen subset can be pursued without silently enabling broader recursive-type inference.

5. [pending] Decide implementation handoff or research stop
   Depends on: item 4
   Completion notes: either the orchestrator writes a concrete implementation-handoff target with explicit boundaries, or it records a no-go / not-yet-go decision and why the research track stops there.
