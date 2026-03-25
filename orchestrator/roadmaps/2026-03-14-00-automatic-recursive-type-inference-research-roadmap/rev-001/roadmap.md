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

1. [pending] Write the inherited-baseline and acceptance contract for automatic recursive-type inference
   Depends on: completed recursive-types campaign under `tasks/todo/2026-03-11-recursive-types-orchestration/`
   Completion notes: a reviewed document defines the current explicit-only boundary, what “automatic recursive-type inference” means in this repo, the non-goals that stay out of scope, and the evidence required before any code-changing spike is allowed.

2. [pending] Audit thesis and solver invariants threatened by automatic recursive-type inference
   Depends on: item 1
   Completion notes: the audit records the specific acyclicity, binding, occurs-check, reconstruction, and principality/termination risks, along with the modules and proof obligations any later spike would have to respect.

3. [pending] Select one bounded candidate subset and research plan
   Depends on: item 2
   Completion notes: one candidate subset/representation is chosen for evaluation, alternatives are explicitly deferred or rejected, and the verifier-visible success/failure criteria for the spike are written down.

4. [pending] Execute a bounded feasibility spike for the selected subset
   Depends on: item 3
   Completion notes: the spike produces concrete evidence, docs, and if needed a narrowly-scoped prototype showing whether the chosen subset can be pursued without silently enabling broader recursive-type inference.

5. [pending] Decide implementation handoff or research stop
   Depends on: item 4
   Completion notes: either the orchestrator writes a concrete implementation-handoff target with explicit boundaries, or it records a no-go / not-yet-go decision and why the research track stops there.
