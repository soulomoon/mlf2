# Automatic Recursive-Type Inference Baseline Contract (Round 001)

Date: 2026-03-14  
Status: accepted as a docs-only inherited baseline and acceptance contract for roadmap item 1.

## Inherited baseline

The inherited baseline for this successor orchestration track is:

- Explicit-only, acyclic `TyMu` support is complete.
- Automatic recursive-type inference remains unresolved and disabled.
- The explicit-only / non-equi-recursive / non-cyclic-graph boundary remains mandatory.

Interpretation for this track:

- Recursive types are currently admitted only when explicitly annotated.
- Recursive-type meaning remains iso-recursive and explicit; no implicit unfolding semantics are introduced.
- Constraint-graph representation must stay structurally acyclic; recursive usage is represented via binders/variables, not graph cycles.

## What automatic recursive-type inference means in this repo

For this repo, “automatic recursive-type inference” means solver/pipeline behavior that can synthesize recursive types for programs that do not carry explicit recursive annotations.

Concrete in-scope meaning for future research discussion:

- Candidate behavior would infer a recursive type shape from unannotated terms.
- Candidate behavior would surface that recursive result in reconstructed/elaborated output without requiring explicit source `mu`.
- Candidate behavior would require clear constraints on soundness, principality, and termination before any implementation is attempted.

Explicit exclusions in the current baseline:

- No synthesis of recursive types from unannotated programs.
- No fallback path that silently inserts inferred recursion when explicit recursive annotations are absent.
- No expansion of current explicit annotation transport semantics into broad solver-wide recursive inference.

## Non-goals (this round and current baseline)

- No solver-wide recursive inference implementation in this round.
- No equi-recursive equality semantics.
- No cyclic graph encoding of recursion in constraint nodes/edges.
- No silent widening from explicit annotations to inferred recursion.
- No implementation work under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.

## Evidence-before-spike acceptance contract

Before any later code-changing spike is allowed, all of the following must exist and be review-visible:

1. Invariant audit document (roadmap item 2 output) covering threatened properties:
   - acyclicity assumptions,
   - binding/tree discipline,
   - occurs-check and unification termination assumptions,
   - reconstruction/reification obligations,
   - principality/termination risk boundaries.
2. Candidate subset selection document (roadmap item 3 output) that chooses exactly one bounded subset and explicitly rejects/defer alternatives.
3. Verifier-checkable success/failure gates for the spike, including:
   - what evidence counts as “feasible,”
   - what outcomes force “no-go / not-yet-go,”
   - what boundaries must remain unchanged (`explicit-only`, `non-equi-recursive`, `non-cyclic-graph`) during the spike.
4. Round plan approval that keeps the spike bounded and non-silent:
   - no hidden behavior widening,
   - no automatic enablement of recursive inference outside the selected subset,
   - no takeover-history rewrite.

If any prerequisite above is missing, a code-changing recursive-inference spike is not authorized.

## Continuity and predecessor evidence

This contract preserves predecessor milestone truth and references historical evidence without rewriting authoritative logs:

- `tasks/todo/2026-03-11-recursive-types-orchestration/`
- `docs/plans/2026-03-11-recursive-types-roadmap.md`
- `docs/plans/2026-03-13-m6-pipeline-feasibility-spike.md`
- `docs/plans/2026-03-13-m7-tymu-design-resolution.md`

Continuity statement:

- The predecessor campaign established explicit-only recursive-type transport and left automatic recursive-type inference unresolved by design.
- This document records that inherited boundary as the contract baseline for successor research rounds.
