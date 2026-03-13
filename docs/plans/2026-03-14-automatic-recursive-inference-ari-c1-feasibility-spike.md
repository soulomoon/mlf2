# Automatic Recursive-Inference `ARI-C1` Feasibility Spike (Roadmap Item 4)

Date: 2026-03-14  
Round: 004  
Status: prototype-only + docs feasibility spike (no production recursive-inference enablement)

## Inherited Constraints (Items 1-3)

- Item 1 baseline contract: explicit-only recursive support is the current baseline; automatic recursive inference remains unresolved and disabled.
- Item 2 invariant audit: acyclicity, binding-tree discipline, occurs-check termination, and reconstruction/witness obligations remain mandatory.
- Item 3 candidate selection: exactly one bounded candidate is active (`ARI-C1`), and broader alternatives remain deferred/rejected.

Active boundary (verbatim and mandatory): **explicit-only / non-equi-recursive / non-cyclic-graph**.

## `ARI-C1` Hypothesis

Within one recursive binder family and one SCC, annotation-anchored recursive shape propagation is feasible to characterize without widening behavior: an explicit recursive annotation path remains processable, while unannotated and out-of-scope variants remain non-inferred or rejected.

## Experiment Matrix (Bounded Characterization)

| Case ID | Shape | Scope | Expected outcome | Boundary-safety reason |
| --- | --- | --- | --- | --- |
| C1-A | `ELamAnn "x" (mu a. a -> Int) (EVar "x")` | In-scope anchor path | Pass/processable | Recursive shape is explicitly provided at ingress; no automatic synthesis needed. |
| C1-B | `ELam "x" (EVar "x")` | In-scope contrast (unannotated variant) | Pass but **not inferred** as recursive | Confirms no silent widening from explicit-only to automatic recursive inference. |
| C1-C | `ELam "f" (EApp (EVar "f") (EVar "f"))` | Intentionally out-of-scope proxy (would require unannotated recursive-style synthesis across self-reference) | Reject/fail (unresolved) | Confirms boundary blocks automatic recursive inference for out-of-scope cases. |

## Round Outcomes Contract

Success outcome (`feasible-continue`):
- C1-A remains processable with explicit anchor.
- C1-B does not gain inferred `mu`.
- C1-C remains rejected/unresolved.
- No evidence of equi-recursive reasoning or cyclic graph encoding.

No-go outcome (`no-go/not-yet-go`):
- C1-A cannot be processed without widening solver behavior.
- C1-B starts inferring recursive shape automatically.
- C1-C only passes when enabling forbidden widening (equi-recursive or cyclic encoding).

Stop outcome (immediate halt):
- Any proposal to broaden beyond `ARI-C1`.
- Any breach of explicit-only / non-equi-recursive / non-cyclic-graph.
- Any hidden default behavior change enabling automatic recursive inference.

## No Hidden Widening Checklist

- [x] Evidence lives in bounded characterization tests only: `test/PipelineSpec.hs` (`describe "ARI-C1 feasibility characterization (bounded prototype-only)"`).
- [x] No solver default changes, strategy rewiring, or API expansion were introduced.
- [x] No edits to `orchestrator/roadmap.md`.
- [x] No edits to predecessor packet authoritative logs under `tasks/todo/2026-03-11-recursive-types-orchestration/`.
