# Automatic Recursive-Inference Item 5 Handoff Decision (`ARI-C1`)

Date: 2026-03-14  
Round: 005  
Roadmap item: 5. Decide implementation handoff or research stop  
Decision outcome: **implementation-handoff**

## Decision Statement

Round 004 concluded `feasible-continue` for `ARI-C1`, and no new contradictory evidence was discovered in round 005.  
Therefore roadmap item 5 resolves to **implementation-handoff** (not `no-go/not-yet-go`), with strict non-widening boundaries preserved:

**explicit-only / non-equi-recursive / non-cyclic-graph**

## Evidence Chain (Items 1-4)

1. Baseline contract (item 1) established inherited truth: explicit-only recursive transport is active, automatic recursive inference unresolved/disabled, and no silent widening is allowed.
2. Invariant audit (item 2) identified mandatory obligations across acyclicity, binding-tree discipline, occurs-check/unification termination, and reconstruction/witness replay contracts.
3. Candidate selection (item 3) selected exactly one candidate (`ARI-C1`), bounded to annotation-anchored ingress with single-SCC/single-binder-family shape, and deferred/rejected broader alternatives.
4. Feasibility spike (item 4) recorded outcome `feasible-continue` for bounded characterization and explicitly kept out-of-scope cases non-inferred/rejected without equi-recursive or cyclic-graph widening.

Continuity note: this round references predecessor packet truth and round-001..004 artifacts; it does not rewrite predecessor authoritative history.

## `ARI-C1` Implementation-Handoff Target (Concrete and Non-Widening)

Authorized target for the next implementation round:

- Candidate: `ARI-C1` only.
- Ingress boundary: annotation-anchored recursive-shape handling only (no fully unannotated recursive synthesis path).
- Shape boundary: single recursive binder family, single SCC.
- Semantic boundary: iso-recursive behavior only; no equi-recursive equality/unfolding.
- Representation boundary: no cyclic structural graph encoding.
- Enablement boundary: no default-on broadening; no silent widening beyond explicit ingress.

### Explicit Exclusions (Must Remain Out of Scope)

- Fully unannotated recursive-type synthesis.
- Equi-recursive reasoning/unfolding for solver success.
- Cyclic `TyNode`/constraint-graph representation of recursion.
- Any default-on behavior widening outside bounded `ARI-C1`.

## First-Touch Implementation File Set (Authorized Entry Surface)

The subsequent implementer is authorized to touch this initial bounded set first:

- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Constraint/Solve/Worklist.hs`
- `src/MLF/Constraint/Unify/Decompose.hs`
- `src/MLF/Reify/Type.hs`
- `test/PipelineSpec.hs`
- `test/PresolutionSpec.hs`

No implication is made that all files must change; this is the approved first-touch boundary.

## Mandatory Stop Triggers (Revert to Research; Do Not Continue Implementation)

Stop and return to `no-go/not-yet-go` research state immediately if any of the following occurs:

1. Implementation requires recursive synthesis for fully unannotated programs to satisfy target cases.
2. Implementation requires equi-recursive unfolding/equality to make `ARI-C1` pass.
3. Implementation introduces or depends on cyclic structural graph edges/nodes to encode recursion.
4. Implementation cannot preserve existing acyclicity, binding-tree, occurs-check termination, or replay/reification invariants from item-2 obligations.
5. Implementation forces default-on behavior widening or cannot keep `ARI-C1` strictly bounded.
6. Evidence becomes non-reproducible or unverifiable against the bounded gates established in items 3 and 4.

## Handoff Signal

Item-5 final signal: **implementation-handoff (`ARI-C1`, strict bounded mode)**.
