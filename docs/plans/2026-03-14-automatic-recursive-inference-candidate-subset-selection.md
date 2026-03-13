# Automatic Recursive-Inference Candidate Subset Selection (Roadmap Item 3)

Date: 2026-03-14  
Round: 003  
Status: docs-only selection artifact for roadmap item 3.

## Inherited Baseline (Items 1 and 2)

- Automatic recursive-type inference remains unresolved.
- Current behavior remains explicit-only.
- The explicit-only / non-equi-recursive / non-cyclic-graph boundary is mandatory.
- This round does not authorize solver or inference behavior changes.

Continuity references:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`
- `tasks/todo/2026-03-11-recursive-types-orchestration/` (reference-only predecessor packet truth)

## Selected Candidate Subset (Exactly One)

Candidate ID: `ARI-C1`

Name: Annotation-anchored recursive-shape propagation (single SCC, single recursive binder family).

In-scope boundary for `ARI-C1`:

- Only cases where recursive shape is anchored by an explicit recursive annotation path.
- Single recursive SCC under one binder family; no cross-family SCC linking.
- Research-only contract for future bounded spike; no enablement in this round.

Out-of-scope boundary for `ARI-C1`:

- No fully unannotated recursive-type synthesis.
- No equi-recursive equality/unfolding.
- No cyclic graph representation in `TyNode`/constraint graph structures.
- No silent widening beyond explicit-only ingress paths.

## Deferred/Rejected Alternatives (This Roadmap Stage)

Deferred:

- Broad automatic inference for fully unannotated recursive SCCs is deferred as out-of-bounds for this stage.

Rejected for this stage:

- Any equi-recursive option requiring implicit recursive unfolding/equality.
- Any option that introduces non-cyclic-graph violations via structural graph cycles.
- Multi-candidate parallel spikes (`ARI-C2+`) in the same round; exactly one candidate is active.

## Bounded Spike Research Plan for Item 4 (Verifier-Visible Gates)

### Preconditions (must all hold before spike execution)

- Item 1 baseline contract and item 2 invariant audit remain accepted and unchanged in authority.
- Spike scope remains `ARI-C1` only; no second candidate activated.
- Proposed spike changes are opt-in research scaffolding and not default-on behavior.
- Boundary statement is copied into spike plan verbatim: explicit-only / non-equi-recursive / non-cyclic-graph.

### Success evidence (feasible/continue)

- Verifier-visible evidence that candidate examples with explicit recursive anchors can be processed under `ARI-C1` constraints without broadening to unannotated inference.
- Evidence that acyclicity, binding-tree validity, occurs-check behavior, and reconstruction obligations remain within item-2 audit bounds.
- Explicit proof that no equi-recursive reasoning and no cyclic graph encoding were introduced.
- Clear review record that behavior remains gated and non-default.

### Failure / no-go triggers

- Any requirement to infer recursion for fully unannotated programs to make examples pass.
- Any dependence on equi-recursive unfolding/equality.
- Any need for cyclic structural graph edges to encode recursion.
- Any evidence of boundary drift from explicit-only ingress.
- Any inability to produce verifier-visible, reproducible evidence for the above success criteria.

### Stop conditions (immediate halt)

- Scope expansion proposal beyond `ARI-C1`.
- Detection of non-equi-recursive boundary breach.
- Detection of non-cyclic-graph boundary breach.
- Discovery that the spike would silently alter default behavior.

## Verification Checklist (Reviewer-Facing)

| Check | Expected |
| --- | --- |
| Single candidate declaration | Exactly one `Candidate ID` line and it is `ARI-C1` |
| Alternative handling | Fully unannotated broad inference marked deferred; equi-recursive and cyclic options marked rejected |
| Boundary preservation | Document explicitly states explicit-only / non-equi-recursive / non-cyclic-graph |
| Spike gates | Preconditions, success evidence, failure/no-go triggers, and stop conditions are all present |
| Continuity | Item-1/item-2 docs and predecessor packet are referenced, not rewritten |

## Round 003 Decision Record

- Chosen candidate subset: `ARI-C1` only.
- Broader alternatives are deferred or rejected for this roadmap stage.
- Boundaries remain explicit-only / non-equi-recursive / non-cyclic-graph.
- This artifact is docs-only and introduces no production behavior change.
