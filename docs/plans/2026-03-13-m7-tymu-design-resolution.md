# M7 `TyMu` Design Resolution

Date: 2026-03-13  
Milestone: M7 - Optional inference implementation  
Status: docs-only design resolution; current product behavior is unchanged.

## Decision

The recommended M7 path is narrow:

- keep the current `RecursiveAnnotationNotSupported` Phase 1 boundary on `master` until a later implementation attempt is ready;
- if recursive annotations cross the pipeline later, they do so through an acyclic first-class `TyMu` representation for explicit annotations only;
- keep `automatic recursive-type inference` disabled; and
- keep cyclic graph support and `equi-recursive` reasoning out of scope.

This keeps the thesis-faithful explicit layer (`STMu`/`TMu`/`XTMu`) as the semantic source of truth while defining the smallest graph/reify bridge that does not require a research-grade rewrite of the solver.

## Thesis and Current-Master Constraints

The design is constrained by both the thesis and the current implementation.

### Thesis-backed constraints

- The xMLF grammar in `papers/these-finale-english.txt:11245-11259` has variables, arrows, constructors, bounded `forall`, and bottom, but no `mu` form. Recursive types are therefore an extension, not a latent thesis feature.
- The thesis keeps type-instance evidence explicit and says xMLF type equivalence is essentially reflexive up to alpha-conversion (`papers/these-finale-english.txt:11293-11310`, `papers/these-finale-english.txt:14724-14736`). That does not justify implicit unfolding, so `equi-recursive` equality is the wrong first step.
- Graphic constraints require each structural slice to be an acyclic term-graph (`papers/these-finale-english.txt:7418`). A recursive-types plan that depends on cyclic term-DAGs crosses a core invariant.
- The thesis future-work note says recursive types plus second-order polymorphism are challenging and that even if cyclic term-graphs may be possible, the harder problem is recursion in the binding structure (`papers/these-finale-english.txt:15420-15424`). That is a warning against treating cyclic graph support as a small local follow-up.

### Current-master code evidence

- The constraint graph `TyNode` grammar is closed today: `TyVar`, `TyBottom`, `TyArrow`, `TyBase`, `TyCon`, `TyForall`, and `TyExp` only. There is no graph-side `TyMu`, and `structuralChildren` is correspondingly closed. See `src/MLF/Constraint/Types/Graph/NodeEdge.hs:225-300`.
- Phase 3 still assumes an acyclic dependency structure. `checkAcyclicity` rejects cyclic instantiation dependencies and its design note explicitly treats cycles as failure, not as a supported recursive feature. See `src/MLF/Constraint/Acyclicity.hs:43-100`.
- Annotation result-type reconstruction is already funneled through existing scheme and reify code, not through any recursive-type-specific escape hatch. `computeResultTypeFromAnnWithView` calls `generalizeWithPlan`, converts the result with `schemeToType`, and probes graph reification with `reifyTypeWithNamedSetNoFallback`. See `src/MLF/Elab/Run/ResultType/Ann.hs:68-145` and `src/MLF/Elab/Run/ResultType/Ann.hs:298-310`.
- `MLF.Reify.Type` `goFull` reconstructs only the current graph constructors: `TyBase`, `TyBottom`, `TyArrow`, `TyCon`, `TyForall`, and `TyExp`. There is no `TyMu` branch today. See `src/MLF/Reify/Type.hs:217-390`, especially `src/MLF/Reify/Type.hs:359-377`.

Together these facts explain why cyclic graphs and recursive-type inference remain out of scope for the smallest safe M7 slice: they would require new graph invariants, new reconstruction semantics, and likely new solver arguments that the thesis does not already justify.

## Recommended Non-Cyclic `TyMu` Path

The only recommended graph representation is an acyclic binder form:

- add a first-class `TyMu` node to the graph, analogous to `TyForall`, with a single structural child for the body;
- represent recursive occurrences inside the body as ordinary `TyVar` nodes bound under that `TyMu` binder through the existing binding tree, not by making the term-DAG cyclic;
- treat `TyMu` as an explicit iso-recursive constructor that reifies back to the already-supported elaborated `TMu`;
- admit it for explicit annotations only; never synthesize `TyMu` from inference, unification search, or solver fallback; and
- keep unfolding explicit-layer-only semantics unchanged: no automatic unfolding during equality, normalization, or result-type reconstruction.

In other words, the body of `mu a. tau` may mention the binder variable `a`, but the graph must still be structurally acyclic because the body points to a bound `TyVar`, not back to the enclosing `TyMu` node.

## Structural-Update Modules

These are the only structural modules that should move in the first implementation attempt:

1. `src/MLF/Constraint/Types/Graph/NodeEdge.hs`
   Add `TyMu` to `TyNode` and extend `structuralChildren` with the single-body case. The representation should mirror `TyForall`: binder ownership stays in the binding tree, not in cyclic structural edges.

2. `src/MLF/Constraint/Traversal.hs`
   Update graph traversals so `TyMu` is visited as a one-child structural node. This is mechanical exhaustiveness work needed for reachability, structural walks, and any helper that currently assumes the closed `TyNode` set.

3. `src/MLF/Reify/Type.hs`
   Extend `goFull` with a `TyMu` branch that reifies directly to `TMu`, preserving existing binder naming discipline rather than storing names in the graph itself.

4. `src/MLF/Elab/Run/ResultType/Ann.hs`
   Keep the existing reconstruction flow, but make it verifier-visible that `computeResultTypeFromAnnWithView`, `schemeToType`, and `reifyTypeWithNamedSetNoFallback` can roundtrip an explicit annotation that contains `TyMu`.

5. `src/MLF/Frontend/ConstraintGen/Translate.hs`
   This is the only intended ingress point. When the later implementation attempt is ready, lower explicit surface `STMu` annotations into graph `TyMu`; do not introduce any other producer of `TyMu`.

## Semantic-Update Modules

These modules carry the semantic guardrails and should stay as small as possible:

1. `src/MLF/Constraint/Acyclicity.hs`
   `checkAcyclicity` must remain valid without any special cyclic escape hatch. `TyMu` must not create structural self-cycles or a new dependency-graph notion of recursion.

2. `src/MLF/Constraint/Unify/Decompose.hs` and any directly adjacent structural matcher
   If annotation checking forces graph-side comparison of two `TyMu` nodes, the rule should be constructor-matching only: `TyMu` matches `TyMu` structurally and never unfolds into an `equi-recursive` equation.

3. `src/MLF/Constraint/Normalize.hs` and presolution/solve callers only if required by the previous item
   The permitted behavior is opaque transport, not recursive-type inference. No new solver path may invent `TyMu`, chase a cyclic equation, or weaken occurs-check/termination assumptions.

4. Existing explicit-layer type operations
   Reification should continue to target the already explicit `TMu` semantics rather than creating a second recursive-type meaning in the graph layer.

Any broader change than that is a separate research track, not the smallest safe M7 slice.

## Explicit Non-Goals

- No change in this round to `RecursiveAnnotationNotSupported`.
- No product-code changes under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- No cyclic `TyNode` representation.
- No `equi-recursive` equality or implicit unfolding.
- No solver-wide recursive unification work.
- No `automatic recursive-type inference`.

## Acceptance Criteria For A Later Implementation Attempt

The later implementation attempt should be considered acceptable only if all of the following are true and verifier-checkable:

1. Graph shape:
   `TyMu` exists in `src/MLF/Constraint/Types/Graph/NodeEdge.hs`, `structuralChildren` handles it as one child, and no code introduces structural cycles to encode recursion.

2. Phase 3 invariant:
   `checkAcyclicity` continues to pass without a recursive-type exception, and focused tests demonstrate that explicit `TyMu` annotations do not require cyclic dependency graphs.

3. Reconstruction path:
   an explicit annotated recursive type can cross the pipeline and come back through `computeResultTypeFromAnnWithView`, `schemeToType`, and `reifyTypeWithNamedSetNoFallback`, yielding `TMu` from `MLF.Reify.Type` rather than a fallback or an unfolded surrogate.

4. Scope boundary:
   the only accepted pipeline entrypoint is explicit annotated `mu`; unannotated programs still do not synthesize recursive types, and tests explicitly assert `explicit annotations only`.

5. Equality/inference boundary:
   no test or implementation step depends on `equi-recursive` equality, cyclic graph reasoning, or `automatic recursive-type inference`; any attempt that needs those properties fails this design gate and should be split into a new research milestone.
