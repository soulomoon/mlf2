# Automatic Recursive-Inference Invariant Audit (Roadmap Item 2)

Date: 2026-03-14  
Round: 002  
Status: docs-only audit artifact for roadmap item 2.

## Inherited Baseline (from Item 1)

- Automatic recursive-type inference remains unresolved.
- Explicit-only recursive behavior is the active boundary.
- The explicit-only / non-equi-recursive / non-cyclic-graph boundary remains mandatory.

Interpretation for this audit:

- Explicit `mu` annotations may be transported through existing explicit paths.
- This audit does not authorize synthesis of recursive types for unannotated programs.
- Any future recursive-inference feasibility spike must satisfy the bounded obligations below before code changes.

## Threat Class 1: Acyclicity Assumptions (Graph Shape, No Cyclic Term-DAG Encoding)

### Invariants currently relied on

- Structural graph traversal is defined over `structuralChildren`; recursion is represented as binder/body structure, not structural back-edges.
- `TyMu` body traversal is one-way (`TyMu -> body`), preserving structural DAG traversal discipline.
- Phase 3 acyclicity requires an acyclic instantiation-edge dependency graph and rejects cycles.

### Enforced/consumed modules

- Enforced: `src/MLF/Constraint/Types/Graph/NodeEdge.hs` (`TyNode`, `structuralChildren`).
- Enforced/consumed: `src/MLF/Constraint/Traversal.hs` (`occursInUnder`, reachable traversals).
- Enforced: `src/MLF/Constraint/Acyclicity.hs` (`checkAcyclicity`, dependency graph + topo order).
- Consumed: `src/MLF/Elab/Run/Pipeline.hs` (Phase 3 gate before presolution).
- Regression evidence: `test/AcyclicitySpec.hs` (explicit `TyMu` remains acyclic).

### What automatic recursive inference would threaten

- If inferred recursion were encoded with structural cycles (instead of binder-mediated references), traversal/occurs/dependency assumptions would no longer hold.
- Phase 3 topological scheduling guarantees for presolution would be invalidated by cyclic dependency encodings.

### Bounded proof obligations before code changes

- Show inferred recursion can be represented without introducing structural cycles in `TyNode`.
- Show `checkAcyclicity` remains sound/complete for inferred-recursion candidates (no special cyclic escape hatch).
- Provide verifier-visible failing examples for cyclic encodings and passing examples for binder-mediated acyclic encodings.

## Threat Class 2: Binding/Tree Discipline Obligations

### Invariants currently relied on

- Binding tree is a separate structure (`BindParents`), with single-parent semantics and upper-parent validity.
- Binder enumeration and ordering (`orderedBinders`, scope-order keys) are required for quantifier/context-sensitive operations.
- Canonicalization and finalization preserve valid binding-parent relations.

### Enforced/consumed modules

- Enforced: `src/MLF/Constraint/Types/Graph/Binding.hs` (`BindParents`, `BindingError` contracts).
- Enforced: `src/MLF/Binding/Tree.hs` (`checkBindingTree`, `orderedBinders`, `isUpper`, canonicalized bind-parent lookups).
- Enforced/consumed: `src/MLF/Constraint/Solve/Worklist.hs` (pre/post binding-tree checks).
- Enforced/consumed: `src/MLF/Constraint/Solve/Finalize.hs` (bind-parent rewrite/repair/validation).
- Consumed: `src/MLF/Elab/Phi/Translate.hs` and `src/MLF/Elab/Phi/Context.hs` (binder-domain replay and instantiation contexts).

### What automatic recursive inference would threaten

- Synthesizing recursive binders outside explicit annotation ingress can create binder ownership ambiguity (scheme owner, parent upperness, replay-domain mismatch).
- Untracked inferred binders could break ordered binder assumptions used by witness translation and context construction.

### Bounded proof obligations before code changes

- Prove inferred recursive binders are inserted with valid parent/upper relations under existing binding-tree invariants.
- Prove binder ordering (`orderedBinders` + order keys) remains deterministic for inferred-recursive cases.
- Prove no new `BindingError` classes appear in presolution/finalization/elaboration replay paths under inferred-recursive inputs.

## Threat Class 3: Occurs-Check and Unification-Termination Risks

### Invariants currently relied on

- Variable-vs-structure unification paths apply occurs checks before union.
- Shared unification decomposition is constructor-directed; `TyMu` only decomposes against `TyMu` (no unfolding equation).
- Presolution/simplification paths also defend against self-reachability through structural occurs queries.

### Enforced/consumed modules

- Enforced: `src/MLF/Constraint/Solve/Worklist.hs` (`occursCheck` using `Traversal.occursInUnder`).
- Enforced: `src/MLF/Constraint/Unify/Core.hs` (policy-driven occurs checks).
- Enforced: `src/MLF/Constraint/Unify/Decompose.hs` (`TyMu` constructor-only decomposition).
- Enforced/consumed: `src/MLF/Constraint/Normalize.hs` (occurs checks in graft paths; `TyMu` rewrite under UF).
- Enforced/consumed: `src/MLF/Constraint/Presolution/EdgeProcessing/Solve.hs` (occurs checks during presolution edge solving).
- Regression evidence: `test/SolveSpec.hs`, `test/Presolution/ExpansionSpec.hs`.

### What automatic recursive inference would threaten

- Inferred recursive equations can introduce hidden self-reference obligations not currently admitted by explicit-only ingress.
- Any shift from constructor-only `TyMu` matching toward implicit unfolding risks non-termination or unsound fixed-point unification behavior.

### Bounded proof obligations before code changes

- Prove inferred-recursive candidate constraints still satisfy existing occurs-check failure conditions (or explicitly reject cases) without changing termination class.
- Prove unification on inferred-recursive cases terminates with current constructor-directed decomposition rules.
- Prove no equi-recursive unfolding is required to decide inference outcomes in the candidate subset.

## Threat Class 4: Reconstruction / Reification / Witness Obligations

### Invariants currently relied on

- Annotation result-type reconstruction flows through generalization + scheme translation + no-fallback reification.
- Reifier requires valid binder shape for `TyMu` (exactly one binder child) and emits explicit `TMu`.
- Per-edge witness + trace domains must stay synchronized and replay-contract valid before Phi translation.

### Enforced/consumed modules

- Consumed: `src/MLF/Elab/Run/ResultType/Ann.hs` (`computeResultTypeFromAnnWithView`, `generalizeWithPlan`, `schemeToType`, `reifyTypeWithNamedSetNoFallback`).
- Enforced: `src/MLF/Reify/Type.hs` (`TyMu` reification with binder-shape checks).
- Enforced: `src/MLF/Constraint/Presolution/Driver.hs` (missing witness/trace checks; replay-map contract validation).
- Enforced contracts: `src/MLF/Constraint/Types/Witness.hs` (`EdgeWitness`, `ReplayContract`).
- Consumed/enforced: `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/Context.hs` (binding-tree + replay-domain invariants for witness translation).

### What automatic recursive inference would threaten

- If recursion is inferred without explicit annotation anchors, reconstruction may lack stable authoritative roots for replay/generalize/reify.
- Witness-domain consistency can break if inferred recursive binders/nodes are produced outside existing presolution provenance contracts.

### Bounded proof obligations before code changes

- Prove inferred-recursive outputs can roundtrip through `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback` without fallback widening.
- Prove witness/traces remain total and domain-synchronized for all non-trivial inferred-recursive edges.
- Prove Phi replay contract checks remain strict-pass for inferred-recursive candidate traces.

## Threat Class 5: Principality and Termination Risk Boundaries

### Invariants currently relied on

- Presolution runs only after acyclicity ordering, and computes minimal expansions in topological order.
- Presolution finalization enforces no residual unification/instantiation/TyExp artifacts.
- Existing notes explicitly avoid rules known to weaken principal solutions in non-ML settings.

### Enforced/consumed modules

- Enforced/consumed: `src/MLF/Constraint/Acyclicity.hs` (well-founded edge order prerequisite).
- Enforced: `src/MLF/Constraint/Presolution/Driver.hs` (phase ordering + residual artifact checks).
- Enforced contracts: `src/MLF/Constraint/Presolution/Base.hs` (`Residual*` errors, principality caution in notes).
- Consumed by pipeline: `src/MLF/Elab/Run/Pipeline.hs`.
- Regression surface: `test/PresolutionSpec.hs` (principal-presolution contract checks).

### What automatic recursive inference would threaten

- Inferred recursion can expand search space beyond current explicit-layer assumptions and may require non-local solver choices that weaken principality.
- Any inference path that requires recursive unfolding/equality reasoning risks breaking current termination and minimal-expansion assumptions.

### Bounded proof obligations before code changes

- Prove the candidate inferred-recursion subset preserves principal-presolution outcomes (or explicitly proves/records any restricted non-principal boundary).
- Prove termination of the full pipeline on the subset with unchanged phase ordering and without residual artifacts.
- Prove no new solver path introduces recursive inference fallback outside explicitly scoped candidate cases.

## Strict Boundaries (Mandatory, Unchanged)

The following boundaries are strict and non-negotiable for future spikes unless explicitly superseded by a new accepted roadmap decision:

- No equi-recursive reasoning.
- No cyclic graph encoding for recursive types.
- No silent widening from explicit recursive annotations to inferred recursion.
- No automatic enablement of recursive inference outside a verifier-approved bounded subset.

## Continuity and Predecessor Evidence (Reference-Only)

This round references predecessor evidence without mutation:

- `tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`
- `tasks/todo/2026-03-11-recursive-types-orchestration/task_plan.md`
- `docs/plans/2026-03-13-m6-pipeline-feasibility-spike.md`
- `docs/plans/2026-03-13-m7-tymu-design-resolution.md`

Continuity statement:

- The predecessor packet truth is preserved as historical authority.
- This audit adds verifier-checkable invariant obligations for roadmap item 2 only.
- No predecessor packet history/log content was rewritten.
