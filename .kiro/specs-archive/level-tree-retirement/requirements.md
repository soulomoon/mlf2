# Requirements Document

## Introduction
The codebase still carries two parallel scope models: the paper-style binding tree
(`cBindParents`, `BindFlag`) and the legacy level tree (`cGNodes`, `GNodeId`,
`tnVarLevel`, `tnOwnerLevel`, `tnQuantLevel`). The binding tree now drives
Raise/Merge/Weaken and interior computations, while the level tree is mostly
bookkeeping. This spec retires the level tree entirely so the binding tree is
the sole source of scope, binder enumeration, and quantifier behavior.

Scope of this spec:
- remove `GNodeId`/`GNode`/`cGNodes`/`cGForest` and all level fields
- derive binder lists and bounds from binding edges + `cVarBounds`
- exclude inert (unreachable) binders from quantification
- preserve binder-to-binder bounds when introducing fresh quantifiers
- update constraint generation, presolution, normalization/solve, and elaboration
- keep alignment with `papers/xmlf.txt` (Q(n), leftmost-lowermost ordering, Raise/Weaken semantics)

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want to remove the level-tree data model so
scope is tracked only by binding edges and there is no duplicate state to keep
in sync.

#### Acceptance Criteria
1. WHEN the data model is compiled THEN THE SYSTEM SHALL not define or export
   `GNodeId`, `GNode`, `cGNodes`, or `cGForest`.
2. WHEN running `rg -n "GNodeId|GNode|cGNodes|cGForest|tnVarLevel|tnOwnerLevel|tnQuantLevel|GNodeOps" src test`
   THEN THE SYSTEM SHALL return no matches.
3. WHEN building the library (`cabal build`) THEN THE SYSTEM SHALL succeed
   without level-tree types or fields.

### Requirement 2
**User Story:** As a solver developer, I want binder enumeration to be derived
from binding edges, matching the paper's Q(n) definition.

#### Acceptance Criteria
1. WHEN enumerating binders for a quantifier node `q` THEN THE SYSTEM SHALL
   return TyVar nodes directly bound to `q` by flexible edges AND reachable
   from `q`'s body via term-DAG structure.
2. WHEN binding edges are updated by Raise/Weaken THEN THE SYSTEM SHALL reflect
   those updates in binder enumeration with no cached lists.
3. WHEN a binder has an instance bound in `cVarBounds` THEN THE SYSTEM SHALL
   attach that bound to the binder order used for quantifier introduction.
4. WHEN a binder bound refers to another binder at the same quantifier THEN THE
   SYSTEM SHALL preserve that dependency so `ExpForall` can remap it onto fresh
   binders.

### Requirement 3
**User Story:** As a constraint-generation maintainer, I want scope to be
represented by a binding-root `NodeId` so new nodes are bound to the correct
scope without GNode levels.

#### Acceptance Criteria
1. WHEN generating constraints for `let` THEN THE SYSTEM SHALL bind all nodes
   created in the RHS scope that are reachable from the RHS root to the
   let-introduced `TyForall` node while keeping the binding-parent "upper"
   invariant; nodes not reachable from the RHS root SHALL keep their structural
   binding parents.
2. WHEN generating constraints for lambda THEN THE SYSTEM SHALL keep the same
   scope root (monomorphic binding) for the body.
3. WHEN internalizing explicit `forall` in source types THEN THE SYSTEM SHALL
   bind its variables to the `TyForall` node and store their bounds in
   `cVarBounds`.
4. WHEN inspecting `AnnExpr` THEN THE SYSTEM SHALL carry a `NodeId` scope root
   (not `GNodeId`).
5. WHEN generating constraints THEN THE SYSTEM SHALL pass
   `Binding.checkBindingTree` on the resulting constraint graph.

### Requirement 4
**User Story:** As a presolution implementer, I want expansions and instantiation
rules to use binding-edge binders so quantifier behavior remains paper-faithful
without levels.

#### Acceptance Criteria
1. WHEN deciding minimal expansions for `forall` vs structure THEN THE SYSTEM
   SHALL compare binder shapes derived from binding edges, not level IDs.
2. WHEN applying `ExpInstantiate` THEN THE SYSTEM SHALL use the ordered binder
   list derived from binding edges at the quantified node and exclude inert
   binders (unreachable from the body).
3. WHEN applying `ExpForall` THEN THE SYSTEM SHALL introduce a `TyForall` node
   and bind (rebind) reachable binder vars according to the `ForallSpec`,
   applying bounds and remapping `BoundBinder` indices onto the chosen binders.
   If the body does not yet contain enough binder candidates (prior to unifying
   with the target forall), THE SYSTEM SHALL not crash and SHALL defer missing
   binders/bounds to later unification.

### Requirement 5
**User Story:** As an elaboration implementer, I want quantifier construction and
ordering to come from binding edges so xMLF translation does not depend on
levels.

#### Acceptance Criteria
1. WHEN elaborating a solved type THEN THE SYSTEM SHALL introduce quantifiers
   based on binding edges (Q(n)), excluding inert binders, and order them using
   leftmost-lowermost ordering from the quantifier body root.
2. WHEN a variable was eliminated during presolution THEN THE SYSTEM SHALL skip
   it when building quantifier lists.

### Requirement 6
**User Story:** As a test maintainer, I want property tests to validate the new
binding-edge-based binder behavior and expansion invariants.

#### Acceptance Criteria
1. WHEN running property tests for binder enumeration THEN THE SYSTEM SHALL
   confirm that binders are exactly the flexibly bound children of the quantifier
   node that are reachable from its body.
2. WHEN running expansion tests THEN THE SYSTEM SHALL confirm that `ExpForall`
   binds/rebinds reachable binders with the expected bounds (including
   binder-to-binder remapping) and binding parents.
3. WHEN running `cabal test --test-show-details=direct` THEN THE SYSTEM SHALL
   pass.
