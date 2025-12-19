# Requirements Document

## Introduction
This feature aligns the implementation with `papers/xmlf.txt` by representing the *binding tree* explicitly (binding edges for every node, with flexible/rigid flags) and by making the ω operations in Ω executable transformations on the expansion graph χe. The immediate objective is to support **paper-general `Raise(n)`** (not limited to `TyVar` rank adjustment) and to have presolution **execute and record** those raises so they can be translated by Φ.

Out of scope for this spec (can be a follow-up once parity is reached): fully retiring the current scope model (`TyVar.tnVarLevel`, `cGNodes` / `GNode.gBinds`) and rewriting all reification logic to use binding edges only.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want constraints to carry paper-style binding edges for all nodes, so that raising/weakening can be implemented as real graph transformations.

#### Acceptance Criteria
1. WHEN the system constructs a `Constraint` from source terms THEN THE SYSTEM SHALL assign a binding edge (parent + flag) for every non-root node in the constraint (where “root node” means a term-dag root: a node with no incoming structure edge).
2. WHEN the system allocates or copies a type node during normalization/presolution/solve THEN THE SYSTEM SHALL create corresponding binding-edge metadata for the new node(s) in a way that preserves the binding-tree invariants.
3. IF a phase that requires binding-edge information encounters a node without a binding edge THEN THE SYSTEM SHALL fail with a structured error that identifies the offending node.
4. WHEN binding edges exist THEN THE SYSTEM SHALL be able to compute the set of roots as exactly the nodes that lack a binding edge.
5. WHEN binding edges exist THEN THE SYSTEM SHALL ensure the set of binding roots equals the set of term-dag roots (nodes with no incoming structure edge).

### Requirement 2
**User Story:** As a developer, I want a single shared binding-tree API with invariant checks, so that multiple phases can manipulate binding edges safely and consistently.

#### Acceptance Criteria
1. WHEN `checkBindingTree` is run on a constraint THEN THE SYSTEM SHALL validate: every non-root node has exactly one parent, the parent pointers are acyclic, and every parent is “upper” than its child via the term-DAG structure (i.e., parent can reach child by following structure edges).
2. WHEN a binding-tree invariant is violated THEN THE SYSTEM SHALL return an error value that includes enough information to diagnose the failure (at minimum: node id(s) and the failed condition).
3. WHEN applying any binding-tree transformation (Raise/Weaken) THEN THE SYSTEM SHALL preserve all binding-tree invariants.

### Requirement 3
**User Story:** As a paper-alignment implementer, I want expansion χe to copy and bind nodes using the paper’s interior definition I(g), so that presolution and Φ can reason about “inside the edge” exactly.

#### Acceptance Criteria
1. WHEN presolution expands an instantiation edge `e` into χe THEN THE SYSTEM SHALL copy exactly the nodes “structurally strictly under g and in I(g)” and preserve binding edges/flags for copied nodes.
2. WHEN presolution records an `EdgeTrace` for an instantiation edge THEN THE SYSTEM SHALL set `etInterior` to the exact interior `I(r)` (all nodes transitively bound to the expansion root `r`), not an approximation.
3. WHEN `etInterior` is computed THEN THE SYSTEM SHALL include both copied nodes and any shared nodes that are transitively bound to `r` (as per the paper definition).

### Requirement 4
**User Story:** As a solver developer, I want ω operations to be executable against χe and recorded in Ω, so that the witness is both operational and translatable.

#### Acceptance Criteria
1. WHEN the system applies `Weaken(n)` THEN THE SYSTEM SHALL update the binding-tree flag on `n`’s binding edge from flexible to rigid (flex → rigid) and SHALL NOT change term-DAG structure.
2. WHEN the system applies `Raise(n)` THEN THE SYSTEM SHALL update the binding edge of `n` according to the paper’s “slide over” semantics (raising one step toward the root) and SHALL NOT change term-DAG structure.
3. WHEN `Raise(n)` is applied repeatedly THEN THE SYSTEM SHALL be able to raise `n` to an arbitrary ancestor binder in a finite number of steps, recording one `OpRaise n` per step.

### Requirement 5
**User Story:** As a presolution implementer, I want presolution to emit `OpRaise` for arbitrary interior nodes (not only scheme binders), so that witnesses match the paper’s normalized Ω.

#### Acceptance Criteria
1. WHEN edge-local unification in χe performs any Raise steps on nodes inside `I(r)` THEN THE SYSTEM SHALL record those steps as `OpRaise` operations for the raised node(s) (with multiplicity).
2. WHEN presolution records a witness Ω THEN THE SYSTEM SHALL ensure Ω respects the paper’s normalization constraint that no operation is performed under rigidly bound nodes (i.e., operations under rigid binders are absent or rejected).
3. WHEN presolution emits Raise operations THEN THE SYSTEM SHALL be able to replay those raises as graph transformations and reach the same binding-tree state as produced by the solver.

### Requirement 6
**User Story:** As an elaboration implementer, I want Φ to translate `OpRaise` for non-binder nodes using real instantiation contexts, so that applying Φ yields the solved edge type.

#### Acceptance Criteria
1. WHEN Φ translates `OpRaise(n)` for a node `n` that is not on the leading quantifier spine THEN THE SYSTEM SHALL compute the appropriate instantiation context(s) using binding edges and ≺ (leftmost-lowermost) ordering.
2. WHEN Φ produces an instantiation for an edge witness THEN THE SYSTEM SHALL satisfy: applying the instantiation to the source scheme/type yields a type α-equivalent to the solved target type for that edge.
3. WHEN Φ must choose the insertion point `m = min≺ {…}` (Figure 10) THEN THE SYSTEM SHALL compute `m` deterministically from the edge-local ≺ ordering and the binding structure.

### Requirement 7
**User Story:** As a maintainer, I want strong automated verification of binding-tree and Raise semantics, so that paper-faithful changes do not regress silently.

#### Acceptance Criteria
1. WHEN running `cabal test` THEN THE SYSTEM SHALL include property-based tests that generate small binding trees/term DAGs and validate `checkBindingTree`.
2. WHEN running `cabal test` THEN THE SYSTEM SHALL include property-based tests that validate `Raise`/`Weaken` preserve invariants and behave consistently with the chosen semantics.
3. WHEN running `cabal test` THEN THE SYSTEM SHALL include at least one regression scenario where presolution emits `OpRaise` for a non-binder interior node and Φ translates it successfully.
