# Requirements Document

## Introduction
This spec captures the structural alignment between the current Haskell implementation and
the thesis in `papers/these-finale-english.txt`. The thesis is the source of truth. The scope
is the end-to-end pipeline (constraint generation through elaboration). Parser/CLI/UI
behavior is out of scope. Deviations are called out explicitly so they can be implemented
or documented with tests.

## Requirements

### Requirement 1: Graphic constraint data model
**User Story:** As a maintainer, I want the constraint graph to match the thesis definition, so that
solver phases operate on paper-faithful structures.

#### Acceptance Criteria
1. WHEN Definition 9.2.1 describes a constraint as a term-DAG plus binding edges and bind flags THEN THE SYSTEM SHALL model constraints with explicit type nodes, binding parents, and bind flags. (Status: present; Evidence: `papers/these-finale-english.txt:9609`, `src/MLF/Constraint/Types.hs:172`, `src/MLF/Constraint/Types.hs:429`, `src/MLF/Constraint/Types.hs:49`)
2. IF binding edges must form a tree and binders must be "upper" than their children THEN THE SYSTEM SHALL validate binding-tree invariants and enforce the upper-than check. (Status: present; Evidence: `papers/these-finale-english.txt:9459`, `src/MLF/Binding/Tree.hs:7`, `src/MLF/Binding/Tree.hs:1222`)
3. WHEN gen nodes represent schemes and scopes THEN THE SYSTEM SHALL represent gen nodes and scheme roots separately from type nodes. (Status: present; Evidence: `papers/these-finale-english.txt:9330`, `src/MLF/Constraint/Types.hs:110`, `src/MLF/Binding/Tree.hs:125`)
4. WHEN instantiation edges express that a type is an instance of a scheme THEN THE SYSTEM SHALL store instantiation edges and their endpoints in the constraint graph. (Status: present; Evidence: `papers/these-finale-english.txt:9552`, `src/MLF/Constraint/Types.hs:358`, `src/MLF/Frontend/ConstraintGen/Translate.hs:64`)

### Requirement 2: Constraint generation from eMLF terms
**User Story:** As a maintainer, I want term-to-constraint translation to follow the thesis, so that
generated constraints match paper examples.

#### Acceptance Criteria
1. WHEN Section 9.1.2 explains application and abstraction constraints THEN THE SYSTEM SHALL allocate arrow structure and emit instantiation edges for applications. (Status: present; Evidence: `papers/these-finale-english.txt:9270`, `src/MLF/Frontend/ConstraintGen/Translate.hs:49`, `src/MLF/Frontend/ConstraintGen/Translate.hs:65`)
2. WHEN Section 9.1.3 introduces gen nodes for let generalization THEN THE SYSTEM SHALL introduce gen nodes for let-bound schemes and rebind RHS scope under them, with per-use expansion nodes. (Status: present; Evidence: `papers/these-finale-english.txt:9330`, `src/MLF/Frontend/ConstraintGen/Translate.hs:77`, `src/MLF/Constraint/Types.hs:243`)
3. WHEN Definition 15.2.10 assumes the alternative let typing (trivial scheme root) THEN THE SYSTEM SHALL construct a trivial scheme root and instantiation edge for the let body. (Status: present; Evidence: `papers/these-finale-english.txt:17320`, `src/MLF/Frontend/ConstraintGen/Translate.hs:90`)

### Requirement 3: Expansion and presolution pipeline
**User Story:** As a maintainer, I want expansion and presolution to follow the thesis, so that
instantiation choices and witnesses are paper-faithful.

#### Acceptance Criteria
1. WHEN Definition 10.1.1 defines expansion THEN THE SYSTEM SHALL model expansion variables and recipes and materialize expansions in presolution, eliminating expansion wrappers afterward. (Status: present; Evidence: `papers/these-finale-english.txt:10549`, `src/MLF/Constraint/Types.hs:243`, `src/MLF/Constraint/Presolution/Driver.hs:77`, `src/MLF/Constraint/Presolution/Driver.hs:313`)
2. WHEN presolution requires a well-founded dependency order THEN THE SYSTEM SHALL build an instantiation-edge dependency graph and process edges topologically. (Status: present; Evidence: `papers/these-finale-english.txt:10882`, `src/MLF/Constraint/Acyclicity.hs:42`, `src/MLF/Elab/Run.hs:39`)
3. IF an instantiation edge has a non-expansion left-hand side THEN THE SYSTEM SHALL still implement the paper instance relation (not force equality). (Status: partial; Evidence: `src/MLF/Constraint/Presolution/Driver.hs:981`)
4. WHEN raising is required to align scopes before merging THEN THE SYSTEM SHALL emit Raise operations for interior nodes in witnesses. (Status: partial; Evidence: `papers/these-finale-english.txt:9443`, `src/MLF/Constraint/Types.hs:526`)

### Requirement 4: Translatable presolutions and inert nodes
**User Story:** As a maintainer, I want presolutions to satisfy the thesis translation preconditions,
so that elaboration to xMLF is valid.

#### Acceptance Criteria
1. WHEN Definition 15.2.10 enumerates translatable presolution conditions THEN THE SYSTEM SHALL validate those conditions (no inert-locked nodes, rigid scheme roots, rigid arrows, rigid non-interior nodes). (Status: present; Evidence: `papers/these-finale-english.txt:17320`, `src/MLF/Constraint/Presolution/Driver.hs:119`)
2. WHEN inert and inert-locked nodes are defined THEN THE SYSTEM SHALL compute them and support weakening to remove inert-locked nodes. (Status: present; Evidence: `src/MLF/Constraint/Inert.hs:1`, `src/MLF/Constraint/Inert.hs:111`)

### Requirement 5: xMLF elaboration and instantiation witnesses
**User Story:** As a maintainer, I want the explicit language to match the thesis, so that elaborated
terms and instantiations are faithful.

#### Acceptance Criteria
1. WHEN Figures 14.2.1-14.2.5 define xMLF types, terms, and computations THEN THE SYSTEM SHALL model these structures in elaboration data types. (Status: present; Evidence: `papers/these-finale-english.txt:14620`, `src/MLF/Elab/Types.hs:23`)
2. WHEN Figure 14.2.6 defines explicit instantiation judgments THEN THE SYSTEM SHALL type-check instantiation computations for elaborated terms. (Status: present; Evidence: `papers/these-finale-english.txt:14796`, `src/MLF/Elab/TypeCheck.hs:25`)
3. WHEN Section 15.3 translates presolutions using instantiation contexts and Phi THEN THE SYSTEM SHALL translate edge witnesses to xMLF instantiations using context steps and named nodes. (Status: present/partial; Evidence: `papers/these-finale-english.txt:17386`, `src/MLF/Elab/Phi.hs:31`, `src/MLF/Constraint/Types.hs:551`)
