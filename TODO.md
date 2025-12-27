# MLF Type Inference & Elaboration — TODO

See [roadmap.md](roadmap.md) for the full algorithm description and paper references (especially `papers/xmlf.txt`).

---

## Phase 1 — Constraint Generation ✅

- [x] Graphic type nodes (`TyVar`, `TyArrow`, `TyBase`, `TyForall`, `TyExp`)
- [x] Binding nodes (`TyForall`) + binding edges (`Constraint.cBindParents`)
- [x] Expansion nodes (`TyExp`) for let-bindings
- [x] Instantiation edges (`InstEdge`)
- [x] Constraint container (`Constraint`)
- [x] `inferConstraintGraph :: Expr -> Either TypeError ConstraintResult`
- [x] Literals produce `TyBase` nodes
- [x] Lambda parameters bound at surrounding binder
- [x] Applications emit instantiation edges
- [x] Let-bindings introduce child binders (`TyForall`)
- [x] Expansion variables shared across multiple uses of same binding
- [x] Variable shadowing / lexical scoping
- [x] Unknown variable error reporting

**Tests:** 23 examples, all passing (`cabal test`)

---

## Phase 2 — Normalize / Local Transformations ✅

- [x] Simplify trivial instantiation/unification edges (T ≤ T, T = T)
- [x] `normalize :: Constraint -> Constraint` with fixed-point iteration
- [x] `dropReflexiveInstEdges`, `dropReflexiveUnifyEdges` helpers
- [x] Grafting: copy structure onto variables when demanded by `InstEdge`
- [x] Merging: process `UnifyEdge`s via union-find
- [x] `graftInstEdges`, `mergeUnifyEdges` helpers
- [x] `NormalizeState` with fresh node allocation and union-find

**Tests:** 16 examples, all passing

---

## Phase 3 — Acyclicity Check ✅

- [x] Build instantiation dependency graph
- [x] Topological sort of `InstEdge` list
- [x] Cycle detection (DFS)
- [x] `isAcyclic :: Constraint -> Bool`
- [x] `checkAcyclicity :: Constraint -> Either CycleError AcyclicityResult`
- [x] `collectReachableNodes` for dependency analysis
- [x] `AcyclicityResult` with sorted edges and dependency graph

**Tests:** 41 examples (was 27 new, total 92), all passing

---

## Phase 4 — Principal Presolution ✅

- [x] Topological processing of `InstEdge`s using `AcyclicityResult`
- [x] Minimal expansion lattice implemented: `ExpIdentity`, `ExpInstantiate`, `ExpForall`, `ExpCompose`
- [x] `decideMinimalExpansion` covers forall↔forall (re-gen), forall→structure (instantiate), structure→forall (wrap), structure→structure (identity+unify)
- [x] `applyExpansion`/`instantiateScheme` to realize expansions and graft fresh nodes
- [x] Incremental unification inside presolution loop
- [x] Tests: `test/PresolutionSpec.hs` covers identity, instantiate, forall-intro, and compose (instantiate→forall)

## Phase 5 — Unification Solver ✅

- [x] `Solve` entrypoint `solveUnify :: Constraint -> Either SolveError SolveResult`
- [x] Robust union-find: reuse canonical NodeId representatives and path compression
- [x] Structural unification cases: Var=Var, Var=Structure, Arrow=Arrow, Base=Base, Forall=Forall
- [x] Occurs check on DAG
- [x] Error reporting
- [x] Tests: success cases and failure cases

---

## Phase 6 — Elaboration to xMLF (New Foundation) ⏳

Based on `papers/xmlf.txt` §3.

- [ ] **Define xMLF AST** (`XMLF.AST`)
    - [ ] Types `τ` (including `∀(α ≥ τ)`)
    - [ ] Instantiations `φ` (Witnesses: `!α`, `N`, `O`, `Inside`, `Under`, `Comp`)
    - [ ] Terms `a` (including `Λ` and `a φ`)
- [ ] **Implement Elaboration Logic** (`Elaborate.hs`)
    - [ ] `elaborate :: Expr -> Presolution -> Constraint -> XMLF.Term`
    - [ ] Generate instantiation witnesses `φ` from graph paths
    - [ ] Insert `Λ` abstractions based on presolution expansions
    - [ ] Insert explicit type annotations on lambda arguments
- [ ] **Tests**
    - [ ] Verify elaboration of basic terms (id, const)
    - [ ] Verify elaboration of polymorphic let-bindings

---

## Phase 7 — xMLF Execution & Verification ⏳

Based on `papers/xmlf.txt` §1 & §2.

- [ ] **Type Checker** (`XMLF.Check`)
    - [ ] Implement `Γ ⊢ a : τ` rules
    - [ ] Verify elaborated terms are well-typed
- [ ] **Evaluator** (`XMLF.Eval`)
    - [ ] Implement small-step reduction `a ⟶ a'`
    - [ ] Implement instantiation reduction rules (e.g., `(Λ...) N ⟶ ...`)

---

## Stretch Goals / Future Work

- [ ] Pretty-printer for xMLF terms
- [ ] Visualization of constraint graph (Graphviz / DOT)
- [ ] REPL that prints the inferred type and the elaborated xMLF term
