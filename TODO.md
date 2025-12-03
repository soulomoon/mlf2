# MLF Type Inference Implementation — TODO

See [roadmap.md](roadmap.md) for the full algorithm description and paper references.

---

## Phase 1 — Constraint Generation ✅

- [x] Graphic type nodes (`TyVar`, `TyArrow`, `TyBase`, `TyForall`, `TyExp`)
- [x] G-nodes / generalization levels with parent/child forest
- [x] Expansion nodes (`TyExp`) for let-bindings
- [x] Instantiation edges (`InstEdge`)
- [x] Constraint container (`Constraint`)
- [x] `inferConstraintGraph :: Expr -> Either TypeError ConstraintResult`
- [x] Literals produce `TyBase` nodes
- [x] Lambda parameters registered at surrounding G-node level
- [x] Applications emit instantiation edges
- [x] Let-bindings create child G-nodes
- [x] Expansion variables shared across multiple uses of same binding
- [x] Variable shadowing / lexical scoping
- [x] Unknown variable error reporting

**Tests:** 23 examples, all passing (`cabal test`)

---

## Phase 2 — Normalize / Local Transformations

- [ ] Grafting and merging nodes
- [ ] Collapse identity expansions
- [ ] Push / pull G-nodes to canonical positions
- [ ] Simplify trivial instantiation/unification edges
- [ ] `normalize :: Constraint -> Constraint`

---

## Phase 3 — Acyclicity Check

- [ ] Build instantiation dependency graph
- [ ] Topological sort of `InstEdge` list
- [ ] Cycle detection (DFS)
- [ ] `isAcyclic :: Constraint -> Bool`

---

## Phase 4 — Principal Presolution

- [ ] Dependency graph between `InstEdge`s
- [ ] `decideMinimalExpansions` (core routine)
- [ ] Presolution map (`ExpVar -> Expansion`)
- [ ] Apply expansions and generate unification edges
- [ ] `computePresolution :: Constraint -> (Presolution, Constraint')`

---

## Phase 5 — Unification Solver

- [ ] Union-find over node IDs
- [ ] Structural checking for constructors
- [ ] Occurs-check
- [ ] `solveUnify :: Constraint -> Substitution`

---

## Phase 6 — Elaboration

- [ ] Replay presolution to produce explicit `Λ` binders
- [ ] Insert type instantiations `t[T]`
- [ ] `elaborate :: AnnotatedExpr -> Presolution -> Substitution -> xMLFTerm`

---

## Stretch Goals / Future Work

- [ ] Better error messages with source locations
- [ ] Pretty-printer for graphic constraints
- [ ] Visualization of constraint graph (Graphviz / DOT)
- [ ] Incremental / interactive mode
- [ ] Benchmark suite
