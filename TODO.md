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

## Phase 2 — Normalize / Local Transformations ✅

- [x] Simplify trivial instantiation/unification edges (T ≤ T, T = T)
- [x] `normalize :: Constraint -> Constraint` with fixed-point iteration
- [x] `dropReflexiveInstEdges`, `dropReflexiveUnifyEdges` helpers
- [x] Grafting: copy structure onto variables when demanded by `InstEdge`
- [x] Merging: process `UnifyEdge`s via union-find
- [x] `graftInstEdges`, `mergeUnifyEdges` helpers
- [x] `NormalizeState` with fresh node allocation and union-find

**Tests:** 16 examples, all passing

> **Note:** Identity expansion collapse is a Phase 4 concern (presolution decides
> `s := Identity`). G-node push/pull is a structural invariant enforced by the
> data types — G-nodes form a separate forest (`cGNodes`) and cannot appear
> inside type constructors like `TyArrow`; variables just *point* to their
> binding level via `tnLevel :: GNodeId`.

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
