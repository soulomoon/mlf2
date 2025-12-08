# Roadmap for Implementing Rémy & Yakobowski MLF Type Inference Algorithm

papers are in the papers directory

> **Implementation progress:** See [TODO.md](TODO.md) for the current checklist.

Excellent — below I give a self-contained, paper-faithful presentation of the Rémy & Yakobowski (ICFP 2008) algorithm for MLF type inference: definitions, the solver strategy, the data structures you need, and precise pseudocode for each stage. I base this on the PDF you asked me to read and I cite the paper at the end for the most important claims and definitions I rely on.  ￼

This answer is long but organised so you can (a) read the overview and then (b) follow the step-by-step algorithm/pseudocode and (c) consult the cited paper sections for formal proofs and details.

⸻

High level overview (what the algorithm achieves)
 • Input: an unannotated lambda-term e (ML-like language).
 • Output: a principal typing for MLF (or report ill-typed).
 • Approach: translate e compositionally to a graphic constraint χ (graph of nodes + G-nodes + instantiation and unification edges), transform χ into an equivalent constraint in solved form by a set of semantics-preserving graph transformations, compute a principal presolution (mapping of expansion variables / G-nodes to expansions), and finally elaborate the program into an explicitly typed xMLF term (System-F-like Λ and instantiations). This yields type soundness, subject reduction for the elaborated term, and principality.  ￼

Key paper sections: constraint language & translation (§1), solved form (§3), transformations (§4), solving strategy (§5), complexity (§7).  ￼

⸻

Core definitions you must implement / represent

These are the primitive data types the algorithm works with; keep them graph-structured (shared nodes), not tree-structured.

 1. Graphic type nodes (term dag nodes):
 • variable nodes (pseudo-symbol ⊥) — represent monotype variables (placeholders).
 • constructor nodes (arrow →, base types, etc.) — each has arity.
 • G-nodes (generalization nodes) — represent generalization levels, used for let-generalization. G-nodes are in the constraint “top part” and have binding edges to variable nodes. G-nodes are not allowed inside type expressions (they live above the type part).  ￼
 2. Expansion variables / special constructor s:
 • The paper uses expansions (often written s T in our conversation) to delay whether a type-position will be generalized or instantiated. In the graph these are explicit labeled nodes you can later replace by an expansion (a recipe).  ￼
 3. Edges in the constraint graph:
 • Unification edges: T1 = T2. They require two graphic types to be made identical (monotype equality).
 • Instantiation edges: T1 ≤ T2. They mean T1 can be instantiated so that it becomes an instance of T2 (this is how uses of polymorphic bindings are expressed).
 • Binding edges from variable nodes to G-nodes indicate the generalization level where the variable was bound.  ￼
 4. Solved form / presolution (informal):
 • A presolution is a map ρ assigning to each expansion variable s an expansion E (a recipe that either introduces ∀ at some levels or instantiates), such that applying ρ transforms the constraint into a graph where all instantiation edges are discharged (converted into unification edges) and the remaining unification problem is in a solved normal form. A principal presolution is the least-committing such ρ (every other solution is an instance of it).  ￼

⸻

Big picture solver strategy (as in the paper)

 1. Translate term e into a graphic constraint χ. (Section 1: compositional translation.)  ￼
 2. Normalize / simplify χ using local semantics-preserving transformations (grafting, merging, pushing G-nodes, canonicalization). These keep the semantics but put χ in a shape amenable to solving. (Section 4.)  ￼
 3. Check acyclicity: typing constraints produced from program are acyclic; the solver depends on acyclicity to guarantee termination and principality. If cycles exist they indicate unsolvable recursion of expansions. (Section 5.)  ￼
 4. Compute principal presolution ρ: [DONE]
 • Build a dependency graph of instantiation edges (which instantiation edges depend on the result of other instantiation edges).
 • Process instantiation edges in a topological order. For each edge T_left ≤ T_right choose the minimal expansion on the left that permits instantiation and generate the unification constraints that follow; after each instantiation, run unification (or incrementally maintain solved unification). This step ensures minimal instantiation (maximal generality) and gives principality. (Section 5.)  ￼
 5. Solve the unification graph of monotypes (graphic unification algorithm on DAGs) to a solved form (merged nodes, renaming). This is standard graph unification (the paper references Rémy’s earlier graphic unification work). (Section 3 & 7.)  ￼
 6. Elaborate: apply presolution ρ to the partial (annotated) term to produce explicit ∀ binders and instantiations t[T] (xMLF term). This is straightforward replay of choices stored during solving. (Section 6.)  ￼

⸻

Detailed algorithm and precise pseudocode

Below I give a precise step-by-step algorithm, with the main procedures you will need to implement. I use familiar names (graph nodes, constraints, expansions). For each operation I also indicate the paper section where the formal statement or correctness is proved.

Notation:
 • χ — a graphic constraint (a pair of: top-level G-nodes forest, and a type graph with edges).
 • InstEdge = (T_left, T_right, id) — a recorded instantiation edge with an identifier.
 • UnifyEdge = (T1, T2) — unification edge.
 • ExpVar — expansion variable s.
 • ρ — presolution map ExpVar -> Expansion.
 • depGraph — directed graph whose nodes are InstEdges; an edge e1 -> e2 means e2 depends on instantiation productions of e1.

I assume you store graphs with persistent node IDs and adjacency lists.

⸻

Phase 0 — Utilities & data structures
 • Representation of a graphic type node:

Node = { id : NodeId, kind : {Var | Arrow | Base(t) | Forall(level) | ExpVar(ev)} , succ : [NodeId] }

ExpVar(ev) is the s application node wrapping another node.

 • Constraint container:

Constraint = { Gnodes : [GNode], TypeGraphRoot : NodeId, InstEdges : [InstEdge], UnifyEdges : [UnifyEdge] }

 • Expansion (what a presolution assigns):

Expansion = Identity            -- do nothing
          | Forall(levels) ...  -- sequence of ∀-introductions at given levels
          | Inst               -- force an instantiation (remove Forall)
          | Compose [Expansion]

 • You must implement graphic unification: merging nodes, doing occurs-check with sharing. Rémy’s previous paper on graphic unification is used for proof; in code this is implemented by union-find with structural checks. (See §7.)  ￼

⸻

Phase 1 — Constraint generation

Pseudocode:

function generateConstraints(expr) -> Constraint χ:
  create fresh G-node root
  recursively:
    - variable occurrence x:
        create a variable node v
        add binding edge v -> Gnode where x was bound
        return node id v
    - lambda λx.e:
        create nodes for argument and body; tie argument var node to scope, produce arrow node with succ = [argnode, bodyNode]
    - application e1 e2:
        n1 = gen(e1); n2 = gen(e2)
        res = fresh variable node r
        add inst-edge: (n1, Arrow(n2, r))
        return r
    - let x = e1 in e2:
        create new G-node g (child of current G)
        n1 = gen(e1) in environment bound to g  -- the RHS is generated inside new G-node
        create ExpVar s for this let binding and represent scheme as s n1
        bind occurrences of x in e2 to the s n1 scheme (variable nodes pointing to s n1)
        n2 = gen(e2) in environment extended with that binding
        return n2
  return the built Constraint χ

Remarks:
 • This is the compositional translation from §1. Implement it carefully to maintain G-node scoping info (which meta-variables can be generalized at which G-level).  ￼

⸻

Phase 2 — Normalize / Local transformations

Apply the paper's small set of semantics-preserving transformations (§4) until none apply. You must implement these transformations as rewrite rules on the graph:
 • Grafting and merging: combine nodes that represent the same structural requirement (see definitions).
 • Collapse identity expansions: remove redundant s wrappers where safe.
 • Push / pull G-nodes: move G-nodes to canonical places so that type graph has G-nodes only at the top (they must not appear inside type nodes).
 • Simplify trivial instantiation/unification edges (e.g. if left is a variable node equal to right, drop).

> **Implementation note:** In our Haskell implementation, G-node push/pull is
> a structural invariant enforced by the data types, not a runtime transformation.
> G-nodes live in a separate forest (`cGNodes :: IntMap GNode`) and type nodes
> only *reference* their binding level via `tnLevel :: GNodeId`. This makes it
> impossible to have G-nodes nested inside type constructors like `TyArrow`.
> Identity expansion collapse is handled during Phase 4 (presolution) rather
> than normalization.

Pseudocode (iterative):

repeat
  changed = false
  for each transformation rule R in rules:
    if R applies to χ:
      χ := R(χ)
      changed = true
until not changed

These rewrites reduce the constraint to a canonical shape that simplifies later steps (see §4).  ￼

⸻

Phase 3 — Acyclicity check

We must ensure instantiation dependencies are acyclic. Build the instantiation dependency graph:
 • For each instantiation edge T1 ≤ T2, its production (if T1 is s applied to something) may introduce new instantiation edges. Create a graph of edges between instantiation edges if one’s produced unification/instantiation depends on metas that appear in the other.

Concrete test (paper uses a notion of acyclic constraints): run DFS on dependency graph built from χ. If cycle found, the constraints are not in the solvable acyclic class — algorithm rejects or handles specially. For standard ML-style programs the graph is acyclic. (See §5.)  ￼

⸻

Phase 4 — Principal presolution (the core)

This is the algorithmic heart. The paper’s strategy:

 1. Build InstEdge list = all T_left ≤ T_right instantiation edges in χ.
 2. Build dependency graph between InstEdges: an edge e1 -> e2 if solving e1 may produce unification/instantiation constraints that e2 needs (i.e. e2 uses nodes that may be instantiated by e1).
 3. Topologically sort InstEdges (because the graph is acyclic).
 4. Process InstEdges in that topological order; for each edge = (L ≤ R):
 • Compute minimal expansion E for every ExpVar that appears in L but is not yet fixed by presolution. The paper describes a constructive procedure that:
 • attempts to unify the shape of L against R by allowing only grafting (insertion of structure under variable nodes) and limited merging, and chooses the smallest expansion that lets L match R.
 • concretely: if L = s T and R expects an arrow A → B, the algorithm either:
 • decides s := inst (instantiate s producing T with fresh metas substituted for its quantifiers), which yields concrete unification edges, or
 • if T contains a nested G level that can be generalized, choose s := ⟂ or ∀ accordingly.
 • When E is determined for those s, record ρ(s) := E (extend presolution).
 • Apply the expansion(s) to L to produce the instantiated monotype L' and generate the unification edges L' = R (or new instantiation edges if nested expansions remain).
 • Immediately propagate resulting unification (call the unifier) and canonicalize the graph; collapsing nodes may reduce future dependencies.
 5. Continue until all InstEdges processed.

Important desiderata that the paper enforces:
 • At each step choose minimal expansions: do not generalize/instantiate more than required because minimality guarantees principality. The constructive local rule for choosing expansions is spelled out in §5 along with correctness.  ￼

Pseudocode (high-level):

function computePrincipalPresolution(χ):
  instList = χ.InstEdges
  depGraph = buildDependencyGraph(instList, χ)
  order = topologicalSort(depGraph)
  ρ = empty presolution map
  unificationState = initial unification state from χ.UnifyEdges

  for e in order:
    L, R = e.left, e.right
    -- apply current presolution to L and R (partial)
    Lp = applyPresolution(ρ, L)
    Rp = applyPresolution(ρ, R)
    if canSolveInstEdgeDirectly(Lp, Rp):
      (newExpAssigns, newUnifyEdges, newInstEdges) =
         decideMinimalExpansions(Lp, Rp, χ, unificationState)
      extend ρ with newExpAssigns
      add newUnifyEdges to unificationState
      add newInstEdges to χ.InstEdges (if any)  -- they will be later in order (acyclic)
      runUnificationAndSimplify(unificationState, χ)  -- maintain solved form invariant
    else:
      fail -- constraints unsatisfiable
  return ρ

Where decideMinimalExpansions is the key routine implementing Rémy & Yakobowski’s local expansion decision rule; it is based on:
 • inspecting Lp’s expansion variables and the expected constructor shape of Rp,
 • choosing instantiation or ∀ insertions only where required to match constructors,
 • producing the minimal set of fresh metas and graftings that make Lp an instance of Rp.
The paper provides formal rules and correctness proof.  ￼

⸻

Phase 5 — Unification solver (graphic unification)

After the presolution has generated (or converted) instantiation edges to plain unification edges, you must solve the unification problem on the graphic types:
 • Implement union-find over node IDs with structural checking:
 • When unify two nodes, you either merge them (if compatible constructors), or generate new equalities for successors (for arrow, unify corresponding children).
 • Occurs-check: prevent merging that would create cycles that break acyclicity (the system already assumes acyclic constraints).
 • The end result is a solved graph: node merges are done, shared nodes are represented by union-find representative; remaining unresolved variable nodes correspond to final meta-variables in principal solution.

Performance note: using union-find and hash-consing for structural comparison leads to linear-ish complexity under the reasonable assumptions in the paper. See §7 for complexity analysis and linearity claim.  ￼

⸻

Phase 6 — Produce solved form and elaborate
 • After unification completes, you have:
 • a unification substitution (node merges),
 • a presolution ρ mapping expansion variables to expansions,
 • possible remaining generalized variables recorded at particular G-levels.
 • Elaboration: replay the original annotated AST (you should have kept an annotated version during constraint generation) and:
 • whenever a let binding had s assigned by ρ, expand it into Λ quantifiers in the elaborated term where ρ(s) demanded them,
 • whenever an application used a polymorphic binding that had been instantiated, insert the explicit type instantiation t[T] with the concrete type chosen by the presolution (or with a metavariable instantiation replaced by the unifier’s result).
 • The result is an xMLF term with explicit type abstractions and instantiations; type-checking of that term is straightforward. (See §6.)  ￼

⸻

Correctness, principality and complexity (where to read in the paper)
 • Soundness & completeness of the translation and solving are proved in the paper: solved forms correspond exactly to sets of MLF typings and the presolution computed by the strategy is principal when constraints are acyclic (main theorems in §3–§5).  ￼
 • Complexity: Section 7 proves the algorithm is linear under common assumptions (amortized costs of union-find and graph traversals), matching ML’s Algorithm W complexity properties.  ￼

⸻

Implementation notes — where the paper’s details are essential

If you want to implement this fully and correctly you must pay special attention to:

 1. Exact graph representation: nodes must be shared (dags), not trees; merging/sharing semantics are central (graphic types).  ￼
 2. G-nodes / levels: generalization must be done relative to G-nodes (levels) and the solver must track which meta-variables are allowed to be quantified at which G-node. Implementation must respect these scoping invariants. (§1.2, §3.)  ￼
 3. Minimal expansion decision procedure: decideMinimalExpansions is the most intricate routine. The paper presents a constructive algorithm using grafting and sharing rules; follow §5 carefully. The correctness proof ensures the produced presolution is principal for acyclic constraints.  ￼
 4. Incrementality: as you process instantiation edges you must incrementally maintain unification solved form (merge nodes as you go), since merging can reduce or remove later instantiation constraints — this is crucial both for correctness and performance. (§5 and §7.)  ￼

⸻

Minimal worked blueprint (development plan / API)

If you want to go straight to code, here is a minimal set of modules/functions you should implement (in roughly this order):

 1. GraphTypes — node/graph representation (with hash-consing, union-find helpers)
 2. Constraints — generator from AST: generateConstraints :: Expr -> Constraint (keeps annotated mapping for elaboration)
 3. Transforms — local rewrite rules; normalize :: Constraint -> Constraint
 4. Dependency — buildDependencyGraph :: Constraint -> DepGraph and isAcyclic
 5. Presolution:
 • computePresolution :: Constraint -> (Presolution, Constraint')
 • implement decideMinimalExpansions (the delicate core routine) using the paper’s rules.
 6. Unify — graphic unifier: solveUnify :: Constraint -> Substitution using union-find and structural checks.
 7. Elaborate — elaborate :: AnnotatedExpr -> Presolution -> Substitution -> xMLFTerm
 8. Driver — glue it and produce diagnostics.

⸻

Short example trace (the k example) — how the algorithm behaves
 • generateConstraints produces two instantiation edges referencing the same s for f (the let-binding). Normalization leaves them as two s T1 ≤ T2 → T4 and s T1 ≤ T3 → T5.
 • Dependency graph has both instantiation edges but neither depends on the other; topological order processes them in either order.
 • For first edge the minimal decision is s := ∀a. (introduce one quantifier) so that instantiations produce a := T2; that generates unification edges.
 • After the second edge is processed, you get a := T3 in that second instantiation; unification solves the relationships and the final presolution is s ↦ ∀a. •. Elaboration yields Λa. λx:a. ... and two instantiations f[int] and f[bool]. This matches the worked example in the paper. (This trace is described across §1, §5, §8.)

⸻

References (paper sections I used heavily)
 • Didier Rémy & Boris Yakobowski. From ML to MLF: Graphic Type Constraints with Efficient Type Inference, ICFP 2008 (PDF you gave). Key sections:
 • Translation to constraints and graphic type definitions: §1 (pp.1–5).  ￼
 • Solved forms and semantic meaning: §3 (pp.6–8).  ￼
 • Transformations on constraints (local rewrites): §4 (pp.8–9).  ￼
 • Principal presolution strategy and algorithm: §5 (pp.9–11).  ￼
 • Complexity and implementation remarks: §7 (p.11).  ￼

(Links / citations above point to the same PDF you gave; see pages indicated in those sections for the formal rules and proofs.)  ￼

⸻

Final notes and offer

This answer gives the full algorithmic recipe and the exact subroutines you must implement to get a faithful Rémy & Yakobowski MLF inference engine. The paper contains the formal inference and correctness proofs and gives the exact local rewrite rules and the presolution decision rules that I summarized; please consult §4–§5 in the PDF for the constructive rules for decideMinimalExpansions and the formal solved-form definition if you want the mechanically precise case analyses.

⸻

## Implementation Status

> **Detailed checklist:** [TODO.md](TODO.md)

| Phase | Description | Status | Tests |
|-------|-------------|--------|-------|
| 0 | Data structures (`TyNode`, `GNode`, `Constraint`, `Expansion`) | ✅ Done | — |
| 1 | Constraint generation (`inferConstraintGraph`) | ✅ Done | 23 |
| 2 | Normalize / Local transformations | ✅ Done | 16 |
| 3 | Acyclicity check | ⬚ Not started | — |
| 4 | Principal presolution (`decideMinimalExpansions`) | ✅ Done | 5 |
| 5 | Unification solver (union-find) | ⬚ Not started | — |
| 6 | Elaboration to xMLF | ⬚ Not started | — |

**Total tests: 39** — Run with `cabal test`

**Key papers:**

- Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) — Main algorithm (Phases 1-5)
- Rémy & Yakobowski, "A graphical representation of MLF types" (TLDI 2007) — Graphical unification
- Rémy & Yakobowski, "A Church-Style Intermediate Language for MLF" (FLOPS 2010) — xMLF elaboration (Phase 6)

---

### Phase 1 — Constraint Generation ✅

```text
src/MLF/
  Syntax.hs        -- AST: Expr, Literal
  Types.hs         -- TyNode, GNode, Constraint, InstEdge, Expansion
  ConstraintGen.hs -- inferConstraintGraph
```

Covers: literals, variables & scope, lambda nodes, applications, let-generalization, expansion nodes, higher-order structure.

---

### Phase 2 — Normalization ✅

```text
src/MLF/
  Normalize.hs   -- normalize, NormalizeState, graftInstEdges, mergeUnifyEdges
```

**Transformations:**

- Drop reflexive edges (T ≤ T, T = T)
- Grafting: copy structure onto variables when demanded by `InstEdge`
- Merging: process `UnifyEdge`s via union-find
- Fixed-point iteration

> **Note:** G-node push/pull is a structural invariant (see `Note [G-node Push/Pull Invariant]` in Types.hs). Identity expansion collapse happens in Phase 4.

---

### Phase 3: Acyclicity Check ✅ Complete

**Implementation:** `MLF.Acyclicity` module

1. ✅ `buildDependencyGraph` — constructs graph between `InstEdge`s based on reachable node overlap
2. ✅ `topologicalSort` — DFS-based sort with cycle detection (returns `Either [EdgeId] [EdgeId]`)
3. ✅ `checkAcyclicity` — main API returning `Either CycleError AcyclicityResult`
4. ✅ `collectReachableNodes` — traverses type graph through `TyArrow`, `TyForall`, `TyExp`

**Tests:** 41 tests covering trivial cases, chains, diamonds, cycles, structured types, edge cases.

---

### Next Up — Phase 4: Principal Presolution

1. Process `InstEdge`s in topological order from Phase 3
2. Compute minimal expansions for `ExpVar`s
3. Apply expansions and generate unification edges

This is the algorithmic heart of MLF inference.
