# Roadmap for Implementing MLF: Inference, Elaboration, and xMLF

> **Foundational Reference:** `papers/these-finale-english.txt` (thesis; more detailed than `papers/xmlf.txt`).
> **Supplementary Reference:** `papers/xmlf.txt` ("A Church-Style Intermediate Language for MLF", Remy & Yakobowski, 2009).
> **Supplementary Reference:** ICFP 2008 ("From ML to MLF") for the specific graphic constraint solving algorithm.
> **Project Goal:** stay paper-faithful to `papers/these-finale-english.txt` and document/test any intentional deviations; use `papers/xmlf.txt` only when the thesis is silent.

**Implementation progress:** See [TODO.md](TODO.md) for the current checklist.

This roadmap outlines the implementation of the full MLF pipeline as described in the thesis. The goal is to take an unannotated ML-like term, infer its principal type using graphic constraints, and **elaborate** it into a fully explicitly typed **xMLF** term. Finally, we implement the xMLF calculus itself to verify type soundness and reduction.

⸻

## High Level Overview

*   **Input:** The full eMLF surface language `e` (including explicit type annotations).
*   **Inference Engine:** Converts `e` to **graphic constraints** `χ`, solves them to find a **principal presolution** `χ_p` (mapping expansion variables to specific instantiation choices).
*   **Elaboration (The Core Focus):** Transforms the original term `e` into an **xMLF term** `a` using the presolution `χ_p`. The xMLF term contains explicit type abstractions `Λ(α ≥ τ)` and explicit instantiations `a φ`.
*   **Target (xMLF):** A Church-style calculus with full type information, suitable for compilation and local type checking.

**Key Paper Sections (see `papers/these-finale-english.txt`; section numbering from `papers/xmlf.txt`):**
*   **§1 The Calculus:** Syntax of xMLF terms, types, and instantiations.
*   **§3 Elaboration:** The translation process from eMLF to xMLF based on presolutions.
*   **§1.4 Reduction:** Small-step reduction semantics for xMLF.

### Thesis-faithful pipeline structure (target)

1. **Frontend (syntax → constraints)**: parse/desugar/annotate; generate constraints (expansion vars, scopes, binding info).
2. **Constraint normalization (graph form)**: normalize constraints; compute structural metadata (binding edges, ordering edges, interiors).
3. **Solving / presolution (choose expansions + witnesses)**: solve constraints, pick expansions, and decide **generalization/binders, dependency order, alias policies**; extract Ω/Φ witnesses and expansion recipes.
4. **Reify / apply (elaboration)**: apply presolution artifacts to the annotated term/type; convert expansions to instantiations; apply Ω/Φ steps; **no new solving decisions**.

This roadmap assumes (and future refactors target) a *thin* Elab layer that consumes explicit plans from presolution rather than recomputing them.

**Current vs target:** The current codebase already matches the Frontend/Normalize/Solve structure and records presolution witnesses, but generalization planning (binder selection, ordering, alias policy, scheme-root policy) still lives in `MLF.Elab.Generalize`. The target structure moves that planning into presolution (e.g., `MLF.Constraint.Presolution.Plan`) so Elab only applies precomputed plans.

⸻

## Phase 0: Core Definitions (xMLF & Graphic Constraints)

You need two sets of data structures: one for the inference graph (Graphic Types) and one for the target language (xMLF).

### 1. Target Language: xMLF (see `papers/these-finale-english.txt`; `papers/xmlf.txt` §1 for numbering)
*   **Types (`τ`):**
    *   Variables `α`, Arrow `τ → τ`, Bottom `⊥`.
    *   **Flexible Quantification:** `∀(α ≥ τ) τ'` (binds `α` in `τ'`, `α` must be an instance of `τ`).
*   **Instantiations (`φ`):**
    *   Witnesses for the instance relation: `!α` (Abstract), `∀(≥ φ)` (Inside), `∀(α ≥) φ` (Under), `N` (Elim), `O` (Intro), `φ; φ` (Comp).
*   **Terms (`a`):**
    *   Standard: `x`, `λ(x:τ) a`, `a a`, `let x = a in a`.
    *   **Type Abstraction:** `Λ(α ≥ τ) a`.
    *   **Type Instantiation:** `a φ` (applying a term to an instantiation witness).

### 2. Inference Structures: Graphic Constraints (from 2008 paper)
*   **Nodes:** Variables and structure (`TyVar`, `TyArrow`, `TyBase`, `TyForall`, `TyExp`).
    * `TyForall` is the repo’s representation of the paper’s binding node `g` (generalization site).
*   **Edges:** Unification (`=`), Instantiation (`≤`), plus an explicit binding tree
    `Constraint.cBindParents` (child → (parent, flex/rigid)).
*   **Expansions:** The mechanism to delay instantiation decisions.

⸻

## Phase 1: Constraint Generation (Inference)
*(Reference: ICFP 2008 §1, summarized in `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §3)*

Translate the source term `e` into a graphic constraint `χ`.
*   Maintain a mapping from source AST nodes to graph nodes (crucial for Phase 6).
*   **Key Requirement:** The translation must be compositional to allow reconstructing the term later.

⸻

## Phase 2–5: Solving to Principal Presolution
*(Reference: ICFP 2008 §3–§5)*

These phases implement the constraint solver. While `papers/these-finale-english.txt` assumes this exists (see also `papers/xmlf.txt`), you must implement the 2008 algorithm to get the **presolution**.

1.  **Normalize:** Apply local graph rewrites (grafting, merging).
2.  **Acyclicity Check:** Ensure instantiation dependencies are acyclic.
3.  **Compute Presolution (`ρ`):**
    *   Topologically sort instantiation edges.
    *   For each edge, determine the **minimal expansion** (either `Inst` or `∀` introduction) required to satisfy the constraint.
    *   Decide **generalization plan** metadata (binder selection, ordering dependencies, alias policy) so elaboration can apply it without new decisions.
    *   **Output (in this repo):**
        * `MLF.Constraint.Presolution.PresolutionResult.prEdgeExpansions` (per-edge expansion decisions), and
        * `MLF.Constraint.Presolution.PresolutionResult.prEdgeWitnesses` (per-edge witnesses:
          `ewWitness` stores Ω ops; `ewSteps` interleaves O with Ω for Φ).
4.  **Unify:** Solve all unification edges to get the final graphic type structure.

**Result:** A **Solved Form** (Presolution) `χ_p` where all instantiation edges are discharged or solved.

⸻

## Phase 6: Elaboration to xMLF (UPDATED)
*(Reference: `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §3 "Elaboration of eMLF programs into xMLF")*

This is the bridge between the graph and the xMLF calculus. You must implement the translation function `⟦ a ⟧_χp`.

### Algorithm
Re-traverse the original AST `a` and transform it into an xMLF term `a'` using the solved constraints `χ_p`:

1.  **Applications (the instantiation sites in this repo)**:
    *   Each application carries an `EdgeId` for the function position.
    *   Look up the recorded per-edge witness `EdgeWitness` and translate `ewSteps` to an xMLF instantiation `φ = Φ(e)` (Fig. 10), including quantifier reordering `Σ(g)` when needed (see `papers/these-finale-english.txt`; `papers/xmlf.txt` “Reordering quantifiers”).
    *   Result shape: `(a1 [φ]) a2` (or just `a1 a2` if `φ` is identity).

2.  **Let-bindings `let x = a1 in a2`:**
    *   Apply the **presolution generalization plan** (binder selection + ordering + alias policy) to compute the scheme.
    *   Wrap the RHS in explicit type abstractions `Λ(α ≥ τ)` for the scheme binders.
    *   Result shape: `let x : σ = (Λ... a1') in a2'`.

3.  **Lambda `λx. a`:**
    *   Reify the inferred parameter type from the solved graph and annotate the binder.
    *   Result: `λ(x : τ) a'`.

**Key Deliverable (in this repo):**
`MLF.Elab.Pipeline.elaborate` consumes the solved graph plus presolution witnesses (and, as refactors complete, explicit planning records) and produces `MLF.Elab.Pipeline.ElabTerm`. `MLF.Elab.Pipeline.runPipelineElab` runs Phases 1–6 end-to-end.

⸻

## Phase 7: xMLF Execution & Verification (NEW)
*(Reference: `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §1 & §2)*

Now that we have an xMLF term, we must treat it as a runnable program.

1.  **Type Checking:**
    *   Implement the xMLF typing rules (see `papers/these-finale-english.txt`; Figure 4 in `papers/xmlf.txt`).
    *   Verify that the elaborated term is well-typed. This confirms the soundness of the inference/elaboration.
    *   *Note:* xMLF type checking is local and deterministic (no unification needed).

2.  **Reduction:**
    *   Implement the small-step reduction rules (see `papers/these-finale-english.txt`; Figure 5 in `papers/xmlf.txt`).
    *   Rules include: `(β)`, `(let)`, and significantly, the **instantiation reductions** (`ι-rules`) like `(Λ(α ≥ τ) a) N ⟶ a{!α ← 1}{α ← τ}`.
    *   These rules allow executing the code and simplifying the type instantiations.

**Status in this repo:** Phase 7 is not implemented yet. We do have a key building block: `MLF.Elab.Pipeline.applyInstantiation` (see `papers/these-finale-english.txt`; `papers/xmlf.txt` Fig. 3) to apply/check instantiations at the type level, which is used by tests to validate Φ/Σ.

⸻

## Summary of Changes (Reference Shift)

*   **Foundation:** Moved from *just* implementing the solver (2008) to implementing the **full language pipeline** (2009).
*   **Elaboration:** Explicitly defined as generating xMLF terms with `Λ` and `φ` witnesses (previously vague).
*   **Target:** Added Phase 7 to implement the xMLF calculus itself (semantics and typing), which was absent in the previous roadmap.
*   **Terminology:** Adopted thesis notation, aligning with `papers/xmlf.txt` for xMLF terms (Type Instantiation `φ`, Flexible Quantification `∀(α ≥ τ)`).
*   **Φ/Σ:** The roadmap now treats `Φ(e)` and `Σ(g)` as explicit deliverables: record per-edge witnesses in presolution and translate them to xMLF instantiations (with reorderings) in elaboration.

⸻

## Minimal Worked Blueprint

This repo’s module-level decomposition:

1. **`MLF.Frontend.Syntax`**: Source `Expr` (+ `SrcType`/`SrcScheme` for annotations).
2. **`MLF.Constraint.Types`**: Graphic constraints (`Constraint`, `TyNode`, binding edges + bound/elimination stores) + per-edge witness types (`EdgeWitness`, `InstanceOp`).
3. **`MLF.Frontend.ConstraintGen`**: Phase 1 constraint generation (produces annotated AST with `NodeId`/`EdgeId`).
4. **`MLF.Constraint.Normalize`**: Phase 2 local rewrites (grafting/merging).
5. **`MLF.Constraint.Acyclicity`**: Phase 3 dependency ordering.
6. **`MLF.Constraint.Presolution`**: Phase 4 minimal expansions + per-edge witnesses.
7. **`MLF.Constraint.Solve`**: Phase 5 unification solve.
8. **`MLF.Elab.Pipeline`** (+ `MLF.Elab.Types`): Phase 6 elaboration to xMLF (`ElabTerm`, `ElabType`, `Instantiation`, Φ/Σ). Target structure keeps Elab thin and plan-driven.
9. **(Planned)**: `MLF.Constraint.Presolution.Plan` (explicit generalization/reify plans), consumed by Elab.
10. **(Future)**: Phase 7 xMLF typechecker + reduction semantics (see `papers/these-finale-english.txt`; `papers/xmlf.txt` Fig. 4/5).
