# Roadmap for Implementing MLF: Inference, Elaboration, and xMLF

> **Foundational Reference:** `papers/these-finale-english.txt` (thesis; more detailed than `papers/xmlf.txt`).
> **Supplementary Reference:** `papers/xmlf.txt` ("A Church-Style Intermediate Language for MLF", Remy & Yakobowski, 2009).
> **Supplementary Reference:** ICFP 2008 ("From ML to MLF") for the specific graphic constraint solving algorithm.
> **Project Goal:** stay paper-faithful to `papers/these-finale-english.txt` and document/test any intentional deviations; use `papers/xmlf.txt` only when the thesis is silent.

**Implementation progress:** See [TODO.md](TODO.md) for the current cleanup/stabilization checklist.

**Status snapshot (2026-03-11):**
*   **Pipeline coverage:** Phases 1–7 are implemented in-tree; current work is mostly guardrail tightening, façade cleanup, and warning-free maintenance rather than filling missing pipeline phases.
*   **Public APIs:** `MLF.API` is the surface-syntax/parse/pretty umbrella, `MLF.Pipeline` is the normalized inference/elaboration/runtime API, and `MLF.XMLF` is the explicit xMLF syntax API.
*   **Audit surfaces:** Paper alignment now lives in `docs/paper-map.md`, `docs/thesis-obligations.yaml`, `docs/thesis-claims.yaml`, and `docs/thesis-deviations.yaml`.

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

### Thesis-faithful pipeline structure (implemented shape)

1. **Frontend (syntax → constraints)**: parse/desugar/annotate; generate constraints (expansion vars, scopes, binding info).
2. **Constraint normalization (graph form)**: normalize constraints; compute structural metadata (binding edges, ordering edges, interiors).
3. **Solving / presolution (choose expansions + witnesses)**: solve constraints, pick expansions, and decide **generalization/binders, dependency order, alias policies**; extract Ω/Φ witnesses and expansion recipes.
4. **Reify / apply (elaboration)**: apply presolution artifacts to the annotated term/type; convert expansions to instantiations; apply Ω/Φ steps; **no new solving decisions**.

This roadmap now matches the implementation: Elab is *thin* and consumes explicit plans from presolution rather than recomputing them.

**Current vs target:** The current codebase matches the Frontend/Normalize/Solve structure and records presolution witnesses, and generalization planning (binder selection, ordering, alias policy, scheme-root policy) now lives in presolution (`MLF.Constraint.Presolution.Plan`). Elab consumes `GeneralizePlan`/`ReifyPlan` outputs and applies them without new solving decisions.

**Syntax/frontend status (2026-03-11):** parser/pretty modules exist for both eMLF (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`) and xMLF (`MLF.XMLF.Parse`, `MLF.XMLF.Pretty`). `MLF.API` exposes surface syntax + parse/pretty/normalization, while `MLF.Pipeline` exposes the normalized constraint/elaboration/runtime path and `MLF.XMLF` exposes explicit xMLF tooling. Canonical syntax and migration deltas are documented in `docs/syntax.md`.

**Known deviations / proof gaps (tracked):** `docs/thesis-deviations.yaml` is the live deviation register, and `docs/paper-map.md` / `docs/thesis-claims.yaml` / `docs/thesis-obligations.yaml` are the live paper-to-code ledgers. The current register records proof gaps, implementation choices, and the `DEV-AUTO-ISO-RECURSIVE` semantic extension for automatic μ-introduction (see `docs/thesis-deviations.yaml`).

### Public entrypoints in this repo

1. **`MLF.API`**: raw + normalized eMLF syntax, parsing, pretty-printing, and normalization helpers.
2. **`MLF.Pipeline`**: normalized pipeline entrypoints (`inferConstraintGraph`, `runPipelineElab*`) plus Phase 7 helpers (`typeCheck`, `step`, `normalize`, `isValue`).
3. **`MLF.XMLF`**: xMLF syntax, parsing, and pretty-printing.
4. **`app/Main.hs`**: a minimal executable that normalizes an example term and runs the pipeline.

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

**Key Deliverables / owners (in this repo):**
*   `MLF.Elab.Run.Pipeline` executes the end-to-end normalized path for Phases 1–6: constraint generation, normalization, acyclicity, presolution, finalize/view preparation, and elaboration.
*   `MLF.Elab.Elaborate` + `MLF.Reify.Core` own the graph→xMLF reconstruction work.
*   `MLF.Elab.Phi` translates per-edge witnesses to xMLF instantiations, and `MLF.Elab.Sigma` handles quantifier reordering.
*   `MLF.Elab.Pipeline` is the stable elaboration/runtime facade, and `MLF.Pipeline.runPipelineElab*` is the downstream normalized public API.

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

**Status in this repo:** Phase 7 is implemented. See `MLF.Elab.TypeCheck` (typing rules) and `MLF.Elab.Reduce` (small-step semantics), with downstream helpers re-exported by `MLF.Pipeline` and regression coverage in `test/TypeCheckSpec.hs`, `test/ReduceSpec.hs`, and `test/TypeSoundnessSpec.hs`. Phase 7 also handles **automatic iso-recursive types**: `TMu` type checking, `ERoll`/`EUnroll` reduction, and the full pipeline works end-to-end for automatically-inferred recursive types (see `DEV-AUTO-ISO-RECURSIVE` in `docs/thesis-deviations.yaml`).

⸻

## Summary of Changes (Reference Shift)

*   **Foundation:** Moved from *just* implementing the solver (2008) to implementing the **full language pipeline** (2009).
*   **Elaboration:** Explicitly defined as generating xMLF terms with `Λ` and `φ` witnesses (previously vague).
*   **Target:** Added Phase 7 to implement the xMLF calculus itself (semantics and typing), which was absent in the previous roadmap.
*   **Terminology:** Adopted thesis notation, aligning with `papers/xmlf.txt` for xMLF terms (Type Instantiation `φ`, Flexible Quantification `∀(α ≥ τ)`).
*   **Φ/Σ:** The roadmap now treats `Φ(e)` and `Σ(g)` as explicit deliverables: record per-edge witnesses in presolution and translate them to xMLF instantiations (with reorderings) in elaboration.

⸻

## Current Module Map

This repo’s current module-level decomposition is broader than the original “minimal blueprint” and is organized around both public APIs and internal owner boundaries:

1. **Public surfaces:** `MLF.API`, `MLF.Pipeline`, `MLF.XMLF`, plus `app/Main.hs` as the demo executable.
2. **Surface frontend:** `MLF.Frontend.Syntax`, `MLF.Frontend.Parse`, `MLF.Frontend.Pretty`, `MLF.Frontend.Normalize`, and `MLF.Frontend.ConstraintGen` (+ `ConstraintGen.*`) cover source syntax, canonicalization/normalization, and annotated constraint generation.
3. **Constraint core:** `MLF.Constraint.Types.Graph`, `MLF.Constraint.Types.Witness`, and `MLF.Constraint.Types.Presolution` hold the split core data types, re-exported via `MLF.Constraint.Types` for compatibility.
4. **Solver pipeline:** `MLF.Constraint.Normalize`, `MLF.Constraint.Acyclicity`, `MLF.Constraint.Presolution` (+ `Presolution.Plan`, `Presolution.View`, `Presolution.EdgeProcessing.*`, `Presolution.EdgeUnify.*`, `Presolution.Witness*`), `MLF.Constraint.Solve`, `MLF.Constraint.Finalize`, and `MLF.Constraint.Canonicalizer` cover Phases 2–5 plus snapshot/finalization support.
5. **Reify + elaboration internals:** `MLF.Reify.Core`, `MLF.Elab.Elaborate`, `MLF.Elab.Run`, `MLF.Elab.Phi`, `MLF.Elab.Sigma`, and `MLF.Elab.Pipeline` cover graph reification, Φ/Σ translation, and the executable elaboration pipeline.
6. **xMLF execution:** `MLF.Elab.TypeCheck` and `MLF.Elab.Reduce` implement local typechecking and small-step reduction.
7. **Explicit xMLF syntax tooling:** `MLF.XMLF.Syntax`, `MLF.XMLF.Parse`, and `MLF.XMLF.Pretty` provide the standalone Church-style language surface.
8. **Audit/test surfaces:** `test/PipelineSpec.hs`, `test/PresolutionSpec.hs`, `test/TypeCheckSpec.hs`, `test/ReduceSpec.hs`, `test/TypeSoundnessSpec.hs`, `docs/paper-map.md`, `docs/thesis-obligations.yaml`, `docs/thesis-claims.yaml`, and `docs/thesis-deviations.yaml` are the main regression/audit anchors for keeping the roadmap aligned with the implementation.
