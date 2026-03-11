# Recursive Types Roadmap

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add recursive types to the language in a staged, thesis-aware way that preserves the current paper-faithful MLF/xMLF pipeline until each extension is explicitly validated.

**Architecture:** Start with an explicit iso-recursive xMLF/core extension (`μ`, `roll`, `unroll`) that does not require equi-recursive equality or cyclic constraint graphs. Only after that layer is stable should the surface eMLF syntax and, later, inference/constraint-graph support be extended.

**Tech Stack:** Haskell, Cabal, Hspec, Megaparsec, existing `MLF.API` / `MLF.Pipeline` / `MLF.XMLF` split.

---

## Scope Decisions

- **In scope for the roadmap:** staged iso-recursive support, explicit fold/unfold operations, parser/pretty/typecheck/reduction support, later surface integration, and a deferred plan for inference.
- **Out of scope for the first implementation:** equi-recursive equality, cyclic constraint-graph representation, solver-wide recursive unification, and automatic inference of recursive types.
- **Design invariant:** keep the thesis-faithful existing behavior as the default; every recursive-type capability is added as an explicit extension with narrow validation gates.

## Milestones Overview

1. **M0 — Freeze semantics and acceptance criteria**
2. **M1 — Explicit xMLF/core recursive types**
3. **M2 — Runtime/typechecker/reduction support**
4. **M3 — Public XMLF syntax + roundtrip tooling**
5. **M4 — Surface eMLF syntax exposure**
6. **M5 — Constraint/inference feasibility spike**
7. **M6 — Optional inference implementation**

Each milestone has a gate. Do not start the next milestone until the current gate is green.

---

### Task 1: M0 — Freeze semantics and extension boundary

**Files:**
- Modify: `docs/plans/2026-03-11-recursive-types-roadmap.md`
- Modify: `tasks/todo/2026-03-11-recursive-types-design/findings.md`
- Optional later: `implementation_notes.md`

**Objective:**
Write down the non-negotiable semantic choices before code changes begin.

**Decisions to freeze:**
- Use **iso-recursive** types, not equi-recursive types.
- Add a type former `μ a. τ`.
- Add explicit term forms `roll[μ a. τ] e` and `unroll e`.
- Keep `alphaEqType` structural/α-only; do not implicitly unfold `μ`.
- Require **contractiveness** of recursive types in v1.
- Do not represent recursive types as cyclic constraint graphs in v1.

**Gate:**
- A written semantic note exists and is agreed upon.
- Everyone implementing the feature can answer: “Are recursive types explicit? Are they iso-recursive? Do we unfold during equality?”

---

### Task 2: M1 — Extend explicit type ASTs only

**Files:**
- Modify: `src/MLF/Types/Elab.hs`
- Modify: `src/MLF/XMLF/Syntax.hs`
- Modify: `src/MLF/Elab/Types.hs`
- Test: `test/XMLFParseSpec.hs`
- Test: `test/XMLFPrettySpec.hs`
- Test: `test/TypeCheckSpec.hs`

**Objective:**
Add recursive-type structure to the explicit/core side without touching inference.

**Changes:**
- Add `TMu :: String -> Ty AllowVar -> Ty a` to `Ty`.
- Extend `TyIF`, recursion-scheme instances, `tyToElab`, `elabToBound`, `containsForallTy`, `containsArrowTy`, and any exhaustive helper.
- Add `XTMu :: String -> XmlfType -> XmlfType` to XMLF syntax.
- Extend `toXmlfType`/pretty/display conversion paths.

**Tests:**
- Parse/pretty roundtrip for `μa. a -> Int`-style shapes.
- Structural equality tests showing `μa. τ` is not equal to its unfolding by default.
- Free-variable/substitution smoke tests on `μ` binders.

**Gate:**
- `TMu` and `XTMu` compile everywhere.
- XMLF type roundtrip tests pass.
- Existing non-recursive type tests remain green.

---

### Task 3: M2 — Add explicit recursive term forms

**Files:**
- Modify: `src/MLF/Types/Elab.hs`
- Modify: `src/MLF/Elab/Types.hs`
- Modify: `src/MLF/Elab/TypeCheck.hs`
- Modify: `src/MLF/Elab/Reduce.hs`
- Test: `test/TypeCheckSpec.hs`
- Test: `test/ReduceSpec.hs`
- Test: `test/TypeSoundnessSpec.hs`

**Objective:**
Make recursive types usable in the elaborated language with explicit runtime forms.

**Changes:**
- Add `ERoll ElabType ElabTerm` and `EUnroll ElabTerm` (or equivalent explicit forms).
- Typechecking rules:
  - `roll[μa. τ] e : μa. τ` when `e : τ[μa.τ/a]`
  - `unroll e : τ[μa.τ/a]` when `e : μa. τ`
- Reduction rule:
  - `unroll (roll[μa. τ] v) -> v`
- Extend `isValue`, free-variable traversals, substitution, and pretty-printing.

**Tests:**
- Positive typecheck tests for `roll`/`unroll`.
- Negative tests for mismatched recursive wrappers.
- Reduction test for `unroll (roll v)`.
- Soundness/progress regression coverage for the new constructs.

**Gate:**
- Typechecker and reducer support the new forms.
- New runtime semantics are deterministic and existing reduction tests still pass.

---

### Task 4: M3 — Extend XMLF parser/pretty surface

**Files:**
- Modify: `src/MLF/XMLF/Parse.hs`
- Modify: `src/MLF/XMLF/Pretty.hs`
- Modify: `src-public/MLF/XMLF.hs` if exports change
- Test: `test/XMLFParseSpec.hs`
- Test: `test/XMLFPrettySpec.hs`
- Test: `test/PublicSurfaceSpec.hs`

**Objective:**
Expose recursive types and explicit recursive terms through the public xMLF layer.

**Changes:**
- Add XMLF grammar for `μ` types.
- Add term grammar for `roll[...] e` and `unroll e`.
- Keep notation explicit; do not introduce sugar yet.
- Preserve existing canonical pretty-printing style.

**Tests:**
- Parse/pretty roundtrip for recursive XMLF types and terms.
- Rejection tests for malformed `μ`, `roll`, `unroll` syntax.
- Public-surface tests proving `MLF.XMLF` exposes the new forms cleanly.

**Gate:**
- Public XMLF syntax is stable and roundtrippable.
- No regression in existing xMLF examples.

---

### Task 5: M4 — Add contractiveness validation

**Files:**
- Modify: `src/MLF/Reify/TypeOps.hs`
- Modify: `src/MLF/Elab/TypeCheck.hs`
- Possibly create: `src/MLF/Types/Recursive.hs`
- Test: `test/TypeCheckSpec.hs`
- Test: `test/ElaborationSpec.hs`

**Objective:**
Prevent obviously ill-founded recursive types in the explicit layer.

**Changes:**
- Introduce a reusable contractiveness checker over elaborated/XMLF types.
- Reject at least direct unguarded self-reference such as `μa. a`.
- Define what counts as a guard in v1: arrow, constructor application, and optionally `forall`.

**Tests:**
- Reject `μa. a`.
- Accept `μa. Int -> a` and `μa. List a`.
- Document any choice about `μa. ∀b. a` explicitly.

**Gate:**
- Contractiveness failures are deterministic and well-explained.
- The checker is shared rather than copied across parser/typecheck/reify paths.

---

### Task 6: M5 — Surface eMLF syntax spike

**Files:**
- Modify: `src/MLF/Frontend/Syntax.hs`
- Modify: `src/MLF/Parse/Type.hs`
- Modify: `src/MLF/Frontend/Parse.hs`
- Modify: `src/MLF/Frontend/Pretty.hs`
- Modify: `src/MLF/Frontend/Normalize.hs`
- Test: `test/FrontendParseSpec.hs`
- Test: `test/FrontendPrettySpec.hs`
- Test: `test/PublicSurfaceSpec.hs`

**Objective:**
Support recursive types in source annotations without inference changes.

**Changes:**
- Add `STMu` to source types.
- Parse `μa. τ` / `mu a. t` in source annotations.
- Extend pretty-printing and normalization.
- Keep normalization simple: preserve `μ`, only normalize its body recursively.

**Tests:**
- Parse/pretty roundtrip for annotated terms using recursive types.
- Normalization preserves recursive structure while still normalizing nested alias bounds.
- Existing annotation/coercion tests remain green.

**Gate:**
- Source annotations can mention recursive types.
- Desugaring/annotation paths remain unchanged except for carrying the new type form.

---

### Task 7: M6 — Pipeline acceptance for explicit annotations only

**Files:**
- Modify: `src/MLF/Frontend/ConstraintGen/Translate.hs`
- Modify: `src/MLF/Frontend/ConstraintGen/Emit.hs`
- Maybe create: `src/MLF/Constraint/Types/Graph/Recursive.hs`
- Test: `test/PipelineSpec.hs`
- Test: `test/FrontendParseSpec.hs`
- Test: `test/ElaborationSpec.hs`

**Objective:**
Decide how annotated recursive types cross the frontend/pipeline boundary without yet doing recursive-type inference.

**Recommended sub-approach:**
- Introduce a **first-class `TyMu` node form** if the pipeline must internalize recursive annotations.
- Do **not** encode recursion as actual cyclic node references in this milestone.
- Restrict support to **checking explicit recursive annotations**, not synthesizing them.

**Questions this milestone must answer:**
- Can constraint generation internalize `μ` without violating `Phase 3` acyclicity assumptions?
- Can reification roundtrip `TyMu` cleanly back to `TMu`?
- Which passes need only structural traversal updates vs new semantics?

**Gate:**
- A spike document exists deciding between `TyMu` node support and “explicit layer only” support.
- If no low-risk graph representation emerges, stop here and keep recursive types as explicit XMLF/elab-only.

---

### Task 8: M7 — Constraint/inference implementation (optional, high risk)

**Files:**
- Modify: `src/MLF/Constraint/Types/Graph/NodeEdge.hs`
- Modify: `src/MLF/Constraint/Traversal.hs`
- Modify: `src/MLF/Constraint/Normalize.hs`
- Modify: `src/MLF/Constraint/Unify/Core.hs`
- Modify: `src/MLF/Constraint/Unify/Decompose.hs`
- Modify: `src/MLF/Constraint/Unify/Closure.hs`
- Modify: `src/MLF/Constraint/Acyclicity.hs`
- Modify: `src/MLF/Reify/Type.hs`
- Modify: `src/MLF/Reify/TypeOps.hs`
- Test: `test/PresolutionSpec.hs`
- Test: `test/SolveSpec.hs`
- Test: `test/TypeSoundnessSpec.hs`
- Test: `test/AcyclicitySpec.hs`

**Objective:**
Support recursive types in the graph/inference pipeline, if and only if the earlier spike proves a tractable representation.

**Risks:**
- This is where the thesis warning becomes real: recursive types would cross the acyclic term-graph invariant and could invalidate current dependency/termination reasoning.
- If this milestone requires equi-recursive reasoning or cyclic graphs, it should become a separate research track rather than part of the mainline feature.

**Recommended success criterion:**
- Explicit recursive annotations typecheck through the full pipeline.
- Inference still does not invent recursive types unless a separate principality/decidability argument has been written.

**Gate:**
- Full `cabal build all && cabal test` passes.
- A written note explains the new representation and why Phase 3/4 remain sound enough for the implemented subset.

---

## Suggested Execution Order

- **Ship target A (recommended first release):** M0 → M1 → M2 → M3 → M4
- **Research target B (optional second release):** M5 → M6 → M7

That gives you a useful release where:
- recursive types exist,
- they are explicit,
- xMLF/core/runtime support them,
- source annotations can express them,
- but inference and constraint solving are still conservative.

## Risk Register

- **Highest risk:** graph/inference integration across acyclicity assumptions.
- **Medium risk:** contractiveness definition interacting with `forall` and bounds.
- **Low risk:** XMLF/core AST, parser, pretty, explicit typechecker, explicit reducer.

## Verification Strategy

- After each milestone, run the narrowest relevant tests first.
- Milestone gates that change runtime or typing behavior should finish with `cabal build all && cabal test`.
- Add focused regression tests before broad suites when changing:
  - parser/pretty
  - type equality/substitution
  - reduction
  - typechecking
  - reification
  - acyclicity / presolution

## Recommended First Release Scope

If you want the highest value-to-risk ratio, stop after **M4**.
That gives a coherent recursive-type story without forcing the solver to become a recursive-type inference engine.
