# Design Document

## Overview
The current constraint generation still uses the leftmost constraint of Figure 15.2.6
(the let expression shares the gen node of its body), which the thesis explicitly avoids
for translation. With gen nodes now split and translation contexts fail-fast, we need to
implement the alternative typing of let (rightmost constraint in Fig. 15.2.6) and add
explicit “translatable presolution” validation (Def. 15.2.10). This brings let scoping and
presolution validity in line with `papers/these-finale-english.txt` §15.2.6–§15.2.7.

## Constraint Shape (Thesis-Exact)
Figure 15.2.6 provides two let constraints. We must implement the rightmost
constraint, which introduces a trivial scheme for the whole let expression.

Leftmost (current, not thesis-exact):
- The let expression shares the gen node of `b` (body).
- The scope of `b` is visible to `a` (RHS).

Rightmost (thesis-exact):
- Allocate a scheme gen node `g_scheme` for `a` (RHS), as usual.
- Allocate a fresh let-expression gen node `g_let`.
- Bind a trivial scheme root `t` (fresh TyVar) under `g_let`.
- Allocate a fresh body gen node `g_body` bound under `g_let`.
- Translate `a` under `g_scheme`; translate `b` under `g_body`.
- Add an instantiation edge `b <= t`.

This prevents `a` from seeing `b`’s scope and matches §15.2.6.

## Architecture
- Constraint generation: for `let x = a in b`, introduce a **new gen node for the let
  expression itself** and a trivial scheme root (fresh type variable) bound under it, then
  add an instantiation edge from `b`’s type to this trivial scheme. This matches the
  rightmost constraint of Fig. 15.2.6 and prevents `a` from seeing `b`’s scope.
- Trivial scheme instantiation: per §15.2.6.1, the instantiation edge to the trivial
  scheme translates to the identity computation for principal presolutions. The
  elaborator should therefore treat it as a no-op (drop the wrapper), without forcing
  extra presolution unifications that would change binding scope.
- Presolution validation: add a translatability check after presolution (Def. 15.2.10),
  and fail early if any condition is violated. Translation contexts now throw on missing
  contexts, so this check is required to keep failures structured and thesis-aligned.
- Translatable presolution normalization: after presolution, enforce Def. 15.2.10
  via the two-phase weakening of Theorem 15.2.11:
  1) weaken inert-locked nodes and application/abstraction arrow nodes;
  2) weaken non-degenerate scheme roots (scheme root bound on its gen node) and
     non-interior nodes bound on gen nodes.
  3) re-run inert-locked weakening to eliminate any new inert-locked nodes created
     by step (2) in our explicit binding-tree encoding.
  Expansions are unchanged; witness normalization must account for these weakened
  nodes so rigid flags and witnesses stay consistent.
- Elaboration/generalization: consume the revised let scoping without introducing
  ad-hoc fallbacks. The trivial scheme for the let expression should translate to identity.

## Components and Interfaces
- `src/MLF/Frontend/ConstraintGen/*`
  - Let-binding translation with an explicit let-expression gen node and trivial scheme.
  - Use `AnnExpr` annotations (e.g., `AAnn`) to carry the let-expression inst edge;
    no new `AnnExpr` fields are required.
- `src/MLF/Constraint/Presolution/*`
  - Translatability checks (Def. 15.2.10) and structured errors.
- `src/MLF/Elab/Generalize.hs` / `src/MLF/Elab/Elaborate.hs`
  - Use the updated let scoping in generalization/elaboration without fallback logic.

## Error Handling
- Introduce a specific error when presolutions are not translatable so the
  failure is explicit and diagnosable (e.g., `NonTranslatablePresolution`).
  - Map each issue to Def. 15.2.10 conditions (1)–(4).

## Translatability Conditions (Def. 15.2.10)
Checks to enforce after presolution:
1. No inert-locked nodes.
2. Non-degenerate scheme roots are rigidly bound (scheme root bound on its gen node).
3. Application/abstraction arrow nodes are rigidly bound.
4. Nodes bound on a gen node but outside its structural interior are rigidly bound.

## Testing Strategy
- Add regression tests with nested lets showing the alternative scoping
  (rightmost constraint) changes quantification compared to the old behavior.
- Add a test that fails when presolution translatability is violated.

## References
- `papers/these-finale-english.txt` Chapter 15.2.6–15.2.7
