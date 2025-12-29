# Design Document

## Overview
Generalization now uses a synthetic `TyRoot` to anchor top-level scope and
binders are enumerated from the binding tree whenever possible. For `TyForall`
scopes, Q(n) (flex children) is used directly. For non-Forall scopes, we walk
binding-parent paths to the scope root and include reachable binders. The
free-variable fallback has been removed now that binding edges cover all
quantifiable variables, so explicit quantifiers are preserved by construction.

## Architecture
- Constraint generation allocates a `TyRoot` node and rebinds top-level scope
  nodes under it to make the binding tree total for generalization.
- `runPipelineElab` selects the constraint root (`TyRoot`) as the generalization
  scope (falling back to the expression root if none exists).
- `generalizeAt`:
  - For `TyForall` scope roots, uses `Binding.boundFlexChildrenUnder` (Q(n)).
  - For non-Forall scopes, uses `boundAtScope` to select binders whose binding
    path reaches the scope root and remains flexible; rigid vars are inlined via
    their bounds (see `rigid-bound-inlining`).
  - Canonicalizes and deduplicates binders before ordering and substitution.

## Components and Interfaces
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Allocate a `TyRoot` binder and rebind top-level scope nodes to it.
- `src/MLF/Elab/Run.hs`
  - Use `ConstraintRoot.findConstraintRoot` for the generalization scope.
- `src/MLF/Elab/Generalize.hs`
  - Add binding-path enumeration for non-Forall scopes.
  - Canonicalize binder ids before substitution.

## Data Models
- A synthetic `TyRoot` node is used as the constraint-level binding root.

## Error Handling
- Binding-tree errors from `Binding` queries still surface as `ElabError`.
- No new error constructors are introduced.

## Testing Strategy
- Unit: `generalizeAt` over a non-Forall scope root and a minimal binding tree.
- Regression: top-level polymorphic let-bound values remain polymorphic.
- Baseline: bounded aliasing preserves quantifiers after elaboration.

## Deviation Notes
`papers/xmlf.txt` defines Q(n) solely from binding edges; the implementation
now matches this by removing the free-variable fallback once binding-edge
coverage was confirmed via regressions.
