# Design Document

## Overview
This spec tightens binding-edge coverage so all quantifiable variables are
reachable from the generalization scope via the binding tree. Once this holds,
`generalizeAt` can drop the remaining free-variable fallback and rely purely on
Q(n) / binding-parent paths as in `papers/xmlf.txt`.

## Architecture
- Identify every place new `TyVar` nodes are created post-constraint-gen:
  - expansion copying (`instantiateSchemeWithTrace`, binder metas),
  - forall introductions (`introduceForallFromSpec`),
  - presolution rewrite/canonicalization (`rewriteConstraint`).
- Ensure each such variable has a binding-parent path to the intended scope
  root (expansion root or forall binder) and is flexibly bound unless the
  paper requires rigidity.
- Add tests that compare binding-parent enumeration with the free-variable set
  of the generalized type for representative programs.

## Components and Interfaces
- `src/MLF/Constraint/Presolution/Expansion.hs`
  - Binder metas created during `ExpInstantiate` should be bound under the
    expansion root (or binder anchor) so they are inside I(r).
- `src/MLF/Constraint/Presolution/ForallIntro.hs`
  - `introduceForallFromSpec` must ensure its binders are flex children of the
    new `TyForall` (Q(n)).
- `src/MLF/Constraint/Presolution/Driver.hs`
  - `rewriteConstraint` should preserve binding parents for canonicalized nodes
    and avoid dropping edges that keep vars under the scope root.
- `src/MLF/Elab/Generalize.hs`
  - No changes now, but will remove fallback once invariants hold.

## Data Models
No new data types; this work relies on `Constraint.cBindParents` and existing
binding-tree queries.

## Error Handling
- If a binding-parent path cannot be established, the new tests should fail.
- We may add an internal assertion or helper to surface missing binding parents
  in debug/test builds.

## Testing Strategy
- Add targeted Hspec tests that:
  - run the pipeline on a bounded-aliasing case and verify the generalized type
    has all free vars bound via the binding tree, and
  - exercise a non-Forall scope generalization where free vars must be present
    in binding-parent enumeration.
- Keep existing paper-alignment baseline tests as end-to-end coverage.

## Follow-up
Once tests confirm binding-edge coverage, remove the scoped free-variable
fallback in `generalizeAt` and update the spec/notes accordingly.
