# Design Document

## Overview
Align application translation with `papers/xmlf.txt` Figure 7 by ensuring that
both the function and the argument positions are instantiated with their own
witnesses. The constraint graph already supports two instantiation edges for an
application; this spec makes that dual instantiation explicit in elaboration
and covered by tests.

## Architecture
Pipeline flow for applications:

1) Constraint generation emits two instantiation edges for `e1 e2`:
   - `e_fun`: function node <= fresh arrow `(d -> r)`
   - `e_arg`: argument node <= domain `d`
2) Presolution produces a witness for each instantiation edge.
3) Elaboration translates each subterm and applies its witness as a type
   instantiation (`ETyInst`), then builds `EApp` from the instantiated terms.

## Components and Interfaces
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Ensure `EApp` emits two instantiation edges and stores both edge ids in
    `AApp` (funEid, argEid).
  - Keep `Note [Application and Instantiation Edges]` aligned with Figure 7.
- `src/MLF/Frontend/ConstraintGen/Types.hs`
  - `AApp` already carries both edge ids; no new fields are required.
- `src/MLF/Elab/Elaborate.hs`
  - Reify instantiation for `funEid` and `argEid`.
  - Wrap both subterms in `ETyInst` when the witness is non-identity.
- `src/MLF/Elab/Phi.hs`
  - No changes; used by `reifyInst` to translate edge witnesses to xMLF
    instantiations.

## Data Models
No new data types are required. This spec relies on:
- `AnnExpr.AApp` storing both edge ids.
- `ElabTerm.ETyInst` to apply instantiation to each subterm.

## Error Handling
- If an edge witness is missing, elaboration treats it as identity (existing
  behavior). This keeps application translation total and matches other uses
  of `reifyInst`.

## Testing Strategy
- ConstraintGen regression: verify that an application emits two instantiation
  edges and that `AApp` records both edge ids.
- Elaboration regression: `let id = \x. x in id id` should elaborate to an
  application with `ETyInst` on both the function and argument sides.
- Property test candidate: for any application where both edges have
  non-identity witnesses, the elaborated term contains two `ETyInst` wrappers
  (one per side).
