# Plan: dedup base‑bound resolution + inline base bounds

## Scope
Unify base‑bound resolution and inlining logic duplicated across:
- `src/MLF/Elab/Run.hs:1204` + `src/MLF/Elab/Run.hs:1223`
- `src/MLF/Elab/Elaborate.hs:612` + `src/MLF/Elab/Elaborate.hs:634`
- `src/MLF/Elab/Phi.hs:640`

## Target location
`MLF.Elab.TypeOps` (or a submodule `MLF.Elab.TypeOps.BaseBounds` if you want separation).

## Canonical API
Two entry points to cover both contexts:
- `resolveBaseBoundForInstConstraint :: Constraint -> (NodeId -> NodeId) -> NodeId -> Maybe NodeId`
- `resolveBaseBoundForInstSolved :: SolveResult -> NodeId -> Maybe NodeId`
- `inlineBaseBoundsType :: Constraint -> (NodeId -> NodeId) -> ElabType -> ElabType`

## Steps
1. Implement the canonical helpers in `TypeOps` using the `Run` variant as baseline (already handles `Constraint` + canonical).
2. Replace local helper definitions in `Run`, `Elaborate`, `Phi` with `TypeOps` imports.
3. Update call sites to use the correct helper (Constraint vs SolveResult).
4. Remove local helper definitions.
5. Build and test.

## Risks
- Canonicalization differences: ensure `SolveResult` uses `frWith` consistently.
- Inlining behavior for `TyBottom` and `TyBase` must match existing logic.

