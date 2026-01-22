# Refactor plan: structuralChildrenWithBounds helper

## Goal
Remove repeated `boundKids` boilerplate by centralizing “structural children + bound” logic.

## Scope
- `src/MLF/Constraint/Types.hs` (add helper + export)
- Call sites that build `boundKids` and use `structuralChildren node ++ boundKids`, including:
  - `src/MLF/Constraint/Presolution/Base.hs`
  - `src/MLF/Constraint/Presolution/Copy.hs`
  - `src/MLF/Constraint/Presolution/Driver.hs`
  - `src/MLF/Constraint/Solve.hs`
  - `src/MLF/Binding/Tree.hs`
  - `src/MLF/Elab/Reify.hs`
  - `src/MLF/Elab/Generalize/Plan.hs`
  - `src/MLF/Elab/Generalize/BinderPlan.hs`
  - `src/MLF/Elab/Generalize.hs`
  - `src/MLF/Elab/Run/Generalize.hs`
  - `src/MLF/Frontend/ConstraintGen/Scope.hs`

## Proposed abstraction
Add `structuralChildrenWithBounds :: TyNode -> [NodeId]` to `MLF.Constraint.Types`:

```
structuralChildrenWithBounds node =
  case node of
    TyVar{ tnBound = Just bnd } -> structuralChildren node ++ [bnd]
    _ -> structuralChildren node
```

Then replace `structuralChildren node ++ boundKids` with `structuralChildrenWithBounds node` (plus existing canonicalization / mapping).

## Steps
1. Add and export `structuralChildrenWithBounds` in `Constraint.Types`.
2. Replace each `boundKids`/`structuralChildren ++ boundKids` pattern in scope with the helper.
3. Remove now-unused `boundKids` bindings and adjust imports if required.

## Risks
- Ensure we only replace sites that intentionally include bounds (do not affect callers that use `structuralChildren` alone).

## Verification
- `cabal test`
