# Refactor plan: unify presolution bindingPathToRootUnder

## Goal
Deduplicate the `bindingPathToRootUnder` helper used in presolution modules for canonicalized binding paths.

## Scope
- `src/MLF/Constraint/Presolution/Base.hs`
- `src/MLF/Constraint/Presolution/Copy.hs`

## Proposed abstraction
Move a single helper into `MLF.Constraint.Presolution.Base` (or new `MLF.Constraint.Presolution.Util`) with signature:

```
bindingPathToRootUnderM
  :: (NodeId -> NodeId)
  -> Constraint
  -> NodeRef
  -> PresolutionM [NodeRef]
```

Then import it in `Copy` and any other presolution module that needs it.

## Steps
1. Move the canonical version to a shared module and export it.
2. Replace local copies with imports.
3. Ensure error handling still throws `BindingTreeError` via `PresolutionM`.

## Risks
- None if the moved body is identical; verify no subtle differences.

## Verification
- Run `cabal test` (focus on `PresolutionSpec`).
