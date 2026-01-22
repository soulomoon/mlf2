# Refactor plan: unify binding-path walkers

## Goal
Consolidate duplicated `bindingPathToRootLocal` logic scattered across modules into a single reusable helper.

## Scope
- `src/MLF/Elab/Generalize/BindingUtil.hs`
- `src/MLF/Elab/Elaborate.hs`
- `src/MLF/Elab/Phi.hs`
- `src/MLF/Elab/Run/Generalize.hs`
- `src/MLF/Binding/Tree.hs` (existing local helper)

## Proposed abstraction
Use `MLF.Elab.Generalize.BindingUtil.bindingPathToRootLocal` (or move it to a more general module like `MLF.Binding.Tree` / `MLF.Util.Binding`) and adapt error types at call sites:
- Provide adapters `bindingPathToRootLocalElab :: BindParents -> NodeRef -> Either ElabError [NodeRef]`
- Optionally expose `bindingPathToRootLocalRaw :: BindParents -> NodeRef -> Either BindingError [NodeRef]`

## Steps
1. Choose the canonical helper location and signature (prefer `Binding.Tree` for raw `BindingError`).
2. Add thin wrappers in Elab modules for error lifting if needed.
3. Replace in-module definitions with calls to the shared helper.
4. Ensure no changes to traversal order or cycle detection error payloads.

## Risks
- Error constructor differences (`BindingError` vs `ElabError`) could alter thrown errors if not wrapped consistently.

## Verification
- Run `cabal test` (focus on generalization and elaboration specs that exercise binding paths).
