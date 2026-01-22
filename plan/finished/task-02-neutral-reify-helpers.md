# Task 2: neutral-reify-helpers

## Goal
- Move reify and type-ops helpers to neutral modules and re-export from Elab modules.

## Scope
- src/MLF/Reify/Core.hs
- src/MLF/Reify/TypeOps.hs
- src/MLF/Elab/Reify.hs
- src/MLF/Elab/TypeOps.hs
- src/MLF/Constraint/Presolution/Plan/*.hs

## Steps
1. Create MLF.Reify.Core with reify helpers used by planning.
2. Create MLF.Reify.TypeOps with stripForallsType/substTypeSimple/freeTypeVarsType (and helpers).
3. Update Elab.Reify/Elab.TypeOps to re-export from neutral modules.
4. Update presolution plan modules to import from MLF.Reify.*.

## Verification
- rg -n "module MLF.Reify.(Core|TypeOps)" src/MLF/Reify
