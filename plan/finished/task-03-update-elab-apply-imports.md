# Task 3: update-elab-apply-imports

## Goal
- Update Elab apply modules to use presolution plan modules/types.

## Scope
- src/MLF/Elab/Generalize.hs
- src/MLF/Elab/Reify.hs
- src/MLF/Elab/**/*.hs (planning imports)

## Steps
1. Replace Elab.Generalize planning imports with Presolution.Plan modules.
2. Confirm Elab only consumes plan records (no planning helpers under Elab.Generalize.*).

## Verification
- rg -n "MLF.Constraint.Presolution.Plan" src/MLF/Elab
