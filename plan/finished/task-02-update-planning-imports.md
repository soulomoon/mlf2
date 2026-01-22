# Task 2: update-planning-imports

## Goal
- Update all imports to the new presolution plan module paths.

## Scope
- src/MLF/Constraint/Presolution/Plan.hs
- src/MLF/Elab/**/*.hs
- src/MLF/Constraint/Presolution/Plan/*.hs

## Steps
1. Replace MLF.Elab.Generalize.* planning imports with MLF.Constraint.Presolution.Plan.*.
2. Ensure Presolution.Plan does not import Elab.Generalize.* modules.

## Verification
- rg -n "MLF.Elab.Generalize" src | rg -v "Generalize.hs"
