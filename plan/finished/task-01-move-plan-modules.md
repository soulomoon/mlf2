# Task 1: move-plan-modules

## Goal
- Move generalization planning modules into the presolution planning namespace.

## Scope
- src/MLF/Elab/Generalize/*.hs
- src/MLF/Constraint/Presolution/Plan/*.hs

## Steps
1. Move files to the new directory and update module headers.
2. Verify the new module declarations exist under Presolution.Plan.

## Verification
- rg -n "module MLF.Constraint.Presolution.Plan" src/MLF/Constraint/Presolution/Plan
