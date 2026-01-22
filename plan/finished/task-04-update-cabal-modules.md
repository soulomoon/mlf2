# Task 4: update-cabal-modules

## Goal
- Update mlf2.cabal module lists for moved presolution plan modules.

## Scope
- mlf2.cabal

## Steps
1. Remove MLF.Elab.Generalize.* planning modules.
2. Add MLF.Constraint.Presolution.Plan.* modules.

## Verification
- rg -n "Presolution.Plan" mlf2.cabal
