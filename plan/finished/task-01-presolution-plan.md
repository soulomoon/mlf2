# Task 1: presolution-plan-records

## Goal
- Add a presolution planning module with GeneralizePlan/ReifyPlan records and stub builders.

## Scope
- src/MLF/Constraint/Presolution/Plan.hs
- mlf2.cabal

## Steps
1. Create Plan module with data records + planGeneralize/planReify stubs.
2. Add module to cabal other-modules list.

## Verification
- rg -n "data GeneralizePlan|data ReifyPlan|planGeneralize|planReify" src/MLF/Constraint/Presolution/Plan.hs
