# Task 3: scheme-type choice

## Goal
- Move scheme-type fallback decision into ReifyPlan output and consume it in Generalize.

## Scope
- src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs
- src/MLF/Constraint/Presolution/Plan.hs
- src/MLF/Elab/Generalize.hs

## Steps
1. Add SchemeTypeChoice to ReifyPlan and compute in buildReifyPlan.
2. Thread inputs from planReify and use choice in Generalize.
3. Remove obsolete decision logic and unused imports.

## Verification
- rg -n "SchemeTypeChoice" src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs src/MLF/Elab/Generalize.hs
