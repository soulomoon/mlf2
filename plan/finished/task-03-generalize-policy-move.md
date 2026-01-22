# Task 3: generalize-policy-move

## Goal
- Move remaining policy/decision logic out of MLF.Elab.Generalize into presolution plan modules so Elab only applies plan outputs.

## Scope
- src/MLF/Elab/Generalize.hs
- src/MLF/Constraint/Presolution/Plan/Normalize.hs (or new Plan/Finalize.hs)

## Steps
1. Audit MLF.Elab.Generalize for policy decisions (SchemeFreeVars, naming/renaming, alias policy).
2. Add plan outputs or finalize helpers in presolution Plan.* modules.
3. Replace Elab.Generalize logic with plan consumption.

## Verification
- rg -n "Plan\.Finalize|Plan\.Normalize" src/MLF/Constraint/Presolution/Plan
