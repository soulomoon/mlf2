# Task 2: presolution-plan-imports

## Goal
- Switch presolution plan modules from MLF.Elab.Types to MLF.Util.ElabError for ElabError.

## Scope
- src/MLF/Constraint/Presolution/Plan/*.hs

## Steps
1. Find Elab.Types imports in plan modules.
2. Replace ElabError/bindingToElab imports with MLF.Util.ElabError.
3. Keep any other Elab.Types imports only if needed for types.

## Verification
- rg -n "MLF.Elab.Types" src/MLF/Constraint/Presolution/Plan | rg -n "ElabError"
