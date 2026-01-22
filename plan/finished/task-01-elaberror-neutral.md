# Task 1: elaberror-neutral

## Goal
- Move ElabError (and bindingToElab) into a neutral module and re-export from Elab.Types.

## Scope
- src/MLF/Util/ElabError.hs
- src/MLF/Elab/Types.hs

## Steps
1. Create MLF.Util.ElabError with ElabError + bindingToElab.
2. Remove ElabError/bindingToElab from MLF.Elab.Types and re-export from neutral module.
3. Update imports in Elab.Types as needed.

## Verification
- rg -n "module MLF.Util.ElabError|ElabError" src/MLF/Util/ElabError.hs src/MLF/Elab/Types.hs
