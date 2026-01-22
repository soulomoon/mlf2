# Task 1: neutral-elab-types

## Goal
- Add a neutral xMLF type module and re-export from MLF.Elab.Types; update presolution plan modules to use neutral types.

## Scope
- src/MLF/Types/Elab.hs
- src/MLF/Elab/Types.hs
- src/MLF/Constraint/Presolution/Plan/*.hs

## Steps
1. Create MLF.Types.Elab with core data types and base functor instances.
2. Remove those definitions from MLF.Elab.Types and import/re-export the neutral module.
3. Update Plan.* imports to use MLF.Types.Elab for ElabType/ElabScheme.

## Verification
- rg -n "module MLF.Types.Elab|ElabType" src/MLF/Types/Elab.hs src/MLF/Elab/Types.hs
