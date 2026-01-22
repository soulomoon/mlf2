# Task 4: cabal-update

## Goal
- Add new modules to mlf2.cabal other-modules.

## Scope
- mlf2.cabal

## Steps
1. Add MLF.Util.ElabError.
2. Add MLF.Constraint.Presolution.Plan.Finalize.

## Verification
- rg -n "ElabError|Plan\.Finalize" mlf2.cabal
