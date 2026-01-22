# Task 4: cabal module list + tests

## Goal
- Register new modules in mlf2.cabal and run the test suite.

## Scope
- mlf2.cabal

## Steps
1. Add new neutral modules to other-modules list.
2. Run cabal test.

## Verification
- rg -n "MLF.Types.Elab|MLF.Reify" mlf2.cabal && cabal test
