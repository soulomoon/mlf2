# Findings

## Root Causes
- `src/MLF/Constraint/Presolution/Expansion.hs` had one redundant `EdgeArtifacts` import and one shadowed `gid` binding in a nested trivial-target check.
- `src/MLF/Constraint/Presolution/EdgeProcessing/Solve.hs` shadowed the outer `canonical` helper with a local monadic lookup.
- `test/Presolution/WitnessSpec.hs` still used older `PresolutionState` record fixtures that omitted the new `psPendingWeakenOwners` field.
- `test/Presolution/InstantiateSpec.hs`, `test/NormalizeSpec.hs`, and `test/InertSpec.hs` had test-only hygiene warnings: an unused match, incomplete list-pattern binds, and a redundant import.

## Closeout
- Forced recompilation (`cabal build all --ghc-options=-fforce-recomp`) now emits zero warnings for the repository.
- `cabal test` stays green after the warning fixes, so the closeout is semantics-preserving.
