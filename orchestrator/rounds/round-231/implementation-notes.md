### Changes Made
- `src/MLF/Constraint/Types/Phase/Singletons.hs`: moved the `Phase` kind plus `genSingletons` boilerplate into a dedicated singleton owner module and exported `Phase(..)` / `SPhase(..)` from that owner.
- `src/MLF/Constraint/Types/Phase.hs`: kept `MLF.Constraint.Types.Phase` as the stable caller-facing boundary by re-exporting the dedicated singleton module and retaining the `Next` type family here.
- `mlf2.cabal`: registered `MLF.Constraint.Types.Phase.Singletons` in the internal library and added the focused `PhaseSingletonsSpec` test module to the test-suite stanza.
- `test/PhaseSingletonsSpec.hs`: added a focused smoke spec that imports only `MLF.Constraint.Types.Phase`, pattern matches every exported singleton constructor, and proves `Next` progression through a typed helper.
- `test/Main.hs`: wired the new singleton smoke spec into the shared Hspec harness.
- `docs/architecture.md`: updated the phase-indexed constraint paragraph so it reflects the dedicated singleton owner plus the top-level `MLF.Constraint.Types.Phase` re-export boundary.

### Tests
- `test/PhaseSingletonsSpec.hs`: verifies the exported `SPhase(..)` constructors are usable from `MLF.Constraint.Types.Phase` and that `Next` progression compiles and evaluates through a typed helper.
- `cabal build mlf2-test`: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase singleton foundation"'`: passed.
- `git diff --check`: passed.
- `cabal build all`: passed.
- `cabal test`: passed (`2566` examples, `0` failures).

### Notes
`MLF.Constraint.Types.Phase` remains the intended import surface; the new `Phase.Singletons` module is the dedicated internal owner for the Template Haskell singleton boilerplate. No broader plumbing or milestone-3 graph migration was required for this split.
