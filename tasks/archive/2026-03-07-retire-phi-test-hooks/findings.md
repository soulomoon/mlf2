# Findings

- `MLF.Elab.Phi.TestOnly` is the only remaining `TestOnly` module under `src/`.
- `shadowCompareTypes` and `selectSolvedOrderWithShadow` already live in `MLF.Elab.Generalize`; only the wrappers are test-only.
- `normalizeInstTestOnly` is used in exactly two tests, both asserting normalization shape rather than runtime behavior.
- `IdentityBridge` has one remaining production use: diagnostics inside `MLF.Elab.Phi.Omega`; all other uses are tests.
- The pure witness-domain ranking logic did not need any production API after the Binder cleanup; only the diagnostic list in `Omega` still needed a local raw/copy/trace projection.
- The renamed `WitnessDomain` suite currently covers 23 examples, matching the pre-migration unit-spec count.
