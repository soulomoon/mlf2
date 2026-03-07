# Progress Log

## 2026-03-07
- Initialized task folder and confirmed the current library-side test hooks still exposed from `mlf2-internal`.
- Verified the remaining production `IdentityBridge` use is diagnostics only in `MLF.Elab.Phi.Omega`.
- Added `test/Phi/WitnessDomainUtil.hs` and renamed `test/Phi/IdentityBridgeSpec.hs` to `test/Phi/WitnessDomainSpec.hs`.
- Removed `src/MLF/Elab/Phi/TestOnly.hs` and `src/MLF/Elab/Phi/IdentityBridge.hs`, updated `mlf2.cabal`, and localized the witness-domain diagnostic helper inside `src/MLF/Elab/Phi/Omega.hs`.
- Retargeted `GeneralizeSpec` to `MLF.Elab.Generalize`, rewrote the two `normalizeInst` checks to behavior-level assertions, and updated source guards to assert the deleted library modules are absent.
- Targeted verification passes:
  - `WitnessDomain` -> PASS (`23 examples, 0 failures`)
  - `Generalize shadow comparator` -> PASS (`8 examples, 0 failures`)
  - `no-trace test entrypoint fails fast with MissingEdgeTrace` -> PASS (`1 example, 0 failures`)
  - `elab-input thesis-exact guard` -> PASS (`2 examples, 0 failures`)
  - `elab-input absolute thesis-exact guard` -> PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard` -> PASS (`1 example, 0 failures`)
- Full verification: `cabal build all && cabal test` -> PASS (`966 examples, 0 failures`).
