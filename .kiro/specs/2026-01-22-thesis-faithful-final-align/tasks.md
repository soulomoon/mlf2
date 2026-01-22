# Implementation Plan

- [x] 1. Create neutral `ElabError` module
  - Add `src/MLF/Util/ElabError.hs` defining `ElabError` and related helpers (moved from `MLF.Elab.Types`).
  - Update `MLF.Elab.Types` to re‑export or import `ElabError` from the new module.
  - Requirements: 2.1, 2.2
  - Verification: `rg -n "module MLF.Util.ElabError|ElabError" src/MLF/Util/ElabError.hs src/MLF/Elab/Types.hs`

- [x] 2. Update presolution plan imports
  - Replace `MLF.Elab.Types` imports (for `ElabError`) in `MLF.Constraint.Presolution.Plan.*` with the neutral module.
  - Requirements: 2.1
  - Verification: `rg -n "MLF.Elab.Types" src/MLF/Constraint/Presolution/Plan | rg -n "ElabError"`

- [x] 3. Move remaining policy logic out of `MLF.Elab.Generalize`
  - Audit `MLF.Elab.Generalize` for remaining decision/policy logic (scheme‑free‑vars validation, naming/renaming policy, alias decisions).
  - Move those decisions into `MLF.Constraint.Presolution.Plan.Normalize` or a new `Plan.Finalize` module.
  - Ensure `MLF.Elab.Generalize` only applies plan outputs.
  - Requirements: 1.1, 1.2
  - Verification: `rg -n "Plan\.Finalize|Plan\.Normalize" src/MLF/Constraint/Presolution/Plan`

- [x] 4. Update cabal module list
  - Add the new neutral module and any new plan module to `mlf2.cabal` `other-modules`.
  - Requirements: 3.1
  - Verification: `rg -n "ElabError|Plan\.Finalize" mlf2.cabal`

- [x] 5. Test
  - Run full test suite.
  - Requirements: 4.1
  - Verification: `cabal test`
