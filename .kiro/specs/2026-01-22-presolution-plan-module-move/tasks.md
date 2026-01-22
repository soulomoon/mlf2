# Implementation Plan

- [x] 1. Move planning modules into presolution namespace (file-by-file)
  - Physically move files from `src/MLF/Elab/Generalize/*.hs` to `src/MLF/Constraint/Presolution/Plan/*.hs` per the Design mapping.
  - Update each module header to `module MLF.Constraint.Presolution.Plan.*`.
  - Requirements: 1.1
  - Verification: `rg -n "module MLF.Constraint.Presolution.Plan" src/MLF/Constraint/Presolution/Plan`

- [x] 2. Update imports to new module paths
  - Replace all `MLF.Elab.Generalize.*` planning imports with `MLF.Constraint.Presolution.Plan.*`.
  - Update `MLF.Constraint.Presolution.Plan` to use its new sibling modules (no Elab.Generalize imports).
  - Requirements: 1.2, 2.1, 2.2
  - Verification: `rg -n "MLF.Elab.Generalize" src | rg -v "Generalize.hs"`

- [x] 3. Update Elab apply modules
  - Update `MLF.Elab.Generalize` and `MLF.Elab.Reify` to import the new presolution plan modules and types.
  - Ensure Elab only consumes plan records (no planning logic remains under `MLF.Elab.Generalize.*`).
  - Requirements: 2.1
  - Verification: `rg -n "MLF.Constraint.Presolution.Plan" src/MLF/Elab`

- [x] 4. Update cabal module lists
  - Remove `MLF.Elab.Generalize.*` planning modules from `mlf2.cabal` `other-modules`.
  - Add `MLF.Constraint.Presolution.Plan.*` modules.
  - Requirements: 3.1
  - Verification: `rg -n "Presolution.Plan" mlf2.cabal`

- [x] 5. Test
  - Run full test suite.
  - Requirements: 4.1
  - Verification: `cabal test`

