# Tasks

- [ ] 1. Rebind explicit-forall binders under scheme gen in annotated schemes
  - Identify quantifier variable nodes created for `ELetAnn` schemes.
  - Rebind those variables to `GenRef schemeGen` with `BindFlex`.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Requirements: 1.1, 1.2
  - Verification: `rg -n "ELetAnn|schemeGen" src/MLF/Frontend/ConstraintGen/Translate.hs`

- [ ] 2. Remove explicit-forall free-name exemption in generalization
  - Delete the “type-bound outside” allowance from `allowedNames`.
  - Files: `src/MLF/Elab/Generalize.hs`
  - Requirements: 1.3
  - Verification: `rg -n "allowedNames" src/MLF/Elab/Generalize.hs`

- [ ] 3. Validate bounded-aliasing and full suite
  - Ensure bounded-aliasing annotated-let test passes without exemptions.
  - Run full suite.
  - Requirements: 1.4, 1.5
  - Verification: `cabal test --test-show-details=direct`
