# Findings

## 2026-02-11
- Target bug: `BUG-2026-02-11-003` from `Bugs.md`.
- Symptom signatures:
  - V2: `PhiInvariantError "PhiReorder: missing binder identity at positions [0,1]"`
  - V4: `TCInstantiationError ... "InstBot expects TBottom, got Int -> Int"`
- Root-cause evidence (V2):
  - Scheme finalization was dropping quantified binder identities when `usedNames` only tracked free vars + bounds.
  - `phiWithSchemeOmega` reorder strictness required identities for all quantifier spine positions, including non-scheme-added positions.
  - Fix direction that held: preserve quantified names in `Finalize` and require missing reorder identities only for scheme-owned quantifier positions.
- Root-cause evidence (V4):
  - Pre-typecheck elaborated term for the failing path contained:
    - `ETyInst (EVar "f") (InstApp (Int -> Int))`
    - where `f` carried a bounded forall shape from desugared `ELamAnn`.
  - This produced `InstBot` on an already-equal non-bottom bound in type checking.
  - A broad collapse of bounded identities fixed V4 but regressed unrelated `make`/Φ suites.
  - Narrow fix that held: collapse only closed bounded-identity forms in desugared annotated-lambda parameter recovery; pair with equal-bound acceptance in `TypeCheck` `instBot`.
- Final touched modules:
  - `src/MLF/Constraint/Presolution/Plan/Finalize.hs`
  - `src/MLF/Elab/Phi/Omega.hs`
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/TypeCheck.hs`
  - `test/ElaborationSpec.hs`
- Verification evidence:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V"'` => `4 examples, 0 failures`
  - `cabal build all && cabal test` => pass
