# Findings

- `cabal build all --ghc-options='-fforce-recomp -Werror'` initially failed on redundant-import warnings in `MLF.Elab.Phi.Omega.Interpret` and `MLF.Elab.Elaborate.Scope`.
- A quick audit of the nearby warning sites from the earlier dead-export loop found the same class of issue in `MLF.Elab.Elaborate.Algebra`, `MLF.Elab.Elaborate.Annotation`, and `MLF.Elab.Run.ResultType`.
- The fixes were all mechanical import cleanups only: remove redundant explicit imports that GHC 9.12 now treats as unused under the current Prelude/reexport surface.
- After those edits, `cabal build all --ghc-options='-fforce-recomp -Werror'` completed successfully and `cabal test` passed.
