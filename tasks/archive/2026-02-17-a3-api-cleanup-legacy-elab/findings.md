# Findings

- `expansionToInst` (legacy conversion) is defined in `src/MLF/Elab/Legacy.hs` and re-exported by `src/MLF/Elab/Pipeline.hs`.
- `src-public/MLF/API.hs` and `src-public/MLF/Pipeline.hs` currently import explicit symbol lists from `MLF.Elab.Pipeline` and do not directly export `expansionToInst`.
- A3 acceptance criteria in `TODO.md` point to `src/MLF/Elab/Pipeline.hs`, `src/MLF/Elab/Legacy.hs`, `src-public/MLF/API.hs`, and `src-public/MLF/Pipeline.hs`.
- Implemented quarantine by removing `expansionToInst` from `MLF.Elab.Pipeline` exports/imports; helper remains available only within internal module boundaries via `MLF.Elab.Legacy`/`MLF.Elab.Elaborate`.
- Full validation gate passed after the API cleanup: `cabal build all && cabal test`.
