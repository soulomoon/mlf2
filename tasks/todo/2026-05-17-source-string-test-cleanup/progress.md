# Progress

## 2026-05-17

- Created cleanup task after five read-only scanner passes.
- Consolidated scanner output into `task_plan.md` and `findings.md`.
- Deleted redundant source-file grep guards with nearby semantic coverage:
  - `test/TranslatablePresolutionSpec.hs`
  - `test/Presolution/EdgePlannerSpec.hs`
  - `test/Presolution/UnificationClosureSpec.hs`
  - `test/FrontendParseSpec.hs`
- Deleted small one-off source-file grep guards:
  - `test/FrontendNormalizeSpec.hs`
  - `test/GeneralizeSpec.hs`
  - `test/Reify/TypeSpec.hs`
  - `test/CanonicalizerSpec.hs`
  - `test/BindingSpec.hs`
  - `test/PublicSurfaceSpec.hs`
  - `test/PresolutionSpec.hs`
- Removed the high-noise source-file grep guards from `test/PipelineSpec.hs`, preserving representative-corpus and runtime behavior checks.
- Deleted remaining source-file grep guards in `test/ElaborationSpec.hs` and `test/Constraint/SolvedSpec.hs`.
- Deleted `test/PresolutionFacadeSpec.hs`, which was entirely source-substring guards, and unwired it from `mlf2.cabal` and `test/Main.hs`.
- Reworked `test/RepoGuardSpec.hs` from broad source/prose marker checks to structural repository guards:
  - spec module wiring in Cabal and `test/Main.hs`
  - removed legacy module files/imports
  - owner-boundary import scans
  - implementation-only Cabal exposure checks
- Fixed validation fallout in `test/PipelineSpec.hs` and `test/Presolution/EdgePlannerSpec.hs`.
- Verified that no tests still read source/docs files under `src`, `src-public`, `app`, `docs`, `README`, `CHANGELOG`, `TODO`, `Bugs`, or `AGENTS.md` for source-substring guards. Remaining repo-file reads are Cabal structural checks in `test/RepoGuardSpec.hs`.
- Validation:
  - `cabal test mlf2-test --test-options='--match=Repository'` passed.
  - `cabal build all && cabal test` passed.
