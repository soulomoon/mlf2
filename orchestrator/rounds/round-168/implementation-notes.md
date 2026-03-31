# Round 168 implementation notes

## Summary
- Added the planned `{- Note [...] -}` design-rationale blocks to these 6 new round-163 split submodules:
  - `src/MLF/Constraint/Normalize/Internal.hs`
  - `src/MLF/Reify/Type/Core.hs`
  - `src/MLF/Constraint/Presolution/Plan/Env.hs`
  - `src/MLF/Constraint/Presolution/Plan/Generalize.hs`
  - `src/MLF/Constraint/Presolution/Plan/ReifyStep.hs`
  - `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs`
- Updated `implementation_notes.md` to document the 4 remaining round-163 module splits (`MLF.Reify.Type`, `MLF.Elab.Run.ResultType.Fallback`, `MLF.Constraint.Presolution.Plan`, `MLF.Elab.Phi.Omega.Interpret`).
- Added the requested documentation-hygiene entry to `CHANGELOG.md` under `## Unreleased` / `### Changed`.

## Scope adherence
- Kept the Haskell file changes comment-only.
- Did not change imports, behavior, cabal metadata, or tests.

## Verification
- Confirmed the note-reference names resolve under `src/`.
- Ran `cabal build all && cabal test` in the round-168 worktree and got `1302 examples, 0 failures` (`Test suite mlf2-test: PASS`).
