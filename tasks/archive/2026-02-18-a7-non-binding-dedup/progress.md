# Progress Log: A7 (P2) Non-Binding Dedup Closure

## 2026-02-18
- Audited `TODO.md` + A7 plan history and confirmed remaining target was non-binding dedup.
- Added shared helpers in `test/SpecUtil.hs`:
  - `runConstraintDefault`
  - `runToPresolutionWithAnnDefault`
  - `runPipelineArtifactsDefault`
  - `PipelineArtifacts` record
- Migrated `test/PipelineSpec.hs` to shared helpers and removed local `runPipelineWithPresolution` chain.
- Migrated `test/ElaborationSpec.hs` to shared helpers:
  - removed local `unsafeNormalize` and `generateConstraintsDefault`
  - replaced local solve-chain setup with `runPipelineArtifactsDefault` / `runToPresolutionWithAnnDefault`
- Updated `test/ConstraintGenSpec.hs` default inference helper to reuse shared `unsafeNormalizeExpr`.
- Verification runs:
  - `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline|Elaboration|ConstraintGen'`
  - `cabal build all && cabal test`
  - both passed.
- Updated docs/tracking:
  - marked A7 complete in `TODO.md`
  - added `CHANGELOG.md` entry
  - added `implementation_notes.md` sync section
