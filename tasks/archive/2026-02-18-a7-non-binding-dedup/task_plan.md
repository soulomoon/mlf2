# Task Plan: A7 (P2) Non-Binding Dedup Closure

## Goal
Close the remaining A7 non-binding dedup work by consolidating leftover test-harness pipeline setup duplication into shared `SpecUtil` helpers and migrating specs to those helpers without behavior changes.

## Scope
- `test/SpecUtil.hs`
- `test/PipelineSpec.hs`
- `test/ElaborationSpec.hs`
- `test/ConstraintGenSpec.hs`
- `TODO.md`
- `CHANGELOG.md`
- `implementation_notes.md`

## Phases
- [x] Phase 1: Audit remaining A7 non-binding duplication and identify concrete targets.
- [x] Phase 2: Add shared helpers to `SpecUtil` for common pipeline stages.
- [x] Phase 3: Migrate `PipelineSpec`, `ElaborationSpec`, `ConstraintGenSpec` to shared helpers.
- [x] Phase 4: Verify (`cabal build all && cabal test`) and update tracking docs.

## Decisions
- Keep all changes behavior-preserving; this is a harness refactor only.
- Prefer shared helper APIs that return richer artifacts (`PipelineArtifacts`) to avoid reintroducing ad hoc solve chains.
- Close A7 in `TODO.md` only after full build+test gate passes.

## Errors Encountered
| Date | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-02-18 | Introduced an incomplete local `chaseRedirects` declaration in `PipelineSpec` during helper migration | 1 | Removed local declaration and used `MLF.Elab.Run.Util.chaseRedirects` import; reran tests |
| 2026-02-18 | Unused import/local-bind warnings in `PipelineSpec` after migration | 1 | Removed unused import (`requireRight`) and restored use of canonicalized `root'` in `generalizeAt` test |
