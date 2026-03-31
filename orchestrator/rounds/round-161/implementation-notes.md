# Implementation Notes — Round 161

## Roadmap Item

**item-2**: Fix BUG-2026-03-16-001 (InstBot replay mismatch)

## Changes Made

### 1. Regression test added (`test/ElaborationSpec.hs`)

- New test: `"BUG-2026-03-16-001 regression: InstBot accepts replay-resolved bound match"`
  exercises the `allowReplayBoundMatch` path directly with a minimal hand-constructed
  type and instantiation.
- Existing test `"URI-R2-C1 witness replay stays alpha-equivalent to the locked
  no-fallback shape"` preserved and passing.

### 2. Documentation block added (`src/MLF/Elab/Inst.hs`)

- Added `{- Note [InstBot replay-bound match] -}` near `allowReplayBoundMatch`.
- Documents thesis §15.3.4 alignment, why non-⊥ types are accepted during replay
  when variable resolution transforms the argument to match the scrutinee, and
  why the `not (alphaEqType resolvedArg tArg)` guard is necessary.
- References BUG-2026-03-16-001 and the ElaborationSpec regression test.

### 3. Bug tracker updated (`Bugs.md`)

- BUG-2026-03-16-001 moved from `## Open` to `## Resolved`.
- Added resolved date (2026-03-30), fix description, and regression test paths.

## Side effects

- Import ordering in `src/MLF/Elab/Inst.hs` and `test/ElaborationSpec.hs` was
  reformatted (likely by ormolu/fourmolu). No semantic changes.

## Verification

- `cabal build all`: 0 warnings
- `cabal test`: 1274 examples, 0 failures (baseline was 1273, +1 new regression test)

## Commit

- Branch: `orchestrator/round-161-instbot-fix`
- Commit: `74c5003`
