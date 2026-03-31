# Review — Round 161

## Round Details

- **Round**: round-161
- **Roadmap item**: item-2 — Fix BUG-2026-03-16-001 (InstBot replay mismatch)
- **Branch**: `orchestrator/round-161-instbot-fix`
- **Commit**: `74c5003`
- **Base**: `f7bdf40` (codex/automatic-recursive-type-inference)

## Baseline Checks

### 1. Build gate

```
$ cabal build all
Build profile: -w ghc-9.12.2 -O1
Building test suite 'mlf2-test' for mlf2-0.2.0.0...
```

**Result**: PASS — exit code 0, 0 warnings.

### 2. Test gate

```
$ cabal test --test-show-details=direct
1274 examples, 0 failures
Test suite mlf2-test: PASS
1 of 1 test suites (1 of 1 test cases) passed.
```

**Result**: PASS — 1274 examples (baseline was 1273, +1 new regression test), 0 failures.

### 3. Thesis conformance gate

```
$ ./scripts/thesis-conformance-gate.sh
[thesis-gate] Running thesis conformance anchors
... (107 obligations checked, all PASS)
[thesis-gate] PASS: thesis conformance anchors are green
```

**Result**: PASS — all 107 thesis obligation anchors green.

### 4. No regressions

Test count: 1274 >= 1273 (pre-round baseline). No test removed or broken.

**Result**: PASS.

### 5. Cabal module lists

No new `.hs` files added. Only existing files modified.

**Result**: PASS (N/A).

### 6. Roadmap identity

- `selection.md` records `roadmap_item_id: item-2`, `roadmap_id: 2026-03-30-01-codebase-quality-and-coverage-improvements`, `roadmap_revision: rev-001`.
- `state.json` matches.

**Result**: PASS.

## Task-Specific Checks (item-2)

### Regression test exercises the previously-failing replay path

The new test `"BUG-2026-03-16-001 regression: InstBot accepts replay-resolved bound match"` is present at line 3169 of `test/ElaborationSpec.hs`. It exercises the `allowReplayBoundMatch` path via `applyInstantiation` where the replay environment resolves `tArg` to match the bound, which was the exact failure path of BUG-2026-03-16-001.

**Result**: PASS.

### Bugs.md updated with resolution details and regression test paths

`Bugs.md` diff shows:
- BUG-2026-03-16-001 moved from `## Open` to `## Resolved`
- Added `Resolved: 2026-03-30`
- Added fix description referencing `allowReplayBoundMatch`
- Added regression test paths: both the new test and the existing URI-R2-C1 test

**Result**: PASS.

### {- Note -} block accurately describes the fix semantics

`{- Note [InstBot replay-bound match] -}` added near `allowReplayBoundMatch` in `src/MLF/Elab/Inst.hs`. Documents:
- Thesis §15.3.4 alignment
- Why non-⊥ accepted during replay when variable resolution transforms argument
- Why `not (alphaEqType resolvedArg tArg)` guard is necessary
- References BUG-2026-03-16-001 and the ElaborationSpec regression test

**Result**: PASS.

### Diff stays within scope

3 files changed:
1. `Bugs.md` — Bug resolution update (in scope)
2. `src/MLF/Elab/Inst.hs` — Note block + import reformatting (in scope; reformatting is cosmetic)
3. `test/ElaborationSpec.hs` — New regression test + import reformatting (in scope; reformatting is cosmetic)

No unrelated functional changes.

**Result**: PASS.

## Evidence Summary

All 6 baseline checks pass. All 4 task-specific checks pass. The round adds 1 focused regression test for the InstBot replay-bound-match path, documents the fix semantics with a proper `{- Note -}` block, and formally resolves BUG-2026-03-16-001 in Bugs.md. The import reformatting across Inst.hs and ElaborationSpec.hs is cosmetic (likely ormolu/fourmolu) and introduces no semantic changes.

## Decision

**APPROVED**
