# Plan: Item-5 — Research Module Hygiene

**Round**: 164
**Roadmap**: `2026-03-30-01-codebase-quality-and-coverage-improvements`
**Revision**: `rev-001`
**Roadmap dir**: `orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001`
**Roadmap item**: `item-5`
**Worktree**: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-164/`

## Goal

Extract the 10 `MLF.Research.*` modules from the `mlf2-internal` library into a separate `mlf2-research` internal library stanza. The main `mlf2-internal` must build without Research modules in scope. All existing tests must continue to pass with the same count.

## Pre-Implementation Findings

### Research modules (10 files, all under `src/MLF/Research/`)

| Module | Core (`mlf2-internal`) imports | External package imports |
|--------|-------------------------------|--------------------------|
| `Types` | _none_ | `filepath` |
| `P1` | `MLF.Constraint.Types.Graph`, `MLF.Reify.TypeOps`, `MLF.Types.Elab` | `directory`, `filepath` |
| `P2` | **Heavy**: `MLF.Constraint.*` (7 modules), `MLF.Elab.*` (7 modules), `MLF.Frontend.*` (3 modules), `MLF.Reify.*` (2 modules) | `containers`, `directory`, `filepath` |
| `P3` | _none_ (only `Research.*.Types`) | `directory`, `filepath` |
| `P4` | _none_ (only `Research.*.Types`) | `directory`, `filepath` |
| `D1` | _none_ (only `Research.*.P2`, `Research.*.Types`) | `directory`, `filepath` |
| `D2` | _none_ (only `Research.*.Types`) | `directory`, `filepath` |
| `D3` | _none_ (only `Research.*.Types`) | `directory`, `filepath` |
| `Entrypoint` | `MLF.Elab.Pipeline`, `MLF.Frontend.Normalize`, `MLF.Frontend.Syntax` | `directory` |
| `Artifact` | _none_ (only other Research modules) | `directory`, `filepath` |

**Conclusion**: `mlf2-research` must depend on `mlf2:mlf2-internal` plus `base`, `containers`, `directory`, `filepath`.

### Consumers of Research modules

| Consumer | Research imports | Action needed |
|----------|-----------------|---------------|
| `app/Main.hs` (executable `mlf2`) | `MLF.Research.*.Entrypoint`, `MLF.Research.*.Types` | Add `mlf2:mlf2-research` to executable `build-depends` |
| `test/Research/UriR2C1PrototypeP1Spec.hs` | `MLF.Research.*.Entrypoint`, `MLF.Research.*.Types` | Add `mlf2:mlf2-research` to test suite `build-depends` |
| `test/Research/C1AuthoritativeSurfaceSpec.hs` | _none_ (only core modules) | No change needed |
| `test/Research/P5ClearBoundarySpec.hs` | _none_ (only core modules) | No change needed |
| `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` | _none_ (only core modules) | No change needed |
| `frozen-parity-gen` executable | _none_ | No change needed |

### No reverse dependency from core → Research

Verified: no non-Research `src/` module imports `MLF.Research.*`. Clean separation already exists.

## Steps

### Step 1: Add `mlf2-research` internal library stanza to `mlf2.cabal`

**File**: `mlf2.cabal`
**Location**: Insert new stanza after the `mlf2-internal` library stanza (after line 265) and before the public `library` stanza (line 269).

**Content to insert**:

```cabal
library mlf2-research
    import:           warnings

    visibility:       private

    exposed-modules:  MLF.Research.URI.R2.C1.Prototype.Types,
                      MLF.Research.URI.R2.C1.Prototype.D1,
                      MLF.Research.URI.R2.C1.Prototype.D2,
                      MLF.Research.URI.R2.C1.Prototype.D3,
                      MLF.Research.URI.R2.C1.Prototype.P1,
                      MLF.Research.URI.R2.C1.Prototype.P2,
                      MLF.Research.URI.R2.C1.Prototype.P3,
                      MLF.Research.URI.R2.C1.Prototype.P4,
                      MLF.Research.URI.R2.C1.Prototype.Artifact,
                      MLF.Research.URI.R2.C1.Prototype.Entrypoint

    build-depends:    base ^>=4.21.0.0,
                      containers,
                      directory,
                      filepath,
                      mlf2:mlf2-internal

    hs-source-dirs:   src

    default-language: Haskell2010

    default-extensions: LambdaCase
```

**Verification**: `cabal build mlf2-research` (should compile the 10 Research modules).

### Step 2: Remove Research modules from `mlf2-internal` exposed-modules

**File**: `mlf2.cabal`
**Lines to remove**: Lines 94–103 (the 10 `MLF.Research.*` entries in `mlf2-internal`'s `exposed-modules`).

Remove these exact lines from the `mlf2-internal` `exposed-modules` list:
```
                      MLF.Research.URI.R2.C1.Prototype.Types,
                      MLF.Research.URI.R2.C1.Prototype.D1,
                      MLF.Research.URI.R2.C1.Prototype.D2,
                      MLF.Research.URI.R2.C1.Prototype.D3,
                      MLF.Research.URI.R2.C1.Prototype.P1,
                      MLF.Research.URI.R2.C1.Prototype.P2,
                      MLF.Research.URI.R2.C1.Prototype.P3,
                      MLF.Research.URI.R2.C1.Prototype.P4,
                      MLF.Research.URI.R2.C1.Prototype.Artifact,
                      MLF.Research.URI.R2.C1.Prototype.Entrypoint,
```

**Verification**: `cabal build mlf2-internal` — must succeed without Research modules. This confirms no core module depends on Research.

### Step 3: Add `mlf2:mlf2-research` dependency to the `mlf2` executable

**File**: `mlf2.cabal`, executable `mlf2` stanza (lines 290–313).
**Change**: Add `mlf2:mlf2-research` to `build-depends`.

Before:
```cabal
    build-depends:
        base ^>=4.21.0.0,
        mlf2,
        mlf2:mlf2-internal
```

After:
```cabal
    build-depends:
        base ^>=4.21.0.0,
        mlf2,
        mlf2:mlf2-internal,
        mlf2:mlf2-research
```

**Verification**: `cabal build mlf2` (the executable) — must succeed.

### Step 4: Add `mlf2:mlf2-research` dependency to the `mlf2-test` test suite

**File**: `mlf2.cabal`, test suite `mlf2-test` stanza (lines 333–425).
**Change**: Add `mlf2:mlf2-research` to `build-depends`.

Before:
```cabal
    build-depends:
        base ^>=4.21.0.0,
        containers,
        directory,
        filepath,
        mtl,
        mlf2,
        mlf2:mlf2-internal,
        hspec >=2.11 && <2.12,
        QuickCheck >=2.14 && <2.18
```

After:
```cabal
    build-depends:
        base ^>=4.21.0.0,
        containers,
        directory,
        filepath,
        mtl,
        mlf2,
        mlf2:mlf2-internal,
        mlf2:mlf2-research,
        hspec >=2.11 && <2.12,
        QuickCheck >=2.14 && <2.18
```

**Verification**: `cabal build mlf2-test` — must succeed.

### Step 5: Update `AGENTS.md` module organization guidance

**File**: `AGENTS.md`, section "## Project Structure & Module Organization".

**Add** a new bullet after the existing `src/` description:

```
- `MLF.Research.*` modules live in a separate internal library (`mlf2-research`) in the same `src/` source directory. They depend on `mlf2-internal` but `mlf2-internal` must not depend on them. Test files and executables that use Research modules must add `mlf2:mlf2-research` to their `build-depends`.
```

### Step 6: Final verification

Run in the worktree:

```bash
cabal clean
cabal build all && cabal test
```

**Expected outcome**:
- `mlf2-internal` builds without Research modules in scope.
- `mlf2-research` builds (depends on `mlf2-internal`).
- `mlf2` (public library) builds.
- `mlf2` (executable) builds (depends on `mlf2-research`).
- `frozen-parity-gen` builds.
- `mlf2-test` builds and all tests pass with the same test count (no regressions).

**Verification-contract checks** (from `verification.md` item-5):
- ✅ `mlf2-internal` builds without Research modules in scope (Step 2 verification).
- ✅ Research modules still compile (Step 1 verification).
- ✅ `cabal build all && cabal test` passes (Step 6).

## Files Modified (complete list)

| File | Change |
|------|--------|
| `mlf2.cabal` | Add `mlf2-research` library stanza; remove 10 Research modules from `mlf2-internal` exposed-modules; add `mlf2:mlf2-research` to `mlf2` executable and `mlf2-test` build-depends |
| `AGENTS.md` | Add guidance about `mlf2-research` internal library |

## Files NOT Modified

- No `.hs` source files change (no import modifications needed — module names stay the same).
- `test/Main.hs` — unchanged (Research test wiring stays as-is).
- `frozen-parity-gen` executable stanza — unchanged (doesn't import Research modules).
- Public `library` stanza — unchanged (doesn't depend on Research).

## Risk Assessment

**Low risk**. This is a pure Cabal reorganization:
- No Haskell source changes.
- Module names stay identical — all imports remain valid.
- The separation already exists in practice (no core→Research imports).
- Only risk: forgetting a dependency in the new stanza → caught by `cabal build all`.
