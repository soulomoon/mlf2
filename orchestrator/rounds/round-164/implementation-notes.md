# Implementation Notes — Round 164, Item 5: Research Module Hygiene

## Summary

Extracted the 10 `MLF.Research.*` modules from `mlf2-internal` into a new
`mlf2-research` private internal library, enforcing the pre-existing
one-way dependency: Research → Core, never Core → Research.

## Plan Deviations

### Separate source directory (`src-research/`)

The plan specified `hs-source-dirs: src` (shared with `mlf2-internal`) for
`mlf2-research`. This does not work with Cabal internal libraries:

1. **Duplicate compilation**: Cabal resolves imported modules from
   `hs-source-dirs` before looking at `build-depends`. With shared `src/`,
   every transitive module imported by Research files was recompiled inside
   `mlf2-research` (~181 modules instead of 10).
2. **Missing transitive deps**: The recompiled modules needed `recursion-schemes`,
   `megaparsec`, and `mtl` — not in `mlf2-research`'s deps.
3. **Linker symbol collisions**: Even after adding those deps, executables
   depending on both `mlf2-internal` and `mlf2-research` failed at link time
   due to duplicate object files for the same modules.

**Fix**: Moved the Research source files to `src-research/MLF/Research/` and
set `hs-source-dirs: src-research` in the `mlf2-research` stanza. No `.hs`
file contents were modified — only their filesystem location changed.

### Promoting hidden modules to exposed

Four modules that were `other-modules` in `mlf2-internal` needed to be
promoted to `exposed-modules` because Research modules import them:

- `MLF.Constraint.Presolution.Base`
- `MLF.Elab.Run.Annotation`
- `MLF.Elab.Run.Generalize`
- `MLF.Elab.Types`

These were already importable when Research lived inside `mlf2-internal`
(same component). As a separate library, they must be explicitly exposed.

## Files Changed

| File / Path | Change |
|---|---|
| `mlf2.cabal` | Added `mlf2-research` library stanza; removed 10 Research modules from `mlf2-internal` exposed-modules; promoted 4 hidden modules; added `mlf2:mlf2-research` to `mlf2` exe and `mlf2-test` build-depends |
| `AGENTS.md` | Added guidance about `mlf2-research` and `src-research/` |
| `src/MLF/Research/` → `src-research/MLF/Research/` | Moved 10 `.hs` files (no content changes) |

## Verification

- `cabal clean && cabal build all` — all 6 components build cleanly
- `cabal test` — 1288 examples, 0 failures (unchanged count)
