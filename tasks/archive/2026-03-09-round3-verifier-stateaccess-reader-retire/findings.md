# Findings

## Repository / docs
- `TODO.md` continues the March 8, 2026 cleanup pattern of retiring duplicate/facade/helper surfaces and keeping single helper homes (`Task 69`, `Task 68`, `Task 61`, `Task 59`).
- `implementation_notes.md:34` records `2026-03-09 — Presolution compatibility facade retired`, explicitly favoring direct owner imports and a smaller maintained presolution module graph.
- `CHANGELOG.md:6`-`CHANGELOG.md:8` logs the same March 9 cleanup wave: stale presolution re-export surfaces removed and the thin presolution compatibility facade retired.
- `Bugs.md:361`-`Bugs.md:388` shows the row3 owner-boundary closure work still treats `EdgeUnify`/`StateAccess` as an important seam, but no active bug entry depends on the `WithCanonicalT` reader layer itself.

## Code
- `src/MLF/Constraint/Presolution/StateAccess.hs:134`-`src/MLF/Constraint/Presolution/StateAccess.hs:164` still defines the reader layer (`WithCanonicalT`, `runWithCanonical`, `askConstraint`, `askCanonical`, `canonicalize`).
- `src/MLF/Constraint/Presolution/StateAccess.hs:213` defines `withCanonical`, and `src/MLF/Constraint/Presolution/StateAccess.hs:339`-`src/MLF/Constraint/Presolution/StateAccess.hs:364` defines the reader-only binding helpers.
- Repo-wide search shows the live external caller is effectively only `src/MLF/Constraint/Presolution/EdgeUnify.hs:576`-`src/MLF/Constraint/Presolution/EdgeUnify.hs:599` (`checkNodeLocked`), plus the architectural note in `src/MLF/Constraint/Presolution/Base.hs:277`-`src/MLF/Constraint/Presolution/Base.hs:295`.
- `src/MLF/Constraint/Presolution/StateAccess.hs:240`-`src/MLF/Constraint/Presolution/StateAccess.hs:243` already exposes the direct lifted helper `lookupBindParentM`, and `src/MLF/Constraint/Presolution/EdgeUnify.hs:323`-`src/MLF/Constraint/Presolution/EdgeUnify.hs:330` / `src/MLF/Constraint/Presolution/EdgeUnify.hs:859`-`src/MLF/Constraint/Presolution/EdgeUnify.hs:865` show `EdgeUnify` already uses direct `PresolutionM` canonical/constraint access elsewhere.
- The current `checkNodeLocked` body is read-only and performs a strict-ancestor walk; rewriting it against direct lifted helpers should remove the duplicate reader surface without changing semantics.

## Thesis
- `papers/these-finale-english.txt:2844`-`papers/these-finale-english.txt:2849` defines locked/red nodes as flexibly bound nodes with a rigid edge above them, not nodes rigidly bound by their own edge.
- `papers/these-finale-english.txt:2851`-`papers/these-finale-english.txt:2864` allows only raising/merging for nodes with rigid edges; it constrains allowed operations, not implementation monad structure.
- `papers/these-finale-english.txt:13436`-`papers/these-finale-english.txt:13455` requires translatable presolutions to avoid inert-locked nodes and weakens those nodes during normalization.
- `papers/xmlf.txt:932`-`papers/xmlf.txt:944` says locked nodes are flexibly bound nodes whose path to the root contains a rigid edge, and all four instance operations are disallowed on locked nodes.
- `papers/xmlf.txt:1364`-`papers/xmlf.txt:1367` requires `Weaken(n)` to appear after operations below `n`, preventing operations under a rigidly bound node; `papers/xmlf.txt:1428`-`papers/xmlf.txt:1442` translates rigid-node operations to identity.

## Validation
- Required source search still finds the reader layer live in `StateAccess` and only one production caller in `EdgeUnify`.
- Attempted `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`, but test-suite build is currently blocked by an unrelated workspace issue: `Solved.fromPreRewriteState` is no longer exported while `test/Parity/FrozenArtifacts.hs` and `test/SpecUtil.hs` still import it.
