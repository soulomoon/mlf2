# Progress

- 2026-03-10: Initialized the dead-export agent-team loop artifacts and mechanism queue.

- 2026-03-10: Row 1 research B inspected the active task plan, `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, and `src/MLF/Elab/Run/ChiQuery.hs`; next step is git-history plus nearby test-guard review.

- 2026-03-10: Row 1 added the failing `chi-first guard: ChiQuery no longer exposes chiLookupBindParent or chiBindParents` matcher and verified the expected red failure.
- 2026-03-10: Row 1 removed the dead `ChiQuery` exports/definitions, then passed the focused guard, the `chi-first` slice, and `cabal build all && cabal test`.

- 2026-03-10: Row 1 research B confirmed that the 2026-03-09 `chiCanonicalBindParents` retirement established the active cleanup rule for `ChiQuery`; `test/PipelineSpec.hs` is the natural guard home, and the live working tree already follows that pattern.
- 2026-03-10: Focused validation passed for `test/PipelineSpec.hs` guards: exact matches for `chi-first guard: ChiQuery no longer exposes chiLookupBindParent or chiBindParents` and `chi-first guard: ChiQuery no longer defines chiCanonicalBindParents` both returned `1 example, 0 failures`.
- 2026-03-10: Row 2 added the failing `binding guard: Validation no longer exports validateSingleGenRoot` matcher and verified the expected red failure.
- 2026-03-10: Row 2 removed the stale `validateSingleGenRoot` export, then passed the focused guard, the binding suite, and `cabal build all && cabal test`.
- 2026-03-10: Row 3 added the failing `canonicalizer guard: canonicalizeRef stays retired` matcher; the first attempt surfaced a missing `forM_` import in the test, which was fixed before rerunning the intended red case.
- 2026-03-10: Row 3 removed `canonicalizeRef`, then passed the focused guard, the canonicalizer suite, and `cabal build all && cabal test`.
- 2026-03-10: Row 4 added the failing `ga-scope debug guard: Debug no longer exposes edgeOrigins` matcher and verified the intended red failure.
- 2026-03-10: Row 4 removed `edgeOrigins`, then passed the focused guard, the `Pipeline (Phases 1-5)` slice, and `cabal build all && cabal test`.
- 2026-03-10: Row 5 added the failing `term-closure guard: TermClosure no longer exposes closeTermWithSchemeSubst` matcher and verified the intended red failure.
- 2026-03-10: Row 5 removed `closeTermWithSchemeSubst`, then passed the focused guard, the `Phase 6` slice, and `cabal build all && cabal test`.
- 2026-03-10: All mechanism rows reached `YES`; wrote the terminal orchestrator status and prepared the loop task for archive.
