# Progress Log

- 2026-03-10: Loaded `planning-with-files`, `executing-plans`, and `test-driven-development` guidance.
- 2026-03-10: Created task workspace at `tasks/todo/2026-03-10-remaining-refactor-loop/`.
- 2026-03-10: Implemented Loop A hard cut: `MLF.Pipeline` is now the public runtime/execution owner; `MLF.API` is frontend-only, and downstream callers/tests were rewired accordingly.
- 2026-03-10: Added/updated Loop A guards in `PublicSurfaceSpec` and `RepoGuardSpec`, and verified the focused public-surface / repository guard slices.
- 2026-03-10: Implemented the low-risk `EdgeArtifacts` bundle path for `dropTrivialSchemeEdges`, `ElabEnv`, `ResultTypeInputs`, and pipeline/result-type wiring.
- 2026-03-10: Implemented Loop C helper prep: `TraceCopyArtifacts`, `prepareTraceCopyArtifacts`, `mkInitialPresolutionState`, and `tyExpNodeIds`.
- 2026-03-10: `cabal build mlf2-test` passes after Loops A-C.
- 2026-03-10: Passed `cabal build all && cabal test` after Loops A-C; the monolith split loops remain to be implemented.
- 2026-03-10: Split `MLF.Elab.Phi.Omega`, `MLF.Constraint.Presolution.EdgeUnify`, `MLF.Reify.Core`, `MLF.Constraint.Solve`, and `MLF.Elab.Elaborate` into façade + child-module structures.
- 2026-03-10: Added the new split modules to `mlf2.cabal` and fixed the resulting integration fallout until `cabal build mlf2-test` and `cabal build all && cabal test` passed again.
