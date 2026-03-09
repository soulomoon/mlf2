# Findings

## Live Tree Snapshot
- The major split façades already exist in the current tree: `MLF.Elab.Phi.Omega`, `MLF.Constraint.Presolution.EdgeUnify`, `MLF.Reify.Core`, `MLF.Constraint.Solve`, and `MLF.Elab.Elaborate` all resolve to façade modules with focused child implementation modules beside them.
- The public-surface hard cut is already live in the codebase: `src-public/MLF/Pipeline.hs`, `src-public/MLF/API.hs`, and `src-public/MLF/XMLF.hs` exist as the current public topology, with `MLF.Pipeline` owning runtime/pipeline behavior and `MLF.API` owning frontend syntax helpers.
- The low-risk shared-plumbing seams called out in the brief are already present in the current design: `EdgeArtifacts`, `TraceCopyArtifacts`, `prepareTraceCopyArtifacts`, `mkInitialPresolutionState`, and `tyExpNodeIds` are already part of the split tree and should be stabilized, not redesigned.

## Guard/Test Posture
- `PipelineSpec` already contains source guards touching `MLF.Elab.Elaborate`, `MLF.Constraint.Presolution.EdgeUnify`, and `MLF.Elab.Phi.Omega`, so Loop 2 should strengthen existing living guards instead of inventing a separate enforcement surface.
- `PresolutionFacadeSpec` is already the natural adjacent guard surface for the presolution façade and test-support seam; it is the right home for any additional `EdgeUnify`/presolution ownership assertions.
- `PublicSurfaceSpec` and `RepoGuardSpec` already encode parts of the `MLF.API` ↔ `MLF.Pipeline` split, which means Loop 3 is a stabilization pass that tightens caller/doc/contract agreement rather than another public API redesign.
- `mlf2.cabal` already wires the relevant owning specs (`PipelineSpec`, `PresolutionFacadeSpec`, `PublicSurfaceSpec`, `RepoGuardSpec`, and the split-owner regression slices), so Loop 4 is a hygiene audit over exposure/import direction and stale references rather than a first-time module-registration task.

## Queue Interpretation
- The correct next campaign is post-split stabilization-and-landing, not another decomposition campaign: the tree already reflects the major structural moves.
- Loop 0 must freeze the baseline from the live tree and current branch state, not from archived pre-split planning assumptions.
- Loop 1 should stay strictly warning/dead-import/redundant-binding cleanup and must not move helpers back into façades.
- Loop 5 should preserve the owner sweep order `Omega` → `EdgeUnify` → `Reify.Core` → `Solve` → `Elaborate` so the most upstream split owners validate before downstream orchestrators.

## Completion Findings
- The stabilization loop completed without further production refactoring: the live split tree was already structurally landed, so the only missing work was explicit guard coverage plus verification.
- `test/PipelineSpec.hs` is the right living guard surface for the runtime-facing split façades (`Omega`, `EdgeUnify`, `Elaborate`), while `test/RepoGuardSpec.hs` is the right home for `Reify.Core`, `Solve`, public-topology doc agreement, and Cabal child-module ownership.
- The public hard cut required no code movement in the live tree: `app/Main.hs` already uses `MLF.API` only for frontend syntax helpers and `MLF.Pipeline` for runtime execution, and the docs already described the same topology.
- The Cabal graph was already normalized; the landed value of Loop 4 was locking that state with a regression guard rather than changing module exposure again.
