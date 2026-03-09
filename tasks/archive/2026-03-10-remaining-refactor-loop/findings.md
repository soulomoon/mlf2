# Findings

## Initial Notes
- The remaining hotspots are still concentrated in `Omega`, `EdgeUnify`, `Reify.Core`, `Solve`, and `Elaborate`.
- `MLF.API` still exports pipeline/execution/runtime functionality that must move behind `MLF.Pipeline` in Loop A.

## Loop A-C Findings
- Loop A hard cut is practical: only a handful of in-repo callers needed to move from `MLF.API` to `MLF.Pipeline`, and the strongest enforcement point is a source guard on the two public modules.
- The full `EdgeArtifacts` rewrite across `PresolutionResult` and `PresolutionState` created disproportionate test fallout; the safer high-value version is to introduce `EdgeArtifacts` for `dropTrivialSchemeEdges`, `ElabEnv`, `ResultTypeInputs`, and pipeline prep while leaving presolution result/state record shapes stable for now.
- The remaining assembly cleanup in `Run.Pipeline` and `Presolution.Driver` is low-risk once it is phrased as explicit helper records/functions (`SnapshotViews`, `TraceCopyArtifacts`, `mkInitialPresolutionState`, `tyExpNodeIds`).
- The monolith splits are compatible with keeping the original façade modules as export owners; the cleanest path is façade + child-implementation modules, not export-surface proliferation.
- `EdgeUnify` and `Solve` were the cleanest structural splits because their top-level groups already aligned with state/omega/unify and worklist/harmonize/finalize boundaries.
- `Omega` and `Elaborate` benefit most from moving their large logic into child modules while preserving the original entrypoints as thin façades.

- The practical low-risk version of Loop B keeps `PresolutionResult` / `PresolutionState` record shapes stable while still introducing `EdgeArtifacts` for result-type/elaboration/plumbing and retiring the triple return from `dropTrivialSchemeEdges`.
