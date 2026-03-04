# Findings

## 2026-03-03
- `Solved.fromConstraintAndUf` is currently present in both required guard targets:
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/Run/ResultType/Types.hs`
- `MLF.Elab.Run.ResultType.View` currently rebuilds/queries via `Solved`; this is the main result-type query junction to convert to chi-first wrappers.
- `runPipelineElabWith` already passes `PresolutionView` in both elaboration and result-type input contexts, so `ChiQuery` facade can stay query-only without changing pipeline high-level behavior.
- Existing tests already include checked-authoritative corpus assertions (`representativeMigrationCorpus`) that can be reused for migration characterization.
