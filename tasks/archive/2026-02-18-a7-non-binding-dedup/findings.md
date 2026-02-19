# Findings: A7 (P2) Non-Binding Dedup Closure

## Discovery Log
- Remaining A7 work was not binding-core; it was non-binding harness duplication in `PipelineSpec`/`ElaborationSpec` plus one local normalize wrapper in `ConstraintGenSpec`.
- `ConstraintGen.Translate` already used shared local combinators (`withScopedBuild`, `withScopedRebind`, `attachUnder`, `rebindScopeRoot`), so source-level non-binding scope wiring was already aligned with the A7 AC.
- Existing `SpecUtil` helpers were insufficient for tests that needed both presolution + solved + annotated/root artifacts from a single pipeline run.
- Adding `PipelineArtifacts` and companion helpers in `SpecUtil` allowed migration without mixing artifacts from different runs.
- After migration, full verification passed (`cabal build all && cabal test`).
