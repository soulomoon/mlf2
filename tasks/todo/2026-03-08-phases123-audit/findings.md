# Findings
- Current worktree passes `cabal build all && cabal test` (`992 examples, 0 failures`).
- Old fallback helper names are gone and guarded in `test/PipelineSpec.hs`, but `src/MLF/Elab/Elaborate.hs` still contains a live let-level fallback chooser (`fallbackChoiceFromVar/App/Lam`, `fallbackChoice`, `schChosen = maybe sch0 fst fallbackChoice`).
- `reifyInst` removed `allowFallbackFromTrace` / `resolveFallbackArgNodes`, but still refines via `targetArgs <|> expansionArgs`; this is narrower than the old ladder but not the single-source fail-fast shape promised by the plan.
- Planner owner resolution is strict body-root only, with direct fail-fast regressions and source guards.
- `inferInstAppArgsFromScheme` no longer has the generic fallback branch; remaining inference is structural.
- Docs/closeout overclaim completion while older notes still describe removed or residual fallback behavior.
