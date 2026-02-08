# Revision history for mlf2

## Unreleased

* Frontend/pipeline: introduced staged raw vs normalized frontend types and parser entrypoints (`parseRaw*`/`parseNorm*`), and made desugaring/constraint generation/pipeline entrypoints normalized-input only.
* Presolution/elaboration: RaiseMerge gating now uses live structural graph state (no binder-bound snapshots), preserving witness normalization/translatability invariants while restoring bounded aliasing baseline elaboration (`∀a. a -> a -> a`) in both checked and unchecked pipelines.
* Presolution: witness normalization is strict for malformed merge direction (`MergeDirectionInvalid`) across helper and production paths; permissive merge-direction fallback was removed entirely.
* Elaboration/generalization: fixed a Phase 6 `MissingNode` crash by guarding base-constraint reification against stale `solvedToBasePref` targets in `reifyWithGaBase`; when the base node is absent, elaboration now falls back to solved-order reification.
* Tests: added `BUG-2026-02-06-001` regression coverage to ensure nested let + annotated-lambda application no longer fails in Phase 6.
* Pipeline: `runPipelineElab` now reports the checked type (`typeCheck term`) as authoritative; reconstructed result-type paths are retained for diagnostics.
* Pipeline: when root generalization yields no binders but the elaborated term remains type-open, top-level closure now quantifies checked free type variables before final type checking.
* Elaboration: shared closure logic now alpha-freshens wrapper binders against nested `ETyAbs` binders and rewrites free type-variable occurrences in term-level types/instantiations to avoid capture.
* Annotation elaboration: annotation-bound alignment now prefers generalized annotation bounds when shaping `InstInside (InstBot ...)` updates.
* Tests: strict checked-authoritative baselines were updated (including top-level `ETyAbs` wrappers and authoritative `Bool` result cases), with bounded aliasing Merge/RaiseMerge still tracked as a known expected-failure gap.
* Frontend: added eMLF parser + pretty-printer (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`) and public API entry points (`parseEmlfExpr`, `parseEmlfType`, `prettyEmlfExpr`, `prettyEmlfType`).
* xMLF syntax: added dedicated parser/pretty modules (`MLF.XMLF.Syntax`, `MLF.XMLF.Parse`, `MLF.XMLF.Pretty`) and new public module `MLF.XMLF`.
* Pretty migration: `MLF.Elab.Types.Pretty` now renders through canonical xMLF syntax printers (Unicode-first, canonical computation forms such as `ε`, `⊲σ`, `α⊳`), with tests updated accordingly.
* Docs/tests: added `docs/syntax.md` as syntax source of truth and added parser/pretty coverage specs (`FrontendParseSpec`, `FrontendPrettySpec`, `XMLFParseSpec`, `XMLFPrettySpec`).

## 0.2.0.0 -- 2026-02-02

* Breaking: pipeline entry points now return `PipelineError` and provide config-aware variants (`runPipelineElabWithConfig`, `runPipelineElabCheckedWithConfig`).
* Breaking: tracing is fully explicit (no global trace config); `PipelineConfig`/`TraceConfig` are part of the public API.
* Breaking: constraint types are split into `MLF.Constraint.Types.Graph`, `MLF.Constraint.Types.Witness`, and `MLF.Constraint.Types.Presolution` (imports updated accordingly).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
