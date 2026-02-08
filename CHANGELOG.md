# Revision history for mlf2

## Unreleased

* Presolution: witness normalization is strict for malformed merge direction (`MergeDirectionInvalid`) across helper and production paths; permissive merge-direction fallback was removed entirely.
* Pipeline: `runPipelineElab` now reports the checked type (`typeCheck term`) as authoritative; reconstructed result-type paths are retained for diagnostics.
* Generalize: solved-order is runtime-authoritative in fallback reification; runtime base-path shadow reify/compare was removed after the 5/5 gate pass.
* Generalize shadow comparator: solved/base mismatch still hard-fails with `ValidationFailed` plus context payload in focused comparator helpers/tests.
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
