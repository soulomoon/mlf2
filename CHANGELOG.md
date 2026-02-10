# Revision history for mlf2

## Unreleased

* Witness normalization/translatability: enforced Fig. 15.3.4 transitive-flex guard for non-rigid `OpRaise` in `WitnessValidation`; added dedicated direct + presolution-path regressions, and updated merge-emission coverage to assert fast failure on non-translatable escaped bounded-binder raises.
* Witness matrix closure: completed Fig. 15.3.4 15-row normalization/emission matrix (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) with explicit row-labeled tests across `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs`; matrix gate (`--match R-`) and full gate (`cabal build all && cabal test`) are green.
* BUG-2026-02-06-002: completed thesis-exact upstream graft/weaken closure (witness canonicalization + Ω localization + named-bound simplification guard + let-lambda fallback harmonization); strict bug matrix and full gate are green (`cabal build all && cabal test` => `604 examples, 0 failures`).
* BUG-2026-02-08-004: fixed nested let + annotated-lambda checked-authoritative path in `MLF.Elab.Elaborate` by guarding `InstApp` against non-∀ function terms and by extending polymorphic-argument instantiation inference to typed post-instantiation function arrows; dedicated `PipelineSpec` sentinel now asserts thesis-green `Int` for unchecked + checked pipeline.
* Witness normalization: condition-(5) delayed-weaken ordering now reports a dedicated `DelayedWeakenViolation` instead of overloading `OpUnderRigid`; added focused witness-spec regressions for delayed-weaken violation and delayed graft/weaken coalescing behavior.
* BUG-2026-02-06-002: retained C18/C21/C21.1 elaboration-path fixes (`Phi.Omega` delayed graft/weaken handling + `Elaborate` let-scheme/app-inst repair), graduated the 4-case sentinel matrix to strict assertions, and brought the full `BUG-2026-02-06-002` matcher to green (`10 examples, 0 failures`).
* Elaboration/typecheck: fixed H15 lambda-parameter source selection in `MLF.Elab.Elaborate` so unannotated nested lambdas no longer leak solved-node names (e.g. `t23`) into let-RHS types; added `PipelineSpec` regression `does not leak solved-node names in make let mismatch`.
* Planning/docs: selected Option 1 (upstream witness-shape correction) for `BUG-2026-02-06-002`, added design/execution plans under `docs/plans/2026-02-09-*witness-shape-correction*`, and kept the 4-case bug sentinel matrix explicitly pending until strict closure tests are green.
* Binding/docs: documented A7 Group 1 shared-helper consolidation; duplicated binding path/node-ref/scope/children helpers are canonicalized in `MLF.Binding.Path`, `MLF.Binding.NodeRefs`, `MLF.Binding.ScopeGraph`, and `MLF.Binding.Children`, with migrations in `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, `MLF.Binding.Canonicalization`, `MLF.Constraint.BindingUtil`, and `MLF.Constraint.Presolution.Base`.
* Tests: finalized A7 shared harness dedup by centralizing shared pipeline test helpers in `test/SpecUtil.hs` and removing duplicate helper paths across `PipelineSpec`, `ElaborationSpec`, and `ConstraintGenSpec` without behavior changes.
* Tests/docs: `PipelineSpec` rewrite regression now exercises both `applyRedirectsToAnn` and `canonicalizeAnn` so every node occurrence (including `ALet` scheme roots) is rewritten, and `docs/plans/2026-02-08-a7-group-2-frontend-elab-abstractions-implementation-plan.md` now lists a single Hspec filter that hits Pipeline/Phase 1/Phase 6 examples.
* Frontend syntax/pretty: consolidated raw and normalized frontend type syntax into indexed `SrcTy` (`SrcNorm`, `SrcTopVar`, `SrcBound`) with compatibility aliases (`SrcType`, `NormSrcType`, `StructBound`), and generalized pretty entry points to staged types (`prettyEmlfType :: SrcTy n v -> String`, `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`).
* Frontend/elab abstractions: deduplicated frontend scope-wiring in `ConstraintGen.Translate` via local helpers (`withScopedBuild`, `attachUnder`, `rebindScopeRoot`) and centralized AnnExpr node traversal in `MLF.Elab.Run.Annotation.mapAnnNodes` (reused by redirect/canonicalization/debug origin utilities).
* Frontend/pipeline: introduced staged raw vs normalized frontend types and parser entrypoints (`parseRaw*`/`parseNorm*`), and made desugaring/constraint generation/pipeline entrypoints normalized-input only.
* Frontend/normalization: removed the reachable `normalizeBound` runtime crash path by reporting `NonStructuralBoundInStructContext`, and completed the parser clean break by removing legacy `parseEmlf*` compatibility aliases.
* Presolution/elaboration: RaiseMerge gating now uses live structural graph state (no binder-bound snapshots), preserving witness normalization/translatability invariants while restoring bounded aliasing baseline elaboration (`∀a. a -> a -> a`) in both checked and unchecked pipelines.
* Presolution: witness normalization is strict for malformed merge direction (`MergeDirectionInvalid`) across helper and production paths; permissive merge-direction fallback was removed entirely.
* Elaboration/generalization: fixed a Phase 6 `MissingNode` crash by guarding base-constraint reification against stale `solvedToBasePref` targets in `reifyWithGaBase`; when the base node is absent, elaboration now falls back to solved-order reification.
* Tests: added `BUG-2026-02-06-001` regression coverage to ensure nested let + annotated-lambda application no longer fails in Phase 6.
* Pipeline: `runPipelineElab` now reports the checked type (`typeCheck term`) as authoritative; reconstructed result-type paths are retained for diagnostics.
* Generalize: solved-order is runtime-authoritative in fallback reification; runtime base-path shadow reify/compare was removed after the 5/5 gate pass.
* Generalize shadow comparator: solved/base mismatch still hard-fails with `ValidationFailed` plus context payload in focused comparator helpers/tests.
* Pipeline: when root generalization yields no binders but the elaborated term remains type-open, top-level closure now quantifies checked free type variables before final type checking.
* Elaboration: shared closure logic now alpha-freshens wrapper binders against nested `ETyAbs` binders and rewrites free type-variable occurrences in term-level types/instantiations to avoid capture.
* Annotation elaboration: annotation-bound alignment now prefers generalized annotation bounds when shaping `InstInside (InstBot ...)` updates.
* Tests: strict checked-authoritative baselines were updated (including top-level `ETyAbs` wrappers and authoritative `Bool` result cases), with bounded aliasing Merge/RaiseMerge still tracked as a known expected-failure gap.
* Frontend: added eMLF parser + pretty-printer (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`) and public API entry points (`parseRawEmlfExpr`, `parseRawEmlfType`, `parseNormEmlfExpr`, `parseNormEmlfType`, `prettyEmlfExpr`, `prettyEmlfType`).
* xMLF syntax: added dedicated parser/pretty modules (`MLF.XMLF.Syntax`, `MLF.XMLF.Parse`, `MLF.XMLF.Pretty`) and new public module `MLF.XMLF`.
* Pretty migration: `MLF.Elab.Types.Pretty` now renders through canonical xMLF syntax printers (Unicode-first, canonical computation forms such as `ε`, `⊲σ`, `α⊳`), with tests updated accordingly.
* Docs/tests: added `docs/syntax.md` as syntax source of truth and added parser/pretty coverage specs (`FrontendParseSpec`, `FrontendPrettySpec`, `XMLFParseSpec`, `XMLFPrettySpec`).

## 0.2.0.0 -- 2026-02-02

* Breaking: pipeline entry points now return `PipelineError` and provide config-aware variants (`runPipelineElabWithConfig`, `runPipelineElabCheckedWithConfig`).
* Breaking: tracing is fully explicit (no global trace config); `PipelineConfig`/`TraceConfig` are part of the public API.
* Breaking: constraint types are split into `MLF.Constraint.Types.Graph`, `MLF.Constraint.Types.Witness`, and `MLF.Constraint.Types.Presolution` (imports updated accordingly).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
