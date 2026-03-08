# Findings

- Task initialized on 2026-03-09.
- `src/MLF/Elab/Run/ResultType/View.hs:19,127-128` still exports and defines `rtvSchemeBodyTarget`, but repo search finds no callers outside that file.
- `src/MLF/Elab/Run/ResultType/View.hs:42` imports `MLF.Elab.Run.Scope (schemeBodyTarget)` only for the dead forwarding wrapper, so removing the wrapper also removes that coupling.
- Live result-type call sites already use `schemeBodyTarget` directly over `View.rtvPresolutionViewOverlay`; see `src/MLF/Elab/Run/ResultType/Ann.hs` and `src/MLF/Elab/Run/ResultType/Fallback.hs`.
- `implementation_notes.md:57-65`, `CHANGELOG.md:22-26`, and `TODO.md:207-219` already describe `MLF.Elab.Run.Scope` as the single owner of `schemeBodyTarget` semantics and future priority.
- `test/PipelineSpec.hs:263-289,335-337` has source guards for the row-2 boundary and for `Elaborate`, but no guard yet against `rtvSchemeBodyTarget` in `ResultType.View`.
- The thesis explicitly distinguishes `S′` (general subterm translation) from `S` (needed for the named nodes themselves) in `papers/these-finale-english.txt:13561-13563`; `src/MLF/Elab/Run/Scope.hs:117-123,137-169` already keeps that distinction in `schemeBodyTarget` vs `generalizeTargetNode`.
- `MLF.Elab.Run.ResultType.View` is an internal Cabal module (`mlf2.cabal:206` under `other-modules`), so wrapper removal is bounded to internal imports; repo search found none.
