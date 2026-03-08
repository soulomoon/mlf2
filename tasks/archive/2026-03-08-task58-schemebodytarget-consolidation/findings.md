# Findings

- `MLF.Elab.Elaborate.schemeBodyTarget` is a smaller helper: canonicalize target, then map `TyVar` bound -> bound/body and `TyForall` -> body.
- `MLF.Elab.Run.Scope.schemeBodyTarget` adds scheme-root awareness using `gnSchemes` and a `schemeRootByBody` map before deciding whether a `TyVar` target should advance to the bound/body.
- Existing users outside `Elaborate` already import the `Run.Scope` version.
- The task is behavior-sensitive because the TODO explicitly requires adopting the richer semantics only if elaboration behavior stays stable.
- The richer `schemeBodyTarget` is thesis-faithful for `S′`-style subterm translation, but nested-let / alias regressions showed that `Elaborate.generalizeAtNode` still needs `S`-style bound descent when generalizing the named node itself.
- Final shape: keep both target selectors in `MLF.Elab.Run.Scope` (`schemeBodyTarget` for `S′`, `generalizeTargetNode` for `S`) and remove the duplicate local helper from `MLF.Elab.Elaborate`.
