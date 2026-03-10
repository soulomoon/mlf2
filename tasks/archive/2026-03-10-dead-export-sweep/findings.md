# Findings

- High-confidence stale export candidate: `MLF.Elab.Run.ChiQuery` still exports `chiLookupBindParent` and `chiBindParents`, but repo-wide source search found no live call sites outside `/Volumes/src/mlf4/src/MLF/Elab/Run/ChiQuery.hs`.
- High-confidence stale export candidate: `MLF.Binding.Validation` still exports `validateSingleGenRoot` under its “Internal helpers (for Tree.hs)” section, but `/Volumes/src/mlf4/src/MLF/Binding/Tree.hs` no longer imports it and the only remaining uses are internal to `Validation.hs` itself.
- Medium-confidence stale export candidate: `MLF.Constraint.Canonicalizer` exports `canonicalizeRef`, but repo-wide source search found no production/test call sites outside `/Volumes/src/mlf4/src/MLF/Constraint/Canonicalizer.hs`.
- Medium-confidence stale export candidate: `MLF.Elab.Run.Debug` exports `edgeOrigins`, but repo-wide source search found no production/test call sites outside `/Volumes/src/mlf4/src/MLF/Elab/Run/Debug.hs`.
- Medium-confidence stale export candidate: `MLF.Elab.TermClosure` exports `closeTermWithSchemeSubst`, while live imports only use `closeTermWithSchemeSubstIfNeeded`, `alignTermTypeVarsToScheme`, `alignTermTypeVarsToTopTyAbs`, and `substInTerm`.
- Lower-confidence / probably intentional surface: `MLF.Util.Trace.traceConfigFromEnv` is unused inside the repo, but the module docs explicitly describe it as an entry-point helper, so it does not look like the same kind of stale internal export.
- Best next bounded cleanup from this sweep: retire `chiLookupBindParent` and `chiBindParents` from `MLF.Elab.Run.ChiQuery`; they are small view pass-throughs, match the recent `chiCanonicalBindParents` surface narrowing, and appear to have zero external users.
