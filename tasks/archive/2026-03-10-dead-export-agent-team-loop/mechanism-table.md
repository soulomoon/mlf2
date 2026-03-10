# Goal Mechanism Table (Current vs Target)

Last updated (UTC): 2026-03-10
Goal: Resolve the dead-export shortlist one mechanism at a time, removing genuinely dead exports and explicitly retaining only those revalidated as intentional.
Source of truth: `tasks/archive/2026-03-10-dead-export-sweep/findings.md`, `AGENTS.md`, live code/tests/docs

| Mechanism | Current codebase behavior | Target behavior | Gap summary | Evidence (spec/code/tests) | Gate (YES/NO) | Next action |
|---|---|---|---|---|---|---|
| `ChiQuery` bind-parent passthroughs | `chiLookupBindParent` and `chiBindParents` are retired from `MLF.Elab.Run.ChiQuery`; the new chi-first guard keeps them absent | Keep the dead passthrough surface retired | none | verifier search; `src/MLF/Elab/Run/ChiQuery.hs`; `test/PipelineSpec.hs`; `cabal build all && cabal test` | YES | Move to the next NO row |
| `Binding.Validation` root helper export | `validateSingleGenRoot` is now local-only inside `MLF.Binding.Validation`; the new binding guard keeps it out of the export list | Keep the helper local and the export surface narrowed | none | verifier search; `src/MLF/Binding/Validation.hs`; `test/BindingSpec.hs`; `cabal build all && cabal test` | YES | Move to the next NO row |
| `Canonicalizer` ref helper | `canonicalizeRef` is retired from `MLF.Constraint.Canonicalizer`; the new canonicalizer guard keeps it absent | Keep the dead export retired | none | verifier recheck; `src/MLF/Constraint/Canonicalizer.hs`; `test/CanonicalizerSpec.hs`; `cabal build all && cabal test` | YES | Move to the next NO row |
| `Debug` edge origin helper | `edgeOrigins` is retired from `MLF.Elab.Run.Debug`; the new ga-scope guard keeps it absent | Keep the dead export retired | none | verifier recheck; `src/MLF/Elab/Run/Debug.hs`; `test/PipelineSpec.hs`; `cabal build all && cabal test` | YES | Move to the final NO row |
| `TermClosure` full close helper | `closeTermWithSchemeSubst` is retired from `MLF.Elab.TermClosure`; the new Phase 6 guard keeps it absent | Keep the dead export retired | none | verifier recheck; `src/MLF/Elab/TermClosure.hs`; `test/PipelineSpec.hs`; `cabal build all && cabal test` | YES | Campaign complete |
