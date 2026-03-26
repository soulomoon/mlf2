# Round 102 Implementation Notes

- Implemented the bounded item-4 representative settlement campaign at
  `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`.
- Row inventory stayed exactly:
  `P1-row`, `C1`, `C2`, `C3`, `C4`, `C5`, `C6`, and `C7`.
  `C5` and `C7` still reuse the same exact `C2` same-lane packet only.
- Exact commands rerun this round:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - the roadmap-bundle and predecessor-presence checks
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects ambiguous repeated graft-weaken on the same non-front binder"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - `cabal repl mlf2-test --repl-options=-ignore-dot-ghci` with
    `:module + *PipelineSpec` and the helper block mirrored from
    `test/PipelineSpec.hs:1495-1597` to print the exact same-lane replay
    outputs
- Observed bounded surface facts:
  - `P1-row`: bounded local same-lane recursive support still exists, but the
    corresponding unannotated variant still does not infer recursive shape.
  - `C1`: the selected non-local scheme-alias / base-like packet still stays
    on `baseTarget -> baseC -> targetC`, and the accepted visible-output fact
    remains `TBase (BaseTy "Int")` with `containsMu False`.
  - `C2` / `C5` / `C7`: exact same-lane replay still prints helper-visible
    internal
    `TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))`,
    then `True`, then
    `Right (TForall "a" Nothing (TVar "a"))` on both authoritative public
    entrypoints.
  - `C3`: quantified crossing still fails closed on the same retained-child
    search.
  - `C4`: ambiguity still fails closed on the current elaboration surface.
  - `C6`: bounded rejection still contains termination / forbidden-growth
    pressure inside the inherited acyclic model, with `N4` remaining boundary
    context only.
- Final per-row classifications:
  - `P1-row` = `admitted but not reconstruction-visible / blocker debt`
  - `C1` = `admitted but not reconstruction-visible / blocker debt`
  - `C2` = `admitted but not reconstruction-visible / blocker debt`
  - `C3` = `fail-closed rejection`
  - `C4` = `fail-closed rejection`
  - `C5` = `admitted but not reconstruction-visible / blocker debt`
  - `C6` = `fail-closed rejection`
  - `C7` = `admitted but not reconstruction-visible / blocker debt`
- Refreshed item-4 matrix tally:
  zero `stable visible persistence` rows;
  five blocker-debt rows (`P1-row`, `C1`, `C2`, `C5`, `C7`);
  three fail-closed rows (`C3`, `C4`, `C6`).
- Bounded settlement read:
  the matrix still reads `bounded subset only`;
  `P5` remains reject-side only;
  `N1`, `N2`, and `N6` remain bounded fail-closed discipline; and
  `N4` remains pressure context for item `5`, not a reopen result in this
  round.
- Scope stayed bounded:
  no production code, test code, `mlf2.cabal`, `Bugs.md`, roadmap contracts,
  or controller-owned machine state were edited.
  `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/state.json`
  still reports only the preexisting `orchestrator/state.json` modification.
