# Round 100 Implementation Notes

- This round implements only roadmap item `2` and adds the canonical
  settlement-evidence artifact at
  `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`.
- The exact frozen rows remain:
  `C1` on `baseTarget -> baseC -> targetC`,
  `C2` on `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  and `C5` as the same exact `C2` packet viewed through the binder-sensitive /
  owner-sensitive placement lens, not as a second packet.
- Fresh commands rerun in this round:
  `git diff --check`,
  `python3 -m json.tool orchestrator/state.json >/dev/null`,
  the roadmap-bundle and predecessor-presence checks,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`,
  `cabal repl mlf2-test --repl-options=-ignore-dot-ghci` with
  `:module + *PipelineSpec` and the exact same-lane replay harness,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`,
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`,
  and
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`.
- Observed bounded surface facts:
  `C1` still keeps the explicit `rootNonLocalSchemeAliasBaseLike` proof
  separate from the preserved local lanes, and the selected non-local packet
  still reads `TBase (BaseTy "Int")` with `containsMu False`;
  `C2` replay still prints
  `TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))`,
  then `True`,
  then `Right (TForall "a" Nothing (TVar "a"))` on both authoritative public
  entrypoints;
  `C5` still reuses that same exact `C2` pocket, keeps lookup bounded to the
  same local `TypeRef` lane, and keeps the nested-`forall` / frame-drift
  contrast fail-closed rather than admitting a second owner-local packet.
- Final case-by-case classifications remain:
  `C1` = `admitted but not reconstruction-visible / blocker debt`;
  `C2` = `admitted but not reconstruction-visible / blocker debt`;
  `C5` = `admitted but not reconstruction-visible / blocker debt`.
- This sharpens only the current `P2` / `P3` / `P4` settlement read.
  Item `3`, item `4`, item `5`, and any code or test edits remain later work
  only. No production code, test code, `mlf2.cabal`, `Bugs.md`, roadmap
  contracts, or controller-owned machine state changed in this round.
