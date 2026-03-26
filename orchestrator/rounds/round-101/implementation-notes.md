# Round 101 Implementation Notes

- Implemented the bounded item-3 docs-only settlement slice at
  `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
  for the two frozen rows only: `C3` nested-`forall` pressure and `C7`
  same-lane output-surface continuity.
- `C3` stayed tied to the same owner-local retained-child search and the same
  clear-boundary candidate route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
  Fresh source/test reruns kept the quantified-boundary guard unchanged:
  `boundHasForallFrom` still detects the crossing, candidate selection still
  requires `not hasForall`, and the same wrapper still fails closed once it
  crosses a nested `forall` boundary. Final classification:
  `fail-closed rejection`.
- `C7` stayed tied to the same exact same-lane retained-child pocket already
  accepted for `C2` / `C5`. Fresh exact-pocket replay through
  `cabal repl mlf2-test` with `:module + *PipelineSpec` re-confirmed:
  helper-visible/internal fallback result
  `TArrow (TVar "t32") (TMu "t38" ...)`,
  `containsMu True`,
  and both authoritative public outputs
  `Right (TForall "a" Nothing (TVar "a"))`.
  `Pipeline.hs` still keeps `checkedAuthoritative = typeCheck termClosed` as
  the authoritative return path while fallback reconstruction remains
  diagnostics-only. Final classification:
  `admitted but not reconstruction-visible / blocker debt`.
- Exact commands rerun this round:
  - `cabal repl mlf2-test` with `:module + *PipelineSpec` and the exact
    same-lane replay harness logic already anchored in
    `test/PipelineSpec.hs:1500-1569`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  - predecessor-presence checks for the item-1 artifact, accepted item-2
    artifact, same-lane predecessor docs, and accepted same-lane review
    records
  - `rg -n 'boundVarTargetRoot =|boundHasForallFrom start0 =|sameLaneLocalRetainedChildTarget =|keepTargetFinal =|case sameLaneLocalRetainedChildTarget of|not hasForall' src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `rg -n 'checkedAuthoritative =|typeCheck termClosed|computeResultTypeFallback resultTypeInputs' src/MLF/Elab/Run/Pipeline.hs`
- Scope stayed bounded: no production code, test code, `mlf2.cabal`,
  or `Bugs.md` edits were introduced by this round. The worktree still carries
  the preexisting controller-owned `orchestrator/state.json` modification
  noted by selection, and it was left untouched.
