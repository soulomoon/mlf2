# Round 184 Implementation Notes

## Summary

- Reproduced the selected baseline in `cabal repl mlf2-test`: `sameLaneTripleAliasFrameClearBoundaryExpr`
  failed on both authoritative entrypoints with
  `PipelineTypeCheckError (TCLetTypeMismatch ...)`, while the settled
  one-alias and double-alias controls still preserved recursive output.
- Added focused triple-alias coverage in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`, verified the new packet failed first, then kept the
  predecessor alias and double-alias assertions read-only.
- Kept the authoritative preservation entrypoint bounded at
  `hasRetainedChildAliasBoundary v body 1 =` and fixed the exact selected debt
  in `src/MLF/Elab/TermClosure.hs` by admitting one exhausted-budget
  same-lane alias shell only when it immediately leads to the existing
  clear-boundary retained-child shape.
- Honest bounded result: `sameLaneTripleAliasFrameClearBoundaryExpr` remains
  honest only via an exact two-extra-alias-shell `TermClosure` rule.

## Verification

- `cabal repl mlf2-test <<'EOF' ... sameLaneTripleAliasFrameClearBoundaryExpr ... EOF`
  before edits: triple-alias packet failed on `runPipelineElab` and
  `runPipelineElabChecked`; alias and double-alias controls still succeeded.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
  after test edits: failed red with the expected triple-alias mismatch and
  missing exact-depth guard; after the `TermClosure` fix: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
  passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
  passed.
- `python3 -m json.tool orchestrator/state.json >/dev/null` passed.
- roadmap pointer and metadata `rg` checks passed.
- writable-slice guard script reported `ROUND184_WRITABLE_SLICE_OK`.
- `git diff --check` passed.
- `cabal build all && cabal test` passed with `1311 examples, 0 failures`.
