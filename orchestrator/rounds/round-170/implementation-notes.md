## Summary

- Outcome: narrow success.
- Root cause: `preserveRetainedChildAuthoritativeResult` only preserved the trivial retained-child shell `ELet v sch rhs (EVar v)`. The frozen packet adds one same-lane alias-frame binder (`hold`) ahead of the trivial retained child, so closure left the authoritative recursive rhs behind and both authoritative entrypoints reached `PipelineTypeCheckError (TCLetTypeMismatch ...)`.
- Fix: extend the authoritative preservation path in `src/MLF/Elab/TermClosure.hs` just enough to descend through top-level `ETyAbs` binders and preserve across one alias-frame boundary when the surrounding let currently fails with `TCLetTypeMismatch` and the alias rhs type is recursive.
- Tests: added one focused RED regression in `test/PipelineSpec.hs`, then tightened the frozen research probe in `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` to assert recursive success on both authoritative entrypoints.

## Commands

- RED confirmation:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- Targeted post-fix checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "alias-frame clear-boundary packet"'`
- Item-2 verification gates:
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
  - `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/Run/Pipeline.hs',
    'src/MLF/Elab/TermClosure.hs',
    'src/MLF/Elab/Run/Scope.hs',
    'src/MLF/Elab/Run/ResultType/Fallback.hs',
    'test/PipelineSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
}
out = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'mlf2.cabal'],
    text=True,
).splitlines()
extra = [path for path in out if path and path not in allowed]
if extra:
    print('\n'.join(extra))
    sys.exit(1)
print('ITEM2_WRITABLE_SLICE_OK')
PY`
  - `git diff --check`
  - `cabal build all && cabal test`

## Outcome

- RED failed for the intended reason before the fix: both authoritative entrypoints surfaced the inherited `TCLetTypeMismatch` blocker on the packet.
- After the fix, the focused regression and the frozen research probe both pass and the full `cabal build all && cabal test` gate passes.
