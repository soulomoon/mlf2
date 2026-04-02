## Summary

- Outcome: `sameLaneAliasFrameClearBoundaryExpr` remains honest only via a
  narrowed shared `TermClosure` rule.
- Root cause: the selected packet still depended on
  `preserveRetainedChildAliasBoundary`; the authoritative output was still
  being recovered by dropping the `hold` / `u` shells back to the recursive
  `k` rhs. The remaining helper was broader than the bounded packet earned
  because any identity-scheme final child that merely mentioned the source
  alias could trigger preservation.
- Fix: keep the shared alias-boundary seam, but narrow the final preserved
  child to the exact clear-boundary shape already frozen by this packet:
  an identity-boundary application whose argument still uses the source alias.
  The one-frame alias recursion stays unchanged, so the selected packet remains
  recursive without reopening route selection, fallback, or pipeline plumbing.
- Tests: tightened the selected packet assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs` to the exact two-forall recursive-arrow result on the
  authoritative entrypoints, and added a focused `TermClosure` mechanism guard
  inside the packet regression so the old broader rule fails RED.

## Commands

- RED:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- Focused green:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr preserves recursive output"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- Verification:
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md" && for f in orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md; do rg -n "2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap|rev-001|orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001" "$f"; done`
  - `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
  - `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/TermClosure.hs',
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
print('ROUND182_WRITABLE_SLICE_OK')
PY`
  - `git diff --check`
  - `cabal build all && cabal test`

## Outcome

- Focused packet reruns passed after the narrowing fix.
- The full repository gate passed with `1307 examples, 0 failures`.
