# Round 174 Plan

- Round: `round-174`
- Roadmap: `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap` / `rev-001`
- Item: `item-2`
- Retry:
  - stage_id: `item-2`
  - attempt: `2`
  - latest_attempt_verdict: `rejected`
  - latest_stage_action: `retry`
- Execution shape: serial, systematic-debugging-first, TDD-first

## Objective

Keep the round on the frozen packet
`sameLaneDoubleAliasFrameClearBoundaryExpr` and correct the attempt-1
scope-widening finding without changing the writable slice:

- `src/MLF/Elab/TermClosure.hs`
- `test/PipelineSpec.hs`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`

Attempt 1 already established packet-bounded focused coverage and a green full
gate. Attempt 2 therefore does not add broader subjects or new packet families.
It must narrow the production change so the retained-child alias helper admits
exactly one extra same-lane alias shell beyond the already accepted one-alias
packet, preserve the existing double-alias focused tests as the exact evidence
surface, refresh `implementation-notes.md`, and rerun the focused and full
verification commands.

## Retry Driver

- Reviewer feedback from `reviews/attempt-1.md` is binding:
  `hasRetainedChildAliasBoundary v body 2` is too broad at the current
  boundary.
- The truthful bounded target is the current `hold` boundary in
  `sameLaneDoubleAliasFrameClearBoundaryExpr`, where the frozen packet needs
  one extra alias shell (`keep`) before the final identity child and no more.
- Do not widen into triple-alias coverage, generalized alias-depth support,
  fallback changes, or any new writable paths.

## Sequential Plan

1. Reconfirm the reviewer finding against the current round diff; modify no
   files in this step.
   - Inspect `src/MLF/Elab/TermClosure.hs` together with
     `reviews/attempt-1.md` and restate the exact scope issue:
     the helper depth at the current retained-child boundary is one step too
     permissive for item-2.
   - Treat the existing packet tests in `test/PipelineSpec.hs` and
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` as the
     already-landed exact evidence surface; do not add triple-alias or broader
     probes.

2. Keep the focused packet tests unchanged unless a wording-only refresh is
   necessary for honesty.
   - Preserve the existing packet-bounded examples for
     `sameLaneDoubleAliasFrameClearBoundaryExpr` in
     `test/PipelineSpec.hs` and
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
   - Reuse those exact focused commands as the verification surface for retry
     attempt 2 instead of widening the subject.

3. Apply the smallest lawful production correction only inside
   `src/MLF/Elab/TermClosure.hs`.
   - Narrow `preserveRetainedChildAliasBoundary` so the current boundary admits
     exactly one additional same-lane alias-frame shell before the final
     identity child.
   - Preserve the accepted one-alias packet and the frozen double-alias packet,
     but do not authorize any deeper alias chain than item-2 earns.
   - Keep the change structural and packet-bounded; do not turn it into
     generalized alias-depth support, let-normalization, fallback widening, or
     broader routing work.

4. Refresh the round notes honestly after the bounded correction.
   - Update `orchestrator/rounds/round-174/implementation-notes.md` so the
     root cause and fix description explicitly state that attempt 2 tightened
     the helper to one extra alias shell at the current boundary.
   - Keep the result token to one bounded item-2 outcome only and avoid any
     broader readiness claim.

5. Rerun the exact verification surface after the narrowing fix.
   - Replay the focused packet commands for
     `sameLaneDoubleAliasFrameClearBoundaryExpr` and
     `double-alias clear-boundary packet`.
   - Recheck the writable-slice guard and `git diff --check`.
   - Rerun `cabal build all && cabal test` because code/test files remain in
     scope.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "double-alias clear-boundary packet"'`
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
print('ITEM174_WRITABLE_SLICE_OK')
PY`
- `git diff --check`
- `cabal build all && cabal test`
