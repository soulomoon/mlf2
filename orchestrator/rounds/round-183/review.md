# Round 183 Review

## Scope and diff basis

- Round: `round-183`
- Item: `item-5`
- Base branch from `orchestrator/state.json`: `codex/automatic-recursive-type-inference`
- `git merge-base HEAD codex/automatic-recursive-type-inference` returned `307fe940f4db8c075e03564944d035aaa9714b6a`
- `git diff --name-status --find-renames codex/automatic-recursive-type-inference...HEAD` was empty, so the reviewed round delta is the current worktree diff on top of base commit `307fe94`
- Implementation paths under review:
  - `test/PipelineSpec.hs`
  - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  - `orchestrator/rounds/round-183/implementation-notes.md`
- Pre-existing out-of-scope modification preserved as instructed: `orchestrator/state.json`

## Commands run

- `python3 -m json.tool orchestrator/state.json >/dev/null` -> exit `0`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"` -> exit `0`
- `python3 - <<'PY'
import json, pathlib, sys
root = pathlib.Path('.')
state = json.loads((root / 'orchestrator/state.json').read_text())
expected = {
    'roadmap_id': state['roadmap_id'],
    'roadmap_revision': state['roadmap_revision'],
    'roadmap_dir': state['roadmap_dir'],
}
for rel in [
    'orchestrator/roadmap.md',
    'orchestrator/verification.md',
    'orchestrator/retry-subloop.md',
    'orchestrator/rounds/round-183/selection.md',
]:
    text = (root / rel).read_text()
    for key, value in expected.items():
        if value not in text:
            print(f'{rel}: missing {key}={value}')
            sys.exit(1)
print('ROADMAP_POINTERS_OK')
PY` -> exit `0` (`ROADMAP_POINTERS_OK`)
- `git diff --name-only -- orchestrator/roadmaps` -> exit `0` (no output)
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"` -> exit `0`
- `git merge-base HEAD codex/automatic-recursive-type-inference && git diff --name-status --find-renames codex/automatic-recursive-type-inference...HEAD && git diff --name-status --find-renames` -> exit `0`
- `git diff -- test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs orchestrator/rounds/round-183/implementation-notes.md` -> exit `0`
- `git diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs` -> exit `0` (no output)
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/PipelineSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'orchestrator/rounds/round-183/implementation-notes.md',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-183', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-183', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [
    p for p in paths
    if p not in allowed
    and p not in {
        'orchestrator/rounds/round-183/plan.md',
        'orchestrator/rounds/round-183/selection.md',
    }
]
forbidden = [p for p in paths if p == 'mlf2.cabal']
if extra or forbidden:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND183_WRITABLE_SLICE_OK')
PY` -> exit `0` (`ROUND183_WRITABLE_SLICE_OK`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr preserves recursive output"'` -> exit `0` (`2 examples, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr double-alias clear-boundary packet preserves recursive output on both authoritative entrypoints"'` -> exit `0` (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'` -> exit `0` (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr preserves recursive output"'` -> exit `0` (`2 examples, 0 failures`)
- `git diff --check` -> exit `0`
- `cabal build all && cabal test` -> exit `0` (`1307 examples, 0 failures`)
- `python3 -m json.tool orchestrator/rounds/round-183/review-record.json >/dev/null` -> exit `0`
- `python3 - <<'PY'
import json, pathlib, sys
root = pathlib.Path('.')
state = json.loads((root / 'orchestrator/state.json').read_text())
record = json.loads((root / 'orchestrator/rounds/round-183/review-record.json').read_text())
for key in ('roadmap_id', 'roadmap_revision', 'roadmap_dir'):
    if record[key] != state[key]:
        print(f'mismatch {key}: {record[key]!r} != {state[key]!r}')
        sys.exit(1)
if record['roadmap_item_id'] != 'item-5':
    print('mismatch roadmap_item_id')
    sys.exit(1)
print('REVIEW_RECORD_IDENTITY_OK')
PY` -> exit `0` (`REVIEW_RECORD_IDENTITY_OK`)

## Pass/fail results

- Baseline 1 `PASS`: `orchestrator/state.json` resolves the active roadmap bundle as `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`; `orchestrator/roadmap.md`, `orchestrator/verification.md`, `orchestrator/retry-subloop.md`, `orchestrator/rounds/round-183/selection.md`, and the newly written `orchestrator/rounds/round-183/review-record.json` all carry the same roadmap identity; `git diff --name-only -- orchestrator/roadmaps` was empty, so prior roadmap families/revisions remain unchanged.
- Baseline 2 `PASS`: `git diff --check` returned clean.
- Baseline 3 `PASS`: the active `roadmap.md` still includes `Item id:`, `Depends on:`, `Parallel safe:`, `Parallel group:`, and `Merge after:` for each roadmap item.
- Baseline 4 `PASS`: the round touches test files, so the full gate applied; `cabal build all && cabal test` passed with `1307 examples, 0 failures`.
- Baseline 5 `N/A`: no thesis-facing files changed.
- Baseline 6 `N/A`: `worker_mode` is `none`; no planner-authored fan-out or `worker-plan.json` is involved.
- Item-5 writable-slice check `PASS`: the implementation diff stayed inside the item-authorized surfaces. No production file changed; the only implementation edits are the selected research/integration tests plus `implementation-notes.md`.
- Item-5 evidence honesty `PASS`: the double-alias research probe now uses the same exact authoritative-output matcher already used by the adjacent one-alias packet, and the integration test now requires both authoritative entrypoints to agree on the exact two-`forall` recursive arrow while also asserting the bounded `TermClosure` mechanism shape. Focused reruns for the selected packet and adjacent one-alias controls all passed.
- Item-5 full-gate requirement `PASS`: the required `cabal build all && cabal test` run passed.
- Item-5 aggregate-read boundary `PASS`: `implementation-notes.md` records only the packet-bounded conclusion `sameLaneDoubleAliasFrameClearBoundaryExpr remains honest only via an exact one-extra-alias-shell TermClosure rule`; it does not upgrade this into general `P3`/`P4`/`P6` support or repo-level readiness.

## Plan comparison

1. `Step 1` investigation/localization: satisfied.
   - The shared authoritative preservation path is still routed through `preserveRetainedChildAuthoritativeResult` in `src/MLF/Elab/Run/Pipeline.hs` and the retained-child guard cluster remains unchanged in `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.
   - `src/MLF/Elab/TermClosure.hs` still carries the bounded one-extra-alias-shell rule at `preserveRetainedChildAliasBoundary` / `hasRetainedChildAliasBoundary`.
   - No packet-local rescue or out-of-slice fallback edit was introduced.
2. `Step 2` focused test tightening: satisfied.
   - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` switched the selected double-alias packet from the old generic `containsMu` success check to the exact authoritative-output matcher.
   - `test/PipelineSpec.hs` replaced the old generic `containsMu` read with exact authoritative-entrypoint checks and a focused source/mechanism guard for the bounded alias-frame rule.
3. `Step 3` production correction: satisfied.
   - No production correction was needed and no production file changed, which matches the plan branch for stale tests / already-correct bounded rule.
4. `Step 4` re-green and bounded conclusion: satisfied.
   - Focused selected-packet reruns and adjacent one-alias control reruns all passed.
   - `implementation-notes.md` records one bounded packet result only.
5. `Step 5` verification gates: satisfied.
   - Writable-slice check, focused reruns, `git diff --check`, and the full `cabal build all && cabal test` gate all passed.

## Evidence summary

- The implementation diff is test-only plus round-local notes; no change was made to `src/MLF/Elab/TermClosure.hs`, `src/MLF/Elab/Run/Pipeline.hs`, or fallback-core files.
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` now demands the exact authoritative shape for the selected double-alias packet: two leading unbounded `forall`s and the recursive-arrow body.
- `test/PipelineSpec.hs` now checks both authoritative entrypoints, requires their elaborated terms to type-check, and ties the packet to the bounded `TermClosure` mechanism by asserting:
  - initial alias-depth budget `1`
  - no depth-`2` entrypoint
  - recursive decrement through alias frames
  - preserved round-182 clear-boundary / identity-scheme / trivial-retained-child shape
- The focused reruns and the full suite confirm no regression in the adjacent one-alias control or the broader test suite.

## Decision

**APPROVED**
