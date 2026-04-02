# Round 174 Review

Round: `round-174`  
Roadmap: `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap` / `rev-001`  
Item: `item-2`  
Base diff: `codex/automatic-recursive-type-inference`

## Commands Run

1. Exit `0`
```sh
python3 -m json.tool orchestrator/state.json >/dev/null && roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md" && printf 'STATE_AND_BUNDLE_OK\n' && printf 'roadmap_id=%s\nroadmap_revision=%s\nroadmap_dir=%s\n' "$(jq -r '.roadmap_id' orchestrator/state.json)" "$(jq -r '.roadmap_revision' orchestrator/state.json)" "$roadmap_dir" && printf '\nPOINTER_FILES\n' && for f in orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md; do printf '%s\n' "$f"; rg -n "2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap|rev-001|orchestrator/roadmaps/2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap/rev-001" "$f"; done && printf '\nSELECTION_IDENTITY\n' && rg -n '^- roadmap_id: |^- roadmap_revision: |^- roadmap_dir: |^- roadmap_item_id: ' orchestrator/rounds/round-174/selection.md
```
Result: `state.json` resolved the active roadmap bundle; `orchestrator/roadmap.md`, `orchestrator/verification.md`, and `orchestrator/retry-subloop.md` all pointed at the same roadmap identity; `selection.md` recorded the same roadmap identity and `roadmap_item_id: item-2`.

2. Exit `0`
```sh
roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"
```
Result: all four roadmap items still carried `Item id`, `Depends on`, `Parallel safe`, `Parallel group`, and `Merge after` metadata.

3. Exit `0`
```sh
git diff --check codex/automatic-recursive-type-inference && printf '\nNAME_STATUS\n' && git diff --name-status codex/automatic-recursive-type-inference
```
Result: no diff-hygiene failures. Name-status vs base was:
`M orchestrator/state.json`
`M src/MLF/Elab/TermClosure.hs`
`M test/PipelineSpec.hs`
`M test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`

4. Exit `0`
```sh
python3 - <<'PY'
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
PY

printf '\nDIFF_TERMCLOSURE_AND_TESTS\n'
git diff --unified=80 codex/automatic-recursive-type-inference -- src/MLF/Elab/TermClosure.hs test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs
```
Result: `ITEM174_WRITABLE_SLICE_OK`. The implementation/test diff stayed inside the frozen writable slice and added the expected double-alias regression coverage plus the `TermClosure` change.

5. Exit `0`
```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
```
Result: focused packet probe passed with `3 examples, 0 failures`.

6. Exit `0`
```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "double-alias clear-boundary packet"'
```
Result: focused pipeline regression passed with `1 example, 0 failures`.

7. Exit `0`
```sh
cabal build all && cabal test
```
Result: full repository gate passed with `1306 examples, 0 failures`.

## Check Results

- Baseline 1, roadmap identity and pointer consistency: PASS.
- Baseline 2, diff hygiene: PASS.
- Baseline 3, roadmap metadata integrity: PASS.
- Baseline 4, build and test gate for production/test changes: PASS.
- Baseline 5, thesis conformance gate: NOT APPLICABLE; no thesis-facing sources changed.
- Baseline 6, worker-plan integrity: NOT APPLICABLE; `worker_mode` is `none` and no worker plan was used.
- Item-2 writable-slice check: PASS for implementation/test files. The only extra path in the base diff is controller-owned `orchestrator/state.json`.
- Item-2 focused regression coverage: PASS. The frozen packet is covered in `test/PipelineSpec.hs` and `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, and both focused runs passed.
- Item-2 bounded result token: PASS. `implementation-notes.md` records exactly one bounded result: `narrow success`.
- Plan step 1, root-cause investigation: PASS. `implementation-notes.md` records a packet-local `TermClosure` root cause consistent with the observed production diff.
- Plan step 2, add focused RED regression before production edit: PASS on final artifact shape. The round now carries exact packet-bounded coverage for the frozen double-alias packet on both authoritative entrypoints.
- Plan step 3, smallest lawful production change: FAIL. `src/MLF/Elab/TermClosure.hs:66-86` now calls `hasRetainedChildAliasBoundary v body 2`, which permits two nested alias-frame shells between the preserved boundary and the final identity child. The round plan and `implementation-notes.md` only authorize one additional same-lane alias shell for the frozen double-alias packet.
- Plan step 4, honest packet refresh after the bounded fix: PASS. The round keeps the test surface packet-bounded and the focused/full verification evidence is honest.

## Evidence Summary

The round is close: the diff is clean, the focused double-alias tests pass, and the full repository gate stays green. The blocking issue is scope control. The new recursive helper in `TermClosure` is broader than the approved item-2 plan because the depth bound is set to admit one more alias-frame layer than the frozen packet requires, while the added tests at `test/PipelineSpec.hs:2131-2147` and `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs:27-35` only justify the exact double-alias packet.

## Decision

REJECTED: `src/MLF/Elab/TermClosure.hs` widens retained-child alias preservation beyond the one-extra-shell scope authorized by item-2, and the current focused tests do not justify that broader behavior.

## Retry Record

- Implemented stage result: `narrow success`
- Attempt verdict: `rejected`
- Stage action: `retry`
- Retry reason: `src/MLF/Elab/TermClosure.hs` allowed more than the one extra alias shell authorized for the frozen double-alias packet.
- Fix hypothesis: tighten the helper depth at the current boundary to admit exactly one extra alias shell, keep the existing packet-bounded tests, refresh `implementation-notes.md`, and rerun the focused and full gates.
