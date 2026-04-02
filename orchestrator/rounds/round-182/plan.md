# Round 182 Plan

- Round: `round-182`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, exact `sameLaneAliasFrameClearBoundaryExpr` slice only, systematic-debugging-first, TDD-first, non-widening

## Objective

Keep this round on exactly one bounded item-5 slice: the first settled
same-lane retained-child packet
`sameLaneAliasFrameClearBoundaryExpr`.

Accepted `round-170` made that packet recursively reconstruction-visible on
`runPipelineElab` and `runPipelineElabChecked` by extending
`preserveRetainedChildAuthoritativeResult` in
`src/MLF/Elab/TermClosure.hs`, and accepted `round-181` then removed a
different packet-local `C1` shortcut from `src/MLF/Elab/Run/Pipeline.hs`.
This round must determine whether the selected same-lane packet now stays
recursive on the authoritative entrypoints without leaning on the remaining
`preserveRetainedChildAliasBoundary` /
`hasRetainedChildAliasBoundary` rescue story, or whether that result is still
packet-specific folklore. If production changes are justified, they must start
with failing focused tests and land only the smallest lawful
`TermClosure` / `Run/Pipeline` correction or narrowing for this exact packet.
No route-family widening, fallback-core edits, search changes, cyclic or
multi-SCC behavior, equi-recursive reasoning, fallback widening, or second
interface work is authorized.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-182/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-182/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan and an optional
  `implementation-notes.md` only.
- `orchestrator/rounds/round-182/selection.md` is the round input and must
  remain untouched.

The only live packet for this round is:

- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  `sameLaneAliasFrameClearBoundaryExpr`

Adjacent read-only controls may be inspected, but they are not writable slice
targets for this round:

- `sameLaneDoubleAliasFrameClearBoundaryExpr` in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
- the accepted `C1` packet in
  `test/Research/C1AuthoritativeSurfaceSpec.hs`
- the `P5` clear-boundary / nested-`forall` control pair in
  `test/Research/P5ClearBoundarySpec.hs`

The inherited boundary remains controlling:

- explicit-only
- iso-recursive
- non-equi-recursive
- non-cyclic-graph
- no-fallback

The item-3-admitted route families and guard cluster also remain fixed and
read-only here:

- `sameLaneLocalRetainedChildTarget`
- `boundHasForallFrom`
- `keepTargetFinal`
- `targetC`

This round must not reinterpret one packet as general `P3`, `P4`, or `P6`
closure or as repo-level readiness.

## Write Scope

Implementer-owned writes for this round are limited to:

- `src/MLF/Elab/TermClosure.hs`
  only for the selected authoritative-preservation seam, centered on
  `preserveRetainedChildAuthoritativeResult`,
  `preserveRetainedChildAliasBoundary`, and
  `hasRetainedChildAliasBoundary`
- `src/MLF/Elab/Run/Pipeline.hs`
  only if `runPipelineElabWith` must change in lockstep with the
  `TermClosure` adjustment and only to remove or rewire the selected
  authoritative-preservation seam, not to add a new packet-local shortcut
- `src/MLF/Elab/Pipeline.hs`
  only if the internal authoritative facade must change in exact lockstep with
  a real `runPipelineElab` / `runPipelineElabChecked` contract change
- `src-public/MLF/Pipeline.hs`
  only if the public authoritative facade must change in exact lockstep with
  the same real contract change
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  only for the exact `sameLaneAliasFrameClearBoundaryExpr` assertions
- `test/PipelineSpec.hs`
  only for the matching
  `sameLaneAliasFrameClearBoundaryExpr` authoritative-entrypoint coverage and
  any source/mechanism guard needed to keep the selected `TermClosure` /
  `Run/Pipeline` seam honest
- `orchestrator/rounds/round-182/implementation-notes.md`
  only if the implementer needs a round-local record of the exact bounded
  result

Do not modify:

- `orchestrator/rounds/round-182/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-182/review.md`
- `orchestrator/rounds/round-182/merge.md`
- `docs/plans/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Scope.hs`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- the `sameLaneDoubleAliasFrameClearBoundaryExpr` assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- the `sameLaneDoubleAliasFrameClearBoundaryExpr` assertions in
  `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

This round must stay inside the exact
`sameLaneAliasFrameClearBoundaryExpr` packet only. Do not reopen the adjacent
double-alias control packet, the `C1` non-local packet, the `P5` control row,
the negative-family rows, or the item-3 route / guard contract.

## Sequential Plan

1. Reproduce and localize the selected same-lane packet before proposing any
   fix; modify no files in this step.
   - Re-run the exact `sameLaneAliasFrameClearBoundaryExpr` assertions from
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and the
     matching authoritative-entrypoint assertion in `test/PipelineSpec.hs`.
   - Inspect the selected authoritative-preservation path in
     `src/MLF/Elab/TermClosure.hs`:
     `preserveRetainedChildAuthoritativeResult`,
     `preserveRetainedChildAliasBoundary`, and
     `hasRetainedChildAliasBoundary`.
   - Inspect the post-closure authoritative entrypoint path in
     `src/MLF/Elab/Run/Pipeline.hs`,
     specifically `runPipelineElabWith` and the
     `preserveRetainedChildAuthoritativeResult` call site.
   - Use `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` only as a read-only
     continuity anchor to confirm that
     `sameLaneLocalRetainedChildTarget`,
     `boundHasForallFrom`,
     `keepTargetFinal`, and
     `targetC`
     are unchanged and are not the lawful edit target for this round.
   - End the investigation with one explicit technical conclusion only:
     either the selected packet still depends on the
     `TermClosure` alias-boundary rescue, or the packet already survives on
     the authoritative surfaces without that rescue and the remaining seam is
     unnecessary debt.
   - If the only truthful fix would require editing the item-3 route families,
     `Fallback/Core`, `Fallback.hs`, `Run/Scope.hs`, or any out-of-slice file,
     stop at an exact narrower current-architecture blocker characterization
     rather than widening scope in place.

2. Add the failing focused tests first and keep them bounded to the selected
   packet.
   - In `test/PipelineSpec.hs`, add one exact
     `sameLaneAliasFrameClearBoundaryExpr`
     regression that is easy to run in isolation and that asserts the honest
     current packet-level outcome on both
     `runPipelineElab` and `runPipelineElabChecked`.
   - Reuse the existing recursive-type helpers in `test/PipelineSpec.hs`
     rather than introducing a new harness or a new test module.
   - Add one focused source/mechanism guard in `test/PipelineSpec.hs` tied to
     the step-1 root-cause conclusion:
     if the packet should survive without the alias-boundary rescue, the guard
     must fail while the old
     `preserveRetainedChildAliasBoundary` /
     `hasRetainedChildAliasBoundary`
     rescue story remains in the authoritative path;
     if a smaller shared `TermClosure` rule is still legitimately required,
     the guard must fail unless the code is narrowed to that smaller exact
     condition and still leaves the item-3 route / guard cluster untouched.
   - In
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     tighten only the
     `sameLaneAliasFrameClearBoundaryExpr`
     assertions to the exact honest authoritative result for this round.
     Leave the adjacent
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     control assertions unchanged.
   - Run the selected packet commands immediately and observe the intended RED
     failure before changing production code.

3. Apply the smallest lawful production correction only after the RED run
   fails.
   - Start in `src/MLF/Elab/TermClosure.hs`, because the selected debt is the
     authoritative-preservation seam there, not the item-3 route selection.
   - If step 1 shows the selected packet already survives without the
     alias-boundary rescue, remove that rescue path from
     `preserveRetainedChildAuthoritativeResult` and do not replace it with a
     differently named packet-local helper.
   - If step 1 shows the selected packet still needs some shared
     `TermClosure` support, narrow
     `preserveRetainedChildAliasBoundary` /
     `hasRetainedChildAliasBoundary`
     to the smallest exact same-lane retained-child condition that keeps the
     selected packet honest without widening beyond the accepted route / guard
     contract.
   - Keep
     `closeTermWithSchemeSubstIfNeeded`,
     `alignTermTypeVarsToScheme`,
     `alignTermTypeVarsToSchemeBody`,
     and the item-3 route / guard tokens unchanged unless step 1 proves the
     selected packet already survives there and the alias-boundary rescue can
     be removed entirely.
   - Touch `src/MLF/Elab/Run/Pipeline.hs` only if the
     `runPipelineElabWith` call order or authoritative post-closure plumbing
     must change in exact lockstep with the `TermClosure` adjustment.
     Do not add a new `Run/Pipeline` shortcut or second rescue story.
   - Touch `src/MLF/Elab/Pipeline.hs` or `src-public/MLF/Pipeline.hs` only if
     the real authoritative entrypoint/export contract changes in lockstep
     with the selected fix; otherwise leave both facades untouched.
   - If no in-slice change can keep the selected packet honest without
     reopening out-of-slice files or regressing the accepted adjacent control
     packet, stop at an exact packet-specific folklore / narrower blocker
     record rather than widening the family in place.

4. Re-green the exact packet and keep the outcome honest.
   - Make only the selected
     `sameLaneAliasFrameClearBoundaryExpr`
     tests green.
   - Leave the adjacent
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     assertions untouched; they remain control evidence only.
   - Update
     `orchestrator/rounds/round-182/implementation-notes.md`
     only if needed, and if written record one packet-bounded result only:
     either
     `sameLaneAliasFrameClearBoundaryExpr no longer depends on packet-local TermClosure rescue`,
     or
     `sameLaneAliasFrameClearBoundaryExpr remains honest only via a narrowed shared TermClosure rule`,
     or
     `sameLaneAliasFrameClearBoundaryExpr remains packet-specific folklore / narrower current-architecture blocker`.
   - Do not extrapolate this one packet into general `P3`, `P4`, or `P6`
     support, and do not reopen docs, roadmap state, or repo-level readiness
     claims.

5. Run the focused and full verification gates.
   - Re-run the exact selected packet commands.
   - Reinspect the
     `TermClosure` and `Run/Pipeline` anchors to confirm the selected seam is
     now deleted or narrowed in the exact way the tests expect.
   - Recheck that the diff stayed inside the authorized writable slice.
   - Re-run `cabal build all && cabal test` to catch regressions against the
     unchanged adjacent controls and the rest of the suite.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr preserves recursive output"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- `rg -n 'preserveRetainedChildAuthoritativeResult|preserveRetainedChildAliasBoundary|hasRetainedChildAliasBoundary' src/MLF/Elab/TermClosure.hs`
- `nl -ba src/MLF/Elab/Run/Pipeline.hs | sed -n '188,206p'`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'src/MLF/Elab/Run/Pipeline.hs',
    'src/MLF/Elab/Pipeline.hs',
    'src-public/MLF/Pipeline.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/PipelineSpec.hs',
    'orchestrator/rounds/round-182/implementation-notes.md',
}
tracked = subprocess.check_output(
    [
        'git',
        'diff',
        '--name-only',
        '--',
        'src',
        'src-public',
        'test',
        'orchestrator/rounds/round-182',
        'docs/plans',
        'TODO.md',
        'implementation_notes.md',
        'Bugs.md',
        'mlf2.cabal',
    ],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    [
        'git',
        'ls-files',
        '--others',
        '--exclude-standard',
        '--',
        'src',
        'src-public',
        'test',
        'orchestrator/rounds/round-182',
        'docs/plans',
    ],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [
    p for p in paths
    if p not in allowed
    and p not in {
        'orchestrator/rounds/round-182/plan.md',
        'orchestrator/rounds/round-182/selection.md',
    }
]
forbidden = [
    p for p in paths
    if p in {
        'docs/plans',
        'TODO.md',
        'implementation_notes.md',
        'Bugs.md',
        'mlf2.cabal',
    }
]
if extra or forbidden:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND182_SAMELANE_WRITABLE_SLICE_OK')
PY`
- `git diff --check`
- `cabal build all && cabal test`
