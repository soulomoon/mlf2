# Round 183 Plan

- Round: `round-183`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, exact `sameLaneDoubleAliasFrameClearBoundaryExpr` slice only, systematic-debugging-first, TDD-first, non-widening, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded item-5 slice: the adjacent same-lane
retained-child follow-on packet
`sameLaneDoubleAliasFrameClearBoundaryExpr`.

Accepted `round-174` previously earned narrow success for that packet by
letting `src/MLF/Elab/TermClosure.hs` admit exactly one extra same-lane alias
shell beyond the final clear-boundary child. Accepted `round-182` then
narrowed the shared retained-child preservation seam again for the adjacent
one-alias packet `sameLaneAliasFrameClearBoundaryExpr`, but deliberately left
the one-frame alias recursion in place. This round must now determine whether
the selected double-alias packet still stays honest on `runPipelineElab` and
`runPipelineElabChecked` only via that one-extra-alias-shell `TermClosure`
rule, whether the remaining alias-shell recursion is now unnecessary debt, or
whether the packet has fallen back to narrower current-architecture blocker
debt on the current baseline. If production changes are justified, they must
start with failing focused double-alias tests and land only the smallest
lawful `TermClosure` plus exact-packet test correction. No route-family
widening, fallback-core edits, pipeline-facade edits, cyclic or multi-SCC
behavior, equi-recursive reasoning, fallback widening, or second-interface
work is authorized.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-183/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-183/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan and an optional
  `implementation-notes.md` only.
- `orchestrator/rounds/round-183/selection.md` is the round input and must
  remain untouched.

The only live packet for this round is:

- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  `sameLaneDoubleAliasFrameClearBoundaryExpr`
- `test/PipelineSpec.hs`
  `sameLaneDoubleAliasFrameClearBoundaryExpr double-alias clear-boundary packet preserves recursive output on both authoritative entrypoints`

Adjacent read-only controls may be inspected and rerun, but they are not
writable slice targets for this round:

- `sameLaneAliasFrameClearBoundaryExpr` in
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
  `preserveRetainedChildAliasBoundary`,
  `hasRetainedChildAliasBoundary`,
  `isClearBoundaryRetainedChildRhs`, and
  `isIdentityBoundaryLambda`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  only for the exact
  `sameLaneDoubleAliasFrameClearBoundaryExpr`
  assertions
- `test/PipelineSpec.hs`
  only for the matching
  `sameLaneDoubleAliasFrameClearBoundaryExpr`
  authoritative-entrypoint coverage and any focused source/mechanism guard
  needed to keep the selected `TermClosure` seam honest
- `orchestrator/rounds/round-183/implementation-notes.md`
  only if the implementer needs a round-local record of the exact bounded
  result

Do not modify:

- `orchestrator/rounds/round-183/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-183/review.md`
- `orchestrator/rounds/round-183/merge.md`
- `docs/plans/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Scope.hs`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- the `sameLaneAliasFrameClearBoundaryExpr` assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- the `sameLaneAliasFrameClearBoundaryExpr` assertions in
  `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

This round must stay inside the exact
`sameLaneDoubleAliasFrameClearBoundaryExpr`
packet only. Do not reopen the adjacent one-alias packet, the `C1` non-local
packet, the `P5` control row, the negative-family rows, or the item-3 route /
guard contract.

## Sequential Plan

1. Reproduce and localize the selected double-alias packet before proposing
   any fix; modify no files in this step.
   - Re-run the exact
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     assertions from
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
     and the matching authoritative-entrypoint assertion in
     `test/PipelineSpec.hs`.
   - Re-run the adjacent
     `sameLaneAliasFrameClearBoundaryExpr`
     control as read-only evidence only, so the round can distinguish a
     selected-packet issue from a regression in the already accepted one-alias
     packet.
   - Inspect the selected authoritative-preservation path in
     `src/MLF/Elab/TermClosure.hs`:
     `preserveRetainedChildAuthoritativeResult`,
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`,
     `isClearBoundaryRetainedChildRhs`, and
     `isIdentityBoundaryLambda`.
   - Inspect `src/MLF/Elab/Run/Pipeline.hs` read-only, specifically
     `runPipelineElabWith` and the
     `preserveRetainedChildAuthoritativeResult` call site, only to confirm
     that the authoritative surfaces still flow through the shared
     `TermClosure` seam and not through a new packet-local rescue.
   - Inspect `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` read-only, only to
     confirm that
     `sameLaneLocalRetainedChildTarget`,
     `boundHasForallFrom`,
     `keepTargetFinal`, and
     `targetC`
     are unchanged and remain outside the writable slice for this round.
   - End the investigation with one explicit technical conclusion only:
     either the selected packet still depends on the one-extra-alias-shell
     branch in `hasRetainedChildAliasBoundary`, or it already survives without
     that alias-shell recursion and the remaining branch is unnecessary debt,
     or it cannot stay honest without reopening out-of-slice files and must be
     downgraded to a narrower blocker read.
   - If the truthful fix would require editing
     `Fallback/Core`,
     `Fallback.hs`,
     `Run/Scope.hs`,
     `Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`, or
     `src-public/MLF/Pipeline.hs`,
     stop at an exact narrower current-architecture blocker characterization
     rather than widening scope in place.

2. Add the failing focused tests first and keep them bounded to the selected
   packet.
   - In
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     tighten only the
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     assertions from the current generic `containsMu` check to the exact
     honest authoritative result for this round.
     If the packet still earns narrow success, reuse
     `expectExactRetainedChildAuthoritativeOutput`;
     if step 1 proves fail-closed or blocker debt instead, encode that exact
     failure shape instead of leaving a generic success claim.
   - In `test/PipelineSpec.hs`, replace the current generic
     `expectAlignedPipelineSuccessType` / `containsMu`
     read for the selected packet with an exact authoritative-entrypoint
     assertion on both
     `runPipelineElab` and `runPipelineElabChecked`,
     reusing
     `countLeadingUnboundedForalls`,
     `stripLeadingUnboundedForalls`,
     `matchesRecursiveArrow`, and
     `expectedSameLaneAliasFrameClearBoundaryArrow`
     if the truthful outcome remains the same recursive arrow shape.
   - Add one focused source/mechanism guard in `test/PipelineSpec.hs` tied to
     the step-1 conclusion:
     if the selected packet still lawfully needs one extra alias shell, the
     guard must fail unless `TermClosure` stays bounded to exactly one extra
     alias frame while still requiring the round-182
     `isClearBoundaryRetainedChildRhs` /
     `isIdentityBoundaryLambda`
     final-child shape;
     if the selected packet no longer needs alias-shell recursion, the guard
     must fail while that recursive alias-shell branch remains in
     `hasRetainedChildAliasBoundary`.
   - Leave the adjacent
     `sameLaneAliasFrameClearBoundaryExpr`
     assertions unchanged; they remain read-only controls only.
   - Run the selected packet commands immediately and observe the intended RED
     failure before changing production code.

3. Apply the smallest lawful production correction only after the RED run
   fails.
   - Start and stay in `src/MLF/Elab/TermClosure.hs`.
   - If step 1 shows the selected packet still honestly depends on one extra
     same-lane alias shell, keep the shared alias-boundary seam but narrow
     `preserveRetainedChildAliasBoundary` /
     `hasRetainedChildAliasBoundary`
     to the smallest exact double-alias condition that keeps the already
     accepted one-alias packet and the selected double-alias packet green
     without admitting deeper alias chains, generalized alias-depth support,
     or a broader final-child rule than round-182 already earned.
   - If step 1 shows the selected packet already survives without alias-shell
     recursion, remove or simplify that recursion rather than leaving
     unnecessary packet-specific folklore in place.
   - If the RED run proves that the current code already has the exact bounded
     rule and only the tests were stale, leave
     `src/MLF/Elab/TermClosure.hs`
     unchanged and stop after the bounded test synchronization.
   - Keep
     `preserveRetainedChildAuthoritativeResult`,
     `closeTermWithSchemeSubstIfNeeded`,
     `alignTermTypeVarsToScheme`,
     `alignTermTypeVarsToSchemeBody`,
     and the item-3 route / guard tokens unchanged unless step 1 proves the
     selected alias-shell branch itself is the exact debt to remove or narrow.
   - Do not touch pipeline facades, fallback-core files, route selection,
     search shape, cyclic handling, multi-SCC handling, equi-recursive
     reasoning, fallback widening, or a second interface.
   - If no in-slice change can keep the selected packet honest without
     reopening out-of-slice files or admitting broader alias-depth support,
     stop at an exact packet-specific blocker record rather than widening the
     family in place.

4. Re-green the exact packet and keep the outcome honest.
   - Make only the selected
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     tests green.
   - Re-run the adjacent one-alias control after the selected packet is green,
     but keep that control read-only and unchanged.
   - Update
     `orchestrator/rounds/round-183/implementation-notes.md`
     only if needed, and if written record one packet-bounded result only:
     either
     `sameLaneDoubleAliasFrameClearBoundaryExpr remains honest only via an exact one-extra-alias-shell TermClosure rule`,
     or
     `sameLaneDoubleAliasFrameClearBoundaryExpr no longer depends on alias-shell recursion`,
     or
     `sameLaneDoubleAliasFrameClearBoundaryExpr remains predecessor-only folklore / narrower current-architecture blocker`.
   - Do not extrapolate this one packet into general `P3`, `P4`, or `P6`
     closure or repo-level readiness.

5. Run the focused and full verification gates serially.
   - Re-run the exact selected-packet commands from both
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
     and `test/PipelineSpec.hs`.
   - Re-run the adjacent one-alias control command as read-only regression
     protection.
   - Recheck that the diff stayed inside the authorized writable slice and
     that the item-3 route / guard cluster stayed read-only.
   - Re-run `cabal build all && cabal test` because source/test files are in
     scope for this round.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `rg -n 'preserveRetainedChildAuthoritativeResult|preserveRetainedChildAliasBoundary|hasRetainedChildAliasBoundary|isClearBoundaryRetainedChildRhs|isIdentityBoundaryLambda' src/MLF/Elab/TermClosure.hs`
- `rg -n 'runPipelineElabWith|preserveRetainedChildAuthoritativeResult' src/MLF/Elab/Run/Pipeline.hs`
- `rg -n 'sameLaneLocalRetainedChildTarget|boundHasForallFrom|keepTargetFinal|targetC' src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr preserves recursive output"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr double-alias clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
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
PY`
- `git diff --check`
- `cabal build all && cabal test`
