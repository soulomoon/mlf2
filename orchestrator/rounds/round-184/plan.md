# Round 184 Plan

- Round: `round-184`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, exact `sameLaneTripleAliasFrameClearBoundaryExpr` slice only, systematic-debugging-first, TDD-first, non-widening, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded item-5 slice: the next same-lane
retained-child follow-on packet
`sameLaneTripleAliasFrameClearBoundaryExpr`.

Accepted `round-182` settled the one-alias packet
`sameLaneAliasFrameClearBoundaryExpr` as honest only via a narrowed
clear-boundary retained-child rule in `src/MLF/Elab/TermClosure.hs`.
Accepted `round-183` then settled the adjacent double-alias packet
`sameLaneDoubleAliasFrameClearBoundaryExpr` as honest only via the still-live
bounded one-extra-alias-shell rule anchored by
`hasRetainedChildAliasBoundary v body 1 =`, with the focused source guard
explicitly forbidding an already-present depth-`2` entrypoint marker. This
round must now determine whether the selected triple-alias packet stays honest
on `runPipelineElab` and `runPipelineElabChecked` only via an exact
two-extra-alias-shell `TermClosure` rule, whether it already survives without
new alias-shell support, or whether the current architecture honestly stops at
the now-locked double-alias case. If production changes are justified, they
must start with failing focused triple-alias tests and land only the smallest
lawful `TermClosure` plus exact-packet test correction. No route-family
widening, fallback-core edits, pipeline-facade edits, cyclic or multi-SCC
behavior, equi-recursive reasoning, fallback widening, or second-interface
work is authorized.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-184/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-184/selection.md` is the round input and must
  remain untouched.
- `orchestrator/rounds/round-184/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan and an optional
  `implementation-notes.md` only.

The only live packet for this round is:

- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  new exact packet `sameLaneTripleAliasFrameClearBoundaryExpr`
- `test/PipelineSpec.hs`
  the matching
  `sameLaneTripleAliasFrameClearBoundaryExpr`
  authoritative-entrypoint assertion and any focused `TermClosure`
  source/mechanism guard needed to keep the selected slice honest

Adjacent read-only controls may be inspected and rerun, but they are not
writable slice targets for this round:

- `sameLaneAliasFrameClearBoundaryExpr` in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
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
  `preserveRetainedChildAliasBoundary`,
  `hasRetainedChildAliasBoundary`,
  `isClearBoundaryRetainedChildRhs`, and
  `isIdentityBoundaryLambda`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  only for:
  `spec`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`,
  and the exact selected-packet assertions that reuse
  `expectExactRetainedChildAuthoritativeOutput`,
  `countLeadingUnboundedForalls`,
  `stripLeadingUnboundedForalls`, and
  `matchesRecursiveArrow`
- `test/PipelineSpec.hs`
  only for the matching selected-packet authoritative-entrypoint coverage,
  reusing the existing helpers
  `expectStrictPipelineFailure`,
  `countLeadingUnboundedForalls`,
  `stripLeadingUnboundedForalls`,
  `matchesRecursiveArrow`, and
  `expectedSameLaneAliasFrameClearBoundaryArrow`
- `orchestrator/rounds/round-184/implementation-notes.md`
  only if the implementer needs a round-local record of the exact bounded
  result

Do not modify:

- `orchestrator/rounds/round-184/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-184/review.md`
- `orchestrator/rounds/round-184/merge.md`
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
- the `sameLaneDoubleAliasFrameClearBoundaryExpr` assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- the `sameLaneAliasFrameClearBoundaryExpr` assertions in
  `test/PipelineSpec.hs`
- the `sameLaneDoubleAliasFrameClearBoundaryExpr` assertions in
  `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

This round must stay inside the exact
`sameLaneTripleAliasFrameClearBoundaryExpr`
packet only. Do not reopen the adjacent one-alias packet, the adjacent
double-alias packet, the `C1` non-local packet, the `P5` control row, the
negative-family rows, or the item-3 route / guard contract.

## Sequential Plan

1. Reproduce and localize the selected triple-alias packet before proposing
   any fix; modify no files in this step.
   - Probe the current baseline directly in `cabal repl mlf2-test` with the
     exact selected surface term: the current
     `sameLaneDoubleAliasFrameClearBoundaryExpr` shape plus one additional
     same-lane alias binder between `keep` and the final retained-child
     consumer, for example
     `ELet "more" (EVar "keep") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u"))`.
   - Re-run the adjacent
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     and
     `sameLaneAliasFrameClearBoundaryExpr`
     controls as read-only evidence only, so the round can distinguish a
     selected-packet issue from a regression in already accepted predecessor
     packets.
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
     either the selected packet still needs an exact two-extra-alias-shell
     branch in `hasRetainedChildAliasBoundary`, or it already survives
     without new alias-shell support, or it cannot stay honest without
     reopening out-of-slice files and must be downgraded to a narrower
     current-architecture blocker read.
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
     add
     `sameLaneTripleAliasFrameClearBoundaryExpr`
     and exactly two selected-packet assertions, one for
     `runPipelineElab` and one for
     `runPipelineElabChecked`.
   - If step 1 proves the packet still earns narrow success, make those two
     new research assertions reuse
     `expectExactRetainedChildAuthoritativeOutput`
     with the same exact two-forall recursive-arrow expectation already used
     for the settled alias and double-alias packets.
   - If step 1 proves the packet is blocker-shaped on the current baseline,
     encode that exact blocker shape in those two new research assertions
     instead of leaving a generic `containsMu` or vague success claim.
   - Keep the existing
     `sameLaneAliasFrameClearBoundaryExpr`
     and
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     research assertions unchanged.
   - In `test/PipelineSpec.hs`, add one exact
     `sameLaneTripleAliasFrameClearBoundaryExpr`
     regression on both authoritative entrypoints.
     Reuse
     `countLeadingUnboundedForalls`,
     `stripLeadingUnboundedForalls`,
     `matchesRecursiveArrow`, and
     `expectedSameLaneAliasFrameClearBoundaryArrow`
     if the packet earns narrow success; otherwise reuse
     `expectStrictPipelineFailure`
     or the exact observed non-recursive output shape from step 1 so the
     selected packet is still locked to one honest current read only.
   - Add one focused `TermClosure` source/mechanism guard in
     `test/PipelineSpec.hs` tied to the step-1 conclusion:
     if the selected packet should pass, require the exact depth-`2`
     entrypoint and still forbid any depth-`3` entrypoint or broader search;
     if the selected packet should fail, require that the depth-`2` entrypoint
     is still absent and that the settled depth-`1` rule remains the current
     outer bound.
   - Run the selected packet commands immediately after the test edits and
     record the intended RED failure before changing production code, unless
     step 1 already proved that the honest selected result is blocker-only and
     the test surface is locking that blocker in place.

3. Apply the smallest lawful production correction only after the selected
   packet behavior is explicit.
   - Start in `src/MLF/Elab/TermClosure.hs`, because the selected debt is the
     authoritative-preservation seam there, not the item-3 route selection.
   - If step 1 and step 2 show that the selected packet honestly earns narrow
     success one alias shell beyond the locked double-alias rule, change only
     `preserveRetainedChildAliasBoundary` /
     `hasRetainedChildAliasBoundary`
     so the entrypoint admits exactly two extra same-lane alias shells for
     this packet, while keeping the final clear-boundary retained-child test,
     the identity-boundary lambda requirement, and the alias-frame recursion
     shape otherwise unchanged.
   - If the selected packet already survives without new alias-shell support,
     remove or narrow only the unnecessary alias-shell recursion inside
     `preserveRetainedChildAuthoritativeResult`; do not replace it with a
     differently named packet-local helper.
   - If step 1 proves that no in-slice `TermClosure` edit can keep the
     selected packet honest without reopening out-of-slice files or admitting
     broader alias-depth support than the exact triple-alias packet earned,
     leave production code unchanged and keep the round at an exact
     packet-specific blocker read.
   - Keep
     `closeTermWithSchemeSubstIfNeeded`,
     `alignTermTypeVarsToScheme`,
     `alignTermTypeVarsToSchemeBody`,
     and the item-3 route / guard tokens unchanged unless step 1 proves the
     selected alias-shell branch itself is the exact debt to remove or narrow.
   - Do not touch pipeline facades, fallback-core files, route selection,
     search shape, cyclic handling, multi-SCC handling, equi-recursive
     reasoning, fallback widening, or a second interface.

4. Re-green the exact packet and keep the outcome honest.
   - Make only the selected
     `sameLaneTripleAliasFrameClearBoundaryExpr`
     tests green, or keep only the exact blocker read green if no lawful
     production fix exists inside the writable slice.
   - Re-run the adjacent one-alias and double-alias controls after the
     selected packet is green, but keep those controls read-only and
     unchanged.
   - Update
     `orchestrator/rounds/round-184/implementation-notes.md`
     only if needed, and if written record one packet-bounded result only:
     either
     `sameLaneTripleAliasFrameClearBoundaryExpr remains honest only via an exact two-extra-alias-shell TermClosure rule`,
     or
     `sameLaneTripleAliasFrameClearBoundaryExpr no longer depends on alias-shell recursion`,
     or
     `sameLaneTripleAliasFrameClearBoundaryExpr remains a narrower current-architecture blocker`.
   - Do not extrapolate this one packet into general `P3`, `P4`, or `P6`
     closure or repo-level readiness.

5. Run the focused and full verification gates serially.
   - Re-run the exact selected-packet commands from both
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
     and `test/PipelineSpec.hs`.
   - Re-run the adjacent one-alias and double-alias control commands as
     read-only regression protection.
   - Recheck that the diff stayed inside the authorized writable slice and
     that the item-3 route / guard cluster stayed read-only.
   - Re-run `cabal build all && cabal test` because source/test files are in
     scope for this round.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md" && for f in orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md; do rg -n "2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap|rev-001|orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001" "$f"; done`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let expr =
      ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
          (ELet "keep" (EVar "hold")
            (ELet "more" (EVar "keep")
              (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u")))))
print (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
print (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
:q
EOF`
- `rg -n 'preserveRetainedChildAuthoritativeResult|preserveRetainedChildAliasBoundary|hasRetainedChildAliasBoundary|isClearBoundaryRetainedChildRhs|isIdentityBoundaryLambda' src/MLF/Elab/TermClosure.hs`
- `rg -n 'runPipelineElabWith|preserveRetainedChildAuthoritativeResult' src/MLF/Elab/Run/Pipeline.hs`
- `rg -n 'sameLaneLocalRetainedChildTarget|boundHasForallFrom|keepTargetFinal|targetC' src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `rg -n 'sameLaneTripleAliasFrameClearBoundaryExpr|sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr' test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/PipelineSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'orchestrator/rounds/round-184/implementation-notes.md',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-184', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-184', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [
    p for p in paths
    if p not in allowed
    and p not in {
        'orchestrator/rounds/round-184/plan.md',
        'orchestrator/rounds/round-184/selection.md',
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
print('ROUND184_WRITABLE_SLICE_OK')
PY`
- `git diff --check`
- `cabal build all && cabal test`
