# Round 190 Plan

- Round: `round-190`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, exact `sameLaneNonupleAliasFrameClearBoundaryExpr` slice only, systematic-debugging-first, TDD-first, non-widening, no worker fan-out, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded item-5 slice: the adjacent same-lane
retained-child follow-on packet
`sameLaneNonupleAliasFrameClearBoundaryExpr`.

Accepted `round-181` through `round-189` are bounded predecessor truth only.
For this lane specifically, accepted `round-189` settled
`sameLaneOctupleAliasFrameClearBoundaryExpr` as honest only via the shared
`src/MLF/Elab/TermClosure.hs` seam with the outer
`hasRetainedChildAliasBoundary v body 2 =` entry budget plus the terminal
`hasRetainedChildClearBoundaryWithAliasBudget source term 4` helper step.

This round must now determine whether one more same-lane local alias binder
beyond the accepted octuple packet remains honest on
`runPipelineElab` / `runPipelineElabChecked`, whether that exact packet earns
only one more bounded terminal-helper alias step inside `TermClosure`, or
whether the current architecture honestly stops at the accepted octuple
case. If production changes are justified, they must start with a failing
focused nonuple test and land only the smallest lawful `TermClosure` plus
selected-packet test correction. No route-family widening, fallback-core edit,
pipeline-facade edit, cyclic or multi-SCC behavior, equi-recursive reasoning,
fallback widening, or second-interface work is authorized.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-190/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `orchestrator/rounds/round-190/selection.md` is the round input and must
  remain untouched.
- `orchestrator/rounds/round-190/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan and an optional
  `implementation-notes.md` only.

Accepted predecessor packets remain read-only evidence only:

- `round-181`: exact `C1` non-local authoritative packet only
- `round-182`: `sameLaneAliasFrameClearBoundaryExpr` only
- `round-183`: `sameLaneDoubleAliasFrameClearBoundaryExpr` only
- `round-184`: `sameLaneTripleAliasFrameClearBoundaryExpr` only
- `round-185`: `sameLaneQuadrupleAliasFrameClearBoundaryExpr` only
- `round-186`: `sameLaneQuintupleAliasFrameClearBoundaryExpr` only
- `round-187`: `sameLaneSextupleAliasFrameClearBoundaryExpr` only
- `round-188`: `sameLaneSeptupleAliasFrameClearBoundaryExpr` only
- `round-189`: `sameLaneOctupleAliasFrameClearBoundaryExpr` only

Direct planner baseline probing in this canonical round worktree already
matches the accepted predecessor read and the current selected gap:

- the accepted octuple packet still returns `Right ...` on both
  `runPipelineElab` and `runPipelineElabChecked`
- the selected nonuple packet still returns
  `Left (PipelineTypeCheckError (TCLetTypeMismatch ...))` on both
  authoritative entrypoints before any new edits
- one fresh deeper decuple read-only control also still returns that same
  `Left ...` shape on both authoritative entrypoints, so the current helper
  remains closed at or before the selected nonuple depth
- the current source markers still read
  `hasRetainedChildAliasBoundary v body 2 =` and
  `hasRetainedChildClearBoundaryWithAliasBudget source term 4`, with no live
  `hasRetainedChildAliasBoundary v body 3 =` or
  `hasRetainedChildClearBoundaryWithAliasBudget source term 5`

The only live packet for this round is:

- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  new exact packet `sameLaneNonupleAliasFrameClearBoundaryExpr`
- `test/PipelineSpec.hs`
  the matching
  `sameLaneNonupleAliasFrameClearBoundaryExpr`
  authoritative-entrypoint assertion and one focused `TermClosure`
  source/mechanism guard
- `src/MLF/Elab/TermClosure.hs`
  only if the investigation proves that the exact selected packet can stay
  honest through one more bounded terminal-helper alias step inside the
  already-admitted shared seam

Adjacent read-only controls may be inspected and rerun, but they are not
writable slice targets for this round:

- `sameLaneAliasFrameClearBoundaryExpr`,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`,
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
  `sameLaneSextupleAliasFrameClearBoundaryExpr`,
  `sameLaneSeptupleAliasFrameClearBoundaryExpr`, and
  `sameLaneOctupleAliasFrameClearBoundaryExpr` in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
- one fresh deeper same-lane alias-shell probe beyond the selected packet in
  `cabal repl mlf2-test` only, as read-only boundedness evidence; do not add a
  persistent decuple test in this round
- the accepted `C1` packet in
  `test/Research/C1AuthoritativeSurfaceSpec.hs`
- the `P5` clear-boundary / nested-`forall` control pair in
  `test/Research/P5ClearBoundarySpec.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
  (`runPipelineElab`, `runPipelineElabChecked`, `runPipelineElabWith`, and the
  `preserveRetainedChildAuthoritativeResult` call site) as read-only authority
  flow context only
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  (`sameLaneLocalRetainedChildTarget`, `boundHasForallFrom`,
  `keepTargetFinal`, and `targetC`) as read-only route/guard context only

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

This round must not reinterpret one exact packet into general `P3`, `P4`, or
`P6` closure or into repo-level readiness.

## Write Scope

Implementer-owned writes for this round are limited to:

- `src/MLF/Elab/TermClosure.hs`
  only for the selected authoritative-preservation seam, centered on
  `preserveRetainedChildAliasBoundary`,
  `hasRetainedChildAliasBoundary`,
  `hasRetainedChildClearBoundary`,
  `hasRetainedChildClearBoundaryWithAliasBudget`,
  `isAliasFrameRhs`, and
  `isClearBoundaryRetainedChildRhs`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  only for:
  `spec`,
  `sameLaneNonupleAliasFrameClearBoundaryExpr`,
  and the exact selected-packet assertions or tiny packet-local expectation
  helper needed to encode the honest current read
- `test/PipelineSpec.hs`
  only for the matching selected-packet authoritative-entrypoint coverage and
  one focused `TermClosure` source/mechanism guard, reusing existing helpers
  such as
  `expectStrictPipelineFailure`,
  `countLeadingUnboundedForalls`,
  `stripLeadingUnboundedForalls`,
  `matchesRecursiveArrow`, and
  `expectedSameLaneAliasFrameClearBoundaryArrow`
- `orchestrator/rounds/round-190/implementation-notes.md`
  only if the implementer needs a round-local record of the exact bounded
  result

Do not modify:

- `orchestrator/rounds/round-190/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-190/review.md`
- `orchestrator/rounds/round-190/merge.md`
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
- the alias / double-alias / triple-alias / quadruple-alias / quintuple-alias /
  sextuple-alias / septuple-alias / octuple-alias predecessor assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- the alias / double-alias / triple-alias / quadruple-alias / quintuple-alias /
  sextuple-alias / septuple-alias / octuple-alias predecessor
  authoritative-outcome assertions in `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

This round must stay inside the exact
`sameLaneNonupleAliasFrameClearBoundaryExpr`
packet only. Do not reopen the accepted alias, double-alias, triple-alias,
quadruple-alias, quintuple-alias, sextuple-alias, septuple-alias, or octuple
packets; do not reopen the `C1` packet, the `P5` control row, the
negative-family rows, the item-3 route / guard contract, or any broader
item-5 aggregation artifact.

## Sequential Plan

1. Reproduce and localize the selected nonuple packet before proposing any
   fix; modify no files in this step.
   - Write targets: none.
   - Read-only inputs:
     `src/MLF/Elab/TermClosure.hs`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, and
     `test/PipelineSpec.hs`.
   - Probe the current baseline directly in `cabal repl mlf2-test` with three
     same-lane packets:
     the accepted octuple packet,
     the exact selected nonuple packet, and
     one fresh deeper read-only decuple control.
     Define the selected packet as the current octuple shape plus one
     additional same-lane alias binder between `bud` and the final retained
     child consumer, for example
     `ELet "seed" (EVar "bud") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "seed")) (EVar "u"))`.
   - Re-run the accepted octuple / septuple / sextuple / quintuple /
     quadruple / triple / double / alias packets as read-only predecessor
     evidence only, so the round can distinguish a selected-packet issue from
     a regression in already-accepted packets.
   - Inspect the selected authoritative-preservation path in
     `src/MLF/Elab/TermClosure.hs`:
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`,
     `hasRetainedChildClearBoundary`,
     `hasRetainedChildClearBoundaryWithAliasBudget`,
     `isAliasFrameRhs`, and
     `isClearBoundaryRetainedChildRhs`.
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
     either the selected packet already survives without production edits, or
     it needs one more exact terminal-helper alias step inside
     `hasRetainedChildClearBoundaryWithAliasBudget` from
     `source term 4` to `source term 5`, or it cannot stay honest without
     reopening out-of-slice files and must therefore be recorded as a narrower
     current-architecture blocker read.
   - If the truthful fix would require editing
     `Fallback/Core`,
     `Fallback.hs`,
     `Run/Scope.hs`,
     `Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`, or
     `src-public/MLF/Pipeline.hs`,
     stop at that exact blocker characterization rather than widening scope in
     place.
   - Verification:
     run the focused `cabal repl mlf2-test` probe for the octuple, selected
     nonuple, and fresh deeper decuple control, then re-run the accepted
     predecessor matches and inspect the current marker strings
     `hasRetainedChildAliasBoundary v body 2 =`,
     `hasRetainedChildClearBoundaryWithAliasBudget source term 4`, and the
     absence of `hasRetainedChildClearBoundaryWithAliasBudget source term 5`.

2. Add the failing focused tests first and keep them bounded to the selected
   packet only.
   - Modify:
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
     `test/PipelineSpec.hs`.
   - In
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     add
     `sameLaneNonupleAliasFrameClearBoundaryExpr`
     and exactly two selected-packet assertions, one for
     `runPipelineElab` and one for
     `runPipelineElabChecked`.
   - If step 1 proves the packet still earns narrow success inside the current
     architecture, make those two new research assertions reuse
     `expectExactRetainedChildAuthoritativeOutput`
     with the same exact two-forall recursive-arrow expectation already used
     for the accepted alias / double-alias / triple-alias / quadruple-alias /
     quintuple-alias / sextuple-alias / septuple-alias / octuple-alias
     packets.
   - If step 1 proves the packet is blocker-shaped on the current baseline,
     encode that exact blocker shape in those two research assertions instead
     of leaving a vague success or generic `containsMu` claim; keep any tiny
     new expectation helper local to this same file only.
   - In `test/PipelineSpec.hs`, add one exact
     `sameLaneNonupleAliasFrameClearBoundaryExpr`
     regression on both authoritative entrypoints.
   - Add one focused `TermClosure` source/mechanism guard in
     `test/PipelineSpec.hs` tied to the step-1 conclusion:
     if the packet should pass, make the red test fail against the current
     `hasRetainedChildClearBoundaryWithAliasBudget source term 4` /
     missing `source term 5` baseline, and after the fix require the exact
     selected-budget marker while still forbidding any
     `hasRetainedChildClearBoundaryWithAliasBudget source term 6`,
     `hasRetainedChildAliasBoundary v body 3 =`,
     fallback marker, or widened search token;
     if the packet should stay blocked, lock the current
     `source term 4` / no `source term 5` boundary and the selected packet's
     honest authoritative outcome instead.
   - Keep the accepted alias / double-alias / triple-alias / quadruple-alias /
     quintuple-alias / sextuple-alias / septuple-alias / octuple-alias
     authoritative-outcome assertions unchanged.
   - Ensure the selected nonuple guard owns the exact helper-step marker for
     this round only. If the selected positive path lands, narrow the existing
     octuple mechanism guard back to the shared seam if needed so that the new
     `source term 5` ownership lives only on the selected packet rather than
     on predecessor evidence.
   - Run the focused selected-packet commands immediately and watch the
     intended red failure before changing `src/MLF/Elab/TermClosure.hs`, unless
     step 1 already proved that the exact packet is green without production
     edits and the honest test expectation is therefore already satisfied.
   - Verification:
     run
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneNonupleAliasFrameClearBoundaryExpr"'`
     and confirm the selected test block is the only live red/green target for
     this round.

3. Apply the smallest lawful production correction only after the selected red
   run fails, and only if the step-1 conclusion permits it.
   - Modify:
     `src/MLF/Elab/TermClosure.hs` only.
   - If step 1 plus the red test prove that the selected packet can stay
     honest within the current architecture, limit source edits to the
     existing shared seam in `src/MLF/Elab/TermClosure.hs`, centered on
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`,
     `hasRetainedChildClearBoundary`, and
     `hasRetainedChildClearBoundaryWithAliasBudget`.
   - The only admissible production move is the exact one-more-shell support
     needed by the selected packet inside the terminal helper, such as raising
     the helper entry from
     `hasRetainedChildClearBoundaryWithAliasBudget source term 4`
     to
     `hasRetainedChildClearBoundaryWithAliasBudget source term 5`,
     while keeping the outer
     `hasRetainedChildAliasBoundary v body 2 =`
     entry budget unchanged.
   - Preserve the decrementing recursive walk, the final
     `hasRetainedChildClearBoundary` surface, and the admitted same-lane
     retained-child route/guard cluster unchanged.
   - Do not edit `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback.hs`,
     `src/MLF/Elab/Run/Scope.hs`,
     `src/MLF/Elab/Pipeline.hs`, or
     `src-public/MLF/Pipeline.hs`.
   - Do not add
     `hasRetainedChildAliasBoundary v body 3 =`,
     `hasRetainedChildClearBoundaryWithAliasBudget source term 6`,
     new route arms,
     new candidate ranking,
     new search loops,
     new retained-child owners,
     cyclic or multi-SCC behavior,
     equi-recursive reasoning,
     fallback widening, or
     a second interface.
   - If the selected packet still requires any out-of-slice file, do not
     widen the round in place; leave production unchanged and carry forward the
     blocker characterization from steps 1-2 instead.
   - Verification:
     re-run the selected `sameLaneNonupleAliasFrameClearBoundaryExpr` tests
     and use `rg -n` to confirm that the exact selected mechanism marker is
     present while the forbidden outer/helper widening markers remain absent.

4. Re-green the selected slice while keeping predecessor truth read-only.
   - Modify:
     `orchestrator/rounds/round-190/implementation-notes.md` only if needed;
     otherwise no further files.
   - Make only the selected
     `sameLaneNonupleAliasFrameClearBoundaryExpr`
     tests green.
   - Replay the accepted octuple / septuple / sextuple / quintuple /
     quadruple / triple / double / alias packets as read-only predecessor
     evidence only, so the round proves whether the new slice preserves
     continuity or honestly stops at the accepted boundary.
   - Run one fresh deeper read-only `cabal repl mlf2-test` probe beyond the
     selected nonuple packet. If a production change lands, the selected
     packet may turn green, but the next deeper control must still fail closed
     to prove the round stayed packet-bounded.
   - In `test/PipelineSpec.hs`, ensure the selected nonuple guard owns the
     exact boundary read for this round only:
     if the positive path lands, it must own the selected
     `hasRetainedChildClearBoundaryWithAliasBudget source term 5` marker,
     still forbid
     `hasRetainedChildClearBoundaryWithAliasBudget source term 6`,
     still forbid
     `hasRetainedChildAliasBoundary v body 3 =`,
     and still show the same retained-child / clear-boundary seam;
     if the blocker path remains honest, it must own the current
     `source term 4` / no `source term 5` stop without moving that ownership
     back onto predecessor packets.
   - Update `orchestrator/rounds/round-190/implementation-notes.md` only if
     needed, and if written record one packet-bounded result only:
     either
     `sameLaneNonupleAliasFrameClearBoundaryExpr preserved via exact terminal-helper alias budget 5`
     or
     `sameLaneNonupleAliasFrameClearBoundaryExpr remains blocked at the current bounded helper`.
   - Do not extrapolate this one packet into general same-lane readiness,
     broader `P3` / `P4` / `P6` closure, or repo-level readiness.
   - Verification:
     re-run the selected nonuple match, replay the predecessor matches, and
     re-run the fresh deeper read-only probe to confirm the next packet still
     fails closed after the selected slice is settled.

5. Run the focused and full verification gates.
   - Write targets: none.
   - Re-run the focused selected-packet commands and the accepted predecessor
     controls.
   - Recheck that no forbidden file drift entered pipeline facades, fallback
     files, scope files, adjacent research controls, `test/Main.hs`, or Cabal
     wiring.
   - Re-run `git diff --check`.
   - Because the round will touch `test/` and may touch `src/`, finish with
     `cabal build all && cabal test`.
   - Verification:
     run the roadmap-identity checks, diff-scope guard, focused same-lane
     test/probe set, mechanism-marker grep, `git diff --check`, and the full
     repo gate.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `rg -n 'roadmap_id|roadmap_revision|roadmap_dir' orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/rounds/round-190/selection.md`
- `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let octuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "bud" (EVar "tip") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u")))))))))))
let nonuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "bud" (EVar "tip") (ELet "seed" (EVar "bud") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "seed")) (EVar "u"))))))))))))
let decuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "bud" (EVar "tip") (ELet "seed" (EVar "bud") (ELet "sprout" (EVar "seed") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "sprout")) (EVar "u")))))))))))))
print (runPipelineElab Set.empty octuple)
print (runPipelineElabChecked Set.empty octuple)
print (runPipelineElab Set.empty nonuple)
print (runPipelineElabChecked Set.empty nonuple)
print (runPipelineElab Set.empty decuple)
print (runPipelineElabChecked Set.empty decuple)
:quit
EOF`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneNonupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneOctupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSextupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuintupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
- `rg -n 'sameLaneNonupleAliasFrameClearBoundaryExpr|sameLaneOctupleAliasFrameClearBoundaryExpr|hasRetainedChildAliasBoundary v body [234] =|hasRetainedChildClearBoundaryWithAliasBudget source term [456]' test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs src/MLF/Elab/TermClosure.hs`
- `git diff --check`
- `python3 - <<'PY'
import subprocess
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/PipelineSpec.hs',
    'orchestrator/rounds/round-190/implementation-notes.md',
    'orchestrator/rounds/round-190/plan.md',
}
changed = {
    line.strip()
    for line in subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines()
    if line.strip() and line.strip() != 'orchestrator/state.json'
}
extra = sorted(changed - allowed)
if extra:
    raise SystemExit('forbidden diff drift: ' + ', '.join(extra))
print('ROUND_190_DIFF_SCOPE_OK')
PY`
- `cabal build all && cabal test`
