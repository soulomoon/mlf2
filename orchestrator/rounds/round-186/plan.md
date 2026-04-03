# Round 186 Plan

- Round: `round-186`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, exact `sameLaneQuintupleAliasFrameClearBoundaryExpr` slice only, systematic-debugging-first, TDD-first, non-widening, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded item-5 slice: the adjacent same-lane
retained-child follow-on packet
`sameLaneQuintupleAliasFrameClearBoundaryExpr`.

Accepted `round-181` through `round-185` are bounded predecessor truth only.
For this lane specifically, accepted `round-185` settled
`sameLaneQuadrupleAliasFrameClearBoundaryExpr` as honest only via the shared
`src/MLF/Elab/TermClosure.hs` alias-boundary seam with
`hasRetainedChildAliasBoundary v body 2 =`, the terminal
`hasRetainedChildClearBoundary` rule, and an explicit source guard that still
forbids `hasRetainedChildAliasBoundary v body 3 =`.

This round must now determine whether one more same-lane local alias binder
beyond the accepted quadruple packet remains honest on
`runPipelineElab` / `runPipelineElabChecked`, whether the exact selected packet
earns only one more bounded `TermClosure` alias-shell step, or whether the
current architecture honestly stops at the accepted quadruple case. If
production changes are justified, they must start with a failing focused
quintuple test and land only the smallest lawful `TermClosure` plus
selected-packet test correction. No route-family widening, fallback-core edit,
pipeline-facade edit, cyclic or multi-SCC behavior, equi-recursive reasoning,
fallback widening, or second-interface work is authorized.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-186/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-186/selection.md` is the round input and must
  remain untouched.
- `orchestrator/rounds/round-186/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan and an optional
  `implementation-notes.md` only.

Accepted predecessor packets remain read-only evidence only:

- `round-181`: exact `C1` non-local authoritative packet only
- `round-182`: `sameLaneAliasFrameClearBoundaryExpr` only
- `round-183`: `sameLaneDoubleAliasFrameClearBoundaryExpr` only
- `round-184`: `sameLaneTripleAliasFrameClearBoundaryExpr` only
- `round-185`: `sameLaneQuadrupleAliasFrameClearBoundaryExpr` only

The only live packet for this round is:

- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  new exact packet `sameLaneQuintupleAliasFrameClearBoundaryExpr`
- `test/PipelineSpec.hs`
  the matching
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`
  authoritative-entrypoint assertion and one focused `TermClosure`
  source/mechanism guard
- `src/MLF/Elab/TermClosure.hs`
  only if the investigation proves that the exact selected packet can stay
  honest through one more bounded alias-shell step inside the already-admitted
  shared seam

Adjacent read-only controls may be inspected and rerun, but they are not
writable slice targets for this round:

- `sameLaneAliasFrameClearBoundaryExpr`,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`,
  `sameLaneTripleAliasFrameClearBoundaryExpr`, and
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr` in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` and
  `test/PipelineSpec.hs`
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
  `isAliasFrameRhs`, and
  `isClearBoundaryRetainedChildRhs`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  only for:
  `spec`,
  `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
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
- `orchestrator/rounds/round-186/implementation-notes.md`
  only if the implementer needs a round-local record of the exact bounded
  result

Do not modify:

- `orchestrator/rounds/round-186/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-186/review.md`
- `orchestrator/rounds/round-186/merge.md`
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
- the alias / double-alias / triple-alias / quadruple-alias predecessor
  assertions in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- the alias / double-alias / triple-alias / quadruple-alias predecessor
  assertions in `test/PipelineSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

This round must stay inside the exact
`sameLaneQuintupleAliasFrameClearBoundaryExpr`
packet only. Do not reopen the accepted alias, double-alias, triple-alias, or
quadruple-alias packets; do not reopen the `C1` packet, the `P5` control row,
the negative-family rows, the item-3 route / guard contract, or any broader
item-5 aggregation artifact.

## Sequential Plan

1. Reproduce and localize the selected quintuple packet before proposing any
   fix; modify no files in this step.
   - Probe the current baseline directly in `cabal repl mlf2-test` with the
     exact selected surface term: the current
     `sameLaneQuadrupleAliasFrameClearBoundaryExpr` shape plus one additional
     same-lane alias binder between `deep` and the final retained-child
     consumer, for example
     `ELet "tail" (EVar "deep") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tail")) (EVar "u"))`.
   - Re-run the accepted
     `sameLaneQuadrupleAliasFrameClearBoundaryExpr`
     packet and the adjacent triple / double / alias controls as read-only
     predecessor evidence only, so the round can distinguish a selected-packet
     issue from a regression in already-accepted packets.
   - Inspect the selected authoritative-preservation path in
     `src/MLF/Elab/TermClosure.hs`:
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`,
     `hasRetainedChildClearBoundary`,
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
     it needs one more exact alias-shell step inside
     `hasRetainedChildAliasBoundary`, or it cannot stay honest without
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

2. Add the failing focused tests first and keep them bounded to the selected
   packet only.
   - In
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     add
     `sameLaneQuintupleAliasFrameClearBoundaryExpr`
     and exactly two selected-packet assertions, one for
     `runPipelineElab` and one for
     `runPipelineElabChecked`.
   - If step 1 proves the packet still earns narrow success inside the current
     architecture, make those two new research assertions reuse
     `expectExactRetainedChildAuthoritativeOutput`
     with the same exact two-forall recursive-arrow expectation already used
     for the accepted alias / double-alias / triple-alias / quadruple-alias
     packets.
   - If step 1 proves the packet is blocker-shaped on the current baseline,
     encode that exact blocker shape in those two research assertions instead
     of leaving a vague success or generic `containsMu` claim; keep any tiny
     new expectation helper local to this same file only.
   - In `test/PipelineSpec.hs`, add one exact
     `sameLaneQuintupleAliasFrameClearBoundaryExpr`
     regression on both authoritative entrypoints.
   - Add one focused `TermClosure` source/mechanism guard in
     `test/PipelineSpec.hs` tied to the step-1 conclusion:
     if the packet should pass, make the red test fail against the current
     `hasRetainedChildAliasBoundary v body 2 =` / missing
     `hasRetainedChildAliasBoundary v body 3 =` baseline, and after the fix
     require the exact selected-budget marker while still forbidding any
     `hasRetainedChildAliasBoundary v body 4 =`, fallback marker, or widened
     search token; if the packet should stay blocked, lock the current
     `v body 2 =` / no `v body 3 =` boundary and the selected packet's honest
     authoritative outcome instead.
   - Keep the accepted alias / double-alias / triple-alias / quadruple-alias
     assertions unchanged.
   - Run the focused selected-packet commands immediately and watch the
     intended red failure before changing `src/MLF/Elab/TermClosure.hs`.

3. Apply the smallest lawful production correction only after the selected red
   run fails.
   - If step 1 plus the red test prove that the selected packet can stay
     honest within the current architecture, limit source edits to
     `src/MLF/Elab/TermClosure.hs`, centered on
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`, and
     `hasRetainedChildClearBoundary`.
   - The only admissible production move is the exact one-more-shell support
     needed by the selected packet, such as raising the alias-boundary entry
     budget from `2` to `3` or an equally small bounded branch in the same
     helper. Keep the final `hasRetainedChildClearBoundary` rule, the
     decrementing recursive walk, and the admitted same-lane retained-child
     route/guard cluster unchanged.
   - Do not edit `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback.hs`,
     `src/MLF/Elab/Run/Scope.hs`,
     `src/MLF/Elab/Pipeline.hs`, or
     `src-public/MLF/Pipeline.hs`.
   - If the selected packet still requires any of those out-of-slice files,
     do not widen the round in place; revert to the blocker characterization
     from steps 1-2 and leave production unchanged.
   - Do not add new route arms, new candidate ranking, new search loops,
     new retained-child owners, cyclic or multi-SCC behavior,
     equi-recursive reasoning, fallback widening, or a second interface.

4. Re-green the selected slice while keeping predecessor truth read-only.
   - Make only the selected
     `sameLaneQuintupleAliasFrameClearBoundaryExpr`
     tests green.
   - Replay the accepted quadruple / triple / double / alias packets as
     read-only predecessor evidence only, so the round proves whether the new
     slice preserves continuity or honestly stops at the accepted boundary.
   - In `test/PipelineSpec.hs`, ensure the selected quintuple guard owns the
     exact boundary read for this round only:
     if the positive path lands, it must own the selected
     `hasRetainedChildAliasBoundary v body 3 =` marker, still forbid
     `hasRetainedChildAliasBoundary v body 4 =`, and still show
     `hasRetainedChildClearBoundary` as the terminal bounded rule; if the
     blocker path remains honest, it must own the current
     `v body 2 =` / no `v body 3 =` stop without moving that ownership back
     onto predecessor packets.
   - Update `orchestrator/rounds/round-186/implementation-notes.md` only if
     needed, and if written record one packet-bounded result only:
     either `sameLaneQuintupleAliasFrameClearBoundaryExpr preserved via exact depth-4 alias-boundary support`
     or
     `sameLaneQuintupleAliasFrameClearBoundaryExpr remains blocked at the current bounded helper`.
   - Do not extrapolate this one packet into general same-lane readiness,
     broader `P3` / `P4` / `P6` closure, or repo-level readiness.

5. Run the focused and full verification gates.
   - Re-run the focused selected-packet commands and the accepted predecessor
     controls.
   - Recheck that no forbidden file drift entered pipeline facades, fallback
     files, scope files, adjacent research controls, `test/Main.hs`, or Cabal
     wiring.
   - Re-run `git diff --check`.
   - Because the round will touch `test/` and may touch `src/`, finish with
     `cabal build all && cabal test`.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let quadruple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "deep")) (EVar "u")))))))
let quintuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tail")) (EVar "u"))))))))
print (runPipelineElab Set.empty quadruple)
print (runPipelineElabChecked Set.empty quadruple)
print (runPipelineElab Set.empty quintuple)
print (runPipelineElabChecked Set.empty quintuple)
:quit
EOF`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuintupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
- `rg -n 'sameLaneQuintupleAliasFrameClearBoundaryExpr|sameLaneQuadrupleAliasFrameClearBoundaryExpr|hasRetainedChildAliasBoundary v body [234] =|hasRetainedChildClearBoundary' test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs src/MLF/Elab/TermClosure.hs`
- `git diff --check`
- `python3 - <<'PY'
import subprocess
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/PipelineSpec.hs',
    'orchestrator/rounds/round-186/implementation-notes.md',
    'orchestrator/rounds/round-186/plan.md',
}
changed = {
    line.strip()
    for line in subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines()
    if line.strip() and line.strip() != 'orchestrator/state.json'
}
extra = sorted(changed - allowed)
if extra:
    raise SystemExit('forbidden diff drift: ' + ', '.join(extra))
print('ROUND_186_DIFF_SCOPE_OK')
PY`
- `cabal build all && cabal test`
