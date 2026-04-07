# Round 207 Plan

- Round: `round-207`
- Roadmap:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
  / `rev-001`
- Milestone: `milestone-2`
- Direction:
  `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
- Extracted item:
  `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
- Retry context: same branch/worktree, step back from implement to `plan`
  because the prior plan only opened candidate admission and did not address
  the still-blocking scope/generalization-owner mismatch
- Worker mode: `none`
- Execution shape: serial, same round worktree only, no worker fan-out, no
  `worker-plan.json`, and no concurrent `cabal` jobs

## Objective

Repair the selected same-wrapper nested-`forall` slice by aligning fallback
target selection with the scope/generalization owner that actually owns the
recursive node.

The next implement attempt must be honest about what is blocked:

- The current red reproducer is not blocked by missing same-wrapper candidate
  admission anymore.
- It is blocked because the selected packet still generalizes through the old
  local `scopeRoot` / owner path, while the recursive node is only valid under
  the nested owner path (`GenRef 4`, not `TypeRef 3`).
- Therefore the next change must pair `targetC` selection with the matching
  generalization scope/owner. Merely widening `boundVarTarget`,
  preferring `sameLaneLocalRetainedChildTarget`, or walking to a recursive
  node is not enough.

This round still must not claim broader-positive frontier closure, must not
touch pipeline/public threading files unless an explicit proof shows the seam
escapes `direction-2a`, and must keep the preserved negative rows closed.

## Current Recovery Diagnosis

The current round outputs are salvageable and should be treated as the
starting point, not discarded blindly:

- `test/PipelineSpec.hs` now isolates the selected packet with
  `extractSelectedBodyApp` and `wireSameLaneLocalRoot`.
- `test/Research/P5ClearBoundarySpec.hs` mirrors that same rewired packet in
  `fallbackType`.
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` now admits the same-wrapper
  nested-`forall` candidate and prefers that packet more aggressively.

Focused verification still fails with:

- `SchemeFreeVars (NodeId {getNodeId = 3}) ["t39"]`

The implement-stage recovery notes narrow the blocker further:

- fallback still ends at the old local `scopeRoot` / owner path;
- the recursive node can be exposed, but it generalizes only when owned under
  the nested `GenRef 4` path, not under local `TypeRef 3`;
- `src/MLF/Elab/TermClosure.hs` was never reached, so speculative
  term-closure edits are not yet justified.

The next attempt must therefore solve or disprove the
`target-selection + generalization-scope` seam inside the frozen slice.

## Locked Round Context

- Stage: `plan`
- Active selection input:
  `orchestrator/rounds/round-207/selection.md`
- Active roadmap pointer comes from `orchestrator/state.json` and must remain
  untouched.
- Current worktree is already dirty. Do not revert controller-owned or
  recovery-owned changes unless a specific hunk blocks the new plan.

Relevant existing modified files in the round worktree:

- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `test/PipelineSpec.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `orchestrator/rounds/round-207/implementation-notes.md`
- `orchestrator/rounds/round-207/plan.md`
- `orchestrator/rounds/round-207/selection.md`
- `orchestrator/state.json` (controller-owned; do not edit)

## Authorized Write Scope

Implementation-owned writes remain bounded to the accepted milestone-1 slice:

- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  inside `computeResultTypeFallbackCore`, specifically the locals around
  `scopeRootPre`, `scopeRootPost`, `scopeRoot`,
  `retainedChildPresolutionView`, `boundVarTargetRoot`,
  `boundHasForallFrom`, `boundVarTarget`, `retainedRecursiveTarget`,
  `sameLaneLocalRetainedChildTarget`, `keepTargetFinal`, `targetC`, and the
  final `generalizeWithPlan` call
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
  only if a strictly mechanical facade update is forced by the Core change in
  `computeResultTypeFallbackWithView` or `computeBodyResultType`
- `src/MLF/Elab/TermClosure.hs`
  only inside `preserveRetainedChildAuthoritativeResult`,
  `preserveRetainedChildAliasBoundary`,
  `hasRetainedChildAliasBoundary`, and
  `hasRetainedChildClearBoundaryWithAliasBudget`
- `test/PipelineSpec.hs`
  only for the selected same-wrapper nested-`forall` reproducer, its source
  guards, and the preserved negative/control expectations around that seam
- `test/Research/P5ClearBoundarySpec.hs`
  only for `fallbackType`, `extractSelectedBodyApp`,
  `wireSameLaneLocalRoot`, and the exact internal-only versus
  authoritative-entrypoint expectations

Read-only continuity anchors for this retry:

- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`

Do not widen into:

- `src/MLF/Elab/Run/Scope.hs`
- any `src/MLF/Constraint/**` plan-builder / reify / target-selection files
- any pipeline/public threading file named above
- any docs/control-plane file other than this round-local `plan.md`

If the only truthful fix requires changing one of those excluded areas, stop
the attempt and return explicit proof that the scope/generalization seam
exceeds this extracted item instead of widening silently.

## Sequential Plan

1. Preserve the current red reproducer and verify that the blocker is still
   owner/scope alignment, not target admission.
   - Files:
     `test/PipelineSpec.hs`,
     `test/Research/P5ClearBoundarySpec.hs`
   - Keep the current rewired helpers
     `extractSelectedBodyApp`,
     `wireSameLaneLocalRoot`,
     and the `rtcBaseConstraint` refresh as the round's reproducer surface.
     Do not broaden the corpus or add new packet families.
   - Keep the selected fallback expectations at `containsMu True` and keep the
     authoritative entrypoint expectation fail-closed for this round.
   - Refresh the `test/PipelineSpec.hs` source guard so it no longer checks
     only candidate admission; it must check that
     `computeResultTypeFallbackCore` derives the selected target and the
     generalization scope from the same local proof, rather than still
     calling `generalizeWithPlan` through the old unqualified `scopeRoot`
     path.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "reports PhiTranslatabilityError at pipeline entrypoints while the nested-forall preservation stays internal-only in this round"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the P5 guard cluster wired through boundHasForallFrom and authoritative preservation"'`
   - Expected state before step 2: the focused fallback test is still red with
     `SchemeFreeVars (NodeId {getNodeId = 3}) ["t39"]`.

2. Rework `computeResultTypeFallbackCore` so the selected same-wrapper packet
   chooses both `targetC` and the matching generalization scope/owner.
   - Files:
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
     and `src/MLF/Elab/Run/ResultType/Fallback.hs` only if a mechanical
     facade adjustment is forced
   - Keep the round inside `computeResultTypeFallbackCore`; do not widen the
     module surface.
   - Replace the current "ownerless recursive target" direction with a single
     same-wrapper proof that carries:
     the retained child,
     the selected child target/body root,
     the chosen `targetC`,
     and the generalization scope/owner that actually owns that target.
   - Revisit these locals together rather than independently:
     `scopeRootPre`,
     `scopeRootPost`,
     `scopeRoot`,
     `boundHasForallFrom`,
     `boundVarTarget`,
     `retainedRecursiveTarget`,
     `sameLaneLocalRetainedChildTarget`,
     `keepTargetFinal`,
     `targetC`,
     and the final `generalizeWithPlan`.
   - Make `boundHasForallFrom` and the same-wrapper candidate filter compare
     nested `TyForall` / scheme-root ownership against the aligned owner for
     the selected packet, not only against the old local `scopeRoot`.
   - Feed `generalizeWithPlan` the aligned generalization root for the chosen
     same-wrapper packet. If the target's recursive node is only valid under a
     nested `GenRef`, use that proven owner; do not keep `targetC` on the new
     packet while still generalizing under the old `TypeRef` lane.
   - Either delete `retainedRecursiveTarget` or demote it behind the same
     owner proof. It must not select a `TyMu` node without also proving the
     corresponding scope/owner alignment.
   - Keep the already-settled neighboring lanes behavior-stable:
     `rootLocalMultiInst`,
     `rootLocalInstArgMultiBase`,
     `rootLocalInstArgSingleBase`,
     `rootLocalEmptyCandidateSchemeAliasBaseLike`,
     `rootLocalSingleBase`,
     `rootLocalSchemeAliasBaseLike`,
     and `rootNonLocalSchemeAliasBaseLike`.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
     `rg -n 'scopeRootPre|scopeRootPost|boundHasForallFrom|sameLaneLocalRetainedChildTarget|generalizeWithPlan' src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
   - Decision gate:
     if the focused fallback test still fails with the same `SchemeFreeVars`
     shape after the aligned-scope rewrite, and the remaining truthful fix
     would require `Run/Scope.hs`, generalization-plan internals, or any
     pipeline/public threading file, stop the round there and return that
     proof instead of guessing.

3. Touch `TermClosure.hs` only after fallback owner/scope alignment is green,
   and only if the selected packet still loses recursive structure in the
   round-owned internal continuity seam.
   - Files:
     `src/MLF/Elab/TermClosure.hs`
   - Do not start with `TermClosure.hs`; it is downstream of step 2.
   - If step 2 makes the selected fallback packet recursive, rerun the focused
     research/pipeline surfaces before editing term closure.
   - Only if the recursive scheme now exists but the retained-child internal
     continuity still collapses, confine the follow-up change to
     `preserveRetainedChildAuthoritativeResult`,
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`, and
     `hasRetainedChildClearBoundaryWithAliasBudget`.
   - Keep alias budgets unchanged, do not add new helper seams, and do not
     use `TermClosure.hs` as a substitute for unresolved owner/scope bugs in
     fallback.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the P5 guard cluster wired through boundHasForallFrom and authoritative preservation"'`
     `rg -n 'preserveRetainedChildAuthoritativeResult|preserveRetainedChildAliasBoundary|hasRetainedChildAliasBoundary|hasRetainedChildClearBoundaryWithAliasBudget' src/MLF/Elab/TermClosure.hs`

4. Re-green the bounded slice, prove no silent widening happened, then run the
   full milestone-2 gate.
   - Files: modify no additional files in this step
   - Re-run the focused commands from steps 1-3 in serial order.
   - Confirm the result is still honest:
     the selected same-wrapper fallback packet is recursive,
     the preserved negative rows remain fail-closed,
     the clear-boundary controls remain recursive,
     and the authoritative entrypoints may remain fail-closed in this round
     as long as the fix stayed inside the internal mechanism slice.
   - Confirm `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`, and
     `src-public/MLF/Pipeline.hs` are untouched.
   - Only after the focused slice is green and still bounded, run the repo
     verification contract for milestone-2.
   - Verification:
     `git diff --check`
     `git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs`
     `cabal build all`
     `cabal test`

## Verification Contract For This Retry

The retry is successful only if all of the following are true:

- The same-wrapper nested-`forall` focused fallback test passes without
  `SchemeFreeVars`.
- The fix changes the selected packet by aligning target selection and
  generalization owner/scope, not by broadening candidate admission alone.
- The preserved negative/control rows remain unchanged in meaning.
- No pipeline/public threading file is edited in this retry.
- `git diff --check`, `cabal build all`, and `cabal test` all pass if the
  retry lands code/test changes.

If the retry instead proves that the required owner/scope seam lives outside
the authorized fallback/term-closure slice, the honest outcome is to stop and
report that boundary rather than widening `direction-2a` silently.
