# Round 212 Plan (rev-015 / milestone-3 `sameLaneClearBoundaryExpr` promotion)

**Goal:** Promote `sameLaneClearBoundaryExpr` from merged-baseline
carry-forward control to the first explicit milestone-3 representative
broader-positive packet on both authoritative entrypoints, while preserving
the merged `round-211` same-wrapper nested-`forall` win, the accepted
`round-197` alias-frame predecessor lane, checked-authoritative parity, and
the current fail-closed quantified guards.

**Architecture:** Start from merged commit `5b775b2` and treat the merged
`round-211` payload as the fixed baseline. The default move is evidence-surface
promotion inside
`test/Research/P5ClearBoundarySpec.hs`,
`test/PipelineSpec.hs`, and
`test/ElaborationSpec.hs`:
make the exact clear-boundary packet explicit and reviewable without
relitigating the merged mechanism. Production edits in
`src/MLF/Elab/Elaborate/Annotation.hs`,
`src/MLF/Elab/Elaborate/Algebra.hs`, or
`src/MLF/Elab/Legacy.hs`
are conditional only if the newly explicit packet fails and the failure is
proven to live inside the existing authoritative-instantiation /
post-annotation handoff seam.

**Tech Stack:** Haskell, Cabal, hspec, `./scripts/thesis-conformance-gate.sh`

---

## Boundaries

- Keep this round on the exact packet `sameLaneClearBoundaryExpr` only.
- Treat `sameLaneAliasFrameClearBoundaryExpr` as accepted predecessor truth
  only, and treat the selected same-wrapper nested-`forall` packet as
  preserved merged-baseline success rather than a reopened live target.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as verify-only continuity anchors.
- Do not widen into milestone-4 closeout, new representative packets, cyclic
  or multi-SCC search, fallback rescue, equi-recursive reasoning, or a second
  interface.
- Default to test/evidence-surface promotion first; only reopen production
  code if the exact packet still fails after the explicit promotion tests are
  in place.

### Task 1: Audit the merged baseline and freeze the exact promotion target

**Files:**
- Verify-only: `orchestrator/state.json`
- Verify-only: `orchestrator/worktrees/round-212/orchestrator/state.json`
- Verify-only: `orchestrator/worktrees/round-212/orchestrator/rounds/round-212/selection.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-015/roadmap.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-015/verification.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-015/retry-subloop.md`
- Verify-only: `orchestrator/rounds/round-211/review.md`
- Verify-only: `orchestrator/rounds/round-211/implementation-notes.md`
- Verify-only: `orchestrator/rounds/round-197/review.md`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] Confirm controller state, round selection, and the active roadmap bundle
      all resolve the same `rev-015` lineage and the extracted item
      `promote-same-lane-clear-boundary-packet-to-first-explicit-milestone-3-representative-corpus-anchor`.
- [ ] Re-read the merged `round-211` review/notes and the accepted
      `round-197` settlement so the preserved truths are explicit before any
      implementation work:
      selected same-wrapper nested-`forall` success stays green,
      alias-frame support stays predecessor truth only,
      and the merged baseline remains the lawful starting point.
- [ ] Inspect
      `test/Research/P5ClearBoundarySpec.hs`,
      `test/PipelineSpec.hs`, and
      `test/ElaborationSpec.hs`
      to confirm the live planning gap: `sameLaneClearBoundaryExpr` already
      exists and passes as control evidence, but only the alias-frame and
      selected nested-`forall` rows are currently named as explicit packet
      authorities.
- [ ] Confirm the closed continuity anchors remain untouched and outside the
      writable continuation surface before any edits.

**Verification commands:**

```bash
python3 -m json.tool orchestrator/state.json >/dev/null
python3 -m json.tool orchestrator/worktrees/round-212/orchestrator/state.json >/dev/null
rg -n 'roadmap_id|roadmap_revision|roadmap_dir|milestone-3|direction-3a|promote-same-lane-clear-boundary-packet' orchestrator/state.json orchestrator/worktrees/round-212/orchestrator/state.json orchestrator/worktrees/round-212/orchestrator/rounds/round-212/selection.md
rg -n 'sameLaneClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr' test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git -C orchestrator/worktrees/round-212 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
```

### Task 2: Make `sameLaneClearBoundaryExpr` the explicit milestone-3 representative packet across the three review surfaces

**Files:**
- Modify: `test/Research/P5ClearBoundarySpec.hs`
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] In `test/Research/P5ClearBoundarySpec.hs`, rename the generic
      clear-boundary control assertions so the packet is explicitly named
      `sameLaneClearBoundaryExpr` and described as the first milestone-3
      representative broader-positive anchor.
- [ ] Keep the alias-frame row explicitly marked as preserved predecessor
      truth and keep the selected same-wrapper nested-`forall` row explicitly
      marked as preserved merged-baseline success; do not let either row turn
      back into the live packet.
- [ ] In `test/PipelineSpec.hs`, promote the two generic
      `"same-lane retained-child exact packet ..."` rows to explicit
      `sameLaneClearBoundaryExpr` authoritative-entrypoint checks. Preserve the
      existing substance: both `runPipelineElab` and `runPipelineElabChecked`
      must succeed, stay recursive, and not collapse to
      `forall a. a -> a`.
- [ ] In `test/ElaborationSpec.hs`, rename the exact-edge authoritative
      instantiation guard to `sameLaneClearBoundaryExpr`, preserving the same
      expression shape, `ExpInstantiate [NodeId 31]` expectation, and
      `runPipelineElab` success check.
- [ ] If local test helpers or names need cleanup, keep that cleanup inside
      the same three files; do not widen the production surface or add new
      modules.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
```

### Task 3: Repair the exact packet only if the explicit promotion tests expose a real within-slice failure

**Files:**
- Modify only if Task 2 fails: `src/MLF/Elab/Elaborate/Annotation.hs`
- Modify only if Task 2 fails: `src/MLF/Elab/Elaborate/Algebra.hs`
- Modify only if Task 2 fails and a focused trace proves a legacy translation gap: `src/MLF/Elab/Legacy.hs`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`

- [ ] If the newly explicit `sameLaneClearBoundaryExpr` rows fail at
      authoritative-instantiation translation, localize the repair to the
      existing `Annotation.hs` seam around
      `inferAuthoritativeInstArgs`,
      `authoritativeTargetType`,
      `reifyTraceBinderInstArgs`, and the `reifyInst` authoritative-translation
      error path so the exact clear-boundary packet inherits the merged
      `round-211` behavior without reopening quantified crossings.
- [ ] If the failure instead appears after instantiation translation, localize
      the repair to the merged `round-211` `Algebra.hs` handoff seam
      (`funInstRecovered`,
      `argInstFromFun`,
      `fApp`,
      `scheme`,
      `rhsAbs0`,
      `rhsAbs`)
      so the same exact packet survives the post-annotation authoritative
      handoff on both entrypoints.
- [ ] Touch `Legacy.hs` only if the failing trace proves
      `expInstantiateArgsToInstNoFallback` /
      `instAppsFromTypes`
      still blocks the exact packet. Do not open
      `TermClosure.hs`, pipeline/public facades, or fallback core as substitute
      continuation surfaces.
- [ ] After any repair, re-run the exact packet trio first and stop if the fix
      changes alias-frame predecessor truth, selected same-wrapper
      nested-`forall` success, checked-authoritative parity, or any fail-closed
      quantified guard.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the P5 guard cluster wired through boundHasForallFrom and authoritative preservation"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary"'
```

### Task 4: Re-run the merged-baseline protection matrix and full gates

**Files:**
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `test/FrozenParitySpec.hs`
- Verify-only: `test/Phi/AlignmentSpec.hs`
- Verify-only: `test/AlignmentInvariantSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] Re-run the exact packet evidence proving `sameLaneClearBoundaryExpr`
      succeeds honestly on both `runPipelineElab` and
      `runPipelineElabChecked`.
- [ ] Re-run the merged `round-211` protected wins:
      selected same-wrapper nested-`forall` success,
      checked-authoritative parity,
      `BUG-2026-02-06-002`,
      `BUG-2026-02-17-002`,
      the correct semantic `g g` failure,
      representative let-polymorphism rows,
      nested-let fail-fast rows,
      Phi alignment,
      thesis alignment, and
      the frozen parity baseline.
- [ ] Re-run `./scripts/thesis-conformance-gate.sh` and
      `cabal build all && cabal test`.
- [ ] Confirm the final diff stays inside
      `src/MLF/Elab/Elaborate/Annotation.hs`,
      `src/MLF/Elab/Elaborate/Algebra.hs`,
      `src/MLF/Elab/Legacy.hs`,
      `test/ElaborationSpec.hs`,
      `test/PipelineSpec.hs`, and
      `test/Research/P5ClearBoundarySpec.hs`,
      with the closed continuity anchors still untouched.

**Verification commands:**

```bash
git -C orchestrator/worktrees/round-212 diff --check
git -C orchestrator/worktrees/round-212 diff --name-only
git -C orchestrator/worktrees/round-212 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "redirected let-use sites keep polymorphic schemes"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "let id ="'
cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-001"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V2"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V1"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V4"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Thesis alignment invariants"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'
./scripts/thesis-conformance-gate.sh
cabal build all && cabal test
```

## Acceptance Criteria

- `sameLaneClearBoundaryExpr` is the explicitly named milestone-3
  representative packet in
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`.
- The exact packet succeeds honestly on both
  `runPipelineElab` and `runPipelineElabChecked`.
- The merged `round-211` selected same-wrapper nested-`forall` win remains
  green.
- Accepted `round-197` alias-frame truth remains preserved green and
  packet-bounded.
- Checked-authoritative parity and the fail-closed guard cluster remain green.
- No closed continuity anchor or out-of-slice path is touched.
