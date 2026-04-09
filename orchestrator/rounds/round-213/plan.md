# Round 213 Plan (rev-016 / milestone-3 `sameLaneDoubleAliasFrameClearBoundaryExpr` promotion)

**Goal:** Promote `sameLaneDoubleAliasFrameClearBoundaryExpr` from inherited
guard/control evidence to the next explicit milestone-3 representative
broader-positive packet on both authoritative entrypoints and the matching
authoritative-instantiation guard, while preserving the merged
`sameLaneClearBoundaryExpr` first anchor, the accepted
`sameLaneAliasFrameClearBoundaryExpr` predecessor lane, the merged selected
same-wrapper nested-`forall` win, checked-authoritative parity, the fail-closed
`P2` / `N1` / `N2` / `N6` rows, and the no-triple-alias boundary.

**Architecture:** Start from merged base-branch `HEAD = 9bb2229` and treat the
merged `round-211` / `round-212` mechanism as fixed baseline truth. The default
move is evidence-surface promotion inside
`test/Research/P5ClearBoundarySpec.hs`,
`test/PipelineSpec.hs`, and
`test/ElaborationSpec.hs`:
publish the exact double-alias packet as the next explicit milestone-3 anchor
without reopening the already-merged first anchor or widening into triple /
deeper alias shells. Production edits in
`src/MLF/Elab/Elaborate/Annotation.hs`,
`src/MLF/Elab/Elaborate/Algebra.hs`, or
`src/MLF/Elab/Legacy.hs`
remain fallback-only if the explicit packet still fails and a focused trace
proves the failure sits inside the existing authoritative-instantiation /
post-annotation seam.

**Tech Stack:** Haskell, Cabal, hspec, `./scripts/thesis-conformance-gate.sh`

---

## Boundaries

- Keep this round on the exact packet
  `sameLaneDoubleAliasFrameClearBoundaryExpr` only.
- Preserve `sameLaneClearBoundaryExpr` as the merged first explicit
  milestone-3 anchor; do not reopen it as live debt.
- Preserve `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only; do
  not treat it as the live milestone-3 packet again.
- Preserve the selected same-wrapper nested-`forall` packet as merged-baseline
  success rather than as a reopened target.
- Keep triple / deeper alias shells as inherited continuity evidence only; do
  not promote them onto the research or elaboration milestone-3 surfaces.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as verify-only continuity anchors.
- Do not widen into fallback rescue, a second interface, cyclic or multi-SCC
  search, equi-recursive reasoning, milestone-4 closeout, or any broader
  representative packet.
- Default to test/evidence-surface promotion first; production edits are lawful
  only after the new explicit packet is shown failing and the trace narrows the
  fault to the admitted within-slice seam.

### Task 1: Audit the merged baseline and freeze the exact promotion target

**Files:**
- Verify-only: `orchestrator/state.json`
- Verify-only: `orchestrator/worktrees/round-213/orchestrator/state.json`
- Verify-only:
  `orchestrator/worktrees/round-213/orchestrator/rounds/round-213/selection.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-016/roadmap.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-016/verification.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-016/retry-subloop.md`
- Verify-only: `orchestrator/rounds/round-212/review.md`
- Verify-only: `orchestrator/rounds/round-212/implementation-notes.md`
- Verify-only: `orchestrator/rounds/round-211/review.md`
- Verify-only: `orchestrator/rounds/round-197/review.md`
- Verify-only: `TODO.md`
- Verify-only: `implementation_notes.md`
- Verify-only:
  `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
- Verify-only:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md`
- Verify-only:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] Confirm controller state, round selection, and the active roadmap bundle
      all resolve the same `rev-016` lineage and the extracted item
      `promote-same-lane-double-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`.
- [ ] Re-read merged `round-212` review / notes so the preserved first-anchor
      truth is explicit before any edits:
      `sameLaneClearBoundaryExpr` is already the first explicit milestone-3
      anchor, the promotion was test-only, and the merged mechanism stayed the
      controlling baseline.
- [ ] Re-read accepted `round-211`, accepted `round-197`, and the
      `docs/plans/2026-04-02-*double-alias*` lineage so the predecessor truths
      stay explicit:
      selected same-wrapper nested-`forall` stays green,
      alias-frame support stays predecessor truth only,
      and the historical double-alias narrow-success read is context rather
      than authority to widen milestone-3 automatically.
- [ ] Inspect
      `test/Research/P5ClearBoundarySpec.hs`,
      `test/PipelineSpec.hs`, and
      `test/ElaborationSpec.hs`
      to confirm the live planning gap:
      `test/PipelineSpec.hs` already carries the bounded double-alias packet,
      while the research surface does not yet publish it as the next explicit
      milestone-3 representative anchor and `test/ElaborationSpec.hs` does not
      yet carry the matching exact-edge authoritative-instantiation guard.
- [ ] Confirm the production fallback seam is still limited to
      `Annotation.hs`, `Algebra.hs`, and `Legacy.hs`, with the closed
      continuity anchors untouched and still outside the writable continuation
      surface.

**Verification commands:**

```bash
python3 -m json.tool orchestrator/state.json >/dev/null
python3 -m json.tool orchestrator/worktrees/round-213/orchestrator/state.json >/dev/null
rg -n 'roadmap_id|roadmap_revision|roadmap_dir|milestone-3|direction-3a|promote-same-lane-double-alias-clear-boundary-packet' orchestrator/state.json orchestrator/worktrees/round-213/orchestrator/state.json orchestrator/worktrees/round-213/orchestrator/rounds/round-213/selection.md
rg -n 'sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr' test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git -C orchestrator/worktrees/round-213 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
```

### Task 2: Promote the exact double-alias packet across the three milestone-3 review surfaces

**Files:**
- Modify: `test/Research/P5ClearBoundarySpec.hs`
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] In `test/Research/P5ClearBoundarySpec.hs`, add the explicit
      `sameLaneDoubleAliasFrameClearBoundaryExpr` packet and the matching
      fallback helper so the research surface now names it as the next
      milestone-3 representative broader-positive clear-boundary anchor. Keep
      `sameLaneClearBoundaryExpr` explicitly described as the first anchor,
      keep `sameLaneAliasFrameClearBoundaryExpr` explicitly marked as
      predecessor truth, and keep the selected same-wrapper nested-`forall`
      packet explicitly marked as preserved merged-baseline success.
- [ ] In `test/PipelineSpec.hs`, sharpen the existing
      `sameLaneDoubleAliasFrameClearBoundaryExpr` row into the authoritative
      milestone-3 promotion check for both `runPipelineElab` and
      `runPipelineElabChecked`, while preserving the current recursive-output,
      type-check, and checked-parity substance. Leave
      `sameLaneClearBoundaryExpr` as the merged first anchor, leave
      `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth, and keep the
      triple / deeper alias shell rows as inherited continuity evidence rather
      than reclassifying them as live corpus anchors.
- [ ] In `test/ElaborationSpec.hs`, add the exact-edge authoritative
      instantiation translation guard for
      `sameLaneDoubleAliasFrameClearBoundaryExpr` by cloning the existing
      exact-edge test shape, changing the packet extractor to the
      `k -> hold -> keep -> u` double-alias form, and locking the exact
      `ExpInstantiate ...` witness that the merged baseline actually produces.
      Keep the direct `runPipelineElab` success check and do not widen the
      elaboration surface into generic alias-depth helpers or triple-alias
      coverage.
- [ ] If helper cleanup is needed to keep the three tests readable, keep it
      inside these same three files only; do not add new modules, widen the
      production surface, or move triple / deeper alias shells onto the
      research / elaboration milestone-3 surfaces.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
```

### Task 3: Reopen production only if the explicit packet fails and the trace proves a within-slice seam

**Files:**
- Modify only if Task 2 fails: `src/MLF/Elab/Elaborate/Annotation.hs`
- Modify only if Task 2 fails: `src/MLF/Elab/Elaborate/Algebra.hs`
- Modify only if Task 2 fails and a focused trace proves a legacy translation gap: `src/MLF/Elab/Legacy.hs`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`

- [ ] If the newly explicit double-alias rows fail during authoritative
      instantiation translation, localize the repair to
      `src/MLF/Elab/Elaborate/Annotation.hs` around
      `reifyInst`,
      `inferAuthoritativeInstArgs`,
      `authoritativeTargetType`,
      `reifyTraceBinderInstArgs`, and the
      `expInstantiateArgsToInstNoFallback` handoff so the exact double-alias
      packet gets the authoritative translation it already deserves without
      reopening quantified crossings or deeper alias shells.
- [ ] If the failure instead appears after annotation translation, localize the
      repair to the existing `src/MLF/Elab/Elaborate/Algebra.hs` application /
      let handoff seam around
      `funInstRecovered`,
      `argInstFromFun`,
      `fApp`,
      `scheme`,
      `rhsAbs0`, and
      `rhsAbs`
      so the exact double-alias packet survives the admitted authoritative
      post-annotation seam on both entrypoints.
- [ ] Touch `src/MLF/Elab/Legacy.hs` only if the failing trace proves
      `expInstantiateArgsToInstNoFallback` or `instAppsFromTypes` is still the
      exact blocker. Do not reopen
      `TermClosure.hs`,
      the pipeline / public facades,
      result-type fallback,
      cyclic search,
      multi-SCC search,
      fallback rescue,
      or a second interface as substitute continuation surfaces.
- [ ] After any repair, rerun the exact packet trio first and stop if the fix
      changes the merged first anchor, alias-frame predecessor truth, selected
      same-wrapper nested-`forall` success, checked-authoritative parity, or
      the fail-closed `P2` / `N1` / `N2` / `N6` guards.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary"'
```

### Task 4: Re-run the protection matrix, diff-scope checks, and full gates

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

- [ ] Re-run the exact packet evidence proving
      `sameLaneDoubleAliasFrameClearBoundaryExpr` succeeds honestly on both
      `runPipelineElab` and `runPipelineElabChecked`, and that the exact-edge
      authoritative-instantiation guard is green.
- [ ] Re-run the preserved merged-baseline wins:
      `sameLaneClearBoundaryExpr`,
      `sameLaneAliasFrameClearBoundaryExpr`,
      selected same-wrapper nested-`forall`,
      checked-authoritative parity,
      `BUG-2026-02-06-002`,
      `BUG-2026-02-17-002`,
      the correct semantic `g g` failure,
      representative let-polymorphism rows,
      nested-let fail-fast rows,
      Phi alignment,
      thesis alignment, and
      the frozen parity baseline.
- [ ] Confirm the implementation-owned diff stays inside
      `test/Research/P5ClearBoundarySpec.hs`,
      `test/PipelineSpec.hs`,
      `test/ElaborationSpec.hs`,
      and only the conditional production fallback files if Task 3 proved they
      were necessary. Closed continuity anchors must remain untouched.
- [ ] Confirm triple / deeper alias shells stay continuity-only:
      no new research-surface or elaboration-surface promotion beyond the exact
      double-alias packet, and no wording that upgrades triple / deeper alias
      rows into live milestone-3 closure.
- [ ] Re-run `./scripts/thesis-conformance-gate.sh` and
      `cabal build all && cabal test`.
      Full-gate expectation on this merged baseline remains
      `1341 examples, 0 failures`.

**Verification commands:**

```bash
git -C orchestrator/worktrees/round-213 diff --check
git -C orchestrator/worktrees/round-213 diff --name-only -- test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Legacy.hs
git -C orchestrator/worktrees/round-213 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
rg -n 'sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneTripleAliasFrameClearBoundaryExpr|sameLaneQuadrupleAliasFrameClearBoundaryExpr' test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'
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

- `sameLaneDoubleAliasFrameClearBoundaryExpr` is explicitly named as the next
  milestone-3 representative packet in
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`.
- `sameLaneClearBoundaryExpr` remains the merged first explicit milestone-3
  anchor.
- `sameLaneAliasFrameClearBoundaryExpr` remains preserved predecessor truth
  only.
- The selected same-wrapper nested-`forall` packet remains preserved
  merged-baseline success.
- Checked-authoritative parity and the fail-closed `P2` / `N1` / `N2` / `N6`
  guard cluster remain green.
- Triple / deeper alias shells remain continuity-only rather than newly
  promoted milestone-3 anchors.
- No closed continuity anchor or out-of-slice path is touched unless Task 3
  proves the conditional production seam is necessary.
