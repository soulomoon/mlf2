# Round 219 Plan (rev-024 / milestone-3 `sameLaneOctupleAliasFrameClearBoundaryExpr` promotion)

**Goal:** Promote `sameLaneOctupleAliasFrameClearBoundaryExpr` from inherited
guard/control evidence to the next explicit milestone-3 representative
broader-positive packet on both authoritative entrypoints and the matching
authoritative-instantiation guard, while preserving the merged
`sameLaneClearBoundaryExpr` first anchor, the merged
`sameLaneDoubleAliasFrameClearBoundaryExpr` next anchor, the merged
`sameLaneTripleAliasFrameClearBoundaryExpr` next anchor after that, the merged
`sameLaneQuadrupleAliasFrameClearBoundaryExpr` next anchor after that, the
merged `sameLaneQuintupleAliasFrameClearBoundaryExpr` next anchor after that,
the merged `sameLaneSextupleAliasFrameClearBoundaryExpr` next anchor after
that, the merged `sameLaneSeptupleAliasFrameClearBoundaryExpr` next anchor
after that, the preserved `sameLaneAliasFrameClearBoundaryExpr` predecessor
lane, the merged selected same-wrapper nested-`forall` win,
checked-authoritative parity, the fail-closed `P2` / `N1` / `N2` / `N6`
shell probes, and the bounded rule that nonuple / deeper alias shells stay
outside this extraction.

**Architecture:** Start from merged base-branch `HEAD = 7a127e2` and treat the
merged `round-211` / `round-212` / `round-213` / `round-214` / `round-215` /
`round-216` / `round-217` / `round-218` mechanism as fixed baseline truth.
The default move is evidence-surface promotion inside
`test/Research/P5ClearBoundarySpec.hs`,
`test/PipelineSpec.hs`, and
`test/ElaborationSpec.hs`:
publish the exact octuple-alias packet as the next explicit milestone-3
anchor, prefer sharpening the existing pipeline continuity row rather than
creating a second pipeline packet, and keep nonuple / deeper alias shells
outside this extraction as continuity evidence only. Production edits in
`src/MLF/Elab/Elaborate/Annotation.hs`,
`src/MLF/Elab/Elaborate/Algebra.hs`, or
`src/MLF/Elab/Legacy.hs`
remain fallback-only if the newly explicit packet fails and a focused trace
proves the blocker sits inside the admitted authoritative-instantiation /
post-annotation seam.

**Tech Stack:** Haskell, Cabal, hspec, `./scripts/thesis-conformance-gate.sh`

---

## Boundaries

- Keep this round on the exact packet
  `sameLaneOctupleAliasFrameClearBoundaryExpr` only.
- Preserve `sameLaneClearBoundaryExpr` as the merged first explicit
  milestone-3 anchor; do not reopen it as live debt.
- Preserve `sameLaneDoubleAliasFrameClearBoundaryExpr` as the merged next
  explicit milestone-3 anchor; do not demote or relitigate it.
- Preserve `sameLaneTripleAliasFrameClearBoundaryExpr` as the merged next
  explicit milestone-3 anchor after the double-alias anchor; do not reopen it
  as live debt.
- Preserve `sameLaneQuadrupleAliasFrameClearBoundaryExpr` as the merged next
  explicit milestone-3 anchor after the triple-alias anchor; do not reopen it
  as live debt.
- Preserve `sameLaneQuintupleAliasFrameClearBoundaryExpr` as the merged next
  explicit milestone-3 anchor after the quadruple-alias anchor; do not reopen
  it as live debt.
- Preserve `sameLaneSextupleAliasFrameClearBoundaryExpr` as the merged next
  explicit milestone-3 anchor after the quintuple-alias anchor; do not reopen
  it as live debt.
- Preserve `sameLaneSeptupleAliasFrameClearBoundaryExpr` as the merged next
  explicit milestone-3 anchor after the sextuple-alias anchor; do not reopen
  it as live debt.
- Preserve `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only; do
  not treat it as the live packet again.
- Preserve the selected same-wrapper nested-`forall` packet as merged-baseline
  success rather than as a reopened target.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as verify-only continuity anchors.
- Keep `sameLaneNonupleAliasFrameClearBoundaryExpr` and deeper alias shells
  outside this extraction as inherited continuity evidence only; do not
  promote them onto the research or elaboration milestone-3 surfaces.
- Do not widen into fallback rescue, a second interface, cyclic or multi-SCC
  search, equi-recursive reasoning, milestone-4 closeout, or any broader
  representative packet.
- Default to test/evidence-surface promotion first; production edits are
  lawful only after the explicit octuple-alias packet is shown failing and a
  focused trace narrows the fault to the admitted within-slice seam.

### Task 1: Audit the merged baseline and freeze the exact promotion target

**Files:**
- Verify-only: `orchestrator/state.json`
- Verify-only: `orchestrator/worktrees/round-219/orchestrator/state.json`
- Verify-only:
  `orchestrator/worktrees/round-219/orchestrator/rounds/round-219/selection.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-024/roadmap.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-024/verification.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-024/retry-subloop.md`
- Verify-only: `orchestrator/rounds/round-218/review-record.json`
- Verify-only: `orchestrator/rounds/round-218/merge.md`
- Verify-only: `TODO.md`
- Verify-only: `implementation_notes.md`
- Verify-only:
  `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
- Verify-only:
  `docs/plans/2026-04-08-p5-polymorphism-nested-forall-explicit-boundary-revision-family-final-handoff-binding-one-exact-downstream-consequence-from-the-revised-planning-ledger.md`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] Confirm controller state, canonical round-worktree state, round
      selection, and the active roadmap bundle all resolve the same
      `rev-024` lineage and the extracted item
      `promote-same-lane-octuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`.
- [ ] Re-read merged `round-218` review / merge evidence so the preserved
      baseline is explicit before any edits:
      `sameLaneSeptupleAliasFrameClearBoundaryExpr` is already the merged
      next explicit milestone-3 anchor after the merged sextuple-alias
      anchor, the promotion was test-only, and the milestone-2 production
      mechanism stayed the controlling baseline.
- [ ] Re-read the `rev-024` roadmap / verification / retry bundle and the
      milestone-1 family-contract docs so the preserved frontier stays
      explicit, and treat `rev-022` plus `rev-023` as stale unusable
      recovery publications only:
      `sameLaneClearBoundaryExpr`,
      `sameLaneDoubleAliasFrameClearBoundaryExpr`,
      `sameLaneTripleAliasFrameClearBoundaryExpr`,
      `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
      `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
      `sameLaneSextupleAliasFrameClearBoundaryExpr`, and
      `sameLaneSeptupleAliasFrameClearBoundaryExpr` are merged baseline
      truth,
      `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth only,
      the selected same-wrapper nested-`forall` packet remains merged
      success, and the next still-unpublished packet is the exact
      octuple-alias lane.
- [ ] Inspect
      `test/Research/P5ClearBoundarySpec.hs`,
      `test/PipelineSpec.hs`, and
      `test/ElaborationSpec.hs`
      to confirm the live planning gap:
      `test/PipelineSpec.hs` already carries the bounded
      `sameLaneOctupleAliasFrameClearBoundaryExpr` continuity row plus
      nonuple / deeper continuity guards, while the research surface does
      not yet publish octuple-alias as the next explicit milestone-3 anchor
      and the elaboration surface does not yet carry the matching exact-edge
      authoritative-instantiation guard.
- [ ] Confirm the production fallback seam is still limited to
      `Annotation.hs`, `Algebra.hs`, and `Legacy.hs`, with the closed
      continuity anchors untouched and still outside the writable
      continuation surface.

**Verification commands:**

```bash
python3 -m json.tool orchestrator/state.json >/dev/null
python3 -m json.tool orchestrator/worktrees/round-219/orchestrator/state.json >/dev/null
rg -n 'roadmap_id|roadmap_revision|roadmap_dir|milestone-3|direction-3a|promote-same-lane-octuple-alias-clear-boundary-packet' orchestrator/state.json orchestrator/worktrees/round-219/orchestrator/state.json orchestrator/worktrees/round-219/orchestrator/rounds/round-219/selection.md
rg -n 'Revision: `rev-024`|HEAD = 7a127e2|sameLaneSeptupleAliasFrameClearBoundaryExpr|sameLaneOctupleAliasFrameClearBoundaryExpr|rev-022|rev-023' orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-024/roadmap.md orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-024/verification.md orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-024/retry-subloop.md
rg -n 'sameLaneClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr|sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneTripleAliasFrameClearBoundaryExpr|sameLaneQuadrupleAliasFrameClearBoundaryExpr|sameLaneQuintupleAliasFrameClearBoundaryExpr|sameLaneSextupleAliasFrameClearBoundaryExpr|sameLaneSeptupleAliasFrameClearBoundaryExpr|sameLaneOctupleAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr|selected same-wrapper nested-forall' test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git -C orchestrator/worktrees/round-219 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
```

### Task 2: Promote the exact octuple-alias packet across the three milestone-3 review surfaces

**Files:**
- Modify: `test/Research/P5ClearBoundarySpec.hs`
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] In `test/Research/P5ClearBoundarySpec.hs`, add
      `sameLaneOctupleAliasFrameClearBoundaryExpr` plus
      `sameLaneOctupleAliasFrameClearBoundaryFallbackType`, then publish the
      two missing explicit research rows:
      one fallback `containsMu` anchor row and one
      `runPipelineElab` / `runPipelineElabChecked` packet row. Keep the row
      order honest:
      first anchor (`sameLaneClearBoundaryExpr`),
      predecessor truth (`sameLaneAliasFrameClearBoundaryExpr`),
      merged next anchor (`sameLaneDoubleAliasFrameClearBoundaryExpr`),
      merged next anchor after that
      (`sameLaneTripleAliasFrameClearBoundaryExpr`),
      merged next anchor after that
      (`sameLaneQuadrupleAliasFrameClearBoundaryExpr`),
      merged next anchor after that
      (`sameLaneQuintupleAliasFrameClearBoundaryExpr`),
      merged next anchor after that
      (`sameLaneSextupleAliasFrameClearBoundaryExpr`),
      merged next anchor after that
      (`sameLaneSeptupleAliasFrameClearBoundaryExpr`),
      new live packet (`sameLaneOctupleAliasFrameClearBoundaryExpr`),
      then preserved selected same-wrapper nested-`forall`.
- [ ] In `test/PipelineSpec.hs`, sharpen the existing
      `sameLaneOctupleAliasFrameClearBoundaryExpr` entrypoint row into the
      live milestone-3 promotion check for both authoritative entrypoints
      while preserving the current recursive-output, type-check, and
      checked-parity substance. Prefer renaming / tightening the existing row
      over adding a second octuple-alias packet. Leave
      `sameLaneSeptupleAliasFrameClearBoundaryExpr` explicitly described as
      the merged next anchor after the sextuple-alias anchor, and leave the
      separate nonuple / deeper alias-shell rows plus the shared helper /
      budget guards as continuity-only outside-extraction evidence.
- [ ] In `test/ElaborationSpec.hs`, add the exact-edge authoritative
      instantiation translation guard for
      `sameLaneOctupleAliasFrameClearBoundaryExpr` immediately after the
      septuple-alias guard. Mirror the existing packet extractor shape,
      extend it to the octuple-alias fixture shape already declared for this
      packet, and lock the concrete
      `ExpInstantiate [...]` / `phiFromEdgeWitnessWithTrace` witness that the
      merged baseline actually emits for that packet. Keep the direct
      `runPipelineElab` success check and do not widen the elaboration surface
      into generic alias-depth helpers or nonuple-alias coverage.
- [ ] If local cleanup is needed to keep the tests readable, keep it inside
      these same three files only; do not add new modules, widen the
      production surface, or move nonuple / deeper alias shells onto the
      research / elaboration milestone-3 surfaces.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneOctupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSextupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuintupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneNonupleAliasFrameClearBoundaryExpr"'
```

### Task 3: Reopen production only if the explicit packet fails and a focused trace proves a within-slice seam

**Files:**
- Modify only if Task 2 fails: `src/MLF/Elab/Elaborate/Annotation.hs`
- Modify only if Task 2 fails: `src/MLF/Elab/Elaborate/Algebra.hs`
- Modify only if Task 2 fails and a focused trace proves a legacy translation gap: `src/MLF/Elab/Legacy.hs`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`

- [ ] If the newly explicit octuple-alias rows fail during authoritative
      instantiation translation, localize the repair to
      `src/MLF/Elab/Elaborate/Annotation.hs` around
      `reifyInst`,
      `authoritativeTargetType`,
      `reifyTraceBinderInstArgs`,
      `inferAuthoritativeInstArgs`, and the
      `expInstantiateArgsToInstNoFallback`
      handoff. The repair must only make the exact octuple-alias packet earn
      the authoritative translation it already deserves; it must not reopen
      quantified crossings, alias-depth generalization, or helper-only
      fallback behavior.
- [ ] If the failure instead appears after annotation translation, localize
      the repair to the existing
      `src/MLF/Elab/Elaborate/Algebra.hs`
      application / let handoff seam around
      `funInstRecovered`,
      `argInstFromFun`,
      `fApp`,
      `scheme`,
      `rhsAbs0`, and
      `rhsAbs`
      so the exact octuple-alias packet survives the admitted
      post-annotation seam on both authoritative entrypoints without changing
      the already merged first seven anchors.
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
- [ ] After any repair, rerun the exact packet and preserved-anchor cluster
      first and stop if the fix changes the merged first anchor, the merged
      double-alias next anchor, the merged triple-alias next anchor after
      that, the merged quadruple-alias next anchor after that, the merged
      quintuple-alias next anchor after that, the merged sextuple-alias next
      anchor after that, the merged septuple-alias next anchor after that,
      alias-frame predecessor truth, selected same-wrapper nested-`forall`
      success, checked-authoritative parity, or the fail-closed
      `P2` / `N1` / `N2` / `N6` shell probes.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneOctupleAliasFrameClearBoundaryExpr exact edge authoritative instantiation translation"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneOctupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSextupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuintupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'
```

### Task 4: Re-run the protection matrix, diff-scope checks, and full gates

**Files:**
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `test/FrozenParitySpec.hs`
- Verify-only: `test/Phi/AlignmentSpec.hs`
- Verify-only: `test/AlignmentInvariantSpec.hs`
- Verify-only: `test/ThesisFixDirectionSpec.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`

- [ ] Re-run the exact packet evidence proving
      `sameLaneOctupleAliasFrameClearBoundaryExpr` now succeeds honestly on
      both `runPipelineElab` and `runPipelineElabChecked`, and that the new
      exact-edge authoritative-instantiation guard is green.
- [ ] Re-run the preserved merged-baseline wins:
      `sameLaneClearBoundaryExpr`,
      `sameLaneDoubleAliasFrameClearBoundaryExpr`,
      `sameLaneTripleAliasFrameClearBoundaryExpr`,
      `sameLaneQuadrupleAliasFrameClearBoundaryExpr`,
      `sameLaneQuintupleAliasFrameClearBoundaryExpr`,
      `sameLaneSextupleAliasFrameClearBoundaryExpr`,
      `sameLaneSeptupleAliasFrameClearBoundaryExpr`,
      `sameLaneAliasFrameClearBoundaryExpr`,
      selected same-wrapper nested-`forall`,
      checked-authoritative parity,
      `BUG-2026-02-06-002`,
      `BUG-2026-02-17-002`,
      correct-semantic `g g`,
      direct let-polymorphism,
      nested-let fail-fast rows,
      `Phi alignment`,
      `Thesis alignment invariants`, and
      `Frozen parity artifact baseline`.
- [ ] Re-run the four quantified fail-closed shell probes with the shared
      `"fail-closed once it leaves the local TypeRef lane"` matcher so the
      current `P2` / `N1` / `N2` / `N6` protection surface stays honest.
- [ ] Re-run the no-nonuple boundary evidence:
      `sameLaneNonupleAliasFrameClearBoundaryExpr` and deeper alias shells
      must stay continuity-only and green in `test/PipelineSpec.hs`, and
      nonuple / deeper alias-shell names must remain absent from
      `test/Research/P5ClearBoundarySpec.hs` and
      `test/ElaborationSpec.hs`.
- [ ] Verify diff hygiene and scope discipline against merge-base `7a127e2`.
      Preferred outcome: implementation-owned diff is limited to the three
      test files. If Task 3 proved a real seam, the only additional lawful
      implementation-owned paths are
      `src/MLF/Elab/Elaborate/Annotation.hs`,
      `src/MLF/Elab/Elaborate/Algebra.hs`, and/or
      `src/MLF/Elab/Legacy.hs`.
- [ ] Run the full gates. Expected end state:
      `./scripts/thesis-conformance-gate.sh` passes,
      `cabal build all && cabal test` passes, and because this round should
      add two research rows and one elaboration row while sharpening the
      existing pipeline row, the full test count should rise from the merged
      `1359 examples` baseline to `1362 examples, 0 failures`.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneOctupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneNonupleAliasFrameClearBoundaryExpr"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "let id ="'
cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Thesis alignment invariants"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'
rg -n 'sameLaneNonupleAliasFrameClearBoundaryExpr|sameLaneDecupleAliasFrameClearBoundaryExpr' test/Research/P5ClearBoundarySpec.hs test/ElaborationSpec.hs test/PipelineSpec.hs
base=$(git -C orchestrator/worktrees/round-219 merge-base codex/automatic-recursive-type-inference HEAD)
git -C orchestrator/worktrees/round-219 diff --name-only "$base" -- test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Legacy.hs
git -C orchestrator/worktrees/round-219 diff --name-only "$base" -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
git -C orchestrator/worktrees/round-219 diff --check "$base" --
./scripts/thesis-conformance-gate.sh
cabal build all && cabal test
```

## Acceptance Criteria

- `sameLaneOctupleAliasFrameClearBoundaryExpr` is explicitly named as the
  next milestone-3 representative packet in
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`.
- `sameLaneClearBoundaryExpr` remains the merged first explicit milestone-3
  anchor.
- `sameLaneDoubleAliasFrameClearBoundaryExpr` remains the merged next explicit
  milestone-3 anchor.
- `sameLaneTripleAliasFrameClearBoundaryExpr` remains the merged next explicit
  milestone-3 anchor after the double-alias anchor.
- `sameLaneQuadrupleAliasFrameClearBoundaryExpr` remains the merged next
  explicit milestone-3 anchor after the triple-alias anchor.
- `sameLaneQuintupleAliasFrameClearBoundaryExpr` remains the merged next
  explicit milestone-3 anchor after the quadruple-alias anchor.
- `sameLaneSextupleAliasFrameClearBoundaryExpr` remains the merged next
  explicit milestone-3 anchor after the quintuple-alias anchor.
- `sameLaneSeptupleAliasFrameClearBoundaryExpr` remains the merged next
  explicit milestone-3 anchor after the sextuple-alias anchor.
- `sameLaneAliasFrameClearBoundaryExpr` remains preserved predecessor truth
  only.
- The selected same-wrapper nested-`forall` packet remains preserved
  merged-baseline success.
- Checked-authoritative parity and the fail-closed `P2` / `N1` / `N2` / `N6`
  guard cluster remain green.
- Nonuple / deeper alias shells remain continuity-only rather than newly
  promoted milestone-3 anchors, matching the bounded rule that they stay
  outside this extraction.
- No closed continuity anchor or out-of-slice path is touched unless Task 3
  proves the conditional production seam is necessary.
