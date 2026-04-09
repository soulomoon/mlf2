# Round 211 Continuation Plan (rev-014 / evidence-first same-round continuation)

> **For agentic workers:** Keep the existing `round-211` branch/worktree. Under `rev-014`, the preserved baseline is already structurally admissible if the same green evidence reproduces. Do not rewrite `src/MLF/Elab/Elaborate/Algebra.hs` merely to flatten helper locals that `rev-014` now explicitly admits.

**Goal:** Carry the same `round-211` baseline through `implement` by
re-establishing the `rev-014` evidence chain and refreshing the round-owned
notes. The default successful outcome is "no production/test edits required";
only a fresh, concrete `rev-014` mismatch would justify reopening code, and
that is not currently expected.

**Architecture:** The current runtime behavior is fixed and already green.
`rev-014` explicitly admits the current helper-local scaffold inside
`src/MLF/Elab/Elaborate/Algebra.hs` under
`funInstRecovered`,
`argInstFromFun`,
`fApp`,
`scheme`,
`rhsAbs0`, and
`rhsAbs`.
The preserved diffs in
`src/MLF/Elab/Elaborate/Annotation.hs`,
`src/MLF/Elab/Legacy.hs`,
`test/ElaborationSpec.hs`,
`test/PipelineSpec.hs`, and
`test/Research/P5ClearBoundarySpec.hs`
remain carry-forward baseline evidence, not rewrite targets.

**Tech Stack:** Haskell, Cabal, hspec, `./scripts/thesis-conformance-gate.sh`

---

## Boundaries

- Keep the live `round-211` branch/worktree and inherited diff intact; do not
  reset the round, restart on a fresh branch, or relitigate the already-green
  selected packet / `BUG-2026-02-17-002` / A6 / nested-let / representative
  let-polymorphism wins as if they were reopened.
- Treat the preserved baseline as already review-valid under `rev-014` unless a
  fresh audit proves otherwise.
- The expected writable artifact for this continuation is
  `orchestrator/rounds/round-211/implementation-notes.md`.
- Do not edit production/test files merely to remove
  `containsMuType`,
  `containsMuBound`,
  `isIdentityLikeSchemeType`,
  `shouldInlineParamTy`,
  `shouldInferArgInst`,
  `isInternalTyVar`,
  `isIdentityLambdaBody`,
  `muAnnotationTy`, or the current `schemeTy` / strip-candidate staging locals;
  those are admitted by `rev-014` when they stay inside the current
  `AAppF` / `ALetF` seam.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  closed and untouched.
- Keep the parent-workspace and canonical round-worktree live pointer stubs
  aligned with `rev-014`; if that housekeeping is already correct, leave those
  files untouched.
- Do not widen into cyclic search, multi-SCC behavior, equi-recursive
  reasoning, fallback rescue, a second interface, milestone-3 corpus widening,
  or milestone-4 closeout work.

### Task 1: Reconfirm that the preserved baseline is already `rev-014`-valid

**Files:**
- Verify-only: `orchestrator/state.json`
- Verify-only: `orchestrator/rounds/round-211/selection.md`
- Verify-only: `orchestrator/rounds/round-211/review.md`
- Verify-only: `orchestrator/rounds/round-211/reviews/attempt-13.md`
- Verify-only: `orchestrator/roadmap.md`
- Verify-only: `orchestrator/verification.md`
- Verify-only: `orchestrator/retry-subloop.md`
- Verify-only: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- Verify-only: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- Verify-only: `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `orchestrator/rounds/round-211/implementation-notes.md`

- [ ] Re-read the current `selection.md`, `review.md`, and `attempt-13.md`, but
      interpret the old helper-local rejection as superseded by `rev-014`:
      under the live revision, the current helper-local scaffold is admitted
      and the preserved baseline should be treated as presumptively valid.
- [ ] Confirm lineage and pointer continuity from the canonical `round-211`
      worktree and the parent workspace: `orchestrator/state.json`,
      `selection.md`, and both pointer-stub surfaces must all resolve the same
      active `roadmap_id`, `roadmap_revision = rev-014`, and `roadmap_dir`.
- [ ] Confirm the live diff is still the preserved `round-211` baseline:
      the production/test surfaces remain limited to the inherited
      `Annotation.hs` carry-forward diff, the unchanged `Legacy.hs` absence of
      diff, the existing `Algebra.hs` handoff seam, and the three round-owned
      test files, with closed continuity surfaces untouched.
- [ ] Inspect `src/MLF/Elab/Elaborate/Algebra.hs` directly and verify that the
      helper-local scaffold still sits inside the admitted `rev-014` locals:
      `funInstRecovered`,
      `argInstFromFun`,
      `fApp`,
      `scheme`,
      `rhsAbs0`, and
      `rhsAbs`.
      If this is still true and no new helper growth appears beyond the
      admitted scaffold, record that the baseline is already review-valid under
      `rev-014` and do not schedule code churn.

**Verification commands:**

```bash
python3 -m json.tool orchestrator/state.json >/dev/null
git branch --show-current
git diff --check
git diff --name-only
git diff -U0 -- src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Legacy.hs
git diff -U0 -- test/ElaborationSpec.hs test/PipelineSpec.hs test/Research/P5ClearBoundarySpec.hs
git diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs
sed -n '1,40p' orchestrator/roadmap.md
sed -n '1,40p' orchestrator/verification.md
sed -n '1,40p' orchestrator/retry-subloop.md
sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md
sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md
sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md
rg -n 'containsMuType|containsMuBound|isIdentityLikeSchemeType|shouldInlineParamTy|shouldInferArgInst|isInternalTyVar|isIdentityLambdaBody|muAnnotationTy|schemeTy' src/MLF/Elab/Elaborate/Algebra.hs
nl -ba src/MLF/Elab/Elaborate/Algebra.hs | sed -n '169,460p'
nl -ba src/MLF/Elab/Elaborate/Algebra.hs | sed -n '560,668p'
```

### Task 2: Refresh the round-owned evidence instead of refactoring code

**Files:**
- Modify: `orchestrator/rounds/round-211/implementation-notes.md`
- Modify only if Task 1 proves stale live-pointer housekeeping:
  `orchestrator/roadmap.md`
- Modify only if Task 1 proves stale live-pointer housekeeping:
  `orchestrator/verification.md`
- Modify only if Task 1 proves stale live-pointer housekeeping:
  `orchestrator/retry-subloop.md`
- Modify only if Task 1 proves stale live-pointer housekeeping:
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- Modify only if Task 1 proves stale live-pointer housekeeping:
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- Modify only if Task 1 proves stale live-pointer housekeeping:
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`

- [ ] Update `orchestrator/rounds/round-211/implementation-notes.md` so it
      reflects the live `rev-014` contract honestly:
      the current helper-local scaffold is admitted,
      the preserved baseline is already structurally acceptable when the green
      evidence reproduces, and this continuation therefore made the implement
      step evidence-oriented rather than forcing new `Algebra.hs` churn.
- [ ] Preserve the current production/test diff unchanged while refreshing the
      notes. `src/MLF/Elab/Elaborate/Algebra.hs`,
      `src/MLF/Elab/Elaborate/Annotation.hs`,
      `src/MLF/Elab/Legacy.hs`,
      `test/ElaborationSpec.hs`,
      `test/PipelineSpec.hs`, and
      `test/Research/P5ClearBoundarySpec.hs`
      are verify-only for this continuation.
- [ ] If Task 1 unexpectedly finds a stale pointer stub, refresh only the
      pointer text to `rev-014` in both surfaces as continuity housekeeping and
      then stop there; do not use stale pointer cleanup as permission to reopen
      code.
- [ ] If Task 1 finds any mismatch other than stale pointer text or broken
      evidence reproduction, stop with explicit proof instead of opportunistic
      refactoring. `rev-014` does not authorize another flatten-only cleanup
      pass.

**Verification commands:**

```bash
git diff --name-only
git diff -U0 -- orchestrator/rounds/round-211/implementation-notes.md
git diff -U0 -- src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Legacy.hs
git diff -U0 -- test/ElaborationSpec.hs test/PipelineSpec.hs test/Research/P5ClearBoundarySpec.hs
git diff -U0 -- orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md
git -C /Users/ares/.codex/worktrees/d432/mlf4 diff -U0 -- orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md
```

### Task 3: Re-run the preserved green matrix and close on evidence

**Files:**
- Verify-only: `src/MLF/Elab/Elaborate/Algebra.hs`
- Verify-only: `src/MLF/Elab/Elaborate/Annotation.hs`
- Verify-only: `src/MLF/Elab/Legacy.hs`
- Verify-only: `test/ElaborationSpec.hs`
- Verify-only: `test/PipelineSpec.hs`
- Verify-only: `test/Research/P5ClearBoundarySpec.hs`
- Verify-only: `test/FrozenParitySpec.hs`
- Verify-only: `test/Phi/AlignmentSpec.hs`
- Verify-only: `test/AlignmentInvariantSpec.hs`
- Verify-only: `orchestrator/rounds/round-211/implementation-notes.md`

- [ ] Re-run the focused protected matrix from the canonical `round-211`
      worktree. The evidence target is the same preserved green cluster named
      in `selection.md`, `verification.md`, and the accepted `rev-014`
      contract, not a fresh code rewrite.
- [ ] Re-run the blocker-cluster / let-polymorphism / parity / thesis-alignment
      sentinels that were already green at attempt 13.
- [ ] Re-run `./scripts/thesis-conformance-gate.sh` and
      `cabal build all && cabal test`.
- [ ] Treat the round as ready for review when all focused gates remain green,
      the current `Algebra.hs` helper-local scaffold remains inside the
      admitted `rev-014` seam, the preserved production/test baseline remains
      unchanged, and the refreshed implementation notes explain that no
      production/test edits were required for this continuation.

**Verification commands:**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "runtime snapshot rebuild stays stable across representative corpus"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "redirected let-use sites keep polymorphic schemes"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates dual instantiation in application"'
cabal test mlf2-test --test-show-details=direct --test-options='--match "let id ="'
cabal test mlf2-test --test-show-details=direct --test-options='--match "dual annotated coercion consumers fail fast on unresolved non-root OpWeaken"'
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

- The preserved `round-211` baseline still matches the active `rev-014`
  lineage and both pointer-stub surfaces.
- The current `Algebra.hs` helper-local scaffold remains inside the admitted
  `funInstRecovered` / `argInstFromFun` / `fApp` / `scheme` / `rhsAbs0` /
  `rhsAbs` seam and does not grow beyond the admitted helper set.
- No fresh production/test drift appears beyond the inherited baseline.
- The focused matrix, thesis gate, and full repo gate rerun green from the
  canonical `round-211` worktree.
- `orchestrator/rounds/round-211/implementation-notes.md` explicitly records
  that the preserved baseline is already review-valid under `rev-014` and that
  this continuation therefore remained evidence-oriented rather than code-
  churn-oriented.
