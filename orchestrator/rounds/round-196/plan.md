# Round 196 Plan

- Round: `round-196`
- Roadmap: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap` / `rev-001`
- Milestone: `milestone-2`
- Direction: `direction-2a-implement-the-selected-p5-lane`
- Extracted item: `implement-the-selected-p5-lane`
- Retry: `null`
- Execution shape: serial, one bounded code/test round only, no worker fan-out,
  and no concurrent `cabal` jobs. A concurrent focused `cabal test` attempt in
  this worktree already tripped the shared-`dist-newstyle` collision
  `ghc-pkg-9.14.1: cannot create ... package.conf.inplace already exists`, so
  all build/test verification in this round must run one command at a time.

## Objective

Land the smallest milestone-2 implementation/verification campaign that can
move the frozen retained-child guard-cluster `P5` lane from docs-only
classification into reviewer-visible authoritative evidence.

The round must either:

- make one retained-child specimen stay recursively visible on
  `runPipelineElab` and `runPipelineElabChecked` without leaving the inherited
  current architecture; or
- stop with an honest fail-closed / boundary-pressure read for that exact lane
  only.

It must do this without reopening the March 28 exact packet,
without relitigating `round-151`,
without opening `P2`,
and without touching anything outside the frozen round-194 writable slice.

## Locked Round Context

- Accepted `round-194` froze the only lawful `milestone-2` writable slice:
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `test/Research/P5ClearBoundarySpec.hs`,
  and `test/PipelineSpec.hs`.
- Accepted `round-195` selected
  `bounded current-architecture continuation` and bound this exact round as
  the only lawful next move before any post-`P5` routing.
- The selected lane is still exactly the retained-child guard cluster centered
  on `boundHasForallFrom`,
  `sameLaneLocalRetainedChildTarget`,
  `keepTargetFinal`,
  `targetC`,
  and `preserveRetainedChildAuthoritativeResult`.
- `nestedForallContrastExpr` remains settled predecessor truth only, and
  nested-forall `mu` absorption under polymorphic mediation remains accepted
  correct behavior only.
- Reviewer-visible success still has to show up on `runPipelineElab` /
  `runPipelineElabChecked` and stay continuous through
  `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs`.

## Authorized Write Scope

Primary implementation/test targets for this round are:

- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/TermClosure.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`

Conditional handoff target only if step 2 proves it is necessary to surface the
same preserved retained-child term on the authoritative entrypoints:

- `src/MLF/Elab/Run/Pipeline.hs`

Read-only continuity anchors unless a strictly mechanical facade repair is
forced by the previous step:

- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`

Do not add modules, do not touch `mlf2.cabal`, and do not write docs, roadmap,
controller-state, review, or merge artifacts in this round.

## Sequential Plan

1. Promote one explicit milestone-2 retained-child specimen into focused tests.
   - Modify `test/Research/P5ClearBoundarySpec.hs`.
   - Add one named retained-child / alias-frame specimen for the selected lane
     that is expected to stay recursive on both `runPipelineElab` and
     `runPipelineElabChecked`.
   - Keep `sameLaneClearBoundaryExpr` as bounded positive control only and keep
     `nestedForallContrastExpr` as settled reject-side contrast only.
   - Modify `test/PipelineSpec.hs`.
   - Refresh the source and behavior guards so the selected lane is still
     locked to `boundHasForallFrom`,
     `sameLaneLocalRetainedChildTarget`,
     `keepTargetFinal`,
     `targetC`,
     and `preserveRetainedChildAuthoritativeResult`, while off-lane or
     quantified-crossing contrasts stay fail-closed.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLane.*clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`

2. Implement the smallest fallback/term-closure correction inside the frozen lane.
   - Modify `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.
   - Keep `boundHasForallFrom` as the reject-side guard for nested `TyForall`
     and foreign scheme-root crossings.
   - Refine `boundVarTarget` / `sameLaneLocalRetainedChildTarget` so only
     same-local-`TypeRef` retained children whose bound root matches
     `boundVarTargetRoot` remain eligible.
   - Route that proof only through `keepTargetFinal` / `targetC`, preserving
     the current local multi-inst / inst-arg-multi-base / scheme-alias lanes
     and leaving non-local or quantified-crossing packets fail-closed.
   - Modify `src/MLF/Elab/TermClosure.hs`.
   - Keep `preserveRetainedChildAuthoritativeResult`,
     `preserveRetainedChildAliasBoundary`,
     `hasRetainedChildAliasBoundary`,
     and `hasRetainedChildClearBoundaryWithAliasBudget` aligned with the same
     selected lane so authoritative term closure preserves the recursive
     retained-child term only for that packet and does not leak continuity onto
     off-lane alias shells.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLane.*clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "reports PhiTranslatabilityError at pipeline entrypoints as a downstream consequence of correct non-recursive nested-forall outcome"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`

3. Repair authoritative handoff only if step 2 still leaves the preserved term invisible at the pipeline entrypoints.
   - Conditional file: `src/MLF/Elab/Run/Pipeline.hs`
   - If needed, adjust only the `termClosed` /
     `preserveRetainedChildAuthoritativeResult` /
     `checkedAuthoritative` plumbing so the retained-child term selected in
     step 2 is the same term/type pair reported by both `runPipelineElab` and
     `runPipelineElabChecked`.
   - Keep `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs` thin and
     mechanically continuous; do not turn either facade into a second
     implementation surface or packet-specific shortcut.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLane.*clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `rg -n 'runPipelineElab|runPipelineElabChecked|preserveRetainedChildAuthoritativeResult' src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs`

4. Run final serial verification and stop honestly if the lane still fails closed.
   - Modify no files in this step.
   - Run diff hygiene first, then the focused retained-child commands above in
     serial order, then the full repo gate.
   - If the focused tests or the full gate still show this exact lane failing
     closed or exerting explicit boundary pressure, stop there; do not widen
     beyond the frozen slice, do not add extra packets, and do not touch
     `mlf2.cabal`.
   - Verification:
     `git diff --check`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLane.*clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
     `cabal build all && cabal test`

## Review Focus

- Diff stays inside the frozen round-194 writable slice and remains serial.
- The selected lane stays exactly the retained-child guard cluster named in
  `round-194` / `round-195`; no fresh `P2` lane, no March 28 reopen, and no
  round-151 relitigation appears.
- Reviewer-visible outcome is measured on `runPipelineElab` /
  `runPipelineElabChecked` and matching pipeline facades, not on fallback-only
  helper evidence.
- The round remains honest if the selected packet still fails closed or
  exposes explicit boundary pressure.
