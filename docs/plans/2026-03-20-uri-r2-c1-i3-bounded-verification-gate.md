# `I3` Bounded Verification Gate For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-056`
Roadmap item: `I3`
Stage: `implement`
Attempt: `attempt-2`
Retry state: active (`retry.stage_id: "I3"`, `retry.attempt: 2`, `retry.latest_accepted_attempt: 1`, `retry.retry_reason: "i3-shell-missing-cabal"`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bounded verification/evidence gate

## Stage Contract Freeze

This artifact executes only roadmap item `I3` for retry `attempt-2`.

`I3` is a docs-only bounded verification/evidence consolidation gate for the
already accepted `I2` local single-base
`rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane.
It does not reopen `I1` target selection, does not reopen `I2`
implementation, does not preempt `I4`, and does not authorize production or
test edits during this attempt.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback or widening.

Any verification failure in this round is a blocker to record, not permission
to patch `src/MLF/Elab/Run/ResultType/Fallback.hs`,
`test/PipelineSpec.hs`, or any other production/test file, and not permission
to create `orchestrator/rounds/round-056/implementation-notes.md`.

## Accepted I1 / I2 Continuity Carried Forward Without Widening

1. `orchestrator/rounds/round-054/review-record.json` remains the
   authoritative acceptance proof that `I1` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`.
2. `orchestrator/rounds/round-055/review-record.json` remains the
   authoritative acceptance proof that `I2` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`.
3. The accepted `I1` artifact still freezes exactly one future slice under
   repaired `URI-R2-C1`: the local-binding single-base
   `baseTarget -> baseC` fail-closed lane in `Fallback.hs`, together with the
   same-lane `targetC` use, with frozen future ownership limited to
   `Fallback.hs` and `PipelineSpec.hs`.
4. The accepted `I2` artifact still records the landed bounded proof
   `rootLocalSingleBase = rootBindingIsLocalType && IntSet.size rootBoundCandidates == 1 && not rootHasMultiInst && not instArgRootMultiBase`,
   the selected `baseTarget -> baseC` route, the same-lane `targetC` use, the
   preserved scheme-alias/base-like `baseTarget` consumer outside the selected
   lane, and the accepted focused/full-gate evidence from `round-055`.
5. The accepted `U6` and `H4` artifacts remain controlling non-widening
   continuity: repaired `URI-R2-C1` stays the live subject, the inherited
   explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface
   / no-fallback boundary remains binding, and accepted `U2` / `U3` / `U4`
   negatives are not reinterpreted as clearance for widening.
6. `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `Open`
   section is still empty and `BUG-2026-03-16-001` remains resolved-only, so
   the bug tracker does not authorize replay reopen, `MLF.Elab.Inst`,
   `InstBot`, `boundVarTarget` widening, `boundTarget` overlay
   materialization, `View.hs`, `schemeBodyTarget` consolidation, non-local
   widening, or broader recursive-inference work in `I3`.
7. Preserved exclusions remain binding throughout this gate:
   `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
   `rootLocalSchemeAliasBaseLike`, and `boundVarTarget` remain inherited
   context only except where cited below as preserved non-selected context; no
   replay reopen; no `MLF.Elab.Inst`; no `InstBot`; no `boundVarTarget`
   widening; no `boundTarget` overlay materialization; no
   `src/MLF/Elab/Run/ResultType/View.hs` work; no `schemeBodyTarget`
   consolidation; no non-local widening or broader recursive search; no second
   executable interface; no compatibility, convenience, or fallback-path
   widening; no equi-recursive reasoning; and no cyclic structural graph
   encoding.

## Read-Only Anchor Evidence

Read-only source/test inspection in the live `round-056` worktree still
matches the accepted `I2` bounded lane:

- `src/MLF/Elab/Run/ResultType/Fallback.hs:367-408` still defines
  `baseTarget` so the selected local single-base admission remains bounded to
  singleton `rootBoundCandidates` together with
  `not rootHasMultiInst` and `not instArgRootMultiBase`; the alternate
  singleton-base fallback cases remain under the same non-multi-inst /
  non-inst-arg-multi-base guard.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:525-539` still exposes
  `rootLocalMultiInst`, `rootLocalInstArgMultiBase`, and
  `rootLocalSchemeAliasBaseLike` as preserved inherited triggers, and still
  defines `rootLocalSingleBase` exactly as
  `rootBindingIsLocalType && IntSet.size rootBoundCandidates == 1 && not rootHasMultiInst && not instArgRootMultiBase`.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:630-678` still keeps
  `boundVarTarget` as preserved inherited machinery outside the selected `I3`
  lane: it remains the retained-child lookup path with same-local-TypeRef
  selection for local roots and `parentRef == scopeRoot` selection for
  non-local roots.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:679-685` still defines
  `keepTargetFinal` only in terms of `rootLocalMultiInst`,
  `rootLocalInstArgMultiBase`, `rootLocalSchemeAliasBaseLike`, and
  `boundVarTarget`; `rootLocalSingleBase` is still excluded from the retained
  target family.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:686-710` still makes the final
  `targetC` decision by choosing `baseC` only when `baseTarget` is present and
  `rootLocalSingleBase` holds, separately preserving the already-accepted
  scheme-alias/base-like `baseTarget` consumer, and otherwise leaving the
  inherited `keepTargetFinal` / `schemeBodyTarget` / fail-closed branches
  unchanged.
- `test/PipelineSpec.hs:1347-1373` still defines
  `localSingleBaseFallback`, the focused helper that rewrites the same local
  wrapper into the selected single-base lane.
- `test/PipelineSpec.hs:1486-1496` still carries the retained-child
  same-local-TypeRef lane source guard as inherited context only.
- `test/PipelineSpec.hs:1508-1535` still preserves the local
  scheme-alias/base-like coverage as the non-selected `baseTarget` consumer
  outside the selected lane, while also still containing the local single-base
  positive example at `1518-1520` and the matched non-local single-base
  fail-closed contrast at `1522-1530`.
- `test/PipelineSpec.hs:1537-1575` still preserves the local multi-inst and
  local inst-arg multi-base coverage as inherited context only.
- `test/PipelineSpec.hs:1594-1650` still contains the source-guard assertions
  naming `rootLocalSingleBase`, the selected `targetC` branch, the preserved
  `keepTargetFinal` trigger names, and the non-local proxy fail-closed pipeline
  entrypoint checks.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-056`

### Attempt-2 Retry Delta Commands

- `command -v cabal` -> pass:
  - `/Users/ares/.ghcup/bin/cabal`
- `cabal --version` -> pass:
  - `cabal-install version 3.16.1.0`
  - `compiled using version 3.16.1.0 of the Cabal library (in-tree)`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass (`exit 0`):
  - `Running 1 test suites...`
  - `Pipeline (Phases 1-5) -> Integration Tests -> ARI-C1 feasibility characterization (bounded prototype-only)`
  - `Finished in 0.0236 seconds`
  - `17 examples, 0 failures`
  - `Test suite mlf2-test: PASS`
  - log:
    `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-056/dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log`
- `cabal build all && cabal test` -> pass (`exit 0`):
  - build step covered `lib:mlf2-internal`, public library, `test:mlf2-test`, `exe:mlf2`, and `exe:frozen-parity-gen`
  - `Running 1 test suites...`
  - `Finished in 2.1037 seconds`
  - `1138 examples, 0 failures`
  - `Test suite mlf2-test: PASS`
  - log:
    `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-056/dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log`

### Inherited Attempt-1 Context Preserved Outside This Retry Delta

- The accepted attempt-1 blocker record remains reviewer-owned in
  `orchestrator/rounds/round-056/review.md` and
  `orchestrator/rounds/round-056/reviews/attempt-1.md`.
- The read-only anchor inspection and predecessor-continuity narrative above
  remain unchanged; retry `attempt-2` only replaces the shell-missing-`cabal`
  blocker with actual PATH proof plus fresh focused/full verification evidence.

### Post-Edit Diff Discipline

- `git diff --check` -> pass (no output)
- `git status --short --untracked-files=all` -> bounded final diff:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
  - `?? orchestrator/rounds/round-056/attempt-log.jsonl`
  - `?? orchestrator/rounds/round-056/plan.md`
  - `?? orchestrator/rounds/round-056/review.md`
  - `?? orchestrator/rounds/round-056/reviews/attempt-1.md`
  - `?? orchestrator/rounds/round-056/selection.md`
- `git diff --name-only` -> pre-existing tracked diff only:
  - `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> no output

These post-edit checks confirm the expected `I3` diff boundary:
pre-existing `M orchestrator/state.json`, pre-existing
`?? orchestrator/rounds/round-056/attempt-log.jsonl`, pre-existing
`?? orchestrator/rounds/round-056/selection.md`, pre-existing
`?? orchestrator/rounds/round-056/plan.md`, and one refreshed canonical `I3`
artifact, alongside the pre-existing reviewer-owned
`?? orchestrator/rounds/round-056/review.md` and
`?? orchestrator/rounds/round-056/reviews/attempt-1.md`. No diff was
introduced in `src/`, `test/`, `src-public/`, `app/`, `mlf2.cabal`,
`orchestrator/roadmap.md`, or `/Volumes/src/mlf4/Bugs.md`.

## Bounded Result

`I3` `attempt-2` is no longer blocked.

The retry delta replaced the shell-level blocker with fresh bounded
verification evidence without touching `Fallback.hs` or `PipelineSpec.hs`:

- `cabal` resolves on `PATH` as `/Users/ares/.ghcup/bin/cabal`;
- the exact focused `ARI-C1` rerun exits `0` with `17 examples, 0 failures`;
  and
- the exact full repo gate `cabal build all && cabal test` exits `0` with
  `1138 examples, 0 failures`.

This attempt therefore supplies the previously missing fresh verification
evidence for the accepted `I2` local single-base lane while preserving
repaired `URI-R2-C1`, the inherited explicit-only / non-equi-recursive /
non-cyclic-graph boundary, and the docs-only / read-only-source contract.
Review still owns the final acceptance decision.

## Preserved Non-Authorization

This verification record does not authorize:

- reopening `I1` target selection or `I2` implementation;
- preempting `I4`;
- editing `src/`, `test/`, `src-public/`, `app/`, `mlf2.cabal`,
  `orchestrator/state.json`, `orchestrator/roadmap.md`, or
  `/Volumes/src/mlf4/Bugs.md`;
- creating `orchestrator/rounds/round-056/implementation-notes.md`;
- replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening,
  `boundTarget` overlay materialization, `View.hs`, `schemeBodyTarget`
  consolidation, or non-local widening;
- equi-recursive reasoning, implicit unfolding, or cyclic structural graph
  encoding; or
- any second interface, compatibility shim, convenience fallback, or
  default-path widening.

The only implementation-stage write for this retry attempt is this refreshed
canonical `I3` artifact.
