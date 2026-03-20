# Round 058 Plan (`J1` Continue-Bounded Next-Target Bind)

## Objective

Execute only roadmap item `J1` and prepare one accepted docs-only bind
artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058/docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`.

This is the initial `J1` plan for `attempt-1` with `retry: null`. The round
must bind the queued next bounded cycle to repaired `URI-R2-C1` under the
accepted `I4` result token `continue-bounded`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
boundary, and freeze exactly one next bounded successor slice without
reopening the accepted `I1` / `I2` / `I3` / `I4` lane or any earlier accepted
bounded cycle as live work.

The selected successor slice is:

- one bounded local-binding inst-arg-only singleton-base
  `baseTarget -> baseC` fail-closed hardening slice centered on
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058/src/MLF/Elab/Run/ResultType/Fallback.hs:382-387`
  plus the downstream final target-selection branch at
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058/src/MLF/Elab/Run/ResultType/Fallback.hs:687-710`;
- future ownership limited to exactly
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058/src/MLF/Elab/Run/ResultType/Fallback.hs`
  and
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058/test/PipelineSpec.hs`;
- exact semantic intent: if the fallback root still resolves through a local
  `TypeRef`, `rootBaseBounds` is empty, the existing `instArgBaseBounds`
  logic collapses to exactly one base-like candidate, and both
  `rootHasMultiInst` and `instArgRootMultiBase` remain false, the next cycle
  may harden only that one local inst-arg-only `baseTarget` lane and its
  same-lane `targetC` consumption while keeping the already accepted
  empty-candidate scheme-alias/base-like `baseTarget` route, the completed
  local single-base `rootLocalSingleBase` lane, the retained-target families,
  and every non-local or broader path as preserved context only; and
- no selection of replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget`, `View.hs`, `schemeBodyTarget`,
  non-local widening, cross-family widening, or roadmap/controller/bug edits.

`J1` is a docs-only bind/selection round. It does not land the future
inst-arg-only singleton-base hardening itself, does not edit production or
test code, does not edit `orchestrator/state.json`, and does not perform
roadmap, bug-tracker, review, or merge bookkeeping.

## Locked Round Context

- Round id: `round-058`
- Roadmap item: `J1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: docs-only bind/selection only; no production or test edits in
  `J1`

Accepted carry-forward facts that must remain binding throughout `J1`:

- `orchestrator/rounds/round-057/review-record.json` finalized `I4` as
  authoritative `attempt=2`, `attempt_verdict=accepted`,
  `stage_action=finalize`, `status=authoritative`, with canonical artifact path
  `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`.
- `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md` is the
  controlling accepted predecessor decision. It explicitly says any successor
  work must begin with a fresh bounded exact-target bind and does not
  authorize replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget`,
  `boundTarget` overlay materialization, `View.hs`, `schemeBodyTarget`,
  non-local widening, or broader trigger-family widening.
- The accepted `H1` and `I1` bind artifacts remain the direct planning
  precedents for this round: each bind consumed exactly one bounded family,
  preserved all completed predecessor families as inherited context only, and
  froze future ownership to `Fallback.hs` plus `PipelineSpec.hs` without
  authorizing implementation in the bind round itself.
- The accepted `I1` / `I2` / `I3` / `I4` chain already consumed the repaired
  `URI-R2-C1` local single-base
  `rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane
  and preserved the already accepted scheme-alias/base-like `baseTarget` route
  outside that completed lane.
- The accepted `E` / `F` / `G` / `H` cycles already consumed the retained-child
  `boundVarTarget`, local `rootLocalSchemeAliasBaseLike`,
  local `rootLocalMultiInst`, and local `rootLocalInstArgMultiBase` families as
  predecessor evidence only. None of those families may be reopened as the
  selected `J2` target.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `Open`
  section is empty, and `BUG-2026-03-16-001` remains resolved-only, so current
  bug state does not authorize replay reopen or any `InstBot` successor slice.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.

Current source and test anchors expose one adjacent unselected local
`baseTarget` sub-branch and no lawful broader alternative:

- `Fallback.hs:372-376` plus `Fallback.hs:389-406` and the accepted `I2` /
  `I3` helper/example coverage in `PipelineSpec.hs:1347-1373` and
  `PipelineSpec.hs:1518-1530` already cover the completed local single-base
  `rootBoundCandidates`-driven lane.
- Inference from the accepted `F2` / `F3` scheme-alias/base-like helper at
  `PipelineSpec.hs:1243-1268` and the current `baseTarget` ordering:
  the empty-candidate / no-inst-arg `baseTarget` branch at
  `Fallback.hs:377-381` is already continuity-only input for the preserved
  scheme-alias/base-like `baseTarget` route outside the completed `I` lane,
  not a new `J2` target.
- `Fallback.hs:382-387` still exposes one remaining local inst-arg-only
  singleton-base case:
  `IntSet.null rootBaseBounds && IntSet.size instArgBaseBounds == 1 && not rootHasMultiInst && not instArgRootMultiBase`.
- The focused helper cluster in `PipelineSpec.hs:1243-1373` names
  `schemeAliasBaseLikeFallback`, `localMultiInstFallback`,
  `localInstArgMultiBaseFallback`, and `localSingleBaseFallback`, but no
  focused helper or example isolates the local inst-arg-only singleton-base
  `baseTarget` / `targetC` lane or names `instArgBaseBounds` as the selected
  authority.

Current repository state is already non-pristine:

- `M orchestrator/state.json`
- `?? orchestrator/rounds/round-058/selection.md`

Respect those existing changes. Do not revert or "clean up" unrelated work.

## File Map

### Modify

- `docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
  - Responsibility: record the `J1` docs-only bind, freeze exactly one future
    `J2` target, and document why the selected inst-arg-only singleton-base
    lane is the only remaining lawful bounded successor under accepted `I4`.

### Read-Only Evidence

- `orchestrator/state.json`
- `orchestrator/rounds/round-058/selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
- `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
- `docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `/Volumes/src/mlf4/Bugs.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/rounds/round-058/selection.md`
- `/Volumes/src/mlf4/Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`

## Sequential Tasks

### Task 1 - Re-establish exact `J1` authority and round boundary

- Write the plan and future canonical `J1` artifact as `attempt-1` with
  `retry: null`.
- State explicitly that `J1` is docs-only bind/selection work under accepted
  `I4 = continue-bounded`; it does not implement `J2`, does not verify `J2`,
  does not pre-decide any later decision gate, and does not update the
  roadmap.
- Preserve repaired `URI-R2-C1` as the only live subject and preserve the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unchanged.
- Treat the accepted `I1` / `I2` / `I3` / `I4` chain as immutable predecessor
  evidence only. Do not reopen accepted `I1` target selection, `I2`
  implementation, `I3` verification, or `I4` decision logic.

### Task 2 - Freeze the exact `J2` successor slice

- In the canonical `J1` artifact, state that the only frozen future `J2`
  target is the local-binding inst-arg-only singleton-base
  `baseTarget -> baseC` hardening slice in `Fallback.hs`, limited to the
  existing `baseTarget` branch at `Fallback.hs:382-387` and the downstream
  same-lane `targetC` decision in `Fallback.hs:687-710`.
- Record the selected reviewer-auditable proof as a local-binding-only
  refinement of existing inputs, using all of:
  - `rootBindingIsLocalType`
  - `IntSet.null rootBaseBounds`
  - `IntSet.size instArgBaseBounds == 1`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`
- State explicitly that future selected behavior may harden only the one local
  `baseTarget` path where direct root-base candidates are absent but
  inst-argument evidence collapses to exactly one base-like candidate.
- State explicitly that the empty-candidate / no-inst-arg `baseTarget` branch
  at `Fallback.hs:377-381` remains preserved continuity context for the
  already accepted scheme-alias/base-like `baseTarget` route and is not the
  new `J2` target.
- Keep `rootLocalSingleBase`, `rootLocalSchemeAliasBaseLike`,
  `rootLocalMultiInst`, `rootLocalInstArgMultiBase`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`, replay reopen, and every non-local or
  broader family as inherited context only and out of scope for `J2`.

### Task 3 - Freeze future ownership and focused `J2` evidence intent

- Freeze future `J2` ownership to exactly:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
- Freeze future focused verification to one bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block only:
  - add one local-binding inst-arg-only singleton-base success helper/example
    that keeps the root on the local `TypeRef` lane, makes
    `instArgBaseBounds` collapse to exactly one `Int` candidate while leaving
    `rootBaseBounds` empty, and expects the concrete selected base result;
  - add one matched non-local contrast using the same wrapper and same
    singleton inst-arg setup that leaves the local `TypeRef` lane and must
    stay on inherited fail-closed behavior rather than the selected `J2`
    lane; and
  - refresh source-guard assertions only enough to name the selected local
    proof and prove that `targetC` consults the new inst-arg-only singleton
    `baseTarget` lane without changing the preserved scheme-alias/base-like
    `baseTarget` route or the completed `rootLocalSingleBase` lane.

### Task 4 - Refresh the canonical `J1` artifact with the exact non-selection list

- Write
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058/docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
  for `attempt-1`.
- Record:
  - the accepted `I4` authority and the required fresh-bind rule;
  - the exact selected `J2` inst-arg-only singleton-base slice;
  - the evidence that the empty-candidate / no-inst-arg `baseTarget` route is
    already preserved scheme-alias/base-like continuity rather than a new
    target;
  - the future ownership limited to `Fallback.hs` and `PipelineSpec.hs`; and
  - the focused positive/negative/source-guard verification intent above.
- State explicitly that `J1` does not authorize implementation, review, merge,
  replay reopen, widening, or roadmap mutation.
- State explicitly that `J1` does not reopen or select:
  - accepted `I1` / `I2` / `I3` / `I4`;
  - the preserved scheme-alias/base-like `baseTarget` route;
  - `boundVarTarget`;
  - `boundTarget` overlay materialization;
  - `src/MLF/Elab/Run/ResultType/View.hs`;
  - `schemeBodyTarget`;
  - non-local widening; or
  - any broader trigger family.

### Task 5 - Run the docs-only verification required for the bind round

Run the baseline docs/state checks required by `orchestrator/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/retry-subloop.md`

Reconfirm exact-target authority with at least these focused checks:

- `test -f docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `test -f docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
- `test -f docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `python3 -m json.tool orchestrator/rounds/round-057/review-record.json >/dev/null`
- short `python3` assertion over `orchestrator/rounds/round-057/review-record.json`
  proving `I4` remains `accepted` + `finalize` + `authoritative` with the
  canonical artifact path
  `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '367,387p'`
  -> capture the completed local single-base branch, the preserved
  empty-candidate / no-inst-arg branch, and the still-unselected
  inst-arg-only singleton-base branch
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '525,710p'`
  -> capture the current local-proof cluster and the downstream `targetC`
  ordering that future `J2` must stay inside
- `nl -ba test/PipelineSpec.hs | sed -n '1243,1373p'`
  -> capture the current helper cluster showing that no focused inst-arg-only
  singleton-base helper exists yet
- `nl -ba test/PipelineSpec.hs | sed -n '1508,1607p'`
  -> capture the preserved scheme-alias/base-like, local single-base,
  local multi-inst, local inst-arg multi-base, and current source-guard
  evidence

Post-edit docs-only diff checks must confirm:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- src src-public app test mlf2.cabal`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not run `cabal build all && cabal test` in `J1`. This round is docs-only
bind/selection work and must not simulate `J2` implementation or `J3`
verification.

## Completion Criteria

This `J1` plan is complete only if the future implementation stage stays
inside all of the following limits:

1. `plan.md` and the future canonical `J1` artifact explicitly name
   `attempt-1` with `retry: null`.
2. The accepted `J1` artifact binds exactly one future `J2` target: the
   local-binding inst-arg-only singleton-base `baseTarget -> baseC` lane and
   its same-lane `targetC` consumption in `Fallback.hs`.
3. The accepted `J1` artifact explicitly preserves the already accepted
   empty-candidate scheme-alias/base-like `baseTarget` route and the completed
   `rootLocalSingleBase` lane as inherited context only rather than reopening
   them as new target families.
4. Future ownership is frozen to `Fallback.hs` and `PipelineSpec.hs` only, and
   future focused evidence is frozen to one bounded `ARI-C1` extension only.
5. No production/test/public-API/executable/Cabal/controller-state/roadmap
   / bug-tracker edit occurs during `J1`, and no implementation/review/merge
   stage result is simulated inside this plan.

## Non-Authorization

This `J1` plan does not authorize:

- any change to `orchestrator/state.json`;
- any rewrite of `orchestrator/rounds/round-058/selection.md`,
  `orchestrator/roadmap.md`, or `/Volumes/src/mlf4/Bugs.md`;
- any production/test/public-API/executable/Cabal edit during `J1` itself;
- any reopening of accepted `I1` / `I2` / `I3` / `I4` as live work;
- any reopening of the preserved scheme-alias/base-like `baseTarget` route as
  a second `J2` family;
- any reopening of replay work, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget` overlay materialization,
  `src/MLF/Elab/Run/ResultType/View.hs`, or `schemeBodyTarget`;
- any non-local widening, cross-family widening, equi-recursive reasoning,
  implicit unfolding, cyclic structural graph encoding, or broader recursive
  inference roadmap; or
- any review approval, merge action, or roadmap update.

The only lawful next step after this plan is a bounded docs-only `J1`
implementation that writes the canonical bind artifact and freezes that one
exact `J2` successor slice.
