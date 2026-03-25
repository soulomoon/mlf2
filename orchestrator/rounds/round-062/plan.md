# Round 062 Plan (`K1` Continue-Bounded Next-Target Bind)

## Objective

Execute only roadmap item `K1` and prepare one accepted docs-only bind
artifact at:
`docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`.

This is the initial `K1` plan for `attempt-1` with `retry: null`. The round
must bind the queued next bounded cycle to repaired `URI-R2-C1` under the
accepted `J4` result token `continue-bounded`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
boundary, and freeze exactly one next bounded successor slice without
reopening the accepted `I4` / `J1` / `J2` / `J3` / `J4` lane or any earlier
accepted bounded cycle as live work.

The selected successor slice is:

- one bounded local-binding empty-candidate / no-inst-arg scheme-alias /
  base-like `baseTarget -> baseC` fail-closed hardening slice centered on
  `src/MLF/Elab/Run/ResultType/Fallback.hs:377-381`
  plus the downstream final target-selection branch at
  `src/MLF/Elab/Run/ResultType/Fallback.hs:698-700`;
- future ownership limited to exactly
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  and
  `test/PipelineSpec.hs`;
- exact semantic intent: if the fallback root stays on the repaired local
  `TypeRef` lane, both `rootBoundCandidates` and `instArgBaseBounds` stay
  empty, the existing scheme-alias / base-like evidence remains true, and both
  `rootHasMultiInst` and `instArgRootMultiBase` remain false, the next cycle
  may harden only that one local empty-candidate / no-inst-arg
  scheme-alias / base-like `baseTarget` lane and its same-lane `targetC`
  consumption while keeping the accepted `F2` / `F3`
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane, the completed `rootLocalSingleBase` lane, the completed
  `rootLocalInstArgSingleBase` lane, the retained-target families, and every
  non-local or broader path as preserved context only; and
- no selection of replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget`, `View.hs`, `schemeBodyTarget`,
  non-local widening, cross-family widening, or roadmap / controller / bug
  edits.

`K1` is a docs-only bind / selection round. It does not land the future
scheme-alias / base-like `baseTarget` hardening itself, does not edit
production or test code, does not edit `orchestrator/rounds/round-062/state-snapshot.json`, and does not
perform roadmap, bug-tracker, review, or merge bookkeeping.

## Locked Round Context

- Round id: `round-062`
- Roadmap item: `K1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: docs-only bind / selection only; no production or test edits in
  `K1`

Accepted carry-forward facts that must remain binding throughout `K1`:

- `orchestrator/rounds/round-061/review-record.json` finalized `J4` as
  authoritative `attempt=1`, `attempt_verdict=accepted`,
  `stage_action=finalize`, `status=authoritative`, with canonical artifact
  path `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`.
- `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md` is the
  controlling accepted predecessor decision. It explicitly says any successor
  work must begin with a fresh bounded exact-target bind and does not
  authorize replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget`,
  `boundTarget`, `View.hs`, `schemeBodyTarget`, non-local widening, or broader
  trigger-family widening.
- The accepted `J1` / `J2` / `J3` / `J4` chain already consumed the repaired
  `URI-R2-C1` local inst-arg-only singleton-base
  `rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
  lane and preserved the completed `rootLocalSingleBase` lane plus the broader
  scheme-alias / base-like `baseTarget` route as inherited continuity only.
- The accepted `F2` / `F3` packet already consumed the local-binding
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane. `K1` must not reopen that accepted local `rootFinal` lane as if it
  were still the live successor target.
- The accepted `C`, `E`, `G`, `H`, `I`, and `J` cycles already consumed the
  local-binding `rootBindingIsLocalType` gate, retained-child
  `boundVarTarget`, local `rootLocalMultiInst`, local
  `rootLocalInstArgMultiBase`, local `rootLocalSingleBase`, and local
  `rootLocalInstArgSingleBase` families as accepted predecessor evidence only.
  None of those families may be reopened as the selected future `K2` target.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `## Open`
  section is empty, so current bug state does not authorize replay reopen,
  `InstBot`, or any broader successor family here.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.

Current source and test anchors expose one adjacent unselected local
`baseTarget` sub-branch and no lawful broader alternative:

- `Fallback.hs:377-381` still exposes the empty-candidate / no-inst-arg
  `baseTarget` branch guarded by
  `IntSet.null rootBoundCandidates && IntSet.null instArgBaseBounds &&
  not rootHasMultiInst && not instArgRootMultiBase`.
- `Fallback.hs:537-545` and `Fallback.hs:692-700` still name
  `rootLocalSchemeAliasBaseLike`, `rootLocalInstArgSingleBase`,
  `rootLocalSingleBase`, and the downstream `targetC` ordering, but no
  dedicated reviewer-auditable local proof isolates the empty-candidate /
  no-inst-arg scheme-alias / base-like `baseTarget` consumer as its own
  bounded authority.
- `PipelineSpec.hs:1244-1269` still exposes `schemeAliasBaseLikeFallback`, but
  the current helper / example / source-guard packet is accepted continuity
  for the `F2` local `keepTargetFinal` / `targetC -> rootFinal` lane rather
  than a dedicated bounded packet for the empty-candidate / no-inst-arg
  `baseTarget -> baseC` consumer.
- `PipelineSpec.hs:1560-1587` and `PipelineSpec.hs:1660-1708` still cover the
  accepted scheme-alias / base-like local-vs-non-local continuity and the
  current source guard, but no focused helper, example, or guard names a
  dedicated local empty-candidate / no-inst-arg scheme-alias / base-like
  `baseTarget` authority yet.

Current repository state is already non-pristine:

- `M orchestrator/rounds/round-062/state-snapshot.json`
- `?? orchestrator/rounds/round-062/selection.md`

Respect those existing changes. Do not revert or "clean up" unrelated work.

## File Map

### Modify

- `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
  - Responsibility: record the `K1` docs-only bind, freeze exactly one future
    `K2` target, and document why the selected empty-candidate / no-inst-arg
    local scheme-alias / base-like `baseTarget` lane is the only remaining
    lawful bounded successor under accepted `J4`.

### Read-Only Evidence

- `AGENTS.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-062/state-snapshot.json`
- `orchestrator/rounds/round-062/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/retry-subloop.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- `docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
- `orchestrator/rounds/round-061/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`
- `orchestrator/rounds/round-050/plan.md`
- `orchestrator/rounds/round-054/plan.md`
- `orchestrator/rounds/round-058/plan.md`
- `docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
- `docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

### Preserve Unchanged

- `orchestrator/rounds/round-062/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/roadmap.md`
- `orchestrator/rounds/round-062/selection.md`
- `/Volumes/src/mlf4/Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`

## Sequential Tasks

### Task 1 - Re-establish exact `K1` authority and round boundary

- Write the plan and future canonical `K1` artifact as `attempt-1` with
  `retry: null`.
- State explicitly that `K1` is docs-only bind / selection work under accepted
  `J4 = continue-bounded`; it does not implement `K2`, does not verify `K2`,
  does not pre-decide any later decision gate, and does not update the
  roadmap.
- Preserve repaired `URI-R2-C1` as the only live subject and preserve the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unchanged.
- Treat the accepted `I4` / `J1` / `J2` / `J3` / `J4` chain as immutable
  predecessor evidence only. Do not reopen accepted `I4` decision logic,
  `J1` target selection, `J2` implementation, `J3` verification, or `J4`
  decision logic.

### Task 2 - Freeze the exact `K2` successor slice

- In the canonical `K1` artifact, state that the only frozen future `K2`
  target is the local-binding empty-candidate / no-inst-arg
  scheme-alias / base-like `baseTarget -> baseC` hardening slice in
  `Fallback.hs`, limited to the existing `baseTarget` branch at
  `Fallback.hs:377-381` and the downstream same-lane `targetC` decision at
  `Fallback.hs:698-700`.
- Record the selected reviewer-auditable proof as a local-binding-only
  refinement of existing inputs, using all of:
  - `rootBindingIsLocalType`
  - `rootIsSchemeAlias`
  - `rootBoundIsBaseLike`
  - `IntSet.null rootBoundCandidates`
  - `IntSet.null instArgBaseBounds`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`
- State explicitly that future selected behavior may harden only the one local
  `baseTarget` path where both candidate sets stay empty but
  scheme-alias / base-like evidence still collapses to a concrete base-like
  target on the repaired local `TypeRef` lane.
- State explicitly that the accepted `F2` / `F3`
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane remains preserved predecessor evidence only and is not a second `K2`
  family.
- Keep `rootLocalSingleBase`, `rootLocalInstArgSingleBase`,
  `rootLocalSchemeAliasBaseLike` as already-accepted `rootFinal` authority,
  `rootLocalMultiInst`, `rootLocalInstArgMultiBase`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`, replay reopen, and every non-local or
  broader family as inherited context only and out of scope for `K2`.

### Task 3 - Freeze future ownership and focused `K2` evidence intent

- Freeze future `K2` ownership to exactly:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
- Freeze future focused verification to one bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block only:
  - add one local-binding empty-candidate / no-inst-arg
    scheme-alias / base-like success helper / example that keeps the root on
    the local `TypeRef` lane, keeps both `rootBoundCandidates` and
    `instArgBaseBounds` empty, preserves the scheme-alias / base-like inputs,
    and expects the concrete selected base result;
  - add one matched local contrast using the same wrapper family but changing
    one selected-lane precondition, so the result stays on already-accepted
    continuity rather than the new `K2` lane; and
  - refresh source-guard assertions only enough to name the selected local
    proof and prove that `targetC` consults the new empty-candidate /
    no-inst-arg local `baseTarget` lane without changing the preserved
    completed `rootLocalSingleBase` lane, the preserved completed
    `rootLocalInstArgSingleBase` lane, or the already-accepted `F2` local
    `rootFinal` lane.

### Task 4 - Refresh the canonical `K1` artifact with the exact non-selection list

- Write
  `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
  for `attempt-1`.
- Record:
  - the accepted `J4` authority and the required fresh-bind rule;
  - the exact selected `K2` local empty-candidate / no-inst-arg
    scheme-alias / base-like slice;
  - the evidence that the accepted `F2` / `F3`
    `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
    lane and the broader scheme-alias / base-like `baseTarget` route remain
    inherited continuity rather than reopened second targets;
  - the future ownership limited to `Fallback.hs` and `PipelineSpec.hs`; and
  - the focused positive / contrast / source-guard verification intent above.
- State explicitly that `K1` does not authorize implementation, review, merge,
  replay reopen, widening, or roadmap mutation.
- State explicitly that `K1` does not reopen or select:
  - accepted `I4` / `J1` / `J2` / `J3` / `J4`;
  - accepted `F2` / `F3` as live `K2` work;
  - the completed `rootLocalSingleBase` lane;
  - the completed `rootLocalInstArgSingleBase` lane;
  - `boundVarTarget`;
  - `boundTarget` overlay materialization;
  - `src/MLF/Elab/Run/ResultType/View.hs`;
  - `schemeBodyTarget`;
  - non-local widening; or
  - any broader trigger family.

### Task 5 - Run the docs-only verification required for the bind round

Run the baseline docs / state checks required by `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-062/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-062/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/retry-subloop.md`

Reconfirm exact-target authority with at least these focused checks:

- `test -f docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
- `test -f docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
- `test -f docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `python3 -m json.tool orchestrator/rounds/round-061/review-record.json >/dev/null`
- short `python3` assertion over `orchestrator/rounds/round-061/review-record.json`
  proving `J4` remains `accepted` + `finalize` + `authoritative` with the
  canonical artifact path
  `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '367,387p'`
  -> capture the preserved empty-candidate / no-inst-arg branch plus the
  completed adjacent singleton-base lanes
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '537,705p'`
  -> capture the current local-proof cluster and the downstream `targetC`
  ordering that future `K2` must stay inside
- `nl -ba test/PipelineSpec.hs | sed -n '1244,1269p'`
  -> capture the current scheme-alias helper that remains accepted continuity
  rather than the new bounded `K2` authority
- `nl -ba test/PipelineSpec.hs | sed -n '1560,1708p'`
  -> capture the preserved scheme-alias / base-like, local single-base,
  local inst-arg-only singleton-base, and current source-guard evidence

Post-edit docs-only diff checks must confirm:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- src src-public app test mlf2.cabal`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not run `cabal build all && cabal test` in `K1`. This round is docs-only
bind / selection work and must not simulate `K2` implementation or `K3`
verification.

## Completion Criteria

This `K1` plan is complete only if the future implementation stage stays
inside all of the following limits:

1. `plan.md` and the future canonical `K1` artifact explicitly name
   `attempt-1` with `retry: null`.
2. The accepted `K1` artifact binds exactly one future `K2` target: the
   local-binding empty-candidate / no-inst-arg
   scheme-alias / base-like `baseTarget -> baseC` lane and its same-lane
   `targetC` consumption in `Fallback.hs`.
3. The accepted `K1` artifact explicitly preserves the accepted `F2` / `F3`
   local `rootFinal` lane, the completed `rootLocalSingleBase` lane, the
   completed `rootLocalInstArgSingleBase` lane, and the broader
   scheme-alias / base-like `baseTarget` route as inherited context only
   rather than reopening them as new `K2` families.
4. Future ownership is frozen to `Fallback.hs` and `PipelineSpec.hs` only, and
   future focused evidence is frozen to one bounded `ARI-C1` extension only.
5. No production / test / public-API / executable / Cabal / controller-state /
   roadmap / bug-tracker edit occurs during `K1`, and no
   implementation / review / merge stage result is simulated inside this
   plan.

## Non-Authorization

This `K1` plan does not authorize:

- any change to `orchestrator/rounds/round-062/state-snapshot.json`;
- any rewrite of `orchestrator/rounds/round-062/selection.md`,
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-029/roadmap.md`, or `/Volumes/src/mlf4/Bugs.md`;
- any production / test / public-API / executable / Cabal edit during `K1`
  itself;
- any reopening of accepted `I4` / `J1` / `J2` / `J3` / `J4` as live work;
- any reopening of accepted `F2` / `F3` as live `K2` work;
- any reopening of replay work, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget` overlay materialization,
  `src/MLF/Elab/Run/ResultType/View.hs`, or `schemeBodyTarget`;
- any non-local widening, cross-family widening, equi-recursive reasoning,
  implicit unfolding, cyclic structural graph encoding, or broader recursive
  inference roadmap; or
- any review approval, merge action, or roadmap update.

The only lawful next step after this plan is a bounded docs-only `K1`
implementation that writes the canonical bind artifact and freezes that one
exact `K2` successor slice.

## Reviewer Checks

1. The authored `plan.md` and accepted `K1` artifact stay docs-only, name
   `attempt-1` with `retry: null`, and keep the round limited to roadmap item
   `K1`.
2. The accepted `K1` artifact binds repaired `URI-R2-C1` under accepted
   `J4 = continue-bounded` and preserves the inherited
   `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
   boundary.
3. The accepted `K1` artifact carries forward the accepted
   `I4` / `J1` / `J2` / `J3` / `J4` evidence chain without reopening it,
   keeps accepted `U2` / `U3` / `U4` negative findings binding, and treats
   `/Volumes/src/mlf4/Bugs.md` as continuity context only.
4. The accepted `K1` artifact freezes exactly one future `K2` target:
   the local-binding empty-candidate / no-inst-arg
   scheme-alias / base-like `baseTarget -> baseC` lane confined to
   `Fallback.hs:377-381` and `Fallback.hs:698-700`, with future ownership
   limited to:
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `test/PipelineSpec.hs`.
5. The accepted `K1` artifact explicitly keeps the accepted `F2` / `F3`
   local `rootFinal` lane, the completed `rootLocalSingleBase` lane, the
   completed `rootLocalInstArgSingleBase` lane, `boundVarTarget`,
   `boundTarget`, `schemeBodyTarget`, replay work, and non-local widening out
   of scope as separate families.
6. Verification evidence includes the required docs-only baseline checks, the
   `K1`-specific target-selection checks, and an explicit full-gate skip note.
