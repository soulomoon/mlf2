# Round 054 Plan (`I1` Continue-Bounded Next-Target Bind)

## Objective

Execute only roadmap item `I1` and prepare one accepted docs-only bind artifact
at:
`docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`.

This is the initial `I1` plan for `attempt-1` with `retry: null`. The round
must bind the queued next bounded cycle to repaired `URI-R2-C1` under the
accepted `H4` result token `continue-bounded`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
boundary, and freeze exactly one next bounded successor slice without
reopening the accepted `H1` / `H2` / `H3` / `H4` lane or any earlier accepted
bounded cycle as live work.

The selected successor slice is:

- one bounded local-binding single-base `baseTarget -> baseC` fail-closed
  hardening slice centered on
  `src/MLF/Elab/Run/ResultType/Fallback.hs:367-408`
  plus the downstream final target-selection branch at
  `src/MLF/Elab/Run/ResultType/Fallback.hs:681-701`;
- future ownership limited to exactly
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  and
  `test/PipelineSpec.hs`;
- exact semantic intent: if the fallback root still resolves through a local
  `TypeRef` and the existing `rootBoundCandidates` logic collapses to one
  base-like candidate while `rootHasMultiInst` and `instArgRootMultiBase`
  remain false, the next cycle may harden only that one local single-base
  `baseTarget` lane while keeping non-local roots, multi-inst/multi-base
  cases, `boundVarTarget`, and the already-accepted `schemeBodyTarget`
  baseline fail-closed or inherited context only; and
- no selection of replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  non-local widening, cross-family widening, `boundTarget` overlay
  materialization, or `MLF.Elab.Run.ResultType.View.hs`.

`I1` is a docs-only bind/selection round. It does not land the future
`baseTarget` hardening itself, does not edit production or test code, does not
edit `orchestrator/rounds/round-054/state-snapshot.json`, and does not perform roadmap, bug-tracker, or
controller bookkeeping.

## Locked Round Context

- Round id: `round-054`
- Roadmap item: `I1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: docs-only bind/selection only; no production or test edits in
  `I1`

Accepted carry-forward facts that must remain binding throughout `I1`:

- `orchestrator/rounds/round-053/review-record.json` finalized `H4` as
  authoritative `attempt=1`, `attempt_verdict=accepted`,
  `stage_action=finalize`, `status=authoritative`, with canonical artifact path
  `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`.
- `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md` is the
  controlling accepted predecessor decision. It explicitly says any successor
  work must begin with a fresh bounded exact-target bind and does not authorize
  replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget`, non-local
  widening, or broader trigger-family widening.
- `orchestrator/rounds/round-053/review.md` already recorded that
  `orchestrator/rounds/round-054/selection.md` contains stale guider bug-state
  text. The selection remains authoritative for choosing `I1` and its scope,
  but its old sentence claiming `BUG-2026-03-16-001` is open is
  non-authoritative context drift.
- The canonical bug tracker at `/Volumes/src/mlf4/Bugs.md` has an empty `Open`
  section, and `BUG-2026-03-16-001` appears only under `Resolved`. Current bug
  state therefore does not authorize replay reopen or any `InstBot` successor
  slice.
- The accepted `C1` / `C2` / `C3` / `C4` chain already bound and verified the
  local-binding-only `rootBindingIsLocalType` / `schemeBodyTarget` /
  non-local-proxy fail-closed baseline in
  `MLF.Elab.Run.ResultType.Fallback` and `test/PipelineSpec.hs`. `I1` must not
  reopen that baseline as if it were still unselected work.
- The accepted `E1` / `E2` / `E3` / `E4` chain already consumed the same-lane
  retained-child `boundVarTarget` / nested-`forall` family as predecessor
  evidence only.
- The accepted `F1` / `F2` / `F3` / `F4` chain already consumed the local
  `rootLocalSchemeAliasBaseLike` / `targetC -> rootFinal` family as
  predecessor evidence only.
- The accepted `G1` / `G2` / `G3` / `G4` chain already consumed the local
  `rootLocalMultiInst` / `targetC -> rootFinal` family as predecessor evidence
  only.
- The accepted `H1` / `H2` / `H3` / `H4` chain already consumed the local
  `rootLocalInstArgMultiBase` / `targetC -> rootFinal` family as predecessor
  evidence only and preserved `baseTarget` rejection outside that selected
  lane.
- Taken together, the accepted `E` / `F` / `G` / `H` cycles have already
  consumed the currently accepted local `keepTargetFinal` trigger families in
  `Fallback.hs`. No still-unopened local `keepTargetFinal` family remains for
  `I1` to select next.
- Current source and test anchors still expose one adjacent unselected local
  target-selection branch:
  - `Fallback.hs:367-408` defines the single-base `baseTarget` candidate path;
  - `Fallback.hs:681-701` still routes `targetC` through `baseTarget` before
    every retained-target or `schemeBodyTarget` fallback branch; and
  - the focused `ARI-C1 feasibility characterization (bounded prototype-only)`
    block in `test/PipelineSpec.hs` names retained-child, scheme-alias/base-like,
    multi-inst, inst-arg multi-base, and non-local proxy coverage, but does
    not name `baseTarget`, `boundTarget`, or `rootBoundCandidates` as a
    focused selected lane.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.

Current repository state is already non-pristine:

- `M orchestrator/rounds/round-054/state-snapshot.json`
- `?? orchestrator/rounds/round-054/selection.md`
- `?? orchestrator/rounds/round-054/plan.md`

Respect those existing changes. Do not revert or "clean up" unrelated work
while preparing the `I1` bind.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-054/state-snapshot.json`
- `orchestrator/rounds/round-054/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-021/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-021/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-021/roadmap.md`
- `orchestrator/rounds/round-053/review-record.json`
- `orchestrator/rounds/round-053/review.md`
- `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
- `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
- `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
- `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
- `TODO.md`
- `implementation_notes.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable artifact for the implement stage:

1. `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
   - canonical `I1` bind/selection record.

Optional bounded note file:

1. `orchestrator/rounds/round-054/implementation-notes.md`
   - optional only if the implementer needs a concise round-local note file.

Read-only target anchors for freezing the successor slice:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`
3. `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
4. `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
5. `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
6. `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
7. `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
8. `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
9. `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
10. `orchestrator/rounds/round-053/review-record.json`
11. `orchestrator/rounds/round-053/review.md`
12. `/Volumes/src/mlf4/Bugs.md`

Files that must remain untouched by `I1` `attempt-1`:

- `orchestrator/rounds/round-054/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-021/roadmap.md`
- `orchestrator/rounds/round-054/selection.md`
- `TODO.md`
- `implementation_notes.md`
- `/Volumes/src/mlf4/Bugs.md`
- all production and test surfaces under
  `src/`,
  `src-public/`,
  `app/`,
  `test/`, and
  `mlf2.cabal`
- prior round artifacts under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-053/`

If the implementer cannot keep the selected successor slice confined to
`Fallback.hs` and `PipelineSpec.hs`, stop and hand the issue back to review
rather than broadening into `MLF.Elab.Run.ResultType.View.hs`, non-local
fallback behavior, replay reopen, or any cross-family work.

## Sequential Tasks

### Task 1 - Freeze the `I1` `attempt-1` contract as a docs-only bind

- In the canonical `I1` artifact, state explicitly:
  - `Round: round-054`
  - `Roadmap item: I1`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility, convenience, or default-path widening.
- State that `I1` is a bind/selection stage only.
- State that `I1` does not itself authorize production edits, test edits,
  roadmap mutation, controller-state edits, bug-tracker edits, or subject
  widening.

### Task 2 - Restate the controlling accepted evidence chain and correct stale context without reopening prior work

- Use
  `orchestrator/rounds/round-053/review-record.json`
  as the authoritative acceptance proof that `H4` finalized as
  `attempt=1`, `attempt_verdict=accepted`, `stage_action=finalize`,
  `status=authoritative`, with canonical artifact path
  `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`.
- Use
  `orchestrator/rounds/round-053/review.md`
  to record that the stale open-bug sentence in
  `orchestrator/rounds/round-054/selection.md` is already known
  non-authoritative context drift. Preserve `selection.md` as the guider-owned
  task/order artifact, but do not reuse its stale bug claim as planning input.
- Use `/Volumes/src/mlf4/Bugs.md` as the canonical current bug state:
  - `Open` is empty; and
  - `BUG-2026-03-16-001` is resolved continuity context only.
- Carry forward the accepted `H4` non-authorization exactly:
  no replay reopen, no `MLF.Elab.Inst` / `InstBot`, no `boundVarTarget`, and
  no non-local widening are lawful `I1` successors.
- Carry forward the accepted bounded predecessor chain without reopening it:
  - `C1` / `C2` / `C3` / `C4` established the local `rootBindingIsLocalType` /
    `schemeBodyTarget` / non-local fail-closed baseline;
  - `E1` / `E2` / `E3` / `E4` consumed the same-lane retained-child
    `boundVarTarget` family;
  - `F1` / `F2` / `F3` / `F4` consumed the local
    `rootLocalSchemeAliasBaseLike` family;
  - `G1` / `G2` / `G3` / `G4` consumed the local `rootLocalMultiInst` family;
  - `H1` / `H2` / `H3` / `H4` consumed the local
    `rootLocalInstArgMultiBase` family and preserved `baseTarget` rejection
    outside that lane.
- State explicitly that the accepted local `keepTargetFinal` families are
  exhausted. `I1` therefore must not fabricate replay reopen, non-local
  widening, or another completed local family as a successor.

### Task 3 - Freeze exactly one bounded successor slice and reject all broader alternatives

- In the `I1` artifact, record exactly one next bounded successor slice:
  the local-binding single-base `baseTarget -> baseC` fail-closed hardening
  lane in
  `src/MLF/Elab/Run/ResultType/Fallback.hs:367-408`
  together with its final target-selection use at
  `src/MLF/Elab/Run/ResultType/Fallback.hs:681-701`.
- Freeze the future semantic boundary for that one lane only:
  - the future reviewer-auditable proof must stay local-binding-only and be
    derived from the existing ingredients already visible in `Fallback.hs`:
    `rootBindingIsLocalType`,
    singleton `rootBoundCandidates`,
    `not rootHasMultiInst`,
    and `not instArgRootMultiBase`;
  - the future selected behavior may harden only the one local `baseTarget`
    path that yields a single base-like target candidate; and
  - non-local roots, replay lanes, retained-child `boundVarTarget`, completed
    `rootLocal*` families, and every multi-base or cross-family case must stay
    fail-closed or inherited context only.
- Freeze future ownership to exactly these files only:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
- Freeze future focused verification intent to one bounded `ARI-C1` extension
  only:
  - one local single-base success example that exercises the selected
    `baseTarget` lane on a local `TypeRef` root;
  - one matched fail-closed contrast that keeps the same-looking wrapper
    rejected once it becomes non-local or otherwise leaves the selected
    singleton-base lane; and
  - one source-guard assertion naming the selected local single-base authority
    while preserving `schemeBodyTarget`, `boundVarTarget`,
    `rootLocalSchemeAliasBaseLike`, `rootLocalMultiInst`, and
    `rootLocalInstArgMultiBase` as inherited context only.
- State explicitly that the selected slice does **not** authorize:
  - replay reopen or any `MLF.Elab.Inst` / `InstBot` work;
  - `boundVarTarget` or non-local fallback widening;
  - reopening the accepted `C`, `E`, `F`, `G`, or `H` chains;
  - selection of `boundTarget` overlay materialization or
    `src/MLF/Elab/Run/ResultType/View.hs`;
  - `schemeBodyTarget` consolidation or reopening the accepted
    local/non-local baseline;
  - any second interface, compatibility shim, convenience fallback, or
    default-path widening; or
  - equi-recursive reasoning, implicit unfolding, cyclic structural graph
    encoding, or cross-family widening.

### Task 4 - Author the canonical `I1` bind artifact

- Write the canonical artifact at:
  `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
- The artifact should include:
  - the `I1` metadata and stage-contract freeze from Task 1;
  - an explicit correction that current bug authority comes from
    `/Volumes/src/mlf4/Bugs.md` and the accepted `H4` review/artifact, not the
    stale open-bug sentence in `selection.md`;
  - the accepted `C` / `E` / `F` / `G` / `H` predecessor chain summarized as
    inherited context only;
  - explicit rationale that no still-unopened local `keepTargetFinal` family
    remains, so replay reopen and other forbidden families are not lawful
    successors;
  - the exact selected future successor slice from Task 3;
  - the frozen future ownership and focused verification intent from Task 3;
  - an explicit statement that no second implementation family is selected; and
  - the retry-aware reviewer handoff fields expected by the repo-local
    contracts.

### Task 5 - Run and record docs-only verification for `I1`

- Required baseline checks:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-054/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-054/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-021/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-021/retry-subloop.md`
- `I1`-specific continuity checks:
  - `test -f docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
  - `test -f docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
  - `test -f docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  - `test -f docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
  - `python3 -m json.tool orchestrator/rounds/round-053/review-record.json >/dev/null`
  - short `python3` assertion over
    `orchestrator/rounds/round-053/review-record.json` confirming
    `H4` / `attempt-1` / `accepted` / `finalize` / `authoritative` and the
    canonical `H4` artifact path
  - `rg -n 'stale guider text|non-authoritative context drift|resolved only' orchestrator/rounds/round-053/review.md`
    -> pass
  - short `python3` check over `/Volumes/src/mlf4/Bugs.md` confirming:
    - the `Open` section is empty; and
    - `BUG-2026-03-16-001` is resolved
- Source/test anchor checks proving the chosen slice is current and still
  unselected:
  - `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '367,408p'`
    -> capture the existing `baseTarget` branch and its
    `rootBoundCandidates` / `rootHasMultiInst` / `instArgRootMultiBase`
    guards
  - `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '681,701p'`
    -> capture `targetC` checking `baseTarget` before retained-target and
    `schemeBodyTarget` fallbacks
  - `sed -n '1102,1597p' test/PipelineSpec.hs | rg -n 'keeps retained-child|scheme-alias/base-like|multi-inst|inst-arg multi-base|non-local proxy'`
    -> pass
  - `sed -n '1102,1597p' test/PipelineSpec.hs | rg -n 'baseTarget|boundTarget|rootBoundCandidates'`
    -> no output, proving the focused `ARI-C1` block does not yet select the
    chosen single-base lane
- Post-edit docs-only diff checks:
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
    -> no output
- Full-gate skip note:
  - record explicitly that `cabal build all && cabal test` is intentionally
    skipped because `I1` is docs-only and does not edit `src/`, `src-public/`,
    `app/`, `test/`, or `mlf2.cabal`.

## Non-Goals

- No replay reopen, `MLF.Elab.Inst`, or `InstBot` work.
- No selection of `boundVarTarget`, non-local widening, or any cross-family
  recursive-inference lane.
- No reopening of the accepted `C`, `E`, `F`, `G`, or `H` chains as live work.
- No selection of `boundTarget` overlay materialization or
  `MLF.Elab.Run.ResultType.View.hs`.
- No reopening of the accepted `schemeBodyTarget` / local-vs-non-local
  baseline from the `C` cycle.
- No production, test, public API, executable, Cabal, roadmap, controller, or
  bug-tracker edits during `I1`.

## Reviewer Checks

1. The canonical `I1` artifact is docs-only, names `attempt-1`, preserves the
   inherited boundary, and states that `I1` is bind/selection only.
2. The artifact explicitly corrects the stale open-bug sentence from
   `selection.md` by grounding current authority in `/Volumes/src/mlf4/Bugs.md`,
   the accepted `H4` review record, and the accepted `H4` artifact.
3. The artifact carries forward the accepted `C` / `E` / `F` / `G` / `H`
   predecessor chain as inherited context only and states that no still-open
   local `keepTargetFinal` family remains.
4. The artifact freezes exactly one future successor slice: the local-binding
   single-base `baseTarget -> baseC` lane in `Fallback.hs`, with future
   ownership limited to `Fallback.hs` and `PipelineSpec.hs`.
5. The artifact does not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
   `boundVarTarget`, non-local widening, `boundTarget` overlay materialization,
   `ResultType.View` changes, or any broader trigger-family widening.
6. Docs-only verification records pass, and the full-gate skip note is
   explicit and justified by file scope.
