# Round 073 Plan (`N6` Verification/Evidence Consolidation For The Accepted `N5` Non-Local `baseTarget -> baseC` Proof Slice)

## Objective

Execute only roadmap item `N6` and produce one accepted docs-only
verification artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`.

This is the initial `N6` plan for `attempt-1` with `retry: null`. The round
must consolidate verifier-visible evidence for the exact accepted `N5`
non-local generic scheme-root alias-bound / base-like `baseTarget -> baseC`
proof slice only, under the still-active accepted `N3` contract. `N6` is a
docs-only verification/evidence gate. It must not widen into a new target,
new implementation work, replay reopen, or `N7` closure/decision work.

`N6` must re-verify the accepted `N5` slice across all required evidence
surfaces:

1. read-only source anchors in
   `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/src/MLF/Elab/Run/ResultType/Fallback.hs`
   showing the explicit `rootNonLocalSchemeAliasBaseLike` proof, the preserved
   local proof cluster, and the dedicated same-lane `targetC` arm;
2. read-only focused test anchors in
   `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/test/PipelineSpec.hs`
   showing `schemeAliasBaseLikeFallback False`, the preserved local
   `schemeAliasBaseLikeFallback True` contrast, and the source guard that keeps
   the non-local proof separate from the local lanes;
3. fresh focused verification for the bounded `ARI-C1 feasibility characterization (bounded prototype-only)` block;
4. a fresh full repo gate via `cabal build all && cabal test`; and
5. predecessor continuity checks against the accepted `N5` review record and
   canonical `N5` artifact, while restating that the active `N3` contract
   remains binding.

If any verification step fails, record the blocker in the canonical `N6`
artifact and optional round notes, then stop. Do not patch
`Fallback.hs`, `PipelineSpec.hs`, or any other production/test file inside
this round.

## Locked Round Context

- Round id: `round-073`
- Roadmap item: `N6`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: exact accepted `N5` non-local proof slice for the
  preserved generic scheme-root alias-bound / base-like
  `baseTarget -> baseC` packet
- Active branch: `codex/round-073-n6-verification-evidence`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073`
- Stage mode: docs-only bounded verification/evidence consolidation only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `?? orchestrator/rounds/round-073/selection.md`

## Accepted Continuity That Remains Binding

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/orchestrator/rounds/round-073/selection.md`
  already fixes this round to `N6` only and forbids using it for new
  implementation, target selection, replay relitigation, roadmap/state edits,
  bug-tracker edits, or predecessor-history rewrites.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` fixes the
  live controller state at `active_round_id: "round-073"`, `stage: "plan"`,
  `current_task: "N6"`, `branch:
  "codex/round-073-n6-verification-evidence"`, and `retry: null`.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` makes
  `N6` the first pending item after accepted `N5`; `N7` depends on accepted
  `N6` evidence and therefore cannot run first.
- `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  still records `N6 = NO`, so this round must stay evidence-only for the
  accepted `N5` slice and must not drift into a decision token.
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
  remains the active `N3` contract. `N6` must keep the following axes explicit
  and unchanged:
  - alias-bound ownership remains local to one owner-binder / owned-bound pair;
  - bound inlining remains inverse-translation-safe only;
  - binding-flag reconstruction remains justified by structural/variance
    evidence only;
  - recursive meaning remains explicit-only, non-equi-recursive, and
    non-cyclic-graph; and
  - confinement remains fixed to the one preserved non-local `baseTarget`
    packet.
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
  remains the accepted `N4` bind. It froze exactly one packet: the preserved
  non-local generic scheme-root alias-bound / base-like
  `baseTarget -> baseC` packet plus its downstream same-lane `targetC`
  consumer.
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
  remains the accepted `N5` implementation record. It states that the generic
  `baseTarget` computation stayed unchanged, the explicit
  `rootNonLocalSchemeAliasBaseLike` proof was added, and only the same-lane
  generic `targetC` consumer now routes through that proof.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review-record.json`
  remains the authoritative acceptance proof that `N5` finalized as
  `attempt = 1`, `attempt_verdict = accepted`, `stage_action = finalize`,
  `status = authoritative`, and
  `final_outcome = "baseTarget-non-local-proof-slice-established"`.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review.md`
  remains the authoritative reviewer-owned evidence that the focused `ARI-C1`
  block and the full repo gate both passed for the accepted `N5` slice.
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` still carries open
  `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains read-only
  predecessor context only and does not authorize replay reopen,
  `MLF.Elab.Inst`, or any different subject in this round.
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
  allows retries for `N6`, but prior attempts remain immutable. This
  `attempt-1` plan must therefore define one bounded verification packet
  without rewriting any prior attempt or review artifact.

## File Map

### Modify

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`
  - Responsibility: canonical `N6` verification/evidence record for the exact
    accepted `N5` slice only, including fresh command evidence, read-only
    anchor evidence, predecessor continuity, and explicit non-authorization.

### Optional Modify

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/orchestrator/rounds/round-073/implementation-notes.md`
  - Responsibility: bounded command transcript / blocker capture file only if
    long outputs need a round-local home.

### Read-Only Evidence

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/test/PipelineSpec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`

### Preserve Unchanged

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/src/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/test/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/src-public/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/app/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/mlf2.cabal`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/orchestrator/rounds/round-073/selection.md`
- reviewer-owned history under
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-001/`
  through
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/`

No edit to `Fallback.hs`, `PipelineSpec.hs`, `src-public/`, `app/`, or
`mlf2.cabal` is authorized during `N6` `attempt-1`. If reverification fails,
record the blocker and stop.

## Exact Selected `N6` Slice (Exactly One)

The only selected `N6` slice is:

record current verifier-visible evidence for the exact accepted `N5` non-local
`rootNonLocalSchemeAliasBaseLike` proof slice only, meaning the existing
explicit proof in `Fallback.hs`, its dedicated same-lane `targetC` consumer,
the focused `schemeAliasBaseLikeFallback False` / `True` and source-guard
anchors in `PipelineSpec.hs`, fresh focused/full gate results, and continuity
to the accepted `N3`, `N4`, and `N5` records.

Required interpretation of that one bounded slice:

- `N6` is not another implementation slice. It re-verifies only the already
  accepted `N5` production/test packet.
- The live source anchors to cite are the existing proof cluster and consumer
  order in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/src/MLF/Elab/Run/ResultType/Fallback.hs`,
  in particular:
  - `rootLocalInstArgSingleBase` at line `531`;
  - `rootLocalEmptyCandidateSchemeAliasBaseLike` at line `537`;
  - `rootNonLocalSchemeAliasBaseLike` at line `545`;
  - `rootLocalSchemeAliasBaseLike` at line `549`;
  - `rootLocalSingleBase` at line `553`;
  - `keepTargetFinal` at line `697`; and
  - the `targetC` branch ordering beginning at line `705`, with
    `rootLocalEmptyCandidateSchemeAliasBaseLike` at line `711` and
    `rootNonLocalSchemeAliasBaseLike` at line `713`.
- The live focused test anchors to cite are the existing helper/test block in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/test/PipelineSpec.hs`,
  in particular:
  - the `ARI-C1` block at line `1103`;
  - `schemeAliasBaseLikeFallback` at line `1244`;
  - `localEmptyCandidateSchemeAliasBaseLikeFallback` at line `1270`;
  - the preserved local continuity assertion using
    `schemeAliasBaseLikeFallback True` at line `1599`;
  - the selected non-local packet assertion using
    `schemeAliasBaseLikeFallback False` at lines `1622-1623`; and
  - the source guard beginning at line `1698`.
- `N6` must restate that the accepted local empty-candidate lane, accepted
  local continuity lane, completed adjacent singleton-base lanes, and every
  blocked route remain continuity context only, not new live work.
- `N6` must not reinterpret fresh green verification as clearance for:
  - `N7`;
  - replay reopen;
  - `MLF.Elab.Inst` or `InstBot`;
  - `boundVarTarget`, `boundTarget`, or `schemeBodyTarget`;
  - `src/MLF/Elab/Run/ResultType/View.hs`;
  - any other fallback family;
  - any different solver/pipeline subject;
  - cross-family search;
  - equi-recursive reasoning or implicit unfolding;
  - cyclic structural graph encoding, graph-cycle exceptions, or multi-SCC
    support; or
  - any second interface, compatibility shim, convenience fallback, or default
    path widening.

## Sequential Tasks

### Task 1 - Freeze the `N6` `attempt-1` contract as a docs-only verification gate

- Write the canonical `N6` artifact as `attempt-1` with `retry: null`.
- State explicitly that `N6` records only current verification/evidence for the
  accepted `N5` non-local proof slice and does not authorize:
  - production/test/public-API/executable/Cabal edits;
  - roadmap edits;
  - `orchestrator/state.json` edits;
  - bug-tracker edits;
  - predecessor-history rewrites; or
  - `N7` decision work.
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility / convenience / default-path widening.
- State that any verification failure is a blocker to record, not permission to
  patch `Fallback.hs`, `PipelineSpec.hs`, or any other code/test file during
  this attempt.

### Task 2 - Reconfirm the accepted `N3` / `N4` / `N5` authority chain without widening it

- Reconfirm the accepted `N3` contract from
  `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
  and restate that it still binds `N6`.
- Reconfirm the exact `N4` packet from
  `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
  and restate that `N6` is still confined to that exact packet.
- Reconfirm the accepted `N5` implementation artifact and review record:
  - `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review-record.json >/dev/null`
  - short `python3` assertion over
    `round-066` through `round-072` review records proving accepted
    authoritative continuity for `L1`, `L2`, `N1`, `N2`, `N3`, `N4`, and `N5`
  - `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
- State explicitly that the accepted `N5` slice still means:
  - `baseTarget` computation unchanged;
  - explicit `rootNonLocalSchemeAliasBaseLike` proof present;
  - only the same-lane generic `targetC` consumer routed through that proof;
  - focused `schemeAliasBaseLikeFallback False` / `True` plus source guard
    remain the bounded test anchors.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` only as continuity
  context. Do not reinterpret open `BUG-2026-03-16-001` as current repair or
  widening authority.

### Task 3 - Collect read-only anchor evidence from the live code/test packet

- Inspect the existing selected-lane logic in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - `rootLocalInstArgSingleBase`;
  - `rootLocalEmptyCandidateSchemeAliasBaseLike`;
  - `rootNonLocalSchemeAliasBaseLike`;
  - `rootLocalSchemeAliasBaseLike`;
  - `rootLocalSingleBase`;
  - `keepTargetFinal`;
  - the `targetC` branch ordering showing the dedicated non-local arm; and
  - the fact that the accepted local lanes remain earlier / separate.
- Recommended anchor command:
  - `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '531,719p'`
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing:
  - the helper family remains in the same focused block;
  - `schemeAliasBaseLikeFallback True` still owns the preserved local
    continuity contrast;
  - `schemeAliasBaseLikeFallback False` still owns the selected non-local
    packet;
  - the source guard still requires the explicit non-local proof and keeps it
    separate from the preserved local lanes; and
  - adjacent completed local lanes remain inherited continuity only.
- Recommended anchor commands:
  - `nl -ba test/PipelineSpec.hs | sed -n '1244,1278p'`
  - `nl -ba test/PipelineSpec.hs | sed -n '1595,1718p'`

### Task 4 - Run the required bounded verification suite for `N6`

- Run the baseline checks required by
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`:
  - `git branch --show-current`
  - `git status --short --untracked-files=all`
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  - `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  - `test -f docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
  - `test -f docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
  - `test -f docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
  - `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  - `test -f orchestrator/retry-subloop.md`
- Run the focused bounded test block exactly:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Require the focused rerun to stay green and to remain bounded to the existing
  selected non-local packet plus preserved local contrasts.
- Run the mandatory fresh full repo gate exactly:
  - `cabal build all && cabal test`
- Even though `N6` is docs-only, do not skip the fresh full repo gate. This
  stage exists to record current verification evidence for the accepted `N5`
  slice, not merely restate prior green results.

### Task 5 - Re-run predecessor continuity and docs-only diff checks

- Re-run accepted-predecessor continuity checks against the authoritative
  implementation artifact and authoritative review record:
  - `python3 -m json.tool orchestrator/rounds/round-072/review-record.json >/dev/null`
  - `rg -n 'rootNonLocalSchemeAliasBaseLike|schemeAliasBaseLikeFallback False|schemeAliasBaseLikeFallback True|cabal build all && cabal test|20 examples, 0 failures' docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md orchestrator/rounds/round-072/review.md`
  - short reviewer-readable continuity summary over the accepted
    `round-072` review-record fields and `checks` map
- Re-run docs-only diff checks so the reviewer can confirm that `N6` did not
  patch code while reverifying code:
  - `git diff --name-only`
  - `git diff --name-only -- src test src-public app mlf2.cabal`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  - `git status --short --untracked-files=all`
- If any non-docs diff appears under `src/`, `test/`, `src-public/`, `app/`,
  or `mlf2.cabal`, stop and report a blocker instead of treating the round as
  a clean docs-only verification gate.

### Task 6 - Author the canonical `N6` artifact and reviewer handoff

- Write the canonical verification artifact at:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`.
- Structure the artifact so the reviewer can audit, in order:
  - stage contract freeze for `N6` `attempt-1`;
  - accepted `N3` / `N4` / `N5` authority chain carried forward without
    widening;
  - read-only anchor evidence from `Fallback.hs` and `PipelineSpec.hs`;
  - fresh baseline / focused-block / full-gate results;
  - accepted-`N5` continuity recheck;
  - docs-only diff evidence;
  - stability conclusion or blocker section; and
  - explicit non-authorization preserving the active `N3` contract boundaries.
- Update
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073/orchestrator/rounds/round-073/implementation-notes.md`
  only if needed to hold long command output or blocker details. Do not create
  any other round artifact during implement stage.
- Ensure the reviewer can see, without inference, that:
  - this is the initial `N6` plan for `attempt-1` with `retry: null`;
  - the canonical `N6` artifact path is exact;
  - the focused `ARI-C1` rerun and `cabal build all && cabal test` gate are
    both fresh;
  - predecessor continuity was checked against the accepted `N5` artifact,
    `round-072` review record, and the active accepted `N3` contract;
  - `rootNonLocalSchemeAliasBaseLike` remains the selected proof anchor;
  - the selected non-local `targetC` arm remains bounded and separate from the
    preserved local lanes; and
  - no new target, implementation slice, or widening authority was inferred.
- Ensure reviewer outcomes remain the lawful retry-subloop set for this
  verification gate:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
