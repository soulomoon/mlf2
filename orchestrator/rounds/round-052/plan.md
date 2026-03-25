# Round 052 Plan (`H3` Bounded Verification Gate)

## Objective

Execute only roadmap item `H3` and prepare one accepted docs/evidence artifact
at:
`docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`.

This is the initial `H3` plan for `attempt-1` with `retry: null`. The round
must stay docs-only and reverify only the accepted `H2` local-binding
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane under repaired
`URI-R2-C1`.

`H3` must record current bounded evidence from exactly four surfaces:

1. read-only anchor checks in
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `test/PipelineSpec.hs`,
   proving the accepted local `rootLocalInstArgMultiBase` gate, the matched
   fail-closed non-local contrast, and preserved `baseTarget` rejection
   outside the selected lane are still the live bounded implementation;
2. a fresh rerun of the focused
   `ARI-C1 feasibility characterization (bounded prototype-only)` block in
   `test/PipelineSpec.hs`;
3. a fresh full repo gate via `cabal build all && cabal test`;
4. predecessor continuity checks anchored to the accepted `H1` / `H2` chain,
   the accepted `G`-cycle boundary, and the inherited non-widening control
   documents.

`attempt-1` remains docs-only. No production, test, public API, executable,
Cabal, roadmap, bug-tracker, or controller-state edits are planned or
authorized in this round. If any verification step fails, capture the blocker
in the canonical `H3` artifact and optional round notes; do not patch
`Fallback.hs`, `PipelineSpec.hs`, or any other code/test file inside this
verification gate.

## Locked Round Context

- Round id: `round-052`
- Roadmap item: `H3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Active worktree:
  `.worktrees/round-052`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no second interface / no fallback widening`
- Stage mode: docs-only bounded verification/evidence consolidation only

Accepted carry-forward facts that must remain binding throughout `H3`:

- `G4` authoritative `attempt-1` finalized `continue-bounded`, so the current
  `H` cycle remains one more bounded non-widening cycle only.
- `H1` authoritative `round-050` `attempt-1` froze exactly one future `H2`
  target: the local-binding `instArgRootMultiBase` `keepTargetFinal` /
  `targetC` lane in `Fallback.hs`, with future ownership limited to
  `Fallback.hs` and `PipelineSpec.hs`, while leaving `rootHasMultiInst`,
  `rootLocalSchemeAliasBaseLike`, and `boundVarTarget` as inherited context
  only.
- `H2` authoritative `round-051` `attempt-1` landed only that bounded slice,
  introduced
  `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`,
  routed `keepTargetFinal` and `targetC -> rootFinal` through that one local
  multi-base lane, preserved `baseTarget` rejection outside the selected lane,
  added exactly one local `TypeRef` success example plus one matched non-local
  fail-closed contrast in the focused `ARI-C1` block, and passed the accepted
  full gate.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. The open replay
  bug does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget` widening, non-local widening, or any broader recursive
  inference in this round.

Current repository state is already non-pristine (`orchestrator/rounds/round-052/state-snapshot.json`
modified and `orchestrator/rounds/round-052/selection.md` untracked). Respect
those existing changes. Do not revert or "clean up" unrelated work while
collecting `H3` evidence.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-052/state-snapshot.json`
- `orchestrator/rounds/round-052/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/roadmap.md`
- `orchestrator/rounds/round-049/review-record.json`
- `orchestrator/rounds/round-050/review-record.json`
- `orchestrator/rounds/round-051/review-record.json`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable verification artifact:

1. `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
   - canonical `H3` verification/evidence record.

Optional bounded note file:

1. `orchestrator/rounds/round-052/implementation-notes.md`
   - optional command transcript / blocker capture file if long outputs need a
     bounded home.

Read-only evidence anchors:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`
3. `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
4. `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
5. `orchestrator/rounds/round-050/review-record.json`
6. `orchestrator/rounds/round-051/review-record.json`
7. `/Volumes/src/mlf4/Bugs.md`

Files that must remain untouched by `H3` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-052/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/roadmap.md`
- `orchestrator/rounds/round-052/selection.md`
- `orchestrator/rounds/round-052/plan.md`
- `/Volumes/src/mlf4/Bugs.md`
- reviewer-owned history under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-051/`

No edit to `Fallback.hs` or `PipelineSpec.hs` is authorized during `H3`
`attempt-1`. If the accepted `H2` slice fails reverification, capture the
blocker with evidence and stop; do not repair the slice inside this docs-only
gate.

## Sequential Tasks

### Task 1 - Freeze the `H3` `attempt-1` contract as a docs-only verification gate

- In the canonical `H3` artifact, state explicitly:
  - `Round: round-052`
  - `Roadmap item: H3`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility, convenience, or default-path fallback or widening.
- State that `H3` verifies only the accepted `H2` local-binding
  `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane.
- State that `H3` does not reopen `H1` selection, does not reopen `H2`
  implementation, does not preempt `H4`, and does not authorize production or
  test edits during this attempt.
- State that any verification failure is a blocker to record, not permission to
  patch `Fallback.hs`, `PipelineSpec.hs`, or any other production/test file
  during this attempt.

### Task 2 - Reconstruct the accepted `H1` / `H2` evidence chain without widening it

- Use
  `orchestrator/rounds/round-050/review-record.json`
  as the authoritative acceptance proof that `H1` finalized as `attempt=1`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`,
  with canonical artifact path
  `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`.
- Use
  `orchestrator/rounds/round-051/review-record.json`
  as the authoritative acceptance proof that `H2` finalized as `attempt=1`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`,
  with canonical artifact path
  `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`.
- Carry forward the exact bounded `H2` slice recorded in
  `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`:
  - `rootLocalInstArgMultiBase` is the reviewer-auditable proof for the
    selected local-binding lane;
  - `keepTargetFinal` retains the final target only on that local lane;
  - `targetC` selects `rootFinal` only for that local lane;
  - the focused `ARI-C1` block contains exactly one local positive example and
    one matched non-local fail-closed contrast for the selected lane; and
  - `baseTarget` rejection remains preserved outside the selected lane.
- State explicitly that the accepted `H2` exclusions remain binding in `H3`:
  - `rootHasMultiInst` remains inherited context only;
  - `rootLocalSchemeAliasBaseLike` remains inherited context only;
  - `boundVarTarget` widening remains out of scope;
  - non-local widening remains out of scope;
  - no replay reopen;
  - no `MLF.Elab.Inst`;
  - no `InstBot`;
  - no equi-recursive reasoning;
  - no cyclic structural encoding;
  - no second executable interface; and
  - no compatibility, convenience, or fallback-path widening.
- Treat `/Volumes/src/mlf4/Bugs.md` only as continuity context. Do not
  reinterpret replay-path bug history as current repair authority.
- State explicitly that `H3` answers only one bounded question:
  whether the accepted `H2` local `rootLocalInstArgMultiBase` /
  `targetC -> rootFinal` lane still looks stable under read-only anchor checks,
  the focused `ARI-C1` rerun, the fresh full repo gate, and predecessor
  continuity rechecks.

### Task 3 - Collect read-only evidence from the bounded code/test anchors

- Inspect the existing selected-lane logic in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - `rootLocalInstArgMultiBase` at lines `528-530`;
  - `rootLocalMultiInst` and `rootLocalSchemeAliasBaseLike` at lines
    `525-534` as inherited context only, not selected `H3` authority;
  - `boundVarTarget` at lines `625-673` as preserved inherited machinery that
    remains outside the selected lane;
  - `keepTargetFinal` at lines `674-680`, including the required
    `rootBindingIsLocalType` gate and the unchanged out-of-scope trigger
    families `rootLocalMultiInst`, `rootLocalSchemeAliasBaseLike`, and
    `boundVarTarget`; and
  - the final `targetC` selection branch at lines `681-701` that chooses
    `rootFinal` only when the local selected trigger family is present and
    otherwise preserves the inherited fail-closed branches plus `baseTarget`
    handling.
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing the accepted bounded block still
  includes:
  - the retained-child same-lane baseline checks at lines `1364-1479`;
  - the local scheme-alias/base-like success example and matched fail-closed
    contrast at lines `1481-1494`;
  - the local multi-inst success example and matched non-local fail-closed
    contrast at lines `1496-1514` as inherited context only;
  - the local `instArgRootMultiBase` local-`TypeRef` success example at lines
    `1516-1524`;
  - the matched non-local inst-arg multi-base fail-closed contrast at lines
    `1526-1534`;
  - the source-guard assertions naming `rootLocalInstArgMultiBase`,
    `rootLocalMultiInst`, `rootLocalSchemeAliasBaseLike`, and the selected
    `targetC -> rootFinal` branch at lines `1553-1585`; and
  - the inherited non-local proxy fail-closed entrypoint checks beyond the
    selected lane starting at line `1587`.
- Reconfirm from
  `orchestrator/rounds/round-050/review-record.json`
  and
  `orchestrator/rounds/round-051/review-record.json`
  that `H1` and `H2` finalized as authoritative accepted attempts with the
  expected artifact paths and no retry still pending.

Recommended evidence commands for this step:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '520,705p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1360,1588p'`
- `python3 -m json.tool orchestrator/rounds/round-050/review-record.json`
- `python3 -m json.tool orchestrator/rounds/round-051/review-record.json`

### Task 4 - Re-run the bounded verification suite required for `H3`

- Run the baseline checks required by
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-052/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-052/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-019/retry-subloop.md`
- Run one focused bounded rerun of the accepted prototype block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Run the mandatory full repo gate required by roadmap item `H3` completion
  notes and the accepted `G3` verification model:
  - `cabal build all && cabal test`
- Run predecessor continuity checks proving the exact `H1` / `H2` chain still
  governs the live lane:
  - `python3 -m json.tool orchestrator/rounds/round-050/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-051/review-record.json >/dev/null`
  - short `python3` assertions or equivalent `jq`/`rg` checks confirming
    `stage_id`, `attempt`, `attempt_verdict`, `stage_action`, `status`, and
    `artifact_path` for both reviewer records.
- Run docs-only diff discipline checks before finishing the artifact:
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- If any command fails:
  - record the failing command, stderr excerpt, and why it blocks `H3`;
  - keep the round docs-only;
  - do not patch code/tests; and
  - leave the failure for reviewer judgment under the retry contract.

### Task 5 - Author the canonical `H3` verification artifact

- Write
  `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  as the sole canonical `H3` evidence artifact.
- Include reviewer-auditable sections covering:
  - stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
    `Retry state`, `Live subject`, `Artifact kind`);
  - a stage-contract freeze stating that `H3` re-verifies only the accepted
    `H2` `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane and does
    not reopen implementation or preempt `H4`;
  - accepted `H1` / `H2` continuity, including the canonical review-record
    evidence and the preserved exclusions;
  - read-only anchor evidence from `Fallback.hs` and `PipelineSpec.hs`,
    including the preserved `baseTarget` rejection and matched non-local
    fail-closed contrast;
  - verification runs and outcomes for the baseline checks, focused bounded
    rerun, full repo gate, continuity checks, and docs-only diff checks; and
  - a result section stating either that the exact `H2` lane remains stable and
    ready for review, or that a blocker was found and recorded without widening
    the live subject.
- Make the artifact explicit that `H3` does not authorize:
  - `H4` decision-making before reviewer acceptance;
  - any production or test repair;
  - replay reopen, `MLF.Elab.Inst`, or `InstBot` work;
  - `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or `boundVarTarget` as
    separate target families;
  - non-local widening, equi-recursive reasoning, cyclic structural graph
    encoding, multi-SCC widening, cross-family widening, or any second
    interface / compatibility / convenience path.

### Task 6 - Keep retry semantics and artifact ownership intact

- Treat this plan as the first-attempt `H3` plan only.
- Because `orchestrator/rounds/round-052/state-snapshot.json.retry` is `null`, do not write a delta plan;
  write the full `attempt-1` plan and keep retry handling deferred unless a
  later review emits `stage_action: retry`.
- If a later retry occurs, the revised planner must preserve this artifact as
  historical context and write only the delta required by the recorded
  `fix_hypothesis`.
- Do not rewrite reviewer-owned history, controller-owned logs, roadmap
  history, or selection state as part of `H3`.
