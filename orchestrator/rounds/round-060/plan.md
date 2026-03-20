# Round 060 Plan (`J3` Bounded Verification Gate)

## Objective

Execute only roadmap item `J3` and prepare one accepted docs/evidence artifact
at:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`.

This is the initial `J3` plan for `attempt-1` with `retry: null`. The round
must stay docs-only and reverify only the accepted `J2` local-binding
inst-arg-only singleton-base
`rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
lane under repaired `URI-R2-C1`.

`J3` must record current bounded evidence from exactly these surfaces:

1. read-only anchor checks in
   `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/test/PipelineSpec.hs`
   for the accepted `rootLocalInstArgSingleBase` /
   `baseTarget -> baseC` / same-lane `targetC` lane only;
2. one fresh rerun of
   `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`;
3. one fresh full repo gate via `cabal build all && cabal test`;
4. predecessor continuity checks anchored to accepted `J1`, accepted `J2`,
   accepted `I4`, and the inherited boundary docs; and
5. docs-only diff-scope checks proving the round itself does not authorize or
   land code/test/public-API/executable/Cabal edits.

`attempt-1` remains docs-only. No production, test, public API, executable,
Cabal, roadmap, bug-tracker, or controller-state edits are planned or
authorized in this verification gate. If any verification step fails, capture
the blocker in the canonical `J3` artifact and leave the failure for reviewer
judgment; do not patch `Fallback.hs`, `PipelineSpec.hs`, or any other source
file inside this round.

## Locked Round Context

- Round id: `round-060`
- Roadmap item: `J3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no second interface / no compatibility or convenience fallback widening`
- Stage mode: docs-only bounded verification/evidence consolidation only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Accepted carry-forward facts that must remain binding throughout `J3`:

- `I4` authoritative `round-057` `attempt-2` finalized
  `continue-bounded`, so successor work must remain one more bounded
  non-widening step only and must begin from accepted continuity rather than
  reopening `I1` / `I2` / `I3`.
- `J1` authoritative `round-058` `attempt-1` froze exactly one future `J2`
  slice: the local-binding inst-arg-only singleton-base
  `baseTarget -> baseC` lane plus its same-lane `targetC` use in
  `Fallback.hs`, with future ownership limited to `Fallback.hs` and
  `PipelineSpec.hs`.
- `J2` authoritative `round-059` `attempt-1` landed only that bounded slice,
  introduced
  `rootLocalInstArgSingleBase = rootBindingIsLocalType && IntSet.null rootBaseBounds && IntSet.size instArgBaseBounds == 1 && not rootHasMultiInst && not instArgRootMultiBase`,
  added the dedicated `targetC` arm for that same local lane, extended the
  focused `ARI-C1` block with the matching helper/examples/source guard, and
  passed both the focused rerun and the full repo gate.
- The completed `rootLocalSingleBase` lane, the preserved
  scheme-alias/base-like `baseTarget` route, `rootLocalMultiInst`,
  `rootLocalInstArgMultiBase`, retained-target / `keepTargetFinal` behavior,
  `boundVarTarget`, replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundTarget`, `schemeBodyTarget`, `ResultType.View`, and non-local
  fail-closed behavior remain inherited context only and are not reopened as
  `J3` targets.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only unless it shows
  a live blocker. Its `Open` section is currently empty, so it does not
  authorize any detour away from the accepted `J1` / `J2` lane.

Current repository state is already non-pristine:

- `M orchestrator/state.json`
- `?? orchestrator/rounds/round-060/selection.md`

Treat those as pre-existing controller/guider preparation. Respect them and do
not revert, rewrite, or expand them while executing `J3`.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/AGENTS.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/roles/planner.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/rounds/round-060/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/rounds/round-057/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/rounds/round-058/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/rounds/round-059/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`

## File Ownership For This Round

### Round-Authored File

- `docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
  - Responsibility: the sole canonical `J3` verification/evidence artifact for
    this round.

### Read-Only Verification Anchors

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `orchestrator/rounds/round-057/review-record.json`
- `orchestrator/rounds/round-058/review-record.json`
- `orchestrator/rounds/round-059/review-record.json`
- `orchestrator/state.json`
- `orchestrator/rounds/round-060/selection.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- `/Volumes/src/mlf4/Bugs.md`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/rounds/round-060/selection.md`
- `orchestrator/rounds/round-060/implementation-notes.md`
- `orchestrator/rounds/round-060/review.md`
- `orchestrator/rounds/round-060/review-record.json`
- `orchestrator/rounds/round-060/merge.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `/Volumes/src/mlf4/Bugs.md`

## Sequential Tasks

### Task 1 - Reconfirm the locked `J3` authority chain before any rerun

- Re-read the accepted `I4`, `J1`, and `J2` artifacts plus their
  authoritative review records and restate the binding facts inside the
  canonical `J3` artifact.
- Confirm from
  `orchestrator/rounds/round-057/review-record.json`,
  `orchestrator/rounds/round-058/review-record.json`, and
  `orchestrator/rounds/round-059/review-record.json` that:
  - `I4`, `J1`, and `J2` each finalized with
    `attempt_verdict: "accepted"`,
    `stage_action: "finalize"`,
    `status: "authoritative"`;
  - the canonical artifact paths are exactly:
    - `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
    - `docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
    - `docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- Reconfirm from the accepted artifacts that `J3` answers only one bounded
  question: whether the accepted `J2`
  `rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
  lane still looks stable under current read-only anchors, the fresh focused
  rerun, the fresh full repo gate, and predecessor continuity rechecks.
- Reconfirm the inherited boundary docs remain continuity authority only:
  `baseline-contract`, `research-stop`, repaired `URI-R2-C1` gate,
  accepted `U6 = continue-bounded`, the approved continue-bounded cycle
  design, and the approved continue-bounded `H`-cycle design.
- Reconfirm `/Volumes/src/mlf4/Bugs.md` is continuity context only for `J3`.
  If it unexpectedly shows a new open blocker that directly invalidates the
  accepted `J2` verification lane, record that blocker in the canonical `J3`
  artifact and stop. Do not treat resolved bugs or historical notes as
  widening authority.

Recommended evidence commands for this step:

- `python3 -m json.tool orchestrator/rounds/round-057/review-record.json`
- `python3 -m json.tool orchestrator/rounds/round-058/review-record.json`
- `python3 -m json.tool orchestrator/rounds/round-059/review-record.json`
- `test -f docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`

### Task 2 - Collect read-only anchor evidence for the accepted `J2` lane

- Inspect the existing selected-lane logic in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - the accepted inst-arg-only singleton-base `baseTarget -> baseC` branch at
    `Fallback.hs:382-387`;
  - the selected local proof
    `rootLocalInstArgSingleBase` at `Fallback.hs:531-536`;
  - the completed adjacent local single-base proof
    `rootLocalSingleBase` at `Fallback.hs:541-545` as inherited context only,
    not reopened `J3` authority;
  - the preserved retained-target family guard
    `keepTargetFinal` at `Fallback.hs:685-691`, proving the selected `J3`
    lane did not widen into retained-target logic; and
  - the final `targetC` selection block at `Fallback.hs:692-700`, especially
    the same-lane `rootLocalSingleBase` arm at `Fallback.hs:694-695`, the
    selected `rootLocalInstArgSingleBase` arm at `Fallback.hs:696-697`, and
    the preserved scheme-alias/base-like `baseTarget` route at
    `Fallback.hs:698-700`.
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing the accepted bounded block still
  includes:
  - the helper
    `localInstArgSingleBaseFallback` at `PipelineSpec.hs:1348-1398`;
  - the adjacent completed `localSingleBaseFallback` helper at
    `PipelineSpec.hs:1399-1425` as inherited context only;
  - the accepted positive local example
    `keeps local inst-arg-only singleton-base fallback on the local TypeRef lane`
    at `PipelineSpec.hs:1629-1631`;
  - the matched non-local fail-closed contrast
    `keeps the same inst-arg-only singleton-base wrapper fail-closed once it leaves the local TypeRef lane`
    at `PipelineSpec.hs:1633-1641`;
  - the source-guard assertions at `PipelineSpec.hs:1660-1712` naming
    `rootLocalSingleBase`, `rootLocalInstArgSingleBase`, the dedicated
    `targetC` arm, preserved `keepTargetFinal`, and the inherited retained
    families; and
  - the inherited non-local pipeline-entrypoint fail-closed check beginning at
    `PipelineSpec.hs:1714`.
- Use the anchor evidence to prove that `J3` remains bound to the exact
  accepted `J2` lane and has not silently widened into replay, retained-child,
  non-local, or broader trigger-family behavior.

Recommended evidence commands for this step:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '382,700p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1348,1725p'`

### Task 3 - Re-run the bounded verification suite required for `J3`

- Run the baseline checks required by
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/orchestrator/verification.md`:
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
- Run exactly one fresh focused rerun of the bounded prototype block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Run exactly one fresh full repo gate:
  - `cabal build all && cabal test`
- If any command fails:
  - record the failing command, stderr excerpt, and why it blocks `J3`;
  - keep the round docs-only;
  - do not patch code/tests/public APIs/Cabal; and
  - leave the failure for reviewer judgment under the retry contract.

### Task 4 - Run predecessor continuity and docs-only diff checks

- Run predecessor continuity checks proving the exact accepted `I4` / `J1` /
  `J2` chain still governs the live `J3` lane:
  - `python3 -m json.tool orchestrator/rounds/round-057/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-058/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-059/review-record.json >/dev/null`
  - short `python3` assertions or equivalent `jq`/`rg` checks confirming
    `stage_id`, `attempt`, `attempt_verdict`, `stage_action`, `status`, and
    `artifact_path` for all three reviewer records
- Run inherited-boundary continuity checks proving the round is still inside
  repaired `URI-R2-C1` and has not reinterpreted accepted negatives as
  widening clearance:
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- Run docs-only diff-discipline checks before finishing the artifact:
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- In the canonical `J3` artifact, distinguish:
  - pre-existing controller/guider preparation
    (`orchestrator/state.json` and `orchestrator/rounds/round-060/selection.md`);
  - planner-owned round doc `orchestrator/rounds/round-060/plan.md`; and
  - the implement-stage canonical `J3` artifact
    `docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`.
- Treat any code/test/public-API/executable/Cabal diff as a blocker for this
  verification gate, not as something to repair inside `J3`.

### Task 5 - Author the canonical `J3` verification artifact

- Write
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060/docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
  as the sole canonical `J3` evidence artifact.
- Include reviewer-auditable sections covering:
  - stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
    `Retry state`, `Live subject`, `Artifact kind`);
  - a stage-contract freeze stating that `J3` re-verifies only the accepted
    `J2` `rootLocalInstArgSingleBase` / `baseTarget -> baseC` /
    same-lane `targetC` lane and does not reopen implementation or preempt
    `J4`;
  - accepted `I4` / `J1` / `J2` continuity, including the canonical
    review-record evidence and the preserved exclusions;
  - read-only anchor evidence from `Fallback.hs` and `PipelineSpec.hs`,
    including the selected local proof, the dedicated `targetC` arm, the
    matched non-local fail-closed contrast, and the preserved neighboring
    families as inherited context only;
  - verification runs and outcomes for the baseline checks, focused rerun,
    full repo gate, predecessor continuity checks, inherited-boundary checks,
    and docs-only diff checks; and
  - a result section stating either that the exact accepted `J2` lane remains
    stable and ready for review, or that a blocker was found and recorded
    without widening the live subject.
- Make the artifact explicit that `J3` does not authorize:
  - `J4` decision-making before reviewer acceptance;
  - any production, test, public API, executable, or Cabal repair;
  - reopening accepted `I1` / `I2` / `I3` / `I4`, `J1`, or `J2`;
  - reopening `rootLocalSingleBase`, the preserved scheme-alias/base-like
    `baseTarget` route, `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
    `keepTargetFinal`, or `boundVarTarget` as new live target families;
  - replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundTarget`,
    `schemeBodyTarget`, or `ResultType.View` work;
  - non-local widening, multi-SCC widening, cross-family widening,
    equi-recursive reasoning, implicit unfolding, cyclic structural graph
    encoding, or any second-interface / compatibility / convenience fallback
    path; or
  - any reinterpretation of accepted `U2` / `U3` / `U4` negatives as widening
    clearance.

### Task 6 - Keep retry semantics and artifact ownership intact

- Treat this plan as the first-attempt `J3` plan only.
- Because `orchestrator/state.json.retry` is `null`, do not write a retry
  delta; write the full `attempt-1` plan and keep retry handling deferred
  unless a later review emits `stage_action: retry`.
- If a later retry occurs, the revised planner must preserve this artifact as
  historical context and write only the delta required by the recorded
  `fix_hypothesis`.
- Do not rewrite reviewer-owned history, controller-owned logs, roadmap
  history, selection state, or bug-tracker history as part of `J3`.
- Do not write `implementation-notes.md`, `review.md`, `review-record.json`,
  or `merge.md` during this planner stage.

## Non-Authorization

This `attempt-1` `J3` plan does not authorize:

- any change to `orchestrator/state.json`;
- any rewrite of `orchestrator/roadmap.md` or
  `orchestrator/rounds/round-060/selection.md`;
- any edit in `src/`, `test/`, `src-public/`, `app/`, or `mlf2.cabal`;
- any reopening of accepted `I1` / `I2` / `I3` / `I4`, `J1`, or `J2`;
- any widening beyond repaired `URI-R2-C1`;
- any reinterpretation of accepted `U2` / `U3` / `U4` negatives as widening
  clearance;
- any replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening,
  `boundTarget` overlay materialization, `schemeBodyTarget`,
  `ResultType.View`, non-local widening, equi-recursive reasoning, implicit
  unfolding, cyclic structural graph encoding, multi-SCC widening,
  cross-family widening, second executable interface, or compatibility /
  convenience / default-path widening; or
- any simulation of implement/review/merge-stage results inside this plan.
