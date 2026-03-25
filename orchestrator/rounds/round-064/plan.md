# Round 064 Plan (`K3` Bounded Verification Gate)

## Objective

Execute only roadmap item `K3` and prepare one accepted docs/evidence artifact
at:
`docs/plans/2026-03-21-uri-r2-c1-k3-bounded-verification-gate.md`.

This is the initial `K3` plan for `attempt-1` with `retry: null`. The round
must stay docs-only and reverify only the accepted `K2` local-binding
empty-candidate / no-inst-arg scheme-alias / base-like
`rootLocalEmptyCandidateSchemeAliasBaseLike` /
`baseTarget -> baseC` / same-lane `targetC` lane under repaired `URI-R2-C1`.

`K3` must record current bounded evidence from exactly these surfaces:

1. read-only anchor checks in
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `test/PipelineSpec.hs`
   for the accepted `K2` lane only;
2. one fresh rerun of
   `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`;
3. one fresh full repo gate via `cabal build all && cabal test`;
4. predecessor continuity checks anchored to accepted `J4`, accepted `K1`,
   accepted `K2`, the inherited boundary docs, and the accepted `K2`
   continuity checks; and
5. docs-only diff-scope checks proving the round itself does not authorize or
   land code, test, public-API, executable, Cabal, roadmap, controller-state,
   or bug-tracker edits.

`attempt-1` remains docs-only. No production, test, public API, executable,
Cabal, roadmap, bug-tracker, or controller-state edits are planned or
authorized in this verification gate. If any verification step fails, capture
the blocker in the canonical `K3` artifact and leave the failure for reviewer
judgment; do not patch `Fallback.hs`, `PipelineSpec.hs`, or any other source
file inside this round.

## Locked Round Context

- Round id: `round-064`
- Roadmap item: `K3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Active branch: `codex/round-064-k3-verification-gate`
- Active worktree:
  `.worktrees/round-064`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no second interface / no compatibility or convenience fallback widening`
- Stage mode: docs-only bounded verification/evidence consolidation only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Accepted carry-forward facts that must remain binding throughout `K3`:

- `J4` authoritative `round-061` `attempt-1` finalized
  `continue-bounded`, so successor work must remain one more bounded
  non-widening step only and must begin from accepted continuity rather than
  reopening `I4`, `J1`, `J2`, or `J3`.
- `K1` authoritative `round-062` `attempt-1` froze exactly one future `K2`
  slice: the local-binding empty-candidate / no-inst-arg scheme-alias /
  base-like `baseTarget -> baseC` lane plus its same-lane `targetC` use in
  `Fallback.hs`, with future ownership limited to `Fallback.hs` and
  `PipelineSpec.hs`.
- `K2` authoritative `round-063` `attempt-1` landed only that bounded slice,
  introduced
  `rootLocalEmptyCandidateSchemeAliasBaseLike = rootBindingIsLocalType && rootIsSchemeAlias && rootBoundIsBaseLike && IntSet.null rootBoundCandidates && IntSet.null instArgBaseBounds && not rootHasMultiInst && not instArgRootMultiBase`,
  added the dedicated same-lane `targetC` arm for that one local lane, added
  the matching local helper/example/source-guard evidence in the bounded
  `ARI-C1 feasibility characterization (bounded prototype-only)` block, and
  passed both the focused rerun and the full repo gate.
- The accepted `K2` continuity checks remain binding for `K3`: preserve the
  completed `rootLocalSingleBase` lane, the completed
  `rootLocalInstArgSingleBase` lane, the already-accepted
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane, the broader scheme-alias / base-like `baseTarget` route as inherited
  continuity only, and the explicit proof that `keepTargetFinal` does not
  widen into the selected `K2` lane.
- `boundVarTarget`, `boundTarget`, `schemeBodyTarget`, `ResultType.View`,
  replay reopen, `MLF.Elab.Inst`, `InstBot`, non-local widening, multi-SCC
  widening, cross-family widening, equi-recursive reasoning, implicit
  unfolding, cyclic structural encoding, second-interface work, and fallback
  widening remain inherited exclusions only and are not reopened as `K3`
  targets.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only unless it shows
  a live blocker. Its `## Open` section is currently empty, so it does not
  authorize any detour away from the accepted `K1` / `K2` lane.

Current repository state is already non-pristine:

- `M orchestrator/rounds/round-064/state-snapshot.json`
- `?? orchestrator/rounds/round-064/selection.md`
- `?? orchestrator/rounds/round-064/plan.md`

Treat those as the pre-existing controller/guider/planner preparation state.
Respect them and do not revert, rewrite, or expand them while executing `K3`
other than authoring the single canonical verification artifact.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-064/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/roadmap.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/verification.md`
- `orchestrator/rounds/round-064/selection.md`
- `orchestrator/rounds/round-063/selection.md`
- `orchestrator/rounds/round-063/plan.md`
- `orchestrator/rounds/round-063/implementation-notes.md`
- `orchestrator/rounds/round-063/review.md`
- `orchestrator/rounds/round-063/reviews/attempt-1.md`
- `orchestrator/rounds/round-063/review-record.json`
- `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Volumes/src/mlf4/Bugs.md`

## File Ownership For This Round

### Round-Authored File

- `docs/plans/2026-03-21-uri-r2-c1-k3-bounded-verification-gate.md`
  - Responsibility: the sole canonical `K3` verification/evidence artifact for
    this round.

### Read-Only Verification Anchors

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `orchestrator/rounds/round-064/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/roadmap.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/verification.md`
- `orchestrator/rounds/round-064/selection.md`
- `orchestrator/rounds/round-064/plan.md`
- `orchestrator/rounds/round-061/review-record.json`
- `orchestrator/rounds/round-062/review-record.json`
- `orchestrator/rounds/round-063/review-record.json`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `/Volumes/src/mlf4/Bugs.md`

### Preserve Unchanged

- `orchestrator/rounds/round-064/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/roadmap.md`
- `orchestrator/rounds/round-064/selection.md`
- `orchestrator/rounds/round-064/plan.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `/Volumes/src/mlf4/Bugs.md`

## Sequential Tasks

### Task 1 - Reconfirm the locked `K3` authority chain before any rerun

- Re-read the accepted `J4`, `K1`, and `K2` artifacts plus their
  authoritative review records and restate the binding facts inside the
  canonical `K3` artifact.
- Confirm from
  `orchestrator/rounds/round-061/review-record.json`,
  `orchestrator/rounds/round-062/review-record.json`, and
  `orchestrator/rounds/round-063/review-record.json` that:
  - `J4`, `K1`, and `K2` each finalized with
    `attempt_verdict: "accepted"`,
    `stage_action: "finalize"`,
    `status: "authoritative"`;
  - the canonical artifact paths are exactly:
    - `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
    - `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
    - `docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`
- Reconfirm from the accepted artifacts that `K3` answers only one bounded
  question: whether the accepted `K2`
  `rootLocalEmptyCandidateSchemeAliasBaseLike` local empty-candidate /
  no-inst-arg scheme-alias / base-like
  `baseTarget -> baseC` / same-lane `targetC` lane still looks stable under
  current read-only anchors, the fresh focused rerun, the fresh full repo
  gate, and predecessor continuity rechecks.
- Reconfirm the inherited boundary docs remain continuity authority only:
  `baseline-contract`, `research-stop`, repaired `URI-R2-C1` gate,
  accepted `U6 = continue-bounded`, and the approved continue-bounded cycle
  design.
- Reconfirm `/Volumes/src/mlf4/Bugs.md` is continuity context only for `K3`.
  If it unexpectedly shows a new open blocker that directly invalidates the
  accepted `K2` verification lane, record that blocker in the canonical `K3`
  artifact and stop. Do not treat resolved bugs or historical notes as
  widening authority.

Recommended evidence commands for this step:

- `python3 -m json.tool orchestrator/rounds/round-061/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-062/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-063/review-record.json >/dev/null`
- `test -f docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
- `test -f docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`

### Task 2 - Collect read-only anchor evidence for the accepted `K2` lane

- Inspect the existing selected-lane logic in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - the selected empty-candidate / no-inst-arg `baseTarget -> baseC` branch at
    `Fallback.hs:377-381`;
  - the selected local proof
    `rootLocalEmptyCandidateSchemeAliasBaseLike` at `Fallback.hs:537-544`;
  - the completed adjacent `rootLocalInstArgSingleBase` proof at
    `Fallback.hs:531-536` as inherited continuity only;
  - the already-accepted `rootLocalSchemeAliasBaseLike` continuity proof at
    `Fallback.hs:545-548` as inherited continuity only;
  - the completed adjacent `rootLocalSingleBase` proof at
    `Fallback.hs:549-553` as inherited continuity only;
  - the preserved retained-target family guard `keepTargetFinal` at
    `Fallback.hs:693-699`, proving the selected `K3` lane did not widen into
    retained-target logic; and
  - the final `targetC` selection block at `Fallback.hs:700-710`,
    especially the completed `rootLocalSingleBase` arm at `703-704`,
    the completed `rootLocalInstArgSingleBase` arm at `705-706`,
    the selected `rootLocalEmptyCandidateSchemeAliasBaseLike` arm at
    `707-707`, and the preserved broader scheme-alias / base-like route at
    `708-710`.
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing the accepted bounded block still
  includes:
  - the adjacent scheme-alias / base-like continuity helper in the existing
    wrapper family above the selected helper;
  - the selected helper
    `localEmptyCandidateSchemeAliasBaseLikeFallback` at
    `PipelineSpec.hs:1270-1303`;
  - the completed adjacent `localInstArgSingleBaseFallback` helper at
    `PipelineSpec.hs:1382-1432` as inherited continuity only;
  - the completed adjacent `localSingleBaseFallback` helper at
    `PipelineSpec.hs:1433-1459` as inherited continuity only;
  - the accepted positive local example
    `keeps local empty-candidate scheme-alias/base-like fallback on the local TypeRef lane`
    at `PipelineSpec.hs:1594-1596`;
  - the matched local continuity contrast
    `keeps the matched local scheme-alias/base-like continuity on the quantified rootFinal lane`
    at `PipelineSpec.hs:1598-1606`;
  - the completed adjacent single-base and inst-arg-only singleton-base
    examples at `PipelineSpec.hs:1608-1620` and `PipelineSpec.hs:1667-1679`
    as inherited continuity only; and
  - the source-guard assertions at `PipelineSpec.hs:1698-1758` naming
    `rootLocalEmptyCandidateSchemeAliasBaseLike`,
    `rootLocalSingleBase`,
    `rootLocalInstArgSingleBase`,
    the dedicated selected `targetC` arm,
    preserved `keepTargetFinal`,
    preserved `rootLocalSchemeAliasBaseLike`,
    and the broader scheme-alias / base-like route.
- Use the anchor evidence to prove that `K3` remains bound to the exact
  accepted `K2` lane and has not silently widened into replay,
  retained-target, non-local, or broader trigger-family behavior.

Recommended evidence commands for this step:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '377,710p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1244,1760p'`

### Task 3 - Re-run the bounded verification suite required for `K3`

- Run the baseline checks required by
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-064/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-064/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/retry-subloop.md`
- Re-run the fresh focused bounded verification command exactly once:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Re-run the fresh full repo gate exactly once:
  - `cabal build all && cabal test`
- Record the actual fresh results in the canonical `K3` artifact. Use the
  accepted `K2` verification posture (`20 examples, 0 failures` in the
  focused block and `1141 examples, 0 failures` in the full gate) as the
  comparison baseline, but report the current rerun output exactly rather than
  assuming counts.
- If any command fails, capture:
  - the exact command;
  - whether failure is baseline, focused rerun, full gate, continuity, or
    diff-scope;
  - the minimal failing symptom; and
  - why the failure blocks `K3` acceptance.
  Stop after documenting the blocker. Do not patch code, tests, docs outside
  the canonical `K3` artifact, or controller files.

### Task 4 - Prove docs-only diff scope and predecessor continuity

- Capture a pre-artifact or post-artifact `git status --short --untracked-files=all`
  snapshot and explicitly distinguish:
  - the pre-existing tracked `orchestrator/rounds/round-064/state-snapshot.json` modification;
  - the pre-existing untracked `orchestrator/rounds/round-064/selection.md`;
  - the pre-existing untracked `orchestrator/rounds/round-064/plan.md`; and
  - the one new canonical `K3` verification artifact.
- Prove the round stays docs-only:
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)orchestrator/rounds/round-064/state-snapshot.json'`
  - `git status --short --untracked-files=all`
- The required docs-only conclusion is:
  - no tracked diffs in `src/`, `test/`, `src-public/`, `app/`, or
    `mlf2.cabal`;
  - no writable ownership exercised over
    `src/MLF/Elab/Run/ResultType/Fallback.hs` or `test/PipelineSpec.hs`;
  - no roadmap, controller-state, or bug-tracker mutation; and
  - no widening beyond repaired `URI-R2-C1` plus the inherited
    explicit-only / non-equi-recursive / non-cyclic-graph /
    no-second-interface / no-fallback boundary.
- Re-run predecessor continuity checks so the canonical `K3` artifact records
  that no accepted authority drift accompanied this verification stage:
  - `test -d tasks/todo/2026-03-11-recursive-types-orchestration`
  - `for i in $(seq 1 33); do d=$(printf 'orchestrator/rounds/round-%03d' "$i"); test -d "$d" || exit 1; done`
  - `git diff --name-only -- orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/roadmap.md docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md orchestrator/rounds/round-061/review-record.json orchestrator/rounds/round-062/review-record.json orchestrator/rounds/round-063/review-record.json docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- Reconfirm `/Volumes/src/mlf4/Bugs.md` remains read-only continuity context
  with an empty `## Open` section and no round-local mutation.

### Task 5 - Author the canonical `K3` verification artifact

- Write only
  `docs/plans/2026-03-21-uri-r2-c1-k3-bounded-verification-gate.md`.
- Structure the artifact as a bounded verification/evidence record, not as an
  implementation note, roadmap update, or next-cycle decision:
  - stage contract freeze;
  - accepted `J4` / `K1` / `K2` continuity chain;
  - read-only anchor evidence with line references;
  - fresh verification commands and outcomes;
  - docs-only diff-scope evidence;
  - continuity/exclusion evidence; and
  - explicit non-authorization of widening or implementation reopen.
- The artifact must say clearly that `K3` does not:
  - reopen `K2` implementation;
  - reopen the completed `rootLocalSingleBase` lane;
  - reopen the completed `rootLocalInstArgSingleBase` lane;
  - reopen the accepted `rootLocalSchemeAliasBaseLike` /
    `targetC -> rootFinal` lane;
  - reopen replay, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
    `boundTarget`, `schemeBodyTarget`, or `ResultType.View`;
  - authorize non-local widening, cross-family widening, multi-SCC widening,
    equi-recursive reasoning, implicit unfolding, cyclic structural encoding,
    a second interface, or fallback widening; or
  - mutate `orchestrator/rounds/round-064/state-snapshot.json`, `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-031/roadmap.md`, or
    `/Volumes/src/mlf4/Bugs.md`.
- If every required check passes, the artifact should make the positive claim
  only that the accepted `K2` lane remains stable under current bounded
  evidence. It must not preempt `K4`, mutate the roadmap, or aggregate a new
  decision token.
- If a required check fails, the artifact should make the narrow negative
  claim only that current `K3` evidence is blocked, including the exact failed
  command and why that prevents acceptance. It must still avoid prescribing or
  landing code changes.

## Verification Handoff Notes

- Reviewer must be able to confirm that `K3` stayed docs-only and that
  `Fallback.hs` / `PipelineSpec.hs` remained read-only anchors.
- Reviewer must be able to confirm that the accepted `K2` continuity checks
  were carried forward unchanged, especially the preserved completed
  `rootLocalSingleBase` lane, the preserved completed
  `rootLocalInstArgSingleBase` lane, the already-accepted
  `rootLocalSchemeAliasBaseLike` lane, the preserved broader scheme-alias /
  base-like route, and the no-widening `keepTargetFinal` boundary.
- Reviewer must be able to confirm a fresh focused
  `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, a
  fresh `cabal build all && cabal test` gate, predecessor continuity checks,
  and docs-only diff scope.
- Any retry after review must revise this same `plan.md` as a delta for the
  recorded `fix_hypothesis`; do not replan the whole round if `retry` becomes
  active later.
