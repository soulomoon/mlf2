# Round 061 Plan (`J4` Next-Cycle Decision Gate)

## Objective

Execute only roadmap item `J4` and produce one accepted aggregate decision
artifact at
`docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`.

This round is the initial `J4` plan with `retry: null`, not a delta retry
plan. It is aggregate-only and docs-only. It must record exactly one lawful
bounded next-step result token over the already reverified `J3` evidence chain
for the accepted repaired `URI-R2-C1` local-binding inst-arg-only
singleton-base `rootLocalInstArgSingleBase` / `baseTarget -> baseC` /
same-lane `targetC` lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

No production, test, public API, executable, Cabal, roadmap, controller-state,
or bug-tracker edits are planned or authorized in `attempt-1`. This round must
not reopen accepted `I4`, `J1`, `J2`, or `J3`, must not widen silently, and
must not reinterpret accepted `U2` / `U3` / `U4` negatives as widening
clearance.

## Locked Round Context

- Round id: `round-061`
- Roadmap item: `J4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: aggregate-only, docs-only next-cycle decision gate

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `J4` and must not appear in the plan,
artifact, reviewer handoff, or final accepted review record.

Accepted predecessor facts that remain binding throughout `J4`:

- `orchestrator/rounds/round-057/review-record.json` finalized `I4` as
  authoritative `attempt: 2` with
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`;
  the accepted `I4` artifact records lawful result token
  `continue-bounded` and requires any successor work to begin with a fresh
  bounded exact-target bind rather than silent widening or reopening.
- `orchestrator/rounds/round-058/review-record.json` finalized `J1` as
  authoritative `attempt: 1` with
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`;
  the accepted `J1` artifact froze exactly one future `J2` slice: the
  adjacent local-binding inst-arg-only singleton-base `baseTarget -> baseC`
  lane plus its same-lane `targetC` use, with future ownership limited to
  `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`.
- `orchestrator/rounds/round-059/review-record.json` finalized `J2` as
  authoritative `attempt: 1` with
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`;
  the accepted `J2` artifact landed only that frozen slice, introducing the
  reviewer-auditable proof
  `rootLocalInstArgSingleBase = rootBindingIsLocalType && IntSet.null rootBaseBounds && IntSet.size instArgBaseBounds == 1 && not rootHasMultiInst && not instArgRootMultiBase`
  plus the dedicated same-lane `targetC` arm, while preserving the completed
  `rootLocalSingleBase` lane, the already-accepted scheme-alias/base-like
  `baseTarget` route, retained-target behavior, and non-local fail-closed
  behavior.
- `orchestrator/rounds/round-060/review-record.json` finalized `J3` as
  authoritative `attempt: 1` with
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
  and all-pass checks for `J3-CONTRACT`, `J3-ANCHORS`, `J3-VERIFICATION`,
  `J3-DIFF-BOUNDARY`, `J3-CONTINUITY`, and `J3-RETRY-SCHEMA`; the accepted
  `J3` artifact already reverified the exact `J2` lane via read-only
  `Fallback.hs` / `PipelineSpec.hs` anchors, a fresh focused rerun of
  `ARI-C1 feasibility characterization (bounded prototype-only)`, a fresh full
  repo `cabal build all && cabal test` gate, predecessor continuity, and
  docs-only diff discipline.
- Accepted `U2 = authority-narrowed`, `U3 = uniqueness-owner-stable-refuted`,
  and `U4 = constructor-acyclic-termination-refuted` remain binding negative
  findings, not widening clearance.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only unless it reveals
  a real blocker that deprives the accepted `J3` chain of current
  authoritative continuity. Its present `Open` section is empty, so resolved
  bug history does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, non-local widening, or any broader
  trigger-family widening in this round.

Current repository state is already non-pristine. Respect existing work and do
not revert or clean up unrelated changes while preparing the `J4` decision
artifact.

## Authoritative Inputs To Preserve

- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-061/state-snapshot.json`
- `orchestrator/rounds/round-061/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- `docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
- `orchestrator/rounds/round-057/review-record.json`
- `orchestrator/rounds/round-058/review-record.json`
- `orchestrator/rounds/round-059/review-record.json`
- `orchestrator/rounds/round-060/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
   - create the canonical `J4` aggregate decision record.

Read-only decision anchors:

1. `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
2. `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
3. `docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
4. `docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
5. `docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
6. `orchestrator/rounds/round-057/review-record.json`
7. `orchestrator/rounds/round-058/review-record.json`
8. `orchestrator/rounds/round-059/review-record.json`
9. `orchestrator/rounds/round-060/review-record.json`
10. `src/MLF/Elab/Run/ResultType/Fallback.hs`
11. `test/PipelineSpec.hs`
12. `/Volumes/src/mlf4/Bugs.md`

Files that must remain untouched by `J4` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-061/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/roadmap.md`
- `orchestrator/rounds/round-061/selection.md`
- `/Volumes/src/mlf4/Bugs.md`
- prior artifacts and review history under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-060/`
- any docs outside the canonical `J4` artifact path

## Sequential Tasks

### Task 1 - Freeze the `J4` `attempt-1` contract as an aggregate-only decision gate

- In the canonical `J4` artifact, state explicitly:
  - `Round: round-061`
  - `Roadmap item: J4`
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
- State that `J4` is aggregate-only and docs-only.
- State that `J4` records exactly one lawful bounded next-step result token
  only.
- State that `J4` does not reopen accepted `I4`, `J1`, `J2`, or `J3`, does
  not amend the roadmap, does not widen the live subject or inherited
  boundary, and does not treat accepted `U2` / `U3` / `U4` negatives as if
  they were already cleared.
- State that any contradiction discovered in the accepted `J3` evidence chain
  or current continuity packet is a blocker to record through
  `stop-blocked`, not permission to patch code, add tests, reopen replay, or
  update `/Volumes/src/mlf4/Bugs.md` during this attempt.
- State explicitly that reviewer outcomes for this stage are limited to
  `accepted + finalize` or `rejected + retry`, and that `accepted + retry` is
  forbidden for `J4`.

### Task 2 - Reconstruct the accepted `I4` / `J1` / `J2` / `J3` evidence chain without reopening prior stages

- Carry forward only accepted authoritative evidence from the current `J`
  chain:
  - the selected live subject remains repaired `URI-R2-C1`;
  - the exact bounded lane remains the accepted `J2` / `J3`
    local-binding inst-arg-only singleton-base
    `rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
    slice;
  - the frozen ownership anchors remain `Fallback.hs` and `PipelineSpec.hs`;
  - the completed `rootLocalSingleBase` lane remains preserved as inherited
    context only;
  - the already-accepted scheme-alias/base-like `baseTarget` route remains
    preserved outside the selected lane; and
  - replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
    `boundTarget`, `schemeBodyTarget`, `ResultType.View`, non-local widening,
    and every broader trigger family remain out of scope.
- Use the authoritative review records as the acceptance proofs that remain
  binding:
  - `round-057` / `I4` -> `attempt = 2`, `attempt_verdict = accepted`,
    `stage_action = finalize`, `status = authoritative`
  - `round-058` / `J1` -> `attempt = 1`, `attempt_verdict = accepted`,
    `stage_action = finalize`, `status = authoritative`
  - `round-059` / `J2` -> `attempt = 1`, `attempt_verdict = accepted`,
    `stage_action = finalize`, `status = authoritative`
  - `round-060` / `J3` -> `attempt = 1`, `attempt_verdict = accepted`,
    `stage_action = finalize`, `status = authoritative`
- Carry forward the accepted `J3` checks map as the current bounded decision
  baseline:
  `J3-CONTRACT`, `J3-ANCHORS`, `J3-VERIFICATION`, `J3-DIFF-BOUNDARY`,
  `J3-CONTINUITY`, and `J3-RETRY-SCHEMA` all remained `pass`.
- Treat `/Volumes/src/mlf4/Bugs.md` only as continuity context unless it shows
  a real blocker. Do not reinterpret resolved bug history as fresh repair or
  widening authority.
- State explicitly that `J4` answers one bounded question only:
  given the accepted `J3` evidence chain and current docs-level continuity,
  what single lawful next-step result token applies to the repaired
  `URI-R2-C1` local-binding inst-arg-only singleton-base
  `rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
  lane?

### Task 3 - Apply a closed-rule decision sequence so exactly one lawful token is recorded

Use the following order and stop at the first matching rule:

1. `stop-blocked`
   - choose this only if a required accepted artifact is missing, one of the
     required `round-057` through `round-060` review records is
     non-authoritative or contradictory, docs-level continuity checks fail, or
     the accepted `J3` verification can no longer be treated as the current
     bounded verification baseline for the same repaired `URI-R2-C1`
     `rootLocalInstArgSingleBase` lane, including the preserved
     `rootLocalSingleBase` lane, the preserved scheme-alias/base-like
     `baseTarget` route outside the selected lane, and the frozen
     `Fallback.hs` / `PipelineSpec.hs` ownership anchors.
   - if this rule matches, record the exact blocker and do not patch around
     it.

2. `widen-approved`
   - choose this only if already-accepted evidence in the current repository
     proves that widening is now lawful without rewriting accepted history.
   - at minimum that would require an accepted artifact or roadmap amendment
     that explicitly changes the live subject or inherited boundary and clears
     the still-binding `U2` / `U3` / `U4` negative findings as widening
     blockers for this slice.
   - absent such already-accepted evidence, `widen-approved` is not lawful.
   - even if it were selected, `J4` would still record the decision only; it
     would not widen the roadmap or authorize silent implementation changes in
     this round.

3. `continue-bounded`
   - choose this if the accepted `I4` / `J1` / `J2` / `J3` chain remains
     authoritative, the accepted `J3` verification still stands as the current
     bounded verification baseline, no new blocker is found, and no
     already-accepted widening authority exists.
   - this result must preserve repaired `URI-R2-C1`, the frozen
     `Fallback.hs` / `PipelineSpec.hs` ownership anchors, the completed
     `rootLocalSingleBase` lane, the already-accepted scheme-alias/base-like
     `baseTarget` route outside the selected lane, and the inherited
     explicit-only / non-equi-recursive / non-cyclic-graph /
     no-second-interface / no-fallback boundary.
   - if selected, the decision rationale must frame any successor work as a
     fresh bounded exact-target bind first, not as silent widening or as
     authorization to reopen accepted prior stages.

The canonical `J4` artifact must record exactly one of those three lawful
tokens and must explain why the selected token is lawful and why the other two
tokens are not.

### Task 4 - Run the docs-only verification and continuity checks required for `J4`

Run the baseline docs/state checks required by
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-061/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-061/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `test -f docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
- `test -f /Volumes/src/mlf4/Bugs.md`
- `python3 -m json.tool orchestrator/rounds/round-057/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-058/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-059/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-060/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `I4`, `J1`, `J2`, and `J3` each keep the expected `stage_id`;
  - each review record remains `accepted` + `finalize` + `authoritative`;
  - the canonical artifact paths match the accepted `I4` / `J1` / `J2` / `J3`
    artifact files; and
  - `J3` still carries the required all-pass checks map.
- a short `python3` or `rg` check proving `/Volumes/src/mlf4/Bugs.md`
  contributes continuity context only here: the `Open` section is empty and no
  open bug blocks the accepted `J3` chain.
- a short `python3` or `rg` check proving the canonical `J4` artifact records
  exactly one of the three lawful result tokens and that its rationale keeps
  the successor rule explicit: any future work must begin with a fresh bounded
  exact-target bind rather than silently widening or reopening accepted
  `I4` / `J1` / `J2` / `J3`.

Post-edit docs-only diff checks must confirm:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- src src-public app test mlf2.cabal`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun `cabal build all && cabal test` in `J4`. This round remains
aggregate-only and docs-only, and the accepted `J3` artifact already carries
the fresh focused rerun plus the fresh full repo gate for this exact lane.

## Completion Criteria

This plan is complete only if the implementation stage stays inside all of the
following limits:

1. The canonical `J4` artifact is created at the planned path and records
   `attempt-1` as an aggregate-only, docs-only decision gate.
2. The artifact grounds its decision strictly in the accepted `J3` evidence
   chain and the accepted `I4` / `J1` / `J2` continuity it carries forward,
   without reopening accepted `I4`, `J1`, `J2`, or `J3`.
3. The artifact records exactly one lawful bounded result token:
   `continue-bounded`, `widen-approved`, or `stop-blocked`.
4. The artifact preserves repaired `URI-R2-C1`, the frozen
   `Fallback.hs` / `PipelineSpec.hs` ownership anchors, the completed
   `rootLocalSingleBase` lane, the already-accepted scheme-alias/base-like
   `baseTarget` route outside the selected lane, and the inherited
   explicit-only / non-equi-recursive / non-cyclic-graph /
   no-second-interface / no-fallback boundary.
5. `/Volumes/src/mlf4/Bugs.md` is treated as continuity context only unless a
   real blocker is demonstrated, and accepted `U2` / `U3` / `U4` negatives are
   not reinterpreted as widening clearance.
6. No production/test/public-API/executable/Cabal/controller-state/roadmap/
   bug-tracker edit occurs, and no implementation, review approval, merge
   action, or roadmap update is simulated inside this round.

## Non-Authorization

This plan does not authorize:

- any change to `orchestrator/rounds/round-061/state-snapshot.json`;
- any rewrite of `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-028/roadmap.md`,
  `orchestrator/rounds/round-061/selection.md`, `review.md`,
  `reviews/attempt-*.md`, `attempt-log.jsonl`, `merge.md`, or
  `review-record.json`;
- any production/test/public-API/executable/Cabal edit;
- any change to `/Volumes/src/mlf4/Bugs.md`;
- any reopening of accepted `I4`, `J1`, `J2`, or `J3`;
- any reopening of the completed `rootLocalSingleBase` lane or the
  already-accepted scheme-alias/base-like `baseTarget` route as new live
  targets;
- any replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`, or `ResultType.View` work;
- any non-local widening, cross-family widening, equi-recursive reasoning,
  implicit unfolding, cyclic structural graph encoding, second executable
  interface, compatibility shim, convenience fallback, or default-path
  widening; or
- any silent successor selection after `J4`; any future work must start with a
  fresh accepted bounded exact-target bind.
