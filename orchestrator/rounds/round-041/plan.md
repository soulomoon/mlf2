# Round 041 Plan (`E4` Next-Cycle Decision Gate)

## Objective

Execute only roadmap item `E4` and produce one accepted aggregate decision artifact
at
`docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`.

This round is aggregate-only and docs-only. It must consolidate the accepted
`C4` / `E1` / `E2` / `E3` evidence chain into exactly one bounded next-step result
for the already-reverified repaired `URI-R2-C1` same-lane retained-child local-`TypeRef`
lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

No production, test, public API, executable, Cabal, roadmap, controller-state, or
bug-tracker edits are planned or authorized in `attempt-1`. This round does not
reopen `E1` selection, `E2` implementation, or `E3` verification.

## Locked Round Context

- Round id: `round-041`
- Roadmap item: `E4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary: `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: aggregate-only, docs-only next-cycle decision gate

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `E4` and must not appear in the plan, artifact,
reviewer handoff, or final accepted review record.

Accepted predecessor facts that remain binding throughout `E4`:

- `C4` finalized in `round-037` as authoritative `attempt-1` and recorded the
  controlling predecessor token `continue-bounded`, not `widen-approved` and not
  `stop-blocked`.
- `E1` finalized in `round-038` as authoritative `attempt-1` and froze exactly one
  bounded next target: the same-lane retained-child `boundVarTarget` /
  nested-`forall` fail-closed lane in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, with future ownership limited to
  `Fallback.hs` and `test/PipelineSpec.hs`.
- `E2` finalized in `round-039` as authoritative `attempt-2` and landed only that
  bounded same-lane retained-child slice, keeping `rootBindingIsLocalType` as the
  mandatory gate, keeping nested-`forall` / nested-owner crossings fail-closed via
  `boundHasForallFrom`, and adding the focused same-lane success plus matched
  fail-closed contrast in the bounded `ARI-C1 feasibility characterization
  (bounded prototype-only)` block.
- `E3` finalized in `round-040` as authoritative `attempt-1` and reverified that
  exact slice under the focused bounded `ARI-C1` rerun, a fresh full repo
  `cabal build all && cabal test` gate, predecessor continuity checks, and
  docs-only diff review.
- `U2` remains `authority-narrowed`.
- `U3` remains `uniqueness-owner-stable-refuted`.
- `U4` remains `constructor-acyclic-termination-refuted`.
- `BUG-2026-03-16-001` remains replay-lane continuity context only and does not
  authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, or broader
  recursive-inference widening in this round.

Current repository state is already non-pristine
(`orchestrator/rounds/round-041/state-snapshot.json` modified and
`tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked). Respect
existing work and do not revert or clean up unrelated changes while preparing the
`E4` decision artifact.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-041/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/retry-subloop.md`
- `orchestrator/roles/planner.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/roadmap.md`
- `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
- `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
- `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`
- `orchestrator/rounds/round-037/review-record.json`
- `orchestrator/rounds/round-038/review-record.json`
- `orchestrator/rounds/round-039/review-record.json`
- `orchestrator/rounds/round-040/review-record.json`
- `Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
   - create the canonical `E4` aggregate decision record.

Optional bounded note file:

1. `orchestrator/rounds/round-041/implementation-notes.md`
   - optional command transcript / continuity note file if long reviewer-facing
     evidence needs a bounded home.

Read-only decision anchors:

1. `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
2. `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
3. `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
4. `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`
5. `orchestrator/rounds/round-037/review-record.json`
6. `orchestrator/rounds/round-038/review-record.json`
7. `orchestrator/rounds/round-039/review-record.json`
8. `orchestrator/rounds/round-040/review-record.json`
9. `Bugs.md`

Files that must remain untouched by `E4` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-041/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/roadmap.md`
- `Bugs.md`
- reviewer-owned history under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-040/`

## Sequential Tasks

### Task 1 - Freeze the `E4` `attempt-1` contract as an aggregate-only decision gate

- In the canonical `E4` artifact, state explicitly:
  - `Round: round-041`
  - `Roadmap item: E4`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding.
- State that `E4` is aggregate-only and docs-only.
- State that `E4` records exactly one bounded next-step result token only.
- State that `E4` does not perform new implementation, does not rerun `E1` / `E2` /
  `E3`, and does not itself widen the live subject or boundary.
- State that any contradiction discovered in the accepted evidence chain is a blocker
  to record through `stop-blocked`, not a license to patch code, add tests, amend
  the roadmap, or update `Bugs.md` during this attempt.
- State explicitly that reviewer outcomes for this stage are limited to
  `accepted + finalize` or `rejected + retry`, and that `accepted + retry` is
  forbidden for `E4`.

### Task 2 - Reconstruct the accepted decision evidence chain without widening it

- Carry forward only accepted authoritative evidence:
  - `C4` supplies the predecessor decision token `continue-bounded` and the closed
    rule that the cycle remains non-widening absent already-accepted widening
    authority.
  - `E1` froze exactly one future bounded target: the same-lane retained-child
    local-`TypeRef` lane in `Fallback.hs`, with future ownership limited to
    `Fallback.hs` and `PipelineSpec.hs`.
  - `E2` authoritative `attempt-2` landed only that bounded same-lane retained-child
    slice and kept all excluded trigger families and widening lanes unchanged.
  - `E3` authoritative `attempt-1` reverified that same slice under the focused
    bounded `ARI-C1` rerun, a fresh full repo gate, predecessor continuity checks,
    and docs-only diff review, so `E3` is the current bounded verification baseline
    for this lane.
- Use
  `orchestrator/rounds/round-037/review-record.json`
  as the authoritative proof that `C4` finalized as `attempt=1`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`,
  with artifact path
  `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`.
- Use
  `orchestrator/rounds/round-038/review-record.json`
  as the authoritative proof that `E1` finalized as `attempt=1`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`,
  with artifact path
  `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`.
- Use
  `orchestrator/rounds/round-039/review-record.json`
  as the authoritative proof that `E2` finalized as `attempt=2`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`,
  with `authoritative_attempt=2`, artifact path
  `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`, and the
  bounded checks `E2-SAME-LANE-EVIDENCE=pass`, `E2-NEGATIVE-CONTRAST=pass`,
  `E2-FOCUSED-BLOCK=pass`, `E2-CONTINUITY=pass`, and `E2-FULL-GATE=pass`.
- Use
  `orchestrator/rounds/round-040/review-record.json`
  as the authoritative proof that `E3` finalized as `attempt=1`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`,
  with artifact path
  `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`, and the
  bounded checks `E3-E2-ACCEPTANCE=pass`, `E3-FOCUSED-BLOCK=pass`,
  `E3-FULL-GATE=pass`, `E3-CONTINUITY=pass`, and `E3-DOCS-ONLY=pass`.
- Treat `Bugs.md` only as continuity context.
  Do not reinterpret `BUG-2026-03-16-001` as current-round repair authority.
- State explicitly that `E4` answers one bounded question only:
  given the accepted `C4` / `E1` / `E2` / `E3` chain and current docs-level
  continuity, what single lawful next-step result token applies to the repaired
  `URI-R2-C1` same-lane retained-child lane?

### Task 3 - Apply a closed-rule decision sequence so exactly one token is recorded

Use the following order and stop at the first matching rule:

1. `stop-blocked`
   - choose this only if a required accepted artifact is missing, a required review
     record is non-authoritative or contradictory, docs-level continuity checks fail,
     or the accepted `E3` verification can no longer be treated as the current
     bounded verification baseline for the same-lane retained-child lane.
   - if this rule matches, record the exact blocker and do not patch around it.

2. `widen-approved`
   - choose this only if already-accepted evidence in the current repository proves
     that widening is now lawful without rewriting accepted history.
   - at minimum that would require an accepted artifact or roadmap amendment that
     explicitly changes the live subject or boundary and clears the still-binding
     `U2` / `U3` / `U4` negative findings as widening blockers for this lane.
   - absent such already-accepted evidence, `widen-approved` is not lawful.

3. `continue-bounded`
   - choose this if the accepted `C4` / `E1` / `E2` / `E3` chain remains
     authoritative, the accepted `E3` verification still stands as the current
     bounded verification baseline, no new blocker is found, and no already-accepted
     widening authority exists.
   - this result must preserve repaired `URI-R2-C1`, the bounded
     `Fallback.hs` / `PipelineSpec.hs` ownership boundary, and the inherited
     explicit-only / non-equi-recursive / non-cyclic-graph limits.

The canonical `E4` artifact must record exactly one of those three tokens and must
explain why the selected token is lawful and why the other two tokens are not.

### Task 4 - Run the docs-only verification and continuity checks required for `E4`

Run the baseline docs/state checks required by
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-041/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-041/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-008/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `python3 -m json.tool orchestrator/rounds/round-037/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-038/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-039/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-040/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `round-037` is `stage_id == "C4"`, `attempt_verdict == "accepted"`,
    `stage_action == "finalize"`, `status == "authoritative"`,
    `authoritative_attempt == 1`, and its artifact path matches the accepted `C4`
    canonical doc;
  - `round-038` is `stage_id == "E1"`, `attempt_verdict == "accepted"`,
    `stage_action == "finalize"`, `status == "authoritative"`,
    `authoritative_attempt == 1`, and its artifact path matches the accepted `E1`
    canonical doc;
  - `round-039` is `stage_id == "E2"`, `attempt == 2`,
    `attempt_verdict == "accepted"`, `stage_action == "finalize"`,
    `status == "authoritative"`, `authoritative_attempt == 2`, and its artifact
    path plus key bounded checks match the accepted `E2` record;
  - `round-040` is `stage_id == "E3"`, `attempt == 1`,
    `attempt_verdict == "accepted"`, `stage_action == "finalize"`,
    `status == "authoritative"`, `authoritative_attempt == 1`, and its artifact
    path plus key bounded checks match the accepted `E3` record.

Reconfirm the round stays docs-only:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun `cabal build all && cabal test` in `E4` `attempt-1` unless an
unexpected non-doc/code-path edit appears first. The required current bounded
verification has already been supplied by accepted `E3`, and `E4` is only
aggregating that accepted verification into one decision token.

### Task 5 - Author the canonical `E4` aggregate decision artifact

Write
`docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
as the canonical stage result.

Required sections:

- stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
  `Retry state`, `Live subject`, `Artifact kind`);
- stage contract freeze;
- accepted evidence chain carried forward from `C4`, `E1`, `E2`, and `E3`, plus the
  review-record continuity evidence and the continuity-only `Bugs.md` note;
- docs-only verification and continuity check results;
- `Recorded Result Token`, with exactly one token line and exactly one token value;
- `Why <selected-token> Is Lawful`;
- `Why The Other Two Tokens Are Not Lawful`;
- blockers;
- non-authorization statement.

The artifact must preserve the same-lane retained-child slice as the only live
decision subject and must not drift into replay repair, `MLF.Elab.Inst`, broader
recursive-inference widening, or any new implementation/testing work.

### Task 6 - Prepare the bounded reviewer handoff

Reviewer-specific checks to call out explicitly:

- `E4-CONTRACT`
  - confirm the artifact records `round-041`, `E4`, `attempt-1`, `retry: null`,
    aggregate-only/docs-only scope, and the explicit prohibition on
    `accepted + retry`.
- `E4-CONTINUITY`
  - confirm `round-037` through `round-040` review records remain authoritative,
    parseable, and aligned with the accepted canonical artifact paths.
- `E4-RESULT-TOKEN`
  - confirm the canonical artifact records exactly one token and that the token is
    one of `continue-bounded`, `widen-approved`, or `stop-blocked`.
- `E4-EVIDENCE-CHAIN`
  - confirm the artifact relies only on accepted `C4` / `E1` / `E2` / `E3`
    evidence plus continuity-only `Bugs.md` context, and explains why the
    unselected tokens are not lawful.
- `E4-BOUNDARY`
  - confirm the live subject remains repaired `URI-R2-C1`, the bounded
    `Fallback.hs` / `PipelineSpec.hs` ownership boundary stays fixed, and the
    inherited explicit-only / non-equi-recursive / non-cyclic-graph limits remain
    unchanged.
- `E4-DOCS-ONLY`
  - confirm no non-doc/code-path edits were introduced.
- `E4-SKIP-NOTE`
  - confirm any skipped full-gate rerun is justified explicitly by docs-only scope
    and by the accepted `E3` verification baseline.
- `E4-SCOPE-ALIGNMENT`
  - confirm `selection.md`, `plan.md`, and the canonical `E4` artifact all agree on
    the same aggregate-only `E4` contract and the same single recorded token.

Review summary requirements remain:

- record `Implemented stage result`, `Attempt verdict`, `Stage action`,
  `Retry reason`, and `Fix hypothesis`;
- if the attempt is accepted, it must finalize with `Retry reason: none` and
  `Fix hypothesis: none`;
- if the attempt is rejected, retry in the same round only for the recorded
  `fix_hypothesis`, without rewriting prior attempt history.
