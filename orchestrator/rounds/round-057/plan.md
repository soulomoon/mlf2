# Round 057 Plan (`I4` Retry Delta Attempt-2)

## Objective

Execute only the same-round retry for roadmap item `I4`.

This rewrite replaces the rejected `attempt-1` plan with the required
`attempt-2` retry delta only. The retry scope is exactly the recorded fix
hypothesis from `orchestrator/rounds/round-057/state-snapshot.json`: repair the `round-057` packet so
this plan and the same canonical `I4` artifact use the live bug tracker plus
accepted `I3` / `H4` continuity correctly, treating `BUG-2026-03-16-001` as
resolved continuity context only rather than as an open-bug premise. The round
must stay aggregate-only, docs-only, and bounded to repaired `URI-R2-C1`, then
rerun the closed-rule `I4` decision over that corrected authoritative
continuity packet.

The canonical `I4` artifact path remains:

`docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`

All non-retried `attempt-1` contract facts remain binding: the live subject
stays repaired `URI-R2-C1`, the inherited explicit-only / non-equi-recursive /
non-cyclic-graph / no-second-interface / no-fallback boundary stays fixed, the
round stays aggregate-only and docs-only, and
`src/MLF/Elab/Run/ResultType/Fallback.hs` plus `test/PipelineSpec.hs` remain
read-only.

## Locked Retry Context

- Round id: `round-057`
- Roadmap item: `I4`
- Stage: `plan`
- Active attempt: `attempt-2`
- Retry state: active
- Retry reason: `i4-bug-continuity-authority-mismatch`
- Fix hypothesis:
  `Repair the round-057 packet so the selection, plan, and canonical I4 artifact all use the live bug tracker plus accepted I3/H4 continuity correctly: BUG-2026-03-16-001 is resolved continuity context only, not an open-bug premise. Preserve the out-of-scope ruling, keep the round aggregate-only/docs-only and bounded to repaired URI-R2-C1, then rerun the closed-rule I4 decision over that corrected authoritative continuity packet.`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Prior accepted scope that remains binding:
  the accepted `I1` / `I2` / `I3` local single-base
  `rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane
  only

Carry forward without replanning:

- `selection.md` already fixes this round to `I4` only and must remain
  unedited. Its stale sentence claiming `BUG-2026-03-16-001` is open is
  non-authoritative guider context drift only, not live bug-status authority.
- The accepted `H4` review already established the governing rule for this
  situation: stale guider bug text is not a blocker when the canonical
  plan/artifact track the live resolved bug state correctly.
- `review.md`, `reviews/attempt-1.md`, and `attempt-log.jsonl` already record
  the rejected `attempt-1` blocker and must remain immutable.
- The same canonical `I4` artifact must be refreshed in place for `attempt-2`;
  this retry does not mint a replacement artifact, does not reopen `I1` / `I2`
  / `I3`, and does not broaden the selected lane.

## File Map

### Modify

- `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  - Responsibility: refresh the canonical `I4` decision artifact with
    `attempt-2` metadata and the corrected bug-authority continuity, then
    rerun the closed-rule `I4` decision over the accepted `I1` / `I2` / `I3`
    evidence chain without inheriting the stale open-bug premise from
    `selection.md`.

### Read-Only Evidence

- `orchestrator/rounds/round-057/state-snapshot.json`
- `orchestrator/rounds/round-057/selection.md`
- `orchestrator/rounds/round-057/review.md`
- `orchestrator/rounds/round-057/reviews/attempt-1.md`
- `orchestrator/rounds/round-057/attempt-log.jsonl`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/retry-subloop.md`
- `docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
- `orchestrator/rounds/round-053/reviews/attempt-1.md`
- `orchestrator/rounds/round-054/review-record.json`
- `orchestrator/rounds/round-055/review-record.json`
- `orchestrator/rounds/round-056/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

### Preserve Unchanged

- `orchestrator/rounds/round-057/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/roadmap.md`
- `orchestrator/rounds/round-057/selection.md`
- `orchestrator/rounds/round-057/review.md`
- `orchestrator/rounds/round-057/reviews/attempt-1.md`
- `orchestrator/rounds/round-057/attempt-log.jsonl`
- `orchestrator/rounds/round-057/implementation-notes.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `/Volumes/src/mlf4/Bugs.md`

## Sequential Retry Tasks

### Task 1 - Reframe `I4` around the actual rejected authority mismatch

- Refresh the canonical `I4` artifact metadata from `attempt-1` / `retry: null`
  to `attempt-2` / active retry state.
- State explicitly that this retry is confined to correcting bug-status
  authority handling inside the `round-057` packet.
- Preserve the round as aggregate-only, docs-only, and bounded to repaired
  `URI-R2-C1`.
- Preserve the read-only ownership anchor in `Fallback.hs` and
  `PipelineSpec.hs`; this retry does not authorize code/test edits.
- State explicitly that `selection.md` is not being repaired in place. Its
  stale open-bug sentence is guider-context drift only and must not be reused
  as live authority in the canonical `I4` artifact.

### Task 2 - Normalize bug-status continuity from the live authoritative sources

- Use `/Volumes/src/mlf4/Bugs.md` as the current canonical bug-status source:
  the `Open` section is empty and `BUG-2026-03-16-001` is resolved.
- Carry forward the accepted continuity already recorded in:
  - `docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
  - `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
  - `orchestrator/rounds/round-053/reviews/attempt-1.md`
- In the refreshed canonical `I4` artifact, state that
  `BUG-2026-03-16-001` is resolved continuity context only and remains
  out-of-scope for `I4`. It still does not authorize replay reopen,
  `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`, `boundTarget` overlay
  materialization, `View.hs`, `schemeBodyTarget`, non-local widening, or any
  broader trigger family.
- Do not propagate the stale `selection.md` open-bug sentence into the
  continuity narrative, blocker logic, or verification notes of the canonical
  `I4` artifact.

### Task 3 - Rerun the closed-rule `I4` decision over the corrected continuity packet

- Re-evaluate the same three legal result tokens in order:
  - `stop-blocked`
  - `widen-approved`
  - `continue-bounded`
- `stop-blocked` is lawful only if a genuine blocker remains after using the
  live bug tracker plus accepted `I3` / `H4` continuity correctly. The stale
  guider sentence in `selection.md` alone is not such a blocker.
- `widen-approved` remains unlawful unless already-accepted authority changes
  the live subject or inherited boundary and clears the still-binding
  `U2` / `U3` / `U4` negatives as widening blockers. No such authority is
  introduced by this retry.
- If the accepted `I1` / `I2` / `I3` chain remains authoritative, the accepted
  `I3` verification still stands as the current bounded verification baseline,
  and bug continuity is correctly treated as resolved-only out-of-scope
  context, the lawful `attempt-2` result token is `continue-bounded`.
- The canonical `I4` artifact must record exactly one result token and explain
  why that token is lawful and why the other two are not.

### Task 4 - Refresh the same canonical `I4` artifact in place

- Keep the canonical artifact path unchanged:
  `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- Preserve the accepted `I1` / `I2` / `I3` evidence chain, the repaired
  `URI-R2-C1` subject, the inherited boundary, the accepted `Fallback.hs` /
  `PipelineSpec.hs` ownership anchor, and the already-accepted
  scheme-alias/base-like `baseTarget` route outside the selected lane.
- Replace the rejected `attempt-1` blocker rationale that derived
  `stop-blocked` from the packet-local open-bug contradiction.
- Rewrite the continuity and decision sections so they match the live bug
  tracker, accepted `I3`, accepted `H4`, and the accepted `H4` review rule on
  guider-context drift.
- Keep the full-gate skip note truthful: `I4` remains docs-only, so it still
  does not rerun `cabal build all && cabal test`; the accepted `I3` artifact
  remains the current bounded verification baseline for this exact lane.
- Do not create `implementation-notes.md`, do not rewrite `selection.md`, and
  do not create a new canonical filename.

### Task 5 - Run the docs-only verification needed to clear the retry reason

Run the baseline docs/state checks required by `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-057/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-057/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `test -f docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
- `test -f orchestrator/rounds/round-053/reviews/attempt-1.md`
- `test -f /Volumes/src/mlf4/Bugs.md`
- `python3 -m json.tool orchestrator/rounds/round-054/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-055/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-056/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `I1` / `I2` / `I3` each have the expected `stage_id`;
  - each review record remains `accepted` + `finalize` + `authoritative`;
  - the canonical artifact paths match the accepted `I1` / `I2` / `I3`
    artifact files; and
  - `I3` still carries the required all-pass checks map.
- `rg -n 'BUG-2026-03-16-001|resolved|continuity context only' /Volumes/src/mlf4/Bugs.md docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md orchestrator/rounds/round-053/reviews/attempt-1.md docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `rg -n 'BUG-2026-03-16-001|open' orchestrator/rounds/round-057/selection.md`
  - expected: the stale guider sentence may still exist there, but the
    canonical `I4` artifact must not treat it as blocking bug-status
    authority.
- `python3` or `rg` check confirming exactly one `Recorded result token:` line
  in the canonical `I4` artifact and confirming that its rationale no longer
  derives the result from the stale packet-local open-bug contradiction.

Post-edit docs-only diff checks must confirm:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- src src-public app test mlf2.cabal`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun `cabal build all && cabal test` in `I4`. This round remains
aggregate-only and docs-only, and the accepted `I3` artifact already carries
the fresh focused rerun plus the fresh full repo gate for this exact lane.

## Completion Criteria

This retry plan is complete only if the implementation stage stays inside all
of the following limits:

1. The same canonical `I4` artifact is refreshed in place with `attempt-2`
   metadata and the corrected resolved-only bug continuity.
2. The plan/artifact pair use `/Volumes/src/mlf4/Bugs.md` plus accepted `I3`
   / `H4` continuity as authority, while treating the stale open-bug sentence
   in `selection.md` as non-authoritative guider context drift only.
3. The refreshed closed-rule decision no longer derives `stop-blocked` from
   the stale packet-local open-bug mismatch.
4. The round stays aggregate-only, docs-only, and bounded to repaired
   `URI-R2-C1`, with `Fallback.hs` and `PipelineSpec.hs` left read-only.
5. No production/test/public-API/executable/Cabal/controller-state/roadmap
   / bug-tracker edit occurs, and no implementation/review/merge-stage result
   is simulated inside this plan.

## Non-Authorization

This retry delta does not authorize:

- any change to `orchestrator/rounds/round-057/state-snapshot.json`;
- any rewrite of `selection.md`, `review.md`, `reviews/attempt-1.md`,
  `attempt-log.jsonl`, `implementation-notes.md`, `merge.md`,
  `review-record.json`, or `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-024/roadmap.md`;
- any production/test/public-API/executable/Cabal edit;
- any change to `/Volumes/src/mlf4/Bugs.md`;
- any reopening of `I1` target selection, `I2` implementation, or `I3`
  verification;
- any reopening of replay work, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget` widening, `boundTarget` overlay materialization,
  `View.hs`, `schemeBodyTarget`, non-local widening, equi-recursive
  reasoning, or cyclic structural graph encoding; or
- any simulation of implement/review/merge-stage results inside this plan.
