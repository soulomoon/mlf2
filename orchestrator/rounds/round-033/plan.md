# Round 033 Retry Delta Plan (`U6` Attempt 2)

## Delta Target

Correct only the `Files Changed By This Round` section in `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` so it matches the exact current round-033 authored artifact set before resubmission.

## Locked Retry Context

- Round: `round-033`
- Roadmap item: `U6`
- Active attempt: `attempt-2`
- Retry reason: `u6-artifact-files-changed-list-not-exact`
- Fix hypothesis: update the `U6` artifact's file-list section so it matches the actual round-033 authored file set, including the existing round-owned `plan.md` and `selection.md`, then resubmit without widening scope or editing controller-owned state
- Live subject remains repaired `URI-R2-C1`
- Scope remains aggregate-only
- Boundary remains `explicit-only / non-equi-recursive / non-cyclic-graph`
- Preserve the current bounded result token `continue-bounded` unless the file-set check exposes contradictory new scope evidence

## Sequential Steps

### Step 1 - Recompute the exact authored round file set

Run:

```bash
git status --short --untracked-files=all -- \
  docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md \
  orchestrator/rounds/round-033
```

Use that snapshot to identify the exact current round-authored artifact set. The corrected `U6` artifact must now include at least:

- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `orchestrator/rounds/round-033/selection.md`
- `orchestrator/rounds/round-033/plan.md`
- `orchestrator/rounds/round-033/implementation-notes.md`

If the same status snapshot also shows already-existing reviewer-authored round artifacts (for example `orchestrator/rounds/round-033/review.md` or `orchestrator/rounds/round-033/reviews/attempt-1.md`), include them as carried round-authored artifacts in the file list as well.

Do not treat `orchestrator/rounds/round-033/attempt-log.jsonl` as an authored artifact for the `U6` document; it is controller-owned state and must remain untouched.

### Step 2 - Repair only the `U6` artifact file-list section

Edit only `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` and update only its `Files Changed By This Round` section so the bullets exactly match the authored set from Step 1.

Keep everything else unchanged except for the minimum wording needed to distinguish authored round artifacts from untouched controller-owned state. Do not reopen the evidence chain, rerun the decision rule, widen scope, or change the result token unless Step 1 reveals unexpected drift outside the allowed round surfaces.

### Step 3 - Re-run only the exactness verification needed for review

Re-run:

```bash
git diff --check
git status --short --untracked-files=all -- \
  docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md \
  orchestrator/rounds/round-033
```

Then run one targeted check (`rg` or a short `python3` script) that proves the `Files Changed By This Round` bullets in `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` exactly match the authored set from Step 1, with controller-owned `attempt-log.jsonl` excluded.

Preserve the already-recorded attempt-1 continuity and full-gate evidence. Do not rerun `cabal build all && cabal test` unless the edit escapes docs/orchestrator artifact surfaces, which this retry plan forbids.

### Step 4 - Resubmit with unchanged aggregate-only semantics

Resubmit the corrected `U6` artifact for review with:

- the same live subject: repaired `URI-R2-C1`
- the same inherited boundary: `explicit-only / non-equi-recursive / non-cyclic-graph`
- the same aggregate-only `U6` scope
- the same bounded result token `continue-bounded`, unless Step 1 uncovered contradictory out-of-scope drift

## Non-Goals

- No edits to `orchestrator/state.json`, `orchestrator/roadmap.md`, `Bugs.md`, production code, tests, or `mlf2.cabal`
- No edits to prior attempt artifacts or reviewer history
- No new implementation slice, roadmap update, or widening decision beyond the file-list correction
