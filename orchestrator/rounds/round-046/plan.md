# Round 046 Retry Delta Plan (`G1` Attempt `2`)

## Delta Scope

Repair only the rejected `attempt-1` docs-only control-plane defect recorded as
`selection-doc-stale-facts`.

All repo-local paths in this plan are relative to the active packet rooted at:

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-046`

The canonical `G1` bind artifact remains:

`docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`

This retry does not redesign `G1`. It preserves the already-selected live
subject, the frozen `G2` slice, the owned future files, the exclusions, and the
docs-only verification/full-gate-skip semantics from `attempt-1`. The only
required repair is to refresh `orchestrator/rounds/round-046/selection.md` so
its factual repository-status and `Bugs.md` statements match the current
round-046 packet.

## Locked Retry Context

- Round: `round-046`
- Roadmap item: `G1`
- Stage: `plan`
- Active attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason: `selection-doc-stale-facts`
- Fix hypothesis: `Refresh orchestrator/rounds/round-046/selection.md so it matches the current repository state and Bugs.md facts, then rerun the same docs-only verification set.`
- Live subject remains repaired `URI-R2-C1`
- Boundary remains `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`

The rejected review authority that this retry must satisfy remains:

- `orchestrator/rounds/round-046/review.md`
- `orchestrator/rounds/round-046/reviews/attempt-1.md`
- `orchestrator/rounds/round-046/attempt-log.jsonl`

The canonical artifact and notes that must remain unchanged remain:

- `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
- `orchestrator/rounds/round-046/implementation-notes.md`

`attempt-1` already froze the canonical `G2` target correctly: the local-binding
`rootHasMultiInst` trigger family only, with `instArgRootMultiBase`
explicitly non-selected. This retry must not rename the artifact, change the
selected target family, widen into `instArgRootMultiBase`, or reopen replay,
`MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening, non-local widening,
equi-recursive reasoning, cyclic structural encoding, a second interface, or
fallback conveniences.

## File Map

### Modify

- `orchestrator/rounds/round-046/selection.md`
  - Responsibility: update stale factual claims so the round-selection record
    matches the current round-046 packet and `Bugs.md`.
  - Why this file: the reviewer found the only blocking defect here.

- `orchestrator/rounds/round-046/implementation-notes.md`
  - Responsibility: add a concise retry-attempt note that `attempt-2` only
    refreshes `selection.md` facts while preserving the accepted `attempt-1`
    `G1` bind artifact, selected `rootHasMultiInst` target, and docs-only
    verification contract.
  - Why this file: reviewer handoff should make the retry scope explicit.

### Evidence-Only, No Planned Edit

- `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
- `orchestrator/rounds/round-046/review.md`
- `orchestrator/rounds/round-046/reviews/attempt-1.md`
- `orchestrator/rounds/round-046/attempt-log.jsonl`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

These files already carry the canonical artifact path, frozen `G2` slice,
exclusions, retry history, and current bug status that this retry must preserve.
This retry must not edit them.

## Sequential Delta Tasks

### Task 1 - Reframe the retry around the actual rejected `attempt-1` issue

- Update this plan to name `attempt-2`, `selection-doc-stale-facts`, and the
  latest fix hypothesis from `attempt-log.jsonl`.
- State explicitly that the canonical `G1` bind artifact and the selected
  `rootHasMultiInst` target family are already correct and are not being changed
  in this retry.
- Keep this plan retry-only. Do not restore the initial broad bind-authoring
  plan.

### Task 2 - Refresh `selection.md` to current packet facts only

- Keep the round selection itself unchanged:
  - roadmap item `13` / `G1` still runs now;
  - the live subject remains repaired `URI-R2-C1`;
  - the frozen next `G2` family remains exactly one of
    `rootHasMultiInst` or `instArgRootMultiBase`, with the current packet
    already selecting `rootHasMultiInst`.
- Replace only the stale factual statements the reviewer identified:
  - do not say `BUG-2026-03-16-001` is open if `Bugs.md` records it as resolved;
  - do not say `orchestrator/state.json` is modified if current packet status is
    clean in the active round worktree;
  - do not say `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` is
    untracked if that folder is absent in the active round worktree.
- Keep the selection rationale bounded to the active round worktree packet, not
  unrelated parent-workspace status.

### Task 3 - Preserve the canonical `G1` bind and bounded target unchanged

- Keep `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md` as the only
  canonical `G1` artifact path.
- Do not rewrite its body, rename it, or change the selected
  `rootHasMultiInst` `G2` family.
- Do not widen toward `instArgRootMultiBase` or any other family.
- Do not edit `Fallback.hs`, `PipelineSpec.hs`, `Bugs.md`, `orchestrator/state.json`,
  or `orchestrator/roadmap.md`.

### Task 4 - Record the retry scope in round notes only

- Update `orchestrator/rounds/round-046/implementation-notes.md` to state that
  `attempt-2` repairs only stale `selection.md` facts.
- Keep the existing docs-only verification/full-gate-skip semantics intact:
  no `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal` edits are
  authorized by this retry.

### Task 5 - Re-run only the docs-only exactness checks needed to clear the retry reason

- Re-run baseline docs-only checks:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - required design/boundary file presence checks from `orchestrator/verification.md`
- Reconfirm the canonical artifact and bounded target remain unchanged:
  - `test -f docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
  - `rg -n 'rootHasMultiInst|instArgRootMultiBase|Fallback\\.hs:240-248|Fallback\\.hs:668-693' docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
- Reconfirm the stale-fact repair specifically:
  - `rg -n 'BUG-2026-03-16-001|Status: Resolved' /Volumes/src/mlf4/Bugs.md`
  - `test -e tasks/todo/2026-03-18-continue-bounded-orchestrator-run && echo exists || echo missing`
  - `git status --short orchestrator/state.json`
  - `rg -n 'open replay-path bug|state\\.json modified|tasks/todo/2026-03-18-continue-bounded-orchestrator-run' orchestrator/rounds/round-046/selection.md`
    - expected: no stale-claim matches after the repair
- Keep docs-only diff evidence:
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  - `git status --short --untracked-files=all -- src src-public app test mlf2.cabal`
- Do not rerun `cabal build all && cabal test` unless the retry escapes
  docs/orchestrator surfaces, which this plan forbids.

## Non-Goals

- No change to the selected `G2` slice, its frozen future ownership, or its
  semantic intent.
- No change to the accepted `E2` / `E3` / `F1` / `F2` / `F3` / `F4`
  carry-forward chain.
- No change to the binding effect of accepted `U2`, `U3`, or `U4`.
- No edits to `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`,
  `orchestrator/state.json`, `orchestrator/roadmap.md`, `/Volumes/src/mlf4/Bugs.md`,
  production code, tests, public API surfaces, executable entrypoints, or
  `mlf2.cabal`.
- No rewrite of review history, `reviews/attempt-1.md`, `review.md`, or
  `attempt-log.jsonl`.
- No alternate canonical filename choice and no reopening of `instArgRootMultiBase`.

## Reviewer Checks For This Retry

1. `plan.md` explicitly names `attempt-2` and frames the retry as a delta-only
   repair for `selection-doc-stale-facts`.
2. `selection.md` no longer claims `BUG-2026-03-16-001` is open, no longer says
   `orchestrator/state.json` is modified when the active packet is clean, and
   no longer cites the absent task folder as untracked packet evidence.
3. The canonical `G1` artifact path remains
   `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`, and the selected
   `rootHasMultiInst` target family remains unchanged.
4. The retry remains docs/orchestrator only, with no production/test/public
   API/Cabal/roadmap/controller-state/bug-tracker edits.
5. The explicit full-gate skip note remains justified because no
   `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal` path is touched.
