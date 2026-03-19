# `G3` Bounded Verification Gate For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-048`
Roadmap item: `G3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only verification/evidence consolidation

## Stage Contract Freeze

This artifact records only roadmap item `G3` for `attempt-1`.

`G3` re-verifies exactly the accepted `G2` local-binding
`rootLocalMultiInst` / `targetC -> rootFinal` lane under repaired `URI-R2-C1`.
It does not reopen `G1` selection, does not reopen `G2` implementation, does
not preempt `G4`, and does not authorize production or test edits during this
attempt.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback or widening.

Any verification failure in this stage would be a blocker to record, not
permission to patch `Fallback.hs`, `PipelineSpec.hs`, or any other production
or test file inside `G3`.

## Accepted `G1` / `G2` Chain Reconfirmed Without Widening

- `orchestrator/rounds/round-046/review-record.json` confirms `G1` finalized as
  authoritative on `attempt: 2` with `attempt_verdict: accepted`,
  `stage_action: finalize`, and canonical artifact
  `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`.
- `orchestrator/rounds/round-047/review-record.json` confirms `G2` finalized as
  authoritative on `attempt: 1` with `attempt_verdict: accepted`,
  `stage_action: finalize`, and canonical artifact
  `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`.
- The accepted `G2` exclusions remain binding in `G3`:
  `instArgRootMultiBase` remains unselected and out of scope;
  `boundVarTarget` widening remains out of scope;
  non-local widening remains out of scope;
  no replay reopen;
  no `MLF.Elab.Inst`;
  no `InstBot`;
  no equi-recursive reasoning;
  no cyclic structural encoding;
  no second executable interface; and
  no compatibility, convenience, or fallback-path widening.
- `/Volumes/src/mlf4/Bugs.md` remained continuity context only and was not
  reinterpreted as current repair authority.

`G3` therefore answers one bounded question only: whether the accepted `G2`
local `rootLocalMultiInst` / `targetC -> rootFinal` lane still looks stable
under read-only anchor checks, the focused `ARI-C1` rerun, the fresh full repo
gate, and predecessor continuity rechecks.

## Read-Only Anchor Evidence

### `Fallback.hs`

Read-only inspection of
`src/MLF/Elab/Run/ResultType/Fallback.hs` confirmed the accepted local lane is
still the live bounded implementation:

- lines `525-527` define the reviewer-auditable proof
  `rootLocalMultiInst = rootBindingIsLocalType && rootHasMultiInst`;
- lines `622-670` show `boundVarTarget` still exists as inherited context, but
  it is carried separately from the selected local multi-inst proof;
- lines `671-677` keep `keepTargetFinal` gated by `rootBindingIsLocalType` and
  preserve the unchanged out-of-scope trigger families
  `instArgRootMultiBase`, `rootLocalSchemeAliasBaseLike`, and `boundVarTarget`;
- lines `678-697` keep `targetC` selecting `rootFinal` only when
  `rootLocalSchemeAliasBaseLike || rootLocalMultiInst` holds, otherwise falling
  back to the inherited fail-closed branches.

### `PipelineSpec.hs`

Read-only inspection of the bounded
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`test/PipelineSpec.hs` confirmed the expected focused evidence remains present:

- lines `1365-1375` retain the same-lane retained-child baseline checks from
  `E2` / `E3`;
- lines `1387-1400` retain the accepted local scheme-alias/base-like success
  example and matched non-local fail-closed contrast from `F2` / `F3`;
- lines `1402-1420` retain the accepted local multi-inst local-`TypeRef`
  success example and matched non-local multi-inst fail-closed contrast from
  `G2`;
- lines `1439-1466` retain the source-guard assertions naming
  `rootLocalMultiInst`, the local-binding gate, and the unchanged
  `targetC -> rootFinal` selection branch;
- lines `1468-1478` retain the inherited non-local proxy fail-closed entrypoint
  checks outside the selected local lane.

These anchors show the verified lane is still exactly the accepted local
`rootLocalMultiInst` / `targetC -> rootFinal` lane, with
`instArgRootMultiBase`, `boundVarTarget` widening, and non-local widening still
outside scope.

## Verification Runs

Verification ran in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-048`.

### Baseline Checks

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass (`contract_version: 2`, `retry: null`)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass; items `1` through `16` remain parseable and item `15` is still the
  pending live step before review/merge
- required design/boundary file presence checks from `orchestrator/verification.md`
  -> pass

### Focused Bounded Test Rerun

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `13 examples, 0 failures`

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - `1134 examples, 0 failures`

### Continuity And Diff Checks

- read-only JSON inspection reconfirmed `G1` and `G2` authoritative acceptance
  with no retry pending
- `git diff --name-only` -> no tracked diff in the round worktree before
  authoring `G3` docs
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> no output
- `git status --short --untracked-files=all` before authoring `G3` docs showed
  only the existing controller packet files:
  - `?? orchestrator/rounds/round-048/plan.md`
  - `?? orchestrator/rounds/round-048/selection.md`

No blocker was found. `Fallback.hs` and `PipelineSpec.hs` served only as
read-only anchors during this round and received no `G3` edits.

## Result

The accepted `G2` local `rootLocalMultiInst` / `targetC -> rootFinal` lane
remains stable under read-only anchor inspection, a fresh focused `ARI-C1`
rerun, a fresh full repo gate, and predecessor continuity checks. `G3`
therefore stays bounded and ready for reviewer verification without widening
into `instArgRootMultiBase`, `boundVarTarget`, non-local lanes, replay reopen,
or any broader recursive-inference work.
