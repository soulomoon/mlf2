# `G2` Bounded Implementation Slice For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-047`
Roadmap item: `G2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded production/test implementation

## Stage Contract

This artifact implements exactly one bounded `G2` fail-closed lane inside
repaired `URI-R2-C1`:

- the local-binding `rootHasMultiInst` `keepTargetFinal` / `targetC` path in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-047/src/MLF/Elab/Run/ResultType/Fallback.hs`; and
- one bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-047/test/PipelineSpec.hs`.

The inherited boundary remains fixed:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This round does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
`instArgRootMultiBase`, `boundVarTarget` widening, non-local binding widening,
equi-recursive reasoning, cyclic structural encoding, multi-SCC widening,
cross-family widening, or broader search.

## Accepted Carry-Forward Chain

The accepted predecessor chain remains unchanged and is treated as binding input:

1. `E2` / `E3` remain inherited same-lane retained-child baseline evidence only.
2. `F2` authoritative `attempt-1` introduced only the bounded local
   `rootLocalSchemeAliasBaseLike` / `targetC -> rootFinal` slice in
   `Fallback.hs` plus focused `PipelineSpec` coverage.
3. `F3` authoritative `attempt-1` reverified that exact `F2` slice under the
   focused `ARI-C1` rerun and a passing full repo gate.
4. `F4` authoritative `attempt-1` finalized `continue-bounded`, so one more
   bounded non-widening slice was the only lawful successor action.
5. `G1` authoritative `attempt-2` froze exactly one future target: the
   local-binding `rootHasMultiInst` `keepTargetFinal` / `targetC` lane in
   `Fallback.hs`, with ownership limited to `Fallback.hs` and `PipelineSpec.hs`,
   while leaving `instArgRootMultiBase` explicitly unselected.
6. Accepted negative findings remain binding:
   `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`,
   `U4 = constructor-acyclic-termination-refuted`.

## Implemented Code Slice

The production edit stays inside the existing local target-selection block in
`Fallback.hs`.

Implemented change:

- introduced `rootLocalMultiInst` as the explicit reviewer-auditable proof
  `rootBindingIsLocalType && rootHasMultiInst`;
- routed `keepTargetFinal` through that one local multi-inst proof instead of
  carrying the selected slice on the raw `rootHasMultiInst` flag; and
- made `targetC` select `rootFinal` explicitly for the local multi-inst lane
  before falling back to the inherited `TyVar` / `boundVarTarget` /
  `schemeBodyTarget` branches.

Scope discipline preserved:

- `instArgRootMultiBase` remains unchanged and out of scope.
- The selected slice does not reopen replay repair, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget` widening, or non-local widening.
- The accepted `F2` local scheme-alias/base-like lane remains unchanged and is
  carried forward only as inherited context.

## Bounded `PipelineSpec` Coverage

The existing `ARI-C1 feasibility characterization (bounded prototype-only)` block
was extended with exactly one new positive behavioral example and one matched
fail-closed contrast:

- Positive example:
  `keeps local multi-inst fallback on the local TypeRef lane`
  This duplicates one in-scope instantiation trace so the selected lane is
  carried only by local `rootHasMultiInst`, while clearing the retained-child
  authority path. The expected result is retention of the final target variable
  on the local `TypeRef` lane.
- Matched fail-closed contrast:
  `keeps the same multi-inst wrapper fail-closed once it leaves the local TypeRef lane`
  This uses the same duplicated-trace wrapper but leaves the root on the
  inherited non-local lane. The expected result stays on the quantified
  fail-closed shell.

The source-guard example
`uses the local-binding gate when deciding retained fallback targets`
was refreshed to anchor the new `rootLocalMultiInst` proof and the
`targetC -> rootFinal` branch.

## Verification

Verification ran in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-047`.

### TDD Red -> Green

- Red:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  failed before the production fix. The new local multi-inst example did not yet
  observe the selected lane, and the source guard still lacked the explicit
  `rootLocalMultiInst` proof / `targetC -> rootFinal` branch.
- Green:
  rerunning the same focused command passed with
  `13 examples, 0 failures`.

### Baseline Controller Checks

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### Focused Bounded Checks

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `13 examples, 0 failures`
- `git diff --name-only` -> bounded to the four owned files only

### Full Repo Gate

- `cabal build all && cabal test` -> pass

### Continuity Statement

Completed predecessor rounds, accepted boundary docs, accepted `E2` / `E3` /
`F2` / `F3` / `F4` / `G1` history, `Bugs.md`, roadmap history, controller
state, `src-public/`, `app/`, and `mlf2.cabal` were left untouched.
