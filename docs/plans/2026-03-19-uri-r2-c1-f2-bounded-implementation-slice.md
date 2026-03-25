# `F2` Bounded Implementation Slice For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-043`
Roadmap item: `F2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded production/test implementation

## Stage Contract

This artifact implements exactly one bounded `F2` fail-closed lane inside repaired
`URI-R2-C1`:

- the adjacent local-binding `rootIsSchemeAlias && rootBoundIsBaseLike`
  `keepTargetFinal` / `targetC` path in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`; and
- one bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `test/PipelineSpec.hs`.

The inherited boundary remains fixed:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This round does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
non-local binding work, equi-recursive reasoning, cyclic structural encoding,
multi-SCC widening, cross-family widening, or broader search.

## Accepted Carry-Forward Chain

The accepted predecessor chain remains unchanged and is treated as binding input:

1. `E2` authoritative `attempt-2` landed only the bounded same-lane retained-child
   `boundVarTarget` / nested-`forall` fail-closed slice in `Fallback.hs` plus
   focused `PipelineSpec` coverage.
2. `E3` authoritative `attempt-1` reverified that exact `E2` slice under the
   focused `ARI-C1` rerun and a passing full repo gate.
3. `E4` authoritative `attempt-1` finalized `continue-bounded`, so one more
   bounded non-widening slice was the only lawful successor action.
4. `F1` authoritative `round-042` froze exactly one future target: the
   local-binding scheme-alias/base-like `keepTargetFinal` / `targetC` lane in
   `Fallback.hs`, with ownership limited to `Fallback.hs` and `PipelineSpec.hs`.
5. Accepted negative findings remain binding: `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`, and
   `U4 = constructor-acyclic-termination-refuted`.

## Implemented Code Slice

The production edit stays inside the existing `Fallback.hs:521-686` local block
that computes `rootIsSchemeAlias`, `rootBoundIsBaseLike`, `keepTargetFinal`, and
`targetC`.

Implemented change:

- introduced `rootLocalSchemeAliasBaseLike` as the explicit reviewer-auditable
  proof `rootBindingIsLocalType && rootIsSchemeAlias && rootBoundIsBaseLike`;
- routed `keepTargetFinal` through that local proof instead of repeating the raw
  conjunction inline; and
- made `targetC` select `rootFinal` explicitly for that one local
  scheme-alias/base-like lane before falling back to the pre-existing generic
  `TyVar` / `boundVarTarget` / `schemeBodyTarget` branches.

Scope discipline preserved:

- `boundVarTarget` was treated as absent for the selected `F2` slice; the new
  behavioral examples use a direct local scheme-alias wrapper with no retained-child
  candidate.
- `rootHasMultiInst` and `instArgRootMultiBase` remain unchanged and out of scope.
- No replay reopen, `MLF.Elab.Inst`, `InstBot`, or broader widening path was
  touched.

## Bounded `PipelineSpec` Coverage

The existing `ARI-C1 feasibility characterization (bounded prototype-only)` block
was extended with exactly one new positive behavioral example and one matched
fail-closed contrast:

- Positive example:
  `keeps local scheme-alias/base-like fallback on the local TypeRef lane`
  This rewrites a direct local `EVar "k"` wrapper so the root stays on the local
  `TypeRef` lane while the scheme alias is rebound to an `Int` base node. The
  expected result is the quantified fail-closed shell `forall _. Int`, proving the
  selected local `keepTargetFinal` / `targetC` path is exercised directly.
- Matched fail-closed contrast:
  `keeps the same scheme-alias/base-like wrapper fail-closed once it leaves the local TypeRef lane`
  This uses the same wrapper and same base-like rebound root, but leaves the root
  on its non-local `GenRef` lane. The expected result stays fail-closed as plain
  `Int`, with `containsMu False`.

The inherited retained-child `E2` / `E3` examples remain in place as historical
baseline only and were not reinterpreted as the new `F2` evidence.

## Verification

Verification ran in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-043`.

### TDD Red -> Green

- Red:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  failed exactly in
  `uses the local-binding gate when deciding retained fallback targets`
  because `Fallback.hs` did not yet define the explicit
  `rootLocalSchemeAliasBaseLike` proof or the corresponding `targetC` branch.
- Green:
  rerunning the same focused command passed with
  `11 examples, 0 failures`.

### Baseline Controller Checks

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - roadmap items `1` through `12` remain parseable, with `10`, `11`, and `12`
    still pending exactly as expected for pre-merge `F2`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### Focused Bounded Checks

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `11 examples, 0 failures`
- `rg -n 'rootIsSchemeAlias|rootBoundIsBaseLike|keepTargetFinal|targetC|boundVarTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
  -> pass:
  - `test/PipelineSpec.hs:1358` records the new
    `rootLocalSchemeAliasBaseLike` source guard
  - `Fallback.hs:521-730` still contains the selected anchors and unchanged
    inherited `boundVarTarget` family
- `git diff --name-only` -> pass:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
- `git status --short --untracked-files=all` -> reviewer continuity evidence:
  - modified only:
    `src/MLF/Elab/Run/ResultType/Fallback.hs`,
    `test/PipelineSpec.hs`
  - new `F2` evidence files only:
    `docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`,
    `orchestrator/rounds/round-043/implementation-notes.md`
  - pre-existing untracked round control files remained untouched:
    `orchestrator/rounds/round-043/plan.md`,
    `orchestrator/rounds/round-043/selection.md`

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - `cabal build all` rebuilt `mlf2-test`, `mlf2`, and `frozen-parity-gen`
  - `cabal test` finished with `1132 examples, 0 failures`

### Continuity Statement

Completed predecessor rounds, accepted boundary docs, accepted `E2` / `E3` / `E4`
/ `F1` history, `Bugs.md`, roadmap history, controller state, `src-public/`,
`app/`, and `mlf2.cabal` were left untouched.
