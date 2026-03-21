# `K2` Bounded Implementation Slice For Repaired `URI-R2-C1`

Date: 2026-03-21
Round: `round-063`
Roadmap item: `K2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded implementation slice

## Stage Contract

This artifact records only roadmap item `K2` for `attempt-1`.

The round stays inside the accepted `K1` freeze:

- live subject remains repaired `URI-R2-C1`;
- inherited boundary remains explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback;
- ownership stays bounded to
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-063/src/MLF/Elab/Run/ResultType/Fallback.hs`
  and
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-063/test/PipelineSpec.hs`; and
- accepted `F2/F3` `rootLocalSchemeAliasBaseLike` / `rootFinal` continuity,
  completed `rootLocalSingleBase`, completed `rootLocalInstArgSingleBase`,
  retained-target logic, replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget`, `schemeBodyTarget`, `ResultType.View`, and
  non-local widening remain excluded as separate families.

Pre-existing controller-preparation files remained present in the worktree and
were left untouched:

- `orchestrator/state.json`
- `orchestrator/rounds/round-063/selection.md`
- `orchestrator/rounds/round-063/plan.md`

## Implemented Slice

`src/MLF/Elab/Run/ResultType/Fallback.hs` keeps the existing selected
empty-candidate / no-inst-arg `baseTarget` branch unchanged and adds one new
reviewer-auditable local proof:

- `rootLocalEmptyCandidateSchemeAliasBaseLike`
  - `rootBindingIsLocalType`
  - `rootIsSchemeAlias`
  - `rootBoundIsBaseLike`
  - `IntSet.null rootBoundCandidates`
  - `IntSet.null instArgBaseBounds`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`

The downstream same-lane selection now consumes only that bounded proof:

- `targetC` adds `Just baseC | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC`;
- completed `rootLocalSingleBase` remains first and unchanged;
- completed `rootLocalInstArgSingleBase` remains second and unchanged;
- the broader `rootIsSchemeAlias && rootBoundIsBaseLike` `baseTarget` route
  remains in place after the new local arm; and
- `keepTargetFinal` plus the inherited `rootLocalSchemeAliasBaseLike` /
  `rootFinal` continuity are unchanged, so `K2` does not reopen adjacent
  families.

The selected behavioral lane was already reachable before the production edit;
`K2` makes that lane explicit and reviewer-auditable without widening the
accepted continuity families.

## Focused Coverage

`test/PipelineSpec.hs` extends only the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block.

Added focused helper:

- `localEmptyCandidateSchemeAliasBaseLikeFallback`
  - keeps the existing local app-wrapper family used across the bounded
    `ARI-C1` lanes;
  - keeps the root on the local `TypeRef` lane;
  - rebinds the root directly to the `Int` base node so the selected
    `baseTarget` path has a concrete base-like target;
  - clears the inherited child bound so both `rootBoundCandidates` and
    `instArgBaseBounds` stay empty; and
  - keeps `rootHasMultiInst = False` and `instArgRootMultiBase = False`.

Recorded examples:

- positive same-lane success:
  `keeps local empty-candidate scheme-alias/base-like fallback on the local TypeRef lane`
  expects `TBase (BaseTy "Int")`;
- matched local continuity contrast:
  `keeps the matched local scheme-alias/base-like continuity on the quantified rootFinal lane`
  reuses the accepted local `schemeAliasBaseLikeFallback True` wrapper and
  expects `TForall _ Nothing (TBase (BaseTy "Int"))`; and
- refreshed source guard:
  `uses the local-binding gate when deciding local single-base and retained fallback targets`
  now requires the explicit `rootLocalEmptyCandidateSchemeAliasBaseLike` proof,
  the dedicated same-lane `targetC` arm, and the preserved broader
  scheme-alias/base-like `baseTarget` route.

## TDD Evidence

Focused RED before the production change:

- `PATH=/Users/ares/.ghcup/bin:$PATH /Users/ares/.ghcup/bin/cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - failed exactly in the refreshed source guard because `Fallback.hs` did not
    yet define `rootLocalEmptyCandidateSchemeAliasBaseLike` or the dedicated
    `targetC` arm.

Focused GREEN after the bounded production change:

- same command as above
  - passed with `20 examples, 0 failures`.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-063`

Required verification:

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `16:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - roadmap item `30` remains parseable and pending during implement stage
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  plus the inherited boundary docs and `orchestrator/retry-subloop.md`
  -> pass
- focused `ARI-C1` command above -> pass (`20 examples, 0 failures`)
- `cabal build all && cabal test`
  -> pass (`1141` examples, `0` failures)

## Continuity And Exclusions

`/Volumes/src/mlf4/Bugs.md` remains continuity context only for this round.
Its `Open` section is still empty, and no bug-tracker mutation was required
for this bounded `K2` slice.

Preserved exclusions:

- no replay reopen;
- no `MLF.Elab.Inst` or `InstBot` work;
- no `boundVarTarget` or `boundTarget` widening;
- no `schemeBodyTarget` or `ResultType.View` work;
- no non-local widening or broader recursive-inference search;
- no equi-recursive reasoning, implicit unfolding, or cyclic structural
  encoding; and
- no compatibility fallback, convenience shim, or second interface.
