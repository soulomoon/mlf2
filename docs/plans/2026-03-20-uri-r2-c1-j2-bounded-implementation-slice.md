# `J2` Bounded Implementation Slice For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-059`
Roadmap item: `J2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded implementation + verification record

## Stage Contract

This artifact records only roadmap item `J2` for `attempt-1`.

The round stays inside the accepted `J1` freeze:

- live subject remains repaired `URI-R2-C1`;
- inherited boundary remains explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback;
- ownership stays bounded to
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-059/src/MLF/Elab/Run/ResultType/Fallback.hs`
  and
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-059/test/PipelineSpec.hs`;
- completed `rootLocalSingleBase`, preserved scheme-alias/base-like
  `baseTarget`, `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
  retained-target / `keepTargetFinal`, replay reopen, `MLF.Elab.Inst`,
  `InstBot`, `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `ResultType.View`, and non-local widening remain excluded as separate
  families.

Pre-existing controller preparation files remained present in the worktree and
were left untouched:

- `orchestrator/state.json`
- `orchestrator/rounds/round-059/selection.md`
- `orchestrator/rounds/round-059/plan.md`

## Implemented Slice

`src/MLF/Elab/Run/ResultType/Fallback.hs` keeps the existing selected
`baseTarget` branch at `Fallback.hs:382-387` unchanged and adds one new
reviewer-auditable local proof at `Fallback.hs:531-536`:

- `rootLocalInstArgSingleBase`
  - `rootBindingIsLocalType`
  - `IntSet.null rootBaseBounds`
  - `IntSet.size instArgBaseBounds == 1`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`

The downstream same-lane selection now consumes only that bounded proof:

- `targetC` adds `Just baseC | rootLocalInstArgSingleBase -> baseC` at
  `Fallback.hs:696-697`;
- the completed `rootLocalSingleBase` arm remains first and unchanged;
- the preserved scheme-alias/base-like `baseTarget` arm remains unchanged; and
- `keepTargetFinal` plus the retained-target / non-local fail-closed paths are
  unchanged, so `J2` does not reopen adjacent families.

## Focused Coverage

`test/PipelineSpec.hs` extends the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block only.

Added focused helper:

- `localInstArgSingleBaseFallback`
  - preserves the existing local wrapper shape used in the bounded `ARI-C1`
    lane;
  - clears the inherited child bound so `rootBaseBounds` stays empty;
  - injects exactly one inst-argument singleton witness for `Int` by
    rewriting one edge witness payload rather than widening trace families;
  - for the positive local case only, rebinds the root directly to `Int` so
    the selected `baseTarget` path has a concrete root base to consume while
    `rootBoundCandidates` remains empty; and
  - keeps `rootHasMultiInst = False` and `instArgRootMultiBase = False`.

Added examples:

- positive same-lane success:
  `keeps local inst-arg-only singleton-base fallback on the local TypeRef lane`
  expects `TBase (BaseTy "Int")`;
- matched fail-closed contrast:
  `keeps the same inst-arg-only singleton-base wrapper fail-closed once it leaves the local TypeRef lane`
  expects `TForall _ Nothing (TVar _)`; and
- refreshed source guard:
  `uses the local-binding gate when deciding local single-base and retained fallback targets`
  now requires the explicit `rootLocalInstArgSingleBase` proof and the
  dedicated `targetC` arm while preserving the existing guards for
  `rootLocalSingleBase`, `rootLocalSchemeAliasBaseLike`,
  `rootLocalMultiInst`, `rootLocalInstArgMultiBase`, and `keepTargetFinal`.

## TDD Evidence

Focused red run during helper finalization before the bounded production fix:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - failed with the intended local-lane symptom once the helper isolated the
    selected slice:
    `keeps local inst-arg-only singleton-base fallback on the local TypeRef lane`
    returned `TVar "t16"` instead of `TBase Int`;
  - also failed the new source guard because `Fallback.hs` did not yet define
    `rootLocalInstArgSingleBase` or the dedicated `targetC` arm.

Focused green run after the bounded production change:

- same command as above
  - passed with `19 examples, 0 failures`.

## Continuity And Exclusions

`/Volumes/src/mlf4/Bugs.md` remains continuity context only for this round.
Its `Open` section is still empty, and no bug-tracker mutation was required
for this bounded `J2` slice.

Preserved exclusions:

- no replay reopen;
- no `MLF.Elab.Inst` or `InstBot` work;
- no `boundVarTarget` or `boundTarget` widening;
- no `schemeBodyTarget` or `ResultType.View` work;
- no non-local widening or broader recursive-inference search;
- no equi-recursive reasoning, implicit unfolding, or cyclic structural
  encoding; and
- no compatibility fallback, convenience shim, or second interface.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-059`

Required verification:

- `git diff --check` -> pass
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass
- `cabal build all && cabal test`
  -> pass (`1140` examples, `0` failures)
