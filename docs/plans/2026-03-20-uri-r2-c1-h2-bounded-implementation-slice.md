# `H2` Bounded Implementation Slice For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-051`
Roadmap item: `H2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded implementation + verification record

## Stage Contract

This artifact records only roadmap item `H2` for `attempt-1`.

The round stays inside the accepted `H1` freeze:

- live subject remains repaired `URI-R2-C1`;
- inherited boundary remains explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback;
- ownership stays bounded to
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/src/MLF/Elab/Run/ResultType/Fallback.hs`
  and
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/test/PipelineSpec.hs`;
- `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
  replay reopen, `MLF.Elab.Inst`, `InstBot`, non-local widening, and broader
  recursive inference remain excluded as separate target families.

## Implemented Slice

`src/MLF/Elab/Run/ResultType/Fallback.hs` keeps the existing
`instArgRootMultiBase` aggregation at `Fallback.hs:289-359` unchanged as the
only source of that trigger family. The bounded implementation adds one new
reviewer-auditable proof:

- `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`

The downstream selection remains fail-closed outside the selected lane:

- `keepTargetFinal` now routes through `rootLocalInstArgMultiBase` rather than
  the raw `instArgRootMultiBase` flag;
- the selected `targetC` root-retention branch now names
  `rootLocalInstArgMultiBase` alongside the inherited local
  `rootLocalMultiInst` and `rootLocalSchemeAliasBaseLike` branches; and
- the existing `baseTarget` guards still require `not instArgRootMultiBase`,
  so multi-base collapse remains rejected outside the selected local lane.

## Focused Coverage

`test/PipelineSpec.hs` extends the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block only.

Added focused helper:

- `localInstArgMultiBaseFallback`
  - preserves the existing local-wrapper shape used in the bounded `ARI-C1`
    lane;
  - clears the inherited retained-child bound so `boundVarTarget` is not the
    selected reason the case succeeds; and
  - injects one single-trace multi-base witness shape with two fresh
    instantiation-argument vars, one bounded by `Int` and one by `⊥`, so the
    selected proof is `rootBindingIsLocalType && instArgRootMultiBase` rather
    than duplicate-trace `rootHasMultiInst`.

Added examples:

- positive same-lane success:
  `keeps local inst-arg multi-base fallback on the local TypeRef lane`
  expects the retained final/root variable (`TVar _`);
- matched fail-closed contrast:
  `keeps the same inst-arg multi-base wrapper fail-closed once it leaves the local TypeRef lane`
  expects the quantified shell (`TForall _ Nothing (TVar _)`); and
- refreshed source guard:
  `uses the local-binding gate when deciding retained fallback targets`
  now requires the explicit `rootLocalInstArgMultiBase` proof and the selected
  `targetC` root-final branch.

## TDD Evidence

Focused red run before the production edit:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - failed with `15 examples, 1 failure`;
  - failure was the intended source-guard miss: `Fallback.hs` did not yet
    define the explicit `rootLocalInstArgMultiBase` proof.

Focused green run after the production edit:

- same command as above
  - passed with `15 examples, 0 failures`.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051`

Baseline checks:

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass (`contract_version: 2`, `retry: null`)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

Round-specific anchor:

- `rg -n 'rootLocalInstArgMultiBase|instArgRootMultiBase|keepTargetFinal|targetC|rootLocalMultiInst|rootLocalSchemeAliasBaseLike|boundVarTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
  -> pass
  - confirms the explicit selected proof is present in `Fallback.hs`;
  - confirms inherited trigger families remain visible but unchanged as context;
  - confirms the focused source guard in `PipelineSpec.hs` names the selected
    authority.

Required code-path gate:

- `cabal build all && cabal test` -> pass
  - `1136 examples, 0 failures`

## Preserved Non-Authorization

This round still does not authorize:

- replay reopen or `MLF.Elab.Inst` / `InstBot` work;
- non-local binding widening or `boundVarTarget` widening;
- reinterpretation of accepted `U2` / `U3` / `U4` negative findings;
- equi-recursive reasoning, implicit unfolding, or cyclic structural encoding;
- any second interface, compatibility shim, or convenience fallback; or
- roadmap, controller-state, review, or merge bookkeeping edits.
