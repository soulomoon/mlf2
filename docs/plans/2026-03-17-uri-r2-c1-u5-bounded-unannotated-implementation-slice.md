# `U5` Bounded Unannotated Implementation Slice

Date: 2026-03-18
Round: `round-032`
Roadmap item: `U5`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded result-type/pipeline hardening slice

## Stage Contract Freeze

This artifact implements only roadmap item `U5` for `attempt-1` with `retry: null`.

The live subject remains repaired `URI-R2-C1` only. The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

Accepted `U4` result `constructor-acyclic-termination-refuted` remains a fail-closed boundary. This round does not reinterpret `U4` as positive admission clearance for broad unannotated recursive inference.

## Carry-Forward From `U1` Through `U4`

Accepted predecessor facts carried forward without rewrite:

1. `U1` (`round-028`) bound the successor lane to repaired `URI-R2-C1` under explicit-only / non-equi-recursive / non-cyclic-graph constraints.
2. `U2` (`round-029`) finalized `authority-narrowed`; no fallback- or shim-manufactured unannotated authority was cleared.
3. `U3` (`round-030`) finalized `uniqueness-owner-stable-refuted`; exactly one admissible local owner/root was not established.
4. `U4` (`round-031`) finalized `constructor-acyclic-termination-refuted`; bounded feasibility remained fail-closed.

Therefore `U5` stays bounded. It may harden the current result-type/pipeline lane, but it may not widen into equi-recursive reasoning, implicit unfolding, cyclic encoding, heuristic owner selection, cross-subject search, a second interface, or a convenience fallback path.

## Bounded Production Slice

Production edits remain inside `src/MLF/Elab/Run/ResultType/Fallback.hs` only.

The bounded code change keeps the non-annotated result-type lane behavior intact while surfacing one reviewer-auditable authority detail already present in the bounded pipeline state:

- record whether the fallback root still binds through a local `TypeRef` (`rootBindingIsLocalType`) after the final result-type overlay and canonical scope resolution; and
- expose that value in the existing result-type debug trace so later bounded reviews can see whether a recursive-looking result came from a local type binding or from some broader wrapper/proxy shape.

This slice is intentionally fail-closed: it adds no new recursive success path, no replay-lane reuse from `MLF.Elab.Inst`, and no widening beyond the current bounded result-type lane.

## Focused Pipeline Coverage

Focused regression coverage remains in `test/PipelineSpec.hs` only:

- the annotation-anchored recursive control now checks both unchecked and checked pipeline elaboration paths, proving the bounded explicit recursive shape remains processable;
- the corresponding unannotated lambda control still proves no recursive `TMu` shape is inferred; and
- the out-of-scope unannotated recursive proxy rejection now checks both unchecked and checked paths, so the bounded failure remains stable across both pipeline entrypoints.

These examples stay local and bounded; they do not widen into multi-SCC, cross-family, equi-recursive, or search-wide recursive inference.

## Files Changed

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `orchestrator/rounds/round-032/implementation-notes.md`

## Non-Authorization Statement

This round does not authorize broader unannotated recursive inference and does not pre-decide `U6`.

It preserves the accepted `U4` fail-closed result, keeps repaired `URI-R2-C1` as the only live subject, and introduces no second interface, compatibility shim, convenience fallback, or default-on widening path.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-032`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `29:1. [done] Execute the `U1` inherited baseline and repaired-subject bind for unannotated iso-recursive inference`
  - `33:2. [done] Execute the `U2` provenance-stable unannotated authority clearance for the live subject`
  - `37:3. [done] Execute the `U3` uniqueness and owner-stability clearance for the live subject`
  - `41:4. [done] Execute the `U4` constructor-directed / acyclicity / termination clearance for the live subject`
  - `45:5. [pending] Execute the `U5` bounded solver/pipeline implementation slice for the still-bound live subject under the `U4` refuted result`
  - `49:6. [pending] Execute the `U6` end-to-end verification and next-widening decision gate`
- `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### Focused `U5` Checks

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'` -> pass:
  - `3 examples, 0 failures`
- `git diff --name-only` -> bounded diff:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - `mlf2-test` finished with `1124 examples, 0 failures`.

The verification evidence confirms the production diff stayed inside the single bounded `ResultType/Fallback` slice, the test diff stayed inside focused `PipelineSpec` coverage, repaired `URI-R2-C1` remained the only live subject, and no second interface, compatibility shim, convenience fallback, or default-path widening was introduced.
