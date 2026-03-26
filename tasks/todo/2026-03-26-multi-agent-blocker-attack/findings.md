# Findings

## Authority Chain

- [baseline contract](/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md)
  still keeps automatic recursive-type inference unresolved and disabled at
  repo scope.
- [capability contract](/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md)
  still requires representative success across `P1` through `P6` and
  fail-closed / bounded handling for `N1` through `N6`.
- [global keep-vs-reopen gate](/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md)
  remains the last accepted repo-level gate; it selected
  `reopen the non-cyclic-graph revision question`.
- [rev-003 roadmap](/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/roadmap.md)
  repaired only the exact same-lane `C2` / `C5` / `C7` pocket.
- [rev-004 settlement ledger](/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md)
  records the repaired exact pocket on new bounded surfaces only and
  explicitly stops short of a repo-level refresh.

## Blocker Reads

- `C1` / `P2`:
  the admitted non-local packet remains visibly non-recursive on accepted
  production surfaces, and the accepted evidence slice explicitly lacks a
  dedicated authoritative public-output harness for `C1`.
- `P5`:
  the current accepted route remains reject-side only because quantified
  crossings trip the `boundHasForallFrom` / `not hasForall` guard.
- Repo-level:
  there is no accepted post-rev-004 representative-matrix refresh, so the
  repo-level reopen of `non-cyclic-graph` still stands procedurally.

## Code Anchors

- Existing same-lane and non-local fallback tests live in
  `test/PipelineSpec.hs`.
- Current route-selection logic and quantified-boundary guards live in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`.
- Public authoritative surfaces live in `src/MLF/Elab/Run/Pipeline.hs`.

## Execution Results

- New `C1` harness module now records two bounded facts:
  the admitted non-local helper packet still yields `TBase (BaseTy "Int")`
  with `containsMu False`, and the exact source packet still stays
  non-recursive on both current pipeline entrypoints.
- New `P5` probe module now records both sides of the current bounded read:
  the same-lane clear-boundary control remains recursive, while the nested
  `forall` contrast still fails closed.
- The repo-scope refresh draft now records the next lawful sequence after the
  repaired exact same-lane pocket:
  refresh the representative matrix, rerun the global gate, then publish a
  short post-gate handoff if needed.
