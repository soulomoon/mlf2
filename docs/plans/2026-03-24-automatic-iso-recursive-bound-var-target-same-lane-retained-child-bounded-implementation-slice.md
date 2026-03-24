# `N12` Bounded Implementation Slice For The Exact `N11`-Frozen Same-Lane Retained-Child Packet

Date: 2026-03-24
Round: `round-079`
Roadmap item: `N12`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: exact `N11`-frozen same-lane local `TypeRef` retained-child
`boundVarTarget -> targetC` packet
Artifact kind: bounded production/test implementation

## Stage Contract

This artifact implements exactly one bounded `N12` slice inside accepted `N11`.

The round makes the already-frozen same-lane local `TypeRef` retained-child
`boundVarTarget -> targetC` packet reviewer-auditable in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/src/MLF/Elab/Run/ResultType/Fallback.hs`
and refreshes only the existing focused
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/test/PipelineSpec.hs`.

This artifact does not authorize `N13`, `N14`, roadmap edits,
`orchestrator/state.json` edits, bug-tracker edits, predecessor-history
rewrites, replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundTarget`,
`schemeBodyTarget` as a live subject, `ResultType.View`, cross-family
widening, equi-recursive reasoning, implicit unfolding, cyclic structural
encoding, multi-SCC support, or any second interface / compatibility /
convenience fallback.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic-graph structural encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback widening.

## Accepted Carry-Forward Chain

The accepted predecessor chain remains binding input:

1. `L1` remains the fail-closed repaired-queue bind:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
2. `L2` remains the post-`L1` repaired-queue closeout:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
3. `N1` through `N8` remain the accepted post-`L2` reopening / first-lane /
   successor-lane authority chain recorded under
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/`.
4. `N9` selected exactly one fresh planning subject, the retained-child /
   nested-`forall` / binding-structure `boundVarTarget` route:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
5. `N10` froze the safety / acceptance contract for that subject:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-safety-acceptance-contract.md`
6. `N11` froze exactly one packet, the same-lane local `TypeRef`
   retained-child `boundVarTarget -> targetC` route:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
7. Accepted `round-078` review continuity remains binding:
   `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-078/review.md`,
   `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-078/reviews/attempt-1.md`,
   and
   `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-078/review-record.json`
8. `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
   still records that this successor lane needs one bounded implementation
   slice before any verification or next-cycle decision can run.
9. `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` remains read-only
   predecessor context only; open replay / `InstBot` pressure does not reopen
   replay as live work here.

## Why This `N12` Slice Is Code-Changing

Accepted `N11` already froze the exact same-lane retained-child packet in the
source and focused test anchors. `N12` therefore chooses one bounded
code-changing slice rather than a second docs-only restatement:

- `Fallback.hs` already contains the accepted `boundVarTarget` candidate
  search, the same-lane filter, the fail-closed nested-`forall` gate, the
  neighboring `schemeBodyTarget` fallback, and the downstream retained-child
  `targetC` consumer.
- `PipelineSpec.hs` already contains the exact same-lane retained-child
  recursive-positive example, the same-lane source guard, the nested-`forall`
  fail-closed contrast, and the adjacent preserved local continuity examples.
- The smallest lawful `N12` progress is to name one explicit same-lane local
  retained-child proof and route only the existing `keepTargetFinal` /
  `targetC` retained-child consumer through that proof, while leaving every
  adjacent lane unchanged.

## Round-Owned Source/Test Slice

This round owns only these bounded edits:

- `Fallback.hs`
  - keep the frozen `boundVarTarget` candidate search unchanged;
  - add exactly one explicit same-lane local retained-child proof derived only
    from `rootBindingIsLocalType` and the already computed `boundVarTarget`;
    and
  - route only the existing retained-child `keepTargetFinal` / `targetC`
    consumer through that explicit proof.
- `PipelineSpec.hs`
  - stay inside the existing `ARI-C1 feasibility characterization (bounded prototype-only)` block;
  - keep the same-lane local `TypeRef` recursive-positive example;
  - keep the nested-`forall` fail-closed contrast and the adjacent preserved
    local continuity examples; and
  - refresh only the focused source guard so it requires the explicit same-lane
    proof name plus the dedicated `keepTargetFinal` / `targetC` consumer
    routing while preserving the existing same-lane filter and
    `Nothing -> schemeBodyTarget ...` fallback.

## Verification Notes

This artifact remains `attempt-1`. Any future retry must be additive and must
not rewrite this attempt or reviewer-owned history.

### TDD Red -> Green

- Source-guard refresh was written first in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/test/PipelineSpec.hs`.
- Focused red run:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> fail (`20 examples, 1 failure`) in
  `keeps retained-child lookup bounded to the same local TypeRef lane`
  because `Fallback.hs` still lacked the explicit
  `sameLaneLocalRetainedChildTarget` proof and the dedicated
  `keepTargetFinal` / retained-child `targetC` routing through it.
- Focused green rerun after the bounded production edit:
  same command
  -> pass (`20 examples, 0 failures`).

### Required Bounded Verification

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079`

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass (`contract_version: 2`, `retry: null`)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass (`N1`-`N11` done, `N12`-`N14` pending)
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  -> pass
- `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  -> pass
- `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass
- `cabal build all && cabal test` -> pass (`1141 examples, 0 failures`)

### Continuity Statement

This round changed only the bounded round-owned production/test/doc slice:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/test/PipelineSpec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-079/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`

Accepted predecessor docs, roadmap/controller files, `Bugs.md`, and every
non-selected route remained unchanged.
