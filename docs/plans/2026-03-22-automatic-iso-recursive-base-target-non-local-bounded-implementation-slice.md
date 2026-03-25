# `N5` Bounded Implementation Slice For The Exact `N4`-Frozen Non-Local `baseTarget -> baseC` Packet

Date: 2026-03-22
Round: `round-072`
Roadmap item: `N5`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: exact `N4`-frozen preserved non-local generic scheme-root alias-bound / base-like `baseTarget -> baseC` packet
Artifact kind: bounded production/test implementation

## Stage Contract

This artifact implements exactly one bounded `N5` slice inside accepted `N4`.

The round makes the already-frozen non-local generic scheme-root alias-bound /
base-like `baseTarget -> baseC` packet reviewer-auditable by naming that
packet explicitly in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
and by refreshing only the existing focused
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`test/PipelineSpec.hs`.

This artifact does not authorize `N6`, `N7`, roadmap edits,
`orchestrator/state.json` edits, bug-tracker edits, predecessor-history
rewrites, replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
`boundTarget`, `schemeBodyTarget`, `ResultType.View`, local-lane widening,
cross-family widening, equi-recursive reasoning, implicit unfolding, cyclic
structural encoding, multi-SCC support, or any second interface / compatibility
/ convenience fallback.

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
3. `N1` reopened only a planning lane:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
4. `N2` selected exactly one live planning subject, the preserved generic
   scheme-alias / base-like `baseTarget` route:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
5. `N3` froze the invariant audit / acceptance contract for that subject:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
6. `N4` froze exactly one packet, the preserved non-local generic
   scheme-root alias-bound / base-like `baseTarget -> baseC` packet plus its
   downstream same-lane `targetC` consumer:
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
7. `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
   still records `N5 = NO`, so this round may land only one smallest safe
   slice.
8. `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` remains read-only
   predecessor context only; open replay / `InstBot` pressure does not reopen
   replay as live work here.

## Why This `N5` Slice Is Code-Changing

Accepted `N4` already froze the exact packet in source and in focused test
anchors. `N5` therefore chooses one bounded code-changing slice rather than a
second docs-only restatement:

- `Fallback.hs` already contains the frozen `baseTarget` computation and the
  downstream same-lane `targetC` consumer for the packet.
- `PipelineSpec.hs` already contains the exact non-local positive example
  `schemeAliasBaseLikeFallback False`, the preserved local continuity contrast
  `schemeAliasBaseLikeFallback True`, and the focused source guard.
- The smallest lawful progress is to name the non-local packet explicitly and
  route only that same-lane consumer through the explicit proof, while keeping
  the accepted local lanes unchanged.

## Round-Owned Source/Test Slice

This round owns only these bounded edits:

- `Fallback.hs`
  - keep the frozen `baseTarget` computation unchanged;
  - add exactly one explicit non-local proof derived only from
    `not rootBindingIsLocalType`, `rootIsSchemeAlias`, and
    `rootBoundIsBaseLike`; and
  - route only the existing same-lane non-local `targetC` consumer through
    that explicit proof.
- `PipelineSpec.hs`
  - stay inside the existing `ARI-C1 feasibility characterization (bounded prototype-only)` block;
  - keep `schemeAliasBaseLikeFallback False` as the non-local selected-packet
    success anchor;
  - keep `schemeAliasBaseLikeFallback True` as the preserved local continuity
    contrast; and
  - refresh only the source guard so it requires the explicit non-local proof
    name, the dedicated same-lane `targetC` arm, and the earlier unchanged
    local empty-candidate arm.

## Verification Notes

This artifact remains `attempt-1`. Any future retry must be additive and must
not rewrite this attempt or reviewer-owned history.

### TDD Red -> Green

- Source-guard refresh was written first in
  `test/PipelineSpec.hs`.
- First focused run surfaced a test-harness error in the new negative source
  guard; that test-only predicate shape was corrected before retrying the red
  run.
- Focused red rerun:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> fail (`20 examples, 1 failure`) in
  `keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes`
  because `Fallback.hs` still lacked the explicit
  `rootNonLocalSchemeAliasBaseLike` proof and the dedicated non-local
  `targetC` arm.
- Focused green rerun after the bounded production edit:
  same command
  -> pass (`20 examples, 0 failures`).

### Required Bounded Verification

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-072`

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass (`contract_version: 2`, `retry: null`)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass (`N1`-`N4` done, `N5`-`N7` pending)
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

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
- `orchestrator/rounds/round-072/implementation-notes.md`

Accepted predecessor docs, roadmap/controller files, `Bugs.md`, and every
non-selected route remained unchanged.
