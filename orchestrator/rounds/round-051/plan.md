# Round 051 Plan (`H2` Bounded Local Inst-Argument Multi-Base Hardening)

## Objective

Execute only roadmap item `H2` and produce one accepted bounded implementation
artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`.

This is the initial `H2` plan for `attempt-1` with `retry: null`. The round
must implement only the accepted `H1`-frozen local-binding
`instArgRootMultiBase` `keepTargetFinal` / `targetC` fail-closed lane under
repaired `URI-R2-C1`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
boundary, and keep `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
`boundVarTarget` explicitly out of scope as selected authority for this cycle.

The selected `H2` implementation intent is:

- keep the accepted `G2` / `G3` local `rootLocalMultiInst` /
  `targetC -> rootFinal` lane unchanged as inherited baseline context, not
  reopened target selection;
- keep the existing inst-argument multi-base aggregation in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/src/MLF/Elab/Run/ResultType/Fallback.hs:289-359`
  as the only source of `instArgRootMultiBase`, but surface one new explicit
  reviewer-auditable local proof of the shape
  `rootBindingIsLocalType && instArgRootMultiBase`;
- route `keepTargetFinal` and the selected `targetC` branch through that one
  local multi-base proof without relaxing the existing `baseTarget`
  fail-closed guards that still reject multi-base collapse outside the selected
  local lane;
- add one bounded same-lane local multi-base success example and one matched
  fail-closed non-local contrast inside the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/test/PipelineSpec.hs`,
  then refresh the source-guard assertion only as needed to name the selected
  authority; and
- keep every non-selected trigger family and excluded widening lane fail-closed.

## Locked Round Context

- Round id: `round-051`
- Roadmap item: `H2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: bounded implementation only; no roadmap or controller-state edits
- Current round review feedback: none yet; this is a full `attempt-1` plan, not
  a retry delta plan

Accepted carry-forward facts that must remain unchanged throughout `H2`:

- `H1` authoritative `round-050` froze exactly one future `H2` slice: the
  remaining local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC`
  lane in `Fallback.hs`, with future ownership limited to `Fallback.hs` and
  `PipelineSpec.hs`.
- `G2` authoritative `round-047` landed only the bounded local
  `rootLocalMultiInst = rootBindingIsLocalType && rootHasMultiInst` proof and
  corresponding `targetC -> rootFinal` behavior, with one focused positive
  example and one matched non-local fail-closed contrast.
- `G3` authoritative `round-048` reverified only that exact `G2` lane under
  read-only anchors, a fresh focused `ARI-C1` rerun, a fresh full repo gate,
  and predecessor continuity.
- `G4` authoritative `round-049` finalized `continue-bounded`, not
  `widen-approved` and not `stop-blocked`, so one more bounded non-widening
  implementation slice is the only lawful successor action.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `BUG-2026-03-16-001` in `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/Bugs.md`
  remains replay-lane continuity context only and does not authorize replay
  reopen, `MLF.Elab.Inst`, `InstBot`, or any widening in this round.

Current repository state is already non-pristine (`orchestrator/state.json`
modified and `orchestrator/rounds/round-051/selection.md` untracked). Respect
those existing changes. Do not revert or "clean up" unrelated work while
preparing the `H2` slice.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/AGENTS.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/roles/implementer.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/rounds/round-051/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/rounds/round-050/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/test/PipelineSpec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/Bugs.md`

## Files Expected In Scope

### Modify / Test

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/test/PipelineSpec.hs`
   - Responsibility: add exactly one bounded same-lane local multi-base
     success example, one matched fail-closed non-local contrast, and refresh
     the source-guard assertion inside the existing
     `ARI-C1 feasibility characterization (bounded prototype-only)` block.
   - TDD rule: write the new focused examples first, watch the selected success
     example or refreshed source guard fail for the intended reason, then land
     the minimal production fix.

### Modify

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/src/MLF/Elab/Run/ResultType/Fallback.hs`
   - Responsibility: keep the existing `instArgRootMultiBase` aggregation as
     the source of truth, add the minimal reviewer-auditable local proof for
     the selected lane, and route the selected `keepTargetFinal` / `targetC`
     behavior only through that one accepted `H1`-frozen lane while preserving
     base-target fail-closed behavior.

2. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
   - Responsibility: record the canonical bounded `H2` implementation result
     and the exact focused positive/negative evidence for the selected lane.

3. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/rounds/round-051/implementation-notes.md`
   - Responsibility: summarize the bounded code/test/doc change and
     verification results for reviewer handoff.

### Evidence-Only, No Planned Edit

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/rounds/round-050/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/Bugs.md`

These carry the accepted bounded selection and continuity context that `H2`
must preserve without rewriting.

## Sequential Tasks

### Task 1 - Lock the bounded `H2` contract before production edits

- Treat this round as the direct implementation of the accepted `H1` bind, not
  a new target-selection round.
- Preserve the live subject fixed to repaired `URI-R2-C1`.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary.
- Preserve the accepted `G2` / `G3` local multi-inst lane as inherited
  baseline context only.
- Keep `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
  replay reopen, and non-local widening explicitly out of scope for the
  selected slice.
- Keep the selected work confined to the existing `instArgRootMultiBase`
  aggregation at `Fallback.hs:289-359` plus the local `keepTargetFinal` /
  `targetC` decision path at `Fallback.hs:671-697`.
- Do not relax the `baseTarget` fail-closed guards that currently require
  `not instArgRootMultiBase` around `Fallback.hs:368-402`.

### Task 2 - TDD the selected local multi-base lane in `PipelineSpec`

- Work only inside the existing
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in `test/PipelineSpec.hs`.
- Add one focused helper adjacent to the current bounded helper cluster
  (alongside `localMultiInstFallback`) that synthesizes the selected
  `instArgRootMultiBase` lane using the existing trace-duplication and
  bound-rewrite utilities:
  - preserve the root on the local `TypeRef` lane when requested;
  - force the aggregation at `Fallback.hs:289-359` to observe more than one
    admissible inst-argument base for the same root; and
  - keep `rootHasMultiInst`, retained-child `boundVarTarget`, and
    scheme-alias/base-like authority from becoming the selected reason the case
    succeeds.
- Add exactly one new bounded same-lane local multi-base success example that:
  - exercises pipeline behavior for the selected local
    `instArgRootMultiBase` lane;
  - expects retention of the final target/root variable rather than the
    quantified fail-closed shell; and
  - does not rely on `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or
    `boundVarTarget` as selected authority.
- Add exactly one matched fail-closed non-local contrast where the same-looking
  wrapper leaves the admissible local `TypeRef` lane and must still reject or
  fall back non-recursively.
- Refresh the existing source-guard example
  `uses the local-binding gate when deciding retained fallback targets` so it
  names the new local multi-base proof and the selected `targetC` branch
  without dropping the accepted `rootLocalMultiInst` and
  `rootLocalSchemeAliasBaseLike` guards.
- Run the focused `ARI-C1` block and watch the new selected success example or
  refreshed source guard fail before touching `Fallback.hs`.

### Task 3 - Land the minimal `Fallback.hs` proof for the selected lane

- Keep the existing `instArgRootMultiBase` aggregation in
  `Fallback.hs:289-359` as the only source of the selected trigger family.
- Introduce one explicit reviewer-auditable local proof for the selected lane,
  for example
  `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`.
- Route `keepTargetFinal` through that local multi-base proof instead of
  carrying the selected slice directly on the raw `instArgRootMultiBase` flag.
- Add the minimal explicit `targetC` branch needed to keep the selected local
  multi-base lane on `rootFinal`, preferably alongside the existing
  `rootLocalSchemeAliasBaseLike || rootLocalMultiInst` branch so the selected
  authority is obvious to reviewers.
- Preserve the current `baseTarget` fail-closed behavior for multi-base cases;
  `instArgRootMultiBase` must continue blocking base collapse outside the
  selected local `keepTargetFinal` lane.
- Do not reopen replay repair, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`
  widening, non-local binding widening, or the already accepted
  `rootLocalMultiInst` / `rootLocalSchemeAliasBaseLike` lanes.
- Prefer the smallest total Haskell change that satisfies the failing focused
  test while preserving existing accepted behavior.

### Task 4 - Refresh the canonical `H2` artifact and round notes

- Write
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
  for `attempt-1`.
- Record:
  - the selected local multi-base proof and the fact that the upstream
    aggregation remains the source of `instArgRootMultiBase`;
  - the exact positive same-lane success example;
  - the matched fail-closed non-local contrast;
  - the bounded ownership (`Fallback.hs` and `PipelineSpec.hs` only); and
  - the preserved exclusions on `rootHasMultiInst`,
    `rootLocalSchemeAliasBaseLike`, `boundVarTarget`, replay reopen,
    `MLF.Elab.Inst`, `InstBot`, non-local widening, and broader recursive
    inference.
- Record the same bounded summary and verification results in
  `orchestrator/rounds/round-051/implementation-notes.md`.

### Task 5 - Run the required bounded verification

- Re-run baseline contract checks from
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/retry-subloop.md`
- Re-run the focused bounded block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Add one focused anchor check proving the selected proof is now explicit while
  the inherited trigger families remain visible but unselected:
  - `rg -n 'rootLocalInstArgMultiBase|instArgRootMultiBase|keepTargetFinal|targetC|rootLocalMultiInst|rootLocalSchemeAliasBaseLike|boundVarTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
- Because this round edits `src/` and `test/`, the full repo gate is mandatory:
  - `cabal build all && cabal test`
- Reconfirm the diff stays bounded to:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
  - `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
  - `orchestrator/rounds/round-051/implementation-notes.md`

### Task 6 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can lawfully emit one of the allowed `H2` combinations:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are supported by the artifact and the checks:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve the immutability rule: if `H2` later retries, this `attempt-1`
  evidence set remains unchanged and later attempts are additive.

## Non-Goals

- No change to the accepted `H1` bind itself.
- No change to the accepted `G2` / `G3` local multi-inst lane except as
  inherited context.
- No reopening of `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or
  `boundVarTarget` as separate target families in this cycle.
- No edits to `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/src/MLF/Elab/Inst.hs`.
- No replay reopen, `InstBot` work, prototype/research entrypoints, public API
  changes, executable changes, roadmap mutation, `Bugs.md` edits, or
  `mlf2.cabal` changes.
- No widening into equi-recursive reasoning, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, cross-family search, heuristic
  owner selection, compatibility shims, or convenience fallbacks.
- No preemption of `H3` or `H4` beyond landing the selected `H2` slice and its
  bounded evidence.

## Reviewer Checks

Baseline checks from
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-051/orchestrator/verification.md`
still apply.

Round-specific checks:

1. `plan.md`, the produced `H2` artifact, and the round notes explicitly name
   `attempt-1` with `retry: null`.
2. `Fallback.hs` keeps `H2` bounded to the accepted `H1`
   `instArgRootMultiBase` slice: the diff stays inside the existing
   aggregation / local-target block, preserves the `baseTarget`
   `not instArgRootMultiBase` fail-closed guards, and makes the new local
   multi-base proof the only new authority for the selected lane.
3. `PipelineSpec.hs` adds exactly one new bounded same-lane local multi-base
   success example plus one matched fail-closed non-local contrast inside the
   existing `ARI-C1` block, and refreshes the source guard only enough to name
   the selected proof.
4. The canonical `H2` artifact and round notes explicitly record the selected
   local multi-base proof, the bounded positive/negative evidence, the
   `H1` continuity chain, the continuity-only status of
   `BUG-2026-03-16-001`, and the preserved exclusions on
   `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
   replay reopen, `MLF.Elab.Inst`, `InstBot`, and non-local widening.
5. The focused `ARI-C1` block passes, the full repo gate passes, and the diff
   stays bounded to the four owned files only.
