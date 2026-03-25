# Round 047 Plan (`G2` Bounded Local Multi-Inst Hardening)

## Objective

Execute only roadmap item `G2` and produce one accepted bounded implementation
artifact at:
`docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`.

This is the initial `G2` plan for `attempt-1` with `retry: null`. The round
must implement only the accepted `G1`-frozen local-binding `rootHasMultiInst`
`keepTargetFinal` / `targetC` fail-closed lane under repaired `URI-R2-C1`,
preserve the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
no-second-interface / no-fallback boundary, and keep `instArgRootMultiBase`
explicitly out of scope for this cycle.

The selected `G2` implementation intent is:

- keep the already accepted `F2` / `F3` local-binding scheme-alias/base-like
  lane unchanged as inherited baseline context, not reopened target selection;
- add one reviewer-auditable local proof in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  that admits retained final-target selection only when the selected lane is
  carried by `rootBindingIsLocalType && rootHasMultiInst`;
- route the selected `keepTargetFinal` / `targetC` behavior through that one
  local multi-inst proof without relying on `instArgRootMultiBase`,
  `rootLocalSchemeAliasBaseLike`, or `boundVarTarget` as authority for the
  selected slice;
- add one bounded same-lane local multi-inst success example and one matched
  fail-closed non-local contrast inside the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `test/PipelineSpec.hs`; and
- keep every non-selected trigger family and excluded widening lane fail-closed.

## Locked Round Context

- Round id: `round-047`
- Roadmap item: `G2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: bounded implementation only; no roadmap or controller-state edits

Accepted carry-forward facts that must remain unchanged throughout `G2`:

- `E2` / `E3` remain completed same-lane retained-child baseline evidence only.
  They are inherited context, not reopened target-selection authority.
- `F2` authoritative `round-043` landed only the bounded
  `rootLocalSchemeAliasBaseLike` / `targetC -> rootFinal` lane in
  `Fallback.hs` plus focused `PipelineSpec` coverage, while keeping
  `boundVarTarget` absent for the selected slice and leaving
  `rootHasMultiInst` / `instArgRootMultiBase` unchanged and inherited-only.
- `F3` authoritative `round-044` reverified that exact `F2` lane under
  read-only anchors, a fresh focused `ARI-C1` rerun, a fresh full repo gate,
  and predecessor continuity.
- `F4` authoritative `round-045` finalized `continue-bounded`.
- `G1` authoritative `round-046` then froze exactly one next bounded `G2`
  slice: the local-binding `rootHasMultiInst` `keepTargetFinal` / `targetC`
  fail-closed lane, while explicitly leaving `instArgRootMultiBase`
  unselected.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains replay-lane continuity context only and
  does not authorize replay reopen, `MLF.Elab.Inst`, or `InstBot` work here.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/implementer.md`
- `orchestrator/rounds/round-047/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-014/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-014/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-014/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
- `docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `docs/plans/2026-03-19-uri-r2-c1-f4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
- `orchestrator/rounds/round-046/review-record.json`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

### Modify / Test

1. `test/PipelineSpec.hs`
   - Responsibility: add exactly one bounded same-lane local multi-inst success
     example and one matched fail-closed non-local contrast inside the existing
     `ARI-C1 feasibility characterization (bounded prototype-only)` block.
   - TDD rule: write the new focused examples first, watch the selected success
     example fail for the intended reason, then land the minimal production fix.

### Modify

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
   - Responsibility: add the minimal reviewer-auditable local multi-inst proof
     and route the selected `keepTargetFinal` / `targetC` behavior only through
     that one accepted `G1`-frozen lane.

2. `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
   - Responsibility: record the canonical bounded `G2` implementation result and
     the exact focused positive/negative evidence for the selected lane.

3. `orchestrator/rounds/round-047/implementation-notes.md`
   - Responsibility: summarize the bounded code/test/doc change and verification
     results for reviewer handoff.

### Evidence-Only, No Planned Edit

- `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
- `orchestrator/rounds/round-046/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`

These carry the accepted bounded selection and continuity context that `G2`
must preserve without rewriting.

## Sequential Tasks

### Task 1 - Lock the bounded `G2` contract before production edits

- Treat this round as the direct implementation of the accepted `G1` bind, not
  a new target-selection round.
- Preserve the live subject fixed to repaired `URI-R2-C1`.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary.
- Preserve the accepted `F2` / `F3` scheme-alias/base-like lane as inherited
  baseline context only.
- Keep `instArgRootMultiBase`, `boundVarTarget`, and non-local widening
  explicitly out of scope for the selected slice.

### Task 2 - TDD the selected local multi-inst lane in `PipelineSpec`

- Work only inside the existing
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in `test/PipelineSpec.hs`.
- Add exactly one new bounded same-lane local multi-inst success example that:
  - exercises pipeline behavior for the selected local `rootHasMultiInst` lane;
  - does not rely on `instArgRootMultiBase`, `rootLocalSchemeAliasBaseLike`, or
    `boundVarTarget` as the selected authority; and
  - is distinct from the already accepted direct-wrapper and retained-child
    evidence.
- Add exactly one matched fail-closed non-local contrast where the wrapper
  leaves the admissible local `TypeRef` lane and must still reject or fall back
  non-recursively.
- Run the focused `ARI-C1` block and watch the new selected success example fail
  before touching `Fallback.hs`.

### Task 3 - Land the minimal `Fallback.hs` proof for the selected lane

- Add one reviewer-auditable local proof in `Fallback.hs` for the selected
  slice, for example a helper on the shape
  `rootBindingIsLocalType && rootHasMultiInst`.
- Use that local proof to admit retained final-target selection only for the
  selected `G1`-frozen lane.
- Keep `instArgRootMultiBase` out of scope and fail-closed for this cycle.
- Do not reopen replay repair, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`
  widening, non-local binding widening, or the already accepted `F2` lane.
- Prefer the smallest total Haskell change that satisfies the failing focused
  test while preserving existing accepted behavior.

### Task 4 - Refresh the canonical `G2` artifact and round notes

- Write
  `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
  for `attempt-1`.
- Record:
  - the selected local `rootHasMultiInst` proof;
  - the exact positive same-lane success example;
  - the matched fail-closed non-local contrast;
  - the bounded ownership (`Fallback.hs` and `PipelineSpec.hs` only); and
  - the preserved exclusions on `instArgRootMultiBase`, replay reopen,
    `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`, non-local widening, and
    broader recursive inference.
- Record the same bounded summary and verification results in
  `orchestrator/rounds/round-047/implementation-notes.md`.

### Task 5 - Run the required bounded verification

- Re-run baseline contract checks from
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-014/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-047/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-047/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-014/roadmap.md`
  - required design/boundary file presence checks
- Re-run the focused bounded block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Because this round edits `src/` and `test/`, the full repo gate is mandatory:
  - `cabal build all && cabal test`
- Reconfirm the diff stays bounded to:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
  - `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
  - `orchestrator/rounds/round-047/implementation-notes.md`

## Non-Goals

- No change to the accepted `G1` bind itself.
- No change to the accepted `F2` / `F3` lane except as inherited context.
- No selection or implementation of `instArgRootMultiBase` in this cycle.
- No edits to `src/MLF/Elab/Inst.hs`.
- No replay reopen, `InstBot` work, prototype/research entrypoints, public API
  changes, executable changes, roadmap mutation, `Bugs.md` edits, or
  `mlf2.cabal` changes.
- No widening into equi-recursive reasoning, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, cross-family search, heuristic
  owner selection, compatibility shims, or convenience fallbacks.

## Reviewer Checks For This Round

1. `plan.md` keeps `G2` bounded to the accepted `G1` `rootHasMultiInst` slice.
2. `PipelineSpec.hs` adds exactly one new bounded same-lane local multi-inst
   success example plus one matched fail-closed non-local contrast inside the
   existing `ARI-C1` block, and the success example was seen failing before the
   production fix.
3. `Fallback.hs` introduces only the minimal local multi-inst proof needed for
   the selected lane and does not widen into `instArgRootMultiBase` or other
   excluded families.
4. The canonical `G2` artifact and round notes explicitly record the bounded
   positive/negative evidence and preserved exclusions.
5. The focused `ARI-C1` block passes, the full repo gate passes, and the diff
   stays bounded to the four owned files only.
