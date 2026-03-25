# Round 055 Plan (`I2` Bounded Local Single-Base Hardening)

## Objective

Execute only roadmap item `I2` and produce one accepted bounded implementation
artifact at:
`docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`.

This is the initial `I2` plan for `attempt-1` with `retry: null`. The round
must implement only the accepted `I1`-frozen local-binding single-base
`baseTarget -> baseC` fail-closed lane, together with its same-lane `targetC`
use, under repaired `URI-R2-C1`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
boundary, and keep replay reopen, retained-child widening, non-local widening,
and every broader trigger family explicitly out of scope.

The selected `I2` implementation intent is:

- keep the accepted `C` / `E` / `F` / `G` / `H` evidence chain and the accepted
  `I1` bind unchanged as inherited baseline context, not reopened target
  selection;
- keep the existing single-base candidate machinery in
  `src/MLF/Elab/Run/ResultType/Fallback.hs:361-408`
  as the only source of `baseTarget`, but surface one new explicit
  reviewer-auditable local single-base proof derived from
  `rootBindingIsLocalType`, singleton `rootBoundCandidates`,
  `not rootHasMultiInst`, and `not instArgRootMultiBase`;
- harden only the local single-base `baseTarget -> baseC` lane through that
  proof, while leaving the empty-candidate, inst-arg-only, retained-child,
  scheme-body, and broader non-local paths fail-closed or inherited context
  only;
- keep `targetC` routing to `baseC` only for that same local single-base lane,
  without turning the selected slice into a new `keepTargetFinal` family or
  widening the accepted retained-target families;
- add exactly one bounded same-lane local single-base success example and one
  matched fail-closed non-local contrast inside the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `test/PipelineSpec.hs`;
  and
- keep replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  `boundTarget` overlay materialization, `View.hs`, `schemeBodyTarget`
  consolidation, non-local widening, and all broader recursive-inference work
  out of scope.

## Locked Round Context

- Round id: `round-055`
- Roadmap item: `I2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: bounded implementation only; no roadmap or controller-state edits
- Current round review feedback: none yet; this is a full `attempt-1` plan, not
  a retry delta plan

Accepted carry-forward facts that must remain unchanged throughout `I2`:

- `I1` finalized in `round-054` as the authoritative bind that froze exactly
  one future `I2` slice: the adjacent local-binding single-base
  `baseTarget -> baseC` fail-closed lane plus the same-lane `targetC`
  selection use in `Fallback.hs`, with future ownership limited to
  `Fallback.hs` and `PipelineSpec.hs`.
- `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md` is the controlling
  accepted predecessor artifact. It explicitly binds the future proof to
  `rootBindingIsLocalType`, singleton `rootBoundCandidates`,
  `not rootHasMultiInst`, and `not instArgRootMultiBase`, and it explicitly
  refuses to authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
  `boundVarTarget`, `boundTarget` overlay materialization,
  `ResultType.View`, `schemeBodyTarget` consolidation, non-local widening, or
  any broader trigger-family widening.
- The accepted `C1` / `C2` / `C3` / `C4` chain already established the bounded
  local-vs-non-local fail-closed baseline around `rootBindingIsLocalType` and
  `schemeBodyTarget`; `I2` must not reopen that baseline as if it were still
  unselected work.
- The accepted `E` / `F` / `G` / `H` cycles already consumed the retained-child
  `boundVarTarget`, local scheme-alias/base-like, local multi-inst, and local
  inst-arg multi-base families as predecessor evidence only. `I2` must not
  re-authorize any of those families or fold the single-base lane into them.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only for this round.
  Its `Open` section is empty, and `BUG-2026-03-16-001` remains resolved only;
  bug state therefore does not authorize replay reopen, `InstBot`, or any
  widening in `I2`.

Current repository state is already non-pristine:

- `M orchestrator/rounds/round-055/state-snapshot.json`
- `?? orchestrator/rounds/round-055/selection.md`

Respect those existing changes. Do not revert or "clean up" unrelated work
while preparing the `I2` slice.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/implementer.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-055/state-snapshot.json`
- `orchestrator/rounds/round-055/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
- `orchestrator/rounds/round-054/review-record.json`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

### Modify / Test

1. `test/PipelineSpec.hs`
   - Responsibility: add exactly one bounded same-lane local single-base
     success example, one matched fail-closed non-local contrast, and refresh
     the existing source-guard assertion inside the existing
     `ARI-C1 feasibility characterization (bounded prototype-only)` block.
   - TDD rule: write the new focused examples first, watch the selected success
     example or refreshed source guard fail for the intended reason, then land
     the minimal production fix.

### Modify

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
   - Responsibility: keep the current `baseTarget` machinery as the source of
     truth, add the minimal explicit reviewer-auditable local single-base proof
     for the selected lane, and route only that lane's `baseTarget -> baseC`
     and same-lane `targetC` behavior without widening other branches.

2. `docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`
   - Responsibility: record the canonical bounded `I2` implementation result
     and the exact focused positive/negative evidence for the selected lane.

3. `orchestrator/rounds/round-055/implementation-notes.md`
   - Responsibility: summarize the bounded code/test/doc change and
     verification results for reviewer handoff.

### Evidence-Only, No Planned Edit

- `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
- `orchestrator/rounds/round-054/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`

These carry the accepted bounded selection and continuity context that `I2`
must preserve without rewriting.

Files that must remain untouched by `I2` `attempt-1`:

- `orchestrator/rounds/round-055/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/roadmap.md`
- `orchestrator/rounds/round-055/selection.md`
- `/Volumes/src/mlf4/Bugs.md`
- `src/MLF/Elab/Inst.hs`
- `src/MLF/Elab/Run/ResultType/View.hs`
- `TODO.md`
- `implementation_notes.md`

If the implementer cannot keep the selected slice confined to `Fallback.hs`
and `PipelineSpec.hs`, stop and hand the issue back to review rather than
broadening into replay repair, `ResultType.View`, non-local fallback behavior,
`boundTarget` overlay work, or any cross-family change.

## Sequential Tasks

### Task 1 - Lock the bounded `I2` contract before production edits

- Treat this round as the direct implementation of the accepted `I1` bind, not
  a new target-selection round, not a verification gate, and not a
  roadmap/controller update.
- Preserve the live subject fixed to repaired `URI-R2-C1`.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary.
- Keep the selected work confined to:
  - the existing single-base candidate block at
    `Fallback.hs:361-408`;
  - the existing local-proof cluster at `Fallback.hs:521-534`; and
  - the target-selection block at `Fallback.hs:681-701`.
- Preserve the accepted retained-target families exactly as inherited context:
  `rootLocalSchemeAliasBaseLike`, `rootLocalMultiInst`,
  `rootLocalInstArgMultiBase`, and `boundVarTarget`.
- Do not add a new local single-base branch to `keepTargetFinal`; the selected
  `I2` lane is the `baseTarget -> baseC` path, not a fourth retained-target
  family.
- Leave `boundTarget`, `schemeBodyTarget`, the empty-candidate `baseTarget`
  fallback, the inst-arg-only fallback, replay reopen, and every non-local or
  broader widening path out of scope unless a tiny refactor is needed to make
  the selected local-single-base proof explicit without changing their
  behavior.

### Task 2 - TDD the selected local single-base lane in `PipelineSpec`

- Work only inside the existing
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in `test/PipelineSpec.hs`.
- Add one focused helper adjacent to the current bounded helper cluster
  (alongside `schemeAliasBaseLikeFallback`, `localMultiInstFallback`, and
  `localInstArgMultiBaseFallback`) that synthesizes the selected local
  single-base lane using the existing input-rewrite utilities:
  - preserve the root on the local `TypeRef` lane when requested;
  - make `rootBoundCandidates` collapse to exactly one base-like candidate
    for the selected root, preferably `Int` so the positive assertion can stay
    concrete and simple;
  - keep `rootHasMultiInst`, `instArgRootMultiBase`,
    `rootLocalSchemeAliasBaseLike`, and `boundVarTarget` from becoming the
    selected reason the case succeeds.
- Add exactly one new bounded same-lane local single-base success example that:
  - exercises pipeline behavior for the selected local `baseTarget -> baseC`
    lane;
  - expects the concrete selected base result, not `rootFinal` and not the
    quantified fail-closed shell; and
  - does not rely on retained-target authority from any accepted `rootLocal*`
    family.
- Add exactly one matched fail-closed non-local contrast where the same-looking
  wrapper leaves the admissible local `TypeRef` lane and must stay on the
  existing non-recursive fallback path.
- Refresh the existing source-guard example
  `uses the local-binding gate when deciding retained fallback targets` only as
  needed so it names the new local single-base proof, proves `targetC` still
  reaches `baseTarget` before retained-target and `schemeBodyTarget`
  fallbacks, and preserves the accepted retained-target families as inherited
  context only.
- Run the focused `ARI-C1` block and watch the new selected success example or
  refreshed source guard fail before touching `Fallback.hs`.

### Task 3 - Land the minimal `Fallback.hs` proof for the selected lane

- Keep the current `rootBoundCandidates` / `baseTarget` machinery in
  `Fallback.hs:361-408` as the source of truth for the selected family.
- Introduce one explicit reviewer-auditable local proof adjacent to the
  existing `rootLocal*` cluster, for example a helper shaped like:
  `rootLocalSingleBase = rootBindingIsLocalType && singleton rootBoundCandidates && not rootHasMultiInst && not instArgRootMultiBase`.
  The exact spelling may differ, but the four required ingredients must remain
  obvious in the source.
- Use that proof to gate the selected `baseTarget` success path and its
  `targetC` consumption.
- Preserve the current meaning of `keepTargetFinal`; do not route the selected
  single-base lane through retained-target logic and do not add a fourth
  `rootLocal*` retained-target family.
- Preserve the current fail-closed guards around multi-inst, multi-base,
  non-local, empty-candidate, and inst-arg-only cases. If a branch not named
  in `I1` must be refactored for clarity, keep its behavior identical.
- Keep `boundTarget`, `boundVarTarget`, `schemeBodyTarget`,
  `rootLocalSchemeAliasBaseLike`, `rootLocalMultiInst`, and
  `rootLocalInstArgMultiBase` semantics unchanged.
- Prefer the smallest total Haskell change that satisfies the failing focused
  test while preserving all accepted predecessor behavior.

### Task 4 - Refresh the canonical `I2` artifact and round notes

- Write
  `docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`
  for `attempt-1`.
- Record:
  - the selected local single-base proof and the fact that the current
    `baseTarget` machinery remains the source of truth;
  - the exact positive same-lane success example;
  - the matched fail-closed non-local contrast;
  - the bounded ownership (`Fallback.hs` and `PipelineSpec.hs` only);
  - the continuity-only status of `BUG-2026-03-16-001`; and
  - the preserved exclusions on retained-child widening, `boundTarget`
    overlay work, `schemeBodyTarget` consolidation, replay reopen,
    `MLF.Elab.Inst`, `InstBot`, `ResultType.View`, non-local widening, and
    broader recursive inference.
- State explicitly in the artifact and round notes that `I2` did not add a new
  retained-target family; it hardened only the selected local
  `baseTarget -> baseC` lane and its same-lane `targetC` use.
- Record the same bounded summary and verification results in
  `orchestrator/rounds/round-055/implementation-notes.md`.

### Task 5 - Run the required bounded verification

- Re-run baseline contract checks from
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-055/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-055/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/retry-subloop.md`
- Re-run the focused bounded block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Add one focused anchor check proving the selected proof is explicit while the
  inherited families remain visible but unselected:
  - `rg -n 'rootLocalSingleBase|baseTarget|rootBoundCandidates|keepTargetFinal|targetC|rootLocalMultiInst|rootLocalInstArgMultiBase|rootLocalSchemeAliasBaseLike|boundVarTarget|schemeBodyTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
- Because this round edits `src/` and `test/`, the full repo gate is mandatory:
  - `cabal build all && cabal test`
- Reconfirm the diff stays bounded to:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
  - `docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`
  - `orchestrator/rounds/round-055/implementation-notes.md`

### Task 6 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can lawfully emit one of the allowed `I2` combinations:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are supported by the artifact and the checks:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve the immutability rule: if `I2` later retries, this `attempt-1`
  evidence set remains unchanged and later attempts are additive.

## Non-Goals

- No change to the accepted `I1` bind itself.
- No change to the accepted `C`, `E`, `F`, `G`, or `H` lanes except as
  inherited context.
- No extension of `keepTargetFinal` with a local single-base branch.
- No replay reopen, `MLF.Elab.Inst`, or `InstBot` work.
- No `boundVarTarget` widening, `boundTarget` overlay materialization,
  `schemeBodyTarget` consolidation, or `ResultType.View` edits.
- No non-local widening, cross-family widening, multi-SCC support,
  equi-recursive reasoning, implicit unfolding, cyclic structural graph
  encoding, compatibility shims, convenience fallbacks, or default-on
  widening.
- No edits to `orchestrator/rounds/round-055/state-snapshot.json`, `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/roadmap.md`,
  `orchestrator/rounds/round-055/selection.md`, `/Volumes/src/mlf4/Bugs.md`,
  `src-public/`, `app/`, or `mlf2.cabal`.
- No preemption of `I3` or `I4` beyond landing the selected `I2` slice and its
  bounded evidence.

## Reviewer Checks

Baseline checks from
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/verification.md`
still apply.

Round-specific checks:

1. `plan.md`, the produced `I2` artifact, and the round notes explicitly name
   `attempt-1` with `retry: null`.
2. `Fallback.hs` keeps `I2` bounded to the accepted `I1` local single-base
   slice: the diff stays inside the existing base-candidate / local-proof /
   target-selection blocks, the selected proof is visibly derived from
   `rootBindingIsLocalType`, singleton `rootBoundCandidates`,
   `not rootHasMultiInst`, and `not instArgRootMultiBase`, and no new
   single-base branch is added to `keepTargetFinal`.
3. `Fallback.hs` preserves fail-closed behavior for non-local, multi-inst,
   multi-base, retained-child, and other non-selected families, and does not
   widen `boundTarget`, `schemeBodyTarget`, or the empty-candidate /
   inst-arg-only branches.
4. `PipelineSpec.hs` adds exactly one new bounded same-lane local single-base
   success example plus one matched fail-closed non-local contrast inside the
   existing `ARI-C1` block, and refreshes the source guard only enough to name
   the selected proof and `targetC` ordering.
5. The canonical `I2` artifact and round notes explicitly record the selected
   local single-base proof, the bounded positive/negative evidence, the `I1`
   continuity chain, the continuity-only status of `BUG-2026-03-16-001`, and
   the preserved exclusions on retained-child widening, `boundTarget`,
   `schemeBodyTarget`, replay reopen, `MLF.Elab.Inst`, `InstBot`,
   `ResultType.View`, and non-local widening.
6. The focused `ARI-C1` block passes, the full repo gate passes, and the diff
   stays bounded to the four owned files only.
