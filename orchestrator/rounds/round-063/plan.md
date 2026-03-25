# Round 063 Plan (`K2` Empty-Candidate Local Scheme-Alias/Base-Like Hardening)

## Objective

Execute only roadmap item `K2` for `attempt-1` with `retry: null`.

This is the initial full `K2` plan, not a retry delta. The round must land
one bounded production-and-test slice inside repaired `URI-R2-C1` only: the
local-binding empty-candidate / no-inst-arg scheme-alias / base-like
`baseTarget -> baseC` lane in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
and the matching focused coverage in
`test/PipelineSpec.hs`.

The inherited boundary remains fixed:

- explicit-only
- non-equi-recursive
- non-cyclic-graph
- no-second-interface
- no-fallback

The selected `K2` implementation intent is:

- keep the existing empty-candidate / no-inst-arg `baseTarget` branch at
  `Fallback.hs:377-381` as the source of truth for this slice rather than
  inventing a second `baseTarget` family;
- add one explicit local proof in the existing local-proof cluster near
  `Fallback.hs:531-545`, using exactly
  `rootBindingIsLocalType`,
  `rootIsSchemeAlias`,
  `rootBoundIsBaseLike`,
  `IntSet.null rootBoundCandidates`,
  `IntSet.null instArgBaseBounds`,
  `not rootHasMultiInst`, and
  `not instArgRootMultiBase`;
- use that proof only for the same-lane `targetC` consumption in the existing
  target-selection block near `Fallback.hs:692-700`;
- extend only the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `PipelineSpec.hs` with one local success example, one matched local
  continuity contrast, and the minimal source-guard refresh needed to name the
  selected proof and its same-lane `targetC` use; and
- preserve the accepted `F2` / `F3` scheme-alias / base-like `rootFinal`
  lane, the completed `rootLocalSingleBase` lane, and the completed
  `rootLocalInstArgSingleBase` lane as inherited continuity only, not as
  reopened `K2` families.

Treat accepted negatives `U2 = authority-narrowed`,
`U3 = uniqueness-owner-stable-refuted`, and
`U4 = constructor-acyclic-termination-refuted` as binding evidence, not
clearance for replay reopen, constructor widening, or any broader recursive
inference family.

## Locked Round Context

- Round id: `round-063`
- Roadmap item: `K2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed file ownership for this slice only:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`

Accepted carry-forward facts that remain binding throughout `K2`:

- `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md` is the controlling
  accepted predecessor artifact. It freezes exactly one future `K2` slice:
  the local-binding empty-candidate / no-inst-arg scheme-alias / base-like
  `baseTarget -> baseC` lane plus its same-lane `targetC` use, with future
  ownership limited to `Fallback.hs` and `PipelineSpec.hs`.
- The accepted `F2` / `F3` packet already consumed the local
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  continuity lane. `K2` must not rewrite that accepted local `rootFinal`
  authority into a second live implementation family.
- The completed `rootLocalSingleBase` and `rootLocalInstArgSingleBase` lanes
  are predecessor continuity only. `K2` must not collapse the new
  empty-candidate lane into either completed singleton-base family.
- `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, replay reopen, `MLF.Elab.Inst`,
  `InstBot`, non-local widening, and any broader trigger family remain out of
  scope.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `## Open`
  section is empty and does not authorize widening.

Current source and test anchors already expose the exact selected gap:

- `Fallback.hs:377-381` keeps the empty-candidate / no-inst-arg
  `baseTarget` branch bounded to the existing `Just baseC` selection.
- `Fallback.hs:531-545` names the completed local proof cluster, but does not
  yet isolate the selected empty-candidate / no-inst-arg
  scheme-alias / base-like lane as its own reviewer-auditable local proof.
- `Fallback.hs:692-700` still routes the broader scheme-alias / base-like
  `baseTarget` consumer without a dedicated local proof for the selected lane.
- `PipelineSpec.hs:1244-1269` already contains the local
  scheme-alias / base-like helper family, which should remain the wrapper
  reference point rather than spawning a new unrelated family.
- `PipelineSpec.hs:1560-1708` already contains the accepted local continuity
  examples and source-guard checks, which `K2` must extend minimally rather
  than fork into a new block.

Current repository state is already non-pristine:

- `M orchestrator/rounds/round-063/state-snapshot.json`
- `?? orchestrator/rounds/round-063/`

Respect those existing changes. Do not revert, rewrite, or clean up unrelated
work while landing this slice.

## File Map

### Modify / Test

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
   - Responsibility: keep the existing empty-candidate / no-inst-arg
     `baseTarget` branch as the source of truth, add one named local proof
     built from the frozen `K1` ingredients, and make `targetC` consume only
     that proof on the same lane without widening adjacent continuity
     families.

2. `test/PipelineSpec.hs`
   - Responsibility: extend only the existing
     `ARI-C1 feasibility characterization (bounded prototype-only)` block with
     one local success example, one matched local continuity contrast in the
     same wrapper family, and the minimal source-guard additions naming the
     new proof and same-lane `targetC` use.

### Preserve Unchanged

- `orchestrator/rounds/round-063/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/roadmap.md`
- `orchestrator/rounds/round-063/selection.md`
- `/Volumes/src/mlf4/Bugs.md`
- `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
- every file outside the two `K1`-frozen ownership targets above

## Sequential Tasks

### Task 1 - Reconfirm the exact `K1`-frozen slice before editing

- Re-read the accepted `K1` bind and keep the implementation inside the exact
  slice it froze:
  - `Fallback.hs:377-381` for the selected empty-candidate / no-inst-arg
    `baseTarget` branch
  - `Fallback.hs:531-545` for the local-proof cluster
  - `Fallback.hs:692-700` for the same-lane `targetC` use
  - `PipelineSpec.hs:1244-1708` for the existing helper, examples, and source
    guards
- Preserve as inherited-only context:
  - accepted `F2` / `F3` `rootLocalSchemeAliasBaseLike` / `rootFinal`
    continuity
  - completed `rootLocalSingleBase`
  - completed `rootLocalInstArgSingleBase`
- Treat `U2` / `U3` / `U4` as binding negatives. Do not reinterpret them as
  clearance for replay reopen, constructor widening, or broader recursive
  inference.
- Keep `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `ResultType.View`, replay paths, and every non-local or cross-family path
  explicitly out of scope.

### Task 2 - TDD the selected local empty-candidate lane in `PipelineSpec.hs`

- Work only inside the existing
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block.
- Add one helper adjacent to the current helper cluster around
  `PipelineSpec.hs:1244-1425`. A concrete name such as
  `localEmptyCandidateSchemeAliasBaseLikeFallback` is appropriate.
- Keep that helper in the same wrapper family as
  `schemeAliasBaseLikeFallback`; do not introduce a new expression family or
  new general-purpose utilities.
- On the selected success path, make the helper keep all of the following true
  at once:
  - the root stays on the local `TypeRef` lane
  - `rootIsSchemeAlias`
  - `rootBoundIsBaseLike`
  - `IntSet.null rootBoundCandidates`
  - `IntSet.null instArgBaseBounds`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`
- Add one local success example in the existing block with a concrete name
  such as
  `"keeps local empty-candidate scheme-alias/base-like fallback on the local TypeRef lane"`
  and expect the concrete selected base result
  `TBase (BaseTy "Int")`.
- Add one matched local contrast in the same wrapper family by flipping
  exactly one selected-lane precondition while keeping the wrapper local and
  scheme-alias / base-like. Prefer tightening the existing local
  `schemeAliasBaseLikeFallback True` example into this continuity role if that
  avoids a second helper family.
- The matched local contrast must stay on already-accepted continuity rather
  than the new `K2` proof. It must not rely on non-local fail-closed behavior
  or reopen the completed singleton-base families as live `K2` scope.
- Run the focused `ARI-C1` block after landing the new success example and
  source-guard expectation, and watch the selected success path fail for the
  intended reason before touching `Fallback.hs`.

### Task 3 - Land the minimal `Fallback.hs` proof and same-lane `targetC` use

- Keep the current empty-candidate / no-inst-arg `baseTarget` branch at
  `Fallback.hs:377-381` as the source of truth. Do not add a second
  `baseTarget` family or broaden the selected condition.
- Add one explicit reviewer-auditable local proof adjacent to the existing
  `rootLocal*` cluster. A concrete name such as
  `rootLocalEmptyCandidateSchemeAliasBaseLike` is appropriate.
- Build that proof from exactly these ingredients:
  - `rootBindingIsLocalType`
  - `rootIsSchemeAlias`
  - `rootBoundIsBaseLike`
  - `IntSet.null rootBoundCandidates`
  - `IntSet.null instArgBaseBounds`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`
- Update `targetC` only enough to consume the selected lane through a dedicated
  same-lane arm of the existing `case baseTarget of` block, for example
  `Just baseC | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC`.
- Place that new local arm before the broader
  `rootIsSchemeAlias && rootBoundIsBaseLike` arm so the selected proof is
  explicit and reviewer-auditable.
- Preserve the completed `rootLocalSingleBase` and
  `rootLocalInstArgSingleBase` arms exactly as inherited continuity.
- Preserve `keepTargetFinal` as continuity-only retained-target logic. Do not
  add the new proof to `keepTargetFinal`, do not rewrite
  `rootLocalSchemeAliasBaseLike`, and do not change `boundVarTarget`,
  `boundTarget`, or `schemeBodyTarget` semantics.

### Task 4 - Refresh the existing source guards only enough to name the lane

- Extend the existing source-guard example near `PipelineSpec.hs:1660-1708`
  instead of adding a second guard family.
- Add a guard proving the new proof is defined from the exact seven frozen
  ingredients above.
- Add a guard proving `targetC` has a dedicated same-lane
  `Just baseC | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC` arm.
- Keep the guard proving the completed `rootLocalSingleBase` and
  `rootLocalInstArgSingleBase` arms intact except for the minimum text needed
  to accommodate the new arm.
- Keep the broader scheme-alias / base-like and retained-target guards intact
  so reviewers can still see that:
  - `rootLocalSchemeAliasBaseLike` remains continuity-only
  - `keepTargetFinal` still names only the accepted retained-target families
  - the broader `rootIsSchemeAlias && rootBoundIsBaseLike` route still exists
    after the new dedicated local proof

## Verification Plan

Run the baseline contract checks required by
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-063/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-063/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/retry-subloop.md`

Run the focused `K2` checks:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- `rg -n 'rootLocalEmptyCandidateSchemeAliasBaseLike|rootLocalSingleBase|rootLocalInstArgSingleBase|rootLocalSchemeAliasBaseLike|rootIsSchemeAlias && rootBoundIsBaseLike|targetC|baseTarget|keepTargetFinal' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
- `git diff --name-only -- src test app src-public mlf2.cabal`
  - expected changed code paths: only
    `src/MLF/Elab/Run/ResultType/Fallback.hs` and
    `test/PipelineSpec.hs`

Because this round edits `src/` and `test/`, the full repo gate is mandatory:

- `cabal build all && cabal test`

## Non-Goals

- No edit to `orchestrator/rounds/round-063/state-snapshot.json`, `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/roadmap.md`,
  `orchestrator/rounds/round-063/selection.md`, or `/Volumes/src/mlf4/Bugs.md`.
- No edit outside `src/MLF/Elab/Run/ResultType/Fallback.hs` and
  `test/PipelineSpec.hs`.
- No reopening of the accepted `F2` / `F3` scheme-alias / base-like
  `rootFinal` lane as live `K2` work.
- No reopening of the completed `rootLocalSingleBase` or
  `rootLocalInstArgSingleBase` lanes as alternate `K2` targets.
- No replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`, or `ResultType.View` work.
- No non-local widening, cross-family widening, multi-SCC support,
  equi-recursive reasoning, implicit unfolding, cyclic structural graph
  encoding, compatibility shims, convenience fallbacks, or default-on
  widening.
- No reinterpretation of accepted negatives `U2` / `U3` / `U4` as if they had
  already been cleared.

## Retry Discipline

- This plan is the full `attempt-1` plan for `K2` with `retry: null`.
- Allowed later review outcomes remain:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- If `round-063` returns to `plan` with an active retry object, revise this
  same `plan.md` only as a delta for the recorded `fix_hypothesis`. Do not
  replan the whole stage, widen file ownership, or swap to a different target
  family.
- Keep the accepted `K1` freeze, the accepted `F2` / `F3` continuity lane,
  the completed singleton-base lanes, and all prior review history immutable
  across retries.

## Reviewer Checks

Baseline checks from
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/verification.md`
still apply.

Round-specific checks:

1. `plan.md` names `attempt-1` with `retry: null` and keeps `K2` bounded to
   `Fallback.hs` and `PipelineSpec.hs` only.
2. `Fallback.hs` stays inside the selected empty-candidate / no-inst-arg
   `baseTarget` branch, local-proof cluster, and same-lane `targetC` block;
   the new proof is visibly derived from the exact seven frozen ingredients;
   and the accepted `F2` / `F3`, `rootLocalSingleBase`, and
   `rootLocalInstArgSingleBase` lanes remain continuity only.
3. `PipelineSpec.hs` extends only the existing `ARI-C1` block with one local
   success example, one matched local continuity contrast in the same wrapper
   family, and the minimal source-guard refresh naming the new proof and
   same-lane `targetC` use.
4. `keepTargetFinal` remains continuity-only retained-target logic; the new
   proof is not added as a retained-target family, and `boundVarTarget`,
   `boundTarget`, `schemeBodyTarget`, replay paths, and non-local widening
   remain untouched.
5. Accepted negatives `U2` / `U3` / `U4` remain binding, and the round stays
   inside repaired `URI-R2-C1` under the inherited
   `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
   boundary.
6. The focused `ARI-C1` block passes, the full `cabal build all && cabal test`
   gate passes, and the code diff stays bounded to the two owned files only.
