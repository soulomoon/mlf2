# Round 072 Plan (`N5` Minimal Slice For The Exact `N4`-Frozen Non-Local `baseTarget -> baseC` Packet)

## Objective

Execute only roadmap item `N5` and land one smallest safe code-changing slice
for the already accepted `N4` packet, with the canonical round artifact at:
`docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`.

This is the initial `N5` plan for `attempt-1` with `retry: null`. The round
must treat accepted `L1`, `L2`, `N1`, `N2`, `N3`, and `N4` as binding
predecessor continuity, stay inside the exact preserved non-local generic
scheme-root alias-bound / base-like `baseTarget -> baseC` packet frozen by
`N4`, and avoid reopening any blocked route or authorizing `N6` / `N7`.

Current planning read: the smallest safe `N5` progress is not a second docs
restatement. The frozen packet already exists in production code and already
has exact positive/contrast fixtures in `PipelineSpec`. The bounded `N5`
implementation should therefore make that one non-local packet
reviewer-auditable by naming it explicitly in
`src/MLF/Elab/Run/ResultType/Fallback.hs` and routing the existing same-lane
`targetC` consumer through that explicit proof only, while keeping the
existing `baseTarget` computation unchanged and preserving the accepted local
`rootFinal` and local empty-candidate lanes unchanged.

## Locked Round Context

- Round id: `round-072`
- Roadmap item: `N5`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: exact `N4`-frozen preserved non-local generic
  scheme-root alias-bound / base-like `baseTarget -> baseC` packet
- Active branch: `codex/round-072-n5-minimal-slice`
- Active worktree:
  `.worktrees/round-072`
- Stage mode: one bounded code-changing hardening slice only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `?? orchestrator/rounds/round-072/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-072/selection.md`
  already fixes this round to roadmap item `N5` only and forbids using this
  round for `N6`, `N7`, replay relitigation, roadmap/state edits, bug-tracker
  edits, or predecessor-history rewrites.
- `orchestrator/rounds/round-072/state-snapshot.json` fixes the
  live controller state at `active_round_id: "round-072"`, `stage: "plan"`,
  `current_task: "N5"`, `branch:
  "codex/round-072-n5-minimal-slice"`, and `retry: null`.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/roadmap.md` makes
  `N5` the first pending item after accepted `N4`; every later item depends on
  an accepted `N5` slice and therefore cannot run first.
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  still records `N5 = NO`, so this round must land exactly one smallest safe
  slice and not widen beyond it.
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
  remains binding predecessor evidence that accepted `N1` reopened only a
  planning lane and preserved the inherited explicit-only /
  non-equi-recursive / non-cyclic-graph / no-second-interface /
  no-fallback boundary unchanged.
- `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
  remains binding predecessor evidence that accepted `N2` selected exactly one
  live subject: the preserved generic scheme-alias / base-like `baseTarget`
  route only.
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
  remains binding predecessor evidence that any lawful slice must preserve the
  exact owner-binder / owned-bound pair, inverse-translation-safe
  bound-inlining story, and structural / variance binding-flag reconstruction
  story without reopening replay, `InstBot`, or broader recursive inference.
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
  remains binding predecessor evidence that the only frozen packet is the
  preserved non-local generic scheme-root alias-bound / base-like
  `baseTarget -> baseC` packet in `Fallback.hs`, limited to the existing
  generic `baseTarget` computation, the downstream same-lane generic
  `targetC` consumer, and the non-local
  `schemeAliasBaseLikeFallback False` regression anchor.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/retry-subloop.md`
  allows retries for `N5`, but prior attempts remain immutable. This
  `attempt-1` plan must therefore create one new bounded slice without
  rewriting any prior attempt or review artifact.
- `Bugs.md` still carries open
  `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains read-only
  predecessor context only and does not authorize replay reopen,
  `MLF.Elab.Inst`, or any different subject in this round.

## File Map

### Modify

- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
  - Responsibility: record the bounded `N5` implementation slice, its exact
    scope, focused evidence, and verification notes without claiming `N6`
    clearance.
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - Responsibility: keep the frozen `baseTarget` computation unchanged, add
    one explicit non-local proof for the selected packet, and route the
    selected same-lane `targetC` consumer through that proof only.
- `test/PipelineSpec.hs`
  - Responsibility: keep the exact non-local selected packet and the preserved
    local continuity visible through focused behavior and source-guard checks.

### Read-Only Evidence

- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/roadmap.md`
- `orchestrator/rounds/round-072/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/verification.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/retry-subloop.md`
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
- `Bugs.md`
- `orchestrator/rounds/round-072/selection.md`

### Preserve Unchanged

- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/roadmap.md`
- `orchestrator/rounds/round-072/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/verification.md`
- `orchestrator/rounds/round-072/selection.md`
- `Bugs.md`
- `src-public/`
- `app/`
- `mlf2.cabal`

## Exact Selected `N5` Slice (Exactly One)

The only selected `N5` slice is:

make the accepted non-local generic scheme-root alias-bound / base-like
`baseTarget -> baseC` packet reviewer-auditable in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
by introducing one explicit non-local proof for that packet and routing the
existing same-lane `targetC` consumer through that proof only, while keeping
the existing generic `baseTarget` computation unchanged and preserving the
accepted local `rootFinal` and local empty-candidate lanes unchanged.

Required interpretation of that one bounded slice:

- the explicit proof should remain confined to the existing frozen packet
  ingredients only, namely:
  `not rootBindingIsLocalType`,
  `rootIsSchemeAlias`,
  `rootBoundIsBaseLike`,
  plus the already-frozen `baseTarget` computation that has already selected
  `baseC`;
- the production edit must not refine, widen, or otherwise change the
  accepted `baseTarget` computation itself;
- the selected same-lane consumer should become reviewer-auditable as a
  dedicated non-local arm such as
  `Just baseC | rootNonLocalSchemeAliasBaseLike -> baseC`;
- the accepted local empty-candidate
  `rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC` lane must remain
  earlier and unchanged; and
- the accepted local scheme-alias/base-like continuity lane exercised by
  `schemeAliasBaseLikeFallback True` must remain preserved continuity only and
  must not be rebound as a second `N5` family.

Everything else remains out of scope, including:

- repaired `URI-R2-C1` as live work;
- replay reopen;
- `MLF.Elab.Inst` and `InstBot`;
- `boundVarTarget`, `boundTarget`, and `schemeBodyTarget`;
- `src/MLF/Elab/Run/ResultType/View.hs`;
- every other fallback family;
- every different solver/pipeline subject;
- cross-family search;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding, graph-cycle exceptions, or multi-SCC
  support; and
- any second interface, compatibility shim, convenience fallback, or default
  path widening.

## Sequential Tasks

### Task 1 - Re-establish exact `N5` authority and bounded code-changing scope

- Write the future canonical `N5` artifact as `attempt-1` with `retry: null`.
- State explicitly that `N5` implements only one bounded slice inside accepted
  `N4` and does not authorize `N6`, `N7`, roadmap mutation,
  controller-state edits, bug-tracker edits, or predecessor-history rewrites.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unchanged.

### Task 2 - Record the bounded `N5` implementation artifact before touching code

- Create
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`.
- Record in that artifact:
  - accepted `L1` / `L2` / `N1` / `N2` / `N3` / `N4` continuity;
  - the exact frozen packet from accepted `N4`;
  - the fact that `N5` chooses one code-changing slice rather than a second
    docs-only restatement because the packet already exists in source and test
    anchors;
  - the exact source/test ownership for this round only; and
  - explicit exclusions for replay, `InstBot`, local-lane widening, and every
    non-selected route.

### Task 3 - Make the non-local packet explicit in `Fallback.hs`

- In
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  keep the frozen `baseTarget` computation at the current `N4` packet
  unchanged.
- Add exactly one explicit proof for the selected packet, with a bounded name
  such as `rootNonLocalSchemeAliasBaseLike`, derived only from:
  - `not rootBindingIsLocalType`;
  - `rootIsSchemeAlias`; and
  - `rootBoundIsBaseLike`.
- Replace the generic same-lane consumer
  `Just baseC | rootIsSchemeAlias && rootBoundIsBaseLike -> baseC`
  with the explicit non-local proof arm only.
- Keep these adjacent lanes unchanged:
  - `rootLocalSingleBase`;
  - `rootLocalInstArgSingleBase`;
  - `rootLocalEmptyCandidateSchemeAliasBaseLike`;
  - `keepTargetFinal`;
  - the preserved local scheme-alias/base-like continuity lane; and
  - all non-selected fallbacks.

### Task 4 - Refresh focused tests in `PipelineSpec.hs` without widening the harness

- Limit test work to the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `test/PipelineSpec.hs`.
- Keep the existing helper family and reuse the accepted
  `schemeAliasBaseLikeFallback` wrapper for the selected packet.
- Preserve and, if necessary, rename only enough focused examples to make the
  selected packet and preserved local contrast reviewer-visible:
  - non-local selected-packet success:
    `schemeAliasBaseLikeFallback False` still returns `TBase (BaseTy "Int")`
    with `containsMu False`;
  - preserved local continuity contrast:
    `schemeAliasBaseLikeFallback True` still stays on the accepted quantified
    local continuity lane; and
  - refreshed source guard:
    require the explicit non-local proof name and the dedicated same-lane
    `targetC` arm, while also proving the accepted local empty-candidate arm
    remains present and earlier.

### Task 5 - Use focused TDD evidence before and after the bounded source change

- First run the focused spec command after updating the source-guard
  expectations but before the production edit:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Expected red signal:
  - the refreshed source-guard assertion fails because `Fallback.hs` does not
    yet name the explicit non-local proof or the dedicated non-local
    `targetC` arm.
- After the bounded production edit, rerun the same focused command and
  require it to pass.
- If the focused run shows any behavior regression outside the selected
  non-local packet or the preserved local continuity contrast, stop and revise
  within the same files only; do not widen into another route.

### Task 6 - Run the required bounded verification commands

Run the baseline checks required by
`orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-072/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-072/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/roadmap.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `test -f orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/retry-subloop.md`
- focused `ARI-C1` command above

Because this round touches `src/` and `test/`, also run the full repo gate:

- `cabal build all && cabal test`

### Task 7 - Keep non-authorization and retry immutability explicit

- In the `N5` artifact, state explicitly that this round does not authorize:
  - `N6` verification or `N7` closure;
  - replay reopen;
  - `MLF.Elab.Inst` or `InstBot`;
  - `boundVarTarget`, `boundTarget`, `schemeBodyTarget`, or `ResultType.View`
    work;
  - any local-lane reopen or cross-family widening; or
  - roadmap/state/bug-tracker edits.
- State explicitly that this is `attempt-1` and that any future retry must be
  additive and must not rewrite prior attempts or reviewer-owned history.
