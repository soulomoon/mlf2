# Round 059 Plan (`J2` Local Inst-Arg-Only Singleton-Base Hardening)

## Objective

Execute only roadmap item `J2` for `attempt-1` with `retry: null`.

This is the initial full `J2` plan, not a retry delta. The round must land
one bounded production-and-test slice inside repaired `URI-R2-C1` only:
the local-binding inst-arg-only singleton-base `baseTarget -> baseC` lane in
`src/MLF/Elab/Run/ResultType/Fallback.hs` and the matching focused coverage in
`test/PipelineSpec.hs`.

The inherited boundary remains fixed:

- explicit-only
- non-equi-recursive
- non-cyclic-graph
- no-second-interface
- no-fallback

Do not widen into replay reopen, `MLF.Elab.Inst`, `InstBot`,
`boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
`src/MLF/Elab/Run/ResultType/View.hs`, non-local widening, cross-family
search, or any second implementation family.

## Locked Round Context

- Round id: `round-059`
- Roadmap item: `J2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed file ownership for this slice only:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`

Pre-existing workspace state is not pristine:

- `M orchestrator/rounds/round-059/state-snapshot.json`
- `?? orchestrator/rounds/round-059/`

Do not revert, rewrite, or clean up anyone else's changes while landing this
slice.

## File Map

### Modify

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - Responsibility: harden only the existing local-binding inst-arg-only
    singleton-base `baseTarget -> baseC` branch at `Fallback.hs:382-387`,
    define the matching local proof in the proof cluster near
    `Fallback.hs:525-539`, and make `targetC` consume that one proof in the
    same-lane `baseTarget` path near `Fallback.hs:686-710`.
- `test/PipelineSpec.hs`
  - Responsibility: add one focused helper for the selected lane, one local
    positive example, one matched non-local fail-closed contrast, and the
    minimal source-guard additions proving the new proof/`targetC` path exists
    without widening adjacent families.

### Preserve Unchanged

- `orchestrator/rounds/round-059/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-026/roadmap.md`
- `/Volumes/src/mlf4/Bugs.md`
- every file outside the two `J1`-frozen ownership targets above

## Sequential Tasks

### Task 1 - Reconfirm the exact `J1`-frozen slice before editing

- Re-read the accepted `J1` bind and keep the implementation inside the exact
  lane it froze:
  - `baseTarget` branch at `Fallback.hs:382-387`
  - same-lane `targetC` consumption at `Fallback.hs:686-710`
- Preserve as inherited-only context:
  - completed `rootLocalSingleBase`
  - preserved scheme-alias/base-like `baseTarget` route
  - `rootLocalMultiInst`
  - `rootLocalInstArgMultiBase`
  - retained-target / `keepTargetFinal` behavior
- Do not collapse this lane into `rootLocalSingleBase`. The selected `J2`
  slice is distinct because it is driven by `instArgBaseBounds`, not the
  completed `rootBoundCandidates`-based single-base path.

### Task 2 - Harden only the selected proof in `Fallback.hs`

- Keep `baseTarget` selection bounded to the existing `Fallback.hs:382-387`
  branch. Do not add any new `baseTarget` family, extra search, or non-local
  authority.
- Add one dedicated local proof in the proof cluster, adjacent to the existing
  `rootLocal*` booleans. A concrete name such as
  `rootLocalInstArgSingleBase` is appropriate.
- That proof must use exactly these ingredients:
  - `rootBindingIsLocalType`
  - `IntSet.null rootBaseBounds`
  - `IntSet.size instArgBaseBounds == 1`
  - `not rootHasMultiInst`
  - `not instArgRootMultiBase`
- Keep the existing `IntSet.member (getNodeId baseC) instArgBaseBounds`
  check inside the `baseTarget` branch as the only witness that the chosen
  `baseC` is the singleton inst-argument candidate.
- Update `targetC` only by teaching the existing `case baseTarget of`
  block to consume the new local proof and return `baseC` on that same lane.
  The new arm should sit alongside the existing `rootLocalSingleBase` and
  scheme-alias/base-like `baseTarget` arms; it must not alter the
  `keepTargetFinal` path or the non-local fallback path.
- Do not touch any other family in the proof cluster or target-selection
  logic unless a tiny textual adjustment is required to thread this one proof.

### Task 3 - Add the focused helper and examples in `PipelineSpec.hs`

- Add one new helper next to the existing helper cluster around
  `PipelineSpec.hs:1243-1373`. Use a concrete name consistent with the
  neighboring helpers, such as `localInstArgSingleBaseFallback`.
- Build that helper from the same local wrapper shape already used for the
  adjacent singleton/multi-base helpers:
  - preserve one application/root shape;
  - keep the root on the local `TypeRef` lane for the positive case;
  - ensure `rootBaseBounds` stays empty;
  - inject exactly one inst-argument base witness for `Int`;
  - keep `rootHasMultiInst == False`;
  - keep `instArgRootMultiBase == False`.
- Reuse the existing local test plumbing where possible
  (`findIntBaseNode`, `rewriteResultTypeInputs`, `rewriteReferencedTrace`,
  `nextFreshNodeIds`, `insertTyNodes`, existing wrapper extraction helpers).
  Do not extract general-purpose utilities or widen the helper surface.
- Add one local positive example in the `ARI-C1 feasibility characterization
  (bounded prototype-only)` block:
  - example name should clearly identify the exact lane, e.g.
    `"keeps local inst-arg-only singleton-base fallback on the local TypeRef lane"`
  - expected result: `TBase (BaseTy "Int")`
- Add one matched non-local contrast using the same helper/wrapper but with
  the root moved off the local `TypeRef` lane:
  - example name should mirror the positive case, e.g.
    `"keeps the same inst-arg-only singleton-base wrapper fail-closed once it leaves the local TypeRef lane"`
  - expected result: the inherited quantified fail-closed shell
    `TForall _ Nothing (TVar _)`, not the concrete `Int` base result

### Task 4 - Extend the focused source guards only enough to prove this lane

- Update the existing source-guard assertions in the same `PipelineSpec.hs`
  block instead of creating a new family of guard tests.
- Add a guard that proves the new proof predicate is defined from the exact
  five ingredients above, using `rootBaseBounds` and `instArgBaseBounds`
  directly rather than `rootBoundCandidates`.
- Add a guard that proves `targetC` has a dedicated
  `Just baseC | <new-proof> -> baseC` arm for this lane.
- Keep the existing source guards for:
  - `rootLocalSingleBase`
  - `rootLocalSchemeAliasBaseLike`
  - `rootLocalMultiInst`
  - `rootLocalInstArgMultiBase`
  - retained-child / `keepTargetFinal`
  unchanged except for the minimum text needed to accommodate the new arm.

### Task 5 - Verify the bounded slice and the file boundary

- Run `git diff --check`.
- Run the focused block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Confirm the production/test diff stays inside the frozen ownership set by
  inspecting:
  - `git diff --name-only -- src test app src-public mlf2.cabal`
  - expected changed code paths: only
    `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`
- Because this round touches `src/` and `test/`, finish with the full gate:
  - `cabal build all && cabal test`

## Completion Criteria

This plan is satisfied only if all of the following are true:

1. The implementation edits only `Fallback.hs` and `PipelineSpec.hs`.
2. `Fallback.hs` gains exactly one dedicated local inst-arg-only singleton-base
   proof built from the five frozen ingredients above.
3. `targetC` consumes that proof through the existing `baseTarget -> baseC`
   path without changing preserved scheme-alias/base-like, completed
   `rootLocalSingleBase`, retained-target, or non-local fallback behavior.
4. `PipelineSpec.hs` gains exactly one helper, one local positive example, one
   matched non-local contrast, and the minimal source-guard additions for this
   same lane.
5. The focused ARI-C1 block passes, and the full `cabal build all && cabal test`
   gate passes.

## Non-Authorization

This `J2` plan does not authorize:

- edits to `orchestrator/rounds/round-059/state-snapshot.json`, `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-026/roadmap.md`, or
  `/Volumes/src/mlf4/Bugs.md`
- any new module, API, executable entrypoint, compatibility shim, or fallback
  path
- any replay-path, `InstBot`, `boundVarTarget`, `boundTarget`,
  `schemeBodyTarget`, `View.hs`, or non-local widening work
- reopening accepted predecessor families or broadening the live subject past
  repaired `URI-R2-C1`
