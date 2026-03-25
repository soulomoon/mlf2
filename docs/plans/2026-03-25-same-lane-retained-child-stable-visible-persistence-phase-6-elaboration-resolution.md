# Same-Lane Retained-Child Stable-Visible-Persistence Phase-6 Elaboration Resolution

Date: 2026-03-25
Round: `round-091`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: exact Phase-6 elaboration resolution for the frozen same-lane
retained-child pocket
Artifact kind: canonical bounded item-3 resolution record

## Stage Contract Freeze

This artifact implements only roadmap item `3` for `attempt-1` with
`retry: null`.

The frozen tuple from item `1` and item `2` remains unchanged:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
- quantified-boundary status: clear-boundary only.

The exact frozen packet remains:

```haskell
ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
```

where
`recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

The exact item-2 breakpoint remains:

```text
Phase 6 (elaboration): PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]"]
```

The exact item-3 success criterion is unchanged:

- both `runPipelineElab` and `runPipelineElabChecked` must stop failing on the
  exact frozen packet at `Phase 6 (elaboration)`; and
- the repair must not change family, anchor, owner-local frame, route, or
  quantified-boundary status.

The exact fail-closed criterion is unchanged:

- if the current same-pocket authority still cannot lawfully produce an
  authoritative translation for edge `3`, this artifact must finalize as
  blocker proof only and stop.

Item `3` ends once that exact Phase-6 question is resolved one way or the
other. It does not classify item-4 persistence outcomes.

## Exact Edge-3 Authority Under Review

- edge id: `3`
- frozen expansion: `ExpInstantiate [NodeId 31]`
- frozen witness authority:
  `EdgeWitness { ewEdgeId = EdgeId 3, ewLeft = NodeId 39, ewRight = NodeId 39, ewRoot = NodeId 15, ewForallIntros = 0, ewWitness = InstanceWitness [] }`
- frozen trace authority:
  `EdgeTrace { etRoot = NodeId 15, etBinderArgs = [], ... }`

## Resolution Record

The exact frozen same-lane retained-child packet cleared the exact Phase-6
breakpoint.

Bounded root cause:

- `phiFromEdgeWitnessWithTrace` still produced placeholder authority for
  edge `3`:
  `InstApp (TVar "t32")`.
- The exact pocket already carried a canonical expansion for the same edge:
  `ExpInstantiate [NodeId 31]`.
- The previous `reifyInst` path therefore had enough exact-pocket authority
  recorded to refine the placeholder, but no local no-fallback translation
  path existed for consuming that exact expansion inside Phase 6.

Bounded repair that landed:

- `src/MLF/Elab/Legacy.hs` now exports
  `expInstantiateArgsToInstNoFallback`, a `PresolutionView`-native helper
  that translates exact `ExpInstantiate` arguments into an elaboration
  instantiation without fallback behavior.
- The helper reifies each exact expansion argument through
  `reifyTypeWithNamedSetNoFallback`, first honoring any exact
  base-bound authority available through
  `resolveBaseBoundForInstConstraint`, then reuses the existing local
  `instAppsFromTypes` builder.
- `src/MLF/Elab/Elaborate/Annotation.hs` now consumes that helper only in the
  bounded case where:
  - the edge already has `ExpInstantiate args`;
  - inferred authoritative args and trace binder args are both absent;
  - the current `phi` still needs authoritative refinement because it carries
    placeholder application variables; and
  - the scheme still has positive arity.
- If those exact conditions are not met, `reifyInst` remains fail-closed and
  still raises the existing `PhiTranslatabilityError` instead of widening into
  neighboring routes or fallback-like recovery.

Exact bounded evidence added:

- `test/ElaborationSpec.hs` now freezes the exact edge-`3` pocket, proves that
  the canonical expansion remains `ExpInstantiate [NodeId 31]`, proves that
  the pre-repair `phi` is the placeholder `InstApp (TVar "t32")`, and then
  requires the exact packet to elaborate successfully.
- `test/PipelineSpec.hs` now freezes the exact public packet and requires both
  `runPipelineElab` and `runPipelineElabChecked` to pass the exact
  `Phase 6 (elaboration)` handoff.

Outcome:

- item `3` cleared the exact Phase-6 breakpoint for the frozen packet;
- the repair consumed only already-recorded same-pocket authority for
  edge `3`;
- no replay repair, `InstBot` widening, alias-bound-family carryover,
  neighboring-route search, nested-`forall` crossing, or fallback widening
  was introduced; and
- item `4` remains later work only.

## Verification Record

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - Result: passed. The exact edge-`3` regression proved the frozen expansion
    remained `ExpInstantiate [NodeId 31]`, the frozen pre-repair `phi`
    remained `InstApp (TVar "t32")`, and the exact packet elaborated.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - Result: passed. Both unchecked and checked pipeline entrypoints cleared
    `Phase 6 (elaboration)` on the exact frozen packet.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004 nested let + annotated lambda now fails fast"'`
  - Result: passed. The neighboring fail-fast sentinel stayed fail-closed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines"'`
  - Result: passed. The alias-bound-family regression sentinel stayed green
    without being widened into the current item.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - Result: passed with `21 examples, 0 failures`, including the exact frozen
    packet and the existing fail-closed contrast cases.
- `cabal build all && cabal test`
  - Result: passed with `1143 examples, 0 failures`.
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - Result: returned only the allowed bounded runtime/test files:
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `test/ElaborationSpec.hs`, and
    `test/PipelineSpec.hs`.
- `git diff --name-only -- orchestrator/state.json orchestrator/roadmap.md orchestrator/retry-subloop.md orchestrator/verification.md Bugs.md`
  - Result: reported pre-existing controller-owned drift on
    `orchestrator/state.json`, `orchestrator/roadmap.md`,
    `orchestrator/retry-subloop.md`, and `orchestrator/verification.md`.
    No round-owned edits landed on those preserved paths or on `Bugs.md`.
