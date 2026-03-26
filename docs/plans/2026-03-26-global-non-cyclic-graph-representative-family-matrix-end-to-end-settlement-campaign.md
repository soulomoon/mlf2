# Global Non-Cyclic-Graph Representative Family-Matrix End-To-End Settlement Campaign

Date: 2026-03-26
Round: `round-102`
Roadmap item: `item-4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded representative family-matrix settlement campaign
across the current automatic iso-recursive inference family
Artifact kind: canonical docs-only item-4 representative settlement-campaign
dossier

## Stage Contract Freeze

This artifact implements only roadmap item `4` for `attempt-1` with
`retry: null`.

This round is docs-first and evidence-first. It refreshes the representative
family matrix on the existing solver -> elaboration -> reconstruction ->
internal/public output surfaces and records one bounded settlement read
without consuming item `5`.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, roadmap contracts, retry contracts,
  verification contracts, or `Bugs.md`;
- item `5` settlement, architecture revision, or a
  `non-cyclic-graph = keep` /
  `reopen the non-cyclic-graph revision question` outcome;
- production implementation, hardening, or the final capability-claim gate;
- a new same-lane packet, a neighboring route, a new positive
  nested-`forall` packet, cyclic search, multi-SCC search, a second
  interface, or fallback widening; or
- reinterpretation of accepted predecessor truth from item `1`, item `2`,
  item `3`, or the exact same-lane predecessor chain.

The inherited boundary remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

The frozen row-level vocabulary remains:

- `stable visible persistence`;
- `admitted but not reconstruction-visible / blocker debt`; and
- `fail-closed rejection`.

## Representative Row Inventory

This round reuses the accepted March 25 representative row schema and adds one
explicit local-shape row so that every positive family `P1` through `P6` is
represented directly in the item-4 matrix.

| Row | Representative label | Primary family responsibility |
| --- | --- | --- |
| `P1-row` | local recursive-shape / local-lane support | `P1` |
| `C1` | non-local alias-bound / base-like admitted family | `P2` |
| `C2` | same-lane retained-child admitted family | `P3` |
| `C3` | nested-`forall` / quantified-crossing pressure | `P5`, `N2` |
| `C4` | ambiguity / competing-candidate pressure | `N1` |
| `C5` | binder-sensitive / owner-sensitive placement pressure using the same exact `C2` pocket only | `P4`, `N2` |
| `C6` | termination-pressure under the inherited acyclic model | `N6`, `N4`, `N5` |
| `C7` | reconstruction-heavy output-surface pressure using the same exact `C2` pocket only | `P6` |

`C5` and `C7` remain the same exact same-lane retained-child pocket already
accepted for `C2`. They are not new packets, not new routes, and not
alternate public interfaces.

## Accepted Continuity Reused Without Widening

The accepted predecessor chain remains binding and unchanged:

- the item-1 freeze in
  `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`;
- the accepted capability contract, architectural audit, full-pipeline
  validation contract, representative campaign, and architecture-decision
  record from March 25;
- the accepted item-2 settlement slice for `C1`, `C2`, and `C5`;
- the accepted item-3 settlement slice for `C3` and `C7`;
- the accepted exact same-lane predecessor chain through
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`; and
- accepted review finalization for `round-094` through `round-101`.

Fresh read-only reruns in this round do not contradict that chain.

## Fresh Read-Only Evidence

### `P1-row` local-shape contrast

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`
- `test/PipelineSpec.hs:1495` still keeps the bounded same-lane local
  retained-child fallback recursive when the local `TypeRef` lane is wired.
- `test/PipelineSpec.hs:1718` still asserts that the corresponding
  unannotated variant does not infer recursive shape (`containsMu False`).

Current read:
bounded local-shape support still exists inside the inherited acyclic model,
but current review-visible success remains annotation-anchored or otherwise
below the representative unannotated automatic-success bar.

### `C1` focused non-local reruns

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`
- `src/MLF/Elab/Run/ResultType/Fallback.hs:545` and
  `src/MLF/Elab/Run/ResultType/Fallback.hs:717` still keep the exact
  `rootNonLocalSchemeAliasBaseLike` proof and the `baseTarget -> baseC`
  selection path distinct from the preserved local lanes.

Current read:
the exact non-local packet remains admitted, but the accepted visible-output
fact is still `TBase (BaseTy "Int")` with `containsMu False`.

### `C2` / `C5` / `C7` exact same-lane reruns

The exact same-lane retained-child replay was rerun through
`cabal repl mlf2-test --repl-options=-ignore-dot-ghci` with
`:module + *PipelineSpec` and a helper block mirrored from the harness logic
anchored at `test/PipelineSpec.hs:1495-1597`.

Replay output:

```text
TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))
True
Right (TForall "a" Nothing (TVar "a"))
Right (TForall "a" Nothing (TVar "a"))
```

Focused same-lane tests also reran and passed:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`
- `src/MLF/Elab/Run/ResultType/Fallback.hs:558`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs:697`, and
  `src/MLF/Elab/Run/ResultType/Fallback.hs:701` still keep the same
  `boundVarTargetRoot` anchor and the same
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` route.
- `src/MLF/Elab/Run/Pipeline.hs:182-193` still keeps
  `checkedAuthoritative = typeCheck termClosed` as the authoritative public
  return path while fallback reconstruction remains diagnostics-only.

Current read:
helper-visible/internal fallback still preserves `TMu` structure with
`containsMu True`, but both authoritative public entrypoints still collapse to
`Right (TForall "a" Nothing (TVar "a"))`.

### `C3` quantified-crossing fail-closed reruns

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`
- `src/MLF/Elab/Run/ResultType/Fallback.hs:563` still defines the
  `boundHasForallFrom` traversal, and
  `src/MLF/Elab/Run/ResultType/Fallback.hs:697-701` still keeps the
  same-lane candidate only while the quantified-boundary guard stays clear.
- The accepted item-3 settlement slice remains the controlling production-
  surface read for the quantified-crossing contrast.

Current read:
once the same wrapper crosses a nested `forall` boundary, no lawful retained-
child recursive route survives on the current surfaces.

### `C4` ambiguity rerun

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects ambiguous repeated graft-weaken on the same non-front binder"'`
  -> `1 example, 0 failures`
- `test/ElaborationSpec.hs:1933` still anchors the representative ambiguity
  rejection on the elaboration surface.

Current read:
ambiguity remains fail-closed rather than being resolved by ranking or
guesswork.

### `C6` bounded termination and `N4` pressure read

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`
- The accepted item-1 freeze, accepted architectural audit, and accepted March
  25 representative campaign remain the controlling evidence for bounded
  search / termination pressure.

Current read:
the refreshed matrix still shows bounded rejection for forbidden growth inside
the inherited acyclic model. No fresh rerun in this round proves that a
required positive family now needs cyclic structure, multi-SCC search, or a
second interface.

## Refreshed Representative Matrix

This table records the item-4 matrix over `P1-row` plus `C1` through `C7`.
The inherited `C1`-`C7` tally remains unchanged; the refreshed item-4 tally
adds one explicit `P1-row` blocker-debt row rather than silently hiding that
family outside the matrix.

| Row | Controlling family or families | Exact frozen packet / route or fail-closed boundary | Fresh rerun evidence | Current surface facts that remain binding | Classification | Why the row remains bounded |
| --- | --- | --- | --- | --- | --- | --- |
| `P1-row` | `P1 local-recursive-shape` | local-lane recursive-shape support on the same-lane retained-child contrast versus the corresponding unannotated variant | focused same-lane local-root test; focused unannotated-variant test; `ARI-C1` block; item-1 freeze; architectural audit | bounded local support still exists on the current surfaces, but the corresponding unannotated variant still does not infer recursive shape and the explicit-only baseline remains live | `admitted but not reconstruction-visible / blocker debt` | the row proves bounded local support only; it does not prove review-visible unannotated automatic recursive-shape success |
| `C1` | `P2`, bounded continuity context for `P3` and `P6` | non-local alias-bound / base-like packet on `baseTarget -> baseC -> targetC` | focused non-local packet test; focused non-local proof-separation test; `ARI-C1` block; `Fallback.hs:545,717` | the exact non-local packet still stays admitted on the `baseTarget -> baseC` lane, but its accepted visible-output fact remains `TBase (BaseTy "Int")` with `containsMu False` | `admitted but not reconstruction-visible / blocker debt` | admitted non-local continuity exists, but recursive structure is still not review-visible on the current production surfaces |
| `C2` | `P3`, bounded continuity context for `P6` | exact same-lane retained-child packet on `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` | exact same-lane `cabal repl` replay; same-lane recursive fallback test; exact Phase-6 clearance test; exact authoritative public-output test; exact edge-3 instantiation test; `ARI-C1` block | helper-visible/internal replay still yields `TArrow (TVar "t32") (TMu "t38" ...)` with `containsMu True`, while both authoritative public outputs still read `Right (TForall "a" Nothing (TVar "a"))` | `admitted but not reconstruction-visible / blocker debt` | the strongest same-lane admitted pocket still collapses on the authoritative public surface, so general owner-sensitive production-surface success remains unearned |
| `C3` | `P5`, `N2` | same retained-child search, but with quantified crossing before a lawful recursive output route survives | nested-`forall` fail-closed test; `ARI-C1` block; `Fallback.hs:563,697-701`; accepted item-3 slice | quantified crossing is still detected, the clear-boundary candidate is still invalidated, and no lawful retained-child recursive route survives after the crossing appears | `fail-closed rejection` | `P5` remains reject-side only on the current surfaces; no fresh rerun upgrades quantified crossing into a positive packet |
| `C4` | `N1 ambiguity-reject` | ambiguity / competing-candidate boundary on the same non-front binder | focused ambiguity test at `test/ElaborationSpec.hs:1933`; accepted representative campaign | ambiguity is still rejected instead of being solved by heuristic ranking | `fail-closed rejection` | the row still proves only reject-side ambiguity discipline, not positive support |
| `C5` | `P4`, `P3`, `N2` | the same exact `C2` packet viewed through the owner-sensitive / binder-sensitive placement lens only | exact same-lane `cabal repl` replay; lookup-bounded test; nested-`forall` fail-closed test; exact authoritative public-output test; `ARI-C1` block | the only admitted owner-local / binder-sensitive placement story is still the exact `C2` pocket; lookup stays bounded to the same local `TypeRef` lane; no second owner-local packet appears | `admitted but not reconstruction-visible / blocker debt` | broader binder-sensitive placement persistence is still not proven, and the same admitted pocket still collapses on authoritative public output |
| `C6` | `N6`, `N4`, `N5` | bounded termination / forbidden-growth pressure under the inherited acyclic model | `ARI-C1` block; item-1 freeze; accepted audit; accepted representative campaign | bounded rejection still contains forbidden growth inside the inherited model; no fresh rerun proves cyclic structure, multi-SCC search, or a second interface is now required | `fail-closed rejection` | the row still records bounded rejection only; it does not convert pressure into an item-5 architecture decision |
| `C7` | `P6 reconstruction-visible-output` | the same exact `C2` pocket viewed through the output-surface continuity lens only | exact same-lane `cabal repl` replay; same-lane recursive fallback test; exact Phase-6 clearance test; exact authoritative public-output test; exact edge-3 instantiation test; `Fallback.hs:558,697,701`; `Pipeline.hs:182-193` | helper-visible/internal recursive structure remains visible with `containsMu True`, but both authoritative public entrypoints still return `Right (TForall "a" Nothing (TVar "a"))` | `admitted but not reconstruction-visible / blocker debt` | internal/public recursive agreement is still missing, so the row remains below `stable visible persistence` |

## Positive-Family Ledger

| Family | Strongest current representative row(s) | Current read | Why the family remains below item-5 settlement |
| --- | --- | --- | --- |
| `P1 local-recursive-shape` | `P1-row` | bounded local support exists, but not review-visible unannotated automatic success | the corresponding unannotated variant still does not infer recursive shape on the current pipeline surfaces |
| `P2 non-local-propagation` | `C1` | admitted packet only | non-local propagation still ends in non-recursive visible output (`TBase "Int"`, `containsMu False`) |
| `P3 retained-child-owner-sensitive` | `C2` primary, with bounded continuity context from `C1` and guard context from `C5` | strongest same-lane pocket is still blocker debt | recursive structure remains helper-visible/internal only; authoritative public output still collapses |
| `P4 binder-sensitive-placement` | `C5` primary, reusing the exact `C2` pocket | bounded owner-local placement only | the only admitted placement story is still one exact packet; broader binder-sensitive persistence is not proven |
| `P5 polymorphism-nested-forall` | `C3` | reject-side only | quantified crossing still invalidates the same-lane candidate before a lawful recursive output read exists |
| `P6 reconstruction-visible-output` | `C7` primary, with contrast from `C1` and route continuity context from `C2` | strongest candidate is still blocker debt | helper-visible/internal recursion still does not survive to both authoritative public entrypoints |

No positive family currently has any representative row at
`stable visible persistence`.

## Negative And Bounded-Family Ledger

| Family | Representative row(s) | Current read |
| --- | --- | --- |
| `N1 ambiguity-reject` | `C4` | ambiguity still fails closed on the current elaboration surface |
| `N2 unsoundness-guard` | `C3`, `C5` | quantified crossing and owner / binder drift remain reject-side guards rather than positive support |
| `N3 equi-recursive-required` | inherited boundary carry-forward only | still out of scope under the unchanged iso-recursive / non-equi-recursive baseline |
| `N4 cyclic-or-multi-scc-required` | explicit pressure note from `C6` plus aggregate matrix read | still boundary context only; no refreshed row proves that a required positive family now needs cyclic or multi-SCC behavior |
| `N5 second-interface-or-fallback-required` | inherited boundary carry-forward, with `C6` as bounded contrast | still out of scope; no rerun in this round points to a lawful fallback or second-interface rescue |
| `N6 termination-pressure` | `C6` | still bounded or fail-closed rejection, not broad positive support |

### Explicit `N4` Pressure Note

The refreshed matrix still keeps `N4` as boundary pressure only, not as an
accepted reopen result.

Why that remains the strongest lawful read:

1. `P1-row` still shows bounded local-shape support inside the inherited
   acyclic model.
2. `C1`, `C2`, `C5`, and `C7` still show bounded admitted pockets inside that
   same model, even though each remains blocker debt rather than success.
3. `C3`, `C4`, and `C6` still fail closed on the current surfaces instead of
   forcing a cyclic or multi-SCC escape hatch.
4. The accepted audit still keeps `non-cyclic-graph = unknown`, and no fresh
   rerun in this round lawfully upgrades that pressure into an item-5 reopen
   decision.

## Bounded Settlement Read

This item-4 refresh sharpens exactly this much, and no more:

- every positive family `P1` through `P6` is now represented directly in one
  reviewer-visible matrix;
- the current matrix still has zero `stable visible persistence` rows;
- five representative rows now sit at
  `admitted but not reconstruction-visible / blocker debt`:
  `P1-row`, `C1`, `C2`, `C5`, and `C7`;
- three representative rows remain `fail-closed rejection`:
  `C3`, `C4`, and `C6`;
- `P5` remains reject-side only through `C3`;
- `N1`, `N2`, and `N6` remain bounded fail-closed discipline rather than
  broad positive support; and
- `N4` remains explicit boundary pressure only.

The lawful refreshed matrix read therefore remains:

`bounded subset only`

This item still does not settle:

- `non-cyclic-graph = keep` versus
  `reopen the non-cyclic-graph revision question`;
- production implementation, hardening, or the repo-level capability claim; or
- any widening into cyclic search, multi-SCC search, second interfaces, or
  fallback behavior.

Item `5` remains the next immediate consumer. Items `6` through `8` remain
blocked behind that settlement gate.

## Verification Record

Baseline and continuity checks executed in the round worktree passed:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- predecessor-presence checks for the accepted item-1 freeze, accepted
  item-2 and item-3 artifacts, accepted March 25 representative campaign and
  architecture-decision artifacts, the accepted same-lane predecessor docs,
  and `orchestrator/rounds/round-094/review-record.json` through
  `orchestrator/rounds/round-101/review-record.json`
- source/test-anchor inspections for the current `P1-row`, `C1`, `C2` / `C5`
  / `C7`, `C3`, and `C4` evidence owners

Focused evidence commands executed in this round passed with the outputs
quoted above:

- all focused `cabal test mlf2-test --test-show-details=direct --test-options='--match "...'"` reruns named in the item-4 plan; and
- the exact same-lane `cabal repl mlf2-test --repl-options=-ignore-dot-ghci`
  replay with reviewer-visible helper-visible/internal and authoritative
  public outputs.

Restricted-path diff check:

- `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/state.json`
  still reports only the preexisting controller-owned
  `orchestrator/state.json` edit already noted by selection; no new edits were
  introduced under `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`, or
  `Bugs.md`.

This round remains docs-only. The full `cabal build all && cabal test` gate
therefore remains lawfully out of scope unless the diff escapes the authorized
docs / round-artifact surface.
