# Global Non-Cyclic-Graph `C3` / `C7` Production-Surface Settlement Evidence Slice

Date: 2026-03-26
Round: `round-101`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded production-surface settlement-evidence slice for
the frozen `C3` and `C7` rows only
Artifact kind: canonical docs-only item-3 settlement-evidence dossier

## Stage Contract Freeze

This artifact implements only roadmap item `3` for `attempt-1` with
`retry: null`.

This round is docs-first and evidence-first. It sharpens only the current
production-surface settlement read for:

- `P5 polymorphism-nested-forall` through `C3`; and
- `P6 reconstruction-visible-output` through `C7`.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, roadmap contracts, retry contracts,
  verification contracts, or `Bugs.md`;
- production implementation, hardening, representative-campaign work, or the
  item-5 global `non-cyclic-graph` settlement gate;
- a second `C7` packet, a neighboring same-lane route, a second public
  interface, or a broader reconstruction family theory;
- a new positive polymorphism replay packet for `C3`, replay repair,
  equi-recursive reasoning, cyclic structural graphs, multi-SCC search, or
  fallback widening; or
- any promotion of this slice into repo-level `P5` success,
  repo-level `P6` success,
  representative-campaign success, or
  `non-cyclic-graph = keep` / `reopen`.

The inherited boundary remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized; and
- no second interface or fallback widening is authorized.

## Frozen Subject And Family Mapping

The frozen two-row subject remains:

- `C3`: nested-`forall` / quantified-crossing pressure observed on the same
  owner-local retained-child search where the clear-boundary candidate route
  would otherwise be
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
- `C7`: the exact same-lane retained-child packet on
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  reread through solver -> elaboration -> reconstruction -> internal output
  -> public output continuity.

The family-to-row responsibilities remain explicit:

- `C3` is the primary bounded row for `P5` and still carries `N2`
  unsoundness-guard context; and
- `C7` is the primary bounded row for `P6`, while reusing the same exact
  same-lane pocket already accepted for `C2` and `C5`.

`C3` is reject-side pressure only. It is not a third admitted family and it
does not upgrade the accepted nested-`forall` record into positive `P5`
success.

`C7` is not a new packet and not a second public path. It is the same exact
same-lane retained-child pocket already accepted for `C2` and `C5`, now read
strictly through the output-surface continuity lens.

## Accepted Continuity Reused Without Widening

The accepted predecessor chain remains binding and unchanged:

- item `1` froze the repo-level settlement vocabulary and unresolved-family
  ledger in
  `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`;
- item `2` already sharpened `C1`, `C2`, and `C5` in
  `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  while intentionally leaving `C3` and `C7` open for this round;
- the accepted capability contract, architectural audit, full-pipeline
  validation contract, representative campaign, and architecture-decision
  record still keep repo-level `non-cyclic-graph` settlement unresolved and
  the current matrix tally at zero `stable visible persistence` rows;
- the accepted representative campaign still binds `C3` to
  `fail-closed rejection` and `C7` to
  `admitted but not reconstruction-visible / blocker debt`;
- rounds `094` through `098` remain the authoritative bounded same-lane
  predecessor chain for the exact `C7` pocket:
  exact pocket frozen,
  `checkedAuthoritative` kept as the first exact break,
  authoritative collapse confirmed,
  end-to-end revalidation classified as blocker debt, and
  the architecture-pressure read kept within the current architecture; and
- open `BUG-2026-03-16-001` remains predecessor replay context only. It does
  not authorize repair work or a broader settlement claim in this round.

Fresh read-only reruns in this round do not contradict that accepted chain.

## Fresh Read-Only Evidence

### `C3` nested-`forall` / quantified-crossing reruns

- `src/MLF/Elab/Run/ResultType/Fallback.hs:558-563` still defines
  `boundVarTargetRoot` and the `boundHasForallFrom` traversal that detects
  quantified crossing on the same owner-local retained-child search.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:692-729` still restricts candidate
  selection to `not hasForall`, keeps the same-lane candidate in
  `sameLaneLocalRetainedChildTarget`, computes `keepTargetFinal`, and falls
  back away from that candidate once the quantified-boundary guard no longer
  stays clear.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  passed with `22 examples, 0 failures`.

No new positive public-output replay harness was introduced for `C3`.
The accepted item-1 ledger, accepted representative campaign, and the fresh
guard/source reruns above still bind the current visible-output read for the
quantified-crossing contrast to non-recursive reject-side evidence only:
`containsMu False`, no lawful retained-child route survives after the
crossing appears, and positive `P5` success remains unearned.

### `C7` exact same-lane output-surface reruns

The exact same-lane replay was rerun through `cabal repl mlf2-test` with
`:module + *PipelineSpec` and the same read-only harness logic already
anchored in `test/PipelineSpec.hs:1500-1569`.

Replay output:

```text
TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))
True
Right (TForall "a" Nothing (TVar "a"))
Right (TForall "a" Nothing (TVar "a"))
```

The replay therefore re-confirmed, for the same exact packet only:

- helper-visible/internal `computeResultTypeFallback` still yields a
  `TMu`-bearing type term;
- `containsMu True` still holds on that same helper-visible/internal read;
- `runPipelineElab` still returns
  `Right (TForall "a" Nothing (TVar "a"))`; and
- `runPipelineElabChecked` still returns that same authoritative public type.

The focused same-lane tests also reran and passed:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`

The current source anchors for the same pocket also remain unchanged:

- `src/MLF/Elab/Run/ResultType/Fallback.hs:697-729` still keeps the exact
  same-lane retained-child route and its `targetC` handoff live only for this
  one owner-local pocket; and
- `src/MLF/Elab/Run/Pipeline.hs:182-193` still computes
  `checkedAuthoritative = typeCheck termClosed`, keeps result-type
  reconstruction for diagnostics only, and returns the checked public type as
  authoritative.

Fresh exact-pocket replay therefore reproduces the same accepted split:
recursive structure remains review-visible on the helper-visible/internal
surface only, while both authoritative public entrypoints still collapse to
`forall identity`.

## Two-Row Settlement Matrix

| Row | Exact frozen packet / route | Controlling target | Fresh rerun evidence | Current surface facts that remain binding | Classification | Why the row remains bounded |
| --- | --- | --- | --- | --- | --- | --- |
| `C3` | same owner-local retained-child search where the clear-boundary candidate would be `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, but the candidate crosses a nested `forall` boundary before a lawful recursive output read exists | `P5 polymorphism-nested-forall` | `Fallback.hs:558-563`; `Fallback.hs:692-729`; same-lane lookup-bounded test; same-lane recursive retained-child contrast test; nested-`forall` fail-closed test; focused `ARI-C1` block; accepted item-1 ledger and representative campaign | `boundHasForallFrom` still detects quantified crossing on the same retained-child search; candidate selection still requires `not hasForall`; once the crossing appears, the same-lane retained-child candidate is no longer lawfully preserved through `keepTargetFinal -> targetC`; the accepted visible-output fact for the quantified-crossing contrast remains non-recursive (`containsMu False`) | `fail-closed rejection` | `C3` still proves reject-side quantified-boundary pressure only. It does not become a positive `P5` packet, does not add a new admitted family, and does not by itself force `reopen the non-cyclic-graph revision question` |
| `C7` | exact same-lane retained-child packet on `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` | `P6 reconstruction-visible-output` | exact same-lane replay through `cabal repl`; same-lane recursive fallback test; exact Phase-6 clearance test; exact authoritative public-output test; exact edge-3 instantiation test; focused `ARI-C1` block; `Fallback.hs:697-729`; `Pipeline.hs:182-193`; accepted same-lane predecessor chain | helper-visible/internal replay still yields `TArrow (TVar "t32") (TMu "t38" ...)` with `containsMu True`; both authoritative public entrypoints still return `Right (TForall "a" Nothing (TVar "a"))`; `checkedAuthoritative` still remains the first exact public-output break because the checked public type is returned as authoritative while fallback reconstruction is diagnostics-only | `admitted but not reconstruction-visible / blocker debt` | `C7` still reuses one exact same-lane pocket only; recursive structure remains helper-visible/internal, but it is still not review-visible on both internal and public output surfaces, so this row does not reach `stable visible persistence`, does not settle repo-level `P6`, and does not settle the architecture gate |

## Bounded Settlement Synthesis

This item-3 slice now sharpens exactly this much, and no more:

- `P5` remains reject-side only because quantified crossing still invalidates
  the same-lane retained-child candidate before a lawful recursive
  production-surface read exists; and
- `P6` still has one strongest bounded same-lane candidate, but recursive
  structure is still not lawfully review-visible on both internal and public
  output surfaces for repo-scope success.

The frozen repo-level consequences remain unchanged:

- `C3` remains `fail-closed rejection`;
- `C7` remains
  `admitted but not reconstruction-visible / blocker debt`;
- neither row reaches `stable visible persistence`;
- the accepted matrix still has zero `stable visible persistence` rows;
- `C3` remains reject-side pressure only rather than positive `P5` success;
- `C7` remains the same exact `C2` / `C5` pocket rather than a new packet or
  alternate public path;
- neither row by itself forces
  `reopen the non-cyclic-graph revision question`; and
- item `4`, item `5`, and any code or test edits remain later work only.

## Verification Record

Baseline and continuity checks executed in the round worktree all passed:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- predecessor-presence checks for the item-1 artifact, the accepted item-2
  artifact, the same-lane predecessor docs, and the accepted same-lane review
  records
- `rg -n 'boundVarTargetRoot =|boundHasForallFrom start0 =|sameLaneLocalRetainedChildTarget =|keepTargetFinal =|case sameLaneLocalRetainedChildTarget of|not hasForall' src/MLF/Elab/Run/ResultType/Fallback.hs`
- `rg -n 'checkedAuthoritative =|typeCheck termClosed|computeResultTypeFallback resultTypeInputs' src/MLF/Elab/Run/Pipeline.hs`
- `rg -n 'C3|C7|P5|P6|stable visible persistence|admitted but not reconstruction-visible / blocker debt|fail-closed rejection' docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md orchestrator/rounds/round-101/implementation-notes.md`
- `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/state.json`

Focused evidence commands executed in this round all passed with the outputs
quoted above. This round remains docs-only, so the full
`cabal build all && cabal test` gate stays lawfully out of scope unless the
diff escapes the authorized docs / round-artifact surface.

The restricted-path diff check still reports the preexisting controller-owned
`orchestrator/state.json` edit already noted in `round-101` planning
context. No new edits were introduced under `src/`, `src-public/`, `app/`,
`test/`, `mlf2.cabal`, or `Bugs.md`.
