# Global Non-Cyclic-Graph `C1` / `C2` / `C5` Production-Surface Settlement Evidence Slice

Date: 2026-03-26
Round: `round-100`
Roadmap item: `item-2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded production-surface settlement-evidence slice for
the frozen `C1`, `C2`, and `C5` rows only
Artifact kind: canonical docs-only item-2 settlement-evidence dossier

## Stage Contract Freeze

This artifact implements only roadmap item `2` for `attempt-1` with
`retry: null`.

This round is docs-first and evidence-first. It sharpens only the current
production-surface settlement read for:

- `P2 non-local-propagation` through `C1`;
- `P3 retained-child-owner-sensitive` through `C2`; and
- `P4 binder-sensitive-placement` through `C5`.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, roadmap contracts, retry contracts,
  verification contracts, or `Bugs.md`;
- production implementation, hardening, representative-campaign work, or the
  item-5 global `non-cyclic-graph` settlement gate;
- a second `C5` packet, a neighboring same-lane route, a non-local-family
  widen, or a broader owner/binder theory;
- replay repair, `InstBot` repair, equi-recursive reasoning, cyclic
  structural graphs, multi-SCC search, second interfaces, or fallback
  widening; or
- any promotion of this slice into repo-level `P6` success,
  representative-campaign success, or
  `non-cyclic-graph = keep` / `reopen`.

The inherited boundary remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized; and
- no second interface or fallback widening is authorized.

## Frozen Subject And Family Mapping

The frozen three-row subject remains:

- `C1`: the admitted non-local alias-bound / base-like packet on
  `baseTarget -> baseC -> targetC`, exercised by
  `schemeAliasBaseLikeFallback False`;
- `C2`: the exact same-lane retained-child packet on
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
- `C5`: the binder-sensitive / owner-sensitive placement lens that reuses the
  same exact `C2` packet, the same one owner-local retained-child frame, and
  the same clear-boundary-only status.

The family-to-row responsibilities remain explicit:

- `C1` is the primary bounded row for `P2` and may contribute only bounded
  continuity context for `P3`;
- `C2` is the primary bounded row for `P3`; and
- `C5` is the primary bounded row for `P4` while still preserving the `N2`
  owner / binder guard context.

`C5` is not a second packet and not a second same-lane family. It is the
same exact `C2` pocket viewed through the binder-sensitive / owner-sensitive
placement lens.

## Accepted Continuity Reused Without Widening

The accepted predecessor chain remains binding and unchanged:

- item `1` froze the repo-level settlement vocabulary and unresolved-family
  ledger in
  `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`;
- the accepted capability contract, architectural audit, full-pipeline
  validation contract, representative campaign, and architecture-decision
  record still keep repo-level `non-cyclic-graph` settlement unresolved and
  the current matrix tally at zero `stable visible persistence` rows;
- rounds `072` through `074` remain the authoritative bounded non-local
  `C1` predecessor chain:
  `baseTarget-non-local-proof-slice-established`,
  accepted evidence consolidation, and `continue-bounded`;
- rounds `094` through `098` remain the authoritative bounded same-lane
  `C2` / `C5` predecessor chain:
  exact pocket frozen,
  `checkedAuthoritative` kept as the first exact break,
  the current-architecture collapse confirmed as blocker debt,
  end-to-end revalidation classified as
  `admitted but not reconstruction-visible / blocker debt`,
  and the exact-pocket architecture-pressure read kept within the current
  architecture; and
- open `BUG-2026-03-16-001` remains predecessor replay context only. It does
  not authorize repair work or a broader settlement claim in this round.

Fresh read-only reruns in this round do not contradict that accepted chain.

## Fresh Read-Only Evidence

### `C1` focused non-local reruns

- `src/MLF/Elab/Run/ResultType/Fallback.hs:545-548` still defines
  `rootNonLocalSchemeAliasBaseLike` only from
  `not rootBindingIsLocalType`, `rootIsSchemeAlias`, and
  `rootBoundIsBaseLike`.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:709-717` still keeps the
  non-local `baseTarget -> baseC -> targetC` arm distinct from the preserved
  local lanes.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`
  passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
  passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  passed with `22 examples, 0 failures`.

No new public-output-specific harness was introduced for `C1`. The accepted
item-1 ledger and the accepted representative campaign still bind the current
visible-output fact for this exact bounded packet to
`TBase (BaseTy "Int")` with `containsMu False`.

### `C2` / `C5` exact same-lane reruns

The exact same-lane replay was rerun through
`cabal repl mlf2-test --repl-options=-ignore-dot-ghci` with
`:module + *PipelineSpec` and the same read-only harness logic already
anchored in `test/PipelineSpec.hs:1495-1570`.

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
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> `22 examples, 0 failures`

## Three-Row Settlement Matrix

| Row | Exact frozen packet / route | Controlling target | Fresh rerun evidence | Current surface facts that remain binding | Classification | Why the row remains bounded |
| --- | --- | --- | --- | --- | --- | --- |
| `C1` | non-local alias-bound / base-like packet on `baseTarget -> baseC -> targetC`, exercised by `schemeAliasBaseLikeFallback False` | `P2 non-local-propagation` | `Fallback.hs:545-548`, `Fallback.hs:709-717`; focused non-local packet test; focused non-local proof-separation test; focused `ARI-C1` block | the explicit `rootNonLocalSchemeAliasBaseLike` proof still exists; the selected packet still reads `TBase (BaseTy "Int")` with `containsMu False`; the currently accepted visible-output fact remains non-recursive base-like output only | `admitted but not reconstruction-visible / blocker debt` | `C1` still proves only one admitted non-local packet; it does not yet show reconstruction-visible recursive persistence on the existing production surfaces, so it sharpens `P2` only and cannot be promoted into repo-level `P6` or global settlement |
| `C2` | exact same-lane retained-child packet on `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` | `P3 retained-child-owner-sensitive` | exact same-lane replay through `cabal repl`; same-lane recursive fallback test; exact Phase-6 clearance test; exact authoritative public-output test; exact edge-3 instantiation test; focused `ARI-C1` block | helper-visible/internal replay still yields `TArrow (TVar "t32") (TMu "t38" ...)` with `containsMu True`; both authoritative public entrypoints still return `Right (TForall "a" Nothing (TVar "a"))`; the accepted item-2 / item-3 same-lane chain still keeps `checkedAuthoritative` as the first exact break and still records no alternate whole-packet authoritative recursive result | `admitted but not reconstruction-visible / blocker debt` | `C2` is still one exact same-lane admitted pocket only; recursive structure remains helper-visible/internal, but authoritative public output still collapses to `forall identity`, so this row does not reach `stable visible persistence` and does not settle general owner-sensitive production-surface success |
| `C5` | the same exact `C2` packet, the same one owner-local retained-child frame, and the same `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` route; not a second packet | `P4 binder-sensitive-placement` | the same exact same-lane replay; same-lane lookup-bounded test; same wrapper nested-`forall` fail-closed test; same authoritative public-output test; focused `ARI-C1` block | the only admitted owner-local / binder-sensitive placement story is still the exact `C2` packet; lookup remains bounded to the same local `TypeRef` lane; the same wrapper still fails closed once it crosses a nested `forall` boundary; no second admitted owner-sensitive packet appears on fresh evidence | `admitted but not reconstruction-visible / blocker debt` | `C5` still reuses one exact owner-local pocket only; broader binder-sensitive placement persistence is not proven, and the nested-`forall` / frame-drift contrast remains fail-closed, so this row cannot be promoted into general `P4` success |

## Bounded Settlement Synthesis

This item-2 slice now sharpens exactly this much, and no more:

- `P2` still has one admitted non-local packet, but not a
  reconstruction-visible non-local production-surface success;
- `P3` still has one admitted same-lane retained-child pocket, but not a
  general owner-sensitive production-surface success; and
- `P4` still has one admitted owner-local placement pocket, but not a
  general binder-sensitive placement success.

The frozen repo-level consequences remain unchanged:

- none of `C1`, `C2`, or `C5` reaches `stable visible persistence`;
- the accepted matrix still has zero `stable visible persistence` rows;
- `C5` remains the same exact `C2` packet rather than a second packet;
- none of these three rows by itself forces
  `reopen the non-cyclic-graph revision question`; and
- item `3`, item `4`, item `5`, and any code or test edits remain later work
  only.

## Verification Record

Baseline and continuity checks executed in the round worktree all passed:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- predecessor-presence checks for the item-1 artifact, the accepted `C1`
  predecessor docs, and the accepted `C2` / `C5` predecessor docs

Focused evidence commands executed in this round all passed with the outputs
quoted above. This round remains docs-only, so the full
`cabal build all && cabal test` gate stays lawfully out of scope unless the
diff escapes the authorized docs / round-artifact surface.
