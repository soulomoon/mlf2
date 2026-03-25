# Same-Lane Retained-Child Public-Output Continuity Authoritative Path Audit

Date: 2026-03-26
Round: `round-095`
Roadmap item: `item-2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: docs-only audit of the exact authoritative public-output path
for the frozen same-lane retained-child pocket only
Artifact kind: canonical docs-only item-2 authoritative-path audit record

## Stage Contract Freeze

This artifact implements only roadmap item `2` for `attempt-1` with
`retry: null`.

Item `2` is docs-only and audit-only. Its job is to inspect only the exact
same-lane retained-child pocket already frozen by accepted item `1`, map the
same-pocket path from helper-visible reconstruction to the authoritative
public output, and localize the first exact owner-local continuity-loss site
without repairing behavior or widening the subject.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`;
- edits to `Bugs.md`;
- the item-3 minimum bounded implementation / proof slice;
- the item-4 end-to-end revalidation / classification slice;
- the item-5 bounded architecture-pressure decision slice;
- the non-local alias-bound family;
- neighboring consumer routes;
- nested-`forall`, nested owner, or nested scheme-root positive success;
- replay / `InstBot` repair;
- broad automatic-recursive-inference claims; or
- a reopened `non-cyclic-graph` revision argument.

The inherited boundary remains fixed and unchanged:

- explicit recursive annotations remain the current production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

Accepted `N14`, accepted strategic items `2`, `5`, `6`, and `7`, plus
accepted rounds `089` through `094`, contribute bounded predecessor evidence
only. They do not prove broad automatic recursive inference, do not clear the
non-local alias-bound family, and do not reopen `non-cyclic-graph`.

## Fixed Pocket Under Audit

The only lawful subject for this audit remains the exact same pocket fixed by
accepted item `1`:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
- quantified-boundary status: clear-boundary only; and
- exact packet:

  ```haskell
  ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
  ```

  where
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

Nothing in this audit upgrades that exact pocket into a family-level or
repo-level success claim. The live subject remains one pocket only.

## Frozen Internal Versus Public Split

The continuity split under audit remains the exact same split already frozen
by accepted predecessor evidence:

| Surface | Frozen read for this pocket | Audit rule |
| --- | --- | --- |
| Helper-visible / internal output | recursive structure remains visible as `TMu ...` and `containsMu True` on the accepted helper-visible reconstruction path | if this path stops carrying recursive structure, continuity has changed |
| Authoritative public output | both authoritative public entrypoints currently return `TForall "a" Nothing (TVar "a")` for the same pocket | if this surface changes, that is later-round behavior change, not silent rewrite here |

This audit explains where the split becomes authoritative. It does not repair
the split.

## Exact Same-Pocket Path Map

The same frozen pocket feeds two bounded read paths.

### Helper-Visible Reconstruction Path

1. The exact same-lane retained-child harness in
   `test/PipelineSpec.hs:1495-1570` rewires the already-frozen packet into
   the same local `TypeRef` lane and then calls
   `computeResultTypeFallback` directly on the exact retained-child inner
   application.
2. `src/MLF/Elab/Run/ResultType.hs:72-106` builds the result-type view and
   dispatches the non-annotation case into
   `Fallback.computeResultTypeFallback`.
3. `src/MLF/Elab/Run/ResultType/Fallback.hs:558-780` keeps the exact pocket
   on the same lane:
   - `boundVarTargetRoot` anchors the retained-child target root;
   - `boundHasForallFrom` rejects quantified crossings for this pocket;
   - `sameLaneLocalRetainedChildTarget` keeps the lookup bounded to the same
     local `TypeRef` lane;
   - `keepTargetFinal` preserves that retained-child choice; and
   - `generalizeWithPlan` then reconstructs the final scheme at `targetC`,
     which folds back into a `TMu`-bearing result.
4. The accepted exact-pocket helper-visible read therefore remains:
   recursive structure survives reconstruction as `TMu ...`, and
   `containsMu True` still holds.

### Authoritative Public-Output Path

1. `src/MLF/Elab/Run/Pipeline.hs:77-95` makes both
   `runPipelineElab` and `runPipelineElabChecked` aliases of the same
   `runPipelineElabWith` implementation.
2. `src/MLF/Elab/Run.hs:1-18`, `src/MLF/Elab/Pipeline.hs:84-92`, and
   `src-public/MLF/Pipeline.hs:61-86` re-export that same implementation
   without adding a second path or alternate authoritative surface.
3. Inside `runPipelineElabWith`,
   `src/MLF/Elab/Run/Pipeline.hs:147-181` computes `rootScope`,
   `rootTarget`, `rootScheme`, `rootSubst`, `termSubst`, and finally
   `termClosed`. Those are the exact same-pocket anchors that feed the
   checked public result.
4. `src/MLF/Elab/Run/Pipeline.hs:182-194` defines
   `checkedAuthoritative = typeCheck termClosed` and then, in both the
   annotation and fallback branches, computes the reconstruction result only
   for diagnostics before returning `checkedAuthoritative`.
5. The accepted exact-pocket public read therefore remains:
   `runPipelineElab` and `runPipelineElabChecked` both surface the checked
   type `TForall "a" Nothing (TVar "a")`.

## First Exact Owner-Local Continuity-Loss Site

The current exact collapse anchor remains unchanged.

The first exact owner-local continuity-loss site is still the
`checkedAuthoritative` return choice in
`src/MLF/Elab/Run/Pipeline.hs:186-194`.

Why this remains the first exact site:

1. The helper-visible reconstruction path already preserves continuity for
   the exact same pocket all the way through
   `computeResultTypeFallback` and the same-lane retained-child
   `generalizeWithPlan` call in `Fallback.hs:758-780`.
2. The public entrypoint chain does not introduce an earlier split:
   `runPipelineElab`, `runPipelineElabChecked`, `MLF.Elab.Run`,
   `MLF.Elab.Pipeline`, and `MLF.Pipeline` all forward to the same
   `runPipelineElabWith` implementation.
3. `termClosed` and `typeCheck termClosed` are exact same-pocket dependencies
   of the authoritative result, but they are not separate public-surface
   exits. They supply the checked type that `checkedAuthoritative` returns.
4. The explicit continuity loss happens when `runPipelineElabWith` discards
   the `computeResultTypeFallback` result as diagnostics-only and returns the
   checked type instead. That is the first exact place where the
   helper-visible `TMu`-bearing reconstruction stops being the public result
   for this pocket.

The bounded unchanged-anchor conclusion is therefore:

- helper-visible continuity still survives through the fallback
  reconstruction path;
- authoritative public continuity still breaks when the pipeline commits to
  `checkedAuthoritative`; and
- the exact same-pocket dependencies of that decision remain `termClosed` and
  `typeCheck termClosed`, not a reopened `Phase 6 (elaboration)` failure.

## Why Earlier Same-Pocket Anchors Are Not The First Break

The following exact same-pocket anchors remain continuity-preserving for this
item-2 audit:

| Anchor | Why it is not the first break |
| --- | --- |
| `computeResultTypeFallback` dispatch in `ResultType.hs:72-106` | It still reaches the retained-child-specific fallback path rather than erasing the same-lane pocket. |
| same-lane retained-child selection in `Fallback.hs:648-731` | It still preserves the exact family / anchor / frame / route / clear-boundary tuple and still chooses the retained child when lawful. |
| `generalizeWithPlan` in `Fallback.hs:758-780` | It still reconstructs the exact same-pocket helper-visible type, which is why `TMu ...` and `containsMu True` remain live evidence. |
| public re-export chain in `Run.hs`, `Elab/Pipeline.hs`, and `src-public/MLF/Pipeline.hs` | It forwards the same authoritative result path without creating an earlier or alternate public surface. |
| accepted Phase-6 clearance from round `091` | The exact-pocket entrypoints still clear elaboration; the current audit shows a later public-output collapse, not a revived elaboration failure. |

## Bounded Audit Conclusion

The canonical item-2 conclusion is:

- the exact frozen tuple under audit is unchanged;
- the helper-visible reconstruction path still preserves recursive structure
  review-visibly as `TMu ...` plus `containsMu True`;
- the authoritative public-output path still collapses to
  `TForall "a" Nothing (TVar "a")` on both public entrypoints; and
- the first exact owner-local continuity-loss site remains the
  `checkedAuthoritative` return choice in
  `src/MLF/Elab/Run/Pipeline.hs:186-194`, with `termClosed` and
  `typeCheck termClosed` as the exact same-pocket dependencies that feed that
  authoritative result.

This remains blocker debt inside the current architecture for now because the
audit localizes a bounded public-output commitment point; it does not prove
that the acyclic representation is impossible for this pocket, and it does
not yet land a repair.

## Item-3 Handoff

The next lawful round remains roadmap item `3` only:
clear or confirm the exact authoritative public-output collapse within the
current architecture for this same frozen pocket.

Item `3` may consume this unchanged-anchor audit, but item `2` itself does
not repair behavior, does not revalidate the whole ledger, and does not
reopen `non-cyclic-graph`.

## Verification Note

This round is expected to change only documentation artifacts:

- this canonical item-2 authoritative-path audit; and
- `orchestrator/rounds/round-095/implementation-notes.md`.

Even though the diff stays docs-only, the focused exact-pocket tests remain
mandatory because this audit makes runtime-path claims about the same frozen
pocket:

- `same-lane retained-child exact packet clears Phase 6 elaboration`;
- `same-lane retained-child exact packet authoritative public output stays forall identity`;
- `same-lane retained-child exact edge 3 authoritative instantiation`; and
- `ARI-C1 feasibility characterization (bounded prototype-only)`.

The full `cabal build all && cabal test` gate is still out of scope unless
the diff escapes the authorized docs-only surface.
