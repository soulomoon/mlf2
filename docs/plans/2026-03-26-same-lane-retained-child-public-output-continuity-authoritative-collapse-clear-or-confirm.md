# Same-Lane Retained-Child Public-Output Continuity Authoritative Collapse Clear-Or-Confirm

Date: 2026-03-26
Round: `round-096`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-3`
Retry state: active retry-only repair for the rejected `attempt-2`
reviewer-visibility miss
Live subject: exact authoritative public-output collapse clear-or-confirm for
the frozen same-lane retained-child pocket only
Artifact kind: canonical bounded item-3 blocker-proof record with live
retry-framing refresh only

## Stage Contract Freeze

This artifact implements only roadmap item `3` for live `attempt-3`.

This pass is the active retry-only repair for the rejected `attempt-2`
reviewer-visibility miss only. It refreshes stale reviewer-visible retry
framing and preserves the same blocker-proof reasoning, later-work note,
accepted split, unchanged blocker anchor, root-handoff replay, and docs-only
scope.

The live pocket remains exactly the accepted frozen pocket from items `1`
and `2`:

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

This round was authorized to change only the exact root handoff around
`rootScheme`, `termClosed`, and `checkedAuthoritative` in
`src/MLF/Elab/Run/Pipeline.hs`, plus a tiny `TermClosure.hs` helper only if
that exact repair existed. No widening into the non-local alias-bound
family, neighboring routes, nested-`forall`, fallback widening, cyclic
search, a second interface, or a reopened `non-cyclic-graph` claim was
authorized.

## Accepted Continuity Kept Unchanged

The accepted predecessor evidence remains unchanged and is consumed only as
bounded input:

- item `1` still freezes the exact same pocket and the exact helper/internal
  versus authoritative/public split;
- item `2` still fixes the accepted unchanged-anchor audit:
  `checkedAuthoritative` remains the first exact owner-local continuity-loss
  site for this same frozen pocket, with `termClosed` and
  `typeCheck termClosed` as the same-pocket dependencies that feed that
  authoritative result; and
- accepted round `092` / item `4` still freezes the helper-visible internal
  recursive read (`TMu ...`, `containsMu True`) versus the authoritative
  public `TForall "a" Nothing (TVar "a")` read.

This item-3 round does not rewrite that accepted history. Its job is only to
determine whether the approved `Pipeline.hs` handoff slice can clear the
public-output collapse, or whether that slice can only confirm blocker debt.

## Exact Root-Handoff Replay

The bounded implementation replay inspected only the live root handoff for
the exact packet inside the current code path.

Observed root-handoff facts:

```text
rootScheme = Forall [("a",Nothing)] TVar "a"
rootSubst = fromList [(16,"a")]
typeCheck term = Right (TForall "a" Nothing (TVar "a"))
typeCheck termSubst = Right (TForall "a" Nothing (TVar "a"))
typeCheck termClosed = Right (TForall "a" Nothing (TVar "a"))
computeResultTypeFallback resultTypeInputs annCanon ann
  = Right (TForall "a" Nothing (TVar "a"))
```

The same replay also printed the elaborated root term. The relevant exact
fact is that the nested retained-child output already appears at the root as:

```text
ELet "u" Forall [("a",Nothing)] TVar "a" ...
```

That means the root term entering the selected `Pipeline.hs` handoff is
already committed to the same `forall identity` public shape that the current
entrypoints report.

## Why The Approved Repair Slice Cannot Clear The Collapse

The approved repair slice was the smallest possible root-handoff experiment:
change only the exact `Pipeline.hs` handoff near `rootScheme`,
`termClosed`, and `checkedAuthoritative`, with `TermClosure.hs` available
only for a tiny coherence helper if that would finish the same exact repair.

The replay above proves that this slice cannot clear the collapse:

1. The root-generalization result is already `Forall [("a",Nothing)] TVar "a"`.
2. The elaborated root term, substituted root term, and closed root term all
   type-check to that same `TForall "a" Nothing (TVar "a")`.
3. The root-level diagnostic fallback for the whole packet also already
   agrees with the same `TForall "a" Nothing (TVar "a")`.
4. Therefore a local change that only swaps which already-computed
   root-handoff artifact becomes authoritative cannot expose recursive
   structure again for this packet, because this exact handoff slice no
   longer contains an alternate recursive root result to return.
5. A tiny term-closure helper cannot repair that state either: the root term
   already type-checks as `forall identity`, so `TermClosure.hs` has no
   same-slice recursive root scheme to align against.

The accepted helper-visible internal recursive evidence still exists, but it
exists on the already-accepted bounded helper path, not as an alternate
whole-packet root result available inside the approved `Pipeline.hs` slice.

## Bounded Blocker Conclusion

Item `3` does not land a code/test repair.

Instead, this round confirms bounded blocker debt inside the unchanged
current architecture:

- the exact frozen same-lane retained-child pocket still preserves recursive
  structure on the accepted helper-visible internal path;
- the exact authoritative public output for the whole packet still collapses
  to `TForall "a" Nothing (TVar "a")`;
- the approved root-handoff `Pipeline.hs` slice contains no alternate
  recursive whole-packet output to expose authoritatively; and
- clearing the collapse would therefore require earlier or wider work than
  the authorized item-3 `Pipeline.hs` / `TermClosure.hs` slice.

The strongest lawful item-3 result is therefore:
`blocker debt confirmed under the unchanged current architecture`.

This artifact does not widen that blocker into the non-local alias-bound
family, neighboring routes, nested-`forall`, cyclic search, a second
interface, fallback widening, or a reopened `non-cyclic-graph` decision.

Current roadmap item `4` and item `5` remain later work only. This round
does not perform the end-to-end revalidation/classification gate or the
architecture-pressure decision gate.

## Files Left Unchanged

No production code or tests changed in this round.

`src/MLF/Elab/Run/Pipeline.hs`,
`src/MLF/Elab/TermClosure.hs`,
`test/PipelineSpec.hs`,
controller-owned state,
roadmap bundle files,
retry / verification contracts,
and accepted predecessor artifacts all remain unchanged.

## Verification Note

This round is docs-only:

- canonical item-3 artifact:
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- round-local notes:
  `orchestrator/rounds/round-096/implementation-notes.md`

Because the diff does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, the full `cabal build all && cabal test` gate is not required
for this item-3 blocker-proof outcome. Focused exact-pocket verification and
baseline control-plane checks remain required.

Recorded passes:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- baseline predecessor-presence `test -f ...` checks from the active
  verification contract
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization"'`
