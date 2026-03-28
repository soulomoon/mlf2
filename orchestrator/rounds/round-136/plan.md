# Round 136 Plan

- Roadmap item: `item-2`
- Retry state: `null`
- Execution shape: one serial bounded implementation/validation lane; no parallel sidecars authorized
- Live subject: exact packet `sameLaneAliasFrameClearBoundaryExpr` only

## Frozen packet

```haskell
recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))

sameLaneAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))
```

The added alias binder `hold` is part of the frozen subject. Do not swap back
to the settled first-pocket packet, the settled exact `P5` packet, the settled
exact `C1` / `P2` packet, or any broader `P3` / `P4` / `P6` family bundle.

## Allowed write scope for this round

Narrow below this set when possible, but do not widen beyond it:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Scope.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `src/MLF/Elab/TermClosure.hs`
- `test/PipelineSpec.hs`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

Do not touch `orchestrator/state.json`, roadmap files, `Bugs.md`,
`src/MLF/Constraint/**`, cyclic or multi-SCC machinery, fallback widening, a
second interface, or any broader family settlement artifact.

## Sequential plan

1. Anchor the exact packet on the authoritative pipeline surface. Reuse or add
   one focused spec entry for `sameLaneAliasFrameClearBoundaryExpr` in
   `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, wiring
   `test/Main.hs` and `mlf2.cabal` only if that focused module does not already
   exist, and add a narrow `test/PipelineSpec.hs` regression only if it is
   needed to keep the authoritative `runPipelineElab` /
   `runPipelineElabChecked` surface visible. Name the focused example so it can
   be selected with `cabal test mlf2-test --test-show-details=direct
   --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`.

2. Capture the current exact-packet read before any production edit. Run the
   focused representative-gap example on the frozen packet and record the
   pre-change outcome on the authoritative pipeline surface only. Use that run
   to distinguish the current state as one of: no recursive visibility yet,
   explicit fail-closed behavior, or a narrower current-architecture blocker.
   Treat this as packet-local evidence only.

3. Apply the smallest production-path change that can preserve the same-lane
   retained-child route across the added alias binder `hold`. Start with
   `src/MLF/Elab/Run/Scope.hs` and
   `src/MLF/Elab/Run/ResultType/Fallback.hs`; touch
   `src/MLF/Elab/TermClosure.hs` or `src/MLF/Elab/Run/Pipeline.hs` only if the
   authoritative surface cannot expose the exact-packet result otherwise; touch
   `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs` only for
   re-export continuity if the production pipeline entrypoint shape changes. Do
   not introduce equi-recursive reasoning, cyclic structure, multi-SCC search,
   fallback behavior, or any packet-general heuristic.

4. Re-run the focused packet test and the narrowest adjacent guards needed to
   prove there was no silent widening. Keep the guard set bounded to the
   settled first-pocket continuity checks and the settled exact `P5` contrast
   checks already living in the writable test slice. End this step with exactly
   one packet-local classification only: authoritative-surface recursive
   visibility, fail-closed, or narrower current-architecture blocker.

5. Run the required verification for a code/test-touching `item-2` round:
   `git diff --check`,
   `python3 -m json.tool orchestrator/state.json >/dev/null`,
   the roadmap-locator checks named in `verification.md`, and
   `cabal build all && cabal test`.
   No verification result from this round may be presented as general `P3` /
   `P4` / `P6` settlement or repo-level automatic recursive-inference
   readiness.
