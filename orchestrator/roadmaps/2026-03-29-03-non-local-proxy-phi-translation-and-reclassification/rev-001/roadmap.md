# Non-Local Proxy Phi-Translation and Reclassification — Roadmap

## Context

- This roadmap family succeeds
  `2026-03-29-02-iso-recursive-inference-gap-fixes`
  (rounds 146–150). That family fixed four implementation gaps and corrected
  documentation, bringing the automatic iso-recursive inference to
  feature-complete status for local recursive types.
- Two remaining issues were identified during the final analysis:

  1. **Nested-forall-mediated μ absorption** — Currently framed as a "known
     remaining limitation" in `implementation_notes.md:25` and tested with
     "fails closed" naming in `test/Research/P5ClearBoundarySpec.hs:73`. In
     reality, this is **correct behavior**: polymorphic mediation (e.g.,
     `let rec f = id f`) legitimately absorbs the μ wrapper during constraint
     solving. The test assertions are correct (`containsMu == False`) but the
     test names and documentation framing mislead readers into thinking this
     is a deficiency. Reclassification only — no semantic changes.

  2. **Non-local proxy PhiTranslatabilityError** — The result-type fallback
     now correctly returns μ-types for non-local reconstruction
     (`containsMu fallbackTy == True` at `PipelineSpec.hs:2242`), but
     pipeline elaboration entrypoints still fail with
     `PhiTranslatabilityError` at two sites:
     - **Site A:** `reifyInst` in `MLF.Reify.Type` (line 352) — TyMu node
       has 0 `orderedFlexChildren` binders in the non-local case. Local TyMu
       nodes have exactly 1 authoritative binder child from the binding tree,
       but non-local proxy wrappers lack this structure.
       Error: `"reifyInst: missing authoritative instantiation translation
       for TyMu without binder child"`
     - **Site B:** OpRaise in `MLF.Elab.Phi.Omega.Interpret` (line 1147) —
       Non-spine computation for a node whose bind-parent doesn't resolve to
       root or forall produces `Nothing` for both `mbCandidate` and
       `mbRootInst`.
       Error: `"OpRaise (non-spine): missing computation context"`
     This is a real implementation gap in the elaboration/Phi-translation
     layer.

- Production baseline: 1175 examples, 0 failures on branch
  `codex/automatic-recursive-type-inference` @ `fb51d26`.
- Thesis alignment: iso-recursive types are an extension beyond the core
  thesis. The non-local proxy fix deepens that extension. Non-recursive
  programs remain unaffected.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [pending] Reclassify nested-forall μ absorption as known correct behavior
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when:
   - Test description in `test/Research/P5ClearBoundarySpec.hs:73` is renamed
     from "fails closed once the same wrapper crosses a nested forall
     boundary" to accurately describe this as correct polymorphic-mediation
     behavior (μ legitimately absorbed)
   - Test description in `test/Research/P5ClearBoundarySpec.hs:91` is renamed
     from "hits the same authoritative instantiation-translation blocker..."
     to accurately describe that the downstream PhiTranslatabilityError is a
     consequence of the correct non-recursive outcome, not a separate blocker
   - `implementation_notes.md:25-26` "Known remaining limitations" section
     reclassifies this from "limitation" to "known correct behavior under
     polymorphic mediation"
   - Test assertions remain unchanged (`containsMu == False` is correct)
   - No code changes beyond test descriptions and documentation
   - `cabal build all && cabal test` passes

2. [pending] Fix reifyInst TyMu without authoritative binder child for non-local proxy
   Item id: `item-2`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when:
   - `reifyInst` in `src/MLF/Reify/Type.hs` (line 352) handles TyMu nodes
     with 0 `orderedFlexChildren` binders for non-local proxy wrappers
     (synthesize binder context, use fallback reconstruction, or reconstruct
     from the TyMu body binding structure)
   - The fix preserves existing local TyMu reification (1-binder path)
     unchanged
   - New targeted test(s) validate non-local TyMu reification succeeds
   - Existing tests do not regress (1175 examples, 0 failures)
   - `cabal build all && cabal test` passes

3. [pending] Fix OpRaise non-spine missing computation context for non-local bind-parent
   Item id: `item-3`
   Depends on: `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: complete when:
   - `OpRaise` in `src/MLF/Elab/Phi/Omega/Interpret.hs` (line 1147)
     provides or constructs computation context for non-local bind-parent
     configurations where both `mbCandidate` and `mbRootInst` are `Nothing`
   - The fix preserves existing spine and local non-spine OpRaise behavior
     unchanged
   - This item may be redundant if item-2 resolves the upstream cause — if
     so, mark done with a note explaining the dependency
   - Existing tests do not regress
   - `cabal build all && cabal test` passes

4. [pending] Upgrade pipeline entrypoint test from expected-failure to success
   Item id: `item-4`
   Depends on: `item-2`, `item-3`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-3`
   Completion notes: complete when:
   - `test/PipelineSpec.hs:2303` test "hits elaboration blocker for non-local
     proxy wrapper despite open fallback at pipeline entrypoints" is upgraded
     from `expectStrictPipelineFailure` to a success assertion that validates
     both `runPipelineElab` and `runPipelineElabChecked` produce correct
     recursive output types
   - Survey `test/ElaborationSpec.hs` PhiTranslatabilityError-asserting tests
     (12+ sites) and determine which, if any, are testing the same non-local
     proxy pattern vs legitimate untranslatable cases — document findings
   - Any related tests that now pass are upgraded accordingly
   - Test description updated to reflect the successful behavior
   - `cabal build all && cabal test` passes

5. [pending] Update documentation to reflect completed non-local proxy support
   Item id: `item-5`
   Depends on: `item-1`, `item-2`, `item-3`, `item-4`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-4`
   Completion notes: complete when:
   - `implementation_notes.md` removes the non-local proxy
     PhiTranslatabilityError from "Known remaining limitations" and describes
     it as a resolved gap
   - `CHANGELOG.md` records the non-local proxy fix
   - `docs/thesis-deviations.yaml` DEV-AUTO-ISO-RECURSIVE is updated with
     the new code paths and expanded test evidence
   - No code changes (documentation only)
   - `cabal build all && cabal test` passes
