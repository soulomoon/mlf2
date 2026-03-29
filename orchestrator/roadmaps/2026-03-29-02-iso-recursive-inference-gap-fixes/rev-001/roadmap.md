# Iso-Recursive Inference Gap Fixes — Roadmap

## Context

- This roadmap family succeeds
  `2026-03-29-01-automatic-iso-recursive-type-inference-completion`
  (rounds 143–145). That family validated simple self-recursion through the
  full pipeline (inference -> elaboration -> type checking -> reduction) and
  recorded documentation claiming the feature was complete.
- Post-mortem analysis revealed that the documentation overstates the feature
  status. The 1175-test suite includes tests that **assert expected failures**
  for several recursive families. Four specific gaps prevent the feature from
  being truly complete:
  1. **Phase 4 witness normalization** — Nested recursive lets fail with
     `WitnessNormalizationError` because `normalizeInstanceOpsCore` and
     `validateNormalizedWitness` do not handle TyMu nodes. Witness operations
     land outside expected interiors or have invalid merge directions due to
     the recursive structure. (Test: `PipelineSpec.hs:1350`)
  2. **Phase 6 alias-bounds hard stop** — μ/∀ interaction and higher-order
     recursion fail with "alias bounds survived scheme finalization" in
     `Finalize.hs:502`. `simplifySchemeBindings` cannot inline variable-bound
     bindings when TyMu cycles are present; `substBound` for TMu avoids
     substitution when var names match (to preserve recursion), blocking
     alias-bound inlining. (Tests: `PipelineSpec.hs:1396`, `:1419`)
  3. **ELet reduction limitation** — `Reduce.hs:33-35` treats `ELet` as
     non-recursive substitution. For `let f = \x. f x in f`, `f` remains
     free in the RHS after substitution, causing `TCUnboundVar`. No
     fixpoint/letrec infrastructure exists in the elab pipeline.
     (Test: `PipelineSpec.hs:1477`)
  4. **Result-type reconstruction fail-closed** — `Fallback.hs` gates
     candidate selection on `rootBindingIsLocalType`. Non-local recursive
     result types get a quantified fail-closed shell instead of the actual
     μ-type. `keepTargetFinal` requires `rootBindingIsLocalType`, blocking
     non-local recursive surface types. (Tests: `PipelineSpec.hs:2104`,
     `:2235`, `:2306`)
- The documentation from round-144 (`implementation_notes.md`,
  `roadmap.md`, `TODO.md`, `CHANGELOG.md`, `docs/thesis-deviations.yaml`)
  must be corrected once these gaps are fixed (or earlier if intermediate
  progress clarifies scope).
- Production baseline: 1175 examples, 0 failures on branch
  `codex/automatic-recursive-type-inference` @ `0542d29`.
- Thesis alignment: iso-recursive types are an extension beyond the core
  thesis (which assumes acyclic constraint graphs). These fixes deepen the
  extension. Any deviation from thesis behavior for non-recursive programs
  remains covered by the existing test suite.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Phase 4: Extend witness normalization to handle TyMu nodes
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when:
   - `normalizeInstanceOpsCore` in `WitnessNorm.hs` / `Witness.hs` handles
     TyMu nodes during instance-operation normalization (e.g., treating TyMu
     interiors as valid operation targets)
   - `validateNormalizedWitness` in `WitnessValidation.hs` accepts witnesses
     involving TyMu nodes without rejecting them as "outside interior" or
     "invalid merge direction"
   - The nested recursive lets test (`PipelineSpec.hs:1350`) is upgraded from
     `expectAlignedPipelinePastPhase3` (expecting Phase 4 failure) to a full
     pipeline success assertion
   - Existing witness normalization behavior for non-recursive types is
     unchanged (zero regressions)
   - `cabal build all && cabal test` passes

2. [done] Phase 6: Fix alias-bound resolution for recursive types
   Item id: `item-2`
   Depends on: `item-1`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-1`
   Completion notes: complete when:
   - `simplifySchemeBindings` in `Normalize.hs` handles TyMu-cycle alias
     bounds (either by inlining through the cycle or by controlled survival
     of recursive alias bounds that are provably safe)
   - `substBound` / scheme finalization in `Finalize.hs` no longer hard-stops
     on alias bounds that are only unresolved because of recursive structure
   - μ/∀ interaction test (`PipelineSpec.hs:1396`) is upgraded from expected
     failure to full pipeline success
   - Higher-order recursion test (`PipelineSpec.hs:1419`) is upgraded from
     expected failure to full pipeline success
   - Existing non-recursive scheme finalization is unchanged
   - `cabal build all && cabal test` passes

3. [pending] ELet: Add recursive-let (fixpoint) reduction support
   Item id: `item-3`
   Depends on: none
   Parallel safe: yes
   Parallel group: elet-and-resulttype
   Merge after: none
   Completion notes: complete when:
   - `ELet` reduction in `Reduce.hs` supports recursive bindings via fixpoint
     unfolding (either add a recursion flag to `ELet`, introduce `ELetRec`,
     or use a Y-combinator / direct self-substitution strategy)
   - `substTermVar` handles recursive capture correctly (bound variable
     appears in its own RHS)
   - The recursive ELet test (`PipelineSpec.hs:1477`) is upgraded from
     tolerating `TCUnboundVar` to a clean success
   - `step`/`normalize` can reduce recursive applications without leaving
     unbound variables
   - Non-recursive `ELet` reduction is unchanged
   - `cabal build all && cabal test` passes

4. [pending] Result-type: Open fallback reconstruction for recursive types
   Item id: `item-4`
   Depends on: none
   Parallel safe: yes
   Parallel group: elet-and-resulttype
   Merge after: none
   Completion notes: complete when:
   - `Fallback.hs` recognizes μ-types in non-local positions and returns
     the actual recursive surface type instead of the quantified fail-closed
     shell
   - The `keepTargetFinal` / candidate selection logic has a controlled
     opening for well-formed recursive type structures
   - Fail-closed tests (`PipelineSpec.hs:2104`, `:2235`, `:2306`) are
     upgraded to assert `containsMu fallbackTy == True` (or pipeline
     success with the recursive type preserved)
   - Local-type fallback behavior for non-recursive types is unchanged
   - `cabal build all && cabal test` passes

5. [pending] Correct documentation to reflect accurate iso-recursive scope
   Item id: `item-5`
   Depends on: `item-1`, `item-2`, `item-3`, `item-4`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-4`
   Completion notes: complete when:
   - `implementation_notes.md` accurately describes which recursive families
     are supported (simple self-recursion, nested recursive lets, μ/∀
     interaction, higher-order recursion, non-local recursive result types)
   - `roadmap.md` records the gap-fix campaign as a completed capability
   - `TODO.md` is updated to reflect current status
   - `CHANGELOG.md` records the expanded iso-recursive inference scope
   - `docs/thesis-deviations.yaml` is updated if the gap fixes introduce
     new thesis deviations
   - No code changes in this round (documentation only)
   - `cabal build all && cabal test` passes
