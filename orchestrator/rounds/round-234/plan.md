### Selected Extraction
- Milestone: ForallSpec Binder Safety
- Milestone id: milestone-4
- Direction id: direction-4a-forallspec-binder-safety
- Extracted item id: item-4a-sigma-reorder-index-totalization
- Roadmap id: 2026-05-05-00-type-level-safety-singletons-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001

### Goal
Close the remaining milestone-4 binder-ordering gap by totalizing `MLF.Elab.Sigma` while preserving the accepted list-shaped `ForallSpec` contract with binder count derived from `fsBounds`.

### Approach
Current HEAD already satisfies two milestone-4 requirements: `ForallSpec` no longer carries a redundant binder-count field, and `forallSpecBinderCount` derives arity from `fsBounds`. Round-233 also removed partial binder-spine reads from the selected Phi/Omega path. The remaining live gap is `src/MLF/Elab/Sigma.hs`, where both `bubbleReorderTo` and `bubbleReorderToFromSpine` still read binder identities through `!!` while driving quantifier reordering used by Phi translation. Existing tests cover successful reordering and some fail-fast cases, but they do not yet close this last partial-indexing seam. This round should therefore stay on milestone-4 as a bounded implementation slice, not a closeout or roadmap-change round.

### Steps
1. Add a local checked lookup boundary in `MLF.Elab.Sigma` for the current target binder at `idx`, so reorder logic reports explicit `InstantiationError` when the desired binder list is inconsistent instead of indexing with `!!`.
2. Rewrite `bubbleReorderTo` and `bubbleReorderToFromSpine` to use the checked lookup boundary for both equality checks and `elemIndex` searches, preserving the existing successful reorder behavior while removing the remaining partial binder-order reads from this path.
3. Add focused regression coverage for the new fail-closed behavior in the Sigma/Phi reorder surface, including a mismatch or short-desired-list case that exercises the new checked reads, plus a preservation case that proves live Phi-driven reordering still succeeds.
4. Keep `ForallSpec` unchanged unless the evidence forces a narrowly scoped helper or comment update: binder count must remain derived from `fsBounds`, no heavier type-level dependency may be introduced, and milestone-5 witness-constructor work stays out of scope.
5. Run focused Sigma/Phi validation first, then diff hygiene, then the full `cabal build all && cabal test` gate required for behavior-changing milestone-4 work.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-options='--match "Σ(g) quantifier reordering"'`
- `cabal test mlf2-test --test-options='--match "scheme-aware Φ can target a non-front binder"'`
- Manual source check that `src/MLF/Elab/Sigma.hs` no longer uses partial `!!` in the selected reorder path and that `ForallSpec` binder count still derives from `fsBounds`
- `cabal build all && cabal test`
