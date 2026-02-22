# Generalized Unification (Chapter 7.6)

## Overview

Implement generalized unification for the Solve phase, matching thesis Section 7.6.
The key change: split `solveUnify` into two phases — first-order unification (worklist),
then batch Rebind — instead of interleaving harmonization per-pair.

## Approach: Hybrid (defer first, upgrade to equivalence-class batch)

### Step 1: Defer + replay pairs

Current flow:
```
worklist loop { unify pair → harmonize pair → enqueue children } → rewrite
```

Proposed flow:
```
worklist loop { unify pair → collect pair → enqueue children } → batch harmonize → rewrite
```

Changes to `Solve.hs`:
- Add `suDeferredHarmonize :: [(NodeRef, NodeRef)]` to `SolveState`
- `solveRepresentative` stops calling `harmonize`, appends pair to deferred list
- After worklist `loop` drains, new `batchHarmonize` step runs before `applyUFConstraint`
- `batchHarmonize` canonicalizes both sides through final UF, deduplicates, calls
  `harmonizeBindParentsWithTrace` for each remaining pair

### Step 2: Multi-node harmonization primitive

Upgrade batch harmonize to operate on equivalence classes.

New function in `Binding.Adjustment`:
```haskell
harmonizeBindParentsMulti
    :: [NodeRef] -> Constraint
    -> Either BindingError (Constraint, [NodeId])
```

This takes all members of an equivalence class and:
1. Collects all their bind parents
2. Computes the LCA across all of them (fold `bindingLCA` pairwise)
3. Raises every member to that LCA in one pass

In `Solve.hs`, `batchHarmonize` changes from replaying pairs to:
1. Compute equivalence classes from the final UF (invert the parent map)
2. For each class with >1 member, call `harmonizeBindParentsMulti`
3. One Rebind per class instead of one per original pair

## Testing

1. **Regression**: all 767 existing tests pass unchanged
2. **Figure 7.6.1**: construct the motivating example where sequential Rebind fails
   but simultaneous succeeds (in `test/SolveSpec.hs`)
3. **Multi-node class**: 3+ nodes in one equivalence class, verify multi-node LCA
   produces correct result

## Error Handling

No new error constructors. Deferred harmonization uses existing `BindingTreeError` path.
Errors surface after the worklist instead of during it.

## Thesis Obligations

- New obligation `O07-GENUNIF` scoped to Section 7.6
- Covers: Def 7.6.1, Def 7.6.2, Section 7.6.2 algorithm, Lemma 7.6.3
- Expand `CLM-UNIFICATION` or add `CLM-GEN-UNIFICATION` in thesis-claims.yaml

## Files Touched

- `src/MLF/Constraint/Solve.hs` — deferred harmonize, batch harmonize, equiv-class grouping
- `src/MLF/Binding/Adjustment.hs` — `harmonizeBindParentsMulti` (Step 2)
- `test/SolveSpec.hs` — new tests
- `docs/thesis-obligations.yaml` — new obligation
- `docs/thesis-claims.yaml` — expanded/new claim

## References

- `papers/these-finale-english.txt` Section 7.6 (lines 8364-8440)
- Definition 7.6.1: generalized unification problem
- Definition 7.6.2: generalized admissibility
- Section 7.6.2: generalized algorithm
- Lemma 7.6.3: soundness, completeness, principality
