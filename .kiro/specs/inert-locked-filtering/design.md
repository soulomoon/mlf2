# Design Document

## Overview
Thesis section 15.2.3 requires that propagation witnesses never transform
inert-locked nodes. These nodes cannot be represented in xMLF translation,
so we rewrite the presolution by weakening inert-locked nodes (Lemma 15.2.4)
and reject any presolution where inert-locked nodes remain. Witness filtering
is not part of the thesis-aligned approach once weakening succeeds.

## Architecture
- **Inert classification**
  - Add a helper (new module `MLF.Constraint.Inert` or similar) to compute
    the set of inert nodes and inert-locked nodes from a `Constraint`.
  - Inert-locked definition: `n` is inert, is flexibly bound, and its
    binding path contains a rigid edge (thesis 15.2.2).
- **Presolution rewrite**
  - Add a pass in presolution/solve that weakens inert-locked nodes to rigidly
    bound nodes when possible (Lemma 15.2.4), producing an inert-equivalent
    presolution.
  - Record a structured error if the rewrite cannot produce a translatable
    presolution.
- **Witness handling**
  - Do not filter witnesses; rely on the post-weaken check that inert-locked
    nodes are absent (Lemma 15.2.6).

## Components and Interfaces
- New helper module: `src/MLF/Constraint/Inert.hs` (proposed)
  - `inertNodes :: Constraint -> Either BindingError IntSet`
  - `inertLockedNodes :: Constraint -> Either BindingError IntSet`
- Presolution/solve hook:
  - `src/MLF/Constraint/Presolution/Driver.hs` or `src/MLF/Constraint/Solve.hs`
    to run the inert-locked weakening pass before witness translation.
- Witness normalization:
  - No inert-locked filtering; normalization only enforces conditions (1)–(5)
    and interior stripping.

## Data Models
- No new witness environment fields required; inert-locked handling is covered
  by presolution rewriting and a post-weaken validation.

## Error Handling
- If inert-locked nodes remain after rewriting, return a structured error and
  do not attempt translation.
- If inert detection fails due to binding tree invariants, surface
  `BindingTreeError`.

## Testing Strategy
- **Regression test**: construct a constraint with an inert-locked node and
  verify that the weakening pass removes it (no inert-locked nodes remain).
- **Integration test**: translation succeeds for a case that previously
  contained inert-locked operations after the rewrite pass.

## References
- `papers/these-finale-english.txt` section 15.2.3 (inert-locked nodes)
- Lemma 15.2.4 (weakening inert nodes)
