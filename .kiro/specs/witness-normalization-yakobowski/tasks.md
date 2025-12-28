# Implementation Plan

- [x] 1. Add normalization context + error types
  - Steps:
    - Define `OmegaNormalizeEnv` and `OmegaNormalizeError` in
      `src/MLF/Constraint/Presolution/Witness.hs` (or a new
      `MLF.Constraint.Presolution.Normalize` module).
    - Thread `OrderKey` imports and keep types internal to presolution.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs` (or new module)
  - Tests: compile-only (no behavior change yet).
  - Verification: `rg -n "OmegaNormalizeEnv|OmegaNormalizeError" src/MLF/Constraint/Presolution` returns matches
  - _Requirements: 1.1, 2.1, 4.1_

- [x] 2. Implement a validator for paper conditions (1)–(5)
  - Steps:
    - Add `validateNormalizedWitness` that checks:
      - Graft/Weaken target in I(r)
      - Merge/RaiseMerge direction uses ≺ (m ≺ n)
      - Raise only for nodes bound under r
      - RaiseMerge uses m ∉ I(r)
      - Weaken occurs after all ops on nodes below n
    - Use `Binding.Tree` for ancestry checks and `OrderKey` for ≺ comparisons.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests:
    - Add unit tests in `test/PresolutionSpec.hs` (or new spec) for each failed
      condition.
  - Verification: `cabal test --test-show-details=direct --test-options='--match /normalized witness/'`
  - _Requirements: 1.1, 1.2, 3.1_

- [x] 3. Normalize RaiseMerge sequences
  - Steps:
    - Implement a pass to coalesce `Raise(n)^k; Merge(n, m)` into
      `OpRaiseMerge n m` when `m ∉ I(r)`.
    - Reject malformed sequences that attempt to leave I(r) without required
      raises (return `MalformedRaiseMerge`).
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests:
    - Unit tests for valid and invalid RaiseMerge patterns.
  - Verification: `cabal test --test-show-details=direct --test-options='--match /RaiseMerge/'`
  - _Requirements: 1.1, 1.2, 3.1_

- [x] 4. Normalize Weaken placement
  - Steps:
    - Implement a pass that moves `OpWeaken n` after all operations on nodes
      below `n` (binding-tree descendants), preserving relative order of
      unrelated ops.
    - Ensure determinism by using ≺ keys to break ties when reordering.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests:
    - Unit tests that verify Weaken is last for its subtree.
  - Verification: `cabal test --test-show-details=direct --test-options='--match /Weaken.*normalized/'`
  - _Requirements: 1.1, 2.1, 3.1_

- [x] 5. Assemble `normalizeInstanceOpsFull`
  - Steps:
    - Compose passes: canonicalize -> coalesce RaiseMerge -> check merge
      direction -> reorder Weaken -> validate.
    - Ensure deterministic output for identical inputs.
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`
  - Tests:
    - Property test: normalized output passes `validateNormalizedWitness`.
  - Verification: `cabal test --test-show-details=direct --test-options='--match /normalizeInstanceOpsFull/'`
  - _Requirements: 1.1, 2.1, 3.2_

- [x] 6. Wire normalization context at call sites
  - Steps:
    - Build `OmegaNormalizeEnv` in `buildEdgeWitness` using
      `EdgeTrace.etInterior`, `Order.orderKeysFromRoot`, and the current
      constraint + canonicalizer.
    - Reorder witness/trace construction if needed so I(r) is available.
    - Use `normalizeInstanceOpsFull` for witness construction.
  - Files: `src/MLF/Constraint/Presolution/Driver.hs`
  - Tests:
    - Regression tests for presolution witness ops still passing.
  - Verification: `cabal test --test-show-details=direct --test-options='--match /Presolution witness ops/'`
  - _Requirements: 4.1, 4.2_

- [x] 7. Keep compatibility wrapper
  - Steps:
    - Keep `normalizeInstanceOps` as a wrapper that calls the full normalizer
      (or update all call sites in this change set and delete the old name).
  - Files: `src/MLF/Constraint/Presolution/Witness.hs`, callers
  - Tests:
    - Compile + existing tests.
  - Verification: `cabal build`
  - _Requirements: 4.1_

- [x] 8. Full test run
  - Verification: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - _Requirements: 3.1, 3.2, 4.1_
