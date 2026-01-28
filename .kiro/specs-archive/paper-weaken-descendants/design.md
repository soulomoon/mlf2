# Design Document

## Overview
The paper’s normalized-witness condition (5) requires `Weaken(n)` to appear after operations on nodes *below* `n`, which the text and examples indicate are strict binding-tree descendants. The implementation will interpret “below n” as strict descendants (excluding `n` itself), ensuring Weaken is moved after descendant ops but not forced past operations on the same binder.

## Architecture
Normalization remains centralized in `MLF.Constraint.Presolution.Witness`. The only behavioral change is the descendant set used by `reorderWeakenWithEnv`, keeping the rest of the Ω normalization pipeline intact.

## Components and Interfaces
- `reorderWeakenWithEnv :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]`
  - Update to use strict descendants only (exclude `n` from the descendant set).
  - Add a comment citing `papers/xmlf.txt` and clarifying the strict-descendant interpretation.
- `validateNormalizedWitness :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError ()`
  - Already uses strict descendants; ensure the comment aligns with the paper text.
- Tests in `test/PresolutionSpec.hs`:
  - Adjust the “pushes Weaken after other ops on the binder” test to reflect strict-descendant semantics.
  - Add or adjust a regression test to confirm Weaken is not forced past same-binder ops in the absence of descendants.

## Data Models
No new data structures. The change is purely in how descendant sets are computed and interpreted.

## Error Handling
No new errors. Existing validation errors remain unchanged.

## Testing Strategy
- Update Hspec tests to match strict-descendant behavior.
- If feasible, add a small QuickCheck property asserting that `reorderWeakenWithEnv` moves Weaken past strict descendants while preserving order against same-binder ops when there are no descendants.
- Run the full test suite.
