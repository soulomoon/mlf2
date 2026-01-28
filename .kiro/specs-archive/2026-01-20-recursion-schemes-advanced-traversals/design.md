# Design Document

## Overview
We replace explicit recursion in Elab/Frontend structural traversals with
recursion-schemes from recursion-schemes 5.2.3. The refactor uses advanced
schemes (para/apo/zygo/histo) when they clarify combined computations or
capture-avoidance rules. Effectful folds are centralized in a helper module.

## Architecture
- **MLF.Util.RecursionSchemes**
  - Provide small helpers for effectful folds (e.g., cataM/cataEither/cataMaybe)
    plus convenience wrappers for common Elab types.
- **MLF.Elab.Types / MLF.Elab.Term / MLF.Frontend.Types**
  - Replace explicit recursion with cata/para/apo/zygo/histo as appropriate.
  - Keep capture-avoidance and binder scoping identical.

## Components and Interfaces
- Add or extend base functors and Recursive/Corecursive instances where
  needed.
- Add helper folds for effectful traversals, without expanding public API.
- Use advanced schemes in areas such as:
  - `para` where the original subtree is needed (e.g., binder shadowing).
  - `zygo` for a combined property + reconstruction pass.
  - `apo` for expansion or early stop rebuilds.
  - `histo` when a node needs access to previously computed subresults.

## Data Models
No changes to runtime data structures are required. Any helper types introduced
for folds should remain local to their modules unless shared broadly.

## Error Handling
Effectful folds must preserve existing error constructors and propagation
patterns. If a traversal previously used Either/Maybe, the new fold must be
structurally equivalent.

## Testing Strategy
- Rerun existing tests after each conversion batch.
- Add a regression test only when a bug is found during the refactor.

## References
- Data.Functor.Foldable (recursion-schemes 5.2.3):
  https://hackage.haskell.org/package/recursion-schemes-5.2.3/docs/Data-Functor-Foldable.html
