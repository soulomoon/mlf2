# Design Document

## Overview

We will simplify MLF.Elab.Generalize by extracting reusable helpers, normalizing naming, and applying recursion-schemes for purely structural ElabType traversals. Graph traversals (with visited/stop sets) will remain explicit but will
be routed through shared utilities to reduce duplication.

## Architecture

- MLF.Elab.Generalize remains the primary module for generalization logic.
- MLF.Elab.Util (or a small Generalize.Util) will host reusable helpers such as reachability with stop predicates and key/canonicalization utilities.

## Components and Interfaces

- New or extended helper(s) in MLF.Elab.Util:
    - reachableFromStop :: (a -> Int) -> (a -> a) -> (a -> [a]) -> (a -> Bool) -> a -> IntSet
    - Optional small helpers for canonical/key extraction if repeated.
- Generalize updates:
    - Replace local reachability functions with reachableFromStop.
    - Promote nested local go functions to named helpers where they serve distinct purposes.
    - Use cata/para for any remaining purely structural ElabType traversals.

## Data Models

No data model changes. All refactors are structural and keep existing types.

## Error Handling

No changes to error constructors or propagation. Shared helpers must preserve current behavior for cycle prevention and stop-set logic.

## Testing Strategy

- cabal build after each refactor batch.
- cabal test once all simplifications are complete.
