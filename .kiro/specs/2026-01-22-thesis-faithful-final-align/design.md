# Design Document

## Overview

This refactor completes the thesis‑faithful boundary by (1) moving remaining policy logic out of `MLF.Elab.Generalize` into presolution planning modules and (2) decoupling `ElabError` into a neutral module so presolution does not depend on Elab. Elab becomes an apply‑only layer.

## Architecture

- **Neutral error module**: introduce `MLF.Util.ElabError` (or `MLF.Types.Error`) to host `ElabError` and related helpers.
  - `MLF.Elab.Types` re‑exports `ElabError` for backwards compatibility.
  - Presolution planning modules import the neutral error module directly.

- **Planning completion**: move any remaining policy logic in `MLF.Elab.Generalize` into `MLF.Constraint.Presolution.Plan.*` (most likely `Plan.Normalize`, `Plan.ReifyPlan`, or a new `Plan.Finalize`).
  - Elab should only apply precomputed plan outputs (e.g., substitution maps, binder names, scheme validation results).

## Target module moves / additions

### New module

- `src/MLF/Util/ElabError.hs`
  - Moves `ElabError` (and any small helpers) out of `MLF.Elab.Types`.

### Presolution plan additions

- `src/MLF/Constraint/Presolution/Plan/Finalize.hs` (if needed)
  - Hosts final scheme validation / naming decisions currently performed in `MLF.Elab.Generalize`.
  - Returns a plan output used verbatim by Elab.

### Elab updates

- `src/MLF/Elab/Types.hs`
  - Re‑export `ElabError` from the neutral module.
- `src/MLF/Elab/Generalize.hs`
  - Replace any remaining decision/policy logic with calls to plan outputs.

## Dependency untangling

1. **Presolution plan modules** must not import `MLF.Elab.Types`.
   - Update imports to `MLF.Util.ElabError` (or neutral module name chosen).
2. **Elab modules** continue to import `MLF.Elab.Types`, which now re‑exports `ElabError`.
3. Ensure no cycles: presolution → elab must remain acyclic.

## Testing Strategy

- Run `cabal test`.

