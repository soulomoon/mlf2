# Design Document

## Overview
`witnessFromExpansion` now suppresses `OpWeaken` for instantiation steps that
occur before a later `ExpForall` in the same expansion order. This keeps binder
metas flexible until the new forall is introduced, preserving Q(n) and the
final quantifier structure.

## Architecture
- Expand the expansion recipe into a linear list (flattening `ExpCompose`).
- Compute a per-component flag `hasForallLater` by scanning from the right.
- For `ExpInstantiate` components with `hasForallLater = True`, omit `OpWeaken`.
- Preserve `OpGraft` and `OpMerge` ordering; keep `StepIntro` placement intact.

## Components and Interfaces
- `src/MLF/Constraint/Presolution/Witness.hs`
  - `witnessFromExpansion` flattens `ExpCompose` and computes suffix flags.
  - `classify` omits `OpWeaken` when a later `ExpForall` exists.

## Data Models
No data-model changes are required.

## Error Handling
- The witness builder continues to surface existing `PresolutionError` cases.
- No new error constructors are added.

## Testing Strategy
- Preserve the existing `PresolutionSpec` case for `ExpCompose` ordering with
  `StepIntro`.
- Use the bounded aliasing pipeline regression as the end-to-end check that
  quantifiers are preserved.
