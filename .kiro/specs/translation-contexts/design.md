# Design Document

## Overview
Implement translation and computation contexts from Chapter 15.3 for gMLF to
xMLF translation. This builds on existing context handling but makes the
context computation explicit and reusable by Phi translation.

## Architecture
- Represent translation contexts as a list of steps (under/inside).
- Compute contexts from binding and structural paths in the solved graph.
- Use contexts in Phi translation for non-spine Raise and insertion decisions.

## Components and Interfaces
- `src/MLF/Elab/Types.hs`
  - Context representation (`ContextStep`) or a new data type if needed.
- `src/MLF/Elab/Phi.hs`
  - Context computation and application in non-spine operations.
- `src/MLF/Binding/Tree.hs`
  - Helper queries for binding paths and bound containment if needed.

## Error Handling
- If a context cannot be derived, return an explicit error rather than using a
  fallback ordering.

## Testing Strategy
- Add unit tests for context computation (under vs inside).
- Add Phi translation regressions that require inside-bound contexts.

## References
- `papers/these-finale-english.txt` Chapter 15.3
- `papers/xmlf.txt` Figure 10 (contexts)
