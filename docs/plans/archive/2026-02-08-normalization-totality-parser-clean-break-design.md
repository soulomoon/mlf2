# Design: Normalization Totality + Parser Clean Break

Date: 2026-02-08

## Context

Two defects were identified in the staged frontend boundary.

1. `normalizeType` currently contains a reachable runtime crash in `normalizeBound` (`error "unreachable"`) when nested alias-bound rewriting yields a variable in structural-bound context.
2. Parser API still exports legacy wrappers (`parseEmlfExpr`, `parseEmlfType`), which conflicts with the locked migration decision: clean break without compatibility wrappers.

This design keeps staged boundaries explicit, preserves normalized-only compiler contracts, and removes non-total behavior.

## Goals

- Make frontend normalization total: no runtime `error` paths.
- Keep parser names explicit and stage-aware.
- Remove compatibility wrappers from internal and public parser APIs.
- Keep behavior deterministic and testable via typed errors.
- Align implementation and documentation with locked PRD decisions.

## Approaches Considered

### Approach A (recommended): Typed failure + explicit parser names only

- Replace the `STVar` crash path in `normalizeBound` with a typed constructor in `NormalizationError`.
- Keep parser entrypoints explicit (`parseRaw*`, `parseNorm*`) and remove legacy aliases everywhere.

Pros:
- Preserves total function discipline (`Either` as single failure channel).
- Minimal surface-area change with high correctness impact.
- Fully aligned with clean-break PRD constraints.

Cons:
- Public API becomes breaking for callers still using alias names.

### Approach B: Internal typed failure + keep wrappers with deprecation

Pros:
- Easier migration for downstream users.

Cons:
- Conflicts with locked migration decision and non-goals.
- Prolongs ambiguous API surface.

### Approach C: Make nested-alias variable case impossible by rewrite-only strategy

Pros:
- Avoids introducing a new error constructor.

Cons:
- More invasive rewriting logic; harder to reason about and test.
- Does not guarantee future totality if new branches are introduced.

## Selected Design

### 1) Normalization error model

Extend `NormalizationError` with:

- `NonStructuralBoundInStructContext SrcType`

`SrcType` payload carries the full offending subtree (selected by user) for diagnostics and test assertions.

### 2) Total normalization behavior

In `normalizeBound`:

- `STVar v` returns `Left (NonStructuralBoundInStructContext (STVar v))`.
- Existing self-bound detection remains `SelfBoundVariable`.
- Alias inlining recursion remains unchanged, but now propagates typed errors instead of crashing.

Result: `normalizeType` and `normalizeExpr` are total in practice and by API contract.

### 3) Parser API clean break

Remove legacy wrappers from:

- `src/MLF/Frontend/Parse.hs`
- `src-public/MLF/API.hs`

Public parser surface becomes strictly:

- raw: `parseRawEmlfExpr`, `parseRawEmlfType`
- normalized: `parseNormEmlfExpr`, `parseNormEmlfType`

No alias names are exported.

## Data Flow and Error Propagation

- Parse normalized path remains: `parseRaw*` then `normalize*`.
- Normalization failures become `NormNormErr` via `NormParseError` unchanged.
- New constructor integrates into this flow without changing parser result types.

## Test Plan

- Add normalization regression for the reported nested-bound reproducer; assert typed `Left`, no crash.
- Add direct unit assertion for `NonStructuralBoundInStructContext` payload shape.
- Remove legacy alias equivalence tests from `FrontendParseSpec`.
- Update `FrontendPrettySpec` roundtrip checks to use `parseRawEmlf*`.
- Run validation gate: `cabal build all && cabal test`.

## Documentation and Tracking Updates

- Update `implementation_notes.md` to remove compatibility-wrapper claims.
- Update `CHANGELOG.md` to state clean-break parser names and totalized normalization.
- Add bug entries in `Bugs.md` for:
  - reachable normalization crash path
  - parser clean-break drift
  Then move both to resolved with linked regression tests in the same iteration.

## Acceptance Criteria

- No runtime `error` reachable from normalization.
- Legacy parser aliases removed from exports and definitions.
- Tests updated and passing for explicit raw/normalized parser API.
- Docs and bug tracker reflect actual behavior and policy alignment.
