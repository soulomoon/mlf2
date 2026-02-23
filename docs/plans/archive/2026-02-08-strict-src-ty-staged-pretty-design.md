# Strict SrcTy + Staged Pretty Design

## Context

Current frontend typing uses three concrete forms (`SrcType`, `NormSrcType`, `StructBound`). This diverges from the intended single indexed model and leaves pretty-printing bound to raw-only signatures.

The approved direction is strict replacement with one staged type family and staged/generic pretty entrypoints.

## Decision

Adopt a single frontend type AST:

- `SrcTy (n :: SrcNorm) (v :: SrcTopVar)`
- `SrcNorm = RawN | NormN`
- `SrcTopVar = TopVarAllowed | TopVarDisallowed`

Retain existing constructor names (`STVar`, `STArrow`, `STBase`, `STCon`, `STForall`, `STBottom`) to minimize migration churn and preserve readability in tests and pipeline modules.

## Type Model

The model enforces the normalized structural-bound invariant without duplicate concrete datatypes:

- Root-level variables are controlled by `v`.
- `STVar` is only constructible for `TopVarAllowed`.
- Structural constructors recurse into `SrcTy n TopVarAllowed`.
- `STForall` binder bounds use a stage-specific structural wrapper (`SrcBound n`) so the old `Nothing` ambiguity is avoided while keeping one `SrcTy` family.
- `StructBound` becomes a type alias to normalized, top-var-disallowed `SrcTy`.

Public aliases:

- `type SrcType = SrcTy 'RawN 'TopVarAllowed`
- `type NormSrcType = SrcTy 'NormN 'TopVarAllowed`
- `type StructBound = SrcTy 'NormN 'TopVarDisallowed`
- `type RawSrcType = SrcType`

## Frontend Pipeline Flow

Raw parse stays explicit (`parseRaw*`) and produces `SrcType`/raw expressions. Normalized parse stays explicit (`parseNorm*`) and runs `normalizeType` / `normalizeExpr` before returning normalized aliases.

Normalization remains the single boundary where alias bounds are inlined capture-safely. Phase 1 (`generateConstraints`) and desugaring stay normalized-only by type. No implicit normalization is introduced inside Phase 1.

## Pretty Strategy

Replace raw-only printers with staged/generic signatures:

- `prettyEmlfType :: SrcTy n v -> String`
- `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`

Implementation keeps one rendering core parameterized by stage/policy indices. Existing raw text formatting is preserved to avoid behavioral drift. Normalized/staged users get direct pretty support with no ad hoc conversion wrappers.

## Migration Scope

Primary modules:

- `src/MLF/Frontend/Syntax.hs`
- `src/MLF/Frontend/Normalize.hs`
- `src/MLF/Frontend/Parse.hs`
- `src/MLF/Frontend/Pretty.hs`
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
- public exports in `src-public/MLF/API.hs` and `src-public/MLF/Pipeline.hs`
- affected tests under `test/`

This is a clean break from the concrete split model. Compatibility wrappers are not retained beyond type aliases.

## Error Handling and Invariants

- `SelfBoundVariable` behavior stays deterministic.
- Structural bound invariants remain enforced by type construction, not runtime checks.
- Existing translatability and presolution behavior is unchanged by this refactor (this is a frontend model and API-shape correction).

## Testing Plan (Acceptance)

1. Update parse/normalize/pretty specs to compile and pass with `SrcTy` aliases.
2. Add/adjust pretty tests for normalized/staged entrypoints.
3. Keep bounded-alias normalization and alpha-capture tests passing.
4. Run full gate: `cabal build all && cabal test`.

## Documentation and Tracking

Update in same iteration:

- `implementation_notes.md` (replace concrete-split narrative with strict indexed model)
- task progress notes under `tasks/todo/2026-02-08-staged-src-types-structural-raise-merge/`
- `CHANGELOG.md`
- `Bugs.md` if any status changes are warranted by this correction.
