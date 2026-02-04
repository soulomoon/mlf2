# PRD: Coercion‑Only Annotations (Remove Declared‑Scheme Lets)

## Introduction/Overview
The thesis defines surface annotations purely as **syntactic sugar** for applying coercion constants (Chapter 12.3.2):

- `(a : τ) ≜ cτ a`
- `λ(x : τ) a ≜ λ(x) let x = (x : τ) in a` and thus `λ(x) let x = cτ x in a`

This repo currently (or historically) supported an extra, non‑thesis “annotated let” interpretation where `let x = (e : σ) in b` (or its desugared form) is treated as a **declared type scheme** for `x` (old `ELetAnn` behavior).

We will remove that extension and make **all annotation effects flow exclusively through κσ coercions**. In particular, `let x = (e : σ) in b` will mean “bind `x` to the value of `cσ e`”, not “declare `x : σ`”.

## Goals
- Make `EAnn` and `ELamAnn` thesis‑exact by using **only coercion constants** in core (`cτ`).
- Remove the “declared‑scheme let via RHS annotation” behavior (old `ELetAnn` semantics).
- Simplify Phase 1 translation so `ELet` is always a normal let (scheme inferred from RHS), regardless of RHS shape.
- Update tests and notes so they describe and assert the thesis coercion semantics.
- Ensure `cabal build all && cabal test` passes.

## User Stories

### US-001: Remove declared-scheme let interpretation
**Description:** As a type‑system maintainer, I want `let x = (e : σ) in b` to be interpreted as a normal let binding of the coerced term `cσ e`, not as a declared scheme for `x`, so the frontend remains thesis‑faithful.

**Acceptance Criteria:**
- [ ] `MLF.Frontend.ConstraintGen.Translate` no longer special‑cases `ELet` RHS annotations to construct an explicit scheme for `x`.
- [ ] Any helpers used only for declared‑scheme construction are removed (e.g. binder splitting/internalize helpers) or relocated if still needed elsewhere.
- [ ] Notes in `Translate.hs` no longer mention “declared scheme” / “old `ELetAnn` behavior”; they state coercion‑only semantics.
- [ ] `cabal build all` succeeds.
- [ ] `cabal test --test-show-details=direct` succeeds.

### US-002: Make annotated lambdas purely desugar to `ELam` + `ELet` + coercion
**Description:** As a contributor aligning with the thesis, I want `ELamAnn` to desugar exactly to `ELam x (ELet x (cτ x) body)` with no core-only lambda annotation constructor, so Phase 1 never needs to “know about” lambda annotations.

**Acceptance Criteria:**
- [ ] Core AST contains no lambda-annotation constructor (remove `ELamAnnCore`).
- [ ] `MLF.Frontend.Desugar.desugarSurface` implements the thesis sugar: `ELamAnn x τ body → ELam x (ELet x (EApp (ECoerceConst τ) (EVar x)) …)`.
- [ ] `Translate.hs` has no `ELamAnnCore` branch; only `ELam` remains.
- [ ] `cabal build all` succeeds.
- [ ] `cabal test --test-show-details=direct` succeeds.

### US-003: Update tests to reflect coercion-only semantics (and add regression)
**Description:** As a reviewer, I want tests that clearly pin coercion-only semantics so the declared‑scheme behavior cannot regress.

**Acceptance Criteria:**
- [ ] Rename/reword test descriptions that say “annotated let” to “let with RHS term annotation (coercion)”.
- [ ] Update Phase 6 expectations for any tests that depended on declared‑scheme semantics.
- [ ] Add a regression test asserting that `ELet x (EAnn e σ) body` does *not* build the explicit-scheme instantiation edge structure (i.e. no “scheme-exp instantiates to RHS” constraint is introduced).
- [ ] `cabal test --test-show-details=direct` succeeds.

### US-004: Preserve thesis-exact rank-2 annotated lambda behavior
**Description:** As a user, I want `\x : (∀a. a -> a). x 1` to elaborate with thesis‑style flexible bounds (bounded quantification) rather than collapsing directly to `Int`, ensuring rank‑2 annotation behavior stays paper‑faithful.

**Acceptance Criteria:**
- [ ] `test/ElaborationSpec.hs` “elaborates lambda with rank-2 argument” passes with the expected bounded result type (up to α‑equivalence).
- [ ] Any required fixes are documented in a `Note [...]` block near the relevant logic (result-type computation or instantiation/witness translation).
- [ ] `cabal test --test-show-details=direct` succeeds.

## Functional Requirements
- FR-1: `(e : σ)` must be implemented only as `cσ e` (coercion application).
- FR-2: `λ(x : τ) a` must desugar only to `λ(x) let x = cτ x in a`.
- FR-3: `ELet` must not interpret RHS annotations as type declarations.
- FR-4: Constraint generation must continue to treat coercion applications as a special form (`buildCoerce`) so it emits the thesis‑style instantiation edge to the coercion domain and returns the codomain.

## Non‑Goals (Out of Scope)
- Introducing a new surface syntax `let x : σ = e in b`.
- Preserving the old “declared-scheme let” extension semantics.
- Broad refactors of presolution/solve/elaboration unrelated to annotations, except as needed to preserve thesis‑exact rank‑2 behavior.

## Technical Considerations
- Removing declared‑scheme lets will change the meaning of programs that relied on the extension. Tests must be updated accordingly.
- Keep constraint generation paper‑faithful to `papers/these-finale-english.txt` (§12.3.2, §12.5.1) and document any intentional deviations.
- Ensure no new `-Wall` warnings after removing dead helpers.

## Success Metrics
- All tests pass via `cabal build all && cabal test`.
- No references remain in code/comments to “declared scheme” annotation semantics.
- A dedicated regression test prevents reintroducing declared‑scheme let handling.

## Open Questions
- If removing declared‑scheme semantics changes the principal types produced for some “annotated let” tests, should we update those expected types to preserve bounded quantifiers (preferred), or should we add a new explicit surface syntax for declared schemes? (Current default: update tests to match coercion-only semantics.)
