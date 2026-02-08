# PRD: Staged Src Types and Structural RaiseMerge Gating

## 0. Clarifying Decisions (Locked)

These decisions were explicitly resolved before drafting this PRD:

1. **Type refactor scope:** Public staged API (not internal-only)
2. **Parser behavior:** Parse Raw AST and provide explicit normalization API
3. **Phase 1 contract:** Normalized-only input at type level
4. **Migration mode:** Clean break now (no compatibility wrappers)

---

## 1. Introduction / Overview

This feature introduces a type-level staged frontend syntax that distinguishes:

- raw source annotations that may still contain alias bounds (e.g. `∀(b ⩾ a) ...`), and
- normalized annotations where alias bounds are impossible to represent.

In parallel, presolution witness gating for `Merge` / `RaiseMerge` is refactored to use only **live structural graph facts** (bounds, binding tree, interior), rather than precomputed alias-bound metadata captured earlier in the pipeline.

This addresses the current paper-faithfulness gap where bounded aliasing paths fail to elaborate as expected (`∀a. a -> a -> a`) and aligns implementation with the thesis direction that graphic constraints should not require alias-bound syntax representation.

Primary references:

- `papers/these-finale-english.txt` (source of truth)
- `papers/xmlf.txt` (supplement when thesis is silent)

---

## 2. Goals

1. Make alias-bound syntax unrepresentable after normalization at the type level.
2. Enforce normalized-only inputs for constraint generation and pipeline elaboration.
3. Refactor `RaiseMerge` gating to depend on structural constraint state only.
4. Preserve existing translatability/rigidity invariants in witness normalization.
5. Make bounded aliasing baseline pass in both unchecked and checked elaboration paths.
6. Keep the design maintainable with strong, expressive Haskell types and explicit boundaries.

---

## 3. User Stories

### US-001: Introduce staged frontend type syntax
**Description:** As a compiler engineer, I want staged frontend type constructors so that normalization invariants are encoded in the type system.

**Acceptance Criteria:**
- [ ] `SrcType` is replaced by staged type constructors indexed by normalization stage and top-variable policy.
- [ ] Aliases are provided for raw and normalized public forms.
- [ ] Bound types in normalized stage cannot represent top-level variable aliases by construction.
- [ ] Project compiles after syntax module updates.

### US-002: Stage frontend expression syntax
**Description:** As a compiler engineer, I want expression AST stages to track raw vs normalized annotation payloads so that APIs enforce correct phase ordering.

**Acceptance Criteria:**
- [ ] `Expr` stage indices are extended to distinguish raw/normalized where annotation payloads are present.
- [ ] Coercion constructor payload type is updated accordingly.
- [ ] Public `SurfaceExprRaw` and `SurfaceExprNorm` (or equivalent names) are exported.
- [ ] Existing internal modules compile with updated signatures.

### US-003: Implement raw-to-normalized type normalization
**Description:** As a compiler engineer, I want a dedicated normalization pass so that alias bounds are inlined before graph generation.

**Acceptance Criteria:**
- [ ] New module provides `normalizeType` and `normalizeExpr` (or equivalent) APIs.
- [ ] Alias bounds `∀(b ⩾ a)` are inlined capture-safely (with alpha-renaming when needed).
- [ ] Invalid self-reference forms continue to produce deterministic errors.
- [ ] Hspec tests cover at least one alpha-capture avoidance case.

### US-004: Split parse API into raw and normalized entrypoints
**Description:** As an API consumer, I want explicit parse targets so that I can choose raw AST inspection or normalized compiler-ready AST.

**Acceptance Criteria:**
- [ ] Parser exports raw parse functions for expressions and types.
- [ ] Parser exports normalized parse functions that run normalization.
- [ ] Parse error rendering behavior remains available.
- [ ] Existing parser tests are migrated or duplicated for both modes where relevant.

### US-005: Make Phase 1 normalized-only
**Description:** As a compiler engineer, I want constraint generation to accept only normalized surface syntax so that alias elimination is guaranteed before graph construction.

**Acceptance Criteria:**
- [ ] `generateConstraints` signature accepts normalized surface expressions only.
- [ ] No implicit normalization happens inside Phase 1.
- [ ] Desugaring consumes normalized surface syntax and produces normalized core syntax.
- [ ] All callsites are updated to normalize before Phase 1.

### US-006: Make pipeline normalized-only
**Description:** As an API consumer, I want the elaboration pipeline to require normalized syntax so that frontend phase boundaries are explicit and type-safe.

**Acceptance Criteria:**
- [ ] `runPipelineElab*` APIs are updated to normalized input types.
- [ ] Public `MLF.API` / `MLF.Pipeline` exports reflect the new normalized contract.
- [ ] Repo tests and internal helpers compile with updated pipeline signatures.

### US-007: Remove alias-special coercion internalization branch
**Description:** As a compiler engineer, I want coercion internalization to rely on normalized inputs so that alias-specific fallback code is removed.

**Acceptance Criteria:**
- [ ] Alias-bound specific branch under `STForall` coercion internalization is removed or made unreachable with invariant checks.
- [ ] Existing well-formedness checks that remain structurally necessary are preserved.
- [ ] Constraint generation tests continue passing with staged normalized input.

### US-008: Remove precomputed binder-bound gating state
**Description:** As a presolution maintainer, I want edge unification state to avoid stale bound snapshots so that merge gating reflects current graph truth.

**Acceptance Criteria:**
- [ ] `eusBinderBounds` is removed from edge unification state.
- [ ] `initEdgeUnifyState` no longer takes binder-bound map input.
- [ ] `runExpansionUnify` no longer constructs and passes binder-bound snapshots.
- [ ] Presolution compiles and existing edge-unify tests are adjusted.

### US-009: Implement structural `shouldRecordRaiseMerge`
**Description:** As a presolution maintainer, I want RaiseMerge gating to query live graph structure so that behavior is independent from alias syntax survival.

**Acceptance Criteria:**
- [ ] `shouldRecordRaiseMerge` looks up current binder bound from canonical graph state.
- [ ] Decision uses only: binder bound, node kind, edge-root ancestry, interior membership, elimination state.
- [ ] Existing exclusions (no bound, same root, eliminated binder) remain enforced.
- [ ] Merge/RaiseMerge-focused specs pass with updated logic.

### US-010: Preserve witness normalization/translatability invariants
**Description:** As a type-soundness maintainer, I want strict witness invariants unchanged while changing gating sources.

**Acceptance Criteria:**
- [ ] `OpRaise; OpMerge -> OpRaiseMerge` normalization behavior is unchanged.
- [ ] Rigid-endpoint translatability checks remain unchanged in behavior.
- [ ] Existing witness canon/validation tests pass without expectation loosening.

### US-011: Restore bounded aliasing end-to-end baseline
**Description:** As a user of thesis-faithful elaboration, I want bounded aliasing example programs to elaborate to expected principal shape.

**Acceptance Criteria:**
- [ ] Existing bounded aliasing baseline test in `test/ElaborationSpec.hs` passes for `runPipelineElab`.
- [ ] Same baseline passes for `runPipelineElabChecked`.
- [ ] Output type alpha-equivalent to `∀a. a -> a -> a`.

### US-012: Update docs and project tracking
**Description:** As a project maintainer, I want docs and bug tracking updated so design intent and status remain auditable.

**Acceptance Criteria:**
- [ ] `implementation_notes.md` documents staged frontend model and structural gating rule.
- [ ] `CHANGELOG.md` includes concise entry for this feature.
- [ ] `Bugs.md` updates status of bounded aliasing bug with linked regression tests.
- [ ] `TODO.md` is updated if priorities changed.

---

## 4. Functional Requirements

- **FR-1:** The frontend type AST must represent normalization stage at the type level.
- **FR-2:** The normalized frontend type AST must make alias bounds unrepresentable.
- **FR-3:** The frontend expression AST must carry staged annotation payloads consistently.
- **FR-4:** The parser must provide raw parse entrypoints for both types and expressions.
- **FR-5:** The parser must provide normalized parse entrypoints that run normalization deterministically.
- **FR-6:** Phase 1 constraint generation must accept normalized expressions only.
- **FR-7:** Desugaring must operate on normalized surface expressions only.
- **FR-8:** Public pipeline elaboration entrypoints must accept normalized expressions only.
- **FR-9:** Coercion type internalization must not depend on alias-bound syntax branches.
- **FR-10:** Edge unification state must not store precomputed binder-bound snapshots for RaiseMerge gating.
- **FR-11:** `shouldRecordRaiseMerge` must compute decisions from live canonical graph and binding tree state.
- **FR-12:** RaiseMerge gating must remain disabled for unbounded binders.
- **FR-13:** RaiseMerge gating must remain disabled when binder-bound root equals external root.
- **FR-14:** Witness normalization and validation semantics must remain unchanged for existing rigid/translatability checks.
- **FR-15:** End-to-end bounded aliasing baseline must pass in both checked and unchecked pipelines.
- **FR-16:** Build and test must pass with `cabal build all && cabal test`.

---

## 5. Non-Goals (Out of Scope)

- Reworking elaboration algorithm outside the bounded aliasing and structural gating scope.
- Introducing compatibility wrappers or deprecation layers for old unstaged frontend APIs.
- Changing xMLF runtime semantics or reduction behavior.
- UI/tooling enhancements unrelated to compiler pipeline correctness.
- Large parser grammar redesign beyond staged API and normalization integration.

---

## 6. Design Considerations

- Follow existing indexed-recursion style used in `MLF.Types.Elab` (`TyIF`, indexed functor folds) to keep frontend syntax design consistent with established architecture.
- Keep phase boundaries explicit:
  - Raw parser output for diagnostics/analysis,
  - explicit normalization step,
  - normalized-only compiler phases.
- Prefer total transformations and explicit error channels (`Either`) for normalization.
- Preserve explicit module export lists and avoid broad implicit exports.

---

## 7. Technical Considerations

### 7.1 Target modules (expected)

- Frontend syntax and APIs:
  - `src/MLF/Frontend/Syntax.hs`
  - `src/MLF/Frontend/Parse.hs`
  - `src/MLF/Frontend/Pretty.hs`
  - `src-public/MLF/API.hs`
  - `src-public/MLF/Pipeline.hs`

- Normalization + Phase 1:
  - `src/MLF/Frontend/Normalize.hs` (new)
  - `src/MLF/Frontend/Desugar.hs`
  - `src/MLF/Frontend/ConstraintGen.hs`
  - `src/MLF/Frontend/ConstraintGen/Translate.hs`

- Pipeline:
  - `src/MLF/Elab/Run/Pipeline.hs`

- Presolution structural gating:
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`
  - `src/MLF/Constraint/Presolution/EdgeUnify.hs`

- Tests:
  - `test/FrontendParseSpec.hs`
  - `test/FrontendPrettySpec.hs`
  - `test/ConstraintGenSpec.hs`
  - `test/Presolution/MergeEmissionSpec.hs`
  - `test/Presolution/RaiseSpec.hs`
  - `test/ElaborationSpec.hs`
  - plus new normalization spec module.

### 7.2 Migration constraints

- `-Wall` is enabled; all signatures and imports must stay warning-free.
- Many tests and helper modules import `SrcType(..)` and `SurfaceExpr`; expect broad but mechanical updates.
- Public API break is intentional; all internal callsites must be migrated in same change.

### 7.3 Correctness constraints

- Normalization must be capture-safe.
- Structural gating must use canonical nodes and current binding graph, never stale snapshots.
- Existing rigid/translatability checks are invariants, not optional behavior.

### 7.4 Documentation constraints

- Reflect thesis alignment changes in `implementation_notes.md`.
- Update bug tracker state only when regression tests prove the fix.

---

## 8. Success Metrics

1. **Type-level invariant metric:** No value of normalized frontend type can encode alias-bound form at compile time.
2. **API contract metric:** Phase 1 and pipeline public APIs reject raw expression input types at compile time.
3. **Behavior metric:** Bounded aliasing baseline in elaboration passes in both checked and unchecked modes.
4. **Regression metric:** Presolution merge/raise and witness normalization suites pass without weakened assertions.
5. **Quality metric:** Full repository validation passes with `cabal build all && cabal test`.

---

## 9. Test Plan

### 9.1 Unit tests

- Normalization unit tests for:
  - simple alias inline,
  - nested alias inline,
  - alpha-renaming capture avoidance,
  - invalid self-bound behavior.

### 9.2 API/parse tests

- Raw parse returns raw staged AST.
- Normalized parse runs normalization and returns normalized staged AST.
- Pretty/parse roundtrip remains stable for normalized syntax.

### 9.3 Constraint generation tests

- No alias-survival shape in normalized input paths.
- Existing coercion and bounded quantification tests continue passing.

### 9.4 Presolution tests

- `MergeEmissionSpec` and `RaiseSpec` verify structural gating behavior with no binder-bound snapshot usage.
- Unbounded binder remains graft/weaken path (no RaiseMerge).

### 9.5 End-to-end elaboration tests

- Existing bounded aliasing baseline (currently failing) becomes passing in:
  - `runPipelineElab`
  - `runPipelineElabChecked`

### 9.6 Full validation

- `cabal build all && cabal test`

---

## 10. Risks and Mitigations

### Risk R-1: Large API break causes broad compile churn
- **Mitigation:** Sequence migration mechanically by module layer (syntax -> parse/pretty -> constraintgen -> pipeline -> tests).

### Risk R-2: Capture bugs in alias normalization
- **Mitigation:** Add focused alpha-capture unit tests and avoid ad-hoc substitution logic without binder-aware renaming.

### Risk R-3: Structural gating changes regress witness behavior
- **Mitigation:** Keep existing witness normalization and validation tests unchanged and green before/after refactor.

### Risk R-4: Bounded alias baseline still fails for reasons beyond gating
- **Mitigation:** Keep the baseline as hard acceptance criterion and use targeted trace assertions in presolution tests.

---

## 11. Rollout and Sequencing

1. Implement staged frontend types and expression stages.
2. Add normalization module and normalized parser entrypoints.
3. Migrate desugar + constraint generation + pipeline to normalized-only.
4. Remove alias-special coercion branch.
5. Refactor structural RaiseMerge gating (drop snapshot field).
6. Migrate and expand tests.
7. Run full validation.
8. Update docs/changelog/bug tracker.

---

## 12. Resolved Decisions

1. Raw parser output remains part of the long-term stable public API.
2. Normalization diagnostics remain structural-only for now.
3. External API split is explicit:
   raw parser APIs may return raw syntax,
   while all APIs that generate graphic constraints must take normalized syntax only.

---

## 13. Definition of Done

This feature is done when all of the following are true:

- All functional requirements FR-1 through FR-16 are satisfied.
- All listed acceptance criteria in US-001 through US-012 are checked off.
- Full validation command passes: `cabal build all && cabal test`.
- Documentation and bug tracker updates are committed in the same change.
