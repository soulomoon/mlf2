# Requirements Document

## Introduction
The repo currently carries *two* scope models:
- the paper-style binding tree (`Constraint.cBindParents`, `BindFlag`) used for χe interior (`I(r)`), Raise/Weaken execution, and Φ contexts;
- a legacy “level tree” (`Constraint.cGNodes`, `TyVar.tnVarLevel`, `TyForall.tnOwnerLevel/tnQuantLevel`) kept in sync via `MLF.RankAdjustment`.

After the binding-tree migration, the level tree is now mostly **bookkeeping** and is increasingly a source of drift, duplication, and non-paper behavior. `papers/xmlf.txt`’s scope story is binding-edge-based; Raise/Weaken are binding-tree rewrites, and legality (instantiable/restricted/locked) comes from binding flags.

This spec retires the legacy scope model so:
- Raise is represented/executed only via binding edges (not `tnVarLevel` mutation),
- variable bounds and “binder elimination” are no longer stored in `cGNodes`,
- elaboration/generalization does not require `cGNodes` or `tnVarLevel`,
- `MLF.RankAdjustment` can be removed.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want variable instance bounds to be stored directly on variables (or in a dedicated map), so we can remove `cGNodes` without losing bound information.

#### Acceptance Criteria
1. WHEN the system reads or writes a type-variable bound (e.g. during Graft / binder-bound copying) THEN THE SYSTEM SHALL use a dedicated bound store (not `GNode.gBinds`).
2. WHEN presolution drops/eliminates a binder-meta variable THEN THE SYSTEM SHALL persist that elimination in the solved graph without relying on `GNode.gBinds` membership.
3. WHEN running `rg -n \"GNodeOps\\.setVarBoundAtLevel|lookupVarBoundAtLevel\" src` THEN THE SYSTEM SHALL return no matches after the migration is complete.

### Requirement 2
**User Story:** As a solver developer, I want presolution’s χe instantiation logic to stop depending on `GNodeId` levels (`tnVarLevel`, `tnOwnerLevel`, `tnQuantLevel`) for scope decisions, so scope is derived from binding edges like in the paper.

#### Acceptance Criteria
1. WHEN `instantiateSchemeWithTrace` decides copy-vs-share THEN THE SYSTEM SHALL use binding-edge interior `I(g)` only (no `tnVarLevel`/quant-level fallbacks).
2. WHEN presolution chooses the scope for fresh binder-metas (binderMetaAt) THEN THE SYSTEM SHALL derive the binder placement from binding edges (not `tnVarLevel`).
3. WHEN running `rg -n \"tnVarLevel\" src/MLF/Presolution.hs` THEN THE SYSTEM SHALL return no matches in the migrated code paths (allowing transitional helpers only behind a clear TODO marker).

### Requirement 3
**User Story:** As an elaboration implementer, I want type reification and generalization to be derived from binding edges and binding flags, so elaboration remains correct without `cGNodes`/`tnVarLevel`.

#### Acceptance Criteria
1. WHEN elaboration reifies a solved type rooted at `n` THEN THE SYSTEM SHALL order quantifiers using paper ≺ keys (`MLF.Order`) and binding edges (not `GNode.gBinds`).
2. WHEN elaboration generalizes at a binding site THEN THE SYSTEM SHALL choose binders from binding edges (flexible bindings) rather than `tnVarLevel`.
3. WHEN running `rg -n \"cGNodes|tnVarLevel\" src/MLF/Elab.hs` THEN THE SYSTEM SHALL return no matches after the cut-over.

### Requirement 4
**User Story:** As a paper-alignment implementer, I want to remove `MLF.RankAdjustment` and all level-based “harmonize” calls, so raising is purely binding-edge-based.

#### Acceptance Criteria
1. WHEN searching the codebase THEN THE SYSTEM SHALL have no call sites of `RankAdjustment.harmonizeVarLevels` / `harmonizeVarLevelsWithCounts`.
2. WHEN running `cabal test --test-show-details=direct` THEN THE SYSTEM SHALL pass without `RankAdjustment` present.
