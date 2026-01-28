# Requirements Document

## Introduction
Source of truth: `papers/these-finale-english.txt` (thesis), focusing on Chapter 14 (xMLF syntax, instantiation, typing) and Chapter 15 (translation from presolutions: Typ/Typexp, quantifier reordering ϕR, computation contexts, Φ). Supplementary mapping to existing code is limited to the current repo modules (`src/MLF/*`). Out of scope: proving metatheory, solver complexity claims, and performance tuning.

## Requirements

### Requirement 1 — xMLF syntax completeness
**User Story:** As a maintainer, I want the xMLF type AST to cover all thesis constructors, so that elaboration can represent every xMLF type the paper defines.

#### Acceptance Criteria
1. WHEN the thesis defines constructor types `Cσ` in xMLF (Fig. 14.2.1) THEN the system SHALL provide a corresponding type constructor (or document an explicit deviation). (Status: **missing**; Evidence: `papers/these-finale-english.txt:14616-14645`; `src/MLF/Types/Elab.hs:85-90`.)
2. WHEN the thesis defines bounded type abstractions and type instantiation terms (Fig. 14.2.2) THEN the system SHALL represent `Λ(α ⩾ σ) a` and `a[ϕ]` in the xMLF term AST. (Status: **present**; Evidence: `papers/these-finale-english.txt:14653-14683`; `src/MLF/Types/Elab.hs:271-278`.)

### Requirement 2 — Quantifier reordering ϕR is applied when needed
**User Story:** As a maintainer, I want elaboration to apply the paper’s quantifier reordering computation ϕR when `Typ(a′)` and `Typexp(a′)` differ, so that Φ computations are applied to the correct binder order.

#### Acceptance Criteria
1. WHEN `Typ(a′)` and `Typexp(a′)` differ only by quantifier order (Def. 15.3.4) THEN the system SHALL compute ϕR and compose it with the edge instantiation Φ(e). (Status: **partial**; Evidence: `papers/these-finale-english.txt:17582-17624`; reorder currently triggered only when Raise ops appear via `needsPrec` → `reorderBindersByPrec` in `src/MLF/Elab/Phi.hs:525-536`.)
2. WHEN a quantifier reordering is required THEN the system SHALL produce a deterministic instantiation (σ-reorder) consistent with the <P ordering. (Status: **present but standalone**; Evidence: `src/MLF/Elab/Sigma.hs:81-98`.)

### Requirement 3 — Φ translation must enforce translatable-presolution invariants
**User Story:** As a maintainer, I want Φ translation to only accept normalized instance operations on translatable presolutions (Fig. 15.3.4), so that elaboration fails fast for non-translatable witnesses rather than silently skipping steps.

#### Acceptance Criteria
1. WHEN an instance operation targets a rigid node THEN Φ translation SHALL behave as the identity computation (Fig. 15.3.4). (Status: **present**; Evidence: `papers/these-finale-english.txt:17807-17815`; `src/MLF/Elab/Phi.hs:761-767`.)
2. WHEN an instance operation targets a node outside the expansion interior or without a computation context THEN Φ translation SHALL report a non‑translatable‑presolution error (rather than skipping the op). (Status: **missing/partial**; Evidence: current skip logic at `src/MLF/Elab/Phi.hs:756-759` and missing‑context error at `src/MLF/Elab/Phi.hs:891-894`; thesis requires operations to be on flexibly-bound interior nodes with contexts, `papers/these-finale-english.txt:17757-17760`.)
3. WHEN presolution emits a witness Ω THEN it SHALL be normalized and include Raise/Weaken/Merge/Graft/RaiseMerge operations in the shape expected by Fig. 15.3.4 (or deviations documented). (Status: **partial**; Evidence: normalized Ω is assumed by the thesis, `papers/these-finale-english.txt:17804-17867`; code notes that arbitrary Raise ops are not emitted yet, `src/MLF/Constraint/Types.hs:594-602`.)
