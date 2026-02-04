# Design Document

## Overview
This spec defines the thesis alignment backlog. It summarizes coverage of
`papers/these-finale-english.txt` at a high level (breadth over depth), and
links each gap to an existing spec or a proposed new spec. Conflicts with
`papers/xmlf.txt` must be recorded when discovered.

## Status Legend
- Implemented: present in code with tests, no known thesis gaps.
- Partial: present in code but missing thesis details or known deviations.
- Missing: not implemented.
- Doc-only: proof or discussion text not directly implementable; document
  relevant assumptions if needed.

## Inventory (High-Level)

Part I: Graphic types and type instance (Ch 3-8)
- Ch 3-4 (term graphs, binding edges, domination): Partial.
  - Code: `src/MLF/Constraint/Types.hs`, `src/MLF/Binding/Tree.hs`.
  - Gaps: thesis-level definitions of domination and related invariants need
    a direct audit against implementation.
- Ch 7 (unification): Partial.
  - Code: `src/MLF/Constraint/Presolution/Unify.hs`,
    `src/MLF/Constraint/Unify/Decompose.hs`.
  - Gaps: generalized unification (7.6), unification modulo similarity (7.4.6).
- Ch 8 (syntactic and graphic translation, bound inlining): Partial.
  - Code: `src/MLF/Elab/Reify.hs`.
  - Gaps: complete translation coverage and bound-inlining rules.

Part II: Graphic constraints (Ch 9-13)
- Ch 9 (constraint definition, interiors): Partial.
  - Code: `src/MLF/Constraint/Types.hs`, `src/MLF/Constraint/Presolution/Base.hs`.
  - Gap: thesis distinction between Ic and Is requires explicit audit.
- Ch 10 (semantics of constraints): Partial.
  - Code: `src/MLF/Constraint/Presolution/Expansion.hs`,
    `src/MLF/Constraint/Presolution/Copy.hs`.
  - Gaps: flag and binding reset details, degenerate schemes handling.
- Ch 11 (reasoning on constraints): Partial.
  - Code/specs: `src/MLF/Constraint/Presolution/Witness.hs`,
    `.kiro/specs/witness-normalization-yakobowski`,
    `.kiro/specs/omega-elimination-graph-rewrite`.
  - Gaps: any thesis details beyond normalization/elimination need audit.
- Ch 12 (type inference): Partial.
  - Code: `src/MLF/Constraint/Solve.hs`, `src/MLF/Constraint/Acyclicity.hs`.
  - Gaps: simplification rules (12.2, 12.4), ML constraints translation.
- Ch 13 (constraints up to similarity or abstraction): Doc-only / deferred.
  - Thesis note: for typing constraints, inference up to similarity is not needed (Cor. 13.2.5; `papers/these-finale-english.txt:14271-14278`).
  - iMLF does not permit type inference and has no principal presolutions (`papers/these-finale-english.txt:14513-14518`).
  - Audit: `.kiro/specs/constraint-similarity-abstraction/audit.md`.

Part III: Explicit language for MLF (Ch 14-15)
- Ch 14 (xMLF types, typing rules, reduction): Partial.
  - Code: `src/MLF/Elab/Types.hs` (syntax), `src/MLF/Elab/Elaborate.hs`.
  - Gaps: xMLF typechecking and reduction semantics (Phase 7).
- Ch 15 (translation gMLF -> xMLF): Partial.
  - Code: `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Sigma.hs`,
    `.kiro/specs/instantiation-o-interleaving`.
  - Gaps: inert-locked node filtering (15.2.3), leftmost-lowermost order (15.2.4),
    translation contexts and computation contexts (15.3), alternative let scoping
    (15.2.6), optimized propagation witnesses (15.4), eMLF/iMLF translation (15.5).

Part IV: Conclusions (Ch 16-17)
- Doc-only: narrative content, no direct implementation. Document assumptions
  only if required for correctness.

## Backlog Items (Proposed)

P0 (must have for thesis alignment)
- P0/L: Implement xMLF typechecking and reduction semantics (Ch 14.2.3, 14.3).
  - Existing spec: `.kiro/specs/xmlf-phase7-semantics`.
- P0/M: Inert-locked node detection and filtering in propagation witnesses
  (Ch 15.2.3).
  - New spec: `.kiro/specs/inert-locked-filtering`.
- P0/M: Leftmost-lowermost ordering for translation decisions (Ch 15.2.4).
  - New spec: `.kiro/specs/leftmost-lowermost-order`.

P1 (important for full coverage)
- P1/L: Translation contexts and computation contexts in gMLF -> xMLF
  (Ch 15.3.4-15.3.6).
  - New spec: `.kiro/specs/translation-contexts`.
- P1/M: Alternative let scoping and translatable presolutions (Ch 15.2.6-15.2.7).
  - New spec: `.kiro/specs/let-scope-alternative-typing`.
- P1/M: Syntactic <-> graphic translation and bound inlining rules (Ch 8).
  - New spec: `.kiro/specs/syntactic-graphic-translation`.
- P1/L: Constraints up to similarity or abstraction (Ch 13) — doc-only unless implementing iMLF tooling.
  - Spec + audit: `.kiro/specs/constraint-similarity-abstraction/`.
- P1/M: Generalized unification (Ch 7.6).
  - New spec: `.kiro/specs/generalized-unification`.

P2 (follow-up or doc-heavy)
- P2/M: Simplification rules and ML constraint specifics (Ch 12.2, 12.4).
  - New spec: `.kiro/specs/constraint-simplification-rules`.
- P2/L: eMLF and iMLF translation variants (Ch 13.3, 15.5).
  - New spec: `.kiro/specs/emlf-imlf-translation`.
- P2/S: Document-only notes for proof-only chapters (Ch 14.4-14.6, Ch 16-17).

## Testing Strategy
This spec is validated by checking that the backlog inventory exists and
contains the required status labels and spec references. Follow-on specs define
their own testing strategies.
