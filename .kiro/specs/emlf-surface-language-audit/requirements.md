# Requirements Document

## Introduction
Source of truth: `papers/these-finale-english.txt`. Scope is the eMLF surface term grammar and annotation syntax, as described in Sec 1.6 (expressions) and Sec 12.3.2 (type annotations). Out of scope: constraint solving, presolution, elaboration, and xMLF.

## Requirements

### Requirement 1: eMLF term grammar alignment
**User Story:** As a maintainer, I want the surface term grammar to match the thesis so that auditing is straightforward.

#### Acceptance Criteria
1. WHEN using the annotated term grammar (paper lines 885-895) THEN THE SYSTEM SHALL provide AST constructors for `x`, `lambda`, `annotated lambda`, `application`, `let`, and `term annotation`. (Status: present; Evidence: `src/MLF/Frontend/Syntax.hs:160-168`)
2. IF extra term forms exist (`ELetAnn`, `ELit`, `EVarRaw`) THEN THE SYSTEM SHALL document them as sugar or internal-only OR desugar them before constraint generation. (Status: extra; Evidence: `src/MLF/Frontend/Syntax.hs:162-169`)
3. WHEN annotated lambdas or term annotations are used THEN THE SYSTEM SHALL align with coercion-function desugaring described in Sec 12.3.2 (paper lines 12635-12644) OR document why explicit annotations are kept. (Status: renamed/merged; Evidence: `src/MLF/Frontend/Desugar.hs:10-20`)

### Requirement 2: Annotation syntax alignment (kappa annotations)
**User Story:** As an auditor, I want annotation syntax to map cleanly to the thesis so I can verify expressivity.

#### Acceptance Criteria
1. WHEN annotations are written THEN THE SYSTEM SHALL support bounded `forall` with optional bottom bounds. (Status: present; Evidence: `src/MLF/Frontend/Syntax.hs:87-93`)
2. WHEN annotations require existential components (kappa pseudo-type in Sec 12.3.2.1) THEN THE SYSTEM SHALL represent them explicitly OR document the mapping to free type variables. (Status: partial; Evidence: `test/ConstraintGenSpec.hs:327-336`)
3. WHERE let annotations are provided THEN THE SYSTEM SHALL treat them as syntax sugar for let + annotation and document this deviation from the paper grammar. (Status: extra; Evidence: `src/MLF/Frontend/Syntax.hs:166-168` and `src/MLF/Frontend/Syntax.hs:121-138`)
