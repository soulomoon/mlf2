# Requirements Document

## Introduction

src/MLF/Elab/Generalize.hs has grown large with repeated local helpers, nested go functions, and mixed naming conventions. This spec simplifies the module by extracting reusable helpers, normalizing naming, and using recursion-schemes
for safe structural traversals. The refactor must preserve existing behavior and test coverage. We explicitly avoid algorithmic changes to generalization logic.

## Requirements

### Requirement 1

User Story: As a maintainer, I want shared helpers for repeated graph/path logic so Generalize remains concise and consistent.

#### Acceptance Criteria

1. WHEN a reachability traversal with a stop predicate appears in Generalize, THEN THE SYSTEM SHALL use a shared helper (e.g., in MLF.Elab.Util or a Generalize.Util module).
2. WHEN repeated canonicalization/key extraction logic appears in multiple local helpers, THEN THE SYSTEM SHALL centralize or name it once and reuse it.

### Requirement 2

User Story: As a maintainer, I want Generalize's structure to be easier to read and audit.

#### Acceptance Criteria

1. WHEN Generalize uses nested go/go* helpers for unrelated concerns, THEN THE SYSTEM SHALL replace them with named helpers at the same scope.
2. WHERE variable naming is inconsistent for canonicalized nodes/keys/sets, THE SYSTEM SHALL align naming conventions (*_C, *_Key, *_Set, keyOf, canonical).

### Requirement 3

User Story: As a maintainer, I want structural ElabType traversals to use recursion-schemes when safe.

#### Acceptance Criteria

1. WHEN a traversal is purely structural over ElabType in Generalize, THEN THE SYSTEM SHALL use cata or para instead of explicit recursion.
2. IF a traversal is graph-shaped (uses visited sets / stop sets), THEN THE SYSTEM SHALL keep explicit traversal logic or a shared helper rather than forcing a fold.

### Requirement 4

User Story: As a reviewer, I want confidence that behavior is unchanged.

#### Acceptance Criteria

1. WHEN the refactor is complete, THEN THE SYSTEM SHALL pass cabal test.
