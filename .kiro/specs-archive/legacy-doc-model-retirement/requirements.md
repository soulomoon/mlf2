# Requirements Document

## Introduction
The repo has migrated from the legacy level-tree scope model (`GNode`/`cGNodes`/`tnVarLevel`/`RankAdjustment`/`gBinds`) to the paper-faithful binding-edge model:

- scope via `Constraint.cBindParents` (binding edges + `BindFlag`)
- variable bounds via `Constraint.cVarBounds` (`MLF.VarStore`)
- eliminated binders via `Constraint.cEliminatedVars` (`MLF.VarStore`)

Several top-level docs and a few comments still describe the removed legacy model. This spec updates/removes those artifacts so repo documentation matches the current implementation and paper alignment work.

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want repo documentation to describe the binding-edge scope model, so contributors are not misled by stale references to removed `GNode`/level-tree machinery.

#### Acceptance Criteria
1. WHEN searching `implementation_notes.md` THEN THE SYSTEM SHALL not mention the legacy scope model identifiers (`tnVarLevel`, `cGNodes`, `gBinds`, `RankAdjustment`, `GNodeId`, `GNode`).
2. WHEN searching `paper_general_raise_plan.txt` THEN THE SYSTEM SHALL not mention the legacy scope model identifiers (`tnVarLevel`, `cGNodes`, `gBinds`, `RankAdjustment`, `GNodeId`, `GNode`).
3. WHEN searching `roadmap.md` and `incompatibility_report.md` THEN THE SYSTEM SHALL not mention the legacy scope model identifiers (`tnVarLevel`, `cGNodes`, `gBinds`, `RankAdjustment`, `GNodeId`, `GNode`).

### Requirement 2
**User Story:** As a maintainer, I want obsolete plan files removed once superseded, so the repo does not retain contradictory scope-model documentation.

#### Acceptance Criteria
1. WHEN checking the workspace THEN THE SYSTEM SHALL not contain `scope_tracking_redesign_plan.txt`.
2. WHEN checking the workspace THEN THE SYSTEM SHALL not contain `phase6.plan`.
3. WHEN searching the repo THEN THE SYSTEM SHALL not reference `scope_tracking_redesign_plan.txt` or `phase6.plan`.

### Requirement 3
**User Story:** As a maintainer, I want code/test comments to match the binding-edge implementation, so readers don’t infer the presence of removed `gBinds` or approximate `I(r)` behavior.

#### Acceptance Criteria
1. WHEN searching `src/MLF/Presolution.hs` THEN THE SYSTEM SHALL not mention copying bounds into `gBinds`.
2. WHEN searching `test/PresolutionSpec.hs` THEN THE SYSTEM SHALL not describe `EdgeTrace.etInterior` as an approximation when binding edges are used.

### Requirement 4
**User Story:** As a maintainer, I want to ensure doc/comment cleanup does not regress behavior, so the suite remains trustworthy.

#### Acceptance Criteria
1. WHEN running `cabal --config-file=.cabal-config test --test-show-details=direct` THEN THE SYSTEM SHALL pass.

