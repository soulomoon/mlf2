# Design Document

## Overview
This is a documentation+tests hygiene spec: it renames “expected failure” paper-alignment baselines that now pass, and updates Kiro specs / plan docs that still claim the test suite contains intentional failures.

## Architecture
No architecture changes. Only Markdown/test description updates.

## Components and Interfaces
- `test/ElaborationSpec.hs`: suite/test names and comments around κσ baselines
- `.kiro/specs/*/tasks.md`: top “Note:” blocks about test status
- `.kiro/specs/scope-model-retirement/requirements.md`: acceptance criteria wording
- `merge_raise_merge_plan.txt`: Phase 8 status section

## Data Models
None.

## Error Handling
None.

## Testing Strategy
- Use `rg` checks for label removal (fast, deterministic).
- Run `cabal --config-file=.cabal-config test --test-show-details=direct` after doc/test-name updates to ensure nothing regresses.

