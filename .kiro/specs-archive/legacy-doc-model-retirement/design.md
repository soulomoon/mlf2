# Design Document

## Overview
This is a documentation + comment cleanup spec. It removes obsolete scope-model plan files and updates repository docs/comments that still describe the retired `GNode`/level-tree scope model, aligning them with the binding-edge + `cVarBounds`/`cEliminatedVars` implementation.

## Architecture
No architecture changes. This spec only updates Markdown/text and comment blocks.

## Components and Interfaces
- Docs: `implementation_notes.md`, `paper_general_raise_plan.txt`, `roadmap.md`, `incompatibility_report.md`
- Removed obsolete docs: `scope_tracking_redesign_plan.txt`, `phase6.plan`
- Comments: `src/MLF/Presolution.hs`, `test/PresolutionSpec.hs`

## Data Models
No data model changes.

## Error Handling
Not applicable.

## Testing Strategy
- Use `rg`-based checks to verify removed legacy identifiers from the targeted docs/comments.
- Run the full test suite to ensure no accidental regressions.

