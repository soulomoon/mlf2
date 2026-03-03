# Task Plan: Phi Replay Bridge Strict Pass-Through

## Metadata
- Date: 2026-02-28
- Plan source: `/Volumes/src/mlf4/docs/plans/2026-03-01-phi-replay-bridge-strict-pass-through-plan.md`
- Branch: `codex/2026-03-01-phi-replay-bridge-design`

## Goal
Execute the strict pass-through replay bridge plan end-to-end: remove runtime fallback projection in `computeTraceBinderReplayBridge`, enforce strict replay-map codomain validation, add regression coverage, and sync docs.

## Phases
- [completed] Phase 1: Add malformed codomain regression test (RED)
- [completed] Phase 2: Implement strict pass-through bridge and remove fallback helpers
- [completed] Phase 3: Verify targeted tests (new codomain + existing domain mismatch)
- [completed] Phase 4: Run full build/test validation and invariant grep checks
- [completed] Phase 5: Sync docs (`CHANGELOG.md`, `implementation_notes.md` if applicable)
- [completed] Phase 6: Commit meaningful checkpoints and final summary

## Decisions
- Execute only the referenced plan scope; do not touch single-solved elaboration plan work.
- Keep presolution/omega behavior read-only unless plan explicitly requires otherwise.
- Existing branch state already contained strict bridge behavior and the new codomain test scaffold; completion work focused on final bridge invariant cleanup (`projectOne` -> `validateTarget`), regression assertion alignment, docs sync, and required gates.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Full gate failed with 2 ElaborationSpec message-prefix assertions after bridge error text cleanup | 1 | Updated two assertions to match the strict invariant substring `"replay-map target outside replay binder domain"`; reran full gate green. |
