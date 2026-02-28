# Task Plan: WitnessNorm C2 Analysis (strict replay cutover)

## Goal
Analyze `activeSourceEntries` / `replayBinders` / `targetKeysUsed` behavior in `/Volumes/src/mlf4-strict-replay-cutover/src/MLF/Constraint/Presolution/WitnessNorm.hs` and propose a minimal patch that preserves non-polymorphic behavior while restoring polymorphic binder args.

## Scope
- No code edits in this repository.
- Produce precise patch guidance only.

## Phases
1. completed - Gather target code and related call paths.
2. completed - Trace data flow for binder args and codomain checks.
3. completed - Identify root cause for C2 failing while BUG-002/BUG-003 pass.
4. completed - Draft minimal patch guidance with rationale and verification hints.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None yet | 0 | N/A |
