# Task Plan: Blocker Runtime vs Test Contract Evaluation

## Metadata
- Date: 2026-03-03
- Owner: codex-main
- Scope: independent triage of migration guardrail blocker

## Goal
Determine whether the current blocker (canonical-map mismatch in migration guardrail) should be fixed in runtime code or in test contract, using thesis-faithful semantics as the decision criterion.

## Phases
1. [in_progress] Gather evidence from failing/guard tests and runtime canonical-map construction
2. [pending] Compare observed behavior to thesis-faithful semantic invariants and migration guard intent
3. [pending] Recommend fix location (runtime vs test) and specify exact assertion updates if test contract should change

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
