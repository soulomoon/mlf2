# Orchestrator Log Format Rationalization Design

## Goal
Standardize orchestrator execution logging on one authoritative format so the workflow is simpler, less redundant, and easier to automate reliably.

## Options Considered

### 1. Keep both `orchestrator-log.md` and `orchestrator-log.jsonl`
Reject. This preserves duplicate truth sources and creates drift risk without adding unique value.

### 2. Make markdown authoritative and derive JSONL secondarily
Reject. The workflow's hard requirements are exact gates, replayability, and deterministic analytics; markdown is a weaker source for those needs.

### 3. Make JSONL authoritative and keep narrative in `findings.md` / `progress.md`
Chosen. JSONL matches the machine-checkable contract, while the existing planning files already provide the human-readable summary surface.

## Design
- Require `orchestrator-log.jsonl` as the only orchestrator event log.
- Remove `orchestrator-log.md` as a required run artifact from the live round-2 plan.
- Update the live prompt text to explicitly name JSONL as the execution log format.
- Update the reusable goal-loop skill, reference template, and scaffold script so newly generated artifacts follow the same contract.
- Leave archived historical run artifacts intact except where an archival note is already appropriate.

## Verification
- Search for live references that still require both `orchestrator-log.md` and `orchestrator-log.jsonl`.
- Confirm the scaffold script defaults to a `.jsonl` round/event log artifact.
- Confirm the prompt/plan/skill language consistently describes JSONL as authoritative and narrative summaries as living in `findings.md` / `progress.md`.
