# Findings

- The live `URI-R2-C1` prototype-evidence contract is still effectively single-shot per stage: `orchestrator/verification.md` only models approve/reject review outcomes, and the role prompts assume a retry happens only after rejection.
- The approved design source `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md` hard-codes `attempt_id` to `1..3` and a three-attempt cap, so it conflicts directly with the newly chosen `100`-retry subloop.
- The clean way to land the retry model is a forward-only amendment: preserve rounds `round-016` through `round-019` as `contract_version: 1` historical evidence, then move the live control plane to `contract_version: 2` with `retry: null` at idle.
- The retry contract needs two distinct review axes:
  - `attempt_verdict`: whether the current attempt is trustworthy enough to count as evidence;
  - `stage_action`: whether the stage finalizes now or loops again.
- `accepted + retry` is the key new state the old contract cannot express: it records a valid bounded non-pass or fixable result without yet making that attempt authoritative for downstream carry-forward.
- `P4` should remain aggregate-only: it may still bounce on ordinary review rejection, but it must never emit `accepted + retry`.
- The repo-local contract is now patched to v2, but the shared `run-orchestrator-loop` skill outside this repo still carries the older generic controller references. That means the repo is now the source of truth for retry behavior, while a separate follow-up would be needed to teach the global controller skill the same transition rules.
