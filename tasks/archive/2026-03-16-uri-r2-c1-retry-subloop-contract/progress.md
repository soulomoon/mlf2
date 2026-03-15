# Progress

- 2026-03-16: Reviewed `AGENTS.md`, `tasks/readme`, `orchestrator/state.json`, `orchestrator/roadmap.md`, `orchestrator/verification.md`, and the live role prompts to find the current single-shot assumptions.
- 2026-03-16: Read the approved prototype-evidence roadmap design and confirmed the current hard-coded three-attempt cap and v1 reviewer-record assumptions.
- 2026-03-16: Created the active task folder `tasks/todo/2026-03-16-uri-r2-c1-retry-subloop-contract/` and seeded `task_plan.md`, `findings.md`, and `progress.md`.
- 2026-03-16: Drafted the v2 retry-subloop design: `P1` through `P3` may retry up to `100` times inside the same round, `P4` remains aggregate-only, and reviewer output now separates `attempt_verdict` from `stage_action`.
- 2026-03-16: Added the v2 amendment spec and repo-local operational retry contract, then updated `orchestrator/state.json`, `orchestrator/verification.md`, `orchestrator/roadmap.md`, `orchestrator/roles/*.md`, `AGENTS.md`, `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md` to point at the new control-plane model.
- 2026-03-16: Verified the repo-local contract patch with `python3 -m json.tool orchestrator/state.json`, `git diff --check`, roadmap marker parsing, presence checks for the new docs, and a repo-wide `rg` sweep for the new retry vocabulary.
- 2026-03-16: Recorded the remaining boundary that the shared `run-orchestrator-loop` skill still uses older generic controller references outside the repo; the live repo contract is authoritative, but the global skill would need a separate update if automatic controller behavior must match v2 exactly.
