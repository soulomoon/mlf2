# Findings

## 2026-03-06
- `docs/plans/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md` defines a 10-round outer loop but a 6-attempt inner implementation loop.
- `docs/prompts/improving-loop-agent.prompt.md` also uses a 10-round outer loop and a 6-attempt inner implementation loop.
- Neither source artifact currently requires researcher agents to brief the Planner before planning begins.
- The round-2 plan needed one extra normalization pass to update the sample run-folder path so it matches the new document version/date.
- `docs/prompts/improving-loop-agent.prompt2.md` remained weaker than its companion plan on four points: contradiction resolution across verifier/researchers, retry-delta requirements, explicit no-progress handling, and accept-or-revert hygiene for failed attempt diffs.
- The prompt is stronger when it is standalone: pulling in row-14 YES/NO mapping and anchor-mechanism rules removes hidden dependence on the companion plan.
