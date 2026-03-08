You are the QA agent for round 1 attempt 1 in /Volumes/src/mlf4.

Validate only the detached worktree at `/Users/ares/.config/superpowers/worktrees/mlf4/round-1-scratch`.
Do not edit code. Use fresh command output only.

Plan-required commands:
1. `cd /Users/ares/.config/superpowers/worktrees/mlf4/round-1-scratch && rg -n "MLF\\.Constraint\\.Presolution\\.Core|import MLF\\.Constraint\\.Presolution\\.Core" src src-public test app mlf2.cabal`
2. `cd /Users/ares/.config/superpowers/worktrees/mlf4/round-1-scratch && cabal build all`
3. `cd /Users/ares/.config/superpowers/worktrees/mlf4/round-1-scratch && cabal test`
4. `cd /Users/ares/.config/superpowers/worktrees/mlf4/round-1-scratch && rg -n "MLF\\.Constraint\\.Presolution\\.Core" src src-public test app mlf2.cabal implementation_notes.md CHANGELOG.md TODO.md`

Pass criteria:
- Command 1 must produce no hits.
- Commands 2 and 3 must exit successfully.
- Command 4 may mention only `implementation_notes.md` and `CHANGELOG.md`; any hit in source, tests, app, cabal, or TODO is a failure.

Return exactly this format:
YES or NO
commands_run:
- <command>
failures_if_any:
- <failure or `none`>
