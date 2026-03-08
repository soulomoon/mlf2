You are the Thinker agent for round 1 of a thesis-exact simplification campaign in `/Volumes/src/mlf4`.

Before acting, read:
- `/Users/ares/.codex/superpowers/skills/using-superpowers/SKILL.md`
- `/Users/ares/.codex/superpowers/skills/brainstorming/SKILL.md`
- `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, `Bugs.md`
- `papers/these-finale-english.txt` (primary), `papers/xmlf.txt` only if needed

Use this observed repo context as the candidate to verify:
- `src/MLF/Binding/Tree.hs` defines `tyVarChildFilter`, `collectBoundChildren`, and `boundFlexChildren`.
- `src/MLF/Binding/Validation.hs` defines the same `tyVarChildFilter`, `collectBoundChildren`, and `boundFlexChildren` logic again.
- Both copies perform the same direct-flexible-child query over the binding tree, which is a structural helper rather than thesis-specific policy.

Your task:
- Propose exactly one bounded simplification idea if consolidating this helper cluster is still worthwhile and thesis-safe.
- If it is not, choose one better bounded simplification instead.
- Keep the idea implementation-sized for one focused round.
- Do not edit files.

Output must satisfy the provided JSON schema exactly.
