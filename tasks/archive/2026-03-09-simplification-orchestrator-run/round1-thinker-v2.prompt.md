You are the Thinker agent for round 1 of a thesis-exact simplification campaign in `/Volumes/src/mlf4`.

Before acting, read:
- `/Users/ares/.codex/superpowers/skills/using-superpowers/SKILL.md`
- `/Users/ares/.codex/superpowers/skills/brainstorming/SKILL.md`
- `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, `Bugs.md`
- `papers/these-finale-english.txt` (primary), `papers/xmlf.txt` only if needed

Use this observed repo context as a starting point, then verify it yourself:
- `src/MLF/Binding/Validation.hs` currently defines local `lookupBindParent`, `isBindingRoot`, and `bindingRoots` helpers.
- `src/MLF/Binding/Queries.hs` already owns `lookupBindParent`, `isBindingRoot`, and `bindingRoots` for `Constraint` binding-tree queries.
- `src/MLF/Constraint/NodeAccess.hs` also exposes `lookupBindParent` and related constraint queries.

Your task:
- Propose exactly one bounded simplification idea if this overlap is still a worthwhile, thesis-safe cleanup.
- If this overlap is not the right candidate, choose one better bounded simplification instead.
- Keep the idea implementation-sized for one focused round.
- Do not edit files.

Output must satisfy the provided JSON schema exactly.
