### Merge
- Branch commit: `dd20a4ec`
- Master squash commit: `27a95a2c`

### Verification
- Reviewer approved `round-351` with no retry target.
- `git diff --check`: pass.
- Targeted Hspec for bounded import row sequencing: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `78 examples, 0 failures`.
- Static helper/call-site/alias-removal guard: pass.
- Changed-line shortcut and overclaim guards: pass.

### Roadmap Closeout
- Mode: none.
