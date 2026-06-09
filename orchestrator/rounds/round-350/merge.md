### Merge
- Branch commit: `43e4e6d0`
- Master squash commit: `a816a4cf`

### Verification
- Reviewer approved `round-350` with no retry target.
- `git diff --check`: pass.
- Targeted Hspec for bounded program module row sequencing: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `77 examples, 0 failures`.
- Static helper/call-site/alias-removal guard: pass.
- Changed-line shortcut and overclaim guards: pass.

### Roadmap Closeout
- Mode: none.
