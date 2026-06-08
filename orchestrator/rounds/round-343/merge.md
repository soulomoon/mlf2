### Merge
- Branch commit: `56d29aafd580d5ce5a48481a86891bbceb8b7189`
- Master squash commit: `075bf0e2f63263b241c5ca5a03d360137516ef55`

### Verification
- Reviewer approved `round-343` with no retry target.
- `git diff --check`: pass.
- Focused static Hspec, parser-library static guard, shortcut, and overclaim guards: pass.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `70 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
