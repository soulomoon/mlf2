### Merge
- Branch commit: `f6dce9e6`
- Master squash commit: `f53546d4`

### Verification
- Reviewer approved `round-345` with no retry target.
- `git diff --check`: pass.
- Static nested parenthesized application helper, call-site, alias-removal, shortcut, and overclaim guards: pass.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `72 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
