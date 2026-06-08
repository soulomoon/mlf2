### Merge
- Branch commit: `de76b3c0`
- Master squash commit: `46d84487`

### Verification
- Reviewer approved `round-346` with no retry target.
- `git diff --check`: pass.
- Static bounded annotated lambda RHS helper, call-site, alias-removal, shortcut, and overclaim guards: pass.
- Targeted Hspec for the new static guard: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `73 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
