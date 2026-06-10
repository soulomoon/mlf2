### Merge
- Branch commit: `9dec0acf`
- Master squash commit: `979c9ca5`

### Verification
- Reviewer approved `round-353` with no retry target.
- `git diff --check`: pass.
- Targeted Hspec for parser-value source-span extraction substrate: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `80 examples, 0 failures`.
- Static helper/call-site/duplicate-fallback-removal guard: pass.
- Changed-line shortcut and overclaim guard: pass.

### Roadmap Closeout
- Mode: none.
