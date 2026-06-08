### Merge
- Branch commit: `139a77b6`
- Master squash commit: `7745624f`

### Verification
- Reviewer approved `round-347` with no retry target.
- `git diff --check`: pass.
- Static bounded source-type arrow-tail helper, call-site, alias-removal, shortcut, and overclaim guards: pass.
- Targeted Hspec for the new static guard: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `74 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
