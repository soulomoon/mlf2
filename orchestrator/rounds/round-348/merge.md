### Merge
- Branch commit: `14ebe9ca`
- Master squash commit: `10a83975`

### Verification
- Reviewer approved `round-348` with no retry target.
- `git diff --check`: pass.
- Static constructor-row payload/helper, call-site, alias-removal, shortcut, and overclaim guards: pass.
- Targeted Hspec for the new static guard: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `75 examples, 0 failures`.
- Standard full Cabal gate: `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`, pass with `2722 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
