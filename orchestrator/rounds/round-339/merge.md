### Merge
- Branch commit: `70084eb037e4c1efdb5670147da881e205d5f655`
- Master squash commit: `dae842513966b3a8e41daa9369f0d7c766af8715`

### Verification
- Reviewer approved `round-339` with no retry target.
- `git diff --check`: pass.
- SeedLexer source-copy byte check: pass.
- Direct SeedLexer parser root output matched `test/conformance/mlfp/parser-parity/compiler-seed-lexer/expected/parser-program.txt` and contained no `parser-error`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `67 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
