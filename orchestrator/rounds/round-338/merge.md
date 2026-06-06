### Merge
- Branch commit: `f85b3f555013d959e663436a38a7f4a15639f68f`
- Master squash commit: `7ce96ec84bd6989086d81625e1f47916204533ea`

### Verification
- Reviewer approved `round-338` with no retry target.
- `git diff --check`: pass.
- Source-copy byte checks for `SeedSource`, `SeedToken`, `SeedDiagnostic`, and `SeedAst`: pass.
- Direct compiler-seed parser root output matched `test/conformance/mlfp/parser-parity/compiler-seed-data-model/expected/parser-program.txt` and contained no `parser-error`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `63 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
