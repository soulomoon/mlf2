### Merge
- Branch commit: `e7ce1526`
- Master squash commit: recorded by the repository commit that adds this merge artifact

### Verification
- Reviewer approved `round-355` with no retry target.
- `git diff --check`: pass.
- Focused WitnessNorm regression: pass.
- Focused recursive class/instance method-row parser gate: pass.
- Focused recursive method-row static guard: pass.
- Focused delayed top-level recursion regression: pass.
- Focused constructor-pattern field mismatch regression: pass.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `84 examples, 0 failures`.
- Standard full Cabal gate: `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`, pass with `2734 examples, 0 failures`.
- Thesis conformance gate: `./scripts/thesis-conformance-gate.sh`, pass.
- Static helper/call-site/alias-removal and changed-line shortcut/overclaim guards: pass.

### Roadmap Closeout
- Mode: none.
