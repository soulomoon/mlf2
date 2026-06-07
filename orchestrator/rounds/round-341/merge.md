### Merge
- Branch commit: `47102af1fde6da8ef2a88e035140ad82edd21634`
- Master squash commit: `1963985662ef9dd8a15a1f13a78ca539ac5ac43a`

### Verification
- Reviewer approved `round-341` with no retry target.
- `git diff --check`: pass.
- Static substrate, alias-removal, shortcut, and overclaim guards: pass.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `68 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
