### Merge
- Branch commit: `513c734e`
- Master squash commit: `474846c1`

### Verification
- Reviewer approved `round-349` with no retry target.
- `git diff --check`: pass.
- Targeted Hspec for bounded source-definition row sequencing: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `76 examples, 0 failures`.
- Static helper/call-site/alias-removal guard: pass.
- Changed-line shortcut and overclaim guards: pass.

### Roadmap Closeout
- Mode: none.
