### Merge
- Branch commit: `afc1ebf6b5742d7ced0a1d3d11242308f1f877cd`
- Master squash commit: `7fbf87a60c927b26fa6afe962861c483e19c3d7e`

### Verification
- Reviewer approved `round-342` with no retry target.
- `git diff --check`: pass.
- Static bounded helper, call-site, alias-removal, shortcut, and overclaim guards: pass.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `69 examples, 0 failures`.

### Roadmap Closeout
- Mode: none.
