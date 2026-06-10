### Merge
- Branch commit: `054e9e36`
- Master squash commit: `b59f17ba`

### Verification
- Reviewer approved `round-354` with no retry target.
- `git diff --check`: pass.
- Targeted Hspec for diagnostic evidence rendering substrate: pass with `1 example, 0 failures`.
- Focused parser parity gate: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`, pass with `81 examples, 0 failures`.
- Static helper/call-site/no-compat-renderer guard: pass.
- Exact label/span/renderSpan source-file comparison: pass.
- Changed-line shortcut and overclaim guard: pass.

### Roadmap Closeout
- Mode: none.
