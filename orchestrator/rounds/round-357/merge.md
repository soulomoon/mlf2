### Merge
- Branch commit: recorded by the repository commit that adds this merge artifact
- Master squash commit: recorded by the repository commit that adds this merge artifact

### Verification
- Reviewer approved `round-357` with no retry target.
- `git diff --check`: pass.
- Focused recursive module-body, constructor-row, method-row, package layout,
  malformed diagnostic, generated public CLI driver, shortcut, and aggregate
  parser-parity checks: pass.
- Static M4 closeout inventory and shortcut guard: pass with 35 required
  phrases and 27 retired/shortcut phrases checked.
- Overclaim and no-source-edit guard: pass after validation artifact cleanup.
- Full Cabal gate: `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`, pass with 2735 examples, 0 failures.
- Thesis conformance gate: `./scripts/thesis-conformance-gate.sh`, pass.

### Roadmap Closeout
- Mode: status-only.
- Status changes: `milestone-4-full-canonical-mlfp-parser-parity` to `[done]`.
