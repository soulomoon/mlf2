### Merge
- Branch commit: recorded by the repository commit that adds this merge artifact
- Master squash commit: recorded by the repository commit that adds this merge artifact

### Verification
- Reviewer approved `round-358` with no retry target.
- Roadmap closeout mode: none.
- `git diff --check`: pass.
- Focused platform contract tests: pass.
- Static platform-contract guard: pass with `round-358 platform contract static guard passed`.
- Full Cabal gate: `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`, pass with 2739 examples, 0 failures.
- Thesis conformance gate: `./scripts/thesis-conformance-gate.sh`, pass.
- Generated `runtime/mlfp_io/target/` artifacts from verification were restored or removed.

### Roadmap Closeout
- Mode: none.
- Status changes: none.
