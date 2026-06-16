### Merge
- Branch commit: recorded by the repository commit that adds this merge artifact
- Master squash commit: recorded by the repository commit that adds this merge artifact

### Verification
- Reviewer approved `round-359` with no retry target.
- Roadmap closeout mode: none.
- `git diff --check`: pass.
- Focused platform contract and environment policy tests: pass.
- Static platform environment-policy guard: pass with `round-359 platform environment-policy static guard passed`.
- Full Cabal gate: `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`, pass with 2743 examples, 0 failures.
- Thesis conformance gate: `./scripts/thesis-conformance-gate.sh`, pass.
- Generated `runtime/mlfp_io/target/` artifacts from verification were restored or removed.

### Roadmap Closeout
- Mode: none.
- Status changes: none.
