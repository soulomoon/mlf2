### Merge
- Round id: round-337
- Branch: orchestrator/round-337-next-parser-parity-slice
- Round commit: aac58b59
- Base branch: master
- Merge mode: squash

### Controller Finalization
- Review decision: APPROVED
- Roadmap Closeout mode: none
- State update: removed round-337 from `active_rounds`
- Semantic roadmap update: none
- User stop request: none

### Verification Summary
- `git diff --check`: pass
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: pass, 60 examples, 0 failures
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program shared conformance corpus"'`: pass, 5 examples, 0 failures
- Byte-for-byte `cmp -s` checks for all copied package sources: pass
