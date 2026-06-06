### Merge
- Round id: round-336
- Branch: orchestrator/round-336-next-parser-parity-slice
- Round commit: 3ab5dcca
- Base branch: master
- Merge mode: squash

### Controller Finalization
- Review decision: APPROVED
- Roadmap Closeout mode: none
- State update: removed round-336 from `active_rounds`
- Semantic roadmap update: none
- User stop request: none

### Verification Summary
- `git diff --check`: pass
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: pass, 56 examples, 0 failures
- Four exact-source `cmp -s` checks against `test/programs/unified/authoritative-*.mlfp`: pass
