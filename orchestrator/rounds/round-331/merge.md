### Merge
- Round id: round-331
- Branch: orchestrator/round-331-next-parser-parity-slice
- Round commit: 4b3c7573
- Base branch: master
- Merge mode: squash

### Controller Finalization
- Review decision: APPROVED
- Roadmap Closeout mode: none
- State update: removed round-331 from `active_rounds`
- Semantic roadmap update: none

### Verification Summary
- `git diff --check`: pass
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: pass, 36 examples, 0 failures
