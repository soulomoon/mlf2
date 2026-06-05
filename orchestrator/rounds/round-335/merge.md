### Merge
- Round id: round-335
- Branch: orchestrator/round-335-next-parser-parity-slice
- Round commit: 63cab09b
- Base branch: master
- Merge mode: squash

### Controller Finalization
- Review decision: APPROVED
- Roadmap Closeout mode: none
- State update: removed round-335 from `active_rounds`
- Semantic roadmap update: none
- User stop request: none

### Verification Summary
- `git diff --check`: pass
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: pass, 50 examples, 0 failures
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`: pass, 1 example, 0 failures
