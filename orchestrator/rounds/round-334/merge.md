### Merge
- Round id: round-334
- Branch: orchestrator/round-334-next-parser-parity-slice
- Round commit: c59feb62
- Base branch: master
- Merge mode: squash

### Controller Finalization
- Review decision: APPROVED
- Roadmap Closeout mode: none
- State update: removed round-334 from `active_rounds`
- Semantic roadmap update: none
- User stop request: none

### Verification Summary
- `git diff --check`: pass
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: pass, 45 examples, 0 failures
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity/shared parser-owned .mlfp parser parses complex recursive programs"'`: pass, 1 example, 0 failures
- `cabal run mlf2 -- check-program test/programs/compiler-parser-parity/complex-recursive-program --search-path test/programs/compiler-parser-parity/parser-library`: pass, printed `OK`
