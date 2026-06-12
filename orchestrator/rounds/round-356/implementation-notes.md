### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: replaced the numbered class method-row continuation ladder with a single self-recursive `parseEqClassMethodRowsMoreOrClose` plus `appendClassMethodRowsAndContinue`, replaced the numbered instance method-row continuation ladder with a single self-recursive `parseEqNatInstanceMethodRowsMoreOrClose` plus `appendInstanceMethodRowsAndContinue`, and replaced the temporary `eq`/`neq` method-name parser with ordinary method identifiers inside the existing method-row surface.
- `test/ProgramParserParitySpec.hs`: extended `recursiveMethodRowsSourceText` to five unique class signatures and five unique instance method definitions, updated the method-row static guard to require the self-recursive helpers and ordinary method-name parser, and added the retired numbered continuations to the banned parser-source phrase list.
- `orchestrator/rounds/round-356/plan.md`: recorded the selected rev-007 milestone-4 direction-4b simple slice and focused verification profile.

### Direct Verification
- Command: `git diff --check`
  Result: pass. No whitespace errors.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser recursively sequences class and instance method rows"'`
  Result: first failed after the initial recursive-helper edit because duplicate `eq`/`neq` method rows exposed canonical/shared span disagreement for duplicate method names. After replacing the temporary `eq`/`neq` method-name parser with ordinary identifiers and changing the source to five unique methods, rerun passed: 1 example, 0 failures, `Finished in 212.8145 seconds`.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser guards recursive class and instance method row substrate"'`
  Result: pass. 1 example, 0 failures, `Finished in 0.1737 seconds`.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: pass. 84 examples, 0 failures, `Finished in 6921.2870 seconds`.
- Command: `git diff --name-only -- orchestrator/state.json orchestrator/roadmaps CHANGELOG.md implementation_notes.md runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass. Empty output; no controller state, active roadmap, changelog, root implementation notes, or known generated runtime dependency file changed.

### Direct Closeout
- Mode: none
- Status changes: none
- Completion pointers: none
- History entries: none
- Semantic update reason: none
