### Changes Made
- `src/MLF/Backend/Emission/Prepare.hs`: added the private backend adapter that parses a caller-provided source string, injects the Prelude, checks the program, and owns backend-emission Prelude retention pruning.
- `src/MLF/Program/CLI.hs`: reduced backend/native emission entrypoints to file reading, backend preparation delegation, LLVM rendering, and user-facing error rendering; `runProgramFile` remains in the CLI path.
- `mlf2.cabal`: registered the new private adapter module and the focused spec module.
- `docs/architecture.md`: documented the backend-emission preparation ownership boundary and the remaining CLI file/command ownership.

### Tests
- `test/BackendEmissionPrepareSpec.hs`: verifies source-string preparation can render LLVM without temp-file IO and that the adapter retains referenced Prelude data while pruning unreferenced Prelude bindings.
- `test/Main.hs`: wires `BackendEmissionPrepareSpec` into the Hspec suite.

### Notes
- No `src-public/` files were changed.
- `runProgramFile` remains in the CLI path; runtime execution was not moved into backend emission preparation.
- The plan's literal spaced matcher form for the CLI file-entrypoint test was rejected by Cabal/Hspec argument splitting, so the equivalent two-option matcher form was used.

### Validation
- PASS: `cabal test mlf2-test --test-options "--match BackendEmissionPrepareSpec"`
- PASS: `cabal test mlf2-test --test-option=--match --test-option="emits LLVM IR from the CLI file entrypoint"`
- PASS: `cabal test mlf2-test --test-option=--match --test-option="legacy MLF.Program compatibility shim is removed"`
- PASS: `git diff --check`
- PASS: `cabal build all && cabal test`
