### Changes Made
- `test/ProgramParserParitySpec.hs`: added the public round-317 matcher for qualified import aliases and qualified references, committed-source canonical projection checks, shared parser fixture checks, malformed import-alias evidence through `renderParserNegativeEvidenceFromSourceText`, and shortcut guards for rejected round-317 fixture-key parser branches.
- `test/conformance/mlfp/parser-parity/qualified-import-alias-references/`: added the canonical source and expected projection for an aliased `Core` import that exposes `Eq`, `Token(..)`, `answer`, and `eq`, then exercises unqualified and qualified value/type/constructor/class/method references.
- `test/conformance/mlfp/parser-parity/qualified-import-alias-only/`: added the canonical source and expected projection for an alias-only `Core as C` import with qualified type and constructor access.
- `test/programs/compiler-parser-parity/qualified-import-alias-references/` and `test/programs/compiler-parser-parity/qualified-import-alias-only/`: added thin fixture roots with only `Main.mlfp` and `ParserParityFixture.mlfp`, exposing `sourceFile` and `sourceText` before calling `renderParserParityProjectionFromSourceText`.
- `test/programs/compiler-parser-parity/parser-library/`: extended the shared lexer, diagnostic ADT, parser-combinator diagnostic propagation, shared parser grammar, span constants, and projection renderer for the bounded qualified import/reference syntax slice. The new parser uses internal projection keys (`qualified-import-reference-projection`, `qualified-import-access-projection`) instead of fixture-key success shortcuts.
- `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`: recorded bounded parser-parity progress without claiming full parser parity, resolver/checker/backend, driver, platform, proof, or self-boot completion.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies canonical Haskell parser projections match committed projections for the two new fixtures, verifies the shared `.mlfp` parser-library projections match the same outputs through public `run-program`, verifies malformed import-alias syntax reports `expected-import-alias`, and guards against round-317 fixture-key parser shortcuts.
- Focused RED evidence: `timeout 900 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to qualified imports and references/"'` failed before implementation with the shared parser fixture returning `Right "parser-error\n"` for `qualified-import-alias-references` after the committed canonical projections were in place.
- Focused GREEN evidence: the same command passed after implementation with `1 example, 0 failures` in `742.4274 seconds`.
- Parser-parity group: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'` timed out with status 124 after confirming the shared lexer, case-expression, typeclass/instance, higher-kinded/constrained, and closed type-family/type-level parser-parity matchers passed. It did not emit group-level GADT/existential or round-317 matcher output before the timeout; the later full `cabal test` completed those matchers successfully.
- Direct run-program smokes: a shell loop using `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/<fixture> --search-path test/programs/compiler-parser-parity/parser-library` passed for all carried parser-parity fixtures plus `qualified-import-alias-references` and `qualified-import-alias-only`.
- Static audits: thin fixture audit, exact-source/static-token audit, rejected round-317 fixture-key shape audit, and shared parser architecture audit all produced `NO_MATCHES`.
- `git diff --check` passed.
- `timeout 7200 cabal build all` passed.
- `timeout 10800 cabal test` passed with `2639 examples, 0 failures` in `9629.0173 seconds`.
- `timeout 7200 ./scripts/thesis-conformance-gate.sh` passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
- Scope remains bounded parser parity only. This round does not claim resolver/checker/backend/platform/driver/proof/full-parser-parity/self-boot completion.
- Validation-generated churn in `runtime/mlfp_io/target/release/libmlfp_io.d` was restored after the full gates.
