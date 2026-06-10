### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added owner-local `parserValue...` helpers for token text, dropped token text, projection-or-token text, constructor rows, token start/end coordinates, module-key-or-token fallback coordinates, token spans, and token-bound span construction. Existing identifier, literal, parser-text, coordinate, token-bound span, constructor-row, module-name, and projection-token helpers now route through that substrate while preserving their prior fallbacks.
- `test/ProgramParserParitySpec.hs`: added a focused static Hspec guard requiring the parser-value source-span extraction substrate, representative migrated wrappers/call sites, and absence of the old direct fallback `case` blocks on migrated helper names.
- `orchestrator/rounds/round-353/implementation-notes.md`: recorded implementer evidence for round-353.

### Tests
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares parser-value source-span extraction substrate"'`: PASS; 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS; 80 examples, 0 failures; Hspec reported `Finished in 8339.1832 seconds`.
- Static helper/call-site/duplicate-fallback-removal guard over `ParserParityParser.mlfp` and `ProgramParserParitySpec.hs`: PASS; found 11 helpers, representative migrated phrases, the spec guard phrases, and no old direct fallback blocks on migrated helper names.
- Changed-line shortcut/overclaim guard over changed parser-library/spec lines: PASS; no fixture-name shortcuts, pre-rendered projections, canonical-parser bypasses, static negative evidence, retired syntax aliases, compiler-package/platform/proof/native/backend/package-manager/linker/self-boot claims, or full parser parity claims in changed implementation/spec lines.
- `git diff --check`: PASS.

### Notes
Focused verification is the approved profile for this non-closeout parser/compiler-frontend ergonomics substrate slice. I did not run full closeout gates, `cabal build all && cabal test`, or `./scripts/thesis-conformance-gate.sh` because the diff stays inside parser-library/spec/round-artifact scope, preserves existing parser-parity outputs, and does not change production parser, checker, resolver, backend, package, platform, proof, or native code.

The implementation uses first-order token-bound span helpers. A higher-order coordinate-extractor helper was avoided because a diagnostic CLI reproduction showed that shape triggered a presolution failure in the shared `.mlfp` parser program; the final first-order helper surface passed both the focused static selector and the aggregate parser-parity gate.
