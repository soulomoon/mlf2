### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityDiagnostic.mlfp`: added diagnostic-owned `diagnosticEvidenceLabel`, `diagnosticEvidenceSpan`, and `renderParserDiagnosticEvidence` helpers. The helper surface preserves the existing constructor-to-label mapping and still renders spans through `renderSpan` with the caller-provided source file.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: migrated token, lexer-negative, parser-negative, and retry evidence paths to `renderParserDiagnosticEvidence`, then removed the parser-local `renderDiagnosticEvidence` case instead of leaving a compatibility wrapper.
- `test/ProgramParserParitySpec.hs`: added a focused static Hspec guard requiring the diagnostic evidence helper surface, representative migrated parser call sites, guard phrases, and absence of the removed parser-local renderer.
- `orchestrator/rounds/round-354/implementation-notes.md`: recorded implementer evidence for round-354.

### Tests
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares diagnostic evidence rendering substrate"'`: PASS; 1 example, 0 failures; Hspec reported `Finished in 0.0569 seconds`.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS; 81 examples, 0 failures; Hspec reported `Finished in 9138.0477 seconds`.
- Static helper/call-site/no-compat-renderer guard over `ParserParityDiagnostic.mlfp`, `ParserParityParser.mlfp`, and `ProgramParserParitySpec.hs`: PASS; found diagnostic label/span/render helpers, representative migrated evidence call sites, spec guard phrases, and no parser-local `renderDiagnosticEvidence` compatibility wrapper on the migrated path.
- Changed-line shortcut/overclaim guard over changed parser-library/spec lines: PASS; no excluded shortcut or overclaim patterns from the plan were found in changed implementation/spec lines.
- `git diff --check`: PASS.

### Notes
No `CHANGELOG.md` or root `implementation_notes.md` update was needed because this is a behavior-preserving parser-library ownership move; the round artifact records the bounded substrate evidence.

No production parser, checker, resolver, backend, package, platform, proof, native code, generated fixtures, expected outputs, roadmap files, or `orchestrator/state.json` were edited. No full parser-parity, compiler-package, platform/proof, native/backend, package-manager/linker, or self-boot claim is made. Full Cabal and thesis closeout gates were not run because the approved plan selected focused verification for this non-closeout parser-library/spec/round-artifact slice.
