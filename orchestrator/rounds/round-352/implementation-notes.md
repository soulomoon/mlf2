### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added exact `parseTwoSourceDefinitionRows` and `parseThreeSourceDefinitionRows` entrypoints that reuse the existing bounded source-definition row substrate and `appendProjectionValues`.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: migrated `parseTwoDefinitionBodyRows`, `parseThreeDefinitionBodyRows`, and `parseThreeImportedSourceDefinitionRows` onto the exact helpers while preserving module-body finish and imported-body continuations.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: removed the migrated second/third source-definition continuation aliases. Two existing non-selected callers that depended on `parseSecondSourceDefinitionRows` now route through `parseTwoSourceDefinitionRowsThenFinishModuleBody` so the removed alias is not kept as a compatibility wrapper.
- `test/ProgramParserParitySpec.hs`: added focused static coverage for helper presence, migrated call sites, spec guard enrollment, and absence of the removed source-definition continuation aliases from parser-library source.
- `CHANGELOG.md` and `implementation_notes.md`: documented the bounded compiler-frontend/parser ergonomics substrate and explicit non-claims: no full parser parity, compiler-package implementation, platform/proof progress, native/backend completion, package-manager/linker work, or self-boot completion.
- `orchestrator/rounds/round-352/implementation-notes.md`: recorded implementation evidence for the implementer stage.

### Tests
- `git diff --check`: PASS.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded module-body source-definition row sequencing"'`: PASS; 1 example, 0 failures; Hspec reported `Finished in 0.0790 seconds`.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS; 79 examples, 0 failures; Hspec reported `Finished in 8322.6673 seconds`.
- Static helper/call-site/alias-removal guard over `ParserParityParser.mlfp` and `ProgramParserParitySpec.hs`: PASS; required exact two-/three-source-definition helper and migrated call-site phrases were present, and removed alias definitions were absent from parser-library source.
- Changed-line shortcut/overclaim guard over changed parser-library, spec, docs, and round-artifact lines: PASS.

### Notes
Focused verification is the approved round profile. I did not run full closeout gates, `cabal build all && cabal test`, or `./scripts/thesis-conformance-gate.sh` because the plan confines this slice to parser-library/spec/docs/round-artifact scope and makes no thesis-facing semantic, package/platform/proof/native/backend, milestone-closeout, or self-boot claim.

No blockers. I did not edit `orchestrator/state.json`, rewrite `plan.md`, merge, or commit.
