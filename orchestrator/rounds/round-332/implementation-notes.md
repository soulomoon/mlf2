### Changes Made
- test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/src/Main.mlfp: added the parser-parity conformance source copied from the approved abstract recursive ADT module-use fixture.
- test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/expected/parser-program.txt: committed the canonical parser-program projection for the new bounded fixture.
- test/programs/compiler-parser-parity/abstract-recursive-adt-module-use/ParserParityFixture.mlfp: added the thin fixture module exposing `sourceFile` and `sourceText`.
- test/programs/compiler-parser-parity/abstract-recursive-adt-module-use/Main.mlfp: added the parser-owned package root calling `renderParserParityProjectionFromSourceText`.
- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp: extended the shared parser library for Nat data plus four generic definitions, five-item import exposing lists, and generic imported Bool `main` expression parsing after legacy exact imported-main fallbacks.
- test/ProgramParserParitySpec.hs: registered the positive and negative batch cases, added the direct shared-parser assertion, and extended shortcut/static guards for the round-332 fixture surface.
- implementation_notes.md: documented bounded round-332 parser-parity evidence and non-claims.
- CHANGELOG.md: recorded the bounded parser-parity addition under Unreleased.
- docs/mlfp-self-boot-readiness.md: added a bounded round-332 readiness note with explicit non-claims.

### Tests
- test/ProgramParserParitySpec.hs: canonical positive projection batch now includes `positive:abstract-recursive-adt-module-use`.
- test/ProgramParserParitySpec.hs: direct shared parser assertion verifies `abstract-recursive-adt-module-use` through `run-program` and the shared parser library.
- test/ProgramParserParitySpec.hs: generated public CLI batch includes malformed destructor-case syntax with `expected-case-branch-arrow@...` negative evidence.
- test/ProgramParserParitySpec.hs: shortcut guards reject fixture-name, whole-source, pre-rendered-row, imported-main-expression, and static-negative shortcut strings for this slice.

### Notes
Focused verification is being used per plan. `./scripts/thesis-conformance-gate.sh` is deferred because this change does not edit thesis obligation ledgers and only records bounded parser-parity evidence with explicit non-claims, not broader thesis/readiness claims.
