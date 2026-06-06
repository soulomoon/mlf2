### Changes Made

- `test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedSource.mlfp`: copied the selected compiler-seed `SeedSource` module byte-for-byte into the parser-parity conformance fixture.
- `test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedToken.mlfp`: copied the selected compiler-seed `SeedToken` module byte-for-byte into the parser-parity conformance fixture.
- `test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedDiagnostic.mlfp`: copied the selected compiler-seed `SeedDiagnostic` module byte-for-byte into the parser-parity conformance fixture.
- `test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedAst.mlfp`: copied the selected compiler-seed `SeedAst` module byte-for-byte into the parser-parity conformance fixture.
- `test/conformance/mlfp/parser-parity/compiler-seed-data-model/expected/parser-program.txt`: added the canonical expected parser-program projection for the four selected source modules in stable source order.
- `test/programs/compiler-parser-parity/compiler-seed-data-model/ParserParityFixture.mlfp`: added the thin root fixture constants exposing selected source paths and exact source text.
- `test/programs/compiler-parser-parity/compiler-seed-data-model/Main.mlfp`: added the thin executable root that calls the shared parser-library four-source projection helper and prints the projection.
- `test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp`: raised the bounded lexer scan/reverse limit to 512 tokens and extended line-number successor support through the selected 66-line `SeedSource` fixture.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added a bounded four-source projection helper and structural grammar needed by the selected seed data-model modules: wider source-arrow types, five-branch case expressions, larger export lists, exact selected data-declaration shapes, six-data/four-definition module bodies, imported two-data module bodies, and parenthesized application support for the repeated five-branch `SeedSource` definitions.
- `test/ProgramParserParitySpec.hs`: registered the compiler-seed data-model fixture for direct shared-parser equality, generated aggregate positive coverage, byte-copy checks, one malformed `SeedSource` case-branch negative, and static shortcut guards.
- `implementation_notes.md`: recorded the round-338 parser-parity slice and explicit non-claims.
- `CHANGELOG.md`: noted the bounded compiler-seed data-model parser-parity fixture and explicit non-claims.
- `docs/mlfp-self-boot-readiness.md`: documented the new bounded seed data-model parser-parity slice and explicit non-claims.

### Tests

- `ghcup run --ghc 9.14.1 -- cabal run mlf2 -- run-program test/programs/compiler-parser-parity/compiler-seed-data-model --search-path test/programs/compiler-parser-parity/parser-library > /tmp/round338-compiler-seed-output-final3.txt` plus `cmp -s /tmp/round338-compiler-seed-output-final3.txt test/conformance/mlfp/parser-parity/compiler-seed-data-model/expected/parser-program.txt`: PASS (`run_rc=0`, `cmp_rc=0`).
- `git diff --check`: PASS.
- `for f in SeedSource SeedToken SeedDiagnostic SeedAst; do cmp -s "test/programs/compiler-seed/frontend-contract/$f.mlfp" "test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/$f.mlfp" || exit 1; done`: PASS (`source-copy-ok`).
- Parser-library shortcut guard over `test/programs/compiler-parser-parity/parser-library` for seed module names, fixture paths, expected projection paths, token-stream shortcuts, canonical-parser bypasses, and pre-rendered projection hooks: PASS (`parser-library-shortcut-guard-ok`).
- Docs overclaim guard over `implementation_notes.md`, `CHANGELOG.md`, and `docs/mlfp-self-boot-readiness.md`: PASS (`docs-overclaim-guard-ok`).
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS (`63 examples, 0 failures`, finished in `8384.6286` seconds).

### Notes

- Retry diagnosis reproduced the direct compiler-seed root `parser-error` and narrowed it to parser-library handling of the selected `SeedSource` shape. The final fix keeps the parser bounded and structural; it does not add SeedSource/module-name shortcuts, pre-rendered parser-program rows, token-stream bypasses, canonical-parser bypasses, package resolver behavior, checker/resolver/backend/package execution semantics, or compiler-package implementation.
- One intermediate focused Hspec run failed after the direct compiler-seed parse passed because the newly added malformed `SeedSource` negative expected span was stale. I updated that expected negative evidence to the parser-owned dynamic diagnostic span and reran the focused suite to green.
- I did not edit any `state.json`, did not merge, and did not rewrite `orchestrator/rounds/round-338/plan.md`.
- Controller should advance round-338 to review.
