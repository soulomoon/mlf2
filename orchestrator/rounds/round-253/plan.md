### Selected Extraction
- Milestone: Token, Span, Diagnostic, And Lexer Seed
- Milestone id: `milestone-2`
- Direction id: `direction-2a-lexer-token-diagnostic-seed`
- Extracted item id: `item-253-lexer-token-diagnostic-seed`
- Roadmap id: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001`

### Goal
Implement the first bounded lexer seed in `.mlfp` on top of the existing compiler seed package. The round should define token, source-position/span, diagnostic, input-symbol, and lexer-result data models, prove one positive and one negative tokenization path through interpreter execution, and record the current text/character/byte/library gaps honestly.

### Approach
Keep the extraction serial and inside `milestone-2`. Build on `test/programs/compiler-seed/frontend-contract/` and `ProgramCompilerSeedSpec` from `milestone-1`; do not create a new package loader or a second source model.

Use a deliberately bounded input representation for this first seed: a seed-owned symbolic input ADT such as `SeedInputSymbol`, not raw `String` or byte/character decomposition. This keeps the proof executable with the current Prelude (`Nat`, `List`, `Option`, `Eq`, `Bool`, `String` literals for labels only) while exposing the real future gap: there is no source-level character stream API, byte API, substring API, map/set library, or parser-combinator library yet.

The seed should return inspectable ADT values, not smoke-only booleans. A good shape is a module split like:
- `SeedSource` for `SourcePosition`, `SourceSpan`, bounded `SeedInputSymbol`, and constructors for positive/negative fixtures;
- `SeedToken` for token ADTs such as identifier, delimiter, equals/signature marker, literal, or a tiny one-definition token subset;
- `SeedDiagnostic` for lexer diagnostics carrying a span/position and diagnostic kind;
- `SeedLexer` for `lexSeedInput : List SeedInputSymbol -> LexerResult`;
- `Main` as an aggregate interpreter evidence entrypoint that returns both positive and negative lexer results in one value.

The exact names may differ, but the implementation must keep source positions/spans explicit and must make the selected bounded grammar obvious. Avoid Haskell-side helpers that manufacture tokens or diagnostics outside `.mlfp`; tests should assert the value produced by `.mlfp` code.

### Steps
1. Inspect the live seed package and support surface before editing: `test/programs/compiler-seed/frontend-contract/`, `test/ProgramCompilerSeedSpec.hs`, `src/MLF/Frontend/Program/Prelude.hs`, `src/MLF/Primitive/Inventory.hs`, `src/MLF/Frontend/Program/Run.hs`, current parser diagnostics in `src/MLF/Frontend/Parse/Program.hs` and `src/MLF/Frontend/Program/Types.hs`, plus `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`, and `docs/architecture.md`.
2. Extend the existing compiler seed package with `.mlfp` source modules for position/span, token, diagnostic, bounded input symbols, and lexer result data. Keep these as ordinary package modules under `test/programs/compiler-seed/frontend-contract/` unless a narrower sub-root is justified by package discovery.
3. Implement a minimal lexer over `List SeedInputSymbol`. Target one tiny grammar slice only, such as an identifier, a delimiter, and one definition marker or literal shape. The lexer may be structurally recursive over `List`; it must not require broad `String`/byte/character APIs in this round.
4. Add positive and negative seed fixtures in `.mlfp` code. The positive path should return an asserted token stream or token result. The negative path should return an asserted diagnostic carrying a deliberate span/position for an unknown or misplaced input symbol.
5. Update `ProgramCompilerSeedSpec` or add a narrowly named seed spec if needed. Assert exact interpreter-rendered output for the aggregate lexer evidence, not only successful execution. If a new spec module is added, register it in both `mlf2.cabal` and `test/Main.hs`.
6. Update readiness/architecture/language docs only as needed to record what this lexer seed proves and what remains. The docs must keep support claims layer-separated and explicitly call out missing text/character/byte/collection/error APIs when the symbolic input representation avoids them.
7. Check for scope leaks: no parser seed or AST contract beyond tokens/diagnostics, no checker/backend/native/package-manager/ABI/linker/driver work, no public facade widening unless directly forced by focused seed evidence, no compatibility fallback, and no Haskell test helper standing in for `.mlfp` lexer logic.

### Verification
Run focused validation first with a single-token matcher:

```sh
cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'
```

If a new focused spec uses a more precise name, run that matcher too. Then run:

```sh
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Manual review should confirm positive and negative lexer evidence is produced by `.mlfp` seed code, token/span/diagnostic/result ADTs are represented in `.mlfp`, the selected input representation is explicitly symbolic and bounded, missing text/character/byte/list/error APIs are recorded as gaps rather than hidden in helpers, and backend/native behavior remains fail-closed and out of scope.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They are the machine authority for lineage and worker scheduling.
