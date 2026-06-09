### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-348-constructor-row-payload-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build an explicit parser-library-owned constructor-row payload and accumulator
substrate, then use it to reduce the exact constructor-count data-row parsers
that currently carry constructor token/type/span tuples through long
continuation chains.

The round should make constructor-row accumulation a reusable parser payload
instead of repeated arity-specific argument plumbing, while preserving the
existing bounded exact-count behavior, source span rendering, data-row
projection text, parser diagnostics, and aggregate parser-parity outputs.

### Approach
Rounds 341-347 already centralized diagnostic expectations, bounded projection
rows, bounded case branch rows, bounded application arguments, nested
parenthesized application depth, bounded annotated lambda RHS depth, and
bounded source-type arrow-tail text accumulation. Build on those substrate
patterns without reopening them.

Target the constructor-row owner surface in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`.
The immediate evidence is the `parseExactFourConstructorDataRows`,
`parseExactFiveConstructorDataRows`, and `parseExactNineConstructorDataRows`
families: each one repeats constructor name, type, and span arguments through
the next constructor parser and then renders a nested `appendLine` tree at the
end. The repeated callers in data-heavy source packages already consume these
functions as parser projection rows, so a constructor-row payload is a bounded
library substrate improvement rather than a new parser feature.

Add the smallest owner-local payload needed for constructor rows. Prefer a
`ParserValue` constructor that carries accumulated rendered constructor-row
text, plus helper functions that append one constructor row from the source
file, constructor token, type value, and computed span. Keep this payload
parser-library-owned and do not widen it into a generic AST, list API, full
parser framework, or production parser API.

Migrate the exact four-, five-, and nine-constructor data-row families onto the
new constructor-row accumulator. Preserve the current parsing order:
`data`, data name, `=`, constructor name, `:`, source type, `|` separators for
non-final constructors, and final `;`. Preserve the current span boundaries:
non-final constructor spans end at the following separator token start; the
final constructor span ends at the final semicolon token start; the data row
span still runs from the data keyword start to the current-token start after
the semicolon. Leave `parseSingleConstructorDataRows`,
`parseTwoConstructorDataRows`, `parseTwoConstructorDerivedDataRows`,
`parseNatRecursiveDataRows`, and `parseExprRecursiveDataRows` unchanged unless
the implementation needs a tiny helper call that is directly shared with the
selected exact-count families.

Do not change source-type parsing, case/lambda parsing, source-span rendering
semantics, diagnostic payloads, canonical parser behavior, checker policy, or
conformance fixture meaning in this round. Do not add fixture-name shortcuts,
pre-rendered projections, canonical-parser bypasses, retired syntax aliases, or
parser-private hacks. This is bounded milestone-4 ergonomics substrate only:
no full parser parity, compiler-package implementation, platform/proof
progress, native/backend completion, package-manager/linker work, or self-boot
completion is claimed.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: The selected goal and owner surface are bounded, but the task changes
  the shared parser-library payload shape used by parser combinator results and
  migrates multiple exact-count constructor parsers. That requires implementer
  and reviewer design judgment around payload naming, fallback behavior in
  existing `ParserValue` destructors, and preservation of span/diagnostic
  behavior. Standard verification is appropriate because the change is
  behavior-changing parser-library source work on the milestone-4 parser path.

### Steps
1. Inspect `ParserValue` and current payload destructors in
   `ParserParityParserCombinator.mlfp` and `ParserParityParser.mlfp`,
   especially `identifierNameFromValue`, `coordinateFromValue`,
   `tokenStartCoordinate`, `tokenEndCoordinate`, `parserTextFromValue`, and
   `parserTextValue`.
2. Add an owner-local constructor-row payload to `ParserValue` and update
   existing destructors so non-constructor contexts keep their current fallback
   behavior. Do not change token, module-key, or projection-row behavior.
3. Add focused helper functions in `ParserParityParser.mlfp` for starting an
   empty constructor-row accumulator, appending one constructor row with the
   existing `constructorRows` rendering helper, and extracting accumulated
   constructor rows for final data-row projection text.
4. Migrate `parseExactFourConstructorDataRows` to use the accumulator after
   each constructor type is parsed. Keep all separator/final-semicolon parsing
   and source-type parsing entrypoints unchanged.
5. Migrate `parseExactFiveConstructorDataRows` and
   `parseExactNineConstructorDataRows` the same way, removing only the migrated
   tuple-threading continuations and finish helpers. Do not leave compatibility
   aliases for removed exact-count helper names.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the constructor-row payload/helper substrate, representative four/five/nine
   exact-count call-site use, and absence of the migrated tuple-threading alias
   families.
7. Run the aggregate parser-parity Hspec group and full standard gate. If any
   migrated parser changes an existing expected parser projection, stop and fix
   the source-span or row-accumulation bug instead of updating expected output
   mechanically.
8. Record implementation evidence in
   `orchestrator/rounds/round-348/implementation-notes.md`, including the
   standard verification commands and explicit non-claims.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the constructor-row payload and
  accumulator helpers exist, exact four/five/nine constructor parsers use them,
  and migrated tuple-threading aliases were not reintroduced.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot
  claims, or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises the selected exact constructor-row
parsers through existing data constructor span, typeclass, recursive ADT/GADT,
existential, package-source, compiler-seed data-model, public generated
`run-program` batches, negative diagnostics, and shortcut guards. The standard
full Cabal gate is required because this round changes shared parser payload
shape and parser-library behavior. `./scripts/thesis-conformance-gate.sh` is
not required unless implementation widens into thesis-facing semantics,
production parser behavior, milestone closeout, package, platform, proof,
native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346, round-347
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
