### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-347-source-type-arrow-tail-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for bounded source-type
arrow-tail parsing, then use it for the existing source-type arrow-chain parser
paths in `ParserParityParser.mlfp`.

The round should reduce the copied `parseSourceTypeArrowTailText*` /
`parseSourceTypeCodomainText*` depth plumbing while preserving the existing
parser-parity behavior proved through the aggregate parser Hspec group. This is
milestone-4 parser/compiler-frontend ergonomics substrate only. It must not
claim full parser parity, compiler-package implementation, platform/proof
progress, native/backend completion, package-manager/linker work, or self-boot
completion.

### Approach
Use the rev-007 roadmap adjustment: milestone 4 remains in progress, and lawful
work may target reusable parser ergonomics substrate instead of another
fixture-shaped syntax slice. Keep `orchestrator/state.json` untouched and use
the controller/user-assigned rev-007 planning context recorded above.

Rounds 341-346 already centralized diagnostic expectations, bounded projection
row lists, bounded case-branch rows, bounded general/simple application
arguments, nested parenthesized application depth handling, and bounded
annotated lambda RHS depth handling. Build on those substrates without
reopening them.

Target the remaining bounded source-type arrow-tail family in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseSourceTypeArrowTailText7` through
  `parseSourceTypeArrowTailText0`;
- `parseSourceTypeCodomainText6` through
  `parseSourceTypeCodomainText0`;
- the entry path from `parseSourceTypeCodomain`; and
- any directly migrated helper names needed to make the bounded depth boundary
  explicit.

Introduce the smallest owner-local helper family that expresses the repeated
shape: accumulate the rendered left/prefix type text, optionally consume the
next `->` token, parse the next source-type codomain atom, and either continue
within the bounded budget or return the accumulated arrow type text. Keep it in
`ParserParityParser.mlfp`; do not move source-type grammar policy into
`ParserParityParserCombinator.mlfp` or expose a production parser API.

Preserve the existing arrow-chain budget, `parseSourceTypeCodomainAtom`
boundary, parenthesized source-type codomain handling, named/applied source-type
fallback, text rendering through `appendSourceArrowTypeText`, and the current
stop-on-no-next-arrow behavior. The current source includes both
`parseSourceTypeArrowTailText4` and `parseSourceTypeArrowTailText3`; inspect
whether depth 3 is reachable before removing or migrating it, and do not leave
dead compatibility aliases behind.

Do not redesign forall source-type parsing, simple arrow source-type parsing,
constructor-row parsing, expression annotations, source-span rendering,
diagnostic payloads, production parser behavior, or checker policy in this
round. Do not add compatibility aliases, fixture-name shortcuts, pre-rendered
projections, static negative evidence, canonical-parser bypasses, retired
syntax shims, parser-private shortcuts, or compiler-seed/package/platform/proof
hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content changes a shared parser-library grammar
  path across bounded source-type arrow-chain depth variants and needs
  implementation judgment to preserve the arrow budget, codomain atom boundary,
  fallback behavior, and rendered source-type text. Focused verification is
  sufficient because the work is confined to the shared parser-parity
  library/spec/docs owner surface, preserves existing behavior, does not touch
  production parser, checker, resolver, backend, package, platform, proof, or
  native code, and makes no milestone closeout or self-boot claim.

### Steps
1. Inspect the existing source-type arrow-tail family in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
   and confirm the behavior that must be preserved: bounded arrow depth,
   `parseSourceTypeCodomainAtom` parsing, parenthesized codomain handling,
   named/applied source-type fallback, stop-on-no-next-arrow behavior, and text
   rendering through `appendSourceArrowTypeText`.
2. Add one owner-local bounded helper family in `ParserParityParser.mlfp` for
   source-type arrow-tail text accumulation. Keep the helper bounded to the
   parser-library source-type path and do not make it a broad parser framework.
3. Route `parseSourceTypeCodomain` through the new bounded helper entry point
   while preserving the existing depth budget that starts after the first
   parsed codomain atom.
4. Migrate the depth-specific `parseSourceTypeArrowTailText*` and
   `parseSourceTypeCodomainText*` paths onto the helper or focused helper entry
   points while preserving each depth's continuation boundary.
5. Remove migrated duplicated source-type arrow-tail definitions instead of
   leaving compatibility aliases. If any depth-specific name must remain for a
   real call-site boundary, document it in code through clear naming rather
   than keeping an old alias.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` proving the
   bounded source-type arrow-tail substrate exists, the source-type codomain
   path uses it, and migrated numbered helper names were not reintroduced as
   compatibility aliases.
7. Run the focused parser-parity gate and static shortcut/overclaim guards.
   Update repo-facing notes only if they record bounded substrate evidence, and
   keep all non-claims explicit.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the bounded source-type
  arrow-tail helper is present, the source-type codomain path uses it, and
  migrated helper aliases were not reintroduced.
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot
  claims, or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises source-type parsing through existing
typed annotation, first-class polymorphism, type-family, data constructor,
GADT, compiler-seed parser paths, public generated `run-program` batches,
negative diagnostics, and shortcut guards. Full closeout gates,
`cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh`
are not required unless the implementation widens beyond parser-library/spec/docs
scope or makes production parser, thesis-facing semantic, milestone closeout,
package, platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
