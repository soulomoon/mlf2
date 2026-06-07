### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-342-delimited-projection-list-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for bounded delimited
projection-row lists, then use it for the existing export and import exposing
projection list parsers.

The round should reduce the copied comma-list continuation ladders in
`ParserParityParser.mlfp` while preserving the existing parser-parity behavior
proved through the aggregate parser Hspec group. This is milestone-4
parser/compiler-frontend ergonomics substrate only. It must not claim full
parser parity, compiler-package implementation, platform/proof progress,
native/backend completion, package-manager/linker work, or self-boot
completion.

### Approach
Use the rev-007 roadmap adjustment approved after round 340: milestone 4
remains in progress, and lawful work may target reusable parser ergonomics
substrate instead of another fixture-shaped syntax slice. The assigned
worktree's checked-in `orchestrator/state.json` still names rev-006, so keep it
untouched and treat this plan's selected extraction as the controller/user
assigned rev-007 planning context.

Round 341 already centralized parser diagnostic expectations. Build on that
substrate without reopening it. Target the repeated projection-list pattern now
visible in `ParserParityParser.mlfp`:

- `parseProjectionExportMoreOrDone8` through `parseProjectionExportMoreOrDone0`
  and their `parseProjectionExportNextItem*` / append continuations;
- `parseImportProjectionMoreOrClose8` through
  `parseImportProjectionMoreOrClose0` and their
  `parseImportProjectionNextItem*` / append continuations.

Introduce a parser-library helper family for bounded comma-separated
projection-row accumulation. Keep it owner-local to
`test/programs/compiler-parser-parity/parser-library/`; place it in
`ParserParityParser.mlfp` unless the implementation can keep a truly generic
`ParserValue` combinator in `ParserParityParserCombinator.mlfp` without making
projection-row semantics leak into the generic combinator module.

Migrate only the export/import projection list parsers in this round. Preserve
the existing maximum item budget, row ordering, close-token behavior, expected
import-exposing-separator diagnostic, and non-backtracking behavior. Do not
attempt to remove every case-branch, nested-depth, or source-expression ladder
in the same round; those are future substrate slices after this list helper has
evidence.

Do not add compatibility aliases, fixture-name shortcuts, pre-rendered
projections, canonical-parser bypasses, retired syntax shims, parser-private
shortcuts, or compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a reusable bounded delimited-list
  parser substrate and migrates two existing parser-library list surfaces across
  it, so it needs implementer/reviewer design judgment and is not simple.
  Focused verification is sufficient because the work is confined to the shared
  parser-parity library/spec/docs owner surface, preserves existing behavior,
  does not touch production parser, checker, resolver, backend, package,
  platform, proof, or native code, and makes no milestone closeout or self-boot
  claim.

### Steps
1. Inspect the existing export/import projection list ladders in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
   and confirm the exact behavior that must be preserved: comma consumption,
   row append order, item budget, `")"` close handling, semicolon handling, and
   `ParserExpectImportExposingSeparator` failure behavior.
2. Add one bounded comma-separated projection-row helper family in the
   parser-library owner surface. Keep it reusable for both export and import
   projection lists, but do not make it a broad parser framework or a
   production parser API.
3. Migrate `parseProjectionExportList` and its
   `parseProjectionExportMoreOrDone*` / `parseProjectionExportNextItem*`
   ladder onto the helper. Remove migrated duplicated definitions instead of
   leaving compatibility aliases.
4. Migrate `parseImportProjectionMoreOrClose` and its
   `parseImportProjectionMoreOrClose*` / `parseImportProjectionNextItem*`
   ladder onto the same helper while preserving the close-parenthesis,
   semicolon, and expected-separator diagnostic behavior.
5. Add focused static coverage in `test/ProgramParserParitySpec.hs` proving the
   delimited-list helper exists, both export and import projection list call
   sites use it, and the migrated numbered projection-list helper names were
   not reintroduced as compatibility aliases.
6. Run the focused parser-parity gate and static shortcut/overclaim guards.
   Update repo-facing notes only if they record bounded substrate evidence, and
   keep all non-claims explicit.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the bounded delimited-list
  helper is present, export and import projection list parsers call it, and the
  migrated numbered projection-list helper aliases were not reintroduced.
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot
  claims, or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises the shared parser-library through the
existing direct projections, package projections, public generated
`run-program` batch, negative diagnostics, and shortcut guards. Full closeout
gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required unless the
implementation widens beyond parser-library/spec/docs scope or makes
production parser, thesis-facing semantic, milestone closeout, package,
platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
