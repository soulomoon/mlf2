### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-357-m4-canonical-parser-parity-closeout-audit
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Run the milestone-4 closeout audit for canonical `.mlfp` parser parity and
produce reviewer-ready evidence for status-only roadmap closeout if, and only
if, the current parser parity coverage still proves the required boundary.

This round closes only the milestone-4 parser/compiler-frontend status. It
must not claim compiler-package implementation, platform/proof progress,
native/backend completion, package-manager/linker work, self-boot completion,
or completion of any later roadmap milestone.

### Approach
Treat this as a closeout audit, not another direction-4b substrate slice.
Current evidence makes a bounded closeout round lawful to attempt: rounds 355
and 356 merged recursive class/instance method-row substrate, the current
parser parity spec includes recursive module-body and constructor-row coverage,
package-capable positive cases, meaningful malformed-source diagnostics, an
aggregate public CLI parser driver, and static shortcut/retired-helper guards.
The implementer must revalidate that evidence from the current worktree before
asking the reviewer to approve status-only closeout.

Owned implementation output is limited to
`orchestrator/rounds/round-357/implementation-notes.md`. Do not edit source,
tests, active roadmap files, `orchestrator/state.json`, `CHANGELOG.md`, root
`implementation_notes.md`, package/platform/proof/native/backend surfaces, or
public parser APIs. If any required audit command fails or any required
coverage surface is missing, stop and record the concrete blocker in
`implementation-notes.md`; do not patch code inside this closeout round and do
not request roadmap status closeout.

Audit these exact surfaces:

- `orchestrator/rounds/round-355/{plan.md,implementation-notes.md,review.md,merge.md}`
- `orchestrator/rounds/round-356/{plan.md,implementation-notes.md,merge.md}`
- `test/ProgramParserParitySpec.hs`
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`
- `test/programs/compiler-parser-parity/parser-library/ParserParityDiagnostic.mlfp`
- `test/conformance/mlfp/parser-parity/`

The audit must account for all of:

- recursive module-body declaration rows over supported `data`, `class`,
  `instance`, and `def` declarations until module close;
- recursive constructor rows without retired exact-count constructor helpers;
- recursive class and instance method rows without numbered continuation
  ladders;
- package-capable parser parity for same-root packages, ordered search-path
  packages, and the compiler-seed data-model package;
- meaningful malformed-source diagnostic evidence through the public
  `run-program` path, not static negative strings;
- one aggregate parser parity run through the generated public CLI driver;
- absence of retired exact-count module-body, constructor-row, and method-row
  helpers in the parser source;
- absence of fixture-name shortcuts, pre-rendered parser outputs,
  canonical-parser bypasses, retired syntax aliases, parser-private hacks, and
  overclaims.

### Execution Profile
- Complexity: closeout
- Verification profile: closeout
- Reason: The selected task is milestone closeout. The implementation content
  is an evidence audit and closeout package rather than new source behavior,
  but closing the public roadmap milestone is a semantic/status boundary and
  requires closeout-profile evidence.

### Steps
1. Confirm the assigned worktree is
   `/Volumes/src/mlf4/orchestrator/worktrees/round-357` on branch
   `orchestrator/round-357-m4-closeout`; read `orchestrator/state.json` only
   to confirm `roadmap_id`, `roadmap_revision`, `roadmap_dir`, empty
   `active_rounds`, and no active `roadmap_update`.
2. Re-read rev-007 `roadmap.md` and `verification.md`, plus round 355 and 356
   artifacts, and summarize the evidence they already record. Keep the summary
   bounded to parser/compiler-frontend milestone-4 evidence.
3. Inspect `test/ProgramParserParitySpec.hs` and the shared parser-library
   sources to map each required closeout surface to concrete tests, helper
   names, fixture families, and static guards.
4. Run the focused parser and guard commands listed below. Any failure is a
   closeout blocker, not permission to widen the round into implementation.
5. Run the static closeout inventory guard exactly as listed below. It must
   prove required positive coverage is present and retired helper names are
   absent from `ParserParityParser.mlfp`; occurrences inside
   `ProgramParserParitySpec.hs` are allowed only as guard data.
6. Run the full closeout gates: `cabal build all && cabal test` and
   `./scripts/thesis-conformance-gate.sh`.
7. Write `orchestrator/rounds/round-357/implementation-notes.md` with:
   coverage inventory, command results, static guard output, non-claims, and a
   reviewer-facing status-only closeout request. Do not edit active roadmap
   files directly.
8. If all required evidence passes, request reviewer approval for status-only
   closeout with the milestone selector and compact closeout text below. If any
   evidence fails, request no closeout and record `Mode: none` with the exact
   blocker.

### Verification
Closeout focused parser checks:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser recursively sequences module-body declarations"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser uses recursive module-body declaration sequencing"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "parser-owned .mlfp parser rejects malformed recursive module-body declaration sequencing"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares constructor row accumulation"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser recursively sequences class and instance method rows"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser guards recursive class and instance method row substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser parses same-root package source layout"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser parses ordered search-path package source layout"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser parses compiler-seed data-model package sources"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "parser-owned .mlfp parser reports malformed"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "runs all .mlfp parser parity fixtures through one generated public CLI driver"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

Static closeout inventory and shortcut guard:

```sh
ruby - <<'RUBY'
parser = File.read("test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp")
spec = File.read("test/ProgramParserParitySpec.hs")
combined = parser + "\n" + spec

required_parser = [
  "def parseModuleBodyRowsFirst : String -> String -> String -> String -> ParserValue -> Parser ParserValue",
  "def parseModuleBodyRowsMoreOrClose : String -> String -> String -> String -> ParserValue -> Parser ParserValue",
  "def parseModuleBodyDeclarationRows : String -> ParserValue -> Parser ParserValue",
  "parseSourceDefinitionRowsWithCurrentDefSemicolon sourceFile ValueUnit",
  "parseEqClassDeclaration sourceFile ValueUnit",
  "parseEqNatInstanceDeclarationRows sourceFile ValueUnit",
  "parseRecursiveConstructorDataRows sourceFile ValueUnit",
  "def parseRecursiveConstructorDataRows : String -> ParserValue -> Parser ParserValue",
  "def parseRecursiveConstructorDataRowsNext : String -> String -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
  "def appendRecursiveConstructorDataRowAndContinue : String -> String -> ParserValue -> ParserValue -> ParserValue -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
  "def parseEqClassMethodRowsMoreOrClose : String -> String -> ParserValue -> Parser ParserValue",
  "def appendClassMethodRowsAndContinue : String -> String -> ParserValue -> ParserValue -> Parser ParserValue",
  "def expectMethodNameParser : ParserValue -> Parser ParserValue",
  "def parseEqNatInstanceMethodRowsMoreOrClose : String -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
  "def appendInstanceMethodRowsAndContinue : String -> ParserValue -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
  "renderParserParityPackageProjectionFromSourceTexts",
  "renderParserParityPackageProjectionFromFourSourceTexts",
  "renderParserNegativeEvidenceFromSourceText",
  "renderParserDiagnosticEvidence"
]

required_spec = [
  "parserParityPositiveCases",
  "parserParityPackagePositiveCases",
  "positive:package-cross-module-let",
  "positive:package-search-path-import",
  "positive:compiler-seed-data-model",
  "parserParityNegativeCases",
  "recursiveModuleBodyPositiveCases",
  "recursiveModuleBodyNegativeCases",
  "sharedParserRecursiveModuleBodySequenceSubstratePhrases",
  "sharedParserRecursiveMethodRowSubstratePhrases",
  "sharedParserRemovedMethodRowContinuationAliases",
  "sharedParserRetiredExactModuleBodySequencePhrases",
  "sharedParserShortcutPhrases",
  "sharedParserBannedPhrases",
  "runs all .mlfp parser parity fixtures through one generated public CLI driver",
  "shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics"
]

retired_parser = [
  "def parseDeclarationLedBodyRows :",
  "def parseDefinitionLedBodyRows :",
  "def parseBoundedSourceDefinitionRows :",
  "def parseBoundedSourceDefinitionRowsRemaining",
  "def finishExactModuleBodyRows :",
  "def finishSixDataFourDefinitionRows :",
  "def parseDataLedSourceDefinitionSuffixRows :",
  "def parseExactFourConstructorDataRows",
  "def parseExactFiveConstructorDataRows",
  "def parseExactNineConstructorDataRows",
  "def parseEqClassMethodRowsMoreOrClose3 :",
  "def parseEqClassMethodRowsMoreOrClose2 :",
  "def parseEqClassMethodRowsMoreOrClose1 :",
  "def parseEqClassMethodRowsMoreOrClose0 :",
  "def appendClassMethodRowsAndContinue2 :",
  "def appendClassMethodRowsAndContinue1 :",
  "def appendClassMethodRowsAndContinue0 :",
  "def parseEqNatInstanceMethodRowsMoreOrClose3 :",
  "def parseEqNatInstanceMethodRowsMoreOrClose2 :",
  "def parseEqNatInstanceMethodRowsMoreOrClose1 :",
  "def parseEqNatInstanceMethodRowsMoreOrClose0 :",
  "def appendInstanceMethodRowsAndContinue2 :",
  "def appendInstanceMethodRowsAndContinue1 :",
  "def appendInstanceMethodRowsAndContinue0 :",
  "parseCompleteProgramFixture",
  "preRenderedParserProjection",
  "canonical-parser bypass"
]

missing = required_parser.reject { |s| parser.include?(s) } +
  required_spec.reject { |s| combined.include?(s) }
present_retired = retired_parser.select { |s| parser.include?(s) }

abort("missing closeout coverage phrases: #{missing.inspect}") unless missing.empty?
abort("retired helper or shortcut phrases present in parser source: #{present_retired.inspect}") unless present_retired.empty?
puts "M4 closeout static inventory passed: #{required_parser.length + required_spec.length} required phrases, #{retired_parser.length} retired/shortcut phrases checked"
RUBY
```

Overclaim and no-source-edit guard:

```sh
ruby - <<'RUBY'
allowed = [
  "?? orchestrator/rounds/round-357/",
  "?? orchestrator/rounds/round-357/plan.md",
  "?? orchestrator/rounds/round-357/implementation-notes.md",
  " M orchestrator/rounds/round-357/implementation-notes.md",
  "M  orchestrator/rounds/round-357/implementation-notes.md",
  "A  orchestrator/rounds/round-357/plan.md",
  "A  orchestrator/rounds/round-357/implementation-notes.md"
]
status = `git status --short`.lines.map(&:chomp)
unexpected = status.reject { |line| allowed.include?(line) }
abort("closeout audit must not edit source, tests, roadmap, or controller files: #{unexpected.inspect}") unless unexpected.empty?
notes_path = "orchestrator/rounds/round-357/implementation-notes.md"
if File.exist?(notes_path)
  notes = File.read(notes_path)
  required_non_claims = [
    "not full parser parity beyond milestone-4 parser/compiler-frontend status",
    "not compiler-package implementation",
    "not platform/proof progress",
    "not native/backend completion",
    "not package-manager/linker work",
    "not self-boot completion"
  ]
  missing = required_non_claims.reject { |s| notes.include?(s) }
  abort("missing closeout non-claims: #{missing.inspect}") unless missing.empty?
end
puts "M4 closeout overclaim/no-source-edit guard passed"
RUBY
```

Full closeout gates:

- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`
- `./scripts/thesis-conformance-gate.sh`

Reviewer-facing closeout request, only if all checks pass:

- Milestone selector: `milestone-4-full-canonical-mlfp-parser-parity`
- Target status: `[done]`
- Completion pointer:
  `round-357 closed milestone 4 by revalidating recursive module-body declarations, recursive constructor rows, recursive class/instance method rows, package-capable parser parity, dynamic negative diagnostics, aggregate parser parity, retired-helper absence, full Cabal gate, thesis conformance gate, and shortcut/overclaim guards. This is parser/compiler-frontend status-only closeout and does not claim compiler-package, platform/proof, native/backend, package-manager/linker, self-boot, or later-milestone completion.`
- History entry:
  `round-357: status-only milestone-4 closeout revalidated canonical parser parity coverage and guards for the parser/compiler-frontend source path; no future sequencing or post-M4 milestone meaning changed.`

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
