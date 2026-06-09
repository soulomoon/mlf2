### Checks Run
- Command: `git diff --check`
  Result: PASS. No whitespace or conflict-marker output.

- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded import row sequencing"'`
  Result: PASS. Hspec reported `1 example, 0 failures`.

- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: PASS. Hspec reported `78 examples, 0 failures` in 8366.6349 seconds.

- Command:
  ```bash
  bash -lc 'set -euo pipefail
  parser="test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp"
  spec="test/ProgramParserParitySpec.hs"
  required_parser=(
    "def parseBoundedOneImportRows : String -> String -> String -> String -> ParserValue -> Parser ParserValue"
    "def parseBoundedThreeImportRows : String -> String -> String -> String -> ParserValue -> Parser ParserValue"
    "def parseBoundedImportRows : String -> String -> String -> String -> (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    "def parseBoundedImportRowsNext : String -> (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    "def appendBoundedImportRowsAndContinue : (ParserValue -> Parser ParserValue) -> ParserValue -> ParserValue -> Parser ParserValue"
    "def parseBoundedImportRowsRemaining2"
    "def parseBoundedImportRowsRemaining1"
    "parserBind (parseImportProjectionRows sourceFile ValueUnit)"
    "parserBind (appendProjectionValues existingRows nextRows)"
    "parseBoundedThreeImportRows sourceFile moduleStart moduleName exportRows ValueUnit"
    "parseBoundedOneImportRows sourceFile moduleStart moduleName exportRows ValueUnit"
    "parseBoundedImportRows sourceFile moduleStart moduleName exportRows (parseBoundedImportRowsRemaining2 sourceFile moduleStart moduleName exportRows) start"
    "parseBoundedImportRows sourceFile moduleStart moduleName exportRows (parseImportedBodyAfterImport sourceFile moduleStart moduleName exportRows) start"
    "parseBoundedImportRowsNext sourceFile (parseBoundedImportRowsRemaining1 sourceFile moduleStart moduleName exportRows) rowsValue"
    "parseBoundedImportRowsNext sourceFile (parseImportedBodyAfterImport sourceFile moduleStart moduleName exportRows) rowsValue"
  )
  required_spec=(
    "shared parser-owned .mlfp parser shares bounded import row sequencing"
    "sharedParserBoundedImportRowSequenceSubstratePhrases"
    "sharedParserBoundedImportRowSequenceUsePhrases"
    "sharedParserImportRowSequenceGuardPhrases"
    "sharedParserRemovedImportRowSequenceAliases"
  )
  removed_aliases=(
    "def parseThreeImportSecondRows"
    "def appendThreeImportSecondRows"
    "def parseThreeImportThirdRows"
    "def appendThreeImportThirdRows"
  )
  for phrase in "${required_parser[@]}"; do rg -Fq "$phrase" "$parser"; done
  for phrase in "${required_spec[@]}"; do rg -Fq "$phrase" "$spec"; done
  for alias in "${removed_aliases[@]}"; do if rg -Fq "$alias" "$parser"; then echo "removed alias still present: $alias"; exit 1; fi; done
  printf "static helper/call-site/alias-removal guard passed\n"'
  ```
  Result: PASS. Required helper surface, migrated one-/three-import call sites, and static guard names are present; removed second/third import-row aliases are absent from parser-library source.

- Command:
  ```bash
  python3 - <<'PY'
  from pathlib import Path
  import subprocess
  import sys

  tracked_paths = [
      'CHANGELOG.md',
      'implementation_notes.md',
      'test/ProgramParserParitySpec.hs',
      'test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp',
  ]
  diff = subprocess.check_output(['git', 'diff', '--unified=0', '--', *tracked_paths], text=True)
  chunks = []
  current_file = None
  current_lines = []

  def flush():
      if current_file and current_lines:
          chunks.append((current_file, '\n'.join(current_lines)))
      current_lines.clear()

  for line in diff.splitlines():
      if line.startswith('diff --git '):
          flush()
          current_file = None
      elif line.startswith('+++ b/'):
          current_file = line[len('+++ b/'):]
      elif line.startswith('@@'):
          flush()
      elif line.startswith('+') and not line.startswith('+++'):
          current_lines.append(line[1:])
      else:
          if current_lines:
              flush()
  flush()

  shortcut_terms = [
      'fixture-name shortcut',
      'fixture name shortcut',
      'pre-rendered',
      'canonical-parser bypass',
      'canonical parser bypass',
      'static negative evidence',
      'retired syntax alias',
      'retired syntax aliases',
  ]
  overclaim_terms = [
      'full parser parity',
      'compiler-package implementation',
      'compiler package implementation',
      'platform/proof progress',
      'platform proof progress',
      'native/backend completion',
      'native backend completion',
      'package-manager/linker',
      'package manager linker',
      'self-boot completion',
      'self boot completion',
  ]
  nonclaim_markers = [
      'not ',
      'no ',
      'do not ',
      'must not ',
      'does not claim',
      'non-claims',
      'out of scope',
      'bounded ',
      'only',
  ]
  failures = []
  for path, chunk in chunks:
      lower = chunk.lower()
      for term in shortcut_terms:
          if term in lower:
              failures.append(f'{path}: shortcut term in added implementation/doc lines: {term}')
      overclaim_hits = [term for term in overclaim_terms if term in lower]
      if overclaim_hits and not any(marker in lower for marker in nonclaim_markers):
          failures.append(f'{path}: possible overclaim without non-claim marker: {overclaim_hits}')

  round_docs = [
      'orchestrator/rounds/round-351/plan.md',
      'orchestrator/rounds/round-351/implementation-notes.md',
  ]
  for path in round_docs:
      lower = Path(path).read_text().lower()
      overclaim_hits = [term for term in overclaim_terms if term in lower]
      if overclaim_hits and not any(marker in lower for marker in ['not ', 'do not ', 'must not ', 'non-claims', 'no ']):
          failures.append(f'{path}: possible round-doc overclaim without non-claim marker: {overclaim_hits}')

  if failures:
      print('\n'.join(failures))
      sys.exit(1)
  print(f'changed-line shortcut/overclaim guard passed ({len(chunks)} added hunks plus {len(round_docs)} round docs checked)')
  PY
  ```
  Result: PASS. Checked 11 added tracked hunks plus the 2 round docs; no shortcut terms were introduced, and claim-related text is explicit non-claim language.

### Plan Compliance
- Step 1, inspect current import-led body call graph: met. The diff targets `parseThreeImportLedBodyRows`, `parseImportLedBodyRows`, and their removed second/third import-row continuation aliases in `ParserParityParser.mlfp`.
- Step 2, add narrow bounded helper family: met. `parseBoundedOneImportRows`, `parseBoundedThreeImportRows`, `parseBoundedImportRows`, `parseBoundedImportRowsNext`, `appendBoundedImportRowsAndContinue`, and explicit remaining-budget entry points are present.
- Step 3, migrate `parseThreeImportLedBodyRows`: met. It now enters `parseBoundedThreeImportRows`, which parses exactly three import projection rows through explicit remaining budgets before `parseImportedBodyAfterImport`.
- Step 4, migrate `parseImportLedBodyRows`: met. It now enters `parseBoundedOneImportRows`, which parses one import projection row before the existing `parseImportedBodyAfterImport` continuation.
- Step 5, remove migrated second/third import aliases: met. Static guard confirms `parseThreeImportSecondRows`, `appendThreeImportSecondRows`, `parseThreeImportThirdRows`, and `appendThreeImportThirdRows` are absent from parser-library source.
- Step 6, keep unrelated parser surfaces unchanged: met. `git diff --name-status` is confined to `ParserParityParser.mlfp`, `ProgramParserParitySpec.hs`, `CHANGELOG.md`, `implementation_notes.md`, and round artifacts; no production parser, checker, resolver, backend, package, platform, proof, native, cabal, test registration, package metadata, or generated batch-routing files changed.
- Step 7, add focused static coverage: met. `ProgramParserParitySpec.hs` adds the named bounded import-row sequencing Hspec guard, helper/use phrase lists, guard phrase enrollment, and alias absence list.
- Step 8, update docs with bounded non-claims: met. `CHANGELOG.md`, root `implementation_notes.md`, and round `implementation-notes.md` describe bounded compiler-frontend/parser ergonomics substrate and explicitly avoid full parser parity, compiler-package implementation, platform/proof progress, native/backend completion, package-manager/linker work, and self-boot completion.
- Step 9, run focused verification and record evidence: met. All focused checks above passed, and the implementer evidence is recorded in `orchestrator/rounds/round-351/implementation-notes.md`.

### Findings
- Blocking: no
  Problem: No blocking findings.
  Evidence: All required focused checks passed; the diff matches the selected bounded import-row sequencing substrate plan; changed docs use explicit non-claim language.
  Suggested fix: none

### Decision
**APPROVED**

### Retry
- Retry target: none
- Required changes: none

### Roadmap Closeout
- Mode: none
- Status changes: none
- Completion pointers: none
- History entries: none
- Semantic update reason: none

### Evidence
The integrated diff introduces a bounded import-row helper family in `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`, migrates the selected one-import and three-import module-body paths, and removes the migrated second/third import-row aliases instead of leaving compatibility wrappers. The helper parses import projection rows through `parseImportProjectionRows`, accumulates later rows through `appendProjectionValues`, and preserves the existing `parseImportedBodyAfterImport` continuation for post-import body parsing.

`test/ProgramParserParitySpec.hs` adds a focused static guard for the helper surface, representative migrated call sites, guard phrase enrollment, and absence of removed aliases from parser-library source. The focused Hspec selector passed with 1 example and 0 failures. The aggregate `MLF.Program parser parity` selector passed with 78 examples and 0 failures.

The changed docs state this is bounded compiler-frontend/parser ergonomics substrate only. They do not claim full parser parity, compiler-package implementation, platform/proof progress, native/backend completion, package-manager/linker work, or self-boot completion. Under `orchestrator/active-roadmap-bundle.md`, this approved non-simple round does not require status-only closeout because milestone 4 remains in progress and the round does not complete a milestone selector. It also does not require a semantic roadmap update because the active rev-007 direction already authorizes bounded ergonomics/library substrate work.

Focused verification is sufficient for this non-closeout slice because the diff is confined to the shared parser-owned parser-parity library, its Hspec static guard, bounded docs, and round artifacts. It does not change production parser/checker/resolver/backend/package/platform/proof/native code, does not add cabal modules or spec modules, does not claim milestone closeout, and does not claim package/platform/proof/native/backend/self-boot progress. Full closeout gates, `cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh` are therefore not required by the active focused profile for this round.
