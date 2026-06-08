### Checks Run
- Command: `git diff --check`
  Result: pass; no whitespace errors.

- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded case branch rows"'`
  Result: pass; 1 example, 0 failures, finished in 0.6239 seconds.

- Command:
  ```sh
  bash -lc '
  set -euo pipefail
  src=test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp
  comb=test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp
  required=(
    "def parseBoundedCaseBranchRows : (ParserValue -> Parser ParserValue) -> ParserValue -> ParserValue -> Parser ParserValue"
    "λ(branchParser : ParserValue -> Parser ParserValue)"
    "parserBind (branchParser ValueUnit)"
    "def parseBoundedCaseBranchRowsMoreOrClose8"
    "def parseBoundedCaseBranchRowsMoreOrClose1"
    "def appendBoundedCaseBranchRowsAndClose"
    "parseBoundedCaseBranchRows parseSourceCaseBranch scrutineeValue ValueUnit"
    "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch scrutineeValue ValueUnit"
    "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch4 scrutineeValue ValueUnit"
    "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch3 scrutineeValue ValueUnit"
    "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch2 scrutineeValue ValueUnit"
    "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch1 scrutineeValue ValueUnit"
    "parserBind (appendSourceCaseBranchText branchRows nextBranch)"
    "parseSourceCaseClose scrutineeValue"
  )
  for phrase in "${required[@]}"; do
    rg -Fq "$phrase" "$src" || { printf "missing required phrase: %s\n" "$phrase"; exit 1; }
  done
  if rg -n "parseSourceCaseMoreOrClose[1-8]|parseSourceCaseNextBranch[0-7]|appendSourceCaseBranchAndContinue[1-7]|appendSourceCaseBranchAndClose|parseNestedCaseBranchMoreOrClose[1-8](Depth[1-4])?|parseNestedCaseBranchNextBranch[0-7](Depth[1-4])?|appendNestedCaseBranchAndContinue[0-7]Depth[1-4]|appendNestedCaseBranchAndContinue[1-7]|appendNestedCaseBranchAndClose" "$src" "$comb"; then
    printf "removed alias name still present\n"
    exit 1
  fi
  printf "PASS: bounded case branch substrate present, all six call sites migrated, removed aliases absent from parser-library source\n"
  '
  ```
  Result: pass; bounded case branch substrate present, all six call sites migrated, removed aliases absent from parser-library source.

- Command:
  ```sh
  bash -lc '
  set -euo pipefail
  if git diff -U0 -- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp test/ProgramParserParitySpec.hs orchestrator/rounds/round-343/implementation-notes.md | rg -n "^\+.*(fixture-name|pre-render|pre rendered|static negative evidence|canonical-parser|canonical parser bypass|package-manager|linker|platform|proof|self-boot|self boot|full parser parity|native/backend|native|backend|retired syntax|parser-private|shortcut|bypass|compatibility alias|compiler-seed/package/platform/proof)"; then
    printf "forbidden shortcut or overclaim text found in added lines\n"
    exit 1
  fi
  printf "PASS: changed parser-library/spec/round-artifact lines add no shortcut or overclaim text\n"
  '
  ```
  Result: pass; no forbidden shortcut or overclaim text was added in changed parser-library/spec/round-artifact lines.

- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: pass; 70 examples, 0 failures, finished in 8743.9701 seconds.

### Plan Compliance
- Step 1: met. The review inspected the migrated case ladders and helper body; semicolon consumption, branch append order, branch budget, close handling, depth-specific branch parser selection, and final close-only behavior are preserved by `parseBoundedCaseBranchRows*`.
- Step 2: met. One owner-local helper family, `parseBoundedCaseBranchRows`, was added in `ParserParityParser.mlfp`. The final shape uses a branch-parser function directly; the implementation notes explain the first-order selector attempt and the focused/aggregate tests prove this higher-order path typechecks and runs in the `.mlfp` parser-library path.
- Step 3: met. Ordinary source cases call `parseBoundedCaseBranchRows parseSourceCaseBranch`; migrated source numbered helpers are absent from parser-library source.
- Step 4: met. Non-depth nested cases call `parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch`; migrated non-depth nested numbered helpers are absent from parser-library source.
- Step 5: met. Depth 4, 3, 2, and 1 nested case branch lists call the shared helper with their own `parseNestedCaseBranchInnerBranch{4,3,2,1}` parser boundaries.
- Step 6: met. `test/ProgramParserParitySpec.hs` adds the static example "shared parser-owned .mlfp parser shares bounded case branch rows" covering helper presence, migrated call sites, and removed alias absence.
- Step 7: met. `git diff --check`, focused static Hspec, focused parser-library static guard, shortcut/overclaim guard, and the aggregate `MLF.Program parser parity` gate all passed.

### Findings
- Blocking: no
  Problem: No blocking findings.
  Evidence: The diff is confined to `test/ProgramParserParitySpec.hs`, `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`, and round artifacts; required focused checks passed; no forbidden aliases, shortcuts, or overclaims were found.
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
Diff scope is inside the approved parser-library/spec/round-artifact surface: `test/ProgramParserParitySpec.hs` and `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`, plus `orchestrator/rounds/round-343/{plan.md,implementation-notes.md,review.md}`.

The bounded helper exists at `ParserParityParser.mlfp` lines 1741-1872. It starts at the same budget as the removed ladders (`parseBoundedCaseBranchRowsMoreOrClose8`), walks down through `MoreOrClose1` and `NextBranch0`, appends rows through `appendSourceCaseBranchText`, keeps ordinary close braces flowing through `finishSourceCaseExpression`, and keeps the final-budget close-only path through `parseSourceCaseClose`.

Call-site migration evidence:
- ordinary source case branch list: `parseBoundedCaseBranchRows parseSourceCaseBranch` at line 1408;
- non-depth nested case branch list: `parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch` at line 1557;
- nested depth branch lists: depth 4 at line 1597, depth 3 at line 1637, depth 2 at line 1677, and depth 1 at line 1717.

Non-backtracking remains plausible from existing parser combinator behavior: `parserChoice` retries only on `UnexpectedSourceText` and `ExpectedCompleteModule`; expectation-specific failures such as case branch arrow diagnostics are returned without trying the close-brace alternative. The new helper keeps the same semicolon-first then close-brace choice shape used by the removed ladders.

Focused profile remains sufficient under rev-007 verification because the implementation does not touch production parser/checker/resolver/backend/package/platform/proof/native code, does not add modules or widen Cabal wiring, and makes no milestone closeout, full parser parity, compiler-package, platform/proof, native/backend, package-manager/linker, or self-boot claim.
