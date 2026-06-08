### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: replaced the copied source-type arrow-tail/codomain text ladder with `parseBoundedSourceTypeArrowTailText` and `parseBoundedSourceTypeCodomainText`, plus explicit bounded budget entry points. The path still starts from `parseSourceTypeCodomain`, parses codomain atoms through `parseSourceTypeCodomainAtom`, renders through `appendSourceArrowTypeText`, stops when no next `->` is present, and keeps the existing budget-4 continuation to budget 2.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: removed the migrated `parseSourceTypeArrowTailText7` through `parseSourceTypeArrowTailText0` and `parseSourceTypeCodomainText6` through `parseSourceTypeCodomainText0` names instead of leaving compatibility aliases. Pre-edit reachability inspection showed `parseSourceTypeArrowTailText3` had no call site.
- `test/ProgramParserParitySpec.hs`: added focused static coverage proving the bounded source-type arrow-tail helper surface exists, the source-type codomain path uses it, the explicit budget-4 to budget-2 continuation remains present, and the migrated numbered aliases are absent from parser-library sources.
- `orchestrator/rounds/round-347/implementation-notes.md`: recorded implementation evidence for the implementer stage.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies helper presence/use, absence of migrated source-type arrow-tail/codomain aliases, and aggregate parser-parity behavior.
- `git diff --check`: PASS.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded source-type arrow-tail text accumulation"'`: PASS; `1 example, 0 failures`; Hspec reported `Finished in 0.1137 seconds`.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS; `74 examples, 0 failures`; Hspec reported `Finished in 8577.6443 seconds`.
- `bash -lc 'set -euo pipefail
parser=test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp
combinator=test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp
spec=test/ProgramParserParitySpec.hs
for phrase in \
  "def parseBoundedSourceTypeArrowTailText : (String -> ParserValue -> Parser ParserValue) -> String -> ParserValue -> Parser ParserValue" \
  "def parseBoundedSourceTypeCodomainText : (String -> ParserValue -> Parser ParserValue) -> String -> ParserValue -> Parser ParserValue" \
  "parseBoundedSourceTypeArrowTailTextBudget7 (parserTextFromValue leftType)" \
  "parseBoundedSourceTypeArrowTailText parseBoundedSourceTypeCodomainTextBudget6" \
  "parseBoundedSourceTypeArrowTailText parseBoundedSourceTypeCodomainTextBudget2" \
  "shared parser-owned .mlfp parser shares bounded source-type arrow-tail text accumulation" \
  "sharedParserRemovedSourceTypeArrowTailAliases"; do
  rg -F "$phrase" "$parser" "$combinator" "$spec" >/dev/null
done
if rg -n "parseSourceTypeArrowTailText[0-7]|parseSourceTypeCodomainText[0-6]" "$parser" "$combinator"; then
  exit 1
fi
printf "source-type arrow-tail static guard passed\n"'`: PASS; printed `source-type arrow-tail static guard passed`.
- `bash -lc 'set -euo pipefail
pattern="fixture-name short""cut|pre-rendered proj""ection|static negative evi""dence|canonical parser by""pass|retired syntax sh""im|parser-private short""cut|compiler-""package|platform/""proof|native/""backend|backend/""native|package-""manager|link""er|self-""boot|full parser par""ity"
{
  git diff -U0 -- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/ProgramParserParitySpec.hs
  sed -n "1,220p" orchestrator/rounds/round-347/implementation-notes.md
} |
  rg -n "$pattern" |
  rg -vi "(no |not |non-claims|without|not required|does not|did not|is not)" &&
  exit 1 || exit 0'`: PASS.

### Notes
Non-claims: this is bounded parser-library/spec/docs substrate evidence only. No compiler-package, platform/proof, native/backend, package-manager/linker, self-boot, or full parser parity claim is made. The full `cabal build all && cabal test` and thesis gate were not required by the focused plan because this did not widen beyond parser-library/spec/docs and did not touch production parser, checker, resolver, backend, package, platform, proof, native, or thesis-facing semantic behavior.
