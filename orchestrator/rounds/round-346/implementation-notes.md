### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: replaced the copied annotated-lambda RHS depth ladder with `parseBoundedAnnotatedLambdaRhsExpressionWithBody` and explicit bounded body-expression entry points. The helper still parses `lambda`, annotated parameter name, `:`, `parseSourceType`, `)`, the bounded RHS body, and renders through `finishAnnotatedLambdaExpression`.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: removed migrated numbered annotated-lambda RHS plumbing names (`parseAnnotatedLambdaRhsExpression5` through `parseAnnotatedLambdaRhsExpression1` and their open/param/type/close/body companions) instead of leaving aliases.
- `test/ProgramParserParitySpec.hs`: added a focused static guard proving the bounded annotated-lambda RHS helper surface exists, nested RHS body paths use it down to depth 0, and the migrated numbered aliases are absent from parser-library sources.
- `orchestrator/rounds/round-346/implementation-notes.md`: recorded implementation evidence for the implementer stage.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies the annotated-lambda RHS substrate is present, used by bounded nested RHS call sites, and not backed by migrated compatibility aliases.
- `git diff --check`: PASS.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS; `73 examples, 0 failures`; Hspec reported `Finished in 8764.0795 seconds`.
- `bash -lc 'set -euo pipefail
parser=test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp
combinator=test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp
spec=test/ProgramParserParitySpec.hs
for phrase in \
  "def parseBoundedAnnotatedLambdaRhsExpressionWithBody" \
  "def parseBoundedAnnotatedLambdaRhsBodyWithBody" \
  "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression5 ValueUnit" \
  "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression0 ValueUnit" \
  "shared parser-owned .mlfp parser shares bounded annotated lambda RHS depth handling" \
  "sharedParserRemovedAnnotatedLambdaRhsAliases"; do
  rg -F "$phrase" "$parser" "$combinator" "$spec" >/dev/null
done
if rg -n "parseAnnotatedLambdaRhs(Expression[1-5]|Open[1-5]|Param[1-5]|ParamColon[1-5]|ParamType[1-5]|ParamClose[1-5]|Body[1-5])" "$parser" "$combinator"; then
  exit 1
fi
printf "static guard passed\n"'`: PASS; printed `static guard passed`.
- `bash -lc 'set -euo pipefail
git diff -U0 -- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/ProgramParserParitySpec.hs orchestrator/rounds/round-346/implementation-notes.md |
  rg "^\+" |
  rg -v "^\+\+\+" |
  rg -v "^\+\s*(rg|exit|fi|done|for |parser=|combinator=|spec=|\"|git diff)" |
  rg -i "(fixture-name shortcut|pre-rendered|static negative evidence|canonical parser bypass|compiler-package|platform/proof|native/backend|backend/native|package-manager|linker|self-boot|full parser parity)" |
  rg -vi "(no |not |non-claims|without|not required)" &&
  exit 1 || exit 0'`: PASS.

### Notes
This is bounded parser-library/spec/docs substrate only. No compiler-package, platform/proof, native/backend, package-manager/linker, self-boot, or full parser parity claim is made. The full `cabal build all && cabal test` and thesis gate were not required by the focused plan because this did not widen beyond parser-library/spec/docs and did not touch production parser, checker, resolver, backend, package, platform, proof, native, or thesis-facing semantic behavior.
