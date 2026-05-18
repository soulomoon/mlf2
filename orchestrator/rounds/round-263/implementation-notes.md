### Changes Made
- Loaded and used `/Users/ares/.agents/skills/tdd/SKILL.md` before editing; followed the plan's RED -> GREEN cycle.
- `test/ProgramConformanceCorpusSpec.hs`: added the focused Hspec example `shared conformance corpus validates check-program missing-import failure fixture`; extended the test-owned conformance helper to support `expect: pass` with `expected-stdout` and `expect: fail` with `expected-stderr`; preserved metadata-driven `command: check-program` dispatch through public `checkProgramArgs`.
- `test/conformance/mlfp/check-program/missing-import/fixture.meta`: added the minimal missing-import check-program fixture metadata with `expect: fail` and committed stderr path.
- `test/conformance/mlfp/check-program/missing-import/src/Main.mlfp`: added the missing-import source fixture.
- `test/conformance/mlfp/check-program/missing-import/expected/check-program.stderr`: added the committed expected stderr oracle.
- `test/conformance/mlfp/README.md`: narrowly documented the `expect: fail` / `expected-stderr` contract beside the existing pass/stdout contract.
- `orchestrator/rounds/round-263/implementation-notes.md`: recorded this implementation evidence.

### Tests
- RED: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates check-program missing-import failure fixture"'` compiled and ran the focused example before fixture data was added. Result: expected RED, 1 example / 1 failure at `test/ProgramConformanceCorpusSpec.hs:127:12`, `expected: True but got: False`, because `test/conformance/mlfp/check-program/missing-import/fixture.meta` was absent.
- GREEN: same focused command passed after adding the fixture data. Result: 1 example / 0 failures.
- Existing corpus: `cabal test mlf2-test --test-options='--match "shared conformance corpus validates"'` passed. Result: all 5 shared conformance examples passed, including the existing run-program and check-program fixtures.
- Expected-stderr/artifact checks: `find test/conformance/mlfp -maxdepth 7 -type f | sort` showed the new missing-import fixture files; metadata `rg` confirmed `fixture-id`, `package-root`, `search-paths`, `command`, `expect`, `normalization`, `stage-applicability`, and `expected-stderr`; source `rg` confirmed the copied missing-import source; `diff -u /tmp/round-263-expected-stderr test/conformance/mlfp/check-program/missing-import/expected/check-program.stderr` passed.
- No dynamic golden checks: `rg -n 'expect: fail|expected-stderr|expected-stdout|accept|bless|regen|regenerat|UPDATE|GOLDEN|actual-output|writeFile|appendFile' test/ProgramConformanceCorpusSpec.hs test/conformance/mlfp/README.md test/conformance/mlfp/check-program/missing-import` showed only the intended committed-oracle reads and README prohibition; no `writeFile` / `appendFile` paths were added.
- Closeout: `git diff --check` passed.
- Closeout: `cabal build all` passed.
- Closeout: `cabal test` passed. Result: 2568 examples / 0 failures.
- Closeout: `./scripts/thesis-conformance-gate.sh` passed; final script status was `PASS: thesis conformance anchors are green`.

### Notes
No production modules, parser/checker/backend/platform/driver/proof code, broad discovery harness, dynamic golden acceptance, or existing package fixtures were changed. The native runtime dependency file was dirtied by closeout gates and restored to its preexisting content. `orchestrator/state.json` remains controller-owned and was not edited by this implementer.
