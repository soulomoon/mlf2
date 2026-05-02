# Round 222 Implementation Notes

## Changed Files

- `docs/architecture.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/backend-native-pipeline.md`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/RepoGuardSpec.hs`
- `orchestrator/rounds/round-222/implementation-notes.md`

## Change Summary

- Froze the one-backend-IR contract across the approved architecture and
  backend-owned module-note surfaces, with one shared future-lower-IR criteria
  list.
- Refreshed only mechanism-table row 1 to `YES`, updated its evidence column,
  and pointed its next action at milestone-2 / row-2 eager-runtime work while
  leaving rows 2 through 7 at `NO`.
- Added the focused `Repository guardrails` example
  `one-backend-IR contract stays explicit and no public lower IR leaks`, which
  checks the synchronized wording markers and rejects any public-library
  exposure of `MLF.Backend.*` or `LowerableBackend.*`.
- Left controller-owned files untouched.

## Verification Commands

1. `git diff --check`
   - Result: pass.

2. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/one-backend-IR contract stays explicit and no public lower IR leaks/"'`
   - First run: failed.
   - Exact failure: `src/MLF/Backend/Convert.hs missing marker: "unsupported checked shapes must fail here instead of being rerouted"`.
   - Fix: tightened the new textual guard markers to line-wrap-safe fragments in
     `test/RepoGuardSpec.hs`.
   - Second run: pass.
   - Exact result: `1 example, 0 failures`.

3. Mechanism-table gate:

   ```sh
   table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
   test "$(awk -F'|' '$2 ~ /^ IR role separation and non-duplication / {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
   for mechanism in \
     'Eager runtime lowering contract' \
     'Direct calls, closure values, and callable shapes' \
     'ADT/case semantics versus layout' \
     'Primitive operations and eager evaluation order' \
     'Polymorphism erasure and lowerability' \
     'Validation, evidence, and guidance synchronization'
   do
     test "$(awk -F'|' -v m="$mechanism" '$2 == " " m " " {gsub(/ /, "", $7); print $7}' "$table")" = "NO"
   done
   ```

   - Result: pass.

4. `cabal build all && cabal test`
   - Result: pass.
   - Exact result: `2339 examples, 0 failures`.

## Residual Risk

- The new repo guard is intentionally textual. Future wording refactors across
  the synchronized contract surfaces will need matching marker updates in
  `test/RepoGuardSpec.hs`.
- During the first focused rebuild, GHC surfaced pre-existing
  `[-Wtype-defaults]` warnings in `src/MLF/Backend/LLVM/Lower.hs` at lines
  923, 924, 969, 970, 1023, and 1024 around `emitMallocLocal`. The round did
  not address that warning debt because it was outside the planner-authored
  contract-freeze scope, and the required full gate still passed.
