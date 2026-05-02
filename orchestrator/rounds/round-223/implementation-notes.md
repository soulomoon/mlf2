# Round 223 Implementation Notes

## Changed files

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `test/RepoGuardSpec.hs`
- `orchestrator/rounds/round-223/implementation-notes.md`

## Change summary

- Froze the milestone-2 / `direction-2a-pin-eager-runtime-contract` wording across the backend architecture note, backend IR note, conversion header, LLVM lowering notes, and native pipeline guide.
- Refreshed only mechanism-table row 2 to `YES`, citing the synchronized contract surfaces plus the new repository guard.
- Added the focused repository guard `eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope`.
- Preserved controller-owned orchestrator files outside this round artifact.

## Verification commands run

1. `git diff --check`
   - Pass.

2. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope/"'`
   - Failed during iteration while the new guard exposed wording drift in `src/MLF/Backend/Convert.hs`.
   - Final pass: `1 example, 0 failures`.

3. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/validates explicit closure construction and indirect closure calls/"'`
   - Pass: `1 example, 0 failures`.

4. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/native process entrypoint/"'`
   - Pass: `7 examples, 0 failures`.

5. Mechanism-table gate:
   - Command:
     ```sh
     table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
     test "$(awk -F'|' '$2 == " IR role separation and non-duplication " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
     test "$(awk -F'|' '$2 == " Eager runtime lowering contract " {gsub(/ /, "", $7); print $7}' "$table")" = "YES"
     for mechanism in \
       'Direct calls, closure values, and callable shapes' \
       'ADT/case semantics versus layout' \
       'Primitive operations and eager evaluation order' \
       'Polymorphism erasure and lowerability' \
       'Validation, evidence, and guidance synchronization'
     do
       test "$(awk -F'|' -v m="$mechanism" '$2 == " " m " " {gsub(/ /, "", $7); print $7}' "$table")" = "NO"
     done
     ```
   - Pass: row 1 = `YES`, row 2 = `YES`, rows 3 through 7 = `NO`.

6. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/"'`
   - Failed during iteration while restoring the pre-existing row-1 `MLF.Backend.Convert` marker strings after the new row-2 wording landed.
   - Final pass: `11 examples, 0 failures`.

7. `cabal build all && cabal test`
   - Failed once during iteration for the same repository-guard marker drift in `src/MLF/Backend/Convert.hs`; fixed before closeout.
   - Final pass: `2340 examples, 0 failures`.

## Residual risk

- The new repository guard is intentionally textual. Future wording-only edits across the synchronized backend contract surfaces will need coordinated updates to keep the row-2 contract explicit.
- During the first focused rebuild, GHC reported pre-existing `-Wtype-defaults` warnings in `src/MLF/Backend/LLVM/Lower.hs` around `emitMallocLocal` numeric literals (lines 934, 935, 980, 981, 1034, 1035 in that build output). This round did not modify those executable sites.
