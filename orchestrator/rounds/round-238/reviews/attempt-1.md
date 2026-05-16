### Checks Run
- Command: `git diff --check`
  Result: pass; no whitespace, conflict-marker, or diff-hygiene failures.
- Command: `cabal build mlf2-test`
  Result: pass; built `mlf2-test` with GHC 9.14.1.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'MLF.Primitive.Inventory'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'MLF.Backend.IR'`
  Result: pass; `34 examples, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'accepts backend conversion when pure bindings reference IO primitives'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'accepts backend conversion when pure bindings reference IORef primitives'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'matches the checked backend IR snapshot for a primitive function program'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'typechecks direct IO bind primitive uses with consistent arguments'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'rejects constructor imports for opaque Prelude IO'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'primitive inventory ownership stays centralized across frontend and backend adapters'`
  Result: pass; `1 example, 0 failures`.
- Command: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match 'primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary'`
  Result: pass; `1 example, 0 failures`.
- Command: `cabal build all && cabal test`
  Result: pass; `2579 examples, 0 failures` in `375.0630s`.

### Plan Compliance
- Step 1 audit the live primitive inventory facts and define the owner surface: met. `src/MLF/Primitive/Inventory.hs` now owns `builtinTypeSpecs`, `primitiveValueSpecs`, builtin qualification/canonicalization, and opaque-builtin queries.
- Step 2 introduce the private owner and register it in Cabal: met. `MLF.Primitive.Inventory` is added under `library mlf2-internal` with `visibility: private`, and the new focused spec is wired into both `mlf2.cabal` and `test/Main.hs`.
- Step 3 rewire the selected source/backend adapters without widening public surfaces: met. `MLF.Frontend.Program.Builtins`, `MLF.Frontend.Program.Resolve`, `MLF.Backend.IR`, and `MLF.Backend.Convert` now consume `MLF.Primitive.Inventory`; no `src-public/`, CLI, or milestone-2 callable-shape files changed.
- Step 4 update docs and focused regression coverage: met. `docs/architecture.md` and `docs/backend-native-pipeline.md` describe the new owner and keep lowerer-local wrapper/runtime implementation details explicit; focused coverage was added in `PrimitiveInventorySpec`, `BackendIRSpec`, `BackendConvertSpec`, and `RepoGuardSpec`.
- Step 5 run focused checks, `git diff --check`, and the full Cabal gate: met. All focused selectors passed, `git diff --check` passed, and `cabal build all && cabal test` passed.

### Decision
**APPROVED**

Implemented stage result: `pass`  
Attempt verdict: `accepted`  
Stage action: `finalize`  
Retry reason: `none`  
Fix hypothesis: `none`

### Evidence
- Scope discipline held. `git diff --name-only -- src-public app` was empty, and `git diff --name-only --` against the inherited recursive-inference plan files, `Bugs.md`, and rounds `094` through `098` was also empty. The round does not reopen cyclic search, multi-SCC search, fallback paths, second interfaces, or broad capability widening.
- Ownership moved into one private source/backend owner. The prior builtin-type and primitive-signature literal tables were removed from `MLF.Frontend.Program.Builtins`, `MLF.Backend.IR`, and `MLF.Backend.Convert`; those modules now derive from `MLF.Primitive.Inventory`.
- The remaining lowerer/runtime primitive-name tables are still adapter-local implementation detail, not a competing typing authority. `MLF.Backend.LLVM.Lower` retains wrapper/entry/runtime-name dispatch, while the updated docs now state that typing/inventory ownership lives in `MLF.Primitive.Inventory` and lowerer wrapper/runtime implementation details remain lowerer-local.
- No public primitive facade or second backend IR was introduced. The new owner lives in `library mlf2-internal` with `visibility: private`, `src-public/` is unchanged, and the backend still uses the existing `BackendVar` / `BackendApp` / `BackendTyApp` seam.
- Milestone-1 is not fully complete yet. The selected extracted item closes the private-owner plus source/backend-adapter slice, but the roadmap milestone still names broader lowering/CLI/native-policy adapter alignment. Status-only closeout should therefore move `milestone-1` from `pending` to `in-progress`, not `done`.
- The inherited non-cyclic-graph and public-output continuity family remains intact as bounded predecessor evidence only. This round did not edit those docs or review records, and the full test gate still passed the repository guardrails plus the recursive-inference continuity harnesses (`C1`, `P2`, and `P5` suites) that protect those settled boundaries.
- Verification side effect was contained. `cabal build all && cabal test` rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` to the round worktree path; that one-line generated depfile path was restored before review closeout, leaving only the intentional round changes in the final diff.
