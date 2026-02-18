# Findings: A5 (P3) Totality and Harness Hardening

## 2026-02-18 initial findings
- `TODO.md` identifies two linked A5 targets:
  - line 263: totalize STCon coercion-copy path and remove remaining partial branch.
  - line 393/394: remove totality/harness footguns; AC requires no partial `error` in frontend STCon path and non-silent presolution umbrella wiring.
- Current harness wiring gap:
  - `mlf2.cabal` lists `PresolutionSpec` in `test-suite mlf2-test` `other-modules`.
  - `test/Main.hs` does not import or execute `PresolutionSpec.spec`; it wires child `Presolution.*` modules directly.
  - This allows accidental omission of umbrella-level wiring to pass silently.
- Current coercion-copy code observations (`src/MLF/Frontend/ConstraintGen/Translate.hs`):
  - STCon branch is recursive and uses `NonEmpty` args.
  - Bare `ECoerceConst` path still uses `InternalConstraintError` string payload, which is a footgun vs explicit typed errors.
- Existing coverage already includes coercion semantics and STCon structure tests in `test/ConstraintGenSpec.hs`; A5 can extend these for typed-failure and harness guarantees.

## 2026-02-18 completion findings
- Frontend coercion-copy typed error closure:
  - Added `UnexpectedBareCoercionConst` to `ConstraintError`.
  - `buildExprRaw` now throws the typed constructor for bare `ECoerceConst` instead of stringly `InternalConstraintError`.
- STCon coercion-copy totality hardening:
  - Refactored constructor argument traversal to total helper `internalizeConArgs` over `NonEmpty` arguments.
  - Removed in-branch `NE.head`/`NE.tail` accumulator pattern in `STCon` handling while preserving bind-parent and `SharedEnv` behavior.
- Harness wiring closure:
  - `test/Main.hs` now uses `PresolutionSpec.spec` as the single presolution entrypoint.
  - Added fail-fast harness check in `test/Main.hs`: marker is set before presolution umbrella wiring and verified via `runIO`; test binary aborts if umbrella wiring is removed.
- Regression coverage closed:
  - `bare ECoerceConst rejects with typed UnexpectedBareCoercionConst (not InternalConstraintError string)`.
  - `STCon coercion-copy failures surface as typed errors`.
  - `nested STCon coercion-copy preserves binding-tree validity`.
- Verification highlights:
  - Presolution-focused and non-presolution filtered runs pass with harness guard in place.
  - Full gate `cabal build all && cabal test` passes.
