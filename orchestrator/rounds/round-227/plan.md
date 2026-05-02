# Round 227 Plan

- Round: `round-227`
- Roadmap:
  `2026-05-02-00-backend-ir-executable-boundary-roadmap` / `rev-001`
- Milestone: `milestone-6`
- Direction: `direction-6a-freeze-polymorphism-lowerability-contract`
- Extracted item: absent
- Retry: `null`
- Actionable slice: `bounded production proof slice`
- Execution shape: serial, one milestone-6 polymorphism-lowerability
  settlement slice only, shared `dist-newstyle/` discipline, no worker
  fan-out, no controller-state edits

## Objective

Close mechanism-table row 6 on top of accepted `round-226` / merged commit
`b4e239c5` by freezing the current boundary between permissive checked
`MLF.Backend.IR` polymorphic shapes and the smaller LLVM-lowerable executable
subset.

The accepted row-6 outcome must make these points explicit and mechanically
reviewable:

- `BackendTyAbs` and `BackendTyApp` remain lawful checked backend-IR nodes;
  `MLF.Backend.Convert` may preserve them instead of prematurely erasing
  polymorphism.
- LLVM lowering remains narrower than checked `Backend.IR`: only fully
  specialized or otherwise fully instantiated polymorphic executable paths may
  reach emitted LLVM/native code.
- Supported row-6 success paths stay on the current static-specialization lane
  already implemented in `MLF.Backend.LLVM.Lower`, including complete type
  applications of globals/locals and the already-supported first-class
  polymorphic lowering cases.
- Residual runtime polymorphism remains intentionally unsupported and must fail
  with explicit backend diagnostics instead of silently lowering, inventing a
  fallback runtime path, or widening the backend boundary.

This round must stay strictly inside `milestone-6`. Do not widen into
row-7 closeout work except for the minimum mechanism-table next-action update.
Do not authorize runtime polymorphism, a second public backend IR, a public
lowering surface, lazy STG machinery, fallback runtime paths, or any broader
lowerability contract than the existing specialization-based backend actually
earns.

## Decision

This round should be behavior-preserving but code/test-bearing rather than
docs-only.

Why:

- `src/MLF/Backend/IR.hs` and `src/MLF/Backend/Convert.hs` already permit and
  preserve polymorphic backend nodes, while `src/MLF/Backend/LLVM/Lower.hs`
  already contains the real lowerability behavior through
  `collectRequiredSpecializations`,
  `lowerTyApp`,
  `lowerGlobalValue`,
  `lowerFunction`,
  `lowerExpr`,
  `resolveTypeArguments`,
  `lowerBackendProgramNative`, and
  `nativeRenderableKind`.
- the current row-6 gap is not the absence of polymorphic backend support; it
  is that the lowerable subset and the residual unsupported subset are still
  partly implementation behavior rather than one explicit named backend
  contract.
- `test/BackendLLVMSpec.hs` already proves several supported polymorphic
  lowering paths and at least one rejection path, but row 6 still lacks a
  single bounded evidence packet that says which paths are supported, which are
  rejected, and which diagnostics are stable enough to count as contract.
- a docs-only round would leave mechanism-table row 6 resting on implicit
  lowerer behavior and scattered diagnostics instead of explicit supported and
  unsupported evidence.

The bounded fix is therefore: publish the row-6 specialization-versus-rejection
contract across the durable backend surfaces, tighten only any ambiguous
row-6 diagnostics the focused tests expose, add focused LLVM plus repository
guard evidence, and then flip row 6 only if the resulting evidence is honest.

## Authorized Write Scope

Modify only these repo-facing files:

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

Use these as read-only context only; do not edit them in this round unless a
new row-6 defect proves the current permissive-side evidence false:

- `test/BackendIRSpec.hs`
- `test/BackendConvertSpec.hs`

Do not create or modify:

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-227/worker-plan.json`
- `AGENTS.md`
- `README.md`
- `Bugs.md`
- `CHANGELOG.md`
- `src-public/**`
- `src/MLF/Backend/LLVM.hs`
- `src/MLF/Backend/LLVM/Syntax.hs`
- `src/MLF/Backend/LLVM/Ppr.hs`
- `test/Main.hs`
- `mlf2.cabal`
- any new `LowerableBackend.*` surface, second executable IR, lazy-runtime
  surface, runtime-polymorphism implementation, or fallback lowering/runtime
  path

## Locked Context

- Active selection:
  `orchestrator/rounds/round-227/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- Roadmap row anchor:
  row 6, `Polymorphism erasure and lowerability`, is the first lawful `NO` in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- Retry state: `null`; there is no same-round retry delta to honor
- Existing permissive-side evidence already present and read-only in this
  round:
  `test/BackendIRSpec.hs` validates backend type abstraction/application
  invariants, and
  `test/BackendConvertSpec.hs` already proves checked-program conversion can
  preserve polymorphic backend nodes. The row-6 gap sits in the explicit
  lowerability contract and LLVM-facing diagnostics, not in a missing second
  backend-IR layer.

Current worktree state is already non-pristine. Respect controller-owned edits
and do not revert them:

- `M orchestrator/retry-subloop.md`
- `M orchestrator/roadmap.md`
- `M orchestrator/state.json`
- `M orchestrator/verification.md`
- `?? orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
- `?? orchestrator/rounds/round-227/`

## Worker Fan-out

Worker fan-out is not required. Keep `worker_mode = none` and do not create
`orchestrator/rounds/round-227/worker-plan.json`.

## Sequential Plan

### Task 1: Publish the row-6 lowerability contract across the durable backend surfaces

**Files**

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`

**Required edits**

- In `docs/architecture.md`, extend the typed-backend-boundary section so it
  states explicitly that checked `Backend.IR` may still contain
  `BackendTyAbs` / `BackendTyApp`, but LLVM/native lowering only owns the
  smaller fully-specialized executable subset. Make it explicit that complete
  type applications may be specialized privately inside the lowerer, while
  residual runtime polymorphism remains unsupported.
- In `src/MLF/Backend/IR.hs`, tighten the boundary note so row 6 is named at
  the IR boundary: the IR remains permissive enough to carry explicit
  polymorphic nodes after checked conversion, but that permissiveness is not a
  promise that all such nodes are LLVM-lowerable.
- In `src/MLF/Backend/Convert.hs`, publish that checked-program conversion
  preserves representable polymorphic backend structure in
  `BackendTyAbs` / `BackendTyApp` instead of erasing it just to satisfy LLVM,
  and that LLVM lowerability is a downstream concern rather than a reason to
  invent a second IR or broaden conversion authority.
- In `src/MLF/Backend/LLVM/Lower.hs`, add a dedicated row-6 note and, if
  useful, a tiny private helper that centralizes the supported-versus-rejected
  contract around
  `collectRequiredSpecializations`,
  `lowerTyApp`,
  `lowerGlobalValue`,
  `lowerFunction`,
  `lowerExpr`,
  `resolveTypeArguments`,
  `lowerBackendProgramNative`, and
  `nativeRenderableKind`.
  The published rule should be: fully-instantiated polymorphic executables may
  lower through static specialization or direct instantiation, while residual
  runtime polymorphism must fail with explicit diagnostics.
- In `docs/backend-native-pipeline.md`, add the test-facing row-6 statement so
  raw/native emission still consumes the same `MLF.Backend.IR` program, but
  emitted LLVM/native code is only promised for the specialized lowerable
  subset. Unsupported polymorphic `main` bindings or result shapes must fail
  before emission rather than silently remaining successful inspection rows.

### Task 2: Lock row-6 success and rejection behavior with focused LLVM and guard evidence

**Files**

- `src/MLF/Backend/LLVM/Lower.hs`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

**Required edits**

- In `src/MLF/Backend/LLVM/Lower.hs`, tighten only the row-6 boundary where
  the new focused tests expose ambiguity. Do not add runtime polymorphism,
  fallback lowering, lazy rescue, or a second IR. Restrict behavior changes to
  stabilizing the existing specialization/rejection contract.
- In `test/BackendLLVMSpec.hs`, add or consolidate a dedicated
  `describe "polymorphism lowerability contract"` section so row-6 evidence is
  easy to review as one bounded packet.
- The focused supported rows in that section must cover, at minimum:
  complete type application of top-level polymorphic zero-arity bindings;
  local/direct type-application specialization when type applications cross
  let heads or otherwise need the existing lowerer push-through path;
  first-class polymorphic arguments that lower statically instead of escaping
  as runtime polymorphic values; and
  one specialization path that still qualifies/collects closure entries
  correctly.
- The focused rejection rows in that same section must cover, at minimum,
  stable explicit diagnostics for the residual unsupported subset:
  `polymorphic main binding`,
  `unspecialized polymorphic binding`,
  `escaping type abstraction` or `escaping polymorphic binding`, and
  `partial type application`.
  Use the real lowerer/native entrypoints and `renderBackendLLVMError` where
  appropriate; do not bury row-6 evidence in unrelated test helpers only.
- In `test/RepoGuardSpec.hs`, add one repository guard named exactly:
  `polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary`.
  The guard must read
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`,
  and fail if the synchronized row-6 markers disappear or if those surfaces
  stop saying that:
  checked `Backend.IR` may carry polymorphic nodes,
  LLVM lowering owns only the specialization-based lowerable subset, and
  unsupported residual polymorphism stays on explicit diagnostics without
  widening the backend boundary.

### Task 3: Refresh mechanism-table row 6 only

**File**

- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`

**Required edits**

- Refresh the `Last updated (UTC)` line if the artifact changes.
- Update only the row `Polymorphism erasure and lowerability` to reflect the
  landed row-6 contract freeze:
  replace the current hidden-assumption gap summary with accepted-contract
  language,
  expand the evidence column to cite the synchronized doc/module-note surfaces
  plus the focused `BackendLLVMSpec` and `RepoGuardSpec` coverage, and
  flip that row from `NO` to `YES` only if Task 2 evidence passes honestly.
- Keep the accepted row-6 wording honest about preserved limits:
  `BackendTyAbs` / `BackendTyApp` remain allowed in checked `Backend.IR`,
  LLVM/native lowering only promises the already-supported specialization lane,
  and this row does not authorize runtime polymorphism, a second executable
  IR, a public lowering surface, lazy STG machinery, or fallback runtime
  paths.
- Change that row’s `Next action` so the next live blocker points at
  `milestone-7` / row 7 closeout synchronization only. Do not edit row 7’s
  gate or broaden the round into row-7 closeout work itself.
- Keep mechanism order fixed. Keep rows 1 through 5 at `YES` and row 7 at
  `NO`.

## Binary Acceptance Criteria

- PASS if the only repo-facing implementation edits are the eight files named
  in `Authorized Write Scope`.
- PASS if `docs/architecture.md`, `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs` all agree on the same row-6 contract:
  checked `Backend.IR` may remain polymorphic,
  LLVM/native lowering owns only the specialization-based lowerable subset,
  and residual runtime polymorphism stays on explicit diagnostics.
- PASS if `test/BackendLLVMSpec.hs` proves at least one supported top-level
  specialization lane, one supported local or closure-sensitive specialization
  lane, one supported first-class polymorphic lowering lane, and the named
  unsupported diagnostics for residual polymorphic executables.
- PASS if `test/RepoGuardSpec.hs` enforces the synchronized row-6 markers
  without authorizing a public `LowerableBackend.IR`, runtime polymorphism, or
  another executable IR.
- PASS if the mechanism table flips row 6 to `YES`, points the next action at
  row 7, and leaves row 7 itself at `NO`.
- FAIL if the round introduces runtime polymorphism, a second backend/public
  IR, a public lowering surface, lazy STG machinery, fallback runtime paths,
  broad row-7 closeout edits, worker fan-out, or any controller-state /
  roadmap-history change.

## Verification Commands

Run these in order:

1. Diff hygiene:

```sh
git diff --check
```

2. Focused row-6 LLVM evidence slice:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/polymorphism lowerability contract/"'
```

3. Focused repository guard for the synchronized row-6 contract:

```sh
cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary/"'
```

4. Full repo gate required because `src/` and `test/` surfaces are in scope:

```sh
cabal build all && cabal test
```
