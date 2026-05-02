# Backend IR Executable-Boundary Roadmap

Roadmap family: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
Revision: `rev-001`
Base branch: `master`
Created: 2026-05-02

## Goal

Continue the backend-IR executable-boundary family after accepted
`round-227` was squash-merged onto `master` as commit `710c92eb` with title
`Freeze backend polymorphism lowerability contract`.

The completed predecessor roadmap family
`2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
remains closed. This family still starts from `master`, not from the
predecessor `codex/automatic-recursive-type-inference` roadmap branch,
because the relevant backend-native pipeline work and the accepted
milestone-1 through milestone-6 contract freezes now all live on `master`.

This family owns one repo-local goal:

- keep xMLF as the thesis-faithful typed elaboration IR;
- keep `MLF.Backend.IR` as the single executable eager backend IR boundary;
- avoid a duplicate public `LowerableBackend.IR` unless later accepted
  evidence proves that a distinct public lower IR has invariants that cannot
  live inside `MLF.Backend.IR` or a private LLVM-lowering helper; and
- turn the seven mechanism-table rows in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
  from design-intent `NO` rows into evidence-backed `YES` rows in the fixed
  mechanism order.

The current baseline is now complete:

- accepted `round-222`, merged as `5365d975`, already synchronized the
  one-backend-IR contract across
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/backend-native-pipeline.md`, and
  `test/RepoGuardSpec.hs`;
- accepted `round-223`, merged as `006eb569`, synchronized the eager-runtime
  lowering contract across
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and `test/RepoGuardSpec.hs`, while keeping lazy STG machinery explicitly out
  of scope and flipping only row 2 to `YES`;
- accepted `round-224`, merged as `2c1661b3`, synchronized the callable-shape
  contract across
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendIRSpec.hs`,
  `test/BackendConvertSpec.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, while flipping only row 3 to `YES`;
- accepted `round-225`, merged as `5adb3702`, synchronized the ADT/case
  semantic-versus-layout ownership contract across
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, while flipping only row 4 to `YES`;
- accepted `round-226`, merged as `b4e239c5`, synchronized the
  primitive-operation and eager-evaluation-order contract across
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, while flipping only row 5 to `YES`;
- accepted `round-227`, merged as `710c92eb`, synchronized the
  polymorphism-erasure and lowerability contract across
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, while flipping only row 6 to `YES`;
- accepted `round-228`, squash-merged as `574b7d7e`, closed the
  validation/evidence/guidance ledger across
  `TODO.md`,
  `implementation_notes.md`,
  `CHANGELOG.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and `test/RepoGuardSpec.hs`, while preserving the accepted
  `one executable eager backend IR` /
  `no public \`LowerableBackend.IR\`` /
  `no lazy STG machinery` boundary and flipping only row 7 to `YES`;
- `src/MLF/Backend/IR.hs` already defines the typed backend IR and local
  validation rules;
- `src/MLF/Backend/Convert.hs` and `src/MLF/Backend/LLVM/Lower.hs` already
  carry substantive backend conversion and lowering behavior; and
- `test/BackendIRSpec.hs`, `test/BackendConvertSpec.hs`, and
  `test/BackendLLVMSpec.hs` already cover selected backend behavior.

The mechanism table therefore no longer has any remaining `NO` rows: rows 1
through 7 are now accepted evidence-backed `YES`. `rev-001` is complete, no
unfinished milestones remain, and any future backend boundary widening must
start from a later accepted roadmap revision rather than silently extending
this closed family.

## Outcome Boundaries

- Source of truth remains `papers/these-finale-english.txt`.
- xMLF remains the typed elaboration / theory-faithful IR. This family must
  not demote it into a disposable frontend convenience layer.
- `MLF.Backend.IR` remains the first backend-owned executable representation
  after checked-program acceptance. `MLF.Backend.Convert` remains the only
  checked-program to backend-IR conversion boundary unless a later accepted
  revision explicitly changes that contract.
- No second public backend IR is authorized in `src-public/` or elsewhere in
  the public surface during `rev-001`.
- Any ANF-like normalization, layout-only structure, or lowerability-only
  representation may live only as a private backend helper unless a later
  accepted revision proves a separate public boundary is required.
- No lazy STG machinery is authorized: no thunks, update frames, CAF update
  semantics, graph reduction, or implicit laziness rescue.
- Preserve the current private backend surface:
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `src/MLF/Backend/LLVM/Syntax.hs`,
  `src/MLF/Backend/LLVM/Ppr.hs`,
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the backend test modules as the primary evidence owners unless a later
  accepted revision widens that writable slice.
- `AGENTS.md` should change only if this family discovers a durable workflow or
  policy constraint that belongs in repo-wide guidance. Backend-architecture
  detail should otherwise stay in architecture docs and backend module notes.
- Keep predecessor roadmap families and revisions immutable. This family does
  not reopen `rev-027`; it starts after it.
- Preserve accepted `round-222`, merged as `5365d975`, as the milestone-1
  contract freeze that keeps xMLF as the typed elaboration IR, keeps
  `MLF.Backend.IR` as the single executable eager backend IR, and guards
  against any public `LowerableBackend.IR` leak unless a later accepted
  revision explicitly changes the boundary.
- Preserve accepted `round-223`, merged as `006eb569`, as the milestone-2
  contract freeze that keeps `MLF.Backend.IR` as the eager executable backend
  boundary, keeps LLVM/native lowering limited to downstream private
  closure/layout/runtime support for that same IR, and keeps lazy STG
  machinery out of scope unless a later accepted revision explicitly changes
  the boundary.
- Preserve accepted `round-224`, merged as `2c1661b3`, as the milestone-3
  callable-shape contract freeze that keeps `BackendApp` as the direct
  first-order call path, keeps `BackendClosureCall` as the explicit indirect
  closure path, requires shared callable-shape classification across backend
  validation, conversion, and lowering, and keeps malformed confused-call
  heads on explicit rejection paths unless a later accepted revision changes
  the boundary.
- Preserve accepted `round-225`, merged as `5adb3702`, as the milestone-4
  semantic-versus-layout ownership freeze that keeps `BackendData`,
  `BackendConstructor`, `BackendConstruct`, and `BackendCase` semantic-only at
  the `MLF.Backend.IR` boundary while keeping constructor tag assignment,
  runtime field layout, closure-field storage policy, and nullary-constructor
  representation private to LLVM/native lowering unless a later accepted
  revision changes the boundary.
- Preserve accepted `round-226`, merged as `b4e239c5`, as the milestone-5
  primitive/eager contract freeze that keeps the primitive surface closed to
  the reserved runtime bindings `__mlfp_and`, `__io_pure`, `__io_bind`, and
  `__io_putStrLn`, keeps those primitives represented through
  `BackendVar` / `BackendApp` / `BackendTyApp`, makes eager sequencing
  explicit for `BackendLet`, `BackendCase`, and direct/primitive call
  arguments, and keeps effect sequencing explicit through `__io_bind` without
  authorizing `BackendPrim`, a public lowering surface, or broad FFI
  expansion unless a later accepted revision changes the boundary.
- Preserve accepted `round-227`, merged as `710c92eb`, as the milestone-6
  polymorphism-lowerability contract freeze that keeps checked
  `BackendTyAbs` / `BackendTyApp` lawful at the `MLF.Backend.IR` boundary,
  keeps LLVM/native lowering limited to the specialization-based lowerable
  subset, requires residual runtime polymorphism to fail with explicit
  diagnostics before emission, and does not authorize runtime polymorphism, a
  second executable IR, or a public lowering surface unless a later accepted
  revision explicitly changes the boundary.
- This family remains intentionally serial.

## Global Sequencing Rules

- The fixed mechanism order in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
  is authoritative. Do not reorder the rows.
- Every round starts with a verifier refresh of the mechanism table in that
  fixed order. The first `NO` row is the only lawful round anchor.
- `milestone-1` is complete on merged commit `5365d975` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- `milestone-2` is complete on merged commit `006eb569` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- `milestone-3` is complete on merged commit `2c1661b3` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- `milestone-4` is complete on merged commit `5adb3702` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- `milestone-5` is complete on merged commit `b4e239c5` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- `milestone-6` is complete on merged commit `710c92eb` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- `milestone-7` is complete on merged commit `574b7d7e` and may not be
  reopened as live debt unless a later accepted roadmap revision explicitly
  changes the family boundary.
- Do not claim a later mechanism complete while an earlier mechanism still
  lacks accepted evidence.
- `rev-001` now has no remaining lawful round anchor because the fixed-order
  mechanism table holds all seven rows at `YES`.
- Docs-only rounds are allowed when the active mechanism is a documentation or
  contract-publication gap. Code- or test-bearing rounds must carry focused
  backend evidence and satisfy the repo-wide build/test gate before approval.
- If accepted evidence shows that a second public backend IR, a broader public
  lowering boundary, or lazy-runtime semantics are actually required, do not
  silently continue into the next milestone. Return to `update-roadmap` and
  publish a same-family successor revision with the explicit new boundary.
- Because this revision is complete, any future work in this family must begin
  with a later accepted roadmap revision instead of continuing inside
  `rev-001`.
- The external orchestrator prompt and JSONL template in
  `docs/prompts/2026-05-02-backend-ir-executable-boundary-orchestrator.prompt.md`
  and
  `docs/plans/2026-05-02-backend-ir-executable-boundary-orchestrator-log-template.jsonl`
  are evidence and coordination references for this family. They do not
  replace the repo-local roadmap or authorize parallel repo-local rounds.

## Parallel Lanes

- `lane-main`: default serial lane for the full family.

## Milestones

### 1. [done] Encode the one-backend-IR role-separation contract

- Milestone id: `milestone-1`
- Depends on: none
- Intent: make the non-duplication contract durable by stating, in the
  authoritative repo surfaces, that xMLF remains the typed elaboration IR and
  `MLF.Backend.IR` is the only executable eager backend IR unless later
  evidence earns a new boundary.
- Completion signal: accepted evidence updates the architecture/backend notes
  and any focused guard needed to make the one-backend-IR contract explicit,
  reviewable, and hard to regress without introducing a duplicate public IR.
- Completion notes: completed by accepted `round-222`, merged as `5365d975`
  with title `Freeze one-backend-IR contract and guard against public lower-IR
  leaks`. The accepted result synchronized the contract across
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/backend-native-pipeline.md`, and
  `test/RepoGuardSpec.hs`, and flipped only row 1 of
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
  to `YES`.

Accepted direction lineage:

- Direction id: `direction-1a-freeze-one-backend-ir-contract`
  Status: accepted in `round-222`, merged as `5365d975`.
  Outcome: published the durable `xMLF` plus one executable backend-IR
  contract, made the future-lower-IR criteria explicit, and added the focused
  repository guard against public `MLF.Backend.*` /
  `LowerableBackend.*` exposure.
  Evidence: `orchestrator/rounds/round-222/review.md`,
  `orchestrator/rounds/round-222/review-record.json`,
  `orchestrator/rounds/round-222/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the synchronized contract surfaces named in the completion notes.

### 2. [done] Make the eager runtime lowering contract explicit

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: state clearly which runtime/codegen concerns belong in
  `MLF.Backend.IR`, which belong in LLVM lowering, and which lazy-runtime
  mechanisms are intentionally out of scope.
- Completion signal: accepted evidence makes the eager-runtime contract
  explicit across the relevant backend docs/notes/tests, with no ambiguity that
  `Backend.IR` is an eager backend boundary rather than the start of a lazy
  STG-like runtime.
- Completion notes: completed by accepted `round-223`, merged as `006eb569`
  with title `Pin eager-runtime lowering contract and keep lazy STG machinery
  out of scope`. The accepted result synchronized the eager-runtime ownership
  split and lazy-STG exclusions across
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and `test/RepoGuardSpec.hs`, flipped only row 2 of the mechanism table to
  `YES`, and left rows 3 through 7 pending.

Accepted direction lineage:

- Direction id: `direction-2a-pin-eager-runtime-contract`
  Status: accepted in `round-223`, merged as `006eb569`.
  Outcome: published the eager-runtime responsibility split across the backend
  contract surfaces, kept checked-program conversion publishing the same
  executable `MLF.Backend.IR`, limited LLVM/native lowering to downstream
  private closure ABI, layout/runtime support, and executable rendering for
  that IR, and made the shared lazy STG exclusions explicit
  (`no thunks`, `no update frames`, `no CAF update semantics`,
  `no graph reduction`, `no implicit laziness rescue`).
  Evidence: `orchestrator/rounds/round-223/review.md`,
  `orchestrator/rounds/round-223/review-record.json`,
  `orchestrator/rounds/round-223/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the synchronized contract surfaces named in the completion notes; the
  accepted review record also captures the focused row-2 repository guard, the
  focused backend IR and LLVM/native slices, and the passing
  `cabal build all && cabal test` gate.

### 3. [done] Tighten callable-shape semantics for direct calls and closure values

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: make callable shapes precise enough that direct calls, local
  first-order calls, and indirect closure calls are hard to confuse and have
  reviewable diagnostics when a backend invariant is violated.
- Completion signal: accepted evidence either refines callable representation
  inside `MLF.Backend.IR` or keeps the current representation with stronger
  validation/tests/diagnostics that make the distinction operationally clear.
- Completion notes: completed by accepted `round-224`, merged as `2c1661b3`
  with title `Clarify direct-vs-closure callable shapes at the backend IR
  boundary`. The accepted result synchronized the callable-shape contract
  across
  `docs/architecture.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendIRSpec.hs`,
  `test/BackendConvertSpec.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, flipped only row 3 of the mechanism table to
  `YES`, and left rows 4 through 7 pending.

Accepted direction lineage:

- Direction id: `direction-3a-clarify-direct-vs-closure-callable-shapes`
  Status: accepted in `round-224`, merged as `2c1661b3`.
  Outcome: published the direct-vs-closure callable-shape contract across the
  durable backend guidance surfaces, made backend validation, conversion, and
  LLVM lowering consume the same callable-head classifier, kept direct local
  first-order aliases on the `BackendApp` path, kept closure-valued aliases
  and selected closure values on the explicit `BackendClosureCall` path, and
  replaced lowerer convention with named confused-call rejection paths.
  Evidence: `orchestrator/rounds/round-224/review.md`,
  `orchestrator/rounds/round-224/review-record.json`,
  `orchestrator/rounds/round-224/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the synchronized contract surfaces named in the completion notes; the
  accepted review record also captures the focused callable-shape backend
  slices and the passing `cabal build all && cabal test` gate.

### 4. [done] Lock the ADT/case semantic boundary versus layout ownership

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: keep constructor/case semantics explicit in the IR while making it
  clear where tag values, field layout, nullary strategy, boxing, and related
  runtime representation decisions are owned.
- Completion signal: accepted evidence records or tests the semantic/layout
  split so that `Backend.IR` stays semantic and lowerer-owned layout decisions
  stop being merely implicit.
- Completion notes: completed by accepted `round-225`, merged as `5adb3702`
  with title `Freeze ADT/case semantic ownership and keep layout policy
  private`. The accepted result synchronized the semantic-versus-layout
  ownership contract across
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, flipped only row 4 of the mechanism table to
  `YES`, and left rows 5 through 7 pending.

Accepted direction lineage:

- Direction id: `direction-4a-freeze-adt-layout-ownership`
  Status: accepted in `round-225`, merged as `5adb3702`.
  Outcome: published the semantic-only ownership of `BackendData`,
  `BackendConstructor`, `BackendConstruct`, and `BackendCase` across the
  durable backend guidance surfaces, kept declaration-order tags, tag-slot-0
  layout, post-tag field slots, closure-field storage, and nullary tag-only
  representation private to LLVM/native lowering, and froze that current
  lowerer-owned policy with focused LLVM and repository-guard evidence.
  Evidence: `orchestrator/rounds/round-225/review.md`,
  `orchestrator/rounds/round-225/review-record.json`,
  `orchestrator/rounds/round-225/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the synchronized contract surfaces named in the completion notes; the
  accepted review record also captures the focused milestone-4 LLVM/layout
  slices, the repository guard, and the passing `cabal build all && cabal
  test` gate.

### 5. [done] Make primitive-operation and eager-evaluation-order assumptions explicit

- Milestone id: `milestone-5`
- Depends on: `milestone-4`
- Intent: identify which primitive operations and evaluation-order assumptions
  are part of the backend contract, and state the narrowest explicit
  representation needed before more effects, FFI, or primitive surface area is
  added.
- Completion signal: accepted evidence makes the primitive/evaluation-order
  contract explicit enough that eager LLVM lowering no longer relies on hidden
  assumptions.
- Completion notes: completed by accepted `round-226`, merged as `b4e239c5`
  with title `Freeze backend primitive surface and eager evaluation order`.
  The accepted result synchronized the primitive/eager contract across
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, flipped only row 5 of the mechanism table to
  `YES`, and left rows 6 through 7 pending.

Accepted direction lineage:

- Direction id: `direction-5a-lock-primitive-and-evaluation-order-contract`
  Status: accepted in `round-226`, merged as `b4e239c5`.
  Outcome: published the closed reserved runtime-binding set
  `__mlfp_and`, `__io_pure`, `__io_bind`, and `__io_putStrLn` as the current
  primitive surface, kept those primitives on the existing
  `BackendVar` / `BackendApp` / `BackendTyApp` path, made eager sequencing
  explicit for `BackendLet` RHS-before-body, `BackendCase`
  scrutinee-before-branch, and direct/primitive call arguments in written
  order, kept effect sequencing explicit through `__io_bind`, and preserved
  any ANF-like cleanup or normalization as a private LLVM-lowering concern
  rather than a new public executable boundary.
  Evidence: `orchestrator/rounds/round-226/review.md`,
  `orchestrator/rounds/round-226/review-record.json`,
  `orchestrator/rounds/round-226/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the synchronized contract surfaces named in the completion notes; the
  accepted review record also captures the focused milestone-5 LLVM/native
  slices, the repository guard, and the passing `cabal build all && cabal
  test` gate.

### 6. [done] Define the polymorphism-erasure and lowerability boundary

- Milestone id: `milestone-6`
- Depends on: `milestone-5`
- Intent: make it explicit which polymorphic nodes may remain in
  `MLF.Backend.IR` and what must be erased, specialized, or rejected before
  LLVM emission.
- Completion signal: accepted evidence names the lowerable subset honestly and
  tests the supported success and rejection paths for polymorphic backend
  programs.
- Completion notes: completed by accepted `round-227`, merged as `710c92eb`
  with title `Freeze backend polymorphism lowerability contract`. The accepted
  result synchronized the row-6 specialization-versus-rejection contract
  across
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`, flipped only row 6 of the mechanism table to
  `YES`, and left row 7 pending.

Accepted direction lineage:

- Direction id: `direction-6a-freeze-polymorphism-lowerability-contract`
  Status: accepted in `round-227`, merged as `710c92eb`.
  Outcome: published that checked `MLF.Backend.IR` may still carry
  `BackendTyAbs` / `BackendTyApp`, froze LLVM/native lowering as the smaller
  specialization-based lowerable subset, kept residual runtime polymorphism
  on explicit diagnostics rather than fallback lowering, and preserved the
  no-second-IR / no-public-lowering-surface boundary.
  Evidence: `orchestrator/rounds/round-227/review.md`,
  `orchestrator/rounds/round-227/review-record.json`,
  `orchestrator/rounds/round-227/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and the synchronized contract surfaces named in the completion notes; the
  accepted review record also captures the focused milestone-6 LLVM slice, the
  repository guard, and the passing `cabal build all && cabal test` gate.

### 7. [done] Synchronize validation, evidence, and guidance across the backend boundary

- Milestone id: `milestone-7`
- Depends on: `milestone-6`
- Intent: finish the family by ensuring every backend-boundary mechanism has a
  focused evidence owner, the mechanism table reflects the actual status, and
  the durable guidance surfaces are synchronized honestly.
- Completion signal: the mechanism table rows flip to `YES` only with cited
  passing evidence, the backend docs/module notes/tests agree on the contract,
  and no stale guidance mismatch remains across the family-owned surfaces.
- Completion notes: completed by accepted `round-228`, squash-merged as
  `574b7d7e` with title `Close backend-boundary guidance ledger and harden
  row-7 guard`. The accepted result synchronized the repo-facing closeout
  ledger across
  `TODO.md`,
  `implementation_notes.md`,
  `CHANGELOG.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and `test/RepoGuardSpec.hs`, kept the preserved boundary explicit as
  `one executable eager backend IR`,
  `no public \`LowerableBackend.IR\``, and
  `no lazy STG machinery`, flipped row 7 of the mechanism table to `YES`, and
  left no unfinished milestones in `rev-001`.

Candidate directions:

- Direction id: `direction-7a-close-the-mechanism-table-and-guidance-ledger`
  Status: accepted in `round-228`, squash-merged as `574b7d7e`.
  Summary: refreshed the mechanism table, repo-facing closeout ledger, and
  the dedicated row-7 repository guard so the repository states the backend
  boundary exactly as the accepted evidence earned it.
  Outcome: closed the family without reopening rows 1 through 6, preserved
  the `one executable eager backend IR` /
  `no public \`LowerableBackend.IR\`` /
  `no lazy STG machinery` boundary, and made explicit that no unfinished
  milestones remain in `rev-001`.
  Evidence: `orchestrator/rounds/round-228/review.md`,
  `orchestrator/rounds/round-228/review-record.json`,
  `orchestrator/rounds/round-228/merge.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `TODO.md`,
  `implementation_notes.md`,
  `CHANGELOG.md`, and
  `test/RepoGuardSpec.hs`; the accepted review record also captures the
  focused row-7 repository guard and the passing `cabal build all && cabal
  test` gate.
