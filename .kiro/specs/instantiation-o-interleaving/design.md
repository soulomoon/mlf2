# Design Document

## Overview
Introduce an explicit witness step for quantifier introduction (O / InstIntro)
so that presolution can interleave these steps with Omega operations. The
witness sequence becomes a list of steps, where Omega operations keep the
existing normalization/validation pipeline and O steps are preserved in order.
Phi translation consumes the step list sequentially.

## Architecture
- Presolution emits a step sequence that includes both O steps and Omega ops.
- Omega normalization is applied only to the Omega subsequences (conditions
  (1)–(5) only, no elimination pruning), with O steps acting as barriers that
  preserve ordering.
- Phi translation consumes the step list and maps O to InstIntro at the
  appropriate position.

## Components and Interfaces
- `MLF.Constraint.Types`
  - Add a new witness-step representation (e.g., `InstanceStep`) with variants:
    - `StepOmega InstanceOp`
    - `StepIntro` (represents O)
  - Update `EdgeWitness` to store a list of `InstanceStep` instead of
    `ewForallIntros` + `InstanceWitness`.
- `MLF.Constraint.Presolution.Witness`
  - Update `witnessFromExpansion` to emit `StepIntro` for each binder in
    `ForallSpec` (use `fsBinderCount`).
  - Provide a normalization function that:
    - normalizes only the Omega ops inside each contiguous Omega segment
    - preserves StepIntro ordering and boundaries
- `MLF.Constraint.Presolution.Driver`
  - Update `buildEdgeWitness` to assemble the step list, normalize Omega
    segments, and drop `ewForallIntros`.
- `MLF.Elab.Phi`
  - Translate `StepIntro` to `InstIntro` inline in the instantiation sequence.
  - Translate `StepOmega op` using the existing Phi logic for Omega ops.

## Data Models
- New type (example):
  - `data InstanceStep = StepOmega InstanceOp | StepIntro`
- `EdgeWitness` changes:
  - Replace `ewForallIntros :: Int` with `ewSteps :: [InstanceStep]`.
  - Keep `InstanceWitness` for Omega-only use or replace it with steps.

## Error Handling
- Normalization errors (e.g., OpOutsideInterior, MergeDirectionInvalid) should
  continue to refer to Omega ops only.
- StepIntro should not trigger Omega validation failures.

## Testing Strategy
- Unit tests:
  - Presolution witness from ExpCompose interleaves StepIntro in the correct
    relative order.
  - Normalization preserves StepIntro ordering and only reorders Omega ops.
- Integration tests:
  - Phi translation produces InstIntro in the correct position for a witness
    that interleaves StepIntro with Omega ops.
- Property tests:
  - For any witness sequence, removing StepIntro and normalizing Omega segments
    yields the same Omega order as the current normalizeInstanceOpsFull.
