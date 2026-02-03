# Design Document

## Overview
This audit compares the eMLF surface language in `MLF.Frontend.Syntax` to the thesis
grammar (Sec 1.6) and the annotation desugaring described in Sec 12.3.2. The goal
is to confirm where the surface syntax matches exactly, and to surface deviations
that should be documented or desugared.

## Architecture
High-level flow for surface terms:

Parse/construct Expr
  -> desugarCoercions (optional)
  -> constraint generation (MLF.Frontend.ConstraintGen)

The thesis models annotated terms via coercion functions c_tau. The code keeps
annotations explicit in the AST and interprets them directly during constraint
generation.

## Components and Interfaces
- `MLF.Frontend.Syntax`
  - `Expr`: core surface terms, including `ELetAnn` and `ELit`.
  - `SrcType`: annotation types with bounded forall and bottom.
  - `SrcScheme`: multi-binder form for let annotations.
- `MLF.Frontend.Desugar`
  - `desugarCoercions`: records thesis sugar (kappa_sigma) but currently leaves
    `EAnn` and `ELamAnn` explicit.
- `MLF.Frontend.ConstraintGen`
  - Authoritative interpretation of annotations into constraints.

## Data Models
- Paper annotated grammar: `x | lambda x | lambda (x : sigma) | app | let | (a : sigma)`.
- Code `Expr` includes the paper forms plus:
  - `ELetAnn` (let with scheme annotation)
  - `ELit` (literal constants)
  - `EVarRaw` (internal only)
- Annotation syntax:
  - Paper uses a kappa pseudo-type that includes existential and universal parts.
  - Code uses `SrcType` with bounded forall and allows free type variables (tests
    cover free vars).

## Error Handling
Surface-level annotation errors are reported during constraint generation (e.g.,
mismatched annotation structure). The audit should track whether these errors
align with the paper's coercion semantics.

## Testing Strategy
- Use `test/ConstraintGenSpec.hs` for annotation behavior:
  - Free type variables in annotations
  - Explicit forall and bottom annotations
  - Let annotations and term annotations
- Add or adjust tests only if we decide to enforce desugaring to coercion functions.
