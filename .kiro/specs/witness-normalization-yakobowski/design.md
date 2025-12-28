# Design Document

## Overview
Implement a paper-faithful normalization for instantiation witnesses (Ω) per
`papers/xmlf.txt` §3.4 and Yakobowski'08. The normalized witness must satisfy
conditions (1)–(5) used by Φ translation. The design introduces an explicit
normalization environment and validator, then replaces the existing lightweight
normalization with the full algorithm while preserving call-site compatibility.

## Architecture
Normalization is split into three layers:
1) **Context builder**: assemble I(r), ≺ keys, binding tree info, and
   canonicalization function from presolution state / `EdgeTrace`.
2) **Validator**: `validateNormalizedWitness` checks conditions (1)–(5).
3) **Normalizer**: `normalizeInstanceOpsFull` transforms Ω until it validates.

Suggested module layout:
- `MLF.Constraint.Presolution.Witness` remains the public entry point.
- Add a submodule or section for normalization (or a new
  `MLF.Constraint.Presolution.Normalize` if the file grows too large).

## Components and Interfaces

### Normalize environment
```hs
-- Context required to verify paper conditions.
data OmegaNormalizeEnv = OmegaNormalizeEnv
  { oneRoot       :: NodeId         -- r, expansion root
  , interior      :: IntSet         -- I(r)
  , orderKeys     :: IntMap OrderKey
  , canonical     :: NodeId -> NodeId
  , constraint    :: Constraint
  }
```

### Errors
```hs
-- Structured normalization failures.
data OmegaNormalizeError
  = OpOutsideInterior InstanceOp
  | MergeDirectionInvalid NodeId NodeId
  | RaiseNotUnderRoot NodeId NodeId
  | RaiseMergeInsideInterior NodeId NodeId
  | OpUnderRigid NodeId
  | MissingOrderKey NodeId
  | MalformedRaiseMerge [InstanceOp]
  deriving (Eq, Show)
```

### Validator
```hs
validateNormalizedWitness
  :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError ()
```
Checks conditions (1)–(5) directly using `interior`, `orderKeys`, and
`Binding.Tree` predicates.

### Normalizer
```hs
normalizeInstanceOpsFull
  :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
```
Pipeline steps (pure, deterministic):
1. **Canonicalize** NodeIds via `canonical` and drop ops on rigid nodes
   (or return `OpUnderRigid`).
2. **Coalesce RaiseMerge**: collapse `Raise(n)^k; Merge(n, m)` when
   `m ∉ I(r)` into `OpRaiseMerge n m`. If a merge leaves I(r) without the
   required raises, return `MalformedRaiseMerge`.
3. **Merge direction check**: ensure `m ≺ n` for Merge/RaiseMerge; if not,
   return `MergeDirectionInvalid` (direction is fixed by presolution).
4. **Reorder Weaken**: move each `Weaken(n)` after all ops on nodes below n.
5. **Finalize**: return the normalized list and require validator success.

### Call-site integration
- `buildEdgeWitness` in `MLF.Constraint.Presolution.Driver` should assemble the
  `OmegaNormalizeEnv` using:
  - `EdgeTrace.etInterior` for I(r)
  - `Order.orderKeysFromRoot` for ≺ keys
  - `srUnionFind` (or presolution UF) for canonicalization
  - current constraint for binding-tree queries
- If `EdgeTrace` is built after the witness today, reorder creation so the
  trace is available for normalization, or compute `interior` locally in
  `buildEdgeWitness` using the same logic as `buildEdgeTrace`.

## Data Models
- New `OmegaNormalizeEnv` and `OmegaNormalizeError` types.
- No changes to `InstanceOp` in this spec (interleaving `O` is handled by a
  separate mismatch spec).

## Error Handling
- Normalization returns `Left OmegaNormalizeError` on malformed or
  non-normalizable sequences.
- Presolution should surface this as a structured error (e.g. `InternalError`)
  rather than silently accepting invalid Ω.

## Testing Strategy
- Unit tests for each condition (1)–(5) using small hand-written witnesses.
- Property test: for randomly generated Ω sequences plus environment,
  `normalizeInstanceOpsFull` returns Ω' that `validateNormalizedWitness` accepts.
- Regression test: sequences containing RaiseMerge patterns are collapsed and
  preserve the intended ordering relative to Weaken and Merge.
