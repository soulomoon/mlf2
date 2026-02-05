# Thesis-Exact Φ Translatability: Resolved State (2026-02-05)

## Purpose
This note supersedes the earlier branch-vs-branch comparison and records the
final behavior now implemented in this branch for thesis-exact Φ translation.

Source of truth:
- Primary: `/Volumes/src/papers/these-finale-english.txt`
- Secondary only when thesis is silent: `/Volumes/src/papers/xmlf.txt`

## Locked Decisions (Implemented)
1. Strict fail-fast semantics for non-translatable witnesses.
2. Literal rigid rule for Raise/Merge/RaiseMerge: rigid identity is keyed on
   the operated node `n` (`χ(n) = (=)`), not on broad endpoint heuristics.
3. Production elaboration requires `EdgeTrace`; no-trace APIs are retained only
   for tests/debug.
4. Root/interior handling is unified across trace construction, trace refresh,
   and witness normalization.
5. Context search follows the thesis shape; non-thesis fallback traversal is
   removed.

## Final Thesis-Alignment Outcomes

### 1) Presolution preconditions are hard invariants
Implemented in:
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`
- `src/MLF/Constraint/Presolution/WitnessValidation.hs`
- `src/MLF/Constraint/Presolution/Base.hs`

Result:
- Missing `<P` order keys fail normalization/validation (`MissingOrderKey`).
- Invalid rigid operand orientation is rejected (`RigidOperandMismatch`).
- Non-transitive-flex precondition failures for non-rigid Merge are rejected
  (`NotTransitivelyFlexBound`).
- Validation happens before restoring original node space, using exact rewritten
  interior alignment.
- No synthetic fallback order-key generation remains.

### 2) Root/interior computation is unified
Implemented in:
- `src/MLF/Constraint/Presolution/Base.hs`
- `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`
- `src/MLF/Constraint/Presolution/Driver.hs`
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`

Result:
- `traceInteriorRootRef` is the shared root-selection helper.
- `edgeInteriorExact` and trace refresh both use the same root reference logic.
- `EdgeTrace` is emitted for all instantiation edges (including non-`TyExp` /
  identity edges), keeping production Φ trace-requirements satisfiable.
- `etRoot`/`etInterior` semantics are now coherent with normalization.

### 3) Φ operation semantics are thesis-literal and strict
Implemented in:
- `src/MLF/Elab/Phi/Omega.hs`

Result:
- `OpRaise`: rigid check is performed before interior rejection.
- `OpMerge` and `OpRaiseMerge`: rigid identity is accepted when operated node
  `n` is rigid; if rigidity appears only on non-operated endpoint `m`, Φ fails.
- Non-binder and missing-context paths remain hard failures for production Φ.
- `reorderBindersByPrec` is fail-fast: missing binder identities / missing order
  keys now raise `PhiReorder:*` errors instead of silently returning `InstId`.

### 4) Computation-context search is literalized
Implemented in:
- `src/MLF/Elab/Phi/Context.hs`

Result:
- Removed fallback descent through `TyForall` body that was not part of thesis
  context grammar.
- Context search now either finds thesis-legal context or fails explicitly.

### 5) No-trace APIs are quarantined; production path is strict
Implemented in:
- `src/MLF/Elab/Phi/Translate.hs`
- `src/MLF/Elab/Phi.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src/MLF/Elab/Elaborate.hs`

Result:
- Production elaboration path uses `phiFromEdgeWitnessWithTrace`.
- `phiFromEdgeWitnessNoTrace` remains available for tests/debug only.
- Legacy `phiFromEdgeWitness` alias remains but is deprecated with explicit
  warning to use trace-required API in production.

## Clarification: “Merge/RaiseMerge with one rigid endpoint”
Question addressed during audit:
- Is “one rigid endpoint” unreachable by construction?

Resolved implementation stance:
- We do **not** assume it is unreachable.
- We encode the thesis-literal rule directly:
  - If operated node `n` is rigid, translation uses identity (`ε`).
  - If only non-operated endpoint `m` is rigid, translation fails as
    non-translatable.

This keeps behavior literal to Fig. 15.3.4’s `χ(n)` condition and avoids
silently accepting ambiguous endpoint cases.

## Regression Coverage Added/Updated
Primary files:
- `test/ElaborationSpec.hs`
- `test/Presolution/WitnessSpec.hs`

Covered scenarios include:
1. Missing `<P` key now fails Σ(g) reordering.
2. `OpRaise` rigid-outside-interior yields identity behavior.
3. `OpMerge`/`OpRaiseMerge` with rigid operated node `n` yield identity.
4. `OpMerge`/`OpRaiseMerge` rigid only on non-operated endpoint fail.
5. Missing trace entry fails elaboration.
6. Context fallback-dependent cases fail explicitly.
7. Presolution witness validation rejects missing order keys / rigid orientation
   violations / non-transitive-flex Merge cases.

## Validation Gate
Executed:
- `cabal build all && cabal test --test-show-details=direct`

Result:
- PASS (`458 examples, 0 failures, 1 pending`)
- Pending item is the existing out-of-scope rank-2 annotation case.

## Remaining Explicit Deviation Notes
None newly introduced by this refactor series. Existing broader project-level
gaps (formal proofs and full phase-7 proof obligations) remain tracked in
`implementation_notes.md` and `.kiro/specs/paper-faithfulness-remaining-deltas/`.
