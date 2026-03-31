# Round 162 — Implementation Notes

## Item 3: Expand property-based testing (QuickCheck)

### Summary

Added 14 QuickCheck property tests in a new `test/Property/QuickCheckPropertySpec.hs`
module, covering four orthogonal domains of the constraint pipeline.

### Deliverables

**D1 — Reification round-trip well-formedness (4 properties)**

1. `substTypeCapture preserves free-variable well-formedness` — after substitution,
   every free variable in the result was either free in the original type or is the
   substituted variable.
2. `alphaEqType is reflexive` — every generated type is alpha-equal to itself.
3. `alphaEqType is symmetric` — alpha-equality is symmetric across all generated pairs.
4. `substituting a variable not free in a type is identity` — substituting a fresh
   variable leaves the type unchanged.

**D2 — Canonicalization idempotency (3 properties, 200 tests each)**

5. `canonicalization is idempotent` — `canonicalizeNode` applied twice equals once.
6. `canonicalization is stable` — triple-application equals double-application.
7. `canonicalization is deterministic` — two independently constructed canonicalizers
   from the same maps produce identical results.

**D3 — Binding tree invariant preservation (4 properties)**

8. `genValidBindingConstraint always produces valid binding trees` — the generator
   itself is validated by `checkBindingTree`.
9. `every child in binding tree has a valid parent node` — parent NodeIds reference
   nodes that exist in the constraint's node map.
10. `binding tree has no cycles` — the parent-of relation is acyclic (finite chain
    to a root).
11. `rootedConstraint preserves binding tree validity` — wrapping a constraint with
    `rootedConstraint` keeps the binding tree valid.

**D4 — Unification symmetry (3 properties)**

12. `decomposeUnifyChildren is symmetric` — swapping the two TyNode arguments does
    not change the success/failure outcome.
13. `decomposeUnifyChildren succeeds for same-head nodes` — same-constructor pairs
    always decompose successfully.
14. `decomposeUnifyChildren fails for different-head nodes` — different-constructor
    pairs always fail with `DecomposeMismatch`.

### Generators

- `genElabType` / `genBoundType` / `shrinkElabType` — sized, well-formed Ty GADT generators.
- `genUnionFindMap` / `genRedirectMap` / `nodeDomain` — canonicalizer input generators
  (adapted from CanonicalizerSpec patterns).
- `genValidBindingConstraint` — generates constraints with valid binding trees.
- `genTyNodePair` / `genSameHeadTyNodePair` / `genDifferentHeadTyNodePair` — TyNode pair
  generators for decomposition symmetry testing.

### Infrastructure changes

- **Exposed module**: `MLF.Constraint.Unify.Decompose` moved from `other-modules` to
  `exposed-modules` in the `mlf2-internal` library stanza (tests can only import exposed
  modules per AGENTS.md).
- **Cabal wiring**: `Property.QuickCheckPropertySpec` added to `test-suite mlf2-test`
  `other-modules`.
- **test/Main.hs**: Added import and `spec` call for the new module.
- **RepoGuardSpec**: Added `"Property"` to the allowed subdirectory list in
  `isMainEntryPath` so the guardrail test recognizes the new module.

### Verification gates

| Gate | Result |
|------|--------|
| `cabal build all` | ✅ 0 warnings |
| `cabal test` | ✅ 1288 examples, 0 failures |
| `./scripts/thesis-conformance-gate.sh` | ✅ PASS |

### Commit

`742e33c` on branch `orchestrator/round-162-quickcheck-properties`
