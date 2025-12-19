# Requirements Document

## Introduction
`papers/xmlf.txt` defines the interior of a node `r`, written `I(r)`, as **all nodes transitively bound to `r` in the binding tree** (§3.2). Our presolution currently records `EdgeTrace.etInterior` as a **superset** (`Binding.interiorOf r ∪ approxInterior`) to keep some instantiation artifacts (binder metas / args) available for downstream logic.

This spec removes that approximation and makes `EdgeTrace.etInterior` the **exact** paper interior `I(r)` in binding-edge mode, computed in a way that is consistent with presolution’s union-find (quotient graph). The result is that:
- `I(r)` is a real, paper-faithful set (not “best effort”),
- presolution records `OpRaise` only for nodes truly inside `I(r)`,
- elaboration’s ≺ computations can be safely restricted to `I(r)` without relying on extra nodes being injected.

Out of scope:
- Retiring the legacy scope model (`GNode`/`tnVarLevel`) or removing the legacy fallback path when `cBindParents` is empty.
- Optimizing all pipeline phases around the new interior (this spec only changes the definition + call sites that currently depend on the approximate set).

## Requirements

### Requirement 1
**User Story:** As a maintainer, I want `EdgeTrace.etInterior` to equal the paper interior `I(r)` exactly, so that scope-sensitive steps (Ω normalization + Φ translation) do not depend on approximations.

#### Acceptance Criteria
1. WHEN `cBindParents` is non-empty AND presolution records an `EdgeTrace` for an instantiation edge THEN THE SYSTEM SHALL set `etInterior` to exactly `I(r)` where `r = etRoot` (nodes transitively bound to `r`), with no union/supplementary nodes.
2. WHEN `cBindParents` is empty (legacy mode) THEN THE SYSTEM SHALL keep the current behavior (an approximate interior derived from the expansion trace) so existing non-binding-edge workflows continue to run.
3. WHEN `etInterior` is computed in binding-edge mode THEN THE SYSTEM SHALL NOT include instantiation argument nodes unless they are actually transitively bound to `r` in the binding tree.

### Requirement 2
**User Story:** As a solver developer, I want edge-local unification and Ω witness recording to use the same exact `I(r)` as `EdgeTrace`, so that `OpRaise` filtering matches the paper’s “operations inside `I(r)` only”.

#### Acceptance Criteria
1. WHEN presolution initializes edge-local unification state for an expansion root `r` in binding-edge mode THEN THE SYSTEM SHALL compute and use the exact `I(r)` (not an approximation) for all “interior membership” bookkeeping (`eusInteriorRoots`, `eusInteriorByRoot`).
2. WHEN binding-parent harmonization raises a node that is not in `I(r)` THEN THE SYSTEM SHALL NOT record `OpRaise` for that node in the edge witness Ω.

### Requirement 3
**User Story:** As an elaboration implementer, I want ≺ ordering for an edge to be restricted to the exact interior `I(r)` (when available), so that Figure 10’s `min≺{…}` selection is computed over the correct domain.

#### Acceptance Criteria
1. WHEN elaboration is given an `EdgeTrace` with non-empty `etInterior` THEN THE SYSTEM SHALL compute order keys using exactly that set as the restriction domain (no dependence on injected “approx interior” nodes).
2. WHEN running the existing “non-spine Raise” regression scenario THEN THE SYSTEM SHALL still satisfy `applyInstantiation source Φ == target` up to α-equivalence after the interior change.

### Requirement 4
**User Story:** As a maintainer, I want automated tests that lock in the “exact interior” guarantee, so regressions cannot silently reintroduce approximations.

#### Acceptance Criteria
1. WHEN running the test suite THEN THE SYSTEM SHALL include property tests that validate the new “quotient-aware interior” implementation against a reference characterization (“a node is in `I(r)` iff `r` appears on its binding-parent chain”) on generated small graphs.
2. WHEN running the test suite THEN THE SYSTEM SHALL include at least one regression test that builds a presolution with an instantiation edge and asserts `etInterior == Binding.interiorOf(r)` on the final canonical constraint (binding-edge mode).

