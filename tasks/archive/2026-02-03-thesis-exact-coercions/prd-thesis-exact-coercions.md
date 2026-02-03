# PRD: Thesis‑Exact Coercions (Rigid Domain / Flex Codomain)

## Introduction/Overview
Align eMLF term annotations with the thesis’ coercion semantics: `(e : κ)` is encoded as a coercion `cκ e` whose type has a **rigidly bound domain** and a **flexibly bound codomain**, sharing existential nodes across both copies. This makes annotation behavior thesis‑exact and improves auditability against `papers/these-finale-english.txt` (§12.3.2.2, §15.3.8).

## Goals
- Implement coercion types with rigid domain / flexible codomain and shared existentials.
- Make annotation results return the **codomain** (thesis exact).
- Preserve binding‑tree invariants (no locked descendants caused by coercions).
- Add regression tests that pin rigidity, codomain return, and existential sharing.
- Ensure `cabal test --test-show-details=direct` passes.

## User Stories

### US-001: Build thesis‑exact coercion graph
**Description:** As a type‑system developer, I want coercion types to have a rigid domain, flexible codomain, and shared existentials so annotations match the thesis semantics.

**Acceptance Criteria:**
- [ ] Domain copy uses rigid binding edges for coercion‑local nodes.
- [ ] Codomain copy uses flexible binding edges.
- [ ] Existential/free vars are shared between domain and codomain copies.
- [ ] No coercion‑local node becomes **locked** due to rigid ancestors (restricted only).
- [ ] Binding tree validates successfully for constraints involving coercions.

### US-002: Annotation returns codomain
**Description:** As a user, I want `(e : κ)` to type as the codomain of `cκ` so annotation results are thesis‑exact.

**Acceptance Criteria:**
- [ ] `buildCoerce` returns the codomain node of the coercion copy.
- [ ] Instantiation edge remains `edgeLeft ≤ domainNode`.
- [ ] `AAnn` records the codomain as the annotation result.
- [ ] Existing annotation tests still pass or are updated to assert codomain behavior.

### US-003: Presolution compatibility with rigid domains
**Description:** As a system maintainer, I need presolution/elaboration to remain valid after adding rigid domain coercions.

**Acceptance Criteria:**
- [ ] No `OperationOnLockedNode` errors during presolution for annotation tests.
- [ ] No `GenSchemeFreeVars` / scheme‑closure errors after switching to codomain.
- [ ] Any required presolution adjustments are minimal and localized.

### US-004: Add targeted regression tests
**Description:** As a contributor, I need tests that lock in thesis‑exact coercion behavior for future audits.

**Acceptance Criteria:**
- [ ] New tests verify the domain is **restricted** (non‑instantiable) but not locked.
- [ ] New tests verify `EAnn` returns the codomain copy.
- [ ] New tests verify existential sharing between domain/codomain copies.
- [ ] `cabal test --test-show-details=direct` passes.

## Functional Requirements
- FR-1: Coercion internalization must build **two copies** of the annotated type with shared existentials.
- FR-2: The **domain** copy must be bound with rigid edges (restricted nodes) and have **no rigid ancestors**.
- FR-3: The **codomain** copy must be bound with flexible edges.
- FR-4: Annotation typing must return the **codomain** copy.
- FR-5: Presolution must remain valid (no locked‑node or scheme‑closure failures).
- FR-6: Add tests for rigidity, codomain return, and existential sharing.

## Non‑Goals (Out of Scope)
- Changing surface grammar beyond existing `EAnn`/`ELamAnn` syntax.
- Introducing a new coercion edge type or alternate instance relation.
- Broader refactors of presolution or elaboration unrelated to coercions.

## Design Considerations
- Use the thesis’ binding‑edge encoding (rigid vs flexible edges) rather than a new edge flag.
- Ensure coercion‑local nodes are **restricted** (rigid edge on their own binding), not **locked** (no rigid ancestors).
- Keep shared existentials bound flexibly under the coercion gen node.

## Technical Considerations
- Structural nodes auto‑bind children via `allocArrow` and `setVarBound`; when building rigid domains, children must be rebound under the coercion gen to avoid rigid ancestors.
- Switching to codomain may surface scheme‑closure issues; ensure all codomain‑reachable nodes have correct bind parents.
- Tests should probe binding‑tree predicates (`isUnderRigidBinder`, `nodeKind`) via existing helpers where possible.

## Success Metrics
- All existing tests pass; no new presolution errors on annotation cases.
- New tests explicitly assert rigidity, codomain return, and shared existentials.
- Code path is visibly aligned with thesis sections (§12.3.2.2, §15.3.8).

## Open Questions
- None (scope and goal are fixed by the thesis‑exact requirement).
