# Defensible Exactness Design

**Date:** 2026-02-21
**Goal:** Move from "tested" to "defensible exactness" — for every thesis rule the implementation claims to satisfy, produce a traceable chain: thesis clause → claim → obligation/code path → executable test → enforced gate.

**Audience:** Thesis defense/review. An examiner can probe any chapter (9–15). Evidence must be executable and continuously enforced, not prose-only.

## Approach: Hybrid (Claim Registry + Enriched Ledger)

Layer a thesis-claim registry on top of the existing 99-obligation ledger. Cross-link them. Add pipeline-level property tests. Gate everything.

## Deliverables

### 1. Thesis Claims Registry (`docs/thesis-claims.yaml`)

~20 entries covering theorems, definitions, and invariants across Ch. 9–15.

```yaml
version: 1
scope:
  chapters: [9, 10, 11, 12, 14, 15]
  claim_count: <N>

claims:
- id: CLM-PRESERVATION
  type: theorem
  chapter: 14
  section: "14.3"
  thesis_ref: "Theorem 14.3.1 (Subject Reduction)"
  thesis_line: <line>
  statement: "If Γ ⊢ a : τ and a → a', then Γ ⊢ a' : τ"
  evidence:
    obligations: [O14-RED-BETA, O14-RED-BETALET, O14-RED-CONTEXT, ...]
    property_tests:
      - matcher: "Phase 7 theorem obligations"
        file: test/TypeSoundnessSpec.hs
        kind: quickcheck
    code_paths:
      - src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv
      - src/MLF/Elab/Reduce.hs#step
  deviations: []
  status: defended
  notes: "Property proxy only; not mechanized."
```

### 2. Deviation Register (`docs/thesis-deviations.yaml`)

~5–8 entries absorbing known gaps from `docs/paper-map.md` "Known Deviations" plus any discovered during claim enumeration.

```yaml
version: 1
deviations:
- id: DEV-STEPINTRO-NOT-OMEGA
  chapter: 15
  section: "15.3"
  thesis_ref: "Definition 15.3.4 (Φ translation)"
  description: >
    Quantifier introduction (O) is not part of Ω in the thesis.
    The repo records these as StepIntro entries in ewSteps and
    translates them interleaved with Ω segments.
  impact: semantic-neutral
  rationale: >
    Interleaving O with Ω in ewSteps simplifies the single-pass
    Φ translation without changing the resulting instantiation.
  code_paths:
    - src/MLF/Constraint/Types/Witness.hs
    - src/MLF/Elab/Phi/Translate.hs
  test_evidence:
    - matcher: "R-"
      file: test/Presolution/WitnessSpec.hs
  status: accepted
```

Impact levels: `semantic-neutral` (no behavioral difference), `semantic-minor` (scoped behavioral difference with rationale), `semantic-gap` (known unimplemented).

Gate checks:
- No `status: open` deviations
- Every deviation ID referenced by a claim exists
- No orphan deviations (each referenced by ≥1 claim)

### 3. Enriched Obligations Ledger

Minimal change to existing `docs/thesis-obligations.yaml` — add one field per obligation:

```yaml
- id: O14-RED-BETA
  # ... existing fields unchanged ...
  supports_claims: [CLM-PRESERVATION, CLM-REDUCTION-RULES]
```

Checker gains: every `supports_claims` entry must exist in `thesis-claims.yaml`.

### 4. New Property Tests

**4a. `test/TranslatablePresolutionSpec.hs`** (CLM-TRANSLATABLE-PRESOLUTION)
- Generator: full pipeline (parse → constraint → normalize → presolution) on eMLF terms
- Property: presolution satisfies Def. 15.2.10 conditions (no inert-locked, rigid scheme roots, rigid arrows, rigid non-interior nodes)

**4b. `test/PhiSoundnessSpec.hs`** (CLM-PHI-CORRECTNESS)
- Generator: for each instantiation edge in a pipeline-generated presolution, compute Φ(e)
- Property: `applyInstantiation sourceType Φ(e) == targetType`
- Coverage labels for edge shapes: identity, instantiate, forall-intro, compose

**4c. `test/ExpansionMinimalitySpec.hs`** (CLM-EXPANSION-MINIMALITY)
- Generator: pipeline-generated presolutions
- Property: each expansion is minimal (identity when possible, instantiate only when forall structure demands it)

### 5. Gate Script Upgrades

`scripts/thesis-conformance-gate.sh` gains three new stages:

**5a. Claim-level roll-up** (`scripts/check-thesis-claims.sh`)
- Every `status: defended` claim has all referenced obligations in `anchored` status
- Every claim's `property_tests` matchers run with ≥1 passing example
- No `status: undefended` claim without an explicit deviation or notes
- Claim count matches `scope.claim_count`

**5b. Deviation register check** (integrated into claim checker)
- No `status: open` deviations
- Every deviation ID referenced by a claim exists in `thesis-deviations.yaml`
- No orphan deviations

**5c. New property test anchors**
```bash
run_anchor "Translatable presolution invariant" "Translatable presolution" 5
run_anchor "Phi soundness property" "Phi soundness" 5
run_anchor "Expansion minimality property" "Expansion minimality" 3
```

### 6. Spec/Document Drift Closure

**6a. Open spec tasks** — for each open `.kiro` spec task:
- `thesis-exact-gen-fallback-removal/tasks.md` (4 open) → complete or register as deviation
- `let-scope-alternative-typing/tasks.md` (2 open: #4, #5) → complete or register
- `thesis-alignment-backlog/tasks.md` (2 open: #4, #5) → close or register

**6b. Document migration**
- `docs/paper-map.md` "Known Deviations" → migrated into `thesis-deviations.yaml`
- `docs/paper-map.md` "Audit Checklist" → migrated into claim evidence chains
- `implementation_notes.md` historical entries stay as-is (log, not claims)

## Dependency Order

1. `docs/thesis-claims.yaml` (enumerate claims)
2. `docs/thesis-deviations.yaml` (absorb known deviations)
3. `docs/thesis-obligations.yaml` enrichment (`supports_claims` back-links)
4. `scripts/check-thesis-claims.sh` (claim-level checker)
5. `test/TranslatablePresolutionSpec.hs`
6. `test/PhiSoundnessSpec.hs`
7. `test/ExpansionMinimalitySpec.hs`
8. `scripts/thesis-conformance-gate.sh` upgrade (claim roll-up + deviation check + new anchors)
9. Spec/doc drift closure
10. `docs/paper-map.md` migration
