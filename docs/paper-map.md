# Paper-to-Code Map

This document maps sections of `papers/these-finale-english.txt` (thesis) to the corresponding implementation modules and functions. Use this as an index for paper-faithfulness auditing.

## Primary Paper Reference

- **Thesis**: `papers/these-finale-english.txt` — primary source for xMLF and elaboration
- **Supplement**: `papers/xmlf.txt` — figure numbering and additional xMLF details

## Canonical Obligations Ledger (Ch. 14/15)

- Canonical rule inventory (source of truth):
  - `/Volumes/src/mlf4/docs/thesis-obligations-ch14-15.yaml`
- Generated human-readable view:
  - `/Volumes/src/mlf4/docs/thesis-obligations-ch14-15.md`
- Enforced checker:
  - `/Volumes/src/mlf4/scripts/check-thesis-obligations-ledger.sh`
- Renderer:
  - `/Volumes/src/mlf4/scripts/render-thesis-obligations-ledger.rb`

This ledger is the canonical rule-to-code-to-test index for thesis Chapter 14 (`14.2`-`14.3`) and Chapter 15 (`15.2`-`15.3`) operational obligations. The thesis conformance gate requires all 61 obligations to be present, uniquely mapped, `status=anchored`, and backed by at least one passing executable anchor matcher.

## Notation Mapping

| Paper | Meaning | Repo Types/Modules |
|------:|---------|-------------------|
| `b` | eMLF surface term | `MLF.Frontend.Syntax.Expr`, `SrcType` |
| `χ` | constraint graph | `MLF.Constraint.Types.Constraint` |
| `n` | type node in the graph | `MLF.Constraint.Types.Graph.NodeId`, `TyNode` in `cNodes` |
| `g` | binding-tree node (generalization site) | `GenNodeId`, `GenNode` in `cGenNodes` |
| `≤` edge | instantiation constraint | `InstEdge` in `cInstEdges` |
| `=` edge | unification constraint | `UnifyEdge` in `cUnifyEdges` |
| `s·τ` | expansion node / variable | `TyExp` with `ExpVarId`, `Expansion` recipes |
| `χp` | (principal) presolution | `MLF.Constraint.Presolution.PresolutionResult` |
| `τ` | xMLF type | `MLF.Elab.Types.ElabType` |
| `φ` | xMLF instantiation witness | `MLF.Elab.Types.Instantiation` |
| `a` | xMLF term | `MLF.Elab.Types.ElabTerm` |

## Phase Mapping

| Phase | Paper Role | Entry Point Module |
|------:|------------|-------------------|
| 1 | Constraint generation | `MLF.Frontend.ConstraintGen` |
| 2 | Local simplification (grafting/merging) | `MLF.Constraint.Normalize` |
| 3 | Acyclicity / dependency ordering | `MLF.Constraint.Acyclicity` |
| 4 | Presolution (minimal expansions) | `MLF.Constraint.Presolution` |
| 5 | Global unification | `MLF.Constraint.Solve` |
| 6 | Elaborate to xMLF | `MLF.Elab.Pipeline` |

## Section-to-Module Mapping

### Figures 1–4: xMLF Grammar and Instantiation

**Paper**: `papers/these-finale-english.txt` Figures 1–4
**Code**: `MLF.Elab.Types`

- xMLF types: `ElabType` (ETyVar, ETyArrow, ETyForall, ETyBot)
- xMLF terms: `ElabTerm` (ELam, EApp, ETyLam, ETyApp, ELet, EVar, EAnn)
- Instantiations: `Instantiation` (InstRefl, InstTrans, InstUnder, InstApp, InstBot, InstIntro)

### Figure 10: Instance Operations (Ω) and Φ Translation

**Paper**: `papers/these-finale-english.txt` Figure 10; `papers/xmlf.txt` §3.4
**Code**:

| Paper Construct | Repo Location |
|-----------------|---------------|
| `OpGraft` | `MLF.Witness.OmegaExec.applyGraft` |
| `OpWeaken` | `MLF.Witness.OmegaExec.applyWeaken` |
| `OpRaise` | `MLF.Binding.Adjustment.applyRaiseStep` |
| `OpMerge` | `MLF.Witness.OmegaExec` (merge logic in presolution) |
| `Φ(e)` (witness → instantiation) | `MLF.Elab.Pipeline.phiFromEdgeWitness` |
| `Σ(g)` (reordering) | `MLF.Elab.Pipeline.sigmaReorder` |

### §3.1–3.5: Elaboration from eMLF to xMLF

**Paper**: `papers/these-finale-english.txt` §3
**Code**:

| Paper | Repo |
|-------|------|
| `/)(g) = Λ(Q(g))` | `MLF.Elab.Generalize.generalizeAt` |
| `S/Q/T` (node → type mapping) | `MLF.Elab.Elaborate.reifyTypeWithNamesNoFallback` |
| Binding tree operations | `MLF.Binding.Tree`, `MLF.Binding.Queries` |
| ω execution (Ω ops) | `MLF.Witness.OmegaExec` |

### Graphic Constraints (ICFP'08 / TLDI'07)

**Paper**: Graphic constraints papers for solver pipeline
**Code**:

| Concept | Module |
|---------|--------|
| Constraint graph | `MLF.Constraint.Types.Graph` |
| Binding tree | `MLF.Constraint.Types.Graph.Binding` |
| Canonicalization | `MLF.Constraint.Canonicalizer` |
| Unification core | `MLF.Constraint.Unify.Core` |

## Known Deviations

Canonical deviation register: [`docs/thesis-deviations.yaml`](thesis-deviations.yaml)

Enforced by: `scripts/check-thesis-claims.sh` (no `status: open` deviations, no orphans, all cross-linked to claims).

## Audit Checklist

Canonical claims registry: [`docs/thesis-claims.yaml`](thesis-claims.yaml)

Each claim carries evidence chains (obligations, property tests, code paths) and is continuously enforced by `scripts/thesis-conformance-gate.sh`.

To verify all claims: `./scripts/check-thesis-claims.sh`
