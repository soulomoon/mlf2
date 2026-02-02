# Paper-to-Code Map

This document maps sections of `papers/these-finale-english.txt` (thesis) to the corresponding implementation modules and functions. Use this as an index for paper-faithfulness auditing.

## Primary Paper Reference

- **Thesis**: `papers/these-finale-english.txt` — primary source for xMLF and elaboration
- **Supplement**: `papers/xmlf.txt` — figure numbering and additional xMLF details

## Notation Mapping

| Paper | Meaning | Repo Types/Modules |
|------:|---------|-------------------|
| `b` | eMLF surface term | `MLF.Frontend.Syntax.Expr`, `SrcType`, `SrcScheme` |
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

1. **Quantifier introduction (`O`)**: In the thesis, `O` is not part of Ω. The repo records these as `StepIntro` entries in `EdgeWitness.ewSteps` (from `ExpForall`) and translates them interleaved with Ω segments when constructing Φ(e).

2. **Witness normalization**: The repo implements witness normalization/ordering (`normalizeEdgeWitnessesM`) but this is not yet backed by formal proofs from the thesis.

3. **Constraint representation**: The repo mirrors the paper's term-DAG + binding tree split (`cNodes` + `cBindParents` with `BindFlex`/`BindRigid`). Some paper machinery is simplified (e.g., fallback-removal relies on regression tests).

4. **xMLF Phase 7**: The repo includes type-checking and reduction (`MLF.Elab.TypeCheck`, `MLF.Elab.Reduce`) but lacks a fully formalized connection to the thesis presentation.

## Audit Checklist

When verifying paper alignment:

- [ ] Check `MLF.Constraint.Presolution.EdgeProcessing` for Raise/Merge/Weaken handling
- [ ] Verify `MLF.Elab.Pipeline.phiFromEdgeWitness` against Figure 10
- [ ] Confirm `MLF.Binding.Adjustment` implements paper `Raise(n)` correctly
- [ ] Validate `MLF.Elab.Generalize` follows `/)(g) = Λ(Q(g))` structure
- [ ] Check that `sigmaReorder` implements adjacent swaps per §3.4
