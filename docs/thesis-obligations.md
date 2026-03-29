# Thesis Obligations Ledger

Generated from `docs/thesis-obligations.yaml` by `scripts/render-thesis-obligations-ledger.rb`.

## Summary

- Total obligations: **107**
- Status counts: `anchored`=107
- Chapters covered: 4, 5, 7, 8, 9, 10, 11, 12, 14, 15

## Chapter 4

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O04-BIND-FLEX-CHILDREN` | `4.2` | `Definition 4.2 Q(n) flex children` | Q(n) flex children | `O04-BIND-FLEX-CHILDREN` | `test/BindingSpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-BIND-INTERIOR` | `4.2` | `Definition 4.2 I(r) interior` | I(r) interior | `O04-BIND-INTERIOR` | `test/BindingSpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-BIND-ORDER` | `4.2` | `Definition 4.2 binder ordering` | Binder ordering ≺ | `O04-BIND-ORDER` | `test/BindingSpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-OP-RAISE-STEP` | `4.4` | `Definition 4.4 Raise(n) single step` | Raise(n) single step | `O04-OP-RAISE-STEP` | `test/GraphOpsSpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-OP-RAISE-TO` | `4.4` | `Definition 4.4 Raise-to-target` | Raise-to-target | `O04-OP-RAISE-TO` | `test/GraphOpsSpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-OP-WEAKEN` | `4.4` | `Definition 4.4 Weaken(n)` | Weaken(n) | `O04-OP-WEAKEN` | `test/GraphOpsSpec.hs` | `CLM-CGEN-SHAPE` |

## Chapter 5

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O05-INERT-LOCKED` | `5.2` | `Definition 15.2.2 inert-locked` | Inert-locked nodes | `O05-INERT-LOCKED` | `test/InertSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O05-INERT-NODES` | `5.2` | `Definition 5.2.2 inert nodes` | Inert nodes | `O05-INERT-NODES` | `test/InertSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O05-WEAKEN-INERT` | `5.3` | `Section 15.2.3.2 weaken inert-locked` | Weaken inert-locked | `O05-WEAKEN-INERT` | `test/InertSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |

## Chapter 7

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O07-REBIND` | `7.3` | `Figure 7.3.x Rebind` | Rebind | `O07-REBIND` | `test/BindingSpec.hs` | `CLM-UNIFICATION` |
| `O07-UNIF-CORE` | `7.3` | `Figure 7.3.x Unif` | Core unification | `O07-UNIF-CORE` | `test/NormalizeSpec.hs` | `CLM-UNIFICATION` |
| `O07-UNIF-PRESOL` | `7.3` | `Presolution unify` | Presolution unify | `O07-UNIF-PRESOL` | `test/SolveSpec.hs` | `CLM-UNIFICATION` |
| `O07-GENUNIF` | `7.6` | `Definition 7.6.1, Definition 7.6.2, Section 7.6.2, Lemma 7.6.3` | Generalized unification | `Generalized unification (Ch 7.6)` | `test/SolveSpec.hs` | `CLM-UNIFICATION`, `CLM-GEN-UNIFICATION` |

## Chapter 8

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O08-BIND-MONO` | `8.2` | `Figure 8.2.2` | B(σ) binding monomorphic subtypes | `O08-BIND-MONO` | `test/PipelineSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-REIFY-NAMES` | `8.2` | `Named reification` | Named reification | `O08-REIFY-NAMES` | `test/PipelineSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-REIFY-TYPE` | `8.2` | `Graphic to syntactic reification` | Graphic→syntactic | `O08-REIFY-TYPE` | `test/PipelineSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-SYN-TO-GRAPH` | `8.2` | `Figure 8.2.3` | G(σ) syntactic to graphic | `O08-SYN-TO-GRAPH` | `test/PipelineSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-INLINE-PRED` | `8.3` | `Inline(τ,n) predicate` | Inline predicate | `O08-INLINE-PRED` | `test/PipelineSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-REIFY-INLINE` | `8.3` | `Figure 8.3.3` | Sᵢ(τ) reification with inlining | `O08-REIFY-INLINE` | `test/PipelineSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |

## Chapter 9

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O09-CGEN-EXPR` | `9.4` | `Expression constraint generation` | Expr constraint | `O09-CGEN-EXPR` | `test/ConstraintGenSpec.hs` | `CLM-CGEN-SHAPE`, `CLM-CGEN-SCOPING` |
| `O09-CGEN-ROOT` | `9.4` | `Root constraint generation` | Root constraint | `O09-CGEN-ROOT` | `test/ConstraintGenSpec.hs` | `CLM-CGEN-SHAPE`, `CLM-CGEN-SCOPING` |

## Chapter 10

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O10-EXP-APPLY` | `10.1` | `Apply expansion` | Apply expansion | `O10-EXP-APPLY` | `test/Presolution/ExpansionSpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-EXP-DECIDE` | `10.1` | `Decide minimal expansion` | Decide minimal expansion | `O10-EXP-DECIDE` | `test/Presolution/ExpansionSpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-PROP-SOLVE` | `10.3` | `Propagation rule` | Propagation rule | `O10-PROP-SOLVE` | `test/Presolution/EdgeTraceSpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-PROP-WITNESS` | `10.3` | `Edge witness recording` | Edge witness recording | `O10-PROP-WITNESS` | `test/Presolution/EdgeTraceSpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-COPY-SCHEME` | `10.4` | `Chi-e scheme copy` | Chi-e scheme copy | `O10-COPY-SCHEME` | `test/Presolution/InstantiateSpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |

## Chapter 11

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O11-UNIFY-STRUCT` | `11.2` | `Structural unify on constraints` | Structural unify | `O11-UNIFY-STRUCT` | `test/NormalizeSpec.hs` | `CLM-WITNESS-NORMALIZATION` |
| `O11-WITNESS-NORM` | `11.5` | `Witness normalization` | Witness normalization | `O11-WITNESS-NORM` | `test/Presolution/WitnessSpec.hs` | `CLM-WITNESS-NORMALIZATION` |
| `O11-WITNESS-COALESCE` | `11.6` | `Raise;Merge to RaiseMerge` | Raise;Merge coalescing | `O11-WITNESS-COALESCE` | `test/Presolution/WitnessSpec.hs` | `CLM-WITNESS-NORMALIZATION` |
| `O11-WITNESS-REORDER` | `11.6` | `Weaken reordering` | Weaken reordering | `O11-WITNESS-REORDER` | `test/Presolution/WitnessSpec.hs` | `CLM-WITNESS-NORMALIZATION` |

## Chapter 12

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O12-ACYCLIC-CHECK` | `12.1` | `Acyclicity check` | Acyclicity check | `O12-ACYCLIC-CHECK` | `test/AcyclicitySpec.hs` | `CLM-ACYCLICITY` |
| `O12-ACYCLIC-TOPO` | `12.1` | `Topological sort` | Topological sort | `O12-ACYCLIC-TOPO` | `test/AcyclicitySpec.hs` | `CLM-ACYCLICITY` |
| `O12-SOLVE-ARROW` | `12.1` | `Arrow decomposition` | Arrow decomposition | `O12-SOLVE-ARROW` | `test/SolveSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-HARMONIZE` | `12.1` | `Binding harmonization during solve` | Binding harmonization | `O12-SOLVE-HARMONIZE` | `test/SolveSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-UNIFY` | `12.1` | `SolveConstraint main` | SolveConstraint main | `O12-SOLVE-UNIFY` | `test/SolveSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-VALIDATE` | `12.1` | `Post-solve validation` | Post-solve validation | `O12-SOLVE-VALIDATE` | `test/SolveSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-VAR-BASE` | `12.1` | `Var = Base merge` | Var = Base merge | `O12-SOLVE-VAR-BASE` | `test/SolveSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-VAR-VAR` | `12.1` | `Var = Var merge` | Var = Var merge | `O12-SOLVE-VAR-VAR` | `test/SolveSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-COPY-INST` | `12.2` | `Inst-Copy rule` | Inst-Copy rule | `O12-COPY-INST` | `test/Presolution/InstantiateSpec.hs` | `CLM-SOLVER-CORRECTNESS`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O12-NORM-DROP` | `12.4` | `Drop reflexive edges` | Drop reflexive edges | `O12-NORM-DROP` | `test/NormalizeSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-NORM-FIXPOINT` | `12.4` | `Normalize to fixed point` | Normalize to fixed point | `O12-NORM-FIXPOINT` | `test/NormalizeSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-NORM-GRAFT` | `12.4` | `Graft inst edges` | Graft inst edges | `O12-NORM-GRAFT` | `test/NormalizeSpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-NORM-MERGE` | `12.4` | `Merge unify edges` | Merge unify edges | `O12-NORM-MERGE` | `test/NormalizeSpec.hs` | `CLM-SOLVER-CORRECTNESS` |

## Chapter 14

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O14-WF-EMPTY` | `14.2` | `Figure 14.2.4` | wf-empty | `O14-WF-EMPTY` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-WF-TVAR` | `14.2` | `Figure 14.2.4` | wf-tvar | `O14-WF-TVAR` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-WF-VAR` | `14.2` | `Figure 14.2.4` | wf-var | `O14-WF-VAR` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-INST-BOT` | `14.2.2` | `Figure 14.2.6` | Inst-Bot | `O14-INST-BOT` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-HYP` | `14.2.2` | `Figure 14.2.6` | Inst-Hyp | `O14-INST-HYP` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-INNER` | `14.2.2` | `Figure 14.2.6` | Inst-Inner | `O14-INST-INNER` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-OUTER` | `14.2.2` | `Figure 14.2.6` | Inst-Outer | `O14-INST-OUTER` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-QUANT-ELIM` | `14.2.2` | `Figure 14.2.6` | Inst-Quant-Elim | `O14-INST-QUANT-ELIM` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-QUANT-INTRO` | `14.2.2` | `Figure 14.2.6` | Inst-Quant-Intro | `O14-INST-QUANT-INTRO` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-REFLEX` | `14.2.2` | `Figure 14.2.6` | Inst-Reflex | `O14-INST-REFLEX` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-TRANS` | `14.2.2` | `Figure 14.2.6` | Inst-Trans | `O14-INST-TRANS` | `test/TypeCheckSpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-APPLY-BOT` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-BOT` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-HYP` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-HYP` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-ID` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-ID` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-INNER` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-INNER` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-N` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-N` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-O` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-O` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-OUTER` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-OUTER` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-SEQ` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-SEQ` | `test/ElaborationSpec.hs` | `CLM-INST-APPLICATION` |
| `O14-T-ABS` | `14.2.3` | `Figure 14.2.8` | Abs | `O14-T-ABS` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-APP` | `14.2.3` | `Figure 14.2.8` | App | `O14-T-APP` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-LET` | `14.2.3` | `Figure 14.2.8` | Let | `O14-T-LET` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-TABS` | `14.2.3` | `Figure 14.2.8` | TAbs | `O14-T-TABS` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-TAPP` | `14.2.3` | `Figure 14.2.8` | TApp | `O14-T-TAPP` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-VAR` | `14.2.3` | `Figure 14.2.8` | Var | `O14-T-VAR` | `test/TypeCheckSpec.hs` | `CLM-TYPING-RULES` |
| `O14-RED-BETA` | `14.3` | `Figure 14.3.1` | (β) | `O14-RED-BETA` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-BETALET` | `14.3` | `Figure 14.3.1` | (βLet) | `O14-RED-BETALET` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-CONTEXT` | `14.3` | `Figure 14.3.1` | Context | `O14-RED-CONTEXT` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-INNER` | `14.3` | `Figure 14.3.1` | Inner | `O14-RED-INNER` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-OUTER` | `14.3` | `Figure 14.3.1` | Outer | `O14-RED-OUTER` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-QUANT-ELIM` | `14.3` | `Figure 14.3.1` | Quant-Elim | `O14-RED-QUANT-ELIM` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-QUANT-INTRO` | `14.3` | `Figure 14.3.1` | Quant-Intro | `O14-RED-QUANT-INTRO` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-REFLEX` | `14.3` | `Figure 14.3.1` | Reflex | `O14-RED-REFLEX` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-TRANS` | `14.3` | `Figure 14.3.1` | Trans | `O14-RED-TRANS` | `test/ReduceSpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |

## Chapter 15

| ID | Section | Figure/Def | Rule | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|
| `O15-TRANS-ARROW-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(3) | `O15-TRANS-ARROW-RIGID` | `test/Presolution/EnforcementSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-TRANS-NO-INERT-LOCKED` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(1) | `O15-TRANS-NO-INERT-LOCKED` | `test/Presolution/EnforcementSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-TRANS-NON-INTERIOR-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(4) | `O15-TRANS-NON-INTERIOR-RIGID` | `test/Presolution/EnforcementSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-TRANS-SCHEME-ROOT-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(2) | `O15-TRANS-SCHEME-ROOT-RIGID` | `test/Presolution/EnforcementSpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-ENV-LAMBDA` | `15.3.3` | `Definition 15.3.6` | Typing environment lambda extension | `O15-ENV-LAMBDA` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ENV-LET` | `15.3.3` | `Definition 15.3.6` | Typing environment let extension | `O15-ENV-LET` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ENV-WF` | `15.3.3` | `Property 15.3.7` | Typing environments are well formed | `O15-ENV-WF` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-CONTEXT-FIND` | `15.3.4` | `Computation contexts` | O15-CONTEXT-FIND | `O15-CONTEXT-FIND` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS` |
| `O15-CONTEXT-REJECT` | `15.3.4` | `Computation contexts` | O15-CONTEXT-REJECT | `O15-CONTEXT-REJECT` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS` |
| `O15-REORDER-IDENTITY` | `15.3.4` | `Definition 15.3.4` | O15-REORDER-IDENTITY | `O15-REORDER-IDENTITY` | `test/ElaborationSpec.hs` | `CLM-SIGMA-REORDER` |
| `O15-REORDER-REQUIRED` | `15.3.4` | `Definition 15.3.4` | O15-REORDER-REQUIRED | `O15-REORDER-REQUIRED` | `test/ElaborationSpec.hs` | `CLM-SIGMA-REORDER` |
| `O15-EDGE-TRANSLATION` | `15.3.5` | `Definition 15.3.12` | O15-EDGE-TRANSLATION | `O15-EDGE-TRANSLATION` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS` |
| `O15-TR-NODE-GRAFT` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-GRAFT | `O15-TR-NODE-GRAFT` | `test/Presolution/WitnessSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-MERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-MERGE | `O15-TR-NODE-MERGE` | `test/Presolution/WitnessSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-RAISE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-RAISE | `O15-TR-NODE-RAISE` | `test/Presolution/WitnessSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-RAISEMERGE | `O15-TR-NODE-RAISEMERGE` | `test/Presolution/WitnessSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-WEAKEN` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-WEAKEN | `O15-TR-NODE-WEAKEN` | `test/Presolution/WitnessSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-RIGID-MERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-MERGE | `O15-TR-RIGID-MERGE` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-RIGID-RAISE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-RAISE | `O15-TR-RIGID-RAISE` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-RIGID-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-RAISEMERGE | `O15-TR-RIGID-RAISEMERGE` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-ROOT-GRAFT` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-GRAFT | `O15-TR-ROOT-GRAFT` | `test/Presolution/WitnessSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-ROOT-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-RAISEMERGE | `O15-TR-ROOT-RAISEMERGE` | `test/Presolution/MergeEmissionSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-ROOT-WEAKEN` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-WEAKEN | `O15-TR-ROOT-WEAKEN` | `test/Presolution/MergeEmissionSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-SEQ-CONS` | `15.3.5` | `Figure 15.3.4` | Trχ row SEQ-CONS | `O15-TR-SEQ-CONS` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-SEQ-EMPTY` | `15.3.5` | `Figure 15.3.4` | Trχ row SEQ-EMPTY | `O15-TR-SEQ-EMPTY` | `test/ElaborationSpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-ELAB-ABS` | `15.3.6` | `Figure 15.3.5` | Elaboration case ABS | `O15-ELAB-ABS` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-APP` | `15.3.6` | `Figure 15.3.5` | Elaboration case APP | `O15-ELAB-APP` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-LAMBDA-VAR` | `15.3.6` | `Figure 15.3.5` | Elaboration case LAMBDA-VAR | `O15-ELAB-LAMBDA-VAR` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-LET` | `15.3.6` | `Figure 15.3.5` | Elaboration case LET | `O15-ELAB-LET` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-LET-VAR` | `15.3.6` | `Figure 15.3.5` | Elaboration case LET-VAR | `O15-ELAB-LET-VAR` | `test/ElaborationSpec.hs` | `CLM-ELABORATION-CORRECTNESS` |

## Validation Notes

- This file is generated; edit the YAML source instead.
- Gate enforcement additionally verifies id set, mapping completeness, file/symbol anchors, and executable test anchors.
