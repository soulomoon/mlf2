# Thesis Obligations Ledger

Generated from `docs/thesis-obligations.yaml` by `scripts/render-thesis-obligations-ledger.rb`.

## Summary

- Total obligations: **107**
- Status counts: `anchored`=107
- Chapters covered: 4, 5, 7, 8, 9, 10, 11, 12, 14, 15

## Chapter 4

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O04-BIND-FLEX-CHILDREN` | `4.2` | `Definition 4.2 Q(n) flex children` | Q(n) flex children | `quickcheck` | `100` | `O04-BIND-FLEX-CHILDREN` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-BIND-INTERIOR` | `4.2` | `Definition 4.2 I(r) interior` | I(r) interior | `quickcheck` | `100` | `O04-BIND-INTERIOR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-BIND-ORDER` | `4.2` | `Definition 4.2 binder ordering` | Binder ordering ≺ | `quickcheck` | `100` | `O04-BIND-ORDER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-OP-RAISE-STEP` | `4.4` | `Definition 4.4 Raise(n) single step` | Raise(n) single step | `quickcheck` | `100` | `O04-OP-RAISE-STEP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-OP-RAISE-TO` | `4.4` | `Definition 4.4 Raise-to-target` | Raise-to-target | `quickcheck` | `100` | `O04-OP-RAISE-TO` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE` |
| `O04-OP-WEAKEN` | `4.4` | `Definition 4.4 Weaken(n)` | Weaken(n) | `quickcheck` | `100` | `O04-OP-WEAKEN` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE` |

## Chapter 5

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O05-INERT-LOCKED` | `5.2` | `Definition 15.2.2 inert-locked` | Inert-locked nodes | `quickcheck` | `100` | `O05-INERT-LOCKED` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O05-INERT-NODES` | `5.2` | `Definition 5.2.2 inert nodes` | Inert nodes | `quickcheck` | `100` | `O05-INERT-NODES` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O05-WEAKEN-INERT` | `5.3` | `Section 15.2.3.2 weaken inert-locked` | Weaken inert-locked | `quickcheck` | `100` | `O05-WEAKEN-INERT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |

## Chapter 7

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O07-REBIND` | `7.3` | `Figure 7.3.x Rebind` | Rebind | `quickcheck` | `100` | `O07-REBIND` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-UNIFICATION` |
| `O07-UNIF-CORE` | `7.3` | `Figure 7.3.x Unif` | Core unification | `quickcheck` | `100` | `O07-UNIF-CORE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-UNIFICATION` |
| `O07-UNIF-PRESOL` | `7.3` | `Presolution unify` | Presolution unify | `quickcheck` | `100` | `O07-UNIF-PRESOL` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-UNIFICATION` |
| `O07-GENUNIF` | `7.6` | `Definition 7.6.1, Definition 7.6.2, Section 7.6.2, Lemma 7.6.3` | Generalized unification | `quickcheck` | `100` | `O07-GENUNIF` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-UNIFICATION`, `CLM-GEN-UNIFICATION` |

## Chapter 8

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O08-BIND-MONO` | `8.2` | `Figure 8.2.2` | B(σ) binding monomorphic subtypes | `quickcheck` | `100` | `O08-BIND-MONO` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-REIFY-NAMES` | `8.2` | `Named reification` | Named reification | `quickcheck` | `100` | `O08-REIFY-NAMES` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-REIFY-TYPE` | `8.2` | `Graphic to syntactic reification` | Graphic→syntactic | `quickcheck` | `100` | `O08-REIFY-TYPE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-SYN-TO-GRAPH` | `8.2` | `Figure 8.2.3` | G(σ) syntactic to graphic | `quickcheck` | `100` | `O08-SYN-TO-GRAPH` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-INLINE-PRED` | `8.3` | `Inline(τ,n) predicate` | Inline predicate | `quickcheck` | `100` | `O08-INLINE-PRED` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O08-REIFY-INLINE` | `8.3` | `Figure 8.3.3` | Sᵢ(τ) reification with inlining | `quickcheck` | `100` | `O08-REIFY-INLINE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |

## Chapter 9

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O09-CGEN-EXPR` | `9.4` | `Expression constraint generation` | Expr constraint | `quickcheck` | `100` | `O09-CGEN-EXPR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE`, `CLM-CGEN-SCOPING` |
| `O09-CGEN-ROOT` | `9.4` | `Root constraint generation` | Root constraint | `quickcheck` | `100` | `O09-CGEN-ROOT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-CGEN-SHAPE`, `CLM-CGEN-SCOPING` |

## Chapter 10

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O10-EXP-APPLY` | `10.1` | `Apply expansion` | Apply expansion | `quickcheck` | `100` | `O10-EXP-APPLY` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-EXP-DECIDE` | `10.1` | `Decide minimal expansion` | Decide minimal expansion | `quickcheck` | `100` | `O10-EXP-DECIDE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-PROP-SOLVE` | `10.3` | `Propagation rule` | Propagation rule | `quickcheck` | `100` | `O10-PROP-SOLVE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-PROP-WITNESS` | `10.3` | `Edge witness recording` | Edge witness recording | `quickcheck` | `100` | `O10-PROP-WITNESS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O10-COPY-SCHEME` | `10.4` | `Chi-e scheme copy` | Chi-e scheme copy | `quickcheck` | `100` | `O10-COPY-SCHEME` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-EXPANSION-MINIMALITY`, `CLM-PRESOLUTION-PRINCIPALITY` |

## Chapter 11

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O11-UNIFY-STRUCT` | `11.2` | `Structural unify on constraints` | Structural unify | `quickcheck` | `100` | `O11-UNIFY-STRUCT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-WITNESS-NORMALIZATION` |
| `O11-WITNESS-NORM` | `11.5` | `Witness normalization` | Witness normalization | `quickcheck` | `100` | `O11-WITNESS-NORM` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-WITNESS-NORMALIZATION` |
| `O11-WITNESS-COALESCE` | `11.6` | `Raise;Merge to RaiseMerge` | Raise;Merge coalescing | `quickcheck` | `100` | `O11-WITNESS-COALESCE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-WITNESS-NORMALIZATION` |
| `O11-WITNESS-REORDER` | `11.6` | `Weaken reordering` | Weaken reordering | `quickcheck` | `100` | `O11-WITNESS-REORDER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-WITNESS-NORMALIZATION` |

## Chapter 12

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O12-ACYCLIC-CHECK` | `12.1` | `Acyclicity check` | Acyclicity check | `quickcheck` | `100` | `O12-ACYCLIC-CHECK` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ACYCLICITY` |
| `O12-ACYCLIC-TOPO` | `12.1` | `Topological sort` | Topological sort | `quickcheck` | `100` | `O12-ACYCLIC-TOPO` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ACYCLICITY` |
| `O12-SOLVE-ARROW` | `12.1` | `Arrow decomposition` | Arrow decomposition | `quickcheck` | `100` | `O12-SOLVE-ARROW` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-HARMONIZE` | `12.1` | `Binding harmonization during solve` | Binding harmonization | `quickcheck` | `100` | `O12-SOLVE-HARMONIZE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-UNIFY` | `12.1` | `SolveConstraint main` | SolveConstraint main | `quickcheck` | `100` | `O12-SOLVE-UNIFY` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-VALIDATE` | `12.1` | `Post-solve validation` | Post-solve validation | `quickcheck` | `100` | `O12-SOLVE-VALIDATE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-VAR-BASE` | `12.1` | `Var = Base merge` | Var = Base merge | `quickcheck` | `100` | `O12-SOLVE-VAR-BASE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-SOLVE-VAR-VAR` | `12.1` | `Var = Var merge` | Var = Var merge | `quickcheck` | `100` | `O12-SOLVE-VAR-VAR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-COPY-INST` | `12.2` | `Inst-Copy rule` | Inst-Copy rule | `quickcheck` | `100` | `O12-COPY-INST` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS`, `CLM-PRESOLUTION-PRINCIPALITY` |
| `O12-NORM-DROP` | `12.4` | `Drop reflexive edges` | Drop reflexive edges | `quickcheck` | `100` | `O12-NORM-DROP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-NORM-FIXPOINT` | `12.4` | `Normalize to fixed point` | Normalize to fixed point | `quickcheck` | `100` | `O12-NORM-FIXPOINT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-NORM-GRAFT` | `12.4` | `Graft inst edges` | Graft inst edges | `quickcheck` | `100` | `O12-NORM-GRAFT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |
| `O12-NORM-MERGE` | `12.4` | `Merge unify edges` | Merge unify edges | `quickcheck` | `100` | `O12-NORM-MERGE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SOLVER-CORRECTNESS` |

## Chapter 14

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O14-WF-EMPTY` | `14.2` | `Figure 14.2.4` | wf-empty | `quickcheck` | `100` | `O14-WF-EMPTY` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-WF-TVAR` | `14.2` | `Figure 14.2.4` | wf-tvar | `quickcheck` | `100` | `O14-WF-TVAR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-WF-VAR` | `14.2` | `Figure 14.2.4` | wf-var | `quickcheck` | `100` | `O14-WF-VAR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-INST-BOT` | `14.2.2` | `Figure 14.2.6` | Inst-Bot | `quickcheck` | `100` | `O14-INST-BOT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-HYP` | `14.2.2` | `Figure 14.2.6` | Inst-Hyp | `quickcheck` | `100` | `O14-INST-HYP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-INNER` | `14.2.2` | `Figure 14.2.6` | Inst-Inner | `quickcheck` | `100` | `O14-INST-INNER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-OUTER` | `14.2.2` | `Figure 14.2.6` | Inst-Outer | `quickcheck` | `100` | `O14-INST-OUTER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-QUANT-ELIM` | `14.2.2` | `Figure 14.2.6` | Inst-Quant-Elim | `quickcheck` | `100` | `O14-INST-QUANT-ELIM` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-QUANT-INTRO` | `14.2.2` | `Figure 14.2.6` | Inst-Quant-Intro | `quickcheck` | `100` | `O14-INST-QUANT-INTRO` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-REFLEX` | `14.2.2` | `Figure 14.2.6` | Inst-Reflex | `quickcheck` | `100` | `O14-INST-REFLEX` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-INST-TRANS` | `14.2.2` | `Figure 14.2.6` | Inst-Trans | `quickcheck` | `100` | `O14-INST-TRANS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-CORRECTNESS` |
| `O14-APPLY-BOT` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-BOT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-HYP` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-HYP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-ID` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-ID` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-INNER` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-INNER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-N` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-N` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-O` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-O` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-OUTER` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-OUTER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-APPLY-SEQ` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `quickcheck` | `100` | `O14-APPLY-SEQ` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-INST-APPLICATION` |
| `O14-T-ABS` | `14.2.3` | `Figure 14.2.8` | Abs | `quickcheck` | `100` | `O14-T-ABS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-APP` | `14.2.3` | `Figure 14.2.8` | App | `quickcheck` | `100` | `O14-T-APP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-LET` | `14.2.3` | `Figure 14.2.8` | Let | `quickcheck` | `100` | `O14-T-LET` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-TABS` | `14.2.3` | `Figure 14.2.8` | TAbs | `quickcheck` | `100` | `O14-T-TABS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-TAPP` | `14.2.3` | `Figure 14.2.8` | TApp | `quickcheck` | `100` | `O14-T-TAPP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-T-VAR` | `14.2.3` | `Figure 14.2.8` | Var | `quickcheck` | `100` | `O14-T-VAR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TYPING-RULES` |
| `O14-RED-BETA` | `14.3` | `Figure 14.3.1` | (β) | `quickcheck` | `100` | `O14-RED-BETA` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-BETALET` | `14.3` | `Figure 14.3.1` | (βLet) | `quickcheck` | `100` | `O14-RED-BETALET` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-CONTEXT` | `14.3` | `Figure 14.3.1` | Context | `quickcheck` | `100` | `O14-RED-CONTEXT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-INNER` | `14.3` | `Figure 14.3.1` | Inner | `quickcheck` | `100` | `O14-RED-INNER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-OUTER` | `14.3` | `Figure 14.3.1` | Outer | `quickcheck` | `100` | `O14-RED-OUTER` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-QUANT-ELIM` | `14.3` | `Figure 14.3.1` | Quant-Elim | `quickcheck` | `100` | `O14-RED-QUANT-ELIM` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-QUANT-INTRO` | `14.3` | `Figure 14.3.1` | Quant-Intro | `quickcheck` | `100` | `O14-RED-QUANT-INTRO` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-REFLEX` | `14.3` | `Figure 14.3.1` | Reflex | `quickcheck` | `100` | `O14-RED-REFLEX` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |
| `O14-RED-TRANS` | `14.3` | `Figure 14.3.1` | Trans | `quickcheck` | `100` | `O14-RED-TRANS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PRESERVATION`, `CLM-REDUCTION-RULES` |

## Chapter 15

| ID | Section | Figure/Def | Rule | Evidence | Min Success | Test Matcher | Test File | Claims |
|---|---|---|---|---|---|---|---|---|
| `O15-TRANS-ARROW-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(3) | `quickcheck` | `100` | `O15-TRANS-ARROW-RIGID` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-TRANS-NO-INERT-LOCKED` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(1) | `quickcheck` | `100` | `O15-TRANS-NO-INERT-LOCKED` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-TRANS-NON-INTERIOR-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(4) | `quickcheck` | `100` | `O15-TRANS-NON-INTERIOR-RIGID` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-TRANS-SCHEME-ROOT-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(2) | `quickcheck` | `100` | `O15-TRANS-SCHEME-ROOT-RIGID` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-TRANSLATABLE-PRESOLUTION` |
| `O15-ENV-LAMBDA` | `15.3.3` | `Definition 15.3.6` | Typing environment lambda extension | `quickcheck` | `100` | `O15-ENV-LAMBDA` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ENV-LET` | `15.3.3` | `Definition 15.3.6` | Typing environment let extension | `quickcheck` | `100` | `O15-ENV-LET` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ENV-WF` | `15.3.3` | `Property 15.3.7` | Typing environments are well formed | `quickcheck` | `100` | `O15-ENV-WF` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-CONTEXT-FIND` | `15.3.4` | `Computation contexts` | O15-CONTEXT-FIND | `quickcheck` | `100` | `O15-CONTEXT-FIND` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS` |
| `O15-CONTEXT-REJECT` | `15.3.4` | `Computation contexts` | O15-CONTEXT-REJECT | `quickcheck` | `100` | `O15-CONTEXT-REJECT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS` |
| `O15-REORDER-IDENTITY` | `15.3.4` | `Definition 15.3.4` | O15-REORDER-IDENTITY | `quickcheck` | `100` | `O15-REORDER-IDENTITY` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SIGMA-REORDER` |
| `O15-REORDER-REQUIRED` | `15.3.4` | `Definition 15.3.4` | O15-REORDER-REQUIRED | `quickcheck` | `100` | `O15-REORDER-REQUIRED` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-SIGMA-REORDER` |
| `O15-EDGE-TRANSLATION` | `15.3.5` | `Definition 15.3.12` | O15-EDGE-TRANSLATION | `quickcheck` | `100` | `O15-EDGE-TRANSLATION` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS` |
| `O15-TR-NODE-GRAFT` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-GRAFT | `quickcheck` | `100` | `O15-TR-NODE-GRAFT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-MERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-MERGE | `quickcheck` | `100` | `O15-TR-NODE-MERGE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-RAISE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-RAISE | `quickcheck` | `100` | `O15-TR-NODE-RAISE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-RAISEMERGE | `quickcheck` | `100` | `O15-TR-NODE-RAISEMERGE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-NODE-WEAKEN` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-WEAKEN | `quickcheck` | `100` | `O15-TR-NODE-WEAKEN` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-RIGID-MERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-MERGE | `quickcheck` | `100` | `O15-TR-RIGID-MERGE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-RIGID-RAISE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-RAISE | `quickcheck` | `100` | `O15-TR-RIGID-RAISE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-RIGID-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-RAISEMERGE | `quickcheck` | `100` | `O15-TR-RIGID-RAISEMERGE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-ROOT-GRAFT` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-GRAFT | `quickcheck` | `100` | `O15-TR-ROOT-GRAFT` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-ROOT-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-RAISEMERGE | `quickcheck` | `100` | `O15-TR-ROOT-RAISEMERGE` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-ROOT-WEAKEN` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-WEAKEN | `quickcheck` | `100` | `O15-TR-ROOT-WEAKEN` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-SEQ-CONS` | `15.3.5` | `Figure 15.3.4` | Trχ row SEQ-CONS | `quickcheck` | `100` | `O15-TR-SEQ-CONS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-TR-SEQ-EMPTY` | `15.3.5` | `Figure 15.3.4` | Trχ row SEQ-EMPTY | `quickcheck` | `100` | `O15-TR-SEQ-EMPTY` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-PHI-CORRECTNESS`, `CLM-WITNESS-TRANSLATION` |
| `O15-ELAB-ABS` | `15.3.6` | `Figure 15.3.5` | Elaboration case ABS | `quickcheck` | `100` | `O15-ELAB-ABS` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-APP` | `15.3.6` | `Figure 15.3.5` | Elaboration case APP | `quickcheck` | `100` | `O15-ELAB-APP` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-LAMBDA-VAR` | `15.3.6` | `Figure 15.3.5` | Elaboration case LAMBDA-VAR | `quickcheck` | `100` | `O15-ELAB-LAMBDA-VAR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-LET` | `15.3.6` | `Figure 15.3.5` | Elaboration case LET | `quickcheck` | `100` | `O15-ELAB-LET` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |
| `O15-ELAB-LET-VAR` | `15.3.6` | `Figure 15.3.5` | Elaboration case LET-VAR | `quickcheck` | `100` | `O15-ELAB-LET-VAR` | `test/Thesis/ObligationPropertySpec.hs` | `CLM-ELABORATION-CORRECTNESS` |

## Validation Notes

- This file is generated; edit the YAML source instead.
- Gate enforcement additionally verifies id set, mapping completeness, code-anchor files, executable test anchors keyed by obligation ID, QuickCheck evidence kind, and minimum property success counts.
- Code-anchor fragments are navigational labels; QuickCheck property anchors carry the semantic obligation evidence.
