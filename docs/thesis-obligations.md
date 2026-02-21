# Thesis Obligations Ledger (Chapters 14/15)

Generated from `/Volumes/src/mlf4/docs/thesis-obligations-ch14-15.yaml` by `scripts/render-thesis-obligations-ledger.rb`.

## Summary

- Total obligations: **61**
- Status counts: `anchored`=61
- Chapters covered: 14, 15

## Chapter 14

| ID | Section | Figure/Def | Rule | Test Matcher | Test File |
|---|---|---|---|---|---|
| `O14-WF-EMPTY` | `14.2` | `Figure 14.2.4` | wf-empty | `O14-WF-EMPTY` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-WF-TVAR` | `14.2` | `Figure 14.2.4` | wf-tvar | `O14-WF-TVAR` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-WF-VAR` | `14.2` | `Figure 14.2.4` | wf-var | `O14-WF-VAR` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-BOT` | `14.2.2` | `Figure 14.2.6` | Inst-Bot | `O14-INST-BOT` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-HYP` | `14.2.2` | `Figure 14.2.6` | Inst-Hyp | `O14-INST-HYP` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-INNER` | `14.2.2` | `Figure 14.2.6` | Inst-Inner | `O14-INST-INNER` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-OUTER` | `14.2.2` | `Figure 14.2.6` | Inst-Outer | `O14-INST-OUTER` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-QUANT-ELIM` | `14.2.2` | `Figure 14.2.6` | Inst-Quant-Elim | `O14-INST-QUANT-ELIM` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-QUANT-INTRO` | `14.2.2` | `Figure 14.2.6` | Inst-Quant-Intro | `O14-INST-QUANT-INTRO` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-REFLEX` | `14.2.2` | `Figure 14.2.6` | Inst-Reflex | `O14-INST-REFLEX` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-INST-TRANS` | `14.2.2` | `Figure 14.2.6` | Inst-Trans | `O14-INST-TRANS` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-APPLY-BOT` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-BOT` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-HYP` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-HYP` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-ID` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-ID` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-INNER` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-INNER` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-N` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-N` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-O` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-O` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-OUTER` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-OUTER` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-APPLY-SEQ` | `14.2.2.2` | `Figure 14.2.7` | Type computation application | `O14-APPLY-SEQ` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O14-T-ABS` | `14.2.3` | `Figure 14.2.8` | Abs | `O14-T-ABS` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-T-APP` | `14.2.3` | `Figure 14.2.8` | App | `O14-T-APP` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-T-LET` | `14.2.3` | `Figure 14.2.8` | Let | `O14-T-LET` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-T-TABS` | `14.2.3` | `Figure 14.2.8` | TAbs | `O14-T-TABS` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-T-TAPP` | `14.2.3` | `Figure 14.2.8` | TApp | `O14-T-TAPP` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-T-VAR` | `14.2.3` | `Figure 14.2.8` | Var | `O14-T-VAR` | `/Volumes/src/mlf4/test/TypeCheckSpec.hs` |
| `O14-RED-BETA` | `14.3` | `Figure 14.3.1` | (β) | `O14-RED-BETA` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-BETALET` | `14.3` | `Figure 14.3.1` | (βLet) | `O14-RED-BETALET` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-CONTEXT` | `14.3` | `Figure 14.3.1` | Context | `O14-RED-CONTEXT` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-INNER` | `14.3` | `Figure 14.3.1` | Inner | `O14-RED-INNER` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-OUTER` | `14.3` | `Figure 14.3.1` | Outer | `O14-RED-OUTER` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-QUANT-ELIM` | `14.3` | `Figure 14.3.1` | Quant-Elim | `O14-RED-QUANT-ELIM` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-QUANT-INTRO` | `14.3` | `Figure 14.3.1` | Quant-Intro | `O14-RED-QUANT-INTRO` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-REFLEX` | `14.3` | `Figure 14.3.1` | Reflex | `O14-RED-REFLEX` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |
| `O14-RED-TRANS` | `14.3` | `Figure 14.3.1` | Trans | `O14-RED-TRANS` | `/Volumes/src/mlf4/test/ReduceSpec.hs` |

## Chapter 15

| ID | Section | Figure/Def | Rule | Test Matcher | Test File |
|---|---|---|---|---|---|
| `O15-TRANS-ARROW-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(3) | `O15-TRANS-ARROW-RIGID` | `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs` |
| `O15-TRANS-NO-INERT-LOCKED` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(1) | `O15-TRANS-NO-INERT-LOCKED` | `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs` |
| `O15-TRANS-NON-INTERIOR-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(4) | `O15-TRANS-NON-INTERIOR-RIGID` | `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs` |
| `O15-TRANS-SCHEME-ROOT-RIGID` | `15.2.7` | `Definition 15.2.10` | Definition 15.2.10(2) | `O15-TRANS-SCHEME-ROOT-RIGID` | `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs` |
| `O15-CONTEXT-FIND` | `15.3.4` | `Computation contexts` | O15-CONTEXT-FIND | `O15-CONTEXT-FIND` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-CONTEXT-REJECT` | `15.3.4` | `Computation contexts` | O15-CONTEXT-REJECT | `O15-CONTEXT-REJECT` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-REORDER-IDENTITY` | `15.3.4` | `Definition 15.3.4` | O15-REORDER-IDENTITY | `O15-REORDER-IDENTITY` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-REORDER-REQUIRED` | `15.3.4` | `Definition 15.3.4` | O15-REORDER-REQUIRED | `O15-REORDER-REQUIRED` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-EDGE-TRANSLATION` | `15.3.5` | `Definition 15.3.12` | O15-EDGE-TRANSLATION | `O15-EDGE-TRANSLATION` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-TR-NODE-GRAFT` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-GRAFT | `O15-TR-NODE-GRAFT` | `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` |
| `O15-TR-NODE-MERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-MERGE | `O15-TR-NODE-MERGE` | `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` |
| `O15-TR-NODE-RAISE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-RAISE | `O15-TR-NODE-RAISE` | `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` |
| `O15-TR-NODE-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-RAISEMERGE | `O15-TR-NODE-RAISEMERGE` | `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` |
| `O15-TR-NODE-WEAKEN` | `15.3.5` | `Figure 15.3.4` | Trχ row NODE-WEAKEN | `O15-TR-NODE-WEAKEN` | `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` |
| `O15-TR-RIGID-MERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-MERGE | `O15-TR-RIGID-MERGE` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-TR-RIGID-RAISE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-RAISE | `O15-TR-RIGID-RAISE` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-TR-RIGID-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row RIGID-RAISEMERGE | `O15-TR-RIGID-RAISEMERGE` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-TR-ROOT-GRAFT` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-GRAFT | `O15-TR-ROOT-GRAFT` | `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` |
| `O15-TR-ROOT-RAISEMERGE` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-RAISEMERGE | `O15-TR-ROOT-RAISEMERGE` | `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs` |
| `O15-TR-ROOT-WEAKEN` | `15.3.5` | `Figure 15.3.4` | Trχ row ROOT-WEAKEN | `O15-TR-ROOT-WEAKEN` | `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs` |
| `O15-TR-SEQ-CONS` | `15.3.5` | `Figure 15.3.4` | Trχ row SEQ-CONS | `O15-TR-SEQ-CONS` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-TR-SEQ-EMPTY` | `15.3.5` | `Figure 15.3.4` | Trχ row SEQ-EMPTY | `O15-TR-SEQ-EMPTY` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-ELAB-ABS` | `15.3.6` | `Figure 15.3.5` | Elaboration case ABS | `O15-ELAB-ABS` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-ELAB-APP` | `15.3.6` | `Figure 15.3.5` | Elaboration case APP | `O15-ELAB-APP` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-ELAB-LAMBDA-VAR` | `15.3.6` | `Figure 15.3.5` | Elaboration case LAMBDA-VAR | `O15-ELAB-LAMBDA-VAR` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-ELAB-LET` | `15.3.6` | `Figure 15.3.5` | Elaboration case LET | `O15-ELAB-LET` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |
| `O15-ELAB-LET-VAR` | `15.3.6` | `Figure 15.3.5` | Elaboration case LET-VAR | `O15-ELAB-LET-VAR` | `/Volumes/src/mlf4/test/ElaborationSpec.hs` |

## Validation Notes

- This file is generated; edit the YAML source instead.
- Gate enforcement additionally verifies id set, mapping completeness, file/symbol anchors, and executable test anchors.
