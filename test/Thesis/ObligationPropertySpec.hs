{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Thesis.ObligationPropertySpec (spec) where

import Control.Monad (forM_)
import Data.Either (isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import MLF.Binding.GraphOps qualified as GraphOps
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Acyclicity (AcyclicityResult (..), checkAcyclicity)
import MLF.Constraint.Inert qualified as Inert
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionError (..), PresolutionResult (..), PresolutionView (..))
import MLF.Constraint.Presolution.TestSupport
  ( PresolutionState (..),
    decideMinimalExpansion,
    fromListInterior,
    runPresolutionM,
    unifyAcyclic,
    validateTranslatablePresolution,
  )
import MLF.Constraint.Types.Presolution (Presolution (..))
import MLF.Constraint.Presolution.Witness
  ( OmegaNormalizeEnv (..),
    coalesceRaiseMergeWithEnv,
    normalizeInstanceOpsFull,
    reorderWeakenWithEnv,
    validateNormalizedWitness,
  )
import MLF.Constraint.Solve (SolveResult (..), frWith, solveUnify)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness (EdgeWitness (..), Expansion (..), ForallSpec (..), InstanceOp (..), InstanceWitness (..), ReplayContract (..))
import MLF.Constraint.Unify.Decompose (decomposeUnifyChildren)
import MLF.Elab.Pipeline qualified as Elab
import MLF.Frontend.ConstraintGen (ConstraintResult (..))
import MLF.Frontend.Syntax qualified as Surf
import Presolution.Util (mkNormalizeConstraint, mkNormalizeEnv)
import SpecUtil
  ( PipelineArtifacts (..),
    bindParentsFromPairs,
    defaultTraceConfig,
    emptyConstraint,
    nodeMapFromList,
    runConstraintDefault,
    runPipelineArtifactsDefault,
    runToPresolutionDefault,
    rootedConstraint,
    unsafeNormalizeExpr,
  )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Thesis obligation property evidence" $
  forM_ obligations $ \obligation ->
    it (obligationId obligation) $
      property $
        withMaxSuccess 100 $
          forAll (chooseInt (3, 16)) $ \size ->
            counterexample (obligationId obligation ++ " failed at size " ++ show size) $
              obligationProperty obligation size

data Obligation = Obligation
  { obligationId :: String,
    obligationProperty :: Int -> Property
  }

obligations :: [Obligation]
obligations =
  [ Obligation "O14-WF-EMPTY" propWfEmpty,
    Obligation "O14-WF-TVAR" propWfTVar,
    Obligation "O14-WF-VAR" propWfVar,
    Obligation "O14-INST-REFLEX" propInstReflex,
    Obligation "O14-INST-TRANS" propInstTrans,
    Obligation "O14-INST-BOT" propInstBot,
    Obligation "O14-INST-HYP" propInstHyp,
    Obligation "O14-INST-INNER" propInstInner,
    Obligation "O14-INST-OUTER" propInstOuter,
    Obligation "O14-INST-QUANT-ELIM" propInstQuantElim,
    Obligation "O14-INST-QUANT-INTRO" propInstQuantIntro,
    Obligation "O14-T-VAR" propTypingVar,
    Obligation "O14-T-ABS" propTypingAbs,
    Obligation "O14-T-APP" propTypingApp,
    Obligation "O14-T-TABS" propTypingTAbs,
    Obligation "O14-T-TAPP" propTypingTApp,
    Obligation "O14-T-LET" propTypingLet,
    Obligation "O14-RED-BETA" propRedBeta,
    Obligation "O14-RED-BETALET" propRedBetaLet,
    Obligation "O14-RED-REFLEX" propRedReflex,
    Obligation "O14-RED-TRANS" propRedTrans,
    Obligation "O14-RED-QUANT-INTRO" propRedQuantIntro,
    Obligation "O14-RED-QUANT-ELIM" propRedQuantElim,
    Obligation "O14-RED-INNER" propRedInner,
    Obligation "O14-RED-OUTER" propRedOuter,
    Obligation "O14-RED-CONTEXT" propRedContext,
    Obligation "O14-APPLY-N" propApplyN,
    Obligation "O14-APPLY-O" propApplyO,
    Obligation "O14-APPLY-SEQ" propApplySeq,
    Obligation "O14-APPLY-INNER" propApplyInner,
    Obligation "O14-APPLY-OUTER" propApplyOuter,
    Obligation "O14-APPLY-HYP" propApplyHyp,
    Obligation "O14-APPLY-BOT" propApplyBot,
    Obligation "O14-APPLY-ID" propApplyId,
    Obligation "O15-TRANS-NO-INERT-LOCKED" propTransNoInertLocked,
    Obligation "O15-TRANS-SCHEME-ROOT-RIGID" propTransSchemeRootRigid,
    Obligation "O15-TRANS-ARROW-RIGID" propTransArrowRigid,
    Obligation "O15-TRANS-NON-INTERIOR-RIGID" propTransNonInteriorRigid,
    Obligation "O15-REORDER-REQUIRED" propSigmaReorderRequired,
    Obligation "O15-REORDER-IDENTITY" propSigmaReorderIdentity,
    Obligation "O15-CONTEXT-FIND" propContextFind,
    Obligation "O15-CONTEXT-REJECT" propContextReject,
    Obligation "O15-EDGE-TRANSLATION" propEdgeTranslation,
    Obligation "O15-ELAB-LAMBDA-VAR" propElabLambdaVar,
    Obligation "O15-ELAB-LET-VAR" propElabLetVar,
    Obligation "O15-ELAB-ABS" propElabAbs,
    Obligation "O15-ELAB-APP" propElabApp,
    Obligation "O15-ELAB-LET" propElabLet,
    Obligation "O15-ENV-LAMBDA" propEnvLambda,
    Obligation "O15-ENV-LET" propEnvLet,
    Obligation "O15-ENV-WF" propEnvWf,
    Obligation "O15-TR-SEQ-EMPTY" propTrSeqEmpty,
    Obligation "O15-TR-SEQ-CONS" propTrSeqCons,
    Obligation "O15-TR-RIGID-RAISE" propTrRigidRaise,
    Obligation "O15-TR-RIGID-MERGE" propTrRigidMerge,
    Obligation "O15-TR-RIGID-RAISEMERGE" propTrRigidRaiseMerge,
    Obligation "O15-TR-ROOT-GRAFT" propTrRootGraft,
    Obligation "O15-TR-ROOT-RAISEMERGE" propTrRootRaiseMerge,
    Obligation "O15-TR-ROOT-WEAKEN" propTrRootWeaken,
    Obligation "O15-TR-NODE-GRAFT" propTrNodeGraft,
    Obligation "O15-TR-NODE-MERGE" propTrNodeMerge,
    Obligation "O15-TR-NODE-RAISEMERGE" propTrNodeRaiseMerge,
    Obligation "O15-TR-NODE-WEAKEN" propTrNodeWeaken,
    Obligation "O15-TR-NODE-RAISE" propTrNodeRaise,
    Obligation "O04-BIND-FLEX-CHILDREN" propBindingFlexChildren,
    Obligation "O04-BIND-INTERIOR" propBindingInterior,
    Obligation "O04-BIND-ORDER" propBindingOrder,
    Obligation "O04-OP-WEAKEN" propGraphWeaken,
    Obligation "O04-OP-RAISE-STEP" propGraphRaiseStep,
    Obligation "O04-OP-RAISE-TO" propGraphRaiseTo,
    Obligation "O05-INERT-NODES" propInertNodes,
    Obligation "O05-INERT-LOCKED" propInertLocked,
    Obligation "O05-WEAKEN-INERT" propInertWeaken,
    Obligation "O07-UNIF-CORE" propUnifyDecompose,
    Obligation "O07-UNIF-PRESOL" propPresolutionUnify,
    Obligation "O07-REBIND" propRebindHarmonize,
    Obligation "O07-GENUNIF" propGeneralizedUnify,
    Obligation "O08-REIFY-TYPE" propReifyType,
    Obligation "O08-REIFY-NAMES" propReifyNames,
    Obligation "O08-BIND-MONO" propBindMono,
    Obligation "O08-SYN-TO-GRAPH" propSynToGraph,
    Obligation "O08-REIFY-INLINE" propReifyInline,
    Obligation "O08-INLINE-PRED" propInlinePred,
    Obligation "O09-CGEN-ROOT" propCgenRoot,
    Obligation "O09-CGEN-EXPR" propCgenExpr,
    Obligation "O10-EXP-DECIDE" propExpDecide,
    Obligation "O10-EXP-APPLY" propExpApply,
    Obligation "O10-PROP-SOLVE" propPropSolve,
    Obligation "O10-PROP-WITNESS" propPropWitness,
    Obligation "O10-COPY-SCHEME" propCopyScheme,
    Obligation "O11-UNIFY-STRUCT" propUnifyDecompose,
    Obligation "O11-WITNESS-NORM" propWitnessNorm,
    Obligation "O11-WITNESS-COALESCE" propWitnessCoalesce,
    Obligation "O11-WITNESS-REORDER" propWitnessReorder,
    Obligation "O12-SOLVE-UNIFY" propSolveVar,
    Obligation "O12-ACYCLIC-CHECK" propAcyclicCheck,
    Obligation "O12-ACYCLIC-TOPO" propAcyclicTopo,
    Obligation "O12-COPY-INST" propCopyInst,
    Obligation "O12-NORM-GRAFT" propNormGraft,
    Obligation "O12-NORM-MERGE" propNormMerge,
    Obligation "O12-NORM-DROP" propNormDrop,
    Obligation "O12-NORM-FIXPOINT" propNormFixpoint,
    Obligation "O12-SOLVE-VAR-BASE" propSolveVarBase,
    Obligation "O12-SOLVE-VAR-VAR" propSolveVarVar,
    Obligation "O12-SOLVE-HARMONIZE" propSolveHarmonize,
    Obligation "O12-SOLVE-ARROW" propSolveArrow,
    Obligation "O12-SOLVE-VALIDATE" propSolveValidate
  ]

propBindingFlexChildren :: Int -> Property
propBindingFlexChildren _size =
  let c = binderConstraint
   in case Binding.boundFlexChildren c (typeRef (NodeId 0)) of
        Right children -> counterexample (show children) (NodeId 1 `elem` children)
        Left err -> counterexample (show err) False

propBindingInterior :: Int -> Property
propBindingInterior size =
  let c = chainConstraint size
   in case Binding.interiorOf c (typeRef (NodeId 0)) of
        Right interior ->
          conjoin
            [ counterexample (show interior) (IntSet.member (nodeRefKey (typeRef (NodeId 0))) interior),
              counterexample (show interior) (IntSet.member (nodeRefKey (typeRef (NodeId 1))) interior)
            ]
        Left err -> counterexample (show err) False

propBindingOrder :: Int -> Property
propBindingOrder size =
  let (c, root, expected) = orderedBinderFixture size
   in case Binding.orderedBinders id c (typeRef root) of
        Right binders -> counterexample (show binders) (binders === expected)
        Left err -> counterexample (show err) False

propGraphWeaken :: Int -> Property
propGraphWeaken size =
  let c = chainConstraint size
      nid = typeRef (NodeId (size - 1))
   in case GraphOps.applyWeaken nid c of
        Right (c', _) ->
          conjoin
            [ Binding.checkBindingTree c' === Right (),
              Binding.lookupBindParent c' nid === Just (typeRef (NodeId (size - 2)), BindRigid)
            ]
        Left err -> counterexample (show err) False

propGraphRaiseStep :: Int -> Property
propGraphRaiseStep size =
  let c = chainConstraint size
      nid = typeRef (NodeId (size - 1))
      grandparent = typeRef (NodeId (size - 3))
   in case GraphOps.applyRaiseStep nid c of
        Right (c', Just _) ->
          conjoin
            [ Binding.checkBindingTree c' === Right (),
              Binding.lookupBindParent c' nid === Just (grandparent, BindFlex)
            ]
        other -> counterexample (show other) False

propGraphRaiseTo :: Int -> Property
propGraphRaiseTo size =
  let c = chainConstraint size
      nid = typeRef (NodeId (size - 1))
      target = typeRef (NodeId 0)
   in case GraphOps.applyRaiseTo nid target c of
        Right (c', ops) ->
          conjoin
            [ counterexample (show ops) (not (null ops)),
              Binding.checkBindingTree c' === Right (),
              Binding.lookupBindParent c' nid === Just (target, BindFlex)
            ]
        Left err -> counterexample (show err) False

propInertNodes :: Int -> Property
propInertNodes size =
  let c = inertConstraint size
   in case Inert.inertNodes c of
        Right nodes ->
          conjoin
            [ counterexample (show nodes) (not (IntSet.null nodes)),
              counterexample (show nodes) (IntSet.member 2 nodes)
            ]
        Left err -> counterexample (show err) False

propInertLocked :: Int -> Property
propInertLocked size =
  let c = inertConstraint size
   in case Inert.inertLockedNodes c of
        Right nodes -> counterexample (show nodes) (IntSet.member 2 nodes)
        Left err -> counterexample (show err) False

propInertWeaken :: Int -> Property
propInertWeaken size =
  let c = inertConstraint size
   in case Inert.weakenInertLockedNodes c of
        Right c' -> Inert.inertLockedNodes c' === Right IntSet.empty
        Left err -> counterexample (show err) False

propUnifyDecompose :: Int -> Property
propUnifyDecompose size =
  let lhs = TyArrow (NodeId 0) (NodeId 1) (NodeId 2)
      rhs = TyArrow (NodeId 3) (NodeId (size + 10)) (NodeId (size + 11))
   in decomposeUnifyChildren lhs rhs
        === Right [UnifyEdge (NodeId 1) (NodeId (size + 10)), UnifyEdge (NodeId 2) (NodeId (size + 11))]

propSolveVar :: Int -> Property
propSolveVar _size =
  let c =
        varTripleConstraint
          { cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 3)]
          }
   in case solveUnify defaultTraceConfig c of
        Right SolveResult {srConstraint = solved, srUnionFind = uf} ->
          conjoin
            [ cUnifyEdges solved === [],
              frWith uf (NodeId 1) === frWith uf (NodeId 3),
              Binding.checkBindingTree solved === Right ()
            ]
        Left err -> counterexample (show err) False

propPresolutionUnify :: Int -> Property
propPresolutionUnify _size =
  let c = varTripleConstraint
      st0 = emptyPresolutionState c
   in case runPresolutionM defaultTraceConfig st0 (unifyAcyclic (NodeId 1) (NodeId 3)) of
        Right ((), st1) ->
          let uf = psUnionFind st1
              solved = psConstraint st1
           in conjoin
                [ frWith uf (NodeId 1) === frWith uf (NodeId 3),
                  Binding.checkBindingTree solved === Right ()
                ]
        Left err -> counterexample (show err) False

propSolveArrow :: Int -> Property
propSolveArrow _size =
  let c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (0, TyCon (NodeId 0) (BaseTy "Pair") (NodeId 1 :| [NodeId 4])),
                    (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)),
                    (2, TyBase (NodeId 2) (BaseTy "Int")),
                    (3, TyBase (NodeId 3) (BaseTy "Bool")),
                    (4, TyArrow (NodeId 4) (NodeId 5) (NodeId 6)),
                    (5, TyBase (NodeId 5) (BaseTy "Int")),
                    (6, TyBase (NodeId 6) (BaseTy "Bool"))
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (NodeId 1, NodeId 0, BindFlex),
                    (NodeId 2, NodeId 1, BindFlex),
                    (NodeId 3, NodeId 1, BindFlex),
                    (NodeId 4, NodeId 0, BindFlex),
                    (NodeId 5, NodeId 4, BindFlex),
                    (NodeId 6, NodeId 4, BindFlex)
                  ],
              cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 4)]
            }
   in case solveUnify defaultTraceConfig c of
        Right SolveResult {srConstraint = solved} ->
          conjoin
            [ cUnifyEdges solved === [],
              Binding.checkBindingTree solved === Right ()
            ]
        Left err -> counterexample (show err) False

propRebindHarmonize :: Int -> Property
propRebindHarmonize size =
  let c = chainConstraint size
      left = typeRef (NodeId (size - 2))
      right = typeRef (NodeId (size - 1))
   in case Binding.bindingLCA c left right of
        Right lca ->
          conjoin
            [ lca === left,
              Binding.checkBindingTree c === Right ()
            ]
        other -> counterexample (show other) False

propGeneralizedUnify :: Int -> Property
propGeneralizedUnify _size =
  let c =
        varTripleConstraint
          { cUnifyEdges =
              [ UnifyEdge (NodeId 1) (NodeId 2),
                UnifyEdge (NodeId 2) (NodeId 3)
              ]
          }
   in case solveUnify defaultTraceConfig c of
        Right SolveResult {srConstraint = solved, srUnionFind = uf} ->
          conjoin
            [ cUnifyEdges solved === [],
              frWith uf (NodeId 1) === frWith uf (NodeId 2),
              frWith uf (NodeId 2) === frWith uf (NodeId 3),
              Binding.checkBindingTree solved === Right ()
            ]
        Left err -> counterexample (show err) False

propWfEmpty :: Int -> Property
propWfEmpty _size =
  Elab.typeCheck (Elab.ELit (Surf.LInt 0)) === Right intTy

propWfTVar :: Int -> Property
propWfTVar _size =
  Elab.typeCheck polyId === Right polyIdTy

propWfVar :: Int -> Property
propWfVar _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propInstReflex :: Int -> Property
propInstReflex _size =
  applyShouldBe intTy Elab.InstId intTy

propInstTrans :: Int -> Property
propInstTrans _size =
  applyShouldBe intTy (Elab.InstSeq Elab.InstIntro Elab.InstElim) intTy

propInstBot :: Int -> Property
propInstBot _size =
  applyShouldBe Elab.TBottom (Elab.InstBot intTy) intTy

propInstHyp :: Int -> Property
propInstHyp _size =
  applyShouldBe Elab.TBottom (Elab.InstAbstr "a") (Elab.TVar "a")

propInstInner :: Int -> Property
propInstInner _size =
  applyShouldBe forallA (Elab.InstInside (Elab.InstBot intTy)) (Elab.TForall "a" (Just (boundFromType intTy)) (Elab.TVar "a"))

propInstOuter :: Int -> Property
propInstOuter _size =
  applyShouldBe (Elab.TForall "a" Nothing (Elab.TVar "z")) (Elab.InstUnder "x" (Elab.InstAbstr "x")) (Elab.TForall "a" Nothing (Elab.TVar "a"))

propInstQuantElim :: Int -> Property
propInstQuantElim _size =
  applyShouldBe forallA Elab.InstElim Elab.TBottom

propInstQuantIntro :: Int -> Property
propInstQuantIntro _size =
  case Elab.applyInstantiation intTy Elab.InstIntro of
    Right (Elab.TForall _ Nothing body) -> body === intTy
    other -> counterexample (show other) False

propTypingVar :: Int -> Property
propTypingVar _size =
  let env = Elab.Env {Elab.termEnv = Map.fromList [("x", intTy)], Elab.typeEnv = Map.empty}
   in Elab.typeCheckWithEnv env (Elab.EVar "x") === Right intTy

propTypingAbs :: Int -> Property
propTypingAbs _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propTypingApp :: Int -> Property
propTypingApp _size =
  Elab.typeCheck (Elab.EApp idLam (Elab.ELit (Surf.LInt 1))) === Right intTy

propTypingTAbs :: Int -> Property
propTypingTAbs _size =
  Elab.typeCheck polyId === Right polyIdTy

propTypingTApp :: Int -> Property
propTypingTApp _size =
  Elab.typeCheck (Elab.ETyInst polyId (Elab.InstApp intTy)) === Right (Elab.TArrow intTy intTy)

propTypingLet :: Int -> Property
propTypingLet _size =
  Elab.typeCheck (Elab.ELet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (Elab.EVar "x")) === Right intTy

propRedBeta :: Int -> Property
propRedBeta _size =
  Elab.step (Elab.EApp idLam (Elab.ELit (Surf.LInt 1))) === Just (Elab.ELit (Surf.LInt 1))

propRedBetaLet :: Int -> Property
propRedBetaLet _size =
  Elab.step (Elab.ELet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (Elab.EVar "x")) === Just (Elab.ELit (Surf.LInt 1))

propRedReflex :: Int -> Property
propRedReflex _size =
  Elab.step (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstId) === Just (Elab.ELit (Surf.LInt 1))

propRedTrans :: Int -> Property
propRedTrans _size =
  let term = Elab.ETyInst (Elab.ELit (Surf.LInt 1)) (Elab.InstSeq Elab.InstIntro Elab.InstElim)
   in Elab.step term === Just (Elab.ETyInst (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro) Elab.InstElim)

propRedQuantIntro :: Int -> Property
propRedQuantIntro _size =
  Elab.step (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro) === Just (Elab.ETyAbs "u0" Nothing (Elab.ELit (Surf.LInt 1)))

propRedQuantElim :: Int -> Property
propRedQuantElim _size =
  Elab.step (Elab.ETyInst polyId Elab.InstElim) === Just (Elab.ELam "x" Elab.TBottom (Elab.EVar "x"))

propRedInner :: Int -> Property
propRedInner _size =
  let term = Elab.ETyInst (Elab.ETyAbs "a" Nothing (Elab.EVar "x")) (Elab.InstInside (Elab.InstBot intTy))
   in Elab.step term === Just (Elab.ETyAbs "a" (Just (boundFromType intTy)) (Elab.EVar "x"))

propRedOuter :: Int -> Property
propRedOuter _size =
  let body = Elab.ELam "x" (Elab.TVar "a") (Elab.EVar "x")
      term = Elab.ETyInst (Elab.ETyAbs "a" Nothing body) (Elab.InstUnder "b" (Elab.InstApp intTy))
   in Elab.step term === Just (Elab.ETyAbs "a" Nothing (Elab.ETyInst body (Elab.InstApp intTy)))

propRedContext :: Int -> Property
propRedContext _size =
  let arg = Elab.EApp (Elab.ELam "y" intTy (Elab.EVar "y")) (Elab.ELit (Surf.LInt 1))
   in Elab.step (Elab.EApp idLam arg) === Just (Elab.EApp idLam (Elab.ELit (Surf.LInt 1)))

propApplyN :: Int -> Property
propApplyN _size =
  applyShouldBe forallA Elab.InstElim Elab.TBottom

propApplyO :: Int -> Property
propApplyO _size =
  case Elab.applyInstantiation intTy Elab.InstIntro of
    Right (Elab.TForall _ Nothing body) -> body === intTy
    other -> counterexample (show other) False

propApplySeq :: Int -> Property
propApplySeq _size =
  applyShouldBe forallA (Elab.InstApp intTy) intTy

propApplyInner :: Int -> Property
propApplyInner _size =
  propInstInner 0

propApplyOuter :: Int -> Property
propApplyOuter _size =
  propInstOuter 0

propApplyHyp :: Int -> Property
propApplyHyp _size =
  applyShouldBe Elab.TBottom (Elab.InstAbstr "a") (Elab.TVar "a")

propApplyBot :: Int -> Property
propApplyBot _size =
  applyShouldBe Elab.TBottom (Elab.InstBot intTy) intTy

propApplyId :: Int -> Property
propApplyId _size =
  applyShouldBe (Elab.TArrow intTy boolTy) Elab.InstId (Elab.TArrow intTy boolTy)

propTransNoInertLocked :: Int -> Property
propTransNoInertLocked size =
  let c = inertConstraint size
   in case validateTranslatablePresolution c of
        Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("InertLockedNodes" `isInfixOf` show issues)
        other -> counterexample (show other) False

propTransSchemeRootRigid :: Int -> Property
propTransSchemeRootRigid _size =
  case validateTranslatablePresolution flexibleSchemeRootConstraint of
    Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("SchemeRootNotRigid" `isInfixOf` show issues)
    other -> counterexample (show other) False

propTransArrowRigid :: Int -> Property
propTransArrowRigid _size =
  case validateTranslatablePresolution flexibleArrowConstraint of
    Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("ArrowNodeNotRigid" `isInfixOf` show issues)
    other -> counterexample (show other) False

propTransNonInteriorRigid :: Int -> Property
propTransNonInteriorRigid _size =
  case validateTranslatablePresolution flexibleNonInteriorConstraint of
    Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("NonInteriorNodeNotRigid" `isInfixOf` show issues)
    other -> counterexample (show other) False

propSigmaReorderRequired :: Int -> Property
propSigmaReorderRequired _size =
  let body = Elab.TArrow (Elab.TVar "a") (Elab.TVar "b")
      src = Elab.TForall "a" Nothing (Elab.TForall "b" Nothing body)
      tgt = Elab.TForall "b" Nothing (Elab.TForall "a" Nothing body)
   in case Elab.sigmaReorder src tgt of
        Right inst ->
          conjoin
            [ counterexample (show inst) (inst /= Elab.InstId),
              counterexample (show inst) (isRight (Elab.applyInstantiation src inst))
            ]
        Left err -> counterexample (show err) False

propSigmaReorderIdentity :: Int -> Property
propSigmaReorderIdentity _size =
  let src = Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") intTy)
   in Elab.sigmaReorder src src === Right Elab.InstId

propContextFind :: Int -> Property
propContextFind size =
  let (c, root, target, expected) = contextFindFixture size
   in case Elab.contextToNodeBound (identityPresolutionView c) root target of
        Right steps -> steps === Just expected
        Left err -> counterexample (show err) False

propContextReject :: Int -> Property
propContextReject size =
  let (c, root, target) = contextRejectFixture size
   in case Elab.contextToNodeBound (identityPresolutionView c) root target of
        Right steps -> steps === Nothing
        Left err -> counterexample (show err) False

propEdgeTranslation :: Int -> Property
propEdgeTranslation _size =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr letIdAppExpr) of
    Right (term, ty) ->
      conjoin
        [ ty === intTy,
          Elab.typeCheck term === Right intTy
        ]
    Left err -> counterexample (Elab.renderPipelineError err) False

propElabLambdaVar :: Int -> Property
propElabLambdaVar _size =
  elaboratesTo (Surf.ELam "x" (Surf.EVar "x")) polyIdTy

propElabLetVar :: Int -> Property
propElabLetVar _size =
  elaboratesTo (Surf.ELet "x" (Surf.ELit (Surf.LInt 1)) (Surf.EVar "x")) intTy

propElabAbs :: Int -> Property
propElabAbs _size =
  case Elab.runPipelineElabChecked Set.empty (unsafeNormalizeExpr (Surf.ELam "x" (Surf.EVar "x"))) of
    Right (Elab.ETyAbs {}, ty) -> ty === polyIdTy
    other -> counterexample (show other) False

propElabApp :: Int -> Property
propElabApp _size =
  elaboratesTo (Surf.EApp (Surf.ELam "x" (Surf.EVar "x")) (Surf.ELit (Surf.LInt 1))) intTy

propElabLet :: Int -> Property
propElabLet _size =
  elaboratesTo letIdAppExpr intTy

propEnvLambda :: Int -> Property
propEnvLambda _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propEnvLet :: Int -> Property
propEnvLet _size =
  Elab.typeCheck (Elab.ELet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (Elab.EVar "x")) === Right intTy

propEnvWf :: Int -> Property
propEnvWf _size =
  conjoin [propEnvLambda 0, propEnvLet 0]

propTrSeqEmpty :: Int -> Property
propTrSeqEmpty _size =
  propSigmaReorderIdentity 0

propTrSeqCons :: Int -> Property
propTrSeqCons _size =
  propSigmaReorderRequired 0

propTrRigidRaise :: Int -> Property
propTrRigidRaise _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsFull env [OpRaise (NodeId 2)] === Right []

propTrRigidMerge :: Int -> Property
propTrRigidMerge _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsFull env [OpMerge (NodeId 2) (NodeId 3)] === Right []

propTrRigidRaiseMerge :: Int -> Property
propTrRigidRaiseMerge _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsFull env [OpRaiseMerge (NodeId 2) (NodeId 3)] === Right []

propTrRootGraft :: Int -> Property
propTrRootGraft _size =
  let root = NodeId 0
      arg = NodeId 1
      c =
        rootedConstraint
          emptyConstraint
            { cNodes = nodeMapFromList [(0, TyArrow root arg arg), (1, TyVar {tnId = arg, tnBound = Nothing})],
              cBindParents = bindParentsFromPairs [(arg, root, BindFlex)]
            }
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId arg])
   in validateNormalizedWitness env [OpGraft arg root] === Right ()

propTrRootRaiseMerge :: Int -> Property
propTrRootRaiseMerge _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      m = NodeId 3
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in coalesceRaiseMergeWithEnv env [OpRaise n, OpMerge n m] === Right [OpRaiseMerge n m]

propTrRootWeaken :: Int -> Property
propTrRootWeaken _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in normalizeInstanceOpsFull env [OpWeaken n] === Right [OpWeaken n]

propTrNodeGraft :: Int -> Property
propTrNodeGraft _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      binder = NodeId 2
      arg = NodeId 3
      env =
        (mkNormalizeEnv c root (IntSet.fromList [getNodeId binder]))
          { binderArgs = IntMap.fromList [(getNodeId binder, arg)],
            binderReplayMap = IntMap.fromList [(getNodeId binder, binder)],
            replayContract = ReplayContractStrict
          }
   in normalizeInstanceOpsFull env [OpGraft arg binder, OpWeaken binder] === Right [OpGraft arg binder, OpWeaken binder]

propTrNodeMerge :: Int -> Property
propTrNodeMerge size =
  assertNodeAliasTranslation size OpMerge

propTrNodeRaiseMerge :: Int -> Property
propTrNodeRaiseMerge size =
  assertNodeAliasTranslation size OpRaiseMerge

propTrNodeWeaken :: Int -> Property
propTrNodeWeaken _size =
  let root = NodeId 0
      parent = NodeId 1
      child = NodeId 2
      sibling = NodeId 3
      nodes =
        nodeMapFromList
          [ (0, TyForall root parent),
            (1, TyForall parent child),
            (2, TyVar {tnId = child, tnBound = Nothing}),
            (3, TyVar {tnId = sibling, tnBound = Nothing})
          ]
      c =
        rootedConstraint
          emptyConstraint
            { cNodes = nodes,
              cBindParents =
                bindParentsFromPairs
                  [ (parent, root, BindFlex),
                    (child, parent, BindFlex),
                    (sibling, root, BindFlex)
                  ]
            }
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child, getNodeId sibling])
   in reorderWeakenWithEnv env [OpWeaken parent, OpGraft child child] === Right [OpGraft child child, OpWeaken parent]

propTrNodeRaise :: Int -> Property
propTrNodeRaise _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in validateNormalizedWitness env [OpRaise n] === Right ()

propReifyType :: Int -> Property
propReifyType _size =
  elaboratesTo (Surf.ELit (Surf.LInt 1)) intTy

propReifyNames :: Int -> Property
propReifyNames _size =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr (Surf.ELam "x" (Surf.EVar "x"))) of
    Right (_term, Elab.TForall _ Nothing (Elab.TArrow dom cod)) -> counterexample (show (dom, cod)) (dom == cod)
    other -> counterexample (show other) False

propBindMono :: Int -> Property
propBindMono _size =
  case runPipelineArtifactsDefault Set.empty (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) of
    Right PipelineArtifacts {paPresolution = PresolutionResult {prConstraint = c}} ->
      Binding.checkBindingTree c === Right ()
    Left err -> counterexample err False

propSynToGraph :: Int -> Property
propSynToGraph _size =
  case runConstraintDefault Set.empty (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) of
    Right ConstraintResult {crConstraint = c, crRoot = root} ->
      conjoin [counterexample (show root) (isJust (lookupNodeIn (cNodes c) root)), Binding.checkBindingTree c === Right ()]
    Left err -> counterexample err False

propReifyInline :: Int -> Property
propReifyInline _size =
  elaboratesTo (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) intTy

propInlinePred :: Int -> Property
propInlinePred _size =
  let inlineable :: Elab.ElabType
      inlineable =
        Elab.TForall
          "a"
          (Just (boundFromType intTy))
          (Elab.TArrow (Elab.TVar "a") boolTy)
      inlined :: Elab.ElabType
      inlined = Elab.TArrow intTy boolTy
      selfBound :: Elab.ElabType
      selfBound =
        Elab.TForall
          "a"
          (Just (Elab.TArrow (Elab.TVar "a") intTy))
          (Elab.TArrow (Elab.TVar "a") boolTy)
   in conjoin
        [ counterexample (Elab.prettyDisplay inlineable) $
            Elab.prettyDisplay inlineable == Elab.prettyDisplay inlined,
          counterexample (Elab.prettyDisplay inlineable) $
            Elab.prettyDisplay inlineable /= Elab.pretty inlineable,
          counterexample (Elab.prettyDisplay selfBound) $
            Elab.prettyDisplay selfBound == Elab.pretty selfBound
        ]

propCgenRoot :: Int -> Property
propCgenRoot _size =
  case runConstraintDefault Set.empty (Surf.ELit (Surf.LInt 1)) of
    Right ConstraintResult {crConstraint = c, crRoot = root} ->
      case lookupNodeIn (cNodes c) root of
        Just TyVar {tnBound = Just bound} ->
          conjoin
            [ lookupNodeIn (cNodes c) bound === Just (TyBase bound (BaseTy "Int")),
              Binding.checkBindingTree c === Right ()
            ]
        other -> counterexample (show other) False
    Left err -> counterexample err False

propCgenExpr :: Int -> Property
propCgenExpr _size =
  case runConstraintDefault Set.empty (Surf.EApp (Surf.ELam "x" (Surf.EVar "x")) (Surf.ELit (Surf.LInt 1))) of
    Right ConstraintResult {crConstraint = c} ->
      conjoin [counterexample (show (cInstEdges c)) (not (null (cInstEdges c))), Binding.checkBindingTree c === Right ()]
    Left err -> counterexample err False

propExpDecide :: Int -> Property
propExpDecide size =
  conjoin
    [ assertMinimalDecision "identity" cId expId targetId $ \(expansion, unifications) ->
        conjoin
          [ expansion === ExpIdentity,
            unifications === [(bodyId, targetId)]
          ],
      assertMinimalDecision "instantiate" cInst expInst targetArrow $ \(expansion, unifications) ->
        case expansion of
          ExpInstantiate args ->
            conjoin
              [ counterexample (show args) (length args === 1),
                unifications === []
              ]
          other -> counterexample (show other) False,
      assertMinimalDecision "compose" cCompose expCompose targetForall2 $ \(expansion, unifications) ->
        case expansion of
          ExpCompose (ExpInstantiate args :| [ExpForall (ForallSpec 2 [Nothing, Nothing] :| [])]) ->
            conjoin
              [ counterexample (show args) (length args === 1),
                unifications === []
              ]
          other -> counterexample (show other) False,
      assertMinimalDecision "forall-intro" cForallIntro expForallIntro targetForallIntro $ \(expansion, unifications) ->
        conjoin
          [ expansion === ExpForall (ForallSpec 2 [Nothing, Nothing] :| []),
            unifications === []
          ]
    ]
  where
    base = max 3 size * 20
    bodyId = NodeId (base + 1)
    targetId = NodeId (base + 2)
    expId = NodeId (base + 3)
    cId =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId bodyId, TyBase bodyId (BaseTy "Int")),
                  (getNodeId targetId, TyBase targetId (BaseTy "Int")),
                  (getNodeId expId, TyExp expId (ExpVarId base) bodyId)
                ],
            cBindParents = bindParentsFromPairs [(bodyId, expId, BindFlex)]
          }

    srcVar = NodeId (base + 10)
    srcArrow = NodeId (base + 11)
    srcForall = NodeId (base + 12)
    targetDom = NodeId (base + 13)
    targetCod = NodeId (base + 14)
    targetArrow = NodeId (base + 15)
    expInst = NodeId (base + 16)
    cInst =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId srcVar, TyVar {tnId = srcVar, tnBound = Nothing}),
                  (getNodeId srcArrow, TyArrow srcArrow srcVar srcVar),
                  (getNodeId srcForall, TyForall srcForall srcArrow),
                  (getNodeId targetDom, TyBase targetDom (BaseTy "Int")),
                  (getNodeId targetCod, TyBase targetCod (BaseTy "Int")),
                  (getNodeId targetArrow, TyArrow targetArrow targetDom targetCod),
                  (getNodeId expInst, TyExp expInst (ExpVarId (base + 1)) srcForall)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (srcVar, srcForall, BindFlex),
                  (srcArrow, srcForall, BindFlex),
                  (targetDom, targetArrow, BindFlex),
                  (targetCod, targetArrow, BindFlex),
                  (srcForall, expInst, BindFlex)
                ]
          }

    srcVarC = NodeId (base + 20)
    srcForallC = NodeId (base + 21)
    targetDomC = NodeId (base + 22)
    targetCodC = NodeId (base + 23)
    targetArrowC = NodeId (base + 24)
    targetForall2 = NodeId (base + 25)
    expCompose = NodeId (base + 26)
    cCompose =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId srcVarC, TyVar {tnId = srcVarC, tnBound = Nothing}),
                  (getNodeId srcForallC, TyForall srcForallC srcVarC),
                  (getNodeId targetDomC, TyVar {tnId = targetDomC, tnBound = Nothing}),
                  (getNodeId targetCodC, TyVar {tnId = targetCodC, tnBound = Nothing}),
                  (getNodeId targetArrowC, TyArrow targetArrowC targetDomC targetCodC),
                  (getNodeId targetForall2, TyForall targetForall2 targetArrowC),
                  (getNodeId expCompose, TyExp expCompose (ExpVarId (base + 2)) srcForallC)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (srcVarC, srcForallC, BindFlex),
                  (srcForallC, expCompose, BindFlex),
                  (targetDomC, targetForall2, BindFlex),
                  (targetCodC, targetForall2, BindFlex),
                  (targetArrowC, targetForall2, BindFlex)
                ]
          }

    srcDomF = NodeId (base + 30)
    srcCodF = NodeId (base + 31)
    srcArrowF = NodeId (base + 32)
    targetDomF = NodeId (base + 33)
    targetCodF = NodeId (base + 34)
    targetArrowF = NodeId (base + 35)
    targetForallIntro = NodeId (base + 36)
    expForallIntro = NodeId (base + 37)
    cForallIntro =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId srcDomF, TyBase srcDomF (BaseTy "Int")),
                  (getNodeId srcCodF, TyBase srcCodF (BaseTy "Bool")),
                  (getNodeId srcArrowF, TyArrow srcArrowF srcDomF srcCodF),
                  (getNodeId targetDomF, TyVar {tnId = targetDomF, tnBound = Nothing}),
                  (getNodeId targetCodF, TyVar {tnId = targetCodF, tnBound = Nothing}),
                  (getNodeId targetArrowF, TyArrow targetArrowF targetDomF targetCodF),
                  (getNodeId targetForallIntro, TyForall targetForallIntro targetArrowF),
                  (getNodeId expForallIntro, TyExp expForallIntro (ExpVarId (base + 3)) srcArrowF)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (srcDomF, srcArrowF, BindFlex),
                  (srcCodF, srcArrowF, BindFlex),
                  (srcArrowF, expForallIntro, BindFlex),
                  (targetDomF, targetForallIntro, BindFlex),
                  (targetCodF, targetForallIntro, BindFlex),
                  (targetArrowF, targetForallIntro, BindFlex)
                ]
          }

propExpApply :: Int -> Property
propExpApply _size =
  propEdgeWitnessOps letIdAppExpr (not . null)

propPropSolve :: Int -> Property
propPropSolve _size =
  propPresolutionClearsEdges letIdAppExpr

propPropWitness :: Int -> Property
propPropWitness _size =
  case runToPresolutionDefault Set.empty letIdAppExpr of
    Right PresolutionResult {prConstraint = c, prEdgeWitnesses = witnesses} ->
      let entries = IntMap.toList witnesses
       in conjoin
            [ counterexample (show entries) (not (null entries)),
              counterexample (show entries) $
                all
                  ( \(edgeKey, edgeWitness) ->
                      getEdgeId (ewEdgeId edgeWitness) == edgeKey
                        && isJust (lookupNodeIn (cNodes c) (ewRoot edgeWitness))
                  )
                  entries
            ]
    Left err -> counterexample err False

propCopyScheme :: Int -> Property
propCopyScheme _size =
  case runToPresolutionDefault Set.empty letIdAppExpr of
    Right PresolutionResult {prEdgeTraces = traces} -> counterexample (show (IntMap.size traces)) (not (IntMap.null traces))
    Left err -> counterexample err False

propWitnessNorm :: Int -> Property
propWitnessNorm _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      m = NodeId 3
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in normalizeInstanceOpsFull env [OpRaise n, OpMerge n m] === Right [OpRaiseMerge n m]

propWitnessCoalesce :: Int -> Property
propWitnessCoalesce _size =
  propTrRootRaiseMerge 0

propWitnessReorder :: Int -> Property
propWitnessReorder _size =
  propTrNodeWeaken 0

propAcyclicCheck :: Int -> Property
propAcyclicCheck size =
  let c = acyclicConstraint size
   in case checkAcyclicity c of
        Right result -> counterexample (show result) (not (null (arSortedEdges result)))
        Left err -> counterexample (show err) False

propAcyclicTopo :: Int -> Property
propAcyclicTopo size =
  let c = acyclicConstraint size
   in case checkAcyclicity c of
        Right result -> arSortedEdges result === [InstEdge (EdgeId size) (NodeId 0) (NodeId 2)]
        Left err -> counterexample (show err) False

propCopyInst :: Int -> Property
propCopyInst _size =
  propCopyScheme 0

propNormGraft :: Int -> Property
propNormGraft size =
  let graftBase = BaseTy ("Graft" ++ show size)
      c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                    (1, TyBase (NodeId 1) graftBase)
                  ],
              cBindParents = bindParentsFromPairs [(NodeId 1, NodeId 0, BindFlex)],
              cInstEdges = [InstEdge (EdgeId size) (NodeId 0) (NodeId 1)]
            }
      normalized = normalize c
   in conjoin
        [ cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TyBase (NodeId 0) graftBase),
          Binding.checkBindingTree normalized === Right ()
        ]

propNormMerge :: Int -> Property
propNormMerge size =
  let mergeBase = BaseTy ("Merge" ++ show size)
      c =
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyBase (NodeId 1) mergeBase)
                ],
            cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
          }
      normalized = normalize c
   in conjoin
        [ cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TyBase (NodeId 0) mergeBase)
        ]

propNormDrop :: Int -> Property
propNormDrop size =
  let node = TyVar {tnId = NodeId 0, tnBound = Nothing}
      edge = InstEdge (EdgeId size) (NodeId 0) (NodeId 0)
      c =
        emptyConstraint
          { cNodes = nodeMapFromList [(0, node)],
            cInstEdges = [edge]
          }
      normalized = normalize c
   in conjoin
        [ cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just node
        ]

propNormFixpoint :: Int -> Property
propNormFixpoint size =
  let fixpointBase = BaseTy ("Fixpoint" ++ show size)
      c =
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
                  (2, TyBase (NodeId 2) fixpointBase)
                ],
            cInstEdges =
              [ InstEdge (EdgeId size) (NodeId 0) (NodeId 1),
                InstEdge (EdgeId (size + 1)) (NodeId 1) (NodeId 2)
              ]
          }
      normalized = normalize c
   in conjoin
        [ normalized === normalize normalized,
          cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TyBase (NodeId 0) fixpointBase),
          lookupNodeIn (cNodes normalized) (NodeId 1) === Just (TyBase (NodeId 1) fixpointBase)
        ]

propSolveVarBase :: Int -> Property
propSolveVarBase _size =
  let c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes = nodeMapFromList [(0, TyCon (NodeId 0) (BaseTy "Box") (NodeId 1 :| [])), (1, TyVar (NodeId 1) Nothing), (2, TyBase (NodeId 2) (BaseTy "Int"))],
              cBindParents = bindParentsFromPairs [(NodeId 1, NodeId 0, BindFlex), (NodeId 2, NodeId 0, BindFlex)],
              cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 2)]
            }
   in case solveUnify defaultTraceConfig c of
        Right SolveResult {srConstraint = solved, srUnionFind = uf} ->
          conjoin [cUnifyEdges solved === [], frWith uf (NodeId 1) === frWith uf (NodeId 2)]
        Left err -> counterexample (show err) False

propSolveVarVar :: Int -> Property
propSolveVarVar _size =
  propSolveVar 0

propSolveHarmonize :: Int -> Property
propSolveHarmonize _size =
  propGeneralizedUnify 0

propSolveValidate :: Int -> Property
propSolveValidate _size =
  case solveUnify defaultTraceConfig varTripleConstraint of
    Right SolveResult {srConstraint = solved} -> Binding.checkBindingTree solved === Right ()
    Left err -> counterexample (show err) False

applyShouldBe :: Elab.ElabType -> Elab.Instantiation -> Elab.ElabType -> Property
applyShouldBe ty inst expected =
  case Elab.applyInstantiation ty inst of
    Right actual -> actual === expected
    Left err -> counterexample (show err) False

elaboratesTo :: Surf.SurfaceExpr -> Elab.ElabType -> Property
elaboratesTo expr expected =
  case Elab.runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
    Right (term, ty) ->
      conjoin
        [ ty === expected,
          Elab.typeCheck term === Right expected
        ]
    Left err -> counterexample (Elab.renderPipelineError err) False

propPresolutionClearsEdges :: Surf.SurfaceExpr -> Property
propPresolutionClearsEdges expr =
  case runToPresolutionDefault Set.empty expr of
    Right PresolutionResult {prConstraint = c} ->
      conjoin
        [ Binding.checkBindingTree c === Right (),
          cInstEdges c === []
        ]
    Left err -> counterexample err False

propEdgeWitnessOps :: Surf.SurfaceExpr -> ([EdgeWitness] -> Bool) -> Property
propEdgeWitnessOps expr predicate =
  case runToPresolutionDefault Set.empty expr of
    Right PresolutionResult {prEdgeWitnesses = witnesses} ->
      let values = IntMap.elems witnesses
       in counterexample (show values) (predicate values)
    Left err -> counterexample err False

acyclicConstraint :: Int -> Constraint
acyclicConstraint size =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TyBase (NodeId 2) (BaseTy "Int"))
            ],
        cInstEdges = [InstEdge (EdgeId size) (NodeId 0) (NodeId 2)]
      }

flexibleSchemeRootConstraint :: Constraint
flexibleSchemeRootConstraint =
  let rootGen = GenNodeId 0
      schemeRoot = NodeId 0
   in rootedConstraint
        emptyConstraint
          { cNodes = nodeMapFromList [(0, TyVar {tnId = schemeRoot, tnBound = Nothing})],
            cBindParents = IntMap.fromList [(nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))],
            cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
          }

flexibleArrowConstraint :: Constraint
flexibleArrowConstraint =
  let rootGen = GenNodeId 0
      dom = NodeId 0
      cod = NodeId 1
      arr = NodeId 2
   in rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (0, TyVar {tnId = dom, tnBound = Nothing}),
                  (1, TyVar {tnId = cod, tnBound = Nothing}),
                  (2, TyArrow arr dom cod)
                ],
            cBindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef arr), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef dom), (typeRef arr, BindFlex)),
                  (nodeRefKey (typeRef cod), (typeRef arr, BindFlex))
                ],
            cGenNodes = fromListGen [(rootGen, GenNode rootGen [arr])]
          }

flexibleNonInteriorConstraint :: Constraint
flexibleNonInteriorConstraint =
  let rootGen = GenNodeId 0
      schemeRoot = NodeId 0
      dom = NodeId 1
      cod = NodeId 2
      arrow = NodeId 3
      outside = NodeId 4
   in emptyConstraint
        { cNodes =
            nodeMapFromList
              [ (getNodeId schemeRoot, TyVar {tnId = schemeRoot, tnBound = Just arrow}),
                (getNodeId dom, TyVar {tnId = dom, tnBound = Nothing}),
                (getNodeId cod, TyVar {tnId = cod, tnBound = Nothing}),
                (getNodeId arrow, TyArrow arrow dom cod),
                (getNodeId outside, TyVar {tnId = outside, tnBound = Nothing})
              ],
          cBindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindRigid)),
                (nodeRefKey (typeRef arrow), (typeRef schemeRoot, BindRigid)),
                (nodeRefKey (typeRef dom), (typeRef arrow, BindFlex)),
                (nodeRefKey (typeRef cod), (typeRef arrow, BindFlex)),
                (nodeRefKey (typeRef outside), (genRef rootGen, BindFlex))
              ],
          cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
        }

forallA :: Elab.ElabType
forallA = Elab.TForall "a" Nothing (Elab.TVar "a")

polyIdTy :: Elab.ElabType
polyIdTy = Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))

letIdAppExpr :: Surf.SurfaceExpr
letIdAppExpr =
  Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x")) (Surf.EApp (Surf.EVar "id") (Surf.ELit (Surf.LInt 1)))

boundFromType :: Elab.ElabType -> Elab.BoundType
boundFromType ty =
  case ty of
    Elab.TVar v -> error ("boundFromType: unexpected variable bound " ++ show v)
    Elab.TArrow a b -> Elab.TArrow a b
    Elab.TCon c args -> Elab.TCon c args
    Elab.TBase b -> Elab.TBase b
    Elab.TBottom -> Elab.TBottom
    Elab.TForall v mb body -> Elab.TForall v mb body
    Elab.TMu v body -> Elab.TMu v body

emptyPresolutionState :: Constraint -> PresolutionState
emptyPresolutionState c =
  PresolutionState
    c
    (Presolution IntMap.empty)
    IntMap.empty
    (maxNodeIdKeyOr0 c + 1)
    IntSet.empty
    IntMap.empty
    IntMap.empty
    IntMap.empty
    IntMap.empty
    IntMap.empty

identityPresolutionView :: Constraint -> PresolutionView
identityPresolutionView c =
  PresolutionView
    { pvConstraint = c,
      pvCanonicalMap = IntMap.empty,
      pvCanonical = id,
      pvLookupNode = \nid -> lookupNodeIn (cNodes c) nid,
      pvLookupVarBound =
        \nid -> case lookupNodeIn (cNodes c) nid of
          Just TyVar {tnBound = mbBound} -> mbBound
          _ -> Nothing,
      pvLookupBindParent = Binding.lookupBindParent c,
      pvBindParents = cBindParents c,
      pvCanonicalConstraint = c
    }

assertMinimalDecision ::
  String ->
  Constraint ->
  NodeId ->
  NodeId ->
  ((Expansion, [(NodeId, NodeId)]) -> Property) ->
  Property
assertMinimalDecision caseName c expNodeId targetNodeId checkDecision =
  case decideMinimalFor c expNodeId targetNodeId of
    Right decision -> counterexample (caseName ++ ": " ++ show decision) (checkDecision decision)
    Left err -> counterexample (caseName ++ ": " ++ err) False

decideMinimalFor :: Constraint -> NodeId -> NodeId -> Either String (Expansion, [(NodeId, NodeId)])
decideMinimalFor c expNodeId targetNodeId =
  case (lookupNodeIn (cNodes c) expNodeId, lookupNodeIn (cNodes c) targetNodeId) of
    (Just expNode, Just targetNode) ->
      case runPresolutionM defaultTraceConfig (emptyPresolutionState c) (decideMinimalExpansion (GenNodeId 0) True expNode targetNode) of
        Right (decision, _st) -> Right decision
        Left err -> Left (show err)
    (Nothing, _) -> Left ("missing expansion node " ++ show expNodeId)
    (_, Nothing) -> Left ("missing target node " ++ show targetNodeId)

assertNodeAliasTranslation :: Int -> (NodeId -> NodeId -> InstanceOp) -> Property
assertNodeAliasTranslation size mkOp =
  let (c, root, binderA, binderB, scheme, si, tr) = nodeAliasTranslationFixture size
      ew =
        EdgeWitness
          { ewEdgeId = EdgeId size,
            ewLeft = root,
            ewRight = root,
            ewRoot = root,
            ewForallIntros = 0,
            ewWitness = InstanceWitness [mkOp binderB binderA]
          }
      expected =
        Elab.TForall
          "a"
          Nothing
          (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
      generalizeAt _ _ _ =
        Left (Elab.InstantiationError "assertNodeAliasTranslation: unexpected generalization")
   in case Elab.phiFromEdgeWitnessWithTrace defaultTraceConfig generalizeAt (identityPresolutionView c) Nothing (Just si) (Just tr) ew of
        Left err -> counterexample (show err) False
        Right phi ->
          case Elab.applyInstantiation (Elab.schemeToType scheme) phi of
            Left err -> counterexample (show err) False
            Right out -> counterexample (Elab.pretty phi ++ " => " ++ Elab.pretty out) (out === expected)

nodeAliasTranslationFixture :: Int -> (Constraint, NodeId, NodeId, NodeId, Elab.ElabScheme, Elab.SchemeInfo, EdgeTrace)
nodeAliasTranslationFixture size =
  (c, root, binderA, binderB, scheme, si, tr)
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    binderA = NodeId (base + 1)
    forallB = NodeId (base + 102)
    binderB = NodeId (base + 2)
    bodyNode = NodeId (base + 103)
    c =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (binderA, root, BindFlex),
                  (forallB, root, BindFlex),
                  (binderB, forallB, BindFlex),
                  (bodyNode, forallB, BindFlex)
                ]
          }
    scheme =
      Elab.schemeFromType
        (Elab.TForall "a" Nothing (Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b"))))
    si =
      Elab.SchemeInfo
        { Elab.siScheme = scheme,
          Elab.siSubst = IntMap.fromList [(getNodeId binderA, "a"), (getNodeId binderB, "b")]
        }
    tr =
      EdgeTrace
        { etRoot = root,
          etBinderArgs = [],
          etInterior = fromListInterior [root, binderA, forallB, binderB, bodyNode],
          etReplayContract = ReplayContractNone,
          etBinderReplayMap = mempty,
          etReplayDomainBinders = [],
          etCopyMap = mempty
        }

orderedBinderFixture :: Int -> (Constraint, NodeId, [NodeId])
orderedBinderFixture size =
  (c, root, [bN, aN])
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    body = NodeId (base + 101)
    aN = NodeId (base + 1)
    bN = NodeId (base + 2)
    c =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root body),
                  (getNodeId body, TyArrow body bN aN),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bN, TyVar {tnId = bN, tnBound = Nothing})
                ],
            cBindParents =
              bindParentsFromPairs
                [ (body, root, BindFlex),
                  (aN, root, BindFlex),
                  (bN, root, BindFlex)
                ]
          }

contextFindFixture :: Int -> (Constraint, NodeId, NodeId, [Elab.ContextStep])
contextFindFixture size =
  (c, root, cN, [Elab.StepUnder ("t" ++ show (getNodeId aN)), Elab.StepInside])
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    body = NodeId (base + 101)
    aN = NodeId (base + 1)
    bN = NodeId (base + 2)
    cN = NodeId (base + 3)
    c =
      rootedConstraintLocal
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root body),
                  (getNodeId body, TyArrow body aN bN),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bN, TyVar {tnId = bN, tnBound = Just cN}),
                  (getNodeId cN, TyVar {tnId = cN, tnBound = Nothing})
                ],
            cBindParents =
              bindParentsFromPairs
                [ (body, root, BindFlex),
                  (aN, root, BindFlex),
                  (bN, root, BindFlex),
                  (cN, bN, BindFlex)
                ]
          }

contextRejectFixture :: Int -> (Constraint, NodeId, NodeId)
contextRejectFixture size =
  (c, root, bodyOnly)
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    body = NodeId (base + 101)
    aN = NodeId (base + 1)
    bodyOnly = NodeId (base + 2)
    c =
      rootedConstraintLocal
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root body),
                  (getNodeId body, TyArrow body aN bodyOnly),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bodyOnly, TyVar {tnId = bodyOnly, tnBound = Nothing})
                ],
            cBindParents =
              bindParentsFromPairs
                [ (body, root, BindFlex),
                  (aN, root, BindFlex),
                  (bodyOnly, body, BindFlex)
                ]
          }

chainConstraint :: Int -> Constraint
chainConstraint rawSize =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList $
            [ (i, TyForall (NodeId i) (NodeId (i + 1)))
              | i <- [0 .. size - 2]
            ]
              ++ [(size - 1, TyVar {tnId = NodeId (size - 1), tnBound = Nothing})],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId i, NodeId (i - 1), BindFlex)
              | i <- [1 .. size - 1]
            ]
      }
  where
    size = max 3 rawSize

binderConstraint :: Constraint
binderConstraint =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyForall (NodeId 0) (NodeId 1)),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TyVar {tnId = NodeId 2, tnBound = Nothing})
            ],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId 1, NodeId 0, BindFlex),
              (NodeId 2, NodeId 0, BindRigid)
            ]
      }

varTripleConstraint :: Constraint
varTripleConstraint =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyCon (NodeId 0) (BaseTy "Triple") (NodeId 1 :| [NodeId 2, NodeId 3])),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TyVar {tnId = NodeId 2, tnBound = Nothing}),
              (3, TyVar {tnId = NodeId 3, tnBound = Nothing})
            ],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId 1, NodeId 0, BindFlex),
              (NodeId 2, NodeId 0, BindFlex),
              (NodeId 3, NodeId 0, BindFlex)
            ]
      }

rootedConstraintLocal :: Constraint -> Constraint
rootedConstraintLocal c0 =
  c0
    { cGenNodes = fromListGen [(GenNodeId 0, GenNode (GenNodeId 0) [NodeId 0])],
      cBindParents =
        IntMap.insertWith
          (\_ old -> old)
          (nodeRefKey (typeRef (NodeId 0)))
          (genRef (GenNodeId 0), BindFlex)
          (cBindParents c0)
    }

inertConstraint :: Int -> Constraint
inertConstraint size =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 1)),
              (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)),
              (2, TyArrow (NodeId 2) (NodeId 4) (NodeId 3)),
              (3, TyBase (NodeId 3) (BaseTy ("Int" ++ show size))),
              (4, TyVar {tnId = NodeId 4, tnBound = Nothing})
            ],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId 1, NodeId 0, BindRigid),
              (NodeId 2, NodeId 1, BindFlex),
              (NodeId 3, NodeId 2, BindFlex),
              (NodeId 4, NodeId 2, BindRigid)
            ]
      }

intTy :: Elab.ElabType
intTy = Elab.TBase (BaseTy "Int")

boolTy :: Elab.ElabType
boolTy = Elab.TBase (BaseTy "Bool")

idLam :: Elab.ElabTerm
idLam = Elab.ELam "x" intTy (Elab.EVar "x")

polyId :: Elab.ElabTerm
polyId = Elab.ETyAbs "a" Nothing (Elab.ELam "x" (Elab.TVar "a") (Elab.EVar "x"))
