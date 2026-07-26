{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.ObligationPropertySpec (spec) where

import IdentityTestSupport
import qualified ElabTypeTestSupport as TestElab
import Control.Monad (foldM, forM_)
import Data.Either (isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import MLF.Binding.GraphOps qualified as GraphOps
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Acyclicity (AcyclicityResult (..))
import MLF.Constraint.Inert qualified as Inert
import MLF.Constraint.Presolution
  ( EdgeTrace (..),
    PresolutionError (..),
    PresolutionView (..),
    prEdgeWitnesses,
    prConstraint,
  )
import MLF.Constraint.Presolution.TestSupport
  ( CopyMapping (..),
    OmegaNormalizeEnv (..),
    OmegaNormalizeError,
    PresolutionState (..),
    coalesceRaiseMergeWithEnv,
    normalizeInstanceOpsFull,
    psEdgeTraces,
    reorderWeakenWithEnv,
    decideMinimalExpansion,
    sourceInteriorFromList,
    instantiateScheme,
    instantiateSchemeWithTrace,
    lookupCopy,
    processInstEdge,
    runPresolutionM,
    unifyAcyclic,
    validateNormalizedWitness,
    validateTranslatablePresolution,
  )
import MLF.Constraint.Solve (frWith)
import MLF.Constraint.Solve.TestSupport (SolveResult (..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness(..), InstanceWitness(..))
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Unify.Decompose (decomposeUnifyChildren)
import ElabTermTestSupport (generatedResolvedLocal, mkTestDeferredVar, mkTestLocalLam, mkTestLocalLet, mkTestTyAbs, testTForall, testTVar)
import MLF.Elab.Pipeline qualified as Elab
import MLF.Elab.Phi.TestSupport qualified as PhiTestSupport
import MLF.Elab.Types qualified as ElabTypes
import MLF.Frontend.ConstraintGen
  ( ConstraintError,
    ConstraintResult (..),
    generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply,
  )
import MLF.Frontend.Program.Builtins qualified as Builtins
import MLF.Frontend.Syntax qualified as Surf
import MLF.Reify.TypeOps qualified as TypeOps
import MLF.Types.Identity
  ( StructuralTypeBinderRole (StructuralSelfBinder),
    TypeBinderIdentity,
    UniqueIdentity (..),
    initialIdentityGenerator,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
  )
import Presolution.Util (mkNormalizeConstraint, mkNormalizeEnv)
import SpecUtil
  ( PipelineArtifacts (..),
    bindParentsFromPairs,
    checkAcyclicityRaw,
    defaultTraceConfig,
    emptyConstraint,
    nodeMapFromList,
    normalizeRaw,
    runConstraintDefault,
    runPipelineArtifactsDefault,
    runToPresolutionDefault,
    rootedConstraint,
    solveUnifyRaw,
    unsafeNormalizeExpr,
  )
import Test.Hspec
import Test.QuickCheck

normalizeInstanceOpsForTest
  :: OmegaNormalizeEnv p
  -> [InstanceOp]
  -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsForTest env ops =
  getValidatedInstanceOps <$> normalizeInstanceOpsFull env ops

spec :: Spec
spec = do
  describe "Thesis obligation property evidence" $
    forM_ obligations $ \obligation ->
      it (obligationId obligation) $
        property $
          withMaxSuccess 100 $
            forAll (chooseInt (3, 16)) $ \size ->
              counterexample (obligationId obligation ++ " failed at size " ++ show size) $
                obligationProperty obligation size

  describe "Thesis fixed annotation evidence" $ do
    forM_ (zip [1 :: Int ..] annotationErasureCases) $ \(caseIndex, expr) ->
      it ("preserves annotation erasure case " ++ show caseIndex) $
        expectElabAnnotationErasure expr
    it "constructs a bounded annotation abstraction" $
      expectElabBoundedAnnotationAbs
    it "constructs the paper's mixed existential/universal annotation" $
      expectElabMixedAnnotation
    it "keeps a nested mixed annotation local to its let RHS" $
      expectNestedMixedAnnotationLocal
    it "constructs the paper's annotated self-application" $
      expectElabAnnotatedSelfApp

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
   in case GraphOps.applyWeaken (TypeRefTag (NodeId (size - 1))) c of
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
   in case GraphOps.applyRaiseStep (TypeRefTag (NodeId (size - 1))) c of
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
   in case GraphOps.applyRaiseTo (TypeRefTag (NodeId (size - 1))) target c of
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
   in case solveUnifyRaw defaultTraceConfig c of
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
                  [ (0, TestTyCon (NodeId 0) (BaseTy "Pair") (NodeId 1 :| [NodeId 4])),
                    (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)),
                    (2, TestTyBase (NodeId 2) (BaseTy "Int")),
                    (3, TestTyBase (NodeId 3) (BaseTy "Bool")),
                    (4, TyArrow (NodeId 4) (NodeId 5) (NodeId 6)),
                    (5, TestTyBase (NodeId 5) (BaseTy "Int")),
                    (6, TestTyBase (NodeId 6) (BaseTy "Bool"))
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
   in case solveUnifyRaw defaultTraceConfig c of
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
   in case solveUnifyRaw defaultTraceConfig c of
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
  Elab.typeCheck (Elab.ELit (Surf.LInt 0)) === Right builtinIntTy

propWfTVar :: Int -> Property
propWfTVar _size =
  typeCheckShouldMatch (Elab.typeCheck polyId) polyIdTy

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
  let refA = elabTypeRef 417 "a"
   in applyShouldBe Elab.TBottom (ElabTypes.instAbstrWithRef refA) (ElabTypes.tVarWithRef refA)

propInstInner :: Int -> Property
propInstInner _size =
  applyShouldBe forallA (Elab.InstInside (Elab.InstBot intTy)) (testTForall "a" (Just (boundFromType intTy)) (testTVar "a"))

propInstOuter :: Int -> Property
propInstOuter _size =
  let refX = elabTypeRef 425 "x"
   in applyShouldBe
        (testTForall "a" Nothing (testTVar "z"))
        (ElabTypes.instUnderWithRef refX (ElabTypes.instAbstrWithRef refX))
        (testTForall "a" Nothing (testTVar "a"))

propInstQuantElim :: Int -> Property
propInstQuantElim _size =
  applyShouldBe forallA Elab.InstElim Elab.TBottom

propInstQuantIntro :: Int -> Property
propInstQuantIntro _size =
  case Elab.applyInstantiation intTy Elab.InstIntro of
    Right (Elab.TForallRef _ Nothing body) -> body === intTy
    other -> counterexample (show other) False

propTypingVar :: Int -> Property
propTypingVar _size =
  let resolved = generatedResolvedLocal 0 "x" "x" intTy
      env = Elab.mkTypeCheckEnvWithResolvedTerms [(resolved, intTy)] Map.empty
   in Elab.typeCheckWithEnv env (Elab.EVarNode resolved) === Right intTy

propTypingAbs :: Int -> Property
propTypingAbs _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propTypingApp :: Int -> Property
propTypingApp _size =
  Elab.typeCheck (Elab.EApp idLam (Elab.ELit (Surf.LInt 1))) === Right intTy

propTypingTAbs :: Int -> Property
propTypingTAbs _size =
  typeCheckShouldMatch (Elab.typeCheck polyId) polyIdTy

propTypingTApp :: Int -> Property
propTypingTApp _size =
  Elab.typeCheck (Elab.ETyInst polyId (Elab.InstApp intTy)) === Right (Elab.TArrow intTy intTy)

propTypingLet :: Int -> Property
propTypingLet _size =
  Elab.typeCheck (mkTestLocalLet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (mkTestDeferredVar "x")) === Right intTy

propRedBeta :: Int -> Property
propRedBeta _size =
  Elab.step (Elab.EApp idLam (Elab.ELit (Surf.LInt 1))) === Just (Elab.ELit (Surf.LInt 1))

propRedBetaLet :: Int -> Property
propRedBetaLet _size =
  Elab.step (mkTestLocalLet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (mkTestDeferredVar "x")) === Just (Elab.ELit (Surf.LInt 1))

propRedReflex :: Int -> Property
propRedReflex _size =
  Elab.step (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstId) === Just (Elab.ELit (Surf.LInt 1))

propRedTrans :: Int -> Property
propRedTrans _size =
  let term = Elab.ETyInst (Elab.ELit (Surf.LInt 1)) (Elab.InstSeq Elab.InstIntro Elab.InstElim)
   in Elab.step term === Just (Elab.ETyInst (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro) Elab.InstElim)

propRedQuantIntro :: Int -> Property
propRedQuantIntro _size =
  case Elab.step (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro) of
    Just (Elab.ETyAbsRef ref Nothing (Elab.ELit (Surf.LInt 1))) -> ElabTypes.typeBinderRefName ref === "u0"
    other -> counterexample ("Expected generated InstIntro abstraction, got: " ++ show other) False

propRedQuantElim :: Int -> Property
propRedQuantElim _size =
  Elab.step (Elab.ETyInst polyId Elab.InstElim) === Just (mkTestLocalLam "x" Elab.TBottom (mkTestDeferredVar "x"))

propRedInner :: Int -> Property
propRedInner _size =
  let term = Elab.ETyInst (mkTestTyAbs "a" Nothing (mkTestDeferredVar "x")) (Elab.InstInside (Elab.InstBot intTy))
   in Elab.step term === Just (mkTestTyAbs "a" (Just (boundFromType intTy)) (mkTestDeferredVar "x"))

propRedOuter :: Int -> Property
propRedOuter _size =
  let body = mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")
      term = Elab.ETyInst (mkTestTyAbs "a" Nothing body) (ElabTypes.instUnderWithRef (elabTypeRef 501 "b") (Elab.InstApp intTy))
   in Elab.step term === Just (mkTestTyAbs "a" Nothing (Elab.ETyInst body (Elab.InstApp intTy)))

propRedContext :: Int -> Property
propRedContext _size =
  let arg = Elab.EApp (mkTestLocalLam "y" intTy (mkTestDeferredVar "y")) (Elab.ELit (Surf.LInt 1))
   in Elab.step (Elab.EApp idLam arg) === Just (Elab.EApp idLam (Elab.ELit (Surf.LInt 1)))

propApplyN :: Int -> Property
propApplyN _size =
  applyShouldBe forallA Elab.InstElim Elab.TBottom

propApplyO :: Int -> Property
propApplyO _size =
  case Elab.applyInstantiation intTy Elab.InstIntro of
    Right (Elab.TForallRef _ Nothing body) -> body === intTy
    other -> counterexample (show other) False

propApplySeq :: Int -> Property
propApplySeq _size =
  let first = Elab.InstIntro
      second = Elab.InstElim
      lhs = Elab.applyInstantiation intTy (Elab.InstSeq first second)
      rhs = Elab.applyInstantiation intTy first >>= \midTy -> Elab.applyInstantiation midTy second
   in conjoin
        [ lhs === rhs,
          lhs === Right intTy
        ]

propApplyInner :: Int -> Property
propApplyInner _size =
  propInstInner 0

propApplyOuter :: Int -> Property
propApplyOuter _size =
  propInstOuter 0

propApplyHyp :: Int -> Property
propApplyHyp _size =
  let refA = elabTypeRef 540 "a"
   in applyShouldBe Elab.TBottom (ElabTypes.instAbstrWithRef refA) (ElabTypes.tVarWithRef refA)

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
  let body = Elab.TArrow (testTVar "a") (testTVar "b")
      src = testTForall "a" Nothing (testTForall "b" Nothing body)
      tgt = testTForall "b" Nothing (testTForall "a" Nothing body)
   in case Elab.sigmaReorder src tgt of
        Right inst ->
          conjoin
            [ counterexample (show inst) (inst /= Elab.InstId),
              counterexample (show inst) (isRight (Elab.applyInstantiation src inst))
            ]
        Left err -> counterexample (show err) False

propSigmaReorderIdentity :: Int -> Property
propSigmaReorderIdentity _size =
  let src = testTForall "a" Nothing (Elab.TArrow (testTVar "a") intTy)
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
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr (Surf.ELam "x" (Surf.EVar "x"))) of
    Right (Elab.ETyAbsRef {}, ty) -> typeShouldMatch ty polyIdTy
    other -> counterexample (show other) False

propElabApp :: Int -> Property
propElabApp _size =
  elaboratesTo (Surf.EApp (Surf.ELam "x" (Surf.EVar "x")) (Surf.ELit (Surf.LInt 1))) intTy

propElabLet :: Int -> Property
propElabLet _size =
  elaboratesTo letIdAppExpr intTy

-- Thesis Property 15.3.14, specialized to the eMLF annotation forms whose
-- translation is introduced in §15.3.8.  Type abstractions, type computations,
-- and explicit recursive-type evidence erase; the remaining value-term shape
-- must be the original annotated source with its annotations removed.
expectElabAnnotationErasure :: Surf.SurfaceExpr -> Expectation
expectElabAnnotationErasure expr =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Right (term, _ty) ->
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations expr
    Left err -> expectationFailure (Elab.renderPipelineError err)

-- Thesis §§12.3.2 and 15.3.8: an annotation coercing the identity
-- abstraction to forall (a >= sigma-id). a -> a is itself an identity term.
-- Its xMLF construction therefore binds the flexible result before building
-- the lambda; it must not retrofit an unrelated outer InstIntro afterwards.
expectElabBoundedAnnotationAbs :: Expectation
expectElabBoundedAnnotationAbs =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr boundedIdentityAnnotationExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      ty `shouldMatchType` boundedIdentityAnnotationType
      case Elab.typeCheck term of
        Left err -> expectationFailure (show err)
        Right checkedTy -> checkedTy `shouldMatchType` boundedIdentityAnnotationType
      expectBoundedIdentityAnnotationShape term

-- Thesis §12.3.2 uses κ = exists beta. forall alpha.
-- beta -> (alpha -> alpha) as the representative source annotation.  The
-- existential beta is inferred and generalized outside the annotation-owned
-- universal alpha; the two binders must both be present in the checked xMLF
-- construction.
expectElabMixedAnnotation :: Expectation
expectElabMixedAnnotation =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr mixedAnnotationExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      if TypeOps.alphaEqType ty mixedAnnotationType
        then pure ()
        else
          expectationFailure
            ( "mixed annotation term: "
                ++ show term
                ++ "\nactual type: "
                ++ show ty
                ++ "\nexpected type: "
                ++ show mixedAnnotationType
            )
      Elab.typeCheck term `shouldBe` Right ty
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations mixedAnnotationExpr

-- The inferred existential in the source annotation belongs to the
-- annotation's publication boundary.  Using the annotated value in an outer
-- let must instantiate that binder, not leak it into the enclosing result.
expectNestedMixedAnnotationLocal :: Expectation
expectNestedMixedAnnotationLocal =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr nestedMixedAnnotationExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      ty `shouldMatchType` boolTy
      Elab.typeCheck term `shouldBe` Right ty
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations nestedMixedAnnotationExpr

-- Thesis §15.3.8: omega = lambda (g : sigma-id) . g g elaborates, up to
-- identity computations, to
--   Lambda (alpha >= sigma-id). lambda (g : sigma-id).
--     (g[sigma-id] g)[alpha]
-- and has the principal flexible result type
--   forall (alpha >= sigma-id). sigma-id -> alpha.
expectElabAnnotatedSelfApp :: Expectation
expectElabAnnotatedSelfApp =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr annotatedSelfAppExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      ty `shouldMatchType` annotatedSelfAppType
      case Elab.typeCheck term of
        Left err -> expectationFailure (show err)
        Right checkedTy -> checkedTy `shouldMatchType` annotatedSelfAppType
      expectAnnotatedSelfAppShape term

shouldMatchType :: Elab.ElabType -> Elab.ElabType -> Expectation
shouldMatchType actual expected =
  if TypeOps.alphaEqType actual expected
    then pure ()
    else expectationFailure (show actual ++ " /= " ++ show expected)

data ErasedTerm
  = ErasedVar String
  | ErasedLit Surf.Lit
  | ErasedLam String ErasedTerm
  | ErasedApp ErasedTerm ErasedTerm
  | ErasedLet String ErasedTerm ErasedTerm
  deriving (Eq, Show)

eraseSurfaceAnnotations :: Surf.SurfaceExpr -> ErasedTerm
eraseSurfaceAnnotations expr =
  case expr of
    Surf.EVarNode reference -> ErasedVar (Surf.termReferenceName reference)
    Surf.ELit lit -> ErasedLit lit
    Surf.ELamNode reference body ->
      ErasedLam (Surf.termReferenceName reference) (eraseSurfaceAnnotations body)
    Surf.EApp fun arg -> ErasedApp (eraseSurfaceAnnotations fun) (eraseSurfaceAnnotations arg)
    Surf.ELetNode reference rhs body ->
      ErasedLet
        (Surf.termReferenceName reference)
        (eraseSurfaceAnnotations rhs)
        (eraseSurfaceAnnotations body)
    Surf.ELamAnnNode reference _ body ->
      ErasedLam (Surf.termReferenceName reference) (eraseSurfaceAnnotations body)
    Surf.EExactLamNode reference _ body ->
      ErasedLam (Surf.termReferenceName reference) (eraseSurfaceAnnotations body)
    Surf.EAnn inner _ -> eraseSurfaceAnnotations inner
    Surf.EExactAnn inner _ _ -> eraseSurfaceAnnotations inner

eraseXmlfTerm :: Elab.XmlfTerm -> ErasedTerm
eraseXmlfTerm term =
  case term of
    Elab.EVarNode resolved -> ErasedVar (ElabTypes.resolvedVarReferenceName resolved)
    Elab.ELit lit -> ErasedLit lit
    Elab.ELam resolved body ->
      ErasedLam (ElabTypes.resolvedVarReferenceName resolved) (eraseXmlfTerm body)
    Elab.EApp fun arg -> ErasedApp (eraseXmlfTerm fun) (eraseXmlfTerm arg)
    Elab.ELet resolved _ rhs body ->
      ErasedLet
        (ElabTypes.resolvedVarReferenceName resolved)
        (eraseXmlfTerm rhs)
        (eraseXmlfTerm body)
    Elab.ETyAbsRef _ _ body -> eraseXmlfTerm body
    Elab.ETyInst inner _ -> eraseXmlfTerm inner
    Elab.ERoll _ body -> eraseXmlfTerm body
    Elab.EUnroll body -> eraseXmlfTerm body

annotationErasureCases :: [Surf.SurfaceExpr]
annotationErasureCases =
  [ Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int"),
    Surf.EAnn (Surf.ELam "x" (Surf.EVar "x")) sigmaIdSource,
    annotatedSelfAppExpr,
    Surf.ELet
      "id"
      (Surf.EAnn (Surf.ELam "x" (Surf.EVar "x")) sigmaIdSource)
      (Surf.EApp (Surf.EVar "id") (Surf.ELit (Surf.LInt 1))),
    Surf.ELamAnn
      "poly"
      sigmaIdSource
      ( Surf.ELet
          "keepInt"
          (Surf.EApp (Surf.EVar "poly") (Surf.ELit (Surf.LInt 1)))
          (Surf.EApp (Surf.EVar "poly") (Surf.ELit (Surf.LBool True)))
      )
  ]

sigmaIdSource :: Surf.SrcType
sigmaIdSource =
  Surf.STForall "a" Nothing (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))

boundedIdentityAnnotationExpr :: Surf.SurfaceExpr
boundedIdentityAnnotationExpr =
  Surf.EAnn
    (Surf.ELam "x" (Surf.EVar "x"))
    ( Surf.STForall
        "a"
        (Just (Surf.mkSrcBound sigmaIdSource))
        (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))
    )

boundedIdentityAnnotationType :: Elab.ElabType
boundedIdentityAnnotationType =
  testTForall
    "a"
    (Just (boundFromType polyIdTy))
    (Elab.TArrow (testTVar "a") (testTVar "a"))

mixedAnnotationExpr :: Surf.SurfaceExpr
mixedAnnotationExpr =
  Surf.EAnn
    (Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "y")))
    ( Surf.STForall
        "alpha"
        Nothing
        ( Surf.STArrow
            (Surf.STVar "beta")
            (Surf.STArrow (Surf.STVar "alpha") (Surf.STVar "alpha"))
        )
    )

nestedMixedAnnotationExpr :: Surf.SurfaceExpr
nestedMixedAnnotationExpr =
  Surf.ELet
    "k"
    mixedAnnotationExpr
    ( Surf.EApp
        (Surf.EApp (Surf.EVar "k") (Surf.ELit (Surf.LInt 1)))
        (Surf.ELit (Surf.LBool True))
    )

mixedAnnotationType :: Elab.ElabType
mixedAnnotationType =
  testTForall
    "beta"
    Nothing
    ( testTForall
        "alpha"
        Nothing
        ( Elab.TArrow
            (testTVar "beta")
            (Elab.TArrow (testTVar "alpha") (testTVar "alpha"))
        )
    )

expectBoundedIdentityAnnotationShape :: Elab.XmlfTerm -> Expectation
expectBoundedIdentityAnnotationShape term =
  case term of
    Elab.ETyAbsRef resultRef (Just resultBound) (Elab.ELam binder (Elab.EVarNode occurrence)) -> do
      ElabTypes.tyToElab resultBound `shouldMatchType` polyIdTy
      expectIdentityLambdaAt resultRef binder occurrence
    Elab.ETyAbsRef resultRef (Just resultBound)
      ( Elab.ETyInst
          (Elab.ETyAbsRef sourceRef Nothing (Elab.ELam binder (Elab.EVarNode occurrence)))
          (Elab.InstApp (Elab.TVarRef instantiatedRef))
        ) -> do
        ElabTypes.tyToElab resultBound `shouldMatchType` polyIdTy
        ElabTypes.typeBinderRefsSameIdentity resultRef instantiatedRef `shouldBe` True
        expectIdentityLambdaAt sourceRef binder occurrence
    _ -> expectationFailure ("expected direct bounded type abstraction, got " ++ show term)
  where
    expectIdentityLambdaAt expectedRef binder occurrence = do
      case ElabTypes.resolvedVarType binder of
        Elab.TVarRef binderRef ->
          ElabTypes.typeBinderRefsSameIdentity expectedRef binderRef `shouldBe` True
        _ -> expectationFailure "bounded annotation lambda does not use its quantified carrier"
      ElabTypes.resolvedVarDetails occurrence
        `shouldBe` ElabTypes.resolvedVarDetails binder
      ElabTypes.resolvedVarType occurrence
        `shouldMatchType` ElabTypes.resolvedVarType binder

annotatedSelfAppExpr :: Surf.SurfaceExpr
annotatedSelfAppExpr =
  Surf.ELamAnn
    "g"
    sigmaIdSource
    (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))

annotatedSelfAppType :: Elab.ElabType
annotatedSelfAppType =
  testTForall
    "result"
    (Just (boundFromType polyIdTy))
    (Elab.TArrow polyIdTy (testTVar "result"))

expectAnnotatedSelfAppShape :: Elab.XmlfTerm -> Expectation
expectAnnotatedSelfAppShape term =
  case term of
    Elab.ETyAbsRef resultRef (Just resultBound) (Elab.ELam binder body) ->
      case body of
        Elab.ETyInst
          ( Elab.EApp
              (Elab.ETyInst (Elab.EVarNode funVar) (Elab.InstApp funArgTy))
              (Elab.EVarNode argVar)
            )
          (Elab.InstAbstrRef abstractedRef) -> do
            ElabTypes.tyToElab resultBound `shouldMatchType` polyIdTy
            funArgTy `shouldMatchType` polyIdTy
            ElabTypes.typeBinderRefsSameIdentity resultRef abstractedRef `shouldBe` True
            ElabTypes.resolvedVarDetails funVar
              `shouldBe` ElabTypes.resolvedVarDetails binder
            ElabTypes.resolvedVarDetails argVar
              `shouldBe` ElabTypes.resolvedVarDetails binder
        _ -> expectationFailure ("unexpected annotated self-application body: " ++ show body)
    _ -> expectationFailure ("unexpected annotated self-application outer form: " ++ show term)

propEnvLambda :: Int -> Property
propEnvLambda _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propEnvLet :: Int -> Property
propEnvLet _size =
  Elab.typeCheck (mkTestLocalLet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (mkTestDeferredVar "x")) === Right intTy

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
   in normalizeInstanceOpsForTest env [OpRaise (NodeId 2)] === Right []

propTrRigidMerge :: Int -> Property
propTrRigidMerge _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsForTest env [OpMerge (NodeId 2) (NodeId 3)] === Right []

propTrRigidRaiseMerge :: Int -> Property
propTrRigidRaiseMerge _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsForTest env [OpRaiseMerge (NodeId 2) (NodeId 3)] === Right []

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
   in normalizeInstanceOpsForTest env [OpWeaken n] === Right [OpWeaken n]

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
   in normalizeInstanceOpsForTest env [OpGraft arg binder, OpWeaken binder] === Right [OpGraft arg binder, OpWeaken binder]

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
    Right (_term, Elab.TForallRef _ Nothing (Elab.TArrow dom cod)) -> counterexample (show (dom, cod)) (dom == cod)
    other -> counterexample (show other) False

propBindMono :: Int -> Property
propBindMono _size =
  case runPipelineArtifactsDefault Set.empty (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) of
    Right PipelineArtifacts {paPresolution = presolution} ->
      Binding.checkBindingTree (prConstraint presolution) === Right ()
    Left err -> counterexample err False

propSynToGraph :: Int -> Property
propSynToGraph size =
  forAll (genMixedAnnotation size) $ \annotation ->
    counterexample ("generated annotation: " ++ show annotation) $
      conjoin
        [ checkSynToGraph annotation,
          checkSynToGraph
            (Surf.STVar ("bare-existential-" ++ show size)),
          checkSynToGraph graphNormalizedEqVarAnnotation
        ]

graphNormalizedEqVarAnnotation :: Surf.SrcType
graphNormalizedEqVarAnnotation =
  Surf.STForall
    "graph-root-a"
    (Just (Surf.mkSrcBound (Surf.STBase "Int")))
    (Surf.STForall "graph-root-unused" Nothing (Surf.STVar "graph-root-a"))

data AnnotationBinderKind
  = AnnotationForallBinder
  | AnnotationMuBinder
  deriving (Eq, Show)

data CoercionCopyEvidence = CoercionCopyEvidence
  { coercionEvidenceFreeNodes :: Map.Map String NodeId,
    coercionEvidenceDomainOwned :: IntSet.IntSet,
    coercionEvidenceCodomainOwned :: IntSet.IntSet
  }
  deriving (Eq, Show)

emptyCoercionCopyEvidence :: CoercionCopyEvidence
emptyCoercionCopyEvidence =
  CoercionCopyEvidence
    { coercionEvidenceFreeNodes = Map.empty,
      coercionEvidenceDomainOwned = IntSet.empty,
      coercionEvidenceCodomainOwned = IntSet.empty
    }

checkSynToGraph :: Surf.SrcType -> Property
checkSynToGraph annotation =
  let binderIdentities = annotationBinderIdentities annotation
   in case runAnnotationConstraint binderIdentities annotation of
        Right result@ConstraintResult {crConstraint = c, crRoot = codomainRoot} ->
          case cInstEdges c of
            [InstEdge _ _ destination] ->
              case lookupNodeIn (cNodes c) destination of
                Just TyVar {tnBound = Just domainRoot} ->
                  conjoin
                    [ case
                        validateAnnotationCopies
                          binderIdentities
                          result
                          annotation
                          domainRoot
                          codomainRoot
                      of
                        Right () -> property True
                        Left err -> counterexample err False,
                      counterexample
                        "annotation source authority was not recorded at the codomain"
                        ( IntMap.member
                            (getNodeId codomainRoot)
                            (crAnnSourceTypes result)
                        ),
                      Binding.checkBindingTree c === Right ()
                    ]
                other ->
                  counterexample
                    ("annotation edge destination did not retain its domain: " ++ show other)
                    False
            edges ->
              counterexample
                ("expected one annotation edge, saw " ++ show edges)
                False
        Left err -> counterexample (show err) False

runAnnotationConstraint ::
  Map.Map String TypeBinderIdentity ->
  Surf.SrcType ->
  Either ConstraintError (ConstraintResult 'Raw)
runAnnotationConstraint binderIdentities annotation =
  generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
    initialIdentityGenerator
    Set.empty
    (Builtins.builtinSourceTypeHeadIdentities annotation)
    binderIdentities
    Map.empty
    ( unsafeNormalizeExpr
        (Surf.EAnn (Surf.ELit (Surf.LInt 1)) annotation)
    )

annotationBinderIdentities :: Surf.SrcType -> Map.Map String TypeBinderIdentity
annotationBinderIdentities annotation =
  Map.fromList
    [ (name, binderIdentity index binderKind)
      | (index, (name, binderKind)) <-
          zip [0 :: Int ..] (annotationBinders annotation)
    ]
  where
    binderIdentity index binderKind =
      let unique = UniqueIdentity (991900000 + index)
       in case binderKind of
            AnnotationForallBinder ->
              typeBinderIdentityFromUnique unique
            AnnotationMuBinder ->
              typeBinderIdentityFromStructural unique StructuralSelfBinder

annotationBinders :: Surf.SrcTy n v -> [(String, AnnotationBinderKind)]
annotationBinders sourceType =
  case sourceType of
    Surf.STVar _ -> []
    Surf.STArrow dom cod ->
      annotationBinders dom ++ annotationBinders cod
    Surf.STBase _ -> []
    Surf.STCon _ args ->
      foldMap annotationBinders args
    Surf.STVarApp _ args ->
      foldMap annotationBinders args
    Surf.STTyLam name body ->
      (name, AnnotationForallBinder) : annotationBinders body
    Surf.STTyApp fun arg ->
      annotationBinders fun ++ annotationBinders arg
    Surf.STForall name mbBound body ->
      (name, AnnotationForallBinder)
        : maybe [] (annotationBinders . Surf.unSrcBound) mbBound
          ++ annotationBinders body
    Surf.STMu name body ->
      (name, AnnotationMuBinder) : annotationBinders body
    Surf.STBottom -> []

annotationGraphicFreeVars :: Surf.SrcTy n v -> Set.Set String
annotationGraphicFreeVars sourceType =
  case sourceType of
    Surf.STVar name -> Set.singleton name
    Surf.STArrow dom cod ->
      annotationGraphicFreeVars dom <> annotationGraphicFreeVars cod
    Surf.STBase _ -> Set.empty
    Surf.STCon _ args ->
      foldMap annotationGraphicFreeVars args
    Surf.STVarApp name args ->
      Set.insert name (foldMap annotationGraphicFreeVars args)
    Surf.STTyLam name body ->
      Set.delete name (annotationGraphicFreeVars body)
    Surf.STTyApp fun arg ->
      annotationGraphicFreeVars fun <> annotationGraphicFreeVars arg
    Surf.STForall name mbBound body ->
      let bodyFree = annotationGraphicFreeVars body
       in if Set.member name bodyFree
            then
              Set.delete name bodyFree
                <> maybe
                  Set.empty
                  (annotationGraphicFreeVars . Surf.unSrcBound)
                  mbBound
            else bodyFree
    Surf.STMu name body ->
      Set.delete name (annotationGraphicFreeVars body)
    Surf.STBottom -> Set.empty

annotationGraphicRootVariable :: Surf.SrcTy n v -> Maybe String
annotationGraphicRootVariable sourceType =
  case sourceType of
    Surf.STVar name -> Just name
    Surf.STForall name mbBound body ->
      let bodyRoot = annotationGraphicRootVariable body
       in if bodyRoot == Just name
            then
              mbBound
                >>= annotationGraphicRootVariable . Surf.unSrcBound
            else
              if Set.notMember name (annotationGraphicFreeVars body)
                then bodyRoot
                else Nothing
    _ -> Nothing

genMixedAnnotation :: Int -> Gen Surf.SrcType
genMixedAnnotation requestedSize = do
  salt <- chooseInt (0, 1000000)
  let existential = annotationName "existential" salt [0]
      constructorHead = annotationName "constructor" salt [0]
      boundExistential = annotationName "bound-existential" salt [0]
      universal = annotationName "forall" salt [0]
      recursive = annotationName "mu" salt [0]
      freeNames = [existential, constructorHead, boundExistential]
      structuralBound =
        Surf.STArrow
          (Surf.STVar boundExistential)
          (Surf.STBase "Bool")
      seed =
        Surf.STForall
          universal
          (Just (Surf.mkSrcBound structuralBound))
          ( Surf.STArrow
              (Surf.STCon "List" (Surf.STVar existential :| []))
              ( Surf.STArrow
                  ( Surf.STVarApp
                      constructorHead
                      (Surf.STVar existential :| [Surf.STBase "Int"])
                  )
                  ( Surf.STMu
                      recursive
                      ( Surf.STArrow
                          (Surf.STVar recursive)
                          ( Surf.STArrow
                              (Surf.STVar universal)
                              Surf.STBottom
                          )
                      )
                  )
              )
          )
      depth = max 1 (min 4 (requestedSize `div` 4))
  growAnnotation freeNames [] salt [1] depth seed

annotationName :: String -> Int -> [Int] -> String
annotationName prefix salt path =
  prefix
    ++ "-"
    ++ show salt
    ++ concatMap (("-" ++) . show) path

growAnnotation ::
  [String] ->
  [String] ->
  Int ->
  [Int] ->
  Int ->
  Surf.SrcType ->
  Gen Surf.SrcType
growAnnotation freeNames boundNames salt path depth seedType
  | depth <= 0 = pure seedType
  | otherwise =
      frequency
        [ (2, pure seedType),
          (4, do
              sibling <-
                genAnnotation
                  freeNames
                  boundNames
                  salt
                  (0 : path)
                  (depth - 1)
              wrapped <-
                elements
                  [ Surf.STArrow seedType sibling,
                    Surf.STArrow sibling seedType
                  ]
              growAnnotation
                freeNames
                boundNames
                salt
                (1 : path)
                (depth - 1)
                wrapped
          ),
          (2,
            growAnnotation
              freeNames
              boundNames
              salt
              (2 : path)
              (depth - 1)
              (Surf.STCon "List" (seedType :| []))
          ),
          (2, do
              headName <- elements (freeNames ++ boundNames)
              sibling <-
                genAnnotation
                  freeNames
                  boundNames
                  salt
                  (3 : path)
                  (depth - 1)
              growAnnotation
                freeNames
                boundNames
                salt
                (4 : path)
                (depth - 1)
                (Surf.STVarApp headName (seedType :| [sibling]))
          ),
          (3, do
              let binder = annotationName "forall" salt (5 : path)
              mbBound <-
                frequency
                  [ (2, pure Nothing),
                    (1,
                      Just . Surf.mkSrcBound
                        <$> genStructuralAnnotation
                          freeNames
                          boundNames
                          salt
                          (6 : path)
                          (depth - 1)
                    )
                  ]
              growAnnotation
                freeNames
                boundNames
                salt
                (7 : path)
                (depth - 1)
                ( Surf.STForall
                    binder
                    mbBound
                    (Surf.STArrow seedType (Surf.STVar binder))
                )
          ),
          (2,
            let binder = annotationName "mu" salt (8 : path)
             in growAnnotation
                  freeNames
                  boundNames
                  salt
                  (9 : path)
                  (depth - 1)
                  ( Surf.STMu
                      binder
                      (Surf.STArrow (Surf.STVar binder) seedType)
                  )
          )
        ]

genAnnotation ::
  [String] ->
  [String] ->
  Int ->
  [Int] ->
  Int ->
  Gen Surf.SrcType
genAnnotation freeNames boundNames salt path depth
  | depth <= 0 =
      genAnnotationLeaf freeNames boundNames
  | otherwise =
      frequency
        [ (4, genAnnotationLeaf freeNames boundNames),
          (5,
            Surf.STArrow
              <$> recurse 0
              <*> recurse 1
          ),
          (2,
            (\arg -> Surf.STCon "List" (arg :| []))
              <$> recurse 2
          ),
          (2, do
              headName <- elements (freeNames ++ boundNames)
              firstArg <- recurse 3
              restArgs <-
                frequency
                  [ (2, pure []),
                    (1, (: []) <$> recurse 4)
                  ]
              pure (Surf.STVarApp headName (firstArg :| restArgs))
          ),
          (3, do
              let binder = annotationName "forall" salt (5 : path)
              mbBound <-
                frequency
                  [ (2, pure Nothing),
                    (1,
                      Just . Surf.mkSrcBound
                        <$> genStructuralAnnotation
                          freeNames
                          boundNames
                          salt
                          (6 : path)
                          (depth - 1)
                    )
                  ]
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (7 : path)
                  (depth - 1)
              pure (Surf.STForall binder mbBound body)
          ),
          (2, do
              let binder = annotationName "mu" salt (8 : path)
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (9 : path)
                  (depth - 1)
              pure (Surf.STMu binder body)
          )
        ]
  where
    recurse tag =
      genAnnotation
        freeNames
        boundNames
        salt
        (tag : path)
        (depth - 1)

genAnnotationLeaf :: [String] -> [String] -> Gen Surf.SrcType
genAnnotationLeaf freeNames boundNames =
  frequency
    [ (4, Surf.STVar <$> elements (freeNames ++ boundNames)),
      (3, Surf.STBase <$> elements ["Int", "Bool", "String"]),
      (1, pure Surf.STBottom)
    ]

genStructuralAnnotation ::
  [String] ->
  [String] ->
  Int ->
  [Int] ->
  Int ->
  Gen Surf.SrcType
genStructuralAnnotation freeNames boundNames salt path depth
  | depth <= 0 =
      frequency
        [ (3, Surf.STBase <$> elements ["Int", "Bool", "String"]),
          (2,
            (\arg -> Surf.STCon "List" (arg :| []))
              <$> genAnnotationLeaf freeNames boundNames
          ),
          (1, pure Surf.STBottom)
        ]
  | otherwise =
      frequency
        [ (3, Surf.STBase <$> elements ["Int", "Bool", "String"]),
          (4,
            Surf.STArrow
              <$> recurse 0
              <*> recurse 1
          ),
          (2,
            (\arg -> Surf.STCon "List" (arg :| []))
              <$> recurse 2
          ),
          (2, do
              headName <- elements (freeNames ++ boundNames)
              firstArg <- recurse 3
              pure (Surf.STVarApp headName (firstArg :| []))
          ),
          (2, do
              let binder = annotationName "forall" salt (4 : path)
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (5 : path)
                  (depth - 1)
              pure (Surf.STForall binder Nothing body)
          ),
          (1, do
              let binder = annotationName "mu" salt (6 : path)
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (7 : path)
                  (depth - 1)
              pure (Surf.STMu binder body)
          ),
          (1, pure Surf.STBottom)
        ]
  where
    recurse tag =
      genAnnotation
        freeNames
        boundNames
        salt
        (tag : path)
        (depth - 1)

validateAnnotationCopies ::
  Map.Map String TypeBinderIdentity ->
  ConstraintResult 'Raw ->
  Surf.SrcType ->
  NodeId ->
  NodeId ->
  Either String ()
validateAnnotationCopies binderIdentities result annotation domainRoot codomainRoot = do
  evidence <-
    go Map.empty Map.empty annotation domainRoot codomainRoot
  let domainOwned = coercionEvidenceDomainOwned evidence
      codomainOwned = coercionEvidenceCodomainOwned evidence
  if IntSet.member (getNodeId domainRoot) domainOwned
    then
      expectNodeKind
        "rigid domain root"
        domainRoot
        Binding.NodeRestricted
    else Right ()
  if IntSet.member (getNodeId codomainRoot) codomainOwned
    then
      expectNodeKind
        "flexible codomain root"
        codomainRoot
        Binding.NodeInstantiable
    else Right ()
  requireEvidence
    (IntSet.null (IntSet.intersection domainOwned codomainOwned))
    ( "copy-owned nodes were shared between domain and codomain: "
        ++ show (IntSet.toList (IntSet.intersection domainOwned codomainOwned))
    )
  forM_ (Map.toList (coercionEvidenceFreeNodes evidence)) $ \(name, node) -> do
    requireEvidence
      (not (IntSet.member (getNodeId node) domainOwned))
      ("free node was owned by the rigid copy: " ++ name)
    requireEvidence
      (not (IntSet.member (getNodeId node) codomainOwned))
      ("free node was owned by the flexible copy: " ++ name)
    expectNodeKind
      ("shared existential " ++ name)
      node
      Binding.NodeInstantiable
  case Binding.checkBindingTree constraint of
    Right () -> Right ()
    Left err ->
      Left
        ( "invalid binding tree: "
            ++ show err
            ++ bindingErrorContext err
        )
  where
    constraint = crConstraint result
    nodes = cNodes constraint

    bindingErrorContext (ParentNotUpper (TypeRef child) (TypeRef parent)) =
      "; child="
        ++ show (nodeContext child)
        ++ "; parent="
        ++ show (nodeContext parent)
    bindingErrorContext _ = ""

    nodeContext node =
      ( lookupNodeIn nodes node,
        IntMap.lookup
          (nodeRefKey (typeRef node))
          (cBindParents constraint),
        [ parent
          | (parent, parentNode) <- toListNode nodes,
            node `elem` structuralChildrenWithBounds parentNode
        ]
      )

    go ::
      Map.Map String NodeId ->
      Map.Map String NodeId ->
      Surf.SrcTy n v ->
      NodeId ->
      NodeId ->
      Either String CoercionCopyEvidence
    go domainEnv codomainEnv sourceType domainNode codomainNode =
      case sourceType of
        Surf.STVar name ->
          validateVariable
            domainEnv
            codomainEnv
            name
            domainNode
            codomainNode
        Surf.STArrow sourceDomain sourceCodomain ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyArrow {tnDom = domainDom, tnCod = domainCod},
                Just TyArrow {tnDom = codomainDom, tnCod = codomainCod}
                ) -> do
                  rootEvidence <-
                    ownedNodeEvidence "arrow" domainNode codomainNode
                  domEvidence <-
                    go
                      domainEnv
                      codomainEnv
                      sourceDomain
                      domainDom
                      codomainDom
                  codEvidence <-
                    go
                      domainEnv
                      codomainEnv
                      sourceCodomain
                      domainCod
                      codomainCod
                  mergeEvidenceList [rootEvidence, domEvidence, codEvidence]
              pair ->
                Left ("arrow copies did not match source shape: " ++ show pair)
        Surf.STBase expectedName ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyBase {tnBase = BaseTy domainName},
                Just TyBase {tnBase = BaseTy codomainName}
                )
                  | domainName == expectedName
                      && codomainName == expectedName ->
                      ownedNodeEvidence "base" domainNode codomainNode
              pair ->
                Left
                  ( "base copies did not match "
                      ++ expectedName
                      ++ ": "
                      ++ show pair
                  )
        Surf.STCon expectedName sourceArgs ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyCon {tnCon = BaseTy domainName, tnArgs = domainArgs},
                Just TyCon {tnCon = BaseTy codomainName, tnArgs = codomainArgs}
                )
                  | domainName == expectedName
                      && codomainName == expectedName -> do
                      rootEvidence <-
                        ownedNodeEvidence "constructor" domainNode codomainNode
                      argsEvidence <-
                        validateChildren
                          domainEnv
                          codomainEnv
                          (NE.toList sourceArgs)
                          (NE.toList domainArgs)
                          (NE.toList codomainArgs)
                      mergeEvidence rootEvidence argsEvidence
              pair ->
                Left
                  ( "constructor copies did not match "
                      ++ expectedName
                      ++ ": "
                      ++ show pair
                  )
        Surf.STVarApp headName sourceArgs ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyVarApp {tnVarHead = domainHead, tnArgs = domainArgs},
                Just TyVarApp {tnVarHead = codomainHead, tnArgs = codomainArgs}
                ) -> do
                  rootEvidence <-
                    ownedNodeEvidence
                      "variable-headed application"
                      domainNode
                      codomainNode
                  headEvidence <-
                    validateVariable
                      domainEnv
                      codomainEnv
                      headName
                      domainHead
                      codomainHead
                  argsEvidence <-
                    validateChildren
                      domainEnv
                      codomainEnv
                      (NE.toList sourceArgs)
                      (NE.toList domainArgs)
                      (NE.toList codomainArgs)
                  mergeEvidenceList
                    [rootEvidence, headEvidence, argsEvidence]
              pair ->
                Left
                  ( "variable-headed application copies did not match source shape: "
                      ++ show pair
                  )
        Surf.STTyLam {} ->
          Left "residual type lambda reached the O08 graph oracle"
        Surf.STTyApp {} ->
          Left "residual type application reached the O08 graph oracle"
        Surf.STForall name mbSourceBound sourceBody
          | annotationGraphicRootVariable sourceBody == Just name ->
              case mbSourceBound of
                Nothing ->
                  go
                    domainEnv
                    codomainEnv
                    Surf.STBottom
                    domainNode
                    codomainNode
                Just sourceBound ->
                  go
                    domainEnv
                    codomainEnv
                    (Surf.unSrcBound sourceBound)
                    domainNode
                    codomainNode
          | Set.notMember name (annotationGraphicFreeVars sourceBody) ->
              go
                domainEnv
                codomainEnv
                sourceBody
                domainNode
                codomainNode
          | otherwise ->
            case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyForall {tnBody = domainBody},
                Just TyForall {tnBody = codomainBody}
                ) -> do
                  identity <-
                    case Map.lookup name binderIdentities of
                      Just found -> Right found
                      Nothing ->
                        Left
                          ("missing generated identity for forall binder " ++ name)
                  domainBinder <-
                    findLexicalBinder
                      identity
                      domainNode
                      BindRigid
                  codomainBinder <-
                    findLexicalBinder
                      identity
                      codomainNode
                      BindFlex
                  ownerEvidence <-
                    ownedNodeEvidence "forall owner" domainNode codomainNode
                  binderEvidence <-
                    ownedNodeEvidence
                      ("forall binder " ++ name)
                      domainBinder
                      codomainBinder
                  boundEvidence <-
                    validateBound
                      domainEnv
                      codomainEnv
                      mbSourceBound
                      domainBinder
                      codomainBinder
                  bodyEvidence <-
                    go
                      (Map.insert name domainBinder domainEnv)
                      (Map.insert name codomainBinder codomainEnv)
                      sourceBody
                      domainBody
                      codomainBody
                  mergeEvidenceList
                    [ ownerEvidence,
                      binderEvidence,
                      boundEvidence,
                      bodyEvidence
                    ]
              pair ->
                Left
                  ("forall copies did not match source shape: " ++ show pair)
        Surf.STMu name sourceBody ->
          case
            ( lookupNodeIn nodes domainNode,
              lookupNodeIn nodes codomainNode
            )
          of
              ( Just TyMu {tnBody = domainBody},
                Just TyMu {tnBody = codomainBody}
                ) -> do
                  identity <-
                    case Map.lookup name binderIdentities of
                      Just found -> Right found
                      Nothing ->
                        Left
                          ("missing generated identity for mu binder " ++ name)
                  ownerEvidence <-
                    ownedNodeEvidence "mu owner" domainNode codomainNode
                  expectSourceIdentity "domain mu owner" identity domainNode
                  expectSourceIdentity "codomain mu owner" identity codomainNode
                  if Set.member name (annotationGraphicFreeVars sourceBody)
                    then do
                      domainBinder <-
                        findLexicalBinder
                          identity
                          domainNode
                          BindRigid
                      codomainBinder <-
                        findLexicalBinder
                          identity
                          codomainNode
                          BindFlex
                      binderEvidence <-
                        ownedNodeEvidence
                          ("mu binder " ++ name)
                          domainBinder
                          codomainBinder
                      bodyEvidence <-
                        go
                          (Map.insert name domainBinder domainEnv)
                          (Map.insert name codomainBinder codomainEnv)
                          sourceBody
                          domainBody
                          codomainBody
                      mergeEvidenceList
                        [ownerEvidence, binderEvidence, bodyEvidence]
                    else do
                      domainBinder <-
                        findGenOwnedLexicalBinder identity BindRigid
                      codomainBinder <-
                        findGenOwnedLexicalBinder identity BindFlex
                      binderEvidence <-
                        ownedNodeEvidence
                          ("vacuous mu binder " ++ name)
                          domainBinder
                          codomainBinder
                      bodyEvidence <-
                        go
                          domainEnv
                          codomainEnv
                          sourceBody
                          domainBody
                          codomainBody
                      mergeEvidenceList
                        [ownerEvidence, binderEvidence, bodyEvidence]
              pair ->
                Left ("mu copies did not match source shape: " ++ show pair)
        Surf.STBottom ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyVar {tnBound = Nothing},
                Just TyVar {tnBound = Nothing}
                ) ->
                  ownedNodeEvidence "bottom" domainNode codomainNode
              pair ->
                Left ("bottom copies did not match source shape: " ++ show pair)

    validateChildren ::
      forall n.
      Map.Map String NodeId ->
      Map.Map String NodeId ->
      [Surf.SrcTy n 'Surf.TopVarAllowed] ->
      [NodeId] ->
      [NodeId] ->
      Either String CoercionCopyEvidence
    validateChildren domainEnv codomainEnv sourceChildren domainChildren codomainChildren = do
      requireEvidence
        ( length sourceChildren == length domainChildren
            && length sourceChildren == length codomainChildren
        )
        ( "copy child arity mismatch: "
            ++ show
              ( length sourceChildren,
                length domainChildren,
                length codomainChildren
              )
        )
      mergeEvidenceList
        =<< sequence
          [ go
              domainEnv
              codomainEnv
              sourceChild
              domainChild
              codomainChild
            | (sourceChild, domainChild, codomainChild) <-
                zip3 sourceChildren domainChildren codomainChildren
          ]

    validateVariable domainEnv codomainEnv name domainNode codomainNode =
      case (Map.lookup name domainEnv, Map.lookup name codomainEnv) of
        (Just expectedDomain, Just expectedCodomain) -> do
          requireEvidence
            (domainNode == expectedDomain)
            ( "domain occurrence of "
                ++ name
                ++ " did not use its lexical binder: "
                ++ show (domainNode, expectedDomain)
            )
          requireEvidence
            (codomainNode == expectedCodomain)
            ( "codomain occurrence of "
                ++ name
                ++ " did not use its lexical binder: "
                ++ show (codomainNode, expectedCodomain)
            )
          requireTyVar name domainNode
          requireTyVar name codomainNode
          Right emptyCoercionCopyEvidence
        (Nothing, Nothing) -> do
          requireEvidence
            (domainNode == codomainNode)
            ( "free annotation variable was not shared: "
                ++ name
                ++ " -> "
                ++ show (domainNode, codomainNode)
            )
          case lookupNodeIn nodes domainNode of
            Just TyVar {tnBound = Nothing} ->
              Right
                emptyCoercionCopyEvidence
                  { coercionEvidenceFreeNodes =
                      Map.singleton name domainNode
                  }
            other ->
              Left
                ( "free annotation variable was not an unbounded TyVar: "
                    ++ name
                    ++ " -> "
                    ++ show other
                )
        pair ->
          Left
            ( "source binder environment disagreed between copies for "
                ++ name
                ++ ": "
                ++ show pair
            )

    validateBound ::
      forall n.
      Map.Map String NodeId ->
      Map.Map String NodeId ->
      Maybe (Surf.SrcBound n) ->
      NodeId ->
      NodeId ->
      Either String CoercionCopyEvidence
    validateBound domainEnv codomainEnv mbSourceBound domainBinder codomainBinder =
      case
          ( mbSourceBound,
            lookupNodeIn nodes domainBinder,
            lookupNodeIn nodes codomainBinder
          )
        of
          (Nothing, Just TyVar {tnBound = Nothing}, Just TyVar {tnBound = Nothing}) ->
            Right emptyCoercionCopyEvidence
          ( Just sourceBound,
            Just TyVar {tnBound = Just domainBound},
            Just TyVar {tnBound = Just codomainBound}
            ) ->
              go
                domainEnv
                codomainEnv
                (Surf.unSrcBound sourceBound)
                domainBound
                codomainBound
          triple ->
            Left
              ( "forall bound copies did not match source presence: "
                  ++ show triple
              )

    requireTyVar name node =
      case lookupNodeIn nodes node of
        Just TyVar {} -> Right ()
        other ->
          Left
            ( "occurrence of "
                ++ name
                ++ " was not a TyVar: "
                ++ show other
            )

    findLexicalBinder identity owner expectedFlag =
      case
          [ NodeId key
            | (key, nodeIdentity) <-
                IntMap.toList (crSourceTypeBinderIdentities result),
              nodeIdentity == identity,
              Just TyVar {} <- [lookupNodeIn nodes (NodeId key)],
              IntMap.lookup
                (nodeRefKey (typeRef (NodeId key)))
                (cBindParents constraint)
                == Just (typeRef owner, expectedFlag)
          ]
        of
          [binder] -> Right binder
          candidates ->
            Left
              ( "expected exactly one lexical binder under "
                  ++ show owner
                  ++ ", saw "
                  ++ show candidates
              )

    expectSourceIdentity description identity node =
      requireEvidence
        ( IntMap.lookup
            (getNodeId node)
            (crSourceTypeBinderIdentities result)
            == Just identity
        )
        (description ++ " did not retain its semantic source identity")

    findGenOwnedLexicalBinder identity expectedFlag =
      case
          [ NodeId key
            | (key, nodeIdentity) <-
                IntMap.toList (crSourceTypeBinderIdentities result),
              nodeIdentity == identity,
              Just TyVar {} <- [lookupNodeIn nodes (NodeId key)],
              Just (GenRef _, actualFlag) <-
                [ IntMap.lookup
                    (nodeRefKey (typeRef (NodeId key)))
                    (cBindParents constraint)
                ],
              actualFlag == expectedFlag
          ]
        of
          [binder] -> Right binder
          candidates ->
            Left
              ( "expected one gen-owned vacuous mu binder with flag "
                  ++ show expectedFlag
                  ++ ", saw "
                  ++ show candidates
              )

    ownedNodeEvidence description domainNode codomainNode = do
      requireEvidence
        (domainNode /= codomainNode)
        (description ++ " was shared between coercion copies: " ++ show domainNode)
      expectDomainNodeKind description domainNode
      expectNodeKind description codomainNode Binding.NodeInstantiable
      Right
        emptyCoercionCopyEvidence
          { coercionEvidenceDomainOwned =
              IntSet.singleton (getNodeId domainNode),
            coercionEvidenceCodomainOwned =
              IntSet.singleton (getNodeId codomainNode)
          }

    expectDomainNodeKind description node =
      case Binding.nodeKind constraint (typeRef node) of
        Right Binding.NodeRestricted -> Right ()
        Right Binding.NodeLocked -> Right ()
        Right actual ->
          Left
            ( description
                ++ " remained instantiable in the rigid domain: "
                ++ show actual
            )
        Left err ->
          Left
            ( description
                ++ " had no valid node kind: "
                ++ show err
            )

    expectNodeKind description node expected =
      case Binding.nodeKind constraint (typeRef node) of
        Right actual
          | actual == expected -> Right ()
          | otherwise ->
              Left
                ( description
                    ++ " had node kind "
                    ++ show actual
                    ++ ", expected "
                    ++ show expected
                )
        Left err ->
          Left
            ( description
                ++ " had no valid node kind: "
                ++ show err
            )

mergeEvidenceList ::
  [CoercionCopyEvidence] ->
  Either String CoercionCopyEvidence
mergeEvidenceList =
  foldM mergeEvidence emptyCoercionCopyEvidence

mergeEvidence ::
  CoercionCopyEvidence ->
  CoercionCopyEvidence ->
  Either String CoercionCopyEvidence
mergeEvidence left right = do
  let leftFree = coercionEvidenceFreeNodes left
      rightFree = coercionEvidenceFreeNodes right
      conflicts =
        [ (name, leftNode, rightNode)
          | (name, leftNode) <- Map.toList leftFree,
            Just rightNode <- [Map.lookup name rightFree],
            leftNode /= rightNode
        ]
  requireEvidence
    (null conflicts)
    ("free annotation variable occurrences were not shared: " ++ show conflicts)
  Right
    CoercionCopyEvidence
      { coercionEvidenceFreeNodes = Map.union leftFree rightFree,
        coercionEvidenceDomainOwned =
          IntSet.union
            (coercionEvidenceDomainOwned left)
            (coercionEvidenceDomainOwned right),
        coercionEvidenceCodomainOwned =
          IntSet.union
            (coercionEvidenceCodomainOwned left)
            (coercionEvidenceCodomainOwned right)
      }

requireEvidence :: Bool -> String -> Either String ()
requireEvidence condition message =
  if condition
    then Right ()
    else Left message

propReifyInline :: Int -> Property
propReifyInline _size =
  elaboratesTo (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) intTy

propInlinePred :: Int -> Property
propInlinePred _size =
  let inlineable :: Elab.ElabType
      inlineable =
        testTForall
          "a"
          (Just (boundFromType intTy))
          (Elab.TArrow (testTVar "a") boolTy)
      inlined :: Elab.ElabType
      inlined = Elab.TArrow intTy boolTy
      selfBound :: Elab.ElabType
      selfBound =
        testTForall
          "a"
          (Just (Elab.TArrow (testTVar "a") intTy))
          (Elab.TArrow (testTVar "a") boolTy)
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
            [ lookupNodeIn (cNodes c) bound === Just (TestTyBase bound (BaseTy "Int")),
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
      assertMinimalDecision "compose-polytype" cCompose expCompose targetForall2 $ \(expansion, unifications) ->
        conjoin
          [ expansion
              === ExpCompose
                ( ExpInstantiate [targetArrowC]
                    :| [ExpForall (ForallSpec [Nothing, Nothing] :| [])]
                ),
            unifications === []
          ],
      assertMinimalDecision "forall-intro" cForallIntro expForallIntro targetForallIntro $ \(expansion, unifications) ->
        conjoin
          [ expansion === ExpForall (ForallSpec [Nothing, Nothing] :| []),
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
                [ (getNodeId bodyId, TestTyBase bodyId (BaseTy "Int")),
                  (getNodeId targetId, TestTyBase targetId (BaseTy "Int")),
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
                  (getNodeId targetDom, TestTyBase targetDom (BaseTy "Int")),
                  (getNodeId targetCod, TestTyBase targetCod (BaseTy "Int")),
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
                [ (getNodeId srcDomF, TestTyBase srcDomF (BaseTy "Int")),
                  (getNodeId srcCodF, TestTyBase srcCodF (BaseTy "Bool")),
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
    Right presolution ->
      let c = prConstraint presolution
          entries = IntMap.toList (prEdgeWitnesses presolution)
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
propCopyScheme size =
  let base = size * 100
      bound = NodeId (base + 1)
      sharedArrow = NodeId (base + 5)
      body = NodeId (base + 6)
      fresh = NodeId (base + 10)
      c =
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId bound, TyVar {tnId = bound, tnBound = Nothing}),
                    (getNodeId sharedArrow, TyArrow sharedArrow bound bound),
                    (getNodeId body, TyArrow body sharedArrow sharedArrow),
                    (getNodeId fresh, TyVar {tnId = fresh, tnBound = Nothing})
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (bound, sharedArrow, BindFlex),
                    (sharedArrow, body, BindFlex)
                  ]
            }
      st0 = emptyPresolutionState c
   in case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
        Right (root, st1) ->
          let c1 = psConstraint st1
              nodes = cNodes c1
              expectedSourceBody = TyArrow body sharedArrow sharedArrow
           in case lookupNodeIn nodes root of
                Just TyArrow {tnDom = dom, tnCod = cod} ->
                  conjoin
                    [ counterexample "scheme root is copied to a fresh node" (root /= body),
                      counterexample "shared body child is copied once and reused" (dom == cod && dom /= sharedArrow),
                      counterexample "source body remains unchanged" (lookupNodeIn nodes body == Just expectedSourceBody),
                      case lookupNodeIn nodes dom of
                        Just TyArrow {tnDom = innerDom, tnCod = innerCod} ->
                          conjoin
                            [ counterexample "substituted binder is used in copied domain" (innerDom == fresh),
                              counterexample "substituted binder is used in copied codomain" (innerCod == fresh)
                            ]
                        other -> counterexample ("expected copied shared arrow, got " ++ show other) False,
                      counterexample "fresh substitution node remains live" (isJust (lookupNodeIn nodes fresh)),
                      Binding.checkBindingTree c1 === Right ()
                    ]
                other -> counterexample ("expected copied scheme root arrow, got " ++ show other) False
        Left err -> counterexample (show err) False

witnessChainFixture
  :: Int
  -> (OmegaNormalizeEnv 'Raw, NodeId, [NodeId], NodeId)
witnessChainFixture requestedSize =
  let chainSize = max 1 requestedSize
      root = NodeId 0
      parent = NodeId 1
      children = map NodeId [2 .. chainSize + 1]
      sibling = NodeId (chainSize + 2)
      allNodes = root : parent : children ++ [sibling]
      c =
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId node, TyVar {tnId = node, tnBound = Nothing})
                  | node <- allNodes
                  ],
              cBindParents =
                bindParentsFromPairs
                  ( (parent, root, BindFlex)
                      : (sibling, root, BindFlex)
                      : [ (child, parent, BindFlex)
                        | child <- children
                        ]
                  )
            }
      env =
        mkNormalizeEnv
          c
          root
          (IntSet.fromList (map getNodeId allNodes))
   in (env, parent, children, sibling)

propWitnessNorm :: Int -> Property
propWitnessNorm size =
  let (env, parent, children, _sibling) = witnessChainFixture size
      duplicatedRaises =
        concat
          [ replicate (1 + getNodeId child `mod` 3) (OpRaise child)
          | child <- children
          ]
      input = OpWeaken parent : duplicatedRaises
      expected = map OpRaise children ++ [OpWeaken parent]
   in case normalizeInstanceOpsFull env input of
        Left err -> counterexample (show err) False
        Right validated ->
          let normalized = getValidatedInstanceOps validated
           in conjoin
                [ counterexample "normalization did not delay Weaken or remove duplicate Raises" $
                    normalized === expected,
                  counterexample "the certified output does not satisfy Definition 11.5.2" $
                    validateNormalizedWitness env normalized === Right (),
                  counterexample "certified normalization is not idempotent" $
                    normalizeInstanceOpsForTest env normalized === Right normalized
                ]

propWitnessCoalesce :: Int -> Property
propWitnessCoalesce size =
  let (env0, operated, _children, exterior) = witnessChainFixture size
      env =
        env0
          { interior =
              IntSet.delete
                (getNodeId exterior)
                (interior env0)
          }
      input = replicate (max 1 size) (OpRaise operated) ++ [OpMerge operated exterior]
   in coalesceRaiseMergeWithEnv env input === Right [OpRaiseMerge operated exterior]

propWitnessReorder :: Int -> Property
propWitnessReorder size =
  let (env, parent, children, sibling) = witnessChainFixture size
      input =
        OpRaise sibling
          : OpWeaken parent
          : concatMap
              (\child -> [OpRaise child, OpRaise sibling])
              children
      nonWeakens = filter (/= OpWeaken parent) input
      lastChildIndex =
        maximum
          [ index
          | (index, OpRaise node) <- zip [0 :: Int ..] nonWeakens
          , node `elem` children
          ]
      (prefix, suffix) = splitAt (lastChildIndex + 1) nonWeakens
      expected = prefix ++ [OpWeaken parent] ++ suffix
   in reorderWeakenWithEnv env input === Right expected

propAcyclicCheck :: Int -> Property
propAcyclicCheck size =
  let c = acyclicConstraint size
   in case checkAcyclicityRaw c of
        Right result -> counterexample (show result) (not (null (arSortedEdges result)))
        Left err -> counterexample (show err) False

propAcyclicTopo :: Int -> Property
propAcyclicTopo size =
  let c = acyclicConstraint size
   in case checkAcyclicityRaw c of
        Right result -> arSortedEdges result === [InstEdge (EdgeId size) (NodeId 0) (NodeId 2)]
        Left err -> counterexample (show err) False

propCopyInst :: Int -> Property
propCopyInst size =
  let base = size * 100
      binder = NodeId (base + 1)
      outerVar = NodeId (base + 2)
      frontierArrow = NodeId (base + 3)
      bodyArrow = NodeId (base + 4)
      forallNode = NodeId (base + 5)
      expNode = NodeId (base + 6)
      meta = NodeId (base + 10)
      c =
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId binder, TyVar {tnId = binder, tnBound = Nothing}),
                    (getNodeId outerVar, TyVar {tnId = outerVar, tnBound = Nothing}),
                    (getNodeId frontierArrow, TyArrow frontierArrow outerVar outerVar),
                    (getNodeId bodyArrow, TyArrow bodyArrow frontierArrow binder),
                    (getNodeId forallNode, TyForall forallNode bodyArrow),
                    (getNodeId expNode, TyExp expNode (ExpVarId base) forallNode),
                    (getNodeId meta, TyVar {tnId = meta, tnBound = Nothing})
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (forallNode, expNode, BindFlex),
                    (bodyArrow, forallNode, BindFlex),
                    (binder, bodyArrow, BindFlex),
                    (frontierArrow, expNode, BindFlex),
                    (outerVar, frontierArrow, BindFlex)
                  ]
            }
      st0 = emptyPresolutionState c
      directCopy =
        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow [(binder, meta)]) of
          Right ((root, copyMap, interior, frontier), st1) ->
            let c1 = psConstraint st1
                nodes = cNodes c1
             in case lookupNodeIn nodes root of
                  Just TyArrow {tnDom = dom, tnCod = cod} ->
                    conjoin
                      [ counterexample "Inst-Copy root is freshly copied" (root /= bodyArrow),
                        lookupCopy bodyArrow copyMap === Just root,
                        lookupCopy binder copyMap === Just meta,
                        lookupCopy frontierArrow copyMap === Just dom,
                        counterexample "binder argument is substituted into the copied codomain" (cod == meta),
                        counterexample "frontier source is recorded in the frontier set" $
                          IntSet.member (getNodeId frontierArrow) frontier,
                        counterexample "fresh copied root is recorded in trace interior" $
                          IntSet.member (getNodeId root) interior,
                        counterexample "binder argument is recorded in trace interior" $
                          IntSet.member (getNodeId meta) interior,
                        case lookupNodeIn nodes dom of
                          Just TyBottom {tnId = bottomId} ->
                            counterexample "frontier copy is replaced with bottom" (bottomId == dom)
                          other -> counterexample ("expected bottom frontier copy, got " ++ show other) False,
                        counterexample "trace copy map records source-to-copy bookkeeping" $
                          not (IntMap.null (getCopyMapping copyMap)),
                        Binding.checkBindingTree c1 === Right ()
                      ]
                  other -> counterexample ("expected copied Inst-Copy arrow, got " ++ show other) False
          Left err -> counterexample (show err) False
      recordedTrace =
        let edgeId = EdgeId (base + 20)
            edgeSourceBinder = NodeId (base + 21)
            edgeSourceArrow = NodeId (base + 22)
            edgeSourceForall = NodeId (base + 23)
            edgeTargetDom = NodeId (base + 24)
            edgeTargetCod = NodeId (base + 25)
            edgeTargetArrow = NodeId (base + 26)
            edgeExp = NodeId (base + 27)
            edge =
              InstEdge
                edgeId
                edgeExp
                edgeTargetArrow
            edgeConstraint =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId edgeSourceBinder, TyVar {tnId = edgeSourceBinder, tnBound = Nothing}),
                          (getNodeId edgeSourceArrow, TyArrow edgeSourceArrow edgeSourceBinder edgeSourceBinder),
                          (getNodeId edgeSourceForall, TyForall edgeSourceForall edgeSourceArrow),
                          (getNodeId edgeTargetDom, TestTyBase edgeTargetDom (BaseTy "Int")),
                          (getNodeId edgeTargetCod, TestTyBase edgeTargetCod (BaseTy "Int")),
                          (getNodeId edgeTargetArrow, TyArrow edgeTargetArrow edgeTargetDom edgeTargetCod),
                          (getNodeId edgeExp, TyExp edgeExp (ExpVarId (base + 28)) edgeSourceForall)
                        ],
                    cBindParents =
                      bindParentsFromPairs
                        [ (edgeSourceBinder, edgeSourceForall, BindFlex),
                          (edgeSourceArrow, edgeSourceForall, BindFlex),
                          (edgeSourceForall, edgeExp, BindFlex),
                          (edgeTargetDom, edgeTargetArrow, BindFlex),
                          (edgeTargetCod, edgeTargetArrow, BindFlex)
                        ]
                  }
            edgeSt0 = emptyPresolutionState edgeConstraint
         in case runPresolutionM defaultTraceConfig edgeSt0 (processInstEdge edge) of
              Right (_, edgeSt1) ->
                let traces = psEdgeTraces edgeSt1
                 in case IntMap.lookup (getEdgeId edgeId) traces of
                      Just tr ->
                        conjoin
                          [ counterexample ("empty binder args in trace: " ++ show tr) $
                              not (null (etBinderArgs tr)),
                            edgeTraceCopyEvidence (psConstraint edgeSt1) tr
                          ]
                      Nothing -> counterexample ("missing trace keys: " ++ show (IntMap.keys traces)) False
              Left err -> counterexample (show err) False
   in conjoin [directCopy, recordedTrace]

edgeTraceCopyEvidence :: Constraint 'Raw -> EdgeTrace -> Property
edgeTraceCopyEvidence c tr =
  let copyPairs = IntMap.toList (getCopyMapping (etCopyMap tr))
      binderPairs = etBinderArgs tr
   in conjoin
        [ counterexample "trace root is live in the presolved constraint" $
            isJust (lookupNodeIn (cNodes c) (etRoot tr)),
          counterexample ("empty trace copy map for " ++ show binderPairs) (not (null copyPairs)),
          counterexample ("binder sources are absent from the trace copy map: " ++ show (binderPairs, copyPairs)) $
            all (\(binder, _arg) -> IntMap.member (getNodeId binder) (getCopyMapping (etCopyMap tr))) binderPairs,
          counterexample ("binder arguments are not live: " ++ show binderPairs) $
            all (\(_binder, arg) -> isJust (lookupNodeIn (cNodes c) arg)) binderPairs,
          counterexample ("copy map targets are not live: " ++ show copyPairs) $
            all (\(_source, copied) -> isJust (lookupNodeIn (cNodes c) copied)) copyPairs
        ]

propNormGraft :: Int -> Property
propNormGraft size =
  let graftBase = BaseTy ("Graft" ++ show size)
      c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                    (1, TestTyBase (NodeId 1) graftBase)
                  ],
              cBindParents = bindParentsFromPairs [(NodeId 1, NodeId 0, BindFlex)],
              cInstEdges = [InstEdge (EdgeId size) (NodeId 0) (NodeId 1)]
            }
      normalized = normalizeRaw c
   in conjoin
        [ cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          counterexample "normalization lost the identity provenance of the grafted edge" $
            IntSet.member size (cGraftedEdges normalized),
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TestTyBase (NodeId 0) graftBase),
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
                  (1, TestTyBase (NodeId 1) mergeBase)
                ],
            cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
          }
      normalized = normalizeRaw c
   in conjoin
        [ cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TestTyBase (NodeId 0) mergeBase)
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
      normalized = normalizeRaw c
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
                  (2, TestTyBase (NodeId 2) fixpointBase)
                ],
            cInstEdges =
              [ InstEdge (EdgeId size) (NodeId 0) (NodeId 1),
                InstEdge (EdgeId (size + 1)) (NodeId 1) (NodeId 2)
              ]
          }
      normalized = normalizeRaw c
   in conjoin
        [ normalized === normalizeRaw normalized,
          cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TestTyBase (NodeId 0) fixpointBase),
          lookupNodeIn (cNodes normalized) (NodeId 1) === Just (TestTyBase (NodeId 1) fixpointBase)
        ]

propSolveVarBase :: Int -> Property
propSolveVarBase _size =
  let c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes = nodeMapFromList [(0, TestTyCon (NodeId 0) (BaseTy "Box") (NodeId 1 :| [])), (1, TyVar (NodeId 1) Nothing), (2, TestTyBase (NodeId 2) (BaseTy "Int"))],
              cBindParents = bindParentsFromPairs [(NodeId 1, NodeId 0, BindFlex), (NodeId 2, NodeId 0, BindFlex)],
              cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 2)]
            }
   in case solveUnifyRaw defaultTraceConfig c of
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
  case solveUnifyRaw defaultTraceConfig varTripleConstraint of
    Right SolveResult {srConstraint = solved} -> Binding.checkBindingTree solved === Right ()
    Left err -> counterexample (show err) False

applyShouldBe :: Elab.ElabType -> Elab.Instantiation -> Elab.ElabType -> Property
applyShouldBe ty inst expected =
  case Elab.applyInstantiation ty inst of
    Right actual -> actual === expected
    Left err -> counterexample (show err) False

elaboratesTo :: Surf.SurfaceExpr -> Elab.ElabType -> Property
elaboratesTo expr expected =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Right (term, ty) ->
      conjoin
        [ typeShouldMatch ty expected,
          typeCheckShouldMatch (Elab.typeCheck term) expected
        ]
    Left err -> counterexample (Elab.renderPipelineError err) False

typeShouldMatch :: Elab.ElabType -> Elab.ElabType -> Property
typeShouldMatch actual expected =
  counterexample (show actual ++ " /= " ++ show expected) $
    TypeOps.alphaEqType actual expected

typeCheckShouldMatch :: Either Elab.TypeCheckError Elab.ElabType -> Elab.ElabType -> Property
typeCheckShouldMatch actual expected =
  case actual of
    Right ty -> typeShouldMatch ty expected
    Left err -> counterexample (show err) False

propPresolutionClearsEdges :: Surf.SurfaceExpr -> Property
propPresolutionClearsEdges expr =
  case runToPresolutionDefault Set.empty expr of
    Right presolution ->
      let c = prConstraint presolution
       in conjoin
            [ Binding.checkBindingTree c === Right (),
              cInstEdges c === []
            ]
    Left err -> counterexample err False

propEdgeWitnessOps :: Surf.SurfaceExpr -> ([EdgeWitness] -> Bool) -> Property
propEdgeWitnessOps expr predicate =
  case runToPresolutionDefault Set.empty expr of
    Right presolution ->
      let values = IntMap.elems (prEdgeWitnesses presolution)
       in counterexample (show values) (predicate values)
    Left err -> counterexample err False

acyclicConstraint :: Int -> Constraint 'Raw
acyclicConstraint size =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TestTyBase (NodeId 2) (BaseTy "Int"))
            ],
        cInstEdges = [InstEdge (EdgeId size) (NodeId 0) (NodeId 2)]
      }

flexibleSchemeRootConstraint :: Constraint 'Raw
flexibleSchemeRootConstraint =
  let rootGen = GenNodeId 0
      schemeRoot = NodeId 0
   in rootedConstraint
        emptyConstraint
          { cNodes = nodeMapFromList [(0, TyVar {tnId = schemeRoot, tnBound = Nothing})],
            cBindParents = IntMap.fromList [(nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))],
            cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
          }

flexibleArrowConstraint :: Constraint 'Raw
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

flexibleNonInteriorConstraint :: Constraint 'Raw
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
forallA = testTForall "a" Nothing (testTVar "a")

polyIdTy :: Elab.ElabType
polyIdTy = testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))

letIdAppExpr :: Surf.SurfaceExpr
letIdAppExpr =
  Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x")) (Surf.EApp (Surf.EVar "id") (Surf.ELit (Surf.LInt 1)))

boundFromType :: Elab.ElabType -> Elab.BoundType
boundFromType ty =
  case ty of
    Elab.TVarRef ref -> error ("boundFromType: unexpected variable bound " ++ show ref)
    Elab.TArrow a b -> Elab.TArrow a b
    Elab.TConWithIdentity _ c args -> TestElab.tCon c args
    Elab.TVarAppRef ref args -> Elab.TVarAppRef ref args
    Elab.TBaseWithIdentity _ b -> TestElab.tBase b
    Elab.TBottom -> Elab.TBottom
    Elab.TForallRef ref mb body -> Elab.TForallRef ref mb body
    Elab.TMuRef ref body -> Elab.TMuRef ref body

emptyPresolutionState :: Constraint 'Raw -> PresolutionState 'Raw
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

identityPresolutionView :: Constraint 'Raw -> PresolutionView 'Raw
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
  Constraint 'Raw ->
  NodeId ->
  NodeId ->
  ((Expansion, [(NodeId, NodeId)]) -> Property) ->
  Property
assertMinimalDecision caseName c expNodeId targetNodeId checkDecision =
  case decideMinimalFor c expNodeId targetNodeId of
    Right decision -> counterexample (caseName ++ ": " ++ show decision) (checkDecision decision)
    Left err -> counterexample (caseName ++ ": " ++ err) False

decideMinimalFor :: Constraint 'Raw -> NodeId -> NodeId -> Either String (Expansion, [(NodeId, NodeId)])
decideMinimalFor c expNodeId targetNodeId =
  case (lookupNodeIn (cNodes c) expNodeId, lookupNodeIn (cNodes c) targetNodeId) of
    (Just expNode, Just targetNode) ->
      case runPresolutionM defaultTraceConfig (emptyPresolutionState c) (decideMinimalExpansion id (GenNodeId 0) True expNode targetNode) of
        Right (decision, _st) -> Right decision
        Left err -> Left (show err)
    (Nothing, _) -> Left ("missing expansion node " ++ show expNodeId)
    (_, Nothing) -> Left ("missing target node " ++ show targetNodeId)

assertNodeAliasTranslation :: Int -> (NodeId -> NodeId -> InstanceOp) -> Property
assertNodeAliasTranslation size mkOp =
  let (c, root, binderA, binderB, scheme, si, tr) = nodeAliasTranslationFixture size
      refA = elabTypeRef (getNodeId binderA) "a"
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
        Elab.TForallRef
          refA
          Nothing
          (Elab.TArrow (Elab.TVarRef refA) (Elab.TVarRef refA))
      generalizeAt _ _ _ =
        Left (Elab.InstantiationError "assertNodeAliasTranslation: unexpected generalization")
   in case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig generalizeAt (identityPresolutionView c) Nothing (Just si) (Just tr) ew of
        Left err -> counterexample (show err) False
        Right phi ->
          case Elab.applyInstantiation (Elab.schemeToType scheme) phi of
            Left err -> counterexample (show err) False
            Right out -> counterexample (Elab.pretty phi ++ " => " ++ Elab.pretty out) (out === expected)

nodeAliasTranslationFixture :: Int -> (Constraint 'Raw, NodeId, NodeId, NodeId, Elab.ElabScheme, Elab.SchemeInfo, EdgeTrace)
nodeAliasTranslationFixture size =
  (c, root, binderA, binderB, scheme, si, tr)
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    binderA = NodeId (base + 1)
    refA = elabTypeRef (getNodeId binderA) "a"
    forallB = NodeId (base + 102)
    binderB = NodeId (base + 2)
    refB = elabTypeRef (getNodeId binderB) "b"
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
        ( Elab.TForallRef
            refA
            Nothing
            (Elab.TForallRef refB Nothing (Elab.TArrow (Elab.TVarRef refA) (Elab.TVarRef refB)))
        )
    si =
      Elab.schemeInfoFromRefSubst
        scheme
        ( IntMap.fromList
            [ (getNodeId binderA, refA),
              (getNodeId binderB, refB)
            ]
        )
    tr =
      EdgeTrace
        { etRoot = root,
          etResultRoot = root,
          etBinderArgs = [],
          etInterior = sourceInteriorFromList [root, binderA, forallB, binderB, bodyNode],
          etReplayContract = ReplayContractNone,
          etBinderReplayMap = mempty,
          etReplayDomainBinders = [],
          etCopyMap = mempty
        }

orderedBinderFixture :: Int -> (Constraint 'Raw, NodeId, [NodeId])
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

contextFindFixture :: Int -> (Constraint 'Raw, NodeId, NodeId, [Elab.ContextStep])
contextFindFixture size =
  (c, root, cN, [stepUnder aN, Elab.StepInside])
  where
    stepUnder nid =
      Elab.StepUnderRef
        ( ElabTypes.typeBinderRefFromIdentity
            (ElabTypes.typeBinderIdentityFromNode nid)
            ("t" ++ show (getNodeId nid))
        )
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

contextRejectFixture :: Int -> (Constraint 'Raw, NodeId, NodeId)
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

chainConstraint :: Int -> Constraint 'Raw
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

binderConstraint :: Constraint 'Raw
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

varTripleConstraint :: Constraint 'Raw
varTripleConstraint =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TestTyCon (NodeId 0) (BaseTy "Triple") (NodeId 1 :| [NodeId 2, NodeId 3])),
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

rootedConstraintLocal :: Constraint 'Raw -> Constraint 'Raw
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

inertConstraint :: Int -> Constraint 'Raw
inertConstraint size =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 1)),
              (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)),
              (2, TyArrow (NodeId 2) (NodeId 4) (NodeId 3)),
              (3, TestTyBase (NodeId 3) (BaseTy ("Int" ++ show size))),
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
intTy = TestElab.tBase (BaseTy "Int")

builtinIntTy :: Elab.ElabType
builtinIntTy = ElabTypes.TBaseWithIdentity (Builtins.builtinTypeIdentity "Int") (BaseTy "Int")

boolTy :: Elab.ElabType
boolTy = TestElab.tBase (BaseTy "Bool")

elabTypeRef :: Int -> String -> ElabTypes.TypeBinderRef
elabTypeRef key name =
  ElabTypes.typeBinderRefFromIdentity (ElabTypes.typeBinderIdentityFromNode (NodeId key)) name

idLam :: Elab.XmlfTerm
idLam = mkTestLocalLam "x" intTy (mkTestDeferredVar "x")

polyId :: Elab.XmlfTerm
polyId = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
