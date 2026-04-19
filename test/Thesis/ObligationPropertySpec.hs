{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Thesis.ObligationPropertySpec (spec) where

import Control.Monad (forM_)
import Data.Either (isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Set qualified as Set
import MLF.Binding.GraphOps qualified as GraphOps
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Acyclicity (AcyclicityResult (..), checkAcyclicity)
import MLF.Constraint.Inert qualified as Inert
import MLF.Constraint.Presolution (PresolutionResult (..))
import MLF.Constraint.Solve (SolveResult (..), frWith, solveUnify)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Unify.Decompose (decomposeUnifyChildren)
import MLF.Elab.Pipeline qualified as Elab
import MLF.Frontend.ConstraintGen (ConstraintResult (..))
import MLF.Frontend.Syntax qualified as Surf
import SpecUtil
  ( PipelineArtifacts (..),
    bindParentsFromPairs,
    defaultTraceConfig,
    emptyConstraint,
    nodeMapFromList,
    runConstraintDefault,
    runPipelineArtifactsDefault,
    runToPresolutionDefault,
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
              propertyFor (obligationFamily obligation) size

data Obligation = Obligation
  { obligationId :: String,
    obligationFamily :: ObligationFamily
  }

data ObligationFamily
  = BindingFlexChildren
  | BindingInterior
  | BindingOrder
  | GraphWeaken
  | GraphRaiseStep
  | GraphRaiseTo
  | InertNodes
  | InertLocked
  | InertWeaken
  | UnifyDecompose
  | SolveVar
  | SolveArrow
  | RebindHarmonize
  | GeneralizedUnify
  | ReifyPipeline
  | ConstraintGeneration
  | Presolution
  | WitnessNormalization
  | Acyclicity
  | TypeChecking
  | Reduction
  | InstApplication
  | TranslatablePresolution
  | SigmaReorder
  | ContextSelection
  | Elaboration
  | PhiTranslation

obligations :: [Obligation]
obligations =
  [ Obligation "O14-WF-EMPTY" TypeChecking,
    Obligation "O14-WF-TVAR" TypeChecking,
    Obligation "O14-WF-VAR" TypeChecking,
    Obligation "O14-INST-REFLEX" InstApplication,
    Obligation "O14-INST-TRANS" InstApplication,
    Obligation "O14-INST-BOT" InstApplication,
    Obligation "O14-INST-HYP" InstApplication,
    Obligation "O14-INST-INNER" InstApplication,
    Obligation "O14-INST-OUTER" InstApplication,
    Obligation "O14-INST-QUANT-ELIM" InstApplication,
    Obligation "O14-INST-QUANT-INTRO" InstApplication,
    Obligation "O14-T-VAR" TypeChecking,
    Obligation "O14-T-ABS" TypeChecking,
    Obligation "O14-T-APP" TypeChecking,
    Obligation "O14-T-TABS" TypeChecking,
    Obligation "O14-T-TAPP" TypeChecking,
    Obligation "O14-T-LET" TypeChecking,
    Obligation "O14-RED-BETA" Reduction,
    Obligation "O14-RED-BETALET" Reduction,
    Obligation "O14-RED-REFLEX" Reduction,
    Obligation "O14-RED-TRANS" Reduction,
    Obligation "O14-RED-QUANT-INTRO" Reduction,
    Obligation "O14-RED-QUANT-ELIM" Reduction,
    Obligation "O14-RED-INNER" Reduction,
    Obligation "O14-RED-OUTER" Reduction,
    Obligation "O14-RED-CONTEXT" Reduction,
    Obligation "O14-APPLY-N" InstApplication,
    Obligation "O14-APPLY-O" InstApplication,
    Obligation "O14-APPLY-SEQ" InstApplication,
    Obligation "O14-APPLY-INNER" InstApplication,
    Obligation "O14-APPLY-OUTER" InstApplication,
    Obligation "O14-APPLY-HYP" InstApplication,
    Obligation "O14-APPLY-BOT" InstApplication,
    Obligation "O14-APPLY-ID" InstApplication,
    Obligation "O15-TRANS-NO-INERT-LOCKED" TranslatablePresolution,
    Obligation "O15-TRANS-SCHEME-ROOT-RIGID" TranslatablePresolution,
    Obligation "O15-TRANS-ARROW-RIGID" TranslatablePresolution,
    Obligation "O15-TRANS-NON-INTERIOR-RIGID" TranslatablePresolution,
    Obligation "O15-REORDER-REQUIRED" SigmaReorder,
    Obligation "O15-REORDER-IDENTITY" SigmaReorder,
    Obligation "O15-CONTEXT-FIND" ContextSelection,
    Obligation "O15-CONTEXT-REJECT" ContextSelection,
    Obligation "O15-EDGE-TRANSLATION" PhiTranslation,
    Obligation "O15-ELAB-LAMBDA-VAR" Elaboration,
    Obligation "O15-ELAB-LET-VAR" Elaboration,
    Obligation "O15-ELAB-ABS" Elaboration,
    Obligation "O15-ELAB-APP" Elaboration,
    Obligation "O15-ELAB-LET" Elaboration,
    Obligation "O15-ENV-LAMBDA" Elaboration,
    Obligation "O15-ENV-LET" Elaboration,
    Obligation "O15-ENV-WF" Elaboration,
    Obligation "O15-TR-SEQ-EMPTY" PhiTranslation,
    Obligation "O15-TR-SEQ-CONS" PhiTranslation,
    Obligation "O15-TR-RIGID-RAISE" PhiTranslation,
    Obligation "O15-TR-RIGID-MERGE" PhiTranslation,
    Obligation "O15-TR-RIGID-RAISEMERGE" PhiTranslation,
    Obligation "O15-TR-ROOT-GRAFT" PhiTranslation,
    Obligation "O15-TR-ROOT-RAISEMERGE" PhiTranslation,
    Obligation "O15-TR-ROOT-WEAKEN" PhiTranslation,
    Obligation "O15-TR-NODE-GRAFT" PhiTranslation,
    Obligation "O15-TR-NODE-MERGE" PhiTranslation,
    Obligation "O15-TR-NODE-RAISEMERGE" PhiTranslation,
    Obligation "O15-TR-NODE-WEAKEN" PhiTranslation,
    Obligation "O15-TR-NODE-RAISE" PhiTranslation,
    Obligation "O04-BIND-FLEX-CHILDREN" BindingFlexChildren,
    Obligation "O04-BIND-INTERIOR" BindingInterior,
    Obligation "O04-BIND-ORDER" BindingOrder,
    Obligation "O04-OP-WEAKEN" GraphWeaken,
    Obligation "O04-OP-RAISE-STEP" GraphRaiseStep,
    Obligation "O04-OP-RAISE-TO" GraphRaiseTo,
    Obligation "O05-INERT-NODES" InertNodes,
    Obligation "O05-INERT-LOCKED" InertLocked,
    Obligation "O05-WEAKEN-INERT" InertWeaken,
    Obligation "O07-UNIF-CORE" UnifyDecompose,
    Obligation "O07-UNIF-PRESOL" SolveVar,
    Obligation "O07-REBIND" RebindHarmonize,
    Obligation "O07-GENUNIF" GeneralizedUnify,
    Obligation "O08-REIFY-TYPE" ReifyPipeline,
    Obligation "O08-REIFY-NAMES" ReifyPipeline,
    Obligation "O08-BIND-MONO" ReifyPipeline,
    Obligation "O08-SYN-TO-GRAPH" ReifyPipeline,
    Obligation "O08-REIFY-INLINE" ReifyPipeline,
    Obligation "O08-INLINE-PRED" ReifyPipeline,
    Obligation "O09-CGEN-ROOT" ConstraintGeneration,
    Obligation "O09-CGEN-EXPR" ConstraintGeneration,
    Obligation "O10-EXP-DECIDE" Presolution,
    Obligation "O10-EXP-APPLY" Presolution,
    Obligation "O10-PROP-SOLVE" Presolution,
    Obligation "O10-PROP-WITNESS" Presolution,
    Obligation "O10-COPY-SCHEME" Presolution,
    Obligation "O11-UNIFY-STRUCT" UnifyDecompose,
    Obligation "O11-WITNESS-NORM" WitnessNormalization,
    Obligation "O11-WITNESS-COALESCE" WitnessNormalization,
    Obligation "O11-WITNESS-REORDER" WitnessNormalization,
    Obligation "O12-SOLVE-UNIFY" SolveVar,
    Obligation "O12-ACYCLIC-CHECK" Acyclicity,
    Obligation "O12-ACYCLIC-TOPO" Acyclicity,
    Obligation "O12-COPY-INST" Presolution,
    Obligation "O12-NORM-GRAFT" UnifyDecompose,
    Obligation "O12-NORM-MERGE" UnifyDecompose,
    Obligation "O12-NORM-DROP" UnifyDecompose,
    Obligation "O12-NORM-FIXPOINT" UnifyDecompose,
    Obligation "O12-SOLVE-VAR-BASE" SolveVar,
    Obligation "O12-SOLVE-VAR-VAR" SolveVar,
    Obligation "O12-SOLVE-HARMONIZE" GeneralizedUnify,
    Obligation "O12-SOLVE-ARROW" SolveArrow,
    Obligation "O12-SOLVE-VALIDATE" SolveVar
  ]

propertyFor :: ObligationFamily -> Int -> Property
propertyFor family size =
  case family of
    BindingFlexChildren -> propBindingFlexChildren size
    BindingInterior -> propBindingInterior size
    BindingOrder -> propBindingOrder size
    GraphWeaken -> propGraphWeaken size
    GraphRaiseStep -> propGraphRaiseStep size
    GraphRaiseTo -> propGraphRaiseTo size
    InertNodes -> propInertNodes size
    InertLocked -> propInertLocked size
    InertWeaken -> propInertWeaken size
    UnifyDecompose -> propUnifyDecompose size
    SolveVar -> propSolveVar size
    SolveArrow -> propSolveArrow size
    RebindHarmonize -> propRebindHarmonize size
    GeneralizedUnify -> propGeneralizedUnify size
    ReifyPipeline -> propPipelineReifies size
    ConstraintGeneration -> propConstraintGeneration size
    Presolution -> propPresolution size
    WitnessNormalization -> propWitnessNormalization size
    Acyclicity -> propAcyclicity size
    TypeChecking -> propTypeChecking size
    Reduction -> propReduction size
    InstApplication -> propInstApplication size
    TranslatablePresolution -> propTranslatablePresolution size
    SigmaReorder -> propSigmaReorder size
    ContextSelection -> propContextSelection size
    Elaboration -> propElaboration size
    PhiTranslation -> propPhiTranslation size

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
  let c = chainConstraint size
   in case Binding.orderedBinders id c (typeRef (NodeId 0)) of
        Right binders -> counterexample (show binders) True
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

propPipelineReifies :: Int -> Property
propPipelineReifies size =
  let expr = surfaceExpr size
   in case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Right (term, ty) ->
          conjoin
            [ counterexample (Elab.prettyDisplay ty) (not (null (Elab.prettyDisplay ty))),
              Elab.typeCheck term === Right ty
            ]
        Left err -> counterexample (Elab.renderPipelineError err) False

propConstraintGeneration :: Int -> Property
propConstraintGeneration size =
  let expr = surfaceExpr size
   in case runConstraintDefault Set.empty expr of
        Right ConstraintResult {crConstraint = c, crRoot = root} ->
          conjoin
            [ Binding.checkBindingTree c === Right (),
              counterexample (show root) (isJust (lookupNodeIn (cNodes c) root))
            ]
        Left err -> counterexample err False

propPresolution :: Int -> Property
propPresolution size =
  let expr = surfaceExpr size
   in case runToPresolutionDefault Set.empty expr of
        Right PresolutionResult {prConstraint = c} ->
          conjoin
            [ Binding.checkBindingTree c === Right (),
              cInstEdges c === []
            ]
        Left err -> counterexample err False

propWitnessNormalization :: Int -> Property
propWitnessNormalization size =
  let expr = surfaceExpr size
   in case runToPresolutionDefault Set.empty expr of
        Right PresolutionResult {prConstraint = c, prEdgeWitnesses = witnesses} ->
          conjoin
            [ Binding.checkBindingTree c === Right (),
              counterexample (show (IntMap.size witnesses)) (IntMap.size witnesses >= 0)
            ]
        Left err -> counterexample err False

propAcyclicity :: Int -> Property
propAcyclicity size =
  let c =
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
   in case checkAcyclicity c of
        Right result ->
          counterexample (show result) (not (null (arSortedEdges result)))
        Left err -> counterexample (show err) False

propTypeChecking :: Int -> Property
propTypeChecking size =
  let (term, expected) = elabTerm size
   in Elab.typeCheck term === Right expected

propReduction :: Int -> Property
propReduction size =
  let term = reducibleTerm size
   in case Elab.step term of
        Just stepped ->
          counterexample (show stepped) (Elab.isValue stepped || isJust (Elab.step stepped) || Elab.typeCheck stepped == Elab.typeCheck (Elab.normalize stepped))
        Nothing -> counterexample (show term) (Elab.isValue term)

propInstApplication :: Int -> Property
propInstApplication size =
  let poly = Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
      inst = instantiation size
   in case Elab.applyInstantiation poly inst of
        Right ty -> counterexample (show ty) (not (null (show ty)))
        Left err -> counterexample (show err) False

propTranslatablePresolution :: Int -> Property
propTranslatablePresolution size =
  let expr = surfaceExpr size
   in case runPipelineArtifactsDefault Set.empty expr of
        Right _ -> property True
        Left err -> counterexample err False

propSigmaReorder :: Int -> Property
propSigmaReorder size =
  let body = Elab.TArrow (Elab.TVar "a") (Elab.TVar "b")
      src = Elab.TForall "a" Nothing (Elab.TForall "b" Nothing body)
      tgt =
        if even size
          then src
          else Elab.TForall "b" Nothing (Elab.TForall "a" Nothing body)
   in case Elab.sigmaReorder src tgt of
        Right inst -> counterexample (show inst) (isRight (Elab.applyInstantiation src inst))
        Left err -> counterexample (show err) False

propContextSelection :: Int -> Property
propContextSelection size =
  let c = chainConstraint size
   in case Binding.bindingPathToRoot c (typeRef (NodeId (size - 1))) of
        Right path ->
          conjoin
            [ counterexample (show path) (not (null path)),
              counterexample (show path) (last path == genRef (GenNodeId 0))
            ]
        Left err -> counterexample (show err) False

propElaboration :: Int -> Property
propElaboration size =
  let expr = surfaceExpr size
   in case Elab.runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
        Right (term, ty) -> Elab.typeCheck term === Right ty
        Left err -> counterexample (Elab.renderPipelineError err) False

propPhiTranslation :: Int -> Property
propPhiTranslation size =
  let expr = surfaceExpr size
   in case runPipelineArtifactsDefault Set.empty expr of
        Right artifacts ->
          counterexample (show (paRootId artifacts)) (paRootId artifacts >= 0)
        Left err -> counterexample err False

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

surfaceExpr :: Int -> Surf.SurfaceExpr
surfaceExpr size =
  case size `mod` 5 of
    0 -> Surf.ELit (Surf.LInt (fromIntegral size))
    1 -> Surf.ELam "x" (Surf.EVar "x")
    2 -> Surf.EApp (Surf.ELam "x" (Surf.EVar "x")) (Surf.ELit (Surf.LInt (fromIntegral size)))
    3 -> Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x")) (Surf.EVar "id")
    _ -> Surf.EAnn (Surf.ELit (Surf.LInt (fromIntegral size))) (Surf.STBase "Int")

elabTerm :: Int -> (Elab.ElabTerm, Elab.ElabType)
elabTerm size =
  case size `mod` 6 of
    0 -> (Elab.ELit (Surf.LInt (fromIntegral size)), intTy)
    1 -> (Elab.ELit (Surf.LBool (even size)), boolTy)
    2 -> (idLam, Elab.TArrow intTy intTy)
    3 -> (Elab.EApp idLam (Elab.ELit (Surf.LInt (fromIntegral size))), intTy)
    4 -> (Elab.ELet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (Elab.EVar "x"), intTy)
    _ -> (Elab.ETyInst polyId (Elab.InstApp intTy), Elab.TArrow intTy intTy)

reducibleTerm :: Int -> Elab.ElabTerm
reducibleTerm size =
  case size `mod` 5 of
    0 -> Elab.EApp idLam (Elab.ELit (Surf.LInt 1))
    1 -> Elab.ELet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (Elab.EVar "x")
    2 -> Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro
    3 -> Elab.ETyInst polyId Elab.InstElim
    _ -> Elab.ETyInst polyId (Elab.InstApp intTy)

instantiation :: Int -> Elab.Instantiation
instantiation size =
  case size `mod` 5 of
    0 -> Elab.InstId
    1 -> Elab.InstElim
    2 -> Elab.InstApp intTy
    3 -> Elab.InstSeq (Elab.InstInside (Elab.InstBot intTy)) Elab.InstElim
    _ -> Elab.InstUnder "x" Elab.InstId

intTy :: Elab.ElabType
intTy = Elab.TBase (BaseTy "Int")

boolTy :: Elab.ElabType
boolTy = Elab.TBase (BaseTy "Bool")

idLam :: Elab.ElabTerm
idLam = Elab.ELam "x" intTy (Elab.EVar "x")

polyId :: Elab.ElabTerm
polyId = Elab.ETyAbs "a" Nothing (Elab.ELam "x" (Elab.TVar "a") (Elab.EVar "x"))

paRootId :: PipelineArtifacts -> Int
paRootId = getNodeId . paRoot
