{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ElaborationSpec (spec) where

import IdentityTestSupport
import qualified ElabTypeTestSupport as TestElab
import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Data.Either (isLeft, isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (find, isInfixOf, mapAccumL)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import MLF.Binding.Canonicalization qualified as BindCanon
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Finalize.TestSupport qualified as Finalize
import MLF.Constraint.Presolution
  ( EdgeTrace (..),
    PresolutionPlanBuilder (..),
    PresolutionView (..),
    prEdgeExpansions,
    prEdgeTraces,
    prEdgeWitnesses,
    prIdentityEdges,
    prConstraint,
    prPlanBuilder,
    prRedirects,
  )
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    GeneralizeCtx (..),
    GeneralizeEnv (..),
    SolvedToBaseResolution (..),
    emptyExpansionConstructionPlacements,
    resolveContext,
    resolveGaSolvedToBase,
    validateCrossGenMapping,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( RequiredGammaPlacement (RequiredGammaAtCurrentScope),
  )
import MLF.Constraint.Presolution.TestSupport
  ( CopyMapping (..),
    EdgeArtifacts,
    EdgeArtifactsError (..),
    deleteEdgeArtifactForTest,
    defaultPlanBuilder,
    eaEdgeExpansionConstructions,
    eaEdgeExpansions,
    eaEdgeTraces,
    eaEdgeWitnesses,
    eaIdentityEdges,
    edgeArtifactsForTest,
    emptyRawExpansionConstruction,
    mkEdgeArtifacts,
    setEdgeArtifactsIdentityEdges,
    sourceInteriorFromList,
    insertCopy,
    lookupCopy,
  )
import MLF.Constraint.Solve.TestSupport (solveUnifyResultWithSnapshot)
import MLF.Constraint.Solved qualified as Solved
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindFlag (..),
    BindingError (..),
    Constraint (..),
    EdgeId (..),
    ExpVarId (..),
    GenNode (..),
    GenNodeId (..),
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    UnifyEdge (..),
    fromListGen,
    fromListNode,
    genRef,
    getEdgeId,
    getNodeId,
    lookupNodeIn,
    nodeRefKey,
    toListNode,
    typeRef,
  )
import MLF.Constraint.Types.Witness
  ( Expansion (..),
    InstanceOp (..),
    ReplayContract (..)
  )
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness (..), InstanceWitness (..))
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Elab.Elaborate.Algebra qualified as Algebra
import MLF.Elab.Elaborate.Algebra.TestSupport qualified as AlgebraTestSupport
import MLF.Elab.Elaborate.Annotation qualified as Annotation
import MLF.Elab.Elaborate.Annotation.TestSupport qualified as AnnotationTestSupport
import MLF.Elab.Pipeline qualified as Elab
import MLF.Elab.ReadModel (buildElabReadModel, ermNamedNodes)
import MLF.Elab.Types (ResolvedVar (..), resolvedVarRuntimeName)
import MLF.Elab.Types qualified as ElabTypes
import MLF.Elab.Phi.TestSupport qualified as PhiTestSupport
import MLF.Elab.Phi.Computation qualified as PhiComputation
import ElabTermTestSupport
  ( generatedDeferredRefForName,
    generatedLocalRefForName,
    mkTestDeferredVar,
    mkTestLocalLam,
    mkTestLocalLet,
    mkTestTyAbs,
    testTForall,
    testTMu,
    testTVar,
    testTVarApp,
  )
import MLF.Elab.Run.ResultType
  ( ResultTypeInputs (..),
    computeResultTypeFallback,
    computeResultTypeFromAnn,
    generalizeWithPlan,
    mkResultTypeInputs,
    rtcEdgeExpansions,
    rtcEdgeTraces,
    rtcEdgeWitnesses,
  )
import MLF.Elab.Run.Scope
  ( ConstructionScopeSelection (..),
    constructionBoundaryScopeSelection,
    constructionNodeScopeSelection,
    constructionScopes,
    resolveCanonicalScope,
    resolveConstructionScopeForNode,
    schemeBodyTarget,
  )
import MLF.Elab.Run.Util
  ( canonicalizeExpansion,
    canonicalizeTrace,
    canonicalizeWitness,
    makeCanonicalizer,
  )
import MLF.Frontend.ConstraintGen
  ( AnnExpr (..),
    InstantiationSite (..),
    instantiationSiteEdgeId,
    mkInstantiationSite,
  )
import MLF.Frontend.Parse (parseRawEmlfExpr, parseRawEmlfType, renderEmlfParseError)
import MLF.Frontend.Program.Builtins qualified as ProgramBuiltins
import MLF.Frontend.Program.Elaborate qualified as ProgramElaborate
import MLF.Frontend.Program.Finalize qualified as ProgramFinalize
import TypeViewTestSupport (mkTypeView)
import MLF.Frontend.Symbol (SymbolNamespace (..), symbolIdentityFromParts)
import MLF.Frontend.Syntax (Expr (..), Lit (..), NormSrcType, SrcTy (..), SrcType, SurfaceExpr, mkSrcBound)
import MLF.Types.Identity
  ( IdDetails (..),
    StructuralTypeBinderRole (..),
    UniqueIdentity (..),
    envRefFromIdentity,
    envRefIdentity,
    localRefFromNodeId,
    typeBinderIdentityFromStructural,
    typeBinderIdentityStableName,
  )
import MLF.Reify.Type qualified as ReifyType
import MLF.Reify.TypeOps qualified as TypeOps
import MLF.Util.Order qualified as Order
import Phi.WitnessDomainUtil qualified as WitnessDomain
import SolvedFacadeTestUtil qualified as SolvedTest
import SpecUtil
  ( PipelineArtifacts (..),
    bindParentsFromPairs,
    collectVarNodes,
    defaultTraceConfig,
    emptyConstraint,
    mkForalls,
    nodeMapFromList,
    requireRight,
    rootedConstraint,
    runPipelineArtifactsDefault,
    runPipelineArtifactsWithAutomaticMuDefault,
    runToPresolutionDefault,
    runToPresolutionWithAnnDefault,
    unsafeNormalizeExpr,
  )
import Test.Hspec

annDetails :: String -> IdDetails
annDetails =
  LocalId . generatedLocalRefForName

annVar :: String -> NodeId -> AnnExpr
annVar name =
  AResolvedVar (annDetails name) name

appSite :: Int -> NodeId -> NodeId -> InstantiationSite
appSite edgeKey source target =
  mkInstantiationSite (EdgeId edgeKey) source target

appSiteKey :: InstantiationSite -> Int
appSiteKey = getEdgeId . instantiationSiteEdgeId

assertOccurrenceEndpoints :: PhiComputation.OccurrenceComputation -> Expectation
assertOccurrenceEndpoints occurrence =
  Elab.applyInstantiation
    (PhiComputation.occurrenceComputationSource occurrence)
    (PhiComputation.occurrenceComputationInstantiation occurrence)
    `shouldBe` Right (PhiComputation.occurrenceComputationTarget occurrence)

assertCheckedRecursiveResult :: Elab.XmlfTerm -> Elab.ElabType -> Expectation
assertCheckedRecursiveResult term ty = do
  Elab.typeCheck term `shouldBe` Right ty
  containsRecursiveType ty `shouldBe` True

containsRecursiveType :: Elab.ElabType -> Bool
containsRecursiveType ty =
  case ty of
    Elab.TVarRef _ -> False
    Elab.TArrow domain codomain ->
      containsRecursiveType domain || containsRecursiveType codomain
    Elab.TConWithIdentity _ _ args ->
      any containsRecursiveType args
    Elab.TVarAppRef _ args ->
      any containsRecursiveType args
    Elab.TBaseWithIdentity _ _ -> False
    Elab.TBottom -> False
    Elab.TForallRef _ mbBound body ->
      maybe False containsRecursiveBound mbBound || containsRecursiveType body
    Elab.TMuRef _ _ -> True

containsRecursiveBound :: Elab.BoundType -> Bool
containsRecursiveBound bound =
  case bound of
    Elab.TArrow domain codomain ->
      containsRecursiveType domain || containsRecursiveType codomain
    Elab.TConWithIdentity _ _ args ->
      any containsRecursiveType args
    Elab.TVarAppRef _ args ->
      any containsRecursiveType args
    Elab.TBaseWithIdentity _ _ -> False
    Elab.TBottom -> False
    Elab.TForallRef _ mbBound body ->
      maybe False containsRecursiveBound mbBound || containsRecursiveType body
    Elab.TMuRef _ _ -> True

boundToType :: Elab.BoundType -> Elab.ElabType
boundToType bound = case bound of
  Elab.TArrow a b -> Elab.TArrow a b
  Elab.TConWithIdentity _ c args -> TestElab.tCon c args
  Elab.TBaseWithIdentity _ b -> TestElab.tBase b
  Elab.TBottom -> Elab.TBottom
  Elab.TVarAppRef ref args -> Elab.TVarAppRef ref args
  Elab.TForallRef ref mb body -> Elab.TForallRef ref mb body
  Elab.TMuRef ref body -> Elab.TMuRef ref body

boundFromType :: Elab.ElabType -> Elab.BoundType
boundFromType ty = case ty of
  Elab.TVarRef ref ->
    error ("boundFromType: unexpected variable bound " ++ show (ElabTypes.typeBinderRefName ref))
  Elab.TArrow a b -> Elab.TArrow a b
  Elab.TConWithIdentity _ c args -> TestElab.tCon c args
  Elab.TBaseWithIdentity _ b -> TestElab.tBase b
  Elab.TBottom -> Elab.TBottom
  Elab.TVarAppRef ref args -> Elab.TVarAppRef ref args
  Elab.TForallRef ref mb body -> Elab.TForallRef ref mb body
  Elab.TMuRef ref body -> Elab.TMuRef ref body

assertOwnResultAbstraction :: Elab.XmlfTerm -> Expectation
assertOwnResultAbstraction term =
  unless (hasOwnResultAbstraction term) $
    expectationFailure $
      "Expected a result abstraction consumed by its own InstAbstrRef, saw "
        ++ Elab.prettyDisplay term

hasOwnResultAbstraction :: Elab.XmlfTerm -> Bool
hasOwnResultAbstraction = go
  where
    go current =
      case current of
        Elab.ETyAbsRef resultRef (Just _) body ->
          resultPathConsumes resultRef body || go body
        Elab.ETyAbsRef _ Nothing body -> go body
        Elab.ELam _ body -> go body
        Elab.EApp function argument -> go function || go argument
        Elab.ELet _ _ rhs body -> go rhs || go body
        Elab.ETyInst inner _ -> go inner
        Elab.ERoll _ body -> go body
        Elab.EUnroll body -> go body
        Elab.EVarNode _ -> False
        Elab.ELit _ -> False

    -- Top-level let-bound helpers are transparent to the returned value.
    -- Follow only the let body so an unrelated coercion in a helper RHS
    -- cannot satisfy the assertion for the root result binder.
    resultPathConsumes resultRef current =
      case current of
        Elab.ELet _ _ _ body ->
          resultPathConsumes resultRef body
        Elab.ETyAbsRef _ _ body ->
          resultPathConsumes resultRef body
        Elab.ELam _ body ->
          any
            (ElabTypes.typeBinderRefsSameIdentity resultRef)
            (instAbstractionRefs body)
        _ -> False

instAbstractionRefs :: Elab.XmlfTerm -> [ElabTypes.TypeBinderRef]
instAbstractionRefs current =
  case current of
    Elab.ETyInst inner inst ->
      instantiationAbstractionRefs inst ++ instAbstractionRefs inner
    Elab.ELam _ body -> instAbstractionRefs body
    Elab.EApp function argument ->
      instAbstractionRefs function ++ instAbstractionRefs argument
    Elab.ELet _ _ rhs body ->
      instAbstractionRefs rhs ++ instAbstractionRefs body
    Elab.ETyAbsRef _ _ body -> instAbstractionRefs body
    Elab.ERoll _ body -> instAbstractionRefs body
    Elab.EUnroll body -> instAbstractionRefs body
    Elab.EVarNode _ -> []
    Elab.ELit _ -> []

instantiationAbstractionRefs :: Elab.Instantiation -> [ElabTypes.TypeBinderRef]
instantiationAbstractionRefs inst =
  case inst of
    Elab.InstAbstrRef ref -> [ref]
    Elab.InstUnderRef _ inner -> instantiationAbstractionRefs inner
    Elab.InstInside inner -> instantiationAbstractionRefs inner
    Elab.InstSeq left right ->
      instantiationAbstractionRefs left ++ instantiationAbstractionRefs right
    Elab.InstId -> []
    Elab.InstApp _ -> []
    Elab.InstBot _ -> []
    Elab.InstIntro -> []
    Elab.InstElim -> []

findNamedLet
  :: String
  -> Elab.XmlfTerm
  -> Maybe (ElabTypes.ResolvedVar, ElabTypes.ElabScheme, Elab.XmlfTerm)
findNamedLet name current =
  case current of
    Elab.ELet resolved scheme rhs body
      | ElabTypes.resolvedVarReferenceName resolved == name ->
          Just (resolved, scheme, rhs)
      | otherwise -> findNamedLet name rhs <|> findNamedLet name body
    Elab.ETyInst inner _ -> findNamedLet name inner
    Elab.ELam _ body -> findNamedLet name body
    Elab.EApp function argument ->
      findNamedLet name function <|> findNamedLet name argument
    Elab.ETyAbsRef _ _ body -> findNamedLet name body
    Elab.ERoll _ body -> findNamedLet name body
    Elab.EUnroll body -> findNamedLet name body
    Elab.EVarNode _ -> Nothing
    Elab.ELit _ -> Nothing

graphTypeBinderRef :: Int -> String -> ElabTypes.TypeBinderRef
graphTypeBinderRef node =
  ElabTypes.typeBinderRefFromIdentity
    (ElabTypes.typeBinderIdentityFromNode (NodeId node))

graphTVar :: Int -> Elab.ElabType
graphTVar node =
  ElabTypes.tVarWithRef (graphTypeBinderRef node ("t" ++ show node))

generalizeAtWith ::
  Maybe (GaBindParents 'Raw) ->
  Solved.Solved ->
  NodeRef ->
  NodeId ->
  Either Elab.ElabError (Elab.ElabScheme, IntMap.IntMap ElabTypes.TypeBinderRef)
generalizeAtWith mbGa s =
  Elab.generalizeAtWithBuilder
    (defaultPlanBuilder defaultTraceConfig)
    mbGa
    (presolutionViewFromSolved s)

generalizeAt ::
  Solved.Solved ->
  NodeRef ->
  NodeId ->
  Either Elab.ElabError (Elab.ElabScheme, IntMap.IntMap ElabTypes.TypeBinderRef)
generalizeAt = generalizeAtWith Nothing

mkSchemeInfoFromNodeNames :: Elab.ElabScheme -> IntMap.IntMap String -> Elab.SchemeInfo
mkSchemeInfoFromNodeNames scheme names =
  Elab.schemeInfoFromRefSubst (attachByFixtureNames refs scheme) refs
  where
    refs = IntMap.mapWithKey refFromName names

    refFromName key =
      ElabTypes.typeBinderRefFromIdentity (ElabTypes.typeBinderIdentityFromNode (NodeId key))

    attachByFixtureNames refsByNode scheme0 =
      ElabTypes.mkElabSchemeWithRefs binds' (applyRenames renames body0)
      where
        refLists =
          Map.fromListWith
            (flip (++))
            [ (ElabTypes.typeBinderRefName ref, [ref])
              | ref <- IntMap.elems refsByNode
            ]
        (binds0, body0) = (ElabTypes.schemeBinderRefs scheme0, ElabTypes.schemeBody scheme0)
        ((_, renames), binds') = mapAccumL attachOne (refLists, []) binds0

    attachOne (refLists, renamesSoFar) (oldRef, mb) =
      let name = ElabTypes.typeBinderRefName oldRef
          mb' = fmap (applyRenames renamesSoFar) mb
       in case Map.lookup name refLists of
            Just (newRef : rest) ->
              let refLists' =
                    if null rest
                      then Map.delete name refLists
                      else Map.insert name rest refLists
                  renames' =
                    if ElabTypes.typeBinderRefsSameIdentity oldRef newRef
                      then renamesSoFar
                      else renamesSoFar ++ [(oldRef, newRef)]
               in ((refLists', renames'), (newRef, mb'))
            _ -> ((refLists, renamesSoFar), (oldRef, mb'))

    applyRenames renames0 ty =
      foldr
        (\(oldRef, newRef) acc -> replaceBinderRef oldRef newRef acc)
        ty
        renames0

    replaceBinderRef :: ElabTypes.TypeBinderRef -> ElabTypes.TypeBinderRef -> ElabTypes.Ty v -> ElabTypes.Ty v
    replaceBinderRef target replacement ty =
      case ty of
        ElabTypes.TVarRef ref
          | ElabTypes.typeBinderRefsSameIdentity target ref -> ElabTypes.TVarRef replacement
          | otherwise -> ElabTypes.TVarRef ref
        ElabTypes.TArrow a b -> ElabTypes.TArrow (replaceBinderRef target replacement a) (replaceBinderRef target replacement b)
        ElabTypes.TConWithIdentity identity c args ->
          ElabTypes.TConWithIdentity identity c (fmap (replaceBinderRef target replacement) args)
        ElabTypes.TVarAppRef ref args ->
          let args' = fmap (replaceBinderRef target replacement) args
           in if ElabTypes.typeBinderRefsSameIdentity target ref
                then ElabTypes.TVarAppRef replacement args'
                else ElabTypes.TVarAppRef ref args'
        ElabTypes.TBaseWithIdentity identity b -> ElabTypes.TBaseWithIdentity identity b
        ElabTypes.TBottom -> ElabTypes.TBottom
        ElabTypes.TForallRef ref mb body ->
          let mb' = fmap (replaceBinderRef target replacement) mb
           in if ElabTypes.typeBinderRefsSameIdentity target ref
                then ElabTypes.TForallRef ref mb' body
                else ElabTypes.TForallRef ref mb' (replaceBinderRef target replacement body)
        ElabTypes.TMuRef ref body
          | ElabTypes.typeBinderRefsSameIdentity target ref -> ElabTypes.TMuRef ref body
          | otherwise -> ElabTypes.TMuRef ref (replaceBinderRef target replacement body)

schemeInfoNameSubst :: Elab.SchemeInfo -> IntMap.IntMap String
schemeInfoNameSubst =
  IntMap.map ElabTypes.typeBinderRefName . ElabTypes.schemeInfoBinderRefSubst

generalizeAtWithActive ::
  Solved.Solved ->
  Maybe (GaBindParents 'Raw) ->
  NodeRef ->
  NodeId ->
  Either Elab.ElabError (Elab.ElabScheme, IntMap.IntMap ElabTypes.TypeBinderRef)
generalizeAtWithActive solved mbGa scopeRoot targetNode =
  generalizeAtWith mbGa solved scopeRoot targetNode

recoverLiveSchemeAt :: PipelineArtifacts -> NodeId -> IO Elab.ElabScheme
recoverLiveSchemeAt artifacts nodeId = do
  let c1 = paConstraintNorm artifacts
      pres = paPresolution artifacts
      solved = paSolved artifacts
      (inputs, _annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
  scopeRoot <- requireRight (resolveCanonicalScope c1 (presolutionViewFromSolved solved) (prRedirects pres) nodeId)
  (scheme, _subst) <-
    requireRight
      (generalizeAtWithActive solved (Just (rtcBindParentsGa inputs)) scopeRoot (schemeBodyTarget (presolutionViewFromSolved solved) nodeId))
  pure scheme

expectWellFormedScheme :: Elab.ElabScheme -> Expectation
expectWellFormedScheme scheme = go Set.empty (ElabTypes.schemeBinderRefs scheme)
  where
    go _ [] = pure ()
    go inScope ((ref, mbBound) : rest) = do
      let name = ElabTypes.typeBinderRefName ref
      case mbBound of
        Nothing -> pure ()
        Just boundTy ->
          Elab.freeTypeVarsType (boundToType boundTy)
            `shouldSatisfy` (`Set.isSubsetOf` inScope)
      go (Set.insert name inScope) rest

xmlfTermTypeRefsClosed :: Elab.XmlfTerm -> Bool
xmlfTermTypeRefsClosed = go []
  where
    refInScope ref = any (ElabTypes.typeBinderRefsSameIdentity ref)

    typeClosed scope ty =
      all (`refInScope` scope) (TypeOps.freeTypeVarRefsType ty)

    resolvedClosed scope resolved =
      typeClosed scope (ElabTypes.resolvedVarType resolved)

    instClosed scope inst =
      case inst of
        Elab.InstId -> True
        Elab.InstApp ty -> typeClosed scope ty
        Elab.InstBot ty -> typeClosed scope ty
        Elab.InstIntro -> True
        Elab.InstElim -> True
        Elab.InstAbstrRef ref -> refInScope ref scope
        Elab.InstUnderRef ref inner -> instClosed (ref : scope) inner
        Elab.InstInside inner -> instClosed scope inner
        Elab.InstSeq left right -> instClosed scope left && instClosed scope right

    go scope term =
      case term of
        Elab.EVarNode resolved -> resolvedClosed scope resolved
        Elab.ELit {} -> True
        Elab.ELam resolved body ->
          resolvedClosed scope resolved && go scope body
        Elab.EApp fun arg -> go scope fun && go scope arg
        Elab.ELet resolved scheme rhs body ->
          resolvedClosed scope resolved
            && typeClosed scope (Elab.schemeToType scheme)
            && go scope rhs
            && go scope body
        Elab.ETyAbsRef ref mbBound body ->
          maybe True (typeClosed scope . boundToType) mbBound
            && go (ref : scope) body
        Elab.ETyInst inner inst ->
          go scope inner && instClosed scope inst
        Elab.ERoll ty body -> typeClosed scope ty && go scope body
        Elab.EUnroll body -> go scope body

mkSolved :: Constraint 'Raw -> IntMap.IntMap NodeId -> Solved.Solved
mkSolved = SolvedTest.mkTestSolved

presolutionViewFromSolved :: Solved.Solved -> PresolutionView 'Raw
presolutionViewFromSolved = Finalize.presolutionViewFromSolved

sourceDomainRaiseFixture
  :: BindFlag
  -> BindFlag
  -> Either Elab.ElabError (Elab.ElabType, Elab.Instantiation)
sourceDomainRaiseFixture sourceFlag replayFlag = do
  phi <-
    PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
      defaultTraceConfig
      (generalizeAtWithActive solved)
      (presolutionViewFromSolved solved)
      (Just gaParents)
      (Just schemeInfo)
      (Just traceInfo)
      witness
  pure (Elab.schemeToType scheme, phi)
  where
    root = NodeId 991860
    sourceBinder = NodeId 991861
    replayBinder = NodeId 991862

    baseConstraint =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyArrow root sourceBinder sourceBinder),
                  (getNodeId sourceBinder, TyVar {tnId = sourceBinder, tnBound = Nothing})
                ],
            cBindParents =
              IntMap.singleton
                (nodeRefKey (typeRef sourceBinder))
                (genRef (GenNodeId 0), sourceFlag)
          }

    finalConstraint =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyArrow root replayBinder replayBinder),
                  (getNodeId replayBinder, TyVar {tnId = replayBinder, tnBound = Nothing})
                ],
            cBindParents =
              IntMap.singleton
                (nodeRefKey (typeRef replayBinder))
                (genRef (GenNodeId 0), replayFlag)
          }

    solved =
      mkSolved
        finalConstraint
        (IntMap.singleton (getNodeId sourceBinder) replayBinder)

    gaParents =
      GaBindParents
        { gaBindParentsBase = cBindParents baseConstraint,
          gaBaseConstraint = baseConstraint,
          gaAnnotationNodeRedirects = IntMap.empty,
          gaBaseToSolved =
            IntMap.fromList
              [ (getNodeId root, root),
                (getNodeId sourceBinder, replayBinder)
              ],
          gaSolvedToBase =
            IntMap.fromList
              [ (getNodeId root, root),
                (getNodeId replayBinder, sourceBinder)
              ],
          gaRestoredSchemeRootTargets = IntMap.empty,
          gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
        }

    replayRef = graphTypeBinderRef (getNodeId replayBinder) "a"
    scheme =
      ElabTypes.mkElabSchemeWithRefs
        [(replayRef, Nothing)]
        ( Elab.TArrow
            (ElabTypes.tVarWithRef replayRef)
            (ElabTypes.tVarWithRef replayRef)
        )
    schemeInfo =
      Elab.schemeInfoFromRefSubst
        scheme
        (IntMap.singleton (getNodeId replayBinder) replayRef)

    traceInfo =
      EdgeTrace
        { etRoot = root,
          etResultRoot = root,
          etBinderArgs = [],
          etInterior = sourceInteriorFromList [root, sourceBinder],
          etBinderReplayMap = mempty,
          etReplayDomainBinders = [],
          etCopyMap = mempty,
          etReplayContract = ReplayContractNone
        }

    witness =
      EdgeWitness
        { ewEdgeId = EdgeId 991863,
          ewLeft = root,
          ewRight = root,
          ewRoot = root,
          ewForallIntros = 0,
          ewWitness = InstanceWitness [OpRaise sourceBinder]
        }

edgeTraceFixtureFromWitness :: EdgeWitness -> EdgeTrace
edgeTraceFixtureFromWitness ew =
  EdgeTrace
    { etRoot = ewRoot ew,
      etResultRoot = ewRoot ew,
      etBinderArgs = [],
      etInterior = sourceInteriorFromList (ewRoot ew : concatMap opTargets ops),
      etBinderReplayMap = IntMap.empty,
      etReplayDomainBinders = [],
      etCopyMap = mempty,
      etReplayContract = ReplayContractNone
    }
  where
    ops = case ewWitness ew of
      InstanceWitness witnessOps -> witnessOps

    opTargets op = case op of
      OpGraft arg n -> [arg, n]
      OpMerge n m -> [n, m]
      OpRaise n -> [n]
      OpWeaken n -> [n]
      OpRaiseMerge n m -> [n, m]

phiFromEdgeWitnessFixtureTrace ::
  Solved.Solved ->
  Maybe Elab.SchemeInfo ->
  EdgeWitness ->
  Either Elab.ElabError Elab.Instantiation
phiFromEdgeWitnessFixtureTrace solved mSchemeInfo ew =
  PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
    defaultTraceConfig
    (generalizeAtWithActive solved)
    (presolutionViewFromSolved solved)
    Nothing
    mSchemeInfo
    (Just (edgeTraceFixtureFromWitness ew))
    ew

resultTypeInputsForArtifacts ::
  PipelineArtifacts ->
  (ResultTypeInputs 'Raw, AnnExpr, AnnExpr)
resultTypeInputsForArtifacts
  PipelineArtifacts
    { paConstraintNorm = c1,
      paPresolution = pres,
      paSolved = solved0,
      paAnnotated = ann0
    } =
    let solvedClean = Finalize.stepPruneSolvedBindParents solved0
        canon = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        canonical = canonicalizeNode canon
        annRedirected = Elab.applyRedirectsToAnn (prRedirects pres) ann0
        annCanon = Elab.canonicalizeAnn canonical annRedirected
        edgeWitnesses = IntMap.map (canonicalizeWitness canon) (prEdgeWitnesses pres)
        edgeTraces = IntMap.map (canonicalizeTrace canon) (prEdgeTraces pres)
        edgeExpansions = IntMap.map (canonicalizeExpansion canon) (prEdgeExpansions pres)
        baseNodeKeys =
          [ getNodeId nid
            | (nid, _) <- toListNode (cNodes c1)
          ]
        baseToSolved =
          IntMap.fromList
            [ (baseKey, canonical (NodeId baseKey))
              | baseKey <- baseNodeKeys
            ]
        solvedToBase =
          foldl'
            ( \acc (baseKey, solvedNid) ->
                IntMap.insertWith
                  (\_ existing -> existing)
                  (getNodeId solvedNid)
                  (NodeId baseKey)
                  acc
            )
            IntMap.empty
            (IntMap.toList baseToSolved)
        bindParentsGa =
          GaBindParents
            { gaBindParentsBase = cBindParents c1,
              gaBaseConstraint = c1,
              gaAnnotationNodeRedirects = IntMap.empty,
              gaBaseToSolved = baseToSolved,
              gaSolvedToBase = solvedToBase,
              gaRestoredSchemeRootTargets = IntMap.empty,
              gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
            }
        inputs =
          mkResultTypeInputs
            canonical
            ( edgeArtifactsForTest
                edgeExpansions
                edgeWitnesses
                edgeTraces
                (prIdentityEdges pres)
            )
            (presolutionViewFromSolved solvedClean)
            bindParentsGa
            (defaultPlanBuilder defaultTraceConfig)
            c1
            (prRedirects pres)
            defaultTraceConfig
     in (inputs, annCanon, ann0)

data UriR2C1ReplayFixture = UriR2C1ReplayFixture
  { uriR2C1ReplaySchemeType :: Elab.ElabType,
    uriR2C1ReplayNoFallbackType :: Elab.ElabType,
    uriR2C1ReplayPhi :: Elab.Instantiation
  }

uriR2C1ReplayFixture :: IO UriR2C1ReplayFixture
uriR2C1ReplayFixture = do
  let expr =
        EAnn
          (ELam "x" (EVar "x"))
          (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
  artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
  let c1 = paConstraintNorm artifacts
      solved = paSolved artifacts
      (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
  (inner, annNodeId, edgeId) <- case annCanon of
    AAnn inner annNodeId edgeId ->
      pure (inner, annNodeId, edgeId)
    other -> do
      expectationFailure ("Expected bounded URI-R2-C1 annotation fixture, got: " ++ show other)
      fail "uriR2C1ReplayFixture"
  let rootC = rtcCanonical inputs (annExprNode inner)
      targetNode = schemeBodyTarget (rtcPresolutionView inputs) rootC
      scopeRootNodePre0 = annExprNode inner
      scopeRootNodePre =
        IntMap.findWithDefault
          scopeRootNodePre0
          (getNodeId targetNode)
          (gaSolvedToBase (rtcBindParentsGa inputs))
  scopeRoot <- requireRight (resolveCanonicalScope c1 (rtcPresolutionView inputs) (rtcRedirects inputs) scopeRootNodePre)
  (scheme, subst) <-
    requireRight
      ( generalizeWithPlan
          (rtcPlanBuilder inputs)
          (rtcBindParentsGa inputs)
          (rtcPresolutionView inputs)
          scopeRoot
          targetNode
      )
  namedSet <- requireRight (Elab.namedNodes (rtcPresolutionView inputs))
  noFallbackTy <-
    requireRight
      ( ReifyType.reifyTypeWithNamedSetRefsNoFallback
          (rtcPresolutionView inputs)
          IntMap.empty
          namedSet
          annNodeId
      )
  witness <- case IntMap.lookup (getEdgeId edgeId) (rtcEdgeWitnesses inputs) of
    Just witness ->
      pure witness
    Nothing -> do
      expectationFailure "Missing URI-R2-C1 witness replay edge witness"
      fail "uriR2C1ReplayFixture"
  edgeTrace <- case IntMap.lookup (getEdgeId edgeId) (rtcEdgeTraces inputs) of
    Just traceInfo -> pure traceInfo
    Nothing -> do
      expectationFailure "Missing URI-R2-C1 witness replay edge trace"
      fail "uriR2C1ReplayFixture"
  phi <-
    requireRight
      ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
          (rtcTraceConfig inputs)
          (generalizeAtWithActive solved)
          (rtcPresolutionView inputs)
          (Just (rtcBindParentsGa inputs))
          (Just (Elab.schemeInfoFromRefSubst scheme subst))
          (Just edgeTrace)
          witness
      )
  pure
    UriR2C1ReplayFixture
      { uriR2C1ReplaySchemeType = Elab.schemeToType scheme,
        uriR2C1ReplayNoFallbackType = noFallbackTy,
        uriR2C1ReplayPhi = phi
      }

requirePipeline :: SurfaceExpr -> IO (Elab.XmlfTerm, Elab.ElabType)
requirePipeline expr =
  requireRight
    (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))

fInstantiationEndpoints :: Elab.XmlfTerm -> [Elab.ElabType]
fInstantiationEndpoints = go []
  where
    go scope term =
      case term of
        Elab.EVarNode {} -> []
        Elab.ELit {} -> []
        Elab.ELam _ body -> go scope body
        Elab.EApp function argument ->
          go scope function ++ go scope argument
        Elab.ELet _ _ rhs body ->
          go scope rhs ++ go scope body
        Elab.ETyAbsRef ref mbBound body ->
          go ((ref, fmap boundToType mbBound) : scope) body
        Elab.ETyInst inner inst ->
          let endpoints =
                if hasNamedHead "f" inner
                  then map (resolveEndpoint scope) (instAppTypes inst)
                  else []
           in endpoints ++ go scope inner
        Elab.ERoll _ body -> go scope body
        Elab.EUnroll body -> go scope body

    hasNamedHead name term =
      case term of
        Elab.EVarNode resolved ->
          ElabTypes.resolvedVarReferenceName resolved == name
        Elab.ETyInst inner _ -> hasNamedHead name inner
        _ -> False

    instAppTypes inst =
      case inst of
        Elab.InstApp ty -> [ty]
        Elab.InstUnderRef _ inner -> instAppTypes inner
        Elab.InstInside inner -> instAppTypes inner
        Elab.InstSeq left right -> instAppTypes left ++ instAppTypes right
        _ -> []

    resolveEndpoint scope ty@(Elab.TVarRef ref) =
      case find
        (ElabTypes.typeBinderRefsSameIdentity ref . fst)
        scope of
        Just (_, Just boundTy) -> boundTy
        _ -> ty
    resolveEndpoint _ ty = ty

annExprNode :: AnnExpr -> NodeId
annExprNode ann = case ann of
  ALit _ nid -> nid
  AResolvedVar _ _ nid -> nid
  ALam _ _ _ _ _ _ nid -> nid
  AApp _ _ _ _ nid -> nid
  ALet _ _ _ _ _ _ _ _ nid -> nid
  AAnn _ nid _ -> nid
  ALetScope _ nid _ -> nid
  AUnfold _ nid _ -> nid

functionOccurrencesFor :: IdDetails -> AnnExpr -> [(AnnExpr, InstantiationSite)]
functionOccurrencesFor binderDetails = go
  where
    binderKey = ElabTypes.idDetailsIdentityKey binderDetails

    go ann =
      case ann of
        AResolvedVar {} -> []
        ALit {} -> []
        ALam _ _ _ _ body _ _ -> go body
        AApp fun arg funSite _ _ ->
          let here =
                case fun of
                  AResolvedVar occurrenceDetails _ _
                    | ElabTypes.idDetailsIdentityKey occurrenceDetails == binderKey ->
                        [(fun, funSite)]
                  _ -> []
           in here ++ go fun ++ go arg
        ALet _ _ _ _ _ _ rhs body _ -> go rhs ++ go body
        AAnn inner _ _ -> go inner
        ALetScope inner _ _ -> go inner
        AUnfold inner _ _ -> go inner

variableAliasFunctionOccurrences :: AnnExpr -> [(AnnExpr, InstantiationSite)]
variableAliasFunctionOccurrences = go
  where
    go ann =
      case ann of
        AResolvedVar {} -> []
        ALit {} -> []
        ALam _ _ _ _ body _ _ -> go body
        AApp fun arg _ _ _ -> go fun ++ go arg
        ALet _ binderDetails _ _ _ _ rhs body _ ->
          let here =
                if isResolvedOccurrence rhs
                  then functionOccurrencesFor binderDetails body
                  else []
           in here ++ go rhs ++ go body
        AAnn inner _ _ -> go inner
        ALetScope inner _ _ -> go inner
        AUnfold inner _ _ -> go inner

    isResolvedOccurrence expr =
      case expr of
        AResolvedVar {} -> True
        AAnn inner _ _ -> isResolvedOccurrence inner
        ALetScope inner _ _ -> isResolvedOccurrence inner
        _ -> False

stripBoundWrapper :: Elab.ElabType -> Elab.ElabType
stripBoundWrapper (Elab.TForallRef ref (Just bound) (Elab.TVarRef bodyRef))
  | ElabTypes.typeBinderRefsSameIdentity ref bodyRef = stripBoundWrapper (boundToType bound)
stripBoundWrapper t = t

-- | Canonicalize binder names in a type to compare up to α-equivalence.
canonType :: Elab.ElabType -> Elab.ElabType
canonType = go [] (0 :: Int)
  where
    lookupRef :: ElabTypes.TypeBinderRef -> [(ElabTypes.TypeBinderRef, String)] -> Maybe String
    lookupRef ref = fmap snd . findRef
      where
        findRef [] = Nothing
        findRef ((candidate, name) : rest)
          | ElabTypes.typeBinderRefsSameIdentity ref candidate = Just (candidate, name)
          | otherwise = findRef rest

    go :: [(ElabTypes.TypeBinderRef, String)] -> Int -> Elab.ElabType -> Elab.ElabType
    go env n ty = case ty of
      Elab.TVarRef ref ->
        case lookupRef ref env of
          Just name -> testTVar name
          Nothing -> testTVar (ElabTypes.typeBinderRefName ref)
      Elab.TConWithIdentity _ c args -> TestElab.tCon c (fmap (go env n) args)
      Elab.TVarAppRef ref args ->
        let name = maybe (ElabTypes.typeBinderRefName ref) id (lookupRef ref env)
         in testTVarApp name (fmap (go env n) args)
      Elab.TBaseWithIdentity _ b -> TestElab.tBase b
      Elab.TBottom -> Elab.TBottom
      Elab.TArrow a b -> Elab.TArrow (go env n a) (go env n b)
      Elab.TForallRef ref mb body ->
        let name = "a" ++ show n
            env' = (ref, name) : env
            -- binder is not in scope for its bound
            mb' = fmap (boundFromType . go env n . boundToType) mb
            body' = go env' (n + 1) body
         in testTForall name mb' body'
      Elab.TMuRef ref body ->
        let name = "a" ++ show n
            env' = (ref, name) : env
         in testTMu name (go env' (n + 1) body)

shouldAlphaEqType :: Elab.ElabType -> Elab.ElabType -> Expectation
shouldAlphaEqType actual expected =
  canonType actual `shouldBe` canonType expected

shouldEqUpToTypeVarRenaming :: Elab.ElabType -> Elab.ElabType -> Expectation
shouldEqUpToTypeVarRenaming actual expected =
  canonAllTypeVars actual `shouldBe` canonAllTypeVars expected
  where
    canonAllTypeVars :: Elab.ElabType -> Elab.ElabType
    canonAllTypeVars ty = let (_, _, ty') = go [] (0 :: Int) ty in ty'

    allocName :: [(ElabTypes.TypeBinderRef, String)] -> Int -> ElabTypes.TypeBinderRef -> ([(ElabTypes.TypeBinderRef, String)], Int, String)
    allocName env n ref = case lookupRef ref env of
      Just name -> (env, n, name)
      Nothing ->
        let name = "a" ++ show n
         in ((ref, name) : env, n + 1, name)

    lookupRef :: ElabTypes.TypeBinderRef -> [(ElabTypes.TypeBinderRef, String)] -> Maybe String
    lookupRef ref = fmap snd . findRef
      where
        findRef [] = Nothing
        findRef ((candidate, name) : rest)
          | ElabTypes.typeBinderRefsSameIdentity ref candidate = Just (candidate, name)
          | otherwise = findRef rest

    go :: [(ElabTypes.TypeBinderRef, String)] -> Int -> Elab.ElabType -> ([(ElabTypes.TypeBinderRef, String)], Int, Elab.ElabType)
    go env n ty = case ty of
      Elab.TVarRef ref ->
        let (env', n', name) = allocName env n ref
         in (env', n', testTVar name)
      Elab.TConWithIdentity _ c args ->
        let (env', n', args') = goList env n (NE.toList args)
         in (env', n', TestElab.tCon c (NE.fromList args'))
      Elab.TVarAppRef ref args ->
        let (env1, n1, name) = allocName env n ref
            (env2, n2, args') = goList env1 n1 (NE.toList args)
         in (env2, n2, testTVarApp name (NE.fromList args'))
      Elab.TBaseWithIdentity _ b -> (env, n, TestElab.tBase b)
      Elab.TBottom -> (env, n, Elab.TBottom)
      Elab.TArrow a b ->
        let (env1, n1, a') = go env n a
            (env2, n2, b') = go env1 n1 b
         in (env2, n2, Elab.TArrow a' b')
      Elab.TForallRef ref mb body ->
        let (env1, n1, mb') = case mb of
              Nothing -> (env, n, Nothing)
              Just bound ->
                let (env', n', bound') = go env n (boundToType bound)
                 in (env', n', Just (boundFromType bound'))
            name = "a" ++ show n1
            envBody = (ref, name) : env1
            (_, n2, body') = go envBody (n1 + 1) body
         in (env1, n2, testTForall name mb' body')
      Elab.TMuRef ref body ->
        let name = "a" ++ show n
            envBody = (ref, name) : env
            (_, n', body') = go envBody (n + 1) body
         in (env, n', testTMu name body')

    goList ::
      [(ElabTypes.TypeBinderRef, String)] ->
      Int ->
      [Elab.ElabType] ->
      ([(ElabTypes.TypeBinderRef, String)], Int, [Elab.ElabType])
    goList env n tys = case tys of
      [] -> (env, n, [])
      ty : rest ->
        let (env1, n1, ty') = go env n ty
            (env2, n2, rest') = goList env1 n1 rest
         in (env2, n2, ty' : rest')

-- | Drop top-level vacuous forall binders to compare equivalent schemes
-- that differ only by unused quantifier wrappers.
stripUnusedTopForalls :: Elab.ElabType -> Elab.ElabType
stripUnusedTopForalls ty =
  case ty of
    Elab.TForallRef ref Nothing body
      | not (occursInType ref body) -> stripUnusedTopForalls body
    _ -> ty
  where
    occursInType needle = go False
      where
        go shadowed t = case t of
          Elab.TVarRef ref -> not shadowed && ElabTypes.typeBinderRefsSameIdentity ref needle
          Elab.TConWithIdentity _ _ args -> any (go shadowed) args
          Elab.TVarAppRef ref args -> (not shadowed && ElabTypes.typeBinderRefsSameIdentity ref needle) || any (go shadowed) args
          Elab.TBaseWithIdentity _ _ -> False
          Elab.TBottom -> False
          Elab.TArrow a b -> go shadowed a || go shadowed b
          Elab.TForallRef ref mb body ->
            let inBound = maybe False (occursInBound shadowed) mb
                bodyShadowed = shadowed || ElabTypes.typeBinderRefsSameIdentity ref needle
             in inBound || go bodyShadowed body
          Elab.TMuRef ref body ->
            go (shadowed || ElabTypes.typeBinderRefsSameIdentity ref needle) body

        occursInBound shadowed b = case b of
          Elab.TArrow a c -> go shadowed a || go shadowed c
          Elab.TConWithIdentity _ _ args -> any (go shadowed) args
          Elab.TVarAppRef ref args -> (not shadowed && ElabTypes.typeBinderRefsSameIdentity ref needle) || any (go shadowed) args
          Elab.TBaseWithIdentity _ _ -> False
          Elab.TBottom -> False
          Elab.TForallRef ref mb body ->
            let inBound = maybe False (occursInBound shadowed) mb
                bodyShadowed = shadowed || ElabTypes.typeBinderRefsSameIdentity ref needle
             in inBound || go bodyShadowed body
          Elab.TMuRef ref body ->
            go (shadowed || ElabTypes.typeBinderRefsSameIdentity ref needle) body

spec :: Spec
spec = describe "Phase 6 — Elaborate (xMLF)" $ do
  describe "Recursive structural types" $ do
    let recursiveInt :: Elab.ElabType
        recursiveInt = testTMu "a" (Elab.TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
        recursiveList :: Elab.ElabType
        recursiveList = testTMu "a" (TestElab.tCon (BaseTy "List") (NE.singleton (testTVar "a")))
        forallRecursive :: Elab.ElabType
        forallRecursive = testTMu "a" (testTForall "b" Nothing (testTVar "a"))

    it "prints μ types through the elaborated pretty path" $ do
      Elab.pretty recursiveInt `shouldBe` "μa. a -> Int"

    it "does not identify μ with its unfolding" $ do
      let unfolded = Elab.TArrow recursiveInt (TestElab.tBase (BaseTy "Int"))
      recursiveInt `shouldNotBe` unfolded

    it "tracks only free variables outside μ binders" $ do
      let ty = testTMu "a" (Elab.TArrow (testTVar "a") (testTVar "x"))
      Elab.freeTypeVarsType ty `shouldBe` Set.singleton "x"

    it "roundtrips μ through bound conversion helpers" $ do
      boundToType (boundFromType recursiveInt) `shouldBe` recursiveInt

    it "keeps the v1 contractiveness policy conservative around forall" $ do
      Elab.typeCheck (mkTestLocalLam "x" recursiveList (mkTestDeferredVar "x"))
        `shouldBe` Right (Elab.TArrow recursiveList recursiveList)
      case Elab.typeCheck (mkTestLocalLam "x" forallRecursive (mkTestDeferredVar "x")) of
        Left (Elab.TCNonContractiveRecursiveType ty) | ty == forallRecursive -> pure ()
        other ->
          expectationFailure
            ("Expected forall-only recursive type rejection, got: " ++ show other)

  describe "Migration guards" $ do
    it "chi-first Elaborate|Phase 6 keeps representative behavior" $ do
      let corpus =
            [ ELit (LInt 1),
              ELam "x" (EVar "x"),
              ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 0)))
            ]
      forM_ corpus $ \expr -> do
        _ <- requirePipeline expr
        _ <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        pure ()

    it "result-type fallback matches pipeline type on non-annotation roots" $ do
      let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 7))
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
      viaFallback <- requireRight (computeResultTypeFallback inputs annCanon annPre)
      (_term, viaPipeline) <- requirePipeline expr
      viaFallback `shouldAlphaEqType` viaPipeline

    it "result-type reconstruction fails on malformed PresolutionView 'Raw materialization" $ do
      let expr = ELit (LInt 1)
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
          rootC = rtcCanonical inputs (annExprNode annCanon)
          view0 = rtcPresolutionView inputs
          brokenView =
            view0
              { pvCanonicalConstraint =
                  (pvCanonicalConstraint view0)
                    { cUnifyEdges = [UnifyEdge rootC rootC]
                    }
              }
          inputsBroken =
            inputs
              { rtcPresolutionView = brokenView
              }
      case computeResultTypeFallback inputsBroken annCanon annPre of
        Left (Elab.ValidationFailed msgs) ->
          msgs `shouldSatisfy` any ("Residual unification edge" `isInfixOf`)
        other ->
          expectationFailure
            ("Expected ValidationFailed from malformed PresolutionView 'Raw, got " ++ show other)

    it "rejects a witnessless annotation packet before result-type computation" $ do
      let expr =
            EAnn
              (ELam "x" (EVar "x"))
              (STArrow (STBase "Int") (STBase "Int"))
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      case annCanon of
        AAnn _ _ eid ->
          case
              mkEdgeArtifacts
                (rtcEdgeExpansions inputs)
                (IntMap.delete (getEdgeId eid) (rtcEdgeWitnesses inputs))
                (rtcEdgeTraces inputs)
                (eaEdgeExpansionConstructions (rtcEdgeArtifacts inputs))
                (eaIdentityEdges (rtcEdgeArtifacts inputs))
            of
            Left EdgeArtifactKeyMismatch{} -> pure ()
            other ->
              expectationFailure
                ("expected witnessless packet construction rejection, got " ++ show other)
        other ->
          expectationFailure ("Expected top-level AAnn for witness guard, got " ++ show other)

    it "annotation elaboration requires one complete edge packet by construction" $ do
      let expr =
            EAnn
              (ELam "x" (EVar "x"))
              (STArrow (STBase "Int") (STBase "Int"))
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
          edgeArtifacts = rtcEdgeArtifacts inputs
          witnesses = eaEdgeWitnesses edgeArtifacts
          traces = eaEdgeTraces edgeArtifacts
          expansions = eaEdgeExpansions edgeArtifacts
          constructions = eaEdgeExpansionConstructions edgeArtifacts
      case annCanon of
        AAnn _ _ eid -> do
          let edgeKey = getEdgeId eid
              sourceTypes =
                IntMap.singleton
                  edgeKey
                  (TestElab.tBase (BaseTy "Int"))
          IntMap.member edgeKey witnesses `shouldBe` True
          IntMap.member edgeKey traces `shouldBe` True
          IntMap.member edgeKey expansions `shouldBe` True
          edgeAuthority <-
            requireRight
              ( Annotation.mkElaborationEdgeAuthority
                  (rtcCanonical inputs)
                  sourceTypes
                  edgeArtifacts
                  [annCanon]
              )
          case Annotation.authorizedElaborationRoots edgeAuthority of
            [authorizedRoot] -> do
              Annotation.authorizedElaborationResultAnn authorizedRoot
                `shouldBe` annCanon
              Annotation.authorizedElaborationConstructionAnn authorizedRoot
                `shouldBe` annCanon
            roots ->
              expectationFailure
                ("expected one authorized elaboration root, got " ++ show roots)
          Annotation.mkElaborationEdgeAuthority
            (rtcCanonical inputs)
            IntMap.empty
            edgeArtifacts
            [annCanon]
            `shouldBe` Left (Elab.ValidationFailed ["missing source type for annotation " ++ show eid])
          Annotation.mkElaborationEdgeAuthority
            (rtcCanonical inputs)
            (IntMap.insert (edgeKey + 100000) (TestElab.tBase (BaseTy "Bool")) sourceTypes)
            edgeArtifacts
            [annCanon]
            `shouldBe` Left
              ( Elab.ValidationFailed
                  [ "annotation expected-type authority has no source occurrence",
                    "  edges: [" ++ show (EdgeId (edgeKey + 100000)) ++ "]"
                  ]
              )
          Annotation.mkElaborationEdgeAuthority
            (rtcCanonical inputs)
            sourceTypes
            edgeArtifacts
            [annCanon, annCanon]
            `shouldBe` Left
              ( Elab.ValidationFailed
                  [ "one annotation edge is owned by multiple source occurrences",
                    "  edge: " ++ show eid
                  ]
              )
          forM_
            [ mkEdgeArtifacts
                expansions
                (IntMap.delete edgeKey witnesses)
                traces
                constructions
                (eaIdentityEdges edgeArtifacts),
              mkEdgeArtifacts
                expansions
                witnesses
                (IntMap.delete edgeKey traces)
                constructions
                (eaIdentityEdges edgeArtifacts),
              mkEdgeArtifacts
                (IntMap.delete edgeKey expansions)
                witnesses
                traces
                constructions
                (eaIdentityEdges edgeArtifacts),
              mkEdgeArtifacts
                expansions
                witnesses
                traces
                (IntMap.delete edgeKey constructions)
                (eaIdentityEdges edgeArtifacts)
            ]
            $ \result ->
              case result of
                Left EdgeArtifactKeyMismatch{} -> pure ()
                other ->
                  expectationFailure
                    ("expected partial edge packet construction rejection, got " ++ show other)
        other ->
          expectationFailure ("Expected top-level AAnn for elaboration authority guard, got " ++ show other)

    it "lambda-body elaboration requires replay artifacts or explicit identity provenance" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
          edgeArtifacts = rtcEdgeArtifacts inputs
          witnesses = eaEdgeWitnesses edgeArtifacts
          traces = eaEdgeTraces edgeArtifacts
          expansions = eaEdgeExpansions edgeArtifacts
          identityEdges = eaIdentityEdges edgeArtifacts
      case annCanon of
        ALam _ _ _ _ _ bodyEid _ -> do
          let edgeKey = getEdgeId bodyEid
              identityEdgesWithoutBody = IntSet.delete edgeKey identityEdges
              validate edgeArtifacts' ann =
                Annotation.mkElaborationEdgeAuthority
                  (rtcCanonical inputs)
                  IntMap.empty
                  edgeArtifacts'
                  [ann]
              nonIdentityArtifacts =
                setEdgeArtifactsIdentityEdges
                  identityEdgesWithoutBody
                  edgeArtifacts
              withoutBodyArtifacts =
                deleteEdgeArtifactForTest bodyEid nonIdentityArtifacts
          IntMap.member edgeKey witnesses `shouldBe` True
          IntMap.member edgeKey traces `shouldBe` True
          IntMap.member edgeKey expansions `shouldBe` True
          validate nonIdentityArtifacts annCanon
            `shouldSatisfy` isRight
          validate withoutBodyArtifacts annCanon
            `shouldSatisfy` isLeft
          validate
            ( setEdgeArtifactsIdentityEdges
                (IntSet.insert edgeKey identityEdgesWithoutBody)
                withoutBodyArtifacts
            )
            annCanon
            `shouldSatisfy` isRight
        other ->
          expectationFailure ("Expected top-level ALam for lambda-body authority guard, got " ++ show other)

    it "application edge authority rejects mismatched ids and endpoints before elaboration" $ do
      artifacts <-
        requireRight
          ( runPipelineArtifactsDefault
              Set.empty
              (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
          )
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
          edgeArtifacts = rtcEdgeArtifacts inputs
          witnesses = eaEdgeWitnesses edgeArtifacts
          identityEdges = eaIdentityEdges edgeArtifacts
          validate ann =
            Annotation.mkElaborationEdgeAuthority
              (rtcCanonical inputs)
              IntMap.empty
              edgeArtifacts
              [ann]
      case annCanon of
        ALet name details schemeGen schemeRoot expVar rhsGen rhs (ALetScope (AApp fun arg funSite argSite appNode) scopeNode scopeEid) resultNode -> do
          let eid@(EdgeId edgeKey) = instantiationSiteEdgeId funSite
              stale = NodeId 999999
              rebuild site =
                ALet
                  name
                  details
                  schemeGen
                  schemeRoot
                  expVar
                  rhsGen
                  rhs
                  (ALetScope (AApp fun arg site argSite appNode) scopeNode scopeEid)
                  resultNode
              badSourceAnn =
                rebuild (funSite {instantiationSiteSource = stale})
              badTargetAnn =
                rebuild (funSite {instantiationSiteTarget = stale})
          IntSet.member edgeKey identityEdges `shouldBe` False
          validate annCanon `shouldSatisfy` isRight
          validate badSourceAnn
            `shouldBe` Left
              (Elab.ValidationFailed ["application function witness source does not match its construction site: " ++ show eid])
          validate badTargetAnn
            `shouldBe` Left
              (Elab.ValidationFailed ["application function witness destination does not match its construction site: " ++ show eid])
          witness <-
            case IntMap.lookup edgeKey witnesses of
              Just value -> pure value
              Nothing -> expectationFailure "expected function replay witness" >> fail "missing function witness"
          let mismatchedWitness = witness {ewEdgeId = EdgeId (edgeKey + 100000)}
              witnessesWithWrongId = IntMap.insert edgeKey mismatchedWitness witnesses
          case
              mkEdgeArtifacts
                (eaEdgeExpansions edgeArtifacts)
                witnessesWithWrongId
                (eaEdgeTraces edgeArtifacts)
                (eaEdgeExpansionConstructions edgeArtifacts)
                (eaIdentityEdges edgeArtifacts)
            of
            Left EdgeArtifactWitnessIdMismatch{} -> pure ()
            other ->
              expectationFailure
                ("Expected mismatched replay edge construction rejection, got " ++ show other)
        other ->
          expectationFailure ("Expected top-level AApp for application authority guard, got " ++ show other)

  describe "SrcTy indexed aliases compile shape" $ do
    it "supports raw and normalized aliases from one SrcTy family" $ do
      let rawTy :: SrcType
          rawTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          normTy :: NormSrcType
          normTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
      show rawTy `shouldNotBe` ""
      show normTy `shouldNotBe` ""

  describe "Basic elaboration" $ do
    it "elaborates integer literal" $ do
      let expr = ELit (LInt 1)
      (term, ty) <- requirePipeline expr
      Elab.prettyDisplay term `shouldBe` "1"
      Elab.prettyDisplay ty `shouldBe` "Int"

    it "elaborates boolean literal" $ do
      let expr = ELit (LBool True)
      (term, ty) <- requirePipeline expr
      Elab.prettyDisplay term `shouldBe` "true"
      Elab.prettyDisplay ty `shouldBe` "Bool"

    it "O15-ELAB-LAMBDA-VAR O15-ELAB-ABS: elaborates lambda" $ do
      let expr = ELam "x" (EVar "x")
      (_, ty) <- requirePipeline expr
      -- Result is generalized at top level
      Elab.prettyDisplay ty `shouldBe` "∀(a ⩾ ⊥) a -> a"

    it "O15-ELAB-APP: elaborates application" $ do
      let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 42))
      (_, ty) <- requirePipeline expr
      Elab.prettyDisplay ty `shouldBe` "Int"

    it "constructs an applied lambda from its solved arrow-domain identity" $ do
      let expr = ELam "k" (EApp (ELam "y" (EVar "y")) (EVar "k"))
          isTypeVariable node =
            case node of
              Just TyVar {} -> True
              _ -> False
          asLambda term =
            case term of
              Elab.ELam resolved _ -> Just resolved
              Elab.ETyAbsRef _ _ inner -> asLambda inner
              Elab.ETyInst inner _ -> asLambda inner
              _ -> Nothing
          asVariable term =
            case term of
              Elab.EVarNode resolved -> Just resolved
              Elab.ETyInst inner _ -> asVariable inner
              _ -> Nothing
          appliedLambdaTypes term =
            case term of
              Elab.EApp fun arg
                | Just lambdaBinder <- asLambda fun,
                  Just argumentVar <- asVariable arg,
                  ElabTypes.resolvedVarReferenceName lambdaBinder == "y",
                  ElabTypes.resolvedVarReferenceName argumentVar == "k" ->
                    Just
                      ( ElabTypes.resolvedVarType lambdaBinder,
                        ElabTypes.resolvedVarType argumentVar
                      )
                | otherwise -> appliedLambdaTypes fun <|> appliedLambdaTypes arg
              Elab.ELam _ body -> appliedLambdaTypes body
              Elab.ELet _ _ rhs body -> appliedLambdaTypes rhs <|> appliedLambdaTypes body
              Elab.ETyAbsRef _ _ inner -> appliedLambdaTypes inner
              Elab.ETyInst inner _ -> appliedLambdaTypes inner
              Elab.ERoll _ inner -> appliedLambdaTypes inner
              Elab.EUnroll inner -> appliedLambdaTypes inner
              Elab.EVarNode {} -> Nothing
              Elab.ELit {} -> Nothing

      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, _) = resultTypeInputsForArtifacts artifacts
          view = rtcPresolutionView inputs
      case annCanon of
        ALam "k" _ _ _ (AApp (ALam "y" _ originalParam _ _ _ lambdaNode) _ _ _ _) _ _ ->
          case Algebra.resolvedLambdaParamNode (pvCanonical view) (pvLookupNode view) lambdaNode of
            Just solvedParam -> do
              pvCanonical view solvedParam `shouldNotBe` pvCanonical view originalParam
              pvLookupNode view (pvCanonical view solvedParam) `shouldSatisfy` isTypeVariable
            Nothing ->
              expectationFailure "Expected retained solved arrow topology for the applied lambda"
        other ->
          expectationFailure ("Expected nested applied lambda annotation, got " ++ show other)
      (term, _) <- requirePipeline expr
      case appliedLambdaTypes term of
        Just (Elab.TVarRef lambdaDomainRef, Elab.TVarRef argumentRef) ->
          ElabTypes.typeBinderRefsSameIdentity lambdaDomainRef argumentRef `shouldBe` True
        Just types ->
          expectationFailure ("Expected variable application topology, got " ++ show types)
        Nothing ->
          expectationFailure ("Expected applied identity lambda in " ++ show term)

  describe "Polymorphism and Generalization" $ do
    it "O15-ELAB-LET: elaborates polymorphic let-binding" $ do
      -- let id = \x. x in id
      let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
      (term, ty) <- requirePipeline expr
      let expected =
            testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))
      case term of
        Elab.ELet idResolved scheme (Elab.ETyAbsRef ref Nothing (Elab.ELam xResolved (Elab.EVarNode xBody))) (Elab.EVarNode idBody) -> do
          ElabTypes.resolvedVarReferenceName idResolved `shouldBe` "id"
          ElabTypes.resolvedVarReferenceName idBody `shouldBe` "id"
          case ElabTypes.schemeBinderRefs scheme of
            [(schemeRef, Nothing)] -> do
              ElabTypes.typeBinderRefsSameIdentity schemeRef ref `shouldBe` True
              case
                  ( ElabTypes.resolvedVarType xResolved
                  , ElabTypes.resolvedVarType xBody
                  )
                of
                  (Elab.TVarRef paramRef, Elab.TVarRef bodyRef) ->
                    map
                      ElabTypes.typeBinderRefName
                      [schemeRef, ref, paramRef, bodyRef]
                      `shouldBe` replicate 4 "a"
                  types ->
                    expectationFailure
                      ("Expected identity body references, got " ++ show types)
            binders ->
              expectationFailure ("Expected one canonical identity binder, got " ++ show binders)
          Elab.schemeToType scheme `shouldAlphaEqType` expected
          ElabTypes.resolvedVarType idResolved `shouldAlphaEqType` expected
          ElabTypes.resolvedVarType idBody `shouldAlphaEqType` expected
          ElabTypes.resolvedVarType xResolved `shouldBe` Elab.TVarRef ref
          ElabTypes.resolvedVarType xBody `shouldBe` Elab.TVarRef ref
        other -> expectationFailure ("Expected polymorphic identity let, got " ++ show other)
      ty `shouldAlphaEqType` expected
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` expected

    it "O15-ELAB-LET-VAR: elaborates monomorphic let without extra instantiation" $ do
      -- let x = 1 in x
      let expr = ELet "x" (ELit (LInt 1)) (EVar "x")
      (term, ty) <- requirePipeline expr
      Elab.prettyDisplay term `shouldBe` "let x = 1 in x"
      Elab.prettyDisplay ty `shouldBe` "Int"

    it "row5 typing-environment O15-ENV-LAMBDA O15-ENV-WF: lambda body inherits the enclosing binder on the live path" $ do
      let expr = ELam "x" (ELam "y" (EVar "x"))
      (term, ty) <- requirePipeline expr
      case term of
        Elab.ETyAbsRef outerRef Nothing
          ( Elab.ETyAbsRef resultRef (Just resultBound)
            ( Elab.ELam xResolved
              ( Elab.ETyInst
                (Elab.ETyAbsRef innerRef Nothing (Elab.ELam yResolved (Elab.EVarNode bodyVar)))
                (Elab.InstAbstrRef resultInstRef)
              )
            )
          )
            | ElabTypes.resolvedVarReferenceName xResolved == "x"
                && ElabTypes.resolvedVarReferenceName yResolved == "y"
                && ElabTypes.resolvedVarReferenceName bodyVar == "x" -> do
                ElabTypes.typeBinderRefsSameIdentity resultRef resultInstRef `shouldBe` True
                ElabTypes.resolvedVarType xResolved `shouldBe` Elab.TVarRef outerRef
                ElabTypes.resolvedVarType yResolved `shouldBe` Elab.TVarRef innerRef
                ElabTypes.resolvedVarType bodyVar `shouldBe` Elab.TVarRef outerRef
                ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails xResolved)
                  `shouldBe` ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails bodyVar)
                case boundToType resultBound of
                  Elab.TForallRef boundRef Nothing
                    (Elab.TArrow (Elab.TVarRef boundDomainRef) (Elab.TVarRef boundOuterRef)) -> do
                      ElabTypes.typeBinderRefsSameIdentity boundRef boundDomainRef `shouldBe` True
                      ElabTypes.typeBinderRefsSameIdentity boundOuterRef outerRef `shouldBe` True
                      ElabTypes.typeBinderRefsSameIdentity innerRef boundRef `shouldBe` True
                  other -> expectationFailure ("Expected paper K result bound, got " ++ show other)
                case ty of
                  Elab.TForallRef typeOuterRef Nothing
                    ( Elab.TForallRef typeResultRef (Just typeResultBound)
                      (Elab.TArrow (Elab.TVarRef typeDomainRef) (Elab.TVarRef typeCodomainRef))
                    ) -> do
                      ElabTypes.typeBinderRefsSameIdentity typeOuterRef outerRef `shouldBe` True
                      ElabTypes.typeBinderRefsSameIdentity typeResultRef resultRef `shouldBe` True
                      ElabTypes.typeBinderRefsSameIdentity typeDomainRef outerRef `shouldBe` True
                      ElabTypes.typeBinderRefsSameIdentity typeCodomainRef resultRef `shouldBe` True
                      case boundToType typeResultBound of
                        Elab.TForallRef typeBoundRef Nothing
                          (Elab.TArrow (Elab.TVarRef typeBoundDomainRef) (Elab.TVarRef typeBoundOuterRef)) -> do
                            ElabTypes.typeBinderRefsSameIdentity typeBoundRef typeBoundDomainRef `shouldBe` True
                            ElabTypes.typeBinderRefsSameIdentity typeBoundOuterRef outerRef `shouldBe` True
                            ElabTypes.typeBinderRefsSameIdentity typeBoundRef innerRef `shouldBe` True
                        other -> expectationFailure ("Expected closed paper K type bound, got " ++ show other)
                  other -> expectationFailure ("Expected paper K principal type, got " ++ show other)
            | otherwise ->
                expectationFailure ("Expected resolved paper K binders, got " ++ show term)
        other -> expectationFailure ("Expected full paper K construction, got " ++ show other)
      TypeOps.freeTypeVarRefsType ty `shouldBe` []
      xmlfTermTypeRefsClosed term `shouldBe` True
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` ty

    it "row5 typing-environment O15-ENV-LET O15-ENV-WF: let body receives the generalized Typ(b) on the live path" $ do
      let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
          expectedTy = testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))
      artifacts@PipelineArtifacts {paAnnotated = ann} <-
        requireRight (runPipelineArtifactsDefault Set.empty expr)
      bodyNode <-
        case ann of
          ALet _ _ _ _ _ _ _ body _ -> pure (annExprNode body)
          other -> expectationFailure ("Expected ALet, got " ++ show other) >> fail "no body node"
      scheme <- recoverLiveSchemeAt artifacts bodyNode
      expectWellFormedScheme scheme
      Elab.schemeToType scheme `shouldAlphaEqType` expectedTy
      (term, ty) <- requirePipeline expr
      case term of
        Elab.ELet idResolved sch _ (Elab.EVarNode bodyVar)
          | ElabTypes.resolvedVarReferenceName idResolved == "id"
              && ElabTypes.resolvedVarReferenceName bodyVar == "id" ->
          Elab.schemeToType sch `shouldAlphaEqType` expectedTy
        other -> expectationFailure ("Expected elaborated let, got " ++ show other)
      ty `shouldAlphaEqType` expectedTy

    it "elaborates polymorphic instantiation" $ do
      -- let id = \x. x in id 1
      let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
      (term, ty) <- requirePipeline expr
      Elab.prettyDisplay ty `shouldBe` "Int"
      Elab.prettyDisplay term `shouldBe` "let id = Λ(a ⩾ ⊥) λ(x : a) x in id[∀(⩾ ⊲Int); N] 1"

    it "generalizeAt quantifies vars bound under the scope root" $ do
      let rootGen = GenNodeId 0
          arrow = NodeId 1
          var = NodeId 2
          root = NodeId 3
          nodes =
            nodeMapFromList
              [ (getNodeId arrow, TyArrow {tnId = arrow, tnDom = var, tnCod = var}),
                (getNodeId var, TyVar {tnId = var, tnBound = Nothing}),
                (getNodeId root, TyVar {tnId = root, tnBound = Just arrow})
              ]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef var), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef arrow), (typeRef root, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [root])]
              }
          solved = mkSolved constraint IntMap.empty

      (scheme, _subst) <- requireRight (generalizeAt solved (genRef rootGen) root)
      -- Eq-Var (thesis Section 8.2) normalizes
      -- forall (b > a -> a). b to a -> a.
      case ElabTypes.schemeBinderRefs scheme of
        [(aRef, Nothing)] -> ElabTypes.typeBinderRefName aRef `shouldBe` "a"
        other -> expectationFailure $ "Expected one canonical binder, got " ++ show other
      Elab.schemeToType scheme
        `shouldAlphaEqType` testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))

    it "generalizeAt uses direct gen-node binders (Q(g))" $ do
      let rootGen = GenNodeId 0
          root = NodeId 10
          body = NodeId 11
          direct = NodeId 12
          interior = NodeId 13
          nodes =
            nodeMapFromList
              [ (getNodeId root, TyForall {tnId = root, tnBody = body}),
                (getNodeId body, TyArrow {tnId = body, tnDom = direct, tnCod = interior}),
                (getNodeId direct, TyVar {tnId = direct, tnBound = Nothing}),
                (getNodeId interior, TyVar {tnId = interior, tnBound = Nothing})
              ]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef body), (typeRef root, BindFlex)),
                (nodeRefKey (typeRef direct), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef interior), (typeRef root, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [root])]
              }
          solved = mkSolved constraint IntMap.empty

      (scheme, _subst) <- requireRight (generalizeAt solved (genRef rootGen) root)
      let directRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode direct)
              "a"
          interiorRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode interior)
              "t13"
          ty = ElabTypes.schemeBody scheme
      ElabTypes.schemeBinderRefs scheme `shouldBe` [(directRef, Nothing)]
      ty
        `shouldBe` Elab.TForallRef
          interiorRef
          Nothing
          (Elab.TArrow (ElabTypes.tVarWithRef directRef) (ElabTypes.tVarWithRef interiorRef))

    it "generalizeAt fallback reifies from solved root even when base mapping points elsewhere" $ do
      let rootGen = GenNodeId 0
          solvedRoot = NodeId 1
          baseMappedRoot = NodeId 2
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId solvedRoot, TestTyBase solvedRoot (BaseTy "Int")),
                      (getNodeId baseMappedRoot, TestTyBase baseMappedRoot (BaseTy "Bool"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef solvedRoot), (genRef rootGen, BindFlex))
                    ],
                cGenNodes =
                  fromListGen
                    [(rootGen, GenNode rootGen [solvedRoot, baseMappedRoot])]
              }
          solved = mkSolved solvedConstraint IntMap.empty
          gaParents =
            GaBindParents
              { gaBindParentsBase = cBindParents solvedConstraint,
                gaBaseConstraint = solvedConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved =
                  IntMap.fromList
                    [ (getNodeId baseMappedRoot, solvedRoot),
                      (getNodeId solvedRoot, solvedRoot)
                    ],
                gaSolvedToBase = IntMap.singleton (getNodeId solvedRoot) baseMappedRoot,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }

      (scheme, _subst) <-
        requireRight (generalizeAtWith (Just gaParents) solved (genRef rootGen) solvedRoot)
      ElabTypes.schemeBinderRefs scheme `shouldBe` []
      ElabTypes.schemeBody scheme `shouldBe` TestElab.tBase (BaseTy "Int")

    it "generalizeAt keeps a base root attached to its graph when domains reuse a NodeId" $ do
      let rootGen = GenNodeId 0
          sharedRoot = NodeId 10
          liveDom = NodeId 11
          liveCod = NodeId 12
          baseDom = NodeId 21
          baseCod = NodeId 22
          liveConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId sharedRoot, TyArrow {tnId = sharedRoot, tnDom = liveDom, tnCod = liveCod}),
                      (getNodeId liveDom, TestTyBase liveDom (BaseTy "Int")),
                      (getNodeId liveCod, TestTyBase liveCod (BaseTy "Int"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef sharedRoot), (genRef rootGen, BindFlex)),
                      (nodeRefKey (typeRef liveDom), (typeRef sharedRoot, BindFlex)),
                      (nodeRefKey (typeRef liveCod), (typeRef sharedRoot, BindFlex))
                    ],
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [sharedRoot])]
              }
          baseConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId sharedRoot, TyArrow {tnId = sharedRoot, tnDom = baseDom, tnCod = baseCod}),
                      (getNodeId baseDom, TestTyBase baseDom (BaseTy "Bool")),
                      (getNodeId baseCod, TestTyBase baseCod (BaseTy "Bool"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef sharedRoot), (genRef rootGen, BindFlex)),
                      (nodeRefKey (typeRef baseDom), (typeRef sharedRoot, BindFlex)),
                      (nodeRefKey (typeRef baseCod), (typeRef sharedRoot, BindFlex))
                    ],
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [sharedRoot])]
              }
          solved = mkSolved liveConstraint IntMap.empty
          gaParents =
            GaBindParents
              { gaBindParentsBase = cBindParents baseConstraint,
                gaBaseConstraint = baseConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved =
                  IntMap.fromList
                    [ (getNodeId sharedRoot, sharedRoot),
                      (getNodeId baseDom, liveDom),
                      (getNodeId baseCod, liveCod)
                    ],
                gaSolvedToBase =
                  IntMap.fromList
                    [ (getNodeId sharedRoot, sharedRoot),
                      (getNodeId liveDom, baseDom),
                      (getNodeId liveCod, baseCod)
                    ],
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          boolTy = TestElab.tBase (BaseTy "Bool")

      (scheme, _subst) <-
        requireRight (generalizeAtWith (Just gaParents) solved (genRef rootGen) sharedRoot)
      ElabTypes.schemeBinderRefs scheme `shouldBe` []
      ElabTypes.schemeBody scheme `shouldBe` Elab.TArrow boolTy boolTy

    it "generalizeAt keeps a rigid copied result at its live bound" $ do
      let rootGen = GenNodeId 4
          schemeRoot = NodeId 8
          liveRoot = NodeId 21
          liveInt = NodeId 22
          baseRoot = NodeId 0
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId schemeRoot, TyVar {tnId = schemeRoot, tnBound = Just liveRoot}),
                      (getNodeId liveRoot, TyVar {tnId = liveRoot, tnBound = Just liveInt}),
                      (getNodeId liveInt, TestTyBase liveInt (BaseTy "Int"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex)),
                      (nodeRefKey (typeRef liveRoot), (genRef rootGen, BindRigid)),
                      (nodeRefKey (typeRef liveInt), (typeRef liveRoot, BindFlex))
                    ],
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
              }
          baseConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId baseRoot, TyVar {tnId = baseRoot, tnBound = Nothing})],
                cBindParents =
                  IntMap.singleton
                    (nodeRefKey (typeRef baseRoot))
                    (genRef rootGen, BindRigid),
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [baseRoot])]
              }
          solved = mkSolved solvedConstraint IntMap.empty
          gaParents =
            GaBindParents
              { gaBindParentsBase = cBindParents baseConstraint,
                gaBaseConstraint = baseConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.singleton (getNodeId baseRoot) liveRoot,
                gaSolvedToBase = IntMap.singleton (getNodeId liveRoot) baseRoot,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }

      (scheme, _subst) <-
        requireRight (generalizeAtWith (Just gaParents) solved (genRef rootGen) liveRoot)
      ElabTypes.schemeBinderRefs scheme `shouldBe` []
      ElabTypes.schemeBody scheme `shouldBe` TestElab.tBase (BaseTy "Int")

    it "generalizeAt does not invent a live bound from base provenance" $ do
      let rootGen = GenNodeId 4
          schemeRoot = NodeId 8
          liveRoot = NodeId 21
          unrelatedLiveInt = NodeId 22
          baseRoot = NodeId 0
          baseInt = NodeId 1
          intTy = TestElab.tBase (BaseTy "Int")
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId schemeRoot, TyVar {tnId = schemeRoot, tnBound = Just liveRoot}),
                      (getNodeId liveRoot, TyVar {tnId = liveRoot, tnBound = Nothing}),
                      (getNodeId unrelatedLiveInt, TestTyBase unrelatedLiveInt (BaseTy "Int"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex)),
                      (nodeRefKey (typeRef liveRoot), (genRef rootGen, BindRigid)),
                      (nodeRefKey (typeRef unrelatedLiveInt), (genRef rootGen, BindRigid))
                    ],
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
              }
          baseConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId baseRoot, TyVar {tnId = baseRoot, tnBound = Just baseInt}),
                      (getNodeId baseInt, TestTyBase baseInt (BaseTy "Int"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef baseRoot), (genRef rootGen, BindRigid)),
                      (nodeRefKey (typeRef baseInt), (typeRef baseRoot, BindFlex))
                    ],
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [baseRoot])]
              }
          solved = mkSolved solvedConstraint IntMap.empty
          gaParents =
            GaBindParents
              { gaBindParentsBase = cBindParents baseConstraint,
                gaBaseConstraint = baseConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved =
                  IntMap.fromList
                    [ (getNodeId baseRoot, liveRoot),
                      (getNodeId baseInt, unrelatedLiveInt)
                    ],
                gaSolvedToBase =
                  IntMap.fromList
                    [ (getNodeId liveRoot, baseRoot),
                      (getNodeId unrelatedLiveInt, baseInt)
                    ],
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }

      (scheme, _subst) <-
        requireRight (generalizeAtWith (Just gaParents) solved (genRef rootGen) liveRoot)
      ElabTypes.schemeBody scheme `shouldNotBe` intTy

    it "elaborates let-polymorphic self-application with the paper's function and result instantiations" $ do
      -- let id = \x. x in id id
      -- Thesis Section 15.3.8 gives the complete construction as
      -- @Lambda alpha. (x[sigma_id] x)[alpha]@: the function occurrence is
      -- instantiated at the polymorphic identity type, the argument edge is
      -- epsilon, and the application result is instantiated at the enclosing
      -- result binder.
      let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
      (term, ty) <- requirePipeline expr
      let stripTyAbs t = case t of
            Elab.ETyAbsRef _ _ inner -> stripTyAbs inner
            _ -> t
      case stripTyAbs term of
        Elab.ELet idResolved scheme _ body ->
          case body of
            Elab.ETyInst (Elab.EApp fun arg) resultInst@(Elab.InstApp resultArgumentTy) ->
              case (fun, arg) of
                ( Elab.ETyInst (Elab.EVarNode funVar) instF@(Elab.InstApp functionArgumentTy),
                  Elab.EVarNode argVar
                  ) -> do
                  let idTy = Elab.schemeToType scheme
                      resultScheme = Elab.schemeFromType ty
                  ElabTypes.resolvedVarSameIdentity funVar idResolved `shouldBe` True
                  ElabTypes.resolvedVarSameIdentity argVar idResolved `shouldBe` True
                  ElabTypes.resolvedVarType funVar `shouldAlphaEqType` idTy
                  ElabTypes.resolvedVarType argVar `shouldAlphaEqType` idTy
                  functionArgumentTy `shouldAlphaEqType` idTy
                  appliedFunctionTy <-
                    requireRight
                      (Elab.applyInstantiation (ElabTypes.resolvedVarType funVar) instF)
                  case appliedFunctionTy of
                    Elab.TArrow domain codomain -> do
                      domain `shouldAlphaEqType` idTy
                      codomain `shouldAlphaEqType` idTy
                    other ->
                      expectationFailure $
                        "Expected id[id] to have an id -> id type, saw " ++ show other
                  resultBinder <-
                    case Elab.schemeBinderRefs resultScheme of
                      [(ref, Nothing)] -> pure ref
                      binders ->
                        expectationFailure
                          ( "Expected one unbounded result binder, saw "
                              ++ show binders
                          )
                          >> fail "missing result binder"
                  resultArgumentTy
                    `shouldAlphaEqType` Elab.TVarRef resultBinder
                  appliedResultTy <-
                    requireRight
                      (Elab.applyInstantiation idTy resultInst)
                  appliedResultTy
                    `shouldAlphaEqType` Elab.schemeBody resultScheme
                  ty `shouldAlphaEqType` idTy
                _ ->
                  expectationFailure $
                    "Expected the paper's (id[id] id)[result] construction, saw " ++ show body
            other ->
              expectationFailure $ "Expected application body, saw " ++ show other
        other ->
          expectationFailure $ "Expected let-binding result, saw " ++ show other
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` ty

    it "elaborates usage of polymorphic let (instantiated at different types)" $ do
      -- let f = \x. x in let _ = f 1 in f true
      -- This forces 'f' to be instantiated twice: once at Int, once at Bool
      let expr =
            ELet
              "f"
              (ELam "x" (EVar "x"))
              ( ELet
                  "_"
                  (EApp (EVar "f") (ELit (LInt 1)))
                  (EApp (EVar "f") (ELit (LBool True)))
              )
      (term, ty) <- requirePipeline expr
      Elab.pretty ty `shouldBe` "Bool"
      let endpoints = fInstantiationEndpoints term
          intTy = TestElab.tBase (BaseTy "Int")
          boolTy = TestElab.tBase (BaseTy "Bool")
      endpoints
        `shouldSatisfy` any (\endpoint -> TypeOps.alphaEqType endpoint intTy)
      endpoints
        `shouldSatisfy` any (\endpoint -> TypeOps.alphaEqType endpoint boolTy)

    it "elaborates nested let bindings" $ do
      -- let x = 1 in let y = x in y
      let expr = ELet "x" (ELit (LInt 1)) (ELet "y" (EVar "x") (EVar "y"))
      (term, ty) <- requirePipeline expr
      Elab.prettyDisplay ty `shouldBe` "Int"

      Elab.prettyDisplay term `shouldSatisfy` ("let x" `isInfixOf`)
      Elab.prettyDisplay term `shouldSatisfy` ("let y" `isInfixOf`)

    it "top-level generalization ignores binders outside the type" $ do
      let expr =
            ELet
              "unused"
              (ELam "x" (EVar "x"))
              (ELam "y" (EVar "y"))
      (_term, ty) <- requirePipeline expr
      let expected =
            testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))
      ty `shouldAlphaEqType` expected

    it "elaborates term annotations" $ do
      -- (\x. x) : Int -> Int
      let ann = STArrow (STBase "Int") (STBase "Int")
          expr = EAnn (ELam "x" (EVar "x")) ann
      (_term, ty) <- requirePipeline expr
      -- Figure 8.2.3 represents the Eq-Var case directly, so the flexible
      -- coercion result is a copy of the annotation itself.
      ty
        `shouldAlphaEqType` Elab.TArrow
          (TestElab.tBase (BaseTy "Int"))
          (TestElab.tBase (BaseTy "Int"))

  describe "Result-type guard rails" $ do
    it "AAnn root: primary annotation result type matches fallback facade with populated GA mappings" $ do
      let cases =
            [ ( "let-poly value annotation",
                EAnn
                  (ELet "id" (ELam "x" (EVar "x")) (EVar "id"))
                  (STArrow (STBase "Int") (STBase "Int"))
              ),
              ( "let-poly application annotation",
                EAnn
                  (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
                  (STBase "Int")
              )
            ]
      forM_ cases $ \(label, expr) -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
        case annCanon of
          AAnn inner annNodeId eid -> do
            IntMap.member (getEdgeId eid) (rtcEdgeWitnesses inputs) `shouldBe` True
            IntMap.member (getEdgeId eid) (rtcEdgeTraces inputs) `shouldBe` True
            let ga = rtcBindParentsGa inputs
                baseToSolved = gaBaseToSolved ga
                solvedToBase = gaSolvedToBase ga
                annRootC = rtcCanonical inputs (annExprNode inner)
            IntMap.null baseToSolved `shouldBe` False
            IntMap.null solvedToBase `shouldBe` False
            case resolveGaSolvedToBase ga annRootC of
              SolvedToBaseMapped annRootBase ->
                IntMap.lookup (getNodeId annRootBase) baseToSolved `shouldBe` Just annRootC
              SolvedToBaseSameDomain sameDomain ->
                expectationFailure
                  ( "Expected ann root mapping in gaSolvedToBase for "
                      ++ label
                      ++ ", got same-domain root: "
                      ++ show sameDomain
                  )
              SolvedToBaseMissing ->
                expectationFailure
                  ( "Expected ann root mapping in gaSolvedToBase for "
                      ++ label
                      ++ ", got missing mapping"
                  )
            primary <-
              requireRight
                (computeResultTypeFromAnn inputs inner inner annNodeId eid)
            fallback <-
              requireRight
                (computeResultTypeFallback inputs annCanon annPre)
            primary `shouldAlphaEqType` fallback
          other ->
            expectationFailure
              ( "Expected top-level annotation after canonicalization for "
                  ++ label
                  ++ ", got: "
                  ++ show other
              )

    it "generalizeWithPlan surfaces SchemeFreeVars instead of falling back from GA to no-GA" $ do
      let root = NodeId 0
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId root, TestTyBase root (BaseTy "Int"))]
              }
          solved = mkSolved constraint IntMap.empty
          ga =
            GaBindParents
              { gaBindParentsBase = cBindParents constraint,
                gaBaseConstraint = constraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.singleton (getNodeId root) root,
                gaSolvedToBase = IntMap.singleton (getNodeId root) root,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          planBuilder =
            PresolutionPlanBuilder $ \_ mbGa _requirements _ _ ->
              case mbGa of
                Just _ -> Left (Elab.SchemeFreeVars root ["ga-first-pass"])
                Nothing -> Left (Elab.ValidationFailed ["ga-fallback-no-ga"])
      generalizeWithPlan planBuilder ga (presolutionViewFromSolved solved) (typeRef root) root
        `shouldBe` Left (Elab.SchemeFreeVars root ["ga-first-pass"])

    it "generalizeWithPlan surfaces SchemeFreeVars instead of falling back to reifyType" $ do
      let root = NodeId 0
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId root, TestTyBase root (BaseTy "Int"))]
              }
          solved = mkSolved constraint IntMap.empty
          ga =
            GaBindParents
              { gaBindParentsBase = cBindParents constraint,
                gaBaseConstraint = constraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.singleton (getNodeId root) root,
                gaSolvedToBase = IntMap.singleton (getNodeId root) root,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          planBuilder =
            PresolutionPlanBuilder $ \_ _ _requirements _ _ ->
              Left (Elab.SchemeFreeVars root ["double-schemefreevars"])
      generalizeWithPlan planBuilder ga (presolutionViewFromSolved solved) (typeRef root) root
        `shouldBe` Left (Elab.SchemeFreeVars root ["double-schemefreevars"])

    it "result-type fallback core handles gaSolvedToBase same-domain roots" $ do
      let expr = ELit (LInt 1)
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
          rootC = rtcCanonical inputs (annExprNode annCanon)
          ga0 = rtcBindParentsGa inputs
          gaSameDomain =
            ga0
              { gaSolvedToBase =
                  IntMap.delete (getNodeId rootC) (gaSolvedToBase ga0)
              }
          inputsSame = inputs {rtcBindParentsGa = gaSameDomain}
      resolveGaSolvedToBase gaSameDomain rootC
        `shouldBe` SolvedToBaseSameDomain rootC
      expected <- requireRight (computeResultTypeFallback inputs annCanon annPre)
      actual <- requireRight (computeResultTypeFallback inputsSame annCanon annPre)
      actual `shouldAlphaEqType` expected

    it "result-type fallback core handles gaSolvedToBase missing roots" $ do
      let expr = ELit (LInt 1)
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
          rootC = rtcCanonical inputs (annExprNode annCanon)
          ga0 = rtcBindParentsGa inputs
          base0 = gaBaseConstraint ga0
          baseNodesMissing =
            fromListNode
              [ (nid, node)
                | (nid, node) <- toListNode (cNodes base0),
                  nid /= rootC
              ]
          rootRef = typeRef rootC
          baseBindParentsMissing =
            IntMap.filterWithKey
              ( \childKey (parentRef, _) ->
                  childKey /= nodeRefKey rootRef
                    && parentRef /= rootRef
              )
              (cBindParents base0)
          baseMissing =
            base0
              { cNodes = baseNodesMissing,
                cBindParents = baseBindParentsMissing
              }
          gaMissing =
            ga0
              { gaBaseConstraint = baseMissing,
                gaSolvedToBase =
                  IntMap.delete (getNodeId rootC) (gaSolvedToBase ga0)
              }
          inputsMissing = inputs {rtcBindParentsGa = gaMissing}
      resolveGaSolvedToBase gaMissing rootC
        `shouldBe` SolvedToBaseMissing
      expected <- requireRight (computeResultTypeFallback inputs annCanon annPre)
      actual <- requireRight (computeResultTypeFallback inputsMissing annCanon annPre)
      actual `shouldAlphaEqType` expected

  describe "Binding tree coverage" $ do
    let runSolvedWithScope :: SurfaceExpr -> Either String (Solved.Solved, NodeRef, NodeId)
        runSolvedWithScope e = do
          PipelineArtifacts {paPresolution = pres, paSolved = solved, paRoot = root} <-
            runPipelineArtifactsDefault Set.empty e
          let root' = Elab.chaseRedirects (prRedirects pres) root
          scopeRoot <- case Binding.bindingRoots (Solved.originalConstraint solved) of
            [rootRef] -> Right rootRef
            roots -> Left ("Expected single binding root, got " ++ show roots)
          pure (solved, scopeRoot, root')

        bindingPathToRootUnder ::
          (NodeId -> NodeId) ->
          Constraint 'Raw ->
          NodeRef ->
          Either BindingError [NodeRef]
        bindingPathToRootUnder canonical constraint start0 =
          let startC = case start0 of
                TypeRef nid -> typeRef (canonical nid)
                GenRef gid -> GenRef gid
              go visited path ref = do
                let key = nodeRefKey ref
                if IntSet.member key visited
                  then Left (BindingCycleDetected (reverse path))
                  else do
                    mbParent <- Binding.lookupBindParentUnder canonical constraint ref
                    case mbParent of
                      Nothing -> Right (reverse path)
                      Just (parent, _flag) ->
                        go (IntSet.insert key visited) (parent : path) parent
           in go IntSet.empty [startC] startC

        freeVarsUnder :: Solved.Solved -> NodeId -> Either BindingError IntSet.IntSet
        freeVarsUnder s nid0 =
          let constraint = Solved.originalConstraint s
              nodes = cNodes constraint
              canonical = Solved.canonical s
              go bound visited nid =
                let key = getNodeId nid
                 in if IntSet.member key visited
                      then Right IntSet.empty
                      else case lookupNodeIn nodes nid of
                        Nothing ->
                          Left (InvalidBindingTree ("freeVarsUnder: missing node " ++ show nid))
                        Just TyVar {} ->
                          if IntSet.member key bound
                            then Right IntSet.empty
                            else Right (IntSet.singleton key)
                        Just TyBase {} -> Right IntSet.empty
                        Just TyBottom {} -> Right IntSet.empty
                        Just TyArrow {tnDom = d, tnCod = c} -> do
                          let visited' = IntSet.insert key visited
                          fv1 <- go bound visited' (canonical d)
                          fv2 <- go bound visited' (canonical c)
                          pure (fv1 `IntSet.union` fv2)
                        Just TyCon {tnArgs = args} -> do
                          let visited' = IntSet.insert key visited
                          fvs <- mapM (go bound visited' . canonical) (NE.toList args)
                          pure (IntSet.unions fvs)
                        Just TyVarApp {tnVarHead = headNode, tnArgs = args} -> do
                          let visited' = IntSet.insert key visited
                          headFvs <- go bound visited' (canonical headNode)
                          argFvs <- mapM (go bound visited' . canonical) (NE.toList args)
                          pure (IntSet.unions (headFvs : argFvs))
                        Just TyForall {tnId = fId, tnBody = b} -> do
                          let visited' = IntSet.insert key visited
                          binders <- Binding.boundFlexChildrenUnder canonical constraint (typeRef (canonical fId))
                          let bound' =
                                bound
                                  `IntSet.union` IntSet.fromList (map (getNodeId . canonical) binders)
                          go bound' visited' (canonical b)
                        Just TyMu {tnId = muId, tnBody = b} -> do
                          let visited' = IntSet.insert key visited
                          binders <- Binding.boundFlexChildrenUnder canonical constraint (typeRef (canonical muId))
                          let bound' =
                                bound
                                  `IntSet.union` IntSet.fromList (map (getNodeId . canonical) binders)
                          go bound' visited' (canonical b)
                        Just TyExp {tnBody = b} -> do
                          let visited' = IntSet.insert key visited
                          go bound visited' (canonical b)
           in go IntSet.empty IntSet.empty (canonical nid0)

        assertBindingCoverage :: SurfaceExpr -> IO ()
        assertBindingCoverage expr = do
          (solved, scopeRoot, typeRoot) <- requireRight (runSolvedWithScope expr)
          freeVars <- requireRight (freeVarsUnder solved typeRoot)
          freeVars `shouldSatisfy` (not . IntSet.null)
          let canonical = Solved.canonical solved
              constraint = Solved.originalConstraint solved
              scopeRootC = case scopeRoot of
                TypeRef nid -> typeRef (canonical nid)
                GenRef gid -> GenRef gid
          forM_ (IntSet.toList freeVars) $ \vid -> do
            let v = typeRef (NodeId vid)
            path <- requireRight (bindingPathToRootUnder canonical constraint v)
            let hasRoot = scopeRootC `elem` path
            when (not hasRoot) $
              expectationFailure $
                "Free var missing binding path to scope root: "
                  ++ show v
                  ++ " path "
                  ++ show path

    it "covers free vars for top-level lambda" $ do
      let expr = ELam "x" (EVar "x")
      assertBindingCoverage expr

    it "covers free vars for let-polymorphic instantiation (f 1)" $ do
      -- let f = \x. \y. x in f 1  ==>  a -> Int
      -- The free var in the result (a) comes from instantiation copying.
      let expr =
            ELet
              "f"
              (ELam "x" (ELam "y" (EVar "x")))
              (EApp (EVar "f") (ELit (LInt 1)))
      assertBindingCoverage expr

  describe "Elaboration of Bounded Quantification (Flexible Bounds)" $ do
    it "elaborates let with RHS term annotation (coercion) and flexible bound (Int -> Int)" $ do
      -- let f = (\x. x : ∀(a ⩾ Int -> Int). a -> a) in f
      -- The RHS annotation is a term coercion (not a declared scheme).
      -- The coercion constrains the RHS to match the annotation type.
      let bound = STArrow (STBase "Int") (STBase "Int")
          ann = mkForalls [("a", Just bound)] (STArrow (STVar "a") (STVar "a"))
          expr = ELet "f" (EAnn (ELam "x" (EVar "x")) ann) (EVar "f")

      (term, ty) <- requirePipeline expr
      let termStr = Elab.prettyDisplay term
      termStr `shouldSatisfy` ("let f =" `isInfixOf`)
      termStr `shouldSatisfy` ("λ(" `isInfixOf`)

      Elab.prettyDisplay ty `shouldBe` "(Int -> Int) -> Int -> Int"

    it "elaborates let with RHS term annotation (coercion) and polymorphic bound (Rank-2ish)" $ do
      -- let f = (\x. x : ∀(a ⩾ ∀b. b -> b). a -> a) in f
      -- The RHS annotation is a term coercion (not a declared scheme).
      let innerBound = STForall "b" Nothing (STArrow (STVar "b") (STVar "b"))
          ann = mkForalls [("a", Just innerBound)] (STArrow (STVar "a") (STVar "a"))
          expr = ELet "f" (EAnn (ELam "x" (EVar "x")) ann) (EVar "f")

      (_term, ty) <- requirePipeline expr
      let expected =
            testTForall "a"
              (Just (boundFromType (testTForall "b" Nothing (Elab.TArrow (testTVar "b") (testTVar "b")))))
              (Elab.TArrow (testTVar "a") (testTVar "a"))
      ty `shouldAlphaEqType` expected

    it "elaborates lambda with rank-2 argument (US-004)" $ do
      -- \x : (∀a. a -> a). x 1
      -- The principal eMLF result retains the flexible choice above Int.
      let paramTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          expr = ELamAnn "x" paramTy (EApp (EVar "x") (ELit (LInt 1)))

      (term, ty) <- requirePipeline expr
      let expected =
            testTForall
              "result"
              (Just (boundFromType (TestElab.tBase (BaseTy "Int"))))
              ( Elab.TArrow
                  (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a")))
                  (testTVar "result")
              )
      ty `shouldAlphaEqType` expected
      Elab.typeCheck term `shouldBe` Right ty
      (_checkedTerm, checkedTy) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      checkedTy `shouldAlphaEqType` ty

    it "elaborates first-class polymorphic parameter used at Int and Bool" $ do
      -- λ(poly : ∀a. a -> a) let keepInt = poly 1 in poly true
      -- This needs the argument itself to remain polymorphic after being passed
      -- as a value; ordinary rank-1 let-polymorphism is not enough.  The
      -- principal eMLF result retains the flexible choice above Bool rather
      -- than prematurely selecting its Bool instance.
      let source = "λ(poly : ∀a. a -> a) let keepInt = poly 1 in poly true"
      expr <-
        case parseRawEmlfExpr source of
          Left err -> expectationFailure (renderEmlfParseError err) >> fail "parse failed"
          Right parsed -> pure parsed

      (_term, ty) <- requirePipeline expr
      let expected =
            testTForall
              "result"
              (Just (boundFromType (TestElab.tBase (BaseTy "Bool"))))
              ( Elab.TArrow
                  (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a")))
                  (testTVar "result")
              )
      ty `shouldAlphaEqType` expected
      (_checkedTerm, checkedTy) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      checkedTy `shouldAlphaEqType` ty

    it "keeps explicit coercion self-application typable" $ do
      let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          idTy = testTForall "id-a" Nothing (Elab.TArrow (testTVar "id-a") (testTVar "id-a"))
          expected =
            testTForall
              "result"
              (Just (boundFromType idTy))
              (Elab.TArrow idTy (testTVar "result"))
          explicitCoercion =
            ELam
              "rawG"
              ( ELet
                  "g"
                  (EAnn (EVar "rawG") sigmaId)
                  (EApp (EVar "g") (EVar "g"))
              )

      (term, ty) <- requirePipeline explicitCoercion
      ty `shouldAlphaEqType` expected
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` expected

    it "keeps annotated self-application typable through apply" $ do
      let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          idTy = testTForall "id-a" Nothing (Elab.TArrow (testTVar "id-a") (testTVar "id-a"))
          expected =
            testTForall
              "result"
              (Just (boundFromType idTy))
              (Elab.TArrow idTy (testTVar "result"))
          applyDef = ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x")))
          viaApply =
            ELet
              "apply"
              applyDef
              (ELamAnn "g" sigmaId (EApp (EApp (EVar "apply") (EVar "g")) (EVar "g")))

      (viaApplyTerm, viaApplyTy) <- requirePipeline viaApply
      viaApplyTy `shouldAlphaEqType` expected
      assertOwnResultAbstraction viaApplyTerm
      viaApplyCheckedTy <- requireRight (Elab.typeCheck viaApplyTerm)
      viaApplyCheckedTy `shouldAlphaEqType` expected

    it "keeps annotated self-application typable through eta-mediator aliases" $ do
      let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          idTy = testTForall "id-a" Nothing (Elab.TArrow (testTVar "id-a") (testTVar "id-a"))
          expected =
            testTForall
              "result"
              (Just (boundFromType idTy))
              (Elab.TArrow idTy (testTVar "result"))
          etaDef = ELam "function" (ELam "argument" (EApp (EVar "function") (EVar "argument")))
          expr =
            ELet
              "etaRoot"
              etaDef
              ( ELet
                  "etaAlias"
                  (EVar "etaRoot")
                  ( ELamAnn
                      "g"
                      sigmaId
                      (EApp (EApp (EVar "etaAlias") (EVar "g")) (EVar "g"))
                  )
              )

      (term, ty) <- requirePipeline expr
      ty `shouldAlphaEqType` expected
      assertOwnResultAbstraction term
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` expected

    it "preserves distinct producer identities through generic result construction" $ do
      let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          idTy = testTForall "id-a" Nothing (Elab.TArrow (testTVar "id-a") (testTVar "id-a"))
          expected =
            testTForall
              "result"
              (Just (boundFromType idTy))
              (Elab.TArrow idTy (testTVar "result"))
          expr =
            ELet
              "h"
              (EAnn (ELam "x" (EVar "x")) sigmaId)
              ( ELamAnn
                  "g"
                  sigmaId
                  (EApp (EVar "g") (EAnn (EVar "h") sigmaId))
              )
          identityKeysFor name = go
            where
              currentKey resolved
                | ElabTypes.resolvedVarReferenceName resolved == name =
                    [ ElabTypes.idDetailsIdentityKey
                        (ElabTypes.resolvedVarDetails resolved)
                    ]
                | otherwise = []

              go current =
                case current of
                  Elab.ELet resolved _ rhs body ->
                    currentKey resolved ++ go rhs ++ go body
                  Elab.ELam resolved body -> currentKey resolved ++ go body
                  Elab.EApp function argument -> go function ++ go argument
                  Elab.ETyInst inner _ -> go inner
                  Elab.ETyAbsRef _ _ body -> go body
                  Elab.ERoll _ body -> go body
                  Elab.EUnroll body -> go body
                  Elab.EVarNode resolved -> currentKey resolved
                  Elab.ELit _ -> []

      (term, ty) <- requirePipeline expr
      ty `shouldAlphaEqType` expected
      assertOwnResultAbstraction term
      case (identityKeysFor "g" term, identityKeysFor "h" term) of
        (gIdentity : gOccurrences@(_ : _), hIdentity : hOccurrences@(_ : _)) -> do
          gOccurrences `shouldSatisfy` all (== gIdentity)
          hOccurrences `shouldSatisfy` all (== hIdentity)
          gIdentity `shouldNotBe` hIdentity
        identities ->
          expectationFailure
            ("expected resolved g and h identities, got " ++ show identities)
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` expected

    it "infers the principal flexible result for annotated self-application" $ do
      let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          expr = ELamAnn "g" sigmaId (EApp (EVar "g") (EVar "g"))
          idTy = testTForall "id-a" Nothing (Elab.TArrow (testTVar "id-a") (testTVar "id-a"))
          expected =
            testTForall
              "result"
              (Just (boundFromType idTy))
              (Elab.TArrow idTy (testTVar "result"))

      (term, ty) <- requirePipeline expr
      ty `shouldAlphaEqType` expected
      case term of
        Elab.ETyAbsRef resultRef (Just resultBound)
          ( Elab.ELam gResolved
            ( Elab.ETyInst
              ( Elab.EApp
                (Elab.ETyInst (Elab.EVarNode functionOccurrence) (Elab.InstApp functionArgumentTy))
                (Elab.EVarNode argumentOccurrence)
              )
              (Elab.InstAbstrRef resultInstRef)
            )
          ) -> do
          ElabTypes.typeBinderRefsSameIdentity resultRef resultInstRef `shouldBe` True
          boundToType resultBound `shouldAlphaEqType` idTy
          ElabTypes.resolvedVarType gResolved `shouldAlphaEqType` idTy
          functionArgumentTy `shouldAlphaEqType` idTy
          ElabTypes.resolvedVarDetails functionOccurrence
            `shouldBe` ElabTypes.resolvedVarDetails gResolved
          ElabTypes.resolvedVarDetails argumentOccurrence
            `shouldBe` ElabTypes.resolvedVarDetails gResolved
        other ->
          expectationFailure $
            "Expected the paper's flexible result abstraction over a sigma-id lambda, saw "
              ++ Elab.prettyDisplay other
      checkedTy <- requireRight (Elab.typeCheck term)
      checkedTy `shouldAlphaEqType` expected

  describe "Elaboration bookkeeping (eliminated vars)" $ do
    it "generalizeAt inlines eliminated binders to bottom" $ do
      let v = NodeId 1
          arrow = NodeId 2
          forallNode = NodeId 3
          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId v, TyVar {tnId = v, tnBound = Nothing}),
                        (getNodeId arrow, TyArrow arrow v v),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    bindParentsFromPairs
                      [ (arrow, forallNode, BindFlex),
                        (v, forallNode, BindFlex)
                      ],
                  cEliminatedVars = IntSet.singleton (getNodeId v)
                }

      solveOut <- requireRight (solveUnifyResultWithSnapshot defaultTraceConfig c)
      solved <- requireRight (Solved.fromSolveOutput solveOut)
      (sch, _subst) <- requireRight (generalizeAt solved (typeRef forallNode) forallNode)
      sch
        `shouldBe` Elab.schemeFromType (Elab.TArrow Elab.TBottom Elab.TBottom)

    it "generalizeAt inlines eliminated binders with bounds" $ do
      let v = NodeId 1
          b = NodeId 2
          arrow = NodeId 3
          forallNode = NodeId 4
          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId v, TyVar {tnId = v, tnBound = Just b}),
                        (getNodeId b, TyVar {tnId = b, tnBound = Nothing}),
                        (getNodeId arrow, TyArrow arrow v b),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    bindParentsFromPairs
                      [ (arrow, forallNode, BindFlex),
                        (v, forallNode, BindFlex),
                        (b, forallNode, BindFlex)
                      ],
                  cEliminatedVars = IntSet.singleton (getNodeId v)
                }

      solveOut <- requireRight (solveUnifyResultWithSnapshot defaultTraceConfig c)
      solved <- requireRight (Solved.fromSolveOutput solveOut)
      (sch, _subst) <- requireRight (generalizeAt solved (typeRef forallNode) forallNode)
      Elab.prettyDisplay sch `shouldBe` "∀(a ⩾ ⊥) a -> a"

    it "generalizeAt normalizes inter-binder alias bounds to unbounded (no ∀(b ⩾ a))" $ do
      let a = NodeId 1
          b = NodeId 2
          arrow = NodeId 3
          forallNode = NodeId 4
          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId a, TyVar {tnId = a, tnBound = Nothing}),
                        (getNodeId b, TyVar {tnId = b, tnBound = Just a}),
                        (getNodeId arrow, TyArrow arrow b b),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    bindParentsFromPairs
                      [ (arrow, forallNode, BindFlex),
                        (a, forallNode, BindFlex),
                        (b, forallNode, BindFlex)
                      ]
                }

      solveOut <- requireRight (solveUnifyResultWithSnapshot defaultTraceConfig c)
      solved <- requireRight (Solved.fromSolveOutput solveOut)
      -- Inter-binder alias bounds are now normalized to unbounded
      -- (see Note [Inter-binder alias bounds in recursive types] in ReifyPlan.hs)
      case generalizeAt solved (typeRef forallNode) forallNode of
        Left err ->
          expectationFailure ("Expected success but got: " ++ show err)
        Right _ ->
          pure ()

    it "originalConstraint preserves solved-away binder after unification" $ do
      -- Construct ∀α. α → α with a unify edge α = Int.
      -- After solving, α is merged into Int in the union-find,
      -- but originalConstraint should still have the TyVar for α.
      let alpha = NodeId 1
          intNode = NodeId 2
          arrow = NodeId 3
          forallNode = NodeId 4
          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId alpha, TyVar {tnId = alpha, tnBound = Nothing}),
                        (getNodeId intNode, TestTyBase intNode (BaseTy "Int")),
                        (getNodeId arrow, TyArrow arrow alpha alpha),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    bindParentsFromPairs
                      [ (arrow, forallNode, BindFlex),
                        (alpha, forallNode, BindFlex),
                        (intNode, forallNode, BindFlex)
                      ],
                  cUnifyEdges = [UnifyEdge alpha intNode]
                }

      solveOut <- requireRight (solveUnifyResultWithSnapshot defaultTraceConfig c)
      solved <- requireRight (Solved.fromSolveOutput solveOut)

      -- After solving, canonical(alpha) should be intNode
      Solved.canonical solved alpha `shouldBe` intNode

      -- originalConstraint should still have the TyVar for alpha
      let origC = Solved.originalConstraint solved
      case lookupNodeIn (cNodes origC) alpha of
        Just TyVar {} -> pure ()
        other ->
          expectationFailure $
            "Expected TyVar for alpha in originalConstraint, got: " ++ show other

      -- canonicalConstraint should map alpha to Int
      let solvedC = Solved.canonicalConstraint solved
      case lookupNodeIn (cNodes solvedC) (Solved.canonical solved alpha) of
        Just (TestTyBase _ (BaseTy "Int")) -> pure ()
        other ->
          expectationFailure $
            "Expected TestTyBase Int for canonical(alpha) in canonicalConstraint, got: " ++ show other

  describe "xMLF types (instance bounds)" $ do
    it "pretty prints unbounded forall" $ do
      let ty :: Elab.ElabType
          ty = testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))
      Elab.pretty ty `shouldBe` "∀(a ⩾ ⊥) a -> a"

    it "pretty prints bounded forall" $ do
      let bound = Elab.TArrow (TestElab.tBase (BaseTy "Int")) (TestElab.tBase (BaseTy "Int"))
          ty :: Elab.ElabType
          ty = testTForall "a" (Just (boundFromType bound)) (testTVar "a")
      Elab.pretty ty `shouldBe` "∀(a ⩾ Int -> Int) a"

    it "pretty prints nested bounded forall" $ do
      let innerBound = Elab.TArrow (testTVar "b") (testTVar "b")
          inner = testTForall "b" Nothing innerBound
          outer :: Elab.ElabType
          outer = testTForall "a" (Just (boundFromType inner)) (testTVar "a")
      Elab.pretty outer `shouldBe` "∀(a ⩾ ∀(b ⩾ ⊥) b -> b) a"

    it "pretty prints bottom type" $ do
      Elab.pretty (Elab.TBottom :: Elab.ElabType) `shouldBe` "⊥"

    it "keeps type binder refs identity-sensitive in types" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
              "a"
          intTy = TestElab.tBase (BaseTy "Int")
          varA = ElabTypes.tVarWithRef refA
          varB = ElabTypes.tVarWithRef refB
          forallA :: Elab.ElabType
          forallA = ElabTypes.tForallWithRef refA Nothing varA
          forallB :: Elab.ElabType
          forallB = ElabTypes.tForallWithRef refB Nothing varB
          muA :: Elab.ElabType
          muA = ElabTypes.tMuWithRef refA varA
          muB :: Elab.ElabType
          muB = ElabTypes.tMuWithRef refB varB
          appA :: Elab.ElabType
          appA = ElabTypes.tVarAppWithRef refA (intTy NE.:| [])
          appB :: Elab.ElabType
          appB = ElabTypes.tVarAppWithRef refB (intTy NE.:| [])
          appBoundA :: Elab.BoundType
          appBoundA = ElabTypes.tVarAppWithRef refA (intTy NE.:| [])
          schemeInfo =
            mkSchemeInfoFromNodeNames
              (ElabTypes.mkElabSchemeWithRefs [(refA, Nothing)] varA)
              (IntMap.singleton 1 "a")
          schemeTy =
            ElabTypes.tForallWithRef
              refA
              Nothing
              varA
      Elab.pretty forallA `shouldBe` "∀(a ⩾ ⊥) a"
      varA == varB `shouldBe` False
      forallA == forallB `shouldBe` False
      muA == muB `shouldBe` False
      appA == appB `shouldBe` False
      ElabTypes.tyToElab appBoundA `shouldBe` appA
      ElabTypes.elabToBound appA `shouldBe` Right appBoundA
      Elab.schemeToType (Elab.siScheme schemeInfo) `shouldBe` schemeTy
      case forallA of
        Elab.TForallRef ref Nothing _ ->
          ElabTypes.typeBinderRefName ref `shouldBe` "a"
        other ->
          expectationFailure ("Expected TForall pattern, got: " ++ show other)

    it "preserves resolved type-head identities in ElabType transforms" $ do
      let boxIdentity =
            symbolIdentityFromParts (UniqueIdentity 90901) SymbolType "Main" "Box" Nothing
          boxArg :: Elab.ElabType
          boxArg = ElabTypes.TBaseWithIdentity boxIdentity (BaseTy "stale.Box")
          boxElab :: Elab.ElabType
          boxElab = ElabTypes.TConWithIdentity boxIdentity (BaseTy "stale.Box") (boxArg NE.:| [])
          boxBound :: Elab.BoundType
          boxBound = ElabTypes.TConWithIdentity boxIdentity (BaseTy "stale.Box") (boxArg NE.:| [])
      ElabTypes.tyToElab boxBound `shouldBe` boxElab
      ElabTypes.elabToBound boxElab `shouldBe` Right boxBound
      ElabTypes.generatedIdentitiesInType boxElab `shouldSatisfy` elem (UniqueIdentity 90901)

    it "resolves finalize TypeView heads to carried identities" $ do
      let scope = ProgramElaborate.mkElaborateScope Map.empty Map.empty Map.empty []
          view =
            mkTypeView (STBase "Int") (STBase "Int")
      ProgramFinalize.typeViewToElabType scope view
        `shouldBe` Right
          ( ElabTypes.TBaseWithIdentity
              (ProgramBuiltins.builtinTypeIdentity "Int")
              (BaseTy "Int")
          )

  describe "xMLF instantiation witnesses" $ do
    it "pretty prints identity instantiation" $ do
      Elab.pretty Elab.InstId `shouldBe` "ε"

    it "pretty prints type application" $ do
      let inst = Elab.InstApp (TestElab.tBase (BaseTy "Int"))
      Elab.pretty inst `shouldBe` "∀(⩾ ⊲Int); N"

    it "pretty prints intro (skip forall)" $ do
      Elab.pretty Elab.InstIntro `shouldBe` "O"

    it "pretty prints elim (eliminate forall)" $ do
      Elab.pretty Elab.InstElim `shouldBe` "N"

    it "pretty prints abstract bound" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1520))
              "a"
      Elab.pretty (ElabTypes.instAbstrWithRef refA) `shouldBe` "a⊳"

    it "keeps instantiation binder refs identity-sensitive" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
              "a"
      Elab.pretty (ElabTypes.instAbstrWithRef refA) `shouldBe` "a⊳"
      (ElabTypes.instAbstrWithRef refA == ElabTypes.instAbstrWithRef refB) `shouldBe` False
      (ElabTypes.instUnderWithRef refA Elab.InstId == ElabTypes.instUnderWithRef refB Elab.InstId) `shouldBe` False

    it "pretty prints under instantiation" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1536))
              "a"
          inst = ElabTypes.instUnderWithRef refA (Elab.InstApp (TestElab.tBase (BaseTy "Int")))
      Elab.pretty inst `shouldBe` "∀(a ⩾) (∀(⩾ ⊲Int); N)"

    it "pretty prints inside instantiation" $ do
      let inst = Elab.InstInside (Elab.InstApp (TestElab.tBase (BaseTy "Int")))
      Elab.pretty inst `shouldBe` "∀(⩾ ∀(⩾ ⊲Int); N)"

    it "pretty prints composed instantiation" $ do
      let inst = Elab.InstSeq (Elab.InstApp (TestElab.tBase (BaseTy "Int"))) Elab.InstIntro
      Elab.pretty inst `shouldBe` "∀(⩾ ⊲Int); N; O"

    it "pretty prints bottom instantiation" $ do
      let inst = Elab.InstBot (TestElab.tBase (BaseTy "Int"))
      Elab.pretty inst `shouldBe` "⊲Int"

  describe "xMLF instantiation semantics (applyInstantiation)" $ do
    it "O14-APPLY-N: InstElim substitutes the binder with its bound (default ⊥)" $ do
      let ty = testTForall "a" Nothing (testTVar "a")
      out <- requireRight (Elab.applyInstantiation ty Elab.InstElim)
      out `shouldBe` Elab.TBottom

    it "InstElim substitutes the binder with an explicit bound" $ do
      let ty = testTForall "a" (Just (boundFromType (TestElab.tBase (BaseTy "Int")))) (testTVar "a")
      out <- requireRight (Elab.applyInstantiation ty Elab.InstElim)
      out `shouldBe` TestElab.tBase (BaseTy "Int")

    it "O14-APPLY-INNER: InstInside can update a ⊥ bound to a concrete bound" $ do
      let ty = testTForall "a" Nothing (testTVar "a")
          inst = Elab.InstInside (Elab.InstBot (TestElab.tBase (BaseTy "Int")))
      out <- requireRight (Elab.applyInstantiation ty inst)
      out `shouldBe` testTForall "a" (Just (boundFromType (TestElab.tBase (BaseTy "Int")))) (testTVar "a")

    it "O14-APPLY-OUTER O14-APPLY-HYP: InstUnder applies to the body and renames the instantiation binder" $ do
      let ty = testTForall "a" Nothing (testTVar "zzz")
          refX =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1570))
              "x"
          inst = ElabTypes.instUnderWithRef refX (ElabTypes.instAbstrWithRef refX)
      out <- requireRight (Elab.applyInstantiation ty inst)
      out `shouldBe` testTForall "a" Nothing (testTVar "a")

    it "applyInstantiation preserves forall binder refs when rewriting bounds and bodies" $ do
      let intTy = TestElab.tBase (BaseTy "Int")
          refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1554))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1555))
              "b"
          ty = ElabTypes.tForallWithRef refA Nothing (ElabTypes.tVarWithRef refA)
      inside <- requireRight (Elab.applyInstantiation ty (Elab.InstInside (Elab.InstBot intTy)))
      inside `shouldBe` ElabTypes.tForallWithRef refA (Just (boundFromType intTy)) (ElabTypes.tVarWithRef refA)
      under <- requireRight (Elab.applyInstantiation ty (ElabTypes.instUnderWithRef refB Elab.InstId))
      under `shouldBe` ty

    it "applyInstantiation preserves InstAbstr refs during abstract elimination" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1556))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1557))
              "b"
          ty = ElabTypes.tForallWithRef refA Nothing (ElabTypes.tVarWithRef refA)
          inst = Elab.InstSeq (Elab.InstInside (ElabTypes.instAbstrWithRef refB)) Elab.InstElim
      out <- requireRight (Elab.applyInstantiation ty inst)
      out `shouldBe` ElabTypes.tVarWithRef refB

    it "applyInstantiation renames InstUnder refs to the target forall identity" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1558))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1559))
              "b"
          ty = ElabTypes.tForallWithRef refA Nothing (testTVar "zzz")
          inst = ElabTypes.instUnderWithRef refB (ElabTypes.instAbstrWithRef refB)
      out <- requireRight (Elab.applyInstantiation ty inst)
      out `shouldBe` ElabTypes.tForallWithRef refA Nothing (ElabTypes.tVarWithRef refA)

    it "applyInstantiation renames InstUnder type payload refs to the target forall identity" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1558))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1559))
              "b"
          refU =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1560))
              "u"
          payloadB = Elab.TArrow (ElabTypes.tVarWithRef refB) (ElabTypes.tVarWithRef refB)
          payloadA = Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refA)
          ty =
            ElabTypes.tForallWithRef
              refA
              Nothing
              (ElabTypes.tForallWithRef refU Nothing (ElabTypes.tVarWithRef refU))
          instBotPayload = ElabTypes.instUnderWithRef refB (Elab.InstInside (Elab.InstBot payloadB))
          instAppPayload = ElabTypes.instUnderWithRef refB (Elab.InstApp payloadB)
      botOut <- requireRight (Elab.applyInstantiation ty instBotPayload)
      appOut <- requireRight (Elab.applyInstantiation ty instAppPayload)
      botOut
        `shouldBe` ElabTypes.tForallWithRef
          refA
          Nothing
          (ElabTypes.tForallWithRef refU (Just (boundFromType payloadA)) (ElabTypes.tVarWithRef refU))
      appOut `shouldBe` ElabTypes.tForallWithRef refA Nothing payloadA

    it "O14-APPLY-SEQ: InstApp behaves like (∀(⩾ τ); N) on the outermost quantifier" $ do
      let ty = testTForall "a" Nothing (testTVar "a")
      out <- requireRight (Elab.applyInstantiation ty (Elab.InstApp (TestElab.tBase (BaseTy "Int"))))
      out `shouldBe` TestElab.tBase (BaseTy "Int")

    it "InstApp accepts arg matching explicit bound on ∀(a ≥ Int). a" $ do
      let ty = testTForall "a" (Just (boundFromType (TestElab.tBase (BaseTy "Int")))) (testTVar "a")
      out <- requireRight (Elab.applyInstantiation ty (Elab.InstApp (TestElab.tBase (BaseTy "Int"))))
      out `shouldBe` TestElab.tBase (BaseTy "Int")

    it "InstApp rejects arg not matching explicit bound on ∀(a ≥ Int). a" $ do
      let ty = testTForall "a" (Just (boundFromType (TestElab.tBase (BaseTy "Int")))) (testTVar "a")
      case Elab.applyInstantiation ty (Elab.InstApp (TestElab.tBase (BaseTy "Bool"))) of
        Left (Elab.InstantiationError _) -> pure ()
        Left err -> expectationFailure ("Expected InstantiationError, got: " ++ show err)
        Right t -> expectationFailure ("Expected failure, got: " ++ show t)

    it "O14-APPLY-ID: InstId leaves the input type unchanged" $ do
      let ty = Elab.TArrow (TestElab.tBase (BaseTy "Int")) (TestElab.tBase (BaseTy "Bool"))
      out <- requireRight (Elab.applyInstantiation ty Elab.InstId)
      out `shouldBe` ty

    it "O14-APPLY-O: InstIntro introduces a trivial quantification" $ do
      out <- requireRight (Elab.applyInstantiation (TestElab.tBase (BaseTy "Int")) Elab.InstIntro)
      case out of
        Elab.TForallRef _ Nothing body ->
          body `shouldBe` TestElab.tBase (BaseTy "Int")
        other ->
          expectationFailure ("Expected forall-introduced type, got: " ++ show other)

    it "InstIntro seeds fresh binders after instantiation payload identities" $ do
      let reservedIdentity = UniqueIdentity 2000000060
          reservedRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique reservedIdentity)
              "payload"
          inst =
            Elab.InstSeq
              Elab.InstIntro
              (Elab.InstInside (Elab.InstBot (ElabTypes.tVarWithRef reservedRef)))
      out <- requireRight (Elab.applyInstantiation (TestElab.tBase (BaseTy "Int")) inst)
      case out of
        Elab.TForallRef ref _ _ ->
          ElabTypes.typeBinderRefIdentity ref
            `shouldBe` ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 2000000061)
        other ->
          expectationFailure ("Expected forall-introduced type, got: " ++ show other)

    it "14.2.1/14.2.7 determinism proxy: InstApp equals InstInside;InstElim application" $ do
      let src = testTForall "a" Nothing (testTVar "a")
          tgt = TestElab.tBase (BaseTy "Int")
      lhs <- requireRight (Elab.applyInstantiation src (Elab.InstApp tgt))
      rhs <- requireRight (Elab.applyInstantiation src (Elab.InstSeq (Elab.InstInside (Elab.InstBot tgt)) Elab.InstElim))
      lhs `shouldBe` rhs

    it "fails InstElim on a non-∀ type" $ do
      case Elab.applyInstantiation (TestElab.tBase (BaseTy "Int")) Elab.InstElim of
        Left (Elab.InstantiationError _) -> pure ()
        Left err -> expectationFailure ("Expected InstantiationError, got: " ++ show err)
        Right t -> expectationFailure ("Expected failure, got: " ++ show t)

    it "fails InstInside on a non-∀ type" $ do
      let inst = Elab.InstInside (Elab.InstBot (TestElab.tBase (BaseTy "Int")))
      case Elab.applyInstantiation (TestElab.tBase (BaseTy "Int")) inst of
        Left (Elab.InstantiationError _) -> pure ()
        Left err -> expectationFailure ("Expected InstantiationError, got: " ++ show err)
        Right t -> expectationFailure ("Expected failure, got: " ++ show t)

    it "fails InstUnder on a non-∀ type" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1669))
              "a"
      case Elab.applyInstantiation (TestElab.tBase (BaseTy "Int")) (ElabTypes.instUnderWithRef refA Elab.InstId) of
        Left (Elab.InstantiationError _) -> pure ()
        Left err -> expectationFailure ("Expected InstantiationError, got: " ++ show err)
        Right t -> expectationFailure ("Expected failure, got: " ++ show t)

    it "O14-APPLY-BOT: fails InstBot on a non-⊥ type" $ do
      case Elab.applyInstantiation (TestElab.tBase (BaseTy "Int")) (Elab.InstBot (TestElab.tBase (BaseTy "Bool"))) of
        Left (Elab.InstantiationError _) -> pure ()
        Left err -> expectationFailure ("Expected InstantiationError, got: " ++ show err)
        Right t -> expectationFailure ("Expected failure, got: " ++ show t)

    it "fails InstBot when argument equals non-bottom input type" $ do
      let ty = Elab.TArrow (TestElab.tBase (BaseTy "Int")) (TestElab.tBase (BaseTy "Int"))
      case Elab.applyInstantiation ty (Elab.InstBot ty) of
        Left (Elab.InstantiationError _) -> pure ()
        Left err -> expectationFailure ("Expected InstantiationError, got: " ++ show err)
        Right t -> expectationFailure ("Expected strict InstBot failure, got: " ++ show t)

    it "URI-R2-C1 empty witness replay is identity and retains the direct source forall" $ do
      fixture <- uriR2C1ReplayFixture
      uriR2C1ReplaySchemeType fixture
        `shouldAlphaEqType` testTForall
          "a"
          Nothing
          (Elab.TArrow (testTVar "a") (testTVar "a"))
      -- Compare the closed forall types.  Their open bodies intentionally
      -- carry distinct lexical binder identities (inferred versus annotated),
      -- so demanding free-identity equality after stripping the foralls would
      -- be a false route requirement.
      uriR2C1ReplayNoFallbackType fixture
        `shouldAlphaEqType` uriR2C1ReplaySchemeType fixture
      -- Thesis Figure 15.3.4 (papers/these-finale-english.txt):
      -- Tχ() = ε.  The explicit source forall is already present in Typ(a′);
      -- an empty replay must not manufacture a second, vacuous quantifier.
      uriR2C1ReplayPhi fixture `shouldBe` Elab.InstId
      replayedTy <-
        requireRight
          (Elab.applyInstantiation (uriR2C1ReplaySchemeType fixture) (uriR2C1ReplayPhi fixture))
      replayedTy `shouldAlphaEqType` uriR2C1ReplaySchemeType fixture

    it "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables" $ do
      let ty = testTForall "a" (Just (boundFromType (TestElab.tBase (BaseTy "Int")))) (testTVar "a")
          inst = Elab.InstInside (Elab.InstBot (TestElab.tBase (BaseTy "Int")))
      case Elab.applyInstantiation ty inst of
        Left (Elab.InstantiationError msg) ->
          msg `shouldBe` "InstBot expects ⊥, got: Int"
        Left err ->
          expectationFailure ("Expected strict InstBot failure, got: " ++ show err)
        Right replayedTy ->
          expectationFailure ("Expected strict InstBot failure, got: " ++ Elab.pretty replayedTy)

    it "InstInside(InstBot (TVar _)) still rejects explicit non-bottom bounds outside the replay lane" $ do
      let bound =
            Elab.TArrow
              (testTVar "u")
              (testTVar "u")
          ty = testTForall "a" (Just (boundFromType bound)) (testTVar "a")
          inst = Elab.InstInside (Elab.InstBot (testTVar "x"))
      case Elab.applyInstantiation ty inst of
        Left (Elab.InstantiationError msg) ->
          msg `shouldBe` "InstBot expects ⊥, got: u -> u"
        Left err ->
          expectationFailure ("Expected strict InstBot failure, got: " ++ show err)
        Right replayedTy ->
          expectationFailure ("Expected strict InstBot failure, got: " ++ Elab.pretty replayedTy)

    it "BUG-2026-03-16-001 regression: InstBot accepts replay-resolved bound match" $ do
      -- Minimal reproduction of the InstBot replay path:
      --   Type: ∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b
      --   Phi:  ∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N)
      --
      -- The pipeline emits InstApp for this shape (InstApp combines
      -- InstInside(InstBot(τ)) + InstElim into one step that directly
      -- substitutes the checked argument into the body, bypassing the
      -- BoundType GADT restriction that prevents TVar from being stored
      -- as a forall bound).
      --
      -- After the first InstApp(t9) peels ∀(a ⩾ ⊥) and adds {a ↦ t9}
      -- to the replay env, the second InstApp(a -> a) sees the resolved
      -- bound (t9 -> t9) and resolves tArg (a -> a) to (t9 -> t9) via
      -- the replay env. The resolved arg matches the resolved bound, so
      -- allowReplayBoundMatch accepts it. Without that fix, this would
      -- fail with "InstBot expects ⊥, got: t9 -> t9".
      let ty =
            testTForall "a"
              (Just (boundFromType Elab.TBottom))
              ( testTForall "b"
                  (Just (boundFromType (Elab.TArrow (testTVar "a") (testTVar "a"))))
                  (testTVar "b")
              )
          phi =
            Elab.InstSeq
              (Elab.InstApp (graphTVar 9))
              (Elab.InstApp (Elab.TArrow (testTVar "a") (testTVar "a")))
      result <- requireRight (Elab.applyInstantiation ty phi)
      -- The result should be alpha-equivalent to t9 -> t9
      shouldEqUpToTypeVarRenaming result (Elab.TArrow (testTVar "t9") (testTVar "t9"))

    it "normalizeInst roundtrip: rule 3 prefix-arg collapse preserves applyInstantiation" $ do
      -- Build an instantiation that matches rule 3: prefix ; intro ; app ; under beta (abstr beta ; elim) ; elim
      let tArg = TestElab.tBase (BaseTy "Int")
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1772))
              "b"
          prefix = Elab.InstInside (Elab.InstBot tArg)
          appArg = Elab.InstInside (Elab.InstBot tArg)
          original =
            Elab.InstSeq
              ( Elab.InstSeq
                  prefix
                  ( Elab.InstSeq
                      Elab.InstIntro
                      ( Elab.InstSeq
                          appArg
                          ( ElabTypes.instUnderWithRef
                              refB
                              (Elab.InstSeq (Elab.InstInside (ElabTypes.instAbstrWithRef refB)) Elab.InstElim)
                          )
                      )
                  )
              )
              Elab.InstElim
          normalized = Elab.InstApp tArg
          ty = testTForall "a" Nothing (testTVar "a")
      lhs <- requireRight (Elab.applyInstantiation ty original)
      rhs <- requireRight (Elab.applyInstantiation ty normalized)
      lhs `shouldBe` rhs

    it "normalizeInst collapses context-wrapped graft+weaken to InstApp (Rule 1b)" $ do
      let tArg = TestElab.tBase (BaseTy "Int")
          refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1790))
              "a"
          original =
            Elab.InstSeq
              (ElabTypes.instUnderWithRef refA (Elab.InstInside (Elab.InstBot tArg)))
              (ElabTypes.instUnderWithRef refA Elab.InstElim)
          normalized = ElabTypes.instUnderWithRef refA (Elab.InstApp tArg)
          ty = testTForall "a" Nothing (testTForall "b" Nothing (testTVar "b"))
      lhs <- requireRight (Elab.applyInstantiation ty original)
      rhs <- requireRight (Elab.applyInstantiation ty normalized)
      lhs `shouldBe` rhs

    it "normalizeInst preserves binder refs when collapsing context-wrapped graft+weaken" $ do
      let tArg = TestElab.tBase (BaseTy "Int")
          ref =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1700))
              "a"
          renamedRef = ElabTypes.renameTypeBinderRef "renamed-a" ref
          original =
            Elab.InstSeq
              (ElabTypes.instUnderWithRef ref (Elab.InstInside (Elab.InstBot tArg)))
              (ElabTypes.instUnderWithRef renamedRef Elab.InstElim)
          expected = ElabTypes.instUnderWithRef ref (Elab.InstApp tArg)
      PhiTestSupport.normalizeInst original `shouldBe` expected

  describe "xMLF terms" $ do
    it "pretty prints type abstraction with bound" $ do
      let bound = Elab.TArrow (testTVar "b") (testTVar "b")
          term = mkTestTyAbs "a" (Just (boundFromType bound)) (mkTestDeferredVar "x")
      Elab.pretty term `shouldBe` "Λ(a ⩾ b -> b) x"

    it "pretty prints unbounded type abstraction" $ do
      let term = mkTestTyAbs "a" Nothing (mkTestDeferredVar "x")
      Elab.pretty term `shouldBe` "Λ(a ⩾ ⊥) x"

    it "keeps type abstraction binder refs identity-sensitive" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
              "a"
          termA = ElabTypes.eTyAbsWithRef refA Nothing (Elab.ELit (LInt 1))
          termB = ElabTypes.eTyAbsWithRef refB Nothing (Elab.ELit (LInt 1))
      Elab.pretty termA `shouldBe` "Λ(a ⩾ ⊥) 1"
      termA == termB `shouldBe` False
      case termA of
        Elab.ETyAbsRef ref Nothing _ ->
          ElabTypes.typeBinderRefName ref `shouldBe` "a"
        other ->
          expectationFailure ("Expected ETyAbs pattern, got: " ++ show other)

    it "prettyDisplay drops unused same-named forall binders by identity" $ do
      let outerRef = graphTypeBinderRef 1892 "a"
          freeRef = graphTypeBinderRef 1893 "a"
          ty :: Elab.ElabType
          ty = ElabTypes.tForallWithRef outerRef Nothing (ElabTypes.tVarWithRef freeRef)
      Elab.prettyDisplay ty `shouldBe` "a"

    it "pretty prints type instantiation" $ do
      let inst = Elab.InstApp (TestElab.tBase (BaseTy "Int"))
          term = Elab.ETyInst (mkTestDeferredVar "f") inst
      Elab.pretty term `shouldBe` "f[∀(⩾ ⊲Int); N]"

    it "pretty prints let as a checked diagnostic dump" $ do
      let scheme = Elab.schemeFromType (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a")))
          term = mkTestLocalLet "id" scheme (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))) (mkTestDeferredVar "id")
      Elab.pretty term `shouldBe` "let id = Λ(a ⩾ ⊥) λ(x : a) x in id"

    it "pretty prints resolved locals by identity instead of runtime spelling" $ do
      let intTy = TestElab.tBase (BaseTy "Int")
          resolved ref _runtime =
            ResolvedVar
              {
                resolvedVarType = intTy,
                resolvedVarDetails = LocalId (generatedLocalRefForName ref)
              }
          term =
            Elab.ELam
              (resolved "$x#0" "runtime-x")
              (Elab.EVarNode (resolved "$x#0" "different-runtime"))
      Elab.pretty term `shouldBe` "λ($x#0 : Int) $x#0"

    it "freshens type abstractions without dropping resolved local sidecars" $ do
      let envRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1864))
              "a"
          absRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1865))
              "a"
          captured =
            ResolvedVar
              {
                resolvedVarType = ElabTypes.tVarWithRef envRef,
                resolvedVarDetails =
                  EnvId (envRefFromIdentity (UniqueIdentity 1866) "captured")
              }
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(captured, ElabTypes.tVarWithRef envRef)] Map.empty
          resolved ref _runtime ty =
            ResolvedVar
              {
                resolvedVarType = ty,
                resolvedVarDetails = LocalId (generatedLocalRefForName ref)
              }
          term =
            ElabTypes.eTyAbsWithRef absRef Nothing $
              Elab.ELam
                (resolved "$x#0" "runtime-x" (ElabTypes.tVarWithRef absRef))
                (Elab.EVarNode (resolved "$x#0" "different-runtime" (ElabTypes.tVarWithRef absRef)))
      case Elab.freshenTypeAbsAgainstEnv env term of
        Elab.ETyAbsRef freshRef Nothing (Elab.ELam binder (Elab.EVarNode occurrence)) -> do
          ElabTypes.typeBinderRefIdentity freshRef `shouldBe` ElabTypes.typeBinderRefIdentity absRef
          ElabTypes.typeBinderRefName freshRef `shouldBe` "a1"
          resolvedVarType binder `shouldBe` ElabTypes.tVarWithRef freshRef
          resolvedVarType occurrence `shouldBe` ElabTypes.tVarWithRef freshRef
          resolvedVarDetails binder `shouldBe` LocalId (generatedLocalRefForName "$x#0")
          resolvedVarRuntimeName occurrence `shouldBe` "$x#0"
        other -> expectationFailure ("Expected resolved freshened term, got: " ++ show other)

    it "freshens annotation type abstractions away from visible stable aliases" $ do
      let envRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1867))
              "captured"
          stableAlias = typeBinderIdentityStableName (ElabTypes.typeBinderRefIdentity envRef)
          absRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromNode (NodeId 1868))
              stableAlias
          captured =
            ResolvedVar
              {
                resolvedVarType = ElabTypes.tVarWithRef envRef,
                resolvedVarDetails =
                  EnvId (envRefFromIdentity (UniqueIdentity 1869) "captured")
              }
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(captured, ElabTypes.tVarWithRef envRef)] Map.empty
          resolved ref _runtime ty =
            ResolvedVar
              {
                resolvedVarType = ty,
                resolvedVarDetails = LocalId (generatedLocalRefForName ref)
              }
          term =
            ElabTypes.eTyAbsWithRef absRef Nothing $
              Elab.ELam
                (resolved "$x#0" "runtime-x" (ElabTypes.tVarWithRef absRef))
                (Elab.EVarNode (resolved "$x#0" "different-runtime" (ElabTypes.tVarWithRef absRef)))
      case Annotation.freshenTermTypeAbsAgainstEnv env term of
        Elab.ETyAbsRef freshRef Nothing (Elab.ELam binder (Elab.EVarNode occurrence)) -> do
          ElabTypes.typeBinderRefIdentity freshRef `shouldBe` ElabTypes.typeBinderRefIdentity absRef
          ElabTypes.typeBinderRefName freshRef `shouldNotBe` stableAlias
          resolvedVarType binder `shouldBe` ElabTypes.tVarWithRef freshRef
          resolvedVarType occurrence `shouldBe` ElabTypes.tVarWithRef freshRef
        other -> expectationFailure ("Expected annotation stable-alias freshening, got: " ++ show other)

    it "uses resolved local identity when selecting authoritative app annotations" $ do
      let intTy = TestElab.tBase (BaseTy "Int")
          resolved ref _runtime =
            ResolvedVar
              {
                resolvedVarType = intTy,
                resolvedVarDetails = LocalId (generatedLocalRefForName ref)
              }
          argAnn = ALit (LInt 1) (NodeId 1)
          appAnn =
            AApp
              (annVar "runtime-x" (NodeId 2))
              argAnn
              (appSite 0 (NodeId 2) (NodeId 3))
              (appSite 1 (NodeId 1) (NodeId 3))
              (NodeId 3)
          term =
            Elab.EApp
              ( Elab.ELam
                  (resolved "$x#0" "runtime-x")
                  (Elab.EVarNode (resolved "$x#0" "different-runtime"))
              )
              (Elab.ELit (LInt 1))
      Elab.authoritativeRootAnn term appAnn `shouldBe` argAnn

    it "uses annotation node identity instead of local names for authoritative var annotations" $ do
      let intTy = TestElab.tBase (BaseTy "Int")
          resolvedAt (NodeId node) ref _runtime =
            ResolvedVar
              {
                resolvedVarType = intTy,
                resolvedVarDetails = LocalId (localRefFromNodeId ref (NodeId node))
              }
          matchingArg = AResolvedVar (LocalId (localRefFromNodeId "$x#0" (NodeId 7))) "stale-name" (NodeId 7)
          matchingApp =
            AApp
              (annVar "runtime-x" (NodeId 2))
              matchingArg
              (appSite 0 (NodeId 2) (NodeId 3))
              (appSite 1 (NodeId 7) (NodeId 3))
              (NodeId 3)
          staleArg = AResolvedVar (LocalId (localRefFromNodeId "$x#0" (NodeId 8))) "$x#0" (NodeId 8)
          staleApp =
            AApp
              (annVar "runtime-x" (NodeId 4))
              staleArg
              (appSite 2 (NodeId 4) (NodeId 5))
              (appSite 3 (NodeId 8) (NodeId 5))
              (NodeId 5)
          term = Elab.EVarNode (resolvedAt (NodeId 7) "$x#0" "runtime-x")
      Elab.authoritativeRootAnn term matchingApp `shouldBe` matchingArg
      Elab.authoritativeRootAnn term staleApp `shouldBe` staleApp

    it "does not select authoritative var annotations by top-level name alone" $ do
      let intTy = TestElab.tBase (BaseTy "Int")
          topIdentity =
            symbolIdentityFromParts (UniqueIdentity 991901) SymbolValue "Main" "x" Nothing
          resolved =
            ResolvedVar
              {
                resolvedVarType = intTy,
                resolvedVarDetails = TopLevelId topIdentity
              }
          matchingByNameArg = annVar "runtime-x" (NodeId 7)
          appAnn =
            AApp
              (annVar "runtime-x" (NodeId 2))
              matchingByNameArg
              (appSite 0 (NodeId 2) (NodeId 3))
              (appSite 1 (NodeId 7) (NodeId 3))
              (NodeId 3)
          term = Elab.EVarNode resolved
      Elab.authoritativeRootAnn term appAnn `shouldBe` appAnn

    it "uses let scheme-root identity instead of local names for authoritative let annotations" $ do
      let intTy = TestElab.tBase (BaseTy "Int")
          resolvedAt (NodeId node) ref _runtime =
            ResolvedVar
              {
                resolvedVarType = intTy,
                resolvedVarDetails = LocalId (localRefFromNodeId ref (NodeId node))
              }
          bodyAnn = AResolvedVar (LocalId (localRefFromNodeId "body" (NodeId 20))) "body" (NodeId 20)
          matchingAnn =
            ALet
              "$x#0"
              (LocalId (localRefFromNodeId "$x#0" (NodeId 7)))
              (GenNodeId 0)
              (NodeId 7)
              (ExpVarId 0)
              (GenNodeId 1)
              (ALit (LInt 1) (NodeId 9))
              bodyAnn
              (NodeId 10)
          staleAnn =
            ALet
              "$x#0"
              (LocalId (localRefFromNodeId "$x#0" (NodeId 8)))
              (GenNodeId 2)
              (NodeId 8)
              (ExpVarId 0)
              (GenNodeId 3)
              (ALit (LInt 1) (NodeId 11))
              bodyAnn
              (NodeId 12)
          term =
            Elab.ELet
              (resolvedAt (NodeId 7) "$x#0" "runtime-x")
              (Elab.schemeFromType intTy)
              (Elab.ELit (LInt 1))
              (Elab.EVarNode (resolvedAt (NodeId 20) "body" "body"))
      Elab.authoritativeRootAnn term matchingAnn `shouldBe` bodyAnn
      Elab.authoritativeRootAnn term staleAnn `shouldBe` staleAnn

  describe "eMLF source annotations" $ do
    it "allows only a direct identity-bearing occurrence to bypass exact source construction" $ do
      let exactFailure =
            Elab.PhiInvariantError
              "deliberate exact annotation construction failure"
          constructed = (Elab.ELit (LInt 0), Elab.InstId)
          directReference =
            Just (Annotation.annBinderKey (annDetails "direct-annotation-source"))
      AnnotationTestSupport.selectAnnotationSourceConstructionForTest
        Nothing
        Nothing
        (Left exactFailure)
        `shouldBe` Left exactFailure
      AnnotationTestSupport.selectAnnotationSourceConstructionForTest
        Nothing
        (Just constructed)
        (Left exactFailure)
        `shouldBe` Right
          (uncurry AnnotationTestSupport.ConstructedAnnotationSource constructed)
      AnnotationTestSupport.selectAnnotationSourceConstructionForTest
        directReference
        Nothing
        (Left exactFailure)
        `shouldBe` Right AnnotationTestSupport.WitnessAnnotationSource

    it "rejects an exact open endpoint whose leading binder is not owned by Gamma" $ do
      let binderRef = graphTypeBinderRef 992050 "a"
          binderTy = ElabTypes.tVarWithRef binderRef
          intTy = TestElab.tBase (BaseTy "Int")
          openTy = Elab.TArrow binderTy binderTy
          parameter =
            ResolvedVar
              { resolvedVarType = binderTy
              , resolvedVarDetails = LocalId (localRefFromNodeId "x" (NodeId 992051))
              }
          producerTerm =
            Elab.ETyAbsRef
              binderRef
              (Just (boundFromType intTy))
              (Elab.ELam parameter (Elab.EVarNode parameter))
          env = Elab.mkTypeCheckEnvWithResolvedTerms [] Map.empty
      Annotation.elaborateClosedExactAnnotationTermAtType
        env
        openTy
        (EdgeId 992050)
        producerTerm
        `shouldSatisfy` isLeft

    it "rejects an exact open endpoint whose Gamma bound disagrees with its leading binder" $ do
      let binderRef = graphTypeBinderRef 992052 "a"
          binderTy = ElabTypes.tVarWithRef binderRef
          intTy = TestElab.tBase (BaseTy "Int")
          boolTy = TestElab.tBase (BaseTy "Bool")
          openTy = Elab.TArrow binderTy binderTy
          parameter =
            ResolvedVar
              { resolvedVarType = binderTy
              , resolvedVarDetails = LocalId (localRefFromNodeId "x" (NodeId 992053))
              }
          producerTerm =
            Elab.ETyAbsRef
              binderRef
              (Just (boundFromType intTy))
              (Elab.ELam parameter (Elab.EVarNode parameter))
          env =
            Elab.mkTypeCheckEnvWithResolvedTerms
              []
              (Map.singleton binderRef boolTy)
      Annotation.elaborateClosedExactAnnotationTermAtType
        env
        openTy
        (EdgeId 992052)
        producerTerm
        `shouldSatisfy` isLeft

    it "accepts an exact endpoint that is already open inside its lexical Gamma" $ do
      let binderRef = graphTypeBinderRef 992054 "a"
          binderTy = ElabTypes.tVarWithRef binderRef
          intTy = TestElab.tBase (BaseTy "Int")
          producerTerm =
            Elab.ETyInst
              (Elab.ELit (LInt 1))
              (Elab.InstAbstrRef binderRef)
          env =
            Elab.mkTypeCheckEnvWithResolvedTerms
              []
              (Map.singleton binderRef intTy)
      Annotation.elaborateClosedExactAnnotationTermAtType
        env
        binderTy
        (EdgeId 992054)
        producerTerm
        `shouldBe` Right producerTerm

    it "accepts a closed exact endpoint with the prepared leading binder intact" $ do
      let binderRef = graphTypeBinderRef 992056 "a"
          binderTy = ElabTypes.tVarWithRef binderRef
          intTy = TestElab.tBase (BaseTy "Int")
          closedTy =
            ElabTypes.tForallWithRef
              binderRef
              (Just (boundFromType intTy))
              binderTy
          producerTerm =
            Elab.ETyAbsRef
              binderRef
              (Just (boundFromType intTy))
              ( Elab.ETyInst
                  (Elab.ELit (LInt 1))
                  (Elab.InstAbstrRef binderRef)
              )
          env = Elab.mkTypeCheckEnvWithResolvedTerms [] Map.empty
      Annotation.elaborateClosedExactAnnotationTermAtType
        env
        closedTy
        (EdgeId 992056)
        producerTerm
        `shouldBe` Right producerTerm

    it "rejects an alpha-equivalent recursive producer whose owner was not fixed during construction" $ do
      let producerRef = graphTypeBinderRef 992057 "producer-rec"
          exactRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992057))
              "exact-rec"
          intTy = TestElab.tBase (BaseTy "Int")
          producerRecursiveTy =
            ElabTypes.tMuWithRef
              producerRef
              (Elab.TArrow (ElabTypes.tVarWithRef producerRef) intTy)
          exactRecursiveTy =
            ElabTypes.tMuWithRef
              exactRef
              (Elab.TArrow (ElabTypes.tVarWithRef exactRef) intTy)
          producerTy = Elab.TArrow producerRecursiveTy producerRecursiveTy
          exactTy = Elab.TArrow exactRecursiveTy exactRecursiveTy
          parameter =
            ResolvedVar
              { resolvedVarType = producerRecursiveTy
              , resolvedVarDetails = LocalId (localRefFromNodeId "x" (NodeId 992058))
              }
          producerTerm = Elab.ELam parameter (Elab.EVarNode parameter)
          env = Elab.mkTypeCheckEnvWithResolvedTerms [] Map.empty
      TypeOps.alphaEqType producerTy exactTy `shouldBe` True
      Annotation.elaborateClosedExactAnnotationTermAtType
        env
        exactTy
        (EdgeId 992057)
        producerTerm
        `shouldSatisfy` isLeft

    it "constructs an exact recursive owner from sidecar authority without renaming a same-key forall" $ do
      let sharedGraphRef = graphTypeBinderRef 992059 "shared-graph"
          exactRecursiveRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992159))
              "exact-recursive"
          intTy = TestElab.tBase (BaseTy "Int")
          producerRecursiveTy =
            ElabTypes.tMuWithRef
              sharedGraphRef
              (Elab.TArrow (ElabTypes.tVarWithRef sharedGraphRef) intTy)
          exactRecursiveTy =
            ElabTypes.tMuWithRef
              exactRecursiveRef
              (Elab.TArrow (ElabTypes.tVarWithRef exactRecursiveRef) intTy)
          producerTy =
            ElabTypes.tForallWithRef
              sharedGraphRef
              Nothing
              (Elab.TArrow producerRecursiveTy producerRecursiveTy)
          exactTy =
            ElabTypes.tForallWithRef
              sharedGraphRef
              Nothing
              (Elab.TArrow exactRecursiveTy exactRecursiveTy)
          parameter =
            ResolvedVar
              { resolvedVarType = producerRecursiveTy
              , resolvedVarDetails = LocalId (localRefFromNodeId "x" (NodeId 992160))
              }
          producerTerm =
            Elab.ETyAbsRef
              sharedGraphRef
              Nothing
              (Elab.ELam parameter (Elab.EVarNode parameter))
          authority =
            IntMap.singleton
              (getNodeId (NodeId 992059))
              exactRecursiveRef
          env = Elab.mkTypeCheckEnvWithResolvedTerms [] Map.empty
      Elab.typeCheckWithEnv env producerTerm `shouldBe` Right producerTy
      case
          Annotation.elaborateClosedExactAnnotationTermAtTypeWithRecursiveOwnerAuthority
            authority
            env
            exactTy
            (EdgeId 992159)
            producerTerm
        of
          Left err -> expectationFailure ("exact recursive owner construction failed: " ++ show err)
          Right constructed -> do
            Elab.typeCheckWithEnv env constructed `shouldBe` Right exactTy
            case constructed of
              Elab.ETyAbsRef outerRef _ _ ->
                ElabTypes.typeBinderRefsSameIdentity outerRef sharedGraphRef
                  `shouldBe` True
              _ -> expectationFailure "exact recursive construction lost the outer forall"

    it "publishes an env-owned recursive producer through an exact lexical binding" $ do
      let producerRef = graphTypeBinderRef 992161 "producer-rec"
          exactRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992261))
              "exact-rec"
          intTy = TestElab.tBase (BaseTy "Int")
          producerTy =
            ElabTypes.tMuWithRef
              producerRef
              (Elab.TArrow (ElabTypes.tVarWithRef producerRef) intTy)
          exactTy =
            ElabTypes.tMuWithRef
              exactRef
              (Elab.TArrow (ElabTypes.tVarWithRef exactRef) intTy)
          producer =
            ResolvedVar
              { resolvedVarType = producerTy
              , resolvedVarDetails = LocalId (localRefFromNodeId "producer" (NodeId 992162))
              }
          authority =
            IntMap.singleton
              (getNodeId (NodeId 992161))
              exactRef
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, producerTy)] Map.empty
      TypeOps.alphaEqType producerTy exactTy `shouldBe` True
      case
          Annotation.elaborateClosedExactAnnotationTermAtTypeWithRecursiveOwnerAuthority
            authority
            env
            exactTy
            (EdgeId 992161)
            (Elab.EVarNode producer)
        of
          Left err -> expectationFailure ("env-owned recursive owner publication failed: " ++ show err)
          Right constructed -> do
            Elab.typeCheckWithEnv env constructed `shouldBe` Right exactTy
            case constructed of
              Elab.ELet published publishedScheme (Elab.EVarNode source) (Elab.EVarNode body) -> do
                publishedScheme `shouldBe` Elab.schemeFromType exactTy
                resolvedVarDetails source `shouldBe` resolvedVarDetails producer
                resolvedVarDetails body `shouldBe` resolvedVarDetails published
                resolvedVarType published `shouldBe` exactTy
              _ -> expectationFailure "env-owned recursive owner was not published by a lexical let"

    it "eliminates a bounded result under a retained binder before introducing a vacuous target forall" $ do
      let retainedRef = graphTypeBinderRef 992060 "b"
          resultRef = graphTypeBinderRef 992061 "result"
          vacuousRef = graphTypeBinderRef 992062 "ghost"
          targetRetainedRef = graphTypeBinderRef 992063 "source-b"
          boolTy = TestElab.tBase (BaseTy "Bool")
          retainedTy = ElabTypes.tVarWithRef retainedRef
          targetRetainedTy = ElabTypes.tVarWithRef targetRetainedRef
          resultBound = Elab.TArrow retainedTy retainedTy
          sourceTy =
            ElabTypes.tForallWithRef
              retainedRef
              Nothing
              ( ElabTypes.tForallWithRef
                  resultRef
                  (Just (boundFromType resultBound))
                  (Elab.TArrow boolTy (ElabTypes.tVarWithRef resultRef))
              )
          targetTy =
            ElabTypes.tForallWithRef
              vacuousRef
              Nothing
              ( ElabTypes.tForallWithRef
                  targetRetainedRef
                  Nothing
                  ( Elab.TArrow
                      boolTy
                      (Elab.TArrow targetRetainedTy targetRetainedTy)
                  )
              )
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy
              , resolvedVarDetails =
                  DeferredId
                    (generatedDeferredRefForName "later-forall-producer")
              }
          producerTerm = Elab.EVarNode producer
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
          expectedInst =
            Elab.InstSeq
              (ElabTypes.instUnderWithRef retainedRef Elab.InstElim)
              Elab.InstIntro
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` Elab.ETyInst producerTerm expectedInst
      case Elab.typeCheckWithEnv env constructed of
        Right actualTy
          | TypeOps.alphaEqType actualTy targetTy -> pure ()
        other -> expectationFailure ("Expected vacuous exact target, got: " ++ show other)

    it "eliminates a vacuous source forall before preserving the exact bounded forall" $ do
      let vacuousRef = graphTypeBinderRef 992070 "vacuous"
          sourceResultRef = graphTypeBinderRef 992071 "source-result"
          targetResultRef = graphTypeBinderRef 992072 "target-result"
          identityRef = graphTypeBinderRef 992073 "identity-a"
          identityTy =
            ElabTypes.tForallWithRef
              identityRef
              Nothing
              (Elab.TArrow (ElabTypes.tVarWithRef identityRef) (ElabTypes.tVarWithRef identityRef))
          resultBound = Just (boundFromType identityTy)
          sourceBody =
            ElabTypes.tForallWithRef
              sourceResultRef
              resultBound
              (Elab.TArrow identityTy (ElabTypes.tVarWithRef sourceResultRef))
          sourceTy =
            ElabTypes.tForallWithRef
              vacuousRef
              Nothing
              sourceBody
          targetTy =
            ElabTypes.tForallWithRef
              targetResultRef
              resultBound
              (Elab.TArrow identityTy (ElabTypes.tVarWithRef targetResultRef))
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy
              , resolvedVarDetails = DeferredId (generatedDeferredRefForName "vacuous-prefix-producer")
              }
          producerTerm = Elab.EVarNode producer
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` Elab.ETyInst producerTerm Elab.InstElim
      case Elab.typeCheckWithEnv env constructed of
        Right actualTy
          | TypeOps.alphaEqType actualTy targetTy -> pure ()
        other -> expectationFailure ("Expected bounded exact target, got: " ++ show other)

    it "introduces a free exact binder before eliminating an unrelated source forall" $ do
      let sourceRef = graphTypeBinderRef 992080 "source"
          targetRef =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992080))
              "target"
          targetVar = ElabTypes.tVarWithRef targetRef
          functionTy = Elab.TArrow targetVar targetVar
          sourceTy = ElabTypes.tForallWithRef sourceRef Nothing functionTy
          targetTy = ElabTypes.tForallWithRef targetRef Nothing functionTy
          parameter =
            ResolvedVar
              { resolvedVarType = targetVar,
                resolvedVarDetails = LocalId (localRefFromNodeId "x" (NodeId 992079))
              }
          producerTerm =
            Elab.ETyAbsRef
              sourceRef
              Nothing
              (Elab.ELam parameter (Elab.EVarNode parameter))
          env = Elab.mkTypeCheckEnvWithResolvedTerms [] Map.empty
          expected =
            Elab.ETyAbsRef
              targetRef
              Nothing
              (Elab.ETyInst producerTerm Elab.InstElim)
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` expected
      Elab.typeCheckWithEnv env constructed `shouldBe` Right targetTy

    it "reorders semantic exact binders before eliminating a bounded result" $ do
      let refA =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992084))
              "a"
          refB =
            ElabTypes.typeBinderRefFromIdentity
              (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992085))
              "b"
          resultRef = graphTypeBinderRef 992086 "result"
          boolTy = TestElab.tBase (BaseTy "Bool")
          resultTy =
            Elab.TArrow
              (ElabTypes.tVarWithRef refA)
              (ElabTypes.tVarWithRef refB)
          sourceTy =
            ElabTypes.tForallWithRef
              refB
              Nothing
              ( ElabTypes.tForallWithRef
                  refA
                  Nothing
                  ( ElabTypes.tForallWithRef
                      resultRef
                      (Just (boundFromType resultTy))
                      ( Elab.TArrow
                          boolTy
                          (ElabTypes.tVarWithRef resultRef)
                      )
                  )
              )
          targetTy =
            ElabTypes.tForallWithRef
              refA
              Nothing
              ( ElabTypes.tForallWithRef
                  refB
                  Nothing
                  (Elab.TArrow boolTy resultTy)
              )
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy
              , resolvedVarDetails =
                  DeferredId
                    (generatedDeferredRefForName "reordered-exact-producer")
              }
          producerTerm = Elab.EVarNode producer
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldNotBe` producerTerm
      case Elab.typeCheckWithEnv env constructed of
        Right actualTy
          | TypeOps.alphaEqType actualTy targetTy -> pure ()
        other ->
          expectationFailure
            ("Expected reordered exact target, got: " ++ show other)

    it "eliminates an inferred prefix and a vacuous bounded suffix canonically" $ do
      let argumentRef = graphTypeBinderRef 992081 "argument"
          vacuousRef = graphTypeBinderRef 992082 "vacuous"
          boundParamRef = graphTypeBinderRef 992083 "bound-param"
          natTy = TestElab.tBase (BaseTy "Nat")
          box ty = TestElab.tCon (BaseTy "Box") (ty NE.:| [])
          polymorphicFunctionBound =
            ElabTypes.tForallWithRef
              boundParamRef
              Nothing
              (Elab.TArrow (ElabTypes.tVarWithRef boundParamRef) (box (ElabTypes.tVarWithRef boundParamRef)))
          sourceTy =
            ElabTypes.tForallWithRef
              argumentRef
              (Just (boundFromType natTy))
              ( ElabTypes.tForallWithRef
                  vacuousRef
                  (Just (boundFromType polymorphicFunctionBound))
                  (box (ElabTypes.tVarWithRef argumentRef))
              )
          targetTy = box natTy
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy,
                resolvedVarDetails = DeferredId (generatedDeferredRefForName "bounded-prefix-producer")
              }
          producerTerm = Elab.EVarNode producer
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
          expectedInst = Elab.InstSeq Elab.InstElim Elab.InstElim
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` Elab.ETyInst producerTerm expectedInst
      Elab.typeCheckWithEnv env constructed `shouldBe` Right targetTy

    it "infers multiple prefix applications before eliminating a vacuous bounded suffix" $ do
      let domainRef = graphTypeBinderRef 992091 "domain"
          codomainRef = graphTypeBinderRef 992092 "codomain"
          vacuousRef = graphTypeBinderRef 992093 "vacuous"
          ambientRef = graphTypeBinderRef 992094 "ambient"
          polymorphicRef = graphTypeBinderRef 992095 "polymorphic"
          boolTy = TestElab.tBase (BaseTy "Bool")
          intTy = TestElab.tBase (BaseTy "Int")
          ambientTy = ElabTypes.tVarWithRef ambientRef
          polymorphicTy =
            ElabTypes.tForallWithRef
              polymorphicRef
              Nothing
              ( Elab.TArrow
                  (ElabTypes.tVarWithRef polymorphicRef)
                  (ElabTypes.tVarWithRef polymorphicRef)
              )
          codomainTy = Elab.TArrow polymorphicTy intTy
          sourceTy =
            ElabTypes.tForallWithRef
              domainRef
              Nothing
              ( ElabTypes.tForallWithRef
                  codomainRef
                  Nothing
                  ( ElabTypes.tForallWithRef
                      vacuousRef
                      (Just (boundFromType boolTy))
                      ( Elab.TArrow
                          (ElabTypes.tVarWithRef domainRef)
                          (ElabTypes.tVarWithRef codomainRef)
                      )
                  )
              )
          targetTy = Elab.TArrow ambientTy codomainTy
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy
              , resolvedVarDetails =
                  DeferredId
                    (generatedDeferredRefForName "multi-prefix-producer")
              }
          producerTerm = Elab.EVarNode producer
          env =
            Elab.mkTypeCheckEnvWithResolvedTerms
              [(producer, sourceTy)]
              (Map.singleton ambientRef Elab.TBottom)
          expectedInst =
            Elab.InstSeq
              (Elab.InstApp ambientTy)
              ( Elab.InstSeq
                  (Elab.InstApp codomainTy)
                  Elab.InstElim
              )
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` Elab.ETyInst producerTerm expectedInst
      Elab.typeCheckWithEnv env constructed `shouldBe` Right targetTy

    it "eliminates a bounded prefix before applying an inferred remaining binder" $ do
      let resultCarrierRef = graphTypeBinderRef 992087 "result-carrier"
          resultBoundRef = graphTypeBinderRef 992088 "result-bound"
          resultRef = graphTypeBinderRef 992089 "result"
          intTy = TestElab.tBase (BaseTy "Int")
          polymorphicResultBound =
            ElabTypes.tForallWithRef
              resultBoundRef
              Nothing
              ( Elab.TArrow
                  (ElabTypes.tVarWithRef resultBoundRef)
                  (ElabTypes.tVarWithRef resultBoundRef)
              )
          sourceTy =
            ElabTypes.tForallWithRef
              resultCarrierRef
              (Just (boundFromType polymorphicResultBound))
              ( ElabTypes.tForallWithRef
                  resultRef
                  Nothing
                  ( Elab.TArrow
                      intTy
                      ( Elab.TArrow
                          (ElabTypes.tVarWithRef resultRef)
                          (ElabTypes.tVarWithRef resultRef)
                      )
                  )
              )
          targetTy = Elab.TArrow intTy (Elab.TArrow intTy intTy)
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy,
                resolvedVarDetails =
                  DeferredId
                    (generatedDeferredRefForName "bounded-prefix-then-app-producer")
              }
          producerTerm = Elab.EVarNode producer
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
          expectedInst =
            Elab.InstSeq
              Elab.InstElim
              (Elab.InstApp intTy)
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` Elab.ETyInst producerTerm expectedInst
      Elab.typeCheckWithEnv env constructed `shouldBe` Right targetTy

    it "constructs shared-forall annotation coercions inside the bound and under the body" $ do
      let outerSourceRef = graphTypeBinderRef 992101 "a"
          outerTargetRef = graphTypeBinderRef 992102 "result"
          boundRef = graphTypeBinderRef 992103 "bound"
          bodyRef = graphTypeBinderRef 992104 "body"
          intTy = TestElab.tBase (BaseTy "Int")
          boolTy = TestElab.tBase (BaseTy "Bool")
          sourceBoundTy =
            ElabTypes.tForallWithRef
              boundRef
              Nothing
              (Elab.TArrow (ElabTypes.tVarWithRef boundRef) (ElabTypes.tVarWithRef boundRef))
          sourceTy =
            ElabTypes.tForallWithRef
              outerSourceRef
              (Just (boundFromType sourceBoundTy))
              ( ElabTypes.tForallWithRef
                  bodyRef
                  Nothing
                  (Elab.TArrow (ElabTypes.tVarWithRef bodyRef) (ElabTypes.tVarWithRef outerSourceRef))
              )
          targetTy =
            ElabTypes.tForallWithRef
              outerTargetRef
              (Just (boundFromType (Elab.TArrow intTy intTy)))
              (Elab.TArrow boolTy (ElabTypes.tVarWithRef outerTargetRef))
          producer =
            ResolvedVar
              { resolvedVarType = sourceTy,
                resolvedVarDetails = DeferredId (generatedDeferredRefForName "shared-forall-producer")
              }
          producerTerm = Elab.EVarNode producer
          env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
          expectedInst =
            Elab.InstSeq
              (Elab.InstInside (Elab.InstApp intTy))
              (ElabTypes.instUnderWithRef outerSourceRef (Elab.InstApp boolTy))
      constructed <-
        requireRight
          (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
      constructed `shouldBe` Elab.ETyInst producerTerm expectedInst
      case Elab.typeCheckWithEnv env constructed of
        Right actualTy
          | TypeOps.alphaEqType actualTy targetTy -> pure ()
        other -> expectationFailure ("Expected shared-forall target type, got: " ++ show other)

    it "omits identity inside and under wrappers from shared-forall coercions" $ do
      let outerSourceRef = graphTypeBinderRef 992111 "a"
          outerTargetRef = graphTypeBinderRef 992112 "result"
          boundRef = graphTypeBinderRef 992113 "bound"
          bodyRef = graphTypeBinderRef 992114 "body"
          intTy = TestElab.tBase (BaseTy "Int")
          boolTy = TestElab.tBase (BaseTy "Bool")
          sourceBoundTy =
            ElabTypes.tForallWithRef
              boundRef
              Nothing
              (Elab.TArrow (ElabTypes.tVarWithRef boundRef) (ElabTypes.tVarWithRef boundRef))
          sourceBodyTy =
            ElabTypes.tForallWithRef
              outerSourceRef
              Nothing
              ( ElabTypes.tForallWithRef
                  bodyRef
                  Nothing
                  (Elab.TArrow (ElabTypes.tVarWithRef bodyRef) (ElabTypes.tVarWithRef outerSourceRef))
              )
          targetBodyTy =
            ElabTypes.tForallWithRef
              outerTargetRef
              Nothing
              (Elab.TArrow boolTy (ElabTypes.tVarWithRef outerTargetRef))
          sourceBoundOnlyTy =
            ElabTypes.tForallWithRef
              outerSourceRef
              (Just (boundFromType sourceBoundTy))
              (ElabTypes.tVarWithRef outerSourceRef)
          targetBoundOnlyTy =
            ElabTypes.tForallWithRef
              outerTargetRef
              (Just (boundFromType (Elab.TArrow intTy intTy)))
              (ElabTypes.tVarWithRef outerTargetRef)
          checkConstruction producerName sourceTy targetTy expectedInst = do
            let producer =
                  ResolvedVar
                    { resolvedVarType = sourceTy,
                      resolvedVarDetails = DeferredId (generatedDeferredRefForName producerName)
                    }
                producerTerm = Elab.EVarNode producer
                env = Elab.mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
            constructed <-
              requireRight
                (Annotation.constructExactTermAtType env sourceTy targetTy producerTerm)
            constructed `shouldBe` Elab.ETyInst producerTerm expectedInst
            case Elab.typeCheckWithEnv env constructed of
              Right actualTy
                | TypeOps.alphaEqType actualTy targetTy -> pure ()
              other -> expectationFailure ("Expected minimal shared-forall target type, got: " ++ show other)
      checkConstruction
        "shared-forall-body-only"
        sourceBodyTy
        targetBodyTy
        (ElabTypes.instUnderWithRef outerSourceRef (Elab.InstApp boolTy))
      checkConstruction
        "shared-forall-bound-only"
        sourceBoundOnlyTy
        targetBoundOnlyTy
        (Elab.InstInside (Elab.InstApp intTy))

    describe "resolved AnnExpr identity analysis" $ do
      let binderDetails = LocalId (localRefFromNodeId "runtime-x" (NodeId 992001))
          siblingDetails = LocalId (localRefFromNodeId "runtime-x" (NodeId 992002))
          mediatorDetails = LocalId (localRefFromNodeId "runtime-x" (NodeId 992012))
          innerBody = ALit (LInt 1) (NodeId 992003)
          annotatedMediator occurrenceDetails occurrenceDisplay =
            ALet
              "runtime-x"
              mediatorDetails
              (GenNodeId 992004)
              (NodeId 992005)
              (ExpVarId 992006)
              (GenNodeId 992007)
              ( AAnn
                  (AResolvedVar occurrenceDetails occurrenceDisplay (NodeId 992008))
                  (NodeId 992009)
                  (EdgeId 992010)
              )
              innerBody
              (NodeId 992011)

      it "matches an annotated lambda mediator by identity despite a stale occurrence spelling" $ do
        Annotation.desugaredAnnLambdaInfo
          binderDetails
          (annotatedMediator binderDetails "stale-x")
          `shouldBe` Just (mediatorDetails, NodeId 992009, EdgeId 992010, innerBody)

      it "rejects an annotated lambda mediator with the same spelling but a different identity" $ do
        Annotation.desugaredAnnLambdaInfo
          binderDetails
          (annotatedMediator siblingDetails "runtime-x")
          `shouldBe` Nothing

    it "parses and represents STVar" $ do
      let st = STVar "alpha"
      st `shouldBe` STVar "alpha"

    it "parses and represents STArrow" $ do
      let st = STArrow (STBase "Int") (STBase "Bool")
      st `shouldBe` STArrow (STBase "Int") (STBase "Bool")

    it "parses and represents unbounded STForall" $ do
      let st = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
      st `shouldBe` STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))

    it "parses and represents bounded STForall" $ do
      let bound = STArrow (STBase "Int") (STBase "Int")
          st = STForall "a" (Just (mkSrcBound bound)) (STVar "a")
      st `shouldBe` STForall "a" (Just (mkSrcBound bound)) (STVar "a")

    it "parses canonical bottom syntax as STBottom" $ do
      parseRawEmlfType "⊥" `shouldBe` Right STBottom
      parseRawEmlfType "_|_" `shouldSatisfy` isLeft
      parseRawEmlfType "bottom" `shouldSatisfy` isLeft

    it "represents nested STForall with multiple binders" $ do
      let binds = [("a", Nothing), ("b", Just (STBase "Int"))]
          body = STArrow (STVar "a") (STVar "b")
          st = mkForalls binds body
      st
        `shouldBe` STForall
          "a"
          Nothing
          (STForall "b" (Just (mkSrcBound (STBase "Int"))) body)

  describe "Expansion to Instantiation conversion" $ do
    it "uses InstId as an operationally inert identity instantiation" $ do
      let ty = TestElab.tBase (BaseTy "Int")
          term = Elab.ETyInst (Elab.ELit (LInt 1)) Elab.InstId
      Elab.applyInstantiation ty Elab.InstId `shouldBe` Right ty
      Elab.step term `shouldBe` Just (Elab.ELit (LInt 1))
      Elab.typeCheck term `shouldBe` Right ty

    it "converts ExpInstantiate to InstApp sequence" $ do
      -- InstSeq combines multiple applications
      let inst =
            Elab.InstSeq
              (Elab.InstApp (TestElab.tBase (BaseTy "Int")))
              (Elab.InstApp (TestElab.tBase (BaseTy "Bool")))
      Elab.pretty inst `shouldBe` "∀(⩾ ⊲Int); N; (∀(⩾ ⊲Bool); N)"

    it "sameLaneClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
          extractSameLaneClearBoundaryEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope (ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _) _ _) _ ->
              pure (schemeRootId, argEdgeId)
            other -> do
              expectationFailure ("Expected sameLaneClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneClearBoundaryEdge annCanon
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (term, ty) -> assertCheckedRecursiveResult term ty

    it "sameLaneDoubleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneDoubleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))
                  )
              )
          extractSameLaneDoubleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                      case uBody of
                        ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                          pure (schemeRootId, argEdgeId)
                        other -> do
                          expectationFailure ("Expected sameLaneDoubleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                          fail "sameLaneDoubleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneDoubleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneDoubleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneDoubleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneDoubleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneDoubleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneDoubleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneDoubleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneDoubleAliasEdge annCanon
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneDoubleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneTripleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneTripleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u"))
                      )
                  )
              )
          extractSameLaneTripleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                          case uBody of
                            ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                              pure (schemeRootId, argEdgeId)
                            other -> do
                              expectationFailure ("Expected sameLaneTripleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                              fail "sameLaneTripleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneTripleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneTripleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneTripleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneTripleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneTripleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneTripleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneTripleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneTripleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneTripleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneTripleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneTripleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneQuadrupleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneQuadrupleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          ( ELet
                              "deep"
                              (EVar "more")
                              (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "deep")) (EVar "u"))
                          )
                      )
                  )
              )
          extractSameLaneQuadrupleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope deepBody _ _) _ ->
                          case deepBody of
                            ALet "deep" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                              case uBody of
                                ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                                  pure (schemeRootId, argEdgeId)
                                other -> do
                                  expectationFailure ("Expected sameLaneQuadrupleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                                  fail "sameLaneQuadrupleAliasFrameClearBoundaryExprExactEdge"
                            other -> do
                              expectationFailure ("Expected sameLaneQuadrupleAliasFrameClearBoundaryExpr deep packet shape, got: " ++ show other)
                              fail "sameLaneQuadrupleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneQuadrupleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneQuadrupleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneQuadrupleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneQuadrupleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneQuadrupleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneQuadrupleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneQuadrupleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneQuadrupleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneQuadrupleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneQuadrupleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneQuadrupleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneQuintupleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneQuintupleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          ( ELet
                              "deep"
                              (EVar "more")
                              ( ELet
                                  "tail"
                                  (EVar "deep")
                                  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tail")) (EVar "u"))
                              )
                          )
                      )
                  )
              )
          extractSameLaneQuintupleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope deepBody _ _) _ ->
                          case deepBody of
                            ALet "deep" _ _ _ _ _ _ (ALetScope tailBody _ _) _ ->
                              case tailBody of
                                ALet "tail" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                                  case uBody of
                                    ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                                      pure (schemeRootId, argEdgeId)
                                    other -> do
                                      expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                                      fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
                                other -> do
                                  expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr tail packet shape, got: " ++ show other)
                                  fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
                            other -> do
                              expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr deep packet shape, got: " ++ show other)
                              fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneQuintupleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneQuintupleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneQuintupleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneQuintupleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneQuintupleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneSextupleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneSextupleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          ( ELet
                              "deep"
                              (EVar "more")
                              ( ELet
                                  "tail"
                                  (EVar "deep")
                                  ( ELet
                                      "leaf"
                                      (EVar "tail")
                                      (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "leaf")) (EVar "u"))
                                  )
                              )
                          )
                      )
                  )
              )
          extractSameLaneSextupleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope deepBody _ _) _ ->
                          case deepBody of
                            ALet "deep" _ _ _ _ _ _ (ALetScope tailBody _ _) _ ->
                              case tailBody of
                                ALet "tail" _ _ _ _ _ _ (ALetScope leafBody _ _) _ ->
                                  case leafBody of
                                    ALet "leaf" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                                      case uBody of
                                        ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                                          pure (schemeRootId, argEdgeId)
                                        other -> do
                                          expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                                          fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
                                    other -> do
                                      expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr leaf packet shape, got: " ++ show other)
                                      fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
                                other -> do
                                  expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr tail packet shape, got: " ++ show other)
                                  fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
                            other -> do
                              expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr deep packet shape, got: " ++ show other)
                              fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneSextupleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneSextupleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneSextupleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneSextupleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneSextupleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneSeptupleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneSeptupleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          ( ELet
                              "deep"
                              (EVar "more")
                              ( ELet
                                  "tail"
                                  (EVar "deep")
                                  ( ELet
                                      "leaf"
                                      (EVar "tail")
                                      ( ELet
                                          "tip"
                                          (EVar "leaf")
                                          (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tip")) (EVar "u"))
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          extractSameLaneSeptupleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope deepBody _ _) _ ->
                          case deepBody of
                            ALet "deep" _ _ _ _ _ _ (ALetScope tailBody _ _) _ ->
                              case tailBody of
                                ALet "tail" _ _ _ _ _ _ (ALetScope leafBody _ _) _ ->
                                  case leafBody of
                                    ALet "leaf" _ _ _ _ _ _ (ALetScope tipBody _ _) _ ->
                                      case tipBody of
                                        ALet "tip" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                                          case uBody of
                                            ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                                              pure (schemeRootId, argEdgeId)
                                            other -> do
                                              expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                                              fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                                        other -> do
                                          expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr tip packet shape, got: " ++ show other)
                                          fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                                    other -> do
                                      expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr leaf packet shape, got: " ++ show other)
                                      fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                                other -> do
                                  expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr tail packet shape, got: " ++ show other)
                                  fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                            other -> do
                              expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr deep packet shape, got: " ++ show other)
                              fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneSeptupleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneSeptupleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneSeptupleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneSeptupleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneSeptupleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneOctupleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneOctupleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          ( ELet
                              "deep"
                              (EVar "more")
                              ( ELet
                                  "tail"
                                  (EVar "deep")
                                  ( ELet
                                      "leaf"
                                      (EVar "tail")
                                      ( ELet
                                          "tip"
                                          (EVar "leaf")
                                          ( ELet
                                              "bud"
                                              (EVar "tip")
                                              (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u"))
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          extractSameLaneOctupleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope deepBody _ _) _ ->
                          case deepBody of
                            ALet "deep" _ _ _ _ _ _ (ALetScope tailBody _ _) _ ->
                              case tailBody of
                                ALet "tail" _ _ _ _ _ _ (ALetScope leafBody _ _) _ ->
                                  case leafBody of
                                    ALet "leaf" _ _ _ _ _ _ (ALetScope tipBody _ _) _ ->
                                      case tipBody of
                                        ALet "tip" _ _ _ _ _ _ (ALetScope budBody _ _) _ ->
                                          case budBody of
                                            ALet "bud" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                                              case uBody of
                                                ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                                                  pure (schemeRootId, argEdgeId)
                                                other -> do
                                                  expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                                                  fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                                            other -> do
                                              expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr bud packet shape, got: " ++ show other)
                                              fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                                        other -> do
                                          expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr tip packet shape, got: " ++ show other)
                                          fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                                    other -> do
                                      expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr leaf packet shape, got: " ++ show other)
                                      fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                                other -> do
                                  expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr tail packet shape, got: " ++ show other)
                                  fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                            other -> do
                              expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr deep packet shape, got: " ++ show other)
                              fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneOctupleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneOctupleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneOctupleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneOctupleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneOctupleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "sameLaneNonupleAliasFrameClearBoundaryExpr builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          sameLaneNonupleAliasFrameClearBoundaryExpr =
            ELet
              "k"
              (ELamAnn "x" recursiveAnn (EVar "x"))
              ( ELet
                  "hold"
                  (EVar "k")
                  ( ELet
                      "keep"
                      (EVar "hold")
                      ( ELet
                          "more"
                          (EVar "keep")
                          ( ELet
                              "deep"
                              (EVar "more")
                              ( ELet
                                  "tail"
                                  (EVar "deep")
                                  ( ELet
                                      "leaf"
                                      (EVar "tail")
                                      ( ELet
                                          "tip"
                                          (EVar "leaf")
                                          ( ELet
                                              "bud"
                                              (EVar "tip")
                                              ( ELet
                                                  "seed"
                                                  (EVar "bud")
                                                  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "seed")) (EVar "u"))
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          extractSameLaneNonupleAliasEdge ann0 = case ann0 of
            ALet "k" _ _ schemeRootId _ _ _ (ALetScope holdBody _ _) _ ->
              case holdBody of
                ALet "hold" _ _ _ _ _ _ (ALetScope keepBody _ _) _ ->
                  case keepBody of
                    ALet "keep" _ _ _ _ _ _ (ALetScope moreBody _ _) _ ->
                      case moreBody of
                        ALet "more" _ _ _ _ _ _ (ALetScope deepBody _ _) _ ->
                          case deepBody of
                            ALet "deep" _ _ _ _ _ _ (ALetScope tailBody _ _) _ ->
                              case tailBody of
                                ALet "tail" _ _ _ _ _ _ (ALetScope leafBody _ _) _ ->
                                  case leafBody of
                                    ALet "leaf" _ _ _ _ _ _ (ALetScope tipBody _ _) _ ->
                                      case tipBody of
                                        ALet "tip" _ _ _ _ _ _ (ALetScope budBody _ _) _ ->
                                          case budBody of
                                            ALet "bud" _ _ _ _ _ _ (ALetScope seedBody _ _) _ ->
                                              case seedBody of
                                                ALet "seed" _ _ _ _ _ _ (ALetScope uBody _ _) _ ->
                                                  case uBody of
                                                    ALet "u" _ _ _ _ _ (AApp _ _ _ argEdgeId _) _ _ ->
                                                      pure (schemeRootId, argEdgeId)
                                                    other -> do
                                                      expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr inner packet shape, got: " ++ show other)
                                                      fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                                                other -> do
                                                  expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr seed packet shape, got: " ++ show other)
                                                  fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                                            other -> do
                                              expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr bud packet shape, got: " ++ show other)
                                              fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                                        other -> do
                                          expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr tip packet shape, got: " ++ show other)
                                          fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                                    other -> do
                                      expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr leaf packet shape, got: " ++ show other)
                                      fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                                other -> do
                                  expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr tail packet shape, got: " ++ show other)
                                  fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                            other -> do
                              expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr deep packet shape, got: " ++ show other)
                              fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                        other -> do
                          expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr more packet shape, got: " ++ show other)
                          fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                    other -> do
                      expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr keep packet shape, got: " ++ show other)
                      fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
                other -> do
                  expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr hold packet shape, got: " ++ show other)
                  fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
            other -> do
              expectationFailure ("Expected sameLaneNonupleAliasFrameClearBoundaryExpr packet shape, got: " ++ show other)
              fail "sameLaneNonupleAliasFrameClearBoundaryExprExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneNonupleAliasFrameClearBoundaryExpr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (schemeRootId, argEdgeId) <- extractSameLaneNonupleAliasEdge annCanon
      scopeRoot <-
        requireRight
          ( resolveCanonicalScope
              (paConstraintNorm artifacts)
              (rtcPresolutionView inputs)
              (rtcRedirects inputs)
              schemeRootId
          )
      let targetNode = schemeBodyTarget (rtcPresolutionView inputs) schemeRootId
      (scheme, subst) <-
        requireRight
          ( generalizeWithPlan
              (rtcPlanBuilder inputs)
              (rtcBindParentsGa inputs)
              (rtcPresolutionView inputs)
              scopeRoot
              targetNode
          )
      let schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
          witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just schemeInfo)
              trace
              witness
          )
      rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId `shouldBe` ExpIdentity
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneNonupleAliasFrameClearBoundaryExpr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (term, ty) -> assertCheckedRecursiveResult term ty

    it "selected same-wrapper nested-forall builds a validated occurrence computation" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          expr =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "k"
                  (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                  (EApp (ELam "y" (EVar "y")) (EVar "k"))
              )
          extractPacket ann0 = case ann0 of
            ALet "id" _ _ idSchemeRoot _ _ _ (ALetScope (ALet "k" _ _ _ _ _ rhs _ _) _ _) _ ->
              case rhs of
                AApp funAnn argAnn argEdgeId _ _ -> pure (idSchemeRoot, funAnn, argAnn, argEdgeId)
                other -> do
                  expectationFailure ("Expected selected same-wrapper nested-forall rhs app, got: " ++ show other)
                  fail "selectedSameWrapperNestedForallExactEdge"
            other -> do
              expectationFailure ("Expected selected same-wrapper nested-forall packet shape, got: " ++ show other)
              fail "selectedSameWrapperNestedForallExactEdge"
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
      (_idSchemeRoot, _funAnn, argAnn, argEdgeId) <- extractPacket annCanon
      let view = rtcPresolutionView inputs
      case rtcEdgeExpansions inputs IntMap.! appSiteKey argEdgeId of
        ExpInstantiate [argumentNode] -> do
          -- Finalization may expose the expansion argument either as its
          -- variable or directly as the variable's structural lower bound.
          -- Compare the semantic lower roots rather than prescribing that
          -- representation shape.
          let semanticLowerRoot nodeId = do
                node <- pvLookupNode view nodeId
                pure $
                  pvCanonical view $
                    case node of
                      TyVar {tnBound = Just bound} -> bound
                      _ -> tnId node
          semanticLowerRoot argumentNode
            `shouldBe` semanticLowerRoot (annExprNode argAnn)
        other -> expectationFailure ("unexpected expansion: " ++ show other)
      sourceTy <- requireRight (ReifyType.reifyType view (annExprNode argAnn))
      let sourceScheme = ElabTypes.schemeFromType sourceTy
          sourceSchemeInfo = Elab.schemeInfoFromRefSubst sourceScheme IntMap.empty
      let witness = rtcEdgeWitnesses inputs IntMap.! appSiteKey argEdgeId
          trace = IntMap.lookup (appSiteKey argEdgeId) (rtcEdgeTraces inputs)
      ElabTypes.schemeBinderRefs sourceScheme `shouldBe` []
      case trace of
        Just strictTrace -> do
          etReplayContract strictTrace `shouldBe` ReplayContractStrict
          etBinderArgs strictTrace `shouldSatisfy` (not . null)
          etReplayDomainBinders strictTrace `shouldSatisfy` (not . null)
        Nothing -> expectationFailure "expected strict replay trace"
      occurrence <-
        requireRight
          ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive (paSolved artifacts))
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              (Just sourceSchemeInfo)
              trace
              witness
          )
      assertOccurrenceEndpoints occurrence
      case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (_, ty) -> Elab.pretty ty `shouldSatisfy` (not . null)

    it "selected same-wrapper nested-forall reaches the post-annotation authoritative handoff" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          expr =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "k"
                  (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                  (EApp (ELam "y" (EVar "y")) (EVar "k"))
              )
          containsMuTy ty0 = case ty0 of
            Elab.TMuRef _ _ -> True
            Elab.TArrow dom cod -> containsMuTy dom || containsMuTy cod
            Elab.TConWithIdentity _ _ args -> any containsMuTy args
            Elab.TVarAppRef _ args -> any containsMuTy args
            Elab.TForallRef _ mb body -> maybe False containsMuBound mb || containsMuTy body
            _ -> False
          containsMuBound bound = case bound of
            Elab.TArrow dom cod -> containsMuTy dom || containsMuTy cod
            Elab.TBaseWithIdentity _ _ -> False
            Elab.TConWithIdentity _ _ args -> any containsMuTy args
            Elab.TVarAppRef _ args -> any containsMuTy args
            Elab.TForallRef _ mb body -> maybe False containsMuBound mb || containsMuTy body
            Elab.TMuRef _ _ -> True
            Elab.TBottom -> False
          assertPipeline label runPipeline =
            case runPipeline Set.empty (unsafeNormalizeExpr expr) of
              Left err ->
                expectationFailure (label ++ ": " ++ Elab.renderPipelineError err)
              Right (term, ty) -> do
                Elab.typeCheck term `shouldBe` Right ty
                containsMuTy ty `shouldBe` True
      assertPipeline "canonical" Elab.runPipelineElab

  describe "Paper ≺ ordering (leftmost-lowermost)" $ do
    it "generalizeAt orders binders by ≺ (not by NodeId)" $ do
      -- Construct a tiny solved graph where the leftmost variable in the type
      -- has a *larger* NodeId than the right one, so NodeId-order would be wrong.
      let rootGen = GenNodeId 0
          vLeft = NodeId 10
          vRight = NodeId 5
          arrow = NodeId 20
          forallNode = NodeId 30

          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId vLeft, TyVar {tnId = vLeft, tnBound = Nothing}),
                        (getNodeId vRight, TyVar {tnId = vRight, tnBound = Nothing}),
                        (getNodeId arrow, TyArrow arrow vLeft vRight),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef vLeft), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef vRight), (genRef rootGen, BindFlex))
                      ]
                }

          solved = mkSolved c IntMap.empty

      (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) forallNode)
      Elab.pretty sch `shouldBe` "∀(a ⩾ ⊥) ∀(b ⩾ ⊥) a -> b"

    it "generalizeAt orders binders by <P when paths diverge (leftmost beats depth)" $ do
      -- The leftmost binder should quantify first even if it is shallower.
      let rootGen = GenNodeId 0
          vShallow = NodeId 5
          vDeep = NodeId 10
          nOuter = NodeId 20
          nInner = NodeId 21
          nInt = NodeId 22
          forallNode = NodeId 30

          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId vShallow, TyVar {tnId = vShallow, tnBound = Nothing}),
                        (getNodeId vDeep, TyVar {tnId = vDeep, tnBound = Nothing}),
                        (getNodeId nInt, TestTyBase nInt (BaseTy "Int")),
                        (getNodeId nInner, TyArrow nInner nInt vDeep),
                        (getNodeId nOuter, TyArrow nOuter vShallow nInner),
                        (getNodeId forallNode, TyForall forallNode nOuter)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef vShallow), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef vDeep), (genRef rootGen, BindFlex))
                      ]
                }

          solved = mkSolved c IntMap.empty

      (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) forallNode)
      Elab.pretty sch `shouldBe` "∀(a ⩾ ⊥) ∀(b ⩾ ⊥) a -> Int -> b"

    it "generalizeAt respects binder bound dependencies (a ≺ b if b’s bound mentions a)" $ do
      let rootGen = GenNodeId 0
          vA = NodeId 10
          vB = NodeId 5
          bnd = NodeId 15
          arrow = NodeId 20
          forallNode = NodeId 30

          c =
            rootedConstraint
              emptyConstraint
                { cEliminatedVars = IntSet.empty,
                  cNodes =
                    nodeMapFromList
                      [ (getNodeId vA, TyVar {tnId = vA, tnBound = Nothing}),
                        (getNodeId vB, TyVar {tnId = vB, tnBound = Just bnd}),
                        (getNodeId bnd, TyArrow bnd vA vA),
                        (getNodeId arrow, TyArrow arrow vB vA),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef vA), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef vB), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef bnd), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef arrow), (genRef rootGen, BindFlex))
                      ]
                }

          solved = mkSolved c IntMap.empty

      (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) forallNode)
      Elab.pretty sch `shouldBe` "∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b -> a"

  describe "Witness translation (Φ/Σ)" $ do
    describe "Σ(g) quantifier reordering" $ do
      it "O15-REORDER-IDENTITY: commutes two adjacent quantifiers" $ do
        let src =
              testTForall "a"
                Nothing
                (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))
            tgt =
              testTForall "b"
                Nothing
                (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))

        case Elab.sigmaReorder src tgt of
          Left err -> expectationFailure (show err)
          Right sig ->
            case Elab.applyInstantiation src sig of
              Left err -> expectationFailure (show err)
              Right out -> canonType out `shouldBe` canonType tgt

      it "O15-REORDER-IDENTITY: returns ε when source and target already match" $ do
        let src =
              testTForall "a"
                Nothing
                (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))
        sig <- requireRight (Elab.sigmaReorder src src)
        sig `shouldBe` Elab.InstId

      it "reorders same-named binders by identity" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3079))
                "a"
            refB =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3080))
                "a"
            body = Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB)
            src = ElabTypes.tForallWithRef refA Nothing (ElabTypes.tForallWithRef refB Nothing body)
            tgt = ElabTypes.tForallWithRef refB Nothing (ElabTypes.tForallWithRef refA Nothing body)
        sig <- requireRight (Elab.sigmaReorder src tgt)
        sig `shouldNotBe` Elab.InstId
        out <- requireRight (Elab.applyInstantiation src sig)
        TypeOps.alphaEqType out tgt `shouldBe` True

      it "reorders same-named spine binders by identity" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3081))
                "a"
            refB =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3082))
                "a"
            body = Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB)
            src = ElabTypes.tForallWithRef refA Nothing (ElabTypes.tForallWithRef refB Nothing body)
            tgt = ElabTypes.tForallWithRef refB Nothing (ElabTypes.tForallWithRef refA Nothing body)
        (sig, binders, ids) <-
          requireRight
            ( PhiTestSupport.reorderSpineRefsTo
                "spineReorder"
                [(refA, Nothing), (refB, Nothing)]
                [1 :: Int, 2]
                [2, 1]
            )
        sig `shouldNotBe` Elab.InstId
        map (ElabTypes.typeBinderRefIdentity . fst) binders
          `shouldBe` [ElabTypes.typeBinderRefIdentity refB, ElabTypes.typeBinderRefIdentity refA]
        ids `shouldBe` [2, 1]
        out <- requireRight (Elab.applyInstantiation src sig)
        TypeOps.alphaEqType out tgt `shouldBe` True

      it "commutes two adjacent bounded quantifiers (bounds preserved)" $ do
        let intTy = TestElab.tBase (BaseTy "Int")
            boolTy = TestElab.tBase (BaseTy "Bool")
            src =
              testTForall "a"
                (Just (boundFromType intTy))
                ( testTForall "b"
                    (Just (boundFromType boolTy))
                    (Elab.TArrow (testTVar "a") (testTVar "b"))
                )
            tgt =
              testTForall "b"
                (Just (boundFromType boolTy))
                ( testTForall "a"
                    (Just (boundFromType intTy))
                    (Elab.TArrow (testTVar "a") (testTVar "b"))
                )
        sig <- requireRight (Elab.sigmaReorder src tgt)
        out <- requireRight (Elab.applyInstantiation src sig)
        canonType out `shouldBe` canonType tgt

      it "permutes three quantifiers" $ do
        let src =
              testTForall "a"
                Nothing
                ( testTForall "b"
                    Nothing
                    ( testTForall "c"
                        Nothing
                        ( Elab.TArrow
                            (testTVar "a")
                            (Elab.TArrow (testTVar "b") (testTVar "c"))
                        )
                    )
                )
            tgt =
              testTForall "c"
                Nothing
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        ( Elab.TArrow
                            (testTVar "a")
                            (Elab.TArrow (testTVar "b") (testTVar "c"))
                        )
                    )
                )

        case Elab.sigmaReorder src tgt of
          Left err -> expectationFailure (show err)
          Right sig ->
            case Elab.applyInstantiation src sig of
              Left err -> expectationFailure (show err)
              Right out -> canonType out `shouldBe` canonType tgt

      it "reports missing target binder identities through InstantiationError" $ do
        let src = testTForall "a" Nothing (testTVar "a")
            tgt = testTForall "b" Nothing (testTVar "b")
        case Elab.sigmaReorder src tgt of
          Left (Elab.InstantiationError msg) ->
            msg `shouldBe` "sigmaReorder: desired binder not found in source"
          Left err ->
            expectationFailure ("Expected InstantiationError, got " ++ show err)
          Right sig -> expectationFailure ("Expected failure, got: " ++ show sig)

      it "reports missing target binder identities through the spine reorder helper" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3231))
                "a"
            refB =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3232))
                "b"
            binders = [(refA, Nothing), (refB, Nothing)]
            ids = [1 :: Int, 2]
        case PhiTestSupport.reorderSpineRefsTo "spineReorder" binders ids [3, 1] of
          Left (Elab.InstantiationError msg) ->
            msg `shouldBe` "spineReorder: desired binder not found in source"
          Left err ->
            expectationFailure ("Expected InstantiationError, got " ++ show err)
          Right (_sig, binders', ids') ->
            expectationFailure
              ( "Expected failure, got binders="
                  ++ show binders'
                  ++ " ids="
                  ++ show ids'
              )

      it "fails closed when the desired binder order is longer than the source spine" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 3247))
                "a"
            binders = [(refA, Nothing)]
            ids = [1 :: Int]
        case PhiTestSupport.reorderSpineRefsTo "spineReorder" binders ids [1, 2] of
          Left (Elab.InstantiationError msg) ->
            msg `shouldBe` "spineReorder: type has only 1 binders"
          Left err ->
            expectationFailure ("Expected InstantiationError, got " ++ show err)
          Right (_sig, binders', ids') ->
            expectationFailure
              ( "Expected failure, got binders="
                  ++ show binders'
                  ++ " ids="
                  ++ show ids'
              )

      it "O15-REORDER-REQUIRED: applies Σ reordering even without Raise when Typ/Typexp differ" $ do
        -- Thesis Def. 15.3.4: ϕR (aka Σ(g)) is required whenever the scheme
        -- type Typ(a′) and the expansion type Typexp(a′) disagree in binder
        -- order. This can happen even when Ω contains no Raise steps, so Φ
        -- must still prefix the translated witness with Σ(g).
        let rootGen = GenNodeId 0
            vA = NodeId 10
            vB = NodeId 11
            arrow = NodeId 20
            forallNode = NodeId 30

            c =
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId vA, TyVar {tnId = vA, tnBound = Nothing}),
                        (getNodeId vB, TyVar {tnId = vB, tnBound = Nothing}),
                        (getNodeId arrow, TyArrow arrow vA vB),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef forallNode), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef arrow), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vA), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vB), (typeRef forallNode, BindFlex))
                      ],
                  cGenNodes =
                    fromListGen
                      [(rootGen, GenNode rootGen [forallNode])]
                }

            solved = mkSolved c IntMap.empty

            -- Typ has binders in the opposite order of <P for the expansion root.
            scheme =
              Elab.schemeFromType
                ( testTForall "b"
                    Nothing
                    ( testTForall "a"
                        Nothing
                        (Elab.TArrow (testTVar "a") (testTVar "b"))
                    )
                )
            subst =
              IntMap.fromList
                [ (getNodeId vA, "a"),
                  (getNodeId vB, "b")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst

            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = arrow,
                  ewRight = arrow,
                  -- Expansion root r (TyExp body); order keys derived from this.
                  ewRoot = arrow,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness []
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        -- Φ should produce a non-identity instantiation (reordering)
        phi `shouldNotBe` Elab.InstId
        -- Apply and verify the result has binders in <P order (a before b)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall "a"
                Nothing
                ( testTForall "b"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "b"))
                )
        canonType out `shouldBe` canonType expected

      it "rejects ambiguous repeated graft-weaken on the same non-front binder" $ do
        let root = NodeId 0
            binderA = NodeId 1
            forallB = NodeId 2
            binderB = NodeId 3
            bodyNode = NodeId 4
            intNode = NodeId 5
            boolNode = NodeId 6
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB),
                  (getNodeId intNode, TestTyBase intNode (BaseTy "Int")),
                  (getNodeId boolNode, TestTyBase boolNode (BaseTy "Bool"))
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty
            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst =
              IntMap.fromList
                [ (getNodeId binderA, "a"),
                  (getNodeId binderB, "b")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst
            ops =
              [ OpGraft intNode binderB,
                OpWeaken binderB,
                OpGraft boolNode binderB,
                OpWeaken binderB
              ]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        case phiFromEdgeWitnessFixtureTrace solved (Just si) ew of
          Left _ -> pure ()
          Right phi ->
            expectationFailure
              ("Expected ambiguity rejection, got Phi: " ++ Elab.pretty phi)

      it "keeps non-front binder targeting stable after root graft" $ do
        let root = NodeId 0
            binderA = NodeId 1
            forallB = NodeId 2
            binderB = NodeId 3
            bodyNode = NodeId 4
            intNode = NodeId 5
            boolNode = NodeId 6
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB),
                  (getNodeId intNode, TestTyBase intNode (BaseTy "Int")),
                  (getNodeId boolNode, TestTyBase boolNode (BaseTy "Bool"))
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty
            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst =
              IntMap.fromList
                [ (getNodeId binderA, "a"),
                  (getNodeId binderB, "b")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst
            ops = [OpGraft intNode root, OpGraft boolNode binderB, OpWeaken binderB]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected = Elab.TArrow (TestElab.tBase (BaseTy "Int")) (TestElab.tBase (BaseTy "Bool"))
        canonType out `shouldBe` canonType expected

      it "O15-TR-SEQ-EMPTY: empty Ω produces non-identity instantiation when binder order differs from <P" $ do
        -- Three binders: <P order is a < b < c (left-to-right in nested arrows)
        -- Scheme order is c, b, a. Φ should reorder to a, b, c.
        let rootGen = GenNodeId 0
            vA = NodeId 10
            vB = NodeId 11
            vC = NodeId 12
            inner = NodeId 21
            arrow = NodeId 20
            forallNode = NodeId 30

            c =
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId vA, TyVar {tnId = vA, tnBound = Nothing}),
                        (getNodeId vB, TyVar {tnId = vB, tnBound = Nothing}),
                        (getNodeId vC, TyVar {tnId = vC, tnBound = Nothing}),
                        (getNodeId inner, TyArrow inner vB vC),
                        (getNodeId arrow, TyArrow arrow vA inner),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef forallNode), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef arrow), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef inner), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vA), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vB), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vC), (typeRef forallNode, BindFlex))
                      ],
                  cGenNodes =
                    fromListGen
                      [(rootGen, GenNode rootGen [forallNode])]
                }

            solved = mkSolved c IntMap.empty

            -- Scheme has binders in reverse order: c, b, a
            scheme =
              Elab.schemeFromType
                ( testTForall "c"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        ( testTForall "a"
                            Nothing
                            ( Elab.TArrow
                                (testTVar "a")
                                (Elab.TArrow (testTVar "b") (testTVar "c"))
                            )
                        )
                    )
                )
            subst =
              IntMap.fromList
                [ (getNodeId vA, "a"),
                  (getNodeId vB, "b"),
                  (getNodeId vC, "c")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst

            -- Empty witness ops
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = arrow,
                  ewRight = arrow,
                  ewRoot = arrow,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness []
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        phi `shouldNotBe` Elab.InstId
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall "a"
                Nothing
                ( testTForall "b"
                    Nothing
                    ( testTForall "c"
                        Nothing
                        ( Elab.TArrow
                            (testTVar "a")
                            (Elab.TArrow (testTVar "b") (testTVar "c"))
                        )
                    )
                )
        canonType out `shouldBe` canonType expected

      it "O15-TR-SEQ-EMPTY-IDENTITY: Trχ(ε)=ε when Σ(g)=ε (isolated from reorder coupling)" $ do
        -- Keep binder order identical to <P so Σ(g) is identity. With empty Ω,
        -- Φ should remain identity as well (Trχ(ε)=ε).
        let rootGen = GenNodeId 0
            vA = NodeId 40
            vB = NodeId 41
            vC = NodeId 42
            inner = NodeId 51
            arrow = NodeId 50
            forallNode = NodeId 60

            c =
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId vA, TyVar {tnId = vA, tnBound = Nothing}),
                        (getNodeId vB, TyVar {tnId = vB, tnBound = Nothing}),
                        (getNodeId vC, TyVar {tnId = vC, tnBound = Nothing}),
                        (getNodeId inner, TyArrow inner vB vC),
                        (getNodeId arrow, TyArrow arrow vA inner),
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef forallNode), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef arrow), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef inner), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vA), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vB), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vC), (typeRef forallNode, BindFlex))
                      ],
                  cGenNodes =
                    fromListGen
                      [(rootGen, GenNode rootGen [forallNode])]
                }

            solved = mkSolved c IntMap.empty

            -- Already in <P order: a, b, c
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        ( testTForall "c"
                            Nothing
                            ( Elab.TArrow
                                (testTVar "a")
                                (Elab.TArrow (testTVar "b") (testTVar "c"))
                            )
                        )
                    )
                )
            subst =
              IntMap.fromList
                [ (getNodeId vA, "a"),
                  (getNodeId vB, "b"),
                  (getNodeId vC, "c")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = arrow,
                  ewRight = arrow,
                  ewRoot = arrow,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness []
                }

        -- Explicitly assert Σ(g) is identity to isolate this guard from reordering.
        sigma <-
          requireRight
            (Elab.sigmaReorder (Elab.schemeToType scheme) (Elab.schemeToType scheme))
        sigma `shouldBe` Elab.InstId

        phi <-
          requireRight
            (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        phi `shouldBe` Elab.InstId
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        canonType out `shouldBe` canonType (Elab.schemeToType scheme)

      it "derives Σ(g) order from the destination expansion root, not the source witness root" $ do
        let sourceRoot = NodeId 5600
            resultRoot = NodeId 5601
            binderA = NodeId 5602
            binderB = NodeId 5603
            sourceGen = GenNodeId 5600
            resultGen = GenNodeId 5601
            constraint =
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId sourceRoot, TyArrow sourceRoot binderA binderA)
                      , (getNodeId resultRoot, TyArrow resultRoot binderA binderB)
                      , (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing})
                      , (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing})
                      ]
                , cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef sourceRoot), (genRef sourceGen, BindFlex))
                      , (nodeRefKey (genRef resultGen), (genRef sourceGen, BindFlex))
                      , (nodeRefKey (typeRef resultRoot), (genRef resultGen, BindFlex))
                      , (nodeRefKey (typeRef binderA), (genRef sourceGen, BindFlex))
                      , (nodeRefKey (typeRef binderB), (genRef resultGen, BindFlex))
                      ]
                , cGenNodes =
                    fromListGen
                      [ (sourceGen, GenNode sourceGen [sourceRoot])
                      , (resultGen, GenNode resultGen [resultRoot])
                      ]
                }
            solved = mkSolved constraint IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall
                    "b"
                    Nothing
                    ( testTForall
                        "a"
                        Nothing
                        (Elab.TArrow (testTVar "a") (testTVar "b"))
                    )
                )
            si =
              mkSchemeInfoFromNodeNames
                scheme
                ( IntMap.fromList
                    [ (getNodeId binderA, "a")
                    , (getNodeId binderB, "b")
                    ]
                )
            trace =
              EdgeTrace
                { etRoot = sourceRoot
                , etResultRoot = resultRoot
                , etBinderArgs = []
                , etInterior = sourceInteriorFromList [sourceRoot, binderA]
                , etBinderReplayMap = mempty
                , etReplayDomainBinders = []
                , etCopyMap = mempty
                , etReplayContract = ReplayContractNone
                }
            witness =
              EdgeWitness
                { ewEdgeId = EdgeId 5600
                , ewLeft = sourceRoot
                , ewRight = resultRoot
                , ewRoot = sourceRoot
                , ewForallIntros = 0
                , ewWitness = InstanceWitness []
                }

        phi <-
          requireRight
            ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                defaultTraceConfig
                (generalizeAtWithActive solved)
                (presolutionViewFromSolved solved)
                Nothing
                (Just si)
                (Just trace)
                witness
            )
        phi `shouldNotBe` Elab.InstId
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall
                "a"
                Nothing
                ( testTForall
                    "b"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "b"))
                )
        canonType out `shouldBe` canonType expected

      it "missing <P order key for a binder fails Σ(g) reordering" $ do
        -- Create a constraint where a binder node is NOT reachable from the root
        -- (so it won't have an order key). Σ(g) must fail fast.
        let rootGen = GenNodeId 0
            otherGen = GenNodeId 1
            vA = NodeId 10
            vB = NodeId 11
            arrow = NodeId 20
            forallNode = NodeId 30

            c =
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId vA, TyVar {tnId = vA, tnBound = Nothing}),
                        (getNodeId vB, TyVar {tnId = vB, tnBound = Nothing}),
                        (getNodeId arrow, TyArrow arrow vA vA), -- a -> a (no b)
                        (getNodeId forallNode, TyForall forallNode arrow)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef forallNode), (genRef rootGen, BindFlex)),
                        (nodeRefKey (typeRef arrow), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vA), (typeRef forallNode, BindFlex)),
                        (nodeRefKey (typeRef vB), (genRef otherGen, BindFlex))
                      ],
                  cGenNodes =
                    fromListGen
                      [ (rootGen, GenNode rootGen [forallNode]),
                        (otherGen, GenNode otherGen [])
                      ]
                }

            solved = mkSolved c IntMap.empty

            -- Scheme references both a and b
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        (Elab.TArrow (testTVar "a") (testTVar "b"))
                    )
                )
            subst =
              IntMap.fromList
                [ (getNodeId vA, "a"),
                  (getNodeId vB, "b")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst

            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = arrow,
                  ewRight = arrow,
                  ewRoot = arrow,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness []
                }

        case phiFromEdgeWitnessFixtureTrace solved (Just si) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg `shouldSatisfy` ("PhiReorder: missing order key" `isInfixOf`)
          Left Elab.BindingTreeError {} ->
            pure ()
          Left other ->
            expectationFailure ("Expected PhiReorder missing-order-key or binding-tree failure, got " ++ show other)
          Right inst ->
            expectationFailure ("Expected PhiReorder failure, got " ++ Elab.pretty inst)

      it "does not recover a missing <P key from the current spine position" $ do
        let scheme =
              Elab.schemeFromType
                ( testTForall
                    "a"
                    Nothing
                    (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))
                )
        (refA, boundA, refB, boundB) <-
          case Elab.schemeBinderRefs scheme of
            [(refA0, boundA0), (refB0, boundB0)] ->
              pure (refA0, boundA0, refB0, boundB0)
            binders ->
              expectationFailure
                ("expected two forall binders, got " ++ show binders)
                >> fail "forall binder setup failed"
        let
            nodeA = NodeId 5710
            nodeB = NodeId 5711
            orderKeys =
              IntMap.singleton
                (getNodeId nodeA)
                Order.OrderKey
                  { Order.okDepth = 1
                  , Order.okPath = [0]
                  }
            binders =
              [ (refA, boundA, Just nodeA)
              , (refB, boundB, Just nodeB)
              ]

        case
            PhiTestSupport.orderPhiBindersByPrecForTest
              orderKeys
              (IntSet.fromList [getNodeId nodeA, getNodeId nodeB])
              binders
          of
            Left (Elab.PhiInvariantError message) ->
              message `shouldSatisfy` isInfixOf "PhiReorder: missing order key"
            other ->
              expectationFailure
                ("expected missing-order-key rejection, got " ++ show other)

      it "does not linearize a cyclic forall-bound dependency by position" $ do
        let scheme =
              Elab.schemeFromType
                ( testTForall
                    "a"
                    Nothing
                    (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))
                )
        (refA, refB) <-
          case Elab.schemeBinderRefs scheme of
            [(refA0, _), (refB0, _)] ->
              pure (refA0, refB0)
            binders ->
              expectationFailure
                ("expected two forall binders, got " ++ show binders)
                >> fail "forall binder setup failed"
        let
            nodeA = NodeId 5720
            nodeB = NodeId 5721
            boundA = Elab.TArrow (ElabTypes.tVarWithRef refB) (TestElab.tBase (BaseTy "Int"))
            boundB = Elab.TArrow (ElabTypes.tVarWithRef refA) (TestElab.tBase (BaseTy "Int"))
            orderKeys =
              IntMap.fromList
                [ ( getNodeId nodeA
                  , Order.OrderKey
                      { Order.okDepth = 1
                      , Order.okPath = [0]
                      }
                  )
                , ( getNodeId nodeB
                  , Order.OrderKey
                      { Order.okDepth = 1
                      , Order.okPath = [1]
                      }
                  )
                ]
            binders =
              [ (refA, Just boundA, Just nodeA)
              , (refB, Just boundB, Just nodeB)
              ]

        PhiTestSupport.orderPhiBindersByPrecForTest
          orderKeys
          (IntSet.fromList [getNodeId nodeA, getNodeId nodeB])
          binders
          `shouldBe`
            Left
              (Elab.InstantiationError "PhiReorder: cycle in bound dependencies")

    describe "Φ translation soundness" $ do
      let runToSolved :: SurfaceExpr -> Either String (Solved.Solved, IntMap.IntMap EdgeWitness, IntMap.IntMap EdgeTrace)
          runToSolved e = do
            PipelineArtifacts {paPresolution = pres, paSolved = solved} <-
              runPipelineArtifactsDefault Set.empty e
            pure (solved, prEdgeWitnesses pres, prEdgeTraces pres)

      it "elaboration fails when a witness has no trace entry" $ do
        let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
        artifacts@PipelineArtifacts {paSolved = solved} <-
          requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs, _annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
        case IntMap.lookupMin (rtcEdgeWitnesses inputs) of
          Nothing -> expectationFailure "Expected at least one edge witness"
          Just (eid, ew) -> do
            let edgeTraces' = IntMap.delete eid (rtcEdgeTraces inputs)
                mTrace = IntMap.lookup eid edgeTraces'
            -- Fail-fast invariant: missing trace entries must surface as
            -- MissingEdgeTrace before scheme reconstruction.
            case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive solved)
              (rtcPresolutionView inputs)
              (Just (rtcBindParentsGa inputs))
              Nothing
              mTrace
              ew of
              Left (Elab.MissingEdgeTrace (EdgeId eid')) -> eid' `shouldBe` eid
              Left err -> expectationFailure ("Expected MissingEdgeTrace, got " ++ show err)
              Right _ -> expectationFailure "Expected elaboration to fail due to missing trace"

      it "no-trace test entrypoint fails fast with MissingEdgeTrace" $ do
        let root = NodeId 0
            binder = NodeId 1
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root binder),
                          (getNodeId binder, TyVar {tnId = binder, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binder), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTVar "a"))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binder, "a")])
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 77,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness []
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) Nothing ew of
          Left (Elab.MissingEdgeTrace (EdgeId eid)) -> eid `shouldBe` 77
          Left err -> expectationFailure ("Expected MissingEdgeTrace, got " ++ show err)
          Right inst -> expectationFailure ("Expected fail-fast MissingEdgeTrace, got " ++ Elab.pretty inst)

      it "constructs Φ replay authority only from one edge-keyed witness/trace packet" $ do
        let edgeId = EdgeId 77
            sourceRoot = NodeId 10
            witnessRoot = NodeId 12
            resultRoot = NodeId 13
            traceInfo =
              EdgeTrace
                { etRoot = sourceRoot,
                  etResultRoot = resultRoot,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [sourceRoot],
                  etReplayContract = ReplayContractNone,
                  etBinderReplayMap = IntMap.empty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty
                }
            witness =
              EdgeWitness
                { ewEdgeId = edgeId,
                  ewLeft = sourceRoot,
                  ewRight = resultRoot,
                  ewRoot = witnessRoot,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness []
                }
            witnesses = IntMap.singleton (getEdgeId edgeId) witness
            traces = IntMap.singleton (getEdgeId edgeId) traceInfo
            edgeArtifacts =
              edgeArtifactsForTest
                (IntMap.singleton (getEdgeId edgeId) ExpIdentity)
                witnesses
                traces
                IntSet.empty
            certificateBuilder ::
              EdgeId ->
              EdgeArtifacts ->
              Either Elab.ElabError Elab.PhiReplayCertificate
            certificateBuilder = Elab.mkPhiReplayCertificate
        case certificateBuilder edgeId edgeArtifacts of
          Right _ -> pure ()
          Left err ->
            expectationFailure
              ("Expected matching replay certificate, got " ++ show err)
        case
            mkEdgeArtifacts
              (eaEdgeExpansions edgeArtifacts)
              witnesses
              IntMap.empty
              (eaEdgeExpansionConstructions edgeArtifacts)
              IntSet.empty
          of
          Left EdgeArtifactKeyMismatch{} -> pure ()
          other ->
            expectationFailure
              ("Expected missing-trace packet construction rejection, got " ++ show other)
        let wrongEdge = EdgeId 78
            wrongKey = getEdgeId wrongEdge
        case
            mkEdgeArtifacts
              (IntMap.singleton wrongKey ExpIdentity)
              (IntMap.singleton wrongKey witness)
              (IntMap.singleton wrongKey traceInfo)
              (IntMap.singleton wrongKey emptyRawExpansionConstruction)
              IntSet.empty
          of
          Left EdgeArtifactWitnessIdMismatch{} -> pure ()
          other ->
            expectationFailure
              ("Expected edge-identity packet rejection, got " ++ show other)

      it "O15-TR-RIGID-RAISE: OpRaise on a rigid node outside I(r) translates to identity" $ do
        let root = NodeId 100
            rigidN = NodeId 1
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root rigidN rigidN),
                          (getNodeId rigidN, TyVar {tnId = rigidN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef rigidN), (genRef (GenNodeId 0), BindRigid))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a")))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId rigidN, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpRaise rigidN]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew)
        phi `shouldBe` Elab.InstId

      it "retains OpRaise from a flexible source when its finalized replay binder is rigid" $ do
        (sourceTy, phi) <-
          requireRight (sourceDomainRaiseFixture BindFlex BindRigid)
        phi `shouldNotBe` Elab.InstId
        _ <- requireRight (Elab.applyInstantiation sourceTy phi)
        pure ()

      it "skips OpRaise from a rigid source when its finalized replay binder is flexible" $ do
        (sourceTy, phi) <-
          requireRight (sourceDomainRaiseFixture BindRigid BindFlex)
        phi `shouldBe` Elab.InstId
        Elab.applyInstantiation sourceTy phi `shouldBe` Right sourceTy

      it "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target" $ do
        let root = NodeId 100
            binderN = NodeId 1
            aliasN = NodeId 30
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderN binderN),
                          (getNodeId binderN, TyVar {tnId = binderN, tnBound = Nothing}),
                          (getNodeId aliasN, TyVar {tnId = aliasN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderN), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a")))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderN, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, binderN],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = insertCopy binderN aliasN mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpRaise binderN]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew)
        -- OpRaise with aliasing should produce a non-identity instantiation
        phi `shouldSatisfy` (/= Elab.InstId)

      it "OpWeaken on solved-away binder emits InstElim (binder preserved in scheme)" $ do
        let root = NodeId 100
            binderA = NodeId 1
            binderB = NodeId 2
            argA = NodeId 30
            argB = NodeId 31
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                          (getNodeId argA, TestTyBase argA (BaseTy "Int")),
                          (getNodeId argB, TestTyBase argB (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef binderB), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            -- Scheme now includes the solved-away binder b (original constraint preserves all binders)
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))))
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a"), (getNodeId binderB, "b")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(binderA, argA), (binderB, argB)],
                  etInterior = sourceInteriorFromList [root, binderA, binderB, argA, argB],
                  etBinderReplayMap =
                    IntMap.fromList
                      [ (getNodeId binderA, binderA),
                        (getNodeId binderB, binderB)
                      ],
                  etReplayDomainBinders = [binderA, binderB],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ops = [OpWeaken binderB]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left err ->
            expectationFailure ("Expected InstElim for solved-away binder, got error: " ++ show err)
          Right inst ->
            -- With the original constraint as primary, binderB is in the scheme
            -- and VSpine, so OpWeaken finds it and emits InstElim (N) under
            -- the prefix context of binder "a".
            Elab.pretty inst `shouldBe` "∀(a ⩾) N"

      it "fails fast when replay-map source domain mismatches trace binder sources" $ do
        let root = NodeId 100
            binderA = NodeId 1
            badTarget = NodeId 31
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId badTarget, TestTyBase badTarget (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "a"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(binderA, badTarget)],
                  etInterior = sourceInteriorFromList [root, binderA, badTarget],
                  -- Missing source key binderA in replay-map domain: strict fail-fast.
                  etBinderReplayMap = IntMap.empty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken binderA]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg `shouldSatisfy` ("trace binder replay-map domain mismatch" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast replay-map validation error, got " ++ Elab.pretty inst)

      it "fails fast when replay-map codomain target is outside replay binder domain" $ do
        let root = NodeId 100
            binderA = NodeId 1
            bogusTarget = NodeId 99
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId bogusTarget, TestTyBase bogusTarget (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "a"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(binderA, binderA)],
                  etInterior = sourceInteriorFromList [root, binderA, bogusTarget],
                  -- Domain is correct (binderA -> bogusTarget), but bogusTarget
                  -- is not in the replay binder domain.
                  etBinderReplayMap = IntMap.fromList [(getNodeId binderA, bogusTarget)],
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken binderA]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg `shouldSatisfy` ("replay-map target outside replay binder domain" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError for codomain, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast codomain error, got " ++ Elab.pretty inst)

      it "fails fast when replay-map codomain only matches replay domain via canonical alias" $ do
        let root = NodeId 100
            sourceKey = NodeId 1
            replayBinder = NodeId 2
            replayAlias = NodeId 31
            argNode = NodeId 40
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root sourceKey sourceKey),
                          (getNodeId sourceKey, TyVar {tnId = sourceKey, tnBound = Nothing}),
                          (getNodeId replayBinder, TyVar {tnId = replayBinder, tnBound = Nothing}),
                          (getNodeId replayAlias, TestTyBase replayAlias (BaseTy "Bool")),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef sourceKey), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef replayBinder), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            -- replayAlias canonicalises to replayBinder, but strict replay-map
            -- codomain validation must use replay key-space membership only.
            solved = mkSolved c (IntMap.singleton (getNodeId replayAlias) replayBinder)
            scheme =
              Elab.schemeFromType
                ( testTForall "t2"
                    Nothing
                    (Elab.TArrow (testTVar "t2") (testTVar "t2"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.singleton (getNodeId replayBinder) "t2")
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(sourceKey, argNode)],
                  etInterior = sourceInteriorFromList [root, sourceKey, replayBinder, replayAlias, argNode],
                  etBinderReplayMap = IntMap.singleton (getNodeId sourceKey) replayAlias,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken sourceKey]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg `shouldSatisfy` ("replay-map target outside replay binder domain" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast canonical-alias codomain rejection, got " ++ Elab.pretty inst)

      it "fails fast on malformed source-space replay target outside replay binder domain" $ do
        let root = NodeId 100
            sourceKey = NodeId 1
            sourceBinder = NodeId 31
            argNode = NodeId 40
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root sourceKey sourceKey),
                          (getNodeId sourceKey, TyVar {tnId = sourceKey, tnBound = Nothing}),
                          (getNodeId sourceBinder, TyVar {tnId = sourceBinder, tnBound = Nothing}),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef sourceKey), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "a"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId sourceKey, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(sourceKey, argNode)],
                  etInterior = sourceInteriorFromList [root, sourceKey, sourceBinder, argNode],
                  etBinderReplayMap = IntMap.fromList [(getNodeId sourceKey, sourceBinder)],
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken sourceKey]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg `shouldSatisfy` ("replay-map target outside replay binder domain" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected hard-fail, got " ++ Elab.pretty inst)

      it "fails fast on source-space identity replay target (no runtime repair)" $ do
        let root = NodeId 100
            sourceKey = NodeId 1
            replayBinder = NodeId 2
            argNode = NodeId 40
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root sourceKey sourceKey),
                          (getNodeId sourceKey, TyVar {tnId = sourceKey, tnBound = Nothing}),
                          (getNodeId replayBinder, TyVar {tnId = replayBinder, tnBound = Nothing}),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef sourceKey), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef replayBinder), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "t2"
                    Nothing
                    (Elab.TArrow (testTVar "t2") (testTVar "t2"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.singleton (getNodeId replayBinder) "t2")
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(sourceKey, argNode)],
                  etInterior = sourceInteriorFromList [root, sourceKey, replayBinder, argNode],
                  -- Invalid source-space identity target: the producer declares
                  -- replayBinder as the exact replay domain, so sourceKey is
                  -- not admissible there. Strict pass-through must fail rather
                  -- than remapping it at runtime.
                  etBinderReplayMap = IntMap.singleton (getNodeId sourceKey) sourceKey,
                  etReplayDomainBinders = [replayBinder],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken sourceKey]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg `shouldSatisfy` ("replay-map target outside replay binder domain" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError, got " ++ show err)
          Right inst ->
            expectationFailure
              ( "Expected hard-fail with no runtime replay-target repair, got "
                  ++ Elab.pretty inst
              )

      it "OpRaise fails fast when a trace-source target resolves to no existing replay node" $ do
        let root = NodeId 100
            binderA = NodeId 1
            sourceKey = NodeId 99
            replayGhost = NodeId 77
            argNode = NodeId 40
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "t77"
                    Nothing
                    (Elab.TArrow (testTVar "t77") (testTVar "t77"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.singleton (getNodeId replayGhost) "t77")
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(sourceKey, argNode)],
                  etInterior = sourceInteriorFromList [root, binderA, argNode],
                  etBinderReplayMap = IntMap.singleton (getNodeId sourceKey) replayGhost,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpRaise sourceKey]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg
              `shouldSatisfy` ("replay-map target outside replay binder domain" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected strict OpRaise fail-fast, got " ++ Elab.pretty inst)

      it "OpRaise fails fast when a non-trace target resolves to no existing replay node" $ do
        let root = NodeId 100
            binderA = NodeId 1
            missingTarget = NodeId 99
            argNode = NodeId 40
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "t1"
                    Nothing
                    (Elab.TArrow (testTVar "t1") (testTVar "t1"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.singleton (getNodeId binderA) "t1")
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(binderA, argNode)],
                  etInterior = sourceInteriorFromList [root, binderA, argNode],
                  etBinderReplayMap = IntMap.singleton (getNodeId binderA) binderA,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpRaise missingTarget]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg) ->
            msg
              `shouldSatisfy` ("OpRaise unresolved target has no direct replay/source node" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiInvariantError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected strict OpRaise fail-fast, got " ++ Elab.pretty inst)

      it "duplicate no-replay graft+weaken aligns source/spine in empty replay-domain lane" $ do
        let root = NodeId 100
            body = NodeId 101
            replayBinder = NodeId 1
            sourceKey = NodeId 99
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body replayBinder replayBinder),
                          (getNodeId replayBinder, TyVar {tnId = replayBinder, tnBound = Nothing}),
                          (getNodeId sourceKey, TyVar {tnId = sourceKey, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef replayBinder), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTVar "a"))
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId replayBinder, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, body, replayBinder, sourceKey],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = insertCopy sourceKey replayBinder mempty,
                  etReplayContract = ReplayContractNone
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft sourceKey sourceKey, OpGraft sourceKey sourceKey, OpWeaken sourceKey]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            unlines msgs `shouldSatisfy` ("OpGraft targets non-binder node" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast non-binder OpGraft, got " ++ Elab.pretty inst)

      it "stores SchemeInfo binder refs from ref-subst identity keys" $ do
        let sourceKey = NodeId 1
            replayTarget = NodeId 2
            sourceRef = graphTypeBinderRef (getNodeId sourceKey) "t1"
            replayRef = graphTypeBinderRef (getNodeId replayTarget) "a"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(sourceRef, Nothing), (replayRef, Nothing)]
                (Elab.TArrow (ElabTypes.tVarWithRef sourceRef) (ElabTypes.tVarWithRef replayRef))
            subst =
              IntMap.fromList
                [ (getNodeId sourceKey, "t1"),
                  (getNodeId replayTarget, "a")
                ]
            si =
              mkSchemeInfoFromNodeNames scheme subst
            binderIdentities =
              map
                (ElabTypes.typeBinderIdentityKey . ElabTypes.typeBinderRefIdentity . fst)
                (ElabTypes.schemeBinderRefs (Elab.siScheme si))
        binderIdentities `shouldBe` [getNodeId sourceKey, getNodeId replayTarget]
        case ElabTypes.schemeBody (Elab.siScheme si) of
          Elab.TArrow (Elab.TVarRef sourceBodyRef) (Elab.TVarRef replayBodyRef) ->
            map (ElabTypes.typeBinderIdentityKey . ElabTypes.typeBinderRefIdentity) [sourceBodyRef, replayBodyRef]
              `shouldBe` [getNodeId sourceKey, getNodeId replayTarget]
          other ->
            expectationFailure ("Expected SchemeInfo body refs, got: " ++ show other)
        schemeInfoNameSubst si `shouldBe` subst

      it "projects authoritative refs through the complete leading binder spine" $ do
        let selfGraphRef = graphTypeBinderRef 91 "list-self"
            resultGraphRef = graphTypeBinderRef 92 "list-result"
            selfRef =
              ElabTypes.typeBinderRefFromIdentity
                (typeBinderIdentityFromStructural (UniqueIdentity 7001) StructuralSelfBinder)
                "List"
            resultRef =
              ElabTypes.typeBinderRefFromIdentity
                (typeBinderIdentityFromStructural (UniqueIdentity 7001) StructuralResultBinder)
                "List$result"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                []
                ( ElabTypes.tForallWithRef
                    selfGraphRef
                    Nothing
                    ( ElabTypes.tForallWithRef
                        resultGraphRef
                        (Just (ElabTypes.TVarAppRef selfGraphRef (Elab.TBottom NE.:| [])))
                        ( Elab.TArrow
                            (ElabTypes.tVarWithRef selfGraphRef)
                            (ElabTypes.tVarWithRef resultGraphRef)
                        )
                    )
                )
            subst = IntMap.fromList [(91, selfRef), (92, resultRef)]
            schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
        ElabTypes.schemeBinderRefs (Elab.siScheme schemeInfo) `shouldBe` []
        Elab.siSubstRefs schemeInfo `shouldBe` subst
        case ElabTypes.schemeBody (Elab.siScheme schemeInfo) of
          Elab.TForallRef actualSelf Nothing
            ( Elab.TForallRef
                actualResult
                (Just (Elab.TVarAppRef boundSelf _))
                (Elab.TArrow (Elab.TVarRef bodySelf) (Elab.TVarRef bodyResult))
              ) -> do
                actualSelf `shouldBe` selfRef
                actualResult `shouldBe` resultRef
                boundSelf `shouldBe` selfRef
                bodySelf `shouldBe` selfRef
                bodyResult `shouldBe` resultRef
          other ->
            expectationFailure ("Expected projected leading binder spine, got: " ++ show other)

      it "applies authoritative binder swaps simultaneously" $ do
        let refA = graphTypeBinderRef 93 "a"
            refB = graphTypeBinderRef 94 "b"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(refA, Nothing), (refB, Just (ElabTypes.TVarAppRef refA (Elab.TBottom NE.:| [])))]
                (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB))
            subst = IntMap.fromList [(93, refB), (94, refA)]
            schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
        Elab.siSubstRefs schemeInfo `shouldBe` subst
        case ElabTypes.schemeBinderRefs (Elab.siScheme schemeInfo) of
          [(actualB, Nothing), (actualA, Just (Elab.TVarAppRef boundB _))] -> do
            actualB `shouldBe` refB
            actualA `shouldBe` refA
            boundB `shouldBe` refB
          other ->
            expectationFailure ("Expected swapped explicit binders, got: " ++ show other)
        case ElabTypes.schemeBody (Elab.siScheme schemeInfo) of
          Elab.TArrow (Elab.TVarRef bodyB) (Elab.TVarRef bodyA) -> do
            bodyB `shouldBe` refB
            bodyA `shouldBe` refA
          other ->
            expectationFailure ("Expected simultaneously swapped body refs, got: " ++ show other)

      it "preserves a non-leading forall shadow while projecting the leading spine" $ do
        let outerGraphRef = graphTypeBinderRef 95 "a"
            leadingGraphRef = graphTypeBinderRef 96 "b"
            outerRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 7002))
                "source-a"
            leadingRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 7003))
                "source-b"
            shadowBound =
              ElabTypes.TVarAppRef
                outerGraphRef
                (Elab.TBottom NE.:| [])
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(outerGraphRef, Nothing)]
                ( ElabTypes.tForallWithRef
                    leadingGraphRef
                    Nothing
                    ( Elab.TArrow
                        (ElabTypes.tVarWithRef outerGraphRef)
                        ( ElabTypes.tForallWithRef
                            outerGraphRef
                            (Just shadowBound)
                            (ElabTypes.tVarWithRef outerGraphRef)
                        )
                    )
                )
            subst = IntMap.fromList [(95, outerRef), (96, leadingRef)]
            schemeInfo = Elab.schemeInfoFromRefSubst scheme subst
        ElabTypes.schemeBinderRefs (Elab.siScheme schemeInfo)
          `shouldBe` [(outerRef, Nothing)]
        Elab.siSubstRefs schemeInfo `shouldBe` subst
        case ElabTypes.schemeBody (Elab.siScheme schemeInfo) of
          Elab.TForallRef actualLeading Nothing
            ( Elab.TArrow
                (Elab.TVarRef outerOccurrence)
                ( Elab.TForallRef
                    shadowRef
                    (Just (Elab.TVarAppRef boundOuter _))
                    (Elab.TVarRef shadowOccurrence)
                  )
              ) -> do
                actualLeading `shouldBe` leadingRef
                outerOccurrence `shouldBe` outerRef
                shadowRef `shouldBe` outerGraphRef
                boundOuter `shouldBe` outerRef
                shadowOccurrence `shouldBe` outerGraphRef
          other ->
            expectationFailure
              ("Expected a projected leading spine with a preserved nested shadow, got: " ++ show other)

      it "does not equate schemes by binder display names alone" $ do
        let refA = graphTypeBinderRef 1 "a"
            refB = graphTypeBinderRef 2 "a"
            siA =
              Elab.schemeInfoFromRefSubst
                (ElabTypes.mkElabSchemeWithRefs [(refA, Nothing)] (ElabTypes.tVarWithRef refA))
                (IntMap.singleton 1 refA)
            siB =
              Elab.schemeInfoFromRefSubst
                (ElabTypes.mkElabSchemeWithRefs [(refB, Nothing)] (ElabTypes.tVarWithRef refB))
                (IntMap.singleton 2 refB)
            binderDisplayNames =
              map (ElabTypes.typeBinderRefName . fst) . ElabTypes.schemeBinderRefs
        binderDisplayNames (Elab.siScheme siA) `shouldBe` binderDisplayNames (Elab.siScheme siB)
        (Elab.siScheme siA == Elab.siScheme siB) `shouldBe` False

      it "keeps same-named SchemeInfo binder refs distinct from ref subst" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 71))
                "a"
            refB =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 72))
                "a"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(refA, Nothing), (refB, Nothing)]
                (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB))
            si =
              Elab.schemeInfoFromRefSubst
                scheme
                (IntMap.fromList [(71, refA), (72, refB)])
        map (ElabTypes.typeBinderRefIdentity . fst) (ElabTypes.schemeBinderRefs (Elab.siScheme si))
          `shouldBe` [ElabTypes.typeBinderRefIdentity refA, ElabTypes.typeBinderRefIdentity refB]

      it "keeps same-named SchemeInfo binder refs distinct from name subst" $ do
        let refA = graphTypeBinderRef 81 "a"
            refB = graphTypeBinderRef 82 "a"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(refA, Nothing), (refB, Nothing)]
                (Elab.TArrow (ElabTypes.tVarWithRef refB) (ElabTypes.tVarWithRef refB))
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(81, "a"), (82, "a")])
            binderIdentityKeys =
              map
                (ElabTypes.typeBinderIdentityKey . ElabTypes.typeBinderRefIdentity . fst)
                (ElabTypes.schemeBinderRefs (Elab.siScheme si))
        binderIdentityKeys `shouldBe` [81, 82]
        case ElabTypes.schemeBody (Elab.siScheme si) of
          Elab.TArrow (Elab.TVarRef leftRef) (Elab.TVarRef rightRef) ->
            map (ElabTypes.typeBinderIdentityKey . ElabTypes.typeBinderRefIdentity) [leftRef, rightRef]
              `shouldBe` [82, 82]
          other ->
            expectationFailure ("Expected duplicate-name SchemeInfo body refs, got: " ++ show other)

      it "seeds mkEnv generated env identities after scheme identities" $ do
        let ref =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 0))
                "a"
            schemeInfo =
              Elab.schemeInfoFromRefSubst
                (Elab.schemeFromType (ElabTypes.tVarWithRef ref))
                IntMap.empty
            env = Algebra.mkEnv (Map.singleton "x" schemeInfo)
            generatedEnvIdentities =
              [ identity
              | (ResolvedVar {resolvedVarDetails = EnvId envRef}, _) <-
                  Elab.resolvedTermEnvEntries (Elab.resolvedTermEnv (Algebra.typeCheckEnvFrom env))
              , let identity = envRefIdentity envRef
              ]
        generatedEnvIdentities `shouldBe` [UniqueIdentity 1]

      it "looks up SchemeInfo by resolved identity when names are stale" $ do
        let targetSchemeInfo =
              Elab.schemeInfoFromRefSubst
                (Elab.schemeFromType (TestElab.tBase (BaseTy "Int")))
                IntMap.empty
            decoySchemeInfo =
              Elab.schemeInfoFromRefSubst
                (Elab.schemeFromType (TestElab.tBase (BaseTy "Bool")))
                IntMap.empty
            targetDetails = EnvId (envRefFromIdentity (UniqueIdentity 991900) "actual")
            decoyDetails = EnvId (envRefFromIdentity (UniqueIdentity 991901) "actual")
            targetResolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Int"),
                  resolvedVarDetails = targetDetails
                }
            decoyResolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Bool"),
                  resolvedVarDetails = decoyDetails
                }
            env =
              Algebra.mkEnvWithResolvedBindings
                ( Map.fromList
                    [ ("actual", (decoySchemeInfo, decoyResolved)),
                      ("$stale_actual", (targetSchemeInfo, targetResolved))
                    ]
                )
            resolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Int"),
                  resolvedVarDetails = targetDetails
                }
        Algebra.lookupSchemeInfoForResolved resolved env `shouldBe` Just targetSchemeInfo

      it "does not fall back to source names when resolved term head identity is absent" $ do
        let decoySchemeInfo =
              Elab.schemeInfoFromRefSubst
                (Elab.schemeFromType (TestElab.tBase (BaseTy "Bool")))
                IntMap.empty
            decoyResolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Bool"),
                  resolvedVarDetails = EnvId (envRefFromIdentity (UniqueIdentity 991901) "actual")
                }
            missingResolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Int"),
                  resolvedVarDetails = EnvId (envRefFromIdentity (UniqueIdentity 991903) "actual")
                }
            env =
              Algebra.mkEnvWithResolvedBindings
                (Map.singleton "actual" (decoySchemeInfo, decoyResolved))
        Algebra.lookupSchemeInfoForResolved missingResolved env
          `shouldBe` Nothing

      it "looks up SchemeInfo across local evidence identity aliases" $ do
        let targetSchemeInfo =
              Elab.schemeInfoFromRefSubst
                (Elab.schemeFromType (TestElab.tBase (BaseTy "Int")))
                IntMap.empty
            localRef = localRefFromNodeId "x" (NodeId 991902)
            targetResolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Int"),
                  resolvedVarDetails = LocalId localRef
                }
            env =
              Algebra.mkEnvWithResolvedBindings
                (Map.singleton "x" (targetSchemeInfo, targetResolved))
            resolved =
              ResolvedVar
                {
                  resolvedVarType = TestElab.tBase (BaseTy "Int"),
                  resolvedVarDetails = EvidenceId localRef
                }
        Algebra.lookupSchemeInfoForResolved resolved env `shouldBe` Just targetSchemeInfo

      it "freshens later SchemeInfo binders by identity when only later names collide" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 61))
                "a"
            refB =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 62))
                "b"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [ (refA, Nothing),
                  (refB, Just (boundFromType (Elab.TArrow (ElabTypes.tVarWithRef refA) (TestElab.tBase (BaseTy "Int")))))
                ]
                (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB))
            schemeInfo =
              Elab.schemeInfoFromRefSubst
                scheme
                (IntMap.fromList [(61, refA), (62, refB)])
            reservedRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 160))
                "b"
            reserved =
              Algebra.mkEnv
                ( Map.singleton
                    "captured"
                    (Elab.schemeInfoFromRefSubst (Elab.schemeFromType (ElabTypes.tVarWithRef reservedRef)) IntMap.empty)
                )
            freshened = Algebra.freshenSchemeInfoAgainstEnv reserved schemeInfo
        case ElabTypes.schemeBinderRefs (Elab.siScheme freshened) of
          [(refA', Nothing), (refB', Just bound)] -> do
            ElabTypes.typeBinderRefIdentity refA' `shouldBe` ElabTypes.typeBinderRefIdentity refA
            ElabTypes.typeBinderRefName refA' `shouldBe` "a"
            ElabTypes.typeBinderRefIdentity refB' `shouldBe` ElabTypes.typeBinderRefIdentity refB
            ElabTypes.typeBinderRefName refB' `shouldBe` "b1"
            ElabTypes.tyToElab bound `shouldBe` Elab.TArrow (ElabTypes.tVarWithRef refA') (TestElab.tBase (BaseTy "Int"))
            ElabTypes.schemeInfoBinderRefSubst freshened `shouldBe` IntMap.fromList [(61, refA'), (62, refB')]
            ElabTypes.schemeBody (Elab.siScheme freshened)
              `shouldBe` Elab.TArrow (ElabTypes.tVarWithRef refA') (ElabTypes.tVarWithRef refB')
          other ->
            expectationFailure ("Expected two freshened SchemeInfo binders, got: " ++ show other)

      it "freshens same-named SchemeInfo binders independently by identity" $ do
        let refA =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 63))
                "a"
            refB =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 64))
                "a"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(refA, Nothing), (refB, Nothing)]
                (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB))
            schemeInfo =
              Elab.schemeInfoFromRefSubst
                scheme
                (IntMap.fromList [(63, refA), (64, refB)])
            reservedRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 164))
                "a"
            reserved =
              Algebra.mkEnv
                ( Map.singleton
                    "captured"
                    (Elab.schemeInfoFromRefSubst (Elab.schemeFromType (ElabTypes.tVarWithRef reservedRef)) IntMap.empty)
                )
            freshened = Algebra.freshenSchemeInfoAgainstEnv reserved schemeInfo
        case ElabTypes.schemeBinderRefs (Elab.siScheme freshened) of
          [(refA', Nothing), (refB', Nothing)] -> do
            ElabTypes.typeBinderRefIdentity refA' `shouldBe` ElabTypes.typeBinderRefIdentity refA
            ElabTypes.typeBinderRefName refA' `shouldBe` "a1"
            ElabTypes.typeBinderRefIdentity refB' `shouldBe` ElabTypes.typeBinderRefIdentity refB
            ElabTypes.typeBinderRefName refB' `shouldBe` "a2"
            ElabTypes.schemeInfoBinderRefSubst freshened `shouldBe` IntMap.fromList [(63, refA'), (64, refB')]
            ElabTypes.schemeBody (Elab.siScheme freshened)
              `shouldBe` Elab.TArrow (ElabTypes.tVarWithRef refA') (ElabTypes.tVarWithRef refB')
          other ->
            expectationFailure ("Expected same-named SchemeInfo binders to freshen independently, got: " ++ show other)

      it "subtracts environment-free SchemeInfo binders by identity while retaining distinct local identities" $ do
        let capturedRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 65))
                "captured"
            staleCapturedBinder =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 65))
                "stale"
            localRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 66))
                "captured"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(staleCapturedBinder, Nothing), (localRef, Nothing)]
                (Elab.TArrow (ElabTypes.tVarWithRef staleCapturedBinder) (ElabTypes.tVarWithRef localRef))
            schemeInfo =
              Elab.schemeInfoFromRefSubst
                scheme
                (IntMap.fromList [(65, staleCapturedBinder), (66, localRef)])
            reserved =
              Algebra.mkEnv
                ( Map.singleton
                    "captured"
                    (Elab.schemeInfoFromRefSubst (Elab.schemeFromType (ElabTypes.tVarWithRef capturedRef)) IntMap.empty)
                )
            generalized = Algebra.freshenSchemeInfoAgainstEnv reserved schemeInfo
        case ElabTypes.schemeBinderRefs (Elab.siScheme generalized) of
          [(localRef', Nothing)] -> do
            ElabTypes.typeBinderRefIdentity localRef'
              `shouldBe` ElabTypes.typeBinderRefIdentity localRef
            ElabTypes.typeBinderRefName localRef' `shouldBe` "captured1"
            ElabTypes.schemeInfoBinderRefSubst generalized
              `shouldBe` IntMap.fromList [(65, capturedRef), (66, localRef')]
            ElabTypes.schemeBody (Elab.siScheme generalized)
              `shouldBe` Elab.TArrow (ElabTypes.tVarWithRef capturedRef) (ElabTypes.tVarWithRef localRef')
            fmap ElabTypes.typeBinderRefName
              (IntMap.lookup 65 (ElabTypes.schemeInfoBinderRefSubst generalized))
              `shouldBe` Just "captured"
            case ElabTypes.schemeBody (Elab.siScheme generalized) of
              Elab.TArrow (Elab.TVarRef alignedCapturedRef) _ ->
                ElabTypes.typeBinderRefName alignedCapturedRef `shouldBe` "captured"
              otherBody ->
                expectationFailure ("Expected aligned captured ref in scheme body, got: " ++ show otherBody)
          other ->
            expectationFailure
              ("Expected only the environment-free binder to be subtracted, got: " ++ show other)

      it "aligns canonically equivalent graph binders to the environment identity before subtraction" $ do
        let capturedRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 27))
                "captured"
            staleAliasBinder =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
                "stale"
            localRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
                "captured"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [ (staleAliasBinder, Nothing),
                  ( localRef,
                    Just
                      ( boundFromType
                          ( Elab.TArrow
                              (ElabTypes.tVarWithRef staleAliasBinder)
                              (TestElab.tBase (BaseTy "Int"))
                          )
                      )
                  )
                ]
                (Elab.TArrow (ElabTypes.tVarWithRef staleAliasBinder) (ElabTypes.tVarWithRef localRef))
            schemeInfo =
              Elab.schemeInfoFromRefSubst
                scheme
                (IntMap.fromList [(1, staleAliasBinder), (2, localRef)])
            reserved =
              Algebra.mkEnv
                ( Map.singleton
                    "captured"
                    (Elab.schemeInfoFromRefSubst (Elab.schemeFromType (ElabTypes.tVarWithRef capturedRef)) IntMap.empty)
                )
            canonical node
              | node == NodeId 1 = NodeId 27
              | otherwise = node
            generalized =
              Algebra.freshenSchemeInfoAgainstEnvWithRepresentative canonical reserved schemeInfo
        case ElabTypes.schemeBinderRefs (Elab.siScheme generalized) of
          [(localRef', Just localBound)] -> do
            ElabTypes.typeBinderRefIdentity localRef'
              `shouldBe` ElabTypes.typeBinderRefIdentity localRef
            ElabTypes.typeBinderRefName localRef' `shouldBe` "captured1"
            ElabTypes.tyToElab localBound
              `shouldBe` Elab.TArrow (ElabTypes.tVarWithRef capturedRef) (TestElab.tBase (BaseTy "Int"))
            ElabTypes.schemeInfoBinderRefSubst generalized
              `shouldBe` IntMap.fromList [(1, capturedRef), (2, localRef')]
            ElabTypes.schemeBody (Elab.siScheme generalized)
              `shouldBe` Elab.TArrow (ElabTypes.tVarWithRef capturedRef) (ElabTypes.tVarWithRef localRef')
            fmap ElabTypes.typeBinderRefName
              (IntMap.lookup 1 (ElabTypes.schemeInfoBinderRefSubst generalized))
              `shouldBe` Just "captured"
            case ElabTypes.schemeBody (Elab.siScheme generalized) of
              Elab.TArrow (Elab.TVarRef alignedCapturedRef) _ ->
                ElabTypes.typeBinderRefName alignedCapturedRef `shouldBe` "captured"
              otherBody ->
                expectationFailure ("Expected canonical captured ref in scheme body, got: " ++ show otherBody)
          other ->
            expectationFailure
              ("Expected canonical environment identity alignment before subtraction, got: " ++ show other)

      it "does not subtract a SchemeInfo binder through an alias-only construction route" $ do
        let localRef = graphTypeBinderRef 167 "local"
            aliasRef = graphTypeBinderRef 168 "exact-alias"
            schemeInfo =
              Elab.schemeInfoFromRefSubst
                ( ElabTypes.mkElabSchemeWithRefs
                    [(localRef, Nothing)]
                    (ElabTypes.tVarWithRef localRef)
                )
                (IntMap.singleton 167 localRef)
            aliasOnlyEnv =
              Algebra.extendEnvTypeScopeWithAliases
                (IntMap.singleton 167 aliasRef)
                []
                (Algebra.mkEnv Map.empty)
            generalized =
              Algebra.freshenSchemeInfoAgainstEnv aliasOnlyEnv schemeInfo
        ElabTypes.schemeBinderRefs (Elab.siScheme generalized)
          `shouldBe` [(localRef, Nothing)]
        ElabTypes.schemeBody (Elab.siScheme generalized)
          `shouldBe` ElabTypes.tVarWithRef localRef
        ElabTypes.schemeInfoBinderRefSubst generalized
          `shouldBe` IntMap.singleton 167 localRef

      it "lets an exact current construction binder shadow an ambient representative" $ do
        let ambientRef = graphTypeBinderRef 130 "ambient"
            localRef = graphTypeBinderRef 162 "local"
            representative node
              | node == NodeId 130 || node == NodeId 162 = NodeId 1
              | otherwise = node
        AlgebraTestSupport.constructionRefAlreadyInGammaForTest
          representative
          [localRef]
          [ambientRef, localRef]
          localRef
          `shouldBe` False

      it "keeps a non-local representative-equivalent binder in ambient Gamma" $ do
        let ambientRef = graphTypeBinderRef 130 "ambient"
            localRef = graphTypeBinderRef 162 "local"
            inheritedRef = graphTypeBinderRef 166 "inherited"
            representative node
              | node `elem` [NodeId 130, NodeId 162, NodeId 166] = NodeId 1
              | otherwise = node
        AlgebraTestSupport.constructionRefAlreadyInGammaForTest
          representative
          [localRef]
          [ambientRef]
          inheritedRef
          `shouldBe` True

      it "enters only an exact direct source declaration into lambda construction" $ do
        let sourceNode = NodeId 6
            sourceRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 14))
                "a"
            differentRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 15))
                "b"
            graphRef = graphTypeBinderRef 6 "t6"
            directSourceRefs =
              IntMap.singleton (getNodeId sourceNode) sourceRef
            directRename =
              AlgebraTestSupport.directSourceBinderConstructionRenameForTest
                directSourceRefs
                (getNodeId sourceNode)
        directRename sourceRef `shouldBe` Just (graphRef, sourceRef)
        AlgebraTestSupport.directSourceBinderConstructionRenameForTest
          IntMap.empty
          (getNodeId sourceNode)
          sourceRef
          `shouldBe` Nothing
        directRename differentRef `shouldBe` Nothing

      it "projects a copied occurrence only through its exact source sidecar route" $ do
        let occurrenceKey = 6
            sourceRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 14))
                "a"
            differentRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 15))
                "b"
            sourceRefs = IntMap.singleton occurrenceKey sourceRef
            routesFor routedRef = IntMap.singleton occurrenceKey routedRef
        AlgebraTestSupport.certifiedSourceOccurrenceRoutesForTest
          sourceRefs
          (routesFor sourceRef)
          `shouldBe` routesFor sourceRef
        AlgebraTestSupport.certifiedSourceOccurrenceRenamesForTest
          sourceRefs
          (routesFor sourceRef)
          `shouldBe` [(graphTypeBinderRef occurrenceKey "a", sourceRef)]
        AlgebraTestSupport.certifiedSourceOccurrenceRoutesForTest
          sourceRefs
          (routesFor differentRef)
          `shouldBe` IntMap.empty
        AlgebraTestSupport.certifiedSourceOccurrenceRoutesForTest
          IntMap.empty
          (routesFor sourceRef)
          `shouldBe` IntMap.empty

      it "orients a packet occurrence to its source only through matching packet and source certificates" $ do
        let occurrenceKey = 15
            sourceRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 15))
                "b"
            sameNamedPeer =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 16))
                "b"
            graphRef = graphTypeBinderRef occurrenceKey "__rigid15"
            otherGraphRef = graphTypeBinderRef 16 "__rigid16"
            sourceSidecar =
              IntMap.singleton occurrenceKey sourceRef
            select =
              AlgebraTestSupport.certifiedSourcePacketOccurrenceRenamesForTest
            selectOperated =
              AlgebraTestSupport.certifiedSourcePacketOperatedOccurrenceRenamesForTest
            completedPacketRefs =
              IntMap.singleton occurrenceKey sourceRef
            operatedPacketRefs =
              IntMap.singleton occurrenceKey graphRef
        select sourceSidecar [(sourceRef, graphRef)]
          `shouldBe` [(graphRef, sourceRef)]
        selectOperated
          sourceSidecar
          completedPacketRefs
          operatedPacketRefs
          `shouldBe` [(graphRef, sourceRef)]
        AlgebraTestSupport.selectTermSourcePacketOccurrenceRenamesForTest
          [graphRef]
          [(graphRef, sourceRef)]
          `shouldBe` [(graphRef, sourceRef)]
        AlgebraTestSupport.selectTermSourcePacketOccurrenceRenamesForTest
          [sourceRef]
          [(graphRef, sourceRef)]
          `shouldBe` []
        AlgebraTestSupport.lambdaBodyConstructionRenamesForTest
          Set.empty
          [(graphRef, sourceRef)]
          [(sourceRef, graphRef)]
          `shouldBe` []
        AlgebraTestSupport.lambdaBodyConstructionRenamesForTest
          Set.empty
          []
          [(sourceRef, graphRef)]
          `shouldBe` [(sourceRef, graphRef)]
        AlgebraTestSupport.lambdaBodyConstructionRenamesForTest
          Set.empty
          [(otherGraphRef, sourceRef)]
          [(sourceRef, graphRef)]
          `shouldBe` [(sourceRef, graphRef)]
        AlgebraTestSupport.lambdaBodyConstructionRenamesForTest
          Set.empty
          [(graphRef, sameNamedPeer)]
          [(sourceRef, graphRef)]
          `shouldBe` [(sourceRef, graphRef)]
        select IntMap.empty [(sourceRef, graphRef)] `shouldBe` []
        select
          (IntMap.singleton occurrenceKey sameNamedPeer)
          [(sourceRef, graphRef)]
          `shouldBe` []
        select sourceSidecar [(sourceRef, otherGraphRef)] `shouldBe` []
        select sourceSidecar [(graphRef, sourceRef)] `shouldBe` []
        selectOperated
          IntMap.empty
          completedPacketRefs
          operatedPacketRefs
          `shouldBe` []
        selectOperated
          sourceSidecar
          (IntMap.singleton occurrenceKey sameNamedPeer)
          operatedPacketRefs
          `shouldBe` []
        selectOperated
          sourceSidecar
          completedPacketRefs
          (IntMap.singleton occurrenceKey otherGraphRef)
          `shouldBe` []

      it "returns a used ambient graph occurrence to its protected lambda boundary" $ do
        let occurrenceKey = 37
            graphRef = graphTypeBinderRef occurrenceKey "c"
            boundaryRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 16))
                "a"
        AlgebraTestSupport.protectedBoundaryOccurrenceRenamesForTest
          (Set.singleton (ElabTypes.typeBinderRefIdentity boundaryRef))
          (IntMap.singleton occurrenceKey boundaryRef)
          [graphRef]
          []
          `shouldBe` [(graphTypeBinderRef occurrenceKey "a", boundaryRef)]

      it "does not project an unprotected, unused, or locally emitted child occurrence" $ do
        let occurrenceKey = 37
            graphRef = graphTypeBinderRef occurrenceKey "c"
            boundaryRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 16))
                "a"
            aliases = IntMap.singleton occurrenceKey boundaryRef
            protected =
              Set.singleton (ElabTypes.typeBinderRefIdentity boundaryRef)
            select =
              AlgebraTestSupport.protectedBoundaryOccurrenceRenamesForTest
        select Set.empty aliases [graphRef] [] `shouldBe` []
        select protected aliases [] [] `shouldBe` []
        select protected aliases [graphRef] [graphRef] `shouldBe` []

      it "projects a validated body consumer bound at its exact ambient identity" $ do
        let consumerRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 16))
                "consumer"
            paramRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 17))
                "a"
            declaredBound =
              ElabTypes.TArrow ElabTypes.TBottom ElabTypes.TBottom
            projectedBound =
              ElabTypes.TArrow
                (ElabTypes.TVarRef paramRef)
                (ElabTypes.TVarRef paramRef)
        AlgebraTestSupport.projectValidatedAmbientConsumerBoundForTest
          AlgebraTestSupport.DirectAmbientEstablished
          consumerRef
          declaredBound
          projectedBound
          (Map.singleton consumerRef declaredBound)
          `shouldBe` Right (Map.singleton consumerRef projectedBound)

      it "does not project a same-named peer or overwrite a contradictory ambient declaration" $ do
        let consumerRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 16))
                "consumer"
            sameNamedPeer =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 17))
                "consumer"
            declaredBound =
              ElabTypes.TArrow ElabTypes.TBottom ElabTypes.TBottom
            projectedBound =
              ElabTypes.TArrow
                (ElabTypes.TVarRef sameNamedPeer)
                (ElabTypes.TVarRef sameNamedPeer)
            peerBindings = Map.singleton sameNamedPeer declaredBound
            project =
              AlgebraTestSupport.projectValidatedAmbientConsumerBoundForTest
                AlgebraTestSupport.DirectAmbientEstablished
                consumerRef
                declaredBound
                projectedBound
        project peerBindings `shouldBe` Right peerBindings
        project (Map.singleton consumerRef ElabTypes.TBottom)
          `shouldSatisfy` isLeft

      it "completes a structured root slot only with frozen provisional-result provenance" $ do
        let consumerRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 39))
                "consumer"
            paramRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 17))
                "a"
            rootSkeleton =
              ElabTypes.TArrow ElabTypes.TBottom ElabTypes.TBottom
            completedBound =
              ElabTypes.TArrow
                (ElabTypes.TVarRef paramRef)
                (ElabTypes.TVarRef paramRef)
            project provenance =
              AlgebraTestSupport.projectValidatedAmbientConsumerBoundForTest
                provenance
                consumerRef
                completedBound
                completedBound
                (Map.singleton consumerRef rootSkeleton)
        project AlgebraTestSupport.DirectAmbientProvisionalNestedResult
          `shouldBe` Right (Map.singleton consumerRef completedBound)
        project AlgebraTestSupport.DirectAmbientEstablished
          `shouldSatisfy` isLeft

      it "keeps an idempotent direct source route before its bounds materialize" $ do
        let graphRef = graphTypeBinderRef 6 "t6"
            sourceRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 14))
                "a"
            otherRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 15))
                "b"
            compatible =
              AlgebraTestSupport.constructionRouteBoundCompatibleForTest
                [(graphRef, sourceRef)]
                graphRef
        compatible sourceRef Nothing Nothing `shouldBe` True
        compatible otherRef Nothing Nothing `shouldBe` False

      it "rejects a fresh construction route between incompatible materialized bounds" $ do
        let graphRef = graphTypeBinderRef 7 "graph"
            outwardRef = graphTypeBinderRef 8 "outward"
            intTy = TestElab.tBase (BaseTy "Int")
            boolTy = TestElab.tBase (BaseTy "Bool")
            compatible =
              AlgebraTestSupport.constructionRouteBoundCompatibleForTest
                []
                graphRef
                outwardRef
        compatible (Just intTy) (Just intTy) `shouldBe` True
        compatible (Just intTy) (Just boolTy) `shouldBe` False

      it "subtracts a construction identity whose final route is an exact ambient Gamma identity" $ do
        let graphRef = graphTypeBinderRef 169 "consumer"
            ambientRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 7006))
                "ambient"
            localRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 7007))
                "local"
            graphIdentity = ElabTypes.typeBinderRefIdentity graphRef
            localIdentity = ElabTypes.typeBinderRefIdentity localRef
            schemeInfoFor routedRef =
              ElabTypes.schemeInfoFromRefSubst
                ( ElabTypes.mkElabSchemeWithRefs
                    [(graphRef, Nothing)]
                    (ElabTypes.tVarWithRef graphRef)
                )
                (IntMap.singleton 169 routedRef)
            ambientOwnsExact ref =
              ElabTypes.typeBinderRefsSameIdentity ref ambientRef
            protectedFor routedRef =
              AlgebraTestSupport.constructionProtectedIdentitiesForTest
                ambientOwnsExact
                IntMap.empty
                (schemeInfoFor routedRef)
                (Set.singleton graphIdentity)
            protectedThroughAmbientRoute =
              AlgebraTestSupport.constructionProtectedIdentitiesForTest
                ambientOwnsExact
                (IntMap.singleton 169 ambientRef)
                (schemeInfoFor graphRef)
                (Set.singleton graphIdentity)
        protectedFor ambientRef `shouldBe` Set.empty
        protectedThroughAmbientRoute `shouldBe` Set.empty
        protectedFor localRef
          `shouldBe` Set.fromList [graphIdentity, localIdentity]

      it "consumes an ordinary body Gamma packet prefix exactly once" $ do
        let packetRef = graphTypeBinderRef 992130 "packet"
            resultRef = graphTypeBinderRef 992131 "result"
            bodyRef = graphTypeBinderRef 992132 "body-packet"
            packetBound =
              ElabTypes.tForallWithRef
                packetRef
                Nothing
                ( Elab.TArrow
                    (ElabTypes.tVarWithRef packetRef)
                    (ElabTypes.tVarWithRef resultRef)
                )
            checkedBodyTy =
              Elab.TArrow
                (ElabTypes.tVarWithRef bodyRef)
                (ElabTypes.tVarWithRef resultRef)
            edgeComputation =
              Elab.InstSeq
                Elab.InstElim
                (Elab.InstAbstrRef resultRef)
            expectedConstruction =
              ( [(packetRef, bodyRef)]
              , ElabTypes.mkElabSchemeWithRefs
                  [(bodyRef, Nothing)]
                  checkedBodyTy
              , Elab.InstAbstrRef resultRef
              )
        AlgebraTestSupport.constructOrdinaryGammaPacketForTest
          checkedBodyTy
          (Just (resultRef, packetBound))
          edgeComputation
          `shouldBe` Just expectedConstruction
        AlgebraTestSupport.constructOrdinaryGammaPacketForTest
          checkedBodyTy
          (Just (resultRef, packetBound))
          (Elab.InstAbstrRef resultRef)
          `shouldBe` Just expectedConstruction
        AlgebraTestSupport.constructOrdinaryGammaPacketForTest
          checkedBodyTy
          Nothing
          edgeComputation
          `shouldBe` Nothing

      it "selects a lambda packet result bound by provenance, independent of source order" $ do
        let resultRef = graphTypeBinderRef 992133 "result"
            localTypeRef = graphTypeBinderRef 992134 "a"
            annotationHeadRef = graphTypeBinderRef 992135 "f"
            intTy = TestElab.tBase (BaseTy "Int")
            localGammaBound =
              boundFromType
                ( ElabTypes.tForallWithRef
                    localTypeRef
                    Nothing
                    ( Elab.TArrow
                        (ElabTypes.tVarWithRef localTypeRef)
                        (ElabTypes.tVarWithRef localTypeRef)
                    )
                )
            sourceAnnotationBound =
              boundFromType
                (Elab.TVarAppRef annotationHeadRef (intTy NE.:| []))
            exactEndpointBound =
              boundFromType (Elab.TArrow intTy intTy)
            candidates =
              [ ( AlgebraTestSupport.ConstructionSourceAnnotationEndpoint
                , (resultRef, Just sourceAnnotationBound)
                )
              , ( AlgebraTestSupport.ConstructionExactEndpoint
                , (resultRef, Just exactEndpointBound)
                )
              , ( AlgebraTestSupport.ConstructionLocalGammaBound
                , (resultRef, Just localGammaBound)
                )
              ]
            expected = Right [(resultRef, Just localGammaBound)]
        forM_ [candidates, reverse candidates] $ \orderedCandidates ->
          AlgebraTestSupport.mergeConstructionBinderBoundsByProvenanceForTest
            "lambda packet result"
            orderedCandidates
            `shouldBe` expected

      it "recognizes an exact transport endpoint beneath a vacuous bounded Gamma prefix" $ do
        let vacuousRef = graphTypeBinderRef 992136 "captured"
            endpointRef = graphTypeBinderRef 992137 "result"
            intTy = TestElab.tBase (BaseTy "Int")
            endpoint =
              ElabTypes.tForallWithRef
                endpointRef
                Nothing
                (Elab.TArrow intTy intTy)
            sourceScheme =
              ElabTypes.mkElabSchemeWithRefs
                [(vacuousRef, Just (boundFromType intTy))]
                endpoint
        AlgebraTestSupport.inferExactTransportArgumentsForTest
          TypeOps.alphaEqType
          sourceScheme
          endpoint
          `shouldBe` Just []

      it "uses established ambient authority to resolve substitution-only construction routes" $ do
        let establishedRef = graphTypeBinderRef 992138 "established"
            routedRef = graphTypeBinderRef 992139 "routed"
            select establishedAuthority localRefs =
              AlgebraTestSupport.selectBoundaryConstructionRouteForTest
                establishedAuthority
                localRefs
                establishedRef
                routedRef
        select (Just establishedRef) [] `shouldBe` Just establishedRef
        select (Just routedRef) [] `shouldBe` Just routedRef
        select Nothing [] `shouldBe` Nothing
        select (Just establishedRef) [routedRef] `shouldBe` Just routedRef

      it "uses the nearest lexical frozen endpoint provider for a shared graph root" $ do
        let sharedRoot = NodeId 992140
            enclosingEndpoint =
              ElabTypes.tVarWithRef
                (graphTypeBinderRef 992141 "enclosing-endpoint")
            directSiblingEndpoint =
              ElabTypes.tVarWithRef
                (graphTypeBinderRef 992142 "direct-sibling-endpoint")
        AlgebraTestSupport.frozenEndpointTypesByLexicalPublicationForTest
          [ (EdgeId 992143, sharedRoot, enclosingEndpoint)
          , (EdgeId 992144, sharedRoot, directSiblingEndpoint)
          ]
          `shouldBe` IntMap.singleton
            (getNodeId sharedRoot)
            directSiblingEndpoint

      it "publishes a graph dependency binder through its own declaration node" $ do
        let dependencyRef = graphTypeBinderRef 15 "__rigid15"
            ownerRef = graphTypeBinderRef 65 "a"
            ownerBound =
              boundFromType
                ( Elab.TArrow
                    (ElabTypes.tVarWithRef dependencyRef)
                    (ElabTypes.tVarWithRef ownerRef)
                )
            binders =
              [ (dependencyRef, Nothing)
              , (ownerRef, Just ownerBound)
              ]
            ownerRoutes =
              IntMap.fromList
                [ (62, ownerRef)
                , (65, ownerRef)
                ]
        AlgebraTestSupport.localGammaConstructionProvenanceForTest
          "lambda dependency route"
          binders
          [ownerRoutes]
          IntMap.empty
          `shouldBe`
            Right
              ( IntMap.fromList
                  [ (15, dependencyRef)
                  , (62, ownerRef)
                  , (65, ownerRef)
                  ]
              , IntMap.empty
              )

      it "delegates a graph node only when the displaced binder has exact source authority" $ do
        let dependencyKey = 15
            dependencyRef = graphTypeBinderRef dependencyKey "__rigid15"
            ownerRef = graphTypeBinderRef 65 "a"
            binders =
              [ (dependencyRef, Nothing)
              , (ownerRef, Nothing)
              ]
            ownerRoutes =
              IntMap.fromList
                [ (dependencyKey, ownerRef)
                , (65, ownerRef)
                ]
            provenance sourceRefs =
              AlgebraTestSupport.localGammaConstructionProvenanceForTest
                "lambda delegated dependency route"
                binders
                [ownerRoutes]
                sourceRefs
        provenance (IntMap.singleton dependencyKey dependencyRef)
          `shouldBe`
            Right
              ( ownerRoutes
              , IntMap.singleton dependencyKey dependencyRef
              )
        provenance IntMap.empty `shouldSatisfy` isLeft

      it "keeps an exact source-owned generated declaration out of graph construction routes" $ do
        let sourceKey = 15
            sourceRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 15))
                "$typevar#15"
            sameNamedPeer =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 16))
                "$typevar#15"
            ownerRef = graphTypeBinderRef 65 "a"
            binders =
              [ (sourceRef, Nothing)
              , (ownerRef, Nothing)
              ]
            ownerRoutes =
              IntMap.fromList
                [ (62, ownerRef)
                , (65, ownerRef)
                ]
            provenance sourceAuthority =
              AlgebraTestSupport.localGammaConstructionProvenanceForTest
                "lambda source declaration"
                binders
                [ownerRoutes]
                (IntMap.singleton sourceKey sourceAuthority)
        provenance sourceRef
          `shouldBe`
            Right
              ( ownerRoutes
              , IntMap.singleton sourceKey sourceRef
              )
        provenance sameNamedPeer `shouldSatisfy` isLeft

      it "recognizes only enclosing-owned completions for an open transparent Gamma result" $ do
        let resultRef = graphTypeBinderRef 11 "result"
            enclosingRef = graphTypeBinderRef 17 "enclosing"
            unrelatedRef = graphTypeBinderRef 19 "unrelated"
            representative node
              | node == NodeId 11 || node == NodeId 17 = NodeId 1
              | otherwise = node
            packetInfo =
              ElabTypes.schemeInfoFromRefSubst
                (ElabTypes.mkElabSchemeWithRefs [] (ElabTypes.tVarWithRef resultRef))
                (IntMap.singleton 11 resultRef)
            completedPacketInfo =
              ElabTypes.schemeInfoFromRefSubst
                ( ElabTypes.mkElabSchemeWithRefs
                    []
                    (TestElab.tBase (BaseTy "Int"))
                )
                (IntMap.singleton 11 resultRef)
            enclosingInfo =
              ElabTypes.schemeInfoFromRefSubst
                ( ElabTypes.mkElabSchemeWithRefs
                    [(enclosingRef, Nothing)]
                    (ElabTypes.tVarWithRef enclosingRef)
                )
                (IntMap.singleton 17 enclosingRef)
            closedMonomorphicInfo =
              ElabTypes.schemeInfoFromRefSubst
                ( ElabTypes.mkElabSchemeWithRefs
                    []
                    ( Elab.TArrow
                        (TestElab.tBase (BaseTy "Int"))
                        (TestElab.tBase (BaseTy "Int"))
                    )
                )
                IntMap.empty
            unrelatedInfo =
              ElabTypes.schemeInfoFromRefSubst
                ( ElabTypes.mkElabSchemeWithRefs
                    []
                    (ElabTypes.tVarWithRef unrelatedRef)
                )
                (IntMap.singleton 19 unrelatedRef)
            resolvedBy =
              AlgebraTestSupport.transparentResultResolvedByEnclosingSchemeForTest
                representative
                packetInfo
                resultRef
            completedResolvedBy =
              AlgebraTestSupport.transparentResultResolvedByEnclosingSchemeForTest
                representative
                completedPacketInfo
                resultRef
        resolvedBy enclosingInfo `shouldBe` True
        resolvedBy closedMonomorphicInfo `shouldBe` True
        resolvedBy unrelatedInfo `shouldBe` False
        completedResolvedBy closedMonomorphicInfo `shouldBe` True
        completedResolvedBy unrelatedInfo `shouldBe` False

      it "does not quotient distinct lexical source binders through one solved representative" $ do
        let representativeNode = NodeId 7
            localNode = NodeId 37
            foreignNode = NodeId 38
            localRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 7004))
                "local"
            foreignRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 7005))
                "foreign"
            representative node
              | node == localNode || node == foreignNode = representativeNode
              | otherwise = node
            sourceRefs =
              IntMap.fromList
                [ (getNodeId representativeNode, foreignRef),
                  (getNodeId localNode, localRef)
                ]
            constructionAliases =
              IntMap.singleton (getNodeId localNode) localRef
        AlgebraTestSupport.mergeConstructionSourceBinderRefsForTest
          representative
          sourceRefs
          constructionAliases
          `shouldBe` Right sourceRefs
        AlgebraTestSupport.mergeConstructionSourceBinderRefsForTest
          representative
          (IntMap.singleton (getNodeId localNode) localRef)
          constructionAliases
          `shouldBe`
            Right
              ( IntMap.fromList
                  [ (getNodeId representativeNode, localRef),
                    (getNodeId localNode, localRef)
                  ]
              )
        AlgebraTestSupport.mergeConstructionSourceBinderRefsForTest
          representative
          ( IntMap.fromList
              [ (getNodeId representativeNode, foreignRef),
                (getNodeId localNode, foreignRef)
              ]
          )
          constructionAliases
          `shouldBe`
            Right
              ( IntMap.fromList
                  [ (getNodeId representativeNode, foreignRef),
                    (getNodeId localNode, localRef)
                  ]
              )
        AlgebraTestSupport.mergeConstructionSourceBinderRefsForTest
          representative
          (IntMap.singleton (getNodeId representativeNode) localRef)
          ( IntMap.fromList
              [ (getNodeId representativeNode, foreignRef),
                (getNodeId localNode, localRef)
              ]
          )
          `shouldBe`
            Right
              ( IntMap.fromList
                  [ (getNodeId representativeNode, foreignRef),
                    (getNodeId localNode, localRef)
                  ]
              )
        AlgebraTestSupport.mergeConstructionSourceBinderRefsForTest
          representative
          IntMap.empty
          ( IntMap.fromList
              [ (getNodeId localNode, localRef),
                (getNodeId foreignNode, foreignRef)
              ]
          )
          `shouldBe`
            Right
              ( IntMap.fromList
                  [ (getNodeId localNode, localRef),
                    (getNodeId foreignNode, foreignRef)
                  ]
              )

      it "protects an ordinary constructed lambda domain from packet renaming" $ do
        let paramNode = NodeId 992139
            constructedRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992138))
                "a"
            packetRef = graphTypeBinderRef 992137 "b"
            constructedTy = ElabTypes.tVarWithRef constructedRef
            protected =
              Set.singleton
                (ElabTypes.typeBinderRefIdentity constructedRef)
        AlgebraTestSupport.installConstructedLambdaParamBoundaryForTest
          paramNode
          constructedTy
          IntMap.empty
          Map.empty
          `shouldBe`
            Right
              ( constructedTy
              , protected
              , IntMap.empty
              , Map.empty
              )
        AlgebraTestSupport.lambdaParamConstructionRenamesForTest
          protected
          [(constructedRef, packetRef)]
          `shouldBe` []

      it "keeps a nullary evidence method domain instead of refining its root alias" $ do
        let paramNode = NodeId 992140
            classParamRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992141))
                "a"
            classParamTy = ElabTypes.tVarWithRef classParamRef
            exactEvidenceTy =
              Elab.TArrow
                classParamTy
                (Elab.TArrow classParamTy classParamTy)
            publishedRootBound =
              Elab.TArrow
                (TestElab.tBase (BaseTy "Int"))
                (TestElab.tBase (BaseTy "Int"))
            aliases =
              IntMap.singleton
                (getNodeId paramNode)
                classParamRef
            bindings =
              Map.singleton classParamRef publishedRootBound

        AlgebraTestSupport.installExactLambdaParamBoundaryForTest
          paramNode
          exactEvidenceTy
          (Just exactEvidenceTy)
          aliases
          bindings
          `shouldBe`
            Right
              ( exactEvidenceTy
              , Set.singleton
                  (ElabTypes.typeBinderRefIdentity classParamRef)
              , IntMap.empty
              , bindings
              )

      it "completes a source-exact parameter's own pending graph root" $ do
        let paramNode = NodeId 992147
            pendingParamRef = graphTypeBinderRef 992147 "pending"
            exactParamRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992148))
                "a"
            exactParamTy = ElabTypes.tVarWithRef exactParamRef
            aliases =
              IntMap.singleton
                (getNodeId paramNode)
                pendingParamRef

        AlgebraTestSupport.installExactLambdaParamBoundaryForTest
          paramNode
          exactParamTy
          (Just (ElabTypes.tVarWithRef pendingParamRef))
          aliases
          Map.empty
          `shouldBe`
            Right
              ( exactParamTy
              , Set.singleton
                  (ElabTypes.typeBinderRefIdentity exactParamRef)
              , IntMap.empty
              , Map.empty
              )

      it "closes an opened parameter body only when its identities do not escape to a sibling" $ do
        let paramNode = NodeId 992149
            exactParamRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992150))
                "a"
            exactParamVar = ElabTypes.tVarWithRef exactParamRef
            openedBody = Elab.TArrow exactParamVar exactParamVar
            exactParamTy =
              ElabTypes.tForallWithRef exactParamRef Nothing openedBody
            sharedSiblingBound =
              Elab.TArrow exactParamVar openedBody
            independentSiblingBound =
              Elab.TArrow (TestElab.tBase (BaseTy "Bool")) openedBody

        AlgebraTestSupport.completeExactLambdaParamBoundaryBoundForTest
          paramNode
          exactParamTy
          sharedSiblingBound
          `shouldBe` Right sharedSiblingBound
        AlgebraTestSupport.completeExactLambdaParamBoundaryBoundForTest
          paramNode
          exactParamTy
          independentSiblingBound
          `shouldBe`
            Right
              ( Elab.TArrow
                  (TestElab.tBase (BaseTy "Bool"))
                  exactParamTy
              )

      it "keeps method-local forall and class identities at a polymorphic evidence boundary" $ do
        let paramNode = NodeId 992142
            classParamRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992143))
                "a"
            methodLocalRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992144))
                "b"
            outerConstraintRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromUnique (UniqueIdentity 992145))
                "b"
            exactEvidenceTy =
              ElabTypes.tForallWithRef
                methodLocalRef
                Nothing
                ( Elab.TArrow
                    (ElabTypes.tVarWithRef methodLocalRef)
                    ( Elab.TArrow
                        (ElabTypes.tVarWithRef classParamRef)
                        (ElabTypes.tVarWithRef methodLocalRef)
                    )
                )
            capturedRootBound =
              Elab.TArrow
                (ElabTypes.tVarWithRef outerConstraintRef)
                ( Elab.TArrow
                    (ElabTypes.tVarWithRef classParamRef)
                    (ElabTypes.tVarWithRef outerConstraintRef)
                )
            specializedEvidenceTy =
              Elab.TArrow
                (ElabTypes.tVarWithRef outerConstraintRef)
                ( Elab.TArrow
                    (ElabTypes.tVarWithRef classParamRef)
                    (ElabTypes.tVarWithRef outerConstraintRef)
                )
            aliases =
              IntMap.singleton
                (getNodeId paramNode)
                outerConstraintRef
            bindings =
              Map.fromList
                [ (classParamRef, Elab.TBottom)
                , (outerConstraintRef, capturedRootBound)
                ]

        result <-
          requireRight
            ( AlgebraTestSupport.installExactLambdaParamBoundaryForTest
                paramNode
                exactEvidenceTy
                (Just exactEvidenceTy)
                aliases
                bindings
            )
        result
          `shouldBe`
            ( exactEvidenceTy
            , Set.fromList
                [ ElabTypes.typeBinderRefIdentity methodLocalRef
                , ElabTypes.typeBinderRefIdentity classParamRef
                ]
            , IntMap.empty
            , bindings
            )
        case result of
          (Elab.TForallRef installedMethodRef _ bodyTy, _, _, _) -> do
            ElabTypes.typeBinderRefsSameIdentity
              installedMethodRef
              methodLocalRef
              `shouldBe` True
            TypeOps.freeTypeVarRefsType bodyTy
              `shouldSatisfy`
                any
                  ( ElabTypes.typeBinderRefsSameIdentity
                      classParamRef
                  )
            TypeOps.freeTypeVarRefsType bodyTy
              `shouldSatisfy`
                all
                  ( not
                      . ElabTypes.typeBinderRefsSameIdentity
                        outerConstraintRef
                  )
          other ->
            expectationFailure
              ("Expected exact method-local forall boundary, got: " ++ show other)
        let exactScheme = Elab.schemeFromType exactEvidenceTy
        AlgebraTestSupport.inferInstAppArgsFromSchemeRefsForTest
          (ElabTypes.schemeBinderRefs exactScheme)
          (ElabTypes.schemeBody exactScheme)
          specializedEvidenceTy
          `shouldBe` Just [ElabTypes.tVarWithRef outerConstraintRef]
        Elab.applyInstantiation
          exactEvidenceTy
          (Elab.InstApp (ElabTypes.tVarWithRef outerConstraintRef))
          `shouldBe` Right specializedEvidenceTy
        let classConstructionRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 992146))
                "a"
        AlgebraTestSupport.lambdaParamConstructionRenamesForTest
          (Set.singleton (ElabTypes.typeBinderRefIdentity methodLocalRef))
          [ (methodLocalRef, outerConstraintRef)
          , (classParamRef, classConstructionRef)
          ]
          `shouldBe` [(classParamRef, classConstructionRef)]
        AlgebraTestSupport.lambdaParamProtectedIdentitiesForTest
          ( Set.fromList
              [ ElabTypes.typeBinderRefIdentity methodLocalRef
              , ElabTypes.typeBinderRefIdentity classParamRef
              ]
          )
          (const True)
          (IntMap.singleton (getNodeId paramNode) outerConstraintRef)
          (Elab.schemeInfoFromRefSubst exactScheme IntMap.empty)
          Set.empty
          `shouldBe`
            Set.fromList
              [ ElabTypes.typeBinderRefIdentity methodLocalRef
              , ElabTypes.typeBinderRefIdentity classParamRef
              ]
        AlgebraTestSupport.requirementNeedsLocalConstructionForTest
          False
          RequiredGammaAtCurrentScope
          False
          `shouldBe` True
        AlgebraTestSupport.requirementNeedsLocalConstructionForTest
          False
          RequiredGammaAtCurrentScope
          True
          `shouldBe` False

      it "constructs ambient Gamma authority only from a direct alias with an exact binding" $ do
        let liveNode = NodeId 17
            exactRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
                "ambient"
            exactBound = TestElab.tBase (BaseTy "Int")
        AlgebraTestSupport.buildAmbientGammaAuthoritiesForTest
          (IntMap.singleton (getNodeId liveNode) exactRef)
          [(exactRef, exactBound)]
          `shouldBe`
            Right
              ( IntMap.singleton
                  (getNodeId liveNode)
                  (exactRef, exactBound)
              )

      it "does not manufacture ambient Gamma authority from a non-exact identity binding" $ do
        let liveNode = NodeId 17
            exactRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
                "ambient"
            unrelatedRef =
              ElabTypes.typeBinderRefFromIdentity
                (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
                "ambient"
            exactBound = TestElab.tBase (BaseTy "Int")
        AlgebraTestSupport.buildAmbientGammaAuthoritiesForTest
          (IntMap.singleton (getNodeId liveNode) exactRef)
          [(unrelatedRef, exactBound)]
          `shouldBe` Right IntMap.empty

      it "accepts an explicit producer replay domain when scheme binders use mixed parseable/non-parseable names" $ do
        let root = NodeId 100
            sourceKey = NodeId 1
            replayTarget = NodeId 2
            argNode = NodeId 40
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root sourceKey sourceKey),
                          (getNodeId sourceKey, TyVar {tnId = sourceKey, tnBound = Nothing}),
                          (getNodeId replayTarget, TyVar {tnId = replayTarget, tnBound = Nothing}),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef sourceKey), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef replayTarget), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            -- Mixed binder names: "t1" parses as binder id, "a" does not.
            scheme =
              Elab.schemeFromType
                ( testTForall "t1"
                    Nothing
                    ( testTForall "a"
                        Nothing
                        (Elab.TArrow (testTVar "t1") (testTVar "t1"))
                    )
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [ (getNodeId sourceKey, "t1"),
                        (getNodeId replayTarget, "a")
                      ])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [(sourceKey, argNode)],
                  etInterior = sourceInteriorFromList [root, sourceKey, replayTarget, argNode],
                  -- Definition 15.3.12 and Lemma 15.3.13 make T(e) the
                  -- producer witness from the edge source to its destination.
                  -- The replay binder domain is therefore explicit producer
                  -- authority, never inferred from a consumer SchemeInfo's
                  -- names or substitution key-space.
                  etBinderReplayMap = IntMap.fromList [(getNodeId sourceKey, replayTarget)],
                  etReplayDomainBinders = [replayTarget],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken sourceKey]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiInvariantError msg)
            | "trace binder replay-map target outside replay binder domain" `isInfixOf` msg ->
                expectationFailure ("Expected the explicit mixed-name replay domain to be accepted, got: " ++ msg)
          Left err ->
            expectationFailure ("Expected successful translation, got " ++ show err)
          Right _ ->
            pure ()

      it "transports only the strict-replay-covered producer quantifiers" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "g"
                        (ELam "y" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))))
                        (EVar "g")
                    )
                )
                (EVar "f")
        artifacts <-
          requireRight
            (runPipelineArtifactsWithAutomaticMuDefault Set.empty expr)
        let solved = paSolved artifacts
            (inputs, _annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
            strictEdges =
              [ (trace, witness)
              | (edgeKey, trace) <- IntMap.toList (rtcEdgeTraces inputs)
              , etReplayContract trace == ReplayContractStrict
              , Just witness <- [IntMap.lookup edgeKey (rtcEdgeWitnesses inputs)]
              ]
        translated <-
          mapM
            ( \(trace, witness) -> do
                occurrence <-
                  requireRight
                    ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
                        (rtcTraceConfig inputs)
                        (generalizeAtWithActive solved)
                        (rtcPresolutionView inputs)
                        (Just (rtcBindParentsGa inputs))
                        Nothing
                        (Just trace)
                        witness
                    )
                let sourceNodes =
                      [ node
                      | (ref, _bound) <-
                          fst
                            ( TypeOps.splitForallsRefs
                                (PhiComputation.occurrenceComputationSource occurrence)
                            )
                      , Just node <- [ElabTypes.typeBinderRefNode ref]
                      ]
                    traceSourceCount =
                      IntSet.size
                        ( IntSet.fromList
                            [ getNodeId sourceBinder
                            | (sourceBinder, _argument) <- etBinderArgs trace
                            ]
                        )
                    replayTargets = IntMap.elems (etBinderReplayMap trace)
                pure (traceSourceCount, replayTargets, sourceNodes)
            )
            strictEdges
        case
            [ (replayTargets, sourceNodes)
            | (traceSourceCount, replayTargets, sourceNodes) <- translated
            , length sourceNodes > traceSourceCount
            , all (`elem` sourceNodes) replayTargets
            ]
          of
            (replayTargets, sourceNodes) : _ ->
              filter (`notElem` replayTargets) sourceNodes
                `shouldSatisfy` (not . null)
            [] ->
              expectationFailure
                ( "expected a strict replay edge whose producer keeps an untouched quantifier, got "
                    ++ show translated
                )

      it "OpWeaken on an alias target fails fast under strict replay-map resolution" $ do
        let root = NodeId 100
            binderA = NodeId 1
            binderB = NodeId 2
            aliasB = NodeId 31
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                          (getNodeId aliasB, TestTyBase aliasB (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef binderB), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            -- Binder b is solved-away to aliasB in canonical space.
            solved = mkSolved c (IntMap.fromList [(getNodeId binderB, aliasB)])
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        (Elab.TArrow (testTVar "a") (testTVar "a"))
                    )
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a"), (getNodeId binderB, "b")])
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken aliasB]
                }
        case phiFromEdgeWitnessFixtureTrace solved (Just si) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            unlines msgs `shouldSatisfy` ("OpWeaken" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast OpWeaken, got inst: " ++ show inst)

      it "OpWeaken on a shared alias class fails fast without trace fallback search" $ do
        let root = NodeId 100
            binderA = NodeId 1
            binderB = NodeId 2
            alias = NodeId 31
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                          (getNodeId alias, TestTyBase alias (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef binderB), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            -- Both binders collapse to the same alias in canonical space.
            solved = mkSolved c (IntMap.fromList [(getNodeId binderA, alias), (getNodeId binderB, alias)])
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        (Elab.TArrow (testTVar "a") (testTVar "a"))
                    )
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a"), (getNodeId binderB, "b")])
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken alias]
                }
        case phiFromEdgeWitnessFixtureTrace solved (Just si) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            unlines msgs `shouldSatisfy` ("OpWeaken" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast OpWeaken, got inst: " ++ show inst)

      it "OpWeaken on unrecoverable non-binder alias fails fast (no no-op fallback)" $ do
        let root = NodeId 100
            binderA = NodeId 1
            aliasN = NodeId 31
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId aliasN, TestTyBase aliasN (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "a"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a")])
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken aliasN]
                }
        case phiFromEdgeWitnessFixtureTrace solved (Just si) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            unlines msgs `shouldSatisfy` ("OpWeaken" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast OpWeaken, got inst: " ++ show inst)

      it "OpWeaken does not repair no-replay triple-pattern targets via nearest-key fallback" $ do
        let root = NodeId 100
            body = NodeId 101
            binderA = NodeId 1
            aliasN = NodeId 31
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId aliasN, TestTyBase aliasN (BaseTy "Bool"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTVar "a"))
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderA, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, body, binderA, aliasN],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft binderA binderA, OpGraft binderA binderA, OpWeaken aliasN]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) -> do
            let rendered = unlines msgs
            rendered `shouldSatisfy` ("OpWeaken: unresolved non-root binder target" `isInfixOf`)
            rendered `shouldSatisfy` ("non-binder target is outside replay binder key-space" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast OpWeaken nearest-key fallback removal, got inst: " ++ show inst)

      it "OpWeaken on binder target missing from quantifier spine fails fast" $ do
        let root = NodeId 100
            binderA = NodeId 1
            binderB = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef binderB), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            -- Deliberately inconsistent fixture: binderB is in ref-subst/binder key-space
            -- but absent from the scheme's quantifier spine.
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "a"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [ (getNodeId binderA, "a"),
                        (getNodeId binderB, "b")
                      ])
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken binderB]
                }
        case phiFromEdgeWitnessFixtureTrace solved (Just si) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            unlines msgs `shouldSatisfy` ("OpWeaken" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast OpWeaken, got inst: " ++ show inst)

      it "OpGraft on binder target missing from quantifier spine still fails fast even when witness-domain matches exist" $ do
        let root = NodeId 100
            binderA = NodeId 1
            binderB = NodeId 2
            argNode = NodeId 3
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root binderA binderA),
                          (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                          (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef binderB), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "a"))
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [ (getNodeId binderA, "a"),
                        (getNodeId binderB, "b")
                      ])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, binderA, binderB, argNode],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = insertCopy binderA binderB mempty,
                  etReplayContract = ReplayContractNone
                }
            presolutionView = presolutionViewFromSolved solved
            bridge =
              WitnessDomain.mkWitnessDomainBridge
                presolutionView
                (Just tr)
                (getCopyMapping (etCopyMap tr))
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft argNode binderB]
                }
        WitnessDomain.sourceKeysForNode bridge binderB
          `shouldSatisfy` elem (getNodeId binderA)
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) presolutionView Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            unlines msgs `shouldSatisfy` ("OpGraft: binder not found in quantifier spine" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast OpGraft, got inst: " ++ show inst)

      it "O15-TR-RIGID-MERGE: OpMerge with rigid operated node n translates to identity" $ do
        let root = NodeId 100
            n = NodeId 1
            m = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root n m),
                          (getNodeId n, TyVar {tnId = n, tnBound = Nothing}),
                          (getNodeId m, TyVar {tnId = m, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef n), (genRef (GenNodeId 0), BindRigid)),
                          (nodeRefKey (typeRef m), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId n, "a"), (getNodeId m, "b")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, n, m],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpMerge n m]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew)
        phi `shouldBe` Elab.InstId

      it "O15-TR-RIGID-RAISEMERGE: OpRaiseMerge with rigid operated node n translates to identity" $ do
        let root = NodeId 100
            n = NodeId 1
            m = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root n m),
                          (getNodeId n, TyVar {tnId = n, tnBound = Nothing}),
                          (getNodeId m, TyVar {tnId = m, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef n), (genRef (GenNodeId 0), BindRigid)),
                          (nodeRefKey (typeRef m), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId n, "a"), (getNodeId m, "b")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, n],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpRaiseMerge n m]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew)
        phi `shouldBe` Elab.InstId

      it "root RaiseMerge emits InstAbstr with the exact exterior identity" $ do
        let root = NodeId 100
            exterior = NodeId 200
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyVar {tnId = root, tnBound = Nothing}),
                          (getNodeId exterior, TyVar {tnId = exterior, tnBound = Nothing})
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTVar "a"))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.singleton (getNodeId root) "a")
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = exterior,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 81,
                  ewLeft = root,
                  ewRight = exterior,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpRaiseMerge root exterior]
                }
        phi <-
          requireRight
            ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                defaultTraceConfig
                (generalizeAtWithActive solved)
                (presolutionViewFromSolved solved)
                Nothing
                (Just si)
                (Just tr)
                ew
            )
        case phi of
          Elab.InstAbstrRef ref ->
            Elab.typeBinderRefNode ref `shouldBe` Just exterior
          other ->
            expectationFailure
              ("expected exact exterior InstAbstr, got " ++ show other)

      it "retains a terminal root RaiseMerge when the target bound projects to the source type" $ do
        let root = NodeId 100
            exterior = NodeId 200
            intNode = NodeId 300
            edgeId@(EdgeId edgeKey) = EdgeId 81
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyVar {tnId = root, tnBound = Nothing}),
                          (getNodeId exterior, TyVar {tnId = exterior, tnBound = Just intNode}),
                          (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
                        ],
                    cBindParents =
                      IntMap.singleton
                        (nodeRefKey (typeRef intNode))
                        (typeRef exterior, BindFlex)
                  }
            solved = mkSolved c IntMap.empty
            view = presolutionViewFromSolved solved
            identityNodeMap =
              IntMap.fromList
                [ (getNodeId nodeId, nodeId)
                  | nodeId <- [root, exterior, intNode]
                ]
            gaParents =
              GaBindParents
                { gaBindParentsBase = cBindParents c,
                  gaBaseConstraint = c,
                  gaAnnotationNodeRedirects = IntMap.empty,
                  gaBaseToSolved = identityNodeMap,
                  gaSolvedToBase = identityNodeMap,
                  gaRestoredSchemeRootTargets = IntMap.empty,
                  gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
                }
            sourceScheme =
              Elab.schemeInfoFromRefSubst
                (Elab.schemeFromType (TestElab.tBase (BaseTy "Int")))
                IntMap.empty
            traceInfo =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = exterior,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            witness =
              EdgeWitness
                { ewEdgeId = edgeId,
                  ewLeft = root,
                  ewRight = exterior,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpRaiseMerge root exterior]
                }
            edgeArtifacts =
              edgeArtifactsForTest
                (IntMap.singleton edgeKey ExpIdentity)
                (IntMap.singleton edgeKey witness)
                (IntMap.singleton edgeKey traceInfo)
                IntSet.empty
        phi <-
          requireRight
            ( PhiTestSupport.reifyInstWithSourceScheme
                defaultTraceConfig
                (defaultPlanBuilder defaultTraceConfig)
                view
                gaParents
                edgeArtifacts
                sourceScheme
                (annVar "x" root)
                edgeId
            )
        case phi of
          Elab.InstAbstrRef ref ->
            Elab.typeBinderRefNode ref `shouldBe` Just exterior
          other ->
            expectationFailure
              ("expected terminal RaiseMerge InstAbstr, got " ++ show other)

      it "does not transport a scheme when strict replay names only the operated root" $ do
        let root = NodeId 100
            argument = NodeId 101
            replayRoot = NodeId 102
            nestedBinder = NodeId 103
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyVar {tnId = root, tnBound = Just argument}),
                          (getNodeId argument, TyForall {tnId = argument, tnBody = nestedBinder}),
                          (getNodeId nestedBinder, TyVar {tnId = nestedBinder, tnBound = Nothing}),
                          (getNodeId replayRoot, TyVar {tnId = replayRoot, tnBound = Just argument})
                        ],
                    cBindParents =
                      bindParentsFromPairs
                        [ (argument, root, BindFlex),
                          (nestedBinder, argument, BindFlex)
                        ]
                  }
            solved = mkSolved c IntMap.empty
            consumerType =
              testTMu
                "self"
                (testTForall "result" Nothing (testTVar "result"))
            si = Elab.schemeInfoFromRefSubst (Elab.schemeFromType consumerType) IntMap.empty
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = replayRoot,
                  etBinderArgs = [(root, argument)],
                  etInterior = sourceInteriorFromList [root],
                  etBinderReplayMap = IntMap.singleton (getNodeId root) replayRoot,
                  etReplayDomainBinders = [replayRoot],
                  etCopyMap = CopyMapping (IntMap.singleton (getNodeId root) replayRoot),
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 83,
                  ewLeft = root,
                  ewRight = replayRoot,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken root]
                }
        phi <-
          requireRight
            ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                defaultTraceConfig
                (generalizeAtWithActive solved)
                (presolutionViewFromSolved solved)
                Nothing
                (Just si)
                (Just tr)
                ew
            )
        phi `shouldBe` Elab.InstId

      it "treats a root RaiseMerge after a strict root Weaken as rigid identity" $ do
        let root = NodeId 100
            argument = NodeId 101
            replayRoot = NodeId 102
            exterior = NodeId 200
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyVar {tnId = root, tnBound = Just argument}),
                          (getNodeId argument, TestTyBase argument (BaseTy "Int")),
                          (getNodeId replayRoot, TyVar {tnId = replayRoot, tnBound = Just argument}),
                          (getNodeId exterior, TyVar {tnId = exterior, tnBound = Just argument})
                        ],
                    cBindParents = bindParentsFromPairs [(argument, root, BindFlex)]
                  }
            solved = mkSolved c IntMap.empty
            si = Elab.schemeInfoFromRefSubst (Elab.schemeFromType Elab.TBottom) IntMap.empty
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = exterior,
                  etBinderArgs = [(root, argument)],
                  etInterior = sourceInteriorFromList [root],
                  etBinderReplayMap = IntMap.singleton (getNodeId root) replayRoot,
                  etReplayDomainBinders = [replayRoot],
                  etCopyMap = CopyMapping (IntMap.singleton (getNodeId root) replayRoot),
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 84,
                  ewLeft = root,
                  ewRight = exterior,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpWeaken root, OpRaiseMerge root exterior]
                }
        phi <-
          requireRight
            ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                defaultTraceConfig
                (generalizeAtWithActive solved)
                (presolutionViewFromSolved solved)
                Nothing
                (Just si)
                (Just tr)
                ew
            )
        phi `shouldBe` Elab.InstId

      it "preserves a rigid root Weaken when its preceding Graft was already inlined" $ do
        let root = NodeId 100
            argument = NodeId 101
            replayRoot = NodeId 102
            exterior = NodeId 200
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyVar {tnId = root, tnBound = Just argument}),
                          (getNodeId argument, TestTyBase argument (BaseTy "Int")),
                          (getNodeId replayRoot, TyVar {tnId = replayRoot, tnBound = Just argument}),
                          (getNodeId exterior, TyVar {tnId = exterior, tnBound = Just argument})
                        ],
                    cBindParents = bindParentsFromPairs [(argument, root, BindFlex)]
                  }
            solved = mkSolved c IntMap.empty
            si = Elab.schemeInfoFromRefSubst (Elab.schemeFromType Elab.TBottom) IntMap.empty
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = exterior,
                  etBinderArgs = [(root, argument)],
                  etInterior = sourceInteriorFromList [root],
                  etBinderReplayMap = IntMap.singleton (getNodeId root) replayRoot,
                  etReplayDomainBinders = [replayRoot],
                  etCopyMap = CopyMapping (IntMap.singleton (getNodeId root) replayRoot),
                  etReplayContract = ReplayContractStrict
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 85,
                  ewLeft = root,
                  ewRight = exterior,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness =
                    InstanceWitness
                      [ OpGraft argument root,
                        OpWeaken root,
                        OpRaiseMerge root exterior
                      ]
                }
        phi <-
          requireRight
            ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                defaultTraceConfig
                (generalizeAtWithActive solved)
                (presolutionViewFromSolved solved)
                Nothing
                (Just si)
                (Just tr)
                ew
            )
        phi `shouldBe` Elab.InstId

      it "root RaiseMerge rejects a trace that does not prove an exterior operand" $ do
        let root = NodeId 100
            notExterior = NodeId 200
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyVar {tnId = root, tnBound = Nothing}),
                          (getNodeId notExterior, TyVar {tnId = notExterior, tnBound = Nothing})
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTVar "a"))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.singleton (getNodeId root) "a")
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = notExterior,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, notExterior],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 82,
                  ewLeft = root,
                  ewRight = notExterior,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpRaiseMerge root notExterior]
                }
        case
            PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
              defaultTraceConfig
              (generalizeAtWithActive solved)
              (presolutionViewFromSolved solved)
              Nothing
              (Just si)
              (Just tr)
              ew
          of
            Left (Elab.PhiTranslatabilityError messages) ->
              messages
                `shouldSatisfy` any
                  ("root operation lacks exact source-interior trace authority" `isInfixOf`)
            Left err ->
              expectationFailure ("expected root trace rejection, got " ++ show err)
            Right inst ->
              expectationFailure ("expected root trace rejection, got " ++ show inst)

      it "OpMerge with rigid endpoint only on m fails as non-translatable" $ do
        let root = NodeId 100
            n = NodeId 1
            m = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root n m),
                          (getNodeId n, TyVar {tnId = n, tnBound = Nothing}),
                          (getNodeId m, TyVar {tnId = m, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef n), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef m), (genRef (GenNodeId 0), BindRigid))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId n, "a"), (getNodeId m, "b")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, n, m],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpMerge n m]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            msgs `shouldSatisfy` any ("OpMerge: rigid endpoint appears only on non-operated node" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right phi ->
            expectationFailure ("Expected failure, got " ++ Elab.pretty phi)

      it "OpRaiseMerge with rigid endpoint only on m fails as non-translatable" $ do
        let root = NodeId 100
            n = NodeId 1
            m = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root n m),
                          (getNodeId n, TyVar {tnId = n, tnBound = Nothing}),
                          (getNodeId m, TyVar {tnId = m, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef n), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef m), (genRef (GenNodeId 0), BindRigid))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId n, "a"), (getNodeId m, "b")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, n],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpRaiseMerge n m]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) ->
            msgs `shouldSatisfy` any ("OpRaiseMerge: rigid endpoint appears only on non-operated node" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right phi ->
            expectationFailure ("Expected failure, got " ++ Elab.pretty phi)

      it "keeps binder identities in sync after root graft InstApp" $ do
        let root = NodeId 0 -- outer TyForall
            binderA = NodeId 1 -- binder for 'a'
            forallB = NodeId 2 -- inner TyForall
            binderB = NodeId 3 -- binder for 'b'
            bodyNode = NodeId 4 -- arrow node
            intNode = NodeId 5 -- Int type (separate root)
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB),
                  (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty
            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst =
              IntMap.fromList
                [ (getNodeId binderA, "a"),
                  (getNodeId binderB, "b")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst
            -- Root graft uses InstApp (eliminates one ∀); a later binder-indexed
            -- op must see the updated identity spine.
            ops = [OpGraft intNode root, OpRaise binderB]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        phi `shouldNotBe` Elab.InstId
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        Elab.pretty out `shouldSatisfy` ("Int" `isInfixOf`)

      describe "binder-spine safety" $ do
        it "detects quantified-type and identity-spine mismatches before reorder reads binders" $ do
          let ty =
                testTForall "a"
                  Nothing
                  (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))
              ids = [Just (NodeId 1)]
              vs = PhiTestSupport.mkVSpine ty ids
          case PhiTestSupport.assertSpineSync vs ty ids of
            Left (Elab.PhiInvariantError msg) -> do
              msg `shouldSatisfy` ("VSpine desync (names)" `isInfixOf`)
              msg `shouldSatisfy` ("[\"a\",\"b\"]" `isInfixOf`)
            Left err ->
              expectationFailure ("Expected PhiInvariantError, got " ++ show err)
            Right () ->
              expectationFailure "Expected binder-spine mismatch to fail"

        it "reports out-of-range binder reads through PhiInvariantError" $ do
          let ty =
                testTForall "a"
                  Nothing
                  (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b")))
              vs = PhiTestSupport.mkVSpine ty [Just (NodeId 1), Just (NodeId 2)]
          case PhiTestSupport.vSpineNameAt vs 2 of
            Left (Elab.PhiInvariantError msg) -> do
              msg `shouldSatisfy` ("VSpine: binder index 2 out of range" `isInfixOf`)
              msg `shouldSatisfy` ("spine length 2" `isInfixOf`)
            Left err ->
              expectationFailure ("Expected PhiInvariantError, got " ++ show err)
            Right name ->
              expectationFailure ("Expected out-of-range failure, got binder " ++ show name)

        it "preserves valid binder names, bounds, and identities through checked access" $ do
          let boundA = boundFromType (TestElab.tBase (BaseTy "Int"))
              refA =
                ElabTypes.typeBinderRefFromIdentity
                  (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
                  "a"
              refB =
                ElabTypes.typeBinderRefFromIdentity
                  (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
                  "b"
              ty =
                ElabTypes.tForallWithRef
                  refA
                  Nothing
                  (ElabTypes.tForallWithRef refB (Just boundA) (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB)))
              vs = PhiTestSupport.mkVSpine ty [Just (NodeId 1), Just (NodeId 2)]
          PhiTestSupport.vSpineBinderAt vs 1 `shouldBe` Right (refB, Just boundA, Just (NodeId 2))

        it "preserves same-named binder identities in the virtual spine" $ do
          let refA =
                ElabTypes.typeBinderRefFromIdentity
                  (ElabTypes.typeBinderIdentityFromNode (NodeId 1))
                  "a"
              refB =
                ElabTypes.typeBinderRefFromIdentity
                  (ElabTypes.typeBinderIdentityFromNode (NodeId 2))
                  "a"
              ty =
                ElabTypes.tForallWithRef
                  refA
                  Nothing
                  (ElabTypes.tForallWithRef
                    refB
                    Nothing
                    (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB)))
              ids = [Just (NodeId 1), Just (NodeId 2)]
              vs = PhiTestSupport.mkVSpine ty ids
          PhiTestSupport.assertSpineSync vs ty ids `shouldBe` Right ()
          map ElabTypes.typeBinderRefIdentity (PhiTestSupport.vSpineBinderRefs vs)
            `shouldBe` [ElabTypes.typeBinderRefIdentity refA, ElabTypes.typeBinderRefIdentity refB]
          PhiTestSupport.vSpineBinderAt vs 0 `shouldBe` Right (refA, Nothing, Just (NodeId 1))
          PhiTestSupport.vSpineBinderAt vs 1 `shouldBe` Right (refB, Nothing, Just (NodeId 2))

      it "scheme-aware Φ can target a non-front binder (reordering before instantiation)" $ do
        -- Build a constraint graph with proper nested TyForall structure for ∀a. ∀b. a -> b
        let root = NodeId 0 -- outer TyForall
            binderA = NodeId 1 -- binder for 'a'
            forallB = NodeId 2 -- inner TyForall
            binderB = NodeId 3 -- binder for 'b'
            bodyNode = NodeId 4 -- arrow node
            intNode = NodeId 5 -- Int type (separate root)
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB),
                  (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
                ]
            -- Binding tree: binders bound to their respective foralls
            -- forallB and bodyNode are inside root's scope
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty

            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst =
              IntMap.fromList
                [ (getNodeId binderA, "a"),
                  (getNodeId binderB, "b")
                ]
            si = mkSchemeInfoFromNodeNames scheme subst

            -- Witness says: graft Int into binder "b", then weaken it.
            ops = [OpGraft intNode binderB, OpWeaken binderB]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)

        -- Because we target the *second* binder, Φ must do more than a plain ⟨Int⟩.
        phi `shouldNotBe` Elab.InstApp (TestElab.tBase (BaseTy "Int"))

        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall "a"
                Nothing
                (Elab.TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
        canonType out `shouldBe` canonType expected

      it "bounded bound-match graft-weaken emits InstElim (thesis-exact individual ops)" $ do
        let root = NodeId 0
            binder = NodeId 1
            bodyNode = NodeId 2
            bound = NodeId 3
            argInt = NodeId 4
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root bodyNode),
                  (getNodeId binder, TyVar {tnId = binder, tnBound = Just bound}),
                  (getNodeId bodyNode, TyArrow bodyNode binder argInt),
                  (getNodeId bound, TestTyBase bound (BaseTy "Int")),
                  (getNodeId argInt, TestTyBase argInt (BaseTy "Int"))
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binder), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef bound), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef argInt), (typeRef bodyNode, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty
            scheme =
              Elab.schemeFromType
                (testTForall "a" (Just (TestElab.tBase (BaseTy "Int"))) (testTVar "a"))
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binder, "a")])
            ops = [OpGraft argInt binder, OpWeaken binder]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        phi `shouldBe` Elab.InstElim

      it "translates non-root graft-raise-weaken and preserves expected instantiated type" $ do
        let root = NodeId 0
            binderA = NodeId 1
            forallB = NodeId 2
            binderB = NodeId 3
            bodyNode = NodeId 4
            intNode = NodeId 5
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB),
                  (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty
            refA = graphTypeBinderRef (getNodeId binderA) "a"
            refB = graphTypeBinderRef (getNodeId binderB) "b"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(refA, Nothing), (refB, Nothing)]
                (Elab.TArrow (ElabTypes.tVarWithRef refA) (ElabTypes.tVarWithRef refB))
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [ (getNodeId binderA, "a"),
                        (getNodeId binderB, "b")
                      ])
            ewGW =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft intNode binderB, OpWeaken binderB]
                }
            ewGRW =
              EdgeWitness
                { ewEdgeId = EdgeId 1,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft intNode binderB, OpRaise binderB, OpWeaken binderB]
                }
        phiGRW <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ewGRW)
        _phiGW <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ewGW)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phiGRW)
        let expected =
              testTForall "a"
                Nothing
                (Elab.TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
        canonType out `shouldBe` canonType expected

      it "non-root graft-weaken with a bottom argument does not collapse codomain to bottom" $ do
        let root = NodeId 0
            binderA = NodeId 1
            forallB = NodeId 2
            binderB = NodeId 3
            bodyNode = NodeId 4
            botNode = NodeId 5
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB),
                  (getNodeId botNode, TyBottom botNode)
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        (Elab.TArrow (testTVar "a") (testTVar "b"))
                    )
                )
            si =
              mkSchemeInfoFromNodeNames scheme (IntMap.fromList [ (getNodeId binderA, "a"),
                        (getNodeId binderB, "b")
                      ])
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft botNode binderB, OpWeaken binderB]
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        Elab.pretty out `shouldSatisfy` ("-> b" `isInfixOf`)

      it "O15-TR-SEQ-CONS: counts forall intros in Φ translation" $ do
        let root = NodeId 0
            binder = NodeId 1
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root binder),
                  (getNodeId binder, TyVar {tnId = binder, tnBound = Nothing})
                ]
            bindParents =
              IntMap.fromList
                [(nodeRefKey (typeRef binder), (typeRef root, BindFlex))]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty

            refA = graphTypeBinderRef (getNodeId binder) "a"
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [(refA, Nothing)]
                (ElabTypes.tVarWithRef refA)
            subst = IntMap.fromList [(getNodeId binder, "a")]
            si = mkSchemeInfoFromNodeNames scheme subst

            ops = [OpWeaken binder]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 1,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        Elab.pretty phi `shouldBe` "O; ∀(u0 ⩾) N"

      it "scheme-aware Φ can translate Merge (alias one binder to another)" $ do
        -- Build a constraint graph with proper nested TyForall structure for ∀a. ∀b. a -> b
        let root = NodeId 0 -- outer TyForall
            binderA = NodeId 1 -- binder for 'a'
            forallB = NodeId 2 -- inner TyForall
            binderB = NodeId 3 -- binder for 'b'
            bodyNode = NodeId 4 -- arrow node
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB)
                ]
            -- Binding tree: binders bound to their respective foralls
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty

            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst = IntMap.fromList [(getNodeId binderA, "a"), (getNodeId binderB, "b")]
            si = mkSchemeInfoFromNodeNames scheme subst

            -- Merge binder "b" into binder "a", i.e. ∀a. ∀b. a -> b  ~~>  ∀a. a -> a
            ops = [OpMerge binderB binderA]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall "a"
                Nothing
                (Elab.TArrow (testTVar "a") (testTVar "a"))
        canonType out `shouldBe` canonType expected

      it "scheme-aware Φ can translate RaiseMerge (alias one binder to another)" $ do
        -- Build a constraint graph with proper nested TyForall structure for ∀a. ∀b. a -> b
        let root = NodeId 0 -- outer TyForall
            binderA = NodeId 1 -- binder for 'a'
            forallB = NodeId 2 -- inner TyForall
            binderB = NodeId 3 -- binder for 'b'
            bodyNode = NodeId 4 -- arrow node
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB)
                ]
            -- Binding tree: binders bound to their respective foralls
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef binderA), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef forallB), (typeRef root, BindFlex)),
                  (nodeRefKey (typeRef binderB), (typeRef forallB, BindFlex)),
                  (nodeRefKey (typeRef bodyNode), (typeRef forallB, BindFlex))
                ]
            constraint =
              rootedConstraint
                emptyConstraint
                  { cNodes = nodes,
                    cBindParents = bindParents
                  }
            solved = mkSolved constraint IntMap.empty

            scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst = IntMap.fromList [(getNodeId binderA, "a"), (getNodeId binderB, "b")]
            si = mkSchemeInfoFromNodeNames scheme subst

            ops = [OpRaiseMerge binderB binderA]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall "a"
                Nothing
                (Elab.TArrow (testTVar "a") (testTVar "a"))
        canonType out `shouldBe` canonType expected

      it "scheme-aware Φ can translate Raise (raise a binder to the front)" $ do
        let scheme =
              Elab.schemeFromType
                (testTForall "a" Nothing (testTForall "b" Nothing (Elab.TArrow (testTVar "a") (testTVar "b"))))
            subst = IntMap.fromList [(1, "a"), (2, "b")]
            si = mkSchemeInfoFromNodeNames scheme subst
            root = NodeId 100
            aN = NodeId 1
            bN = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cEliminatedVars = IntSet.empty,
                    cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root aN bN),
                          (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                          (getNodeId bN, TyVar {tnId = bN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

            -- Raise binder “b” outward by introducing a fresh front binder and
            -- aliasing/eliminating the old one (paper Fig. 10 Raise).
            ops = [OpRaise (NodeId 2)]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
        let expected =
              testTForall "u0"
                Nothing
                ( testTForall "a"
                    Nothing
                    (Elab.TArrow (testTVar "a") (testTVar "u0"))
                )
        canonType out `shouldBe` canonType expected

      it "scheme-aware Φ places Raise after bound dependencies (well-scoped bound)" $ do
        let scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        ( testTForall "c"
                            (Just (boundFromType (Elab.TArrow (testTVar "a") (testTVar "a"))))
                            (Elab.TArrow (testTVar "a") (Elab.TArrow (testTVar "c") (testTVar "b")))
                        )
                    )
                )
            subst = IntMap.fromList [(1, "a"), (2, "b"), (3, "c")]
            si = mkSchemeInfoFromNodeNames scheme subst
            root = NodeId 100
            aN = NodeId 1
            bN = NodeId 2
            cN = NodeId 3
            inner = NodeId 101
            c =
              rootedConstraint
                emptyConstraint
                  { cEliminatedVars = IntSet.empty,
                    cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root aN inner),
                          (getNodeId inner, TyArrow inner cN bN),
                          (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                          (getNodeId bN, TyVar {tnId = bN, tnBound = Nothing}),
                          (getNodeId cN, TyVar {tnId = cN, tnBound = Just aN})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef cN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef inner), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

            ops = [OpRaise (NodeId 3)]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (phiFromEdgeWitnessFixtureTrace solved (Just si) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

        let expected =
              testTForall "a"
                Nothing
                ( testTForall "u0"
                    (Just (boundFromType (Elab.TArrow (testTVar "a") (testTVar "a"))))
                    ( testTForall "b"
                        Nothing
                        (Elab.TArrow (testTVar "a") (Elab.TArrow (testTVar "u0") (testTVar "b")))
                    )
                )
        canonType out `shouldBe` canonType expected

      it "Φ uses per-edge ≺ (via EdgeTrace) to order binders before placing Raise" $ do
        let root = NodeId 100
            aN = NodeId 1
            bN = NodeId 2
            cN = NodeId 3
            inner = NodeId 101

            c =
              rootedConstraint
                emptyConstraint
                  { cEliminatedVars = IntSet.empty,
                    cNodes =
                      nodeMapFromList
                        [ (100, TyArrow root bN inner),
                          (getNodeId inner, TyArrow inner cN aN),
                          (1, TyVar {tnId = aN, tnBound = Nothing}),
                          (2, TyVar {tnId = bN, tnBound = Nothing}),
                          (3, TyVar {tnId = cN, tnBound = Just bN})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef cN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef inner), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        ( testTForall "c"
                            (Just (boundFromType (Elab.TArrow (testTVar "b") (testTVar "b"))))
                            (Elab.TArrow (testTVar "b") (Elab.TArrow (testTVar "c") (testTVar "a")))
                        )
                    )
                )
            subst = IntMap.fromList [(1, "a"), (2, "b"), (3, "c")]
            si = mkSchemeInfoFromNodeNames scheme subst

            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = mempty,
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }

            ops = [OpRaise cN]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = NodeId 0,
                  ewRight = NodeId 0,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

        let expected =
              testTForall "b"
                Nothing
                ( testTForall "u0"
                    (Just (boundFromType (Elab.TArrow (testTVar "b") (testTVar "b"))))
                    ( testTForall "a"
                        Nothing
                        (Elab.TArrow (testTVar "b") (Elab.TArrow (testTVar "u0") (testTVar "a")))
                    )
                )
        canonType out `shouldBe` canonType expected

      it "canonicalizes a frozen source-interior alias only at Phi named-set adoption" $ do
        let root = NodeId 100
            aN = NodeId 1
            bN = NodeId 2
            cN = NodeId 3
            aliasN = NodeId 30
            inner = NodeId 101
            c =
              rootedConstraint
                emptyConstraint
                  { cEliminatedVars = IntSet.empty,
                    cNodes =
                      nodeMapFromList
                        [ (100, TyArrow root bN inner),
                          (getNodeId inner, TyArrow inner cN aN),
                          (1, TyVar {tnId = aN, tnBound = Nothing}),
                          (2, TyVar {tnId = bN, tnBound = Nothing}),
                          (3, TyVar {tnId = cN, tnBound = Just bN})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef cN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef inner), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c (IntMap.singleton (getNodeId aliasN) cN)
            view = presolutionViewFromSolved solved
            scheme =
              Elab.schemeFromType
                ( testTForall "a"
                    Nothing
                    ( testTForall "b"
                        Nothing
                        ( testTForall "c"
                            (Just (boundFromType (Elab.TArrow (testTVar "b") (testTVar "b"))))
                            (Elab.TArrow (testTVar "b") (Elab.TArrow (testTVar "c") (testTVar "a")))
                        )
                    )
                )
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(1, "a"), (2, "b"), (3, "c")])
            traceFor sourceInteriorNode =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [sourceInteriorNode],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            witnessFor raiseTarget =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = NodeId 0,
                  ewRight = NodeId 0,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpRaise raiseTarget]
                }
            translate sourceInteriorNode raiseTarget =
              PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                defaultTraceConfig
                (generalizeAtWithActive solved)
                view
                Nothing
                (Just si)
                (Just (traceFor sourceInteriorNode))
                (witnessFor raiseTarget)

        readModel <- requireRight (buildElabReadModel view)
        ermNamedNodes readModel `shouldSatisfy` IntSet.member (getNodeId cN)
        ermNamedNodes readModel `shouldSatisfy` not . IntSet.member (getNodeId aliasN)

        canonicalPhi <- requireRight (translate cN cN)
        aliasPhi <- requireRight (translate aliasN aliasN)
        aliasPhi `shouldBe` canonicalPhi
        aliasPhi `shouldNotBe` Elab.InstId

        canonicalOut <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) canonicalPhi)
        aliasOut <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) aliasPhi)
        canonType aliasOut `shouldBe` canonType canonicalOut

      it "O15-EDGE-TRANSLATION: witness instantiation matches solved edge types (id @ Int)" $ do
        let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
        case runToSolved expr of
          Left err -> expectationFailure err
          Right (solved, ews, traces) -> do
            IntMap.size ews `shouldSatisfy` (> 0)
            forM_ (IntMap.elems ews) $ \ew -> do
              let EdgeId eid = ewEdgeId ew
                  mTrace = IntMap.lookup eid traces
                  canonical = Solved.canonical solved
                  skipNoReplayNoop =
                    case mTrace of
                      Just tr ->
                        etReplayContract tr == ReplayContractNone
                          && null (getInstanceOps (ewWitness ew))
                      Nothing ->
                        False
              let scopeRootFor nid = do
                    path <- Binding.bindingPathToRoot (Solved.originalConstraint solved) (typeRef (canonical nid))
                    case drop 1 path of
                      [] -> Right (typeRef (canonical nid))
                      rest ->
                        case [gid | GenRef gid <- rest] of
                          (gid : _) -> Right (genRef gid)
                          [] -> Right (typeRef (canonical nid))
              unless skipNoReplayNoop $ do
                srcScope <- requireRight (scopeRootFor (ewRoot ew))
                tgtScope <- requireRight (scopeRootFor (ewRight ew))
                (srcSch, _) <- requireRight (generalizeAt solved srcScope (ewRoot ew))
                (tgtSch, _) <- requireRight (generalizeAt solved tgtScope (ewRight ew))
                let srcTy = Elab.schemeToType srcSch
                    tgtTy = Elab.schemeToType tgtSch
                phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing Nothing mTrace ew)
                out <- requireRight (Elab.applyInstantiation srcTy phi)
                canonType (stripBoundWrapper out) `shouldBe` canonType (stripBoundWrapper tgtTy)

      it "witness instantiation matches solved edge types (two instantiations)" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (EVar "x"))
                ( ELet
                    "_"
                    (EApp (EVar "f") (ELit (LInt 1)))
                    (EApp (EVar "f") (ELit (LBool True)))
                )
        artifacts@PipelineArtifacts {paPresolution = pres, paSolved = solved} <-
          requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs, annCanon, _annPre) = resultTypeInputsForArtifacts artifacts
            view = rtcPresolutionView inputs
            gaParents = rtcBindParentsGa inputs
            edgeArtifacts = rtcEdgeArtifacts inputs
            canonical = Solved.canonical solved
            scopeRootFor nid = do
              path <- Binding.bindingPathToRoot (Solved.originalConstraint solved) (typeRef (canonical nid))
              case drop 1 path of
                [] -> Right (typeRef (canonical nid))
                rest ->
                  case [gid | GenRef gid <- rest] of
                    (gid : _) -> Right (genRef gid)
                    [] -> Right (typeRef (canonical nid))
        (fDetails, bodyAnn) <-
          case annCanon of
            ALet _ details _ _ _ _ _ body _ -> pure (details, body)
            other -> do
              expectationFailure ("expected the outer f binding, got " ++ show other)
              fail "missing f binding"
        let occurrences = functionOccurrencesFor fDetails bodyAnn
        length occurrences `shouldBe` 2
        -- Figure 15.3.5 elaborates an occurrence with
        -- [phi_R(a); T(e)].  Raw T(e) alone is only the edge witness of
        -- Definition 15.3.12 and need not include the occurrence expansion.
        occurrenceTypes <- mapM (\(funAnn, funSite) -> do
          let edgeId@(EdgeId edgeKey) = instantiationSiteEdgeId funSite
          ew <-
            case IntMap.lookup edgeKey (eaEdgeWitnesses edgeArtifacts) of
              Just witness -> pure witness
              Nothing -> do
                expectationFailure ("missing function occurrence witness for " ++ show edgeId)
                fail "missing occurrence witness"
          srcScope <- requireRight (scopeRootFor (ewRoot ew))
          (srcSch, srcSubst) <-
            requireRight
              ( generalizeAtWithActive
                  solved
                  (Just gaParents)
                  srcScope
                  (ewRoot ew)
              )
          let sourceSchemeInfo = Elab.schemeInfoFromRefSubst srcSch srcSubst
              srcTy = Elab.schemeToType srcSch
          occurrenceInst <-
            requireRight
              ( PhiTestSupport.reifyInstWithSourceScheme
                  defaultTraceConfig
                  (prPlanBuilder pres)
                  view
                  gaParents
                  edgeArtifacts
                  sourceSchemeInfo
                  funAnn
                  edgeId
              )
          out <- requireRight (Elab.applyInstantiation srcTy occurrenceInst)
          pure (stripBoundWrapper out)) occurrences
        let intTy = TestElab.tBase (BaseTy "Int")
            boolTy = TestElab.tBase (BaseTy "Bool")
            expectedOccurrenceTypes =
              [ Elab.TArrow intTy intTy,
                Elab.TArrow boolTy boolTy
              ]
        map canonType occurrenceTypes `shouldBe` map canonType expectedOccurrenceTypes

      it "witness normalization preserves OpRaiseMerge coalescing end-to-end (US-010)" $ do
        -- Verify that the full presolution pipeline still produces valid
        -- normalized witnesses after the structural RaiseMerge gating
        -- refactor (US-007 through US-009).
        let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
            checkNoDupRaises [] = pure ()
            checkNoDupRaises [_] = pure ()
            checkNoDupRaises (OpRaise n : rest@(OpRaise m : _))
              | n == m = expectationFailure ("Consecutive duplicate OpRaise on " ++ show n)
              | otherwise = checkNoDupRaises rest
            checkNoDupRaises (_ : rest) = checkNoDupRaises rest
        case runToSolved expr of
          Left err -> expectationFailure err
          Right (solved, ews, traces) -> do
            IntMap.size ews `shouldSatisfy` (> 0)
            forM_ (IntMap.elems ews) $ \ew -> do
              let ops = getInstanceOps (ewWitness ew)
              checkNoDupRaises ops
              let EdgeId eid = ewEdgeId ew
                  mTrace = IntMap.lookup eid traces
              case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing Nothing mTrace ew of
                Left err -> expectationFailure ("Expected successful Phi translation, got: " ++ show err)
                Right _ -> pure ()

      it "rejects OpGraft on out-of-scheme target (no non-binder recovery)" $ do
        let root = NodeId 100
            body = NodeId 101
            binderN = NodeId 1
            nonBinderN = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body binderN nonBinderN),
                          (getNodeId binderN, TyVar {tnId = binderN, tnBound = Nothing}),
                          (getNodeId nonBinderN, TyVar {tnId = nonBinderN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef nonBinderN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTVar "a"))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderN, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [binderN, nonBinderN],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpGraft binderN nonBinderN, OpWeaken nonBinderN]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = nonBinderN,
                  ewRight = nonBinderN,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) -> do
            let rendered = unlines msgs
            rendered `shouldSatisfy` ("OpGraft targets non-binder node" `isInfixOf`)
            rendered `shouldNotSatisfy` ("InstBot expects" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected non-binder rejection, got inst: " ++ show inst)

      it "rejects OpGraft on out-of-scheme target (no InstBot/InstApp fallback)" $ do
        let root = NodeId 200
            body = NodeId 201
            binderN = NodeId 11
            nonBinderN = NodeId 12
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body binderN nonBinderN),
                          (getNodeId binderN, TyVar {tnId = binderN, tnBound = Nothing}),
                          (getNodeId nonBinderN, TyVar {tnId = nonBinderN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef nonBinderN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTVar "a"))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderN, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [binderN, nonBinderN],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }
            ops = [OpGraft binderN nonBinderN]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = nonBinderN,
                  ewRight = nonBinderN,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) -> do
            let rendered = unlines msgs
            rendered `shouldSatisfy` ("OpGraft targets non-binder node" `isInfixOf`)
            rendered `shouldNotSatisfy` ("InstBot expects" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected non-binder rejection, got inst: " ++ show inst)

      it "producer-trace OpGraft on non-binder still fails fast (no copy-map skip fallback)" $ do
        let root = NodeId 300
            body = NodeId 301
            binderN = NodeId 11
            nonBinderN = NodeId 12
            argNode = NodeId 13
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body binderN binderN),
                          (getNodeId binderN, TyVar {tnId = binderN, tnBound = Nothing}),
                          (getNodeId nonBinderN, TestTyBase nonBinderN (BaseTy "Bool")),
                          (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef binderN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
            scheme = Elab.schemeFromType (testTForall "a" Nothing (testTVar "a"))
            si = mkSchemeInfoFromNodeNames scheme (IntMap.fromList [(getNodeId binderN, "a")])
            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, body, binderN, nonBinderN, argNode],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = insertCopy (NodeId 999) nonBinderN mempty,
                  etReplayContract = ReplayContractNone
                }
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = root,
                  ewRight = root,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness [OpGraft argNode nonBinderN]
                }
        case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew of
          Left (Elab.PhiTranslatabilityError msgs) -> do
            let rendered = unlines msgs
            rendered `shouldSatisfy` ("OpGraft targets non-binder node" `isInfixOf`)
          Left err ->
            expectationFailure ("Expected PhiTranslatabilityError, got " ++ show err)
          Right inst ->
            expectationFailure ("Expected fail-fast producer-trace OpGraft, got inst: " ++ show inst)

      it "O15-CONTEXT-FIND: contextToNodeBound computes inside-bound contexts (context)" $ do
        -- root binds a and b; b's bound contains binder c.
        -- Context to reach c must go under a, then inside b's bound.
        let root = NodeId 100
            body = NodeId 101
            aN = NodeId 1
            bN = NodeId 2
            cN = NodeId 3

            c =
              rootedConstraint
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
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef bN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef cN), (typeRef bN, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

        steps <- requireRight (Elab.contextToNodeBound (presolutionViewFromSolved solved) root cN)
        steps `shouldBe` Just [Elab.StepUnderRef (ElabTypes.typeBinderRefFromIdentity (ElabTypes.typeBinderIdentityFromNode aN) "t1"), Elab.StepInside]

      it "contextToNodeBound computes under-quantifier contexts (context)" $ do
        -- Same graph as above: binder b is after a at the root.
        let root = NodeId 100
            body = NodeId 101
            aN = NodeId 1
            bN = NodeId 2
            cN = NodeId 3

            c =
              rootedConstraint
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
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef bN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef cN), (typeRef bN, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

        steps <- requireRight (Elab.contextToNodeBound (presolutionViewFromSolved solved) root bN)
        steps `shouldBe` Just [Elab.StepUnderRef (ElabTypes.typeBinderRefFromIdentity (ElabTypes.typeBinderIdentityFromNode aN) "t1")]

      it "contextToNodeBound handles shared bound subgraphs (context dag)" $ do
        -- The bound of b is the same node that also appears in the body.
        -- The context should still be computed without treating sharing as a cycle.
        let root = NodeId 100
            body = NodeId 101
            bN = NodeId 2
            shared = NodeId 200
            xN = NodeId 3

            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body bN shared),
                          (getNodeId bN, TyVar {tnId = bN, tnBound = Just shared}),
                          (getNodeId shared, TyArrow shared xN xN),
                          (getNodeId xN, TyVar {tnId = xN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef bN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef body), (typeRef root, BindRigid)),
                          (nodeRefKey (typeRef shared), (typeRef bN, BindFlex)),
                          (nodeRefKey (typeRef xN), (typeRef shared, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

        steps <- requireRight (Elab.contextToNodeBound (presolutionViewFromSolved solved) root xN)
        steps `shouldBe` Just [Elab.StepInside]

      it "contextToNodeBound ignores non-variable binder bounds (context non-var)" $ do
        -- Binder b is an arrow node; non-variable bounds are ignored.
        let root = NodeId 100
            body = NodeId 101
            bN = NodeId 2
            domN = NodeId 3
            codN = NodeId 4

            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body bN bN),
                          (getNodeId bN, TyArrow bN domN codN),
                          (getNodeId domN, TyVar {tnId = domN, tnBound = Nothing}),
                          (getNodeId codN, TyVar {tnId = codN, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef body), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef bN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef domN), (typeRef bN, BindFlex)),
                          (nodeRefKey (typeRef codN), (typeRef bN, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

        steps <- requireRight (Elab.contextToNodeBound (presolutionViewFromSolved solved) root domN)
        steps `shouldBe` Nothing

      it "O15-CONTEXT-REJECT: contextToNodeBound does not descend through forall body fallback" $ do
        let root = NodeId 100
            body = NodeId 101
            aN = NodeId 1
            bodyOnly = NodeId 2
            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyForall root body),
                          (getNodeId body, TyArrow body aN bodyOnly),
                          (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                          (getNodeId bodyOnly, TyVar {tnId = bodyOnly, tnBound = Nothing})
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef body), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef aN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef bodyOnly), (typeRef body, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty
        steps <- requireRight (Elab.contextToNodeBound (presolutionViewFromSolved solved) root bodyOnly)
        steps `shouldBe` Nothing

      it "rejects fallback-dependent binders (gen fallback invariant)" $ do
        let rootGen = GenNodeId 0
            root = NodeId 100
            aN = NodeId 1
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root aN),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing})
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef aN), (genRef rootGen, BindFlex))
                ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cBindParents = bindParents,
                  cGenNodes = fromListGen [(rootGen, GenNode rootGen [root])]
                }

        case Binding.checkNoGenFallback constraint of
          Left GenFallbackRequired {fallbackBinder, fallbackGen, fallbackBinders} -> do
            fallbackBinder `shouldBe` root
            fallbackGen `shouldBe` rootGen
            fallbackBinders `shouldBe` [aN]
          Left err ->
            expectationFailure ("Expected GenFallbackRequired, got " ++ show err)
          Right () ->
            expectationFailure "Expected GenFallbackRequired, got success"

      it "accepts a vacuous forall whose body forall owns the live binders" $ do
        let rootGen = GenNodeId 0
            outerForall = NodeId 100
            innerForall = NodeId 101
            innerBody = NodeId 102
            innerBinder = NodeId 1
            outerBinder = NodeId 2
            nodes =
              nodeMapFromList
                [ (getNodeId outerForall, TyForall outerForall innerForall),
                  (getNodeId innerForall, TyForall innerForall innerBody),
                  (getNodeId innerBody, TyArrow innerBody innerBinder outerBinder),
                  (getNodeId innerBinder, TyVar {tnId = innerBinder, tnBound = Nothing}),
                  (getNodeId outerBinder, TyVar {tnId = outerBinder, tnBound = Nothing})
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef outerForall), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef innerForall), (typeRef outerForall, BindFlex)),
                  (nodeRefKey (typeRef innerBody), (typeRef innerForall, BindFlex)),
                  (nodeRefKey (typeRef innerBinder), (typeRef innerForall, BindFlex)),
                  (nodeRefKey (typeRef outerBinder), (genRef rootGen, BindFlex))
                ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cBindParents = bindParents,
                  cGenNodes = fromListGen [(rootGen, GenNode rootGen [outerForall])]
                }

        Binding.checkNoGenFallback constraint `shouldBe` Right ()

      it "Q(g) returns direct flex children of gen node (positive)" $ do
        -- Gen node owns schemeRoot (TyForall) and aN (TyVar).
        -- schemeRoot owns bN and cN (TyVar).
        -- Q(g) = [aN], Q(schemeRoot) = [bN, cN].
        -- checkNoGenFallback passes because schemeRoot has direct binders.
        let rootGen = GenNodeId 0
            schemeRoot = NodeId 100
            body = NodeId 101
            aN = NodeId 1
            bN = NodeId 2
            cN = NodeId 3
            nodes =
              nodeMapFromList
                [ (getNodeId schemeRoot, TyForall schemeRoot body),
                  (getNodeId body, TyArrow body bN cN),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bN, TyVar {tnId = bN, tnBound = Nothing}),
                  (getNodeId cN, TyVar {tnId = cN, tnBound = Nothing})
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef aN), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef bN), (typeRef schemeRoot, BindFlex)),
                  (nodeRefKey (typeRef cN), (typeRef schemeRoot, BindFlex))
                ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cBindParents = bindParents,
                  cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
                }

        -- Q(g): direct flex TyVar children of gen node
        genBinders <- requireRight (Binding.boundFlexChildren constraint (genRef rootGen))
        genBinders `shouldBe` [aN]

        -- Q(n): direct flex TyVar children of schemeRoot
        forallBinders <- requireRight (Binding.boundFlexChildren constraint (typeRef schemeRoot))
        forallBinders `shouldBe` [bN, cN]

        -- checkNoGenFallback passes (schemeRoot has direct binders)
        Binding.checkNoGenFallback constraint `shouldBe` Right ()

      it "rejects schemes that reach named nodes outside their gen scope" $ do
        let rootGen = GenNodeId 0
            innerGen = GenNodeId 1
            root = NodeId 100
            vN = NodeId 1
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root vN),
                  (getNodeId vN, TyVar {tnId = vN, tnBound = Nothing})
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef vN), (genRef innerGen, BindFlex)),
                  (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cBindParents = bindParents,
                  cGenNodes =
                    fromListGen
                      [ (rootGen, GenNode rootGen [root]),
                        (innerGen, GenNode innerGen [])
                      ]
                }

        case Binding.checkSchemeClosure constraint of
          Left GenSchemeFreeVars {schemeRoot, schemeGen, freeNodes} -> do
            schemeRoot `shouldBe` root
            schemeGen `shouldBe` rootGen
            freeNodes `shouldBe` [vN]
          Left err ->
            expectationFailure ("Expected GenSchemeFreeVars, got " ++ show err)
          Right () ->
            expectationFailure "Expected GenSchemeFreeVars, got success"

      it "accepts higher-rank binders reached only through a rigid alias bound" $ do
        let rootGen = GenNodeId 0
            higherRankGen = GenNodeId 1
            root = NodeId 100
            alias = NodeId 1
            binder = NodeId 2
            result = NodeId 3
            nodes =
              nodeMapFromList
                [ (getNodeId root, TyArrow root alias result),
                  (getNodeId alias, TyVar {tnId = alias, tnBound = Just binder}),
                  (getNodeId binder, TyVar {tnId = binder, tnBound = Nothing}),
                  (getNodeId result, TestTyBase result (BaseTy "Int"))
                ]
            bindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef alias), (genRef higherRankGen, BindRigid)),
                  (nodeRefKey (typeRef binder), (genRef higherRankGen, BindFlex)),
                  (nodeRefKey (typeRef result), (typeRef root, BindFlex)),
                  (nodeRefKey (genRef higherRankGen), (genRef rootGen, BindFlex))
                ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cBindParents = bindParents,
                  cGenNodes =
                    fromListGen
                      [ (rootGen, GenNode rootGen [root]),
                        (higherRankGen, GenNode higherRankGen [])
                      ]
                }

        Binding.checkSchemeClosure constraint `shouldBe` Right ()

      it "selectMinPrecInsertionIndex implements m = min≺ selection (min≺)" $ do
        -- Keys are ordered by <P (lexicographic with empty path greatest).
        -- We craft: key(1) ≺ key(n) ≺ key(2) ≺ key(3).
        let k path = Order.OrderKey {Order.okDepth = length path, Order.okPath = path}
            keys =
              IntMap.fromList
                [ (1, k [0]),
                  (2, k [2]),
                  (3, k [3]),
                  (10, k [1])
                ]
            ids = [Just (NodeId 1), Just (NodeId 2), Just (NodeId 3)]
            nN = NodeId 10
            canonical = id

        Elab.selectMinPrecInsertionIndex 0 keys canonical nN ids `shouldBe` 1
        Elab.selectMinPrecInsertionIndex 2 keys canonical nN ids `shouldBe` 2

      -- Regression test for non-spine OpRaise (paper Fig. 10)
      -- Requirements: 6.1, 6.2, 6.3, 7.3
      it "Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine)" $ do
        -- This models a Raise(n) where n is a flex node bound under m, and m is
        -- bound under the edge root. In the type, n's quantifier appears *inside*
        -- m's bound (non-spine). Raise(n) should:
        --   1) insert a fresh quantifier at the root level (before m), and
        --   2) alias/eliminate the original nested quantifier for n inside m's bound.
        let root = NodeId 100
            aN = NodeId 1
            mN = NodeId 2
            nN = NodeId 3

            c =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId root, TyArrow root aN mN),
                          (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                          (getNodeId mN, TyArrow mN nN aN),
                          (getNodeId nN, TyArrow nN aN aN)
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex)),
                          (nodeRefKey (typeRef mN), (typeRef root, BindFlex)),
                          (nodeRefKey (typeRef nN), (typeRef mN, BindFlex))
                        ]
                  }
            solved = mkSolved c IntMap.empty

            aRef = graphTypeBinderRef (getNodeId aN) "a"
            mRef = graphTypeBinderRef (getNodeId mN) "m"
            cRef = graphTypeBinderRef (getNodeId nN) "c"
            nTy = Elab.TArrow (ElabTypes.tVarWithRef aRef) (ElabTypes.tVarWithRef aRef)
            scheme =
              ElabTypes.mkElabSchemeWithRefs
                [ (aRef, Nothing),
                  ( mRef,
                    Just
                      ( boundFromType
                          (ElabTypes.tForallWithRef cRef (Just (boundFromType nTy)) (ElabTypes.tVarWithRef cRef))
                      )
                  )
                ]
                (ElabTypes.tVarWithRef mRef)
            subst = IntMap.fromList [(getNodeId aN, aRef), (getNodeId mN, mRef)]
            si = Elab.schemeInfoFromRefSubst scheme subst

            tr =
              EdgeTrace
                { etRoot = root,
                  etResultRoot = root,
                  etBinderArgs = [],
                  etInterior = sourceInteriorFromList [root, aN, mN, nN],
                  etBinderReplayMap = mempty,
                  etReplayDomainBinders = [],
                  etCopyMap = mempty,
                  etReplayContract = ReplayContractNone
                }

            ops = [OpRaise nN]
            ew =
              EdgeWitness
                { ewEdgeId = EdgeId 0,
                  ewLeft = NodeId 0,
                  ewRight = NodeId 0,
                  ewRoot = root,
                  ewForallIntros = 0,
                  ewWitness = InstanceWitness ops
                }

        phi <- requireRight (PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig (generalizeAtWithActive solved) (presolutionViewFromSolved solved) Nothing (Just si) (Just tr) ew)
        out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

        let expected =
              ElabTypes.tForallWithRef
                aRef
                Nothing
                ( testTForall "u0"
                    (Just (boundFromType nTy))
                    ( ElabTypes.tForallWithRef
                        mRef
                        (Just (boundFromType nTy))
                        (ElabTypes.tVarWithRef mRef)
                    )
                )
        out `shouldAlphaEqType` expected

  describe "Presolution witness ops (paper alignment)" $ do
    it "presolves bounded aliasing (b ⩾ a) through coercion-only annotations" $ do
      -- Note: With coercion-only annotations, this test's behavior changes.
      -- Previously, the let-binding with EAnn RHS was treated as a declared scheme.
      -- Now it's treated as a normal let with a coercion term.
      -- This test is kept to verify the coercion path still works correctly.
      let rhs = ELam "x" (ELam "y" (EVar "x"))
          schemeTy =
            mkForalls
              [ ("a", Nothing),
                ("b", Just (STVar "a"))
              ]
              (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
          ann =
            STForall
              "a"
              Nothing
              (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
          expr =
            ELet "c" (EAnn rhs schemeTy) (EAnn (EVar "c") ann)

      let runToPresolutionWitnesses :: SurfaceExpr -> Either String (IntMap.IntMap EdgeWitness)
          runToPresolutionWitnesses e = do
            pres <- runToPresolutionDefault Set.empty e
            pure (prEdgeWitnesses pres)

      _ <- requireRight (runToPresolutionWitnesses expr)
      pure ()

  describe "Paper alignment baselines" $ do
    it "let id = (\\x. x) in id id should have type ∀a. a -> a" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              (EApp (EVar "id") (EVar "id"))
      (_term, ty) <- requirePipeline expr
      let expected =
            testTForall "a"
              Nothing
              (Elab.TArrow (testTVar "a") (testTVar "a"))
      ty `shouldAlphaEqType` expected

    it "let-use sites are redirected for polymorphic instantiation" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              (EApp (EVar "id") (EVar "id"))
      (pres, ann) <- requireRight (runToPresolutionWithAnnDefault Set.empty expr)
      let redirects = prRedirects pres
          varNodes = collectVarNodes "id" ann
          redirected =
            [ nid
              | nid <- varNodes,
                Elab.chaseRedirects redirects nid /= nid
            ]
      varNodes `shouldSatisfy` (not . null)
      redirected `shouldSatisfy` (not . null)

    it "generalizeAt inlines rigid vars via bounds at top-level" $ do
      let rootGen = GenNodeId 0
          arrow = NodeId 1
          rigidVar = NodeId 2
          flexVar = NodeId 3
          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId arrow, TyArrow arrow rigidVar rigidVar),
                        (getNodeId rigidVar, TyVar {tnId = rigidVar, tnBound = Just flexVar}),
                        (getNodeId flexVar, TyVar {tnId = flexVar, tnBound = Nothing})
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef arrow), (genRef rootGen, BindRigid)),
                        (nodeRefKey (typeRef rigidVar), (typeRef arrow, BindRigid)),
                        (nodeRefKey (typeRef flexVar), (genRef rootGen, BindFlex))
                      ]
                }
          solved = mkSolved c IntMap.empty

      (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) arrow)
      let expected =
            testTForall
              "a"
              Nothing
              (Elab.TArrow (testTVar "a") (testTVar "a"))
      Elab.schemeToType sch `shouldAlphaEqType` expected

    it "generalizeAt inlines rigid vars with structured bounds" $ do
      let rootGen = GenNodeId 0
          arrow = NodeId 1
          rigidVar = NodeId 2
          flexVar = NodeId 3
          rigidBound = NodeId 4
          c =
            rootedConstraint
              emptyConstraint
                { cNodes =
                    nodeMapFromList
                      [ (getNodeId arrow, TyArrow arrow rigidVar rigidVar),
                        (getNodeId rigidVar, TyVar {tnId = rigidVar, tnBound = Just rigidBound}),
                        (getNodeId flexVar, TyVar {tnId = flexVar, tnBound = Nothing}),
                        (getNodeId rigidBound, TyArrow rigidBound flexVar flexVar)
                      ],
                  cBindParents =
                    IntMap.fromList
                      [ (nodeRefKey (typeRef arrow), (genRef rootGen, BindRigid)),
                        (nodeRefKey (typeRef rigidVar), (typeRef arrow, BindRigid)),
                        (nodeRefKey (typeRef rigidBound), (typeRef arrow, BindRigid)),
                        (nodeRefKey (typeRef flexVar), (genRef rootGen, BindFlex))
                      ]
                }
          solved = mkSolved c IntMap.empty

      (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) arrow)
      let ty = Elab.schemeToType sch
          expected =
            testTForall "a"
              Nothing
              ( Elab.TArrow
                  (Elab.TArrow (testTVar "a") (testTVar "a"))
                  (Elab.TArrow (testTVar "a") (testTVar "a"))
              )
      ty `shouldAlphaEqType` expected

    it "generalizeAt preserves a rigid structural forall while quantifying the flexible result" $ do
      let rootGen = GenNodeId 0
          root = NodeId 1
          rigidDomain = NodeId 2
          flexibleResult = NodeId 3
          domainForall = NodeId 4
          domainBody = NodeId 5
          domainBinder = NodeId 6
          resultForall = NodeId 7
          resultBody = NodeId 8
          resultBinder = NodeId 9
          nodes =
            nodeMapFromList
              [ (getNodeId root, TyArrow root rigidDomain flexibleResult),
                (getNodeId rigidDomain, TyVar rigidDomain (Just domainForall)),
                (getNodeId flexibleResult, TyVar flexibleResult (Just resultForall)),
                (getNodeId domainForall, TyForall domainForall domainBody),
                (getNodeId domainBody, TyArrow domainBody domainBinder domainBinder),
                (getNodeId domainBinder, TyVar domainBinder Nothing),
                (getNodeId resultForall, TyForall resultForall resultBody),
                (getNodeId resultBody, TyArrow resultBody resultBinder resultBinder),
                (getNodeId resultBinder, TyVar resultBinder Nothing)
              ]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef rigidDomain), (genRef rootGen, BindRigid)),
                (nodeRefKey (typeRef domainForall), (typeRef rigidDomain, BindRigid)),
                (nodeRefKey (typeRef domainBody), (typeRef domainForall, BindRigid)),
                (nodeRefKey (typeRef domainBinder), (typeRef domainForall, BindRigid)),
                (nodeRefKey (typeRef flexibleResult), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef resultForall), (genRef rootGen, BindFlex)),
                (nodeRefKey (typeRef resultBody), (typeRef resultForall, BindFlex)),
                (nodeRefKey (typeRef resultBinder), (typeRef resultForall, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = fromListGen [(rootGen, GenNode rootGen [root])]
              }
          solved = mkSolved constraint IntMap.empty
          sigmaId =
            testTForall
              "id-a"
              Nothing
              (Elab.TArrow (testTVar "id-a") (testTVar "id-a"))
          expected =
            testTForall
              "result"
              (Just (boundFromType sigmaId))
              (Elab.TArrow sigmaId (testTVar "result"))

      (scheme, _subst) <- requireRight (generalizeAt solved (genRef rootGen) root)
      Elab.schemeToType scheme `shouldAlphaEqType` expected

    it "\\y. let id = (\\x. x) in id y should have type ∀a. a -> a" $ do
      let expr =
            ELam
              "y"
              ( ELet
                  "id"
                  (ELam "x" (EVar "x"))
                  (EApp (EVar "id") (EVar "y"))
              )
      (_term, ty) <- requirePipeline expr
      let expected =
            testTForall "a"
              Nothing
              (Elab.TArrow (testTVar "a") (testTVar "a"))
      ty `shouldAlphaEqType` expected

    it "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in the canonical pipeline" $ do
      -- This corresponds to aliasing a bounded variable to an existing binder:
      --   ∀a. ∀(b ⩾ a). a -> b -> a  ≤  ∀a. a -> a -> a
      let rhs = ELam "x" (ELam "y" (EVar "x"))
          schemeTy =
            mkForalls
              [ ("a", Nothing),
                ("b", Just (STVar "a"))
              ]
              (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
          ann =
            STForall
              "a"
              Nothing
              (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
          expr =
            ELet "c" (EAnn rhs schemeTy) (EAnn (EVar "c") ann)
          expected =
            testTForall "a"
              Nothing
              ( Elab.TArrow
                  (testTVar "a")
                  (Elab.TArrow (testTVar "a") (testTVar "a"))
              )

      (_term, ty) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      ty `shouldAlphaEqType` expected

    it "term annotation can instantiate a polymorphic result" $ do
      -- Paper view (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1):
      -- (b : σ) is κσ b, which checks that
      -- type(b) ≤ σ (instantiation), not type(b) == σ.
      --
      -- Here the lambda returns a polymorphic `id`, and the annotation asks
      -- for a monomorphic instance of that result.
      let ann =
            STArrow
              (STBase "Int")
              (STArrow (STBase "Int") (STBase "Int"))
          expr =
            EAnn
              ( ELam
                  "x"
                  (ELet "id" (ELam "y" (EVar "y")) (EVar "id"))
              )
              ann

      (_term, ty) <- requirePipeline expr
      -- c_tau returns a direct flexible copy of tau while the emitted coercion
      -- still performs the required polymorphic-result instantiation.
      let expected =
            Elab.TArrow
              (TestElab.tBase (BaseTy "Int"))
              ( Elab.TArrow
                  (TestElab.tBase (BaseTy "Int"))
                  (TestElab.tBase (BaseTy "Int"))
              )
      ty `shouldAlphaEqType` expected

    it "checked elaboration accepts monomorphic annotated lambda parameters" $ do
      let expr =
            EApp
              (ELamAnn "x" (STBase "Int") (EVar "x"))
              (ELit (LInt 1))
      _ <- requirePipeline expr
      _ <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      pure ()

    it "BUG-2026-02-06-001 mapped-base elaboration remains Int for nested let + annotated lambda" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "n"
                  (ELit (LInt 0))
                  ( EApp
                      ( ELamAnn
                          "f"
                          (STArrow (STBase "Int") (STBase "Int"))
                          (EApp (EVar "f") (EVar "n"))
                      )
                      (EVar "id")
                  )
              )
      (term, ty) <- requirePipeline expr
      ty `shouldBe` TestElab.tBase (BaseTy "Int")
      Elab.typeCheck term `shouldBe` Right ty

    it "annotated lambda parameter should accept a polymorphic argument via κσ (US-004)" $ do
      -- λ(f : Int -> Int). f 1   applied to polymorphic id
      -- Desugaring: λf. let f = κ(Int->Int) f in f 1
      -- Outer f may be ∀a. a -> a as long as it can be instantiated to Int -> Int.
      --
      -- Thesis-exact acceptance for this case is checked-authoritative.
      let idExpr = ELam "x" (EVar "x")
          paramTy = STArrow (STBase "Int") (STBase "Int")
          use =
            EApp
              ( ELamAnn
                  "f"
                  paramTy
                  (EApp (EVar "f") (ELit (LInt 1)))
              )
              (EVar "id")
          expr = ELet "id" idExpr use

      (term, ty) <- requirePipeline expr
      ty `shouldBe` TestElab.tBase (BaseTy "Int")
      Elab.typeCheck term `shouldBe` Right ty

    it "nested let + annotated lambda application does not crash in Phase 6 (BUG-2026-02-06-001)" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "use"
                  ( ELamAnn
                      "f"
                      (STArrow (STBase "Int") (STBase "Int"))
                      (EApp (EVar "f") (ELit (LInt 0)))
                  )
                  (EApp (EVar "use") (EVar "id"))
              )
      (term, ty) <- requirePipeline expr
      ty `shouldBe` TestElab.tBase (BaseTy "Int")
      Elab.typeCheck term `shouldBe` Right ty

    describe "Systematic bug variants (2026-02-11 matrix)" $ do
      let makeFactory = ELam "x" (ELam "y" (EVar "x"))

          assertPipelineType expr expected = do
            (_term, ty) <- requirePipeline expr
            ty `shouldAlphaEqType` expected

      it "BUG-002-V1: factory twice with mixed instantiations elaborates to Int" $ do
        let expr =
              ELet
                "make"
                makeFactory
                ( ELet
                    "c1"
                    (EApp (EVar "make") (ELit (LInt 1)))
                    ( ELet
                        "c2"
                        (EApp (EVar "make") (ELit (LBool True)))
                        (EApp (EVar "c1") (ELit (LBool False)))
                    )
                )
        assertPipelineType expr (TestElab.tBase (BaseTy "Int"))

      it "BUG-002-V2: alias indirection elaborates to Int" $ do
        let expr =
              ELet
                "make"
                makeFactory
                ( ELet
                    "f"
                    (EVar "make")
                    ( ELet
                        "c1"
                        (EApp (EVar "f") (ELit (LInt 3)))
                        (EApp (EVar "c1") (ELit (LBool True)))
                    )
                )
        assertPipelineType expr (TestElab.tBase (BaseTy "Int"))

      it "BUG-002-V3: intermediate annotation elaborates to Int" $ do
        let expr =
              ELet
                "make"
                makeFactory
                ( ELet
                    "c1"
                    ( EAnn
                        (EApp (EVar "make") (ELit (LInt 7)))
                        (STArrow (STBase "Bool") (STBase "Int"))
                    )
                    (EApp (EVar "c1") (ELit (LBool False)))
                )
        assertPipelineType expr (TestElab.tBase (BaseTy "Int"))

      it "BUG-002-V4: factory-under-lambda elaborates to ∀a. a -> a" $ do
        let expr =
              ELam
                "k"
                ( ELet
                    "make"
                    makeFactory
                    ( ELet
                        "c1"
                        (EApp (EVar "make") (EVar "k"))
                        (EApp (EVar "c1") (ELit (LBool True)))
                    )
                )
        assertPipelineType
          expr
          (testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a")))

      it "BUG-004-V1: bool analog of annotated-lambda consumer accepts polymorphic id" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                ( ELet
                    "use"
                    ( ELamAnn
                        "f"
                        (STArrow (STBase "Bool") (STBase "Bool"))
                        (EApp (EVar "f") (ELit (LBool True)))
                    )
                    (EApp (EVar "use") (EVar "id"))
                )
        assertPipelineType expr (TestElab.tBase (BaseTy "Bool"))

      it "BUG-004-V2: call-site annotation accepts explicit monomorphic instance" $ do
        let intArrow = STArrow (STBase "Int") (STBase "Int")
            expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                ( ELet
                    "use"
                    ( ELamAnn
                        "f"
                        intArrow
                        (EApp (EVar "f") (ELit (LInt 0)))
                    )
                    (EApp (EVar "use") (EAnn (EVar "id") intArrow))
                )
        assertPipelineType expr (TestElab.tBase (BaseTy "Int"))

      it "BUG-004-V3: dual annotated consumers reuse one let-polymorphic id in checked and unchecked" $ do
        let useInt =
              ELamAnn
                "f"
                (STArrow (STBase "Int") (STBase "Int"))
                (EApp (EVar "f") (ELit (LInt 0)))
            useBool =
              ELamAnn
                "f"
                (STArrow (STBase "Bool") (STBase "Bool"))
                (EApp (EVar "f") (ELit (LBool True)))
            expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                ( ELet
                    "useI"
                    useInt
                    ( ELet
                        "useB"
                        useBool
                        ( ELet
                            "_"
                            (EApp (EVar "useI") (EVar "id"))
                            (EApp (EVar "useB") (EVar "id"))
                        )
                    )
                )
        assertPipelineType expr (TestElab.tBase (BaseTy "Bool"))

      it "BUG-004-V4: annotated parameter + inner let preserves Int result" $ do
        let expr =
              EApp
                ( ELamAnn
                    "seed"
                    (STBase "Int")
                    ( ELet
                        "id"
                        (ELam "x" (EVar "x"))
                        ( ELet
                            "use"
                            ( ELamAnn
                                "f"
                                (STArrow (STBase "Int") (STBase "Int"))
                                (EApp (EVar "f") (EVar "seed"))
                            )
                            (EApp (EVar "use") (EVar "id"))
                        )
                    )
                )
                (ELit (LInt 1))
        (term, ty) <- requirePipeline expr
        ty `shouldAlphaEqType` TestElab.tBase (BaseTy "Int")
        case findNamedLet "use" term of
          Nothing ->
            expectationFailure ("expected elaborated let use, got " ++ show term)
          Just (useResolved, useScheme, useRhs) ->
            case (ElabTypes.schemeBinderRefs useScheme, useRhs) of
              ( [(schemeRef, Just schemeBound)]
                , Elab.ETyAbsRef rhsRef (Just rhsBound) rhsBody
                ) -> do
                  let intTy = TestElab.tBase (BaseTy "Int")
                      intArrow = Elab.TArrow intTy intTy
                  boundToType schemeBound `shouldAlphaEqType` intTy
                  boundToType rhsBound `shouldAlphaEqType` intTy
                  ElabTypes.schemeBody useScheme
                    `shouldAlphaEqType` Elab.TArrow intArrow (Elab.TVarRef schemeRef)
                  ElabTypes.resolvedVarType useResolved
                    `shouldBe` Elab.schemeToType useScheme
                  ElabTypes.typeBinderRefsSameIdentity schemeRef rhsRef
                    `shouldBe` True
                  case
                      filter
                        (ElabTypes.typeBinderRefsSameIdentity schemeRef)
                        (instAbstractionRefs rhsBody)
                    of
                      [instRef] -> do
                        ElabTypes.typeBinderRefsSameIdentity rhsRef instRef
                          `shouldBe` True
                      refs ->
                        expectationFailure
                          ( "expected one InstAbstrRef for the let scheme binder, got "
                              ++ show refs
                          )
              shape ->
                expectationFailure
                  ("unexpected completed let use construction: " ++ show shape)
        checkedTy <- requireRight (Elab.typeCheck term)
        checkedTy `shouldAlphaEqType` ty

      describe "Thesis-exact fallback rework strict regressions" $ do
        it "composes Var-Let alias indirection into the application expansion" $ do
          let expr =
                ELet
                  "make"
                  makeFactory
                  ( ELet
                      "f"
                      (EVar "make")
                      ( ELet
                          "c1"
                          (EApp (EVar "f") (ELit (LInt 3)))
                          (EApp (EVar "c1") (ELit (LBool True)))
                      )
                  )
          (pres, ann) <- requireRight (runToPresolutionWithAnnDefault Set.empty expr)
          -- Figure 15.3.5 assigns each occurrence its own instantiation edge.
          -- Select the application through the Var-Let alias by the resolved
          -- binder identity carried by AnnExpr, not by allocation order.
          aliasSite <-
            case variableAliasFunctionOccurrences ann of
              [(_, site)] -> pure site
              sites -> do
                expectationFailure
                  ("expected one application through a variable alias, got " ++ show sites)
                fail "ambiguous variable-alias application"
          let EdgeId aliasEdgeKey = instantiationSiteEdgeId aliasSite
          case IntMap.lookup aliasEdgeKey (prEdgeExpansions pres) of
            Just (ExpInstantiate copiedNodes) ->
              copiedNodes `shouldSatisfy` (not . null)
            actual ->
              expectationFailure
                ( "expected the application through alias f to copy make's scheme, got "
                    ++ show actual
                )
          assertPipelineType expr (TestElab.tBase (BaseTy "Int"))

      it "BUG-003-PRES: edge-0 presolution does not leave self-bound binder metas" $ do
        let rhs = ELam "x" (ELam "y" (ELam "z" (EVar "x")))
            schemeTy =
              mkForalls
                [ ("a", Nothing),
                  ("b", Just (STVar "a")),
                  ("c", Just (STVar "b"))
                ]
                ( STArrow
                    (STVar "a")
                    ( STArrow
                        (STVar "b")
                        (STArrow (STVar "c") (STVar "a"))
                    )
                )
            ann =
              STForall
                "a"
                Nothing
                ( STArrow
                    (STVar "a")
                    ( STArrow
                        (STVar "a")
                        (STArrow (STVar "a") (STVar "a"))
                    )
                )
            expr =
              ELet "c" (EAnn rhs schemeTy) (EAnn (EVar "c") ann)

        pres <- requireRight (runToPresolutionDefault Set.empty expr)
        tr <- case IntMap.lookup 0 (prEdgeTraces pres) of
          Nothing ->
            expectationFailure "BUG-003-PRES: missing edge-0 trace"
              >> fail "missing edge-0 trace"
          Just tr0 -> pure tr0

        let c = prConstraint pres
            copyMap = etCopyMap tr
            selfBoundMetas =
              [ meta
                | (binder, _arg) <- etBinderArgs tr,
                  Just meta <- [lookupCopy binder copyMap],
                  Just TyVar {tnBound = Just bnd} <- [lookupNodeIn (cNodes c) meta],
                  bnd == meta
              ]

        unless (null selfBoundMetas) $
          expectationFailure
            ( "BUG-003-PRES: self-bound binder metas in prConstraint: "
                ++ show selfBoundMetas
            )

      it "BUG-003-V1: triple bounded chain elaborates to ∀a. a -> a -> a -> a" $ do
        let rhs = ELam "x" (ELam "y" (ELam "z" (EVar "x")))
            schemeTy =
              mkForalls
                [ ("a", Nothing),
                  ("b", Just (STVar "a")),
                  ("c", Just (STVar "b"))
                ]
                ( STArrow
                    (STVar "a")
                    ( STArrow
                        (STVar "b")
                        (STArrow (STVar "c") (STVar "a"))
                    )
                )
            ann =
              STForall
                "a"
                Nothing
                ( STArrow
                    (STVar "a")
                    ( STArrow
                        (STVar "a")
                        (STArrow (STVar "a") (STVar "a"))
                    )
                )
            expr =
              ELet "c" (EAnn rhs schemeTy) (EAnn (EVar "c") ann)
            expected =
              testTForall "a"
                Nothing
                ( Elab.TArrow
                    (testTVar "a")
                    ( Elab.TArrow
                        (testTVar "a")
                        (Elab.TArrow (testTVar "a") (testTVar "a"))
                    )
                )
        assertPipelineType expr expected

      it "BUG-003-V2: dual-alias chain elaborates to ∀a. a -> a -> a -> a" $ do
        let rhs = ELam "x" (ELam "y" (ELam "z" (EVar "x")))
            schemeTy =
              mkForalls
                [ ("a", Nothing),
                  ("b", Just (STVar "a")),
                  ("c", Just (STVar "a"))
                ]
                ( STArrow
                    (STVar "a")
                    ( STArrow
                        (STVar "b")
                        (STArrow (STVar "c") (STVar "a"))
                    )
                )
            ann =
              STForall
                "a"
                Nothing
                ( STArrow
                    (STVar "a")
                    ( STArrow
                        (STVar "a")
                        (STArrow (STVar "a") (STVar "a"))
                    )
                )
            expr =
              ELet "c" (EAnn rhs schemeTy) (EAnn (EVar "c") ann)
            expected =
              testTForall "a"
                Nothing
                ( Elab.TArrow
                    (testTVar "a")
                    ( Elab.TArrow
                        (testTVar "a")
                        (Elab.TArrow (testTVar "a") (testTVar "a"))
                    )
                )
        assertPipelineType expr expected

    describe "Explicit forall annotation edge cases" $ do
      it "explicit forall annotation round-trips on let-bound variables" $ do
        let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (EAnn (EVar "id") ann)

        (_term, ty) <- requirePipeline expr
        let expected =
              testTForall "a"
                Nothing
                (Elab.TArrow (testTVar "a") (testTVar "a"))
        stripUnusedTopForalls ty
          `shouldAlphaEqType` stripUnusedTopForalls expected

    it "explicit forall coercion in let RHS elaborates through use-site application" $ do
      let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          expr =
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) ann)
              (EApp (EVar "f") (ELit (LInt 1)))

      (term, ty) <- requirePipeline expr
      checkedFromUnchecked <- requireRight (Elab.typeCheck term)
      checkedFromUnchecked `shouldBe` TestElab.tBase (BaseTy "Int")
      ty `shouldBe` TestElab.tBase (BaseTy "Int")
      (_checkedTerm, checkedTy) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      checkedTy `shouldBe` TestElab.tBase (BaseTy "Int")

    it "explicit forall annotation preserves foralls in bounds" $ do
      let ann =
            STForall
              "a"
              (Just (mkSrcBound (STForall "b" Nothing (STArrow (STVar "b") (STVar "b")))))
              (STArrow (STVar "a") (STVar "a"))
          expr = EAnn (ELam "x" (EVar "x")) ann

      (_term, ty) <- requirePipeline expr
      let expected =
            testTForall "a"
              (Just (boundFromType (testTForall "b" Nothing (Elab.TArrow (testTVar "b") (testTVar "b")))))
              (Elab.TArrow (testTVar "a") (testTVar "a"))
      ty `shouldAlphaEqType` expected

  -- See Note [ga′ scope selection — Def. 15.3.2 alignment] in Scope.hs
  -- See Note [ga′ preservation across redirects] in Scope.hs
  -- See Note [binding-parent projection — ga′ invariants] in Generalize.hs
  describe "ga′ redirect stability" $ do
    it "ga′ stable when redirect changes binding path (TyExp redirect)" $ do
      -- gen g0 owns nodes e1 (TyExp) and n2; redirect e1 → n2
      let g0 = GenNodeId 0
          e1 = NodeId 1
          n2 = NodeId 2
          nodes =
            nodeMapFromList
              [ (getNodeId e1, TyExp {tnId = e1, tnExpVar = ExpVarId 0, tnBody = n2}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing})
              ]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef e1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef n2), (typeRef e1, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = fromListGen [(g0, GenNode g0 [e1])]
              }
          solved = mkSolved constraint IntMap.empty
          noRedirects = IntMap.empty
          withRedirects = IntMap.fromList [(getNodeId e1, n2)]
      -- Without redirects: scope of e1 should be GenRef g0
      scopeNoRedir <- requireRight (resolveCanonicalScope constraint (presolutionViewFromSolved solved) noRedirects e1)
      scopeNoRedir `shouldBe` GenRef g0
      -- With redirects: scope of e1 should still be GenRef g0
      scopeWithRedir <- requireRight (resolveCanonicalScope constraint (presolutionViewFromSolved solved) withRedirects e1)
      scopeWithRedir `shouldBe` GenRef g0

    it "ga′ stable when UF merges nodes under same gen scope" $ do
      -- gen g0 owns n1 and n2 (both flex-bound); UF: n2 → n1
      let g0 = GenNodeId 0
          n1 = NodeId 1
          n2 = NodeId 2
          nodes =
            nodeMapFromList
              [ (getNodeId n1, TyVar {tnId = n1, tnBound = Nothing}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing})
              ]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef n1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef n2), (genRef g0, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = fromListGen [(g0, GenNode g0 [n1, n2])]
              }
          uf = IntMap.fromList [(getNodeId n2, n1)]
          solved = SolvedTest.mkTestSolved constraint uf
          noRedirects = IntMap.empty
      scope1 <- requireRight (resolveCanonicalScope constraint (presolutionViewFromSolved solved) noRedirects n1)
      scope1 `shouldBe` GenRef g0
      scope2 <- requireRight (resolveCanonicalScope constraint (presolutionViewFromSolved solved) noRedirects n2)
      scope2 `shouldBe` GenRef g0

    it "binding-parent canonicalization drops self-edges from UF merge" $ do
      -- n2 bound under n1, n1 bound under g0; UF: n2 → n1
      -- creates self-edge n1→n1 which must be dropped
      let g0 = GenNodeId 0
          n1 = NodeId 1
          n2 = NodeId 2
          nodes =
            nodeMapFromList
              [ (getNodeId n1, TyVar {tnId = n1, tnBound = Nothing}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing})
              ]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef n1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef n2), (typeRef n1, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = fromListGen [(g0, GenNode g0 [n1, n2])]
              }
          canonical nid
            | nid == n2 = n1
            | otherwise = nid
      result <- requireRight (BindCanon.canonicalizeBindParentsUnder canonical constraint)
      -- n1→g0 edge must survive
      IntMap.lookup (nodeRefKey (typeRef n1)) result
        `shouldBe` Just (genRef g0, BindFlex)
      -- self-edge n1→n1 (from merging n2→n1 with child n2→n1) must be dropped
      let selfEdge = case IntMap.lookup (nodeRefKey (typeRef n1)) result of
            Just (TypeRef parent, _) -> parent == n1
            _ -> False
      selfEdge `shouldBe` False

    it "end-to-end: let f = (\\x.x : forall a. a -> a) in f 42 elaborates to Int" $ do
      let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          expr =
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) ann)
              (EApp (EVar "f") (ELit (LInt 42)))
      (_term, ty) <- requirePipeline expr
      Elab.prettyDisplay ty `shouldBe` "Int"

    it "explicit-forall closure: checkSchemeClosureUnder passes without GenSchemeFreeVars exemption" $ do
      -- Regression: forall binders in annotations previously triggered
      -- GenSchemeFreeVars because domain/codomain copy gens created
      -- cross-branch type edges after solving.  The fix in
      -- checkSchemeClosureUnder walks up through scheme-owning gens.
      let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
          expr =
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) ann)
              (EApp (EVar "f") (ELit (LInt 42)))
      result <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      snd result `shouldBe` TestElab.tBase (BaseTy "Int")

    it "constructionScopes inserts a node override on base-vs-solved scope divergence" $ do
      -- Base: e1 (TyExp) bound under g0, n2 bound under e1
      -- SolvedForGen: n2 bound under n3, n3 is root (no gen ancestor)
      -- Redirect: e1 → n2; UF empty
      -- Divergence: base scope GenRef g0 ≠ solved scope TypeRef n2
      let g0 = GenNodeId 0
          e1 = NodeId 1
          n2 = NodeId 2
          n3 = NodeId 3
          baseNodes =
            nodeMapFromList
              [ (getNodeId e1, TyExp {tnId = e1, tnExpVar = ExpVarId 0, tnBody = n2}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing}),
                (getNodeId n3, TyVar {tnId = n3, tnBound = Nothing})
              ]
          baseBindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef e1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef n2), (typeRef e1, BindFlex))
              ]
          base =
            emptyConstraint
              { cNodes = baseNodes,
                cBindParents = baseBindParents,
                cGenNodes = fromListGen [(g0, GenNode g0 [e1])]
              }
          solvedForGenNodes =
            nodeMapFromList
              [ (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing}),
                (getNodeId n3, TyVar {tnId = n3, tnBound = Nothing})
              ]
          solvedForGenBP =
            IntMap.fromList
              [ (nodeRefKey (typeRef n2), (typeRef n3, BindFlex))
              ]
          solvedForGen =
            emptyConstraint
              { cNodes = solvedForGenNodes,
                cBindParents = solvedForGenBP
              }
          redirects = IntMap.fromList [(getNodeId e1, n2)]
          solved = SolvedTest.mkTestSolved solvedForGen IntMap.empty
          ann =
            ALet
              "x"
              (annDetails "x")
              g0
              e1
              (ExpVarId 0)
              g0
              (annVar "y" n2)
              (annVar "z" n3)
              n3
      overrides <-
        requireRight
          (constructionScopes base solvedForGen (presolutionViewFromSolved solved) redirects ann)
      constructionNodeScopeSelection overrides n2
        `shouldBe` UniqueConstructionScope (GenRef g0)

    it "constructionScopes preserves missing and ambiguous node selections" $ do
      let g0 = GenNodeId 0
          g1 = GenNodeId 1
          n1 = NodeId 1
          n2 = NodeId 2
          resultNode = NodeId 3
          nodes =
            nodeMapFromList
              [ (getNodeId n1, TyVar {tnId = n1, tnBound = Nothing}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing}),
                (getNodeId resultNode, TyVar {tnId = resultNode, tnBound = Nothing})
              ]
          base =
            emptyConstraint
              { cNodes = nodes,
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef n1), (genRef g0, BindFlex)),
                      (nodeRefKey (typeRef n2), (genRef g1, BindFlex))
                    ],
                cGenNodes =
                  fromListGen
                    [ (g0, GenNode g0 [n1]),
                      (g1, GenNode g1 [n2])
                    ]
              }
          solvedForGen = emptyConstraint {cNodes = nodes}
          solved = SolvedTest.mkTestSolved solvedForGen (IntMap.singleton (getNodeId n2) n1)
          solvedView = presolutionViewFromSolved solved
          ann =
            ALet
              "x"
              (annDetails "x")
              g0
              n1
              (ExpVarId 0)
              g0
              (annVar "x-rhs" resultNode)
              ( ALet
                  "y"
                  (annDetails "y")
                  g1
                  n2
                  (ExpVarId 1)
                  g1
                  (annVar "y-rhs" resultNode)
                  (annVar "body" resultNode)
                  resultNode
              )
              resultNode
          ga =
            GaBindParents
              { gaBindParentsBase = cBindParents base,
                gaBaseConstraint = base,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.empty,
                gaSolvedToBase = IntMap.empty,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
      overrides <-
        requireRight
          ( constructionScopes
              base
              solvedForGen
              solvedView
              IntMap.empty
              ann
          )
      constructionNodeScopeSelection overrides n1
        `shouldBe` AmbiguousConstructionScope
          (GenRef g0 NE.:| [GenRef g1])
      constructionNodeScopeSelection overrides (NodeId 99)
        `shouldBe` MissingConstructionScope
      case resolveConstructionScopeForNode (pvCanonical solvedView) ga overrides n1 of
        Left (Elab.ValidationFailed messages) ->
          messages
            `shouldBe`
              [ "one construction node has conflicting source scopes",
                "  node: " ++ show n1,
                "  scopes: " ++ show (GenRef g0 NE.:| [GenRef g1])
              ]
        other ->
          expectationFailure
            ("Expected ambiguous construction scope to fail closed, got: " ++ show other)

    it "constructionScopes records only the application function boundary" $ do
      let genId = GenNodeId 0
          functionNode = NodeId 1
          argumentNode = NodeId 2
          resultNode = NodeId 3
          functionEdge = EdgeId 10
          argumentEdge = EdgeId 11
          lambdaBodyEdge = EdgeId 12
          nodes =
            nodeMapFromList
              [ (getNodeId functionNode, TyVar {tnId = functionNode, tnBound = Nothing}),
                (getNodeId argumentNode, TyVar {tnId = argumentNode, tnBound = Nothing}),
                (getNodeId resultNode, TyVar {tnId = resultNode, tnBound = Nothing})
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents =
                  IntMap.singleton
                    (nodeRefKey (typeRef resultNode))
                    (genRef genId, BindFlex),
                cGenNodes = fromListGen [(genId, GenNode genId [resultNode])]
              }
          solved = SolvedTest.mkTestSolved constraint IntMap.empty
          ann =
            AApp
              (annVar "f" functionNode)
              (annVar "x" argumentNode)
              (appSite (getEdgeId functionEdge) functionNode resultNode)
              (appSite (getEdgeId argumentEdge) argumentNode resultNode)
              resultNode
      overrides <-
        requireRight
          ( constructionScopes
              constraint
              constraint
              (presolutionViewFromSolved solved)
              IntMap.empty
              ann
          )
      lambdaOverrides <-
        requireRight
          ( constructionScopes
              constraint
              constraint
              (presolutionViewFromSolved solved)
              IntMap.empty
              ( ALam
                  "x"
                  (annDetails "x")
                  argumentNode
                  genId
                  (annVar "x" argumentNode)
                  lambdaBodyEdge
                  resultNode
              )
          )
      constructionBoundaryScopeSelection overrides functionEdge
        `shouldBe` UniqueConstructionScope (GenRef genId)
      constructionBoundaryScopeSelection overrides argumentEdge
        `shouldBe` MissingConstructionScope
      constructionBoundaryScopeSelection lambdaOverrides lambdaBodyEdge
        `shouldBe` MissingConstructionScope

    it "constructionScopes propagates binding-tree errors instead of dropping ownership" $ do
      let n1 = NodeId 1
          n2 = NodeId 2
          nodes =
            nodeMapFromList
              [ (getNodeId n1, TyVar {tnId = n1, tnBound = Nothing}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing})
              ]
          base =
            emptyConstraint
              { cNodes = nodes,
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef n1), (typeRef n2, BindFlex)),
                      (nodeRefKey (typeRef n2), (typeRef n1, BindFlex))
                    ]
              }
          solvedForGen = emptyConstraint {cNodes = nodes}
          solved = SolvedTest.mkTestSolved solvedForGen IntMap.empty
          ann =
            AApp
              (annVar "f" n1)
              (annVar "x" n2)
              (appSite 20 n1 n1)
              (appSite 21 n2 n1)
              n1
      case
          constructionScopes
            base
            solvedForGen
            (presolutionViewFromSolved solved)
            IntMap.empty
            ann
        of
          Left (BindingCycleDetected _) -> pure ()
          other ->
            expectationFailure
              ("Expected construction scope binding cycle, got: " ++ show other)

    it "constructionScopes returns empty when base and solved scopes agree" $ do
      -- Both base and solvedForGen have n1 bound under g0
      -- No redirects, empty UF → scopes agree, no overrides
      let g0 = GenNodeId 0
          n1 = NodeId 1
          n2 = NodeId 2
          nodes =
            nodeMapFromList
              [ (getNodeId n1, TyVar {tnId = n1, tnBound = Nothing}),
                (getNodeId n2, TyVar {tnId = n2, tnBound = Nothing})
              ]
          bp =
            IntMap.fromList
              [ (nodeRefKey (typeRef n1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef n2), (genRef g0, BindFlex))
              ]
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bp,
                cGenNodes = fromListGen [(g0, GenNode g0 [n1, n2])]
              }
          solved = SolvedTest.mkTestSolved constraint IntMap.empty
          noRedirects = IntMap.empty
          ann =
            ALet
              "x"
              (annDetails "x")
              g0
              n1
              (ExpVarId 0)
              g0
              (annVar "y" n2)
              (annVar "z" n2)
              n2
      overrides <-
        requireRight
          (constructionScopes constraint constraint (presolutionViewFromSolved solved) noRedirects ann)
      overrides `shouldBe` mempty

    it "ga-invariant: validateCrossGenMapping filters out cross-scope nodes" $ do
      -- b1 under g0, b2 under g1; both map to same solved key.
      -- Filter with gidScope=g0 excludes b2 → only b1 in group → no conflict.
      -- Verifies the gidScope filter correctly scopes the check.
      let g0 = GenNodeId 0
          g1 = GenNodeId 10
          b1 = NodeId 1
          b2 = NodeId 2
          s1Key = 5
          baseBP =
            IntMap.fromList
              [ (nodeRefKey (typeRef b1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef b2), (genRef g0, BindFlex))
              ]
          fga ref = case ref of
            TypeRef nid
              | nid == b1 -> Just g0
              | nid == b2 -> Just g1
              | otherwise -> Nothing
            _ -> Nothing
          findSolvedKey nid
            | nid == getNodeId b1 = Just s1Key
            | nid == getNodeId b2 = Just s1Key
            | otherwise = Nothing
      validateCrossGenMapping g0 fga baseBP findSolvedKey
        `shouldBe` Right ()

    it "gaSolvedToBase resolution classifies mapped, same-domain, and missing outcomes" $ do
      let mappedSolved = NodeId 5
          mappedBase = NodeId 20
          sameDomain = NodeId 21
          missing = NodeId 999
          baseConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId mappedBase, TyVar {tnId = mappedBase, tnBound = Nothing}),
                      (getNodeId sameDomain, TyVar {tnId = sameDomain, tnBound = Nothing})
                    ]
              }
          ga =
            GaBindParents
              { gaBindParentsBase = IntMap.empty,
                gaBaseConstraint = baseConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.empty,
                gaSolvedToBase = IntMap.singleton (getNodeId mappedSolved) mappedBase,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
      resolveGaSolvedToBase ga mappedSolved
        `shouldBe` SolvedToBaseMapped mappedBase
      resolveGaSolvedToBase ga sameDomain
        `shouldBe` SolvedToBaseSameDomain sameDomain
      resolveGaSolvedToBase ga missing
        `shouldBe` SolvedToBaseMissing

    it "resolveContext propagates ga base binding-path failures instead of falling back" $ do
      let solvedN = NodeId 5
          baseN = NodeId 20
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId solvedN, TestTyBase solvedN (BaseTy "Int"))]
              }
          baseBindParents =
            IntMap.singleton
              (nodeRefKey (typeRef baseN))
              (typeRef baseN, BindFlex)
          baseConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId baseN, TestTyBase baseN (BaseTy "Int"))],
                cBindParents = baseBindParents
              }
          ga =
            GaBindParents
              { gaBindParentsBase = baseBindParents,
                gaBaseConstraint = baseConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.singleton (getNodeId baseN) solvedN,
                gaSolvedToBase = IntMap.singleton (getNodeId solvedN) baseN,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          solved = mkSolved solvedConstraint IntMap.empty
          nodes = IntMap.singleton (getNodeId solvedN) (TestTyBase solvedN (BaseTy "Int"))
          env =
            GeneralizeEnv { geConstraint = solvedConstraint,
                geOriginalConstraint = solvedConstraint,
                geNodes = nodes,
                geCanonical = id,
                geCanonKey = getNodeId,
                geLookupNode = \key -> IntMap.lookup key nodes,
                geIsTyVarKey = const False,
                geIsTyForallKey = const False,
                geIsBaseLikeKey = (== getNodeId solvedN),
                geBindParentsGa = Just ga,
                geCanonicalMap = Solved.canonicalMap solved,
                geDebugEnabled = False
              }
      case resolveContext env IntMap.empty (typeRef solvedN) solvedN of
        Left (Elab.BindingTreeError (BindingCycleDetected _)) -> pure ()
        _ -> expectationFailure "expected binding-cycle error"

    it "resolveContext preserves solved rigidity when a base mapping supplies the parent" $ do
      let scopeGen = GenNodeId 0
          solvedN = NodeId 5
          baseN = NodeId 20
          solvedBindParents =
            IntMap.singleton
              (nodeRefKey (typeRef solvedN))
              (genRef scopeGen, BindRigid)
          baseBindParents =
            IntMap.singleton
              (nodeRefKey (typeRef baseN))
              (genRef scopeGen, BindFlex)
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId solvedN, TyVar solvedN Nothing)],
                cBindParents = solvedBindParents,
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [solvedN])]
              }
          baseConstraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [(getNodeId baseN, TyVar baseN Nothing)],
                cBindParents = baseBindParents,
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [baseN])]
              }
          ga =
            GaBindParents
              { gaBindParentsBase = baseBindParents,
                gaBaseConstraint = baseConstraint,
                gaAnnotationNodeRedirects = IntMap.empty,
                gaBaseToSolved = IntMap.singleton (getNodeId baseN) solvedN,
                gaSolvedToBase = IntMap.singleton (getNodeId solvedN) baseN,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          solved = mkSolved solvedConstraint IntMap.empty
          nodes = IntMap.singleton (getNodeId solvedN) (TyVar solvedN Nothing)
          env =
            GeneralizeEnv
              { geConstraint = solvedConstraint,
                geOriginalConstraint = solvedConstraint,
                geNodes = nodes,
                geCanonical = id,
                geCanonKey = getNodeId,
                geLookupNode = \key -> IntMap.lookup key nodes,
                geIsTyVarKey = (== getNodeId solvedN),
                geIsTyForallKey = const False,
                geIsBaseLikeKey = const False,
                geBindParentsGa = Just ga,
                geCanonicalMap = Solved.canonicalMap solved,
                geDebugEnabled = False
              }

      generalizeCtx <-
        requireRight
          (resolveContext env solvedBindParents (genRef scopeGen) solvedN)
      IntMap.lookup
        (nodeRefKey (typeRef solvedN))
        (gcBindParents generalizeCtx)
        `shouldBe` Just (genRef scopeGen, BindRigid)

    it "ga-invariant: validateCrossGenMapping succeeds when multi-base mapping shares ancestor" $ do
      -- Two base nodes both under g0, both map to same solved key.
      -- fga returns g0 for both → no conflict.
      let g0 = GenNodeId 0
          b1 = NodeId 1
          b2 = NodeId 2
          s1Key = 5
          baseBP =
            IntMap.fromList
              [ (nodeRefKey (typeRef b1), (genRef g0, BindFlex)),
                (nodeRefKey (typeRef b2), (genRef g0, BindFlex))
              ]
          fga ref = case ref of
            TypeRef nid
              | nid == b1 -> Just g0
              | nid == b2 -> Just g0
              | otherwise -> Nothing
            _ -> Nothing
          findSolvedKey nid
            | nid == getNodeId b1 = Just s1Key
            | nid == getNodeId b2 = Just s1Key
            | otherwise = Nothing
      validateCrossGenMapping g0 fga baseBP findSolvedKey
        `shouldBe` Right ()
