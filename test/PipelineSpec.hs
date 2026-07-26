{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module PipelineSpec (spec) where

import IdentityTestSupport
import qualified ElabTypeTestSupport as TestElab
import Control.Monad (foldM, forM_, replicateM, unless, when)
import Data.Either (isLeft, isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set qualified as Set
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Acyclicity qualified as Acyc
import MLF.Constraint.Canonicalizer (canonicalizeNode, canonicalizerFrom)
import MLF.Constraint.Finalize qualified as Finalize
import MLF.Constraint.Finalize.TestSupport qualified as FinalizeTestSupport
import MLF.Constraint.NodeAccess qualified as NodeAccess
import MLF.Constraint.Normalize qualified as CNormalize
import MLF.Constraint.Presolution
import MLF.Constraint.Presolution.Construction (mkRawExpansionConstruction)
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    emptyExpansionConstructionPlacements,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( AmbientGammaAuthority (..),
    GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    RequiredGammaPlacement (..),
  )
import MLF.Constraint.Presolution.Plan.Finalize.TestSupport qualified as ReifyPlanTestSupport
import MLF.Constraint.Presolution.Plan.Target.GammaPlan.TestSupport
  ( expandSourceBinderRefsForTest,
    expandSourceBinderRefsWithPreferenceForTest,
  )
import MLF.Constraint.Presolution.TestSupport
  ( CopyMapping (..),
    defaultPlanBuilder,
    edgeArtifactExpansion,
    edgeArtifactTrace,
    edgeArtifactWitness,
    edgeArtifactsForTest,
    emptyEdgeArtifacts,
    insertEdgeArtifactForTest,
    lookupEdgeArtifact,
    setEdgeArtifactTraceForTest,
    setEdgeArtifactWitnessForTest,
    setEdgeArtifactsIdentityEdges,
    sourceInteriorFromList,
    toListInterior,
  )
import MLF.Constraint.Presolution.View qualified as PresolutionViewBoundary
import MLF.Constraint.Solved (Solved)
import MLF.Constraint.Solved qualified as Solved
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Elab.Pipeline
  ( ElabType,
    Pretty (..),
    applyRedirectsToAnn,
    canonicalizeAnn,
    generalizeAtWithBuilder,
    isValue,
    normalize,
    renderPipelineError,
    runPipelineElab,
    schemeBinderRefs,
    schemeBody,
    step,
    typeCheck,
  )
import MLF.Elab.Pipeline qualified as Elab
import MLF.Elab.Run.Generalize (generalizeAtWithBuilderRequired)
import MLF.Elab.Types (ResolvedVar (..), resolvedVarName)
import MLF.Elab.Elaborate.Algebra
  ( LocalGammaConstruction (..),
    LocalGammaConstructionCertificate (..),
    OwnerFinalConstruction (..),
  )
import MLF.Elab.Elaborate.Algebra.TestSupport
  ( BodyConsumerRouteTestView (..),
    DirectAmbientGammaAuthorityProvenance (..),
    DirectApplicationAmbientGammaClaim (..),
    DirectApplicationGammaClaim (..),
    attachBodyConsumerBoundRefinementForTest,
    bodyConsumerLocallyEmittedRouteProjectionProvenanceForTest,
    bodyConsumerProjectionProvenanceForTest,
    bodyConsumerRouteProjectionProvenanceForTest,
    constructionBoundAfterScopeExtensionForTest,
    directAmbientGammaAuthorityProvenanceForTest,
    inheritNestedApplicationResidualAuthorityForTest,
    inheritNestedApplicationResidualReplayAuthorityForTest,
    inheritNestedApplicationZeroLocalResidualAuthorityForTest,
    lambdaParamLocalGammaRenamesForTest,
    mkApplicationPendingLocalResultSourcePacketForTest,
    operationalEndpointTypesAgreeForTest,
    selectLocalGammaClosureOwnerLaneForTest,
    selectDirectAmbientGammaAuthorityForTest,
    selectDirectLocalApplicationArgumentTopologyForTest,
    sourceBinderAuthorityRefsForTest,
  )
import MLF.Elab.Generalize
  ( LocalGammaClosure (..),
    LocalGammaConstructor (..),
    LocalGammaEdgeOwnership (..),
    LocalGammaOwner (..),
    inheritDescendantGammaRequirements,
    localGammaDirectApplicationEdgeOwners,
    retainedDescendantGammaClosures,
    selectLocalGammaEdgeOwnership,
  )
import MLF.Elab.Elaborate.Annotation
  ( authorizedElaborationResultAnn,
    sourceTypeToElabTypeWithIdentitiesFromSupply,
  )
import MLF.Elab.Run.Generalize.Prepare
  ( authorizePreparedAnn,
    computePreparedResultType,
    generalizePreparedRoot,
    prepareGeneralizationArtifact,
    preparedAnnotated,
  )
import MLF.Elab.Run.Annotation (alignAnnInstantiationSites)
import MLF.Elab.Run.Generalize.Prepare.TestSupport
  ( PreparedGeneralizationArtifactTestView (..),
    alignSourceExpectedOperatedTypeForTest,
    prepareCompilerExactEdgePlansForTest,
    applyPreparedRootBinderSubstForTest,
    prepareCompilerExactRootBinderSubstForTest,
    prepareLocalApplicationRootClosureForTest,
    prepareLocalApplicationRootConstructionScopeForTest,
    prepareMatchedLocalGammaRootConstructionScopeForTest,
    prepareProvisionalLocalGammaRootConstructionScopeForTest,
    prepareProvisionalLocalGammaRootConstructionScopeWithRequirementEvidenceForTest,
    projectPreparedRootFreeSourceDeclarationCopiesForTest,
    reconcileRootSourceBinderAliasesForTest,
    prepareRootClosureSchemeForTest,
    prepareRootClosureSchemeWithOwnerFinalAndApplicationsForTest,
    prepareRootClosureSchemeWithOwnerFinalForTest,
    projectRootClosureSchemeWithOwnerFinalForTest,
    prepareAnnotationExpectedTypesByEdgeForTest,
    prepareElaborationExpansionConstructionPlacementsForTest,
    preparedGeneralizationArtifactTestView,
    projectPreparedSourceBinderSubstExceptForTest,
    insertPreparedTermSourceBinderAliasForTest,
    exactApplicationClosureOwnsRequirementForTest,
    applicationCertificateOwnsRootRequirementForTest,
    applicationCertificateOwnsAmbientRootRequirementForTest,
    applicationCertificateDirectClaimOwnsPlanningRequirementForTest,
    applicationCertificateCompletesProvisionalResultRequirementForTest,
    applicationCertificateTransfersRootRequirementOwnershipForTest,
    applicationCertificateDischargesRootClosureForTest,
    rootRequirementOwnershipAllowsLocalGammaClosureForTest,
    validateLocalApplicationCertificatesForTest,
    unclaimedEdgesOutsideLocalGammaClosuresForTest,
    placeFrozenRootGammaRequirementsForTest,
  )
import MLF.Elab.Run.Pipeline.TestSupport
  ( PipelineElabDetailedResult (..),
    prepareExternalBindings,
    runPipelineElabDetailedModuleKeyedForTest,
  )
import MLF.Elab.TermClosure
  ( renameTermTypeBinderRefPayloads,
    renameTermTypeVars,
    substInTermRefs,
  )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType
  ( ResultTypeInputs (..),
    computeResultTypeFallback,
    mkResultTypeInputs,
    rtcEdgeTraces,
    rtcEdgeWitnesses,
  )
import MLF.Elab.Run.ResultType.View qualified as ResultTypeView
import MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    candidateSelectionIsAmbiguous,
    candidateSelectionValue,
    selectUniqueCandidate,
    selectUniqueCandidateBy,
  )
import MLF.Elab.Run.Util
  ( canonicalizeExpansion,
    canonicalizeTrace,
    canonicalizeWitness,
    chaseRedirects,
    makeCanonicalizer,
  )
import MLF.Frontend.ConstraintGen
import MLF.Frontend.Normalize (normalizeType)
import MLF.Frontend.Syntax
import MLF.Reify.Core
  ( reifyBoundWithExternalRefsOnConstraint,
    reifyType,
    structuralBinders,
  )
import MLF.Types.Elab
  ( Ty (..),
    TypeBinderRef,
    containsArrowTy,
    containsForallTy,
    elabToBound,
    mkElabSchemeWithRefs,
    tVarWithRef,
    tyToElab,
    typeBinderRefIdentity,
    typeBinderIdentityFromNode,
    typeBinderIdentityFromUnique,
    typeBinderRefFromIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
  )
import MLF.Types.Identity
  ( IdDetails (EvidenceId),
    LocalIdentity (GeneratedLocalId),
    StructuralTypeBinderRole (StructuralSelfBinder),
    identityGeneratorAfter,
    localRefFromIdentity,
    typeBinderIdentityFromStructural,
  )
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarRefsType)
import ElabTermTestSupport
  ( mkTestDeferredVar,
    mkTestLocalLam,
    testTForall,
    testTMu,
    testTVar,
  )
import SolvedFacadeTestUtil qualified as SolvedTest
import SpecUtil
  ( PipelineArtifacts (..),
    breakCyclesAndCheckAcyclicityRaw,
    collectVarNodes,
    defaultTraceConfig,
    emptyConstraint,
    eraseConstraintPhaseForTest,
    firstShowE,
    mkForalls,
    requireRight,
    runConstraintDefault,
    runPipelineArtifactsDefault,
    runToPresolutionWithAnnDefault,
    runToSolvedDefault,
    unsafeNormalizeExpr,
  )
import MLF.Util.ElabError (ElabError (..))
import MLF.Types.Unique (UniqueIdentity (..))
import Test.Hspec
import Test.QuickCheck
  ( Gen,
    arbitrary,
    checkCoverage,
    chooseInt,
    counterexample,
    cover,
    discard,
    elements,
    forAll,
    property,
    withMaxSuccess,
    (===),
  )

viewFromSolved :: Solved -> PresolutionViewBoundary.PresolutionView 'Raw
viewFromSolved = FinalizeTestSupport.presolutionViewFromSolved

matchesRecursiveArrow :: ElabType -> ElabType -> Bool
matchesRecursiveArrow actual expected = case (actual, expected) of
  (TArrow domA codA, TArrow domE codE) ->
    matchesRecursiveMu domA domE && matchesRecursiveMu codA codE
  _ -> False

matchesRecursiveArrowCodomain :: ElabType -> ElabType -> Bool
matchesRecursiveArrowCodomain actual expected = case actual of
  TArrow _ cod -> matchesRecursiveMu cod expected
  _ -> False

matchesRecursiveMu :: ElabType -> ElabType -> Bool
matchesRecursiveMu actual expected = case (actual, expected) of
  (TMuRef _ bodyA, TMuRef _ bodyE) -> stripMuNames bodyA == stripMuNames bodyE
  _ -> False
  where
    stripMuNames ty = case ty of
      TVarRef _ -> testTVar "_"
      TArrow dom cod -> TArrow (stripMuNames dom) (stripMuNames cod)
      TBaseWithIdentity identity base -> TBaseWithIdentity identity base
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripMuNames args)
      TVarAppRef ref args -> TVarAppRef ref (fmap stripMuNames args)
      TForallRef _ mb body -> testTForall "_" (fmap stripBoundNames mb) (stripMuNames body)
      TMuRef _ body -> testTMu "_" (stripMuNames body)
      TBottom -> TBottom

    stripBoundNames bound = case bound of
      TArrow dom cod -> TArrow (stripMuNames dom) (stripMuNames cod)
      TBaseWithIdentity identity base -> TBaseWithIdentity identity base
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripMuNames args)
      TVarAppRef ref args -> TVarAppRef ref (fmap stripMuNames args)
      TForallRef _ mb body -> testTForall "_" (fmap stripBoundNames mb) (stripMuNames body)
      TMuRef _ body -> testTMu "_" (stripMuNames body)
      TBottom -> TBottom

countLeadingUnboundedForalls :: ElabType -> Int
countLeadingUnboundedForalls ty = case ty of
  TForallRef _ Nothing body -> 1 + countLeadingUnboundedForalls body
  _ -> 0

stripLeadingUnboundedForalls :: ElabType -> ElabType
stripLeadingUnboundedForalls ty = case ty of
  TForallRef _ Nothing body -> stripLeadingUnboundedForalls body
  _ -> ty

expectedSameLaneAliasFrameClearBoundaryArrow :: ElabType
expectedSameLaneAliasFrameClearBoundaryArrow =
  let recursiveTy = testTMu "a" (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
   in TArrow recursiveTy recursiveTy

expectedUriR2C1RecursiveIntCarrier :: ElabType
expectedUriR2C1RecursiveIntCarrier =
  testTMu "a" (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))

expectedUriR2C1RecursiveBoolCarrier :: ElabType
expectedUriR2C1RecursiveBoolCarrier =
  testTMu "a" (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Bool")))

containsMu :: ElabType -> Bool
containsMu ty = case ty of
  TMuRef _ _ -> True
  TArrow dom cod -> containsMu dom || containsMu cod
  TConWithIdentity _ _ args -> any containsMu args
  TForallRef _ mb body -> maybe False containsMuBound mb || containsMu body
  _ -> False
  where
    containsMuBound bound = case bound of
      TArrow dom cod -> containsMu dom || containsMu cod
      TBaseWithIdentity _ _ -> False
      TConWithIdentity _ _ args -> any containsMu args
      TVarAppRef _ args -> any containsMu args
      TForallRef _ mb body -> maybe False containsMuBound mb || containsMu body
      TMuRef _ _ -> True
      TBottom -> False

containsRollTerm :: Elab.XmlfTerm -> Bool
containsRollTerm term = case term of
  Elab.EVarNode _ -> False
  Elab.ELit _ -> False
  Elab.ELam _ body -> containsRollTerm body
  Elab.EApp f a -> containsRollTerm f || containsRollTerm a
  Elab.ELet _ _ rhs body -> containsRollTerm rhs || containsRollTerm body
  Elab.ETyAbsRef _ _ body -> containsRollTerm body
  Elab.ETyInst e _ -> containsRollTerm e
  Elab.ERoll _ _ -> True
  Elab.EUnroll body -> containsRollTerm body

containsUnrollTerm :: Elab.XmlfTerm -> Bool
containsUnrollTerm term = case term of
  Elab.EVarNode _ -> False
  Elab.ELit _ -> False
  Elab.ELam _ body -> containsUnrollTerm body
  Elab.EApp f a -> containsUnrollTerm f || containsUnrollTerm a
  Elab.ELet _ _ rhs body -> containsUnrollTerm rhs || containsUnrollTerm body
  Elab.ETyAbsRef _ _ body -> containsUnrollTerm body
  Elab.ETyInst e _ -> containsUnrollTerm e
  Elab.ERoll _ body -> containsUnrollTerm body
  Elab.EUnroll _ -> True

containsGroundIntApplication :: Elab.XmlfTerm -> Bool
containsGroundIntApplication = go []
  where
    go bounds term =
      matches bounds term
        || case term of
          Elab.ETyAbsRef ref (Just bound) body ->
            go ((ref, tyToElab bound) : bounds) body
          _ -> any (go bounds) (termChildren term)

    matches bounds candidate =
      case eraseOuterTypeConstruction candidate of
        Elab.EApp fun argument ->
          case (eraseOuterTypeConstruction fun, eraseOuterTypeConstruction argument) of
            ( Elab.ELam parameter body,
              Elab.ELit (LInt 2)
              ) ->
                alphaEqType
                  (resolveBound bounds (resolvedVarType parameter))
                  (TestElab.tBase (BaseTy "Int"))
                  && case eraseOuterTypeConstruction body of
                    Elab.ELit (LInt 1) -> True
                    _ -> False
            _ -> False
        _ -> False

    resolveBound bounds ty =
      case ty of
        TVarRef ref ->
          case
              [ bound
              | (boundRef, bound) <- bounds,
                typeBinderRefsSameIdentity ref boundRef
              ]
            of
              bound : _ -> resolveBound bounds bound
              [] -> ty
        _ -> ty

containsIdentityLinkedConstructionBridge :: Elab.XmlfTerm -> Bool
containsIdentityLinkedConstructionBridge term =
  case term of
    Elab.ETyAbsRef binderRef (Just _) body ->
      any (instantiationAppliesBinder binderRef) (termInstantiations body)
        && any (instantiationAbstractsBinder binderRef) (termInstantiations body)
        || any containsIdentityLinkedConstructionBridge (termChildren body)
    _ -> any containsIdentityLinkedConstructionBridge (termChildren term)

instantiationAppliesBinder :: TypeBinderRef -> Elab.Instantiation -> Bool
instantiationAppliesBinder binderRef inst =
  case inst of
    Elab.InstApp (TVarRef argumentRef) ->
      typeBinderRefsSameIdentity binderRef argumentRef
    Elab.InstUnderRef _ inner -> instantiationAppliesBinder binderRef inner
    Elab.InstInside inner -> instantiationAppliesBinder binderRef inner
    Elab.InstSeq left right ->
      instantiationAppliesBinder binderRef left
        || instantiationAppliesBinder binderRef right
    _ -> False

instantiationAbstractsBinder :: TypeBinderRef -> Elab.Instantiation -> Bool
instantiationAbstractsBinder binderRef inst =
  case inst of
    Elab.InstAbstrRef abstractedRef ->
      typeBinderRefsSameIdentity binderRef abstractedRef
    Elab.InstUnderRef _ inner -> instantiationAbstractsBinder binderRef inner
    Elab.InstInside inner -> instantiationAbstractsBinder binderRef inner
    Elab.InstSeq left right ->
      instantiationAbstractsBinder binderRef left
        || instantiationAbstractsBinder binderRef right
    _ -> False

termInstantiations :: Elab.XmlfTerm -> [Elab.Instantiation]
termInstantiations term =
  case term of
    Elab.ETyInst body inst -> inst : termInstantiations body
    _ -> concatMap termInstantiations (termChildren term)

eraseOuterTypeConstruction :: Elab.XmlfTerm -> Elab.XmlfTerm
eraseOuterTypeConstruction term =
  case term of
    Elab.ETyAbsRef _ _ body -> eraseOuterTypeConstruction body
    Elab.ETyInst body _ -> eraseOuterTypeConstruction body
    _ -> term

termChildren :: Elab.XmlfTerm -> [Elab.XmlfTerm]
termChildren term =
  case term of
    Elab.EVarNode{} -> []
    Elab.ELit{} -> []
    Elab.ELam _ body -> [body]
    Elab.EApp fun argument -> [fun, argument]
    Elab.ELet _ _ rhs body -> [rhs, body]
    Elab.ETyAbsRef _ _ body -> [body]
    Elab.ETyInst body _ -> [body]
    Elab.ERoll _ body -> [body]
    Elab.EUnroll body -> [body]

-- | Collect the sequence of intermediate terms produced by iterated 'step'.
-- Lazy, so @take n (iterateStep t)@ is safe even for divergent terms.
iterateStep :: Elab.XmlfTerm -> [Elab.XmlfTerm]
iterateStep t = case step t of
  Nothing -> []
  Just t' -> t' : iterateStep t'

packetCopyRefs
  :: PipelineElabDetailedResult
  -> IO (TypeBinderRef, TypeBinderRef)
packetCopyRefs result =
  case (pedTerm result, pedType result) of
    ( Elab.ETyAbsRef _ Nothing
        (Elab.ETyAbsRef _ (Just termResultBound) _),
      TForallRef _ Nothing
        (TForallRef _ (Just typeResultBound) _)
      ) ->
        case (tyToElab termResultBound, tyToElab typeResultBound) of
          (TForallRef termCopyRef _ _, TForallRef typeCopyRef _ _) ->
            pure (termCopyRef, typeCopyRef)
          bounds ->
            expectationFailure ("expected copied packet binders in K bounds, got " ++ show bounds)
              >> fail "missing copied packet binders"
    shapes ->
      expectationFailure ("expected paper K term/type shapes, got " ++ show shapes)
        >> fail "missing paper K shapes"

expectCanonicalPipelineSuccess :: SurfaceExpr -> IO (Elab.XmlfTerm, ElabType)
expectCanonicalPipelineSuccess expr =
  let normExpr = unsafeNormalizeExpr expr
   in case runPipelineElab Set.empty normExpr of
        Left err ->
          expectationFailure ("Expected canonical pipeline to succeed: " ++ renderPipelineError err)
            *> pure (error "unreachable after expectationFailure", TBottom)
        Right result@(term, ty) -> do
          typeCheck term `shouldBe` Right ty
          pure result

expectCanonicalPipelineSuccessType :: SurfaceExpr -> IO ElabType
expectCanonicalPipelineSuccessType expr =
  snd <$> expectCanonicalPipelineSuccess expr

expectCanonicalPipelinePastPhase3 :: SurfaceExpr -> Expectation
expectCanonicalPipelinePastPhase3 expr =
  let normExpr = unsafeNormalizeExpr expr
      assertNotPhase3 err =
        renderPipelineError err
          `shouldSatisfy` (not . isInfixOf "Phase 3 (acyclicity)")
   in case runPipelineElab Set.empty normExpr of
        Left err -> assertNotPhase3 err
        Right (term, ty) ->
          typeCheck term `shouldBe` Right ty

automaticMuConstraint :: SurfaceExpr -> IO (Constraint 'Raw)
automaticMuConstraint expr = do
  ConstraintResult {crConstraint = c0} <-
    requireRight (runConstraintDefault Set.empty expr)
  fst <$> requireRight (breakCyclesAndCheckAcyclicityRaw c0)

constraintContainsTyMu :: Constraint 'Raw -> Bool
constraintContainsTyMu constraint =
  any isTyMu (map snd (toListNode (cNodes constraint)))
  where
    isTyMu node = case node of
      TyMu {} -> True
      _ -> False

resultTypeInputsForArtifacts :: PipelineArtifacts -> (ResultTypeInputs 'Raw, AnnExpr, AnnExpr)
resultTypeInputsForArtifacts
  PipelineArtifacts
    { paConstraintNorm = c1,
      paPresolution = pres,
      paSolved = solved0,
      paAnnotated = ann0
    } =
    let solvedClean = FinalizeTestSupport.stepPruneSolvedBindParents solved0
        canon = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        canonical = canonicalizeNode canon
        annRedirected = applyRedirectsToAnn (prRedirects pres) ann0
        annCanon = canonicalizeAnn canonical annRedirected
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
            (\acc (baseKey, solvedNid) -> IntMap.insertWith (\_ existing -> existing) (getNodeId solvedNid) (NodeId baseKey) acc)
            IntMap.empty
            (IntMap.toList baseToSolved)
        bindParentsGa =
          GaBindParents
            { gaBindParentsBase = cBindParents c1,
              gaBaseConstraint = c1,
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
            (viewFromSolved solvedClean)
            bindParentsGa
            (defaultPlanBuilder defaultTraceConfig)
            c1
            (prRedirects pres)
            defaultTraceConfig
     in (inputs, annCanon, ann0)

spec :: Spec
spec = describe "Pipeline (Phases 1-5)" $ do
  describe "prepared application-site authority" $ do
    it "fails closed unless each site has a complete packet or identity-edge authority" $ do
      let funEdge = EdgeId 990001
          argEdge = EdgeId 990002
          funSite =
            mkInstantiationSite funEdge (NodeId 990003) (NodeId 990004)
          argSite =
            mkInstantiationSite argEdge (NodeId 990005) (NodeId 990006)
          ann =
            AApp
              (ALit (LInt 1) (NodeId 990003))
              (ALit (LInt 2) (NodeId 990005))
              funSite
              argSite
              (NodeId 990007)
          identityArtifacts =
            setEdgeArtifactsIdentityEdges
              (IntSet.fromList [getEdgeId funEdge, getEdgeId argEdge])
              emptyEdgeArtifacts
      case alignAnnInstantiationSites emptyEdgeArtifacts ann of
        Left (PhiInvariantError message) ->
          message `shouldSatisfy` ("neither a complete edge artifact" `isInfixOf`)
        other ->
          expectationFailure
            ("expected missing application-site authority, got " ++ show other)
      alignAnnInstantiationSites identityArtifacts ann `shouldBe` Right ann

  describe "External binding validation" $ do
    it "accepts variable-headed external env types" $ do
      let extEnv = Map.singleton "x" (STVarApp "f" (STVar "a" :| []))
          result = Elab.runPipelineElabWithEnv Set.empty extEnv (EVar "x")
      result `shouldSatisfy` isRight

  describe "Module packet identity supply" $ do
    it "keeps sibling K packet identities stable, distinct, and term/type aligned" $ do
      preparedExternal <- requireRight (prepareExternalBindings Map.empty)
      let kExpr = unsafeNormalizeExpr (ELam "x" (ELam "y" (EVar "x")))
          roots =
            [ ("first", "first", kExpr)
            , ("second", "second", kExpr)
            ]
          runBatch orderedRoots =
            runPipelineElabDetailedModuleKeyedForTest
              Set.empty
              preparedExternal
              Map.empty
              orderedRoots
              >>= requireRight
          requireRoot key results =
            case Map.lookup key results of
              Just result -> pure result
              Nothing -> expectationFailure ("missing module root " ++ key) >> fail "missing root"
          copyIdentities results = do
            firstResult <- requireRoot "first" results
            secondResult <- requireRoot "second" results
            (firstTermCopy, firstTypeCopy) <- packetCopyRefs firstResult
            (secondTermCopy, secondTypeCopy) <- packetCopyRefs secondResult
            typeBinderRefsSameIdentity firstTermCopy firstTypeCopy `shouldBe` True
            typeBinderRefsSameIdentity secondTermCopy secondTypeCopy `shouldBe` True
            typeBinderRefsSameIdentity firstTermCopy secondTermCopy `shouldBe` False
            pure
              ( typeBinderRefIdentity firstTermCopy
              , typeBinderRefIdentity secondTermCopy
              )
          checkedIr results =
            [ (pedTerm result, pedType result)
            | key <- ["first", "second"]
            , Just result <- [Map.lookup key results]
            ]
      firstRun <- runBatch roots
      repeatedRuns <- replicateM 8 (runBatch roots)
      reversedInputRun <- runBatch (reverse roots)
      expectedCopies <- copyIdentities firstRun
      repeatedCopies <- mapM copyIdentities repeatedRuns
      reversedInputCopies <- copyIdentities reversedInputRun
      repeatedCopies `shouldBe` replicate 8 expectedCopies
      reversedInputCopies `shouldBe` expectedCopies
      map checkedIr repeatedRuns
        `shouldBe` replicate 8 (checkedIr firstRun)

  describe "Source annotation validation" $ do
    it "accepts variable-headed term annotations" $ do
      let ty = STVarApp "f" (STVar "a" :| [])
          extEnv = Map.singleton "x" ty
          expr = EAnn (EVar "x") ty
      Elab.runPipelineElabWithEnv Set.empty extEnv (unsafeNormalizeExpr expr)
        `shouldSatisfy` isRight

    it "accepts variable-headed annotated lambda parameters" $ do
      let expr = ELamAnn "x" (STVarApp "f" (STBase "Int" :| [])) (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err ->
          expectationFailure
            ( "expected dependency-ordered construction Gamma, got "
                ++ show err
            )
        Right (term, ty) ->
          typeCheck term `shouldBe` Right ty

    it "accepts higher-kinded forall annotations with variable-headed bodies" $ do
      let body = STArrow (STVarApp "f" (STVar "a" :| [])) (STVarApp "f" (STVar "a" :| []))
          ty = mkForalls [("f", Nothing), ("a", Nothing)] body
          expr = EAnn (ELam "x" (EVar "x")) ty
      runPipelineElab Set.empty (unsafeNormalizeExpr expr) `shouldSatisfy` isRight

    it "accepts reducible type-lambda applications in term annotations" $ do
      let ty = STTyApp (STTyLam "a" (STArrow (STVar "a") (STVar "a"))) (STBase "Int")
          expr = EAnn (ELam "x" (EVar "x")) ty
      runPipelineElab Set.empty (unsafeNormalizeExpr expr) `shouldSatisfy` isRight

  describe "Shared candidate selection" $ do
    it "deduplicates repeated equal candidates instead of treating repeats as ambiguity" $ do
      selectUniqueCandidate [1 :: Int, 1, 1]
        `shouldBe` UniqueCandidateSelection 1

    it "keeps distinct candidates fail-closed as ambiguity" $ do
      selectUniqueCandidate [1 :: Int, 2]
        `shouldBe` AmbiguousCandidateSelection

    it "supports custom equality so structurally equivalent candidates collapse to one choice" $ do
      selectUniqueCandidateBy
        (\(_, arityA) (_, arityB) -> arityA == arityB)
        [("helper", 2 :: Int), ("direct", 2)]
        `shouldBe` UniqueCandidateSelection ("helper", 2)

    it "extracts only unique selections" $ do
      candidateSelectionValue (selectUniqueCandidate [1 :: Int, 1])
        `shouldBe` Just 1
      candidateSelectionValue (selectUniqueCandidate [1 :: Int, 2])
        `shouldBe` Nothing

    it "flags only ambiguous selections as ambiguous" $ do
      candidateSelectionIsAmbiguous (selectUniqueCandidate [1 :: Int, 2])
        `shouldBe` True
      candidateSelectionIsAmbiguous (selectUniqueCandidate [1 :: Int, 1])
        `shouldBe` False

  describe "Elaboration helpers" $ do
    it "W-normalizes a weakened flexible ground bound during reification" $ do
      -- Note: With coercion-only annotations, let-bindings with annotated RHS
      -- are treated as normal lets with coercion terms, not declared schemes.
      -- let f = ((\x.x) : ∀(a ⩾ Int). a -> a) in f
      -- The coercion constrains the RHS to match the annotation type.  Since
      -- the flexible ground-bound variable is inert, paper §15.2.8's W
      -- normalization weakens it and the xMLF type is Int -> Int.
      let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "a"))
          expr =
            let schemeTy = mkForalls [] ann
             in ELet "f" (EAnn (ELam "x" (EVar "x")) schemeTy) (EVar "f")

      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paPresolution = pres, paSolved = res, paRoot = root} -> do
          validateStrict res
          let rootRedirected = chaseRedirects (prRedirects pres) root
              root' = Solved.canonical res rootRedirected
          -- With coercion-only semantics, f's type is inferred (not declared)
          -- The coercion ensures the RHS has the annotated type.
          let scopeRoot =
                case Binding.bindingRoots (Solved.originalConstraint res) of
                  [GenRef gid] -> genRef gid
                  roots -> error ("PipelineSpec: unexpected binding roots " ++ show roots)
          let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
          case generalizeAt (viewFromSolved res) scopeRoot root' of
            Right (scheme, _subst) -> do
              schemeBinderRefs scheme `shouldBe` []
              let intTy = TestElab.tBase (BaseTy "Int")
              schemeBody scheme `shouldBe` TArrow intTy intTy
            Left err -> expectationFailure $ "Generalize error: " ++ show err

    it "generalizes at binding site" $ do
      -- let id = \x. x in id
      let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")

      -- We intercept the pipeline after solve to test generalizeAt on the 'id' binding
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right artifacts@PipelineArtifacts {paPresolution = pres, paSolved = res, paAnnotated = ann0} -> do
          validateStrict res
          let ann = applyRedirectsToAnn (prRedirects pres) ann0
              (inputs, _, _) = resultTypeInputsForArtifacts artifacts
          case ann of
            ALet _ _ schemeGen schemeRoot _ _ _ _ _ -> do
              let generalizeAt =
                    generalizeAtWithBuilder
                      (defaultPlanBuilder defaultTraceConfig)
                      (Just (rtcBindParentsGa inputs))
              case generalizeAt (viewFromSolved res) (genRef schemeGen) schemeRoot of
                Right scheme -> show scheme `shouldSatisfy` ("Forall" `isInfixOf`)
                Left err -> expectationFailure $ "Generalize error: " ++ show err
            _ -> expectationFailure "Expected ALet annotation"

    it "reifies TestTyCon nodes to TConWithIdentity in elaborated types" $ do
      -- Test that a constraint containing TestTyCon nodes reifies correctly to an identity-aware constructor head.
      -- Create a simple constraint with TyCon: List Int
      let intBase = BaseTy "Int"
          listBase = BaseTy "List"
          var0 = NodeId 0
          var1 = NodeId 1
          baseNode = TestTyBase var0 intBase
          listNode = TestTyCon var1 listBase (var0 :| [])
          nodes = fromListNode [(var0, baseNode), (var1, listNode)]
          constraint = emptyConstraint {cNodes = nodes}
          res = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (viewFromSolved res) var1 of
        Right ty ->
          case ty of
            TConWithIdentity _ con args -> do
              con `shouldBe` listBase
              length args `shouldBe` 1
              case args of
                (TBaseWithIdentity _ b :| []) -> b `shouldBe` intBase
                _ -> expectationFailure "Expected TBaseWithIdentity Int as argument"
            _ -> expectationFailure $ "Expected TConWithIdentity, got: " ++ show ty
        Left err -> expectationFailure $ "Reify error: " ++ show err

    it "reifies nested TestTyCon nodes to nested TConWithIdentity" $ do
      -- Test nested TyCon: List (Maybe Int)
      let intBase = BaseTy "Int"
          maybeBase = BaseTy "Maybe"
          listBase = BaseTy "List"
          var0 = NodeId 0
          var1 = NodeId 1
          var2 = NodeId 2
          intNode = TestTyBase var0 intBase
          maybeNode = TestTyCon var1 maybeBase (var0 :| [])
          listNode = TestTyCon var2 listBase (var1 :| [])
          nodes = fromListNode [(var0, intNode), (var1, maybeNode), (var2, listNode)]
          constraint = emptyConstraint {cNodes = nodes}
          res = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (viewFromSolved res) var2 of
        Right ty ->
          case ty of
            TConWithIdentity _ outerCon outerArgs -> do
              outerCon `shouldBe` listBase
              case outerArgs of
                (TConWithIdentity _ innerCon innerArgs :| []) -> do
                  innerCon `shouldBe` maybeBase
                  case innerArgs of
                    (TBaseWithIdentity _ b :| []) -> b `shouldBe` intBase
                    _ -> expectationFailure "Expected TBaseWithIdentity Int as innermost arg"
                _ -> expectationFailure "Expected nested TConWithIdentity (Maybe Int)"
            _ -> expectationFailure $ "Expected TConWithIdentity, got: " ++ show ty
        Left err -> expectationFailure $ "Reify error: " ++ show err

    it "reifies TyMu without binder child (non-local proxy fallback)" $ do
      -- Construct a minimal constraint with a TyMu whose μ-variable has NO
      -- binding-tree entry as a flex-child of the TyMu.  This simulates
      -- the non-local proxy scenario.
      let muVarId = NodeId 0 -- the μ-variable (TyVar)
          intId = NodeId 1 -- base type Int
          arrowId = NodeId 2 -- arrow: muVar -> Int
          muId = NodeId 3 -- TyMu node (body = arrow)
          muVar = TyVar muVarId Nothing
          intNode = TestTyBase intId (BaseTy "Int")
          arrowNd = TyArrow arrowId muVarId intId
          muNode = TyMu muId arrowId
          nodes =
            fromListNode
              [ (muVarId, muVar),
                (intId, intNode),
                (arrowId, arrowNd),
                (muId, muNode)
              ]
          -- NO bind-parent entry for muVarId under muId
          constraint = emptyConstraint {cNodes = nodes}
          solved = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (viewFromSolved solved) muId of
        Right ty -> do
          -- Should produce a TMu wrapping the body type
          case ty of
            TMuRef _ _ -> pure ()
            _ ->
              expectationFailure $
                "Expected TMu, got: " ++ show ty
        Left err ->
          expectationFailure $
            "Non-local proxy TyMu reify should not error: " ++ show err

    it "reifies a frozen graph-owned mu self binder after live reparenting" $ do
      let muSelf = NodeId 0
          intId = NodeId 1
          arrowId = NodeId 2
          muId = NodeId 3
          nodes =
            fromListNode
              [ (muSelf, TyVar muSelf Nothing),
                (intId, TestTyBase intId (BaseTy "Int")),
                (arrowId, TyArrow arrowId muSelf intId),
                (muId, TyMu muId arrowId)
              ]
          frozenBindParents =
            IntMap.singleton
              (nodeRefKey (typeRef muSelf))
              (typeRef muId, BindRigid)
          liveBindParents =
            IntMap.singleton
              (nodeRefKey (typeRef muSelf))
              (typeRef arrowId, BindRigid)
          constraint =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = liveBindParents
              }
          structural =
            structuralBinders constraint frozenBindParents IntMap.empty
      structural `shouldBe` IntMap.singleton (getNodeId muId) [muSelf]
      case
          reifyBoundWithExternalRefsOnConstraint
            constraint
            IntMap.empty
            IntSet.empty
            structural
            muId
        of
          Right ty@(TMuRef binderRef (TArrow (TVarRef selfRef) _)) -> do
            typeBinderRefsSameIdentity binderRef selfRef `shouldBe` True
            freeTypeVarRefsType ty `shouldBe` []
          Right ty ->
            expectationFailure
              ("expected a closed mu type whose body uses its binder, got " ++ show ty)
          Left err ->
            expectationFailure
              ("frozen graph-owned mu binder reify should not error: " ++ show err)

  describe "Root RaiseMerge required Gamma construction" $ do
    let fixture exteriorParent =
          let scopeGen = GenNodeId 0
              exterior = NodeId 1
              operated = NodeId 2
              baseTarget = NodeId 3
              baseArrow = NodeId 4
              resultRoot = NodeId 10
              targetRoot = NodeId 11
              liveArrow = NodeId 12
              liveOperated = NodeId 13
              intBase = BaseTy "Int"
              baseParents =
                IntMap.fromList $
                  maybe
                    []
                    (\parent -> [(nodeRefKey (typeRef exterior), parent)])
                    exteriorParent
                    ++ [ (nodeRefKey (typeRef operated), (typeRef exterior, BindRigid)),
                         (nodeRefKey (typeRef baseTarget), (genRef scopeGen, BindFlex)),
                         (nodeRefKey (typeRef baseArrow), (typeRef baseTarget, BindRigid))
                       ]
              baseConstraint =
                emptyConstraint
                  { cNodes =
                      fromListNode
                        [ (exterior, TyVar {tnId = exterior, tnBound = Nothing}),
                          (operated, TestTyBase operated intBase),
                          (baseTarget, TyVar {tnId = baseTarget, tnBound = Just baseArrow}),
                          (baseArrow, TyArrow {tnId = baseArrow, tnDom = exterior, tnCod = exterior})
                        ],
                    cBindParents = baseParents,
                    cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [baseTarget])]
                  }
              solvedConstraint =
                emptyConstraint
                  { cNodes =
                      fromListNode
                        [ (resultRoot, TyVar {tnId = resultRoot, tnBound = Nothing}),
                          (targetRoot, TyVar {tnId = targetRoot, tnBound = Just liveArrow}),
                          (liveArrow, TyArrow {tnId = liveArrow, tnDom = resultRoot, tnCod = resultRoot}),
                          (liveOperated, TestTyBase liveOperated intBase)
                        ],
                    cBindParents =
                      IntMap.fromList
                        [ (nodeRefKey (typeRef resultRoot), (genRef scopeGen, BindFlex)),
                          (nodeRefKey (typeRef targetRoot), (genRef scopeGen, BindFlex)),
                          (nodeRefKey (typeRef liveArrow), (typeRef targetRoot, BindRigid)),
                          (nodeRefKey (typeRef liveOperated), (typeRef resultRoot, BindRigid))
                        ],
                    cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [targetRoot])]
                  }
              exteriorRef =
                typeBinderRefFromIdentity
                  (typeBinderIdentityFromNode exterior)
                  "exterior"
              requirements =
                GeneralizationRequirements
                  { grRequiredGammaBinders =
                      [ RequiredGammaBinder
                          { rgbEdgeIds = EdgeId 0 :| [],
                            rgbExteriorNode = exterior,
                            rgbOperatedRoot = operated,
                            rgbResultRoots = resultRoot :| [],
                            rgbOperatedType = TestElab.tBase intBase,
                            rgbExactOperatedOccurrenceRef = Nothing,
                            rgbPlacement = RequiredGammaAtCurrentScope
                          }
                      ],
                    grSourceBinderRefs = IntMap.singleton (getNodeId exterior) exteriorRef,
                    grAmbientBinderRefs = [],
                    grAmbientGammaAuthorities = IntMap.empty,
                    grLocallyClosedGammaNodes = mempty
                  }
              ga =
                GaBindParents
                  { gaBindParentsBase = baseParents,
                    gaBaseConstraint = baseConstraint,
                    gaBaseToSolved =
                      IntMap.fromList
                        [ (getNodeId exterior, resultRoot),
                          (getNodeId operated, liveOperated),
                          (getNodeId baseTarget, targetRoot),
                          (getNodeId baseArrow, liveArrow)
                        ],
                    gaSolvedToBase =
                      IntMap.fromList
                        [ (getNodeId resultRoot, exterior),
                          (getNodeId liveOperated, operated),
                          (getNodeId targetRoot, baseTarget),
                          (getNodeId liveArrow, baseArrow)
                        ],
                    gaRestoredSchemeRootTargets = IntMap.empty,
                    gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
                  }
              view = Finalize.presolutionViewFromSnapshot solvedConstraint IntMap.empty
           in (scopeGen, resultRoot, targetRoot, intBase, exteriorRef, requirements, ga, view)
        generalizeRequired scopeGen targetRoot requirements ga view =
          generalizeAtWithBuilderRequired
            (defaultPlanBuilder defaultTraceConfig)
            requirements
            (Just ga)
            view
            (genRef scopeGen)
            targetRoot
        placeRequirements placement requirements =
          requirements
            { grRequiredGammaBinders =
                [ requirement {rgbPlacement = placement}
                | requirement <- grRequiredGammaBinders requirements
                ]
            }

    it "does not reintroduce a required Gamma binder through its structural alias" $ do
      let scopeGen = GenNodeId 0

          -- Frozen/base graph.
          exterior = NodeId 1
          operated = NodeId 2
          baseForall = NodeId 4
          baseForallBinder = NodeId 5

          -- The exterior's global solved route and the structural forall
          -- binder coincide at node 75.  The edge-local result remains the
          -- distinct required-Gamma route at node 225.
          liveOperated = NodeId 13
          liveForall = NodeId 74
          liveForallBinder = NodeId 75
          resultRoot = NodeId 225
          intBase = BaseTy "Int"

          baseParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef exterior), (genRef scopeGen, BindFlex)),
                (nodeRefKey (typeRef operated), (typeRef exterior, BindRigid)),
                (nodeRefKey (typeRef baseForall), (genRef scopeGen, BindFlex)),
                (nodeRefKey (typeRef baseForallBinder), (typeRef baseForall, BindFlex))
              ]
          baseConstraint =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (exterior, TyVar {tnId = exterior, tnBound = Nothing}),
                      (operated, TestTyBase operated intBase),
                      (baseForall, TyForall {tnId = baseForall, tnBody = baseForallBinder}),
                      (baseForallBinder, TyVar {tnId = baseForallBinder, tnBound = Nothing})
                    ],
                cBindParents = baseParents,
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [baseForall])]
              }
          liveParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef resultRoot), (genRef scopeGen, BindFlex)),
                (nodeRefKey (typeRef liveForall), (genRef scopeGen, BindFlex)),
                (nodeRefKey (typeRef liveForallBinder), (typeRef liveForall, BindFlex)),
                (nodeRefKey (typeRef liveOperated), (typeRef liveForallBinder, BindRigid))
              ]
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (resultRoot, TyVar {tnId = resultRoot, tnBound = Nothing}),
                      (liveForall, TyForall {tnId = liveForall, tnBody = liveForallBinder}),
                      (liveForallBinder, TyVar {tnId = liveForallBinder, tnBound = Nothing}),
                      (liveOperated, TestTyBase liveOperated intBase)
                    ],
                cBindParents = liveParents,
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [liveForall])]
              }
          exteriorRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode exterior)
              "exterior"
          requirements =
            GeneralizationRequirements
              { grRequiredGammaBinders =
                  [ RequiredGammaBinder
                      { rgbEdgeIds = EdgeId 0 :| [],
                        rgbExteriorNode = exterior,
                        rgbOperatedRoot = operated,
                        rgbResultRoots = resultRoot :| [],
                        rgbOperatedType = TestElab.tBase intBase,
                        rgbExactOperatedOccurrenceRef = Nothing,
                        rgbPlacement = RequiredGammaAtCurrentScope
                      }
                  ],
                grSourceBinderRefs =
                  IntMap.singleton (getNodeId exterior) exteriorRef,
                grAmbientBinderRefs = [],
                grAmbientGammaAuthorities = IntMap.empty,
                grLocallyClosedGammaNodes = mempty
              }
          ga =
            GaBindParents
              { gaBindParentsBase = baseParents,
                gaBaseConstraint = baseConstraint,
                gaBaseToSolved =
                  IntMap.fromList
                    [ (getNodeId exterior, liveForallBinder),
                      (getNodeId operated, liveOperated),
                      (getNodeId baseForall, liveForall),
                      (getNodeId baseForallBinder, liveForallBinder)
                    ],
                gaSolvedToBase =
                  IntMap.fromList
                    [ (getNodeId liveOperated, operated),
                      (getNodeId liveForall, baseForall),
                      (getNodeId liveForallBinder, baseForallBinder)
                    ],
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          view =
            Finalize.presolutionViewFromSnapshot solvedConstraint IntMap.empty
          generalize =
            generalizeAtWithBuilderRequired
              (defaultPlanBuilder defaultTraceConfig)
              requirements
              (Just ga)
              view
              (genRef scopeGen)
              liveForall
          leadingBinders ty =
            case ty of
              TForallRef ref mbBound body ->
                (ref, mbBound) : leadingBinders body
              _ -> []

      case generalize of
        Left err ->
          expectationFailure
            ("required Gamma structural-alias generalization failed: " ++ show err)
        Right (scheme, substRefs) -> do
          let completeLeadingSpine =
                schemeBinderRefs scheme ++ leadingBinders (schemeBody scheme)
              exteriorDeclarations =
                [ declaration
                | declaration@(ref, _) <- completeLeadingSpine,
                  typeBinderRefsSameIdentity ref exteriorRef
                ]
          case map snd exteriorDeclarations of
            [Just bound]
              | bound == TestElab.tBase intBase -> pure ()
            declarations ->
              expectationFailure
                ( "expected one bounded exterior declaration, got "
                    ++ show declarations
                    ++ "; full scheme="
                    ++ show scheme
                    ++ "; substitution="
                    ++ show substRefs
                )
          case schemeBody scheme of
            TVarRef bodyRef ->
              typeBinderRefsSameIdentity bodyRef exteriorRef `shouldBe` True
            body ->
              expectationFailure
                ( "expected the structural forall to reuse the outer Gamma binder, got "
                    ++ show body
                    ++ "; full scheme="
                    ++ show scheme
                )
          case IntMap.lookup (getNodeId resultRoot) substRefs of
            Nothing ->
              expectationFailure "required Gamma result route was absent"
            Just resultRef ->
              typeBinderRefsSameIdentity resultRef exteriorRef `shouldBe` True

    it "constructs the live result binder with the exterior identity and S'(operated) bound" $ do
      let (scopeGen, resultRoot, targetRoot, intBase, exteriorRef, requirements, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err -> expectationFailure ("required Gamma generalization failed: " ++ show err)
        Right (scheme, substRefs) -> do
          case
              [ mbBound
                | (binderRef, mbBound) <- schemeBinderRefs scheme,
                  typeBinderRefsSameIdentity binderRef exteriorRef
              ] of
            [mbBound]
              | mbBound == Just (TestElab.tBase intBase) -> pure ()
              | otherwise ->
                  expectationFailure
                    ( "expected S'(operated) as the exterior bound; got "
                        ++ show mbBound
                        ++ "; scheme="
                        ++ show scheme
                        ++ "; substitution="
                        ++ show substRefs
                    )
            bindings ->
              expectationFailure
                ( "expected one exterior-identity binder, got "
                    ++ show bindings
                    ++ "; scheme="
                    ++ show scheme
                    ++ "; substitution="
                    ++ show substRefs
                )
          case IntMap.lookup (getNodeId resultRoot) substRefs of
            Nothing -> expectationFailure "required Gamma result root was absent from the substitution"
            Just resultRef ->
              typeBinderRefsSameIdentity resultRef exteriorRef `shouldBe` True
          case grRequiredGammaBinders requirements of
            [requirement] ->
              case
                  IntMap.lookup
                    (getNodeId (rgbExteriorNode requirement))
                    substRefs
              of
                Nothing ->
                  expectationFailure
                    "required Gamma exterior was absent from the substitution"
                Just occurrenceRef ->
                  typeBinderRefsSameIdentity occurrenceRef exteriorRef `shouldBe` True
            other ->
              expectationFailure
                ("expected one required Gamma construction, got " ++ show other)
          case schemeBody scheme of
            TArrow (TVarRef domRef) (TVarRef codRef) -> do
              typeBinderRefsSameIdentity domRef exteriorRef `shouldBe` True
              typeBinderRefsSameIdentity codRef exteriorRef `shouldBe` True
            body ->
              expectationFailure
                ( "expected the result body to use the exterior binder twice, got "
                    ++ show body
                    ++ "; scheme="
                    ++ show scheme
                    ++ "; substitution="
                    ++ show substRefs
                )

    it "keeps required Gamma construction authoritative over structural source metadata" $ do
      let (scopeGen, resultRoot, targetRoot, intBase, exteriorRef, requirements0, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          structuralRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromStructural (UniqueIdentity 991008) StructuralSelfBinder)
              "structuralSelf"
          requirements =
            requirements0
              { grSourceBinderRefs =
                  IntMap.insert
                    (getNodeId resultRoot)
                    structuralRef
                    ( IntMap.insert
                        1
                        structuralRef
                        (grSourceBinderRefs requirements0)
                    )
              }
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err ->
          expectationFailure
            ("required Gamma/source metadata separation failed: " ++ show err)
        Right (scheme, substRefs) -> do
          case IntMap.lookup (getNodeId resultRoot) substRefs of
            Nothing -> expectationFailure "required Gamma result root was absent from the substitution"
            Just resultRef ->
              typeBinderRefsSameIdentity resultRef exteriorRef `shouldBe` True
          let exteriorBounds =
                [ mbBound
                | (binderRef, mbBound) <- schemeBinderRefs scheme,
                  typeBinderRefsSameIdentity binderRef exteriorRef
                ]
          exteriorBounds `shouldBe` [Just (TestElab.tBase intBase)]
          case schemeBody scheme of
            TArrow (TVarRef domRef) (TVarRef codRef) -> do
              typeBinderRefsSameIdentity domRef exteriorRef `shouldBe` True
              typeBinderRefsSameIdentity codRef exteriorRef `shouldBe` True
            body ->
              expectationFailure
                ("expected required Gamma body to retain exterior identity, got " ++ show body)

    it "accepts a nested constructor that shares the current graph scope" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements0, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          requirements =
            placeRequirements
              (RequiredGammaAtNestedScope (genRef scopeGen))
              requirements0
      generalizeRequired scopeGen targetRoot requirements ga view
        `shouldSatisfy` isRight

    it "accepts a result requirement owned by a nested graph scope" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements0, ga0, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          nestedGen = GenNodeId 1
          exterior = NodeId 1
          base0 = gaBaseConstraint ga0
          nestedParents =
            IntMap.insert
              (nodeRefKey (genRef nestedGen))
              (genRef scopeGen, BindFlex)
              ( IntMap.insert
                  (nodeRefKey (typeRef exterior))
                  (genRef nestedGen, BindFlex)
                  (cBindParents base0)
              )
          base =
            base0
              { cBindParents = nestedParents,
                cGenNodes =
                  insertGen nestedGen (GenNode nestedGen []) (cGenNodes base0)
              }
          ga =
            ga0
              { gaBindParentsBase = nestedParents,
                gaBaseConstraint = base
              }
          requirements =
            placeRequirements
              (RequiredGammaAtNestedScope (genRef nestedGen))
              requirements0
      generalizeRequired scopeGen targetRoot requirements ga view
        `shouldSatisfy` isRight

    it "accepts a certified nested constructor above an intervening flexible gen" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements0, ga0, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          nestedGen = GenNodeId 1
          exterior = NodeId 1
          base0 = gaBaseConstraint ga0
          nestedParents =
            IntMap.insert
              (nodeRefKey (genRef nestedGen))
              (genRef scopeGen, BindFlex)
              ( IntMap.insert
                  (nodeRefKey (typeRef exterior))
                  (genRef nestedGen, BindFlex)
                  (cBindParents base0)
              )
          base =
            base0
              { cBindParents = nestedParents,
                cGenNodes =
                  insertGen nestedGen (GenNode nestedGen []) (cGenNodes base0)
              }
          ga =
            ga0
              { gaBindParentsBase = nestedParents,
                gaBaseConstraint = base
              }
          requirements =
            placeRequirements
              (RequiredGammaAtNestedScope (genRef scopeGen))
              requirements0
      generalizeRequired scopeGen targetRoot requirements ga view
        `shouldSatisfy` isRight

    it "rejects a nested exterior declared as current-scope construction" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements, ga0, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          nestedGen = GenNodeId 1
          exterior = NodeId 1
          base0 = gaBaseConstraint ga0
          nestedParents =
            IntMap.insert
              (nodeRefKey (genRef nestedGen))
              (genRef scopeGen, BindFlex)
              ( IntMap.insert
                  (nodeRefKey (typeRef exterior))
                  (genRef nestedGen, BindFlex)
                  (cBindParents base0)
              )
          base =
            base0
              { cBindParents = nestedParents,
                cGenNodes =
                  insertGen nestedGen (GenNode nestedGen []) (cGenNodes base0)
              }
          ga =
            ga0
              { gaBindParentsBase = nestedParents,
                gaBaseConstraint = base
              }
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "declared construction Gamma")
        Left err -> expectationFailure ("expected misplaced-construction validation, got " ++ show err)
        Right result -> expectationFailure ("expected misplaced-construction rejection, got " ++ show result)

    it "rejects a declared nested owner in a sibling graph scope" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements0, ga0, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          siblingGen = GenNodeId 1
          siblingRoot = GenNodeId 2
          exterior = NodeId 1
          base0 = gaBaseConstraint ga0
          siblingParents =
            IntMap.insert
              (nodeRefKey (genRef siblingGen))
              (genRef siblingRoot, BindFlex)
              ( IntMap.insert
                  (nodeRefKey (typeRef exterior))
                  (genRef siblingGen, BindFlex)
                  (cBindParents base0)
              )
          base =
            base0
              { cBindParents = siblingParents,
                cGenNodes =
                  insertGen
                    siblingRoot
                    (GenNode siblingRoot [])
                    (insertGen siblingGen (GenNode siblingGen []) (cGenNodes base0))
              }
          ga =
            ga0
              { gaBindParentsBase = siblingParents,
                gaBaseConstraint = base
              }
          requirements =
            placeRequirements
              (RequiredGammaAtNestedScope (genRef siblingGen))
              requirements0
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "declared construction Gamma")
        Left err -> expectationFailure ("expected sibling-owner validation, got " ++ show err)
        Right result -> expectationFailure ("expected sibling-owner rejection, got " ++ show result)

    it "rejects a required exterior across a rigid ownership hop" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindRigid))
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "declared construction Gamma")
        Left err -> expectationFailure ("expected rigid-hop validation, got " ++ show err)
        Right result -> expectationFailure ("expected rigid-hop rejection, got " ++ show result)

    it "shares one exterior Gamma binder across multiple edge-local results" $ do
      let (scopeGen, resultRoot, targetRoot, intBase, exteriorRef, requirements0, ga, view0) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          secondResultRoot = NodeId 15
          solved0 = PresolutionViewBoundary.pvConstraint view0
          solved =
            solved0
              { cNodes =
                  insertNode
                    secondResultRoot
                    (TyVar {tnId = secondResultRoot, tnBound = Nothing})
                    (cNodes solved0),
                cBindParents =
                  IntMap.insert
                    (nodeRefKey (typeRef secondResultRoot))
                    (genRef scopeGen, BindFlex)
                    (cBindParents solved0)
              }
          requirements =
            requirements0
              { grRequiredGammaBinders =
                  concatMap
                    (\requirement ->
                      [ requirement,
                        requirement
                          { rgbResultRoots = secondResultRoot :| []
                          }
                      ])
                    (grRequiredGammaBinders requirements0)
              }
          view = Finalize.presolutionViewFromSnapshot solved IntMap.empty
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err ->
          expectationFailure
            ("multi-result required Gamma generalization failed: " ++ show err)
        Right (scheme, substRefs) -> do
          forM_ [resultRoot, secondResultRoot] $ \result ->
            case IntMap.lookup (getNodeId result) substRefs of
              Nothing ->
                expectationFailure
                  ("required Gamma result root was absent: " ++ show result)
              Just resultRef ->
                typeBinderRefsSameIdentity resultRef exteriorRef `shouldBe` True
          [ mbBound
            | (binderRef, mbBound) <- schemeBinderRefs scheme,
              typeBinderRefsSameIdentity binderRef exteriorRef
            ]
            `shouldBe` [Just (TestElab.tBase intBase)]

    it "rejects incompatible bounds for multiple results of one exterior" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements0, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          requirements =
            requirements0
              { grRequiredGammaBinders =
                  concatMap
                    (\requirement ->
                      [ requirement,
                        requirement
                          { rgbResultRoots = NodeId 15 :| [],
                            rgbOperatedType = TestElab.tBase (BaseTy "Bool")
                          }
                      ])
                    (grRequiredGammaBinders requirements0)
              }
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "incompatible bounds")
        Left err ->
          expectationFailure
            ("expected incompatible required Gamma bounds, got " ++ show err)
        Right result ->
          expectationFailure
            ("expected incompatible required Gamma rejection, got " ++ show result)

    it "lets a non-bottom exterior bound subsume a bottom-only edge" $ do
      let (scopeGen, _resultRoot, targetRoot, intBase, exteriorRef, requirements0, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          requirements =
            requirements0
              { grRequiredGammaBinders =
                  concatMap
                    (\requirement -> [requirement {rgbOperatedType = TBottom}, requirement])
                    (grRequiredGammaBinders requirements0)
              }
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err ->
          expectationFailure
            ("bottom-neutral required Gamma merge failed: " ++ show err)
        Right (scheme, _) ->
          [ mbBound
          | (binderRef, mbBound) <- schemeBinderRefs scheme,
            typeBinderRefsSameIdentity binderRef exteriorRef
          ]
            `shouldBe` [Just (TestElab.tBase intBase)]

    it "quotients a required Gamma variable bound to its existing lexical binder" $ do
      let (scopeGen, resultRoot, targetRoot, _intBase, exteriorRef, requirements0, ga0, view0) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          sourceVar = NodeId 5
          sourceLive = NodeId 14
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991007))
              "sourceAlias"
          base0 = gaBaseConstraint ga0
          base =
            base0
              { cNodes =
                  insertNode
                    sourceVar
                    (TyVar {tnId = sourceVar, tnBound = Nothing})
                    (cNodes base0),
                cBindParents =
                  IntMap.insert
                    (nodeRefKey (typeRef sourceVar))
                    (genRef scopeGen, BindFlex)
                    (cBindParents base0)
              }
          solved0 = PresolutionViewBoundary.pvConstraint view0
          solved =
            solved0
              { cNodes =
                  insertNode
                    sourceLive
                    (TyVar {tnId = sourceLive, tnBound = Nothing})
                    (cNodes solved0),
                cBindParents =
                  IntMap.insert
                    (nodeRefKey (typeRef sourceLive))
                    (genRef scopeGen, BindFlex)
                    (cBindParents solved0)
              }
          requirements =
            requirements0
              { grRequiredGammaBinders =
                  [ requirement
                      { rgbOperatedType = TVarRef sourceRef
                      }
                  | requirement <- grRequiredGammaBinders requirements0
                  ],
                grSourceBinderRefs =
                  IntMap.insert
                    (getNodeId sourceVar)
                    sourceRef
                    (grSourceBinderRefs requirements0)
              }
          ga =
            ga0
              { gaBindParentsBase = cBindParents base,
                gaBaseConstraint = base,
                gaBaseToSolved =
                  IntMap.insert
                    (getNodeId sourceVar)
                    sourceLive
                    (gaBaseToSolved ga0),
                gaSolvedToBase =
                  IntMap.insert
                    (getNodeId sourceLive)
                    sourceVar
                    (gaSolvedToBase ga0)
              }
          view = Finalize.presolutionViewFromSnapshot solved IntMap.empty

      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err ->
          expectationFailure
            ("required Gamma variable alias generalization failed: " ++ show err)
        Right (scheme, substRefs) -> do
          [ ref
            | (ref, _) <- schemeBinderRefs scheme,
              typeBinderRefsSameIdentity ref exteriorRef
            ]
            `shouldBe` []
          case IntMap.lookup (getNodeId resultRoot) substRefs of
            Nothing -> expectationFailure "required Gamma alias lost its result substitution"
            Just resultRef ->
              typeBinderRefsSameIdentity resultRef sourceRef `shouldBe` True
          case schemeBody scheme of
            TArrow (TVarRef domRef) (TVarRef codRef) -> do
              typeBinderRefsSameIdentity domRef sourceRef `shouldBe` True
              typeBinderRefsSameIdentity codRef sourceRef `shouldBe` True
            body ->
              expectationFailure
                ("expected required Gamma alias body sourceAlias -> sourceAlias, got " ++ show body)

    it "uses the edge-local result when a named exterior maps to itself globally" $ do
      let (scopeGen, resultRoot, targetRoot, intBase, exteriorRef, requirements, ga0, view0) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          exterior = NodeId 1
          solved0 = PresolutionViewBoundary.pvConstraint view0
          solved =
            solved0
              { cNodes =
                  insertNode
                    exterior
                    (TyVar {tnId = exterior, tnBound = Nothing})
                    (cNodes solved0)
              }
          ga =
            ga0
              { gaBaseToSolved =
                  IntMap.insert
                    (getNodeId exterior)
                    exterior
                    (gaBaseToSolved ga0),
                gaSolvedToBase =
                  IntMap.insert
                    (getNodeId exterior)
                    exterior
                    (gaSolvedToBase ga0)
              }
          view = Finalize.presolutionViewFromSnapshot solved IntMap.empty
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err ->
          expectationFailure
            ("edge-local required Gamma generalization failed: " ++ show err)
        Right (scheme, substRefs) -> do
          forM_ [exterior, resultRoot] $ \node ->
            case IntMap.lookup (getNodeId node) substRefs of
              Nothing ->
                expectationFailure
                  ("missing required Gamma substitution at " ++ show node)
              Just actualRef ->
                typeBinderRefsSameIdentity actualRef exteriorRef `shouldBe` True
          case
              [ mbBound
                | (binderRef, mbBound) <- schemeBinderRefs scheme,
                  typeBinderRefsSameIdentity binderRef exteriorRef
              ] of
            [Just bound] -> bound `shouldBe` TestElab.tBase intBase
            bindings ->
              expectationFailure
                ( "expected one edge-local required Gamma binder, got "
                    ++ show bindings
                    ++ "; scheme="
                    ++ show scheme
                    ++ "; substitution="
                    ++ show substRefs
                )

    it "preserves a source-named binder identity inside S'(operated)" $ do
      let (scopeGen, resultRoot, targetRoot, intBase, exteriorRef, requirements0, ga0, view0) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          sourceVar = NodeId 5
          sourceOperated = NodeId 6
          sourceLive = NodeId 14
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991006))
              "semanticSource"
          base0 = gaBaseConstraint ga0
          baseParents =
            IntMap.insert
              (nodeRefKey (typeRef sourceOperated))
              (typeRef (NodeId 1), BindRigid)
              ( IntMap.insert
                  (nodeRefKey (typeRef sourceVar))
                  (genRef scopeGen, BindFlex)
                  (cBindParents base0)
              )
          base =
            base0
              { cNodes =
                  insertNode
                    sourceOperated
                    (TyArrow {tnId = sourceOperated, tnDom = sourceVar, tnCod = NodeId 2})
                    (insertNode sourceVar (TyVar {tnId = sourceVar, tnBound = Nothing}) (cNodes base0)),
                cBindParents = baseParents
              }
          solved0 = PresolutionViewBoundary.pvConstraint view0
          solved =
            solved0
              { cNodes =
                  insertNode
                    sourceLive
                    (TyVar {tnId = sourceLive, tnBound = Nothing})
                    (cNodes solved0),
                cBindParents =
                  IntMap.insert
                    (nodeRefKey (typeRef sourceLive))
                    (genRef scopeGen, BindFlex)
                    (cBindParents solved0)
              }
          requirements =
            requirements0
              { grRequiredGammaBinders =
                  [ RequiredGammaBinder
                      { rgbEdgeIds = EdgeId 0 :| [],
                        rgbExteriorNode = NodeId 1,
                        rgbOperatedRoot = sourceOperated,
                        rgbResultRoots = resultRoot :| [],
                        rgbOperatedType =
                          TForallRef
                            sourceRef
                            Nothing
                            (TArrow (TVarRef sourceRef) (TestElab.tBase intBase)),
                        rgbExactOperatedOccurrenceRef = Nothing,
                        rgbPlacement = RequiredGammaAtCurrentScope
                      }
                  ],
                grSourceBinderRefs =
                  IntMap.insert
                    (getNodeId sourceVar)
                    sourceRef
                    (grSourceBinderRefs requirements0)
              }
          ga =
            ga0
              { gaBindParentsBase = baseParents,
                gaBaseConstraint = base,
                gaBaseToSolved =
                  IntMap.insert (getNodeId sourceVar) sourceLive (gaBaseToSolved ga0),
                gaSolvedToBase =
                  IntMap.insert (getNodeId sourceLive) sourceVar (gaSolvedToBase ga0)
              }
          view = Finalize.presolutionViewFromSnapshot solved IntMap.empty
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left err -> expectationFailure ("source-named required Gamma generalization failed: " ++ show err)
        Right (scheme, substRefs) -> do
          case
              [ mbBound
                | (binderRef, mbBound) <- schemeBinderRefs scheme,
                  typeBinderRefsSameIdentity binderRef exteriorRef
              ] of
            [Just (TForallRef actualSourceBinder Nothing (TArrow (TVarRef actualSourceUse) actualCod))] -> do
              typeBinderRefsSameIdentity actualSourceBinder sourceRef `shouldBe` True
              typeBinderRefsSameIdentity actualSourceUse sourceRef `shouldBe` True
              typeBinderRefNode actualSourceBinder `shouldBe` Nothing
              typeBinderRefNode actualSourceUse `shouldBe` Nothing
              actualCod `shouldBe` TestElab.tBase intBase
            exteriorBounds ->
              expectationFailure
                ( "expected exterior bound forall semanticSource. semanticSource -> Int, got "
                    ++ show exteriorBounds
                    ++ "; scheme="
                    ++ show scheme
                )
          case IntMap.lookup (getNodeId resultRoot) substRefs of
            Nothing -> expectationFailure "source-named case lost the required result substitution"
            Just resultRef ->
              typeBinderRefsSameIdentity resultRef exteriorRef `shouldBe` True

    it "rejects a required exterior without a base-to-solved result bridge" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements, ga, view) =
            fixture (Just (genRef (GenNodeId 0), BindFlex))
          gaMissingBridge = ga {gaBaseToSolved = IntMap.empty}
      case generalizeRequired scopeGen targetRoot requirements gaMissingBridge view of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "exterior has no base-to-solved bridge")
        Left err -> expectationFailure ("expected missing-bridge validation, got " ++ show err)
        Right result -> expectationFailure ("expected missing-bridge rejection, got " ++ show result)

    it "rejects a required exterior outside the frozen base Gamma" $ do
      let (scopeGen, _resultRoot, targetRoot, _intBase, _exteriorRef, requirements, ga, view) =
            fixture Nothing
      case generalizeRequired scopeGen targetRoot requirements ga view of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "declared construction Gamma")
        Left err -> expectationFailure ("expected base-Gamma validation, got " ++ show err)
        Right result -> expectationFailure ("expected base-Gamma rejection, got " ++ show result)

    it "routes one source identity through every base alias of a solved class" $ do
      let sourceAlias = NodeId 20
          traversalAlias = NodeId 21
          solvedNode = NodeId 120
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992001))
              "source"
          baseToSolved =
            IntMap.fromList
              [ (getNodeId sourceAlias, solvedNode),
                (getNodeId traversalAlias, solvedNode)
              ]
          directRefs = IntMap.singleton (getNodeId sourceAlias) sourceRef
      case expandSourceBinderRefsForTest id baseToSolved directRefs of
        Left err -> expectationFailure ("source identity expansion failed: " ++ show err)
        Right expanded ->
          forM_ [sourceAlias, traversalAlias, solvedNode] $ \node ->
            case IntMap.lookup (getNodeId node) expanded of
              Nothing -> expectationFailure ("missing expanded source identity at " ++ show node)
              Just actualRef ->
                typeBinderRefsSameIdentity actualRef sourceRef `shouldBe` True

    it "keeps construction-local identities out of solved source-binder classes" $ do
      let graphAlias = NodeId 22
          graphPeer = NodeId 23
          graphSolved = NodeId 122
          structuralAlias = NodeId 24
          structuralPeer = NodeId 25
          structuralSolved = NodeId 124
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphAlias)
              "graph"
          structuralRef =
            typeBinderRefFromIdentity
              ( typeBinderIdentityFromStructural
                  (UniqueIdentity 992008)
                  StructuralSelfBinder
              )
              "structural"
          baseToSolved =
            IntMap.fromList
              [ (getNodeId graphAlias, graphSolved)
              , (getNodeId graphPeer, graphSolved)
              , (getNodeId structuralAlias, structuralSolved)
              , (getNodeId structuralPeer, structuralSolved)
              ]
          directRefs =
            IntMap.fromList
              [ (getNodeId graphAlias, graphRef)
              , (getNodeId structuralAlias, structuralRef)
              ]
      case expandSourceBinderRefsForTest id baseToSolved directRefs of
        Left err ->
          expectationFailure
            ("construction-local identity expansion failed: " ++ show err)
        Right expanded -> do
          IntMap.lookup (getNodeId graphAlias) expanded
            `shouldBe` Just graphRef
          IntMap.lookup (getNodeId structuralAlias) expanded
            `shouldBe` Just structuralRef
          forM_ [graphPeer, graphSolved, structuralPeer, structuralSolved] $ \node ->
            IntMap.lookup (getNodeId node) expanded `shouldBe` Nothing

    it "keeps a direct graph source identity off its canonical ReifyPlan key" $ do
      let graphNode = NodeId 23
          canonicalNode = NodeId 30
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "h"
          canonical node
            | node == graphNode = canonicalNode
            | otherwise = node
          planned =
            ReifyPlanTestSupport.canonicalizeReifySubstRefsForTest
              canonical
              (IntMap.singleton (getNodeId graphNode) graphRef)
          reifySubst =
            ReifyPlanTestSupport.mergeReifySubstRefsForTest
              IntMap.empty
              (IntMap.singleton (getNodeId graphNode) graphRef)
              IntMap.empty
              IntMap.empty
              planned
      IntMap.lookup (getNodeId graphNode) reifySubst
        `shouldBe` Just graphRef
      IntMap.lookup (getNodeId canonicalNode) reifySubst
        `shouldBe` Nothing

    it "publishes a graph route certified by solved and base Gamma provenance" $ do
      let baseNode = NodeId 44
          solvedKey = 56
          binderKey = 54
          binderRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId binderKey))
              "t54"
          baseGammaRep =
            IntMap.singleton (getNodeId baseNode) binderKey
          gammaAlias = IntMap.singleton solvedKey binderKey
          substBase = IntMap.singleton binderKey binderRef
          certified =
            ReifyPlanTestSupport.certifiedFromBaseAliasRouteForTest
              baseGammaRep
              gammaAlias
              substBase
              (solvedKey, baseNode)
      certified `shouldBe` Just (solvedKey, binderRef)
      ReifyPlanTestSupport.certifiedFromBaseAliasRouteForTest
        baseGammaRep
        (IntMap.singleton solvedKey (binderKey + 1))
        substBase
        (solvedKey, baseNode)
        `shouldBe` Nothing

    describe "frozen root Gamma placement" $ do
      let currentGen = GenNodeId 991770
          nestedGen = GenNodeId 991771
          siblingRoot = GenNodeId 991772
          exterior = NodeId 991773
          exteriorAlias = NodeId 991774
          requirement =
            RequiredGammaBinder
              { rgbEdgeIds = EdgeId 991775 :| [],
                rgbExteriorNode = exterior,
                rgbOperatedRoot = NodeId 991776,
                rgbResultRoots = NodeId 991777 :| [],
                rgbOperatedType = TBottom,
                rgbExactOperatedOccurrenceRef = Nothing,
                rgbPlacement = RequiredGammaAtCurrentScope
              }
          requirements =
            GeneralizationRequirements
              { grRequiredGammaBinders = [requirement],
                grSourceBinderRefs = IntMap.empty,
                grAmbientBinderRefs = [],
                grAmbientGammaAuthorities = IntMap.empty,
                grLocallyClosedGammaNodes = IntSet.empty
              }
          baseConstraint =
            toPresolvedConstraint
              (toAcyclicConstraint (toNormalizedConstraint emptyConstraint))
          gaWith parents =
            GaBindParents
              { gaBindParentsBase = parents,
                gaBaseConstraint =
                  baseConstraint
                    { cBindParents = parents,
                      cGenNodes =
                        fromListGen
                          [ (currentGen, GenNode currentGen []),
                            (nestedGen, GenNode nestedGen []),
                            (siblingRoot, GenNode siblingRoot [])
                          ]
                    },
                gaBaseToSolved = IntMap.empty,
                gaSolvedToBase = IntMap.empty,
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements =
                  emptyExpansionConstructionPlacements
              }
          exteriorParents ownerParent =
            IntMap.fromList
              [ ( nodeRefKey (typeRef exterior),
                  (typeRef exteriorAlias, BindFlex)
                ),
                ( nodeRefKey (typeRef exteriorAlias),
                  (genRef nestedGen, BindFlex)
                ),
                ( nodeRefKey (genRef nestedGen),
                  (ownerParent, BindFlex)
                )
              ]

      it "constructs placement at the nearest contained frozen gen" $ do
        let ga = gaWith (exteriorParents (genRef currentGen))
        case
            placeFrozenRootGammaRequirementsForTest
              ga
              (genRef currentGen)
              requirements
          of
            Left err ->
              expectationFailure
                ("frozen nested Gamma placement failed: " ++ show err)
            Right placed ->
              map rgbPlacement (grRequiredGammaBinders placed)
                `shouldBe`
                  [RequiredGammaAtNestedScope (genRef nestedGen)]

      it "constructs a parentless result exterior at the exact current scope" $ do
        let rootRequirement =
              requirement
                { rgbResultRoots = exterior :| []
                }
            rootRequirements =
              requirements
                { grRequiredGammaBinders = [rootRequirement]
                }
        case
            placeFrozenRootGammaRequirementsForTest
              (gaWith IntMap.empty)
              (genRef currentGen)
              rootRequirements
          of
            Left err ->
              expectationFailure
                ("parentless root-endpoint Gamma placement failed: " ++ show err)
            Right placed ->
              map rgbPlacement (grRequiredGammaBinders placed)
                `shouldBe`
                  [RequiredGammaAtConstructionScope (genRef currentGen)]

      it "rejects a parentless exterior that is not a result endpoint" $ do
        case
            placeFrozenRootGammaRequirementsForTest
              (gaWith IntMap.empty)
              (genRef currentGen)
              requirements
          of
            Left (ValidationFailed messages) ->
              messages
                `shouldSatisfy` any
                  (isInfixOf "has no owning gen")
            Left err ->
              expectationFailure
                ("expected missing-owner failure, got " ++ show err)
            Right placed ->
              expectationFailure
                ("expected missing-owner rejection, got " ++ show placed)

      it "rejects a frozen owner outside the current construction scope" $ do
        let ga = gaWith (exteriorParents (genRef siblingRoot))
        case
            placeFrozenRootGammaRequirementsForTest
              ga
              (genRef currentGen)
              requirements
          of
            Left (ValidationFailed messages) ->
              messages
                `shouldSatisfy` any
                  (isInfixOf "not contained by the current construction scope")
            Left err ->
              expectationFailure
                ("expected frozen-owner containment failure, got " ++ show err)
            Right placed ->
              expectationFailure
                ("expected frozen-owner containment rejection, got " ++ show placed)

    describe "local application Gamma ownership" $ do
      let pendingBoundaryEdge = EdgeId 991750
          pendingSiblingEdge = EdgeId 991751
          pendingWrongEdge = EdgeId 991752
          pendingExterior = NodeId 991753
          pendingWrongExterior = NodeId 991754
          pendingOperated = NodeId 991755
          pendingResult = NodeId 991756
          pendingWrongResult = NodeId 991757
          pendingOwnerResult = NodeId 991758
          pendingOwner =
            LocalGammaOwner
              { lgoConstructor = LocalApplicationGamma
              , lgoBoundaryEdge = pendingBoundaryEdge
              , lgoTermNode = pendingOwnerResult
              , lgoScope = GenRef (GenNodeId 991759)
              }
          pendingEdges =
            pendingBoundaryEdge :| [pendingSiblingEdge]
          pendingRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode pendingExterior)
              "pending"
          sameNamedRepresentativeRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode pendingWrongExterior)
              "pending"
          separateResultRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode pendingResult)
              "result"
          sourcePendingRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991761))
              "source-pending"
          bodyConstructionRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId 991762))
              "body-consumer"
          completedBodyBound =
            TArrow
              (TVarRef separateResultRef)
              (TVarRef separateResultRef)
          bodyOwner =
            pendingOwner
              { lgoConstructor = LocalLambdaGamma
              }
          bodyRequirement =
            pendingRequirement
              { rgbEdgeIds = pendingBoundaryEdge :| []
              , rgbOperatedType = completedBodyBound
              }
          ordinaryBodyClosure =
            pendingClosure
              { lgcEdgeIds = pendingBoundaryEdge :| []
              , lgcOwner = bodyOwner
              , lgcOwnerPendingScheme = Nothing
              }
          ordinaryBodyClosures =
            IntMap.singleton
              (getEdgeId pendingBoundaryEdge)
              ordinaryBodyClosure
          ordinaryBodyRoute =
            BodyConsumerRouteTestView
              { bcrtvEdgeId = pendingBoundaryEdge
              , bcrtvOwner = bodyOwner
              , bcrtvExteriorNode = pendingExterior
              , bcrtvSemanticRef = pendingRef
              , bcrtvConstructionRef = bodyConstructionRef
              , bcrtvOperatedType = completedBodyBound
              , bcrtvConstructionOperatedType = completedBodyBound
              }
          ordinaryBodyBindings =
            Map.fromList
              [ (bodyConstructionRef, TArrow TBottom TBottom)
              , (pendingRef, completedBodyBound)
              ]
          pendingSchemeWithResult ref resultNode resultRef =
            Elab.schemeInfoFromRefSubst
              (Elab.mkElabSchemeWithRefs [(ref, Nothing)] TBottom)
              ( IntMap.fromList
                  [ (getNodeId pendingExterior, ref)
                  , (getNodeId resultNode, resultRef)
                  ]
              )
          pendingScheme ref resultNode =
            pendingSchemeWithResult ref resultNode ref
          applicationSourceScheme ref resultNode =
            Elab.schemeInfoFromRefSubst
              ( Elab.schemeFromType
                  (TestElab.tBase (BaseTy "ClosedApplicationResult"))
              )
              ( IntMap.fromList
                  [ (getNodeId pendingExterior, ref)
                  , (getNodeId resultNode, ref)
                  ]
              )
          pendingClosure =
            LocalGammaClosure
              { lgcEdgeIds = pendingEdges
              , lgcDirectApplicationEdgeIds = []
              , lgcExteriorNode = pendingExterior
              , lgcConsumerIdentity =
                  typeBinderIdentityFromNode pendingExterior
              , lgcOwner = pendingOwner
              , lgcOwnerPendingScheme =
                  Just (pendingScheme pendingRef pendingResult)
              }
          pendingClosures closure =
            IntMap.fromList
              [ (getEdgeId pendingBoundaryEdge, closure)
              , (getEdgeId pendingSiblingEdge, closure)
              ]
          pendingRequirement =
            RequiredGammaBinder
              { rgbEdgeIds = pendingEdges
              , rgbExteriorNode = pendingExterior
              , rgbOperatedRoot = pendingOperated
              , rgbResultRoots = pendingResult :| []
              , rgbOperatedType = TestElab.tBase (BaseTy "Int")
              , rgbExactOperatedOccurrenceRef = Nothing
              , rgbPlacement = RequiredGammaAtCurrentScope
              }
          coincidentResultRequirement =
            pendingRequirement
              { rgbResultRoots = pendingExterior :| []
              }
          coincidentConstructionRequirement =
            coincidentResultRequirement
              { rgbPlacement =
                  RequiredGammaAtConstructionScope (lgoScope pendingOwner)
              }
          argumentConstructionRequirement =
            coincidentConstructionRequirement
              { rgbEdgeIds = pendingSiblingEdge :| []
              }
          classifyPending =
            directAmbientGammaAuthorityProvenanceForTest

      it "uses a consumed local argument bound when an exact result constructs it" $ do
        let exactResult = TestElab.tBase (BaseTy "Result")
            localBound = TestElab.tBase (BaseTy "Int")
            localDeclaration = TVarRef pendingRef
        selectDirectLocalApplicationArgumentTopologyForTest
          (Just exactResult)
          (Just localBound)
          (Just localDeclaration)
          `shouldBe` Just localBound

      it "keeps a direct local argument declaration without an exact result" $ do
        let localBound = TestElab.tBase (BaseTy "Int")
            localDeclaration = TVarRef pendingRef
        selectDirectLocalApplicationArgumentTopologyForTest
          Nothing
          (Just localBound)
          (Just localDeclaration)
          `shouldBe` Just localDeclaration

      it "refines the current application's exact source-packet result" $ do
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              (applicationSourceScheme pendingRef pendingExterior)
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket -> do
              let provenance =
                    classifyPending
                      []
                      pendingOwner
                      (Just sourcePacket)
                      IntMap.empty
                      argumentConstructionRequirement
                      pendingExterior
                      pendingRef
                      TBottom
                  exactBound = TestElab.tBase (BaseTy "Int")
                  select =
                    selectDirectAmbientGammaAuthorityForTest pendingRef
              provenance `shouldBe` DirectAmbientProvisionalNestedResult
              select
                [ (provenance, pendingRef, TBottom)
                , (DirectAmbientEstablished, pendingRef, exactBound)
                ]
                `shouldBe` Right (Just (pendingRef, exactBound))
              select
                [ (DirectAmbientEstablished, pendingRef, exactBound)
                , (provenance, pendingRef, TBottom)
                ]
                `shouldBe` Right (Just (pendingRef, exactBound))

      it "retains exact frozen descendant-closure result authority" $ do
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures pendingClosure)
          coincidentResultRequirement
          pendingExterior
          pendingRef
          TBottom
          `shouldBe` DirectAmbientProvisionalNestedResult

      it "does not transfer argument-side authority to another edge" $ do
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              (applicationSourceScheme pendingRef pendingExterior)
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket ->
              classifyPending
                []
                pendingOwner
                (Just sourcePacket)
                IntMap.empty
                ( argumentConstructionRequirement
                    { rgbEdgeIds = pendingWrongEdge :| []
                    }
                )
                pendingExterior
                pendingRef
                TBottom
                `shouldBe` DirectAmbientEstablished

      it "does not classify the application function edge as argument-side" $ do
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              (applicationSourceScheme pendingRef pendingExterior)
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket ->
              classifyPending
                []
                pendingOwner
                (Just sourcePacket)
                IntMap.empty
                ( argumentConstructionRequirement
                    { rgbEdgeIds = pendingBoundaryEdge :| []
                    }
                )
                pendingExterior
                pendingRef
                TBottom
                `shouldBe` DirectAmbientEstablished

      it "does not transfer argument-side authority to another endpoint" $ do
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              (applicationSourceScheme pendingRef pendingWrongExterior)
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket ->
              classifyPending
                []
                pendingOwner
                (Just sourcePacket)
                IntMap.empty
                ( argumentConstructionRequirement
                    { rgbExteriorNode = pendingWrongExterior
                    , rgbResultRoots = pendingWrongExterior :| []
                    }
                )
                pendingWrongExterior
                pendingRef
                TBottom
                `shouldBe` DirectAmbientEstablished

      it "requires an exact scheme route at the argument endpoint" $ do
        let missingEndpointScheme =
              Elab.schemeInfoFromRefSubst
                ( Elab.schemeFromType
                    (TestElab.tBase (BaseTy "ClosedApplicationResult"))
                )
                ( IntMap.singleton
                    (getNodeId pendingWrongExterior)
                    pendingRef
                )
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              missingEndpointScheme
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket ->
              classifyPending
                []
                pendingOwner
                (Just sourcePacket)
                IntMap.empty
                argumentConstructionRequirement
                pendingExterior
                pendingRef
                TBottom
                `shouldBe` DirectAmbientEstablished

      it "requires the exterior to be the sole frozen edge-local result" $ do
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              (applicationSourceScheme pendingRef pendingWrongResult)
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket ->
              classifyPending
                []
                pendingOwner
                (Just sourcePacket)
                IntMap.empty
                ( argumentConstructionRequirement
                    { rgbResultRoots = pendingWrongResult :| []
                    }
                )
                pendingWrongResult
                pendingRef
                TBottom
                `shouldBe` DirectAmbientEstablished

      it "does not transfer a current source packet across construction scopes" $ do
        case
            mkApplicationPendingLocalResultSourcePacketForTest
              pendingOwner
              pendingSiblingEdge
              [argumentConstructionRequirement]
              (applicationSourceScheme pendingRef pendingExterior)
          of
            Left err ->
              expectationFailure
                ("expected exact application source packet, got " ++ show err)
            Right sourcePacket ->
              classifyPending
                []
                pendingOwner
                (Just sourcePacket)
                IntMap.empty
                ( argumentConstructionRequirement
                    { rgbPlacement =
                        RequiredGammaAtConstructionScope
                          (GenRef (GenNodeId 991760))
                    }
                )
                pendingExterior
                pendingRef
                TBottom
                `shouldBe` DirectAmbientEstablished

      it "rejects source packets for another constructor or the function edge" $ do
        mkApplicationPendingLocalResultSourcePacketForTest
          (pendingOwner {lgoConstructor = LocalLambdaGamma})
          pendingSiblingEdge
          [argumentConstructionRequirement]
          (applicationSourceScheme pendingRef pendingExterior)
          `shouldSatisfy` isLeft
        mkApplicationPendingLocalResultSourcePacketForTest
          pendingOwner
          pendingBoundaryEdge
          [argumentConstructionRequirement]
          (applicationSourceScheme pendingRef pendingExterior)
          `shouldSatisfy` isLeft

      it "classifies a structured root skeleton from the exact frozen pending result" $ do
        bodyConsumerProjectionProvenanceForTest
          []
          pendingOwner
          (pendingClosures pendingClosure)
          coincidentResultRequirement
          pendingExterior
          pendingRef
          (TArrow TBottom TBottom)
          `shouldBe` DirectAmbientProvisionalNestedResult

      it "completes a structured consumer when its exact result has a distinct route" $ do
        let distinctResultClosure =
              pendingClosure
                { lgcOwnerPendingScheme =
                    Just
                      ( pendingSchemeWithResult
                          pendingRef
                          pendingResult
                          separateResultRef
                      )
                }
        bodyConsumerProjectionProvenanceForTest
          []
          pendingOwner
          (pendingClosures distinctResultClosure)
          pendingRequirement
          pendingResult
          pendingRef
          (TArrow TBottom TBottom)
          `shouldBe` DirectAmbientProvisionalNestedResult
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures distinctResultClosure)
          pendingRequirement
          pendingResult
          pendingRef
          TBottom
          `shouldBe` DirectAmbientEstablished

      it "completes a source-projected pending consumer only through its exact construction rename" $ do
        let sourceProjectedClosure =
              pendingClosure
                { lgcOwnerPendingScheme =
                    Just
                      ( pendingSchemeWithResult
                          sourcePendingRef
                          pendingResult
                          separateResultRef
                      )
                }
            classify renames =
              bodyConsumerProjectionProvenanceForTest
                renames
                pendingOwner
                (pendingClosures sourceProjectedClosure)
                pendingRequirement
                pendingResult
                pendingRef
                (TArrow TBottom TBottom)
        classify [(sourcePendingRef, pendingRef)]
          `shouldBe` DirectAmbientProvisionalNestedResult
        classify []
          `shouldBe` DirectAmbientEstablished

      it "keeps a same-shaped ordinary ambient bottom declaration established and conflicting" $ do
        let ordinaryClosure =
              pendingClosure {lgcOwnerPendingScheme = Nothing}
            provenance =
              classifyPending
                []
                pendingOwner
                Nothing
                (pendingClosures ordinaryClosure)
                coincidentResultRequirement
                pendingExterior
                pendingRef
                TBottom
            exactBound = TestElab.tBase (BaseTy "Int")
        provenance `shouldBe` DirectAmbientEstablished
        selectDirectAmbientGammaAuthorityForTest
          pendingRef
          [ (provenance, pendingRef, TBottom)
          , (DirectAmbientEstablished, pendingRef, exactBound)
          ]
          `shouldSatisfy` isLeft

      it "keeps the pending-scheme projection lane established without its scheme" $ do
        let ordinaryClosure =
              pendingClosure {lgcOwnerPendingScheme = Nothing}
        bodyConsumerProjectionProvenanceForTest
          []
          pendingOwner
          (pendingClosures ordinaryClosure)
          coincidentResultRequirement
          pendingExterior
          pendingRef
          (TArrow TBottom TBottom)
          `shouldBe` DirectAmbientEstablished

      it "completes an ordinary frozen body route from its exact ambient exterior" $ do
        bodyConsumerRouteProjectionProvenanceForTest
          []
          bodyOwner
          ordinaryBodyClosures
          bodyRequirement
          ordinaryBodyRoute
          completedBodyBound
          ordinaryBodyBindings
          `shouldBe` DirectAmbientProvisionalNestedResult

      it "completes an ordinary frozen body route from its exact local emission" $ do
        let provisionalAmbientBindings =
              Map.singleton
                bodyConstructionRef
                (TArrow TBottom TBottom)
            classify route bindings =
              bodyConsumerLocallyEmittedRouteProjectionProvenanceForTest
                []
                bodyOwner
                ordinaryBodyClosures
                bodyRequirement
                bodyConstructionRef
                completedBodyBound
                route
                completedBodyBound
                bindings
        bodyConsumerRouteProjectionProvenanceForTest
          []
          bodyOwner
          ordinaryBodyClosures
          bodyRequirement
          ordinaryBodyRoute
          completedBodyBound
          provisionalAmbientBindings
          `shouldBe` DirectAmbientEstablished
        classify ordinaryBodyRoute provisionalAmbientBindings
          `shouldBe` DirectAmbientProvisionalNestedResult
        classify
          (ordinaryBodyRoute {bcrtvOwner = pendingOwner})
          provisionalAmbientBindings
          `shouldBe` DirectAmbientEstablished
        classify
          (ordinaryBodyRoute {bcrtvEdgeId = pendingWrongEdge})
          provisionalAmbientBindings
          `shouldBe` DirectAmbientEstablished
        classify
          (ordinaryBodyRoute {bcrtvExteriorNode = pendingWrongExterior})
          provisionalAmbientBindings
          `shouldBe` DirectAmbientEstablished
        classify
          ( ordinaryBodyRoute
              { bcrtvConstructionRef = sameNamedRepresentativeRef
              }
          )
          provisionalAmbientBindings
          `shouldBe` DirectAmbientEstablished
        classify
          ordinaryBodyRoute
          ( Map.insert
              pendingRef
              (TestElab.tBase (BaseTy "Bool"))
              provisionalAmbientBindings
          )
          `shouldBe` DirectAmbientEstablished

      it "does not nominate an ordinary body exterior by type shape or a wrong identity" $ do
        let sameShapedPeerBindings =
              Map.fromList
                [ (bodyConstructionRef, TArrow TBottom TBottom)
                , (sameNamedRepresentativeRef, completedBodyBound)
                ]
            wrongSemanticRoute =
              ordinaryBodyRoute
                { bcrtvSemanticRef = sameNamedRepresentativeRef
                }
            classify route bindings =
              bodyConsumerRouteProjectionProvenanceForTest
                []
                bodyOwner
                ordinaryBodyClosures
                bodyRequirement
                route
                completedBodyBound
                bindings
        classify ordinaryBodyRoute sameShapedPeerBindings
          `shouldBe` DirectAmbientEstablished
        classify wrongSemanticRoute sameShapedPeerBindings
          `shouldBe` DirectAmbientEstablished

      it "requires distinct, exact ambient consumer and exterior declarations" $ do
        let coincidentRoute =
              ordinaryBodyRoute
                { bcrtvConstructionRef = pendingRef
                }
            missingConsumerBindings =
              Map.singleton pendingRef completedBodyBound
            mismatchedExteriorBindings =
              Map.insert
                pendingRef
                (TestElab.tBase (BaseTy "Bool"))
                ordinaryBodyBindings
            classify route bindings =
              bodyConsumerRouteProjectionProvenanceForTest
                []
                bodyOwner
                ordinaryBodyClosures
                bodyRequirement
                route
                completedBodyBound
                bindings
        classify coincidentRoute missingConsumerBindings
          `shouldBe` DirectAmbientEstablished
        classify ordinaryBodyRoute missingConsumerBindings
          `shouldBe` DirectAmbientEstablished
        classify ordinaryBodyRoute mismatchedExteriorBindings
          `shouldBe` DirectAmbientEstablished

      it "does not complete a structured consumer whose pending slot is already materialized" $ do
        let materializedScheme =
              Elab.schemeInfoFromRefSubst
                ( Elab.mkElabSchemeWithRefs
                    [ ( pendingRef
                      , Just (TestElab.tBase (BaseTy "Int"))
                      )
                    ]
                    TBottom
                )
                ( IntMap.fromList
                    [ (getNodeId pendingExterior, pendingRef)
                    , (getNodeId pendingResult, separateResultRef)
                    ]
                )
            materializedClosure =
              pendingClosure
                { lgcOwnerPendingScheme = Just materializedScheme
                }
        bodyConsumerProjectionProvenanceForTest
          []
          pendingOwner
          (pendingClosures materializedClosure)
          pendingRequirement
          pendingResult
          pendingRef
          (TArrow TBottom TBottom)
          `shouldBe` DirectAmbientEstablished

      it "does not transfer provisional result authority across owners" $ do
        let wrongOwner =
              pendingOwner
                { lgoConstructor = LocalLambdaGamma
                , lgoBoundaryEdge = pendingSiblingEdge
                }
            wrongClosure = pendingClosure {lgcOwner = wrongOwner}
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures wrongClosure)
          pendingRequirement
          pendingResult
          pendingRef
          TBottom
          `shouldBe` DirectAmbientEstablished

      it "does not transfer provisional result authority across edge sets" $ do
        let wrongClosure =
              pendingClosure
                { lgcEdgeIds =
                    pendingBoundaryEdge :| [pendingWrongEdge]
                }
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures wrongClosure)
          pendingRequirement
          pendingResult
          pendingRef
          TBottom
          `shouldBe` DirectAmbientEstablished

      it "does not transfer provisional result authority across exteriors" $ do
        let wrongClosure =
              pendingClosure
                { lgcExteriorNode = pendingWrongExterior
                , lgcConsumerIdentity =
                    typeBinderIdentityFromNode pendingWrongExterior
                }
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures wrongClosure)
          pendingRequirement
          pendingResult
          pendingRef
          TBottom
          `shouldBe` DirectAmbientEstablished

      it "requires the pending scheme's exact direct result route" $ do
        let wrongResultClosure =
              pendingClosure
                { lgcOwnerPendingScheme =
                    Just
                      ( pendingScheme
                          pendingRef
                          pendingWrongResult
                      )
                }
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures wrongResultClosure)
          pendingRequirement
          pendingResult
          pendingRef
          TBottom
          `shouldBe` DirectAmbientEstablished

      it "does not use representative equality or display names as result provenance" $ do
        let representative node
              | node == pendingWrongExterior = pendingExterior
              | otherwise = node
            sameNamedRouteClosure =
              pendingClosure
                { lgcOwnerPendingScheme =
                    Just
                      ( pendingScheme
                          sameNamedRepresentativeRef
                          pendingResult
                      )
                }
        representative pendingWrongExterior
          `shouldBe` representative pendingExterior
        classifyPending
          []
          pendingOwner
          Nothing
          (pendingClosures sameNamedRouteClosure)
          pendingRequirement
          pendingResult
          pendingRef
          TBottom
          `shouldBe` DirectAmbientEstablished

      it "does not refine an exact unbounded declaration from a bounded route" $ do
        let exactRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991769))
                "f"
            exactBound = TestElab.tBase (BaseTy "Int")
        selectDirectAmbientGammaAuthorityForTest
          exactRef
          [ (DirectAmbientEstablished, exactRef, TBottom)
          , (DirectAmbientEstablished, exactRef, exactBound)
          ]
          `shouldSatisfy` isLeft

      it "rejects two established bounds for one exact direct Gamma declaration" $ do
        let exactRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991771))
                "f"
            intTy = TestElab.tBase (BaseTy "Int")
            boolTy = TestElab.tBase (BaseTy "Bool")
        selectDirectAmbientGammaAuthorityForTest
          exactRef
          [ (DirectAmbientEstablished, exactRef, intTy)
          , (DirectAmbientEstablished, exactRef, boolTy)
          ]
          `shouldSatisfy` isLeft

      it "accepts only recursive representation equality at an operational endpoint" $ do
        let sourceSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991772))
                "source-self"
            sourceResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991773))
                "source-result"
            targetSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991774))
                "target-self"
            targetResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991775))
                "target-result"
            outerRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991776))
                "outer"
            completeChurchType =
              TMuRef
                sourceSelf
                ( TForallRef
                    sourceResult
                    Nothing
                    (TArrow (TVarRef sourceResult) (TVarRef sourceResult))
                )
            instantiatedChurchType =
              TMuRef
                targetSelf
                (TArrow (TVarRef targetResult) (TVarRef targetResult))
        operationalEndpointTypesAgreeForTest
          completeChurchType
          instantiatedChurchType
          `shouldBe` True
        operationalEndpointTypesAgreeForTest
          (TForallRef outerRef Nothing completeChurchType)
          completeChurchType
          `shouldBe` False

      it "finds a Church representation transition below a preserved recursive binder" $ do
        let outerSourceSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991791))
                "outer-source-self"
            outerSourceResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991792))
                "outer-source-result"
            innerSourceSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991793))
                "inner-source-self"
            innerSourceResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991794))
                "inner-source-result"
            outerTargetSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991795))
                "outer-target-self"
            outerTargetResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991796))
                "outer-target-result"
            innerTargetSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991797))
                "inner-target-self"
            innerTargetResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991798))
                "inner-target-result"
            sourceType =
              TMuRef outerSourceSelf $
                TForallRef outerSourceResult Nothing $
                  TArrow
                    ( TMuRef innerSourceSelf $
                        TForallRef innerSourceResult Nothing $
                          TArrow
                            (TVarRef innerSourceResult)
                            (TVarRef innerSourceResult)
                    )
                    (TVarRef outerSourceResult)
            targetType =
              TMuRef outerTargetSelf $
                TForallRef outerTargetResult Nothing $
                  TArrow
                    ( TMuRef innerTargetSelf $
                        TArrow
                          (TVarRef innerTargetResult)
                          (TVarRef innerTargetResult)
                    )
                    (TVarRef outerTargetResult)
        operationalEndpointTypesAgreeForTest sourceType targetType
          `shouldBe` True

      it "rejects two forall-preserving recursive result algebras" $ do
        let exactRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991777))
                "f"
            sourceSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991778))
                "source-self"
            sourceResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991779))
                "source-result"
            targetSelf =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991788))
                "target-self"
            targetResult =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991790))
                "target-result"
            sourceType =
              TMuRef
                sourceSelf
                ( TForallRef
                    sourceResult
                    Nothing
                    (TArrow (TVarRef sourceResult) (TVarRef sourceResult))
                )
            targetResultArrow =
              TArrow
                (TVarRef targetResult)
                (TVarRef targetResult)
            targetType =
              TMuRef
                targetSelf
                ( TForallRef
                    targetResult
                    Nothing
                    (TArrow targetResultArrow targetResultArrow)
                )
        operationalEndpointTypesAgreeForTest sourceType targetType
          `shouldBe` False
        selectDirectAmbientGammaAuthorityForTest
          exactRef
          [ (DirectAmbientEstablished, exactRef, sourceType)
          , (DirectAmbientEstablished, exactRef, targetType)
          ]
          `shouldSatisfy` isLeft

      it "inherits a nested application residual only through its exact result route" $ do
        let nestedEdge = EdgeId 991780
            outerEdge = EdgeId 991781
            nestedResult = NodeId 991782
            outerExterior = NodeId 991783
            outerOperated = NodeId 991784
            outerConstructionScope = GenRef (GenNodeId 991789)
            nestedOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = nestedEdge,
                  lgoTermNode = nestedResult,
                  lgoScope = GenRef (GenNodeId 991785)
                }
            firstEmittedRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991786))
                "first"
            secondEmittedRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991787))
                "second"
            outerRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode outerExterior)
                "outer"
            intTy = TestElab.tBase (BaseTy "Int")
            residualTy = TArrow intTy intTy
            intBoundTy = TestElab.tBase (BaseTy "Int")
            residualBoundTy = TArrow intBoundTy intBoundTy
            firstBoundTy = TArrow intBoundTy residualBoundTy
            emittedBinders =
              (firstEmittedRef, Just firstBoundTy)
                :| [(secondEmittedRef, Just intBoundTy)]
            constructedTy =
              TForallRef
                firstEmittedRef
                (Just firstBoundTy)
                ( TForallRef
                    secondEmittedRef
                    (Just intBoundTy)
                    residualTy
                )
            certificate =
              LocalGammaConstructionCertificate
                { lgccOwner = nestedOwner,
                  lgccConstructedType = constructedTy,
                  lgccConstruction = LocalGammaEmitted emittedBinders [],
                  lgccDirectApplicationSourceEdgeIds = nestedEdge :| [],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims = [],
                  lgccAmbientDeclarationAuthorities = [],
                  lgccLocalBinderRoutes =
                    IntMap.fromList
                      [ (getNodeId (NodeId 991786), firstEmittedRef),
                        (getNodeId (NodeId 991787), secondEmittedRef)
                      ],
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = []
                }
            outerRequirement =
              RequiredGammaBinder
                { rgbEdgeIds = outerEdge :| [],
                  rgbExteriorNode = outerExterior,
                  rgbOperatedRoot = outerOperated,
                  rgbResultRoots = nestedResult :| [],
                  rgbOperatedType = residualTy,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement =
                    RequiredGammaAtConstructionScope
                      outerConstructionScope
                }
            provisionalAuthority =
              AmbientGammaAuthority
                { agaExactRef = outerRef,
                  agaBound = TBottom
                }
            requirements =
              GeneralizationRequirements
                { grRequiredGammaBinders = [outerRequirement],
                  grSourceBinderRefs = IntMap.empty,
                  grAmbientBinderRefs = [outerRef],
                  grAmbientGammaAuthorities =
                    IntMap.fromList
                      [ (getNodeId nestedResult, provisionalAuthority),
                        (getNodeId outerExterior, provisionalAuthority)
                      ],
                  grLocallyClosedGammaNodes = IntSet.empty
                }
            inherit owner cert reqs =
              inheritNestedApplicationResidualAuthorityForTest
                owner
                cert
                reqs
            inheritReplay owner cert ownerReqs replayReqs =
              inheritNestedApplicationResidualReplayAuthorityForTest
                owner
                cert
                ownerReqs
                replayReqs

        case inherit nestedOwner certificate requirements of
          Left err ->
            expectationFailure
              ("nested application residual inheritance failed: " ++ show err)
          Right inherited -> do
            fmap agaBound
              ( IntMap.lookup
                  (getNodeId nestedResult)
                  (grAmbientGammaAuthorities inherited)
              )
              `shouldBe` Just residualTy
            fmap agaBound
              ( IntMap.lookup
                  (getNodeId outerExterior)
                  (grAmbientGammaAuthorities inherited)
              )
              `shouldBe` Just residualTy
            grRequiredGammaBinders inherited
              `shouldBe` [outerRequirement]

        case
            inheritReplay
              nestedOwner
              certificate
              requirements
              requirements
          of
            Left err ->
              expectationFailure
                ("nested application replay inheritance failed: " ++ show err)
            Right (inheritedOwner, inheritedReplay) -> do
              inheritedOwner `shouldBe` inheritedReplay
              fmap agaBound
                ( IntMap.lookup
                    (getNodeId nestedResult)
                    (grAmbientGammaAuthorities inheritedReplay)
                )
                `shouldBe` Just residualTy
              fmap agaBound
                ( IntMap.lookup
                    (getNodeId outerExterior)
                    (grAmbientGammaAuthorities inheritedReplay)
                )
                `shouldBe` Just residualTy

        let conflictingReplayAuthority =
              provisionalAuthority
                { agaBound = intTy
                }
            conflictingReplayRequirements =
              requirements
                { grAmbientGammaAuthorities =
                    IntMap.insert
                      (getNodeId nestedResult)
                      conflictingReplayAuthority
                      (grAmbientGammaAuthorities requirements)
                }
        inheritReplay
          nestedOwner
          certificate
          requirements
          conflictingReplayRequirements
          `shouldSatisfy` isLeft

        inherit
          (nestedOwner {lgoBoundaryEdge = EdgeId 991788})
          certificate
          requirements
          `shouldSatisfy` isLeft
        inherit
          nestedOwner
          certificate
          ( requirements
              { grAmbientGammaAuthorities =
                  IntMap.delete
                    (getNodeId nestedResult)
                    (grAmbientGammaAuthorities requirements)
              }
          )
          `shouldSatisfy` isLeft

        case
            inheritNestedApplicationZeroLocalResidualAuthorityForTest
              nestedOwner
              nestedOwner
              residualTy
              []
              IntMap.empty
              requirements
          of
            Left err ->
              expectationFailure
                ("zero-local nested application residual inheritance failed: " ++ show err)
            Right inherited ->
              fmap agaBound
                ( IntMap.lookup
                    (getNodeId nestedResult)
                    (grAmbientGammaAuthorities inherited)
                )
                `shouldBe` Just residualTy

        inheritNestedApplicationZeroLocalResidualAuthorityForTest
          nestedOwner
          (nestedOwner {lgoBoundaryEdge = EdgeId 991788})
          residualTy
          []
          IntMap.empty
          requirements
          `shouldSatisfy` isLeft

        inheritNestedApplicationZeroLocalResidualAuthorityForTest
          nestedOwner
          nestedOwner
          residualTy
          [firstEmittedRef]
          IntMap.empty
          requirements
          `shouldSatisfy` isLeft

        inheritNestedApplicationZeroLocalResidualAuthorityForTest
          nestedOwner
          nestedOwner
          residualTy
          []
          (IntMap.singleton (getNodeId nestedResult) firstEmittedRef)
          requirements
          `shouldSatisfy` isLeft

      it "inherits only the exact descendant Gamma in a chained application" $ do
        let descendantEdge = EdgeId 991800
            descendantArgEdge = EdgeId 991801
            currentEdge = EdgeId 991802
            currentArgEdge = EdgeId 991803
            unrelatedEdge = EdgeId 991804
            descendantTerm = NodeId 991805
            currentTerm = NodeId 991806
            unrelatedTerm = NodeId 991807
            descendantScope = GenRef (GenNodeId 991808)
            currentScope = GenRef (GenNodeId 991809)
            unrelatedScope = GenRef (GenNodeId 991810)
            descendantExterior = NodeId 7
            descendantOperated = NodeId 5
            descendantResult = NodeId 11
            currentExterior = NodeId 61
            currentOperated = NodeId 12
            currentResult = NodeId 13
            descendantBound = TestElab.tBase (BaseTy "Int")
            currentBound = TestElab.tBase (BaseTy "String")
            descendantOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = descendantEdge,
                  lgoTermNode = descendantTerm,
                  lgoScope = descendantScope
                }
            currentOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = currentEdge,
                  lgoTermNode = currentTerm,
                  lgoScope = currentScope
                }
            unrelatedOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = unrelatedEdge,
                  lgoTermNode = unrelatedTerm,
                  lgoScope = unrelatedScope
                }
            descendantClosure =
              LocalGammaClosure
                { lgcEdgeIds = descendantEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcExteriorNode = descendantExterior,
                  lgcConsumerIdentity =
                    typeBinderIdentityFromNode descendantExterior,
                  lgcOwner = descendantOwner,
                  lgcOwnerPendingScheme = Nothing
                }
            unrelatedClosure =
              LocalGammaClosure
                { lgcEdgeIds = unrelatedEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcExteriorNode = descendantExterior,
                  lgcConsumerIdentity =
                    typeBinderIdentityFromNode descendantExterior,
                  lgcOwner = unrelatedOwner,
                  lgcOwnerPendingScheme = Nothing
                }
            descendantRequirement =
              RequiredGammaBinder
                { rgbEdgeIds = descendantEdge :| [],
                  rgbExteriorNode = descendantExterior,
                  rgbOperatedRoot = descendantOperated,
                  rgbResultRoots = descendantResult :| [],
                  rgbOperatedType = descendantBound,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
            currentRequirement =
              RequiredGammaBinder
                { rgbEdgeIds = currentEdge :| [],
                  rgbExteriorNode = currentExterior,
                  rgbOperatedRoot = currentOperated,
                  rgbResultRoots = currentResult :| [],
                  rgbOperatedType = currentBound,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
            currentRequirements =
              GeneralizationRequirements
                { grRequiredGammaBinders = [currentRequirement],
                  grSourceBinderRefs = IntMap.empty,
                  grAmbientBinderRefs = [],
                  grAmbientGammaAuthorities = IntMap.empty,
                  grLocallyClosedGammaNodes = IntSet.empty
                }
            descendantAnn =
              AApp
                (ALit (LInt 1) (NodeId 991811))
                (ALit (LInt 2) (NodeId 991812))
                (mkInstantiationSite descendantEdge (NodeId 991811) descendantTerm)
                (mkInstantiationSite descendantArgEdge (NodeId 991812) descendantTerm)
                descendantTerm
            applicationAnn =
              AApp
                descendantAnn
                (ALit (LInt 3) (NodeId 991813))
                (mkInstantiationSite currentEdge descendantTerm currentTerm)
                (mkInstantiationSite currentArgEdge (NodeId 991813) currentTerm)
                currentTerm
            closures =
              IntMap.fromList
                [ (getEdgeId descendantEdge, descendantClosure),
                  (getEdgeId unrelatedEdge, unrelatedClosure)
                ]
            scopeForBoundary edgeId termNode
              | edgeId == descendantEdge
                  && termNode == descendantTerm =
                  Right descendantScope
              | edgeId == currentEdge
                  && termNode == currentTerm =
                  Right currentScope
              | otherwise =
                  Left
                    ( ValidationFailed
                        [ "unexpected application boundary in chained Gamma fixture"
                        , "  edge: " ++ show edgeId
                        , "  term: " ++ show termNode
                        ]
                    )
            selected =
              retainedDescendantGammaClosures
                scopeForBoundary
                currentOwner
                (IntSet.singleton (getNodeId descendantExterior))
                closures
                applicationAnn

        case selected of
          Left err ->
            expectationFailure
              ("descendant Gamma selection failed: " ++ show err)
          Right selectedClosures -> do
            selectedClosures `shouldBe` [descendantClosure]
            case
                inheritDescendantGammaRequirements
                  selectedClosures
                  [descendantRequirement]
                  currentRequirements
              of
                Left err ->
                  expectationFailure
                    ("descendant Gamma inheritance failed: " ++ show err)
                Right inherited -> do
                  grRequiredGammaBinders inherited
                    `shouldBe` [currentRequirement]
                  IntMap.keys (grAmbientGammaAuthorities inherited)
                    `shouldBe` [getNodeId descendantExterior]
                  IntMap.member
                    (getNodeId descendantOperated)
                    (grAmbientGammaAuthorities inherited)
                    `shouldBe` False
                  IntMap.member
                    (getNodeId descendantResult)
                    (grAmbientGammaAuthorities inherited)
                    `shouldBe` False
                  IntSet.toList (grLocallyClosedGammaNodes inherited)
                    `shouldBe` [getNodeId descendantExterior]
                  case
                      IntMap.lookup
                        (getNodeId descendantExterior)
                        (grAmbientGammaAuthorities inherited)
                    of
                      Nothing ->
                        expectationFailure
                          "descendant exterior has no ambient Gamma authority"
                      Just authority -> do
                        agaBound authority `shouldBe` descendantBound
                        typeBinderRefIdentity (agaExactRef authority)
                          `shouldBe` lgcConsumerIdentity descendantClosure
                  grAmbientBinderRefs inherited
                    `shouldSatisfy` \refs ->
                      length refs == 1
                        && any
                          ( (== lgcConsumerIdentity descendantClosure)
                              . typeBinderRefIdentity
                          )
                          refs

      it "retains an exact application owner over only a provisional root bottom slot" $ do
        let localEdge = EdgeId 991820
            argumentEdge = EdgeId 991819
            rootEdge = EdgeId 991821
            exterior = NodeId 991822
            operated = NodeId 991823
            localResult = NodeId 991824
            rootResult = NodeId 991825
            ownerScope = GenRef (GenNodeId 991826)
            owner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = localEdge,
                  lgoTermNode = NodeId 991827,
                  lgoScope = ownerScope
                }
            closure =
              LocalGammaClosure
                { lgcEdgeIds = localEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcExteriorNode = exterior,
                  lgcConsumerIdentity = typeBinderIdentityFromNode exterior,
                  lgcOwner = owner,
                  lgcOwnerPendingScheme = Nothing
                }
            localRequirement =
              RequiredGammaBinder
                { rgbEdgeIds = localEdge :| [],
                  rgbExteriorNode = exterior,
                  rgbOperatedRoot = operated,
                  rgbResultRoots = localResult :| [],
                  rgbOperatedType = TArrow TBottom TBottom,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
            provisionalRootRequirement =
              RequiredGammaBinder
                { rgbEdgeIds = rootEdge :| [],
                  rgbExteriorNode = exterior,
                  rgbOperatedRoot = operated,
                  rgbResultRoots = rootResult :| [],
                  rgbOperatedType = TBottom,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
            ga =
              GaBindParents
                { gaBindParentsBase =
                    IntMap.singleton
                      (nodeRefKey (typeRef exterior))
                      (ownerScope, BindFlex),
                  gaBaseConstraint =
                    toPresolvedConstraint
                      (toAcyclicConstraint (toNormalizedConstraint emptyConstraint)),
                  gaBaseToSolved = IntMap.empty,
                  gaSolvedToBase = IntMap.empty,
                  gaRestoredSchemeRootTargets = IntMap.empty,
                  gaExpansionConstructionPlacements =
                    emptyExpansionConstructionPlacements
                }
            exactOwner =
              exactApplicationClosureOwnsRequirementForTest
                ga
                [provisionalRootRequirement]
                closure
        exactOwner localRequirement `shouldBe` True
        exactOwner
          ( localRequirement
              { rgbEdgeIds = argumentEdge :| []
              }
          )
          `shouldBe` False
        exactApplicationClosureOwnsRequirementForTest
          ga
          [provisionalRootRequirement]
          (closure {lgcEdgeIds = argumentEdge :| []})
          (localRequirement {rgbEdgeIds = argumentEdge :| []})
          `shouldBe` True
        exactOwner
          ( localRequirement
              { rgbEdgeIds = EdgeId 991828 :| []
              }
          )
          `shouldBe` False
        exactApplicationClosureOwnsRequirementForTest
          ga
          [ ( provisionalRootRequirement
                { rgbOperatedType = TArrow TBottom TBottom
                }
            )
          ]
          closure
          localRequirement
          `shouldBe` False
        exactApplicationClosureOwnsRequirementForTest
          ga
          [provisionalRootRequirement]
          ( closure
              { lgcOwner =
                  owner
                    { lgoConstructor = LocalLambdaGamma
                    }
              }
          )
          localRequirement
          `shouldBe` False
        exactApplicationClosureOwnsRequirementForTest
          ga
          [provisionalRootRequirement]
          ( closure
              { lgcOwner =
                  owner
                    { lgoScope = GenRef (GenNodeId 991829)
                    }
              }
          )
          localRequirement
          `shouldBe` False

      it "discharges a root requirement only through one complete exact application certificate" $ do
        let applicationEdge = EdgeId 991830
            rootEdge = EdgeId 991831
            exterior = NodeId 991832
            operated = NodeId 991833
            alternateResult = NodeId 991838
            ownerScope = GenRef (GenNodeId 991834)
            emittedRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode exterior)
                "root-local"
            unrelatedRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991835))
                "unrelated"
            operatedType = TestElab.tBase (BaseTy "SeedSpan")
            owner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = applicationEdge,
                  lgoTermNode = NodeId 991836,
                  lgoScope = ownerScope
                }
            requirement =
              RequiredGammaBinder
                { rgbEdgeIds = rootEdge :| [],
                  rgbExteriorNode = exterior,
                  rgbOperatedRoot = operated,
                  rgbResultRoots = operated :| [],
                  rgbOperatedType = operatedType,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
            provisionalLambdaClosure =
              LocalGammaClosure
                { lgcEdgeIds = rootEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcExteriorNode = exterior,
                  lgcConsumerIdentity =
                    typeBinderIdentityFromNode exterior,
                  lgcOwner =
                    owner
                      { lgoConstructor = LocalLambdaGamma,
                        lgoBoundaryEdge = rootEdge
                      },
                  lgcOwnerPendingScheme = Nothing
                }
            alternatePlannerRequirement =
              requirement
                { rgbResultRoots = alternateResult :| []
                }
            specializedPlannerRequirement =
              alternatePlannerRequirement
                { rgbOperatedType =
                    TArrow operatedType operatedType
                }
            provisionalResultRequirement =
              requirement
                { rgbEdgeIds = EdgeId 991839 :| [],
                  rgbOperatedRoot = NodeId 991844,
                  rgbResultRoots = lgoTermNode owner :| [],
                  rgbOperatedType = TBottom
                }
            ambientExterior = NodeId 991846
            ambientOperated = NodeId 991847
            ambientRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode ambientExterior)
                "ambient"
            ambientRequirement =
              RequiredGammaBinder
                { rgbEdgeIds = applicationEdge :| [],
                  rgbExteriorNode = ambientExterior,
                  rgbOperatedRoot = ambientOperated,
                  rgbResultRoots = ambientOperated :| [],
                  rgbOperatedType = operatedType,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
        operatedBound <- requireRight (elabToBound operatedType)
        let certificate =
              LocalGammaConstructionCertificate
                { lgccOwner = owner,
                  lgccConstructedType =
                    TForallRef emittedRef (Just operatedBound) TBottom,
                  lgccConstruction =
                    LocalGammaEmitted
                      ((emittedRef, Just operatedBound) :| [])
                      [],
                  lgccDirectApplicationSourceEdgeIds =
                    applicationEdge :| [],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims = [],
                  lgccAmbientDeclarationAuthorities = [],
                  lgccLocalBinderRoutes =
                    IntMap.fromList
                      [ (getNodeId exterior, emittedRef),
                        (getNodeId operated, emittedRef)
                      ],
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = []
                }
            directClaim =
              DirectApplicationGammaClaim
                { dagcEdgeIds = rootEdge :| [],
                  dagcExteriorNode = exterior,
                  dagcOperatedRoot = operated,
                  dagcConstructionResultRoots = operated :| [],
                  dagcOperatedType = operatedType,
                  dagcBinderRef = emittedRef,
                  dagcConstructedBound = Just operatedBound
                }
            directCertificate =
              certificate
                { lgccDirectApplicationSourceEdgeIds =
                    applicationEdge :| [rootEdge],
                  lgccDirectApplicationGammaClaims = [directClaim],
                  lgccLocalBinderRoutes =
                    IntMap.insert
                      (getNodeId alternateResult)
                      emittedRef
                      (lgccLocalBinderRoutes certificate)
                }
            resultCertificate =
              directCertificate
                { lgccDirectApplicationGammaClaims =
                    [ directClaim
                        { dagcConstructionResultRoots =
                            lgoTermNode owner :| []
                        }
                    ],
                  lgccLocalBinderRoutes =
                    IntMap.insert
                      (getNodeId (lgoTermNode owner))
                      emittedRef
                      (lgccLocalBinderRoutes directCertificate)
                }
            ambientClaim =
              DirectApplicationAmbientGammaClaim
                { daagcEdgeIds = applicationEdge :| [],
                  daagcExteriorNode = ambientExterior,
                  daagcOperatedRoot = ambientOperated,
                  daagcConstructionResultRoots = ambientOperated :| [],
                  daagcOperatedType = operatedType,
                  daagcAmbientRef = ambientRef,
                  daagcAmbientBound = operatedType
                }
            ambientCertificate =
              LocalGammaConstructionCertificate
                { lgccOwner = owner,
                  lgccConstructedType = operatedType,
                  lgccConstruction = LocalGammaAmbient,
                  lgccDirectApplicationSourceEdgeIds =
                    applicationEdge :| [],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims = [ambientClaim],
                  lgccAmbientDeclarationAuthorities =
                    [AmbientGammaAuthority ambientRef operatedType],
                  lgccLocalBinderRoutes = IntMap.empty,
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = []
                }
            consumedCertificate =
              certificate
                { lgccConstructedType = TBottom,
                  lgccConstruction =
                    LocalGammaConsumed
                      ((emittedRef, Just operatedBound) :| [])
                }
            directConsumedCertificate =
              directCertificate
                { lgccConstructedType = TBottom,
                  lgccConstruction =
                    LocalGammaConsumed
                      ((emittedRef, Just operatedBound) :| [])
                }
            owns =
              applicationCertificateOwnsRootRequirementForTest
                ownerScope
            foreignRootScope = GenRef (GenNodeId 991837)
            ownsFromForeignRoot =
              applicationCertificateOwnsRootRequirementForTest
                foreignRootScope
            scopeRequirement =
              requirement
                { rgbEdgeIds = EdgeId 991840 :| []
                }
            mixedRequirement =
              requirement
                { rgbEdgeIds = rootEdge :| [EdgeId 991845]
                }
            applicationAnn =
              AApp
                (ALit (LInt 1) (NodeId 991841))
                (ALit (LInt 2) (NodeId 991842))
                ( mkInstantiationSite
                    applicationEdge
                    (NodeId 991841)
                    (lgoTermNode owner)
                )
                ( mkInstantiationSite
                    rootEdge
                    (NodeId 991842)
                    (lgoTermNode owner)
                )
                (lgoTermNode owner)
            scopeForBoundary edgeId termNode
              | edgeId == applicationEdge
                  && termNode == lgoTermNode owner =
                  Right ownerScope
              | otherwise =
                  Left
                    ( ValidationFailed
                        [ "unexpected direct-certificate boundary"
                        , "  edge: " ++ show edgeId
                        , "  term: " ++ show termNode
                        ]
                    )
        owns certificate requirement `shouldBe` True
        owns consumedCertificate requirement `shouldBe` True
        owns
          ( certificate
              { lgccDirectApplicationSourceEdgeIds =
                  applicationEdge :| [rootEdge]
              }
          )
          requirement
          `shouldBe` False
        ownsFromForeignRoot directCertificate requirement `shouldBe` True
        ownsFromForeignRoot directConsumedCertificate requirement `shouldBe` True
        ownsFromForeignRoot
          ( directCertificate
              { lgccLocalBinderRoutes =
                  IntMap.delete
                    (getNodeId operated)
                    (lgccLocalBinderRoutes directCertificate)
              }
          )
          requirement
          `shouldBe` False
        ownsFromForeignRoot
          ( directCertificate
              { lgccLocalBinderRoutes =
                  IntMap.insert
                    (getNodeId operated)
                    unrelatedRef
                    (lgccLocalBinderRoutes directCertificate)
              }
          )
          requirement
          `shouldBe` False
        owns directCertificate scopeRequirement `shouldBe` True
        owns directCertificate mixedRequirement `shouldBe` True
        ownsFromForeignRoot directCertificate mixedRequirement `shouldBe` False
        ownsFromForeignRoot
          directCertificate
          alternatePlannerRequirement
          `shouldBe` True
        applicationCertificateDirectClaimOwnsPlanningRequirementForTest
          directCertificate
          specializedPlannerRequirement
          `shouldBe` True
        applicationCertificateDirectClaimOwnsPlanningRequirementForTest
          directCertificate
          ( specializedPlannerRequirement
              { rgbOperatedRoot = alternateResult
              }
          )
          `shouldBe` False
        applicationCertificateCompletesProvisionalResultRequirementForTest
          resultCertificate
          provisionalResultRequirement
          `shouldBe` True
        applicationCertificateCompletesProvisionalResultRequirementForTest
          resultCertificate
          ( provisionalResultRequirement
              { rgbResultRoots = alternateResult :| []
              }
          )
          `shouldBe` False
        applicationCertificateCompletesProvisionalResultRequirementForTest
          resultCertificate
          ( provisionalResultRequirement
              { rgbOperatedType = operatedType
              }
          )
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          ambientCertificate
          ambientRequirement
          `shouldBe` True
        applicationCertificateOwnsAmbientRootRequirementForTest
          ( ambientCertificate
              { lgccLocalBinderRoutes =
                  IntMap.insert
                    (getNodeId ambientExterior)
                    emittedRef
                    (lgccLocalBinderRoutes ambientCertificate)
              }
          )
          ambientRequirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          ( ambientCertificate
              { lgccDirectApplicationAmbientGammaClaims =
                  [ ambientClaim
                      { daagcOperatedRoot = alternateResult
                      }
                  ]
              }
          )
          ambientRequirement
          `shouldBe` False
        applicationCertificateTransfersRootRequirementOwnershipForTest
          foreignRootScope
          directCertificate
          requirement
          alternatePlannerRequirement
          `shouldBe` True
        applicationCertificateTransfersRootRequirementOwnershipForTest
          foreignRootScope
          ( directCertificate
              { lgccLocalBinderRoutes =
                  IntMap.delete
                    (getNodeId alternateResult)
                    (lgccLocalBinderRoutes directCertificate)
              }
          )
          requirement
          alternatePlannerRequirement
          `shouldBe` True
        ownsFromForeignRoot
          ( directCertificate
              { lgccLocalBinderRoutes =
                  IntMap.delete
                    (getNodeId alternateResult)
                    (lgccLocalBinderRoutes directCertificate)
              }
          )
          alternatePlannerRequirement
          `shouldBe` False
        ownsFromForeignRoot
          directCertificate
          (requirement {rgbEdgeIds = rootEdge :| [applicationEdge]})
          `shouldBe` False
        ownsFromForeignRoot
          ( directCertificate
              { lgccDirectApplicationGammaClaims =
                  [directClaim {dagcEdgeIds = applicationEdge :| []}]
              }
          )
          requirement
          `shouldBe` False
        ownsFromForeignRoot
          ( directCertificate
              { lgccDirectApplicationGammaClaims =
                  [directClaim, directClaim]
              }
          )
          requirement
          `shouldBe` False
        ownsFromForeignRoot
          ( directCertificate
              { lgccDirectApplicationGammaClaims =
                  [ directClaim
                      { dagcOperatedRoot = alternateResult
                      }
                  ]
              }
          )
          requirement
          `shouldBe` False
        applicationCertificateDischargesRootClosureForTest
          foreignRootScope
          [requirement]
          provisionalLambdaClosure
          directCertificate
          `shouldBe` True
        applicationCertificateDischargesRootClosureForTest
          foreignRootScope
          [requirement]
          ( provisionalLambdaClosure
              { lgcEdgeIds = EdgeId 991844 :| []
              }
          )
          directCertificate
          `shouldBe` False
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [directCertificate]
          `shouldSatisfy` isRight
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ directCertificate
              { lgccDirectApplicationSourceEdgeIds =
                  applicationEdge :| []
              }
          ]
          `shouldSatisfy` isLeft
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ directCertificate
              { lgccDirectApplicationGammaClaims =
                  [ directClaim
                      { dagcEdgeIds = EdgeId 991843 :| []
                      }
                  ]
              }
          ]
          `shouldSatisfy` isLeft
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ directCertificate
              { lgccDirectApplicationGammaClaims =
                  [directClaim, directClaim]
              }
          ]
          `shouldSatisfy` isLeft
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ directCertificate
              { lgccDirectApplicationGammaClaims =
                  [ directClaim,
                    directClaim
                      { dagcEdgeIds =
                          rootEdge :| [applicationEdge]
                      }
                  ]
              }
          ]
          `shouldSatisfy` isLeft
        applicationCertificateTransfersRootRequirementOwnershipForTest
          ownerScope
          certificate
          requirement
          alternatePlannerRequirement
          `shouldBe` True
        applicationCertificateTransfersRootRequirementOwnershipForTest
          ownerScope
          consumedCertificate
          requirement
          alternatePlannerRequirement
          `shouldBe` True
        applicationCertificateTransfersRootRequirementOwnershipForTest
          ownerScope
          certificate
          requirement
          (alternatePlannerRequirement {rgbOperatedRoot = alternateResult})
          `shouldBe` False
        applicationCertificateTransfersRootRequirementOwnershipForTest
          ownerScope
          certificate
          requirement
          ( alternatePlannerRequirement
              { rgbOperatedType = TestElab.tBase (BaseTy "Bool")
              }
          )
          `shouldBe` False
        applicationCertificateTransfersRootRequirementOwnershipForTest
          ownerScope
          certificate
          requirement
          ( alternatePlannerRequirement
              { rgbEdgeIds = EdgeId 991839 :| []
              }
          )
          `shouldBe` False
        applicationCertificateTransfersRootRequirementOwnershipForTest
          ownerScope
          certificate
          requirement
          ( alternatePlannerRequirement
              { rgbEdgeIds =
                  rootEdge :| [EdgeId 991839]
              }
          )
          `shouldBe` False
        applicationCertificateDischargesRootClosureForTest
          ownerScope
          [requirement]
          provisionalLambdaClosure
          certificate
          `shouldBe` True
        applicationCertificateDischargesRootClosureForTest
          ownerScope
          [requirement]
          provisionalLambdaClosure
          consumedCertificate
          `shouldBe` True
        owns
          ( certificate
              { lgccLocalBinderRoutes =
                  IntMap.delete
                    (getNodeId operated)
                    (lgccLocalBinderRoutes certificate)
              }
          )
          requirement
          `shouldBe` False
        owns
          ( certificate
              { lgccLocalBinderRoutes =
                  IntMap.insert
                    (getNodeId operated)
                    unrelatedRef
                    (lgccLocalBinderRoutes certificate)
              }
          )
          requirement
          `shouldBe` False
        owns
          certificate
          (requirement {rgbOperatedType = TestElab.tBase (BaseTy "Bool")})
          `shouldBe` False
        applicationCertificateOwnsRootRequirementForTest
          (GenRef (GenNodeId 991837))
          certificate
          requirement
          `shouldBe` False
        owns
          ( certificate
              { lgccOwner =
                  owner {lgoConstructor = LocalLambdaGamma}
              }
          )
          requirement
          `shouldBe` False

      it "does not let a zero-local ambient application displace an enclosing structural closure" $ do
        let applicationEdge = EdgeId 991847
            siblingEdge = EdgeId 991855
            exterior = NodeId 991848
            operated = NodeId 991849
            applicationScope = GenRef (GenNodeId 991850)
            dependencyRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991856))
                "dependency"
            operatedType =
              TArrow
                (tVarWithRef dependencyRef)
                (tVarWithRef dependencyRef)
            ambientRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode exterior)
                "ambient"
            wrongAmbientRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId 991852))
                "wrong-ambient"
            ga =
              GaBindParents
                { gaBindParentsBase = IntMap.empty,
                  gaBaseConstraint =
                    toPresolvedConstraint
                      (toAcyclicConstraint (toNormalizedConstraint emptyConstraint)),
                  gaBaseToSolved = IntMap.empty,
                  gaSolvedToBase = IntMap.empty,
                  gaRestoredSchemeRootTargets = IntMap.empty,
                  gaExpansionConstructionPlacements =
                    emptyExpansionConstructionPlacements
                }
            applicationOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = applicationEdge,
                  lgoTermNode = NodeId 991851,
                  lgoScope = applicationScope
                }
            requirement =
              RequiredGammaBinder
                { rgbEdgeIds = applicationEdge :| [],
                  rgbExteriorNode = exterior,
                  rgbOperatedRoot = operated,
                  rgbResultRoots = operated :| [],
                  rgbOperatedType = operatedType,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtCurrentScope
                }
            structuralClosure =
              LocalGammaClosure
                { lgcEdgeIds = applicationEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcExteriorNode = exterior,
                  lgcConsumerIdentity =
                    typeBinderIdentityFromNode exterior,
                  lgcOwner =
                    applicationOwner
                      { lgoConstructor = LocalLambdaGamma
                      },
                  lgcOwnerPendingScheme = Nothing
                }
            ambientClaim =
              DirectApplicationAmbientGammaClaim
                { daagcEdgeIds = applicationEdge :| [],
                  daagcExteriorNode = exterior,
                  daagcOperatedRoot = operated,
                  daagcConstructionResultRoots = operated :| [],
                  daagcOperatedType = operatedType,
                  daagcAmbientRef = ambientRef,
                  daagcAmbientBound = operatedType
                }
            ambientCertificate =
              LocalGammaConstructionCertificate
                { lgccOwner = applicationOwner,
                  lgccConstructedType = operatedType,
                  lgccConstruction = LocalGammaAmbient,
                  lgccDirectApplicationSourceEdgeIds =
                    applicationEdge :| [siblingEdge],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims =
                    [ambientClaim],
                  lgccAmbientDeclarationAuthorities =
                    [AmbientGammaAuthority ambientRef operatedType],
                  lgccLocalBinderRoutes = IntMap.empty,
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = [dependencyRef]
                }
            wrongBoundCertificate =
              ambientCertificate
                { lgccDirectApplicationAmbientGammaClaims =
                    [ambientClaim {daagcAmbientBound = TBottom}],
                  lgccAmbientDeclarationAuthorities =
                    [AmbientGammaAuthority ambientRef TBottom]
                }
            applicationAnn =
              AApp
                (ALit (LInt 1) (NodeId 991857))
                (ALit (LInt 2) (NodeId 991858))
                ( mkInstantiationSite
                    applicationEdge
                    (NodeId 991857)
                    (lgoTermNode applicationOwner)
                )
                ( mkInstantiationSite
                    siblingEdge
                    (NodeId 991858)
                    (lgoTermNode applicationOwner)
                )
                (lgoTermNode applicationOwner)
            scopeForBoundary edgeId termNode
              | edgeId == applicationEdge
                  && termNode == lgoTermNode applicationOwner =
                  Right applicationScope
              | otherwise =
                  Left
                    ( ValidationFailed
                        [ "unexpected ambient-certificate boundary"
                        , "  edge: " ++ show edgeId
                        , "  term: " ++ show termNode
                        ]
                    )
        applicationCertificateOwnsAmbientRootRequirementForTest
          ambientCertificate
          requirement
          `shouldBe` True
        applicationCertificateOwnsAmbientRootRequirementForTest
          wrongBoundCertificate
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          ( ambientCertificate
              { lgccDirectApplicationAmbientGammaClaims =
                  [ambientClaim {daagcAmbientRef = wrongAmbientRef}]
              }
          )
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          (ambientCertificate {lgccUsedAmbientBinderRefs = []})
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          (ambientCertificate {lgccUsedAmbientBinderRefs = [ambientRef]})
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          (ambientCertificate {lgccAmbientDeclarationAuthorities = []})
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          ( ambientCertificate
              { lgccAmbientDeclarationAuthorities =
                  [ AmbientGammaAuthority
                      wrongAmbientRef
                      operatedType
                  ]
              }
          )
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          ( ambientCertificate
              { lgccAmbientDeclarationAuthorities =
                  [ AmbientGammaAuthority ambientRef operatedType
                  , AmbientGammaAuthority ambientRef operatedType
                  ]
              }
          )
          requirement
          `shouldBe` False
        applicationCertificateOwnsAmbientRootRequirementForTest
          ( ambientCertificate
              { lgccConstruction =
                  LocalGammaConsumed
                    ((ambientRef, Nothing) :| [])
              }
          )
          requirement
          `shouldBe` False
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ambientCertificate]
          `shouldSatisfy` isRight
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ambientCertificate {lgccUsedAmbientBinderRefs = []}]
          `shouldSatisfy` isLeft
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ambientCertificate {lgccAmbientDeclarationAuthorities = []}]
          `shouldSatisfy` isLeft
        validateLocalApplicationCertificatesForTest
          scopeForBoundary
          applicationAnn
          IntMap.empty
          [ ambientCertificate
              { lgccLocalBinderRoutes =
                  IntMap.singleton (getNodeId exterior) ambientRef
              }
          ]
          `shouldSatisfy` isLeft
        applicationCertificateDischargesRootClosureForTest
          applicationScope
          [requirement]
          structuralClosure
          ambientCertificate
          `shouldBe` False
        rootRequirementOwnershipAllowsLocalGammaClosureForTest
          ga
          applicationScope
          [ambientCertificate]
          [requirement]
          [requirement]
          structuralClosure
          `shouldBe` True
        rootRequirementOwnershipAllowsLocalGammaClosureForTest
          ga
          applicationScope
          [wrongBoundCertificate]
          [requirement]
          [requirement]
          structuralClosure
          `shouldBe` False

    describe "owner-final root construction certificate" $ do
      let ambientNode = NodeId 991840
          plannedLocalNode = NodeId 991841
          exteriorNode = NodeId 991842
          emittedLocalNode = NodeId 991843
          edgeId = EdgeId 991844
          ownerScope = GenRef (GenNodeId 991845)
          owner =
            LocalGammaOwner
              { lgoConstructor = LocalLambdaGamma,
                lgoBoundaryEdge = edgeId,
                lgoTermNode = NodeId 991846,
                lgoScope = ownerScope
              }
          graphRef node name =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode node)
              name
          ambientRef = graphRef ambientNode "ambient"
          plannedLocalRef = graphRef plannedLocalNode "planned-local"
          consumerRef = graphRef exteriorNode "consumer"
          emittedLocalRef = graphRef emittedLocalNode "emitted-local"
          plannedScheme =
            mkElabSchemeWithRefs
              [(ambientRef, Nothing), (plannedLocalRef, Nothing)]
              (TArrow (tVarWithRef ambientRef) (tVarWithRef plannedLocalRef))
          constructedType =
            TForallRef
              emittedLocalRef
              Nothing
              (TArrow (tVarWithRef emittedLocalRef) (tVarWithRef emittedLocalRef))
          closure =
            LocalGammaClosure
              { lgcEdgeIds = edgeId :| [],
                lgcDirectApplicationEdgeIds = [],
                lgcExteriorNode = exteriorNode,
                lgcConsumerIdentity = typeBinderIdentityFromNode exteriorNode,
                lgcOwner = owner,
                lgcOwnerPendingScheme = Nothing
              }
          rootSubst =
            IntMap.singleton
              (getNodeId exteriorNode)
              plannedLocalRef
          certificate =
            OwnerFinalConstruction
              { ofcOwner = owner,
                ofcConstructedType = constructedType,
                ofcLocallyEmittedBinderRefs = [emittedLocalRef],
                ofcLocalBinderRoutes =
                  IntMap.singleton
                    (getNodeId plannedLocalNode)
                    emittedLocalRef,
                ofcUsedAmbientBinderRefs = [],
                ofcBodyConsumerBoundRefinements = []
              }
          expectCertificateFailure label result =
            case result of
              Left _ -> pure ()
              Right closed ->
                expectationFailure
                  ("expected " ++ label ++ " rejection, got " ++ show closed)

      it "rejects a same-spelled ordinary free identity at the no-local root closure boundary" $ do
        let declaredRef = graphRef (NodeId 991837) "a"
            escapedRef = graphRef (NodeId 991838) "a"
        case
            prepareRootClosureSchemeForTest
              IntMap.empty
              (mkElabSchemeWithRefs [(declaredRef, Nothing)] (tVarWithRef escapedRef))
          of
            Left (ValidationFailed messages) ->
              messages `shouldSatisfy` any (isInfixOf "prepared root closure")
            Left other ->
              expectationFailure
                ("expected root scheme-closure validation, got " ++ show other)
            Right closed ->
              expectationFailure
                ("expected unauthorized free identity rejection, got " ++ show closed)

      it "keeps a pending local Gamma exterior out of the provisional exact-root scope" $ do
        let scopeFor closures =
              prepareProvisionalLocalGammaRootConstructionScopeForTest
                closures
                rootSubst
                plannedScheme
            (localBinders, localAliases) = scopeFor [closure]
            (wholeRootBinders, wholeRootAliases) = scopeFor []
            wrongExterior = NodeId 991847
            sameNamedWrongRef = graphRef wrongExterior "planned-local"
            wrongClosure =
              closure
                { lgcExteriorNode = wrongExterior,
                  lgcConsumerIdentity =
                    typeBinderRefIdentity sameNamedWrongRef
                }
            (wrongBinders, wrongAliases) = scopeFor [wrongClosure]
            ambientAliases =
              IntMap.singleton
                (getNodeId ambientNode)
                ambientRef
            wholeAliases =
              IntMap.fromList
                [ (getNodeId ambientNode, ambientRef),
                  (getNodeId plannedLocalNode, plannedLocalRef),
                  (getNodeId exteriorNode, plannedLocalRef)
                ]
        map fst localBinders `shouldBe` [ambientRef]
        -- Only the exact exterior/construction identity is local.  The
        -- unrelated ambient binder keeps its own root route.
        localAliases `shouldBe` ambientAliases
        map fst wholeRootBinders
          `shouldBe` [ambientRef, plannedLocalRef]
        wholeRootAliases `shouldBe` wholeAliases
        map fst wrongBinders
          `shouldBe` [ambientRef, plannedLocalRef]
        wrongAliases `shouldBe` wholeAliases

      it "excludes the exact packet-owned construction endpoint rather than its semantic exterior" $ do
        let dependencyRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991848))
                "$typevar#8"
            operatedNode = NodeId 991849
            resultNode = NodeId 991850
            conflictingResultNode = NodeId 991851
            conflictingRef = graphRef (NodeId 991852) "planned-local"
            dependencyRouteNode = NodeId 991855
            requirement =
              RequiredGammaBinder
                { rgbEdgeIds = edgeId :| [],
                  rgbExteriorNode = exteriorNode,
                  rgbOperatedRoot = operatedNode,
                  rgbResultRoots = resultNode :| [],
                  rgbOperatedType =
                    TArrow
                      (tVarWithRef dependencyRef)
                      (tVarWithRef dependencyRef),
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement = RequiredGammaAtNestedScope ownerScope
                }
            requirements required =
              GeneralizationRequirements
                { grRequiredGammaBinders = [required],
                  grSourceBinderRefs = IntMap.empty,
                  grAmbientBinderRefs = [],
                  grAmbientGammaAuthorities = IntMap.empty,
                  grLocallyClosedGammaNodes = IntSet.empty
                }
            parents =
              IntMap.singleton
                (nodeRefKey (typeRef exteriorNode))
                (ownerScope, BindFlex)
            baseConstraint =
              toPresolvedConstraint
                (toAcyclicConstraint (toNormalizedConstraint emptyConstraint))
            ga =
              GaBindParents
                { gaBindParentsBase = parents,
                  gaBaseConstraint =
                    baseConstraint {cBindParents = parents},
                  gaBaseToSolved = IntMap.empty,
                  gaSolvedToBase = IntMap.empty,
                  gaRestoredSchemeRootTargets = IntMap.empty,
                  gaExpansionConstructionPlacements =
                    emptyExpansionConstructionPlacements
                }
        provisionalBound <-
          requireRight (elabToBound (TArrow TBottom TBottom))
        semanticBound <-
          requireRight
            ( elabToBound
                ( TArrow
                    (tVarWithRef dependencyRef)
                    (tVarWithRef dependencyRef)
                )
            )
        let productionScheme =
              mkElabSchemeWithRefs
                [ (dependencyRef, Nothing),
                  (plannedLocalRef, Just provisionalBound),
                  (consumerRef, Just semanticBound)
                ]
                (tVarWithRef plannedLocalRef)
            productionSubst =
              IntMap.fromList
                [ (getNodeId operatedNode, plannedLocalRef),
                  (getNodeId resultNode, plannedLocalRef),
                  (getNodeId dependencyRouteNode, dependencyRef),
                  -- The semantic exterior intentionally remains a different
                  -- identity, as in the exact lambda-body packet.
                  (getNodeId exteriorNode, consumerRef)
                ]
            scopeFor required subst =
              prepareMatchedLocalGammaRootConstructionScopeForTest
                ga
                [closure]
                (requirements required)
                subst
                productionScheme
            expectedDependencyAliases =
              IntMap.singleton
                (getNodeId dependencyRouteNode)
                dependencyRef
        (matchedBinders, matchedAliases) <-
          requireRight (scopeFor requirement productionSubst)
        map fst matchedBinders `shouldBe` [dependencyRef]
        matchedAliases `shouldBe` expectedDependencyAliases

        -- A different edge does not own the construction endpoint, even when
        -- the endpoint has the same display payload and type shape.
        let wrongEdgeRequirement =
              requirement {rgbEdgeIds = EdgeId 991853 :| []}
        (wrongEdgeBinders, _) <-
          requireRight (scopeFor wrongEdgeRequirement productionSubst)
        map fst wrongEdgeBinders
          `shouldBe` [dependencyRef, plannedLocalRef]

        -- Placement ownership is part of the proof, rather than a later
        -- name/type-shape repair.
        case
            scopeFor
              ( requirement
                  { rgbPlacement =
                      RequiredGammaAtNestedScope
                        (GenRef (GenNodeId 991854))
                  }
              )
              productionSubst
          of
            Left (ValidationFailed messages) ->
              messages
                `shouldSatisfy` any
                  (isInfixOf "placement disagrees")
            Left other ->
              expectationFailure
                ("expected local-owner validation, got " ++ show other)
            Right scope ->
              expectationFailure
                ("expected local-owner rejection, got " ++ show scope)

        -- Multiple result routes are one construction obligation and may not
        -- use list order to choose between distinct identities.
        let conflictingRequirement =
              requirement
                { rgbResultRoots =
                    resultNode :| [conflictingResultNode]
                }
            conflictingSubst =
              IntMap.insert
                (getNodeId conflictingResultNode)
                conflictingRef
                productionSubst
        case scopeFor conflictingRequirement conflictingSubst of
          Left (ValidationFailed messages) ->
            messages
              `shouldSatisfy` any
                (isInfixOf "conflicting result endpoints")
          Left other ->
            expectationFailure
              ("expected endpoint conflict validation, got " ++ show other)
          Right scope ->
              expectationFailure
                ("expected endpoint conflict rejection, got " ++ show scope)

      it "selects one typed direct-owner lane before flexible exterior ownership" $ do
        let applicationEdge = EdgeId 991896
            argumentEdge = EdgeId 991902
            lambdaEdge = EdgeId 991899
            applicationOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = applicationEdge,
                  lgoTermNode = NodeId 991897,
                  lgoScope = GenRef (GenNodeId 991898)
                }
            lambdaOwner =
              LocalGammaOwner
                { lgoConstructor = LocalLambdaGamma,
                  lgoBoundaryEdge = lambdaEdge,
                  lgoTermNode = NodeId 991900,
                  lgoScope = GenRef (GenNodeId 991901)
                }
            applicationAnn =
              AApp
                (ALit (LInt 1) (NodeId 991903))
                ( AAnn
                    (ALit (LInt 2) (NodeId 991904))
                    (NodeId 991904)
                    argumentEdge
                )
                ( mkInstantiationSite
                    applicationEdge
                    (NodeId 991903)
                    (lgoTermNode applicationOwner)
                )
                ( mkInstantiationSite
                    argumentEdge
                    (NodeId 991904)
                    (lgoTermNode applicationOwner)
                )
                (lgoTermNode applicationOwner)
            scopeForBoundary candidateEdgeId termNode
              | candidateEdgeId == applicationEdge
                  && termNode == lgoTermNode applicationOwner =
                  Right (lgoScope applicationOwner)
              | otherwise =
                  Left
                    ( ValidationFailed
                        [ "unexpected sticky direct-owner boundary"
                        , "  edge: " ++ show candidateEdgeId
                        , "  term: " ++ show termNode
                        ]
                    )
            duplicateDirectEdgeAnn =
              AApp
                (ALit (LInt 1) (NodeId 991903))
                (ALit (LInt 2) (NodeId 991904))
                ( mkInstantiationSite
                    applicationEdge
                    (NodeId 991903)
                    (lgoTermNode applicationOwner)
                )
                ( mkInstantiationSite
                    applicationEdge
                    (NodeId 991904)
                    (lgoTermNode applicationOwner)
                )
                (lgoTermNode applicationOwner)
            ownsOnlyLambda candidateOwner = candidateOwner == lambdaOwner
        directOwners <-
          requireRight
            ( localGammaDirectApplicationEdgeOwners
                scopeForBoundary
                applicationAnn
            )
        directOwners
          `shouldBe` IntMap.fromList
            [ (getEdgeId applicationEdge, applicationOwner),
              (getEdgeId argumentEdge, applicationOwner)
            ]
        localGammaDirectApplicationEdgeOwners
          scopeForBoundary
          duplicateDirectEdgeAnn
          `shouldSatisfy` isLeft
        selectLocalGammaEdgeOwnership
          directOwners
          argumentEdge
          [applicationOwner, lambdaOwner]
          ownsOnlyLambda
          `shouldBe` Just
            (DirectApplicationEdgeOwnership applicationOwner)
        -- The operand wrapper revisits the same edge without carrying the
        -- AApp as its current frame owner.  The precomputed edge map keeps
        -- direct precedence sticky and blocks the enclosing lambda.
        selectLocalGammaEdgeOwnership
          directOwners
          argumentEdge
          [lambdaOwner]
          ownsOnlyLambda
          `shouldBe` Just
            (DirectApplicationEdgeOwnership applicationOwner)
        selectLocalGammaEdgeOwnership
          directOwners
          lambdaEdge
          [lambdaOwner]
          ownsOnlyLambda
          `shouldBe` Just
            (FlexibleExteriorEdgeOwnership lambdaOwner)

      it "closes an argument requirement from its direct application-edge provenance" $ do
        let functionEdge = EdgeId 991900
            argumentEdge = EdgeId 991901
            exterior = NodeId 991902
            operated = NodeId 991903
            result = NodeId 991904
            applicationScope = GenRef (GenNodeId 991905)
            applicationOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = functionEdge,
                  lgoTermNode = NodeId 991906,
                  lgoScope = applicationScope
                }
            lambdaEdge = EdgeId 991908
            secondLambdaEdge = EdgeId 991909
            lambdaScope = GenRef (GenNodeId 991910)
            lambdaOwner =
              LocalGammaOwner
                { lgoConstructor = LocalLambdaGamma,
                  lgoBoundaryEdge = lambdaEdge,
                  lgoTermNode = NodeId 991911,
                  lgoScope = lambdaScope
                }
            argumentClosure =
              LocalGammaClosure
                { lgcEdgeIds = argumentEdge :| [],
                  lgcDirectApplicationEdgeIds = [argumentEdge],
                  lgcExteriorNode = exterior,
                  lgcConsumerIdentity =
                    typeBinderIdentityFromNode exterior,
                  lgcOwner = applicationOwner,
                  lgcOwnerPendingScheme = Nothing
                }
            lambdaClosure =
              argumentClosure
                { lgcEdgeIds = lambdaEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcOwner = lambdaOwner
                }
            requirement =
              RequiredGammaBinder
                { rgbEdgeIds = argumentEdge :| [],
                  rgbExteriorNode = exterior,
                  rgbOperatedRoot = operated,
                  rgbResultRoots = result :| [],
                  rgbOperatedType = TBottom,
                  rgbExactOperatedOccurrenceRef = Nothing,
                  rgbPlacement =
                    RequiredGammaAtNestedScope applicationScope
                }
            requirements =
              GeneralizationRequirements
                { grRequiredGammaBinders = [requirement],
                  grSourceBinderRefs = IntMap.empty,
                  grAmbientBinderRefs = [],
                  grAmbientGammaAuthorities = IntMap.empty,
                  grLocallyClosedGammaNodes = IntSet.empty
                }
            resultRef = graphRef result "application-result"
            constructionScheme =
              mkElabSchemeWithRefs
                [(resultRef, Nothing)]
                (tVarWithRef resultRef)
            constructionSubst =
              IntMap.singleton (getNodeId result) resultRef
            baseConstraint =
              toPresolvedConstraint
                (toAcyclicConstraint (toNormalizedConstraint emptyConstraint))
            ga =
              GaBindParents
                { gaBindParentsBase = IntMap.empty,
                  gaBaseConstraint = baseConstraint,
                  gaBaseToSolved = IntMap.empty,
                  gaSolvedToBase = IntMap.empty,
                  gaRestoredSchemeRootTargets = IntMap.empty,
                  gaExpansionConstructionPlacements =
                    emptyExpansionConstructionPlacements
                }
            scopeForRequirement requirement' closure' =
              prepareMatchedLocalGammaRootConstructionScopeForTest
                ga
                [closure']
                requirements
                  { grRequiredGammaBinders = [requirement']
                  }
                constructionSubst
                constructionScheme
            scopeFor = scopeForRequirement requirement
        (binders, aliases) <- requireRight (scopeFor argumentClosure)
        binders `shouldBe` []
        aliases `shouldBe` IntMap.empty

        -- The transparent operand wrapper can visit the same paper edge a
        -- second time.  The direct AApp claim wins and the unrelated edge
        -- remains root-owned exactly once.
        unclaimedEdgesOutsideLocalGammaClosuresForTest
          (IntMap.singleton (getEdgeId argumentEdge) argumentClosure)
          [argumentEdge, functionEdge, argumentEdge, functionEdge]
          `shouldBe` [functionEdge]
        unclaimedEdgesOutsideLocalGammaClosuresForTest
          IntMap.empty
          [argumentEdge, argumentEdge]
          `shouldBe` [argumentEdge]

        -- A direct AApp closure on the same requirement edge is foreign to
        -- the enclosing lambda lane, so it is absence rather than conflict.
        selectLocalGammaClosureOwnerLaneForTest
          lambdaOwner
          (IntMap.singleton (getEdgeId argumentEdge) argumentClosure)
          requirement
          `shouldBe` Right Nothing

        -- The two owner lanes can coexist in one environment and each
        -- selects only its own exact occurrence.
        let lambdaRequirement =
              requirement
                { rgbEdgeIds = lambdaEdge :| []
                }
            coexistingClosures =
              IntMap.fromList
                [ (getEdgeId argumentEdge, argumentClosure),
                  (getEdgeId lambdaEdge, lambdaClosure)
                ]
        selectLocalGammaClosureOwnerLaneForTest
          applicationOwner
          coexistingClosures
          requirement
          `shouldBe` Right (Just argumentClosure)
        selectLocalGammaClosureOwnerLaneForTest
          lambdaOwner
          coexistingClosures
          lambdaRequirement
          `shouldBe` Right (Just lambdaClosure)

        -- Once a lane claims one edge of a merged requirement, two distinct
        -- closure records in that same lane remain an ambiguity.
        let secondLambdaClosure =
              lambdaClosure
                { lgcEdgeIds = secondLambdaEdge :| []
                }
            ambiguousLambdaRequirement =
              lambdaRequirement
                { rgbEdgeIds = lambdaEdge :| [secondLambdaEdge]
                }
            ambiguousLambdaClosures =
              IntMap.fromList
                [ (getEdgeId lambdaEdge, lambdaClosure),
                  (getEdgeId secondLambdaEdge, secondLambdaClosure)
                ]
        selectLocalGammaClosureOwnerLaneForTest
          lambdaOwner
          ambiguousLambdaClosures
          ambiguousLambdaRequirement
          `shouldSatisfy` isLeft

        -- The function occurrence cannot stand in for the argument
        -- occurrence merely because both belong to the same AApp.
        scopeFor
          ( argumentClosure
              { lgcDirectApplicationEdgeIds = [functionEdge]
              }
          )
          `shouldSatisfy` isLeft

        -- A direct argument edge cannot absorb a descendant obligation that
        -- happened to merge under the same exterior.
        let descendantEdge = EdgeId 991907
            mergedRequirement =
              requirement
                { rgbEdgeIds = argumentEdge :| [descendantEdge]
                }
            partiallyDirectClosure =
              argumentClosure
                { lgcEdgeIds = argumentEdge :| [descendantEdge]
                }
        scopeForRequirement mergedRequirement partiallyDirectClosure
          `shouldSatisfy` isLeft

        -- Only an application frame may publish direct-application
        -- provenance.
        scopeFor
          ( argumentClosure
              { lgcOwner =
                  applicationOwner
                    { lgoConstructor = LocalLambdaGamma
                    }
              }
          )
          `shouldSatisfy` isLeft

      it "retains only the exact inherited dependency of a routed local bound" $ do
        let dependencyNode = NodeId 991873
            wrongDependencyNode = NodeId 991874
            routedLocalNode = NodeId 991875
            dependencyRef = graphRef dependencyNode "a"
            sameNamedWrongDependencyRef =
              graphRef wrongDependencyNode "a"
            routedLocalRef = graphRef routedLocalNode "local"
            dependencyAliasKey = 991876
            dependencyAliases ref =
              IntMap.singleton dependencyAliasKey ref
            constructionSubst =
              IntMap.singleton
                (getNodeId exteriorNode)
                consumerRef
        dependencyBound <-
          requireRight
            ( elabToBound
                ( TArrow
                    (tVarWithRef dependencyRef)
                    (tVarWithRef dependencyRef)
                )
            )
        let dependentScheme =
              mkElabSchemeWithRefs
                [(routedLocalRef, Just dependencyBound)]
                (tVarWithRef routedLocalRef)
            scopeFor dependency =
              prepareProvisionalLocalGammaRootConstructionScopeWithRequirementEvidenceForTest
                [closure]
                [routedLocalRef]
                [(dependency, Nothing)]
                (dependencyAliases dependency)
                constructionSubst
                dependentScheme
        (exactBinders, exactAliases) <-
          requireRight (scopeFor dependencyRef)
        exactBinders `shouldBe` [(dependencyRef, Nothing)]
        exactAliases `shouldBe` dependencyAliases dependencyRef
        (wrongBinders, wrongAliases) <-
          requireRight (scopeFor sameNamedWrongDependencyRef)
        wrongBinders `shouldBe` []
        wrongAliases `shouldBe` IntMap.empty

      it "authorizes only the exact ambient identity in a locally emitted bound" $ do
        let dependencyRef = graphRef (NodeId 991877) "a"
            sameNamedWrongDependencyRef =
              graphRef (NodeId 991878) "a"
        dependencyBound <-
          requireRight
            ( elabToBound
                ( TArrow
                    (tVarWithRef dependencyRef)
                    (tVarWithRef dependencyRef)
                )
            )
        let dependentScheme =
              mkElabSchemeWithRefs
                [(plannedLocalRef, Just dependencyBound)]
                (tVarWithRef plannedLocalRef)
            dependentConstructedType =
              TForallRef
                emittedLocalRef
                (Just dependencyBound)
                (tVarWithRef emittedLocalRef)
            dependencyCertificate =
              certificate
                { ofcConstructedType = dependentConstructedType,
                  ofcUsedAmbientBinderRefs = [dependencyRef]
                }
        closed <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalForTest
                [closure]
                rootSubst
                dependentScheme
                dependencyCertificate
            )
        schemeBinderRefs closed `shouldBe` []
        schemeBody closed `shouldBe` dependentConstructedType
        prepareRootClosureSchemeWithOwnerFinalForTest
          [closure]
          rootSubst
          dependentScheme
          dependencyCertificate
            { ofcUsedAmbientBinderRefs =
                [sameNamedWrongDependencyRef]
            }
          `shouldSatisfy` isLeft

      it "retains exact ambient authority when source projection rebuilds a local root closure" $ do
        let dependencyRef = graphRef (NodeId 991879) "a"
            projectedPeer =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991880))
                "a"
            preferredSubst =
              IntMap.singleton
                991879
                projectedPeer
        dependencyBound <-
          requireRight
            ( elabToBound
                ( TArrow
                    (tVarWithRef dependencyRef)
                    (tVarWithRef dependencyRef)
                )
            )
        let dependentScheme =
              mkElabSchemeWithRefs
                [(plannedLocalRef, Just dependencyBound)]
                (tVarWithRef plannedLocalRef)
            dependentConstructedType =
              TForallRef
                emittedLocalRef
                (Just dependencyBound)
                (tVarWithRef emittedLocalRef)
            dependencyCertificate =
              certificate
                { ofcConstructedType = dependentConstructedType,
                  ofcUsedAmbientBinderRefs = [dependencyRef]
                }
        (projectedAmbientRefs, projectedScheme) <-
          requireRight
            ( projectRootClosureSchemeWithOwnerFinalForTest
                [closure]
                rootSubst
                preferredSubst
                dependentScheme
                dependencyCertificate
            )
        projectedAmbientRefs `shouldBe` [dependencyRef]
        projectedAmbientRefs `shouldNotContain` [projectedPeer]
        schemeBody projectedScheme `shouldBe` dependentConstructedType

      it "uses the typed owner's local binder and drops an unused ambient root candidate" $ do
        closed <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalForTest
                [closure]
                rootSubst
                plannedScheme
                certificate
            )
        schemeBinderRefs closed `shouldBe` []
        schemeBody closed `shouldBe` constructedType

      it "carries the exact body-consumer bound into the final root binder spine" $ do
        let dependencyRef = graphRef (NodeId 991881) "a"
            provisionalRef = graphRef (NodeId 991882) "b"
            semanticRef = graphRef (NodeId 991883) "c"
            refinementEdge = EdgeId 991884
            ambientApplicationEdge = EdgeId 991887
            ambientArgumentEdge = EdgeId 991888
            ambientExterior = NodeId 991889
            ambientOperated = NodeId 991890
            ambientType = TestElab.tBase (BaseTy "Int")
            ambientClaimRef = graphRef ambientExterior "ambient"
            refinementOwner =
              owner
                { lgoBoundaryEdge = refinementEdge,
                  lgoTermNode = NodeId 991885,
                  lgoScope = GenRef (GenNodeId 991886)
                }
            ambientOwner =
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = ambientApplicationEdge,
                  lgoTermNode = NodeId 991891,
                  lgoScope = GenRef (GenNodeId 991892)
                }
            ambientClaim =
              DirectApplicationAmbientGammaClaim
                { daagcEdgeIds = ambientApplicationEdge :| [],
                  daagcExteriorNode = ambientExterior,
                  daagcOperatedRoot = ambientOperated,
                  daagcConstructionResultRoots =
                    ambientOperated :| [],
                  daagcOperatedType = ambientType,
                  daagcAmbientRef = ambientClaimRef,
                  daagcAmbientBound = ambientType
                }
            ambientApplicationCertificate =
              LocalGammaConstructionCertificate
                { lgccOwner = ambientOwner,
                  lgccConstructedType = ambientType,
                  lgccConstruction = LocalGammaAmbient,
                  lgccDirectApplicationSourceEdgeIds =
                    ambientApplicationEdge :| [ambientArgumentEdge],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims =
                    [ambientClaim],
                  lgccAmbientDeclarationAuthorities =
                    [AmbientGammaAuthority ambientClaimRef ambientType],
                  lgccLocalBinderRoutes = IntMap.empty,
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = []
                }
            completedBound =
              TArrow
                (tVarWithRef dependencyRef)
                (tVarWithRef dependencyRef)
            provisionalBound =
              TArrow TBottom TBottom
        completedBoundTy <- requireRight (elabToBound completedBound)
        provisionalBoundTy <- requireRight (elabToBound provisionalBound)
        let refinementClosure =
              LocalGammaClosure
                { lgcEdgeIds = refinementEdge :| [],
                  lgcDirectApplicationEdgeIds = [],
                  lgcExteriorNode = NodeId 991883,
                  lgcConsumerIdentity =
                    typeBinderIdentityFromNode (NodeId 991883),
                  lgcOwner = refinementOwner,
                  lgcOwnerPendingScheme = Nothing
                }
            refinementRoute =
              BodyConsumerRouteTestView
                { bcrtvEdgeId = refinementEdge,
                  bcrtvOwner = refinementOwner,
                  bcrtvExteriorNode = NodeId 991883,
                  bcrtvSemanticRef = semanticRef,
                  bcrtvConstructionRef = provisionalRef,
                  bcrtvOperatedType = completedBound,
                  bcrtvConstructionOperatedType = completedBound
                }
            refinementScheme =
              mkElabSchemeWithRefs
                [ (dependencyRef, Nothing),
                  (provisionalRef, Just provisionalBoundTy),
                  (semanticRef, Nothing)
                ]
                ( TArrow
                    (tVarWithRef dependencyRef)
                    (tVarWithRef semanticRef)
                )
            refinementConstructedType =
              TForallRef
                semanticRef
                Nothing
                ( TArrow
                    (tVarWithRef dependencyRef)
                    (tVarWithRef semanticRef)
                )
            refinementOwnerCertificate =
              OwnerFinalConstruction
                { ofcOwner = refinementOwner,
                  ofcConstructedType = refinementConstructedType,
                  ofcLocallyEmittedBinderRefs = [semanticRef],
                  ofcLocalBinderRoutes =
                    IntMap.singleton 991883 semanticRef,
                  ofcUsedAmbientBinderRefs =
                    [dependencyRef, provisionalRef],
                  ofcBodyConsumerBoundRefinements = []
                }
            refinementSubst =
              IntMap.singleton 991883 semanticRef
            binderBoundFor ref scheme =
              [ bound
              | (candidate, bound) <- schemeBinderRefs scheme
              , typeBinderRefsSameIdentity candidate ref
              ]
        staleClosure <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalAndApplicationsForTest
                [refinementClosure]
                [ambientApplicationCertificate]
                refinementSubst
                refinementScheme
                refinementOwnerCertificate
            )
        binderBoundFor provisionalRef staleClosure
          `shouldBe` [Just provisionalBoundTy]
        refinedCertificate <-
          requireRight
            ( attachBodyConsumerBoundRefinementForTest
                DirectAmbientProvisionalNestedResult
                [(provisionalRef, Just completedBoundTy)]
                refinementRoute
                completedBound
                (Map.singleton provisionalRef provisionalBound)
                refinementOwnerCertificate
            )
        refinedClosure <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalAndApplicationsForTest
                [refinementClosure]
                [ambientApplicationCertificate]
                refinementSubst
                refinementScheme
                refinedCertificate
            )
        binderBoundFor provisionalRef refinedClosure
          `shouldBe` [Just completedBoundTy]
        attachBodyConsumerBoundRefinementForTest
          DirectAmbientEstablished
          [(provisionalRef, Just completedBoundTy)]
          refinementRoute
          completedBound
          (Map.singleton provisionalRef provisionalBound)
          refinementOwnerCertificate
          `shouldSatisfy` isLeft

      it "uses the typed owner's route when the root substitution omits the local binder" $ do
        closed <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalForTest
                [closure]
                IntMap.empty
                plannedScheme
                certificate
                  { ofcLocalBinderRoutes =
                      IntMap.singleton
                        (getNodeId exteriorNode)
                        emittedLocalRef
                  }
            )
        schemeBinderRefs closed `shouldBe` []
        schemeBody closed `shouldBe` constructedType

      it "accepts an exact owner that discharges an unused local Gamma slot without a binder" $ do
        let exactType = TestElab.tBase (BaseTy "Int")
            dischargedCertificate =
              certificate
                { ofcConstructedType = exactType,
                  ofcLocallyEmittedBinderRefs = [],
                  ofcLocalBinderRoutes = IntMap.empty,
                  ofcUsedAmbientBinderRefs = []
                }
        closed <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalForTest
                [closure]
                IntMap.empty
                (Elab.schemeFromType exactType)
                dischargedCertificate
            )
        schemeBinderRefs closed `shouldBe` []
        schemeBody closed `shouldBe` exactType

      it "does not discharge a local Gamma slot still used by the exact owner" $ do
        expectCertificateFailure
          "used local Gamma slot"
          ( prepareRootClosureSchemeWithOwnerFinalForTest
              [closure]
              IntMap.empty
              (Elab.schemeFromType (tVarWithRef consumerRef))
              certificate
                { ofcConstructedType = tVarWithRef consumerRef,
                  ofcLocallyEmittedBinderRefs = [],
                  ofcLocalBinderRoutes = IntMap.empty,
                  ofcUsedAmbientBinderRefs = [consumerRef]
                }
          )

      it "rejects a locally emitted binder without an exact graph provenance route" $ do
        expectCertificateFailure
          "missing local route"
          ( prepareRootClosureSchemeWithOwnerFinalForTest
              [closure]
              rootSubst
              plannedScheme
              certificate {ofcLocalBinderRoutes = IntMap.empty}
          )

      it "rejects a certificate whose source owner differs from the local Gamma owner" $ do
        let wrongOwner =
              owner
                { lgoBoundaryEdge = EdgeId 991847,
                  lgoScope = GenRef (GenNodeId 991848)
                }
        expectCertificateFailure
          "owner mismatch"
          ( prepareRootClosureSchemeWithOwnerFinalForTest
              [closure]
              rootSubst
              plannedScheme
              certificate {ofcOwner = wrongOwner}
          )

      it "rejects a constructed result that uses an uncertified ambient binder" $ do
        let ambientConstructedType =
              TForallRef
                emittedLocalRef
                Nothing
                (TArrow (tVarWithRef ambientRef) (tVarWithRef emittedLocalRef))
        expectCertificateFailure
          "uncertified ambient use"
          ( prepareRootClosureSchemeWithOwnerFinalForTest
              [closure]
              rootSubst
              plannedScheme
              certificate {ofcConstructedType = ambientConstructedType}
          )

      it "accepts an exact ambient Gamma identity certified and bound by the root plan" $ do
        let ambientConstructedType =
              TForallRef
                emittedLocalRef
                Nothing
                (TArrow (tVarWithRef ambientRef) (tVarWithRef emittedLocalRef))
        closed <-
          requireRight
            ( prepareRootClosureSchemeWithOwnerFinalForTest
                [closure]
                rootSubst
                plannedScheme
                certificate
                  { ofcConstructedType = ambientConstructedType,
                    ofcUsedAmbientBinderRefs = [ambientRef]
                  }
            )
        schemeBinderRefs closed `shouldBe` [(ambientRef, Nothing)]
        schemeBody closed `shouldBe` ambientConstructedType

      it "rejects a certified ambient identity with no planner-owned root binder" $ do
        let escapedAmbientRef = graphRef (NodeId 991848) "escaped-ambient"
            escapedAmbientType =
              TForallRef
                emittedLocalRef
                Nothing
                (TArrow (tVarWithRef escapedAmbientRef) (tVarWithRef emittedLocalRef))
        case
            prepareRootClosureSchemeWithOwnerFinalForTest
              [closure]
              rootSubst
              plannedScheme
              certificate
                { ofcConstructedType = escapedAmbientType,
                  ofcUsedAmbientBinderRefs = [escapedAmbientRef]
                }
          of
            Left (ValidationFailed messages) ->
              messages `shouldSatisfy` any (isInfixOf "prepared root closure")
            Left other ->
              expectationFailure
                ("expected root scheme-closure validation, got " ++ show other)
            Right closed ->
              expectationFailure
                ("expected unowned ambient identity rejection, got " ++ show closed)

      it "rejects an ambient-use certificate that omits a planner-bound dependency" $ do
        let dependentNode = NodeId 991849
            dependentRef = graphRef dependentNode "dependent"
        dependencyBound <-
          requireRight
            ( elabToBound
                (TArrow (tVarWithRef ambientRef) (tVarWithRef ambientRef))
            )
        let dependentScheme =
              mkElabSchemeWithRefs
                [ (ambientRef, Nothing),
                  (dependentRef, Just dependencyBound),
                  (plannedLocalRef, Nothing)
                ]
                (TArrow (tVarWithRef dependentRef) (tVarWithRef plannedLocalRef))
            dependentConstructedType =
              TForallRef
                emittedLocalRef
                Nothing
                (TArrow (tVarWithRef dependentRef) (tVarWithRef emittedLocalRef))
        expectCertificateFailure
          "missing ambient bound dependency"
          ( prepareRootClosureSchemeWithOwnerFinalForTest
              [closure]
              rootSubst
              dependentScheme
              certificate
                { ofcConstructedType = dependentConstructedType,
                  ofcUsedAmbientBinderRefs = [dependentRef]
                }
          )

      it "keeps an application-local route key from capturing its ambient root dependency" $ do
        let localRouteKey = getNodeId ambientNode
            ambientAliasKey = 991850
            applicationOwner =
              owner {lgoConstructor = LocalApplicationGamma}
            sourceBinderRefs =
              IntMap.singleton localRouteKey emittedLocalRef
            constructionSubst =
              IntMap.fromList
                [ (localRouteKey, ambientRef),
                  (ambientAliasKey, ambientRef)
                ]
        consumerBound <-
          requireRight
            ( elabToBound
                (TArrow (tVarWithRef ambientRef) (tVarWithRef ambientRef))
            )
        let constructionScheme =
              mkElabSchemeWithRefs
                [ (ambientRef, Nothing),
                  (emittedLocalRef, Just consumerBound)
                ]
                (tVarWithRef emittedLocalRef)
            applicationCertificate =
              LocalGammaConstructionCertificate
                { lgccOwner = applicationOwner,
                  lgccConstructedType = tVarWithRef emittedLocalRef,
                  lgccConstruction =
                    LocalGammaEmitted
                      ((emittedLocalRef, Just consumerBound) :| [])
                      [],
                  lgccDirectApplicationSourceEdgeIds = edgeId :| [],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims = [],
                  lgccAmbientDeclarationAuthorities = [],
                  lgccLocalBinderRoutes =
                    IntMap.singleton localRouteKey emittedLocalRef,
                  lgccSourceBinderAuthorities =
                    IntMap.singleton localRouteKey emittedLocalRef,
                  lgccUsedAmbientBinderRefs = [ambientRef]
                }
        (constructionBinders, constructionAliases) <-
          requireRight
            ( prepareLocalApplicationRootConstructionScopeForTest
                sourceBinderRefs
                [applicationCertificate]
                constructionSubst
                constructionScheme
            )
        constructionBinders `shouldBe` [(ambientRef, Nothing)]
        constructionAliases
          `shouldBe` IntMap.singleton ambientAliasKey ambientRef

      it "treats a fully consumed application Gamma as a discharged local closure" $ do
        let applicationOwner =
              owner {lgoConstructor = LocalApplicationGamma}
            consumedRef = emittedLocalRef
            consumedClosure =
              closure {lgcOwner = applicationOwner}
            consumedCertificate =
              LocalGammaConstructionCertificate
                { lgccOwner = applicationOwner,
                  lgccConstructedType = TestElab.tBase (BaseTy "Int"),
                  lgccConstruction =
                    LocalGammaConsumed
                      ((consumedRef, Nothing) :| []),
                  lgccDirectApplicationSourceEdgeIds = edgeId :| [],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims = [],
                  lgccAmbientDeclarationAuthorities = [],
                  lgccLocalBinderRoutes =
                    IntMap.singleton
                      (getNodeId exteriorNode)
                      consumedRef,
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = []
                }
            monomorphicScheme =
              mkElabSchemeWithRefs
                []
                (TestElab.tBase (BaseTy "Int"))
        closed <-
          requireRight
            ( prepareLocalApplicationRootClosureForTest
                IntMap.empty
                [consumedClosure]
                [consumedCertificate]
                IntMap.empty
                monomorphicScheme
            )
        schemeBinderRefs closed `shouldBe` []
        schemeBody closed `shouldBe` TestElab.tBase (BaseTy "Int")

      it "does not let a consumed application certificate discharge another exterior" $ do
        let applicationOwner =
              owner {lgoConstructor = LocalApplicationGamma}
            consumedClosure =
              closure {lgcOwner = applicationOwner}
            consumedCertificate =
              LocalGammaConstructionCertificate
                { lgccOwner = applicationOwner,
                  lgccConstructedType = TestElab.tBase (BaseTy "Int"),
                  lgccConstruction =
                    LocalGammaConsumed
                      ((emittedLocalRef, Nothing) :| []),
                  lgccDirectApplicationSourceEdgeIds = edgeId :| [],
                  lgccDirectApplicationGammaClaims = [],
                  lgccDirectApplicationAmbientGammaClaims = [],
                  lgccAmbientDeclarationAuthorities = [],
                  lgccLocalBinderRoutes =
                    IntMap.singleton
                      (getNodeId plannedLocalNode)
                      emittedLocalRef,
                  lgccSourceBinderAuthorities = IntMap.empty,
                  lgccUsedAmbientBinderRefs = []
                }
            monomorphicScheme =
              mkElabSchemeWithRefs
                []
                (TestElab.tBase (BaseTy "Int"))
        case
            prepareLocalApplicationRootClosureForTest
              IntMap.empty
              [consumedClosure]
              [consumedCertificate]
              IntMap.empty
              monomorphicScheme
          of
            Left (ValidationFailed messages) ->
              messages
                `shouldSatisfy` any
                  (isInfixOf "result-local Gamma has no root or owner-final substitution route")
            Left other ->
              expectationFailure
                ("expected unrelated exterior rejection, got " ++ show other)
            Right closed ->
              expectationFailure
                ("expected unrelated exterior rejection, got " ++ show closed)

      it "keeps an expanded source alias out of local Gamma ownership" $ do
        let directNode = NodeId 991855
            derivedNode = NodeId 991856
            sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991857))
                "source"
            expandedSourceRefs =
              IntMap.fromList
                [ (getNodeId directNode, sourceRef),
                  (getNodeId derivedNode, sourceRef)
                ]
        authorityRefs <-
          requireRight
            ( sourceBinderAuthorityRefsForTest
                id
                (IntSet.singleton (getNodeId directNode))
                expandedSourceRefs
                IntMap.empty
            )
        authorityRefs
          `shouldBe` IntMap.singleton (getNodeId directNode) sourceRef

      it "routes an ordinary lambda payload through its emitted local Gamma" $ do
        let paramNode = NodeId 991860
            localNode = NodeId 991861
            sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991862))
                "source"
            localRef = graphRef localNode "local"
            renames =
              lambdaParamLocalGammaRenamesForTest
                paramNode
                (TVarRef sourceRef)
                [localRef]
                []
                (IntMap.singleton (getNodeId paramNode) localRef)
            sourceLambda =
              mkTestLocalLam
                "value"
                (TVarRef sourceRef)
                (mkTestDeferredVar "value")
        renames `shouldBe` [(sourceRef, localRef)]
        case renameTermTypeVars renames sourceLambda of
          Elab.ELam parameter (Elab.EVarNode occurrence) -> do
            resolvedVarType parameter
              `shouldBe` TVarRef localRef
            resolvedVarType occurrence
              `shouldBe` TVarRef localRef
          other ->
            expectationFailure
              ("expected a locally routed lambda payload, got " ++ show other)

      it "routes the sole free identity inside a structured lambda parameter" $ do
        let paramNode = NodeId 991863
            localNode = NodeId 991864
            sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991865))
                "source"
            localRef = graphRef localNode "local"
            structuredParamTy =
              TestElab.tCon
                (BaseTy "Box")
                (TVarRef sourceRef :| [])
        lambdaParamLocalGammaRenamesForTest
          paramNode
          structuredParamTy
          [localRef]
          [(sourceRef, localRef)]
          IntMap.empty
          `shouldBe` [(sourceRef, localRef)]
        lambdaParamLocalGammaRenamesForTest
          paramNode
          structuredParamTy
          []
          [(sourceRef, localRef)]
          IntMap.empty
          `shouldBe` []

      it "constructs a later ambient Gamma bound in the active quotient domain" $ do
        let sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991866))
                "source"
            constructionRef = graphRef (NodeId 991867) "construction"
            ambientConsumerRef = graphRef (NodeId 991868) "consumer"
            boxSourceTy =
              TestElab.tCon
                (BaseTy "Box")
                (TVarRef sourceRef :| [])
            boxConstructionTy =
              TestElab.tCon
                (BaseTy "Box")
                (TVarRef constructionRef :| [])
            sourceBoundTy =
              TArrow
                boxSourceTy
                (TArrow boxSourceTy (TestElab.tBase (BaseTy "Bool")))
            constructionBoundTy =
              TArrow
                boxConstructionTy
                (TArrow boxConstructionTy (TestElab.tBase (BaseTy "Bool")))
        sourceBound <- requireRight (elabToBound sourceBoundTy)
        alignedBound <-
          requireRight
            ( constructionBoundAfterScopeExtensionForTest
                [(sourceRef, constructionRef)]
                [(ambientConsumerRef, Just sourceBound)]
                ambientConsumerRef
            )
        alignedBound `shouldBe` Just constructionBoundTy

      it "re-enters the exact body quotient after source occurrence projection" $ do
        let occurrenceNode = NodeId 991869
            constructionRef = graphRef occurrenceNode "construction"
            sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991870))
                "source"
            graphLambda =
              mkTestLocalLam
                "value"
                (TVarRef constructionRef)
                (mkTestDeferredVar "value")
            sourceProjected =
              substInTermRefs
                (IntMap.singleton (getNodeId occurrenceNode) sourceRef)
                graphLambda
            constructionProjected =
              renameTermTypeVars
                [(sourceRef, constructionRef)]
                sourceProjected
        case constructionProjected of
          Elab.ELam parameter (Elab.EVarNode occurrence) -> do
            resolvedVarType parameter
              `shouldBe` TVarRef constructionRef
            resolvedVarType occurrence
              `shouldBe` TVarRef constructionRef
          other ->
            expectationFailure
              ( "expected an exact construction-domain lambda occurrence, got "
                  ++ show other
              )

      it "enters an exact body quotient atomically across its matching type abstraction" $ do
        let constructionRef =
              graphRef (NodeId 991871) "construction"
            sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991872))
                "source"
            sameNamedPeer =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991873))
                "source"
            sourceTerm =
              Elab.ETyAbsRef
                sourceRef
                Nothing
                ( mkTestLocalLam
                    "value"
                    (TVarRef sourceRef)
                    (mkTestDeferredVar "value")
                )
            constructionTerm =
              renameTermTypeBinderRefPayloads
                [(sourceRef, constructionRef)]
                sourceTerm
        case constructionTerm of
          Elab.ETyAbsRef binderRef Nothing
            (Elab.ELam parameter (Elab.EVarNode occurrence)) -> do
              binderRef
                `shouldSatisfy` typeBinderRefsSameIdentity constructionRef
              resolvedVarType parameter
                `shouldBe` TVarRef constructionRef
              resolvedVarType occurrence
                `shouldBe` TVarRef constructionRef
          other ->
            expectationFailure
              ( "expected one atomically projected type abstraction, got "
                  ++ show other
              )
        renameTermTypeBinderRefPayloads
          [(sameNamedPeer, constructionRef)]
          sourceTerm
          `shouldBe` sourceTerm

      it "projects an exact evidence binder and its occurrences through one construction quotient" $ do
        let constructionRef =
              graphRef (NodeId 991874) "construction"
            sourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991875))
                "evidence-a"
            sameNamedPeer =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991876))
                "evidence-a"
            sourceTy =
              TArrow
                (TVarRef sourceRef)
                (TVarRef sourceRef)
            constructionTy =
              TArrow
                (TVarRef constructionRef)
                (TVarRef constructionRef)
            evidenceRef =
              localRefFromIdentity
                (GeneratedLocalId (UniqueIdentity 991877))
                "$evidence"
            sourceEvidence =
              ResolvedVar
                { resolvedVarType = sourceTy
                , resolvedVarDetails = EvidenceId evidenceRef
                }
            sourceTerm =
              Elab.ELam
                sourceEvidence
                (Elab.EVarNode sourceEvidence)
            constructionTerm =
              renameTermTypeBinderRefPayloads
                [(sourceRef, constructionRef)]
                sourceTerm
        case constructionTerm of
          Elab.ELam binder (Elab.EVarNode occurrence) -> do
            resolvedVarType binder `shouldBe` constructionTy
            resolvedVarType occurrence `shouldBe` constructionTy
          other ->
            expectationFailure
              ( "expected an atomically projected evidence lambda, got "
                  ++ show other
              )
        renameTermTypeBinderRefPayloads
          [(sameNamedPeer, constructionRef)]
          sourceTerm
          `shouldBe` sourceTerm

      it "projects a direct source route without capturing an expansion-only consumer alias" $ do
        let projectionAmbientNode = NodeId 991851
            projectionConsumerNode = NodeId 991852
            projectionLocalNode = NodeId 991853
            projectionAmbientRef =
              graphRef projectionAmbientNode "ambient"
            projectionConsumerRef =
              graphRef projectionConsumerNode "consumer"
            projectionLocalRef =
              graphRef projectionLocalNode "local"
            projectionSourceRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromUnique (UniqueIdentity 991854))
                "source"
            preferredSourceRefs =
              IntMap.fromList
                [ (getNodeId projectionAmbientNode, projectionSourceRef),
                  (getNodeId projectionConsumerNode, projectionSourceRef),
                  (getNodeId projectionLocalNode, projectionSourceRef)
                ]
            projectionRootSubst =
              IntMap.fromList
                [ (getNodeId projectionAmbientNode, projectionAmbientRef),
                  (getNodeId projectionConsumerNode, projectionConsumerRef),
                  (getNodeId projectionLocalNode, projectionLocalRef)
                ]
            protectedIdentities =
              Set.fromList
                [ typeBinderRefIdentity projectionAmbientRef,
                  typeBinderRefIdentity projectionConsumerRef,
                  typeBinderRefIdentity projectionLocalRef
                ]
        projected <-
          requireRight
            ( projectPreparedSourceBinderSubstExceptForTest
                protectedIdentities
                ( IntSet.fromList
                    [ getNodeId projectionAmbientNode,
                      getNodeId projectionLocalNode
                    ]
                )
                (IntSet.singleton (getNodeId projectionLocalNode))
                preferredSourceRefs
                projectionRootSubst
            )
        IntMap.lookup (getNodeId projectionAmbientNode) projected
          `shouldBe` Just projectionSourceRef
        IntMap.lookup (getNodeId projectionConsumerNode) projected
          `shouldBe` Just projectionConsumerRef
        IntMap.lookup (getNodeId projectionLocalNode) projected
          `shouldBe` Just projectionLocalRef

    it "quotients prepared root binders only from authoritative substitution identity" $ do
      let graphA = NodeId 991850
          graphB = NodeId 991851
          graphRef node name =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) name
          refA = graphRef graphA "a"
          refB = graphRef graphB "b"
          scheme =
            mkElabSchemeWithRefs
              [(refA, Nothing), (refB, Nothing)]
              (TArrow (tVarWithRef refA) (tVarWithRef refB))
          sharedRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991852))
              "source"
          sharedSubst =
            IntMap.fromList
              [ (getNodeId graphA, sharedRef),
                (getNodeId graphB, sharedRef)
              ]
          sameSpellingLeft =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991853))
              "same"
          sameSpellingRight =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991854))
              "same"
          distinctSubst =
            IntMap.fromList
              [ (getNodeId graphA, sameSpellingLeft),
                (getNodeId graphB, sameSpellingRight)
              ]
      case applyPreparedRootBinderSubstForTest sharedSubst scheme of
        Left err -> expectationFailure ("prepared-root quotient failed: " ++ show err)
        Right preparedScheme -> do
          map fst (schemeBinderRefs preparedScheme) `shouldBe` [sharedRef]
          schemeBody preparedScheme
            `shouldBe` TArrow (tVarWithRef sharedRef) (tVarWithRef sharedRef)
      case applyPreparedRootBinderSubstForTest distinctSubst scheme of
        Left err -> expectationFailure ("distinct prepared-root identities failed: " ++ show err)
        Right preparedScheme -> do
          map fst (schemeBinderRefs preparedScheme)
            `shouldBe` [sameSpellingLeft, sameSpellingRight]
          schemeBody preparedScheme
            `shouldBe` TArrow
              (tVarWithRef sameSpellingLeft)
              (tVarWithRef sameSpellingRight)

    it "projects a free external binder through its exact declaration-copy key" $ do
      let declarationNode = NodeId 993113
          declarationRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode declarationNode)
              "external-copy"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 993114))
              "external-source"
          scheme =
            mkElabSchemeWithRefs
              []
              (TArrow (tVarWithRef declarationRef) (tVarWithRef declarationRef))
          projected =
            projectPreparedRootFreeSourceDeclarationCopiesForTest
              Set.empty
              (IntSet.singleton (getNodeId declarationNode))
              IntSet.empty
              (IntMap.singleton (getNodeId declarationNode) sourceRef)
              scheme
      schemeBinderRefs projected `shouldBe` []
      schemeBody projected
        `shouldBe` TArrow (tVarWithRef sourceRef) (tVarWithRef sourceRef)

    it "does not project a locally bound declaration through the source sidecar" $ do
      let declarationNode = NodeId 993115
          declarationRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode declarationNode)
              "local-copy"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 993116))
              "external-source"
          scheme =
            mkElabSchemeWithRefs
              [(declarationRef, Nothing)]
              (TArrow (tVarWithRef declarationRef) (tVarWithRef declarationRef))
      projectPreparedRootFreeSourceDeclarationCopiesForTest
        Set.empty
        (IntSet.singleton (getNodeId declarationNode))
        IntSet.empty
        (IntMap.singleton (getNodeId declarationNode) sourceRef)
        scheme
        `shouldBe` scheme

    it "does not project a same-named source binder from another declaration copy" $ do
      let freeNode = NodeId 993117
          otherDeclarationNode = NodeId 993118
          freeRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode freeNode)
              "same"
          unrelatedSourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 993119))
              "same"
          scheme =
            mkElabSchemeWithRefs
              []
              (TArrow (tVarWithRef freeRef) (tVarWithRef freeRef))
      projectPreparedRootFreeSourceDeclarationCopiesForTest
        Set.empty
        (IntSet.singleton (getNodeId otherDeclarationNode))
        IntSet.empty
        (IntMap.singleton (getNodeId otherDeclarationNode) unrelatedSourceRef)
        scheme
        `shouldBe` scheme

    it "updates a stale root alias when the same closure projects its binder identity" $ do
      let routeKey = 993106
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId routeKey))
              "graph"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 993107))
              "source"
          sourceRefs = IntMap.singleton routeKey sourceRef
          staleAliases = IntMap.singleton routeKey graphRef
      reconcileRootSourceBinderAliasesForTest
        [graphRef]
        [sourceRef]
        sourceRefs
        staleAliases
        `shouldBe` Right (IntMap.singleton routeKey sourceRef)

    it "uses an exact graph route when the projected closure still lists the old binder" $ do
      let routeKey = 993108
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId routeKey))
              "graph"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 993109))
              "source"
          sourceRefs = IntMap.singleton routeKey sourceRef
          staleAliases = IntMap.singleton routeKey graphRef
      reconcileRootSourceBinderAliasesForTest
        [graphRef]
        [sourceRef, graphRef]
        sourceRefs
        staleAliases
        `shouldBe` Right (IntMap.singleton routeKey sourceRef)

    it "rejects a source alias conflict with an unrelated pre-projection binder" $ do
      let routeKey = 993110
          unrelatedGraphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId 993111))
              "same"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 993112))
              "same"
          sourceRefs = IntMap.singleton routeKey sourceRef
          conflictingAliases =
            IntMap.singleton routeKey unrelatedGraphRef
      case
          reconcileRootSourceBinderAliasesForTest
            [unrelatedGraphRef]
            [sourceRef, unrelatedGraphRef]
            sourceRefs
            conflictingAliases
        of
          Left (ValidationFailed messages) ->
            messages
              `shouldSatisfy` any
                (isInfixOf "root construction source alias conflicts")
          Left other ->
            expectationFailure
              ("expected unrelated root alias conflict, got " ++ show other)
          Right aliases ->
            expectationFailure
              ("expected unrelated root alias rejection, got " ++ show aliases)

    it "orders a projected source binder before a bound that captures it" $ do
      let graphSource = NodeId 991855
          graphConsumer = NodeId 991856
          graphRef node name =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) name
          sourceGraphRef = graphRef graphSource "source-graph"
          consumerRef = graphRef graphConsumer "consumer"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991857))
              "source"
          consumerBound =
            TArrow (tVarWithRef sourceRef) (tVarWithRef sourceRef)
          scheme =
            mkElabSchemeWithRefs
              [(consumerRef, Just consumerBound), (sourceGraphRef, Nothing)]
              (TArrow (tVarWithRef sourceGraphRef) (tVarWithRef consumerRef))
          subst =
            IntMap.fromList
              [ (getNodeId graphSource, sourceRef),
                (getNodeId graphConsumer, consumerRef)
              ]
      case applyPreparedRootBinderSubstForTest subst scheme of
        Left err -> expectationFailure ("projected dependency ordering failed: " ++ show err)
        Right preparedScheme -> do
          map fst (schemeBinderRefs preparedScheme)
            `shouldBe` [sourceRef, consumerRef]
          schemeBody preparedScheme
            `shouldBe` TArrow (tVarWithRef sourceRef) (tVarWithRef consumerRef)

    it "preserves input order for independent projected root binders" $ do
      let graphFirst = NodeId 993102
          graphSecond = NodeId 993100
          graphThird = NodeId 993101
          graphRef node name =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) name
          firstGraphRef = graphRef graphFirst "first-graph"
          secondGraphRef = graphRef graphSecond "second-graph"
          thirdGraphRef = graphRef graphThird "third-graph"
          sourceRef unique name =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity unique))
              name
          firstSourceRef = sourceRef 993105 "first"
          secondSourceRef = sourceRef 993103 "second"
          thirdSourceRef = sourceRef 993104 "third"
          scheme =
            mkElabSchemeWithRefs
              [ (firstGraphRef, Nothing),
                (secondGraphRef, Nothing),
                (thirdGraphRef, Nothing)
              ]
              ( TArrow
                  (tVarWithRef firstGraphRef)
                  (TArrow (tVarWithRef secondGraphRef) (tVarWithRef thirdGraphRef))
              )
          subst =
            IntMap.fromList
              [ (getNodeId graphFirst, firstSourceRef),
                (getNodeId graphSecond, secondSourceRef),
                (getNodeId graphThird, thirdSourceRef)
              ]
      case applyPreparedRootBinderSubstForTest subst scheme of
        Left err -> expectationFailure ("stable projected binder ordering failed: " ++ show err)
        Right preparedScheme -> do
          map fst (schemeBinderRefs preparedScheme)
            `shouldBe` [firstSourceRef, secondSourceRef, thirdSourceRef]
          schemeBody preparedScheme
            `shouldBe` TArrow
              (tVarWithRef firstSourceRef)
              (TArrow (tVarWithRef secondSourceRef) (tVarWithRef thirdSourceRef))

    it "does not expand a root substitution with unrelated global source routes" $ do
      let graphNode = NodeId 161
          unrelatedSourceNode = NodeId 207
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992101))
              "source"
          existingSubst =
            IntMap.singleton (getNodeId graphNode) graphRef
          preferredRefs =
            IntMap.singleton (getNodeId unrelatedSourceNode) sourceRef
      projectPreparedSourceBinderSubstExceptForTest
        Set.empty
        IntSet.empty
        IntSet.empty
        preferredRefs
        existingSubst
        `shouldBe` Right existingSubst

    it "lets only a direct source declaration replace a root graph placeholder" $ do
      let sourceNode = NodeId 992100
          sourceNodeKey = getNodeId sourceNode
          rootGraphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode sourceNode)
              "root-graph"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992101))
              "source"
          sourceRefs = IntMap.singleton sourceNodeKey sourceRef
          rootSubst = IntMap.singleton sourceNodeKey rootGraphRef
          alias = (sourceNodeKey, sourceRef)
      insertPreparedTermSourceBinderAliasForTest
        (IntSet.singleton sourceNodeKey)
        sourceRefs
        rootSubst
        alias
        `shouldBe` Right (IntMap.singleton sourceNodeKey sourceRef)
      case
          insertPreparedTermSourceBinderAliasForTest
            IntSet.empty
            sourceRefs
            rootSubst
            alias
        of
        Left (ValidationFailed messages) ->
          messages
            `shouldSatisfy` any
              (isInfixOf "prepared root and source-binder substitutions disagree")
        Left err ->
          expectationFailure
            ("expected expanded-only source alias rejection, got " ++ show err)
        Right projected ->
          expectationFailure
            ("expected expanded-only source alias rejection, got " ++ show projected)

    it "projects every existing route in a shared source alias class" $ do
      let graphNode = NodeId 161
          sharedSourceNode = NodeId 207
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992102))
              "source"
          existingSubst =
            IntMap.fromList
              [ (getNodeId graphNode, graphRef),
                (getNodeId sharedSourceNode, graphRef)
              ]
          preferredRefs =
            IntMap.singleton (getNodeId sharedSourceNode) sourceRef
      case
        projectPreparedSourceBinderSubstExceptForTest
          Set.empty
          IntSet.empty
          IntSet.empty
          preferredRefs
          existingSubst
        of
        Left err -> expectationFailure ("source alias-class projection failed: " ++ show err)
        Right projected -> do
          IntMap.keys projected `shouldBe` IntMap.keys existingSubst
          IntMap.lookup (getNodeId graphNode) projected `shouldBe` Just sourceRef
          IntMap.lookup (getNodeId sharedSourceNode) projected `shouldBe` Just sourceRef

    it "rejects conflicting source projections for one existing alias class" $ do
      let graphNode = NodeId 161
          firstSourceNode = NodeId 207
          secondSourceNode = NodeId 208
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          firstSourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992103))
              "firstSource"
          secondSourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992104))
              "secondSource"
          existingSubst =
            IntMap.fromList
              [ (getNodeId firstSourceNode, graphRef),
                (getNodeId secondSourceNode, graphRef)
              ]
          preferredRefs =
            IntMap.fromList
              [ (getNodeId firstSourceNode, firstSourceRef),
                (getNodeId secondSourceNode, secondSourceRef)
              ]
      case
        projectPreparedSourceBinderSubstExceptForTest
          Set.empty
          IntSet.empty
          IntSet.empty
          preferredRefs
          existingSubst
        of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "conflicting identities for one substitution alias class")
        Left err -> expectationFailure ("expected source alias-class conflict, got " ++ show err)
        Right projected -> expectationFailure ("expected source alias-class rejection, got " ++ show projected)

    it "keeps a raw compiler-exact type unchanged for an explicit empty edge plan" $ do
      let edge = EdgeId 992105
          graphNode = NodeId 161
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          unrelatedRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992106))
              "unrelated"
          rawType = tVarWithRef graphRef
          trace = compilerExactTrace graphNode []
      prepareCompilerExactEdgePlansForTest
        (IntMap.singleton (getEdgeId edge) rawType)
        (IntMap.singleton (getEdgeId edge) trace)
        (IntMap.singleton (getNodeId graphNode) unrelatedRef)
        `shouldBe` Right (IntMap.singleton (getEdgeId edge) (rawType, IntMap.empty))

    it "keeps compiler-exact binder routes isolated between sibling edges" $ do
      let firstEdge = EdgeId 992107
          secondEdge = EdgeId 992108
          producer = NodeId 161
          firstArgument = NodeId 162
          secondArgument = NodeId 163
          firstRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992109))
              "first"
          secondRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992110))
              "second"
          exactTypes =
            IntMap.fromList
              [ (getEdgeId firstEdge, tVarWithRef firstRef),
                (getEdgeId secondEdge, tVarWithRef secondRef)
              ]
          traces =
            IntMap.fromList
              [ (getEdgeId firstEdge, compilerExactTrace producer [(producer, firstArgument)]),
                (getEdgeId secondEdge, compilerExactTrace producer [(producer, secondArgument)])
              ]
          sourceRefs =
            IntMap.fromList
              [ (getNodeId firstArgument, firstRef),
                (getNodeId secondArgument, secondRef)
              ]
      prepareCompilerExactEdgePlansForTest exactTypes traces sourceRefs
        `shouldBe`
          Right
            ( IntMap.fromList
                [ ( getEdgeId firstEdge,
                    (tVarWithRef firstRef, IntMap.singleton (getNodeId producer) firstRef)
                  ),
                  ( getEdgeId secondEdge,
                    (tVarWithRef secondRef, IntMap.singleton (getNodeId producer) secondRef)
                  )
                ]
            )

    it "rejects a compiler-exact type whose edge trace is missing" $ do
      let edge = EdgeId 992111
      prepareCompilerExactEdgePlansForTest
        (IntMap.singleton (getEdgeId edge) TBottom)
        IntMap.empty
        IntMap.empty
        `shouldBe` Left (MissingEdgeTrace edge)

    it "rejects conflicting binder routes within one compiler-exact edge" $ do
      let edge = EdgeId 992112
          producer = NodeId 161
          firstArgument = NodeId 162
          secondArgument = NodeId 163
          firstRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992113))
              "first"
          secondRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992114))
              "second"
          trace =
            compilerExactTrace
              producer
              [(producer, firstArgument), (producer, secondArgument)]
          sourceRefs =
            IntMap.fromList
              [ (getNodeId firstArgument, firstRef),
                (getNodeId secondArgument, secondRef)
              ]
      case
          prepareCompilerExactEdgePlansForTest
            (IntMap.singleton (getEdgeId edge) (TArrow (tVarWithRef firstRef) (tVarWithRef secondRef)))
            (IntMap.singleton (getEdgeId edge) trace)
            sourceRefs
        of
          Left (ValidationFailed messages) ->
            messages `shouldSatisfy` any (isInfixOf "maps one construction binder to multiple identities")
          Left err -> expectationFailure ("expected edge-local binder route conflict, got " ++ show err)
          Right plans -> expectationFailure ("expected edge-local route rejection, got " ++ show plans)

    it "installs an absent compiler-exact root binder capture" $ do
      let graphNode = NodeId 991860
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          exactRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991861))
              "source"
          exactType =
            TForallRef exactRef Nothing
              (TArrow (tVarWithRef exactRef) TBottom)
          rootScheme =
            mkElabSchemeWithRefs
              [(graphRef, Nothing)]
              (TArrow (tVarWithRef exactRef) TBottom)
      prepareCompilerExactRootBinderSubstForTest
        exactType
        rootScheme
        IntMap.empty
        `shouldBe` Right (IntMap.singleton (getNodeId graphNode) exactRef)

    it "projects a non-vacuous compiler-exact root binder by exact body identity" $ do
      let graphNode = NodeId 991873
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          exactRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991874))
              "source"
          exactType =
            TForallRef exactRef Nothing
              (TArrow (tVarWithRef exactRef) TBottom)
          rootScheme =
            mkElabSchemeWithRefs
              [(graphRef, Nothing)]
              (TArrow (tVarWithRef graphRef) TBottom)
      prepareCompilerExactRootBinderSubstForTest
        exactType
        rootScheme
        IntMap.empty
        `shouldBe` Right (IntMap.singleton (getNodeId graphNode) exactRef)

    it "propagates a compiler-exact root binder capture through its graph alias class" $ do
      let graphNode = NodeId 991862
          firstAlias = NodeId 991863
          secondAlias = NodeId 991864
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          exactRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991865))
              "source"
          exactType =
            TForallRef exactRef Nothing
              (TArrow (tVarWithRef exactRef) TBottom)
          rootScheme =
            mkElabSchemeWithRefs
              [(graphRef, Nothing)]
              (TArrow (tVarWithRef exactRef) TBottom)
          existingSubst =
            IntMap.fromList
              [ (getNodeId firstAlias, graphRef),
                (getNodeId secondAlias, graphRef)
              ]
      case
          prepareCompilerExactRootBinderSubstForTest
            exactType
            rootScheme
            existingSubst
        of
          Left err -> expectationFailure ("compiler-exact root alias projection failed: " ++ show err)
          Right preparedSubst ->
            forM_ [graphNode, firstAlias, secondAlias] $ \node ->
              IntMap.lookup (getNodeId node) preparedSubst `shouldBe` Just exactRef

    it "does not capture a compiler-exact root binder used by its root body" $ do
      let graphNode = NodeId 991866
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          exactRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991867))
              "source"
          exactType =
            TForallRef exactRef Nothing
              (TArrow (tVarWithRef exactRef) TBottom)
          rootScheme =
            mkElabSchemeWithRefs
              [(graphRef, Nothing)]
              (TArrow (tVarWithRef graphRef) (tVarWithRef exactRef))
      prepareCompilerExactRootBinderSubstForTest
        exactType
        rootScheme
        IntMap.empty
        `shouldBe` Right IntMap.empty

    it "does not capture a compiler-exact root binder when its exact identity is absent" $ do
      let graphNode = NodeId 991868
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          exactRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991869))
              "source"
          exactType = TForallRef exactRef Nothing TBottom
          rootScheme =
            mkElabSchemeWithRefs
              [(graphRef, Nothing)]
              TBottom
      prepareCompilerExactRootBinderSubstForTest
        exactType
        rootScheme
        IntMap.empty
        `shouldBe` Right IntMap.empty

    it "rejects an incompatible compiler-exact root binder capture route" $ do
      let graphNode = NodeId 991870
          graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode graphNode)
              "graph"
          exactRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991871))
              "source"
          incompatibleRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 991872))
              "incompatible"
          exactType =
            TForallRef exactRef Nothing
              (TArrow (tVarWithRef exactRef) TBottom)
          rootScheme =
            mkElabSchemeWithRefs
              [(graphRef, Nothing)]
              (TArrow (tVarWithRef exactRef) TBottom)
          existingSubst =
            IntMap.singleton (getNodeId graphNode) incompatibleRef
      case
          prepareCompilerExactRootBinderSubstForTest
            exactType
            rootScheme
            existingSubst
        of
          Left (ValidationFailed messages) ->
            messages `shouldSatisfy` any (isInfixOf "conflicts with existing root binder route")
          Left err -> expectationFailure ("expected compiler-exact root route conflict, got " ++ show err)
          Right preparedSubst -> expectationFailure ("expected compiler-exact root route rejection, got " ++ show preparedSubst)

    it "rejects conflicting source identities in one solved class" $ do
      let firstAlias = NodeId 30
          secondAlias = NodeId 31
          solvedNode = NodeId 130
          firstRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992002))
              "first"
          secondRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992003))
              "second"
          baseToSolved =
            IntMap.fromList
              [ (getNodeId firstAlias, solvedNode),
                (getNodeId secondAlias, solvedNode)
              ]
          directRefs =
            IntMap.fromList
              [ (getNodeId firstAlias, firstRef),
                (getNodeId secondAlias, secondRef)
              ]
      case expandSourceBinderRefsForTest id baseToSolved directRefs of
        Left (ValidationFailed messages) ->
          messages `shouldSatisfy` any (isInfixOf "conflicting source-binder identities")
        Left err -> expectationFailure ("expected source-identity conflict, got " ++ show err)
        Right expanded -> expectationFailure ("expected source-identity conflict, got " ++ show expanded)

    it "routes a solved class through its scope-owned base identity" $ do
      let firstAlias = NodeId 40
          preferredAlias = NodeId 41
          solvedNode = NodeId 140
          firstRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992004))
              "first"
          preferredRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992005))
              "preferred"
          baseToSolved =
            IntMap.fromList
              [ (getNodeId firstAlias, solvedNode),
                (getNodeId preferredAlias, solvedNode)
              ]
          directRefs =
            IntMap.fromList
              [ (getNodeId firstAlias, firstRef),
                (getNodeId preferredAlias, preferredRef)
              ]
          preference =
            IntMap.singleton
              (getNodeId solvedNode)
              preferredAlias
      case
          expandSourceBinderRefsWithPreferenceForTest
            preference
            id
            baseToSolved
            directRefs
        of
          Left err -> expectationFailure ("preferred source identity expansion failed: " ++ show err)
          Right expanded ->
            forM_ [firstAlias, preferredAlias, solvedNode] $ \node ->
              case IntMap.lookup (getNodeId node) expanded of
                Nothing -> expectationFailure ("missing preferred source identity at " ++ show node)
                Just actualRef ->
                  typeBinderRefsSameIdentity actualRef preferredRef `shouldBe` True

    it "uses the selected live identity when the structural base has no source binder" $ do
      let structuralBase = NodeId 42
          otherAlias = NodeId 43
          solvedNode = NodeId 150
          otherRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992006))
              "other"
          representativeRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992007))
              "representative"
          baseToSolved =
            IntMap.fromList
              [ (getNodeId structuralBase, solvedNode),
                (getNodeId otherAlias, solvedNode)
              ]
          directRefs =
            IntMap.fromList
              [ (getNodeId otherAlias, otherRef),
                (getNodeId solvedNode, representativeRef)
              ]
          preference =
            IntMap.singleton
              (getNodeId solvedNode)
              structuralBase
      case
          expandSourceBinderRefsWithPreferenceForTest
            preference
            id
            baseToSolved
            directRefs
        of
          Left err -> expectationFailure ("representative source identity expansion failed: " ++ show err)
          Right expanded ->
            forM_ [structuralBase, otherAlias, solvedNode] $ \node ->
              case IntMap.lookup (getNodeId node) expanded of
                Nothing -> expectationFailure ("missing representative source identity at " ++ show node)
                Just actualRef ->
                  typeBinderRefsSameIdentity actualRef representativeRef `shouldBe` True

  describe "Integration Tests" $ do
    it "ResultTypeView owns bound-overlay reify, base-target projection, and target generalization" $ do
      let rootN = NodeId 0
          intN = NodeId 1
          baseRootN = NodeId 10
          baseIntN = NodeId 11
          intBase = BaseTy "Int"
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (rootN, TyVar {tnId = rootN, tnBound = Nothing}),
                      (intN, TestTyBase intN intBase)
                    ]
              }
          baseConstraint =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (baseRootN, TyVar {tnId = baseRootN, tnBound = Nothing}),
                      (baseIntN, TestTyBase baseIntN intBase)
                    ]
              }
          view0 = Finalize.presolutionViewFromSnapshot solvedConstraint IntMap.empty
          bindParentsGa =
            GaBindParents
              { gaBindParentsBase = cBindParents baseConstraint,
                gaBaseConstraint = baseConstraint,
                gaBaseToSolved = IntMap.fromList [(getNodeId baseRootN, rootN), (getNodeId baseIntN, intN)],
                gaSolvedToBase = IntMap.fromList [(getNodeId rootN, baseRootN), (getNodeId intN, baseIntN)],
                gaRestoredSchemeRootTargets = IntMap.empty,
                gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
              }
          inputs =
            mkResultTypeInputs
              id
              emptyEdgeArtifacts
              view0
              bindParentsGa
              (defaultPlanBuilder defaultTraceConfig)
              baseConstraint
              IntMap.empty
              defaultTraceConfig
      view <- requireRight (ResultTypeView.buildResultTypeView inputs)
      let viewBound = ResultTypeView.rtvWithBoundOverlay rootN intN view
      ResultTypeView.rtvLookupVarBound viewBound rootN `shouldBe` Just intN
      ResultTypeView.rtvDirectBoundTarget viewBound rootN `shouldBe` Just intN
      ResultTypeView.rtvReifyNoFallback viewBound rootN
        `shouldBe` Right (TestElab.tBase intBase)
      ResultTypeView.rtvReifyBaseNoFallback viewBound baseRootN
        `shouldBe` Right (TestElab.tBase intBase)
      (scheme, _subst) <- requireRight (ResultTypeView.rtvGeneralizeTarget viewBound (typeRef rootN) rootN)
      scheme `shouldBe` Elab.schemeFromType (TestElab.tBase intBase)

    it "single-solved refactor keeps canonical pipeline authoritative on representative corpus" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "keeps an unused applied let at its live ground result" $ do
      let expr =
            ELet
              "a"
              (EApp (ELam "x" (ELit (LInt 1))) (ELit (LInt 2)))
              (ELit (LBool False))
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure (renderPipelineError err)
        Right (term, ty) -> do
          ty `shouldBe` TestElab.tBase (BaseTy "Bool")
          typeCheck term `shouldBe` Right ty
          case term of
            Elab.ELet _ rhsScheme rhs body -> do
              let intTy = TestElab.tBase (BaseTy "Int")
                  rhsTy = Elab.schemeToType rhsScheme
              rhsTy `shouldBe` intTy
              typeCheck rhs `shouldBe` Right rhsTy
              containsGroundIntApplication rhs `shouldBe` True
              typeCheck body
                `shouldBe` Right (TestElab.tBase (BaseTy "Bool"))
            other ->
              expectationFailure
                ("expected the unused source let to remain in the checked term, got " ++ show other)

    it "chi-first ResultType|checked-authoritative keeps representative corpus parity" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "checked-authoritative keeps representative corpus parity" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "Phase 6 — Elaborate|ResultType|Dual-path verification gate stays green" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "migration guardrail: thesis-core boundary matches legacy outcome" $ do
      forM_ representativeMigrationCorpus $ \expr -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let pres = paPresolution artifacts
            legacy = paSolved artifacts
        view <-
          requireRight
            ( Finalize.finalizePresolutionViewFromSnapshot
                (snapshotConstraint pres)
                (snapshotUnionFind pres)
            )
        validateStrict legacy
        assertViewParity view legacy

    describe "Dual-path verification" $ do
      it "production entrypoint remains checked-authoritative on representative corpus" $ do
        forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

      it "opt-in result-type diagnostics still clear the representative corpus" $ do
        forM_ representativeMigrationCorpus assertDiagnosticPipelineTypeChecks

    it "shared solved-to-presolution adapter matches selected solved queries on representative corpus" $ do
      let corpus =
            [ ELam "x" (EVar "x"),
              ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))),
              EAnn
                (ELam "x" (EVar "x"))
                (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
            ]
      forM_ corpus $ \expr -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let solved = paSolved artifacts
            view = viewFromSolved solved
            nodeIds = map fst (toListNode (cNodes (Solved.originalConstraint solved)))
            probeIds = nodeIds ++ [NodeId 999]
            probeRefs = map typeRef probeIds

        pvConstraint view `shouldBe` Solved.originalConstraint solved
        pvCanonicalMap view `shouldBe` Solved.canonicalMap solved
        pvBindParents view `shouldBe` cBindParents (Solved.originalConstraint solved)
        pvCanonicalConstraint view `shouldBe` Solved.canonicalConstraint solved

        forM_ probeIds $ \nid -> do
          pvCanonical view nid `shouldBe` Solved.canonical solved nid
          pvLookupNode view nid
            `shouldBe` NodeAccess.lookupNode (Solved.originalConstraint solved) (Solved.canonical solved nid)
          pvLookupVarBound view nid
            `shouldBe` NodeAccess.lookupVarBound (Solved.originalConstraint solved) (Solved.canonical solved nid)

        forM_ probeRefs $ \ref ->
          pvLookupBindParent view ref
            `shouldBe` NodeAccess.lookupBindParent (Solved.originalConstraint solved) ref

    it "runtime snapshot rebuild stays stable across representative corpus" $ do
      let corpus =
            [ ELam "x" (EVar "x"),
              ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")),
              EAnn
                (ELam "x" (EVar "x"))
                (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
            ]
      forM_ corpus $ \expr -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let pres = paPresolution artifacts
        expected <-
          requireRight
            (SolvedTest.solvedFromSnapshot (snapshotUnionFind pres) (snapshotConstraint pres))
        paSolved artifacts `shouldBe` expected
        Solved.validateCanonicalGraphStrict (paSolved artifacts)
          `shouldBe` []
        runPipelineElab Set.empty (unsafeNormalizeExpr expr)
          `shouldSatisfy` isRight

    it "keeps recursive lets out of the Phase 3 cycle-error path" $ do
      let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err ->
          renderPipelineError err
            `shouldSatisfy` (not . isInfixOf "Phase 3 (acyclicity)")
        Right _ -> pure ()

    it "keeps the non-recursive identity control stable" $ do
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
        Right (_term, ty) -> do
          containsMu ty `shouldBe` False
          ty `shouldSatisfy` containsArrowTy
          ty `shouldSatisfy` containsForallTy

    describe "Automatic μ-introduction (item-2)" $ do
      it "self-recursive function infers μ on the canonical pipeline entrypoint" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        cBroken `shouldSatisfy` constraintContainsTyMu

      it "nested-let mutually recursive aliases stay Phase-3-safe even when no structural μ rewrite is needed" $ do
        let expr =
              ELet
                "f"
                (ELet "g" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "g"))
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        cBroken `shouldNotSatisfy` constraintContainsTyMu

      it "recursive data-like constructor shape stays Phase-3-safe even when no structural μ rewrite is needed" $ do
        let expr =
              ELet
                "lst"
                (ELam "x" (ELam "xs" (EApp (EApp (EVar "lst") (EVar "x")) (EVar "xs"))))
                (EVar "lst")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        cBroken `shouldNotSatisfy` constraintContainsTyMu

      it "non-recursive control expression stays μ-free on the canonical pipeline entrypoint" $ do
        let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` False
        ty `shouldSatisfy` containsArrowTy
        ty `shouldSatisfy` containsForallTy
        cBroken <- automaticMuConstraint expr
        cBroken `shouldNotSatisfy` constraintContainsTyMu

    describe "Automatic μ-introduction (item-3)" $ do
      it "elaborates recursive uses with explicit ERoll/EUnroll and passes Phase 7" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, ty) -> do
            unless (containsMu ty) $
              expectationFailure
                ( "expected TMu in type, got: "
                    ++ show ty
                    ++ " term: "
                    ++ show term
                )
            unless (containsRollTerm term) $
              expectationFailure ("expected ERoll in term: " ++ show term)
            unless (containsUnrollTerm term) $
              expectationFailure ("expected EUnroll in term: " ++ show term)
            typeCheck term `shouldBe` Right ty

    describe "Automatic μ-introduction (item-4 edge cases)" $ do
      it "preserves returned nested recursive helper fixed points on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "g" (ELam "y" (EApp (EVar "f") (EApp (EVar "g") (EVar "y")))) (EVar "g")))
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` True
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " nested recursive helper fixed point: expected recursive success, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              case strippedTy of
                TArrow dom cod -> do
                  unless (containsMu dom && matchesRecursiveMu dom cod) $
                    expectationFailure
                      ( label
                          ++ " nested recursive helper fixed point: expected matching recursive domain/codomain after stripping leading foralls, got "
                          ++ show ty
                      )
                _ ->
                  expectationFailure
                    ( label
                        ++ " nested recursive helper fixed point: expected arrow type after stripping leading foralls, got "
                        ++ show ty
                    )
              typeCheck term `shouldBe` Right ty

      it "rejects returned helpers whose recursive calls imply incompatible outer carriers" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "g"
                        ( ELam
                            "y"
                            ( ELet
                                "_"
                                (EApp (EVar "f") (EApp (EVar "g") (EVar "y")))
                                (EApp (EVar "f") (ELit (LInt 0)))
                            )
                        )
                        (EVar "g")
                    )
                )
                (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err ->
            renderPipelineError err
              `shouldSatisfy` isInfixOf "incompatible structural equations"
          Right result ->
            expectationFailure
              ("expected incompatible returned-helper carriers to be rejected, got " ++ show result)

      it "derives distinct automatic mu binder identities from sibling raw owners" $ do
        let recursiveBinding name param result =
              ELet
                "_"
                (EApp (EVar name) (EVar param))
                result
            expr =
              ELet
                "f"
                (ELam "x" (recursiveBinding "f" "x" (ELit (LInt 0))))
                ( ELet
                    "g"
                    (ELam "y" (recursiveBinding "g" "y" (ELit (LBool True))))
                    (EVar "g")
                )
            collectMuBinderRefs ty =
              case ty of
                TMuRef ref body -> ref : collectMuBinderRefs body
                TArrow domain codomain ->
                  collectMuBinderRefs domain ++ collectMuBinderRefs codomain
                TConWithIdentity _ _ arguments -> concatMap collectMuBinderRefs arguments
                TForallRef _ mbBound body ->
                  maybe [] (collectMuBinderRefs . tyToElab) mbBound
                    ++ collectMuBinderRefs body
                _ -> []
            collectLetSchemeMuBinderRefs term =
              case term of
                Elab.ELet _ scheme rhs body ->
                  collectMuBinderRefs (schemeBody scheme)
                    ++ collectLetSchemeMuBinderRefs rhs
                    ++ collectLetSchemeMuBinderRefs body
                Elab.ELam _ body -> collectLetSchemeMuBinderRefs body
                Elab.EApp function argument ->
                  collectLetSchemeMuBinderRefs function
                    ++ collectLetSchemeMuBinderRefs argument
                Elab.ETyAbsRef _ _ body -> collectLetSchemeMuBinderRefs body
                Elab.ETyInst body _ -> collectLetSchemeMuBinderRefs body
                Elab.ERoll _ body -> collectLetSchemeMuBinderRefs body
                Elab.EUnroll body -> collectLetSchemeMuBinderRefs body
                Elab.EVarNode _ -> []
                Elab.ELit _ -> []
        (term, _) <- expectCanonicalPipelineSuccess expr
        let muBinderIdentities =
              nub (map typeBinderRefIdentity (collectLetSchemeMuBinderRefs term))
        length muBinderIdentities `shouldSatisfy` (>= 2)

      it "characterizes polymorphic recursion with annotation without Phase-3 regression" $ do
        let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            expr =
              ELet
                "f"
                (EAnn (ELam "x" (EApp (EVar "f") (EVar "x"))) ann)
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` False
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> do
            let rendered = renderPipelineError err
            rendered `shouldSatisfy` (not . isInfixOf "Phase 3 (acyclicity)")
          Right (term, ty) ->
            typeCheck term `shouldBe` Right ty

      it "preserves visible μ across μ/∀ interaction when a contractive recursive witness already exists" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "f" (ELam "x" (EApp (EVar "f") (EApp (EVar "id") (EVar "x")))) (EVar "f"))
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` True
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                (label ++ " μ/∀ interaction: expected visible μ, got error " ++ renderPipelineError err)
            Right (term, ty) -> do
              unless (containsMu ty) $
                expectationFailure
                  (label ++ " μ/∀ interaction: expected TMu in type, got " ++ show ty)
              typeCheck term `shouldBe` Right ty

      it "keeps μ/∀ mediation fail-closed without a contractive recursive witness" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "f" (EApp (EVar "id") (EVar "f")) (EVar "f"))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left _ -> pure ()
            Right (term, ty) -> do
              when (containsMu ty) $
                expectationFailure
                  (label ++ " μ/∀ mediation without a recursive witness unexpectedly preserved " ++ show ty)
              typeCheck term `shouldBe` Right ty

      it "keeps non-contractive μ annotations out of the mediated witness lane" $ do
        let badRecursiveAnn = STMu "a" (STVar "a")
            expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "g" (EApp (EVar "id") (ELamAnn "x" badRecursiveAnn (EVar "x"))) (EVar "g"))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left _ -> pure ()
            Right (_term, ty) ->
              expectationFailure
                (label ++ " unexpectedly accepted non-contractive mediated μ annotation with type " ++ show ty)

      it "URI-R2-C1 unannotated carrier: direct recursiveArrowInt admits a visible recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                (EVar "f")
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 direct carrier: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 direct carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 unannotated carrier: direct recursiveArrowBool admits a visible recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                (EVar "f")
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 direct carrier: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 direct carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 uniqueness reject: witnessless mediation stays fail-closed without a contractive recursive witness" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "f" (EApp (EVar "id") (EVar "f")) (EVar "f"))
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left _ -> pure ()
            Right (term, ty) -> do
              containsMu ty `shouldBe` False
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 non-local identity consumer: direct id application preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                    (EApp (EVar "id") (EVar "f"))
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 non-local identity consumer: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 non-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 non-local identity consumer: direct id application preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                    (EApp (EVar "id") (EVar "f"))
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 non-local identity consumer: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 non-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 owner-local identity consumer: let-aliased recursive Int carrier survives id application on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                    (ELet "hold" (EVar "f") (EApp (EVar "id") (EVar "hold")))
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 owner-local identity consumer: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 owner-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 owner-local identity consumer: let-aliased recursive Bool carrier survives id application on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                    (ELet "hold" (EVar "f") (EApp (EVar "id") (EVar "hold")))
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 owner-local identity consumer: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 owner-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 identity consumer wrapper: named wrap preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EVar "u")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 identity consumer wrapper: named wrap preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EVar "u")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested identity consumer wrapper: repeated id application preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EApp (EVar "id") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested identity consumer wrapper: repeated id application preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EApp (EVar "id") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 staged identity consumer wrapper: let-bound id result preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "k" (EApp (EVar "id") (EVar "u")) (EVar "k")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 staged identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 staged identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 staged identity consumer wrapper: let-bound id result preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "k" (EApp (EVar "id") (EVar "u")) (EVar "k")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 staged identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 staged identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 local helper identity consumer wrapper: let-bound helper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "use" (ELam "v" (EApp (EVar "id") (EVar "v"))) (EApp (EVar "use") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 local helper identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 local helper identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 local helper identity consumer wrapper: let-bound helper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "use" (ELam "v" (EApp (EVar "id") (EVar "v"))) (EApp (EVar "use") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 local helper identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 local helper identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 reconstruction: same-lane alias wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                (ELet "hold" (EVar "f") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 reconstruction: same-lane alias wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                (ELet "hold" (EVar "f") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested-forall carrier: same-wrapper identity preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                    (EApp (ELam "y" (EVar "y")) (EVar "f"))
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested-forall carrier: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested-forall carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested-forall carrier: same-wrapper identity preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                    (EApp (ELam "y" (EVar "y")) (EVar "f"))
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested-forall carrier: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested-forall carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                    (EVar "f")
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                    (EVar "f")
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: let-aliased transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                    (EVar "f")
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Int carrier through let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: let-aliased transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                    (EVar "f")
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Bool carrier through let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      let uriR2C1OwnerSensitiveNonLocalTransparentIntRhs =
            ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))
          uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs =
            ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))
          transparentMediatorWrap =
            ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z")))
          aliasedTransparentMediatorWrap =
            ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z"))))
          ownerSensitiveNonLocalAliasChain aliases source =
            case aliases of
              [] ->
                ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
              aliasName : rest ->
                ELet aliasName (EVar source) (ownerSensitiveNonLocalAliasChain rest aliasName)
          ownerSensitiveNonLocalTransparentExpr wrap recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap"
                  wrap
                  ( ELet
                      "f"
                      (EApp (EVar "id") recursiveRhs)
                      (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EVar "hold"))
                  )
              )
          ownerSensitiveNonLocalStackedTransparentExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          (ELet "hold" (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f"))) (EVar "hold"))
                      )
                  )
              )
          ownerSensitiveNonLocalTransparentAliasChainExpr wrap recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap"
                  wrap
                  ( ELet
                      "f"
                      (EApp (EVar "id") recursiveRhs)
                      ( ELet
                          "hold"
                          (EApp (EVar "wrap") (EVar "f"))
                          (ownerSensitiveNonLocalAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")
                      )
                  )
              )
          ownerSensitiveNonLocalStackedTransparentAliasChainExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                              (ownerSensitiveNonLocalAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")
                          )
                      )
                  )
              )
          expectUriR2C1OwnerSensitiveNonLocalTransparentMediation label expectedCarrier expr = do
            let entryLabel = "canonical"
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err ->
                  expectationFailure
                    ( entryLabel
                        ++ " "
                        ++ label
                        ++ ": expected recursive carrier, got error "
                        ++ renderPipelineError err
                    )
                Right (term, ty) -> do
                  let strippedTy = stripLeadingUnboundedForalls ty
                  unless (matchesRecursiveMu strippedTy expectedCarrier) $
                    expectationFailure
                      ( entryLabel
                          ++ " "
                          ++ label
                          ++ ": expected "
                          ++ show expectedCarrier
                          ++ " after stripping leading foralls, got "
                          ++ show ty
                      )
                  typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      let combinedWrapperAliasChain aliases source =
            case aliases of
              [] ->
                ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
              aliasName : rest ->
                ELet aliasName (EVar source) (combinedWrapperAliasChain rest aliasName)
          uriR2C1CombinedWrapperStackedExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                              (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                          )
                      )
                  )
              )
          uriR2C1CombinedWrapperStackedAliasChainExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                              (combinedWrapperAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")
                          )
                      )
                  )
              )
          expectUriR2C1CombinedWrapper label expectedCarrier expr = do
            let entryLabel = "canonical"
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err ->
                  expectationFailure
                    ( entryLabel
                        ++ " "
                        ++ label
                        ++ ": expected recursive carrier, got error "
                        ++ renderPipelineError err
                    )
                Right (term, ty) -> do
                  let strippedTy = stripLeadingUnboundedForalls ty
                  unless (matchesRecursiveMu strippedTy expectedCarrier) $
                    expectationFailure
                      ( entryLabel
                          ++ " "
                          ++ label
                          ++ ": expected "
                          ++ show expectedCarrier
                          ++ " after stripping leading foralls, got "
                          ++ show ty
                      )
                  typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through identity consumer plus transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through identity consumer plus transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through identity consumer plus let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through identity consumer plus let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through let-aliased transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through let-aliased transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "wrap2"
                        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through stacked transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "wrap2"
                        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through stacked transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through stacked let-aliased transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through stacked let-aliased transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 reconstruction: deeper same-lane alias chain preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                ( ELet
                    "hold"
                    (EVar "f")
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
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Int carrier through deeper alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 reconstruction: deeper same-lane alias chain preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                ( ELet
                    "hold"
                    (EVar "f")
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
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Bool carrier through deeper alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested recursive helper: preserves recursive Int codomain on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "g"
                        (ELam "y" (ELet "_" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))) (ELit (LInt 0))))
                        (EVar "g")
                    )
                )
                (EVar "f")
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested recursive helper: expected recursive Int codomain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveArrowCodomain strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested recursive helper: expected codomain "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested recursive helper: preserves recursive Bool codomain on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "g"
                        (ELam "y" (ELet "_" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))) (ELit (LBool True))))
                        (EVar "g")
                    )
                )
                (EVar "f")
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested recursive helper: expected recursive Bool codomain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveArrowCodomain strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested recursive helper: expected codomain "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 ambiguity reject: direct self-app and returned-helper clusters stay fail-closed on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "_"
                        (EApp (EVar "f") (EVar "x"))
                        ( ELet
                            "g"
                            (ELam "y" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))))
                            (EVar "g")
                        )
                    )
                )
                (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left _ -> pure ()
            Right (term, ty) -> do
              containsMu ty `shouldBe` False
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 higher-order recursion: preserves visible recursive structure on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELam "y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))))
                (EVar "f")
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 higher-order recursion: expected visible recursive structure, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              unless (containsMu ty) $
                expectationFailure
                  (label ++ " URI-R2-C1 higher-order recursion: expected TMu in type, got " ++ show ty)
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 recursive data-like constructor shape: preserves visible recursive structure on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "lst"
                (ELam "x" (ELam "xs" (EApp (EApp (EVar "lst") (EVar "x")) (EVar "xs"))))
                (EVar "lst")
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 recursive data-like constructor shape: expected visible recursive structure, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              unless (containsMu ty) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 recursive data-like constructor shape: expected TMu in type, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "characterizes higher-order recursion as preserving typechecked output without a distinct automaticMuConstraint witness" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELam "y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))))
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` False
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err -> do
              let rendered = renderPipelineError err
              -- Alias-bound resolution fix means the old "alias bounds survived"
              -- error is gone; the new blocker is PhiTranslatabilityError.
              rendered `shouldSatisfy` (not . isInfixOf "alias bounds survived scheme finalization")
            Right (term, ty) ->
              typeCheck term `shouldBe` Right ty

      it "keeps already-annotated μ behavior stable" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr = ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, canonicalTy) -> typeCheck term `shouldBe` Right canonicalTy

    describe "Phase 7 reduction of auto-inferred recursive terms (item-1)" $ do
      it "isValue recognizes ERoll wrapping a value as a value" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            let nf = normalize term
            isValue nf `shouldBe` True

      it "step reduces EUnroll (ERoll ty v) to v for auto-inferred recursive terms" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            let steps = iterateStep term
            length steps `shouldSatisfy` (< 1000)
            length steps `shouldSatisfy` (> 0)

      it "normalize produces a value for simple self-recursive elaborated term" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            let nf = normalize term
            isValue nf `shouldBe` True

      it "type preservation: typeCheck(term) == typeCheck(step(term)) for recursive terms" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, ty) -> do
            typeCheck term `shouldBe` Right ty
            -- Check type preservation for each step.
            let checkPreservation t = case step t of
                  Nothing -> pure ()
                  Just t' -> do
                    case typeCheck t' of
                      Right ty' -> ty' `shouldBe` ty
                      Left tcErr ->
                        expectationFailure
                          ( "Type preservation failed after step:\n"
                              ++ "  before: "
                              ++ show t
                              ++ "\n"
                              ++ "  after:  "
                              ++ show t'
                              ++ "\n"
                              ++ "  error:  "
                              ++ show tcErr
                          )
                    checkPreservation t'
            checkPreservation term

      it "application of recursive function reduces through roll/unroll" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            -- normalize produces ERoll ty (ELam ...) — a rolled value
            let nf = normalize term
            case nf of
              Elab.ERoll _muTy _body -> do
                -- Exercise the roll/unroll β-rule: EUnroll (ERoll ty v) → v
                let unrolled = Elab.EUnroll nf
                case step unrolled of
                  Nothing -> expectationFailure "EUnroll (ERoll ty v) should reduce"
                  Just reduced -> do
                    isValue reduced `shouldBe` True
                    -- The reduced term should be the lambda body of the ERoll
                    case reduced of
                      Elab.ELam {} -> pure ()
                      _ -> expectationFailure ("Expected ELam after unroll, got: " ++ show reduced)
              _ -> expectationFailure ("Expected ERoll as normal form, got: " ++ show nf)

      it "step/normalize unchanged for non-recursive programs" $ do
        let nonRecExprs =
              [ ("identity", ELam "x" (EVar "x")),
                ("let-id", ELet "id" (ELam "x" (EVar "x")) (EVar "id")),
                ("app-id-int", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))),
                ("nested-let", ELet "a" (ELit (LInt 1)) (ELet "b" (ELit (LInt 2)) (EVar "a")))
              ]
        forM_ nonRecExprs $ \(label, expr) ->
          case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (term, ty) -> do
              containsMu ty `shouldBe` False
              containsRollTerm term `shouldBe` False
              containsUnrollTerm term `shouldBe` False
              let nf = normalize term
              isValue nf `shouldBe` True
              case typeCheck nf of
                Right nfTy -> nfTy `shouldBe` ty
                Left _ -> pure () -- some normal forms lose let-scheme context
      it "runPipelineElab succeeds for self-recursive definition" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, ty) -> do
            containsMu ty `shouldBe` True
            let nf = normalize term
            isValue nf `shouldBe` True

    describe "ARI-C1 feasibility characterization (bounded prototype-only)" $ do
      let ariSetVarBound nid newBound constraint =
            let tweak node = case node of
                  TyVar {tnId = varId}
                    | varId == nid ->
                        TyVar {tnId = varId, tnBound = Just newBound}
                  _ -> node
             in constraint
                  { cNodes =
                      fromListNode
                        [ (nodeIdKey, tweak node)
                          | (nodeIdKey, node) <- toListNode (cNodes constraint)
                        ]
                  }
          ariSetTypeParent child mbParent constraint =
            let childKey = nodeRefKey (typeRef child)
                bindParents' = case mbParent of
                  Nothing ->
                    IntMap.delete childKey (cBindParents constraint)
                  Just parentRef ->
                    IntMap.insert childKey (parentRef, BindFlex) (cBindParents constraint)
             in constraint {cBindParents = bindParents'}
          rewriteResultTypeInputs rewrite inputs =
            let view0 = rtcPresolutionView inputs
                baseConstraint' = rewrite (pvConstraint view0)
                canonicalConstraint' = rewrite (pvCanonicalConstraint view0)
                view' =
                  view0
                    { pvConstraint = baseConstraint',
                      pvLookupNode =
                        \nid -> NodeAccess.lookupNode baseConstraint' ((pvCanonical view0) nid),
                      pvLookupVarBound =
                        \nid -> NodeAccess.lookupVarBound baseConstraint' ((pvCanonical view0) nid),
                      pvLookupBindParent = NodeAccess.lookupBindParent baseConstraint',
                      pvBindParents = cBindParents baseConstraint',
                      pvCanonicalConstraint = canonicalConstraint'
                    }
                ga0 = rtcBindParentsGa inputs
                ga' =
                  ga0
                    { gaBindParentsBase = cBindParents baseConstraint',
                      gaBaseConstraint = baseConstraint'
                    }
             in inputs
                  { rtcPresolutionView = view',
                    rtcBindParentsGa = ga'
                  }
          ariClearVarBound nid constraint =
            let tweak node = case node of
                  TyVar {tnId = varId}
                    | varId == nid ->
                        TyVar {tnId = varId, tnBound = Nothing}
                  _ -> node
             in constraint
                  { cNodes =
                      fromListNode
                        [ (nodeIdKey, tweak node)
                          | (nodeIdKey, node) <- toListNode (cNodes constraint)
                        ]
                  }
          makeLocalTypeRoot inputs rootNid =
            let rootC = pvCanonical (rtcPresolutionView inputs) rootNid
             in rewriteResultTypeInputs (ariSetTypeParent rootC Nothing) inputs
          rebindRootTo inputs rootNid newBound =
            let view0 = rtcPresolutionView inputs
                rootC = pvCanonical view0 rootNid
                boundC = pvCanonical view0 newBound
             in rewriteResultTypeInputs (ariSetVarBound rootC boundC) inputs
          clearRetainedChildBoundInInputs inputs rootNid childNid =
            let view0 = rtcPresolutionView inputs
                rootC = pvCanonical view0 rootNid
                childC = pvCanonical view0 childNid
             in if childC == rootC
                  then
                    case nextFreshNodeIds 1 view0 of
                      [freshChild] ->
                        rewriteResultTypeInputs
                          ( insertTyNodes [TyVar {tnId = freshChild, tnBound = Nothing}]
                              . ariSetTypeParent freshChild (Just (typeRef rootC))
                          )
                          inputs
                      other ->
                        error
                          ( "expected one fresh unbounded retained-child node, got "
                              ++ show other
                          )
                  else rewriteResultTypeInputs (ariClearVarBound childC) inputs
          duplicateReferencedTrace inputs eids =
            let edgeArtifacts0 = rtcEdgeArtifacts inputs
                matchingArtifact =
                  listToMaybe
                    [ artifact
                      | eid <- eids,
                        Just artifact <- [lookupEdgeArtifact eid edgeArtifacts0]
                    ]
                nextEdgeKey =
                  case IntMap.lookupMax (rtcEdgeTraces inputs) of
                    Just (edgeKey, _) -> edgeKey + 1
                    Nothing -> 0
                nextEdgeId = EdgeId nextEdgeKey
             in case matchingArtifact of
                  Just artifact ->
                    inputs
                      { rtcEdgeArtifacts =
                          insertEdgeArtifactForTest
                            nextEdgeId
                            (edgeArtifactExpansion artifact)
                            ( (edgeArtifactWitness artifact)
                                { ewEdgeId = nextEdgeId
                                }
                            )
                            (edgeArtifactTrace artifact)
                            edgeArtifacts0
                      }
                  Nothing ->
                    error
                      ( "expected edge artifact packet for "
                          ++ show eids
                          ++ " for local multi-inst fallback case"
                      )
          rewriteReferencedTrace rewrite eids inputs =
            let edgeTraces0 = rtcEdgeTraces inputs
                matchingTrace =
                  listToMaybe
                    [ (getEdgeId eid, tr)
                      | eid <- eids,
                        Just tr <- [IntMap.lookup (getEdgeId eid) edgeTraces0]
                    ]
             in case matchingTrace of
                  Just (edgeKey, tr) ->
                    inputs
                      { rtcEdgeArtifacts =
                          setEdgeArtifactTraceForTest
                            (EdgeId edgeKey)
                            (rewrite tr)
                            (rtcEdgeArtifacts inputs)
                      }
                  Nothing ->
                    error
                      ( "expected edge trace for "
                          ++ show eids
                          ++ " for local inst-arg multi-base case"
                      )
          findIntBaseNode view0 =
            case [ tnId node
                   | (_nodeIdKey, node@TyBase {tnBase = BaseTy "Int"}) <-
                       toListNode (cNodes (pvConstraint view0))
                 ] of
              baseNid : _ -> baseNid
              [] -> error "expected Int base node for local fallback case"
          nextFreshNodeIds count view0 =
            let occupiedNodeIds =
                  [ getNodeId nodeIdKey
                    | constraint <- [pvConstraint view0, pvCanonicalConstraint view0]
                    , (nodeIdKey, _node) <- toListNode (cNodes constraint)
                  ]
                    ++ IntMap.keys (pvCanonicalMap view0)
                    ++ map getNodeId (IntMap.elems (pvCanonicalMap view0))
                start =
                  case occupiedNodeIds of
                    [] -> 0
                    nodeIds -> maximum nodeIds + 1
             in fmap NodeId [start .. start + count - 1]
          insertTyNodes newNodes constraint =
            constraint
              { cNodes =
                  fromListNode (toListNode (cNodes constraint) ++ fmap (\node -> (tnId node, node)) newNodes)
              }
          firstRecursiveTarget view0 = go IntSet.empty
            where
              go visited nid
                | IntSet.member (getNodeId nid) visited = Nothing
                | otherwise =
                    let visited' = IntSet.insert (getNodeId nid) visited
                     in case pvLookupNode view0 nid of
                          Just TyMu {} -> Just nid
                          Just TyVar {tnBound = Just boundNid} -> go visited' boundNid
                          Just TyForall {tnBody = bodyNid} -> go visited' bodyNid
                          Just TyExp {tnBody = bodyNid} -> go visited' bodyNid
                          Just TyArrow {tnDom = domNid, tnCod = codNid} ->
                            case go visited' domNid of
                              Just target -> Just target
                              Nothing -> go visited' codNid
                          _ -> Nothing
          wireSameLaneRetainedChild inputs rootNid childNid retainedTarget =
            let view0 = rtcPresolutionView inputs
                rootC = pvCanonical view0 rootNid
                childC = pvCanonical view0 childNid
                targetC = pvCanonical view0 retainedTarget
                (retainedChildC, syntheticNodes) =
                  if childC == rootC
                    then case nextFreshNodeIds 1 view0 of
                      [freshChild] ->
                        ( freshChild,
                          [TyVar {tnId = freshChild, tnBound = Just targetC}]
                        )
                      other ->
                        error
                          ( "expected one fresh retained-child node, got "
                              ++ show other
                          )
                    else (childC, [])
                rewrite =
                  insertTyNodes syntheticNodes
                    . ariSetVarBound rootC targetC
                    . ariSetVarBound retainedChildC targetC
                    . ariSetTypeParent retainedChildC (Just (typeRef rootC))
                    . ariSetTypeParent rootC Nothing
             in (rewriteResultTypeInputs rewrite inputs, retainedChildC)
          schemeAliasBaseLikeFallback keepLocalTypeRoot = do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr =
                  ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")
                extractVarBody ann0 = case ann0 of
                  ALet _ _ _ _ _ _ _ (ALetScope body _ _) _ -> body
                  _ -> error ("unexpected scheme-alias/base-like wrapper shape: " ++ show ann0)
                bodyRoot ann0 = case extractVarBody ann0 of
                  AResolvedVar _ _ nid -> nid
                  other ->
                    error ("expected local scheme alias variable body, got " ++ show other)
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                bodyCanon = extractVarBody annCanon0
                bodyPre = extractVarBody annPre0
                rootNid = rtcCanonical inputs0 (bodyRoot annCanon0)
                inputs1 =
                  if keepLocalTypeRoot
                    then makeLocalTypeRoot inputs0 rootNid
                    else inputs0
                inputs2 =
                  rebindRootTo
                    inputs1
                    rootNid
                    (findIntBaseNode (rtcPresolutionView inputs1))
            requireRight (computeResultTypeFallback inputs2 bodyCanon bodyPre)
          localEmptyCandidateSchemeAliasBaseLikeFallback keepLocalTypeRoot = do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr =
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                extractInnerLetRhs ann0 = case ann0 of
                  ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                  _ ->
                    error
                      ( "unexpected local empty-candidate scheme-alias/base-like wrapper shape: "
                          ++ show ann0
                      )
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                innerCanon = extractInnerLetRhs annCanon0
                innerPre = extractInnerLetRhs annPre0
                (rootNid, childNid) = case innerCanon of
                  AApp _ (AResolvedVar _ _ nid) _ _ appNid -> (appNid, nid)
                  other ->
                    error
                      ( "expected local empty-candidate scheme-alias/base-like app shape, got "
                          ++ show other
                      )
                inputs1 =
                  if keepLocalTypeRoot
                    then makeLocalTypeRoot inputs0 rootNid
                    else inputs0
                inputs2 =
                  rebindRootTo
                    inputs1
                    rootNid
                    (findIntBaseNode (rtcPresolutionView inputs1))
                inputs3 = clearRetainedChildBoundInInputs inputs2 rootNid childNid
            requireRight (computeResultTypeFallback inputs3 innerCanon innerPre)
          localMultiInstFallback keepLocalTypeRoot = do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr =
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                extractInnerLetRhs ann0 = case ann0 of
                  ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                  _ -> error ("unexpected local multi-inst wrapper shape: " ++ show ann0)
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                innerCanon = extractInnerLetRhs annCanon0
                innerPre = extractInnerLetRhs annPre0
                (rootNid, childNid, edgeIds) = case innerCanon of
                  AApp _ (AResolvedVar _ _ nid) funSite argSite appNid ->
                    (appNid, nid, map instantiationSiteEdgeId [funSite, argSite])
                  other ->
                    error ("expected local multi-inst app shape, got " ++ show other)
                inputs1 =
                  if keepLocalTypeRoot
                    then makeLocalTypeRoot inputs0 rootNid
                    else inputs0
                inputs2 = clearRetainedChildBoundInInputs inputs1 rootNid childNid
                inputs3 = duplicateReferencedTrace inputs2 edgeIds
            requireRight (computeResultTypeFallback inputs3 innerCanon innerPre)
          localInstArgMultiBaseFallback keepLocalTypeRoot = do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr =
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                extractInnerLetRhs ann0 = case ann0 of
                  ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                  _ -> error ("unexpected local inst-arg multi-base wrapper shape: " ++ show ann0)
                injectMultiBaseArgs inputs eids =
                  let view0 = rtcPresolutionView inputs
                      (bottomNid, binderA, argA, binderB, argB) =
                        case nextFreshNodeIds 5 view0 of
                          [nid0, nid1, nid2, nid3, nid4] ->
                            (nid0, nid1, nid2, nid3, nid4)
                          other ->
                            error
                              ( "expected five fresh node ids for local inst-arg multi-base case, got "
                                  ++ show other
                              )
                      inputs' =
                        rewriteResultTypeInputs
                          ( insertTyNodes
                              [ TyBottom {tnId = bottomNid},
                                TyVar {tnId = binderA, tnBound = Nothing},
                                TyVar {tnId = argA, tnBound = Just (findIntBaseNode view0)},
                                TyVar {tnId = binderB, tnBound = Nothing},
                                TyVar {tnId = argB, tnBound = Just bottomNid}
                              ]
                          )
                          inputs
                   in rewriteReferencedTrace
                        ( \tr ->
                            tr
                              { etBinderArgs =
                                  etBinderArgs tr ++ [(binderA, argA), (binderB, argB)]
                              }
                        )
                        eids
                        inputs'
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                innerCanon = extractInnerLetRhs annCanon0
                innerPre = extractInnerLetRhs annPre0
                (rootNid, childNid, edgeIds) = case innerCanon of
                  AApp _ (AResolvedVar _ _ nid) funSite argSite appNid ->
                    (appNid, nid, map instantiationSiteEdgeId [funSite, argSite])
                  other ->
                    error ("expected local inst-arg multi-base app shape, got " ++ show other)
                inputs1 =
                  if keepLocalTypeRoot
                    then makeLocalTypeRoot inputs0 rootNid
                    else inputs0
                inputs2 = clearRetainedChildBoundInInputs inputs1 rootNid childNid
                inputs3 = injectMultiBaseArgs inputs2 edgeIds
            requireRight (computeResultTypeFallback inputs3 innerCanon innerPre)
          localInstArgSingleBaseFallback keepLocalTypeRoot = do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr =
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                extractInnerLetRhs ann0 = case ann0 of
                  ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                  _ -> error ("unexpected local inst-arg singleton-base wrapper shape: " ++ show ann0)
                injectSingleBaseWitness inputs eid =
                  let view0 = rtcPresolutionView inputs
                      edgeWitnesses0 = rtcEdgeWitnesses inputs
                      edgeKey = getEdgeId eid
                      seedWitness =
                        case IntMap.lookup edgeKey edgeWitnesses0 of
                          Just ew -> ew
                          Nothing ->
                            error "expected a complete edge packet for local inst-arg singleton-base case"
                      ew' =
                        seedWitness
                          { ewEdgeId = eid,
                            ewRight = findIntBaseNode view0
                          }
                   in inputs
                        { rtcEdgeArtifacts =
                            setEdgeArtifactWitnessForTest
                              eid
                              ew'
                              (rtcEdgeArtifacts inputs)
                        }
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                innerCanon = extractInnerLetRhs annCanon0
                innerPre = extractInnerLetRhs annPre0
                (rootNid, childNid, argEid) = case innerCanon of
                  AApp _ (AResolvedVar _ _ nid) _funEid argEdgeId appNid -> (appNid, nid, argEdgeId)
                  other ->
                    error ("expected local inst-arg singleton-base app shape, got " ++ show other)
                inputs1 =
                  if keepLocalTypeRoot
                    then makeLocalTypeRoot inputs0 rootNid
                    else inputs0
                inputs2 =
                  if keepLocalTypeRoot
                    then rebindRootTo inputs1 rootNid (findIntBaseNode (rtcPresolutionView inputs1))
                    else inputs1
                inputs3 = clearRetainedChildBoundInInputs inputs2 rootNid childNid
                inputs4 = injectSingleBaseWitness inputs3 (instantiationSiteEdgeId argEid)
            requireRight (computeResultTypeFallback inputs4 innerCanon innerPre)
          localSingleBaseFallback keepLocalTypeRoot = do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr =
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                extractInnerLetRhs ann0 = case ann0 of
                  ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                  _ -> error ("unexpected local single-base wrapper shape: " ++ show ann0)
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                innerCanon = extractInnerLetRhs annCanon0
                innerPre = extractInnerLetRhs annPre0
                (rootNid, childNid) = case innerCanon of
                  AApp _ (AResolvedVar _ _ nid) _ _ appNid -> (appNid, nid)
                  other ->
                    error ("expected local single-base app shape, got " ++ show other)
                inputs1 =
                  if keepLocalTypeRoot
                    then makeLocalTypeRoot inputs0 rootNid
                    else inputs0
                inputs2 =
                  let view1 = rtcPresolutionView inputs1
                      childC = pvCanonical view1 childNid
                   in rewriteResultTypeInputs
                        ( ariSetVarBound childC (findIntBaseNode view1)
                            . ariSetTypeParent childC Nothing
                        )
                        inputs1
            requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)

      it "keeps annotation-anchored recursive shape processable" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr = ELamAnn "x" recursiveAnn (EVar "x")
            recursiveTy =
              testTMu
                "a"
                (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              case ty of
                TForallRef resultRef (Just resultBound) (TArrow paramTy (TVarRef resultUseRef)) -> do
                  unless (alphaEqType paramTy recursiveTy) $
                    expectationFailure
                      ("expected annotated parameter " ++ show recursiveTy ++ ", got " ++ show paramTy)
                  unless (alphaEqType (tyToElab resultBound) recursiveTy) $
                    expectationFailure
                      ("expected flexible result bound " ++ show recursiveTy ++ ", got " ++ show resultBound)
                  resultUseRef `shouldSatisfy` typeBinderRefsSameIdentity resultRef
                other ->
                  expectationFailure
                    ("expected forall (result > R). R -> result, got " ++ show other)

      it "keeps local-binding recursive retention processable through a direct wrapper" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "y" (EVar "y"))
                (ELamAnn "x" recursiveAnn (EVar "x"))
            expectedTy =
              TArrow
                (testTMu "a" (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int"))))
                (testTMu "a" (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int"))))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              ty `shouldSatisfy` (`matchesRecursiveArrow` expectedTy)

      it "keeps retained-child fallback recursive through a same-lane local TypeRef root" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
            wireSameLaneLocalRoot inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  childC = pvCanonical view0 childNid
                  retainedTarget =
                    case firstRecursiveTarget view0 childC of
                      Just targetNid -> targetNid
                      Nothing ->
                        error
                          ( "expected retained child recursive target for "
                              ++ show childC
                          )
               in wireSameLaneRetainedChild inputs rootNid childNid retainedTarget
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            innerCanon = extractInnerLetRhs annCanon0
            innerPre = extractInnerLetRhs annPre0
            (retainedRoot, retainedChild) = case innerCanon of
              AApp _ (AResolvedVar _ _ nid) _ _ rootNid -> (rootNid, nid)
              _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
            (inputs, _retainedChildC) = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
        fallbackTy <- requireRight (computeResultTypeFallback inputs innerCanon innerPre)
        containsMu fallbackTy `shouldBe` True

      it "deduplicates equivalent same-lane proofs and preserves their recursive target" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
            wireSameLaneLocalRoot inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  childC = pvCanonical view0 childNid
                  retainedTarget =
                    case firstRecursiveTarget view0 childC of
                      Just targetNid -> targetNid
                      Nothing ->
                        error
                          ( "expected retained child recursive target for "
                              ++ show childC
                          )
               in wireSameLaneRetainedChild inputs rootNid childNid retainedTarget
            duplicateRetainedChildCandidate inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  rootC = pvCanonical view0 rootNid
                  childC = pvCanonical view0 childNid
                  retainedTarget =
                    case pvLookupVarBound view0 childC of
                      Just boundNid -> boundNid
                      Nothing ->
                        error
                          ( "expected retained child bound for "
                              ++ show childC
                          )
                  peerNid =
                    case nextFreshNodeIds 1 view0 of
                      [freshNid] -> freshNid
                      other ->
                        error
                          ( "expected one fresh node id for retained-child ambiguity case, got "
                              ++ show other
                          )
                  rewrite =
                    insertTyNodes [TyVar {tnId = peerNid, tnBound = Just retainedTarget}]
                      . ariSetTypeParent peerNid (Just (typeRef rootC))
               in rewriteResultTypeInputs rewrite inputs
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            innerCanon = extractInnerLetRhs annCanon0
            innerPre = extractInnerLetRhs annPre0
            (retainedRoot, retainedChild) = case innerCanon of
              AApp _ (AResolvedVar _ _ nid) _ _ rootNid -> (rootNid, nid)
              _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
            (inputs1, retainedChildC) = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
            inputs2 = duplicateRetainedChildCandidate inputs1 retainedRoot retainedChildC
        fallbackTy <- requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)
        containsMu fallbackTy `shouldBe` True

      it "keeps multiple recursive descendants at their enclosing target" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
            wireSameLaneLocalRoot inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  childC = pvCanonical view0 childNid
                  retainedTarget =
                    case firstRecursiveTarget view0 childC of
                      Just targetNid -> targetNid
                      Nothing ->
                        error
                          ( "expected retained child recursive target for "
                              ++ show childC
                          )
               in wireSameLaneRetainedChild inputs rootNid childNid retainedTarget
            injectAmbiguousRetainedChildTarget inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  rootC = pvCanonical view0 rootNid
                  childC = pvCanonical view0 childNid
                  retainedTarget =
                    case pvLookupVarBound view0 childC of
                      Just boundNid -> boundNid
                      Nothing ->
                        error
                          ( "expected retained child bound for "
                              ++ show childC
                          )
                  (ambiguousTargetNid, alternateMuNid) =
                    case nextFreshNodeIds 2 view0 of
                      [freshTargetNid, freshMuNid] -> (freshTargetNid, freshMuNid)
                      other ->
                        error
                          ( "expected two fresh node ids for retained-child intra-target ambiguity case, got "
                              ++ show other
                          )
                  rewrite =
                    ariSetVarBound rootC ambiguousTargetNid
                      . ariSetVarBound childC ambiguousTargetNid
                      . insertTyNodes
                        [ TyArrow {tnId = ambiguousTargetNid, tnDom = retainedTarget, tnCod = alternateMuNid},
                          TyMu {tnId = alternateMuNid, tnBody = retainedTarget}
                        ]
               in rewriteResultTypeInputs rewrite inputs
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            innerCanon = extractInnerLetRhs annCanon0
            innerPre = extractInnerLetRhs annPre0
            (retainedRoot, retainedChild) = case innerCanon of
              AApp _ (AResolvedVar _ _ nid) _ _ rootNid -> (rootNid, nid)
              _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
            (inputs1, retainedChildC) = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
            inputs2 = injectAmbiguousRetainedChildTarget inputs1 retainedRoot retainedChildC
        fallbackTy <- requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)
        case stripLeadingUnboundedForalls fallbackTy of
          TArrow dom cod -> do
            containsMu dom `shouldBe` True
            containsMu cod `shouldBe` True
          other ->
            expectationFailure
              ( "expected fail-closed enclosing recursive arrow, got "
                  ++ show other
              )

      it "keeps mixed retained-child/base-target competition at the recursive enclosing target" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
            wireSameLaneLocalRoot inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  childC = pvCanonical view0 childNid
                  retainedTarget =
                    case firstRecursiveTarget view0 childC of
                      Just targetNid -> targetNid
                      Nothing ->
                        error
                          ( "expected retained child recursive target for "
                              ++ show childC
                          )
               in wireSameLaneRetainedChild inputs rootNid childNid retainedTarget
            injectSingleBaseWitness inputs eid =
              let view0 = rtcPresolutionView inputs
                  edgeWitnesses0 = rtcEdgeWitnesses inputs
                  edgeKey = getEdgeId eid
                  seedWitness =
                    case IntMap.lookup edgeKey edgeWitnesses0 of
                      Just ew -> ew
                      Nothing ->
                        error "expected a complete edge packet for mixed retained-child/base-target case"
                  ew' =
                    seedWitness
                      { ewEdgeId = eid,
                        ewRight = findIntBaseNode view0
                      }
               in inputs
                    { rtcEdgeArtifacts =
                        setEdgeArtifactWitnessForTest
                          eid
                          ew'
                          (rtcEdgeArtifacts inputs)
                    }
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            innerCanon = extractInnerLetRhs annCanon0
            innerPre = extractInnerLetRhs annPre0
            (retainedRoot, retainedChild, argEid) = case innerCanon of
              AApp _ (AResolvedVar _ _ nid) _funEid argEdgeId appNid -> (appNid, nid, argEdgeId)
              other ->
                error
                  ( "expected retained-child app shape for mixed retained-child/base-target case, got "
                      ++ show other
                  )
            (inputs1, _retainedChildC) = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
            inputs2 = injectSingleBaseWitness inputs1 (instantiationSiteEdgeId argEid)
        fallbackTy <- requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)
        containsMu fallbackTy `shouldBe` True

      it "sameLaneClearBoundaryExpr clears Phase 6 with recursive authoritative output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            sameLaneClearBoundaryExpr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            collapsedTy = testTForall "a" Nothing (testTVar "a")
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr))
        when (ty == collapsedTy) $
          expectationFailure
            ( "canonical term collapsed: "
                ++ show term
                ++ " :: "
                ++ show ty
            )
        containsMu ty `shouldBe` True

      it "sameLaneAliasFrameClearBoundaryExpr preserves predecessor alias-frame truth on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u"))
                )
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneDoubleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneTripleAliasFrameClearBoundaryExpr is the next milestone-3 representative broader-positive clear-boundary packet after the merged double-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneQuadrupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged triple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneQuintupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged quadruple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneSextupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged quintuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneSeptupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged sextuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneOctupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged septuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneNonupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged octuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneDecupleAliasFrameClearBoundaryExpr is the next broader-positive owner-sensitive clear-boundary packet on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
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
                                                    ( ELet
                                                        "grain"
                                                        (EVar "seed")
                                                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "grain")) (EVar "u"))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        (term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck term `shouldBe` Right ty
        countLeadingUnboundedForalls ty `shouldBe` 0
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls ty)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary" $ do
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
            extractSelectedPacket ann0 = case ann0 of
              ALet _ _ _ _ _ _ _ (ALetScope (ALet _ _ _ _ _ _ rhs body _) _ _) _ ->
                let bodyApp = case body of
                      ALetScope inner _ _ -> inner
                      other -> other
                    annotationRoots expr0 = case expr0 of
                      AResolvedVar {} -> []
                      ALit {} -> []
                      ALam _ _ _ _ inner _ _ -> annotationRoots inner
                      AApp fun arg _ _ _ -> annotationRoots fun ++ annotationRoots arg
                      ALet _ _ _ _ _ _ innerRhs innerBody _ ->
                        annotationRoots innerRhs ++ annotationRoots innerBody
                      AAnn inner annNode _ -> annNode : annotationRoots inner
                      ALetScope inner _ _ -> annotationRoots inner
                      AUnfold inner _ _ -> annotationRoots inner
                    annotationRoot = case annotationRoots rhs of
                      [annNode] -> annNode
                      other ->
                        error
                          ( "expected one recursive annotation in nested-forall RHS, got "
                              ++ show other
                          )
                 in (bodyApp, annotationRoot)
              _ ->
                error
                  ( "unexpected same-wrapper nested-forall wrapper shape: "
                      ++ show ann0
                  )
            wireSameLaneLocalRoot inputs rootNid childNid annotationRoot =
              let view0 = rtcPresolutionView inputs
                  recursiveTarget =
                    case firstRecursiveTarget view0 annotationRoot of
                      Just targetNid -> targetNid
                      Nothing ->
                        error
                          ( "expected recursive annotation target below "
                              ++ show annotationRoot
                          )
                  nestedTarget =
                    case nextFreshNodeIds 1 view0 of
                      [freshNid] -> freshNid
                      other ->
                        error
                          ( "expected one fresh nested-forall target, got "
                              ++ show other
                          )
                  inputsWithNestedTarget =
                    rewriteResultTypeInputs
                      (insertTyNodes [TyForall {tnId = nestedTarget, tnBody = recursiveTarget}])
                      inputs
                  (inputs', _retainedChildC) =
                    wireSameLaneRetainedChild
                      inputsWithNestedTarget
                      rootNid
                      childNid
                      nestedTarget
               in inputs'
                    { rtcBaseConstraint =
                        pvConstraint (rtcPresolutionView inputs')
                    }
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            (bodyCanon, annotationRoot) = extractSelectedPacket annCanon0
            (bodyPre, _annotationRootPre) = extractSelectedPacket annPre0
            (retainedRoot, retainedChild) = case bodyCanon of
              AApp _ (AResolvedVar _ _ nid) _ _ rootNid -> (rootNid, nid)
              other ->
                error
                  ( "expected same-wrapper nested-forall retained-child app shape, got "
                      ++ show other
                  )
            inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild annotationRoot
        fallbackTy <- requireRight (computeResultTypeFallback inputs bodyCanon bodyPre)
        containsMu fallbackTy `shouldBe` True

      it "same-wrapper nested-forall packet preserves recursive output on the canonical pipeline entrypoint" $ do
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
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus owner-local alias frame preserves recursive output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    ( ELet
                        "hold"
                        (EVar "k")
                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u"))
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus decuple owner-local alias frames preserves recursive output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    ( aliasChain
                        ["hold", "keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain"]
                        "k"
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus let-aliased transparent eta mediator stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z")))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      let sameWrapperNestedForallStackedTransparentExpr wrap1 wrap2 recursiveAnn =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "k"
                          (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k")))
                              (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                          )
                      )
                  )
              )
          sameWrapperNestedForallAliasChain aliases source =
            case aliases of
              [] ->
                ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
              aliasName : rest ->
                ELet aliasName (EVar source) (sameWrapperNestedForallAliasChain rest aliasName)
          sameWrapperNestedForallStackedTransparentAliasChainExpr wrap1 wrap2 recursiveAnn =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "k"
                          (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k")))
                              ( sameWrapperNestedForallAliasChain
                                  ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                  "hold"
                              )
                          )
                      )
                  )
              )

      it "same-wrapper nested-forall plus stacked transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentAliasChainExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentAliasChainExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "sibling transparent eta mediators do not poison direct recursive wrapper application" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            directExpr wrapperName =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "wrap2"
                        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar wrapperName) (EVar "k"))
                                (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                            )
                        )
                    )
                )
        ty1 <- expectCanonicalPipelineSuccessType (directExpr "wrap1")
        containsMu ty1 `shouldBe` True
        ty2 <- expectCanonicalPipelineSuccessType (directExpr "wrap2")
        containsMu ty2 `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive wrapper application" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            directExpr wrapperName =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar wrapperName) (EVar "k"))
                                (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                            )
                        )
                    )
                )
        ty1 <- expectCanonicalPipelineSuccessType (directExpr "wrap1")
        containsMu ty1 `shouldBe` True
        ty2 <- expectCanonicalPipelineSuccessType (directExpr "wrap2")
        containsMu ty2 `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive Bool wrapper application" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EVar "k"))
                                (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive wrapper application through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EVar "k"))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive Bool wrapper application through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EVar "k"))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus let-aliased transparent eta mediator stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z")))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "keeps local empty-candidate scheme-alias/base-like fallback on the local TypeRef lane" $ do
        fallbackTy <- localEmptyCandidateSchemeAliasBaseLikeFallback True
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")

      it "returns the selected local scheme-alias/base-like target without a vacuous forall" $ do
        fallbackTy <- schemeAliasBaseLikeFallback True
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")

      it "keeps local single-base fallback on the local TypeRef lane" $ do
        fallbackTy <- localSingleBaseFallback True
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")

      it "keeps the same single-base wrapper on a unique non-local baseTarget lane" $ do
        fallbackTy <- localSingleBaseFallback False
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")
        containsMu fallbackTy `shouldBe` False

      it "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane" $ do
        fallbackTy <- schemeAliasBaseLikeFallback False
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")
        containsMu fallbackTy `shouldBe` False

      it "keeps the selected non-local scheme-alias/base-like packet recursive on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")
            blockedTy = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
        (_term, ty) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        ty `shouldNotBe` blockedTy
        containsMu ty `shouldBe` True

      it "keeps local multi-inst ambiguity at the recursive enclosing target" $ do
        fallbackTy <- localMultiInstFallback True
        unless
          (matchesRecursiveArrow fallbackTy expectedSameLaneAliasFrameClearBoundaryArrow)
          (expectationFailure ("expected recursive enclosing target, got " ++ show fallbackTy))

      it "keeps non-local multi-inst ambiguity open to the recursive enclosing target" $ do
        fallbackTy <- localMultiInstFallback False
        unless
          (matchesRecursiveArrow fallbackTy expectedSameLaneAliasFrameClearBoundaryArrow)
          (expectationFailure ("expected recursive enclosing target, got " ++ show fallbackTy))

      it "keeps local inst-arg multi-base ambiguity at the recursive enclosing target" $ do
        fallbackTy <- localInstArgMultiBaseFallback True
        unless
          (matchesRecursiveArrow fallbackTy expectedSameLaneAliasFrameClearBoundaryArrow)
          (expectationFailure ("expected recursive enclosing target, got " ++ show fallbackTy))

      it "keeps non-local inst-arg multi-base ambiguity open to the recursive enclosing target" $ do
        fallbackTy <- localInstArgMultiBaseFallback False
        unless
          (matchesRecursiveArrow fallbackTy expectedSameLaneAliasFrameClearBoundaryArrow)
          (expectationFailure ("expected recursive enclosing target, got " ++ show fallbackTy))

      it "keeps local inst-arg-only singleton-base fallback on the local TypeRef lane" $ do
        fallbackTy <- localInstArgSingleBaseFallback True
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")

      it "keeps the same inst-arg-only singleton-base wrapper on a unique non-local baseTarget lane" $ do
        fallbackTy <- localInstArgSingleBaseFallback False
        fallbackTy `shouldBe` TestElab.tBase (BaseTy "Int")
        containsMu fallbackTy `shouldBe` False

      it "does not infer recursive shape for the corresponding unannotated variant" $ do
        let expr = ELam "x" (EVar "x")
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
        fallbackTy <- requireRight (computeResultTypeFallback inputs annCanon annPre)
        containsMu fallbackTy `shouldBe` False
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              containsMu ty `shouldBe` False

      -- This is not thesis §15.3.8's polymorphic annotated omega.  Here the
      -- proxy itself has type R -> R for R = mu a. a -> Int.  Applying it to
      -- itself would identify R with a structure that contains R, so the
      -- acyclic presolution graph must reject it at the occurs-check boundary.
      it "rejects self-application of a non-local recursive-type proxy during presolution" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EApp (EVar "g") (EVar "g"))
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left (Elab.PipelinePresolutionError (ExecError OccursCheckPresolution{})) ->
              pure ()
            Left err ->
              expectationFailure
                ("Expected a presolution occurs-check for " ++ label ++ ", but got " ++ show err)
            Right (term, ty) ->
              expectationFailure
                ("Expected a presolution occurs-check for " ++ label ++ ", but got type " ++ show ty ++ " from " ++ show term)

      it "non-local proxy wrapper let g = (λx:μα.α→Int. x) in g succeeds with correct arrow type" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EVar "g")
            muTy = testTMu "t6" (TArrow (testTVar "t6") (TestElab.tBase (BaseTy "Int")))
            expectedTy = TArrow muTy muTy
        let label = "canonical"
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Right (_term, ty) ->
              unless (matchesRecursiveArrow ty expectedTy) $
                expectationFailure
                  (label ++ " non-local proxy: expected arrow of μ-types, got " ++ show ty)
            Left err ->
              expectationFailure
                (label ++ " non-local proxy: expected success, got error " ++ show err)

    it "uses presolution-native solved artifacts" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      cUnifyEdges (prConstraint (paPresolution artifacts)) `shouldBe` []
      let pres = paPresolution artifacts
      expectedNative <-
        requireRight
          (SolvedTest.solvedFromSnapshot (snapshotUnionFind pres) (snapshotConstraint pres))
      paSolved artifacts `shouldBe` expectedNative
      runPipelineElab Set.empty (unsafeNormalizeExpr (ELam "x" (EVar "x")))
        `shouldSatisfy` isRight

    it "solves let-bound id applied to Bool" $ do
      let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LBool True)))
      case runToSolvedDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right solved -> do
          validateStrict solved
          let nodes = cNodes (Solved.originalConstraint solved)
          baseNames nodes `shouldContain` [BaseTy "Bool"]
          noExpNodes nodes

    it "instantiates let-polymorphic id at Int and Bool" $ do
      -- let id = \x. x in let a = id 1 in id True
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      case runToSolvedDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right res -> do
          validateStrict res
          let nodes = cNodes (Solved.originalConstraint res)
          baseNames nodes `shouldContain` [BaseTy "Int"]
          noExpNodes nodes

    it "prepared generalization artifact drives redirecting instantiation behavior" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      ConstraintResult
        { crConstraint = c0,
          crAnnotated = ann,
          crIdentityGenerator = packetIdentityGenerator,
          crExactProducerTypes = exactProducerTypes,
          crSourceTypeBinderIdentities = sourceTypeBinderIdentities
        } <-
        requireRight (runConstraintDefault defaultPolySyms expr)
      let cNorm = CNormalize.normalize c0
      (cAcyclic, acyc) <-
        requireRight (firstShowE (Acyc.breakCyclesAndCheckAcyclicity cNorm))
      pres <-
        requireRight (firstShowE (computePresolution defaultTraceConfig acyc cAcyclic))
      artifact <-
        requireRight
          ( firstShowE
              ( prepareGeneralizationArtifact
                  defaultTraceConfig
                  packetIdentityGenerator
                  exactProducerTypes
                  IntMap.empty
                  sourceTypeBinderIdentities
                  cAcyclic
                  pres
                  ann
              )
          )
      let redirects = prRedirects pres
          artifactView = preparedGeneralizationArtifactTestView artifact
          redirectedAnn = applyRedirectsToAnn redirects ann
          canonicalizedAnn = canonicalizeAnn (preparedTestCanonicalizeNode artifactView) redirectedAnn
          baseConstraint = preparedTestBaseConstraint artifactView
          solvedToBase = preparedTestSolvedToBase artifactView
          baseNamedKeysAll = collectBaseNamedKeys baseConstraint
          baseCopyPairs =
            [ (baseKey, copyN)
              | tr <- IntMap.elems (prEdgeTraces pres),
                (baseKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr)),
                IntSet.member baseKey baseNamedKeysAll
            ]
      redirects `shouldSatisfy` (not . IntMap.null)
      preparedTestRedirects artifactView `shouldBe` redirects
      preparedAnnotated artifact `shouldBe` canonicalizedAnn
      authorizedRoot <-
        requireRight (authorizePreparedAnn artifact ann)
      authorizedElaborationResultAnn authorizedRoot
        `shouldBe` canonicalizedAnn
      authorizePreparedAnn artifact (ALit (LInt 0) (NodeId 999999))
        `shouldSatisfy` isLeft
      annNodeOccurrences (preparedAnnotated artifact)
        `shouldBe` map (preparedTestCanonicalizeNode artifactView) (annNodeOccurrences redirectedAnn)
      baseNamedKeysAll `shouldSatisfy` (not . IntSet.null)
      baseCopyPairs `shouldSatisfy` (not . null)
      forM_ baseCopyPairs $ \(_baseKey, copyN) -> do
        let copyKey = getNodeId (preparedTestCanonicalizeNode artifactView copyN)
        case IntMap.lookup copyKey solvedToBase of
          Nothing ->
            expectationFailure ("Prepared artifact missed copy provenance for " ++ show copyN)
          Just actualBase ->
            lookupNodeIn (cNodes baseConstraint) actualBase `shouldSatisfy` isJust
      case generalizePreparedRoot artifact (preparedAnnotated artifact) ann of
        Right (scheme, _subst) ->
          pretty (schemeBody scheme) `shouldSatisfy` ("Bool" `isInfixOf`)
        Left err ->
          expectationFailure ("Prepared artifact generalize-at failed: " ++ show err)
      resultTy <-
        requireRight
          (firstShowE (computePreparedResultType artifact (preparedAnnotated artifact) ann))
      pretty resultTy `shouldSatisfy` ("Bool" `isInfixOf`)

    it "keeps colliding canonical annotation types keyed by their source edges" $ do
      let intAnnotationNode = NodeId 9601
          boolAnnotationNode = NodeId 9602
          exactAnnotationNode = NodeId 9605
          intEdge = EdgeId 9701
          boolEdge = EdgeId 9702
          exactEdge = EdgeId 9703
          intTy = TestElab.tBase (BaseTy "Int")
          boolTy = TestElab.tBase (BaseTy "Bool")
          exactTy = TBottom
          intAnn =
            AAnn
              (ALit (LInt 1) (NodeId 9603))
              intAnnotationNode
              intEdge
          boolAnn =
            AAnn
              (ALit (LBool True) (NodeId 9604))
              boolAnnotationNode
              boolEdge
          exactAnn =
            AExactAnn
              (ALit (LBool False) (NodeId 9606))
              RSTBottom
              exactAnnotationNode
              exactEdge
          expectedTypesByNode =
            IntMap.fromList
              [ (getNodeId intAnnotationNode, intTy),
                (getNodeId boolAnnotationNode, boolTy),
                (getNodeId exactAnnotationNode, exactTy)
              ]
          canonical _ = NodeId 9800
          canonicalAnnotations =
            map (canonicalizeAnn canonical) [intAnn, boolAnn, exactAnn]
      case canonicalAnnotations of
        [AAnn _ firstNode _, AAnn _ secondNode _, AExactAnn _ _ thirdNode _] -> do
          firstNode `shouldBe` secondNode
          secondNode `shouldBe` thirdNode
        other ->
          expectationFailure
            ("expected two canonical source annotations, got " ++ show other)
      forward <-
        requireRight
          ( prepareAnnotationExpectedTypesByEdgeForTest
              expectedTypesByNode
              [intAnn, boolAnn, exactAnn]
          )
      reversed <-
        requireRight
          ( prepareAnnotationExpectedTypesByEdgeForTest
              expectedTypesByNode
              [exactAnn, boolAnn, intAnn]
          )
      forward `shouldBe` reversed
      fst forward
        `shouldBe` IntMap.fromList
          [ (getEdgeId intEdge, intTy),
            (getEdgeId boolEdge, boolTy),
            (getEdgeId exactEdge, exactTy)
          ]
      snd forward
        `shouldBe` IntSet.fromList
          [ getNodeId intAnnotationNode,
            getNodeId boolAnnotationNode,
            getNodeId exactAnnotationNode
          ]

    it "allocates annotation-local forall identities from the preparation supply" $ do
      let occupiedIdentity = UniqueIdentity 42
          expectedBinderIdentity =
            typeBinderIdentityFromUnique (UniqueIdentity 43)
      sourceType <-
        requireRight
          ( normalizeType
              ( mkForalls
                  [("a", Nothing)]
                  (STArrow (STVar "a") (STVar "a"))
              )
          )
      (expectedType, advancedGenerator) <-
        requireRight
          ( sourceTypeToElabTypeWithIdentitiesFromSupply
              (identityGeneratorAfter [occupiedIdentity])
              Map.empty
              Map.empty
              sourceType
          )
      case expectedType of
        TForallRef binderRef Nothing (TArrow (TVarRef domainRef) (TVarRef codomainRef)) -> do
          typeBinderRefIdentity binderRef `shouldBe` expectedBinderIdentity
          typeBinderRefIdentity domainRef `shouldBe` expectedBinderIdentity
          typeBinderRefIdentity codomainRef `shouldBe` expectedBinderIdentity
        other ->
          expectationFailure
            ("expected a source-local forall identity arrow, got " ++ show other)
      advancedGenerator
        `shouldBe` identityGeneratorAfter [UniqueIdentity 43]

    it "quotients a source-local forall only through its exact inferred occurrence" $ do
      let sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 44))
              "source-a"
          inferredRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 45))
              "inferred-a"
          sourceType =
            TForallRef
              sourceRef
              Nothing
              (TArrow (TVarRef sourceRef) (TVarRef sourceRef))
          inferredType =
            TForallRef
              inferredRef
              Nothing
              (TArrow (TVarRef inferredRef) (TVarRef inferredRef))
          sourceSchemeInfo =
            Elab.schemeInfoFromRefSubst
              (Elab.schemeFromType sourceType)
              IntMap.empty
          inferredSchemeInfo =
            Elab.schemeInfoFromRefSubst
              (Elab.schemeFromType inferredType)
              (IntMap.singleton 460 inferredRef)
      (alignedSchemeInfo, constructionRenames) <-
        requireRight
          ( alignSourceExpectedOperatedTypeForTest
              (IntMap.singleton 460 sourceRef)
              sourceSchemeInfo
              inferredSchemeInfo
              sourceType
          )
      Elab.schemeToType (Elab.siScheme alignedSchemeInfo)
        `shouldBe` inferredType
      constructionRenames `shouldBe` [(sourceRef, inferredRef)]
      (unalignedSchemeInfo, unprovedRenames) <-
        requireRight
          ( alignSourceExpectedOperatedTypeForTest
              IntMap.empty
              sourceSchemeInfo
              inferredSchemeInfo
              sourceType
          )
      Elab.schemeToType (Elab.siScheme unalignedSchemeInfo)
        `shouldBe` sourceType
      unprovedRenames `shouldBe` []

    it "constructs an annotated self-application root scheme with its source parameter domain" $ do
      let sigmaId = mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
          expr = ELamAnn "g" sigmaId (EApp (EVar "g") (EVar "g"))
      ConstraintResult
        { crConstraint = c0,
          crAnnotated = ann,
          crIdentityGenerator = packetIdentityGenerator,
          crAnnSourceTypes = annSourceTypes,
          crExactProducerTypes = exactProducerTypes,
          crSourceTypeBinderIdentities = sourceTypeBinderIdentities
        } <-
        requireRight (runConstraintDefault defaultPolySyms expr)
      let cNorm = CNormalize.normalize c0
      (cAcyclic, acyc) <-
        requireRight (firstShowE (Acyc.breakCyclesAndCheckAcyclicity cNorm))
      pres <-
        requireRight (firstShowE (computePresolution defaultTraceConfig acyc cAcyclic))
      (annExpectedTypes, preparationIdentityGenerator) <-
        requireRight
          ( foldM
              ( \(expectedTypes, generator) (nodeKey, sourceType) -> do
                  (expectedType, generator') <-
                    sourceTypeToElabTypeWithIdentitiesFromSupply
                      generator
                      Map.empty
                      Map.empty
                      sourceType
                  pure
                    ( IntMap.insert nodeKey expectedType expectedTypes,
                      generator'
                    )
              )
              (IntMap.empty, packetIdentityGenerator)
              (IntMap.toAscList annSourceTypes)
          )
      artifact <-
        requireRight
          ( firstShowE
              ( prepareGeneralizationArtifact
                  defaultTraceConfig
                  preparationIdentityGenerator
                  exactProducerTypes
                  annExpectedTypes
                  sourceTypeBinderIdentities
                  cAcyclic
                  pres
                  ann
              )
          )
      let artifactView = preparedGeneralizationArtifactTestView artifact
          constructionParents =
            preparedTestExpansionConstructionParents artifactView
          semanticMetaKeys =
            IntMap.keys
              (preparedTestExpansionSemanticMetaParents artifactView)
          solvedToBase = preparedTestSolvedToBase artifactView
          collapsedSemanticPlacements =
            [ ()
              | childKey <- semanticMetaKeys,
                Just (TypeRef parent, _) <-
                  [IntMap.lookup childKey constructionParents],
                Just childSource <- [IntMap.lookup childKey solvedToBase],
                Just parentSource <-
                  [IntMap.lookup (getNodeId parent) solvedToBase],
                childSource == parentSource
            ]
      collapsedSemanticPlacements `shouldSatisfy` (not . null)
      (scheme, subst) <-
        requireRight
          ( firstShowE
              (generalizePreparedRoot artifact (preparedAnnotated artifact) ann)
          )
      case Elab.schemeToType scheme of
        TForallRef resultRef (Just resultBound) (TArrow paramTy (TVarRef resultUseRef)) -> do
          paramTy `shouldSatisfy` containsForallTy
          unless (alphaEqType paramTy (tyToElab resultBound)) $
            expectationFailure
              ( "parameter/bound mismatch: parameter="
                  ++ show paramTy
                  ++ "; bound="
                  ++ show (tyToElab resultBound)
                  ++ "; scheme="
                  ++ show scheme
                  ++ "; packets="
                  ++ show
                    ( preparedTestSubtermGeneralizations
                        (preparedGeneralizationArtifactTestView artifact)
                    )
              )
          resultUseRef `shouldSatisfy` typeBinderRefsSameIdentity resultRef
          IntMap.elems subst
            `shouldSatisfy` any (typeBinderRefsSameIdentity resultRef)
        other ->
          expectationFailure
            ( "expected forall (result > sigmaId). sigmaId -> result, saw "
                ++ show other
                ++ "; packets="
                ++ show
                  ( preparedTestSubtermGeneralizations
                      (preparedGeneralizationArtifactTestView artifact)
                  )
                ++ "; witnesses="
                ++ show (prEdgeWitnesses pres)
            )

    it "tracks instantiation copy maps for named binders" $ do
      -- Non-trivial instantiation: polymorphic id used at two types
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paConstraintNorm = c1, paPresolution = pres, paSolved = solved} ->
          case Solved.validateCanonicalGraphStrict solved of
            [] -> do
              let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
                  adoptNode = canonicalizeNode canon
                  baseNamedKeysAll = collectBaseNamedKeys c1
                  traceMaps =
                    map
                      (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                      (IntMap.elems (prEdgeTraces pres))
                  instCopyMapFull = foldl' IntMap.union IntMap.empty traceMaps
                  baseCopyDestinationKeys =
                    IntSet.fromList
                      [ getNodeId (adoptNode copyN)
                        | tr <- IntMap.elems (prEdgeTraces pres)
                        , (baseKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                        , IntSet.member baseKey baseNamedKeysAll
                      ]
                  traceSourcesByDestination =
                    IntMap.fromListWith
                      IntSet.union
                      [ (copyKey, IntSet.singleton (getNodeId source))
                        | traceMap <- traceMaps
                        , (copyKey, source) <- IntMap.toList traceMap
                      ]
              baseNamedKeysAll `shouldSatisfy` (not . IntSet.null)
              baseCopyDestinationKeys `shouldSatisfy` (not . IntSet.null)
              -- Definition 10.1.1 gives each expansion its own source-to-copy
              -- relation.  If solving coalesces copies from distinct
              -- expansions, the final representative may have several valid
              -- source identities. Every named-binder copy destination must
              -- survive aggregation, and the chosen source must come from one
              -- of the exact edge-local trace projections (including their
              -- replay/root precedence), rather than an assumed numeric
              -- inverse of the final quotient.
              mapM_
                (\copyKey -> IntMap.member copyKey instCopyMapFull `shouldBe` True)
                (IntSet.toList baseCopyDestinationKeys)
              mapM_
                ( \(copyKey, mapped) ->
                    case IntMap.lookup copyKey traceSourcesByDestination of
                      Nothing ->
                        expectationFailure
                          ("Missing edge-local provenance for destination " ++ show copyKey)
                      Just sourceKeys ->
                        IntSet.member (getNodeId mapped) sourceKeys `shouldBe` True
                )
                (IntMap.toList instCopyMapFull)
            vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

    it "projects strict replay binders back onto their source binders for generalization" $ do
      let expr =
            ELet
              "f"
              (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
              (EVar "f")
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paConstraintNorm = c1, paPresolution = pres, paSolved = solved} ->
          case Solved.validateCanonicalGraphStrict solved of
            [] -> do
              let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
                  adoptNode = canonicalizeNode canon
                  baseNamedKeysAll = collectBaseNamedKeys c1
                  tr = (prEdgeTraces pres) IntMap.! 0
                  traceMap = buildTraceCopyMap c1 baseNamedKeysAll adoptNode tr
              etReplayContract tr `shouldBe` ReplayContractStrict
              etReplayDomainBinders tr `shouldSatisfy` (not . null)
              etBinderArgs tr `shouldSatisfy` (not . null)
              let replayMap = etBinderReplayMap tr
                  sourceKeys =
                    IntSet.fromList
                      [ getNodeId sourceBinder
                      | (sourceBinder, _arg) <- etBinderArgs tr
                      ]
              IntSet.fromList (IntMap.keys replayMap) `shouldBe` sourceKeys
              mapM_
                ( \(sourceBinder, _arg) ->
                    case IntMap.lookup (getNodeId sourceBinder) replayMap of
                      Nothing ->
                        expectationFailure
                          ("Missing strict replay target for source binder " ++ show sourceBinder)
                      Just replayBinder ->
                        case IntMap.lookup (getNodeId (adoptNode replayBinder)) traceMap of
                          Nothing ->
                            expectationFailure
                              ("Missing replay-binder provenance entry for " ++ show replayBinder)
                          Just mapped ->
                            adoptNode mapped `shouldBe` adoptNode sourceBinder
                )
                (etBinderArgs tr)
            vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

    it "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization" $ do
      let makeFactory = ELam "x" (ELam "y" (EVar "x"))
          expr =
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
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paPresolution = pres, paSolved = solved} -> do
          let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
              edgeWitnesses = IntMap.map (canonicalizeWitness canon) (prEdgeWitnesses pres)
              edgeTraces = IntMap.map (canonicalizeTrace canon) (prEdgeTraces pres)
              raisesForEdge (eid, ew) =
                [ (eid, n)
                  | OpRaise n <- getInstanceOps (ewWitness ew)
                ]
              raiseTargets = concatMap raisesForEdge (IntMap.toList edgeWitnesses)
          forM_ raiseTargets $ \(eid, n) ->
            case IntMap.lookup eid edgeTraces of
              Nothing ->
                expectationFailure ("Missing trace for edge with OpRaise: " ++ show eid)
              Just tr -> do
                let interiorKeys =
                      IntSet.fromList
                        [ getNodeId nid
                          | nid <- toListInterior (getEdgeSourceInterior (etInterior tr))
                        ]
                IntSet.member (getNodeId n) interiorKeys `shouldBe` True

    it "BUG-002-V4 translates a root Weaken to identity without replay metadata" $ do
      let makeFactory = ELam "x" (ELam "y" (EVar "x"))
          expr =
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
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paPresolution = pres} -> do
          ew0 <- case IntMap.lookup 0 (prEdgeWitnesses pres) of
            Just ew -> pure ew
            Nothing -> expectationFailure "Expected edge 0 witness" >> fail "missing edge 0 witness"
          tr0 <- case IntMap.lookup 0 (prEdgeTraces pres) of
            Just tr -> pure tr
            Nothing -> expectationFailure "Expected edge 0 trace" >> fail "missing edge 0 trace"
          -- Thesis Figure 15.3.4 (papers/these-finale-english.txt):
          -- T_chi(Weaken(r)) = epsilon at the expansion root.  Once the
          -- paired Graft/Weaken operations normalize away, no replay domain
          -- may be manufactured for that identity computation.
          etReplayContract tr0 `shouldBe` ReplayContractNone
          getInstanceOps (ewWitness ew0) `shouldBe` []
          etBinderArgs tr0 `shouldBe` []
          etBinderReplayMap tr0 `shouldBe` IntMap.empty
          etReplayDomainBinders tr0 `shouldBe` []

  it "handles higher-order polymorphic apply used twice" $ do
    -- let apply f x = f x; let id = \y. y; let a = apply id 1 in apply id True
    let expr =
          ELet
            "apply"
            (ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))
            ( ELet
                "id"
                (ELam "y" (EVar "y"))
                ( ELet
                    "a"
                    (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LInt 1)))
                    (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LBool True)))
                )
            )
    case runToSolvedDefault defaultPolySyms expr of
      Left err -> expectationFailure err
      Right res -> do
        validateStrict res
        let c = Solved.originalConstraint res
            nodes = cNodes c
        baseNames nodes `shouldContain` [BaseTy "Int"]
        baseNames nodes `shouldContain` [BaseTy "Bool"]
        noExpNodes nodes
        cInstEdges c `shouldBe` []
        cUnifyEdges c `shouldBe` []

  it "redirected let-use sites keep polymorphic schemes" $ do
    -- let id = \x. x in id id
    let expr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            (EApp (EVar "id") (EVar "id"))
    case runToPresolutionWithAnnDefault defaultPolySyms expr of
      Left err -> expectationFailure err
      Right (pres, ann) -> do
        let redirects = prRedirects pres
            varNodes = collectVarNodes "id" ann
            redirected =
              [ nid
                | nid <- varNodes,
                  chaseRedirects redirects nid /= nid
              ]
        varNodes `shouldSatisfy` (not . null)
        redirected `shouldSatisfy` (not . null)
    case runPipelineElab Set.empty expr of
      Left err -> expectationFailure (renderPipelineError err)
      Right (_term, ty) -> pretty ty `shouldSatisfy` ("∀" `isInfixOf`)

  it "applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently" $ do
    -- Exercise rewrite coverage on a shape with redirected + canonicalized let scheme roots.
    let rewriteExpr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            ( ELet
                "f"
                (EVar "id")
                ( ELet
                    "a"
                    (EApp (EVar "f") (ELit (LInt 1)))
                    (EApp (EVar "f") (ELit (LBool True)))
                )
            )
        -- Separately exercise the production canonicalizeAnn path via pipeline run.
        canonicalizePathExpr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            ( ELet
                "a"
                (EApp (EVar "id") (ELit (LInt 1)))
                (EApp (EVar "id") (ELit (LBool True)))
            )
    case runPipelineArtifactsDefault defaultPolySyms rewriteExpr of
      Left err -> expectationFailure err
      Right PipelineArtifacts {paPresolution = pres, paSolved = solved, paAnnotated = ann} -> do
        let schemeRoots = annLetSchemeRoots ann
        sourceRoot <-
          case listToMaybe schemeRoots of
            Just root -> pure root
            Nothing -> expectationFailure "expected at least one annotated let scheme root" *> pure (NodeId 0)
        let syntheticTarget =
              NodeId
                ( 1
                    + maximum
                      [ getNodeId nid
                        | nid <- annNodeOccurrences ann
                      ]
                )
            syntheticRedirects =
              IntMap.singleton (getNodeId sourceRoot) syntheticTarget
            annSyntheticRedirected = applyRedirectsToAnn syntheticRedirects ann
        annLetSchemeRoots annSyntheticRedirected
          `shouldBe` map (chaseRedirects syntheticRedirects) schemeRoots
        annNodeOccurrences annSyntheticRedirected
          `shouldBe` map (chaseRedirects syntheticRedirects) (annNodeOccurrences ann)
        syntheticTarget `shouldSatisfy` (`elem` annLetSchemeRoots annSyntheticRedirected)
        let redirects = prRedirects pres
            annRedirected = applyRedirectsToAnn redirects ann
            staleRedirectNodes =
              [ nid
                | nid <- annNodeOccurrences annRedirected,
                  chaseRedirects redirects nid /= nid
              ]
        annNodeOccurrences annRedirected
          `shouldBe` map (chaseRedirects redirects) (annNodeOccurrences ann)
        staleRedirectNodes `shouldBe` []
        annRootNode annRedirected `shouldSatisfy` (\nid -> chaseRedirects redirects nid == nid)
        validateStrict solved
        let canonicalize = canonicalizeNode (canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects redirects nid)))
            annCanonical = canonicalizeAnn canonicalize annRedirected
            staleCanonicalNodes =
              [ nid
                | nid <- annNodeOccurrences annCanonical,
                  canonicalize nid /= nid
              ]
        annNodeOccurrences annCanonical
          `shouldBe` map canonicalize (annNodeOccurrences annRedirected)
        annNodeOccurrences annCanonical `shouldSatisfy` (not . null)
        staleCanonicalNodes `shouldBe` []
        annRootNode annCanonical `shouldSatisfy` (\nid -> canonicalize nid == nid)
    case runPipelineElab Set.empty canonicalizePathExpr of
      Left err -> expectationFailure (renderPipelineError err)
      Right (_term, _ty) -> pure ()

  it "generalizes reused constructors via make const" $ do
    -- let make x = (\z -> x) in let c1 = make 2 in let c2 = make False in c1 True
    let expr =
          ELet
            "make"
            (ELam "x" (ELam "z" (EVar "x")))
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt 2)))
                ( ELet
                    "c2"
                    (EApp (EVar "make") (ELit (LBool False)))
                    (EApp (EVar "c1") (ELit (LBool True)))
                )
            )
    case runToSolvedDefault defaultPolySyms expr of
      Left err -> expectationFailure err
      Right res -> do
        validateStrict res
        let c = Solved.originalConstraint res
            nodes = cNodes c
        baseNames nodes `shouldContain` [BaseTy "Int"]
        baseNames nodes `shouldContain` [BaseTy "Bool"]
        noExpNodes nodes
        cInstEdges c `shouldBe` []
        cUnifyEdges c `shouldBe` []

  it "make let-c1-apply-bool path typechecks to Int" $ do
    -- BUG-2026-02-06-002 / H15 follow-up:
    -- let make = \x.\y.x in let c1 = make (-4) in c1 True
    let expr =
          ELet
            "make"
            (ELam "x" (ELam "y" (EVar "x")))
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )
    ty <- expectCanonicalPipelineSuccessType expr
    ty `shouldBe` TestElab.tBase (BaseTy "Int")

  describe "BUG-2026-02-08-004 sentinel" $ do
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
    it "BUG-2026-02-08-004 nested let + annotated lambda constructs Int" $ do
      ty <- expectCanonicalPipelineSuccessType expr
      ty `shouldBe` TestElab.tBase (BaseTy "Int")

  it "A6 parity: bounded alias + coercion-heavy path agrees across canonical construction and independent typeCheck" $ do
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
          ELet
            "c"
            (EAnn rhs schemeTy)
            (EAnn (EVar "c") ann)
        normExpr = unsafeNormalizeExpr expr
        expectPolyBinaryId ty =
          case ty of
            TForallRef ref Nothing (TArrow (TVarRef domRef) (TArrow (TVarRef domRef') (TVarRef codRef)))
              | domRef == ref && domRef' == ref && codRef == ref -> pure ()
            other ->
              expectationFailure
                ("Expected forall a. a -> a -> a, got: " ++ show other)
    case runPipelineElab Set.empty normExpr of
      Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
      Right (term, ty) -> do
        expectPolyBinaryId ty
        checkedByTypeChecker <-
          case typeCheck term of
            Left tcErr -> expectationFailure ("typeCheck(canonical term) failed: " ++ show tcErr) >> fail "typeCheck failed"
            Right out -> pure out
        expectPolyBinaryId checkedByTypeChecker
        checkedByTypeChecker `shouldBe` ty

  it "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in the canonical pipeline" $ do
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
          ELet
            "c"
            (EAnn rhs schemeTy)
            ( EApp
                (EApp (EAnn (EVar "c") ann) (ELit (LInt 1)))
                (ELit (LInt 2))
            )
        normExpr = unsafeNormalizeExpr expr
        expectedTy = TestElab.tBase (BaseTy "Int")
    let expectInt label result =
          case result of
            Left err ->
              expectationFailure (label ++ " failed:\n" ++ renderPipelineError err)
            Right (term, ty) -> do
              ty `shouldBe` expectedTy
              typeCheck term `shouldBe` Right expectedTy
    expectInt "canonical pipeline" (runPipelineElab Set.empty normExpr)

  describe "post-quotient expansion construction projection" $ do
    it "projects unified fresh copies at their raw-parent LCA independently of edge order" $ do
      let owner = GenNodeId 9101
          firstParent = NodeId 9111
          secondParent = NodeId 9112
          firstFresh = NodeId 9121
          secondFresh = NodeId 9122
          representative = NodeId 9131
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (firstParent, var firstParent),
                      (secondParent, var secondParent)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef firstParent), (genRef owner, BindFlex)),
                      (nodeRefKey (typeRef secondParent), (genRef owner, BindFlex))
                    ],
                cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          mkSemanticConstruction child parent =
            requireRight
              ( mkRawExpansionConstruction
                  ( IntMap.singleton
                      (nodeRefKey (typeRef child))
                      (typeRef parent, BindFlex)
                  )
                  IntSet.empty
                  (IntSet.singleton (getNodeId child))
              )
          adopt node
            | node == firstFresh || node == secondFresh = representative
            | otherwise = node
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId representative)
                (genRef owner, owner),
              IntMap.singleton
                (getNodeId representative)
                (genRef owner, BindFlex)
            )
      firstConstruction <- mkSemanticConstruction firstFresh firstParent
      secondConstruction <- mkSemanticConstruction secondFresh secondParent
      let forward =
            IntMap.fromList
              [ (1, firstConstruction),
                (2, secondConstruction)
              ]
          reverseOrder =
            IntMap.fromList
              [ (2, firstConstruction),
                (1, secondConstruction)
              ]
          project constructions =
            requireRight
              ( prepareElaborationExpansionConstructionPlacementsForTest
                  base
                  adopt
                  IntMap.empty
                  constructions
              )
      project forward `shouldReturn` expected
      project reverseOrder `shouldReturn` expected

    it "rejects unified construction origins with no common lexical ancestor" $ do
      let firstOwner = GenNodeId 9132
          secondOwner = GenNodeId 9133
          firstParent = NodeId 9134
          secondParent = NodeId 9135
          firstFresh = NodeId 9136
          secondFresh = NodeId 9137
          representative = NodeId 9138
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (firstParent, var firstParent),
                      (secondParent, var secondParent)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef firstParent), (genRef firstOwner, BindFlex)),
                      (nodeRefKey (typeRef secondParent), (genRef secondOwner, BindFlex))
                    ],
                cGenNodes =
                  fromListGen
                    [ (firstOwner, GenNode firstOwner []),
                      (secondOwner, GenNode secondOwner [])
                    ]
              }
          construction child parent =
            requireRight
              ( mkRawExpansionConstruction
                  ( IntMap.singleton
                      (nodeRefKey (typeRef child))
                      (typeRef parent, BindFlex)
                  )
                  IntSet.empty
                  (IntSet.singleton (getNodeId child))
              )
          adopt node
            | node == firstFresh || node == secondFresh = representative
            | otherwise = node
      firstConstruction <- construction firstFresh firstParent
      secondConstruction <- construction secondFresh secondParent
      case
          prepareElaborationExpansionConstructionPlacementsForTest
            base
            adopt
            IntMap.empty
            (IntMap.fromList [(1, firstConstruction), (2, secondConstruction)])
        of
          Left err ->
            show err `shouldSatisfy` ("no quotient LCA" `isInfixOf`)
          Right accepted ->
            expectationFailure
              ("Expected unrelated construction scopes to be rejected, got: " ++ show accepted)

    it "takes the LCA after parent classes have themselves been quotiented" $ do
      let owner = GenNodeId 9140
          rawGrandparent = NodeId 9141
          firstRawParent = NodeId 9142
          secondRawParent = NodeId 9143
          firstRawChild = NodeId 9144
          secondRawChild = NodeId 9145
          parent = NodeId 9146
          child = NodeId 9147
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes = fromListNode [(rawGrandparent, var rawGrandparent)],
                cBindParents =
                  IntMap.singleton
                    (nodeRefKey (typeRef rawGrandparent))
                    (genRef owner, BindFlex),
                cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          adopt node
            | node == firstRawParent || node == secondRawParent = parent
            | node == firstRawChild || node == secondRawChild = child
            | otherwise = node
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId child)
                (typeRef parent, owner),
              IntMap.fromList
                [ (getNodeId parent, (typeRef rawGrandparent, BindFlex)),
                  (getNodeId child, (typeRef parent, BindFlex))
                ]
            )
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.fromList
                  [ ( nodeRefKey (typeRef firstRawParent),
                      (typeRef rawGrandparent, BindFlex)
                    ),
                    ( nodeRefKey (typeRef secondRawParent),
                      (typeRef rawGrandparent, BindFlex)
                    ),
                    ( nodeRefKey (typeRef firstRawChild),
                      (typeRef firstRawParent, BindFlex)
                    ),
                    ( nodeRefKey (typeRef secondRawChild),
                      (typeRef secondRawParent, BindFlex)
                    )
                  ]
              )
              IntSet.empty
              ( IntSet.fromList
                  [ getNodeId firstRawChild,
                    getNodeId secondRawChild
                  ]
              )
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        adopt
        IntMap.empty
        (IntMap.singleton 1 construction)
        `shouldBe` Right expected

    it "derives a child scope from its projected parent class rather than an arbitrary raw origin" $ do
      let rootOwner = GenNodeId 9148
          firstOwner = GenNodeId 9149
          secondOwner = GenNodeId 9150
          firstRawParent = NodeId 9151
          secondRawParent = NodeId 9152
          firstRawChild = NodeId 9153
          secondRawChild = NodeId 9154
          parent = NodeId 9155
          child = NodeId 9156
          base =
            emptyConstraint
              { cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (genRef firstOwner), (genRef rootOwner, BindFlex)),
                      (nodeRefKey (genRef secondOwner), (genRef rootOwner, BindFlex))
                    ],
                cGenNodes =
                  fromListGen
                    [ (rootOwner, GenNode rootOwner []),
                      (firstOwner, GenNode firstOwner []),
                      (secondOwner, GenNode secondOwner [])
                    ]
              }
          adopt node
            | node == firstRawParent || node == secondRawParent = parent
            | node == firstRawChild || node == secondRawChild = child
            | otherwise = node
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId child)
                (typeRef parent, rootOwner),
              IntMap.fromList
                [ (getNodeId parent, (genRef rootOwner, BindFlex)),
                  (getNodeId child, (typeRef parent, BindFlex))
                ]
            )
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.fromList
                  [ ( nodeRefKey (typeRef firstRawParent),
                      (genRef firstOwner, BindFlex)
                    ),
                    ( nodeRefKey (typeRef secondRawParent),
                      (genRef secondOwner, BindFlex)
                    ),
                    ( nodeRefKey (typeRef firstRawChild),
                      (typeRef firstRawParent, BindFlex)
                    ),
                    ( nodeRefKey (typeRef secondRawChild),
                      (typeRef secondRawParent, BindFlex)
                    )
                  ]
              )
              IntSet.empty
              ( IntSet.fromList
                  [ getNodeId firstRawChild,
                    getNodeId secondRawChild
                  ]
              )
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        adopt
        IntMap.empty
        (IntMap.singleton 1 construction)
        `shouldBe` Right expected

    it "keeps a source-projected semantic occurrence below its copied result parent" $ do
      let owner = GenNodeId 9151
          copiedResult = NodeId 9161
          argumentOccurrence = NodeId 9171
          semanticOccurrence = NodeId 9172
          representative = NodeId 9181
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes = fromListNode [(copiedResult, var copiedResult)],
                cBindParents =
                  IntMap.singleton
                    (nodeRefKey (typeRef copiedResult))
                    (genRef owner, BindFlex),
                cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          adopt node
            | node == argumentOccurrence || node == semanticOccurrence = representative
            | otherwise = node
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId representative)
                (typeRef copiedResult, owner),
              IntMap.singleton
                (getNodeId representative)
                (typeRef copiedResult, BindFlex)
            )
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.fromList
                  [ ( nodeRefKey (typeRef argumentOccurrence),
                      (genRef owner, BindFlex)
                    ),
                    ( nodeRefKey (typeRef semanticOccurrence),
                      (typeRef copiedResult, BindFlex)
                    )
                  ]
              )
              (IntSet.singleton (getNodeId argumentOccurrence))
              (IntSet.singleton (getNodeId semanticOccurrence))
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        adopt
        (IntMap.singleton (getNodeId representative) copiedResult)
        (IntMap.singleton 1 construction)
        `shouldBe` Right expected

    it "retains a source-projected copied-root support needed by a semantic occurrence" $ do
      let owner = GenNodeId 9191
          sourceBase = NodeId 9192
          rawCopiedRoot = NodeId 9193
          rawSemantic = NodeId 9194
          copiedRoot = NodeId 9195
          semantic = NodeId 9196
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes = fromListNode [(sourceBase, var sourceBase)],
                cBindParents =
                  IntMap.singleton
                    (nodeRefKey (typeRef sourceBase))
                    (genRef owner, BindFlex),
                cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          adopt node
            | node == rawCopiedRoot = copiedRoot
            | node == rawSemantic = semantic
            | otherwise = node
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId semantic)
                (typeRef copiedRoot, owner),
              IntMap.fromList
                [ (getNodeId copiedRoot, (genRef owner, BindFlex)),
                  (getNodeId semantic, (typeRef copiedRoot, BindFlex))
                ]
            )
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.fromList
                  [ ( nodeRefKey (typeRef rawCopiedRoot),
                      (genRef owner, BindFlex)
                    ),
                    ( nodeRefKey (typeRef rawSemantic),
                      (typeRef rawCopiedRoot, BindFlex)
                    )
                  ]
              )
              IntSet.empty
              (IntSet.singleton (getNodeId rawSemantic))
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        adopt
        (IntMap.singleton (getNodeId copiedRoot) sourceBase)
        (IntMap.singleton 1 construction)
        `shouldBe` Right expected

    it "retains a source-projected argument class when it is also semantic support" $ do
      let owner = GenNodeId 9197
          rawArgument = NodeId 9198
          rawSemantic = NodeId 9199
          argument = NodeId 9200
          semantic = NodeId 9201
          base =
            emptyConstraint
              { cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          adopt node
            | node == rawArgument = argument
            | node == rawSemantic = semantic
            | otherwise = node
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId semantic)
                (typeRef argument, owner),
              IntMap.fromList
                [ (getNodeId argument, (genRef owner, BindFlex)),
                  (getNodeId semantic, (typeRef argument, BindFlex))
                ]
            )
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.fromList
                  [ ( nodeRefKey (typeRef rawArgument),
                      (genRef owner, BindFlex)
                    ),
                    ( nodeRefKey (typeRef rawSemantic),
                      (typeRef rawArgument, BindFlex)
                    )
                  ]
              )
              (IntSet.singleton (getNodeId rawArgument))
              (IntSet.singleton (getNodeId rawSemantic))
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        adopt
        (IntMap.singleton (getNodeId argument) (NodeId 9299))
        (IntMap.singleton 1 construction)
        `shouldBe` Right expected

    it "uses rigid copied nodes as path support without making them role candidates" $ do
      let owner = GenNodeId 9202
          rigidSupport = NodeId 9203
          semantic = NodeId 9204
          base =
            emptyConstraint
              { cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          expected =
            ( IntMap.empty,
              IntMap.singleton
                (getNodeId semantic)
                (typeRef rigidSupport, owner),
              IntMap.fromList
                [ (getNodeId rigidSupport, (genRef owner, BindRigid)),
                  (getNodeId semantic, (typeRef rigidSupport, BindFlex))
                ]
            )
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.fromList
                  [ ( nodeRefKey (typeRef rigidSupport),
                      (genRef owner, BindRigid)
                    ),
                    ( nodeRefKey (typeRef semantic),
                      (typeRef rigidSupport, BindFlex)
                    )
                  ]
              )
              IntSet.empty
              (IntSet.singleton (getNodeId semantic))
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        id
        IntMap.empty
        (IntMap.singleton 1 construction)
        `shouldBe` Right expected

    it "does not claim a quotient class that contains an actual base node" $ do
      let owner = GenNodeId 9205
          baseMember = NodeId 9206
          rawFresh = NodeId 9207
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes = fromListNode [(baseMember, var baseMember)],
                cBindParents =
                  IntMap.singleton
                    (nodeRefKey (typeRef baseMember))
                    (genRef owner, BindFlex),
                cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          adopt node
            | node == rawFresh = baseMember
            | otherwise = node
      construction <-
        requireRight
          ( mkRawExpansionConstruction
              ( IntMap.singleton
                  (nodeRefKey (typeRef rawFresh))
                  (genRef owner, BindFlex)
              )
              IntSet.empty
              (IntSet.singleton (getNodeId rawFresh))
          )
      prepareElaborationExpansionConstructionPlacementsForTest
        base
        adopt
        IntMap.empty
        (IntMap.singleton 1 construction)
        `shouldBe` Right (IntMap.empty, IntMap.empty, IntMap.empty)

    it "rejects a rigid node named as an expansion role at certificate construction" $ do
      let owner = GenNodeId 9208
          rigidRole = NodeId 9209
      mkRawExpansionConstruction
        ( IntMap.singleton
            (nodeRefKey (typeRef rigidRole))
            (genRef owner, BindRigid)
        )
        IntSet.empty
        (IntSet.singleton (getNodeId rigidRole))
        `shouldSatisfy` isLeft

    it "rejects contradictory exact parents for the same raw fresh node" $ do
      let owner = GenNodeId 9201
          firstParent = NodeId 9211
          secondParent = NodeId 9212
          fresh = NodeId 9221
          var node = TyVar {tnId = node, tnBound = Nothing}
          base =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (firstParent, var firstParent),
                      (secondParent, var secondParent)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef firstParent), (genRef owner, BindFlex)),
                      (nodeRefKey (typeRef secondParent), (genRef owner, BindFlex))
                    ],
                cGenNodes = fromListGen [(owner, GenNode owner [])]
              }
          construction parent =
            requireRight
              ( mkRawExpansionConstruction
                  ( IntMap.singleton
                      (nodeRefKey (typeRef fresh))
                      (typeRef parent, BindFlex)
                  )
                  IntSet.empty
                  (IntSet.singleton (getNodeId fresh))
              )
      firstConstruction <- construction firstParent
      secondConstruction <- construction secondParent
      case
          prepareElaborationExpansionConstructionPlacementsForTest
            base
            id
            IntMap.empty
            (IntMap.fromList [(1, firstConstruction), (2, secondConstruction)])
        of
          Left err ->
            show err
              `shouldSatisfy` ("conflicting raw creation-time placements" `isInfixOf`)
          Right accepted ->
            expectationFailure
              ("Expected contradictory raw placements to be rejected, got: " ++ show accepted)

  describe "BUG-2026-02-06-002 polymorphic factory regression" $ do
    let makeFactory = ELam "x" (ELam "y" (EVar "x"))
        makeOnlyExpr = ELet "make" makeFactory (EVar "make")
        makeAppExpr = ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))
        letC1ReturnExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EVar "c1")
            )
        letC1ApplyBoolExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )

        intTy = TestElab.tBase (BaseTy "Int")
        expectForallArgumentReturningInt ty =
          case ty of
            TForallRef binderRef Nothing (TArrow (TVarRef argumentRef) resultTy)
              | binderRef == argumentRef,
                resultTy == intTy -> pure ()
            other ->
              expectationFailure
                ("Expected forall a. a -> Int, got: " ++ show other)

        findLetBinding sought term =
          case term of
            Elab.ELet resolved scheme rhs body
              | resolvedVarName resolved == sought -> Just (scheme, rhs)
              | Just found <- findLetBinding sought rhs -> Just found
              | otherwise -> findLetBinding sought body
            Elab.ELam _ body -> findLetBinding sought body
            Elab.EApp fun argument ->
              case findLetBinding sought fun of
                Just found -> Just found
                Nothing -> findLetBinding sought argument
            Elab.ETyAbsRef _ _ body -> findLetBinding sought body
            Elab.ETyInst body _ -> findLetBinding sought body
            Elab.ERoll _ body -> findLetBinding sought body
            Elab.EUnroll body -> findLetBinding sought body
            Elab.EVarNode _ -> Nothing
            Elab.ELit _ -> Nothing

        prepareFactoryArtifactView expr = do
          ConstraintResult
            { crConstraint = c0,
              crAnnotated = ann,
              crIdentityGenerator = packetIdentityGenerator,
              crExactProducerTypes = exactProducerTypes,
              crSourceTypeBinderIdentities = sourceTypeBinderIdentities
            } <-
            requireRight (runConstraintDefault defaultPolySyms expr)
          let cNorm = CNormalize.normalize c0
          (cAcyclic, acyc) <-
            requireRight (Acyc.breakCyclesAndCheckAcyclicity cNorm)
          pres <-
            requireRight (computePresolution defaultTraceConfig acyc cAcyclic)
          artifact <-
            requireRight
              ( prepareGeneralizationArtifact
                  defaultTraceConfig
                  packetIdentityGenerator
                  exactProducerTypes
                  IntMap.empty
                  sourceTypeBinderIdentities
                  cAcyclic
                  pres
                  ann
              )
          pure (preparedGeneralizationArtifactTestView artifact, pres)

    it "make-only still elaborates as polymorphic factory" $
      do
        ty <- expectCanonicalPipelineSuccessType makeOnlyExpr
        ty `shouldSatisfy` containsForallTy
        ty `shouldSatisfy` containsArrowTy
        show ty `shouldNotSatisfy` ("TBottom" `isInfixOf`)

    it "make-app generalizes its unused argument" $ do
      (term, ty) <- expectCanonicalPipelineSuccess makeAppExpr
      expectForallArgumentReturningInt ty
      let resultConstruction candidate =
            case candidate of
              Elab.ELet _ _ _ body -> resultConstruction body
              Elab.ETyAbsRef binderRef Nothing _ -> Just binderRef
              _ -> Nothing
      case (resultConstruction term, ty) of
        (Just termBinderRef, TForallRef typeBinderRef Nothing _) ->
          typeBinderRefsSameIdentity termBinderRef typeBinderRef `shouldBe` True
        shape ->
          expectationFailure
            ( "Expected the let-body result construction to emit its forall, got: "
                ++ show shape
                ++ "; full term="
                ++ show term
            )

    it "does not classify monomorphic or ambient rank-1 results as local polymorphic endpoints" $ do
      let monomorphicApplication =
            EApp
              (ELam "x" (EVar "x"))
              (ELit (LInt 1))
          ambientPolymorphicApplication =
            ELam
              "f"
              ( ELam
                  "x"
                  (EApp (EVar "f") (EVar "x"))
              )
      expectCanonicalPipelineSuccessType monomorphicApplication
        `shouldReturn` intTy
      ambientTy <- expectCanonicalPipelineSuccessType ambientPolymorphicApplication
      ambientTy `shouldSatisfy` containsForallTy
      ambientTy `shouldSatisfy` containsArrowTy

    it "projects each expansion binder to its exact creation-time placement" $ do
      (artifactView, pres) <- prepareFactoryArtifactView letC1ReturnExpr
      let base = preparedTestBaseConstraint artifactView
          constructed = preparedTestGeneralizationConstraint artifactView
          canonical = preparedTestCanonicalizeNode artifactView
          expansionArgumentOwners =
            preparedTestExpansionArgumentScopes artifactView
          semanticMetaParents =
            preparedTestExpansionSemanticMetaParents artifactView
          constructionParents =
            preparedTestExpansionConstructionParents artifactView
          solvedToBase = preparedTestSolvedToBase artifactView
          crossSourceSemanticPlacements =
            [ ()
              | metaKey <- IntMap.keys semanticMetaParents,
                Just (TypeRef parent, _) <-
                  [IntMap.lookup metaKey constructionParents],
                Just childSource <- [IntMap.lookup metaKey solvedToBase],
                Just parentSource <-
                  [IntMap.lookup (getNodeId parent) solvedToBase],
                childSource /= parentSource
            ]
          baseEdges =
            IntMap.fromList
              [ (getEdgeId (instEdgeId edge), edge)
                | edge <- cInstEdges base
              ]
          creationClaims argumentKey =
            [ owner
              | (edgeKey, traceInfo) <- IntMap.toList (prEdgeTraces pres),
                (_binder, argument) <- etBinderArgs traceInfo,
                getNodeId (canonical argument) == argumentKey,
                Just edge <- [IntMap.lookup edgeKey baseEdges],
                Just owner <-
                  [ firstGenAncestorForTest
                      (cBindParents base)
                      (typeRef (instRight edge))
                  ]
            ]
      expansionArgumentOwners `shouldSatisfy` (not . IntMap.null)
      semanticMetaParents `shouldSatisfy` (not . IntMap.null)
      crossSourceSemanticPlacements `shouldSatisfy` (not . null)
      forM_ (IntMap.toList expansionArgumentOwners) $ \(argumentKey, owner) -> do
        creationClaims argumentKey `shouldContain` [owner]
        firstGenAncestorForTest
          (cBindParents constructed)
          (typeRef (NodeId argumentKey))
          `shouldBe` Just owner
      forM_ (IntMap.toList semanticMetaParents) $ \(metaKey, (parent, owner)) -> do
        IntMap.lookup
          (nodeRefKey (typeRef (NodeId metaKey)))
          (cBindParents constructed)
          `shouldBe` Just (parent, BindFlex)
        firstGenAncestorForTest
          (cBindParents constructed)
          (typeRef (NodeId metaKey))
          `shouldBe` Just owner

    it "let-c1-return preserves forall a. a -> Int" $ do
      (term, ty) <- expectCanonicalPipelineSuccess letC1ReturnExpr
      (artifactView, _pres) <- prepareFactoryArtifactView letC1ReturnExpr
      let semanticMetaParents =
            preparedTestExpansionSemanticMetaParents artifactView
      expectForallArgumentReturningInt ty
      case (findLetBinding "c1" term, ty) of
        (Just (c1Scheme, c1Rhs), TForallRef resultBinderRef Nothing _) ->
          case Elab.schemeToType c1Scheme of
            TForallRef schemeBinderRef Nothing _ -> do
              typeBinderRefsSameIdentity schemeBinderRef resultBinderRef `shouldBe` True
              containsIdentityLinkedConstructionBridge c1Rhs `shouldBe` True
              case typeBinderRefNode resultBinderRef of
                Just binderNode ->
                  IntMap.member
                    (getNodeId binderNode)
                    semanticMetaParents
                    `shouldBe` True
                Nothing ->
                  expectationFailure
                    ("Expected c1 result binder to retain its graph construction identity: "
                      ++ show resultBinderRef)
            schemeTy ->
              expectationFailure
                ("Expected polymorphic c1 scheme, got: " ++ show schemeTy)
        shape ->
          expectationFailure
            ("Expected c1 RHS construction to emit its forall, got: " ++ show shape)

    it "let-c1-apply-bool instantiates the argument and returns Int" $ do
      ty <- expectCanonicalPipelineSuccessType letC1ApplyBoolExpr
      ty `shouldBe` intTy

  describe "Phase 3 atomic wrapping equivalence gates" $ do
    let makeFactory :: SurfaceExpr
        makeFactory =
          ELam "x" (ELam "y" (EVar "x"))

        makeOnlyExpr :: SurfaceExpr
        makeOnlyExpr =
          ELet "make" makeFactory (EVar "make")

        makeAppExpr :: SurfaceExpr
        makeAppExpr =
          ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))

        letC1ReturnExpr :: SurfaceExpr
        letC1ReturnExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EVar "c1")
            )

        bugExpr :: SurfaceExpr
        bugExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )

        letC1ApplyIntExpr :: SurfaceExpr
        letC1ApplyIntExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LInt 0)))
            )

        mixedFactoryExpr :: SurfaceExpr
        mixedFactoryExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                ( ELet
                    "c2"
                    (EApp (EVar "make") (ELit (LBool False)))
                    ( ELet
                        "_"
                        (EApp (EVar "c2") (ELit (LInt 0)))
                        (EApp (EVar "c1") (ELit (LBool True)))
                    )
                )
            )

        lambdaLetIdExpr :: SurfaceExpr
        lambdaLetIdExpr =
          ELam
            "y"
            ( ELet
                "id"
                (ELam "x" (EVar "x"))
                (EApp (EVar "id") (EVar "y"))
            )

        expectForallIdentityArrow :: ElabType -> Expectation
        expectForallIdentityArrow ty =
          case ty of
            TForallRef ref Nothing (TArrow (TVarRef domRef) (TVarRef codRef))
              | domRef == ref && codRef == ref -> pure ()
            other ->
              expectationFailure
                ("Expected forall identity arrow (forall a. a -> a), got: " ++ show other)

        intTy = TestElab.tBase (BaseTy "Int")

        expectForallArgumentReturningInt ty =
          case ty of
            TForallRef binderRef Nothing (TArrow (TVarRef argumentRef) resultTy)
              | binderRef == argumentRef,
                resultTy == intTy -> pure ()
            other ->
              expectationFailure
                ("Expected forall a. a -> Int, got: " ++ show other)

    it "gate: make let-c1-apply-bool typechecks to Int" $ do
      ty <- expectCanonicalPipelineSuccessType bugExpr
      ty `shouldBe` intTy

    it "gate: partial factory application generalizes its unused argument" $ do
      ty <- expectCanonicalPipelineSuccessType makeAppExpr
      expectForallArgumentReturningInt ty

    it "gate: let-bound partial factory preserves forall a. a -> Int" $ do
      ty <- expectCanonicalPipelineSuccessType letC1ReturnExpr
      expectForallArgumentReturningInt ty

    it "gate: let-bound partial factory instantiates at Int" $ do
      ty <- expectCanonicalPipelineSuccessType letC1ApplyIntExpr
      ty `shouldBe` intTy

    it "gate: independent factory instances retain their own result types" $ do
      ty <- expectCanonicalPipelineSuccessType mixedFactoryExpr
      ty `shouldBe` intTy

    it "gate: make-only prefix remains a polymorphic factory" $
      case runPipelineElab Set.empty (unsafeNormalizeExpr makeOnlyExpr) of
        Left err -> expectationFailure ("pipeline failed: " ++ renderPipelineError err)
        Right (_term, ty) -> do
          ty `shouldSatisfy` containsForallTy
          ty `shouldSatisfy` containsArrowTy
          show ty `shouldNotSatisfy` ("TBottom" `isInfixOf`)

    it "gate: \\y. let id = (\\x. x) in id y has type forall a. a -> a" $
      case runPipelineElab Set.empty (unsafeNormalizeExpr lambdaLetIdExpr) of
        Left err -> expectationFailure ("pipeline failed: " ++ renderPipelineError err)
        Right (_term, ty) -> expectForallIdentityArrow ty

  describe "Phase 4 regression matrix" $ do
    it "preserves thesis-exact OpWeaken on annotation edges and expansion assignments" $ do
      let annTy = mkForalls [] (STArrow (STBase "Int") (STBase "Int"))
          expr =
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) annTy)
              (EApp (EVar "f") (ELit (LInt 1)))

          hasWeakenOp op = case op of
            OpWeaken _ -> True
            _ -> False

      case runToPresolutionWithAnnDefault defaultPolySyms expr of
        Left err -> expectationFailure ("Pipeline failed: " ++ err)
        Right (presolution, _ann) -> do
          let cPres = prConstraint presolution
              exps = prEdgeExpansions presolution
              ews = prEdgeWitnesses presolution
              annEdges = IntSet.toList (cAnnEdges cPres)
          annEdges `shouldSatisfy` (not . null)
          forM_ annEdges $ \eid -> do
            IntMap.member eid exps `shouldBe` True
            case IntMap.lookup eid ews of
              Nothing -> expectationFailure ("Missing witness for annotation edge " ++ show eid)
              Just ew ->
                getInstanceOps (ewWitness ew) `shouldSatisfy` any hasWeakenOp

  describe "Pipeline soundness proxies" $ do
    -- The generator below emits only closed, well-typed source terms, so a
    -- pipeline or type-check failure is itself a regression.  One-step
    -- preservation discards only irreducible terms, for which its premise does
    -- not hold; progress and multi-step preservation have no discard branch.

    it "BUG-2026-02-20-001: stepped annotated-let identity remains type-checkable" $ do
      let ann = mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
          expr =
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) ann)
              (EApp (EVar "f") (ELit (LInt 7)))
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
        Right (term, _ty) -> do
          ty0 <-
            case typeCheck term of
              Left tcErr ->
                expectationFailure ("typeCheck(term) failed:\n" ++ show tcErr)
                  >> fail "typeCheck(term) failed"
              Right out -> pure out
          stepped <-
            case step term of
              Nothing ->
                expectationFailure "Expected a reduction step for elaborated let term"
                  >> fail "missing step"
              Just t' -> pure t'
          typeCheck stepped `shouldBe` Right ty0

    it "keeps an outer lambda type binder ambient while constructing its let-body application" $ do
      let containsTypeAbstractionWithIdentity targetRef candidate =
            case candidate of
              Elab.EVarNode _ -> False
              Elab.ELit _ -> False
              Elab.ELam _ body -> containsTypeAbstractionWithIdentity targetRef body
              Elab.EApp fn arg ->
                containsTypeAbstractionWithIdentity targetRef fn
                  || containsTypeAbstractionWithIdentity targetRef arg
              Elab.ELet _ _ rhs body ->
                containsTypeAbstractionWithIdentity targetRef rhs
                  || containsTypeAbstractionWithIdentity targetRef body
              Elab.ETyAbsRef ref _ body ->
                typeBinderRefsSameIdentity targetRef ref
                  || containsTypeAbstractionWithIdentity targetRef body
              Elab.ETyInst inner _ -> containsTypeAbstractionWithIdentity targetRef inner
              Elab.ERoll _ body -> containsTypeAbstractionWithIdentity targetRef body
              Elab.EUnroll body -> containsTypeAbstractionWithIdentity targetRef body
          expr =
            ELam
              "y"
              ( ELet
                  "id"
                  (ELam "x" (EVar "x"))
                  (EApp (EVar "id") (EVar "y"))
              )
      (term, pipelineTy) <-
        requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      typeCheck term `shouldBe` Right pipelineTy
      case term of
        Elab.ETyAbsRef outerRef _ (Elab.ELam _ lambdaBody) ->
          containsTypeAbstractionWithIdentity outerRef lambdaBody `shouldBe` False
        other ->
          expectationFailure
            ("expected one outer type abstraction around the lambda, got " ++ show other)

    it "Pipeline preservation proxy: elaborated term preserves type under step" $
      property $
        withMaxSuccess 300 $
          forAll genClosedWellTypedExpr $ \expr ->
            let pipelineResult = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                pipelineSuccess = isRight pipelineResult
             in checkCoverage $
                  cover 20 pipelineSuccess "pipeline-success" $
                    case pipelineResult of
                      Left err ->
                        counterexample
                          ( "pipeline preservation setup failed\nexpr: "
                              ++ show expr
                              ++ "\nerror: "
                              ++ renderPipelineError err
                          )
                          False
                      Right (term, _pipelineTy) ->
                        let typeCheckResult = typeCheck term
                            typeCheckSuccess = isRight typeCheckResult
                            stepResult =
                              case step term of
                                Nothing -> Nothing
                                Just term' -> Just (term', typeCheck term')
                            reducible = isJust stepResult
                            steppedTypeCheckSuccess =
                              case stepResult of
                                Nothing -> False
                                Just (_term', result) -> isRight result
                         in cover 20 typeCheckSuccess "typecheck-success" $
                              cover 5 reducible "reducible" $
                                cover 5 steppedTypeCheckSuccess "stepped-typecheck-success" $
                                  case typeCheckResult of
                                    Left tcErr ->
                                      counterexample
                                        ( "pipeline preservation source type-check failed\nexpr: "
                                            ++ show expr
                                            ++ "\nterm: "
                                            ++ show term
                                            ++ "\nerror: "
                                            ++ show tcErr
                                        )
                                        False
                                    Right ty ->
                                      case stepResult of
                                        Nothing -> discard
                                        Just (term', Left tcErr) ->
                                          counterexample
                                            ( "pipeline preservation stepped type-check failed\nexpr: "
                                                ++ show expr
                                                ++ "\nterm: "
                                                ++ show term
                                                ++ "\nterm': "
                                                ++ show term'
                                                ++ "\nerror: "
                                                ++ show tcErr
                                            )
                                            False
                                        Just (term', Right ty') ->
                                          counterexample
                                            ( "pipeline preservation failed\nexpr: "
                                                ++ show expr
                                                ++ "\nterm: "
                                                ++ show term
                                                ++ "\nterm': "
                                                ++ show term'
                                                ++ "\ntype(term): "
                                                ++ show ty
                                                ++ "\ntype(term'): "
                                                ++ show ty'
                                            )
                                            (ty' === ty)

    it "Pipeline progress proxy: elaborated well-typed closed term is value or steps" $
      property $
        withMaxSuccess 300 $
          forAll genClosedWellTypedExpr $ \expr ->
            let pipelineResult = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                pipelineSuccess = isRight pipelineResult
             in checkCoverage $
                  cover 20 pipelineSuccess "pipeline-success" $
                    case pipelineResult of
                      Left err ->
                        counterexample
                          ( "pipeline progress setup failed\nexpr: "
                              ++ show expr
                              ++ "\nerror: "
                              ++ renderPipelineError err
                          )
                          False
                      Right (term, _pipelineTy) ->
                        let typeCheckResult = typeCheck term
                            typeCheckSuccess = isRight typeCheckResult
                            valueResult = isValue term
                            stepResult = step term
                            canStep = isJust stepResult
                         in cover 20 typeCheckSuccess "typecheck-success" $
                              cover 20 valueResult "value" $
                                cover 5 canStep "steps" $
                                  case typeCheckResult of
                                    Left tcErr ->
                                      counterexample
                                        ( "pipeline progress source type-check failed\nexpr: "
                                            ++ show expr
                                            ++ "\nterm: "
                                            ++ show term
                                            ++ "\nerror: "
                                            ++ show tcErr
                                        )
                                        False
                                    Right _ ->
                                      counterexample
                                        ( "pipeline progress failed\nexpr: "
                                            ++ show expr
                                            ++ "\nterm: "
                                            ++ show term
                                        )
                                        (valueResult || canStep)

    it "Pipeline multi-step preservation proxy: typeCheck(term) = typeCheck(normalize term)" $
      property $
        withMaxSuccess 300 $
          forAll genClosedWellTypedExpr $ \expr ->
            let pipelineResult = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                pipelineSuccess = isRight pipelineResult
             in checkCoverage $
                  cover 20 pipelineSuccess "pipeline-success" $
                    case pipelineResult of
                      Left err ->
                        counterexample
                          ( "pipeline multi-step preservation setup failed\nexpr: "
                              ++ show expr
                              ++ "\nerror: "
                              ++ renderPipelineError err
                          )
                          False
                      Right (term, _pipelineTy) ->
                        let typeCheckResult = typeCheck term
                            typeCheckSuccess = isRight typeCheckResult
                            term' = normalize term
                            normalizedTypeCheckResult = typeCheck term'
                            normalizedTypeCheckSuccess = isRight normalizedTypeCheckResult
                         in cover 20 typeCheckSuccess "typecheck-success" $
                              cover 10 normalizedTypeCheckSuccess "normalized-typecheck-success" $
                                case typeCheckResult of
                                  Left tcErr ->
                                    counterexample
                                      ( "pipeline multi-step preservation source type-check failed\nexpr: "
                                          ++ show expr
                                          ++ "\nterm: "
                                          ++ show term
                                          ++ "\nerror: "
                                          ++ show tcErr
                                      )
                                      False
                                  Right ty ->
                                    case normalizedTypeCheckResult of
                                      Left tcErr ->
                                        counterexample
                                          ( "pipeline multi-step preservation normalized type-check failed\nexpr: "
                                              ++ show expr
                                              ++ "\nterm: "
                                              ++ show term
                                              ++ "\nnormalize(term): "
                                              ++ show term'
                                              ++ "\nerror: "
                                              ++ show tcErr
                                          )
                                          False
                                      Right ty' ->
                                        counterexample
                                          ( "pipeline multi-step preservation failed\nexpr: "
                                              ++ show expr
                                              ++ "\nterm: "
                                              ++ show term
                                              ++ "\nnormalize(term): "
                                              ++ show term'
                                              ++ "\ntype(term): "
                                              ++ show ty
                                              ++ "\ntype(normalize): "
                                              ++ show ty'
                                          )
                                          (ty' === ty)

    it "Pipeline one-step normalization proxy: representative elaborated terms preserve their normal form" $ do
      let cases =
            [ ( "annotated identity application",
                ELet
                  "f"
                  (EAnn (ELam "x" (EVar "x")) (mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))))
                  (EApp (EVar "f") (ELit (LInt 7)))
              ),
              ( "polymorphic let used at Int and Bool",
                ELet
                  "id"
                  (ELam "x" (EVar "x"))
                  ( ELet
                      "a"
                      (EApp (EVar "id") (ELit (LInt 1)))
                      (EApp (EVar "id") (ELit (LBool True)))
                  )
              )
            ]
      forM_ cases $ \(label, expr) ->
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (label ++ ": pipeline failed:\n" ++ renderPipelineError err)
          Right (term, ty) -> do
            typeCheck term `shouldBe` Right ty
            stepped <-
              case step term of
                Nothing ->
                  expectationFailure (label ++ ": expected elaborated term to reduce at least once")
                    >> fail "missing reduction step"
                Just term' -> pure term'
            normalize stepped `shouldBe` normalize term

  describe "Thesis obligations" $ do
    it "O08-REIFY-TYPE" $ do
      -- Graphic→syntactic: reifyType converts a solved constraint graph to a syntactic type
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) -> ty `shouldSatisfy` containsArrowTy

    it "O08-REIFY-NAMES" $ do
      -- Named reification: reifyType produces named type variables
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) ->
          ty `shouldSatisfy` \candidate ->
            case candidate of
              TForallRef ref Nothing (TArrow (TVarRef domRef) (TVarRef codRef)) ->
                let name = typeBinderRefName ref
                    dom = typeBinderRefName domRef
                    cod = typeBinderRefName codRef
                 in not (null name) && dom == name && cod == name
              _ -> False

    it "O08-BIND-MONO: alias bounds are inlined during normalization (B(σ))" $ do
      -- B(σ) from Fig 8.2.2: alias bounds (∀(a ⩾ b). body) are inlined
      -- by normalizeType, producing restricted types. The pipeline exercises
      -- this via unsafeNormalizeExpr which calls normalizeType internally.
      -- An annotated identity with alias bound: (λx.x) : ∀(a ⩾ Int). a -> a
      let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "a"))
          expr = ELet "f" (EAnn (ELam "x" (EVar "x")) (mkForalls [] ann)) (EVar "f")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) ->
          -- After alias inlining, the type should contain Int (the inlined bound)
          show ty `shouldSatisfy` ("Int" `isInfixOf`)

    it "O08-SYN-TO-GRAPH: annotation types are internalized as graphic constraints (G(σ))" $ do
      -- G(σ) from Fig 8.2.3: syntactic annotations are translated to graphic
      -- constraint nodes via internalizeCoercionCopy during constraint generation.
      -- A coercion annotation forces syntactic→graphic translation.
      let ann = STArrow (STBase "Int") (STBase "Int")
          expr = EAnn (ELam "x" (EVar "x")) (mkForalls [] ann)
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) ->
          -- The annotation constrains the type to Int -> Int
          ty `shouldSatisfy` containsArrowTy

    it "O08-REIFY-INLINE: bound inlining during reification (Sᵢ)" $ do
      -- Sᵢ from Fig 8.3.3: reification with bound inlining. A polymorphic
      -- identity λx.x has type ∀a. a -> a; the bound on a is ⊥ (flexible)
      -- and gets inlined away, leaving a clean arrow type in display form.
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) -> do
          -- The reified type should be a forall with an arrow body
          ty `shouldSatisfy` containsArrowTy
          ty `shouldSatisfy` containsForallTy

    it "O08-INLINE-PRED: Inline(τ,n) predicate distinguishes inlineable bounds" $ do
      -- Inline(τ,n) predicate: single covariant occurrence with ≥ bound → inline;
      -- multiple occurrences or = bound → keep. Test via a let-bound polymorphic
      -- function applied at two different types, producing a pair-like result.
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (term, ty) -> do
          ty `shouldBe` TestElab.tBase (BaseTy "Bool")
          typeCheck (normalize term) `shouldBe` Right ty
          normalize term `shouldBe` Elab.ELit (LBool True)

  -- See Note [Constraint simplification: Var-Abs (Ch 12.4.1)] in Translate.hs
  describe "Constraint simplification: Var-Abs (Ch 12.4.1)" $ do
    it "lambda parameters do not create gen nodes (on-the-fly Var-Abs)" $ do
      -- λx. λy. x y — two lambda params, zero let-bindings.
      -- Only the root gen node should exist; lambda params are bound
      -- monomorphically at the root scope, not under child gen nodes.
      let expr = ELam "x" (ELam "y" (EApp (EVar "x") (EVar "y")))
      case runConstraintDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right ConstraintResult {crConstraint = c0} -> do
          let genCount = length (toListGen (cGenNodes c0))
          -- Exactly 1 gen node: the root scope.
          genCount `shouldBe` 1

  -- See Note [Constraint simplification: Var-Let (Ch 12.4.1)] in Base.hs
  describe "Constraint simplification: Var-Let (Ch 12.4.1)" $ do
    it "trivial let edges are dropped from presolution expansions (on-the-fly Var-Let)" $ do
      -- let id = λx. x in id — one let-binding, one use site.
      -- The constraint should have let edges, but presolution should
      -- drop them from the expansion map (they are indirections).
      let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
      case runToPresolutionWithAnnDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right (pres, _ann) -> do
          let c1 = prConstraint pres
              letEdgeIds = cLetEdges c1
              expansionKeys = IntMap.keysSet (prEdgeExpansions pres)
          -- Let edges exist in the constraint
          IntSet.null letEdgeIds `shouldBe` False
          -- But none of them appear in the expansion map
          IntSet.intersection letEdgeIds expansionKeys
            `shouldBe` IntSet.empty

representativeMigrationCorpus :: [SurfaceExpr]
representativeMigrationCorpus =
  [ ELam "x" (EVar "x"),
    ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))),
    ELet
      "id"
      (ELam "x" (EVar "x"))
      ( ELet
          "a"
          (EApp (EVar "id") (ELit (LInt 1)))
          (EApp (EVar "id") (ELit (LBool True)))
      ),
    EAnn
      (ELam "x" (EVar "x"))
      (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
  ]

assertCanonicalPipelineTypeChecks :: SurfaceExpr -> Expectation
assertCanonicalPipelineTypeChecks expr =
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, ty) ->
      typeCheck term `shouldBe` Right ty

assertDiagnosticPipelineTypeChecks :: SurfaceExpr -> Expectation
assertDiagnosticPipelineTypeChecks expr =
  let config = Elab.defaultPipelineConfig {Elab.pcResultTypeDiagnostics = True}
   in case Elab.runPipelineElabWithConfig config Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure (Elab.renderPipelineError err)
        Right (term, ty) ->
          Elab.typeCheck term `shouldBe` Right ty

assertViewParity :: PresolutionViewBoundary.PresolutionView p -> Solved -> Expectation
assertViewParity view legacy = do
  let sharedLiveDomain =
        IntSet.intersection
          (liveNodeKeySet (pvConstraint view))
          (liveNodeKeySet (Solved.originalConstraint legacy))
  projectCanonicalMap sharedLiveDomain (pvCanonicalMap view)
    `shouldBe` projectCanonicalMap sharedLiveDomain (Solved.canonicalMap legacy)
  eraseConstraintPhaseForTest (pvCanonicalConstraint view) `shouldBe` Solved.canonicalConstraint legacy

  let thesisNodes =
        map
          fst
          (toListNode (cNodes (pvConstraint view)))
      legacyNodes =
        map
          fst
          (toListNode (cNodes (Solved.originalConstraint legacy)))
      probeIds = nub (thesisNodes ++ legacyNodes ++ [NodeId 999, NodeId 1000])
      probeRefs = map typeRef probeIds

  forM_ probeIds $ \nid -> do
    pvCanonical view nid `shouldBe` Solved.canonical legacy nid
    pvLookupNode view nid
      `shouldBe` NodeAccess.lookupNode (Solved.originalConstraint legacy) (Solved.canonical legacy nid)
    pvLookupVarBound view nid
      `shouldBe` NodeAccess.lookupVarBound (Solved.originalConstraint legacy) (Solved.canonical legacy nid)

  forM_ probeRefs $ \ref ->
    pvLookupBindParent view ref
      `shouldBe` NodeAccess.lookupBindParent (Solved.originalConstraint legacy) ref

projectCanonicalMap :: IntSet.IntSet -> IntMap.IntMap NodeId -> IntMap.IntMap NodeId
projectCanonicalMap domain =
  IntMap.filterWithKey keepInDomain
  where
    keepInDomain key rep =
      IntSet.member key domain
        && IntSet.member (nodeIdToKey rep) domain
        && rep /= NodeId key

liveNodeKeySet :: Constraint p -> IntSet.IntSet
liveNodeKeySet constraint =
  IntSet.fromList
    [ nodeIdToKey nid
      | (nid, _) <- toListNode (cNodes constraint)
    ]

firstGenAncestorForTest :: BindParents -> NodeRef -> Maybe GenNodeId
firstGenAncestorForTest bindParents start =
  case Binding.bindingPathToRootLocal bindParents start of
    Left _ -> Nothing
    Right path -> listToMaybe [gid | GenRef gid <- path]

nodeIdToKey :: NodeId -> Int
nodeIdToKey (NodeId k) = k

compilerExactTrace :: NodeId -> [(NodeId, NodeId)] -> EdgeTrace
compilerExactTrace root binderArgs =
  EdgeTrace
    { etRoot = root,
      etResultRoot = root,
      etBinderArgs = binderArgs,
      etInterior = sourceInteriorFromList (root : concatMap (\(binder, argument) -> [binder, argument]) binderArgs),
      etReplayContract = ReplayContractNone,
      etBinderReplayMap = IntMap.empty,
      etReplayDomainBinders = [],
      etCopyMap = mempty
    }

annNodeOccurrences :: AnnExpr -> [NodeId]
annNodeOccurrences expr = case expr of
  AResolvedVar _ _ nid -> [nid]
  ALit _ nid -> [nid]
  ALam _ _ pNode _ body _ nid -> pNode : nid : annNodeOccurrences body
  AApp fn arg _ _ nid -> nid : annNodeOccurrences fn ++ annNodeOccurrences arg
  ALet _ _ _ schemeRoot _ _ rhs body nid ->
    schemeRoot : nid : annNodeOccurrences rhs ++ annNodeOccurrences body
  AAnn inner nid _ -> nid : annNodeOccurrences inner
  ALetScope inner nid _ -> nid : annNodeOccurrences inner
  AUnfold inner nid _ -> nid : annNodeOccurrences inner

annLetSchemeRoots :: AnnExpr -> [NodeId]
annLetSchemeRoots expr = case expr of
  AResolvedVar _ _ _ -> []
  ALit _ _ -> []
  ALam _ _ _ _ body _ _ -> annLetSchemeRoots body
  AApp fn arg _ _ _ -> annLetSchemeRoots fn ++ annLetSchemeRoots arg
  ALet _ _ _ schemeRoot _ _ rhs body _ ->
    schemeRoot : annLetSchemeRoots rhs ++ annLetSchemeRoots body
  AAnn inner _ _ -> annLetSchemeRoots inner
  ALetScope inner _ _ -> annLetSchemeRoots inner
  AUnfold inner _ _ -> annLetSchemeRoots inner

annRootNode :: AnnExpr -> NodeId
annRootNode expr = case expr of
  AResolvedVar _ _ nid -> nid
  ALit _ nid -> nid
  ALam _ _ _ _ _ _ nid -> nid
  AApp _ _ _ _ nid -> nid
  ALet _ _ _ _ _ _ _ _ nid -> nid
  AAnn _ nid _ -> nid
  ALetScope _ nid _ -> nid
  AUnfold _ nid _ -> nid

validateStrict :: Solved -> Expectation
validateStrict s =
  case Solved.validateCanonicalGraphStrict s of
    [] -> pure ()
    vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

defaultPolySyms :: PolySyms
defaultPolySyms = Set.empty

noExpNodes :: NodeMap TyNode -> Expectation
noExpNodes nodes =
  case [nid | TyExp {tnId = nid} <- map snd (toListNode nodes)] of
    [] -> pure ()
    bad -> expectationFailure ("Unexpected TyExp nodes: " ++ show bad)

baseNames :: NodeMap TyNode -> [BaseTy]
baseNames nodes = [b | TestTyBase _ b <- map snd (toListNode nodes)]

genClosedWellTypedExpr :: Gen SurfaceExpr
genClosedWellTypedExpr = do
  n <- chooseInt (-5, 5)
  m <- chooseInt (-3, 9)
  b1 <- arbitrary
  let idLam = ELam "x" (EVar "x")
      intLit = ELit (LInt (fromIntegral n))
      intLit2 = ELit (LInt (fromIntegral m))
      boolLit = ELit (LBool b1)
      polyIdTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
      exprs =
        [ idLam,
          EApp idLam intLit,
          EApp idLam boolLit,
          ELet "id" idLam (EVar "id"),
          ELet "id" idLam (EApp (EVar "id") intLit),
          ELet "id" idLam (EApp (EVar "id") boolLit),
          ELet "id" idLam (ELet "_" (EApp (EVar "id") intLit) (EApp (EVar "id") boolLit)),
          ELet "id" idLam (EApp (EVar "id") (EVar "id")),
          ELam "y" (ELet "id" idLam (EApp (EVar "id") (EVar "y"))),
          ELamAnn "x" polyIdTy (EApp (EVar "x") intLit),
          ELet "f" (EAnn idLam polyIdTy) (EApp (EVar "f") intLit2)
        ]
  elements exprs
