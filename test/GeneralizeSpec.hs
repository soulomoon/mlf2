{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module GeneralizeSpec (spec) where

import qualified ElabTypeTestSupport as TestElab
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import ElabTermTestSupport
    ( generatedResolvedLocal
    , mkTestDeferredVar
    , mkTestLocalLam
    , testTForall
    , testTVar
    )
import MLF.Constraint.Presolution.Base
    ( EdgeTrace(..)
    )
import MLF.Constraint.Presolution.Plan.Requirements
    ( AmbientGammaAuthority(..)
    , GeneralizationRequirements(..)
    , RequiredGammaBinder(..)
    , RequiredGammaPlacement(..)
    , emptyExpansionConstructionPlacements
    , emptyGeneralizationRequirements
    )
import MLF.Constraint.Presolution.Plan.Finalize.TestSupport
    ( finalizeBinderPlanBinderRefs
    , mkFinalizeBinderPlan
    )
import MLF.Constraint.Presolution.TestSupport
    ( edgeArtifactsForTest
    , setEdgeArtifactTraceForTest
    , setEdgeArtifactWitnessForTest
    , sourceInteriorFromList
    )
import MLF.Constraint.Types.Graph
    ( BaseTy(..)
    , BindFlag(..)
    , EdgeId(..)
    , ExpVarId(..)
    , GraftResultConstruction(..)
    , GenNode(..)
    , GenNodeId(..)
    , InstEdge(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cEliminatedVars
    , cGenNodes
    , cGraftResultConstructions
    , cInstEdges
    , cLetEdges
    , cNodes
    , fromListGen
    , getNodeId
    , nodeRefKey
    )
import MLF.Constraint.Types.Witness
    ( Expansion(ExpIdentity)
    , InstanceOp(..)
    , ReplayContract(..)
    )
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness(..), InstanceWitness(..))
import MLF.Elab.Generalize
    ( CompilerExactResultStage(..)
    , GaBindParents(..)
    , GammaPacketAuthority(..)
    , GeneralizedResultRoute(..)
    , GeneralizedResultRouteRequest(..)
    , IdentityTopologyConsumerAuthority
    , LocalGammaConstructor(..)
    , LocalGammaFrame
    , LocalGammaOwner(..)
    , PreparedSubtermGeneralization
    , SubtermPacketPlacement(..)
    , scaConsumerIdentity
    , scaEdgeId
    , inlineRigidTypes
    , lgfChildren
    , lgfDirectEdgeSources
    , lgfOwner
    , localGammaFrame
    , freshenSchemeInfoBinderNamesAgainst
    , gaConstructionRouteNodes
    , mergeSubtermGeneralizations
    , mkIdentityTopologyConsumerAuthority
    , packetTypeSpecializesToExactEndpoint
    , pairSubtermGeneralizationRoots
    , placeSubtermGeneralizationBinders
    , placeSubtermGeneralizationBindersWithRoutes
    , prepareRootRaiseMergeScheme
    , prepareRootRaiseMergeSchemeAtEdge
    , prepareSubtermGeneralizationPacket
    , resolveAmbientGammaOperatedEndpoint
    , resolveFrozenOperatedOccurrenceEndpoint
    , rootRaiseMergeAuthorityFor
    , publishSubtermGammaConstructionSourceSchemeInfo
    , selectConstructionRequirementEndpoint
    , selectSolvedOrderWithShadow
    , shadowCompareTypes
    , subtermGeneralizationConsumerConstructionSchemeInfo
    , subtermGeneralizationConsumerAuthority
    , subtermGeneralizationGammaAuthority
    , subtermGeneralizationOperatedSchemeInfo
    , subtermGeneralizationResultAbstractionRef
    , subtermGeneralizationConstructionResultAbstractionRef
    , subtermGeneralizationGammaBoundScheme
    , withConstructionBinderRenames
    , subtermGeneralizationGammaBoundSchemeForConsumer
    , subtermGeneralizationSchemeInfo
    , subtermConsumerAuthorityEnclosingOwner
    , subtermResultOwnershipFor
    , subtermResultOwnershipLocalSourceDeclarationRefs
    , withCompilerExactBinderRenames
    , withCompilerExactPacketSubtermResult
    , withCompilerExactSourceSubtermResult
    )
import MLF.Elab.Elaborate.Algebra
    ( CompilerExactResultBoundCertificate(..)
    , completeCompilerExactSubtermResults
    , completeCompilerExactSubtermResultsWithBounds
    )
import MLF.Elab.Elaborate.Annotation.TestSupport
    ( checkedArgumentClosedTopologyForTest
    , checkedOccurrenceSchemeInfoForTest
    , scopedAnnotationConstructionBinderRenamesForTest
    , strictReplayCheckedSchemeInfoForTest
    )
import qualified MLF.Elab.Elaborate.Algebra.TestSupport as AlgebraTestSupport
import MLF.Elab.Run.TypeOps (simplifyAnnotationType)
import MLF.Elab.Run.Generalize.Phase1.TestSupport
    ( Phase1ResultTestView(..)
    , restoreSchemeNodesForTest
    )
import MLF.Elab.Run.Generalize.Prepare.TestSupport
    ( identityTopologyAncestryFailuresForTest
    , publishSourceLambdaTopologyConsumerRouteForTest
    , publishTopologyConsumerRoutesForTest
    , resolvedSourceApplicationArgumentEndpointForTest
    , sourceLambdaGeneralizedResultRouteRequestForTest
    )
import MLF.Elab.Run.ResultType
    ( inferInstAppArgsFromSchemeRefs
    , inferInstAppArgsFromSchemeRefsExact
    , residualTopologyAgreesExact
    , substTypeSelectiveRefs
    )
import MLF.Elab.Pipeline (ElabError(..), applyInstantiation, schemeToType)
import MLF.Elab.Types
    ( inheritedGammaSchemeClosureAuthority
    , locallyClosedGammaSchemeClosureAuthority
    , schemeClosureFreeRefs
    )
import MLF.Elab.SourceBinder.TestSupport
    ( orderSourceProjectedSchemeBindersForTest
    , resolveConstructionSourceBindersInSchemeInfoExceptForTest
    , resolveConstructionSourceBindersInSchemeInfoForTest
    , resolveConstructionSourceBindersInPacketAtExpectedForTest
    , resolveConstructionSourceBindersInTypeAtExpectedForTest
    , resolveConstructionSourceBindersInTypeExceptForTest
    , resolveConstructionSourceBindersInTypeForTest
    , sourceBinderConstructionRenamesForTest
    )
import MLF.Frontend.ConstraintGen
    ( AnnExpr(..)
    , InstantiationSite(..)
    , InstantiationTargetTopology(..)
    , mkInstantiationSite
    )
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace(..), symbolIdentityFromParts)
import MLF.Frontend.Syntax (Lit(..))
import MLF.Reify.TypeOps (alphaEqType)
import MLF.Types.Elab
    ( BoundType
    , ElabScheme
    , ElabType
    , Instantiation(..)
    , XmlfTerm(..)
    , SchemeInfo(..)
    , Ty(..)
    , TypeBinderRef
    , elabToBound
    , mkElabSchemeWithRefs
    , schemeBinderRefs
    , schemeBody
    , schemeInfoBinderRefSubst
    , schemeInfoFromRefSubst
    , tForallWithRef
    , tMuWithRef
    , tVarAppWithRef
    , tVarWithRef
    , typeBinderIdentityFromNode
    , typeBinderIdentityFromUnique
    , typeBinderRefFromIdentity
    , typeBinderRefIdentity
    , typeBinderRefName
    , typeBinderRefNode
    , typeBinderRefsSameIdentity
    , tyToElab
    )
import MLF.Types.Identity
    ( IdDetails(..)
    , IdentityGenerator
    , LocalIdentity(..)
    , ResolvedTermIdentityKey(..)
    , StructuralTypeBinderRole(..)
    , UniqueIdentity(..)
    , initialIdentityGenerator
    , localRefFromIdentity
    , typeBinderIdentityFromStructural
    )
import SpecUtil (emptyConstraint, nodeMapFromList)

typeRef :: Int -> String -> TypeBinderRef
typeRef key name =
    typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

typeIdentity :: Int -> SymbolIdentity
typeIdentity unique =
    symbolIdentityFromParts (UniqueIdentity unique) SymbolType "Main" "Token" Nothing

ownerDetails :: Int -> String -> IdDetails
ownerDetails unique name =
    LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity unique)) name)

ownerKey :: Int -> String -> ResolvedTermIdentityKey
ownerKey unique name =
    ResolvedTermLocalKey
        (localRefFromIdentity (GeneratedLocalId (UniqueIdentity unique)) name)

testEnclosingConsumerOwner :: LocalGammaOwner
testEnclosingConsumerOwner =
    LocalGammaOwner
        { lgoConstructor = LocalLambdaGamma
        , lgoBoundaryEdge = EdgeId 9000
        , lgoTermNode = NodeId 9000
        , lgoScope = GenRef (GenNodeId 0)
        }

testTopologyConsumerAuthority
    :: EdgeId
    -> NodeId
    -> NodeId
    -> Either ElabError IdentityTopologyConsumerAuthority
testTopologyConsumerAuthority edgeId sourceBodyRoot frozenResultRoot =
    mkIdentityTopologyConsumerAuthority
        ( IntMap.singleton
            (getNodeId sourceBodyRoot)
            sourceBodyRoot
        )
        edgeId
        (GenNodeId 0)
        sourceBodyRoot
        (GenNodeId 0)
        sourceBodyRoot
        frozenResultRoot
        testEnclosingConsumerOwner
            { lgoBoundaryEdge = edgeId
            }

requireTestTopologyConsumerAuthority
    :: EdgeId
    -> NodeId
    -> NodeId
    -> IO IdentityTopologyConsumerAuthority
requireTestTopologyConsumerAuthority edgeId sourceBodyRoot frozenResultRoot =
    case
        testTopologyConsumerAuthority
            edgeId
            sourceBodyRoot
            frozenResultRoot
    of
        Left err ->
            expectationFailure (show err)
                >> fail "test topology authority construction failed"
        Right authority -> pure authority

requireTopologyConsumerAuthority
    :: EdgeId
    -> GenNodeId
    -> NodeId
    -> GenNodeId
    -> NodeId
    -> NodeId
    -> LocalGammaOwner
    -> IO IdentityTopologyConsumerAuthority
requireTopologyConsumerAuthority
    edgeId
    sourceScopeRoot
    sourceBodyRoot
    boundaryScopeRoot
    boundaryBodyRoot
    frozenResultRoot
    owner =
    case
        mkIdentityTopologyConsumerAuthority
            ( IntMap.singleton
                (getNodeId sourceBodyRoot)
                boundaryBodyRoot
            )
            edgeId
            sourceScopeRoot
            sourceBodyRoot
            boundaryScopeRoot
            boundaryBodyRoot
            frozenResultRoot
            owner
    of
        Left err ->
            expectationFailure (show err)
                >> fail "topology authority construction failed"
        Right authority -> pure authority

prepareTopologyPacketForTest
    :: IdentityTopologyConsumerAuthority
    -> SchemeInfo
    -> IO PreparedSubtermGeneralization
prepareTopologyPacketForTest topologyAuthority topologyInfo =
    case
        prepareSubtermGeneralizationPacket
            initialIdentityGenerator
            (TopologyConsumerPacket topologyAuthority)
            topologyInfo
            topologyInfo
    of
        Left err ->
            expectationFailure (show err)
                >> fail "topology packet preparation failed"
        Right (packet, _) -> pure packet

preparePacket
    :: IdentityGenerator
    -> EdgeId
    -> TypeBinderRef
    -> SchemeInfo
    -> IO (PreparedSubtermGeneralization, IdentityGenerator)
preparePacket generator consumerEdge consumerRef packet =
    preparePacketForOwner
        generator
        testEnclosingConsumerOwner
        consumerEdge
        consumerRef
        packet

preparePacketForOwner
    :: IdentityGenerator
    -> LocalGammaOwner
    -> EdgeId
    -> TypeBinderRef
    -> SchemeInfo
    -> IO (PreparedSubtermGeneralization, IdentityGenerator)
preparePacketForOwner generator owner consumerEdge consumerRef packet =
    case
        prepareSubtermGeneralizationPacket
            generator
            ( EnclosingConsumerPacket
                (typeBinderRefIdentity consumerRef)
                consumerEdge
                owner
            )
            packet
            packet
    of
        Left err -> expectationFailure (show err) >> fail "packet preparation failed"
        Right prepared -> pure prepared

requireSingleCopiedBinder :: ElabScheme -> IO TypeBinderRef
requireSingleCopiedBinder scheme =
    case schemeBinderRefs scheme of
        [(_, Just (TForallRef copiedRef _ _))] -> pure copiedRef
        other ->
            expectationFailure ("expected one placed copied binder, got " ++ show other)
                >> fail "copied binder extraction failed"

requireLocalGammaFrame :: Either ElabError LocalGammaFrame -> IO LocalGammaFrame
requireLocalGammaFrame result =
    case result of
        Left err ->
            expectationFailure ("local Gamma frame construction failed: " ++ show err)
                >> fail "local Gamma frame construction failed"
        Right frame -> pure frame

isLeftWithExactEndpointMismatch :: Either String ElabType -> Bool
isLeftWithExactEndpointMismatch result =
    case result of
        Left cause -> "does not equal its exact source endpoint" `isInfixOf` cause
        Right _ -> False

spec :: Spec
spec = do
    describe "direct ambient Gamma endpoint authority" $ do
        let liveRef = typeRef 17 "live"
            exactRef = typeRef 1 "ambient"
            exactBound = TestElab.tBase (BaseTy "Int")
            authorities =
                IntMap.singleton
                    17
                    AmbientGammaAuthority
                        { agaExactRef = exactRef
                        , agaBound = exactBound
                        }

        it "adopts the exact checked endpoint for the certified live node" $ do
            resolveAmbientGammaOperatedEndpoint
                authorities
                exactBound
                (tVarWithRef liveRef)
                `shouldBe` Just exactBound

        it "does not derive authority through another node key" $ do
            resolveAmbientGammaOperatedEndpoint
                authorities
                exactBound
                (tVarWithRef (typeRef 18 "same-lane"))
                `shouldBe` Nothing

        it "does not adopt an endpoint whose bound disagrees with the certificate" $ do
            resolveAmbientGammaOperatedEndpoint
                authorities
                (TestElab.tBase (BaseTy "Bool"))
                (tVarWithRef liveRef)
                `shouldBe` Nothing

        it "does not reinterpret a structured operated packet as a live alias" $ do
            resolveAmbientGammaOperatedEndpoint
                authorities
                exactBound
                (TArrow (tVarWithRef liveRef) exactBound)
                `shouldBe` Nothing

    describe "frozen operated occurrence endpoint authority" $ do
        let operatedRoot = NodeId 17
            otherRoot = NodeId 18
            exactEndpoint = TestElab.tBase (BaseTy "Int")
            aliasedAuthority =
                IntMap.singleton
                    17
                    AmbientGammaAuthority
                        { agaExactRef = typeRef 18 "exact-occurrence"
                        , agaBound = TBottom
                        }
            variableConstraint =
                emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ ( 17
                              , TyVar
                                  { tnId = operatedRoot
                                  , tnBound = Nothing
                                  }
                              )
                            , ( 18
                              , TyVar
                                  { tnId = otherRoot
                                  , tnBound = Nothing
                                  }
                              )
                            ]
                    }

        it "adopts the checked endpoint for the exact frozen TyVar root" $ do
            resolveFrozenOperatedOccurrenceEndpoint
                variableConstraint
                IntMap.empty
                []
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 17 "operated"))
                `shouldBe` Just exactEndpoint

        it "adopts through the exact frozen-root declaration route" $ do
            resolveFrozenOperatedOccurrenceEndpoint
                variableConstraint
                aliasedAuthority
                []
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 18 "packet"))
                `shouldBe` Just exactEndpoint

        it "adopts through an exact construction copy of the frozen root" $ do
            resolveFrozenOperatedOccurrenceEndpoint
                variableConstraint
                IntMap.empty
                [otherRoot]
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 18 "construction-copy"))
                `shouldBe` Just exactEndpoint

        it "does not adopt through a different graph ref even if a caller might quotient it" $ do
            resolveFrozenOperatedOccurrenceEndpoint
                variableConstraint
                IntMap.empty
                []
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 18 "representative-only"))
                `shouldBe` Nothing

        it "does not use an authority stored under another frozen node" $ do
            resolveFrozenOperatedOccurrenceEndpoint
                variableConstraint
                (IntMap.singleton 18 (aliasedAuthority IntMap.! 17))
                []
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 18 "packet"))
                `shouldBe` Nothing

        it "does not use a frozen-root declaration for another exact identity" $ do
            let wrongAuthority =
                    IntMap.adjust
                        (\authority -> authority {agaExactRef = typeRef 19 "other"})
                        17
                        aliasedAuthority
            resolveFrozenOperatedOccurrenceEndpoint
                variableConstraint
                wrongAuthority
                []
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 18 "packet"))
                `shouldBe` Nothing

        it "does not treat a structural frozen root as an occurrence certificate" $ do
            let structuralConstraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ ( 17
                                  , TyArrow
                                      operatedRoot
                                      otherRoot
                                      otherRoot
                                  )
                                ]
                        }
            resolveFrozenOperatedOccurrenceEndpoint
                structuralConstraint
                aliasedAuthority
                [operatedRoot]
                operatedRoot
                exactEndpoint
                (tVarWithRef (typeRef 17 "operated"))
                `shouldBe` Nothing

    describe "identity-topology consumer ancestry" $ do
        let lambdaScope = GenNodeId 20
            nestedScope = GenNodeId 10
            bodyNode = NodeId 1
            resultNode = NodeId 2
            binding child parent flag =
                (nodeRefKey child, (parent, flag))
            validParents =
                IntMap.fromList
                    [ binding (TypeRef bodyNode) (GenRef nestedScope) BindFlex
                    , binding (GenRef nestedScope) (GenRef lambdaScope) BindFlex
                    , binding (TypeRef resultNode) (GenRef lambdaScope) BindFlex
                    ]
            failuresFor parents =
                identityTopologyAncestryFailuresForTest
                    parents
                    lambdaScope
                    bodyNode
                    resultNode

        it "accepts one distinct all-flex descendant scope" $ do
            failuresFor validParents `shouldBe` []

        it "rejects a body that is a direct sibling of the result" $ do
            let directParents =
                    IntMap.fromList
                        [ binding (TypeRef bodyNode) (GenRef lambdaScope) BindFlex
                        , binding (TypeRef resultNode) (GenRef lambdaScope) BindFlex
                        ]
            failuresFor directParents
                `shouldSatisfy` any (isInfixOf "no nested generalization scope")

        it "rejects a rigid hop in the descendant ancestry" $ do
            let rigidParents =
                    IntMap.insert
                        (nodeRefKey (GenRef nestedScope))
                        (GenRef lambdaScope, BindRigid)
                        validParents
            failuresFor rigidParents
                `shouldSatisfy` any (isInfixOf "not entirely flexible")

        it "rejects a cyclic descendant ancestry" $ do
            let otherNestedScope = GenNodeId 11
                cyclicParents =
                    IntMap.fromList
                        [ binding (TypeRef bodyNode) (GenRef nestedScope) BindFlex
                        , binding (GenRef nestedScope) (GenRef otherNestedScope) BindFlex
                        , binding (GenRef otherNestedScope) (GenRef nestedScope) BindFlex
                        , binding (TypeRef resultNode) (GenRef lambdaScope) BindFlex
                        ]
            failuresFor cyclicParents
                `shouldSatisfy` any (isInfixOf "ancestry is invalid")

    describe "Phase1 eliminated scheme-root restoration" $ do
        let owner = GenNodeId 44
            letEdge = EdgeId 73
            body = NodeId 431
            root = NodeId 430
            target = NodeId 427
            domain = NodeId 416
            codomain = NodeId 426
            base =
                emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [(getNodeId root, TyVar { tnId = root, tnBound = Nothing })]
                    , cBindParents =
                        IntMap.singleton
                            (nodeRefKey (TypeRef root))
                            (GenRef owner, BindFlex)
                    , cGenNodes = fromListGen [(owner, GenNode owner [root])]
                    , cInstEdges = [InstEdge letEdge body root]
                    , cLetEdges = IntSet.singleton (getEdgeId letEdge)
                    }
            solvedWithTarget =
                emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ ( getNodeId target
                              , TyArrow
                                    { tnId = target
                                    , tnDom = domain
                                    , tnCod = codomain
                                    }
                              )
                            ]
                    , cEliminatedVars = IntSet.singleton (getNodeId root)
                    }

        it "restores an eliminated unbounded root from its distinct live redirect" $ do
            let restored =
                    restoreSchemeNodesForTest
                        base
                        solvedWithTarget
                        (IntMap.singleton (getNodeId root) target)
            IntMap.lookup (getNodeId root) (phase1TestRestoredNodes restored)
                `shouldBe` Just (TyVar { tnId = root, tnBound = Just target })
            phase1TestRestoredBaseSchemeRoots restored `shouldBe` [root]
            phase1TestRestoredSchemeRootTargets restored
                `shouldBe` IntMap.singleton (getNodeId root) target

        it "does not invent a bound without both a distinct redirect and a live target" $ do
            let withoutRedirect =
                    restoreSchemeNodesForTest base solvedWithTarget IntMap.empty
                withoutLiveTarget =
                    restoreSchemeNodesForTest
                        base
                        ( solvedWithTarget
                            { cNodes = nodeMapFromList []
                            }
                        )
                        (IntMap.singleton (getNodeId root) target)
                withoutLetEdgeAuthority =
                    restoreSchemeNodesForTest
                        (base { cLetEdges = IntSet.empty })
                        solvedWithTarget
                        (IntMap.singleton (getNodeId root) target)
                restoredBounds restored =
                    [ bound
                    | TyVar { tnBound = Just bound } <-
                        IntMap.elems (phase1TestRestoredNodes restored)
                    ]
            IntMap.lookup (getNodeId root) (phase1TestRestoredNodes withoutRedirect)
                `shouldBe` Just (TyVar { tnId = root, tnBound = Nothing })
            restoredBounds withoutRedirect `shouldBe` []
            restoredBounds withoutLiveTarget `shouldBe` []
            restoredBounds withoutLetEdgeAuthority `shouldBe` []
            phase1TestRestoredSchemeRootTargets withoutRedirect `shouldBe` IntMap.empty
            phase1TestRestoredSchemeRootTargets withoutLiveTarget `shouldBe` IntMap.empty
            phase1TestRestoredSchemeRootTargets withoutLetEdgeAuthority `shouldBe` IntMap.empty

    describe "scheme publication authority" $ do
        it "accepts a planner-owned binder with inherited and locally closed Gamma identities" $ do
            let plannedRef = typeRef 690 "planned"
                inheritedRef = typeRef 691 "inherited"
                locallyClosedRef = typeRef 692 "locally-closed"
                scheme =
                    mkElabSchemeWithRefs
                        [(plannedRef, Nothing)]
                        ( TArrow
                            (TVarRef plannedRef)
                            (TArrow (TVarRef inheritedRef) (TVarRef locallyClosedRef))
                        )
                authority =
                    inheritedGammaSchemeClosureAuthority [inheritedRef]
                        <> locallyClosedGammaSchemeClosureAuthority [locallyClosedRef]
            case
                mkFinalizeBinderPlan
                    [(690, plannedRef)]
                    [(plannedRef, Nothing)]
              of
                Left err ->
                    expectationFailure
                        ("expected valid finalize binder capability, got " ++ show err)
                Right binderPlan ->
                    finalizeBinderPlanBinderRefs binderPlan
                        `shouldBe` [(690, plannedRef)]
            schemeClosureFreeRefs authority scheme `shouldBe` []

        it "rejects a same-spelled reified binder with a distinct identity" $ do
            let plannedRef = typeRef 693 "a"
                unrelatedRef = typeRef 694 "a"
            case
                mkFinalizeBinderPlan
                    [(693, plannedRef)]
                    [(unrelatedRef, Nothing)]
              of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "reified binding order")
                Left err ->
                    expectationFailure
                        ("expected binder identity validation, got " ++ show err)
                Right _ ->
                    expectationFailure "expected distinct binder identity rejection"

        it "rejects reified binder bounds presented in a different planner order" $ do
            let firstRef = typeRef 695 "first"
                secondRef = typeRef 696 "second"
            case
                mkFinalizeBinderPlan
                    [(695, firstRef), (696, secondRef)]
                    [(secondRef, Nothing), (firstRef, Nothing)]
              of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "reified binding order")
                Left err ->
                    expectationFailure
                        ("expected binder-order validation, got " ++ show err)
                Right _ ->
                    expectationFailure "expected binder-order mismatch rejection"

    describe "construction source routes" $ do
        it "closes an opened argument topology from its exact checked source declarations" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 718))
                        "a"
                openedBody =
                    TArrow
                        (TVarRef sourceRef)
                        (TVarRef sourceRef)
                closedType =
                    TForallRef sourceRef Nothing openedBody
                sourceSchemeInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sourceRef, Nothing)] openedBody)
                        IntMap.empty
            checkedArgumentClosedTopologyForTest
                (Just sourceSchemeInfo)
                closedType
                openedBody
                `shouldBe` Just closedType

        it "does not close an opened topology with an unrelated free identity" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 718))
                        "a"
                unrelatedRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 719))
                        "a"
                sourceBody =
                    TArrow
                        (TVarRef sourceRef)
                        (TVarRef sourceRef)
                closedType =
                    TForallRef sourceRef Nothing sourceBody
                sourceSchemeInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sourceRef, Nothing)] sourceBody)
                        IntMap.empty
                unrelatedTopology =
                    TArrow
                        (TVarRef unrelatedRef)
                        (TVarRef unrelatedRef)
            checkedArgumentClosedTopologyForTest
                (Just sourceSchemeInfo)
                closedType
                unrelatedTopology
                `shouldBe` Nothing

        it "does not close a vacuous source forall without opened-binder identity evidence" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 720))
                        "ghost"
                body = TestElab.tBase (BaseTy "Unit")
                closedType =
                    TForallRef sourceRef Nothing body
                sourceSchemeInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sourceRef, Nothing)] body)
                        IntMap.empty
            checkedArgumentClosedTopologyForTest
                (Just sourceSchemeInfo)
                closedType
                body
                `shouldBe` Nothing

        it "uses the exact checked type as occurrence source authority without recovered metadata" $ do
            let aRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 719))
                        "a"
                checkedType =
                    TForallRef
                        aRef
                        Nothing
                        (TArrow (TVarRef aRef) (TVarRef aRef))
            checkedInfo <-
                case checkedOccurrenceSchemeInfoForTest checkedType Nothing of
                    Left err ->
                        expectationFailure err >> fail "checked source construction failed"
                    Right projectionResult -> pure projectionResult
            schemeToType (siScheme checkedInfo) `shouldBe` checkedType
            schemeInfoBinderRefSubst checkedInfo `shouldBe` IntMap.empty

        it "keys checked semantic binders only at their strict replay targets" $ do
            let sourceA = NodeId 720
                sourceB = NodeId 721
                replayA = NodeId 730
                replayB = NodeId 731
                unrelatedNode = NodeId 740
                argumentA = NodeId 750
                argumentB = NodeId 751
                aRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 720))
                        "a"
                bRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 721))
                        "b"
                unrelatedRef = typeRef 740 "unrelated"
                checkedScheme =
                    mkElabSchemeWithRefs
                        [(aRef, Nothing), (bRef, Nothing)]
                        (TArrow (TVarRef aRef) (TVarRef bRef))
                checkedInfo =
                    schemeInfoFromRefSubst
                        checkedScheme
                        ( IntMap.fromList
                            [ (getNodeId sourceA, aRef)
                            , (getNodeId sourceB, bRef)
                            , (getNodeId unrelatedNode, unrelatedRef)
                            ]
                        )
                traceInfo =
                    EdgeTrace
                        { etRoot = NodeId 700
                        , etResultRoot = NodeId 701
                        , etBinderArgs =
                            [(sourceA, argumentA), (sourceB, argumentB)]
                        , etInterior =
                            sourceInteriorFromList
                                [sourceA, sourceB, argumentA, argumentB]
                        , etReplayContract = ReplayContractStrict
                        , etBinderReplayMap =
                            IntMap.fromList
                                [ (getNodeId sourceA, replayA)
                                , (getNodeId sourceB, replayB)
                                ]
                        , etReplayDomainBinders =
                            [replayA, replayB, NodeId 732]
                        , etCopyMap = mempty
                        }
            aligned <-
                case
                    strictReplayCheckedSchemeInfoForTest
                        ( IntMap.fromList
                            [ (getNodeId sourceA, aRef)
                            , (getNodeId sourceB, bRef)
                            ]
                        )
                        traceInfo
                        checkedInfo
                  of
                    Left err ->
                        expectationFailure err >> fail "strict replay alignment failed"
                    Right projectionResult -> pure projectionResult
            siScheme aligned `shouldBe` checkedScheme
            schemeInfoBinderRefSubst aligned
                `shouldBe`
                    IntMap.fromList
                        [ (getNodeId replayA, aRef)
                        , (getNodeId replayB, bRef)
                        , (getNodeId unrelatedNode, unrelatedRef)
                        ]

        it "routes every exact trace occurrence of one checked semantic binder" $ do
            let sourceA = NodeId 760
                sourceAliasA = NodeId 761
                replayA = NodeId 770
                replayAliasA = NodeId 771
                aRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 760))
                        "a"
                checkedInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(aRef, Nothing)] (TVarRef aRef))
                        IntMap.empty
                traceInfo =
                    EdgeTrace
                        { etRoot = NodeId 700
                        , etResultRoot = NodeId 701
                        , etBinderArgs =
                            [(sourceA, NodeId 780), (sourceAliasA, NodeId 781)]
                        , etInterior =
                            sourceInteriorFromList
                                [sourceA, sourceAliasA]
                        , etReplayContract = ReplayContractStrict
                        , etBinderReplayMap =
                            IntMap.fromList
                                [ (getNodeId sourceA, replayA)
                                , (getNodeId sourceAliasA, replayAliasA)
                                ]
                        , etReplayDomainBinders = [replayA, replayAliasA]
                        , etCopyMap = mempty
                        }
            aligned <-
                case
                    strictReplayCheckedSchemeInfoForTest
                        ( IntMap.fromList
                            [ (getNodeId sourceA, aRef)
                            , (getNodeId sourceAliasA, aRef)
                            ]
                        )
                        traceInfo
                        checkedInfo
                  of
                    Left err ->
                        expectationFailure err >> fail "strict alias-cluster replay alignment failed"
                    Right result -> pure result
            siScheme aligned `shouldBe` siScheme checkedInfo
            schemeInfoBinderRefSubst aligned
                `shouldBe`
                    IntMap.fromList
                        [ (getNodeId replayA, aRef)
                        , (getNodeId replayAliasA, aRef)
                        ]

        it "routes a resolved source binder to its local construction Gamma by shared graph node" $ do
            let sharedNode = NodeId 714
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 715))
                        "source"
                localGammaRef = typeRef 714 "local-gamma"
            scopedAnnotationConstructionBinderRenamesForTest
                id
                (IntMap.singleton (getNodeId sharedNode) sourceRef)
                (IntMap.singleton (getNodeId sharedNode) localGammaRef)
                (TVarRef sourceRef)
                `shouldBe` Right [(sourceRef, localGammaRef)]

        it "does not route a same-spelled but distinct annotation source identity" $ do
            let sharedNode = NodeId 716
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 717))
                        "a"
                unrelatedRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 718))
                        "a"
                localGammaRef = typeRef 716 "a"
            scopedAnnotationConstructionBinderRenamesForTest
                id
                (IntMap.singleton (getNodeId sharedNode) sourceRef)
                (IntMap.singleton (getNodeId sharedNode) localGammaRef)
                (TVarRef unrelatedRef)
                `shouldBe` Right []

        it "keeps an exact source identity when its graph representative has lexical peers" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 700))
                        "source"
                peerRef = typeRef 702 "peer"
                representative _ = NodeId 700
                sourceRefs = IntMap.singleton 701 sourceRef
                constructionAliases =
                    IntMap.fromList
                        [ (700, sourceRef)
                        , (702, peerRef)
                        ]
            sourceBinderConstructionRenamesForTest
                representative
                sourceRefs
                constructionAliases
                `shouldBe` Right []

        it "rejects a representative with two distinct non-source construction binders" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 710))
                        "source"
                firstPeer = typeRef 712 "first-peer"
                secondPeer = typeRef 713 "second-peer"
                representative _ = NodeId 710
                sourceRefs = IntMap.singleton 711 sourceRef
                constructionAliases =
                    IntMap.fromList
                        [ (712, firstPeer)
                        , (713, secondPeer)
                        ]
            case
                sourceBinderConstructionRenamesForTest
                    representative
                    sourceRefs
                    constructionAliases
              of
                Left cause ->
                    cause
                        `shouldSatisfy` isInfixOf "multiple outward binders"
                Right routes ->
                    expectationFailure
                        ("expected ambiguous construction routes, got " ++ show routes)

    describe "local Gamma source frames" $ do
        it "uses the application function edge as owner while retaining both direct edge sources" $ do
            let funEdge = EdgeId 31
                argEdge = EdgeId 32
                applicationNode = NodeId 35
                applicationScope = GenRef (GenNodeId 30)
                fun = ALit (LInt 1) (NodeId 33)
                arg = ALit (LInt 2) (NodeId 34)
                application =
                    AApp
                        fun
                        arg
                        (mkInstantiationSite funEdge (NodeId 33) applicationNode)
                        (mkInstantiationSite argEdge (NodeId 34) applicationNode)
                        applicationNode
                scopeForBoundary edgeId nodeId
                    | edgeId == funEdge
                    , nodeId == applicationNode = Right applicationScope
                    | otherwise =
                        Left
                            ( ValidationFailed
                                [ "unexpected local Gamma scope lookup"
                                , show (edgeId, nodeId)
                                ]
                            )
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalApplicationGamma
                        , lgoBoundaryEdge = funEdge
                        , lgoTermNode = applicationNode
                        , lgoScope = applicationScope
                        }
            frame <- requireLocalGammaFrame (localGammaFrame scopeForBoundary application)
            lgfOwner frame `shouldBe` Just owner
            lgfDirectEdgeSources frame `shouldBe` [(funEdge, fun), (argEdge, arg)]
            lgfChildren frame `shouldBe` [fun, arg]

        it "takes lambda ownership directly from the annotation without a scope lookup" $ do
            let bodyEdge = EdgeId 41
                lambdaScope = GenNodeId 42
                lambdaNode = NodeId 43
                body = ALit (LInt 1) (NodeId 44)
                lambda =
                    ALam
                        "x"
                        (ownerDetails 41 "x")
                        (NodeId 45)
                        lambdaScope
                        body
                        bodyEdge
                        lambdaNode
                rejectScopeLookup _ _ =
                    Left (ValidationFailed ["lambda unexpectedly queried boundary scope"])
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLambdaGamma
                        , lgoBoundaryEdge = bodyEdge
                        , lgoTermNode = lambdaNode
                        , lgoScope = GenRef lambdaScope
                        }
            frame <- requireLocalGammaFrame (localGammaFrame rejectScopeLookup lambda)
            lgfOwner frame `shouldBe` Just owner
            lgfDirectEdgeSources frame `shouldBe` [(bodyEdge, body)]
            lgfChildren frame `shouldBe` [body]

        it "fails closed when a let body has no direct constraint-only boundary" $ do
            let rhs = ALit (LInt 1) (NodeId 51)
                body = ALit (LInt 2) (NodeId 52)
                malformedLet =
                    ALet
                        "x"
                        (ownerDetails 51 "x")
                        (GenNodeId 53)
                        (NodeId 54)
                        (ExpVarId 55)
                        (GenNodeId 56)
                        rhs
                        body
                        (NodeId 57)
            case localGammaFrame (\_ _ -> Right (GenRef (GenNodeId 58))) malformedLet of
                Left (ValidationFailed messages) ->
                    unlines messages
                        `shouldSatisfy` isInfixOf "missing its constraint-only scope edge"
                result ->
                    expectationFailure
                        ("expected missing let-boundary rejection, got " ++ show result)

        it "derives let ownership from the direct constraint-only boundary" $ do
            let letEdge = EdgeId 61
                resultNode = NodeId 62
                letScope = GenRef (GenNodeId 63)
                rhs = ALit (LInt 1) (NodeId 64)
                innerBody = ALit (LInt 2) (NodeId 65)
                body = ALetScope innerBody resultNode letEdge
                letAnn =
                    ALet
                        "x"
                        (ownerDetails 61 "x")
                        (GenNodeId 66)
                        (NodeId 67)
                        (ExpVarId 68)
                        (GenNodeId 69)
                        rhs
                        body
                        resultNode
                scopeForBoundary edgeId nodeId
                    | edgeId == letEdge
                    , nodeId == resultNode = Right letScope
                    | otherwise =
                        Left
                            ( ValidationFailed
                                [ "unexpected let Gamma scope lookup"
                                , show (edgeId, nodeId)
                                ]
                            )
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLetGamma
                        , lgoBoundaryEdge = letEdge
                        , lgoTermNode = resultNode
                        , lgoScope = letScope
                        }
            frame <- requireLocalGammaFrame (localGammaFrame scopeForBoundary letAnn)
            lgfOwner frame `shouldBe` Just owner
            lgfDirectEdgeSources frame `shouldBe` []
            lgfChildren frame `shouldBe` [rhs, body]

    describe "construction source-binder projection" $ do
        it "orders a declaration before an earlier sibling bound that consumes it" $ do
            let consumerRef = typeRef 22 "consumer"
                dependencyRef = typeRef 23 "dependency"
                body =
                    TArrow
                        (tVarWithRef consumerRef)
                        (tVarWithRef dependencyRef)
                unordered =
                    mkElabSchemeWithRefs
                        [ ( consumerRef
                          , Just
                              ( TArrow
                                  (tVarWithRef dependencyRef)
                                  (tVarWithRef dependencyRef)
                              )
                          )
                        , (dependencyRef, Nothing)
                        ]
                        body
                expected =
                    mkElabSchemeWithRefs
                        [ (dependencyRef, Nothing)
                        , ( consumerRef
                          , Just
                              ( TArrow
                                  (tVarWithRef dependencyRef)
                                  (tVarWithRef dependencyRef)
                              )
                          )
                        ]
                        body
            orderSourceProjectedSchemeBindersForTest
                "explicit structural scheme"
                unordered
                `shouldBe` Right expected

        it "captures an unbounded graph forall through its source sidecar in the whole packet" $ do
            let graphRef = typeRef 274 "graph-a"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 720))
                        "a"
                representative node
                    | node == NodeId 274 = NodeId 720
                    | otherwise = node
                graphBody =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
                sourceBody =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(graphRef, Nothing)] graphBody)
                        (IntMap.singleton 274 graphRef)
                expected =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] sourceBody)
                        (IntMap.singleton 274 sourceRef)
            resolveConstructionSourceBindersInSchemeInfoForTest
                representative
                (IntMap.singleton 720 sourceRef)
                packet
                `shouldBe` Right expected

        it "retains the current unbounded consumer as construction authority" $ do
            let graphRef = typeRef 274 "graph-a"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 726))
                        "a"
                body =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(graphRef, Nothing)] body)
                        (IntMap.singleton 274 graphRef)
            resolveConstructionSourceBindersInSchemeInfoExceptForTest
                (Set.singleton (typeBinderRefIdentity graphRef))
                id
                (IntMap.singleton 274 sourceRef)
                packet
                `shouldBe` Right packet

        it "joins a packet-local graph identity to its source identity by substitution key" $ do
            let graphRef = typeRef 274 "graph-a"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 723))
                        "a"
                graphBody =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
                sourceBody =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(graphRef, Nothing)] graphBody)
                        (IntMap.singleton 278 graphRef)
                expected =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] sourceBody)
                        (IntMap.fromList [(274, sourceRef), (278, sourceRef)])
            resolveConstructionSourceBindersInSchemeInfoForTest
                id
                (IntMap.singleton 278 sourceRef)
                packet
                `shouldBe` Right expected

        it "keeps an already-projected inherited source binder free" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 727))
                        "a"
                body =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sourceRef, Nothing)] body)
                        (IntMap.singleton 280 sourceRef)
                expected =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] body)
                        (IntMap.singleton 280 sourceRef)
            resolveConstructionSourceBindersInSchemeInfoForTest
                id
                (IntMap.singleton 280 sourceRef)
                packet
                `shouldBe` Right expected

        it "keeps an already-projected binder closed when its packet owns the declaration" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 731))
                        "a"
                packetType =
                    TForallRef
                        sourceRef
                        Nothing
                        ( TArrow
                            (tVarWithRef sourceRef)
                            (tVarWithRef sourceRef)
                        )
            resolveConstructionSourceBindersInTypeExceptForTest
                (Set.singleton (typeBinderRefIdentity sourceRef))
                id
                (IntMap.singleton 283 sourceRef)
                packetType
                `shouldBe` Right packetType

        it "keeps an already-projected inherited binder free from sidecar identity alone" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 729))
                        "a"
                body =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sourceRef, Nothing)] body)
                        IntMap.empty
                expected =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] body)
                        IntMap.empty
            resolveConstructionSourceBindersInSchemeInfoForTest
                id
                (IntMap.singleton 281 sourceRef)
                packet
                `shouldBe` Right expected

        it "retains an already-projected declaration owned by an exact endpoint" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 730))
                        "a"
                body =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sourceRef, Nothing)] body)
                        IntMap.empty
            resolveConstructionSourceBindersInSchemeInfoExceptForTest
                (Set.singleton (typeBinderRefIdentity sourceRef))
                id
                (IntMap.singleton 282 sourceRef)
                packet
                `shouldBe` Right packet

        it "rejects one packet identity joined to two source identities" $ do
            let graphRef = typeRef 274 "graph-a"
                firstSource =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 724))
                        "a"
                secondSource =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 725))
                        "b"
                packet =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(graphRef, Nothing)]
                            (tVarWithRef graphRef)
                        )
                        (IntMap.fromList [(278, graphRef), (279, graphRef)])
            case
                resolveConstructionSourceBindersInSchemeInfoForTest
                    id
                    (IntMap.fromList [(278, firstSource), (279, secondSource)])
                    packet
              of
                Left cause ->
                    cause
                        `shouldSatisfy` isInfixOf "multiple source binders"
                Right resolved ->
                    expectationFailure
                        ("expected ambiguous packet-source route, got " ++ show resolved)

        it "keeps structural owner identities out of free Gamma aliases" $ do
            let graphRef = typeRef 41 "structural-alias"
                structuralRef =
                    typeBinderRefFromIdentity
                        ( typeBinderIdentityFromStructural
                            (UniqueIdentity 7)
                            StructuralSelfBinder
                        )
                        "self"
                operatedType =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
            resolveConstructionSourceBindersInTypeForTest
                id
                (IntMap.singleton 41 structuralRef)
                operatedType
                `shouldBe` Right operatedType

        it "keeps a nested source-owned forall local to its type" $ do
            let graphRef = typeRef 741 "graph-field"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 742))
                        "field"
                nestedForall =
                    TForallRef
                        graphRef
                        Nothing
                        (TArrow (tVarWithRef graphRef) (tVarWithRef graphRef))
                operatedType =
                    TArrow
                        nestedForall
                        (TestElab.tBase (BaseTy "Int"))
            resolveConstructionSourceBindersInTypeForTest
                id
                (IntMap.singleton 741 sourceRef)
                operatedType
                `shouldBe` Right operatedType

        it "renames a bounded packet binder only under an equal exact endpoint" $ do
            let graphRef = typeRef 42 "graph-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 8))
                        "source-result"
                exactBoundRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 9))
                        "exact-bound"
                exactBound =
                    testTForall
                        "exact-bound"
                        Nothing
                        (TArrow (tVarWithRef exactBoundRef) (tVarWithRef exactBoundRef))
                operatedType =
                    TForallRef
                        graphRef
                        (Just exactBound)
                        (TArrow exactBound (tVarWithRef graphRef))
                exactType =
                    TForallRef
                        sourceRef
                        (Just exactBound)
                        (TArrow exactBound (tVarWithRef sourceRef))
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                (IntMap.singleton 42 sourceRef)
                exactType
                operatedType
                `shouldBe` Right exactType

        it "rejects exact bounded-binder adoption without a source sidecar route" $ do
            let graphRef = typeRef 42 "graph-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 8))
                        "source-result"
                exactBoundRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 9))
                        "exact-bound"
                exactBound =
                    testTForall
                        "exact-bound"
                        Nothing
                        (TArrow (tVarWithRef exactBoundRef) (tVarWithRef exactBoundRef))
                operatedType =
                    TForallRef
                        graphRef
                        (Just exactBound)
                        (TArrow exactBound (tVarWithRef graphRef))
                exactType =
                    TForallRef
                        sourceRef
                        (Just exactBound)
                        (TArrow exactBound (tVarWithRef sourceRef))
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                IntMap.empty
                exactType
                operatedType
                `shouldSatisfy` isLeftWithExactEndpointMismatch

        it "rejects exact bounded-binder adoption through the wrong source identity" $ do
            let graphRef = typeRef 42 "graph-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 8))
                        "source-result"
                wrongSourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 12))
                        "wrong-source-result"
                exactBoundRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 9))
                        "exact-bound"
                exactBound =
                    testTForall
                        "exact-bound"
                        Nothing
                        (TArrow (tVarWithRef exactBoundRef) (tVarWithRef exactBoundRef))
                operatedType =
                    TForallRef
                        graphRef
                        (Just exactBound)
                        (TArrow exactBound (tVarWithRef graphRef))
                exactType =
                    TForallRef
                        sourceRef
                        (Just exactBound)
                        (TArrow exactBound (tVarWithRef sourceRef))
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                (IntMap.singleton 42 wrongSourceRef)
                exactType
                operatedType
                `shouldSatisfy` isLeftWithExactEndpointMismatch

        it "keeps an ordinary bounded packet binder locally owned" $ do
            let graphRef = typeRef 43 "graph-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 10))
                        "source-result"
                boundRef = typeRef 44 "bound"
                bound =
                    testTForall
                        "bound"
                        Nothing
                        (TArrow (tVarWithRef boundRef) (tVarWithRef boundRef))
                operatedType =
                    TForallRef
                        graphRef
                        (Just bound)
                        (TArrow bound (tVarWithRef graphRef))
            resolveConstructionSourceBindersInTypeForTest
                id
                (IntMap.singleton 43 sourceRef)
                operatedType
                `shouldBe` Right operatedType

        it "reopens an already-projected inherited binder at its exact endpoint" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 728))
                        "a"
                exactType =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                operatedType =
                    TForallRef sourceRef Nothing exactType
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                IntMap.empty
                exactType
                operatedType
                `shouldBe` Right exactType

        it "reopens a graph-backed inherited binder only with an explicit exact sidecar route" $ do
            let graphRef = typeRef 729 "graph-a"
                exactType =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
                operatedType =
                    TForallRef graphRef Nothing exactType
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                (IntMap.singleton 729 graphRef)
                exactType
                operatedType
                `shouldBe` Right exactType
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                IntMap.empty
                exactType
                operatedType
                `shouldSatisfy` isLeftWithExactEndpointMismatch

        it "preserves an operated packet forall when the exact edge owns only its body" $ do
            let packetRef = typeRef 731 "packet-a"
                packetResultRef = typeRef 732 "packet-result"
                exactPacketRef = typeRef 731 "source-a"
                exactResultRef = typeRef 732 "source-result"
                packetBody =
                    TArrow
                        (tVarWithRef packetRef)
                        (tVarWithRef packetResultRef)
                exactBody =
                    TArrow
                        (tVarWithRef exactPacketRef)
                        (tVarWithRef exactResultRef)
                packetType =
                    TForallRef packetRef Nothing packetBody
                expectedPacket =
                    TForallRef
                        packetRef
                        Nothing
                        ( TArrow
                            (tVarWithRef packetRef)
                            (tVarWithRef exactResultRef)
                        )
            resolveConstructionSourceBindersInPacketAtExpectedForTest
                id
                IntMap.empty
                exactBody
                packetType
                `shouldBe` Right expectedPacket

        it "keeps an already-projected binder declared by its exact endpoint" $ do
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 730))
                        "a"
                exactType =
                    TForallRef
                        sourceRef
                        Nothing
                        ( TArrow
                            (tVarWithRef sourceRef)
                            (tVarWithRef sourceRef)
                        )
            resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                (IntMap.singleton 282 sourceRef)
                exactType
                exactType
                `shouldBe` Right exactType

        it "keeps a bounded packet binder graph-owned in its scheme and substitution" $ do
            let graphRef = typeRef 43 "graph-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 721))
                        "source-result"
                bound :: BoundType
                bound = TestElab.tBase (BaseTy "Bool")
                body =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
                packet =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(graphRef, Just bound)] body)
                        (IntMap.singleton 43 graphRef)
            resolveConstructionSourceBindersInSchemeInfoForTest
                id
                (IntMap.singleton 43 sourceRef)
                packet
                `shouldBe` Right packet

        it "preserves a bounded leading-body forall as construction-local" $ do
            let graphRef = typeRef 46 "leading-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 722))
                        "source-leading-result"
                bound :: BoundType
                bound = TestElab.tBase (BaseTy "Bool")
                body =
                    TArrow
                        (tVarWithRef graphRef)
                        (tVarWithRef graphRef)
                packet =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            []
                            (TForallRef graphRef (Just bound) body)
                        )
                        (IntMap.singleton 46 graphRef)
            resolveConstructionSourceBindersInSchemeInfoForTest
                id
                (IntMap.singleton 46 sourceRef)
                packet
                `shouldBe` Right packet

        it "rejects a bounded packet whose exact endpoint has a different bound" $ do
            let graphRef = typeRef 45 "graph-result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 11))
                        "source-result"
                graphBound = TestElab.tBase (BaseTy "Int")
                exactBound = TestElab.tBase (BaseTy "Bool")
                operatedType =
                    TForallRef
                        graphRef
                        (Just graphBound)
                        (tVarWithRef graphRef)
                exactType =
                    TForallRef
                        sourceRef
                        (Just exactBound)
                        (tVarWithRef sourceRef)
            case resolveConstructionSourceBindersInTypeAtExpectedForTest
                id
                (IntMap.singleton 45 sourceRef)
                exactType
                operatedType of
                Left cause ->
                    cause `shouldSatisfy` isInfixOf "does not equal its exact source endpoint"
                Right aligned ->
                    expectationFailure
                        ("expected exact bound mismatch, got " ++ show aligned)

    describe "compiler-exact source result completion" $ do
        let exactEdge = EdgeId 433
            rawResultRef = typeRef 434 "result"
            boolTy :: ElabType
            boolTy = TestElab.tBase (BaseTy "Bool")
            exactResultTy = TArrow boolTy boolTy
            exactResultBound :: BoundType
            exactResultBound = TArrow boolTy boolTy
            provisionalResultBound :: BoundType
            provisionalResultBound = TArrow TBottom TBottom
            provisionalInfo =
                schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [(rawResultRef, Just provisionalResultBound)]
                        (tVarWithRef rawResultRef)
                    )
                    (IntMap.singleton 434 rawResultRef)
            evidenceOwner =
                generatedResolvedLocal
                    435
                    "$evidence"
                    "$evidence"
                    boolTy
            ownerPackets packet =
                Map.singleton
                    (ownerKey 435 "$evidence")
                    packet
            exactBody =
                mkTestLocalLam
                    "value"
                    boolTy
                    (mkTestDeferredVar "value")
            exactSourceTerm resultRef =
                ETyAbsRef
                    resultRef
                    (Just exactResultBound)
                    (ELam evidenceOwner exactBody)
            preparePacket0 =
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        DirectPacket
                        provisionalInfo
                        provisionalInfo
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "compiler-exact result packet preparation failed"
                    Right (packet, _) ->
                        case schemeBinderRefs (siScheme (subtermGeneralizationSchemeInfo packet)) of
                            [(resultRef, _)] -> pure (resultRef, packet)
                            binders ->
                                expectationFailure
                                    ("expected one prepared result binder, got " ++ show binders)
                                    >> fail "compiler-exact result packet shape failed"

        it "keeps the lambda-constructed bound for a source-owned delayed result" $ do
            (resultRef, packet0) <- preparePacket0
            packet <-
                case
                    withCompilerExactSourceSubtermResult
                        exactEdge
                        resultRef
                        resultRef
                        packet0
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-owned result preparation failed"
                    Right prepared -> pure prepared
            completed <-
                case
                    completeCompilerExactSubtermResults
                        CompleteBeforeCompilerExact
                        exactEdge
                        (ownerPackets packet)
                        (exactSourceTerm resultRef)
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-owned result completion failed"
                    Right term -> pure term
            case completed of
                ETyAbsRef completedRef (Just completedBound) _ -> do
                    completedRef
                        `shouldSatisfy` typeBinderRefsSameIdentity resultRef
                    tyToElab completedBound `shouldBe` exactResultTy
                other ->
                    expectationFailure
                        ("expected one completed source result binder, got " ++ show other)
            AlgebraTestSupport.closedTermTypeChecksForTest completed
                `shouldBe` True

        it "replaces the provisional bound in the source-owned producer spine" $ do
            (resultRef, packet0) <- preparePacket0
            packet <-
                case
                    withCompilerExactSourceSubtermResult
                        exactEdge
                        resultRef
                        resultRef
                        packet0
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-owned result preparation failed"
                    Right prepared -> pure prepared
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 437))
                        "a"
                sourceTy = tVarWithRef sourceRef
                constructedResultTy = TArrow sourceTy sourceTy
                constructedResultBound :: BoundType
                constructedResultBound =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                localResultRef = typeRef 438 "local-result"
                evidenceTy =
                    TArrow
                        constructedResultTy
                        constructedResultTy
                evidence =
                    generatedResolvedLocal
                        435
                        "$evidence"
                        "$evidence"
                        evidenceTy
                value =
                    generatedResolvedLocal
                        439
                        "value"
                        "value"
                        sourceTy
                producer =
                    ETyAbsRef sourceRef Nothing
                        ( ETyAbsRef resultRef (Just provisionalResultBound)
                            ( ETyAbsRef localResultRef (Just constructedResultBound)
                                ( ELam evidence
                                    ( ETyInst
                                        (ELam value (EVarNode value))
                                        (InstAbstrRef resultRef)
                                    )
                                )
                            )
                        )
                certificate =
                    CompilerExactResultBoundCertificate
                        { cerbcOwner = ownerKey 435 "$evidence"
                        , cerbcBoundary = exactEdge
                        , cerbcResultRef = resultRef
                        , cerbcBound = constructedResultTy
                        }
            completed <-
                case
                    completeCompilerExactSubtermResultsWithBounds
                        [certificate]
                        CompleteBeforeCompilerExact
                        exactEdge
                        (ownerPackets packet)
                        producer
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-owned producer completion failed"
                    Right term -> pure term
            case completed of
                ETyAbsRef completedSource Nothing
                    (ETyAbsRef completedResult (Just completedBound) _) -> do
                        completedSource
                            `shouldSatisfy` typeBinderRefsSameIdentity sourceRef
                        completedResult
                            `shouldSatisfy` typeBinderRefsSameIdentity resultRef
                        tyToElab completedBound `shouldBe` constructedResultTy
                other ->
                    expectationFailure
                        ("expected the real producer Gamma spine, got " ++ show other)
            AlgebraTestSupport.closedTermTypeChecksForTest completed
                `shouldBe` True

        it "rejects a same-named result-bound certificate for the producer spine" $ do
            (resultRef, packet0) <- preparePacket0
            packet <-
                case
                    withCompilerExactSourceSubtermResult
                        exactEdge
                        resultRef
                        resultRef
                        packet0
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-owned result preparation failed"
                    Right prepared -> pure prepared
            let peerRef = typeRef 437 (typeBinderRefName resultRef)
                wrongCertificate =
                    CompilerExactResultBoundCertificate
                        { cerbcOwner = ownerKey 435 "$evidence"
                        , cerbcBoundary = exactEdge
                        , cerbcResultRef = peerRef
                        , cerbcBound = exactResultTy
                        }
                provisionalTerm =
                    ETyAbsRef
                        resultRef
                        (Just provisionalResultBound)
                        (ELam evidenceOwner exactBody)
            completed <-
                case
                    completeCompilerExactSubtermResultsWithBounds
                        [wrongCertificate]
                        CompleteBeforeCompilerExact
                        exactEdge
                        (ownerPackets packet)
                        provisionalTerm
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "wrong-identity certificate completion failed"
                    Right term -> pure term
            case completed of
                ETyAbsRef completedRef (Just completedBound) _ -> do
                    completedRef
                        `shouldSatisfy` typeBinderRefsSameIdentity resultRef
                    tyToElab completedBound
                        `shouldBe` TArrow TBottom TBottom
                other ->
                    expectationFailure
                        ("expected one provisional result binder, got " ++ show other)
            AlgebraTestSupport.closedTermTypeChecksForTest completed
                `shouldBe` False

        it "does not use a same-named peer as source result authority" $ do
            (resultRef, packet0) <- preparePacket0
            packet <-
                case
                    withCompilerExactSourceSubtermResult
                        exactEdge
                        resultRef
                        resultRef
                        packet0
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-owned result preparation failed"
                    Right prepared -> pure prepared
            let peerRef = typeRef 436 (typeBinderRefName resultRef)
                peerTerm = exactSourceTerm peerRef
            completed <-
                case
                    completeCompilerExactSubtermResults
                        CompleteBeforeCompilerExact
                        exactEdge
                        (ownerPackets packet)
                        peerTerm
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "same-named peer completion failed"
                    Right term -> pure term
            case completed of
                ETyAbsRef completedPeer (Just peerBound)
                    (ETyAbsRef completedResult (Just packetBound) _) -> do
                        completedPeer
                            `shouldSatisfy` typeBinderRefsSameIdentity peerRef
                        completedResult
                            `shouldSatisfy` typeBinderRefsSameIdentity resultRef
                        tyToElab peerBound `shouldBe` exactResultTy
                        tyToElab packetBound
                            `shouldBe` TArrow TBottom TBottom
                other ->
                    expectationFailure
                        ("expected distinct peer and packet result binders, got " ++ show other)
            AlgebraTestSupport.closedTermTypeChecksForTest completed
                `shouldBe` False

        it "does not let a packet-only completion inherit a pre-bound declaration" $ do
            (resultRef, packet0) <- preparePacket0
            packet <-
                case
                    withCompilerExactPacketSubtermResult
                        exactEdge
                        resultRef
                        packet0
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "packet-only result preparation failed"
                    Right prepared -> pure prepared
            completeCompilerExactSubtermResults
                CompleteAfterCompilerExact
                exactEdge
                (ownerPackets packet)
                (exactSourceTerm resultRef)
                `shouldSatisfy` isLeft

    describe "compiler-exact Gamma-bound publication" $ do
        let sourceRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromUnique (UniqueIdentity 430))
                    "a"
            constructionRef = typeRef 431 "c"
            unrelatedRef = typeRef 432 "d"
            boolTy :: ElabType
            boolTy = TestElab.tBase (BaseTy "Bool")
            boolBound :: BoundType
            boolBound = TestElab.tBase (BaseTy "Bool")
            schemeFor ty =
                schemeInfoFromRefSubst
                    (mkElabSchemeWithRefs [] ty)
                    IntMap.empty
            preparePublishedResult ty renames = do
                    (packet, _) <-
                        prepareSubtermGeneralizationPacket
                            initialIdentityGenerator
                            DirectPacket
                            (schemeFor ty)
                            (schemeFor ty)
                    withCompilerExactBinderRenames renames packet
            preparePublished ty renames =
                case preparePublishedResult ty renames
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "compiler-exact packet publication failed"
                    Right packet -> pure packet
            publishedType =
                schemeToType
                    . subtermGeneralizationGammaBoundScheme

        it "publishes a free construction reference in the exact source domain" $ do
            let constructionTy =
                    TArrow (tVarWithRef constructionRef) boolTy
                sourceTy = TArrow (tVarWithRef sourceRef) boolTy
            packet <-
                preparePublished
                    constructionTy
                    [(sourceRef, constructionRef)]
            publishedType packet `shouldBe` sourceTy

        it "keeps an already source-aligned bound unchanged" $ do
            let sourceTy = TArrow (tVarWithRef sourceRef) boolTy
            packet <-
                preparePublished
                    sourceTy
                    [(sourceRef, constructionRef)]
            publishedType packet `shouldBe` sourceTy

        it "rejects a matching bounded slot without a valid bound proof" $ do
            let boundedConstructionTy =
                    tForallWithRef constructionRef (Just boolBound)
                        (tVarWithRef constructionRef)
            case preparePublishedResult
                boundedConstructionTy
                [(sourceRef, constructionRef)] of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "compiler-exact Gamma-bound publication failed")
                other ->
                    expectationFailure
                        ("expected bounded publication rejection, got " ++ show other)

        it "instantiates only the matching leading construction binder" $ do
            let constructionTy =
                    tForallWithRef constructionRef Nothing
                        (TArrow (tVarWithRef constructionRef) (tVarWithRef constructionRef))
                sourceTy =
                    TArrow (tVarWithRef sourceRef) (tVarWithRef sourceRef)
            packet <-
                preparePublished
                    constructionTy
                    [(sourceRef, constructionRef)]
            publishedType packet `shouldBe` sourceTy

        it "keeps an irrelevant leading binder unchanged" $ do
            let unrelatedTy =
                    tForallWithRef unrelatedRef Nothing
                        (TArrow (tVarWithRef unrelatedRef) (tVarWithRef unrelatedRef))
            packet <-
                preparePublished
                    unrelatedTy
                    [(sourceRef, constructionRef)]
            publishedType packet `shouldBe` unrelatedTy

        it "preserves unrelated leading binders before the matching slot" $ do
            let constructionTy =
                    tForallWithRef unrelatedRef Nothing
                        ( tForallWithRef constructionRef Nothing
                            (TArrow (tVarWithRef unrelatedRef) (tVarWithRef constructionRef))
                        )
                sourceTy =
                    tForallWithRef unrelatedRef Nothing
                        (TArrow (tVarWithRef unrelatedRef) (tVarWithRef sourceRef))
            packet <-
                preparePublished
                    constructionTy
                    [(sourceRef, constructionRef)]
            publishedType packet `shouldBe` sourceTy

        it "specializes only the matching slot in a consumer-owned bound" $ do
            let consumerRef = typeRef 433 "consumer"
                consumerBound :: BoundType
                consumerBound =
                    tForallWithRef unrelatedRef Nothing
                        ( tForallWithRef constructionRef Nothing
                            (TArrow (tVarWithRef unrelatedRef) (tVarWithRef constructionRef))
                        )
                expectedBound :: BoundType
                expectedBound =
                    tForallWithRef unrelatedRef Nothing
                        (TArrow (tVarWithRef unrelatedRef) (tVarWithRef constructionRef))
                consumerInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(consumerRef, Just consumerBound)]
                            (tVarWithRef consumerRef)
                        )
                        IntMap.empty
            (prepared, _) <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( EnclosingConsumerPacket
                            (typeBinderRefIdentity consumerRef)
                            (EdgeId 433)
                            testEnclosingConsumerOwner
                        )
                        consumerInfo
                        consumerInfo
                of
                    Left err -> expectationFailure (show err) >> fail "consumer packet preparation failed"
                    Right result -> pure result
            packet <-
                case withCompilerExactBinderRenames [(sourceRef, constructionRef)] prepared of
                    Left err -> expectationFailure (show err) >> fail "consumer specialization failed"
                    Right specialized -> pure specialized
            schemeBinderRefs
                (siScheme (subtermGeneralizationConsumerConstructionSchemeInfo packet))
                `shouldBe` [(consumerRef, Just expectedBound)]

        it "keeps a consumer bound whose exact source argument is already published" $ do
            let consumerRef = typeRef 434 "consumer"
                consumerBound :: BoundType
                consumerBound =
                    TArrow (tVarWithRef sourceRef) boolTy
                consumerInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(consumerRef, Just consumerBound)]
                            (tVarWithRef consumerRef)
                        )
                        IntMap.empty
            (prepared, _) <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( EnclosingConsumerPacket
                            (typeBinderRefIdentity consumerRef)
                            (EdgeId 434)
                            testEnclosingConsumerOwner
                        )
                        consumerInfo
                        consumerInfo
                of
                    Left err -> expectationFailure (show err) >> fail "consumer packet preparation failed"
                    Right result -> pure result
            packet <-
                case withCompilerExactBinderRenames [(sourceRef, constructionRef)] prepared of
                    Left err -> expectationFailure (show err) >> fail "source-aligned consumer specialization failed"
                    Right specialized -> pure specialized
            schemeBinderRefs
                (siScheme (subtermGeneralizationConsumerConstructionSchemeInfo packet))
                `shouldBe` [(consumerRef, Just consumerBound)]

        it "rejects a consumer bound without the routed construction slot" $ do
            let consumerRef = typeRef 435 "consumer"
                consumerBound :: BoundType
                consumerBound =
                    tForallWithRef unrelatedRef Nothing
                        (TArrow (tVarWithRef unrelatedRef) (tVarWithRef unrelatedRef))
                consumerInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(consumerRef, Just consumerBound)]
                            (tVarWithRef consumerRef)
                        )
                        IntMap.empty
            prepared <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( EnclosingConsumerPacket
                            (typeBinderRefIdentity consumerRef)
                            (EdgeId 435)
                            testEnclosingConsumerOwner
                        )
                        consumerInfo
                        consumerInfo
                of
                    Left err -> expectationFailure (show err) >> fail "consumer packet preparation failed"
                    Right (packet, _) -> pure packet
            case withCompilerExactBinderRenames [(sourceRef, constructionRef)] prepared of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "has no matching construction slot")
                Left err ->
                    expectationFailure
                        ("expected construction-slot validation failure, got " ++ show err)
                Right specialized ->
                    expectationFailure
                        ("expected consumer specialization rejection, got " ++ show specialized)

    describe "Gamma construction source publication" $ do
        let consumerRef = typeRef 436 "result"
            authority =
                GammaPacketAuthority
                    (EdgeId 436)
                    (GenNodeId 0)
                    (typeBinderRefIdentity consumerRef)
            intTy :: ElabType
            intTy = TestElab.tBase (BaseTy "Int")
            intBound :: BoundType
            intBound = TestElab.tBase (BaseTy "Int")
            packetInfo =
                schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [(consumerRef, Just intBound)]
                        (TArrow intTy (tVarWithRef consumerRef))
                    )
                    IntMap.empty
            operatedInfo =
                schemeInfoFromRefSubst
                    (mkElabSchemeWithRefs [] intTy)
                    IntMap.empty
            prepareGammaPublisherPacket =
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (GammaPacket authority)
                        packetInfo
                        operatedInfo
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "Gamma publisher packet preparation failed"
                    Right (packet, _) -> pure packet

        it "publishes the completed producer after its owned Gamma result is constructed" $ do
            prepared <- prepareGammaPublisherPacket
            published <-
                case
                    publishSubtermGammaConstructionSourceSchemeInfo
                        (gpaEdgeId authority)
                        prepared
                        packetInfo
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "Gamma construction source publication failed"
                    Right schemeInfo -> pure schemeInfo
            schemeToType
                (siScheme published)
                `shouldBe` TArrow intTy intTy

        it "rejects a publisher edge outside the packet Gamma authority" $ do
            prepared <- prepareGammaPublisherPacket
            case
                publishSubtermGammaConstructionSourceSchemeInfo
                    (EdgeId 437)
                    prepared
                    packetInfo
                of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "no matching packet authority")
                Left err ->
                    expectationFailure
                        ("expected authority rejection, got " ++ show err)
                Right published ->
                    expectationFailure
                        ("expected authority rejection, got " ++ show published)

    describe "root RaiseMerge Γ construction" $ do
        let edgeId = EdgeId 91
            operatedRoot = NodeId 11
            exterior = NodeId 17
            aliasRef = typeRef 14 "result"
            innerRef = typeRef 35 "inner"
            resultBound =
                testTForall
                    "inner"
                    Nothing
                    (TArrow (tVarWithRef innerRef) (tVarWithRef innerRef))
            lambdaOwnerBody result =
                TArrow (tyToElab resultBound) result
            resultScheme =
                mkElabSchemeWithRefs
                    [(aliasRef, Just resultBound)]
                    (lambdaOwnerBody (tVarWithRef aliasRef))
            resultOnlyExteriorRef = typeRef 17 "result"
            resultOnlySchemeInfo =
                schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [(resultOnlyExteriorRef, Just resultBound)]
                        (tVarWithRef resultOnlyExteriorRef)
                    )
                    (IntMap.singleton (getNodeId exterior) resultOnlyExteriorRef)
            ann =
                ALam
                    "x"
                    (ownerDetails 91 "x")
                    (NodeId 90)
                    (GenNodeId 90)
                    (ALit (LInt 1) (NodeId 92))
                    edgeId
                    (NodeId 93)
            witness =
                EdgeWitness
                    { ewEdgeId = edgeId
                    , ewLeft = operatedRoot
                    , ewRight = exterior
                    , ewRoot = operatedRoot
                    , ewForallIntros = 0
                    , ewWitness = InstanceWitness [OpRaiseMerge operatedRoot exterior]
                    }
            trace =
                EdgeTrace
                    { etRoot = operatedRoot
                    , etResultRoot = exterior
                    , etBinderArgs = []
                    , etInterior = sourceInteriorFromList [operatedRoot]
                    , etBinderReplayMap = IntMap.empty
                    , etReplayDomainBinders = []
                    , etCopyMap = mempty
                    , etReplayContract = ReplayContractNone
                    }
            artifacts =
                edgeArtifactsForTest
                    (IntMap.singleton 91 ExpIdentity)
                    (IntMap.singleton 91 witness)
                    (IntMap.singleton 91 trace)
                    IntSet.empty
            requirementsFor resultRoot expectedBound =
                    GeneralizationRequirements
                        { grRequiredGammaBinders =
                        [ RequiredGammaBinder
                            { rgbEdgeIds = edgeId :| []
                            , rgbExteriorNode = exterior
                            , rgbOperatedRoot = operatedRoot
                            , rgbResultRoots = resultRoot :| []
                            , rgbOperatedType = expectedBound
                            , rgbExactOperatedOccurrenceRef = Nothing
                            , rgbPlacement = RequiredGammaAtCurrentScope
                            }
                        ]
                        , grSourceBinderRefs = IntMap.empty
                        , grAmbientBinderRefs = []
                        , grAmbientGammaAuthorities = IntMap.empty
                        , grLocallyClosedGammaNodes = mempty
                        }

        it "does not grant Γ authority to a rigid root transition" $ do
            let rigidWitness =
                    witness
                        { ewWitness =
                            InstanceWitness
                                [ OpWeaken operatedRoot
                                , OpRaiseMerge operatedRoot exterior
                                ]
                        }
                rigidArtifacts =
                    setEdgeArtifactWitnessForTest
                        edgeId
                        rigidWitness
                        artifacts
            rootRaiseMergeAuthorityFor rigidArtifacts edgeId `shouldBe` Right Nothing

        it "accepts an exactly constructed exterior Γ binder" $ do
            let exteriorRef = typeRef 17 "result"
                exteriorScheme =
                    mkElabSchemeWithRefs
                        [(exteriorRef, Just resultBound)]
                        (lambdaOwnerBody (tVarWithRef exteriorRef))
                schemeInfo =
                    schemeInfoFromRefSubst
                        exteriorScheme
                        (IntMap.singleton (getNodeId exterior) exteriorRef)
            prepared <-
                case prepareRootRaiseMergeScheme
                        artifacts
                        ann
                        (requirementsFor exterior (tyToElab resultBound))
                        schemeInfo of
                    Left err -> expectationFailure (show err) >> fail "Γ preparation failed"
                    Right result -> pure result
            case schemeBinderRefs (siScheme prepared) of
                [(actualExteriorRef, Just actualBound)] -> do
                    typeBinderRefNode actualExteriorRef `shouldBe` Just exterior
                    actualBound `shouldBe` resultBound
                    schemeBody (siScheme prepared)
                        `shouldBe` lambdaOwnerBody (tVarWithRef actualExteriorRef)
                    IntMap.lookup (getNodeId exterior) (siSubstRefs prepared)
                        `shouldSatisfy` maybe False (typeBinderRefsSameIdentity actualExteriorRef)
                other ->
                    expectationFailure
                        ("expected one existing exterior Γ binder, got " ++ show other)

        it "rejects a result-only scheme at the lambda-owner boundary" $ do
            case prepareRootRaiseMergeScheme
                    artifacts
                    ann
                    (requirementsFor exterior (tyToElab resultBound))
                    resultOnlySchemeInfo of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "owner did not reify as a lambda arrow")
                other ->
                    expectationFailure
                        ("expected result-only lambda-owner rejection, got " ++ show other)

        it "accepts the same result-only scheme at the expression-result edge boundary" $ do
            prepareRootRaiseMergeSchemeAtEdge
                artifacts
                edgeId
                (requirementsFor exterior (tyToElab resultBound))
                resultOnlySchemeInfo
                `shouldBe` Right resultOnlySchemeInfo

        it "accepts an omitted exterior bound exactly when S(operated) is bottom" $ do
            let exteriorRef = typeRef 17 "result"
                schemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(exteriorRef, Nothing)]
                            (lambdaOwnerBody (tVarWithRef exteriorRef))
                        )
                        (IntMap.singleton (getNodeId exterior) exteriorRef)
            prepareRootRaiseMergeScheme
                artifacts
                ann
                (requirementsFor exterior TBottom)
                schemeInfo
                `shouldBe` Right schemeInfo

        it "rejects an omitted exterior bound when S(operated) is non-bottom" $ do
            let exteriorRef = typeRef 17 "result"
                schemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(exteriorRef, Nothing)]
                            (lambdaOwnerBody (tVarWithRef exteriorRef))
                        )
                        (IntMap.singleton (getNodeId exterior) exteriorRef)
            case prepareRootRaiseMergeScheme
                    artifacts
                    ann
                    (requirementsFor exterior (tyToElab resultBound))
                    schemeInfo of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "disagrees with S(operated)")
                other ->
                    expectationFailure
                        ("expected omitted non-bottom bound rejection, got " ++ show other)

        it "rejects a local Gamma alias whose bound disagrees with S(operated)" $ do
            let localAliasRef = typeRef 14 "alias"
                schemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(localAliasRef, Nothing)]
                            (lambdaOwnerBody (tVarWithRef localAliasRef))
                        )
                        (IntMap.singleton (getNodeId exterior) localAliasRef)
            case prepareRootRaiseMergeScheme
                    artifacts
                    ann
                    (requirementsFor exterior (tyToElab resultBound))
                    schemeInfo of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "local Gamma alias target disagrees")
                other ->
                    expectationFailure
                        ("expected local alias bound rejection, got " ++ show other)

        it "accepts quotienting a local Gamma to the lexical variable used by S(operated)" $ do
            let localAliasRef = typeRef 14 "alias"
                schemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(localAliasRef, Nothing)]
                            (lambdaOwnerBody (tVarWithRef localAliasRef))
                        )
                        (IntMap.singleton (getNodeId exterior) localAliasRef)
            prepareRootRaiseMergeScheme
                artifacts
                ann
                (requirementsFor exterior (TVarRef localAliasRef))
                schemeInfo
                `shouldBe` Right schemeInfo

        it "accepts an exact requirement-owned alias after its result occurrence becomes vacuous" $ do
            let localAliasRef = typeRef 14 "alias"
                boolTy = TestElab.tBase (BaseTy "Bool")
                schemeInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] (lambdaOwnerBody boolTy))
                        (IntMap.singleton (getNodeId exterior) localAliasRef)
            prepareRootRaiseMergeScheme
                artifacts
                ann
                (requirementsFor exterior (TVarRef localAliasRef))
                schemeInfo
                `shouldBe` Right schemeInfo

        it "rejects result-root-only provenance" $ do
            let resultRoot = NodeId 18
                resultTrace = trace {etResultRoot = resultRoot}
                resultArtifacts =
                    setEdgeArtifactTraceForTest
                        edgeId
                        resultTrace
                        artifacts
                schemeInfo =
                    schemeInfoFromRefSubst
                        resultScheme
                        (IntMap.singleton (getNodeId resultRoot) aliasRef)
            case prepareRootRaiseMergeScheme
                    resultArtifacts
                    ann
                    (requirementsFor resultRoot (tyToElab resultBound))
                    schemeInfo of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "exterior has no Γ substitution")
                other ->
                    expectationFailure
                        ("expected result-root-only provenance rejection, got " ++ show other)

        it "rejects a root RaiseMerge when Γ lacks exact exterior provenance" $ do
            let schemeInfo =
                    schemeInfoFromRefSubst
                        resultScheme
                        (IntMap.singleton 14 aliasRef)
            case prepareRootRaiseMergeScheme
                    artifacts
                    ann
                    (requirementsFor exterior (tyToElab resultBound))
                    schemeInfo of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "no Γ substitution")
                other ->
                    expectationFailure
                        ("expected missing exterior Γ provenance rejection, got " ++ show other)

        it "rejects conflicting direct and substitution Γ provenance" $ do
            let directExteriorRef = typeRef 17 "result"
                conflictingRef = typeRef 14 "other-result"
                scheme =
                    mkElabSchemeWithRefs
                        [ (directExteriorRef, Just resultBound)
                        , (conflictingRef, Nothing)
                        ]
                        (lambdaOwnerBody (tVarWithRef directExteriorRef))
                schemeInfo =
                    SchemeInfo
                        { siScheme = scheme
                        , siSubstRefs = IntMap.singleton (getNodeId exterior) conflictingRef
                        }
            case prepareRootRaiseMergeScheme
                    artifacts
                    ann
                    (requirementsFor exterior (tyToElab resultBound))
                    schemeInfo of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "alias was not quotiented to S(operated)")
                other ->
                    expectationFailure
                        ("expected conflicting Γ provenance rejection, got " ++ show other)

    describe "lambda-body Gamma consumer routes" $ do
        let edgeId = EdgeId 901
            otherEdgeId = EdgeId 902
            exterior = NodeId 68
            operated = NodeId 67
            result = NodeId 69
            constructionRef = typeRef 32 "construction-consumer"
            otherConstructionRef = typeRef 33 "other-construction-consumer"
            semanticRef = typeRef 68 "semantic-consumer"
            otherSemanticRef = typeRef 69 "other-semantic-consumer"
            declaredBound =
                TArrow TBottom TBottom
            owner =
                testEnclosingConsumerOwner
                    { lgoBoundaryEdge = edgeId
                    , lgoTermNode = NodeId 101
                    }
            otherOwner =
                owner
                    { lgoTermNode = NodeId 102
                    }
            requirementAt placement =
                RequiredGammaBinder
                    { rgbEdgeIds = edgeId :| []
                    , rgbExteriorNode = exterior
                    , rgbOperatedRoot = operated
                    , rgbResultRoots = result :| []
                    , rgbOperatedType = declaredBound
                    , rgbExactOperatedOccurrenceRef = Nothing
                    , rgbPlacement = placement
                    }
            requirementsWith binders =
                emptyGeneralizationRequirements
                    { grRequiredGammaBinders = binders
                    }
            requirements =
                requirementsWith
                    [requirementAt RequiredGammaAtCurrentScope]
            aliases =
                IntMap.singleton
                    (getNodeId exterior)
                    constructionRef
            packetInfo ref =
                schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [(ref, Just TBottom)]
                        (tVarWithRef ref)
                    )
                    IntMap.empty
            requireRoute selected =
                case selected of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "body-consumer route selection failed"
                    Right Nothing ->
                        expectationFailure "expected a body-consumer route"
                            >> fail "body-consumer route was absent"
                    Right (Just route) -> pure route

        it "selects and validates only the exact exterior-to-construction route" $ do
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        requirements
                        aliases
                    )
            AlgebraTestSupport.bcrtvEdgeId route `shouldBe` edgeId
            AlgebraTestSupport.bcrtvOwner route `shouldBe` owner
            AlgebraTestSupport.bcrtvExteriorNode route `shouldBe` exterior
            AlgebraTestSupport.bcrtvSemanticRef route
                `shouldSatisfy` typeBinderRefsSameIdentity semanticRef
            AlgebraTestSupport.bcrtvConstructionRef route
                `shouldSatisfy` typeBinderRefsSameIdentity constructionRef
            AlgebraTestSupport.bcrtvOperatedType route `shouldBe` declaredBound
            (packet, _) <-
                preparePacketForOwner
                    initialIdentityGenerator
                    owner
                    edgeId
                    semanticRef
                    (packetInfo semanticRef)
            AlgebraTestSupport.validateBodyConsumerRouteForTest
                owner
                edgeId
                aliases
                packet
                route
                `shouldBe` Right ()

        it "routes a bare packet-operated occurrence through its exact consumer authority" $ do
            let operatedRef =
                    typeRef (getNodeId operated) "packet-operated"
                operatedType = tVarWithRef operatedRef
                operatedRequirements =
                    requirementsWith
                        [ (requirementAt RequiredGammaAtCurrentScope)
                            { rgbOperatedType = operatedType
                            }
                        ]
                operatedPacketInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] operatedType)
                        (IntMap.singleton (getNodeId operated) operatedRef)
            (packet, _) <-
                preparePacketForOwner
                    initialIdentityGenerator
                    owner
                    edgeId
                    semanticRef
                    operatedPacketInfo
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteWithPacketForTest
                        owner
                        edgeId
                        packet
                        operatedRequirements
                        aliases
                    )
            AlgebraTestSupport.bcrtvOperatedType route
                `shouldBe` operatedType
            AlgebraTestSupport.bcrtvConstructionOperatedType route
                `shouldBe` tVarWithRef constructionRef
            AlgebraTestSupport.validateBodyConsumerRouteForTest
                owner
                edgeId
                aliases
                packet
                route
                `shouldBe` Right ()

        it "projects a bare source-sidecar occurrence through its exact construction alias" $ do
            let operatedRef =
                    typeRef (getNodeId operated) "source-operated"
                operatedType = tVarWithRef operatedRef
                operatedRequirements =
                    requirementsWith
                        [ (requirementAt RequiredGammaAtCurrentScope)
                            { rgbOperatedType = operatedType
                            }
                        ]
                sourceSidecarAliases =
                    IntMap.insert
                        (getNodeId operated)
                        constructionRef
                        aliases
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        operatedRequirements
                        sourceSidecarAliases
                    )
            AlgebraTestSupport.bcrtvConstructionOperatedType route
                `shouldBe` tVarWithRef constructionRef

        it "constructs a checked source projection from exact sidecar and construction routes" $ do
            let operatedRef =
                    typeRef (getNodeId operated) "source-operated"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 7))
                        "a"
                operatedType =
                    TArrow
                        (tVarWithRef operatedRef)
                        TBottom
                checkedSource =
                    TArrow
                        (tVarWithRef sourceRef)
                        TBottom
                projectedType =
                    TArrow
                        (tVarWithRef operatedRef)
                        TBottom
                operatedRequirements =
                    requirementsWith
                        [ (requirementAt RequiredGammaAtCurrentScope)
                            { rgbOperatedType = operatedType
                            }
                        ]
                constructionAliases =
                    IntMap.insert
                        (getNodeId operated)
                        constructionRef
                        aliases
                sourceBinderRefs =
                    IntMap.singleton
                        (getNodeId operated)
                        sourceRef
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        operatedRequirements
                        constructionAliases
                    )
            AlgebraTestSupport.bcrtvConstructionOperatedType route
                `shouldBe` operatedType
            AlgebraTestSupport.validateBodyConsumerCheckedSourceProjectionForTest
                sourceBinderRefs
                constructionAliases
                [(sourceRef, operatedRef)]
                route
                checkedSource
                projectedType
                `shouldBe` Right ()

        it "specializes an already projected body consumer without reapplying its construction route" $ do
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        requirements
                        aliases
                    )
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 7))
                        "a"
                sameNamedPeer =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 8))
                        "a"
                sourceTy =
                    TArrow
                        (tVarWithRef sourceRef)
                        TBottom
                peerTy =
                    TArrow
                        (tVarWithRef sameNamedPeer)
                        TBottom
                specialize projectedTy resultRef publishedTy =
                    AlgebraTestSupport.validatedBodyConsumerProjectionSpecializationForTest
                        route
                        sourceTy
                        projectedTy
                        resultRef
                        publishedTy
            specialize sourceTy constructionRef sourceTy
                `shouldBe` Right (Just InstId)
            specialize sourceTy semanticRef sourceTy
                `shouldBe` Right (Just InstId)
            specialize sourceTy otherConstructionRef sourceTy
                `shouldBe` Right Nothing
            specialize sourceTy constructionRef peerTy
                `shouldBe` Right Nothing
            specialize peerTy constructionRef sourceTy
                `shouldSatisfy` isLeft

        it "projects an established same-identity occurrence without publishing a root refinement" $ do
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        requirements
                        aliases
                    )
            let importedRef = typeRef 0 "__rigid0"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 7))
                        "a"
                declaredType =
                    TArrow
                        (tVarWithRef importedRef)
                        TBottom
                projectedType =
                    TArrow
                        (tVarWithRef sourceRef)
                        TBottom
                establishedRoute =
                    route
                        { AlgebraTestSupport.bcrtvSemanticRef =
                            constructionRef
                        , AlgebraTestSupport.bcrtvConstructionRef =
                            constructionRef
                        , AlgebraTestSupport.bcrtvOperatedType =
                            declaredType
                        }
                ambientBindings =
                    Map.singleton constructionRef declaredType
            (projectedBindings, publishedRootRefinement) <-
                case
                    AlgebraTestSupport.projectBodyConsumerBoundWithCertificateForTest
                        AlgebraTestSupport.DirectAmbientEstablished
                        []
                        establishedRoute
                        projectedType
                        projectedType
                        ambientBindings
                of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "established body-consumer projection failed"
                    Right projectionResult -> pure projectionResult
            Map.lookup constructionRef projectedBindings
                `shouldBe` Just projectedType
            publishedRootRefinement `shouldBe` False

        it "treats an exact self-operated unbounded consumer as identity specialization" $ do
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        requirements
                        aliases
                    )
            let selfType = tVarWithRef constructionRef
                selfRoute =
                    route
                        { AlgebraTestSupport.bcrtvOperatedType = selfType
                        }
            AlgebraTestSupport.validatedBodyConsumerProjectionSpecializationForTest
                selfRoute
                selfType
                selfType
                constructionRef
                TBottom
                `shouldBe` Right (Just InstId)
            AlgebraTestSupport.validatedBodyConsumerProjectionSpecializationForTest
                selfRoute
                selfType
                selfType
                semanticRef
                TBottom
                `shouldBe` Right (Just InstId)

        it "eliminates an exact projected body result before the construction quotient" $ do
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        requirements
                        aliases
                    )
            let sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 7))
                        "a"
                sameNamedSourcePeer =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 8))
                        "a"
                constructionSourceRef = typeRef 0 "a"
                sameNamedConstructionPeer = typeRef 1 "a"
                projectedBodyType =
                    TArrow
                        (tVarWithRef sourceRef)
                        TBottom
                staleProjectedBodyType =
                    TArrow
                        (tVarWithRef sameNamedSourcePeer)
                        TBottom
                constructionBodyType =
                    TArrow
                        (tVarWithRef constructionSourceRef)
                        TBottom
                staleConstructionBodyType =
                    TArrow
                        (tVarWithRef sameNamedConstructionPeer)
                        TBottom
                lambdaParamType = TBottom
            projectedBound <-
                case elabToBound projectedBodyType of
                    Left cause ->
                        expectationFailure cause
                            >> fail "projected body endpoint was not a bound"
                    Right bound -> pure bound
            staleProjectedBound <-
                case elabToBound staleProjectedBodyType of
                    Left cause ->
                        expectationFailure cause
                            >> fail "stale projected body endpoint was not a bound"
                    Right bound -> pure bound
            let constructedLambdaType resultRef bodyResultRef bound =
                    TForallRef
                        resultRef
                        (Just bound)
                        ( TArrow
                            lambdaParamType
                            (tVarWithRef bodyResultRef)
                        )
                expectedLambdaType bodyType =
                    TArrow lambdaParamType bodyType
                selectElimination
                    constructionOperatedType
                    completedType
                    targetType =
                        AlgebraTestSupport.validatedBodyConsumerLeadingEliminationForTest
                            route
                            projectedBodyType
                            projectedBodyType
                            constructionOperatedType
                            completedType
                            targetType
                exactCompletedType =
                    constructedLambdaType
                        constructionRef
                        constructionRef
                        projectedBound
                expectedType =
                    expectedLambdaType constructionBodyType
            selectElimination
                constructionBodyType
                exactCompletedType
                expectedType
                `shouldBe` Right (Just InstElim)
            selectElimination
                constructionBodyType
                ( constructedLambdaType
                    otherConstructionRef
                    otherConstructionRef
                    projectedBound
                )
                expectedType
                `shouldBe` Right Nothing
            selectElimination
                constructionBodyType
                ( constructedLambdaType
                    constructionRef
                    otherConstructionRef
                    projectedBound
                )
                expectedType
                `shouldBe` Right Nothing
            selectElimination
                constructionBodyType
                ( constructedLambdaType
                    constructionRef
                    constructionRef
                    staleProjectedBound
                )
                expectedType
                `shouldBe` Right Nothing
            selectElimination
                staleConstructionBodyType
                exactCompletedType
                expectedType
                `shouldBe` Right Nothing

        it "rejects missing, nested, ambiguous, and non-lambda selection authority" $ do
            let select selectedOwner selectedRequirements selectedAliases =
                    AlgebraTestSupport.selectBodyConsumerRouteForTest
                        selectedOwner
                        edgeId
                        selectedRequirements
                        selectedAliases
                resultOnlyAliases =
                    IntMap.singleton
                        (getNodeId result)
                        constructionRef
                nestedRequirements =
                    requirementsWith
                        [ requirementAt
                            (RequiredGammaAtNestedScope (GenRef (GenNodeId 8)))
                        ]
                mismatchedConstructionRequirements =
                    requirementsWith
                        [ requirementAt
                            (RequiredGammaAtConstructionScope (GenRef (GenNodeId 8)))
                        ]
                ambiguousRequirements =
                    requirementsWith
                        [ requirementAt RequiredGammaAtCurrentScope
                        , requirementAt RequiredGammaAtCurrentScope
                        ]
                nonLambdaOwner =
                    owner
                        { lgoConstructor = LocalApplicationGamma
                        }
            select owner requirements IntMap.empty `shouldSatisfy` isLeft
            select owner requirements resultOnlyAliases `shouldSatisfy` isLeft
            select owner nestedRequirements aliases `shouldSatisfy` isLeft
            select
                owner
                mismatchedConstructionRequirements
                aliases
                `shouldSatisfy` isLeft
            select owner ambiguousRequirements aliases `shouldSatisfy` isLeft
            select nonLambdaOwner requirements aliases `shouldSatisfy` isLeft
            select
                owner {lgoBoundaryEdge = otherEdgeId}
                requirements
                aliases
                `shouldSatisfy` isLeft

        it "rejects stale route facts and sibling packet authorities" $ do
            route <-
                requireRoute
                    ( AlgebraTestSupport.selectBodyConsumerRouteForTest
                        owner
                        edgeId
                        requirements
                        aliases
                    )
            (packet, generator1) <-
                preparePacketForOwner
                    initialIdentityGenerator
                    owner
                    edgeId
                    semanticRef
                    (packetInfo semanticRef)
            let validate selectedAliases selectedPacket selectedRoute =
                    AlgebraTestSupport.validateBodyConsumerRouteForTest
                        owner
                        edgeId
                        selectedAliases
                        selectedPacket
                        selectedRoute
            validate
                (IntMap.singleton (getNodeId exterior) otherConstructionRef)
                packet
                route
                `shouldSatisfy` isLeft
            validate
                aliases
                packet
                route {AlgebraTestSupport.bcrtvEdgeId = otherEdgeId}
                `shouldSatisfy` isLeft
            validate
                aliases
                packet
                route {AlgebraTestSupport.bcrtvOwner = otherOwner}
                `shouldSatisfy` isLeft
            validate
                ( IntMap.fromList
                    [ (getNodeId exterior, constructionRef)
                    , (getNodeId result, constructionRef)
                    ]
                )
                packet
                route {AlgebraTestSupport.bcrtvExteriorNode = result}
                `shouldSatisfy` isLeft
            (wrongIdentityPacket, generator2) <-
                preparePacketForOwner
                    generator1
                    owner
                    edgeId
                    otherSemanticRef
                    (packetInfo otherSemanticRef)
            validate aliases wrongIdentityPacket route `shouldSatisfy` isLeft
            (wrongOwnerPacket, generator3) <-
                preparePacketForOwner
                    generator2
                    otherOwner
                    edgeId
                    semanticRef
                    (packetInfo semanticRef)
            validate aliases wrongOwnerPacket route `shouldSatisfy` isLeft
            (wrongEdgePacket, _) <-
                preparePacketForOwner
                    generator3
                    owner
                    otherEdgeId
                    semanticRef
                    (packetInfo semanticRef)
            validate aliases wrongEdgePacket route `shouldSatisfy` isLeft

    describe "Gamma consumer packets" $ do
        let sigmaRef = typeRef 110 "a"
            resultRef = typeRef 111 "result"
            constructionOnlyRef = typeRef 112 "construction-only"
            edgeId = EdgeId 114
            gammaAuthority =
                GammaPacketAuthority
                    edgeId
                    (GenNodeId 114)
                    (typeBinderRefIdentity resultRef)
            producer = ownerKey 115 "g"
            sigmaType :: ElabType
            sigmaType =
                tForallWithRef sigmaRef Nothing
                    (TArrow (tVarWithRef sigmaRef) (tVarWithRef sigmaRef))
            sigmaBound :: BoundType
            sigmaBound =
                tForallWithRef sigmaRef Nothing
                    (TArrow (tVarWithRef sigmaRef) (tVarWithRef sigmaRef))
            constructionInfo =
                schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [(resultRef, Just sigmaBound)]
                        (TArrow sigmaType (tVarWithRef resultRef))
                    )
                    ( IntMap.fromList
                        [ (getNodeId resultRefNode, resultRef)
                        , (getNodeId constructionOnlyNode, constructionOnlyRef)
                        ]
                    )
            operatedInfo =
                schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [(sigmaRef, Nothing)]
                        (TArrow (tVarWithRef sigmaRef) (tVarWithRef sigmaRef))
                    )
                    (IntMap.singleton (getNodeId sigmaRefNode) sigmaRef)
            sigmaRefNode = NodeId 110
            resultRefNode = NodeId 111
            constructionOnlyNode = NodeId 112
            prepareGammaPacket =
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (GammaPacket gammaAuthority)
                        constructionInfo
                        operatedInfo
                of
                    Left err -> expectationFailure (show err) >> fail "Gamma packet preparation failed"
                    Right prepared -> pure prepared

        it "keeps construction and operated schemes distinct" $ do
            (packet, _) <- prepareGammaPacket
            subtermGeneralizationSchemeInfo packet `shouldBe` constructionInfo
            subtermGeneralizationOperatedSchemeInfo packet `shouldBe` operatedInfo
            subtermGeneralizationSchemeInfo packet `shouldNotBe` subtermGeneralizationOperatedSchemeInfo packet

        it "keeps exact sigma-id closed in every enclosing packet view" $ do
            let exactSigmaRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 731))
                        "a"
                exactSigmaType =
                    TForallRef exactSigmaRef Nothing
                        (TArrow (tVarWithRef exactSigmaRef) (tVarWithRef exactSigmaRef))
                exactSigmaBound :: BoundType
                exactSigmaBound =
                    TForallRef exactSigmaRef Nothing
                        (TArrow (tVarWithRef exactSigmaRef) (tVarWithRef exactSigmaRef))
                rawConstructionInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(resultRef, Just exactSigmaBound)]
                            (tVarWithRef resultRef)
                        )
                        (IntMap.singleton (getNodeId resultRefNode) resultRef)
                exactOperatedInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(exactSigmaRef, Nothing)]
                            (TArrow (tVarWithRef exactSigmaRef) (tVarWithRef exactSigmaRef))
                        )
                        IntMap.empty
            projectedConstructionInfo <-
                case
                    resolveConstructionSourceBindersInSchemeInfoExceptForTest
                        (Set.singleton (typeBinderRefIdentity exactSigmaRef))
                        id
                        (IntMap.singleton 283 exactSigmaRef)
                        rawConstructionInfo
                  of
                    Left err -> expectationFailure err >> fail "exact construction projection failed"
                    Right projected -> pure projected
            (packet, _) <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (GammaPacket gammaAuthority)
                        projectedConstructionInfo
                        exactOperatedInfo
                  of
                    Left err -> expectationFailure (show err) >> fail "exact Gamma packet preparation failed"
                    Right prepared -> pure prepared
            subtermGeneralizationSchemeInfo packet `shouldBe` rawConstructionInfo
            schemeToType
                (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                `shouldBe` exactSigmaType
            schemeToType (subtermGeneralizationGammaBoundScheme packet)
                `shouldBe` exactSigmaType

        it "leaves a packet-owned Gamma consumer pending only in the local construction view" $ do
            (packet, _) <- prepareGammaPacket
            let localConstruction =
                    siScheme
                        (subtermGeneralizationConsumerConstructionSchemeInfo packet)
                fullConstruction =
                    siScheme (subtermGeneralizationSchemeInfo packet)
            schemeBinderRefs fullConstruction
                `shouldSatisfy` any
                    ( \(ref, mbBound) ->
                        typeBinderRefsSameIdentity ref resultRef
                            && mbBound == Just sigmaBound
                    )
            schemeBinderRefs localConstruction
                `shouldSatisfy` any
                    ( \(ref, mbBound) ->
                        typeBinderRefsSameIdentity ref resultRef
                            && mbBound == Nothing
                    )

        it "leaves an identity-topology consumer pending until the checked child is constructed" $ do
            topologyAuthority <-
                requireTestTopologyConsumerAuthority
                    edgeId
                    (NodeId 109)
                    resultRefNode
            (packet, _) <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (TopologyConsumerPacket topologyAuthority)
                        constructionInfo
                        operatedInfo
                  of
                    Left err -> expectationFailure (show err) >> fail "topology packet preparation failed"
                    Right prepared -> pure prepared
            let localConstruction =
                    siScheme
                        (subtermGeneralizationConsumerConstructionSchemeInfo packet)
            schemeBinderRefs localConstruction
                `shouldSatisfy` any
                    ( \(ref, mbBound) ->
                        typeBinderRefsSameIdentity ref resultRef
                            && mbBound == Nothing
                    )

        it "constructs enclosing topology placement from prepared S'(operated)" $ do
            topologyAuthority <-
                requireTestTopologyConsumerAuthority
                    edgeId
                    (NodeId 109)
                    resultRefNode
            (packet, _) <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (TopologyConsumerPacket topologyAuthority)
                        constructionInfo
                        operatedInfo
                  of
                    Left err -> expectationFailure (show err) >> fail "topology packet preparation failed"
                    Right prepared -> pure prepared
            let pendingRoot =
                    mkElabSchemeWithRefs
                        [(resultRef, Nothing)]
                        (tVarWithRef resultRef)
            placed <-
                case
                    placeSubtermGeneralizationBinders
                        (Map.singleton producer packet)
                        pendingRoot
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "topology placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [(placedRef, Just placedBound)] -> do
                    placedRef `shouldSatisfy` typeBinderRefsSameIdentity resultRef
                    alphaEqType (tyToElab placedBound) sigmaType `shouldBe` True
                binders ->
                    expectationFailure
                        ("expected one prepared topology bound, got " ++ show binders)

        it "routes a frozen topology consumer only through its exact published graph key" $ do
            let outwardConsumerRef = typeRef 54 "result"
                packetRef = typeRef 39 "a"
                topologyInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] (tVarWithRef packetRef))
                        IntMap.empty
            topologyAuthority <-
                requireTestTopologyConsumerAuthority
                    (EdgeId 6)
                    (NodeId 39)
                    (NodeId 44)
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (TopologyConsumerPacket topologyAuthority)
                        topologyInfo
                        topologyInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "routed topology packet preparation failed"
                    Right (prepared, _) -> pure prepared
            placed <-
                case
                    placeSubtermGeneralizationBindersWithRoutes
                        (IntMap.singleton 44 outwardConsumerRef)
                        (Map.singleton (ownerKey 39 "owner") packet)
                        ( mkElabSchemeWithRefs
                            [(outwardConsumerRef, Nothing)]
                            (tVarWithRef outwardConsumerRef)
                        )
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "routed topology packet placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [(placedRef, Just placedBound)] -> do
                    placedRef
                        `shouldSatisfy` typeBinderRefsSameIdentity outwardConsumerRef
                    alphaEqType
                        (tyToElab placedBound)
                        (schemeToType (siScheme topologyInfo))
                        `shouldBe` True
                binders ->
                    expectationFailure
                        ("expected one routed topology bound, got " ++ show binders)

        it "requires the exact restored source-to-boundary proof for a topology authority" $ do
            let restorationEdgeId = EdgeId 6
                sourceScope = GenNodeId 4
                sourceBody = NodeId 39
                boundaryBody = NodeId 44
                frozenResult = NodeId 44
                lambdaNode = NodeId 46
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLambdaGamma
                        , lgoBoundaryEdge = restorationEdgeId
                        , lgoTermNode = lambdaNode
                        , lgoScope = GenRef sourceScope
                        }
                wrongRestoration =
                    IntMap.singleton
                        (getNodeId sourceBody)
                        (NodeId 45)
            case
                mkIdentityTopologyConsumerAuthority
                    wrongRestoration
                    restorationEdgeId
                    sourceScope
                    sourceBody
                    sourceScope
                    boundaryBody
                    frozenResult
                    owner
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "not restored to the paired boundary body root")
                other ->
                    expectationFailure
                        ("expected restoration-proof rejection, got " ++ show other)

        it "closes an exact Ga route through quotient-aligned and chained copies" $ do
            let sourceRoot = NodeId 39
                restoredRoot = NodeId 44
                alignedBase = NodeId 42
                constructionResult = NodeId 54
                constructionAlias = NodeId 56
                canonical node
                    | node == alignedBase = restoredRoot
                    | otherwise = node
                routeProvenance =
                    GaBindParents
                        { gaBindParentsBase = IntMap.empty
                        , gaBaseConstraint = emptyConstraint
                        , gaBaseToSolved =
                            IntMap.singleton
                                (getNodeId sourceRoot)
                                restoredRoot
                        , gaSolvedToBase =
                            IntMap.fromList
                                [ (getNodeId constructionResult, alignedBase)
                                , (getNodeId constructionAlias, constructionResult)
                                ]
                        , gaRestoredSchemeRootTargets =
                            IntMap.singleton
                                (getNodeId sourceRoot)
                                restoredRoot
                        , gaExpansionConstructionPlacements =
                            emptyExpansionConstructionPlacements
                        }
            gaConstructionRouteNodes canonical routeProvenance sourceRoot
                `shouldBe` [restoredRoot, constructionResult, constructionAlias]

        it "publishes a topology consumer from its canonical route and rejects conflicts" $ do
            let outwardConsumerRef = typeRef 54 "result"
                conflictingConsumerRef = typeRef 55 "other-result"
                packetRef = typeRef 39 "a"
                topologyInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] (tVarWithRef packetRef))
                        IntMap.empty
                topologyConstructionInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(outwardConsumerRef, Nothing)]
                            (tVarWithRef outwardConsumerRef)
                        )
                        (IntMap.singleton 56 outwardConsumerRef)
                conflictingConstructionInfo =
                    topologyConstructionInfo
                        { siSubstRefs =
                            IntMap.fromList
                                [ (44, conflictingConsumerRef)
                                , (56, outwardConsumerRef)
                                ]
                        }
                routeProvenance =
                    GaBindParents
                        { gaBindParentsBase = IntMap.empty
                        , gaBaseConstraint = emptyConstraint
                        , gaBaseToSolved = IntMap.singleton 44 (NodeId 56)
                        , gaSolvedToBase = IntMap.singleton 56 (NodeId 44)
                        , gaRestoredSchemeRootTargets = IntMap.empty
                        , gaExpansionConstructionPlacements =
                            emptyExpansionConstructionPlacements
                        }
                constructionRoute =
                    gaConstructionRouteNodes id routeProvenance
            topologyAuthority <-
                requireTestTopologyConsumerAuthority
                    (EdgeId 6)
                    (NodeId 39)
                    (NodeId 44)
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (TopologyConsumerPacket topologyAuthority)
                        topologyInfo
                        topologyInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "topology producer packet preparation failed"
                    Right (prepared, _) -> pure prepared
            published <-
                case
                    publishTopologyConsumerRoutesForTest
                        constructionRoute
                        (Map.singleton (ownerKey 39 "owner") packet)
                        topologyConstructionInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "topology consumer route publication failed"
                    Right schemeInfo -> pure schemeInfo
            IntMap.lookup 44 (schemeInfoBinderRefSubst published)
                `shouldBe` Just outwardConsumerRef
            case
                publishTopologyConsumerRoutesForTest
                    constructionRoute
                    (Map.singleton (ownerKey 39 "owner") packet)
                    conflictingConstructionInfo
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "conflicts with its construction provenance route")
                other ->
                    expectationFailure
                        ("expected topology route conflict, got " ++ show other)

        it "publishes a named-retained topology consumer through reverse solved-to-base provenance" $ do
            let outwardConsumerRef = typeRef 54 "result"
                packetRef = typeRef 39 "a"
                topologyInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] (tVarWithRef packetRef))
                        IntMap.empty
                topologyConstructionInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(outwardConsumerRef, Nothing)]
                            (tVarWithRef outwardConsumerRef)
                        )
                        (IntMap.singleton 56 outwardConsumerRef)
                routeProvenance =
                    GaBindParents
                        { gaBindParentsBase = IntMap.empty
                        , gaBaseConstraint = emptyConstraint
                        , gaBaseToSolved = IntMap.singleton 44 (NodeId 44)
                        , gaSolvedToBase = IntMap.singleton 56 (NodeId 44)
                        , gaRestoredSchemeRootTargets = IntMap.empty
                        , gaExpansionConstructionPlacements =
                            emptyExpansionConstructionPlacements
                        }
            topologyAuthority <-
                requireTestTopologyConsumerAuthority
                    (EdgeId 6)
                    (NodeId 39)
                    (NodeId 44)
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (TopologyConsumerPacket topologyAuthority)
                        topologyInfo
                        topologyInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "named-retained topology packet preparation failed"
                    Right (prepared, _) -> pure prepared
            published <-
                case
                    publishTopologyConsumerRoutesForTest
                        (gaConstructionRouteNodes id routeProvenance)
                        (Map.singleton (ownerKey 39 "owner") packet)
                        topologyConstructionInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "reverse topology route publication failed"
                    Right schemeInfo -> pure schemeInfo
            IntMap.lookup 44 (schemeInfoBinderRefSubst published)
                `shouldBe` Just outwardConsumerRef

        it "publishes an owned topology consumer at its exact source-lambda codomain" $ do
            let topologyEdgeId = EdgeId 6
                scopeRoot = GenNodeId 4
                frozenBodyRoot = NodeId 39
                boundaryBodyRoot = NodeId 44
                frozenResultRoot = NodeId 44
                lambdaNode = NodeId 46
                nestedLambdaNode = boundaryBodyRoot
                packetRef = typeRef 39 "a"
                firstParamRef = typeRef 48 "a"
                secondParamRef = typeRef 53 "evidence"
                outwardConsumerRef = typeRef 54 "result"
                constructionRoutes node
                    | node == frozenBodyRoot =
                        [NodeId 54, NodeId 56]
                    | otherwise = [node]
                topologyInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] (tVarWithRef packetRef))
                        (IntMap.singleton 44 packetRef)
                lambdaInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(outwardConsumerRef, Nothing)]
                            ( tForallWithRef firstParamRef Nothing
                                ( tForallWithRef secondParamRef Nothing
                                    ( TArrow
                                        (tVarWithRef firstParamRef)
                                        ( TArrow
                                            (tVarWithRef secondParamRef)
                                            (tVarWithRef outwardConsumerRef)
                                        )
                                    )
                                )
                            )
                        )
                        ( IntMap.fromList
                            [ (54, outwardConsumerRef)
                            , (56, outwardConsumerRef)
                            ]
                        )
                nestedLambda =
                    ALam
                        "evidence"
                        (ownerDetails 45 "evidence")
                        (NodeId 53)
                        scopeRoot
                        (AResolvedVar (ownerDetails 39 "g") "g" (NodeId 43))
                        (EdgeId 7)
                        nestedLambdaNode
                sourceLambda =
                    ALam
                        "x"
                        (ownerDetails 46 "x")
                        (NodeId 48)
                        scopeRoot
                        nestedLambda
                        topologyEdgeId
                        lambdaNode
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLambdaGamma
                        , lgoBoundaryEdge = topologyEdgeId
                        , lgoTermNode = lambdaNode
                        , lgoScope = GenRef scopeRoot
                        }
            topologyAuthority <-
                requireTopologyConsumerAuthority
                    topologyEdgeId
                    scopeRoot
                    frozenBodyRoot
                    scopeRoot
                    boundaryBodyRoot
                    frozenResultRoot
                    owner
            packet <-
                prepareTopologyPacketForTest
                    topologyAuthority
                    topologyInfo
            published <-
                case
                    publishSourceLambdaTopologyConsumerRouteForTest
                        Nothing
                        constructionRoutes
                        sourceLambda
                        (Map.singleton (ownerKey 39 "owner") packet)
                        lambdaInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-lambda topology publication failed"
                    Right schemeInfo -> pure schemeInfo
            IntMap.lookup 44 (schemeInfoBinderRefSubst published)
                `shouldBe` Just outwardConsumerRef
            IntMap.lookup 56 (schemeInfoBinderRefSubst published)
                `shouldBe` Just outwardConsumerRef

        it "publishes a returned-function consumer through its certified application result" $ do
            let topologyEdgeId = EdgeId 6
                functionEdgeId = EdgeId 3
                scopeRoot = GenNodeId 4
                frozenBodyRoot = NodeId 39
                boundaryBodyRoot = NodeId 44
                frozenResultRoot = NodeId 44
                lambdaNode = NodeId 46
                producerRoot = NodeId 13
                producerArrow = NodeId 49
                producerDomain = NodeId 48
                producerCodomain = NodeId 50
                constructionDomain = NodeId 51
                constructionRoot = NodeId 52
                outwardConsumerNode = NodeId 54
                outwardConsumerRef = typeRef 54 "result"
                returnedParameterRef = typeRef 53 "returned-parameter"
                lambdaParameterRef = typeRef 48 "parameter"
                functionConstruction =
                    GraftArrowResultConstruction
                        { grcEdgeId = functionEdgeId
                        , grcSourceRoot = producerRoot
                        , grcSourceBoundRoot = Just producerArrow
                        , grcSourceResultRoot = Just producerCodomain
                        , grcTargetRoot = NodeId 43
                        , grcTargetDomain = NodeId 41
                        , grcTargetCodomain = NodeId 42
                        , grcConstructionDomain = constructionDomain
                        , grcConstructionCodomain = constructionRoot
                        }
                producerConstraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ ( getNodeId producerRoot
                                  , TyArrow
                                        { tnId = producerRoot
                                        , tnDom = constructionDomain
                                        , tnCod = constructionRoot
                                        }
                                  )
                                , ( getNodeId producerArrow
                                  , TyArrow
                                        { tnId = producerArrow
                                        , tnDom = producerDomain
                                        , tnCod = producerCodomain
                                        }
                                  )
                                , ( getNodeId producerDomain
                                  , TyVar
                                        { tnId = producerDomain
                                        , tnBound = Nothing
                                        }
                                  )
                                , ( getNodeId producerCodomain
                                  , TyVar
                                        { tnId = producerCodomain
                                        , tnBound = Nothing
                                        }
                                  )
                                , ( getNodeId constructionDomain
                                  , TyVar
                                        { tnId = constructionDomain
                                        , tnBound = Nothing
                                        }
                                  )
                                , ( getNodeId constructionRoot
                                  , TyVar
                                        { tnId = constructionRoot
                                        , tnBound = Nothing
                                        }
                                  )
                                ]
                        , cGraftResultConstructions =
                            IntMap.singleton
                                (getEdgeId functionEdgeId)
                                functionConstruction
                        }
                routeProvenance =
                    GaBindParents
                        { gaBindParentsBase = IntMap.empty
                        , gaBaseConstraint = producerConstraint
                        , gaBaseToSolved = IntMap.empty
                        , gaSolvedToBase = IntMap.empty
                        , gaRestoredSchemeRootTargets = IntMap.empty
                        , gaExpansionConstructionPlacements =
                            emptyExpansionConstructionPlacements
                        }
                functionSite =
                    (mkInstantiationSite functionEdgeId producerRoot (NodeId 43))
                        { instantiationSiteTargetTopology =
                            ArrowInstantiationTarget
                                { instantiationArrowAllocatedDomain = NodeId 41
                                , instantiationArrowAllocatedCodomain = NodeId 42
                                , instantiationArrowDomain = NodeId 36
                                , instantiationArrowCodomain = boundaryBodyRoot
                                }
                        }
                argumentSite =
                    mkInstantiationSite
                        (EdgeId 4)
                        (NodeId 40)
                        (NodeId 41)
                sourceBody =
                    AApp
                        ( AResolvedVar
                            (ownerDetails 1 "make")
                            "make"
                            producerRoot
                        )
                        ( AResolvedVar
                            (ownerDetails 20 "x")
                            "x"
                            (NodeId 36)
                        )
                        functionSite
                        argumentSite
                        boundaryBodyRoot
                sourceLambda =
                    ALam
                        "x"
                        (ownerDetails 46 "x")
                        (NodeId 48)
                        scopeRoot
                        sourceBody
                        topologyEdgeId
                        lambdaNode
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLambdaGamma
                        , lgoBoundaryEdge = topologyEdgeId
                        , lgoTermNode = lambdaNode
                        , lgoScope = GenRef scopeRoot
                        }
                topologyInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(typeRef 39 "packet", Nothing)]
                            (tVarWithRef (typeRef 39 "packet"))
                        )
                        (IntMap.singleton 44 (typeRef 39 "packet"))
                lambdaScheme =
                    mkElabSchemeWithRefs
                        [(outwardConsumerRef, Nothing)]
                        ( tForallWithRef lambdaParameterRef Nothing
                            ( tForallWithRef returnedParameterRef Nothing
                                ( TArrow
                                    (tVarWithRef lambdaParameterRef)
                                    ( TArrow
                                        (tVarWithRef returnedParameterRef)
                                        (tVarWithRef outwardConsumerRef)
                                    )
                                )
                            )
                        )
                lambdaInfo =
                    schemeInfoFromRefSubst
                        lambdaScheme
                        (IntMap.singleton 54 outwardConsumerRef)
                constructionRoutes =
                    gaConstructionRouteNodes id routeProvenance
                certificate =
                    GeneralizedResultRoute
                        { grrOwnerTarget = lambdaNode
                        , grrTypeRoot = lambdaNode
                        , grrFrozenConsumer = frozenResultRoot
                        , grrConstructionRoot = constructionRoot
                        , grrBinderNode = outwardConsumerNode
                        , grrBinderRef = outwardConsumerRef
                        }
            topologyAuthority <-
                requireTopologyConsumerAuthority
                    topologyEdgeId
                    scopeRoot
                    frozenBodyRoot
                    scopeRoot
                    boundaryBodyRoot
                    frozenResultRoot
                    owner
            packet <-
                prepareTopologyPacketForTest
                    topologyAuthority
                    topologyInfo
            let packets = Map.singleton (ownerKey 39 "owner") packet
            sourceLambdaGeneralizedResultRouteRequestForTest
                routeProvenance
                sourceLambda
                packets
                `shouldBe`
                    Right
                        ( Just
                            GeneralizedResultRouteRequest
                                { grrrOwnerTarget = lambdaNode
                                , grrrFrozenConsumer = frozenResultRoot
                                , grrrConstructionRoot = constructionRoot
                                }
                        )
            published <-
                case
                    publishSourceLambdaTopologyConsumerRouteForTest
                        (Just certificate)
                        constructionRoutes
                        sourceLambda
                        packets
                        lambdaInfo
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "returned-function topology publication failed"
                    Right schemeInfo -> pure schemeInfo
            IntMap.lookup 44 (schemeInfoBinderRefSubst published)
                `shouldBe` Just outwardConsumerRef
            let wrongEdgeProvenance =
                    routeProvenance
                        { gaBaseConstraint =
                            producerConstraint
                                { cGraftResultConstructions =
                                    IntMap.singleton
                                        (getEdgeId (EdgeId 4))
                                        functionConstruction
                                }
                        }
            case
                sourceLambdaGeneralizedResultRouteRequestForTest
                    wrongEdgeProvenance
                    sourceLambda
                    packets
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "missing exact application graft construction")
                other ->
                    expectationFailure
                        ( "expected wrong-edge graft-construction rejection, got "
                            ++ show other
                        )
            let wrongSiteProvenance =
                    routeProvenance
                        { gaBaseConstraint =
                            producerConstraint
                                { cGraftResultConstructions =
                                    IntMap.singleton
                                        (getEdgeId functionEdgeId)
                                        functionConstruction
                                            { grcSourceRoot = producerArrow
                                            }
                                }
                        }
            case
                sourceLambdaGeneralizedResultRouteRequestForTest
                    wrongSiteProvenance
                    sourceLambda
                    packets
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "does not match the exact allocated application site")
                other ->
                    expectationFailure
                        ( "expected wrong graft/site rejection, got "
                            ++ show other
                        )
            let missingSourceAuthorityProvenance =
                    routeProvenance
                        { gaBaseConstraint =
                            producerConstraint
                                { cGraftResultConstructions =
                                    IntMap.singleton
                                        (getEdgeId functionEdgeId)
                                        functionConstruction
                                            { grcSourceResultRoot = Nothing
                                            }
                                }
                        }
            case
                sourceLambdaGeneralizedResultRouteRequestForTest
                    missingSourceAuthorityProvenance
                    sourceLambda
                    packets
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "has no source-bound result authority")
                other ->
                    expectationFailure
                        ( "expected missing source-bound result authority rejection, got "
                            ++ show other
                        )
            case
                publishSourceLambdaTopologyConsumerRouteForTest
                    ( Just
                        certificate
                            { grrBinderNode = NodeId 55
                            }
                    )
                    constructionRoutes
                    sourceLambda
                    packets
                    lambdaInfo
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "absent from the generalized routes")
                other ->
                    expectationFailure
                        ( "expected mismatched planner certificate rejection, got "
                            ++ show other
                        )
        it "rejects topology publication when source-lambda provenance changes" $ do
            let expectedEdge = EdgeId 6
                expectedScope = GenNodeId 4
                expectedFrozenBody = NodeId 39
                expectedBoundaryBody = NodeId 44
                frozenResult = NodeId 44
                expectedLambda = NodeId 46
                outwardConsumerRef = typeRef 54 "result"
                packetRef = typeRef 39 "a"
                topologyInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] (tVarWithRef packetRef))
                        IntMap.empty
                lambdaInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(outwardConsumerRef, Nothing)]
                            (TArrow TBottom (tVarWithRef outwardConsumerRef))
                        )
                        (IntMap.singleton 54 outwardConsumerRef)
                sourceLambda =
                    ALam
                        "x"
                        (ownerDetails 46 "x")
                        (NodeId 47)
                        expectedScope
                        (AResolvedVar (ownerDetails 39 "g") "g" expectedBoundaryBody)
                        expectedEdge
                        expectedLambda
                constructionRoutes node
                    | node == expectedFrozenBody = [NodeId 54]
                    | otherwise = [node]
                missingConstructionRoutes _ = [NodeId 55]
                authorityFor
                    authorityEdgeId
                    boundaryScopeRoot
                    boundaryBodyRoot
                    lambdaNode =
                    requireTopologyConsumerAuthority
                        authorityEdgeId
                        expectedScope
                        expectedFrozenBody
                        boundaryScopeRoot
                        boundaryBodyRoot
                        frozenResult
                        LocalGammaOwner
                            { lgoConstructor = LocalLambdaGamma
                            , lgoBoundaryEdge = authorityEdgeId
                            , lgoTermNode = lambdaNode
                            , lgoScope = GenRef boundaryScopeRoot
                            }
                expectRejected routes authority expectedDetail = do
                    packet <-
                        prepareTopologyPacketForTest
                            authority
                            topologyInfo
                    case
                        publishSourceLambdaTopologyConsumerRouteForTest
                            Nothing
                            routes
                            sourceLambda
                            (Map.singleton (ownerKey 39 "owner") packet)
                            lambdaInfo
                      of
                        Left (ValidationFailed messages) ->
                            messages
                                `shouldSatisfy` any
                                    (isInfixOf expectedDetail)
                        other ->
                            expectationFailure
                                ( "expected source-lambda topology rejection, got "
                                    ++ show other
                                )
            wrongEdge <-
                authorityFor
                    (EdgeId 7)
                    expectedScope
                    expectedBoundaryBody
                    expectedLambda
            expectRejected constructionRoutes wrongEdge "source body edge changed"
            wrongScope <-
                authorityFor
                    expectedEdge
                    (GenNodeId 5)
                    expectedBoundaryBody
                    expectedLambda
            expectRejected constructionRoutes wrongScope "source lambda scope changed"
            wrongBody <-
                authorityFor
                    expectedEdge
                    expectedScope
                    (NodeId 40)
                    expectedLambda
            expectRejected constructionRoutes wrongBody "source body root changed"
            wrongLambda <-
                authorityFor
                    expectedEdge
                    expectedScope
                    expectedBoundaryBody
                    (NodeId 48)
            expectRejected constructionRoutes wrongLambda "recorded lambda node changed"
            validAuthority <-
                authorityFor
                    expectedEdge
                    expectedScope
                    expectedBoundaryBody
                    expectedLambda
            expectRejected
                missingConstructionRoutes
                validAuthority
                "certified source root has no generalized construction route"

        it "rejects wrong, non-arrow, non-binder, ambiguous, and conflicting topology publications" $ do
            let topologyEdgeId = EdgeId 6
                scopeRoot = GenNodeId 4
                frozenBodyRoot = NodeId 39
                boundaryBodyRoot = NodeId 44
                frozenResultRoot = NodeId 44
                lambdaNode = NodeId 46
                packetRef = typeRef 39 "a"
                firstParamRef = typeRef 48 "a"
                secondParamRef = typeRef 53 "evidence"
                outwardConsumerRef = typeRef 54 "result"
                outwardConsumerAlias =
                    typeBinderRefFromIdentity
                        (typeBinderRefIdentity outwardConsumerRef)
                        "duplicate-result"
                conflictingRef = typeRef 55 "other-result"
                constructionRoutes node
                    | node == frozenBodyRoot =
                        [NodeId 54, NodeId 56]
                    | otherwise = [node]
                conflictingConstructionRoutes node
                    | node == frozenBodyRoot =
                        [NodeId 54, NodeId 55]
                    | otherwise = [node]
                topologyInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] (tVarWithRef packetRef))
                        IntMap.empty
                nestedLambda =
                    ALam
                        "evidence"
                        (ownerDetails 45 "evidence")
                        (NodeId 53)
                        scopeRoot
                        (AResolvedVar (ownerDetails 39 "g") "g" (NodeId 43))
                        (EdgeId 7)
                        boundaryBodyRoot
                sourceLambda =
                    ALam
                        "x"
                        (ownerDetails 46 "x")
                        (NodeId 48)
                        scopeRoot
                        nestedLambda
                        topologyEdgeId
                        lambdaNode
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLambdaGamma
                        , lgoBoundaryEdge = topologyEdgeId
                        , lgoTermNode = lambdaNode
                        , lgoScope = GenRef scopeRoot
                        }
                lambdaBodyWith codomain =
                    tForallWithRef firstParamRef Nothing
                        ( tForallWithRef secondParamRef Nothing
                            ( TArrow
                                (tVarWithRef firstParamRef)
                                ( TArrow
                                    (tVarWithRef secondParamRef)
                                    codomain
                                )
                            )
                        )
                validLambdaInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(outwardConsumerRef, Nothing)]
                            (lambdaBodyWith (tVarWithRef outwardConsumerRef))
                        )
                        (IntMap.singleton 54 outwardConsumerRef)
                wrongBinderCodomainInfo =
                    validLambdaInfo
                        { siScheme =
                            mkElabSchemeWithRefs
                                [(outwardConsumerRef, Nothing)]
                                (lambdaBodyWith (tVarWithRef conflictingRef))
                        }
                nonArrowInfo =
                    validLambdaInfo
                        { siScheme =
                            mkElabSchemeWithRefs
                                [(outwardConsumerRef, Nothing)]
                                ( tForallWithRef firstParamRef Nothing
                                    ( tForallWithRef secondParamRef Nothing
                                        (tVarWithRef outwardConsumerRef)
                                    )
                                )
                        }
                nonBinderCodomainInfo =
                    validLambdaInfo
                        { siScheme =
                            mkElabSchemeWithRefs
                                [(outwardConsumerRef, Nothing)]
                                (lambdaBodyWith TBottom)
                        }
                ambiguousCodomainInfo =
                    validLambdaInfo
                        { siScheme =
                            mkElabSchemeWithRefs
                                [ (outwardConsumerRef, Nothing)
                                , (outwardConsumerAlias, Nothing)
                                ]
                                (lambdaBodyWith (tVarWithRef outwardConsumerRef))
                        }
                conflictingInfo =
                    validLambdaInfo
                        { siSubstRefs =
                            IntMap.fromList
                                [ (54, outwardConsumerRef)
                                , ( getNodeId frozenResultRoot
                                  , conflictingRef
                                  )
                                ]
                        }
                conflictingConstructionInfo =
                    validLambdaInfo
                        { siSubstRefs =
                            IntMap.fromList
                                [ (54, outwardConsumerRef)
                                , (55, conflictingRef)
                                ]
                        }
                expectRejected routes source schemeInfo expectedDetail packet =
                    case
                        publishSourceLambdaTopologyConsumerRouteForTest
                            Nothing
                            routes
                            source
                            (Map.singleton (ownerKey 39 "owner") packet)
                            schemeInfo
                      of
                        Left (ValidationFailed messages) ->
                            messages
                                `shouldSatisfy` any
                                    (isInfixOf expectedDetail)
                        other ->
                            expectationFailure
                                ( "expected source-lambda topology rejection, got "
                                    ++ show other
                                )
            topologyAuthority <-
                requireTopologyConsumerAuthority
                    topologyEdgeId
                    scopeRoot
                    frozenBodyRoot
                    scopeRoot
                    boundaryBodyRoot
                    frozenResultRoot
                    owner
            packet <-
                prepareTopologyPacketForTest
                    topologyAuthority
                    topologyInfo
            expectRejected
                constructionRoutes
                (ALit (LInt 1) boundaryBodyRoot)
                validLambdaInfo
                "not at a source-lambda boundary"
                packet
            expectRejected
                constructionRoutes
                sourceLambda
                wrongBinderCodomainInfo
                "construction consumer is not one unique free scheme binder"
                packet
            expectRejected
                constructionRoutes
                sourceLambda
                nonArrowInfo
                "arrow spine ended before the exact source lambda"
                packet
            expectRejected
                constructionRoutes
                sourceLambda
                nonBinderCodomainInfo
                "construction consumer is not one unique free scheme binder"
                packet
            expectRejected
                constructionRoutes
                sourceLambda
                ambiguousCodomainInfo
                "construction consumer is not one unique free scheme binder"
                packet
            expectRejected
                constructionRoutes
                sourceLambda
                conflictingInfo
                "already routes to a different scheme binder"
                packet
            expectRejected
                conflictingConstructionRoutes
                sourceLambda
                conflictingConstructionInfo
                "conflicting generalized construction routes"
                packet

        it "keeps a non-alias Gamma consumer at its frozen exterior identity" $ do
            let pendingRef = typeRef 121 "pending"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 121))
                        "source"
                nonAliasType =
                    TArrow
                        (tVarWithRef sourceRef)
                        (tVarWithRef sourceRef)
                sourceRoutedInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] nonAliasType)
                        (IntMap.singleton 121 sourceRef)
                pendingAuthority =
                    gammaAuthority
                        { gpaConsumerIdentity = typeBinderRefIdentity pendingRef
                        }
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (GammaPacket pendingAuthority)
                        sourceRoutedInfo
                        sourceRoutedInfo
                of
                    Left err -> expectationFailure (show err) >> fail "non-alias packet preparation failed"
                    Right (prepared, _) -> pure prepared
            let pendingInfo =
                    subtermGeneralizationConsumerConstructionSchemeInfo packet
            IntMap.lookup 121 (siSubstRefs pendingInfo)
                `shouldBe` Just pendingRef
            schemeBinderRefs (siScheme pendingInfo)
                `shouldSatisfy` any
                    ( \(ref, mbBound) ->
                        typeBinderRefsSameIdentity ref pendingRef
                            && mbBound == Nothing
                    )

        it "uses a routed Gamma consumer only for an exact peer-variable alias" $ do
            let pendingRef = typeRef 122 "pending"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 122))
                        "source"
                aliasInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] (tVarWithRef sourceRef))
                        (IntMap.singleton 122 sourceRef)
                pendingAuthority =
                    gammaAuthority
                        { gpaConsumerIdentity = typeBinderRefIdentity pendingRef
                        }
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (GammaPacket pendingAuthority)
                        aliasInfo
                        aliasInfo
                of
                    Left err -> expectationFailure (show err) >> fail "alias packet preparation failed"
                    Right (prepared, _) -> pure prepared
            let pendingInfo =
                    subtermGeneralizationConsumerConstructionSchemeInfo packet
            IntMap.lookup 122 (siSubstRefs pendingInfo)
                `shouldBe` Just sourceRef
            subtermGeneralizationResultAbstractionRef packet
                `shouldBe` Just pendingRef
            subtermGeneralizationConstructionResultAbstractionRef packet
                `shouldBe` Just sourceRef
            schemeBinderRefs (siScheme pendingInfo)
                `shouldSatisfy` any
                    ( \(ref, mbBound) ->
                        typeBinderRefsSameIdentity ref sourceRef
                            && mbBound == Nothing
                    )

        it "does not capture a packet result binder in its enclosing operated bound" $ do
            (packet, _) <- prepareGammaPacket
            consumerBoundScheme <-
                case
                    subtermGeneralizationGammaBoundSchemeForConsumer
                        (typeBinderRefIdentity resultRef)
                        (Map.singleton producer packet)
                of
                    Left err -> expectationFailure (show err) >> fail "consumer packet lookup failed"
                    Right Nothing -> expectationFailure "consumer packet was not found" >> fail "consumer packet lookup failed"
                    Right (Just scheme) -> pure scheme
            consumerBoundScheme `shouldBe` siScheme operatedInfo
            consumerBoundScheme `shouldNotBe` siScheme constructionInfo

        it "closes packet-owned variables that remain free in the operated view" $ do
            let openConstructionInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(sigmaRef, Nothing)] (tVarWithRef sigmaRef))
                        (IntMap.singleton (getNodeId sigmaRefNode) sigmaRef)
                openOperatedInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [] (tVarWithRef sigmaRef))
                        (IntMap.singleton (getNodeId sigmaRefNode) sigmaRef)
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( GammaPacket
                            gammaAuthority
                                { gpaConsumerIdentity = typeBinderRefIdentity sigmaRef
                                }
                        )
                        openConstructionInfo
                        openOperatedInfo
                of
                    Left err -> expectationFailure (show err) >> fail "open packet preparation failed"
                    Right (prepared, _) -> pure prepared
            consumerBoundScheme <-
                case
                    subtermGeneralizationGammaBoundSchemeForConsumer
                        (typeBinderRefIdentity sigmaRef)
                        (Map.singleton producer packet)
                of
                    Left err -> expectationFailure (show err) >> fail "open consumer packet lookup failed"
                    Right Nothing -> expectationFailure "open consumer packet was not found" >> fail "open consumer packet lookup failed"
                    Right (Just scheme) -> pure scheme
            consumerBoundScheme `shouldBe` siScheme openConstructionInfo

        it "leaves an enclosing K binder free in the constructed Gamma bound" $ do
            let innerRef = typeRef 118 "inner"
                outerRef = typeRef 119 "outer"
                consumerRef = typeRef 120 "result"
                kConstructionInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(innerRef, Nothing)]
                            (TArrow (tVarWithRef innerRef) (tVarWithRef outerRef))
                        )
                        IntMap.empty
                kOperatedInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(innerRef, Nothing), (outerRef, Nothing)]
                            (TArrow (tVarWithRef innerRef) (tVarWithRef outerRef))
                        )
                        IntMap.empty
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( EnclosingConsumerPacket
                            (typeBinderRefIdentity consumerRef)
                            (EdgeId 120)
                            testEnclosingConsumerOwner
                        )
                        kConstructionInfo
                        kOperatedInfo
                of
                    Left err -> expectationFailure (show err) >> fail "K packet preparation failed"
                    Right (prepared, _) -> pure prepared
            consumerBoundScheme <-
                case
                    subtermGeneralizationGammaBoundSchemeForConsumer
                        (typeBinderRefIdentity consumerRef)
                        (Map.singleton producer packet)
                of
                    Left err -> expectationFailure (show err) >> fail "K consumer packet lookup failed"
                    Right Nothing -> expectationFailure "K consumer packet was not found" >> fail "K consumer packet lookup failed"
                    Right (Just scheme) -> pure scheme
            consumerBoundScheme `shouldBe` siScheme kConstructionInfo

        it "retains the Gamma edge and its lexical owner as one authority" $ do
            (packet, _) <- prepareGammaPacket
            subtermGeneralizationGammaAuthority packet
                `shouldBe` Just gammaAuthority

        it "uses a Gamma consumer as the result abstraction" $ do
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        (GammaPacket gammaAuthority)
                        constructionInfo
                        operatedInfo
                of
                    Left err -> expectationFailure (show err) >> fail "Gamma packet preparation failed"
                    Right (prepared, _) -> pure prepared
            case subtermGeneralizationResultAbstractionRef packet of
                Just abstractionRef ->
                    typeBinderRefIdentity abstractionRef
                        `shouldBe` typeBinderRefIdentity resultRef
                Nothing -> expectationFailure "expected the Gamma consumer result abstraction"

        it "uses an exact enclosing Gamma consumer as the result abstraction" $ do
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( EnclosingConsumerPacket
                            (typeBinderRefIdentity resultRef)
                            edgeId
                            testEnclosingConsumerOwner
                        )
                        constructionInfo
                        operatedInfo
                of
                    Left err -> expectationFailure (show err) >> fail "consumer packet preparation failed"
                    Right (prepared, _) -> pure prepared
            case subtermGeneralizationResultAbstractionRef packet of
                Just abstractionRef ->
                    typeBinderRefIdentity abstractionRef
                        `shouldBe` typeBinderRefIdentity resultRef
                Nothing -> expectationFailure "expected the enclosing Gamma consumer result abstraction"
            case subtermGeneralizationConsumerAuthority packet of
                Just authority -> do
                    scaEdgeId authority `shouldBe` edgeId
                    scaConsumerIdentity authority
                        `shouldBe` typeBinderRefIdentity resultRef
                    subtermConsumerAuthorityEnclosingOwner authority
                        `shouldBe` Just testEnclosingConsumerOwner
                Nothing -> expectationFailure "expected enclosing consumer authority"

    describe "Prepared subterm generalization placement" $ do
        it "classifies only the exact result-lambda source declaration as locally emitted" $ do
            let edgeId = EdgeId 860
                lambdaNode = NodeId 861
                lambdaScope = GenNodeId 862
                parameterNode = NodeId 863
                bodyNode = NodeId 864
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 865))
                        "b"
                sameNamedPeer =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 866))
                        "b"
                consumerRef = typeRef 867 "result"
                owner =
                    LocalGammaOwner
                        { lgoConstructor = LocalLambdaGamma
                        , lgoBoundaryEdge = edgeId
                        , lgoTermNode = lambdaNode
                        , lgoScope = GenRef lambdaScope
                        }
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(sourceRef, Nothing), (consumerRef, Nothing)]
                            (tVarWithRef consumerRef)
                        )
                        IntMap.empty
                sourceLambda =
                    ALam
                        "evidence"
                        (ownerDetails 868 "evidence")
                        parameterNode
                        lambdaScope
                        (ALit (LInt 1) bodyNode)
                        edgeId
                        lambdaNode
            (packet, _) <-
                preparePacketForOwner
                    initialIdentityGenerator
                    owner
                    edgeId
                    consumerRef
                    packetInfo
            ownership <-
                case
                    subtermResultOwnershipFor
                        sourceLambda
                        (Map.singleton (ownerKey 868 "evidence") packet)
                of
                    Just resultOwnership -> pure resultOwnership
                    Nothing ->
                        expectationFailure "expected exact result-lambda ownership"
                            >> fail "result-lambda ownership was absent"
            subtermResultOwnershipLocalSourceDeclarationRefs
                (IntMap.singleton 865 sourceRef)
                ownership
                `shouldBe` [sourceRef]
            subtermResultOwnershipLocalSourceDeclarationRefs
                (IntMap.singleton 865 sameNamedPeer)
                ownership
                `shouldBe` []

        it "rejects source/canonical root count truncation" $ do
            let source = ALit (LInt 1) (NodeId 80)
            case pairSubtermGeneralizationRoots [source] [] of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "root count mismatch")
                other -> expectationFailure ("expected root-count validation failure, got " ++ show other)

        it "rejects source/canonical shape and resolved-owner drift" $ do
            let source = AResolvedVar (ownerDetails 81 "x") "x" (NodeId 81)
                shapeChanged = ALit (LInt 1) (NodeId 82)
                ownerChanged = AResolvedVar (ownerDetails 82 "x") "x" (NodeId 83)
            case pairSubtermGeneralizationRoots [source] [shapeChanged] of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "shape mismatch")
                other -> expectationFailure ("expected shape validation failure, got " ++ show other)
            case pairSubtermGeneralizationRoots [source] [ownerChanged] of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "owner identity mismatch")
                other -> expectationFailure ("expected owner validation failure, got " ++ show other)

        it "rejects duplicate resolved packet owners instead of choosing by traversal order" $ do
            let ref = typeRef 84 "a"
                packetInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(ref, Nothing)] (tVarWithRef ref))
                        IntMap.empty
                duplicateOwner = ownerKey 84 "owner"
            (packet, _) <- preparePacket initialIdentityGenerator (EdgeId 84) ref packetInfo
            let packets = Map.singleton duplicateOwner packet
            case mergeSubtermGeneralizations packets packets of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "duplicate prepared subterm generalization owners")
                other -> expectationFailure ("expected duplicate-owner failure, got " ++ show other)

        it "aligns packet free-reference payloads with their enclosing binder" $ do
            let outerRef = typeRef 70 "a"
                staleOuterRef = typeRef 70 "b"
                packetRef = typeRef 71 "inner"
                targetRef = typeRef 72 "result"
                packetScheme =
                    mkElabSchemeWithRefs
                        [(packetRef, Nothing)]
                        (TArrow (tVarWithRef packetRef) (tVarWithRef staleOuterRef))
                packetInfo :: SchemeInfo
                packetInfo = schemeInfoFromRefSubst packetScheme IntMap.empty
                owner = ownerKey 70 "owner"
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (outerRef, Nothing)
                        , (packetRef, Nothing)
                        , ( targetRef
                          , Just (TArrow (tVarWithRef packetRef) (tVarWithRef outerRef))
                          )
                        ]
                        (TArrow (tVarWithRef outerRef) (tVarWithRef targetRef))
            (packet, _) <- preparePacket initialIdentityGenerator (EdgeId 72) targetRef packetInfo
            placed <-
                case placeSubtermGeneralizationBinders (Map.singleton owner packet) rootScheme of
                    Left err -> expectationFailure (show err) >> fail "packet placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [ (placedOuterRef, Nothing)
                  , (placedTargetRef, Just (TForallRef placedPacketRef Nothing (TArrow packetDomain packetResult)))
                  ] -> do
                    placedOuterRef `shouldBe` outerRef
                    placedTargetRef `shouldBe` targetRef
                    typeBinderRefsSameIdentity placedPacketRef packetRef `shouldBe` False
                    packetDomain `shouldBe` tVarWithRef placedPacketRef
                    packetResult `shouldBe` tVarWithRef outerRef
                    case packetResult of
                        TVarRef resultRef -> typeBinderRefName resultRef `shouldBe` "a"
                        other -> expectationFailure ("expected packet result reference, got " ++ show other)
                other -> expectationFailure ("unexpected placed scheme binders: " ++ show other)

        it "projects a packet rigid free reference through its explicit source route" $ do
            let rigidRef = typeRef 200 "__rigid0"
                packetRef = typeRef 201 "inner"
                targetRef = typeRef 202 "result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 700))
                        "a"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef rigidRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (sourceRef, Nothing)
                        , (packetRef, Nothing)
                        , ( targetRef
                          , Just
                              ( TArrow
                                  (tVarWithRef packetRef)
                                  (tVarWithRef sourceRef)
                              )
                          )
                        ]
                        (tVarWithRef targetRef)
                sourceRoutes =
                    IntMap.singleton (getNodeId (NodeId 200)) sourceRef
            (packet0, _) <-
                preparePacket
                    initialIdentityGenerator
                    (EdgeId 202)
                    targetRef
                    packetInfo
            let packet =
                    withConstructionBinderRenames
                        [(sourceRef, rigidRef)]
                        packet0
            placed <-
                case
                    placeSubtermGeneralizationBindersWithRoutes
                        sourceRoutes
                        (Map.singleton (ownerKey 200 "owner") packet)
                        rootScheme
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "source-routed packet placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [ (placedSourceRef, Nothing)
                  , ( placedTargetRef
                    , Just
                        ( TForallRef
                            copiedPacketRef
                            Nothing
                            (TArrow packetDomain packetCodomain)
                          )
                    )
                  ] -> do
                    placedSourceRef `shouldSatisfy` typeBinderRefsSameIdentity sourceRef
                    placedTargetRef `shouldSatisfy` typeBinderRefsSameIdentity targetRef
                    copiedPacketRef
                        `shouldNotSatisfy` typeBinderRefsSameIdentity packetRef
                    packetDomain `shouldBe` tVarWithRef copiedPacketRef
                    packetCodomain `shouldBe` tVarWithRef sourceRef
                other ->
                    expectationFailure
                        ("unexpected source-routed placement: " ++ show other)

        it "keeps the construction identity when the enclosing scheme owns it lexically" $ do
            let rigidRef = typeRef 206 "__rigid0"
                packetRef = typeRef 207 "inner"
                targetRef = typeRef 208 "result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 702))
                        "a"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef rigidRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (packetRef, Nothing)
                        , ( targetRef
                          , Just
                              (TArrow (tVarWithRef packetRef) (tVarWithRef rigidRef))
                          )
                        ]
                        (TArrow (tVarWithRef rigidRef) (tVarWithRef targetRef))
            (packet0, _) <-
                preparePacket
                    initialIdentityGenerator
                    (EdgeId 208)
                    targetRef
                    packetInfo
            let packet =
                    withConstructionBinderRenames
                        [(sourceRef, rigidRef)]
                        packet0
            placed <-
                case
                    placeSubtermGeneralizationBinders
                        (Map.singleton (ownerKey 206 "owner") packet)
                        rootScheme
                  of
                    Left err ->
                        expectationFailure (show err)
                            >> fail "construction-routed packet placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [ ( placedTargetRef
                    , Just
                        ( TForallRef
                            copiedPacketRef
                            Nothing
                            (TArrow packetDomain packetCodomain)
                          )
                    )
                  ] -> do
                    placedTargetRef
                        `shouldSatisfy` typeBinderRefsSameIdentity targetRef
                    copiedPacketRef
                        `shouldNotSatisfy` typeBinderRefsSameIdentity packetRef
                    packetDomain `shouldBe` tVarWithRef copiedPacketRef
                    packetCodomain `shouldBe` tVarWithRef rigidRef
                other ->
                    expectationFailure
                        ("unexpected construction-routed placement: " ++ show other)

        it "rejects conflicting graph and source binders for one packet free-reference route" $ do
            let rigidRef = typeRef 203 "__rigid0"
                packetRef = typeRef 204 "inner"
                targetRef = typeRef 205 "result"
                sourceRef =
                    typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 701))
                        "a"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef rigidRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (sourceRef, Nothing)
                        , (rigidRef, Nothing)
                        , (packetRef, Nothing)
                        , ( targetRef
                          , Just
                              ( TArrow
                                  (tVarWithRef packetRef)
                                  (tVarWithRef sourceRef)
                              )
                          )
                        ]
                        (tVarWithRef targetRef)
                sourceRoutes =
                    IntMap.singleton (getNodeId (NodeId 203)) sourceRef
            (packet, _) <-
                preparePacket
                    initialIdentityGenerator
                    (EdgeId 205)
                    targetRef
                    packetInfo
            case
                placeSubtermGeneralizationBindersWithRoutes
                    sourceRoutes
                    (Map.singleton (ownerKey 203 "owner") packet)
                    rootScheme
              of
                Left (ValidationFailed messages) ->
                    messages
                        `shouldSatisfy` any
                            (isInfixOf "source route conflicts with an enclosing graph binder")
                other ->
                    expectationFailure
                        ("expected packet source-route conflict, got " ++ show other)

        it "retains an exact source bound when only the packet body matches" $ do
            let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 73)
                packetRef = typeBinderRefFromIdentity sourceIdentity "packet-a"
                sourceRef = typeBinderRefFromIdentity sourceIdentity "a"
                targetRef = typeRef 74 "result"
                exactBound =
                    TArrow
                        (tVarWithRef sourceRef)
                        (TArrow (tVarWithRef sourceRef) (tVarWithRef sourceRef))
                packetInfo =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs [(packetRef, Nothing)] exactBound)
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (packetRef, Nothing)
                        , (targetRef, Just exactBound)
                        ]
                        (tVarWithRef targetRef)
            (packet, _) <-
                preparePacket initialIdentityGenerator (EdgeId 74) targetRef packetInfo
            placed <-
                case
                    placeSubtermGeneralizationBinders
                        (Map.singleton (ownerKey 73 "owner") packet)
                        rootScheme
                of
                    Left err -> expectationFailure (show err) >> fail "packet placement failed"
                    Right scheme -> pure scheme
            schemeBinderRefs placed `shouldBe` [(targetRef, Just exactBound)]

        it "retains a specialized target bound when it matches the packet's leading instantiation" $ do
            let packetRef = typeRef 75 "a"
                targetRef = typeRef 76 "result"
                boolTy = TestElab.tBase (BaseTy "Bool")
                specializedBound = TArrow boolTy boolTy
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef packetRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (packetRef, Nothing)
                        , (targetRef, Just specializedBound)
                        ]
                        (tVarWithRef targetRef)
            (packet, _) <-
                preparePacket initialIdentityGenerator (EdgeId 76) targetRef packetInfo
            placed <-
                case
                    placeSubtermGeneralizationBinders
                        (Map.singleton (ownerKey 75 "owner") packet)
                        rootScheme
                of
                    Left err -> expectationFailure (show err) >> fail "packet placement failed"
                    Right scheme -> pure scheme
            schemeBinderRefs placed `shouldBe` [(targetRef, Just specializedBound)]

        it "selects an ambient exact specialization over a closed packet bound" $ do
            let packetRef = typeRef 175 "packet-result"
                boolTy = TestElab.tBase (BaseTy "Bool")
                packetScheme =
                    mkElabSchemeWithRefs
                        [(packetRef, Just boolTy)]
                        (TArrow boolTy (tVarWithRef packetRef))
                packetType = schemeToType packetScheme
                exactEndpoint = TArrow boolTy boolTy
            packetTypeSpecializesToExactEndpoint packetType exactEndpoint
                `shouldBe` True
            selectConstructionRequirementEndpoint
                (Just exactEndpoint)
                (Just packetScheme)
                `shouldBe` Just exactEndpoint

        it "preserves a packet forall when the exact edge owns only its body" $ do
            let packetRef = typeRef 176 "packet-result"
                packetBody =
                    TArrow
                        (tVarWithRef packetRef)
                        (tVarWithRef packetRef)
                packetScheme =
                    mkElabSchemeWithRefs
                        [(packetRef, Nothing)]
                        packetBody
            packetTypeSpecializesToExactEndpoint
                (schemeToType packetScheme)
                packetBody
                `shouldBe` False
            selectConstructionRequirementEndpoint
                (Just packetBody)
                (Just packetScheme)
                `shouldBe` Just (schemeToType packetScheme)

        it "allocates packet, consumer, and copied-bound names in construction order" $ do
            let outerRef = typeRef 170 "a"
                staleOuterRef = typeRef 170 "t170"
                packetRef = typeRef 171 "a"
                targetRef = typeRef 172 "b"
                boundPacketRef = typeRef 171 "b"
                packetScheme =
                    mkElabSchemeWithRefs
                        [(packetRef, Nothing)]
                        (TArrow (tVarWithRef packetRef) (tVarWithRef staleOuterRef))
                packetInfo = schemeInfoFromRefSubst packetScheme IntMap.empty
                localPacketInfo =
                    freshenSchemeInfoBinderNamesAgainst
                        (Set.singleton "a")
                        packetInfo
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (outerRef, Nothing)
                        , ( targetRef
                          , Just
                              ( TForallRef
                                  boundPacketRef
                                  Nothing
                                  ( TArrow
                                      (tVarWithRef boundPacketRef)
                                      (tVarWithRef outerRef)
                                  )
                              )
                          )
                        ]
                        (TArrow (tVarWithRef outerRef) (tVarWithRef targetRef))
            (packet, _) <- preparePacket initialIdentityGenerator (EdgeId 172) targetRef packetInfo
            localPacketRef <-
                case schemeBinderRefs (siScheme localPacketInfo) of
                    [(ref, Nothing)] -> pure ref
                    other -> expectationFailure ("unexpected local packet binders: " ++ show other) >> fail "packet naming failed"
            placed <-
                case
                    placeSubtermGeneralizationBinders
                        (Map.singleton (ownerKey 170 "owner") packet)
                        rootScheme
                of
                    Left err -> expectationFailure (show err) >> fail "packet placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [ (placedOuterRef, Nothing)
                  , (placedTargetRef, Just (TForallRef copiedRef Nothing _))
                  ] -> do
                    typeBinderRefName placedOuterRef `shouldBe` "a"
                    typeBinderRefName localPacketRef `shouldBe` "b"
                    typeBinderRefName placedTargetRef `shouldBe` "c"
                    typeBinderRefName copiedRef `shouldBe` "d"
                    typeBinderRefsSameIdentity copiedRef packetRef `shouldBe` False
                other -> expectationFailure ("unexpected construction-order binders: " ++ show other)

        it "allocates distinct sibling copies while reusing one packet identity across term and type views" $ do
            let firstPacketRef = typeRef 100 "inner"
                firstTypeViewRef = typeRef 100 "type-inner"
                secondPacketRef = typeRef 101 "inner"
                firstTargetRef = typeRef 102 "first-result"
                secondTargetRef = typeRef 103 "second-result"
                packetInfo ref =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(ref, Nothing)]
                            (TArrow (tVarWithRef ref) (tVarWithRef ref))
                        )
                        IntMap.empty
                placementRoot packetRef targetRef =
                    mkElabSchemeWithRefs
                        [ (packetRef, Nothing)
                        , (targetRef, Just (TArrow (tVarWithRef packetRef) (tVarWithRef packetRef)))
                        ]
                        (tVarWithRef targetRef)
                place owner packet root =
                    case placeSubtermGeneralizationBinders (Map.singleton owner packet) root of
                        Left err -> expectationFailure (show err) >> fail "packet placement failed"
                        Right scheme -> pure scheme
            (firstPacket, generatorAfterFirst) <-
                preparePacket initialIdentityGenerator (EdgeId 102) firstTargetRef (packetInfo firstPacketRef)
            (secondPacket, _) <-
                preparePacket generatorAfterFirst (EdgeId 103) secondTargetRef (packetInfo secondPacketRef)
            firstTermScheme <-
                place
                    (ownerKey 100 "first-owner")
                    firstPacket
                    (placementRoot firstPacketRef firstTargetRef)
            firstTypeScheme <-
                place
                    (ownerKey 100 "first-owner")
                    firstPacket
                    (placementRoot firstTypeViewRef firstTargetRef)
            secondTermScheme <-
                place
                    (ownerKey 101 "second-owner")
                    secondPacket
                    (placementRoot secondPacketRef secondTargetRef)
            firstTermCopy <- requireSingleCopiedBinder firstTermScheme
            firstTypeCopy <- requireSingleCopiedBinder firstTypeScheme
            secondTermCopy <- requireSingleCopiedBinder secondTermScheme
            typeBinderRefsSameIdentity firstTermCopy firstTypeCopy `shouldBe` True
            typeBinderRefsSameIdentity firstTermCopy secondTermCopy `shouldBe` False
            typeBinderRefsSameIdentity firstTermCopy firstPacketRef `shouldBe` False
            typeBinderRefsSameIdentity secondTermCopy secondPacketRef `shouldBe` False

        it "rejects a prepared packet with no enclosing consumer" $ do
            let packetRef = typeRef 90 "inner"
                missingTargetRef = typeRef 91 "missing-result"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef packetRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [(packetRef, Nothing)]
                        (tVarWithRef packetRef)
            (packet, _) <- preparePacket initialIdentityGenerator (EdgeId 91) missingTargetRef packetInfo
            case placeSubtermGeneralizationBinders (Map.singleton (ownerKey 90 "owner") packet) rootScheme of
                Left (InstantiationError message) ->
                    message `shouldSatisfy` isInfixOf "no enclosing binder consumer"
                other -> expectationFailure ("expected missing-consumer failure, got " ++ show other)

        it "does not install a Gamma-owned packet into an enclosing scheme" $ do
            let packetRef = typeRef 190 "inner"
                gammaTargetRef = typeRef 191 "gamma-result"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef packetRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [(packetRef, Nothing)]
                        (tVarWithRef packetRef)
            packet <-
                case
                    prepareSubtermGeneralizationPacket
                        initialIdentityGenerator
                        ( GammaPacket
                            ( GammaPacketAuthority
                                (EdgeId 190)
                                (GenNodeId 190)
                                (typeBinderRefIdentity gammaTargetRef)
                            )
                        )
                        packetInfo
                        packetInfo
                of
                    Left err -> expectationFailure (show err) >> fail "Gamma packet preparation failed"
                    Right (prepared, _) -> pure prepared
            placeSubtermGeneralizationBinders
                (Map.singleton (ownerKey 190 "owner") packet)
                rootScheme
                `shouldBe` Right rootScheme

        it "uses the prepared consumer identity when multiple bounds have the packet shape" $ do
            let packetRef = typeRef 91 "inner"
                firstTarget = typeRef 92 "first-result"
                secondTarget = typeRef 93 "second-result"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef packetRef))
                        )
                        IntMap.empty
                packetShape = TArrow (tVarWithRef packetRef) (tVarWithRef packetRef)
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (packetRef, Nothing)
                        , (firstTarget, Just packetShape)
                        , (secondTarget, Just packetShape)
                        ]
                        (tVarWithRef secondTarget)
            (packet, _) <- preparePacket initialIdentityGenerator (EdgeId 92) firstTarget packetInfo
            placed <-
                case placeSubtermGeneralizationBinders (Map.singleton (ownerKey 91 "owner") packet) rootScheme of
                    Left err -> expectationFailure (show err) >> fail "packet placement failed"
                    Right scheme -> pure scheme
            case schemeBinderRefs placed of
                [ (placedFirst, Just TForallRef {})
                  , (placedSecond, Just secondBound)
                  ] -> do
                    placedFirst `shouldBe` firstTarget
                    placedSecond `shouldBe` secondTarget
                    secondBound `shouldBe` packetShape
                other -> expectationFailure ("unexpected explicit-consumer placement: " ++ show other)

        it "rejects packet free refs outside the target binder's enclosing lexical scope" $ do
            let packetRef = typeRef 94 "inner"
                targetRef = typeRef 95 "result"
                laterRef = typeRef 96 "later"
                packetInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(packetRef, Nothing)]
                            (TArrow (tVarWithRef packetRef) (tVarWithRef laterRef))
                        )
                        IntMap.empty
                rootScheme =
                    mkElabSchemeWithRefs
                        [ (packetRef, Nothing)
                        , (targetRef, Just (TArrow (tVarWithRef packetRef) (tVarWithRef packetRef)))
                        , (laterRef, Nothing)
                        ]
                        (tVarWithRef targetRef)
            (packet, _) <- preparePacket initialIdentityGenerator (EdgeId 95) targetRef packetInfo
            case placeSubtermGeneralizationBinders (Map.singleton (ownerKey 94 "owner") packet) rootScheme of
                Left (ValidationFailed messages) ->
                    messages `shouldSatisfy` any (isInfixOf "outside its enclosing lexical scheme")
                other -> expectationFailure ("expected packet free-reference failure, got " ++ show other)

    describe "Generalize shadow comparator" $ do
        it "accepts alpha-equivalent types" $ do
            let solvedTy = testTForall "a" Nothing (testTVar "a")
                baseTy = testTForall "b" Nothing (testTVar "b")
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects same structure when solved/base free identities differ" $ do
            let solvedTy = TArrow (testTVar "t14") (testTVar "t14")
                baseTy = TArrow (testTVar "a") (testTVar "a")
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed identity mismatch, got: " ++ show other)

        it "accepts same identity when display names differ" $ do
            let solvedRef = typeRef 40 "t14"
                baseRef = typeRef 40 "a"
                solvedTy = TArrow (tVarWithRef solvedRef) (tVarWithRef solvedRef)
                baseTy = TArrow (tVarWithRef baseRef) (tVarWithRef baseRef)
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects same-named free variables with different identities" $ do
            let solvedRef = typeRef 41 "a"
                baseRef = typeRef 42 "a"
                solvedTy = TArrow (tVarWithRef solvedRef) (tVarWithRef solvedRef)
                baseTy = TArrow (tVarWithRef baseRef) (tVarWithRef baseRef)
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed identity mismatch, got: " ++ show other)

        it "accepts nested forall body renaming without bounds" $ do
            let solvedTy =
                    testTForall "a" Nothing
                        (testTForall "b" Nothing (TArrow (testTVar "a") (testTVar "b")))
                baseTy =
                    testTForall "x" Nothing
                        (testTForall "y" Nothing (TArrow (testTVar "x") (testTVar "y")))
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "accepts nested forall renaming through explicit bounds and body" $ do
            let solvedFreeA = typeRef 50 "a"
                baseFreeX = typeRef 50 "x"
                solvedFreeB = typeRef 51 "b"
                baseFreeY = typeRef 51 "y"
                solvedA = typeRef 52 "a"
                baseX = typeRef 53 "x"
                solvedB = typeRef 54 "b"
                baseY = typeRef 55 "y"
                solvedTy =
                    tForallWithRef solvedA (Just (TArrow (tVarWithRef solvedFreeA) (tVarWithRef solvedFreeA)))
                        (tForallWithRef solvedB (Just (TestElab.tCon (BaseTy "Box") (tVarWithRef solvedFreeA :| [tVarWithRef solvedFreeB])))
                            (TArrow (tVarWithRef solvedB) (tVarWithRef solvedA)))
                baseTy =
                    tForallWithRef baseX (Just (TArrow (tVarWithRef baseFreeX) (tVarWithRef baseFreeX)))
                        (tForallWithRef baseY (Just (TestElab.tCon (BaseTy "Box") (tVarWithRef baseFreeX :| [tVarWithRef baseFreeY])))
                            (TArrow (tVarWithRef baseY) (tVarWithRef baseX)))
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects inconsistent free-variable reuse under renaming" $ do
            let solvedTy = TArrow (testTVar "a") (testTVar "b")
                baseTy = TArrow (testTVar "x") (testTVar "x")
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

        it "rejects non-bijective mapping reused across bound and body" $ do
            let solvedTy =
                    testTForall "a" (Just (TArrow (testTVar "a") (testTVar "a")))
                        (TArrow (testTVar "a") (testTVar "b"))
                baseTy =
                    testTForall "x" (Just (TArrow (testTVar "x") (testTVar "x")))
                        (TArrow (testTVar "x") (testTVar "x"))
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

        it "accepts renamed variables through constructor arguments" $ do
            let solvedTy =
                    testTForall "a" Nothing
                        (testTForall "b" Nothing (TestElab.tCon (BaseTy "Pair") (testTVar "a" :| [testTVar "b"])))
                baseTy =
                    testTForall "x" Nothing
                        (testTForall "y" Nothing (TestElab.tCon (BaseTy "Pair") (testTVar "x" :| [testTVar "y"])))
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects same-named type heads with different identities" $ do
            let solvedTy = TBaseWithIdentity (typeIdentity 991811) (BaseTy "Token")
                baseTy = TBaseWithIdentity (typeIdentity 991812) (BaseTy "Token")
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed identity mismatch, got: " ++ show other)

        it "keeps same-named base bounds separate when type head identities differ" $ do
            let leftRef = typeRef 991813 "a"
                rightRef = typeRef 991814 "b"
                leftBound = TBaseWithIdentity (typeIdentity 991815) (BaseTy "Token")
                rightBound = TBaseWithIdentity (typeIdentity 991816) (BaseTy "Token")
                ty =
                    tForallWithRef leftRef (Just leftBound) $
                        tForallWithRef rightRef (Just rightBound) $
                            TArrow (tVarWithRef leftRef) (tVarWithRef rightRef)
            simplifyAnnotationType ty `shouldBe` ty

        it "rejects semantic mismatch with shadow reify mismatch diagnostics" $ do
            let solvedTy = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
                baseTy = testTForall "a" Nothing (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

    describe "selectSolvedOrderWithShadow" $ do
        it "returns solved type when solved/base shadow comparison succeeds" $ do
            let solvedTy = testTForall "a" Nothing (testTVar "a")
                baseTy = testTForall "b" Nothing (testTVar "b")
            selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy

        it "returns solved output even when base output is alpha-equivalent but syntactically different" $ do
            let solvedTy = testTForall "a" Nothing (testTVar "a")
                baseTy = testTForall "z" Nothing (testTVar "z")
            selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy

        it "fails hard on solved/base shadow mismatch when base shadow is present" $ do
            let solvedTy = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
                baseTy = testTForall "a" Nothing (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
            case selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

        it "reports context and normalized type diagnostics on mismatch" $ do
            let solvedTy = testTVar "a"
                baseTy = TestElab.tBase (BaseTy "Int")
            case selectSolvedOrderWithShadow "generalizeAt:caseX" solvedTy (Just baseTy) of
                Left (ValidationFailed msgs) -> do
                    msgs `shouldSatisfy` any (isInfixOf "context=generalizeAt:caseX")
                    msgs `shouldSatisfy` any (isInfixOf "scopeRootC=")
                    msgs `shouldSatisfy` any (isInfixOf "typeRoot=")
                    msgs `shouldSatisfy` any (isInfixOf "binders=")
                    msgs `shouldSatisfy` any (isInfixOf "solved=")
                    msgs `shouldSatisfy` any (isInfixOf "base=")
                other ->
                    expectationFailure ("Expected ValidationFailed diagnostics, got: " ++ show other)

    describe "Instantiation inference strictness" $ do
        it "publishes an exact endpoint from complete checked source instantiation" $ do
            let refA = typeRef 8 "a"
                intTy = TestElab.tBase (BaseTy "Int")
                specializedIdentityTy = TArrow intTy intTy
                functionSchemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            []
                            (TArrow specializedIdentityTy intTy)
                        )
                        IntMap.empty
                argumentSchemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(refA, Nothing)]
                            (TArrow (tVarWithRef refA) (tVarWithRef refA))
                        )
                        IntMap.empty
            resolvedSourceApplicationArgumentEndpointForTest
                functionSchemeInfo
                argumentSchemeInfo
                `shouldBe` Just specializedIdentityTy

        it "does not preconstruct the paper g g endpoint from an open source domain" $ do
            let refA = typeRef 11 "a"
                gSchemeInfo =
                    schemeInfoFromRefSubst
                        ( mkElabSchemeWithRefs
                            [(refA, Nothing)]
                            (TArrow (tVarWithRef refA) (tVarWithRef refA))
                        )
                        IntMap.empty
            resolvedSourceApplicationArgumentEndpointForTest
                gSchemeInfo
                gSchemeInfo
                `shouldBe` Nothing

        it "eliminates an unused bounded quantifier at its bound" $ do
            let refA = typeRef 9 "a"
                intTy = TestElab.tBase (BaseTy "Int")
                body = TArrow intTy intTy
                source = tForallWithRef refA (Just body) body
            inferInstAppArgsFromSchemeRefs
                [(refA, Just body)]
                body
                body
                `shouldBe` Just [body]
            applyInstantiation source InstElim `shouldBe` Right body

        it "returns Nothing when a bounded body variable only matches via fallback recovery" $ do
            let refA = typeRef 10 "a"
            inferInstAppArgsFromSchemeRefs
                [(refA, Just (TestElab.tBase (BaseTy "Bool")))]
                (tVarWithRef refA)
                (TestElab.tBase (BaseTy "Int"))
                `shouldBe` Nothing

        it "rejects residual specialization when only the terminal result agrees" $ do
            let refB = typeRef 37 "b"
                unitTy = TestElab.tBase (BaseTy "Unit")
                intTy = TestElab.tBase (BaseTy "Int")
                boolTy = TestElab.tBase (BaseTy "Bool")
                io ty = TestElab.tCon (BaseTy "IO") (ty :| [])
                sourceTerminal = io (tVarWithRef refB)
                targetTerminal = io intTy
                sourceResidual =
                    TArrow
                        (TArrow unitTy sourceTerminal)
                        sourceTerminal
                inconsistentTarget =
                    TArrow
                        (TArrow unitTy (io boolTy))
                        targetTerminal
                specializedResidual =
                    TArrow
                        (TArrow unitTy (io unitTy))
                        targetTerminal
                generalizedRef = typeRef 38 "result"
                generalizedTarget =
                    TArrow
                        ( TArrow
                            unitTy
                            ( tForallWithRef
                                generalizedRef
                                (Just unitTy)
                                (io (tVarWithRef generalizedRef))
                            )
                        )
                        targetTerminal
            inferInstAppArgsFromSchemeRefsExact
                [(refB, Nothing)]
                sourceTerminal
                targetTerminal
                `shouldBe` Just [intTy]
            inferInstAppArgsFromSchemeRefsExact
                [(refB, Nothing)]
                sourceResidual
                inconsistentTarget
                `shouldBe` Nothing
            residualTopologyAgreesExact specializedResidual inconsistentTarget
                `shouldBe` False
            residualTopologyAgreesExact specializedResidual generalizedTarget
                `shouldBe` True

        it "infers one exact argument across equivalent Church mu presentations" $ do
            let refA = typeRef 39 "a"
                annotatedSelf = typeRef 40 "Unit_self"
                annotatedResult = typeRef 41 "Unit_result"
                annotatedUnit =
                    tMuWithRef annotatedSelf $
                        tForallWithRef annotatedResult Nothing $
                            TArrow
                                (tVarWithRef annotatedResult)
                                (tVarWithRef annotatedResult)
                instantiatedSelf = typeRef 42 "tUnit"
                instantiatedResult = typeRef 43 "tResult"
                instantiatedUnit =
                    tMuWithRef instantiatedSelf $
                        TArrow
                            (tVarWithRef instantiatedResult)
                            (tVarWithRef instantiatedResult)
                sourceBody =
                    TArrow
                        (tVarWithRef refA)
                        (tVarWithRef refA)
                exactTarget =
                    TArrow annotatedUnit instantiatedUnit
            inferInstAppArgsFromSchemeRefsExact
                [(refA, Nothing)]
                sourceBody
                exactTarget
                `shouldBe` Just [annotatedUnit]

        it "preserves identity refs during selective substitution walks" $ do
            let refA = typeRef 20 "a"
                refF = typeRef 21 "f"
                refM = typeRef 22 "m"
                ty :: ElabType
                ty =
                    tForallWithRef
                        refA
                        (Just (tVarAppWithRef refF (tVarWithRef refA :| [])))
                        (tMuWithRef refM (tVarWithRef refA))
            substTypeSelectiveRefs [] Map.empty ty `shouldBe` ty

        it "infers instantiation args by type binder identity after display renames" $ do
            let refA = typeRef 30 "a"
                refA' = typeRef 30 "a1"
            inferInstAppArgsFromSchemeRefs
                [(refA, Nothing)]
                (tVarWithRef refA')
                (TestElab.tBase (BaseTy "Int"))
                `shouldBe` Just [TestElab.tBase (BaseTy "Int")]

        it "constructs eliminations when quantified refs reappear free in the target" $ do
            let refF = typeRef 35 "f"
                refA = typeRef 36 "a"
                body = tVarAppWithRef refF (tVarWithRef refA :| [])
                source = tForallWithRef refF Nothing (tForallWithRef refA Nothing body)
                expectedArgs = [tVarWithRef refF, tVarWithRef refA]
                expectedInst = InstSeq (InstApp (tVarWithRef refF)) (InstApp (tVarWithRef refA))
            inferInstAppArgsFromSchemeRefs
                [(refF, Nothing), (refA, Nothing)]
                body
                body
                `shouldBe` Just expectedArgs
            applyInstantiation source expectedInst `shouldBe` Right body

        it "does not infer instantiation args for same-named different identities" $ do
            let refA = typeRef 31 "a"
                refB = typeRef 32 "a"
            inferInstAppArgsFromSchemeRefs
                [(refA, Nothing)]
                (tVarWithRef refB)
                (TestElab.tBase (BaseTy "Int"))
                `shouldBe` Nothing

        it "does not selectively substitute same-named different identities" $ do
            let refA = typeRef 33 "a"
                refB = typeRef 34 "a"
                subst = Map.singleton refA (TestElab.tBase (BaseTy "Int"))
            substTypeSelectiveRefs [] subst (tVarWithRef refB)
                `shouldBe` tVarWithRef refB

    describe "inlineRigidTypes" $ do
        it "inlines rigid bounds by identity, not display name" $ do
            let refA = typeRef 60 "a"
                refARenamed = typeRef 60 "renamed"
                refB = typeRef 61 "a"
                rigidBounds = Map.singleton refA (TestElab.tBase (BaseTy "Int"))
                ty = TArrow (tVarWithRef refARenamed) (tVarWithRef refB)
            inlineRigidTypes rigidBounds ty
                `shouldBe` TArrow (TestElab.tBase (BaseTy "Int")) (tVarWithRef refB)

        it "does not inline under a binder with the same identity" $ do
            let refA = typeRef 62 "a"
                rigidBounds = Map.singleton refA (TestElab.tBase (BaseTy "Int"))
                ty = tForallWithRef refA Nothing (tVarWithRef refA)
            inlineRigidTypes rigidBounds ty `shouldBe` ty
