{-# LANGUAGE DataKinds #-}

module MLF.Elab.Run.Generalize.Prepare.TestSupport (
    PreparedGeneralizationArtifactTestView(..),
    preparedGeneralizationArtifactTestView,
    applyPreparedRootBinderSubstForTest,
    prepareRootClosureSchemeForTest,
    prepareRootClosureSchemeWithOwnerFinalForTest,
    prepareRootClosureSchemeWithOwnerFinalAndApplicationsForTest,
    projectRootClosureSchemeWithOwnerFinalForTest,
    prepareLocalApplicationRootClosureForTest,
    prepareLocalApplicationRootConstructionScopeForTest,
    prepareProvisionalLocalGammaRootConstructionScopeForTest,
    prepareProvisionalLocalGammaRootConstructionScopeWithRequirementEvidenceForTest,
    prepareMatchedLocalGammaRootConstructionScopeForTest,
    projectPreparedRootFreeSourceDeclarationCopiesForTest,
    reconcileRootSourceBinderAliasesForTest,
    projectPreparedSourceBinderSubstExceptForTest,
    insertPreparedTermSourceBinderAliasForTest,
    prepareCompilerExactEdgePlansForTest,
    prepareCompilerExactRootBinderSubstForTest,
    prepareAnnotationExpectedTypesByEdgeForTest,
    alignSourceExpectedOperatedTypeForTest,
    publishTopologyConsumerRoutesForTest,
    publishSourceLambdaTopologyConsumerRouteForTest,
    sourceLambdaGeneralizedResultRouteRequestForTest,
    prepareElaborationExpansionConstructionPlacementsForTest,
    identityTopologyAncestryFailuresForTest,
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
    resolvedSourceApplicationArgumentEndpointForTest,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import MLF.Constraint.Presolution
    ( EdgeTrace
    , PresolutionView(..)
    )
import MLF.Constraint.Presolution.Construction (RawExpansionConstruction)
import MLF.Constraint.BindingUtil (firstGenAncestorFrom)
import MLF.Constraint.Presolution.Plan.Requirements
    ( GeneralizationRequirements(..)
    , RequiredGammaBinder
    )
import MLF.Constraint.Solve (SolveError)
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , Constraint
    , EdgeId
    , GenNodeId
    , NodeId(..)
    , NodeRef
    , cBindParents
    , getEdgeId
    , nodeRefKey
    , typeRef
    )
import MLF.Constraint.Types.Phase (Phase(Presolved))
import MLF.Elab.Elaborate.Algebra
    ( LocalGammaConstructionCertificate(..)
    , OwnerFinalConstruction
    )
import MLF.Elab.Generalize
    ( GaBindParents(..)
    , GeneralizedResultRoute
    , GeneralizedResultRouteRequest
    , LocalGammaClosure(..)
    , SubtermGeneralizations
    , publishSourceLambdaTopologyConsumerRoute
    , publishTopologyConsumerRoutes
    , sourceLambdaGeneralizedResultRouteRequest
    )
import MLF.Elab.Types
    ( BoundType
    , ElabError
    , ElabScheme
    , ElabType
    , SchemeInfo
    , TypeBinderIdentity
    , TypeBinderRef
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr)
import MLF.Elab.Run.Generalize.Prepare.Internal
    ( PreparedGeneralizationArtifact(..)
    , PreparedRootClosure(..)
    , preparedRootConstructionScopeAliases
    , preparedRootConstructionScopeBinders
    , preparedRootClosureScheme
    , preparedRootClosureAmbientBinderRefs
    , prepareRootConstructionScope
    , prepareRootConstructionScopeWithRequirementEvidence
    , requiredGammaBinderClosedLocally
    , requiredGammaBinderConstructionRef
    , prepareRootClosureScheme
    , prepareRootClosureSchemeWithAmbient
    , projectPreparedRootFreeSourceDeclarationCopies
    , reconcileRootSourceBinderAliases
    , alignSourceExpectedOperatedType
    , applyPreparedRootBinderSubst
    , quotientPreparedRootClosureIdentities
    , CompilerExactEdgePlan(..)
    , prepareCompilerExactEdgePlans
    , prepareCompilerExactRootBinderSubst
    , prepareAnnotationExpectedTypesByEdge
    , prepareElaborationExpansionConstructionPlacements
    , identityTopologyAncestryFailures
    , exactApplicationClosureOwnsRequirement
    , applicationCertificateOwnsRootRequirement
    , applicationCertificateOwnsAmbientRootRequirement
    , applicationCertificateDirectClaimOwnsPlanningRequirement
    , applicationCertificateCompletesProvisionalResultRequirement
    , applicationCertificateTransfersRootRequirementOwnership
    , applicationCertificateDischargesRootClosure
    , rootRequirementOwnershipAllowsLocalGammaClosure
    , validateLocalApplicationCertificates
    , unclaimedEdgesOutsideLocalGammaClosures
    , placeNestedRootRequirements
    , insertPreparedTermSourceBinderAlias
    , projectPreparedSourceBinderSubstExceptWithLocalKeys
    )
import MLF.Elab.Run.Generalize.Types
    ( expansionArgumentParentsToIntMap
    , expansionConstructionParentsToIntMap
    , expansionSemanticMetaParentsToIntMap
    )
import MLF.Elab.Run.Instantiation
    ( resolvedSourceApplicationArgumentEndpoint
    )
import qualified MLF.Elab.TypeCheck as TypeCheck

data PreparedGeneralizationArtifactTestView = PreparedGeneralizationArtifactTestView
    { preparedTestBaseConstraint :: Constraint 'Presolved
    , preparedTestGeneralizationConstraint :: Constraint 'Presolved
    , preparedTestExpansionArgumentScopes :: IntMap.IntMap GenNodeId
    , preparedTestExpansionSemanticMetaParents :: IntMap.IntMap (NodeRef, GenNodeId)
    , preparedTestExpansionConstructionParents :: IntMap.IntMap (NodeRef, BindFlag)
    , preparedTestSolvedToBase :: IntMap.IntMap NodeId
    , preparedTestCanonicalizeNode :: NodeId -> NodeId
    , preparedTestRedirects :: IntMap.IntMap NodeId
    , preparedTestSubtermGeneralizations :: Either ElabError SubtermGeneralizations
    }

exactApplicationClosureOwnsRequirementForTest
    :: GaBindParents 'Presolved
    -> [RequiredGammaBinder]
    -> LocalGammaClosure
    -> RequiredGammaBinder
    -> Bool
exactApplicationClosureOwnsRequirementForTest =
    exactApplicationClosureOwnsRequirement

applicationCertificateOwnsRootRequirementForTest
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateOwnsRootRequirementForTest =
    applicationCertificateOwnsRootRequirement

applicationCertificateOwnsAmbientRootRequirementForTest
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateOwnsAmbientRootRequirementForTest =
    applicationCertificateOwnsAmbientRootRequirement

applicationCertificateDirectClaimOwnsPlanningRequirementForTest
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateDirectClaimOwnsPlanningRequirementForTest =
    applicationCertificateDirectClaimOwnsPlanningRequirement

applicationCertificateCompletesProvisionalResultRequirementForTest
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateCompletesProvisionalResultRequirementForTest =
    applicationCertificateCompletesProvisionalResultRequirement

applicationCertificateTransfersRootRequirementOwnershipForTest
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> RequiredGammaBinder
    -> Bool
applicationCertificateTransfersRootRequirementOwnershipForTest =
    applicationCertificateTransfersRootRequirementOwnership

applicationCertificateDischargesRootClosureForTest
    :: NodeRef
    -> [RequiredGammaBinder]
    -> LocalGammaClosure
    -> LocalGammaConstructionCertificate
    -> Bool
applicationCertificateDischargesRootClosureForTest =
    applicationCertificateDischargesRootClosure

rootRequirementOwnershipAllowsLocalGammaClosureForTest
    :: GaBindParents 'Presolved
    -> NodeRef
    -> [LocalGammaConstructionCertificate]
    -> [RequiredGammaBinder]
    -> [RequiredGammaBinder]
    -> LocalGammaClosure
    -> Bool
rootRequirementOwnershipAllowsLocalGammaClosureForTest =
    rootRequirementOwnershipAllowsLocalGammaClosure

validateLocalApplicationCertificatesForTest
    :: (EdgeId -> NodeId -> Either ElabError NodeRef)
    -> AnnExpr
    -> IntMap.IntMap TypeBinderRef
    -> [LocalGammaConstructionCertificate]
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
validateLocalApplicationCertificatesForTest =
    validateLocalApplicationCertificates

unclaimedEdgesOutsideLocalGammaClosuresForTest
    :: IntMap.IntMap LocalGammaClosure
    -> [EdgeId]
    -> [EdgeId]
unclaimedEdgesOutsideLocalGammaClosuresForTest =
    unclaimedEdgesOutsideLocalGammaClosures

placeFrozenRootGammaRequirementsForTest
    :: GaBindParents 'Presolved
    -> NodeRef
    -> GeneralizationRequirements
    -> Either ElabError GeneralizationRequirements
placeFrozenRootGammaRequirementsForTest ga currentScope =
    placeNestedRootRequirements ga currentScope IntMap.empty

resolvedSourceApplicationArgumentEndpointForTest
    :: SchemeInfo
    -> SchemeInfo
    -> Maybe ElabType
resolvedSourceApplicationArgumentEndpointForTest =
    resolvedSourceApplicationArgumentEndpoint TypeCheck.emptyEnv

preparedGeneralizationArtifactTestView
    :: PreparedGeneralizationArtifact
    -> PreparedGeneralizationArtifactTestView
preparedGeneralizationArtifactTestView artifact =
    let GaBindParents
            { gaBaseConstraint = baseConstraint
            , gaSolvedToBase = solvedToBase
            } = pgaBindParentsGa artifact
        generalizationConstraint =
            pvCanonicalConstraint (pgaPresolutionView artifact)
        currentParents = cBindParents generalizationConstraint
        placements = pgaExpansionConstructionPlacements artifact
        currentOwner key =
            firstGenAncestorFrom currentParents (typeRef (NodeId key))
        argumentScopes =
            IntMap.mapMaybeWithKey
                (\key _placement -> currentOwner key)
                (expansionArgumentParentsToIntMap placements)
        semanticMetaParents =
            IntMap.mapMaybeWithKey
                (\key _placement -> do
                    owner <- currentOwner key
                    (parent, _flag) <-
                        IntMap.lookup
                            (nodeRefKey (typeRef (NodeId key)))
                            currentParents
                    pure (parent, owner)
                )
                (expansionSemanticMetaParentsToIntMap placements)
     in PreparedGeneralizationArtifactTestView
            { preparedTestBaseConstraint = baseConstraint
            , preparedTestGeneralizationConstraint = generalizationConstraint
            , preparedTestExpansionArgumentScopes = argumentScopes
            , preparedTestExpansionSemanticMetaParents = semanticMetaParents
            , preparedTestExpansionConstructionParents =
                expansionConstructionParentsToIntMap placements
            , preparedTestSolvedToBase = solvedToBase
            , preparedTestCanonicalizeNode = pgaAnnNodeCanonical artifact
            , preparedTestRedirects = pgaRedirects artifact
            , preparedTestSubtermGeneralizations = pgaSubtermGeneralizations artifact
            }

applyPreparedRootBinderSubstForTest
    :: IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either ElabError ElabScheme
applyPreparedRootBinderSubstForTest =
    applyPreparedRootBinderSubst "test result"

prepareRootClosureSchemeWithOwnerFinalForTest
    :: [LocalGammaClosure]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> OwnerFinalConstruction
    -> Either ElabError ElabScheme
prepareRootClosureSchemeWithOwnerFinalForTest closures subst scheme certificate =
    preparedRootClosureScheme
        <$> prepareRootClosureScheme
            Nothing
            closures
            []
            subst
            scheme
            (Just certificate)

prepareRootClosureSchemeWithOwnerFinalAndApplicationsForTest
    :: [LocalGammaClosure]
    -> [LocalGammaConstructionCertificate]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> OwnerFinalConstruction
    -> Either ElabError ElabScheme
prepareRootClosureSchemeWithOwnerFinalAndApplicationsForTest closures applicationCertificates subst scheme certificate =
    preparedRootClosureScheme
        <$> prepareRootClosureScheme
            Nothing
            closures
            applicationCertificates
            subst
            scheme
            (Just certificate)

projectRootClosureSchemeWithOwnerFinalForTest
    :: [LocalGammaClosure]
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> OwnerFinalConstruction
    -> Either ElabError ([TypeBinderRef], ElabScheme)
projectRootClosureSchemeWithOwnerFinalForTest
    closures
    subst
    preferredSubst
    scheme
    certificate = do
        closure <-
            prepareRootClosureScheme
                Nothing
                closures
                []
                subst
                scheme
                (Just certificate)
        projectedClosure <-
            quotientPreparedRootClosureIdentities
                preferredSubst
                closure
        pure
            ( preparedRootClosureAmbientBinderRefs projectedClosure
            , preparedRootClosureScheme projectedClosure
            )

prepareRootClosureSchemeForTest
    :: IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either ElabError ElabScheme
prepareRootClosureSchemeForTest subst scheme =
    preparedRootClosureScheme
        <$> prepareRootClosureScheme
            Nothing
            []
            []
            subst
            scheme
            Nothing

prepareLocalApplicationRootClosureForTest
    :: IntMap.IntMap TypeBinderRef
    -> [LocalGammaClosure]
    -> [LocalGammaConstructionCertificate]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either ElabError ElabScheme
prepareLocalApplicationRootClosureForTest sourceBinderRefs closures certificates subst scheme =
    preparedRootClosureScheme
        <$> prepareRootClosureSchemeWithAmbient
            []
            sourceBinderRefs
            Nothing
            closures
            certificates
            subst
            scheme
            Nothing

prepareLocalApplicationRootConstructionScopeForTest
    :: IntMap.IntMap TypeBinderRef
    -> [LocalGammaConstructionCertificate]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either
        ElabError
        ( [(TypeBinderRef, Maybe BoundType)]
        , IntMap.IntMap TypeBinderRef
        )
prepareLocalApplicationRootConstructionScopeForTest sourceBinderRefs certificates subst scheme = do
    closure <-
        prepareRootClosureSchemeWithAmbient
            []
            sourceBinderRefs
            Nothing
            []
            certificates
            subst
            scheme
            Nothing
    let localRouteKeys =
            IntSet.unions
                ( map
                    (IntMap.keysSet . lgccLocalBinderRoutes)
                    certificates
                )
        scope =
            prepareRootConstructionScope
                IntMap.empty
                localRouteKeys
                closure
                subst
    pure
        ( preparedRootConstructionScopeBinders scope
        , preparedRootConstructionScopeAliases scope
        )

prepareProvisionalLocalGammaRootConstructionScopeForTest
    :: [LocalGammaClosure]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> ( [(TypeBinderRef, Maybe BoundType)]
       , IntMap.IntMap TypeBinderRef
       )
prepareProvisionalLocalGammaRootConstructionScopeForTest closures subst scheme =
    ( preparedRootConstructionScopeBinders scope
    , preparedRootConstructionScopeAliases scope
    )
  where
    locallyClosedGammas =
        IntMap.fromList (zip [0 ..] closures)
    scope =
        prepareRootConstructionScope
            locallyClosedGammas
            IntSet.empty
            (PreparedWholeRootClosure [] scheme)
            subst

prepareProvisionalLocalGammaRootConstructionScopeWithRequirementEvidenceForTest
    :: [LocalGammaClosure]
    -> [TypeBinderRef]
    -> [(TypeBinderRef, Maybe BoundType)]
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either
        ElabError
        ( [(TypeBinderRef, Maybe BoundType)]
        , IntMap.IntMap TypeBinderRef
        )
prepareProvisionalLocalGammaRootConstructionScopeWithRequirementEvidenceForTest
    closures
    exactLocalRefs
    dependencyBinders
    dependencyAliases
    subst
    scheme = do
        scope <-
            prepareRootConstructionScopeWithRequirementEvidence
                locallyClosedGammas
                IntSet.empty
                exactLocalRefs
                dependencyBinders
                dependencyAliases
                (PreparedWholeRootClosure (map fst dependencyBinders) scheme)
                subst
        pure
            ( preparedRootConstructionScopeBinders scope
            , preparedRootConstructionScopeAliases scope
            )
      where
        locallyClosedGammas =
            IntMap.fromList (zip [0 ..] closures)

prepareMatchedLocalGammaRootConstructionScopeForTest
    :: GaBindParents 'Presolved
    -> [LocalGammaClosure]
    -> GeneralizationRequirements
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either
        ElabError
        ( [(TypeBinderRef, Maybe BoundType)]
        , IntMap.IntMap TypeBinderRef
        )
prepareMatchedLocalGammaRootConstructionScopeForTest ga closures requirements subst scheme = do
    classified <-
        traverse
            (\requirement -> do
                closedLocally <-
                    requiredGammaBinderClosedLocally
                        ga
                        locallyClosedGammas
                        requirement
                pure (requirement, closedLocally)
            )
            (grRequiredGammaBinders requirements)
    exactLocalRefs <-
        traverse
            (requiredGammaBinderConstructionRef subst)
            [ requirement
            | (requirement, True) <- classified
            ]
    scope <-
        prepareRootConstructionScopeWithRequirementEvidence
            locallyClosedGammas
            IntSet.empty
            exactLocalRefs
            []
            IntMap.empty
            (PreparedWholeRootClosure [] scheme)
            subst
    pure
        ( preparedRootConstructionScopeBinders scope
        , preparedRootConstructionScopeAliases scope
        )
  where
    locallyClosedGammas =
        IntMap.fromList
            [ (getEdgeId edgeId, closure)
            | closure <- closures
            , edgeId <- NonEmpty.toList (lgcEdgeIds closure)
            ]

projectPreparedRootFreeSourceDeclarationCopiesForTest
    :: Set.Set TypeBinderIdentity
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> ElabScheme
projectPreparedRootFreeSourceDeclarationCopiesForTest =
    projectPreparedRootFreeSourceDeclarationCopies

reconcileRootSourceBinderAliasesForTest
    :: [TypeBinderRef]
    -> [TypeBinderRef]
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
reconcileRootSourceBinderAliasesForTest =
    reconcileRootSourceBinderAliases

projectPreparedSourceBinderSubstExceptForTest
    :: Set.Set TypeBinderIdentity
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
projectPreparedSourceBinderSubstExceptForTest =
    projectPreparedSourceBinderSubstExceptWithLocalKeys

insertPreparedTermSourceBinderAliasForTest
    :: IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> (Int, TypeBinderRef)
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
insertPreparedTermSourceBinderAliasForTest =
    insertPreparedTermSourceBinderAlias

prepareCompilerExactEdgePlansForTest
    :: IntMap.IntMap ElabType
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap TypeBinderRef
    -> Either
        ElabError
        (IntMap.IntMap (ElabType, IntMap.IntMap TypeBinderRef))
prepareCompilerExactEdgePlansForTest exactTypes traces sourceBinderRefs =
    fmap
        ( IntMap.map
            (\plan -> (ceepExpectedType plan, ceepConstructionRefs plan))
        )
        (prepareCompilerExactEdgePlans exactTypes traces sourceBinderRefs)

prepareCompilerExactRootBinderSubstForTest
    :: ElabType
    -> ElabScheme
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
prepareCompilerExactRootBinderSubstForTest =
    prepareCompilerExactRootBinderSubst

prepareAnnotationExpectedTypesByEdgeForTest
    :: IntMap.IntMap ElabType
    -> [AnnExpr]
    -> Either ElabError (IntMap.IntMap ElabType, IntSet.IntSet)
prepareAnnotationExpectedTypesByEdgeForTest =
    prepareAnnotationExpectedTypesByEdge

alignSourceExpectedOperatedTypeForTest
    :: IntMap.IntMap TypeBinderRef
    -> SchemeInfo
    -> SchemeInfo
    -> ElabType
    -> Either ElabError (SchemeInfo, [(TypeBinderRef, TypeBinderRef)])
alignSourceExpectedOperatedTypeForTest =
    alignSourceExpectedOperatedType id

publishTopologyConsumerRoutesForTest
    :: (NodeId -> [NodeId])
    -> SubtermGeneralizations
    -> SchemeInfo
    -> Either ElabError SchemeInfo
publishTopologyConsumerRoutesForTest =
    publishTopologyConsumerRoutes

publishSourceLambdaTopologyConsumerRouteForTest
    :: Maybe GeneralizedResultRoute
    -> (NodeId -> [NodeId])
    -> AnnExpr
    -> SubtermGeneralizations
    -> SchemeInfo
    -> Either ElabError SchemeInfo
publishSourceLambdaTopologyConsumerRouteForTest =
    publishSourceLambdaTopologyConsumerRoute

sourceLambdaGeneralizedResultRouteRequestForTest
    :: GaBindParents p
    -> AnnExpr
    -> SubtermGeneralizations
    -> Either ElabError (Maybe GeneralizedResultRouteRequest)
sourceLambdaGeneralizedResultRouteRequestForTest =
    sourceLambdaGeneralizedResultRouteRequest

prepareElaborationExpansionConstructionPlacementsForTest
    :: Constraint p
    -> (NodeId -> NodeId)
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap RawExpansionConstruction
    -> Either
        SolveError
        ( IntMap.IntMap GenNodeId
        , IntMap.IntMap (NodeRef, GenNodeId)
        , IntMap.IntMap (NodeRef, BindFlag)
        )
prepareElaborationExpansionConstructionPlacementsForTest base adoptNode instCopyMap constructions = do
    placements <-
        prepareElaborationExpansionConstructionPlacements
            base
            adoptNode
            (\_ _ -> True)
            instCopyMap
            constructions
    let constructionParents = expansionConstructionParentsToIntMap placements
        constructionBindParents =
            IntMap.fromList
                [ (nodeRefKey (typeRef (NodeId key)), placement)
                | (key, placement) <- IntMap.toList constructionParents
                ]
        projectedParents =
            IntMap.union constructionBindParents (cBindParents base)
        ownerFor key =
            firstGenAncestorFrom projectedParents (typeRef (NodeId key))
        argumentScopes =
            IntMap.mapMaybeWithKey
                (\key _placement -> ownerFor key)
                (expansionArgumentParentsToIntMap placements)
        semanticParents =
            IntMap.mapMaybeWithKey
                (\key (parent, _flag) ->
                    case ownerFor key of
                        Just owner -> Just (parent, owner)
                        Nothing -> Nothing
                )
                (expansionSemanticMetaParentsToIntMap placements)
    pure (argumentScopes, semanticParents, constructionParents)

identityTopologyAncestryFailuresForTest
    :: BindParents
    -> GenNodeId
    -> NodeId
    -> NodeId
    -> [String]
identityTopologyAncestryFailuresForTest =
    identityTopologyAncestryFailures
