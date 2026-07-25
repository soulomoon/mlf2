{-# LANGUAGE DataKinds #-}
module ScopeSpec (spec) where

import IdentityTestSupport
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty(..))
import Test.Hspec

import qualified MLF.Constraint.Finalize as Finalize
import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import MLF.Constraint.Presolution.Plan.Context
    ( GaBindParents (..)
    , emptyExpansionConstructionPlacements
    )
import MLF.Constraint.Presolution.Plan.Requirements
    ( GeneralizationRequirements(..)
    , RequiredGammaBinder(..)
    , RequiredGammaPlacement(..)
    , emptyGeneralizationRequirements
    , placeCurrentGammaRequirementsAt
    )
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BaseTy(..)
    , BindingError(..)
    , Constraint(..)
    , EdgeId(..)
    , GenNode(..)
    , GenNodeId(..)
    , NodeId(..)
    , TyNode(..)
    , fromListGen
    , genRef
    , getNodeId
    , nodeRefKey
    , typeRef
    )
import MLF.Elab.Run.Scope
    ( ApplicationConstructionScopes (..)
    , applicationGeneralizationScopeForRequirements
    , bindingScopeRef
    , generalizeTargetNode
    , resolveCanonicalScope
    , resolveApplicationConstructionScopes
    , resolveConstructionScopeForNode
    , schemeBodyTarget
    )
import MLF.Elab.Types (Ty(TBottom))
import SpecUtil (emptyConstraint, nodeMapFromList)
import MLF.Constraint.Types.Phase (Phase(Raw))

spec :: Spec
spec = do
    describe "ga scope" $ do
        it "bindingScopeRef propagates binding tree cycle errors" $ do
            let root = NodeId 1
                cycleNode = NodeId 2
                constraint = cyclicConstraint root cycleNode
            bindingScopeRef constraint root
                `shouldSatisfy` isBindingCycleError

        it "resolveCanonicalScope propagates binding tree cycle errors" $ do
            let root = NodeId 1
                cycleNode = NodeId 2
                constraint = cyclicConstraint root cycleNode
                view = presolutionView constraint IntMap.empty
            resolveCanonicalScope constraint view IntMap.empty root
                `shouldSatisfy` isBindingCycleError

        it "same-domain fallback still resolves the nearest base gen" $ do
            let genId = GenNodeId 0
                root = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId root, TyVar {tnId = root, tnBound = Nothing})
                                ]
                        , cBindParents =
                            IntMap.singleton
                                (nodeRefKey (typeRef root))
                                (genRef genId, BindFlex)
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
                ga =
                    GaBindParents
                        { gaBindParentsBase = cBindParents constraint
                        , gaBaseConstraint = constraint
                        , gaBaseToSolved = IntMap.empty
                        , gaSolvedToBase = IntMap.empty
                        , gaRestoredSchemeRootTargets = IntMap.empty
                        , gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
                        }
            resolveConstructionScopeForNode id ga mempty root
                `shouldBe` Right (genRef genId)

        it "keeps an application occurrence scope separate from its unwrapped target scope" $ do
            let occurrenceGen = GenNodeId 0
                targetGen = GenNodeId 1
                applicationNode = NodeId 10
                targetNode = NodeId 11
                boundaryEdge = EdgeId 12
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ ( getNodeId applicationNode
                                  , TyVar
                                        { tnId = applicationNode
                                        , tnBound = Just targetNode
                                        }
                                  )
                                , ( getNodeId targetNode
                                  , TyVar
                                        { tnId = targetNode
                                        , tnBound = Nothing
                                        }
                                  )
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ ( nodeRefKey (typeRef applicationNode)
                                  , (genRef occurrenceGen, BindFlex)
                                  )
                                , ( nodeRefKey (typeRef targetNode)
                                  , (genRef targetGen, BindFlex)
                                  )
                                ]
                        , cGenNodes =
                            fromListGen
                                [ ( occurrenceGen
                                  , GenNode occurrenceGen [applicationNode]
                                  )
                                , ( targetGen
                                  , GenNode targetGen [targetNode]
                                  )
                                ]
                        }
                ga =
                    GaBindParents
                        { gaBindParentsBase = cBindParents constraint
                        , gaBaseConstraint = constraint
                        , gaBaseToSolved = IntMap.empty
                        , gaSolvedToBase = IntMap.empty
                        , gaRestoredSchemeRootTargets = IntMap.empty
                        , gaExpansionConstructionPlacements =
                            emptyExpansionConstructionPlacements
                        }
                unwrappedTarget =
                    generalizeTargetNode
                        (presolutionView constraint IntMap.empty)
                        applicationNode
            unwrappedTarget `shouldBe` targetNode
            resolveApplicationConstructionScopes
                id
                ga
                mempty
                boundaryEdge
                applicationNode
                unwrappedTarget
                `shouldBe`
                    Right
                        ApplicationConstructionScopes
                            { applicationOccurrenceScope =
                                genRef occurrenceGen
                            , applicationTargetGeneralizationScope =
                                genRef targetGen
                            }

        it "uses the unwrapped target scope when the application emits no Gamma" $ do
            let occurrenceScope = genRef (GenNodeId 4)
                targetScope = genRef (GenNodeId 2)
                scopes =
                    ApplicationConstructionScopes
                        { applicationOccurrenceScope = occurrenceScope
                        , applicationTargetGeneralizationScope = targetScope
                        }
            applicationGeneralizationScopeForRequirements
                scopes
                emptyGeneralizationRequirements
                `shouldBe` targetScope

        mapM_
            ( \(regression, occurrenceGen, targetGen) ->
                it
                    ( "carries the exact occurrence Gamma while generalizing the target for "
                        ++ regression
                    )
                    $ do
                        let occurrenceScope = genRef occurrenceGen
                            targetScope = genRef targetGen
                            scopes =
                                ApplicationConstructionScopes
                                    { applicationOccurrenceScope = occurrenceScope
                                    , applicationTargetGeneralizationScope = targetScope
                                    }
                            currentRequirement =
                                RequiredGammaBinder
                                    { rgbEdgeIds = EdgeId 1 :| []
                                    , rgbExteriorNode = NodeId 15
                                    , rgbOperatedRoot = NodeId 6
                                    , rgbResultRoots = NodeId 6 :| []
                                    , rgbOperatedType = TBottom
                                    , rgbExactOperatedOccurrenceRef = Nothing
                                    , rgbPlacement = RequiredGammaAtCurrentScope
                                    }
                            requirements =
                                emptyGeneralizationRequirements
                                    { grRequiredGammaBinders = [currentRequirement]
                                    }
                            placedRequirements =
                                placeCurrentGammaRequirementsAt
                                    occurrenceScope
                                    requirements
                        map
                            rgbPlacement
                            (grRequiredGammaBinders placedRequirements)
                            `shouldBe`
                                [ RequiredGammaAtConstructionScope
                                    occurrenceScope
                                ]
                        applicationGeneralizationScopeForRequirements
                            scopes
                            placedRequirements
                            `shouldBe` targetScope
            )
            [ ( "nested IO putStrLn"
              , GenNodeId 8
              , GenNodeId 7
              )
            , ( "recursive Nat construction"
              , GenNodeId 0
              , GenNodeId 1
              )
            , ( "closure-valued partial application"
              , GenNodeId 12
              , GenNodeId 2
              )
            , ( "eta-expanded closure alias"
              , GenNodeId 4
              , GenNodeId 2
              )
            ]

    describe "schemeBodyTarget" $ do
        it "keeps named non-scheme-root vars at the named node" $ do
            let genId = GenNodeId 0
                target = NodeId 1
                forallNode = NodeId 2
                body = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId target, TyVar { tnId = target, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TestTyBase body (BaseTy "Int") )
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef target), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [])]
                        }
            schemeBodyTarget (presolutionView constraint IntMap.empty) target
                `shouldBe` target

        it "generalizeTargetNode unwraps named non-scheme-root vars to their bound body" $ do
            let genId = GenNodeId 0
                target = NodeId 1
                forallNode = NodeId 2
                body = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId target, TyVar { tnId = target, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TestTyBase body (BaseTy "Int") )
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef target), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [])]
                        }
            generalizeTargetNode (presolutionView constraint IntMap.empty) target
                `shouldBe` body

        it "unwraps a scheme-root var to its bound target" $ do
            let genId = GenNodeId 0
                root = NodeId 10
                bound = NodeId 11
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyVar { tnId = root, tnBound = Just bound })
                            , (getNodeId bound, TyVar { tnId = bound, tnBound = Nothing })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef root), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
            schemeBodyTarget (presolutionView constraint IntMap.empty) root
                `shouldBe` bound

        it "unwraps a scheme-root var bound to forall to the forall body" $ do
            let genId = GenNodeId 0
                root = NodeId 20
                forallNode = NodeId 21
                body = NodeId 22
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyVar { tnId = root, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TestTyBase body (BaseTy "Bool") )
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef root), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
            schemeBodyTarget (presolutionView constraint IntMap.empty) root
                `shouldBe` body

        it "unwraps a canonical scheme-body alias to the shared bound body" $ do
            let genId = GenNodeId 0
                root = NodeId 30
                forallNode = NodeId 31
                body = NodeId 32
                aliasBound = NodeId 33
                aliasNode = NodeId 34
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyVar { tnId = root, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TestTyBase body (BaseTy "String") )
                            , (getNodeId aliasBound, TyVar { tnId = aliasBound, tnBound = Nothing })
                            , (getNodeId aliasNode, TyVar { tnId = aliasNode, tnBound = Just aliasBound })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef root), (genRef genId, BindFlex))
                                , (nodeRefKey (typeRef aliasNode), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
                uf = IntMap.singleton (getNodeId aliasBound) forallNode
            schemeBodyTarget (presolutionView constraint uf) aliasNode
                `shouldBe` body

isBindingCycleError :: Either BindingError a -> Bool
isBindingCycleError result = case result of
    Left (BindingCycleDetected _) -> True
    _ -> False

presolutionView :: Constraint 'Raw -> IntMap.IntMap NodeId -> PresolutionViewBoundary.PresolutionView 'Raw
presolutionView constraint uf =
    Finalize.presolutionViewFromSnapshot constraint uf

cyclicConstraint :: NodeId -> NodeId -> Constraint 'Raw
cyclicConstraint n1 n2 =
    let nodes = nodeMapFromList
            [ (getNodeId n1, TyVar { tnId = n1, tnBound = Nothing })
            , (getNodeId n2, TyVar { tnId = n2, tnBound = Nothing })
            ]
        bindParents = IntMap.fromList
            [ (nodeRefKey (typeRef n1), (typeRef n2, BindFlex))
            , (nodeRefKey (typeRef n2), (typeRef n1, BindFlex))
            ]
    in emptyConstraint
        { cNodes = nodes
        , cBindParents = bindParents
        }
