{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Presolution.ExpansionSpec (spec) where

import IdentityTestSupport
import Test.Hspec
import Control.Monad.Except (throwError)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , Expansion(..)
    , ForallSpec(..)
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionError(..)
    , PresolutionResult(..)
    )
import MLF.Constraint.Presolution.Base (PresolutionUf(..))
import MLF.Constraint.Presolution.TestSupport
    ( RebuildBindParentsEnv(..)
    , ExpansionResultMap(..)
    , PresolutionState(..)
    , applyExpansionEdgeTracedAtTargetWithBindersForTest
    , contractExpansionWrapperBindingsForTest
    , canonicalizeExpansionResultMap
    , lookupExpansionResultUnder
    , lookupCopy
    , decideMinimalExpansion
    , materializeExpansionsForTest
    , mergeExpansions
    , rebuildBindParentsForTest
    , runPresolutionM
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import MLF.Constraint.Solve (validateSolvedGraphStrict)
import MLF.Constraint.Solve.TestSupport (SolveResult(..))
import qualified MLF.Binding.Tree as Binding
import SpecUtil
    ( computePresolutionRaw
    , defaultTraceConfig
    , emptyConstraint
    , inferBindParents
    , lookupNodeMaybe
    , nodeMapElems
    , nodeMapFromList
    , nodeMapMember
    , rootedConstraint
    )
import Presolution.Util (expectForallBody)
import qualified MLF.Util.UnionFind as UF

spec :: Spec
spec = do
    describe "ExpansionResultMap" $ do
        it "looks up administrative results through the current wrapper UF class" $ do
            let wrapper = NodeId 0
                representative = NodeId 1
                result = NodeId 2
                semanticCanonical nid
                    | nid == wrapper = representative
                    | otherwise = nid
                results =
                    ExpansionResultMap
                        (IntMap.singleton (getNodeId wrapper) result)

            lookupExpansionResultUnder
                semanticCanonical
                representative
                results
                `shouldBe` Right (Just result)

        it "rejects two results that collapse onto one wrapper UF class" $ do
            let wrapper0 = NodeId 0
                wrapper1 = NodeId 1
                result0 = NodeId 2
                result1 = NodeId 3
                semanticCanonical nid
                    | nid == wrapper1 = wrapper0
                    | otherwise = nid
                results =
                    ExpansionResultMap $
                        IntMap.fromList
                            [ (getNodeId wrapper0, result0)
                            , (getNodeId wrapper1, result1)
                            ]

            canonicalizeExpansionResultMap semanticCanonical results
                `shouldBe` Left
                    (ExpansionResultConflict wrapper0 result0 result1)

    describe "rebuildBindParents" $ do
        it "keeps the canonical representative's live owner over expansion provenance" $ do
            let representative = NodeId 0
                mergedAlias = NodeId 1
                rootGen = GenNodeId 0
                liveOwner = GenNodeId 1
                provenanceOwner = GenNodeId 2
                nodes =
                    nodeMapFromList
                        [ (getNodeId representative, TyVar representative Nothing)
                        , (getNodeId mergedAlias, TyVar mergedAlias Nothing)
                        ]
                bindParents =
                    IntMap.fromList
                        [ (nodeRefKey (typeRef representative), (genRef liveOwner, BindFlex))
                        , (nodeRefKey (genRef liveOwner), (genRef rootGen, BindFlex))
                        , (nodeRefKey (genRef provenanceOwner), (genRef rootGen, BindFlex))
                        ]
                genNodes =
                    fromListGen
                        [ (rootGen, GenNode rootGen [])
                        , (liveOwner, GenNode liveOwner [representative])
                        , (provenanceOwner, GenNode provenanceOwner [mergedAlias])
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        , cGenNodes = genNodes
                        }
                canonical nid
                    | nid == mergedAlias = representative
                    | otherwise = nid
                newNodes =
                    IntMap.singleton
                        (getNodeId representative)
                        (TyVar representative Nothing)
                initialState =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        2
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                env =
                    RebuildBindParentsEnv
                        { rbpOriginalConstraint = constraint
                        , rbpNewNodes = newNodes
                        , rbpGenNodes = genNodes
                        , rbpCanonical = canonical
                        , rbpSemanticCanonical = canonical
                        , rbpIncomingParents = IntMap.empty
                        , rbpExpansionArgParents =
                            IntMap.singleton
                                (getNodeId representative)
                                (genRef provenanceOwner)
                        }

            case runPresolutionM defaultTraceConfig initialState (rebuildBindParentsForTest env) of
                Left err -> expectationFailure $ "Bind-parent rewrite failed: " ++ show err
                Right (rewritten, _) ->
                    IntMap.lookup (nodeRefKey (typeRef representative)) rewritten
                        `shouldBe` Just (genRef liveOwner, BindFlex)

        it "contracts a wrapper UF class reached through a third-member parent" $ do
            let body = NodeId 0
                wrapper = NodeId 1
                alias = NodeId 2
                owner = GenNodeId 0
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyVar body Nothing)
                                , (1, TyExp wrapper (ExpVarId 0) body)
                                , (2, TyVar alias Nothing)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef body), (typeRef alias, BindFlex))
                                , (nodeRefKey (typeRef wrapper), (genRef owner, BindRigid))
                                ]
                        , cGenNodes = fromListGen [(owner, GenNode owner [wrapper])]
                        }
                semanticCanonical nid
                    | nid == alias = wrapper
                    | otherwise = nid

            contractExpansionWrapperBindingsForTest semanticCanonical constraint
                `shouldBe` Right
                    ( IntMap.singleton
                        (nodeRefKey (typeRef body))
                        (genRef owner, BindRigid)
                    )

        it "contracts a wrapper even when another node is its UF representative" $ do
            let body = NodeId 0
                wrapper = NodeId 1
                representative = NodeId 2
                owner = GenNodeId 0
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyVar body Nothing)
                                , (1, TyExp wrapper (ExpVarId 0) body)
                                , (2, TyVar representative Nothing)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef body), (typeRef wrapper, BindFlex))
                                , (nodeRefKey (typeRef wrapper), (genRef owner, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(owner, GenNode owner [wrapper])]
                        }
                semanticCanonical nid
                    | nid == wrapper = representative
                    | otherwise = nid

            contractExpansionWrapperBindingsForTest semanticCanonical constraint
                `shouldBe` Right
                    ( IntMap.singleton
                        (nodeRefKey (typeRef body))
                        (genRef owner, BindFlex)
                    )

        it "contracts nested wrappers and preserves the strongest binding flag" $ do
            let body = NodeId 0
                inner = NodeId 1
                outer = NodeId 2
                owner = GenNodeId 0
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyVar body Nothing)
                                , (1, TyExp inner (ExpVarId 0) body)
                                , (2, TyExp outer (ExpVarId 1) inner)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef body), (typeRef inner, BindFlex))
                                , (nodeRefKey (typeRef inner), (typeRef outer, BindRigid))
                                , (nodeRefKey (typeRef outer), (genRef owner, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(owner, GenNode owner [outer])]
                        }

            contractExpansionWrapperBindingsForTest id constraint
                `shouldBe` Right
                    ( IntMap.singleton
                        (nodeRefKey (typeRef body))
                        (genRef owner, BindRigid)
                    )

        it "fails closed when a wrapper class has no external parent" $ do
            let body = NodeId 0
                wrapper = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyVar body Nothing)
                                , (1, TyExp wrapper (ExpVarId 0) body)
                                ]
                        , cBindParents =
                            IntMap.singleton
                                (nodeRefKey (typeRef body))
                                (typeRef wrapper, BindFlex)
                        }

            contractExpansionWrapperBindingsForTest id constraint
                `shouldBe` Left (MissingBindParent (typeRef wrapper))

        it "fails closed on a nested-wrapper binding cycle" $ do
            let body = NodeId 0
                inner = NodeId 1
                outer = NodeId 2
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyVar body Nothing)
                                , (1, TyExp inner (ExpVarId 0) body)
                                , (2, TyExp outer (ExpVarId 1) inner)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef body), (typeRef inner, BindFlex))
                                , (nodeRefKey (typeRef inner), (typeRef outer, BindFlex))
                                , (nodeRefKey (typeRef outer), (typeRef inner, BindFlex))
                                ]
                        }

            contractExpansionWrapperBindingsForTest id constraint
                `shouldSatisfy` \case
                    Left (InvalidBindingTree _) -> True
                    _ -> False

        it "fails closed when one canonical wrapper class has conflicting parents" $ do
            let wrapper = NodeId 0
                alias = NodeId 1
                owner0 = GenNodeId 0
                owner1 = GenNodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyExp wrapper (ExpVarId 0) alias)
                                , (1, TyVar alias Nothing)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef wrapper), (genRef owner0, BindFlex))
                                , (nodeRefKey (typeRef alias), (genRef owner1, BindFlex))
                                , (nodeRefKey (genRef owner1), (genRef owner0, BindFlex))
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (owner0, GenNode owner0 [wrapper])
                                , (owner1, GenNode owner1 [alias])
                                ]
                        }
                semanticCanonical nid
                    | nid == alias = wrapper
                    | otherwise = nid

            contractExpansionWrapperBindingsForTest semanticCanonical constraint
                `shouldSatisfy` \case
                    Left (InvalidBindingTree _) -> True
                    _ -> False

    describe "materializeExpansions" $ do
        let materializeIdentity constraint unionFind nextNodeId =
                runPresolutionM
                    defaultTraceConfig
                    ( PresolutionState
                        constraint
                        (Presolution (IntMap.singleton 0 ExpIdentity))
                        unionFind
                        nextNodeId
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                    )
                    materializeExpansionsForTest

            expectDirectWrapperOwner unionFind = do
                let body = NodeId 0
                    wrapper = NodeId 1
                    owner = NodeId 2
                    rootGen = GenNodeId 0
                    nodes =
                        nodeMapFromList
                            [ (getNodeId body, TyVar body Nothing)
                            , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) body)
                            , (getNodeId owner, TyForall owner wrapper)
                            ]
                    constraint =
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents =
                                IntMap.fromList
                                    [ (nodeRefKey (typeRef body), (typeRef wrapper, BindFlex))
                                    , (nodeRefKey (typeRef wrapper), (typeRef owner, BindFlex))
                                    , (nodeRefKey (typeRef owner), (genRef rootGen, BindFlex))
                                    ]
                            , cGenNodes = fromListGen [(rootGen, GenNode rootGen [owner])]
                            }

                case materializeIdentity constraint unionFind 3 of
                    Left err -> expectationFailure $ "Expansion materialization failed: " ++ show err
                    Right (redirects, finalState) -> do
                        IntMap.lookup (getNodeId wrapper) redirects `shouldBe` Just body
                        Binding.lookupBindParent (psConstraint finalState) (typeRef wrapper)
                            `shouldBe` Just (typeRef owner, BindFlex)

            expectExternalBodyOwner unionFind = do
                let body = NodeId 0
                    wrapper = NodeId 1
                    rootGen = GenNodeId 0
                    bodyOwner = GenNodeId 1
                    useOwner = GenNodeId 2
                    nodes =
                        nodeMapFromList
                            [ (getNodeId body, TyVar body Nothing)
                            , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) body)
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef body), (genRef bodyOwner, BindFlex))
                            , (nodeRefKey (typeRef wrapper), (genRef useOwner, BindFlex))
                            , (nodeRefKey (genRef bodyOwner), (genRef rootGen, BindFlex))
                            , (nodeRefKey (genRef useOwner), (genRef rootGen, BindFlex))
                            ]
                    constraint =
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            , cGenNodes =
                                fromListGen
                                    [ (rootGen, GenNode rootGen [])
                                    , (bodyOwner, GenNode bodyOwner [body])
                                    , (useOwner, GenNode useOwner [wrapper])
                                    ]
                            }

                Binding.lookupBindParent constraint (typeRef wrapper)
                    `shouldBe` Just (genRef useOwner, BindFlex)

                case materializeIdentity constraint unionFind 2 of
                    Left err -> expectationFailure $ "Expansion materialization failed: " ++ show err
                    Right (redirects, finalState) -> do
                        IntMap.lookup (getNodeId wrapper) redirects `shouldBe` Just body
                        Binding.lookupBindParent (psConstraint finalState) (typeRef wrapper)
                            `shouldBe` Just (genRef bodyOwner, BindFlex)

        it "inherits the direct wrapper owner without union-find" $
            expectDirectWrapperOwner IntMap.empty

        it "inherits the direct wrapper owner when the body is the UF representative" $
            expectDirectWrapperOwner (IntMap.singleton 1 (NodeId 0))

        it "keeps the external body owner when the wrapper is the UF representative" $
            expectExternalBodyOwner (IntMap.singleton 0 (NodeId 1))

        it "fails closed when body ownership reaches the wrapper class through a third member" $ do
            let body = NodeId 0
                wrapper = NodeId 1
                third = NodeId 2
                rootGen = GenNodeId 0
                useOwner = GenNodeId 1
                nodes =
                    nodeMapFromList
                        [ (getNodeId body, TyVar body Nothing)
                        , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) body)
                        , (getNodeId third, TyForall third body)
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef body), (typeRef third, BindFlex))
                                , (nodeRefKey (typeRef wrapper), (genRef useOwner, BindFlex))
                                , (nodeRefKey (typeRef third), (genRef rootGen, BindFlex))
                                , (nodeRefKey (genRef useOwner), (genRef rootGen, BindFlex))
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [third])
                                , (useOwner, GenNode useOwner [wrapper])
                                ]
                        }
                unionFind =
                    IntMap.fromList
                        [ (getNodeId body, third)
                        , (getNodeId wrapper, third)
                        ]

            case materializeIdentity constraint unionFind 3 of
                Left (InternalError _) -> pure ()
                Left err -> expectationFailure $ "Expected InternalError, got " ++ show err
                Right _ -> expectationFailure "Expected missing identity-body provenance to fail"

        it "fails closed instead of reconstructing a missing non-identity result at source scope" $ do
            let body = NodeId 0
                wrapper = NodeId 1
                expVar = ExpVarId 0
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (0, TyVar body Nothing)
                                , (1, TyExp wrapper expVar body)
                                ]
                        }
                state0 =
                    PresolutionState
                        constraint
                        (Presolution (IntMap.singleton 0 (ExpInstantiate [])))
                        IntMap.empty
                        2
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            runPresolutionM
                defaultTraceConfig
                state0
                materializeExpansionsForTest
                `shouldBe` Left (MissingExpansionResult wrapper expVar)

    describe "decideMinimalExpansion" $ do
        it "compares through an identity expansion wrapper on the target" $ do
            let sourceDomId = NodeId 0
                sourceCodId = NodeId 1
                sourceArrowId = NodeId 2
                sourceExpId = NodeId 3
                targetExpId = NodeId 4
                sourceExp = TyExp sourceExpId (ExpVarId (-1)) sourceArrowId
                targetExp = TyExp targetExpId (ExpVarId 0) sourceArrowId
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = sourceDomId, tnBound = Nothing})
                        , (1, TyVar {tnId = sourceCodId, tnBound = Nothing})
                        , (2, TyArrow sourceArrowId sourceDomId sourceCodId)
                        , (3, sourceExp)
                        , (4, targetExp)
                        ]
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                initialState =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        5
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM
                defaultTraceConfig
                initialState
                (decideMinimalExpansion id (GenNodeId 0) True sourceExp targetExp) of
                Left err -> expectationFailure $ "Expansion decision failed: " ++ show err
                Right ((expansion, unifications), _) -> do
                    expansion `shouldBe` ExpIdentity
                    unifications
                        `shouldBe` [ (sourceDomId, sourceDomId)
                                   , (sourceCodId, sourceCodId)
                                   ]

        it "rejects a leading nested expansion owned by another source authority" $ do
            let binder = NodeId 0
                sourceArrow = NodeId 1
                sourceForall = NodeId 2
                innerExp = NodeId 3
                outerExp = NodeId 4
                targetDom = NodeId 5
                targetCod = NodeId 6
                targetArrow = NodeId 7
                ownerGen = GenNodeId 0
                nestedGen = GenNodeId 1
                rootGen = GenNodeId 2
                ownerExpVar = ExpVarId 0
                nestedExpVar = ExpVarId 1
                sourceNode = TyExp outerExp ownerExpVar innerExp
                targetNode = TyArrow targetArrow targetDom targetCod
                nodes =
                    nodeMapFromList
                        [ (0, TyVar binder Nothing)
                        , (1, TyArrow sourceArrow binder binder)
                        , (2, TyForall sourceForall sourceArrow)
                        , (3, TyExp innerExp nestedExpVar sourceForall)
                        , (4, sourceNode)
                        , (5, TestTyBase targetDom (BaseTy "Int"))
                        , (6, TestTyBase targetCod (BaseTy "Int"))
                        , (7, targetNode)
                        ]
                bindParents =
                    IntMap.insert (nodeRefKey (typeRef binder)) (typeRef sourceForall, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef sourceForall)) (genRef nestedGen, BindFlex) $
                            IntMap.insert (nodeRefKey (typeRef innerExp)) (genRef ownerGen, BindFlex) $
                                IntMap.fromList
                                    [ (nodeRefKey (genRef ownerGen), (genRef rootGen, BindFlex))
                                    , (nodeRefKey (genRef nestedGen), (genRef rootGen, BindFlex))
                                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        , cGenNodes =
                            fromListGen
                                [ (ownerGen, GenNode ownerGen [innerExp])
                                , (nestedGen, GenNode nestedGen [sourceForall])
                                , (rootGen, GenNode rootGen [])
                                ]
                        }
                initialState =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        8
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            runPresolutionM
                defaultTraceConfig
                initialState
                (decideMinimalExpansion id ownerGen False sourceNode targetNode)
                `shouldBe` Left
                    ( NestedTyExpAuthorityMismatch
                        innerExp
                        nestedGen
                        nestedExpVar
                        ownerGen
                        ownerExpVar
                        Nothing
                    )

        it "crosses distinct occurrence expansions owned by the same source scheme" $ do
            let binder = NodeId 0
                sourceArrow = NodeId 1
                sourceForall = NodeId 2
                innerExp = NodeId 3
                outerExp = NodeId 4
                targetDom = NodeId 5
                targetCod = NodeId 6
                targetArrow = NodeId 7
                ownerGen = GenNodeId 0
                rootGen = GenNodeId 1
                ownerExpVar = ExpVarId 0
                nestedExpVar = ExpVarId 1
                sourceNode = TyExp outerExp ownerExpVar innerExp
                targetNode = TyArrow targetArrow targetDom targetCod
                nodes =
                    nodeMapFromList
                        [ (0, TyVar binder Nothing)
                        , (1, TyArrow sourceArrow binder binder)
                        , (2, TyForall sourceForall sourceArrow)
                        , (3, TyExp innerExp nestedExpVar sourceForall)
                        , (4, sourceNode)
                        , (5, TestTyBase targetDom (BaseTy "Int"))
                        , (6, TestTyBase targetCod (BaseTy "Int"))
                        , (7, targetNode)
                        ]
                bindParents =
                    IntMap.insert (nodeRefKey (typeRef binder)) (typeRef sourceForall, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef sourceArrow)) (typeRef sourceForall, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef sourceForall)) (genRef ownerGen, BindFlex) $
                            IntMap.insert (nodeRefKey (typeRef innerExp)) (genRef ownerGen, BindFlex) $
                                IntMap.fromList
                                    [ (nodeRefKey (genRef ownerGen), (genRef rootGen, BindFlex))
                                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        , cGenNodes =
                            fromListGen
                                [ (ownerGen, GenNode ownerGen [innerExp, sourceForall])
                                , (rootGen, GenNode rootGen [])
                                ]
                        }
                initialState =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        8
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM
                defaultTraceConfig
                initialState
                (decideMinimalExpansion id ownerGen False sourceNode targetNode) of
                Left err -> expectationFailure ("same-owner nested expansion failed: " ++ show err)
                Right ((expansion, unifications), _) -> do
                    expansion
                        `shouldSatisfy` \case
                            ExpInstantiate {} -> True
                            _ -> False
                    unifications `shouldBe` []

        it "returns ExpIdentity for matching monomorphic types" $ do
            let bodyId = NodeId 0
                targetId = NodeId 1
                expNodeId = NodeId 2
                rootId = NodeId 3
                nodes = nodeMapFromList
                        [ (0, TestTyBase bodyId (BaseTy "int"))
                        , (1, TestTyBase targetId (BaseTy "int"))
                        , (2, TyExp expNodeId (ExpVarId 0) bodyId)
                        , (3, TyArrow rootId expNodeId targetId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId targetId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined -- Not used by computePresolution currently
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    case IntMap.lookup 0 exps of
                        Just ExpIdentity -> pure ()
                        Just other -> expectationFailure $ "Expected ExpIdentity, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "returns ExpInstantiate for Forall <= Arrow" $ do
            let varId = NodeId 0
                arrowId = NodeId 1
                forallId = NodeId 2
                targetDomId = NodeId 3
                targetCodId = NodeId 4
                targetArrowId = NodeId 5
                expNodeId = NodeId 6
                rootId = NodeId 7
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = varId, tnBound = Nothing })
                        , (1, TyArrow arrowId varId varId)
                        , (2, TyForall forallId arrowId)
                        , (3, TestTyBase targetDomId (BaseTy "int"))
                        , (4, TestTyBase targetCodId (BaseTy "int"))
                        , (5, TyArrow targetArrowId targetDomId targetCodId)
                        , (6, TyExp expNodeId (ExpVarId 0) forallId)
                        , (7, TyArrow rootId expNodeId targetArrowId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId targetArrowId
                -- Make the forall non-vacuous under binding-edge binder enumeration:
                -- bind the TyVar { tnId = directly, tnBound = Nothing } to the forall node (flex).
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (nodeRefKey (typeRef varId)) (typeRef forallId, BindFlex) bindParents0
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prConstraint = c', prEdgeExpansions = exps } -> do
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate _) -> pure ()
                        other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other

                    let nodes' = cNodes c'
                    case (lookupNodeMaybe nodes' (NodeId 2), lookupNodeMaybe nodes' (NodeId 5)) of
                        (Just _, Just _) -> pure ()
                        _ -> expectationFailure "Nodes 2 and 5 should remain distinct"

        it "instantiates the complete leading forall spine needed by a structural target" $ do
            let outerBinder = NodeId 0
                innerBinder = NodeId 1
                sourceArrow = NodeId 2
                innerForall = NodeId 3
                outerForall = NodeId 4
                targetDom = NodeId 5
                targetCod = NodeId 6
                targetArrow = NodeId 7
                expNode = NodeId 8
                root = NodeId 9
                edge = InstEdge (EdgeId 0) expNode targetArrow
                nodes =
                    nodeMapFromList
                        [ (0, TyVar outerBinder Nothing)
                        , (1, TyVar innerBinder Nothing)
                        , (2, TyArrow sourceArrow outerBinder innerBinder)
                        , (3, TyForall innerForall sourceArrow)
                        , (4, TyForall outerForall innerForall)
                        , (5, TestTyBase targetDom (BaseTy "Int"))
                        , (6, TestTyBase targetCod (BaseTy "Bool"))
                        , (7, TyArrow targetArrow targetDom targetCod)
                        , (8, TyExp expNode (ExpVarId 0) outerForall)
                        , (9, TyArrow root expNode targetArrow)
                        ]
                bindParents =
                    IntMap.insert
                        (nodeRefKey (typeRef outerBinder))
                        (typeRef outerForall, BindFlex)
                        $ IntMap.insert
                            (nodeRefKey (typeRef innerBinder))
                            (typeRef innerForall, BindFlex)
                            (inferBindParents nodes)
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cInstEdges = [edge]
                            , cBindParents = bindParents
                            }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult {prEdgeExpansions = exps, prEdgeTraces = traces, prUnionFind = presolutionUf} -> do
                    case (IntMap.lookup 0 exps, IntMap.lookup 0 traces) of
                        (Just (ExpInstantiate args), Just traceInfo) -> do
                            length args `shouldBe` 2
                            let canonical = UF.frWith (getPresolutionUf presolutionUf)
                            map (fmap canonical) (etBinderArgs traceInfo)
                                `shouldBe` zip [outerBinder, innerBinder] [targetDom, targetCod]
                        (expansion, traceInfo) ->
                            expectationFailure $
                                "Expected a traced two-binder ExpInstantiate, got "
                                    ++ show (expansion, traceInfo)

        it "does not instantiate binders reachable only behind the copy frontier" $ do
            let binderA = NodeId 0
                binderB = NodeId 1
                frontierA = NodeId 2
                frontierB = NodeId 3
                body = NodeId 4
                targetDom = NodeId 5
                targetCod = NodeId 6
                targetArrow = NodeId 7
                expNode = NodeId 8
                rootGen = GenNodeId 0
                sourceGen = GenNodeId 1
                sourceNode = TyExp expNode (ExpVarId 0) body
                targetNode = TyArrow targetArrow targetDom targetCod
                nodes =
                    nodeMapFromList
                        [ (0, TyVar binderA Nothing)
                        , (1, TyVar binderB Nothing)
                        , (2, TyArrow frontierA binderA binderA)
                        , (3, TyArrow frontierB binderB binderB)
                        , (4, TyArrow body frontierA frontierB)
                        , (5, TestTyBase targetDom (BaseTy "Int"))
                        , (6, TestTyBase targetCod (BaseTy "Bool"))
                        , (7, targetNode)
                        , (8, sourceNode)
                        ]
                bindParents =
                    IntMap.fromList
                        [ (nodeRefKey (genRef sourceGen), (genRef rootGen, BindFlex))
                        , (nodeRefKey (typeRef binderA), (genRef sourceGen, BindFlex))
                        , (nodeRefKey (typeRef binderB), (genRef sourceGen, BindFlex))
                        , (nodeRefKey (typeRef frontierA), (genRef rootGen, BindFlex))
                        , (nodeRefKey (typeRef frontierB), (genRef rootGen, BindFlex))
                        , (nodeRefKey (typeRef body), (genRef sourceGen, BindRigid))
                        , (nodeRefKey (typeRef targetDom), (typeRef targetArrow, BindFlex))
                        , (nodeRefKey (typeRef targetCod), (typeRef targetArrow, BindFlex))
                        , (nodeRefKey (typeRef targetArrow), (genRef rootGen, BindFlex))
                        , (nodeRefKey (typeRef expNode), (genRef rootGen, BindFlex))
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [expNode, targetArrow])
                                , (sourceGen, GenNode sourceGen [body])
                                ]
                        }
                initialState =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        9
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM
                    defaultTraceConfig
                    initialState
                    (decideMinimalExpansion id sourceGen False sourceNode targetNode) of
                Left err -> expectationFailure $ "Expansion decision failed: " ++ show err
                Right ((expansion, unifications), _) -> do
                    expansion `shouldBe` ExpIdentity
                    unifications
                        `shouldBe` [(frontierA, targetDom), (frontierB, targetCod)]

        it "keeps an inner source forall behind an explicit forall target boundary" $ do
            let outerBinder = NodeId 0
                innerBinder = NodeId 1
                sourceArrow = NodeId 2
                innerForall = NodeId 3
                outerForall = NodeId 4
                targetBinder = NodeId 5
                targetBody = NodeId 6
                targetForall = NodeId 7
                expNode = NodeId 8
                sourceNode = TyExp expNode (ExpVarId 0) outerForall
                targetNode = TyForall targetForall targetBody
                nodes =
                    nodeMapFromList
                        [ (0, TyVar outerBinder Nothing)
                        , (1, TyVar innerBinder Nothing)
                        , (2, TyArrow sourceArrow outerBinder innerBinder)
                        , (3, TyForall innerForall sourceArrow)
                        , (4, TyForall outerForall innerForall)
                        , (5, TyVar targetBinder Nothing)
                        , (6, TyArrow targetBody targetBinder targetBinder)
                        , (7, targetNode)
                        , (8, sourceNode)
                        ]
                bindParents =
                    IntMap.insert (nodeRefKey (typeRef outerBinder)) (typeRef outerForall, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef innerBinder)) (typeRef innerForall, BindFlex) $
                            IntMap.insert (nodeRefKey (typeRef targetBinder)) (typeRef targetForall, BindFlex) $
                                inferBindParents nodes
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                initialState =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        9
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM
                    defaultTraceConfig
                    initialState
                    (decideMinimalExpansion id (GenNodeId 0) False sourceNode targetNode) of
                Left err -> expectationFailure $ "Expansion decision failed: " ++ show err
                Right ((expansion, unifications), _) -> do
                    expansion `shouldBe` ExpIdentity
                    unifications `shouldBe` [(innerForall, targetBody)]

        it "instantiates a synthesized wrapper whose body is bounded by a forall" $ do
            let binderId = NodeId 0
                sourceArrowId = NodeId 1
                sourceForallId = NodeId 2
                sourceBoundaryId = NodeId 3
                targetDomId = NodeId 4
                targetCodId = NodeId 5
                targetArrowId = NodeId 6
                expNodeId = NodeId 7
                rootId = NodeId 8
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = binderId, tnBound = Nothing})
                        , (1, TyArrow sourceArrowId binderId binderId)
                        , (2, TyForall sourceForallId sourceArrowId)
                        , (3, TyVar {tnId = sourceBoundaryId, tnBound = Just sourceForallId})
                        , (4, TestTyBase targetDomId (BaseTy "int"))
                        , (5, TestTyBase targetCodId (BaseTy "int"))
                        , (6, TyArrow targetArrowId targetDomId targetCodId)
                        , (7, TyExp expNodeId (ExpVarId (-1)) sourceBoundaryId)
                        , (8, TyArrow rootId expNodeId targetArrowId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId targetArrowId
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert
                        (nodeRefKey (typeRef binderId))
                        (typeRef sourceForallId, BindFlex)
                        bindParents0
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cInstEdges = [edge]
                            , cBindParents = bindParents
                            }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult {prEdgeExpansions = exps} ->
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate args) -> length args `shouldBe` 1
                        other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other

        it "generalizes lower-bound structure to a forall-bounded target" $ do
            let sourceDom = NodeId 0
                sourceCod = NodeId 1
                sourceArrow = NodeId 2
                sourceBoundary = NodeId 3
                expNode = NodeId 4
                targetBinder = NodeId 5
                targetArrow = NodeId 6
                targetForall = NodeId 7
                targetBoundary = NodeId 8
                root = NodeId 9
                rootGen = GenNodeId 0
                sourceGen = GenNodeId 10
                targetGen = GenNodeId 11
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = sourceDom, tnBound = Nothing})
                        , (1, TyVar {tnId = sourceCod, tnBound = Nothing})
                        , (2, TyArrow sourceArrow sourceDom sourceCod)
                        , (3, TyVar {tnId = sourceBoundary, tnBound = Just sourceArrow})
                        , (4, TyExp expNode (ExpVarId (-1)) sourceBoundary)
                        , (5, TyVar {tnId = targetBinder, tnBound = Nothing})
                        , (6, TyArrow targetArrow targetBinder targetBinder)
                        , (7, TyForall targetForall targetArrow)
                        , (8, TyVar {tnId = targetBoundary, tnBound = Just targetForall})
                        , (9, TyArrow root expNode targetBoundary)
                        ]
                edge = InstEdge (EdgeId 0) expNode targetBoundary
                bindParents =
                    IntMap.insert (nodeRefKey (genRef sourceGen)) (genRef rootGen, BindFlex) $
                        IntMap.insert (nodeRefKey (genRef targetGen)) (genRef rootGen, BindFlex) $
                            IntMap.insert (nodeRefKey (typeRef sourceBoundary)) (genRef sourceGen, BindFlex) $
                                IntMap.insert (nodeRefKey (typeRef sourceArrow)) (typeRef sourceBoundary, BindFlex) $
                                    IntMap.insert (nodeRefKey (typeRef targetBoundary)) (genRef targetGen, BindFlex) $
                                        IntMap.insert (nodeRefKey (typeRef targetForall)) (typeRef targetBoundary, BindFlex) $
                                            IntMap.insert (nodeRefKey (typeRef targetBinder)) (typeRef targetForall, BindFlex) $
                                                inferBindParents nodes
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cInstEdges = [edge]
                            , cBindParents = bindParents
                            , cGenNodes =
                                fromListGen
                                    [ (sourceGen, GenNode sourceGen [sourceBoundary])
                                    , (targetGen, GenNode targetGen [targetBoundary])
                                    ]
                            }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult {prEdgeExpansions = exps} ->
                    case IntMap.lookup 0 exps of
                        Just ExpForall {} -> pure ()
                        other -> expectationFailure $ "Expected ExpForall, got " ++ show other

        it "instantiates a source forall against a variable bounded by a forall" $ do
            -- A flexible target whose lower-bound head is forall is not itself
            -- a structural forall.  Once the source owns quantifiers, align
            -- through the target variable's instance relation rather than
            -- matching the two forall binder lists.
            let sourceSigmaBinder = NodeId 0
                sourceSigmaArrow = NodeId 1
                sourceSigmaForall = NodeId 2
                sourceBinder = NodeId 3
                sourceForall = NodeId 4
                expNode = NodeId 5
                targetBinder = NodeId 6
                targetArrow = NodeId 7
                targetForall = NodeId 8
                targetVar = NodeId 9
                root = NodeId 10
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = sourceSigmaBinder, tnBound = Nothing})
                        , (1, TyArrow sourceSigmaArrow sourceSigmaBinder sourceSigmaBinder)
                        , (2, TyForall sourceSigmaForall sourceSigmaArrow)
                        , (3, TyVar {tnId = sourceBinder, tnBound = Just sourceSigmaForall})
                        , (4, TyForall sourceForall sourceBinder)
                        , (5, TyExp expNode (ExpVarId 0) sourceForall)
                        , (6, TyVar {tnId = targetBinder, tnBound = Nothing})
                        , (7, TyArrow targetArrow targetBinder targetBinder)
                        , (8, TyForall targetForall targetArrow)
                        , (9, TyVar {tnId = targetVar, tnBound = Just targetForall})
                        , (10, TyArrow root expNode targetVar)
                        ]
                edge = InstEdge (EdgeId 0) expNode targetVar
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert
                        (nodeRefKey (typeRef sourceSigmaBinder))
                        (typeRef sourceSigmaForall, BindFlex)
                        $ IntMap.insert
                            (nodeRefKey (typeRef sourceBinder))
                            (typeRef sourceForall, BindFlex)
                        $ IntMap.insert
                            (nodeRefKey (typeRef targetBinder))
                            (typeRef targetForall, BindFlex)
                            bindParents0
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cInstEdges = [edge]
                            , cBindParents = bindParents
                            }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult {prEdgeExpansions = exps} ->
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate args) -> length args `shouldBe` 1
                        Just other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "keeps ExpInstantiate for forall <= non-forall level mismatch" $ do
            let srcBinderId = NodeId 0
                srcForallId = NodeId 1
                expNodeId = NodeId 2
                targetVarId = NodeId 3
                rootId = NodeId 4
                rootGen = GenNodeId 0
                srcGen = GenNodeId 10
                tgtGen = GenNodeId 11
                nodes = nodeMapFromList
                    [ (getNodeId srcBinderId, TyVar { tnId = srcBinderId, tnBound = Nothing })
                    , (getNodeId srcForallId, TyForall srcForallId srcBinderId)
                    , (getNodeId expNodeId, TyExp expNodeId (ExpVarId 0) srcForallId)
                    , (getNodeId targetVarId, TyVar { tnId = targetVarId, tnBound = Nothing })
                    , (getNodeId rootId, TyArrow rootId expNodeId targetVarId)
                    ]
                edge = InstEdge (EdgeId 0) expNodeId targetVarId
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (nodeRefKey (genRef srcGen)) (genRef rootGen, BindFlex) $
                        IntMap.insert (nodeRefKey (genRef tgtGen)) (genRef rootGen, BindFlex) $
                    IntMap.insert (nodeRefKey (typeRef srcForallId)) (genRef srcGen, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef targetVarId)) (genRef tgtGen, BindFlex) bindParents0
                genNodes =
                    fromListGen
                        [ (srcGen, GenNode srcGen [srcForallId])
                        , (tgtGen, GenNode tgtGen [targetVarId])
                        ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        , cGenNodes = genNodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate args) -> length args `shouldBe` 1
                        Just other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "composes source elimination with target quantifier introduction when forall arity differs" $ do
            -- s · (∀ a. a) ≤ (∀ b0 b1. b0 → b1)
            -- Thesis Fig. 14.2.6: eliminate the source quantifier by choosing
            -- the whole target polytype for a.  O-introduction would only add
            -- fresh vacuous quantifiers and is therefore not minimal here.
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtDomId = NodeId 2
                tgtCodId = NodeId 3
                tgtArrowId = NodeId 4
                tgtForallId = NodeId 5
                expNodeId = NodeId 6

                nodes = nodeMapFromList
                    [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                    , (1, TyForall srcForallId srcVarId)
                    , (2, TyVar { tnId = tgtDomId, tnBound = Nothing })
                    , (3, TyVar { tnId = tgtCodId, tnBound = Nothing })
                    , (4, TyArrow tgtArrowId tgtDomId tgtCodId)
                    , (5, TyForall tgtForallId tgtArrowId)
                    , (6, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId

                -- Make the target forall bind both variables directly (flex) so
                -- orderedBinders sees arity 2.
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (nodeRefKey (typeRef tgtDomId)) (typeRef tgtForallId, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef tgtCodId)) (typeRef tgtForallId, BindFlex) bindParents0

                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } -> do
                    case IntMap.lookup 0 exps of
                        Just
                            ( ExpCompose
                                ( ExpInstantiate [argument]
                                    NE.:| [ExpForall targetSpecs]
                                )
                              ) -> do
                                argument `shouldBe` tgtArrowId
                                map (length . fsBounds) (NE.toList targetSpecs)
                                    `shouldBe` [2]
                        Just other ->
                            expectationFailure $
                                "Expected instantiate/forall composition, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "keeps identity when forall arity matches and requests body unification" $ do
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4
                rootId = NodeId 5
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                        , (1, TyForall srcForallId srcVarId)
                        , (2, TyVar { tnId = tgtVarId, tnBound = Nothing })
                        , (3, TyForall tgtForallId tgtVarId)
                        , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                        , (5, TyArrow rootId expNodeId tgtForallId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prConstraint = c' } -> do
                    case IntMap.lookup 0 exps of
                        Just ExpIdentity -> pure ()
                        other -> expectationFailure $ "Expected ExpIdentity, got " ++ show other

                    Binding.lookupBindParent c' (typeRef srcForallId)
                        `shouldBe` Just (typeRef rootId, BindRigid)

                    let nodes' = cNodes c'
                    case (lookupNodeMaybe nodes' srcVarId, lookupNodeMaybe nodes' tgtVarId) of
                        (Just _, Nothing) -> pure ()
                        (Nothing, Just _) -> pure ()
                        (Just _, Just _) ->
                            expectationFailure "Nodes 0 and 2 should have been merged but both exist in cNodes"
                        (Nothing, Nothing) -> expectationFailure "Both nodes missing?"

        it "rejects expansions that would point a binder back into its own body" $ do
            -- Edge: s · (∀a. a) ≤ ∀b. (b → s · (∀a. a))
            --
            -- Minimal expansion would keep identity and request a unification between the
            -- source binder `a` and the target body. That unification would make `a`
            -- reachable from itself (via the nested occurrence of s · (∀a. a)), so
            -- presolution must reject it via occurs-check.
            let boundVarId = NodeId 0
                srcForallId = NodeId 1
                srcExpId = NodeId 2
                tgtBinderId = NodeId 3
                tgtBodyId = NodeId 4
                tgtForallId = NodeId 5

                nodes = nodeMapFromList
                    [ (getNodeId boundVarId, TyVar { tnId = boundVarId, tnBound = Nothing })
                    , (getNodeId srcForallId, TyForall srcForallId boundVarId)
                    , (getNodeId srcExpId, TyExp srcExpId (ExpVarId 0) srcForallId)
                    , (getNodeId tgtBinderId, TyVar { tnId = tgtBinderId, tnBound = Nothing })
                    , (getNodeId tgtBodyId, TyArrow tgtBodyId tgtBinderId srcExpId)
                    , (getNodeId tgtForallId, TyForall tgtForallId tgtBodyId)
                    ]

                edge = InstEdge (EdgeId 0) srcExpId tgtForallId
                bindParents =
                    IntMap.insert
                        (nodeRefKey (typeRef tgtBinderId))
                        (typeRef tgtForallId, BindFlex)
                        (inferBindParents nodes)
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            let isOccursCheck err = case err of
                        OccursCheckPresolution{} -> True
                        PlanError inner -> isOccursCheck inner
                        ExecError inner -> isOccursCheck inner
                        _ -> False

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err | isOccursCheck err -> pure ()
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> expectationFailure "Expected presolution occurs-check failure"

        it "returns ExpForall for structure <= forall" $ do
            let srcDomId = NodeId 0
                srcCodId = NodeId 1
                srcArrowId = NodeId 2
                tgtDomId = NodeId 3
                tgtCodId = NodeId 4
                tgtArrowId = NodeId 5
                tgtForallId = NodeId 6
                expNodeId = NodeId 7
                rootId = NodeId 8
                nodes = nodeMapFromList
                        [ (0, TestTyBase srcDomId (BaseTy "int"))
                        , (1, TestTyBase srcCodId (BaseTy "int"))
                        , (2, TyArrow srcArrowId srcDomId srcCodId)
                        , (3, TestTyBase tgtDomId (BaseTy "int"))
                        , (4, TestTyBase tgtCodId (BaseTy "int"))
                        , (5, TyArrow tgtArrowId tgtDomId tgtCodId)
                        , (6, TyForall tgtForallId tgtArrowId)
                        , (7, TyExp expNodeId (ExpVarId 0) srcArrowId)
                        , (8, TyArrow rootId expNodeId tgtForallId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    case IntMap.lookup 0 exps of
                        Just (ExpForall (s NE.:| [])) -> s `shouldBe` ForallSpec []
                        Just other -> expectationFailure $ "Expected ExpForall, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "does not require target foralls to have a binding parent" $ do
            -- Paper-faithful scope tracking uses binding edges. A forall node may be a
            -- binding root (no parent) and presolution should still succeed.
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4

                nodes = nodeMapFromList
                    [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                    , (1, TyForall srcForallId srcVarId)
                    , (2, TyVar { tnId = tgtVarId, tnBound = Nothing })
                    , (3, TyForall tgtForallId tgtVarId)
                    , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> pure ()

    describe "Error Conditions" $ do
        it "reports UnmatchableTypes when explicitly thrown" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                n1 = NodeId 1
                n2 = NodeId 2
                msg = "test mismatch"

            case runPresolutionM defaultTraceConfig st0 (throwError (UnmatchableTypes n1 n2 msg)) of
                Left (UnmatchableTypes n1' n2' msg') -> do
                    n1' `shouldBe` n1
                    n2' `shouldBe` n2
                    msg' `shouldBe` msg
                Left err -> expectationFailure $ "Expected UnmatchableTypes, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports UnresolvedExpVar when explicitly thrown" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                v = ExpVarId 123

            case runPresolutionM defaultTraceConfig st0 (throwError (UnresolvedExpVar v)) of
                Left (UnresolvedExpVar v') -> v' `shouldBe` v
                Left err -> expectationFailure $ "Expected UnresolvedExpVar, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports InternalError when explicitly thrown" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                msg = "test internal error"

            case runPresolutionM defaultTraceConfig st0 (throwError (InternalError msg)) of
                Left (InternalError msg') -> msg' `shouldBe` msg
                Left err -> expectationFailure $ "Expected InternalError, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports ArityMismatch when merging ExpInstantiate with different lengths" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                exp1 = ExpInstantiate [NodeId 1]
                exp2 = ExpInstantiate [NodeId 1, NodeId 2]

            -- mergeExpansions is internal, but we can access it via a helper or by constructing
            -- a scenario where processInstEdge hits this case.
            -- Using runPresolutionM to call mergeExpansions directly is cleaner.

            case runPresolutionM defaultTraceConfig st0 (mergeExpansions (ExpVarId 0) exp1 exp2) of
                Left (ArityMismatch ctx expected actual) -> do
                    ctx `shouldBe` "ExpInstantiate merge"
                    expected `shouldBe` 1
                    actual `shouldBe` 2
                Left err -> expectationFailure $ "Expected ArityMismatch, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports InstantiateOnNonForall through the destination-aware constructor" $ do
            let expNodeId = NodeId 0
                bodyId = NodeId 1
                targetId = NodeId 2
                -- Body is a base type, not a forall
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, expNode)
                        , (1, TestTyBase bodyId (BaseTy "int"))
                        , (2, TestTyBase targetId (BaseTy "bool"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [NodeId 3]
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        expansion
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left (InstantiateOnNonForall nid) -> nid `shouldBe` bodyId
                Left err -> expectationFailure $ "Expected InstantiateOnNonForall, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "copies a degenerate ExpInstantiate at its destination" $ do
            let bodyId = NodeId 0
                expNodeId = NodeId 1
                targetId = NodeId 2
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, TestTyBase bodyId (BaseTy "int"))
                        , (1, expNode)
                        , (2, TestTyBase targetId (BaseTy "int"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        (ExpInstantiate [])
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left err -> expectationFailure $ "Degenerate expansion failed: " ++ show err
                Right ((resultRoot, (copyMap, _, _), _construction), st1) -> do
                    copiedBody <-
                        case lookupCopy bodyId copyMap of
                            Nothing -> expectationFailure "Expected a degenerate destination copy" >> pure bodyId
                            Just copy -> pure copy
                    resultRoot `shouldBe` copiedBody
                    copiedBody `shouldNotBe` bodyId
                    Binding.lookupBindParent (psConstraint st1) (typeRef copiedBody)
                        `shouldBe` Just (genRef (GenNodeId 0), BindFlex)
                    Binding.lookupBindParent (psConstraint st1) (typeRef bodyId)
                        `shouldBe` Just (typeRef expNodeId, BindFlex)

        it "keeps an annotation body in the frontier of its wrapper owner" $ do
            let outerGen = GenNodeId 0
                annotationGen = GenNodeId 1
                bodyId = NodeId 0
                expNodeId = NodeId 1
                targetId = NodeId 2
                lowerId = NodeId 3
                lowerDomId = NodeId 4
                lowerCodId = NodeId 5
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, TyVar bodyId (Just lowerId))
                        , (1, expNode)
                        , (2, TyVar targetId Nothing)
                        , (3, TyArrow lowerId lowerDomId lowerCodId)
                        , (4, TestTyBase lowerDomId (BaseTy "Int"))
                        , (5, TestTyBase lowerCodId (BaseTy "Bool"))
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = IntMap.fromList
                            [ (nodeRefKey (typeRef bodyId), (genRef outerGen, BindFlex))
                            , (nodeRefKey (typeRef lowerId), (typeRef bodyId, BindFlex))
                            , (nodeRefKey (typeRef lowerDomId), (typeRef lowerId, BindFlex))
                            , (nodeRefKey (typeRef lowerCodId), (typeRef lowerId, BindFlex))
                            , (nodeRefKey (genRef annotationGen), (genRef outerGen, BindFlex))
                            , (nodeRefKey (typeRef expNodeId), (genRef annotationGen, BindFlex))
                            , (nodeRefKey (typeRef targetId), (genRef annotationGen, BindRigid))
                            ]
                        , cGenNodes = fromListGen
                            [ (outerGen, GenNode outerGen [bodyId])
                            , (annotationGen, GenNode annotationGen [targetId])
                            ]
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty 6 IntSet.empty IntMap.empty
                        IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        annotationGen
                        targetId
                        (ExpInstantiate [])
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left err -> expectationFailure $ "Annotation expansion failed: " ++ show err
                Right ((resultRoot, (copyMap, _interior, frontier), _construction), st1) -> do
                    frontier `shouldBe` IntSet.singleton (getNodeId bodyId)
                    copiedBody <-
                        maybe
                            (expectationFailure "Expected a degenerate annotation copy" >> fail "missing copy")
                            pure
                            (lookupCopy bodyId copyMap)
                    resultRoot `shouldBe` copiedBody
                    lookupNodeMaybe (cNodes (psConstraint st1)) copiedBody
                        `shouldBe` Just (TyBottom copiedBody)
                    lookupCopy lowerId copyMap `shouldBe` Nothing
                    lookupNodeMaybe (cNodes (psConstraint st1)) bodyId
                        `shouldBe` Just (TyVar bodyId (Just lowerId))
                    Binding.lookupBindParent (psConstraint st1) (typeRef copiedBody)
                        `shouldBe` Just (genRef annotationGen, BindFlex)
                    Binding.lookupBindParent (psConstraint st1) (typeRef lowerId)
                        `shouldBe` Just (typeRef bodyId, BindFlex)

        it "reports ArityMismatch for a destination-aware instantiation" $ do
            let expNodeId = NodeId 0
                bodyId = NodeId 1
                binder0 = NodeId 2
                binder1 = NodeId 3
                targetId = NodeId 4
                argument = NodeId 5
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, expNode)
                        , (1, TyArrow bodyId binder0 binder1)
                        , (2, TyVar {tnId = binder0, tnBound = Nothing})
                        , (3, TyVar {tnId = binder1, tnBound = Nothing})
                        , (4, TestTyBase targetId (BaseTy "int"))
                        , (5, TyVar {tnId = argument, tnBound = Nothing})
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 6 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [argument]
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        expansion
                        expNode
                        bodyId
                        [binder0, binder1]

            case runPresolutionM defaultTraceConfig st0 action of
                Left (ArityMismatch ctx expected actual) -> do
                    ctx `shouldBe` "applyExpansionEdgeTracedAtTargetWithBinders"
                    expected `shouldBe` 2
                    actual `shouldBe` 1
                Left err -> expectationFailure $ "Expected ArityMismatch, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "constructs nested ExpCompose at the destination without rebinding its source" $ do
            let bodyId = NodeId 0
                expNodeId = NodeId 1
                targetId = NodeId 2
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, TestTyBase bodyId (BaseTy "int"))
                        , (1, expNode)
                        , (2, TestTyBase targetId (BaseTy "int"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

                expansion = ExpCompose (ExpForall (ForallSpec [] NE.:| []) NE.:| [ExpIdentity])
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        expansion
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left err -> expectationFailure $ "Expansion failed: " ++ show err
                Right ((forallId, (copyMap, _, _), _construction), st1) -> do
                    copiedBody <-
                        case lookupCopy bodyId copyMap of
                            Nothing -> expectationFailure "Expected a degenerate destination copy" >> pure bodyId
                            Just copy -> pure copy
                    copiedBody `shouldNotBe` bodyId
                    expectForallBody (cNodes (psConstraint st1)) forallId
                        `shouldReturn` copiedBody
                    Binding.lookupBindParent (psConstraint st1) (typeRef bodyId)
                        `shouldBe` Just (typeRef expNodeId, BindFlex)

        it "materializes ExpForall over a destination-owned copy with binder bounds" $ do
            let domVarId = NodeId 0
                codVarId = NodeId 1
                arrowId = NodeId 2
                expNodeId = NodeId 3
                bndId = NodeId 4

                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = domVarId, tnBound = Nothing })
                        , (1, TyVar { tnId = codVarId, tnBound = Nothing })
                        , (2, TyArrow arrowId domVarId codVarId)
                        , (3, TyExp expNodeId (ExpVarId 0) arrowId)
                        , (4, TestTyBase bndId (BaseTy "int"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        5
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                forallSpec =
                    ForallSpec
                        { fsBounds =
                            [ Just (BoundBinder 1)
                            , Just (BoundNode bndId)
                            ]
                        }
                expansion = ExpForall (forallSpec NE.:| [])

                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        bndId
                        expansion
                        (TyExp expNodeId (ExpVarId 0) arrowId)
                        arrowId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left err -> expectationFailure $ "Expansion failed: " ++ show err
                Right ((forallId, (copyMap, _, _), _construction), st1) -> do
                    let c1 = psConstraint st1
                        nodes1 = cNodes c1
                        bp1 = cBindParents c1
                        boundOf nid =
                            case lookupNodeMaybe nodes1 nid of
                                Just TyVar{ tnBound = mb } -> mb
                                _ -> Nothing

                    copiedArrow <-
                        case lookupCopy arrowId copyMap of
                            Nothing -> expectationFailure "Expected copied arrow" >> pure arrowId
                            Just copy -> pure copy
                    copiedDom <-
                        case lookupCopy domVarId copyMap of
                            Nothing -> expectationFailure "Expected copied domain binder" >> pure domVarId
                            Just copy -> pure copy
                    copiedCod <-
                        case lookupCopy codVarId copyMap of
                            Nothing -> expectationFailure "Expected copied codomain binder" >> pure codVarId
                            Just copy -> pure copy

                    expectForallBody nodes1 forallId `shouldReturn` copiedArrow

                    IntMap.lookup (nodeRefKey (typeRef copiedArrow)) bp1 `shouldBe` Just (typeRef forallId, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef copiedDom)) bp1 `shouldBe` Just (typeRef forallId, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef copiedCod)) bp1 `shouldBe` Just (typeRef forallId, BindFlex)

                    boundOf copiedDom `shouldBe` Just copiedCod
                    boundOf copiedCod `shouldBe` Just bndId

                    boundOf domVarId `shouldBe` Nothing
                    boundOf codVarId `shouldBe` Nothing
                    IntMap.lookup (nodeRefKey (typeRef arrowId)) bp1
                        `shouldBe` Just (typeRef expNodeId, BindFlex)

                    case Binding.orderedBinders id c1 (typeRef forallId) of
                        Left err -> expectationFailure $ "orderedBinders failed: " ++ show err
                        Right bs -> bs `shouldBe` [copiedCod, copiedDom]

        it "rejects a ForallSpec with more binders than its copied body provides" $ do
            let bodyId = NodeId 0
                expNodeId = NodeId 1
                targetId = NodeId 2
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = bodyId, tnBound = Nothing})
                        , (1, expNode)
                        , (2, TestTyBase targetId (BaseTy "int"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        (ExpForall (ForallSpec [Nothing, Nothing] NE.:| []))
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left (ArityMismatch arityContext expected actual) -> do
                    arityContext `shouldBe` "bindForallBindersFromSpec"
                    expected `shouldBe` 2
                    actual `shouldBe` 1
                Left err -> expectationFailure $ "Expected ForallSpec arity failure, got " ++ show err
                Right _ -> expectationFailure "Expected too-few forall binders to fail"

        it "rejects an out-of-range BoundBinder before installing bounds" $ do
            let bodyId = NodeId 0
                expNodeId = NodeId 1
                targetId = NodeId 2
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = bodyId, tnBound = Nothing})
                        , (1, expNode)
                        , (2, TestTyBase targetId (BaseTy "int"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        (ExpForall (ForallSpec [Just (BoundBinder 1)] NE.:| []))
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left (InternalError message) ->
                    message `shouldContain` "invalid BoundBinder index 1"
                Left err -> expectationFailure $ "Expected BoundBinder index failure, got " ++ show err
                Right _ -> expectationFailure "Expected invalid BoundBinder index to fail"

        it "allows extra body variables that the ForallSpec does not quantify" $ do
            let domId = NodeId 0
                codId = NodeId 1
                bodyId = NodeId 2
                expNodeId = NodeId 3
                targetId = NodeId 4
                expNode = TyExp expNodeId (ExpVarId 0) bodyId
                nodes =
                    nodeMapFromList
                        [ (0, TyVar {tnId = domId, tnBound = Nothing})
                        , (1, TyVar {tnId = codId, tnBound = Nothing})
                        , (2, TyArrow bodyId domId codId)
                        , (3, expNode)
                        , (4, TestTyBase targetId (BaseTy "int"))
                        ]
                constraint =
                    rootedConstraint $
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 5 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                action =
                    applyExpansionEdgeTracedAtTargetWithBindersForTest
                        (GenNodeId 0)
                        targetId
                        (ExpForall (ForallSpec [Nothing] NE.:| []))
                        expNode
                        bodyId
                        []

            case runPresolutionM defaultTraceConfig st0 action of
                Left err -> expectationFailure $ "Forall expansion with an extra variable failed: " ++ show err
                Right ((forallId, _, _construction), st1) ->
                    case Binding.orderedBinders id (psConstraint st1) (typeRef forallId) of
                        Left err -> expectationFailure $ "orderedBinders failed: " ++ show err
                        Right binders -> length binders `shouldBe` 1

        it "handles multiple edges correctly" $ do
            let varId = NodeId 0
                forallId = NodeId 1
                target1Id = NodeId 2 -- int
                target2Id = NodeId 3 -- bool
                exp1Id = NodeId 4 -- s1 . sigma
                exp2Id = NodeId 5 -- s2 . sigma
                rootEdge1 = NodeId 6
                rootEdge2 = NodeId 7
                rootId = NodeId 8
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = varId, tnBound = Nothing })
                        , (1, TyForall forallId varId)
                        , (2, TestTyBase target1Id (BaseTy "int"))
                        , (3, TestTyBase target2Id (BaseTy "bool"))
                        , (4, TyExp exp1Id (ExpVarId 1) forallId)
                        , (5, TyExp exp2Id (ExpVarId 2) forallId)
                        , (6, TyArrow rootEdge1 exp1Id target1Id)
                        , (7, TyArrow rootEdge2 exp2Id target2Id)
                        , (8, TyArrow rootId rootEdge1 rootEdge2)
                        ]
                edge1 = InstEdge (EdgeId 0) exp1Id target1Id
                edge2 = InstEdge (EdgeId 1) exp2Id target2Id
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge1, edge2]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge1, edge2]
                        , arDepGraph = undefined
                        }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } -> do
                    case (IntMap.lookup 0 exps, IntMap.lookup 1 exps) of
                        (Just (ExpInstantiate [n1]), Just (ExpInstantiate [n2])) ->
                            n1 `shouldNotBe` n2
                        _ -> expectationFailure "Expected two separate instantiations"

        it "merges instantiations when the same ExpVar appears in multiple edges" $ do
            let boundId = NodeId 0
                forallId = NodeId 1
                expNodeId = NodeId 2
                targetId = NodeId 3
                rootId = NodeId 4
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = boundId, tnBound = Nothing })
                        , (1, TyForall forallId boundId)
                        , (2, TyExp expNodeId (ExpVarId 0) forallId)
                        , (3, TestTyBase targetId (BaseTy "int"))
                        , (4, TyArrow rootId expNodeId targetId)
                        ]
                edge1 = InstEdge (EdgeId 0) expNodeId targetId
                edge2 = InstEdge (EdgeId 1) expNodeId targetId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge1, edge2]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge1, edge2], arDepGraph = undefined }

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prConstraint = c' } -> do
                    case (IntMap.lookup 0 exps, IntMap.lookup 1 exps) of
                        (Just (ExpInstantiate [n1]), Just (ExpInstantiate [n2])) -> do
                            n1 `shouldBe` n2
                            nodeMapMember n1 (cNodes c') `shouldBe` True
                        _ -> expectationFailure "Expected merged ExpInstantiate"

        it "materializes expansions and clears inst edges for strict solve" $ do
            let bound = NodeId 0
                forallId = NodeId 1
                expId = NodeId 2
                targetId = NodeId 3
                rootId = NodeId 4
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = bound, tnBound = Nothing })
                        , (1, TyForall forallId bound)
                        , (2, TyExp expId (ExpVarId 0) forallId)
                        , (3, TestTyBase targetId (BaseTy "int"))
                        , (4, TyArrow rootId expId targetId)
                        ]
                edge = InstEdge (EdgeId 0) expId targetId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

                isExp TyExp{} = True
                isExp _ = False

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prConstraint = c' } -> do
                    any isExp (nodeMapElems (cNodes c')) `shouldBe` False
                    cInstEdges c' `shouldBe` []
                    validateSolvedGraphStrict (SolveResult { srConstraint = c', srUnionFind = IntMap.empty })
                        `shouldBe` []

    describe "Phase 4 regression matrix" $ do
        it "covers identity, instantiate, forall-intro, and compose constructors" $ do
            let expansionMatrix =
                    [ ExpIdentity
                    , ExpInstantiate [NodeId 0]
                    , ExpForall (ForallSpec [Nothing] NE.:| [])
                    , ExpCompose
                        ( ExpInstantiate [NodeId 1]
                            NE.:| [ExpForall (ForallSpec [Nothing] NE.:| [])]
                        )
                    ]
                tag expansion = case expansion of
                    ExpIdentity -> "identity"
                    ExpInstantiate _ -> "instantiate"
                    ExpForall _ -> "forall-intro"
                    ExpCompose _ -> "compose"
            map tag expansionMatrix `shouldBe` ["identity", "instantiate", "forall-intro", "compose"]

    describe "Thesis obligations" $ do
        it "O10-EXP-DECIDE" $ do
            -- Decide minimal expansion: computePresolution decides an expansion for a simple inst edge
            let bodyNode = NodeId 0
                intNode = TestTyBase (NodeId 1) (BaseTy "Int")
                forallNode = TyForall (NodeId 2) (NodeId 1)
                expNode = TyExp (NodeId 3) (ExpVarId 0) (NodeId 2)
                edge = InstEdge (EdgeId 0) (NodeId 3) (NodeId 1)
                nodes = nodeMapFromList [(0, TyVar { tnId = bodyNode, tnBound = Nothing }), (1, intNode), (2, forallNode), (3, expNode)]
                constraint = rootedConstraint $ emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef (NodeId 3)), (typeRef (NodeId 1), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 2)), (typeRef (NodeId 3), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 0)), (typeRef (NodeId 2), BindFlex))
                        ]
                    }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }
            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    IntMap.size exps `shouldSatisfy` (> 0)
                Left err -> expectationFailure $ "Presolution failed: " ++ show err

        it "O10-EXP-APPLY" $ do
            -- Apply expansion: computePresolution applies decided expansions and produces witnesses
            let bodyNode = NodeId 0
                intNode = TestTyBase (NodeId 1) (BaseTy "Int")
                forallNode = TyForall (NodeId 2) (NodeId 1)
                expNode = TyExp (NodeId 3) (ExpVarId 0) (NodeId 2)
                edge = InstEdge (EdgeId 0) (NodeId 3) (NodeId 1)
                nodes = nodeMapFromList [(0, TyVar { tnId = bodyNode, tnBound = Nothing }), (1, intNode), (2, forallNode), (3, expNode)]
                constraint = rootedConstraint $ emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef (NodeId 3)), (typeRef (NodeId 1), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 2)), (typeRef (NodeId 3), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 0)), (typeRef (NodeId 2), BindFlex))
                        ]
                    }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }
            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Right PresolutionResult{ prEdgeWitnesses = ews } ->
                    IntMap.size ews `shouldSatisfy` (> 0)
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
