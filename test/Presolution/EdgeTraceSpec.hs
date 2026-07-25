{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Presolution.EdgeTraceSpec (spec) where

import IdentityTestSupport
import Test.Hspec
import Control.Monad (forM_)
import Control.Monad.State.Strict (get)
import Data.Either (isLeft)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Phase (Phase (Raw))
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Witness.TestSupport
    ( EdgeWitness(..)
    , InstanceWitness(..)
    )
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionError(..)
    , PresolutionResult(..)
    )
import MLF.Constraint.Presolution.Base
    ( EdgeWitnessNonSourceOrigin(..)
    )
import MLF.Constraint.Presolution.Construction
    ( combineRawExpansionConstructions
    , mkRawExpansionConstruction
    , rawExpansionConstructionArgumentKeys
    , rawExpansionConstructionParents
    , rawExpansionConstructionSemanticMetaKeys
    )
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , EdgeExecutionArtifacts(..)
    , psEdgeExecutionArtifacts
    , psEdgeExpansionConstructions
    , psEdgeExpansions
    , psEdgeTraces
    , psEdgeWitnesses
    , applyExpansionEdgeTracedAtTargetWithBindersForTest
    , bindExpansionArgsForTest
    , copiedNodes
    , emptyRawExpansionConstruction
    , sourceInteriorFromList
    , lookupExpansionResult
    , lookupCopy
    , processInstEdge
    , recordEdgeExecutionArtifactsForTest
    , runPresolutionM
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Binding.ScopeGraph (buildTypeEdgesFrom)
import qualified MLF.Util.UnionFind as UF
import SpecUtil
    ( bindParentsFromPairs
    , computePresolutionRaw
    , defaultTraceConfig
    , emptyConstraint
    , nodeMapFromList
    , rootedConstraint
    )

data SharedDestinationLayout
    = SameDestination
    | SiblingDestinations
    | NestedDestinations
    deriving (Eq, Show)

sharedExpansionDestinationFixture
    :: SharedDestinationLayout
    -> (PresolutionState 'Raw, InstEdge, InstEdge)
sharedExpansionDestinationFixture layout =
    (state0, edge0, edge1)
  where
    sourceBinder = NodeId 0
    sourceArrow = NodeId 1
    sourceForall = NodeId 2
    expansionNode0 = NodeId 3
    targetArgument0 = NodeId 4
    targetArrow0 = NodeId 5
    targetArgument1 = NodeId 6
    targetArrow1 = NodeId 7
    expansionNode1 = NodeId 8

    rootGen = GenNodeId 0
    destinationGen0 = GenNodeId 1
    destinationGen1 =
        case layout of
            SameDestination -> destinationGen0
            SiblingDestinations -> GenNodeId 2
            NestedDestinations -> GenNodeId 2

    edge0 = InstEdge (EdgeId 0) expansionNode0 targetArrow0
    edge1 = InstEdge (EdgeId 1) expansionNode1 targetArrow1

    genNodes =
        GenNodeMap $
            IntMap.fromList $
                [ (getGenNodeId rootGen, GenNode rootGen [expansionNode0, expansionNode1])
                , (getGenNodeId destinationGen0, GenNode destinationGen0 [])
                ]
                    ++ [ (getGenNodeId destinationGen1, GenNode destinationGen1 [])
                       | destinationGen1 /= destinationGen0
                       ]

    destinationGenParents =
        [ (nodeRefKey (genRef destinationGen0), (genRef rootGen, BindFlex))
        ]
            ++ case layout of
                SameDestination -> []
                SiblingDestinations ->
                    [(nodeRefKey (genRef destinationGen1), (genRef rootGen, BindFlex))]
                NestedDestinations ->
                    [(nodeRefKey (genRef destinationGen1), (genRef destinationGen0, BindFlex))]

    bindParents =
        IntMap.fromList $
            [ (nodeRefKey (typeRef sourceBinder), (typeRef sourceForall, BindFlex))
            , (nodeRefKey (typeRef sourceArrow), (typeRef sourceForall, BindFlex))
            , (nodeRefKey (typeRef sourceForall), (typeRef expansionNode0, BindFlex))
            , (nodeRefKey (typeRef expansionNode0), (genRef rootGen, BindFlex))
            , (nodeRefKey (typeRef expansionNode1), (genRef rootGen, BindFlex))
            , (nodeRefKey (typeRef targetArgument0), (genRef destinationGen0, BindFlex))
            , (nodeRefKey (typeRef targetArrow0), (genRef destinationGen0, BindFlex))
            , (nodeRefKey (typeRef targetArgument1), (genRef destinationGen1, BindFlex))
            , (nodeRefKey (typeRef targetArrow1), (genRef destinationGen1, BindFlex))
            ]
                ++ destinationGenParents

    constraint =
        emptyConstraint
            { cNodes =
                nodeMapFromList
                    [ (0, TyVar {tnId = sourceBinder, tnBound = Nothing})
                    , (1, TyArrow sourceArrow sourceBinder sourceBinder)
                    , (2, TyForall sourceForall sourceArrow)
                    , (3, TyExp expansionNode0 (ExpVarId 0) sourceForall)
                    , (4, TyVar {tnId = targetArgument0, tnBound = Nothing})
                    , (5, TyArrow targetArrow0 targetArgument0 targetArgument0)
                    , (6, TyVar {tnId = targetArgument1, tnBound = Nothing})
                    , (7, TyArrow targetArrow1 targetArgument1 targetArgument1)
                    , (8, TyExp expansionNode1 (ExpVarId 0) sourceForall)
                    ]
            , cInstEdges = [edge0, edge1]
            , cBindParents = bindParents
            , cGenNodes = genNodes
            }

    state0 =
        PresolutionState constraint (Presolution IntMap.empty)
            IntMap.empty
            9
            IntSet.empty
            IntMap.empty
            IntMap.empty
            IntMap.empty
            IntMap.empty
            IntMap.empty

spec :: Spec
spec = describe "EdgeTrace" $ do
    it "records the complete edge execution packet atomically and rejects conflicting duplicates" $ do
        let edgeId = EdgeId 7
            trace0 =
                EdgeTrace
                    { etRoot = NodeId 0
                    , etResultRoot = NodeId 0
                    , etBinderArgs = []
                    , etInterior = sourceInteriorFromList []
                    , etReplayContract = ReplayContractNone
                    , etBinderReplayMap = IntMap.empty
                    , etReplayDomainBinders = []
                    , etCopyMap = mempty
                    }
            conflictingTrace = trace0 {etResultRoot = NodeId 1}
            witness0 =
                EdgeWitness
                    { ewEdgeId = edgeId
                    , ewLeft = NodeId 0
                    , ewRight = NodeId 0
                    , ewRoot = NodeId 0
                    , ewForallIntros = 0
                    , ewWitness = InstanceWitness []
                    }
            construction1 =
                case
                    mkRawExpansionConstruction
                        ( IntMap.singleton
                            (nodeRefKey (typeRef (NodeId 1)))
                            (genRef (GenNodeId 0), BindRigid)
                        )
                        IntSet.empty
                        IntSet.empty
                  of
                    Left err -> error err
                    Right construction -> construction
            artifacts0 =
                EdgeExecutionArtifacts
                    { eeaExpansion = ExpIdentity
                    , eeaWitness = witness0
                    , eeaRaiseAuthorityNodes = IntSet.empty
                    , eeaNonSourceOpOrigins = IntMap.empty
                    , eeaExpansionConstruction = emptyRawExpansionConstruction
                    , eeaTrace = trace0
                    }
            st0 =
                PresolutionState emptyConstraint (Presolution IntMap.empty)
                    IntMap.empty
                    1
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            record artifacts =
                recordEdgeExecutionArtifactsForTest
                    edgeId
                    artifacts
        case runPresolutionM defaultTraceConfig st0 (record artifacts0 >> record artifacts0) of
            Left err ->
                expectationFailure ("idempotent artifact write failed: " ++ show err)
            Right (_, st1) -> do
                IntMap.lookup 7 (psEdgeTraces st1) `shouldBe` Just trace0
                IntMap.lookup 7 (psEdgeExpansionConstructions st1)
                    `shouldBe` Just emptyRawExpansionConstruction
                IntMap.lookup 7 (psEdgeExecutionArtifacts st1)
                    `shouldBe` Just artifacts0
        forM_
            [ artifacts0 {eeaExpansion = ExpForall (ForallSpec [] NE.:| [])}
            , artifacts0 {eeaWitness = witness0 {ewRight = NodeId 1}}
            , artifacts0 {eeaRaiseAuthorityNodes = IntSet.singleton 0}
            , artifacts0
                { eeaNonSourceOpOrigins =
                    IntMap.singleton 0 DestinationEdgeOperation
                }
            , artifacts0 {eeaTrace = conflictingTrace}
            , artifacts0 {eeaExpansionConstruction = construction1}
            ]
            $ \conflictingArtifacts ->
                case
                    runPresolutionM
                        defaultTraceConfig
                        st0
                        (record artifacts0 >> record conflictingArtifacts)
                  of
                    Left (InternalError message) ->
                        message `shouldContain` "conflicting edge execution artifact write"
                    Left err ->
                        expectationFailure ("expected conflicting artifact error, saw " ++ show err)
                    Right _ ->
                        expectationFailure "expected conflicting artifact write to fail"

    it "combines composed construction evidence without choosing a conflicting parent" $ do
        let firstChild = NodeId 0
            secondChild = NodeId 1
            firstParents =
                IntMap.singleton
                    (nodeRefKey (typeRef firstChild))
                    (genRef (GenNodeId 0), BindFlex)
            secondParents =
                IntMap.singleton
                    (nodeRefKey (typeRef secondChild))
                    (genRef (GenNodeId 0), BindFlex)
            conflictingParents =
                IntMap.singleton
                    (nodeRefKey (typeRef firstChild))
                    (genRef (GenNodeId 1), BindFlex)
            build parents argumentKeys =
                case
                    mkRawExpansionConstruction
                        parents
                        argumentKeys
                        IntSet.empty
                  of
                    Left err -> expectationFailure err >> fail "invalid test construction"
                    Right construction -> pure construction
        first <- build firstParents (IntSet.singleton (getNodeId firstChild))
        second <- build secondParents (IntSet.singleton (getNodeId secondChild))
        expected <-
            build
                (IntMap.union firstParents secondParents)
                (IntSet.fromList [getNodeId firstChild, getNodeId secondChild])
        conflict <-
            build
                conflictingParents
                (IntSet.singleton (getNodeId firstChild))
        combineRawExpansionConstructions first first `shouldBe` Right first
        combineRawExpansionConstructions first second `shouldBe` Right expected
        combineRawExpansionConstructions first conflict `shouldSatisfy` isLeft

    it "binds fresh expansion arguments at the expansion destination" $ do
        let expansionRoot = NodeId 0
            sourceBinder = NodeId 1
            argument = NodeId 2
            canonicalArgument = NodeId 3
            rootGen = GenNodeId 0
            destinationGen = GenNodeId 1
            constraint =
                emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyVar {tnId = expansionRoot, tnBound = Nothing})
                            , (1, TyVar {tnId = sourceBinder, tnBound = Nothing})
                            , (2, TyVar {tnId = argument, tnBound = Nothing})
                            , (3, TyVar {tnId = canonicalArgument, tnBound = Nothing})
                            ]
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef expansionRoot), (genRef destinationGen, BindFlex))
                            , (nodeRefKey (typeRef sourceBinder), (genRef rootGen, BindFlex))
                            , (nodeRefKey (genRef destinationGen), (genRef rootGen, BindFlex))
                            ]
                    , cGenNodes =
                        GenNodeMap
                            ( IntMap.fromList
                                [ (getGenNodeId rootGen, GenNode rootGen [])
                                , (getGenNodeId destinationGen, GenNode destinationGen [])
                                ]
                            )
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    (IntMap.singleton (getNodeId argument) canonicalArgument)
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM
            defaultTraceConfig
            st0
            (bindExpansionArgsForTest expansionRoot [(sourceBinder, argument)]) of
            Left err -> expectationFailure ("binding expansion argument failed: " ++ show err)
            Right (_, st1) -> do
                Binding.lookupBindParent (psConstraint st1) (typeRef argument)
                    `shouldBe` Just (genRef destinationGen, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef canonicalArgument)
                    `shouldBe` Just (genRef destinationGen, BindFlex)

    it "rejects a shared instantiating ExpVar across distinct destinations independent of edge order" $ do
        forM_ [SiblingDestinations, NestedDestinations] $ \layout -> do
            let (st0, edge0, edge1) = sharedExpansionDestinationFixture layout
                expectedDestinations = [GenNodeId 1, GenNodeId 2]
            forM_ [[edge0, edge1], [edge1, edge0]] $ \edges -> do
                let stForOrder =
                        st0
                            { psConstraint =
                                (psConstraint st0) { cInstEdges = edges }
                            }
                case runPresolutionM defaultTraceConfig stForOrder (mapM_ processInstEdge edges) of
                    Left (ExpansionDestinationConflict expVar destinations) -> do
                        expVar `shouldBe` ExpVarId 0
                        destinations `shouldBe` expectedDestinations
                    Left err ->
                        expectationFailure
                            ( "expected an order-independent expansion destination conflict for "
                                ++ show layout
                                ++ ", saw "
                                ++ show err
                            )
                    Right _ ->
                        expectationFailure
                            ("expected shared ExpVar destination rejection for " ++ show layout)

    it "allows a shared instantiating ExpVar when all edges have one destination" $ do
        let (st0, edge0, edge1) = sharedExpansionDestinationFixture SameDestination
            sourceInterior = sourceInteriorFromList [NodeId 0, NodeId 1]
            action = do
                processInstEdge edge0
                stAfterFirst <- get
                processInstEdge edge1
                stAfterSecond <- get
                pure (stAfterFirst, stAfterSecond)
        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("same-destination shared ExpVar failed: " ++ show err)
            Right ((stAfterFirst, stAfterSecond), _) -> do
                firstTrace <-
                    maybe
                        (expectationFailure "missing first shared-destination trace" >> fail "missing first trace")
                        pure
                        (IntMap.lookup 0 (psEdgeTraces stAfterFirst))
                secondTrace <-
                    maybe
                        (expectationFailure "missing reused shared-destination trace" >> fail "missing second trace")
                        pure
                        (IntMap.lookup 1 (psEdgeTraces stAfterSecond))
                -- The public trace stays in the producer domain even though
                -- chi_e builds fresh destination nodes.
                etInterior firstTrace `shouldBe` sourceInterior
                copiedNodes (etCopyMap firstTrace)
                    `shouldSatisfy` all (`notElem` [NodeId 0, NodeId 1, NodeId 2])
                -- Duplicate-edge reuse consumes the already-built destination
                -- graph without treating the source interior as that graph.
                psNextNodeId stAfterSecond `shouldBe` psNextNodeId stAfterFirst
                etCopyMap secondTrace `shouldBe` etCopyMap firstTrace
                etResultRoot secondTrace `shouldBe` etResultRoot firstTrace
                etInterior secondTrace `shouldBe` sourceInterior

    it "rejects one occurrence wrapper reused for two expansion results" $ do
        let (st0, edge0, edge1Fresh) =
                sharedExpansionDestinationFixture SameDestination
            edge1 = edge1Fresh {instLeft = instLeft edge0}
            stInvalid =
                st0
                    { psConstraint =
                        (psConstraint st0) {cInstEdges = [edge0, edge1]}
                    }
        case runPresolutionM
            defaultTraceConfig
            stInvalid
            (processInstEdge edge0 >> processInstEdge edge1) of
            Left (ExecError (ExpansionResultConflict wrapper _ _)) ->
                wrapper `shouldBe` instLeft edge0
            Left err ->
                expectationFailure
                    ("expected ExpansionResultConflict, saw " ++ show err)
            Right _ ->
                expectationFailure
                    "expected duplicate occurrence wrapper to fail closed"

    it "constructs an ExpCompose instantiation in the edge destination scope" $ do
        -- Exercise the constructor before structural unification can merge or
        -- Raise the target scope.  ExpIdentity after ExpInstantiate forces the
        -- composed-recipe interpreter without changing the copied result.
        let sourceBinder = NodeId 0
            sourceArrow = NodeId 1
            sourceForall = NodeId 2
            expansionNode = NodeId 3
            target = NodeId 4
            argument = NodeId 5
            rootGen = GenNodeId 0
            destinationGen = GenNodeId 1
            expansion =
                ExpCompose (ExpInstantiate [argument] NE.:| [ExpIdentity])
            constraint =
                emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyVar sourceBinder Nothing)
                            , (1, TyArrow sourceArrow sourceBinder sourceBinder)
                            , (2, TyForall sourceForall sourceArrow)
                            , (3, TyExp expansionNode (ExpVarId 0) sourceForall)
                            , (4, TyVar target Nothing)
                            , (5, TyVar argument Nothing)
                            ]
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef sourceBinder), (typeRef sourceForall, BindFlex))
                            , (nodeRefKey (typeRef sourceArrow), (typeRef sourceForall, BindFlex))
                            , (nodeRefKey (typeRef sourceForall), (typeRef expansionNode, BindFlex))
                            , (nodeRefKey (typeRef expansionNode), (genRef rootGen, BindFlex))
                            , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                            , (nodeRefKey (genRef destinationGen), (genRef rootGen, BindFlex))
                            ]
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId rootGen, GenNode rootGen [expansionNode])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    6
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            expectDestinationPath c uf node =
                let nodeC = UF.frWith uf node
                in case Binding.bindingPathToRoot c (typeRef nodeC) of
                    Left err -> expectationFailure ("binding path failed: " ++ show err)
                    Right path -> path `shouldContain` [genRef destinationGen]
            action =
                applyExpansionEdgeTracedAtTargetWithBindersForTest
                    rootGen
                    target
                    expansion
                    (TyExp expansionNode (ExpVarId 0) sourceForall)
                    sourceArrow
                    [sourceBinder]
        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("ExpCompose edge failed: " ++ show err)
            Right
                ( ( resultRoot
                    , (copyMap, _interior, _frontier)
                    , construction
                    )
                  , st1
                  ) ->
                case (lookupCopy sourceArrow copyMap, lookupCopy sourceBinder copyMap) of
                    (Nothing, _) ->
                        expectationFailure "ExpCompose did not copy its source body"
                    (_, Nothing) ->
                        expectationFailure "ExpCompose did not copy its source binder"
                    (Just copiedArrow, Just semanticMeta) -> do
                        resultRoot `shouldBe` copiedArrow
                        copiedArrow `shouldNotBe` sourceArrow
                        rawExpansionConstructionArgumentKeys construction
                            `shouldBe` IntSet.singleton (getNodeId argument)
                        rawExpansionConstructionSemanticMetaKeys construction
                            `shouldBe` IntSet.singleton (getNodeId semanticMeta)
                        IntMap.lookup
                            (nodeRefKey (typeRef argument))
                            (rawExpansionConstructionParents construction)
                            `shouldBe` Just (genRef destinationGen, BindFlex)
                        IntMap.lookup
                            (nodeRefKey (typeRef semanticMeta))
                            (rawExpansionConstructionParents construction)
                            `shouldBe` Just (typeRef copiedArrow, BindFlex)
                        let c1 = psConstraint st1
                            uf1 = psUnionFind st1
                        expectDestinationPath c1 uf1 copiedArrow
                        expectDestinationPath c1 uf1 argument

    it "records nodes allocated while solving an instantiation edge" $ do
        -- TyExp s · (∀@1. a -> a) ≤ (Int -> Int)
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            targetArrow = NodeId 5

            nodes = nodeMapFromList
                    [ (0, TyVar { tnId = a, tnBound = Nothing })
                    , (1, TyArrow arrow a a)
                    , (2, TyForall forallNode arrow)
                    , (3, TyExp expNode (ExpVarId 0) forallNode)
                    , (4, TestTyBase intNode (BaseTy "Int"))
                    , (5, TyArrow targetArrow intNode intNode)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (a, forallNode, BindFlex)
                            , (arrow, forallNode, BindFlex)
                            , (forallNode, expNode, BindFlex)
                            , (intNode, targetArrow, BindFlex)
                            ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    6
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                let traces = psEdgeTraces st1
                case IntMap.lookup 0 traces of
                    Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                    Just tr -> do
                        etRoot tr `shouldBe` arrow
                        etInterior tr `shouldBe` sourceInteriorFromList [a, arrow]
                        copiedNodes (etCopyMap tr)
                            `shouldSatisfy` all (/= arrow)
                        case etBinderArgs tr of
                            [(bv, _arg)] -> do
                                bv `shouldBe` a
                                case lookupCopy bv (etCopyMap tr) of
                                    Nothing -> expectationFailure "Expected binder meta in etCopyMap"
                                    Just _meta -> pure ()
                            other -> expectationFailure ("Unexpected binder/arg pairs: " ++ show other)

    it "replays a processed expansion edge without allocating fresh copies" $ do
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            targetArrow = NodeId 5

            nodes = nodeMapFromList
                    [ (0, TyVar { tnId = a, tnBound = Nothing })
                    , (1, TyArrow arrow a a)
                    , (2, TyForall forallNode arrow)
                    , (3, TyExp expNode (ExpVarId 0) forallNode)
                    , (4, TestTyBase intNode (BaseTy "Int"))
                    , (5, TyArrow targetArrow intNode intNode)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (a, forallNode, BindFlex)
                            , (arrow, forallNode, BindFlex)
                            , (forallNode, expNode, BindFlex)
                            , (intNode, targetArrow, BindFlex)
                            ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    6
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action = do
                processInstEdge edge
                stAfterFirst <- get
                processInstEdge edge
                stAfterSecond <- get
                pure (stAfterFirst, stAfterSecond)
        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("processInstEdge replay failed: " ++ show err)
            Right ((stAfterFirst, stAfterSecond), _) -> do
                psNextNodeId stAfterSecond `shouldBe` psNextNodeId stAfterFirst
                psEdgeWitnesses stAfterSecond `shouldBe` psEdgeWitnesses stAfterFirst
                case ( IntMap.lookup 0 (psEdgeTraces stAfterFirst)
                     , IntMap.lookup 0 (psEdgeTraces stAfterSecond)
                     ) of
                    (Just firstTrace, Just secondTrace) ->
                        etCopyMap secondTrace `shouldBe` etCopyMap firstTrace
                    other ->
                        expectationFailure ("Missing replay traces: " ++ show other)

    it "tracks binder-argument nodes across merged expansions" $ do
        -- When an expansion variable is reused across multiple instantiation edges, the
        -- *final* expansion may keep the argument nodes allocated by an earlier edge
        -- (mergeExpansions keeps the first ExpInstantiate payload).
        --
        -- Traces should still record binder metas in etCopyMap even when
        -- expansion arguments are reused across edges; etInterior itself is
        -- now the exact I(r) and may exclude merged nodes.
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode0 = NodeId 3
            intNode = NodeId 4
            target1 = NodeId 5
            boolNode = NodeId 6
            target2 = NodeId 7
            expNode1 = NodeId 8

            nodes = nodeMapFromList
                    [ (0, TyVar { tnId = a, tnBound = Nothing })
                    , (1, TyArrow arrow a a)
                    , (2, TyForall forallNode arrow)
                    , (3, TyExp expNode0 (ExpVarId 0) forallNode)
                    , (4, TestTyBase intNode (BaseTy "Int"))
                    , (5, TyArrow target1 intNode intNode)
                    , (6, TestTyBase boolNode (BaseTy "Bool"))
                    , (7, TyArrow target2 boolNode boolNode)
                    , (8, TyExp expNode1 (ExpVarId 0) forallNode)
                    ]

            edge0 = InstEdge (EdgeId 0) expNode0 target1
            edge1 = InstEdge (EdgeId 1) expNode1 target2
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge0, edge1]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (a, forallNode, BindFlex)
                            , (arrow, forallNode, BindFlex)
                            , (forallNode, expNode0, BindFlex)
                            , (intNode, target1, BindFlex)
                            , (boolNode, target2, BindFlex)
                            ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    9
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge0 >> processInstEdge edge1) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                let traces = psEdgeTraces st1
                case (IntMap.lookup 0 traces, IntMap.lookup 1 traces) of
                    (Just tr0, Just tr1) -> do
                        -- sanity: first edge created its binder arg
                        case etBinderArgs tr0 of
                            [(bv0, _arg0)] ->
                                case lookupCopy bv0 (etCopyMap tr0) of
                                    Nothing -> expectationFailure "Expected binder meta in etCopyMap (edge0)"
                                    Just _meta0 -> pure ()
                            other -> expectationFailure ("Unexpected binder/arg pairs (edge0): " ++ show other)
                        -- expected: second edge should also include its binder arg in the trace interior
                        case etBinderArgs tr1 of
                            [(bv1, _arg1)] ->
                                case lookupCopy bv1 (etCopyMap tr1) of
                                    Nothing -> expectationFailure "Expected binder meta in etCopyMap (edge1)"
                                    Just _meta1 -> pure ()
                            other -> expectationFailure ("Unexpected binder/arg pairs (edge1): " ++ show other)
                    other -> expectationFailure ("Missing traces: " ++ show other)

    it "binds expansion root at the same binder as the edge target (paper §3.2)" $ do
        -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
        -- "the root of the expansion is bound at the same binder as the target".
        --
        -- Setup: TyExp s · (∀@1. a -> a) ≤ (Int -> Int)
        -- where the target arrow has a binding parent.
        --
        -- After expansion, the expansion root (the forall body) should have the
        -- same binding parent as the target arrow.
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            targetArrow = NodeId 5
            outerBinder = NodeId 6  -- A node that will be the binding parent
            mentionsNode nid = \case
                OpGraft x y -> x == nid || y == nid
                OpMerge x y -> x == nid || y == nid
                OpRaise x -> x == nid
                OpWeaken x -> x == nid
                OpRaiseMerge x y -> x == nid || y == nid

            nodes = nodeMapFromList
                    [ (0, TyVar { tnId = a, tnBound = Nothing })
                    , (1, TyArrow arrow a a)
                    , (2, TyForall forallNode arrow)
                    , (3, TyExp expNode (ExpVarId 0) forallNode)
                    , (4, TestTyBase intNode (BaseTy "Int"))
                    , (5, TyArrow targetArrow intNode intNode)
                    , (6, TyForall outerBinder targetArrow)  -- Outer binder
                    ]

            -- Set up binding edges: target arrow is bound by outerBinder
            bindParents =
                bindParentsFromPairs
                    [ (a, forallNode, BindFlex)
                    , (arrow, forallNode, BindFlex)
                    , (forallNode, expNode, BindFlex)
                    , (intNode, targetArrow, BindFlex)
                    , (targetArrow, outerBinder, BindFlex)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint = rootedConstraint emptyConstraint
                { cNodes = nodes
                , cInstEdges = [edge]
                , cBindParents = bindParents
                }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    7
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                let traces = psEdgeTraces st1
                    c = psConstraint st1
                case IntMap.lookup 0 traces of
                    Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                    Just tr -> do
                        -- The expansion result (resNodeId) should be bound at the same
                        -- binder as the target. For ExpInstantiate, the result is a copy
                        -- of the forall body (the arrow), which should be in the copyMap.
                        -- We can find it by looking for the copied arrow node.
                        let copyMap = etCopyMap tr
                        -- The copied arrow should be in the copyMap (arrow -> copied arrow)
                        case lookupCopy arrow copyMap of
                            Nothing -> expectationFailure $
                                "Expected arrow to be copied. CopyMap: " ++ show copyMap
                            Just copiedArrow -> do
                                let uf = psUnionFind st1
                                    copiedArrowC = UF.frWith uf copiedArrow
                                    outerBinderC = UF.frWith uf outerBinder
                                    sourceForallC = UF.frWith uf forallNode
                                    quotientTypeEdges =
                                        buildTypeEdgesFrom
                                            (getNodeId . UF.frWith uf)
                                            (cNodes c)
                                    resultChildren =
                                        IntMap.findWithDefault
                                            IntSet.empty
                                            (getNodeId copiedArrowC)
                                            quotientTypeEdges

                                -- The copied arrow (expansion result) should be bound
                                -- at the same binder as the target (up to UF).
                                case Binding.lookupBindParent c (typeRef copiedArrowC) of
                                    Nothing -> expectationFailure $
                                        "Expected expansion result " ++ show copiedArrowC ++
                                        " to have a binding parent. BindParents: " ++
                                        show (cBindParents c)
                                    Just (parentId, _flag) -> parentId `shouldBe` typeRef outerBinderC
                                -- TyExp is only an administrative occurrence wrapper:
                                -- it must not enter semantic UF or add a quotient
                                -- result -> source-scheme edge.
                                UF.frWith uf expNode `shouldBe` expNode
                                lookupExpansionResult expNode (psExpansionResults st1)
                                    `shouldBe` Just copiedArrowC
                                IntSet.member (getNodeId sourceForallC) resultChildren
                                    `shouldBe` False
                                Binding.lookupBindParent c (typeRef sourceForallC)
                                    `shouldBe` Just (typeRef expNode, BindFlex)
                                case IntMap.lookup 0 (psEdgeWitnesses st1) of
                                    Nothing -> expectationFailure "Expected edge witness"
                                    Just witness ->
                                        getInstanceOps (ewWitness witness)
                                            `shouldSatisfy` all (not . mentionsNode expNode)

        let acyclicityRes =
                AcyclicityResult
                    { arSortedEdges = [edge]
                    , arDepGraph = undefined
                    }
        case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("full presolution failed: " ++ show err)
            Right result -> do
                let finalConstraint = prConstraint result
                    redirects = prRedirects result
                    sourceForallFinal =
                        IntMap.findWithDefault
                            forallNode
                            (getNodeId forallNode)
                            redirects
                    targetArrowFinal =
                        IntMap.findWithDefault
                            targetArrow
                            (getNodeId targetArrow)
                            redirects
                IntMap.lookup (getNodeId expNode) redirects
                    `shouldSatisfy` maybe False (/= expNode)
                fmap fst (Binding.lookupBindParent finalConstraint (typeRef sourceForallFinal))
                    `shouldBe` Just (genRef (GenNodeId 0))
                fmap fst (Binding.lookupBindParent finalConstraint (typeRef targetArrowFinal))
                    `shouldBe` Just (typeRef outerBinder)
                Binding.checkBindingTree finalConstraint `shouldBe` Right ()

    it "preserves source-domain trace artifacts through final materialization" $ do
        -- Binding-edge mode freezes the paper's source I(r) before chi_e.
        -- Final graph rewriting may remove or redirect those nodes, but it
        -- must not rewrite only part of the provenance bundle.
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            targetArrow = NodeId 5
            rootArrow = NodeId 6

            nodes = nodeMapFromList
                    [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                    , (getNodeId arrow, TyArrow arrow a a)
                    , (getNodeId forallNode, TyForall forallNode arrow)
                    , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                    , (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
                    , (getNodeId targetArrow, TyArrow targetArrow intNode intNode)
                    , (getNodeId rootArrow, TyArrow rootArrow expNode targetArrow)
                    ]

            bindParents =
                bindParentsFromPairs
                    [ (a, forallNode, BindFlex)
                    , (arrow, forallNode, BindFlex)
                    , (forallNode, rootArrow, BindFlex)
                    , (expNode, rootArrow, BindFlex)
                    , (intNode, targetArrow, BindFlex)
                    , (targetArrow, rootArrow, BindFlex)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
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

            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    7
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

            frozenTraceArtifacts tr =
                ( etRoot tr
                , etBinderArgs tr
                , etInterior tr
                , etCopyMap tr
                )

        rawTrace <-
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Left err -> expectationFailure ("Raw edge execution failed: " ++ show err) >> fail "raw edge failed"
                Right (_, rawState) ->
                    maybe
                        (expectationFailure "Expected raw EdgeTrace for EdgeId 0" >> fail "missing raw trace")
                        pure
                        (IntMap.lookup 0 (psEdgeTraces rawState))
        etRoot rawTrace `shouldBe` arrow
        etInterior rawTrace `shouldBe` sourceInteriorFromList [a, arrow]
        copiedNodes (etCopyMap rawTrace)
            `shouldSatisfy` all (`notElem` [a, arrow])
        etBinderArgs rawTrace
            `shouldSatisfy` \case
                [(sourceBinder, _argument)] -> sourceBinder == a
                _ -> False
        case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("Presolution failed: " ++ show err)
            Right PresolutionResult{ prEdgeTraces = traces } -> do
                finalizedTrace <-
                    maybe
                        (expectationFailure "Expected finalized EdgeTrace for EdgeId 0" >> fail "missing finalized trace")
                        pure
                        (IntMap.lookup 0 traces)
                -- etBinderArgs is a mixed-domain bridge: its binder is a
                -- source identity and its argument is the destination
                -- identity selected while constructing chi_e.  Neither side
                -- is replaced merely because final materialization chose a
                -- different solved-graph representative.
                frozenTraceArtifacts finalizedTrace
                    `shouldBe` frozenTraceArtifacts rawTrace

    it "records identity expansion with empty binder-argument trace" $ do
        let body = NodeId 10
            target = NodeId 11
            expNode = NodeId 12
            nBody = TestTyBase body (BaseTy "Int")
            nTarget = TestTyBase target (BaseTy "Int")
            nExp = TyExp expNode (ExpVarId 42) body
            edge = InstEdge (EdgeId 9) expNode target
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList
                        [ (getNodeId body, nBody)
                        , (getNodeId target, nTarget)
                        , (getNodeId expNode, nExp)
                        ]
                    , cInstEdges = [edge]
                    , cBindParents = bindParentsFromPairs
                        [ (body, expNode, BindFlex)
                        , (target, expNode, BindFlex)
                        ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    13
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                IntMap.lookup 9 (psEdgeExpansions st1) `shouldBe` Just ExpIdentity
                case IntMap.lookup 9 (psEdgeTraces st1) of
                    Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 9"
                    Just tr -> etBinderArgs tr `shouldBe` []

    describe "Thesis obligations" $ do
        it "O10-PROP-SOLVE" $ do
            -- Propagation rule: processInstEdge solves a simple inst edge
            let bodyNode = NodeId 0
                targetNode = NodeId 1
                expNode = NodeId 2
                body = TestTyBase bodyNode (BaseTy "Int")
                target = TestTyBase targetNode (BaseTy "Int")
                tyExp = TyExp expNode (ExpVarId 0) bodyNode
                edge = InstEdge (EdgeId 0) expNode targetNode
                nodes = nodeMapFromList [(0, body), (1, target), (2, tyExp)]
                constraint = rootedConstraint $ emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef expNode), (typeRef targetNode, BindFlex))
                        , (nodeRefKey (typeRef bodyNode), (typeRef expNode, BindFlex))
                        ]
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 10 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Right _ -> pure ()
                Left err -> expectationFailure $ "processInstEdge failed: " ++ show err

        it "O10-PROP-WITNESS" $ do
            -- Edge witness recording: processInstEdge records an expansion for the edge
            let bodyNode = NodeId 0
                targetNode = NodeId 1
                expNode = NodeId 2
                body = TestTyBase bodyNode (BaseTy "Int")
                target = TestTyBase targetNode (BaseTy "Int")
                tyExp = TyExp expNode (ExpVarId 0) bodyNode
                edge = InstEdge (EdgeId 0) expNode targetNode
                nodes = nodeMapFromList [(0, body), (1, target), (2, tyExp)]
                constraint = rootedConstraint $ emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef expNode), (typeRef targetNode, BindFlex))
                        , (nodeRefKey (typeRef bodyNode), (typeRef expNode, BindFlex))
                        ]
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 10 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Right (_, st1) ->
                    IntMap.lookup 0 (psEdgeExpansions st1) `shouldSatisfy` \case
                        Just _ -> True
                        Nothing -> False
                Left err -> expectationFailure $ "processInstEdge failed: " ++ show err
