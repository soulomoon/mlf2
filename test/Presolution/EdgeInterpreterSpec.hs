{-# LANGUAGE LambdaCase #-}
module Presolution.EdgeInterpreterSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (catchError)
import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , EdgeExecutionArtifacts(..)
    , psEdgeExecutionArtifacts
    , psEdgeExpansions
    , psEdgeWitnesses
    , psEdgeNonSourceOpOrigins
    , psEdgeTraces
    , EdgeWitnessOp(..)
    , TerminalRootTransition(..)
    , classifyTerminalRootTransitionForTest
    , edgeExpansionExtraOpsForTest
    , insertCopy
    , emptyRawExpansionConstruction
    , lookupCopy
    , lookupExpansionResult
    , requireExpansionResultScopeForTest
    , runEdgeBoundInstallForTest
    , runEdgeTerminalStructureUnifyForTest
    , runIdentityExpansionWithBaseOpsForTest
    , runIdentityStructuralUnificationsForTest
    , runPresolutionM
    , runPresolutionLoopWithOperationTimingForTest
    , sourceWitnessNodeWithCopyMapForTest
    , InteriorNodes(..)
    )
import MLF.Constraint.Presolution
    ( EdgeSourceInterior(..)
    , EdgeTrace(..)
    , PresolutionError(..)
    , prEdgeExpansions
    , prEdgeTraces
    , prEdgeWitnesses
    )
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter
    ( EdgeExecutionDecision(..)
    , executeEdgePlan
    , prepareEdgeExecutionDecision
    , prepareEdgeExecutionWitness
    , recordEdgeExecutionExpansion
    , runEdgeExecutionExpansionUnify
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , Constraint(..)
    , EdgeId(..)
    , ExpVarId(..)
    , GenNode(..)
    , GenNodeId(..)
    , InstEdge(..)
    , NodeId(..)
    , TyNode(..)
    , fromListGen
    , genRef
    , nodeRefKey
    , typeRef
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Witness
    ( Expansion(..)
    , InstanceOp(..)
    , ReplayContract(..)
    )
import MLF.Constraint.Types.Witness.TestSupport
    ( EdgeWitness(..)
    , InstanceWitness(..)
    )
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.Syntax (Expr(..))
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Util.UnionFind as UF
import SpecUtil
    ( PipelineArtifacts(..)
    , bindParentsFromPairs
    , defaultTraceConfig
    , emptyConstraint
    , lookupNodeMaybe
    , nodeMapElems
    , nodeMapFromList
    , requireRight
    , rootedConstraint
    , runPipelineArtifactsDefault
    )
import Test.Hspec

spec :: Spec
spec = describe "Edge interpreter" $ do
    describe "terminal-root transition authority" $ do
        forM_
            [ ("missing/missing", Nothing, Nothing, Nothing)
            , ("missing/flexible", Nothing, Just BindFlex, Nothing)
            , ("missing/rigid", Nothing, Just BindRigid, Nothing)
            , ("flexible/missing", Just BindFlex, Nothing, Nothing)
            , ("flexible/flexible", Just BindFlex, Just BindFlex, Just RootRaiseMerge)
            , ("flexible/rigid", Just BindFlex, Just BindRigid, Just RootWeakenRaiseMerge)
            , ("rigid/missing", Just BindRigid, Nothing, Just RigidRootIdentity)
            , ("rigid/flexible", Just BindRigid, Just BindFlex, Just RigidRootIdentity)
            , ("rigid/rigid", Just BindRigid, Just BindRigid, Just RigidRootIdentity)
            ]
            $ \(label, sourceFlag, targetFlag, expected) ->
                it ("classifies " ++ label ++ " from complete binding evidence") $
                    classifyTerminalRootTransitionForTest sourceFlag targetFlag
                        `shouldBe` expected

        forM_
            [ ( BindFlex
              , BindFlex
              , [OpRaise (NodeId 0), OpMerge (NodeId 0) (NodeId 1)]
              )
            , ( BindFlex
              , BindRigid
              , [ OpWeaken (NodeId 0)
                , OpRaise (NodeId 0)
                , OpMerge (NodeId 0) (NodeId 1)
                ]
              )
            , (BindRigid, BindFlex, [])
            , (BindRigid, BindRigid, [])
            ]
            $ \(sourceFlag, targetFlag, expectedOps) ->
                it
                    ( "executes the "
                        ++ show sourceFlag
                        ++ "/"
                        ++ show targetFlag
                        ++ " terminal-root transition"
                    )
                    $ do
                        let sourceRoot = NodeId 0
                            target = NodeId 1
                            rootGen = GenNodeId 0
                            constraint =
                                rootedConstraint
                                    emptyConstraint
                                        { cNodes =
                                            nodeMapFromList
                                                [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                                                , (getNodeId target, TyVar target Nothing)
                                                ]
                                        , cBindParents =
                                            IntMap.fromList
                                                [ ( nodeRefKey (typeRef sourceRoot)
                                                  , (genRef rootGen, sourceFlag)
                                                  )
                                                , ( nodeRefKey (typeRef target)
                                                  , (genRef rootGen, targetFlag)
                                                  )
                                                ]
                                        }
                            st0 =
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
                        case
                            runPresolutionM
                                defaultTraceConfig
                                st0
                                ( runEdgeTerminalStructureUnifyForTest
                                    sourceRoot
                                    (IntSet.singleton (getNodeId sourceRoot))
                                    sourceRoot
                                    target
                                ) of
                            Left err ->
                                expectationFailure
                                    ("terminal-root transition failed: " ++ show err)
                            Right (ops, _st1) -> ops `shouldBe` expectedOps

    describe "lambda-body terminal-root authority" $ do
        let rootRaiseMergesFor artifacts (EdgeId edgeKey) = do
                let presolution = paPresolution artifacts
                witness <-
                    maybe
                        (expectationFailure "missing lambda-body witness" >> fail "missing witness")
                        pure
                        (IntMap.lookup edgeKey (prEdgeWitnesses presolution))
                trace <-
                    maybe
                        (expectationFailure "missing lambda-body trace" >> fail "missing trace")
                        pure
                        (IntMap.lookup edgeKey (prEdgeTraces presolution))
                pure
                    [ (operated, exterior)
                    | OpRaiseMerge operated exterior <- getInstanceOps (ewWitness witness)
                    , operated == etRoot trace
                    ]

        it "keeps the degenerate identity body edge quotient-only" $ do
            artifacts <-
                requireRight
                    (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
            bodyEdge <-
                case paAnnotated artifacts of
                    ALam _ _ _ _ _ edgeId _ -> pure edgeId
                    other -> do
                        expectationFailure ("expected identity lambda, got " ++ show other)
                        fail "missing identity lambda-body edge"
            let EdgeId edgeKey = bodyEdge
            IntMap.lookup edgeKey (prEdgeExpansions (paPresolution artifacts))
                `shouldBe` Just ExpIdentity
            rootRaiseMergesFor artifacts bodyEdge `shouldReturn` []

        it "constructs Gamma only for the nondegenerate outer K body edge" $ do
            artifacts <-
                requireRight
                    ( runPipelineArtifactsDefault
                        Set.empty
                        (ELam "x" (ELam "y" (EVar "x")))
                    )
            (outerBodyEdge, innerBodyEdge) <-
                case paAnnotated artifacts of
                    ALam _ _ _ _ (ALam _ _ _ _ _ innerEdge _) outerEdge _ ->
                        pure (outerEdge, innerEdge)
                    other -> do
                        expectationFailure ("expected K lambda nest, got " ++ show other)
                        fail "missing K lambda-body edges"
            innerRootTransitions <- rootRaiseMergesFor artifacts innerBodyEdge
            outerRootTransitions <- rootRaiseMergesFor artifacts outerBodyEdge
            innerRootTransitions `shouldBe` []
            case outerRootTransitions of
                [_] -> pure ()
                other -> do
                    let EdgeId outerEdgeKey = outerBodyEdge
                        presolution = paPresolution artifacts
                    expectationFailure
                        ( "expected one outer K root RaiseMerge, got "
                            ++ show other
                            ++ "; expansion="
                            ++ show (IntMap.lookup outerEdgeKey (prEdgeExpansions presolution))
                            ++ "; witness="
                            ++ show (IntMap.lookup outerEdgeKey (prEdgeWitnesses presolution))
                            ++ "; trace="
                            ++ show (IntMap.lookup outerEdgeKey (prEdgeTraces presolution))
                        )

    it "rejects non-TyExp-left edges in planner before interpreter" $ do
        let n0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
            n1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 1) (NodeId 0) (NodeId 1)
            constraint = rootedConstraint emptyConstraint
                { cNodes = nodeMapFromList [(0, n0), (1, n1)]
                , cInstEdges = [edge]
                }
            st0 = PresolutionState constraint (Presolution IntMap.empty)
                IntMap.empty 2 IntSet.empty IntMap.empty
                IntMap.empty IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (planEdge id edge >>= executeEdgePlan id) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected planner fail-fast on non-TyExp left edge"

    it "executes resolved TyExp plans without error" $ do
        let a = NodeId 0
            forallNode = NodeId 1
            expNode = NodeId 2
            target = NodeId 3
            nodes = nodeMapFromList
                [ (0, TyVar { tnId = a, tnBound = Nothing })
                , (1, TyForall forallNode a)
                , (2, TyExp expNode (ExpVarId 0) forallNode)
                , (3, TyVar { tnId = target, tnBound = Nothing })
                ]
            edge = InstEdge (EdgeId 0) expNode target
            constraint = rootedConstraint emptyConstraint
                { cNodes = nodes
                , cInstEdges = [edge]
                , cBindParents = bindParentsFromPairs
                    [ (a, forallNode, BindFlex)
                    , (forallNode, expNode, BindFlex)
                    ]
                }
            st0 = PresolutionState constraint (Presolution IntMap.empty)
                IntMap.empty 4 IntSet.empty IntMap.empty
                IntMap.empty IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (planEdge id edge >>= executeEdgePlan id) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                IntMap.member 0 (psEdgeExpansions st1) `shouldBe` True
                IntMap.member 0 (psEdgeWitnesses st1) `shouldBe` True
                IntMap.member 0 (psEdgeTraces st1) `shouldBe` True

    it "orders copied-child RaiseMerge/Weaken before a rigid terminal-root transition" $ do
        let sourceDom = NodeId 0
            sourceCod = NodeId 1
            sourceArrow = NodeId 2
            sourceRoot = NodeId 3
            expNode = NodeId 4
            targetLeaf = NodeId 5
            targetArrow = NodeId 6
            targetForall = NodeId 7
            graphRoot = NodeId 8
            sourceGen = GenNodeId 10
            edge = InstEdge (EdgeId 0) expNode targetForall
            nodes =
                nodeMapFromList
                    [ (0, TyBottom sourceDom)
                    , (1, TyBottom sourceCod)
                    , (2, TyArrow sourceArrow sourceDom sourceCod)
                    , (3, TyVar sourceRoot (Just sourceArrow))
                    , (4, TyExp expNode (ExpVarId 0) sourceRoot)
                    , (5, TyBottom targetLeaf)
                    , (6, TyArrow targetArrow targetLeaf targetLeaf)
                    , (7, TyForall targetForall targetArrow)
                    , (8, TyArrow graphRoot expNode targetForall)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (genRef sourceGen), (genRef (GenNodeId 0), BindFlex))
                    , (nodeRefKey (typeRef sourceDom), (genRef (GenNodeId 0), BindFlex))
                    , (nodeRefKey (typeRef sourceCod), (genRef (GenNodeId 0), BindFlex))
                    , (nodeRefKey (typeRef sourceArrow), (typeRef sourceRoot, BindFlex))
                    , (nodeRefKey (typeRef sourceRoot), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef targetLeaf), (typeRef targetForall, BindRigid))
                    , (nodeRefKey (typeRef targetArrow), (typeRef targetForall, BindRigid))
                    , (nodeRefKey (typeRef targetForall), (genRef (GenNodeId 0), BindRigid))
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        , cGenNodes =
                            fromListGen
                                [ (sourceGen, GenNode sourceGen [sourceRoot])
                                ]
                        }
            st0 =
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
            action = do
                plan <- planEdge id edge
                decision <- prepareEdgeExecutionDecision id plan
                recordEdgeExecutionExpansion decision
                executionContext <- prepareEdgeExecutionWitness decision
                result <- runEdgeExecutionExpansionUnify executionContext
                pure (decision, reverse (edgeExpansionExtraOpsForTest result))

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("rigid terminal expansion failed: " ++ show err)
            Right ((decision, extraOps), _st1) -> do
                eedBodyRoot decision `shouldBe` sourceRoot
                [ op
                  | SourceEdgeWitnessOp op <- extraOps
                  ]
                    `shouldBe`
                        [ OpRaise sourceArrow
                        , OpMerge sourceArrow targetArrow
                        , OpWeaken sourceArrow
                        , OpWeaken sourceRoot
                        , OpRaise sourceRoot
                        , OpMerge sourceRoot targetArrow
                        ]

    it "does not emit witness operations for locked children or an already-rigid terminal source root" $ do
        let sourceDom = NodeId 0
            sourceCod = NodeId 1
            sourceArrow = NodeId 2
            sourceRoot = NodeId 3
            expNode = NodeId 4
            targetLeaf = NodeId 5
            targetArrow = NodeId 6
            targetForall = NodeId 7
            graphRoot = NodeId 8
            sourceGen = GenNodeId 10
            edge = InstEdge (EdgeId 0) expNode targetForall
            nodes =
                nodeMapFromList
                    [ (0, TyBottom sourceDom)
                    , (1, TyBottom sourceCod)
                    , (2, TyArrow sourceArrow sourceDom sourceCod)
                    , (3, TyVar sourceRoot (Just sourceArrow))
                    , (4, TyExp expNode (ExpVarId 0) sourceRoot)
                    , (5, TyBottom targetLeaf)
                    , (6, TyArrow targetArrow targetLeaf targetLeaf)
                    , (7, TyForall targetForall targetArrow)
                    , (8, TyArrow graphRoot expNode targetForall)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (genRef sourceGen), (genRef (GenNodeId 0), BindFlex))
                    , (nodeRefKey (typeRef sourceDom), (genRef (GenNodeId 0), BindFlex))
                    , (nodeRefKey (typeRef sourceCod), (genRef (GenNodeId 0), BindFlex))
                    , (nodeRefKey (typeRef sourceArrow), (typeRef sourceRoot, BindFlex))
                    , (nodeRefKey (typeRef sourceRoot), (genRef sourceGen, BindRigid))
                    , (nodeRefKey (typeRef targetLeaf), (typeRef targetForall, BindRigid))
                    , (nodeRefKey (typeRef targetArrow), (typeRef targetForall, BindRigid))
                    , (nodeRefKey (typeRef targetForall), (genRef (GenNodeId 0), BindRigid))
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        , cGenNodes =
                            fromListGen
                                [ (sourceGen, GenNode sourceGen [sourceRoot])
                                ]
                        }
            st0 =
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
            action = do
                plan <- planEdge id edge
                decision <- prepareEdgeExecutionDecision id plan
                recordEdgeExecutionExpansion decision
                executionContext <- prepareEdgeExecutionWitness decision
                result <- runEdgeExecutionExpansionUnify executionContext
                pure (reverse (edgeExpansionExtraOpsForTest result))

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("already-rigid terminal expansion failed: " ++ show err)
            Right (extraOps, _st1) ->
                [ op
                | SourceEdgeWitnessOp op <- extraOps
                ]
                    `shouldBe` []

    it "rejects an edge-bound Raise outside the source interior transactionally" $ do
        let meta = NodeId 0
            boundRoot = NodeId 1
            boundLeaf = NodeId 2
            leftOwner = NodeId 3
            edgeRoot = NodeId 4
            rightOwner = NodeId 5
            constraint =
                rootedConstraint $
                  emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId meta, TyVar meta Nothing)
                            , (getNodeId boundRoot, TyArrow boundRoot boundLeaf boundLeaf)
                            , (getNodeId boundLeaf, TyVar boundLeaf Nothing)
                            , (getNodeId leftOwner, TyArrow leftOwner meta meta)
                            , (getNodeId edgeRoot, TyArrow edgeRoot leftOwner rightOwner)
                            , (getNodeId rightOwner, TyArrow rightOwner boundRoot boundRoot)
                            ]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (meta, leftOwner, BindFlex)
                            , (boundLeaf, boundRoot, BindFlex)
                            , (boundRoot, rightOwner, BindFlex)
                            , (leftOwner, edgeRoot, BindFlex)
                            , (rightOwner, edgeRoot, BindFlex)
                            ]
                    }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    IntMap.empty
                    6
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        let action =
                catchError
                    ( Right
                        <$> runEdgeBoundInstallForTest
                            edgeRoot
                            IntSet.empty
                            meta
                            boundRoot
                    )
                    (pure . Left)
        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("failed to catch edge error: " ++ show err)
            Right (outcome, st1) -> do
                case outcome of
                    Left (EdgeBoundRaiseOutsideInterior rejectedRoot outside) -> do
                        rejectedRoot `shouldBe` edgeRoot
                        outside `shouldContain` [boundRoot]
                    Left err -> expectationFailure ("unexpected outside-interior error: " ++ show err)
                    Right _ -> expectationFailure "expected outside-interior bound rejection"
                -- PresolutionM is transactional on failure: the attempted
                -- target-side Raise and all partially-recorded artifacts roll
                -- back together.
                psConstraint st1 `shouldBe` constraint
                psPresolution st1 `shouldBe` Presolution IntMap.empty
                psEdgeExpansions st1 `shouldBe` IntMap.empty
                psEdgeWitnesses st1 `shouldBe` IntMap.empty
                psEdgeTraces st1 `shouldBe` IntMap.empty

    it "records and replays a nonempty source-interior Raise for ExpIdentity" $ do
        let source = NodeId 0
            sourceParent = NodeId 1
            target = NodeId 2
            edgeRoot = NodeId 3
            wrapper = NodeId 4
            edgeId = EdgeId 0
            edge = InstEdge edgeId wrapper target
            constraint =
                rootedConstraint $
                  emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId source, TyVar source Nothing)
                            , (getNodeId sourceParent, TyArrow sourceParent source source)
                            , (getNodeId target, TyVar target Nothing)
                            , (getNodeId edgeRoot, TyArrow edgeRoot sourceParent target)
                            , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) source)
                            ]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (source, sourceParent, BindFlex)
                            , (sourceParent, edgeRoot, BindFlex)
                            , (target, edgeRoot, BindFlex)
                            , (wrapper, edgeRoot, BindFlex)
                            ]
                    , cInstEdges = [edge]
                    }
            st0 =
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

        case runPresolutionM defaultTraceConfig st0 (planEdge id edge >>= executeEdgePlan id) of
            Left err -> expectationFailure ("identity source Raise failed: " ++ show err)
            Right ((), st1) -> do
                IntMap.lookup (getEdgeId edgeId) (psEdgeExpansions st1)
                    `shouldBe` Just ExpIdentity
                witness <-
                    maybe
                        (expectationFailure "missing identity-edge witness" >> fail "missing witness")
                        pure
                        (IntMap.lookup (getEdgeId edgeId) (psEdgeWitnesses st1))
                trace <-
                    maybe
                        (expectationFailure "missing identity-edge trace" >> fail "missing trace")
                        pure
                        (IntMap.lookup (getEdgeId edgeId) (psEdgeTraces st1))
                let ops = getInstanceOps (ewWitness witness)
                    EdgeSourceInterior (InteriorNodes interior) = etInterior trace
                    opIsInside op =
                        case op of
                            OpRaise node -> IntSet.member (getNodeId node) interior
                            OpRaiseMerge node other ->
                                IntSet.member (getNodeId node) interior
                                    && IntSet.member (getNodeId other) interior
                            _ -> True
                ops `shouldContain` [OpRaise source]
                IntMap.lookup
                    (getEdgeId edgeId)
                    (psEdgeNonSourceOpOrigins st1)
                    `shouldBe` Just IntMap.empty
                ops `shouldSatisfy` all opIsInside
                Binding.lookupBindParent (psConstraint st1) (typeRef source)
                    `shouldBe` Just (typeRef edgeRoot, BindFlex)

                let canonical = UF.frWith (psUnionFind st1)
                    edgeKey = getEdgeId edgeId
                case runPresolutionM defaultTraceConfig st1 (planEdge canonical edge >>= executeEdgePlan canonical) of
                    Left err -> expectationFailure ("identity Raise replay failed: " ++ show err)
                    Right ((), st2) -> do
                        st2 `shouldBe` st1
                        IntMap.lookup (getEdgeId edgeId) (psEdgeWitnesses st2)
                            `shouldBe` Just witness
                        IntMap.lookup (getEdgeId edgeId) (psEdgeTraces st2)
                            `shouldBe` Just trace
                artifacts <-
                    maybe
                        (expectationFailure "missing complete identity-edge artifacts" >> fail "missing artifacts")
                        pure
                        (IntMap.lookup edgeKey (psEdgeExecutionArtifacts st1))
                let conflictingArtifacts =
                        [ artifacts {eeaExpansion = ExpInstantiate []}
                        , artifacts
                            { eeaWitness = witness {ewLeft = target}
                            }
                        , artifacts
                            { eeaTrace = trace {etRoot = edgeRoot}
                            }
                        ]
                forM_ conflictingArtifacts $ \conflicting -> do
                    let conflictingState =
                            st1
                                { psEdgeExecutionArtifacts =
                                    IntMap.insert
                                        edgeKey
                                        conflicting
                                        (psEdgeExecutionArtifacts st1)
                                }
                    case
                        runPresolutionM
                            defaultTraceConfig
                            conflictingState
                            (planEdge canonical edge >>= executeEdgePlan canonical)
                      of
                        Left (ExecError (InternalError message)) ->
                            message `shouldContain` "conflicting committed edge execution artifacts"
                        Left err ->
                            expectationFailure
                                ("expected conflicting replay error, saw " ++ show err)
                        Right _ ->
                            expectationFailure
                                "conflicting committed replay artifacts were re-executed"

    it "matches ExpIdentity bounds before a restricted owner quotient can lock them" $ do
        let rootGen = GenNodeId 0
            sourceGen = GenNodeId 1
            sourceRoot = NodeId 0
            sourceVar = NodeId 1
            sourceBound = NodeId 2
            sourceArrow = NodeId 3
            wrapper = NodeId 4
            targetVar = NodeId 5
            targetBound = NodeId 6
            targetArrow = NodeId 7
            edgeId = EdgeId 0
            edge = InstEdge edgeId wrapper targetArrow
            sourceInteriorSet =
                IntSet.fromList
                    ( map
                        getNodeId
                        [ sourceRoot
                        , sourceVar
                        , sourceBound
                        , sourceArrow
                        ]
                    )
            sourceNodeKeys =
                IntSet.fromList
                    (map getNodeId [sourceRoot, sourceVar, sourceBound, sourceArrow, wrapper, targetVar, targetBound, targetArrow])
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId sourceRoot, TyVar sourceRoot (Just sourceArrow))
                                , (getNodeId sourceVar, TyVar sourceVar (Just sourceBound))
                                , (getNodeId sourceBound, TyBottom sourceBound)
                                , (getNodeId sourceArrow, TyArrow sourceArrow sourceVar sourceVar)
                                , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) sourceRoot)
                                , (getNodeId targetVar, TyVar targetVar (Just targetBound))
                                , (getNodeId targetBound, TyBottom targetBound)
                                , (getNodeId targetArrow, TyArrow targetArrow targetVar targetVar)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (genRef sourceGen), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef sourceRoot), (genRef sourceGen, BindFlex))
                                , (nodeRefKey (typeRef sourceVar), (typeRef sourceRoot, BindFlex))
                                , (nodeRefKey (typeRef sourceBound), (typeRef sourceVar, BindFlex))
                                , (nodeRefKey (typeRef sourceArrow), (typeRef sourceRoot, BindFlex))
                                , (nodeRefKey (typeRef wrapper), (genRef sourceGen, BindFlex))
                                , (nodeRefKey (typeRef targetVar), (typeRef targetArrow, BindRigid))
                                , (nodeRefKey (typeRef targetBound), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef targetArrow), (genRef rootGen, BindRigid))
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (sourceGen, GenNode sourceGen [sourceRoot])
                                ]
                        , cInstEdges = [edge]
                        }
            st0 =
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

        Binding.nodeKind constraint (typeRef sourceBound)
            `shouldBe` Right Binding.NodeInstantiable
        Binding.nodeKind constraint (typeRef targetVar)
            `shouldBe` Right Binding.NodeRestricted
        case
            runPresolutionM
                defaultTraceConfig
                st0
                ( runIdentityStructuralUnificationsForTest
                    sourceGen
                    edgeId
                    (TyExp wrapper (ExpVarId 0) sourceRoot)
                    (TyArrow targetArrow targetVar targetVar)
                    sourceRoot
                    (EdgeSourceInterior (InteriorNodes sourceInteriorSet))
                    sourceInteriorSet
                    sourceNodeKeys
                    [(sourceVar, targetVar)]
                ) of
            Left err ->
                expectationFailure
                    ("children-first ExpIdentity bound matching failed: " ++ show err)
            Right (_result, st1) -> do
                let canonical = UF.frWith (psUnionFind st1)
                    finalConstraint = psConstraint st1
                canonical sourceBound `shouldBe` canonical targetBound
                canonical sourceVar `shouldBe` canonical targetVar
                Binding.nodeKind finalConstraint (typeRef (canonical sourceBound))
                    `shouldBe` Right Binding.NodeInstantiable
                Binding.checkBindingTreeUnder canonical finalConstraint
                    `shouldBe` Right ()

    it "uses the current edge source identity over historical many-to-one copies" $ do
        let outerGen = GenNodeId 0
            sourceGen = GenNodeId 1
            sourceRoot = NodeId 0
            target = NodeId 1
            wrapper = NodeId 2
            historicalSourceA = NodeId 3
            historicalSourceB = NodeId 4
            edgeId = EdgeId 2
            edge = InstEdge edgeId wrapper target
            leftTyExp =
                ResolvedTyExp
                    { rteNodeId = wrapper
                    , rteExpVar = ExpVarId 0
                    , rteBodyId = sourceRoot
                    }
            plan =
                mkEmptyResolvedPlan
                    edge
                    leftTyExp
                    (TyVar target Nothing)
                    wrapper
                    target
                    sourceGen
            constraint =
                emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                            , (getNodeId target, TyVar target Nothing)
                            , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) sourceRoot)
                            , (getNodeId historicalSourceA, TyVar historicalSourceA Nothing)
                            , (getNodeId historicalSourceB, TyVar historicalSourceB Nothing)
                            ]
                    , cGenNodes =
                        fromListGen
                            [ (outerGen, GenNode outerGen [target])
                            , (sourceGen, GenNode sourceGen [sourceRoot])
                            ]
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (genRef sourceGen), (genRef outerGen, BindFlex))
                            , (nodeRefKey (typeRef sourceRoot), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef wrapper), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef target), (genRef outerGen, BindFlex))
                            , (nodeRefKey (typeRef historicalSourceA), (genRef outerGen, BindFlex))
                            , (nodeRefKey (typeRef historicalSourceB), (genRef outerGen, BindFlex))
                            ]
                    , cInstEdges = [edge]
                    }
            historicalTrace source =
                EdgeTrace
                    { etRoot = source
                    , etResultRoot = target
                    , etBinderArgs = []
                    , etInterior = EdgeSourceInterior (InteriorNodes (IntSet.singleton (getNodeId source)))
                    , etReplayContract = ReplayContractNone
                    , etBinderReplayMap = IntMap.empty
                    , etReplayDomainBinders = []
                    , etCopyMap = insertCopy source target mempty
                    }
            historicalTraces =
                IntMap.fromList
                    [ (0, historicalTrace historicalSourceA)
                    , (1, historicalTrace historicalSourceB)
                    ]
            historicalArtifacts =
                IntMap.mapWithKey
                    (\edgeKey trace ->
                        EdgeExecutionArtifacts
                            { eeaExpansion = ExpIdentity
                            , eeaWitness =
                                EdgeWitness
                                    { ewEdgeId = EdgeId edgeKey
                                    , ewLeft = etRoot trace
                                    , ewRight = target
                                    , ewRoot = etRoot trace
                                    , ewForallIntros = 0
                                    , ewWitness = InstanceWitness []
                                    }
                            , eeaRaiseAuthorityNodes = IntSet.empty
                            , eeaNonSourceOpOrigins = IntMap.empty
                            , eeaExpansionConstruction =
                                emptyRawExpansionConstruction
                            , eeaTrace = trace
                            }
                    )
                    historicalTraces
            st0 =
                ( PresolutionState
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
                )
                    { psEdgeExecutionArtifacts = historicalArtifacts
                    }

        case runPresolutionM defaultTraceConfig st0 (executeEdgePlan id plan) of
            Left err -> expectationFailure ("current source identity was obscured: " ++ show err)
            Right ((), st1) -> do
                witness <-
                    maybe
                        (expectationFailure "missing current-edge witness" >> fail "missing witness")
                        pure
                        (IntMap.lookup (getEdgeId edgeId) (psEdgeWitnesses st1))
                getInstanceOps (ewWitness witness)
                    `shouldSatisfy` \ops ->
                        [ exterior
                        | OpMerge operated exterior <- ops
                        , operated == sourceRoot
                        ]
                            == [target]

    it "resolves a fresh terminal destination through the current edge copy map" $ do
        let sourceRoot = NodeId 0
            exteriorSource = NodeId 1
            copiedExterior = NodeId 2
            copyMap =
                insertCopy exteriorSource copiedExterior mempty
            sourceNodeKeys =
                IntSet.fromList
                    [ getNodeId sourceRoot
                    , getNodeId exteriorSource
                    ]
            sourceInterior =
                EdgeSourceInterior
                    (InteriorNodes (IntSet.singleton (getNodeId sourceRoot)))
            constraint =
                rootedConstraint
                    emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                            , (getNodeId exteriorSource, TyVar exteriorSource Nothing)
                            , (getNodeId copiedExterior, TyVar copiedExterior Nothing)
                            ]
                    }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    IntMap.empty
                    3
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case
            runPresolutionM
                defaultTraceConfig
                st0
                ( sourceWitnessNodeWithCopyMapForTest
                    copyMap
                    sourceNodeKeys
                    sourceRoot
                    sourceInterior
                    copiedExterior
                ) of
            Left err -> expectationFailure ("fresh copy provenance failed: " ++ show err)
            Right (source, _) -> source `shouldBe` Just exteriorSource

    it "prefers copy provenance when a pre-quotiented destination is also a frozen source node" $ do
        let sourceRoot = NodeId 0
            copiedSource = NodeId 1
            reusedDestination = NodeId 2
            rawCopyDestination = NodeId 3
            copyMap = insertCopy copiedSource rawCopyDestination mempty
            sourceNodeKeys =
                IntSet.fromList
                    [ getNodeId sourceRoot
                    , getNodeId copiedSource
                    , getNodeId reusedDestination
                    ]
            sourceInterior =
                EdgeSourceInterior
                    (InteriorNodes (IntSet.singleton (getNodeId sourceRoot)))
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                                , (getNodeId copiedSource, TyVar copiedSource Nothing)
                                , (getNodeId reusedDestination, TyVar reusedDestination Nothing)
                                , (getNodeId rawCopyDestination, TyVar rawCopyDestination Nothing)
                                ]
                        }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    (IntMap.singleton (getNodeId rawCopyDestination) reusedDestination)
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case
            runPresolutionM
                defaultTraceConfig
                st0
                ( sourceWitnessNodeWithCopyMapForTest
                    copyMap
                    sourceNodeKeys
                    sourceRoot
                    sourceInterior
                    reusedDestination
                ) of
            Left err -> expectationFailure ("reused copy provenance failed: " ++ show err)
            Right (source, _) -> source `shouldBe` Just copiedSource

    it "keeps a copied source identity frozen after its live class aliases a child" $ do
        let sourceChild = NodeId 0
            sourceRoot = NodeId 1
            destination = NodeId 2
            copyMap = insertCopy sourceRoot destination mempty
            sourceNodeKeys =
                IntSet.fromList
                    [ getNodeId sourceChild
                    , getNodeId sourceRoot
                    ]
            sourceInterior =
                EdgeSourceInterior
                    ( InteriorNodes
                        ( IntSet.fromList
                            [ getNodeId sourceChild
                            , getNodeId sourceRoot
                            ]
                        )
                    )
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId sourceChild, TyVar sourceChild Nothing)
                                , (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                                , (getNodeId destination, TyVar destination Nothing)
                                ]
                        }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    (IntMap.singleton (getNodeId sourceRoot) sourceChild)
                    3
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case
            runPresolutionM
                defaultTraceConfig
                st0
                ( sourceWitnessNodeWithCopyMapForTest
                    copyMap
                    sourceNodeKeys
                    sourceRoot
                    sourceInterior
                    destination
                ) of
            Left err -> expectationFailure ("frozen source provenance failed: " ++ show err)
            Right (source, _) -> source `shouldBe` Just sourceRoot

    it "rejects ambiguous frozen copy sources even when live UF aliases them" $ do
        let sourceRoot = NodeId 0
            sourceAlias = NodeId 1
            canonicalSource = NodeId 2
            destination = NodeId 3
            copyMap =
                insertCopy canonicalSource destination $
                    insertCopy sourceAlias destination mempty
            sourceNodeKeys =
                IntSet.fromList
                    [ getNodeId sourceRoot
                    , getNodeId sourceAlias
                    , getNodeId canonicalSource
                    ]
            sourceInterior =
                EdgeSourceInterior
                    (InteriorNodes (IntSet.singleton (getNodeId sourceRoot)))
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                                , (getNodeId sourceAlias, TyVar sourceAlias Nothing)
                                , (getNodeId canonicalSource, TyVar canonicalSource Nothing)
                                , (getNodeId destination, TyVar destination Nothing)
                                ]
                        }
            stateWith uf =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    uf
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            resolve st =
                runPresolutionM
                    defaultTraceConfig
                    st
                    ( sourceWitnessNodeWithCopyMapForTest
                        copyMap
                        sourceNodeKeys
                        sourceRoot
                        sourceInterior
                        destination
                    )

        forM_
            [ stateWith (IntMap.singleton (getNodeId sourceAlias) canonicalSource)
            , stateWith IntMap.empty
            ]
            $ \state ->
                case resolve state of
                    Left (InternalError message) ->
                        message `shouldSatisfy` isInfixOf "ambiguous construction-time copy source"
                    Left err -> expectationFailure ("expected exact copy ambiguity, got: " ++ show err)
                    Right _ -> expectationFailure "distinct frozen construction sources were accepted"

    it "rejects base operations on ExpIdentity before edge execution" $ do
        let body = NodeId 0
            wrapper = NodeId 1
            target = NodeId 2
            edgeId = EdgeId 7
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId body, TyVar body Nothing)
                                , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) body)
                                , (getNodeId target, TyVar target Nothing)
                                ]
                        }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    IntMap.empty
                    3
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            baseOps = [OpRaise body]
        runPresolutionM
            defaultTraceConfig
            st0
            ( runIdentityExpansionWithBaseOpsForTest
                (GenNodeId 0)
                edgeId
                (TyExp wrapper (ExpVarId 0) body)
                (TyVar target Nothing)
                body
                baseOps
            )
            `shouldBe` Left (IdentityExpansionHasBaseOps edgeId baseOps)

    it "keeps decomposed operation timing semantically equal to fused edge execution" $ do
        let binder = NodeId 0
            sourceArrow = NodeId 1
            sourceForall = NodeId 2
            wrapper = NodeId 3
            targetLeaf = NodeId 4
            targetArrow = NodeId 5
            edge = InstEdge (EdgeId 0) wrapper targetArrow
            constraint =
                rootedConstraint $
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId binder, TyVar binder Nothing)
                                , (getNodeId sourceArrow, TyArrow sourceArrow binder binder)
                                , (getNodeId sourceForall, TyForall sourceForall sourceArrow)
                                , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) sourceForall)
                                , (getNodeId targetLeaf, TyBottom targetLeaf)
                                , (getNodeId targetArrow, TyArrow targetArrow targetLeaf targetLeaf)
                                ]
                        , cBindParents =
                            bindParentsFromPairs
                                [ (binder, sourceForall, BindFlex)
                                , (sourceArrow, sourceForall, BindFlex)
                                , (sourceForall, wrapper, BindFlex)
                                , (targetLeaf, targetArrow, BindFlex)
                                ]
                        , cInstEdges = [edge]
                        }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    IntMap.empty
                    6
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        fused <-
            runPresolutionLoopWithOperationTimingForTest
                False
                "edge-interpreter-fused"
                defaultTraceConfig
                [edge]
                st0
        timed <-
            runPresolutionLoopWithOperationTimingForTest
                True
                "edge-interpreter-decomposed"
                defaultTraceConfig
                [edge]
                st0
        case (fused, timed) of
            (Right ((), fusedState), Right ((), timedState)) ->
                timedState `shouldBe` fusedState
            (Left err, _) -> expectationFailure ("fused edge execution failed: " ++ show err)
            (_, Left err) -> expectationFailure ("timed edge execution failed: " ++ show err)

    it "allows a degenerate annotation copy to raise along its owner path during omega" $ do
        let outerGen = GenNodeId 0
            annotationGen = GenNodeId 1
            body = NodeId 0
            bodyBound = NodeId 1
            boundDom = NodeId 2
            boundCod = NodeId 3
            wrapper = NodeId 4
            target = NodeId 5
            edgeId = EdgeId 0
            edge = InstEdge edgeId wrapper target
            nodes =
                nodeMapFromList
                    [ (getNodeId body, TyVar body (Just bodyBound))
                    , (getNodeId bodyBound, TyArrow bodyBound boundDom boundCod)
                    , (getNodeId boundDom, TyBottom boundDom)
                    , (getNodeId boundCod, TyBottom boundCod)
                    , (getNodeId wrapper, TyExp wrapper (ExpVarId 0) body)
                    , (getNodeId target, TyVar target (Just bodyBound))
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef body), (genRef outerGen, BindFlex))
                    , (nodeRefKey (typeRef bodyBound), (typeRef body, BindFlex))
                    , (nodeRefKey (typeRef boundDom), (typeRef bodyBound, BindFlex))
                    , (nodeRefKey (typeRef boundCod), (typeRef bodyBound, BindFlex))
                    , (nodeRefKey (genRef annotationGen), (genRef outerGen, BindFlex))
                    , (nodeRefKey (typeRef wrapper), (genRef annotationGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef annotationGen, BindRigid))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cGenNodes =
                        fromListGen
                            [ (outerGen, GenNode outerGen [body])
                            , (annotationGen, GenNode annotationGen [target])
                            ]
                    , cBindParents = bindParents
                    , cInstEdges = [edge]
                    , cAnnEdges = IntSet.singleton (getEdgeId edgeId)
                    }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    IntMap.empty
                    6
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (planEdge id edge >>= executeEdgePlan id) of
            Left err -> expectationFailure ("degenerate annotation edge failed: " ++ show err)
            Right ((), st1) -> do
                IntMap.lookup (getEdgeId edgeId) (psEdgeExpansions st1)
                    `shouldBe` Just (ExpInstantiate [])
                trace <-
                    maybe
                        (expectationFailure "missing degenerate annotation edge trace" >> fail "missing trace")
                        pure
                        (IntMap.lookup (getEdgeId edgeId) (psEdgeTraces st1))
                copiedBody <-
                    maybe
                        (expectationFailure "missing degenerate body copy" >> fail "missing copy")
                        pure
                        (lookupCopy body (etCopyMap trace))
                lookupNodeMaybe (cNodes (psConstraint st1)) copiedBody
                    `shouldBe` Just (TyBottom copiedBody)
                let canonical = UF.frWith (psUnionFind st1)
                    resultRoot = canonical copiedBody
                etResultRoot trace `shouldBe` resultRoot
                Binding.lookupBindParentUnder
                    canonical
                    (psConstraint st1)
                    (typeRef resultRoot)
                    `shouldSatisfy` \case
                        Right (Just (owner, _flag)) -> owner == genRef outerGen
                        _ -> False
                Binding.checkBindingTreeUnder canonical (psConstraint st1)
                    `shouldBe` Right ()

    it "rejects a post-omega expansion owner outside the frozen ancestor path" $ do
        let outerGen = GenNodeId 0
            destinationGen = GenNodeId 1
            siblingGen = GenNodeId 2
            result = NodeId 0
            constraint =
                emptyConstraint
                    { cNodes = nodeMapFromList [(getNodeId result, TyBottom result)]
                    , cGenNodes =
                        fromListGen
                            [ (outerGen, GenNode outerGen [])
                            , (destinationGen, GenNode destinationGen [])
                            , (siblingGen, GenNode siblingGen [result])
                            ]
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (genRef destinationGen), (genRef outerGen, BindFlex))
                            , (nodeRefKey (genRef siblingGen), (genRef outerGen, BindFlex))
                            , (nodeRefKey (typeRef result), (genRef siblingGen, BindFlex))
                            ]
                    }
            st0 =
                PresolutionState
                    constraint
                    (Presolution IntMap.empty)
                    IntMap.empty
                    1
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                requireExpansionResultScopeForTest
                    result
                    [genRef destinationGen, genRef outerGen]

        case runPresolutionM defaultTraceConfig st0 action of
            Left (InternalError message) ->
                message `shouldSatisfy` isInfixOf "outside its construction-owner ancestor path"
            Left err -> expectationFailure ("expected monotone owner rejection, got " ++ show err)
            Right _ -> expectationFailure "expected sibling owner to be rejected"

    it "executes synthesized-wrapper TyExp plans" $ do
        let body = NodeId 0
            expNode = NodeId 1
            target = NodeId 2
            nBody = TyVar { tnId = body, tnBound = Nothing }
            nExp = TyExp { tnId = expNode, tnExpVar = ExpVarId (-1), tnBody = body }
            nTarget = TyVar { tnId = target, tnBound = Nothing }
            edge = InstEdge (EdgeId 11) expNode target
            leftTyExp = ResolvedTyExp
                { rteNodeId = expNode
                , rteExpVar = ExpVarId (-1)
                , rteBodyId = body
                }
            plan = mkEmptyResolvedPlan edge leftTyExp nTarget expNode target (GenNodeId 0)
            constraint = rootedConstraint emptyConstraint
                { cNodes = nodeMapFromList
                    [ (getNodeId body, nBody)
                    , (getNodeId expNode, nExp)
                    , (getNodeId target, nTarget)
                    ]
                , cInstEdges = [edge]
                }
            st0 = PresolutionState constraint (Presolution IntMap.empty)
                IntMap.empty 3 IntSet.empty IntMap.empty
                IntMap.empty IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (executeEdgePlan id plan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                let Presolution assignments = psPresolution st1
                IntMap.member (-1) assignments `shouldBe` True

    it "executes synthesized-wrapper plans against forall targets without forcing a specific expansion form" $ do
        let body = NodeId 0
            expNode = NodeId 1
            targetBody = NodeId 2
            targetForall = NodeId 3
            nBody = TyVar { tnId = body, tnBound = Nothing }
            nExp = TyExp { tnId = expNode, tnExpVar = ExpVarId (-2), tnBody = body }
            nTargetBody = TyVar { tnId = targetBody, tnBound = Nothing }
            nTargetForall = TyForall { tnId = targetForall, tnBody = targetBody }
            edge = InstEdge (EdgeId 12) expNode targetForall
            leftTyExp = ResolvedTyExp
                { rteNodeId = expNode
                , rteExpVar = ExpVarId (-2)
                , rteBodyId = body
                }
            plan = mkEmptyResolvedPlan edge leftTyExp nTargetForall expNode targetForall (GenNodeId 0)
            constraint = rootedConstraint emptyConstraint
                { cNodes = nodeMapFromList
                    [ (getNodeId body, nBody)
                    , (getNodeId expNode, nExp)
                    , (getNodeId targetBody, nTargetBody)
                    , (getNodeId targetForall, nTargetForall)
                    ]
                , cInstEdges = [edge]
                }
            st0 = PresolutionState constraint (Presolution IntMap.empty)
                IntMap.empty 4 IntSet.empty IntMap.empty
                IntMap.empty IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (executeEdgePlan id plan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                let Presolution assignments = psPresolution st1
                IntMap.lookup (-2) assignments `shouldSatisfy` (/= Nothing)

    it "constructs forall expansion beside an identity target wrapper without entering wrapper UF" $ do
        let body = NodeId 0
            targetForall = NodeId 1
            sourceWrapper = NodeId 2
            targetWrapper = NodeId 3
            edge = InstEdge (EdgeId 13) sourceWrapper targetWrapper
            constraint =
                rootedConstraint $
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId body, TyVar body Nothing)
                                , (getNodeId targetForall, TyForall targetForall body)
                                , (getNodeId sourceWrapper, TyExp sourceWrapper (ExpVarId (-3)) body)
                                , (getNodeId targetWrapper, TyExp targetWrapper (ExpVarId 0) targetForall)
                                ]
                        , cInstEdges = [edge]
                        , cBindParents =
                            IntMap.insert
                                (nodeRefKey (typeRef targetForall))
                                (genRef (GenNodeId 0), BindFlex)
                                ( bindParentsFromPairs
                                    [ (body, targetForall, BindFlex)
                                    ]
                                )
                        }
            st0 =
                PresolutionState
                    constraint
                    (Presolution (IntMap.singleton 0 ExpIdentity))
                    IntMap.empty
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        Binding.lookupBindParent constraint (typeRef body)
            `shouldBe` Just (typeRef targetForall, BindFlex)
        Binding.lookupBindParent constraint (typeRef targetForall)
            `shouldBe` Just (genRef (GenNodeId 0), BindFlex)
        case runPresolutionM defaultTraceConfig st0 (planEdge id edge >>= executeEdgePlan id) of
            Left err -> expectationFailure ("identity-target forall expansion failed: " ++ show err)
            Right ((), st1) -> do
                let uf = psUnionFind st1
                    constraint1 = psConstraint st1
                UF.frWith uf sourceWrapper `shouldBe` sourceWrapper
                UF.frWith uf targetWrapper `shouldBe` targetWrapper
                Binding.checkBindingTree constraint1 `shouldBe` Right ()
                lookupNodeMaybe (cNodes constraint1) targetForall
                    `shouldBe` Just (TyForall targetForall body)
                IntMap.lookup 13 (psEdgeExpansions st1)
                    `shouldSatisfy` \case
                        Just ExpForall{} -> True
                        _ -> False
                case IntMap.lookup 13 (psEdgeTraces st1) of
                    Nothing -> expectationFailure "missing forall edge trace"
                    Just trace -> do
                        copiedBody <-
                            maybe
                                (expectationFailure "missing destination-owned body copy" >> fail "missing copy")
                                pure
                                (lookupCopy body (etCopyMap trace))
                        copiedBody `shouldNotBe` body
                        [ forallId
                            | TyForall {tnId = forallId, tnBody = copiedForallBody} <-
                                nodeMapElems (cNodes constraint1)
                            , copiedForallBody == copiedBody
                            , forallId /= targetForall
                            ]
                            `shouldSatisfy` (not . null)
                        etResultRoot trace `shouldBe` UF.frWith uf targetForall
                lookupExpansionResult sourceWrapper (psExpansionResults st1)
                    `shouldBe` Just (UF.frWith uf targetForall)
