module Presolution.EdgePlannerSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.Presolution (PresolutionError(..))
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , runPresolutionM
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Presolution.EdgeProcessing.Worklist
    ( EdgeFingerprint(..)
    , EdgePlanSeed(..)
    , EdgeWorkItem(..)
    , WorklistInvalidation(..)
    , buildEdgeWorklist
    , buildIndexedEdgeWorklist
    , invalidateExpansions
    , invalidateOwners
    , invalidateRoots
    , invalidateRootsOwnersExpansionsWithinRootOwnersExcept
    , noteInertEdge
    , notePlannedEdge
    , noteProcessedEdge
    , popEdgeWorkItem
    , queuedEdgeCount
    )
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
import MLF.Constraint.Types.Witness (Expansion(..))
import SpecUtil (bindParentsFromPairs, defaultTraceConfig, emptyConstraint, nodeMapFromList, rootedConstraint)
import Test.Hspec

spec :: Spec
spec = describe "Edge plan types" $ do
    it "constructs concrete resolved plans" $ do
        let leftTyExp = ResolvedTyExp
                { rteNodeId = NodeId 10
                , rteExpVar = ExpVarId 0
                , rteBodyId = NodeId 0
                }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 0) (NodeId 10) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftTyExp rightNode (NodeId 10) (NodeId 1) (GenNodeId 0)
        eprLeftCanonical plan `shouldBe` NodeId 10

    it "captures TyExp-left payload in resolved plans" $ do
        let leftTyExp = ResolvedTyExp
                { rteNodeId = NodeId 10
                , rteExpVar = ExpVarId 0
                , rteBodyId = NodeId 0
                }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 1) (NodeId 10) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftTyExp rightNode (NodeId 10) (NodeId 1) (GenNodeId 0)
        eprLeftTyExp plan `shouldBe` leftTyExp

    it "preserves edge identity through plan" $ do
        let leftTyExp = ResolvedTyExp
                { rteNodeId = NodeId 10
                , rteExpVar = ExpVarId 0
                , rteBodyId = NodeId 0
                }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 42) (NodeId 10) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftTyExp rightNode (NodeId 10) (NodeId 1) (GenNodeId 0)
        edgePlanEdge plan `shouldBe` edge

    it "indexes planned worklist edges by roots, owner, and expansion variable" $ do
        let leftTyExp = ResolvedTyExp
                { rteNodeId = NodeId 10
                , rteExpVar = ExpVarId 3
                , rteBodyId = NodeId 0
                }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 42) (NodeId 10) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftTyExp rightNode (NodeId 10) (NodeId 1) (GenNodeId 7)
            worklist0 = buildEdgeWorklist [edge]
        case popEdgeWorkItem worklist0 of
            Nothing -> expectationFailure "expected one queued edge"
            Just (popped, worklist1) -> do
                ewiEdge popped `shouldBe` edge
                let worklist2 = notePlannedEdge plan worklist1
                    (rootInvalidation, rootWorklist) =
                        invalidateRoots (IntSet.singleton 10) worklist2
                    (ownerInvalidation, ownerWorklist) =
                        invalidateOwners (IntSet.singleton 7) worklist2
                    (expInvalidation, expWorklist) =
                        invalidateExpansions (IntSet.singleton 3) worklist2
                wiEdges rootInvalidation `shouldBe` IntSet.singleton 42
                wiEdges ownerInvalidation `shouldBe` IntSet.singleton 42
                wiEdges expInvalidation `shouldBe` IntSet.singleton 42
                queuedEdgeCount rootWorklist `shouldBe` 1
                queuedEdgeCount ownerWorklist `shouldBe` 1
                queuedEdgeCount expWorklist `shouldBe` 1

    it "requeues processed stale edges with their latest fingerprint" $ do
        let leftTyExp = ResolvedTyExp
                { rteNodeId = NodeId 10
                , rteExpVar = ExpVarId 3
                , rteBodyId = NodeId 0
                }
            edge = InstEdge (EdgeId 42) (NodeId 10) (NodeId 1)
            seed = EdgePlanSeed
                { epsLeftTyExp = leftTyExp
                , epsLeftRoot = NodeId 10
                , epsRightRoot = NodeId 1
                , epsBodyRoot = NodeId 0
                , epsSchemeOwnerGen = GenNodeId 7
                , epsExpansionVar = ExpVarId 3
                }
            fingerprint = EdgeFingerprint
                { efLeftRoot = NodeId 10
                , efRightRoot = NodeId 1
                , efBodyRoot = NodeId 0
                , efSchemeOwnerGen = GenNodeId 7
                , efExpansionVar = ExpVarId 3
                , efCurrentExpansion = ExpIdentity
                }
            worklist0 = buildEdgeWorklist [edge]
        case popEdgeWorkItem worklist0 of
            Nothing -> expectationFailure "expected one queued edge"
            Just (_popped, worklist1) -> do
                let worklist2 = noteProcessedEdge edge (Just (seed, fingerprint)) worklist1
                    (_rootInvalidation, worklist3) =
                        invalidateRoots (IntSet.singleton 10) worklist2
                queuedEdgeCount worklist3 `shouldBe` 1
                case popEdgeWorkItem worklist3 of
                    Nothing -> expectationFailure "expected requeued stale edge"
                    Just (staleItem, worklist4) -> do
                        ewiStale staleItem `shouldBe` True
                        ewiFingerprint staleItem `shouldBe` Just fingerprint
                        popEdgeWorkItem (noteInertEdge staleItem worklist4) `shouldBe` Nothing

    it "deduplicates root, owner, and expansion invalidation into one stale requeue" $ do
        let leftTyExp = ResolvedTyExp
                { rteNodeId = NodeId 10
                , rteExpVar = ExpVarId 3
                , rteBodyId = NodeId 0
                }
            edge = InstEdge (EdgeId 42) (NodeId 10) (NodeId 1)
            seed = EdgePlanSeed
                { epsLeftTyExp = leftTyExp
                , epsLeftRoot = NodeId 10
                , epsRightRoot = NodeId 1
                , epsBodyRoot = NodeId 0
                , epsSchemeOwnerGen = GenNodeId 7
                , epsExpansionVar = ExpVarId 3
                }
            fingerprint = EdgeFingerprint
                { efLeftRoot = NodeId 10
                , efRightRoot = NodeId 1
                , efBodyRoot = NodeId 0
                , efSchemeOwnerGen = GenNodeId 7
                , efExpansionVar = ExpVarId 3
                , efCurrentExpansion = ExpIdentity
                }
            worklist0 = buildEdgeWorklist [edge]
        case popEdgeWorkItem worklist0 of
            Nothing -> expectationFailure "expected one queued edge"
            Just (_popped, worklist1) -> do
                let worklist2 = noteProcessedEdge edge (Just (seed, fingerprint)) worklist1
                    (rootInvalidation, ownerInvalidation, expInvalidation, worklist3) =
                        invalidateRootsOwnersExpansionsWithinRootOwnersExcept
                            IntSet.empty
                            (IntSet.singleton 10)
                            (IntSet.singleton 7)
                            (IntSet.singleton 3)
                            IntSet.empty
                            worklist2
                wiEdges rootInvalidation `shouldBe` IntSet.singleton 42
                wiEdges ownerInvalidation `shouldBe` IntSet.singleton 42
                wiEdges expInvalidation `shouldBe` IntSet.singleton 42
                queuedEdgeCount worklist3 `shouldBe` 1
                case popEdgeWorkItem worklist3 of
                    Nothing -> expectationFailure "expected one combined stale requeue"
                    Just (staleItem, worklist4) -> do
                        ewiStale staleItem `shouldBe` True
                        ewiFingerprint staleItem `shouldBe` Just fingerprint
                        popEdgeWorkItem (noteInertEdge staleItem worklist4) `shouldBe` Nothing

    it "builds indexed work items with stable owner seeds from one snapshot" $ do
        let a = NodeId 0
            forallNode = NodeId 1
            expNode = NodeId 2
            target = NodeId 3
            edge = InstEdge (EdgeId 0) expNode target
            nodes = nodeMapFromList
                [ (0, TyVar { tnId = a, tnBound = Nothing })
                , (1, TyForall forallNode a)
                , (2, TyExp expNode (ExpVarId 0) forallNode)
                , (3, TyVar { tnId = target, tnBound = Nothing })
                ]
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
        case runPresolutionM defaultTraceConfig st0 (buildIndexedEdgeWorklist [edge]) of
            Left err -> expectationFailure ("buildIndexedEdgeWorklist failed: " ++ show err)
            Right (worklist, _) ->
                case popEdgeWorkItem worklist of
                    Nothing -> expectationFailure "expected one queued edge"
                    Just (item, _) -> do
                        ewiEdge item `shouldBe` edge
                        ewiStale item `shouldBe` False
                        case ewiPlanSeed item of
                            Nothing -> expectationFailure "expected indexed plan seed"
                            Just seed -> do
                                epsBodyRoot seed `shouldBe` forallNode
                                epsSchemeOwnerGen seed `shouldBe` GenNodeId 0

    describe "planner classification" $ do
        it "returns a resolved TyExp payload when left node is TyExp" $ do
            let a = NodeId 0
                forallNode = NodeId 1
                expNode = NodeId 2
                target = NodeId 3
                leftTyExp = ResolvedTyExp
                    { rteNodeId = expNode
                    , rteExpVar = ExpVarId 0
                    , rteBodyId = forallNode
                    }
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
            case runPresolutionM defaultTraceConfig st0 (planEdge id edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) -> do
                    eprLeftTyExp plan `shouldBe` leftTyExp
                    eprAllowTrivial plan `shouldBe` False

        it "adds planner tag for non-TyExp fail-fast invariant" $ do
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
            case runPresolutionM defaultTraceConfig st0 (planEdge id edge) of
                Left (PlanError (ExpectedTyExpLeftInPlanner edgeId leftNode)) -> do
                    edgeId `shouldBe` EdgeId 1
                    leftNode `shouldBe` n0
                Left err ->
                    expectationFailure ("expected PlanError/ExpectedTyExpLeftInPlanner, got " ++ show err)
                Right _ ->
                    expectationFailure "expected planner fail-fast on non-TyExp left edge"

        it "threads let-edge flag into allowTrivial" $ do
            let body = NodeId 0
                target = NodeId 1
                expNode = NodeId 2
                n0 = TyVar { tnId = body, tnBound = Nothing }
                n1 = TyVar { tnId = target, tnBound = Nothing }
                nExp = TyExp { tnId = expNode, tnExpVar = ExpVarId (-1), tnBody = body }
                edge = InstEdge (EdgeId 5) expNode target
                constraint = rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1), (2, nExp)]
                    , cInstEdges = [edge]
                    , cLetEdges = IntSet.singleton 5
                    , cBindParents = bindParentsFromPairs
                        [ (body, expNode, BindFlex)
                        ]
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 3 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge id edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) -> do
                    eprAllowTrivial plan `shouldBe` True
                    eprSchemeOwnerGen plan `shouldBe` GenNodeId 0

        it "recognizes annotation edges without special suppression" $ do
            let body = NodeId 0
                target = NodeId 1
                expNode = NodeId 2
                n0 = TyVar { tnId = body, tnBound = Nothing }
                n1 = TyVar { tnId = target, tnBound = Nothing }
                nExp = TyExp { tnId = expNode, tnExpVar = ExpVarId (-2), tnBody = body }
                edge = InstEdge (EdgeId 3) expNode target
                constraint = rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1), (2, nExp)]
                    , cInstEdges = [edge]
                    , cAnnEdges = IntSet.singleton 3
                    , cBindParents = bindParentsFromPairs
                        [ (body, expNode, BindFlex)
                        ]
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 3 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge id edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) -> do
                    eprSchemeOwnerGen plan `shouldBe` GenNodeId 0

        it "fails fast when a synthesized wrapper owner is only recoverable from the wrapper root" $ do
            let body = NodeId 0
                target = NodeId 1
                expNode = NodeId 2
                n0 = TyVar { tnId = body, tnBound = Nothing }
                n1 = TyVar { tnId = target, tnBound = Nothing }
                nExp = TyExp { tnId = expNode, tnExpVar = ExpVarId (-3), tnBody = body }
                edge = InstEdge (EdgeId 6) expNode target
                constraint = rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1), (2, nExp)]
                    , cInstEdges = [edge]
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 3 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge id edge) of
                Left (InternalError msg) ->
                    msg `shouldSatisfy` isInfixOf "scheme introducer not found"
                Left err ->
                    expectationFailure ("expected strict synthesized-wrapper owner failure, got " ++ show err)
                Right _ ->
                    expectationFailure "expected planner fail-fast when body-root ownership is missing"

        it "fails fast when a synthesized wrapper owner would only be recoverable from wrapper-root fallback" $ do
            let gid = GenNodeId 0
                body = NodeId 0
                target = NodeId 1
                expNode = NodeId 2
                n0 = TyVar { tnId = body, tnBound = Nothing }
                n1 = TyVar { tnId = target, tnBound = Nothing }
                nExp = TyExp { tnId = expNode, tnExpVar = ExpVarId (-4), tnBody = body }
                edge = InstEdge (EdgeId 7) expNode target
                constraint = emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1), (2, nExp)]
                    , cInstEdges = [edge]
                    , cGenNodes = fromListGen [(gid, GenNode gid [expNode])]
                    , cBindParents = IntMap.singleton (nodeRefKey (typeRef expNode)) (genRef gid, BindFlex)
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 3 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge id edge) of
                Left (InternalError msg) ->
                    msg `shouldSatisfy` isInfixOf "scheme introducer not found"
                Left err ->
                    expectationFailure ("expected strict wrapper-root fallback rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected planner fail-fast instead of wrapper-root recovery"
