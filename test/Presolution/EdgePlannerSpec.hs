module Presolution.EdgePlannerSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph (
    NodeId(..), EdgeId(..), ExpVarId(..), BindFlag(..), Constraint(..),
    TyNode(..), InstEdge(..)
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Presolution
    ( PresolutionState(..)
    , runPresolutionM
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import SpecUtil (emptyConstraint, nodeMapFromList, defaultTraceConfig, rootedConstraint, bindParentsFromPairs)

spec :: Spec
spec = describe "Edge plan types" $ do
    it "exposes stage-indexed plan constructors" $ do
        let leftNode = TyVar { tnId = NodeId 0, tnBound = Nothing }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftNode rightNode (NodeId 0) (NodeId 1) LegacyDirectMode
        edgePlanStage plan `shouldBe` StageResolved

    it "classifies ExpansionMode for TyExp-left plans" $ do
        let leftNode = TyExp { tnId = NodeId 10, tnExpVar = ExpVarId 0, tnBody = NodeId 0 }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 1) (NodeId 10) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftNode rightNode (NodeId 10) (NodeId 1) ExpansionMode
        edgePlanMode plan `shouldBe` ExpansionMode

    it "classifies LegacyDirectMode for non-TyExp-left plans" $ do
        let leftNode = TyVar { tnId = NodeId 0, tnBound = Nothing }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 2) (NodeId 0) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftNode rightNode (NodeId 0) (NodeId 1) LegacyDirectMode
        edgePlanMode plan `shouldBe` LegacyDirectMode

    it "preserves edge identity through plan" $ do
        let leftNode = TyVar { tnId = NodeId 0, tnBound = Nothing }
            rightNode = TyVar { tnId = NodeId 1, tnBound = Nothing }
            edge = InstEdge (EdgeId 42) (NodeId 0) (NodeId 1)
            plan = mkEmptyResolvedPlan edge leftNode rightNode (NodeId 0) (NodeId 1) LegacyDirectMode
        edgePlanEdge plan `shouldBe` edge

    describe "planner classification" $ do
        it "returns ExpansionMode when left node is TyExp" $ do
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
                    IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) -> do
                    edgePlanMode plan `shouldBe` ExpansionMode
                    eprAllowTrivial plan `shouldBe` False
                    eprSuppressWeaken plan `shouldBe` False

        it "returns LegacyDirectMode when left node is non-TyExp" $ do
            let n0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                n1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
                edge = InstEdge (EdgeId 1) (NodeId 0) (NodeId 1)
                constraint = rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1)]
                    , cInstEdges = [edge]
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 2 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) -> do
                    edgePlanMode plan `shouldBe` LegacyDirectMode
                    eprAllowTrivial plan `shouldBe` False

        it "threads let-edge flag into allowTrivial" $ do
            let n0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                n1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
                edge = InstEdge (EdgeId 5) (NodeId 0) (NodeId 1)
                constraint = rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1)]
                    , cInstEdges = [edge]
                    , cLetEdges = IntSet.singleton 5
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 2 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) ->
                    eprAllowTrivial plan `shouldBe` True

        it "threads ann-edge flag into suppressWeaken" $ do
            let n0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                n1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
                edge = InstEdge (EdgeId 3) (NodeId 0) (NodeId 1)
                constraint = rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, n0), (1, n1)]
                    , cInstEdges = [edge]
                    , cAnnEdges = IntSet.singleton 3
                    }
                st0 = PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 2 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (planEdge edge) of
                Left err -> expectationFailure ("planEdge failed: " ++ show err)
                Right (plan, _) ->
                    eprSuppressWeaken plan `shouldBe` True
