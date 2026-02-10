module Presolution.EdgeInterpreterSpec (spec) where

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
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter (executeEdgePlan)
import SpecUtil (emptyConstraint, nodeMapFromList, defaultTraceConfig, rootedConstraint, bindParentsFromPairs)

spec :: Spec
spec = describe "Edge interpreter" $ do
    it "executes LegacyDirectMode plan without error" $ do
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
        case runPresolutionM defaultTraceConfig st0 (planEdge edge >>= executeEdgePlan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                -- Should have recorded identity expansion
                IntMap.member 1 (psEdgeExpansions st1) `shouldBe` True
                -- Should have recorded a witness
                IntMap.member 1 (psEdgeWitnesses st1) `shouldBe` True
                -- Should have recorded a trace
                IntMap.member 1 (psEdgeTraces st1) `shouldBe` True

    it "executes ExpansionMode plan without error" $ do
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
        case runPresolutionM defaultTraceConfig st0 (planEdge edge >>= executeEdgePlan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                -- Should have recorded an expansion
                IntMap.member 0 (psEdgeExpansions st1) `shouldBe` True
                -- Should have recorded a witness
                IntMap.member 0 (psEdgeWitnesses st1) `shouldBe` True
                -- Should have recorded a trace
                IntMap.member 0 (psEdgeTraces st1) `shouldBe` True
