module Presolution.EdgeInterpreterSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.Presolution
    ( PresolutionState(..)
    , runPresolutionM
    )
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter (executeEdgePlan)
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , Constraint(..)
    , EdgeId(..)
    , ExpVarId(..)
    , InstEdge(..)
    , NodeId(..)
    , TyNode(..)
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Witness (Expansion(..))
import SpecUtil (bindParentsFromPairs, defaultTraceConfig, emptyConstraint, nodeMapFromList, rootedConstraint)
import Test.Hspec

spec :: Spec
spec = describe "Edge interpreter" $ do
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
                IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (planEdge edge >>= executeEdgePlan) of
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
                IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (planEdge edge >>= executeEdgePlan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                IntMap.member 0 (psEdgeExpansions st1) `shouldBe` True
                IntMap.member 0 (psEdgeWitnesses st1) `shouldBe` True
                IntMap.member 0 (psEdgeTraces st1) `shouldBe` True

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
            plan = mkEmptyResolvedPlan edge leftTyExp nTarget expNode target
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
                IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (executeEdgePlan plan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                let Presolution assignments = psPresolution st1
                IntMap.member (-1) assignments `shouldBe` True

    it "keeps synthesized-wrapper expansions at ExpIdentity against forall targets" $ do
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
            plan = mkEmptyResolvedPlan edge leftTyExp nTargetForall expNode targetForall
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
                IntMap.empty IntMap.empty IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (executeEdgePlan plan) of
            Left err -> expectationFailure ("executeEdgePlan failed: " ++ show err)
            Right ((), st1) -> do
                let Presolution assignments = psPresolution st1
                IntMap.lookup (-2) assignments `shouldBe` Just ExpIdentity

