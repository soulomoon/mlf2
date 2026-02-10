module Presolution.EdgePlannerSpec (spec) where

import Test.Hspec

import MLF.Constraint.Types.Graph (
    NodeId(..), EdgeId(..), ExpVarId(..),
    TyNode(..), InstEdge(..)
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan

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
