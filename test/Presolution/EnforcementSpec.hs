module Presolution.EnforcementSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution (PresolutionResult(..), computePresolution)
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import SpecUtil
    ( defaultTraceConfig
    , emptyConstraint
    , nodeMapFromList
    , rootedConstraint
    )

spec :: Spec
spec = describe "Translatable presolution enforcement" $ do
    it "rigidifies scheme roots, arrow nodes, and non-interior nodes" $ do
        let rootGen = GenNodeId 0
            schemeRoot = NodeId 0
            dom = NodeId 1
            cod = NodeId 2
            arrow = NodeId 3
            outside = NodeId 4
            nodes = nodeMapFromList
                [ (getNodeId schemeRoot, TyVar { tnId = schemeRoot, tnBound = Just arrow })
                , (getNodeId dom, TyVar { tnId = dom, tnBound = Nothing })
                , (getNodeId cod, TyVar { tnId = cod, tnBound = Nothing })
                , (getNodeId arrow, TyArrow arrow dom cod)
                , (getNodeId outside, TyVar { tnId = outside, tnBound = Nothing })
                ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef arrow), (typeRef schemeRoot, BindFlex))
                    , (nodeRefKey (typeRef dom), (typeRef arrow, BindFlex))
                    , (nodeRefKey (typeRef cod), (typeRef arrow, BindFlex))
                    , (nodeRefKey (typeRef outside), (genRef rootGen, BindFlex))
                    ]
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
                    }
            acyclicityRes = AcyclicityResult { arSortedEdges = [], arDepGraph = undefined }

        case computePresolution defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("computePresolution failed: " ++ show err)
            Right pr -> do
                let bp = cBindParents (prConstraint pr)
                IntMap.lookup (nodeRefKey (typeRef schemeRoot)) bp `shouldBe` Just (genRef rootGen, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef arrow)) bp `shouldBe` Just (typeRef schemeRoot, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef outside)) bp `shouldBe` Just (genRef rootGen, BindRigid)
