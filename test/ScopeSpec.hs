module ScopeSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

import qualified MLF.Constraint.Solved as Solved
import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import MLF.Constraint.Types
    ( BindFlag(..)
    , BindingError(..)
    , Constraint(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , getNodeId
    , nodeRefKey
    , typeRef
    )
import MLF.Elab.Run.Scope (preferGenScope, resolveCanonicalScope)
import qualified SolvedFacadeTestUtil as SolvedTest
import SpecUtil (emptyConstraint, nodeMapFromList)

spec :: Spec
spec =
    describe "ga scope" $ do
        it "preferGenScope propagates binding tree cycle errors" $ do
            let root = NodeId 1
                cycleNode = NodeId 2
                constraint = cyclicConstraint root cycleNode
            preferGenScope constraint (TypeRef root)
                `shouldSatisfy` isBindingCycleError

        it "resolveCanonicalScope propagates binding tree cycle errors" $ do
            let root = NodeId 1
                cycleNode = NodeId 2
                constraint = cyclicConstraint root cycleNode
                solved = SolvedTest.mkTestSolved constraint IntMap.empty
            resolveCanonicalScope constraint (PresolutionViewBoundary.fromSolved solved) IntMap.empty root
                `shouldSatisfy` isBindingCycleError

isBindingCycleError :: Either BindingError a -> Bool
isBindingCycleError result = case result of
    Left (BindingCycleDetected _) -> True
    _ -> False

cyclicConstraint :: NodeId -> NodeId -> Constraint
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
