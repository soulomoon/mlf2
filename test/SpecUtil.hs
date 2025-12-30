module SpecUtil (
    emptyConstraint,
    expectRight,
    requireRight,
    inferBindParents,
    lookupNodeMaybe,
    lookupNode,
    lookupNodeIO
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Hspec (Expectation, expectationFailure)

import MLF.Constraint.Types (BindFlag(..), BindParents, Constraint(..), NodeId(..), TyNode(..), structuralChildren)

emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cNodes = IntMap.empty
    , cInstEdges = []
    , cUnifyEdges = []
    , cBindParents = IntMap.empty
    , cVarBounds = IntMap.empty
    , cPolySyms = Set.empty
    , cEliminatedVars = IntSet.empty
    }

expectRight :: Show e => Either e a -> (a -> Expectation) -> Expectation
expectRight value k =
    case value of
        Left err -> expectationFailure $ "Expected success, but got: " ++ show err
        Right result -> k result

requireRight :: Show e => Either e a -> IO a
requireRight = either (\e -> expectationFailure (show e) >> fail "requireRight") pure

inferBindParents :: IntMap.IntMap TyNode -> BindParents
inferBindParents nodes =
    -- Follow term structure edges (child → structural parent).
    foldl' addEdges IntMap.empty (IntMap.elems nodes)
  where
    addEdges bp parentNode =
        let parent = tnId parentNode
            kids = structuralChildren parentNode

            addOne m child
                | child == parent = m
                | otherwise =
                    IntMap.insertWith
                        (\_ old -> old)
                        (getNodeId child)
                        (parent, BindFlex)
                        m
        in foldl' addOne bp kids

lookupNodeMaybe :: IntMap.IntMap TyNode -> NodeId -> Maybe TyNode
lookupNodeMaybe table nid = IntMap.lookup (getNodeId nid) table

lookupNode :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
lookupNode = lookupNodeIO

lookupNodeIO :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
lookupNodeIO table nid =
    case lookupNodeMaybe table nid of
        Just node -> pure node
        Nothing -> do
            expectationFailure $ "Missing node: " ++ show nid
            pure (error "unreachable: missing TyNode")
