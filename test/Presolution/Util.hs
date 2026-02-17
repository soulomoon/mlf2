module Presolution.Util
    ( -- * Node lookup helpers
      nodeAt
    , expectArrow
    , expectForall
      -- * Test constraint builders
    , mkNormalizeConstraint
    , mkNormalizeEnv
    , orderedPairByPrec
      -- * Property test helpers
    , NormalizeEnvParams(..)
    , genNormalizeEnvParams
    , mkTestNormalizeEnv
    , genInstanceOps
    , genInstanceOp
    , hasRedundantOps
    ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution.Witness (OmegaNormalizeEnv(..))
import MLF.Constraint.Types.Witness (InstanceOp(..))
import qualified MLF.Util.Order as Order
import SpecUtil
    ( bindParentsFromPairs
    , emptyConstraint
    , lookupNodeMaybe
    , nodeMapFromList
    , rootedConstraint
    )

nodeAt :: NodeMap TyNode -> Int -> TyNode
nodeAt nodes key =
    case lookupNodeMaybe nodes (NodeId key) of
        Just node -> node
        Nothing -> error ("Missing node: " ++ show key)

expectArrow :: HasCallStack => NodeMap TyNode -> NodeId -> IO TyNode
expectArrow nodes nid = case lookupNodeMaybe nodes nid of
    Just a@TyArrow{} -> return a
    other -> do
        let msg = "Expected TyArrow at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

expectForall :: HasCallStack => NodeMap TyNode -> NodeId -> IO TyNode
expectForall nodes nid = case lookupNodeMaybe nodes nid of
    Just f@TyForall{} -> return f
    other -> do
        let msg = "Expected TyForall at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

mkNormalizeConstraint :: Constraint
mkNormalizeConstraint =
    let root = NodeId 0
        arrow = NodeId 1
        dom = NodeId 2
        cod = NodeId 3
    in rootedConstraint $ emptyConstraint
        { cNodes =
            nodeMapFromList
                [ (getNodeId root, TyForall root arrow)
                , (getNodeId arrow, TyArrow arrow dom cod)
                , (getNodeId dom, TyVar { tnId = dom, tnBound = Nothing })
                , (getNodeId cod, TyVar { tnId = cod, tnBound = Nothing })
                ]
        , cBindParents =
            bindParentsFromPairs
                [ (arrow, root, BindFlex)
                , (dom, root, BindFlex)
                , (cod, root, BindFlex)
                ]
        }

mkNormalizeEnv :: Constraint -> NodeId -> IntSet.IntSet -> OmegaNormalizeEnv
mkNormalizeEnv c root interior =
    OmegaNormalizeEnv
        { oneRoot = root
        , interior = interior
        , weakened = IntSet.empty
        , orderKeys = Order.orderKeysFromRootWith id (cNodes c) root Nothing
        , canonical = id
        , constraint = c
        , binderArgs = IntMap.empty
        , binderReplayHints = IntMap.empty
        }

orderedPairByPrec :: Constraint -> NodeId -> (NodeId, NodeId)
orderedPairByPrec c root =
    let n1 = NodeId 2
        n2 = NodeId 3
        keys = Order.orderKeysFromRootWith id (cNodes c) root Nothing
        k1 = keys IntMap.! getNodeId n1
        k2 = keys IntMap.! getNodeId n2
    in if Order.compareOrderKey k1 k2 == LT
        then (n1, n2)
        else (n2, n1)

-- | Parameters for constructing an OmegaNormalizeEnv (showable version).
data NormalizeEnvParams = NormalizeEnvParams
    { nepRoot :: NodeId
    , nepInteriorSize :: Int
    } deriving (Eq, Show)

-- | Generate parameters for a NormalizeEnv.
genNormalizeEnvParams :: Gen NormalizeEnvParams
genNormalizeEnvParams = do
    root <- NodeId <$> choose (0, 10)
    interiorSize <- choose (0, 20)
    pure $ NormalizeEnvParams root interiorSize

-- | Construct an OmegaNormalizeEnv from parameters.
mkTestNormalizeEnv :: NormalizeEnvParams -> OmegaNormalizeEnv
mkTestNormalizeEnv params =
    let interiorNodes = IntSet.fromList [0 .. nepInteriorSize params]
     in OmegaNormalizeEnv
            { oneRoot = nepRoot params
            , interior = interiorNodes
            , weakened = IntSet.empty
            , orderKeys = IntMap.fromList [(n, Order.OrderKey 0 [n]) | n <- [0 .. 50]]
            , canonical = id
            , constraint = emptyConstraint
            , binderArgs = IntMap.empty
            , binderReplayHints = IntMap.empty
            }

-- | Generate a list of InstanceOps with bounded size.
genInstanceOps :: Int -> Gen [InstanceOp]
genInstanceOps maxOps = do
    n <- choose (0, maxOps)
    vectorOf n genInstanceOp

-- | Generate a single InstanceOp.
genInstanceOp :: Gen InstanceOp
genInstanceOp = do
    nid1 <- NodeId <$> choose (0, 50)
    nid2 <- NodeId <$> choose (0, 50)
    elements
        [ OpGraft nid1 nid2
        , OpMerge nid1 nid2
        , OpRaise nid1
        , OpWeaken nid1
        , OpRaiseMerge nid1 nid2
        ]

-- | Check if a list of InstanceOps has redundant operations.
-- Redundant operations include:
-- - Consecutive raises on the same binder
-- - Merge operations where both nodes are the same
hasRedundantOps :: [InstanceOp] -> Bool
hasRedundantOps ops = go Nothing ops
  where
    go _ [] = False
    go mbLastRaise (op:rest) =
        case op of
            OpRaise n ->
                case mbLastRaise of
                    Just n' | n == n' -> True  -- Consecutive raises on same binder
                    _ -> go (Just n) rest
            OpMerge n m ->
                n == m || go Nothing rest  -- Merge of same node is redundant
            OpRaiseMerge n m ->
                n == m || go Nothing rest
            _ -> go Nothing rest
