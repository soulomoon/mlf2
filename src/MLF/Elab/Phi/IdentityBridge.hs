{- |
Module      : MLF.Elab.Phi.IdentityBridge
Description : Centralised source-ID / canonical-ID reconciliation
Copyright   : (c) 2026
License     : BSD-3-Clause

Pure data structure that captures the source-domain ↔ canonical-domain
mapping for a single edge, centralising the ranking and de-duplication
logic currently duplicated between "MLF.Elab.Phi.Omega" and
"MLF.Elab.Phi.Translate".
-}
module MLF.Elab.Phi.IdentityBridge (
    IdentityBridge,
    mkIdentityBridge,
    bridgeCanonical,
    bridgeCopyMap,
    sourceKeysForNode,
    sourceBinderKeysForNode,
    isBinderNode,
) where

import Data.List (sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Constraint.Presolution.Base (EdgeTrace(..))

-- | Bridge between source-domain and canonical-domain node identities.
--
-- Constructed once per edge from an 'EdgeTrace', a canonical function,
-- a copy map, and a scheme-binder key set.  All helpers are pure.
data IdentityBridge = IdentityBridge
    { ibCanonical              :: NodeId -> NodeId
    , ibBinderOrderIx          :: IntMap.IntMap Int
    , ibReverseCopyByCanonical :: IntMap.IntMap [Int]
      -- ^ Copy-map source keys grouped by canonical(destination)
    , ibReverseTraceByCanonical :: IntMap.IntMap [Int]
      -- ^ Trace binder IDs grouped by canonical(binder)
    , ibInvCopyMap             :: IntMap.IntMap NodeId
    , ibCopyMap                :: IntMap.IntMap NodeId
    }

-- | Access the canonical function stored in the bridge.
bridgeCanonical :: IdentityBridge -> (NodeId -> NodeId)
bridgeCanonical = ibCanonical

-- | Access the raw copy map stored in the bridge.
bridgeCopyMap :: IdentityBridge -> IntMap.IntMap NodeId
bridgeCopyMap = ibCopyMap

-- | Build an 'IdentityBridge' from the canonical function, an optional
-- 'EdgeTrace', and the raw copy map.
mkIdentityBridge
    :: (NodeId -> NodeId)
    -> Maybe EdgeTrace
    -> IntMap.IntMap NodeId
    -> IdentityBridge
mkIdentityBridge canonical mTrace copyMap =
    IdentityBridge
        { ibCanonical              = canonical
        , ibBinderOrderIx          = binderOrderIx
        , ibReverseCopyByCanonical = reverseCopyByCanonical
        , ibReverseTraceByCanonical = reverseTraceByCanonical
        , ibInvCopyMap             = invCopyMap
        , ibCopyMap                = copyMap
        }
  where
    binderOrderIx :: IntMap.IntMap Int
    binderOrderIx =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr ->
                IntMap.fromList
                    [ (getNodeId binder, ix)
                    | (ix, (binder, _arg)) <- zip [0 :: Int ..] (etBinderArgs tr)
                    ]

    reverseCopyByCanonical :: IntMap.IntMap [Int]
    reverseCopyByCanonical =
        IntMap.fromListWith (++)
            [ (getNodeId (canonical dst), [src])
            | (src, dst) <- IntMap.toList copyMap
            ]

    reverseTraceByCanonical :: IntMap.IntMap [Int]
    reverseTraceByCanonical =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr ->
                IntMap.fromListWith (++)
                    [ (getNodeId (canonical binder), [getNodeId binder])
                    | (binder, _arg) <- etBinderArgs tr
                    ]

    invCopyMap :: IntMap.IntMap NodeId
    invCopyMap =
        IntMap.fromList
            [ (getNodeId v, NodeId k)
            | (k, v) <- IntMap.toList copyMap
            , k /= getNodeId v
            ]

-- | All source-domain keys for a node, de-duplicated and ranked by
-- trace order (lower index = higher priority).  Falls back to numeric
-- order for keys absent from the trace.
sourceKeysForNode :: IdentityBridge -> NodeId -> [Int]
sourceKeysForNode ib nid =
    let canonical = ibCanonical ib
        keyRaw   = getNodeId nid
        keyCanon = getNodeId (canonical nid)
        invRaw   = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (ibInvCopyMap ib))
        invCanon = maybe [] (pure . getNodeId) (IntMap.lookup keyCanon (ibInvCopyMap ib))
        copyCanon  = IntMap.findWithDefault [] keyCanon (ibReverseCopyByCanonical ib)
        traceCanon = IntMap.findWithDefault [] keyCanon (ibReverseTraceByCanonical ib)
        rank key = (IntMap.findWithDefault maxBound key (ibBinderOrderIx ib), key)
        (_seen, keysRev) =
            foldl'
                (\(seenAcc, acc) key ->
                    if IntSet.member key seenAcc
                        then (seenAcc, acc)
                        else (IntSet.insert key seenAcc, key : acc)
                )
                (IntSet.empty, [])
                (keyRaw : keyCanon : invRaw ++ invCanon ++ copyCanon ++ traceCanon)
    in sortOn rank (reverse keysRev)

-- | Source keys filtered to membership in the given binder key set.
sourceBinderKeysForNode :: IdentityBridge -> IntSet.IntSet -> NodeId -> [Int]
sourceBinderKeysForNode ib binderKeys nid =
    filter (`IntSet.member` binderKeys) (sourceKeysForNode ib nid)

-- | Does this node have any source key in the given binder key set?
isBinderNode :: IdentityBridge -> IntSet.IntSet -> NodeId -> Bool
isBinderNode ib binderKeys nid =
    not (null (sourceBinderKeysForNode ib binderKeys nid))
