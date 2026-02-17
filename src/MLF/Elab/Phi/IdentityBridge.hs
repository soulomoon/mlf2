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
    traceBinderKeysInOrder,
    canonicalKeyForNode,
    canonicalKeyForSource,
    sourceKeysForNode,
    safeSourceCandidatesForCanonicalBinder,
    sourceBinderKeysForNode,
    isBinderNode,
    lookupBinderIndex,
    traceOrderRank,
) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
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
    , ibTraceBinderOrder       :: [Int]
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

traceBinderKeysInOrder :: IdentityBridge -> [Int]
traceBinderKeysInOrder = ibTraceBinderOrder

canonicalKeyForNode :: IdentityBridge -> NodeId -> Int
canonicalKeyForNode ib nid = getNodeId (ibCanonical ib nid)

canonicalKeyForSource :: IdentityBridge -> Int -> Int
canonicalKeyForSource ib key = canonicalKeyForNode ib (NodeId key)

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
        , ibTraceBinderOrder       = traceBinderOrder
        , ibBinderOrderIx          = binderOrderIx
        , ibReverseCopyByCanonical = reverseCopyByCanonical
        , ibReverseTraceByCanonical = reverseTraceByCanonical
        , ibInvCopyMap             = invCopyMap
        , ibCopyMap                = copyMap
        }
  where
    traceBinderOrder :: [Int]
    traceBinderOrder =
        case mTrace of
            Nothing -> []
            Just tr ->
                reverse $
                    snd $
                        foldl'
                            (\(seen, acc) (binder, _arg) ->
                                let key = getNodeId binder
                                in if IntSet.member key seen
                                    then (seen, acc)
                                    else (IntSet.insert key seen, key : acc)
                            )
                            (IntSet.empty, [])
                            (etBinderArgs tr)

    binderOrderIx :: IntMap.IntMap Int
    binderOrderIx = IntMap.fromList (zip traceBinderOrder [0 :: Int ..])

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
        fwdRaw   = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (ibCopyMap ib))
        fwdCanon = map (getNodeId . canonical . NodeId) fwdRaw
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
                (keyRaw : keyCanon : fwdRaw ++ fwdCanon ++ invRaw ++ invCanon ++ copyCanon ++ traceCanon)
    in sortOn rank (reverse keysRev)

-- | Candidate source keys for a canonical binder, ranked deterministically
-- and constrained to that canonical binder identity.
safeSourceCandidatesForCanonicalBinder :: IdentityBridge -> NodeId -> [Int]
safeSourceCandidatesForCanonicalBinder ib binderCanonical =
    let canonKey = canonicalKeyForNode ib binderCanonical
    in filter
        (\key -> canonicalKeyForSource ib key == canonKey)
        (sourceKeysForNode ib (NodeId canonKey))

-- | Source keys filtered to membership in the given binder key set.
sourceBinderKeysForNode :: IdentityBridge -> IntSet.IntSet -> NodeId -> [Int]
sourceBinderKeysForNode ib binderKeys nid =
    filter (`IntSet.member` binderKeys) (sourceKeysForNode ib nid)

-- | Does this node have any source key in the given binder key set?
isBinderNode :: IdentityBridge -> IntSet.IntSet -> NodeId -> Bool
isBinderNode ib binderKeys nid =
    not (null (sourceBinderKeysForNode ib binderKeys nid))

-- | Look up the spine index for a binder node.
--
-- Given a binder key set, a spine of @[Maybe NodeId]@ (one per quantifier
-- position), and a target node, return the index of the best-matching spine
-- position, or 'Nothing' if the target is not a binder node or no spine
-- position matches.
--
-- Ranking: exact source-key match (matchClass 0) beats canonical-alias
-- match (matchClass 1).  Within a match class, the key with the best
-- trace-order rank wins; ties are broken by spine index (lower wins).
lookupBinderIndex
    :: IdentityBridge -> IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
lookupBinderIndex ib binderKeys ids nid
    | not (isBinderNode ib binderKeys nid) = Nothing
    | otherwise =
        case sortOn rankCandidate candidates of
            [] -> Nothing
            ((ix, _matchClass, _key) : _) -> Just ix
  where
    keyRank :: Int -> (Int, Int)
    keyRank key = (IntMap.findWithDefault maxBound key (ibBinderOrderIx ib), key)

    canonicalKey :: Int -> Int
    canonicalKey key = getNodeId (ibCanonical ib (NodeId key))

    targetKeys :: [Int]
    targetKeys =
        filter (`IntSet.member` binderKeys) (sourceKeysForNode ib nid)

    targetKeySet :: IntSet.IntSet
    targetKeySet = IntSet.fromList targetKeys

    chooseBestKey :: [Int] -> Int
    chooseBestKey keys =
        case sortOn keyRank keys of
            [] -> maxBound
            k : _ -> k

    candidateFor :: Int -> Maybe NodeId -> Maybe (Int, Int, Int)
    candidateFor _ Nothing = Nothing
    candidateFor ix (Just nid') =
        let idKeys =
                filter (`IntSet.member` binderKeys) (sourceKeysForNode ib nid')
            idKeySet = IntSet.fromList idKeys
            idCanonSet = IntSet.fromList (map canonicalKey idKeys)
            exactMatches =
                filter (`IntSet.member` idKeySet) targetKeys
            aliasMatches =
                [ key
                | key <- targetKeys
                , not (IntSet.member key idKeySet)
                , IntSet.member (canonicalKey key) idCanonSet
                ]
            matchPick =
                if not (null exactMatches)
                    then Just (0, chooseBestKey exactMatches)
                    else if not (null aliasMatches)
                        then Just (1, chooseBestKey aliasMatches)
                        else Nothing
        in case matchPick of
            Nothing -> Nothing
            Just (matchClass, key) ->
                if IntSet.member key targetKeySet
                    then Just (ix, matchClass, key)
                    else Nothing

    candidates :: [(Int, Int, Int)]
    candidates =
        mapMaybe (uncurry candidateFor) (zip [0 :: Int ..] ids)

    rankCandidate :: (Int, Int, Int) -> (Int, (Int, Int), Int)
    rankCandidate (ix, matchClass, key) =
        (matchClass, keyRank key, ix)


-- | Trace-order rank for a raw key: @(trace-index, key)@ suitable for
-- 'Data.List.sortOn'.  Keys absent from the trace receive @maxBound@ as
-- their index, so they sort last.
traceOrderRank :: IdentityBridge -> Int -> (Int, Int)
traceOrderRank ib key =
    (IntMap.findWithDefault maxBound key (ibBinderOrderIx ib), key)
