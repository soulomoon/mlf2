{- |
Module      : MLF.Elab.Phi.IdentityBridge
Description : Centralised trace-domain/source-key reconciliation
Copyright   : (c) 2026
License     : BSD-3-Clause

Pure witness-domain utility/test surface for a single edge.

It records source-key provenance and deterministic witness-domain ranking for
diagnostics, focused tests, and non-runtime helper logic. The live `Omega`
path remains direct and fail-fast on replay-spine targets; this module is not
its target-repair engine.
-}
module MLF.Elab.Phi.IdentityBridge (
    IdentityBridge,
    mkIdentityBridge,
    bridgePresolutionView,
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
import MLF.Constraint.Presolution (PresolutionView(..), EdgeTrace(..))

-- | Bridge between witness-domain key artifacts (trace/copy/raw node ids).
--
-- Constructed once per edge from a 'PresolutionView', an optional
-- 'EdgeTrace', and a copy map.  All helpers are pure.
data IdentityBridge = IdentityBridge
    { ibPresolutionView        :: PresolutionView
    , ibCanonical              :: NodeId -> NodeId
    , ibTraceBinderOrder       :: [Int]
    , ibBinderOrderIx          :: IntMap.IntMap Int
    , ibReverseCopyByTarget    :: IntMap.IntMap [Int]
      -- ^ Copy-map source keys grouped by destination key.
    , ibReverseTraceByTarget   :: IntMap.IntMap [Int]
      -- ^ Trace binder IDs grouped by binder key.
    , ibInvCopyMap             :: IntMap.IntMap NodeId
    , ibCopyMap                :: IntMap.IntMap NodeId
    }

-- | Access the canonical function stored in the bridge.
bridgeCanonical :: IdentityBridge -> (NodeId -> NodeId)
bridgeCanonical = ibCanonical

-- | Access the 'PresolutionView' handle stored in the bridge.
bridgePresolutionView :: IdentityBridge -> PresolutionView
bridgePresolutionView = ibPresolutionView

-- | Access the raw copy map stored in the bridge.
bridgeCopyMap :: IdentityBridge -> IntMap.IntMap NodeId
bridgeCopyMap = ibCopyMap

traceBinderKeysInOrder :: IdentityBridge -> [Int]
traceBinderKeysInOrder = ibTraceBinderOrder

-- | Compatibility accessors retained for the exported bridge API.
-- In strict witness-domain mode these are identity projections.
canonicalKeyForNode :: IdentityBridge -> NodeId -> Int
canonicalKeyForNode _ib nid = getNodeId nid

canonicalKeyForSource :: IdentityBridge -> Int -> Int
canonicalKeyForSource _ib key = key

-- | Build an 'IdentityBridge' from a 'PresolutionView', an optional
-- 'EdgeTrace', and the raw copy map.
mkIdentityBridge
    :: PresolutionView
    -> Maybe EdgeTrace
    -> IntMap.IntMap NodeId
    -> IdentityBridge
mkIdentityBridge presolutionView mTrace copyMap =
    IdentityBridge
        { ibPresolutionView        = presolutionView
        , ibCanonical              = pvCanonical presolutionView
        , ibTraceBinderOrder       = traceBinderOrder
        , ibBinderOrderIx          = binderOrderIx
        , ibReverseCopyByTarget    = reverseCopyByTarget
        , ibReverseTraceByTarget   = reverseTraceByTarget
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

    reverseCopyByTarget :: IntMap.IntMap [Int]
    reverseCopyByTarget =
        IntMap.fromListWith (++)
            [ (getNodeId dst, [src])
            | (src, dst) <- IntMap.toList copyMap
            ]

    reverseTraceByTarget :: IntMap.IntMap [Int]
    reverseTraceByTarget =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr ->
                IntMap.fromListWith (++)
                    [ (getNodeId binder, [getNodeId binder])
                    | (binder, _arg) <- etBinderArgs tr
                    ]

    invCopyMap :: IntMap.IntMap NodeId
    invCopyMap =
        IntMap.fromList
            [ (getNodeId v, NodeId k)
            | (k, v) <- IntMap.toList copyMap
            , k /= getNodeId v
            ]

{- Note [Witness-Domain Source Keys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Per thesis §15.3.5–15.3.6, Phi translation resolves binder/source identity
from witness-domain artifacts first. `sourceKeysForNode` therefore stays in
raw witness key-space and includes only:

  1. raw key
  2. copy-map forward/reverse aliases
  3. trace binder aliases (same raw key-space)

No runtime equivalence-class expansion is performed here. The live runtime
path in `MLF.Elab.Phi.Omega` may consult this module for diagnostics, but it
still resolves replay targets directly from the quantifier spine.
-}

-- | All source-domain keys for a node, de-duplicated and ranked by
-- trace order (lower index = higher priority), restricted to witness-domain
-- provenance (raw/copy/trace).
sourceKeysForNode :: IdentityBridge -> NodeId -> [Int]
sourceKeysForNode ib nid =
    let keyRaw   = getNodeId nid
        fwdRaw   = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (ibCopyMap ib))
        invRaw   = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (ibInvCopyMap ib))
        revCopy = IntMap.findWithDefault [] keyRaw (ibReverseCopyByTarget ib)
        traceRaw = IntMap.findWithDefault [] keyRaw (ibReverseTraceByTarget ib)
        rank key = (IntMap.findWithDefault maxBound key (ibBinderOrderIx ib), key)
        (_seen, keysRev) =
            foldl'
                (\(seenAcc, acc) key ->
                    if IntSet.member key seenAcc
                        then (seenAcc, acc)
                        else (IntSet.insert key seenAcc, key : acc)
                )
                (IntSet.empty, [])
                (keyRaw : fwdRaw ++ invRaw ++ revCopy ++ traceRaw)
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
-- Ranking:
--   * matchClass 0: exact source-key match
-- Within exact matches, the key with the best trace-order rank wins;
-- ties are broken by spine index (lower wins).
lookupBinderIndex
    :: IdentityBridge -> IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
lookupBinderIndex ib binderKeys ids nid
    | null targetKeys = Nothing
    | otherwise =
        case sortOn rankCandidate candidates of
            [] -> Nothing
            ((ix, _matchClass, _key) : _) -> Just ix
  where
    keyRank :: Int -> (Int, Int)
    keyRank key = (IntMap.findWithDefault maxBound key (ibBinderOrderIx ib), key)

    targetKeys :: [Int]
    targetKeys = sourceBinderKeysForNode ib binderKeys nid

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
                sourceBinderKeysForNode ib binderKeys nid'
            idKeySet = IntSet.fromList idKeys
            exactMatches =
                filter (`IntSet.member` idKeySet) targetKeys
            matchPick =
                if not (null exactMatches)
                    then Just (0, chooseBestKey exactMatches)
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
