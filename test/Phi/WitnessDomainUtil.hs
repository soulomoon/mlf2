module Phi.WitnessDomainUtil (
    WitnessDomainBridge,
    mkWitnessDomainBridge,
    sourceKeysForNode,
    sourceBinderKeysForNode,
    isBinderNode,
    lookupBinderIndex,
    traceOrderRank
) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution (EdgeTrace(..), PresolutionView(..))
import MLF.Constraint.Types.Graph (NodeId(..), getNodeId)

-- | Test-local witness-domain bridge used to preserve the pure source-key unit
-- coverage without keeping helper hooks in the main library.
data WitnessDomainBridge = WitnessDomainBridge
    { wbTraceBinderOrder :: [Int]
    , wbBinderOrderIx :: IntMap.IntMap Int
    , wbReverseCopyByTarget :: IntMap.IntMap [Int]
    , wbReverseTraceByTarget :: IntMap.IntMap [Int]
    , wbInvCopyMap :: IntMap.IntMap NodeId
    , wbCopyMap :: IntMap.IntMap NodeId
    , wbPresolutionView :: PresolutionView
    }

mkWitnessDomainBridge
    :: PresolutionView
    -> Maybe EdgeTrace
    -> IntMap.IntMap NodeId
    -> WitnessDomainBridge
mkWitnessDomainBridge presolutionView mTrace copyMap =
    WitnessDomainBridge
        { wbTraceBinderOrder = traceBinderOrder
        , wbBinderOrderIx = IntMap.fromList (zip traceBinderOrder [0 :: Int ..])
        , wbReverseCopyByTarget =
            IntMap.fromListWith (++)
                [ (getNodeId dst, [src])
                | (src, dst) <- IntMap.toList copyMap
                ]
        , wbReverseTraceByTarget =
            case mTrace of
                Nothing -> IntMap.empty
                Just tr ->
                    IntMap.fromListWith (++)
                        [ (getNodeId binder, [getNodeId binder])
                        | (binder, _arg) <- etBinderArgs tr
                        ]
        , wbInvCopyMap =
            IntMap.fromList
                [ (getNodeId v, NodeId k)
                | (k, v) <- IntMap.toList copyMap
                , k /= getNodeId v
                ]
        , wbCopyMap = copyMap
        , wbPresolutionView = presolutionView
        }
  where
    traceBinderOrder =
        case mTrace of
            Nothing -> []
            Just tr ->
                reverse $
                    snd $
                        foldl
                            (\(seen, acc) (binder, _arg) ->
                                let key = getNodeId binder
                                in if IntSet.member key seen
                                    then (seen, acc)
                                    else (IntSet.insert key seen, key : acc)
                            )
                            (IntSet.empty, [])
                            (etBinderArgs tr)

sourceKeysForNode :: WitnessDomainBridge -> NodeId -> [Int]
sourceKeysForNode wb nid =
    let keyRaw = getNodeId nid
        fwdRaw = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (wbCopyMap wb))
        invRaw = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (wbInvCopyMap wb))
        revCopy = IntMap.findWithDefault [] keyRaw (wbReverseCopyByTarget wb)
        traceRaw = IntMap.findWithDefault [] keyRaw (wbReverseTraceByTarget wb)
        rank key = (IntMap.findWithDefault maxBound key (wbBinderOrderIx wb), key)
        (_seen, keysRev) =
            foldl
                (\(seenAcc, acc) key ->
                    if IntSet.member key seenAcc
                        then (seenAcc, acc)
                        else (IntSet.insert key seenAcc, key : acc)
                )
                (IntSet.empty, [])
                (keyRaw : fwdRaw ++ invRaw ++ revCopy ++ traceRaw)
    in sortOn rank (reverse keysRev)

sourceBinderKeysForNode :: WitnessDomainBridge -> IntSet.IntSet -> NodeId -> [Int]
sourceBinderKeysForNode wb binderKeys nid =
    filter (`IntSet.member` binderKeys) (sourceKeysForNode wb nid)

isBinderNode :: WitnessDomainBridge -> IntSet.IntSet -> NodeId -> Bool
isBinderNode wb binderKeys nid =
    not (null (sourceBinderKeysForNode wb binderKeys nid))

lookupBinderIndex
    :: WitnessDomainBridge
    -> IntSet.IntSet
    -> [Maybe NodeId]
    -> NodeId
    -> Maybe Int
lookupBinderIndex wb binderKeys ids nid
    | null targetKeys = Nothing
    | otherwise =
        case sortOn rankCandidate candidates of
            [] -> Nothing
            ((ix, _, _) : _) -> Just ix
  where
    keyRank key = (IntMap.findWithDefault maxBound key (wbBinderOrderIx wb), key)
    targetKeys = sourceBinderKeysForNode wb binderKeys nid
    targetKeySet = IntSet.fromList targetKeys
    chooseBestKey keys = case sortOn keyRank keys of
        [] -> maxBound
        k : _ -> k
    candidateFor _ Nothing = Nothing
    candidateFor ix (Just nid') =
        let idKeys = sourceBinderKeysForNode wb binderKeys nid'
            idKeySet = IntSet.fromList idKeys
            exactMatches = filter (`IntSet.member` idKeySet) targetKeys
            matchPick =
                if not (null exactMatches)
                    then Just (0 :: Int, chooseBestKey exactMatches)
                    else Nothing
        in case matchPick of
            Nothing -> Nothing
            Just (matchClass, key) ->
                if IntSet.member key targetKeySet
                    then Just (ix, matchClass, key)
                    else Nothing
    candidates = mapMaybe (uncurry candidateFor) (zip [0 :: Int ..] ids)
    rankCandidate (ix, matchClass, key) = (matchClass, keyRank key, ix)

traceOrderRank :: WitnessDomainBridge -> Int -> (Int, Int)
traceOrderRank wb key =
    (IntMap.findWithDefault maxBound key (wbBinderOrderIx wb), key)
