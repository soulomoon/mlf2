{- |
Module      : MLF.Elab.Phi.Omega.Domain
Description : Omega binder-domain and replay lookup helpers
-}
module MLF.Elab.Phi.Omega.Domain
    ( OmegaContext(..)
    , OmegaDomainEnv
    , mkOmegaDomainEnv
    , isTraceBinderSource
    , isTyVarNode
    , isBinderNode
    , lookupBinderIndex
    , resolveTraceBinderTarget
    , resolveNonRootGraftBinder
    , resolveNonRootWeakenBinder
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import Data.Maybe (listToMaybe)

import MLF.Constraint.Presolution (EdgeTrace(..), PresolutionView(..))
import MLF.Constraint.Types
import MLF.Constraint.Types.Witness (ReplayContract)
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Types
import MLF.Util.Trace (TraceConfig)

-- | Shared context for Omega/Step interpretation.
data OmegaContext = OmegaContext
    { ocTraceConfig :: TraceConfig
    , ocPresolutionView :: PresolutionView
    , ocReifyBoundWithNames :: IntMap.IntMap String -> NodeId -> Either ElabError ElabType
    , ocReifyTypeWithNamedSetNoFallback :: IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
    , ocCopyMap :: IntMap.IntMap NodeId
    , ocGaParents :: Maybe GaBindParents
    , ocTrace :: Maybe EdgeTrace
    , ocSchemeInfo :: Maybe SchemeInfo
    , ocTraceBinderSources :: IntSet.IntSet
    , ocTraceBinderReplayMap :: IntMap.IntMap NodeId
    , ocTraceBinderMapDomain :: IntSet.IntSet
    , ocReplayContract :: ReplayContract
    , ocEdgeRoot :: NodeId
    , ocEdgeLeft :: NodeId
    , ocEdgeRight :: NodeId
    }

data OmegaDomainEnv = OmegaDomainEnv
    { odeCanonicalNode :: NodeId -> NodeId
    , odeLookupNodePV :: NodeId -> Maybe TyNode
    , odeCopyMap :: IntMap.IntMap NodeId
    , odeTrace :: Maybe EdgeTrace
    , odeTraceBinderSources :: IntSet.IntSet
    , odeTraceBinderReplayMap :: IntMap.IntMap NodeId
    , odeTraceBinderMapDomain :: IntSet.IntSet
    }

mkOmegaDomainEnv :: OmegaContext -> OmegaDomainEnv
mkOmegaDomainEnv ctx =
    let presolutionView = ocPresolutionView ctx
    in OmegaDomainEnv
        { odeCanonicalNode = pvCanonical presolutionView
        , odeLookupNodePV = pvLookupNode presolutionView
        , odeCopyMap = ocCopyMap ctx
        , odeTrace = ocTrace ctx
        , odeTraceBinderSources = ocTraceBinderSources ctx
        , odeTraceBinderReplayMap = ocTraceBinderReplayMap ctx
        , odeTraceBinderMapDomain = ocTraceBinderMapDomain ctx
        }

isTraceBinderSource :: OmegaDomainEnv -> NodeId -> Bool
isTraceBinderSource env nid =
    IntSet.member (getNodeId nid) (odeTraceBinderSources env)

isTyVarNode :: OmegaDomainEnv -> NodeId -> Bool
isTyVarNode env nid =
    case odeLookupNodePV env nid of
        Just TyVar{} -> True
        _ -> False

isBinderNode :: OmegaDomainEnv -> IntSet.IntSet -> NodeId -> Bool
isBinderNode env binderKeys nid =
    IntSet.member (getNodeId nid) binderKeys
        && isTyVarNode env nid

lookupBinderIndex :: OmegaDomainEnv -> IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
lookupBinderIndex env binderKeys ids nid
    | IntSet.member (getNodeId nid) binderKeys && isTyVarNode env nid =
        listToMaybe
            [ i
            | (i, Just binderNid) <- zip [0 :: Int ..] ids
            , binderNid == nid
            ]
    | otherwise =
        Nothing

resolveTraceBinderTarget
    :: OmegaDomainEnv
    -> Bool
    -> String
    -> [Int]
    -> (NodeId -> Bool)
    -> NodeId
    -> Either ElabError NodeId
resolveTraceBinderTarget env requireBinder opName replayBinderKeys isSchemeBinder rawTarget
    | IntSet.member rawKey (odeTraceBinderSources env) =
        case IntMap.lookup rawKey (odeTraceBinderReplayMap env) of
            Nothing ->
                Left $
                    PhiInvariantError $
                        unlines
                            [ "trace/replay binder key-space mismatch"
                            , "op: " ++ opName
                            , "source target: " ++ show rawTarget
                            , "trace binder sources: " ++ show (IntSet.toList (odeTraceBinderSources env))
                            , "replay binder keys: " ++ show replayBinderKeys
                            , "replay-map domain: " ++ show (IntMap.keys (odeTraceBinderReplayMap env))
                            ]
            Just replayTargetRaw ->
                let replayTarget = replayTargetRaw
                    replayTargetIsSchemeBinder =
                        isSchemeBinder replayTarget
                in if requireBinder && not replayTargetIsSchemeBinder
                    then
                        Left $
                            PhiInvariantError $
                                unlines
                                    [ "trace/replay binder target is not a scheme TyVar binder"
                                    , "op: " ++ opName
                                    , "source target: " ++ show rawTarget
                                    , "replay target: " ++ show replayTarget
                                    , "replay-map domain: " ++ show (IntMap.keys (odeTraceBinderReplayMap env))
                                    ]
                    else Right replayTarget
    | otherwise = Right rawTarget
  where
    rawKey = getNodeId rawTarget

resolveNonRootGraftBinder
    :: OmegaDomainEnv
    -> IntSet.IntSet
    -> [Maybe NodeId]
    -> (NodeId -> Maybe String)
    -> NodeId
    -> NodeId
    -> Either ElabError NodeId
resolveNonRootGraftBinder env binderKeys ids lookupBinder sourceTarget replayTarget =
    case lookupBinderIndex env binderKeys ids replayTarget of
        Just _ -> Right replayTarget
        Nothing -> Left (unresolvedGraftError "binder target missing from quantifier spine")
  where
    unresolvedGraftError reason =
        PhiTranslatabilityError
            [ "OpGraft: binder not found in quantifier spine"
            , "  reason: " ++ reason
            , "  op-target: " ++ show sourceTarget
            , "  replay-target: " ++ show replayTarget
            , "  source-name: " ++ show (lookupBinder sourceTarget)
            , "  replay-name: " ++ show (lookupBinder replayTarget)
            , "  ids: " ++ show ids
            , "  replayMapDomain: " ++ show (IntMap.keys (odeTraceBinderReplayMap env))
            , "  replayMapKeys: " ++ show (IntSet.toList (odeTraceBinderMapDomain env))
            ]

resolveNonRootWeakenBinder
    :: OmegaDomainEnv
    -> IntSet.IntSet
    -> [Maybe NodeId]
    -> NodeId
    -> NodeId
    -> Either ElabError NodeId
resolveNonRootWeakenBinder env binderKeys ids sourceTarget replayTarget
    | not (isBinderNode env binderKeys replayTarget) =
        Left (unresolvedWeakenError "non-binder target is outside replay binder key-space")
    | otherwise =
        case lookupBinderIndex env binderKeys ids replayTarget of
            Just _ -> Right replayTarget
            Nothing -> Left (unresolvedWeakenError "binder target missing from quantifier spine")
  where
    replayCanonical = odeCanonicalNode env replayTarget

    traceSourceMatch = witnessDomainSourceMatches env binderKeys replayTarget

    unresolvedWeakenError reason =
        PhiTranslatabilityError
            [ "OpWeaken: unresolved non-root binder target"
            , "  reason: " ++ reason
            , "  op-target: " ++ show sourceTarget
            , "  replay-target: " ++ show replayTarget
            , "  canonical-target: " ++ show replayCanonical
            , "  traceSourceMatch: " ++ show traceSourceMatch
            , "  ids: " ++ show ids
            , "  replayMapDomain: " ++ show (IntMap.keys (odeTraceBinderReplayMap env))
            , "  replayMapKeys: " ++ show (IntSet.toList (odeTraceBinderMapDomain env))
            ]

traceBinderOrder :: OmegaDomainEnv -> [Int]
traceBinderOrder env =
    case odeTrace env of
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

traceBinderOrderIx :: OmegaDomainEnv -> IntMap.IntMap Int
traceBinderOrderIx env = IntMap.fromList (zip (traceBinderOrder env) [0 :: Int ..])

reverseCopyByTarget :: OmegaDomainEnv -> IntMap.IntMap [Int]
reverseCopyByTarget env =
    IntMap.fromListWith (++)
        [ (getNodeId dst, [src])
        | (src, dst) <- IntMap.toList (odeCopyMap env)
        ]

reverseTraceByTarget :: OmegaDomainEnv -> IntMap.IntMap [Int]
reverseTraceByTarget env =
    case odeTrace env of
        Nothing -> IntMap.empty
        Just tr ->
            IntMap.fromListWith (++)
                [ (getNodeId binder, [getNodeId binder])
                | (binder, _arg) <- etBinderArgs tr
                ]

invCopyMap :: OmegaDomainEnv -> IntMap.IntMap NodeId
invCopyMap env =
    IntMap.fromList
        [ (getNodeId v, NodeId k)
        | (k, v) <- IntMap.toList (odeCopyMap env)
        , k /= getNodeId v
        ]

witnessDomainSourceKeys :: OmegaDomainEnv -> NodeId -> [Int]
witnessDomainSourceKeys env nid =
    let keyRaw = getNodeId nid
        copyMap = odeCopyMap env
        fwdRaw = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw copyMap)
        invRaw = maybe [] (pure . getNodeId) (IntMap.lookup keyRaw (invCopyMap env))
        revCopy = IntMap.findWithDefault [] keyRaw (reverseCopyByTarget env)
        traceRaw = IntMap.findWithDefault [] keyRaw (reverseTraceByTarget env)
        rank key = (IntMap.findWithDefault maxBound key (traceBinderOrderIx env), key)
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

witnessDomainSourceMatches :: OmegaDomainEnv -> IntSet.IntSet -> NodeId -> [NodeId]
witnessDomainSourceMatches env binderKeys nid =
    [ NodeId member
    | member <- witnessDomainSourceKeys env nid
    , IntSet.member member binderKeys
    ]
