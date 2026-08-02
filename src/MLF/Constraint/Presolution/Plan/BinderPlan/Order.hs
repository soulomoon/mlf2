{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Order
Description : Binder ordering helpers
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Presolution.Plan.BinderPlan.Order (
    GaBindParentsInfo(..),
    orderBinderCandidates
) where

import Control.Monad (forM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Plan.Requirements (ExpansionConstructionPlacements)
import MLF.Constraint.Types.Graph
import MLF.Util.ElabError (ElabError(..))
import MLF.Util.Graph (topoSortBy)
import qualified MLF.Util.Order as Order
import MLF.Util.Trace (traceWhen)

data GaBindParentsInfo p = GaBindParentsInfo
    { gbiBindParentsBase :: BindParents
    , gbiBaseConstraint :: Constraint p
    , gbiBaseToSolved :: IntMap.IntMap NodeId
    , gbiSolvedToBase :: IntMap.IntMap NodeId
    , gbiRestoredSchemeRootTargets :: IntMap.IntMap NodeId
    , gbiExpansionConstructionPlacements :: ExpansionConstructionPlacements
    }

-- | Order binder candidates topologically by their bound dependencies.
orderBinderCandidates
    :: Bool
    -> Maybe (GaBindParentsInfo p)
    -> (NodeId -> NodeId)
    -> Constraint p
    -> NodeId
    -> NodeId
    -> [Int]
    -> (Int -> Either ElabError [Int])
    -> Either ElabError [Int]
orderBinderCandidates debugEnabled mbBindParentsGa canonical constraint root rootBase candidates depsForE =
    let keysSolved = Order.orderKeysFromConstraintWith canonical constraint root Nothing
    in case mbBindParentsGa of
        Nothing -> orderBinderCandidatesSolved keysSolved candidates depsForE
        Just ga -> orderBinderCandidatesBase ga keysSolved rootBase candidates depsForE
  where
    traceOrderingEnabledM enabled msg =
        traceWhen enabled msg (Right ())

    orderBinderCandidatesSolved keysSolved candidates' depsForE' = do
        let candidateSet = IntSet.fromList candidates'
            keyMaybe k = IntMap.lookup k keysSolved
            cmpReady a b =
                case (keyMaybe a, keyMaybe b) of
                    (Just ka, Just kb) ->
                        case Order.compareOrderKey ka kb of
                            EQ -> compare a b
                            other -> other
                    _ -> compare a b
            filterDeps k deps = filter (\d -> d /= k && IntSet.member d candidateSet) deps

        depsList <- forM candidates' $ \k -> do
            deps <- depsForE' k
            pure (k, filterDeps k deps)
        let depsFor k = IntMap.findWithDefault [] k (IntMap.fromList depsList)

        case
            topoSortBy
                "generalizeAt: cycle in binder bound dependencies"
                cmpReady
                depsFor
                candidates' of
            Right ordered -> pure ordered
            Left cause ->
                Left
                    (ValidationFailed
                        [ "generalizeAt: cycle in binder bound dependencies"
                        , "  candidates/dependencies: " ++ show depsList
                        , "  solved candidates: "
                            ++ show
                                [ ( k
                                  , lookupNodeIn
                                        (cNodes constraint)
                                        (NodeId k)
                                  )
                                | k <- candidates'
                                ]
                        , "  cause: " ++ show cause
                        ])

    orderBinderCandidatesBase ga keysSolved rootBase' candidates' depsForE' = do
        let baseConstraint = gbiBaseConstraint ga
            keysBase = Order.orderKeysFromConstraintWith id baseConstraint rootBase' Nothing
            candidateSet = IntSet.fromList candidates'
            toBase k = IntMap.lookup k (gbiSolvedToBase ga)
            keyBase k = toBase k >>= (\b -> IntMap.lookup (getNodeId b) keysBase)
            keySolved k = IntMap.lookup k keysSolved
            missingKeys =
                [ k
                | k <- candidates'
                , Just baseN <- [toBase k]
                , not (IntMap.member (getNodeId baseN) keysBase)
                ]
            cmpReady a b =
                case (keyBase a, keyBase b) of
                    (Just ka, Just kb) ->
                        case Order.compareOrderKey ka kb of
                            EQ -> compare a b
                            other -> other
                    _ ->
                        case (keySolved a, keySolved b) of
                            (Just sa, Just sb) ->
                                case Order.compareOrderKey sa sb of
                                    EQ -> compare a b
                                    other -> other
                            _ -> compare a b
            filterDeps k deps = filter (\d -> d /= k && IntSet.member d candidateSet) deps

        traceOrderingEnabledM debugEnabled
            ("generalizeAt: missing base order keys (falling back to solved) "
                ++ show (map NodeId missingKeys)
            )

        depsList <- forM candidates' $ \k -> do
            deps <- depsForE' k
            pure (k, filterDeps k deps)
        let depsFor k = IntMap.findWithDefault [] k (IntMap.fromList depsList)

        case
            topoSortBy
                "generalizeAt: cycle in binder bound dependencies"
                cmpReady
                depsFor
                candidates' of
            Right ordered -> pure ordered
            Left cause ->
                Left
                    (ValidationFailed
                        [ "generalizeAt: cycle in binder bound dependencies"
                        , "  candidates/dependencies: " ++ show depsList
                        , "  solved candidates: "
                            ++ show
                                [ ( k
                                  , lookupNodeIn
                                        (cNodes constraint)
                                        (NodeId k)
                                  )
                                | k <- candidates'
                                ]
                        , "  base candidates: "
                            ++ show
                                [ ( k
                                  , toBase k
                                      >>= \baseNode ->
                                        lookupNodeIn
                                            (cNodes baseConstraint)
                                            baseNode
                                  )
                                | k <- candidates'
                                ]
                        , "  cause: " ++ show cause
                        ])
