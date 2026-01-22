module MLF.Constraint.Presolution.Plan.Ordering (
    orderBinderCandidates
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Debug.Trace (trace)

import MLF.Constraint.Types
import MLF.Util.ElabError (ElabError)
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..))
import MLF.Util.Graph (topoSortBy)
import qualified MLF.Util.Order as Order

traceOrderingEnabledM :: Bool -> String -> Either ElabError ()
traceOrderingEnabledM enabled msg =
    if enabled then trace msg (Right ()) else Right ()

orderBinderCandidates
    :: Bool
    -> Maybe GaBindParentsInfo
    -> (NodeId -> NodeId)
    -> Constraint
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
    orderBinderCandidatesSolved keysSolved candidates' depsForE' = do
        let keys = keysSolved
            candidateSet = IntSet.fromList candidates'
            keyMaybe k = IntMap.lookup k keys
            cmpReady a b =
                case (keyMaybe a, keyMaybe b) of
                    (Just ka, Just kb) ->
                        case Order.compareOrderKey ka kb of
                            EQ -> compare a b
                            other -> other
                    _ -> compare a b

        depsList <- mapM
            (\k -> do
                deps <- depsForE' k
                pure (k, filter (\d -> d /= k && IntSet.member d candidateSet) deps)
            )
            candidates'
        let depsMap = IntMap.fromList depsList
            depsFor k = IntMap.findWithDefault [] k depsMap

        topoSortBy
            "generalizeAt: cycle in binder bound dependencies"
            cmpReady
            depsFor
            candidates'

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

        traceOrderingEnabledM debugEnabled
            ("generalizeAt: missing base order keys (falling back to solved) "
                ++ show (map NodeId missingKeys)
            )

        depsList <- mapM
            (\k -> do
                deps <- depsForE' k
                pure (k, filter (\d -> d /= k && IntSet.member d candidateSet) deps)
            )
            candidates'
        let depsMap = IntMap.fromList depsList
            depsFor k = IntMap.findWithDefault [] k depsMap

        topoSortBy
            "generalizeAt: cycle in binder bound dependencies"
            cmpReady
            depsFor
            candidates'
