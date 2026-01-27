module MLF.Util.Graph (
    topoSortBy,
    reachableFrom,
    reachableFromStop
) where

import Data.Functor.Foldable (ListF(..), hylo)
import Data.List (sortBy)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Util.ElabError (ElabError(..))

-- | Stable topological sort with a tie-break comparator.
--
-- Dependencies are given as edges (d -> n) via @depsFor n@. When multiple
-- nodes are ready, @cmp@ selects the next one.
topoSortBy :: String -> (Int -> Int -> Ordering) -> (Int -> [Int]) -> [Int] -> Either ElabError [Int]
topoSortBy errLabel cmp depsFor nodes =
    if length nodes < 2
        then Right nodes
        else step [] indeg0 startQueue
  where
    edges = [ (d, n) | n <- nodes, d <- depsFor n ]

    adj :: IntMap.IntMap [Int]
    adj = IntMap.fromListWith (++) [ (a, [b]) | (a, b) <- edges ]

    indeg0 :: IntMap.IntMap Int
    indeg0 =
        IntMap.fromListWith (+)
            ( [ (n, 0) | n <- nodes ]
                ++ [ (b, 1) | (_a, b) <- edges ]
            )

    startQueue =
        sortBy cmp
            [ n
            | n <- nodes
            , IntMap.findWithDefault 0 n indeg0 == 0
            ]

    step acc indeg queue =
        case queue of
            [] ->
                if length acc == length nodes
                    then Right acc
                    else Left (InstantiationError errLabel)
            (k : rest) ->
                let outs = IntMap.findWithDefault [] k adj
                    (indeg', newlyZero) =
                        foldr
                            (\j (m, zs) ->
                                case IntMap.lookup j m of
                                    Nothing -> (m, zs)
                                    Just c ->
                                        let c' = c - 1
                                            m' = IntMap.insert j c' m
                                        in if c' == 0 then (m', j : zs) else (m', zs)
                            )
                            (indeg, [])
                            outs
                    queue' = sortBy cmp (rest ++ newlyZero)
                in step (acc ++ [k]) indeg' queue'

-- | Reachability over a graph with an explicit stop predicate.
-- Stops expansion at nodes that satisfy @shouldStop@, except for the start node.
reachableFromStop
    :: (a -> Int)         -- ^ Key for visited/acc sets.
    -> (a -> a)           -- ^ Canonicalization for nodes.
    -> (a -> [a])         -- ^ Successors for a node.
    -> (a -> Bool)        -- ^ Stop expansion at a node (except start).
    -> a                  -- ^ Start node.
    -> IntSet.IntSet
reachableFromStop keyOf canonical successors shouldStop start =
    let startC = canonical start
        startKey = keyOf startC
        isStop nC = keyOf nC /= startKey && shouldStop nC
        alg Nil = IntSet.empty
        alg (Cons nC acc)
            | isStop nC = acc
            | otherwise = IntSet.insert (keyOf nC) acc
        coalg (visited, queue) =
            case queue of
                [] -> Nil
                (n:rest) ->
                    let nC = canonical n
                        key = keyOf nC
                    in if IntSet.member key visited
                        then Cons nC (visited, rest)
                        else
                            let visited' = IntSet.insert key visited
                            in if isStop nC
                                then Cons nC (visited', rest)
                                else
                                    let kids = map canonical (successors nC)
                                    in Cons nC (visited', kids ++ rest)
    in hylo alg coalg (IntSet.empty, [startC])

reachableFrom
    :: (a -> Int)         -- ^ Key for visited/acc sets.
    -> (a -> a)           -- ^ Canonicalization for nodes.
    -> (a -> [a])         -- ^ Successors for a node.
    -> a                  -- ^ Start node.
    -> IntSet.IntSet
reachableFrom keyOf canonical successors start =
    reachableFromStop keyOf canonical successors (const False) start
