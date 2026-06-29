module MLF.Util.Names (
    alphaName,
    freshNameLike,
) where

import qualified Data.Set as Set

alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)

freshNameLike :: String -> Set.Set String -> String
freshNameLike base used =
    case filter (`Set.notMember` used) candidates of
        (x:_) -> x
        [] -> base
  where
    candidates = base : [base ++ show i | i <- [(1::Int)..]]
