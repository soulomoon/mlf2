module MLF.Util.Names (
    alphaName,
    parseNameId,
    freshNameLike,
) where

import qualified Data.Set as Set

alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)

parseNameId :: String -> Maybe Int
parseNameId name =
    case name of
        ('t':rest) ->
            case reads rest of
                [(n, "")] -> Just n
                _ -> Nothing
        _ -> Nothing

freshNameLike :: String -> Set.Set String -> String
freshNameLike base used =
    case filter (`Set.notMember` used) candidates of
        (x:_) -> x
        [] -> base
  where
    candidates = base : [base ++ show i | i <- [(1::Int)..]]
