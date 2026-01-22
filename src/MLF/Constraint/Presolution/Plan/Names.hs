module MLF.Constraint.Presolution.Plan.Names (
    alphaName,
    parseNameId
) where

import MLF.Util.Names (parseNameId)

alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)
