module MyLib
    ( module MLF.Types
    , module MLF.Syntax
    , ConstraintResult (..)
    , ConstraintError (..)
    , inferConstraintGraph
    , someFunc
    ) where

import MLF.ConstraintGen (ConstraintError (..), ConstraintResult (..), generateConstraints)
import MLF.Syntax
import MLF.Types

inferConstraintGraph :: Expr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints

someFunc :: IO ()
someFunc = putStrLn "someFunc"
