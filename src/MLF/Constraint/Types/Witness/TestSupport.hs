{- |
Module      : MLF.Constraint.Types.Witness.TestSupport
Description : Explicit raw witness fixture seam for tests
Copyright   : (c) 2024
License     : BSD-3-Clause

This module intentionally re-exports the raw witness constructors so negative
tests can build malformed fixtures without widening the default production
surface in "MLF.Constraint.Types.Witness".
-}
module MLF.Constraint.Types.Witness.TestSupport (
    InstanceWitness(..),
    EdgeWitness(..)
) where

import MLF.Constraint.Types.Witness.Internal (EdgeWitness(..), InstanceWitness(..))
