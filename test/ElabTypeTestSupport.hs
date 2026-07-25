{-# LANGUAGE DataKinds #-}

module ElabTypeTestSupport
  ( tBase,
    tCon,
    testTypeHeadIdentity,
  )
where

import Data.List.NonEmpty (NonEmpty)
import IdentityTestSupport (testTypeIdentity)
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Types.Elab (Ty)
import qualified MLF.Types.Elab as Elab

tBase :: BaseTy -> Ty v
tBase base@(BaseTy name) =
  Elab.tBase (testTypeHeadIdentity name) base

tCon :: BaseTy -> NonEmpty (Ty 'Elab.AllowVar) -> Ty v
tCon base@(BaseTy name) =
  Elab.tCon (testTypeHeadIdentity name) base

testTypeHeadIdentity :: String -> SymbolIdentity
testTypeHeadIdentity =
  testTypeIdentity
