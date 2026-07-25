{-# LANGUAGE PatternSynonyms #-}

module IdentityTestSupport
  ( testTypeIdentity,
    pattern TestTyBase,
    pattern TestTyCon,
    testElabBase,
    testElabCon,
  )
where

import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId, TyNode (..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (SymbolType), symbolIdentityFromParts)
import MLF.Primitive.Identity (builtinTypeHeadIdentity)
import MLF.Types.Elab (ElabType, Ty (TBaseWithIdentity, TConWithIdentity))
import MLF.Types.Unique (UniqueIdentity (..))

testTypeIdentity :: String -> SymbolIdentity
testTypeIdentity name =
  fromMaybe
    (symbolIdentityFromParts (UniqueIdentity (1000000000 + stableNameHash name)) SymbolType "<test>" name Nothing)
    (builtinTypeHeadIdentity name)

pattern TestTyBase :: NodeId -> BaseTy -> TyNode
pattern TestTyBase node base <- TyBase node _ base
  where
    TestTyBase node base@(BaseTy name) =
      TyBase node (testTypeIdentity name) base

pattern TestTyCon :: NodeId -> BaseTy -> NonEmpty NodeId -> TyNode
pattern TestTyCon node base args <- TyCon node _ base args
  where
    TestTyCon node base@(BaseTy name) args =
      TyCon node (testTypeIdentity name) base args

testElabBase :: BaseTy -> ElabType
testElabBase base@(BaseTy name) =
  TBaseWithIdentity (testTypeIdentity name) base

testElabCon :: BaseTy -> NonEmpty ElabType -> ElabType
testElabCon base@(BaseTy name) =
  TConWithIdentity (testTypeIdentity name) base

stableNameHash :: String -> Int
stableNameHash =
  foldl (\hash char -> (hash * 131 + ord char) `mod` 100000000) 0
