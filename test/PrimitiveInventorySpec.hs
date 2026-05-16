module PrimitiveInventorySpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (ValueInfo (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import Test.Hspec

spec :: Spec
spec = describe "MLF.Primitive.Inventory" $ do
  it "keeps the frontend builtin registry derived from the shared primitive inventory owner" $ do
    Builtins.builtinTypeNames `shouldBe` PrimitiveInventory.builtinTypeNames
    Builtins.builtinOpaqueTypeNames `shouldBe` PrimitiveInventory.builtinOpaqueTypeNames
    Map.keysSet Builtins.builtinValues `shouldBe` PrimitiveInventory.primitiveValueNames
    Map.keysSet Builtins.builtinOpaqueTypes `shouldBe` PrimitiveInventory.builtinOpaqueTypeNames

    forM_ (Map.toList PrimitiveInventory.primitiveValueSpecs) $ \(name, spec0) ->
      case Map.lookup name Builtins.builtinValues of
        Just OrdinaryValue {valueDisplayName, valueRuntimeName, valueType, valueIdentityType} -> do
          valueDisplayName `shouldBe` name
          valueRuntimeName `shouldBe` name
          valueType
            `shouldBe` PrimitiveInventory.primitiveTypeToSourceType (PrimitiveInventory.primitiveValueType spec0)
          valueIdentityType
            `shouldBe` PrimitiveInventory.canonicalizeBuiltinSourceType valueType
        other ->
          expectationFailure ("expected ordinary builtin value for " ++ name ++ ", got " ++ show other)
