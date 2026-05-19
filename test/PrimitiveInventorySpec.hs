module PrimitiveInventorySpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

  it "classifies native-lowerable primitive support from the shared primitive inventory owner" $ do
    PrimitiveInventory.primitiveNativeSupport PrimitiveInventory.nativeAndPrimitiveName
      `shouldBe` Just PrimitiveInventory.PrimitiveNativeBooleanAnd

    PrimitiveInventory.nativeIOPrimitiveNames
      `shouldBe` Set.fromList (map PrimitiveInventory.nativeIOPrimitiveName allNativeIOOperations)
    PrimitiveInventory.nativeLowerablePrimitiveNames
      `shouldBe` Set.fromList
        [ PrimitiveInventory.nativeAndPrimitiveName,
          PrimitiveInventory.stringLengthPrimitiveName,
          PrimitiveInventory.stringIsEmptyPrimitiveName,
          PrimitiveInventory.stringContainsCharPrimitiveName,
          PrimitiveInventory.stringContainsPrimitiveName,
          PrimitiveInventory.stringStartsWithPrimitiveName,
          PrimitiveInventory.stringEndsWithPrimitiveName,
          PrimitiveInventory.stringAppendPrimitiveName,
          PrimitiveInventory.stringFromCharPrimitiveName,
          PrimitiveInventory.stringFromIntPrimitiveName,
          PrimitiveInventory.stringFromBoolPrimitiveName,
          PrimitiveInventory.stringFromNatPrimitiveName,
          PrimitiveInventory.stringToListPrimitiveName,
          PrimitiveInventory.stringDropPrimitiveName,
          PrimitiveInventory.stringTakePrimitiveName,
          PrimitiveInventory.stringSlicePrimitiveName,
          PrimitiveInventory.stringCharAtPrimitiveName,
          PrimitiveInventory.charIsDigitPrimitiveName,
          PrimitiveInventory.charIsAsciiLowerPrimitiveName,
          PrimitiveInventory.charIsAsciiUpperPrimitiveName,
          PrimitiveInventory.charIsAsciiAlphaPrimitiveName,
          PrimitiveInventory.charIsAsciiAlphaNumPrimitiveName,
          PrimitiveInventory.charIsAsciiIdentifierStartPrimitiveName,
          PrimitiveInventory.charIsAsciiIdentifierContinuePrimitiveName,
          PrimitiveInventory.charIsAsciiWhitespacePrimitiveName,
          PrimitiveInventory.charIsAsciiPunctuationPrimitiveName,
          PrimitiveInventory.charIsAsciiPrintablePrimitiveName
        ]
        <> PrimitiveInventory.nativeIOPrimitiveNames
    PrimitiveInventory.nativeLowerablePrimitiveNames
      `shouldSatisfy` (`Set.isSubsetOf` PrimitiveInventory.primitiveValueNames)

    forM_ (Map.toList PrimitiveInventory.primitiveValueSpecs) $ \(name, spec0) ->
      case PrimitiveInventory.primitiveValueNativeSupport spec0 of
        PrimitiveInventory.PrimitiveNativeUnsupported ->
          name `shouldNotSatisfy` (`Set.member` PrimitiveInventory.nativeLowerablePrimitiveNames)
        PrimitiveInventory.PrimitiveNativeBooleanAnd ->
          name `shouldBe` PrimitiveInventory.nativeAndPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringLength ->
          name `shouldBe` PrimitiveInventory.stringLengthPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringIsEmpty ->
          name `shouldBe` PrimitiveInventory.stringIsEmptyPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringContainsChar ->
          name `shouldBe` PrimitiveInventory.stringContainsCharPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringContains ->
          name `shouldBe` PrimitiveInventory.stringContainsPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringStartsWith ->
          name `shouldBe` PrimitiveInventory.stringStartsWithPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringEndsWith ->
          name `shouldBe` PrimitiveInventory.stringEndsWithPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringAppend ->
          name `shouldBe` PrimitiveInventory.stringAppendPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromChar ->
          name `shouldBe` PrimitiveInventory.stringFromCharPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromInt ->
          name `shouldBe` PrimitiveInventory.stringFromIntPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromBool ->
          name `shouldBe` PrimitiveInventory.stringFromBoolPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromNat ->
          name `shouldBe` PrimitiveInventory.stringFromNatPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringToList ->
          name `shouldBe` PrimitiveInventory.stringToListPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringDrop ->
          name `shouldBe` PrimitiveInventory.stringDropPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringTake ->
          name `shouldBe` PrimitiveInventory.stringTakePrimitiveName
        PrimitiveInventory.PrimitiveNativeStringSlice ->
          name `shouldBe` PrimitiveInventory.stringSlicePrimitiveName
        PrimitiveInventory.PrimitiveNativeStringCharAt ->
          name `shouldBe` PrimitiveInventory.stringCharAtPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsDigit ->
          name `shouldBe` PrimitiveInventory.charIsDigitPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiLower ->
          name `shouldBe` PrimitiveInventory.charIsAsciiLowerPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiUpper ->
          name `shouldBe` PrimitiveInventory.charIsAsciiUpperPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiAlpha ->
          name `shouldBe` PrimitiveInventory.charIsAsciiAlphaPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiAlphaNum ->
          name `shouldBe` PrimitiveInventory.charIsAsciiAlphaNumPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiIdentifierStart ->
          name `shouldBe` PrimitiveInventory.charIsAsciiIdentifierStartPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiIdentifierContinue ->
          name `shouldBe` PrimitiveInventory.charIsAsciiIdentifierContinuePrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiWhitespace ->
          name `shouldBe` PrimitiveInventory.charIsAsciiWhitespacePrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiPunctuation ->
          name `shouldBe` PrimitiveInventory.charIsAsciiPunctuationPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiPrintable ->
          name `shouldBe` PrimitiveInventory.charIsAsciiPrintablePrimitiveName
        PrimitiveInventory.PrimitiveNativeIO operation ->
          name `shouldBe` PrimitiveInventory.nativeIOPrimitiveName operation

allNativeIOOperations :: [PrimitiveInventory.PrimitiveIOOperation]
allNativeIOOperations = [minBound .. maxBound]
