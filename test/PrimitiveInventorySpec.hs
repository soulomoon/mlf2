{-# LANGUAGE GADTs #-}

module PrimitiveInventorySpec (spec) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Types.Elab (Ty (..), TypeBinderIdentity, typeBinderRefIdentity)
import MLF.Types.Identity (UniqueIdentity (..), typeBinderIdentityFromUnique, typeBinderIdentityStableName)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (ValueInfo (..), valueIdentityType, valueType)
import MLF.Frontend.Symbol (renameSymbolDefiningName, symbolIdentityStableName)
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType)
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import Test.Hspec

spec :: Spec
spec = describe "MLF.Primitive.Inventory" $ do
  it "keeps the frontend builtin registry derived from the shared primitive inventory owner" $ do
    Map.keysSet PrimitiveInventory.builtinTypeSpecs `shouldBe` PrimitiveInventory.builtinTypeNames
    Builtins.builtinTypeNames `shouldBe` PrimitiveInventory.builtinTypeNames
    Builtins.builtinOpaqueTypeNames `shouldBe` PrimitiveInventory.builtinOpaqueTypeNames
    Map.keysSet Builtins.builtinValues `shouldBe` PrimitiveInventory.primitiveValueNames
    Map.keysSet Builtins.builtinOpaqueTypes `shouldBe` PrimitiveInventory.builtinOpaqueTypeNames
    PrimitiveInventory.isBuiltinTypeName (PrimitiveInventory.builtinModuleName ++ ".Int") `shouldBe` True
    PrimitiveInventory.isOpaqueBuiltinTypeName (PrimitiveInventory.builtinModuleName ++ ".IO") `shouldBe` True
    PrimitiveInventory.builtinTypeKind (PrimitiveInventory.builtinModuleName ++ ".IO")
      `shouldBe` PrimitiveInventory.builtinTypeKind "IO"
    let intIdentity = PrimitiveInventory.builtinTypeIdentity "Int"
        stableInt = symbolIdentityStableName intIdentity
    PrimitiveInventory.normalizeBuiltinTypeReference stableInt `shouldBe` "Int"
    PrimitiveInventory.builtinTypeHeadIdentity stableInt `shouldBe` Nothing
    PrimitiveInventory.isBuiltinTypeName stableInt `shouldBe` False
    let stableHeads = Builtins.builtinSourceTypeHeadIdentities (STBase stableInt)
    Map.lookup stableInt stableHeads `shouldBe` Nothing
    Map.lookup "Int" stableHeads `shouldBe` Nothing
    let ioIdentity = PrimitiveInventory.builtinTypeIdentity "IO"
        stableIO = symbolIdentityStableName ioIdentity
    PrimitiveInventory.builtinTypeHeadIdentity stableIO `shouldBe` Nothing
    PrimitiveInventory.isBuiltinTypeName stableIO `shouldBe` False
    PrimitiveInventory.sourceTypeMentionsOpaqueBuiltin (STCon stableIO (STBase stableInt :| [])) `shouldBe` True
    let andIdentity = PrimitiveInventory.builtinValueIdentity PrimitiveInventory.nativeAndPrimitiveName
    PrimitiveInventory.builtinValueIdentity (PrimitiveInventory.builtinModuleName ++ "." ++ PrimitiveInventory.nativeAndPrimitiveName)
      `shouldBe` andIdentity

    forM_ (Map.toList PrimitiveInventory.primitiveValueSpecs) $ \(name, spec0) ->
      case Map.lookup name Builtins.builtinValues of
        Just valueInfo@OrdinaryValue {valueRuntimeName} -> do
          valueRuntimeName `shouldBe` name
          valueInfoSymbol valueInfo `shouldBe` PrimitiveInventory.builtinValueIdentity name
          valueType valueInfo
            `shouldBe` PrimitiveInventory.primitiveTypeToSourceType (PrimitiveInventory.primitiveValueType spec0)
          case Map.lookup name PrimitiveInventory.primitiveValueElabTypes of
            Just identityTy ->
              valueIdentityType valueInfo `shouldBe` elabIdentitySourceType identityTy
            Nothing ->
              expectationFailure ("missing primitive elaborated type for " ++ name)
        other ->
          expectationFailure ("expected ordinary builtin value for " ++ name ++ ", got " ++ show other)

  it "canonicalizes qualified builtin source type heads once" $ do
    let builtinHead name = PrimitiveInventory.builtinModuleName ++ "." ++ name
    PrimitiveInventory.matchesBuiltinTypeName (builtinHead "Int") "Int" `shouldBe` True
    PrimitiveInventory.canonicalizeBuiltinSourceType (STBase (builtinHead "Int"))
      `shouldBe` STBase (builtinHead "Int")
    PrimitiveInventory.canonicalizeBuiltinSourceType (STCon (builtinHead "IO") (STBase (builtinHead "Int") :| []))
      `shouldBe` STCon (builtinHead "IO") (STBase (builtinHead "Int") :| [])

  it "assigns unique generated type identities across primitive elab types" $ do
    let idsByPrimitive =
          map
            (Set.fromList . typeIdentitiesInType)
            (Map.elems PrimitiveInventory.primitiveValueElabTypes)
        ids = concatMap Set.toList idsByPrimitive
    ids `shouldSatisfy` (not . null)
    length ids `shouldBe` Set.size (Set.fromList ids)

  it "attaches builtin identities to primitive elab type heads" $ do
    PrimitiveInventory.builtinTypeIdentity (PrimitiveInventory.builtinModuleName ++ ".Int")
      `shouldBe` PrimitiveInventory.builtinTypeIdentity "Int"
    PrimitiveInventory.primitiveTypeToElabType (PrimitiveInventory.PrimitiveTypeBase "Int")
      `shouldBe` TBaseWithIdentity (Builtins.builtinTypeIdentity "Int") (BaseTy "Int")
    PrimitiveInventory.primitiveTypeToElabType (PrimitiveInventory.PrimitiveTypeCon "IO" (PrimitiveInventory.PrimitiveTypeBase "Int" :| []))
      `shouldBe` TConWithIdentity
        (Builtins.builtinTypeIdentity "IO")
        (BaseTy "IO")
        (TBaseWithIdentity (Builtins.builtinTypeIdentity "Int") (BaseTy "Int") :| [])

  it "retains the nominal List owner in the stringFromList signature" $ do
    let expectedTy =
          PrimitiveInventory.PrimitiveTypeArrow
            ( PrimitiveInventory.PrimitiveTypeCon
                "List"
                (PrimitiveInventory.PrimitiveTypeBase "Char" :| [])
            )
            (PrimitiveInventory.PrimitiveTypeBase "String")
    PrimitiveInventory.primitiveValueType
      <$> Map.lookup PrimitiveInventory.stringFromListPrimitiveName PrimitiveInventory.primitiveValueSpecs
      `shouldBe` Just expectedTy

  it "generates primitive type binder identities for stable-looking names" $ do
    let stableName = "$typevar#991611"
        stableIdentity = typeBinderIdentityFromUnique (UniqueIdentity 0)
        freshIdentity = typeBinderIdentityFromUnique (UniqueIdentity 1)
        sourceTy =
          PrimitiveInventory.PrimitiveTypeForall
            stableName
            ( PrimitiveInventory.PrimitiveTypeForall
                "a"
                ( PrimitiveInventory.PrimitiveTypeArrow
                    (PrimitiveInventory.PrimitiveTypeVar stableName)
                    (PrimitiveInventory.PrimitiveTypeVar "a")
                )
            )
    case PrimitiveInventory.primitiveTypeToElabType sourceTy of
      TForallRef stableRef Nothing (TForallRef freshRef Nothing (TArrow (TVarRef stableOcc) (TVarRef freshOcc))) -> do
        typeBinderRefIdentity stableRef `shouldBe` stableIdentity
        typeBinderRefIdentity stableOcc `shouldBe` stableIdentity
        typeBinderRefIdentity freshRef `shouldBe` freshIdentity
        typeBinderRefIdentity freshOcc `shouldBe` freshIdentity
      other ->
        expectationFailure ("unexpected primitive elab type: " ++ show other)

  it "classifies native-lowerable primitive support by exact shared primitive identity" $ do
    let andIdentity = PrimitiveInventory.builtinValueIdentity PrimitiveInventory.nativeAndPrimitiveName
        staleAndIdentity =
          renameSymbolDefiningName "$stale_and" (PrimitiveInventory.builtinValueIdentity PrimitiveInventory.nativeAndPrimitiveName)
    PrimitiveInventory.primitiveValueNameByIdentity andIdentity
      `shouldBe` Just PrimitiveInventory.nativeAndPrimitiveName
    PrimitiveInventory.primitiveValueNameByIdentity staleAndIdentity
      `shouldBe` Nothing
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
          PrimitiveInventory.stringEqualsPrimitiveName,
          PrimitiveInventory.stringStartsWithPrimitiveName,
          PrimitiveInventory.stringEndsWithPrimitiveName,
          PrimitiveInventory.stringAppendPrimitiveName,
          PrimitiveInventory.stringReplaceCharPrimitiveName,
          PrimitiveInventory.stringReplacePrimitiveName,
          PrimitiveInventory.stringIndexOfCharPrimitiveName,
          PrimitiveInventory.stringIndexOfPrimitiveName,
          PrimitiveInventory.stringSplitPrimitiveName,
          PrimitiveInventory.stringJoinPrimitiveName,
          PrimitiveInventory.stringSplitCharPrimitiveName,
          PrimitiveInventory.stringComparePrimitiveName,
          PrimitiveInventory.stringFromCharPrimitiveName,
          PrimitiveInventory.stringFromIntPrimitiveName,
          PrimitiveInventory.stringFromBoolPrimitiveName,
          PrimitiveInventory.stringFromNatPrimitiveName,
          PrimitiveInventory.stringFromListPrimitiveName,
          PrimitiveInventory.stringToListPrimitiveName,
          PrimitiveInventory.stringDropPrimitiveName,
          PrimitiveInventory.stringTakePrimitiveName,
          PrimitiveInventory.stringSlicePrimitiveName,
          PrimitiveInventory.stringCharAtPrimitiveName,
          PrimitiveInventory.stringCharAtOptionPrimitiveName,
          PrimitiveInventory.charIsDigitPrimitiveName,
          PrimitiveInventory.charIsAsciiLowerPrimitiveName,
          PrimitiveInventory.charIsAsciiUpperPrimitiveName,
          PrimitiveInventory.charIsAsciiAlphaPrimitiveName,
          PrimitiveInventory.charIsAsciiAlphaNumPrimitiveName,
          PrimitiveInventory.charIsAsciiIdentifierStartPrimitiveName,
          PrimitiveInventory.charIsAsciiIdentifierContinuePrimitiveName,
          PrimitiveInventory.charIsAsciiWhitespacePrimitiveName,
          PrimitiveInventory.charIsAsciiPunctuationPrimitiveName,
          PrimitiveInventory.charIsAsciiPrintablePrimitiveName,
          PrimitiveInventory.charIsAsciiHexDigitPrimitiveName,
          PrimitiveInventory.charIsAsciiLineBreakPrimitiveName,
          PrimitiveInventory.charIsAsciiControlPrimitiveName,
          PrimitiveInventory.charToAsciiLowerPrimitiveName,
          PrimitiveInventory.charToAsciiUpperPrimitiveName,
          PrimitiveInventory.stringToAsciiLowerPrimitiveName,
          PrimitiveInventory.stringToAsciiUpperPrimitiveName
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
        PrimitiveInventory.PrimitiveNativeStringEquals ->
          name `shouldBe` PrimitiveInventory.stringEqualsPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringStartsWith ->
          name `shouldBe` PrimitiveInventory.stringStartsWithPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringEndsWith ->
          name `shouldBe` PrimitiveInventory.stringEndsWithPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringAppend ->
          name `shouldBe` PrimitiveInventory.stringAppendPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringReplaceChar ->
          name `shouldBe` PrimitiveInventory.stringReplaceCharPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringReplace ->
          name `shouldBe` PrimitiveInventory.stringReplacePrimitiveName
        PrimitiveInventory.PrimitiveNativeStringIndexOfChar ->
          name `shouldBe` PrimitiveInventory.stringIndexOfCharPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringIndexOf ->
          name `shouldBe` PrimitiveInventory.stringIndexOfPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringSplit ->
          name `shouldBe` PrimitiveInventory.stringSplitPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringJoin ->
          name `shouldBe` PrimitiveInventory.stringJoinPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringSplitChar ->
          name `shouldBe` PrimitiveInventory.stringSplitCharPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringCompare ->
          name `shouldBe` PrimitiveInventory.stringComparePrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromChar ->
          name `shouldBe` PrimitiveInventory.stringFromCharPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromInt ->
          name `shouldBe` PrimitiveInventory.stringFromIntPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromBool ->
          name `shouldBe` PrimitiveInventory.stringFromBoolPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromNat ->
          name `shouldBe` PrimitiveInventory.stringFromNatPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringFromList ->
          name `shouldBe` PrimitiveInventory.stringFromListPrimitiveName
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
        PrimitiveInventory.PrimitiveNativeStringCharAtOption ->
          name `shouldBe` PrimitiveInventory.stringCharAtOptionPrimitiveName
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
        PrimitiveInventory.PrimitiveNativeCharIsAsciiHexDigit ->
          name `shouldBe` PrimitiveInventory.charIsAsciiHexDigitPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiLineBreak ->
          name `shouldBe` PrimitiveInventory.charIsAsciiLineBreakPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharIsAsciiControl ->
          name `shouldBe` PrimitiveInventory.charIsAsciiControlPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharToAsciiLower ->
          name `shouldBe` PrimitiveInventory.charToAsciiLowerPrimitiveName
        PrimitiveInventory.PrimitiveNativeCharToAsciiUpper ->
          name `shouldBe` PrimitiveInventory.charToAsciiUpperPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringToAsciiLower ->
          name `shouldBe` PrimitiveInventory.stringToAsciiLowerPrimitiveName
        PrimitiveInventory.PrimitiveNativeStringToAsciiUpper ->
          name `shouldBe` PrimitiveInventory.stringToAsciiUpperPrimitiveName
        PrimitiveInventory.PrimitiveNativeIO operation ->
          name `shouldBe` PrimitiveInventory.nativeIOPrimitiveName operation

elabIdentitySourceType :: Ty v -> SrcType
elabIdentitySourceType ty =
  case ty of
    TVarRef ref -> STVar (binderName ref)
    TArrow dom cod -> STArrow (elabIdentitySourceType dom) (elabIdentitySourceType cod)
    TBaseWithIdentity identity (BaseTy name) -> STBase (headName identity name)
    TConWithIdentity identity (BaseTy name) args ->
      STCon (headName identity name) (fmap elabIdentitySourceType args)
    TVarAppRef ref args -> STVarApp (binderName ref) (fmap elabIdentitySourceType args)
    TForallRef ref mbBound body ->
      STForall
        (binderName ref)
        (fmap (SrcBound . elabIdentitySourceType) mbBound)
        (elabIdentitySourceType body)
    TMuRef ref body -> STMu (binderName ref) (elabIdentitySourceType body)
    TBottom -> STBottom
  where
    binderName = typeBinderIdentityStableName . typeBinderRefIdentity
    headName identity _ = symbolIdentityStableName identity

allNativeIOOperations :: [PrimitiveInventory.PrimitiveIOOperation]
allNativeIOOperations = [minBound .. maxBound]

typeIdentitiesInType :: Ty v -> [TypeBinderIdentity]
typeIdentitiesInType ty =
  case ty of
    TVarRef ref ->
      [typeBinderRefIdentity ref]
    TArrow dom cod ->
      typeIdentitiesInType dom ++ typeIdentitiesInType cod
    TConWithIdentity _ _ args ->
      foldMap typeIdentitiesInType args
    TVarAppRef ref args ->
      typeBinderRefIdentity ref : foldMap typeIdentitiesInType args
    TBaseWithIdentity _ _ ->
      []
    TForallRef ref mb body ->
      [typeBinderRefIdentity ref]
        ++ maybe [] typeIdentitiesInType mb
        ++ typeIdentitiesInType body
    TMuRef ref body ->
      typeBinderRefIdentity ref : typeIdentitiesInType body
    TBottom ->
      []
