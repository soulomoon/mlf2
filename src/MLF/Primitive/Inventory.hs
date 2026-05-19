{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Primitive.Inventory
  ( BuiltinTypeSpec (..),
    PrimitiveIOOperation (..),
    PrimitiveNativeSupport (..),
    PrimitiveType (..),
    PrimitiveValueSpec (..),
    builtinModuleName,
    builtinTypeSpecs,
    builtinTypeNames,
    builtinOpaqueTypeNames,
    isBuiltinTypeName,
    isOpaqueBuiltinTypeName,
    builtinTypeKind,
    qualifyBuiltinTypeName,
    normalizeBuiltinTypeReference,
    matchesBuiltinTypeName,
    primitiveValueSpecs,
    primitiveValueNames,
    primitiveNativeSupport,
    nativeAndPrimitiveName,
    stringLengthPrimitiveName,
    stringIsEmptyPrimitiveName,
    stringContainsCharPrimitiveName,
    stringContainsPrimitiveName,
    stringStartsWithPrimitiveName,
    stringEndsWithPrimitiveName,
    stringAppendPrimitiveName,
    stringDropPrimitiveName,
    stringTakePrimitiveName,
    stringSlicePrimitiveName,
    stringCharAtPrimitiveName,
    charIsDigitPrimitiveName,
    charIsAsciiLowerPrimitiveName,
    charIsAsciiUpperPrimitiveName,
    charIsAsciiAlphaPrimitiveName,
    charIsAsciiAlphaNumPrimitiveName,
    charIsAsciiIdentifierStartPrimitiveName,
    charIsAsciiIdentifierContinuePrimitiveName,
    charIsAsciiWhitespacePrimitiveName,
    charIsAsciiPunctuationPrimitiveName,
    charIsAsciiPrintablePrimitiveName,
    nativeIOPrimitiveName,
    nativeIOPrimitiveNames,
    nativeLowerablePrimitiveNames,
    primitiveTypeToSourceType,
    primitiveTypeToElabType,
    canonicalizeBuiltinSourceType,
    sourceTypeMentionsOpaqueBuiltin,
  )
where

import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Types (ElabType, Ty (..))
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType)
import qualified MLF.Frontend.Syntax.Program as P

data BuiltinTypeSpec = BuiltinTypeSpec
  { builtinTypeSpecKind :: P.SrcKind,
    builtinTypeSpecParameters :: [String],
    builtinTypeSpecOpaque :: Bool
  }
  deriving (Eq, Show)

data PrimitiveType
  = PrimitiveTypeVar String
  | PrimitiveTypeArrow PrimitiveType PrimitiveType
  | PrimitiveTypeBase String
  | PrimitiveTypeCon String (NonEmpty PrimitiveType)
  | PrimitiveTypeForall String PrimitiveType
  deriving (Eq, Show)

data PrimitiveIOOperation
  = PrimitiveIOPure
  | PrimitiveIOBind
  | PrimitiveIOMap
  | PrimitiveIOPutStrLn
  | PrimitiveIOGetLine
  | PrimitiveIOPutStr
  | PrimitiveIOReadFile
  | PrimitiveIOWriteFile
  | PrimitiveIOAppendFile
  | PrimitiveIOExitWith
  | PrimitiveIONewIORef
  | PrimitiveIOReadIORef
  | PrimitiveIOWriteIORef
  | PrimitiveIOGetArgs
  deriving (Bounded, Enum, Eq, Ord, Show)

data PrimitiveNativeSupport
  = PrimitiveNativeUnsupported
  | PrimitiveNativeBooleanAnd
  | PrimitiveNativeStringLength
  | PrimitiveNativeStringIsEmpty
  | PrimitiveNativeStringContainsChar
  | PrimitiveNativeStringContains
  | PrimitiveNativeStringStartsWith
  | PrimitiveNativeStringEndsWith
  | PrimitiveNativeStringAppend
  | PrimitiveNativeStringDrop
  | PrimitiveNativeStringTake
  | PrimitiveNativeStringSlice
  | PrimitiveNativeStringCharAt
  | PrimitiveNativeCharIsDigit
  | PrimitiveNativeCharIsAsciiLower
  | PrimitiveNativeCharIsAsciiUpper
  | PrimitiveNativeCharIsAsciiAlpha
  | PrimitiveNativeCharIsAsciiAlphaNum
  | PrimitiveNativeCharIsAsciiIdentifierStart
  | PrimitiveNativeCharIsAsciiIdentifierContinue
  | PrimitiveNativeCharIsAsciiWhitespace
  | PrimitiveNativeCharIsAsciiPunctuation
  | PrimitiveNativeCharIsAsciiPrintable
  | PrimitiveNativeIO PrimitiveIOOperation
  deriving (Eq, Ord, Show)

data PrimitiveValueSpec = PrimitiveValueSpec
  { primitiveValueType :: PrimitiveType,
    primitiveValueClosureValueArguments :: Set Int,
    primitiveValueNativeSupport :: PrimitiveNativeSupport
  }
  deriving (Eq, Show)

builtinModuleName :: String
builtinModuleName = "<builtin>"

builtinTypeSpecs :: Map String BuiltinTypeSpec
builtinTypeSpecs =
  Map.fromList
    [ ("Int", BuiltinTypeSpec P.KType [] False),
      ("Bool", BuiltinTypeSpec P.KType [] False),
      ("Char", BuiltinTypeSpec P.KType [] False),
      ("String", BuiltinTypeSpec P.KType [] False),
      ("IO", BuiltinTypeSpec (P.KArrow P.KType P.KType) ["a"] True),
      ("IORef", BuiltinTypeSpec (P.KArrow P.KType P.KType) ["a"] True)
    ]

builtinTypeNames :: Set String
builtinTypeNames = Map.keysSet builtinTypeSpecs

builtinOpaqueTypeNames :: Set String
builtinOpaqueTypeNames =
  Map.keysSet (Map.filter builtinTypeSpecOpaque builtinTypeSpecs)

isBuiltinTypeName :: String -> Bool
isBuiltinTypeName = (`Map.member` builtinTypeSpecs)

isOpaqueBuiltinTypeName :: String -> Bool
isOpaqueBuiltinTypeName = (`Set.member` builtinOpaqueTypeNames)

builtinTypeKind :: String -> Maybe P.SrcKind
builtinTypeKind name =
  builtinTypeSpecKind <$> Map.lookup name builtinTypeSpecs

qualifyBuiltinTypeName :: String -> String
qualifyBuiltinTypeName name = builtinModuleName ++ "." ++ name

normalizeBuiltinTypeReference :: String -> String
normalizeBuiltinTypeReference name =
  case stripPrefix (builtinModuleName ++ ".") name of
    Just builtinName
      | isBuiltinTypeName builtinName -> builtinName
    _ -> name

matchesBuiltinTypeName :: String -> String -> Bool
matchesBuiltinTypeName builtinName referenceName =
  normalizeBuiltinTypeReference referenceName == builtinName

primitiveValueSpecs :: Map String PrimitiveValueSpec
primitiveValueSpecs =
  Map.fromList
    [ ( andPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeBooleanAnd
          (boolTy `PrimitiveTypeArrow` (boolTy `PrimitiveTypeArrow` boolTy))
          Set.empty
      ),
      ( stringLengthPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringLength
          (stringTy `PrimitiveTypeArrow` intTy)
          Set.empty
      ),
      ( stringIsEmptyPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringIsEmpty
          (stringTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( stringContainsCharPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringContainsChar
          (stringTy `PrimitiveTypeArrow` (charTy `PrimitiveTypeArrow` boolTy))
          Set.empty
      ),
      ( stringContainsPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringContains
          (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` boolTy))
          Set.empty
      ),
      ( stringStartsWithPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringStartsWith
          (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` boolTy))
          Set.empty
      ),
      ( stringEndsWithPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringEndsWith
          (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` boolTy))
          Set.empty
      ),
      ( stringAppendPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringAppend
          (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` stringTy))
          Set.empty
      ),
      ( stringDropPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringDrop
          (stringTy `PrimitiveTypeArrow` (intTy `PrimitiveTypeArrow` stringTy))
          Set.empty
      ),
      ( stringTakePrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringTake
          (stringTy `PrimitiveTypeArrow` (intTy `PrimitiveTypeArrow` stringTy))
          Set.empty
      ),
      ( stringSlicePrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringSlice
          (stringTy `PrimitiveTypeArrow` (intTy `PrimitiveTypeArrow` (intTy `PrimitiveTypeArrow` stringTy)))
          Set.empty
      ),
      ( stringCharAtPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeStringCharAt
          (stringTy `PrimitiveTypeArrow` (intTy `PrimitiveTypeArrow` charTy))
          Set.empty
      ),
      ( charIsDigitPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsDigit
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiLowerPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiLower
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiUpperPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiUpper
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiAlphaPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiAlpha
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiAlphaNumPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiAlphaNum
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiIdentifierStartPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiIdentifierStart
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiIdentifierContinuePrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiIdentifierContinue
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiWhitespacePrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiWhitespace
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiPunctuationPrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiPunctuation
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      ( charIsAsciiPrintablePrimitiveName,
        primitiveValueSpec
          PrimitiveNativeCharIsAsciiPrintable
          (charTy `PrimitiveTypeArrow` boolTy)
          Set.empty
      ),
      nativeIOSpec
        PrimitiveIOPure
        ( PrimitiveTypeForall
            "a"
            (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "a"))
        )
        Set.empty,
      nativeIOSpec
        PrimitiveIOBind
        ( PrimitiveTypeForall
            "a"
            ( PrimitiveTypeForall
                "b"
                ( ioOf (PrimitiveTypeVar "a")
                    `PrimitiveTypeArrow` ( (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "b"))
                                             `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "b")
                                         )
                )
            )
        )
        (Set.singleton 1),
      nativeIOSpec
        PrimitiveIOMap
        ( PrimitiveTypeForall
            "a"
            ( PrimitiveTypeForall
                "b"
                ( (PrimitiveTypeVar "a" `PrimitiveTypeArrow` PrimitiveTypeVar "b")
                    `PrimitiveTypeArrow` (ioOf (PrimitiveTypeVar "a") `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "b"))
                )
            )
        )
        (Set.singleton 0),
      ( "__io_ap",
        primitiveValueSpec
          PrimitiveNativeUnsupported
          ( PrimitiveTypeForall
              "a"
              ( PrimitiveTypeForall
                  "b"
                  ( ioOf (PrimitiveTypeVar "a" `PrimitiveTypeArrow` PrimitiveTypeVar "b")
                      `PrimitiveTypeArrow` (ioOf (PrimitiveTypeVar "a") `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "b"))
                  )
              )
          )
          Set.empty
      ),
      nativeIOSpec PrimitiveIOPutStrLn (stringTy `PrimitiveTypeArrow` ioOf unitTy) Set.empty,
      nativeIOSpec PrimitiveIOGetLine (ioOf stringTy) Set.empty,
      nativeIOSpec PrimitiveIOPutStr (stringTy `PrimitiveTypeArrow` ioOf unitTy) Set.empty,
      nativeIOSpec PrimitiveIOReadFile (stringTy `PrimitiveTypeArrow` ioOf stringTy) Set.empty,
      nativeIOSpec
        PrimitiveIOWriteFile
        (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` ioOf unitTy))
        Set.empty,
      nativeIOSpec
        PrimitiveIOAppendFile
        (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` ioOf unitTy))
        Set.empty,
      nativeIOSpec PrimitiveIOExitWith (intTy `PrimitiveTypeArrow` ioOf unitTy) Set.empty,
      nativeIOSpec
        PrimitiveIONewIORef
        ( PrimitiveTypeForall
            "a"
            (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf (ioRefOf (PrimitiveTypeVar "a")))
        )
        Set.empty,
      nativeIOSpec
        PrimitiveIOReadIORef
        ( PrimitiveTypeForall
            "a"
            (ioRefOf (PrimitiveTypeVar "a") `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "a"))
        )
        Set.empty,
      nativeIOSpec
        PrimitiveIOWriteIORef
        ( PrimitiveTypeForall
            "a"
            ( ioRefOf (PrimitiveTypeVar "a")
                `PrimitiveTypeArrow` (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf unitTy)
            )
        )
        Set.empty,
      nativeIOSpec
        PrimitiveIOGetArgs
        (ioOf (listOf stringTy))
        Set.empty
    ]
  where
    nativeIOSpec operation ty closureValueArguments =
      ( nativeIOPrimitiveName operation,
        primitiveValueSpec (PrimitiveNativeIO operation) ty closureValueArguments
      )

    primitiveValueSpec nativeSupport ty closureValueArguments =
      PrimitiveValueSpec
        { primitiveValueType = ty,
          primitiveValueClosureValueArguments = closureValueArguments,
          primitiveValueNativeSupport = nativeSupport
        }
    boolTy = PrimitiveTypeBase "Bool"
    charTy = PrimitiveTypeBase "Char"
    intTy = PrimitiveTypeBase "Int"
    stringTy = PrimitiveTypeBase "String"
    unitTy = PrimitiveTypeBase "Unit"
    ioOf ty = PrimitiveTypeCon "IO" (ty :| [])
    ioRefOf ty = PrimitiveTypeCon "IORef" (ty :| [])
    listOf ty = PrimitiveTypeCon "List" (ty :| [])

primitiveValueNames :: Set String
primitiveValueNames = Map.keysSet primitiveValueSpecs

primitiveNativeSupport :: String -> Maybe PrimitiveNativeSupport
primitiveNativeSupport name =
  primitiveValueNativeSupport <$> Map.lookup name primitiveValueSpecs

nativeAndPrimitiveName :: String
nativeAndPrimitiveName =
  requireSinglePrimitiveNativeSupport PrimitiveNativeBooleanAnd

nativeIOPrimitiveName :: PrimitiveIOOperation -> String
nativeIOPrimitiveName =
  \case
    PrimitiveIOPure -> "__io_pure"
    PrimitiveIOBind -> "__io_bind"
    PrimitiveIOMap -> "__io_map"
    PrimitiveIOPutStrLn -> "__io_putStrLn"
    PrimitiveIOGetLine -> "__io_getLine"
    PrimitiveIOPutStr -> "__io_putStr"
    PrimitiveIOReadFile -> "__io_readFile"
    PrimitiveIOWriteFile -> "__io_writeFile"
    PrimitiveIOAppendFile -> "__io_appendFile"
    PrimitiveIOExitWith -> "__io_exitWith"
    PrimitiveIONewIORef -> "__io_newIORef"
    PrimitiveIOReadIORef -> "__io_readIORef"
    PrimitiveIOWriteIORef -> "__io_writeIORef"
    PrimitiveIOGetArgs -> "__io_getArgs"

nativeIOPrimitiveNames :: Set String
nativeIOPrimitiveNames =
  Map.keysSet (Map.filter (isPrimitiveNativeIO . primitiveValueNativeSupport) primitiveValueSpecs)

nativeLowerablePrimitiveNames :: Set String
nativeLowerablePrimitiveNames =
  Map.keysSet (Map.filter (isPrimitiveNativeLowerable . primitiveValueNativeSupport) primitiveValueSpecs)

andPrimitiveName :: String
andPrimitiveName =
  "__mlfp_and"

stringLengthPrimitiveName :: String
stringLengthPrimitiveName =
  "__string_length"

stringIsEmptyPrimitiveName :: String
stringIsEmptyPrimitiveName =
  "__string_is_empty"

stringContainsCharPrimitiveName :: String
stringContainsCharPrimitiveName =
  "__string_contains_char"

stringContainsPrimitiveName :: String
stringContainsPrimitiveName =
  "__string_contains"

stringStartsWithPrimitiveName :: String
stringStartsWithPrimitiveName =
  "__string_starts_with"

stringEndsWithPrimitiveName :: String
stringEndsWithPrimitiveName =
  "__string_ends_with"

stringAppendPrimitiveName :: String
stringAppendPrimitiveName =
  "__string_append"

stringDropPrimitiveName :: String
stringDropPrimitiveName =
  "__string_drop"

stringTakePrimitiveName :: String
stringTakePrimitiveName =
  "__string_take"

stringSlicePrimitiveName :: String
stringSlicePrimitiveName =
  "__string_slice"

stringCharAtPrimitiveName :: String
stringCharAtPrimitiveName =
  "__string_char_at"

charIsDigitPrimitiveName :: String
charIsDigitPrimitiveName =
  "__char_is_digit"

charIsAsciiLowerPrimitiveName :: String
charIsAsciiLowerPrimitiveName =
  "__char_is_ascii_lower"

charIsAsciiUpperPrimitiveName :: String
charIsAsciiUpperPrimitiveName =
  "__char_is_ascii_upper"

charIsAsciiAlphaPrimitiveName :: String
charIsAsciiAlphaPrimitiveName =
  "__char_is_ascii_alpha"

charIsAsciiAlphaNumPrimitiveName :: String
charIsAsciiAlphaNumPrimitiveName =
  "__char_is_ascii_alpha_num"

charIsAsciiIdentifierStartPrimitiveName :: String
charIsAsciiIdentifierStartPrimitiveName =
  "__char_is_ascii_identifier_start"

charIsAsciiIdentifierContinuePrimitiveName :: String
charIsAsciiIdentifierContinuePrimitiveName =
  "__char_is_ascii_identifier_continue"

charIsAsciiWhitespacePrimitiveName :: String
charIsAsciiWhitespacePrimitiveName =
  "__char_is_ascii_whitespace"

charIsAsciiPunctuationPrimitiveName :: String
charIsAsciiPunctuationPrimitiveName =
  "__char_is_ascii_punctuation"

charIsAsciiPrintablePrimitiveName :: String
charIsAsciiPrintablePrimitiveName =
  "__char_is_ascii_printable"

requireSinglePrimitiveNativeSupport :: PrimitiveNativeSupport -> String
requireSinglePrimitiveNativeSupport nativeSupport =
  case Set.toList (Map.keysSet (Map.filter ((== nativeSupport) . primitiveValueNativeSupport) primitiveValueSpecs)) of
    [name] -> name
    names ->
      error
        ( "primitive inventory expected exactly one "
            ++ show nativeSupport
            ++ " primitive, found "
            ++ show names
        )

isPrimitiveNativeIO :: PrimitiveNativeSupport -> Bool
isPrimitiveNativeIO =
  \case
    PrimitiveNativeIO {} -> True
    _ -> False

isPrimitiveNativeLowerable :: PrimitiveNativeSupport -> Bool
isPrimitiveNativeLowerable =
  \case
    PrimitiveNativeUnsupported -> False
    PrimitiveNativeBooleanAnd -> True
    PrimitiveNativeStringLength -> True
    PrimitiveNativeStringIsEmpty -> True
    PrimitiveNativeStringContainsChar -> True
    PrimitiveNativeStringContains -> True
    PrimitiveNativeStringStartsWith -> True
    PrimitiveNativeStringEndsWith -> True
    PrimitiveNativeStringAppend -> True
    PrimitiveNativeStringDrop -> True
    PrimitiveNativeStringTake -> True
    PrimitiveNativeStringSlice -> True
    PrimitiveNativeStringCharAt -> True
    PrimitiveNativeCharIsDigit -> True
    PrimitiveNativeCharIsAsciiLower -> True
    PrimitiveNativeCharIsAsciiUpper -> True
    PrimitiveNativeCharIsAsciiAlpha -> True
    PrimitiveNativeCharIsAsciiAlphaNum -> True
    PrimitiveNativeCharIsAsciiIdentifierStart -> True
    PrimitiveNativeCharIsAsciiIdentifierContinue -> True
    PrimitiveNativeCharIsAsciiWhitespace -> True
    PrimitiveNativeCharIsAsciiPunctuation -> True
    PrimitiveNativeCharIsAsciiPrintable -> True
    PrimitiveNativeIO {} -> True

primitiveTypeToSourceType :: PrimitiveType -> SrcType
primitiveTypeToSourceType =
  \case
    PrimitiveTypeVar name -> STVar name
    PrimitiveTypeArrow dom cod ->
      STArrow (primitiveTypeToSourceType dom) (primitiveTypeToSourceType cod)
    PrimitiveTypeBase name -> STBase name
    PrimitiveTypeCon name args ->
      STCon name (fmap primitiveTypeToSourceType args)
    PrimitiveTypeForall name body ->
      STForall name Nothing (primitiveTypeToSourceType body)

primitiveTypeToElabType :: PrimitiveType -> ElabType
primitiveTypeToElabType =
  \case
    PrimitiveTypeVar name -> TVar name
    PrimitiveTypeArrow dom cod ->
      TArrow (primitiveTypeToElabType dom) (primitiveTypeToElabType cod)
    PrimitiveTypeBase name -> TBase (BaseTy name)
    PrimitiveTypeCon name args ->
      TCon (BaseTy name) (fmap primitiveTypeToElabType args)
    PrimitiveTypeForall name body ->
      TForall name Nothing (primitiveTypeToElabType body)

canonicalizeBuiltinSourceType :: SrcType -> SrcType
canonicalizeBuiltinSourceType =
  \case
    asIs@STVar {} -> asIs
    STBase name
      | isBuiltinTypeName name -> STBase (qualifyBuiltinTypeName name)
      | otherwise -> STBase name
    STCon name args
      | isBuiltinTypeName name ->
          STCon (qualifyBuiltinTypeName name) (fmap canonicalizeBuiltinSourceType args)
      | otherwise ->
          STCon name (fmap canonicalizeBuiltinSourceType args)
    STVarApp name args ->
      STVarApp name (fmap canonicalizeBuiltinSourceType args)
    STTyLam name body ->
      STTyLam name (canonicalizeBuiltinSourceType body)
    STTyApp fun arg ->
      STTyApp (canonicalizeBuiltinSourceType fun) (canonicalizeBuiltinSourceType arg)
    STArrow dom cod ->
      STArrow (canonicalizeBuiltinSourceType dom) (canonicalizeBuiltinSourceType cod)
    STForall name mb body ->
      STForall
        name
        (fmap (SrcBound . canonicalizeBuiltinSourceType . unSrcBound) mb)
        (canonicalizeBuiltinSourceType body)
    STMu name body ->
      STMu name (canonicalizeBuiltinSourceType body)
    STBottom ->
      STBottom

sourceTypeMentionsOpaqueBuiltin :: SrcType -> Bool
sourceTypeMentionsOpaqueBuiltin =
  \case
    STVar {} -> False
    STBase name -> isOpaqueBuiltinTypeReference name
    STCon name args ->
      isOpaqueBuiltinTypeReference name || any sourceTypeMentionsOpaqueBuiltin args
    STVarApp _ args ->
      any sourceTypeMentionsOpaqueBuiltin args
    STTyLam _ body ->
      sourceTypeMentionsOpaqueBuiltin body
    STTyApp fun arg ->
      sourceTypeMentionsOpaqueBuiltin fun || sourceTypeMentionsOpaqueBuiltin arg
    STArrow dom cod ->
      sourceTypeMentionsOpaqueBuiltin dom || sourceTypeMentionsOpaqueBuiltin cod
    STForall _ mb body ->
      maybe False (sourceTypeMentionsOpaqueBuiltin . unSrcBound) mb
        || sourceTypeMentionsOpaqueBuiltin body
    STMu _ body ->
      sourceTypeMentionsOpaqueBuiltin body
    STBottom ->
      False
  where
    isOpaqueBuiltinTypeReference name =
      isOpaqueBuiltinTypeName name
        || case stripPrefix (builtinModuleName ++ ".") name of
          Just builtinName -> isOpaqueBuiltinTypeName builtinName
          Nothing -> False
