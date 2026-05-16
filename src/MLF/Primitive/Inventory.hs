{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Primitive.Inventory
  ( BuiltinTypeSpec (..),
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

data PrimitiveValueSpec = PrimitiveValueSpec
  { primitiveValueType :: PrimitiveType,
    primitiveValueClosureValueArguments :: Set Int
  }
  deriving (Eq, Show)

builtinModuleName :: String
builtinModuleName = "<builtin>"

builtinTypeSpecs :: Map String BuiltinTypeSpec
builtinTypeSpecs =
  Map.fromList
    [ ("Int", BuiltinTypeSpec P.KType [] False),
      ("Bool", BuiltinTypeSpec P.KType [] False),
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
    [ ("__mlfp_and", primitiveValueSpec (boolTy `PrimitiveTypeArrow` (boolTy `PrimitiveTypeArrow` boolTy)) Set.empty),
      ( "__io_pure",
        primitiveValueSpec
          (PrimitiveTypeForall "a" (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "a")))
          Set.empty
      ),
      ( "__io_bind",
        primitiveValueSpec
          ( PrimitiveTypeForall
              "a"
              ( PrimitiveTypeForall
                  "b"
                  (ioOf (PrimitiveTypeVar "a")
                    `PrimitiveTypeArrow` ((PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "b")) `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "b")))
              )
          )
          (Set.singleton 1)
      ),
      ("__io_putStrLn", primitiveValueSpec (stringTy `PrimitiveTypeArrow` ioOf unitTy) Set.empty),
      ("__io_getLine", primitiveValueSpec (ioOf stringTy) Set.empty),
      ("__io_putStr", primitiveValueSpec (stringTy `PrimitiveTypeArrow` ioOf unitTy) Set.empty),
      ("__io_readFile", primitiveValueSpec (stringTy `PrimitiveTypeArrow` ioOf stringTy) Set.empty),
      ("__io_writeFile", primitiveValueSpec (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` ioOf unitTy)) Set.empty),
      ("__io_appendFile", primitiveValueSpec (stringTy `PrimitiveTypeArrow` (stringTy `PrimitiveTypeArrow` ioOf unitTy)) Set.empty),
      ("__io_exitWith", primitiveValueSpec (intTy `PrimitiveTypeArrow` ioOf unitTy) Set.empty),
      ( "__io_newIORef",
        primitiveValueSpec
          ( PrimitiveTypeForall
              "a"
              (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf (PrimitiveTypeCon "IORef" (PrimitiveTypeVar "a" :| [])))
          )
          Set.empty
      ),
      ( "__io_readIORef",
        primitiveValueSpec
          ( PrimitiveTypeForall
              "a"
              (PrimitiveTypeCon "IORef" (PrimitiveTypeVar "a" :| []) `PrimitiveTypeArrow` ioOf (PrimitiveTypeVar "a"))
          )
          Set.empty
      ),
      ( "__io_writeIORef",
        primitiveValueSpec
          ( PrimitiveTypeForall
              "a"
              ( PrimitiveTypeCon "IORef" (PrimitiveTypeVar "a" :| [])
                  `PrimitiveTypeArrow` (PrimitiveTypeVar "a" `PrimitiveTypeArrow` ioOf unitTy)
              )
          )
          Set.empty
      ),
      ("__io_getArgs", primitiveValueSpec (ioOf (PrimitiveTypeCon "List" (stringTy :| []))) Set.empty)
    ]
  where
    primitiveValueSpec ty closureValueArguments =
      PrimitiveValueSpec
        { primitiveValueType = ty,
          primitiveValueClosureValueArguments = closureValueArguments
        }
    boolTy = PrimitiveTypeBase "Bool"
    intTy = PrimitiveTypeBase "Int"
    stringTy = PrimitiveTypeBase "String"
    unitTy = PrimitiveTypeBase "Unit"
    ioOf ty = PrimitiveTypeCon "IO" (ty :| [])

primitiveValueNames :: Set String
primitiveValueNames = Map.keysSet primitiveValueSpecs

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
