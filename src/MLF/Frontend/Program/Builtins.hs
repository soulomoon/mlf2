{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Builtins
  ( builtinModuleName,
    builtinTypeNames,
    builtinTypeKind,
    builtinTypeIdentity,
    builtinTypeHeadIdentity,
    builtinTypeSymbol,
    builtinValueIdentity,
    builtinValueSymbol,
    builtinValues,
    builtinOpaqueValueNames,
    builtinOpaqueTypes,
    builtinOpaqueTypeNames,
    isBuiltinTypeName,
    isBuiltinTypeSymbol,
    isOpaqueBuiltinDataInfo,
    normalizeBuiltinTypeReference,
    srcTypeMentionsOpaqueBuiltin,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Frontend.Program.Types
  ( DataInfo (..),
    ResolvedSymbol,
    SymbolIdentity (..),
    SymbolNamespace (..),
    SymbolOrigin (..),
    ValueInfo (..),
    mkResolvedSymbol,
    resolvedSymbolIdentity,
  )
import MLF.Frontend.Symbol (symbolIdentityStableName)
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType, firstOrderTypeParam)
import qualified MLF.Frontend.Syntax.Program as P
import qualified MLF.Primitive.Inventory as Inventory
import MLF.Types.Unique (UniqueIdentity (..))

builtinModuleName :: String
builtinModuleName = Inventory.builtinModuleName

builtinTypeNames :: Set String
builtinTypeNames = Inventory.builtinTypeNames

builtinOpaqueTypeNames :: Set String
builtinOpaqueTypeNames = Inventory.builtinOpaqueTypeNames

isBuiltinTypeName :: String -> Bool
isBuiltinTypeName = Inventory.isBuiltinTypeName

builtinTypeKind :: String -> Maybe P.SrcKind
builtinTypeKind = Inventory.builtinTypeKind

builtinTypeSymbol :: String -> ResolvedSymbol
builtinTypeSymbol = builtinSymbol SymbolType

builtinValueSymbol :: String -> ResolvedSymbol
builtinValueSymbol = builtinSymbol SymbolValue

builtinTypeIdentity :: String -> SymbolIdentity
builtinTypeIdentity =
  builtinIdentity SymbolType

builtinTypeHeadIdentity :: String -> Maybe SymbolIdentity
builtinTypeHeadIdentity name
  | isBuiltinTypeName canonical = Just (builtinTypeIdentity canonical)
  | otherwise = Nothing
  where
    canonical = normalizeBuiltinTypeReference name

builtinValueIdentity :: String -> SymbolIdentity
builtinValueIdentity =
  builtinIdentity SymbolValue

builtinSymbol :: SymbolNamespace -> String -> ResolvedSymbol
builtinSymbol namespace name =
  mkResolvedSymbol
    (builtinIdentity namespace name)
    name
    name
    SymbolBuiltin

isBuiltinTypeSymbol :: ResolvedSymbol -> Bool
isBuiltinTypeSymbol symbol =
  resolvedSymbolIdentity symbol `Set.member` builtinTypeIdentities

builtinTypeIdentities :: Set SymbolIdentity
builtinTypeIdentities =
  Set.fromList (map builtinTypeIdentity (Set.toList builtinTypeNames))

builtinValues :: Map String ValueInfo
builtinValues =
  Map.mapWithKey
    (\name spec -> builtinOrdinary name (Inventory.primitiveTypeToSourceType (Inventory.primitiveValueType spec)))
    Inventory.primitiveValueSpecs

builtinOrdinary :: String -> SrcType -> ValueInfo
builtinOrdinary name ty =
  OrdinaryValue
    { valueInfoSymbol = builtinIdentity SymbolValue name,
      valueRuntimeName = name,
      valueType = ty,
      valueIdentityType = canonicalBuiltinSrcType ty,
      valueConstraints = [],
      valueConstraintInfos = []
    }

builtinOpaqueValueNames :: Set String
builtinOpaqueValueNames =
  Set.fromList
    [ runtimeName
      | OrdinaryValue {valueRuntimeName = runtimeName, valueType = ty} <- Map.elems builtinValues,
        srcTypeMentionsOpaqueBuiltin ty
    ]

builtinOpaqueTypes :: Map String DataInfo
builtinOpaqueTypes =
  Map.fromList
    [ ( name,
        DataInfo
          { dataInfoSymbol = builtinIdentity SymbolType name,
            dataTypeParams = fmap firstOrderTypeParam params,
            dataConstructors = []
          }
      )
      | (name, spec) <- Map.toList Inventory.builtinTypeSpecs,
        Inventory.builtinTypeSpecOpaque spec,
        let params = Inventory.builtinTypeSpecParameters spec
    ]

isOpaqueBuiltinDataInfo :: DataInfo -> Bool
isOpaqueBuiltinDataInfo info =
  dataInfoSymbol info `Set.member` builtinOpaqueTypeIdentities

builtinOpaqueTypeIdentities :: Set SymbolIdentity
builtinOpaqueTypeIdentities =
  Set.fromList (map builtinTypeIdentity (Set.toList builtinOpaqueTypeNames))

srcTypeMentionsOpaqueBuiltin :: SrcType -> Bool
srcTypeMentionsOpaqueBuiltin ty =
  Inventory.sourceTypeMentionsOpaqueBuiltin ty || mentionsOpaqueBuiltinIdentity ty
  where
    mentionsOpaqueBuiltinIdentity =
      \case
        STVar {} -> False
        STBase name -> isOpaqueBuiltinIdentityReference name
        STCon name args ->
          isOpaqueBuiltinIdentityReference name || any mentionsOpaqueBuiltinIdentity args
        STVarApp _ args ->
          any mentionsOpaqueBuiltinIdentity args
        STTyLam _ body ->
          mentionsOpaqueBuiltinIdentity body
        STTyApp fun arg ->
          mentionsOpaqueBuiltinIdentity fun || mentionsOpaqueBuiltinIdentity arg
        STArrow dom cod ->
          mentionsOpaqueBuiltinIdentity dom || mentionsOpaqueBuiltinIdentity cod
        STForall _ mb body ->
          maybe False (mentionsOpaqueBuiltinIdentity . unSrcBound) mb
            || mentionsOpaqueBuiltinIdentity body
        STMu _ body ->
          mentionsOpaqueBuiltinIdentity body
        STBottom ->
          False

    isOpaqueBuiltinIdentityReference name =
      name `Set.member` builtinOpaqueTypeIdentityNames

builtinOpaqueTypeIdentityNames :: Set String
builtinOpaqueTypeIdentityNames =
  Set.map (symbolIdentityStableName . builtinTypeIdentity) builtinOpaqueTypeNames

normalizeBuiltinTypeReference :: String -> String
normalizeBuiltinTypeReference name =
  Map.findWithDefault (Inventory.normalizeBuiltinTypeReference name) name builtinTypeNamesByIdentityName

builtinTypeNamesByIdentityName :: Map String String
builtinTypeNamesByIdentityName =
  Map.fromList
    [ (symbolIdentityStableName (builtinTypeIdentity name), name)
    | name <- Set.toList builtinTypeNames
    ]

builtinIdentity :: SymbolNamespace -> String -> SymbolIdentity
builtinIdentity namespace name =
  SymbolIdentity
    { symbolUniqueIdentity = builtinUniqueIdentity namespace name,
      symbolNamespace = namespace,
      symbolDefiningModule = builtinModuleName,
      symbolDefiningName = name,
      symbolOwnerIdentity = Nothing
    }

builtinUniqueIdentity :: SymbolNamespace -> String -> UniqueIdentity
builtinUniqueIdentity namespace name =
  case namespace of
    SymbolType -> lookupBuiltinIdentity builtinTypeUniqueIdentities
    SymbolValue -> lookupBuiltinIdentity builtinValueUniqueIdentities
    _ -> missingBuiltinIdentity
  where
    lookupBuiltinIdentity identities =
      Map.findWithDefault missingBuiltinIdentity name identities
    missingBuiltinIdentity =
      error ("missing builtin identity for " ++ show namespace ++ " `" ++ name ++ "`")

builtinTypeUniqueIdentities :: Map String UniqueIdentity
builtinTypeUniqueIdentities =
  stableNegativeIdentities 100000 (Set.toAscList builtinTypeNames)

builtinValueUniqueIdentities :: Map String UniqueIdentity
builtinValueUniqueIdentities =
  stableNegativeIdentities 200000 (Set.toAscList Inventory.primitiveValueNames)

stableNegativeIdentities :: Int -> [String] -> Map String UniqueIdentity
stableNegativeIdentities offset names =
  Map.fromList (zip names (map (UniqueIdentity . negate) [offset ..]))

canonicalBuiltinSrcType :: SrcType -> SrcType
canonicalBuiltinSrcType = Inventory.canonicalizeBuiltinSourceType
