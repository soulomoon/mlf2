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
    builtinSourceTypeHeadIdentities,
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
import MLF.Elab.Types (Ty (..), TypeBinderRef, typeBinderRefIdentity, typeBinderRefName)
import MLF.Frontend.Program.Types
  ( DataInfo (..),
    CheckedTypeParam (..),
    ResolvedSymbol,
    SymbolIdentity,
    SymbolNamespace (..),
    SymbolOrigin (..),
    ValueInfo (..),
    mkResolvedSymbol,
    requireTypeViewFromSourceType,
    resolvedSymbolIdentity,
    valueInfoRuntimeName,
    typeViewDisplay,
  )
import MLF.Frontend.Symbol (symbolIdentityAliasMapWith)
import MLF.Frontend.Syntax (SrcBound (..), SrcKind (..), SrcTy (..), SrcType, resolvedTypeBinderRefFromIdentity)
import qualified MLF.Frontend.Syntax.Program as P
import qualified MLF.Primitive.Inventory as Inventory
import MLF.Types.Identity (TypeBinderIdentity, UniqueIdentity (..), typeBinderIdentityAliasMap, typeBinderIdentityFromUnique)

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
  Inventory.builtinTypeIdentity

builtinTypeHeadIdentity :: String -> Maybe SymbolIdentity
builtinTypeHeadIdentity =
  Inventory.builtinTypeHeadIdentity

builtinValueIdentity :: String -> SymbolIdentity
builtinValueIdentity =
  Inventory.builtinValueIdentity

builtinSymbol :: SymbolNamespace -> String -> ResolvedSymbol
builtinSymbol SymbolType name =
  mkResolvedSymbol
    (builtinTypeIdentity canonical)
    canonical
    canonical
    SymbolBuiltin
  where
    canonical = normalizeBuiltinTypeReference name
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
      valueTypeView =
        requireTypeViewFromSourceType
          (builtinSourceTypeHeadIdentities ty)
          (builtinValueTypeBinderIdentities name)
          ty,
      valueConstraintInfos = []
    }

builtinValueTypeBinderIdentities :: String -> Map String TypeBinderIdentity
builtinValueTypeBinderIdentities name =
  case Map.lookup name Inventory.primitiveValueElabTypes of
    Just ty ->
      typeBinderIdentityAliasMap
        [ (typeBinderRefName ref, typeBinderRefIdentity ref)
        | ref <- elabTypeBinderRefs ty
        ]
    Nothing -> Map.empty

elabTypeBinderRefs :: Ty v -> [TypeBinderRef]
elabTypeBinderRefs ty =
  case ty of
    TVarRef ref -> [ref]
    TArrow dom cod -> elabTypeBinderRefs dom ++ elabTypeBinderRefs cod
    TBaseWithIdentity {} -> []
    TConWithIdentity _ _ args -> foldMap elabTypeBinderRefs args
    TVarAppRef ref args -> ref : foldMap elabTypeBinderRefs args
    TForallRef ref mbBound body -> ref : maybe [] elabTypeBinderRefs mbBound ++ elabTypeBinderRefs body
    TMuRef ref body -> ref : elabTypeBinderRefs body
    TBottom -> []

builtinSourceTypeHeadIdentities :: SrcTy n v -> Map String SymbolIdentity
builtinSourceTypeHeadIdentities =
  \case
    STVar {} -> Map.empty
    STBase name -> headIdentity name
    STCon name args -> headIdentity name <> foldMap builtinSourceTypeHeadIdentities args
    STVarApp _ args -> foldMap builtinSourceTypeHeadIdentities args
    STTyLam _ body -> builtinSourceTypeHeadIdentities body
    STTyApp fun arg -> builtinSourceTypeHeadIdentities fun <> builtinSourceTypeHeadIdentities arg
    STArrow dom cod -> builtinSourceTypeHeadIdentities dom <> builtinSourceTypeHeadIdentities cod
    STForall _ mb body ->
      maybe Map.empty (builtinSourceTypeHeadIdentities . unSrcBound) mb
        <> builtinSourceTypeHeadIdentities body
    STMu _ body -> builtinSourceTypeHeadIdentities body
    STBottom -> Map.empty
  where
    headIdentity name =
      case Inventory.primitiveTypeHeadIdentity name of
        Just identity ->
          symbolIdentityAliasMapWith [(identity, [name, normalizeBuiltinTypeReference name])]
        Nothing -> Map.empty

builtinOpaqueValueNames :: Set String
builtinOpaqueValueNames =
  Set.fromList
    [ valueInfoRuntimeName valueInfo
      | valueInfo@OrdinaryValue {valueTypeView = tyView} <- Map.elems builtinValues,
        let ty = typeViewDisplay tyView,
        srcTypeMentionsOpaqueBuiltin ty
    ]

builtinOpaqueTypes :: Map String DataInfo
builtinOpaqueTypes =
  Map.fromList
    [ ( name,
        DataInfo
          { dataInfoSymbol = builtinIdentity SymbolType name,
            dataTypeParams = zipWith (builtinTypeParam name) [0 :: Int ..] params,
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
srcTypeMentionsOpaqueBuiltin =
  Inventory.sourceTypeMentionsOpaqueBuiltin

normalizeBuiltinTypeReference :: String -> String
normalizeBuiltinTypeReference =
  Inventory.normalizeBuiltinTypeReference

builtinIdentity :: SymbolNamespace -> String -> SymbolIdentity
builtinIdentity namespace =
  case namespace of
    SymbolType -> builtinTypeIdentity
    SymbolValue -> builtinValueIdentity
    _ -> error ("unsupported builtin identity namespace " ++ show namespace)

builtinTypeParam :: String -> Int -> String -> CheckedTypeParam
builtinTypeParam typeName index paramName =
  CheckedTypeParam
    (resolvedTypeBinderRefFromIdentity (typeBinderIdentityFromUnique (builtinTypeParamIdentity typeName index paramName)) paramName)
    KType

builtinTypeParamIdentity :: String -> Int -> String -> UniqueIdentity
builtinTypeParamIdentity typeName index paramName =
  Map.findWithDefault missing (typeName, index, paramName) builtinTypeParamUniqueIdentities
  where
    missing =
      error ("missing builtin type parameter identity for `" ++ typeName ++ "." ++ paramName ++ "`")

builtinTypeParamUniqueIdentities :: Map (String, Int, String) UniqueIdentity
builtinTypeParamUniqueIdentities =
  Map.fromList
    [ (("IO", 0, "a"), UniqueIdentity (-300000)),
      (("IORef", 0, "a"), UniqueIdentity (-300001))
    ]
