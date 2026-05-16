{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Builtins
  ( builtinModuleName,
    builtinTypeNames,
    builtinTypeKind,
    builtinTypeSymbol,
    builtinValueSymbol,
    builtinValues,
    builtinOpaqueValueNames,
    builtinOpaqueTypes,
    builtinOpaqueTypeNames,
    isBuiltinTypeName,
    isBuiltinTypeSymbol,
    isOpaqueBuiltinDataInfo,
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
import MLF.Frontend.Syntax (SrcType, firstOrderTypeParam)
import qualified MLF.Frontend.Syntax.Program as P
import qualified MLF.Primitive.Inventory as Inventory

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

builtinSymbol :: SymbolNamespace -> String -> ResolvedSymbol
builtinSymbol namespace name =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = namespace,
          symbolDefiningModule = builtinModuleName,
          symbolDefiningName = name,
          symbolOwnerIdentity = Nothing
        }
    )
    name
    name
    SymbolBuiltin

isBuiltinTypeSymbol :: ResolvedSymbol -> Bool
isBuiltinTypeSymbol symbol =
  let identity = resolvedSymbolIdentity symbol
   in symbolNamespace identity == SymbolType
        && symbolDefiningModule identity == builtinModuleName

builtinValues :: Map String ValueInfo
builtinValues =
  Map.mapWithKey
    (\name spec -> builtinOrdinary name (Inventory.primitiveTypeToSourceType (Inventory.primitiveValueType spec)))
    Inventory.primitiveValueSpecs

builtinOrdinary :: String -> SrcType -> ValueInfo
builtinOrdinary name ty =
  OrdinaryValue
    { valueDisplayName = name,
      valueInfoSymbol = builtinIdentity SymbolValue name,
      valueRuntimeName = name,
      valueType = ty,
      valueIdentityType = canonicalBuiltinSrcType ty,
      valueConstraints = [],
      valueConstraintInfos = [],
      valueOriginModule = builtinModuleName
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
          { dataName = name,
            dataInfoSymbol = builtinIdentity SymbolType name,
            dataModule = builtinModuleName,
            dataTypeParams = fmap firstOrderTypeParam params,
            dataParams = params,
            dataConstructors = []
          }
      )
      | (name, spec) <- Map.toList Inventory.builtinTypeSpecs,
        Inventory.builtinTypeSpecOpaque spec,
        let params = Inventory.builtinTypeSpecParameters spec
    ]

isOpaqueBuiltinDataInfo :: DataInfo -> Bool
isOpaqueBuiltinDataInfo info =
  dataModule info == builtinModuleName
    && Inventory.isOpaqueBuiltinTypeName (dataName info)

srcTypeMentionsOpaqueBuiltin :: SrcType -> Bool
srcTypeMentionsOpaqueBuiltin = Inventory.sourceTypeMentionsOpaqueBuiltin

builtinIdentity :: SymbolNamespace -> String -> SymbolIdentity
builtinIdentity namespace name =
  SymbolIdentity
    { symbolNamespace = namespace,
      symbolDefiningModule = builtinModuleName,
      symbolDefiningName = name,
      symbolOwnerIdentity = Nothing
    }

canonicalBuiltinSrcType :: SrcType -> SrcType
canonicalBuiltinSrcType = Inventory.canonicalizeBuiltinSourceType
