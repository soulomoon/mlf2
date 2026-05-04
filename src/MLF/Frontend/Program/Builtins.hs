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

import Data.List.NonEmpty (NonEmpty (..))
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
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType, firstOrderTypeParam)
import qualified MLF.Frontend.Syntax.Program as P

builtinModuleName :: String
builtinModuleName = "<builtin>"

builtinTypeKinds :: Map String P.SrcKind
builtinTypeKinds =
  Map.fromList
    [ ("Int", P.KType),
      ("Bool", P.KType),
      ("String", P.KType),
      ("IO", P.KArrow P.KType P.KType),
      ("IORef", P.KArrow P.KType P.KType)
    ]

builtinTypeNames :: Set String
builtinTypeNames = Map.keysSet builtinTypeKinds

builtinOpaqueTypeNames :: Set String
builtinOpaqueTypeNames = Set.fromList ["IO", "IORef"]

isBuiltinTypeName :: String -> Bool
isBuiltinTypeName = (`Map.member` builtinTypeKinds)

builtinTypeKind :: String -> Maybe P.SrcKind
builtinTypeKind = (`Map.lookup` builtinTypeKinds)

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
  Map.fromList
    [ builtinOrdinary "__mlfp_and" (bool `STArrow` (bool `STArrow` bool)),
      builtinOrdinary "__io_pure" (STForall "a" Nothing (STVar "a" `STArrow` ioOf (STVar "a"))),
      builtinOrdinary "__io_bind" (STForall "a" Nothing (STForall "b" Nothing ioBindType)),
      builtinOrdinary "__io_putStrLn" (string `STArrow` ioOf unit),
      builtinOrdinary "__io_getLine" (ioOf string),
      builtinOrdinary "__io_putStr" (string `STArrow` ioOf unit),
      builtinOrdinary "__io_readFile" (string `STArrow` ioOf string),
      builtinOrdinary "__io_writeFile" (string `STArrow` (string `STArrow` ioOf unit)),
      builtinOrdinary "__io_appendFile" (string `STArrow` (string `STArrow` ioOf unit)),
      builtinOrdinary "__io_exitWith" (STBase "Int" `STArrow` ioOf unit),
      builtinOrdinary "__io_newIORef" (STForall "a" Nothing (STVar "a" `STArrow` ioOf (STCon "IORef" (STVar "a" :| [])))),
      builtinOrdinary "__io_readIORef" (STForall "a" Nothing (STCon "IORef" (STVar "a" :| []) `STArrow` ioOf (STVar "a"))),
      builtinOrdinary "__io_writeIORef" (STForall "a" Nothing (STCon "IORef" (STVar "a" :| []) `STArrow` (STVar "a" `STArrow` ioOf unit))),
      builtinOrdinary "__io_getArgs" (ioOf (STCon "List" (STBase "String" :| [])))
    ]
  where
    bool = STBase "Bool"
    string = STBase "String"
    unit = STBase "Unit"
    ioOf ty = STCon "IO" (ty :| [])
    ioBindType =
      ioOf (STVar "a")
        `STArrow` ((STVar "a" `STArrow` ioOf (STVar "b")) `STArrow` ioOf (STVar "b"))

builtinOrdinary :: String -> SrcType -> (String, ValueInfo)
builtinOrdinary name ty =
  ( name,
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
  )

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
    [ ("IO",
        DataInfo
          { dataName = "IO",
            dataInfoSymbol = builtinIdentity SymbolType "IO",
            dataModule = builtinModuleName,
            dataTypeParams = [firstOrderTypeParam "a"],
            dataParams = ["a"],
            dataConstructors = []
          })
    , ("IORef",
        DataInfo
          { dataName = "IORef",
            dataInfoSymbol = builtinIdentity SymbolType "IORef",
            dataModule = builtinModuleName,
            dataTypeParams = [firstOrderTypeParam "a"],
            dataParams = ["a"],
            dataConstructors = []
          })
    ]

isOpaqueBuiltinDataInfo :: DataInfo -> Bool
isOpaqueBuiltinDataInfo info =
  dataInfoSymbol info == builtinIdentity SymbolType "IO"
    || dataInfoSymbol info == builtinIdentity SymbolType "IORef"

srcTypeMentionsOpaqueBuiltin :: SrcType -> Bool
srcTypeMentionsOpaqueBuiltin = go
  where
    go ty =
      case ty of
        STVar {} -> False
        STBase name -> isOpaqueName name
        STCon name args -> isOpaqueName name || any go args
        STVarApp _ args -> any go args
        STArrow dom cod -> go dom || go cod
        STForall _ mb body -> maybe False (go . unSrcBound) mb || go body
        STMu _ body -> go body
        STBottom -> False

    isOpaqueName name =
      name `Set.member` builtinOpaqueTypeNames
        || name `Set.member` Set.map qualifyBuiltin builtinOpaqueTypeNames

    qualifyBuiltin name = builtinModuleName ++ "." ++ name

builtinIdentity :: SymbolNamespace -> String -> SymbolIdentity
builtinIdentity namespace name =
  SymbolIdentity
    { symbolNamespace = namespace,
      symbolDefiningModule = builtinModuleName,
      symbolDefiningName = name,
      symbolOwnerIdentity = Nothing
    }

canonicalBuiltinSrcType :: SrcType -> SrcType
canonicalBuiltinSrcType = go
  where
    go ty =
      case ty of
        STVar {} -> ty
        STBase name
          | isBuiltinTypeName name -> STBase (builtinModuleName ++ "." ++ name)
          | otherwise -> ty
        STCon name args
          | isBuiltinTypeName name -> STCon (builtinModuleName ++ "." ++ name) (fmap go args)
          | otherwise -> STCon name (fmap go args)
        STVarApp name args -> STVarApp name (fmap go args)
        STArrow dom cod -> STArrow (go dom) (go cod)
        STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu name body -> STMu name (go body)
        STBottom -> STBottom
