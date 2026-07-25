{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module BackendIRTestSupport
  ( fixtureLocalDetails,
    fixtureTopLevelVar,
    fixtureTypeBinderIdentity,
    fixtureSymbolIdentity,
    fixtureClosureEntryIdentity,
    pattern BackendProgram,
    backendProgramModules,
    backendProgramMain,
    pattern BackendModule,
    backendModuleName,
    backendModuleData,
    backendModuleBindings,
    pattern BackendData,
    backendDataName,
    backendDataParameters,
    backendDataConstructors,
    pattern BackendConstructor,
    backendConstructorName,
    backendConstructorForalls,
    backendConstructorFields,
    backendConstructorResult,
    pattern BackendBinding,
    backendBindingName,
    backendBindingType,
    backendBindingExpr,
    backendBindingExportedAsMain,
    pattern BackendTypeBinder,
    pattern BTVar,
    pattern BTBase,
    pattern BTCon,
    pattern BTVarApp,
    pattern BTForall,
    pattern BTMu,
    pattern BackendVar,
    pattern BackendLam,
    pattern BackendLet,
    pattern BackendTyAbs,
    pattern BackendClosure,
    backendClosureParams,
    pattern BackendConstruct,
    pattern BackendConstructorPattern,
  )
where

import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import MLF.Backend.IR
  ( BackendBinding,
    BackendClosureCapture (..),
    BackendClosureParam (..),
    BackendConstructor,
    BackendData,
    BackendExpr (..),
    BackendModule,
    BackendPattern (..),
    BackendPatternBinder (..),
    BackendProgram,
    BackendType,
    BackendTypeBinder,
    backendDataParameterRefFromIdentity,
  )
import qualified MLF.Backend.IR as IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity)
import MLF.Frontend.Symbol
  ( SymbolIdentity,
    SymbolNamespace (..),
    symbolIdentityFromParts,
  )
import MLF.Types.Identity
  ( IdDetails (..),
    LocalIdentity (..),
    TypeBinderIdentity,
    UniqueIdentity (..),
    localRefFromIdentity,
    typeBinderIdentityFromUnique,
  )

-- These constructors are deliberately test-only. They preserve the compact
-- fixture notation while constructing the identity-complete production IR.
-- Explicit identity tests continue to use the production constructors.

fixtureUnique :: Int -> String -> UniqueIdentity
fixtureUnique role name =
  UniqueIdentity (negate (100000000 + role * 1000003 + hashName name))
  where
    hashName =
      foldl' (\acc char -> (acc * 167 + ord char) `mod` 1000003) 0

fixtureSymbolIdentity :: SymbolNamespace -> String -> SymbolIdentity
fixtureSymbolIdentity namespace name =
  symbolIdentityFromParts
    (fixtureUnique (10 + namespaceRole namespace) name)
    namespace
    "$backend_fixture"
    name
    Nothing
  where
    namespaceRole namespace0 =
      case namespace0 of
        SymbolModule -> 0
        SymbolType -> 1
        SymbolConstructor -> 2
        SymbolClass -> 3
        SymbolMethod -> 4
        SymbolValue -> 5

fixtureTypeBinderIdentity :: String -> TypeBinderIdentity
fixtureTypeBinderIdentity =
  typeBinderIdentityFromUnique . fixtureUnique 2

fixtureLocalDetails :: String -> IdDetails
fixtureLocalDetails name =
  LocalId (localRefFromIdentity (GeneratedLocalId (fixtureUnique 1 name)) name)

fixtureTopLevelVar :: BackendType -> String -> BackendExpr
fixtureTopLevelVar resultTy name =
  IR.BackendVarWithIdentity
    resultTy
    (TopLevelId (fixtureSymbolIdentity SymbolValue name))
    name

fixtureClosureEntryIdentity :: String -> UniqueIdentity
fixtureClosureEntryIdentity =
  fixtureUnique 3

pattern BackendProgram :: [BackendModule] -> String -> BackendProgram
pattern BackendProgram
  { backendProgramModules,
    backendProgramMain
  } <-
  IR.BackendProgramWithIdentity backendProgramModules _ backendProgramMain
  where
    BackendProgram modules mainName =
      IR.BackendProgramWithIdentity modules (fixtureSymbolIdentity SymbolValue mainName) mainName

pattern BackendModule :: String -> [BackendData] -> [BackendBinding] -> BackendModule
pattern BackendModule
  { backendModuleName,
    backendModuleData,
    backendModuleBindings
  } <-
  IR.BackendModuleWithIdentity _ backendModuleName backendModuleData backendModuleBindings
  where
    BackendModule name dataDecls bindings =
      IR.BackendModuleWithIdentity (fixtureSymbolIdentity SymbolModule name) name dataDecls bindings

pattern BackendData :: String -> [String] -> [BackendConstructor] -> BackendData
pattern BackendData
  { backendDataName,
    backendDataParameters,
    backendDataConstructors
  } <-
  IR.BackendDataWithIdentity
    _
    backendDataName
    (map IR.backendDataParameterRefName -> backendDataParameters)
    backendDataConstructors
  where
    BackendData name parameters constructors =
      IR.BackendDataWithIdentity
        (fixtureSymbolIdentity SymbolType name)
        name
        [ backendDataParameterRefFromIdentity (fixtureTypeBinderIdentity parameter) parameter
        | parameter <- parameters
        ]
        constructors

pattern BackendConstructor :: String -> [BackendTypeBinder] -> [BackendType] -> BackendType -> BackendConstructor
pattern BackendConstructor
  { backendConstructorName,
    backendConstructorForalls,
    backendConstructorFields,
    backendConstructorResult
  } <-
  IR.BackendConstructorWithIdentity
    _
    backendConstructorName
    backendConstructorForalls
    backendConstructorFields
    backendConstructorResult
  where
    BackendConstructor name foralls fields result =
      IR.BackendConstructorWithIdentity
        (fixtureSymbolIdentity SymbolConstructor name)
        name
        foralls
        fields
        result

pattern BackendBinding :: String -> BackendType -> BackendExpr -> Bool -> BackendBinding
pattern BackendBinding
  { backendBindingName,
    backendBindingType,
    backendBindingExpr,
    backendBindingExportedAsMain
  } <-
  IR.BackendBindingWithMetadata
    _
    backendBindingName
    backendBindingType
    backendBindingExpr
    backendBindingExportedAsMain
    _
  where
    BackendBinding name ty expr exported =
      IR.BackendBindingWithMetadata
        (fixtureSymbolIdentity SymbolValue name)
        name
        ty
        expr
        exported
        Set.empty

pattern BackendTypeBinder :: String -> Maybe BackendType -> BackendTypeBinder
pattern BackendTypeBinder name bound <-
  IR.BackendTypeBinderWithIdentity _ name bound
  where
    BackendTypeBinder name bound =
      IR.BackendTypeBinderWithIdentity (fixtureTypeBinderIdentity name) name bound

pattern BTVar :: String -> BackendType
pattern BTVar name <- IR.BTVarWithIdentity _ name
  where
    BTVar name = IR.BTVarWithIdentity (fixtureTypeBinderIdentity name) name

pattern BTBase :: BaseTy -> BackendType
pattern BTBase base <- IR.BTBaseWithIdentity _ base
  where
    BTBase base@(BaseTy name) =
      IR.BTBaseWithIdentity
        (fromMaybe (fixtureSymbolIdentity SymbolType name) (builtinTypeHeadIdentity name))
        base

pattern BTCon :: BaseTy -> NonEmpty BackendType -> BackendType
pattern BTCon base args <- IR.BTConWithIdentity _ base args
  where
    BTCon base@(BaseTy name) args =
      IR.BTConWithIdentity
        (fromMaybe (fixtureSymbolIdentity SymbolType name) (builtinTypeHeadIdentity name))
        base
        args

pattern BTVarApp :: String -> NonEmpty BackendType -> BackendType
pattern BTVarApp name args <- IR.BTVarAppWithIdentity _ name args
  where
    BTVarApp name args =
      IR.BTVarAppWithIdentity (fixtureTypeBinderIdentity name) name args

pattern BTForall :: String -> Maybe BackendType -> BackendType -> BackendType
pattern BTForall name bound body <- IR.BTForallWithIdentity _ name bound body
  where
    BTForall name bound body =
      IR.BTForallWithIdentity (fixtureTypeBinderIdentity name) name bound body

pattern BTMu :: String -> BackendType -> BackendType
pattern BTMu name body <- IR.BTMuWithIdentity _ name body
  where
    BTMu name body =
      IR.BTMuWithIdentity (fixtureTypeBinderIdentity name) name body

pattern BackendVar :: BackendType -> String -> BackendExpr
pattern BackendVar resultTy name <- IR.BackendVarWithIdentity resultTy _ name
  where
    BackendVar resultTy name =
      IR.BackendVarWithIdentity resultTy (fixtureLocalDetails name) name

pattern BackendLam :: BackendType -> String -> BackendType -> BackendExpr -> BackendExpr
pattern BackendLam resultTy name paramTy body <-
  IR.BackendLamWithIdentity resultTy _ name paramTy body
  where
    BackendLam resultTy name paramTy body =
      IR.BackendLamWithIdentity resultTy (fixtureLocalDetails name) name paramTy body

pattern BackendLet :: BackendType -> String -> BackendType -> BackendExpr -> BackendExpr -> BackendExpr
pattern BackendLet resultTy name bindingTy rhs body <-
  IR.BackendLetWithIdentity resultTy _ name bindingTy rhs body
  where
    BackendLet resultTy name bindingTy rhs body =
      IR.BackendLetWithIdentity resultTy (fixtureLocalDetails name) name bindingTy rhs body

pattern BackendTyAbs :: BackendType -> String -> Maybe BackendType -> BackendExpr -> BackendExpr
pattern BackendTyAbs resultTy name bound body <-
  IR.BackendTyAbsWithIdentity resultTy _ name bound body
  where
    BackendTyAbs resultTy name bound body =
      IR.BackendTyAbsWithIdentity resultTy (fixtureTypeBinderIdentity name) name bound body

pattern BackendClosure :: BackendType -> String -> [BackendClosureCapture] -> [(String, BackendType)] -> BackendExpr -> BackendExpr
pattern BackendClosure resultTy entryName captures params body <-
  IR.BackendClosureWithParamIdentities
    resultTy
    _
    entryName
    captures
    (map closureParamPair -> params)
    body
  where
    BackendClosure resultTy entryName captures params body =
      IR.BackendClosureWithParamIdentities
        resultTy
        (fixtureClosureEntryIdentity entryName)
        entryName
        captures
        [ BackendClosureParam (fixtureLocalDetails name) name ty
        | (name, ty) <- params
        ]
        body

backendClosureParams :: [(String, BackendType)] -> [BackendClosureParam]
backendClosureParams params =
  [ BackendClosureParam (fixtureLocalDetails name) name ty
  | (name, ty) <- params
  ]

pattern BackendConstruct :: BackendType -> String -> [BackendExpr] -> BackendExpr
pattern BackendConstruct resultTy name args <-
  IR.BackendConstructWithIdentity resultTy _ name args
  where
    BackendConstruct resultTy name args =
      IR.BackendConstructWithIdentity
        resultTy
        (fixtureSymbolIdentity SymbolConstructor name)
        name
        args

pattern BackendConstructorPattern :: String -> [String] -> BackendPattern
pattern BackendConstructorPattern name binders <-
  IR.BackendConstructorPatternWithBinderIdentities
    _
    name
    (map IR.backendPatternBinderName -> binders)
  where
    BackendConstructorPattern name binders =
      IR.BackendConstructorPatternWithBinderIdentities
        (fixtureSymbolIdentity SymbolConstructor name)
        name
        [ BackendPatternBinder (fixtureLocalDetails binder) binder
        | binder <- binders
        ]

closureParamPair :: BackendClosureParam -> (String, BackendType)
closureParamPair param =
  (IR.backendClosureParamName param, IR.backendClosureParamType param)
