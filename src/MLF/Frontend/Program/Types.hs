{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module MLF.Frontend.Program.Types
  ( ProgramError (..),
    ConstructorInfo (..),
    DataInfo (..),
    MethodInfo (..),
    ClassInfo (..),
    ValueInfo (..),
    InstanceInfo (..),
    DeferredBindingMode (..),
    DeferredMethodCall (..),
    DeferredConstructorCall (..),
    DeferredCaseCall (..),
    DeferredProgramObligation (..),
    ExportedTypeInfo (..),
    ModuleExports (..),
    LoweredBinding (..),
    CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    splitForalls,
    splitArrows,
    substituteTypeVar,
    specializeMethodType,
  )
where

import Data.Map.Strict (Map)
import MLF.Elab.Types (ElabTerm, ElabType)
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType, SurfaceExpr)
import qualified MLF.Frontend.Syntax.Program as P

data ProgramError
  = ProgramDuplicateModule P.ModuleName
  | ProgramUnknownImportModule P.ModuleName
  | ProgramImportNotExported P.ModuleName String
  | ProgramImportCycle [P.ModuleName]
  | ProgramInvalidExport String
  | ProgramExportNotLocal String
  | ProgramDuplicateVisibleName String
  | ProgramDuplicateType String
  | ProgramDuplicateConstructor String
  | ProgramDuplicateClass String
  | ProgramDuplicateValue String
  | ProgramDuplicateMethod String
  | ProgramDuplicateInstance P.ClassName SrcType
  | ProgramUnknownValue String
  | ProgramUnknownConstructor String
  | ProgramUnknownType String
  | ProgramUnknownClass String
  | ProgramUnknownMethod String
  | ProgramInvalidConstructorResult P.ConstructorName SrcType P.TypeName
  | ProgramUnsupportedDeriving P.ClassName
  | ProgramDerivingRequiresNullaryType P.TypeName
  | ProgramMissingInstanceMethod P.ClassName P.MethodName
  | ProgramUnexpectedInstanceMethod P.ClassName P.MethodName
  | ProgramNoMatchingInstance P.ClassName SrcType
  | ProgramAmbiguousMethodUse P.MethodName
  | ProgramAmbiguousConstructorUse P.ConstructorName
  | ProgramExpectedFunction SrcType
  | ProgramTypeMismatch SrcType SrcType
  | ProgramCaseOnNonDataType SrcType
  | ProgramNonExhaustiveCase [P.ConstructorName]
  | ProgramDuplicateCaseBranch P.ConstructorName
  | ProgramPatternConstructorMismatch P.ConstructorName SrcType
  | ProgramPipelineError String
  | ProgramMainNotFound
  | ProgramMultipleMainDefinitions [String]
  deriving (Eq, Show)

data ConstructorInfo = ConstructorInfo
  { ctorName :: P.ConstructorName,
    ctorRuntimeName :: String,
    ctorType :: SrcType,
    ctorForalls :: [(String, Maybe SrcType)],
    ctorArgs :: [SrcType],
    ctorResult :: SrcType,
    ctorOwningType :: P.TypeName,
    ctorIndex :: Int
  }
  deriving (Eq, Show)

data DataInfo = DataInfo
  { dataName :: P.TypeName,
    dataModule :: P.ModuleName,
    dataParams :: [String],
    dataConstructors :: [ConstructorInfo]
  }
  deriving (Eq, Show)

data MethodInfo = MethodInfo
  { methodClassName :: P.ClassName,
    methodName :: P.MethodName,
    methodRuntimeBase :: String,
    methodType :: SrcType,
    methodParamName :: String
  }
  deriving (Eq, Show)

data ClassInfo = ClassInfo
  { className :: P.ClassName,
    classModule :: P.ModuleName,
    classParamName :: String,
    classMethods :: Map P.MethodName MethodInfo
  }
  deriving (Eq, Show)

data ValueInfo
  = OrdinaryValue
      { valueDisplayName :: String,
        valueRuntimeName :: String,
        valueType :: SrcType,
        valueOriginModule :: P.ModuleName
      }
  | ConstructorValue
      { valueDisplayName :: String,
        valueRuntimeName :: String,
        valueType :: SrcType,
        valueCtorInfo :: ConstructorInfo,
        valueOriginModule :: P.ModuleName
      }
  | OverloadedMethod
      { valueDisplayName :: String,
        valueMethodInfo :: MethodInfo,
        valueOriginModule :: P.ModuleName
      }
  deriving (Eq, Show)

data InstanceInfo = InstanceInfo
  { instanceClassName :: P.ClassName,
    instanceHeadType :: SrcType,
    instanceMethods :: Map P.MethodName ValueInfo
  }
  deriving (Eq, Show)

data DeferredBindingMode
  = DeferredBindingScheme
  | DeferredBindingMonomorphic
  deriving (Eq, Show)

data DeferredMethodCall = DeferredMethodCall
  { deferredMethodPlaceholder :: String,
    deferredMethodInfo :: MethodInfo,
    deferredMethodArgCount :: Int,
    deferredMethodFullArity :: Int,
    deferredMethodName :: P.MethodName
  }
  deriving (Eq, Show)

data DeferredConstructorCall = DeferredConstructorCall
  { deferredConstructorPlaceholder :: String,
    deferredConstructorInfo :: ConstructorInfo,
    deferredConstructorArgCount :: Int,
    deferredConstructorSourceType :: SrcType,
    deferredConstructorOccurrenceType :: SrcType,
    deferredConstructorInstBinders :: [String],
    deferredConstructorInitialSubst :: Map String SrcType,
    deferredConstructorBindingMode :: DeferredBindingMode
  }
  deriving (Eq, Show)

data DeferredCaseCall = DeferredCaseCall
  { deferredCasePlaceholder :: String,
    deferredCaseDataInfo :: DataInfo,
    deferredCaseResultType :: SrcType,
    deferredCaseHandlerNames :: [String],
    deferredCaseExpectedArgCount :: Int
  }
  deriving (Eq, Show)

data DeferredProgramObligation
  = DeferredMethod DeferredMethodCall
  | DeferredConstructor DeferredConstructorCall
  | DeferredCase DeferredCaseCall
  deriving (Eq, Show)

data ExportedTypeInfo = ExportedTypeInfo
  { exportedTypeData :: DataInfo,
    exportedTypeConstructors :: Map String ConstructorInfo
  }
  deriving (Eq, Show)

data ModuleExports = ModuleExports
  { exportedValues :: Map String ValueInfo,
    exportedTypes :: Map String ExportedTypeInfo,
    exportedClasses :: Map String ClassInfo
  }
  deriving (Eq, Show)

data LoweredBinding = LoweredBinding
  { loweredBindingName :: String,
    loweredBindingExpectedType :: SrcType,
    loweredBindingSurfaceExpr :: SurfaceExpr,
    loweredBindingDeferredObligations :: Map String DeferredProgramObligation,
    loweredBindingExternalTypes :: Map String SrcType,
    loweredBindingExportedAsMain :: Bool
  }
  deriving (Eq, Show)

data CheckedBinding = CheckedBinding
  { checkedBindingName :: String,
    checkedBindingSourceType :: SrcType,
    checkedBindingSurfaceExpr :: SurfaceExpr,
    checkedBindingTerm :: ElabTerm,
    checkedBindingType :: ElabType,
    checkedBindingExportedAsMain :: Bool
  }
  deriving (Eq, Show)

data CheckedModule = CheckedModule
  { checkedModuleName :: P.ModuleName,
    checkedModuleBindings :: [CheckedBinding],
    checkedModuleData :: Map String DataInfo,
    checkedModuleClasses :: Map String ClassInfo,
    checkedModuleInstances :: [InstanceInfo],
    checkedModuleExports :: ModuleExports
  }
  deriving (Eq, Show)

data CheckedProgram = CheckedProgram
  { checkedProgramModules :: [CheckedModule],
    checkedProgramMain :: String
  }
  deriving (Eq, Show)

splitForalls :: SrcType -> ([(String, Maybe SrcType)], SrcType)
splitForalls = go []
  where
    go acc = \case
      STForall name mb body -> go (acc ++ [(name, fmap unSrcBound mb)]) body
      ty -> (acc, ty)

splitArrows :: SrcType -> ([SrcType], SrcType)
splitArrows = go []
  where
    go acc = \case
      STArrow dom cod -> go (acc ++ [dom]) cod
      ty -> (acc, ty)

substituteTypeVar :: String -> SrcType -> SrcType -> SrcType
substituteTypeVar needle replacement = go
  where
    go ty = case ty of
      STVar name
        | name == needle -> replacement
        | otherwise -> ty
      STArrow dom cod -> STArrow (go dom) (go cod)
      STBase _ -> ty
      STCon name args -> STCon name (fmap go args)
      STForall name mb body
        | name == needle -> STForall name mb body
        | otherwise -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
      STMu name body
        | name == needle -> STMu name body
        | otherwise -> STMu name (go body)
      STBottom -> STBottom

specializeMethodType :: SrcType -> String -> SrcType -> SrcType
specializeMethodType methodTy paramName headTy =
  let (foralls, body) = splitForalls methodTy
      rebuilt = foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) (substituteTypeVar paramName headTy body) foralls
   in rebuilt
