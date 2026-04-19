{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module MLF.Frontend.Program.Types
  ( ProgramError (..),
    ProgramDiagnostic (..),
    diagnosticForProgramError,
    renderProgramDiagnostic,
    EvidenceInfo (..),
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
    constrainedVisibleType,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  | ProgramDuplicateImportAlias P.ModuleName
  | ProgramDuplicateInstance P.ClassName SrcType
  | ProgramOverlappingInstance P.ClassName SrcType SrcType
  | ProgramUnknownValue String
  | ProgramUnknownConstructor String
  | ProgramUnknownType String
  | ProgramUnknownClass String
  | ProgramUnknownMethod String
  | ProgramInvalidConstructorResult P.ConstructorName SrcType P.TypeName
  | ProgramUnsupportedDeriving P.ClassName
  | ProgramDerivingRequiresNullaryType P.TypeName
  | ProgramDerivingMissingFieldInstance P.ClassName SrcType
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

data ProgramDiagnostic = ProgramDiagnostic
  { diagnosticError :: ProgramError,
    diagnosticSpan :: Maybe P.SourceSpan,
    diagnosticMessage :: String,
    diagnosticHints :: [String]
  }
  deriving (Eq, Show)

diagnosticForProgramError :: Maybe P.LocatedProgram -> ProgramError -> ProgramDiagnostic
diagnosticForProgramError mbLocated err =
  ProgramDiagnostic
    { diagnosticError = err,
      diagnosticSpan = mbLocated >>= spanForError err . P.locatedProgramSpans,
      diagnosticMessage = programErrorMessage err,
      diagnosticHints = programErrorHints err
    }

renderProgramDiagnostic :: ProgramDiagnostic -> String
renderProgramDiagnostic diagnostic =
  unlines $
    header
      ++ ["error: " ++ diagnosticMessage diagnostic]
      ++ map ("hint: " ++) (diagnosticHints diagnostic)
  where
    header =
      case diagnosticSpan diagnostic of
        Just span0 -> [renderSourceSpan span0]
        Nothing -> []

renderSourceSpan :: P.SourceSpan -> String
renderSourceSpan span0 =
  P.sourceFile span0
    ++ ":"
    ++ show (P.sourceLine (P.sourceStart span0))
    ++ ":"
    ++ show (P.sourceColumn (P.sourceStart span0))

spanForError :: ProgramError -> P.ProgramSpanIndex -> Maybe P.SourceSpan
spanForError err index =
  case err of
    ProgramDuplicateModule name -> Map.lookup name (P.spanModules index)
    ProgramUnknownImportModule name -> Map.lookup name (P.spanModules index)
    ProgramImportNotExported _ name -> lookupAnyName name index
    ProgramInvalidExport name -> lookupAnyName name index
    ProgramExportNotLocal name -> lookupAnyName name index
    ProgramDuplicateVisibleName name -> lookupAnyName name index
    ProgramDuplicateType name -> firstSpan name (P.spanTypes index)
    ProgramDuplicateConstructor name -> firstSpan name (P.spanConstructors index)
    ProgramDuplicateClass name -> firstSpan name (P.spanClasses index)
    ProgramDuplicateValue name -> firstSpan name (P.spanValues index)
    ProgramDuplicateMethod name -> firstSpan name (P.spanValues index)
    ProgramDuplicateImportAlias name -> Map.lookup name (P.spanModules index)
    ProgramOverlappingInstance className0 _ _ -> firstSpan className0 (P.spanClasses index)
    ProgramUnknownValue name -> lookupAnyName name index
    ProgramUnknownConstructor name -> firstSpan name (P.spanConstructors index)
    ProgramUnknownType name -> firstSpan name (P.spanTypes index)
    ProgramUnknownClass name -> firstSpan name (P.spanClasses index)
    ProgramUnknownMethod name -> firstSpan name (P.spanValues index)
    ProgramInvalidConstructorResult ctor _ _ -> firstSpan ctor (P.spanConstructors index)
    ProgramUnsupportedDeriving className0 -> firstSpan className0 (P.spanClasses index)
    ProgramDerivingRequiresNullaryType typeName -> firstSpan typeName (P.spanTypes index)
    ProgramDerivingMissingFieldInstance className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramMissingInstanceMethod _ methodName0 -> firstSpan methodName0 (P.spanValues index)
    ProgramUnexpectedInstanceMethod _ methodName0 -> firstSpan methodName0 (P.spanValues index)
    ProgramNoMatchingInstance className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramAmbiguousMethodUse methodName0 -> firstSpan methodName0 (P.spanValues index)
    ProgramAmbiguousConstructorUse ctor -> firstSpan ctor (P.spanConstructors index)
    ProgramPatternConstructorMismatch ctor _ -> firstSpan ctor (P.spanConstructors index)
    ProgramNonExhaustiveCase (ctor : _) -> firstSpan ctor (P.spanConstructors index)
    ProgramDuplicateCaseBranch ctor -> firstSpan ctor (P.spanConstructors index)
    _ -> Nothing

firstSpan :: String -> Map String [P.SourceSpan] -> Maybe P.SourceSpan
firstSpan name spans = do
  matches <- Map.lookup name spans
  case matches of
    span0 : _ -> Just span0
    [] -> Nothing

lookupAnyName :: String -> P.ProgramSpanIndex -> Maybe P.SourceSpan
lookupAnyName name index =
  firstSpan name (P.spanValues index)
    <|> firstSpan name (P.spanConstructors index)
    <|> firstSpan name (P.spanTypes index)
    <|> firstSpan name (P.spanClasses index)
    <|> Map.lookup name (P.spanModules index)

programErrorMessage :: ProgramError -> String
programErrorMessage err =
  case err of
    ProgramDuplicateModule name -> "duplicate module `" ++ name ++ "`"
    ProgramUnknownImportModule name -> "unknown imported module `" ++ name ++ "`"
    ProgramImportNotExported moduleName name -> "module `" ++ moduleName ++ "` does not export `" ++ name ++ "`"
    ProgramImportCycle modules0 -> "module import cycle: " ++ show modules0
    ProgramInvalidExport name -> "invalid export `" ++ name ++ "`"
    ProgramExportNotLocal name -> "export is not local: `" ++ name ++ "`"
    ProgramDuplicateVisibleName name -> "duplicate visible name `" ++ name ++ "`"
    ProgramDuplicateType name -> "duplicate type `" ++ name ++ "`"
    ProgramDuplicateConstructor name -> "duplicate constructor `" ++ name ++ "`"
    ProgramDuplicateClass name -> "duplicate class `" ++ name ++ "`"
    ProgramDuplicateValue name -> "duplicate value `" ++ name ++ "`"
    ProgramDuplicateMethod name -> "duplicate method `" ++ name ++ "`"
    ProgramDuplicateImportAlias name -> "duplicate import alias `" ++ name ++ "`"
    ProgramDuplicateInstance className0 ty -> "duplicate instance `" ++ className0 ++ " " ++ show ty ++ "`"
    ProgramOverlappingInstance className0 left right -> "overlapping instances for `" ++ className0 ++ "`: `" ++ show left ++ "` overlaps `" ++ show right ++ "`"
    ProgramUnknownValue name -> "unknown value `" ++ name ++ "`"
    ProgramUnknownConstructor name -> "unknown constructor `" ++ name ++ "`"
    ProgramUnknownType name -> "unknown type `" ++ name ++ "`"
    ProgramUnknownClass name -> "unknown class `" ++ name ++ "`"
    ProgramUnknownMethod name -> "unknown method `" ++ name ++ "`"
    ProgramInvalidConstructorResult ctor resultTy owner -> "constructor `" ++ ctor ++ "` returns `" ++ show resultTy ++ "` instead of owning type `" ++ owner ++ "`"
    ProgramUnsupportedDeriving className0 -> "unsupported deriving class `" ++ className0 ++ "`"
    ProgramDerivingRequiresNullaryType typeName -> "deriving currently requires a nullary type, but `" ++ typeName ++ "` has parameters"
    ProgramDerivingMissingFieldInstance className0 ty -> "cannot derive `" ++ className0 ++ "` because field type `" ++ show ty ++ "` has no matching instance or constraint"
    ProgramMissingInstanceMethod className0 methodName0 -> "instance for `" ++ className0 ++ "` is missing method `" ++ methodName0 ++ "`"
    ProgramUnexpectedInstanceMethod className0 methodName0 -> "instance for `" ++ className0 ++ "` defines unexpected method `" ++ methodName0 ++ "`"
    ProgramNoMatchingInstance className0 ty -> "no matching instance for `" ++ className0 ++ " " ++ show ty ++ "`"
    ProgramAmbiguousMethodUse methodName0 -> "ambiguous overloaded method use `" ++ methodName0 ++ "`"
    ProgramAmbiguousConstructorUse ctor -> "ambiguous constructor use `" ++ ctor ++ "`"
    ProgramExpectedFunction ty -> "expected a function, got `" ++ show ty ++ "`"
    ProgramTypeMismatch actual expected -> "type mismatch: expected `" ++ show expected ++ "`, got `" ++ show actual ++ "`"
    ProgramCaseOnNonDataType ty -> "case scrutinee is not a data type: `" ++ show ty ++ "`"
    ProgramNonExhaustiveCase ctors -> "non-exhaustive case; missing constructors " ++ show ctors
    ProgramDuplicateCaseBranch ctor -> "unreachable or duplicate case branch for constructor `" ++ ctor ++ "`"
    ProgramPatternConstructorMismatch ctor ty -> "pattern for constructor `" ++ ctor ++ "` does not match expected type `" ++ show ty ++ "`"
    ProgramPipelineError msg -> "pipeline error: " ++ msg
    ProgramMainNotFound -> "main is not defined"
    ProgramMultipleMainDefinitions names -> "multiple main definitions: " ++ show names

programErrorHints :: ProgramError -> [String]
programErrorHints err =
  case err of
    ProgramAmbiguousConstructorUse ctor ->
      ["add an explicit result type annotation, for example `" ++ ctor ++ " : <Type>`"]
    ProgramAmbiguousMethodUse methodName0 ->
      ["apply `" ++ methodName0 ++ "` to enough arguments or add an annotation that fixes the instance type"]
    ProgramNoMatchingInstance className0 ty ->
      ["define or import an instance for `" ++ className0 ++ " " ++ show ty ++ "`"]
    ProgramDerivingMissingFieldInstance className0 ty ->
      ["add a `" ++ className0 ++ " " ++ show ty ++ "` instance or add a type parameter constraint through deriving"]
    ProgramTypeMismatch {} ->
      ["check the nearest annotation; `.mlfp` uses eMLF inference before resolving program obligations"]
    ProgramPatternConstructorMismatch {} ->
      ["check the constructor arity and the data type being matched"]
    ProgramNonExhaustiveCase {} ->
      ["add missing constructor branches or a final wildcard branch"]
    ProgramImportNotExported {} ->
      ["export the name from the source module or remove it from the import exposing list"]
    _ -> []

data EvidenceInfo = EvidenceInfo
  { evidenceClassName :: P.ClassName,
    evidenceType :: SrcType,
    evidenceMethods :: Map P.MethodName (String, SrcType)
  }
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
    methodConstraints :: [P.ClassConstraint],
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
        valueConstraints :: [P.ClassConstraint],
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
    instanceConstraints :: [P.ClassConstraint],
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

constrainedVisibleType :: P.ConstrainedType -> SrcType
constrainedVisibleType constrained
  | null (P.constrainedConstraints constrained) = P.constrainedBody constrained
  | otherwise =
      quantifyFreeVars
        (P.constrainedBody constrained)
        (foldMap constraintFreeVars (P.constrainedConstraints constrained) `mappend` freeVars (P.constrainedBody constrained))
  where
    quantifyFreeVars ty vars =
      foldr forallNoBound ty (Map.keys (Map.fromList [(var, ()) | var <- vars]))

    forallNoBound name acc = STForall name Nothing acc

    constraintFreeVars constraint = freeVars (P.constraintType constraint)

    freeVars ty = case ty of
      STVar name -> [name]
      STArrow dom cod -> freeVars dom ++ freeVars cod
      STBase {} -> []
      STCon _ args -> concatMap freeVars (toList args)
      STForall name mb body ->
        filter (/= name) (maybe [] (freeVars . unSrcBound) mb ++ freeVars body)
      STMu name body -> filter (/= name) (freeVars body)
      STBottom -> []
