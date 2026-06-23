{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module MLF.Frontend.Program.Types
  ( ProgramError (..),
    ProgramDiagnostic (..),
    diagnosticForProgramError,
    renderProgramDiagnostic,
    TypeView (..),
    ConstraintInfo (..),
    constraintTypeView,
    typeViewFromResolved,
    displayConstraint,
    mkTypeView,
    applyTypeViewSubst,
    applyConstraintInfoSubst,
    freeTypeVarsTypeView,
    freeTypeVarsTypeViews,
    typeViewsDisplay,
    typeViewsIdentity,
    TypeViewSubstKey (..),
    TypeViewSubst,
    typeViewSubstKeyFor,
    lookupTypeViewSubst,
    insertTypeViewSubst,
    typeViewSubstDisplayTypes,
    typeViewSubstIdentityTypes,
    typeViewSubstFromParamBinders,
    typeViewSubstFromTypeParams,
    typeParamBinderIdentity,
    TypeBinderSubst,
    emptyTypeBinderSubst,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToNameMap,
    typeBinderSubstToTypeViewSubstWith,
    lookupTypeBinderSubst,
    insertTypeBinderSubst,
    EvidenceMethod (..),
    EvidenceInfo (..),
    SymbolNamespace (..),
    SymbolOwnerIdentity (..),
    SymbolIdentity (..),
    SymbolOrigin (..),
    SymbolSpelling (..),
    ResolvedSymbol (..),
    ResolvedReferenceKind (..),
    ResolvedReference (..),
    ResolvedScope (..),
    ResolvedLocalSymbols (..),
    ResolvedSemanticModule (..),
    ResolvedModuleDiagnosticAdapter (..),
    ResolvedModule (..),
    ResolvedSemanticProgramArtifact (..),
    ResolvedProgram (..),
    resolvedProgramSemanticArtifact,
    resolvedModuleName,
    resolvedModuleIdentity,
    resolvedModuleSyntax,
    resolvedModuleLocalValues,
    resolvedModuleLocalTypes,
    resolvedModuleLocalClasses,
    resolvedModuleScope,
    resolvedModuleExports,
    resolvedModuleReferences,
    mkResolvedSymbol,
    sameResolvedSymbol,
    unqualifiedSymbolName,
    valueInfoSymbolIdentity,
    valueInfoIdentityName,
    dataInfoSymbolIdentity,
    dataName,
    dataInfoIdentityModule,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataInfoIdentityHeadName,
    dataParams,
    constructorInfoSymbolIdentity,
    constructorInfoIdentityName,
    ctorName,
    ctorOwningType,
    classInfoSymbolIdentity,
    classInfoIdentityModule,
    classInfoIdentityName,
    classInfoIdentityQualifiedName,
    className,
    classParamNames,
    classParamIdentityNames,
    classParamBinderIdentities,
    methodInfoSymbolIdentity,
    methodInfoIdentityName,
    methodClassName,
    methodName,
    methodParamName,
    methodParamIdentityName,
    methodParamBinders,
    lookupMethodParamViewSubst,
    methodTypeView,
    methodResultTypeView,
    methodInfoOwnerClassSymbolIdentity,
    instanceClassName,
    instanceInfoClassSymbolIdentity,
    instanceOriginModuleName,
    moduleSymbolIdentity,
    lookupClassMethod,
    lookupInstanceMethod,
    resolvedValueInfoSymbol,
    resolvedDataInfoSymbol,
    resolvedConstructorInfoSymbol,
    resolvedClassInfoSymbol,
    resolvedMethodInfoSymbol,
    resolvedModuleSymbol,
    resolvedModuleSymbolFromIdentity,
    ConstructorShape (..),
    ConstructorInfo (..),
    DataInfo (..),
    FunctionalDependencyInfo (..),
    MethodInfo (..),
    ClassInfo (..),
    ValueInfo (..),
    InstanceInfo (..),
    LocalRef (..),
    PrimitiveRef (..),
    DeferredRef (..),
    ConstructorRef (..),
    IdDetails (..),
    LoweredBindingIdentity (..),
    ResolvedVar (..),
    constructorRefFromInfo,
    loweredBindingIdentityFromConstructorInfo,
    loweredBindingIdentityFromValueInfo,
    resolvedVarFromValueInfo,
    resolvedVarFromLoweredBinding,
    loweredBindingConstructorRef,
    checkedBindingConstructorRef,
    DeferredBindingMode (..),
    DeferredMethodEvidence (..),
    DeferredMethodCall (..),
    deferredMethodPlaceholder,
    deferredMethodName,
    DeferredConstructorCall (..),
    deferredConstructorPlaceholder,
    DeferredCaseCall (..),
    deferredCasePlaceholder,
    DeferredProgramObligation (..),
    DeferredObligations,
    deferredProgramObligationRef,
    ExportedTypeInfo (..),
    mkExportedTypeInfo,
    exportedTypeConstructorsForDisplay,
    ModuleExports (..),
    moduleExportsFromMaps,
    exportedValuesForDisplay,
    exportedTypesForDisplay,
    exportedClassesForDisplay,
    LoweredBinding (..),
    loweredBindingName,
    CheckedBinding (..),
    checkedBindingName,
    CheckedModule (..),
    CheckedProgram (..),
    checkedProgramMain,
    splitForalls,
    splitArrows,
    applyTypeHead,
    substituteTypeVar,
    constructorOwnerRuntimeTypeTrackable,
    constructorOwnerHasVariableHeadApplication,
    constructorOwnerShapes,
    constructorShapeName,
    constructorShapeFromInfo,
    dataConstructorsRuntimeTypeTrackable,
    srcTypeHasVariableHeadApplication,
    specializeMethodType,
    specializeMethodTypes,
    specializeMethodTypeView,
    constrainedVisibleType,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Elab.Types (XmlfTerm, ElabType, ResolvedVar (..), resolvedVarConstructorRef)
import MLF.Frontend.Symbol
  ( ResolvedReference (..),
    ResolvedReferenceKind (..),
    ResolvedSymbol (..),
    SymbolIdentity (..),
    SymbolNamespace (..),
    SymbolOrigin (..),
    SymbolOwnerIdentity (..),
    SymbolSpelling (..),
    mkResolvedSymbol,
    sameResolvedSymbol,
    unqualifiedSymbolName,
  )
import MLF.Frontend.Syntax
  ( ResolvedSrcType,
    ResolvedSrcTy (..),
    ResolvedSrcBound (..),
    ResolvedTypeBinderRef (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    SurfaceExpr,
    resolvedSrcTypeBinderIdentityName,
    resolvedSrcTypeIdentityType,
    resolvedSrcTypeToSrcType,
  )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Frontend.TypeLevel (TypeLevelKind (..), TypeLevelNormalizeError (..), TypeLevelTy (..))
import MLF.Types.Identity

data ProgramError
  = ProgramDuplicateModule P.ModuleName
  | ProgramUnknownImportModule P.ModuleName
  | ProgramImportNotExported P.ModuleName String
  | ProgramImportCycle [P.ModuleName]
  | ProgramInvalidExport String
  | ProgramExportNotLocal String
  | ProgramDuplicateVisibleName String
  | ProgramDuplicateType String
  | ProgramDuplicateTypeParameter String
  | ProgramDuplicateConstructor String
  | ProgramDuplicateClass String
  | ProgramDuplicateValue String
  | ProgramDuplicateMethod String
  | ProgramDuplicateImportAlias P.ModuleName
  | ProgramDuplicateInstance P.ClassName SrcType
  | ProgramDuplicateInstanceHead P.ClassName [SrcType]
  | ProgramOverlappingInstance P.ClassName SrcType SrcType
  | ProgramOverlappingInstanceHead P.ClassName [SrcType] [SrcType]
  | ProgramUnknownValue String
  | ProgramUnknownConstructor String
  | ProgramUnknownType String
  | ProgramUnknownClass String
  | ProgramUnknownMethod String
  | ProgramAmbiguousUnqualifiedReference String
  | ProgramKindMismatch SrcType P.SrcKind P.SrcKind
  | ProgramTypeArityMismatch String Int Int
  | ProgramClassArityMismatch P.ClassName Int Int
  | ProgramInvalidConstructorResult P.ConstructorName SrcType P.TypeName
  | ProgramUnsupportedDeriving P.ClassName
  | ProgramUnsupportedTypeFamily P.TypeName
  | ProgramUnsupportedTypeFamilyType SrcType
  | ProgramResidualTypeLambda SrcType
  | ProgramUnsupportedTypeApplication SrcType
  | ProgramTypeLevelReductionFailed SrcType TypeLevelNormalizeError
  | ProgramTypeFamilyReductionFailed P.TypeName TypeLevelNormalizeError
  | ProgramUnboundTypeFamilyVariable String
  | ProgramTypeFamilyEquationArityMismatch P.TypeName Int Int
  | ProgramTypeFamilyKindMismatch P.TypeName TypeLevelTy TypeLevelKind TypeLevelKind
  | ProgramUnsupportedMultiParameterClass P.ClassName Int
  | ProgramUnsupportedSuperclassConstraint P.ClassName
  | ProgramUnsupportedFunctionalDependency P.ClassName
  | ProgramInvalidFunctionalDependency P.ClassName String
  | ProgramAmbiguousFunctionalDependencyInstance P.ClassName [SrcType]
  | ProgramConflictingFunctionalDependency P.ClassName [SrcType] [SrcType] [SrcType]
  | ProgramUnsupportedMultiParameterConstraint P.ClassName [SrcType]
  | ProgramUnsupportedMultiParameterInstance P.ClassName [SrcType]
  | ProgramDerivingRequiresNullaryType P.TypeName
  | ProgramDerivingMissingFieldInstance P.ClassName SrcType
  | ProgramMissingInstanceMethod P.ClassName P.MethodName
  | ProgramUnexpectedInstanceMethod P.ClassName P.MethodName
  | ProgramNoMatchingInstance P.ClassName SrcType
  | ProgramNoMatchingInstanceHead P.ClassName [SrcType]
  | ProgramAmbiguousMethodUse P.MethodName
  | ProgramAmbiguousConstrainedValueUse String
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
    ProgramUnknownImportModule name -> firstSpan name (P.spanImports index) <|> Map.lookup name (P.spanModules index)
    ProgramImportNotExported _ name -> firstSpan name (P.spanImportItems index) <|> lookupAnyName name index
    ProgramImportCycle (name : _) -> Map.lookup name (P.spanModules index) <|> firstSpan name (P.spanImports index)
    ProgramImportCycle [] -> Nothing
    ProgramInvalidExport name -> firstSpan name (P.spanExportItems index) <|> lookupAnyName name index
    ProgramExportNotLocal name -> firstSpan name (P.spanExportItems index) <|> lookupAnyName name index
    ProgramDuplicateVisibleName name -> lookupAnyName name index
    ProgramDuplicateType name -> firstSpan name (P.spanTypes index)
    ProgramDuplicateTypeParameter name -> lookupAnyName name index
    ProgramDuplicateConstructor name -> firstSpan name (P.spanConstructors index)
    ProgramDuplicateClass name -> firstSpan name (P.spanClasses index)
    ProgramDuplicateValue name -> firstSpan name (P.spanValues index)
    ProgramDuplicateMethod name -> firstSpan name (P.spanValues index)
    ProgramDuplicateImportAlias name -> firstSpan name (P.spanImportAliases index) <|> Map.lookup name (P.spanModules index)
    ProgramDuplicateInstance className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramDuplicateInstanceHead className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramOverlappingInstance className0 _ _ -> firstSpan className0 (P.spanClasses index)
    ProgramOverlappingInstanceHead className0 _ _ -> firstSpan className0 (P.spanClasses index)
    ProgramUnknownValue name -> lookupAnyName name index
    ProgramUnknownConstructor name -> firstSpan name (P.spanConstructors index)
    ProgramUnknownType name -> firstSpan name (P.spanTypes index)
    ProgramUnknownClass name -> firstSpan name (P.spanClasses index)
    ProgramUnknownMethod name -> firstSpan name (P.spanValues index)
    ProgramAmbiguousUnqualifiedReference name -> lookupAnyName name index
    ProgramKindMismatch ty _ _ -> sourceTypeHeadSpan ty index
    ProgramTypeArityMismatch name _ _ -> lookupAnyName name index
    ProgramClassArityMismatch className0 _ _ -> firstSpan className0 (P.spanClasses index)
    ProgramInvalidConstructorResult ctor _ _ -> firstSpan ctor (P.spanConstructors index)
    ProgramUnsupportedDeriving className0 -> firstSpan className0 (P.spanClasses index)
    ProgramUnsupportedTypeFamily typeName -> firstSpan typeName (P.spanTypes index)
    ProgramUnsupportedTypeFamilyType ty -> sourceTypeHeadSpan ty index
    ProgramResidualTypeLambda ty -> sourceTypeHeadSpan ty index
    ProgramUnsupportedTypeApplication ty -> sourceTypeHeadSpan ty index
    ProgramTypeLevelReductionFailed ty _ -> sourceTypeHeadSpan ty index
    ProgramTypeFamilyReductionFailed typeName _ -> lookupAnyName typeName index
    ProgramUnboundTypeFamilyVariable name -> firstSpan name (P.spanTypes index)
    ProgramTypeFamilyEquationArityMismatch typeName _ _ -> firstSpan typeName (P.spanTypes index)
    ProgramTypeFamilyKindMismatch typeName _ _ _ -> firstSpan typeName (P.spanTypes index)
    ProgramUnsupportedMultiParameterClass className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramUnsupportedSuperclassConstraint className0 -> firstSpan className0 (P.spanClasses index)
    ProgramUnsupportedFunctionalDependency className0 -> firstSpan className0 (P.spanClasses index)
    ProgramInvalidFunctionalDependency className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramAmbiguousFunctionalDependencyInstance className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramConflictingFunctionalDependency className0 _ _ _ -> firstSpan className0 (P.spanClasses index)
    ProgramUnsupportedMultiParameterConstraint className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramUnsupportedMultiParameterInstance className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramDerivingRequiresNullaryType typeName -> firstSpan typeName (P.spanTypes index)
    ProgramDerivingMissingFieldInstance className0 _ -> firstSpan className0 (P.spanClasses index)
    ProgramMissingInstanceMethod _ methodName0 -> firstSpan methodName0 (P.spanValues index)
    ProgramUnexpectedInstanceMethod _ methodName0 -> firstSpan methodName0 (P.spanValues index)
    ProgramNoMatchingInstance {} -> Nothing
    ProgramNoMatchingInstanceHead {} -> Nothing
    ProgramAmbiguousMethodUse methodName0 -> firstSpan methodName0 (P.spanValues index)
    ProgramAmbiguousConstrainedValueUse valueName -> firstSpan valueName (P.spanValues index)
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
    ProgramDuplicateTypeParameter name -> "duplicate type parameter `" ++ name ++ "`"
    ProgramDuplicateConstructor name -> "duplicate constructor `" ++ name ++ "`"
    ProgramDuplicateClass name -> "duplicate class `" ++ name ++ "`"
    ProgramDuplicateValue name -> "duplicate value `" ++ name ++ "`"
    ProgramDuplicateMethod name -> "duplicate method `" ++ name ++ "`"
    ProgramDuplicateImportAlias name -> "duplicate import alias `" ++ name ++ "`"
    ProgramDuplicateInstance className0 ty -> "duplicate instance `" ++ className0 ++ " " ++ show ty ++ "`"
    ProgramDuplicateInstanceHead className0 tys -> "duplicate instance `" ++ renderClassHead className0 tys ++ "`"
    ProgramOverlappingInstance className0 left right -> "overlapping instances for `" ++ className0 ++ "`: `" ++ show left ++ "` overlaps `" ++ show right ++ "`"
    ProgramOverlappingInstanceHead className0 left right -> "overlapping instances for `" ++ className0 ++ "`: `" ++ renderClassHead className0 left ++ "` overlaps `" ++ renderClassHead className0 right ++ "`"
    ProgramUnknownValue name -> "unknown value `" ++ name ++ "`"
    ProgramUnknownConstructor name -> "unknown constructor `" ++ name ++ "`"
    ProgramUnknownType name -> "unknown type `" ++ name ++ "`"
    ProgramUnknownClass name -> "unknown class `" ++ name ++ "`"
    ProgramUnknownMethod name -> "unknown method `" ++ name ++ "`"
    ProgramAmbiguousUnqualifiedReference name -> "ambiguous unqualified reference `" ++ name ++ "`"
    ProgramKindMismatch ty expected actual ->
      "kind mismatch in `"
        ++ show ty
        ++ "`: expected `"
        ++ renderSrcKind expected
        ++ "`, got `"
        ++ renderSrcKind actual
        ++ "`"
    ProgramTypeArityMismatch name expected actual ->
      "type constructor `"
        ++ name
        ++ "` expects "
        ++ show expected
        ++ " type argument"
        ++ plural expected
        ++ ", but got "
        ++ show actual
    ProgramClassArityMismatch className0 expected actual ->
      "class `"
        ++ className0
        ++ "` expects "
        ++ show expected
        ++ " type argument"
        ++ plural expected
        ++ ", but got "
        ++ show actual
    ProgramInvalidConstructorResult ctor resultTy owner -> "constructor `" ++ ctor ++ "` returns `" ++ show resultTy ++ "` instead of owning type `" ++ owner ++ "`"
    ProgramUnsupportedDeriving className0 -> "unsupported deriving class `" ++ className0 ++ "`"
    ProgramUnsupportedTypeFamily typeName -> "resolved program still contains unerased type family `" ++ typeName ++ "`"
    ProgramUnsupportedTypeFamilyType ty -> "unsupported type-family argument or result type `" ++ show ty ++ "`"
    ProgramResidualTypeLambda ty -> "residual type lambda reached `.mlfp` core boundary: `" ++ show ty ++ "`"
    ProgramUnsupportedTypeApplication ty -> "unsupported type application reached `.mlfp` core boundary: `" ++ show ty ++ "`"
    ProgramTypeLevelReductionFailed ty reductionErr -> "type-level expression `" ++ show ty ++ "` failed to reduce: " ++ renderTypeLevelNormalizeError reductionErr
    ProgramTypeFamilyReductionFailed typeName reductionErr -> "type family `" ++ typeName ++ "` failed to reduce: " ++ renderTypeLevelNormalizeError reductionErr
    ProgramUnboundTypeFamilyVariable name -> "unbound type-family equation variable `" ++ name ++ "`"
    ProgramTypeFamilyEquationArityMismatch typeName expected actual ->
      "type family `"
        ++ typeName
        ++ "` equation expects "
        ++ show expected
        ++ " pattern"
        ++ plural expected
        ++ ", but got "
        ++ show actual
    ProgramTypeFamilyKindMismatch typeName ty expected actual ->
      "type family `"
        ++ typeName
        ++ "` has kind mismatch in `"
        ++ show ty
        ++ "`: expected `"
        ++ show expected
        ++ "`, got `"
        ++ show actual
        ++ "`"
    ProgramUnsupportedMultiParameterClass className0 count ->
      "class `"
        ++ className0
        ++ "` has "
        ++ show count
        ++ " parameters; generalized typeclass evidence is not wired through the checker yet"
    ProgramUnsupportedSuperclassConstraint className0 -> "class `" ++ className0 ++ "` declares unsupported superclass constraints"
    ProgramUnsupportedFunctionalDependency className0 -> "class `" ++ className0 ++ "` declares functional dependencies in a checker context that does not support class declarations"
    ProgramInvalidFunctionalDependency className0 name ->
      "class `" ++ className0 ++ "` declares a functional dependency over unknown or repeated parameter `" ++ name ++ "`"
    ProgramAmbiguousFunctionalDependencyInstance className0 tys ->
      "instance `" ++ renderClassHead className0 tys ++ "` leaves a functional-dependency result unconstrained"
    ProgramConflictingFunctionalDependency className0 determiners left right ->
      "functional dependency conflict for `"
        ++ className0
        ++ "`: determinant `"
        ++ unwords (map show determiners)
        ++ "` maps to both `"
        ++ unwords (map show left)
        ++ "` and `"
        ++ unwords (map show right)
        ++ "`"
    ProgramUnsupportedMultiParameterConstraint className0 tys ->
      "constraint `" ++ renderClassHead className0 tys ++ "` uses multiple class arguments; generalized constraint evidence is not wired through the checker yet"
    ProgramUnsupportedMultiParameterInstance className0 tys ->
      "instance `" ++ renderClassHead className0 tys ++ "` uses multiple class arguments; generalized instance evidence is not wired through the checker yet"
    ProgramDerivingRequiresNullaryType typeName -> "deriving currently requires a nullary type, but `" ++ typeName ++ "` has parameters"
    ProgramDerivingMissingFieldInstance className0 ty -> "cannot derive `" ++ className0 ++ "` because field type `" ++ show ty ++ "` has no matching instance or constraint"
    ProgramMissingInstanceMethod className0 methodName0 -> "instance for `" ++ className0 ++ "` is missing method `" ++ methodName0 ++ "`"
    ProgramUnexpectedInstanceMethod className0 methodName0 -> "instance for `" ++ className0 ++ "` defines unexpected method `" ++ methodName0 ++ "`"
    ProgramNoMatchingInstance className0 ty -> "no matching instance for `" ++ className0 ++ " " ++ show ty ++ "`"
    ProgramNoMatchingInstanceHead className0 tys -> "no matching instance for `" ++ renderClassHead className0 tys ++ "`"
    ProgramAmbiguousMethodUse methodName0 -> "ambiguous overloaded method use `" ++ methodName0 ++ "`"
    ProgramAmbiguousConstrainedValueUse valueName -> "ambiguous constrained value use `" ++ valueName ++ "`"
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
      ["apply `" ++ methodName0 ++ "` to enough arguments, or give a nullary method an expected type annotation that fixes the instance type"]
    ProgramAmbiguousConstrainedValueUse valueName ->
      ["use `" ++ valueName ++ "` at a concrete instance type; generic constrained value aliases are not supported yet"]
    ProgramNoMatchingInstance className0 ty ->
      ["define or import an instance for `" ++ className0 ++ " " ++ show ty ++ "`"]
    ProgramNoMatchingInstanceHead className0 tys ->
      ["define or import an instance for `" ++ renderClassHead className0 tys ++ "`"]
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
    ProgramKindMismatch {} ->
      ["check higher-kinded parameter annotations and type constructor application arguments"]
    ProgramTypeArityMismatch {} ->
      ["apply the type constructor to exactly the number of arguments required by its kind"]
    ProgramClassArityMismatch {} ->
      ["apply the class to exactly the number of arguments declared by its class head"]
    ProgramUnsupportedTypeFamily {} ->
      ["run parsed `.mlfp` programs through the type-family normalization entrypoint before resolving"]
    ProgramUnsupportedTypeFamilyType {} ->
      ["keep closed type-family arguments and results in the first-order type-level fragment for now"]
    ProgramResidualTypeLambda {} ->
      ["apply type lambdas before the core boundary; only normalized family-free source types can be erased"]
    ProgramUnsupportedTypeApplication {} ->
      ["use constructor-headed or variable-headed type applications after type-level normalization"]
    ProgramTypeLevelReductionFailed {} ->
      ["type-level applications reduce before `.mlfp` erasure; simplify the type expression or remove the cycle"]
    ProgramTypeFamilyReductionFailed {} ->
      ["closed type families reduce by ordered first match; add a matching equation or remove the cyclic family use"]
    ProgramUnboundTypeFamilyVariable {} ->
      ["bind the variable in the equation's left-hand pattern or in a local type lambda"]
    ProgramTypeFamilyEquationArityMismatch {} ->
      ["make every closed type-family equation use exactly the declared family parameter count"]
    ProgramTypeFamilyKindMismatch {} ->
      ["check family parameter/result kind annotations, type-lambda binder kinds, and constructor application arity"]
    ProgramUnsupportedMultiParameterClass {} ->
      ["multi-parameter method dispatch requires each class argument to be fixed by supplied arguments, result type, or local evidence"]
    ProgramUnsupportedSuperclassConstraint {} ->
      ["keep superclass constraints within the checked method-evidence subset"]
    ProgramUnsupportedFunctionalDependency {} ->
      ["use the `.mlfp` program checker for functional-dependency class declarations"]
    ProgramInvalidFunctionalDependency {} ->
      ["use only class-head parameter names in each functional dependency"]
    ProgramAmbiguousFunctionalDependencyInstance {} ->
      ["make every determined instance-head variable appear in the determinant side of the functional dependency"]
    ProgramConflictingFunctionalDependency {} ->
      ["remove or specialize one of the instances so the determinant side fixes a single result"]
    ProgramUnsupportedMultiParameterConstraint {} ->
      ["parenthesize complex single class arguments; true multi-argument constraints require the generalized evidence path"]
    ProgramUnsupportedMultiParameterInstance {} ->
      ["parenthesize complex single instance arguments; true multi-argument instances require the generalized evidence path"]
    _ -> []

renderClassHead :: P.ClassName -> [SrcType] -> String
renderClassHead className0 tys =
  unwords (className0 : map show tys)

renderTypeLevelNormalizeError :: TypeLevelNormalizeError -> String
renderTypeLevelNormalizeError err =
  case err of
    UnknownTypeFamily name -> "unknown family `" ++ name ++ "`"
    TypeFamilyArityMismatch name expected actual ->
      "family `"
        ++ name
        ++ "` expects "
        ++ show expected
        ++ " argument"
        ++ plural expected
        ++ ", but got "
        ++ show actual
    TypeFamilyStuck name args -> "stuck application `" ++ name ++ " " ++ unwords (map show args) ++ "`"
    TypeFamilyCycle names -> "cycle " ++ show names
    TypeLevelFuelExhausted ty -> "fuel exhausted while reducing `" ++ show ty ++ "`"

renderSrcKind :: P.SrcKind -> String
renderSrcKind = go 0
  where
    go :: Int -> P.SrcKind -> String
    go prec kind0 =
      case kind0 of
        P.KType -> "*"
        P.KArrow left right ->
          let rendered = go 1 left ++ " -> " ++ go 0 right
           in if prec > 0 then "(" ++ rendered ++ ")" else rendered

plural :: Int -> String
plural 1 = ""
plural _ = "s"

sourceTypeHeadSpan :: SrcType -> P.ProgramSpanIndex -> Maybe P.SourceSpan
sourceTypeHeadSpan ty index =
  case sourceTypeHeadName ty of
    Just name -> lookupAnyName name index
    Nothing -> Nothing

sourceTypeHeadName :: SrcType -> Maybe String
sourceTypeHeadName ty =
  case ty of
    STVar name -> Just name
    STBase name -> Just name
    STCon name _ -> Just name
    STVarApp name _ -> Just name
    STTyLam _ body -> sourceTypeHeadName body
    STTyApp fun _ -> sourceTypeHeadName fun
    STArrow dom _ -> sourceTypeHeadName dom
    STForall _ _ body -> sourceTypeHeadName body
    STMu _ body -> sourceTypeHeadName body
    STBottom -> Nothing

data ResolvedScope = ResolvedScope
  { resolvedScopeValues :: Map String ResolvedSymbol,
    resolvedScopeTypes :: Map String ResolvedSymbol,
    resolvedScopeClasses :: Map String ResolvedSymbol,
    resolvedScopeModules :: Map P.ModuleName ResolvedSymbol
  }
  deriving (Eq, Show)

data ResolvedLocalSymbols = ResolvedLocalSymbols
  { resolvedLocalValues :: Map String [ResolvedSymbol],
    resolvedLocalTypes :: Map String [ResolvedSymbol],
    resolvedLocalClasses :: Map String [ResolvedSymbol]
  }
  deriving (Eq, Show)

data ResolvedSemanticModule = ResolvedSemanticModule
  { resolvedSemanticModuleName :: P.ModuleName,
    resolvedSemanticModuleIdentity :: SymbolIdentity,
    resolvedSemanticModuleSyntax :: P.ResolvedModuleSyntax,
    resolvedSemanticModuleLocalSymbols :: ResolvedLocalSymbols,
    resolvedSemanticModuleScope :: ResolvedScope,
    resolvedSemanticModuleExports :: ResolvedScope
  }
  deriving (Eq, Show)

data ResolvedModuleDiagnosticAdapter = ResolvedModuleDiagnosticAdapter
  { resolvedDiagnosticReferences :: [ResolvedReference]
  }
  deriving (Eq, Show)

data ResolvedModule = ResolvedModule
  { resolvedModuleSemantic :: ResolvedSemanticModule,
    resolvedModuleDiagnosticAdapter :: ResolvedModuleDiagnosticAdapter
  }
  deriving (Eq, Show)

newtype ResolvedSemanticProgramArtifact = ResolvedSemanticProgramArtifact
  { resolvedSemanticProgramModules :: [ResolvedSemanticModule]
  }
  deriving (Eq, Show)

newtype ResolvedProgram = ResolvedProgram
  { resolvedProgramModules :: [ResolvedModule]
  }
  deriving (Eq, Show)

resolvedProgramSemanticArtifact :: ResolvedProgram -> ResolvedSemanticProgramArtifact
resolvedProgramSemanticArtifact resolvedProgram =
  ResolvedSemanticProgramArtifact
    { resolvedSemanticProgramModules =
        map resolvedModuleSemantic (resolvedProgramModules resolvedProgram)
    }

resolvedModuleName :: ResolvedModule -> P.ModuleName
resolvedModuleName = resolvedSemanticModuleName . resolvedModuleSemantic

resolvedModuleIdentity :: ResolvedModule -> SymbolIdentity
resolvedModuleIdentity = resolvedSemanticModuleIdentity . resolvedModuleSemantic

resolvedModuleSyntax :: ResolvedModule -> P.ResolvedModuleSyntax
resolvedModuleSyntax = resolvedSemanticModuleSyntax . resolvedModuleSemantic

resolvedModuleLocalValues :: ResolvedModule -> Map String [ResolvedSymbol]
resolvedModuleLocalValues =
  resolvedLocalValues . resolvedSemanticModuleLocalSymbols . resolvedModuleSemantic

resolvedModuleLocalTypes :: ResolvedModule -> Map String [ResolvedSymbol]
resolvedModuleLocalTypes =
  resolvedLocalTypes . resolvedSemanticModuleLocalSymbols . resolvedModuleSemantic

resolvedModuleLocalClasses :: ResolvedModule -> Map String [ResolvedSymbol]
resolvedModuleLocalClasses =
  resolvedLocalClasses . resolvedSemanticModuleLocalSymbols . resolvedModuleSemantic

resolvedModuleScope :: ResolvedModule -> ResolvedScope
resolvedModuleScope = resolvedSemanticModuleScope . resolvedModuleSemantic

resolvedModuleExports :: ResolvedModule -> ResolvedScope
resolvedModuleExports = resolvedSemanticModuleExports . resolvedModuleSemantic

resolvedModuleReferences :: ResolvedModule -> [ResolvedReference]
resolvedModuleReferences =
  resolvedDiagnosticReferences . resolvedModuleDiagnosticAdapter

{- Note [Resolved semantic program artifact]
`ResolvedSemanticProgramArtifact` is the Resolve-to-Check seam.  It groups the
resolved module syntax, local semantic symbols, full visible scope, and export
scope as one semantic artifact so `Check` does not assemble policy from peer
records on `ResolvedModule`.

`ResolvedModule` keeps the semantic artifact plus diagnostic adapters.  Raw
resolved syntax and reference-list accessors stay available for diagnostics,
audits, backend adapters, and tests, but checker policy should enter through
`resolvedProgramSemanticArtifact`.
-}

{- Note [Resolved .mlfp symbol identities]
`SymbolIdentity` is the semantic key. Local declarations carry a generated
unique identity, and equality/ordering uses that identity. String module/name
fields remain as stable display and compatibility metadata for boundary paths.
Method/constructor identities carry their owning class/type identity.
`SymbolSpelling` is the reference-side surface data: the source name, display
name, and whether it came from a local declaration, unqualified import,
qualified/aliased import, or builtin.

The checker and elaborator keep visible maps keyed by surface spelling because
source lookup and diagnostics need those names. Downstream metadata stores
`SymbolIdentity` separately, and semantic checks compare that identity instead
of inferring declaration equality from qualified/unqualified strings. Runtime
names remain explicit generated names, not semantic identities.
-}

data TypeView = TypeView
  { typeViewDisplay :: SrcType,
    typeViewIdentity :: SrcType,
    typeViewBinderIdentities :: Map String TypeBinderIdentity
  }
  deriving (Eq, Show)

mkTypeView :: SrcType -> SrcType -> TypeView
mkTypeView display identity =
  TypeView
    { typeViewDisplay = display,
      typeViewIdentity = identity,
      typeViewBinderIdentities = Map.empty
    }

data TypeViewSubstKey
  = TypeViewSubstByIdentity TypeBinderIdentity String String
  | TypeViewSubstByName String
  deriving (Show)

instance Eq TypeViewSubstKey where
  left == right =
    compare left right == EQ

instance Ord TypeViewSubstKey where
  compare left right =
    case (left, right) of
      (TypeViewSubstByIdentity leftIdentity _ _, TypeViewSubstByIdentity rightIdentity _ _) ->
        compare leftIdentity rightIdentity
      (TypeViewSubstByIdentity {}, TypeViewSubstByName {}) ->
        LT
      (TypeViewSubstByName {}, TypeViewSubstByIdentity {}) ->
        GT
      (TypeViewSubstByName leftName, TypeViewSubstByName rightName) ->
        compare leftName rightName

type TypeViewSubst = Map TypeViewSubstKey TypeView

typeViewSubstKeyFor :: TypeView -> String -> String -> TypeViewSubstKey
typeViewSubstKeyFor view displayName identityName =
  case Map.lookup identityName (typeViewBinderIdentities view) of
    Just identity -> TypeViewSubstByIdentity identity displayName identityName
    Nothing -> TypeViewSubstByName identityName

lookupTypeViewSubst :: TypeViewSubstKey -> TypeViewSubst -> Maybe TypeView
lookupTypeViewSubst key subst =
  Map.lookup key subst

insertTypeViewSubst :: TypeViewSubstKey -> TypeView -> TypeViewSubst -> TypeViewSubst
insertTypeViewSubst =
  Map.insert

data ConstraintInfo = ConstraintInfo
  { constraintDisplayClass :: P.ClassName,
    constraintClassSymbol :: SymbolIdentity,
    constraintTypeViews :: NonEmpty TypeView
  }
  deriving (Eq, Show)

typeViewFromResolved :: ResolvedSrcType -> TypeView
typeViewFromResolved ty =
  TypeView
    { typeViewDisplay = resolvedSrcTypeToSrcType ty,
      typeViewIdentity = resolvedSrcTypeIdentityType ty,
      typeViewBinderIdentities = resolvedSrcTypeBinderIdentities ty
    }

resolvedSrcTypeBinderIdentities :: ResolvedSrcTy n v -> Map String TypeBinderIdentity
resolvedSrcTypeBinderIdentities ty =
  case ty of
    RSTVar ref -> binder ref
    RSTArrow dom cod -> resolvedSrcTypeBinderIdentities dom <> resolvedSrcTypeBinderIdentities cod
    RSTBase {} -> Map.empty
    RSTCon _ args -> foldMap resolvedSrcTypeBinderIdentities args
    RSTVarApp ref args -> binder ref <> foldMap resolvedSrcTypeBinderIdentities args
    RSTTyLam ref body -> binder ref <> resolvedSrcTypeBinderIdentities body
    RSTTyApp fun arg -> resolvedSrcTypeBinderIdentities fun <> resolvedSrcTypeBinderIdentities arg
    RSTForall ref mb body ->
      binder ref
        <> maybe Map.empty (resolvedSrcTypeBinderIdentities . unResolvedSrcBound) mb
        <> resolvedSrcTypeBinderIdentities body
    RSTMu ref body -> binder ref <> resolvedSrcTypeBinderIdentities body
    RSTBottom -> Map.empty
  where
    binder ref =
      Map.singleton
        (resolvedSrcTypeBinderIdentityName ref)
        (typeBinderIdentityFromUnique (resolvedTypeBinderIdentity ref))

displayConstraint :: ConstraintInfo -> P.ClassConstraint
displayConstraint constraint =
  P.ClassConstraint
    { P.constraintClassName = constraintDisplayClass constraint,
      P.constraintTypes = typeViewsDisplay (constraintTypeViews constraint)
    }

applyTypeViewSubst :: TypeViewSubst -> TypeView -> TypeView
applyTypeViewSubst subst view =
  TypeView
    { typeViewDisplay = Map.foldrWithKey substituteTypeVar (typeViewDisplay view) displaySubst,
      typeViewIdentity = Map.foldrWithKey substituteTypeVar (typeViewIdentity view) identitySubst,
      typeViewBinderIdentities = Map.empty
    }
  where
    displaySubst = typeViewSubstDisplayTypes subst
    identitySubst = typeViewSubstIdentityTypes subst

typeViewSubstDisplayTypes :: TypeViewSubst -> Map String SrcType
typeViewSubstDisplayTypes subst =
  Map.fromList
    [ (typeViewSubstKeyDisplayName key, typeViewDisplay substView)
      | (key, substView) <- Map.toList subst
    ]

typeViewSubstIdentityTypes :: TypeViewSubst -> Map String SrcType
typeViewSubstIdentityTypes subst =
  Map.fromList
    [ (typeViewSubstKeyIdentityName key, typeViewIdentity substView)
      | (key, substView) <- Map.toList subst
    ]

typeViewSubstKeyDisplayName :: TypeViewSubstKey -> String
typeViewSubstKeyDisplayName key =
  case key of
    TypeViewSubstByIdentity _ displayName _ -> displayName
    TypeViewSubstByName name -> name

typeViewSubstKeyIdentityName :: TypeViewSubstKey -> String
typeViewSubstKeyIdentityName key =
  case key of
    TypeViewSubstByIdentity _ _ identityName -> identityName
    TypeViewSubstByName name -> name

applyConstraintInfoSubst :: TypeViewSubst -> ConstraintInfo -> ConstraintInfo
applyConstraintInfoSubst subst constraint =
  let views = fmap (applyTypeViewSubst subst) (constraintTypeViews constraint)
   in constraint
        { constraintTypeViews = views
        }

constraintTypeView :: ConstraintInfo -> TypeView
constraintTypeView =
  NE.head . constraintTypeViews

freeTypeVarsTypeView :: TypeView -> Set String
freeTypeVarsTypeView = freeTypeVarsSrcType . typeViewIdentity

freeTypeVarsTypeViews :: NonEmpty TypeView -> Set String
freeTypeVarsTypeViews = foldMap freeTypeVarsTypeView

typeViewsDisplay :: NonEmpty TypeView -> NonEmpty SrcType
typeViewsDisplay = fmap typeViewDisplay

typeViewsIdentity :: NonEmpty TypeView -> NonEmpty SrcType
typeViewsIdentity = fmap typeViewIdentity

typeViewSubstFromParamBinders :: NonEmpty (String, String, Maybe TypeBinderIdentity) -> NonEmpty TypeView -> TypeViewSubst
typeViewSubstFromParamBinders params views =
  Map.fromList (concat (zipWith entries (NE.toList params) (NE.toList views)))
  where
    entries (displayName, identityName, mbIdentity) view =
      let nameEntries =
            [ (TypeViewSubstByName displayName, view),
              (TypeViewSubstByName identityName, view)
            ]
       in case mbIdentity of
            Just identity ->
              [(TypeViewSubstByIdentity identity displayName identityName, view)]
            Nothing -> nameEntries

typeParamBinderIdentity :: P.TypeParam -> Maybe TypeBinderIdentity
typeParamBinderIdentity param =
  typeBinderIdentityFromUnique . resolvedTypeBinderIdentity <$> P.typeParamRef param

typeViewSubstFromTypeParams :: NonEmpty P.TypeParam -> NonEmpty TypeView -> TypeViewSubst
typeViewSubstFromTypeParams params views =
  typeViewSubstFromParamBinders (fmap typeParamBinder params) views
  where
    typeParamBinder param =
      (P.typeParamName param, P.typeParamIdentityName param, typeParamBinderIdentity param)

data TypeBinderSubst = TypeBinderSubst
  { typeBinderSubstByIdentity :: Map TypeBinderIdentity (Set String, SrcType),
    typeBinderSubstByName :: Map String SrcType
  }
  deriving (Eq, Show)

emptyTypeBinderSubst :: TypeBinderSubst
emptyTypeBinderSubst =
  TypeBinderSubst
    { typeBinderSubstByIdentity = Map.empty,
      typeBinderSubstByName = Map.empty
    }

typeBinderSubstFromTypeViewSubst :: [(String, Maybe TypeBinderIdentity)] -> TypeViewSubst -> TypeBinderSubst
typeBinderSubstFromTypeViewSubst binders subst =
  foldr insertView emptyTypeBinderSubst (Map.toList subst)
  where
    bindersByIdentity =
      Map.fromList [(identity, name) | (name, Just identity) <- binders]
    bindersByName =
      Map.fromList binders

    insertView (key, view) acc =
      case key of
        TypeViewSubstByIdentity identity displayName identityName ->
          let name = Map.findWithDefault (identityNameOrDisplay displayName identityName) identity bindersByIdentity
           in insertTypeBinderSubst (name, Just identity) (typeViewDisplay view) acc
        TypeViewSubstByName name ->
          case Map.lookup name bindersByName of
            Just Nothing -> insertTypeBinderSubst (name, Nothing) (typeViewDisplay view) acc
            Just (Just {}) -> acc
            Nothing -> insertTypeBinderSubst (name, Nothing) (typeViewDisplay view) acc

    identityNameOrDisplay displayName identityName
      | null identityName = displayName
      | otherwise = identityName

typeBinderSubstToNameMap :: TypeBinderSubst -> Map String SrcType
typeBinderSubstToNameMap subst =
  identityMap `Map.union` typeBinderSubstByName subst
  where
    identityMap =
      Map.fromList
        [ (name, ty)
        | (names, ty) <- Map.elems (typeBinderSubstByIdentity subst),
          name <- Set.toList names
        ]

typeBinderSubstToTypeViewSubstWith :: (SrcType -> TypeView) -> TypeBinderSubst -> TypeViewSubst
typeBinderSubstToTypeViewSubstWith mkView subst =
  Map.fromList (identityEntries ++ nameEntries)
  where
    identityEntries =
      [ (TypeViewSubstByIdentity identity name name, mkView ty)
        | (identity, (names, ty)) <- Map.toList (typeBinderSubstByIdentity subst),
          name <- Set.toList names
      ]
    nameEntries =
      [ (TypeViewSubstByName name, mkView ty)
        | (name, ty) <- Map.toList (typeBinderSubstByName subst)
      ]

lookupTypeBinderSubst :: (String, Maybe TypeBinderIdentity) -> TypeBinderSubst -> Maybe SrcType
lookupTypeBinderSubst (name, mbIdentity) subst =
  case mbIdentity of
    Just identity -> snd <$> Map.lookup identity (typeBinderSubstByIdentity subst)
    Nothing -> Map.lookup name (typeBinderSubstByName subst)

insertTypeBinderSubst :: (String, Maybe TypeBinderIdentity) -> SrcType -> TypeBinderSubst -> TypeBinderSubst
insertTypeBinderSubst (name, mbIdentity) ty subst =
  case mbIdentity of
    Just identity ->
      subst
        { typeBinderSubstByIdentity =
            Map.insertWith
              (\(newNames, newTy) (oldNames, _) -> (newNames <> oldNames, newTy))
              identity
              (Set.singleton name, ty)
              (typeBinderSubstByIdentity subst)
        }
    Nothing ->
      subst
        { typeBinderSubstByName = Map.insert name ty (typeBinderSubstByName subst)
        }

data EvidenceMethod = EvidenceMethod
  { evidenceMethodRuntimeName :: String,
    evidenceMethodSymbol :: SymbolIdentity,
    evidenceMethodResolvedVar :: Maybe ResolvedVar,
    evidenceMethodTypeView :: TypeView
  }
  deriving (Eq, Show)

data EvidenceInfo = EvidenceInfo
  { evidenceClassSymbol :: SymbolIdentity,
    evidenceTypeViews :: NonEmpty TypeView,
    evidenceMethodsByIdentity :: Map SymbolIdentity EvidenceMethod
  }
  deriving (Eq, Show)

data ConstructorShape = ConstructorShape
  { constructorShapeSymbol :: SymbolIdentity,
    constructorShapeRuntimeName :: String,
    constructorShapeForalls :: [(String, Maybe SrcType)],
    constructorShapeForallsIdentity :: [(String, Maybe SrcType)],
    constructorShapeForallBinderIdentities :: [Maybe TypeBinderIdentity],
    constructorShapeArgs :: [SrcType],
    constructorShapeArgsIdentity :: [SrcType],
    constructorShapeResult :: SrcType,
    constructorShapeResultIdentity :: SrcType,
    constructorShapeIndex :: Int,
    constructorShapeOwnerTypeParams :: [P.TypeParam]
  }
  deriving (Eq, Show)

data ConstructorInfo = ConstructorInfo
  { ctorInfoSymbol :: SymbolIdentity,
    ctorRuntimeName :: String,
    ctorType :: SrcType,
    ctorTypeIdentity :: SrcType,
    ctorForalls :: [(String, Maybe SrcType)],
    ctorForallBinderIdentities :: [Maybe TypeBinderIdentity],
    ctorArgs :: [SrcType],
    ctorResult :: SrcType,
    ctorOwningTypeIdentity :: SymbolIdentity,
    ctorIndex :: Int,
    ctorOwnerConstructors :: [ConstructorShape]
  }
  deriving (Eq, Show)

data DataInfo = DataInfo
  { dataInfoSymbol :: SymbolIdentity,
    dataTypeParams :: [P.TypeParam],
    dataConstructors :: [ConstructorInfo]
  }
  deriving (Eq, Show)

data MethodInfo = MethodInfo
  { methodInfoSymbol :: SymbolIdentity,
    methodType :: SrcType,
    methodTypeIdentity :: SrcType,
    methodTypeBinderIdentities :: Map String TypeBinderIdentity,
    methodConstraints :: [P.ClassConstraint],
    methodConstraintInfos :: [ConstraintInfo],
    methodParamNames :: NonEmpty String,
    methodParamIdentityNames :: NonEmpty String,
    methodParamBinderIdentities :: NonEmpty (Maybe TypeBinderIdentity)
  }
  deriving (Eq, Show)

data FunctionalDependencyInfo = FunctionalDependencyInfo
  { functionalDependencyDeterminerRefs :: NonEmpty TypeBinderIdentity,
    functionalDependencyDeterminedRefs :: NonEmpty TypeBinderIdentity
  }
  deriving (Eq, Show)

data ClassInfo = ClassInfo
  { classInfoSymbol :: SymbolIdentity,
    classTypeParams :: NonEmpty P.TypeParam,
    classSuperclasses :: [P.ClassConstraint],
    classSuperclassInfos :: [ConstraintInfo],
    classFunctionalDependencies :: [FunctionalDependencyInfo],
    classMethodsByIdentity :: Map SymbolIdentity MethodInfo
  }
  deriving (Eq, Show)

data ValueInfo
  = OrdinaryValue
      { valueInfoSymbol :: SymbolIdentity,
        valueRuntimeName :: String,
        valueType :: SrcType,
        valueIdentityType :: SrcType,
        valueConstraints :: [P.ClassConstraint],
        valueConstraintInfos :: [ConstraintInfo]
      }
  | ConstructorValue
      { valueInfoSymbol :: SymbolIdentity,
        valueRuntimeName :: String,
        valueType :: SrcType,
        valueIdentityType :: SrcType,
        valueCtorInfo :: ConstructorInfo
      }
  | OverloadedMethod
      { valueInfoSymbol :: SymbolIdentity,
        valueMethodInfo :: MethodInfo
      }
  deriving (Eq, Show)

data InstanceInfo = InstanceInfo
  { instanceClassSymbol :: SymbolIdentity,
    instanceOriginModuleIdentity :: SymbolIdentity,
    instanceConstraints :: [P.ClassConstraint],
    instanceConstraintInfos :: [ConstraintInfo],
    instanceHeadTypes :: NonEmpty SrcType,
    instanceHeadIdentityTypes :: NonEmpty SrcType,
    instanceMethodsByIdentity :: Map SymbolIdentity ValueInfo
  }
  deriving (Eq, Show)

data LoweredBindingIdentity = LoweredBindingIdentity
  { loweredIdentityRuntimeName :: String,
    loweredIdentityDetails :: IdDetails
  }
  deriving (Show)

instance Eq LoweredBindingIdentity where
  left == right =
    idDetailsSameIdentity (loweredIdentityDetails left) (loweredIdentityDetails right)

constructorRefFromInfo :: ConstructorInfo -> ConstructorRef
constructorRefFromInfo ctor =
  ConstructorRef
    { constructorRefSymbol = ctorInfoSymbol ctor
    }

loweredBindingIdentityFromConstructorInfo :: ConstructorInfo -> LoweredBindingIdentity
loweredBindingIdentityFromConstructorInfo ctor =
  LoweredBindingIdentity
    { loweredIdentityRuntimeName = ctorRuntimeName ctor,
      loweredIdentityDetails = ConstructorId (constructorRefFromInfo ctor)
    }

loweredBindingIdentityFromValueInfo :: ValueInfo -> LoweredBindingIdentity
loweredBindingIdentityFromValueInfo valueInfo =
  case valueInfo of
    OrdinaryValue
      { valueRuntimeName = runtimeName,
        valueInfoSymbol = symbol
      } ->
        LoweredBindingIdentity
          { loweredIdentityRuntimeName = runtimeName,
            loweredIdentityDetails = TopLevelId symbol
          }
    ConstructorValue {valueCtorInfo = ctor} ->
      loweredBindingIdentityFromConstructorInfo ctor
    OverloadedMethod
      { valueInfoSymbol = symbol
      } ->
      LoweredBindingIdentity
        { loweredIdentityRuntimeName = symbolDefiningName symbol,
          loweredIdentityDetails = MethodId symbol
        }

resolvedVarFromLoweredBinding :: LoweredBinding -> ElabType -> ResolvedVar
resolvedVarFromLoweredBinding lowered ty =
  ResolvedVar
    { resolvedVarRuntimeName = loweredIdentityRuntimeName identity,
      resolvedVarType = ty,
      resolvedVarDetails = loweredIdentityDetails identity
    }
  where
    identity = loweredBindingIdentity lowered

resolvedVarFromValueInfo :: ValueInfo -> ElabType -> ResolvedVar
resolvedVarFromValueInfo valueInfo ty =
  ResolvedVar
    { resolvedVarRuntimeName = loweredIdentityRuntimeName identity,
      resolvedVarType = ty,
      resolvedVarDetails = loweredIdentityDetails identity
    }
  where
    identity = loweredBindingIdentityFromValueInfo valueInfo

data DeferredBindingMode
  = DeferredBindingScheme
  | DeferredBindingMonomorphic
  deriving (Eq, Show)

data DeferredMethodEvidence = DeferredMethodEvidence
  { deferredMethodEvidenceClassArg :: TypeView,
    deferredMethodEvidenceClassArgs :: NonEmpty TypeView,
    deferredMethodEvidenceMethod :: EvidenceMethod
  }
  deriving (Eq, Show)

data DeferredMethodCall = DeferredMethodCall
  { deferredMethodRef :: DeferredRef,
    deferredMethodInfo :: MethodInfo,
    deferredMethodArgCount :: Int,
    deferredMethodFullArity :: Int,
    deferredMethodExpectedResult :: Maybe TypeView,
    deferredMethodEvidence :: Maybe DeferredMethodEvidence,
    deferredMethodLocalEvidence :: [EvidenceInfo]
  }
  deriving (Eq, Show)

deferredMethodPlaceholder :: DeferredMethodCall -> String
deferredMethodPlaceholder =
  deferredRefName . deferredMethodRef

deferredMethodName :: DeferredMethodCall -> P.MethodName
deferredMethodName =
  methodName . deferredMethodInfo

data DeferredConstructorCall = DeferredConstructorCall
  { deferredConstructorRef :: DeferredRef,
    deferredConstructorInfo :: ConstructorInfo,
    deferredConstructorArgCount :: Int,
    deferredConstructorSourceType :: SrcType,
    deferredConstructorOccurrenceType :: SrcType,
    deferredConstructorInstBinders :: [(String, Maybe TypeBinderIdentity)],
    deferredConstructorInitialSubst :: TypeBinderSubst,
    deferredConstructorBindingMode :: DeferredBindingMode
  }
  deriving (Eq, Show)

deferredConstructorPlaceholder :: DeferredConstructorCall -> String
deferredConstructorPlaceholder =
  deferredRefName . deferredConstructorRef

data DeferredCaseCall = DeferredCaseCall
  { deferredCaseRef :: DeferredRef,
    deferredCaseDataInfo :: DataInfo,
    deferredCaseScrutineeType :: SrcType,
    deferredCaseResultType :: SrcType,
    deferredCaseExpectedArgCount :: Int
  }
  deriving (Eq, Show)

deferredCasePlaceholder :: DeferredCaseCall -> String
deferredCasePlaceholder =
  deferredRefName . deferredCaseRef

data DeferredProgramObligation
  = DeferredMethod DeferredMethodCall
  | DeferredConstructor DeferredConstructorCall
  | DeferredCase DeferredCaseCall
  deriving (Eq, Show)

type DeferredObligations = Map DeferredRef DeferredProgramObligation

deferredProgramObligationRef :: DeferredProgramObligation -> DeferredRef
deferredProgramObligationRef obligation =
  case obligation of
    DeferredMethod deferred -> deferredMethodRef deferred
    DeferredConstructor deferred -> deferredConstructorRef deferred
    DeferredCase deferred -> deferredCaseRef deferred

data ExportedTypeInfo = ExportedTypeInfo
  { exportedTypeData :: DataInfo,
    exportedTypeConstructorsByIdentity :: Map SymbolIdentity ConstructorInfo,
    exportedTypeConstructorDisplaysByIdentity :: Map SymbolIdentity String
  }
  deriving (Eq, Show)

mkExportedTypeInfo :: DataInfo -> [(String, ConstructorInfo)] -> ExportedTypeInfo
mkExportedTypeInfo dataInfo constructors =
  ExportedTypeInfo
    { exportedTypeData = dataInfo,
      exportedTypeConstructorsByIdentity =
        Map.fromList [(ctorInfoSymbol ctorInfo, ctorInfo) | (_, ctorInfo) <- constructors],
      exportedTypeConstructorDisplaysByIdentity =
        Map.fromList [(ctorInfoSymbol ctorInfo, displayName) | (displayName, ctorInfo) <- constructors]
    }

exportedTypeConstructorsForDisplay :: ExportedTypeInfo -> Map String ConstructorInfo
exportedTypeConstructorsForDisplay typeInfo =
  Map.fromList
    [ (displayName, ctorInfo)
    | (identity, ctorInfo) <- Map.toList (exportedTypeConstructorsByIdentity typeInfo),
      Just displayName <- [Map.lookup identity (exportedTypeConstructorDisplaysByIdentity typeInfo)]
    ]

data ModuleExports = ModuleExports
  { exportedValuesByIdentity :: Map SymbolIdentity ValueInfo,
    exportedValueDisplaysByIdentity :: Map SymbolIdentity String,
    exportedTypesByIdentity :: Map SymbolIdentity ExportedTypeInfo,
    exportedTypeDisplaysByIdentity :: Map SymbolIdentity String,
    exportedClassesByIdentity :: Map SymbolIdentity ClassInfo,
    exportedClassDisplaysByIdentity :: Map SymbolIdentity String
  }
  deriving (Eq, Show)

moduleExportsFromMaps ::
  Map String ValueInfo ->
  Map String ExportedTypeInfo ->
  Map String ClassInfo ->
  ModuleExports
moduleExportsFromMaps values0 types0 classes0 =
  ModuleExports
    { exportedValuesByIdentity = indexInfo valueInfoSymbolIdentity values0,
      exportedValueDisplaysByIdentity = indexDisplay valueInfoSymbolIdentity values0,
      exportedTypesByIdentity = indexInfo (dataInfoSymbolIdentity . exportedTypeData) types0,
      exportedTypeDisplaysByIdentity = indexDisplay (dataInfoSymbolIdentity . exportedTypeData) types0,
      exportedClassesByIdentity = indexInfo classInfoSymbolIdentity classes0,
      exportedClassDisplaysByIdentity = indexDisplay classInfoSymbolIdentity classes0
    }
  where
    indexInfo identityFor values =
      Map.fromListWith
        (flip const)
        [ (identityFor info, info)
        | (_, info) <- Map.toList values
        ]

    indexDisplay identityFor values =
      Map.fromListWith
        (flip const)
        [ (identityFor info, name)
        | (name, info) <- Map.toList values
        ]

exportedValuesForDisplay :: ModuleExports -> Map String ValueInfo
exportedValuesForDisplay exports =
  displayMap (exportedValuesByIdentity exports) (exportedValueDisplaysByIdentity exports)

exportedTypesForDisplay :: ModuleExports -> Map String ExportedTypeInfo
exportedTypesForDisplay exports =
  displayMap (exportedTypesByIdentity exports) (exportedTypeDisplaysByIdentity exports)

exportedClassesForDisplay :: ModuleExports -> Map String ClassInfo
exportedClassesForDisplay exports =
  displayMap (exportedClassesByIdentity exports) (exportedClassDisplaysByIdentity exports)

displayMap :: Map SymbolIdentity a -> Map SymbolIdentity String -> Map String a
displayMap values displays =
  Map.fromList
    [ (displayName, value)
    | (identity, value) <- Map.toList values,
      Just displayName <- [Map.lookup identity displays]
    ]

data LoweredBinding = LoweredBinding
  { loweredBindingIdentity :: LoweredBindingIdentity,
    loweredBindingSourceType :: SrcType,
    loweredBindingExpectedType :: SrcType,
    loweredBindingSurfaceExpr :: SurfaceExpr,
    loweredBindingDeferredObligations :: DeferredObligations,
    loweredBindingExternalTypes :: Map String SrcType,
    loweredBindingEvidenceParamCount :: Int,
    loweredBindingExportedAsMain :: Bool
  }
  deriving (Eq, Show)

loweredBindingName :: LoweredBinding -> String
loweredBindingName =
  loweredIdentityRuntimeName . loweredBindingIdentity

loweredBindingConstructorRef :: LoweredBinding -> Maybe ConstructorRef
loweredBindingConstructorRef =
  idDetailsConstructorRef . loweredIdentityDetails . loweredBindingIdentity

data CheckedBinding = CheckedBinding
  { checkedBindingResolvedVar :: ResolvedVar,
    checkedBindingSourceType :: SrcType,
    checkedBindingSurfaceExpr :: SurfaceExpr,
    checkedBindingDeferredObligations :: DeferredObligations,
    checkedBindingTerm :: XmlfTerm,
    checkedBindingType :: ElabType,
    checkedBindingExportedAsMain :: Bool
  }
  deriving (Eq, Show)

checkedBindingName :: CheckedBinding -> String
checkedBindingName =
  resolvedVarRuntimeName . checkedBindingResolvedVar

checkedBindingConstructorRef :: CheckedBinding -> Maybe ConstructorRef
checkedBindingConstructorRef = resolvedVarConstructorRef . checkedBindingResolvedVar

data CheckedModule = CheckedModule
  { checkedModuleName :: P.ModuleName,
    checkedModuleIdentity :: SymbolIdentity,
    checkedModuleBindings :: [CheckedBinding],
    checkedModuleData :: Map SymbolIdentity DataInfo,
    checkedModuleClasses :: Map SymbolIdentity ClassInfo,
    checkedModuleInstances :: [InstanceInfo],
    checkedModuleExports :: ModuleExports
  }
  deriving (Eq, Show)

data CheckedProgram = CheckedProgram
  { checkedProgramModules :: [CheckedModule],
    checkedProgramMainResolvedVar :: ResolvedVar,
    checkedProgramResolved :: ResolvedProgram
  }
  deriving (Eq, Show)

checkedProgramMain :: CheckedProgram -> String
checkedProgramMain =
  resolvedVarRuntimeName . checkedProgramMainResolvedVar

valueInfoSymbolIdentity :: ValueInfo -> SymbolIdentity
valueInfoSymbolIdentity = valueInfoSymbol

valueInfoIdentityName :: ValueInfo -> P.ValueName
valueInfoIdentityName =
  symbolDefiningName . valueInfoSymbolIdentity

dataInfoSymbolIdentity :: DataInfo -> SymbolIdentity
dataInfoSymbolIdentity = dataInfoSymbol

dataInfoIdentityModule :: DataInfo -> P.ModuleName
dataInfoIdentityModule =
  symbolDefiningModule . dataInfoSymbolIdentity

dataInfoIdentityName :: DataInfo -> P.TypeName
dataInfoIdentityName =
  symbolDefiningName . dataInfoSymbolIdentity

dataName :: DataInfo -> P.TypeName
dataName =
  dataInfoIdentityName

dataInfoIdentityQualifiedName :: DataInfo -> String
dataInfoIdentityQualifiedName info =
  dataInfoIdentityModule info ++ "." ++ dataInfoIdentityName info

dataInfoIdentityHeadName :: DataInfo -> String
dataInfoIdentityHeadName =
  dataInfoIdentityQualifiedName

dataParams :: DataInfo -> [String]
dataParams =
  map P.typeParamName . dataTypeParams

constructorInfoSymbolIdentity :: DataInfo -> ConstructorInfo -> SymbolIdentity
constructorInfoSymbolIdentity _ = ctorInfoSymbol

constructorInfoIdentityName :: ConstructorInfo -> String
constructorInfoIdentityName =
  symbolDefiningName . ctorInfoSymbol

ctorName :: ConstructorInfo -> P.ConstructorName
ctorName =
  constructorInfoIdentityName

ctorOwningType :: ConstructorInfo -> P.TypeName
ctorOwningType =
  symbolDefiningName . ctorOwningTypeIdentity

constructorOwnerRuntimeTypeTrackable :: Map SymbolIdentity [DataInfo] -> ConstructorInfo -> Bool
constructorOwnerRuntimeTypeTrackable dataInfosByIdentity ctor =
  case Map.lookup (ctorOwningTypeIdentity ctor) dataInfosByIdentity of
    Just (dataInfo : _) -> dataConstructorsRuntimeTypeTrackable dataInfo
    Just [] -> all constructorShapeRuntimeTypeTrackable (constructorOwnerShapes ctor)
    Nothing -> all constructorShapeRuntimeTypeTrackable (constructorOwnerShapes ctor)

constructorOwnerHasVariableHeadApplication :: Map SymbolIdentity [DataInfo] -> ConstructorInfo -> Bool
constructorOwnerHasVariableHeadApplication dataInfosByIdentity ctor =
  case Map.lookup (ctorOwningTypeIdentity ctor) dataInfosByIdentity of
    Just (dataInfo : _) -> any constructorRuntimeTypeHasVariableHeadApplication (dataConstructors dataInfo)
    Just [] -> any constructorShapeHasVariableHeadApplication (constructorOwnerShapes ctor)
    Nothing -> any constructorShapeHasVariableHeadApplication (constructorOwnerShapes ctor)

dataConstructorsRuntimeTypeTrackable :: DataInfo -> Bool
dataConstructorsRuntimeTypeTrackable =
  all constructorRuntimeTypeShapeTrackable . dataConstructors

constructorRuntimeTypeShapeTrackable :: ConstructorInfo -> Bool
constructorRuntimeTypeShapeTrackable ctor =
  constructorShapeRuntimeTypeTrackable (constructorShapeFromInfo ctor)

constructorShapeRuntimeTypeTrackable :: ConstructorShape -> Bool
constructorShapeRuntimeTypeTrackable shape =
  let involvedTypes =
        constructorShapeArgs shape
          ++ [constructorShapeResult shape]
          ++ [bound | (_, Just bound) <- constructorShapeForalls shape]
      evidenceVars = foldMap freeTypeVarsSrcType involvedTypes
   in not (any hasVariableHeadApplication involvedTypes)
        && all (\(name, _) -> name `Set.member` evidenceVars) (constructorShapeForalls shape)

constructorRuntimeTypeHasVariableHeadApplication :: ConstructorInfo -> Bool
constructorRuntimeTypeHasVariableHeadApplication ctor =
  constructorShapeHasVariableHeadApplication (constructorShapeFromInfo ctor)

constructorShapeHasVariableHeadApplication :: ConstructorShape -> Bool
constructorShapeHasVariableHeadApplication shape =
  any hasVariableHeadApplication $
    constructorShapeArgs shape
      ++ [constructorShapeResult shape]
      ++ [bound | (_, Just bound) <- constructorShapeForalls shape]

constructorOwnerShapes :: ConstructorInfo -> [ConstructorShape]
constructorOwnerShapes ctor =
  case ctorOwnerConstructors ctor of
    [] -> [constructorShapeFromInfo ctor]
    shapes -> shapes

constructorShapeFromInfo :: ConstructorInfo -> ConstructorShape
constructorShapeFromInfo ctor =
  ConstructorShape
    { constructorShapeSymbol = ctorInfoSymbol ctor,
      constructorShapeRuntimeName = ctorRuntimeName ctor,
      constructorShapeForalls = ctorForalls ctor,
      constructorShapeForallsIdentity = identityForalls,
      constructorShapeForallBinderIdentities = ctorForallBinderIdentities ctor,
      constructorShapeArgs = ctorArgs ctor,
      constructorShapeArgsIdentity = identityArgs,
      constructorShapeResult = ctorResult ctor,
      constructorShapeResultIdentity = identityResult,
      constructorShapeIndex = ctorIndex ctor,
      constructorShapeOwnerTypeParams = []
    }
  where
    (identityForalls, identityBody) = splitForalls (ctorTypeIdentity ctor)
    (identityArgs, identityResult) = splitArrows identityBody

constructorShapeName :: ConstructorShape -> P.ConstructorName
constructorShapeName =
  symbolDefiningName . constructorShapeSymbol

srcTypeHasVariableHeadApplication :: SrcType -> Bool
srcTypeHasVariableHeadApplication = hasVariableHeadApplication

hasVariableHeadApplication :: SrcType -> Bool
hasVariableHeadApplication ty =
  case ty of
    STVar {} -> False
    STArrow dom cod -> hasVariableHeadApplication dom || hasVariableHeadApplication cod
    STBase {} -> False
    STCon _ args -> any hasVariableHeadApplication args
    STVarApp {} -> True
    STTyLam _ body -> hasVariableHeadApplication body
    STTyApp fun arg -> hasVariableHeadApplication fun || hasVariableHeadApplication arg
    STForall _ mb body ->
      maybe False (hasVariableHeadApplication . unSrcBound) mb
        || hasVariableHeadApplication body
    STMu _ body -> hasVariableHeadApplication body
    STBottom -> False

classInfoSymbolIdentity :: ClassInfo -> SymbolIdentity
classInfoSymbolIdentity = classInfoSymbol

classInfoIdentityModule :: ClassInfo -> P.ModuleName
classInfoIdentityModule =
  symbolDefiningModule . classInfoSymbolIdentity

classInfoIdentityName :: ClassInfo -> P.ClassName
classInfoIdentityName =
  symbolDefiningName . classInfoSymbolIdentity

className :: ClassInfo -> P.ClassName
className =
  classInfoIdentityName

classInfoIdentityQualifiedName :: ClassInfo -> String
classInfoIdentityQualifiedName info =
  classInfoIdentityModule info ++ "." ++ classInfoIdentityName info

classParamNames :: ClassInfo -> NonEmpty String
classParamNames =
  fmap P.typeParamName . classTypeParams

classParamIdentityNames :: ClassInfo -> NonEmpty String
classParamIdentityNames =
  fmap P.typeParamIdentityName . classTypeParams

classParamBinderIdentities :: ClassInfo -> NonEmpty (Maybe TypeBinderIdentity)
classParamBinderIdentities =
  fmap typeParamBinderIdentity . classTypeParams

methodInfoSymbolIdentity :: MethodInfo -> SymbolIdentity
methodInfoSymbolIdentity = methodInfoSymbol

methodInfoIdentityName :: MethodInfo -> P.MethodName
methodInfoIdentityName =
  symbolDefiningName . methodInfoSymbolIdentity

methodClassName :: MethodInfo -> P.ClassName
methodClassName =
  symbolDefiningName . methodInfoOwnerClassSymbolIdentity

methodName :: MethodInfo -> P.MethodName
methodName =
  methodInfoIdentityName

methodParamName :: MethodInfo -> String
methodParamName =
  NE.head . methodParamNames

methodParamIdentityName :: MethodInfo -> String
methodParamIdentityName =
  NE.head . methodParamIdentityNames

methodParamBinders :: MethodInfo -> NonEmpty (String, String, Maybe TypeBinderIdentity)
methodParamBinders methodInfo =
  NE.fromList $
    zipWith3
      (\displayName identityName binderIdentity -> (displayName, identityName, binderIdentity))
      (NE.toList (methodParamNames methodInfo))
      (NE.toList (methodParamIdentityNames methodInfo))
      (NE.toList (methodParamBinderIdentities methodInfo))

lookupMethodParamViewSubst :: MethodInfo -> TypeViewSubst -> Maybe (NonEmpty TypeView)
lookupMethodParamViewSubst methodInfo subst =
  traverse lookupParam (methodParamBinders methodInfo)
  where
    lookupParam (displayName, identityName, mbIdentity) =
      case mbIdentity of
        Just identity -> lookupTypeViewSubst (TypeViewSubstByIdentity identity displayName identityName) subst
        Nothing ->
          lookupTypeViewSubst (TypeViewSubstByName identityName) subst
            <|> lookupTypeViewSubst (TypeViewSubstByName displayName) subst

methodTypeView :: MethodInfo -> TypeView
methodTypeView methodInfo =
  TypeView
    { typeViewDisplay = methodType methodInfo,
      typeViewIdentity = methodTypeIdentity methodInfo,
      typeViewBinderIdentities =
        methodTypeBinderIdentities methodInfo
          <> Map.fromList
            [ (identityName, identity)
            | (_, identityName, Just identity) <- NE.toList (methodParamBinders methodInfo)
            ]
    }

methodResultTypeView :: MethodInfo -> TypeView
methodResultTypeView methodInfo =
  TypeView
    { typeViewDisplay = displayResult,
      typeViewIdentity = identityResult,
      typeViewBinderIdentities = typeViewBinderIdentities view
    }
  where
    view = methodTypeView methodInfo
    (_, displayBody) = splitForalls (typeViewDisplay view)
    (_, identityBody) = splitForalls (typeViewIdentity view)
    (_, displayResult) = splitArrows displayBody
    (_, identityResult) = splitArrows identityBody

methodInfoOwnerClassSymbolIdentity :: MethodInfo -> SymbolIdentity
methodInfoOwnerClassSymbolIdentity methodInfo =
  case symbolOwnerIdentity (methodInfoSymbolIdentity methodInfo) of
    Just (SymbolOwnerClass classIdentity) ->
      classIdentity
    _ ->
      error
        ( "methodInfoOwnerClassSymbolIdentity: method symbol missing class owner identity: "
            ++ show (methodInfoSymbolIdentity methodInfo)
        )

instanceInfoClassSymbolIdentity :: InstanceInfo -> SymbolIdentity
instanceInfoClassSymbolIdentity = instanceClassSymbol

instanceClassName :: InstanceInfo -> P.ClassName
instanceClassName =
  symbolDefiningName . instanceInfoClassSymbolIdentity

instanceOriginModuleName :: InstanceInfo -> P.ModuleName
instanceOriginModuleName =
  symbolDefiningModule . instanceOriginModuleIdentity

lookupClassMethod :: ResolvedSymbol -> ClassInfo -> Maybe MethodInfo
lookupClassMethod symbol classInfo =
  Map.lookup (resolvedSymbolIdentity symbol) (classMethodsByIdentity classInfo)

lookupInstanceMethod :: MethodInfo -> InstanceInfo -> Maybe ValueInfo
lookupInstanceMethod methodInfo instanceInfo =
  Map.lookup (methodInfoSymbolIdentity methodInfo) (instanceMethodsByIdentity instanceInfo)

resolvedValueInfoSymbol :: SymbolOrigin -> String -> ValueInfo -> ResolvedSymbol
resolvedValueInfoSymbol origin displayName valueInfo =
  mkResolvedSymbol
    (valueInfoSymbolIdentity valueInfo)
    (valueInfoIdentityName valueInfo)
    displayName
    origin

resolvedDataInfoSymbol :: SymbolOrigin -> String -> DataInfo -> ResolvedSymbol
resolvedDataInfoSymbol origin displayName dataInfo =
  mkResolvedSymbol
    (dataInfoSymbolIdentity dataInfo)
    (dataInfoIdentityName dataInfo)
    displayName
    origin

resolvedConstructorInfoSymbol :: SymbolOrigin -> String -> DataInfo -> ConstructorInfo -> ResolvedSymbol
resolvedConstructorInfoSymbol origin displayName dataInfo ctorInfo =
  mkResolvedSymbol
    (constructorInfoSymbolIdentity dataInfo ctorInfo)
    (constructorInfoIdentityName ctorInfo)
    displayName
    origin

resolvedClassInfoSymbol :: SymbolOrigin -> String -> ClassInfo -> ResolvedSymbol
resolvedClassInfoSymbol origin displayName classInfo =
  mkResolvedSymbol
    (classInfoSymbolIdentity classInfo)
    (classInfoIdentityName classInfo)
    displayName
    origin

resolvedMethodInfoSymbol :: SymbolOrigin -> String -> MethodInfo -> ResolvedSymbol
resolvedMethodInfoSymbol origin displayName methodInfo =
  mkResolvedSymbol
    (methodInfoSymbolIdentity methodInfo)
    (methodInfoIdentityName methodInfo)
    displayName
    origin

resolvedModuleSymbol :: SymbolOrigin -> UniqueIdentity -> P.ModuleName -> P.ModuleName -> ResolvedSymbol
resolvedModuleSymbol origin identity definingModule displayName =
  resolvedModuleSymbolFromIdentity origin (moduleSymbolIdentity identity definingModule) displayName

resolvedModuleSymbolFromIdentity :: SymbolOrigin -> SymbolIdentity -> P.ModuleName -> ResolvedSymbol
resolvedModuleSymbolFromIdentity origin identity displayName =
  mkResolvedSymbol
    identity
    (symbolDefiningName identity)
    displayName
    origin

moduleSymbolIdentity :: UniqueIdentity -> P.ModuleName -> SymbolIdentity
moduleSymbolIdentity identity moduleName =
  SymbolIdentity
    { symbolUniqueIdentity = identity,
      symbolNamespace = SymbolModule,
      symbolDefiningModule = moduleName,
      symbolDefiningName = moduleName,
      symbolOwnerIdentity = Nothing
    }

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

applyTypeHead :: SrcType -> [SrcType] -> Maybe SrcType
applyTypeHead headTy args =
  case headTy of
    STVar name -> Just (mkVarHead name args)
    STBase name -> Just (mkConHead name args)
    STCon name existingArgs -> Just (mkConHead name (toList existingArgs ++ args))
    STVarApp name existingArgs -> Just (mkVarHead name (toList existingArgs ++ args))
    _ -> Nothing
  where
    mkVarHead name = \case
      [] -> STVar name
      arg : rest -> STVarApp name (arg :| rest)

    mkConHead name = \case
      [] -> STBase name
      arg : rest -> STCon name (arg :| rest)

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
      STVarApp name args ->
        let args' = fmap go args
         in case replacementHead name (toList args') of
              Just ty' -> ty'
              Nothing -> STVarApp name args'
      STTyLam name body
        | name == needle -> STTyLam name body
        | otherwise -> STTyLam name (go body)
      STTyApp fun arg -> STTyApp (go fun) (go arg)
      STForall name mb body
        | name == needle -> STForall name mb body
        | otherwise -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
      STMu name body
        | name == needle -> STMu name body
        | otherwise -> STMu name (go body)
      STBottom -> STBottom

    replacementHead name args
      | name /= needle = Nothing
      | otherwise = applyTypeHead replacement args

freeTypeVarsSrcType :: SrcType -> Set String
freeTypeVarsSrcType = go Set.empty
  where
    go bound ty = case ty of
      STVar name
        | name `Set.member` bound -> Set.empty
        | otherwise -> Set.singleton name
      STArrow dom cod -> go bound dom `Set.union` go bound cod
      STBase {} -> Set.empty
      STCon _ args -> foldMap (go bound) args
      STVarApp name args ->
        let headVars =
              if name `Set.member` bound
                then Set.empty
                else Set.singleton name
         in headVars `Set.union` foldMap (go bound) args
      STTyLam name body -> go (Set.insert name bound) body
      STTyApp fun arg -> go bound fun `Set.union` go bound arg
      STForall name mb body ->
        maybe Set.empty (go bound . unSrcBound) mb `Set.union` go (Set.insert name bound) body
      STMu name body -> go (Set.insert name bound) body
      STBottom -> Set.empty

specializeMethodType :: SrcType -> String -> SrcType -> SrcType
specializeMethodType methodTy paramName headTy =
  specializeMethodTypes methodTy (paramName :| []) (headTy :| [])

specializeMethodTypes :: SrcType -> NonEmpty String -> NonEmpty SrcType -> SrcType
specializeMethodTypes methodTy paramNames headTys =
  let (foralls, body) = splitForalls methodTy
      subst = Map.fromList (zip (NE.toList paramNames) (NE.toList headTys))
      rebuilt = foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) (Map.foldrWithKey substituteTypeVar body subst) foralls
   in rebuilt

specializeMethodTypeView :: MethodInfo -> NonEmpty TypeView -> TypeView
specializeMethodTypeView methodInfo classArgViews =
  TypeView
    { typeViewDisplay = specialize (typeViewDisplay view) (typeViewSubstDisplayTypes subst),
      typeViewIdentity = specialize (typeViewIdentity view) (typeViewSubstIdentityTypes subst),
      typeViewBinderIdentities = typeViewBinderIdentities view
    }
  where
    view = methodTypeView methodInfo
    subst = typeViewSubstFromParamBinders (methodParamBinders methodInfo) classArgViews

    specialize ty substMap =
      let (foralls, body) = splitForalls ty
          rebuilt = Map.foldrWithKey substituteTypeVar body substMap
       in foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) rebuilt foralls

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

    constraintFreeVars constraint = foldMap freeVars (P.constraintTypes constraint)

    freeVars ty = case ty of
      STVar name -> [name]
      STArrow dom cod -> freeVars dom ++ freeVars cod
      STBase {} -> []
      STCon _ args -> concatMap freeVars (toList args)
      STVarApp name args -> name : concatMap freeVars (toList args)
      STTyLam name body -> filter (/= name) (freeVars body)
      STTyApp fun arg -> freeVars fun ++ freeVars arg
      STForall name mb body ->
        filter (/= name) (maybe [] (freeVars . unSrcBound) mb ++ freeVars body)
      STMu name body -> filter (/= name) (freeVars body)
      STBottom -> []
