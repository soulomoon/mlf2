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
    freeTypeBinderIdentitiesTypeView,
    freeTypeBinderIdentitiesTypeViews,
    typeViewIsBareBinderIdentity,
    typeViewMentionsFreeBinderIdentity,
    typeViewHeadIdentityForAlias,
    typeViewBinderIdentityForAlias,
    typeViewBinderIdentityAliasEntries,
    filterBinderIdentitiesByNames,
    filterHeadIdentitiesByNames,
    typeViewMentionedHeadIdentities,
    typeViewVarPairs,
    typeViewHeadPairs,
    typeHeadNamesSrcType,
    typeViewsDisplay,
    typeViewsIdentity,
    TypeViewSubstKey,
    TypeViewSubst,
    typeViewSubstKeyForIdentity,
    typeViewSubstKeyIdentity,
    typeViewSubstKeyFor,
    lookupTypeViewSubst,
    insertTypeViewSubst,
    typeViewSubstDisplayTypes,
    typeViewSubstFromParamIdentities,
    typeParamBinderIdentity,
    mergeUniquePairMaps,
    TypeBinderSubst,
    emptyTypeBinderSubst,
    typeBinderSubstFromTypeViewSubst,
    applyTypeBinderSubst,
    typeBinderSubstToTypeViewSubstWith,
    lookupTypeBinderSubstByIdentity,
    insertTypeBinderSubstWithIdentity,
    EvidenceMethod (..),
    uniqueEvidenceMethod,
    uniqueEvidenceMethodMatch,
    EvidenceInfo (..),
    SymbolNamespace (..),
    SymbolOwnerIdentity (..),
    SymbolIdentity,
    symbolIdentityFromParts,
    symbolIdentityWithUnique,
    symbolUniqueIdentity,
    symbolNamespace,
    symbolDefiningModule,
    symbolDefiningName,
    symbolOwnerIdentity,
    renameSymbolDefiningName,
    SymbolOrigin (..),
    SymbolSpelling (..),
    ResolvedSymbol,
    resolvedSymbolIdentity,
    resolvedSymbolSpelling,
    mapResolvedSymbolIdentity,
    ResolvedReferenceKind (..),
    ResolvedReference,
    resolvedReferenceKind,
    resolvedReferenceName,
    resolvedReferenceSymbol,
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
    mkResolvedReference,
    sameResolvedSymbol,
    unqualifiedSymbolName,
    resolvedProgramGeneratedIdentities,
    resolvedModuleGeneratedIdentities,
    resolvedDeclGeneratedIdentities,
    resolvedConstrainedTypeGeneratedIdentities,
    resolvedExprGeneratedIdentities,
    resolvedSrcTypeGeneratedIdentities,
    resolvedTypeBinderGeneratedIdentities,
    typeParamGeneratedIdentities,
    typeViewGeneratedIdentities,
    constraintInfoGeneratedIdentities,
    constructorForallBinderGeneratedIdentities,
    constructorShapeGeneratedIdentities,
    constructorInfoGeneratedIdentities,
    dataInfoGeneratedIdentities,
    functionalDependencyGeneratedIdentities,
    methodInfoGeneratedIdentities,
    classInfoGeneratedIdentities,
    valueInfoGeneratedIdentities,
    instanceInfoGeneratedIdentities,
    evidenceMethodGeneratedIdentities,
    evidenceInfoGeneratedIdentities,
    deferredMethodEvidenceGeneratedIdentities,
    deferredProgramObligationGeneratedIdentities,
    loweredBindingIdentityGeneratedIdentities,
    valueInfoSymbolIdentity,
    valueInfoIdentityName,
    valueType,
    valueIdentityType,
    ordinaryValueTypeView,
    dataInfoSymbolIdentity,
    dataName,
    dataInfoIdentityModule,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataInfoIdentityHeadName,
    dataParams,
    dataParamBinderIdentities,
    dataParamBinders,
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
    classParamBinderIdentities,
    classParamBinders,
    methodInfoSymbolIdentity,
    methodInfoIdentityName,
    methodClassName,
    methodName,
    methodParamNames,
    methodParamName,
    methodParamBinderIdentities,
    methodType,
    methodTypeIdentity,
    lookupMethodParamViewSubst,
    methodTypeView,
    typeViewArrowArgViews,
    typeViewDirectArrowDomainView,
    typeViewDirectArrowCodomainView,
    typeViewArrowResultView,
    typeViewArrowResultViewForArity,
    methodParamTypeViews,
    methodResultTypeViewFrom,
    methodResultTypeView,
    methodInfoOwnerClassSymbolIdentity,
    instanceClassName,
    instanceInfoClassSymbolIdentity,
    instanceOriginModuleName,
    instanceHeadTypes,
    instanceHeadIdentityTypes,
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
    ConstructorForallBinder (..),
    ConstructorShape (..),
    constructorShapeType,
    constructorShapeTypeIdentity,
    constructorShapeForalls,
    constructorShapeForallsIdentity,
    constructorShapeArgViews,
    constructorShapeResultView,
    constructorShapeArgs,
    constructorShapeArgsIdentity,
    constructorShapeResult,
    constructorShapeResultIdentity,
    ConstructorInfo (..),
    ctorType,
    ctorTypeIdentity,
    ctorForalls,
    ctorArgs,
    constructorInfoArgViews,
    constructorInfoResultView,
    ctorResult,
    DataInfo (..),
    FunctionalDependencyInfo (..),
    MethodInfo (..),
    ClassInfo (..),
    ValueInfo (..),
    InstanceInfo (..),
    LocalRef,
    localRefFromIdentity,
    localRefIdentity,
    localRefName,
    localRefDiscard,
    PrimitiveRef,
    primitiveRefFromSymbol,
    primitiveRefSymbol,
    DeferredRef,
    deferredRefFromIdentity,
    deferredRefIdentity,
    deferredRefName,
    ConstructorRef,
    constructorRefFromSymbol,
    constructorRefSymbol,
    IdDetails (..),
    LoweredBindingIdentity,
    loweredIdentityRuntimeName,
    loweredIdentityDetails,
    loweredBindingIdentityFromResolvedVar,
    ResolvedVar (..),
    constructorRefFromInfo,
    loweredBindingIdentityFromConstructorInfo,
    loweredBindingIdentityFromValueInfo,
    resolvedVarFromValueInfo,
    resolvedVarFromLoweredBinding,
    loweredBindingConstructorRef,
    checkedBindingConstructorRef,
    LoweredResolvedLocalIdentity (..),
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
    uniqueInfoEntriesByIdentity,
    uniqueInfoListByIdentity,
    uniqueInfoByIdentity,
    uniqueDisplayByIdentity,
    exportedValuesForDisplay,
    exportedTypesForDisplay,
    exportedClassesForDisplay,
    LoweredBinding (..),
    loweredBindingName,
    CheckedBinding (..),
    checkedBindingName,
    checkedBindingSourceType,
    checkedBindingSourceTypeIdentity,
    CheckedModule (..),
    CheckedProgram (..),
    checkedProgramMain,
    splitForalls,
    splitArrows,
    applyTypeHead,
    substituteTypeVar,
    mergeSymbolIdentityMaps,
    mergeTypeBinderIdentityMaps,
    typeBinderAliasIdentityMap,
    constructorOwnerRuntimeTypeTrackable,
    constructorOwnerHasVariableHeadApplication,
    constructorOwnerShapes,
    constructorOwnerDataInfoFromShapes,
    constructorShapeName,
    constructorShapeFromInfo,
    dataConstructorsRuntimeTypeTrackable,
    srcTypeHasVariableHeadApplication,
    specializeMethodTypeView,
    constrainedVisibleType,
    constrainedVisibleTypeView,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (nub, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Elab.Types (XmlfTerm, ElabType, ResolvedVar (..), generatedIdentitiesInType, resolvedVarConstructorRef)
import MLF.Frontend.Symbol
  ( ResolvedReference,
    ResolvedReferenceKind (..),
    ResolvedSymbol,
    SymbolIdentity,
    SymbolNamespace (..),
    SymbolOrigin (..),
    SymbolOwnerIdentity (..),
    SymbolSpelling (..),
    mapResolvedSymbolIdentity,
    mkResolvedReference,
    mkResolvedSymbol,
    renameSymbolDefiningName,
    resolvedReferenceKind,
    resolvedReferenceName,
    resolvedReferenceSymbol,
    resolvedSymbolIdentity,
    resolvedSymbolSpelling,
    sameResolvedSymbol,
    symbolIdentityAliasMap,
    symbolIdentityAliasMapWith,
    symbolIdentityAliasNamesWith,
    symbolIdentityStableName,
    symbolDefiningModule,
    symbolDefiningName,
    symbolIdentityFromParts,
    symbolIdentityWithUnique,
    symbolNamespace,
    symbolOwnerIdentity,
    symbolUniqueIdentity,
    unqualifiedSymbolName,
  )
import MLF.Frontend.Syntax
  ( ResolvedSrcType,
    ResolvedSrcTy (..),
    ResolvedSrcBound (..),
    ResolvedTypeBinderRef,
    SrcBound (..),
    SrcTy (..),
    SrcType,
    SurfaceExpr,
    resolvedSrcTypeIdentityType,
    resolvedSrcTypeToSrcType,
    resolvedTypeBinderIdentity,
    resolvedTypeBinderName,
    resolvedTypeBinderRefFromIdentity,
    resolvedTypeBinderTypeIdentity,
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

resolvedProgramGeneratedIdentities :: ResolvedSemanticProgramArtifact -> [UniqueIdentity]
resolvedProgramGeneratedIdentities (ResolvedSemanticProgramArtifact modules0) =
  concatMap resolvedModuleGeneratedIdentities modules0

resolvedModuleGeneratedIdentities :: ResolvedSemanticModule -> [UniqueIdentity]
resolvedModuleGeneratedIdentities resolvedModule =
  symbolGeneratedIdentities (resolvedSemanticModuleIdentity resolvedModule)
    ++ resolvedLocalSymbolsGeneratedIdentities (resolvedSemanticModuleLocalSymbols resolvedModule)
    ++ concatMap resolvedDeclGeneratedIdentities (P.moduleDecls (resolvedSemanticModuleSyntax resolvedModule))

resolvedLocalSymbolsGeneratedIdentities :: ResolvedLocalSymbols -> [UniqueIdentity]
resolvedLocalSymbolsGeneratedIdentities localSymbols =
  concatMap resolvedSymbolGeneratedIdentities $
    concatMap concat $
      [ Map.elems (resolvedLocalValues localSymbols),
        Map.elems (resolvedLocalTypes localSymbols),
        Map.elems (resolvedLocalClasses localSymbols)
      ]

resolvedSymbolGeneratedIdentities :: ResolvedSymbol -> [UniqueIdentity]
resolvedSymbolGeneratedIdentities =
  symbolGeneratedIdentities . resolvedSymbolIdentity

resolvedLocalRefGeneratedIdentities :: LocalRef -> [UniqueIdentity]
resolvedLocalRefGeneratedIdentities =
  localRefGeneratedIdentities

resolvedDeclGeneratedIdentities :: P.ResolvedDecl -> [UniqueIdentity]
resolvedDeclGeneratedIdentities = \case
  P.DeclClass decl ->
    resolvedSymbolGeneratedIdentities (P.classDeclName decl)
      ++ concatMap typeParamGeneratedIdentities (NE.toList (P.classDeclParams decl))
      ++ concatMap resolvedClassConstraintGeneratedIdentities (P.classDeclSuperclasses decl)
      ++ concatMap (resolvedSymbolGeneratedIdentities . P.methodSigName) (P.classDeclMethods decl)
      ++ concatMap (resolvedConstrainedTypeGeneratedIdentities . P.methodSigType) (P.classDeclMethods decl)
  P.DeclInstance decl ->
    resolvedSymbolGeneratedIdentities (P.instanceDeclClass decl)
      ++ concatMap resolvedClassConstraintGeneratedIdentities (P.instanceDeclConstraints decl)
      ++ concatMap resolvedSrcTypeGeneratedIdentities (NE.toList (P.instanceDeclTypes decl))
      ++ concatMap resolvedMethodDefGeneratedIdentities (P.instanceDeclMethods decl)
  P.DeclData decl ->
    resolvedSymbolGeneratedIdentities (P.dataDeclName decl)
      ++ concatMap typeParamGeneratedIdentities (P.dataDeclParams decl)
      ++ concatMap (resolvedSymbolGeneratedIdentities . P.constructorDeclName) (P.dataDeclConstructors decl)
      ++ concatMap (resolvedSrcTypeGeneratedIdentities . P.constructorDeclType) (P.dataDeclConstructors decl)
      ++ concatMap resolvedSymbolGeneratedIdentities (P.dataDeclDeriving decl)
  P.DeclTypeFamily _ ->
    []
  P.DeclDef decl ->
    resolvedSymbolGeneratedIdentities (P.defDeclName decl)
      ++ resolvedConstrainedTypeGeneratedIdentities (P.defDeclType decl)
      ++ resolvedExprGeneratedIdentities (P.defDeclExpr decl)

resolvedMethodDefGeneratedIdentities :: P.ResolvedMethodDef -> [UniqueIdentity]
resolvedMethodDefGeneratedIdentities methodDef =
  resolvedSymbolGeneratedIdentities (P.methodDefName methodDef)
    ++ resolvedExprGeneratedIdentities (P.methodDefExpr methodDef)

typeParamGeneratedIdentities :: P.TypeParam -> [UniqueIdentity]
typeParamGeneratedIdentities param =
  maybe [] resolvedTypeBinderGeneratedIdentities (P.typeParamRef param)

resolvedClassConstraintGeneratedIdentities :: P.ResolvedClassConstraint -> [UniqueIdentity]
resolvedClassConstraintGeneratedIdentities constraint =
  resolvedSymbolGeneratedIdentities (P.constraintClassName constraint)
    ++ concatMap resolvedSrcTypeGeneratedIdentities (NE.toList (P.constraintTypes constraint))

resolvedConstrainedTypeGeneratedIdentities :: P.ResolvedConstrainedType -> [UniqueIdentity]
resolvedConstrainedTypeGeneratedIdentities ty =
  concatMap resolvedClassConstraintGeneratedIdentities (P.constrainedConstraints ty)
    ++ resolvedSrcTypeGeneratedIdentities (P.constrainedBody ty)

resolvedSrcTypeGeneratedIdentities :: ResolvedSrcTy n v -> [UniqueIdentity]
resolvedSrcTypeGeneratedIdentities = \case
  RSTVar ref -> resolvedTypeBinderGeneratedIdentities ref
  RSTArrow dom cod -> resolvedSrcTypeGeneratedIdentities dom ++ resolvedSrcTypeGeneratedIdentities cod
  RSTBase symbol -> resolvedSymbolGeneratedIdentities symbol
  RSTCon symbol args ->
    resolvedSymbolGeneratedIdentities symbol
      ++ concatMap resolvedSrcTypeGeneratedIdentities (NE.toList args)
  RSTVarApp ref args ->
    resolvedTypeBinderGeneratedIdentities ref
      ++ concatMap resolvedSrcTypeGeneratedIdentities (NE.toList args)
  RSTTyLam ref body ->
    resolvedTypeBinderGeneratedIdentities ref
      ++ resolvedSrcTypeGeneratedIdentities body
  RSTTyApp fun arg -> resolvedSrcTypeGeneratedIdentities fun ++ resolvedSrcTypeGeneratedIdentities arg
  RSTForall ref mb body ->
    resolvedTypeBinderGeneratedIdentities ref
      ++ maybe [] (resolvedSrcTypeGeneratedIdentities . unResolvedSrcBound) mb
      ++ resolvedSrcTypeGeneratedIdentities body
  RSTMu ref body ->
    resolvedTypeBinderGeneratedIdentities ref
      ++ resolvedSrcTypeGeneratedIdentities body
  RSTBottom -> []

resolvedTypeBinderGeneratedIdentities :: ResolvedTypeBinderRef -> [UniqueIdentity]
resolvedTypeBinderGeneratedIdentities ref =
  typeBinderGeneratedIdentities (resolvedTypeBinderIdentity ref)

resolvedExprGeneratedIdentities :: P.ResolvedExpr -> [UniqueIdentity]
resolvedExprGeneratedIdentities = \case
  P.EVar (P.ResolvedLocalValue ref) -> resolvedLocalRefGeneratedIdentities ref
  P.EVar (P.ResolvedGlobalValue symbol) -> resolvedSymbolGeneratedIdentities symbol
  P.ELit _ -> []
  P.ELam param body ->
    resolvedLocalRefGeneratedIdentities (P.paramName param)
      ++ maybe [] resolvedSrcTypeGeneratedIdentities (P.paramType param)
      ++ resolvedExprGeneratedIdentities body
  P.EApp fun arg -> resolvedExprGeneratedIdentities fun ++ resolvedExprGeneratedIdentities arg
  P.ELet name mbTy rhs body ->
    resolvedLocalRefGeneratedIdentities name
      ++ maybe [] resolvedSrcTypeGeneratedIdentities mbTy
      ++ resolvedExprGeneratedIdentities rhs
      ++ resolvedExprGeneratedIdentities body
  P.EAnn inner ty -> resolvedExprGeneratedIdentities inner ++ resolvedSrcTypeGeneratedIdentities ty
  P.ECase scrutinee alts ->
    resolvedExprGeneratedIdentities scrutinee
      ++ concatMap resolvedAltGeneratedIdentities alts

resolvedAltGeneratedIdentities :: P.ResolvedAlt -> [UniqueIdentity]
resolvedAltGeneratedIdentities (P.Alt pattern0 body) =
  resolvedPatternGeneratedIdentities pattern0 ++ resolvedExprGeneratedIdentities body

resolvedPatternGeneratedIdentities :: P.ResolvedPattern -> [UniqueIdentity]
resolvedPatternGeneratedIdentities = \case
  P.PatCtor symbol patterns ->
    resolvedSymbolGeneratedIdentities symbol
      ++ concatMap resolvedPatternGeneratedIdentities patterns
  P.PatVar ref -> resolvedLocalRefGeneratedIdentities ref
  P.PatWildcard -> []
  P.PatAnn inner ty -> resolvedPatternGeneratedIdentities inner ++ resolvedSrcTypeGeneratedIdentities ty

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
    typeViewHeadIdentities :: Map String SymbolIdentity,
    typeViewBinderIdentities :: Map String TypeBinderIdentity
  }
  deriving (Show)

instance Eq TypeView where
  left == right =
    typeViewIdentityTypesMatch left right
      && typeViewHeadIdentitySet left == typeViewHeadIdentitySet right
      && typeViewBinderIdentitySet left == typeViewBinderIdentitySet right

typeViewIdentityTypesMatch :: TypeView -> TypeView -> Bool
typeViewIdentityTypesMatch leftView rightView =
  go Map.empty Map.empty (typeViewIdentity leftView) (typeViewIdentity rightView)
  where
    go leftNames rightNames left right =
      case (left, right) of
        (STVar leftName, STVar rightName) ->
          sameTypeVar leftNames rightNames leftName rightName
        (STArrow leftDom leftCod, STArrow rightDom rightCod) ->
          go leftNames rightNames leftDom rightDom
            && go leftNames rightNames leftCod rightCod
        (STBase leftName, STBase rightName) ->
          sameTypeHead leftName rightName
        (STCon leftName leftArgs, STCon rightName rightArgs) ->
          sameTypeHead leftName rightName
            && length (NE.toList leftArgs) == length (NE.toList rightArgs)
            && and (zipWith (go leftNames rightNames) (NE.toList leftArgs) (NE.toList rightArgs))
        (STVarApp leftName leftArgs, STVarApp rightName rightArgs) ->
          sameTypeVar leftNames rightNames leftName rightName
            && length (NE.toList leftArgs) == length (NE.toList rightArgs)
            && and (zipWith (go leftNames rightNames) (NE.toList leftArgs) (NE.toList rightArgs))
        (STTyLam leftName leftBody, STTyLam rightName rightBody) ->
          maybe False (\(leftNames', rightNames') -> go leftNames' rightNames' leftBody rightBody) (bindTypeVars leftNames rightNames leftName rightName)
        (STTyApp leftFun leftArg, STTyApp rightFun rightArg) ->
          go leftNames rightNames leftFun rightFun
            && go leftNames rightNames leftArg rightArg
        (STForall leftName leftMb leftBody, STForall rightName rightMb rightBody) ->
          case bindTypeVars leftNames rightNames leftName rightName of
            Just (leftNames', rightNames') ->
              sameBounds leftNames' rightNames' leftMb rightMb
                && go leftNames' rightNames' leftBody rightBody
            Nothing ->
              False
        (STMu leftName leftBody, STMu rightName rightBody) ->
          maybe False (\(leftNames', rightNames') -> go leftNames' rightNames' leftBody rightBody) (bindTypeVars leftNames rightNames leftName rightName)
        (STBottom, STBottom) ->
          True
        _ ->
          False

    sameBounds _ _ Nothing Nothing = True
    sameBounds leftNames rightNames (Just (SrcBound leftBound)) (Just (SrcBound rightBound)) =
      go leftNames rightNames leftBound rightBound
    sameBounds _ _ _ _ = False

    bindTypeVars leftNames rightNames leftName rightName
      | sameFreeTypeVar leftName rightName =
          Just (Map.insert leftName rightName leftNames, Map.insert rightName leftName rightNames)
      | otherwise =
          Nothing

    sameTypeVar leftNames rightNames leftName rightName =
      case (Map.lookup leftName leftNames, Map.lookup rightName rightNames) of
        (Just mappedRight, Just mappedLeft) -> mappedRight == rightName && mappedLeft == leftName
        (Nothing, Nothing) -> sameFreeTypeVar leftName rightName
        _ -> False

    sameFreeTypeVar leftName rightName =
      case (typeViewBinderIdentityForAlias leftView leftName, typeViewBinderIdentityForAlias rightView rightName) of
        (Just leftIdentity, Just rightIdentity) -> leftIdentity == rightIdentity
        (Nothing, Nothing) -> leftName == rightName
        _ -> False

    sameTypeHead leftName rightName =
      case (typeViewHeadIdentityForAlias leftView leftName, typeViewHeadIdentityForAlias rightView rightName) of
        (Just leftIdentity, Just rightIdentity) -> leftIdentity == rightIdentity
        (Nothing, Nothing) -> leftName == rightName
        _ -> False

typeViewHeadIdentitySet :: TypeView -> Set SymbolIdentity
typeViewHeadIdentitySet =
  Set.fromList . Map.elems . typeViewHeadIdentities

typeViewBinderIdentitySet :: TypeView -> Set TypeBinderIdentity
typeViewBinderIdentitySet =
  Set.fromList . Map.elems . typeViewBinderIdentities

typeViewHeadIdentityFor :: TypeView -> String -> Maybe SymbolIdentity
typeViewHeadIdentityFor view name =
  Map.lookup name (typeViewHeadIdentities view)

typeViewBinderIdentityFor :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewBinderIdentityFor view name =
  Map.lookup name (typeViewBinderIdentities view)

typeViewHeadIdentityForAlias :: TypeView -> String -> Maybe SymbolIdentity
typeViewHeadIdentityForAlias view name =
  typeViewHeadIdentityFor view name
    <|> (Map.lookup name (typeViewHeadPairs view) >>= typeViewHeadIdentityFor view)
    <|> (Map.lookup name (typeViewReverseHeadPairs view) >>= typeViewHeadIdentityFor view)
    <|> Map.lookup name (typeViewHeadStableAliases view)

typeViewBinderIdentityForAlias :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewBinderIdentityForAlias view name =
  typeViewBinderIdentityFor view name
    <|> (Map.lookup name (typeViewVarPairs view) >>= typeViewBinderIdentityFor view)
    <|> (Map.lookup name (typeViewReverseVarPairs view) >>= typeViewBinderIdentityFor view)
    <|> Map.lookup name (typeViewBinderStableAliases view)

typeViewHeadStableAliases :: TypeView -> Map String SymbolIdentity
typeViewHeadStableAliases =
  symbolIdentityAliasMap
    . Map.elems
    . typeViewHeadIdentities

typeViewBinderStableAliases :: TypeView -> Map String TypeBinderIdentity
typeViewBinderStableAliases =
  typeBinderAliasIdentityMap . Map.toList . typeViewBinderIdentities

typeViewMentionedHeadIdentities :: TypeView -> Set SymbolIdentity
typeViewMentionedHeadIdentities view =
  Set.fromList
    [ identity
    | name <- Set.toList headNames,
      Just identity <- [typeViewHeadIdentityForAlias view name]
    ]
  where
    headNames =
      typeHeadNamesSrcType (typeViewIdentity view)
        <> typeHeadNamesSrcType (typeViewDisplay view)

mkTypeView :: SrcType -> SrcType -> TypeView
mkTypeView display identity =
  TypeView
    { typeViewDisplay = display,
      typeViewIdentity = identity,
      typeViewHeadIdentities = Map.empty,
      typeViewBinderIdentities = Map.empty
    }

data TypeViewSubstKey
  = TypeViewSubstByIdentity TypeBinderIdentity
  deriving (Eq, Ord, Show)

type TypeViewSubst = Map TypeViewSubstKey TypeView

typeViewSubstKeyForIdentity :: TypeBinderIdentity -> TypeViewSubstKey
typeViewSubstKeyForIdentity =
  TypeViewSubstByIdentity

typeViewSubstKeyIdentity :: TypeViewSubstKey -> TypeBinderIdentity
typeViewSubstKeyIdentity =
  \case
    TypeViewSubstByIdentity identity -> identity

typeViewSubstKeyFor :: TypeView -> String -> Maybe TypeViewSubstKey
typeViewSubstKeyFor view identityName =
  typeViewSubstKeyForIdentity <$> typeViewSubstIdentityFor view identityName

typeViewSubstIdentityFor :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewSubstIdentityFor =
  typeViewBinderIdentityForAlias

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
  deriving (Show)

instance Eq ConstraintInfo where
  left == right =
    constraintClassSymbol left == constraintClassSymbol right
      && constraintTypeViews left == constraintTypeViews right

constraintMetadataMatches :: [P.ClassConstraint] -> [ConstraintInfo] -> [P.ClassConstraint] -> [ConstraintInfo] -> Bool
constraintMetadataMatches leftDisplay leftInfos rightDisplay rightInfos
  | not (null leftInfos) || not (null rightInfos) =
      leftInfos == rightInfos
  | otherwise =
      leftDisplay == rightDisplay

typeViewGeneratedIdentities :: TypeView -> [UniqueIdentity]
typeViewGeneratedIdentities view =
  concatMap symbolGeneratedIdentities (Map.elems (typeViewHeadIdentities view))
    ++ concatMap typeBinderGeneratedIdentities (Map.elems (typeViewBinderIdentities view))

typeBinderSubstGeneratedIdentities :: TypeBinderSubst -> [UniqueIdentity]
typeBinderSubstGeneratedIdentities subst =
  concatMap typeBinderGeneratedIdentities (Map.keys (typeBinderSubstByIdentity subst))

constraintInfoGeneratedIdentities :: ConstraintInfo -> [UniqueIdentity]
constraintInfoGeneratedIdentities constraint =
  symbolGeneratedIdentities (constraintClassSymbol constraint)
    ++ concatMap typeViewGeneratedIdentities (NE.toList (constraintTypeViews constraint))

typeViewFromResolved :: ResolvedSrcType -> TypeView
typeViewFromResolved ty =
  TypeView
    { typeViewDisplay = resolvedSrcTypeToSrcType ty,
      typeViewIdentity = resolvedSrcTypeIdentityType ty,
      typeViewHeadIdentities = resolvedSrcTypeHeadIdentities ty,
      typeViewBinderIdentities = resolvedSrcTypeBinderIdentities ty
    }

resolvedSrcTypeHeadIdentities :: ResolvedSrcTy n v -> Map String SymbolIdentity
resolvedSrcTypeHeadIdentities ty =
  mergeSymbolIdentityMaps
    [ Map.singleton name (resolvedSymbolIdentity symbol)
    | symbol <- symbols,
      name <- resolvedTypeHeadIdentityNames symbol,
      not (null name)
    ]
  where
    symbols =
      [ symbol
      | symbol <- resolvedSrcTypeHeadSymbols ty,
        symbolNamespace (resolvedSymbolIdentity symbol) == SymbolType
      ]

resolvedSrcTypeHeadSymbols :: ResolvedSrcTy n v -> [ResolvedSymbol]
resolvedSrcTypeHeadSymbols =
  \case
    RSTVar {} -> []
    RSTArrow dom cod -> resolvedSrcTypeHeadSymbols dom ++ resolvedSrcTypeHeadSymbols cod
    RSTBase symbol -> [symbol]
    RSTCon symbol args -> symbol : foldMap resolvedSrcTypeHeadSymbols args
    RSTVarApp _ args -> foldMap resolvedSrcTypeHeadSymbols args
    RSTTyLam _ body -> resolvedSrcTypeHeadSymbols body
    RSTTyApp fun arg -> resolvedSrcTypeHeadSymbols fun ++ resolvedSrcTypeHeadSymbols arg
    RSTForall _ mb body ->
      maybe [] (resolvedSrcTypeHeadSymbols . unResolvedSrcBound) mb
        ++ resolvedSrcTypeHeadSymbols body
    RSTMu _ body -> resolvedSrcTypeHeadSymbols body
    RSTBottom -> []

resolvedTypeHeadIdentityNames :: ResolvedSymbol -> [String]
resolvedTypeHeadIdentityNames symbol =
  symbolIdentityAliasNamesWith [symbolSourceName spelling, symbolDisplayName spelling] identity
  where
    identity =
      resolvedSymbolIdentity symbol
    spelling =
      resolvedSymbolSpelling symbol

resolvedSrcTypeBinderIdentities :: ResolvedSrcTy n v -> Map String TypeBinderIdentity
resolvedSrcTypeBinderIdentities =
  typeBinderAliasIdentityMap
    . map (\ref -> (resolvedTypeBinderName ref, resolvedTypeBinderTypeIdentity ref))
    . resolvedSrcTypeBinderRefs

resolvedSrcTypeBinderRefs :: ResolvedSrcTy n v -> [ResolvedTypeBinderRef]
resolvedSrcTypeBinderRefs ty =
  case ty of
    RSTVar ref -> [ref]
    RSTArrow dom cod -> resolvedSrcTypeBinderRefs dom ++ resolvedSrcTypeBinderRefs cod
    RSTBase {} -> []
    RSTCon _ args -> foldMap resolvedSrcTypeBinderRefs args
    RSTVarApp ref args -> ref : foldMap resolvedSrcTypeBinderRefs args
    RSTTyLam ref body -> ref : resolvedSrcTypeBinderRefs body
    RSTTyApp fun arg -> resolvedSrcTypeBinderRefs fun ++ resolvedSrcTypeBinderRefs arg
    RSTForall ref mb body ->
      ref
        : maybe [] (resolvedSrcTypeBinderRefs . unResolvedSrcBound) mb
          ++ resolvedSrcTypeBinderRefs body
    RSTMu ref body -> ref : resolvedSrcTypeBinderRefs body
    RSTBottom -> []

displayConstraint :: ConstraintInfo -> P.ClassConstraint
displayConstraint constraint =
  P.ClassConstraint
    { P.constraintClassName = constraintDisplayClass constraint,
      P.constraintTypes = typeViewsDisplay (constraintTypeViews constraint)
    }

identityConstraint :: ConstraintInfo -> P.ClassConstraint
identityConstraint constraint =
  P.ClassConstraint
    { P.constraintClassName = constraintDisplayClass constraint,
      P.constraintTypes = typeViewsIdentity (constraintTypeViews constraint)
    }

constrainedVisibleTypeView :: [ConstraintInfo] -> TypeView -> TypeView
constrainedVisibleTypeView constraints bodyView =
  TypeView
    { typeViewDisplay =
        constrainedVisibleType $
          P.ConstrainedType
            (map displayConstraint constraints)
            (typeViewDisplay bodyView),
      typeViewIdentity =
        constrainedVisibleType $
          P.ConstrainedType
            (map identityConstraint constraints)
            (typeViewIdentity bodyView),
      typeViewHeadIdentities =
        mergeSymbolIdentityMaps
          ( typeViewHeadIdentities bodyView
              : concatMap
                (map typeViewHeadIdentities . NE.toList . constraintTypeViews)
                constraints
          ),
      typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          ( typeViewBinderIdentities bodyView
              : concatMap
                (map typeViewBinderIdentities . NE.toList . constraintTypeViews)
                constraints
          )
    }

applyTypeViewSubst :: TypeViewSubst -> TypeView -> TypeView
applyTypeViewSubst subst view =
  TypeView
    { typeViewDisplay = displayTy,
      typeViewIdentity = identityTy,
      typeViewHeadIdentities =
        filterHeadIdentitiesByTypeNames displayTy identityTy substitutedHeadIdentities,
      typeViewBinderIdentities =
        filterBinderIdentitiesByNames
          (freeIdentityNames <> freeDisplayNames)
          substitutedBinderIdentityAliases
          substitutedBinderIdentities
    }
  where
    displaySubst = typeViewSubstDisplayTypes view subst
    identitySubst = typeViewIdentitySubstTypesFor view subst
    displayTy = Map.foldrWithKey substituteTypeVar (typeViewDisplay view) displaySubst
    identityTy = Map.foldrWithKey substituteTypeVar (typeViewIdentity view) identitySubst
    freeDisplayNames = freeTypeVarsSrcType displayTy
    freeIdentityNames = freeTypeVarsSrcType identityTy
    substitutedHeadIdentities =
      mergeSymbolIdentityMaps (typeViewHeadIdentities view : map typeViewHeadIdentities (Map.elems subst))
    substitutedBinderIdentities =
      mergeTypeBinderIdentityMaps (typeViewBinderIdentities view : map typeViewBinderIdentities (Map.elems subst))
    substitutedBinderIdentityAliases =
      concatMap typeViewBinderIdentityAliasEntries (view : Map.elems subst)

filterHeadIdentitiesByNames :: Set String -> Map String SymbolIdentity -> Map String SymbolIdentity
filterHeadIdentitiesByNames names identities =
  mergeSymbolIdentityMaps [keptRaw, rescuedStable]
  where
    identityByName =
      symbolIdentityAliasMapWith [(identity, [key]) | (key, identity) <- Map.toList identities]

    keptRaw =
      Map.filterWithKey keep identities

    rescuedStable =
      mergeSymbolIdentityMaps
        [ Map.singleton stableName identity
        | (key, identity) <- Map.toList identities,
          not (keep key identity),
          let stableName = symbolIdentityStableName identity,
          Set.member stableName names,
          Map.lookup stableName identityByName == Just identity
        ]

    keep key identity =
      Map.lookup key identityByName == Just identity && mentioned identity

    mentioned identity =
      any (\name -> Map.lookup name identityByName == Just identity) (Set.toList names)

filterHeadIdentitiesByTypeNames :: SrcType -> SrcType -> Map String SymbolIdentity -> Map String SymbolIdentity
filterHeadIdentitiesByTypeNames displayTy identityTy identities =
  mergeSymbolIdentityMaps [filtered, pairedDisplayAliases]
  where
    names =
      typeHeadNamesSrcType identityTy <> typeHeadNamesSrcType displayTy

    filtered =
      filterHeadIdentitiesByNames names identities

    filteredAliases =
      symbolIdentityAliasMap (Map.elems filtered)

    lookupFiltered name =
      Map.lookup name filtered <|> Map.lookup name filteredAliases

    pairedDisplayAliases =
      mergeSymbolIdentityMaps
        [ Map.singleton displayName identity
        | (identityName, displayName) <- Map.toList (srcTypeHeadPairs displayTy identityTy),
          displayName /= identityName,
          Set.member displayName names,
          Just identity <- [lookupFiltered identityName]
        ]

filterBinderIdentitiesByNames :: Set String -> [(String, TypeBinderIdentity)] -> Map String TypeBinderIdentity -> Map String TypeBinderIdentity
filterBinderIdentitiesByNames names aliases identities =
  mergeTypeBinderIdentityMaps [keptRaw, rescuedStable]
  where
    identityByName =
      mergeTypeBinderIdentityMaps
        [ Map.singleton name identity
        | (name, identity) <- aliases
        ]

    keptRaw =
      Map.filterWithKey keep identities

    rescuedStable =
      mergeTypeBinderIdentityMaps
        [ Map.singleton stableName identity
        | (key, identity) <- Map.toList identities,
          not (keep key identity),
          let stableName = typeBinderIdentityStableName identity,
          Set.member stableName names,
          Map.lookup stableName identityByName == Just identity
        ]

    keep key identity =
      Map.lookup key identityByName == Just identity && mentioned identity

    mentioned identity =
      any (\name -> Map.lookup name identityByName == Just identity) (Set.toList names)

typeViewBinderIdentityAliasEntries :: TypeView -> [(String, TypeBinderIdentity)]
typeViewBinderIdentityAliasEntries view =
  Map.toList (mergeTypeBinderIdentityMaps [directAliases, pairedAliases])
  where
    identities =
      typeViewBinderIdentities view

    directAliases =
      typeBinderIdentityAliasMap (Map.toList identities)

    pairedAliases =
      mergeTypeBinderIdentityMaps
        [ Map.singleton alias identity
        | (identityName, displayName) <- Map.toList (typeViewBinderPairs view)
        , (alias, identity) <-
            maybe [] (\identity -> [(displayName, identity)]) (Map.lookup identityName identities)
              ++ maybe [] (\identity -> [(identityName, identity)]) (Map.lookup displayName identities)
        ]

mergeSymbolIdentityMaps :: [Map String SymbolIdentity] -> Map String SymbolIdentity
mergeSymbolIdentityMaps maps =
  Map.fromList
    [ (name, identity)
    | (name, identities) <- Map.toList identitiesByName,
      [identity] <- [Set.toList identities]
    ]
  where
    identitiesByName =
      Map.fromListWith
        Set.union
        [ (name, Set.singleton identity)
        | identityMap <- maps,
          (name, identity) <- Map.toList identityMap
        ]

mergeTypeBinderIdentityMaps :: [Map String TypeBinderIdentity] -> Map String TypeBinderIdentity
mergeTypeBinderIdentityMaps maps =
  Map.fromList
    [ (name, identity)
    | (name, identities) <- Map.toList identitiesByName,
      [identity] <- [Set.toList identities]
    ]
  where
    identitiesByName =
      Map.fromListWith
        Set.union
        [ (name, Set.singleton identity)
        | identityMap <- maps,
          (name, identity) <- Map.toList identityMap
        ]

typeBinderAliasIdentityMap :: [(String, TypeBinderIdentity)] -> Map String TypeBinderIdentity
typeBinderAliasIdentityMap =
  typeBinderIdentityAliasMap

typeViewSubstDisplayTypes :: TypeView -> TypeViewSubst -> Map String SrcType
typeViewSubstDisplayTypes view subst =
  Map.fromList
    [ (name, ty)
      | (name, tys) <- Map.toList typesByName,
        [ty] <- [Set.toList tys]
    ]
  where
    typesByName =
      Map.fromListWith
        Set.union
        [ (name, Set.singleton (typeViewDisplay substView))
        | (key, substView) <- Map.toList subst,
          name <- typeViewSubstKeyDisplayNames view key
        ]

typeViewIdentitySubstTypesFor :: TypeView -> TypeViewSubst -> Map String SrcType
typeViewIdentitySubstTypesFor view subst =
  Map.fromList (concatMap entries (Map.toList subst))
  where
    entries (key, substView) =
      case key of
        TypeViewSubstByIdentity identity ->
          [ (name, typeViewIdentity substView)
          | name <- Set.toList (identityNames identity)
          ]

    identityNames =
      typeViewSubstIdentityNames view

typeViewSubstKeyDisplayNames :: TypeView -> TypeViewSubstKey -> [String]
typeViewSubstKeyDisplayNames view key =
  case key of
    TypeViewSubstByIdentity identity ->
      Set.toList (typeViewSubstDisplayNames view identity)

typeViewSubstDisplayNames :: TypeView -> TypeBinderIdentity -> Set String
typeViewSubstDisplayNames view identity =
  Set.fromList
    [ name
    | name <-
        Set.toList binderNames
          ++ [ displayName
             | (identityName, displayName) <- Map.toList (typeViewVarPairs view),
               Set.member identityName binderNames || Set.member displayName binderNames
             ],
      typeViewBinderIdentityForAlias view name == Just identity
    ]
  where
    binderNames =
      typeViewSubstBinderNames view identity

typeViewSubstIdentityNames :: TypeView -> TypeBinderIdentity -> Set String
typeViewSubstIdentityNames view identity =
  Set.fromList
    [ name
    | name <-
        Set.toList binderNames
          ++ [ identityName
             | (identityName, displayName) <- Map.toList (typeViewVarPairs view),
               Set.member identityName binderNames || Set.member displayName binderNames
             ],
      typeViewBinderIdentityForAlias view name == Just identity
    ]
  where
    binderNames =
      typeViewSubstBinderNames view identity

typeViewSubstBinderNames :: TypeView -> TypeBinderIdentity -> Set String
typeViewSubstBinderNames view identity =
  Map.keysSet (Map.filter (== identity) (typeViewBinderIdentities view))

typeViewVarPairs :: TypeView -> Map String String
typeViewVarPairs view =
  srcTypeVarPairs (typeViewDisplay view) (typeViewIdentity view)

typeViewBinderPairs :: TypeView -> Map String String
typeViewBinderPairs view =
  mergeUniquePairMaps [typeViewVarPairs view, boundBinderPairs (typeViewDisplay view) (typeViewIdentity view)]
  where
    boundBinderPairs display identityTy =
      case (display, identityTy) of
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          mergeUniquePairMaps [boundBinderPairs displayDom identityDom, boundBinderPairs displayCod identityCod]
        (STCon _ displayArgs, STCon _ identityArgs) ->
          pairsFromArgs displayArgs identityArgs
        (STVarApp _ displayArgs, STVarApp _ identityArgs) ->
          pairsFromArgs displayArgs identityArgs
        (STTyLam displayName displayBody, STTyLam identityName identityBody) ->
          mergeUniquePairMaps [Map.singleton identityName displayName, boundBinderPairs displayBody identityBody]
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          mergeUniquePairMaps [boundBinderPairs displayFun identityFun, boundBinderPairs displayArg identityArg]
        (STForall displayName displayMb displayBody, STForall identityName identityMb identityBody) ->
          mergeUniquePairMaps
            [ Map.singleton identityName displayName,
              boundPairs displayMb identityMb,
              boundBinderPairs displayBody identityBody
            ]
        (STMu displayName displayBody, STMu identityName identityBody) ->
          mergeUniquePairMaps [Map.singleton identityName displayName, boundBinderPairs displayBody identityBody]
        _ ->
          Map.empty

    pairsFromArgs displayArgs identityArgs =
      mergeUniquePairMaps (zipWith boundBinderPairs (NE.toList displayArgs) (NE.toList identityArgs))

    boundPairs displayMb identityMb =
      case (displayMb, identityMb) of
        (Just (SrcBound displayBound), Just (SrcBound identityBound)) ->
          boundBinderPairs displayBound identityBound
        _ ->
          Map.empty

typeViewHeadPairs :: TypeView -> Map String String
typeViewHeadPairs view =
  srcTypeHeadPairs (typeViewDisplay view) (typeViewIdentity view)

typeViewReverseVarPairs :: TypeView -> Map String String
typeViewReverseVarPairs =
  uniqueReverseMap . typeViewVarPairs

typeViewReverseHeadPairs :: TypeView -> Map String String
typeViewReverseHeadPairs =
  uniqueReverseMap . typeViewHeadPairs

uniqueReverseMap :: (Ord k, Ord v) => Map k v -> Map v k
uniqueReverseMap pairs =
  Map.fromList
    [ (value, key)
    | (value, keys) <- Map.toList keysByValue,
      [key] <- [Set.toList keys]
    ]
  where
    keysByValue =
      Map.fromListWith
        Set.union
        [ (value, Set.singleton key)
        | (key, value) <- Map.toList pairs
        ]

mergeUniquePairMaps :: (Ord k, Ord v) => [Map k v] -> Map k v
mergeUniquePairMaps maps =
  Map.fromList
    [ (key, value)
    | (key, values) <- Map.toList valuesByKey,
      [value] <- [Set.toList values]
    ]
  where
    valuesByKey =
      Map.fromListWith
        Set.union
        [ (key, Set.singleton value)
        | pairMap <- maps,
          (key, value) <- Map.toList pairMap
        ]

srcTypeVarPairs :: SrcType -> SrcType -> Map String String
srcTypeVarPairs =
  go Set.empty Set.empty
  where
    go displayBound identityBound display identityTy =
      case (display, identityTy) of
        (STVar displayName, STVar identityName)
          | identityName `Set.notMember` identityBound ->
              Map.singleton identityName displayName
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          mergeUniquePairMaps
            [ go displayBound identityBound displayDom identityDom
            , go displayBound identityBound displayCod identityCod
            ]
        (STCon _ displayArgs, STCon _ identityArgs) ->
          pairsFromArgs displayBound identityBound displayArgs identityArgs
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs) ->
          let headPair =
                if identityName `Set.member` identityBound
                  then Map.empty
                  else Map.singleton identityName displayName
           in mergeUniquePairMaps
                [ headPair
                , pairsFromArgs displayBound identityBound displayArgs identityArgs
                ]
        (STTyLam displayName displayBody, STTyLam identityName identityBody) ->
          go (Set.insert displayName displayBound) (Set.insert identityName identityBound) displayBody identityBody
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          mergeUniquePairMaps
            [ go displayBound identityBound displayFun identityFun
            , go displayBound identityBound displayArg identityArg
            ]
        (STForall displayName displayMb displayBody, STForall identityName identityMb identityBody) ->
          mergeUniquePairMaps
            [ boundPairs displayBound identityBound displayMb identityMb
            , go (Set.insert displayName displayBound) (Set.insert identityName identityBound) displayBody identityBody
            ]
        (STMu displayName displayBody, STMu identityName identityBody) ->
          go (Set.insert displayName displayBound) (Set.insert identityName identityBound) displayBody identityBody
        _ -> Map.empty

    pairsFromArgs displayBound identityBound displayArgs identityArgs =
      mergeUniquePairMaps (zipWith (go displayBound identityBound) (NE.toList displayArgs) (NE.toList identityArgs))

    boundPairs displayBound identityBound displayMb identityMb =
      case (displayMb, identityMb) of
        (Just (SrcBound displayBoundTy), Just (SrcBound identityBoundTy)) ->
          go displayBound identityBound displayBoundTy identityBoundTy
        _ -> Map.empty

srcTypeHeadPairs :: SrcType -> SrcType -> Map String String
srcTypeHeadPairs =
  go
  where
    go display identityTy =
      case (display, identityTy) of
        (STBase displayName, STBase identityName) ->
          Map.singleton identityName displayName
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          mergeUniquePairMaps
            [ Map.singleton identityName displayName
            , pairsFromArgs displayArgs identityArgs
            ]
        (STVarApp _ displayArgs, STVarApp _ identityArgs) ->
          pairsFromArgs displayArgs identityArgs
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          mergeUniquePairMaps [go displayDom identityDom, go displayCod identityCod]
        (STTyLam _ displayBody, STTyLam _ identityBody) ->
          go displayBody identityBody
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          mergeUniquePairMaps [go displayFun identityFun, go displayArg identityArg]
        (STForall _ displayMb displayBody, STForall _ identityMb identityBody) ->
          mergeUniquePairMaps [boundPairs displayMb identityMb, go displayBody identityBody]
        (STMu _ displayBody, STMu _ identityBody) ->
          go displayBody identityBody
        _ ->
          Map.empty

    pairsFromArgs displayArgs identityArgs =
      mergeUniquePairMaps (zipWith go (NE.toList displayArgs) (NE.toList identityArgs))

    boundPairs displayMb identityMb =
      case (displayMb, identityMb) of
        (Just (SrcBound displayBound), Just (SrcBound identityBound)) ->
          go displayBound identityBound
        _ ->
          Map.empty

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

freeTypeBinderIdentitiesTypeView :: TypeView -> Either String (Set TypeBinderIdentity)
freeTypeBinderIdentitiesTypeView view =
  Set.fromList <$> traverse requireBinderIdentity (Set.toList freeNames)
  where
    freeNames =
      freeTypeVarsSrcType (typeViewDisplay view)
        <> freeTypeVarsSrcType (typeViewIdentity view)

    requireBinderIdentity name =
      case typeViewBinderIdentityForAlias view name of
        Just identity -> Right identity
        Nothing -> Left name

freeTypeBinderIdentitiesTypeViews :: NonEmpty TypeView -> Either String (Set TypeBinderIdentity)
freeTypeBinderIdentitiesTypeViews views =
  Set.unions <$> mapM freeTypeBinderIdentitiesTypeView views

typeViewIsBareBinderIdentity :: TypeBinderIdentity -> TypeView -> Bool
typeViewIsBareBinderIdentity identity view =
  case typeViewIdentity view of
    STVar name -> typeViewFreeVarMatchesBinderIdentity identity view name
    _ -> False

typeViewMentionsFreeBinderIdentity :: TypeBinderIdentity -> TypeView -> Bool
typeViewMentionsFreeBinderIdentity identity view =
  case freeTypeBinderIdentitiesTypeView view of
    Right identities -> Set.member identity identities
    Left _ -> False

typeViewFreeVarMatchesBinderIdentity :: TypeBinderIdentity -> TypeView -> String -> Bool
typeViewFreeVarMatchesBinderIdentity identity view name =
  typeViewBinderIdentityForAlias view name == Just identity

typeViewsDisplay :: NonEmpty TypeView -> NonEmpty SrcType
typeViewsDisplay = fmap typeViewDisplay

typeViewsIdentity :: NonEmpty TypeView -> NonEmpty SrcType
typeViewsIdentity = fmap typeViewIdentity

typeViewSubstFromParamIdentities :: NonEmpty TypeBinderIdentity -> NonEmpty TypeView -> TypeViewSubst
typeViewSubstFromParamIdentities paramIdentities views =
  Map.fromList (zipWith entry (NE.toList paramIdentities) (NE.toList views))
  where
    entry identity view =
      (typeViewSubstKeyForIdentity identity, view)

typeParamBinderIdentity :: P.TypeParam -> Maybe TypeBinderIdentity
typeParamBinderIdentity param =
  resolvedTypeBinderTypeIdentity <$> P.typeParamRef param

newtype TypeBinderSubst = TypeBinderSubst
  { typeBinderSubstByIdentity :: Map TypeBinderIdentity (Set String, SrcType)
  }
  deriving (Show)

instance Eq TypeBinderSubst where
  left == right =
    typeBinderSubstIdentityTypes left == typeBinderSubstIdentityTypes right

typeBinderSubstIdentityTypes :: TypeBinderSubst -> Map TypeBinderIdentity SrcType
typeBinderSubstIdentityTypes =
  fmap snd . typeBinderSubstByIdentity

emptyTypeBinderSubst :: TypeBinderSubst
emptyTypeBinderSubst =
  TypeBinderSubst
    { typeBinderSubstByIdentity = Map.empty
    }

typeBinderSubstFromTypeViewSubst :: [(String, TypeBinderIdentity)] -> TypeViewSubst -> TypeBinderSubst
typeBinderSubstFromTypeViewSubst binders subst =
  foldr insertView emptyTypeBinderSubst (Map.toList subst)
  where
    bindersByIdentity =
      Map.fromListWith
        Set.union
        [(identity, Set.singleton name) | (name, identity) <- binders]
    insertView (key, view) acc =
      case key of
        TypeViewSubstByIdentity identity ->
          insertTypeBinderSubstByIdentity
            identity
            (Map.findWithDefault Set.empty identity bindersByIdentity)
            (typeViewIdentity view)
            acc

applyTypeBinderSubst :: TypeBinderSubst -> SrcType -> SrcType
applyTypeBinderSubst subst ty =
  Map.foldrWithKey substituteTypeVar ty identityMap
  where
    identityMap =
      Map.fromList
        [ (name, substTy)
        | (name, substitutions) <- Map.toList substitutionsByName,
          [(_, substTy)] <- [Map.toList substitutions]
        ]
    substitutionsByName =
      Map.fromListWith
        Map.union
        [ (name, Map.singleton identity substTy)
        | (identity, (names, substTy)) <- Map.toList (typeBinderSubstByIdentity subst),
          name <- Set.toList names
        ]

typeBinderSubstToTypeViewSubstWith :: (SrcType -> TypeView) -> TypeBinderSubst -> TypeViewSubst
typeBinderSubstToTypeViewSubstWith mkView subst =
  Map.fromList identityEntries
  where
    identityEntries =
      [ (TypeViewSubstByIdentity identity, mkView ty)
        | (identity, (_, ty)) <- Map.toList (typeBinderSubstByIdentity subst)
      ]

lookupTypeBinderSubstByIdentity :: TypeBinderIdentity -> TypeBinderSubst -> Maybe SrcType
lookupTypeBinderSubstByIdentity identity subst =
  snd <$> Map.lookup identity (typeBinderSubstByIdentity subst)

insertTypeBinderSubstWithIdentity :: TypeBinderIdentity -> String -> SrcType -> TypeBinderSubst -> TypeBinderSubst
insertTypeBinderSubstWithIdentity identity name =
  insertTypeBinderSubstByIdentity identity (Set.singleton name)

insertTypeBinderSubstByIdentity :: TypeBinderIdentity -> Set String -> SrcType -> TypeBinderSubst -> TypeBinderSubst
insertTypeBinderSubstByIdentity identity names ty subst =
  subst
    { typeBinderSubstByIdentity =
        Map.insertWith
          (\(newNames, newTy) (oldNames, _) -> (newNames <> oldNames, newTy))
          identity
          (Set.insert (typeBinderIdentityStableName identity) (Set.filter (not . null) names), ty)
          (typeBinderSubstByIdentity subst)
    }

data EvidenceMethod = EvidenceMethod
  { evidenceMethodRuntimeName :: String,
    evidenceMethodSymbol :: SymbolIdentity,
    evidenceMethodResolvedVar :: Maybe ResolvedVar,
    evidenceMethodTypeView :: TypeView
  }
  deriving (Show)

instance Eq EvidenceMethod where
  left == right =
    evidenceMethodSymbol left == evidenceMethodSymbol right
      && evidenceMethodResolvedVar left == evidenceMethodResolvedVar right
      && evidenceMethodTypeView left == evidenceMethodTypeView right

uniqueEvidenceMethod :: [EvidenceMethod] -> Maybe EvidenceMethod
uniqueEvidenceMethod methods =
  case nub methods of
    [method] -> Just method
    _ -> Nothing

uniqueEvidenceMethodMatch :: [(EvidenceMethod, TypeViewSubst)] -> Maybe (EvidenceMethod, TypeViewSubst)
uniqueEvidenceMethodMatch matches =
  case nub matches of
    [match] -> Just match
    _ -> Nothing

data EvidenceInfo = EvidenceInfo
  { evidenceClassSymbol :: SymbolIdentity,
    evidenceTypeViews :: NonEmpty TypeView,
    evidenceMethodsByIdentity :: Map SymbolIdentity EvidenceMethod
  }
  deriving (Eq, Show)

data ConstructorForallBinder = ConstructorForallBinder
  { constructorForallDisplayName :: String,
    constructorForallIdentity :: TypeBinderIdentity
  }
  deriving (Show)

instance Eq ConstructorForallBinder where
  left == right =
    constructorForallIdentity left == constructorForallIdentity right

data ConstructorShape = ConstructorShape
  { constructorShapeSymbol :: SymbolIdentity,
    constructorShapeRuntimeName :: String,
    constructorShapeTypeView :: TypeView,
    constructorShapeForallBinderInfo :: [ConstructorForallBinder],
    constructorShapeIndex :: Int,
    constructorShapeOwnerTypeParams :: [P.TypeParam]
  }
  deriving (Show)

instance Eq ConstructorShape where
  left == right =
    constructorShapeSymbol left == constructorShapeSymbol right
      && constructorShapeTypeView left == constructorShapeTypeView right
      && constructorShapeForallBinderInfo left == constructorShapeForallBinderInfo right
      && constructorShapeIndex left == constructorShapeIndex right
      && constructorShapeOwnerTypeParams left == constructorShapeOwnerTypeParams right

constructorShapeType :: ConstructorShape -> SrcType
constructorShapeType =
  typeViewDisplay . constructorShapeTypeView

constructorShapeTypeIdentity :: ConstructorShape -> SrcType
constructorShapeTypeIdentity =
  typeViewIdentity . constructorShapeTypeView

constructorShapeForalls :: ConstructorShape -> [(String, Maybe SrcType)]
constructorShapeForalls =
  fst . splitForalls . constructorShapeType

constructorShapeForallsIdentity :: ConstructorShape -> [(String, Maybe SrcType)]
constructorShapeForallsIdentity =
  fst . splitForalls . constructorShapeTypeIdentity

constructorShapeArgViews :: ConstructorShape -> [TypeView]
constructorShapeArgViews shape =
  typeViewArrowArgViews (constructorShapeTypeView shape)

constructorShapeResultView :: ConstructorShape -> TypeView
constructorShapeResultView shape =
  typeViewArrowResultView (constructorShapeTypeView shape)

constructorShapeArgs :: ConstructorShape -> [SrcType]
constructorShapeArgs =
  map typeViewDisplay . constructorShapeArgViews

constructorShapeArgsIdentity :: ConstructorShape -> [SrcType]
constructorShapeArgsIdentity =
  map typeViewIdentity . constructorShapeArgViews

constructorShapeResult :: ConstructorShape -> SrcType
constructorShapeResult =
  typeViewDisplay . constructorShapeResultView

constructorShapeResultIdentity :: ConstructorShape -> SrcType
constructorShapeResultIdentity =
  typeViewIdentity . constructorShapeResultView

data ConstructorInfo = ConstructorInfo
  { ctorInfoSymbol :: SymbolIdentity,
    ctorRuntimeName :: String,
    ctorTypeView :: TypeView,
    ctorForallBinderInfo :: [ConstructorForallBinder],
    ctorOwningTypeIdentity :: SymbolIdentity,
    ctorIndex :: Int,
    ctorOwnerConstructors :: [ConstructorShape]
  }
  deriving (Show)

instance Eq ConstructorInfo where
  left == right =
    ctorInfoSymbol left == ctorInfoSymbol right
      && ctorTypeView left == ctorTypeView right
      && ctorForallBinderInfo left == ctorForallBinderInfo right
      && ctorOwningTypeIdentity left == ctorOwningTypeIdentity right
      && ctorIndex left == ctorIndex right
      && ctorOwnerConstructors left == ctorOwnerConstructors right

ctorType :: ConstructorInfo -> SrcType
ctorType =
  typeViewDisplay . ctorTypeView

ctorTypeIdentity :: ConstructorInfo -> SrcType
ctorTypeIdentity =
  typeViewIdentity . ctorTypeView

ctorForalls :: ConstructorInfo -> [(String, Maybe SrcType)]
ctorForalls =
  fst . splitForalls . ctorType

ctorArgs :: ConstructorInfo -> [SrcType]
ctorArgs =
  fst . splitArrows . snd . splitForalls . ctorType

constructorInfoArgViews :: ConstructorInfo -> [TypeView]
constructorInfoArgViews ctorInfo =
  typeViewArrowArgViews (ctorTypeView ctorInfo)

constructorInfoResultView :: ConstructorInfo -> TypeView
constructorInfoResultView ctorInfo =
  typeViewArrowResultView (ctorTypeView ctorInfo)

ctorResult :: ConstructorInfo -> SrcType
ctorResult =
  snd . splitArrows . snd . splitForalls . ctorType

data DataInfo = DataInfo
  { dataInfoSymbol :: SymbolIdentity,
    dataTypeParams :: [P.TypeParam],
    dataConstructors :: [ConstructorInfo]
  }
  deriving (Eq, Show)

data MethodInfo = MethodInfo
  { methodInfoSymbol :: SymbolIdentity,
    methodDisplayName :: P.MethodName,
    methodTypeViewRaw :: TypeView,
    methodConstraints :: [P.ClassConstraint],
    methodConstraintInfos :: [ConstraintInfo],
    methodParamBinders :: NonEmpty (String, TypeBinderIdentity)
  }
  deriving (Show)

instance Eq MethodInfo where
  left == right =
    methodInfoSymbol left == methodInfoSymbol right
      && methodTypeViewRaw left == methodTypeViewRaw right
      && constraintMetadataMatches
        (methodConstraints left)
        (methodConstraintInfos left)
        (methodConstraints right)
        (methodConstraintInfos right)
      && typeBinderEntriesMatch (methodParamBinders left) (methodParamBinders right)

typeBinderEntriesMatch :: NonEmpty (String, TypeBinderIdentity) -> NonEmpty (String, TypeBinderIdentity) -> Bool
typeBinderEntriesMatch left right =
  typeBinderIdentityEntryListMatches (NE.toList left) (NE.toList right)

typeBinderIdentityEntryListMatches :: [(String, TypeBinderIdentity)] -> [(String, TypeBinderIdentity)] -> Bool
typeBinderIdentityEntryListMatches left right =
  length left == length right
    && and (zipWith typeBinderIdentityEntryMatches left right)

typeBinderIdentityEntryMatches :: (String, TypeBinderIdentity) -> (String, TypeBinderIdentity) -> Bool
typeBinderIdentityEntryMatches (_, leftIdentity) (_, rightIdentity) =
  leftIdentity == rightIdentity

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
  deriving (Show)

instance Eq ClassInfo where
  left == right =
    classInfoSymbol left == classInfoSymbol right
      && classTypeParams left == classTypeParams right
      && constraintMetadataMatches
        (classSuperclasses left)
        (classSuperclassInfos left)
        (classSuperclasses right)
        (classSuperclassInfos right)
      && classFunctionalDependencies left == classFunctionalDependencies right
      && classMethodsByIdentity left == classMethodsByIdentity right

data ValueInfo
  = OrdinaryValue
      { valueInfoSymbol :: SymbolIdentity,
        valueRuntimeName :: String,
        valueTypeView :: TypeView,
        valueConstraints :: [P.ClassConstraint],
        valueConstraintInfos :: [ConstraintInfo]
      }
  | ConstructorValue
      { valueInfoSymbol :: SymbolIdentity,
        valueRuntimeName :: String,
        valueCtorInfo :: ConstructorInfo
      }
  | OverloadedMethod
      { valueInfoSymbol :: SymbolIdentity,
        valueMethodInfo :: MethodInfo
      }
  deriving (Show)

instance Eq ValueInfo where
  left == right =
    case (left, right) of
      (OrdinaryValue {}, OrdinaryValue {}) ->
        valueInfoSymbol left == valueInfoSymbol right
          && valueTypeView left == valueTypeView right
          && constraintMetadataMatches
            (valueConstraints left)
            (valueConstraintInfos left)
            (valueConstraints right)
            (valueConstraintInfos right)
      (ConstructorValue {}, ConstructorValue {}) ->
        valueInfoSymbol left == valueInfoSymbol right
          && valueCtorInfo left == valueCtorInfo right
      (OverloadedMethod {}, OverloadedMethod {}) ->
        valueInfoSymbol left == valueInfoSymbol right
          && valueMethodInfo left == valueMethodInfo right
      _ ->
        False

valueType :: ValueInfo -> SrcType
valueType valueInfo =
  case valueInfo of
    OrdinaryValue {valueTypeView = view} -> typeViewDisplay view
    ConstructorValue {valueCtorInfo = ctor} -> ctorType ctor
    OverloadedMethod {} -> STBottom

valueIdentityType :: ValueInfo -> SrcType
valueIdentityType valueInfo =
  case valueInfo of
    OrdinaryValue {valueTypeView = view} -> typeViewIdentity view
    ConstructorValue {valueCtorInfo = ctor} -> ctorTypeIdentity ctor
    OverloadedMethod {} -> STBottom

ordinaryValueTypeView :: ValueInfo -> TypeView
ordinaryValueTypeView OrdinaryValue {valueTypeView = view, valueConstraintInfos = constraints} =
  view
    { typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          ( typeViewBinderIdentities view
              : map constraintBinderIdentities constraints
          )
    }
  where
    constraintBinderIdentities =
      foldMap typeViewBinderIdentities . constraintTypeViews
ordinaryValueTypeView _ =
  mkTypeView STBottom STBottom

data InstanceInfo = InstanceInfo
  { instanceClassSymbol :: SymbolIdentity,
    instanceOriginModuleIdentity :: SymbolIdentity,
    instanceConstraints :: [P.ClassConstraint],
    instanceConstraintInfos :: [ConstraintInfo],
    instanceHeadTypeViews :: NonEmpty TypeView,
    instanceMethodsByIdentity :: Map SymbolIdentity ValueInfo
  }
  deriving (Show)

instance Eq InstanceInfo where
  left == right =
    instanceClassSymbol left == instanceClassSymbol right
      && instanceOriginModuleIdentity left == instanceOriginModuleIdentity right
      && constraintMetadataMatches
        (instanceConstraints left)
        (instanceConstraintInfos left)
        (instanceConstraints right)
        (instanceConstraintInfos right)
      && instanceHeadTypeViews left == instanceHeadTypeViews right
      && instanceMethodsByIdentity left == instanceMethodsByIdentity right

instanceHeadTypes :: InstanceInfo -> NonEmpty SrcType
instanceHeadTypes =
  fmap typeViewDisplay . instanceHeadTypeViews

instanceHeadIdentityTypes :: InstanceInfo -> NonEmpty SrcType
instanceHeadIdentityTypes =
  fmap typeViewIdentity . instanceHeadTypeViews

constructorForallBinderGeneratedIdentities :: ConstructorForallBinder -> [UniqueIdentity]
constructorForallBinderGeneratedIdentities =
  typeBinderGeneratedIdentities . constructorForallIdentity

constructorShapeGeneratedIdentities :: ConstructorShape -> [UniqueIdentity]
constructorShapeGeneratedIdentities shape =
  symbolGeneratedIdentities (constructorShapeSymbol shape)
    ++ typeViewGeneratedIdentities (constructorShapeTypeView shape)
    ++ concatMap constructorForallBinderGeneratedIdentities (constructorShapeForallBinderInfo shape)
    ++ concatMap typeParamGeneratedIdentities (constructorShapeOwnerTypeParams shape)

constructorInfoGeneratedIdentities :: ConstructorInfo -> [UniqueIdentity]
constructorInfoGeneratedIdentities ctorInfo =
  symbolGeneratedIdentities (ctorInfoSymbol ctorInfo)
    ++ symbolGeneratedIdentities (ctorOwningTypeIdentity ctorInfo)
    ++ typeViewGeneratedIdentities (ctorTypeView ctorInfo)
    ++ concatMap constructorForallBinderGeneratedIdentities (ctorForallBinderInfo ctorInfo)
    ++ concatMap constructorShapeGeneratedIdentities (ctorOwnerConstructors ctorInfo)

dataInfoGeneratedIdentities :: DataInfo -> [UniqueIdentity]
dataInfoGeneratedIdentities info =
  symbolGeneratedIdentities (dataInfoSymbol info)
    ++ concatMap typeParamGeneratedIdentities (dataTypeParams info)
    ++ concatMap constructorInfoGeneratedIdentities (dataConstructors info)

functionalDependencyGeneratedIdentities :: FunctionalDependencyInfo -> [UniqueIdentity]
functionalDependencyGeneratedIdentities info =
  foldMap typeBinderGeneratedIdentities (functionalDependencyDeterminerRefs info)
    ++ foldMap typeBinderGeneratedIdentities (functionalDependencyDeterminedRefs info)

methodInfoGeneratedIdentities :: MethodInfo -> [UniqueIdentity]
methodInfoGeneratedIdentities info =
  symbolGeneratedIdentities (methodInfoSymbol info)
    ++ typeViewGeneratedIdentities (methodTypeViewRaw info)
    ++ concatMap constraintInfoGeneratedIdentities (methodConstraintInfos info)
    ++ foldMap typeBinderGeneratedIdentities (methodParamBinderIdentities info)

classInfoGeneratedIdentities :: ClassInfo -> [UniqueIdentity]
classInfoGeneratedIdentities info =
  symbolGeneratedIdentities (classInfoSymbol info)
    ++ foldMap typeParamGeneratedIdentities (classTypeParams info)
    ++ concatMap constraintInfoGeneratedIdentities (classSuperclassInfos info)
    ++ concatMap functionalDependencyGeneratedIdentities (classFunctionalDependencies info)
    ++ concatMap methodInfoGeneratedIdentities (Map.elems (classMethodsByIdentity info))

valueInfoGeneratedIdentities :: ValueInfo -> [UniqueIdentity]
valueInfoGeneratedIdentities valueInfo =
  case valueInfo of
    OrdinaryValue {valueInfoSymbol = symbol, valueConstraintInfos = constraints} ->
      symbolGeneratedIdentities symbol
        ++ typeViewGeneratedIdentities (ordinaryValueTypeView valueInfo)
        ++ concatMap constraintInfoGeneratedIdentities constraints
    ConstructorValue {valueInfoSymbol = symbol, valueCtorInfo = ctorInfo} ->
      symbolGeneratedIdentities symbol ++ constructorInfoGeneratedIdentities ctorInfo
    OverloadedMethod {valueInfoSymbol = symbol, valueMethodInfo = methodInfo} ->
      symbolGeneratedIdentities symbol ++ methodInfoGeneratedIdentities methodInfo

instanceInfoGeneratedIdentities :: InstanceInfo -> [UniqueIdentity]
instanceInfoGeneratedIdentities info =
  symbolGeneratedIdentities (instanceClassSymbol info)
    ++ symbolGeneratedIdentities (instanceOriginModuleIdentity info)
    ++ concatMap constraintInfoGeneratedIdentities (instanceConstraintInfos info)
    ++ foldMap typeViewGeneratedIdentities (instanceHeadTypeViews info)
    ++ concatMap valueInfoGeneratedIdentities (Map.elems (instanceMethodsByIdentity info))

evidenceMethodGeneratedIdentities :: EvidenceMethod -> [UniqueIdentity]
evidenceMethodGeneratedIdentities method =
  symbolGeneratedIdentities (evidenceMethodSymbol method)
    ++ maybe [] resolvedVarGeneratedIdentities (evidenceMethodResolvedVar method)
    ++ typeViewGeneratedIdentities (evidenceMethodTypeView method)
  where
    resolvedVarGeneratedIdentities resolved =
      idDetailsGeneratedIdentities (resolvedVarDetails resolved)
        ++ generatedIdentitiesInType (resolvedVarType resolved)

evidenceInfoGeneratedIdentities :: EvidenceInfo -> [UniqueIdentity]
evidenceInfoGeneratedIdentities info =
  symbolGeneratedIdentities (evidenceClassSymbol info)
    ++ foldMap typeViewGeneratedIdentities (evidenceTypeViews info)
    ++ concatMap evidenceMethodGeneratedIdentities (Map.elems (evidenceMethodsByIdentity info))

deferredMethodEvidenceGeneratedIdentities :: DeferredMethodEvidence -> [UniqueIdentity]
deferredMethodEvidenceGeneratedIdentities evidence =
  typeViewGeneratedIdentities (deferredMethodEvidenceClassArg evidence)
    ++ foldMap typeViewGeneratedIdentities (deferredMethodEvidenceClassArgs evidence)
    ++ evidenceMethodGeneratedIdentities (deferredMethodEvidenceMethod evidence)

deferredProgramObligationGeneratedIdentities :: DeferredProgramObligation -> [UniqueIdentity]
deferredProgramObligationGeneratedIdentities obligation =
  idDetailsGeneratedIdentities (DeferredId (deferredProgramObligationRef obligation))
    ++ case obligation of
      DeferredMethod deferred ->
        methodInfoGeneratedIdentities (deferredMethodInfo deferred)
          ++ maybe [] typeViewGeneratedIdentities (deferredMethodExpectedResult deferred)
          ++ maybe [] deferredMethodEvidenceGeneratedIdentities (deferredMethodEvidence deferred)
          ++ concatMap evidenceInfoGeneratedIdentities (deferredMethodLocalEvidence deferred)
      DeferredConstructor deferred ->
        constructorInfoGeneratedIdentities (deferredConstructorInfo deferred)
          ++ concatMap symbolGeneratedIdentities (Map.elems (deferredConstructorTypeHeadIdentities deferred))
          ++ concatMap (typeBinderGeneratedIdentities . snd) (deferredConstructorInstBinders deferred)
          ++ typeBinderSubstGeneratedIdentities (deferredConstructorInitialSubst deferred)
      DeferredCase deferred ->
        dataInfoGeneratedIdentities (deferredCaseDataInfo deferred)

data LoweredBindingIdentity = LoweredBindingIdentity
  { loweredIdentityRuntimeName :: String,
    loweredIdentityDetails :: IdDetails
  }
  deriving (Show)

loweredBindingIdentityFromDetails :: String -> IdDetails -> LoweredBindingIdentity
loweredBindingIdentityFromDetails runtimeName details =
  LoweredBindingIdentity
    { loweredIdentityRuntimeName = runtimeName,
      loweredIdentityDetails = details
    }

loweredBindingIdentityFromResolvedVar :: ResolvedVar -> LoweredBindingIdentity
loweredBindingIdentityFromResolvedVar resolved =
  loweredBindingIdentityFromDetails
    (resolvedVarRuntimeName resolved)
    (resolvedVarDetails resolved)

instance Eq LoweredBindingIdentity where
  left == right =
    idDetailsSameIdentity (loweredIdentityDetails left) (loweredIdentityDetails right)

loweredBindingIdentityGeneratedIdentities :: LoweredBindingIdentity -> [UniqueIdentity]
loweredBindingIdentityGeneratedIdentities =
  idDetailsGeneratedIdentities . loweredIdentityDetails

constructorRefFromInfo :: ConstructorInfo -> ConstructorRef
constructorRefFromInfo ctor =
  constructorRefFromSymbol (ctorInfoSymbol ctor)

loweredBindingIdentityFromConstructorInfo :: ConstructorInfo -> LoweredBindingIdentity
loweredBindingIdentityFromConstructorInfo ctor =
  loweredBindingIdentityFromDetails
    (ctorRuntimeName ctor)
    (ConstructorId (constructorRefFromInfo ctor))

loweredBindingIdentityFromValueInfo :: ValueInfo -> LoweredBindingIdentity
loweredBindingIdentityFromValueInfo valueInfo =
  case valueInfo of
    OrdinaryValue
      { valueRuntimeName = runtimeName,
        valueInfoSymbol = symbol
      } ->
        loweredBindingIdentityFromDetails runtimeName (TopLevelId symbol)
    ConstructorValue {valueCtorInfo = ctor} ->
      loweredBindingIdentityFromConstructorInfo ctor
    OverloadedMethod
      { valueInfoSymbol = symbol
      } ->
      loweredBindingIdentityFromDetails (symbolDefiningName symbol) (MethodId symbol)

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
    deferredConstructorTypeHeadIdentities :: Map String SymbolIdentity,
    deferredConstructorInstBinders :: [(String, TypeBinderIdentity)],
    deferredConstructorInitialSubst :: TypeBinderSubst,
    deferredConstructorBindingMode :: DeferredBindingMode
  }
  deriving (Show)

instance Eq DeferredConstructorCall where
  left == right =
    deferredConstructorRef left == deferredConstructorRef right
      && deferredConstructorInfo left == deferredConstructorInfo right
      && deferredConstructorArgCount left == deferredConstructorArgCount right
      && deferredConstructorSourceType left == deferredConstructorSourceType right
      && deferredConstructorOccurrenceType left == deferredConstructorOccurrenceType right
      && typeHeadIdentityMapMatches (deferredConstructorTypeHeadIdentities left) (deferredConstructorTypeHeadIdentities right)
      && typeBinderIdentityEntryListMatches (deferredConstructorInstBinders left) (deferredConstructorInstBinders right)
      && deferredConstructorInitialSubst left == deferredConstructorInitialSubst right
      && deferredConstructorBindingMode left == deferredConstructorBindingMode right

typeHeadIdentityMapMatches :: Map String SymbolIdentity -> Map String SymbolIdentity -> Bool
typeHeadIdentityMapMatches left right =
  Set.fromList (Map.elems left) == Set.fromList (Map.elems right)

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
  deriving (Show)

instance Eq ExportedTypeInfo where
  left == right =
    exportedTypeData left == exportedTypeData right
      && exportedTypeConstructorsByIdentity left == exportedTypeConstructorsByIdentity right

mkExportedTypeInfo :: DataInfo -> [(String, ConstructorInfo)] -> ExportedTypeInfo
mkExportedTypeInfo dataInfo constructors =
  ExportedTypeInfo
    { exportedTypeData = dataInfo,
      exportedTypeConstructorsByIdentity =
        uniqueInfoEntriesByIdentity [(ctorInfoSymbol ctorInfo, ctorInfo) | (_, ctorInfo) <- constructors],
      exportedTypeConstructorDisplaysByIdentity =
        uniqueDisplayEntriesByIdentity [(ctorInfoSymbol ctorInfo, displayName) | (displayName, ctorInfo) <- constructors]
    }

exportedTypeConstructorsForDisplay :: ExportedTypeInfo -> Map String ConstructorInfo
exportedTypeConstructorsForDisplay typeInfo =
  displayMap (exportedTypeConstructorsByIdentity typeInfo) (exportedTypeConstructorDisplaysByIdentity typeInfo)

data ModuleExports = ModuleExports
  { exportedValuesByIdentity :: Map SymbolIdentity ValueInfo,
    exportedValueDisplaysByIdentity :: Map SymbolIdentity String,
    exportedTypesByIdentity :: Map SymbolIdentity ExportedTypeInfo,
    exportedTypeDisplaysByIdentity :: Map SymbolIdentity String,
    exportedClassesByIdentity :: Map SymbolIdentity ClassInfo,
    exportedClassDisplaysByIdentity :: Map SymbolIdentity String
  }
  deriving (Show)

instance Eq ModuleExports where
  left == right =
    exportedValuesByIdentity left == exportedValuesByIdentity right
      && exportedTypesByIdentity left == exportedTypesByIdentity right
      && exportedClassesByIdentity left == exportedClassesByIdentity right

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
      uniqueInfoByIdentity identityFor values

    indexDisplay identityFor values =
      uniqueDisplayByIdentity identityFor values

exportedValuesForDisplay :: ModuleExports -> Map String ValueInfo
exportedValuesForDisplay exports =
  displayMap (exportedValuesByIdentity exports) (exportedValueDisplaysByIdentity exports)

exportedTypesForDisplay :: ModuleExports -> Map String ExportedTypeInfo
exportedTypesForDisplay exports =
  displayMap (exportedTypesByIdentity exports) (exportedTypeDisplaysByIdentity exports)

exportedClassesForDisplay :: ModuleExports -> Map String ClassInfo
exportedClassesForDisplay exports =
  displayMap (exportedClassesByIdentity exports) (exportedClassDisplaysByIdentity exports)

uniqueInfoByIdentity :: (Eq a) => (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity a
uniqueInfoByIdentity identityFor values =
  uniqueInfoListByIdentity identityFor (Map.elems values)

uniqueInfoListByIdentity :: (Eq a) => (a -> SymbolIdentity) -> [a] -> Map SymbolIdentity a
uniqueInfoListByIdentity identityFor values =
  uniqueInfoEntriesByIdentity [(identityFor info, info) | info <- values]

uniqueInfoEntriesByIdentity :: (Eq a) => [(SymbolIdentity, a)] -> Map SymbolIdentity a
uniqueInfoEntriesByIdentity entries =
  Map.fromList
    [ (identity, info)
    | (identity, info : rest) <- Map.toList infosByIdentity,
      all (== info) rest
    ]
  where
    infosByIdentity =
      Map.fromListWith
        (++)
        [(identity, [info]) | (identity, info) <- entries]

uniqueDisplayByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity String
uniqueDisplayByIdentity identityFor values =
  uniqueDisplayEntriesByIdentity
    [ (identityFor info, displayName)
    | (displayName, info) <- Map.toList values
    ]

uniqueDisplayEntriesByIdentity :: [(SymbolIdentity, String)] -> Map SymbolIdentity String
uniqueDisplayEntriesByIdentity entries =
  Map.fromList
    [ (identity, displayName)
    | (identity, displayName : rest) <- Map.toList displayNamesByIdentity,
      all (== displayName) rest
    ]
  where
    displayNamesByIdentity =
      Map.fromListWith
        (++)
        [(identity, [displayName]) | (identity, displayName) <- entries]

displayMap :: Map SymbolIdentity a -> Map SymbolIdentity String -> Map String a
displayMap values displays =
  Map.fromList
    [ (displayName, value)
    | (displayName, [value]) <- Map.toList valuesByDisplay
    ]
  where
    valuesByDisplay =
      Map.fromListWith
        (++)
        [ (displayName, [value])
        | (identity, value) <- Map.toList values,
          Just displayName <- [Map.lookup identity displays]
        ]

data LoweredBinding = LoweredBinding
  { loweredBindingIdentity :: LoweredBindingIdentity,
    loweredBindingSourceType :: SrcType,
    loweredBindingSourceTypeView :: Maybe TypeView,
    loweredBindingExpectedType :: SrcType,
    loweredBindingExpectedTypeView :: Maybe TypeView,
    loweredBindingSurfaceExpr :: SurfaceExpr,
    loweredBindingResolvedLocalIdentities :: [LoweredResolvedLocalIdentity],
    loweredBindingDeferredObligations :: DeferredObligations,
    loweredBindingExternalTypeViews :: Map String TypeView,
    loweredBindingEvidenceParamCount :: Int,
    loweredBindingExportedAsMain :: Bool
  }
  deriving (Eq, Show)

data LoweredResolvedLocalIdentity = LoweredResolvedLocalIdentity
  { loweredResolvedLocalRuntimeRef :: LocalRef,
    loweredResolvedLocalRef :: LocalRef
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
    checkedBindingSourceTypeView :: TypeView,
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

checkedBindingSourceType :: CheckedBinding -> SrcType
checkedBindingSourceType =
  typeViewDisplay . checkedBindingSourceTypeView

checkedBindingSourceTypeIdentity :: CheckedBinding -> SrcType
checkedBindingSourceTypeIdentity =
  typeViewIdentity . checkedBindingSourceTypeView

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
  deriving (Show)

instance Eq CheckedModule where
  left == right =
    checkedModuleIdentity left == checkedModuleIdentity right
      && checkedModuleBindings left == checkedModuleBindings right
      && checkedModuleData left == checkedModuleData right
      && checkedModuleClasses left == checkedModuleClasses right
      && checkedModuleInstances left == checkedModuleInstances right
      && checkedModuleExports left == checkedModuleExports right

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

dataParamBinderIdentities :: DataInfo -> [TypeBinderIdentity]
dataParamBinderIdentities =
  map requiredIdentity . dataTypeParams
  where
    requiredIdentity param =
      case typeParamBinderIdentity param of
        Just identity -> identity
        Nothing -> error ("checked data parameter `" ++ P.typeParamName param ++ "` is missing identity")

dataParamBinders :: DataInfo -> [(String, TypeBinderIdentity)]
dataParamBinders info =
  zip (dataParams info) (dataParamBinderIdentities info)

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

constructorOwnerRuntimeTypeTrackable :: Map SymbolIdentity DataInfo -> ConstructorInfo -> Bool
constructorOwnerRuntimeTypeTrackable dataInfosByIdentity ctor =
  case Map.lookup (ctorOwningTypeIdentity ctor) dataInfosByIdentity of
    Just dataInfo -> dataConstructorsRuntimeTypeTrackable dataInfo
    Nothing -> all constructorShapeRuntimeTypeTrackable (constructorOwnerShapes ctor)

constructorOwnerHasVariableHeadApplication :: Map SymbolIdentity DataInfo -> ConstructorInfo -> Bool
constructorOwnerHasVariableHeadApplication dataInfosByIdentity ctor =
  case Map.lookup (ctorOwningTypeIdentity ctor) dataInfosByIdentity of
    Just dataInfo -> any constructorRuntimeTypeHasVariableHeadApplication (dataConstructors dataInfo)
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
      constructorShapeTypeView = ctorTypeView ctor,
      constructorShapeForallBinderInfo = ctorForallBinderInfo ctor,
      constructorShapeIndex = ctorIndex ctor,
      constructorShapeOwnerTypeParams = []
    }

constructorOwnerDataInfoFromShapes :: ConstructorInfo -> DataInfo
constructorOwnerDataInfoFromShapes ctorInfo =
  DataInfo
    { dataInfoSymbol = ownerIdentity,
      dataTypeParams = typeParams,
      dataConstructors = constructors
    }
  where
    ownerIdentity = ctorOwningTypeIdentity ctorInfo
    ownerShapes = constructorOwnerShapes ctorInfo
    typeParams =
      case [params | shape <- ownerShapes, let params = constructorShapeOwnerTypeParams shape, not (null params)] of
        params : _ -> params
        [] -> inferredConstructorOwnerTypeParams ctorInfo ownerShapes
    constructors = map constructorInfoFromShape ownerShapes

    constructorInfoFromShape shape =
      ConstructorInfo
        { ctorInfoSymbol = constructorShapeSymbol shape,
          ctorRuntimeName = constructorShapeRuntimeName shape,
          ctorTypeView = constructorShapeTypeView shape,
          ctorForallBinderInfo = constructorShapeForallBinderInfo shape,
          ctorOwningTypeIdentity = ownerIdentity,
          ctorIndex = constructorShapeIndex shape,
          ctorOwnerConstructors = ownerShapes
        }

inferredConstructorOwnerTypeParams :: ConstructorInfo -> [ConstructorShape] -> [P.TypeParam]
inferredConstructorOwnerTypeParams ctorInfo ownerShapes =
  [ maybe (P.TypeParam name kind0) (`P.ResolvedTypeParam` kind0) (Map.lookup name paramRefs)
  | name <- inferredConstructorOwnerParamNames ctorInfo ownerShapes paramRefs
  , let kind0 = kindForName name
  ]
  where
    paramArities = foldMap constructorShapeVariableHeadArities ownerShapes
    paramRefs = inferredConstructorOwnerParamRefs ctorInfo ownerShapes

    kindForName name =
      kindFromMaxApplicationArity $
        maximum
          ( 0 :
            [ Map.findWithDefault 0 alias paramArities
            | alias <- ownerParamNameAliases name
            ]
          )

    ownerParamNameAliases name =
      case Map.lookup name paramRefs of
        Just ref -> [alias | (alias, aliasRef) <- Map.toList paramRefs, aliasRef == ref]
        Nothing -> [name]

inferredConstructorOwnerParamRefs :: ConstructorInfo -> [ConstructorShape] -> Map String ResolvedTypeBinderRef
inferredConstructorOwnerParamRefs ctorInfo ownerShapes =
  Map.mapMaybe singleRef refsByName
  where
    refsByName =
      Map.fromListWith
        Set.union
        [ (name, Set.singleton ref)
        | shape <- ownerShapes,
          (name, ref) <- constructorOwnerResultArgRefs ctorInfo shape
        ]

    singleRef refs =
      case Set.toList refs of
        [ref] -> Just ref
        _ -> Nothing

constructorOwnerResultArgRefs :: ConstructorInfo -> ConstructorShape -> [(String, ResolvedTypeBinderRef)]
constructorOwnerResultArgRefs ctorInfo shape =
  [ (displayName, ref)
  | (displayName, identity) <-
      constructorOwnerResultArgPairs ctorInfo (constructorShapeResultView shape),
    Just ref <- [Map.lookup identity refsByIdentity]
  ]
  where
    refsByIdentity =
      Map.fromList
        [ (constructorForallIdentity binder, resolvedTypeBinderRefFromIdentity (constructorForallIdentity binder) (constructorForallDisplayName binder))
        | binder <- constructorShapeForallBinderInfo shape
        ]

constructorOwnerResultArgPairs :: ConstructorInfo -> TypeView -> [(String, TypeBinderIdentity)]
constructorOwnerResultArgPairs ctorInfo view =
  case (constructorOwnerResultArgs ctorInfo view (typeViewDisplay view), constructorOwnerResultArgs ctorInfo view (typeViewIdentity view)) of
    (Just displayArgs, Just identityArgs) ->
      [ (displayName, identity)
      | (displayArg, identityArg) <- zip (NE.toList displayArgs) (NE.toList identityArgs),
        Just displayName <- [srcTypeVarName displayArg],
        Just identityName <- [srcTypeVarName identityArg],
        Just identity <- [typeViewBinderIdentityForAlias view identityName]
      ]
    _ -> []

inferredConstructorOwnerParamNames :: ConstructorInfo -> [ConstructorShape] -> Map String ResolvedTypeBinderRef -> [String]
inferredConstructorOwnerParamNames ctorInfo ownerShapes paramRefs =
  case transpose (map shapeOwnerDisplayArgs ownerShapes) of
    [] -> map fst (constructorOwnerResultArgPairs ctorInfo (constructorShapeResultView (constructorShapeFromInfo ctorInfo)))
    columns -> mapMaybe firstName columns
  where
    shapeOwnerDisplayArgs shape =
      map fst (constructorOwnerResultArgPairs ctorInfo (constructorShapeResultView shape))

    firstName names =
      case Set.toList (Set.fromList [ref | name <- names, Just ref <- [Map.lookup name paramRefs]]) of
        [_] ->
          case filter (`Map.member` paramRefs) names of
            name : _ -> Just name
            [] -> Nothing
        [] ->
          case names of
            name : _ -> Just name
            [] -> Nothing
        _ -> Nothing

constructorOwnerResultArgs :: ConstructorInfo -> TypeView -> SrcType -> Maybe (NonEmpty SrcType)
constructorOwnerResultArgs ctorInfo view ty =
  case ty of
    STBase name
      | ownerHeadMatches name ->
          Nothing
    STCon name args
      | ownerHeadMatches name ->
          Just args
    _ -> Nothing
  where
    ownerIdentity = ctorOwningTypeIdentity ctorInfo
    ownerHeadMatches name =
      typeViewHeadIdentityForAlias view name == Just ownerIdentity

srcTypeVarName :: SrcType -> Maybe String
srcTypeVarName ty =
  case ty of
    STVar name -> Just name
    _ -> Nothing

constructorShapeVariableHeadArities :: ConstructorShape -> Map String Int
constructorShapeVariableHeadArities shape =
  foldMap
    srcTypeVariableHeadArities
    ( constructorShapeArgs shape
        ++ [constructorShapeResult shape]
        ++ [bound | (_, Just bound) <- constructorShapeForalls shape]
    )

srcTypeVariableHeadArities :: SrcType -> Map String Int
srcTypeVariableHeadArities ty =
  case ty of
    STVar {} -> Map.empty
    STArrow dom cod -> srcTypeVariableHeadArities dom <> srcTypeVariableHeadArities cod
    STBase {} -> Map.empty
    STCon _ args -> foldMap srcTypeVariableHeadArities (NE.toList args)
    STVarApp name args ->
      Map.singleton name (NE.length args)
        <> foldMap srcTypeVariableHeadArities (NE.toList args)
    STTyLam _ body -> srcTypeVariableHeadArities body
    STTyApp fun arg -> srcTypeVariableHeadArities fun <> srcTypeVariableHeadArities arg
    STForall _ mb body ->
      maybe Map.empty (srcTypeVariableHeadArities . unSrcBound) mb
        <> srcTypeVariableHeadArities body
    STMu _ body -> srcTypeVariableHeadArities body
    STBottom -> Map.empty

kindFromMaxApplicationArity :: Int -> P.SrcKind
kindFromMaxApplicationArity arity =
  foldr P.KArrow P.KType (replicate arity P.KType)

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

classParamBinderIdentities :: ClassInfo -> NonEmpty TypeBinderIdentity
classParamBinderIdentities =
  fmap requiredIdentity . classTypeParams
  where
    requiredIdentity param =
      case typeParamBinderIdentity param of
        Just identity -> identity
        Nothing -> error ("checked class parameter `" ++ P.typeParamName param ++ "` is missing identity")

classParamBinders :: ClassInfo -> NonEmpty (String, TypeBinderIdentity)
classParamBinders info =
  NE.zip (classParamNames info) (classParamBinderIdentities info)

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
  methodDisplayName

methodParamNames :: MethodInfo -> NonEmpty String
methodParamNames =
  fmap fst . methodParamBinders

methodParamName :: MethodInfo -> String
methodParamName =
  NE.head . methodParamNames

methodParamBinderIdentities :: MethodInfo -> NonEmpty TypeBinderIdentity
methodParamBinderIdentities =
  fmap snd . methodParamBinders

methodType :: MethodInfo -> SrcType
methodType =
  typeViewDisplay . methodTypeViewRaw

methodTypeIdentity :: MethodInfo -> SrcType
methodTypeIdentity =
  typeViewIdentity . methodTypeViewRaw

lookupMethodParamViewSubst :: MethodInfo -> TypeViewSubst -> Maybe (NonEmpty TypeView)
lookupMethodParamViewSubst methodInfo subst =
  traverse lookupParam (methodParamBinderIdentities methodInfo)
  where
    lookupParam identity =
      lookupTypeViewSubst (TypeViewSubstByIdentity identity) subst

methodTypeView :: MethodInfo -> TypeView
methodTypeView methodInfo =
  (methodTypeViewRaw methodInfo)
    { typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities (methodTypeViewRaw methodInfo),
            methodParamStableBinderIdentities methodInfo
          ]
    }

methodParamStableBinderIdentities :: MethodInfo -> Map String TypeBinderIdentity
methodParamStableBinderIdentities methodInfo =
  Map.fromList
    [ (typeBinderIdentityStableName identity, identity)
    | identity <- NE.toList (methodParamBinderIdentities methodInfo)
    ]

typeViewArrowArgViews :: TypeView -> [TypeView]
typeViewArrowArgViews view =
  zipWith (projectTypeView view) displayParamTys identityParamTys
  where
    (_, displayBodyTy) = splitForalls (typeViewDisplay view)
    (displayParamTys, _) = splitArrows displayBodyTy
    (_, identityBodyTy) = splitForalls (typeViewIdentity view)
    (identityParamTys, _) = splitArrows identityBodyTy

typeViewDirectArrowDomainView :: TypeView -> Maybe TypeView
typeViewDirectArrowDomainView view =
  case (typeViewDisplay view, typeViewIdentity view) of
    (STArrow displayDom _, STArrow identityDom _) ->
      Just (projectTypeView view displayDom identityDom)
    _ ->
      Nothing

typeViewDirectArrowCodomainView :: TypeView -> Maybe TypeView
typeViewDirectArrowCodomainView view =
  case (typeViewDisplay view, typeViewIdentity view) of
    (STArrow _ displayCod, STArrow _ identityCod) ->
      Just (projectTypeView view displayCod identityCod)
    _ ->
      Nothing

typeViewArrowResultView :: TypeView -> TypeView
typeViewArrowResultView view =
  projectTypeView view displayResult identityResult
  where
    (_, displayBody) = splitForalls (typeViewDisplay view)
    (_, identityBody) = splitForalls (typeViewIdentity view)
    (_, displayResult) = splitArrows displayBody
    (_, identityResult) = splitArrows identityBody

typeViewArrowResultViewForArity :: TypeView -> Int -> TypeView
typeViewArrowResultViewForArity view argCount =
  projectTypeView view displayTy identityTy
  where
    (displayArgs, displayResult) = splitArrows (snd (splitForalls (typeViewDisplay view)))
    (identityArgs, identityResult) = splitArrows (snd (splitForalls (typeViewIdentity view)))
    displayTy = foldr STArrow displayResult (drop argCount displayArgs)
    identityTy = foldr STArrow identityResult (drop argCount identityArgs)

methodParamTypeViews :: TypeView -> [TypeView]
methodParamTypeViews =
  typeViewArrowArgViews

methodResultTypeView :: MethodInfo -> TypeView
methodResultTypeView methodInfo =
  methodResultTypeViewFrom (methodTypeView methodInfo)

methodResultTypeViewFrom :: TypeView -> TypeView
methodResultTypeViewFrom =
  typeViewArrowResultView

projectTypeView :: TypeView -> SrcType -> SrcType -> TypeView
projectTypeView view displayTy identityTy =
  TypeView
    { typeViewDisplay = displayTy,
      typeViewIdentity = identityTy,
      typeViewHeadIdentities =
        filterHeadIdentitiesByTypeNames displayTy identityTy (typeViewHeadIdentities view),
      typeViewBinderIdentities = typeViewBinderIdentities view
    }

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
    (methodName methodInfo)
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
  symbolIdentityFromParts identity SymbolModule moduleName moduleName Nothing

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

typeHeadNamesSrcType :: SrcType -> Set String
typeHeadNamesSrcType =
  \case
    STVar {} -> Set.empty
    STArrow dom cod -> typeHeadNamesSrcType dom `Set.union` typeHeadNamesSrcType cod
    STBase name -> Set.singleton name
    STCon name args -> Set.insert name (foldMap typeHeadNamesSrcType args)
    STVarApp _ args -> foldMap typeHeadNamesSrcType args
    STTyLam _ body -> typeHeadNamesSrcType body
    STTyApp fun arg -> typeHeadNamesSrcType fun `Set.union` typeHeadNamesSrcType arg
    STForall _ mb body ->
      maybe Set.empty (typeHeadNamesSrcType . unSrcBound) mb `Set.union` typeHeadNamesSrcType body
    STMu _ body -> typeHeadNamesSrcType body
    STBottom -> Set.empty

specializeMethodTypeView :: MethodInfo -> NonEmpty TypeView -> TypeView
specializeMethodTypeView methodInfo classArgViews =
  specialized
    { typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps (typeViewBinderIdentities view : map typeViewBinderIdentities (NE.toList classArgViews))
    }
  where
    view = methodTypeView methodInfo
    subst = typeViewSubstFromParamIdentities (methodParamBinderIdentities methodInfo) classArgViews
    specialized = specializeQuantifiedTypeView subst view

specializeQuantifiedTypeView :: TypeViewSubst -> TypeView -> TypeView
specializeQuantifiedTypeView subst view =
  TypeView
    { typeViewDisplay = displayTy,
      typeViewIdentity = identityTy,
      typeViewHeadIdentities =
        filterHeadIdentitiesByTypeNames displayTy identityTy substitutedHeadIdentities,
      typeViewBinderIdentities =
        filterBinderIdentitiesByNames
          (freeIdentityNames <> freeDisplayNames)
          substitutedBinderIdentityAliases
          substitutedBinderIdentities
    }
  where
    displayTy = specialize (typeViewDisplay view) (typeViewSubstDisplayTypes view subst)
    identityTy = specialize (typeViewIdentity view) (typeViewIdentitySubstTypesFor view subst)
    freeDisplayNames = freeTypeVarsSrcType displayTy
    freeIdentityNames = freeTypeVarsSrcType identityTy
    substitutedHeadIdentities =
      mergeSymbolIdentityMaps (typeViewHeadIdentities view : map typeViewHeadIdentities (Map.elems subst))
    substitutedBinderIdentities =
      mergeTypeBinderIdentityMaps (typeViewBinderIdentities view : map typeViewBinderIdentities (Map.elems subst))
    substitutedBinderIdentityAliases =
      concatMap typeViewBinderIdentityAliasEntries (view : Map.elems subst)

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
