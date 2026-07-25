{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module MLF.Frontend.Program.Types
  ( ProgramError (..),
    ProgramSourceTypeShape (..),
    ProgramDiagnostic (..),
    diagnosticForProgramError,
    renderProgramDiagnostic,
    TypeView,
    TypeViewNodeView (..),
    typeViewNodeView,
    splitTypeViewHeadApplication,
    typeViewDisplay,
    typeViewIdentity,
    typeViewHeadIdentities,
    typeViewBinderIdentities,
    typeViewRootHeadIdentity,
    TypeViewConstructionError (..),
    TypeViewShapeError (..),
    typeViewFromSourceType,
    typeViewWithIdentityAliases,
    typeViewWithBinderIdentityAliases,
    typeViewMergeHeadIdentityAliases,
    typeViewMergeBinderIdentityAliases,
    typeViewBottom,
    requireTypeViewFromSourceType,
    typeViewWithDisplay,
    typeViewOverlayDisplay,
    mapTypeViewDisplayHeadNames,
    mapTypeViewDisplayBinderNames,
    typeViewArrow,
    typeViewForallBinderViews,
    typeViewAddArgumentsInsideForalls,
    typeViewQuantifyBinders,
    typeViewRebuildArrowBody,
    quantifyFreeTypeView,
    stripVacuousTypeViewForalls,
    ConstraintInfo (..),
    ClassApplicationKey,
    constraintClassApplicationKey,
    classApplicationKey,
    EvidenceMethodKey,
    evidenceMethodKey,
    constraintTypeView,
    typeViewFromResolved,
    typeViewFromElabType,
    typeViewToResolved,
    resolvedSourceTypeToElabType,
    displayConstraint,
    applyTypeViewSubst,
    applyConstraintInfoSubst,
    freeTypeBinderIdentitiesTypeView,
    freeTypeBinderIdentitiesTypeViews,
    freeTypeBinderDisplayNamesTypeView,
    typeViewIsBareBinderIdentity,
    typeViewMentionsFreeBinderIdentity,
    typeViewHeadIdentityForAlias,
    typeViewHeadIdentityLookupAliases,
    typeViewBinderIdentityForAlias,
    typeViewBinderIdentityAliasEntries,
    typeViewMentionedHeadIdentities,
    typeHeadNamesSrcType,
    typeViewsDisplay,
    typeViewsIdentity,
    TypeViewSubst,
    typeViewSubstKeyFor,
    lookupTypeViewSubst,
    insertTypeViewSubst,
    typeViewSubstDisplayTypes,
    typeViewSubstFromParamIdentities,
    typeParamBinderIdentity,
    CheckedTypeParam (..),
    checkedTypeParamName,
    checkedTypeParamIdentity,
    checkedTypeParamIsFirstOrder,
    mergeUniquePairMaps,
    TypeBinderSubst,
    typeBinderSubstViews,
    emptyTypeBinderSubst,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToTypeViewSubst,
    lookupTypeBinderSubstViewByIdentity,
    insertTypeBinderSubstView,
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
    valueInfoRuntimeName,
    valueInfoRuntimeDetails,
    valueInfoRawRuntimeName,
    valueInfoIdentityRuntimeAliases,
    valueInfoRuntimeAliases,
    valueInfoIdentityName,
    valueType,
    valueIdentityType,
    ordinaryValueTypeView,
    dataInfoSymbolIdentity,
    dataInfoHeadIdentityLookupAliases,
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
    constructorInfoRuntimeName,
    constructorInfoHeadIdentityLookupAliases,
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
    typeViewHeadArgViews,
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
    constructorShapeForallBinderInfo,
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
    ctorForallBinderInfo,
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
    loweredBindingIdentityFromTopLevel,
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
    deferredMethodTotalArgCount,
    deferredMethodResolutionArgCount,
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
    uniqueDisplayNamesByIdentity,
    exportedValuesForDisplay,
    exportedTypesForDisplay,
    exportedClassesForDisplay,
    LoweredBinding (..),
    loweredBindingName,
    loweredBindingSourceType,
    loweredBindingExpectedType,
    CheckedBinding (..),
    checkedBindingName,
    checkedBindingSourceType,
    checkedBindingSourceTypeIdentity,
    checkedBindingGeneratedIdentities,
    checkedBindingsIdentityGenerator,
    CheckedModule (..),
    checkedModuleGeneratedIdentities,
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
    specializeQuantifiedTypeView,
    constrainedVisibleTypeView,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (isPrefixOf, nub, sort, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Types (XmlfTerm, ElabType, ResolvedVar (..), generatedIdentitiesInTerm, generatedIdentitiesInType, resolvedVarConstructorRef, resolvedVarRuntimeName)
import qualified MLF.Elab.Types as X
import MLF.Frontend.Symbol
  ( ResolvedReference,
    ResolvedReferenceKind (..),
    ResolvedSymbol,
    SymbolIdentity,
    SymbolIdentityPayloadKey,
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
    sameSymbolIdentity,
    lookupSymbolIdentityAlias,
    lookupSymbolIdentityExact,
    symbolIdentityAliasMap,
    symbolIdentityAliasMapWith,
    symbolIdentityAliasNamesWith,
    symbolIdentityPayloadKey,
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
    ResolvedSurfaceExpr,
    resolvedTypeBinderIdentity,
    resolvedTypeBinderName,
    resolvedTypeBinderRefFromIdentity,
    resolvedTypeBinderTypeIdentity,
  )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Frontend.TypeLevel (TypeLevelKind (..), TypeLevelNormalizeError (..), TypeLevelTy (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity

data ProgramSourceTypeShape
  = ProgramSourceArrowShape
  deriving (Eq, Show)

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
  | ProgramTypeMismatchWithCause SrcType SrcType ProgramError
  | ProgramTypeShapeMismatch ProgramSourceTypeShape SrcType
  | ProgramTypeShapeMismatchWithCause ProgramSourceTypeShape SrcType ProgramError
  | ProgramCaseOnNonDataType SrcType
  | ProgramDeferredCaseArityMismatch DeferredCaseCall Int
  | ProgramDeferredCaseBottomScrutinee DeferredCaseCall SrcType
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
  let (surfaceError, internalDetails) = programDiagnosticProjection err
   in ProgramDiagnostic
        { diagnosticError = surfaceError,
          diagnosticSpan = mbLocated >>= spanForError surfaceError . P.locatedProgramSpans,
          diagnosticMessage =
            programErrorMessage surfaceError
              ++ concatMap ("\n" ++) internalDetails,
          diagnosticHints = programErrorHints surfaceError
        }

-- | Keep a source-facing mismatch as the diagnostic classification while
-- retaining the compiler failure that proved the exact annotation could not
-- be constructed.  Raw 'ProgramError' callers can inspect the structured
-- cause; located/CLI consumers see the corresponding type-mismatch surface
-- plus the internal detail needed for debugging.
programDiagnosticProjection :: ProgramError -> (ProgramError, [String])
programDiagnosticProjection err =
  case err of
    ProgramTypeMismatchWithCause actual expected cause ->
      ( ProgramTypeMismatch actual expected,
        ["internal pipeline detail: " ++ show cause]
      )
    ProgramTypeShapeMismatchWithCause shape expected cause ->
      ( ProgramTypeShapeMismatch shape expected,
        ["internal pipeline detail: " ++ show cause]
      )
    _ -> (err, [])

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
    ProgramTypeMismatchWithCause actual expected cause ->
      programErrorMessage (ProgramTypeMismatch actual expected)
        ++ "\ninternal pipeline detail: "
        ++ show cause
    ProgramTypeShapeMismatch ProgramSourceArrowShape expected ->
      "type mismatch: expected `" ++ show expected ++ "`, got a function"
    ProgramTypeShapeMismatchWithCause shape expected cause ->
      programErrorMessage (ProgramTypeShapeMismatch shape expected)
        ++ "\ninternal pipeline detail: "
        ++ show cause
    ProgramCaseOnNonDataType ty -> "case scrutinee is not a data type: `" ++ show ty ++ "`"
    ProgramDeferredCaseArityMismatch deferred actual ->
      "binding `"
        ++ loweredIdentityRuntimeName (deferredCaseBindingIdentity deferred)
        ++ "`: "
        ++ "deferred case `"
        ++ deferredRefName (deferredCaseRef deferred)
        ++ "` for `"
        ++ symbolIdentityStableName (dataInfoSymbol (deferredCaseDataInfo deferred))
        ++ "` expects "
        ++ show (deferredCaseExpectedArgCount deferred)
        ++ " arguments, got "
        ++ show actual
    ProgramDeferredCaseBottomScrutinee deferred expected ->
      "binding `"
        ++ loweredIdentityRuntimeName (deferredCaseBindingIdentity deferred)
        ++ "`: "
        ++ "deferred case `"
        ++ deferredRefName (deferredCaseRef deferred)
        ++ "` for `"
        ++ symbolIdentityStableName (dataInfoSymbol (deferredCaseDataInfo deferred))
        ++ "` resolved its scrutinee to `STBottom`; expected `"
        ++ show expected
        ++ "`"
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
    ProgramTypeMismatchWithCause {} ->
      ["check the nearest annotation; `.mlfp` uses eMLF inference before resolving program obligations"]
    ProgramTypeShapeMismatch {} ->
      ["check the nearest annotation; `.mlfp` uses eMLF inference before resolving program obligations"]
    ProgramTypeShapeMismatchWithCause {} ->
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

data TypeViewName identity = TypeViewName
  { typeViewNameDisplay :: String,
    typeViewNamePayload :: identity,
    typeViewNameReferences :: Map String identity,
    typeViewNameStructuralHeads :: Map String SymbolIdentity
  }
  deriving (Show)

data TypeViewType
  = TypeViewVar (TypeViewName TypeBinderIdentity)
  | TypeViewArrow TypeViewType TypeViewType
  | TypeViewBase (TypeViewName SymbolIdentity)
  | TypeViewCon (TypeViewName SymbolIdentity) (NonEmpty TypeViewType)
  | TypeViewVarApp (TypeViewName TypeBinderIdentity) (NonEmpty TypeViewType)
  | TypeViewTyLam (TypeViewName TypeBinderIdentity) TypeViewType
  | TypeViewTyApp TypeViewType TypeViewType
  | TypeViewForall (TypeViewName TypeBinderIdentity) (Maybe TypeViewType) TypeViewType
  | TypeViewMu (TypeViewName TypeBinderIdentity) TypeViewType
  | TypeViewContextHead (TypeViewName SymbolIdentity) TypeViewType
  | TypeViewContextBinder (TypeViewName TypeBinderIdentity) TypeViewType
  | TypeViewBottom
  deriving (Show)

newtype TypeView = TypeViewNode TypeViewType

data TypeViewNodeView
  = TypeViewVarNode String TypeBinderIdentity
  | TypeViewArrowNode TypeView TypeView
  | TypeViewBaseNode String SymbolIdentity
  | TypeViewConNode String SymbolIdentity (NonEmpty TypeView)
  | TypeViewVarAppNode String TypeBinderIdentity (NonEmpty TypeView)
  | TypeViewTyLamNode String TypeBinderIdentity TypeView
  | TypeViewTyAppNode TypeView TypeView
  | TypeViewForallNode String TypeBinderIdentity (Maybe TypeView) TypeView
  | TypeViewMuNode String TypeBinderIdentity TypeView
  | TypeViewBottomNode
  deriving (Eq, Show)

data TypeViewConstructionError
  = TypeViewMissingHeadIdentity String
  | TypeViewAmbiguousHeadIdentity String [SymbolIdentity]
  | TypeViewMissingBinderIdentity String
  | TypeViewAmbiguousBinderIdentity String [TypeBinderIdentity]
  deriving (Eq, Show)

data TypeViewShapeError = TypeViewShapeMismatch SrcType SrcType
  deriving (Eq, Show)

instance Show TypeView where
  show view =
    "TypeView {typeViewDisplay = "
      ++ show (typeViewDisplay view)
      ++ ", typeViewIdentity = "
      ++ show (typeViewIdentity view)
      ++ ", typeViewHeadIdentities = "
      ++ show (typeViewHeadIdentities view)
      ++ ", typeViewBinderIdentities = "
      ++ show (typeViewBinderIdentities view)
      ++ "}"

typeViewDisplay :: TypeView -> SrcType
typeViewDisplay (TypeViewNode ty) =
  typeViewTypeDisplay ty

typeViewIdentity :: TypeView -> SrcType
typeViewIdentity (TypeViewNode ty) =
  typeViewTypeIdentity ty

typeViewHeadIdentities :: TypeView -> Map String SymbolIdentity
typeViewHeadIdentities (TypeViewNode ty) =
  typeViewTypeHeadIdentities ty

typeViewBinderIdentities :: TypeView -> Map String TypeBinderIdentity
typeViewBinderIdentities (TypeViewNode ty) =
  typeViewTypeBinderIdentities ty

typeViewRootHeadIdentity :: TypeView -> Maybe SymbolIdentity
typeViewRootHeadIdentity (TypeViewNode ty) =
  go ty
  where
    go typeNode =
      case typeNode of
        TypeViewBase name -> Just (typeViewNamePayload name)
        TypeViewCon name _ -> Just (typeViewNamePayload name)
        TypeViewContextHead _ body -> go body
        TypeViewContextBinder _ body -> go body
        _ -> Nothing

typeViewNodeView :: TypeView -> TypeViewNodeView
typeViewNodeView view@(TypeViewNode ty) =
  case dropTypeViewContexts ty of
    TypeViewVar name ->
      TypeViewVarNode (typeViewNameDisplay name) (typeViewNamePayload name)
    TypeViewArrow dom cod ->
      TypeViewArrowNode (projectTypeViewNode view dom) (projectTypeViewNode view cod)
    TypeViewBase name ->
      TypeViewBaseNode (typeViewNameDisplay name) (typeViewNamePayload name)
    TypeViewCon name args ->
      TypeViewConNode
        (typeViewNameDisplay name)
        (typeViewNamePayload name)
        (fmap (projectTypeViewNode view) args)
    TypeViewVarApp name args ->
      TypeViewVarAppNode
        (typeViewNameDisplay name)
        (typeViewNamePayload name)
        (fmap (projectTypeViewNode view) args)
    TypeViewTyLam name body ->
      TypeViewTyLamNode
        (typeViewNameDisplay name)
        (typeViewNamePayload name)
        (projectTypeViewNode view body)
    TypeViewTyApp fun arg ->
      TypeViewTyAppNode
        (projectTypeViewNode view fun)
        (projectTypeViewNode view arg)
    TypeViewForall name mbBound body ->
      TypeViewForallNode
        (typeViewNameDisplay name)
        (typeViewNamePayload name)
        (fmap (projectTypeViewNode view) mbBound)
        (projectTypeViewNode view body)
    TypeViewMu name body ->
      TypeViewMuNode
        (typeViewNameDisplay name)
        (typeViewNamePayload name)
        (projectTypeViewNode view body)
    TypeViewContextHead _ body ->
      typeViewNodeView (projectTypeViewNode view body)
    TypeViewContextBinder _ body ->
      typeViewNodeView (projectTypeViewNode view body)
    TypeViewBottom ->
      TypeViewBottomNode

splitTypeViewHeadApplication :: Int -> TypeView -> Maybe (TypeView, [TypeView])
splitTypeViewHeadApplication suffixCount view@(TypeViewNode ty)
  | suffixCount < 0 = Nothing
  | otherwise =
      case dropTypeViewContexts ty of
        TypeViewCon name args -> splitHead (TypeViewBase name) (TypeViewCon name) args
        TypeViewVarApp name args -> splitHead (TypeViewVar name) (TypeViewVarApp name) args
        _ -> Nothing
  where
    splitHead bareHead appliedHead args
      | suffixCount > length allArgs = Nothing
      | otherwise =
          Just
            ( projectTypeViewNode view headNode,
              map (projectTypeViewNode view) suffixArgs
            )
      where
        allArgs = NE.toList args
        prefixCount = length allArgs - suffixCount
        (prefixArgs, suffixArgs) = splitAt prefixCount allArgs
        headNode =
          case prefixArgs of
            [] -> bareHead
            prefixHead : prefixTail -> appliedHead (prefixHead :| prefixTail)

typeViewFromSourceType :: Map String SymbolIdentity -> Map String TypeBinderIdentity -> SrcType -> Either TypeViewConstructionError TypeView
typeViewFromSourceType headIdentities binderIdentities sourceTy =
  TypeViewNode . addContextReferences <$> go sourceTy
  where
    headAliases =
      symbolIdentityAliasMapWith
        [ (headIdentity, [name])
        | (name, headIdentity) <- Map.toList headIdentities
        ]

    binderAliases =
      typeBinderIdentityAliasMap (Map.toList binderIdentities)

    headName displayName = do
      payload <- requireHeadIdentity displayName
      if "$identity#" `isPrefixOf` displayName
        && symbolIdentityStableName payload /= displayName
        && maybe True (not . sameSymbolIdentity payload) (Map.lookup displayName headIdentities)
        then Left (TypeViewMissingHeadIdentity displayName)
        else Right ()
      pure
          TypeViewName
            { typeViewNameDisplay = displayName,
              typeViewNamePayload = payload,
              typeViewNameReferences =
                Map.filterWithKey
                  (headReferenceBelongsTo displayName payload)
                  headIdentities,
              typeViewNameStructuralHeads = Map.empty
            }

    binderName displayName = do
      payload <- requireBinderIdentity displayName
      if "$typevar#" `isPrefixOf` displayName
        && typeBinderIdentityStableName payload /= displayName
        && Map.lookup displayName binderIdentities /= Just payload
        then Left (TypeViewMissingBinderIdentity displayName)
        else Right ()
      pure
          TypeViewName
            { typeViewNameDisplay = displayName,
              typeViewNamePayload = payload,
              typeViewNameReferences =
                Map.filterWithKey
                  (binderReferenceBelongsTo displayName payload)
                  binderIdentities,
              typeViewNameStructuralHeads = structuralHeadsForBinder payload
            }

    headCandidates name =
      mapMaybe
        id
        [ Map.lookup name headIdentities,
          Map.lookup name headAliases
        ]

    binderCandidates name =
      mapMaybe
        id
        [ Map.lookup name binderIdentities,
          Map.lookup name binderAliases
        ]

    requireHeadIdentity name =
      case Map.elems (Map.fromList [(symbolIdentityPayloadKey candidate, candidate) | candidate <- headCandidates name]) of
        [] -> Left (TypeViewMissingHeadIdentity name)
        [candidate] -> Right candidate
        candidates -> Left (TypeViewAmbiguousHeadIdentity name candidates)

    requireBinderIdentity name =
      case Set.toList (Set.fromList (binderCandidates name)) of
        [] -> Left (TypeViewMissingBinderIdentity name)
        [candidate] -> Right candidate
        candidates -> Left (TypeViewAmbiguousBinderIdentity name candidates)

    headReferenceBelongsTo displayName payload name candidate =
      name == displayName
        || name == symbolIdentityStableName payload
        || sameSymbolIdentity payload candidate
        || name `elem` symbolIdentityAliasNamesWith [displayName] payload

    binderReferenceBelongsTo displayName payload name candidate =
      name == displayName
        || name == typeBinderIdentityStableName payload
        || payload == candidate
        || name `elem` typeBinderIdentityAliasNames displayName payload

    structuralHeadsForBinder binderIdentity =
      case typeBinderIdentityStructural binderIdentity of
        Just (unique, _) ->
          Map.filter ((== unique) . symbolUniqueIdentity) headIdentities
        Nothing -> Map.empty

    go = \case
      STVar displayName ->
        TypeViewVar <$> binderName displayName
      STArrow displayDom displayCod ->
        TypeViewArrow <$> go displayDom <*> go displayCod
      STBase displayName ->
        TypeViewBase <$> headName displayName
      STCon displayName displayArgs ->
        TypeViewCon <$> headName displayName <*> traverse go displayArgs
      STVarApp displayName displayArgs ->
        TypeViewVarApp <$> binderName displayName <*> traverse go displayArgs
      STTyLam displayName displayBody ->
        TypeViewTyLam <$> binderName displayName <*> go displayBody
      STTyApp displayFun displayArg ->
        TypeViewTyApp <$> go displayFun <*> go displayArg
      STForall displayName displayBound displayBody ->
        TypeViewForall
          <$> binderName displayName
          <*> traverse (go . unSrcBound) displayBound
          <*> go displayBody
      STMu displayName displayBody ->
        TypeViewMu <$> binderName displayName <*> go displayBody
      STBottom ->
        Right TypeViewBottom

    addContextReferences =
      addTypeViewIdentityContexts headIdentities binderIdentities

typeViewWithIdentityAliases :: Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeView -> TypeView
typeViewWithIdentityAliases headIdentities binderIdentities (TypeViewNode ty) =
  TypeViewNode (annotateTypeViewType headIdentities binderIdentities ty)

typeViewWithBinderIdentityAliases :: Map String TypeBinderIdentity -> TypeView -> TypeView
typeViewWithBinderIdentityAliases binderIdentities view =
  typeViewWithIdentityAliases (typeViewHeadIdentities view) binderIdentities view

typeViewMergeHeadIdentityAliases :: Map String SymbolIdentity -> TypeView -> TypeView
typeViewMergeHeadIdentityAliases headIdentities view =
  typeViewWithIdentityAliases
    (mergeSymbolIdentityMaps [typeViewHeadIdentities view, headIdentities])
    (typeViewBinderIdentities view)
    view

typeViewMergeBinderIdentityAliases :: Map String TypeBinderIdentity -> TypeView -> TypeView
typeViewMergeBinderIdentityAliases binderIdentities view =
  typeViewWithBinderIdentityAliases
    (mergeTypeBinderIdentityMaps [typeViewBinderIdentities view, binderIdentities])
    view

typeViewBottom :: TypeView
typeViewBottom =
  TypeViewNode TypeViewBottom

requireTypeViewFromSourceType :: HasCallStack => Map String SymbolIdentity -> Map String TypeBinderIdentity -> SrcType -> TypeView
requireTypeViewFromSourceType headIdentities binderIdentities sourceTy =
  case typeViewFromSourceType headIdentities binderIdentities sourceTy of
    Right view -> view
    Left err ->
      error
        ( "identity-incomplete TypeView construction: "
            ++ show err
            ++ "; source type="
            ++ show sourceTy
            ++ "; head aliases="
            ++ show (Map.keys headIdentities)
            ++ "; binder aliases="
            ++ show (Map.keys binderIdentities)
        )

typeViewWithDisplay :: SrcType -> TypeView -> Either TypeViewShapeError TypeView
typeViewWithDisplay display view@(TypeViewNode ty) =
  TypeViewNode <$> go display ty
  where
    go displayTy typeNode =
      case typeNode of
        TypeViewContextHead name body ->
          TypeViewContextHead name <$> go displayTy body
        TypeViewContextBinder name body ->
          TypeViewContextBinder name <$> go displayTy body
        TypeViewVar name ->
          case displayTy of
            STVar displayName -> Right (TypeViewVar (rename displayName name))
            _ -> mismatch
        TypeViewArrow dom cod ->
          case displayTy of
            STArrow displayDom displayCod ->
              TypeViewArrow <$> go displayDom dom <*> go displayCod cod
            _ -> mismatch
        TypeViewBase name ->
          case displayTy of
            STBase displayName -> Right (TypeViewBase (rename displayName name))
            _ -> mismatch
        TypeViewCon name args ->
          case displayTy of
            STCon displayName displayArgs
              | NE.length displayArgs == NE.length args ->
                  TypeViewCon (rename displayName name)
                    <$> sequenceA (NE.zipWith go displayArgs args)
            _ -> mismatch
        TypeViewVarApp name args ->
          case displayTy of
            STVarApp displayName displayArgs
              | NE.length displayArgs == NE.length args ->
                  TypeViewVarApp (rename displayName name)
                    <$> sequenceA (NE.zipWith go displayArgs args)
            _ -> mismatch
        TypeViewTyLam name body ->
          case displayTy of
            STTyLam displayName displayBody ->
              TypeViewTyLam (rename displayName name) <$> go displayBody body
            _ -> mismatch
        TypeViewTyApp fun arg ->
          case displayTy of
            STTyApp displayFun displayArg ->
              TypeViewTyApp <$> go displayFun fun <*> go displayArg arg
            _ -> mismatch
        TypeViewForall name mbBound body ->
          case displayTy of
            STForall displayName displayBound displayBody ->
              TypeViewForall
                (rename displayName name)
                <$> goBound displayBound mbBound
                <*> go displayBody body
            _ -> mismatch
        TypeViewMu name body ->
          case displayTy of
            STMu displayName displayBody ->
              TypeViewMu (rename displayName name) <$> go displayBody body
            _ -> mismatch
        TypeViewBottom ->
          case displayTy of
            STBottom -> Right TypeViewBottom
            _ -> mismatch
      where
        mismatch = Left (TypeViewShapeMismatch display (typeViewIdentity view))

    goBound Nothing Nothing = Right Nothing
    goBound (Just (SrcBound displayBound)) (Just bound) = Just <$> go displayBound bound
    goBound _ _ = Left (TypeViewShapeMismatch display (typeViewIdentity view))

    rename displayName name =
      name {typeViewNameDisplay = displayName}

-- | Overlay a recovered display tree while preserving every identity-bearing
-- subtree whose shape still agrees. A caller-provided constructor owns only
-- the smallest subtrees whose semantic shape was deliberately recovered (for
-- example, a Church encoding recovered to its nominal data head).
typeViewOverlayDisplay :: (SrcType -> TypeView) -> SrcType -> TypeView -> TypeView
typeViewOverlayDisplay recover display (TypeViewNode ty) =
  TypeViewNode (go display ty)
  where
    go displayTy typeNode =
      case typeNode of
        TypeViewContextHead name body ->
          TypeViewContextHead name (go displayTy body)
        TypeViewContextBinder name body ->
          TypeViewContextBinder name (go displayTy body)
        TypeViewVar name ->
          case displayTy of
            STVar displayName -> TypeViewVar (rename displayName name)
            _ -> recovered displayTy
        TypeViewArrow dom cod ->
          case displayTy of
            STArrow displayDom displayCod ->
              TypeViewArrow (go displayDom dom) (go displayCod cod)
            _ -> recovered displayTy
        TypeViewBase name ->
          case displayTy of
            STBase displayName -> TypeViewBase (rename displayName name)
            _ -> recovered displayTy
        TypeViewCon name args ->
          case displayTy of
            STCon displayName displayArgs
              | NE.length displayArgs == NE.length args ->
                  TypeViewCon
                    (rename displayName name)
                    (NE.zipWith go displayArgs args)
            _ -> recovered displayTy
        TypeViewVarApp name args ->
          case displayTy of
            STVarApp displayName displayArgs
              | NE.length displayArgs == NE.length args ->
                  TypeViewVarApp
                    (rename displayName name)
                    (NE.zipWith go displayArgs args)
            _ -> recovered displayTy
        TypeViewTyLam name body ->
          case displayTy of
            STTyLam displayName displayBody ->
              TypeViewTyLam (rename displayName name) (go displayBody body)
            _ -> recovered displayTy
        TypeViewTyApp fun arg ->
          case displayTy of
            STTyApp displayFun displayArg ->
              TypeViewTyApp (go displayFun fun) (go displayArg arg)
            _ -> recovered displayTy
        TypeViewForall name mbBound body ->
          case displayTy of
            STForall displayName displayBound displayBody ->
              case overlayBound displayBound mbBound of
                Just recoveredBound ->
                  TypeViewForall
                    (rename displayName name)
                    recoveredBound
                    (go displayBody body)
                Nothing -> recovered displayTy
            _ -> recovered displayTy
        TypeViewMu name body ->
          case displayTy of
            STMu displayName displayBody ->
              TypeViewMu (rename displayName name) (go displayBody body)
            _ -> recovered displayTy
        TypeViewBottom ->
          case displayTy of
            STBottom -> TypeViewBottom
            _ -> recovered displayTy

    overlayBound Nothing Nothing = Just Nothing
    overlayBound (Just (SrcBound displayBound)) (Just bound) =
      Just (Just (go displayBound bound))
    overlayBound _ _ = Nothing

    recovered sourceTy =
      case recover sourceTy of
        TypeViewNode recoveredTy -> recoveredTy

    rename displayName name =
      name {typeViewNameDisplay = displayName}

mapTypeViewDisplayHeadNames :: (SymbolIdentity -> String -> String) -> TypeView -> TypeView
mapTypeViewDisplayHeadNames rename (TypeViewNode ty) =
  TypeViewNode (go ty)
  where
    renameHead name =
      name {typeViewNameDisplay = rename (typeViewNamePayload name) (typeViewNameDisplay name)}

    go typeNode =
      case typeNode of
        TypeViewVar {} -> typeNode
        TypeViewArrow dom cod -> TypeViewArrow (go dom) (go cod)
        TypeViewBase name -> TypeViewBase (renameHead name)
        TypeViewCon name args -> TypeViewCon (renameHead name) (fmap go args)
        TypeViewVarApp name args -> TypeViewVarApp name (fmap go args)
        TypeViewTyLam name body -> TypeViewTyLam name (go body)
        TypeViewTyApp fun arg -> TypeViewTyApp (go fun) (go arg)
        TypeViewForall name mbBound body -> TypeViewForall name (fmap go mbBound) (go body)
        TypeViewMu name body -> TypeViewMu name (go body)
        TypeViewContextHead name body -> TypeViewContextHead (renameHead name) (go body)
        TypeViewContextBinder name body -> TypeViewContextBinder name (go body)
        TypeViewBottom -> TypeViewBottom

mapTypeViewDisplayBinderNames :: (TypeBinderIdentity -> String -> String) -> TypeView -> TypeView
mapTypeViewDisplayBinderNames rename (TypeViewNode ty) =
  TypeViewNode (go ty)
  where
    renameBinder name =
      name {typeViewNameDisplay = rename (typeViewNamePayload name) (typeViewNameDisplay name)}

    go typeNode =
      case typeNode of
        TypeViewVar name -> TypeViewVar (renameBinder name)
        TypeViewArrow dom cod -> TypeViewArrow (go dom) (go cod)
        TypeViewBase {} -> typeNode
        TypeViewCon name args -> TypeViewCon name (fmap go args)
        TypeViewVarApp name args -> TypeViewVarApp (renameBinder name) (fmap go args)
        TypeViewTyLam name body -> TypeViewTyLam (renameBinder name) (go body)
        TypeViewTyApp fun arg -> TypeViewTyApp (go fun) (go arg)
        TypeViewForall name mbBound body -> TypeViewForall (renameBinder name) (fmap go mbBound) (go body)
        TypeViewMu name body -> TypeViewMu (renameBinder name) (go body)
        TypeViewContextHead name body -> TypeViewContextHead name (go body)
        TypeViewContextBinder name body -> TypeViewContextBinder (renameBinder name) (go body)
        TypeViewBottom -> TypeViewBottom

typeViewArrow :: TypeView -> TypeView -> TypeView
typeViewArrow (TypeViewNode domain) (TypeViewNode codomain) =
  TypeViewNode (TypeViewArrow domain codomain)

typeViewAddArgumentsInsideForalls :: [TypeView] -> TypeView -> TypeView
typeViewAddArgumentsInsideForalls arguments view@(TypeViewNode sourceTy) =
  typeViewWithIdentityAliases headIdentities binderIdentities rebuilt
  where
    (foralls, body) = splitTypeViewForalls sourceTy
    argumentNodes = [node | TypeViewNode node <- arguments]
    rebuiltBody = foldr TypeViewArrow body argumentNodes
    rebuilt = TypeViewNode (foldr (\(name, mbBound) acc -> TypeViewForall name mbBound acc) rebuiltBody foralls)
    headIdentities =
      mergeSymbolIdentityMaps (typeViewHeadIdentities view : map typeViewHeadIdentities arguments)
    binderIdentities =
      mergeTypeBinderIdentityMaps (typeViewBinderIdentities view : map typeViewBinderIdentities arguments)

typeViewQuantifyBinders :: [(String, TypeBinderIdentity)] -> TypeView -> TypeView
typeViewQuantifyBinders binders view =
  typeViewMergeBinderIdentityAliases binderIdentities (TypeViewNode quantifiedTy)
  where
    TypeViewNode sourceTy = view
    quantifiedTy =
      foldr
        (\(displayName, identity) body -> TypeViewForall (binderName displayName identity) Nothing body)
        sourceTy
        binders
    binderIdentities = typeBinderIdentityAliasMap binders
    binderName displayName identity =
      TypeViewName
        { typeViewNameDisplay = displayName,
          typeViewNamePayload = identity,
          typeViewNameReferences = Map.empty,
          typeViewNameStructuralHeads = Map.empty
        }

typeViewRebuildArrowBody :: TypeView -> [TypeView] -> TypeView -> TypeView
typeViewRebuildArrowBody template arguments result =
  typeViewWithIdentityAliases headIdentities binderIdentities rebuilt
  where
    TypeViewNode templateTy = template
    TypeViewNode resultTy = result
    (foralls, _) = splitTypeViewForalls templateTy
    argumentTys = [ty | TypeViewNode ty <- arguments]
    rebuiltBody = foldr TypeViewArrow resultTy argumentTys
    rebuilt = TypeViewNode (foldr (\(name, mbBound) body -> TypeViewForall name mbBound body) rebuiltBody foralls)
    allViews = template : result : arguments
    headIdentities = mergeSymbolIdentityMaps (map typeViewHeadIdentities allViews)
    binderIdentities = mergeTypeBinderIdentityMaps (map typeViewBinderIdentities allViews)

quantifyFreeTypeView :: TypeView -> TypeView
quantifyFreeTypeView view@(TypeViewNode sourceTy) =
  typeViewWithIdentityAliases
    (typeViewHeadIdentities view)
    (typeViewBinderIdentities view)
    (TypeViewNode (foldr (\name body -> TypeViewForall name Nothing body) sourceTy freeNames))
  where
    freeNames = Map.elems (freeTypeViewNames Set.empty sourceTy)

    freeTypeViewNames bound typeNode =
      case typeNode of
        TypeViewVar name -> singletonFreeName bound name
        TypeViewArrow dom cod -> freeTypeViewNames bound dom `Map.union` freeTypeViewNames bound cod
        TypeViewBase {} -> Map.empty
        TypeViewCon _ args -> foldMap (freeTypeViewNames bound) args
        TypeViewVarApp name args -> singletonFreeName bound name `Map.union` foldMap (freeTypeViewNames bound) args
        TypeViewTyLam name body -> freeTypeViewNames (Set.insert (binderKey name) bound) body
        TypeViewTyApp fun arg -> freeTypeViewNames bound fun `Map.union` freeTypeViewNames bound arg
        TypeViewForall name mbBound body ->
          foldMap (freeTypeViewNames bound) mbBound
            `Map.union` freeTypeViewNames (Set.insert (binderKey name) bound) body
        TypeViewMu name body -> freeTypeViewNames (Set.insert (binderKey name) bound) body
        TypeViewContextHead _ body -> freeTypeViewNames bound body
        TypeViewContextBinder _ body -> freeTypeViewNames bound body
        TypeViewBottom -> Map.empty

    singletonFreeName bound name
      | Set.member (binderKey name) bound = Map.empty
      | otherwise = Map.singleton (binderKey name) name

    binderKey =
      typeViewNamePayload

stripVacuousTypeViewForalls :: TypeView -> TypeView
stripVacuousTypeViewForalls view@(TypeViewNode sourceTy) =
  typeViewWithIdentityAliases
    (typeViewHeadIdentities view)
    (typeViewBinderIdentities view)
    (TypeViewNode (stripLeadingForalls (dropTypeViewContexts sourceTy)))
  where
    stripLeadingForalls typeNode =
      case typeNode of
        TypeViewForall name _ body
          | binderIsVacuous name body -> stripLeadingForalls (dropTypeViewContexts body)
        _ -> typeNode

    binderIsVacuous name body =
      typeViewNamePayload name
        `Set.notMember` freeTypeBinderIdentitiesTypeView (TypeViewNode body)

rebuildTypeViewAliases :: TypeViewType -> TypeView
rebuildTypeViewAliases typeNode =
  typeViewWithIdentityAliases
    (typeViewHeadIdentities view)
    (typeViewBinderIdentities view)
    view
  where
    view = TypeViewNode typeNode

stripTypeViewContexts :: TypeViewType -> TypeViewType
stripTypeViewContexts =
  \case
    TypeViewContextHead _ body -> stripTypeViewContexts body
    TypeViewContextBinder _ body -> stripTypeViewContexts body
    TypeViewArrow dom cod -> TypeViewArrow (stripTypeViewContexts dom) (stripTypeViewContexts cod)
    TypeViewCon name args -> TypeViewCon name (fmap stripTypeViewContexts args)
    TypeViewVarApp name args -> TypeViewVarApp name (fmap stripTypeViewContexts args)
    TypeViewTyLam name body -> TypeViewTyLam name (stripTypeViewContexts body)
    TypeViewTyApp fun arg -> TypeViewTyApp (stripTypeViewContexts fun) (stripTypeViewContexts arg)
    TypeViewForall name mbBound body ->
      TypeViewForall name (fmap stripTypeViewContexts mbBound) (stripTypeViewContexts body)
    TypeViewMu name body -> TypeViewMu name (stripTypeViewContexts body)
    typeNode -> typeNode

annotateTypeViewType :: Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeViewType -> TypeViewType
annotateTypeViewType headIdentities binderIdentities =
  addContextReferences . go . stripTypeViewContexts
  where
    headName oldName =
      let displayName = typeViewNameDisplay oldName
          payload = typeViewNamePayload oldName
          identityName = symbolIdentityStableName payload
       in TypeViewName
            { typeViewNameDisplay = displayName,
              typeViewNamePayload = payload,
              typeViewNameReferences =
                Map.filterWithKey
                  (headReferenceBelongsTo displayName identityName payload)
                  headIdentities,
              typeViewNameStructuralHeads = Map.empty
            }

    binderName oldName =
      let displayName = typeViewNameDisplay oldName
          payload = typeViewNamePayload oldName
          identityName = typeBinderIdentityStableName payload
       in TypeViewName
            { typeViewNameDisplay = displayName,
              typeViewNamePayload = payload,
              typeViewNameReferences =
                Map.filterWithKey
                  (binderReferenceBelongsTo displayName identityName payload)
                  binderIdentities,
              typeViewNameStructuralHeads = structuralHeadsForBinder payload
            }

    headReferenceBelongsTo displayName identityName payload name candidate =
      name == displayName
        || name == identityName
        || sameSymbolIdentity payload candidate
        || name `elem` symbolIdentityAliasNamesWith [displayName, identityName] payload

    binderReferenceBelongsTo displayName identityName payload name candidate =
      name == displayName
        || name == identityName
        || payload == candidate
        || name `elem` typeBinderIdentityAliasNames displayName payload
        || name `elem` typeBinderIdentityAliasNames identityName payload

    structuralHeadsForBinder binderIdentity =
      case typeBinderIdentityStructural binderIdentity of
        Just (unique, _) ->
          Map.filter ((== unique) . symbolUniqueIdentity) headIdentities
        Nothing -> Map.empty

    go typeNode =
      case typeNode of
        TypeViewVar name -> TypeViewVar (binderName name)
        TypeViewArrow dom cod -> TypeViewArrow (go dom) (go cod)
        TypeViewBase name -> TypeViewBase (headName name)
        TypeViewCon name args -> TypeViewCon (headName name) (fmap go args)
        TypeViewVarApp name args -> TypeViewVarApp (binderName name) (fmap go args)
        TypeViewTyLam name body -> TypeViewTyLam (binderName name) (go body)
        TypeViewTyApp fun arg -> TypeViewTyApp (go fun) (go arg)
        TypeViewForall name mbBound body -> TypeViewForall (binderName name) (fmap go mbBound) (go body)
        TypeViewMu name body -> TypeViewMu (binderName name) (go body)
        TypeViewContextHead _ body -> go body
        TypeViewContextBinder _ body -> go body
        TypeViewBottom -> TypeViewBottom

    addContextReferences =
      addTypeViewIdentityContexts headIdentities binderIdentities

-- | Retain aliases whose identities are not mentioned by the visible type.
-- Build each identity's context node from a single grouping pass: filtering the
-- complete alias map once per identity made large module finalization quadratic
-- in the number of identities carried by the scope.
addTypeViewIdentityContexts :: Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeViewType -> TypeViewType
addTypeViewIdentityContexts headIdentities binderIdentities ty =
  foldr addBinderContext (foldr addHeadContext ty (Map.elems remainingHeadGroups)) (Map.elems remainingBinderGroups)
  where
    projectedHeads = typeViewTypeHeadIdentities ty
    projectedBinders = typeViewTypeBinderIdentities ty

    remainingHeadReferences =
      Map.filterWithKey
        (\name candidate -> Map.lookup name projectedHeads /= Just candidate)
        headIdentities
    remainingBinderReferences =
      Map.filterWithKey
        (\name candidate -> Map.lookup name projectedBinders /= Just candidate)
        binderIdentities

    remainingHeadGroups =
      Map.foldlWithKey'
        (\groups name candidate ->
            Map.insertWith
              mergeHeadReferenceGroups
              (symbolIdentityPayloadKey candidate)
              (candidate, Map.singleton name candidate)
              groups
        )
        Map.empty
        remainingHeadReferences
    remainingBinderGroups =
      Map.foldlWithKey'
        (\groups name candidate ->
            Map.insertWith
              mergeBinderReferenceGroups
              candidate
              (candidate, Map.singleton name candidate)
              groups
        )
        Map.empty
        remainingBinderReferences

    mergeHeadReferenceGroups (newPayload, newReferences) (_, oldReferences) =
      (newPayload, newReferences `Map.union` oldReferences)

    mergeBinderReferenceGroups (newPayload, newReferences) (_, oldReferences) =
      (newPayload, newReferences `Map.union` oldReferences)

    addHeadContext (payloadIdentity, references) body =
      TypeViewContextHead
        TypeViewName
          { typeViewNameDisplay = symbolDefiningName payloadIdentity,
            typeViewNamePayload = payloadIdentity,
            typeViewNameReferences = references,
            typeViewNameStructuralHeads = Map.empty
          }
        body

    addBinderContext (payloadIdentity, references) body =
      TypeViewContextBinder
        TypeViewName
          { typeViewNameDisplay = typeBinderIdentityStableName payloadIdentity,
            typeViewNamePayload = payloadIdentity,
            typeViewNameReferences = references,
            typeViewNameStructuralHeads = Map.empty
          }
        body

typeViewTypeDisplay :: TypeViewType -> SrcType
typeViewTypeDisplay ty =
  case ty of
    TypeViewVar name -> STVar (typeViewNameDisplay name)
    TypeViewArrow dom cod -> STArrow (typeViewTypeDisplay dom) (typeViewTypeDisplay cod)
    TypeViewBase name -> STBase (typeViewNameDisplay name)
    TypeViewCon name args -> STCon (typeViewNameDisplay name) (fmap typeViewTypeDisplay args)
    TypeViewVarApp name args -> STVarApp (typeViewNameDisplay name) (fmap typeViewTypeDisplay args)
    TypeViewTyLam name body -> STTyLam (typeViewNameDisplay name) (typeViewTypeDisplay body)
    TypeViewTyApp fun arg -> STTyApp (typeViewTypeDisplay fun) (typeViewTypeDisplay arg)
    TypeViewForall name mbBound body ->
      STForall
        (typeViewNameDisplay name)
        (SrcBound . typeViewTypeDisplay <$> mbBound)
        (typeViewTypeDisplay body)
    TypeViewMu name body -> STMu (typeViewNameDisplay name) (typeViewTypeDisplay body)
    TypeViewContextHead _ body -> typeViewTypeDisplay body
    TypeViewContextBinder _ body -> typeViewTypeDisplay body
    TypeViewBottom -> STBottom

typeViewTypeIdentity :: TypeViewType -> SrcType
typeViewTypeIdentity ty =
  case ty of
    TypeViewVar name -> STVar (typeBinderIdentityStableName (typeViewNamePayload name))
    TypeViewArrow dom cod -> STArrow (typeViewTypeIdentity dom) (typeViewTypeIdentity cod)
    TypeViewBase name -> STBase (symbolIdentityStableName (typeViewNamePayload name))
    TypeViewCon name args -> STCon (symbolIdentityStableName (typeViewNamePayload name)) (fmap typeViewTypeIdentity args)
    TypeViewVarApp name args -> STVarApp (typeBinderIdentityStableName (typeViewNamePayload name)) (fmap typeViewTypeIdentity args)
    TypeViewTyLam name body -> STTyLam (typeBinderIdentityStableName (typeViewNamePayload name)) (typeViewTypeIdentity body)
    TypeViewTyApp fun arg -> STTyApp (typeViewTypeIdentity fun) (typeViewTypeIdentity arg)
    TypeViewForall name mbBound body ->
      STForall
        (typeBinderIdentityStableName (typeViewNamePayload name))
        (SrcBound . typeViewTypeIdentity <$> mbBound)
        (typeViewTypeIdentity body)
    TypeViewMu name body -> STMu (typeBinderIdentityStableName (typeViewNamePayload name)) (typeViewTypeIdentity body)
    TypeViewContextHead _ body -> typeViewTypeIdentity body
    TypeViewContextBinder _ body -> typeViewTypeIdentity body
    TypeViewBottom -> STBottom

typeViewTypeHeadIdentities :: TypeViewType -> Map String SymbolIdentity
typeViewTypeHeadIdentities ty =
  mergeSymbolIdentityMaps (collectIdentityMaps ty [])
  where
    collectIdentityMaps typeNode rest =
      case typeNode of
        TypeViewVar name -> structuralHeadIdentityMap name : rest
        TypeViewArrow dom cod -> collectIdentityMaps dom (collectIdentityMaps cod rest)
        TypeViewBase name -> headIdentityMap name : rest
        TypeViewCon name args -> headIdentityMap name : foldr collectIdentityMaps rest (NE.toList args)
        TypeViewVarApp name args -> structuralHeadIdentityMap name : foldr collectIdentityMaps rest (NE.toList args)
        TypeViewTyLam name body -> structuralHeadIdentityMap name : collectIdentityMaps body rest
        TypeViewTyApp fun arg -> collectIdentityMaps fun (collectIdentityMaps arg rest)
        TypeViewForall name mbBound body ->
          structuralHeadIdentityMap name
            : collectIdentityMaps body (maybe rest (\bound -> collectIdentityMaps bound rest) mbBound)
        TypeViewMu name body -> structuralHeadIdentityMap name : collectIdentityMaps body rest
        TypeViewContextHead name body -> headIdentityMap name : collectIdentityMaps body rest
        TypeViewContextBinder name body -> structuralHeadIdentityMap name : collectIdentityMaps body rest
        TypeViewBottom -> rest

    headIdentityMap name =
      Map.insert (typeViewNameDisplay name) (typeViewNamePayload name)
        . Map.insert (symbolIdentityStableName (typeViewNamePayload name)) (typeViewNamePayload name)
        $ typeViewNameReferences name

    structuralHeadIdentityMap name =
      typeViewNameStructuralHeads name

typeViewTypeBinderIdentities :: TypeViewType -> Map String TypeBinderIdentity
typeViewTypeBinderIdentities ty =
  mergeTypeBinderIdentityMaps (collectIdentityMaps ty [])
  where
    collectIdentityMaps typeNode rest =
      case typeNode of
        TypeViewVar name -> binderIdentityMap name : rest
        TypeViewArrow dom cod -> collectIdentityMaps dom (collectIdentityMaps cod rest)
        TypeViewBase {} -> rest
        TypeViewCon _ args -> foldr collectIdentityMaps rest (NE.toList args)
        TypeViewVarApp name args -> binderIdentityMap name : foldr collectIdentityMaps rest (NE.toList args)
        TypeViewTyLam name body -> binderIdentityMap name : collectIdentityMaps body rest
        TypeViewTyApp fun arg -> collectIdentityMaps fun (collectIdentityMaps arg rest)
        TypeViewForall name mbBound body ->
          binderIdentityMap name
            : collectIdentityMaps body (maybe rest (\bound -> collectIdentityMaps bound rest) mbBound)
        TypeViewMu name body -> binderIdentityMap name : collectIdentityMaps body rest
        TypeViewContextHead _ body -> collectIdentityMaps body rest
        TypeViewContextBinder name body -> binderIdentityMap name : collectIdentityMaps body rest
        TypeViewBottom -> rest

    binderIdentityMap name =
      Map.insert (typeViewNameDisplay name) (typeViewNamePayload name)
        . Map.insert (typeBinderIdentityStableName (typeViewNamePayload name)) (typeViewNamePayload name)
        $ typeViewNameReferences name

instance Eq TypeView where
  left == right =
    typeViewIdentityTypesMatch left right
      && typeViewHeadIdentitySet left == typeViewHeadIdentitySet right
      && typeViewBinderIdentitySet left == typeViewBinderIdentitySet right

typeViewIdentityTypesMatch :: TypeView -> TypeView -> Bool
typeViewIdentityTypesMatch (TypeViewNode leftType) (TypeViewNode rightType) =
  go leftType rightType
  where
    go left right =
      case (dropTypeViewContexts left, dropTypeViewContexts right) of
        (TypeViewVar leftName, TypeViewVar rightName) ->
          sameBinder leftName rightName
        (TypeViewArrow leftDom leftCod, TypeViewArrow rightDom rightCod) ->
          go leftDom rightDom && go leftCod rightCod
        (TypeViewBase leftName, TypeViewBase rightName) ->
          sameHead leftName rightName
        (TypeViewCon leftName leftArgs, TypeViewCon rightName rightArgs) ->
          sameHead leftName rightName
            && length (NE.toList leftArgs) == length (NE.toList rightArgs)
            && and (zipWith go (NE.toList leftArgs) (NE.toList rightArgs))
        (TypeViewVarApp leftName leftArgs, TypeViewVarApp rightName rightArgs) ->
          sameBinder leftName rightName
            && length (NE.toList leftArgs) == length (NE.toList rightArgs)
            && and (zipWith go (NE.toList leftArgs) (NE.toList rightArgs))
        (TypeViewTyLam leftName leftBody, TypeViewTyLam rightName rightBody) ->
          sameBinder leftName rightName && go leftBody rightBody
        (TypeViewTyApp leftFun leftArg, TypeViewTyApp rightFun rightArg) ->
          go leftFun rightFun && go leftArg rightArg
        (TypeViewForall leftName leftBound leftBody, TypeViewForall rightName rightBound rightBody) ->
          sameBinder leftName rightName
            && sameBounds leftBound rightBound
            && go leftBody rightBody
        (TypeViewMu leftName leftBody, TypeViewMu rightName rightBody) ->
          sameBinder leftName rightName && go leftBody rightBody
        (TypeViewBottom, TypeViewBottom) ->
          True
        _ ->
          False

    sameBounds Nothing Nothing = True
    sameBounds (Just leftBound) (Just rightBound) = go leftBound rightBound
    sameBounds _ _ = False

    sameBinder leftName rightName =
      typeViewNamePayload leftName == typeViewNamePayload rightName

    sameHead leftName rightName =
      sameSymbolIdentity
        (typeViewNamePayload leftName)
        (typeViewNamePayload rightName)

typeViewHeadIdentitySet :: TypeView -> Set SymbolIdentityPayloadKey
typeViewHeadIdentitySet =
  typeHeadIdentityPayloadSet . typeViewHeadIdentities

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
    <|> lookupSymbolIdentityAlias (typeViewHeadIdentities view) name

typeViewBinderIdentityForAlias :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewBinderIdentityForAlias view name =
  typeViewBinderIdentityFor view name
    <|> lookupTypeBinderIdentityAlias (typeViewBinderIdentities view) name

typeViewHeadIdentityLookupAliases :: TypeView -> Map String SymbolIdentity
typeViewHeadIdentityLookupAliases view =
  mergeSymbolIdentityMaps [identities, symbolIdentityAliasMap (Map.elems identities)]
  where
    identities = typeViewHeadIdentities view

typeViewMentionedHeadIdentities :: TypeView -> Set SymbolIdentity
typeViewMentionedHeadIdentities (TypeViewNode ty) =
  go ty
  where
    go typeNode =
      case typeNode of
        TypeViewVar {} -> Set.empty
        TypeViewArrow dom cod -> go dom <> go cod
        TypeViewBase name -> Set.singleton (typeViewNamePayload name)
        TypeViewCon name args -> Set.insert (typeViewNamePayload name) (foldMap go args)
        TypeViewVarApp _ args -> foldMap go args
        TypeViewTyLam _ body -> go body
        TypeViewTyApp fun arg -> go fun <> go arg
        TypeViewForall _ mbBound body -> foldMap go mbBound <> go body
        TypeViewMu _ body -> go body
        TypeViewContextHead _ body -> go body
        TypeViewContextBinder _ body -> go body
        TypeViewBottom -> Set.empty

type TypeViewSubst = Map TypeBinderIdentity TypeView

typeViewSubstKeyFor :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewSubstKeyFor view identityName =
  typeViewSubstIdentityFor view identityName

typeViewSubstIdentityFor :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewSubstIdentityFor =
  typeViewBinderIdentityForAlias

lookupTypeViewSubst :: TypeBinderIdentity -> TypeViewSubst -> Maybe TypeView
lookupTypeViewSubst =
  Map.lookup

insertTypeViewSubst :: TypeBinderIdentity -> TypeView -> TypeViewSubst -> TypeViewSubst
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
    sameSymbolIdentity (constraintClassSymbol left) (constraintClassSymbol right)
      && constraintTypeViews left == constraintTypeViews right

data ClassApplicationKey = ClassApplicationKey SymbolIdentity (NonEmpty TypeView)
  deriving (Eq, Show)

constraintClassApplicationKey :: ConstraintInfo -> ClassApplicationKey
constraintClassApplicationKey constraint =
  classApplicationKey
    (constraintClassSymbol constraint)
    (constraintTypeViews constraint)

classApplicationKey :: SymbolIdentity -> NonEmpty TypeView -> ClassApplicationKey
classApplicationKey =
  ClassApplicationKey

data EvidenceMethodKey = EvidenceMethodKey ClassApplicationKey SymbolIdentity
  deriving (Eq, Show)

evidenceMethodKey :: SymbolIdentity -> NonEmpty TypeView -> SymbolIdentity -> EvidenceMethodKey
evidenceMethodKey classIdentity views =
  EvidenceMethodKey (classApplicationKey classIdentity views)

typeViewGeneratedIdentities :: TypeView -> [UniqueIdentity]
typeViewGeneratedIdentities view =
  concatMap symbolGeneratedIdentities uniqueHeads
    ++ concatMap typeBinderGeneratedIdentities uniqueBinders
  where
    uniqueHeads =
      Map.elems
        ( Map.fromList
            [ (symbolIdentityPayloadKey identity, identity)
            | identity <- Map.elems (typeViewHeadIdentities view)
            ]
        )
    uniqueBinders =
      Set.toList (Set.fromList (Map.elems (typeViewBinderIdentities view)))

typeBinderSubstGeneratedIdentities :: TypeBinderSubst -> [UniqueIdentity]
typeBinderSubstGeneratedIdentities subst =
  concatMap typeBinderGeneratedIdentities (Map.keys (typeBinderSubstByIdentity subst))
    ++ concatMap typeViewGeneratedIdentities (Map.elems (typeBinderSubstByIdentity subst))

constraintInfoGeneratedIdentities :: ConstraintInfo -> [UniqueIdentity]
constraintInfoGeneratedIdentities constraint =
  symbolGeneratedIdentities (constraintClassSymbol constraint)
    ++ concatMap typeViewGeneratedIdentities (NE.toList (constraintTypeViews constraint))

typeViewFromResolved :: ResolvedSrcType -> TypeView
typeViewFromResolved =
  TypeViewNode . resolvedSrcTypeViewType

typeViewFromElabType :: X.Ty v -> TypeView
typeViewFromElabType =
  TypeViewNode . go
  where
    go :: X.Ty a -> TypeViewType
    go ty =
      case ty of
        X.TVarRef ref -> TypeViewVar (binderName ref)
        X.TArrow dom cod -> TypeViewArrow (go dom) (go cod)
        X.TBaseWithIdentity identity (BaseTy name) -> TypeViewBase (headName identity name)
        X.TConWithIdentity identity (BaseTy name) args ->
          TypeViewCon (headName identity name) (fmap go args)
        X.TVarAppRef ref args -> TypeViewVarApp (binderName ref) (fmap go args)
        X.TForallRef ref mbBound body ->
          TypeViewForall (binderName ref) (fmap go mbBound) (go body)
        X.TMuRef ref body -> TypeViewMu (binderName ref) (go body)
        X.TBottom -> TypeViewBottom

    binderName ref =
      let identity = X.typeBinderRefIdentity ref
          displayName = X.typeBinderRefName ref
       in TypeViewName
            { typeViewNameDisplay = displayName,
              typeViewNamePayload = identity,
              typeViewNameReferences =
                typeBinderAliasIdentityMap [(displayName, identity)],
              typeViewNameStructuralHeads = Map.empty
            }

    headName identity displayName =
      TypeViewName
        { typeViewNameDisplay = displayName,
          typeViewNamePayload = identity,
          typeViewNameReferences =
            symbolIdentityAliasMapWith [(identity, [displayName])],
          typeViewNameStructuralHeads = Map.empty
        }

-- | Project the identity-bearing source type directly from the node tree.
-- Context nodes retain identities for later projections but do not add syntax.
typeViewToResolved :: TypeView -> ResolvedSrcType
typeViewToResolved (TypeViewNode ty) =
  go ty
  where
    go typeNode =
      case typeNode of
        TypeViewVar name -> RSTVar (resolvedBinder name)
        TypeViewArrow dom cod -> RSTArrow (go dom) (go cod)
        TypeViewBase name -> RSTBase (resolvedHead name)
        TypeViewCon name args -> RSTCon (resolvedHead name) (fmap go args)
        TypeViewVarApp name args -> RSTVarApp (resolvedBinder name) (fmap go args)
        TypeViewTyLam name body -> RSTTyLam (resolvedBinder name) (go body)
        TypeViewTyApp fun arg -> RSTTyApp (go fun) (go arg)
        TypeViewForall name mbBound body ->
          RSTForall
            (resolvedBinder name)
            (ResolvedSrcBound . go <$> mbBound)
            (go body)
        TypeViewMu name body -> RSTMu (resolvedBinder name) (go body)
        TypeViewContextHead _ body -> go body
        TypeViewContextBinder _ body -> go body
        TypeViewBottom -> RSTBottom

    resolvedBinder name =
      resolvedTypeBinderRefFromIdentity
        (typeViewNamePayload name)
        (typeViewNameDisplay name)

    resolvedHead name =
      let identity = typeViewNamePayload name
          displayName = typeViewNameDisplay name
       in mkResolvedSymbol
            identity
            displayName
            displayName
            (SymbolLocal (symbolDefiningModule identity))

-- | Convert an identity-bearing source type to the elaborator's type tree.
-- No binder is looked up by spelling and no fresh identity is allocated.
-- Compiler-owned exact annotations and program finalization share this bridge
-- so their treatment of bounds cannot drift.
resolvedSourceTypeToElabType :: ResolvedSrcType -> Either String ElabType
resolvedSourceTypeToElabType = go
  where
    go ty =
      case ty of
        RSTVar ref ->
          Right (X.TVarRef (elabBinderRef ref))
        RSTArrow dom cod ->
          X.TArrow <$> go dom <*> go cod
        RSTBase symbol ->
          Right
            ( X.TBaseWithIdentity
                (resolvedSymbolIdentity symbol)
                (BaseTy (PrimitiveInventory.normalizeBuiltinTypeReference (resolvedHeadDisplay symbol)))
            )
        RSTCon symbol args ->
          X.TConWithIdentity
            (resolvedSymbolIdentity symbol)
            (BaseTy (PrimitiveInventory.normalizeBuiltinTypeReference (resolvedHeadDisplay symbol)))
            <$> traverse go args
        RSTVarApp ref args ->
          X.TVarAppRef (elabBinderRef ref) <$> traverse go args
        RSTForall ref mbBound body -> do
          elabBound <- maybe (Right Nothing) resolvedBoundToElabBound mbBound
          X.TForallRef (elabBinderRef ref) elabBound <$> go body
        RSTMu ref body ->
          X.TMuRef (elabBinderRef ref) <$> go body
        RSTTyLam {} ->
          Left "residual type lambda reached identity-bearing type conversion"
        RSTTyApp {} ->
          Left "residual type application reached identity-bearing type conversion"
        RSTBottom ->
          Right X.TBottom

    resolvedBoundToElabBound (ResolvedSrcBound boundTy) = do
      elabTy <- go boundTy
      case elabTy of
        X.TVarRef {} -> Right Nothing
        X.TBottom -> Right Nothing
        _ -> Just <$> X.elabToBound elabTy

    elabBinderRef ref =
      X.typeBinderRefFromIdentity
        (resolvedTypeBinderIdentity ref)
        (resolvedTypeBinderName ref)

    resolvedHeadDisplay =
      symbolDisplayName . resolvedSymbolSpelling

resolvedSrcTypeViewType :: ResolvedSrcTy n v -> TypeViewType
resolvedSrcTypeViewType ty =
  case ty of
    RSTVar ref -> TypeViewVar (resolvedBinderName ref)
    RSTArrow dom cod -> TypeViewArrow (resolvedSrcTypeViewType dom) (resolvedSrcTypeViewType cod)
    RSTBase symbol -> TypeViewBase (resolvedHeadName symbol)
    RSTCon symbol args -> TypeViewCon (resolvedHeadName symbol) (fmap resolvedSrcTypeViewType args)
    RSTVarApp ref args -> TypeViewVarApp (resolvedBinderName ref) (fmap resolvedSrcTypeViewType args)
    RSTTyLam ref body -> TypeViewTyLam (resolvedBinderName ref) (resolvedSrcTypeViewType body)
    RSTTyApp fun arg -> TypeViewTyApp (resolvedSrcTypeViewType fun) (resolvedSrcTypeViewType arg)
    RSTForall ref mbBound body ->
      TypeViewForall
        (resolvedBinderName ref)
        (resolvedSrcTypeViewType . unResolvedSrcBound <$> mbBound)
        (resolvedSrcTypeViewType body)
    RSTMu ref body -> TypeViewMu (resolvedBinderName ref) (resolvedSrcTypeViewType body)
    RSTBottom -> TypeViewBottom
  where
    resolvedBinderName ref =
      let identity = resolvedTypeBinderTypeIdentity ref
       in TypeViewName
            { typeViewNameDisplay = resolvedTypeBinderName ref,
              typeViewNamePayload = identity,
              typeViewNameReferences =
                Map.fromList
                  [ (alias, identity)
                  | alias <-
                      typeBinderIdentityAliasNames (resolvedTypeBinderName ref) identity
                        ++ typeBinderIdentityAliasNames (typeBinderIdentityStableName identity) identity
                  ],
              typeViewNameStructuralHeads = Map.empty
            }

    resolvedHeadName symbol =
      let identity = resolvedSymbolIdentity symbol
          spelling = resolvedSymbolSpelling symbol
          displayName = symbolDisplayName spelling
          identityName = symbolIdentityStableName identity
       in TypeViewName
            { typeViewNameDisplay = displayName,
              typeViewNamePayload = identity,
              typeViewNameReferences =
                Map.fromList
                  [ (alias, identity)
                  | alias <- symbolIdentityAliasNamesWith [symbolSourceName spelling, displayName, identityName] identity
                  ],
              typeViewNameStructuralHeads = Map.empty
            }

displayConstraint :: ConstraintInfo -> P.ClassConstraint
displayConstraint constraint =
  P.ClassConstraint
    { P.constraintClassName = constraintDisplayClass constraint,
      P.constraintTypes = typeViewsDisplay (constraintTypeViews constraint)
    }

constrainedVisibleTypeView :: [ConstraintInfo] -> TypeView -> TypeView
constrainedVisibleTypeView [] bodyView = bodyView
constrainedVisibleTypeView constraints bodyView =
  typeViewQuantifyBinders binderPairs viewWithConstraintIdentities
  where
    constraintViews = concatMap (NE.toList . constraintTypeViews) constraints
    allViews = bodyView : constraintViews
    viewWithConstraintIdentities =
      typeViewWithIdentityAliases
        (mergeSymbolIdentityMaps (map typeViewHeadIdentities allViews))
        (mergeTypeBinderIdentityMaps (map typeViewBinderIdentities allViews))
        bodyView
    freeBinderIdentities =
      sort . Set.toList . Set.unions $
        map freeTypeBinderIdentitiesTypeView allViews
    displayNamesByIdentity =
      mergeUniquePairMaps (map freeTypeBinderDisplayNamesTypeView allViews)
    binderPairs =
      [ ( Map.findWithDefault
            (typeBinderIdentityStableName identity)
            identity
            displayNamesByIdentity,
          identity
        )
      | identity <- freeBinderIdentities
      ]

applyTypeViewSubst :: TypeViewSubst -> TypeView -> TypeView
applyTypeViewSubst subst (TypeViewNode sourceTy) =
  rebuildTypeViewAliases
    (stripTypeViewContexts (substituteTypeViewType subst sourceTy))

substituteTypeViewType :: TypeViewSubst -> TypeViewType -> TypeViewType
substituteTypeViewType subst = go Set.empty
  where
    go bound ty =
      case ty of
        TypeViewVar name ->
          maybe ty id (replacementFor bound name)
        TypeViewArrow dom cod ->
          TypeViewArrow (go bound dom) (go bound cod)
        TypeViewBase {} ->
          ty
        TypeViewCon name args ->
          TypeViewCon name (fmap (go bound) args)
        TypeViewVarApp name args ->
          let args' = fmap (go bound) args
           in case replacementFor bound name >>= (`applyTypeViewHead` args') of
                Just replacement -> replacement
                Nothing -> TypeViewVarApp name args'
        TypeViewTyLam name body ->
          TypeViewTyLam name (go (bindName bound name) body)
        TypeViewTyApp fun arg ->
          TypeViewTyApp (go bound fun) (go bound arg)
        TypeViewForall name mbBound body ->
          TypeViewForall
            name
            (fmap (go bound) mbBound)
            (go (bindName bound name) body)
        TypeViewMu name body ->
          TypeViewMu name (go (bindName bound name) body)
        TypeViewContextHead name body ->
          TypeViewContextHead name (go bound body)
        TypeViewContextBinder name body ->
          TypeViewContextBinder name (go bound body)
        TypeViewBottom ->
          TypeViewBottom

    replacementFor bound name =
      let identity = typeViewNamePayload name
       in if Set.member identity bound
        then Nothing
        else do
          TypeViewNode replacement <- Map.lookup identity subst
          pure replacement

    bindName bound name =
      Set.insert (typeViewNamePayload name) bound

applyTypeViewHead :: TypeViewType -> NonEmpty TypeViewType -> Maybe TypeViewType
applyTypeViewHead headTy args =
  case headTy of
    TypeViewVar name ->
      Just (TypeViewVarApp name args)
    TypeViewBase name ->
      Just (TypeViewCon name args)
    TypeViewCon name existingArgs ->
      Just (TypeViewCon name (existingArgs <> args))
    TypeViewVarApp name existingArgs ->
      Just (TypeViewVarApp name (existingArgs <> args))
    TypeViewContextHead name body ->
      TypeViewContextHead name <$> applyTypeViewHead body args
    TypeViewContextBinder name body ->
      TypeViewContextBinder name <$> applyTypeViewHead body args
    _ ->
      Nothing

typeViewBinderIdentityAliasEntries :: TypeView -> [(String, TypeBinderIdentity)]
typeViewBinderIdentityAliasEntries view =
  Map.toList (mergeTypeBinderIdentityMaps [identities, typeBinderIdentityAliasMap (Map.toList identities)])
  where
    identities = typeViewBinderIdentities view

mergeSymbolIdentityMaps :: [Map String SymbolIdentity] -> Map String SymbolIdentity
mergeSymbolIdentityMaps maps =
  Map.fromList
    [ (name, identity)
    | (name, identities) <- Map.toList identitiesByName,
      [identity] <- [Map.elems identities]
    ]
  where
    identitiesByName =
      Map.fromListWith
        Map.union
        [ (name, Map.singleton (symbolIdentityPayloadKey identity) identity)
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

typeViewSubstKeyDisplayNames :: TypeView -> TypeBinderIdentity -> [String]
typeViewSubstKeyDisplayNames view identity =
  Set.toList (typeViewSubstDisplayNames view identity)

typeViewSubstDisplayNames :: TypeView -> TypeBinderIdentity -> Set String
typeViewSubstDisplayNames view identity =
  typeViewSubstBinderNames view identity

typeViewSubstBinderNames :: TypeView -> TypeBinderIdentity -> Set String
typeViewSubstBinderNames view identity =
  Map.keysSet (Map.filter (== identity) (typeViewBinderIdentities view))

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

applyConstraintInfoSubst :: TypeViewSubst -> ConstraintInfo -> ConstraintInfo
applyConstraintInfoSubst subst constraint =
  let views = fmap (applyTypeViewSubst subst) (constraintTypeViews constraint)
   in constraint
        { constraintTypeViews = views
        }

constraintTypeView :: ConstraintInfo -> TypeView
constraintTypeView =
  NE.head . constraintTypeViews

freeTypeBinderIdentitiesTypeView :: TypeView -> Set TypeBinderIdentity
freeTypeBinderIdentitiesTypeView (TypeViewNode ty) =
  go Set.empty ty
  where
    go bound typeNode =
      case typeNode of
        TypeViewVar name -> freeBinder bound name
        TypeViewArrow dom cod -> go bound dom `Set.union` go bound cod
        TypeViewBase {} -> Set.empty
        TypeViewCon _ args -> foldMap (go bound) args
        TypeViewVarApp name args -> freeBinder bound name `Set.union` foldMap (go bound) args
        TypeViewTyLam name body -> go (Set.insert (typeViewNamePayload name) bound) body
        TypeViewTyApp fun arg -> go bound fun `Set.union` go bound arg
        TypeViewForall name mbBound body ->
          foldMap (go bound) mbBound
            `Set.union` go (Set.insert (typeViewNamePayload name) bound) body
        TypeViewMu name body -> go (Set.insert (typeViewNamePayload name) bound) body
        TypeViewContextHead _ body -> go bound body
        TypeViewContextBinder _ body -> go bound body
        TypeViewBottom -> Set.empty

    freeBinder bound name
      | Set.member identity bound = Set.empty
      | otherwise = Set.singleton identity
      where
        identity = typeViewNamePayload name

freeTypeBinderIdentitiesTypeViews :: NonEmpty TypeView -> Set TypeBinderIdentity
freeTypeBinderIdentitiesTypeViews =
  foldMap freeTypeBinderIdentitiesTypeView

freeTypeBinderDisplayNamesTypeView :: TypeView -> Map TypeBinderIdentity String
freeTypeBinderDisplayNamesTypeView (TypeViewNode ty) =
  go Set.empty ty
  where
    go bound typeNode =
      case typeNode of
        TypeViewVar name -> freeBinder bound name
        TypeViewArrow dom cod -> mergeUniquePairMaps [go bound dom, go bound cod]
        TypeViewBase {} -> Map.empty
        TypeViewCon _ args -> mergeUniquePairMaps (map (go bound) (NE.toList args))
        TypeViewVarApp name args ->
          mergeUniquePairMaps (freeBinder bound name : map (go bound) (NE.toList args))
        TypeViewTyLam name body -> go (Set.insert (typeViewNamePayload name) bound) body
        TypeViewTyApp fun arg -> mergeUniquePairMaps [go bound fun, go bound arg]
        TypeViewForall name mbBound body ->
          mergeUniquePairMaps
            [ foldMap (go bound) mbBound,
              go (Set.insert (typeViewNamePayload name) bound) body
            ]
        TypeViewMu name body -> go (Set.insert (typeViewNamePayload name) bound) body
        TypeViewContextHead _ body -> go bound body
        TypeViewContextBinder _ body -> go bound body
        TypeViewBottom -> Map.empty

    freeBinder bound name
      | Set.member identity bound = Map.empty
      | otherwise = Map.singleton identity (typeViewNameDisplay name)
      where
        identity = typeViewNamePayload name

typeViewIsBareBinderIdentity :: TypeBinderIdentity -> TypeView -> Bool
typeViewIsBareBinderIdentity identity (TypeViewNode ty) =
  case dropTypeViewContexts ty of
    TypeViewVar name -> typeViewNamePayload name == identity
    _ -> False

typeViewMentionsFreeBinderIdentity :: TypeBinderIdentity -> TypeView -> Bool
typeViewMentionsFreeBinderIdentity identity view =
  Set.member identity (freeTypeBinderIdentitiesTypeView view)

typeViewsDisplay :: NonEmpty TypeView -> NonEmpty SrcType
typeViewsDisplay = fmap typeViewDisplay

typeViewsIdentity :: NonEmpty TypeView -> NonEmpty SrcType
typeViewsIdentity = fmap typeViewIdentity

typeViewSubstFromParamIdentities :: NonEmpty TypeBinderIdentity -> NonEmpty TypeView -> TypeViewSubst
typeViewSubstFromParamIdentities paramIdentities views =
  Map.fromList (zipWith entry (NE.toList paramIdentities) (NE.toList views))
  where
    entry identity view =
      (identity, view)

typeParamBinderIdentity :: P.TypeParam -> Maybe TypeBinderIdentity
typeParamBinderIdentity param =
  resolvedTypeBinderTypeIdentity <$> P.typeParamRef param

-- | A type parameter that has crossed the resolved-program boundary.  Unlike
-- source 'P.TypeParam', this representation cannot encode a missing binder
-- identity.
data CheckedTypeParam = CheckedTypeParam
  { checkedTypeParamRef :: ResolvedTypeBinderRef,
    checkedTypeParamKind :: P.SrcKind
  }
  deriving (Eq, Show)

checkedTypeParamName :: CheckedTypeParam -> String
checkedTypeParamName =
  resolvedTypeBinderName . checkedTypeParamRef

checkedTypeParamIdentity :: CheckedTypeParam -> TypeBinderIdentity
checkedTypeParamIdentity =
  resolvedTypeBinderTypeIdentity . checkedTypeParamRef

checkedTypeParamIsFirstOrder :: CheckedTypeParam -> Bool
checkedTypeParamIsFirstOrder param =
  checkedTypeParamKind param == P.KType

checkedTypeParamGeneratedIdentities :: CheckedTypeParam -> [UniqueIdentity]
checkedTypeParamGeneratedIdentities =
  resolvedTypeBinderGeneratedIdentities . checkedTypeParamRef

newtype TypeBinderSubst = TypeBinderSubst
  { typeBinderSubstByIdentity :: Map TypeBinderIdentity TypeView
  }
  deriving (Show)

instance Eq TypeBinderSubst where
  left == right =
    typeBinderSubstIdentityViews left == typeBinderSubstIdentityViews right

typeBinderSubstIdentityViews :: TypeBinderSubst -> Map TypeBinderIdentity TypeView
typeBinderSubstIdentityViews = typeBinderSubstByIdentity

typeBinderSubstViews :: TypeBinderSubst -> [TypeView]
typeBinderSubstViews =
  Map.elems . typeBinderSubstIdentityViews

emptyTypeBinderSubst :: TypeBinderSubst
emptyTypeBinderSubst =
  TypeBinderSubst
    { typeBinderSubstByIdentity = Map.empty
    }

typeBinderSubstFromTypeViewSubst :: TypeViewSubst -> TypeBinderSubst
typeBinderSubstFromTypeViewSubst = TypeBinderSubst

typeBinderSubstToTypeViewSubst :: TypeBinderSubst -> TypeViewSubst
typeBinderSubstToTypeViewSubst = typeBinderSubstByIdentity

lookupTypeBinderSubstViewByIdentity :: TypeBinderIdentity -> TypeBinderSubst -> Maybe TypeView
lookupTypeBinderSubstViewByIdentity identity subst =
  Map.lookup identity (typeBinderSubstByIdentity subst)

insertTypeBinderSubstView :: TypeBinderIdentity -> TypeView -> TypeBinderSubst -> TypeBinderSubst
insertTypeBinderSubstView identity view subst =
  subst
    { typeBinderSubstByIdentity =
        Map.insert identity view (typeBinderSubstByIdentity subst)
    }

data EvidenceMethod = EvidenceMethod
  { evidenceMethodSymbol :: SymbolIdentity,
    evidenceMethodResolvedVar :: ResolvedVar,
    evidenceMethodTypeView :: TypeView
  }
  deriving (Show)

instance Eq EvidenceMethod where
  left == right =
    sameSymbolIdentity (evidenceMethodSymbol left) (evidenceMethodSymbol right)
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
  deriving (Show)

instance Eq EvidenceInfo where
  left == right =
    sameSymbolIdentity (evidenceClassSymbol left) (evidenceClassSymbol right)
      && evidenceTypeViews left == evidenceTypeViews right
      && symbolIdentityMapMatches (evidenceMethodsByIdentity left) (evidenceMethodsByIdentity right)

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
    constructorShapeIndex :: Int,
    constructorShapeOwnerTypeParams :: [CheckedTypeParam]
  }
  deriving (Show)

instance Eq ConstructorShape where
  left == right =
    sameSymbolIdentity (constructorShapeSymbol left) (constructorShapeSymbol right)
      && constructorShapeTypeView left == constructorShapeTypeView right
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
    ctorOwningTypeIdentity :: SymbolIdentity,
    ctorIndex :: Int,
    ctorOwnerConstructors :: [ConstructorShape]
  }
  deriving (Show)

instance Eq ConstructorInfo where
  left == right =
    sameSymbolIdentity (ctorInfoSymbol left) (ctorInfoSymbol right)
      && ctorTypeView left == ctorTypeView right
      && sameSymbolIdentity (ctorOwningTypeIdentity left) (ctorOwningTypeIdentity right)
      && ctorIndex left == ctorIndex right
      && ctorOwnerConstructors left == ctorOwnerConstructors right

constructorShapeForallBinderInfo :: ConstructorShape -> [ConstructorForallBinder]
constructorShapeForallBinderInfo =
  constructorForallBinderInfoFromTypeView . constructorShapeTypeView

ctorForallBinderInfo :: ConstructorInfo -> [ConstructorForallBinder]
ctorForallBinderInfo =
  constructorForallBinderInfoFromTypeView . ctorTypeView

constructorForallBinderInfoFromTypeView :: TypeView -> [ConstructorForallBinder]
constructorForallBinderInfoFromTypeView =
  map
    ( \(name, identity, _) ->
        ConstructorForallBinder
          { constructorForallDisplayName = name,
            constructorForallIdentity = identity
          }
    )
    . typeViewForallBinderViews

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
    dataTypeParams :: [CheckedTypeParam],
    dataConstructors :: [ConstructorInfo]
  }
  deriving (Show)

instance Eq DataInfo where
  left == right =
    sameSymbolIdentity (dataInfoSymbol left) (dataInfoSymbol right)
      && dataTypeParams left == dataTypeParams right
      && dataConstructors left == dataConstructors right

data MethodInfo = MethodInfo
  { methodInfoSymbol :: SymbolIdentity,
    methodDisplayName :: P.MethodName,
    methodTypeViewRaw :: TypeView,
    methodConstraintInfos :: [ConstraintInfo],
    methodParamBinders :: NonEmpty (String, TypeBinderIdentity)
  }
  deriving (Show)

instance Eq MethodInfo where
  left == right =
    sameSymbolIdentity (methodInfoSymbol left) (methodInfoSymbol right)
      && methodTypeViewRaw left == methodTypeViewRaw right
      && methodConstraintInfos left == methodConstraintInfos right
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
    classTypeParams :: NonEmpty CheckedTypeParam,
    classSuperclassInfos :: [ConstraintInfo],
    classFunctionalDependencies :: [FunctionalDependencyInfo],
    classMethodsByIdentity :: Map SymbolIdentity MethodInfo
  }
  deriving (Show)

instance Eq ClassInfo where
  left == right =
    sameSymbolIdentity (classInfoSymbol left) (classInfoSymbol right)
      && classTypeParams left == classTypeParams right
      && classSuperclassInfos left == classSuperclassInfos right
      && classFunctionalDependencies left == classFunctionalDependencies right
      && symbolIdentityMapMatches (classMethodsByIdentity left) (classMethodsByIdentity right)

data ValueInfo
  = OrdinaryValue
      { valueInfoSymbol :: SymbolIdentity,
        valueRuntimeName :: String,
        valueTypeView :: TypeView,
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
        sameSymbolIdentity (valueInfoSymbol left) (valueInfoSymbol right)
          && valueTypeView left == valueTypeView right
          && valueConstraintInfos left == valueConstraintInfos right
      (ConstructorValue {}, ConstructorValue {}) ->
        sameSymbolIdentity (valueInfoSymbol left) (valueInfoSymbol right)
          && valueCtorInfo left == valueCtorInfo right
      (OverloadedMethod {}, OverloadedMethod {}) ->
        sameSymbolIdentity (valueInfoSymbol left) (valueInfoSymbol right)
          && valueMethodInfo left == valueMethodInfo right
      _ ->
        False

valueInfoRuntimeName :: ValueInfo -> String
valueInfoRuntimeName valueInfo =
  case valueInfo of
    OrdinaryValue {valueInfoSymbol = symbol} ->
      idDetailsRuntimeName (TopLevelId symbol)
    ConstructorValue {valueCtorInfo = ctor} ->
      idDetailsRuntimeName (ConstructorId (constructorRefFromInfo ctor))
    OverloadedMethod {valueInfoSymbol = symbol} ->
      idDetailsRuntimeName (MethodId symbol)

valueInfoRuntimeDetails :: ValueInfo -> IdDetails
valueInfoRuntimeDetails valueInfo =
  case valueInfo of
    OrdinaryValue {valueInfoSymbol = symbol} ->
      TopLevelId symbol
    ConstructorValue {valueCtorInfo = ctor} ->
      ConstructorId (constructorRefFromInfo ctor)
    OverloadedMethod {valueInfoSymbol = symbol} ->
      MethodId symbol

valueInfoRawRuntimeName :: ValueInfo -> Maybe String
valueInfoRawRuntimeName valueInfo =
  case valueInfo of
    OrdinaryValue {valueRuntimeName = runtimeName} ->
      Just runtimeName
    ConstructorValue {valueRuntimeName = runtimeName} ->
      Just runtimeName
    OverloadedMethod {} ->
      Nothing

valueInfoIdentityRuntimeAliases :: ValueInfo -> [String]
valueInfoIdentityRuntimeAliases valueInfo =
  let details = valueInfoRuntimeDetails valueInfo
   in idDetailsAliasNamesWith (idDetailsRuntimeName details) details

valueInfoRuntimeAliases :: ValueInfo -> [String]
valueInfoRuntimeAliases valueInfo =
  filter (not . null) $
    nub $
      valueInfoIdentityRuntimeAliases valueInfo
        ++ maybe [] pure (valueInfoRawRuntimeName valueInfo)

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
  typeViewMergeBinderIdentityAliases
    (mergeTypeBinderIdentityMaps (map constraintBinderIdentities constraints))
    view
  where
    constraintBinderIdentities =
      foldMap typeViewBinderIdentities . constraintTypeViews
ordinaryValueTypeView _ =
  typeViewBottom

data InstanceInfo = InstanceInfo
  { instanceClassSymbol :: SymbolIdentity,
    instanceOriginModuleIdentity :: SymbolIdentity,
    instanceConstraintInfos :: [ConstraintInfo],
    instanceHeadTypeViews :: NonEmpty TypeView,
    instanceMethodsByIdentity :: Map SymbolIdentity ValueInfo
  }
  deriving (Show)

instance Eq InstanceInfo where
  left == right =
    sameSymbolIdentity (instanceClassSymbol left) (instanceClassSymbol right)
      && sameSymbolIdentity (instanceOriginModuleIdentity left) (instanceOriginModuleIdentity right)
      && instanceConstraintInfos left == instanceConstraintInfos right
      && instanceHeadTypeViews left == instanceHeadTypeViews right
      && symbolIdentityMapMatches (instanceMethodsByIdentity left) (instanceMethodsByIdentity right)

instanceHeadTypes :: InstanceInfo -> NonEmpty SrcType
instanceHeadTypes =
  fmap typeViewDisplay . instanceHeadTypeViews

instanceHeadIdentityTypes :: InstanceInfo -> NonEmpty SrcType
instanceHeadIdentityTypes =
  fmap typeViewIdentity . instanceHeadTypeViews

constructorShapeGeneratedIdentities :: ConstructorShape -> [UniqueIdentity]
constructorShapeGeneratedIdentities shape =
  symbolGeneratedIdentities (constructorShapeSymbol shape)
    ++ typeViewGeneratedIdentities (constructorShapeTypeView shape)
    ++ concatMap checkedTypeParamGeneratedIdentities (constructorShapeOwnerTypeParams shape)

constructorInfoGeneratedIdentities :: ConstructorInfo -> [UniqueIdentity]
constructorInfoGeneratedIdentities ctorInfo =
  symbolGeneratedIdentities (ctorInfoSymbol ctorInfo)
    ++ symbolGeneratedIdentities (ctorOwningTypeIdentity ctorInfo)
    ++ typeViewGeneratedIdentities (ctorTypeView ctorInfo)
    ++ concatMap constructorShapeGeneratedIdentities (ctorOwnerConstructors ctorInfo)

dataInfoGeneratedIdentities :: DataInfo -> [UniqueIdentity]
dataInfoGeneratedIdentities info =
  symbolGeneratedIdentities (dataInfoSymbol info)
    ++ concatMap checkedTypeParamGeneratedIdentities (dataTypeParams info)
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
    ++ foldMap checkedTypeParamGeneratedIdentities (classTypeParams info)
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
    ++ resolvedVarGeneratedIdentities (evidenceMethodResolvedVar method)
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
          ++ concatMap (typeBinderGeneratedIdentities . snd) (deferredMethodInstBinders deferred)
          ++ maybe [] typeViewGeneratedIdentities (deferredMethodExpectedResult deferred)
          ++ maybe [] deferredMethodEvidenceGeneratedIdentities (deferredMethodEvidence deferred)
          ++ concatMap evidenceInfoGeneratedIdentities (deferredMethodLocalEvidence deferred)
      DeferredConstructor deferred ->
        constructorInfoGeneratedIdentities (deferredConstructorInfo deferred)
          ++ typeViewGeneratedIdentities (deferredConstructorSourceTypeView deferred)
          ++ typeViewGeneratedIdentities (deferredConstructorOccurrenceTypeView deferred)
          ++ concatMap (typeBinderGeneratedIdentities . snd) (deferredConstructorInstBinders deferred)
          ++ typeBinderSubstGeneratedIdentities (deferredConstructorInitialSubst deferred)
      DeferredCase deferred ->
        loweredBindingIdentityGeneratedIdentities (deferredCaseBindingIdentity deferred)
          ++ dataInfoGeneratedIdentities (deferredCaseDataInfo deferred)
          ++ typeViewGeneratedIdentities (deferredCaseScrutineeTypeView deferred)
          ++ typeViewGeneratedIdentities (deferredCaseResultTypeView deferred)

data LoweredBindingIdentity
  = LoweredTopLevelIdentity SymbolIdentity
  | LoweredConstructorIdentity ConstructorRef
  | LoweredMethodIdentity SymbolIdentity
  deriving (Show)

loweredIdentityDetails :: LoweredBindingIdentity -> IdDetails
loweredIdentityDetails identity =
  case identity of
    LoweredTopLevelIdentity symbol -> TopLevelId symbol
    LoweredConstructorIdentity ref -> ConstructorId ref
    LoweredMethodIdentity symbol -> MethodId symbol

loweredIdentityRuntimeName :: LoweredBindingIdentity -> String
loweredIdentityRuntimeName =
  idDetailsRuntimeName . loweredIdentityDetails

loweredBindingIdentityFromTopLevel :: SymbolIdentity -> LoweredBindingIdentity
loweredBindingIdentityFromTopLevel = LoweredTopLevelIdentity

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
loweredBindingIdentityFromConstructorInfo =
  LoweredConstructorIdentity . constructorRefFromInfo

loweredBindingIdentityFromValueInfo :: ValueInfo -> LoweredBindingIdentity
loweredBindingIdentityFromValueInfo valueInfo =
  case valueInfo of
    OrdinaryValue
      { valueInfoSymbol = symbol
      } ->
        LoweredTopLevelIdentity symbol
    ConstructorValue {valueCtorInfo = ctor} ->
      loweredBindingIdentityFromConstructorInfo ctor
    OverloadedMethod
      { valueInfoSymbol = symbol
      } ->
      LoweredMethodIdentity symbol

resolvedVarFromLoweredBinding :: LoweredBinding -> ElabType -> ResolvedVar
resolvedVarFromLoweredBinding lowered ty =
  ResolvedVar
    { resolvedVarType = ty,
      resolvedVarDetails = loweredIdentityDetails identity
    }
  where
    identity = loweredBindingIdentity lowered

resolvedVarFromValueInfo :: ValueInfo -> ElabType -> ResolvedVar
resolvedVarFromValueInfo valueInfo ty =
  ResolvedVar
    { resolvedVarType = ty,
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
    -- Arguments already present at the source method occurrence, before
    -- elaboration eta-expands a partial application.
    deferredMethodSuppliedArgCount :: Int,
    -- Method arguments introduced by that eta expansion. Together with the
    -- supplied count, this is the declaration's complete value arity.
    deferredMethodRemainingArgCount :: Int,
    -- The ordered forall binders from the exact placeholder scheme registered
    -- for this obligation. Finalization must not reconstruct this order from
    -- the unspecialized method declaration.
    deferredMethodInstBinders :: [(String, TypeBinderIdentity)],
    deferredMethodExpectedResult :: Maybe TypeView,
    deferredMethodEvidence :: Maybe DeferredMethodEvidence,
    deferredMethodLocalEvidence :: [EvidenceInfo]
  }
  deriving (Show)

instance Eq DeferredMethodCall where
  left == right =
    deferredMethodRef left == deferredMethodRef right
      && deferredMethodInfo left == deferredMethodInfo right
      && deferredMethodSuppliedArgCount left == deferredMethodSuppliedArgCount right
      && deferredMethodRemainingArgCount left == deferredMethodRemainingArgCount right
      && typeBinderIdentityEntryListMatches (deferredMethodInstBinders left) (deferredMethodInstBinders right)
      && deferredMethodExpectedResult left == deferredMethodExpectedResult right
      && deferredMethodEvidence left == deferredMethodEvidence right
      && deferredMethodLocalEvidence left == deferredMethodLocalEvidence right

deferredMethodTotalArgCount :: DeferredMethodCall -> Int
deferredMethodTotalArgCount deferred =
  deferredMethodSuppliedArgCount deferred
    + deferredMethodRemainingArgCount deferred

-- | A source partial application has already crossed its checked elaboration
-- boundary once every source-supplied argument is visible. Its remaining
-- method variables are then carried by the checked head instantiations. An
-- evidence placeholder has no source-supplied arguments, so it must instead
-- wait for every eta-introduced argument.
deferredMethodResolutionArgCount :: DeferredMethodCall -> Int
deferredMethodResolutionArgCount deferred
  | suppliedArgCount == 0 = deferredMethodTotalArgCount deferred
  | otherwise = suppliedArgCount
  where
    suppliedArgCount = deferredMethodSuppliedArgCount deferred

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
    deferredConstructorSourceTypeView :: TypeView,
    deferredConstructorOccurrenceTypeView :: TypeView,
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
      && deferredConstructorSourceTypeView left == deferredConstructorSourceTypeView right
      && deferredConstructorOccurrenceTypeView left == deferredConstructorOccurrenceTypeView right
      && typeBinderIdentityEntryListMatches (deferredConstructorInstBinders left) (deferredConstructorInstBinders right)
      && deferredConstructorInitialSubst left == deferredConstructorInitialSubst right
      && deferredConstructorBindingMode left == deferredConstructorBindingMode right

symbolIdentityMapMatches :: Eq a => Map SymbolIdentity a -> Map SymbolIdentity a -> Bool
symbolIdentityMapMatches left right =
  payloadMap left == payloadMap right
  where
    payloadMap =
      Map.fromList . map (\(identity, value) -> (symbolIdentityPayloadKey identity, value)) . Map.toList

typeHeadIdentityPayloadSet :: Map String SymbolIdentity -> Set SymbolIdentityPayloadKey
typeHeadIdentityPayloadSet =
  Set.fromList . map symbolIdentityPayloadKey . Map.elems

deferredConstructorPlaceholder :: DeferredConstructorCall -> String
deferredConstructorPlaceholder =
  deferredRefName . deferredConstructorRef

data DeferredCaseCall = DeferredCaseCall
  { deferredCaseBindingIdentity :: LoweredBindingIdentity,
    deferredCaseRef :: DeferredRef,
    deferredCaseDataInfo :: DataInfo,
    deferredCaseScrutineeTypeView :: TypeView,
    deferredCaseResultTypeView :: TypeView,
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
      && symbolIdentityMapMatches (exportedTypeConstructorsByIdentity left) (exportedTypeConstructorsByIdentity right)

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
    symbolIdentityMapMatches (exportedValuesByIdentity left) (exportedValuesByIdentity right)
      && symbolIdentityMapMatches (exportedTypesByIdentity left) (exportedTypesByIdentity right)
      && symbolIdentityMapMatches (exportedClassesByIdentity left) (exportedClassesByIdentity right)

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
    | (identity, info : rest) <- uniquePayloadIdentityGroups entries,
      all (== info) rest
    ]

uniquePayloadIdentityGroups :: [(SymbolIdentity, a)] -> [(SymbolIdentity, [a])]
uniquePayloadIdentityGroups entries =
  [ (identity, values)
  | (_, identityEntries) <- Map.toList entriesByUnique,
    [(identity, values)] <- [payloadGroups identityEntries]
  ]
  where
    entriesByUnique =
      Map.fromListWith
        (++)
        [(symbolUniqueIdentity identity, [(identity, value)]) | (identity, value) <- entries]

    payloadGroups identityEntries =
      [ (identity, map snd payloadEntries)
      | (_, payloadEntries@((identity, _) : _)) <-
          Map.toList
            ( Map.fromListWith
                (++)
                [ (symbolIdentityPayloadKey identity, [(identity, value)])
                | (identity, value) <- identityEntries
                ]
            )
      ]

uniqueDisplayByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity String
uniqueDisplayByIdentity identityFor values =
  uniqueDisplayEntriesByIdentity
    [ (identityFor info, displayName)
    | (displayName, info) <- Map.toList values
    ]

uniqueDisplayNamesByIdentity :: [(SymbolIdentity, String)] -> Map SymbolIdentity [String]
uniqueDisplayNamesByIdentity entries =
  Map.fromList (uniquePayloadIdentityGroups entries)

uniqueDisplayEntriesByIdentity :: [(SymbolIdentity, String)] -> Map SymbolIdentity String
uniqueDisplayEntriesByIdentity entries =
  Map.fromList
    [ (identity, displayName)
    | (identity, displayName : rest) <- uniquePayloadIdentityGroups entries,
      all (== displayName) rest
    ]

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
          Just displayName <- [lookupSymbolIdentityExact identity displays]
        ]

data LoweredBinding = LoweredBinding
  { loweredBindingIdentity :: LoweredBindingIdentity,
    loweredBindingSourceTypeView :: TypeView,
    loweredBindingExpectedTypeView :: TypeView,
    loweredBindingSurfaceExpr :: ResolvedSurfaceExpr,
    loweredBindingResolvedLocalIdentities :: [LoweredResolvedLocalIdentity],
    loweredBindingResolvedEvidenceIdentities :: [LoweredResolvedLocalIdentity],
    loweredBindingDeferredObligations :: DeferredObligations,
    loweredBindingExternalTypeViews :: Map String TypeView,
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

loweredBindingSourceType :: LoweredBinding -> SrcType
loweredBindingSourceType =
  typeViewIdentity . loweredBindingSourceTypeView

loweredBindingExpectedType :: LoweredBinding -> SrcType
loweredBindingExpectedType =
  typeViewDisplay . loweredBindingExpectedTypeView

loweredBindingConstructorRef :: LoweredBinding -> Maybe ConstructorRef
loweredBindingConstructorRef =
  idDetailsConstructorRef . loweredIdentityDetails . loweredBindingIdentity

data CheckedBinding = CheckedBinding
  { checkedBindingResolvedVar :: ResolvedVar,
    checkedBindingSourceTypeView :: TypeView,
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

-- | Complete generated-identity inventory for a finalized binding.  Identity
-- supply owners use this one bridge both to thread the next supply and to
-- validate that no post-lowering finalization identity can be reissued.
checkedBindingGeneratedIdentities :: CheckedBinding -> [UniqueIdentity]
checkedBindingGeneratedIdentities binding =
  idDetailsGeneratedIdentities (resolvedVarDetails resolved)
    ++ generatedIdentitiesInType (resolvedVarType resolved)
    ++ typeViewGeneratedIdentities (checkedBindingSourceTypeView binding)
    ++ generatedIdentitiesInType (checkedBindingType binding)
    ++ generatedIdentitiesInTerm (checkedBindingTerm binding)
    ++ concatMap (idDetailsGeneratedIdentities . DeferredId) (Map.keys (checkedBindingDeferredObligations binding))
    ++ concatMap deferredProgramObligationGeneratedIdentities (Map.elems (checkedBindingDeferredObligations binding))
  where
    resolved = checkedBindingResolvedVar binding

checkedBindingsIdentityGenerator :: IdentityGenerator -> [CheckedBinding] -> IdentityGenerator
checkedBindingsIdentityGenerator generator =
  flip advanceIdentityGeneratorPastMany generator
    . concatMap checkedBindingGeneratedIdentities

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
    sameSymbolIdentity (checkedModuleIdentity left) (checkedModuleIdentity right)
      && checkedModuleBindings left == checkedModuleBindings right
      && symbolIdentityMapMatches (checkedModuleData left) (checkedModuleData right)
      && symbolIdentityMapMatches (checkedModuleClasses left) (checkedModuleClasses right)
      && checkedModuleInstances left == checkedModuleInstances right
      && checkedModuleExports left == checkedModuleExports right

-- | Complete generated-identity inventory for a finalized module.  Cache and
-- backend supply owners share this projection so newly added checked-module
-- metadata cannot silently diverge between consumers.
checkedModuleGeneratedIdentities :: CheckedModule -> [UniqueIdentity]
checkedModuleGeneratedIdentities checkedModule =
  symbolGeneratedIdentities (checkedModuleIdentity checkedModule)
    ++ concatMap checkedBindingGeneratedIdentities (checkedModuleBindings checkedModule)
    ++ concatMap dataInfoGeneratedIdentities (Map.elems (checkedModuleData checkedModule))
    ++ concatMap classInfoGeneratedIdentities (Map.elems (checkedModuleClasses checkedModule))
    ++ concatMap instanceInfoGeneratedIdentities (checkedModuleInstances checkedModule)

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

dataInfoHeadIdentityLookupAliases :: DataInfo -> Map String SymbolIdentity
dataInfoHeadIdentityLookupAliases info =
  mergeSymbolIdentityMaps
    ( symbolIdentityAliasMap [dataInfoSymbol info]
        : map constructorInfoHeadIdentityLookupAliases (dataConstructors info)
    )

dataParams :: DataInfo -> [String]
dataParams =
  map checkedTypeParamName . dataTypeParams

dataParamBinderIdentities :: DataInfo -> [TypeBinderIdentity]
dataParamBinderIdentities =
  map checkedTypeParamIdentity . dataTypeParams

dataParamBinders :: DataInfo -> [(String, TypeBinderIdentity)]
dataParamBinders =
  map (\param -> (checkedTypeParamName param, checkedTypeParamIdentity param)) . dataTypeParams

constructorInfoSymbolIdentity :: DataInfo -> ConstructorInfo -> SymbolIdentity
constructorInfoSymbolIdentity _ = ctorInfoSymbol

constructorInfoIdentityName :: ConstructorInfo -> String
constructorInfoIdentityName =
  symbolDefiningName . ctorInfoSymbol

constructorInfoRuntimeName :: ConstructorInfo -> String
constructorInfoRuntimeName =
  idDetailsRuntimeName . ConstructorId . constructorRefFromInfo

ctorName :: ConstructorInfo -> P.ConstructorName
ctorName =
  constructorInfoIdentityName

ctorOwningType :: ConstructorInfo -> P.TypeName
ctorOwningType =
  symbolDefiningName . ctorOwningTypeIdentity

constructorInfoHeadIdentityLookupAliases :: ConstructorInfo -> Map String SymbolIdentity
constructorInfoHeadIdentityLookupAliases ctorInfo =
  mergeSymbolIdentityMaps
    ( ownerHeadIdentities
        : typeViewHeadIdentityLookupAliases (ctorTypeView ctorInfo)
        : map (typeViewHeadIdentityLookupAliases . constructorShapeTypeView) (constructorOwnerShapes ctorInfo)
    )
  where
    ownerHeadIdentities =
      symbolIdentityAliasMap [ctorOwningTypeIdentity ctorInfo]

constructorOwnerRuntimeTypeTrackable :: Map SymbolIdentity DataInfo -> ConstructorInfo -> Bool
constructorOwnerRuntimeTypeTrackable dataInfosByIdentity ctor =
  case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctor) dataInfosByIdentity of
    Just dataInfo -> dataConstructorsRuntimeTypeTrackable dataInfo
    Nothing -> all constructorShapeRuntimeTypeTrackable (constructorOwnerShapes ctor)

constructorOwnerHasVariableHeadApplication :: Map SymbolIdentity DataInfo -> ConstructorInfo -> Bool
constructorOwnerHasVariableHeadApplication dataInfosByIdentity ctor =
  case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctor) dataInfosByIdentity of
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
      constructorShapeRuntimeName = constructorInfoRuntimeName ctor,
      constructorShapeTypeView = ctorTypeView ctor,
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
          ctorRuntimeName = constructorShapeRuntimeNameFromIdentity shape,
          ctorTypeView = constructorShapeTypeView shape,
          ctorOwningTypeIdentity = ownerIdentity,
          ctorIndex = constructorShapeIndex shape,
          ctorOwnerConstructors = ownerShapes
        }

    constructorShapeRuntimeNameFromIdentity =
      idDetailsRuntimeName . ConstructorId . constructorRefFromSymbol . constructorShapeSymbol

inferredConstructorOwnerTypeParams :: ConstructorInfo -> [ConstructorShape] -> [CheckedTypeParam]
inferredConstructorOwnerTypeParams ctorInfo ownerShapes =
  [ CheckedTypeParam ref kind0
  | name <- inferredConstructorOwnerParamNames ctorInfo ownerShapes paramRefs
  , Just ref <- [Map.lookup name paramRefs]
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
  case typeViewHeadArgViews view of
    Just argViews
      | typeViewRootHeadIdentity view == Just (ctorOwningTypeIdentity ctorInfo) ->
          mapMaybe bareBinder argViews
    _ -> []
  where
    bareBinder (TypeViewNode ty) =
      case dropTypeViewContexts ty of
        TypeViewVar name ->
          Just (typeViewNameDisplay name, typeViewNamePayload name)
        _ -> Nothing

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
        [] -> Nothing
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
  fmap checkedTypeParamName . classTypeParams

classParamBinderIdentities :: ClassInfo -> NonEmpty TypeBinderIdentity
classParamBinderIdentities =
  fmap checkedTypeParamIdentity . classTypeParams

classParamBinders :: ClassInfo -> NonEmpty (String, TypeBinderIdentity)
classParamBinders =
  fmap (\param -> (checkedTypeParamName param, checkedTypeParamIdentity param)) . classTypeParams

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
      lookupTypeViewSubst identity subst

methodTypeView :: MethodInfo -> TypeView
methodTypeView methodInfo =
  typeViewMergeBinderIdentityAliases
    (methodParamStableBinderIdentities methodInfo)
    (methodTypeViewRaw methodInfo)

methodParamStableBinderIdentities :: MethodInfo -> Map String TypeBinderIdentity
methodParamStableBinderIdentities methodInfo =
  Map.fromList
    [ (typeBinderIdentityStableName identity, identity)
    | identity <- NE.toList (methodParamBinderIdentities methodInfo)
    ]

typeViewArrowArgViews :: TypeView -> [TypeView]
typeViewArrowArgViews view@(TypeViewNode sourceTy) =
  map (projectTypeViewNode view) args
  where
    (_, bodyTy) = splitTypeViewForalls sourceTy
    (args, _) = splitTypeViewArrows bodyTy

typeViewForallBinderViews :: TypeView -> [(String, TypeBinderIdentity, Maybe TypeView)]
typeViewForallBinderViews view@(TypeViewNode sourceTy) =
  [ ( typeViewNameDisplay name,
      typeViewNamePayload name,
      fmap (projectTypeViewNode view) mbBound
    )
  | (name, mbBound) <- foralls
  ]
  where
    (foralls, _) = splitTypeViewForalls sourceTy

typeViewHeadArgViews :: TypeView -> Maybe [TypeView]
typeViewHeadArgViews view@(TypeViewNode sourceTy) =
  case dropTypeViewContexts sourceTy of
    TypeViewBase {} ->
      Just []
    TypeViewCon _ args ->
      Just (map (projectTypeViewNode view) (NE.toList args))
    _ ->
      Nothing

typeViewDirectArrowDomainView :: TypeView -> Maybe TypeView
typeViewDirectArrowDomainView view@(TypeViewNode sourceTy) =
  case dropTypeViewContexts sourceTy of
    TypeViewArrow domain _ ->
      Just (projectTypeViewNode view domain)
    _ ->
      Nothing

typeViewDirectArrowCodomainView :: TypeView -> Maybe TypeView
typeViewDirectArrowCodomainView view@(TypeViewNode sourceTy) =
  case dropTypeViewContexts sourceTy of
    TypeViewArrow _ codomain ->
      Just (projectTypeViewNode view codomain)
    _ ->
      Nothing

typeViewArrowResultView :: TypeView -> TypeView
typeViewArrowResultView view@(TypeViewNode sourceTy) =
  projectTypeViewNode view result
  where
    (_, body) = splitTypeViewForalls sourceTy
    (_, result) = splitTypeViewArrows body

typeViewArrowResultViewForArity :: TypeView -> Int -> TypeView
typeViewArrowResultViewForArity view@(TypeViewNode sourceTy) argCount =
  projectTypeViewNode view (foldr TypeViewArrow result (drop argCount args))
  where
    (_, body) = splitTypeViewForalls sourceTy
    (args, result) = splitTypeViewArrows body

methodParamTypeViews :: TypeView -> [TypeView]
methodParamTypeViews =
  typeViewArrowArgViews

methodResultTypeView :: MethodInfo -> TypeView
methodResultTypeView methodInfo =
  methodResultTypeViewFrom (methodTypeView methodInfo)

methodResultTypeViewFrom :: TypeView -> TypeView
methodResultTypeViewFrom =
  typeViewArrowResultView

projectTypeViewNode :: TypeView -> TypeViewType -> TypeView
projectTypeViewNode (TypeViewNode sourceTy) typeNode =
  TypeViewNode
    (foldr TypeViewContextBinder typeNode leadingBinders)
  where
    leadingBinders = map fst (fst (splitTypeViewForalls sourceTy))

dropTypeViewContexts :: TypeViewType -> TypeViewType
dropTypeViewContexts typeNode =
  case typeNode of
    TypeViewContextHead _ body -> dropTypeViewContexts body
    TypeViewContextBinder _ body -> dropTypeViewContexts body
    _ -> typeNode

splitTypeViewForalls :: TypeViewType -> ([(TypeViewName TypeBinderIdentity, Maybe TypeViewType)], TypeViewType)
splitTypeViewForalls typeNode =
  case dropTypeViewContexts typeNode of
    TypeViewForall name mbBound body ->
      let (rest, result) = splitTypeViewForalls body
       in ((name, mbBound) : rest, result)
    body -> ([], body)

splitTypeViewArrows :: TypeViewType -> ([TypeViewType], TypeViewType)
splitTypeViewArrows typeNode =
  case dropTypeViewContexts typeNode of
    TypeViewArrow domain codomain ->
      let (rest, result) = splitTypeViewArrows codomain
       in (domain : rest, result)
    result -> ([], result)

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
  lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) (classMethodsByIdentity classInfo)

lookupInstanceMethod :: MethodInfo -> InstanceInfo -> Maybe ValueInfo
lookupInstanceMethod methodInfo instanceInfo =
  lookupSymbolIdentityExact (methodInfoSymbolIdentity methodInfo) (instanceMethodsByIdentity instanceInfo)

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
  typeViewMergeBinderIdentityAliases
    (mergeTypeBinderIdentityMaps (map typeViewBinderIdentities (NE.toList classArgViews)))
    specialized
  where
    view = methodTypeView methodInfo
    subst = typeViewSubstFromParamIdentities (methodParamBinderIdentities methodInfo) classArgViews
    specialized = specializeQuantifiedTypeView subst view

specializeQuantifiedTypeView :: TypeViewSubst -> TypeView -> TypeView
specializeQuantifiedTypeView subst (TypeViewNode sourceTy) =
  rebuildTypeViewAliases
    (stripTypeViewContexts (specializeTypeViewType subst sourceTy))

specializeTypeViewType :: TypeViewSubst -> TypeViewType -> TypeViewType
specializeTypeViewType subst = go
  where
    go ty =
      case ty of
        TypeViewContextHead name body ->
          TypeViewContextHead name (go body)
        TypeViewContextBinder name body ->
          TypeViewContextBinder name (go body)
        TypeViewForall name mbBound body
          | hasReplacement name ->
              go (substituteTypeViewType subst body)
          | otherwise ->
              TypeViewForall
                name
                (fmap (substituteTypeViewType subst) mbBound)
                (go body)
        _ ->
          substituteTypeViewType subst ty

    hasReplacement name =
      Map.member (typeViewNamePayload name) subst
