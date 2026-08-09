{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Check.Internal
  ( ProgramError (..),
    ProgramDiagnostic (..),
    CheckedProgram,
    CheckedModule (..),
    CheckedBinding (..),
    DataInfo (..),
    ConstructorShape (..),
    ConstructorInfo (..),
    ClassInfo (..),
    MethodInfo (..),
    InstanceInfo (..),
    ValueInfo (..),
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
    ResolvedVar (..),
    ExportedTypeInfo (..),
    ModuleExports (..),
    checkProgram,
    checkProgramPackage,
    checkResolvedProgram,
    checkLocatedProgram,
    checkLocatedProgramPackage,
    checkLocatedProgramPackageWithTiming,
    newBuiltinPreludeCheckCacheForTest,
    checkLocatedProgramPackageWithBuiltinPreludeCheckCacheForTest,
    checkLocatedProgramPackageWithTimingAndBuiltinPreludeCheckCacheForTest,
    nextClientIdentityAfterCachedBuiltinPreludeForTest,
    splitContiguousEligibleBatch,
  )
where

import Control.Exception (evaluate)
import Control.Monad (foldM, forM, when, zipWithM)
import Control.Monad.Except (MonadError (throwError))
import Data.Char (isAlphaNum)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, intercalate, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified MLF.Frontend.Program.Builtins as Builtins
import qualified MLF.Frontend.Program.Check.Cache as PreludeCache
import MLF.Frontend.Program.Check.IdentitySupply
  ( builtinPreludeCheckIdentityGenerator,
  )
import MLF.Frontend.Program.Checked
  ( CheckedProgram,
    mkCheckedProgram,
  )
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    lowerConstructorBinding,
    lowerConstrainedResolvedExprBindingWithGenerator,
    lowerResolvedConstrainedExprBindingWithGenerator,
    mkElaborateScope,
    resolveInstanceInfoWithIdentityType,
  )
import MLF.Frontend.Program.Finalize
  ( FinalizeContext,
    ModuleFinalizeContext,
    finalizeBindingsAllowOpaqueWithContextFromSupply,
    finalizeBindingsAllowOpaqueWithContextWithTimingFromSupply,
    finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply,
    finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply,
    finalizeBindingAllowOpaqueWithContextWithTimingFromSupply,
    finalizeBindingAllowOpaqueWithContextFromSupply,
    finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply,
    mkFinalizeContext,
    mkFinalizeContextWithTiming,
    mkModuleFinalizeContext,
  )
import MLF.Frontend.Program.Interface
  ( ModuleInterface,
    ProgramInterfaceError,
    moduleInterfaceDataByIdentity,
    moduleInterfaceExports,
    moduleInterfaceIdentity,
    moduleInterfaceFromCheckedModule,
    moduleInterfaceInstances,
    packageInterfaceFromCheckedProgram,
    renderProgramInterfaceError,
  )
import MLF.Frontend.Program.Package
  ( LocatedProgramPackage,
    PackageModuleGraph (..),
    PackageModuleGraphNode (..),
    PackageModuleId (..),
    ProgramPackage,
    locatedProgramPackageModuleGraph,
    locatedProgramPackageOrderedProgram,
    locatedProgramPackageProgram,
    packageModuleGraphNodeIsBuiltinPrelude,
    programPackageModuleGraph,
    programPackageOrderedProgram,
    trivialPackageId,
    trivialLocatedProgramPackage,
    trivialProgramPackage,
  )
import MLF.Frontend.Program.Resolve (resolveProgram)
import MLF.Frontend.Symbol (SymbolIdentityPayloadKey, lookupSymbolIdentityExact, sameSymbolIdentity, symbolIdentityAliasMap, symbolIdentityPayloadKey, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.Program.TypeFamilies (normalizeTypeFamiliesInProgram)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    ClassApplicationKey,
    ClassInfo (..),
    ConstructorShape (..),
    ConstructorInfo (..),
    ConstructorRef,
    DataInfo (..),
    DeferredRef,
    ExportedTypeInfo (..),
    FunctionalDependencyInfo (..),
    IdDetails (..),
    InstanceInfo (..),
    LocalRef,
    LoweredBinding (..),
    loweredBindingSourceType,
    LoweredBindingIdentity,
    MethodInfo (..),
    ModuleExports (..),
    PrimitiveRef,
    ProgramDiagnostic (..),
    ProgramError (..),
    ResolvedLocalSymbols (..),
    ResolvedProgram (..),
    ResolvedSemanticModule (..),
    ResolvedSemanticProgramArtifact (..),
    ResolvedSymbol,
    ResolvedVar (..),
    SymbolOrigin (..),
    SymbolIdentity,
    SymbolNamespace (..),
    SymbolOwnerIdentity (..),
    ConstraintInfo (..),
    CheckedTypeParam (..),
    TypeView,
    TypeViewNodeView (..),
    typeViewBinderIdentities,
    typeViewDisplay,
    typeViewNodeView,
    ValueInfo (..),
    applyConstraintInfoSubst,
    checkedBindingName,
    checkedBindingsIdentityGenerator,
    constructorRefFromSymbol,
    constructorRefSymbol,
    constraintTypeView,
    classInfoIdentityModule,
    classApplicationKey,
    className,
    classParamBinderIdentities,
    classInfoSymbolIdentity,
    constrainedVisibleTypeView,
    ctorName,
    ctorType,
    constructorInfoSymbolIdentity,
    constructorOwnerDataInfoFromShapes,
    constructorOwnerShapes,
    constructorShapeFromInfo,
    constructorShapeType,
    dataParamBinders,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataInfoSymbolIdentity,
    diagnosticForProgramError,
    instanceClassName,
    instanceInfoClassSymbolIdentity,
    instanceHeadTypes,
    freeTypeBinderIdentitiesTypeView,
    loweredBindingIdentityFromValueInfo,
    loweredIdentityDetails,
    loweredIdentityRuntimeName,
    localRefDiscard,
    localRefFromIdentity,
    localRefIdentity,
    localRefName,
    deferredRefFromIdentity,
    deferredRefIdentity,
    deferredRefName,
    lookupClassMethod,
    lookupInstanceMethod,
    methodName,
    methodParamBinders,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    mkExportedTypeInfo,
    moduleExportsFromMaps,
    uniqueDisplayByIdentity,
    uniqueDisplayNamesByIdentity,
    uniqueInfoEntriesByIdentity,
    uniqueInfoListByIdentity,
    uniqueInfoByIdentity,
    exportedClassesForDisplay,
    exportedTypesForDisplay,
    exportedTypeConstructorsForDisplay,
    exportedValuesForDisplay,
    mkResolvedSymbol,
    resolvedSymbolIdentity,
    resolvedProgramSemanticArtifact,
    resolvedProgramGeneratedIdentities,
    symbolDefiningModule,
    symbolDefiningName,
    symbolIdentityFromParts,
    symbolNamespace,
    symbolOwnerIdentity,
    specializeMethodTypeView,
    typeBinderAliasIdentityMap,
    typeParamBinderIdentity,
    checkedTypeParamIdentity,
    checkedTypeParamName,
    typeViewFromResolved,
    mapTypeViewDisplayHeadNames,
    typeViewMergeBinderIdentityAliases,
    typeViewMergeHeadIdentityAliases,
    typeViewMentionedHeadIdentities,
    typeViewsDisplay,
    typeViewsIdentity,
    typeViewSubstFromParamIdentities,
    typeViewWithBinderIdentityAliases,
    typeViewWithDisplay,
    constructorInfoRuntimeName,
    ordinaryValueTypeView,
    primitiveRefFromSymbol,
    primitiveRefSymbol,
    valueInfoRuntimeName,
    valueInfoSymbolIdentity,
  )
import MLF.Util.Timing
  ( TimingConfig,
    defaultTimingConfig,
    timingProgramDefDetails,
    timeProgramDetailIO,
    timeProgramIO,
    timeProgramOperationIO,
  )
import MLF.Frontend.Syntax
  ( Lit (..),
    ResolvedSrcBound (..),
    ResolvedSrcTy (..),
    ResolvedSrcType,
    ResolvedTypeBinderRef,
    SrcBound (..),
    SrcTy (..),
    SrcType,
    resolvedSrcTypeBinderName,
    resolvedSrcTypeToSrcType,
    resolvedTypeBinderRefFromIdentity,
    resolvedTypeBinderTypeIdentity,
  )
import qualified MLF.Frontend.Syntax.Program as P
import System.IO.Unsafe (unsafePerformIO)
import MLF.Frontend.TypeLevel (TypeFamilyDecl, familyDeclName)
import MLF.Types.Identity
  ( IdentityGenerator,
    TypeBinderIdentity,
    UniqueIdentity,
    freshIdentity,
    freshLocalRef,
    identityGeneratorAfter,
    typeBinderIdentityFromUnique,
  )

type TcM a = Either ProgramError a

data OverlapSide
  = OverlapLeft
  | OverlapRight
  deriving (Eq, Ord, Show)

data OverlapMeta = OverlapMeta OverlapSide TypeBinderIdentity
  deriving (Eq, Ord, Show)

data OverlapType
  = OverlapMetaVar OverlapMeta
  | OverlapRigidVar Int
  | OverlapArrow OverlapType OverlapType
  | OverlapHead SymbolIdentityPayloadKey
  | OverlapCon SymbolIdentityPayloadKey (NonEmpty OverlapType)
  | OverlapMetaApp OverlapMeta (NonEmpty OverlapType)
  | OverlapRigidApp Int (NonEmpty OverlapType)
  | OverlapTyLam OverlapType
  | OverlapTyApp OverlapType OverlapType
  | OverlapForall (Maybe OverlapType) OverlapType
  | OverlapMu OverlapType
  | OverlapBottom
  deriving (Eq, Ord, Show)

type OverlapSubstitution = Map OverlapMeta OverlapType

overlapTypeView :: OverlapSide -> TypeView -> OverlapType
overlapTypeView side =
  go Map.empty
  where
    go bound view =
      case typeViewNodeView view of
        TypeViewVarNode _ identity ->
          either OverlapMetaVar OverlapRigidVar (overlapBinderKey bound identity)
        TypeViewArrowNode dom cod ->
          OverlapArrow (go bound dom) (go bound cod)
        TypeViewBaseNode _ identity ->
          OverlapHead (symbolIdentityPayloadKey identity)
        TypeViewConNode _ identity args ->
          OverlapCon (symbolIdentityPayloadKey identity) (fmap (go bound) args)
        TypeViewVarAppNode _ identity args ->
          case overlapBinderKey bound identity of
            Left meta -> OverlapMetaApp meta (fmap (go bound) args)
            Right index0 -> OverlapRigidApp index0 (fmap (go bound) args)
        TypeViewTyLamNode _ identity body ->
          OverlapTyLam (go (pushBound identity bound) body)
        TypeViewTyAppNode fun arg ->
          OverlapTyApp (go bound fun) (go bound arg)
        TypeViewForallNode _ identity mbBound body ->
          OverlapForall (fmap (go bound) mbBound) (go (pushBound identity bound) body)
        TypeViewMuNode _ identity body ->
          OverlapMu (go (pushBound identity bound) body)
        TypeViewBottomNode ->
          OverlapBottom

    overlapBinderKey bound identity =
      case Map.lookup identity bound of
        Just index0 -> Right index0
        Nothing -> Left (OverlapMeta side identity)

    pushBound identity =
      Map.insert identity 0 . fmap (+ 1)

unifyOverlapTypes :: OverlapSubstitution -> OverlapType -> OverlapType -> Maybe OverlapSubstitution
unifyOverlapTypes subst left right =
  case (applyOverlapSubstitution subst left, applyOverlapSubstitution subst right) of
    (OverlapMetaVar meta, ty) -> bindOverlapMeta meta ty subst
    (ty, OverlapMetaVar meta) -> bindOverlapMeta meta ty subst
    (OverlapRigidVar leftIndex, OverlapRigidVar rightIndex)
      | leftIndex == rightIndex -> Just subst
    (OverlapHead leftIdentity, OverlapHead rightIdentity)
      | leftIdentity == rightIdentity -> Just subst
    (OverlapCon leftIdentity leftArgs, OverlapCon rightIdentity rightArgs)
      | leftIdentity == rightIdentity,
        NE.length leftArgs == NE.length rightArgs ->
          unifyOverlapLists subst (NE.toList leftArgs) (NE.toList rightArgs)
    (OverlapMetaApp meta args, actual) ->
      unifyOverlapAppliedMeta subst meta (NE.toList args) actual
    (actual, OverlapMetaApp meta args) ->
      unifyOverlapAppliedMeta subst meta (NE.toList args) actual
    (OverlapRigidApp leftIndex leftArgs, OverlapRigidApp rightIndex rightArgs)
      | leftIndex == rightIndex,
        NE.length leftArgs == NE.length rightArgs ->
          unifyOverlapLists subst (NE.toList leftArgs) (NE.toList rightArgs)
    (OverlapArrow leftDom leftCod, OverlapArrow rightDom rightCod) -> do
      subst' <- unifyOverlapTypes subst leftDom rightDom
      unifyOverlapTypes subst' leftCod rightCod
    (OverlapTyLam leftBody, OverlapTyLam rightBody) ->
      unifyOverlapTypes subst leftBody rightBody
    (OverlapTyApp leftFun leftArg, OverlapTyApp rightFun rightArg) -> do
      subst' <- unifyOverlapTypes subst leftFun rightFun
      unifyOverlapTypes subst' leftArg rightArg
    (OverlapForall leftBound leftBody, OverlapForall rightBound rightBody) -> do
      subst' <- unifyOverlapMaybe subst leftBound rightBound
      unifyOverlapTypes subst' leftBody rightBody
    (OverlapMu leftBody, OverlapMu rightBody) ->
      unifyOverlapTypes subst leftBody rightBody
    (OverlapBottom, OverlapBottom) ->
      Just subst
    _ ->
      Nothing

unifyOverlapLists :: OverlapSubstitution -> [OverlapType] -> [OverlapType] -> Maybe OverlapSubstitution
unifyOverlapLists subst left right
  | length left /= length right = Nothing
  | otherwise =
      foldM
        (\acc (leftTy, rightTy) -> unifyOverlapTypes acc leftTy rightTy)
        subst
        (zip left right)

unifyOverlapMaybe :: OverlapSubstitution -> Maybe OverlapType -> Maybe OverlapType -> Maybe OverlapSubstitution
unifyOverlapMaybe subst Nothing Nothing = Just subst
unifyOverlapMaybe subst (Just left) (Just right) = unifyOverlapTypes subst left right
unifyOverlapMaybe _ _ _ = Nothing

unifyOverlapAppliedMeta :: OverlapSubstitution -> OverlapMeta -> [OverlapType] -> OverlapType -> Maybe OverlapSubstitution
unifyOverlapAppliedMeta subst meta templateArgs actual =
  case actual of
    OverlapCon identity actualArgs ->
      matchAppliedHead (OverlapHead identity) (NE.toList actualArgs)
    OverlapMetaApp actualMeta actualArgs ->
      matchAppliedHead (OverlapMetaVar actualMeta) (NE.toList actualArgs)
    OverlapRigidApp index0 actualArgs ->
      matchAppliedHead (OverlapRigidVar index0) (NE.toList actualArgs)
    _ -> Nothing
  where
    templateArgCount = length templateArgs

    matchAppliedHead headTy actualArgs
      | length actualArgs < templateArgCount = Nothing
      | otherwise = do
          let (headArgs, matchedArgs) = splitAt (length actualArgs - templateArgCount) actualArgs
          appliedHead <- applyOverlapTypeHead headTy headArgs
          subst' <- bindOverlapMeta meta appliedHead subst
          unifyOverlapLists subst' templateArgs matchedArgs

bindOverlapMeta :: OverlapMeta -> OverlapType -> OverlapSubstitution -> Maybe OverlapSubstitution
bindOverlapMeta meta ty subst =
  case Map.lookup meta subst of
    Just existing -> unifyOverlapTypes subst existing ty
    Nothing
      | ty == OverlapMetaVar meta -> Just subst
      | overlapMetaOccurs meta ty -> Nothing
      | otherwise -> Just (Map.insert meta ty subst)

applyOverlapSubstitution :: OverlapSubstitution -> OverlapType -> OverlapType
applyOverlapSubstitution subst ty =
  case ty of
    OverlapMetaVar meta ->
      maybe ty (applyOverlapSubstitution subst) (Map.lookup meta subst)
    OverlapRigidVar {} -> ty
    OverlapArrow dom cod ->
      OverlapArrow (applyOverlapSubstitution subst dom) (applyOverlapSubstitution subst cod)
    OverlapHead {} -> ty
    OverlapCon identity args ->
      OverlapCon identity (fmap (applyOverlapSubstitution subst) args)
    OverlapMetaApp meta args ->
      let args' = fmap (applyOverlapSubstitution subst) args
       in case Map.lookup meta subst >>= (\replacement -> applyOverlapTypeHead (applyOverlapSubstitution subst replacement) (NE.toList args')) of
            Just replacement -> replacement
            Nothing -> OverlapMetaApp meta args'
    OverlapRigidApp index0 args ->
      OverlapRigidApp index0 (fmap (applyOverlapSubstitution subst) args)
    OverlapTyLam body ->
      OverlapTyLam (applyOverlapSubstitution subst body)
    OverlapTyApp fun arg ->
      OverlapTyApp (applyOverlapSubstitution subst fun) (applyOverlapSubstitution subst arg)
    OverlapForall mbBound body ->
      OverlapForall
        (applyOverlapSubstitution subst <$> mbBound)
        (applyOverlapSubstitution subst body)
    OverlapMu body ->
      OverlapMu (applyOverlapSubstitution subst body)
    OverlapBottom ->
      OverlapBottom

applyOverlapTypeHead :: OverlapType -> [OverlapType] -> Maybe OverlapType
applyOverlapTypeHead headTy args =
  case headTy of
    OverlapMetaVar meta -> Just (mkMetaHead meta args)
    OverlapRigidVar index0 -> Just (mkRigidHead index0 args)
    OverlapHead identity -> Just (mkGlobalHead identity args)
    OverlapCon identity existing -> Just (mkGlobalHead identity (NE.toList existing ++ args))
    OverlapMetaApp meta existing -> Just (mkMetaHead meta (NE.toList existing ++ args))
    OverlapRigidApp index0 existing -> Just (mkRigidHead index0 (NE.toList existing ++ args))
    _ -> Nothing
  where
    mkMetaHead meta0 [] = OverlapMetaVar meta0
    mkMetaHead meta0 (arg : rest) = OverlapMetaApp meta0 (arg :| rest)
    mkRigidHead index0 [] = OverlapRigidVar index0
    mkRigidHead index0 (arg : rest) = OverlapRigidApp index0 (arg :| rest)
    mkGlobalHead identity [] = OverlapHead identity
    mkGlobalHead identity (arg : rest) = OverlapCon identity (arg :| rest)

overlapMetaOccurs :: OverlapMeta -> OverlapType -> Bool
overlapMetaOccurs needle ty =
  case ty of
    OverlapMetaVar meta -> meta == needle
    OverlapRigidVar {} -> False
    OverlapArrow dom cod -> overlapMetaOccurs needle dom || overlapMetaOccurs needle cod
    OverlapHead {} -> False
    OverlapCon _ args -> any (overlapMetaOccurs needle) args
    OverlapMetaApp meta args -> meta == needle || any (overlapMetaOccurs needle) args
    OverlapRigidApp _ args -> any (overlapMetaOccurs needle) args
    OverlapTyLam body -> overlapMetaOccurs needle body
    OverlapTyApp fun arg -> overlapMetaOccurs needle fun || overlapMetaOccurs needle arg
    OverlapForall mbBound body -> maybe False (overlapMetaOccurs needle) mbBound || overlapMetaOccurs needle body
    OverlapMu body -> overlapMetaOccurs needle body
    OverlapBottom -> False

-- Scope ----------------------------------------------------------------------

data Scope = Scope
  { scopeValues :: Map String ValueInfo,
    scopeValuesByIdentity :: Map SymbolIdentity ValueInfo,
    scopeTypes :: Map String DataInfo,
    scopeTypesByIdentity :: Map SymbolIdentity DataInfo,
    scopeHiddenTypes :: Map String DataInfo,
    scopeClasses :: Map String ClassInfo,
    scopeClassesByIdentity :: Map SymbolIdentity ClassInfo,
    scopeInstances :: [InstanceInfo]
  }
  deriving (Eq, Show)

type ClassIdentity = SymbolIdentity

data DisplayNameEnv = DisplayNameEnv
  { dneValues :: Map SymbolIdentity [String],
    dneTypes :: Map SymbolIdentity [String],
    dneClasses :: Map SymbolIdentity [String]
  }
  deriving (Eq, Show)

data KindEnv = KindEnv
  { kindTypeConstructors :: Map SymbolIdentity P.SrcKind,
    kindTypeVariables :: Map ResolvedTypeBinderRef KindTerm,
    kindMetaSubst :: Map Int KindTerm,
    kindNextMeta :: Int
  }
  deriving (Eq, Show)

data KindTerm
  = KTType
  | KTArrow KindTerm KindTerm
  | KTMeta Int
  deriving (Eq, Show)

emptyScope :: Scope
emptyScope = mkScopeWithHidden Builtins.builtinValues Map.empty Builtins.builtinOpaqueTypes Map.empty []

mkScopeWithHidden ::
  Map String ValueInfo ->
  Map String DataInfo ->
  Map String DataInfo ->
  Map String ClassInfo ->
  [InstanceInfo] ->
  Scope
mkScopeWithHidden values0 types0 hiddenTypes0 classes0 instances0 =
  Scope
    { scopeValues = values0,
      scopeValuesByIdentity = indexByIdentity valueInfoSymbolIdentity values0,
      scopeTypes = types0,
      scopeTypesByIdentity = indexByIdentity dataInfoSymbolIdentity (types0 `Map.union` hiddenTypes0),
      scopeHiddenTypes = hiddenTypes0,
      scopeClasses = classes0,
      scopeClassesByIdentity = indexByIdentity classInfoSymbolIdentity classes0,
      scopeInstances = instances0
    }

withScopeValues :: Map String ValueInfo -> Scope -> Scope
withScopeValues values0 scope =
  mkScopeWithHidden values0 (scopeTypes scope) (scopeHiddenTypes scope) (scopeClasses scope) (scopeInstances scope)

withScopeTypes :: Map String DataInfo -> Scope -> Scope
withScopeTypes types0 scope =
  mkScopeWithHidden (scopeValues scope) types0 (scopeHiddenTypes scope) (scopeClasses scope) (scopeInstances scope)

withScopeHiddenTypes :: Map String DataInfo -> Scope -> Scope
withScopeHiddenTypes hiddenTypes0 scope =
  mkScopeWithHidden (scopeValues scope) (scopeTypes scope) hiddenTypes0 (scopeClasses scope) (scopeInstances scope)

withScopeClasses :: Map String ClassInfo -> Scope -> Scope
withScopeClasses classes0 scope =
  mkScopeWithHidden (scopeValues scope) (scopeTypes scope) (scopeHiddenTypes scope) classes0 (scopeInstances scope)

withScopeInstances :: [InstanceInfo] -> Scope -> Scope
withScopeInstances instances0 scope =
  mkScopeWithHidden (scopeValues scope) (scopeTypes scope) (scopeHiddenTypes scope) (scopeClasses scope) instances0

scopeElaborateTypes :: Scope -> Map String DataInfo
scopeElaborateTypes scope =
  scopeTypes scope `Map.union` scopeHiddenTypes scope

indexByIdentity :: (Eq a) => (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity a
indexByIdentity =
  uniqueInfoByIdentity

emptyDisplayNameEnv :: DisplayNameEnv
emptyDisplayNameEnv =
  DisplayNameEnv
    { dneValues = Map.empty,
      dneTypes = Map.empty,
      dneClasses = Map.empty
    }

preferDisplayNames :: DisplayNameEnv -> DisplayNameEnv -> DisplayNameEnv
preferDisplayNames preferred fallback =
  DisplayNameEnv
    { dneValues = preferNames (dneValues preferred) (dneValues fallback),
      dneTypes = preferNames (dneTypes preferred) (dneTypes fallback),
      dneClasses = preferNames (dneClasses preferred) (dneClasses fallback)
    }
  where
    preferNames left right =
      uniqueDisplayNamesByIdentity
        [ (identity, name)
        | (identity, names) <- Map.toList left ++ Map.toList right,
          name <- names
        ]

displayNameEnvFromScope :: Scope -> DisplayNameEnv
displayNameEnvFromScope scope =
  DisplayNameEnv
    { dneValues =
        uniqueDisplayNamesByIdentity
          [ (valueInfoSymbolIdentity info, name)
            | (name, info) <- Map.toList (scopeValues scope)
          ],
      dneTypes =
        uniqueDisplayNamesByIdentity
          [ (dataInfoSymbolIdentity info, name)
            | (name, info) <- Map.toList (scopeTypes scope)
          ],
      dneClasses =
        uniqueDisplayNamesByIdentity
          [ (classInfoSymbolIdentity info, name)
            | (name, info) <- Map.toList (scopeClasses scope)
          ]
    }

displayNameEnvFromResolvedLocals :: ResolvedSemanticModule -> DisplayNameEnv
displayNameEnvFromResolvedLocals resolvedModule =
  DisplayNameEnv
    { dneValues = localNames (resolvedLocalValues localSymbols),
      dneTypes = localNames (resolvedLocalTypes localSymbols),
      dneClasses = localNames (resolvedLocalClasses localSymbols)
    }
  where
    localSymbols = resolvedSemanticModuleLocalSymbols resolvedModule
    localNames symbolsByName =
      uniqueDisplayNamesByIdentity
        [ (resolvedSymbolIdentity symbol, name)
          | (name, symbols) <- Map.toList symbolsByName,
            symbol <- symbols
        ]

resolvedProgramIdentityGenerator :: ResolvedSemanticProgramArtifact -> IdentityGenerator
resolvedProgramIdentityGenerator =
  identityGeneratorAfter . resolvedProgramGeneratedIdentities

displayNameEnvFromData :: Map String DataInfo -> DisplayNameEnv
displayNameEnvFromData dataInfos =
  emptyDisplayNameEnv
    { dneValues =
        uniqueDisplayNamesByIdentity
          [ (ctorInfoSymbol ctor, ctorName ctor)
            | dataInfo <- Map.elems dataInfos,
              ctor <- dataConstructors dataInfo
          ],
      dneTypes =
        uniqueDisplayNamesByIdentity
          [ (dataInfoSymbolIdentity dataInfo, name)
            | (name, dataInfo) <- Map.toList dataInfos
          ]
    }

displayNameEnvFromClasses :: Map String ClassInfo -> DisplayNameEnv
displayNameEnvFromClasses classInfos =
  emptyDisplayNameEnv
    { dneValues =
        uniqueDisplayNamesByIdentity
          [ (methodInfoSymbolIdentity methodInfo, methodName methodInfo)
            | classInfo <- Map.elems classInfos,
              methodInfo <- Map.elems (classMethodsByIdentity classInfo)
          ],
      dneClasses =
        uniqueDisplayNamesByIdentity
          [ (classInfoSymbolIdentity classInfo, name)
            | (name, classInfo) <- Map.toList classInfos
          ]
    }

displayNameEnvFromValues :: Map String ValueInfo -> DisplayNameEnv
displayNameEnvFromValues values0 =
  emptyDisplayNameEnv
    { dneValues =
        uniqueDisplayNamesByIdentity
          [ (valueInfoSymbolIdentity valueInfo, name)
            | (name, valueInfo) <- Map.toList values0
          ]
    }

checkedDataByIdentity :: Map String DataInfo -> Map SymbolIdentity DataInfo
checkedDataByIdentity =
  indexByIdentity dataInfoSymbolIdentity

checkedClassesByIdentity :: Map String ClassInfo -> Map SymbolIdentity ClassInfo
checkedClassesByIdentity =
  indexByIdentity classInfoSymbolIdentity

addVisibleByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map String a -> Either ProgramError (Map String a)
addVisibleByIdentity identityFor base incoming =
  foldM
    ( \acc (name, info) ->
        case Map.lookup name acc of
          Just existing
            | sameSymbolIdentity (identityFor existing) (identityFor info) -> Right acc
            | otherwise -> Left (ProgramDuplicateVisibleName name)
          Nothing -> Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

addValues :: Map String ValueInfo -> Map String ValueInfo -> Either ProgramError (Map String ValueInfo)
addValues =
  addVisibleByIdentity valueInfoSymbolIdentity

addTypes :: Map String DataInfo -> Map String DataInfo -> Either ProgramError (Map String DataInfo)
addTypes =
  addVisibleByIdentity dataInfoSymbolIdentity

addClasses :: Map String ClassInfo -> Map String ClassInfo -> Either ProgramError (Map String ClassInfo)
addClasses =
  addVisibleByIdentity classInfoSymbolIdentity

lookupValueInfoBySymbol :: Scope -> ResolvedSymbol -> TcM ValueInfo
lookupValueInfoBySymbol scope symbol =
  case lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) (scopeValuesByIdentity scope) of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownValue (resolvedSymbolDisplayName symbol))

lookupClassInfoBySymbol :: Scope -> ResolvedSymbol -> TcM ClassInfo
lookupClassInfoBySymbol scope symbol =
  case lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) (scopeClassesByIdentity scope) of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownClass (resolvedSymbolDisplayName symbol))

resolvedSymbolDisplayName :: ResolvedSymbol -> String
resolvedSymbolDisplayName =
  P.refDisplayName

isBuiltinTypeSymbol :: ResolvedSymbol -> Bool
isBuiltinTypeSymbol = Builtins.isBuiltinTypeSymbol

-- Program checking ------------------------------------------------------------

checkProgram :: P.Program -> Either ProgramError CheckedProgram
checkProgram program =
  checkProgramPackage (trivialProgramPackage program)

checkProgramPackage :: ProgramPackage -> Either ProgramError CheckedProgram
checkProgramPackage package = do
  graph <- programPackageModuleGraph package
  orderedProgram <- programPackageOrderedProgram package
  normalized <- normalizeTypeFamiliesInProgram orderedProgram
  rejectUnsupportedGeneralizedClassFeatures normalized
  resolved <- resolveProgram normalized
  checkResolvedProgramWithPackageGraph graph resolved

checkResolvedProgram :: ResolvedProgram -> Either ProgramError CheckedProgram
checkResolvedProgram =
  checkResolvedProgramWithContext Nothing

checkResolvedProgramWithPackageGraph :: PackageModuleGraph -> ResolvedProgram -> Either ProgramError CheckedProgram
checkResolvedProgramWithPackageGraph graph =
  checkResolvedProgramWithContext (Just graph)

checkResolvedProgramWithContext :: Maybe PackageModuleGraph -> ResolvedProgram -> Either ProgramError CheckedProgram
checkResolvedProgramWithContext =
  checkResolvedProgramWithContextAndBuiltinPreludeCheckCache
    processBuiltinPreludeCheckCache

checkResolvedProgramWithContextAndBuiltinPreludeCheckCache ::
  PreludeCache.BuiltinPreludeCheckCacheHandle ->
  Maybe PackageModuleGraph ->
  ResolvedProgram ->
  Either ProgramError CheckedProgram
checkResolvedProgramWithContextAndBuiltinPreludeCheckCache cacheHandle mbGraph resolved = do
  checkedProgram <- checkResolvedProgramCore cacheHandle mbGraph resolved
  case mbGraph of
    Nothing -> pure ()
    Just graph -> validateCheckedPackageInterface graph checkedProgram
  pure checkedProgram

checkResolvedProgramCore :: PreludeCache.BuiltinPreludeCheckCacheHandle -> Maybe PackageModuleGraph -> ResolvedProgram -> TcM CheckedProgram
checkResolvedProgramCore cacheHandle mbGraph resolved =
  -- The public checker is pure at its semantic boundary, but module-layer
  -- construction is intentionally an eager IO implementation: independent
  -- roots are checked concurrently and the Prelude cache is synchronized.
  -- Keep one checker implementation here so the ordinary API cannot silently
  -- fall back to the historical definition-at-a-time path.
  unsafePerformIO
    ( checkResolvedProgramCoreWithTiming
        defaultTimingConfig
        cacheHandle
        mbGraph
        resolved
    )
{-# NOINLINE checkResolvedProgramCore #-}

checkedProgramFromCheckedModules :: ResolvedProgram -> [CheckedModule] -> TcM CheckedProgram
checkedProgramFromCheckedModules resolved modulesChecked = do
  let mainBindings =
        [ binding
          | checked <- modulesChecked,
            binding <- checkedModuleBindings checked,
            checkedBindingExportedAsMain binding
        ]
  mainBinding <-
    case mainBindings of
      [] -> throwError ProgramMainNotFound
      [binding] -> pure binding
      bindings -> throwError (ProgramMultipleMainDefinitions (map checkedBindingName bindings))
  pure (mkCheckedProgram resolved modulesChecked (checkedBindingResolvedVar mainBinding))

checkLocatedProgram :: P.LocatedProgram -> Either ProgramDiagnostic CheckedProgram
checkLocatedProgram located =
  checkLocatedProgramPackage (trivialLocatedProgramPackage located)

checkLocatedProgramPackage :: LocatedProgramPackage -> Either ProgramDiagnostic CheckedProgram
checkLocatedProgramPackage =
  checkLocatedProgramPackageWithBuiltinPreludeCheckCacheForTest
    processBuiltinPreludeCheckCache

-- This explicit-cache entrypoint is exported only so the test-support facade
-- can run independent checks without mutating the process cache.
checkLocatedProgramPackageWithBuiltinPreludeCheckCacheForTest ::
  PreludeCache.BuiltinPreludeCheckCacheHandle ->
  LocatedProgramPackage ->
  Either ProgramDiagnostic CheckedProgram
checkLocatedProgramPackageWithBuiltinPreludeCheckCacheForTest cacheHandle package =
  case (locatedProgramPackageModuleGraph package, locatedProgramPackageOrderedProgram package) of
    (Left err, _) -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
    (_, Left err) -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
    (Right graph, Right orderedProgram) ->
      case do
        normalized <- normalizeTypeFamiliesInProgram (P.locatedProgram orderedProgram)
        rejectUnsupportedGeneralizedClassFeatures normalized
        resolved <- resolveProgram normalized
        checkResolvedProgramWithContextAndBuiltinPreludeCheckCache cacheHandle (Just graph) resolved of
        Right checked -> Right checked
        Left err -> Left (diagnosticForProgramError (Just orderedProgram) err)

checkLocatedProgramPackageWithTiming :: TimingConfig -> LocatedProgramPackage -> IO (Either ProgramDiagnostic CheckedProgram)
checkLocatedProgramPackageWithTiming timing =
  checkLocatedProgramPackageWithTimingAndBuiltinPreludeCheckCacheForTest
    timing
    processBuiltinPreludeCheckCache

-- Timed test checks use the same explicit handle as ordinary test checks, so
-- call order cannot select a different cache or semantic Prelude builder.
checkLocatedProgramPackageWithTimingAndBuiltinPreludeCheckCacheForTest ::
  TimingConfig ->
  PreludeCache.BuiltinPreludeCheckCacheHandle ->
  LocatedProgramPackage ->
  IO (Either ProgramDiagnostic CheckedProgram)
checkLocatedProgramPackageWithTimingAndBuiltinPreludeCheckCacheForTest timing cacheHandle package = do
  graphResult <-
    timeProgramIO
      timing
      "program.check.module-graph"
      (evaluate (locatedProgramPackageModuleGraph package))
  case graphResult of
    Left err ->
      pure (Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err))
    Right graph -> do
      orderedResult <-
        timeProgramIO
          timing
          "program.check.module-order"
          (evaluate (locatedProgramPackageOrderedProgram package))
      case orderedResult of
        Left err ->
          pure (Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err))
        Right orderedProgram -> do
          normalizedResult <-
            timeProgramIO
              timing
              "program.check.normalize-type-families"
              (evaluate (normalizeTypeFamiliesInProgram (P.locatedProgram orderedProgram)))
          case normalizedResult of
            Left err ->
              pure (Left (diagnosticForProgramError (Just orderedProgram) err))
            Right normalized -> do
              generalizedClassResult <-
                timeProgramIO
                  timing
                  "program.check.reject-generalized-class-features"
                  (evaluate (rejectUnsupportedGeneralizedClassFeatures normalized))
              case generalizedClassResult of
                Left err ->
                  pure (Left (diagnosticForProgramError (Just orderedProgram) err))
                Right () -> do
                  resolvedResult <-
                    timeProgramIO
                      timing
                      "program.check.resolve"
                      (evaluate (resolveProgram normalized))
                  case resolvedResult of
                    Left err ->
                      pure (Left (diagnosticForProgramError (Just orderedProgram) err))
                    Right resolved -> do
                      checkedResult <-
                        timeProgramIO
                          timing
                          "program.check.modules"
                          (checkResolvedProgramCoreWithTiming timing cacheHandle (Just graph) resolved)
                      case checkedResult of
                        Left err ->
                          pure (Left (diagnosticForProgramError (Just orderedProgram) err))
                        Right checked -> do
                          interfaceResult <-
                            timeProgramIO
                              timing
                              "program.check.package-interface"
                              (evaluate (validateCheckedPackageInterface graph checked))
                          pure $
                            case interfaceResult of
                              Left err -> Left (diagnosticForProgramError (Just orderedProgram) err)
                              Right () -> Right checked

checkResolvedProgramCoreWithTiming :: TimingConfig -> PreludeCache.BuiltinPreludeCheckCacheHandle -> Maybe PackageModuleGraph -> ResolvedProgram -> IO (Either ProgramError CheckedProgram)
checkResolvedProgramCoreWithTiming timing cacheHandle mbGraph resolved = do
  modulesResult <-
    checkModulesWithTiming
      timing
      cacheHandle
      mbGraph
      (resolvedProgramSemanticArtifact resolved)
  case modulesResult of
    Left err ->
      pure (Left err)
    Right modulesChecked ->
      timeProgramDetailIO
        timing
        "program.check.modules.main-binding"
        (evaluate (checkedProgramFromCheckedModules resolved modulesChecked))

validateCheckedPackageInterface :: PackageModuleGraph -> CheckedProgram -> TcM ()
validateCheckedPackageInterface graph checked =
  liftEitherWithInterface (packageInterfaceFromCheckedProgram graph checked) >> pure ()

liftEitherWithInterface :: Either ProgramInterfaceError a -> TcM a
liftEitherWithInterface =
  either (throwError . ProgramPipelineError . interfaceErrorMessage) pure
  where
    interfaceErrorMessage err =
      "invalid .mlfp interface artifact: " ++ renderProgramInterfaceError err

checkModulesWithTiming :: TimingConfig -> PreludeCache.BuiltinPreludeCheckCacheHandle -> Maybe PackageModuleGraph -> ResolvedSemanticProgramArtifact -> IO (TcM [CheckedModule])
checkModulesWithTiming timing cacheHandle mbGraph artifact@(ResolvedSemanticProgramArtifact resolvedModules) = do
  distinctResult <-
    timeProgramDetailIO
      timing
      "program.check.modules.distinct"
      ( evaluate $ do
          ensureDistinctBy ProgramDuplicateModule resolvedSemanticModuleName resolvedModules
          ensureDistinctModuleIdentities "resolved module" resolvedSemanticModuleIdentity resolvedModules
          mapM_ ensureDistinctResolvedModuleSymbolIdentities resolvedModules
      )
  case distinctResult of
    Left err ->
      pure (Left err)
    Right () ->
      go (resolvedProgramIdentityGenerator artifact) [] [] resolvedModules
  where
    nodesByModule =
      Map.fromList
        [ (packageModuleName (packageModuleGraphNodeId node), node)
          | graph <- maybe [] pure mbGraph,
            node <- packageModuleGraphNodes graph
        ]

    go _ _ checkedAcc [] =
      pure (Right (reverse checkedAcc))
    go generator0 interfaceAcc checkedAcc (resolvedModule : rest) = do
      let moduleName0 = resolvedSemanticModuleName resolvedModule
          isBuiltinPrelude = isBuiltinPreludeModule nodesByModule mbGraph resolvedModule
      checkedResult <-
        if isBuiltinPrelude
          then
            timeProgramDetailIO
              timing
              ("program.check.module." ++ moduleName0 ++ ".cache")
              (checkedBuiltinPreludeModuleWithTiming timing cacheHandle generator0 resolvedModule)
          else
            timeProgramDetailIO
              timing
              ("program.check.module." ++ moduleName0)
              (checkModuleWithTiming timing generator0 resolvedModule interfaceAcc)
      case checkedResult of
        Left err ->
          pure (Left err)
        Right (checked, generator1) -> do
          interfaceResult <-
            timeProgramDetailIO
              timing
              ("program.check.module-interface." ++ moduleName0)
              (evaluate $ do
                node <- moduleInterfaceNodeForResolved nodesByModule mbGraph resolvedModule
                liftEitherWithInterface (moduleInterfaceFromCheckedModule node checked))
          case interfaceResult of
            Left err ->
              pure (Left err)
            Right interface ->
              go generator1 (interface : interfaceAcc) (checked : checkedAcc) rest

checkedBuiltinPreludeModule :: PreludeCache.BuiltinPreludeCheckCacheHandle -> IdentityGenerator -> ResolvedSemanticModule -> TcM (CheckedModule, IdentityGenerator)
checkedBuiltinPreludeModule cacheHandle generator0 resolvedModule =
  attachClientGenerator generator0 $
    unsafePerformIO $
      PreludeCache.cachedBuiltinPreludeCheck
        cacheHandle
        resolvedModule
{-# NOINLINE checkedBuiltinPreludeModule #-}

-- This test-only seam exercises the actual cached Prelude builder and its
-- client-supply attachment without manufacturing thousands of source binders
-- merely to move the client supply past a historical fixed boundary.
nextClientIdentityAfterCachedBuiltinPreludeForTest ::
  PreludeCache.BuiltinPreludeCheckCacheHandle ->
  UniqueIdentity ->
  ResolvedSemanticModule ->
  TcM UniqueIdentity
nextClientIdentityAfterCachedBuiltinPreludeForTest cacheHandle lastClientIdentity resolvedModule = do
  (_, generator) <-
    checkedBuiltinPreludeModule
      cacheHandle
      (identityGeneratorAfter [lastClientIdentity])
      resolvedModule
  pure (fst (freshIdentity generator))

checkedBuiltinPreludeModuleWithTiming ::
  TimingConfig ->
  PreludeCache.BuiltinPreludeCheckCacheHandle ->
  IdentityGenerator ->
  ResolvedSemanticModule ->
  IO (TcM (CheckedModule, IdentityGenerator))
checkedBuiltinPreludeModuleWithTiming timing cacheHandle generator0 resolvedModule =
  attachClientGenerator generator0
    <$> PreludeCache.cachedBuiltinPreludeCheckWithTiming
      timing
      "program.check.module.Prelude.semantic-build"
      cacheHandle
      resolvedModule

-- Both ordinary and timed entrypoints cache this one semantic builder. Timing
-- wraps its evaluation but cannot select the batching/module-context checker or
-- otherwise change the cached Prelude artifact by call order.
buildBuiltinPreludeCheckedModule ::
  ResolvedSemanticModule ->
  TcM CheckedModule
buildBuiltinPreludeCheckedModule resolvedModule =
  fst <$> checkModule builtinPreludeCheckIdentityGenerator resolvedModule []

-- The process cache owns the only production Prelude builder. Cache lookups
-- receive no build action, so a caller cannot populate a key with a different
-- semantic artifact.
processBuiltinPreludeCheckCache :: PreludeCache.BuiltinPreludeCheckCacheHandle
processBuiltinPreludeCheckCache =
  unsafePerformIO newBuiltinPreludeCheckCacheForTest
{-# NOINLINE processBuiltinPreludeCheckCache #-}

-- Exported only through the test-support facade so tests can obtain an
-- isolated cache while retaining the production semantic builder.
newBuiltinPreludeCheckCacheForTest ::
  IO PreludeCache.BuiltinPreludeCheckCacheHandle
newBuiltinPreludeCheckCacheForTest =
  PreludeCache.newBuiltinPreludeCheckCache
    (evaluate . buildBuiltinPreludeCheckedModule)

-- The builtin Prelude is a package-owned artifact, so the identity supply
-- threaded through its checker belongs to the Prelude rather than to whichever
-- client happens to import it. Source resolution and ordinary program checking
-- allocate upward from non-negative identities; the Prelude's authoritative
-- checker supply allocates downward in the lower half of the negative range.
--
-- Constructor surface lowering and metadata finalization both consume that
-- authoritative supply.  Attaching the cached artifact still advances the
-- caller over the complete finalized module inventory: this keeps the cache
-- boundary valid for every generated-identity owner, independent of supply
-- direction or which construction phase introduced the identity.

attachClientGenerator ::
  IdentityGenerator ->
  TcM PreludeCache.CachedBuiltinPreludeCheck ->
  TcM (CheckedModule, IdentityGenerator)
attachClientGenerator generator0 cachedResult = do
  cached <- cachedResult
  pure
    ( PreludeCache.cachedBuiltinPreludeCheckedModule cached,
      PreludeCache.advanceIdentityGeneratorPastCachedBuiltinPrelude
        generator0
        cached
    )

isBuiltinPreludeModule ::
  Map P.ModuleName PackageModuleGraphNode ->
  Maybe PackageModuleGraph ->
  ResolvedSemanticModule ->
  Bool
isBuiltinPreludeModule nodesByModule mbGraph resolvedModule =
  resolvedSemanticModuleName resolvedModule == "Prelude"
    && null (P.moduleImports (resolvedSemanticModuleSyntax resolvedModule))
    && case mbGraph of
      Nothing ->
        False
      Just _ ->
        case Map.lookup "Prelude" nodesByModule of
          Just node ->
            packageModuleGraphNodeIsBuiltinPrelude node
          Nothing ->
            False

moduleInterfaceNodeForResolved ::
  Map P.ModuleName PackageModuleGraphNode ->
  Maybe PackageModuleGraph ->
  ResolvedSemanticModule ->
  TcM PackageModuleGraphNode
moduleInterfaceNodeForResolved nodesByModule mbGraph resolvedModule =
  case mbGraph of
    Just _ ->
      case Map.lookup moduleName0 nodesByModule of
        Just node -> pure node
        Nothing ->
          throwError
            ( ProgramPipelineError
                ("missing package module graph node for checked module `" ++ moduleName0 ++ "`")
            )
    Nothing ->
      pure
        PackageModuleGraphNode
          { packageModuleGraphNodeId = PackageModuleId trivialPackageId moduleName0,
            packageModuleGraphNodeSourcePath = Nothing,
            packageModuleGraphNodeImports =
              [ PackageModuleId trivialPackageId (resolvedImportDefiningModule imp)
                | imp <- P.moduleImports (resolvedSemanticModuleSyntax resolvedModule)
              ]
          }
  where
    moduleName0 = resolvedSemanticModuleName resolvedModule

priorInterfaceMaps :: [ModuleInterface] -> TcM (Map SymbolIdentity ModuleExports, Map SymbolIdentity (Map SymbolIdentity DataInfo))
priorInterfaceMaps priorInterfaces = do
  ensureDistinctModuleIdentities "prior interface module" moduleInterfaceIdentity priorInterfaces
  pure
    ( Map.fromList [(moduleInterfaceIdentity interface, moduleInterfaceExports interface) | interface <- priorInterfaces],
      Map.fromList [(moduleInterfaceIdentity interface, moduleInterfaceDataByIdentity interface) | interface <- priorInterfaces]
    )

checkModule :: IdentityGenerator -> ResolvedSemanticModule -> [ModuleInterface] -> TcM (CheckedModule, IdentityGenerator)
checkModule generator0 resolvedModule priorInterfaces = do
  (priorExportsByIdentity, priorData) <- priorInterfaceMaps priorInterfaces
  let resolvedSyntax = resolvedSemanticModuleSyntax resolvedModule
      moduleName0 = resolvedSemanticModuleName resolvedModule
      priorInstances = concatMap moduleInterfaceInstances priorInterfaces
      unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExportsByIdentity (P.moduleImports resolvedSyntax)
      visibleImportedInstances =
        visibleInstancesForImports priorExportsByIdentity priorData priorInstances unqualifiedClassIdentities (P.moduleImports resolvedSyntax)
  ensureDistinctImportAliases (P.moduleImports resolvedSyntax)
  rejectUnsupportedTypeFamilies resolvedSyntax
  rejectUnsupportedGeneralizedClassFeaturesModule P.refDisplayName resolvedSrcTypeToSrcType resolvedSyntax
  importScope <- buildImportScopeResolved priorExportsByIdentity (P.moduleImports resolvedSyntax)
  let importedEnv = displayNameEnvFromScope importScope
      localSymbolEnv = displayNameEnvFromResolvedLocals resolvedModule
      baseNameEnv = localSymbolEnv `preferDisplayNames` importedEnv
  localData <- buildLocalDataInfo baseNameEnv resolvedSyntax
  let dataNameEnv = displayNameEnvFromData localData `preferDisplayNames` baseNameEnv
  localClasses <- buildLocalClassInfo dataNameEnv resolvedSyntax
  let classNameEnv = displayNameEnvFromClasses localClasses `preferDisplayNames` dataNameEnv
  localDefs <- buildLocalDefInfo classNameEnv resolvedSyntax
  localValues0 <- addConstructorValues localData
  localValues1 <- mergeMaps ProgramDuplicateValue localValues0 localDefs
  let localMethodValues =
        Map.fromList
          [ ( methodName method,
              OverloadedMethod
                { valueInfoSymbol = methodInfoSymbolIdentity method,
                  valueMethodInfo = method
                }
            )
            | classInfo <- Map.elems localClasses,
              method <- Map.elems (classMethodsByIdentity classInfo)
          ]
  localValues <- mergeMaps ProgramDuplicateValue localValues1 localMethodValues
  let valueNameEnv = displayNameEnvFromValues localValues `preferDisplayNames` classNameEnv
  valueScope <- liftEither =<< pure (addValues (scopeValues importScope) localValues)
  typeScope <- liftEither =<< pure (addTypes (scopeTypes importScope) localData)
  classScope <- liftEither =<< pure (addClasses (scopeClasses importScope) localClasses)
  let scope0 = mkScopeWithHidden valueScope typeScope (scopeHiddenTypes importScope) classScope (scopeInstances importScope ++ visibleImportedInstances)
      fullNameEnv = valueNameEnv `preferDisplayNames` displayNameEnvFromScope scope0
  validateModuleKinds scope0 resolvedSyntax
  validateLocalClassMethodConstraints scope0 resolvedSyntax
  (derivedInstances, generator1) <-
    synthesizeDerivedInstances
      (resolvedSemanticModuleIdentity resolvedModule)
      generator0
      fullNameEnv
      scope0
      resolvedSyntax
  (instanceSkeletons, generator2) <-
    buildInstanceSkeletons (resolvedSemanticModuleIdentity resolvedModule) generator1 fullNameEnv scope0 resolvedSyntax derivedInstances
  let scope1 = withScopeInstances (scopeInstances scope0 ++ instanceSkeletons) scope0
  let elaborateScope = mkElaborateScope (scopeValues scope1) (scopeElaborateTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
  finalizeContext <- mkFinalizeContext elaborateScope
  (constructorBindings, generatorAfterConstructors) <-
    checkConstructors
      generator2
      finalizeContext
      elaborateScope
      localData
  (instanceBindings, generator3) <- checkInstances generatorAfterConstructors fullNameEnv finalizeContext elaborateScope scope1 (derivedInstances ++ explicitInstances resolvedSyntax)
  (defBindings, generator4) <- checkDefs generator3 finalizeContext elaborateScope scope1 (moduleDefDecls resolvedSyntax)
  exports <- buildExports resolvedSyntax localData localClasses localValues
  let exportedMain = exportedMainIdentity resolvedSyntax exports
      markExportedMain binding =
        binding
          { checkedBindingExportedAsMain =
              maybe False (\identity -> checkedBindingValueIdentity binding == Just identity) exportedMain
          }
      checkedBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings
      checkedModule =
        CheckedModule
          { checkedModuleName = moduleName0,
            checkedModuleIdentity = resolvedSemanticModuleIdentity resolvedModule,
            checkedModuleBindings = checkedBindings,
            checkedModuleData = checkedDataByIdentity localData,
            checkedModuleClasses = checkedClassesByIdentity localClasses,
            checkedModuleInstances = instanceSkeletons,
            checkedModuleExports = exports
          }
  pure
    ( checkedModule,
      checkedBindingsIdentityGenerator generator4 checkedBindings
    )

checkModuleWithTiming :: TimingConfig -> IdentityGenerator -> ResolvedSemanticModule -> [ModuleInterface] -> IO (TcM (CheckedModule, IdentityGenerator))
checkModuleWithTiming timing generator0 resolvedModule priorInterfaces = do
  let resolvedSyntax = resolvedSemanticModuleSyntax resolvedModule
      moduleName0 = resolvedSemanticModuleName resolvedModule
      priorInstances = concatMap moduleInterfaceInstances priorInterfaces
      timePhase :: String -> TcM a -> IO (TcM a)
      timePhase = timeCheckModulePhase timing moduleName0
  preflightResult <-
    timePhase "preflight" $ do
      priorMaps <- priorInterfaceMaps priorInterfaces
      ensureDistinctImportAliases (P.moduleImports resolvedSyntax)
      rejectUnsupportedTypeFamilies resolvedSyntax
      rejectUnsupportedGeneralizedClassFeaturesModule P.refDisplayName resolvedSrcTypeToSrcType resolvedSyntax
      pure priorMaps
  case preflightResult of
    Left err -> pure (Left err)
    Right (priorExportsByIdentity, priorData) -> do
      let unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExportsByIdentity (P.moduleImports resolvedSyntax)
          visibleImportedInstances =
            visibleInstancesForImports priorExportsByIdentity priorData priorInstances unqualifiedClassIdentities (P.moduleImports resolvedSyntax)
      importScopeResult <- timePhase "import-scope" (buildImportScopeResolved priorExportsByIdentity (P.moduleImports resolvedSyntax))
      case importScopeResult of
        Left err -> pure (Left err)
        Right importScope -> do
          let importedEnv = displayNameEnvFromScope importScope
              localSymbolEnv = displayNameEnvFromResolvedLocals resolvedModule
              baseNameEnv = localSymbolEnv `preferDisplayNames` importedEnv
          localDataResult <- timePhase "local-data" (buildLocalDataInfo baseNameEnv resolvedSyntax)
          case localDataResult of
            Left err -> pure (Left err)
            Right localData -> do
              let dataNameEnv = displayNameEnvFromData localData `preferDisplayNames` baseNameEnv
              localClassesResult <- timePhase "local-classes" (buildLocalClassInfo dataNameEnv resolvedSyntax)
              case localClassesResult of
                Left err -> pure (Left err)
                Right localClasses -> do
                  let classNameEnv = displayNameEnvFromClasses localClasses `preferDisplayNames` dataNameEnv
                  localDefsResult <- timePhase "local-defs" (buildLocalDefInfo classNameEnv resolvedSyntax)
                  case localDefsResult of
                    Left err -> pure (Left err)
                    Right localDefs -> do
                      localValuesResult <-
                        timePhase "local-values" $ do
                          localValues0 <- addConstructorValues localData
                          localValues1 <- mergeMaps ProgramDuplicateValue localValues0 localDefs
                          let localMethodValues =
                                Map.fromList
                                  [ ( methodName method,
                                      OverloadedMethod
                                        { valueInfoSymbol = methodInfoSymbolIdentity method,
                                          valueMethodInfo = method
                                        }
                                    )
                                    | classInfo <- Map.elems localClasses,
                                      method <- Map.elems (classMethodsByIdentity classInfo)
                                  ]
                          mergeMaps ProgramDuplicateValue localValues1 localMethodValues
                      case localValuesResult of
                        Left err -> pure (Left err)
                        Right localValues -> do
                          let valueNameEnv = displayNameEnvFromValues localValues `preferDisplayNames` classNameEnv
                          scopeResult <-
                            timePhase "scopes" $ do
                              valueScope <- liftEither (addValues (scopeValues importScope) localValues)
                              typeScope <- liftEither (addTypes (scopeTypes importScope) localData)
                              classScope <- liftEither (addClasses (scopeClasses importScope) localClasses)
                              let scope0 = mkScopeWithHidden valueScope typeScope (scopeHiddenTypes importScope) classScope (scopeInstances importScope ++ visibleImportedInstances)
                                  fullNameEnv = valueNameEnv `preferDisplayNames` displayNameEnvFromScope scope0
                              pure (scope0, fullNameEnv)
                          case scopeResult of
                            Left err -> pure (Left err)
                            Right (scope0, fullNameEnv) -> do
                              validationResult <-
                                timePhase "validations" $ do
                                  validateModuleKinds scope0 resolvedSyntax
                                  validateLocalClassMethodConstraints scope0 resolvedSyntax
                              case validationResult of
                                Left err -> pure (Left err)
                                Right () -> do
                                  derivedInstancesResult <-
                                    timePhase
                                      "derived-instances"
                                      ( synthesizeDerivedInstances
                                          (resolvedSemanticModuleIdentity resolvedModule)
                                          generator0
                                          fullNameEnv
                                          scope0
                                          resolvedSyntax
                                      )
                                  case derivedInstancesResult of
                                    Left err -> pure (Left err)
                                    Right (derivedInstances, generator1) -> do
                                      instanceSkeletonsResult <-
                                        timePhase "instance-skeletons" $
                                          buildInstanceSkeletons (resolvedSemanticModuleIdentity resolvedModule) generator1 fullNameEnv scope0 resolvedSyntax derivedInstances
                                      case instanceSkeletonsResult of
                                        Left err -> pure (Left err)
                                        Right (instanceSkeletons, generator2) -> do
                                          let scope1 = withScopeInstances (scopeInstances scope0 ++ instanceSkeletons) scope0
                                              elaborateScope = mkElaborateScope (scopeValues scope1) (scopeElaborateTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
                                          finalizeContextResult <-
                                            timeCheckModulePhaseIO timing moduleName0 "finalize-context" $
                                              mkFinalizeContextWithTiming
                                                timing
                                                ("program.check.module." ++ moduleName0 ++ ".finalize-context")
                                                elaborateScope
                                          case finalizeContextResult of
                                            Left err -> pure (Left err)
                                            Right finalizeContext -> do
                                              checkedResult <-
                                                finalizeCheckedModuleWithTiming
                                                  timing
                                                  moduleName0
                                                  (resolvedSemanticModuleIdentity resolvedModule)
                                                  resolvedSyntax
                                                  localData
                                                  localClasses
                                                  localValues
                                                  instanceSkeletons
                                                  fullNameEnv
                                                  finalizeContext
                                                  elaborateScope
                                                  scope1
                                                  derivedInstances
                                                  generator2
                                              pure checkedResult

finalizeCheckedModuleWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  SymbolIdentity ->
  P.ResolvedModuleSyntax ->
  Map String DataInfo ->
  Map String ClassInfo ->
  Map String ValueInfo ->
  [InstanceInfo] ->
  DisplayNameEnv ->
  FinalizeContext ->
  ElaborateScope ->
  Scope ->
  [P.ResolvedInstanceDecl] ->
  IdentityGenerator ->
  IO (TcM (CheckedModule, IdentityGenerator))
finalizeCheckedModuleWithTiming timing moduleName0 moduleIdentity resolvedSyntax localData localClasses localValues instanceSkeletons displayEnv finalizeContext elaborateScope scope1 derivedInstances generator0 = do
  constructorBindingsResult <-
    timeCheckModulePhaseIO timing moduleName0 "constructor-bindings" $
      checkConstructorsWithTiming timing moduleName0 generator0 finalizeContext elaborateScope localData
  case constructorBindingsResult of
    Left err -> pure (Left err)
    Right (constructorBindings, generatorAfterConstructors) -> do
      instanceBindingsResult <-
        timeCheckModulePhaseIO timing moduleName0 "instance-bindings" $
          checkInstancesWithTiming timing moduleName0 generatorAfterConstructors displayEnv finalizeContext elaborateScope scope1 (derivedInstances ++ explicitInstances resolvedSyntax)
      case instanceBindingsResult of
        Left err -> pure (Left err)
        Right (instanceBindings, generator1) -> do
          defBindingsResult <-
            timeCheckModulePhaseIO timing moduleName0 "def-bindings" $
              checkDefsWithTiming timing moduleName0 generator1 finalizeContext elaborateScope scope1 (moduleDefDecls resolvedSyntax)
          case defBindingsResult of
            Left err -> pure (Left err)
            Right (defBindings, generator2) -> do
              exportsResult <- timeCheckModulePhase timing moduleName0 "exports" (buildExports resolvedSyntax localData localClasses localValues)
              pure $ do
                exports <- exportsResult
                let exportedMain = exportedMainIdentity resolvedSyntax exports
                    markExportedMain binding =
                      binding
                        { checkedBindingExportedAsMain =
                            maybe False (\identity -> checkedBindingValueIdentity binding == Just identity) exportedMain
                        }
                    checkedBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings
                    checkedModule =
                      CheckedModule
                        { checkedModuleName = moduleName0,
                          checkedModuleIdentity = moduleIdentity,
                          checkedModuleBindings = checkedBindings,
                          checkedModuleData = checkedDataByIdentity localData,
                          checkedModuleClasses = checkedClassesByIdentity localClasses,
                          checkedModuleInstances = instanceSkeletons,
                          checkedModuleExports = exports
                        }
                pure
                  ( checkedModule,
                    checkedBindingsIdentityGenerator generator2 checkedBindings
                  )

timeCheckModulePhase :: TimingConfig -> P.ModuleName -> String -> TcM a -> IO (TcM a)
timeCheckModulePhase timing moduleName0 phase action =
  timeProgramDetailIO
    timing
    ("program.check.module." ++ moduleName0 ++ "." ++ phase)
    (evaluate action)

timeCheckModulePhaseIO :: TimingConfig -> P.ModuleName -> String -> IO (TcM a) -> IO (TcM a)
timeCheckModulePhaseIO timing moduleName0 phase action =
  timeProgramDetailIO
    timing
    ("program.check.module." ++ moduleName0 ++ "." ++ phase)
    action

timeCheckModuleOperation :: TimingConfig -> P.ModuleName -> String -> TcM a -> IO (TcM a)
timeCheckModuleOperation timing moduleName0 operation action =
  timeProgramOperationIO
    timing
    (checkModuleOperationLabel moduleName0 operation)
    (evaluate action)

checkModuleOperationLabel :: P.ModuleName -> String -> String
checkModuleOperationLabel moduleName0 operation =
  "program.check.operation." ++ moduleName0 ++ "." ++ sanitizeTimingLabel operation

sanitizeTimingLabel :: String -> String
sanitizeTimingLabel =
  map sanitizeChar
  where
    sanitizeChar char
      | isAlphaNum char = char
      | otherwise = '_'

checkConstructors :: IdentityGenerator -> FinalizeContext -> ElaborateScope -> Map String DataInfo -> TcM ([CheckedBinding], IdentityGenerator)
checkConstructors generator0 finalizeContext elaborateScope localData =
  go generator0 [] constructors
  where
    constructors =
      [ ctor
      | dataInfo <- Map.elems localData,
        ctor <- dataConstructors dataInfo
      ]

    go generator acc [] = Right (reverse acc, generator)
    go generator acc (ctor : rest) = do
      (lowered, generatorAfterLowering) <-
        lowerConstructorBinding generator elaborateScope ctor
      (binding, generator') <-
        liftEither $
          finalizeBindingAllowOpaqueWithContextFromSupply
            generatorAfterLowering
            finalizeContext
            lowered
      go generator' (binding : acc) rest

checkConstructorsWithTiming :: TimingConfig -> P.ModuleName -> IdentityGenerator -> FinalizeContext -> ElaborateScope -> Map String DataInfo -> IO (TcM ([CheckedBinding], IdentityGenerator))
checkConstructorsWithTiming timing moduleName0 generator0 finalizeContext elaborateScope localData =
  go generator0 []
    [ ctor
      | dataInfo <- Map.elems localData,
        ctor <- dataConstructors dataInfo
      ]
  where
    go generator acc [] = pure (Right (reverse acc, generator))
    go generator acc (ctor : rest) = do
      case lowerConstructorBinding generator elaborateScope ctor of
        Left err -> pure (Left err)
        Right (lowered, generatorAfterLowering) -> do
          result <-
            timeCheckModuleOperation timing moduleName0 ("constructor." ++ ctorName ctor) $
              finalizeBindingAllowOpaqueWithContextFromSupply
                generatorAfterLowering
                finalizeContext
                lowered
          case result of
            Left err -> pure (Left err)
            Right (binding, generator') -> go generator' (binding : acc) rest

checkInstancesWithTiming :: TimingConfig -> P.ModuleName -> IdentityGenerator -> DisplayNameEnv -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedInstanceDecl] -> IO (TcM ([CheckedBinding], IdentityGenerator))
checkInstancesWithTiming timing moduleName0 generator0 displayEnv finalizeContext elaborateScope scope instDecls =
  go generator0 [] (zip [(1 :: Int) ..] instDecls)
  where
    go generator acc [] = do
      finalizeBindingsAllowOpaqueWithContextWithTimingFromSupply
          timing
          (checkModuleOperationLabel moduleName0 "instance_methods.group_finalize")
          generator
          finalizeContext
          (concat (reverse acc))
    go generator acc ((index, instDecl) : rest) = do
      result <-
        lowerInstanceWithTiming
          timing
          moduleName0
          (instanceTimingLabel index instDecl)
          generator
          displayEnv
          elaborateScope
          scope
          instDecl
      case result of
        Left err -> pure (Left err)
        Right (lowereds, generator') -> go generator' (lowereds : acc) rest

instanceTimingLabel :: Int -> P.ResolvedInstanceDecl -> String
instanceTimingLabel index instDecl =
  "instance."
    ++ show index
    ++ "."
    ++ P.refDisplayName (P.instanceDeclClass instDecl)
    ++ "."
    ++ intercalate "_" (map (sanitizeType . resolvedSrcTypeToSrcType) (NE.toList (P.instanceDeclTypes instDecl)))

lowerInstanceWithTiming :: TimingConfig -> P.ModuleName -> String -> IdentityGenerator -> DisplayNameEnv -> ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> IO (TcM ([LoweredBinding], IdentityGenerator))
lowerInstanceWithTiming timing moduleName0 instanceLabel generator displayEnv elaborateScope scope instDecl = do
  instanceResult <-
    timeCheckModuleOperation timing moduleName0 (instanceLabel ++ ".lookup") $
      lookupInstanceForDecl displayEnv scope instDecl
  case instanceResult of
    Left err -> pure (Left err)
    Right (classInfo, instanceInfo) ->
      lowerInstanceMethodsWithTiming timing moduleName0 instanceLabel generator elaborateScope classInfo instanceInfo (instanceHeadTypeViews instanceInfo) (P.instanceDeclMethods instDecl)

lowerInstanceMethodsWithTiming :: TimingConfig -> P.ModuleName -> String -> IdentityGenerator -> ElaborateScope -> ClassInfo -> InstanceInfo -> NonEmpty TypeView -> [P.ResolvedMethodDef] -> IO (TcM ([LoweredBinding], IdentityGenerator))
lowerInstanceMethodsWithTiming timing moduleName0 instanceLabel generator0 elaborateScope classInfo instanceInfo instanceHeadViews methodDefs =
  go generator0 [] methodDefs
  where
    go generator acc [] = pure (Right (reverse acc, generator))
    go generator acc (methodDef : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 (instanceLabel ++ ".method." ++ P.refDisplayName (P.methodDefName methodDef) ++ ".lower") $
          lowerInstanceMethodWithGenerator generator elaborateScope classInfo instanceInfo instanceHeadViews methodDef
      case result of
        Left err -> pure (Left err)
        Right (lowered, generator') -> go generator' (lowered : acc) rest

checkInstances :: IdentityGenerator -> DisplayNameEnv -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedInstanceDecl] -> TcM ([CheckedBinding], IdentityGenerator)
checkInstances generator0 displayEnv finalizeContext elaborateScope scope instDecls = do
  (lowereds, generator1) <- lowerInstances generator0 [] instDecls
  liftEither (finalizeBindingsAllowOpaqueWithContextFromSupply generator1 finalizeContext lowereds)
  where
    lowerInstances generator acc [] =
      pure (concat (reverse acc), generator)
    lowerInstances generator acc (instDecl : rest) = do
      (lowereds, generator') <- lowerInstance generator displayEnv elaborateScope scope instDecl
      lowerInstances generator' (lowereds : acc) rest

lowerInstance :: IdentityGenerator -> DisplayNameEnv -> ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> TcM ([LoweredBinding], IdentityGenerator)
lowerInstance generator0 displayEnv elaborateScope scope instDecl = do
  (classInfo, instanceInfo) <- lookupInstanceForDecl displayEnv scope instDecl
  let go generator acc [] = pure (reverse acc, generator)
      go generator acc (methodDef : rest) = do
        (lowered, generator') <-
          lowerInstanceMethodWithGenerator generator elaborateScope classInfo instanceInfo (instanceHeadTypeViews instanceInfo) methodDef
        go generator' (lowered : acc) rest
  go generator0 [] (P.instanceDeclMethods instDecl)

lookupInstanceForDecl :: DisplayNameEnv -> Scope -> P.ResolvedInstanceDecl -> TcM (ClassInfo, InstanceInfo)
lookupInstanceForDecl displayEnv scope instDecl = do
  classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
  headViews <- mapM (typeViewForDisplayEnv displayEnv) (P.instanceDeclTypes instDecl)
  let headTys = typeViewsDisplay headViews
  instanceInfo <-
    case findInstance classInfo headViews of
      Just info -> pure info
      Nothing ->
        throwError $
          case headTys of
            headTy :| [] -> ProgramNoMatchingInstance (className classInfo) headTy
            tys -> ProgramNoMatchingInstanceHead (className classInfo) (NE.toList tys)
  pure (classInfo, instanceInfo)
  where
    findInstance classInfo headViews =
      find
        ( \info ->
            instanceClassIdentity info == classIdentity classInfo
              && instanceHeadTypeViews info == headViews
        )
        (scopeInstances scope)

lowerInstanceMethodWithGenerator :: IdentityGenerator -> ElaborateScope -> ClassInfo -> InstanceInfo -> NonEmpty TypeView -> P.ResolvedMethodDef -> TcM (LoweredBinding, IdentityGenerator)
lowerInstanceMethodWithGenerator generator elaborateScope classInfo instanceInfo instanceHeadViews methodDef =
  case lookupClassMethod (P.methodDefName methodDef) classInfo of
    Just methodInfo | Just valueInfo@OrdinaryValue {} <- lookupInstanceMethod methodInfo instanceInfo -> do
      let methodBodyView = specializeMethodTypeView methodInfo instanceHeadViews
          methodSourceView =
            typeViewWithBinderIdentityAliases
              (typeViewBinderIdentities methodBodyView)
              (ordinaryValueTypeView valueInfo)
      liftEither
        ( lowerConstrainedResolvedExprBindingWithGenerator
            generator
            elaborateScope
            (loweredBindingIdentityFromValueInfo valueInfo)
            (valueConstraintInfos valueInfo)
            methodSourceView
            methodBodyView
            False
            (P.methodDefExpr methodDef)
        )
    _ -> throwError (ProgramUnexpectedInstanceMethod (className classInfo) (P.refDisplayName (P.methodDefName methodDef)))

data DefWorkItem = DefWorkItem
  { defWorkItemDecl :: P.ResolvedDefDecl,
    defWorkItemIdentity :: SymbolIdentity,
    defWorkItemLowered :: LoweredBinding,
    defWorkItemDependencies :: [SymbolIdentity]
  }

checkDefsWithTiming :: TimingConfig -> P.ModuleName -> IdentityGenerator -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedDefDecl] -> IO (TcM ([CheckedBinding], IdentityGenerator))
checkDefsWithTiming timing moduleName0 generator0 finalizeContext elaborateScope scope defDecls = do
  workItemsResult <- lowerDefWorkItemsWithTiming timing moduleName0 generator0 elaborateScope scope defDecls
  case workItemsResult of
    Left err -> pure (Left err)
    Right (workItems, generator1) -> do
      batchSize <- moduleDefBatchSize
      nonRecursiveIdentitiesResult <-
        timeCheckModuleOperation timing moduleName0 "defs.scc_classification" $
          Right (nonRecursiveDefIdentities workItems)
      case nonRecursiveIdentitiesResult of
        Left err -> pure (Left err)
        Right nonRecursiveIdentities -> do
          moduleContextResult <-
            timeCheckModuleOperation timing moduleName0 "defs.module_finalize_context" $
              mkModuleFinalizeContext finalizeContext (map defWorkItemLowered workItems)
          case moduleContextResult of
            Left err -> pure (Left err)
            Right moduleContext ->
              finalizeDefWorkItemsWithTiming
                timing
                moduleName0
                generator1
                finalizeContext
                (Just moduleContext)
                batchSize
                nonRecursiveIdentities
                workItems

lowerDefWorkItemsWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  IdentityGenerator ->
  ElaborateScope ->
  Scope ->
  [P.ResolvedDefDecl] ->
  IO (TcM ([DefWorkItem], IdentityGenerator))
lowerDefWorkItemsWithTiming timing moduleName0 generator0 elaborateScope scope defDecls =
  go (Set.fromList (map (resolvedSymbolIdentity . P.defDeclName) defDecls)) generator0 [] defDecls
  where
    go _ generator acc [] = pure (Right (reverse acc, generator))
    go localDefIdentities generator acc (defDecl : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 ("def." ++ P.refDisplayName (P.defDeclName defDecl) ++ ".lower") $
          lowerDefWorkItem generator elaborateScope scope localDefIdentities defDecl
      case result of
        Left err -> pure (Left err)
        Right (workItem, generator') -> go localDefIdentities generator' (workItem : acc) rest

lowerDefWorkItem ::
  IdentityGenerator ->
  ElaborateScope ->
  Scope ->
  Set.Set SymbolIdentity ->
  P.ResolvedDefDecl ->
  TcM (DefWorkItem, IdentityGenerator)
lowerDefWorkItem generator elaborateScope scope localDefIdentities defDecl = do
  let defName = P.refDisplayName (P.defDeclName defDecl)
  valueInfo <- lookupValueInfoBySymbol scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      (lowered, generator') <-
        liftEither $
          lowerResolvedConstrainedExprBindingWithGenerator
            generator
            elaborateScope
            (loweredBindingIdentityFromValueInfo ordinary)
            (P.defDeclType defDecl)
            (resolvedDefDeclIsMain defDecl)
            (P.defDeclExpr defDecl)
      pure
        ( DefWorkItem
            { defWorkItemDecl = defDecl,
              defWorkItemIdentity = valueInfoSymbolIdentity ordinary,
              defWorkItemLowered = lowered,
              defWorkItemDependencies = localResolvedDefDependencies localDefIdentities (P.defDeclExpr defDecl)
            },
          generator'
        )
    _ -> throwError (ProgramDuplicateValue defName)

localResolvedDefDependencies :: Set.Set SymbolIdentity -> P.ResolvedExpr -> [SymbolIdentity]
localResolvedDefDependencies localDefIdentities expr =
  Set.toList (Set.fromList (collectFreeResolvedGlobalValues Set.empty expr) `Set.intersection` localDefIdentities)

collectFreeResolvedGlobalValues :: Set.Set LocalRef -> P.ResolvedExpr -> [SymbolIdentity]
collectFreeResolvedGlobalValues bound expr =
  case expr of
    P.EVar P.ResolvedLocalValue {} -> []
    P.EVar (P.ResolvedGlobalValue symbol) -> [resolvedSymbolIdentity symbol]
    P.ELit {} -> []
    P.ELam param body -> collectFreeResolvedGlobalValues (Set.insert (P.paramName param) bound) body
    P.EApp fun arg -> collectFreeResolvedGlobalValues bound fun ++ collectFreeResolvedGlobalValues bound arg
    P.ELet name _ rhs body ->
      collectFreeResolvedGlobalValues bound rhs
        ++ collectFreeResolvedGlobalValues (Set.insert name bound) body
    P.EAnn inner _ -> collectFreeResolvedGlobalValues bound inner
    P.ECase scrutinee alts -> collectFreeResolvedGlobalValues bound scrutinee ++ concatMap collectAlt alts
  where
    collectAlt (P.Alt pattern0 body) =
      collectFreeResolvedGlobalValues (bound `Set.union` Set.fromList (patternBinders pattern0)) body

    patternBinders = \case
      P.PatCtor _ patterns -> concatMap patternBinders patterns
      P.PatVar name -> [name]
      P.PatWildcard -> []
      P.PatAnn inner _ -> patternBinders inner

nonRecursiveDefIdentities :: [DefWorkItem] -> Set.Set SymbolIdentity
nonRecursiveDefIdentities workItems =
  Set.fromList
    [ defWorkItemIdentity workItem
    | AcyclicSCC workItem <- stronglyConnComp (map graphNode workItems)
    ]
  where
    graphNode workItem =
      ( workItem,
        defWorkItemIdentity workItem,
        defWorkItemDependencies workItem
      )

finalizeDefWorkItemsWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  IdentityGenerator ->
  FinalizeContext ->
  Maybe ModuleFinalizeContext ->
  Int ->
  Set.Set SymbolIdentity ->
  [DefWorkItem] ->
  IO (TcM ([CheckedBinding], IdentityGenerator))
finalizeDefWorkItemsWithTiming timing moduleName0 generator0 finalizeContext moduleContext batchSize nonRecursiveIdentities workItems
  | Just moduleContext0 <- moduleContext =
      finalizeDefWorkItemLayersWithTiming timing moduleName0 generator0 finalizeContext moduleContext0 batchSize nonRecursiveIdentities workItems
  | otherwise =
      go generator0 [] workItems
  where
    go generator acc [] = pure (Right (reverse acc, generator))
    go generator acc (workItem : rest) = do
      result <- finalizeDefWorkItemWithTimingFromSupply timing moduleName0 generator finalizeContext moduleContext nonRecursiveIdentities workItem
      case result of
        Left err -> pure (Left (annotateDefWorkItemPipelineError workItem err))
        Right (binding, generator') -> go generator' (binding : acc) rest

finalizeDefWorkItemLayersWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  IdentityGenerator ->
  FinalizeContext ->
  ModuleFinalizeContext ->
  Int ->
  Set.Set SymbolIdentity ->
  [DefWorkItem] ->
  IO (TcM ([CheckedBinding], IdentityGenerator))
finalizeDefWorkItemLayersWithTiming timing moduleName0 generator0 finalizeContext moduleContext batchSize nonRecursiveIdentities workItems = do
  let layers = nonRecursiveDefLayers batchSize nonRecursiveIdentities workItems
      layeredIdentities = Set.unions (map (Set.fromList . map defWorkItemIdentity) layers)
      fallbackItems =
        [ workItem
        | workItem <- workItems
        , defWorkItemIdentity workItem `Set.notMember` layeredIdentities
        ]
  layerResults <- finalizeLayers generator0 Map.empty (1 :: Int) layers
  case layerResults of
    Left err -> pure (Left err)
    Right (checkedByIdentity0, generator1) -> do
      fallbackResult <- finalizeFallbackItems generator1 checkedByIdentity0 fallbackItems
      pure $ do
        (checkedByIdentity, generator2) <- fallbackResult
        checked <-
          traverse
            ( \workItem ->
                case Map.lookup (defWorkItemIdentity workItem) checkedByIdentity of
                  Just binding -> Right binding
                  Nothing -> Left (ProgramPipelineError ("missing checked definition `" ++ defWorkItemName workItem ++ "`"))
            )
            workItems
        pure (checked, generator2)
  where
    finalizeLayers generator checkedByIdentity _ [] =
      pure (Right (checkedByIdentity, generator))
    finalizeLayers generator checkedByIdentity index (layer : rest) = do
      let layerOperation = "defs.layer_" ++ show index
          layerLabel = checkModuleOperationLabel moduleName0 layerOperation
      layerResult <-
        if length layer > 1 && all moduleLayerEligibleDefWorkItem layer
          then
            if timingProgramDefDetails timing
              then
                finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply
                  timing
                  layerLabel
                  generator
                  moduleContext
                  (map defWorkItemLowered layer)
              else
                timeProgramOperationIO timing layerLabel $
                  finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply
                    timing
                    layerLabel
                    generator
                    moduleContext
                    (map defWorkItemLowered layer)
          else
            finalizeLayerIndividually generator index layer
      case layerResult of
        Left err -> pure (Left err)
        Right (checkedLayer, generator') -> do
          let checkedByIdentity' =
                foldl'
                  ( \acc (workItem, checked) ->
                      Map.insert (defWorkItemIdentity workItem) checked acc
                  )
                  checkedByIdentity
                  (zip layer checkedLayer)
          finalizeLayers generator' checkedByIdentity' (index + 1) rest

    finalizeLayerIndividually generator _layerIndex layer =
      goLayer generator [] (1 :: Int) layer
      where
        goLayer generator1 acc _ [] = pure (Right (reverse acc, generator1))
        goLayer generator1 acc itemIndex (workItem : rest) = do
          result <-
            finalizeDefWorkItemWithTimingFromSupply
              timing
              moduleName0
              generator1
              finalizeContext
              (Just moduleContext)
              nonRecursiveIdentities
              workItem
          case result of
            Left err -> pure (Left (annotateDefWorkItemPipelineError workItem err))
            Right (checked, generator2) ->
              checked `seq` goLayer generator2 (checked : acc) (itemIndex + 1) rest

    finalizeFallbackItems generator checkedByIdentity [] =
      pure (Right (checkedByIdentity, generator))
    finalizeFallbackItems generator checkedByIdentity workItems0@(workItem : rest)
      | moduleDeferredLayerEligibleDefWorkItem nonRecursiveIdentities workItem = do
          let (deferredLayer, remaining) =
                splitContiguousEligibleBatch
                  batchSize
                  (moduleDeferredLayerEligibleDefWorkItem nonRecursiveIdentities)
                  workItems0
          if length deferredLayer <= 1
            then finalizeFallbackItem generator checkedByIdentity workItem rest
            else do
              layerResult <-
                finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply
                  timing
                  (checkModuleOperationLabel moduleName0 ("defs.deferred_layer_" ++ show (Map.size checkedByIdentity + 1)))
                  generator
                  moduleContext
                  (map defWorkItemLowered deferredLayer)
              case layerResult of
                Left err -> pure (Left err)
                Right (checkedLayer, generator') -> do
                  let checkedByIdentity' =
                        foldl'
                          ( \acc (workItem0, checked) ->
                              Map.insert (defWorkItemIdentity workItem0) checked acc
                          )
                          checkedByIdentity
                          (zip deferredLayer checkedLayer)
                  finalizeFallbackItems generator' checkedByIdentity' remaining
    finalizeFallbackItems generator checkedByIdentity (workItem : rest) =
      finalizeFallbackItem generator checkedByIdentity workItem rest

    finalizeFallbackItem generator checkedByIdentity workItem rest = do
      result <-
        finalizeDefWorkItemWithTimingFromSupply
          timing
          moduleName0
          generator
          finalizeContext
          (Just moduleContext)
          nonRecursiveIdentities
          workItem
      case result of
        Left err -> pure (Left (annotateDefWorkItemPipelineError workItem err))
        Right (checked, generator') ->
          finalizeFallbackItems generator' (Map.insert (defWorkItemIdentity workItem) checked checkedByIdentity) rest

defWorkItemName :: DefWorkItem -> String
defWorkItemName = P.refDisplayName . P.defDeclName . defWorkItemDecl

annotateDefWorkItemPipelineError :: DefWorkItem -> ProgramError -> ProgramError
annotateDefWorkItemPipelineError workItem err =
  case err of
    ProgramPipelineError message ->
      ProgramPipelineError
        ( "definition `"
            ++ defWorkItemName workItem
            ++ "`: "
            ++ message
        )
    _ -> err

moduleLayerEligibleDefWorkItem :: DefWorkItem -> Bool
moduleLayerEligibleDefWorkItem workItem =
  let lowered = defWorkItemLowered workItem
   in Map.null (loweredBindingDeferredObligations lowered)
        && not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))

moduleDeferredLayerEligibleDefWorkItem :: Set.Set SymbolIdentity -> DefWorkItem -> Bool
moduleDeferredLayerEligibleDefWorkItem nonRecursiveIdentities workItem =
  defWorkItemIdentity workItem `Set.member` nonRecursiveIdentities
    && let lowered = defWorkItemLowered workItem
        in not (Map.null (loweredBindingDeferredObligations lowered))
             && not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))

nonRecursiveDefLayers :: Int -> Set.Set SymbolIdentity -> [DefWorkItem] -> [[DefWorkItem]]
nonRecursiveDefLayers batchSize nonRecursiveIdentities workItems =
  concatMap (chunksOf batchSize) (dependencyLayers eligible)
  where
    -- Local references are still checked through the declared source types,
    -- as in the old per-def path.  Keep the SCC/layer classification in place,
    -- but only enable multi-root batches once the exact graph path is measured
    -- faster than the per-def module read-context path.
    eligible =
      [ workItem
      | workItem <- workItems
      , defWorkItemIdentity workItem `Set.member` nonRecursiveIdentities
      , moduleLayerEligibleDefWorkItem workItem
      ]
    eligibleIdentities = Set.fromList (map defWorkItemIdentity eligible)
    eligibleDependencies workItem =
      Set.fromList
        [ dep
        | dep <- defWorkItemDependencies workItem
        , dep `Set.member` eligibleIdentities
        ]

    dependencyLayers [] = []
    dependencyLayers remaining =
      let remainingIdentities = Set.fromList (map defWorkItemIdentity remaining)
          isReady workItem =
            Set.null (eligibleDependencies workItem `Set.intersection` remainingIdentities)
          (ready, blocked) = partition isReady remaining
       in case ready of
            [] -> [remaining]
            _ -> ready : dependencyLayers blocked

    chunksOf _ [] = []
    chunksOf n xs =
      let (chunk, rest) = splitAt n xs
       in chunk : chunksOf n rest

finalizeDefWorkItemWithTimingFromSupply ::
  TimingConfig ->
  P.ModuleName ->
  IdentityGenerator ->
  FinalizeContext ->
  Maybe ModuleFinalizeContext ->
  Set.Set SymbolIdentity ->
  DefWorkItem ->
  IO (TcM (CheckedBinding, IdentityGenerator))
finalizeDefWorkItemWithTimingFromSupply timing moduleName0 generator finalizeContext moduleContext nonRecursiveIdentities workItem =
  timeProgramOperationIO timing label $
    case moduleContext of
      Just moduleContext0
        | moduleContextEligibleDefWorkItem nonRecursiveIdentities workItem ->
            if timingProgramDefDetails timing
              then
                finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply
                  timing
                  label
                  generator
                  moduleContext0
                  False
                  lowered
              else
                finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply
                  timing
                  label
                  generator
                  moduleContext0
                  False
                  lowered
      _ ->
        if timingProgramDefDetails timing
          then
            finalizeBindingAllowOpaqueWithContextWithTimingFromSupply
              timing
              label
              generator
              finalizeContext
              lowered
          else
            evaluate (finalizeBindingAllowOpaqueWithContextFromSupply generator finalizeContext lowered)
  where
    defName = defWorkItemName workItem
    lowered = defWorkItemLowered workItem
    label = checkModuleOperationLabel moduleName0 ("def." ++ defName)

moduleDefBatchSize :: IO Int
moduleDefBatchSize = do
  mbValue <- lookupEnv "MLF_MODULE_DEF_BATCH_SIZE"
  pure $
    case mbValue >>= readMaybe of
      Just n | n > 0 -> n
      _ -> 16

-- | Take one bounded batch from the eligible prefix without dropping the
-- first ineligible item or anything after it.  Fallback work mixes deferred
-- non-recursive definitions with recursive/unsupported definitions, so the
-- untouched suffix must remain in the work queue.
splitContiguousEligibleBatch :: Int -> (a -> Bool) -> [a] -> ([a], [a])
splitContiguousEligibleBatch batchSize eligible items =
  let (eligiblePrefix, suffix) = span eligible items
      (batch, eligibleRemainder) = splitAt batchSize eligiblePrefix
   in (batch, eligibleRemainder ++ suffix)

moduleContextEligibleDefWorkItem :: Set.Set SymbolIdentity -> DefWorkItem -> Bool
moduleContextEligibleDefWorkItem nonRecursiveIdentities workItem =
  defWorkItemIdentity workItem `Set.member` nonRecursiveIdentities

moduleDefDecls :: P.ModuleF p -> [P.DefDeclF p]
moduleDefDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclDef defDecl -> defDecl : acc
      _ -> acc

explicitInstances :: P.ModuleF p -> [P.InstanceDeclF p]
explicitInstances = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclInstance instDecl -> instDecl : acc
      _ -> acc

moduleDataDecls :: P.ModuleF p -> [P.DataDeclF p]
moduleDataDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclData dataDecl -> dataDecl : acc
      _ -> acc

moduleClassDecls :: P.ModuleF p -> [P.ClassDeclF p]
moduleClassDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclClass classDecl -> classDecl : acc
      _ -> acc

moduleTypeFamilyDecls :: P.ModuleF p -> [TypeFamilyDecl]
moduleTypeFamilyDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclTypeFamily familyDecl -> familyDecl : acc
      _ -> acc

rejectUnsupportedTypeFamilies :: P.ModuleF p -> TcM ()
rejectUnsupportedTypeFamilies mod0 =
  case moduleTypeFamilyDecls mod0 of
    [] -> pure ()
    familyDecl : _ -> throwError (ProgramUnsupportedTypeFamily (familyDeclName familyDecl))

rejectUnsupportedGeneralizedClassFeatures :: P.Program -> TcM ()
rejectUnsupportedGeneralizedClassFeatures (P.Program modules0) =
  mapM_ (rejectUnsupportedGeneralizedClassFeaturesModule id id) modules0

rejectUnsupportedGeneralizedClassFeaturesModule :: (P.ClassRef p -> P.ClassName) -> (P.ProgramSrcType p -> SrcType) -> P.ModuleF p -> TcM ()
rejectUnsupportedGeneralizedClassFeaturesModule _renderClass _renderTy mod0 =
  mapM_ rejectDecl (P.moduleDecls mod0)
  where
    rejectDecl :: P.DeclF p -> TcM ()
    rejectDecl decl =
      case decl of
        P.DeclClass classDecl -> rejectClassDecl classDecl
        P.DeclInstance instDecl -> rejectInstanceDecl instDecl
        P.DeclDef defDecl -> rejectConstrainedType (P.defDeclType defDecl)
        P.DeclData {} -> pure ()
        P.DeclTypeFamily {} -> pure ()

    rejectClassDecl :: P.ClassDeclF p -> TcM ()
    rejectClassDecl classDecl = do
      mapM_ (rejectConstrainedType . P.methodSigType) (P.classDeclMethods classDecl)

    rejectInstanceDecl :: P.InstanceDeclF p -> TcM ()
    rejectInstanceDecl instDecl = do
      mapM_ rejectConstraint (P.instanceDeclConstraints instDecl)

    rejectConstrainedType :: P.ConstrainedTypeF p -> TcM ()
    rejectConstrainedType constrained =
      mapM_ rejectConstraint (P.constrainedConstraints constrained)

    rejectConstraint :: P.ClassConstraintF p -> TcM ()
    rejectConstraint _constraint =
      pure ()

buildImportScopeResolved :: Map SymbolIdentity ModuleExports -> [P.ResolvedImport] -> TcM Scope
buildImportScopeResolved priorExports imports0 = foldM go emptyScope imports0
  where
    go scope imp = do
      let moduleName0 = resolvedImportDefiningModule imp
          moduleIdentity = resolvedImportModuleIdentity imp
      exports <-
        case Map.lookup moduleIdentity priorExports of
          Nothing -> throwError (ProgramUnknownImportModule moduleName0)
          Just ex -> pure ex
      case P.importAlias imp of
        Nothing ->
          case P.importExposing imp of
            Nothing -> addAllExports scope exports
            Just items -> foldM (applyResolvedImportItem moduleName0 exports) scope items
        Just alias -> do
          qualifiedScope <- addAllExports scope (qualifyModuleExports alias exports)
          case P.importExposing imp of
            Nothing -> pure qualifiedScope
            Just items -> foldM (applyResolvedImportItem moduleName0 exports) qualifiedScope items

resolvedImportDefiningModule :: P.ResolvedImport -> P.ModuleName
resolvedImportDefiningModule =
  symbolDefiningModule . resolvedSymbolIdentity . P.importModuleName

resolvedImportModuleIdentity :: P.ResolvedImport -> SymbolIdentity
resolvedImportModuleIdentity =
  resolvedSymbolIdentity . P.importModuleName

addAllExports :: Scope -> ModuleExports -> TcM Scope
addAllExports scope exports = do
  let (scopeWithOwners, importedValues) = prepareBulkImportedValues exports scope (exportedValuesForDisplay exports)
  liftEither $ do
    values <- addValues (scopeValues scopeWithOwners) importedValues
    types <- addTypes (scopeTypes scopeWithOwners) (Map.map exportedTypeData (exportedTypesForDisplay exports))
    classes <- addClasses (scopeClasses scopeWithOwners) (exportedClassesForDisplay exports)
    pure (mkScopeWithHidden values types (scopeHiddenTypes scopeWithOwners) classes (scopeInstances scopeWithOwners))

prepareBulkImportedValues :: ModuleExports -> Scope -> Map String ValueInfo -> (Scope, Map String ValueInfo)
prepareBulkImportedValues exports =
  Map.mapAccumWithKey (\scope0 _ valueInfo -> prepareBulkImportedValue exports scope0 valueInfo)

prepareBulkImportedValue :: ModuleExports -> Scope -> ValueInfo -> (Scope, ValueInfo)
prepareBulkImportedValue exports scope valueInfo@ConstructorValue {valueCtorInfo = ctorInfo}
  | constructorOwnerVisibleInExports ctorInfo exports = (scope, valueInfo)
  | otherwise = prepareImportedValue exports scope valueInfo
prepareBulkImportedValue _ scope valueInfo = (scope, valueInfo)

constructorOwnerVisibleInExports :: ConstructorInfo -> ModuleExports -> Bool
constructorOwnerVisibleInExports ctorInfo exports =
  case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctorInfo) (exportedTypesByIdentity exports) of
    Just _ -> True
    Nothing -> False

qualifyModuleExports :: P.ModuleName -> ModuleExports -> ModuleExports
qualifyModuleExports alias exports =
  moduleExportsFromMaps qualifiedValues qualifiedTypes qualifiedClasses
  where
    qualifiedName name = alias ++ "." ++ name
    qualifiedTypes =
      Map.fromList
        [ let qualifiedDataInfo = qualifyDataInfo dataInfo
              qualifiedCtorsByIdentity =
                Map.fromList [(ctorInfoSymbol ctor, ctor) | ctor <- dataConstructors qualifiedDataInfo]
              qualifiedCtors =
                [ (qualifiedName sourceName, qualifiedCtor)
                  | (sourceName, ctor) <- Map.toList (exportedTypeConstructorsForDisplay typeInfo),
                    Just _ <- [lookupSymbolIdentityExact (ctorInfoSymbol ctor) (exportedTypeConstructorsByIdentity typeInfo)],
                    Just qualifiedCtor <- [lookupSymbolIdentityExact (ctorInfoSymbol ctor) qualifiedCtorsByIdentity]
                ]
           in ( qualifiedName typeName,
                mkExportedTypeInfo qualifiedDataInfo qualifiedCtors
              )
          | (typeName, typeInfo) <- Map.toList (exportedTypesForDisplay exports),
            let dataInfo = exportedTypeData typeInfo
        ]

    qualifiedClasses =
      Map.fromList
        [ (qualifiedName className0, qualifyClassInfo classInfo)
          | (className0, classInfo) <- Map.toList (exportedClassesForDisplay exports)
        ]

    qualifiedCtorValues =
      Map.fromList
        [ ( sourceName,
            ConstructorValue
              { valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
                valueRuntimeName = constructorInfoRuntimeName ctor,
                valueCtorInfo = ctor
              }
          )
          | typeInfo <- Map.elems qualifiedTypes,
            let dataInfo = exportedTypeData typeInfo,
            (sourceName, ctor) <- Map.toList (exportedTypeConstructorsForDisplay typeInfo)
        ]

    qualifiedExportedValues =
      Map.fromList
        [ ( qualifiedName name,
            qualifyValueInfo valueInfo
          )
          | (name, valueInfo) <- Map.toList (exportedValuesForDisplay exports)
        ]

    qualifiedValues = qualifiedCtorValues `Map.union` qualifiedExportedValues

    qualifyDataInfo dataInfo =
      let qualifyCtor ctor = qualifyConstructorInfo ctor
       in dataInfo
            { dataConstructors = map qualifyCtor (dataConstructors dataInfo)
            }

    qualifyConstructorInfo ctor =
      ctor
        { ctorTypeView =
            qualifyTypeView (ctorTypeView ctor),
          ctorOwningTypeIdentity = ctorOwningTypeIdentity ctor,
          ctorOwnerConstructors = map qualifyConstructorShape (ctorOwnerConstructors ctor)
        }

    qualifyConstructorShape shape =
      shape
        { constructorShapeTypeView =
            qualifyTypeView (constructorShapeTypeView shape)
        }

    qualifyClassInfo classInfo =
      let qualifyConstraintInfo constraintInfo =
            constraintInfo
              { constraintDisplayClass =
                  qualifiedClassNameFor
                    (constraintClassSymbol constraintInfo)
                    (constraintDisplayClass constraintInfo),
                constraintTypeViews = fmap qualifyTypeView (constraintTypeViews constraintInfo)
              }
          qualifyMethod methodInfo =
            methodInfo
              { methodTypeViewRaw = qualifyTypeView (methodTypeViewRaw methodInfo),
                methodConstraintInfos = map qualifyConstraintInfo (methodConstraintInfos methodInfo)
              }
          qualifiedMethodsByIdentity = Map.map qualifyMethod (classMethodsByIdentity classInfo)
       in classInfo
            { classSuperclassInfos = map qualifyConstraintInfo (classSuperclassInfos classInfo),
              classMethodsByIdentity = qualifiedMethodsByIdentity
            }

    qualifyValueInfo valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          valueInfo
            { valueTypeView =
                qualifyTypeView (valueTypeView valueInfo),
              valueConstraintInfos = map qualifyConstraintInfoFromExport (valueConstraintInfos valueInfo)
            }
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          OverloadedMethod
            { valueInfoSymbol = valueInfoSymbolIdentity valueInfo,
              valueMethodInfo = qualifyMethodFromExport methodInfo
            }
        ConstructorValue {valueCtorInfo = ctorInfo} ->
          let qualifiedCtorInfo = qualifyConstructorInfo ctorInfo
           in ConstructorValue
                { valueInfoSymbol = valueInfoSymbolIdentity valueInfo,
                  valueRuntimeName = valueInfoRuntimeName valueInfo,
                  valueCtorInfo = qualifiedCtorInfo
                }

    qualifyMethodFromExport methodInfo =
      methodInfo
        { methodTypeViewRaw =
            qualifyTypeView (methodTypeViewRaw methodInfo),
          methodConstraintInfos = map qualifyConstraintInfoFromExport (methodConstraintInfos methodInfo)
        }

    qualifyConstraintInfoFromExport constraintInfo =
      constraintInfo
        { constraintDisplayClass =
            qualifiedClassNameFor
              (constraintClassSymbol constraintInfo)
              (constraintDisplayClass constraintInfo),
          constraintTypeViews = fmap qualifyTypeViewFromExport (constraintTypeViews constraintInfo)
        }

    qualifyTypeViewFromExport view =
      qualifyTypeView view

    qualifyTypeView =
      mapTypeViewDisplayHeadNames qualifyHead

    qualifyHead identity name
      | Just exportedName <- lookupSymbolIdentityExact identity (exportedTypeDisplaysByIdentity exports) =
          qualifiedName exportedName
      | otherwise = name

    qualifiedClassNameFor identity className0
      | Just exportedName <- lookupSymbolIdentityExact identity (exportedClassDisplaysByIdentity exports) =
          qualifiedName exportedName
      | otherwise = className0

classIdentity :: ClassInfo -> ClassIdentity
classIdentity = classInfoSymbolIdentity

methodClassIdentity :: ValueInfo -> Maybe ClassIdentity
methodClassIdentity valueInfo =
  case valueInfo of
    OverloadedMethod {valueMethodInfo = methodInfo} ->
      Just (methodInfoOwnerClassSymbolIdentity methodInfo)
    _ -> Nothing

resolvedMethodOwnerClassIdentity :: ResolvedSymbol -> Maybe ClassIdentity
resolvedMethodOwnerClassIdentity symbol =
  case symbolOwnerIdentity (resolvedSymbolIdentity symbol) of
    Just (SymbolOwnerClass ownerClassIdentity) ->
      Just ownerClassIdentity
    _ -> Nothing

instanceClassIdentity :: InstanceInfo -> ClassIdentity
instanceClassIdentity = instanceInfoClassSymbolIdentity

visibleInstancesForImports ::
  Map SymbolIdentity ModuleExports ->
  Map SymbolIdentity (Map SymbolIdentity DataInfo) ->
  [InstanceInfo] ->
  Set.Set ClassIdentity ->
  [P.ResolvedImport] ->
  [InstanceInfo]
visibleInstancesForImports priorExports priorData priorInstances unqualifiedClassIdentities =
  distinctInstanceHeads . concatMap instancesForImport
  where
    instancesForImport imp =
      unqualifiedInstancesForImport priorExports priorData priorInstances unqualifiedClassIdentities imp
        ++ qualifiedInstancesForImport priorExports priorData priorInstances unqualifiedClassIdentities imp

unqualifiedInstancesForImport ::
  Map SymbolIdentity ModuleExports ->
  Map SymbolIdentity (Map SymbolIdentity DataInfo) ->
  [InstanceInfo] ->
  Set.Set ClassIdentity ->
  P.ResolvedImport ->
  [InstanceInfo]
unqualifiedInstancesForImport priorExports priorData priorInstances unqualifiedClassIdentities imp =
  case Map.lookup moduleIdentity priorExports of
    Nothing -> []
    Just exports ->
      let importClassIdentities = importUnqualifiedClassIdentitiesFor exports imp
          unqualifiedTypeIdentities = importUnqualifiedTypeIdentities exports imp
       in [ instanceInfo
            | instanceInfo <- priorInstances,
              instanceBelongsToModule moduleIdentity instanceInfo,
              instanceVisibleForUnqualifiedImport priorData unqualifiedClassIdentities importClassIdentities unqualifiedTypeIdentities instanceInfo
          ]
  where
    moduleIdentity = resolvedImportModuleIdentity imp

qualifiedInstancesForImport ::
  Map SymbolIdentity ModuleExports ->
  Map SymbolIdentity (Map SymbolIdentity DataInfo) ->
  [InstanceInfo] ->
  Set.Set ClassIdentity ->
  P.ResolvedImport ->
  [InstanceInfo]
qualifiedInstancesForImport priorExports priorData priorInstances _unqualifiedClassIdentities imp =
  case P.importAlias imp of
    Nothing -> []
    Just _alias ->
      case Map.lookup moduleIdentity priorExports of
        Nothing -> []
        Just exports ->
          [ instanceInfo
            | instanceInfo <- priorInstances,
              instanceBelongsToModule moduleIdentity instanceInfo,
              instanceVisibleForQualifiedImport priorData exports instanceInfo
          ]
  where
    moduleIdentity = resolvedImportModuleIdentity imp

importedUnqualifiedClassIdentities ::
  Map SymbolIdentity ModuleExports ->
  [P.ResolvedImport] ->
  Set.Set ClassIdentity
importedUnqualifiedClassIdentities priorExports =
  Set.unions . map importUnqualifiedClassIdentities
  where
    importUnqualifiedClassIdentities imp =
      case Map.lookup (resolvedImportModuleIdentity imp) priorExports of
        Nothing -> Set.empty
        Just exports -> importUnqualifiedClassIdentitiesFor exports imp

importUnqualifiedClassIdentitiesFor :: ModuleExports -> P.ResolvedImport -> Set.Set ClassIdentity
importUnqualifiedClassIdentitiesFor exports imp =
  case (P.importAlias imp, P.importExposing imp) of
    (Nothing, Nothing) ->
      Map.keysSet (exportedClassesByIdentity exports)
        `Set.union` overloadedMethodClassIdentities (Map.elems (exportedValuesByIdentity exports))
    (_, Just items) -> Set.unions (map importItemClassIdentities items)
    (Just _, Nothing) -> Set.empty
  where
    importItemClassIdentities item =
      case item of
        P.ExportType ref ->
          case exportedClassByRef ref exports of
            Just (_, classInfo) -> classDependencyIdentityClosure exports (classInfoSymbolIdentity classInfo)
            Nothing -> Set.empty
        P.ExportValue symbol ->
          case exportedValueByIdentity (resolvedSymbolIdentity symbol) exports of
            Just (_, valueInfo) -> importedValueClassDependencyIdentities exports valueInfo
            Nothing -> Set.fromList (maybe [] (: []) (resolvedMethodOwnerClassIdentity symbol))
        _ -> Set.empty

    overloadedMethodClassIdentities =
      Set.fromList . mapMaybe methodClassIdentity

importedValueClassDependencyIdentities :: ModuleExports -> ValueInfo -> Set.Set ClassIdentity
importedValueClassDependencyIdentities exports valueInfo =
  case valueInfo of
    OverloadedMethod {valueMethodInfo = methodInfo} ->
      classDependencyIdentityClosure exports (methodInfoOwnerClassSymbolIdentity methodInfo)
    _ -> Set.empty

classDependencyIdentityClosure :: ModuleExports -> ClassIdentity -> Set.Set ClassIdentity
classDependencyIdentityClosure exports = go Set.empty
  where
    go seen identity
      | identity `Set.member` seen = seen
      | otherwise =
          case exportedClassByIdentity identity exports of
            Nothing -> Set.insert identity seen
            Just (_, classInfo) ->
              foldl'
                go
                (Set.insert identity seen)
                [ constraintClassSymbol constraint
                  | constraint <- classSuperclassInfos classInfo
                ]

importUnqualifiedTypeIdentities :: ModuleExports -> P.ResolvedImport -> Set.Set SymbolIdentity
importUnqualifiedTypeIdentities exports imp =
  case (P.importAlias imp, P.importExposing imp) of
    (Nothing, Nothing) -> exportedTypeIdentities exports
    (_, Just items) -> importExposedTypeIdentities items
    (Just _, Nothing) -> Set.empty

importExposedTypeIdentities :: [P.ResolvedExportItem] -> Set.Set SymbolIdentity
importExposedTypeIdentities items =
  Set.fromList (concatMap exposedTypeIdentity items)
  where
    exposedTypeIdentity item =
      case item of
        P.ExportType ref -> resolvedExportTypeIdentities ref
        P.ExportTypeWithConstructors ref -> resolvedExportTypeIdentities ref
        P.ExportValue {} -> []

    resolvedExportTypeIdentities ref =
      [ identity
      | symbol <- P.resolvedExportTypeSymbols ref,
        let identity = resolvedSymbolIdentity symbol,
        symbolNamespace identity == SymbolType
      ]

exportedTypeIdentities :: ModuleExports -> Set.Set SymbolIdentity
exportedTypeIdentities = Map.keysSet . exportedTypesByIdentity

instanceVisibleForUnqualifiedImport ::
  Map SymbolIdentity (Map SymbolIdentity DataInfo) ->
  Set.Set ClassIdentity ->
  Set.Set ClassIdentity ->
  Set.Set SymbolIdentity ->
  InstanceInfo ->
  Bool
instanceVisibleForUnqualifiedImport priorData unqualifiedClassIdentities importClassIdentities unqualifiedTypeIdentities instanceInfo =
  (classVisibleGlobally && originDataVisible && not (Set.null originDataMentions))
    || (classVisibleThroughImport && Set.null originDataMentions)
  where
    identity = instanceClassIdentity instanceInfo
    classVisibleGlobally = identity `Set.member` unqualifiedClassIdentities
    classVisibleThroughImport = identity `Set.member` importClassIdentities
    originDataMentions = instanceOriginDataMentions priorData instanceInfo
    originDataVisible = originDataMentions `Set.isSubsetOf` unqualifiedTypeIdentities

instanceVisibleForQualifiedImport :: Map SymbolIdentity (Map SymbolIdentity DataInfo) -> ModuleExports -> InstanceInfo -> Bool
instanceVisibleForQualifiedImport priorData exports instanceInfo =
  originDataVisible
    && ( instanceClassIdentity instanceInfo `Set.member` exportedClassIdentities
           || any (not . Set.null . typeViewMentionedDataIdentities exportedDataByIdentity) (instanceHeadTypeViews instanceInfo)
       )
  where
    exportedDataByIdentity =
      Map.map exportedTypeData (exportedTypesByIdentity exports)
    exportedTypeIdentities0 = exportedTypeIdentities exports
    exportedClassIdentities = Map.keysSet (exportedClassesByIdentity exports)
    originDataVisible = instanceOriginDataMentions priorData instanceInfo `Set.isSubsetOf` exportedTypeIdentities0

distinctInstanceHeads :: [InstanceInfo] -> [InstanceInfo]
distinctInstanceHeads = reverse . foldl' add []
  where
    add acc instanceInfo
      | any (sameInstanceHead instanceInfo) acc = acc
      | otherwise = instanceInfo : acc

sameInstanceHead :: InstanceInfo -> InstanceInfo -> Bool
sameInstanceHead left right =
  instanceClassIdentity left == instanceClassIdentity right
    && instanceHeadTypeViews left == instanceHeadTypeViews right

instanceExportedTypeMentions :: Map SymbolIdentity DataInfo -> InstanceInfo -> Set.Set SymbolIdentity
instanceExportedTypeMentions dataByIdentity instanceInfo =
  Set.unions (headMentions : constraintMentions ++ methodMentions)
  where
    headMentions = foldMap (typeViewMentionedDataIdentities dataByIdentity) (instanceHeadTypeViews instanceInfo)
    constraintMentions =
      concatMap
        (map (typeViewMentionedDataIdentities dataByIdentity) . NE.toList . constraintTypeViews)
        (instanceConstraintInfos instanceInfo)
    methodMentions = concatMap valueExportedTypeMentions (Map.elems (instanceMethodsByIdentity instanceInfo))

    valueExportedTypeMentions valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          typeViewMentionedDataIdentities dataByIdentity (ordinaryValueTypeView valueInfo)
            : concatMap
              (map (typeViewMentionedDataIdentities dataByIdentity) . NE.toList . constraintTypeViews)
              (valueConstraintInfos valueInfo)
        _ -> []

instanceOriginDataMentions :: Map SymbolIdentity (Map SymbolIdentity DataInfo) -> InstanceInfo -> Set.Set SymbolIdentity
instanceOriginDataMentions priorData instanceInfo =
  case Map.lookup (instanceOriginModuleIdentity instanceInfo) priorData of
    Nothing -> Set.empty
    Just dataInfos ->
      instanceExportedTypeMentions dataInfos instanceInfo

typeViewMentionedDataIdentities :: Map SymbolIdentity DataInfo -> TypeView -> Set.Set SymbolIdentity
typeViewMentionedDataIdentities dataByIdentity view =
  Set.filter (`Map.member` dataByIdentity) (typeViewMentionedHeadIdentities view)

instanceBelongsToModule :: SymbolIdentity -> InstanceInfo -> Bool
instanceBelongsToModule moduleIdentity instanceInfo =
  instanceOriginModuleIdentity instanceInfo == moduleIdentity

applyResolvedImportItem :: P.ModuleName -> ModuleExports -> Scope -> P.ResolvedExportItem -> TcM Scope
applyResolvedImportItem moduleName0 exports scope item =
  case item of
    P.ExportValue symbol ->
      case exportedValueByIdentity (resolvedSymbolIdentity symbol) exports of
        Just (name, info) -> do
          let (scopeWithOwner, importedInfo) = prepareImportedValue exports scope info
          classes <- liftEither (addClasses (scopeClasses scopeWithOwner) (importedValueClassDependencies exports importedInfo))
          values <- liftEither (addValues (scopeValues scopeWithOwner) (Map.singleton name importedInfo))
          pure (withScopeValues values (withScopeClasses classes scopeWithOwner))
        Nothing -> throwError (ProgramImportNotExported moduleName0 (resolvedSymbolDisplayName symbol))
    P.ExportType ref ->
      case exportedTypeByRef ref exports of
        Just (typeName, typeInfo) -> do
          let dataInfo = exportedTypeData typeInfo
          types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
          let scope' = withScopeTypes types scope
          case exportedClassByRef ref exports of
            Just (_, classInfo) -> do
              classes <- liftEither (addClasses (scopeClasses scope') (classDependencyClosure exports (classInfoSymbolIdentity classInfo)))
              pure (withScopeClasses classes scope')
            Nothing -> pure scope'
        Nothing ->
          case exportedClassByRef ref exports of
            Just (_, classInfo) -> do
              classes <- liftEither (addClasses (scopeClasses scope) (classDependencyClosure exports (classInfoSymbolIdentity classInfo)))
              pure (withScopeClasses classes scope)
            Nothing -> throwError (ProgramImportNotExported moduleName0 (P.resolvedExportTypeName ref))
    P.ExportTypeWithConstructors ref ->
      case exportedTypeByRef ref exports of
        Just (typeName, typeInfo) -> do
          when (Map.null (exportedTypeConstructorsByIdentity typeInfo)) $
            throwError (ProgramImportNotExported moduleName0 typeName)
          let dataInfo = exportedTypeData typeInfo
              ctorValues =
                Map.fromList
                  [ ( ctorName ctor,
                      ConstructorValue
                        { valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
                          valueRuntimeName = constructorInfoRuntimeName ctor,
                          valueCtorInfo = ctor
                        }
                    )
                    | ctor <- Map.elems (exportedTypeConstructorsByIdentity typeInfo)
                  ]
          values <- liftEither (addValues (scopeValues scope) ctorValues)
          types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
          pure (mkScopeWithHidden values types (scopeHiddenTypes scope) (scopeClasses scope) (scopeInstances scope))
        Nothing -> throwError (ProgramImportNotExported moduleName0 (P.resolvedExportTypeName ref))

prepareImportedValue :: ModuleExports -> Scope -> ValueInfo -> (Scope, ValueInfo)
prepareImportedValue exports scope valueInfo =
  case valueInfo of
    OrdinaryValue {valueTypeView = view} ->
      ( scope,
        valueInfo
          { valueTypeView =
              typeViewMergeHeadIdentityAliases
                (importedValueTypeHeadIdentities exports view)
                view
          }
      )
    ConstructorValue {valueCtorInfo = ctorInfo} ->
      case exportedConstructorOwnerType ctorInfo exports of
        Just dataInfo ->
          let hiddenDataInfo = hiddenOwnerDataInfo dataInfo
              hiddenTypes = Map.insert (hiddenOwnerTypeName dataInfo) hiddenDataInfo (scopeHiddenTypes scope)
              hiddenCtorInfo = importedHiddenConstructorInfo ctorInfo hiddenDataInfo
              importedInfo =
                valueInfo
                  { valueCtorInfo = hiddenCtorInfo
                  }
           in (withScopeHiddenTypes hiddenTypes scope, importedInfo)
        Nothing -> (scope, valueInfo)
    _ -> (scope, valueInfo)

importedValueTypeHeadIdentities :: ModuleExports -> TypeView -> Map String SymbolIdentity
importedValueTypeHeadIdentities exports view =
  symbolIdentityAliasMap
    [ identity
    | identity <- Map.keys (exportedTypesByIdentity exports),
      identity `Set.member` mentionedHeadIdentities
    ]
  where
    mentionedHeadIdentities =
      typeViewMentionedHeadIdentities view

importedValueClassDependencies :: ModuleExports -> ValueInfo -> Map String ClassInfo
importedValueClassDependencies exports valueInfo =
  case valueInfo of
    OverloadedMethod {valueMethodInfo = methodInfo} ->
      classDependencyClosure exports (methodInfoOwnerClassSymbolIdentity methodInfo)
    _ -> Map.empty

classDependencyClosure :: ModuleExports -> ClassIdentity -> Map String ClassInfo
classDependencyClosure exports identity =
  go Set.empty identity Map.empty
  where
    go seen classIdentity0 acc
      | classIdentity0 `Set.member` seen = acc
      | otherwise =
          case exportedClassByIdentity classIdentity0 exports of
            Nothing -> acc
            Just (className0, classInfo) ->
              foldl'
                (\acc0 superclass -> go (Set.insert classIdentity0 seen) (constraintClassSymbol superclass) acc0)
                (Map.insert className0 classInfo acc)
                (classSuperclassInfos classInfo)

hiddenOwnerDataInfo :: DataInfo -> DataInfo
hiddenOwnerDataInfo dataInfo =
  let hiddenName = hiddenOwnerTypeName dataInfo
      ownerParams = dataTypeParams dataInfo
      ownerParamBinderIdentities = typeBinderAliasIdentityMap (dataParamBinders dataInfo)
      ownerNames =
        Set.fromList
          [ dataInfoIdentityName dataInfo,
            dataInfoIdentityQualifiedName dataInfo
          ]
      ownerIdentity = dataInfoSymbolIdentity dataInfo
      rewriteTypeView _sourceTy view =
        typeViewMergeBinderIdentityAliases ownerParamBinderIdentities $
          mapTypeViewDisplayHeadNames rewriteHead view
      rewriteHead identity name
        | sameSymbolIdentity ownerIdentity identity = hiddenName
        | name `Set.member` ownerNames = hiddenName
        | otherwise = name
      rewriteShape shape =
        shape
          { constructorShapeTypeView =
              rewriteTypeView (constructorShapeType shape) (constructorShapeTypeView shape),
            constructorShapeOwnerTypeParams =
              if null (constructorShapeOwnerTypeParams shape)
                then ownerParams
                else constructorShapeOwnerTypeParams shape
          }
      rewriteCtor ctor =
        ctor
          { ctorTypeView =
              rewriteTypeView (ctorType ctor) (ctorTypeView ctor),
            ctorOwnerConstructors = map rewriteShape (constructorOwnerShapes ctor)
          }
   in dataInfo
        { dataConstructors = map rewriteCtor (dataConstructors dataInfo)
        }

hiddenOwnerTypeName :: DataInfo -> String
hiddenOwnerTypeName dataInfo =
  let identity = dataInfoSymbolIdentity dataInfo
   in "$" ++ symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

importedHiddenConstructorInfo :: ConstructorInfo -> DataInfo -> ConstructorInfo
importedHiddenConstructorInfo ctorInfo hiddenDataInfo =
  case find (sameSymbolIdentity (ctorInfoSymbol ctorInfo) . ctorInfoSymbol) (dataConstructors hiddenDataInfo) of
    Just hiddenCtorInfo ->
      hiddenCtorInfo
        { ctorRuntimeName = constructorInfoRuntimeName ctorInfo
        }
    Nothing -> ctorInfo

exportedConstructorOwnerType :: ConstructorInfo -> ModuleExports -> Maybe DataInfo
exportedConstructorOwnerType ctorInfo exports =
  case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctorInfo) (exportedTypesByIdentity exports) of
    Just typeInfo -> Just (exportedTypeData typeInfo)
    Nothing -> Just (constructorOwnerDataInfoFromShapes ctorInfo)

exportedValueByIdentity :: SymbolIdentity -> ModuleExports -> Maybe (String, ValueInfo)
exportedValueByIdentity identity exports =
  (,) <$> lookupSymbolIdentityExact identity (exportedValueDisplaysByIdentity exports) <*> lookupSymbolIdentityExact identity (exportedValuesByIdentity exports)

exportedMainIdentity :: P.ResolvedModuleSyntax -> ModuleExports -> Maybe SymbolIdentity
exportedMainIdentity mod0 exports = do
  mainIdentity <- moduleMainDefinitionIdentity mod0
  (_, OrdinaryValue {}) <- exportedValueByIdentity mainIdentity exports
  pure mainIdentity

moduleMainDefinitionIdentity :: P.ResolvedModuleSyntax -> Maybe SymbolIdentity
moduleMainDefinitionIdentity mod0 =
  case
    [ resolvedSymbolIdentity (P.defDeclName defDecl)
    | defDecl <- moduleDefDecls mod0
    , resolvedDefDeclIsMain defDecl
    ]
  of
    [identity] -> Just identity
    _ -> Nothing

resolvedDefDeclIsMain :: P.ResolvedDefDecl -> Bool
resolvedDefDeclIsMain defDecl =
  let identity = resolvedSymbolIdentity (P.defDeclName defDecl)
   in symbolNamespace identity == SymbolValue
        && symbolDefiningName identity == "main"
        && symbolOwnerIdentity identity == Nothing

checkedBindingValueIdentity :: CheckedBinding -> Maybe SymbolIdentity
checkedBindingValueIdentity binding =
  case resolvedVarDetails (checkedBindingResolvedVar binding) of
    TopLevelId identity -> Just identity
    _ -> Nothing

exportedTypeByRef :: P.ResolvedExportTypeRef -> ModuleExports -> Maybe (String, ExportedTypeInfo)
exportedTypeByRef ref exports =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolType] of
    symbol : _ -> exportedTypeByIdentity (resolvedSymbolIdentity symbol) exports
    [] -> Nothing

exportedClassByRef :: P.ResolvedExportTypeRef -> ModuleExports -> Maybe (String, ClassInfo)
exportedClassByRef ref exports =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolClass] of
    symbol : _ -> exportedClassByIdentity (resolvedSymbolIdentity symbol) exports
    [] -> Nothing

exportedTypeByIdentity :: SymbolIdentity -> ModuleExports -> Maybe (String, ExportedTypeInfo)
exportedTypeByIdentity identity exports =
  (,) <$> lookupSymbolIdentityExact identity (exportedTypeDisplaysByIdentity exports) <*> lookupSymbolIdentityExact identity (exportedTypesByIdentity exports)

exportedClassByIdentity :: ClassIdentity -> ModuleExports -> Maybe (String, ClassInfo)
exportedClassByIdentity identity exports =
  (,) <$> lookupSymbolIdentityExact identity (exportedClassDisplaysByIdentity exports) <*> lookupSymbolIdentityExact identity (exportedClassesByIdentity exports)

displaySrcTypeForResolved :: DisplayNameEnv -> ResolvedSrcType -> TcM SrcType
displaySrcTypeForResolved env = \case
  RSTVar ref -> pure (STVar (resolvedSrcTypeBinderName ref))
  RSTArrow dom cod -> STArrow <$> displaySrcTypeForResolved env dom <*> displaySrcTypeForResolved env cod
  RSTBase symbol -> STBase <$> displayTypeHeadName env symbol
  RSTCon symbol args -> STCon <$> displayTypeHeadName env symbol <*> traverse (displaySrcTypeForResolved env) args
  RSTVarApp ref args -> STVarApp (resolvedSrcTypeBinderName ref) <$> traverse (displaySrcTypeForResolved env) args
  RSTTyLam ref body -> STTyLam (resolvedSrcTypeBinderName ref) <$> displaySrcTypeForResolved env body
  RSTTyApp fun arg -> STTyApp <$> displaySrcTypeForResolved env fun <*> displaySrcTypeForResolved env arg
  RSTForall ref mb body ->
    STForall (resolvedSrcTypeBinderName ref)
      <$> traverse (fmap SrcBound . displaySrcTypeForResolved env . unResolvedSrcBound) mb
      <*> displaySrcTypeForResolved env body
  RSTMu ref body -> STMu (resolvedSrcTypeBinderName ref) <$> displaySrcTypeForResolved env body
  RSTBottom -> pure STBottom

displayTypeHeadName :: DisplayNameEnv -> ResolvedSymbol -> TcM String
displayTypeHeadName env symbol =
  case displayNameForSymbol (dneTypes env) symbol of
    Just name -> pure name
    Nothing
      | isBuiltinTypeSymbol symbol -> pure (resolvedSymbolDisplayName symbol)
    Nothing -> throwError (ProgramUnknownType (resolvedSymbolDisplayName symbol))

typeViewForDisplayEnv :: DisplayNameEnv -> ResolvedSrcType -> TcM TypeView
typeViewForDisplayEnv env ty = do
  display <- displaySrcTypeForResolved env ty
  let view = typeViewFromResolved ty
  case typeViewWithDisplay display view of
    Right displayedView -> pure displayedView
    Left err -> throwError (ProgramPipelineError ("resolved type display shape mismatch: " ++ show err))

constraintInfoForDisplayEnv :: DisplayNameEnv -> P.ResolvedClassConstraint -> TcM ConstraintInfo
constraintInfoForDisplayEnv env constraint = do
  views <- mapM (typeViewForDisplayEnv env) (P.constraintTypes constraint)
  ConstraintInfo
    <$> displayClassName env (P.constraintClassName constraint)
    <*> pure (resolvedSymbolIdentity (P.constraintClassName constraint))
    <*> pure views

displayClassName :: DisplayNameEnv -> ResolvedSymbol -> TcM String
displayClassName env symbol =
  case displayNameForSymbol (dneClasses env) symbol of
    Just name -> pure name
    Nothing -> throwError (ProgramUnknownClass (resolvedSymbolDisplayName symbol))

displayNameForSymbol :: Map SymbolIdentity [String] -> ResolvedSymbol -> Maybe String
displayNameForSymbol namesByIdentity symbol =
  resolvedSymbolDisplayName symbol <$ lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) namesByIdentity

-- Source kind checking -------------------------------------------------------

validateModuleKinds :: Scope -> P.ResolvedModuleSyntax -> TcM ()
validateModuleKinds scope mod0 = do
  let baseEnv = kindEnvFromScope scope
  mapM_ (validateDataDeclKinds baseEnv) (moduleDataDecls mod0)
  mapM_ (validateClassDeclKinds scope baseEnv) (moduleClassDecls mod0)
  mapM_ (validateDefDeclKinds scope baseEnv) (moduleDefDecls mod0)
  mapM_ (validateInstanceDeclKinds scope baseEnv) (explicitInstances mod0)

kindEnvFromScope :: Scope -> KindEnv
kindEnvFromScope scope =
  KindEnv
    { kindTypeConstructors =
        Map.fromList
          [ (dataInfoSymbolIdentity dataInfo, dataKind (dataTypeParams dataInfo))
            | dataInfo <- Map.elems (scopeElaborateTypes scope)
          ],
      kindTypeVariables = Map.empty,
      kindMetaSubst = Map.empty,
      kindNextMeta = 0
    }

dataKind :: [CheckedTypeParam] -> P.SrcKind
dataKind params =
  foldr P.KArrow P.KType (map checkedTypeParamKind params)

checkedTypeParamFromResolved :: P.TypeParam -> TcM CheckedTypeParam
checkedTypeParamFromResolved param =
  case P.typeParamRef param of
    Just ref ->
      pure (CheckedTypeParam ref (P.typeParamKind param))
    Nothing ->
      throwError $
        ProgramPipelineError
          ("resolved type parameter `" ++ P.typeParamName param ++ "` is missing identity")

validateDataDeclKinds :: KindEnv -> P.ResolvedDataDecl -> TcM ()
validateDataDeclKinds baseEnv dataDecl = do
  env <- extendKindParams (P.dataDeclParams dataDecl) baseEnv
  mapM_ (validateConstructorDeclKind env) (P.dataDeclConstructors dataDecl)

validateConstructorDeclKind :: KindEnv -> P.ResolvedConstructorDecl -> TcM ()
validateConstructorDeclKind env ctorDecl = do
  _ <- checkResolvedKind env (P.constructorDeclType ctorDecl) P.KType
  pure ()

validateClassDeclKinds :: Scope -> KindEnv -> P.ResolvedClassDecl -> TcM ()
validateClassDeclKinds scope baseEnv classDecl = do
  env <- extendKindParams (NE.toList (P.classDeclParams classDecl)) baseEnv
  env1 <- foldM (validateClassConstraintKind scope) env (P.classDeclSuperclasses classDecl)
  mapM_ (validateMethodSigKind scope env1) (P.classDeclMethods classDecl)

validateMethodSigKind :: Scope -> KindEnv -> P.ResolvedMethodSig -> TcM ()
validateMethodSigKind scope env methodSig = do
  _ <- validateConstrainedTypeKinds scope env (P.methodSigType methodSig)
  pure ()

validateDefDeclKinds :: Scope -> KindEnv -> P.ResolvedDefDecl -> TcM ()
validateDefDeclKinds scope env defDecl = do
  _ <- validateConstrainedTypeKinds scope env (P.defDeclType defDecl)
  pure ()

validateInstanceDeclKinds :: Scope -> KindEnv -> P.ResolvedInstanceDecl -> TcM ()
validateInstanceDeclKinds scope env0 instDecl = do
  classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
  env1 <- foldM (validateClassConstraintKind scope) env0 (P.instanceDeclConstraints instDecl)
  validateClassApplicationArity classInfo (length (P.instanceDeclTypes instDecl))
  _ <-
    foldM
      (\acc (ty, param) -> checkResolvedKind acc ty (checkedTypeParamKind param))
      env1
      (zip (NE.toList (P.instanceDeclTypes instDecl)) (NE.toList (classTypeParams classInfo)))
  pure ()

validateConstrainedTypeKinds :: Scope -> KindEnv -> P.ResolvedConstrainedType -> TcM KindEnv
validateConstrainedTypeKinds scope env0 ty = do
  env1 <- foldM (validateClassConstraintKind scope) env0 (P.constrainedConstraints ty)
  checkResolvedKind env1 (P.constrainedBody ty) P.KType

validateClassConstraintKind :: Scope -> KindEnv -> P.ResolvedClassConstraint -> TcM KindEnv
validateClassConstraintKind scope env constraint = do
  classInfo <- lookupClassInfoBySymbol scope (P.constraintClassName constraint)
  validateClassApplicationArity classInfo (length (P.constraintTypes constraint))
  foldM
    (\acc (ty, param) -> checkResolvedKind acc ty (checkedTypeParamKind param))
    env
    (zip (NE.toList (P.constraintTypes constraint)) (NE.toList (classTypeParams classInfo)))

validateClassApplicationArity :: ClassInfo -> Int -> TcM ()
validateClassApplicationArity classInfo actual =
  let expected = length (classTypeParams classInfo)
   in when (expected /= actual) $
        throwError (ProgramClassArityMismatch (className classInfo) expected actual)

extendKindParams :: [P.TypeParam] -> KindEnv -> TcM KindEnv
extendKindParams params env =
  foldM
    ( \acc param -> do
        ref <- typeParamResolvedRef param
        bindKindVariable
          ref
          (kindFromSrc (P.typeParamKind param))
          acc
    )
    env
    params
  where
    typeParamResolvedRef :: P.TypeParam -> TcM ResolvedTypeBinderRef
    typeParamResolvedRef param =
      case P.typeParamRef param of
        Just ref -> pure ref
        Nothing ->
          throwError $
            ProgramPipelineError
              ("resolved type parameter `" ++ P.typeParamName param ++ "` is missing identity")

checkResolvedKind :: KindEnv -> ResolvedSrcType -> P.SrcKind -> TcM KindEnv
checkResolvedKind env ty expected =
  checkResolvedKindTerm env ty (kindFromSrc expected)

checkResolvedKindTerm :: KindEnv -> ResolvedSrcType -> KindTerm -> TcM KindEnv
checkResolvedKindTerm env ty expected =
  case ty of
    RSTVar ref -> bindKindVariable ref expected env
    RSTArrow dom cod -> do
      env1 <- requireKindTerm env ty expected KTType
      env2 <- checkResolvedKindTerm env1 dom KTType
      checkResolvedKindTerm env2 cod KTType
    RSTForall ref mb body -> do
      env1 <- requireKindTerm env ty expected KTType
      env2 <-
        case mb of
          Nothing -> pure env1
          Just bound -> checkResolvedKindTerm env1 (unResolvedSrcBound bound) KTType
      withScopedKindVariable ref KTType env2 $ \env3 ->
        checkResolvedKindTerm env3 body KTType
    RSTMu ref body -> do
      env1 <- requireKindTerm env ty expected KTType
      withScopedKindVariable ref KTType env1 $ \env2 ->
        checkResolvedKindTerm env2 body KTType
    RSTVarApp ref args -> checkVarAppKind env ref args expected
    RSTTyApp fun arg -> checkTyAppKind env ty fun arg expected
    _ -> do
      (actual, env1) <- inferResolvedKindTerm env ty
      requireKindTerm env1 ty expected actual

inferResolvedKindTerm :: KindEnv -> ResolvedSrcType -> TcM (KindTerm, KindEnv)
inferResolvedKindTerm env ty =
  case ty of
    RSTVar ref -> kindTermForVariable ref env
    RSTBase symbol -> do
      kind0 <- resolvedTypeHeadKind env symbol
      pure (kindFromSrc kind0, env)
    RSTCon symbol args -> do
      headKind <- kindFromSrc <$> resolvedTypeHeadKind env symbol
      applyKindArgs env (RSTBase symbol) (resolvedSymbolDisplayName symbol) headKind args
    RSTVarApp ref args -> inferVarAppKind env ref args
    RSTTyLam {} ->
      throwError (ProgramKindMismatch (resolvedSrcTypeToSrcType ty) P.KType (P.KArrow P.KType P.KType))
    RSTTyApp fun arg -> inferTyAppKind env ty fun arg
    RSTArrow dom cod -> do
      env1 <- checkResolvedKindTerm env dom KTType
      env2 <- checkResolvedKindTerm env1 cod KTType
      pure (KTType, env2)
    RSTForall ref mb body -> do
      env1 <-
        case mb of
          Nothing -> pure env
          Just bound -> checkResolvedKindTerm env (unResolvedSrcBound bound) KTType
      env2 <-
        withScopedKindVariable ref KTType env1 $ \env3 ->
          checkResolvedKindTerm env3 body KTType
      pure (KTType, env2)
    RSTMu ref body -> do
      env1 <-
        withScopedKindVariable ref KTType env $ \env2 ->
          checkResolvedKindTerm env2 body KTType
      pure (KTType, env1)
    RSTBottom -> pure (KTType, env)

checkVarAppKind :: KindEnv -> ResolvedTypeBinderRef -> NonEmpty ResolvedSrcType -> KindTerm -> TcM KindEnv
checkVarAppKind env ref args expected = do
  (actual, env1) <- inferVarAppKind env ref args
  requireKindTerm env1 (RSTVarApp ref args) expected actual

checkTyAppKind :: KindEnv -> ResolvedSrcType -> ResolvedSrcType -> ResolvedSrcType -> KindTerm -> TcM KindEnv
checkTyAppKind env whole fun arg expected = do
  (actual, env1) <- inferTyAppKind env whole fun arg
  requireKindTerm env1 whole expected actual

inferTyAppKind :: KindEnv -> ResolvedSrcType -> ResolvedSrcType -> ResolvedSrcType -> TcM (KindTerm, KindEnv)
inferTyAppKind env whole fun arg = do
  (funKind, env1) <- inferResolvedKindTerm env fun
  case zonkKindTerm env1 funKind of
    KTArrow argKind resultKind -> do
      env2 <- checkResolvedKindTerm env1 arg argKind
      pure (resultKind, env2)
    KTMeta meta -> do
      (argKind, env2) <- inferResolvedKindTerm env1 arg
      let (resultKind, env3) = freshKindMeta env2
      env4 <- requireKindTerm env3 fun (KTMeta meta) (KTArrow argKind resultKind)
      pure (resultKind, env4)
    KTType ->
      throwError (ProgramKindMismatch (resolvedSrcTypeToSrcType whole) P.KType P.KType)

inferVarAppKind :: KindEnv -> ResolvedTypeBinderRef -> NonEmpty ResolvedSrcType -> TcM (KindTerm, KindEnv)
inferVarAppKind env ref args = do
  let name = resolvedSrcTypeBinderName ref
  (headKind, env1) <- kindTermForVariable ref env
  applyKindArgs env1 (RSTVar ref) name headKind args

applyKindArgs :: KindEnv -> ResolvedSrcType -> String -> KindTerm -> NonEmpty ResolvedSrcType -> TcM (KindTerm, KindEnv)
applyKindArgs env headTy headName headKind args =
  go 0 env headKind (NE.toList args)
  where
    go _ env0 kind0 [] = pure (zonkKindTerm env0 kind0, env0)
    go consumed env0 kind0 (arg : rest) =
      case zonkKindTerm env0 kind0 of
        KTArrow expected result -> do
          env1 <- checkResolvedKindTerm env0 arg expected
          go (consumed + 1) env1 result rest
        KTMeta meta -> do
          (argKind, env1) <- inferResolvedKindTerm env0 arg
          let (resultKind, env2) = freshKindMeta env1
          env3 <- requireKindTerm env2 headTy (KTMeta meta) (KTArrow argKind resultKind)
          go (consumed + 1) env3 resultKind rest
        KTType ->
          throwError (ProgramTypeArityMismatch headName consumed (consumed + length rest + 1))

requireKindTerm :: KindEnv -> ResolvedSrcType -> KindTerm -> KindTerm -> TcM KindEnv
requireKindTerm env ty expected actual =
  case unifyKindTerm env expected actual of
    Right env1 -> pure env1
    Left (KindUnifyMismatch expectedKind actualKind) ->
      case typeHeadArity env ty of
        Just (headName, expectedArgs, actualArgs)
          | expectedKind == P.KType && isArrowKind actualKind ->
              throwError (ProgramTypeArityMismatch headName expectedArgs actualArgs)
        _ -> throwError (ProgramKindMismatch (resolvedSrcTypeToSrcType ty) expectedKind actualKind)

data KindUnifyMismatch = KindUnifyMismatch P.SrcKind P.SrcKind
  deriving (Eq, Show)

unifyKindTerm :: KindEnv -> KindTerm -> KindTerm -> Either KindUnifyMismatch KindEnv
unifyKindTerm env left right =
  case (zonkKindTerm env left, zonkKindTerm env right) of
    (KTType, KTType) -> Right env
    (KTArrow leftDom leftCod, KTArrow rightDom rightCod) -> do
      env1 <- unifyKindTerm env leftDom rightDom
      unifyKindTerm env1 leftCod rightCod
    (KTMeta meta, term) -> bindKindMeta meta term env
    (term, KTMeta meta) -> bindKindMeta meta term env
    (leftTerm, rightTerm) ->
      Left (KindUnifyMismatch (kindTermToSrcKind env leftTerm) (kindTermToSrcKind env rightTerm))

bindKindMeta :: Int -> KindTerm -> KindEnv -> Either KindUnifyMismatch KindEnv
bindKindMeta meta term env =
  let term0 = zonkKindTerm env term
   in case term0 of
        KTMeta other
          | other == meta -> Right env
        _
          | kindMetaOccurs meta term0 env ->
              Left (KindUnifyMismatch (kindTermToSrcKind env (KTMeta meta)) (kindTermToSrcKind env term0))
          | otherwise ->
              Right env {kindMetaSubst = Map.insert meta term0 (kindMetaSubst env)}

kindMetaOccurs :: Int -> KindTerm -> KindEnv -> Bool
kindMetaOccurs meta term env =
  case zonkKindTerm env term of
    KTType -> False
    KTArrow dom cod -> kindMetaOccurs meta dom env || kindMetaOccurs meta cod env
    KTMeta other -> meta == other

kindTermForVariable :: ResolvedTypeBinderRef -> KindEnv -> TcM (KindTerm, KindEnv)
kindTermForVariable ref env =
  case Map.lookup ref (kindTypeVariables env) of
    Just kind0 -> pure (zonkKindTerm env kind0, env)
    Nothing ->
      let (kind0, env1) = freshKindMeta env
       in pure (kind0, env1 {kindTypeVariables = Map.insert ref kind0 (kindTypeVariables env1)})

freshKindMeta :: KindEnv -> (KindTerm, KindEnv)
freshKindMeta env =
  (KTMeta (kindNextMeta env), env {kindNextMeta = kindNextMeta env + 1})

kindFromSrc :: P.SrcKind -> KindTerm
kindFromSrc kind0 =
  case kind0 of
    P.KType -> KTType
    P.KArrow dom cod -> KTArrow (kindFromSrc dom) (kindFromSrc cod)

kindTermToSrcKind :: KindEnv -> KindTerm -> P.SrcKind
kindTermToSrcKind env kind0 =
  case zonkKindTerm env kind0 of
    KTType -> P.KType
    KTArrow dom cod -> P.KArrow (kindTermToSrcKind env dom) (kindTermToSrcKind env cod)
    KTMeta _ -> P.KType

zonkKindTerm :: KindEnv -> KindTerm -> KindTerm
zonkKindTerm env kind0 =
  case kind0 of
    KTType -> KTType
    KTArrow dom cod -> KTArrow (zonkKindTerm env dom) (zonkKindTerm env cod)
    KTMeta meta ->
      case Map.lookup meta (kindMetaSubst env) of
        Just replacement -> zonkKindTerm env replacement
        Nothing -> KTMeta meta

typeHeadArity :: KindEnv -> ResolvedSrcType -> Maybe (String, Int, Int)
typeHeadArity env ty =
  case ty of
    RSTVar ref ->
      withActualArity 0 <$> kindForVar ref
    RSTBase symbol ->
      withActualArity 0 <$> kindForHead symbol
    RSTVarApp ref args ->
      withActualArity (NE.length args) <$> kindForVar ref
    RSTCon symbol args ->
      withActualArity (NE.length args) <$> kindForHead symbol
    _ -> Nothing
  where
    withActualArity actualArgs (headName, expectedArgs) =
      (headName, expectedArgs, actualArgs)

    kindForVar ref =
      case Map.lookup ref (kindTypeVariables env) of
        Just kind0 -> Just (resolvedSrcTypeBinderName ref, kindTermArity env kind0)
        Nothing -> Nothing

    kindForHead symbol =
      case resolvedTypeHeadKindMaybe env symbol of
        Just kind0 -> Just (resolvedSymbolDisplayName symbol, kindArity kind0)
        Nothing -> Nothing

kindArity :: P.SrcKind -> Int
kindArity kind0 =
  case kind0 of
    P.KType -> 0
    P.KArrow _ result -> 1 + kindArity result

kindTermArity :: KindEnv -> KindTerm -> Int
kindTermArity env kind0 =
  case zonkKindTerm env kind0 of
    KTType -> 0
    KTArrow _ result -> 1 + kindTermArity env result
    KTMeta _ -> 0

isArrowKind :: P.SrcKind -> Bool
isArrowKind kind0 =
  case kind0 of
    P.KArrow {} -> True
    P.KType -> False

bindKindVariable :: ResolvedTypeBinderRef -> KindTerm -> KindEnv -> TcM KindEnv
bindKindVariable ref expected env =
  case Map.lookup ref (kindTypeVariables env) of
    Just actual -> requireKindTerm env (RSTVar ref) expected actual
    Nothing ->
      pure env {kindTypeVariables = Map.insert ref (zonkKindTerm env expected) (kindTypeVariables env)}

withScopedKindVariable :: ResolvedTypeBinderRef -> KindTerm -> KindEnv -> (KindEnv -> TcM KindEnv) -> TcM KindEnv
withScopedKindVariable ref kind0 env action = do
  let previous = Map.lookup ref (kindTypeVariables env)
      envWithBinder = env {kindTypeVariables = Map.insert ref kind0 (kindTypeVariables env)}
  envAfter <- action envWithBinder
  pure
    envAfter
      { kindTypeVariables =
          case previous of
            Just previousKind -> Map.insert ref previousKind (kindTypeVariables envAfter)
            Nothing -> Map.delete ref (kindTypeVariables envAfter)
      }

resolvedTypeHeadKind :: KindEnv -> ResolvedSymbol -> TcM P.SrcKind
resolvedTypeHeadKind env symbol =
  case resolvedTypeHeadKindMaybe env symbol of
    Just kind0 -> pure kind0
    Nothing -> throwError (ProgramUnknownType (resolvedSymbolDisplayName symbol))

resolvedTypeHeadKindMaybe :: KindEnv -> ResolvedSymbol -> Maybe P.SrcKind
resolvedTypeHeadKindMaybe env symbol
  | Just kind0 <- builtinTypeKindByIdentity (resolvedSymbolIdentity symbol) = Just kind0
  | otherwise = lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) (kindTypeConstructors env)

builtinTypeKindByIdentity :: SymbolIdentity -> Maybe P.SrcKind
builtinTypeKindByIdentity identity =
  lookupSymbolIdentityExact identity builtinTypeKindsByIdentity

builtinTypeKindsByIdentity :: Map SymbolIdentity P.SrcKind
builtinTypeKindsByIdentity =
  Map.fromList
    [ (Builtins.builtinTypeIdentity name, kind0)
    | name <- Set.toList Builtins.builtinTypeNames,
      Just kind0 <- [Builtins.builtinTypeKind name]
    ]

buildLocalDataInfo :: DisplayNameEnv -> P.ResolvedModuleSyntax -> TcM (Map String DataInfo)
buildLocalDataInfo displayEnv mod0 = do
  let dataDecls = moduleDataDecls mod0
  ensureDistinctBy ProgramDuplicateType P.dataDeclDisplayName dataDecls
  ctorNames <- pure (concatMap (map (P.refDisplayName . P.constructorDeclName) . P.dataDeclConstructors) dataDecls)
  ensureDistinctPlain ProgramDuplicateConstructor ctorNames
  pure . Map.fromList =<< mapM toDataInfo dataDecls
  where
    toDataInfo dataDecl = do
      let sourceParams = P.dataDeclParams dataDecl
          paramNames = P.typeParamNames sourceParams
      ensureDistinctPlain ProgramDuplicateTypeParameter paramNames
      params <- traverse checkedTypeParamFromResolved sourceParams
      let dataSymbol = P.dataDeclName dataDecl
          dataIdentity = resolvedSymbolIdentity dataSymbol
          dataName0 = P.refDisplayName dataSymbol
      constructors0 <- zipWithM (toCtorInfo dataDecl dataIdentity) [0 ..] (P.dataDeclConstructors dataDecl)
      let ownerShapes =
            [ (constructorShapeFromInfo ctor) {constructorShapeOwnerTypeParams = params}
              | ctor <- constructors0
            ]
          constructors =
            [ ctor {ctorOwnerConstructors = ownerShapes}
              | ctor <- constructors0
            ]
      pure
        ( dataName0,
          DataInfo
            { dataInfoSymbol = dataIdentity,
              dataTypeParams = params,
              dataConstructors = constructors
            }
        )

    toCtorInfo dataDecl dataIdentity index ctorDecl = do
      let ctorSymbol = P.constructorDeclName ctorDecl
          ctorIdentity = resolvedSymbolIdentity ctorSymbol
      validateConstructorResult dataIdentity dataDecl ctorDecl (constructorResolvedResult (P.constructorDeclType ctorDecl))
      ctorTypeView0 <- typeViewForDisplayEnv displayEnv (P.constructorDeclType ctorDecl)
      pure
        ConstructorInfo
          { ctorInfoSymbol = ctorIdentity,
            ctorRuntimeName = qualify (symbolDefiningModule ctorIdentity) (symbolDefiningName ctorIdentity),
            ctorTypeView = ctorTypeView0,
            ctorOwningTypeIdentity = dataIdentity,
            ctorIndex = index,
            ctorOwnerConstructors = []
          }

    validateConstructorResult :: SymbolIdentity -> P.ResolvedDataDecl -> P.ResolvedConstructorDecl -> ResolvedSrcType -> TcM ()
    validateConstructorResult dataIdentity dataDecl ctorDecl resultTy =
      let owner = P.dataDeclDisplayName dataDecl
          params = P.typeParamNames (P.dataDeclParams dataDecl)
          invalid = throwError (ProgramInvalidConstructorResult (P.refDisplayName (P.constructorDeclName ctorDecl)) (resolvedSrcTypeToSrcType resultTy) owner)
       in case constructorResultHead resultTy of
            Just (symbol, argCount)
              | sameSymbolIdentity (resolvedSymbolIdentity symbol) dataIdentity && argCount == length params -> pure ()
            _ -> invalid

    constructorResultHead :: ResolvedSrcType -> Maybe (ResolvedSymbol, Int)
    constructorResultHead resultTy =
      case resultTy of
        RSTBase symbol -> Just (symbol, 0)
        RSTCon symbol args -> Just (symbol, NE.length args)
        _ -> Nothing

    constructorResolvedResult :: ResolvedSrcType -> ResolvedSrcType
    constructorResolvedResult =
      snd . splitResolvedArrows . stripResolvedForalls

    stripResolvedForalls :: ResolvedSrcType -> ResolvedSrcType
    stripResolvedForalls resultTy =
      case resultTy of
        RSTForall _ _ body -> stripResolvedForalls body
        _ -> resultTy

    splitResolvedArrows :: ResolvedSrcType -> ([ResolvedSrcType], ResolvedSrcType)
    splitResolvedArrows resultTy =
      case resultTy of
        RSTArrow dom cod ->
          let (args, result) = splitResolvedArrows cod
           in (dom : args, result)
        _ -> ([], resultTy)

buildLocalClassInfo :: DisplayNameEnv -> P.ResolvedModuleSyntax -> TcM (Map String ClassInfo)
buildLocalClassInfo displayEnv mod0 = do
  let classDecls = moduleClassDecls mod0
  ensureDistinctBy ProgramDuplicateClass P.classDeclDisplayName classDecls
  pure . Map.fromList =<< mapM toClassInfo classDecls
  where
    toClassInfo classDecl = do
      ensureDistinctBy ProgramDuplicateMethod P.methodSigDisplayName (P.classDeclMethods classDecl)
      let classSymbol = P.classDeclName classDecl
          classIdentity0 = resolvedSymbolIdentity classSymbol
          className0 = P.refDisplayName classSymbol
          sourceClassParams = P.classDeclParams classDecl
          classParamNames0 = fmap P.typeParamName sourceClassParams
      ensureDistinctPlain ProgramDuplicateTypeParameter (NE.toList classParamNames0)
      classParams <- traverse checkedTypeParamFromResolved sourceClassParams
      let classParamBinders0 = fmap (\param -> (checkedTypeParamName param, checkedTypeParamIdentity param)) classParams
      validateFunctionalDependencies className0 classParamNames0 (P.classDeclFundeps classDecl)
      fundeps0 <- mapM (functionalDependencyInfo className0 classParamBinders0) (P.classDeclFundeps classDecl)
      superclassInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.classDeclSuperclasses classDecl)
      methodEntries <-
        forM
          (P.classDeclMethods classDecl)
          ( \sig -> do
              let methodSymbol = P.methodSigName sig
                  methodIdentity = resolvedSymbolIdentity methodSymbol
                  methodName0 = P.refDisplayName methodSymbol
              constraintInfos <- mapM (constraintInfoForDisplayEnv displayEnv) (P.constrainedConstraints (P.methodSigType sig))
              methodBodyView <- typeViewForDisplayEnv displayEnv (P.constrainedBody (P.methodSigType sig))
              let methodInfo =
                    MethodInfo
                      { methodInfoSymbol = methodIdentity,
                        methodDisplayName = methodName0,
                        methodTypeViewRaw = methodBodyView,
                        methodConstraintInfos = constraintInfos,
                        methodParamBinders = classParamBinders0
                      }
              pure (methodName0, methodInfo)
          )
      let methodsByIdentity =
            uniqueInfoListByIdentity methodInfoSymbolIdentity (map snd methodEntries)
      pure
        ( className0,
          ClassInfo
            { classInfoSymbol = classIdentity0,
              classTypeParams = classParams,
              classSuperclassInfos = superclassInfos0,
              classFunctionalDependencies = fundeps0,
              classMethodsByIdentity = methodsByIdentity
            }
        )

validateFunctionalDependencies :: P.ClassName -> NonEmpty String -> [P.FunctionalDependency] -> TcM ()
validateFunctionalDependencies className0 paramNames fundeps =
  mapM_ validate fundeps
  where
    params = Set.fromList (NE.toList paramNames)

    validate fundep = do
      let determiners = NE.toList (P.fundepDeterminers fundep)
          determined = NE.toList (P.fundepDetermined fundep)
      case invalidName (determiners ++ determined) of
        Just name -> throwError (ProgramInvalidFunctionalDependency className0 name)
        Nothing -> pure ()

    invalidName names =
      case [name | name <- names, name `Set.notMember` params] of
        name : _ -> Just name
        [] ->
          case duplicates names of
            dup : _ -> Just dup
            [] -> Nothing

functionalDependencyInfo :: P.ClassName -> NonEmpty (String, TypeBinderIdentity) -> P.FunctionalDependency -> TcM FunctionalDependencyInfo
functionalDependencyInfo className0 paramBinders fundep =
  FunctionalDependencyInfo
    <$> traverse lookupParam (P.fundepDeterminers fundep)
    <*> traverse lookupParam (P.fundepDetermined fundep)
  where
    paramRefs = Map.fromList (NE.toList paramBinders)

    lookupParam name =
      case Map.lookup name paramRefs of
        Just identity -> pure identity
        Nothing -> throwError (ProgramInvalidFunctionalDependency className0 name)

validateLocalClassMethodConstraints :: Scope -> P.ResolvedModuleSyntax -> TcM ()
validateLocalClassMethodConstraints scope mod0 =
  mapM_ validateClassDecl (moduleClassDecls mod0)
  where
    validateClassDecl classDecl = do
      validateResolvedClassConstraintClasses scope (P.classDeclSuperclasses classDecl)
      mapM_ validateMethodConstraints (P.classDeclMethods classDecl)

    validateMethodConstraints =
      validateResolvedClassConstraintClasses scope
        . P.constrainedConstraints
        . P.methodSigType

validateResolvedClassConstraintClasses :: Scope -> [P.ResolvedClassConstraint] -> TcM ()
validateResolvedClassConstraintClasses scope =
  mapM_ $ \constraint -> do
    _ <- lookupClassInfoBySymbol scope (P.constraintClassName constraint)
    pure ()

buildLocalDefInfo :: DisplayNameEnv -> P.ResolvedModuleSyntax -> TcM (Map String ValueInfo)
buildLocalDefInfo displayEnv mod0 = do
  let defs = moduleDefDecls mod0
  ensureDistinctBy ProgramDuplicateValue (P.refDisplayName . P.defDeclName) defs
  Map.fromList <$> mapM toValueInfo defs
  where
    toValueInfo defDecl = do
      let defSymbol = P.defDeclName defDecl
          defName = P.refDisplayName defSymbol
          valueIdentity = resolvedSymbolIdentity defSymbol
          valueIdentityName = symbolDefiningName valueIdentity
      defConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.constrainedConstraints (P.defDeclType defDecl))
      defBodyView0 <- typeViewForDisplayEnv displayEnv (P.constrainedBody (P.defDeclType defDecl))
      let defTypeView0 =
            constrainedVisibleTypeView defConstraintInfos0 defBodyView0
      pure
        ( defName,
          OrdinaryValue
            { valueInfoSymbol = valueIdentity,
              valueRuntimeName = qualify (symbolDefiningModule valueIdentity) valueIdentityName,
              valueTypeView = defTypeView0,
              valueConstraintInfos = defConstraintInfos0
            }
        )

addConstructorValues :: Map String DataInfo -> TcM (Map String ValueInfo)
addConstructorValues dataInfos =
  pure $
    Map.fromList
      [ ( ctorName ctor,
          ConstructorValue
            { valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
              valueRuntimeName = constructorInfoRuntimeName ctor,
              valueCtorInfo = ctor
            }
        )
        | dataInfo <- Map.elems dataInfos,
          ctor <- dataConstructors dataInfo
      ]

synthesizeDerivedInstances ::
  SymbolIdentity ->
  IdentityGenerator ->
  DisplayNameEnv ->
  Scope ->
  P.ResolvedModuleSyntax ->
  TcM ([P.ResolvedInstanceDecl], IdentityGenerator)
synthesizeDerivedInstances moduleIdentity generator0 displayEnv scope mod0 = do
  (candidates, generator1) <- deriveDataDecls generator0 (moduleDataDecls mod0)
  pendingInstances <- mapM (\(_, _, classInfo, classDisplayName, instDecl) -> pendingDerivedInstance classInfo classDisplayName instDecl) candidates
  let validationScope = withScopeInstances (scopeInstances scope ++ pendingInstances) scope
  mapM_
    (\(resolvedDataDecl, displayDataDecl, classInfo, classDisplayName, _) -> validateEqDerivingFields classInfo classDisplayName validationScope resolvedDataDecl displayDataDecl)
    candidates
  pure ([instDecl | (_, _, _, _, instDecl) <- candidates], generator1)
  where
    deriveDataDecls generator [] = pure ([], generator)
    deriveDataDecls generator (dataDecl : rest) = do
      (current, generator1) <- deriveForData generator dataDecl
      (remaining, generator2) <- deriveDataDecls generator1 rest
      pure (current ++ remaining, generator2)

    deriveForData generator0' dataDecl = do
      displayDataDecl <- resolvedDataDeclForEnv dataDecl
      deriveClasses displayDataDecl generator0' (P.dataDeclDeriving dataDecl)
      where
        deriveClasses _ generator [] = pure ([], generator)
        deriveClasses displayDataDecl generator (classSymbol : rest) = do
          classInfo <- lookupClassInfoBySymbol scope classSymbol
          let classDisplayName = resolvedSymbolDisplayName classSymbol
          if hasDisplayName (dneClasses displayEnv) (classInfoSymbolIdentity classInfo) isEqName
            then
              case eqMethodReference classInfo of
                Just eqMethodSymbol -> do
                  (instDecl, generator1) <- mkEqInstance generator classSymbol classInfo eqMethodSymbol dataDecl displayDataDecl
                  (remaining, generator2) <- deriveClasses displayDataDecl generator1 rest
                  pure ((dataDecl, displayDataDecl, classInfo, classDisplayName, instDecl) : remaining, generator2)
                Nothing -> throwError (ProgramUnsupportedDeriving (resolvedSymbolDisplayName classSymbol))
            else throwError (ProgramUnsupportedDeriving (resolvedSymbolDisplayName classSymbol))

    eqMethodReference classInfo =
      methodSymbol <$> find hasEqMethodDisplay (Map.elems (classMethodsByIdentity classInfo))
      where
        hasEqMethodDisplay methodInfo =
          hasDisplayName (dneValues displayEnv) (methodInfoSymbolIdentity methodInfo) isEqMethodName

        methodSymbol methodInfo =
          mkResolvedSymbol
            (methodInfoSymbolIdentity methodInfo)
            (methodName methodInfo)
            (methodName methodInfo)
            (SymbolLocal (classInfoIdentityModule classInfo))

    hasDisplayName namesByIdentity identity predicate =
      maybe False (any predicate) (lookupSymbolIdentityExact identity namesByIdentity)

    isEqName name =
      unqualifiedDisplayName name == "Eq"

    isEqMethodName name =
      unqualifiedDisplayName name == "eq"

    unqualifiedDisplayName =
      reverse . takeWhile (/= '.') . reverse

    pendingDerivedInstance classInfo classDisplayName instDecl = do
      constraintInfos <-
        forM (P.instanceDeclConstraints instDecl) $ \constraint -> do
          constraintViews <- mapM (typeViewForDisplayEnv displayEnv) (P.constraintTypes constraint)
          pure
            ConstraintInfo
              { constraintDisplayClass = classDisplayName,
                constraintClassSymbol = classInfoSymbolIdentity classInfo,
                constraintTypeViews = constraintViews
              }
      instanceHeadView <- typeViewForDisplayEnv displayEnv (P.instanceDeclType instDecl)
      pure
        InstanceInfo
          { instanceClassSymbol = classInfoSymbolIdentity classInfo,
            instanceOriginModuleIdentity = moduleIdentity,
            instanceConstraintInfos = constraintInfos,
            instanceHeadTypeViews = instanceHeadView :| [],
            instanceMethodsByIdentity = Map.empty
          }

    resolvedDataDeclForEnv :: P.ResolvedDataDecl -> TcM P.DataDecl
    resolvedDataDeclForEnv dataDecl = do
      constructors <-
        forM (P.dataDeclConstructors dataDecl) $ \ctor -> do
          ctorTy <- displaySrcTypeForResolved displayEnv (P.constructorDeclType ctor)
          pure (P.ConstructorDecl (P.refDisplayName (P.constructorDeclName ctor)) ctorTy)
      pure
        P.DataDecl
          { P.dataDeclName = P.dataDeclDisplayName dataDecl,
            P.dataDeclParams = P.dataDeclParams dataDecl,
            P.dataDeclConstructors = constructors,
            P.dataDeclDeriving = map resolvedSymbolDisplayName (P.dataDeclDeriving dataDecl)
          }

    validateEqDerivingFields :: ClassInfo -> P.ClassName -> Scope -> P.ResolvedDataDecl -> P.DataDecl -> TcM ()
    validateEqDerivingFields eqClassInfo eqClassDisplayName validationScope resolvedDataDecl displayDataDecl = do
      paramRefs <- traverse requiredResolvedDataParamRef (P.dataDeclParams resolvedDataDecl)
      let ownerHeadView =
            typeViewFromResolved
              (dataDeclHeadResolvedType (P.dataDeclName resolvedDataDecl) paramRefs)
      mapM_
        (validateEqDerivingField eqClassInfo eqClassDisplayName validationScope displayDataDecl ownerHeadView)
        (concatMap (resolvedConstructorFieldTypes . P.constructorDeclType) (P.dataDeclConstructors resolvedDataDecl))

    validateEqDerivingField :: ClassInfo -> P.ClassName -> Scope -> P.DataDecl -> TypeView -> ResolvedSrcType -> TcM ()
    validateEqDerivingField eqClassInfo eqClassDisplayName validationScope dataDecl ownerHeadView fieldTy = do
      fieldView <- typeViewForDisplayEnv displayEnv fieldTy
      satisfiable <-
        constraintTypeSatisfiable
          (classInfoSymbolIdentity eqClassInfo)
          eqClassDisplayName
          validationScope
          dataDecl
          ownerHeadView
          []
          (classInfoSymbolIdentity eqClassInfo)
          eqClassDisplayName
          fieldView
      if satisfiable
        then pure ()
        else throwError (ProgramDerivingMissingFieldInstance eqClassDisplayName (typeViewDisplay fieldView))

    constraintTypeSatisfiable :: ClassIdentity -> P.ClassName -> Scope -> P.DataDecl -> TypeView -> [ClassApplicationKey] -> ClassIdentity -> P.ClassName -> TypeView -> TcM Bool
    constraintTypeSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl ownerHeadView seen classIdentity0 className0 fieldView = do
      coveredByDerived <-
        if classIdentity0 == derivedClassIdentity
          then fieldCoveredByDerivedConstraints dataDecl ownerHeadView fieldView
          else pure False
      if coveredByDerived
        then pure True
        else
          if key `elem` seen
            then pure False
            else
              case resolveInstanceInfoWithIdentityType elaborateScope classIdentity0 className0 fieldView of
                Right (instanceInfo, subst) -> do
                  let seen' = key : seen
                  allM
                    (constraintSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl ownerHeadView seen' . applyConstraintInfoSubst subst)
                    (instanceConstraintInfos instanceInfo)
                Left _ -> pure False
      where
        elaborateScope = scopeToElaborateScope validationScope
        key = classApplicationKey classIdentity0 (fieldView :| [])

    constraintSatisfiable :: ClassIdentity -> P.ClassName -> Scope -> P.DataDecl -> TypeView -> [ClassApplicationKey] -> ConstraintInfo -> TcM Bool
    constraintSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl ownerHeadView seen constraint =
      constraintTypeSatisfiable
        derivedClassIdentity
        derivedClassName
        validationScope
        dataDecl
        ownerHeadView
        seen
        (constraintClassSymbol constraint)
        (constraintDisplayClass constraint)
        (constraintTypeView constraint)

    allM :: (a -> TcM Bool) -> [a] -> TcM Bool
    allM predicate =
      foldM step True
      where
        step False _ = pure False
        step True value = predicate value

    fieldCoveredByDerivedConstraints :: P.DataDecl -> TypeView -> TypeView -> TcM Bool
    fieldCoveredByDerivedConstraints dataDecl ownerHeadView fieldView =
      case typeViewNodeView fieldView of
        TypeViewVarNode _ identity -> do
          dataParamIdentities <- traverse dataParamIdentity (P.dataDeclParams dataDecl)
          pure (identity `Set.member` Set.fromList dataParamIdentities)
        _ ->
          pure (fieldView == ownerHeadView)

    dataParamIdentity :: P.TypeParam -> TcM TypeBinderIdentity
    dataParamIdentity param =
      case typeParamBinderIdentity param of
        Just identity -> pure identity
        Nothing ->
          throwError $
            ProgramPipelineError
              ("resolved data parameter `" ++ P.typeParamName param ++ "` is missing identity")

    requiredResolvedDataParamRef :: P.TypeParam -> TcM ResolvedTypeBinderRef
    requiredResolvedDataParamRef param =
      case P.typeParamRef param of
        Just ref -> pure ref
        Nothing ->
          throwError $
            ProgramPipelineError
              ("resolved data parameter `" ++ P.typeParamName param ++ "` is missing identity")

    derivedConstraintParams dataDecl ownerHeadTy paramRefs =
      let params = Set.fromList (map resolvedTypeBinderTypeIdentity paramRefs)
          fieldTypes =
            filter
              (not . isRecursiveOwnerResolvedField ownerHeadTy)
              (concatMap (resolvedConstructorFieldTypes . P.constructorDeclType) (P.dataDeclConstructors dataDecl))
          usedParams = Set.intersection params (foldMap freeTypeBinderIdentities fieldTypes)
       in usedParams

    freeTypeBinderIdentities ty =
      case ty of
        RSTVar ref -> Set.singleton (resolvedTypeBinderTypeIdentity ref)
        RSTArrow dom cod -> Set.union (freeTypeBinderIdentities dom) (freeTypeBinderIdentities cod)
        RSTBase {} -> Set.empty
        RSTCon _ args -> foldMap freeTypeBinderIdentities args
        RSTVarApp ref args -> Set.insert (resolvedTypeBinderTypeIdentity ref) (foldMap freeTypeBinderIdentities args)
        RSTTyLam ref body -> Set.delete (resolvedTypeBinderTypeIdentity ref) (freeTypeBinderIdentities body)
        RSTTyApp fun arg -> Set.union (freeTypeBinderIdentities fun) (freeTypeBinderIdentities arg)
        RSTForall ref mb body ->
          maybe Set.empty (freeTypeBinderIdentities . unResolvedSrcBound) mb
            `Set.union` Set.delete (resolvedTypeBinderTypeIdentity ref) (freeTypeBinderIdentities body)
        RSTMu ref body -> Set.delete (resolvedTypeBinderTypeIdentity ref) (freeTypeBinderIdentities body)
        RSTBottom -> Set.empty

    scopeToElaborateScope scope0 =
      mkElaborateScope (scopeValues scope0) (scopeElaborateTypes scope0) (scopeClasses scope0) (scopeInstances scope0)

    mkEqInstance generator0' classSymbol _classInfo eqMethodSymbol resolvedDataDecl _displayDataDecl = do
      let dataSymbol = P.dataDeclName resolvedDataDecl
          dataName0 = symbolDefiningName (resolvedSymbolIdentity dataSymbol)
          andSymbol = Builtins.builtinValueSymbol "__mlfp_and"
      boolSymbol <- pure (Builtins.builtinTypeSymbol "Bool")
      dataParamRefs <- traverse resolvedDataParamRef (P.dataDeclParams resolvedDataDecl)
      let (paramRefs, deriveGen0) = freshDerivedInstanceParamRefs generator0' dataParamRefs
          paramSubst =
            Map.fromList
              [ (resolvedTypeBinderTypeIdentity dataParamRef, instanceParamRef)
              | (dataParamRef, instanceParamRef) <- zip dataParamRefs paramRefs
              ]
      ctorEntries <-
        forM (P.dataDeclConstructors resolvedDataDecl) $ \ctor -> do
          let ctorSymbol = P.constructorDeclName ctor
              argTypes =
                map
                  (substituteResolvedTypeBinders paramSubst)
                  (resolvedConstructorFieldTypes (P.constructorDeclType ctor))
          pure (ctor, ctorSymbol, argTypes)
      let headTy = dataDeclHeadResolvedType dataSymbol paramRefs
          originalHeadTy = dataDeclHeadResolvedType dataSymbol dataParamRefs
          originalConstraintParamIdentities = derivedConstraintParams resolvedDataDecl originalHeadTy dataParamRefs
          derivedConstraintParamIdentities =
            Set.fromList
              [ resolvedTypeBinderTypeIdentity instanceParamRef
              | (dataParamRef, instanceParamRef) <- zip dataParamRefs paramRefs,
                resolvedTypeBinderTypeIdentity dataParamRef `Set.member` originalConstraintParamIdentities
              ]
          (leftRef, deriveGen1) = freshLocalRef "left" deriveGen0
          (rightRef, deriveGen2) = freshLocalRef "right" deriveGen1
          left = P.Param leftRef (Just headTy)
          right = P.Param rightRef (Just headTy)
          selfName = "__derived_eq_" ++ dataName0
          (selfRef, deriveGen3) = freshLocalRef selfName deriveGen2
          (recursiveBody, recursiveGenerator) =
            deriveEqBody eqMethodSymbol andSymbol headTy ctorEntries leftRef rightRef (Just selfRef) deriveGen3
          (nonRecursiveBody, nonRecursiveGenerator) =
            deriveEqBody eqMethodSymbol andSymbol headTy ctorEntries leftRef rightRef Nothing deriveGen3
          recursive =
            any
              (isRecursiveOwnerResolvedField headTy)
              [ argTy
              | (_, _, argTypes) <- ctorEntries,
                argTy <- argTypes
              ]
          methodBody =
            if recursive
              then
                P.ELet
                  selfRef
                  (Just (RSTArrow headTy (RSTArrow headTy (RSTBase boolSymbol))))
                  (P.ELam left (P.ELam right recursiveBody))
                  (P.EVar (P.ResolvedLocalValue selfRef))
              else
                P.ELam left (P.ELam right nonRecursiveBody)
          generator1
            | recursive = recursiveGenerator
            | otherwise = nonRecursiveGenerator
      pure
        ( P.InstanceDecl
            { P.instanceDeclClass = classSymbol,
              P.instanceDeclConstraints =
                [ P.ClassConstraint
                    { P.constraintClassName = classSymbol,
                      P.constraintTypes = RSTVar paramRef :| []
                    }
                  | paramRef <- paramRefs,
                    resolvedTypeBinderTypeIdentity paramRef `Set.member` derivedConstraintParamIdentities
                ],
              P.instanceDeclTypes = headTy :| [],
              P.instanceDeclMethods = [P.MethodDef eqMethodSymbol methodBody]
            },
          generator1
        )
      where
        resolvedDataParamRef :: P.TypeParam -> TcM ResolvedTypeBinderRef
        resolvedDataParamRef param =
          case P.typeParamRef param of
            Just ref -> pure ref
            Nothing ->
              throwError $
                ProgramPipelineError
                  ("resolved data parameter `" ++ P.typeParamName param ++ "` is missing identity")

        freshDerivedInstanceParamRefs :: IdentityGenerator -> [ResolvedTypeBinderRef] -> ([ResolvedTypeBinderRef], IdentityGenerator)
        freshDerivedInstanceParamRefs generator refs =
          case refs of
            [] -> ([], generator)
            ref : rest ->
              let (unique, generator1) = freshIdentity generator
                  instanceRef =
                    resolvedTypeBinderRefFromIdentity
                      (typeBinderIdentityFromUnique unique)
                      (resolvedSrcTypeBinderName ref)
                  (instanceRefs, generator2) = freshDerivedInstanceParamRefs generator1 rest
               in (instanceRef : instanceRefs, generator2)

        substituteResolvedTypeBinders :: Map TypeBinderIdentity ResolvedTypeBinderRef -> ResolvedSrcType -> ResolvedSrcType
        substituteResolvedTypeBinders subst =
          go subst
          where
            go active ty =
              case ty of
                RSTVar ref -> RSTVar (substituteRef active ref)
                RSTArrow dom cod -> RSTArrow (go active dom) (go active cod)
                RSTBase symbol -> RSTBase symbol
                RSTCon symbol args -> RSTCon symbol (fmap (go active) args)
                RSTVarApp ref args -> RSTVarApp (substituteRef active ref) (fmap (go active) args)
                RSTTyLam ref body -> RSTTyLam ref (go (withoutBinder active ref) body)
                RSTTyApp fun arg -> RSTTyApp (go active fun) (go active arg)
                RSTForall ref mbBound body ->
                  RSTForall
                    ref
                    (fmap (ResolvedSrcBound . go active . unResolvedSrcBound) mbBound)
                    (go (withoutBinder active ref) body)
                RSTMu ref body -> RSTMu ref (go (withoutBinder active ref) body)
                RSTBottom -> RSTBottom

            substituteRef active ref =
              Map.findWithDefault ref (resolvedTypeBinderTypeIdentity ref) active

            withoutBinder active ref =
              Map.delete (resolvedTypeBinderTypeIdentity ref) active

    deriveEqBody eqMethodSymbol andSymbol ownerHeadTy ctorEntries leftRef rightRef mbSelfRef initialGenerator =
      let (alts, generator1) = deriveCtorAlts initialGenerator ctorEntries
       in (P.ECase (P.EVar (P.ResolvedLocalValue leftRef)) alts, generator1)
      where
        deriveCtorAlts generator [] = ([], generator)
        deriveCtorAlts generator ((ctor, ctorSymbol, argTypes) : rest) =
          let fieldNames prefix = [prefix ++ show i | i <- [1 .. length argTypes]]
              (leftRefs, generator1) = freshLocalRefs (fieldNames "l") generator
              (rightRefs, generator2) = freshLocalRefs (fieldNames "r") generator1
              alt =
                P.Alt
                  (P.PatCtor ctorSymbol (map P.PatVar leftRefs))
                  (P.ECase (P.EVar (P.ResolvedLocalValue rightRef)) (matchingAlt ctor ctorSymbol argTypes leftRefs rightRefs : mismatchAlts ctor))
              (restAlts, generator3) = deriveCtorAlts generator2 rest
           in (alt : restAlts, generator3)

        matchingAlt _ ctorSymbol argTypes leftRefs rightRefs =
          P.Alt (P.PatCtor ctorSymbol (map P.PatVar rightRefs)) (foldEqCalls (zip3 argTypes leftRefs rightRefs))

        mismatchAlts ctor =
          [ P.Alt (P.PatCtor otherSymbol [P.PatWildcard | _ <- otherArgTypes]) (P.ELit (LBool False))
            | (other, otherSymbol, otherArgTypes) <- ctorEntries,
              resolvedSymbolIdentity (P.constructorDeclName other) /= resolvedSymbolIdentity (P.constructorDeclName ctor)
          ]

        foldEqCalls [] = P.ELit (LBool True)
        foldEqCalls [(argTy, l, r)] = eqCall argTy l r
        foldEqCalls ((argTy, l, r) : rest) =
          P.EApp
            (P.EApp (P.EVar (P.ResolvedGlobalValue andSymbol)) (eqCall argTy l r))
            (foldEqCalls rest)

        eqCall argTy l r =
          let (eqRef, annotateArgs) =
                case mbSelfRef of
                  Just selfRef | isRecursiveOwnerResolvedField ownerHeadTy argTy -> (P.ResolvedLocalValue selfRef, False)
                  _ -> (P.ResolvedGlobalValue eqMethodSymbol, True)
              field ref =
                let var = P.EVar (P.ResolvedLocalValue ref)
                 in if annotateArgs then P.EAnn var argTy else var
              left = field l
              right = field r
           in P.EApp (P.EApp (P.EVar eqRef) left) right

        freshLocalRefs :: [String] -> IdentityGenerator -> ([LocalRef], IdentityGenerator)
        freshLocalRefs names generator =
          case names of
            [] -> ([], generator)
            name : rest ->
              let (ref, generator1) = freshLocalRef name generator
                  (refs, generator2) = freshLocalRefs rest generator1
               in (ref : refs, generator2)

    dataDeclHeadResolvedType dataSymbol paramRefs =
      case paramRefs of
        [] -> RSTBase dataSymbol
        param0 : paramsRest -> RSTCon dataSymbol (RSTVar param0 :| map RSTVar paramsRest)

    isRecursiveOwnerResolvedField ownerHeadTy argTy =
      typeViewFromResolved argTy == typeViewFromResolved ownerHeadTy

    resolvedConstructorFieldTypes ty =
      fst (splitResolvedArrows (stripResolvedForalls ty))

    stripResolvedForalls (RSTForall _ _ body) = stripResolvedForalls body
    stripResolvedForalls ty = ty

    splitResolvedArrows (RSTArrow dom cod) =
      let (args, result) = splitResolvedArrows cod
       in (dom : args, result)
    splitResolvedArrows ty = ([], ty)

buildInstanceSkeletons ::
  SymbolIdentity ->
  IdentityGenerator ->
  DisplayNameEnv ->
  Scope ->
  P.ResolvedModuleSyntax ->
  [P.ResolvedInstanceDecl] ->
  TcM ([InstanceInfo], IdentityGenerator)
buildInstanceSkeletons moduleIdentity generator0 displayEnv scope mod0 derived = do
  let instances0 = derived ++ explicitInstances mod0
  (infos, generator1) <- buildInstanceInfos generator0 instances0
  case duplicateLocalInstances infos of
    info : _ -> throwError (duplicateInstanceError info)
    [] -> pure ()
  case duplicateExistingInstances infos of
    info : _ -> throwError (duplicateInstanceError info)
    [] -> pure ()
  ambiguousInstances <- ambiguousFunctionalDependencyInstances infos
  case ambiguousInstances of
    info : _ -> throwError (ambiguousFunctionalDependencyInstanceError info)
    [] -> pure ()
  case conflictingFunctionalDependencyInstances infos of
    (className0, determiners, left, right) : _ ->
      throwError (ProgramConflictingFunctionalDependency className0 (NE.toList determiners) (NE.toList left) (NE.toList right))
    [] -> pure ()
  case overlappingInstances infos of
    (left, right) : _ ->
      throwError (overlappingInstanceError left right)
    [] -> pure ()
  case overlappingWithExistingInstances infos of
    (left, right) : _ ->
      throwError (overlappingInstanceError left right)
    [] -> pure ()
  pure (infos, generator1)
  where
    buildInstanceInfos generator [] =
      pure ([], generator)
    buildInstanceInfos generator (instDecl : rest) = do
      (info, generator1) <- toInstanceInfo generator instDecl
      (infos, generator2) <- buildInstanceInfos generator1 rest
      pure (info : infos, generator2)

    toInstanceInfo generator0' instDecl = do
      classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
      instanceClassName0 <- displayClassName displayEnv (P.instanceDeclClass instDecl)
      validateResolvedClassConstraintClasses scope (P.instanceDeclConstraints instDecl)
      let instanceHeadTysResolved = P.instanceDeclTypes instDecl
      validateClassApplicationArity classInfo (length instanceHeadTysResolved)
      instanceHeadViews0 <- mapM (typeViewForDisplayEnv displayEnv) instanceHeadTysResolved
      let instanceHeadIdentityTypes0 = typeViewsIdentity instanceHeadViews0
      declaredInstanceConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.instanceDeclConstraints instDecl)
      let superclassConstraintInfos0 =
            map
              (applyConstraintInfoSubst (typeViewSubstFromParamIdentities (classParamBinderIdentities classInfo) instanceHeadViews0))
              (classSuperclassInfos classInfo)
          instanceConstraintInfos0 = declaredInstanceConstraintInfos0 ++ superclassConstraintInfos0
      let methodMapByIdentity = classMethodsByIdentity classInfo
          expected = Map.keys methodMapByIdentity
          provided = map (resolvedSymbolIdentity . P.methodDefName) (P.instanceDeclMethods instDecl)
          missingMethods =
            [ identity
            | identity <- expected
            , not (any (sameSymbolIdentity identity) provided)
            ]
          extraMethods =
            [ identity
            | identity <- provided
            , not (any (sameSymbolIdentity identity) expected)
            ]
      case missingMethods of
        (missing : _) ->
          case Map.lookup missing methodMapByIdentity of
            Just methodInfo -> throwError (ProgramMissingInstanceMethod instanceClassName0 (methodName methodInfo))
            Nothing -> throwError (ProgramMissingInstanceMethod instanceClassName0 (symbolDefiningName missing))
        [] -> pure ()
      case extraMethods of
        (extra : _) ->
          let extraName =
                case find (sameSymbolIdentity extra . resolvedSymbolIdentity . P.methodDefName) (P.instanceDeclMethods instDecl) of
                  Just methodDef -> P.refDisplayName (P.methodDefName methodDef)
                  Nothing -> symbolDefiningName extra
           in throwError (ProgramUnexpectedInstanceMethod instanceClassName0 extraName)
        [] -> pure ()
      let buildInstanceMethodEntries generator [] =
            pure ([], generator)
          buildInstanceMethodEntries generator (methodDef : rest) = do
            (entry, generator1') <- buildInstanceMethodEntry generator methodDef
            (entries, generator2') <- buildInstanceMethodEntries generator1' rest
            pure (entry : entries, generator2')

          buildInstanceMethodEntry generator methodDef = do
            methodInfo <-
              case lookupClassMethod (P.methodDefName methodDef) classInfo of
                Just info -> pure info
                Nothing -> throwError (ProgramUnexpectedInstanceMethod instanceClassName0 (P.refDisplayName (P.methodDefName methodDef)))
            let (methodIdentity, generator1') = freshIdentity generator
                methodValueView =
                  specializeMethodTypeView methodInfo instanceHeadViews0
                methodValueConstraintInfos =
                  declaredInstanceConstraintInfos0
                    ++ map
                      (applyConstraintInfoSubst (typeViewSubstFromParamIdentities (classParamBinderIdentities classInfo) instanceHeadViews0))
                      (methodConstraintInfos methodInfo)
                constrainedMethodValueView =
                  constrainedVisibleTypeView methodValueConstraintInfos methodValueView
                methodName0 = methodName methodInfo
                methodRuntimeName =
                  renderInstanceNameHead
                    (symbolIdentityStableName (classInfoSymbolIdentity classInfo))
                    instanceHeadIdentityTypes0
                    (symbolIdentityStableName (methodInfoSymbolIdentity methodInfo))
                methodValueIdentity =
                  symbolIdentityFromParts methodIdentity SymbolValue (P.moduleName mod0) methodRuntimeName Nothing
                methodValue =
                  OrdinaryValue
                    { valueInfoSymbol = methodValueIdentity,
                      valueRuntimeName = qualify (symbolDefiningModule methodValueIdentity) methodRuntimeName,
                      valueTypeView = constrainedMethodValueView,
                      valueConstraintInfos = methodValueConstraintInfos
                    }
            pure ((methodName0, methodInfo, methodValue), generator1')
      (instanceMethodEntries, generator1') <- buildInstanceMethodEntries generator0' (P.instanceDeclMethods instDecl)
      let instanceMethodInfosByIdentity =
            uniqueInfoEntriesByIdentity
              [ (methodInfoSymbolIdentity methodInfo, valueInfo)
              | (_, methodInfo, valueInfo) <- instanceMethodEntries
              ]
      pure
        ( InstanceInfo
            { instanceClassSymbol = classInfoSymbolIdentity classInfo,
              instanceOriginModuleIdentity = moduleIdentity,
              instanceConstraintInfos = instanceConstraintInfos0,
              instanceHeadTypeViews = instanceHeadViews0,
              instanceMethodsByIdentity = instanceMethodInfosByIdentity
            },
          generator1'
        )
    duplicateInstanceError info =
      case instanceHeadTypes info of
        ty :| [] -> ProgramDuplicateInstance (instanceClassName info) ty
        tys -> ProgramDuplicateInstanceHead (instanceClassName info) (NE.toList tys)

    overlappingInstanceError left right =
      case (instanceHeadTypes left, instanceHeadTypes right) of
        (leftTy :| [], rightTy :| []) -> ProgramOverlappingInstance (instanceClassName left) leftTy rightTy
        (leftTys, rightTys) -> ProgramOverlappingInstanceHead (instanceClassName left) (NE.toList leftTys) (NE.toList rightTys)

    ambiguousFunctionalDependencyInstanceError info =
      ProgramAmbiguousFunctionalDependencyInstance (instanceClassName info) (NE.toList (instanceHeadTypes info))

    ambiguousFunctionalDependencyInstances :: [InstanceInfo] -> TcM [InstanceInfo]
    ambiguousFunctionalDependencyInstances infos =
      concat <$> mapM ambiguousFunctionalDependencyInstance infos

    ambiguousFunctionalDependencyInstance :: InstanceInfo -> TcM [InstanceInfo]
    ambiguousFunctionalDependencyInstance info =
      concat
        <$> traverse
          (ambiguousFunctionalDependencyFor info)
          [ (classInfo, fundep, indices)
          | Just classInfo <- [classInfoForInstance info],
            fundep <- classFunctionalDependencies classInfo,
            Just indices <- [functionalDependencyIndices classInfo fundep]
          ]

    ambiguousFunctionalDependencyFor ::
      InstanceInfo ->
      (ClassInfo, FunctionalDependencyInfo, (NonEmpty Int, NonEmpty Int)) ->
      TcM [InstanceInfo]
    ambiguousFunctionalDependencyFor info (_, _, (determinerIndices, determinedIndices)) = do
      determinerVars <- freeProjectedTypeBinderIdentities determinerIndices (instanceHeadTypeViews info)
      determinedVars <- freeProjectedTypeBinderIdentities determinedIndices (instanceHeadTypeViews info)
      pure [info | not (determinedVars `Set.isSubsetOf` determinerVars)]

    conflictingFunctionalDependencyInstances infos =
      [ conflict
        | (ix, left) <- zip [(0 :: Int) ..] infos,
          right <- drop (ix + 1) infos,
          sameInstanceClass left right,
          Just conflict <- [functionalDependencyConflict left right]
      ]
        ++ [ conflict
             | local <- infos,
               existing <- scopeInstances scope,
               sameInstanceClass local existing,
               Just conflict <- [functionalDependencyConflict local existing]
           ]

    functionalDependencyConflict left right = do
      classInfo <- classInfoForInstance left
      firstJust
        [ conflictForFundep classInfo fundep left right
          | fundep <- classFunctionalDependencies classInfo
        ]

    conflictForFundep classInfo fundep left right = do
      (determinerIndices, determinedIndices) <- functionalDependencyIndices classInfo fundep
      let leftDeterminers = projectInstanceTypes determinerIndices (instanceHeadTypes left)
          leftDetermined = projectInstanceTypes determinedIndices (instanceHeadTypes left)
          rightDetermined = projectInstanceTypes determinedIndices (instanceHeadTypes right)
          leftDeterminerViews = projectInstanceTypes determinerIndices (instanceHeadTypeViews left)
          rightDeterminerViews = projectInstanceTypes determinerIndices (instanceHeadTypeViews right)
          leftDeterminedViews = projectInstanceTypes determinedIndices (instanceHeadTypeViews left)
          rightDeterminedViews = projectInstanceTypes determinedIndices (instanceHeadTypeViews right)
      if functionalDependencyHeadsConflict leftDeterminerViews rightDeterminerViews leftDeterminedViews rightDeterminedViews
        then Just (className classInfo, leftDeterminers, leftDetermined, rightDetermined)
        else Nothing

    functionalDependencyHeadsConflict leftDeterminers rightDeterminers leftDetermined rightDetermined =
      case unifyTaggedProjectionViews leftDeterminers rightDeterminers leftDetermined rightDetermined of
        Just True -> False
        Just False -> True
        Nothing -> False

    unifyTaggedProjectionViews leftDeterminers rightDeterminers leftDetermined rightDetermined = do
      let leftDeterminerTypes = map (overlapTypeView OverlapLeft) (NE.toList leftDeterminers)
          rightDeterminerTypes = map (overlapTypeView OverlapRight) (NE.toList rightDeterminers)
          leftDeterminedTypes = map (overlapTypeView OverlapLeft) (NE.toList leftDetermined)
          rightDeterminedTypes = map (overlapTypeView OverlapRight) (NE.toList rightDetermined)
      determinerSubst <- unifyProjectionTypes Map.empty leftDeterminerTypes rightDeterminerTypes
      case unifyProjectionTypes determinerSubst leftDeterminedTypes rightDeterminedTypes of
        Just _ -> Just True
        Nothing -> Just False

    unifyProjectionTypes =
      unifyOverlapLists

    freeProjectedTypeBinderIdentities :: NonEmpty Int -> NonEmpty TypeView -> TcM (Set.Set TypeBinderIdentity)
    freeProjectedTypeBinderIdentities indices views =
      pure $
        Set.unions
          (map freeTypeBinderIdentitiesTypeView (NE.toList (projectInstanceTypes indices views)))

    functionalDependencyIndices classInfo fundep =
      (,) <$> traverse lookupParamIndex (functionalDependencyDeterminerRefs fundep) <*> traverse lookupParamIndex (functionalDependencyDeterminedRefs fundep)
      where
        paramIndices =
          Map.fromList
            [ (identity, ix)
            | (identity, ix) <- zip (NE.toList (classParamBinderIdentities classInfo)) [(0 :: Int) ..]
            ]
        lookupParamIndex identity = Map.lookup identity paramIndices

    projectInstanceTypes :: (Functor f) => f Int -> NonEmpty a -> f a
    projectInstanceTypes indices tys =
      let values = NE.toList tys
       in fmap (values !!) indices

    classInfoForInstance info =
      case lookupSymbolIdentityExact (instanceInfoClassSymbolIdentity info) (scopeClassesByIdentity scope) of
        Just classInfo -> Just classInfo
        Nothing -> Nothing

    firstJust [] = Nothing
    firstJust (mbValue : rest) =
      case mbValue of
        Just value -> Just value
        Nothing -> firstJust rest

    overlappingInstances infos =
      [ (left, right)
        | (ix, left) <- zip [(0 :: Int) ..] infos,
          right <- drop (ix + 1) infos,
          sameInstanceClass left right,
          instanceHeadTypeViews left /= instanceHeadTypeViews right,
          instanceHeadViewsOverlap (instanceHeadTypeViews left) (instanceHeadTypeViews right)
      ]

    duplicateLocalInstances infos =
      [ left
        | (ix, left) <- zip [(0 :: Int) ..] infos,
          right <- drop (ix + 1) infos,
          sameCanonicalInstanceHead left right
      ]

    duplicateExistingInstances infos =
      [ local
        | local <- infos,
          existing <- scopeInstances scope,
          sameCanonicalInstanceHead local existing
      ]

    overlappingWithExistingInstances infos =
      [ (local, existing)
        | local <- infos,
          existing <- scopeInstances scope,
          sameInstanceClass local existing,
          instanceHeadTypeViews local /= instanceHeadTypeViews existing,
          instanceHeadViewsOverlap (instanceHeadTypeViews local) (instanceHeadTypeViews existing)
      ]

    sameInstanceClass left right =
      instanceClassIdentity left == instanceClassIdentity right

    sameCanonicalInstanceHead left right =
      sameInstanceClass left right
        && instanceHeadTypeViews left == instanceHeadTypeViews right

    instanceHeadViewsOverlap left right =
      NE.length left == NE.length right
        && case
          unifyOverlapLists
            Map.empty
            (map (overlapTypeView OverlapLeft) (NE.toList left))
            (map (overlapTypeView OverlapRight) (NE.toList right))
        of
          Just _ -> True
          Nothing -> False

renderInstanceNameHead :: P.ClassName -> NonEmpty SrcType -> P.MethodName -> String
renderInstanceNameHead className0 headTys methodName0 =
  intercalate "__" (sanitizeName className0 : map sanitizeType (NE.toList headTys) ++ [sanitizeName methodName0])

sanitizeType :: SrcType -> String
sanitizeType = \case
  STVar v -> sanitizeName v
  STArrow dom cod -> "arr_" ++ sanitizeType dom ++ "_" ++ sanitizeType cod
  STBase base -> sanitizeName base
  STCon con args -> intercalate "_" (sanitizeName con : map sanitizeType (NE.toList args))
  STVarApp name args -> intercalate "_" (sanitizeName name : map sanitizeType (NE.toList args))
  STTyLam v body -> "tylam_" ++ sanitizeName v ++ "_" ++ sanitizeType body
  STTyApp fun arg -> "tyapp_" ++ sanitizeType fun ++ "_" ++ sanitizeType arg
  STForall v _ body -> "forall_" ++ sanitizeName v ++ "_" ++ sanitizeType body
  STMu v body -> "mu_" ++ sanitizeName v ++ "_" ++ sanitizeType body
  STBottom -> "bottom"
sanitizeName :: String -> String
sanitizeName = concatMap sanitizeNameChar

sanitizeNameChar :: Char -> String
sanitizeNameChar c
  | c `elem` ['a' .. 'z'] = [c]
  | c `elem` ['A' .. 'Z'] = [c]
  | c `elem` ['0' .. '9'] = [c]
  | otherwise = "_u" ++ show (fromEnum c) ++ "_"

checkDefs :: IdentityGenerator -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedDefDecl] -> TcM ([CheckedBinding], IdentityGenerator)
checkDefs generator0 finalizeContext elaborateScope scope =
  go generator0 []
  where
    go generator acc [] = pure (reverse acc, generator)
    go generator acc (defDecl : rest) = do
      (checked, generator') <- checkDef generator finalizeContext elaborateScope scope defDecl
      go generator' (checked : acc) rest

checkDef :: IdentityGenerator -> FinalizeContext -> ElaborateScope -> Scope -> P.ResolvedDefDecl -> TcM (CheckedBinding, IdentityGenerator)
checkDef generator finalizeContext elaborateScope scope defDecl = do
  let defName = P.refDisplayName (P.defDeclName defDecl)
  valueInfo <- lookupValueInfoBySymbol scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      (lowered, generator') <-
        liftEither
          ( lowerResolvedConstrainedExprBindingWithGenerator
              generator
              elaborateScope
              (loweredBindingIdentityFromValueInfo ordinary)
              (P.defDeclType defDecl)
              (resolvedDefDeclIsMain defDecl)
              (P.defDeclExpr defDecl)
          )
      liftEither (finalizeBindingAllowOpaqueWithContextFromSupply generator' finalizeContext lowered)
    _ -> throwError (ProgramDuplicateValue defName)

buildExports :: P.ResolvedModuleSyntax -> Map String DataInfo -> Map String ClassInfo -> Map String ValueInfo -> TcM ModuleExports
buildExports mod0 localData localClasses localValues = do
  let exportItems = P.moduleExports mod0
      defaultValues = Map.filter (\info -> case info of OverloadedMethod {} -> True; ConstructorValue {} -> True; OrdinaryValue {} -> True) localValues
      defaultTypes = Map.fromList [(name, mkExportedTypeInfo info []) | (name, info) <- Map.toList localData]
      defaultClasses = localClasses
      localValuesByIdentity = identityExportIndex valueInfoSymbolIdentity localValues
      localDataByIdentity = identityExportIndex dataInfoSymbolIdentity localData
      localClassesByIdentity = identityExportIndex classInfoSymbolIdentity localClasses
  case exportItems of
    Nothing ->
      pure
        (moduleExportsFromMaps defaultValues defaultTypes defaultClasses)
    Just items -> do
      values <- foldM (collectResolvedExportValue localValuesByIdentity localClassesByIdentity localDataByIdentity) Map.empty items
      types <- foldM (collectResolvedExportType (P.moduleName mod0) localDataByIdentity) Map.empty items
      classes <- foldM (collectResolvedExportClass localClassesByIdentity) Map.empty items
      pure
        ModuleExports
          { exportedValuesByIdentity = selectedExportInfos values,
            exportedValueDisplaysByIdentity = selectedExportDisplays values,
            exportedTypesByIdentity = selectedExportInfos types,
            exportedTypeDisplaysByIdentity = selectedExportDisplays types,
            exportedClassesByIdentity = selectedExportInfos classes,
            exportedClassDisplaysByIdentity = selectedExportDisplays classes
          }

type IdentityExportIndex a = (Map SymbolIdentity a, Map SymbolIdentity String)

identityExportIndex :: (Eq a) => (a -> SymbolIdentity) -> Map String a -> IdentityExportIndex a
identityExportIndex identityFor values =
  ( uniqueInfoByIdentity identityFor values,
    uniqueDisplayByIdentity identityFor values
  )

lookupIdentityExport :: SymbolIdentity -> IdentityExportIndex a -> Maybe (String, a)
lookupIdentityExport identity (infos, displays) =
  (,) <$> lookupSymbolIdentityExact identity displays <*> lookupSymbolIdentityExact identity infos

type SelectedExports a = Map SymbolIdentity (String, a)

selectedExportInfos :: SelectedExports a -> Map SymbolIdentity a
selectedExportInfos =
  fmap snd

selectedExportDisplays :: SelectedExports a -> Map SymbolIdentity String
selectedExportDisplays =
  fmap fst

insertSelectedExport :: (a -> SymbolIdentity) -> String -> a -> SelectedExports a -> Either ProgramError (SelectedExports a)
insertSelectedExport identityFor displayName info acc =
  case lookupSymbolIdentityExact identity acc of
    Just {} -> Right acc
    Nothing
      | displayName `elem` map (fst . snd) (Map.toList acc) ->
          Left (ProgramDuplicateVisibleName displayName)
      | otherwise ->
          Right (Map.insert identity (displayName, info) acc)
  where
    identity = identityFor info

insertSelectedExports :: (a -> SymbolIdentity) -> SelectedExports a -> [(String, a)] -> Either ProgramError (SelectedExports a)
insertSelectedExports identityFor =
  foldM (\acc (displayName, info) -> insertSelectedExport identityFor displayName info acc)

collectResolvedExportValue :: IdentityExportIndex ValueInfo -> IdentityExportIndex ClassInfo -> IdentityExportIndex DataInfo -> SelectedExports ValueInfo -> P.ResolvedExportItem -> TcM (SelectedExports ValueInfo)
collectResolvedExportValue localValues localClasses localData acc = \case
  P.ExportValue symbol ->
    case lookupIdentityExport (resolvedSymbolIdentity symbol) localValues of
      Just (name, info) -> liftEither (insertSelectedExport valueInfoSymbolIdentity name info acc)
      Nothing -> throwError (ProgramExportNotLocal (resolvedSymbolDisplayName symbol))
  P.ExportTypeWithConstructors ref ->
    case localDataByRef ref localData of
      Just (_, dataInfo) ->
        liftEither
          ( insertSelectedExports
              valueInfoSymbolIdentity
              acc
              [ ( ctorName ctor,
                  ConstructorValue
                    { valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
                      valueRuntimeName = constructorInfoRuntimeName ctor,
                      valueCtorInfo = ctor
                    }
                )
              | ctor <- dataConstructors dataInfo
              ]
          )
      Nothing -> throwError (ProgramExportNotLocal (P.resolvedExportTypeName ref))
  P.ExportType ref ->
    case localClassByRef ref localClasses of
      Just (_, classInfo) ->
        liftEither
          ( insertSelectedExports
              valueInfoSymbolIdentity
              acc
              [ ( methodName method,
                  OverloadedMethod
                    { valueInfoSymbol = methodInfoSymbolIdentity method,
                      valueMethodInfo = method
                    }
                )
              | method <- Map.elems (classMethodsByIdentity classInfo)
              ]
          )
      Nothing -> pure acc

collectResolvedExportType :: P.ModuleName -> IdentityExportIndex DataInfo -> SelectedExports ExportedTypeInfo -> P.ResolvedExportItem -> TcM (SelectedExports ExportedTypeInfo)
collectResolvedExportType moduleName0 localData acc = \case
  P.ExportType ref ->
    case localDataByRef ref localData of
      Just (typeName, dataInfo) ->
        liftEither (insertSelectedExport exportedTypeInfoIdentity typeName (mkExportedTypeInfo dataInfo []) acc)
      Nothing
        | moduleName0 == "Prelude",
          Just dataInfo <- builtinOpaqueDataByRef ref ->
            liftEither (insertSelectedExport exportedTypeInfoIdentity (P.resolvedExportTypeName ref) (mkExportedTypeInfo dataInfo []) acc)
      Nothing -> pure acc
  P.ExportTypeWithConstructors ref ->
    case localDataByRef ref localData of
      Just (typeName, dataInfo) ->
        liftEither
          ( insertSelectedExport
              exportedTypeInfoIdentity
              typeName
              (mkExportedTypeInfo dataInfo [(ctorName ctor, ctor) | ctor <- dataConstructors dataInfo])
              acc
          )
      Nothing -> throwError (ProgramExportNotLocal (P.resolvedExportTypeName ref))
  P.ExportValue _ -> pure acc

exportedTypeInfoIdentity :: ExportedTypeInfo -> SymbolIdentity
exportedTypeInfoIdentity =
  dataInfoSymbolIdentity . exportedTypeData

builtinOpaqueDataByRef :: P.ResolvedExportTypeRef -> Maybe DataInfo
builtinOpaqueDataByRef ref =
  case
    [ dataInfo
      | symbol <- P.resolvedExportTypeSymbols ref,
        Just dataInfo <- [lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) builtinOpaqueTypesByIdentity]
    ]
  of
    dataInfo : _ -> Just dataInfo
    [] -> Nothing
  where
    builtinOpaqueTypesByIdentity =
      Map.fromList
        [ (dataInfoSymbolIdentity dataInfo, dataInfo)
        | dataInfo <- Map.elems Builtins.builtinOpaqueTypes
        ]

collectResolvedExportClass :: IdentityExportIndex ClassInfo -> SelectedExports ClassInfo -> P.ResolvedExportItem -> TcM (SelectedExports ClassInfo)
collectResolvedExportClass localClasses acc = \case
  P.ExportType ref ->
    case localClassByRef ref localClasses of
      Just (className0, classInfo) -> liftEither (insertSelectedExport classInfoSymbolIdentity className0 classInfo acc)
      Nothing -> pure acc
  _ -> pure acc

localDataByRef :: P.ResolvedExportTypeRef -> IdentityExportIndex DataInfo -> Maybe (String, DataInfo)
localDataByRef ref localData =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolType] of
    symbol : _ -> lookupIdentityExport (resolvedSymbolIdentity symbol) localData
    [] -> Nothing

localClassByRef :: P.ResolvedExportTypeRef -> IdentityExportIndex ClassInfo -> Maybe (String, ClassInfo)
localClassByRef ref localClasses =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolClass] of
    symbol : _ -> lookupIdentityExport (resolvedSymbolIdentity symbol) localClasses
    [] -> Nothing

-- Helpers --------------------------------------------------------------------

qualify :: P.ModuleName -> String -> String
qualify moduleName0 name = moduleName0 ++ "__" ++ name

ensureDistinctBy :: (Ord a) => (a -> ProgramError) -> (b -> a) -> [b] -> TcM ()
ensureDistinctBy mkErr project values = ensureDistinctPlain mkErr (map project values)

ensureDistinctModuleIdentities :: String -> (a -> SymbolIdentity) -> [a] -> TcM ()
ensureDistinctModuleIdentities label project =
  ensureDistinctSymbolIdentities label . map project

ensureDistinctResolvedModuleSymbolIdentities :: ResolvedSemanticModule -> TcM ()
ensureDistinctResolvedModuleSymbolIdentities resolvedModule =
  ensureDistinctSymbolIdentities "resolved symbol" (resolvedSemanticModuleSymbolIdentities resolvedModule)

resolvedSemanticModuleSymbolIdentities :: ResolvedSemanticModule -> [SymbolIdentity]
resolvedSemanticModuleSymbolIdentities resolvedModule =
  resolvedSemanticModuleIdentity resolvedModule : concatMap declIdentities (P.moduleDecls syntax)
  where
    syntax =
      resolvedSemanticModuleSyntax resolvedModule

    declIdentities decl =
      case decl of
        P.DeclClass classDecl ->
          resolvedSymbolIdentity (P.classDeclName classDecl)
            : [resolvedSymbolIdentity (P.methodSigName methodSig) | methodSig <- P.classDeclMethods classDecl]
        P.DeclInstance {} ->
          []
        P.DeclData dataDecl ->
          resolvedSymbolIdentity (P.dataDeclName dataDecl)
            : [resolvedSymbolIdentity (P.constructorDeclName constructor) | constructor <- P.dataDeclConstructors dataDecl]
        P.DeclTypeFamily {} ->
          []
        P.DeclDef defDecl ->
          [resolvedSymbolIdentity (P.defDeclName defDecl)]

ensureDistinctSymbolIdentities :: String -> [SymbolIdentity] -> TcM ()
ensureDistinctSymbolIdentities label =
  go Map.empty
  where
    go _ [] =
      pure ()
    go seen (identity : rest)
      | Just existing <- Map.lookup (symbolUniqueIdentity identity) seen =
          if symbolIdentityPayloadKey existing == symbolIdentityPayloadKey identity
            then throwError (duplicateIdentityError identity)
            else throwError (conflictingIdentityPayloadError identity)
      | otherwise =
          go (Map.insert (symbolUniqueIdentity identity) identity seen) rest

    duplicateIdentityError identity =
      ProgramPipelineError ("duplicate " ++ label ++ " identity: " ++ symbolIdentityStableName identity)

    conflictingIdentityPayloadError identity =
      ProgramPipelineError ("conflicting " ++ label ++ " identity payload: " ++ symbolIdentityStableName identity)

ensureDistinctImportAliases :: [P.ImportF p] -> TcM ()
ensureDistinctImportAliases imports0 =
  ensureDistinctPlain ProgramDuplicateImportAlias [alias | Just alias <- map P.importAlias imports0]

ensureDistinctPlain :: (Ord a) => (a -> ProgramError) -> [a] -> TcM ()
ensureDistinctPlain mkErr =
  go Set.empty
  where
    go _ [] =
      pure ()
    go seen (value : rest)
      | value `Set.member` seen = throwError (mkErr value)
      | otherwise = go (Set.insert value seen) rest

duplicates :: (Ord a) => [a] -> [a]
duplicates =
  go Set.empty Set.empty
  where
    go _ _ [] =
      []
    go seen reported (value : rest)
      | value `Set.member` seen =
          if value `Set.member` reported
            then go seen reported rest
            else value : go seen (Set.insert value reported) rest
      | otherwise =
          go (Set.insert value seen) reported rest

mergeMaps :: (String -> ProgramError) -> Map String a -> Map String a -> TcM (Map String a)
mergeMaps mkErr base incoming =
  foldM
    ( \acc (name, value) ->
        if Map.member name acc
          then throwError (mkErr name)
          else pure (Map.insert name value acc)
    )
    base
    (Map.toList incoming)

liftEither :: Either ProgramError a -> TcM a
liftEither = either throwError pure
