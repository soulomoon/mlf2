{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Check
  ( ProgramError (..),
    ProgramDiagnostic (..),
    CheckedProgram (..),
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
    loweredBindingIdentityFromDetails,
    ResolvedVar (..),
    ExportedTypeInfo (..),
    ModuleExports (..),
    checkProgram,
    checkProgramPackage,
    checkResolvedProgram,
    checkLocatedProgram,
    checkLocatedProgramPackage,
    checkLocatedProgramPackageWithTiming,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM, when, zipWithM)
import Control.Monad.Except (MonadError (throwError))
import Data.Char (isAlphaNum)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, intercalate, nub, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    lowerConstructorBinding,
    lowerConstrainedResolvedExprBinding,
    lowerResolvedConstrainedExprBinding,
    mkElaborateScope,
    resolveInstanceInfoWithIdentityType,
  )
import MLF.Frontend.Program.Finalize
  ( FinalizeContext,
    ModuleFinalizeContext,
    finalizeBindingsAllowOpaqueWithContext,
    finalizeBindingsAllowOpaqueWithContextWithTiming,
    finalizeBindingAllowOpaqueWithModuleContext,
    finalizeBindingAllowOpaqueWithModuleContextWithTiming,
    finalizeBindingLayerAllowOpaqueWithModuleContext,
    finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming,
    finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming,
    finalizeBindingWithContext,
    finalizeBindingAllowOpaqueWithContext,
    finalizeBindingAllowOpaqueWithContextWithTiming,
    mkFinalizeContext,
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
    programPackageModuleGraph,
    programPackageOrderedProgram,
    trivialPackageId,
    trivialLocatedProgramPackage,
    trivialProgramPackage,
  )
import MLF.Frontend.Program.Resolve (resolveProgram)
import MLF.Frontend.Symbol (symbolIdentityAliasNames, symbolIdentityStableName)
import MLF.Frontend.Program.TypeFamilies (normalizeTypeFamiliesInProgram)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ClassInfo (..),
    ConstructorForallBinder (..),
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
    TypeView (..),
    ValueInfo (..),
    applyTypeHead,
    applyConstraintInfoSubst,
    checkedBindingName,
    constructorRefFromSymbol,
    constructorRefSymbol,
    constraintTypeView,
    classInfoIdentityModule,
    className,
    classParamBinderIdentities,
    classParamNames,
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
    instanceHeadIdentityTypes,
    loweredBindingIdentityFromDetails,
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
    methodType,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    mergeSymbolIdentityMaps,
    mergeTypeBinderIdentityMaps,
    mkExportedTypeInfo,
    moduleExportsFromMaps,
    uniqueDisplayByIdentity,
    exportedClassesForDisplay,
    exportedTypesForDisplay,
    exportedTypeConstructorsForDisplay,
    exportedValuesForDisplay,
    mkResolvedSymbol,
    resolvedSymbolIdentity,
    resolvedProgramSemanticArtifact,
    resolvedProgramGeneratedIdentities,
    resolvedModuleGeneratedIdentities,
    resolvedDeclGeneratedIdentities,
    resolvedTypeBinderGeneratedIdentities,
    symbolDefiningModule,
    symbolDefiningName,
    symbolIdentityFromParts,
    symbolNamespace,
    symbolOwnerIdentity,
    displayConstraint,
    specializeMethodTypeView,
    substituteTypeVar,
    splitArrows,
    splitForalls,
    typeBinderAliasIdentityMap,
    typeParamBinderIdentity,
    typeViewBinderIdentityForAlias,
    typeViewHeadIdentityForAlias,
    typeViewFromResolved,
    typeViewMentionedHeadIdentities,
    typeViewsDisplay,
    typeViewsIdentity,
    typeViewSubstFromParamIdentities,
    ordinaryValueTypeView,
    primitiveRefFromSymbol,
    primitiveRefSymbol,
    valueInfoSymbolIdentity,
  )
import MLF.Util.Timing
  ( TimingConfig,
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
    resolvedTypeBinderTypeIdentity,
  )
import qualified MLF.Frontend.Syntax.Program as P
import System.IO.Unsafe (unsafePerformIO)
import MLF.Frontend.TypeLevel (TypeFamilyDecl, familyDeclName)
import MLF.Types.Identity
  ( IdentityGenerator,
    TypeBinderIdentity,
    UniqueIdentity (..),
    freshIdentity,
    freshLocalRef,
    identityGeneratorAfter,
  )

type TcM a = Either ProgramError a

runTcM :: TcM a -> Either ProgramError a
runTcM = id

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

indexByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity a
indexByIdentity identityOf =
  Map.fromList . map (\info -> (identityOf info, info)) . Map.elems

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
    { dneValues = Map.unionWith (++) (dneValues preferred) (dneValues fallback),
      dneTypes = Map.unionWith (++) (dneTypes preferred) (dneTypes fallback),
      dneClasses = Map.unionWith (++) (dneClasses preferred) (dneClasses fallback)
    }

displayNameEnvFromScope :: Scope -> DisplayNameEnv
displayNameEnvFromScope scope =
  DisplayNameEnv
    { dneValues =
        Map.fromListWith (++)
          [ (valueInfoSymbolIdentity info, [name])
            | (name, info) <- Map.toList (scopeValues scope)
          ],
      dneTypes =
        Map.fromListWith (++)
          [ (dataInfoSymbolIdentity info, [name])
            | (name, info) <- Map.toList (scopeTypes scope)
          ],
      dneClasses =
        Map.fromListWith (++)
          [ (classInfoSymbolIdentity info, [name])
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
      Map.fromListWith (++)
        [ (resolvedSymbolIdentity symbol, [name])
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
        Map.fromListWith (++)
          [ (ctorInfoSymbol ctor, [ctorName ctor])
            | dataInfo <- Map.elems dataInfos,
              ctor <- dataConstructors dataInfo
          ],
      dneTypes =
        Map.fromListWith (++)
          [ (dataInfoSymbolIdentity dataInfo, [name])
            | (name, dataInfo) <- Map.toList dataInfos
          ]
    }

displayNameEnvFromClasses :: Map String ClassInfo -> DisplayNameEnv
displayNameEnvFromClasses classInfos =
  emptyDisplayNameEnv
    { dneValues =
        Map.fromListWith (++)
          [ (methodInfoSymbolIdentity methodInfo, [methodName methodInfo])
            | classInfo <- Map.elems classInfos,
              methodInfo <- Map.elems (classMethodsByIdentity classInfo)
          ],
      dneClasses =
        Map.fromListWith (++)
          [ (classInfoSymbolIdentity classInfo, [name])
            | (name, classInfo) <- Map.toList classInfos
          ]
    }

displayNameEnvFromValues :: Map String ValueInfo -> DisplayNameEnv
displayNameEnvFromValues values0 =
  emptyDisplayNameEnv
    { dneValues =
        Map.fromListWith (++)
          [ (valueInfoSymbolIdentity valueInfo, [name])
            | (name, valueInfo) <- Map.toList values0
          ]
    }

checkedDataByIdentity :: Map String DataInfo -> Map SymbolIdentity DataInfo
checkedDataByIdentity =
  Map.fromList . map (\dataInfo -> (dataInfoSymbolIdentity dataInfo, dataInfo)) . Map.elems

checkedClassesByIdentity :: Map String ClassInfo -> Map SymbolIdentity ClassInfo
checkedClassesByIdentity =
  Map.fromList . map (\classInfo -> (classInfoSymbolIdentity classInfo, classInfo)) . Map.elems

addValues :: Map String ValueInfo -> Map String ValueInfo -> Either ProgramError (Map String ValueInfo)
addValues base incoming =
  foldM
    ( \acc (name, info) ->
        case Map.lookup name acc of
          Just existing
            | valueInfoSymbolIdentity existing == valueInfoSymbolIdentity info -> Right acc
            | otherwise -> Left (ProgramDuplicateVisibleName name)
          Nothing -> Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

addTypes :: Map String DataInfo -> Map String DataInfo -> Either ProgramError (Map String DataInfo)
addTypes base incoming =
  foldM
    ( \acc (name, info) ->
        case Map.lookup name acc of
          Just existing
            | dataInfoSymbolIdentity existing == dataInfoSymbolIdentity info -> Right acc
            | otherwise -> Left (ProgramDuplicateVisibleName name)
          Nothing -> Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

addClasses :: Map String ClassInfo -> Map String ClassInfo -> Either ProgramError (Map String ClassInfo)
addClasses base incoming =
  foldM
    ( \acc (name, info) ->
        case Map.lookup name acc of
          Just existing
            | classInfoSymbolIdentity existing == classInfoSymbolIdentity info -> Right acc
            | otherwise -> Left (ProgramDuplicateVisibleName name)
          Nothing -> Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

lookupValueInfoBySymbol :: Scope -> ResolvedSymbol -> TcM ValueInfo
lookupValueInfoBySymbol scope symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) (scopeValuesByIdentity scope) of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownValue (resolvedSymbolDisplayName symbol))

lookupClassInfoBySymbol :: Scope -> ResolvedSymbol -> TcM ClassInfo
lookupClassInfoBySymbol scope symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) (scopeClassesByIdentity scope) of
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
checkResolvedProgramWithContext mbGraph resolved = runTcM $ do
  checkedProgram <- checkResolvedProgramCore mbGraph resolved
  case mbGraph of
    Nothing -> pure ()
    Just graph -> validateCheckedPackageInterface graph checkedProgram
  pure checkedProgram

checkResolvedProgramCore :: Maybe PackageModuleGraph -> ResolvedProgram -> TcM CheckedProgram
checkResolvedProgramCore mbGraph resolved = do
  modulesChecked <- checkModules mbGraph (resolvedProgramSemanticArtifact resolved)
  checkedProgramFromCheckedModules resolved modulesChecked

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
  pure (checkedProgramFromModules resolved modulesChecked (checkedBindingResolvedVar mainBinding))

checkLocatedProgram :: P.LocatedProgram -> Either ProgramDiagnostic CheckedProgram
checkLocatedProgram located =
  checkLocatedProgramPackage (trivialLocatedProgramPackage located)

checkLocatedProgramPackage :: LocatedProgramPackage -> Either ProgramDiagnostic CheckedProgram
checkLocatedProgramPackage package =
  case (locatedProgramPackageModuleGraph package, locatedProgramPackageOrderedProgram package) of
    (Left err, _) -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
    (_, Left err) -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
    (Right graph, Right orderedProgram) ->
      case do
        normalized <- normalizeTypeFamiliesInProgram (P.locatedProgram orderedProgram)
        rejectUnsupportedGeneralizedClassFeatures normalized
        resolved <- resolveProgram normalized
        checkResolvedProgramWithPackageGraph graph resolved of
        Right checked -> Right checked
        Left err -> Left (diagnosticForProgramError (Just orderedProgram) err)

checkLocatedProgramPackageWithTiming :: TimingConfig -> LocatedProgramPackage -> IO (Either ProgramDiagnostic CheckedProgram)
checkLocatedProgramPackageWithTiming timing package = do
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
                          (checkResolvedProgramCoreWithTiming timing (Just graph) resolved)
                      case checkedResult of
                        Left err ->
                          pure (Left (diagnosticForProgramError (Just orderedProgram) err))
                        Right checked -> do
                          interfaceResult <-
                            timeProgramIO
                              timing
                              "program.check.package-interface"
                              (evaluate (runTcM (validateCheckedPackageInterface graph checked)))
                          pure $
                            case interfaceResult of
                              Left err -> Left (diagnosticForProgramError (Just orderedProgram) err)
                              Right () -> Right checked

checkResolvedProgramCoreWithTiming :: TimingConfig -> Maybe PackageModuleGraph -> ResolvedProgram -> IO (Either ProgramError CheckedProgram)
checkResolvedProgramCoreWithTiming timing mbGraph resolved = do
  modulesResult <-
    checkModulesWithTiming
      timing
      mbGraph
      (resolvedProgramSemanticArtifact resolved)
  case modulesResult of
    Left err ->
      pure (Left err)
    Right modulesChecked ->
      timeProgramDetailIO
        timing
        "program.check.modules.main-binding"
        (evaluate (runTcM (checkedProgramFromCheckedModules resolved modulesChecked)))

checkedProgramFromModules :: ResolvedProgram -> [CheckedModule] -> ResolvedVar -> CheckedProgram
checkedProgramFromModules resolved modulesChecked mainResolved =
  CheckedProgram
    { checkedProgramModules = modulesChecked,
      checkedProgramMainResolvedVar = mainResolved,
      checkedProgramResolved = resolved
    }

validateCheckedPackageInterface :: PackageModuleGraph -> CheckedProgram -> TcM ()
validateCheckedPackageInterface graph checked =
  liftEitherWithInterface (packageInterfaceFromCheckedProgram graph checked) >> pure ()

liftEitherWithInterface :: Either ProgramInterfaceError a -> TcM a
liftEitherWithInterface =
  either (throwError . ProgramPipelineError . interfaceErrorMessage) pure
  where
    interfaceErrorMessage err =
      "invalid .mlfp interface artifact: " ++ renderProgramInterfaceError err

checkModules :: Maybe PackageModuleGraph -> ResolvedSemanticProgramArtifact -> TcM [CheckedModule]
checkModules mbGraph artifact@(ResolvedSemanticProgramArtifact resolvedModules) = do
  ensureDistinctBy ProgramDuplicateModule resolvedSemanticModuleName resolvedModules
  go (resolvedProgramIdentityGenerator artifact) [] [] resolvedModules
  where
    nodesByModule =
      Map.fromList
        [ (packageModuleName (packageModuleGraphNodeId node), node)
          | graph <- maybe [] pure mbGraph,
            node <- packageModuleGraphNodes graph
        ]

    go _ _ checkedAcc [] = pure (reverse checkedAcc)
    go generator0 interfaceAcc checkedAcc (resolvedModule : rest) = do
      (checked, generator1) <-
        if isBuiltinPreludeModule nodesByModule mbGraph resolvedModule
          then checkedBuiltinPreludeModule generator0 resolvedModule
          else checkModule generator0 resolvedModule interfaceAcc
      node <- moduleInterfaceNodeForResolved nodesByModule mbGraph resolvedModule
      interface <- liftEitherWithInterface (moduleInterfaceFromCheckedModule node checked)
      go generator1 (interface : interfaceAcc) (checked : checkedAcc) rest

checkModulesWithTiming :: TimingConfig -> Maybe PackageModuleGraph -> ResolvedSemanticProgramArtifact -> IO (TcM [CheckedModule])
checkModulesWithTiming timing mbGraph artifact@(ResolvedSemanticProgramArtifact resolvedModules) = do
  distinctResult <-
    timeProgramDetailIO
      timing
      "program.check.modules.distinct"
      (evaluate (ensureDistinctBy ProgramDuplicateModule resolvedSemanticModuleName resolvedModules))
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
              (evaluate (checkedBuiltinPreludeModule generator0 resolvedModule))
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

builtinPreludeSourcePath :: FilePath
builtinPreludeSourcePath = "<mlfp-prelude>"

builtinPreludeCheckCache :: MVar (Maybe BuiltinPreludeCheckCacheEntry)
builtinPreludeCheckCache = unsafePerformIO (newMVar Nothing)
{-# NOINLINE builtinPreludeCheckCache #-}

type BuiltinPreludeCheckCacheEntry =
  ( IdentityGenerator,
    [UniqueIdentity],
    TcM (CheckedModule, IdentityGenerator)
  )

checkedBuiltinPreludeModule :: IdentityGenerator -> ResolvedSemanticModule -> TcM (CheckedModule, IdentityGenerator)
checkedBuiltinPreludeModule generator0 resolvedModule =
  unsafePerformIO $
    modifyMVar builtinPreludeCheckCache $ \case
      Just (cachedGenerator, cachedIdentities, cached)
        | cachedGenerator == generator0 && cachedIdentities == resolvedIdentities ->
            pure (Just (cachedGenerator, cachedIdentities, cached), cached)
      Nothing -> do
        checked <- evaluate (checkModule generator0 resolvedModule [])
        pure (Just (generator0, resolvedIdentities, checked), checked)
      Just _ -> do
        checked <- evaluate (checkModule generator0 resolvedModule [])
        pure (Just (generator0, resolvedIdentities, checked), checked)
  where
    resolvedIdentities = resolvedModuleGeneratedIdentities resolvedModule
{-# NOINLINE checkedBuiltinPreludeModule #-}

isBuiltinPreludeModule ::
  Map P.ModuleName PackageModuleGraphNode ->
  Maybe PackageModuleGraph ->
  ResolvedSemanticModule ->
  Bool
isBuiltinPreludeModule nodesByModule mbGraph resolvedModule =
  resolvedSemanticModuleName resolvedModule == "Prelude"
    && case mbGraph of
      Nothing ->
        False
      Just _ ->
        case Map.lookup "Prelude" nodesByModule of
          Just node ->
            packageModuleGraphNodeSourcePath node == Just builtinPreludeSourcePath
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

checkModule :: IdentityGenerator -> ResolvedSemanticModule -> [ModuleInterface] -> TcM (CheckedModule, IdentityGenerator)
checkModule generator0 resolvedModule priorInterfaces = do
  let resolvedSyntax = resolvedSemanticModuleSyntax resolvedModule
      moduleName0 = resolvedSemanticModuleName resolvedModule
      priorExportsByIdentity = Map.fromList [(moduleInterfaceIdentity interface, moduleInterfaceExports interface) | interface <- priorInterfaces]
      priorData = Map.fromList [(moduleInterfaceIdentity interface, moduleInterfaceDataByIdentity interface) | interface <- priorInterfaces]
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
  derivedInstances <- synthesizeDerivedInstances (resolvedSemanticModuleIdentity resolvedModule) fullNameEnv scope0 resolvedSyntax
  (instanceSkeletons, generator1) <-
    buildInstanceSkeletons (resolvedSemanticModuleIdentity resolvedModule) generator0 fullNameEnv scope0 resolvedSyntax derivedInstances
  let scope1 = withScopeInstances (scopeInstances scope0 ++ instanceSkeletons) scope0
  let elaborateScope = mkElaborateScope (scopeValues scope1) (scopeElaborateTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
  finalizeContext <- mkFinalizeContext elaborateScope
  constructorBindings <-
    mapM
      (liftEither . (finalizeBindingWithContext finalizeContext . lowerConstructorBinding elaborateScope))
      [ ctor
        | dataInfo <- Map.elems localData,
          ctor <- dataConstructors dataInfo
      ]
  instanceBindings <- checkInstances fullNameEnv finalizeContext elaborateScope scope1 (derivedInstances ++ explicitInstances resolvedSyntax)
  defBindings <- mapM (checkDef finalizeContext elaborateScope scope1) (moduleDefDecls resolvedSyntax)
  exports <- buildExports resolvedSyntax localData localClasses localValues
  let exportedMain = exportedMainIdentity resolvedSyntax exports
      markExportedMain binding =
        binding
          { checkedBindingExportedAsMain =
              maybe False (\identity -> checkedBindingValueIdentity binding == Just identity) exportedMain
          }
  pure
    ( CheckedModule
        { checkedModuleName = moduleName0,
          checkedModuleIdentity = resolvedSemanticModuleIdentity resolvedModule,
          checkedModuleBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings,
          checkedModuleData = checkedDataByIdentity localData,
          checkedModuleClasses = checkedClassesByIdentity localClasses,
          checkedModuleInstances = instanceSkeletons,
          checkedModuleExports = exports
        },
      generator1
    )

checkModuleWithTiming :: TimingConfig -> IdentityGenerator -> ResolvedSemanticModule -> [ModuleInterface] -> IO (TcM (CheckedModule, IdentityGenerator))
checkModuleWithTiming timing generator0 resolvedModule priorInterfaces = do
  let resolvedSyntax = resolvedSemanticModuleSyntax resolvedModule
      moduleName0 = resolvedSemanticModuleName resolvedModule
      priorExportsByIdentity = Map.fromList [(moduleInterfaceIdentity interface, moduleInterfaceExports interface) | interface <- priorInterfaces]
      priorData = Map.fromList [(moduleInterfaceIdentity interface, moduleInterfaceDataByIdentity interface) | interface <- priorInterfaces]
      priorInstances = concatMap moduleInterfaceInstances priorInterfaces
      unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExportsByIdentity (P.moduleImports resolvedSyntax)
      visibleImportedInstances =
        visibleInstancesForImports priorExportsByIdentity priorData priorInstances unqualifiedClassIdentities (P.moduleImports resolvedSyntax)
      timePhase :: String -> TcM a -> IO (TcM a)
      timePhase = timeCheckModulePhase timing moduleName0
  preflightResult <-
    timePhase "preflight" $ do
      ensureDistinctImportAliases (P.moduleImports resolvedSyntax)
      rejectUnsupportedTypeFamilies resolvedSyntax
      rejectUnsupportedGeneralizedClassFeaturesModule P.refDisplayName resolvedSrcTypeToSrcType resolvedSyntax
  case preflightResult of
    Left err -> pure (Left err)
    Right () -> do
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
                                  derivedInstancesResult <- timePhase "derived-instances" (synthesizeDerivedInstances (resolvedSemanticModuleIdentity resolvedModule) fullNameEnv scope0 resolvedSyntax)
                                  case derivedInstancesResult of
                                    Left err -> pure (Left err)
                                    Right derivedInstances -> do
                                      instanceSkeletonsResult <-
                                        timePhase "instance-skeletons" $
                                          buildInstanceSkeletons (resolvedSemanticModuleIdentity resolvedModule) generator0 fullNameEnv scope0 resolvedSyntax derivedInstances
                                      case instanceSkeletonsResult of
                                        Left err -> pure (Left err)
                                        Right (instanceSkeletons, generator1) -> do
                                          let scope1 = withScopeInstances (scopeInstances scope0 ++ instanceSkeletons) scope0
                                              elaborateScope = mkElaborateScope (scopeValues scope1) (scopeElaborateTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
                                          finalizeContextResult <- timePhase "finalize-context" (mkFinalizeContext elaborateScope)
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
                                              pure (fmap (\checked -> (checked, generator1)) checkedResult)

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
  IO (TcM CheckedModule)
finalizeCheckedModuleWithTiming timing moduleName0 moduleIdentity resolvedSyntax localData localClasses localValues instanceSkeletons displayEnv finalizeContext elaborateScope scope1 derivedInstances = do
  constructorBindingsResult <-
    timeCheckModulePhaseIO timing moduleName0 "constructor-bindings" $
      checkConstructorsWithTiming timing moduleName0 finalizeContext elaborateScope localData
  case constructorBindingsResult of
    Left err -> pure (Left err)
    Right constructorBindings -> do
      instanceBindingsResult <-
        timeCheckModulePhaseIO timing moduleName0 "instance-bindings" $
          checkInstancesWithTiming timing moduleName0 displayEnv finalizeContext elaborateScope scope1 (derivedInstances ++ explicitInstances resolvedSyntax)
      case instanceBindingsResult of
        Left err -> pure (Left err)
        Right instanceBindings -> do
          defBindingsResult <-
            timeCheckModulePhaseIO timing moduleName0 "def-bindings" $
              checkDefsWithTiming timing moduleName0 finalizeContext elaborateScope scope1 (moduleDefDecls resolvedSyntax)
          case defBindingsResult of
            Left err -> pure (Left err)
            Right defBindings -> do
              exportsResult <- timeCheckModulePhase timing moduleName0 "exports" (buildExports resolvedSyntax localData localClasses localValues)
              pure $ do
                exports <- exportsResult
                let exportedMain = exportedMainIdentity resolvedSyntax exports
                    markExportedMain binding =
                      binding
                        { checkedBindingExportedAsMain =
                            maybe False (\identity -> checkedBindingValueIdentity binding == Just identity) exportedMain
                        }
                pure
                  CheckedModule
                    { checkedModuleName = moduleName0,
                      checkedModuleIdentity = moduleIdentity,
                      checkedModuleBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings,
                      checkedModuleData = checkedDataByIdentity localData,
                      checkedModuleClasses = checkedClassesByIdentity localClasses,
                      checkedModuleInstances = instanceSkeletons,
                      checkedModuleExports = exports
                    }

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

checkConstructorsWithTiming :: TimingConfig -> P.ModuleName -> FinalizeContext -> ElaborateScope -> Map String DataInfo -> IO (TcM [CheckedBinding])
checkConstructorsWithTiming timing moduleName0 finalizeContext elaborateScope localData =
  go []
    [ ctor
      | dataInfo <- Map.elems localData,
        ctor <- dataConstructors dataInfo
    ]
  where
    go acc [] = pure (Right (reverse acc))
    go acc (ctor : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 ("constructor." ++ ctorName ctor) $
          liftEither (finalizeBindingWithContext finalizeContext (lowerConstructorBinding elaborateScope ctor))
      case result of
        Left err -> pure (Left err)
        Right binding -> go (binding : acc) rest

checkInstancesWithTiming :: TimingConfig -> P.ModuleName -> DisplayNameEnv -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedInstanceDecl] -> IO (TcM [CheckedBinding])
checkInstancesWithTiming timing moduleName0 displayEnv finalizeContext elaborateScope scope instDecls =
  go [] (zip [(1 :: Int) ..] instDecls)
  where
    go acc [] =
      finalizeBindingsAllowOpaqueWithContextWithTiming
        timing
        (checkModuleOperationLabel moduleName0 "instance_methods.group_finalize")
        finalizeContext
        (concat (reverse acc))
    go acc ((index, instDecl) : rest) = do
      result <-
        lowerInstanceWithTiming
          timing
          moduleName0
          (instanceTimingLabel index instDecl)
          displayEnv
          elaborateScope
          scope
          instDecl
      case result of
        Left err -> pure (Left err)
        Right lowereds -> go (lowereds : acc) rest

instanceTimingLabel :: Int -> P.ResolvedInstanceDecl -> String
instanceTimingLabel index instDecl =
  "instance."
    ++ show index
    ++ "."
    ++ P.refDisplayName (P.instanceDeclClass instDecl)
    ++ "."
    ++ intercalate "_" (map (sanitizeType . resolvedSrcTypeToSrcType) (NE.toList (P.instanceDeclTypes instDecl)))

lowerInstanceWithTiming :: TimingConfig -> P.ModuleName -> String -> DisplayNameEnv -> ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> IO (TcM [LoweredBinding])
lowerInstanceWithTiming timing moduleName0 instanceLabel displayEnv elaborateScope scope instDecl = do
  instanceResult <-
    timeCheckModuleOperation timing moduleName0 (instanceLabel ++ ".lookup") $
      lookupInstanceForDecl displayEnv scope instDecl
  case instanceResult of
    Left err -> pure (Left err)
    Right (classInfo, instanceInfo) ->
      lowerInstanceMethodsWithTiming timing moduleName0 instanceLabel elaborateScope classInfo instanceInfo (instanceHeadTypeViews instanceInfo) (P.instanceDeclMethods instDecl)

lowerInstanceMethodsWithTiming :: TimingConfig -> P.ModuleName -> String -> ElaborateScope -> ClassInfo -> InstanceInfo -> NonEmpty TypeView -> [P.ResolvedMethodDef] -> IO (TcM [LoweredBinding])
lowerInstanceMethodsWithTiming timing moduleName0 instanceLabel elaborateScope classInfo instanceInfo instanceHeadViews methodDefs =
  go [] methodDefs
  where
    go acc [] = pure (Right (reverse acc))
    go acc (methodDef : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 (instanceLabel ++ ".method." ++ P.refDisplayName (P.methodDefName methodDef) ++ ".lower") $
          lowerInstanceMethod elaborateScope classInfo instanceInfo instanceHeadViews methodDef
      case result of
        Left err -> pure (Left err)
        Right lowered -> go (lowered : acc) rest

checkInstances :: DisplayNameEnv -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedInstanceDecl] -> TcM [CheckedBinding]
checkInstances displayEnv finalizeContext elaborateScope scope instDecls = do
  lowereds <- concat <$> mapM (lowerInstance displayEnv elaborateScope scope) instDecls
  liftEither (finalizeBindingsAllowOpaqueWithContext finalizeContext lowereds)

lowerInstance :: DisplayNameEnv -> ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> TcM [LoweredBinding]
lowerInstance displayEnv elaborateScope scope instDecl = do
  (classInfo, instanceInfo) <- lookupInstanceForDecl displayEnv scope instDecl
  mapM (lowerInstanceMethod elaborateScope classInfo instanceInfo (instanceHeadTypeViews instanceInfo)) (P.instanceDeclMethods instDecl)

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

lowerInstanceMethod :: ElaborateScope -> ClassInfo -> InstanceInfo -> NonEmpty TypeView -> P.ResolvedMethodDef -> TcM LoweredBinding
lowerInstanceMethod elaborateScope classInfo instanceInfo instanceHeadViews methodDef =
  case lookupClassMethod (P.methodDefName methodDef) classInfo of
    Just methodInfo | Just valueInfo@OrdinaryValue {} <- lookupInstanceMethod methodInfo instanceInfo -> do
      let methodBodyView = specializeMethodTypeView methodInfo instanceHeadViews
          methodSourceView =
            (ordinaryValueTypeView valueInfo)
              { typeViewBinderIdentities = typeViewBinderIdentities methodBodyView
              }
      liftEither
        (lowerConstrainedResolvedExprBinding elaborateScope (loweredBindingIdentityFromValueInfo valueInfo) (valueConstraintInfos valueInfo) methodSourceView methodBodyView False (P.methodDefExpr methodDef))
    _ -> throwError (ProgramUnexpectedInstanceMethod (className classInfo) (P.refDisplayName (P.methodDefName methodDef)))

data DefWorkItem = DefWorkItem
  { defWorkItemDecl :: P.ResolvedDefDecl,
    defWorkItemIdentity :: SymbolIdentity,
    defWorkItemLowered :: LoweredBinding,
    defWorkItemDependencies :: [SymbolIdentity]
  }

checkDefsWithTiming :: TimingConfig -> P.ModuleName -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedDefDecl] -> IO (TcM [CheckedBinding])
checkDefsWithTiming timing moduleName0 finalizeContext elaborateScope scope defDecls = do
  workItemsResult <- lowerDefWorkItemsWithTiming timing moduleName0 elaborateScope scope defDecls
  case workItemsResult of
    Left err -> pure (Left err)
    Right workItems -> do
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
                finalizeContext
                (Just moduleContext)
                batchSize
                nonRecursiveIdentities
                workItems

lowerDefWorkItemsWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  ElaborateScope ->
  Scope ->
  [P.ResolvedDefDecl] ->
  IO (TcM [DefWorkItem])
lowerDefWorkItemsWithTiming timing moduleName0 elaborateScope scope defDecls =
  go (Set.fromList (map (resolvedSymbolIdentity . P.defDeclName) defDecls)) [] defDecls
  where
    go _ acc [] = pure (Right (reverse acc))
    go localDefIdentities acc (defDecl : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 ("def." ++ P.refDisplayName (P.defDeclName defDecl) ++ ".lower") $
          lowerDefWorkItem elaborateScope scope localDefIdentities defDecl
      case result of
        Left err -> pure (Left err)
        Right workItem -> go localDefIdentities (workItem : acc) rest

lowerDefWorkItem ::
  ElaborateScope ->
  Scope ->
  Set.Set SymbolIdentity ->
  P.ResolvedDefDecl ->
  TcM DefWorkItem
lowerDefWorkItem elaborateScope scope localDefIdentities defDecl = do
  let defName = P.refDisplayName (P.defDeclName defDecl)
  valueInfo <- lookupValueInfoBySymbol scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      lowered <-
        liftEither $
          lowerResolvedConstrainedExprBinding
            elaborateScope
            (loweredBindingIdentityFromValueInfo ordinary)
            (P.defDeclType defDecl)
            (resolvedDefDeclIsMain defDecl)
            (P.defDeclExpr defDecl)
      pure
        DefWorkItem
          { defWorkItemDecl = defDecl,
            defWorkItemIdentity = valueInfoSymbolIdentity ordinary,
            defWorkItemLowered = lowered,
            defWorkItemDependencies = localResolvedDefDependencies localDefIdentities (P.defDeclExpr defDecl)
          }
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
  FinalizeContext ->
  Maybe ModuleFinalizeContext ->
  Int ->
  Set.Set SymbolIdentity ->
  [DefWorkItem] ->
  IO (TcM [CheckedBinding])
finalizeDefWorkItemsWithTiming timing moduleName0 finalizeContext moduleContext batchSize nonRecursiveIdentities workItems
  | Just moduleContext0 <- moduleContext =
      finalizeDefWorkItemLayersWithTiming timing moduleName0 finalizeContext moduleContext0 batchSize nonRecursiveIdentities workItems
  | otherwise =
      go [] workItems
  where
    go acc [] = pure (Right (reverse acc))
    go acc (workItem : rest) = do
      result <- finalizeDefWorkItemWithTiming timing moduleName0 finalizeContext moduleContext nonRecursiveIdentities workItem
      case result of
        Left err -> pure (Left err)
        Right binding -> go (binding : acc) rest

finalizeDefWorkItemLayersWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  FinalizeContext ->
  ModuleFinalizeContext ->
  Int ->
  Set.Set SymbolIdentity ->
  [DefWorkItem] ->
  IO (TcM [CheckedBinding])
finalizeDefWorkItemLayersWithTiming timing moduleName0 finalizeContext moduleContext batchSize nonRecursiveIdentities workItems = do
  let layers = nonRecursiveDefLayers batchSize nonRecursiveIdentities workItems
      layeredIdentities = Set.unions (map (Set.fromList . map defWorkItemIdentity) layers)
      fallbackItems =
        [ workItem
        | workItem <- workItems
        , defWorkItemIdentity workItem `Set.notMember` layeredIdentities
        ]
  layerResults <- finalizeLayers Map.empty (1 :: Int) layers
  case layerResults of
    Left err -> pure (Left err)
    Right checkedByIdentity0 -> do
      fallbackResult <- finalizeFallbackItems checkedByIdentity0 fallbackItems
      pure $ do
        checkedByIdentity <- fallbackResult
        traverse
          ( \workItem ->
              case Map.lookup (defWorkItemIdentity workItem) checkedByIdentity of
                Just checked -> Right checked
                Nothing -> Left (ProgramPipelineError ("missing checked definition `" ++ defWorkItemName workItem ++ "`"))
          )
          workItems
  where
    finalizeLayers checkedByIdentity _ [] =
      pure (Right checkedByIdentity)
    finalizeLayers checkedByIdentity index (layer : rest) = do
      let layerOperation = "defs.layer_" ++ show index
          layerLabel = checkModuleOperationLabel moduleName0 layerOperation
      layerResult <-
        if length layer > 1 && all moduleLayerEligibleDefWorkItem layer
          then
            if timingProgramDefDetails timing
              then
                finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming
                  timing
                  layerLabel
                  moduleContext
                  (map defWorkItemLowered layer)
              else
                timeProgramOperationIO timing layerLabel $
                  finalizeBindingLayerAllowOpaqueWithModuleContext
                    moduleContext
                    (map defWorkItemLowered layer)
          else
            finalizeLayerIndividually index layer
      case layerResult of
        Left err -> pure (Left err)
        Right checkedLayer -> do
          let checkedByIdentity' =
                foldl'
                  ( \acc (workItem, checked) ->
                      Map.insert (defWorkItemIdentity workItem) checked acc
                  )
                  checkedByIdentity
                  (zip layer checkedLayer)
          finalizeLayers checkedByIdentity' (index + 1) rest

    finalizeLayerIndividually _layerIndex layer =
      goLayer [] (1 :: Int) layer
      where
        goLayer acc _ [] = pure (Right (reverse acc))
        goLayer acc itemIndex (workItem : rest) = do
          result <-
            finalizeDefWorkItemWithTiming
              timing
              moduleName0
              finalizeContext
              (Just moduleContext)
              nonRecursiveIdentities
              workItem
          case result of
            Left err -> pure (Left err)
            Right checked ->
              checked `seq` goLayer (checked : acc) (itemIndex + 1) rest

    finalizeFallbackItems checkedByIdentity [] =
      pure (Right checkedByIdentity)
    finalizeFallbackItems checkedByIdentity workItems0@(workItem : rest)
      | moduleDeferredLayerEligibleDefWorkItem nonRecursiveIdentities workItem = do
          let (deferredLayer, remaining) =
                splitAt
                  batchSize
                  (takeWhile (moduleDeferredLayerEligibleDefWorkItem nonRecursiveIdentities) workItems0)
          if length deferredLayer <= 1
            then finalizeFallbackItem checkedByIdentity workItem rest
            else do
              layerResult <-
                finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming
                  timing
                  (checkModuleOperationLabel moduleName0 ("defs.deferred_layer_" ++ show (Map.size checkedByIdentity + 1)))
                  moduleContext
                  (map defWorkItemLowered deferredLayer)
              case layerResult of
                Left err -> pure (Left err)
                Right checkedLayer -> do
                  let checkedByIdentity' =
                        foldl'
                          ( \acc (workItem0, checked) ->
                              Map.insert (defWorkItemIdentity workItem0) checked acc
                          )
                          checkedByIdentity
                          (zip deferredLayer checkedLayer)
                  finalizeFallbackItems checkedByIdentity' remaining
    finalizeFallbackItems checkedByIdentity (workItem : rest) =
      finalizeFallbackItem checkedByIdentity workItem rest

    finalizeFallbackItem checkedByIdentity workItem rest = do
      result <-
        finalizeDefWorkItemWithTiming
          timing
          moduleName0
          finalizeContext
          (Just moduleContext)
          nonRecursiveIdentities
          workItem
      case result of
        Left err -> pure (Left err)
        Right checked ->
          finalizeFallbackItems (Map.insert (defWorkItemIdentity workItem) checked checkedByIdentity) rest

defWorkItemName :: DefWorkItem -> String
defWorkItemName = P.refDisplayName . P.defDeclName . defWorkItemDecl

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

finalizeDefWorkItemWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  FinalizeContext ->
  Maybe ModuleFinalizeContext ->
  Set.Set SymbolIdentity ->
  DefWorkItem ->
  IO (TcM CheckedBinding)
finalizeDefWorkItemWithTiming timing moduleName0 finalizeContext moduleContext nonRecursiveIdentities workItem =
  timeProgramOperationIO timing label $
    case moduleContext of
      Just moduleContext0
        | moduleContextEligibleDefWorkItem nonRecursiveIdentities workItem ->
            if timingProgramDefDetails timing
              then
                finalizeBindingAllowOpaqueWithModuleContextWithTiming
                  timing
                  label
                  moduleContext0
                  False
                  lowered
              else
                evaluate (finalizeBindingAllowOpaqueWithModuleContext moduleContext0 lowered)
      _ ->
        if timingProgramDefDetails timing
          then
            finalizeBindingAllowOpaqueWithContextWithTiming
              timing
              label
              finalizeContext
              lowered
          else
            evaluate (finalizeBindingAllowOpaqueWithContext finalizeContext lowered)
  where
    defName = defWorkItemName workItem
    lowered = defWorkItemLowered workItem
    label = checkModuleOperationLabel moduleName0 ("def." ++ defName)

moduleDefBatchSize :: IO Int
moduleDefBatchSize = do
  mbValue <- lookupEnv "MLF_MODULE_DEF_BATCH_SIZE"
  pure $
    case mbValue >>= readMaybe of
      Just n | n > 1 -> n
      _ -> 16

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
  Map.member (ctorOwningTypeIdentity ctorInfo) (exportedTypesByIdentity exports)

qualifyModuleExports :: P.ModuleName -> ModuleExports -> ModuleExports
qualifyModuleExports alias exports =
  moduleExportsFromMaps qualifiedValues qualifiedTypes qualifiedClasses
  where
    qualifiedName name = alias ++ "." ++ name
    exportedTypeNames = Map.keysSet (exportedTypesForDisplay exports)
    exportedClassNames = Map.keysSet (exportedClassesForDisplay exports)

    qualifiedTypes =
      Map.fromList
        [ let qualifiedDataInfo = qualifyDataInfo dataInfo
              visibleCtorIdentities = Map.keysSet (exportedTypeConstructorsByIdentity typeInfo)
              qualifiedCtorsByIdentity =
                Map.fromList [(ctorInfoSymbol ctor, ctor) | ctor <- dataConstructors qualifiedDataInfo]
              qualifiedCtors =
                [ (qualifiedName sourceName, qualifiedCtor)
                  | (sourceName, ctor) <- Map.toList (exportedTypeConstructorsForDisplay typeInfo),
                    ctorInfoSymbol ctor `Set.member` visibleCtorIdentities,
                    Just qualifiedCtor <- [Map.lookup (ctorInfoSymbol ctor) qualifiedCtorsByIdentity]
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
                valueRuntimeName = ctorRuntimeName ctor,
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
            qualifyValueInfo name valueInfo
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
            (ctorTypeView ctor)
              { typeViewDisplay = qualifySrcType (ctorType ctor)
              },
          ctorOwningTypeIdentity = ctorOwningTypeIdentity ctor,
          ctorOwnerConstructors = map qualifyConstructorShape (ctorOwnerConstructors ctor)
        }

    qualifyConstructorShape shape =
      shape
        { constructorShapeTypeView =
            (constructorShapeTypeView shape)
              { typeViewDisplay = qualifySrcType (constructorShapeType shape)
              }
        }

    qualifyClassInfo classInfo =
      let qualifyConstraintInfo constraintInfo =
            constraintInfo
              { constraintDisplayClass = qualifiedClassNameFor (constraintDisplayClass constraintInfo),
                constraintTypeViews = fmap qualifyTypeView (constraintTypeViews constraintInfo)
              }
          qualifyTypeView view =
            view
              { typeViewDisplay = qualifySrcType (typeViewDisplay view)
              }
          qualifyMethod methodInfo =
            methodInfo
              { methodTypeViewRaw = qualifyTypeView (methodTypeViewRaw methodInfo),
                methodConstraints = map qualifyConstraint (methodConstraints methodInfo),
                methodConstraintInfos = map qualifyConstraintInfo (methodConstraintInfos methodInfo)
              }
          qualifiedMethodsByIdentity = Map.map qualifyMethod (classMethodsByIdentity classInfo)
       in classInfo
            { classSuperclasses = map qualifyConstraint (classSuperclasses classInfo),
              classSuperclassInfos = map qualifyConstraintInfo (classSuperclassInfos classInfo),
              classMethodsByIdentity = qualifiedMethodsByIdentity
            }

    qualifyValueInfo _sourceName valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          valueInfo
            { valueTypeView =
                (valueTypeView valueInfo)
                  { typeViewDisplay = qualifySrcType (typeViewDisplay (valueTypeView valueInfo))
                  },
              valueConstraints = map qualifyConstraint (valueConstraints valueInfo)
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
                  valueRuntimeName = valueRuntimeName valueInfo,
                  valueCtorInfo = qualifiedCtorInfo
                }

    qualifyMethodFromExport methodInfo =
      methodInfo
        { methodTypeViewRaw =
            (methodTypeViewRaw methodInfo)
              { typeViewDisplay = qualifySrcType (methodType methodInfo)
              },
          methodConstraints = map qualifyConstraint (methodConstraints methodInfo),
          methodConstraintInfos = map qualifyConstraintInfoFromExport (methodConstraintInfos methodInfo)
        }

    qualifyConstraintInfoFromExport constraintInfo =
      constraintInfo
        { constraintDisplayClass = qualifiedClassNameFor (constraintDisplayClass constraintInfo),
          constraintTypeViews = fmap qualifyTypeViewFromExport (constraintTypeViews constraintInfo)
        }

    qualifyTypeViewFromExport view =
      view
        { typeViewDisplay = qualifySrcType (typeViewDisplay view)
        }

    qualifiedClassNameFor className0
      | className0 `Set.member` exportedClassNames = qualifiedName className0
      | otherwise = className0

    qualifyConstraint constraint =
      constraint
        { P.constraintClassName =
            if P.constraintClassName constraint `Set.member` exportedClassNames
              then qualifiedName (P.constraintClassName constraint)
              else P.constraintClassName constraint,
          P.constraintTypes = fmap qualifySrcType (P.constraintTypes constraint)
        }

    qualifySrcType ty =
      case ty of
        STVar {} -> ty
        STBase name
          | name `Set.member` exportedTypeNames -> STBase (qualifiedName name)
          | otherwise -> ty
        STCon name args
          | name `Set.member` exportedTypeNames -> STCon (qualifiedName name) (fmap qualifySrcType args)
          | otherwise -> STCon name (fmap qualifySrcType args)
        STVarApp name args -> STVarApp name (fmap qualifySrcType args)
        STTyLam name body -> STTyLam name (qualifySrcType body)
        STTyApp fun arg -> STTyApp (qualifySrcType fun) (qualifySrcType arg)
        STArrow dom cod -> STArrow (qualifySrcType dom) (qualifySrcType cod)
        STForall name mb body -> STForall name (fmap (SrcBound . qualifySrcType . unSrcBound) mb) (qualifySrcType body)
        STMu name body -> STMu name (qualifySrcType body)
        STBottom -> STBottom

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
    && instanceHeadIdentityTypes left == instanceHeadIdentityTypes right

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
                          valueRuntimeName = ctorRuntimeName ctor,
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
              view
                { typeViewHeadIdentities =
                    mergeSymbolIdentityMaps
                      [ typeViewHeadIdentities view,
                        importedValueTypeHeadIdentities exports view
                      ]
                }
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
  Map.fromList
    [ (alias, identity)
    | identity <- Map.keys (exportedTypesByIdentity exports),
      identity `Set.member` mentionedHeadIdentities,
      alias <- symbolIdentityAliasNames identity,
      not (null alias)
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
      rewriteTypeView sourceTy view =
        (view {typeViewDisplay = rewriteOwnerTypeHeads ownerIdentity ownerNames hiddenName view sourceTy})
          { typeViewBinderIdentities =
              mergeTypeBinderIdentityMaps
                [ ownerParamBinderIdentities,
                  typeViewBinderIdentities view
                ]
          }
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

rewriteOwnerTypeHeads :: SymbolIdentity -> Set.Set String -> String -> TypeView -> SrcType -> SrcType
rewriteOwnerTypeHeads ownerIdentity ownerNames hiddenName view = go
  where
    rewriteHead name
      | typeViewHeadIdentityForAlias view name == Just ownerIdentity = hiddenName
      | name `Set.member` ownerNames = hiddenName
      | otherwise = name

    go ty =
      case ty of
        STVar {} -> ty
        STArrow dom cod -> STArrow (go dom) (go cod)
        STBase name -> STBase (rewriteHead name)
        STCon name args -> STCon (rewriteHead name) (fmap go args)
        STVarApp name args -> STVarApp name (fmap go args)
        STTyLam name body -> STTyLam name (go body)
        STTyApp fun arg -> STTyApp (go fun) (go arg)
        STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu name body -> STMu name (go body)
        STBottom -> STBottom

importedHiddenConstructorInfo :: ConstructorInfo -> DataInfo -> ConstructorInfo
importedHiddenConstructorInfo ctorInfo hiddenDataInfo =
  case find ((== ctorInfoSymbol ctorInfo) . ctorInfoSymbol) (dataConstructors hiddenDataInfo) of
    Just hiddenCtorInfo ->
      hiddenCtorInfo
        { ctorRuntimeName = ctorRuntimeName ctorInfo
        }
    Nothing -> ctorInfo

exportedConstructorOwnerType :: ConstructorInfo -> ModuleExports -> Maybe DataInfo
exportedConstructorOwnerType ctorInfo exports =
  case Map.lookup (ctorOwningTypeIdentity ctorInfo) (exportedTypesByIdentity exports) of
    Just typeInfo -> Just (exportedTypeData typeInfo)
    Nothing -> Just (constructorOwnerDataInfoFromShapes ctorInfo)

exportedValueByIdentity :: SymbolIdentity -> ModuleExports -> Maybe (String, ValueInfo)
exportedValueByIdentity identity exports =
  (,) <$> Map.lookup identity (exportedValueDisplaysByIdentity exports) <*> Map.lookup identity (exportedValuesByIdentity exports)

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
  (,) <$> Map.lookup identity (exportedTypeDisplaysByIdentity exports) <*> Map.lookup identity (exportedTypesByIdentity exports)

exportedClassByIdentity :: ClassIdentity -> ModuleExports -> Maybe (String, ClassInfo)
exportedClassByIdentity identity exports =
  (,) <$> Map.lookup identity (exportedClassDisplaysByIdentity exports) <*> Map.lookup identity (exportedClassesByIdentity exports)

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

displayClassConstraintForResolved :: DisplayNameEnv -> P.ResolvedClassConstraint -> TcM P.ClassConstraint
displayClassConstraintForResolved env constraint = do
  className0 <- displayClassName env (P.constraintClassName constraint)
  tys <- traverse (displaySrcTypeForResolved env) (P.constraintTypes constraint)
  pure
    P.ClassConstraint
      { P.constraintClassName = className0,
        P.constraintTypes = tys
      }

typeViewForDisplayEnv :: DisplayNameEnv -> ResolvedSrcType -> TcM TypeView
typeViewForDisplayEnv env ty = do
  display <- displaySrcTypeForResolved env ty
  pure (typeViewFromResolved ty) {typeViewDisplay = display}

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
  case Map.lookup (resolvedSymbolIdentity symbol) namesByIdentity of
    Just names
      | resolvedSymbolDisplayName symbol `elem` names -> Just (resolvedSymbolDisplayName symbol)
      | name : _ <- names -> Just name
    _ -> Nothing

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

dataKind :: [P.TypeParam] -> P.SrcKind
dataKind params =
  foldr P.KArrow P.KType (map P.typeParamKind params)

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
      (\acc (ty, param) -> checkResolvedKind acc ty (P.typeParamKind param))
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
    (\acc (ty, param) -> checkResolvedKind acc ty (P.typeParamKind param))
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
  | otherwise = Map.lookup (resolvedSymbolIdentity symbol) (kindTypeConstructors env)

builtinTypeKindByIdentity :: SymbolIdentity -> Maybe P.SrcKind
builtinTypeKindByIdentity identity =
  Map.lookup identity builtinTypeKindsByIdentity

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
      let params = P.dataDeclParams dataDecl
          paramNames = P.typeParamNames params
      ensureDistinctPlain ProgramDuplicateTypeParameter paramNames
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
      let (foralls, _) = splitForalls (typeViewDisplay ctorTypeView0)
      ctorForallBinderInfo0 <- constructorForallBinderInfo foralls (P.constructorDeclType ctorDecl)
      pure
        ConstructorInfo
          { ctorInfoSymbol = ctorIdentity,
            ctorRuntimeName = qualify (symbolDefiningModule ctorIdentity) (symbolDefiningName ctorIdentity),
            ctorTypeView = ctorTypeView0,
            ctorForallBinderInfo = ctorForallBinderInfo0,
            ctorOwningTypeIdentity = dataIdentity,
            ctorIndex = index,
            ctorOwnerConstructors = []
          }

    constructorForallBinderInfo :: [(String, Maybe SrcType)] -> ResolvedSrcType -> TcM [ConstructorForallBinder]
    constructorForallBinderInfo displayForalls resolvedTy =
      go displayForalls (constructorForallRefs resolvedTy)
      where
        go :: [(String, Maybe SrcType)] -> [ResolvedTypeBinderRef] -> TcM [ConstructorForallBinder]
        go [] _ =
          pure []
        go ((displayName, _) : rest) (ref : refs) =
          ( ConstructorForallBinder
              { constructorForallDisplayName = displayName,
                constructorForallIdentity = resolvedTypeBinderTypeIdentity ref
              }
              :
            )
            <$> go rest refs
        go ((displayName, _) : _) [] =
          throwError $
            ProgramPipelineError
              ("resolved constructor forall `" ++ displayName ++ "` is missing identity")

    constructorForallRefs :: ResolvedSrcType -> [ResolvedTypeBinderRef]
    constructorForallRefs ty =
      case ty of
        RSTForall ref _ body ->
          ref : constructorForallRefs body
        _ -> []

    validateConstructorResult :: SymbolIdentity -> P.ResolvedDataDecl -> P.ResolvedConstructorDecl -> ResolvedSrcType -> TcM ()
    validateConstructorResult dataIdentity dataDecl ctorDecl resultTy =
      let owner = P.dataDeclDisplayName dataDecl
          params = P.typeParamNames (P.dataDeclParams dataDecl)
          invalid = throwError (ProgramInvalidConstructorResult (P.refDisplayName (P.constructorDeclName ctorDecl)) (resolvedSrcTypeToSrcType resultTy) owner)
       in case constructorResultHead resultTy of
            Just (symbol, argCount)
              | resolvedSymbolIdentity symbol == dataIdentity && argCount == length params -> pure ()
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
          classParams = P.classDeclParams classDecl
          classParamNames0 = fmap P.typeParamName classParams
      ensureDistinctPlain ProgramDuplicateTypeParameter (NE.toList classParamNames0)
      classParamBinders0 <- traverse classParamBinder classParams
      validateFunctionalDependencies className0 classParamNames0 (P.classDeclFundeps classDecl)
      fundeps0 <- mapM (functionalDependencyInfo className0 classParamBinders0) (P.classDeclFundeps classDecl)
      superclasses0 <- mapM (displayClassConstraintForResolved displayEnv) (P.classDeclSuperclasses classDecl)
      superclassInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.classDeclSuperclasses classDecl)
      methodEntries <-
        forM
          (P.classDeclMethods classDecl)
          ( \sig -> do
              let methodSymbol = P.methodSigName sig
                  methodIdentity = resolvedSymbolIdentity methodSymbol
                  methodName0 = P.refDisplayName methodSymbol
              methodConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.constrainedConstraints (P.methodSigType sig))
              methodBodyView <- typeViewForDisplayEnv displayEnv (P.constrainedBody (P.methodSigType sig))
              let methodConstraints0 = map displayConstraint methodConstraintInfos0
                  methodInfo =
                    MethodInfo
                      { methodInfoSymbol = methodIdentity,
                        methodDisplayName = methodName0,
                        methodTypeViewRaw = methodBodyView,
                        methodConstraints = methodConstraints0,
                        methodConstraintInfos = methodConstraintInfos0,
                        methodParamBinders = classParamBinders0
                      }
              pure (methodName0, methodInfo)
          )
      let methodsByIdentity =
            Map.fromList
              [ (methodInfoSymbolIdentity methodInfo, methodInfo)
              | (_, methodInfo) <- methodEntries
              ]
      pure
        ( className0,
          ClassInfo
            { classInfoSymbol = classIdentity0,
              classTypeParams = classParams,
              classSuperclasses = superclasses0,
              classSuperclassInfos = superclassInfos0,
              classFunctionalDependencies = fundeps0,
              classMethodsByIdentity = methodsByIdentity
            }
        )

    classParamBinder :: P.TypeParam -> TcM (String, TypeBinderIdentity)
    classParamBinder param =
      case typeParamBinderIdentity param of
        Just identity -> pure (P.typeParamName param, identity)
        Nothing ->
          throwError $
            ProgramPipelineError
              ("resolved class parameter `" ++ P.typeParamName param ++ "` is missing identity")

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
      let defConstraints0 = map displayConstraint defConstraintInfos0
          defTypeView0 =
            constrainedVisibleTypeView defConstraintInfos0 defBodyView0
      pure
        ( defName,
          OrdinaryValue
            { valueInfoSymbol = valueIdentity,
              valueRuntimeName = qualify (symbolDefiningModule valueIdentity) valueIdentityName,
              valueTypeView = defTypeView0,
              valueConstraints = defConstraints0,
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
              valueRuntimeName = ctorRuntimeName ctor,
              valueCtorInfo = ctor
            }
        )
        | dataInfo <- Map.elems dataInfos,
          ctor <- dataConstructors dataInfo
      ]

synthesizeDerivedInstances ::
  SymbolIdentity ->
  DisplayNameEnv ->
  Scope ->
  P.ResolvedModuleSyntax ->
  TcM [P.ResolvedInstanceDecl]
synthesizeDerivedInstances moduleIdentity displayEnv scope mod0 = do
  candidates <- concat <$> mapM deriveForData (moduleDataDecls mod0)
  pendingInstances <- mapM (\(_, _, classInfo, classDisplayName, instDecl) -> pendingDerivedInstance classInfo classDisplayName instDecl) candidates
  let validationScope = withScopeInstances (scopeInstances scope ++ pendingInstances) scope
  mapM_
    (\(resolvedDataDecl, displayDataDecl, classInfo, classDisplayName, _) -> validateEqDerivingFields classInfo classDisplayName validationScope resolvedDataDecl displayDataDecl)
    candidates
  pure [instDecl | (_, _, _, _, instDecl) <- candidates]
  where
    deriveForData dataDecl = do
      displayDataDecl <- resolvedDataDeclForEnv dataDecl
      forM (P.dataDeclDeriving dataDecl) $ \classSymbol -> do
        classInfo <- lookupClassInfoBySymbol scope classSymbol
        let classDisplayName = resolvedSymbolDisplayName classSymbol
        if hasDisplayName (dneClasses displayEnv) (classInfoSymbolIdentity classInfo) isEqName
          then
            case eqMethodReference classInfo of
              Just eqMethodSymbol -> do
                instDecl <- mkEqInstance classSymbol classInfo eqMethodSymbol dataDecl displayDataDecl
                pure (dataDecl, displayDataDecl, classInfo, classDisplayName, instDecl)
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
      maybe False (any predicate) (Map.lookup identity namesByIdentity)

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
            instanceConstraints = map displayConstraint constraintInfos,
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

    constructorFieldTypes ctor =
      fst (splitArrows (snd (splitForalls (P.constructorDeclType ctor))))

    validateEqDerivingFields :: ClassInfo -> P.ClassName -> Scope -> P.ResolvedDataDecl -> P.DataDecl -> TcM ()
    validateEqDerivingFields eqClassInfo eqClassDisplayName validationScope resolvedDataDecl displayDataDecl =
      mapM_
        (validateEqDerivingField eqClassInfo eqClassDisplayName validationScope displayDataDecl)
        (concatMap (resolvedConstructorFieldTypes . P.constructorDeclType) (P.dataDeclConstructors resolvedDataDecl))

    validateEqDerivingField :: ClassInfo -> P.ClassName -> Scope -> P.DataDecl -> ResolvedSrcType -> TcM ()
    validateEqDerivingField eqClassInfo eqClassDisplayName validationScope dataDecl fieldTy = do
      fieldView <- typeViewForDisplayEnv displayEnv fieldTy
      satisfiable <-
        constraintTypeSatisfiable
          (classInfoSymbolIdentity eqClassInfo)
          eqClassDisplayName
          validationScope
          dataDecl
          Set.empty
          (classInfoSymbolIdentity eqClassInfo)
          eqClassDisplayName
          fieldView
      if satisfiable
        then pure ()
        else throwError (ProgramDerivingMissingFieldInstance eqClassDisplayName (typeViewDisplay fieldView))

    constraintTypeSatisfiable :: ClassIdentity -> P.ClassName -> Scope -> P.DataDecl -> Set.Set (ClassIdentity, String) -> ClassIdentity -> P.ClassName -> TypeView -> TcM Bool
    constraintTypeSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl seen classIdentity0 className0 fieldView = do
      coveredByDerived <-
        if classIdentity0 == derivedClassIdentity
          then fieldCoveredByDerivedConstraints dataDecl fieldView
          else pure False
      if coveredByDerived
        then pure True
        else
          if key `Set.member` seen
            then pure False
            else
              case resolveInstanceInfoWithIdentityType elaborateScope classIdentity0 className0 fieldView of
                Right (instanceInfo, subst) -> do
                  let seen' = Set.insert key seen
                  allM
                    (constraintSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl seen' . applyConstraintInfoSubst subst)
                    (instanceConstraintInfos instanceInfo)
                Left _ -> pure False
      where
        elaborateScope = scopeToElaborateScope validationScope
        key = (classIdentity0, show (typeViewIdentity fieldView))

    constraintSatisfiable :: ClassIdentity -> P.ClassName -> Scope -> P.DataDecl -> Set.Set (ClassIdentity, String) -> ConstraintInfo -> TcM Bool
    constraintSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl seen constraint =
      constraintTypeSatisfiable
        derivedClassIdentity
        derivedClassName
        validationScope
        dataDecl
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

    fieldCoveredByDerivedConstraints :: P.DataDecl -> TypeView -> TcM Bool
    fieldCoveredByDerivedConstraints dataDecl fieldView =
      case (typeViewDisplay fieldView, typeViewIdentity fieldView) of
        (STVar _, STVar identityName) -> do
          case typeViewBinderIdentityForAlias fieldView identityName of
            Just identity -> do
              dataParamIdentities <- traverse dataParamIdentity (P.dataDeclParams dataDecl)
              pure (identity `Set.member` Set.fromList dataParamIdentities)
            Nothing -> pure False
        _ ->
          pure $
            isRecursiveOwnerField dataDecl (typeViewDisplay fieldView)
              || isRecursiveOwnerField dataDecl (typeViewIdentity fieldView)

    dataParamIdentity :: P.TypeParam -> TcM TypeBinderIdentity
    dataParamIdentity param =
      case typeParamBinderIdentity param of
        Just identity -> pure identity
        Nothing ->
          throwError $
            ProgramPipelineError
              ("resolved data parameter `" ++ P.typeParamName param ++ "` is missing identity")

    derivedConstraintParams dataDecl =
      let params = Set.fromList (P.typeParamNames (P.dataDeclParams dataDecl))
          fieldTypes =
            filter
              (not . isRecursiveOwnerField dataDecl)
              (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))
          usedParams = Set.intersection params (foldMap freeTypeVars fieldTypes)
       in [paramName | paramName <- P.typeParamNames (P.dataDeclParams dataDecl), paramName `Set.member` usedParams]

    freeTypeVars ty =
      case ty of
        STVar name -> Set.singleton name
        STArrow dom cod -> Set.union (freeTypeVars dom) (freeTypeVars cod)
        STBase {} -> Set.empty
        STCon _ args -> foldMap freeTypeVars args
        STVarApp name args -> Set.insert name (foldMap freeTypeVars args)
        STTyLam name body -> Set.delete name (freeTypeVars body)
        STTyApp fun arg -> Set.union (freeTypeVars fun) (freeTypeVars arg)
        STForall name mb body ->
          maybe Set.empty (freeTypeVars . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVars body)
        STMu name body -> Set.delete name (freeTypeVars body)
        STBottom -> Set.empty

    scopeToElaborateScope scope0 =
      mkElaborateScope (scopeValues scope0) (scopeElaborateTypes scope0) (scopeClasses scope0) (scopeInstances scope0)

    mkEqInstance classSymbol _classInfo eqMethodSymbol resolvedDataDecl displayDataDecl = do
      let dataSymbol = P.dataDeclName resolvedDataDecl
          dataName0 = symbolDefiningName (resolvedSymbolIdentity dataSymbol)
          andSymbol = Builtins.builtinValueSymbol "__mlfp_and"
      boolSymbol <- pure (Builtins.builtinTypeSymbol "Bool")
      paramRefs <- traverse resolvedDataParamRef (P.dataDeclParams resolvedDataDecl)
      ctorEntries <-
        forM (P.dataDeclConstructors resolvedDataDecl) $ \ctor -> do
          let ctorSymbol = P.constructorDeclName ctor
              argTypes = resolvedConstructorFieldTypes (P.constructorDeclType ctor)
          pure (ctor, ctorSymbol, argTypes)
      let deriveGen0 =
            identityGeneratorAfter
              ( concatMap resolvedTypeBinderGeneratedIdentities paramRefs
                  ++ resolvedDeclGeneratedIdentities (P.DeclData resolvedDataDecl)
              )
          derivedConstraintParamNames = Set.fromList (derivedConstraintParams displayDataDecl)
          headTy = dataDeclHeadResolvedType dataSymbol paramRefs
          (leftRef, deriveGen1) = freshLocalRef "left" deriveGen0
          (rightRef, deriveGen2) = freshLocalRef "right" deriveGen1
          left = P.Param leftRef (Just headTy)
          right = P.Param rightRef (Just headTy)
          selfName = "__derived_eq_" ++ dataName0
          (selfRef, deriveGen3) = freshLocalRef selfName deriveGen2
          (recursiveBody, _) =
            deriveEqBody eqMethodSymbol andSymbol headTy ctorEntries leftRef rightRef (Just selfRef) deriveGen3
          (nonRecursiveBody, _) =
            deriveEqBody eqMethodSymbol andSymbol headTy ctorEntries leftRef rightRef Nothing deriveGen3
          methodBody =
            if hasRecursiveOwnerFields displayDataDecl
              then
                P.ELet
                  selfRef
                  (Just (RSTArrow headTy (RSTArrow headTy (RSTBase boolSymbol))))
                  (P.ELam left (P.ELam right recursiveBody))
                  (P.EVar (P.ResolvedLocalValue selfRef))
              else
                P.ELam left (P.ELam right nonRecursiveBody)
      pure
        P.InstanceDecl
            { P.instanceDeclClass = classSymbol,
              P.instanceDeclConstraints =
                [ P.ClassConstraint
                    { P.constraintClassName = classSymbol,
                      P.constraintTypes = RSTVar paramRef :| []
                    }
                  | paramRef <- paramRefs,
                    resolvedSrcTypeBinderName paramRef `Set.member` derivedConstraintParamNames
                ],
              P.instanceDeclTypes = headTy :| [],
              P.instanceDeclMethods = [P.MethodDef eqMethodSymbol methodBody]
            }
      where
        resolvedDataParamRef :: P.TypeParam -> TcM ResolvedTypeBinderRef
        resolvedDataParamRef param =
          case P.typeParamRef param of
            Just ref -> pure ref
            Nothing ->
              throwError $
                ProgramPipelineError
                  ("resolved data parameter `" ++ P.typeParamName param ++ "` is missing identity")

    hasRecursiveOwnerFields dataDecl =
      any (isRecursiveOwnerField dataDecl) (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))

    deriveEqBody eqMethodSymbol andSymbol ownerHeadTy ctorEntries leftRef rightRef mbSelfRef generator0 =
      let (alts, generator1) = deriveCtorAlts generator0 ctorEntries
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

    isRecursiveOwnerField dataDecl argTy =
      argTy == dataDeclHeadType dataDecl

    dataDeclHeadType dataDecl =
      case P.typeParamNames (P.dataDeclParams dataDecl) of
        [] -> STBase (P.dataDeclName dataDecl)
        param0 : paramsRest -> STCon (P.dataDeclName dataDecl) (STVar param0 :| map STVar paramsRest)

    dataDeclHeadResolvedType dataSymbol paramRefs =
      case paramRefs of
        [] -> RSTBase dataSymbol
        param0 : paramsRest -> RSTCon dataSymbol (RSTVar param0 :| map RSTVar paramsRest)

    isRecursiveOwnerResolvedField ownerHeadTy argTy =
      typeViewIdentity (typeViewFromResolved argTy) == typeViewIdentity (typeViewFromResolved ownerHeadTy)

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
  case ambiguousFunctionalDependencyInstances infos of
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
      let instanceHeadTypes0 = typeViewsDisplay instanceHeadViews0
          instanceHeadIdentityTypes0 = typeViewsIdentity instanceHeadViews0
      declaredInstanceConstraints0 <- mapM (displayClassConstraintForResolved displayEnv) (P.instanceDeclConstraints instDecl)
      declaredInstanceConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.instanceDeclConstraints instDecl)
      let superclassConstraints0 =
            map
              (substituteConstraintTypes (classParamNames classInfo) instanceHeadTypes0)
              (classSuperclasses classInfo)
          superclassConstraintInfos0 =
            map
              (applyConstraintInfoSubst (typeViewSubstFromParamIdentities (classParamBinderIdentities classInfo) instanceHeadViews0))
              (classSuperclassInfos classInfo)
          instanceConstraints0 = declaredInstanceConstraints0 ++ superclassConstraints0
          instanceConstraintInfos0 = declaredInstanceConstraintInfos0 ++ superclassConstraintInfos0
      let methodMapByIdentity = classMethodsByIdentity classInfo
          expected = Map.keysSet methodMapByIdentity
          provided = Set.fromList (map (resolvedSymbolIdentity . P.methodDefName) (P.instanceDeclMethods instDecl))
      case Set.toList (expected Set.\\ provided) of
        (missing : _) ->
          case Map.lookup missing methodMapByIdentity of
            Just methodInfo -> throwError (ProgramMissingInstanceMethod instanceClassName0 (methodName methodInfo))
            Nothing -> throwError (ProgramMissingInstanceMethod instanceClassName0 (symbolDefiningName missing))
        [] -> pure ()
      case Set.toList (provided Set.\\ expected) of
        (extra : _) ->
          let extraName =
                case find ((== extra) . resolvedSymbolIdentity . P.methodDefName) (P.instanceDeclMethods instDecl) of
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
                methodValueConstraintInfos =
                  declaredInstanceConstraintInfos0
                    ++ map
                      (applyConstraintInfoSubst (typeViewSubstFromParamIdentities (classParamBinderIdentities classInfo) instanceHeadViews0))
                      (methodConstraintInfos methodInfo)
                methodValueConstraints =
                  map displayConstraint methodValueConstraintInfos
                methodValueView =
                  specializeMethodTypeView methodInfo instanceHeadViews0
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
                      valueConstraints = methodValueConstraints,
                      valueConstraintInfos = methodValueConstraintInfos
                    }
            pure ((methodName0, methodInfo, methodValue), generator1')
      (instanceMethodEntries, generator1') <- buildInstanceMethodEntries generator0' (P.instanceDeclMethods instDecl)
      let instanceMethodInfosByIdentity =
            Map.fromList
              [ (methodInfoSymbolIdentity methodInfo, valueInfo)
              | (_, methodInfo, valueInfo) <- instanceMethodEntries
              ]
      pure
        ( InstanceInfo
            { instanceClassSymbol = classInfoSymbolIdentity classInfo,
              instanceOriginModuleIdentity = moduleIdentity,
              instanceConstraints = instanceConstraints0,
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

    ambiguousFunctionalDependencyInstances infos =
      [ info
        | info <- infos,
          Just classInfo <- [classInfoForInstance info],
          fundep <- classFunctionalDependencies classInfo,
          Just (determinerIndices, determinedIndices) <- [functionalDependencyIndices classInfo fundep],
          let determinerVars = freeProjectedTypeVars determinerIndices (instanceHeadIdentityTypes info)
              determinedVars = freeProjectedTypeVars determinedIndices (instanceHeadIdentityTypes info),
          not (determinedVars `Set.isSubsetOf` determinerVars)
      ]

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
      let headIdentities =
            mergeTypeViewHeadIdentities
              ( NE.toList leftDeterminers
                  ++ NE.toList rightDeterminers
                  ++ NE.toList leftDetermined
                  ++ NE.toList rightDetermined
              )
          (leftEnv, taggedLeftDeterminers) =
            tagTypeList "__fundep_left__" Map.empty (map typeViewIdentity (NE.toList leftDeterminers))
          (rightEnv, taggedRightDeterminers) =
            tagTypeList "__fundep_right__" Map.empty (map typeViewIdentity (NE.toList rightDeterminers))
          (_, taggedLeftDetermined) =
            tagTypeList "__fundep_left__" leftEnv (map typeViewIdentity (NE.toList leftDetermined))
          (_, taggedRightDetermined) =
            tagTypeList "__fundep_right__" rightEnv (map typeViewIdentity (NE.toList rightDetermined))
      determinerSubst <- unifyProjectionTypes headIdentities Map.empty taggedLeftDeterminers taggedRightDeterminers
      case unifyProjectionTypes headIdentities determinerSubst taggedLeftDetermined taggedRightDetermined of
        Just _ -> Just True
        Nothing -> Just False

    unifyProjectionTypes headIdentities subst left right
      | length left /= length right = Nothing
      | otherwise = foldM (\acc (leftTy, rightTy) -> unifyOverlap headIdentities acc leftTy rightTy) subst (zip left right)

    freeProjectedTypeVars indices tys =
      foldMap freeTypeVarsInType (projectInstanceTypes indices tys)

    functionalDependencyIndices classInfo fundep =
      (,) <$> traverse lookupParamIndex (functionalDependencyDeterminerRefs fundep) <*> traverse lookupParamIndex (functionalDependencyDeterminedRefs fundep)
      where
        paramIndices =
          Map.fromList
            [ (identity, ix)
            | (identity, ix) <- zip (NE.toList (classParamBinderIdentities classInfo)) [(0 :: Int) ..]
            ]
        lookupParamIndex identity = Map.lookup identity paramIndices

    projectInstanceTypes indices tys =
      let values = NE.toList tys
       in fmap (values !!) indices

    classInfoForInstance info =
      case Map.lookup (instanceInfoClassSymbolIdentity info) (scopeClassesByIdentity scope) of
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
          instanceHeadIdentityTypes left /= instanceHeadIdentityTypes right,
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
          instanceHeadIdentityTypes local /= instanceHeadIdentityTypes existing,
          instanceHeadViewsOverlap (instanceHeadTypeViews local) (instanceHeadTypeViews existing)
      ]

    sameInstanceClass left right =
      instanceClassIdentity left == instanceClassIdentity right

    sameCanonicalInstanceHead left right =
      sameInstanceClass left right
        && instanceHeadIdentityTypes left == instanceHeadIdentityTypes right

    instanceHeadViewsOverlap left right =
      NE.length left == NE.length right
        && case
          foldM
            ( \(subst, ix) (leftView, rightView) -> do
                subst' <-
                  unifyOverlap
                    overlapHeadIdentities
                    subst
                    (tagTypeVars ("__overlap_left" ++ show ix ++ "__") (typeViewIdentity leftView))
                    (tagTypeVars ("__overlap_right" ++ show ix ++ "__") (typeViewIdentity rightView))
                pure (subst', ix + 1)
            )
            (Map.empty, 0 :: Int)
            (zip (NE.toList left) (NE.toList right))
        of
          Just _ -> True
          Nothing -> False
      where
        overlapHeadIdentities =
          mergeTypeViewHeadIdentities (NE.toList left ++ NE.toList right)

    mergeTypeViewHeadIdentities views =
      mergeSymbolIdentityMaps
        [ Map.singleton name identity
        | view <- views,
          (name, identity) <- typeViewHeadIdentityAliases view
        ]

    typeViewHeadIdentityAliases view =
      concat
        [ (name, identity) : [(alias, identity) | alias <- symbolIdentityAliasNames identity]
        | (name, identity) <- Map.toList (typeViewHeadIdentities view)
        ]

    unifyOverlap headIdentities subst left right =
      case (applyOverlapSubst subst left, applyOverlapSubst subst right) of
        (STVar name, ty) -> bindOverlap headIdentities name ty subst
        (ty, STVar name) -> bindOverlap headIdentities name ty subst
        (STBase leftName, STBase rightName)
          | sameOverlapHead headIdentities leftName rightName -> Just subst
        (STCon leftName leftArgs, STCon rightName rightArgs)
          | sameOverlapHead headIdentities leftName rightName && NE.length leftArgs == NE.length rightArgs ->
              foldM
                (\acc (leftTy, rightTy) -> unifyOverlap headIdentities acc leftTy rightTy)
                subst
                (zip (NE.toList leftArgs) (NE.toList rightArgs))
        (STTyLam leftName leftBody, STTyLam rightName rightBody)
          | leftName == rightName -> unifyOverlap headIdentities subst leftBody rightBody
        (STTyApp leftFun leftArg, STTyApp rightFun rightArg) -> do
          subst' <- unifyOverlap headIdentities subst leftFun rightFun
          unifyOverlap headIdentities subst' leftArg rightArg
        (STVarApp leftName leftArgs, rightTy) ->
          unifyOverlapTypeHead headIdentities subst leftName (NE.toList leftArgs) rightTy
        (leftTy, STVarApp rightName rightArgs) ->
          unifyOverlapTypeHead headIdentities subst rightName (NE.toList rightArgs) leftTy
        (STArrow leftDom leftCod, STArrow rightDom rightCod) -> do
          subst' <- unifyOverlap headIdentities subst leftDom rightDom
          unifyOverlap headIdentities subst' leftCod rightCod
        _ -> Nothing

    sameOverlapHead headIdentities leftName rightName =
      let leftIdentities = overlapHeadDataIdentities headIdentities leftName
          rightIdentities = overlapHeadDataIdentities headIdentities rightName
       in if Set.null leftIdentities && Set.null rightIdentities
            then leftName == rightName
            else not (Set.null (Set.intersection leftIdentities rightIdentities))

    overlapHeadDataIdentities headIdentities name =
      case Map.lookup name headIdentities of
        Just identity
          | Map.member identity overlapDataByIdentity -> Set.singleton identity
          | otherwise -> Set.empty
        Nothing -> Set.empty

    overlapDataByIdentity =
      checkedDataByIdentity (scopeElaborateTypes scope)

    unifyOverlapTypeHead headIdentities subst name templateArgs actual =
      case actual of
        STCon actualName actualArgs ->
          matchAppliedHead (STBase actualName) (NE.toList actualArgs)
        STVarApp actualName actualArgs ->
          matchAppliedHead (STVar actualName) (NE.toList actualArgs)
        _ -> Nothing
      where
        templateArgCount = length templateArgs

        matchAppliedHead headTy actualArgs
          | length actualArgs < templateArgCount = Nothing
          | otherwise = do
              let (headArgs, matchedArgs) = splitAt (length actualArgs - templateArgCount) actualArgs
              appliedHead <- applyTypeHead headTy headArgs
              subst' <- bindOverlap headIdentities name appliedHead subst
              foldM
                (\acc (templateTy, actualTy) -> unifyOverlap headIdentities acc templateTy actualTy)
                subst'
                (zip templateArgs matchedArgs)

    bindOverlap headIdentities name ty subst =
      case Map.lookup name subst of
        Just existing -> unifyOverlap headIdentities subst existing ty
        Nothing
          | ty == STVar name -> Just subst
          | name `Set.member` freeTypeVarsInType ty -> Nothing
          | otherwise -> Just (Map.insert name ty subst)

    applyOverlapSubst subst ty =
      case ty of
        STVar name ->
          case Map.lookup name subst of
            Just replacement -> applyOverlapSubst subst replacement
            Nothing -> ty
        STArrow dom cod -> STArrow (applyOverlapSubst subst dom) (applyOverlapSubst subst cod)
        STCon name args -> STCon name (fmap (applyOverlapSubst subst) args)
        STVarApp name args ->
          let args' = fmap (applyOverlapSubst subst) args
           in case Map.lookup name subst >>= \replacement -> applyTypeHead replacement (NE.toList args') of
                Just replacementTy -> replacementTy
                Nothing -> STVarApp name args'
        STTyLam name body ->
          STTyLam name (applyOverlapSubst (Map.delete name subst) body)
        STTyApp fun arg -> STTyApp (applyOverlapSubst subst fun) (applyOverlapSubst subst arg)
        STForall name mb body ->
          let subst' = Map.delete name subst
           in STForall name (fmap (SrcBound . applyOverlapSubst subst' . unSrcBound) mb) (applyOverlapSubst subst' body)
        STMu name body -> STMu name (applyOverlapSubst subst body)
        STBase {} -> ty
        STBottom -> STBottom

    tagTypeVars prefix = go Map.empty
      where
        go env ty =
          case ty of
            STVar name -> STVar (Map.findWithDefault (prefix ++ name) name env)
            STArrow dom cod -> STArrow (go env dom) (go env cod)
            STCon name args -> STCon name (fmap (go env) args)
            STVarApp name args -> STVarApp (Map.findWithDefault (prefix ++ name) name env) (fmap (go env) args)
            STTyLam name body ->
              let tagged = prefix ++ name
               in STTyLam tagged (go (Map.insert name tagged env) body)
            STTyApp fun arg -> STTyApp (go env fun) (go env arg)
            STForall name mb body ->
              let tagged = prefix ++ name
                  env' = Map.insert name tagged env
               in STForall tagged (fmap (SrcBound . go env . unSrcBound) mb) (go env' body)
            STMu name body ->
              let tagged = prefix ++ name
               in STMu tagged (go (Map.insert name tagged env) body)
            STBase {} -> ty
            STBottom -> STBottom

    tagTypeList prefix env0 tys =
      foldl step (env0, []) tys
      where
        step (env, acc) ty =
          let (env', ty') = tagTypeWithEnv prefix env ty
           in (env', acc ++ [ty'])

    tagTypeWithEnv prefix env ty =
      case ty of
        STVar name ->
          let (env', tagged) = lookupOrTag prefix env name
           in (env', STVar tagged)
        STArrow dom cod ->
          let (env1, dom') = tagTypeWithEnv prefix env dom
              (env2, cod') = tagTypeWithEnv prefix env1 cod
           in (env2, STArrow dom' cod')
        STCon name args ->
          let (env', args') = tagTypeList prefix env (NE.toList args)
           in (env', STCon name (NE.fromList args'))
        STVarApp name args ->
          let (env1, tagged) = lookupOrTag prefix env name
              (env2, args') = tagTypeList prefix env1 (NE.toList args)
           in (env2, STVarApp tagged (NE.fromList args'))
        STTyLam name body ->
          let tagged = prefix ++ name
              previous = Map.lookup name env
              (env', body') = tagTypeWithEnv prefix (Map.insert name tagged env) body
           in (restoreTagBinding name previous env', STTyLam tagged body')
        STTyApp fun arg ->
          let (env1, fun') = tagTypeWithEnv prefix env fun
              (env2, arg') = tagTypeWithEnv prefix env1 arg
           in (env2, STTyApp fun' arg')
        STForall name mb body ->
          let tagged = prefix ++ name
              previous = Map.lookup name env
              (env1, mb') = tagMaybeBound env mb
              (env2, body') = tagTypeWithEnv prefix (Map.insert name tagged env1) body
           in (restoreTagBinding name previous env2, STForall tagged mb' body')
          where
            tagMaybeBound envBound Nothing = (envBound, Nothing)
            tagMaybeBound envBound (Just bound) =
              let (env', bound') = tagTypeWithEnv prefix envBound (unSrcBound bound)
               in (env', Just (SrcBound bound'))
        STMu name body ->
          let tagged = prefix ++ name
              previous = Map.lookup name env
              (env', body') = tagTypeWithEnv prefix (Map.insert name tagged env) body
           in (restoreTagBinding name previous env', STMu tagged body')
        STBase {} -> (env, ty)
        STBottom -> (env, STBottom)

    lookupOrTag prefix env name =
      case Map.lookup name env of
        Just tagged -> (env, tagged)
        Nothing ->
          let tagged = prefix ++ name
           in (Map.insert name tagged env, tagged)

    restoreTagBinding name previous env =
      case previous of
        Just tagged -> Map.insert name tagged env
        Nothing -> Map.delete name env

    freeTypeVarsInType ty =
      case ty of
        STVar name -> Set.singleton name
        STArrow dom cod -> freeTypeVarsInType dom `Set.union` freeTypeVarsInType cod
        STCon _ args -> foldMap freeTypeVarsInType args
        STVarApp name args -> Set.insert name (foldMap freeTypeVarsInType args)
        STTyLam name body -> Set.delete name (freeTypeVarsInType body)
        STTyApp fun arg -> freeTypeVarsInType fun `Set.union` freeTypeVarsInType arg
        STForall name mb body ->
          maybe Set.empty (freeTypeVarsInType . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVarsInType body)
        STMu name body -> Set.delete name (freeTypeVarsInType body)
        STBase {} -> Set.empty
        STBottom -> Set.empty

    substituteConstraintTypes paramNames headTys constraint =
      constraint
        { P.constraintTypes = fmap (substituteTypeVars paramNames headTys) (P.constraintTypes constraint)
        }

    substituteTypeVars paramNames headTys ty =
      Map.foldrWithKey substituteTypeVar ty (Map.fromList (zip (NE.toList paramNames) (NE.toList headTys)))

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

checkDef :: FinalizeContext -> ElaborateScope -> Scope -> P.ResolvedDefDecl -> TcM CheckedBinding
checkDef finalizeContext elaborateScope scope defDecl = do
  let defName = P.refDisplayName (P.defDeclName defDecl)
  valueInfo <- lookupValueInfoBySymbol scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      liftEither
        ( lowerResolvedConstrainedExprBinding elaborateScope (loweredBindingIdentityFromValueInfo ordinary) (P.defDeclType defDecl) (resolvedDefDeclIsMain defDecl) (P.defDeclExpr defDecl)
            >>= finalizeBindingAllowOpaqueWithContext finalizeContext
        )
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

identityExportIndex :: (a -> SymbolIdentity) -> Map String a -> IdentityExportIndex a
identityExportIndex identityFor values =
  ( Map.fromListWith
      (flip const)
      [ (identityFor info, info)
      | (_, info) <- Map.toList values
      ],
    uniqueDisplayByIdentity identityFor values
  )

lookupIdentityExport :: SymbolIdentity -> IdentityExportIndex a -> Maybe (String, a)
lookupIdentityExport identity (infos, displays) =
  (,) <$> Map.lookup identity displays <*> Map.lookup identity infos

type SelectedExports a = Map SymbolIdentity (String, a)

selectedExportInfos :: SelectedExports a -> Map SymbolIdentity a
selectedExportInfos =
  fmap snd

selectedExportDisplays :: SelectedExports a -> Map SymbolIdentity String
selectedExportDisplays =
  fmap fst

insertSelectedExport :: (a -> SymbolIdentity) -> String -> a -> SelectedExports a -> Either ProgramError (SelectedExports a)
insertSelectedExport identityFor displayName info acc =
  case Map.lookup identity acc of
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
                      valueRuntimeName = ctorRuntimeName ctor,
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
        Just dataInfo <- [Map.lookup (resolvedSymbolIdentity symbol) builtinOpaqueTypesByIdentity]
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

ensureDistinctBy :: (Eq a) => (a -> ProgramError) -> (b -> a) -> [b] -> TcM ()
ensureDistinctBy mkErr project values = ensureDistinctPlain mkErr (map project values)

ensureDistinctImportAliases :: [P.ImportF p] -> TcM ()
ensureDistinctImportAliases imports0 =
  ensureDistinctPlain ProgramDuplicateImportAlias [alias | Just alias <- map P.importAlias imports0]

ensureDistinctPlain :: (Eq a) => (a -> ProgramError) -> [a] -> TcM ()
ensureDistinctPlain mkErr values =
  case duplicates values of
    (dup : _) -> throwError (mkErr dup)
    [] -> pure ()

duplicates :: (Eq a) => [a] -> [a]
duplicates values = [x | x <- nub values, length (filter (== x) values) > 1]

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
