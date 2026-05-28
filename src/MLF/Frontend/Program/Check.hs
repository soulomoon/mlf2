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
import Data.List (find, intercalate, nub, partition, transpose)
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
    sourceTypeViewInScope,
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
    finalizeBindingWithContext,
    finalizeBindingAllowOpaqueWithContext,
    finalizeBindingAllowOpaqueWithContextWithTiming,
    mkFinalizeContext,
    mkModuleFinalizeContext,
  )
import MLF.Frontend.Program.Interface
  ( ModuleInterface,
    ProgramInterfaceError,
    moduleInterfaceData,
    moduleInterfaceExports,
    moduleInterfaceId,
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
import MLF.Frontend.Program.TypeFamilies (normalizeTypeFamiliesInProgram)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ClassInfo (..),
    ConstructorShape (..),
    ConstructorInfo (..),
    DataInfo (..),
    ExportedTypeInfo (..),
    InstanceInfo (..),
    LoweredBinding (..),
    MethodInfo (..),
    ModuleExports (..),
    ProgramDiagnostic (..),
    ProgramError (..),
    ResolvedLocalSymbols (..),
    ResolvedProgram (..),
    ResolvedSemanticModule (..),
    ResolvedSemanticProgramArtifact (..),
    ResolvedSymbol (..),
    SymbolOrigin (..),
    SymbolIdentity (..),
    SymbolNamespace (..),
    SymbolOwnerIdentity (..),
    ConstraintInfo (..),
    TypeView (..),
    ValueInfo (..),
    applyTypeHead,
    applyConstraintInfoSubst,
    classInfoSymbolIdentity,
    constrainedVisibleType,
    constructorInfoSymbolIdentity,
    constructorOwnerShapes,
    constructorShapeFromInfo,
    dataInfoSymbolIdentity,
    diagnosticForProgramError,
    instanceInfoClassSymbolIdentity,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    mkResolvedSymbol,
    resolvedProgramSemanticArtifact,
    displayConstraint,
    specializeMethodTypes,
    substituteTypeVar,
    splitArrows,
    splitForalls,
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
    SrcBound (..),
    SrcTy (..),
    SrcType,
    resolvedSrcTypeIdentityType,
    resolvedSrcTypeToSrcType,
  )
import qualified MLF.Frontend.Syntax.Program as P
import System.IO.Unsafe (unsafePerformIO)
import MLF.Frontend.TypeLevel (TypeFamilyDecl, familyDeclName)

type TcM a = Either ProgramError a

runTcM :: TcM a -> Either ProgramError a
runTcM = id

-- Scope ----------------------------------------------------------------------

data Scope = Scope
  { scopeValues :: Map String ValueInfo,
    scopeValuesByIdentity :: Map SymbolIdentity [(String, ValueInfo)],
    scopeTypes :: Map String DataInfo,
    scopeTypesByIdentity :: Map SymbolIdentity [(String, DataInfo)],
    scopeHiddenTypes :: Map String DataInfo,
    scopeClasses :: Map String ClassInfo,
    scopeClassesByIdentity :: Map SymbolIdentity [(String, ClassInfo)],
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
    kindTypeVariables :: Map String KindTerm,
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
      scopeTypesByIdentity = indexByIdentity dataInfoSymbolIdentity types0,
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

indexByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity [(String, a)]
indexByIdentity identityOf =
  Map.fromListWith (++) . map (\(name, info) -> (identityOf info, [(name, info)])) . Map.toList

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
              methodInfo <- Map.elems (classMethods classInfo)
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

lookupValueInfo :: Scope -> String -> TcM ValueInfo
lookupValueInfo scope name =
  case Map.lookup name (scopeValues scope) of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownValue name)

lookupClassInfoBySymbol :: Scope -> ResolvedSymbol -> TcM ClassInfo
lookupClassInfoBySymbol scope symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) (scopeClassesByIdentity scope) of
    Just ((_, info) : _) -> pure info
    Nothing -> throwError (ProgramUnknownClass (resolvedSymbolDisplayName symbol))
    Just [] -> throwError (ProgramUnknownClass (resolvedSymbolDisplayName symbol))

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
  let mainNames =
        [ checkedBindingName binding
          | checked <- modulesChecked,
            binding <- checkedModuleBindings checked,
            checkedBindingExportedAsMain binding
        ]
  mainRuntime <-
    case mainNames of
      [] -> throwError ProgramMainNotFound
      [name] -> pure name
      names -> throwError (ProgramMultipleMainDefinitions names)
  pure (checkedProgramFromModules resolved modulesChecked mainRuntime)

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

checkedProgramFromModules :: ResolvedProgram -> [CheckedModule] -> String -> CheckedProgram
checkedProgramFromModules resolved modulesChecked mainRuntime =
  CheckedProgram
    { checkedProgramModules = modulesChecked,
      checkedProgramMain = mainRuntime,
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

lookupResolvedLocalTypeIdentity :: ResolvedSemanticModule -> P.TypeName -> TcM SymbolIdentity
lookupResolvedLocalTypeIdentity resolvedModule name =
  resolvedSymbolIdentity <$>
    uniqueResolvedLocalSymbol
      ProgramDuplicateType
      name
      (resolvedLocalTypes (resolvedSemanticModuleLocalSymbols resolvedModule))

lookupResolvedLocalClassIdentity :: ResolvedSemanticModule -> P.ClassName -> TcM SymbolIdentity
lookupResolvedLocalClassIdentity resolvedModule name =
  resolvedSymbolIdentity <$>
    uniqueResolvedLocalSymbol
      ProgramDuplicateClass
      name
      (resolvedLocalClasses (resolvedSemanticModuleLocalSymbols resolvedModule))

lookupResolvedLocalValueIdentity ::
  ResolvedSemanticModule ->
  SymbolNamespace ->
  Maybe SymbolOwnerIdentity ->
  P.ValueName ->
  TcM SymbolIdentity
lookupResolvedLocalValueIdentity resolvedModule namespace owner name =
  resolvedSymbolIdentity <$> uniqueResolvedLocalSymbol ProgramDuplicateValue name matchingSymbols
  where
    matchingSymbols =
      Map.map
        ( filter
            ( \symbol ->
                let identity = resolvedSymbolIdentity symbol
                 in symbolNamespace identity == namespace
                      && symbolOwnerIdentity identity == owner
            )
        )
        (resolvedLocalValues (resolvedSemanticModuleLocalSymbols resolvedModule))

lookupResolvedLocalValueSymbol ::
  ResolvedSemanticModule ->
  SymbolNamespace ->
  Maybe SymbolOwnerIdentity ->
  P.ValueName ->
  TcM ResolvedSymbol
lookupResolvedLocalValueSymbol resolvedModule namespace owner name =
  uniqueResolvedLocalSymbol ProgramDuplicateValue name matchingSymbols
  where
    matchingSymbols =
      Map.map
        ( filter
            ( \symbol ->
                let identity = resolvedSymbolIdentity symbol
                 in symbolNamespace identity == namespace
                      && symbolOwnerIdentity identity == owner
            )
        )
        (resolvedLocalValues (resolvedSemanticModuleLocalSymbols resolvedModule))

uniqueResolvedLocalSymbol ::
  (String -> ProgramError) ->
  String ->
  Map String [ResolvedSymbol] ->
  TcM ResolvedSymbol
uniqueResolvedLocalSymbol duplicateErr name symbolsByName =
  case Map.lookup name symbolsByName of
    Just [symbol] -> pure symbol
    Just [] -> throwError (duplicateErr name)
    Just _ -> throwError (duplicateErr name)
    Nothing -> throwError (duplicateErr name)

checkModules :: Maybe PackageModuleGraph -> ResolvedSemanticProgramArtifact -> TcM [CheckedModule]
checkModules mbGraph (ResolvedSemanticProgramArtifact resolvedModules) = do
  ensureDistinctBy ProgramDuplicateModule resolvedSemanticModuleName resolvedModules
  go [] [] resolvedModules
  where
    nodesByModule =
      Map.fromList
        [ (packageModuleName (packageModuleGraphNodeId node), node)
          | graph <- maybe [] pure mbGraph,
            node <- packageModuleGraphNodes graph
        ]

    go _ checkedAcc [] = pure (reverse checkedAcc)
    go interfaceAcc checkedAcc (resolvedModule : rest) = do
      checked <-
        if isBuiltinPreludeModule nodesByModule mbGraph resolvedModule
          then checkedBuiltinPreludeModule resolvedModule
          else checkModule resolvedModule interfaceAcc
      node <- moduleInterfaceNodeForResolved nodesByModule mbGraph resolvedModule
      interface <- liftEitherWithInterface (moduleInterfaceFromCheckedModule node checked)
      go (interface : interfaceAcc) (checked : checkedAcc) rest

checkModulesWithTiming :: TimingConfig -> Maybe PackageModuleGraph -> ResolvedSemanticProgramArtifact -> IO (TcM [CheckedModule])
checkModulesWithTiming timing mbGraph (ResolvedSemanticProgramArtifact resolvedModules) = do
  distinctResult <-
    timeProgramDetailIO
      timing
      "program.check.modules.distinct"
      (evaluate (ensureDistinctBy ProgramDuplicateModule resolvedSemanticModuleName resolvedModules))
  case distinctResult of
    Left err ->
      pure (Left err)
    Right () ->
      go [] [] resolvedModules
  where
    nodesByModule =
      Map.fromList
        [ (packageModuleName (packageModuleGraphNodeId node), node)
          | graph <- maybe [] pure mbGraph,
            node <- packageModuleGraphNodes graph
        ]

    go _ checkedAcc [] =
      pure (Right (reverse checkedAcc))
    go interfaceAcc checkedAcc (resolvedModule : rest) = do
      let moduleName0 = resolvedSemanticModuleName resolvedModule
          isBuiltinPrelude = isBuiltinPreludeModule nodesByModule mbGraph resolvedModule
      checkedResult <-
        if isBuiltinPrelude
          then
            timeProgramDetailIO
              timing
              ("program.check.module." ++ moduleName0 ++ ".cache")
              (evaluate (checkedBuiltinPreludeModule resolvedModule))
          else
            timeProgramDetailIO
              timing
              ("program.check.module." ++ moduleName0)
              (checkModuleWithTiming timing resolvedModule interfaceAcc)
      case checkedResult of
        Left err ->
          pure (Left err)
        Right checked -> do
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
              go (interface : interfaceAcc) (checked : checkedAcc) rest

builtinPreludeSourcePath :: FilePath
builtinPreludeSourcePath = "<mlfp-prelude>"

builtinPreludeCheckCache :: MVar (Maybe (TcM CheckedModule))
builtinPreludeCheckCache = unsafePerformIO (newMVar Nothing)
{-# NOINLINE builtinPreludeCheckCache #-}

checkedBuiltinPreludeModule :: ResolvedSemanticModule -> TcM CheckedModule
checkedBuiltinPreludeModule resolvedModule =
  unsafePerformIO $
    modifyMVar builtinPreludeCheckCache $ \case
      Just cached ->
        pure (Just cached, cached)
      Nothing -> do
        checked <- evaluate (checkModule resolvedModule [])
        pure (Just checked, checked)
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

checkModule :: ResolvedSemanticModule -> [ModuleInterface] -> TcM CheckedModule
checkModule resolvedModule priorInterfaces = do
  let resolvedSyntax = resolvedSemanticModuleSyntax resolvedModule
      moduleName0 = resolvedSemanticModuleName resolvedModule
      priorExports = Map.fromList [(moduleInterfaceName interface, moduleInterfaceExports interface) | interface <- priorInterfaces]
      priorData = Map.fromList [(moduleInterfaceName interface, moduleInterfaceData interface) | interface <- priorInterfaces]
      priorInstances = concatMap moduleInterfaceInstances priorInterfaces
      unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExports (P.moduleImports resolvedSyntax)
      visibleImportedInstances =
        visibleInstancesForImports priorExports priorData priorInstances unqualifiedClassIdentities (P.moduleImports resolvedSyntax)
  ensureDistinctImportAliases (P.moduleImports resolvedSyntax)
  rejectUnsupportedTypeFamilies resolvedSyntax
  rejectUnsupportedGeneralizedClassFeaturesModule P.refDisplayName resolvedSrcTypeToSrcType resolvedSyntax
  importScope <- buildImportScopeResolved priorExports (P.moduleImports resolvedSyntax)
  let importedEnv = displayNameEnvFromScope importScope
      localSymbolEnv = displayNameEnvFromResolvedLocals resolvedModule
      baseNameEnv = localSymbolEnv `preferDisplayNames` importedEnv
  localData <- buildLocalDataInfo baseNameEnv resolvedModule resolvedSyntax
  let dataNameEnv = displayNameEnvFromData localData `preferDisplayNames` baseNameEnv
  localClasses <- buildLocalClassInfo dataNameEnv resolvedModule resolvedSyntax
  let classNameEnv = displayNameEnvFromClasses localClasses `preferDisplayNames` dataNameEnv
  localDefs <- buildLocalDefInfo classNameEnv resolvedModule resolvedSyntax
  localValues0 <- addConstructorValues moduleName0 localData
  localValues1 <- mergeMaps ProgramDuplicateValue localValues0 localDefs
  let localMethodValues =
        Map.fromList
          [ ( methodName method,
              OverloadedMethod
                { valueDisplayName = methodName method,
                  valueInfoSymbol = methodInfoSymbolIdentity method,
                  valueMethodInfo = method,
                  valueOriginModule = moduleName0
                }
            )
            | classInfo <- Map.elems localClasses,
              method <- Map.elems (classMethods classInfo)
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
  derivedInstances <- synthesizeDerivedInstances fullNameEnv scope0 resolvedModule resolvedSyntax
  instanceSkeletons <- buildInstanceSkeletons fullNameEnv scope0 resolvedSyntax derivedInstances
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
  instanceBindings <- checkInstances finalizeContext elaborateScope scope1 (derivedInstances ++ explicitInstances resolvedSyntax)
  defBindings <- mapM (checkDef finalizeContext elaborateScope scope1) (moduleDefDecls resolvedSyntax)
  exports <- buildExports resolvedSyntax localData localClasses localValues
  let exportedMainRuntime =
        case Map.lookup "main" (exportedValues exports) of
          Just OrdinaryValue {valueRuntimeName = runtimeName} -> Just runtimeName
          _ -> Nothing
      markExportedMain binding =
        binding
          { checkedBindingExportedAsMain =
              Just (checkedBindingName binding) == exportedMainRuntime
          }
  pure
    CheckedModule
      { checkedModuleName = moduleName0,
        checkedModuleBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings,
        checkedModuleData = localData,
        checkedModuleClasses = localClasses,
        checkedModuleInstances = instanceSkeletons,
        checkedModuleExports = exports
      }

checkModuleWithTiming :: TimingConfig -> ResolvedSemanticModule -> [ModuleInterface] -> IO (TcM CheckedModule)
checkModuleWithTiming timing resolvedModule priorInterfaces = do
  let resolvedSyntax = resolvedSemanticModuleSyntax resolvedModule
      moduleName0 = resolvedSemanticModuleName resolvedModule
      priorExports = Map.fromList [(moduleInterfaceName interface, moduleInterfaceExports interface) | interface <- priorInterfaces]
      priorData = Map.fromList [(moduleInterfaceName interface, moduleInterfaceData interface) | interface <- priorInterfaces]
      priorInstances = concatMap moduleInterfaceInstances priorInterfaces
      unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExports (P.moduleImports resolvedSyntax)
      visibleImportedInstances =
        visibleInstancesForImports priorExports priorData priorInstances unqualifiedClassIdentities (P.moduleImports resolvedSyntax)
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
      importScopeResult <- timePhase "import-scope" (buildImportScopeResolved priorExports (P.moduleImports resolvedSyntax))
      case importScopeResult of
        Left err -> pure (Left err)
        Right importScope -> do
          let importedEnv = displayNameEnvFromScope importScope
              localSymbolEnv = displayNameEnvFromResolvedLocals resolvedModule
              baseNameEnv = localSymbolEnv `preferDisplayNames` importedEnv
          localDataResult <- timePhase "local-data" (buildLocalDataInfo baseNameEnv resolvedModule resolvedSyntax)
          case localDataResult of
            Left err -> pure (Left err)
            Right localData -> do
              let dataNameEnv = displayNameEnvFromData localData `preferDisplayNames` baseNameEnv
              localClassesResult <- timePhase "local-classes" (buildLocalClassInfo dataNameEnv resolvedModule resolvedSyntax)
              case localClassesResult of
                Left err -> pure (Left err)
                Right localClasses -> do
                  let classNameEnv = displayNameEnvFromClasses localClasses `preferDisplayNames` dataNameEnv
                  localDefsResult <- timePhase "local-defs" (buildLocalDefInfo classNameEnv resolvedModule resolvedSyntax)
                  case localDefsResult of
                    Left err -> pure (Left err)
                    Right localDefs -> do
                      localValuesResult <-
                        timePhase "local-values" $ do
                          localValues0 <- addConstructorValues moduleName0 localData
                          localValues1 <- mergeMaps ProgramDuplicateValue localValues0 localDefs
                          let localMethodValues =
                                Map.fromList
                                  [ ( methodName method,
                                      OverloadedMethod
                                        { valueDisplayName = methodName method,
                                          valueInfoSymbol = methodInfoSymbolIdentity method,
                                          valueMethodInfo = method,
                                          valueOriginModule = moduleName0
                                        }
                                    )
                                    | classInfo <- Map.elems localClasses,
                                      method <- Map.elems (classMethods classInfo)
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
                                  derivedInstancesResult <- timePhase "derived-instances" (synthesizeDerivedInstances fullNameEnv scope0 resolvedModule resolvedSyntax)
                                  case derivedInstancesResult of
                                    Left err -> pure (Left err)
                                    Right derivedInstances -> do
                                      instanceSkeletonsResult <- timePhase "instance-skeletons" (buildInstanceSkeletons fullNameEnv scope0 resolvedSyntax derivedInstances)
                                      case instanceSkeletonsResult of
                                        Left err -> pure (Left err)
                                        Right instanceSkeletons -> do
                                          let scope1 = withScopeInstances (scopeInstances scope0 ++ instanceSkeletons) scope0
                                              elaborateScope = mkElaborateScope (scopeValues scope1) (scopeElaborateTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
                                          finalizeContextResult <- timePhase "finalize-context" (mkFinalizeContext elaborateScope)
                                          case finalizeContextResult of
                                            Left err -> pure (Left err)
                                            Right finalizeContext -> do
                                              finalizeCheckedModuleWithTiming timing moduleName0 resolvedSyntax localData localClasses localValues instanceSkeletons finalizeContext elaborateScope scope1 derivedInstances

finalizeCheckedModuleWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  P.ResolvedModuleSyntax ->
  Map String DataInfo ->
  Map String ClassInfo ->
  Map String ValueInfo ->
  [InstanceInfo] ->
  FinalizeContext ->
  ElaborateScope ->
  Scope ->
  [P.ResolvedInstanceDecl] ->
  IO (TcM CheckedModule)
finalizeCheckedModuleWithTiming timing moduleName0 resolvedSyntax localData localClasses localValues instanceSkeletons finalizeContext elaborateScope scope1 derivedInstances = do
  constructorBindingsResult <-
    timeCheckModulePhaseIO timing moduleName0 "constructor-bindings" $
      checkConstructorsWithTiming timing moduleName0 finalizeContext elaborateScope localData
  case constructorBindingsResult of
    Left err -> pure (Left err)
    Right constructorBindings -> do
      instanceBindingsResult <-
        timeCheckModulePhaseIO timing moduleName0 "instance-bindings" $
          checkInstancesWithTiming timing moduleName0 finalizeContext elaborateScope scope1 (derivedInstances ++ explicitInstances resolvedSyntax)
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
                let exportedMainRuntime =
                      case Map.lookup "main" (exportedValues exports) of
                        Just OrdinaryValue {valueRuntimeName = runtimeName} -> Just runtimeName
                        _ -> Nothing
                    markExportedMain binding =
                      binding
                        { checkedBindingExportedAsMain =
                            Just (checkedBindingName binding) == exportedMainRuntime
                        }
                pure
                  CheckedModule
                    { checkedModuleName = moduleName0,
                      checkedModuleBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings,
                      checkedModuleData = localData,
                      checkedModuleClasses = localClasses,
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

checkInstancesWithTiming :: TimingConfig -> P.ModuleName -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedInstanceDecl] -> IO (TcM [CheckedBinding])
checkInstancesWithTiming timing moduleName0 finalizeContext elaborateScope scope instDecls =
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

lowerInstanceWithTiming :: TimingConfig -> P.ModuleName -> String -> ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> IO (TcM [LoweredBinding])
lowerInstanceWithTiming timing moduleName0 instanceLabel elaborateScope scope instDecl = do
  instanceResult <-
    timeCheckModuleOperation timing moduleName0 (instanceLabel ++ ".lookup") $
      lookupInstanceForDecl scope instDecl
  case instanceResult of
    Left err -> pure (Left err)
    Right (classInfo, instanceInfo) ->
      lowerInstanceMethodsWithTiming timing moduleName0 instanceLabel elaborateScope classInfo instanceInfo (P.instanceDeclMethods instDecl)

lowerInstanceMethodsWithTiming :: TimingConfig -> P.ModuleName -> String -> ElaborateScope -> ClassInfo -> InstanceInfo -> [P.ResolvedMethodDef] -> IO (TcM [LoweredBinding])
lowerInstanceMethodsWithTiming timing moduleName0 instanceLabel elaborateScope classInfo instanceInfo methodDefs =
  go [] methodDefs
  where
    go acc [] = pure (Right (reverse acc))
    go acc (methodDef : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 (instanceLabel ++ ".method." ++ P.methodDefName methodDef ++ ".lower") $
          lowerInstanceMethod elaborateScope classInfo instanceInfo methodDef
      case result of
        Left err -> pure (Left err)
        Right lowered -> go (lowered : acc) rest

checkInstances :: FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedInstanceDecl] -> TcM [CheckedBinding]
checkInstances finalizeContext elaborateScope scope instDecls = do
  lowereds <- concat <$> mapM (lowerInstance elaborateScope scope) instDecls
  liftEither (finalizeBindingsAllowOpaqueWithContext finalizeContext lowereds)

lowerInstance :: ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> TcM [LoweredBinding]
lowerInstance elaborateScope scope instDecl = do
  (classInfo, instanceInfo) <- lookupInstanceForDecl scope instDecl
  mapM (lowerInstanceMethod elaborateScope classInfo instanceInfo) (P.instanceDeclMethods instDecl)

lookupInstanceForDecl :: Scope -> P.ResolvedInstanceDecl -> TcM (ClassInfo, InstanceInfo)
lookupInstanceForDecl scope instDecl = do
  classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
  let headTys = fmap resolvedSrcTypeToSrcType (P.instanceDeclTypes instDecl)
      headIdentityTys = fmap resolvedSrcTypeIdentityType (P.instanceDeclTypes instDecl)
  instanceInfo <-
    case findInstance classInfo headIdentityTys of
      Just info -> pure info
      Nothing ->
        throwError $
          case headTys of
            headTy :| [] -> ProgramNoMatchingInstance (className classInfo) headTy
            tys -> ProgramNoMatchingInstanceHead (className classInfo) (NE.toList tys)
  pure (classInfo, instanceInfo)
  where
    findInstance classInfo headIdentityTys =
      find
        ( \info ->
            instanceClassIdentity info == classIdentity classInfo
              && instanceHeadIdentityTypes info == headIdentityTys
        )
        (scopeInstances scope)

lowerInstanceMethod :: ElaborateScope -> ClassInfo -> InstanceInfo -> P.ResolvedMethodDef -> TcM LoweredBinding
lowerInstanceMethod elaborateScope classInfo instanceInfo methodDef =
  case instanceMethods instanceInfo Map.! P.methodDefName methodDef of
    valueInfo@OrdinaryValue {} -> do
      let methodRuntimeName = valueRuntimeName valueInfo
          methodSourceView =
            TypeView
              { typeViewDisplay = valueType valueInfo,
                typeViewIdentity = valueIdentityType valueInfo
              }
      liftEither
        (lowerConstrainedResolvedExprBinding elaborateScope methodRuntimeName (valueConstraintInfos valueInfo) methodSourceView False (P.methodDefExpr methodDef))
    _ -> throwError (ProgramUnexpectedInstanceMethod (className classInfo) (P.methodDefName methodDef))

data DefWorkItem = DefWorkItem
  { defWorkItemDecl :: P.ResolvedDefDecl,
    defWorkItemLowered :: LoweredBinding,
    defWorkItemDependencies :: [String]
  }

checkDefsWithTiming :: TimingConfig -> P.ModuleName -> FinalizeContext -> ElaborateScope -> Scope -> [P.ResolvedDefDecl] -> IO (TcM [CheckedBinding])
checkDefsWithTiming timing moduleName0 finalizeContext elaborateScope scope defDecls = do
  workItemsResult <- lowerDefWorkItemsWithTiming timing moduleName0 elaborateScope scope defDecls
  case workItemsResult of
    Left err -> pure (Left err)
    Right workItems -> do
      batchSize <- moduleDefBatchSize
      nonRecursiveNamesResult <-
        timeCheckModuleOperation timing moduleName0 "defs.scc_classification" $
          Right (nonRecursiveDefNames workItems)
      case nonRecursiveNamesResult of
        Left err -> pure (Left err)
        Right nonRecursiveNames
          | length workItems >= moduleDefContextMinDefs -> do
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
                    nonRecursiveNames
                    workItems
          | otherwise ->
              finalizeDefWorkItemsWithTiming
                timing
                moduleName0
                finalizeContext
                Nothing
                batchSize
                nonRecursiveNames
                workItems

lowerDefWorkItemsWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  ElaborateScope ->
  Scope ->
  [P.ResolvedDefDecl] ->
  IO (TcM [DefWorkItem])
lowerDefWorkItemsWithTiming timing moduleName0 elaborateScope scope defDecls =
  go [] defDecls
  where
    localDefNames = Set.fromList (map P.defDeclName defDecls)

    go acc [] = pure (Right (reverse acc))
    go acc (defDecl : rest) = do
      result <-
        timeCheckModuleOperation timing moduleName0 ("def." ++ P.defDeclName defDecl ++ ".lower") $
          lowerDefWorkItem elaborateScope scope localDefNames defDecl
      case result of
        Left err -> pure (Left err)
        Right workItem -> go (workItem : acc) rest

lowerDefWorkItem ::
  ElaborateScope ->
  Scope ->
  Set.Set String ->
  P.ResolvedDefDecl ->
  TcM DefWorkItem
lowerDefWorkItem elaborateScope scope localDefNames defDecl = do
  valueInfo <- lookupValueInfo scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      lowered <-
        liftEither $
          lowerResolvedConstrainedExprBinding
            elaborateScope
            (valueRuntimeName ordinary)
            (P.defDeclType defDecl)
            (P.defDeclName defDecl == "main")
            (P.defDeclExpr defDecl)
      pure
        DefWorkItem
          { defWorkItemDecl = defDecl,
            defWorkItemLowered = lowered,
            defWorkItemDependencies = localResolvedDefDependencies localDefNames (P.defDeclExpr defDecl)
          }
    _ -> throwError (ProgramDuplicateValue (P.defDeclName defDecl))

localResolvedDefDependencies :: Set.Set String -> P.ResolvedExpr -> [String]
localResolvedDefDependencies localDefNames expr =
  Set.toList (Set.fromList (collectFreeResolvedValues Set.empty expr) `Set.intersection` localDefNames)

collectFreeResolvedValues :: Set.Set String -> P.ResolvedExpr -> [String]
collectFreeResolvedValues bound expr =
  case expr of
    P.EVar (P.ResolvedLocalValue name)
      | name `Set.member` bound -> []
      | otherwise -> [name]
    P.EVar P.ResolvedGlobalValue {} -> []
    P.ELit {} -> []
    P.ELam param body -> collectFreeResolvedValues (Set.insert (P.paramName param) bound) body
    P.EApp fun arg -> collectFreeResolvedValues bound fun ++ collectFreeResolvedValues bound arg
    P.ELet name _ rhs body -> collectFreeResolvedValues bound rhs ++ collectFreeResolvedValues (Set.insert name bound) body
    P.EAnn inner _ -> collectFreeResolvedValues bound inner
    P.ECase scrutinee alts -> collectFreeResolvedValues bound scrutinee ++ concatMap collectAlt alts
  where
    collectAlt (P.Alt pattern0 body) =
      collectFreeResolvedValues (bound `Set.union` Set.fromList (patternBinders pattern0)) body

    patternBinders = \case
      P.PatCtor _ patterns -> concatMap patternBinders patterns
      P.PatVar name -> [name]
      P.PatWildcard -> []
      P.PatAnn inner _ -> patternBinders inner

nonRecursiveDefNames :: [DefWorkItem] -> Set.Set String
nonRecursiveDefNames workItems =
  Set.fromList
    [ P.defDeclName (defWorkItemDecl workItem)
    | AcyclicSCC workItem <- stronglyConnComp (map graphNode workItems)
    ]
  where
    graphNode workItem =
      ( workItem,
        P.defDeclName (defWorkItemDecl workItem),
        defWorkItemDependencies workItem
      )

finalizeDefWorkItemsWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  FinalizeContext ->
  Maybe ModuleFinalizeContext ->
  Int ->
  Set.Set String ->
  [DefWorkItem] ->
  IO (TcM [CheckedBinding])
finalizeDefWorkItemsWithTiming timing moduleName0 finalizeContext moduleContext batchSize nonRecursiveNames workItems
  | Just moduleContext0 <- moduleContext =
      finalizeDefWorkItemLayersWithTiming timing moduleName0 finalizeContext moduleContext0 batchSize nonRecursiveNames workItems
  | otherwise =
      go [] workItems
  where
    go acc [] = pure (Right (reverse acc))
    go acc (workItem : rest) = do
      result <- finalizeDefWorkItemWithTiming timing moduleName0 finalizeContext moduleContext nonRecursiveNames workItem
      case result of
        Left err -> pure (Left err)
        Right binding -> go (binding : acc) rest

finalizeDefWorkItemLayersWithTiming ::
  TimingConfig ->
  P.ModuleName ->
  FinalizeContext ->
  ModuleFinalizeContext ->
  Int ->
  Set.Set String ->
  [DefWorkItem] ->
  IO (TcM [CheckedBinding])
finalizeDefWorkItemLayersWithTiming timing moduleName0 finalizeContext moduleContext batchSize nonRecursiveNames workItems = do
  let layers = nonRecursiveDefLayers batchSize nonRecursiveNames workItems
      layeredNames = Set.unions (map (Set.fromList . map defWorkItemName) layers)
      fallbackItems =
        [ workItem
        | workItem <- workItems
        , defWorkItemName workItem `Set.notMember` layeredNames
        ]
  layerResults <- finalizeLayers Map.empty (1 :: Int) layers
  case layerResults of
    Left err -> pure (Left err)
    Right checkedByName0 -> do
      fallbackResult <- finalizeFallbackItems checkedByName0 fallbackItems
      pure $ do
        checkedByName <- fallbackResult
        traverse
          ( \workItem ->
              case Map.lookup (defWorkItemRuntimeName workItem) checkedByName of
                Just checked -> Right checked
                Nothing -> Left (ProgramPipelineError ("missing checked definition `" ++ defWorkItemName workItem ++ "`"))
          )
          workItems
  where
    finalizeLayers checkedByName _ [] =
      pure (Right checkedByName)
    finalizeLayers checkedByName index (layer : rest) = do
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
          let checkedByName' =
                foldl'
                  ( \acc checked ->
                      Map.insert (checkedBindingName checked) checked acc
                  )
                  checkedByName
                  checkedLayer
          finalizeLayers checkedByName' (index + 1) rest

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
              nonRecursiveNames
              workItem
          case result of
            Left err -> pure (Left err)
            Right checked ->
              checked `seq` goLayer (checked : acc) (itemIndex + 1) rest

    finalizeFallbackItems checkedByName [] =
      pure (Right checkedByName)
    finalizeFallbackItems checkedByName (workItem : rest) = do
      result <-
        finalizeDefWorkItemWithTiming
          timing
          moduleName0
          finalizeContext
          (Just moduleContext)
          nonRecursiveNames
          workItem
      case result of
        Left err -> pure (Left err)
        Right checked ->
          finalizeFallbackItems (Map.insert (checkedBindingName checked) checked checkedByName) rest

defWorkItemName :: DefWorkItem -> String
defWorkItemName = P.defDeclName . defWorkItemDecl

defWorkItemRuntimeName :: DefWorkItem -> String
defWorkItemRuntimeName = loweredBindingName . defWorkItemLowered

moduleLayerEligibleDefWorkItem :: DefWorkItem -> Bool
moduleLayerEligibleDefWorkItem workItem =
  let lowered = defWorkItemLowered workItem
   in Map.null (loweredBindingDeferredObligations lowered)
        && not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))

nonRecursiveDefLayers :: Int -> Set.Set String -> [DefWorkItem] -> [[DefWorkItem]]
nonRecursiveDefLayers batchSize nonRecursiveNames workItems =
  concatMap (chunksOf batchSize) (dependencyLayers eligible)
  where
    -- Local references are still checked through the declared source types,
    -- as in the old per-def path.  Keep the SCC/layer classification in place,
    -- but only enable multi-root batches once the exact graph path is measured
    -- faster than the per-def module read-context path.
    eligible =
      [ workItem
      | workItem <- workItems
      , defWorkItemName workItem `Set.member` nonRecursiveNames
      , moduleLayerEligibleDefWorkItem workItem
      ]
    eligibleNames = Set.fromList (map defWorkItemName eligible)
    eligibleDependencies workItem =
      Set.fromList
        [ dep
        | dep <- defWorkItemDependencies workItem
        , dep `Set.member` eligibleNames
        ]

    dependencyLayers [] = []
    dependencyLayers remaining =
      let remainingNames = Set.fromList (map defWorkItemName remaining)
          isReady workItem =
            Set.null (eligibleDependencies workItem `Set.intersection` remainingNames)
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
  Set.Set String ->
  DefWorkItem ->
  IO (TcM CheckedBinding)
finalizeDefWorkItemWithTiming timing moduleName0 finalizeContext moduleContext nonRecursiveNames workItem =
  timeProgramOperationIO timing label $
    case moduleContext of
      Just moduleContext0
        | moduleContextEligibleDefWorkItem nonRecursiveNames workItem ->
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
    defName = P.defDeclName (defWorkItemDecl workItem)
    lowered = defWorkItemLowered workItem
    label = checkModuleOperationLabel moduleName0 ("def." ++ defName)

moduleDefContextMinDefs :: Int
moduleDefContextMinDefs = 150

moduleDefBatchSize :: IO Int
moduleDefBatchSize = do
  mbValue <- lookupEnv "MLF_MODULE_DEF_BATCH_SIZE"
  pure $
    case mbValue >>= readMaybe of
      Just n | n > 1 -> n
      _ -> 1

moduleContextEligibleDefWorkItem :: Set.Set String -> DefWorkItem -> Bool
moduleContextEligibleDefWorkItem nonRecursiveNames workItem =
  P.defDeclName (defWorkItemDecl workItem) `Set.member` nonRecursiveNames

moduleInterfaceName :: ModuleInterface -> P.ModuleName
moduleInterfaceName =
  packageModuleName . moduleInterfaceId

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

buildImportScopeResolved :: Map P.ModuleName ModuleExports -> [P.ResolvedImport] -> TcM Scope
buildImportScopeResolved priorExports imports0 = foldM go emptyScope imports0
  where
    go scope imp = do
      let moduleName0 = resolvedImportDefiningModule imp
      exports <-
        case Map.lookup moduleName0 priorExports of
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

addAllExports :: Scope -> ModuleExports -> TcM Scope
addAllExports scope exports = do
  let (scopeWithOwners, importedValues) = prepareBulkImportedValues exports scope (exportedValues exports)
  liftEither $ do
    values <- addValues (scopeValues scopeWithOwners) importedValues
    types <- addTypes (scopeTypes scopeWithOwners) (Map.map exportedTypeData (exportedTypes exports))
    classes <- addClasses (scopeClasses scopeWithOwners) (exportedClasses exports)
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
  any
    ((== ctorOwningTypeIdentity ctorInfo) . dataInfoSymbolIdentity . exportedTypeData)
    (Map.elems (exportedTypes exports))

qualifyModuleExports :: P.ModuleName -> ModuleExports -> ModuleExports
qualifyModuleExports alias exports =
  ModuleExports
    { exportedValues = qualifiedValues,
      exportedTypes = qualifiedTypes,
      exportedClasses = qualifiedClasses
    }
  where
    qualifiedName name = alias ++ "." ++ name
    exportedTypeNames = Map.keysSet (exportedTypes exports)
    exportedClassNames = Map.keysSet (exportedClasses exports)

    qualifiedTypes =
      Map.fromList
        [ let qualifiedDataInfo = qualifyDataInfo dataInfo
              visibleCtorNames = Set.fromList (Map.keys ctors)
           in ( qualifiedName typeName,
                ExportedTypeInfo
                  { exportedTypeData = qualifiedDataInfo,
                    exportedTypeConstructors =
                      Map.fromList
                        [ (ctorName ctor, ctor)
                          | ctor <- dataConstructors qualifiedDataInfo,
                            unqualifyQualifiedName alias (ctorName ctor) `Set.member` visibleCtorNames
                        ]
                  }
              )
          | (typeName, ExportedTypeInfo dataInfo ctors) <- Map.toList (exportedTypes exports)
        ]

    qualifiedClasses =
      Map.fromList
        [ (qualifiedName className0, qualifyClassInfo classInfo)
          | (className0, classInfo) <- Map.toList (exportedClasses exports)
        ]

    qualifiedCtorValues =
      Map.fromList
        [ ( ctorName ctor,
            ConstructorValue
              { valueDisplayName = ctorName ctor,
                valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
                valueRuntimeName = ctorRuntimeName ctor,
                valueType = ctorType ctor,
                valueCtorInfo = ctor,
                valueOriginModule = dataModule dataInfo
              }
          )
          | ExportedTypeInfo dataInfo ctors <- Map.elems qualifiedTypes,
            ctor <- Map.elems ctors
        ]

    qualifiedExportedValues =
      Map.fromList
        [ ( qualifiedName name,
            qualifyValueInfo name valueInfo
          )
          | (name, valueInfo) <- Map.toList (exportedValues exports)
        ]

    qualifiedValues = qualifiedCtorValues `Map.union` qualifiedExportedValues

    qualifyDataInfo dataInfo =
      let qualifyCtor ctor = qualifyConstructorInfo ctor
       in dataInfo
            { dataName = dataName dataInfo,
              dataConstructors = map qualifyCtor (dataConstructors dataInfo)
            }

    qualifyConstructorInfo ctor =
      ctor
        { ctorName = qualifiedName (ctorName ctor),
          ctorType = qualifySrcType (ctorType ctor),
          ctorArgs = map qualifySrcType (ctorArgs ctor),
          ctorResult = qualifySrcType (ctorResult ctor),
          ctorOwningType = ctorOwningType ctor,
          ctorOwningTypeIdentity = ctorOwningTypeIdentity ctor,
          ctorOwnerConstructors = map qualifyConstructorShape (ctorOwnerConstructors ctor)
        }

    qualifyConstructorShape shape =
      shape
        { constructorShapeName = qualifiedName (constructorShapeName shape),
          constructorShapeForalls =
            [ (name, fmap qualifySrcType mbBound)
              | (name, mbBound) <- constructorShapeForalls shape
            ],
          constructorShapeArgs = map qualifySrcType (constructorShapeArgs shape),
          constructorShapeResult = qualifySrcType (constructorShapeResult shape)
        }

    qualifyClassInfo classInfo =
      let qualifiedClassName = qualifiedName (className classInfo)
          qualifyConstraintInfo constraintInfo =
            constraintInfo
              { constraintDisplayClass = qualifiedClassNameFor (constraintDisplayClass constraintInfo),
                constraintTypeView = qualifyTypeView (constraintTypeView constraintInfo),
                constraintTypeViews = fmap qualifyTypeView (constraintTypeViews constraintInfo)
              }
          qualifyTypeView view =
            view
              { typeViewDisplay = qualifySrcType (typeViewDisplay view)
              }
          qualifyMethod methodInfo =
            methodInfo
              { methodClassName = qualifiedClassName,
                methodType = qualifySrcType (methodType methodInfo),
                methodConstraints = map qualifyConstraint (methodConstraints methodInfo),
                methodConstraintInfos = map qualifyConstraintInfo (methodConstraintInfos methodInfo)
              }
       in classInfo
            { className = qualifiedClassName,
              classSuperclasses = map qualifyConstraint (classSuperclasses classInfo),
              classSuperclassInfos = map qualifyConstraintInfo (classSuperclassInfos classInfo),
              classMethods = Map.map qualifyMethod (classMethods classInfo)
            }

    qualifyValueInfo sourceName valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          valueInfo
            { valueDisplayName = qualifiedName sourceName,
              valueType = qualifySrcType (valueType valueInfo),
              valueConstraints = map qualifyConstraint (valueConstraints valueInfo)
            }
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          OverloadedMethod
            { valueDisplayName = qualifiedName sourceName,
              valueInfoSymbol = valueInfoSymbolIdentity valueInfo,
              valueMethodInfo = qualifyMethodFromExport methodInfo,
              valueOriginModule = valueOriginModule valueInfo
            }
        ConstructorValue {valueCtorInfo = ctorInfo} ->
          let qualifiedCtorInfo = qualifyConstructorInfo ctorInfo
           in ConstructorValue
                { valueDisplayName = qualifiedName sourceName,
                  valueInfoSymbol = valueInfoSymbolIdentity valueInfo,
                  valueRuntimeName = valueRuntimeName valueInfo,
                  valueType = qualifySrcType (valueType valueInfo),
                  valueCtorInfo = qualifiedCtorInfo,
                  valueOriginModule = valueOriginModule valueInfo
                }

    qualifyMethodFromExport methodInfo =
      methodInfo
        { methodClassName = qualifiedClassNameFor (methodClassName methodInfo),
          methodType = qualifySrcType (methodType methodInfo),
          methodConstraints = map qualifyConstraint (methodConstraints methodInfo),
          methodConstraintInfos = map qualifyConstraintInfoFromExport (methodConstraintInfos methodInfo)
        }

    qualifyConstraintInfoFromExport constraintInfo =
      constraintInfo
        { constraintDisplayClass = qualifiedClassNameFor (constraintDisplayClass constraintInfo),
          constraintTypeView = qualifyTypeViewFromExport (constraintTypeView constraintInfo),
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

unqualifyQualifiedName :: P.ModuleName -> String -> String
unqualifyQualifiedName alias name =
  case splitAt (length alias + 1) name of
    (prefix, rest)
      | prefix == alias ++ "." -> rest
    _ -> name

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
    Just (SymbolOwnerClass moduleName0 className0) ->
      Just
        SymbolIdentity
          { symbolNamespace = SymbolClass,
            symbolDefiningModule = moduleName0,
            symbolDefiningName = className0,
            symbolOwnerIdentity = Nothing
          }
    _ -> Nothing

instanceClassIdentity :: InstanceInfo -> ClassIdentity
instanceClassIdentity = instanceInfoClassSymbolIdentity

visibleInstancesForImports ::
  Map P.ModuleName ModuleExports ->
  Map P.ModuleName (Map String DataInfo) ->
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
  Map P.ModuleName ModuleExports ->
  Map P.ModuleName (Map String DataInfo) ->
  [InstanceInfo] ->
  Set.Set ClassIdentity ->
  P.ResolvedImport ->
  [InstanceInfo]
unqualifiedInstancesForImport priorExports priorData priorInstances unqualifiedClassIdentities imp =
  case Map.lookup (resolvedImportDefiningModule imp) priorExports of
    Nothing -> []
    Just exports ->
      let importClassIdentities = importUnqualifiedClassIdentitiesFor exports imp
          unqualifiedTypeNames = importUnqualifiedTypeNames exports imp
       in [ instanceInfo
            | instanceInfo <- priorInstances,
              instanceBelongsToModule (resolvedImportDefiningModule imp) instanceInfo,
              instanceVisibleForUnqualifiedImport priorData unqualifiedClassIdentities importClassIdentities unqualifiedTypeNames instanceInfo
          ]

qualifiedInstancesForImport ::
  Map P.ModuleName ModuleExports ->
  Map P.ModuleName (Map String DataInfo) ->
  [InstanceInfo] ->
  Set.Set ClassIdentity ->
  P.ResolvedImport ->
  [InstanceInfo]
qualifiedInstancesForImport priorExports priorData priorInstances _unqualifiedClassIdentities imp =
  case P.importAlias imp of
    Nothing -> []
    Just _alias ->
      case Map.lookup (resolvedImportDefiningModule imp) priorExports of
        Nothing -> []
        Just exports ->
          [ instanceInfo
            | instanceInfo <- priorInstances,
              instanceBelongsToModule (resolvedImportDefiningModule imp) instanceInfo,
              instanceVisibleForQualifiedImport priorData exports instanceInfo
          ]

importedUnqualifiedClassIdentities ::
  Map P.ModuleName ModuleExports ->
  [P.ResolvedImport] ->
  Set.Set ClassIdentity
importedUnqualifiedClassIdentities priorExports =
  Set.unions . map importUnqualifiedClassIdentities
  where
    importUnqualifiedClassIdentities imp =
      case Map.lookup (resolvedImportDefiningModule imp) priorExports of
        Nothing -> Set.empty
        Just exports -> importUnqualifiedClassIdentitiesFor exports imp

importUnqualifiedClassIdentitiesFor :: ModuleExports -> P.ResolvedImport -> Set.Set ClassIdentity
importUnqualifiedClassIdentitiesFor exports imp =
  case (P.importAlias imp, P.importExposing imp) of
    (Nothing, Nothing) ->
      Set.fromList (map classIdentity (Map.elems (exportedClasses exports)))
        `Set.union` overloadedMethodClassIdentities (Map.elems (exportedValues exports))
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

importUnqualifiedTypeNames :: ModuleExports -> P.ResolvedImport -> Set.Set String
importUnqualifiedTypeNames exports imp =
  case (P.importAlias imp, P.importExposing imp) of
    (Nothing, Nothing) -> exportedTypeIdentityNames exports
    (_, Just items) -> importExposedTypeIdentityNames items
    (Just _, Nothing) -> Set.empty

importExposedTypeIdentityNames :: [P.ResolvedExportItem] -> Set.Set String
importExposedTypeIdentityNames items =
  Set.fromList (concatMap exposedTypeIdentityName items)
  where
    exposedTypeIdentityName item =
      case item of
        P.ExportType ref -> resolvedExportTypeIdentityNames ref
        P.ExportTypeWithConstructors ref -> resolvedExportTypeIdentityNames ref
        P.ExportValue {} -> []

    resolvedExportTypeIdentityNames ref =
      [ symbolIdentityTypeName (resolvedSymbolIdentity symbol)
        | symbol <- P.resolvedExportTypeSymbols ref,
          symbolNamespace (resolvedSymbolIdentity symbol) == SymbolType
      ]

exportedTypeIdentityNames :: ModuleExports -> Set.Set String
exportedTypeIdentityNames exports =
  Set.fromList
    [ symbolIdentityTypeName (dataInfoSymbolIdentity dataInfo)
      | ExportedTypeInfo dataInfo _ <- Map.elems (exportedTypes exports)
    ]

symbolIdentityTypeName :: SymbolIdentity -> String
symbolIdentityTypeName identity =
  symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

instanceVisibleForUnqualifiedImport ::
  Map P.ModuleName (Map String DataInfo) ->
  Set.Set ClassIdentity ->
  Set.Set ClassIdentity ->
  Set.Set String ->
  InstanceInfo ->
  Bool
instanceVisibleForUnqualifiedImport priorData unqualifiedClassIdentities importClassIdentities unqualifiedTypeNames instanceInfo =
  (classVisibleGlobally && originDataVisible && not (Set.null originDataMentions))
    || (classVisibleThroughImport && Set.null originDataMentions)
  where
    identity = instanceClassIdentity instanceInfo
    classVisibleGlobally = identity `Set.member` unqualifiedClassIdentities
    classVisibleThroughImport = identity `Set.member` importClassIdentities
    originDataMentions = instanceOriginDataMentions priorData instanceInfo
    originDataVisible = originDataMentions `Set.isSubsetOf` unqualifiedTypeNames

instanceVisibleForQualifiedImport :: Map P.ModuleName (Map String DataInfo) -> ModuleExports -> InstanceInfo -> Bool
instanceVisibleForQualifiedImport priorData exports instanceInfo =
  originDataVisible
    && ( instanceClassIdentity instanceInfo `Set.member` exportedClassIdentities
           || any (srcTypeMentionsAny exportedTypeNames) (instanceHeadIdentityTypes instanceInfo)
       )
  where
    exportedTypeNames = exportedTypeIdentityNames exports
    exportedClassIdentities = Set.fromList (map classIdentity (Map.elems (exportedClasses exports)))
    originDataVisible = instanceOriginDataMentions priorData instanceInfo `Set.isSubsetOf` exportedTypeNames

srcTypeMentionsAny :: Set.Set String -> SrcType -> Bool
srcTypeMentionsAny names ty =
  case ty of
    STVar {} -> False
    STBase name -> name `Set.member` names
    STCon name args -> name `Set.member` names || any (srcTypeMentionsAny names) args
    STVarApp _ args -> any (srcTypeMentionsAny names) args
    STTyLam _ body -> srcTypeMentionsAny names body
    STTyApp fun arg -> srcTypeMentionsAny names fun || srcTypeMentionsAny names arg
    STArrow dom cod -> srcTypeMentionsAny names dom || srcTypeMentionsAny names cod
    STForall _ mb body ->
      maybe False (srcTypeMentionsAny names . unSrcBound) mb
        || srcTypeMentionsAny names body
    STMu _ body -> srcTypeMentionsAny names body
    STBottom -> False

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

instanceExportedTypeMentions :: Set.Set String -> InstanceInfo -> Set.Set String
instanceExportedTypeMentions exportedTypeNames instanceInfo =
  Set.unions (headMentions : constraintMentions ++ methodMentions)
  where
    headMentions = foldMap (srcTypeMentionedNames exportedTypeNames) (instanceHeadIdentityTypes instanceInfo)
    constraintMentions =
      concatMap
        (map (srcTypeMentionedNames exportedTypeNames . typeViewIdentity) . NE.toList . constraintTypeViews)
        (instanceConstraintInfos instanceInfo)
    methodMentions = concatMap valueExportedTypeMentions (Map.elems (instanceMethods instanceInfo))

    valueExportedTypeMentions valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          srcTypeMentionedNames exportedTypeNames (valueIdentityType valueInfo)
            : concatMap
              (map (srcTypeMentionedNames exportedTypeNames . typeViewIdentity) . NE.toList . constraintTypeViews)
              (valueConstraintInfos valueInfo)
        _ -> []

instanceOriginDataMentions :: Map P.ModuleName (Map String DataInfo) -> InstanceInfo -> Set.Set String
instanceOriginDataMentions priorData instanceInfo =
  case Map.lookup (instanceOriginModule instanceInfo) priorData of
    Nothing -> Set.empty
    Just dataInfos ->
      instanceExportedTypeMentions
        (Set.fromList [symbolIdentityTypeName (dataInfoSymbolIdentity info) | info <- Map.elems dataInfos])
        instanceInfo

srcTypeMentionedNames :: Set.Set String -> SrcType -> Set.Set String
srcTypeMentionedNames names ty =
  case ty of
    STVar {} -> Set.empty
    STBase name
      | name `Set.member` names -> Set.singleton name
      | otherwise -> Set.empty
    STCon name args ->
      let headNames
            | name `Set.member` names = Set.singleton name
            | otherwise = Set.empty
       in Set.unions (headNames : map (srcTypeMentionedNames names) (NE.toList args))
    STVarApp _ args -> Set.unions (map (srcTypeMentionedNames names) (NE.toList args))
    STTyLam _ body -> srcTypeMentionedNames names body
    STTyApp fun arg ->
      srcTypeMentionedNames names fun `Set.union` srcTypeMentionedNames names arg
    STArrow dom cod ->
      srcTypeMentionedNames names dom `Set.union` srcTypeMentionedNames names cod
    STForall _ mb body ->
      maybe Set.empty (srcTypeMentionedNames names . unSrcBound) mb
        `Set.union` srcTypeMentionedNames names body
    STMu _ body -> srcTypeMentionedNames names body
    STBottom -> Set.empty

instanceBelongsToModule :: P.ModuleName -> InstanceInfo -> Bool
instanceBelongsToModule moduleName0 instanceInfo =
  instanceOriginModule instanceInfo == moduleName0

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
          when (Map.null (exportedTypeConstructors typeInfo)) $
            throwError (ProgramImportNotExported moduleName0 typeName)
          let dataInfo = exportedTypeData typeInfo
              ctorValues =
                Map.fromList
                  [ ( ctorName ctor,
                      ConstructorValue
                        { valueDisplayName = ctorName ctor,
                          valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
                          valueRuntimeName = ctorRuntimeName ctor,
                          valueType = ctorType ctor,
                          valueCtorInfo = ctor,
                          valueOriginModule = dataModule dataInfo
                        }
                    )
                    | ctor <- Map.elems (exportedTypeConstructors typeInfo)
                  ]
          values <- liftEither (addValues (scopeValues scope) ctorValues)
          types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
          pure (mkScopeWithHidden values types (scopeHiddenTypes scope) (scopeClasses scope) (scopeInstances scope))
        Nothing -> throwError (ProgramImportNotExported moduleName0 (P.resolvedExportTypeName ref))

prepareImportedValue :: ModuleExports -> Scope -> ValueInfo -> (Scope, ValueInfo)
prepareImportedValue exports scope valueInfo =
  case valueInfo of
    ConstructorValue {valueCtorInfo = ctorInfo} ->
      case exportedConstructorOwnerType ctorInfo exports of
        Just dataInfo ->
          let hiddenDataInfo = hiddenOwnerDataInfo dataInfo
              hiddenTypes = Map.insert (dataName hiddenDataInfo) hiddenDataInfo (scopeHiddenTypes scope)
              hiddenCtorInfo = importedHiddenConstructorInfo ctorInfo hiddenDataInfo
              importedInfo =
                valueInfo
                  { valueType = ctorType hiddenCtorInfo,
                    valueCtorInfo = hiddenCtorInfo
                  }
           in (withScopeHiddenTypes hiddenTypes scope, importedInfo)
        Nothing -> (scope, valueInfo)
    _ -> (scope, valueInfo)

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
      ownerNames =
        Set.fromList
          [ dataName dataInfo,
            symbolDefiningName (dataInfoSymbolIdentity dataInfo)
          ]
      rewrite = rewriteOwnerTypeHeads ownerNames hiddenName
      rewriteShape shape =
        shape
          { constructorShapeForalls =
              [ (name, fmap rewrite mbBound)
                | (name, mbBound) <- constructorShapeForalls shape
              ],
            constructorShapeArgs = map rewrite (constructorShapeArgs shape),
            constructorShapeResult = rewrite (constructorShapeResult shape)
          }
      rewriteCtor ctor =
        ctor
          { ctorType = rewrite (ctorType ctor),
            ctorForalls =
              [ (name, fmap rewrite mbBound)
                | (name, mbBound) <- ctorForalls ctor
              ],
            ctorArgs = map rewrite (ctorArgs ctor),
            ctorResult = rewrite (ctorResult ctor),
            ctorOwningType = hiddenName,
            ctorOwnerConstructors = map rewriteShape (ctorOwnerConstructors ctor)
          }
   in dataInfo
        { dataName = hiddenName,
          dataConstructors = map rewriteCtor (dataConstructors dataInfo)
        }

hiddenOwnerTypeName :: DataInfo -> String
hiddenOwnerTypeName dataInfo =
  let identity = dataInfoSymbolIdentity dataInfo
   in "$" ++ symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

rewriteOwnerTypeHeads :: Set.Set String -> String -> SrcType -> SrcType
rewriteOwnerTypeHeads ownerNames hiddenName = go
  where
    rewriteHead name
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
        { ctorName = ctorName ctorInfo,
          ctorRuntimeName = ctorRuntimeName ctorInfo
        }
    Nothing -> ctorInfo

exportedConstructorOwnerType :: ConstructorInfo -> ModuleExports -> Maybe DataInfo
exportedConstructorOwnerType ctorInfo exports =
  case
    [ dataInfo
      | ExportedTypeInfo dataInfo _ <- Map.elems (exportedTypes exports),
        dataInfoSymbolIdentity dataInfo == ctorOwningTypeIdentity ctorInfo
    ] of
    dataInfo : _ -> Just dataInfo
    [] -> Just (constructorOwnerDataInfoFromShapes ctorInfo)

constructorOwnerDataInfoFromShapes :: ConstructorInfo -> DataInfo
constructorOwnerDataInfoFromShapes ctorInfo =
  DataInfo
    { dataName = ctorOwningType ctorInfo,
      dataInfoSymbol = ownerIdentity,
      dataModule = symbolDefiningModule ownerIdentity,
      dataTypeParams = typeParams,
      dataParams = paramNames,
      dataConstructors = constructors
    }
  where
    ownerIdentity = ctorOwningTypeIdentity ctorInfo
    ownerShapes = constructorOwnerShapes ctorInfo
    paramNames = P.typeParamNames typeParams
    typeParams =
      case [params | shape <- ownerShapes, let params = constructorShapeOwnerTypeParams shape, not (null params)] of
        params : _ -> params
        [] -> inferredConstructorOwnerTypeParams ctorInfo ownerShapes
    constructors = map constructorInfoFromShape ownerShapes

    constructorInfoFromShape shape =
      ConstructorInfo
        { ctorName = constructorShapeName shape,
          ctorInfoSymbol = constructorShapeSymbol shape,
          ctorRuntimeName = constructorShapeRuntimeName shape,
          ctorType = constructorShapeType shape,
          ctorForalls = constructorShapeForalls shape,
          ctorArgs = constructorShapeArgs shape,
          ctorResult = constructorShapeResult shape,
          ctorOwningType = ctorOwningType ctorInfo,
          ctorOwningTypeIdentity = ownerIdentity,
          ctorIndex = constructorShapeIndex shape,
          ctorOwnerConstructors = ownerShapes
        }

constructorShapeType :: ConstructorShape -> SrcType
constructorShapeType shape =
  foldr
    (\(name, mbBound) body -> STForall name (fmap SrcBound mbBound) body)
    (foldr STArrow (constructorShapeResult shape) (constructorShapeArgs shape))
    (constructorShapeForalls shape)

inferredConstructorOwnerTypeParams :: ConstructorInfo -> [ConstructorShape] -> [P.TypeParam]
inferredConstructorOwnerTypeParams ctorInfo ownerShapes =
  [ P.TypeParam name (kindFromMaxApplicationArity (Map.findWithDefault 0 name paramArities))
    | name <- inferredConstructorOwnerParamNames ctorInfo ownerShapes
  ]
  where
    paramArities = foldMap constructorShapeVariableHeadArities ownerShapes

inferredConstructorOwnerParamNames :: ConstructorInfo -> [ConstructorShape] -> [String]
inferredConstructorOwnerParamNames ctorInfo ownerShapes =
  case transpose (mapMaybe (fmap NE.toList . constructorOwnerResultArgs ctorInfo . constructorShapeResult) ownerShapes) of
    [] -> maybe [] (mapMaybe srcTypeVarName . NE.toList) (constructorOwnerResultArgs ctorInfo (ctorResult ctorInfo))
    columns -> mapMaybe firstSrcTypeVarName columns

constructorOwnerResultArgs :: ConstructorInfo -> SrcType -> Maybe (NonEmpty SrcType)
constructorOwnerResultArgs ctorInfo ty =
  case ty of
    STBase name
      | name == ctorOwningType ctorInfo || name == symbolDefiningName (ctorOwningTypeIdentity ctorInfo) ->
          Nothing
    STCon name args
      | name == ctorOwningType ctorInfo || name == symbolDefiningName (ctorOwningTypeIdentity ctorInfo) ->
          Just args
    _ -> Nothing

firstSrcTypeVarName :: [SrcType] -> Maybe String
firstSrcTypeVarName tys =
  case mapMaybe srcTypeVarName tys of
    name : _ -> Just name
    [] -> Nothing

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

exportedValueByIdentity :: SymbolIdentity -> ModuleExports -> Maybe (String, ValueInfo)
exportedValueByIdentity identity exports =
  find ((== identity) . valueInfoSymbolIdentity . snd) (Map.toList (exportedValues exports))

exportedTypeByRef :: P.ResolvedExportTypeRef -> ModuleExports -> Maybe (String, ExportedTypeInfo)
exportedTypeByRef ref exports =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolType] of
    symbol : _ -> find ((== resolvedSymbolIdentity symbol) . dataInfoSymbolIdentity . exportedTypeData . snd) (Map.toList (exportedTypes exports))
    [] -> Nothing

exportedClassByRef :: P.ResolvedExportTypeRef -> ModuleExports -> Maybe (String, ClassInfo)
exportedClassByRef ref exports =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolClass] of
    symbol : _ -> find ((== resolvedSymbolIdentity symbol) . classInfoSymbolIdentity . snd) (Map.toList (exportedClasses exports))
    [] -> Nothing

exportedClassByIdentity :: ClassIdentity -> ModuleExports -> Maybe (String, ClassInfo)
exportedClassByIdentity identity exports =
  find ((== identity) . classInfoSymbolIdentity . snd) (Map.toList (exportedClasses exports))

displaySrcTypeForResolved :: DisplayNameEnv -> ResolvedSrcType -> TcM SrcType
displaySrcTypeForResolved env = \case
  RSTVar name -> pure (STVar name)
  RSTArrow dom cod -> STArrow <$> displaySrcTypeForResolved env dom <*> displaySrcTypeForResolved env cod
  RSTBase symbol -> STBase <$> displayTypeHeadName env symbol
  RSTCon symbol args -> STCon <$> displayTypeHeadName env symbol <*> traverse (displaySrcTypeForResolved env) args
  RSTVarApp name args -> STVarApp name <$> traverse (displaySrcTypeForResolved env) args
  RSTTyLam name body -> STTyLam name <$> displaySrcTypeForResolved env body
  RSTTyApp fun arg -> STTyApp <$> displaySrcTypeForResolved env fun <*> displaySrcTypeForResolved env arg
  RSTForall name mb body ->
    STForall name
      <$> traverse (fmap SrcBound . displaySrcTypeForResolved env . unResolvedSrcBound) mb
      <*> displaySrcTypeForResolved env body
  RSTMu name body -> STMu name <$> displaySrcTypeForResolved env body
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
typeViewForDisplayEnv env ty =
  TypeView
    <$> displaySrcTypeForResolved env ty
    <*> pure (resolvedSrcTypeIdentityType ty)

constraintInfoForDisplayEnv :: DisplayNameEnv -> P.ResolvedClassConstraint -> TcM ConstraintInfo
constraintInfoForDisplayEnv env constraint = do
  views <- mapM (typeViewForDisplayEnv env) (P.constraintTypes constraint)
  ConstraintInfo
    <$> displayClassName env (P.constraintClassName constraint)
    <*> pure (resolvedSymbolIdentity (P.constraintClassName constraint))
    <*> pure (NE.head views)
    <*> pure views

displayConstrainedTypeForResolved :: DisplayNameEnv -> P.ResolvedConstrainedType -> TcM P.ConstrainedType
displayConstrainedTypeForResolved env ty =
  P.ConstrainedType
    <$> mapM (displayClassConstraintForResolved env) (P.constrainedConstraints ty)
    <*> displaySrcTypeForResolved env (P.constrainedBody ty)

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
    (\acc param -> bindKindVariable (P.typeParamName param) (kindFromSrc (P.typeParamKind param)) acc)
    env
    params

checkResolvedKind :: KindEnv -> ResolvedSrcType -> P.SrcKind -> TcM KindEnv
checkResolvedKind env ty expected =
  checkResolvedKindTerm env ty (kindFromSrc expected)

checkResolvedKindTerm :: KindEnv -> ResolvedSrcType -> KindTerm -> TcM KindEnv
checkResolvedKindTerm env ty expected =
  case ty of
    RSTVar name -> bindKindVariable name expected env
    RSTArrow dom cod -> do
      env1 <- requireKindTerm env ty expected KTType
      env2 <- checkResolvedKindTerm env1 dom KTType
      checkResolvedKindTerm env2 cod KTType
    RSTForall name mb body -> do
      env1 <- requireKindTerm env ty expected KTType
      env2 <-
        case mb of
          Nothing -> pure env1
          Just bound -> checkResolvedKindTerm env1 (unResolvedSrcBound bound) KTType
      withScopedKindVariable name KTType env2 $ \env3 ->
        checkResolvedKindTerm env3 body KTType
    RSTMu name body -> do
      env1 <- requireKindTerm env ty expected KTType
      withScopedKindVariable name KTType env1 $ \env2 ->
        checkResolvedKindTerm env2 body KTType
    RSTVarApp name args -> checkVarAppKind env name args expected
    RSTTyApp fun arg -> checkTyAppKind env ty fun arg expected
    _ -> do
      (actual, env1) <- inferResolvedKindTerm env ty
      requireKindTerm env1 ty expected actual

inferResolvedKindTerm :: KindEnv -> ResolvedSrcType -> TcM (KindTerm, KindEnv)
inferResolvedKindTerm env ty =
  case ty of
    RSTVar name -> kindTermForVariable name env
    RSTBase symbol -> do
      kind0 <- resolvedTypeHeadKind env symbol
      pure (kindFromSrc kind0, env)
    RSTCon symbol args -> do
      headKind <- kindFromSrc <$> resolvedTypeHeadKind env symbol
      applyKindArgs env (resolvedSymbolDisplayName symbol) headKind args
    RSTVarApp name args -> inferVarAppKind env name args
    RSTTyLam {} ->
      throwError (ProgramKindMismatch (resolvedSrcTypeToSrcType ty) P.KType (P.KArrow P.KType P.KType))
    RSTTyApp fun arg -> inferTyAppKind env ty fun arg
    RSTArrow dom cod -> do
      env1 <- checkResolvedKindTerm env dom KTType
      env2 <- checkResolvedKindTerm env1 cod KTType
      pure (KTType, env2)
    RSTForall name mb body -> do
      env1 <-
        case mb of
          Nothing -> pure env
          Just bound -> checkResolvedKindTerm env (unResolvedSrcBound bound) KTType
      env2 <-
        withScopedKindVariable name KTType env1 $ \env3 ->
          checkResolvedKindTerm env3 body KTType
      pure (KTType, env2)
    RSTMu name body -> do
      env1 <-
        withScopedKindVariable name KTType env $ \env2 ->
          checkResolvedKindTerm env2 body KTType
      pure (KTType, env1)
    RSTBottom -> pure (KTType, env)

checkVarAppKind :: KindEnv -> String -> NonEmpty ResolvedSrcType -> KindTerm -> TcM KindEnv
checkVarAppKind env name args expected = do
  (actual, env1) <- inferVarAppKind env name args
  requireKindTerm env1 (RSTVarApp name args) expected actual

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

inferVarAppKind :: KindEnv -> String -> NonEmpty ResolvedSrcType -> TcM (KindTerm, KindEnv)
inferVarAppKind env name args = do
  (headKind, env1) <- kindTermForVariable name env
  applyKindArgs env1 name headKind args

applyKindArgs :: KindEnv -> String -> KindTerm -> NonEmpty ResolvedSrcType -> TcM (KindTerm, KindEnv)
applyKindArgs env headName headKind args =
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
          env3 <- requireKindTerm env2 (RSTVar headName) (KTMeta meta) (KTArrow argKind resultKind)
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

kindTermForVariable :: String -> KindEnv -> TcM (KindTerm, KindEnv)
kindTermForVariable name env =
  case Map.lookup name (kindTypeVariables env) of
    Just kind0 -> pure (zonkKindTerm env kind0, env)
    Nothing ->
      let (kind0, env1) = freshKindMeta env
       in pure (kind0, env1 {kindTypeVariables = Map.insert name kind0 (kindTypeVariables env1)})

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
    RSTVar name ->
      withActualArity 0 <$> kindForVar name
    RSTBase symbol ->
      withActualArity 0 <$> kindForHead symbol
    RSTVarApp name args ->
      withActualArity (NE.length args) <$> kindForVar name
    RSTCon symbol args ->
      withActualArity (NE.length args) <$> kindForHead symbol
    _ -> Nothing
  where
    withActualArity actualArgs (headName, expectedArgs) =
      (headName, expectedArgs, actualArgs)

    kindForVar name =
      case Map.lookup name (kindTypeVariables env) of
        Just kind0 -> Just (name, kindTermArity env kind0)
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

bindKindVariable :: String -> KindTerm -> KindEnv -> TcM KindEnv
bindKindVariable name expected env =
  case Map.lookup name (kindTypeVariables env) of
    Just actual -> requireKindTerm env (RSTVar name) expected actual
    Nothing ->
      pure env {kindTypeVariables = Map.insert name (zonkKindTerm env expected) (kindTypeVariables env)}

withScopedKindVariable :: String -> KindTerm -> KindEnv -> (KindEnv -> TcM KindEnv) -> TcM KindEnv
withScopedKindVariable name kind0 env action = do
  let previous = Map.lookup name (kindTypeVariables env)
      envWithBinder = env {kindTypeVariables = Map.insert name kind0 (kindTypeVariables env)}
  envAfter <- action envWithBinder
  pure
    envAfter
      { kindTypeVariables =
          case previous of
            Just previousKind -> Map.insert name previousKind (kindTypeVariables envAfter)
            Nothing -> Map.delete name (kindTypeVariables envAfter)
      }

resolvedTypeHeadKind :: KindEnv -> ResolvedSymbol -> TcM P.SrcKind
resolvedTypeHeadKind env symbol =
  case resolvedTypeHeadKindMaybe env symbol of
    Just kind0 -> pure kind0
    Nothing -> throwError (ProgramUnknownType (resolvedSymbolDisplayName symbol))

resolvedTypeHeadKindMaybe :: KindEnv -> ResolvedSymbol -> Maybe P.SrcKind
resolvedTypeHeadKindMaybe env symbol
  | isBuiltinTypeSymbol symbol = Builtins.builtinTypeKind (symbolDefiningName (resolvedSymbolIdentity symbol))
  | otherwise = Map.lookup (resolvedSymbolIdentity symbol) (kindTypeConstructors env)

buildLocalDataInfo :: DisplayNameEnv -> ResolvedSemanticModule -> P.ResolvedModuleSyntax -> TcM (Map String DataInfo)
buildLocalDataInfo displayEnv resolvedModule mod0 = do
  let dataDecls = moduleDataDecls mod0
  ensureDistinctBy ProgramDuplicateType P.dataDeclName dataDecls
  ctorNames <- pure (concatMap (map P.constructorDeclName . P.dataDeclConstructors) dataDecls)
  ensureDistinctPlain ProgramDuplicateConstructor ctorNames
  pure . Map.fromList =<< mapM toDataInfo dataDecls
  where
    toDataInfo dataDecl = do
      let params = P.dataDeclParams dataDecl
          paramNames = P.typeParamNames params
      ensureDistinctPlain ProgramDuplicateTypeParameter paramNames
      dataIdentity <- lookupResolvedLocalTypeIdentity resolvedModule (P.dataDeclName dataDecl)
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
        ( P.dataDeclName dataDecl,
          DataInfo
            { dataName = P.dataDeclName dataDecl,
              dataInfoSymbol = dataIdentity,
              dataModule = P.moduleName mod0,
              dataTypeParams = params,
              dataParams = paramNames,
              dataConstructors = constructors
            }
        )

    toCtorInfo dataDecl dataIdentity index ctorDecl = do
      ctorIdentity <-
        lookupResolvedLocalValueIdentity
          resolvedModule
          SymbolConstructor
          (Just (SymbolOwnerType (P.moduleName mod0) (P.dataDeclName dataDecl)))
          (P.constructorDeclName ctorDecl)
      validateConstructorResult dataIdentity dataDecl ctorDecl (constructorResolvedResult (P.constructorDeclType ctorDecl))
      ctorType0 <- displaySrcTypeForResolved displayEnv (P.constructorDeclType ctorDecl)
      let (foralls, ctorBody) = splitForalls ctorType0
          (args0, result0) = splitArrows ctorBody
      pure
        ConstructorInfo
          { ctorName = P.constructorDeclName ctorDecl,
            ctorInfoSymbol = ctorIdentity,
            ctorRuntimeName = qualify (P.moduleName mod0) (P.constructorDeclName ctorDecl),
            ctorType = ctorType0,
            ctorForalls = foralls,
            ctorArgs = args0,
            ctorResult = result0,
            ctorOwningType = P.dataDeclName dataDecl,
            ctorOwningTypeIdentity = dataIdentity,
            ctorIndex = index,
            ctorOwnerConstructors = []
          }

    validateConstructorResult :: SymbolIdentity -> P.ResolvedDataDecl -> P.ResolvedConstructorDecl -> ResolvedSrcType -> TcM ()
    validateConstructorResult dataIdentity dataDecl ctorDecl resultTy =
      let owner = P.dataDeclName dataDecl
          params = P.typeParamNames (P.dataDeclParams dataDecl)
          invalid = throwError (ProgramInvalidConstructorResult (P.constructorDeclName ctorDecl) (resolvedSrcTypeToSrcType resultTy) owner)
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

buildLocalClassInfo :: DisplayNameEnv -> ResolvedSemanticModule -> P.ResolvedModuleSyntax -> TcM (Map String ClassInfo)
buildLocalClassInfo displayEnv resolvedModule mod0 = do
  let classDecls = moduleClassDecls mod0
  ensureDistinctBy ProgramDuplicateClass P.classDeclName classDecls
  pure . Map.fromList =<< mapM toClassInfo classDecls
  where
    toClassInfo classDecl = do
      ensureDistinctBy ProgramDuplicateMethod P.methodSigName (P.classDeclMethods classDecl)
      resolvedClassIdentity <- lookupResolvedLocalClassIdentity resolvedModule (P.classDeclName classDecl)
      let classParam = P.classDeclParam classDecl
          classParamName0 = P.typeParamName classParam
          classParams = P.classDeclParams classDecl
          classParamNames0 = fmap P.typeParamName classParams
      ensureDistinctPlain ProgramDuplicateTypeParameter (NE.toList classParamNames0)
      validateFunctionalDependencies (P.classDeclName classDecl) classParamNames0 (P.classDeclFundeps classDecl)
      superclasses0 <- mapM (displayClassConstraintForResolved displayEnv) (P.classDeclSuperclasses classDecl)
      superclassInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.classDeclSuperclasses classDecl)
      methods <-
        Map.fromList
          <$> forM
            (P.classDeclMethods classDecl)
            ( \sig -> do
                methodIdentity <-
                  lookupResolvedLocalValueIdentity
                    resolvedModule
                    SymbolMethod
                    (Just (SymbolOwnerClass (P.moduleName mod0) (P.classDeclName classDecl)))
                    (P.methodSigName sig)
                methodSigType0 <- displayConstrainedTypeForResolved displayEnv (P.methodSigType sig)
                methodConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.constrainedConstraints (P.methodSigType sig))
                pure
                  ( P.methodSigName sig,
                    MethodInfo
                      { methodClassName = P.classDeclName classDecl,
                        methodInfoSymbol = methodIdentity,
                        methodClassModule = P.moduleName mod0,
                        methodName = P.methodSigName sig,
                        methodRuntimeBase = qualify (P.moduleName mod0) (P.classDeclName classDecl ++ "__" ++ P.methodSigName sig),
                        methodType = P.constrainedBody methodSigType0,
                        methodTypeIdentity = resolvedSrcTypeIdentityType (P.constrainedBody (P.methodSigType sig)),
                        methodConstraints = P.constrainedConstraints methodSigType0,
                        methodConstraintInfos = methodConstraintInfos0,
                        methodTypeParam = classParam,
                        methodParamName = classParamName0,
                        methodTypeParams = classParams,
                        methodParamNames = classParamNames0
                      }
                  )
            )
      pure
        ( P.classDeclName classDecl,
          ClassInfo
            { className = P.classDeclName classDecl,
              classInfoSymbol = resolvedClassIdentity,
              classModule = P.moduleName mod0,
              classTypeParam = classParam,
              classParamName = classParamName0,
              classTypeParams = classParams,
              classParamNames = classParamNames0,
              classSuperclasses = superclasses0,
              classSuperclassInfos = superclassInfos0,
              classFunctionalDependencies = P.classDeclFundeps classDecl,
              classMethods = methods
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

buildLocalDefInfo :: DisplayNameEnv -> ResolvedSemanticModule -> P.ResolvedModuleSyntax -> TcM (Map String ValueInfo)
buildLocalDefInfo displayEnv resolvedModule mod0 = do
  let defs = moduleDefDecls mod0
  ensureDistinctBy ProgramDuplicateValue P.defDeclName defs
  Map.fromList <$> mapM toValueInfo defs
  where
    toValueInfo defDecl = do
      valueIdentity <- lookupResolvedLocalValueIdentity resolvedModule SymbolValue Nothing (P.defDeclName defDecl)
      defType0 <- displayConstrainedTypeForResolved displayEnv (P.defDeclType defDecl)
      defConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.constrainedConstraints (P.defDeclType defDecl))
      let defIdentityType0 =
            constrainedVisibleType $
              P.ConstrainedType
                (map displayConstraint defConstraintInfos0)
                (resolvedSrcTypeIdentityType (P.constrainedBody (P.defDeclType defDecl)))
      pure
        ( P.defDeclName defDecl,
          OrdinaryValue
            { valueDisplayName = P.defDeclName defDecl,
              valueInfoSymbol = valueIdentity,
              valueRuntimeName = qualify (P.moduleName mod0) (P.defDeclName defDecl),
              valueType = constrainedVisibleType defType0,
              valueIdentityType = defIdentityType0,
              valueConstraints = P.constrainedConstraints defType0,
              valueConstraintInfos = defConstraintInfos0,
              valueOriginModule = P.moduleName mod0
            }
        )

addConstructorValues :: P.ModuleName -> Map String DataInfo -> TcM (Map String ValueInfo)
addConstructorValues moduleName0 dataInfos =
  pure $
    Map.fromList
      [ ( ctorName ctor,
          ConstructorValue
            { valueDisplayName = ctorName ctor,
              valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
              valueRuntimeName = ctorRuntimeName ctor,
              valueType = ctorType ctor,
              valueCtorInfo = ctor,
              valueOriginModule = moduleName0
            }
        )
        | dataInfo <- Map.elems dataInfos,
          ctor <- dataConstructors dataInfo
      ]

synthesizeDerivedInstances :: DisplayNameEnv -> Scope -> ResolvedSemanticModule -> P.ResolvedModuleSyntax -> TcM [P.ResolvedInstanceDecl]
synthesizeDerivedInstances displayEnv scope resolvedModule mod0 = do
  candidates <- concat <$> mapM deriveForData (moduleDataDecls mod0)
  let pendingInstances = map (\(_, _, classInfo, instDecl) -> pendingDerivedInstance classInfo instDecl) candidates
      validationScope = withScopeInstances (scopeInstances scope ++ pendingInstances) scope
  mapM_
    (\(_, displayDataDecl, classInfo, _) -> validateEqDerivingFields classInfo validationScope displayDataDecl)
    candidates
  pure [instDecl | (_, _, _, instDecl) <- candidates]
  where
    deriveForData dataDecl = do
      displayDataDecl <- resolvedDataDeclForEnv dataDecl
      forM (P.dataDeclDeriving dataDecl) $ \classSymbol -> do
        classInfo <- lookupClassInfoBySymbol scope classSymbol
        if symbolDefiningName (classInfoSymbolIdentity classInfo) == "Eq"
          then
            case eqMethodReference classInfo of
              Just eqMethodSymbol -> do
                instDecl <- mkEqInstance classSymbol classInfo eqMethodSymbol dataDecl displayDataDecl
                pure (dataDecl, displayDataDecl, classInfo, instDecl)
              Nothing -> throwError (ProgramUnsupportedDeriving (resolvedSymbolDisplayName classSymbol))
          else throwError (ProgramUnsupportedDeriving (resolvedSymbolDisplayName classSymbol))

    eqMethodReference classInfo =
      toResolvedValueSymbol . snd <$> find matchesEqMethod (Map.toList (scopeValues scope))
      where
        matchesEqMethod (_, OverloadedMethod {valueMethodInfo = methodInfo}) =
          methodInfoOwnerClassSymbolIdentity methodInfo == classInfoSymbolIdentity classInfo
            && methodName methodInfo == "eq"
        matchesEqMethod _ = False

    toResolvedValueSymbol valueInfo =
      mkResolvedSymbol
        (valueInfoSymbolIdentity valueInfo)
        (valueDisplayName valueInfo)
        (valueDisplayName valueInfo)
        (SymbolLocal (valueOriginModule valueInfo))

    pendingDerivedInstance classInfo instDecl =
      let headTy = resolvedSrcTypeToSrcType (P.instanceDeclType instDecl)
          headIdentityTy = resolvedSrcTypeIdentityType (P.instanceDeclType instDecl)
          constraintInfos =
            [ ConstraintInfo
                { constraintDisplayClass = className classInfo,
                  constraintClassSymbol = classInfoSymbolIdentity classInfo,
                  constraintTypeView =
                    TypeView
                      { typeViewDisplay = resolvedSrcTypeToSrcType (P.constraintType constraint),
                        typeViewIdentity = resolvedSrcTypeIdentityType (P.constraintType constraint)
                      },
                  constraintTypeViews =
                    fmap
                      ( \constraintTy ->
                          TypeView
                            { typeViewDisplay = resolvedSrcTypeToSrcType constraintTy,
                              typeViewIdentity = resolvedSrcTypeIdentityType constraintTy
                            }
                      )
                      (P.constraintTypes constraint)
                }
              | constraint <- P.instanceDeclConstraints instDecl
            ]
       in InstanceInfo
        { instanceClassName = className classInfo,
          instanceClassSymbol = classInfoSymbolIdentity classInfo,
          instanceClassModule = classModule classInfo,
          instanceOriginModule = P.moduleName mod0,
          instanceConstraints = map displayConstraint constraintInfos,
          instanceConstraintInfos = constraintInfos,
          instanceHeadType = headTy,
          instanceHeadIdentityType = headIdentityTy,
          instanceHeadTypes = headTy :| [],
          instanceHeadIdentityTypes = headIdentityTy :| [],
          instanceMethods = Map.empty
        }

    resolvedDataDeclForEnv dataDecl = do
      constructors <-
        forM (P.dataDeclConstructors dataDecl) $ \ctor ->
          P.ConstructorDecl (P.constructorDeclName ctor) <$> displaySrcTypeForResolved displayEnv (P.constructorDeclType ctor)
      pure
        P.DataDecl
          { P.dataDeclName = P.dataDeclName dataDecl,
            P.dataDeclParams = P.dataDeclParams dataDecl,
            P.dataDeclConstructors = constructors,
            P.dataDeclDeriving = map resolvedSymbolDisplayName (P.dataDeclDeriving dataDecl)
          }

    constructorFieldTypes ctor =
      fst (splitArrows (snd (splitForalls (P.constructorDeclType ctor))))

    validateEqDerivingFields :: ClassInfo -> Scope -> P.DataDecl -> TcM ()
    validateEqDerivingFields eqClassInfo validationScope dataDecl =
      mapM_ (validateEqDerivingField eqClassInfo validationScope dataDecl) (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))

    validateEqDerivingField :: ClassInfo -> Scope -> P.DataDecl -> SrcType -> TcM ()
    validateEqDerivingField eqClassInfo validationScope dataDecl fieldTy
      | constraintTypeSatisfiable
          (classInfoSymbolIdentity eqClassInfo)
          (className eqClassInfo)
          validationScope
          dataDecl
          Set.empty
          (classInfoSymbolIdentity eqClassInfo)
          (className eqClassInfo)
          (sourceTypeViewInScope (scopeToElaborateScope validationScope) fieldTy) =
          pure ()
      | otherwise = throwError (ProgramDerivingMissingFieldInstance (className eqClassInfo) fieldTy)

    constraintTypeSatisfiable :: ClassIdentity -> P.ClassName -> Scope -> P.DataDecl -> Set.Set (ClassIdentity, String) -> ClassIdentity -> P.ClassName -> TypeView -> Bool
    constraintTypeSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl seen classIdentity0 className0 fieldView
      | classIdentity0 == derivedClassIdentity && fieldCoveredByDerivedConstraints dataDecl (typeViewDisplay fieldView) = True
      | key `Set.member` seen = False
      | otherwise =
          case resolveInstanceInfoWithIdentityType elaborateScope classIdentity0 className0 fieldView of
            Right (instanceInfo, subst) ->
              let seen' = Set.insert key seen
               in all
                    (constraintSatisfiable derivedClassIdentity derivedClassName validationScope dataDecl seen' . applyConstraintInfoSubst subst)
                    (instanceConstraintInfos instanceInfo)
            Left _ -> False
      where
        elaborateScope = scopeToElaborateScope validationScope
        key = (classIdentity0, show (typeViewIdentity fieldView))

    constraintSatisfiable :: ClassIdentity -> P.ClassName -> Scope -> P.DataDecl -> Set.Set (ClassIdentity, String) -> ConstraintInfo -> Bool
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

    fieldCoveredByDerivedConstraints dataDecl fieldTy =
      case fieldTy of
        STVar name -> name `elem` P.typeParamNames (P.dataDeclParams dataDecl)
        _ -> isRecursiveOwnerField dataDecl fieldTy

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
        STArrow dom cod -> freeTypeVars dom `Set.union` freeTypeVars cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap freeTypeVars args
        STVarApp name args -> Set.insert name (foldMap freeTypeVars args)
        STTyLam name body -> Set.delete name (freeTypeVars body)
        STTyApp fun arg -> freeTypeVars fun `Set.union` freeTypeVars arg
        STForall name mb body ->
          maybe Set.empty (freeTypeVars . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVars body)
        STMu name body -> Set.delete name (freeTypeVars body)
        STBottom -> Set.empty

    scopeToElaborateScope scope0 =
      mkElaborateScope (scopeValues scope0) (scopeElaborateTypes scope0) (scopeClasses scope0) (scopeInstances scope0)

    mkEqInstance classSymbol classInfo eqMethodSymbol resolvedDataDecl displayDataDecl = do
      dataSymbol <-
        uniqueResolvedLocalSymbol
          ProgramDuplicateType
          (P.dataDeclName resolvedDataDecl)
          (resolvedLocalTypes (resolvedSemanticModuleLocalSymbols resolvedModule))
      boolSymbol <- pure (Builtins.builtinTypeSymbol "Bool")
      andSymbol <- valueSymbolForName "__mlfp_and"
      ctorEntries <-
        forM (P.dataDeclConstructors resolvedDataDecl) $ \ctor -> do
          ctorSymbol <-
            lookupResolvedLocalValueSymbol
              resolvedModule
              SymbolConstructor
              (Just (SymbolOwnerType (P.moduleName mod0) (P.dataDeclName resolvedDataDecl)))
              (P.constructorDeclName ctor)
          ctorTy <- displaySrcTypeForResolved displayEnv (P.constructorDeclType ctor)
          let argTypes = fst (splitArrows (snd (splitForalls ctorTy)))
          pure (ctor, ctorSymbol, argTypes)
      let headTy = dataDeclHeadResolvedType dataSymbol resolvedDataDecl
          headDisplayTy = dataDeclHeadType displayDataDecl
          eqClassName = className classInfo
          left = P.Param "left" (Just headTy)
          right = P.Param "right" (Just headTy)
          selfName = "__derived_eq_" ++ P.dataDeclName resolvedDataDecl
          methodBody =
            if hasRecursiveOwnerFields displayDataDecl
              then
                P.ELet
                  selfName
                  (Just (RSTArrow headTy (RSTArrow headTy (RSTBase boolSymbol))))
                  (P.ELam left (P.ELam right (deriveEqBody eqClassName eqMethodSymbol andSymbol headDisplayTy displayDataDecl ctorEntries (Just selfName))))
                  (P.EVar (P.ResolvedLocalValue selfName))
              else
                P.ELam left (P.ELam right (deriveEqBody eqClassName eqMethodSymbol andSymbol headDisplayTy displayDataDecl ctorEntries Nothing))
      pure
        P.InstanceDecl
            { P.instanceDeclClass = classSymbol,
              P.instanceDeclConstraints =
                [ P.ClassConstraint
                    { P.constraintClassName = classSymbol,
                      P.constraintTypes = RSTVar paramName :| []
                    }
                  | paramName <- derivedConstraintParams displayDataDecl
                ],
              P.instanceDeclTypes = headTy :| [],
              P.instanceDeclMethods = [P.MethodDef "eq" methodBody]
            }

    hasRecursiveOwnerFields dataDecl =
      any (isRecursiveOwnerField dataDecl) (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))

    deriveEqBody eqClassName eqMethodSymbol andSymbol headTy dataDecl ctorEntries mbSelfName =
      P.ECase
        (P.EVar (P.ResolvedLocalValue "left"))
        [ P.Alt
            (P.PatCtor ctorSymbol (map P.PatVar leftNames))
            (P.ECase (P.EVar (P.ResolvedLocalValue "right")) (matchingAlt ctor ctorSymbol argTypes leftNames : mismatchAlts ctor))
          | (ctor, ctorSymbol, argTypes) <- ctorEntries,
            let leftNames = ["l" ++ show i | i <- [1 .. length argTypes]]
        ]
      where
        recursiveEqName = qualify (P.moduleName mod0) (renderInstanceName eqClassName headTy "eq")

        matchingAlt _ ctorSymbol argTypes leftNames =
          let rightNames = ["r" ++ show i | i <- [1 .. length leftNames]]
           in P.Alt (P.PatCtor ctorSymbol (map P.PatVar rightNames)) (foldEqCalls (zip3 argTypes leftNames rightNames))

        mismatchAlts ctor =
          [ P.Alt (P.PatCtor otherSymbol [P.PatWildcard | _ <- otherArgTypes]) (P.ELit (LBool False))
            | (other, otherSymbol, otherArgTypes) <- ctorEntries,
              P.constructorDeclName other /= P.constructorDeclName ctor
          ]

        foldEqCalls [] = P.ELit (LBool True)
        foldEqCalls [(argTy, l, r)] = eqCall argTy l r
        foldEqCalls ((argTy, l, r) : rest) =
          P.EApp
            (P.EApp (P.EVar (P.ResolvedGlobalValue andSymbol)) (eqCall argTy l r))
            (foldEqCalls rest)

        eqCall argTy l r =
          let eqRef =
                case mbSelfName of
                  Just selfName | isRecursiveOwnerField dataDecl argTy -> P.ResolvedLocalValue selfName
                  _ ->
                    if isRecursiveOwnerField dataDecl argTy
                      then P.ResolvedLocalValue recursiveEqName
                      else P.ResolvedGlobalValue eqMethodSymbol
           in P.EApp (P.EApp (P.EVar eqRef) (P.EVar (P.ResolvedLocalValue l))) (P.EVar (P.ResolvedLocalValue r))

    isRecursiveOwnerField dataDecl argTy =
      argTy == dataDeclHeadType dataDecl

    dataDeclHeadType dataDecl =
      case P.typeParamNames (P.dataDeclParams dataDecl) of
        [] -> STBase (P.dataDeclName dataDecl)
        param0 : paramsRest -> STCon (P.dataDeclName dataDecl) (STVar param0 :| map STVar paramsRest)

    dataDeclHeadResolvedType dataSymbol dataDecl =
      case P.typeParamNames (P.dataDeclParams dataDecl) of
        [] -> RSTBase dataSymbol
        param0 : paramsRest -> RSTCon dataSymbol (RSTVar param0 :| map RSTVar paramsRest)

    valueSymbolForName name =
      case Map.lookup name (scopeValues scope) of
        Just valueInfo -> pure (toResolvedValueSymbol valueInfo)
        Nothing -> throwError (ProgramUnknownValue name)

buildInstanceSkeletons :: DisplayNameEnv -> Scope -> P.ResolvedModuleSyntax -> [P.ResolvedInstanceDecl] -> TcM [InstanceInfo]
buildInstanceSkeletons displayEnv scope mod0 derived = do
  let instances0 = derived ++ explicitInstances mod0
  infos <- mapM toInstanceInfo instances0
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
  pure infos
  where
    toInstanceInfo instDecl = do
      classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
      instanceClassName0 <- displayClassName displayEnv (P.instanceDeclClass instDecl)
      validateResolvedClassConstraintClasses scope (P.instanceDeclConstraints instDecl)
      let instanceHeadTysResolved = P.instanceDeclTypes instDecl
      validateClassApplicationArity classInfo (length instanceHeadTysResolved)
      instanceHeadTypes0 <- mapM (displaySrcTypeForResolved displayEnv) instanceHeadTysResolved
      let instanceHeadType0 = NE.head instanceHeadTypes0
          instanceHeadIdentityTypes0 = fmap resolvedSrcTypeIdentityType instanceHeadTysResolved
          instanceHeadIdentityType0 = NE.head instanceHeadIdentityTypes0
          instanceHeadViews0 = NE.zipWith TypeView instanceHeadTypes0 instanceHeadIdentityTypes0
      declaredInstanceConstraints0 <- mapM (displayClassConstraintForResolved displayEnv) (P.instanceDeclConstraints instDecl)
      declaredInstanceConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.instanceDeclConstraints instDecl)
      let superclassConstraints0 =
            map
              (substituteConstraintTypes (classParamNames classInfo) instanceHeadTypes0)
              (classSuperclasses classInfo)
          superclassConstraintInfos0 =
            map
              (substituteConstraintInfoTypes (classParamNames classInfo) instanceHeadViews0)
              (classSuperclassInfos classInfo)
          instanceConstraints0 = declaredInstanceConstraints0 ++ superclassConstraints0
          instanceConstraintInfos0 = declaredInstanceConstraintInfos0 ++ superclassConstraintInfos0
      let methodMap = classMethods classInfo
          expected = Map.keysSet methodMap
          provided = Set.fromList (map P.methodDefName (P.instanceDeclMethods instDecl))
      case Set.toList (expected Set.\\ provided) of
        (missing : _) -> throwError (ProgramMissingInstanceMethod instanceClassName0 missing)
        [] -> pure ()
      case Set.toList (provided Set.\\ expected) of
        (extra : _) -> throwError (ProgramUnexpectedInstanceMethod instanceClassName0 extra)
        [] -> pure ()
      let instanceMethodInfos =
            Map.fromList
              [ let methodInfo = methodMap Map.! P.methodDefName methodDef
                    rawMethodType = specializeMethodTypes (methodType methodInfo) (classParamNames classInfo) instanceHeadTypes0
                    rawMethodIdentityType = specializeMethodTypes (methodTypeIdentity methodInfo) (classParamNames classInfo) instanceHeadIdentityTypes0
                    methodValueConstraints =
                      declaredInstanceConstraints0
                        ++ map
                          (substituteConstraintTypes (classParamNames classInfo) instanceHeadTypes0)
                          (methodConstraints methodInfo)
                    methodValueConstraintInfos =
                      declaredInstanceConstraintInfos0
                        ++ map
                          (substituteConstraintInfoTypes (classParamNames classInfo) instanceHeadViews0)
                          (methodConstraintInfos methodInfo)
                    methodValueIdentityType =
                      constrainedVisibleType $
                        P.ConstrainedType
                          (map displayConstraint methodValueConstraintInfos)
                          rawMethodIdentityType
                 in ( P.methodDefName methodDef,
                      OrdinaryValue
                        { valueDisplayName = P.methodDefName methodDef,
                          valueInfoSymbol =
                            SymbolIdentity
                              { symbolNamespace = SymbolValue,
                                symbolDefiningModule = P.moduleName mod0,
                                symbolDefiningName = renderInstanceNameHead instanceClassName0 instanceHeadTypes0 (P.methodDefName methodDef),
                                symbolOwnerIdentity = Nothing
                          },
                          valueRuntimeName = qualify (P.moduleName mod0) (renderInstanceNameHead instanceClassName0 instanceHeadTypes0 (P.methodDefName methodDef)),
                          valueType = constrainedVisibleType (P.ConstrainedType methodValueConstraints rawMethodType),
                          valueIdentityType = methodValueIdentityType,
                          valueConstraints = methodValueConstraints,
                          valueConstraintInfos = methodValueConstraintInfos,
                          valueOriginModule = P.moduleName mod0
                        }
                    )
                | methodDef <- P.instanceDeclMethods instDecl
              ]
      pure
        InstanceInfo
          { instanceClassName = instanceClassName0,
            instanceClassSymbol = classInfoSymbolIdentity classInfo,
            instanceClassModule = classModule classInfo,
            instanceOriginModule = P.moduleName mod0,
            instanceConstraints = instanceConstraints0,
            instanceConstraintInfos = instanceConstraintInfos0,
            instanceHeadType = instanceHeadType0,
            instanceHeadIdentityType = instanceHeadIdentityType0,
            instanceHeadTypes = instanceHeadTypes0,
            instanceHeadIdentityTypes = instanceHeadIdentityTypes0,
            instanceMethods = instanceMethodInfos
          }

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
          leftDeterminerIdentities = projectInstanceTypes determinerIndices (instanceHeadIdentityTypes left)
          rightDeterminerIdentities = projectInstanceTypes determinerIndices (instanceHeadIdentityTypes right)
          leftDeterminedIdentities = projectInstanceTypes determinedIndices (instanceHeadIdentityTypes left)
          rightDeterminedIdentities = projectInstanceTypes determinedIndices (instanceHeadIdentityTypes right)
      if functionalDependencyHeadsConflict leftDeterminerIdentities rightDeterminerIdentities leftDeterminedIdentities rightDeterminedIdentities
        then Just (className classInfo, leftDeterminers, leftDetermined, rightDetermined)
        else Nothing

    functionalDependencyHeadsConflict leftDeterminers rightDeterminers leftDetermined rightDetermined =
      case unifyTaggedProjections leftDeterminers rightDeterminers leftDetermined rightDetermined of
        Just True -> False
        Just False -> True
        Nothing -> False

    unifyTaggedProjections leftDeterminers rightDeterminers leftDetermined rightDetermined = do
      let (leftEnv, taggedLeftDeterminers) = tagTypeList "__fundep_left__" Map.empty (NE.toList leftDeterminers)
          (rightEnv, taggedRightDeterminers) = tagTypeList "__fundep_right__" Map.empty (NE.toList rightDeterminers)
          (_, taggedLeftDetermined) = tagTypeList "__fundep_left__" leftEnv (NE.toList leftDetermined)
          (_, taggedRightDetermined) = tagTypeList "__fundep_right__" rightEnv (NE.toList rightDetermined)
      determinerSubst <- unifyProjectionTypes Map.empty taggedLeftDeterminers taggedRightDeterminers
      case unifyProjectionTypes determinerSubst taggedLeftDetermined taggedRightDetermined of
        Just _ -> Just True
        Nothing -> Just False

    unifyProjectionTypes subst left right
      | length left /= length right = Nothing
      | otherwise = foldM (\acc (leftTy, rightTy) -> unifyOverlap acc leftTy rightTy) subst (zip left right)

    freeProjectedTypeVars indices tys =
      foldMap freeTypeVarsInType (projectInstanceTypes indices tys)

    functionalDependencyIndices classInfo fundep =
      (,) <$> traverse lookupParamIndex (P.fundepDeterminers fundep) <*> traverse lookupParamIndex (P.fundepDetermined fundep)
      where
        paramIndices = Map.fromList (zip (NE.toList (classParamNames classInfo)) [(0 :: Int) ..])
        lookupParamIndex name = Map.lookup name paramIndices

    projectInstanceTypes indices tys =
      let values = NE.toList tys
       in fmap (values !!) indices

    classInfoForInstance info =
      case Map.lookup (instanceInfoClassSymbolIdentity info) (scopeClassesByIdentity scope) of
        Just ((_, classInfo) : _) -> Just classInfo
        _ -> Nothing

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
          instanceHeadsOverlap (instanceHeadIdentityTypes left) (instanceHeadIdentityTypes right)
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
          instanceHeadsOverlap (instanceHeadIdentityTypes local) (instanceHeadIdentityTypes existing)
      ]

    sameInstanceClass left right =
      instanceClassIdentity left == instanceClassIdentity right

    sameCanonicalInstanceHead left right =
      sameInstanceClass left right
        && instanceHeadIdentityTypes left == instanceHeadIdentityTypes right

    instanceHeadsOverlap left right =
      length left == length right
        && case
          foldM
            ( \(subst, ix) (leftTy, rightTy) -> do
                subst' <-
                  unifyOverlap
                    subst
                    (tagTypeVars ("__overlap_left" ++ show ix ++ "__") leftTy)
                    (tagTypeVars ("__overlap_right" ++ show ix ++ "__") rightTy)
                pure (subst', ix + 1)
            )
            (Map.empty, 0 :: Int)
            (zip (NE.toList left) (NE.toList right))
        of
          Just _ -> True
          Nothing -> False

    unifyOverlap subst left right =
      case (applyOverlapSubst subst left, applyOverlapSubst subst right) of
        (STVar name, ty) -> bindOverlap name ty subst
        (ty, STVar name) -> bindOverlap name ty subst
        (STBase leftName, STBase rightName)
          | leftName == rightName -> Just subst
        (STCon leftName leftArgs, STCon rightName rightArgs)
          | leftName == rightName && NE.length leftArgs == NE.length rightArgs ->
              foldM
                (\acc (leftTy, rightTy) -> unifyOverlap acc leftTy rightTy)
                subst
                (zip (NE.toList leftArgs) (NE.toList rightArgs))
        (STTyLam leftName leftBody, STTyLam rightName rightBody)
          | leftName == rightName -> unifyOverlap subst leftBody rightBody
        (STTyApp leftFun leftArg, STTyApp rightFun rightArg) -> do
          subst' <- unifyOverlap subst leftFun rightFun
          unifyOverlap subst' leftArg rightArg
        (STVarApp leftName leftArgs, rightTy) ->
          unifyOverlapTypeHead subst leftName (NE.toList leftArgs) rightTy
        (leftTy, STVarApp rightName rightArgs) ->
          unifyOverlapTypeHead subst rightName (NE.toList rightArgs) leftTy
        (STArrow leftDom leftCod, STArrow rightDom rightCod) -> do
          subst' <- unifyOverlap subst leftDom rightDom
          unifyOverlap subst' leftCod rightCod
        _ -> Nothing

    unifyOverlapTypeHead subst name templateArgs actual =
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
              subst' <- bindOverlap name appliedHead subst
              foldM
                (\acc (templateTy, actualTy) -> unifyOverlap acc templateTy actualTy)
                subst'
                (zip templateArgs matchedArgs)

    bindOverlap name ty subst =
      case Map.lookup name subst of
        Just existing -> unifyOverlap subst existing ty
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

    substituteConstraintInfoTypes paramNames headViews constraint =
      let views = fmap substituteView (constraintTypeViews constraint)
       in constraint
            { constraintTypeView = NE.head views,
              constraintTypeViews = views
            }
      where
        substituteView view =
          TypeView
            { typeViewDisplay = substituteTypeVars paramNames (fmap typeViewDisplay headViews) (typeViewDisplay view),
              typeViewIdentity = substituteTypeVars paramNames (fmap typeViewIdentity headViews) (typeViewIdentity view)
            }

    substituteTypeVars paramNames headTys ty =
      Map.foldrWithKey substituteTypeVar ty (Map.fromList (zip (NE.toList paramNames) (NE.toList headTys)))

renderInstanceName :: P.ClassName -> SrcType -> P.MethodName -> String
renderInstanceName className0 headTy =
  renderInstanceNameHead className0 (headTy :| [])

renderInstanceNameHead :: P.ClassName -> NonEmpty SrcType -> P.MethodName -> String
renderInstanceNameHead className0 headTys methodName0 =
  intercalate "__" (className0 : map sanitizeType (NE.toList headTys) ++ [methodName0])

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
  where
    sanitizeName = concatMap sanitizeChar

    sanitizeChar c
      | c `elem` ['a' .. 'z'] = [c]
      | c `elem` ['A' .. 'Z'] = [c]
      | c `elem` ['0' .. '9'] = [c]
      | otherwise = "_u" ++ show (fromEnum c) ++ "_"

checkDef :: FinalizeContext -> ElaborateScope -> Scope -> P.ResolvedDefDecl -> TcM CheckedBinding
checkDef finalizeContext elaborateScope scope defDecl = do
  valueInfo <- lookupValueInfo scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      liftEither
        ( lowerResolvedConstrainedExprBinding elaborateScope (valueRuntimeName ordinary) (P.defDeclType defDecl) (P.defDeclName defDecl == "main") (P.defDeclExpr defDecl)
            >>= finalizeBindingAllowOpaqueWithContext finalizeContext
        )
    _ -> throwError (ProgramDuplicateValue (P.defDeclName defDecl))

buildExports :: P.ResolvedModuleSyntax -> Map String DataInfo -> Map String ClassInfo -> Map String ValueInfo -> TcM ModuleExports
buildExports mod0 localData localClasses localValues = do
  let exportItems = P.moduleExports mod0
      defaultValues = Map.filter (\info -> case info of OverloadedMethod {} -> True; ConstructorValue {} -> True; OrdinaryValue {} -> True) localValues
      defaultTypes = Map.fromList [(name, ExportedTypeInfo info Map.empty) | (name, info) <- Map.toList localData]
      defaultClasses = localClasses
  case exportItems of
    Nothing ->
      pure
        ModuleExports
          { exportedValues = defaultValues,
            exportedTypes = defaultTypes,
            exportedClasses = defaultClasses
          }
    Just items -> do
      values <- foldM (collectResolvedExportValue localValues localClasses localData) Map.empty items
      types <- foldM (collectResolvedExportType (P.moduleName mod0) localData) Map.empty items
      classes <- foldM (collectResolvedExportClass localClasses) Map.empty items
      pure
        ModuleExports
          { exportedValues = values,
            exportedTypes = types,
            exportedClasses = classes
          }

collectResolvedExportValue :: Map String ValueInfo -> Map String ClassInfo -> Map String DataInfo -> Map String ValueInfo -> P.ResolvedExportItem -> TcM (Map String ValueInfo)
collectResolvedExportValue localValues localClasses localData acc = \case
  P.ExportValue symbol ->
    case localValueByIdentity (resolvedSymbolIdentity symbol) localValues of
      Just (name, info) -> pure (Map.insert name info acc)
      Nothing -> throwError (ProgramExportNotLocal (resolvedSymbolDisplayName symbol))
  P.ExportTypeWithConstructors ref ->
    case localDataByRef ref localData of
      Just (_, dataInfo) ->
        let ctorValues =
              Map.fromList
                [ ( ctorName ctor,
                    ConstructorValue
                      { valueDisplayName = ctorName ctor,
                        valueInfoSymbol = constructorInfoSymbolIdentity dataInfo ctor,
                        valueRuntimeName = ctorRuntimeName ctor,
                        valueType = ctorType ctor,
                        valueCtorInfo = ctor,
                        valueOriginModule = dataModule dataInfo
                      }
                  )
                  | ctor <- dataConstructors dataInfo
                ]
         in liftEither (addValues acc ctorValues)
      Nothing -> throwError (ProgramExportNotLocal (P.resolvedExportTypeName ref))
  P.ExportType ref ->
    case localClassByRef ref localClasses of
      Just (_, classInfo) ->
        let methodValues =
              Map.fromList
                [ ( methodName method,
                    OverloadedMethod
                      { valueDisplayName = methodName method,
                        valueInfoSymbol = methodInfoSymbolIdentity method,
                        valueMethodInfo = method,
                        valueOriginModule = classModule classInfo
                      }
                  )
                  | method <- Map.elems (classMethods classInfo)
                ]
         in liftEither (addValues acc methodValues)
      Nothing -> pure acc

collectResolvedExportType :: P.ModuleName -> Map String DataInfo -> Map String ExportedTypeInfo -> P.ResolvedExportItem -> TcM (Map String ExportedTypeInfo)
collectResolvedExportType moduleName0 localData acc = \case
  P.ExportType ref ->
    case localDataByRef ref localData of
      Just (typeName, dataInfo) -> pure (Map.insert typeName (ExportedTypeInfo dataInfo Map.empty) acc)
      Nothing
        | moduleName0 == "Prelude",
          Just dataInfo <- builtinOpaqueDataByRef ref ->
            pure (Map.insert (P.resolvedExportTypeName ref) (ExportedTypeInfo dataInfo Map.empty) acc)
      Nothing -> pure acc
  P.ExportTypeWithConstructors ref ->
    case localDataByRef ref localData of
      Just (typeName, dataInfo) ->
        pure
          (Map.insert typeName (ExportedTypeInfo dataInfo (Map.fromList [(ctorName ctor, ctor) | ctor <- dataConstructors dataInfo])) acc)
      Nothing -> throwError (ProgramExportNotLocal (P.resolvedExportTypeName ref))
  P.ExportValue _ -> pure acc

builtinOpaqueDataByRef :: P.ResolvedExportTypeRef -> Maybe DataInfo
builtinOpaqueDataByRef ref =
  case
    [ dataInfo
      | symbol <- P.resolvedExportTypeSymbols ref,
        Builtins.isBuiltinTypeSymbol symbol,
        Just dataInfo <- [Map.lookup (symbolDefiningName (resolvedSymbolIdentity symbol)) Builtins.builtinOpaqueTypes]
    ]
  of
    dataInfo : _ -> Just dataInfo
    [] -> Nothing

collectResolvedExportClass :: Map String ClassInfo -> Map String ClassInfo -> P.ResolvedExportItem -> TcM (Map String ClassInfo)
collectResolvedExportClass localClasses acc = \case
  P.ExportType ref ->
    case localClassByRef ref localClasses of
      Just (className0, classInfo) -> pure (Map.insert className0 classInfo acc)
      Nothing -> pure acc
  _ -> pure acc

localValueByIdentity :: SymbolIdentity -> Map String ValueInfo -> Maybe (String, ValueInfo)
localValueByIdentity identity =
  find ((== identity) . valueInfoSymbolIdentity . snd) . Map.toList

localDataByRef :: P.ResolvedExportTypeRef -> Map String DataInfo -> Maybe (String, DataInfo)
localDataByRef ref localData =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolType] of
    symbol : _ -> find ((== resolvedSymbolIdentity symbol) . dataInfoSymbolIdentity . snd) (Map.toList localData)
    [] -> Nothing

localClassByRef :: P.ResolvedExportTypeRef -> Map String ClassInfo -> Maybe (String, ClassInfo)
localClassByRef ref localClasses =
  case [symbol | symbol <- P.resolvedExportTypeSymbols ref, symbolNamespace (resolvedSymbolIdentity symbol) == SymbolClass] of
    symbol : _ -> find ((== resolvedSymbolIdentity symbol) . classInfoSymbolIdentity . snd) (Map.toList localClasses)
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
