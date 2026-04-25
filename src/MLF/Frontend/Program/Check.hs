{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Check
  ( ProgramError (..),
    ProgramDiagnostic (..),
    CheckedProgram (..),
    CheckedModule (..),
    CheckedBinding (..),
    DataInfo (..),
    ConstructorInfo (..),
    ClassInfo (..),
    MethodInfo (..),
    InstanceInfo (..),
    ValueInfo (..),
    ExportedTypeInfo (..),
    ModuleExports (..),
    checkProgram,
    checkResolvedProgram,
    checkLocatedProgram,
  )
where

import Control.Monad (foldM, forM, when, zipWithM)
import Control.Monad.Except (MonadError (throwError))
import Data.List (find, intercalate, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    lowerConstructorBinding,
    lowerConstrainedResolvedExprBinding,
    lowerResolvedConstrainedExprBinding,
    mkElaborateScope,
    resolveInstanceInfoWithIdentityType,
    sourceTypeViewInScope,
  )
import MLF.Frontend.Program.Finalize (finalizeBinding)
import MLF.Frontend.Program.Resolve (resolveProgram)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ClassInfo (..),
    ConstructorInfo (..),
    DataInfo (..),
    ExportedTypeInfo (..),
    InstanceInfo (..),
    MethodInfo (..),
    ModuleExports (..),
    ProgramDiagnostic (..),
    ProgramError (..),
    ResolvedModule (..),
    ResolvedProgram (..),
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
    dataInfoSymbolIdentity,
    diagnosticForProgramError,
    instanceInfoClassSymbolIdentity,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    mkResolvedSymbol,
    displayConstraint,
    specializeMethodType,
    substituteTypeVar,
    splitArrows,
    splitForalls,
    valueInfoSymbolIdentity,
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

type TcM a = Either ProgramError a

runTcM :: TcM a -> Either ProgramError a
runTcM = id

-- Scope ----------------------------------------------------------------------

data Scope = Scope
  { scopeValues :: Map String ValueInfo,
    scopeValuesByIdentity :: Map SymbolIdentity [(String, ValueInfo)],
    scopeTypes :: Map String DataInfo,
    scopeTypesByIdentity :: Map SymbolIdentity [(String, DataInfo)],
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
emptyScope = mkScope builtinValues Map.empty Map.empty []

mkScope :: Map String ValueInfo -> Map String DataInfo -> Map String ClassInfo -> [InstanceInfo] -> Scope
mkScope values0 types0 classes0 instances0 =
  Scope
    { scopeValues = values0,
      scopeValuesByIdentity = indexByIdentity valueInfoSymbolIdentity values0,
      scopeTypes = types0,
      scopeTypesByIdentity = indexByIdentity dataInfoSymbolIdentity types0,
      scopeClasses = classes0,
      scopeClassesByIdentity = indexByIdentity classInfoSymbolIdentity classes0,
      scopeInstances = instances0
    }

withScopeValues :: Map String ValueInfo -> Scope -> Scope
withScopeValues values0 scope =
  mkScope values0 (scopeTypes scope) (scopeClasses scope) (scopeInstances scope)

withScopeTypes :: Map String DataInfo -> Scope -> Scope
withScopeTypes types0 scope =
  mkScope (scopeValues scope) types0 (scopeClasses scope) (scopeInstances scope)

withScopeClasses :: Map String ClassInfo -> Scope -> Scope
withScopeClasses classes0 scope =
  mkScope (scopeValues scope) (scopeTypes scope) classes0 (scopeInstances scope)

withScopeInstances :: [InstanceInfo] -> Scope -> Scope
withScopeInstances instances0 scope =
  mkScope (scopeValues scope) (scopeTypes scope) (scopeClasses scope) instances0

indexByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity [(String, a)]
indexByIdentity identityOf =
  Map.fromListWith (++) . map (\(name, info) -> (identityOf info, [(name, info)])) . Map.toList

builtinValues :: Map String ValueInfo
builtinValues =
  Map.singleton
    "__mlfp_and"
    OrdinaryValue
      { valueDisplayName = "__mlfp_and",
        valueInfoSymbol =
          SymbolIdentity
            { symbolNamespace = SymbolValue,
              symbolDefiningModule = "<builtin>",
              symbolDefiningName = "__mlfp_and",
              symbolOwnerIdentity = Nothing
        },
        valueRuntimeName = "__mlfp_and",
        valueType = STArrow (STBase "Bool") (STArrow (STBase "Bool") (STBase "Bool")),
        valueIdentityType = STArrow (STBase "Bool") (STArrow (STBase "Bool") (STBase "Bool")),
        valueConstraints = [],
        valueConstraintInfos = [],
        valueOriginModule = "<builtin>"
      }

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

displayNameEnvFromResolvedLocals :: ResolvedModule -> DisplayNameEnv
displayNameEnvFromResolvedLocals resolvedModule =
  DisplayNameEnv
    { dneValues = localNames resolvedModuleLocalValues,
      dneTypes = localNames resolvedModuleLocalTypes,
      dneClasses = localNames resolvedModuleLocalClasses
    }
  where
    localNames select =
      Map.fromListWith (++)
        [ (resolvedSymbolIdentity symbol, [name])
          | (name, symbols) <- Map.toList (select resolvedModule),
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
isBuiltinTypeSymbol symbol =
  let identity = resolvedSymbolIdentity symbol
   in symbolNamespace identity == SymbolType
        && symbolDefiningModule identity == "<builtin>"

-- Program checking ------------------------------------------------------------

checkProgram :: P.Program -> Either ProgramError CheckedProgram
checkProgram program =
  resolveProgram program >>= checkResolvedProgram

checkResolvedProgram :: ResolvedProgram -> Either ProgramError CheckedProgram
checkResolvedProgram resolved = runTcM $ do
  modulesChecked <- checkModules resolved
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
  pure
    CheckedProgram
      { checkedProgramModules = modulesChecked,
        checkedProgramMain = mainRuntime,
        checkedProgramResolved = resolved
      }

checkLocatedProgram :: P.LocatedProgram -> Either ProgramDiagnostic CheckedProgram
checkLocatedProgram located =
  case checkProgram (P.locatedProgram located) of
    Right checked -> Right checked
    Left err -> Left (diagnosticForProgramError (Just located) err)

lookupResolvedLocalTypeIdentity :: ResolvedModule -> P.TypeName -> TcM SymbolIdentity
lookupResolvedLocalTypeIdentity resolvedModule name =
  resolvedSymbolIdentity <$> uniqueResolvedLocalSymbol ProgramDuplicateType name (resolvedModuleLocalTypes resolvedModule)

lookupResolvedLocalClassIdentity :: ResolvedModule -> P.ClassName -> TcM SymbolIdentity
lookupResolvedLocalClassIdentity resolvedModule name =
  resolvedSymbolIdentity <$> uniqueResolvedLocalSymbol ProgramDuplicateClass name (resolvedModuleLocalClasses resolvedModule)

lookupResolvedLocalValueIdentity ::
  ResolvedModule ->
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
        (resolvedModuleLocalValues resolvedModule)

lookupResolvedLocalValueSymbol ::
  ResolvedModule ->
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
        (resolvedModuleLocalValues resolvedModule)

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

checkModules :: ResolvedProgram -> TcM [CheckedModule]
checkModules (ResolvedProgram resolvedModules) = do
  ensureDistinctBy ProgramDuplicateModule resolvedModuleName resolvedModules
  go [] resolvedModules
  where
    go acc [] = pure (reverse acc)
    go acc (resolvedModule : rest) = do
      checked <- checkModule resolvedModule acc
      go (checked : acc) rest

checkModule :: ResolvedModule -> [CheckedModule] -> TcM CheckedModule
checkModule resolvedModule priorModules = do
  let resolvedSyntax = resolvedModuleSyntax resolvedModule
      moduleName0 = resolvedModuleName resolvedModule
      priorExports = Map.fromList [(checkedModuleName checked, checkedModuleExports checked) | checked <- priorModules]
      priorData = Map.fromList [(checkedModuleName checked, checkedModuleData checked) | checked <- priorModules]
      priorInstances = concatMap checkedModuleInstances priorModules
      unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExports (P.moduleImports resolvedSyntax)
      visibleImportedInstances =
        visibleInstancesForImports priorExports priorData priorInstances unqualifiedClassIdentities (P.moduleImports resolvedSyntax)
  ensureDistinctImportAliases (P.moduleImports resolvedSyntax)
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
  let scope0 = mkScope valueScope typeScope classScope (scopeInstances importScope ++ visibleImportedInstances)
      fullNameEnv = valueNameEnv `preferDisplayNames` displayNameEnvFromScope scope0
  validateModuleKinds scope0 resolvedSyntax
  validateLocalClassMethodConstraints scope0 resolvedSyntax
  derivedInstances <- synthesizeDerivedInstances fullNameEnv scope0 resolvedModule resolvedSyntax
  instanceSkeletons <- buildInstanceSkeletons fullNameEnv scope0 resolvedSyntax derivedInstances
  let scope1 = withScopeInstances (scopeInstances scope0 ++ instanceSkeletons) scope0
  let elaborateScope = mkElaborateScope (scopeValues scope1) (scopeTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
  constructorBindings <-
    mapM
      (liftEither . (finalizeBinding elaborateScope . lowerConstructorBinding elaborateScope))
      [ ctor
        | dataInfo <- Map.elems localData,
          ctor <- dataConstructors dataInfo,
          constructorRuntimeBindingRecoverable ctor
      ]
  instanceBindings <- concat <$> mapM (checkInstance elaborateScope scope1) (derivedInstances ++ explicitInstances resolvedSyntax)
  defBindings <- mapM (checkDef elaborateScope scope1) (moduleDefDecls resolvedSyntax)
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
addAllExports scope exports =
  liftEither $ do
    values <- addValues (scopeValues scope) (exportedValues exports)
    types <- addTypes (scopeTypes scope) (Map.map exportedTypeData (exportedTypes exports))
    classes <- addClasses (scopeClasses scope) (exportedClasses exports)
    pure (mkScope values types classes (scopeInstances scope))

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
          ctorOwningTypeIdentity = ctorOwningTypeIdentity ctor
        }

    qualifyClassInfo classInfo =
      let qualifiedClassName = qualifiedName (className classInfo)
          qualifyMethod methodInfo =
            methodInfo
              { methodClassName = qualifiedClassName,
                methodType = qualifySrcType (methodType methodInfo),
                methodConstraints = map qualifyConstraint (methodConstraints methodInfo)
              }
       in classInfo
            { className = qualifiedClassName,
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
          methodConstraints = map qualifyConstraint (methodConstraints methodInfo)
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
          P.constraintType = qualifySrcType (P.constraintType constraint)
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

resolvedClassIdentityFromSymbol :: ResolvedSymbol -> Maybe ClassIdentity
resolvedClassIdentityFromSymbol symbol =
  let identity = resolvedSymbolIdentity symbol
   in case symbolNamespace identity of
        SymbolClass -> Just identity
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
          Set.fromList
            [ identity
              | symbol <- P.resolvedExportTypeSymbols ref,
                Just identity <- [resolvedClassIdentityFromSymbol symbol]
            ]
        P.ExportValue symbol ->
          Set.fromList (maybe [] (: []) (resolvedMethodOwnerClassIdentity symbol))
        _ -> Set.empty

    overloadedMethodClassIdentities =
      Set.fromList . mapMaybe methodClassIdentity

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
           || srcTypeMentionsAny exportedTypeNames (instanceHeadIdentityType instanceInfo)
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
    && instanceHeadIdentityType left == instanceHeadIdentityType right

instanceExportedTypeMentions :: Set.Set String -> InstanceInfo -> Set.Set String
instanceExportedTypeMentions exportedTypeNames instanceInfo =
  Set.unions (headMentions : constraintMentions ++ methodMentions)
  where
    headMentions = srcTypeMentionedNames exportedTypeNames (instanceHeadIdentityType instanceInfo)
    constraintMentions = map (srcTypeMentionedNames exportedTypeNames . typeViewIdentity . constraintTypeView) (instanceConstraintInfos instanceInfo)
    methodMentions = concatMap valueExportedTypeMentions (Map.elems (instanceMethods instanceInfo))

    valueExportedTypeMentions valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          srcTypeMentionedNames exportedTypeNames (valueIdentityType valueInfo)
            : map (srcTypeMentionedNames exportedTypeNames . typeViewIdentity . constraintTypeView) (valueConstraintInfos valueInfo)
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
          values <- liftEither (addValues (scopeValues scope) (Map.singleton name info))
          pure (withScopeValues values scope)
        Nothing -> throwError (ProgramImportNotExported moduleName0 (resolvedSymbolDisplayName symbol))
    P.ExportType ref ->
      case exportedTypeByRef ref exports of
        Just (typeName, typeInfo) -> do
          let dataInfo = exportedTypeData typeInfo
          types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
          let scope' = withScopeTypes types scope
          case exportedClassByRef ref exports of
            Just (className0, classInfo) -> do
              classes <- liftEither (addClasses (scopeClasses scope') (Map.singleton className0 classInfo))
              pure (withScopeClasses classes scope')
            Nothing -> pure scope'
        Nothing ->
          case exportedClassByRef ref exports of
            Just (className0, classInfo) -> do
              classes <- liftEither (addClasses (scopeClasses scope) (Map.singleton className0 classInfo))
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
          pure (mkScope values types (scopeClasses scope) (scopeInstances scope))
        Nothing -> throwError (ProgramImportNotExported moduleName0 (P.resolvedExportTypeName ref))

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

displaySrcTypeForResolved :: DisplayNameEnv -> ResolvedSrcType -> TcM SrcType
displaySrcTypeForResolved env = \case
  RSTVar name -> pure (STVar name)
  RSTArrow dom cod -> STArrow <$> displaySrcTypeForResolved env dom <*> displaySrcTypeForResolved env cod
  RSTBase symbol -> STBase <$> displayTypeHeadName env symbol
  RSTCon symbol args -> STCon <$> displayTypeHeadName env symbol <*> traverse (displaySrcTypeForResolved env) args
  RSTVarApp name args -> STVarApp name <$> traverse (displaySrcTypeForResolved env) args
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
displayClassConstraintForResolved env constraint =
  P.ClassConstraint
    <$> displayClassName env (P.constraintClassName constraint)
    <*> displaySrcTypeForResolved env (P.constraintType constraint)

typeViewForDisplayEnv :: DisplayNameEnv -> ResolvedSrcType -> TcM TypeView
typeViewForDisplayEnv env ty =
  TypeView
    <$> displaySrcTypeForResolved env ty
    <*> pure (resolvedSrcTypeIdentityType ty)

constraintInfoForDisplayEnv :: DisplayNameEnv -> P.ResolvedClassConstraint -> TcM ConstraintInfo
constraintInfoForDisplayEnv env constraint =
  ConstraintInfo
    <$> displayClassName env (P.constraintClassName constraint)
    <*> pure (resolvedSymbolIdentity (P.constraintClassName constraint))
    <*> typeViewForDisplayEnv env (P.constraintType constraint)

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
            | dataInfo <- Map.elems (scopeTypes scope)
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
  env <- extendKindParams [P.classDeclParam classDecl] baseEnv
  mapM_ (validateMethodSigKind scope env) (P.classDeclMethods classDecl)

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
  _ <- checkResolvedKind env1 (P.instanceDeclType instDecl) (P.typeParamKind (classTypeParam classInfo))
  pure ()

validateConstrainedTypeKinds :: Scope -> KindEnv -> P.ResolvedConstrainedType -> TcM KindEnv
validateConstrainedTypeKinds scope env0 ty = do
  env1 <- foldM (validateClassConstraintKind scope) env0 (P.constrainedConstraints ty)
  checkResolvedKind env1 (P.constrainedBody ty) P.KType

validateClassConstraintKind :: Scope -> KindEnv -> P.ResolvedClassConstraint -> TcM KindEnv
validateClassConstraintKind scope env constraint = do
  classInfo <- lookupClassInfoBySymbol scope (P.constraintClassName constraint)
  checkResolvedKind env (P.constraintType constraint) (P.typeParamKind (classTypeParam classInfo))

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
  | isBuiltinTypeSymbol symbol = Just P.KType
  | otherwise = Map.lookup (resolvedSymbolIdentity symbol) (kindTypeConstructors env)

buildLocalDataInfo :: DisplayNameEnv -> ResolvedModule -> P.ResolvedModuleSyntax -> TcM (Map String DataInfo)
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
      constructors <- zipWithM (toCtorInfo dataDecl dataIdentity) [0 ..] (P.dataDeclConstructors dataDecl)
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
            ctorIndex = index
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

buildLocalClassInfo :: DisplayNameEnv -> ResolvedModule -> P.ResolvedModuleSyntax -> TcM (Map String ClassInfo)
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
                        methodParamName = classParamName0
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
              classMethods = methods
            }
        )

validateLocalClassMethodConstraints :: Scope -> P.ResolvedModuleSyntax -> TcM ()
validateLocalClassMethodConstraints scope mod0 =
  mapM_ validateClassDecl (moduleClassDecls mod0)
  where
    validateClassDecl classDecl =
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

buildLocalDefInfo :: DisplayNameEnv -> ResolvedModule -> P.ResolvedModuleSyntax -> TcM (Map String ValueInfo)
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

constructorRuntimeBindingRecoverable :: ConstructorInfo -> Bool
constructorRuntimeBindingRecoverable ctor =
  let involvedTypes =
        ctorArgs ctor
          ++ [ctorResult ctor]
          ++ mapMaybe snd (ctorForalls ctor)
      evidenceVars = foldMap freeTypeVars involvedTypes
   in not (any hasVariableHeadApplication involvedTypes)
        && all (\(name, _) -> name `Set.member` evidenceVars) (ctorForalls ctor)
  where
    hasVariableHeadApplication ty =
      case ty of
        STVar {} -> False
        STArrow dom cod -> hasVariableHeadApplication dom || hasVariableHeadApplication cod
        STBase {} -> False
        STCon _ args -> any hasVariableHeadApplication args
        STVarApp {} -> True
        STForall _ mb body ->
          maybe False (hasVariableHeadApplication . unSrcBound) mb
            || hasVariableHeadApplication body
        STMu _ body -> hasVariableHeadApplication body
        STBottom -> False

    freeTypeVars ty =
      case ty of
        STVar name -> Set.singleton name
        STArrow dom cod -> freeTypeVars dom `Set.union` freeTypeVars cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap freeTypeVars args
        STVarApp name args -> Set.insert name (foldMap freeTypeVars args)
        STForall name mb body ->
          maybe Set.empty (freeTypeVars . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVars body)
        STMu name body -> Set.delete name (freeTypeVars body)
        STBottom -> Set.empty

synthesizeDerivedInstances :: DisplayNameEnv -> Scope -> ResolvedModule -> P.ResolvedModuleSyntax -> TcM [P.ResolvedInstanceDecl]
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
                      }
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
        STForall name mb body ->
          maybe Set.empty (freeTypeVars . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVars body)
        STMu name body -> Set.delete name (freeTypeVars body)
        STBottom -> Set.empty

    scopeToElaborateScope scope0 =
      mkElaborateScope (scopeValues scope0) (scopeTypes scope0) (scopeClasses scope0) (scopeInstances scope0)

    mkEqInstance classSymbol classInfo eqMethodSymbol resolvedDataDecl displayDataDecl = do
      dataSymbol <- uniqueResolvedLocalSymbol ProgramDuplicateType (P.dataDeclName resolvedDataDecl) (resolvedModuleLocalTypes resolvedModule)
      boolSymbol <- pure (builtinTypeSymbol "Bool")
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
                [ P.ClassConstraint classSymbol (RSTVar paramName)
                  | paramName <- derivedConstraintParams displayDataDecl
                ],
              P.instanceDeclType = headTy,
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

    builtinTypeSymbol name =
      mkResolvedSymbol
        ( SymbolIdentity
            { symbolNamespace = SymbolType,
              symbolDefiningModule = "<builtin>",
              symbolDefiningName = name,
              symbolOwnerIdentity = Nothing
            }
        )
        name
        name
        SymbolBuiltin

    valueSymbolForName name =
      case Map.lookup name (scopeValues scope) of
        Just valueInfo -> pure (toResolvedValueSymbol valueInfo)
        Nothing -> throwError (ProgramUnknownValue name)

buildInstanceSkeletons :: DisplayNameEnv -> Scope -> P.ResolvedModuleSyntax -> [P.ResolvedInstanceDecl] -> TcM [InstanceInfo]
buildInstanceSkeletons displayEnv scope mod0 derived = do
  let instances0 = derived ++ explicitInstances mod0
  infos <- mapM toInstanceInfo instances0
  case duplicateLocalInstances infos of
    info : _ -> throwError (ProgramDuplicateInstance (instanceClassName info) (instanceHeadType info))
    [] -> pure ()
  case duplicateExistingInstances infos of
    info : _ -> throwError (ProgramDuplicateInstance (instanceClassName info) (instanceHeadType info))
    [] -> pure ()
  case overlappingInstances infos of
    (left, right) : _ ->
      throwError (ProgramOverlappingInstance (instanceClassName left) (instanceHeadType left) (instanceHeadType right))
    [] -> pure ()
  case overlappingWithExistingInstances infos of
    (left, right) : _ ->
      throwError (ProgramOverlappingInstance (instanceClassName left) (instanceHeadType left) (instanceHeadType right))
    [] -> pure ()
  pure infos
  where
    toInstanceInfo instDecl = do
      classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
      instanceClassName0 <- displayClassName displayEnv (P.instanceDeclClass instDecl)
      validateResolvedClassConstraintClasses scope (P.instanceDeclConstraints instDecl)
      instanceHeadType0 <- displaySrcTypeForResolved displayEnv (P.instanceDeclType instDecl)
      let instanceHeadIdentityType0 = resolvedSrcTypeIdentityType (P.instanceDeclType instDecl)
          instanceHeadView0 =
            TypeView
              { typeViewDisplay = instanceHeadType0,
                typeViewIdentity = instanceHeadIdentityType0
              }
      instanceConstraints0 <- mapM (displayClassConstraintForResolved displayEnv) (P.instanceDeclConstraints instDecl)
      instanceConstraintInfos0 <- mapM (constraintInfoForDisplayEnv displayEnv) (P.instanceDeclConstraints instDecl)
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
                    rawMethodType = specializeMethodType (methodType methodInfo) (classParamName classInfo) instanceHeadType0
                    rawMethodIdentityType = specializeMethodType (methodTypeIdentity methodInfo) (classParamName classInfo) instanceHeadIdentityType0
                    methodValueConstraints =
                      instanceConstraints0
                        ++ map
                          (substituteConstraint (classParamName classInfo) instanceHeadType0)
                          (methodConstraints methodInfo)
                    methodValueConstraintInfos =
                      instanceConstraintInfos0
                        ++ map
                          (substituteConstraintInfo (classParamName classInfo) instanceHeadView0)
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
                                symbolDefiningName = renderInstanceName instanceClassName0 instanceHeadType0 (P.methodDefName methodDef),
                                symbolOwnerIdentity = Nothing
                          },
                          valueRuntimeName = qualify (P.moduleName mod0) (renderInstanceName instanceClassName0 instanceHeadType0 (P.methodDefName methodDef)),
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
            instanceMethods = instanceMethodInfos
          }

    overlappingInstances infos =
      [ (left, right)
        | (ix, left) <- zip [(0 :: Int) ..] infos,
          right <- drop (ix + 1) infos,
          sameInstanceClass left right,
          instanceHeadIdentityType left /= instanceHeadIdentityType right,
          instanceHeadsOverlap (instanceHeadIdentityType left) (instanceHeadIdentityType right)
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
          instanceHeadIdentityType local /= instanceHeadIdentityType existing,
          instanceHeadsOverlap (instanceHeadIdentityType local) (instanceHeadIdentityType existing)
      ]

    sameInstanceClass left right =
      instanceClassIdentity left == instanceClassIdentity right

    sameCanonicalInstanceHead left right =
      sameInstanceClass left right
        && instanceHeadIdentityType left == instanceHeadIdentityType right

    instanceHeadsOverlap left right =
      case
        unifyOverlap
          Map.empty
          (tagTypeVars "__overlap_left__" left)
          (tagTypeVars "__overlap_right__" right)
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
        STForall name mb body ->
          STForall name (fmap (SrcBound . applyOverlapSubst subst . unSrcBound) mb) (applyOverlapSubst subst body)
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
            STForall name mb body ->
              let tagged = prefix ++ name
                  env' = Map.insert name tagged env
               in STForall tagged (fmap (SrcBound . go env . unSrcBound) mb) (go env' body)
            STMu name body ->
              let tagged = prefix ++ name
               in STMu tagged (go (Map.insert name tagged env) body)
            STBase {} -> ty
            STBottom -> STBottom

    freeTypeVarsInType ty =
      case ty of
        STVar name -> Set.singleton name
        STArrow dom cod -> freeTypeVarsInType dom `Set.union` freeTypeVarsInType cod
        STCon _ args -> foldMap freeTypeVarsInType args
        STVarApp name args -> Set.insert name (foldMap freeTypeVarsInType args)
        STForall name mb body ->
          maybe Set.empty (freeTypeVarsInType . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVarsInType body)
        STMu name body -> Set.delete name (freeTypeVarsInType body)
        STBase {} -> Set.empty
        STBottom -> Set.empty

    substituteConstraint paramName headTy constraint =
      constraint
        { P.constraintType = substituteTypeVar paramName headTy (P.constraintType constraint)
        }

    substituteConstraintInfo paramName headView constraint =
      constraint
        { constraintTypeView =
            TypeView
              { typeViewDisplay = substituteTypeVar paramName (typeViewDisplay headView) (typeViewDisplay (constraintTypeView constraint)),
                typeViewIdentity = substituteTypeVar paramName (typeViewIdentity headView) (typeViewIdentity (constraintTypeView constraint))
              }
        }

renderInstanceName :: P.ClassName -> SrcType -> P.MethodName -> String
renderInstanceName className0 headTy methodName0 = className0 ++ "__" ++ sanitizeType headTy ++ "__" ++ methodName0

sanitizeType :: SrcType -> String
sanitizeType = \case
  STVar v -> sanitizeName v
  STArrow dom cod -> "arr_" ++ sanitizeType dom ++ "_" ++ sanitizeType cod
  STBase base -> sanitizeName base
  STCon con args -> intercalate "_" (sanitizeName con : map sanitizeType (NE.toList args))
  STVarApp name args -> intercalate "_" (sanitizeName name : map sanitizeType (NE.toList args))
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

checkInstance :: ElaborateScope -> Scope -> P.ResolvedInstanceDecl -> TcM [CheckedBinding]
checkInstance elaborateScope scope instDecl = do
  classInfo <- lookupClassInfoBySymbol scope (P.instanceDeclClass instDecl)
  let headTy = resolvedSrcTypeToSrcType (P.instanceDeclType instDecl)
  instanceInfo <-
    case findInstance classInfo (resolvedSrcTypeIdentityType (P.instanceDeclType instDecl)) of
      Just info -> pure info
      Nothing -> throwError (ProgramNoMatchingInstance (className classInfo) headTy)
  forM (P.instanceDeclMethods instDecl) $ \methodDef -> do
    case instanceMethods instanceInfo Map.! P.methodDefName methodDef of
      valueInfo@OrdinaryValue {} -> do
        let methodRuntimeName = valueRuntimeName valueInfo
            methodSourceView =
              TypeView
                { typeViewDisplay = valueType valueInfo,
                  typeViewIdentity = valueIdentityType valueInfo
                }
        liftEither
          ( lowerConstrainedResolvedExprBinding elaborateScope methodRuntimeName (valueConstraintInfos valueInfo) methodSourceView False (P.methodDefExpr methodDef)
              >>= finalizeBinding elaborateScope
          )
      _ -> throwError (ProgramUnexpectedInstanceMethod (className classInfo) (P.methodDefName methodDef))
  where
    findInstance classInfo headIdentityTy =
      find
        ( \info ->
            instanceClassIdentity info == classIdentity classInfo
              && instanceHeadIdentityType info == headIdentityTy
        )
        (scopeInstances scope)

checkDef :: ElaborateScope -> Scope -> P.ResolvedDefDecl -> TcM CheckedBinding
checkDef elaborateScope scope defDecl = do
  valueInfo <- lookupValueInfo scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      liftEither
        ( lowerResolvedConstrainedExprBinding elaborateScope (valueRuntimeName ordinary) (P.defDeclType defDecl) (P.defDeclName defDecl == "main") (P.defDeclExpr defDecl)
            >>= finalizeBinding elaborateScope
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
      types <- foldM (collectResolvedExportType localData) Map.empty items
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

collectResolvedExportType :: Map String DataInfo -> Map String ExportedTypeInfo -> P.ResolvedExportItem -> TcM (Map String ExportedTypeInfo)
collectResolvedExportType localData acc = \case
  P.ExportType ref ->
    case localDataByRef ref localData of
      Just (typeName, dataInfo) -> pure (Map.insert typeName (ExportedTypeInfo dataInfo Map.empty) acc)
      Nothing -> pure acc
  P.ExportTypeWithConstructors ref ->
    case localDataByRef ref localData of
      Just (typeName, dataInfo) ->
        pure
          (Map.insert typeName (ExportedTypeInfo dataInfo (Map.fromList [(ctorName ctor, ctor) | ctor <- dataConstructors dataInfo])) acc)
      Nothing -> throwError (ProgramExportNotLocal (P.resolvedExportTypeName ref))
  P.ExportValue _ -> pure acc

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
