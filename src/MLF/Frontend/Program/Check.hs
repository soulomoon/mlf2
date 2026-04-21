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
    lowerConstrainedExprBinding,
    mkElaborateScope,
    resolveInstanceInfoWithSubst,
  )
import MLF.Frontend.Program.Finalize (finalizeBinding)
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
    ValueInfo (..),
    constrainedVisibleType,
    diagnosticForProgramError,
    specializeMethodType,
    substituteTypeVar,
    splitArrows,
    splitForalls,
  )
import MLF.Frontend.Syntax (Lit (..), SrcBound (..), SrcTy (..), SrcType)
import qualified MLF.Frontend.Syntax.Program as P

type TcM a = Either ProgramError a

runTcM :: TcM a -> Either ProgramError a
runTcM = id

-- Scope ----------------------------------------------------------------------

data Scope = Scope
  { scopeValues :: Map String ValueInfo,
    scopeTypes :: Map String DataInfo,
    scopeClasses :: Map String ClassInfo,
    scopeInstances :: [InstanceInfo]
  }
  deriving (Eq, Show)

type ClassIdentity = (P.ModuleName, P.ClassName)

emptyScope :: Scope
emptyScope = Scope builtinValues Map.empty Map.empty []

builtinValues :: Map String ValueInfo
builtinValues =
  Map.singleton
    "__mlfp_and"
    OrdinaryValue
      { valueDisplayName = "__mlfp_and",
        valueRuntimeName = "__mlfp_and",
        valueType = STArrow (STBase "Bool") (STArrow (STBase "Bool") (STBase "Bool")),
        valueConstraints = [],
        valueOriginModule = "<builtin>"
      }

addValues :: Map String ValueInfo -> Map String ValueInfo -> Either ProgramError (Map String ValueInfo)
addValues base incoming =
  foldM
    ( \acc (name, info) ->
        if Map.member name acc
          then Left (ProgramDuplicateVisibleName name)
          else Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

addTypes :: Map String DataInfo -> Map String DataInfo -> Either ProgramError (Map String DataInfo)
addTypes base incoming =
  foldM
    ( \acc (name, info) ->
        if Map.member name acc
          then Left (ProgramDuplicateVisibleName name)
          else Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

addClasses :: Map String ClassInfo -> Map String ClassInfo -> Either ProgramError (Map String ClassInfo)
addClasses base incoming =
  foldM
    ( \acc (name, info) ->
        if Map.member name acc
          then Left (ProgramDuplicateVisibleName name)
          else Right (Map.insert name info acc)
    )
    base
    (Map.toList incoming)

lookupValueInfo :: Scope -> String -> TcM ValueInfo
lookupValueInfo scope name =
  case Map.lookup name (scopeValues scope) of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownValue name)

lookupClassInfo :: Scope -> String -> TcM ClassInfo
lookupClassInfo scope name =
  case Map.lookup name (scopeClasses scope) of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownClass name)

-- Program checking ------------------------------------------------------------

checkProgram :: P.Program -> Either ProgramError CheckedProgram
checkProgram program = runTcM $ do
  modulesChecked <- checkModules program
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
        checkedProgramMain = mainRuntime
      }

checkLocatedProgram :: P.LocatedProgram -> Either ProgramDiagnostic CheckedProgram
checkLocatedProgram located =
  case checkProgram (P.locatedProgram located) of
    Right checked -> Right checked
    Left err -> Left (diagnosticForProgramError (Just located) err)

checkModules :: P.Program -> TcM [CheckedModule]
checkModules (P.Program modules0) = do
  ensureDistinctBy ProgramDuplicateModule P.moduleName modules0
  orderedModules <- topoSortModules modules0
  go [] orderedModules
  where
    go acc [] = pure (reverse acc)
    go acc (mod0 : rest) = do
      checked <- checkModule acc mod0
      go (checked : acc) rest

topoSortModules :: [P.Module] -> TcM [P.Module]
topoSortModules modules0 = do
  (_, _, orderedRev) <- foldM visit (Set.empty, Set.empty, []) (map P.moduleName modules0)
  pure (reverse orderedRev)
  where
    moduleMap = Map.fromList [(P.moduleName mod0, mod0) | mod0 <- modules0]

    visit (tempMarks, permMarks, ordered) moduleName0
      | moduleName0 `Set.member` permMarks = pure (tempMarks, permMarks, ordered)
      | moduleName0 `Set.member` tempMarks = throwError (ProgramImportCycle [moduleName0])
      | otherwise = do
          mod0 <-
            case Map.lookup moduleName0 moduleMap of
              Just found -> pure found
              Nothing -> throwError (ProgramUnknownImportModule moduleName0)
          let tempMarks' = Set.insert moduleName0 tempMarks
          (_, permMarks', ordered') <-
            foldM
              visit
              (tempMarks', permMarks, ordered)
              [P.importModuleName imp | imp <- P.moduleImports mod0]
          pure
            ( Set.delete moduleName0 tempMarks',
              Set.insert moduleName0 permMarks',
              mod0 : ordered'
            )

checkModule :: [CheckedModule] -> P.Module -> TcM CheckedModule
checkModule priorModules mod0 = do
  let priorExports = Map.fromList [(checkedModuleName checked, checkedModuleExports checked) | checked <- priorModules]
      priorInstances = concatMap checkedModuleInstances priorModules
      unqualifiedClassIdentities = importedUnqualifiedClassIdentities priorExports (P.moduleImports mod0)
      qualifiedPriorInstances =
        concatMap
          (qualifiedInstancesForImport priorExports priorInstances unqualifiedClassIdentities)
          (P.moduleImports mod0)
  ensureDistinctImportAliases (P.moduleImports mod0)
  importScope <- buildImportScope priorExports (P.moduleImports mod0)
  localData <- buildLocalDataInfo mod0
  localClasses <- buildLocalClassInfo mod0
  localDefs <- buildLocalDefInfo mod0
  localValues0 <- addConstructorValues (P.moduleName mod0) localData
  localValues1 <- mergeMaps ProgramDuplicateValue localValues0 localDefs
  let localMethodValues =
        Map.fromList
          [ (methodName method, OverloadedMethod (methodName method) method (P.moduleName mod0))
            | classInfo <- Map.elems localClasses,
              method <- Map.elems (classMethods classInfo)
          ]
  localValues <- mergeMaps ProgramDuplicateValue localValues1 localMethodValues
  valueScope <- liftEither =<< pure (addValues (scopeValues importScope) localValues)
  typeScope <- liftEither =<< pure (addTypes (scopeTypes importScope) localData)
  classScope <- liftEither =<< pure (addClasses (scopeClasses importScope) localClasses)
  let scope0 = Scope valueScope typeScope classScope (scopeInstances importScope ++ priorInstances ++ qualifiedPriorInstances)
  validateLocalClassMethodConstraints scope0 mod0
  derivedInstances <- synthesizeDerivedInstances scope0 mod0
  instanceSkeletons <- buildInstanceSkeletons scope0 mod0 derivedInstances
  let scope1 = scope0 {scopeInstances = scopeInstances scope0 ++ instanceSkeletons}
  let elaborateScope = mkElaborateScope (scopeValues scope1) (scopeTypes scope1) (scopeClasses scope1) (scopeInstances scope1)
  constructorBindings <-
    mapM
      (liftEither . (finalizeBinding elaborateScope . lowerConstructorBinding elaborateScope))
      [ ctor
        | dataInfo <- Map.elems localData,
          ctor <- dataConstructors dataInfo,
          constructorRuntimeBindingRecoverable ctor
      ]
  instanceBindings <- concat <$> mapM (checkInstance elaborateScope scope1) (derivedInstances ++ explicitInstances mod0)
  defBindings <- mapM (checkDef elaborateScope scope1) (moduleDefDecls mod0)
  exports <- buildExports mod0 localData localClasses localValues
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
      { checkedModuleName = P.moduleName mod0,
        checkedModuleBindings = constructorBindings ++ instanceBindings ++ map markExportedMain defBindings,
        checkedModuleData = localData,
        checkedModuleClasses = localClasses,
        checkedModuleInstances = instanceSkeletons,
        checkedModuleExports = exports
      }

moduleDefDecls :: P.Module -> [P.DefDecl]
moduleDefDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclDef defDecl -> defDecl : acc
      _ -> acc

explicitInstances :: P.Module -> [P.InstanceDecl]
explicitInstances = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclInstance instDecl -> instDecl : acc
      _ -> acc

moduleDataDecls :: P.Module -> [P.DataDecl]
moduleDataDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclData dataDecl -> dataDecl : acc
      _ -> acc

moduleClassDecls :: P.Module -> [P.ClassDecl]
moduleClassDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclClass classDecl -> classDecl : acc
      _ -> acc

buildImportScope :: Map P.ModuleName ModuleExports -> [P.Import] -> TcM Scope
buildImportScope priorExports imports0 = foldM go emptyScope imports0
  where
    go scope imp = do
      exports <-
        case Map.lookup (P.importModuleName imp) priorExports of
          Nothing -> throwError (ProgramUnknownImportModule (P.importModuleName imp))
          Just ex -> pure ex
      case P.importAlias imp of
        Nothing ->
          case P.importExposing imp of
            Nothing -> addAllExports scope exports
            Just items -> foldM (applyImportItem (P.importModuleName imp) exports) scope items
        Just alias -> do
          qualifiedScope <- addAllExports scope (qualifyModuleExports alias exports)
          case P.importExposing imp of
            Nothing -> pure qualifiedScope
            Just items -> foldM (applyImportItem (P.importModuleName imp) exports) qualifiedScope items

addAllExports :: Scope -> ModuleExports -> TcM Scope
addAllExports scope exports =
  liftEither $ do
    values <- addValues (scopeValues scope) (exportedValues exports)
    types <- addTypes (scopeTypes scope) (Map.map exportedTypeData (exportedTypes exports))
    classes <- addClasses (scopeClasses scope) (exportedClasses exports)
    pure
      scope
        { scopeValues = values,
          scopeTypes = types,
          scopeClasses = classes
        }

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
          ctorOwningType = ctorOwningType ctor
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
              valueMethodInfo = qualifyMethodFromExport methodInfo,
              valueOriginModule = valueOriginModule valueInfo
            }
        ConstructorValue {valueCtorInfo = ctorInfo} ->
          let qualifiedCtorInfo = qualifyConstructorInfo ctorInfo
           in ConstructorValue
                { valueDisplayName = qualifiedName sourceName,
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

unqualifiedName :: String -> String
unqualifiedName =
  reverse . takeWhile (/= '.') . reverse

classIdentity :: ClassInfo -> ClassIdentity
classIdentity classInfo =
  (classModule classInfo, unqualifiedName (className classInfo))

methodClassIdentity :: ValueInfo -> Maybe ClassIdentity
methodClassIdentity valueInfo =
  case valueInfo of
    OverloadedMethod {valueMethodInfo = methodInfo} ->
      Just (valueOriginModule valueInfo, unqualifiedName (methodClassName methodInfo))
    _ -> Nothing

instanceClassIdentity :: InstanceInfo -> ClassIdentity
instanceClassIdentity instanceInfo =
  (instanceClassModule instanceInfo, unqualifiedName (instanceClassName instanceInfo))

qualifiedInstancesForImport :: Map P.ModuleName ModuleExports -> [InstanceInfo] -> Set.Set ClassIdentity -> P.Import -> [InstanceInfo]
qualifiedInstancesForImport priorExports priorInstances unqualifiedClassIdentities imp =
  case P.importAlias imp of
    Nothing -> []
    Just alias ->
      case Map.lookup (P.importModuleName imp) priorExports of
        Nothing -> []
        Just exports ->
          let importedInstances =
                [ instanceInfo
                  | instanceInfo <- priorInstances,
                    instanceBelongsToModule (P.importModuleName imp) instanceInfo,
                    instanceVisibleForQualifiedImport exports instanceInfo
                ]
              unqualifiedTypeNames = importExposedTypeNames imp
           in concatMap (qualifiedInstanceVariants alias exports unqualifiedTypeNames unqualifiedClassIdentities) importedInstances

importedUnqualifiedClassIdentities ::
  Map P.ModuleName ModuleExports ->
  [P.Import] ->
  Set.Set ClassIdentity
importedUnqualifiedClassIdentities priorExports =
  Set.unions . map importUnqualifiedClassIdentities
  where
    importUnqualifiedClassIdentities imp =
      case Map.lookup (P.importModuleName imp) priorExports of
        Nothing -> Set.empty
        Just exports ->
          case (P.importAlias imp, P.importExposing imp) of
            (Nothing, Nothing) ->
              Set.fromList (map classIdentity (Map.elems (exportedClasses exports)))
                `Set.union` overloadedMethodClassIdentities (Map.elems (exportedValues exports))
            (_, Just items) -> Set.unions (map (importItemClassIdentities exports) items)
            (Just _, Nothing) -> Set.empty

    importItemClassIdentities exports item =
      case item of
        P.ExportType name
          | Just classInfo <- Map.lookup name (exportedClasses exports) -> Set.singleton (classIdentity classInfo)
        P.ExportValue name ->
          case Map.lookup name (exportedValues exports) of
            Just valueInfo -> maybe Set.empty Set.singleton (methodClassIdentity valueInfo)
            _ -> Set.empty
        _ -> Set.empty

    overloadedMethodClassIdentities =
      Set.fromList . mapMaybe methodClassIdentity

importExposedTypeNames :: P.Import -> Set.Set String
importExposedTypeNames imp =
  case P.importExposing imp of
    Nothing -> Set.empty
    Just items -> Set.fromList (concatMap exposedTypeName items)
  where
    exposedTypeName item =
      case item of
        P.ExportType name -> [name]
        P.ExportTypeWithConstructors name -> [name]
        P.ExportValue {} -> []

instanceVisibleForQualifiedImport :: ModuleExports -> InstanceInfo -> Bool
instanceVisibleForQualifiedImport exports instanceInfo =
  instanceClassName instanceInfo `Map.member` exportedClasses exports
    || srcTypeMentionsAny (Map.keysSet (exportedTypes exports)) (instanceHeadType instanceInfo)

srcTypeMentionsAny :: Set.Set String -> SrcType -> Bool
srcTypeMentionsAny names ty =
  case ty of
    STVar {} -> False
    STBase name -> name `Set.member` names
    STCon name args -> name `Set.member` names || any (srcTypeMentionsAny names) args
    STArrow dom cod -> srcTypeMentionsAny names dom || srcTypeMentionsAny names cod
    STForall _ mb body ->
      maybe False (srcTypeMentionsAny names . unSrcBound) mb
        || srcTypeMentionsAny names body
    STMu _ body -> srcTypeMentionsAny names body
    STBottom -> False

qualifiedInstanceVariants :: P.ModuleName -> ModuleExports -> Set.Set String -> Set.Set ClassIdentity -> InstanceInfo -> [InstanceInfo]
qualifiedInstanceVariants alias exports unqualifiedTypeNames unqualifiedClassIdentities instanceInfo =
  distinctInstanceHeads
    [ variant
      | variant <- fullQualifiedVariants ++ aliasHeadVariants,
        not (sameInstanceHead instanceInfo variant)
    ]
  where
    qualifiedInstance = qualifyInstance alias exports instanceInfo
    aliasHeadNeeded =
      instanceClassIdentity instanceInfo `Set.member` unqualifiedClassIdentities
        && needsAliasHeadInstanceVariant exports unqualifiedTypeNames instanceInfo
    fullQualifiedVariants =
      [ qualifiedInstance
        | shouldEmitQualifiedInstanceVariant
      ]
    aliasHeadVariants =
      [ qualifyInstanceHeadOnly alias exports instanceInfo
        | aliasHeadNeeded
      ]
    shouldEmitQualifiedInstanceVariant =
      instanceClassName qualifiedInstance /= instanceClassName instanceInfo
        || aliasHeadNeeded

needsAliasHeadInstanceVariant :: ModuleExports -> Set.Set String -> InstanceInfo -> Bool
needsAliasHeadInstanceVariant exports unqualifiedTypeNames instanceInfo =
  not (Set.null mentionedTypeNames)
    && not (mentionedTypeNames `Set.isSubsetOf` unqualifiedTypeNames)
  where
    exportedTypeNames = Map.keysSet (exportedTypes exports)
    mentionedTypeNames =
      instanceExportedTypeMentions exportedTypeNames instanceInfo

distinctInstanceHeads :: [InstanceInfo] -> [InstanceInfo]
distinctInstanceHeads = reverse . foldl' add []
  where
    add acc instanceInfo
      | any (sameInstanceHead instanceInfo) acc = acc
      | otherwise = instanceInfo : acc

sameInstanceHead :: InstanceInfo -> InstanceInfo -> Bool
sameInstanceHead left right =
  instanceClassName left == instanceClassName right
    && instanceHeadType left == instanceHeadType right

instanceExportedTypeMentions :: Set.Set String -> InstanceInfo -> Set.Set String
instanceExportedTypeMentions exportedTypeNames instanceInfo =
  Set.unions (headMentions : constraintMentions ++ methodMentions)
  where
    headMentions = srcTypeMentionedNames exportedTypeNames (instanceHeadType instanceInfo)
    constraintMentions = map (srcTypeMentionedNames exportedTypeNames . P.constraintType) (instanceConstraints instanceInfo)
    methodMentions = concatMap valueExportedTypeMentions (Map.elems (instanceMethods instanceInfo))

    valueExportedTypeMentions valueInfo =
      case valueInfo of
        OrdinaryValue {} ->
          srcTypeMentionedNames exportedTypeNames (valueType valueInfo)
            : map (srcTypeMentionedNames exportedTypeNames . P.constraintType) (valueConstraints valueInfo)
        _ -> []

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

qualifyInstance :: P.ModuleName -> ModuleExports -> InstanceInfo -> InstanceInfo
qualifyInstance alias exports instanceInfo =
  instanceInfo
    { instanceClassName = qualifyClassName (instanceClassName instanceInfo),
      instanceConstraints = map qualifyConstraint (instanceConstraints instanceInfo),
      instanceHeadType = qualifySrcType (instanceHeadType instanceInfo),
      instanceMethods = Map.map qualifyMethodValue (instanceMethods instanceInfo)
    }
  where
    qualifiedName name = alias ++ "." ++ name
    exportedTypeNames = Map.keysSet (exportedTypes exports)
    exportedClassNames = Map.keysSet (exportedClasses exports)

    qualifyClassName className0
      | className0 `Set.member` exportedClassNames = qualifiedName className0
      | otherwise = className0

    qualifyConstraint constraint =
      constraint
        { P.constraintClassName = qualifyClassName (P.constraintClassName constraint),
          P.constraintType = qualifySrcType (P.constraintType constraint)
        }

    qualifyMethodValue valueInfo@OrdinaryValue {} =
      valueInfo
        { valueType = qualifySrcType (valueType valueInfo),
          valueConstraints = map qualifyConstraint (valueConstraints valueInfo)
        }
    qualifyMethodValue valueInfo = valueInfo

    qualifySrcType ty =
      case ty of
        STVar {} -> ty
        STBase name
          | name `Set.member` exportedTypeNames -> STBase (qualifiedName name)
          | otherwise -> ty
        STCon name args
          | name `Set.member` exportedTypeNames -> STCon (qualifiedName name) (fmap qualifySrcType args)
          | otherwise -> STCon name (fmap qualifySrcType args)
        STArrow dom cod -> STArrow (qualifySrcType dom) (qualifySrcType cod)
        STForall name mb body -> STForall name (fmap (SrcBound . qualifySrcType . unSrcBound) mb) (qualifySrcType body)
        STMu name body -> STMu name (qualifySrcType body)
        STBottom -> STBottom

qualifyInstanceHeadOnly :: P.ModuleName -> ModuleExports -> InstanceInfo -> InstanceInfo
qualifyInstanceHeadOnly alias exports instanceInfo =
  instanceInfo
    { instanceConstraints = map qualifyConstraintType (instanceConstraints instanceInfo),
      instanceHeadType = qualifySrcType (instanceHeadType instanceInfo),
      instanceMethods = Map.map qualifyMethodValue (instanceMethods instanceInfo)
    }
  where
    qualifiedName name = alias ++ "." ++ name
    exportedTypeNames = Map.keysSet (exportedTypes exports)

    qualifyConstraintType constraint =
      constraint {P.constraintType = qualifySrcType (P.constraintType constraint)}

    qualifyMethodValue valueInfo@OrdinaryValue {} =
      valueInfo
        { valueType = qualifySrcType (valueType valueInfo),
          valueConstraints = map qualifyConstraintType (valueConstraints valueInfo)
        }
    qualifyMethodValue valueInfo = valueInfo

    qualifySrcType ty =
      case ty of
        STVar {} -> ty
        STBase name
          | name `Set.member` exportedTypeNames -> STBase (qualifiedName name)
          | otherwise -> ty
        STCon name args
          | name `Set.member` exportedTypeNames -> STCon (qualifiedName name) (fmap qualifySrcType args)
          | otherwise -> STCon name (fmap qualifySrcType args)
        STArrow dom cod -> STArrow (qualifySrcType dom) (qualifySrcType cod)
        STForall name mb body -> STForall name (fmap (SrcBound . qualifySrcType . unSrcBound) mb) (qualifySrcType body)
        STMu name body -> STMu name (qualifySrcType body)
        STBottom -> STBottom

applyImportItem :: P.ModuleName -> ModuleExports -> Scope -> P.ExportItem -> TcM Scope
applyImportItem moduleName0 exports scope item =
  case item of
    P.ExportValue name ->
      case Map.lookup name (exportedValues exports) of
        Just info -> do
          values <- liftEither (addValues (scopeValues scope) (Map.singleton name info))
          pure scope {scopeValues = values}
        Nothing -> throwError (ProgramImportNotExported moduleName0 name)
    P.ExportType typeName ->
      case Map.lookup typeName (exportedTypes exports) of
        Just typeInfo -> do
          let dataInfo = exportedTypeData typeInfo
          types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
          let scope' = scope {scopeTypes = types}
          case Map.lookup typeName (exportedClasses exports) of
            Just classInfo -> do
              classes <- liftEither (addClasses (scopeClasses scope') (Map.singleton typeName classInfo))
              pure scope' {scopeClasses = classes}
            Nothing -> pure scope'
        Nothing ->
          case Map.lookup typeName (exportedClasses exports) of
            Just classInfo -> do
              classes <- liftEither (addClasses (scopeClasses scope) (Map.singleton typeName classInfo))
              pure scope {scopeClasses = classes}
            Nothing -> throwError (ProgramImportNotExported moduleName0 typeName)
    P.ExportTypeWithConstructors typeName ->
      case Map.lookup typeName (exportedTypes exports) of
        Just typeInfo -> do
          when (Map.null (exportedTypeConstructors typeInfo)) $
            throwError (ProgramImportNotExported moduleName0 typeName)
          let dataInfo = exportedTypeData typeInfo
              ctorValues =
                Map.fromList
                  [ ( ctorName ctor,
                      ConstructorValue (ctorName ctor) (ctorRuntimeName ctor) (ctorType ctor) ctor (dataModule dataInfo)
                    )
                    | ctor <- Map.elems (exportedTypeConstructors typeInfo)
                  ]
          values <- liftEither (addValues (scopeValues scope) ctorValues)
          types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
          pure
            scope
              { scopeValues = values,
                scopeTypes = types
              }
        Nothing -> throwError (ProgramImportNotExported moduleName0 typeName)

buildLocalDataInfo :: P.Module -> TcM (Map String DataInfo)
buildLocalDataInfo mod0 = do
  let dataDecls = moduleDataDecls mod0
  ensureDistinctBy ProgramDuplicateType P.dataDeclName dataDecls
  ctorNames <- pure (concatMap (map P.constructorDeclName . P.dataDeclConstructors) dataDecls)
  ensureDistinctPlain ProgramDuplicateConstructor ctorNames
  pure . Map.fromList =<< mapM toDataInfo dataDecls
  where
    toDataInfo dataDecl = do
      constructors <- zipWithM (toCtorInfo dataDecl) [0 ..] (P.dataDeclConstructors dataDecl)
      pure
        ( P.dataDeclName dataDecl,
          DataInfo
            { dataName = P.dataDeclName dataDecl,
              dataModule = P.moduleName mod0,
              dataParams = P.dataDeclParams dataDecl,
              dataConstructors = constructors
            }
        )

    toCtorInfo dataDecl index ctorDecl =
      let (foralls, ctorBody) = splitForalls (P.constructorDeclType ctorDecl)
          (args0, result0) = splitArrows ctorBody
       in do
            validateConstructorResult dataDecl ctorDecl result0
            pure
              ConstructorInfo
                { ctorName = P.constructorDeclName ctorDecl,
                  ctorRuntimeName = qualify (P.moduleName mod0) (P.constructorDeclName ctorDecl),
                  ctorType = P.constructorDeclType ctorDecl,
                  ctorForalls = foralls,
                  ctorArgs = args0,
                  ctorResult = result0,
                  ctorOwningType = P.dataDeclName dataDecl,
                  ctorIndex = index
                }

    validateConstructorResult :: P.DataDecl -> P.ConstructorDecl -> SrcType -> TcM ()
    validateConstructorResult dataDecl ctorDecl resultTy =
      let owner = P.dataDeclName dataDecl
          params = P.dataDeclParams dataDecl
          invalid = throwError (ProgramInvalidConstructorResult (P.constructorDeclName ctorDecl) resultTy owner)
       in case (params, resultTy) of
            ([], STBase name)
              | name == owner -> pure ()
            (_ : _, STCon name args)
              | name == owner && NE.length args == length params -> pure ()
            _ -> invalid

buildLocalClassInfo :: P.Module -> TcM (Map String ClassInfo)
buildLocalClassInfo mod0 = do
  let classDecls = moduleClassDecls mod0
  ensureDistinctBy ProgramDuplicateClass P.classDeclName classDecls
  pure . Map.fromList =<< mapM toClassInfo classDecls
  where
    toClassInfo classDecl = do
      ensureDistinctBy ProgramDuplicateMethod P.methodSigName (P.classDeclMethods classDecl)
      let methods =
            Map.fromList
              [ ( P.methodSigName sig,
                  MethodInfo
                    { methodClassName = P.classDeclName classDecl,
                      methodName = P.methodSigName sig,
                      methodRuntimeBase = qualify (P.moduleName mod0) (P.classDeclName classDecl ++ "__" ++ P.methodSigName sig),
                      methodType = P.constrainedBody (P.methodSigType sig),
                      methodConstraints = P.constrainedConstraints (P.methodSigType sig),
                      methodParamName = P.classDeclParam classDecl
                    }
                )
                | sig <- P.classDeclMethods classDecl
              ]
      pure
        ( P.classDeclName classDecl,
          ClassInfo
            { className = P.classDeclName classDecl,
              classModule = P.moduleName mod0,
              classParamName = P.classDeclParam classDecl,
              classMethods = methods
            }
        )

validateLocalClassMethodConstraints :: Scope -> P.Module -> TcM ()
validateLocalClassMethodConstraints scope mod0 =
  mapM_ validateClassDecl (moduleClassDecls mod0)
  where
    validateClassDecl classDecl =
      mapM_ validateMethodConstraints (P.classDeclMethods classDecl)

    validateMethodConstraints =
      validateClassConstraintClasses scope
        . P.constrainedConstraints
        . P.methodSigType

validateClassConstraintClasses :: Scope -> [P.ClassConstraint] -> TcM ()
validateClassConstraintClasses scope =
  mapM_ $ \constraint -> do
    _ <- lookupClassInfo scope (P.constraintClassName constraint)
    pure ()

buildLocalDefInfo :: P.Module -> TcM (Map String ValueInfo)
buildLocalDefInfo mod0 = do
  let defs = moduleDefDecls mod0
  ensureDistinctBy ProgramDuplicateValue P.defDeclName defs
  pure $
    Map.fromList
      [ ( P.defDeclName defDecl,
          OrdinaryValue
            { valueDisplayName = P.defDeclName defDecl,
              valueRuntimeName = qualify (P.moduleName mod0) (P.defDeclName defDecl),
              valueType = constrainedVisibleType (P.defDeclType defDecl),
              valueConstraints = P.constrainedConstraints (P.defDeclType defDecl),
              valueOriginModule = P.moduleName mod0
            }
        )
        | defDecl <- defs
      ]

addConstructorValues :: P.ModuleName -> Map String DataInfo -> TcM (Map String ValueInfo)
addConstructorValues moduleName0 dataInfos =
  pure $
    Map.fromList
      [ ( ctorName ctor,
          ConstructorValue
            { valueDisplayName = ctorName ctor,
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
  let evidenceVars = foldMap freeTypeVars (ctorArgs ctor ++ [ctorResult ctor])
   in all (\(name, _) -> name `Set.member` evidenceVars) (ctorForalls ctor)
  where
    freeTypeVars ty =
      case ty of
        STVar name -> Set.singleton name
        STArrow dom cod -> freeTypeVars dom `Set.union` freeTypeVars cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap freeTypeVars args
        STForall name mb body ->
          maybe Set.empty (freeTypeVars . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVars body)
        STMu name body -> Set.delete name (freeTypeVars body)
        STBottom -> Set.empty

synthesizeDerivedInstances :: Scope -> P.Module -> TcM [P.InstanceDecl]
synthesizeDerivedInstances scope mod0 = do
  candidates <- concat <$> mapM deriveForData (moduleDataDecls mod0)
  let pendingInstances = map (\(_, classInfo, instDecl) -> pendingDerivedInstance classInfo instDecl) candidates
      validationScope = scope {scopeInstances = scopeInstances scope ++ pendingInstances}
  mapM_
    (\(dataDecl, _, instDecl) -> validateEqDerivingFields (P.instanceDeclClass instDecl) validationScope dataDecl)
    candidates
  pure [instDecl | (_, _, instDecl) <- candidates]
  where
    deriveForData dataDecl = do
      forM (P.dataDeclDeriving dataDecl) $ \className0 -> do
        if unqualifiedName className0 == "Eq"
          then do
            classInfo <- lookupClassInfo scope className0
            case eqMethodReference classInfo of
              Just eqMethodName -> pure (dataDecl, classInfo, mkEqInstance classInfo eqMethodName dataDecl)
              Nothing -> throwError (ProgramUnsupportedDeriving className0)
          else throwError (ProgramUnsupportedDeriving className0)

    eqMethodReference classInfo =
      fst <$> find matchesEqMethod (Map.toList (scopeValues scope))
      where
        matchesEqMethod (_, OverloadedMethod {valueMethodInfo = methodInfo}) =
          methodClassName methodInfo == className classInfo && methodName methodInfo == "eq"
        matchesEqMethod _ = False

    pendingDerivedInstance classInfo instDecl =
      InstanceInfo
        { instanceClassName = P.instanceDeclClass instDecl,
          instanceClassModule = classModule classInfo,
          instanceOriginModule = P.moduleName mod0,
          instanceConstraints = P.instanceDeclConstraints instDecl,
          instanceHeadType = P.instanceDeclType instDecl,
          instanceMethods = Map.empty
        }

    constructorFieldTypes ctor =
      fst (splitArrows (snd (splitForalls (P.constructorDeclType ctor))))

    validateEqDerivingFields :: P.ClassName -> Scope -> P.DataDecl -> TcM ()
    validateEqDerivingFields eqClassName validationScope dataDecl =
      mapM_ (validateEqDerivingField eqClassName validationScope dataDecl) (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))

    validateEqDerivingField :: P.ClassName -> Scope -> P.DataDecl -> SrcType -> TcM ()
    validateEqDerivingField eqClassName validationScope dataDecl fieldTy
      | constraintTypeSatisfiable eqClassName validationScope dataDecl Set.empty eqClassName fieldTy = pure ()
      | otherwise = throwError (ProgramDerivingMissingFieldInstance eqClassName fieldTy)

    constraintTypeSatisfiable derivedClassName validationScope dataDecl seen className0 fieldTy
      | className0 == derivedClassName && fieldCoveredByDerivedConstraints dataDecl fieldTy = True
      | key `Set.member` seen = False
      | otherwise =
          case resolveInstanceInfoWithSubst (scopeToElaborateScope validationScope) className0 fieldTy of
            Right (instanceInfo, subst) ->
              let seen' = Set.insert key seen
               in all
                    (constraintSatisfiable derivedClassName validationScope dataDecl seen' . applyConstraintSubst subst)
                    (instanceConstraints instanceInfo)
            Left _ -> False
      where
        key = (className0, show fieldTy)

    constraintSatisfiable derivedClassName validationScope dataDecl seen constraint =
      constraintTypeSatisfiable
        derivedClassName
        validationScope
        dataDecl
        seen
        (P.constraintClassName constraint)
        (P.constraintType constraint)

    applyConstraintSubst subst constraint =
      constraint
        { P.constraintType =
            Map.foldrWithKey substituteTypeVar (P.constraintType constraint) subst
        }

    fieldCoveredByDerivedConstraints dataDecl fieldTy =
      case fieldTy of
        STVar name -> name `elem` P.dataDeclParams dataDecl
        _ -> isRecursiveOwnerField dataDecl fieldTy

    derivedConstraintParams dataDecl =
      let params = Set.fromList (P.dataDeclParams dataDecl)
          fieldTypes =
            filter
              (not . isRecursiveOwnerField dataDecl)
              (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))
          usedParams = Set.intersection params (foldMap freeTypeVars fieldTypes)
       in [paramName | paramName <- P.dataDeclParams dataDecl, paramName `Set.member` usedParams]

    freeTypeVars ty =
      case ty of
        STVar name -> Set.singleton name
        STArrow dom cod -> freeTypeVars dom `Set.union` freeTypeVars cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap freeTypeVars args
        STForall name mb body ->
          maybe Set.empty (freeTypeVars . unSrcBound) mb
            `Set.union` Set.delete name (freeTypeVars body)
        STMu name body -> Set.delete name (freeTypeVars body)
        STBottom -> Set.empty

    scopeToElaborateScope scope0 =
      mkElaborateScope (scopeValues scope0) (scopeTypes scope0) (scopeClasses scope0) (scopeInstances scope0)

    mkEqInstance classInfo eqMethodName dataDecl =
      let headTy = dataDeclHeadType dataDecl
          eqClassName = className classInfo
          left = P.Param "left" (Just headTy)
          right = P.Param "right" (Just headTy)
          selfName = "__derived_eq_" ++ P.dataDeclName dataDecl
          methodBody =
            if hasRecursiveOwnerFields dataDecl
              then
                P.ELet
                  selfName
                  (Just (STArrow headTy (STArrow headTy (STBase "Bool"))))
                  (P.ELam left (P.ELam right (deriveEqBody eqClassName eqMethodName headTy dataDecl (Just selfName))))
                  (P.EVar selfName)
              else
                P.ELam left (P.ELam right (deriveEqBody eqClassName eqMethodName headTy dataDecl Nothing))
       in P.InstanceDecl
            { P.instanceDeclClass = eqClassName,
              P.instanceDeclConstraints =
                [ P.ClassConstraint eqClassName (STVar paramName)
                  | paramName <- derivedConstraintParams dataDecl
                ],
              P.instanceDeclType = headTy,
              P.instanceDeclMethods = [P.MethodDef "eq" methodBody]
            }

    hasRecursiveOwnerFields dataDecl =
      any (isRecursiveOwnerField dataDecl) (concatMap constructorFieldTypes (P.dataDeclConstructors dataDecl))

    deriveEqBody eqClassName eqMethodName headTy dataDecl mbSelfName =
      P.ECase
        (P.EVar "left")
        [ P.Alt
            (P.PatCtor (P.constructorDeclName ctor) (map P.PatVar leftNames))
            (P.ECase (P.EVar "right") (matchingAlt ctor leftNames : mismatchAlts ctor))
          | ctor <- P.dataDeclConstructors dataDecl,
            let leftNames = ["l" ++ show i | i <- [1 .. length (ctorArgTypes ctor)]]
        ]
      where
        ctorArgTypes ctor = fst (splitArrows (snd (splitForalls (P.constructorDeclType ctor))))

        recursiveEqName = qualify (P.moduleName mod0) (renderInstanceName eqClassName headTy "eq")

        matchingAlt ctor leftNames =
          let rightNames = ["r" ++ show i | i <- [1 .. length leftNames]]
              argTypes = ctorArgTypes ctor
           in P.Alt (P.PatCtor (P.constructorDeclName ctor) (map P.PatVar rightNames)) (foldEqCalls (zip3 argTypes leftNames rightNames))

        mismatchAlts ctor =
          [ P.Alt (P.PatCtor (P.constructorDeclName other) [P.PatWildcard | _ <- fst (splitArrows (snd (splitForalls (P.constructorDeclType other))))]) (P.ELit (LBool False))
            | other <- P.dataDeclConstructors dataDecl,
              P.constructorDeclName other /= P.constructorDeclName ctor
          ]

        foldEqCalls [] = P.ELit (LBool True)
        foldEqCalls [(argTy, l, r)] = eqCall argTy l r
        foldEqCalls ((argTy, l, r) : rest) =
          P.EApp
            (P.EApp (P.EVar "__mlfp_and") (eqCall argTy l r))
            (foldEqCalls rest)

        eqCall argTy l r =
          let eqName =
                case mbSelfName of
                  Just selfName | isRecursiveOwnerField dataDecl argTy -> selfName
                  _ ->
                    if isRecursiveOwnerField dataDecl argTy
                      then recursiveEqName
                      else eqMethodName
           in P.EApp (P.EApp (P.EVar eqName) (P.EVar l)) (P.EVar r)

    isRecursiveOwnerField dataDecl argTy =
      argTy == dataDeclHeadType dataDecl

    dataDeclHeadType dataDecl =
      case P.dataDeclParams dataDecl of
        [] -> STBase (P.dataDeclName dataDecl)
        param0 : paramsRest -> STCon (P.dataDeclName dataDecl) (STVar param0 :| map STVar paramsRest)

buildInstanceSkeletons :: Scope -> P.Module -> [P.InstanceDecl] -> TcM [InstanceInfo]
buildInstanceSkeletons scope mod0 derived = do
  let instances0 = derived ++ explicitInstances mod0
  infos <- mapM toInstanceInfo instances0
  ensureDistinctPlain (\(className0, ty) -> ProgramDuplicateInstance className0 ty) [(instanceClassName info, instanceHeadType info) | info <- infos]
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
      classInfo <- lookupClassInfo scope (P.instanceDeclClass instDecl)
      validateClassConstraintClasses scope (P.instanceDeclConstraints instDecl)
      let methodMap = classMethods classInfo
          expected = Map.keysSet methodMap
          provided = Set.fromList (map P.methodDefName (P.instanceDeclMethods instDecl))
      case Set.toList (expected Set.\\ provided) of
        (missing : _) -> throwError (ProgramMissingInstanceMethod (P.instanceDeclClass instDecl) missing)
        [] -> pure ()
      case Set.toList (provided Set.\\ expected) of
        (extra : _) -> throwError (ProgramUnexpectedInstanceMethod (P.instanceDeclClass instDecl) extra)
        [] -> pure ()
      let instanceMethodInfos =
            Map.fromList
              [ let methodInfo = methodMap Map.! P.methodDefName methodDef
                    rawMethodType = specializeMethodType (methodType methodInfo) (classParamName classInfo) (P.instanceDeclType instDecl)
                    methodValueConstraints =
                      P.instanceDeclConstraints instDecl
                        ++ map
                          (substituteConstraint (classParamName classInfo) (P.instanceDeclType instDecl))
                          (methodConstraints methodInfo)
                 in ( P.methodDefName methodDef,
                      OrdinaryValue
                        { valueDisplayName = P.methodDefName methodDef,
                          valueRuntimeName = qualify (P.moduleName mod0) (renderInstanceName (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl) (P.methodDefName methodDef)),
                          valueType = constrainedVisibleType (P.ConstrainedType methodValueConstraints rawMethodType),
                          valueConstraints = methodValueConstraints,
                          valueOriginModule = P.moduleName mod0
                        }
                    )
                | methodDef <- P.instanceDeclMethods instDecl
              ]
      pure
        InstanceInfo
          { instanceClassName = P.instanceDeclClass instDecl,
            instanceClassModule = classModule classInfo,
            instanceOriginModule = P.moduleName mod0,
            instanceConstraints = P.instanceDeclConstraints instDecl,
            instanceHeadType = P.instanceDeclType instDecl,
            instanceMethods = instanceMethodInfos
          }

    overlappingInstances infos =
      [ (left, right)
        | (ix, left) <- zip [(0 :: Int) ..] infos,
          right <- drop (ix + 1) infos,
          instanceClassName left == instanceClassName right,
          instanceHeadType left /= instanceHeadType right,
          instanceHeadsOverlap (instanceHeadType left) (instanceHeadType right)
      ]

    duplicateExistingInstances infos =
      [ local
        | local <- infos,
          existing <- scopeInstances scope,
          instanceClassName local == instanceClassName existing,
          instanceHeadType local == instanceHeadType existing
      ]

    overlappingWithExistingInstances infos =
      [ (local, existing)
        | local <- infos,
          existing <- scopeInstances scope,
          instanceClassName local == instanceClassName existing,
          instanceHeadType local /= instanceHeadType existing,
          instanceHeadsOverlap (instanceHeadType local) (instanceHeadType existing)
      ]

    instanceHeadsOverlap left right =
      case
        unifyOverlap
          Map.empty
          (tagTypeVars "__overlap_left__" (canonicalInstanceHead left))
          (tagTypeVars "__overlap_right__" (canonicalInstanceHead right))
        of
        Just _ -> True
        Nothing -> False

    canonicalInstanceHead :: SrcType -> SrcType
    canonicalInstanceHead = canonical
      where
        canonical ty =
          case ty of
            STVar {} -> ty
            STArrow dom cod -> STArrow (canonical dom) (canonical cod)
            STBase name -> STBase (canonicalTypeName name)
            STCon name args -> STCon (canonicalTypeName name) (fmap canonical args)
            STForall name mb body ->
              STForall name (fmap (SrcBound . canonical . unSrcBound) mb) (canonical body)
            STMu name body -> STMu name (canonical body)
            STBottom -> STBottom

        canonicalTypeName name =
          case Map.lookup name (scopeTypes scope) of
            Just info -> dataModule info ++ "." ++ dataName info
            Nothing -> name

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
        (STArrow leftDom leftCod, STArrow rightDom rightCod) -> do
          subst' <- unifyOverlap subst leftDom rightDom
          unifyOverlap subst' leftCod rightCod
        _ -> Nothing

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

renderInstanceName :: P.ClassName -> SrcType -> P.MethodName -> String
renderInstanceName className0 headTy methodName0 = className0 ++ "__" ++ sanitizeType headTy ++ "__" ++ methodName0

sanitizeType :: SrcType -> String
sanitizeType = \case
  STVar v -> sanitizeName v
  STArrow dom cod -> "arr_" ++ sanitizeType dom ++ "_" ++ sanitizeType cod
  STBase base -> sanitizeName base
  STCon con args -> intercalate "_" (sanitizeName con : map sanitizeType (NE.toList args))
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

checkInstance :: ElaborateScope -> Scope -> P.InstanceDecl -> TcM [CheckedBinding]
checkInstance elaborateScope scope instDecl = do
  instanceInfo <-
    case findInstance scope (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl) of
      Just info -> pure info
      Nothing -> throwError (ProgramNoMatchingInstance (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl))
  forM (P.instanceDeclMethods instDecl) $ \methodDef -> do
    case instanceMethods instanceInfo Map.! P.methodDefName methodDef of
      valueInfo@OrdinaryValue {} -> do
        let methodRuntimeName = valueRuntimeName valueInfo
            methodSourceType = valueType valueInfo
        liftEither
          ( lowerConstrainedExprBinding elaborateScope methodRuntimeName (valueConstraints valueInfo) methodSourceType False (P.methodDefExpr methodDef)
              >>= finalizeBinding elaborateScope
          )
      _ -> throwError (ProgramUnexpectedInstanceMethod (P.instanceDeclClass instDecl) (P.methodDefName methodDef))
  where
    findInstance scope0 className0 headTy =
      find
        (\info -> instanceClassName info == className0 && instanceHeadType info == headTy)
        (scopeInstances scope0)

checkDef :: ElaborateScope -> Scope -> P.DefDecl -> TcM CheckedBinding
checkDef elaborateScope scope defDecl = do
  valueInfo <- lookupValueInfo scope (P.defDeclName defDecl)
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      liftEither
        ( lowerConstrainedExprBinding elaborateScope (valueRuntimeName ordinary) (valueConstraints ordinary) (valueType ordinary) (P.defDeclName defDecl == "main") (P.defDeclExpr defDecl)
            >>= finalizeBinding elaborateScope
        )
    _ -> throwError (ProgramDuplicateValue (P.defDeclName defDecl))

buildExports :: P.Module -> Map String DataInfo -> Map String ClassInfo -> Map String ValueInfo -> TcM ModuleExports
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
      values <- foldM (collectExportValue localValues localClasses localData) Map.empty items
      types <- foldM (collectExportType localData) Map.empty items
      classes <- foldM (collectExportClass localClasses) Map.empty items
      pure
        ModuleExports
          { exportedValues = values,
            exportedTypes = types,
            exportedClasses = classes
          }

collectExportValue :: Map String ValueInfo -> Map String ClassInfo -> Map String DataInfo -> Map String ValueInfo -> P.ExportItem -> TcM (Map String ValueInfo)
collectExportValue localValues localClasses localData acc = \case
  P.ExportValue name ->
    case Map.lookup name localValues of
      Just info -> pure (Map.insert name info acc)
      Nothing -> throwError (ProgramExportNotLocal name)
  P.ExportTypeWithConstructors typeName ->
    case Map.lookup typeName localData of
      Just dataInfo ->
        let ctorValues =
              Map.fromList
                [ ( ctorName ctor,
                    ConstructorValue (ctorName ctor) (ctorRuntimeName ctor) (ctorType ctor) ctor (dataModule dataInfo)
                  )
                  | ctor <- dataConstructors dataInfo
                ]
         in liftEither (addValues acc ctorValues)
      Nothing -> throwError (ProgramExportNotLocal typeName)
  P.ExportType typeName ->
    case Map.lookup typeName localClasses of
      Just classInfo ->
        let methodValues = Map.fromList [(methodName method, OverloadedMethod (methodName method) method (classModule classInfo)) | method <- Map.elems (classMethods classInfo)]
         in liftEither (addValues acc methodValues)
      Nothing -> pure acc

collectExportType :: Map String DataInfo -> Map String ExportedTypeInfo -> P.ExportItem -> TcM (Map String ExportedTypeInfo)
collectExportType localData acc = \case
  P.ExportType typeName ->
    case Map.lookup typeName localData of
      Just dataInfo -> pure (Map.insert typeName (ExportedTypeInfo dataInfo Map.empty) acc)
      Nothing -> pure acc
  P.ExportTypeWithConstructors typeName ->
    case Map.lookup typeName localData of
      Just dataInfo ->
        pure
          (Map.insert typeName (ExportedTypeInfo dataInfo (Map.fromList [(ctorName ctor, ctor) | ctor <- dataConstructors dataInfo])) acc)
      Nothing -> throwError (ProgramExportNotLocal typeName)
  P.ExportValue _ -> pure acc

collectExportClass :: Map String ClassInfo -> Map String ClassInfo -> P.ExportItem -> TcM (Map String ClassInfo)
collectExportClass localClasses acc = \case
  P.ExportType typeName ->
    case Map.lookup typeName localClasses of
      Just classInfo -> pure (Map.insert typeName classInfo acc)
      Nothing -> pure acc
  _ -> pure acc

-- Helpers --------------------------------------------------------------------

qualify :: P.ModuleName -> String -> String
qualify moduleName0 name = moduleName0 ++ "__" ++ name

ensureDistinctBy :: (Eq a) => (a -> ProgramError) -> (b -> a) -> [b] -> TcM ()
ensureDistinctBy mkErr project values = ensureDistinctPlain mkErr (map project values)

ensureDistinctImportAliases :: [P.Import] -> TcM ()
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
