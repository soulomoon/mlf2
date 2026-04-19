{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Check
  ( ProgramError (..),
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
  )
where

import Control.Monad (foldM, forM, when, zipWithM)
import Control.Monad.Except (MonadError (throwError))
import Data.List (find, intercalate, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    lowerConstructorBinding,
    lowerExprBinding,
    mkElaborateScope,
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
    ProgramError (..),
    ValueInfo (..),
    specializeMethodType,
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

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty []

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
  let scope0 = Scope valueScope typeScope classScope (scopeInstances importScope ++ priorInstances)
  derivedInstances <- synthesizeDerivedInstances scope0 mod0
  instanceSkeletons <- buildInstanceSkeletons scope0 mod0 derivedInstances
  let scope1 = scope0 {scopeInstances = scopeInstances scope0 ++ instanceSkeletons}
  let elaborateScope = mkElaborateScope (scopeValues scope1) (scopeTypes scope1) (scopeInstances scope1)
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
      case P.importExposing imp of
        Nothing ->
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
        Just items -> foldM (applyImportItem (P.importModuleName imp) exports) scope items

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
                      methodType = P.methodSigType sig,
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
              valueType = P.defDeclType defDecl,
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
synthesizeDerivedInstances scope mod0 = concat <$> mapM deriveForData (moduleDataDecls mod0)
  where
    deriveForData dataDecl = do
      forM (P.dataDeclDeriving dataDecl) $ \className0 ->
        case className0 of
          "Eq" -> do
            when (not (null (P.dataDeclParams dataDecl))) (throwError (ProgramDerivingRequiresNullaryType (P.dataDeclName dataDecl)))
            _ <- lookupClassInfo scope className0
            pure (mkEqInstance dataDecl)
          other -> throwError (ProgramUnsupportedDeriving other)

    mkEqInstance dataDecl =
      let actualHead = case P.dataDeclParams dataDecl of
            [] -> STBase (P.dataDeclName dataDecl)
            (param0 : paramsRest) -> STCon (P.dataDeclName dataDecl) (STVar param0 :| map STVar paramsRest)
          headTy = case P.dataDeclParams dataDecl of
            [] -> STBase (P.dataDeclName dataDecl)
            _ -> actualHead
          left = P.Param "left" (Just headTy)
          right = P.Param "right" (Just headTy)
          body = deriveEqBody dataDecl
       in P.InstanceDecl
            { P.instanceDeclClass = "Eq",
              P.instanceDeclType = headTy,
              P.instanceDeclMethods = [P.MethodDef "eq" (P.ELam left (P.ELam right body))]
            }

    deriveEqBody dataDecl =
      P.ECase
        (P.EVar "left")
        [ P.Alt
            (P.PatCtor (P.constructorDeclName ctor) leftNames)
            (P.ECase (P.EVar "right") (matchingAlt ctor leftNames : mismatchAlts ctor))
          | ctor <- P.dataDeclConstructors dataDecl,
            let leftNames = ["l" ++ show i | i <- [1 .. length (ctorArgTypes ctor)]]
        ]
      where
        ctorArgTypes ctor = fst (splitArrows (snd (splitForalls (P.constructorDeclType ctor))))

        matchingAlt ctor leftNames =
          let rightNames = ["r" ++ show i | i <- [1 .. length leftNames]]
           in P.Alt (P.PatCtor (P.constructorDeclName ctor) rightNames) (foldEqCalls leftNames rightNames)

        mismatchAlts ctor =
          [ P.Alt (P.PatCtor (P.constructorDeclName other) ["_" | _ <- fst (splitArrows (snd (splitForalls (P.constructorDeclType other))))]) (P.ELit (LBool False))
            | other <- P.dataDeclConstructors dataDecl,
              P.constructorDeclName other /= P.constructorDeclName ctor
          ]

        foldEqCalls [] [] = P.ELit (LBool True)
        foldEqCalls [l] [r] = P.EApp (P.EApp (P.EVar "eq") (P.EVar l)) (P.EVar r)
        foldEqCalls (l : _) (r : _) =
          -- The initial deriving target is nullary recursive ADTs like Nat,
          -- so multiple-field conjunction is intentionally deferred.
          P.EApp (P.EApp (P.EVar "eq") (P.EVar l)) (P.EVar r)
        foldEqCalls _ _ = P.ELit (LBool False)

buildInstanceSkeletons :: Scope -> P.Module -> [P.InstanceDecl] -> TcM [InstanceInfo]
buildInstanceSkeletons scope mod0 derived = do
  let instances0 = derived ++ explicitInstances mod0
  infos <- mapM toInstanceInfo instances0
  ensureDistinctPlain (\(className0, ty) -> ProgramDuplicateInstance className0 ty) [(instanceClassName info, instanceHeadType info) | info <- infos]
  pure infos
  where
    toInstanceInfo instDecl = do
      classInfo <- lookupClassInfo scope (P.instanceDeclClass instDecl)
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
              [ ( P.methodDefName methodDef,
                  OrdinaryValue
                    { valueDisplayName = P.methodDefName methodDef,
                      valueRuntimeName = qualify (P.moduleName mod0) (renderInstanceName (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl) (P.methodDefName methodDef)),
                      valueType = specializeMethodType (methodType (methodMap Map.! P.methodDefName methodDef)) (classParamName classInfo) (P.instanceDeclType instDecl),
                      valueOriginModule = P.moduleName mod0
                    }
                )
                | methodDef <- P.instanceDeclMethods instDecl
              ]
      pure
        InstanceInfo
          { instanceClassName = P.instanceDeclClass instDecl,
            instanceHeadType = P.instanceDeclType instDecl,
            instanceMethods = instanceMethodInfos
          }

renderInstanceName :: P.ClassName -> SrcType -> P.MethodName -> String
renderInstanceName className0 headTy methodName0 = className0 ++ "__" ++ sanitizeType headTy ++ "__" ++ methodName0

sanitizeType :: SrcType -> String
sanitizeType = \case
  STVar v -> map sanitizeChar v
  STArrow dom cod -> "arr_" ++ sanitizeType dom ++ "_" ++ sanitizeType cod
  STBase base -> map sanitizeChar base
  STCon con args -> intercalate "_" (map sanitizeChar con : map sanitizeType (NE.toList args))
  STForall v _ body -> "forall_" ++ map sanitizeChar v ++ "_" ++ sanitizeType body
  STMu v body -> "mu_" ++ map sanitizeChar v ++ "_" ++ sanitizeType body
  STBottom -> "bottom"
  where
    sanitizeChar c
      | c `elem` ['a' .. 'z'] = c
      | c `elem` ['A' .. 'Z'] = c
      | c `elem` ['0' .. '9'] = c
      | otherwise = '_'

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
          ( lowerExprBinding elaborateScope methodRuntimeName methodSourceType False (P.methodDefExpr methodDef)
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
        ( lowerExprBinding elaborateScope (valueRuntimeName ordinary) (valueType ordinary) (P.defDeclName defDecl == "main") (P.defDeclExpr defDecl)
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
