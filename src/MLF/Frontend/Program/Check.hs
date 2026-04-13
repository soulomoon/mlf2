{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module MLF.Frontend.Program.Check
    ( ProgramError (..)
    , CheckedProgram (..)
    , CheckedModule (..)
    , ResolvedBinding (..)
    , ResolvedExpr (..)
    , ResolvedAlt (..)
    , DataInfo (..)
    , ConstructorInfo (..)
    , ClassInfo (..)
    , MethodInfo (..)
    , InstanceInfo (..)
    , ValueInfo (..)
    , ExportedTypeInfo (..)
    , ModuleExports (..)
    , checkProgram
    ) where

import Control.Monad (foldM, forM, unless, when, zipWithM_)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, evalState, get, gets, modify, put)
import Data.List (find, intercalate, nub)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified MLF.Frontend.Program.Syntax as P
import MLF.Frontend.Syntax (Lit (..), SrcBound (..), SrcTy (..), SrcType)

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
    | ProgramDuplicateInstance P.ClassName SrcType
    | ProgramUnknownValue String
    | ProgramUnknownConstructor String
    | ProgramUnknownType String
    | ProgramUnknownClass String
    | ProgramUnknownMethod String
    | ProgramUnsupportedDeriving P.ClassName
    | ProgramDerivingRequiresNullaryType P.TypeName
    | ProgramMissingInstanceMethod P.ClassName P.MethodName
    | ProgramUnexpectedInstanceMethod P.ClassName P.MethodName
    | ProgramNoMatchingInstance P.ClassName SrcType
    | ProgramAmbiguousMethodUse P.MethodName
    | ProgramCannotInferLambda P.ValueName
    | ProgramExpectedFunction SrcType
    | ProgramTypeMismatch SrcType SrcType
    | ProgramCaseOnNonDataType SrcType
    | ProgramNonExhaustiveCase [P.ConstructorName]
    | ProgramDuplicateCaseBranch P.ConstructorName
    | ProgramPatternConstructorMismatch P.ConstructorName SrcType
    | ProgramMainNotFound
    | ProgramMultipleMainDefinitions [String]
    deriving (Eq, Show)

data Scheme = Scheme
    { schemeBinders :: [String]
    , schemeBody :: SrcType
    }
    deriving (Eq, Show)

data ConstructorInfo = ConstructorInfo
    { ctorName :: P.ConstructorName
    , ctorRuntimeName :: String
    , ctorType :: SrcType
    , ctorForalls :: [(String, Maybe SrcType)]
    , ctorArgs :: [SrcType]
    , ctorResult :: SrcType
    , ctorOwningType :: P.TypeName
    , ctorIndex :: Int
    }
    deriving (Eq, Show)

data DataInfo = DataInfo
    { dataName :: P.TypeName
    , dataModule :: P.ModuleName
    , dataParams :: [String]
    , dataConstructors :: [ConstructorInfo]
    }
    deriving (Eq, Show)

data MethodInfo = MethodInfo
    { methodClassName :: P.ClassName
    , methodName :: P.MethodName
    , methodRuntimeBase :: String
    , methodScheme :: Scheme
    , methodParamName :: String
    }
    deriving (Eq, Show)

data ClassInfo = ClassInfo
    { className :: P.ClassName
    , classModule :: P.ModuleName
    , classParamName :: String
    , classMethods :: Map P.MethodName MethodInfo
    }
    deriving (Eq, Show)

data ValueInfo
    = OrdinaryValue
        { valueDisplayName :: String
        , valueRuntimeName :: String
        , valueScheme :: Scheme
        , valueOriginModule :: P.ModuleName
        }
    | ConstructorValue
        { valueDisplayName :: String
        , valueRuntimeName :: String
        , valueScheme :: Scheme
        , valueCtorInfo :: ConstructorInfo
        , valueOriginModule :: P.ModuleName
        }
    | OverloadedMethod
        { valueDisplayName :: String
        , valueMethodInfo :: MethodInfo
        , valueOriginModule :: P.ModuleName
        }
    deriving (Eq, Show)

data InstanceInfo = InstanceInfo
    { instanceClassName :: P.ClassName
    , instanceHeadType :: SrcType
    , instanceMethods :: Map P.MethodName ValueInfo
    }
    deriving (Eq, Show)

data ExportedTypeInfo = ExportedTypeInfo
    { exportedTypeData :: DataInfo
    , exportedTypeConstructors :: Map String ConstructorInfo
    }
    deriving (Eq, Show)

data ModuleExports = ModuleExports
    { exportedValues :: Map String ValueInfo
    , exportedTypes :: Map String ExportedTypeInfo
    , exportedClasses :: Map String ClassInfo
    }
    deriving (Eq, Show)

data ResolvedExpr
    = RVar String
    | RLit Lit
    | RLam String ResolvedExpr
    | RApp ResolvedExpr ResolvedExpr
    | RLet String ResolvedExpr ResolvedExpr
    | RCase ResolvedExpr [ResolvedAlt]
    deriving (Eq, Show)

data ResolvedAlt
    = RAltCtor ConstructorInfo [String] ResolvedExpr
    | RAltVar String ResolvedExpr
    | RAltWildcard ResolvedExpr
    deriving (Eq, Show)

data ResolvedBinding = ResolvedBinding
    { resolvedBindingName :: String
    , resolvedBindingType :: SrcType
    , resolvedBindingExpr :: ResolvedExpr
    , resolvedBindingExportedAsMain :: Bool
    }
    deriving (Eq, Show)

data CheckedModule = CheckedModule
    { checkedModuleName :: P.ModuleName
    , checkedModuleBindings :: [ResolvedBinding]
    , checkedModuleData :: Map String DataInfo
    , checkedModuleClasses :: Map String ClassInfo
    , checkedModuleInstances :: [InstanceInfo]
    , checkedModuleExports :: ModuleExports
    }
    deriving (Eq, Show)

data CheckedProgram = CheckedProgram
    { checkedProgramModules :: [CheckedModule]
    , checkedProgramMain :: String
    }
    deriving (Eq, Show)

-- Type checker state ----------------------------------------------------------

data TcState = TcState
    { tcNextMeta :: Int
    , tcSubst :: Map String SrcType
    , tcNextLocal :: Int
    }
    deriving (Eq, Show)

type TcM a = ExceptT ProgramError (State TcState) a

runTcM :: TcM a -> Either ProgramError a
runTcM action = evalState (runExceptT action) (TcState 0 Map.empty 0)

freshMeta :: TcM SrcType
freshMeta = do
    n <- gets tcNextMeta
    modify (\st -> st { tcNextMeta = n + 1 })
    pure (STVar ("?t" ++ show n))

freshLocalName :: String -> TcM String
freshLocalName base = do
    n <- gets tcNextLocal
    modify (\st -> st { tcNextLocal = n + 1 })
    pure ("$" ++ base ++ "#" ++ show n)

isMetaVar :: String -> Bool
isMetaVar ('?':'t':_) = True
isMetaVar _ = False

applySubst :: Map String SrcType -> SrcType -> SrcType
applySubst substMap = go
  where
    go ty = case ty of
        STVar v -> maybe (STVar v) go (Map.lookup v substMap)
        STArrow dom cod -> STArrow (go dom) (go cod)
        STBase base -> STBase base
        STCon con args -> STCon con (fmap go args)
        STForall v mb body ->
            let subst' = Map.delete v substMap
            in STForall v (fmap (SrcBound . applySubst subst' . unSrcBound) mb) (applySubst subst' body)
        STMu v body -> STMu v (applySubst (Map.delete v substMap) body)
        STBottom -> STBottom

zonkType :: SrcType -> TcM SrcType
zonkType ty = gets tcSubst >>= \substMap -> pure (applySubst substMap ty)

bindMeta :: String -> SrcType -> TcM ()
bindMeta meta ty
    | ty == STVar meta = pure ()
    | occursMeta meta ty = throwError (ProgramTypeMismatch (STVar meta) ty)
    | otherwise = modify (\st -> st { tcSubst = Map.insert meta ty (tcSubst st) })

occursMeta :: String -> SrcType -> Bool
occursMeta needle = \case
    STVar v -> v == needle
    STArrow dom cod -> occursMeta needle dom || occursMeta needle cod
    STBase _ -> False
    STCon _ args -> any (occursMeta needle) (NE.toList args)
    STForall v mb body ->
        v /= needle
            && maybe False (occursMeta needle . unSrcBound) mb
            || (v /= needle && occursMeta needle body)
    STMu v body -> v /= needle && occursMeta needle body
    STBottom -> False

renameTypeVar :: String -> String -> SrcType -> SrcType
renameTypeVar from to = go
  where
    go ty = case ty of
        STVar v | v == from -> STVar to
        STVar v -> STVar v
        STArrow dom cod -> STArrow (go dom) (go cod)
        STBase base -> STBase base
        STCon con args -> STCon con (fmap go args)
        STForall v mb body
            | v == from -> STForall v mb body
            | otherwise -> STForall v (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu v body
            | v == from -> STMu v body
            | otherwise -> STMu v (go body)
        STBottom -> STBottom

unifyTypes :: SrcType -> SrcType -> TcM ()
unifyTypes left0 right0 = do
    left <- zonkType left0
    right <- zonkType right0
    case (left, right) of
        (STVar v, _) | isMetaVar v -> bindMeta v right
        (_, STVar v) | isMetaVar v -> bindMeta v left
        (STVar a, STVar b)
            | a == b -> pure ()
            | otherwise -> throwError (ProgramTypeMismatch left right)
        (STArrow domA codA, STArrow domB codB) -> unifyTypes domA domB >> unifyTypes codA codB
        (STBase a, STBase b)
            | a == b -> pure ()
            | otherwise -> throwError (ProgramTypeMismatch left right)
        (STCon conA argsA, STCon conB argsB)
            | conA == conB && length argsA == length argsB -> zipWithM_ unifyTypes (NE.toList argsA) (NE.toList argsB)
            | otherwise -> throwError (ProgramTypeMismatch left right)
        (STForall vA mbA bodyA, STForall vB mbB bodyB) -> do
            when (isJust mbA /= isJust mbB) (throwError (ProgramTypeMismatch left right))
            case (mbA, mbB) of
                (Just boundA, Just boundB) -> unifyTypes (unSrcBound boundA) (renameTypeVar vB vA (unSrcBound boundB))
                _ -> pure ()
            unifyTypes bodyA (renameTypeVar vB vA bodyB)
        (STMu vA bodyA, STMu vB bodyB) -> unifyTypes bodyA (renameTypeVar vB vA bodyB)
        (STBottom, STBottom) -> pure ()
        _ -> throwError (ProgramTypeMismatch left right)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

freeTypeVars :: SrcType -> Set String
freeTypeVars = \case
    STVar v -> Set.singleton v
    STArrow dom cod -> Set.union (freeTypeVars dom) (freeTypeVars cod)
    STBase _ -> Set.empty
    STCon _ args -> Set.unions (map freeTypeVars (NE.toList args))
    STForall v mb body -> Set.delete v (Set.union (maybe Set.empty (freeTypeVars . unSrcBound) mb) (freeTypeVars body))
    STMu v body -> Set.delete v (freeTypeVars body)
    STBottom -> Set.empty

splitForalls :: SrcType -> ([(String, Maybe SrcType)], SrcType)
splitForalls = go []
  where
    go acc = \case
        STForall v mb body -> go (acc ++ [(v, fmap unSrcBound mb)]) body
        ty -> (acc, ty)

splitArrows :: SrcType -> ([SrcType], SrcType)
splitArrows = go []
  where
    go acc = \case
        STArrow dom cod -> go (acc ++ [dom]) cod
        ty -> (acc, ty)

makeScheme :: SrcType -> Scheme
makeScheme ty =
    let (explicit, body) = splitForalls ty
        explicitBinders = map fst explicit
        binders = explicitBinders ++ Set.toList (freeTypeVars body Set.\\ Set.fromList explicitBinders)
    in Scheme binders body

instantiateScheme :: Scheme -> TcM SrcType
instantiateScheme scheme = snd <$> instantiateSchemeWithMap scheme

instantiateSchemeWithMap :: Scheme -> TcM (Map String SrcType, SrcType)
instantiateSchemeWithMap (Scheme binders body) = do
    freshened <- mapM (const freshMeta) binders
    let substMap = Map.fromList (zip binders freshened)
    pure (substMap, applySubst substMap body)

withIsolatedSubst :: TcM a -> TcM a
withIsolatedSubst action = do
    st0 <- get
    result <- action
    st1 <- get
    put st1 { tcSubst = tcSubst st0 }
    pure result

-- Scope ----------------------------------------------------------------------

data Scope = Scope
    { scopeValues :: Map String ValueInfo
    , scopeTypes :: Map String DataInfo
    , scopeClasses :: Map String ClassInfo
    , scopeInstances :: [InstanceInfo]
    }
    deriving (Eq, Show)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty []

addValues :: Map String ValueInfo -> Map String ValueInfo -> Either ProgramError (Map String ValueInfo)
addValues base incoming =
    foldM
        (\acc (name, info) ->
            if Map.member name acc
                then Left (ProgramDuplicateVisibleName name)
                else Right (Map.insert name info acc)
        )
        base
        (Map.toList incoming)

addTypes :: Map String DataInfo -> Map String DataInfo -> Either ProgramError (Map String DataInfo)
addTypes base incoming =
    foldM
        (\acc (name, info) ->
            if Map.member name acc
                then Left (ProgramDuplicateVisibleName name)
                else Right (Map.insert name info acc)
        )
        base
        (Map.toList incoming)

addClasses :: Map String ClassInfo -> Map String ClassInfo -> Either ProgramError (Map String ClassInfo)
addClasses base incoming =
    foldM
        (\acc (name, info) ->
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
            [ resolvedBindingName binding
            | checked <- modulesChecked
            , binding <- checkedModuleBindings checked
            , resolvedBindingExportedAsMain binding
            ]
    mainRuntime <-
        case mainNames of
            [] -> throwError ProgramMainNotFound
            [name] -> pure name
            names -> throwError (ProgramMultipleMainDefinitions names)
    pure CheckedProgram
        { checkedProgramModules = modulesChecked
        , checkedProgramMain = mainRuntime
        }

checkModules :: P.Program -> TcM [CheckedModule]
checkModules (P.Program modules0) = do
    ensureDistinctBy ProgramDuplicateModule P.moduleName modules0
    orderedModules <- topoSortModules modules0
    go [] orderedModules
  where
    go acc [] = pure (reverse acc)
    go acc (mod0:rest) = do
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
                ( Set.delete moduleName0 tempMarks'
                , Set.insert moduleName0 permMarks'
                , mod0 : ordered'
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
    let localMethodValues = Map.fromList
            [ (methodName method, OverloadedMethod (methodName method) method (P.moduleName mod0))
            | classInfo <- Map.elems localClasses
            , method <- Map.elems (classMethods classInfo)
            ]
    localValues <- mergeMaps ProgramDuplicateValue localValues1 localMethodValues
    valueScope <- liftEither =<< pure (addValues (scopeValues importScope) localValues)
    typeScope <- liftEither =<< pure (addTypes (scopeTypes importScope) localData)
    classScope <- liftEither =<< pure (addClasses (scopeClasses importScope) localClasses)
    let scope0 = Scope valueScope typeScope classScope (scopeInstances importScope ++ priorInstances)
    derivedInstances <- synthesizeDerivedInstances scope0 mod0
    instanceSkeletons <- buildInstanceSkeletons scope0 mod0 derivedInstances
    let scope1 = scope0 { scopeInstances = scopeInstances scope0 ++ instanceSkeletons }
    instanceBindings <- concat <$> mapM (checkInstance scope1) (derivedInstances ++ explicitInstances mod0)
    defBindings <- mapM (checkDef scope1) (moduleDefDecls mod0)
    exports <- buildExports mod0 localData localClasses localValues
    let exportedMainRuntime =
            case Map.lookup "main" (exportedValues exports) of
                Just OrdinaryValue { valueRuntimeName = runtimeName } -> Just runtimeName
                _ -> Nothing
        markExportedMain binding =
            binding
                { resolvedBindingExportedAsMain =
                    Just (resolvedBindingName binding) == exportedMainRuntime
                }
    pure CheckedModule
        { checkedModuleName = P.moduleName mod0
        , checkedModuleBindings = instanceBindings ++ map markExportedMain defBindings
        , checkedModuleData = localData
        , checkedModuleClasses = localClasses
        , checkedModuleInstances = instanceSkeletons
        , checkedModuleExports = exports
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
                            { scopeValues = values
                            , scopeTypes = types
                            , scopeClasses = classes
                            }
            Just items -> foldM (applyImportItem (P.importModuleName imp) exports) scope items

applyImportItem :: P.ModuleName -> ModuleExports -> Scope -> P.ExportItem -> TcM Scope
applyImportItem moduleName0 exports scope item =
    case item of
        P.ExportValue name ->
            case Map.lookup name (exportedValues exports) of
                Just info -> do
                    values <- liftEither (addValues (scopeValues scope) (Map.singleton name info))
                    pure scope { scopeValues = values }
                Nothing -> throwError (ProgramImportNotExported moduleName0 name)
        P.ExportType typeName ->
            case Map.lookup typeName (exportedTypes exports) of
                Just typeInfo -> do
                    let dataInfo = exportedTypeData typeInfo
                    types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
                    let scope' = scope { scopeTypes = types }
                    case Map.lookup typeName (exportedClasses exports) of
                        Just classInfo -> do
                            classes <- liftEither (addClasses (scopeClasses scope') (Map.singleton typeName classInfo))
                            pure scope' { scopeClasses = classes }
                        Nothing -> pure scope'
                Nothing ->
                    case Map.lookup typeName (exportedClasses exports) of
                        Just classInfo -> do
                            classes <- liftEither (addClasses (scopeClasses scope) (Map.singleton typeName classInfo))
                            pure scope { scopeClasses = classes }
                        Nothing -> throwError (ProgramImportNotExported moduleName0 typeName)
        P.ExportTypeWithConstructors typeName ->
            case Map.lookup typeName (exportedTypes exports) of
                Just typeInfo -> do
                    when (Map.null (exportedTypeConstructors typeInfo)) $
                        throwError (ProgramImportNotExported moduleName0 typeName)
                    let dataInfo = exportedTypeData typeInfo
                        ctorValues =
                            Map.fromList
                                [ ( ctorName ctor
                                  , ConstructorValue (ctorName ctor) (ctorRuntimeName ctor) (makeScheme (ctorType ctor)) ctor (dataModule dataInfo)
                                  )
                                | ctor <- Map.elems (exportedTypeConstructors typeInfo)
                                ]
                    values <- liftEither (addValues (scopeValues scope) ctorValues)
                    types <- liftEither (addTypes (scopeTypes scope) (Map.singleton typeName dataInfo))
                    pure scope
                        { scopeValues = values
                        , scopeTypes = types
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
        let constructors = zipWith (toCtorInfo dataDecl) [0 ..] (P.dataDeclConstructors dataDecl)
        pure
            ( P.dataDeclName dataDecl
            , DataInfo
                { dataName = P.dataDeclName dataDecl
                , dataModule = P.moduleName mod0
                , dataParams = P.dataDeclParams dataDecl
                , dataConstructors = constructors
                }
            )

    toCtorInfo dataDecl index ctorDecl =
        let (foralls, ctorBody) = splitForalls (P.constructorDeclType ctorDecl)
            (args0, result0) = splitArrows ctorBody
        in ConstructorInfo
            { ctorName = P.constructorDeclName ctorDecl
            , ctorRuntimeName = qualify (P.moduleName mod0) (P.constructorDeclName ctorDecl)
            , ctorType = P.constructorDeclType ctorDecl
            , ctorForalls = foralls
            , ctorArgs = args0
            , ctorResult = result0
            , ctorOwningType = P.dataDeclName dataDecl
            , ctorIndex = index
            }

buildLocalClassInfo :: P.Module -> TcM (Map String ClassInfo)
buildLocalClassInfo mod0 = do
    let classDecls = moduleClassDecls mod0
    ensureDistinctBy ProgramDuplicateClass P.classDeclName classDecls
    pure . Map.fromList =<< mapM toClassInfo classDecls
  where
    toClassInfo classDecl = do
        ensureDistinctBy ProgramDuplicateMethod P.methodSigName (P.classDeclMethods classDecl)
        let methods = Map.fromList
                [ ( P.methodSigName sig
                  , MethodInfo
                        { methodClassName = P.classDeclName classDecl
                        , methodName = P.methodSigName sig
                        , methodRuntimeBase = qualify (P.moduleName mod0) (P.classDeclName classDecl ++ "__" ++ P.methodSigName sig)
                        , methodScheme = makeScheme (P.methodSigType sig)
                        , methodParamName = P.classDeclParam classDecl
                        }
                  )
                | sig <- P.classDeclMethods classDecl
                ]
        pure
            ( P.classDeclName classDecl
            , ClassInfo
                { className = P.classDeclName classDecl
                , classModule = P.moduleName mod0
                , classParamName = P.classDeclParam classDecl
                , classMethods = methods
                }
            )

buildLocalDefInfo :: P.Module -> TcM (Map String ValueInfo)
buildLocalDefInfo mod0 = do
    let defs = moduleDefDecls mod0
    ensureDistinctBy ProgramDuplicateValue P.defDeclName defs
    pure $ Map.fromList
        [ ( P.defDeclName defDecl
          , OrdinaryValue
                { valueDisplayName = P.defDeclName defDecl
                , valueRuntimeName = qualify (P.moduleName mod0) (P.defDeclName defDecl)
                , valueScheme = makeScheme (P.defDeclType defDecl)
                , valueOriginModule = P.moduleName mod0
                }
          )
        | defDecl <- defs
        ]

addConstructorValues :: P.ModuleName -> Map String DataInfo -> TcM (Map String ValueInfo)
addConstructorValues moduleName0 dataInfos =
    pure $ Map.fromList
        [ ( ctorName ctor
          , ConstructorValue
                { valueDisplayName = ctorName ctor
                , valueRuntimeName = ctorRuntimeName ctor
                , valueScheme = makeScheme (ctorType ctor)
                , valueCtorInfo = ctor
                , valueOriginModule = moduleName0
                }
          )
        | dataInfo <- Map.elems dataInfos
        , ctor <- dataConstructors dataInfo
        ]

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
                (param0:paramsRest) -> STCon (P.dataDeclName dataDecl) (STVar param0 :| map STVar paramsRest)
            headTy = case P.dataDeclParams dataDecl of
                [] -> STBase (P.dataDeclName dataDecl)
                _ -> actualHead
            left = P.Param "left" (Just headTy)
            right = P.Param "right" (Just headTy)
            body = deriveEqBody dataDecl
        in P.InstanceDecl
            { P.instanceDeclClass = "Eq"
            , P.instanceDeclType = headTy
            , P.instanceDeclMethods = [P.MethodDef "eq" (P.ELam left (P.ELam right body))]
            }

    deriveEqBody dataDecl =
        P.ECase (P.EVar "left")
            [ P.Alt
                (P.PatCtor (P.constructorDeclName ctor) leftNames)
                (P.ECase (P.EVar "right") (matchingAlt ctor leftNames : mismatchAlts ctor))
            | ctor <- P.dataDeclConstructors dataDecl
            , let leftNames = ["l" ++ show i | i <- [1 .. length (ctorArgTypes ctor)]]
            ]
      where
        ctorArgTypes ctor = fst (splitArrows (snd (splitForalls (P.constructorDeclType ctor))))

        matchingAlt ctor leftNames =
            let rightNames = ["r" ++ show i | i <- [1 .. length leftNames]]
            in P.Alt (P.PatCtor (P.constructorDeclName ctor) rightNames) (foldEqCalls leftNames rightNames)

        mismatchAlts ctor =
            [ P.Alt (P.PatCtor (P.constructorDeclName other) ["_" | _ <- fst (splitArrows (snd (splitForalls (P.constructorDeclType other))))]) (P.ELit (LBool False))
            | other <- P.dataDeclConstructors dataDecl
            , P.constructorDeclName other /= P.constructorDeclName ctor
            ]

        foldEqCalls [] [] = P.ELit (LBool True)
        foldEqCalls [l] [r] = P.EApp (P.EApp (P.EVar "eq") (P.EVar l)) (P.EVar r)
        foldEqCalls (l:_) (r:_) =
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
            (missing:_) -> throwError (ProgramMissingInstanceMethod (P.instanceDeclClass instDecl) missing)
            [] -> pure ()
        case Set.toList (provided Set.\\ expected) of
            (extra:_) -> throwError (ProgramUnexpectedInstanceMethod (P.instanceDeclClass instDecl) extra)
            [] -> pure ()
        let instanceMethodInfos = Map.fromList
                [ ( P.methodDefName methodDef
                  , OrdinaryValue
                        { valueDisplayName = P.methodDefName methodDef
                        , valueRuntimeName = qualify (P.moduleName mod0) (renderInstanceName (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl) (P.methodDefName methodDef))
                        , valueScheme = specializeMethodScheme (methodScheme (methodMap Map.! P.methodDefName methodDef)) (classParamName classInfo) (P.instanceDeclType instDecl)
                        , valueOriginModule = P.moduleName mod0
                        }
                  )
                | methodDef <- P.instanceDeclMethods instDecl
                ]
        pure InstanceInfo
            { instanceClassName = P.instanceDeclClass instDecl
            , instanceHeadType = P.instanceDeclType instDecl
            , instanceMethods = instanceMethodInfos
            }

specializeMethodScheme :: Scheme -> String -> SrcType -> Scheme
specializeMethodScheme scheme paramName headTy =
    let binders = filter (/= paramName) (schemeBinders scheme)
    in Scheme binders (substTypeVar paramName headTy (schemeBody scheme))

substTypeVar :: String -> SrcType -> SrcType -> SrcType
substTypeVar needle replacement = go
  where
    go ty = case ty of
        STVar v | v == needle -> replacement
        STVar v -> STVar v
        STArrow dom cod -> STArrow (go dom) (go cod)
        STBase base -> STBase base
        STCon con args -> STCon con (fmap go args)
        STForall v mb body
            | v == needle -> STForall v mb body
            | otherwise -> STForall v (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu v body
            | v == needle -> STMu v body
            | otherwise -> STMu v (go body)
        STBottom -> STBottom

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

checkInstance :: Scope -> P.InstanceDecl -> TcM [ResolvedBinding]
checkInstance scope instDecl = do
    instanceInfo <-
        case findInstance scope (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl) of
            Just info -> pure info
            Nothing -> throwError (ProgramNoMatchingInstance (P.instanceDeclClass instDecl) (P.instanceDeclType instDecl))
    forM (P.instanceDeclMethods instDecl) $ \methodDef -> do
        case instanceMethods instanceInfo Map.! P.methodDefName methodDef of
            valueInfo@OrdinaryValue {} -> do
                expectedTy <- instantiateScheme (valueScheme valueInfo)
                resolvedExpr <- withNamedLocals scope [] $ \localScope -> checkExpr localScope expectedTy (P.methodDefExpr methodDef)
                pure ResolvedBinding
                    { resolvedBindingName = valueRuntimeName valueInfo
                    , resolvedBindingType = schemeBody (valueScheme valueInfo)
                    , resolvedBindingExpr = resolvedExpr
                    , resolvedBindingExportedAsMain = False
                    }
            _ -> throwError (ProgramUnexpectedInstanceMethod (P.instanceDeclClass instDecl) (P.methodDefName methodDef))
  where
    findInstance scope0 className0 headTy =
        find
            (\info -> instanceClassName info == className0 && instanceHeadType info == headTy)
            (scopeInstances scope0)

checkDef :: Scope -> P.DefDecl -> TcM ResolvedBinding
checkDef scope defDecl = do
    valueInfo <- lookupValueInfo scope (P.defDeclName defDecl)
    case valueInfo of
        ordinary@OrdinaryValue {} -> do
            expectedTy <- instantiateScheme (valueScheme ordinary)
            resolvedExpr <- withNamedLocals scope [] $ \localScope -> checkExpr localScope expectedTy (P.defDeclExpr defDecl)
            pure ResolvedBinding
                { resolvedBindingName = valueRuntimeName ordinary
                , resolvedBindingType = schemeBody (valueScheme ordinary)
                , resolvedBindingExpr = resolvedExpr
                , resolvedBindingExportedAsMain = P.defDeclName defDecl == "main"
                }
        _ -> throwError (ProgramDuplicateValue (P.defDeclName defDecl))

buildExports :: P.Module -> Map String DataInfo -> Map String ClassInfo -> Map String ValueInfo -> TcM ModuleExports
buildExports mod0 localData localClasses localValues = do
    let exportItems = P.moduleExports mod0
        defaultValues = Map.filter (\info -> case info of OverloadedMethod {} -> True; ConstructorValue {} -> True; OrdinaryValue {} -> True) localValues
        defaultTypes = Map.fromList [(name, ExportedTypeInfo info Map.empty) | (name, info) <- Map.toList localData]
        defaultClasses = localClasses
    case exportItems of
        Nothing ->
            pure ModuleExports
                { exportedValues = defaultValues
                , exportedTypes = defaultTypes
                , exportedClasses = defaultClasses
                }
        Just items -> do
            values <- foldM (collectExportValue localValues localClasses localData) Map.empty items
            types <- foldM (collectExportType localData) Map.empty items
            classes <- foldM (collectExportClass localClasses) Map.empty items
            pure ModuleExports
                { exportedValues = values
                , exportedTypes = types
                , exportedClasses = classes
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
                            [ ( ctorName ctor
                              , ConstructorValue (ctorName ctor) (ctorRuntimeName ctor) (makeScheme (ctorType ctor)) ctor (dataModule dataInfo)
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

-- Expression checking ---------------------------------------------------------

data ExprScope = ExprScope
    { exprValues :: Map String ValueInfo
    , exprTypes :: Map String DataInfo
    , exprClasses :: Map String ClassInfo
    , exprInstances :: [InstanceInfo]
    }
    deriving (Eq, Show)

withNamedLocals :: Scope -> [(String, ValueInfo)] -> (ExprScope -> TcM a) -> TcM a
withNamedLocals scope locals k =
    k
        ExprScope
            { exprValues = Map.union (Map.fromList locals) (scopeValues scope)
            , exprTypes = scopeTypes scope
            , exprClasses = scopeClasses scope
            , exprInstances = scopeInstances scope
            }

extendLocal :: ExprScope -> String -> String -> SrcType -> ExprScope
extendLocal scope srcName runtimeName ty =
    scope { exprValues = Map.insert srcName (OrdinaryValue srcName runtimeName (Scheme [] ty) "<local>") (exprValues scope) }

checkExpr :: ExprScope -> SrcType -> P.Expr -> TcM ResolvedExpr
checkExpr scope expected expr = case expr of
    P.ELam param body -> do
        expected' <- zonkType expected
        case expected' of
            STArrow dom cod -> do
                case P.paramType param of
                    Just ann -> unifyTypes ann dom
                    Nothing -> pure ()
                runtimeName <- freshLocalName (P.paramName param)
                let scope' = extendLocal scope (P.paramName param) runtimeName dom
                resolvedBody <- checkExpr scope' cod body
                pure (RLam runtimeName resolvedBody)
            _ -> throwError (ProgramExpectedFunction expected')
    P.ECase scrutinee alts -> do
        (resolvedScrutinee, scrutTy) <- inferExpr scope scrutinee
        dataInfo <- requireDataResultType scope scrutTy
        resolvedAlts <- mapM (checkAlt scope expected scrutTy) alts
        ensureExhaustive (dataConstructors dataInfo) resolvedAlts
        pure (RCase resolvedScrutinee resolvedAlts)
    P.EAnn inner annTy -> do
        unifyTypes annTy expected
        checkExpr scope annTy inner
    _ -> do
        (resolvedExpr, inferredTy) <- inferExpr scope expr
        unifyTypes inferredTy expected
        pure resolvedExpr

inferExpr :: ExprScope -> P.Expr -> TcM (ResolvedExpr, SrcType)
inferExpr scope expr = case expr of
    P.EVar name -> do
        valueInfo <-
            case Map.lookup name (exprValues scope) of
                Just info -> pure info
                Nothing -> throwError (ProgramUnknownValue name)
        case valueInfo of
            OverloadedMethod {} -> throwError (ProgramAmbiguousMethodUse name)
            _ -> do
                ty <- instantiateScheme (valueScheme valueInfo)
                pure (RVar (valueRuntimeName valueInfo), ty)
    P.ELit lit -> pure (RLit lit, litType lit)
    P.ELam param _ ->
        case P.paramType param of
            Nothing -> throwError (ProgramCannotInferLambda (P.paramName param))
            Just annTy -> do
                runtimeName <- freshLocalName (P.paramName param)
                bodyTy <- freshMeta
                let scope' = extendLocal scope (P.paramName param) runtimeName annTy
                let P.ELam _ lamBody = expr
                resolvedBody <- checkExpr scope' bodyTy lamBody
                zonkedBody <- zonkType bodyTy
                pure (RLam runtimeName resolvedBody, STArrow annTy zonkedBody)
    P.EApp _ _ -> inferApp scope expr
    P.ELet name mbTy rhs body -> do
        bindingTy <- maybe freshMeta pure mbTy
        runtimeName <- freshLocalName name
        let scope' = extendLocal scope name runtimeName bindingTy
        resolvedRhs <- checkExpr scope' bindingTy rhs
        (resolvedBody, bodyTy) <- inferExpr scope' body
        pure (RLet runtimeName resolvedRhs resolvedBody, bodyTy)
    P.EAnn inner annTy -> do
        resolved <- checkExpr scope annTy inner
        pure (resolved, annTy)
    P.ECase scrutinee alts -> do
        resultTy <- freshMeta
        resolved <- checkExpr scope resultTy (P.ECase scrutinee alts)
        zonked <- zonkType resultTy
        pure (resolved, zonked)

inferApp :: ExprScope -> P.Expr -> TcM (ResolvedExpr, SrcType)
inferApp scope expr = do
    let (headExpr, args) = collectApps expr
    case headExpr of
        P.EVar name
            | Just (OverloadedMethod _ methodInfo _) <- Map.lookup name (exprValues scope) -> inferMethodApp scope methodInfo args
        _ -> do
            (resolvedHead, headTy0) <- inferExpr scope headExpr
            foldM step (resolvedHead, headTy0) args
  where
    step (resolvedFun, funTy) arg = do
        funTy' <- zonkType funTy
        case funTy' of
            STArrow dom cod -> do
                resolvedArg <- checkExpr scope dom arg
                pure (RApp resolvedFun resolvedArg, cod)
            _ -> throwError (ProgramExpectedFunction funTy')

inferMethodApp :: ExprScope -> MethodInfo -> [P.Expr] -> TcM (ResolvedExpr, SrcType)
inferMethodApp scope methodInfo args = do
    (methodSubst, methodTy0) <- instantiateSchemeWithMap (methodScheme methodInfo)
    (resolvedArgs, resultTy) <- foldM step ([], methodTy0) args
    classArgTy <-
        case Map.lookup (methodParamName methodInfo) methodSubst of
            Just ty -> pure ty
            Nothing -> throwError (ProgramUnknownMethod (methodName methodInfo))
    classArgTy' <- zonkType classArgTy
    instanceInfo <- resolveInstance (exprInstances scope) (methodClassName methodInfo) classArgTy'
    case instanceMethods instanceInfo Map.! methodName methodInfo of
        methodValue@OrdinaryValue {} ->
            pure (foldl RApp (RVar (valueRuntimeName methodValue)) resolvedArgs, resultTy)
        _ -> throwError (ProgramUnknownMethod (methodName methodInfo))
  where
    step (resolvedAcc, currentTy) arg = do
        currentTy' <- zonkType currentTy
        case currentTy' of
            STArrow dom cod -> do
                resolvedArg <- checkExpr scope dom arg
                pure (resolvedAcc ++ [resolvedArg], cod)
            _ -> throwError (ProgramExpectedFunction currentTy')

resolveInstance :: [InstanceInfo] -> P.ClassName -> SrcType -> TcM InstanceInfo
resolveInstance infos className0 headTy =
    case filter matches infos of
        [info] -> pure info
        [] -> throwError (ProgramNoMatchingInstance className0 headTy)
        _ -> throwError (ProgramNoMatchingInstance className0 headTy)
  where
    matches info = instanceClassName info == className0 && instanceHeadType info == headTy

collectApps :: P.Expr -> (P.Expr, [P.Expr])
collectApps = go []
  where
    go acc (P.EApp fun arg) = go (arg : acc) fun
    go acc headExpr = (headExpr, acc)

checkAlt :: ExprScope -> SrcType -> SrcType -> P.Alt -> TcM ResolvedAlt
checkAlt scope expected scrutTy alt =
    case P.altPattern alt of
        P.PatCtor ctorName0 binders -> do
            ctorInfo <- lookupCtor scope ctorName0
            resolved <- withIsolatedSubst $ do
                ctorTy <- instantiateScheme (makeScheme (ctorType ctorInfo))
                let (args0, result0) = splitArrows ctorTy
                unifyTypes result0 scrutTy
                when (length args0 /= length binders) (throwError (ProgramPatternConstructorMismatch ctorName0 scrutTy))
                runtimeNames <- mapM freshLocalName binders
                let argTypes = args0
                    scope' = foldl' (\sc (srcName, runtimeName, ty) -> extendLocal sc srcName runtimeName ty) scope (zip3 binders runtimeNames argTypes)
                resolvedBody <- checkExpr scope' expected (P.altExpr alt)
                pure (RAltCtor ctorInfo runtimeNames resolvedBody)
            pure resolved
        P.PatVar name -> do
            runtimeName <- freshLocalName name
            let scope' = extendLocal scope name runtimeName scrutTy
            resolvedBody <- checkExpr scope' expected (P.altExpr alt)
            pure (RAltVar runtimeName resolvedBody)
        P.PatWildcard -> RAltWildcard <$> checkExpr scope expected (P.altExpr alt)

lookupCtor :: ExprScope -> String -> TcM ConstructorInfo
lookupCtor scope name =
    case Map.lookup name (exprValues scope) of
        Just ConstructorValue { valueCtorInfo = ctorInfo } -> pure ctorInfo
        _ -> throwError (ProgramUnknownConstructor name)

requireDataResultType :: ExprScope -> SrcType -> TcM DataInfo
requireDataResultType scope ty = do
    ty' <- zonkType ty
    case ty' of
        STCon name _ -> lookupDataInfoScope scope name
        STBase name -> lookupDataInfoScope scope name
        _ -> throwError (ProgramCaseOnNonDataType ty')

lookupDataInfoScope :: ExprScope -> String -> TcM DataInfo
lookupDataInfoScope scope name =
    case Map.lookup name (exprTypes scope) of
        Just info -> pure info
        Nothing -> throwError (ProgramUnknownType name)

ensureExhaustive :: [ConstructorInfo] -> [ResolvedAlt] -> TcM ()
ensureExhaustive constructors0 alts =
    let seen = [ctorName ctor | RAltCtor ctor _ _ <- alts]
        dupes = duplicates seen
    in case dupes of
        (dup:_) -> throwError (ProgramDuplicateCaseBranch dup)
        [] ->
            if any isCatchAll alts
                then pure ()
                else do
                    let missing = [ctorName ctor | ctor <- constructors0, ctorName ctor `notElem` seen]
                    unless (null missing) (throwError (ProgramNonExhaustiveCase missing))
  where
    isCatchAll (RAltVar _ _) = True
    isCatchAll (RAltWildcard _) = True
    isCatchAll _ = False

litType :: Lit -> SrcType
litType lit = case lit of
    LInt _ -> STBase "Int"
    LBool _ -> STBase "Bool"
    LString _ -> STBase "String"

-- Helpers --------------------------------------------------------------------

qualify :: P.ModuleName -> String -> String
qualify moduleName0 name = moduleName0 ++ "__" ++ name

ensureDistinctBy :: Eq a => (a -> ProgramError) -> (b -> a) -> [b] -> TcM ()
ensureDistinctBy mkErr project values = ensureDistinctPlain mkErr (map project values)

ensureDistinctPlain :: Eq a => (a -> ProgramError) -> [a] -> TcM ()
ensureDistinctPlain mkErr values =
    case duplicates values of
        (dup:_) -> throwError (mkErr dup)
        [] -> pure ()

duplicates :: Eq a => [a] -> [a]
duplicates values = [x | x <- nub values, length (filter (== x) values) > 1]

mergeMaps :: (String -> ProgramError) -> Map String a -> Map String a -> TcM (Map String a)
mergeMaps mkErr base incoming =
    foldM
        (\acc (name, value) ->
            if Map.member name acc
                then throwError (mkErr name)
                else pure (Map.insert name value acc)
        )
        base
        (Map.toList incoming)

liftEither :: Either ProgramError a -> TcM a
liftEither = either throwError pure
