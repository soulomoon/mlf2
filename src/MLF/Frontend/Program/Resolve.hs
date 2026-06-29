{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module MLF.Frontend.Program.Resolve
  ( resolveProgram,
  )
where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.State.Strict (StateT (..), runStateT, state)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import MLF.Frontend.Program.Builtins
  ( builtinTypeSymbol,
    builtinValueSymbol,
  )
import MLF.Frontend.Normalize (freeVarsSrcType)
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax
  ( ResolvedSrcType,
    ResolvedSrcTy (..),
    ResolvedTypeBinderRef,
    SrcBound (..),
    SrcTy (..),
    SrcType,
    mkResolvedSrcBound,
    resolvedTypeBinderRefFromIdentity,
  )
import qualified MLF.Frontend.Syntax.Program as P
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (IdentityGenerator, freshIdentity, freshLocalRef, initialIdentityGenerator, typeBinderIdentityFromUnique)

type ResolveM a = Either ProgramError a

type LocalResolveM a = StateT IdentityGenerator (Either ProgramError) a

type PriorExports = Map P.ModuleName (SymbolIdentity, ResolvedScope)

type LocalEnv = Map String LocalRef

type TypeBinderEnv = Map String ResolvedTypeBinderRef

runLocalResolveM :: IdentityGenerator -> LocalResolveM a -> ResolveM (a, IdentityGenerator)
runLocalResolveM generator action =
  runStateT action generator

freshLocalResolveRef :: String -> LocalResolveM LocalRef
freshLocalResolveRef name =
  state (freshLocalRef name)

freshResolvedTypeBinderRef :: String -> LocalResolveM ResolvedTypeBinderRef
freshResolvedTypeBinderRef name =
  state $ \generator ->
    let (identity, generator') = freshIdentity generator
     in ( resolvedTypeBinderRefFromIdentity (typeBinderIdentityFromUnique identity) name,
          generator'
        )

freshSymbolIdentity :: SymbolNamespace -> P.ModuleName -> String -> Maybe SymbolOwnerIdentity -> LocalResolveM SymbolIdentity
freshSymbolIdentity namespace moduleName0 name owner =
  state $ \generator ->
    let (identity, generator') = freshIdentity generator
     in ( symbolIdentityFromParts identity namespace moduleName0 name owner,
          generator'
        )

liftResolve :: ResolveM a -> LocalResolveM a
liftResolve action =
  StateT $ \generator ->
    case action of
      Left err -> Left err
      Right value -> Right (value, generator)

data CandidateScope = CandidateScope
  { candidateValues :: Map String [ResolvedSymbol],
    candidateTypes :: Map String [ResolvedSymbol],
    candidateClasses :: Map String [ResolvedSymbol],
    candidateModules :: Map P.ModuleName [ResolvedSymbol]
  }
  deriving (Eq, Show)

data LocalSymbols = LocalSymbols
  { localValues :: Map String [ResolvedSymbol],
    localTypes :: Map String [ResolvedSymbol],
    localClasses :: Map String [ResolvedSymbol]
  }
  deriving (Eq, Show)

emptyCandidateScope :: CandidateScope
emptyCandidateScope =
  CandidateScope
    { candidateValues = Map.empty,
      candidateTypes = Map.empty,
      candidateClasses = Map.empty,
      candidateModules = Map.empty
    }

emptyLocalSymbols :: LocalSymbols
emptyLocalSymbols =
  LocalSymbols
    { localValues = Map.empty,
      localTypes = Map.empty,
      localClasses = Map.empty
    }

resolveProgram :: P.Program -> Either ProgramError ResolvedProgram
resolveProgram (P.Program modules0) = do
  ensureDistinctBy ProgramDuplicateModule P.moduleName modules0
  orderedModules <- topoSortModules modules0
  (_, resolvedModulesRev, _) <- foldM resolveModule (Map.empty, [], initialIdentityGenerator) orderedModules
  pure (ResolvedProgram (reverse resolvedModulesRev))

topoSortModules :: [P.Module] -> ResolveM [P.Module]
topoSortModules modules0 = do
  (_, _, orderedRev) <- foldM visit (Set.empty, Set.empty, []) (map P.moduleName modules0)
  pure (reverse orderedRev)
  where
    moduleMap = Map.fromList [(P.moduleName mod0, mod0) | mod0 <- modules0]

    visit (tempMarks, permMarks, ordered) moduleName0
      | moduleName0 `Set.member` permMarks = pure (tempMarks, permMarks, ordered)
      | moduleName0 `Set.member` tempMarks = Left (ProgramImportCycle [moduleName0])
      | otherwise = do
          mod0 <-
            case Map.lookup moduleName0 moduleMap of
              Just found -> pure found
              Nothing -> Left (ProgramUnknownImportModule moduleName0)
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

resolveModule ::
  (PriorExports, [ResolvedModule], IdentityGenerator) ->
  P.Module ->
  ResolveM (PriorExports, [ResolvedModule], IdentityGenerator)
resolveModule (priorExports, resolvedRev, generator0) mod0 = do
  ensureDistinctImportAliases (P.moduleImports mod0)
  importScope <- buildImportScope priorExports (P.moduleImports mod0)
  let (moduleIdentity, generator1) = freshModuleIdentity (P.moduleName mod0) generator0
  (locals, generator2) <- runLocalResolveM generator1 (buildLocalSymbols mod0)
  let fullCandidates = addLocalSymbols importScope locals
  ((resolvedSyntax, references), generator3) <-
    runLocalResolveM generator2 (resolveModuleSyntax priorExports locals fullCandidates mod0)
  fullScope <- resolvedModuleScopeFromCandidates (P.moduleName mod0) fullCandidates
  exports <- buildExports mod0 locals
  let resolved =
        ResolvedModule
          { resolvedModuleSemantic =
              ResolvedSemanticModule
                { resolvedSemanticModuleName = P.moduleName mod0,
                  resolvedSemanticModuleIdentity = moduleIdentity,
                  resolvedSemanticModuleSyntax = resolvedSyntax,
                  resolvedSemanticModuleLocalSymbols =
                    ResolvedLocalSymbols
                      { resolvedLocalValues = localValues locals,
                        resolvedLocalTypes = localTypes locals,
                        resolvedLocalClasses = localClasses locals
                      },
                  resolvedSemanticModuleScope = fullScope,
                  resolvedSemanticModuleExports = exports
                },
            resolvedModuleDiagnosticAdapter =
              ResolvedModuleDiagnosticAdapter
                { resolvedDiagnosticReferences = references
                }
          }
  pure (Map.insert (P.moduleName mod0) (moduleIdentity, exports) priorExports, resolved : resolvedRev, generator3)

freshModuleIdentity :: P.ModuleName -> IdentityGenerator -> (SymbolIdentity, IdentityGenerator)
freshModuleIdentity moduleName0 generator =
  let (identity, generator') = freshIdentity generator
   in ( symbolIdentityFromParts identity SymbolModule moduleName0 moduleName0 Nothing,
        generator'
      )

buildImportScope :: PriorExports -> [P.Import] -> ResolveM CandidateScope
buildImportScope priorExports =
  foldM addImport (addBuiltinSymbols emptyCandidateScope)
  where
    addImport scope imp = do
      (moduleIdentity, exports) <-
        case Map.lookup (P.importModuleName imp) priorExports of
          Just found -> pure found
          Nothing -> Left (ProgramUnknownImportModule (P.importModuleName imp))
      let moduleName0 = P.importModuleName imp
      case P.importAlias imp of
        Nothing ->
          case P.importExposing imp of
            Nothing -> pure (addAllExports (SymbolUnqualifiedImport moduleName0) id exports scope)
            Just items -> foldM (applyImportItem moduleName0 exports) scope items
        Just alias -> do
          let qualifiedScope =
                addCandidateModule
                  alias
                  (resolvedModuleSymbolFromIdentity (SymbolQualifiedImport moduleName0 alias) moduleIdentity alias)
                  (addAllExports (SymbolQualifiedImport moduleName0 alias) (qualifyName alias) exports scope)
          case P.importExposing imp of
            Nothing -> pure qualifiedScope
            Just items -> foldM (applyImportItem moduleName0 exports) qualifiedScope items

addBuiltinSymbols :: CandidateScope -> CandidateScope
addBuiltinSymbols scope =
  foldr
    (\name acc -> addCandidateValue name (builtinValueSymbol name) acc)
    ( foldr
        (\name acc -> addCandidateType name (builtinTypeSymbol name) acc)
        scope
        (Set.toList PrimitiveInventory.builtinTypeNames)
    )
    (Set.toList PrimitiveInventory.primitiveValueNames)

addAllExports ::
  SymbolOrigin ->
  (String -> String) ->
  ResolvedScope ->
  CandidateScope ->
  CandidateScope
addAllExports origin rename exports scope =
  foldl'
    (\acc (name, symbol) -> addCandidateValue (rename name) (respell origin name (rename name) symbol) acc)
    ( foldl'
        (\acc (name, symbol) -> addCandidateType (rename name) (respell origin name (rename name) symbol) acc)
        ( foldl'
            (\acc (name, symbol) -> addCandidateClass (rename name) (respell origin name (rename name) symbol) acc)
            scope
            (Map.toList (resolvedScopeClasses exports))
        )
        (Map.toList (resolvedScopeTypes exports))
    )
    (Map.toList (resolvedScopeValues exports))

applyImportItem :: P.ModuleName -> ResolvedScope -> CandidateScope -> P.ExportItem -> ResolveM CandidateScope
applyImportItem moduleName0 exports scope = \case
  P.ExportValue name ->
    case Map.lookup name (resolvedScopeValues exports) of
      Just symbol -> pure (addCandidateValue name (respell (SymbolUnqualifiedImport moduleName0) name name symbol) scope)
      Nothing -> Left (ProgramImportNotExported moduleName0 name)
  P.ExportType typeName ->
    case (Map.lookup typeName (resolvedScopeTypes exports), Map.lookup typeName (resolvedScopeClasses exports)) of
      (Just typeSymbol, Just classSymbol) ->
        pure
          ( addCandidateClass typeName (respell (SymbolUnqualifiedImport moduleName0) typeName typeName classSymbol)
              (addCandidateType typeName (respell (SymbolUnqualifiedImport moduleName0) typeName typeName typeSymbol) scope)
          )
      (Just typeSymbol, Nothing) ->
        pure (addCandidateType typeName (respell (SymbolUnqualifiedImport moduleName0) typeName typeName typeSymbol) scope)
      (Nothing, Just classSymbol) ->
        pure (addCandidateClass typeName (respell (SymbolUnqualifiedImport moduleName0) typeName typeName classSymbol) scope)
      (Nothing, Nothing) ->
        case Map.lookup typeName (resolvedScopeValues exports) of
          Just symbol -> pure (addCandidateValue typeName (respell (SymbolUnqualifiedImport moduleName0) typeName typeName symbol) scope)
          Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
  P.ExportTypeWithConstructors typeName ->
    case Map.lookup typeName (resolvedScopeTypes exports) of
      Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
      Just typeSymbol -> do
        let constructorSymbols =
              [ (name, symbol)
                | (name, symbol) <- Map.toList (resolvedScopeValues exports),
                  symbolNamespace (resolvedSymbolIdentity symbol) == SymbolConstructor,
                  symbolOwnerIdentity (resolvedSymbolIdentity symbol) == Just (SymbolOwnerType (resolvedSymbolIdentity typeSymbol))
              ]
        when (null constructorSymbols) $
          Left (ProgramImportNotExported moduleName0 typeName)
        pure $
          foldl'
            ( \acc (name, symbol) ->
                addCandidateValue name (respell (SymbolUnqualifiedImport moduleName0) name name symbol) acc
            )
            (addCandidateType typeName (respell (SymbolUnqualifiedImport moduleName0) typeName typeName typeSymbol) scope)
            constructorSymbols

buildLocalSymbols :: P.Module -> LocalResolveM LocalSymbols
buildLocalSymbols mod0 = do
  liftResolve (ensureDistinctBy ProgramDuplicateType P.dataDeclName dataDecls)
  liftResolve (ensureDistinctPlain ProgramDuplicateConstructor (concatMap (map P.constructorDeclName . P.dataDeclConstructors) dataDecls))
  liftResolve (ensureDistinctBy ProgramDuplicateClass P.classDeclName classDecls)
  liftResolve (ensureDistinctBy ProgramDuplicateValue P.defDeclName defDecls)
  forM_ classDecls $ \classDecl ->
    liftResolve (ensureDistinctBy ProgramDuplicateMethod P.methodSigName (P.classDeclMethods classDecl))
  dataEntries <- forM dataDecls $ \decl -> do
    dataIdentity <- freshSymbolIdentity SymbolType modName (P.dataDeclName decl) Nothing
    constructorEntries <-
      forM (P.dataDeclConstructors decl) $ \ctor -> do
        ctorIdentity <-
          freshSymbolIdentity
            SymbolConstructor
            modName
            (P.constructorDeclName ctor)
            (Just (SymbolOwnerType dataIdentity))
        pure (P.constructorDeclName ctor, [constructorDeclSymbol modName ctorIdentity ctor])
    pure ((P.dataDeclName decl, [dataDeclSymbol modName dataIdentity decl]), constructorEntries)
  classEntries <- forM classDecls $ \decl -> do
    classIdentity <- freshSymbolIdentity SymbolClass modName (P.classDeclName decl) Nothing
    methodEntries <-
      forM (P.classDeclMethods decl) $ \method -> do
        methodIdentity <-
          freshSymbolIdentity
            SymbolMethod
            modName
            (P.methodSigName method)
            (Just (SymbolOwnerClass classIdentity))
        pure (P.methodSigName method, [methodSigSymbol modName methodIdentity method])
    pure ((P.classDeclName decl, [classDeclSymbol modName classIdentity decl]), methodEntries)
  defEntries <- forM defDecls $ \decl -> do
    valueIdentity <- freshSymbolIdentity SymbolValue modName (P.defDeclName decl) Nothing
    pure (P.defDeclName decl, [defDeclSymbol modName valueIdentity decl])
  let dataTypes = Map.fromListWith (++) (map fst dataEntries)
      constructors = Map.fromListWith (++) (concatMap snd dataEntries)
      classes = Map.fromListWith (++) (map fst classEntries)
      methods = Map.fromListWith (++) (concatMap snd classEntries)
      defs = Map.fromListWith (++) defEntries
  pure
    emptyLocalSymbols
      { localValues = constructors `mergeCandidateMaps` defs `mergeCandidateMaps` methods,
        localTypes = dataTypes,
        localClasses = classes
      }
  where
    modName = P.moduleName mod0
    dataDecls = moduleDataDecls mod0
    classDecls = moduleClassDecls mod0
    defDecls = moduleDefDecls mod0

buildExports :: P.Module -> LocalSymbols -> ResolveM ResolvedScope
buildExports mod0 locals =
  case P.moduleExports mod0 of
    Nothing ->
      resolvedScopeFromCandidates ProgramDuplicateVisibleName
        ( CandidateScope
            { candidateValues = localValues locals,
              candidateTypes = localTypes locals,
              candidateClasses = localClasses locals,
              candidateModules = Map.empty
            }
        )
    Just items -> do
      candidates <- foldM collectExport emptyCandidateScope items
      resolvedScopeFromCandidates ProgramDuplicateVisibleName candidates
  where
    collectExport acc = \case
      P.ExportValue name ->
        case builtinPreludeExportType name of
          Just symbol -> pure acc {candidateTypes = Map.insertWith (++) name [symbol] (candidateTypes acc)}
          Nothing ->
            case Map.lookup name (localValues locals) of
              Just symbols -> pure acc {candidateValues = Map.insertWith (++) name symbols (candidateValues acc)}
              Nothing -> Left (ProgramExportNotLocal name)
      P.ExportType typeName ->
        case (Map.lookup typeName (localTypes locals), Map.lookup typeName (localClasses locals)) of
          (Nothing, Nothing) ->
            case builtinPreludeExportType typeName of
              Just symbol -> pure acc {candidateTypes = Map.insertWith (++) typeName [symbol] (candidateTypes acc)}
              Nothing ->
                case Map.lookup typeName (localValues locals) of
                  Just symbols -> pure acc {candidateValues = Map.insertWith (++) typeName symbols (candidateValues acc)}
                  Nothing -> Left (ProgramExportNotLocal typeName)
          (mbTypes, mbClasses) ->
            pure
              acc
                { candidateTypes = maybe (candidateTypes acc) (\symbols -> Map.insertWith (++) typeName symbols (candidateTypes acc)) mbTypes,
                  candidateClasses = maybe (candidateClasses acc) (\symbols -> Map.insertWith (++) typeName symbols (candidateClasses acc)) mbClasses,
                  candidateValues =
                    maybe
                      (candidateValues acc)
                      (\_ -> exportClassMethods typeName (candidateValues acc))
                      mbClasses
                }
      P.ExportTypeWithConstructors typeName ->
        case Map.lookup typeName (localTypes locals) of
          Nothing -> Left (ProgramExportNotLocal typeName)
          Just typeSymbols ->
            pure
              acc
                { candidateTypes = Map.insertWith (++) typeName typeSymbols (candidateTypes acc),
                  candidateValues = exportConstructors typeName (candidateValues acc)
                }

    exportClassMethods className0 values0 =
      Map.unionWith
        (++)
        values0
        (Map.mapMaybe (onlyMethodsOf className0) (localValues locals))

    onlyMethodsOf className0 symbols =
      case filter (isMethodOf className0) symbols of
        [] -> Nothing
        matchingSymbols -> Just matchingSymbols

    exportConstructors typeName values0 =
      Map.unionWith
        (++)
        values0
        (Map.filter (any (isConstructorOf typeName)) (localValues locals))

    isMethodOf className0 symbol =
      case Map.lookup className0 (localClasses locals) of
        Just classSymbols ->
          symbolOwnerIdentity (resolvedSymbolIdentity symbol)
            `elem` [Just (SymbolOwnerClass (resolvedSymbolIdentity classSymbol)) | classSymbol <- classSymbols]
        Nothing -> False

    isConstructorOf typeName symbol =
      case Map.lookup typeName (localTypes locals) of
        Just typeSymbols ->
          symbolOwnerIdentity (resolvedSymbolIdentity symbol)
            `elem` [Just (SymbolOwnerType (resolvedSymbolIdentity typeSymbol)) | typeSymbol <- typeSymbols]
        Nothing -> False

    builtinPreludeExportType typeName
      | P.moduleName mod0 == "Prelude",
        typeName == "IO" =
          Just (builtinTypeSymbol typeName)
      | otherwise = Nothing

resolveModuleSyntax ::
  PriorExports ->
  LocalSymbols ->
  CandidateScope ->
  P.Module ->
  LocalResolveM (P.ResolvedModuleSyntax, [ResolvedReference])
resolveModuleSyntax priorExports locals scope mod0 = do
  imports0 <- liftResolve (mapM (resolveImport priorExports) (P.moduleImports mod0))
  exports0 <- liftResolve (mapM (mapM (resolveExportItem (P.moduleName mod0) locals)) (P.moduleExports mod0))
  (decls0, refs) <- mapAndRefsLocal resolveDecl (P.moduleDecls mod0)
  pure
    ( P.Module
        { P.moduleName = P.moduleName mod0,
          P.moduleExports = exports0,
          P.moduleImports = imports0,
          P.moduleDecls = decls0
        },
      refs
    )
  where
    resolveDecl = \case
      P.DeclData decl -> firstWithRefs P.DeclData <$> resolveDataDecl locals scope decl
      P.DeclClass decl -> firstWithRefs P.DeclClass <$> resolveClassDecl locals scope decl
      P.DeclInstance decl -> firstWithRefs P.DeclInstance <$> resolveInstanceDecl scope decl
      P.DeclTypeFamily decl -> pure (P.DeclTypeFamily decl, [])
      P.DeclDef decl -> firstWithRefs P.DeclDef <$> resolveDefDecl locals scope decl

resolveImport :: PriorExports -> P.Import -> ResolveM P.ResolvedImport
resolveImport priorExports imp = do
  let moduleName0 = P.importModuleName imp
  case Map.lookup moduleName0 priorExports of
    Nothing -> Left (ProgramUnknownImportModule moduleName0)
    Just (moduleIdentity, exports) -> do
      exposing0 <- mapM (mapM (resolveImportItem moduleName0 exports)) (P.importExposing imp)
      pure
        P.Import
          { P.importModuleName = resolvedModuleSymbolFromIdentity (SymbolUnqualifiedImport moduleName0) moduleIdentity moduleName0,
            P.importAlias = P.importAlias imp,
            P.importExposing = exposing0
          }

resolveImportItem :: P.ModuleName -> ResolvedScope -> P.ExportItem -> ResolveM P.ResolvedExportItem
resolveImportItem moduleName0 exports item =
  case item of
    P.ExportValue name ->
      case Map.lookup name (resolvedScopeValues exports) of
        Just symbol -> pure (P.ExportValue (respell (SymbolUnqualifiedImport moduleName0) name name symbol))
        Nothing -> Left (ProgramImportNotExported moduleName0 name)
    P.ExportType typeName ->
      case resolvedExportTypeRef typeName exports of
        Just ref -> pure (P.ExportType ref)
        Nothing ->
          case Map.lookup typeName (resolvedScopeValues exports) of
            Just symbol -> pure (P.ExportValue (respell (SymbolUnqualifiedImport moduleName0) typeName typeName symbol))
            Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
    P.ExportTypeWithConstructors typeName ->
      case Map.lookup typeName (resolvedScopeTypes exports) of
        Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
        Just _ ->
          case resolvedExportTypeRef typeName exports of
            Just ref -> pure (P.ExportTypeWithConstructors ref)
            Nothing -> Left (ProgramImportNotExported moduleName0 typeName)

resolveExportItem :: P.ModuleName -> LocalSymbols -> P.ExportItem -> ResolveM P.ResolvedExportItem
resolveExportItem moduleName0 locals item =
  case item of
    P.ExportValue name ->
      case builtinPreludeExportType name of
        Just ref -> pure (P.ExportType ref)
        Nothing -> P.ExportValue <$> uniqueLocalSymbol ProgramExportNotLocal name (localValues locals)
    P.ExportType typeName ->
      case resolvedLocalExportTypeRef locals typeName of
        Just ref -> pure (P.ExportType ref)
        Nothing
          | Just ref <- builtinPreludeExportType typeName ->
              pure (P.ExportType ref)
        Nothing -> P.ExportValue <$> uniqueLocalSymbol ProgramExportNotLocal typeName (localValues locals)
    P.ExportTypeWithConstructors typeName ->
      case Map.lookup typeName (localTypes locals) of
        Nothing -> Left (ProgramExportNotLocal typeName)
        Just _ ->
          case resolvedLocalExportTypeRef locals typeName of
            Just ref -> pure (P.ExportTypeWithConstructors ref)
            Nothing -> Left (ProgramExportNotLocal typeName)
  where
    builtinPreludeExportType typeName
      | moduleName0 == "Prelude",
        typeName == "IO" =
          Just (P.resolvedExportTypeRefFromSymbols typeName [builtinTypeSymbol typeName])
      | otherwise = Nothing

resolvedLocalExportTypeRef :: LocalSymbols -> String -> Maybe P.ResolvedExportTypeRef
resolvedLocalExportTypeRef locals name =
  let symbols =
        Map.findWithDefault [] name (localTypes locals)
          ++ Map.findWithDefault [] name (localClasses locals)
   in case distinctByIdentity symbols of
        [] -> Nothing
        distinct -> Just (P.resolvedExportTypeRefFromSymbols name distinct)

resolvedExportTypeRef :: String -> ResolvedScope -> Maybe P.ResolvedExportTypeRef
resolvedExportTypeRef name exports =
  let symbols =
        maybe [] (: []) (Map.lookup name (resolvedScopeTypes exports))
          ++ maybe [] (: []) (Map.lookup name (resolvedScopeClasses exports))
   in case distinctByIdentity symbols of
        [] -> Nothing
        distinct -> Just (P.resolvedExportTypeRefFromSymbols name distinct)

uniqueLocalSymbol :: (String -> ProgramError) -> String -> Map String [ResolvedSymbol] -> ResolveM ResolvedSymbol
uniqueLocalSymbol err name symbolsByName =
  case Map.lookup name symbolsByName of
    Just symbols ->
      case distinctByIdentity symbols of
        [symbol] -> pure symbol
        _ -> Left (err name)
    Nothing -> Left (err name)

uniqueMatchingLocalSymbol :: (String -> ProgramError) -> String -> (ResolvedSymbol -> Bool) -> Map String [ResolvedSymbol] -> ResolveM ResolvedSymbol
uniqueMatchingLocalSymbol err name predicate symbolsByName =
  case Map.lookup name symbolsByName of
    Just symbols ->
      case distinctByIdentity (filter predicate symbols) of
        [symbol] -> pure symbol
        _ -> Left (err name)
    Nothing -> Left (err name)

resolveDataDecl :: LocalSymbols -> CandidateScope -> P.DataDecl -> LocalResolveM (P.ResolvedDataDecl, [ResolvedReference])
resolveDataDecl locals scope decl = do
  dataSymbol <- liftResolve (uniqueLocalSymbol ProgramUnknownType (P.dataDeclName decl) (localTypes locals))
  (params, typeBinders) <- freshResolvedTypeParams (P.dataDeclParams decl)
  (ctors, ctorRefs) <- mapAndRefsLocal (resolveConstructorDecl locals dataSymbol scope typeBinders) (P.dataDeclConstructors decl)
  derivingRefs <- liftResolve (mapM (resolveClassRef scope) (P.dataDeclDeriving decl))
  pure
    ( P.DataDecl
        { P.dataDeclName = dataSymbol,
          P.dataDeclParams = params,
          P.dataDeclConstructors = ctors,
          P.dataDeclDeriving = map resolvedReferenceSymbol derivingRefs
        },
      ctorRefs ++ derivingRefs
    )

resolveConstructorDecl :: LocalSymbols -> ResolvedSymbol -> CandidateScope -> TypeBinderEnv -> P.ConstructorDecl -> LocalResolveM (P.ResolvedConstructorDecl, [ResolvedReference])
resolveConstructorDecl locals dataSymbol scope typeBinders decl = do
  ctorSymbol <-
    liftResolve $
    uniqueMatchingLocalSymbol
      ProgramUnknownConstructor
      (P.constructorDeclName decl)
      ( \symbol ->
          symbolOwnerIdentity (resolvedSymbolIdentity symbol)
            == Just (SymbolOwnerType (resolvedSymbolIdentity dataSymbol))
      )
      (localValues locals)
  (ty, refs) <- resolveTypeWith typeBinders scope (P.constructorDeclType decl)
  pure
    ( P.ConstructorDecl
        { P.constructorDeclName = ctorSymbol,
          P.constructorDeclType = ty
        },
      refs
    )

resolveClassDecl :: LocalSymbols -> CandidateScope -> P.ClassDecl -> LocalResolveM (P.ResolvedClassDecl, [ResolvedReference])
resolveClassDecl locals scope decl = do
  classSymbol <- liftResolve (uniqueLocalSymbol ProgramUnknownClass (P.classDeclName decl) (localClasses locals))
  (params, typeBinders) <- freshResolvedTypeParams (toListNE (P.classDeclParams decl))
  (superclasses, superclassRefs) <- mapAndRefsLocal (resolveConstraintWith typeBinders scope) (P.classDeclSuperclasses decl)
  (methods, refs) <- mapAndRefsLocal (resolveMethodSig locals classSymbol scope typeBinders) (P.classDeclMethods decl)
  pure
    ( P.ClassDecl
        { P.classDeclName = classSymbol,
          P.classDeclSuperclasses = superclasses,
          P.classDeclParams = toNonEmpty params,
          P.classDeclFundeps = P.classDeclFundeps decl,
          P.classDeclMethods = methods
        },
      superclassRefs ++ refs
    )

resolveMethodSig :: LocalSymbols -> ResolvedSymbol -> CandidateScope -> TypeBinderEnv -> P.MethodSig -> LocalResolveM (P.ResolvedMethodSig, [ResolvedReference])
resolveMethodSig locals classSymbol scope typeBinders sig = do
  methodTypeBinders <- extendTypeBinderEnv typeBinders (Set.toList (freeTypeNamesInConstrainedType (P.methodSigType sig)))
  methodSymbol <-
    liftResolve $
    uniqueMatchingLocalSymbol
      ProgramUnknownMethod
      (P.methodSigName sig)
      ( \symbol ->
          symbolOwnerIdentity (resolvedSymbolIdentity symbol)
            == Just (SymbolOwnerClass (resolvedSymbolIdentity classSymbol))
      )
      (localValues locals)
  (ty, refs) <- resolveConstrainedTypeWith methodTypeBinders scope (P.methodSigType sig)
  pure (P.MethodSig {P.methodSigName = methodSymbol, P.methodSigType = ty}, refs)

resolveInstanceDecl :: CandidateScope -> P.InstanceDecl -> LocalResolveM (P.ResolvedInstanceDecl, [ResolvedReference])
resolveInstanceDecl scope decl = do
  classRef <- liftResolve (resolveClassRef scope (P.instanceDeclClass decl))
  typeBinders <- freshTypeNameEnv (Set.toList (freeTypeNamesInInstanceDecl decl))
  (constraints, constraintRefs) <- mapAndRefsLocal (resolveConstraintWith typeBinders scope) (P.instanceDeclConstraints decl)
  (headTys, typeRefs) <- mapAndRefsLocalNE (resolveTypeWith typeBinders scope) (P.instanceDeclTypes decl)
  (methods, methodRefs) <- mapAndRefsLocal (resolveMethodDef scope typeBinders (resolvedReferenceSymbol classRef)) (P.instanceDeclMethods decl)
  pure
    ( P.InstanceDecl
        { P.instanceDeclConstraints = constraints,
          P.instanceDeclClass = resolvedReferenceSymbol classRef,
          P.instanceDeclTypes = headTys,
          P.instanceDeclMethods = methods
        },
      classRef : constraintRefs ++ typeRefs ++ methodRefs
    )

resolveMethodDef :: CandidateScope -> TypeBinderEnv -> ResolvedSymbol -> P.MethodDef -> LocalResolveM (P.ResolvedMethodDef, [ResolvedReference])
resolveMethodDef scope typeBinders classSymbol def = do
  methodRef <- liftResolve (resolveInstanceMethodRef scope classSymbol (P.methodDefName def))
  (expr, refs) <- resolveExpr scope typeBinders Map.empty (P.methodDefExpr def)
  pure (P.MethodDef {P.methodDefName = resolvedReferenceSymbol methodRef, P.methodDefExpr = expr}, methodRef : refs)

resolveInstanceMethodRef :: CandidateScope -> ResolvedSymbol -> P.MethodName -> ResolveM ResolvedReference
resolveInstanceMethodRef scope classSymbol name = do
  symbol <-
    uniqueMatchingLocalSymbol
      ProgramUnknownMethod
      name
      ( \candidate ->
          symbolNamespace (resolvedSymbolIdentity candidate) == SymbolMethod
            && symbolOwnerIdentity (resolvedSymbolIdentity candidate) == Just owner
      )
      (candidateValues scope)
  pure (mkResolvedReference ResolvedMethodReference name symbol)
  where
    classIdentity = resolvedSymbolIdentity classSymbol
    owner = SymbolOwnerClass classIdentity

resolveDefDecl :: LocalSymbols -> CandidateScope -> P.DefDecl -> LocalResolveM (P.ResolvedDefDecl, [ResolvedReference])
resolveDefDecl locals scope decl = do
  defSymbol <-
    liftResolve $
      uniqueMatchingLocalSymbol
        ProgramUnknownValue
        (P.defDeclName decl)
        ( \symbol ->
            symbolNamespace (resolvedSymbolIdentity symbol) == SymbolValue
              && symbolOwnerIdentity (resolvedSymbolIdentity symbol) == Nothing
        )
        (localValues locals)
  typeBinders <- freshTypeNameEnv (Set.toList (freeTypeNamesInDefDecl decl))
  (ty, typeRefs) <- resolveConstrainedTypeWith typeBinders scope (P.defDeclType decl)
  (expr, exprRefs) <- resolveExpr scope typeBinders Map.empty (P.defDeclExpr decl)
  pure
    ( P.DefDecl
        { P.defDeclName = defSymbol,
          P.defDeclType = ty,
          P.defDeclExpr = expr
        },
      typeRefs ++ exprRefs
    )

resolveConstrainedTypeWith :: TypeBinderEnv -> CandidateScope -> P.ConstrainedType -> LocalResolveM (P.ResolvedConstrainedType, [ResolvedReference])
resolveConstrainedTypeWith typeBinders scope ty = do
  (constraints, constraintRefs) <- mapAndRefsLocal (resolveConstraintWith typeBinders scope) (P.constrainedConstraints ty)
  (body, bodyRefs) <- resolveTypeWith typeBinders scope (P.constrainedBody ty)
  pure
    ( P.ConstrainedType
        { P.constrainedConstraints = constraints,
          P.constrainedBody = body
        },
      constraintRefs ++ bodyRefs
    )

resolveConstraintWith :: TypeBinderEnv -> CandidateScope -> P.ClassConstraint -> LocalResolveM (P.ResolvedClassConstraint, [ResolvedReference])
resolveConstraintWith typeBinders scope constraint = do
  classRef <- liftResolve (resolveClassRef scope (P.constraintClassName constraint))
  (tys, typeRefs) <- mapAndRefsLocalNE (resolveTypeWith typeBinders scope) (P.constraintTypes constraint)
  pure
    ( P.ClassConstraint
        { P.constraintClassName = resolvedReferenceSymbol classRef,
          P.constraintTypes = tys
        },
      classRef : typeRefs
    )

freshResolvedTypeParams :: [P.TypeParam] -> LocalResolveM ([P.TypeParam], TypeBinderEnv)
freshResolvedTypeParams params = do
  pairs <- mapM freshParam params
  pure ([param | (param, _) <- pairs], Map.fromList [(P.typeParamName param, ref) | (param, ref) <- pairs])
  where
    freshParam param = do
      ref <- freshResolvedTypeBinderRef (P.typeParamName param)
      pure (P.ResolvedTypeParam ref (P.typeParamKind param), ref)

freshTypeNameEnv :: [String] -> LocalResolveM TypeBinderEnv
freshTypeNameEnv names =
  Map.fromList <$> mapM freshName (Set.toList (Set.fromList names))
  where
    freshName name = do
      ref <- freshResolvedTypeBinderRef name
      pure (name, ref)

extendTypeBinderEnv :: TypeBinderEnv -> [String] -> LocalResolveM TypeBinderEnv
extendTypeBinderEnv env names =
  foldM addMissing env (Set.toList (Set.fromList names))
  where
    addMissing acc name
      | Map.member name acc = pure acc
      | otherwise = do
          ref <- freshResolvedTypeBinderRef name
          pure (Map.insert name ref acc)

freeTypeNamesInInstanceDecl :: P.InstanceDecl -> Set.Set String
freeTypeNamesInInstanceDecl decl =
  foldMap freeTypeNamesInConstraint (P.instanceDeclConstraints decl)
    `Set.union` foldMap freeVarsSrcType (toListNE (P.instanceDeclTypes decl))
    `Set.union` foldMap (freeTypeNamesInExpr . P.methodDefExpr) (P.instanceDeclMethods decl)

freeTypeNamesInDefDecl :: P.DefDecl -> Set.Set String
freeTypeNamesInDefDecl decl =
  freeTypeNamesInConstrainedType (P.defDeclType decl)
    `Set.union` freeTypeNamesInExpr (P.defDeclExpr decl)

freeTypeNamesInConstrainedType :: P.ConstrainedType -> Set.Set String
freeTypeNamesInConstrainedType ty =
  foldMap freeTypeNamesInConstraint (P.constrainedConstraints ty)
    `Set.union` freeVarsSrcType (P.constrainedBody ty)

freeTypeNamesInConstraint :: P.ClassConstraint -> Set.Set String
freeTypeNamesInConstraint =
  foldMap freeVarsSrcType . toListNE . P.constraintTypes

freeTypeNamesInExpr :: P.Expr -> Set.Set String
freeTypeNamesInExpr expr =
  case expr of
    P.EVar {} -> Set.empty
    P.ELit {} -> Set.empty
    P.ELam param body ->
      maybe Set.empty freeVarsSrcType (P.paramType param)
        `Set.union` freeTypeNamesInExpr body
    P.EApp fun arg ->
      freeTypeNamesInExpr fun `Set.union` freeTypeNamesInExpr arg
    P.ELet _ mbTy rhs body ->
      maybe Set.empty freeVarsSrcType mbTy
        `Set.union` freeTypeNamesInExpr rhs
        `Set.union` freeTypeNamesInExpr body
    P.EAnn inner ty ->
      freeTypeNamesInExpr inner `Set.union` freeVarsSrcType ty
    P.ECase scrutinee alts ->
      freeTypeNamesInExpr scrutinee `Set.union` foldMap freeTypeNamesInAlt alts

freeTypeNamesInAlt :: P.Alt -> Set.Set String
freeTypeNamesInAlt alt =
  freeTypeNamesInPattern (P.altPattern alt)
    `Set.union` freeTypeNamesInExpr (P.altExpr alt)

freeTypeNamesInPattern :: P.Pattern -> Set.Set String
freeTypeNamesInPattern pattern0 =
  case pattern0 of
    P.PatCtor _ args -> foldMap freeTypeNamesInPattern args
    P.PatVar {} -> Set.empty
    P.PatWildcard -> Set.empty
    P.PatAnn inner ty ->
      freeTypeNamesInPattern inner `Set.union` freeVarsSrcType ty

resolveTypeWith :: TypeBinderEnv -> CandidateScope -> SrcType -> LocalResolveM (ResolvedSrcType, [ResolvedReference])
resolveTypeWith typeBinders scope = \case
  STVar name -> do
    ref <- resolvedTypeBinder name
    pure (RSTVar ref, [])
  STBase name -> do
    ref <- liftResolve (resolveTypeName scope name)
    pure (RSTBase (resolvedReferenceSymbol ref), [ref])
  STCon name args -> do
    headRef <- liftResolve (resolveTypeName scope name)
    (args', argRefs) <- mapAndRefsLocal (resolveTypeWith typeBinders scope) (toListNE args)
    pure (RSTCon (resolvedReferenceSymbol headRef) (toNonEmpty args'), headRef : argRefs)
  STVarApp name args -> do
    (args', argRefs) <- mapAndRefsLocal (resolveTypeWith typeBinders scope) (toListNE args)
    ref <- resolvedTypeBinder name
    pure (RSTVarApp ref (toNonEmpty args'), argRefs)
  STTyLam name body -> do
    ref <- freshResolvedTypeBinderRef name
    (body', bodyRefs) <- resolveTypeWith (Map.insert name ref typeBinders) scope body
    pure (RSTTyLam ref body', bodyRefs)
  STTyApp fun arg -> do
    (fun', funRefs) <- resolveTypeWith typeBinders scope fun
    (arg', argRefs) <- resolveTypeWith typeBinders scope arg
    pure (RSTTyApp fun' arg', funRefs ++ argRefs)
  STArrow dom cod -> do
    (dom', domRefs) <- resolveTypeWith typeBinders scope dom
    (cod', codRefs) <- resolveTypeWith typeBinders scope cod
    pure (RSTArrow dom' cod', domRefs ++ codRefs)
  STForall name mb body -> do
    ref <- freshResolvedTypeBinderRef name
    (mb', boundRefs) <-
      case mb of
        Nothing -> pure (Nothing, [])
        Just bound -> do
          (bound', refs) <- resolveTypeWith typeBinders scope (unSrcBound bound)
          pure (Just (mkResolvedSrcBound bound'), refs)
    (body', bodyRefs) <- resolveTypeWith (Map.insert name ref typeBinders) scope body
    pure (RSTForall ref mb' body', boundRefs ++ bodyRefs)
  STMu name body -> do
    ref <- freshResolvedTypeBinderRef name
    (body', refs) <- resolveTypeWith (Map.insert name ref typeBinders) scope body
    pure (RSTMu ref body', refs)
  STBottom -> pure (RSTBottom, [])
  where
    resolvedTypeBinder name =
      case Map.lookup name typeBinders of
        Just ref -> pure ref
        Nothing -> liftResolve (Left (ProgramPipelineError ("unresolved type binder `" ++ name ++ "` reached resolver")))

resolveTypeName :: CandidateScope -> String -> ResolveM ResolvedReference
resolveTypeName scope name
  | name `Set.member` PrimitiveInventory.builtinTypeNames =
      pure (mkResolvedReference ResolvedTypeReference name (builtinTypeSymbol name))
  | otherwise = resolveReference ResolvedTypeReference ProgramUnknownType candidateTypes scope name

resolveClassRef :: CandidateScope -> P.ClassName -> ResolveM ResolvedReference
resolveClassRef scope name =
  resolveReference ResolvedClassReference ProgramUnknownClass candidateClasses scope name

resolveValueRef :: CandidateScope -> P.ValueName -> ResolveM ResolvedReference
resolveValueRef scope name =
  let kindFor symbol =
        case symbolNamespace (resolvedSymbolIdentity symbol) of
          SymbolConstructor -> ResolvedConstructorReference
          SymbolMethod -> ResolvedMethodReference
          _ -> ResolvedValueReference
   in do
        symbol <- resolveSymbol ProgramUnknownValue (candidateValues scope) name
        pure (mkResolvedReference (kindFor symbol) name symbol)

resolveConstructorRef :: CandidateScope -> P.ConstructorName -> ResolveM ResolvedReference
resolveConstructorRef scope name = do
  symbol <- resolveSymbol ProgramUnknownConstructor (candidateValues scope) name
  if symbolNamespace (resolvedSymbolIdentity symbol) == SymbolConstructor
    then pure (mkResolvedReference ResolvedConstructorReference name symbol)
    else Left (ProgramUnknownConstructor name)

resolveReference ::
  ResolvedReferenceKind ->
  (String -> ProgramError) ->
  (CandidateScope -> Map String [ResolvedSymbol]) ->
  CandidateScope ->
  String ->
  ResolveM ResolvedReference
resolveReference kind unknownErr select scope name = do
  symbol <- resolveSymbol unknownErr (select scope) name
  pure (mkResolvedReference kind name symbol)

resolveSymbol :: (String -> ProgramError) -> Map String [ResolvedSymbol] -> String -> ResolveM ResolvedSymbol
resolveSymbol unknownErr candidates name =
  case Map.lookup name candidates of
    Nothing -> Left (unknownErr name)
    Just [] -> Left (unknownErr name)
    Just symbols ->
      case distinctByIdentity symbols of
        [symbol] -> pure symbol
        _ -> Left (ProgramAmbiguousUnqualifiedReference name)

resolveExpr :: CandidateScope -> TypeBinderEnv -> LocalEnv -> P.Expr -> LocalResolveM (P.ResolvedExpr, [ResolvedReference])
resolveExpr scope typeBinders locals = \case
  P.EVar name
    | Just localRef <- Map.lookup name locals -> pure (P.EVar (P.ResolvedLocalValue localRef), [])
    | otherwise -> do
        ref <- liftResolve (resolveValueRef scope name)
        pure (P.EVar (P.ResolvedGlobalValue (resolvedReferenceSymbol ref)), [ref])
  P.ELit lit -> pure (P.ELit lit, [])
  P.ELam param body -> do
    localRef <- freshLocalResolveRef (P.paramName param)
    (param', paramTypeRefs) <- resolveParam scope typeBinders localRef param
    (body', bodyRefs) <- resolveExpr scope typeBinders (Map.insert (P.paramName param) localRef locals) body
    pure (P.ELam param' body', paramTypeRefs ++ bodyRefs)
  P.EApp fun arg -> do
    (fun', funRefs) <- resolveExpr scope typeBinders locals fun
    (arg', argRefs) <- resolveExpr scope typeBinders locals arg
    pure (P.EApp fun' arg', funRefs ++ argRefs)
  P.ELet name mbTy rhs body -> do
    localRef <- freshLocalResolveRef name
    let locals' = Map.insert name localRef locals
    (mbTy', typeRefs) <-
      case mbTy of
        Nothing -> pure (Nothing, [])
        Just ty -> firstWithRefs Just <$> resolveTypeWith typeBinders scope ty
    (rhs', rhsRefs) <- resolveExpr scope typeBinders locals' rhs
    (body', bodyRefs) <- resolveExpr scope typeBinders locals' body
    pure (P.ELet localRef mbTy' rhs' body', typeRefs ++ rhsRefs ++ bodyRefs)
  P.EAnn expr ty -> do
    (expr', exprRefs) <- resolveExpr scope typeBinders locals expr
    (ty', typeRefs) <- resolveTypeWith typeBinders scope ty
    pure (P.EAnn expr' ty', exprRefs ++ typeRefs)
  P.ECase scrutinee alts -> do
    (scrutinee', scrutineeRefs) <- resolveExpr scope typeBinders locals scrutinee
    (alts', altRefs) <- mapAndRefsLocal (resolveAlt scope typeBinders locals) alts
    pure (P.ECase scrutinee' alts', scrutineeRefs ++ altRefs)

resolveParam :: CandidateScope -> TypeBinderEnv -> LocalRef -> P.Param -> LocalResolveM (P.ResolvedParam, [ResolvedReference])
resolveParam scope typeBinders localRef param =
  case P.paramType param of
    Nothing ->
      pure (P.Param {P.paramName = localRef, P.paramType = Nothing}, [])
    Just ty -> do
      (ty', refs) <- resolveTypeWith typeBinders scope ty
      pure (P.Param {P.paramName = localRef, P.paramType = Just ty'}, refs)

resolveAlt :: CandidateScope -> TypeBinderEnv -> LocalEnv -> P.Alt -> LocalResolveM (P.ResolvedAlt, [ResolvedReference])
resolveAlt scope typeBinders locals alt = do
  (pattern', patternLocals, patternRefs) <- resolvePattern scope typeBinders (P.altPattern alt)
  (body', bodyRefs) <- resolveExpr scope typeBinders (patternLocals `Map.union` locals) (P.altExpr alt)
  pure (P.Alt {P.altPattern = pattern', P.altExpr = body'}, patternRefs ++ bodyRefs)

resolvePattern :: CandidateScope -> TypeBinderEnv -> P.Pattern -> LocalResolveM (P.ResolvedPattern, LocalEnv, [ResolvedReference])
resolvePattern scope typeBinders = \case
  P.PatCtor name args -> do
    ctorRef <- liftResolve (resolveConstructorRef scope name)
    resolvedArgs <- mapM (resolvePattern scope typeBinders) args
    let locals = Map.unions [localNames | (_, localNames, _) <- resolvedArgs]
        refs = ctorRef : concat [refs0 | (_, _, refs0) <- resolvedArgs]
        args' = [pattern0 | (pattern0, _, _) <- resolvedArgs]
    pure (P.PatCtor (resolvedReferenceSymbol ctorRef) args', locals, refs)
  P.PatVar name -> do
    localRef <- freshLocalResolveRef name
    pure (P.PatVar localRef, Map.singleton name localRef, [])
  P.PatWildcard -> pure (P.PatWildcard, Map.empty, [])
  P.PatAnn pattern0 ty -> do
    (pattern', locals, patternRefs) <- resolvePattern scope typeBinders pattern0
    (ty', typeRefs) <- resolveTypeWith typeBinders scope ty
    pure (P.PatAnn pattern' ty', locals, patternRefs ++ typeRefs)

resolvedScopeFromCandidates :: (String -> ProgramError) -> CandidateScope -> ResolveM ResolvedScope
resolvedScopeFromCandidates duplicateErr scope =
  ResolvedScope
    <$> uniqueMap duplicateErr (candidateValues scope)
    <*> uniqueMap duplicateErr (candidateTypes scope)
    <*> uniqueMap duplicateErr (candidateClasses scope)
    <*> uniqueMap duplicateErr (candidateModules scope)

{- Note [Local method candidates in resolved module scope]
`resolveModuleReferences` reads the full candidate map, so a bare use of a
same-named method from multiple local classes is still ambiguous. The exported
module-scope snapshot is a unique-symbol map, though, and same-named local
methods have no unique value entry until an export selects the owning class.
-}
resolvedModuleScopeFromCandidates :: P.ModuleName -> CandidateScope -> ResolveM ResolvedScope
resolvedModuleScopeFromCandidates moduleName0 scope =
  resolvedScopeFromCandidates
    ProgramDuplicateVisibleName
    scope {candidateValues = Map.mapMaybe moduleScopeValue (candidateValues scope)}
  where
    moduleScopeValue symbols =
      case distinctByIdentity symbols of
        [] -> Just symbols
        [_] -> Just symbols
        distinct
          | all (isLocalMethod moduleName0) distinct -> Nothing
          | otherwise -> Just symbols

    isLocalMethod currentModule symbol =
      symbolNamespace (resolvedSymbolIdentity symbol) == SymbolMethod
        && symbolSpellingOrigin (resolvedSymbolSpelling symbol) == SymbolLocal currentModule

uniqueMap :: (String -> ProgramError) -> Map String [ResolvedSymbol] -> ResolveM (Map String ResolvedSymbol)
uniqueMap duplicateErr =
  fmap Map.fromList . mapM uniqueEntry . Map.toList
  where
    uniqueEntry (name, symbols) =
      case distinctByIdentity symbols of
        [symbol] -> pure (name, symbol)
        [] -> Left (duplicateErr name)
        _ -> Left (duplicateErr name)

distinctByIdentity :: [ResolvedSymbol] -> [ResolvedSymbol]
distinctByIdentity =
  reverse . foldl' add []
  where
    add acc symbol
      | any (sameResolvedSymbol symbol) acc = acc
      | otherwise = symbol : acc

addLocalSymbols :: CandidateScope -> LocalSymbols -> CandidateScope
addLocalSymbols scope locals =
  scope
    { candidateValues = candidateValues scope `mergeCandidateMaps` localValues locals,
      candidateTypes = candidateTypes scope `mergeCandidateMaps` localTypes locals,
      candidateClasses = candidateClasses scope `mergeCandidateMaps` localClasses locals
    }

mergeCandidateMaps :: Map String [ResolvedSymbol] -> Map String [ResolvedSymbol] -> Map String [ResolvedSymbol]
mergeCandidateMaps = Map.unionWith (++)

addCandidateValue :: String -> ResolvedSymbol -> CandidateScope -> CandidateScope
addCandidateValue name symbol scope =
  scope {candidateValues = Map.insertWith (++) name [symbol] (candidateValues scope)}

addCandidateType :: String -> ResolvedSymbol -> CandidateScope -> CandidateScope
addCandidateType name symbol scope =
  scope {candidateTypes = Map.insertWith (++) name [symbol] (candidateTypes scope)}

addCandidateClass :: String -> ResolvedSymbol -> CandidateScope -> CandidateScope
addCandidateClass name symbol scope =
  scope {candidateClasses = Map.insertWith (++) name [symbol] (candidateClasses scope)}

addCandidateModule :: P.ModuleName -> ResolvedSymbol -> CandidateScope -> CandidateScope
addCandidateModule name symbol scope =
  scope {candidateModules = Map.insertWith (++) name [symbol] (candidateModules scope)}

respell :: SymbolOrigin -> String -> String -> ResolvedSymbol -> ResolvedSymbol
respell origin sourceName displayName symbol =
  mkResolvedSymbol (resolvedSymbolIdentity symbol) sourceName displayName origin

qualifyName :: P.ModuleName -> String -> String
qualifyName alias name = alias ++ "." ++ name

dataDeclSymbol :: P.ModuleName -> SymbolIdentity -> P.DataDecl -> ResolvedSymbol
dataDeclSymbol moduleName0 identity decl =
  mkResolvedSymbol
    identity
    (P.dataDeclName decl)
    (P.dataDeclName decl)
    (SymbolLocal moduleName0)

constructorDeclSymbol :: P.ModuleName -> SymbolIdentity -> P.ConstructorDecl -> ResolvedSymbol
constructorDeclSymbol moduleName0 identity ctorDecl =
  mkResolvedSymbol
    identity
    (P.constructorDeclName ctorDecl)
    (P.constructorDeclName ctorDecl)
    (SymbolLocal moduleName0)

classDeclSymbol :: P.ModuleName -> SymbolIdentity -> P.ClassDecl -> ResolvedSymbol
classDeclSymbol moduleName0 identity decl =
  mkResolvedSymbol
    identity
    (P.classDeclName decl)
    (P.classDeclName decl)
    (SymbolLocal moduleName0)

methodSigSymbol :: P.ModuleName -> SymbolIdentity -> P.MethodSig -> ResolvedSymbol
methodSigSymbol moduleName0 identity methodSig =
  mkResolvedSymbol
    identity
    (P.methodSigName methodSig)
    (P.methodSigName methodSig)
    (SymbolLocal moduleName0)

defDeclSymbol :: P.ModuleName -> SymbolIdentity -> P.DefDecl -> ResolvedSymbol
defDeclSymbol moduleName0 identity decl =
  mkResolvedSymbol
    identity
    (P.defDeclName decl)
    (P.defDeclName decl)
    (SymbolLocal moduleName0)

moduleDefDecls :: P.Module -> [P.DefDecl]
moduleDefDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc = case decl of
      P.DeclDef defDecl -> defDecl : acc
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

ensureDistinctBy :: (String -> ProgramError) -> (a -> String) -> [a] -> ResolveM ()
ensureDistinctBy err key xs = ensureDistinctPlain err (map key xs)

ensureDistinctPlain :: (String -> ProgramError) -> [String] -> ResolveM ()
ensureDistinctPlain err = go Set.empty
  where
    go _ [] = pure ()
    go seen (name : rest)
      | name `Set.member` seen = Left (err name)
      | otherwise = go (Set.insert name seen) rest

ensureDistinctImportAliases :: [P.Import] -> ResolveM ()
ensureDistinctImportAliases imports0 =
  ensureDistinctPlain
    ProgramDuplicateImportAlias
    [alias | Just alias <- map P.importAlias imports0]

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

toNonEmpty :: [a] -> NonEmpty a
toNonEmpty values =
  case values of
    x : xs -> x :| xs
    [] -> error "internal resolver invariant: STCon has at least one argument"

firstWithRefs :: (a -> b) -> (a, [ResolvedReference]) -> (b, [ResolvedReference])
firstWithRefs f (value, refs) = (f value, refs)

mapAndRefsLocal :: (a -> LocalResolveM (b, [ResolvedReference])) -> [a] -> LocalResolveM ([b], [ResolvedReference])
mapAndRefsLocal f values = do
  resolved <- mapM f values
  pure ([value | (value, _) <- resolved], concat [refs | (_, refs) <- resolved])

mapAndRefsLocalNE :: (a -> LocalResolveM (b, [ResolvedReference])) -> NonEmpty a -> LocalResolveM (NonEmpty b, [ResolvedReference])
mapAndRefsLocalNE f values = do
  (resolved, refs) <- mapAndRefsLocal f (toListNE values)
  pure (toNonEmpty resolved, refs)
