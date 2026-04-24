{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module MLF.Frontend.Program.Resolve
  ( resolveProgram,
  )
where

import Control.Monad (foldM, forM_, when)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax
  ( ResolvedSrcType,
    ResolvedSrcTy (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    mkResolvedSrcBound,
  )
import qualified MLF.Frontend.Syntax.Program as P

type ResolveM a = Either ProgramError a

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
  (_, resolvedModulesRev) <- foldM resolveModule (Map.empty, []) orderedModules
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
  (Map P.ModuleName ResolvedScope, [ResolvedModule]) ->
  P.Module ->
  ResolveM (Map P.ModuleName ResolvedScope, [ResolvedModule])
resolveModule (priorExports, resolvedRev) mod0 = do
  ensureDistinctImportAliases (P.moduleImports mod0)
  importScope <- buildImportScope priorExports (P.moduleImports mod0)
  locals <- buildLocalSymbols mod0
  let fullCandidates = addLocalSymbols importScope locals
  (resolvedSyntax, references) <- resolveModuleSyntax priorExports locals fullCandidates mod0
  fullScope <- resolvedModuleScopeFromCandidates (P.moduleName mod0) fullCandidates
  exports <- buildExports mod0 locals
  let resolved =
        ResolvedModule
          { resolvedModuleName = P.moduleName mod0,
            resolvedModuleSyntax = resolvedSyntax,
            resolvedModuleLocalValues = localValues locals,
            resolvedModuleLocalTypes = localTypes locals,
            resolvedModuleLocalClasses = localClasses locals,
            resolvedModuleScope = fullScope,
            resolvedModuleExports = exports,
            resolvedModuleReferences = references
          }
  pure (Map.insert (P.moduleName mod0) exports priorExports, resolved : resolvedRev)

buildImportScope :: Map P.ModuleName ResolvedScope -> [P.Import] -> ResolveM CandidateScope
buildImportScope priorExports =
  foldM addImport (addBuiltinSymbols emptyCandidateScope)
  where
    addImport scope imp = do
      exports <-
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
                  (resolvedModuleSymbol (SymbolQualifiedImport moduleName0 alias) moduleName0 alias)
                  (addAllExports (SymbolQualifiedImport moduleName0 alias) (qualifyName alias) exports scope)
          case P.importExposing imp of
            Nothing -> pure qualifiedScope
            Just items -> foldM (applyImportItem moduleName0 exports) qualifiedScope items

addBuiltinSymbols :: CandidateScope -> CandidateScope
addBuiltinSymbols =
  addCandidateValue "__mlfp_and" (builtinSymbol SymbolValue "__mlfp_and")
    . addCandidateType "Int" (builtinSymbol SymbolType "Int")
    . addCandidateType "Bool" (builtinSymbol SymbolType "Bool")
    . addCandidateType "String" (builtinSymbol SymbolType "String")

builtinSymbol :: SymbolNamespace -> String -> ResolvedSymbol
builtinSymbol namespace name =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = namespace,
          symbolDefiningModule = "<builtin>",
          symbolDefiningName = name,
          symbolOwnerIdentity = Nothing
        }
    )
    name
    name
    SymbolBuiltin

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
      (Nothing, Nothing) -> Left (ProgramImportNotExported moduleName0 typeName)
  P.ExportTypeWithConstructors typeName ->
    case Map.lookup typeName (resolvedScopeTypes exports) of
      Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
      Just typeSymbol -> do
        let constructorSymbols =
              [ (name, symbol)
                | (name, symbol) <- Map.toList (resolvedScopeValues exports),
                  symbolNamespace (resolvedSymbolIdentity symbol) == SymbolConstructor,
                  symbolOwnerIdentity (resolvedSymbolIdentity symbol) == Just (SymbolOwnerType moduleName0 typeName)
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

buildLocalSymbols :: P.Module -> ResolveM LocalSymbols
buildLocalSymbols mod0 = do
  ensureDistinctBy ProgramDuplicateType P.dataDeclName dataDecls
  ensureDistinctPlain ProgramDuplicateConstructor (concatMap (map P.constructorDeclName . P.dataDeclConstructors) dataDecls)
  ensureDistinctBy ProgramDuplicateClass P.classDeclName classDecls
  ensureDistinctBy ProgramDuplicateValue P.defDeclName defDecls
  forM_ classDecls $ \classDecl ->
    ensureDistinctBy ProgramDuplicateMethod P.methodSigName (P.classDeclMethods classDecl)
  let dataTypes = Map.fromListWith (++) [(P.dataDeclName decl, [dataDeclSymbol modName decl]) | decl <- dataDecls]
      constructors =
        Map.fromListWith
          (++)
          [ (P.constructorDeclName ctor, [constructorDeclSymbol modName dataDecl ctor])
            | dataDecl <- dataDecls,
              ctor <- P.dataDeclConstructors dataDecl
          ]
      classes = Map.fromListWith (++) [(P.classDeclName decl, [classDeclSymbol modName decl]) | decl <- classDecls]
      methods =
        Map.fromListWith
          (++)
          [ (P.methodSigName method, [methodSigSymbol modName classDecl method])
            | classDecl <- classDecls,
              method <- P.classDeclMethods classDecl
          ]
      defs = Map.fromListWith (++) [(P.defDeclName decl, [defDeclSymbol modName decl]) | decl <- defDecls]
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
        case Map.lookup name (localValues locals) of
          Just symbols -> pure acc {candidateValues = Map.insertWith (++) name symbols (candidateValues acc)}
          Nothing -> Left (ProgramExportNotLocal name)
      P.ExportType typeName ->
        case (Map.lookup typeName (localTypes locals), Map.lookup typeName (localClasses locals)) of
          (Nothing, Nothing) -> Left (ProgramExportNotLocal typeName)
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
      symbolOwnerIdentity (resolvedSymbolIdentity symbol) == Just (SymbolOwnerClass (P.moduleName mod0) className0)

    isConstructorOf typeName symbol =
      symbolOwnerIdentity (resolvedSymbolIdentity symbol) == Just (SymbolOwnerType (P.moduleName mod0) typeName)

resolveModuleSyntax ::
  Map P.ModuleName ResolvedScope ->
  LocalSymbols ->
  CandidateScope ->
  P.Module ->
  ResolveM (P.ResolvedModuleSyntax, [ResolvedReference])
resolveModuleSyntax priorExports locals scope mod0 = do
  imports0 <- mapM (resolveImport priorExports) (P.moduleImports mod0)
  exports0 <- mapM (mapM (resolveExportItem locals)) (P.moduleExports mod0)
  (decls0, refs) <- mapAndRefs resolveDecl (P.moduleDecls mod0)
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
      P.DeclData decl -> firstWithRefs P.DeclData <$> resolveDataDecl scope decl
      P.DeclClass decl -> firstWithRefs P.DeclClass <$> resolveClassDecl scope decl
      P.DeclInstance decl -> firstWithRefs P.DeclInstance <$> resolveInstanceDecl scope decl
      P.DeclDef decl -> firstWithRefs P.DeclDef <$> resolveDefDecl scope decl

resolveImport :: Map P.ModuleName ResolvedScope -> P.Import -> ResolveM P.ResolvedImport
resolveImport priorExports imp = do
  let moduleName0 = P.importModuleName imp
  case Map.lookup moduleName0 priorExports of
    Nothing -> Left (ProgramUnknownImportModule moduleName0)
    Just exports -> do
      exposing0 <- mapM (mapM (resolveImportItem moduleName0 exports)) (P.importExposing imp)
      pure
        P.Import
          { P.importModuleName = resolvedModuleSymbol (SymbolUnqualifiedImport moduleName0) moduleName0 moduleName0,
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
        Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
    P.ExportTypeWithConstructors typeName ->
      case Map.lookup typeName (resolvedScopeTypes exports) of
        Nothing -> Left (ProgramImportNotExported moduleName0 typeName)
        Just _ ->
          case resolvedExportTypeRef typeName exports of
            Just ref -> pure (P.ExportTypeWithConstructors ref)
            Nothing -> Left (ProgramImportNotExported moduleName0 typeName)

resolveExportItem :: LocalSymbols -> P.ExportItem -> ResolveM P.ResolvedExportItem
resolveExportItem locals item =
  case item of
    P.ExportValue name ->
      P.ExportValue <$> uniqueLocalSymbol ProgramExportNotLocal name (localValues locals)
    P.ExportType typeName ->
      case resolvedLocalExportTypeRef locals typeName of
        Just ref -> pure (P.ExportType ref)
        Nothing -> Left (ProgramExportNotLocal typeName)
    P.ExportTypeWithConstructors typeName ->
      case Map.lookup typeName (localTypes locals) of
        Nothing -> Left (ProgramExportNotLocal typeName)
        Just _ ->
          case resolvedLocalExportTypeRef locals typeName of
            Just ref -> pure (P.ExportTypeWithConstructors ref)
            Nothing -> Left (ProgramExportNotLocal typeName)

resolvedLocalExportTypeRef :: LocalSymbols -> String -> Maybe P.ResolvedExportTypeRef
resolvedLocalExportTypeRef locals name =
  let symbols =
        Map.findWithDefault [] name (localTypes locals)
          ++ Map.findWithDefault [] name (localClasses locals)
   in case distinctByIdentity symbols of
        [] -> Nothing
        distinct -> Just (P.ResolvedExportTypeRef name distinct)

resolvedExportTypeRef :: String -> ResolvedScope -> Maybe P.ResolvedExportTypeRef
resolvedExportTypeRef name exports =
  let symbols =
        maybe [] (: []) (Map.lookup name (resolvedScopeTypes exports))
          ++ maybe [] (: []) (Map.lookup name (resolvedScopeClasses exports))
   in case distinctByIdentity symbols of
        [] -> Nothing
        distinct -> Just (P.ResolvedExportTypeRef name distinct)

uniqueLocalSymbol :: (String -> ProgramError) -> String -> Map String [ResolvedSymbol] -> ResolveM ResolvedSymbol
uniqueLocalSymbol err name symbolsByName =
  case Map.lookup name symbolsByName of
    Just symbols ->
      case distinctByIdentity symbols of
        [symbol] -> pure symbol
        _ -> Left (err name)
    Nothing -> Left (err name)

resolveDataDecl :: CandidateScope -> P.DataDecl -> ResolveM (P.ResolvedDataDecl, [ResolvedReference])
resolveDataDecl scope decl = do
  (ctors, ctorRefs) <- mapAndRefs (resolveConstructorDecl scope) (P.dataDeclConstructors decl)
  derivingRefs <- mapM (resolveClassRef scope) (P.dataDeclDeriving decl)
  pure
    ( P.DataDecl
        { P.dataDeclName = P.dataDeclName decl,
          P.dataDeclParams = P.dataDeclParams decl,
          P.dataDeclConstructors = ctors,
          P.dataDeclDeriving = map resolvedReferenceSymbol derivingRefs
        },
      ctorRefs ++ derivingRefs
    )

resolveConstructorDecl :: CandidateScope -> P.ConstructorDecl -> ResolveM (P.ResolvedConstructorDecl, [ResolvedReference])
resolveConstructorDecl scope decl = do
  (ty, refs) <- resolveType scope (P.constructorDeclType decl)
  pure
    ( P.ConstructorDecl
        { P.constructorDeclName = P.constructorDeclName decl,
          P.constructorDeclType = ty
        },
      refs
    )

resolveClassDecl :: CandidateScope -> P.ClassDecl -> ResolveM (P.ResolvedClassDecl, [ResolvedReference])
resolveClassDecl scope decl = do
  (methods, refs) <- mapAndRefs (resolveMethodSig scope) (P.classDeclMethods decl)
  pure
    ( P.ClassDecl
        { P.classDeclName = P.classDeclName decl,
          P.classDeclParam = P.classDeclParam decl,
          P.classDeclMethods = methods
        },
      refs
    )

resolveMethodSig :: CandidateScope -> P.MethodSig -> ResolveM (P.ResolvedMethodSig, [ResolvedReference])
resolveMethodSig scope sig = do
  (ty, refs) <- resolveConstrainedType scope (P.methodSigType sig)
  pure (P.MethodSig {P.methodSigName = P.methodSigName sig, P.methodSigType = ty}, refs)

resolveInstanceDecl :: CandidateScope -> P.InstanceDecl -> ResolveM (P.ResolvedInstanceDecl, [ResolvedReference])
resolveInstanceDecl scope decl = do
  classRef <- resolveClassRef scope (P.instanceDeclClass decl)
  (constraints, constraintRefs) <- mapAndRefs (resolveConstraint scope) (P.instanceDeclConstraints decl)
  (headTy, typeRefs) <- resolveType scope (P.instanceDeclType decl)
  (methods, methodRefs) <- mapAndRefs (resolveMethodDef scope) (P.instanceDeclMethods decl)
  pure
    ( P.InstanceDecl
        { P.instanceDeclConstraints = constraints,
          P.instanceDeclClass = resolvedReferenceSymbol classRef,
          P.instanceDeclType = headTy,
          P.instanceDeclMethods = methods
        },
      classRef : constraintRefs ++ typeRefs ++ methodRefs
    )

resolveMethodDef :: CandidateScope -> P.MethodDef -> ResolveM (P.ResolvedMethodDef, [ResolvedReference])
resolveMethodDef scope def = do
  (expr, refs) <- resolveExpr scope Set.empty (P.methodDefExpr def)
  pure (P.MethodDef {P.methodDefName = P.methodDefName def, P.methodDefExpr = expr}, refs)

resolveDefDecl :: CandidateScope -> P.DefDecl -> ResolveM (P.ResolvedDefDecl, [ResolvedReference])
resolveDefDecl scope decl = do
  (ty, typeRefs) <- resolveConstrainedType scope (P.defDeclType decl)
  (expr, exprRefs) <- resolveExpr scope Set.empty (P.defDeclExpr decl)
  pure
    ( P.DefDecl
        { P.defDeclName = P.defDeclName decl,
          P.defDeclType = ty,
          P.defDeclExpr = expr
        },
      typeRefs ++ exprRefs
    )

resolveConstrainedType :: CandidateScope -> P.ConstrainedType -> ResolveM (P.ResolvedConstrainedType, [ResolvedReference])
resolveConstrainedType scope ty = do
  (constraints, constraintRefs) <- mapAndRefs (resolveConstraint scope) (P.constrainedConstraints ty)
  (body, bodyRefs) <- resolveType scope (P.constrainedBody ty)
  pure
    ( P.ConstrainedType
        { P.constrainedConstraints = constraints,
          P.constrainedBody = body
        },
      constraintRefs ++ bodyRefs
    )

resolveConstraint :: CandidateScope -> P.ClassConstraint -> ResolveM (P.ResolvedClassConstraint, [ResolvedReference])
resolveConstraint scope constraint = do
  classRef <- resolveClassRef scope (P.constraintClassName constraint)
  (ty, typeRefs) <- resolveType scope (P.constraintType constraint)
  pure
    ( P.ClassConstraint
        { P.constraintClassName = resolvedReferenceSymbol classRef,
          P.constraintType = ty
        },
      classRef : typeRefs
    )

resolveType :: CandidateScope -> SrcType -> ResolveM (ResolvedSrcType, [ResolvedReference])
resolveType scope = \case
  STVar name -> pure (RSTVar name, [])
  STBase name -> do
    ref <- resolveTypeName scope name
    pure (RSTBase (resolvedReferenceSymbol ref), [ref])
  STCon name args -> do
    headRef <- resolveTypeName scope name
    (args', argRefs) <- mapAndRefs (resolveType scope) (toListNE args)
    pure (RSTCon (resolvedReferenceSymbol headRef) (toNonEmpty args'), headRef : argRefs)
  STArrow dom cod -> do
    (dom', domRefs) <- resolveType scope dom
    (cod', codRefs) <- resolveType scope cod
    pure (RSTArrow dom' cod', domRefs ++ codRefs)
  STForall name mb body -> do
    (mb', boundRefs) <-
      case mb of
        Nothing -> pure (Nothing, [])
        Just bound -> do
          (bound', refs) <- resolveType scope (unSrcBound bound)
          pure (Just (mkResolvedSrcBound bound'), refs)
    (body', bodyRefs) <- resolveType scope body
    pure (RSTForall name mb' body', boundRefs ++ bodyRefs)
  STMu name body -> do
    (body', refs) <- resolveType scope body
    pure (RSTMu name body', refs)
  STBottom -> pure (RSTBottom, [])

resolveTypeName :: CandidateScope -> String -> ResolveM ResolvedReference
resolveTypeName scope name
  | name `Set.member` builtinTypeNames = pure (ResolvedReference ResolvedTypeReference name (builtinSymbol SymbolType name))
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
        pure (ResolvedReference (kindFor symbol) name symbol)

resolveConstructorRef :: CandidateScope -> P.ConstructorName -> ResolveM ResolvedReference
resolveConstructorRef scope name = do
  symbol <- resolveSymbol ProgramUnknownConstructor (candidateValues scope) name
  if symbolNamespace (resolvedSymbolIdentity symbol) == SymbolConstructor
    then pure (ResolvedReference ResolvedConstructorReference name symbol)
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
  pure (ResolvedReference kind name symbol)

resolveSymbol :: (String -> ProgramError) -> Map String [ResolvedSymbol] -> String -> ResolveM ResolvedSymbol
resolveSymbol unknownErr candidates name =
  case Map.lookup name candidates of
    Nothing -> Left (unknownErr name)
    Just [] -> Left (unknownErr name)
    Just symbols ->
      case distinctByIdentity symbols of
        [symbol] -> pure symbol
        _ -> Left (ProgramAmbiguousUnqualifiedReference name)

resolveExpr :: CandidateScope -> Set.Set String -> P.Expr -> ResolveM (P.ResolvedExpr, [ResolvedReference])
resolveExpr scope locals = \case
  P.EVar name
    | name `Set.member` locals -> pure (P.EVar (P.ResolvedLocalValue name), [])
    | otherwise -> do
        ref <- resolveValueRef scope name
        pure (P.EVar (P.ResolvedGlobalValue (resolvedReferenceSymbol ref)), [ref])
  P.ELit lit -> pure (P.ELit lit, [])
  P.ELam param body -> do
    (param', paramTypeRefs) <- resolveParam scope param
    (body', bodyRefs) <- resolveExpr scope (Set.insert (P.paramName param) locals) body
    pure (P.ELam param' body', paramTypeRefs ++ bodyRefs)
  P.EApp fun arg -> do
    (fun', funRefs) <- resolveExpr scope locals fun
    (arg', argRefs) <- resolveExpr scope locals arg
    pure (P.EApp fun' arg', funRefs ++ argRefs)
  P.ELet name mbTy rhs body -> do
    let locals' = Set.insert name locals
    (mbTy', typeRefs) <-
      case mbTy of
        Nothing -> pure (Nothing, [])
        Just ty -> firstWithRefs Just <$> resolveType scope ty
    (rhs', rhsRefs) <- resolveExpr scope locals' rhs
    (body', bodyRefs) <- resolveExpr scope locals' body
    pure (P.ELet name mbTy' rhs' body', typeRefs ++ rhsRefs ++ bodyRefs)
  P.EAnn expr ty -> do
    (expr', exprRefs) <- resolveExpr scope locals expr
    (ty', typeRefs) <- resolveType scope ty
    pure (P.EAnn expr' ty', exprRefs ++ typeRefs)
  P.ECase scrutinee alts -> do
    (scrutinee', scrutineeRefs) <- resolveExpr scope locals scrutinee
    (alts', altRefs) <- mapAndRefs (resolveAlt scope locals) alts
    pure (P.ECase scrutinee' alts', scrutineeRefs ++ altRefs)

resolveParam :: CandidateScope -> P.Param -> ResolveM (P.ResolvedParam, [ResolvedReference])
resolveParam scope param =
  case P.paramType param of
    Nothing ->
      pure (P.Param {P.paramName = P.paramName param, P.paramType = Nothing}, [])
    Just ty -> do
      (ty', refs) <- resolveType scope ty
      pure (P.Param {P.paramName = P.paramName param, P.paramType = Just ty'}, refs)

resolveAlt :: CandidateScope -> Set.Set String -> P.Alt -> ResolveM (P.ResolvedAlt, [ResolvedReference])
resolveAlt scope locals alt = do
  (pattern', patternLocals, patternRefs) <- resolvePattern scope (P.altPattern alt)
  (body', bodyRefs) <- resolveExpr scope (patternLocals `Set.union` locals) (P.altExpr alt)
  pure (P.Alt {P.altPattern = pattern', P.altExpr = body'}, patternRefs ++ bodyRefs)

resolvePattern :: CandidateScope -> P.Pattern -> ResolveM (P.ResolvedPattern, Set.Set String, [ResolvedReference])
resolvePattern scope = \case
  P.PatCtor name args -> do
    ctorRef <- resolveConstructorRef scope name
    resolvedArgs <- mapM (resolvePattern scope) args
    let locals = Set.unions [localNames | (_, localNames, _) <- resolvedArgs]
        refs = ctorRef : concat [refs0 | (_, _, refs0) <- resolvedArgs]
        args' = [pattern0 | (pattern0, _, _) <- resolvedArgs]
    pure (P.PatCtor (resolvedReferenceSymbol ctorRef) args', locals, refs)
  P.PatVar name -> pure (P.PatVar name, Set.singleton name, [])
  P.PatWildcard -> pure (P.PatWildcard, Set.empty, [])
  P.PatAnn pattern0 ty -> do
    (pattern', locals, patternRefs) <- resolvePattern scope pattern0
    (ty', typeRefs) <- resolveType scope ty
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

dataDeclSymbol :: P.ModuleName -> P.DataDecl -> ResolvedSymbol
dataDeclSymbol moduleName0 decl =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = SymbolType,
          symbolDefiningModule = moduleName0,
          symbolDefiningName = P.dataDeclName decl,
          symbolOwnerIdentity = Nothing
        }
    )
    (P.dataDeclName decl)
    (P.dataDeclName decl)
    (SymbolLocal moduleName0)

constructorDeclSymbol :: P.ModuleName -> P.DataDecl -> P.ConstructorDecl -> ResolvedSymbol
constructorDeclSymbol moduleName0 dataDecl ctorDecl =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = SymbolConstructor,
          symbolDefiningModule = moduleName0,
          symbolDefiningName = P.constructorDeclName ctorDecl,
          symbolOwnerIdentity = Just (SymbolOwnerType moduleName0 (P.dataDeclName dataDecl))
        }
    )
    (P.constructorDeclName ctorDecl)
    (P.constructorDeclName ctorDecl)
    (SymbolLocal moduleName0)

classDeclSymbol :: P.ModuleName -> P.ClassDecl -> ResolvedSymbol
classDeclSymbol moduleName0 decl =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = SymbolClass,
          symbolDefiningModule = moduleName0,
          symbolDefiningName = P.classDeclName decl,
          symbolOwnerIdentity = Nothing
        }
    )
    (P.classDeclName decl)
    (P.classDeclName decl)
    (SymbolLocal moduleName0)

methodSigSymbol :: P.ModuleName -> P.ClassDecl -> P.MethodSig -> ResolvedSymbol
methodSigSymbol moduleName0 classDecl methodSig =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = SymbolMethod,
          symbolDefiningModule = moduleName0,
          symbolDefiningName = P.methodSigName methodSig,
          symbolOwnerIdentity = Just (SymbolOwnerClass moduleName0 (P.classDeclName classDecl))
        }
    )
    (P.methodSigName methodSig)
    (P.methodSigName methodSig)
    (SymbolLocal moduleName0)

defDeclSymbol :: P.ModuleName -> P.DefDecl -> ResolvedSymbol
defDeclSymbol moduleName0 decl =
  mkResolvedSymbol
    ( SymbolIdentity
        { symbolNamespace = SymbolValue,
          symbolDefiningModule = moduleName0,
          symbolDefiningName = P.defDeclName decl,
          symbolOwnerIdentity = Nothing
        }
    )
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

builtinTypeNames :: Set.Set String
builtinTypeNames = Set.fromList ["Int", "Bool", "String"]

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

toNonEmpty :: [a] -> NonEmpty a
toNonEmpty values =
  case values of
    x : xs -> x :| xs
    [] -> error "internal resolver invariant: STCon has at least one argument"

firstWithRefs :: (a -> b) -> (a, [ResolvedReference]) -> (b, [ResolvedReference])
firstWithRefs f (value, refs) = (f value, refs)

mapAndRefs :: (a -> ResolveM (b, [ResolvedReference])) -> [a] -> ResolveM ([b], [ResolvedReference])
mapAndRefs f values = do
  resolved <- mapM f values
  pure ([value | (value, _) <- resolved], concat [refs | (_, refs) <- resolved])
