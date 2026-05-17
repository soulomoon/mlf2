{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.TypeFamilies
  ( normalizeTypeFamiliesInProgram,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (ProgramError (..))
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Frontend.Syntax
  ( SrcBound (..),
    SrcNorm (..),
    SrcTy (..),
    SrcType,
    mkSrcBound,
  )
import MLF.Frontend.TypeLevel
  ( TypeFamilyDecl (..),
    TypeFamilyEnv,
    TypeFamilyEquation (..),
    TypeLevelPattern (..),
    TypeLevelKind (..),
    TypeLevelNormalizeError (..),
    TypeLevelTy (..),
    familyEnvFromDecls,
    isFamilyFree,
    normalizeTypeLevel,
  )

normalizeTypeFamiliesInProgram :: P.Program -> Either ProgramError P.Program
normalizeTypeFamiliesInProgram (P.Program modules0) = do
  (_, normalizedRev) <- foldM (normalizeModuleWithFamilyScope modules0) (Map.empty, []) modules0
  pure (P.Program (reverse normalizedRev))

newtype FamilyModuleExports = FamilyModuleExports
  { exportedFamilies :: Map.Map String TypeFamilyDecl
  }
  deriving (Eq, Show)

normalizeModuleWithFamilyScope ::
  [P.Module] ->
  (Map.Map P.ModuleName FamilyModuleExports, [P.Module]) ->
  P.Module ->
  Either ProgramError (Map.Map P.ModuleName FamilyModuleExports, [P.Module])
normalizeModuleWithFamilyScope modules0 (priorExports, normalizedRev) mod0 = do
  importedFamilies <- importFamilyScope priorExports (P.moduleImports mod0)
  let rawLocalDecls = moduleTypeFamilyDecls mod0
      localFamilyNames = map familyDeclName rawLocalDecls
      localFamilyNameSet = Set.fromList localFamilyNames
      dataNames = map P.dataDeclName (moduleDataDecls mod0)
  ensureNoDuplicates ProgramDuplicateType localFamilyNames
  ensureDisjointTypes localFamilyNames dataNames
  rawLocalFamilies <- mapFromUnique ProgramDuplicateType [(familyDeclName decl, decl) | decl <- rawLocalDecls]
  rawVisibleFamilies <- mergeFamilyScope importedFamilies rawLocalFamilies
  let visibleFamilyNames = Map.keysSet rawVisibleFamilies
      constructorKinds = typeLevelConstructorKinds modules0 (Map.elems rawVisibleFamilies)
      markedLocalDecls = map (markTypeFamilyDecl visibleFamilyNames) rawLocalDecls
  mapM_ (validateTypeFamilyDecl constructorKinds) markedLocalDecls
  markedLocalFamilies <- mapFromUnique ProgramDuplicateType [(familyDeclName decl, decl) | decl <- markedLocalDecls]
  visibleFamilies <- mergeFamilyScope importedFamilies markedLocalFamilies
  env <- typeFamilyEnvFromMap visibleFamilies
  normalized <- normalizeModule env priorExports localFamilyNameSet mod0
  let exports = FamilyModuleExports (exportedFamilyDecls mod0 markedLocalFamilies)
  pure (Map.insert (P.moduleName mod0) exports priorExports, normalized : normalizedRev)

typeFamilyEnvFromMap :: Map.Map String TypeFamilyDecl -> Either ProgramError TypeFamilyEnv
typeFamilyEnvFromMap families =
  case familyEnvFromDecls (Map.elems families) of
    Right env -> pure env
    Left msg -> Left (ProgramPipelineError msg)

mapFromUnique :: (String -> ProgramError) -> [(String, a)] -> Either ProgramError (Map.Map String a)
mapFromUnique err =
  foldM add Map.empty
  where
    add acc (name, value)
      | Map.member name acc = Left (err name)
      | otherwise = Right (Map.insert name value acc)

mergeFamilyScope ::
  Map.Map String TypeFamilyDecl ->
  Map.Map String TypeFamilyDecl ->
  Either ProgramError (Map.Map String TypeFamilyDecl)
mergeFamilyScope left right =
  foldM add left (Map.toList right)
  where
    add acc (name, decl)
      | Map.member name acc = Left (ProgramDuplicateVisibleName name)
      | otherwise = Right (Map.insert name decl acc)

importFamilyScope ::
  Map.Map P.ModuleName FamilyModuleExports ->
  [P.Import] ->
  Either ProgramError (Map.Map String TypeFamilyDecl)
importFamilyScope priorExports =
  foldM addImport Map.empty
  where
    addImport scope imp = do
      exports <-
        case Map.lookup (P.importModuleName imp) priorExports of
          Just found -> pure found
          Nothing -> Left (ProgramUnknownImportModule (P.importModuleName imp))
      imported <-
        case (P.importAlias imp, P.importExposing imp) of
          (Nothing, Nothing) ->
            pure (exportedFamilies exports)
          (Just _, Nothing) ->
            pure Map.empty
          (_, Just items) ->
            pure $
              Map.fromList
                [ (name, decl)
                | P.ExportType name <- items,
                  Just decl <- [Map.lookup name (exportedFamilies exports)]
                ]
      mergeFamilyScope scope imported

exportedFamilyDecls ::
  P.Module ->
  Map.Map String TypeFamilyDecl ->
  Map.Map String TypeFamilyDecl
exportedFamilyDecls mod0 localFamilies =
  case P.moduleExports mod0 of
    Nothing -> localFamilies
    Just items ->
      Map.fromList
        [ (name, decl)
        | P.ExportType name <- items,
          Just decl <- [Map.lookup name localFamilies]
        ]

typeLevelConstructorKinds :: [P.ModuleF p] -> [TypeFamilyDecl] -> Map.Map String TypeLevelKind
typeLevelConstructorKinds modules0 familyDecls =
  Map.fromList builtinKinds
    `Map.union` Map.fromList dataKinds
    `Map.union` Map.fromList familyKinds
  where
    builtinKinds =
      mapMaybe
        (\name -> (\kind0 -> (name, srcKindToTypeLevelKind kind0)) <$> Builtins.builtinTypeKind name)
        (Set.toList Builtins.builtinTypeNames)
    dataKinds =
      [ (P.dataDeclName dataDecl, dataDeclKind dataDecl)
      | dataDecl <- concatMap moduleDataDecls modules0
      ]
    familyKinds =
      [ (familyDeclName decl, familyDeclKind decl)
      | decl <- familyDecls
      ]

    dataDeclKind dataDecl =
      foldr TLKArrow TLKType (map (srcKindToTypeLevelKind . P.typeParamKind) (P.dataDeclParams dataDecl))

    familyDeclKind decl =
      foldr TLKArrow (familyDeclResultKind decl) (map snd (familyDeclParams decl))

srcKindToTypeLevelKind :: P.SrcKind -> TypeLevelKind
srcKindToTypeLevelKind kind0 =
  case kind0 of
    P.KType -> TLKType
    P.KArrow dom cod -> TLKArrow (srcKindToTypeLevelKind dom) (srcKindToTypeLevelKind cod)

type TypeLevelKindSubst = Map.Map String TypeLevelKind

validateTypeFamilyDecl :: Map.Map String TypeLevelKind -> TypeFamilyDecl -> Either ProgramError ()
validateTypeFamilyDecl constructorKinds decl =
  mapM_ validateEquation (familyDeclEquations decl)
  where
    familyName = familyDeclName decl
    params = familyDeclParams decl
    expectedArity = length params
    resultKind = familyDeclResultKind decl

    validateEquation equation = do
      let patterns = familyEquationPatterns equation
          actualArity = length patterns
      when (expectedArity /= actualArity) $
        Left (ProgramTypeFamilyEquationArityMismatch familyName expectedArity actualArity)
      let typeVars0 = Map.empty
      (typeVars1, subst1) <-
        foldM
          ( \acc (expectedKind, pattern0) ->
              validateTypeLevelPattern familyName constructorKinds expectedKind acc pattern0
          )
          (typeVars0, Map.empty)
          (zip (map snd params) patterns)
      (rhsKind, subst2) <- inferTypeLevelKind familyName constructorKinds typeVars1 subst1 (familyEquationRhs equation)
      _ <- requireTypeLevelKind familyName (familyEquationRhs equation) subst2 resultKind rhsKind
      pure ()

validateTypeLevelPattern ::
  P.TypeName ->
  Map.Map String TypeLevelKind ->
  TypeLevelKind ->
  (Map.Map String TypeLevelKind, TypeLevelKindSubst) ->
  TypeLevelPattern ->
  Either ProgramError (Map.Map String TypeLevelKind, TypeLevelKindSubst)
validateTypeLevelPattern familyName constructorKinds expected (typeVars, subst) pattern0 =
  case pattern0 of
    TLPVar name ->
      case Map.lookup name typeVars of
        Nothing -> Right (Map.insert name (applyKindSubst subst expected) typeVars, subst)
        Just actual -> do
          subst' <- requireTypeLevelKind familyName (TLTVar name) subst expected actual
          Right (typeVars, subst')
    TLPCon name patterns -> do
      headKind <- constructorKind constructorKinds name
      (resultKind, typeVars', subst') <-
        applyPatternKindArgs
          familyName
          constructorKinds
          typeVars
          subst
          (TLTCon name)
          headKind
          patterns
      subst'' <- requireTypeLevelKind familyName (patternToTypeLevel pattern0) subst' expected resultKind
      Right (typeVars', subst'')

applyPatternKindArgs ::
  P.TypeName ->
  Map.Map String TypeLevelKind ->
  Map.Map String TypeLevelKind ->
  TypeLevelKindSubst ->
  TypeLevelTy ->
  TypeLevelKind ->
  [TypeLevelPattern] ->
  Either ProgramError (TypeLevelKind, Map.Map String TypeLevelKind, TypeLevelKindSubst)
applyPatternKindArgs familyName constructorKinds typeVars subst headTy headKind patterns =
  go typeVars subst headKind patterns
  where
    go typeVars0 subst0 kind0 [] = Right (applyKindSubst subst0 kind0, typeVars0, subst0)
    go typeVars0 subst0 kind0 (pattern0 : rest) =
      case applyKindSubst subst0 kind0 of
        TLKArrow expectedArg resultKind -> do
          (typeVars1, subst1) <- validateTypeLevelPattern familyName constructorKinds expectedArg (typeVars0, subst0) pattern0
          go typeVars1 subst1 resultKind rest
        actualKind ->
          Left (ProgramTypeFamilyKindMismatch familyName headTy (TLKArrow TLKType TLKType) actualKind)

patternToTypeLevel :: TypeLevelPattern -> TypeLevelTy
patternToTypeLevel pattern0 =
  case pattern0 of
    TLPVar name -> TLTVar name
    TLPCon name patterns -> foldl TLTApp (TLTCon name) (map patternToTypeLevel patterns)

inferTypeLevelKind ::
  P.TypeName ->
  Map.Map String TypeLevelKind ->
  Map.Map String TypeLevelKind ->
  TypeLevelKindSubst ->
  TypeLevelTy ->
  Either ProgramError (TypeLevelKind, TypeLevelKindSubst)
inferTypeLevelKind familyName constructorKinds typeVars subst ty =
  case collectApps ty of
    (TLTCon name, arg : args) -> do
      kind0 <- constructorKind constructorKinds name
      applyTypeKindArgs familyName constructorKinds typeVars subst (TLTCon name) kind0 (arg : args)
    (TLTVar name, arg : args) -> do
      headKind <- typeVariableKind name typeVars
      applyTypeKindArgs familyName constructorKinds typeVars subst (TLTVar name) headKind (arg : args)
    _ ->
      case ty of
        TLTVar name -> do
          kind0 <- typeVariableKind name typeVars
          pure (kind0, subst)
        TLTCon name -> do
          kind0 <- constructorKind constructorKinds name
          pure (kind0, subst)
        TLTArrow dom cod -> do
          subst1 <- checkTypeLevelKind familyName constructorKinds typeVars subst dom TLKType
          subst2 <- checkTypeLevelKind familyName constructorKinds typeVars subst1 cod TLKType
          pure (TLKType, subst2)
        TLTLam name kind0 body -> do
          let typeVars' = Map.insert name kind0 typeVars
          (bodyKind, subst1) <- inferTypeLevelKind familyName constructorKinds typeVars' subst body
          pure (TLKArrow (applyKindSubst subst1 kind0) bodyKind, subst1)
        TLTApp fun arg -> do
          (funKind, subst1) <- inferTypeLevelKind familyName constructorKinds typeVars subst fun
          applyTypeKindArgs familyName constructorKinds typeVars subst1 fun funKind [arg]
        TLTFamilyApp name args -> do
          kind0 <- constructorKind constructorKinds name
          applyTypeKindArgs familyName constructorKinds typeVars subst (TLTCon name) kind0 args

checkTypeLevelKind ::
  P.TypeName ->
  Map.Map String TypeLevelKind ->
  Map.Map String TypeLevelKind ->
  TypeLevelKindSubst ->
  TypeLevelTy ->
  TypeLevelKind ->
  Either ProgramError TypeLevelKindSubst
checkTypeLevelKind familyName constructorKinds typeVars subst ty expected = do
  (actual, subst1) <- inferTypeLevelKind familyName constructorKinds typeVars subst ty
  requireTypeLevelKind familyName ty subst1 expected actual

applyTypeKindArgs ::
  P.TypeName ->
  Map.Map String TypeLevelKind ->
  Map.Map String TypeLevelKind ->
  TypeLevelKindSubst ->
  TypeLevelTy ->
  TypeLevelKind ->
  [TypeLevelTy] ->
  Either ProgramError (TypeLevelKind, TypeLevelKindSubst)
applyTypeKindArgs familyName constructorKinds typeVars subst headTy headKind args =
  go subst headKind args
  where
    go subst0 kind0 [] = Right (applyKindSubst subst0 kind0, subst0)
    go subst0 kind0 (arg : rest) =
      case applyKindSubst subst0 kind0 of
        TLKArrow expectedArg resultKind -> do
          subst1 <- checkTypeLevelKind familyName constructorKinds typeVars subst0 arg expectedArg
          go subst1 resultKind rest
        actualKind ->
          Left (ProgramTypeFamilyKindMismatch familyName headTy (TLKArrow TLKType TLKType) actualKind)

typeVariableKind :: String -> Map.Map String TypeLevelKind -> Either ProgramError TypeLevelKind
typeVariableKind name typeVars =
  case Map.lookup name typeVars of
    Just kind0 -> Right kind0
    Nothing -> Left (ProgramUnboundTypeFamilyVariable name)

constructorKind :: Map.Map String TypeLevelKind -> String -> Either ProgramError TypeLevelKind
constructorKind constructorKinds name =
  case Map.lookup name constructorKinds of
    Just kind0 -> Right kind0
    Nothing -> Left (ProgramUnknownType name)

requireTypeLevelKind ::
  P.TypeName ->
  TypeLevelTy ->
  TypeLevelKindSubst ->
  TypeLevelKind ->
  TypeLevelKind ->
  Either ProgramError TypeLevelKindSubst
requireTypeLevelKind familyName ty subst expected actual =
  case unifyTypeLevelKind subst expected actual of
    Right subst' -> Right subst'
    Left (expected', actual') -> Left (ProgramTypeFamilyKindMismatch familyName ty expected' actual')

unifyTypeLevelKind ::
  TypeLevelKindSubst ->
  TypeLevelKind ->
  TypeLevelKind ->
  Either (TypeLevelKind, TypeLevelKind) TypeLevelKindSubst
unifyTypeLevelKind subst expected actual =
  case (applyKindSubst subst expected, applyKindSubst subst actual) of
    (TLKType, TLKType) -> Right subst
    (TLKArrow leftDom leftCod, TLKArrow rightDom rightCod) -> do
      subst1 <- unifyTypeLevelKind subst leftDom rightDom
      unifyTypeLevelKind subst1 leftCod rightCod
    (TLKVar name, kind0) -> bindTypeLevelKindVar subst name kind0
    (kind0, TLKVar name) -> bindTypeLevelKindVar subst name kind0
    (expected', actual') -> Left (expected', actual')

bindTypeLevelKindVar ::
  TypeLevelKindSubst ->
  String ->
  TypeLevelKind ->
  Either (TypeLevelKind, TypeLevelKind) TypeLevelKindSubst
bindTypeLevelKindVar subst name kind0 =
  let kind1 = applyKindSubst subst kind0
   in case kind1 of
        TLKVar other
          | other == name -> Right subst
        _
          | typeLevelKindOccurs name kind1 -> Left (TLKVar name, kind1)
          | otherwise -> Right (Map.insert name kind1 subst)

typeLevelKindOccurs :: String -> TypeLevelKind -> Bool
typeLevelKindOccurs name kind0 =
  case kind0 of
    TLKType -> False
    TLKVar other -> name == other
    TLKArrow dom cod -> typeLevelKindOccurs name dom || typeLevelKindOccurs name cod

applyKindSubst :: TypeLevelKindSubst -> TypeLevelKind -> TypeLevelKind
applyKindSubst subst kind0 =
  case kind0 of
    TLKType -> TLKType
    TLKVar name ->
      case Map.lookup name subst of
        Just replacement -> applyKindSubst subst replacement
        Nothing -> TLKVar name
    TLKArrow dom cod -> TLKArrow (applyKindSubst subst dom) (applyKindSubst subst cod)

ensureNoDuplicates :: (String -> ProgramError) -> [String] -> Either ProgramError ()
ensureNoDuplicates err = go Set.empty
  where
    go _ [] = pure ()
    go seen (name : rest)
      | Set.member name seen = Left (err name)
      | otherwise = go (Set.insert name seen) rest

ensureDisjointTypes :: [String] -> [String] -> Either ProgramError ()
ensureDisjointTypes familyNames dataNames =
  case filter (`Set.member` Set.fromList dataNames) familyNames of
    name : _ -> Left (ProgramDuplicateType name)
    [] -> pure ()

moduleTypeFamilyDecls :: P.ModuleF p -> [TypeFamilyDecl]
moduleTypeFamilyDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc =
      case decl of
        P.DeclTypeFamily familyDecl -> familyDecl : acc
        _ -> acc

moduleDataDecls :: P.ModuleF p -> [P.DataDeclF p]
moduleDataDecls = foldr collect [] . P.moduleDecls
  where
    collect decl acc =
      case decl of
        P.DeclData dataDecl -> dataDecl : acc
        _ -> acc

markTypeFamilyDecl :: Set.Set String -> TypeFamilyDecl -> TypeFamilyDecl
markTypeFamilyDecl familyNames decl =
  decl
    { familyDeclEquations =
        [ equation {familyEquationRhs = markFamilyApps familyNames (familyEquationRhs equation)}
          | equation <- familyDeclEquations decl
        ]
    }

markFamilyApps :: Set.Set String -> TypeLevelTy -> TypeLevelTy
markFamilyApps familyNames ty =
  case collectApps ty of
    (TLTCon name, args)
      | Set.member name familyNames ->
          TLTFamilyApp name (map (markFamilyApps familyNames) args)
    _ ->
      case ty of
        TLTVar {} -> ty
        TLTCon {} -> ty
        TLTArrow dom cod -> TLTArrow (go dom) (go cod)
        TLTLam name kind body -> TLTLam name kind (go body)
        TLTApp fun arg -> TLTApp (go fun) (go arg)
        TLTFamilyApp name args -> TLTFamilyApp name (map go args)
  where
    go = markFamilyApps familyNames

normalizeModule ::
  TypeFamilyEnv ->
  Map.Map P.ModuleName FamilyModuleExports ->
  Set.Set String ->
  P.Module ->
  Either ProgramError P.Module
normalizeModule env priorExports localFamilyNames mod0 = do
  decls <- concat <$> traverse (normalizeDecl env) (P.moduleDecls mod0)
  imports <- traverse (stripFamilyImport priorExports) (P.moduleImports mod0)
  let exports = stripFamilyExports localFamilyNames <$> P.moduleExports mod0
  pure mod0 {P.moduleExports = exports, P.moduleImports = imports, P.moduleDecls = decls}

stripFamilyExports :: Set.Set String -> [P.ExportItem] -> [P.ExportItem]
stripFamilyExports localFamilyNames =
  filter (not . isLocalFamilyExport)
  where
    isLocalFamilyExport item =
      case item of
        P.ExportType name -> Set.member name localFamilyNames
        P.ExportTypeWithConstructors name -> Set.member name localFamilyNames
        P.ExportValue {} -> False

stripFamilyImport ::
  Map.Map P.ModuleName FamilyModuleExports ->
  P.Import ->
  Either ProgramError P.Import
stripFamilyImport priorExports imp =
  case P.importExposing imp of
    Nothing -> pure imp
    Just items -> do
      exports <-
        case Map.lookup (P.importModuleName imp) priorExports of
          Just found -> pure found
          Nothing -> Left (ProgramUnknownImportModule (P.importModuleName imp))
      let familyNames = Map.keysSet (exportedFamilies exports)
          keep item =
            case item of
              P.ExportType name -> Set.notMember name familyNames
              P.ExportTypeWithConstructors name -> Set.notMember name familyNames
              P.ExportValue {} -> True
      pure imp {P.importExposing = Just (filter keep items)}

normalizeDecl :: TypeFamilyEnv -> P.Decl -> Either ProgramError [P.Decl]
normalizeDecl env decl =
  case decl of
    P.DeclClass classDecl -> do
      superclasses <- traverse (normalizeClassConstraint env) (P.classDeclSuperclasses classDecl)
      methods <- traverse (normalizeMethodSig env) (P.classDeclMethods classDecl)
      pure
        [ P.DeclClass
            classDecl
              { P.classDeclSuperclasses = superclasses,
                P.classDeclMethods = methods
              }
        ]
    P.DeclInstance instDecl -> do
      constraints <- traverse (normalizeClassConstraint env) (P.instanceDeclConstraints instDecl)
      headTys <- traverse (normalizeSrcType env) (P.instanceDeclTypes instDecl)
      methods <- traverse (normalizeMethodDef env) (P.instanceDeclMethods instDecl)
      pure
        [ P.DeclInstance
            instDecl
              { P.instanceDeclConstraints = constraints,
                P.instanceDeclTypes = headTys,
                P.instanceDeclMethods = methods
              }
        ]
    P.DeclData dataDecl -> do
      constructors <- traverse (normalizeConstructorDecl env) (P.dataDeclConstructors dataDecl)
      pure [P.DeclData dataDecl {P.dataDeclConstructors = constructors}]
    P.DeclTypeFamily {} -> pure []
    P.DeclDef defDecl -> do
      ty <- normalizeConstrainedType env (P.defDeclType defDecl)
      expr <- normalizeExpr env (P.defDeclExpr defDecl)
      pure [P.DeclDef defDecl {P.defDeclType = ty, P.defDeclExpr = expr}]

normalizeMethodSig :: TypeFamilyEnv -> P.MethodSig -> Either ProgramError P.MethodSig
normalizeMethodSig env sig = do
  ty <- normalizeConstrainedType env (P.methodSigType sig)
  pure sig {P.methodSigType = ty}

normalizeMethodDef :: TypeFamilyEnv -> P.MethodDef -> Either ProgramError P.MethodDef
normalizeMethodDef env def = do
  expr <- normalizeExpr env (P.methodDefExpr def)
  pure def {P.methodDefExpr = expr}

normalizeConstructorDecl :: TypeFamilyEnv -> P.ConstructorDecl -> Either ProgramError P.ConstructorDecl
normalizeConstructorDecl env ctor = do
  ty <- normalizeSrcType env (P.constructorDeclType ctor)
  pure ctor {P.constructorDeclType = ty}

normalizeConstrainedType :: TypeFamilyEnv -> P.ConstrainedType -> Either ProgramError P.ConstrainedType
normalizeConstrainedType env ty =
  P.ConstrainedType
    <$> traverse (normalizeClassConstraint env) (P.constrainedConstraints ty)
    <*> normalizeSrcType env (P.constrainedBody ty)

normalizeClassConstraint :: TypeFamilyEnv -> P.ClassConstraint -> Either ProgramError P.ClassConstraint
normalizeClassConstraint env constraint = do
  tys <- traverse (normalizeSrcType env) (P.constraintTypes constraint)
  pure constraint {P.constraintTypes = tys}

normalizeExpr :: TypeFamilyEnv -> P.Expr -> Either ProgramError P.Expr
normalizeExpr env expr =
  case expr of
    P.EVar {} -> pure expr
    P.ELit {} -> pure expr
    P.ELam param body ->
      P.ELam <$> normalizeParam env param <*> normalizeExpr env body
    P.EApp fun arg ->
      P.EApp <$> normalizeExpr env fun <*> normalizeExpr env arg
    P.ELet name mbTy rhs body ->
      P.ELet name
        <$> traverse (normalizeSrcType env) mbTy
        <*> normalizeExpr env rhs
        <*> normalizeExpr env body
    P.EAnn inner ty ->
      P.EAnn <$> normalizeExpr env inner <*> normalizeSrcType env ty
    P.ECase scrutinee alts ->
      P.ECase <$> normalizeExpr env scrutinee <*> traverse (normalizeAlt env) alts

normalizeParam :: TypeFamilyEnv -> P.Param -> Either ProgramError P.Param
normalizeParam env param =
  P.Param (P.paramName param) <$> traverse (normalizeSrcType env) (P.paramType param)

normalizeAlt :: TypeFamilyEnv -> P.Alt -> Either ProgramError P.Alt
normalizeAlt env alt =
  P.Alt <$> normalizePattern env (P.altPattern alt) <*> normalizeExpr env (P.altExpr alt)

normalizePattern :: TypeFamilyEnv -> P.Pattern -> Either ProgramError P.Pattern
normalizePattern env pattern0 =
  case pattern0 of
    P.PatCtor ctor args -> P.PatCtor ctor <$> traverse (normalizePattern env) args
    P.PatVar {} -> pure pattern0
    P.PatWildcard -> pure pattern0
    P.PatAnn inner ty ->
      P.PatAnn <$> normalizePattern env inner <*> normalizeSrcType env ty

normalizeSrcType :: TypeFamilyEnv -> SrcType -> Either ProgramError SrcType
normalizeSrcType env ty =
  case ty of
    STVar {} -> pure ty
    STBase name
      | Map.member name env -> reduceFamilySource env name []
      | otherwise -> pure ty
    STCon name args
      | Map.member name env ->
          reduceFamilySource env name (toList args)
      | otherwise ->
          STCon name <$> traverse (normalizeSrcType env) args
    STVarApp name args -> STVarApp name <$> traverse (normalizeSrcType env) args
    STTyLam name body -> do
      body' <- normalizeSrcType env body
      Left (ProgramResidualTypeLambda (STTyLam name body'))
    STTyApp {} -> do
      tyLevel <- srcTypeToTypeLevel ty
      normalizeTypeLevelSource env ty tyLevel
    STArrow dom cod -> STArrow <$> normalizeSrcType env dom <*> normalizeSrcType env cod
    STForall name mb body ->
      STForall name
        <$> traverse (normalizeSrcBound env) mb
        <*> normalizeSrcType env body
    STMu name body -> STMu name <$> normalizeSrcType env body
    STBottom -> pure STBottom

normalizeSrcBound :: TypeFamilyEnv -> SrcBound 'RawN -> Either ProgramError (SrcBound 'RawN)
normalizeSrcBound env bound =
  mkSrcBound <$> normalizeSrcType env (unSrcBound bound)

reduceFamilySource :: TypeFamilyEnv -> String -> [SrcType] -> Either ProgramError SrcType
reduceFamilySource env name args = do
  argsLevel <- traverse srcTypeToTypeLevel args
  normalizeTypeLevelSource env (familySourceType name args) (TLTFamilyApp name argsLevel)

normalizeTypeLevelSource :: TypeFamilyEnv -> SrcType -> TypeLevelTy -> Either ProgramError SrcType
normalizeTypeLevelSource env original ty = do
  let marked = markFamilyApps (Map.keysSet env) ty
  normalized <-
    case normalizeTypeLevel env marked of
      Right normalizedTy -> pure normalizedTy
      Left err -> Left (typeLevelReductionError original err)
  if isFamilyFree normalized
    then typeLevelToSrcType normalized >>= ensureCoreErasableType
    else Left (typeLevelReductionError original (residualFamilyError normalized))

typeLevelReductionError :: SrcType -> TypeLevelNormalizeError -> ProgramError
typeLevelReductionError original err =
  case err of
    UnknownTypeFamily name -> ProgramTypeFamilyReductionFailed name err
    TypeFamilyArityMismatch name _ _ -> ProgramTypeFamilyReductionFailed name err
    TypeFamilyStuck name _ -> ProgramTypeFamilyReductionFailed name err
    TypeFamilyCycle (name : _) -> ProgramTypeFamilyReductionFailed name err
    TypeFamilyCycle [] -> ProgramTypeLevelReductionFailed original err
    TypeLevelFuelExhausted {} -> ProgramTypeLevelReductionFailed original err

residualFamilyError :: TypeLevelTy -> TypeLevelNormalizeError
residualFamilyError ty =
  case firstFamilyApp ty of
    Just (name, args) -> TypeFamilyStuck name args
    Nothing -> TypeLevelFuelExhausted ty

firstFamilyApp :: TypeLevelTy -> Maybe (String, [TypeLevelTy])
firstFamilyApp ty =
  case ty of
    TLTVar {} -> Nothing
    TLTCon {} -> Nothing
    TLTArrow dom cod -> firstFamilyApp dom <|> firstFamilyApp cod
    TLTLam _ _ body -> firstFamilyApp body
    TLTApp fun arg -> firstFamilyApp fun <|> firstFamilyApp arg
    TLTFamilyApp name args -> Just (name, args)

ensureCoreErasableType :: SrcType -> Either ProgramError SrcType
ensureCoreErasableType ty =
  case ty of
    STVar {} -> pure ty
    STBase {} -> pure ty
    STBottom -> pure ty
    STArrow dom cod -> STArrow <$> ensureCoreErasableType dom <*> ensureCoreErasableType cod
    STCon name args -> STCon name <$> traverse ensureCoreErasableType args
    STVarApp name args -> STVarApp name <$> traverse ensureCoreErasableType args
    STTyLam {} -> Left (ProgramResidualTypeLambda ty)
    STTyApp {} -> Left (ProgramUnsupportedTypeApplication ty)
    STForall name mb body ->
      STForall name
        <$> traverse ensureCoreErasableBound mb
        <*> ensureCoreErasableType body
    STMu name body -> STMu name <$> ensureCoreErasableType body

ensureCoreErasableBound :: SrcBound 'RawN -> Either ProgramError (SrcBound 'RawN)
ensureCoreErasableBound bound =
  mkSrcBound <$> ensureCoreErasableType (unSrcBound bound)

familySourceType :: String -> [SrcType] -> SrcType
familySourceType name [] = STBase name
familySourceType name (arg : args) = STCon name (arg :| args)

srcTypeToTypeLevel :: SrcType -> Either ProgramError TypeLevelTy
srcTypeToTypeLevel ty =
  case ty of
    STVar name -> pure (TLTVar name)
    STBase name -> pure (TLTCon name)
    STCon name args -> foldl TLTApp (TLTCon name) <$> traverse srcTypeToTypeLevel (toList args)
    STVarApp name args -> foldl TLTApp (TLTVar name) <$> traverse srcTypeToTypeLevel (toList args)
    STTyLam name body -> TLTLam name TLKType <$> srcTypeToTypeLevel body
    STTyApp fun arg -> TLTApp <$> srcTypeToTypeLevel fun <*> srcTypeToTypeLevel arg
    STArrow dom cod -> TLTArrow <$> srcTypeToTypeLevel dom <*> srcTypeToTypeLevel cod
    STForall {} -> Left (ProgramUnsupportedTypeFamilyType ty)
    STMu {} -> Left (ProgramUnsupportedTypeFamilyType ty)
    STBottom -> Left (ProgramUnsupportedTypeFamilyType ty)

typeLevelToSrcType :: TypeLevelTy -> Either ProgramError SrcType
typeLevelToSrcType ty =
  case collectApps ty of
    (TLTCon name, arg : args) -> do
      arg' <- typeLevelToSrcType arg
      args' <- traverse typeLevelToSrcType args
      pure (STCon name (arg' :| args'))
    (TLTVar name, arg : args) -> do
      arg' <- typeLevelToSrcType arg
      args' <- traverse typeLevelToSrcType args
      pure (STVarApp name (arg' :| args'))
    _ ->
      case ty of
        TLTVar name -> pure (STVar name)
        TLTCon name -> pure (STBase name)
        TLTArrow dom cod -> STArrow <$> typeLevelToSrcType dom <*> typeLevelToSrcType cod
        TLTLam name _ body -> STTyLam name <$> typeLevelToSrcType body
        TLTApp fun arg -> STTyApp <$> typeLevelToSrcType fun <*> typeLevelToSrcType arg
        TLTFamilyApp name args -> Left (ProgramTypeFamilyReductionFailed name (TypeFamilyStuck name args))

collectApps :: TypeLevelTy -> (TypeLevelTy, [TypeLevelTy])
collectApps = go []
  where
    go args (TLTApp fun arg) = go (arg : args) fun
    go args headTy = (headTy, args)
