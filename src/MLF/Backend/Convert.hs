{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : MLF.Backend.Convert
Description : Convert checked .mlfp programs to typed backend IR

This module is the backend-owned cut from checked `.mlfp` artifacts into the
typed IR from "MLF.Backend.IR". It does not infer or repair programs: inputs
must already have passed the `.mlfp` checker and xMLF typecheck guard, and the
converter reports unsupported checked shapes explicitly.
-}
module MLF.Backend.Convert
  ( BackendConversionError (..),
    convertCheckedProgram,
    convertElabType,
    convertSourceType,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, unless, when, zipWithM)
import Control.Monad.State.Strict (StateT (StateT), get, modify, runStateT)
import Data.List (find, intercalate, sort, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck (Env (..), typeCheckWithEnv)
import MLF.Elab.Types
  ( ElabTerm (..),
    ElabScheme,
    ElabType,
    BoundType,
    Instantiation (..),
    pattern Forall,
    Ty (..),
    TypeCheckError,
    elabToBound,
    tyToElab,
  )
import MLF.Frontend.Program.Elaborate (ElaborateScope, elaborateScopeDataTypes, lowerType, mkElaborateScope)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ConstructorInfo (..),
    DataInfo (..),
    ResolvedModule (..),
    ResolvedProgram (..),
    ResolvedScope (..),
    ResolvedSymbol (..),
    SymbolIdentity (..),
  )
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType)

data BackendConversionError
  = BackendUnsupportedSourceType SrcType
  | BackendUnsupportedInstantiation Instantiation
  | BackendUnsupportedRecursiveLet String
  | BackendUnsupportedCaseShape String
  | BackendTypeCheckFailed ElabTerm TypeCheckError
  | BackendValidationFailed BackendValidationError
  deriving (Eq, Show)

data ConvertContext = ConvertContext
  { ccModuleScopes :: Map String ElaborateScope,
    ccConstructors :: Map String ConstructorMeta,
    ccBindingData :: Map String DataMeta,
    ccData :: [DataMeta],
    ccGlobalTerms :: Set.Set String,
    ccCurrentModuleName :: Maybe String,
    ccCurrentBindingName :: String
  }

data ConstructorMeta = ConstructorMeta
  { cmInfo :: ConstructorInfo,
    cmBackend :: BackendConstructor,
    cmData :: DataMeta
  }

data DataMeta = DataMeta
  { dmInfo :: DataInfo,
    dmBackend :: BackendData
  }

data ConstructorApplication = ConstructorApplication ConstructorMeta [BackendType] [ElabTerm]

type BackendParameterBounds = Map String (Maybe BackendType)

type BackendTypeBounds = Map String (Maybe BackendType)

data BackendTypeAbsBinder = BackendTypeAbsBinder String (Maybe BackendType)

data LiftedRecursiveLet = LiftedRecursiveLet
  { lrlName :: String,
    lrlElabType :: ElabType,
    lrlBackendType :: BackendType,
    lrlTerm :: ElabTerm
  }

data LiftState = LiftState
  { lsNextHelperIndex :: Int,
    lsLiftedRecursiveLets :: [LiftedRecursiveLet],
    lsGeneratedHelperNames :: Set.Set String
  }

type LiftM = StateT LiftState (Either BackendConversionError)

convertCheckedProgram :: CheckedProgram -> Either BackendConversionError BackendProgram
convertCheckedProgram checked = do
  context <- buildConvertContext checked
  initialEnv <- buildInitialEnv context checked
  modules0 <- mapM (convertCheckedModule context initialEnv) (checkedProgramModules checked)
  let program =
        BackendProgram
          { backendProgramModules = modules0,
            backendProgramMain = checkedProgramMain checked
          }
  case validateBackendProgram program of
    Right () -> Right program
    Left err -> Left (BackendValidationFailed err)

buildInitialEnv :: ConvertContext -> CheckedProgram -> Either BackendConversionError Env
buildInitialEnv context checked = do
  terms <-
    forM
      [ (checkedModule, binding)
        | checkedModule <- checkedProgramModules checked,
          binding <- checkedModuleBindings checkedModule
      ]
      ( \(checkedModule, binding) -> do
          bindingTy <- checkedBindingCanonicalType context checkedModule binding
          Right (checkedBindingName binding, bindingTy)
      )
  Right
    Env
      { termEnv = Map.fromList terms `Map.union` backendBuiltinTermTypes,
        typeEnv = Map.empty
      }

backendBuiltinTermTypes :: Map String ElabType
backendBuiltinTermTypes =
  Map.fromList
    [ ( "__mlfp_and",
        TArrow
          (TBase (BaseTy "Bool"))
          (TArrow (TBase (BaseTy "Bool")) (TBase (BaseTy "Bool")))
      )
    ]

convertCheckedModule :: ConvertContext -> Env -> CheckedModule -> Either BackendConversionError BackendModule
convertCheckedModule context env checkedModule = do
  dataDecls <- mapM (convertDataInfo context) (Map.elems (checkedModuleData checkedModule))
  bindings <- concat <$> mapM (convertCheckedBinding context env checkedModule) (checkedModuleBindings checkedModule)
  Right
    BackendModule
      { backendModuleName = checkedModuleName checkedModule,
        backendModuleData = dataDecls,
        backendModuleBindings = bindings
      }

convertCheckedBinding :: ConvertContext -> Env -> CheckedModule -> CheckedBinding -> Either BackendConversionError [BackendBinding]
convertCheckedBinding context env checkedModule binding = do
  let bindingContext =
        context
          { ccCurrentModuleName = Just (checkedModuleName checkedModule),
            ccCurrentBindingName = checkedBindingName binding
          }
  canonicalElabTy <- checkedBindingCanonicalType context checkedModule binding
  bindingTy <- checkedBindingCanonicalBackendType context checkedModule binding canonicalElabTy
  (expr, liftedBindings) <-
    case Map.lookup (checkedBindingName binding) (ccConstructors context) of
      Just constructorMeta
        | constructorBindingResultMatches bindingTy constructorMeta ->
            do
              expr <- synthesizeConstructorBinding bindingTy constructorMeta
              Right (expr, [])
      _ -> do
        (liftedTerm, liftedSpecs) <- liftRecursiveLetsInBinding bindingContext (checkedBindingTerm binding)
        let envWithLifted =
              foldr
                (\lifted acc -> extendTermEnv (lrlName lifted) (lrlElabType lifted) acc)
                env
                liftedSpecs
        liftedBindings <- mapM (convertLiftedRecursiveLet bindingContext envWithLifted) liftedSpecs
        expr <- convertTermExpected bindingContext envWithLifted (Just bindingTy) liftedTerm
        Right (expr, liftedBindings)
  let convertedBinding =
        BackendBinding
          { backendBindingName = checkedBindingName binding,
            backendBindingType = bindingTy,
            backendBindingExpr = expr,
            backendBindingExportedAsMain = checkedBindingExportedAsMain binding
          }
  Right (convertedBinding : liftedBindings)

liftRecursiveLetsInBinding :: ConvertContext -> ElabTerm -> Either BackendConversionError (ElabTerm, [LiftedRecursiveLet])
liftRecursiveLetsInBinding context term = do
  (term', state') <-
    runStateT
      (liftRecursiveLetsInTerm context Set.empty term)
      LiftState
        { lsNextHelperIndex = 0,
          lsLiftedRecursiveLets = [],
          lsGeneratedHelperNames = Set.empty
        }
  Right (term', lsLiftedRecursiveLets state')

convertLiftedRecursiveLet :: ConvertContext -> Env -> LiftedRecursiveLet -> Either BackendConversionError BackendBinding
convertLiftedRecursiveLet context env lifted = do
  expr <- convertTermExpected context env (Just (lrlBackendType lifted)) (lrlTerm lifted)
  Right
    BackendBinding
      { backendBindingName = lrlName lifted,
        backendBindingType = lrlBackendType lifted,
        backendBindingExpr = expr,
        backendBindingExportedAsMain = False
      }

liftRecursiveLetsInTerm :: ConvertContext -> Set.Set String -> ElabTerm -> LiftM ElabTerm
liftRecursiveLetsInTerm context lexicalLocals term =
  case term of
    EVar {} ->
      pure term
    ELit {} ->
      pure term
    ELam name ty body ->
      ELam name ty <$> liftRecursiveLetsInTerm context (Set.insert name lexicalLocals) body
    EApp fun arg ->
      EApp
        <$> liftRecursiveLetsInTerm context lexicalLocals fun
        <*> liftRecursiveLetsInTerm context lexicalLocals arg
    ELet name scheme rhs body -> do
      let schemeTy = schemeToType scheme
          bodyLocals = Set.insert name lexicalLocals
      if termMentionsFreeVariable name rhs
        then do
          bindingTy0 <- liftEitherConversion (convertElabType schemeTy)
          let bindingTy = normalizeBackendTypeForContext context bindingTy0
          ensureLiftableRecursiveLet lexicalLocals name bindingTy rhs
          helperName <- freshLiftedRecursiveLetName context name
          rhs' <- liftRecursiveLetsInTerm context (Set.insert name lexicalLocals) rhs
          let helperTerm = renameFreeTermVariable name helperName rhs'
          emitLiftedRecursiveLet
            LiftedRecursiveLet
              { lrlName = helperName,
                lrlElabType = schemeTy,
                lrlBackendType = bindingTy,
                lrlTerm = helperTerm
              }
          body' <- liftRecursiveLetsInTerm context bodyLocals body
          pure (ELet name scheme (EVar helperName) body')
        else
          ELet name scheme
            <$> liftRecursiveLetsInTerm context lexicalLocals rhs
            <*> liftRecursiveLetsInTerm context bodyLocals body
    ETyAbs name mbBound body ->
      ETyAbs name mbBound <$> liftRecursiveLetsInTerm context lexicalLocals body
    ETyInst inner inst ->
      ETyInst <$> liftRecursiveLetsInTerm context lexicalLocals inner <*> pure inst
    ERoll ty body ->
      ERoll ty <$> liftRecursiveLetsInTerm context lexicalLocals body
    EUnroll body ->
      EUnroll <$> liftRecursiveLetsInTerm context lexicalLocals body

ensureLiftableRecursiveLet :: Set.Set String -> String -> BackendType -> ElabTerm -> LiftM ()
ensureLiftableRecursiveLet lexicalLocals name bindingTy rhs = do
  let captures =
        Set.toList $
          freeTermVariables rhs
            `Set.intersection` lexicalLocals
      unsupported reason =
        BackendUnsupportedRecursiveLet
          ( name
              ++ " ("
              ++ reason
              ++ ")"
          )
  unless (null captures) $
    throwLiftError
      ( unsupported
          ("captures lexical bindings: " ++ intercalate ", " captures)
      )
  unless (isMonomorphicFirstOrderFunctionType bindingTy) $
    throwLiftError (unsupported "expected a monomorphic first-order function type")
  unless (isFunctionValueTerm rhs) $
    throwLiftError (unsupported "expected a function-valued recursive right-hand side")

freshLiftedRecursiveLetName :: ConvertContext -> String -> LiftM String
freshLiftedRecursiveLetName context localName = do
  state0 <- get
  let (name, nextIndex) = pickName (lsNextHelperIndex state0)
  modify
    ( \state1 ->
        state1
          { lsNextHelperIndex = nextIndex,
            lsGeneratedHelperNames = Set.insert name (lsGeneratedHelperNames state1)
          }
    )
  pure name
  where
    pickName index0 =
      let candidate =
            ccCurrentBindingName context
              ++ "$letrec$"
              ++ localName
              ++ "$"
              ++ show index0
       in if Set.member candidate (ccGlobalTerms context)
            then pickName (index0 + 1)
            else (candidate, index0 + 1)

emitLiftedRecursiveLet :: LiftedRecursiveLet -> LiftM ()
emitLiftedRecursiveLet lifted =
  modify
    ( \state0 ->
        state0
          { lsLiftedRecursiveLets = lsLiftedRecursiveLets state0 ++ [lifted]
          }
    )

liftEitherConversion :: Either BackendConversionError a -> LiftM a
liftEitherConversion result =
  StateT $ \state0 ->
    case result of
      Right value -> Right (value, state0)
      Left err -> Left err

throwLiftError :: BackendConversionError -> LiftM a
throwLiftError err =
  StateT (const (Left err))

isMonomorphicFirstOrderFunctionType :: BackendType -> Bool
isMonomorphicFirstOrderFunctionType ty =
  case ty of
    BTForall {} ->
      False
    _ ->
      let (args, resultTy) = splitBackendArrows ty
       in not (null args) && all (isFirstOrderValueType Set.empty) (resultTy : args)

isFirstOrderValueType :: Set.Set String -> BackendType -> Bool
isFirstOrderValueType bound =
  \case
    BTVar name ->
      Set.member name bound
    BTArrow {} ->
      False
    BTBase {} ->
      True
    BTCon _ args ->
      all (isFirstOrderValueType bound) args
    BTVarApp name args ->
      Set.member name bound && all (isFirstOrderValueType bound) args
    BTForall {} ->
      False
    BTMu {} ->
      True
    BTBottom ->
      False

isFunctionValueTerm :: ElabTerm -> Bool
isFunctionValueTerm term =
  case stripAdministrativeTermWrappers term of
    ELam {} -> True
    _ -> False

stripAdministrativeTermWrappers :: ElabTerm -> ElabTerm
stripAdministrativeTermWrappers =
  \case
    ETyAbs _ _ body -> stripAdministrativeTermWrappers body
    ETyInst inner _ -> stripAdministrativeTermWrappers inner
    ERoll _ body -> stripAdministrativeTermWrappers body
    term -> term

freeTermVariables :: ElabTerm -> Set.Set String
freeTermVariables =
  go Set.empty
  where
    go bound =
      \case
        EVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        ELit {} ->
          Set.empty
        ELam name _ body ->
          go (Set.insert name bound) body
        EApp fun arg ->
          go bound fun `Set.union` go bound arg
        ELet name _ rhs body ->
          go (Set.insert name bound) rhs `Set.union` go (Set.insert name bound) body
        ETyAbs _ _ body ->
          go bound body
        ETyInst inner _ ->
          go bound inner
        ERoll _ body ->
          go bound body
        EUnroll body ->
          go bound body

renameFreeTermVariable :: String -> String -> ElabTerm -> ElabTerm
renameFreeTermVariable needle replacement =
  go Set.empty
  where
    renameName bound name
      | name == needle && Set.notMember name bound = replacement
      | otherwise = name

    go bound =
      \case
        EVar name ->
          EVar (renameName bound name)
        ELit lit ->
          ELit lit
        ELam name ty body ->
          ELam name ty (go (Set.insert name bound) body)
        EApp fun arg ->
          EApp (go bound fun) (go bound arg)
        ELet name scheme rhs body
          | name == needle ->
              ELet name scheme rhs body
          | otherwise ->
              ELet name scheme (go bound rhs) (go (Set.insert name bound) body)
        ETyAbs name mbBound body ->
          ETyAbs name mbBound (go bound body)
        ETyInst inner inst ->
          ETyInst (go bound inner) inst
        ERoll ty body ->
          ERoll ty (go bound body)
        EUnroll body ->
          EUnroll (go bound body)

checkedBindingCanonicalType :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingCanonicalType context checkedModule binding = do
  let checkedTy = normalizeBuiltinElabType (checkedBindingType binding)
      scope = scopeForModule context (checkedModuleName checkedModule)
  checkedBackendTy <- convertElabType checkedTy
  case sourceTypeToElabType (lowerType scope (checkedBindingSourceType binding)) of
    Right canonicalTy -> do
      let canonicalTy' = normalizeBuiltinElabType canonicalTy
      canonicalBackendTy <- convertElabType canonicalTy'
      if alphaEqBackendType checkedBackendTy canonicalBackendTy
        then Right canonicalTy'
        else
          case (checkedBackendTy, canonicalBackendTy) of
            (BTVar {}, BTVar {}) -> Right checkedTy
            (BTVar {}, _) -> Right canonicalTy'
            _ -> Right checkedTy
    Left _ -> Right checkedTy

checkedBindingCanonicalBackendType :: ConvertContext -> CheckedModule -> CheckedBinding -> ElabType -> Either BackendConversionError BackendType
checkedBindingCanonicalBackendType context checkedModule binding fallbackTy =
  do
    fallbackBackendTy0 <- convertElabType fallbackTy
    let fallbackBackendTy = canonicalizeStructuralMuNames moduleContext fallbackBackendTy0
    let recoveredFallbackTy = recoverStructuralBackendType moduleContext fallbackBackendTy
    case convertLoweredSourceType scope (checkedBindingSourceType binding) of
      Right sourceTy0 ->
        let sourceTy = canonicalizeStructuralMuNames moduleContext sourceTy0
            recoveredSourceTy = recoverStructuralBackendType moduleContext sourceTy
         in if backendTypeNeedsStructuralRecovery moduleContext sourceTy
                || backendTypeNeedsStructuralRecovery moduleContext recoveredSourceTy
                || backendTypeNeedsStructuralRecovery moduleContext fallbackBackendTy
                || backendTypeNeedsStructuralRecovery moduleContext recoveredFallbackTy
                then Right recoveredSourceTy
                else Right fallbackBackendTy
      Left _ -> Right recoveredFallbackTy
  where
    scope = scopeForModule context (checkedModuleName checkedModule)
    moduleContext = context {ccCurrentModuleName = Just (checkedModuleName checkedModule)}

normalizeBackendTypeForContext :: ConvertContext -> BackendType -> BackendType
normalizeBackendTypeForContext context ty =
  let canonicalTy = canonicalizeStructuralMuNames context ty
   in if backendTypeNeedsStructuralRecovery context canonicalTy
        then recoverStructuralBackendType context canonicalTy
        else canonicalTy

scopeForModule :: ConvertContext -> String -> ElaborateScope
scopeForModule context moduleName =
  Map.findWithDefault
    (fallbackElaborateScope (map dmInfo (ccData context)))
    moduleName
    (ccModuleScopes context)

fallbackElaborateScope :: [DataInfo] -> ElaborateScope
fallbackElaborateScope dataInfos =
  mkElaborateScope Map.empty (qualifiedDataInfoMap dataInfos) Map.empty []

sourceTypeToElabType :: SrcTy n v -> Either BackendConversionError ElabType
sourceTypeToElabType =
  \case
    STVar name -> Right (TVar name)
    STArrow dom cod -> TArrow <$> sourceTypeToElabType dom <*> sourceTypeToElabType cod
    STBase name -> Right (TBase (BaseTy name))
    STCon name args -> TCon (BaseTy name) <$> traverse sourceTypeToElabType args
    STVarApp name _ -> Left (BackendUnsupportedCaseShape ("unsupported variable-headed source type application `" ++ name ++ "`"))
    STForall name mb body ->
      TForall name
        <$> maybe (Right Nothing) sourceBoundToElabBound mb
        <*> sourceTypeToElabType body
    STMu name body -> TMu name <$> sourceTypeToElabType body
    STBottom -> Right TBottom

sourceBoundToElabBound :: SrcBound n -> Either BackendConversionError (Maybe BoundType)
sourceBoundToElabBound (SrcBound boundTy) =
  case sourceTypeToElabType boundTy of
    Right (TVar {}) -> Right Nothing
    Right TBottom -> Right Nothing
    Right (TArrow dom cod) -> Right (Just (TArrow dom cod))
    Right (TBase base) -> Right (Just (TBase base))
    Right (TCon con args) -> Right (Just (TCon con args))
    Right (TForall name mb body) -> Right (Just (TForall name mb body))
    Right (TMu name body) -> Right (Just (TMu name body))
    Left err -> Left err

constructorBindingResultMatches :: BackendType -> ConstructorMeta -> Bool
constructorBindingResultMatches bindingTy constructorMeta =
  case matchBackendTypeParameters Map.empty dataParameters parameters Map.empty (backendConstructorResult constructor) resultTy of
    Just _ -> True
    Nothing -> False
  where
    constructor = cmBackend constructorMeta
    dataParameters = constructorDataParameters constructorMeta
    parameters = constructorTypeParameters constructorMeta
    (_, bodyTy) = splitBackendForalls bindingTy
    (_, resultTy) = splitBackendArrows bodyTy

synthesizeConstructorBinding :: BackendType -> ConstructorMeta -> Either BackendConversionError BackendExpr
synthesizeConstructorBinding bindingTy constructorMeta = do
  let constructor = cmBackend constructorMeta
      (typeBinders, bodyTy) = splitBackendForalls bindingTy
      (argTys, resultTy) = splitBackendArrows bodyTy
      fields = backendConstructorFields constructor
  unless (length argTys == length fields) $
    Left
      ( BackendUnsupportedCaseShape
          ("constructor binding arity does not match metadata for `" ++ backendConstructorName constructor ++ "`")
      )
  let argNames = ["$" ++ backendConstructorName constructor ++ "_arg" ++ show ix | ix <- [1 .. length argTys]]
      argExprs = zipWith BackendVar argTys argNames
      constructExpr =
        BackendConstruct
          { backendExprType = resultTy,
            backendConstructName = backendConstructorName constructor,
            backendConstructArgs = argExprs
          }
      expr =
        wrapBackendTypeAbs typeBinders $
          wrapBackendLams (zip argNames argTys) constructExpr
  unless (alphaEqBackendType (backendExprType expr) bindingTy) $
    Left
      ( BackendUnsupportedCaseShape
          ("synthesized constructor binding type does not match checked binding type for `" ++ backendConstructorName constructor ++ "`")
      )
  Right expr

splitBackendForalls :: BackendType -> ([BackendTypeAbsBinder], BackendType)
splitBackendForalls =
  go []
  where
    go binders ty =
      case ty of
        BTForall name mbBound body -> go (binders ++ [BackendTypeAbsBinder name mbBound]) body
        _ -> (binders, ty)

splitBackendArrows :: BackendType -> ([BackendType], BackendType)
splitBackendArrows =
  go []
  where
    go args ty =
      case ty of
        BTArrow arg result -> go (args ++ [arg]) result
        _ -> (args, ty)

wrapBackendTypeAbs :: [BackendTypeAbsBinder] -> BackendExpr -> BackendExpr
wrapBackendTypeAbs binders body =
  foldr wrap body binders
  where
    wrap (BackendTypeAbsBinder name mbBound) expr =
      BackendTyAbs
        { backendExprType = BTForall name mbBound (backendExprType expr),
          backendTyParamName = name,
          backendTyParamBound = mbBound,
          backendTyAbsBody = expr
        }

wrapBackendLams :: [(String, BackendType)] -> BackendExpr -> BackendExpr
wrapBackendLams params body =
  foldr wrap body params
  where
    wrap (name, paramTy) expr =
      BackendLam
        { backendExprType = BTArrow paramTy (backendExprType expr),
          backendParamName = name,
          backendParamType = paramTy,
          backendBody = expr
        }

buildConvertContext :: CheckedProgram -> Either BackendConversionError ConvertContext
buildConvertContext checked = do
  let dataInfos = allDataInfos checked
      dataByIdentity = dataInfoIdentityMap dataInfos
      moduleScopes = moduleElaborateScopes checked dataByIdentity
  dataMetas <- mapM (buildDataMetaForDataInfo moduleScopes dataInfos) dataInfos
  let constructorMetas =
        [ (backendConstructorName (cmBackend constructorMeta), constructorMeta)
          | dataMeta <- dataMetas,
            constructorMeta <- constructorMetasForData dataMeta
        ]
      bindingData = bindingDataHints dataMetas checked
  Right
    ConvertContext
      { ccModuleScopes = moduleScopes,
        ccConstructors = Map.fromList constructorMetas,
        ccBindingData = bindingData,
        ccData = dataMetas,
        ccGlobalTerms = checkedProgramGlobalTerms checked,
        ccCurrentModuleName = Nothing,
        ccCurrentBindingName = ""
      }

allDataInfos :: CheckedProgram -> [DataInfo]
allDataInfos checked =
  [ dataInfo
    | checkedModule <- checkedProgramModules checked,
      dataInfo <- Map.elems (checkedModuleData checkedModule)
  ]

checkedProgramGlobalTerms :: CheckedProgram -> Set.Set String
checkedProgramGlobalTerms checked =
  Set.fromList (Map.keys backendBuiltinTermTypes)
    `Set.union` Set.fromList
      [ checkedBindingName binding
        | checkedModule <- checkedProgramModules checked,
          binding <- checkedModuleBindings checkedModule
      ]

dataInfoIdentityMap :: [DataInfo] -> Map SymbolIdentity DataInfo
dataInfoIdentityMap dataInfos =
  Map.fromList [(dataInfoSymbol info, info) | info <- dataInfos]

moduleElaborateScopes :: CheckedProgram -> Map SymbolIdentity DataInfo -> Map String ElaborateScope
moduleElaborateScopes checked dataByIdentity =
  Map.fromList
    [ (resolvedModuleName resolvedModule, elaborateScopeForResolvedModule dataByIdentity resolvedModule)
      | resolvedModule <- resolvedProgramModules (checkedProgramResolved checked)
    ]

elaborateScopeForResolvedModule :: Map SymbolIdentity DataInfo -> ResolvedModule -> ElaborateScope
elaborateScopeForResolvedModule dataByIdentity resolvedModule =
  mkElaborateScope Map.empty dataTypes Map.empty []
  where
    dataTypes =
      visibleDataInfoMap dataByIdentity (resolvedScopeTypes (resolvedModuleScope resolvedModule))
        `Map.union` qualifiedDataInfoMap (Map.elems dataByIdentity)

visibleDataInfoMap :: Map SymbolIdentity DataInfo -> Map String ResolvedSymbol -> Map String DataInfo
visibleDataInfoMap dataByIdentity =
  Map.mapMaybe (\symbol -> canonicalDataInfo <$> Map.lookup (resolvedSymbolIdentity symbol) dataByIdentity)

qualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
qualifiedDataInfoMap dataInfos =
  Map.fromList [(qualifiedDataName info, canonicalDataInfo info) | info <- dataInfos]

fallbackElaborateScopeForDataInfo :: [DataInfo] -> DataInfo -> ElaborateScope
fallbackElaborateScopeForDataInfo dataInfos info =
  mkElaborateScope Map.empty dataTypes Map.empty []
  where
    dataTypes =
      localDataInfoMap dataInfos info
        `Map.union` uniqueUnqualifiedDataInfoMap dataInfos
        `Map.union` qualifiedDataInfoMap dataInfos

localDataInfoMap :: [DataInfo] -> DataInfo -> Map String DataInfo
localDataInfoMap dataInfos info =
  Map.fromList
    [ (dataName candidate, canonicalDataInfo candidate)
      | candidate <- dataInfos,
        dataModule candidate == dataModule info
    ]

uniqueUnqualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
uniqueUnqualifiedDataInfoMap dataInfos =
  Map.fromList
    [ (name, canonicalDataInfo info)
      | (name, infos) <- Map.toList grouped,
        [info] <- [infos]
    ]
  where
    grouped =
      Map.fromListWith (++)
        [ (dataName info, [info])
          | info <- dataInfos
        ]

canonicalDataInfo :: DataInfo -> DataInfo
canonicalDataInfo info =
  info {dataName = qualifiedDataName info}

bindingDataHints :: [DataMeta] -> CheckedProgram -> Map String DataMeta
bindingDataHints dataMetas checked =
  Map.fromList
    [ (checkedBindingName binding, dataMeta)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        Just dataMeta <- [bindingDataHint dataMetas binding]
    ]

bindingDataHint :: [DataMeta] -> CheckedBinding -> Maybe DataMeta
bindingDataHint dataMetas binding =
  case splitSourceArrows (dropSourceForalls (checkedBindingSourceType binding)) of
    ([], resultTy) -> sourceTypeDataMeta dataMetas resultTy
    _ -> Nothing

sourceTypeDataMeta :: [DataMeta] -> SrcType -> Maybe DataMeta
sourceTypeDataMeta dataMetas ty =
  sourceTypeDataHead ty >>= \name ->
    find (\dataMeta -> backendDataName (dmBackend dataMeta) == name) dataMetas

sourceTypeDataHead :: SrcType -> Maybe String
sourceTypeDataHead =
  \case
    STBase name -> Just name
    STCon name _ -> Just name
    _ -> Nothing

dropSourceForalls :: SrcType -> SrcType
dropSourceForalls =
  \case
    STForall _ _ body -> dropSourceForalls body
    ty -> ty

splitSourceArrows :: SrcType -> ([SrcType], SrcType)
splitSourceArrows =
  go []
  where
    go args ty =
      case ty of
        STArrow arg result -> go (args ++ [arg]) result
        _ -> (args, ty)

buildDataMetaForDataInfo :: Map String ElaborateScope -> [DataInfo] -> DataInfo -> Either BackendConversionError DataMeta
buildDataMetaForDataInfo moduleScopes dataInfos info =
  buildDataMeta (elaborateScopeForDataInfo moduleScopes dataInfos info) info

elaborateScopeForDataInfo :: Map String ElaborateScope -> [DataInfo] -> DataInfo -> ElaborateScope
elaborateScopeForDataInfo moduleScopes dataInfos info =
  Map.findWithDefault
    (fallbackElaborateScopeForDataInfo dataInfos info)
    (dataModule info)
    moduleScopes

qualifiedDataName :: DataInfo -> String
qualifiedDataName info =
  symbolDefiningModule (dataInfoSymbol info) ++ "." ++ symbolDefiningName (dataInfoSymbol info)

buildDataMeta :: ElaborateScope -> DataInfo -> Either BackendConversionError DataMeta
buildDataMeta scope info = do
  rawConstructors <- mapM (convertConstructorInfo scope) (dataConstructors info)
  let rawData =
        BackendData
          { backendDataName = qualifiedDataName info,
            backendDataParameters = dataParams info,
            backendDataConstructors = rawConstructors
          }
      rawMeta =
        DataMeta
          { dmInfo = info,
            dmBackend = rawData
          }
      rawRecoveryContext =
        ConvertContext
          { ccModuleScopes = Map.empty,
            ccConstructors = Map.empty,
            ccBindingData = Map.empty,
            ccData = [rawMeta],
            ccGlobalTerms = Set.empty,
            ccCurrentModuleName = Just (dataModule info),
            ccCurrentBindingName = ""
          }
      canonicalConstructors =
        map (canonicalizeBackendConstructorTypes rawRecoveryContext) rawConstructors
      canonicalData =
        rawData {backendDataConstructors = canonicalConstructors}
      canonicalMeta =
        rawMeta {dmBackend = canonicalData}
      recoveryContext =
        rawRecoveryContext {ccData = [canonicalMeta]}
      constructors =
        if any backendConstructorContainsVarApp rawConstructors
          then map (recoverBackendConstructorTypes recoveryContext) canonicalConstructors
          else canonicalConstructors
  Right
    DataMeta
      { dmInfo = info,
        dmBackend =
          BackendData
            { backendDataName = qualifiedDataName info,
              backendDataParameters = dataParams info,
              backendDataConstructors = constructors
            }
      }

canonicalizeBackendConstructorTypes :: ConvertContext -> BackendConstructor -> BackendConstructor
canonicalizeBackendConstructorTypes context constructor =
  constructor
    { backendConstructorForalls = map canonicalizeTypeBinder (backendConstructorForalls constructor),
      backendConstructorFields = map canonicalizeTy (backendConstructorFields constructor),
      backendConstructorResult = canonicalizeTy (backendConstructorResult constructor)
    }
  where
    canonicalizeTy =
      canonicalizeStructuralMuNames context

    canonicalizeTypeBinder binder =
      binder {backendTypeBinderBound = fmap canonicalizeTy (backendTypeBinderBound binder)}

recoverBackendConstructorTypes :: ConvertContext -> BackendConstructor -> BackendConstructor
recoverBackendConstructorTypes context constructor =
  constructor
    { backendConstructorForalls = map recoverTypeBinder (backendConstructorForalls constructor),
      backendConstructorFields = map (recoverStructuralBackendType context) (backendConstructorFields constructor),
      backendConstructorResult = recoverStructuralBackendType context (backendConstructorResult constructor)
    }
  where
    recoverTypeBinder binder =
      binder {backendTypeBinderBound = fmap (recoverStructuralBackendType context) (backendTypeBinderBound binder)}

backendConstructorContainsVarApp :: BackendConstructor -> Bool
backendConstructorContainsVarApp constructor =
  any backendTypeContainsVarApp (backendConstructorFields constructor)
    || backendTypeContainsVarApp (backendConstructorResult constructor)
    || any (maybe False backendTypeContainsVarApp . backendTypeBinderBound) (backendConstructorForalls constructor)

backendTypeContainsVarApp :: BackendType -> Bool
backendTypeContainsVarApp =
  \case
    BTVar {} -> False
    BTArrow dom cod -> backendTypeContainsVarApp dom || backendTypeContainsVarApp cod
    BTBase {} -> False
    BTCon _ args -> any backendTypeContainsVarApp args
    BTVarApp {} -> True
    BTForall _ mb body -> maybe False backendTypeContainsVarApp mb || backendTypeContainsVarApp body
    BTMu _ body -> backendTypeContainsVarApp body
    BTBottom -> False

backendTypeNeedsStructuralRecovery :: ConvertContext -> BackendType -> Bool
backendTypeNeedsStructuralRecovery context =
  \case
    BTVar {} -> False
    BTArrow dom cod -> backendTypeNeedsStructuralRecovery context dom || backendTypeNeedsStructuralRecovery context cod
    BTBase {} -> False
    BTCon _ args -> any (backendTypeNeedsStructuralRecovery context) args
    BTVarApp {} -> True
    BTForall _ mb body -> maybe False (backendTypeNeedsStructuralRecovery context) mb || backendTypeNeedsStructuralRecovery context body
    BTMu name body ->
      maybe False dataMetaNeedsStructuralRecovery (structuralRecursiveDataMeta context name)
        || backendTypeNeedsStructuralRecovery context body
    BTBottom -> False

dataMetaNeedsStructuralRecovery :: DataMeta -> Bool
dataMetaNeedsStructuralRecovery dataMeta =
  any backendConstructorContainsVarApp (backendDataConstructors (dmBackend dataMeta))

contextForDataMeta :: ConvertContext -> DataMeta -> ConvertContext
contextForDataMeta context dataMeta =
  context {ccCurrentModuleName = Just (dataModule (dmInfo dataMeta))}

constructorMetasForData :: DataMeta -> [ConstructorMeta]
constructorMetasForData dataMeta =
  [ ConstructorMeta
      { cmInfo = ctorInfo,
        cmBackend = backendCtor,
        cmData = dataMeta
      }
    | (ctorInfo, backendCtor) <- zip (dataConstructors (dmInfo dataMeta)) (backendDataConstructors (dmBackend dataMeta))
  ]

convertDataInfo :: ConvertContext -> DataInfo -> Either BackendConversionError BackendData
convertDataInfo context info =
  case find ((== dataInfoSymbol info) . dataInfoSymbol . dmInfo) (ccData context) of
    Just dataMeta -> Right (dmBackend dataMeta)
    Nothing ->
      buildDataMeta
        (elaborateScopeForDataInfo (ccModuleScopes context) (map dmInfo (ccData context)) info)
        info
        >>= Right . dmBackend

convertConstructorInfo :: ElaborateScope -> ConstructorInfo -> Either BackendConversionError BackendConstructor
convertConstructorInfo scope info = do
  foralls <- mapM (convertConstructorForall scope) (ctorForalls info)
  fields <- mapM (convertLoweredSourceType scope) (ctorArgs info)
  resultTy <- convertLoweredSourceType scope (ctorResult info)
  Right
    BackendConstructor
      { backendConstructorName = ctorRuntimeName info,
        backendConstructorForalls = foralls,
        backendConstructorFields = fields,
        backendConstructorResult = resultTy
      }

convertConstructorForall :: ElaborateScope -> (String, Maybe SrcType) -> Either BackendConversionError BackendTypeBinder
convertConstructorForall scope (name, mbBound) =
  BackendTypeBinder name <$> traverse (convertLoweredSourceType scope) mbBound

convertLoweredSourceType :: ElaborateScope -> SrcType -> Either BackendConversionError BackendType
convertLoweredSourceType scope =
  convertSourceType . lowerType scope

convertSourceType :: SrcType -> Either BackendConversionError BackendType
convertSourceType =
  \case
    STVar name -> Right (BTVar name)
    STArrow dom cod -> BTArrow <$> convertSourceType dom <*> convertSourceType cod
    STBase name -> Right (BTBase (backendBaseTy name))
    STCon name args -> BTCon (backendBaseTy name) <$> traverse convertSourceType args
    STVarApp name args -> BTVarApp name <$> traverse convertSourceType args
    STForall name mb body ->
      BTForall name
        <$> traverse (convertSourceType . unSrcBound) mb
        <*> convertSourceType body
    STMu name body -> BTMu name <$> convertSourceType body
    STBottom -> Right BTBottom

convertElabType :: ElabType -> Either BackendConversionError BackendType
convertElabType =
  \case
    TVar name -> Right (BTVar name)
    TArrow dom cod -> BTArrow <$> convertElabType dom <*> convertElabType cod
    TCon (BaseTy name) args -> BTCon (backendBaseTy name) <$> traverse convertElabType args
    TBase (BaseTy name) -> Right (BTBase (backendBaseTy name))
    TForall name mb body ->
      BTForall name
        <$> traverse (convertElabType . tyToElab) mb
        <*> convertElabType body
    TMu name body -> BTMu name <$> convertElabType body
    TBottom -> Right BTBottom

backendBaseTy :: String -> BaseTy
backendBaseTy name =
  BaseTy $
    case stripPrefix "<builtin>." name of
      Just builtinName
        | builtinName `Set.member` backendBuiltinTypeNames -> builtinName
      _ -> name

backendBuiltinTypeNames :: Set.Set String
backendBuiltinTypeNames =
  Set.fromList ["Bool", "Int", "String"]

normalizeBuiltinElabType :: Ty v -> Ty v
normalizeBuiltinElabType =
  \case
    TVar name -> TVar name
    TArrow dom cod -> TArrow (normalizeBuiltinElabType dom) (normalizeBuiltinElabType cod)
    TCon (BaseTy name) args -> TCon (backendBaseTy name) (fmap normalizeBuiltinElabType args)
    TBase (BaseTy name) -> TBase (backendBaseTy name)
    TForall name mb body ->
      TForall name (fmap normalizeBuiltinElabType mb) (normalizeBuiltinElabType body)
    TMu name body -> TMu name (normalizeBuiltinElabType body)
    TBottom -> TBottom

normalizeBuiltinElabScheme :: ElabScheme -> ElabScheme
normalizeBuiltinElabScheme (Forall binders body) =
  Forall
    [(name, fmap normalizeBuiltinElabType mbBound) | (name, mbBound) <- binders]
    (normalizeBuiltinElabType body)

normalizeBuiltinInstantiation :: Instantiation -> Instantiation
normalizeBuiltinInstantiation =
  \case
    InstId -> InstId
    InstApp ty -> InstApp (normalizeBuiltinElabType ty)
    InstBot ty -> InstBot (normalizeBuiltinElabType ty)
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstr name -> InstAbstr name
    InstUnder name inst -> InstUnder name (normalizeBuiltinInstantiation inst)
    InstInside inst -> InstInside (normalizeBuiltinInstantiation inst)
    InstSeq left right -> InstSeq (normalizeBuiltinInstantiation left) (normalizeBuiltinInstantiation right)

normalizeBuiltinElabTerm :: ElabTerm -> ElabTerm
normalizeBuiltinElabTerm =
  \case
    EVar name -> EVar name
    ELit lit -> ELit lit
    ELam name ty body -> ELam name (normalizeBuiltinElabType ty) (normalizeBuiltinElabTerm body)
    EApp fun arg -> EApp (normalizeBuiltinElabTerm fun) (normalizeBuiltinElabTerm arg)
    ELet name scheme rhs body ->
      ELet
        name
        (normalizeBuiltinElabScheme scheme)
        (normalizeBuiltinElabTerm rhs)
        (normalizeBuiltinElabTerm body)
    ETyAbs name mbBound body ->
      ETyAbs name (fmap normalizeBuiltinElabType mbBound) (normalizeBuiltinElabTerm body)
    ETyInst inner inst ->
      ETyInst (normalizeBuiltinElabTerm inner) (normalizeBuiltinInstantiation inst)
    ERoll ty body -> ERoll (normalizeBuiltinElabType ty) (normalizeBuiltinElabTerm body)
    EUnroll body -> EUnroll (normalizeBuiltinElabTerm body)

normalizeBuiltinEnv :: Env -> Env
normalizeBuiltinEnv env =
  Env
    { termEnv = Map.map normalizeBuiltinElabType (termEnv env),
      typeEnv = Map.map normalizeBuiltinElabType (typeEnv env)
    }

backendTypeToElabType :: BackendType -> Maybe ElabType
backendTypeToElabType =
  \case
    BTVar name -> Just (TVar name)
    BTArrow dom cod -> TArrow <$> backendTypeToElabType dom <*> backendTypeToElabType cod
    BTBase name -> Just (TBase name)
    BTCon name args -> TCon name <$> traverse backendTypeToElabType args
    BTVarApp {} -> Nothing
    BTForall name mb body ->
      TForall name
        <$> traverse backendTypeToBoundType mb
        <*> backendTypeToElabType body
    BTMu name body -> TMu name <$> backendTypeToElabType body
    BTBottom -> Just TBottom

backendTypeToBoundType :: BackendType -> Maybe BoundType
backendTypeToBoundType ty =
  backendTypeToElabType ty >>= either (const Nothing) Just . elabToBound

convertTerm :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError BackendExpr
convertTerm context env =
  convertTermExpected context env Nothing

convertTermExpected :: ConvertContext -> Env -> Maybe BackendType -> ElabTerm -> Either BackendConversionError BackendExpr
convertTermExpected context env mbExpectedTy term =
  case mbExpectedTy of
    Just resultTy ->
      convertSpecialTerm context env term resultTy
        >>= \case
          Just expr -> Right expr
          Nothing -> convertOrdinaryTerm context env term resultTy
    Nothing -> do
      resultTy <- inferBackendType env term
      convertSpecialTerm context env term resultTy
        >>= \case
          Just expr -> Right expr
          Nothing -> convertOrdinaryTerm context env term resultTy

convertSpecialTerm ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  BackendType ->
  Either BackendConversionError (Maybe BackendExpr)
convertSpecialTerm context env term resultTy =
  convertCaseApplication context env term resultTy
    >>= \case
      Just expr -> Right (Just expr)
      Nothing ->
        convertConstructorApplication context env term resultTy

convertOrdinaryTerm :: ConvertContext -> Env -> ElabTerm -> BackendType -> Either BackendConversionError BackendExpr
convertOrdinaryTerm context env term resultTy0 =
  let resultTy = normalizeBackendTypeForContext context resultTy0
   in case term of
    EVar name ->
      Right
        BackendVar
          { backendExprType = resultTy,
            backendVarName = name
          }
    ELit lit ->
      Right
        BackendLit
          { backendExprType = resultTy,
            backendLit = lit
          }
    ELam name paramTy body -> do
      paramBackendTy <-
        case resultTy of
          BTArrow dom _ -> Right dom
          _ -> normalizeBackendTypeForContext context <$> convertElabType paramTy
      let paramEnvTy =
            case backendTypeToElabType paramBackendTy of
              Just canonicalTy -> canonicalTy
              Nothing -> paramTy
      let bodyExpected =
            case resultTy of
              BTArrow _ cod -> Just cod
              _ -> Nothing
      bodyExpr <- convertTermExpected context (extendTermEnv name paramEnvTy env) bodyExpected body
      Right
        BackendLam
          { backendExprType = resultTy,
            backendParamName = name,
            backendParamType = paramBackendTy,
            backendBody = bodyExpr
          }
    EApp fun arg -> do
      funExpr <- convertTerm context env fun
      argExpr <-
        case backendExprType funExpr of
          BTArrow expectedArg _ -> convertTermExpected context env (Just expectedArg) arg
          _ -> convertTerm context env arg
      Right
        BackendApp
          { backendExprType = resultTy,
            backendFunction = funExpr,
            backendArgument = argExpr
          }
    ELet name scheme rhs body -> do
      let schemeTy = schemeToType scheme
      when (termMentionsFreeVariable name rhs) $
        Left (BackendUnsupportedRecursiveLet name)
      bindingTy <- normalizeBackendTypeForContext context <$> convertElabType schemeTy
      rhsExpr <- convertTermExpected context env (Just bindingTy) rhs
      let bindingEnvTy =
            case backendTypeToElabType bindingTy of
              Just canonicalTy -> canonicalTy
              Nothing -> schemeTy
      bodyExpr <- convertTermExpected context (extendTermEnv name bindingEnvTy env) (Just resultTy) body
      Right
        BackendLet
          { backendExprType = resultTy,
            backendLetName = name,
            backendLetType = bindingTy,
            backendLetRhs = rhsExpr,
            backendLetBody = bodyExpr
          }
    ETyAbs name mbBound body -> do
      mbBackendBound <- traverse (fmap (normalizeBackendTypeForContext context) . convertElabType . tyToElab) mbBound
      let boundTy = maybe TBottom tyToElab mbBound
          bodyExpected =
            case resultTy of
              BTForall expectedName _ bodyTy -> Just (substituteBackendType expectedName (BTVar name) bodyTy)
              _ -> Nothing
      bodyExpr <- convertTermExpected context (extendTypeEnv name boundTy env) bodyExpected body
      Right
        BackendTyAbs
          { backendExprType = resultTy,
            backendTyParamName = name,
            backendTyParamBound = mbBackendBound,
            backendTyAbsBody = bodyExpr
          }
    ETyInst inner inst ->
      convertTypeInstantiation context env resultTy inner inst
    ERoll _ body -> do
      let bodyExpected = unfoldBackendRecursiveType resultTy
      bodyExpr <- convertTermExpected context env bodyExpected body
      Right
        BackendRoll
          { backendExprType = resultTy,
            backendRollPayload = bodyExpr
          }
    EUnroll body -> do
      bodyExpr <- convertTerm context env body
      Right
        BackendUnroll
          { backendExprType = resultTy,
            backendUnrollPayload = bodyExpr
          }

termMentionsFreeVariable :: String -> ElabTerm -> Bool
termMentionsFreeVariable needle =
  go
  where
    go term =
      case term of
        EVar name ->
          name == needle
        ELit {} ->
          False
        ELam name _ body
          | name == needle -> False
          | otherwise -> go body
        EApp fun arg ->
          go fun || go arg
        ELet name _ rhs body
          | name == needle -> False
          | otherwise -> go rhs || go body
        ETyAbs _ _ body ->
          go body
        ETyInst inner _ ->
          go inner
        ERoll _ body ->
          go body
        EUnroll body ->
          go body

convertTypeInstantiation ::
  ConvertContext ->
  Env ->
  BackendType ->
  ElabTerm ->
  Instantiation ->
  Either BackendConversionError BackendExpr
convertTypeInstantiation context env resultTy inner inst =
  case inst of
    InstId -> do
      innerExpr <- convertTerm context env inner
      if alphaEqBackendType (backendExprType innerExpr) resultTy
        then Right innerExpr
        else Left (BackendUnsupportedInstantiation inst)
    _ ->
      case appLikeInstantiationType inst of
        Just tyArg -> do
          innerExpr <- convertTerm context env inner
          case backendExprType innerExpr of
            BTForall {} -> do
              backendTyArg <- normalizeBackendTypeForContext context <$> convertElabType tyArg
              Right
                BackendTyApp
                  { backendExprType = resultTy,
                    backendTyFunction = innerExpr,
                    backendTyArgument = backendTyArg
                  }
            _
              | alphaEqBackendType (backendExprType innerExpr) resultTy -> Right innerExpr
              | otherwise -> Left (BackendUnsupportedInstantiation inst)
        Nothing -> Left (BackendUnsupportedInstantiation inst)

appLikeInstantiationType :: Instantiation -> Maybe ElabType
appLikeInstantiationType inst =
  case inst of
    InstApp ty -> Just ty
    InstSeq (InstInside (InstBot ty)) InstElim -> Just ty
    InstSeq (InstInside (InstApp ty)) InstElim -> Just ty
    _ -> Nothing

convertConstructorApplication ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  BackendType ->
  Either BackendConversionError (Maybe BackendExpr)
convertConstructorApplication context env term resultTy =
  constructorApplicationTerm context term >>= \case
    Just (ConstructorApplication constructorMeta headTypeArgs args) -> do
      let constructor = cmBackend constructorMeta
          ownerContext = contextForDataMeta context (cmData constructorMeta)
          dataParameters = constructorDataParameters constructorMeta
          parameters = constructorTypeParameters constructorMeta
          rawFields = backendConstructorFields constructor
          effectiveResultTy = recoverStructuralBackendType ownerContext (recoverStructuralBackendType context resultTy)
          constructorResultTy = recoverStructuralBackendType ownerContext (backendConstructorResult constructor)
      typeBounds <- backendTypeBoundsFromEnv env
      initialSubstitution <- constructorTypeApplicationSubstitution constructorMeta headTypeArgs
      resultSubstitution <-
        case constructorResultSubstitution
          ownerContext
          typeBounds
          dataParameters
          parameters
          initialSubstitution
          constructorResultTy
          effectiveResultTy of
          Just substitution -> Right substitution
          Nothing ->
            Left
              ( BackendUnsupportedCaseShape
                  ( "constructor result type does not match expected result for `"
                      ++ backendConstructorName constructor
                      ++ "`"
                  )
              )
      substitution <-
        foldM
          (matchConstructorApplicationArgument context env typeBounds dataParameters parameters)
          resultSubstitution
          (zip rawFields args)
      let completedSubstitution = completeBackendParameterSubstitution parameters substitution
          fields = map (substituteBackendTypes completedSubstitution) rawFields
          substitutedResultTy = recoverStructuralBackendType ownerContext (substituteBackendTypes completedSubstitution constructorResultTy)
      unless (alphaEqBackendType substitutedResultTy effectiveResultTy) $
        Left
          ( BackendUnsupportedCaseShape
              ( "constructor result type does not match expected result for `"
                  ++ backendConstructorName constructor
                  ++ "`"
              )
          )
      argExprs <- zipWithM (convertTermExpected context env . Just) fields args
      mapM_ (checkConstructorArgumentType constructor) (zip [0 :: Int ..] (zip fields argExprs))
      Right
        ( Just
            BackendConstruct
              { backendExprType = effectiveResultTy,
                backendConstructName = backendConstructorName constructor,
                backendConstructArgs = argExprs
              }
        )
    Nothing -> Right Nothing
  where
    constructorResultSubstitution ownerContext typeBounds dataParameters parameters explicitSubstitution constructorResultTy effectiveResultTy =
      matchBackendTypeParameters typeBounds dataParameters parameters explicitSubstitution constructorResultTy effectiveResultTy
        <|> do
          inferredSubstitution <-
            matchBackendTypeParameters typeBounds dataParameters parameters Map.empty constructorResultTy effectiveResultTy
          if explicitSubstitutionAgreesWithInferred ownerContext typeBounds explicitSubstitution inferredSubstitution
            then Just inferredSubstitution
            else Nothing

    explicitSubstitutionAgreesWithInferred ownerContext typeBounds explicitSubstitution inferredSubstitution =
      all explicitArgumentAgrees (Map.toList explicitSubstitution)
      where
        explicitArgumentAgrees (name, explicitTy) =
          case Map.lookup name inferredSubstitution of
            Just (BTVar inferredName)
              | inferredName == name -> True
            Just inferredTy ->
              alphaEqBackendType (resolveTypeBoundDependencies explicitTy) (resolveTypeBoundDependencies inferredTy)
            Nothing -> False

        resolveTypeBoundDependencies =
          recoverStructuralBackendType ownerContext
            . substituteBackendTypes (completeBackendParameterSubstitution typeBounds Map.empty)

    checkConstructorArgumentType constructor (index, (expectedTy, argExpr)) =
      unless (alphaEqBackendType (backendExprType argExpr) expectedTy) $
        Left
          ( BackendUnsupportedCaseShape
              ( "constructor argument "
                  ++ show index
                  ++ " type does not match expected field for `"
                  ++ backendConstructorName constructor
                  ++ "`"
              )
          )

constructorApplicationTerm :: ConvertContext -> ElabTerm -> Either BackendConversionError (Maybe ConstructorApplication)
constructorApplicationTerm context term =
  case collectApps term of
    (headTerm, args) ->
      case directConstructorApplication headTerm args of
        Just application -> Right (Just application)
        Nothing -> structuralConstructorApplication headTerm args
  where
    directConstructorApplication headTerm args =
      case constructorHead context headTerm of
        Just (constructorName, headTypeArgs) -> do
          constructorMeta <- Map.lookup constructorName (ccConstructors context)
          guardConstructorArity constructorMeta args
          Just (ConstructorApplication constructorMeta headTypeArgs args)
        Nothing -> Nothing

    structuralConstructorApplication headTerm args =
      case filter (`constructorArityMatches` args) (Map.elems (ccConstructors context)) of
        [] -> Right Nothing
        candidates ->
          let strippedHead = stripTypeInsts headTerm
              matches = filter (\candidate -> structuralConstructorHeadMatches context candidate strippedHead) candidates
           in case matches of
                [constructorMeta] -> Right (Just (ConstructorApplication constructorMeta [] args))
                [] -> Right Nothing
                _ ->
                  Left
                    ( BackendUnsupportedCaseShape
                        ( "ambiguous structural constructor matches: "
                            ++ show (map (backendConstructorName . cmBackend) matches)
                        )
                    )

    constructorArityMatches constructorMeta args =
      length args == length (backendConstructorFields (cmBackend constructorMeta))

    guardConstructorArity constructorMeta args =
      if constructorArityMatches constructorMeta args
        then Just ()
        else Nothing

structuralConstructorHeadMatches :: ConvertContext -> ConstructorMeta -> ElabTerm -> Bool
structuralConstructorHeadMatches context constructorMeta headTerm =
  case collectStructuralLams fieldArity headTerm of
    Just (argNames, ERoll resultTy rolledBody)
      | structuralConstructorResultMatches context constructorMeta resultTy ->
          case collectStructuralLams ownerArity (stripLeadingTypeAbs rolledBody) of
            Just (handlerNames, selectedBody) ->
              case drop constructorIndex handlerNames of
                selectedHandler : _ ->
                  selectedHandlerCallMatches selectedHandler argNames selectedBody
                [] -> False
            Nothing -> False
    _ -> False
  where
    constructor = cmBackend constructorMeta
    fieldArity = length (backendConstructorFields constructor)
    ownerArity = length (backendDataConstructors (dmBackend (cmData constructorMeta)))
    constructorIndex = ctorIndex (cmInfo constructorMeta)

structuralConstructorResultMatches :: ConvertContext -> ConstructorMeta -> ElabType -> Bool
structuralConstructorResultMatches context constructorMeta resultTy =
  case convertElabType resultTy >>= backendTypeStructuralDataName of
    Right resultDataName -> constructorDataNameMatches context constructorMeta resultDataName
    Left _ -> False

constructorDataNameMatches :: ConvertContext -> ConstructorMeta -> String -> Bool
constructorDataNameMatches context constructorMeta resultDataName =
  resultDataName == backendDataName (dmBackend dataMeta)
    || localUnqualifiedDataNameMatches context dataMeta resultDataName
  where
    dataMeta = cmData constructorMeta

localUnqualifiedDataNameMatches :: ConvertContext -> DataMeta -> String -> Bool
localUnqualifiedDataNameMatches context dataMeta resultDataName =
  case ccCurrentModuleName context of
    Just moduleName ->
      dataModule (dmInfo dataMeta) == moduleName
        && resultDataName == symbolDefiningName (dataInfoSymbol (dmInfo dataMeta))
    Nothing -> False

backendTypeStructuralDataName :: BackendType -> Either BackendConversionError String
backendTypeStructuralDataName =
  \case
    BTBase (BaseTy name) -> Right name
    BTCon (BaseTy name) _ -> Right name
    BTMu name _ ->
      case structuralRecursiveDataName name of
        Just resultDataName -> Right resultDataName
        Nothing -> Left (BackendUnsupportedCaseShape ("unsupported structural constructor result type " ++ show name))
    ty -> Left (BackendUnsupportedCaseShape ("unsupported constructor result type " ++ show ty))

collectStructuralLams :: Int -> ElabTerm -> Maybe ([String], ElabTerm)
collectStructuralLams expectedCount =
  go [] expectedCount
  where
    go names remaining term
      | remaining <= 0 = Just (names, term)
      | otherwise =
          case term of
            ELam name _ body -> go (names ++ [name]) (remaining - 1) body
            _ -> Nothing

stripLeadingTypeAbs :: ElabTerm -> ElabTerm
stripLeadingTypeAbs =
  \case
    ETyAbs _ _ body -> stripLeadingTypeAbs body
    term -> term

selectedHandlerCallMatches :: String -> [String] -> ElabTerm -> Bool
selectedHandlerCallMatches selectedHandler argNames body =
  case collectApps body of
    (EVar handlerName, args) ->
      handlerName == selectedHandler && map selectedArgName args == map Just argNames
    _ -> False
  where
    selectedArgName =
      \case
        EVar name -> Just name
        _ -> Nothing

constructorTypeParameters :: ConstructorMeta -> BackendParameterBounds
constructorTypeParameters constructorMeta =
  constructorTypeParameterBoundsFor (dmBackend (cmData constructorMeta)) (cmBackend constructorMeta)

constructorDataParameters :: ConstructorMeta -> [String]
constructorDataParameters =
  backendDataParameters . dmBackend . cmData

constructorTypeParameterBoundsFor :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsFor dataDecl constructor =
  Map.fromList $
    [(name, Nothing) | name <- backendDataParameters dataDecl]
      ++ [ (backendTypeBinderName binder, backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

constructorTypeApplicationSubstitution ::
  ConstructorMeta ->
  [BackendType] ->
  Either BackendConversionError (Map String BackendType)
constructorTypeApplicationSubstitution constructorMeta typeArgs =
  if length typeArgs <= length typeApplicationNames
    then Right (Map.fromList (zip typeApplicationNames typeArgs))
    else
      Left
        ( BackendUnsupportedCaseShape
            ( "constructor type application arity mismatch for `"
                ++ backendConstructorName (cmBackend constructorMeta)
                ++ "`"
            )
        )
  where
    typeApplicationNames = constructorTypeApplicationParameterNames constructorMeta

constructorTypeApplicationParameterNames :: ConstructorMeta -> [String]
constructorTypeApplicationParameterNames constructorMeta =
  sort (Set.toList (freeSourceTypeVars (ctorType (cmInfo constructorMeta))))
    ++ map backendTypeBinderName (backendConstructorForalls (cmBackend constructorMeta))

freeSourceTypeVars :: SrcType -> Set.Set String
freeSourceTypeVars =
  go Set.empty
  where
    go bound =
      \case
        STVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod ->
          go bound dom `Set.union` go bound cod
        STBase {} ->
          Set.empty
        STCon _ args ->
          foldMap (go bound) args
        STVarApp name args ->
          let headVars =
                if Set.member name bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body ->
          go (Set.insert name bound) body
        STBottom ->
          Set.empty

constructorHead :: ConvertContext -> ElabTerm -> Maybe (String, [BackendType])
constructorHead context term =
  case collectConstructorHeadTypes [] term of
    Just (name, typeArgs) ->
      case traverse (fmap (normalizeBackendTypeForContext context) . convertElabType) typeArgs of
        Right backendTypeArgs -> Just (name, backendTypeArgs)
        Left _ -> Nothing
    Nothing -> Nothing
  where
    collectConstructorHeadTypes typeArgs =
      \case
        ETyInst inner inst
          | Just ty <- appLikeInstantiationType inst ->
              collectConstructorHeadTypes (ty : typeArgs) inner
        EVar name -> Just (name, typeArgs)
        _ -> Nothing

stripTypeInsts :: ElabTerm -> ElabTerm
stripTypeInsts =
  \case
    ETyInst inner _ -> stripTypeInsts inner
    other -> other

convertCaseApplication ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  BackendType ->
  Either BackendConversionError (Maybe BackendExpr)
convertCaseApplication context env term resultTy =
  case collectApps term of
    (headTerm, args) ->
      case caseScrutinee headTerm of
        Nothing -> Right Nothing
        Just scrutineeTerm -> do
          (backendScrutineeTy, mbScrutineeData) <- caseScrutineeInfo context env scrutineeTerm
          scrutineeExpr <- convertTermExpected context env (Just backendScrutineeTy) scrutineeTerm
          dataMeta <-
            case mbScrutineeData of
              Just scrutineeData -> Right scrutineeData
              Nothing -> do
                typeBounds <- backendTypeBoundsFromEnv env
                requireCaseData context typeBounds (backendExprType scrutineeExpr)
          let constructors = backendDataConstructors (dmBackend dataMeta)
          case compare (length args) (length constructors) of
            EQ -> Just <$> convertCaseWithHandlers context env resultTy scrutineeExpr constructors args
            GT -> do
              let (handlers, extraArgs) = splitAt (length constructors) args
              extraArgTys <- mapM (inferBackendType env) extraArgs
              case scanr BTArrow resultTy extraArgTys of
                caseResultTy : appliedResultTys -> do
                  caseExpr <- convertCaseWithHandlers context env caseResultTy scrutineeExpr constructors handlers
                  Just
                    <$> foldM
                      (applyCaseExtraArgument context env)
                      caseExpr
                      (zip3 extraArgs extraArgTys appliedResultTys)
                [] -> Right Nothing
            LT -> Right Nothing

convertCaseWithHandlers ::
  ConvertContext ->
  Env ->
  BackendType ->
  BackendExpr ->
  [BackendConstructor] ->
  [ElabTerm] ->
  Either BackendConversionError BackendExpr
convertCaseWithHandlers context env resultTy scrutineeExpr constructors handlers = do
  alternatives <- zipWithMCase (convertCaseAlternative context env resultTy) constructors handlers
  Right
    BackendCase
      { backendExprType = resultTy,
        backendScrutinee = scrutineeExpr,
        backendAlternatives = alternatives
      }

applyCaseExtraArgument ::
  ConvertContext ->
  Env ->
  BackendExpr ->
  (ElabTerm, BackendType, BackendType) ->
  Either BackendConversionError BackendExpr
applyCaseExtraArgument context env funExpr (arg, argTy, resultTy) = do
  argExpr <- convertTermExpected context env (Just argTy) arg
  Right
    BackendApp
      { backendExprType = resultTy,
        backendFunction = funExpr,
        backendArgument = argExpr
      }

caseScrutinee :: ElabTerm -> Maybe ElabTerm
caseScrutinee term =
  case term of
    ETyInst (EUnroll scrutinee) inst
      | Just _ <- appLikeInstantiationType inst -> Just scrutinee
    _ -> Nothing

caseScrutineeInfo :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError (BackendType, Maybe DataMeta)
caseScrutineeInfo context env scrutineeTerm =
  constructorApplicationResultType context env scrutineeTerm
    >>= \case
      Just info -> Right info
      Nothing -> do
        scrutineeTy0 <- inferBackendType env scrutineeTerm
        let scrutineeTy = normalizeBackendTypeForContext context scrutineeTy0
        Right
          ( scrutineeTy,
            scrutineeDataHint context scrutineeTerm
              <|> backendTypeDataMeta context scrutineeTy
          )

scrutineeDataHint :: ConvertContext -> ElabTerm -> Maybe DataMeta
scrutineeDataHint context term =
  case stripTypeInsts term of
    EVar name -> Map.lookup name (ccBindingData context)
    _ -> Nothing

backendTypeDataMeta :: ConvertContext -> BackendType -> Maybe DataMeta
backendTypeDataMeta context ty =
  case ty of
    BTBase (BaseTy name) -> dataMetaByBackendName context name
    BTCon (BaseTy name) _ -> dataMetaByBackendName context name
    BTMu name _ -> structuralRecursiveDataMeta context name
    _ -> Nothing

dataMetaByBackendName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByBackendName context name =
  find ((== name) . backendDataName . dmBackend) (ccData context)

dataMetaByStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByStructuralName context name =
  dataMetaByBackendName context name
    <|> dataMetaByCurrentScopeStructuralName context name
    <|> dataMetaByCurrentModuleStructuralName context name

dataMetaByCurrentScopeStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByCurrentScopeStructuralName context name = do
  moduleName <- ccCurrentModuleName context
  scope <- Map.lookup moduleName (ccModuleScopes context)
  info <- Map.lookup name (elaborateScopeDataTypes scope)
  dataMetaBySymbol context (dataInfoSymbol info)

dataMetaByCurrentModuleStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByCurrentModuleStructuralName context name =
  case ccCurrentModuleName context of
    Nothing -> Nothing
    Just moduleName ->
      case
        [ dataMeta
          | dataMeta <- ccData context,
            dataModule (dmInfo dataMeta) == moduleName,
            symbolDefiningName (dataInfoSymbol (dmInfo dataMeta)) == name
        ]
      of
        [dataMeta] -> Just dataMeta
        _ -> Nothing

dataMetaBySymbol :: ConvertContext -> SymbolIdentity -> Maybe DataMeta
dataMetaBySymbol context symbol =
  find ((== symbol) . dataInfoSymbol . dmInfo) (ccData context)

canonicalizeStructuralMuNames :: ConvertContext -> BackendType -> BackendType
canonicalizeStructuralMuNames context =
  go
  where
    go ty =
      case ty of
        BTVar {} -> ty
        BTArrow dom cod -> BTArrow (go dom) (go cod)
        BTBase {} -> ty
        BTCon name args -> BTCon name (fmap go args)
        BTVarApp name args -> BTVarApp name (fmap go args)
        BTForall name mb body -> BTForall name (fmap go mb) (go body)
        BTMu name body ->
          let (name', body') = canonicalizeStructuralMuBinder context name body
           in BTMu name' (go body')
        BTBottom -> BTBottom

canonicalizeStructuralMuBinder :: ConvertContext -> String -> BackendType -> (String, BackendType)
canonicalizeStructuralMuBinder context name body =
  case structuralRecursiveDataMeta context name of
    Just dataMeta ->
      let canonicalName = "$" ++ backendDataName (dmBackend dataMeta) ++ "_self"
       in if name == canonicalName
            then (name, body)
            else (canonicalName, substituteBackendType name (BTVar canonicalName) body)
    Nothing -> (name, body)

recoverStructuralBackendType :: ConvertContext -> BackendType -> BackendType
recoverStructuralBackendType context =
  go Set.empty
  where
    go seen ty =
      case ty of
        BTVar {} -> ty
        BTArrow dom cod -> BTArrow (go seen dom) (go seen cod)
        BTBase {} -> ty
        BTCon name args -> BTCon name (fmap (go seen) args)
        BTVarApp name args -> BTVarApp name (fmap (go seen) args)
        BTForall name mb body -> BTForall name (fmap (go seen) mb) (go seen body)
        BTMu name body ->
          let (name', body') = canonicalizeStructuralMuBinder context name body
              seen' = Set.insert name' (Set.insert name seen)
           in if Set.member name seen || Set.member name' seen
                then BTMu name' (canonicalizeStructuralMuNames context body')
                else case structuralRecursiveDataMeta context name' of
                  Just dataMeta
                    | Just args <- structuralBackendDataArguments (go seen') dataMeta body' ->
                        backendDataType (backendDataName (dmBackend dataMeta)) args
                  _ -> BTMu name' (go seen' body')
        BTBottom -> BTBottom

backendDataType :: String -> [BackendType] -> BackendType
backendDataType name args =
  case args of
    [] -> BTBase (BaseTy name)
    arg : rest -> BTCon (BaseTy name) (arg :| rest)

structuralRecursiveDataMeta :: ConvertContext -> String -> Maybe DataMeta
structuralRecursiveDataMeta context name =
  structuralRecursiveDataName name >>= dataMetaByStructuralName context

structuralRecursiveDataName :: String -> Maybe String
structuralRecursiveDataName name = do
  let withoutPrefix = dropWhile (== '$') name
  stripSuffix "_self" withoutPrefix

structuralMuAsDataType :: [String] -> String -> Maybe BackendType
structuralMuAsDataType dataParameterOrder muName = do
  structuralName <- structuralRecursiveDataName muName
  let parameterArgs = map BTVar dataParameterOrder
  Just $
    case parameterArgs of
      [] -> BTBase (BaseTy structuralName)
      arg : rest -> BTCon (BaseTy structuralName) (arg :| rest)

structuralMuAsActualDataType :: String -> BackendType -> Maybe BackendType
structuralMuAsActualDataType muName actual =
  case actual of
    BTBase (BaseTy actualName)
      | structuralMuNameMatches actualName muName -> Just actual
    BTCon (BaseTy actualName) _
      | structuralMuNameMatches actualName muName -> Just actual
    _ -> Nothing

structuralMuNameMatches :: String -> String -> Bool
structuralMuNameMatches actualName muName =
  case structuralRecursiveDataName muName of
    Just structuralName -> actualName == structuralName
    Nothing -> False

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix value =
  reverse <$> stripPrefix (reverse suffix) (reverse value)

structuralBackendDataArguments :: (BackendType -> BackendType) -> DataMeta -> BackendType -> Maybe [BackendType]
structuralBackendDataArguments recoverFieldTy dataMeta body = do
  handlerFields <- structuralBackendHandlerFields body
  let dataDecl = dmBackend dataMeta
      dataParameters = backendDataParameters dataDecl
      constructors = backendDataConstructors dataDecl
      parameterBounds =
        Map.fromList [(name, Nothing) | name <- dataParameters]
  if length handlerFields == length constructors
    then do
      substitution <-
        foldM
          (matchConstructorFields dataParameters parameterBounds)
          Map.empty
          (zip constructors handlerFields)
      let completedSubstitution = completeBackendParameterSubstitution parameterBounds substitution
      Just [Map.findWithDefault (BTVar name) name completedSubstitution | name <- dataParameters]
    else Nothing
  where
    matchConstructorFields dataParameters parameterBounds substitution (constructor, fields) =
      if length fields == length (backendConstructorFields constructor)
        then
          foldM
            ( \substitutionAcc (expectedTy, actualTy) ->
                matchBackendTypeParameters
                  Map.empty
                  dataParameters
                  (constructorParameterBounds parameterBounds constructor)
                  substitutionAcc
                  expectedTy
                  (recoverFieldTy actualTy)
            )
            substitution
            (zip (backendConstructorFields constructor) fields)
        else Nothing

    constructorParameterBounds parameterBounds constructor =
      parameterBounds
        `Map.union` Map.fromList
          [ (backendTypeBinderName binder, backendTypeBinderBound binder)
            | binder <- backendConstructorForalls constructor
          ]

structuralBackendHandlerFields :: BackendType -> Maybe [[BackendType]]
structuralBackendHandlerFields =
  \case
    BTForall resultName _ handlerTy -> collectHandlers resultName handlerTy
    _ -> Nothing
  where
    collectHandlers resultName =
      go []
      where
        go handlers ty
          | alphaEqBackendType ty (BTVar resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> do
                  fields <- collectHandlerFields resultName handlerTy
                  go (handlers ++ [fields]) rest
                _ -> Nothing

    collectHandlerFields resultName =
      go []
      where
        go fields ty
          | alphaEqBackendType ty (BTVar resultName) = Just fields
          | otherwise =
              case ty of
                BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
                _ -> Nothing

constructorApplicationResultType :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError (Maybe (BackendType, Maybe DataMeta))
constructorApplicationResultType context env term =
  constructorApplicationTerm context term >>= \case
    Just (ConstructorApplication constructorMeta headTypeArgs args) -> do
      let constructor = cmBackend constructorMeta
          ownerContext = contextForDataMeta context (cmData constructorMeta)
          fields = backendConstructorFields constructor
          dataParameters = constructorDataParameters constructorMeta
          parameters = constructorTypeParameters constructorMeta
      typeBounds <- backendTypeBoundsFromEnv env
      initialSubstitution <- constructorTypeApplicationSubstitution constructorMeta headTypeArgs
      substitution <-
        foldM
          (matchConstructorApplicationArgument context env typeBounds dataParameters parameters)
          initialSubstitution
          (zip fields args)
      let resultTy =
            recoverStructuralBackendType ownerContext $
              substituteBackendTypes
                (completeBackendParameterSubstitution parameters substitution)
                (backendConstructorResult constructor)
      Right (Just (resultTy, Just (cmData constructorMeta)))
    Nothing -> Right Nothing

matchConstructorApplicationArgument ::
  ConvertContext ->
  Env ->
  BackendTypeBounds ->
  [String] ->
  BackendParameterBounds ->
  Map String BackendType ->
  (BackendType, ElabTerm) ->
  Either BackendConversionError (Map String BackendType)
matchConstructorApplicationArgument context env typeBounds dataParameters parameters substitution (expectedTy, arg) =
  -- This is only a best-effort way to recover constructor type parameters.
  -- Expected-type conversion of the argument remains authoritative because it
  -- can canonicalize nested constructor applications before validation.
  case inferBackendType env arg of
    Right actualTy0 ->
      let actualTy = recoverStructuralBackendType context actualTy0
       in case matchBackendTypeParameters typeBounds dataParameters parameters substitution expectedTy actualTy of
            Just substitution' -> Right substitution'
            Nothing -> Right substitution
    Left _ -> Right substitution

requireCaseData :: ConvertContext -> BackendTypeBounds -> BackendType -> Either BackendConversionError DataMeta
requireCaseData context typeBounds scrutineeTy =
  case filter (dataMatchesScrutinee typeBounds scrutineeTy) (ccData context) of
    [dataMeta] -> Right dataMeta
    [] -> Left (BackendUnsupportedCaseShape ("no backend data matches scrutinee type " ++ show scrutineeTy))
    matches ->
      Left
        ( BackendUnsupportedCaseShape
            ("ambiguous backend data matches scrutinee type " ++ show scrutineeTy ++ ": " ++ show (map (backendDataName . dmBackend) matches))
        )

dataMatchesScrutinee :: BackendTypeBounds -> BackendType -> DataMeta -> Bool
dataMatchesScrutinee typeBounds scrutineeTy dataMeta =
  any
    ( \constructor ->
        case
          matchBackendTypeParameters
            typeBounds
            (backendDataParameters (dmBackend dataMeta))
            (constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor)
            Map.empty
            (backendConstructorResult constructor)
            scrutineeTy of
          Just _ -> True
          Nothing -> False
    )
    (backendDataConstructors (dmBackend dataMeta))

convertCaseAlternative ::
  ConvertContext ->
  Env ->
  BackendType ->
  BackendConstructor ->
  ElabTerm ->
  Either BackendConversionError BackendAlternative
convertCaseAlternative context env resultTy constructor handler = do
  let fields = backendConstructorFields constructor
      (params, body) = collectLeadingLams (length fields) handler
  when (length params /= length fields) $
    Left
      ( BackendUnsupportedCaseShape
          ("handler arity does not match constructor `" ++ backendConstructorName constructor ++ "`")
      )
  let env' =
        foldr
          (\(name, ty) acc -> extendTermEnv name ty acc)
          env
          params
  bodyExpr <- convertTermExpected context env' (Just resultTy) body
  unless (alphaEqBackendType (backendExprType bodyExpr) resultTy) $
    Left
      ( BackendUnsupportedCaseShape
          ("handler result type does not match case result for `" ++ backendConstructorName constructor ++ "`")
      )
  Right
    BackendAlternative
      { backendAltPattern = BackendConstructorPattern (backendConstructorName constructor) (map fst params),
        backendAltBody = bodyExpr
      }

collectLeadingLams :: Int -> ElabTerm -> ([(String, ElabType)], ElabTerm)
collectLeadingLams arity =
  go [] arity . stripLeadingTypeWrappers
  where
    go params remaining term
      | remaining <= 0 = (params, term)
      | otherwise =
          case term of
            ETyAbs _ _ body -> go params remaining body
            ETyInst inner _ -> go params remaining inner
            ELam name ty body -> go (params ++ [(name, ty)]) (remaining - 1) body
            other -> (params, other)

    stripLeadingTypeWrappers term =
      case term of
        ETyAbs _ _ body -> stripLeadingTypeWrappers body
        ETyInst inner _ -> stripLeadingTypeWrappers inner
        other -> other

collectApps :: ElabTerm -> (ElabTerm, [ElabTerm])
collectApps =
  go []
  where
    go args term =
      case term of
        EApp fun arg -> go (arg : args) fun
        other -> (other, args)

inferBackendType :: Env -> ElabTerm -> Either BackendConversionError BackendType
inferBackendType env term =
  case typeCheckWithEnv (normalizeBuiltinEnv env) (normalizeBuiltinElabTerm term) of
    Right ty -> convertElabType ty
    Left err -> Left (BackendTypeCheckFailed term err)

extendTermEnv :: String -> ElabType -> Env -> Env
extendTermEnv name ty env =
  env {termEnv = Map.insert name (normalizeBuiltinElabType ty) (termEnv env)}

extendTypeEnv :: String -> ElabType -> Env -> Env
extendTypeEnv name ty env =
  env {typeEnv = Map.insert name (normalizeBuiltinElabType ty) (typeEnv env)}

backendTypeBoundsFromEnv :: Env -> Either BackendConversionError BackendTypeBounds
backendTypeBoundsFromEnv env =
  traverse convertTypeBound (typeEnv env)
  where
    convertTypeBound TBottom = Right Nothing
    convertTypeBound boundTy = Just <$> convertElabType boundTy

zipWithMCase ::
  (BackendConstructor -> ElabTerm -> Either BackendConversionError BackendAlternative) ->
  [BackendConstructor] ->
  [ElabTerm] ->
  Either BackendConversionError (NonEmpty BackendAlternative)
zipWithMCase f constructors handlers =
  case zipWith f constructors handlers of
    firstAlt : restAlts ->
      (:|) <$> firstAlt <*> sequence restAlts
    [] ->
      Left (BackendUnsupportedCaseShape "case expression has no alternatives")

matchBackendTypeParameters ::
  BackendTypeBounds ->
  [String] ->
  BackendParameterBounds ->
  Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map String BackendType)
matchBackendTypeParameters typeBounds dataParameterOrder parameterBounds =
  go Map.empty Map.empty
  where
    go leftEnv rightEnv substitution expected actual =
      case expected of
        BTVar name
          | Map.member name parameterBounds,
            Map.notMember name leftEnv ->
              insertParameterSubstitution name actual substitution
        _ ->
          case (expected, actual) of
            (BTVar expectedName, BTVar actualName)
              | sameTypeVar leftEnv rightEnv expectedName actualName ->
                  Just substitution
            (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
              go leftEnv rightEnv substitution expectedDom actualDom
                >>= \substitution' -> go leftEnv rightEnv substitution' expectedCod actualCod
            (BTBase expectedBase, BTBase actualBase)
              | expectedBase == actualBase ->
                  Just substitution
            (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
              | expectedCon == actualCon,
                length expectedArgs == length actualArgs ->
                  foldM
                    ( \substitutionAcc (expectedArg, actualArg) ->
                        go leftEnv rightEnv substitutionAcc expectedArg actualArg
                    )
                    substitution
                    (zip (NE.toList expectedArgs) (NE.toList actualArgs))
            (BTMu expectedName expectedBody, actualTy@(BTBase {})) ->
              matchStructuralMuExpected leftEnv rightEnv substitution expectedName expectedBody actualTy
            (BTMu expectedName expectedBody, actualTy@(BTCon {})) ->
              matchStructuralMuExpected leftEnv rightEnv substitution expectedName expectedBody actualTy
            (expectedTy@(BTBase {}), BTMu actualName actualBody) ->
              matchStructuralMuActual leftEnv rightEnv substitution expectedTy actualName actualBody
            (expectedTy@(BTCon {}), BTMu actualName actualBody) ->
              matchStructuralMuActual leftEnv rightEnv substitution expectedTy actualName actualBody
            (BTVarApp expectedName expectedArgs, _) ->
              matchBackendTypeApplication leftEnv rightEnv substitution expectedName (NE.toList expectedArgs) actual
            (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
              substitution' <-
                case (expectedBound, actualBound) of
                  (Nothing, Nothing) -> Just substitution
                  (Just expectedBoundTy, Just actualBoundTy) -> go leftEnv rightEnv substitution expectedBoundTy actualBoundTy
                  _ -> Nothing
              go
                (Map.insert expectedName actualName leftEnv)
                (Map.insert actualName expectedName rightEnv)
                substitution'
                expectedBody
                actualBody
            (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
              go
                (Map.insert expectedName actualName leftEnv)
                (Map.insert actualName expectedName rightEnv)
                substitution
                expectedBody
                actualBody
            (BTBottom, BTBottom) ->
              Just substitution
            _ ->
              Nothing

    matchBackendTypeApplication leftEnv rightEnv substitution name expectedArgs actual =
      case decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                if Map.member name parameterBounds && Map.notMember name leftEnv
                  then insertParameterSubstitution name actualHead substitution
                  else go leftEnv rightEnv substitution (BTVar name) actualHead
              foldM
                (\substitutionAcc (expectedArg, actualArg) -> go leftEnv rightEnv substitutionAcc expectedArg actualArg)
                substitution'
                (zip expectedArgs actualArgs)
        _ -> Nothing

    matchStructuralMuExpected leftEnv rightEnv substitution muName _body actualTy =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \expectedTy -> go leftEnv rightEnv substitution expectedTy actualTy,
          structuralMuAsActualDataType muName actualTy
            >>= \expectedTy -> go leftEnv rightEnv substitution expectedTy actualTy
        ]

    matchStructuralMuActual leftEnv rightEnv substitution expectedTy muName _body =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \actualTy -> go leftEnv rightEnv substitution expectedTy actualTy,
          structuralMuAsActualDataType muName expectedTy
            >>= \actualTy -> go leftEnv rightEnv substitution expectedTy actualTy
        ]

    sameTypeVar leftEnv rightEnv expectedName actualName =
      case (Map.lookup expectedName leftEnv, Map.lookup actualName rightEnv) of
        (Just expectedActual, Just actualExpected) -> expectedActual == actualName && actualExpected == expectedName
        (Nothing, Nothing) -> expectedName == actualName
        _ -> False

    insertParameterSubstitution name actual substitution =
      case Map.lookup name substitution of
        Nothing ->
          if backendParameterBoundMatches name actual substitution
            then Just (Map.insert name actual substitution)
            else Nothing
        Just previous
          | alphaEqBackendType previous actual && backendParameterBoundMatches name previous substitution -> Just substitution
        _ -> Nothing

    firstJust =
      \case
        [] -> Nothing
        candidate : rest ->
          case candidate of
            Just value -> Just value
            Nothing -> firstJust rest

    backendParameterBoundMatches name actual substitution =
      case Map.lookup name parameterBounds of
        Just (Just _)
          | BTVar actualName <- actual,
            actualName == name ->
              True
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let dependencySubstitution =
                    completeBackendParameterSubstitution
                      (Map.delete name parameterBounds)
                      (Map.delete name substitution)
                  expectedBound = substituteBackendTypes dependencySubstitution boundTy
               in typeBoundDependenciesMatch actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVar actualName ->
          case Map.lookup actualName typeBounds of
            Just (Just actualBound) ->
              typeBoundDependenciesMatch actualBound expectedBound
            _ ->
              False
        _ ->
          False

    resolveTypeBoundDependencies =
      substituteBackendTypes resolvedTypeBounds

    resolvedTypeBounds =
      completeBackendParameterSubstitution typeBounds Map.empty

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVar name -> Just (BTVar name, [])
    BTBase name -> Just (BTBase name, [])
    BTCon name args -> Just (BTBase name, NE.toList args)
    BTVarApp name args -> Just (BTVar name, NE.toList args)
    _ -> Nothing

completeBackendParameterSubstitution :: BackendParameterBounds -> Map String BackendType -> Map String BackendType
completeBackendParameterSubstitution parameterBounds substitution0 =
  resolveDefaultedBounds defaultedNames substitution1
  where
    substitution1 =
      foldl insertBoundDefault substitution0 (Map.toList parameterBounds)

    defaultedNames =
      Set.fromList
        [ name
          | (name, Just boundTy) <- Map.toList parameterBounds,
            Map.notMember name substitution0,
            not (alphaEqBackendType boundTy BTBottom)
        ]

    insertBoundDefault substitution (name, Just boundTy)
      | Map.member name substitution = substitution
      | alphaEqBackendType boundTy BTBottom = substitution
      | otherwise = Map.insert name (substituteBackendTypes substitution boundTy) substitution
    insertBoundDefault substitution _ =
      substitution

    resolveDefaultedBounds names =
      go (Set.size names + Map.size parameterBounds + 1)
      where
        go remaining substitution
          | remaining <= 0 = substitution
          | substitution' == substitution = substitution
          | otherwise = go (remaining - 1) substitution'
          where
            substitution' =
              foldl resolveDefaultedBound substitution (Set.toList names)

    resolveDefaultedBound substitution name =
      case Map.lookup name substitution of
        Just ty ->
          Map.insert name (substituteBackendTypes (Map.delete name substitution) ty) substitution
        Nothing ->
          substitution
