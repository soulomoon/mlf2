{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Monad (foldM, forM, unless, when, zipWithM)
import Data.List (find, sort)
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
    ElabType,
    BoundType,
    Instantiation (..),
    Ty (..),
    TypeCheckError,
    tyToElab,
  )
import MLF.Frontend.Program.Elaborate (ElaborateScope, lowerType, mkElaborateScope)
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
    ccData :: [DataMeta]
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

type BackendParameterBounds = Map String (Maybe BackendType)

type BackendTypeBounds = Map String (Maybe BackendType)

data BackendTypeAbsBinder = BackendTypeAbsBinder String (Maybe BackendType)

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
      { termEnv = Map.fromList terms,
        typeEnv = Map.empty
      }

convertCheckedModule :: ConvertContext -> Env -> CheckedModule -> Either BackendConversionError BackendModule
convertCheckedModule context env checkedModule = do
  dataDecls <- mapM (convertDataInfo context) (Map.elems (checkedModuleData checkedModule))
  bindings <- mapM (convertCheckedBinding context env checkedModule) (checkedModuleBindings checkedModule)
  Right
    BackendModule
      { backendModuleName = checkedModuleName checkedModule,
        backendModuleData = dataDecls,
        backendModuleBindings = bindings
      }

convertCheckedBinding :: ConvertContext -> Env -> CheckedModule -> CheckedBinding -> Either BackendConversionError BackendBinding
convertCheckedBinding context env checkedModule binding = do
  canonicalElabTy <- checkedBindingCanonicalType context checkedModule binding
  bindingTy <- convertElabType canonicalElabTy
  expr <-
    case Map.lookup (checkedBindingName binding) (ccConstructors context) of
      Just constructorMeta
        | constructorBindingResultMatches bindingTy constructorMeta ->
            synthesizeConstructorBinding bindingTy constructorMeta
      _ -> convertTermExpected context env (Just bindingTy) (checkedBindingTerm binding)
  Right
    BackendBinding
      { backendBindingName = checkedBindingName binding,
        backendBindingType = bindingTy,
        backendBindingExpr = expr,
        backendBindingExportedAsMain = checkedBindingExportedAsMain binding
      }

checkedBindingCanonicalType :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingCanonicalType context checkedModule binding = do
  let checkedTy = checkedBindingType binding
      scope = scopeForModule context (checkedModuleName checkedModule)
  checkedBackendTy <- convertElabType checkedTy
  canonicalTy <- sourceTypeToElabType (lowerType scope (checkedBindingSourceType binding))
  canonicalBackendTy <- convertElabType canonicalTy
  if alphaEqBackendType checkedBackendTy canonicalBackendTy
    then Right canonicalTy
    else Right checkedTy

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
  case matchBackendTypeParameters Map.empty parameters Map.empty (backendConstructorResult constructor) resultTy of
    Just _ -> True
    Nothing -> False
  where
    constructor = cmBackend constructorMeta
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
        ccData = dataMetas
      }

allDataInfos :: CheckedProgram -> [DataInfo]
allDataInfos checked =
  [ dataInfo
    | checkedModule <- checkedProgramModules checked,
      dataInfo <- Map.elems (checkedModuleData checkedModule)
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
  constructors <- mapM (convertConstructorInfo scope) (dataConstructors info)
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
    STBase name -> Right (BTBase (BaseTy name))
    STCon name args -> BTCon (BaseTy name) <$> traverse convertSourceType args
    ty@STVarApp {} -> Left (BackendUnsupportedSourceType ty)
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
    TCon name args -> BTCon name <$> traverse convertElabType args
    TBase name -> Right (BTBase name)
    TForall name mb body ->
      BTForall name
        <$> traverse (convertElabType . tyToElab) mb
        <*> convertElabType body
    TMu name body -> BTMu name <$> convertElabType body
    TBottom -> Right BTBottom

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
convertOrdinaryTerm context env term resultTy =
  case term of
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
      paramBackendTy <- convertElabType paramTy
      let bodyExpected =
            case resultTy of
              BTArrow _ cod -> Just cod
              _ -> Nothing
      bodyExpr <- convertTermExpected context (extendTermEnv name paramTy env) bodyExpected body
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
      bindingTy <- convertElabType schemeTy
      rhsExpr <- convertTermExpected context env (Just bindingTy) rhs
      bodyExpr <- convertTermExpected context (extendTermEnv name schemeTy env) (Just resultTy) body
      Right
        BackendLet
          { backendExprType = resultTy,
            backendLetName = name,
            backendLetType = bindingTy,
            backendLetRhs = rhsExpr,
            backendLetBody = bodyExpr
          }
    ETyAbs name mbBound body -> do
      mbBackendBound <- traverse (convertElabType . tyToElab) mbBound
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
              backendTyArg <- convertElabType tyArg
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
  case collectApps term of
    (headTerm, args) ->
      case constructorHead headTerm of
        Just (constructorName, headTypeArgs) ->
          case Map.lookup constructorName (ccConstructors context) of
            Just constructorMeta -> do
              let constructor = cmBackend constructorMeta
                  parameters = constructorTypeParameters constructorMeta
                  rawFields = backendConstructorFields constructor
              if length args == length rawFields
                then do
                  typeBounds <- backendTypeBoundsFromEnv env
                  initialSubstitution <- constructorTypeApplicationSubstitution constructorMeta headTypeArgs
                  resultSubstitution <-
                    case matchBackendTypeParameters typeBounds parameters initialSubstitution (backendConstructorResult constructor) resultTy of
                      Just substitution -> Right substitution
                      Nothing ->
                        Left
                          ( BackendUnsupportedCaseShape
                              ("constructor result type does not match expected result for `" ++ backendConstructorName constructor ++ "`")
                          )
                  substitution <-
                    foldM
                      (matchConstructorApplicationArgument env typeBounds parameters)
                      resultSubstitution
                      (zip rawFields args)
                  let completedSubstitution = completeBackendParameterSubstitution parameters substitution
                      fields = map (substituteBackendTypes completedSubstitution) rawFields
                  argExprs <- zipWithM (convertTermExpected context env . Just) fields args
                  Right
                    ( Just
                        BackendConstruct
                          { backendExprType = resultTy,
                            backendConstructName = backendConstructorName constructor,
                            backendConstructArgs = argExprs
                          }
                    )
                else Right Nothing
            Nothing -> Right Nothing
        Nothing -> Right Nothing

constructorTypeParameters :: ConstructorMeta -> BackendParameterBounds
constructorTypeParameters constructorMeta =
  constructorTypeParameterBoundsFor (dmBackend (cmData constructorMeta)) (cmBackend constructorMeta)

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
  Right (Map.fromList (zip typeApplicationNames typeArgs))
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

constructorHead :: ElabTerm -> Maybe (String, [BackendType])
constructorHead term =
  case collectConstructorHeadTypes [] term of
    Just (name, typeArgs) ->
      case traverse convertElabType typeArgs of
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
        scrutineeTy <- inferBackendType env scrutineeTerm
        Right (scrutineeTy, scrutineeDataHint context scrutineeTerm)

scrutineeDataHint :: ConvertContext -> ElabTerm -> Maybe DataMeta
scrutineeDataHint context term =
  case stripTypeInsts term of
    EVar name -> Map.lookup name (ccBindingData context)
    _ -> Nothing

constructorApplicationResultType :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError (Maybe (BackendType, Maybe DataMeta))
constructorApplicationResultType context env term =
  case collectApps term of
    (headTerm, args) ->
      case constructorHead headTerm of
        Just (constructorName, headTypeArgs) ->
          case Map.lookup constructorName (ccConstructors context) of
            Just constructorMeta
              | length args == length fields -> do
                  typeBounds <- backendTypeBoundsFromEnv env
                  initialSubstitution <- constructorTypeApplicationSubstitution constructorMeta headTypeArgs
                  substitution <-
                    foldM
                      (matchConstructorApplicationArgument env typeBounds parameters)
                      initialSubstitution
                      (zip fields args)
                  let resultTy =
                        substituteBackendTypes
                          (completeBackendParameterSubstitution parameters substitution)
                          (backendConstructorResult constructor)
                  Right (Just (resultTy, Just (cmData constructorMeta)))
              | otherwise -> Right Nothing
              where
                constructor = cmBackend constructorMeta
                fields = backendConstructorFields constructor
                parameters = constructorTypeParameters constructorMeta
            Nothing -> Right Nothing
        Nothing -> Right Nothing

matchConstructorApplicationArgument ::
  Env ->
  BackendTypeBounds ->
  BackendParameterBounds ->
  Map String BackendType ->
  (BackendType, ElabTerm) ->
  Either BackendConversionError (Map String BackendType)
matchConstructorApplicationArgument env typeBounds parameters substitution (expectedTy, arg) =
  -- This is only a best-effort way to recover constructor type parameters.
  -- Expected-type conversion of the argument remains authoritative because it
  -- can canonicalize nested constructor applications before validation.
  case inferBackendType env arg of
    Right actualTy ->
      case matchBackendTypeParameters typeBounds parameters substitution expectedTy actualTy of
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
        case matchBackendTypeParameters typeBounds (constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor) Map.empty (backendConstructorResult constructor) scrutineeTy of
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
  case typeCheckWithEnv env term of
    Right ty -> convertElabType ty
    Left err -> Left (BackendTypeCheckFailed term err)

extendTermEnv :: String -> ElabType -> Env -> Env
extendTermEnv name ty env =
  env {termEnv = Map.insert name ty (termEnv env)}

extendTypeEnv :: String -> ElabType -> Env -> Env
extendTypeEnv name ty env =
  env {typeEnv = Map.insert name ty (typeEnv env)}

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
  BackendParameterBounds ->
  Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map String BackendType)
matchBackendTypeParameters typeBounds parameterBounds =
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

    backendParameterBoundMatches name actual substitution =
      case Map.lookup name parameterBounds of
        Just (Just _)
          | BTVar actualName <- actual,
            actualName == name ->
              True
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let expectedBound = substituteBackendTypes (Map.delete name substitution) boundTy
               in alphaEqBackendType actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVar actualName ->
          case Map.lookup actualName typeBounds of
            Just (Just actualBound) ->
              alphaEqBackendType actualBound expectedBound
            _ ->
              False
        _ ->
          False

completeBackendParameterSubstitution :: BackendParameterBounds -> Map String BackendType -> Map String BackendType
completeBackendParameterSubstitution parameterBounds substitution0 =
  foldl insertBoundDefault substitution0 (Map.toList parameterBounds)
  where
    insertBoundDefault substitution (name, Just boundTy)
      | Map.member name substitution = substitution
      | alphaEqBackendType boundTy BTBottom = substitution
      | otherwise = Map.insert name (substituteBackendTypes substitution boundTy) substitution
    insertBoundDefault substitution _ =
      substitution
