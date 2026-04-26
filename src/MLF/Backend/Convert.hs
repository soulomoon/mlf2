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

import Control.Monad (foldM, when, zipWithM)
import Data.List (find)
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
  | BackendUnsupportedCaseShape String
  | BackendTypeCheckFailed ElabTerm TypeCheckError
  | BackendValidationFailed BackendValidationError
  deriving (Eq, Show)

data ConvertContext = ConvertContext
  { ccModuleScopes :: Map String ElaborateScope,
    ccConstructors :: Map String ConstructorMeta,
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

convertCheckedProgram :: CheckedProgram -> Either BackendConversionError BackendProgram
convertCheckedProgram checked = do
  context <- buildConvertContext checked
  modules0 <- mapM (convertCheckedModule context initialEnv) (checkedProgramModules checked)
  let program =
        BackendProgram
          { backendProgramModules = modules0,
            backendProgramMain = checkedProgramMain checked
          }
  case validateBackendProgram program of
    Right () -> Right program
    Left err -> Left (BackendValidationFailed err)
  where
    initialEnv =
      Env
        { termEnv =
            Map.fromList
              [ (checkedBindingName binding, checkedBindingType binding)
                | checkedModule <- checkedProgramModules checked,
                  binding <- checkedModuleBindings checkedModule
              ],
          typeEnv = Map.empty
        }

convertCheckedModule :: ConvertContext -> Env -> CheckedModule -> Either BackendConversionError BackendModule
convertCheckedModule context env checkedModule = do
  dataDecls <- mapM (convertDataInfo context) (Map.elems (checkedModuleData checkedModule))
  bindings <- mapM (convertCheckedBinding context env) (checkedModuleBindings checkedModule)
  Right
    BackendModule
      { backendModuleName = checkedModuleName checkedModule,
        backendModuleData = dataDecls,
        backendModuleBindings = bindings
      }

convertCheckedBinding :: ConvertContext -> Env -> CheckedBinding -> Either BackendConversionError BackendBinding
convertCheckedBinding context env binding = do
  bindingTy <- convertElabType (checkedBindingType binding)
  expr <- convertTermExpected context env (Just bindingTy) (checkedBindingTerm binding)
  Right
    BackendBinding
      { backendBindingName = checkedBindingName binding,
        backendBindingType = bindingTy,
        backendBindingExpr = expr,
        backendBindingExportedAsMain = checkedBindingExportedAsMain binding
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
  Right
    ConvertContext
      { ccModuleScopes = moduleScopes,
        ccConstructors = Map.fromList constructorMetas,
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
  Map.mapMaybe (\symbol -> Map.lookup (resolvedSymbolIdentity symbol) dataByIdentity)

qualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
qualifiedDataInfoMap dataInfos =
  Map.fromList [(qualifiedDataName info, info) | info <- dataInfos]

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
    [ (dataName candidate, candidate)
      | candidate <- dataInfos,
        dataModule candidate == dataModule info
    ]

uniqueUnqualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
uniqueUnqualifiedDataInfoMap dataInfos =
  Map.fromList
    [ (name, info)
      | (name, infos) <- Map.toList grouped,
        [info] <- [infos]
    ]
  where
    grouped =
      Map.fromListWith (++)
        [ (dataName info, [info])
          | info <- dataInfos
        ]

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
  fields <- mapM (convertLoweredSourceType scope) (ctorArgs info)
  resultTy <- convertLoweredSourceType scope (ctorResult info)
  Right
    BackendConstructor
      { backendConstructorName = ctorRuntimeName info,
        backendConstructorFields = fields,
        backendConstructorResult = resultTy
      }

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
          Nothing -> do
            inferredTy <- inferBackendType env term
            convertOrdinaryTerm context env term inferredTy
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
          envForRhs = extendTermEnv name schemeTy env
      bindingTy <- convertElabType schemeTy
      rhsExpr <- convertTermExpected context envForRhs (Just bindingTy) rhs
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
              BTForall _ _ bodyTy -> Just bodyTy
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
      if backendExprType innerExpr == resultTy
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
              | backendExprType innerExpr == resultTy -> Right innerExpr
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
      case constructorHeadName headTerm >>= (`Map.lookup` ccConstructors context) of
        Just constructorMeta -> do
          let fields = backendConstructorFields (cmBackend constructorMeta)
          if length args == length fields
            then do
              argExprs <- zipWithM (convertTermExpected context env . Just) fields args
              Right
                ( Just
                    BackendConstruct
                      { backendExprType = resultTy,
                        backendConstructName = backendConstructorName (cmBackend constructorMeta),
                        backendConstructArgs = argExprs
                      }
                )
            else Right Nothing
        Nothing -> Right Nothing

constructorHeadName :: ElabTerm -> Maybe String
constructorHeadName term =
  case stripTypeInsts term of
    EVar name -> Just name
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
              Nothing -> requireCaseData context (backendExprType scrutineeExpr)
          let constructors = backendDataConstructors (dmBackend dataMeta)
          when (length args /= length constructors) $
            Left
              ( BackendUnsupportedCaseShape
                  ("handler count does not match constructor count for `" ++ backendDataName (dmBackend dataMeta) ++ "`")
              )
          alternatives <- zipWithMCase (convertCaseAlternative context env resultTy) constructors args
          Right
            ( Just
                BackendCase
                  { backendExprType = resultTy,
                    backendScrutinee = scrutineeExpr,
                    backendAlternatives = alternatives
                  }
            )

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
        Right (scrutineeTy, Nothing)

constructorApplicationResultType :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError (Maybe (BackendType, Maybe DataMeta))
constructorApplicationResultType context env term =
  case collectApps term of
    (headTerm, args) ->
      case constructorHeadName headTerm >>= (`Map.lookup` ccConstructors context) of
        Just constructorMeta
          | length args == length fields -> do
              substitution <-
                foldM
                  (matchConstructorApplicationArgument env parameters constructor)
                  Map.empty
                  (zip fields args)
              let resultTy = substituteBackendTypesLocal substitution (backendConstructorResult constructor)
              Right (Just (resultTy, Just (cmData constructorMeta)))
          | otherwise -> Right Nothing
          where
            constructor = cmBackend constructorMeta
            fields = backendConstructorFields constructor
            parameters = Set.fromList (backendDataParameters (dmBackend (cmData constructorMeta)))
        Nothing -> Right Nothing

matchConstructorApplicationArgument ::
  Env ->
  Set.Set String ->
  BackendConstructor ->
  Map String BackendType ->
  (BackendType, ElabTerm) ->
  Either BackendConversionError (Map String BackendType)
matchConstructorApplicationArgument env parameters constructor substitution (expectedTy, arg) = do
  actualTy <- inferBackendType env arg
  case matchBackendTypeParameters parameters substitution expectedTy actualTy of
    Just substitution' -> Right substitution'
    Nothing ->
      Left
        ( BackendUnsupportedCaseShape
            ("constructor argument type does not match case scrutinee constructor `" ++ backendConstructorName constructor ++ "`")
        )

substituteBackendTypesLocal :: Map String BackendType -> BackendType -> BackendType
substituteBackendTypesLocal substitution ty =
  Map.foldrWithKey substituteBackendType ty substitution

requireCaseData :: ConvertContext -> BackendType -> Either BackendConversionError DataMeta
requireCaseData context scrutineeTy =
  case filter (dataMatchesScrutinee scrutineeTy) (ccData context) of
    [dataMeta] -> Right dataMeta
    [] -> Left (BackendUnsupportedCaseShape ("no backend data matches scrutinee type " ++ show scrutineeTy))
    matches ->
      Left
        ( BackendUnsupportedCaseShape
            ("ambiguous backend data matches scrutinee type " ++ show scrutineeTy ++ ": " ++ show (map (backendDataName . dmBackend) matches))
        )

dataMatchesScrutinee :: BackendType -> DataMeta -> Bool
dataMatchesScrutinee scrutineeTy dataMeta =
  any
    ( \constructor ->
        case matchBackendTypeParameters (Set.fromList (backendDataParameters (dmBackend dataMeta))) Map.empty (backendConstructorResult constructor) scrutineeTy of
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
  let (params, body) = collectLeadingLams handler
      fields = backendConstructorFields constructor
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
  when (backendExprType bodyExpr /= resultTy) $
    Left
      ( BackendUnsupportedCaseShape
          ("handler result type does not match case result for `" ++ backendConstructorName constructor ++ "`")
      )
  Right
    BackendAlternative
      { backendAltPattern = BackendConstructorPattern (backendConstructorName constructor) (map fst params),
        backendAltBody = bodyExpr
      }

collectLeadingLams :: ElabTerm -> ([(String, ElabType)], ElabTerm)
collectLeadingLams =
  go []
  where
    go params term =
      case term of
        ELam name ty body -> go (params ++ [(name, ty)]) body
        other -> (params, other)

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
  Set.Set String ->
  Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map String BackendType)
matchBackendTypeParameters parameters =
  go Map.empty Map.empty
  where
    go leftEnv rightEnv substitution expected actual =
      case expected of
        BTVar name
          | Set.member name parameters,
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
        Nothing -> Just (Map.insert name actual substitution)
        Just previous
          | previous == actual -> Just substitution
        _ -> Nothing
