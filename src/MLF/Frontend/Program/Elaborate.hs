{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeRuntimeTypes,
    mkElaborateScope,
    lowerConstructorBinding,
    lowerExprBinding,
    inferClassArgument,
    lowerType,
    resolveInstanceInfo,
  )
where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, get, gets, modify, runState)
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Frontend.Normalize (substSrcType)
import MLF.Frontend.Program.Surface
  ( surfaceAnn,
    surfaceApp,
    surfaceLam,
    surfaceLamAnn,
    surfaceLet,
    surfaceLit,
    surfaceVar,
  )
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax
  ( Lit (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    SurfaceExpr,
  )
import MLF.Frontend.Syntax.Program (Expr (..))
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Reify.TypeOps (freshNameLike)

data ElaborateScope = ElaborateScope
  { esValues :: Map String ValueInfo,
    esRuntimeTypes :: Map String SrcType,
    esTypes :: Map String DataInfo,
    esInstances :: [InstanceInfo]
  }

data ElaborateState = ElaborateState
  { elaborateFreshCounter :: Int,
    elaborateDeferredMethods :: Map String DeferredMethodCall,
    elaborateExternalTypes :: Map String SrcType,
    elaborateAnnotateNullaryConstructors :: Bool
  }

type ElaborateM a = ExceptT ProgramError (State ElaborateState) a

data ElaborateResult a = ElaborateResult
  { elaborateResultValue :: a,
    elaborateResultDeferredMethods :: Map String DeferredMethodCall,
    elaborateResultExternalTypes :: Map String SrcType
  }

runElaborateM :: ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateM action =
  let initialState =
        ElaborateState
          { elaborateFreshCounter = 0,
            elaborateDeferredMethods = Map.empty,
            elaborateExternalTypes = Map.empty,
            elaborateAnnotateNullaryConstructors = False
          }
      (result, finalState) = runState (runExceptT action) initialState
   in case result of
        Left err -> Left err
        Right value ->
          Right
            ElaborateResult
              { elaborateResultValue = value,
                elaborateResultDeferredMethods = elaborateDeferredMethods finalState,
                elaborateResultExternalTypes = elaborateExternalTypes finalState
              }

mkElaborateScope :: Map String ValueInfo -> Map String DataInfo -> [InstanceInfo] -> ElaborateScope
mkElaborateScope values0 dataTypes instances0 =
  ElaborateScope
    { esValues = values0,
      esRuntimeTypes =
        Map.fromList
          [ (runtimeNameFor info, lowerTypeRaw dataTypes (valueTypeFor info))
            | info <- Map.elems values0,
              shouldTrackRuntimeType info
          ]
          `Map.union` Map.fromList
            [ (runtimeNameFor methodInfo, lowerTypeRaw dataTypes (valueTypeFor methodInfo))
              | instanceInfo <- instances0,
                methodInfo <- Map.elems (instanceMethods instanceInfo),
                shouldTrackRuntimeType methodInfo
            ],
      esTypes = dataTypes,
      esInstances = instances0
    }
  where
    shouldTrackRuntimeType OverloadedMethod {} = False
    shouldTrackRuntimeType _ = True

    runtimeNameFor OrdinaryValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor ConstructorValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor OverloadedMethod {} = error "overloaded methods do not have runtime names"

    valueTypeFor OrdinaryValue {valueType = ty} = ty
    valueTypeFor ConstructorValue {valueType = ty} = ty
    valueTypeFor OverloadedMethod {} = error "overloaded methods do not have concrete runtime types"

elaborateScopeRuntimeTypes :: ElaborateScope -> Map String SrcType
elaborateScopeRuntimeTypes = esRuntimeTypes

elaborateScopeDataTypes :: ElaborateScope -> Map String DataInfo
elaborateScopeDataTypes = esTypes

lowerType :: ElaborateScope -> SrcType -> SrcType
lowerType scope = lowerTypeRaw (esTypes scope)

lowerTypeRaw :: Map String DataInfo -> SrcType -> SrcType
lowerTypeRaw dataTypes = lower Map.empty Nothing
  where
    lower subst currentData ty = case ty of
      STVar name -> Map.findWithDefault ty name subst
      STArrow dom cod -> STArrow (lower subst currentData dom) (lower subst currentData cod)
      STBase name ->
        case Map.lookup name dataTypes of
          Just info -> encodeDataType subst info []
          Nothing -> STBase name
      STCon name args ->
        case Map.lookup name dataTypes of
          Just info -> encodeDataType subst info (map (lower subst currentData) (toListNE args))
          Nothing -> STCon name (fmap (lower subst currentData) args)
      STForall name mb body ->
        let subst' = Map.delete name subst
         in STForall name (fmap (SrcBound . lower subst' currentData . unSrcBound) mb) (lower subst' currentData body)
      STMu name body -> STMu name (lower (Map.delete name subst) currentData body)
      STBottom -> STBottom

    encodeDataType subst info actualArgs =
      let actualArgs' =
            if null actualArgs
              then map STVar (dataParams info)
              else actualArgs
          selfName = "$" ++ dataName info ++ "_self"
          resultName = "$" ++ dataName info ++ "_result"
          paramSubst = Map.union (Map.fromList (zip (dataParams info) actualArgs')) subst
       in STMu selfName (STForall resultName Nothing (handlerChain info paramSubst (STVar selfName) (STVar resultName)))

    handlerChain info subst selfTy resultTy =
      foldr
        STArrow
        resultTy
        [ foldr
            (\(name, mbBound) acc -> STForall name (fmap (SrcBound . lowerCtorArg subst (Just (dataName info)) selfTy) mbBound) acc)
            (foldr STArrow resultTy (map (lowerCtorArg subst (Just (dataName info)) selfTy) (ctorArgs ctor)))
            (ctorForalls ctor)
          | ctor <- dataConstructors info
        ]

    lowerCtorArg subst currentData selfTy ty = case ty of
      STVar name -> Map.findWithDefault ty name subst
      STArrow dom cod -> STArrow (lowerCtorArg subst currentData selfTy dom) (lowerCtorArg subst currentData selfTy cod)
      STBase name
        | Just name == currentData -> selfTy
        | otherwise ->
            case Map.lookup name dataTypes of
              Just info -> encodeDataType subst info []
              Nothing -> STBase name
      STCon name args
        | Just name == currentData -> selfTy
        | otherwise ->
            case Map.lookup name dataTypes of
              Just info -> encodeDataType subst info (map (lowerCtorArg subst currentData selfTy) (toListNE args))
              Nothing -> STCon name (fmap (lowerCtorArg subst currentData selfTy) args)
      STForall name mb body ->
        let subst' = Map.delete name subst
         in STForall name (fmap (SrcBound . lowerCtorArg subst' currentData selfTy . unSrcBound) mb) (lowerCtorArg subst' currentData selfTy body)
      STMu name body -> STMu name (lowerCtorArg (Map.delete name subst) currentData selfTy body)
      STBottom -> STBottom

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

lowerConstructorBinding :: ElaborateScope -> ConstructorInfo -> LoweredBinding
lowerConstructorBinding scope ctorInfo =
  LoweredBinding
    { loweredBindingName = ctorRuntimeName ctorInfo,
      loweredBindingExpectedType = lowerType scope (ctorType ctorInfo),
      loweredBindingSurfaceExpr = constructorSurfaceExpr scope ctorInfo,
      loweredBindingDeferredMethods = Map.empty,
      loweredBindingExternalTypes = Map.empty,
      loweredBindingExportedAsMain = False
    }

lowerExprBinding :: ElaborateScope -> String -> SrcType -> Bool -> P.Expr -> Either ProgramError LoweredBinding
lowerExprBinding scope runtimeName expectedTy exportedAsMain expr = do
  result <- runElaborateM (compileExpr scope (Just expectedTy) expr)
  pure
    LoweredBinding
      { loweredBindingName = runtimeName,
        loweredBindingExpectedType = lowerType scope expectedTy,
        loweredBindingSurfaceExpr = elaborateResultValue result,
        loweredBindingDeferredMethods = elaborateResultDeferredMethods result,
        loweredBindingExternalTypes = elaborateResultExternalTypes result,
        loweredBindingExportedAsMain = exportedAsMain
      }

constructorSurfaceExpr :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExpr scope ctorInfo =
  surfaceAnn (constructorSurfaceExprRaw scope ctorInfo) (lowerType scope (ctorType ctorInfo))

constructorSurfaceExprRaw :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExprRaw scope ctorInfo =
  let argNames = ["$" ++ ctorName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
      handlerNames = ["$" ++ ctorName ctorInfo ++ "_k" ++ show ix | ix <- [1 .. length ctorOrder]]
      resultVar =
        if any (not . null . ctorForalls) ctorOrder
          then "$" ++ ctorOwningType ctorInfo ++ "_result"
          else "a"
      handlerTypes = map (\ctor -> handlerSurfaceType scope ctor (STVar resultVar)) ctorOrder
      selectedHandler =
        foldl
          surfaceApp
          (surfaceVar (handlerNames !! ctorIndex ctorInfo))
          (map surfaceVar argNames)
      body = foldr (\(handlerName, handlerTy) acc -> surfaceLamAnn handlerName handlerTy acc) selectedHandler (zip handlerNames handlerTypes)
      lifted =
        foldr
          (\(argName, argTy) acc -> surfaceLamAnn argName (lowerType scope argTy) acc)
          body
          (zip argNames (ctorArgs ctorInfo))
   in lifted
  where
    ctorOrder =
      maybe [] dataConstructors (Map.lookup (ctorOwningType ctorInfo) (esTypes scope))

compileExpr :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpr scope mbExpected expr = case expr of
  EVar name ->
    case Map.lookup name (esValues scope) of
      Just OverloadedMethod {} -> throwError (ProgramAmbiguousMethodUse name)
      Just OrdinaryValue {valueRuntimeName = runtimeName} -> pure (surfaceVar runtimeName)
      Just ConstructorValue {valueCtorInfo = ctorInfo} -> do
        annotateNullary <- gets elaborateAnnotateNullaryConstructors
        let ctorExpr =
              if null (ctorArgs ctorInfo) && not annotateNullary
                then constructorSurfaceExprRaw scope ctorInfo
                else constructorSurfaceExpr scope ctorInfo
        pure $ case mbExpected of
          Just expectedTy
            | null (ctorArgs ctorInfo), isRecursiveResultType expectedTy -> surfaceAnn ctorExpr (lowerType scope expectedTy)
          _ -> ctorExpr
      Nothing -> throwError (ProgramUnknownValue name)
  ELit lit -> pure (surfaceLit lit)
  ELam param body -> do
    runtimeName <- freshRuntimeName (P.paramName param)
    let paramTy = case (P.paramType param, mbExpected) of
          (Just ty, _) -> Just ty
          (Nothing, Just (STArrow dom _)) -> Just dom
          _ -> Nothing
    scope' <- extendLocal scope (P.paramName param) runtimeName paramTy
    bodyExpr0 <- compileExpr scope' (expectedCodomain mbExpected) body
    let bodyExpr =
          case expectedCodomain mbExpected of
            Just codTy | isRecursiveResultType codTy -> surfaceAnn bodyExpr0 (lowerType scope codTy)
            _ -> bodyExpr0
    pure $
      case paramTy of
        Just ty -> surfaceLamAnn runtimeName (lowerType scope ty) bodyExpr
        Nothing -> surfaceLam runtimeName bodyExpr
  EApp _ _ -> compileApp scope mbExpected expr
  ELet name mbTy rhs body -> do
    runtimeName <- freshRuntimeName name
    let recursive = mentionsFreeValue name rhs
    provisionalTy <- case (recursive, mbTy) of
      (True, Nothing) -> Just <$> freshTypeName
      _ -> pure mbTy
    selfScope <-
      if recursive
        then extendLocal scope name runtimeName provisionalTy
        else pure scope
    rhsExpr <- compileExpr selfScope provisionalTy rhs
    bindingTy <- case mbTy of
      Just ty -> pure (lowerType scope ty)
      Nothing
        | Just rhsTy <- explicitExprAnnotation rhs -> pure (lowerType scope rhsTy)
        | Just rhsTy <- inferKnownExprType selfScope rhs -> pure (lowerType scope rhsTy)
        | Just ty <- provisionalTy -> pure (lowerType scope ty)
      Nothing -> freshTypeName
    let rhsExpr' =
          case mbTy of
            Just ty -> surfaceAnn rhsExpr (lowerType scope ty)
            Nothing -> rhsExpr
    bodyScope <- extendLocalLowered scope name runtimeName bindingTy
    bodyExpr <- compileExpr bodyScope mbExpected body
    pure (surfaceLet runtimeName rhsExpr' bodyExpr)
  EAnn inner annTy -> do
    innerExpr <- compileExpr scope (Just annTy) inner
    pure (surfaceAnn innerExpr (lowerType scope annTy))
  ECase scrutinee alts -> compileCase scope mbExpected scrutinee alts

compileApp :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileApp scope mbExpected expr =
  case collectApps expr of
    (EVar name, args)
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope) ->
          compileMethodApp scope mbExpected methodInfo args
      | Just valueInfo <- Map.lookup name (esValues scope) ->
          compileValueApp scope mbExpected valueInfo args
    (headExpr, args) -> do
      headSurface <- compileExpr scope Nothing headExpr
      argSurfaces <- mapM (compileExpr scope Nothing) args
      pure (foldl surfaceApp headSurface argSurfaces)

explicitExprAnnotation :: P.Expr -> Maybe SrcType
explicitExprAnnotation expr =
  case expr of
    EAnn _ ty -> Just ty
    _ -> Nothing

compileValueApp :: ElaborateScope -> Maybe SrcType -> ValueInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileValueApp scope mbExpected valueInfo args = do
  argSurfaces <-
    case valueInfo of
      ConstructorValue {valueCtorInfo = ctorInfo} ->
        zipWithM (\ty arg -> compileExpr scope (Just ty) arg) (take (length args) (ctorArgs ctorInfo)) args
      _ -> mapM (compileExpr scope Nothing) args
  let headSurface =
        case valueInfo of
          OrdinaryValue {valueRuntimeName = runtimeName} -> surfaceVar runtimeName
          ConstructorValue {valueCtorInfo = ctorInfo} -> constructorSurfaceExpr scope ctorInfo
          OverloadedMethod {} -> error "compileValueApp does not handle overloaded methods"
      applied = foldl surfaceApp headSurface argSurfaces
      knownResultTy = appliedValueResultType valueInfo (length args)
  pure $ case (valueInfo, mbExpected, knownResultTy) of
    (ConstructorValue {}, Just expectedTy, _)
      | isRecursiveResultType expectedTy -> surfaceAnn applied (lowerType scope expectedTy)
    (ConstructorValue {}, _, Just resultTy)
      | isRecursiveResultType resultTy -> surfaceAnn applied (lowerType scope resultTy)
    _ -> applied

compileMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileMethodApp scope _mbExpected methodInfo args
  | null args = throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
  | otherwise = do
      argSurfaces <- withNullaryConstructorAnnotations (mapM (compileExpr scope Nothing) args)
      placeholder <- deferMethodCall scope methodInfo args
      pure (foldl surfaceApp (surfaceVar placeholder) argSurfaces)

withNullaryConstructorAnnotations :: ElaborateM a -> ElaborateM a
withNullaryConstructorAnnotations action = do
  wasAnnotating <- gets elaborateAnnotateNullaryConstructors
  modify (\state -> state {elaborateAnnotateNullaryConstructors = True})
  result <- action
  modify (\state -> state {elaborateAnnotateNullaryConstructors = wasAnnotating})
  pure result

deferMethodCall :: ElaborateScope -> MethodInfo -> [P.Expr] -> ElaborateM String
deferMethodCall scope methodInfo args = do
  placeholder <- freshDeferredMethodName (methodName methodInfo)
  let placeholderTy = lowerType scope (placeholderMethodType scope methodInfo args)
      deferred =
        DeferredMethodCall
          { deferredMethodPlaceholder = placeholder,
            deferredMethodInfo = methodInfo,
            deferredMethodArgCount = length args,
            deferredMethodName = methodName methodInfo
          }
  modify
    ( \state ->
        state
          { elaborateDeferredMethods = Map.insert placeholder deferred (elaborateDeferredMethods state),
            elaborateExternalTypes = Map.insert placeholder placeholderTy (elaborateExternalTypes state)
          }
    )
  pure placeholder

placeholderMethodType :: ElaborateScope -> MethodInfo -> [P.Expr] -> SrcType
placeholderMethodType scope methodInfo args =
  let paramName = methodParamName methodInfo
      methodTy = methodType methodInfo
      (foralls, _) = splitForalls methodTy
      quantifiedMethodTy =
        if any ((== paramName) . fst) foralls || paramName `Set.notMember` freeTypeVarsSrcType methodTy
          then methodTy
          else STForall paramName Nothing methodTy
      knownClassArg = do
        argTypes <- traverse (inferKnownExprType scope) args
        inferClassArgument (lowerType scope methodTy) paramName argTypes
   in case knownClassArg of
        Just classArgTy -> stripVacuousSrcForalls (specializeMethodType methodTy paramName classArgTy)
        Nothing -> loosenPlaceholderArgumentTypes args quantifiedMethodTy

loosenPlaceholderArgumentTypes :: [P.Expr] -> SrcType -> SrcType
loosenPlaceholderArgumentTypes args methodTy =
  let (foralls, bodyTy) = splitForalls methodTy
      (argTys, resultTy) = splitArrows bodyTy
      placeholderArgTys =
        if any needsLoosePlaceholderArg args
          then replicate (length argTys) STBottom
          else argTys
      placeholderBody = foldr STArrow resultTy placeholderArgTys
   in foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) placeholderBody foralls

needsLoosePlaceholderArg :: P.Expr -> Bool
needsLoosePlaceholderArg expr =
  case stripExprAnn expr of
    EApp fun _ ->
      case stripExprAnn fun of
        ELam {} -> True
        _ -> False
    _ -> False

stripExprAnn :: P.Expr -> P.Expr
stripExprAnn expr =
  case expr of
    EAnn inner _ -> stripExprAnn inner
    _ -> expr

inferKnownExprType :: ElaborateScope -> P.Expr -> Maybe SrcType
inferKnownExprType scope expr =
  case expr of
    ELit lit -> Just (litSrcType lit)
    EVar name ->
      case Map.lookup name (esValues scope) of
        Just OrdinaryValue {valueType = ty} -> Just ty
        Just ConstructorValue {valueType = ty} -> Just ty
        _ -> Nothing
    EAnn _ annTy -> Just annTy
    EApp _ _ ->
      case collectApps expr of
        (EVar name, args)
          | Just valueInfo <- Map.lookup name (esValues scope) ->
              appliedValueResultType valueInfo (length args)
        _ -> Nothing
    _ -> Nothing

litSrcType :: Lit -> SrcType
litSrcType lit =
  case lit of
    LInt _ -> STBase "Int"
    LBool _ -> STBase "Bool"
    LString _ -> STBase "String"

appliedValueResultType :: ValueInfo -> Int -> Maybe SrcType
appliedValueResultType valueInfo argCount =
  case valueInfo of
    OrdinaryValue {valueType = ty} -> peelAppliedType ty argCount
    ConstructorValue {valueCtorInfo = ctorInfo} ->
      if argCount > length (ctorArgs ctorInfo)
        then Nothing
        else Just (foldr STArrow (ctorResult ctorInfo) (drop argCount (ctorArgs ctorInfo)))
    OverloadedMethod {} -> Nothing

peelAppliedType :: SrcType -> Int -> Maybe SrcType
peelAppliedType ty argCount =
  let (_, bodyTy) = splitForalls ty
      (argTys, resultTy) = splitArrows bodyTy
   in if argCount > length argTys
        then Nothing
        else Just (foldr STArrow resultTy (drop argCount argTys))

compileCase :: ElaborateScope -> Maybe SrcType -> P.Expr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCase scope mbExpected scrutinee alts = do
  case ctorOwners alts of
    [] -> do
      scrutineeExpr <- compileExpr scope Nothing scrutinee
      compileCatchAllOnly scope mbExpected scrutineeExpr alts
    owners -> do
      dataInfo <- requireSingleDataOwner scope owners
      let headTy = dataHeadType dataInfo
          variableScrutinee =
            case scrutinee of
              EVar _ -> True
              EAnn inner _ -> case inner of
                EVar _ -> True
                _ -> False
              _ -> False
          constructorScrutinee = isConstructorCall scope scrutinee
          localOrdinaryScrutinee = isLocalOrdinaryCall scope scrutinee
          shouldSpecializeHere = shouldSpecializeCaseScrutinee scrutinee
          scrutineeExpected = if shouldSpecializeHere && not localOrdinaryScrutinee then Just headTy else Nothing
          ctorNames = [name | P.Alt (P.PatCtor name _) _ <- alts]
          dupes = duplicates ctorNames
      case dupes of
        (dup : _) -> throwError (ProgramDuplicateCaseBranch dup)
        [] -> pure ()
      catchAll <- pure (findCatchAll alts)
      (resultTy, _quantifyResult) <-
        case mbExpected of
          Just expectedTy -> pure (expectedTy, False)
          Nothing -> do
            resultVar <- freshTypeVarName
            pure (STVar resultVar, True)
      let shouldAnnotateScrutinee = isJust mbExpected && shouldSpecializeHere && not localOrdinaryScrutinee
      scrutineeExpr <-
        if shouldAnnotateScrutinee && constructorScrutinee
          then compileConstructorScrutineeRaw scope scrutinee
          else compileExpr scope scrutineeExpected scrutinee
      handlers <- mapM (compileHandler scope scrutineeExpr resultTy headTy dataInfo alts catchAll False) (dataConstructors dataInfo)
      let scrutineeWithHeadTy =
            if constructorScrutinee && isRecursiveResultType headTy && not shouldAnnotateScrutinee
              then surfaceAnn scrutineeExpr (lowerType scope headTy)
              else scrutineeExpr
          recursiveVariableScrutinee = variableScrutinee && isRecursiveResultType resultTy
      let scrutineeCaseExpr =
            if shouldAnnotateScrutinee
              then
                if recursiveVariableScrutinee
                  then surfaceAnn scrutineeExpr (lowerType scope headTy)
                  else surfaceAnn scrutineeWithHeadTy (caseScrutineeType scope dataInfo resultTy)
              else scrutineeWithHeadTy
      pure (foldl surfaceApp scrutineeCaseExpr handlers)

compileConstructorScrutineeRaw :: ElaborateScope -> P.Expr -> ElaborateM SurfaceExpr
compileConstructorScrutineeRaw scope expr =
  case collectApps expr of
    (EVar name, args)
      | Just ConstructorValue {valueCtorInfo = ctorInfo} <- Map.lookup name (esValues scope) -> do
          argSurfaces <-
            zipWithM
              (\ty arg -> compileExpr scope (Just ty) arg)
              (take (length args) (ctorArgs ctorInfo))
              args
          pure (foldl surfaceApp (constructorSurfaceExprRaw scope ctorInfo) argSurfaces)
    (EAnn inner _, []) -> compileConstructorScrutineeRaw scope inner
    _ -> compileExpr scope Nothing expr

compileCatchAllOnly :: ElaborateScope -> Maybe SrcType -> SurfaceExpr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCatchAllOnly scope mbExpected scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> compileExpr scope mbExpected body
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName name
      scope' <- extendLocalLowered scope name runtimeName =<< freshTypeName
      bodyExpr <- compileExpr scope' mbExpected body
      pure (surfaceLet runtimeName scrutineeExpr bodyExpr)
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileHandler :: ElaborateScope -> SurfaceExpr -> SrcType -> SrcType -> DataInfo -> [P.Alt] -> Maybe P.Alt -> Bool -> ConstructorInfo -> ElaborateM SurfaceExpr
compileHandler scope scrutineeExpr resultTy headTy dataInfo alts catchAll forceAnnotateHandlers ctorInfo =
  case findMatchingAlt ctorInfo of
    Just alt -> compileMatchingAlt scope resultTy headTy forceAnnotateHandlers ctorInfo alt
    Nothing ->
      case catchAll of
        Just alt -> compileCatchAllAlt scope scrutineeExpr resultTy forceAnnotateHandlers ctorInfo alt
        Nothing -> throwError (ProgramNonExhaustiveCase missing)
  where
    missing = [ctorName ctor | ctor <- dataConstructors dataInfo, not (hasAlt ctor)]

    hasAlt ctor =
      any
        ( \case
            P.Alt (P.PatCtor ctorName0 _) _ -> ctorName0 == ctorName ctor
            P.Alt P.PatVar {} _ -> True
            P.Alt P.PatWildcard _ -> True
        )
        alts

    findMatchingAlt ctor =
      find
        ( \case
            P.Alt (P.PatCtor ctorName0 _) _ -> ctorName0 == ctorName ctor
            _ -> False
        )
        alts

compileMatchingAlt :: ElaborateScope -> SrcType -> SrcType -> Bool -> ConstructorInfo -> P.Alt -> ElaborateM SurfaceExpr
compileMatchingAlt scope resultTy headTy forceAnnotateHandlers ctorInfo (P.Alt (P.PatCtor _ binders) body)
  | length binders /= length (ctorArgs ctorInfo) = throwError (ProgramPatternConstructorMismatch (ctorName ctorInfo) headTy)
  | otherwise = do
      runtimeNames <- mapM freshRuntimeName binders
      scope' <- foldM (\acc (srcName, runtimeName, argTy) -> extendLocal acc srcName runtimeName (Just argTy)) scope (zip3 binders runtimeNames (ctorArgs ctorInfo))
      let resultTyElab = lowerType scope resultTy
      bodyExpr <- compileExpr scope' (Just resultTyElab) body
      let handlerBody =
            foldr
              (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
              bodyExpr
              (zip runtimeNames (ctorArgs ctorInfo))
      if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
        then pure handlerBody
        else do
          let handlerTy = handlerSurfaceType scope ctorInfo resultTyElab
          pure (surfaceAnn handlerBody handlerTy)
compileMatchingAlt _ _ headTy _ ctorInfo _ =
  throwError (ProgramPatternConstructorMismatch (ctorName ctorInfo) headTy)

compileCatchAllAlt :: ElaborateScope -> SurfaceExpr -> SrcType -> Bool -> ConstructorInfo -> P.Alt -> ElaborateM SurfaceExpr
compileCatchAllAlt scope scrutineeExpr resultTy forceAnnotateHandlers ctorInfo alt = do
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
  let lambdaBinders = zip runtimeNames (ctorArgs ctorInfo)
  case P.altPattern alt of
    P.PatWildcard -> do
      let resultTyElab = lowerType scope resultTy
      bodyExpr <- compileExpr scope (Just resultTyElab) (P.altExpr alt)
      let handlerBody =
            foldr
              (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
              bodyExpr
              lambdaBinders
      if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
        then pure handlerBody
        else do
          let handlerTy = handlerSurfaceType scope ctorInfo resultTyElab
          pure (surfaceAnn handlerBody handlerTy)
    P.PatVar name -> do
      scrutineeName <- freshRuntimeName name
      scope' <- extendLocalLowered scope name scrutineeName (lowerType scope (ctorResult ctorInfo))
      let resultTyElab = lowerType scope resultTy
      bodyExpr <- compileExpr scope' (Just resultTyElab) (P.altExpr alt)
      let handlerBody =
            foldr
              (\(name', argTy) acc -> surfaceLamAnn name' (lowerType scope argTy) acc)
              (surfaceLet scrutineeName scrutineeExpr bodyExpr)
              lambdaBinders
      if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
        then pure handlerBody
        else do
          let handlerTy = handlerSurfaceType scope ctorInfo resultTyElab
          pure (surfaceAnn handlerBody handlerTy)
    _ -> throwError (ProgramUnknownConstructor (ctorName ctorInfo))

ctorOwners :: [P.Alt] -> [String]
ctorOwners = foldr go []
  where
    go alt acc = case P.altPattern alt of
      P.PatCtor ctorName0 _ -> ctorName0 : acc
      _ -> acc

requireSingleDataOwner :: ElaborateScope -> [String] -> ElaborateM DataInfo
requireSingleDataOwner scope ctorNames0 = do
  ctors <- mapM lookupCtor ctorNames0
  let owners = nub (map ctorOwningType ctors)
  case owners of
    [owner] ->
      case Map.lookup owner (esTypes scope) of
        Just info -> pure info
        Nothing -> throwError (ProgramUnknownType owner)
    _ -> throwError (ProgramCaseOnNonDataType STBottom)
  where
    lookupCtor ctorName0 =
      case Map.lookup ctorName0 (esValues scope) of
        Just ConstructorValue {valueCtorInfo = ctorInfo} -> pure ctorInfo
        _ -> throwError (ProgramUnknownConstructor ctorName0)

findCatchAll :: [P.Alt] -> Maybe P.Alt
findCatchAll = find isCatchAll
  where
    isCatchAll (P.Alt P.PatVar {} _) = True
    isCatchAll (P.Alt P.PatWildcard _) = True
    isCatchAll _ = False

handlerSurfaceType :: ElaborateScope -> ConstructorInfo -> SrcType -> SrcType
handlerSurfaceType scope ctorInfo resultTy =
  let (foralls, argTys) = freshenCtorForallsForResult resultTy (ctorForalls ctorInfo) (ctorArgs ctorInfo)
   in stripVacuousSrcForalls $
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound (fmap lowerBound mbBound)) acc)
          (foldr STArrow resultTy (map (lowerType scope) argTys))
          foralls
  where
    lowerBound = lowerType scope

freshenCtorForallsForResult :: SrcType -> [(String, Maybe SrcType)] -> [SrcType] -> ([(String, Maybe SrcType)], [SrcType])
freshenCtorForallsForResult resultTy foralls0 args0 =
  let initialUsed = Set.union (freeTypeVarsSrcType resultTy) (foldMap (maybe Set.empty freeTypeVarsSrcType . snd) foralls0)
      step (used, accForalls, currentArgs) (name, mbBound) =
        let name' =
              if Set.member name used
                then freshNameLike name (Set.union used (foldMap freeTypeVarsSrcType currentArgs))
                else name
            renameTy =
              if name' == name
                then id
                else substSrcType name (STVar name')
            mbBound' = fmap renameTy mbBound
            args' = map renameTy currentArgs
        in (Set.insert name' used, accForalls ++ [(name', mbBound')], args')
      (_, foralls, args) = foldl' step (initialUsed, [], args0) foralls0
   in (foralls, args)

caseScrutineeType :: ElaborateScope -> DataInfo -> SrcType -> SrcType
caseScrutineeType scope dataInfo resultTy =
  foldr
    STArrow
    (lowerType scope resultTy)
    [ handlerSurfaceType scope ctorInfo (lowerType scope resultTy)
      | ctorInfo <- dataConstructors dataInfo
    ]

shouldSpecializeCaseScrutinee :: P.Expr -> Bool
shouldSpecializeCaseScrutinee expr = case expr of
  EVar _ -> False
  EAnn inner _ -> shouldSpecializeCaseScrutinee inner
  _ -> True

isLocalOrdinaryCall :: ElaborateScope -> P.Expr -> Bool
isLocalOrdinaryCall scope expr =
  case collectApps expr of
    (EVar name, _ : _) ->
      case Map.lookup name (esValues scope) of
        Just OrdinaryValue {valueOriginModule = "<local>"} -> True
        _ -> False
    _ -> False

isConstructorCall :: ElaborateScope -> P.Expr -> Bool
isConstructorCall scope expr =
  case collectApps expr of
    (EVar name, _ : _) ->
      case Map.lookup name (esValues scope) of
        Just ConstructorValue {} -> True
        _ -> False
    _ -> False

dataHeadType :: DataInfo -> SrcType
dataHeadType info =
  case dataParams info of
    [] -> STBase (dataName info)
    p : ps -> STCon (dataName info) (STVar p :| map STVar ps)

isRecursiveResultType :: SrcType -> Bool
isRecursiveResultType ty =
  case ty of
    STMu {} -> True
    STForall _ _ body -> isRecursiveResultType body
    _ -> False

resolveInstanceInfo :: ElaborateScope -> P.ClassName -> SrcType -> Either ProgramError InstanceInfo
resolveInstanceInfo scope className0 headTy =
  case filter matches (esInstances scope) of
    [info] -> Right info
    [] -> Left (ProgramNoMatchingInstance className0 headTy)
    _ -> Left (ProgramNoMatchingInstance className0 headTy)
  where
    matches info =
      instanceClassName info == className0
        && lowerType scope (instanceHeadType info) == lowerType scope headTy

inferClassArgument :: SrcType -> String -> [SrcType] -> Maybe SrcType
inferClassArgument methodTy classParam args =
  let (_, bodyTy) = splitForalls methodTy
      (paramTys, _) = splitArrows bodyTy
   in Map.lookup classParam
        =<< foldM (\subst (templateTy, actualTy) -> matchTypes subst templateTy actualTy) Map.empty (zip paramTys args)

matchTypes :: Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypes subst template actual = case template of
  STVar name ->
    case Map.lookup name subst of
      Nothing -> Just (Map.insert name actual subst)
      Just existing
        | existing == actual -> Just subst
        | otherwise -> Nothing
  STArrow dom cod ->
    case actual of
      STArrow dom' cod' -> do
        subst' <- matchTypes subst dom dom'
        matchTypes subst' cod cod'
      _ -> Nothing
  STBase name ->
    case actual of
      STBase name' | name == name' -> Just subst
      _ -> Nothing
  STCon name args ->
    case actual of
      STCon name' args'
        | name == name' && length (toListNE args) == length (toListNE args') ->
            foldM (\acc (leftTy, rightTy) -> matchTypes acc leftTy rightTy) subst (zip (toListNE args) (toListNE args'))
      _ -> Nothing
  STForall name mb body ->
    case actual of
      STForall name' mb' body'
        | maybe True (\bound -> fmap unSrcBound mb' == Just (unSrcBound bound)) mb && name == name' ->
            matchTypes subst body body'
      _ -> Nothing
  STMu name body ->
    case actual of
      STMu name' body'
        | name == name' -> matchTypes subst body body'
      _ -> Nothing
  STBottom ->
    case actual of
      STBottom -> Just subst
      _ -> Nothing

-- | Strip leading STForall binders that do not appear in the body.
stripVacuousSrcForalls :: SrcType -> SrcType
stripVacuousSrcForalls (STForall v _ body)
  | v `Set.notMember` freeTypeVarsSrcType body = stripVacuousSrcForalls body
stripVacuousSrcForalls ty = ty

-- | Collect free type variables in a SrcType.
freeTypeVarsSrcType :: SrcType -> Set String
freeTypeVarsSrcType = go Set.empty
  where
    go bound (STVar name)
      | name `Set.member` bound = Set.empty
      | otherwise = Set.singleton name
    go _ (STBase _) = Set.empty
    go _ STBottom = Set.empty
    go bound (STArrow dom cod) = go bound dom `Set.union` go bound cod
    go bound (STCon _ args) = foldMap (go bound) args
    go bound (STForall name mb body) =
      let bound' = Set.insert name bound
          mbFvs = maybe Set.empty (go bound . unSrcBound) mb
       in mbFvs `Set.union` go bound' body
    go bound (STMu name body) = go (Set.insert name bound) body

extendLocal :: ElaborateScope -> String -> String -> Maybe SrcType -> ElaborateM ElaborateScope
extendLocal scope sourceName runtimeName mbTy = do
  case mbTy of
    Just sourceTy -> pure (extendLocalSourceTypePure scope sourceName runtimeName sourceTy)
    Nothing -> do
      loweredTy <- freshTypeName
      pure (extendLocalLoweredPure scope sourceName runtimeName loweredTy)

extendLocalLowered :: ElaborateScope -> String -> String -> SrcType -> ElaborateM ElaborateScope
extendLocalLowered scope sourceName runtimeName loweredTy =
  pure (extendLocalLoweredPure scope sourceName runtimeName loweredTy)

extendLocalLoweredPure :: ElaborateScope -> String -> String -> SrcType -> ElaborateScope
extendLocalLoweredPure scope sourceName runtimeName loweredTy =
  scope
    { esValues =
        Map.insert
          sourceName
          OrdinaryValue
            { valueDisplayName = sourceName,
              valueRuntimeName = runtimeName,
              valueType = loweredTy,
              valueOriginModule = "<local>"
            }
          (esValues scope),
      esRuntimeTypes = Map.insert runtimeName loweredTy (esRuntimeTypes scope)
    }

extendLocalSourceTypePure :: ElaborateScope -> String -> String -> SrcType -> ElaborateScope
extendLocalSourceTypePure scope sourceName runtimeName sourceTy =
  scope
    { esValues =
        Map.insert
          sourceName
          OrdinaryValue
            { valueDisplayName = sourceName,
              valueRuntimeName = runtimeName,
              valueType = sourceTy,
              valueOriginModule = "<local>"
            }
          (esValues scope),
      esRuntimeTypes = Map.insert runtimeName (lowerType scope sourceTy) (esRuntimeTypes scope)
    }

expectedCodomain :: Maybe SrcType -> Maybe SrcType
expectedCodomain = \case
  Just (STArrow _ cod) -> Just cod
  _ -> Nothing

mentionsFreeValue :: String -> P.Expr -> Bool
mentionsFreeValue name = elem name . collectFreeValues Set.empty

collectFreeValues :: Set String -> P.Expr -> [String]
collectFreeValues bound expr = case expr of
  EVar name
    | name `Set.member` bound -> []
    | otherwise -> [name]
  ELit _ -> []
  ELam param body -> collectFreeValues (Set.insert (P.paramName param) bound) body
  EApp fun arg -> collectFreeValues bound fun ++ collectFreeValues bound arg
  ELet name _ rhs body -> collectFreeValues bound rhs ++ collectFreeValues (Set.insert name bound) body
  EAnn inner _ -> collectFreeValues bound inner
  ECase scrutinee alts ->
    collectFreeValues bound scrutinee ++ concatMap collectAlt alts
  where
    collectAlt (P.Alt pattern0 body) =
      collectFreeValues (Set.union bound (Set.fromList (patternBinders pattern0))) body

    patternBinders = \case
      P.PatCtor _ binders -> binders
      P.PatVar name -> [name]
      P.PatWildcard -> []

collectApps :: P.Expr -> (P.Expr, [P.Expr])
collectApps = go []
  where
    go acc (EApp fun arg) = go (arg : acc) fun
    go acc headExpr = (headExpr, acc)

duplicates :: (Eq a) => [a] -> [a]
duplicates xs = [x | x <- nub xs, length (filter (== x) xs) > 1]

freshRuntimeName :: String -> ElaborateM String
freshRuntimeName base = do
  n <- freshCounter
  pure ("$" ++ base ++ "#" ++ show n)

freshTypeName :: ElaborateM SrcType
freshTypeName = do
  n <- freshCounter
  pure (STVar ("p$" ++ show n))

freshTypeVarName :: ElaborateM String
freshTypeVarName = do
  n <- freshCounter
  pure ("r$" ++ show n)

freshDeferredMethodName :: String -> ElaborateM String
freshDeferredMethodName methodName0 = do
  n <- freshCounter
  pure ("$deferred_" ++ methodName0 ++ "_" ++ show n)

freshCounter :: ElaborateM Int
freshCounter = do
  state <- get
  let n = elaborateFreshCounter state
  modify (\state' -> state' {elaborateFreshCounter = n + 1})
  pure n
