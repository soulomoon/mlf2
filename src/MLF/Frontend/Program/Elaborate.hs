{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeInstances,
    elaborateScopeRuntimeTypes,
    mkElaborateScope,
    lowerConstructorBinding,
    lowerConstrainedExprBinding,
    lowerExprBinding,
    inferClassArgument,
    lowerType,
    matchTypes,
    freeTypeVarsSrcType,
    resolveInstanceInfo,
    resolveInstanceInfoWithSubst,
  )
where

import Control.Monad (foldM, replicateM, when, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    esClasses :: Map String ClassInfo,
    esEvidence :: [EvidenceInfo],
    esInstances :: [InstanceInfo]
  }

data ElaborateState = ElaborateState
  { elaborateFreshCounter :: Int,
    elaborateDeferredObligations :: Map String DeferredProgramObligation,
    elaborateExternalTypes :: Map String SrcType
  }

type ElaborateM a = ExceptT ProgramError (State ElaborateState) a

data ElaborateResult a = ElaborateResult
  { elaborateResultValue :: a,
    elaborateResultDeferredObligations :: Map String DeferredProgramObligation,
    elaborateResultExternalTypes :: Map String SrcType
  }

runElaborateM :: ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateM action =
  let initialState =
        ElaborateState
          { elaborateFreshCounter = 0,
            elaborateDeferredObligations = Map.empty,
            elaborateExternalTypes = Map.empty
          }
      (result, finalState) = runState (runExceptT action) initialState
   in case result of
        Left err -> Left err
        Right value ->
          Right
            ElaborateResult
              { elaborateResultValue = value,
                elaborateResultDeferredObligations = elaborateDeferredObligations finalState,
                elaborateResultExternalTypes = elaborateExternalTypes finalState
              }

mkElaborateScope :: Map String ValueInfo -> Map String DataInfo -> Map String ClassInfo -> [InstanceInfo] -> ElaborateScope
mkElaborateScope values0 dataTypes classes0 instances0 =
  ElaborateScope
    { esValues = values0 `Map.union` instanceRuntimeValues,
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
      esClasses = classes0,
      esEvidence = [],
      esInstances = instances0
    }
  where
    shouldTrackRuntimeType OverloadedMethod {} = False
    shouldTrackRuntimeType _ = True

    runtimeNameFor OrdinaryValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor ConstructorValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor OverloadedMethod {} = error "overloaded methods do not have runtime names"

    valueTypeFor OrdinaryValue {valueType = ty, valueConstraints = constraints} =
      constrainedRuntimeTypeRaw dataTypes classes0 constraints ty
    valueTypeFor ConstructorValue {valueType = ty} = quantifyFreeTypeVars ty
    valueTypeFor OverloadedMethod {} = error "overloaded methods do not have concrete runtime types"

    instanceRuntimeValues =
      Map.fromList
        [ (runtimeName, methodValue)
          | instanceInfo <- instances0,
            methodValue@OrdinaryValue {valueRuntimeName = runtimeName} <- Map.elems (instanceMethods instanceInfo)
        ]

elaborateScopeRuntimeTypes :: ElaborateScope -> Map String SrcType
elaborateScopeRuntimeTypes = esRuntimeTypes

elaborateScopeDataTypes :: ElaborateScope -> Map String DataInfo
elaborateScopeDataTypes = esTypes

elaborateScopeInstances :: ElaborateScope -> [InstanceInfo]
elaborateScopeInstances = esInstances

lowerType :: ElaborateScope -> SrcType -> SrcType
lowerType scope = lowerTypeRaw (esTypes scope)

constrainedRuntimeType :: ElaborateScope -> [P.ClassConstraint] -> SrcType -> SrcType
constrainedRuntimeType scope =
  constrainedRuntimeTypeRaw (esTypes scope) (esClasses scope)

constrainedRuntimeTypeRaw :: Map String DataInfo -> Map String ClassInfo -> [P.ClassConstraint] -> SrcType -> SrcType
constrainedRuntimeTypeRaw dataTypes classes0 constraints visibleTy =
  let (foralls, bodyTy) = splitForalls visibleTy
      evidenceTys = concatMap constraintEvidenceTypes constraints
      runtimeBody = foldr STArrow bodyTy evidenceTys
   in foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) runtimeBody foralls
  where
    constraintEvidenceTypes constraint =
      case Map.lookup (P.constraintClassName constraint) classes0 of
        Nothing -> []
        Just classInfo ->
          [ lowerTypeRaw dataTypes (specializeMethodType (methodType methodInfo) (classParamName classInfo) (P.constraintType constraint))
            | methodInfo <- Map.elems (classMethods classInfo)
          ]

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
      loweredBindingExpectedType = lowerType scope (quantifyFreeTypeVars (ctorType ctorInfo)),
      loweredBindingSurfaceExpr = constructorSurfaceExpr scope ctorInfo,
      loweredBindingDeferredObligations = Map.empty,
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
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypes = elaborateResultExternalTypes result,
        loweredBindingExportedAsMain = exportedAsMain
      }

lowerConstrainedExprBinding :: ElaborateScope -> String -> [P.ClassConstraint] -> SrcType -> Bool -> P.Expr -> Either ProgramError LoweredBinding
lowerConstrainedExprBinding scope runtimeName constraints visibleTy exportedAsMain expr = do
  result <- runElaborateM $ do
    (scopeWithEvidence, evidenceParams) <- extendConstraintEvidence scope constraints
    let (_, bodyExpectedTy) = splitForalls visibleTy
    bodyExpr <- compileExpr scopeWithEvidence (Just bodyExpectedTy) expr
    pure (foldr wrapEvidence bodyExpr evidenceParams)
  let expectedTy = constrainedRuntimeType scope constraints visibleTy
  pure
    LoweredBinding
      { loweredBindingName = runtimeName,
        loweredBindingExpectedType = lowerType scope expectedTy,
        loweredBindingSurfaceExpr = elaborateResultValue result,
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypes = elaborateResultExternalTypes result,
        loweredBindingExportedAsMain = exportedAsMain
      }
  where
    wrapEvidence (runtimeName0, evidenceTy) acc =
      surfaceLamAnn runtimeName0 evidenceTy acc

constructorSurfaceExpr :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExpr scope ctorInfo =
  surfaceAnn (constructorSurfaceExprRaw scope ctorInfo) (lowerType scope (quantifyFreeTypeVars (ctorType ctorInfo)))

constructorSurfaceExprRaw :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExprRaw scope ctorInfo =
  let argNames = ["$" ++ ctorName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
      handlerNames = ["$" ++ ctorName ctorInfo ++ "_k" ++ show ix | ix <- [1 .. length ctorOrder]]
      resultVar =
        if any (not . null . ctorForalls) ctorOrder || constructorOwnerHasParams
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
    constructorOwnerHasParams =
      maybe False (not . null . dataParams) (Map.lookup (ctorOwningType ctorInfo) (esTypes scope))

compileExpr :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpr scope mbExpected expr = case expr of
  EVar name ->
    case Map.lookup name (esValues scope) of
      Just OverloadedMethod {} -> throwError (ProgramAmbiguousMethodUse name)
      Just OrdinaryValue {valueRuntimeName = runtimeName} -> pure (surfaceVar runtimeName)
      Just ConstructorValue {valueCtorInfo = ctorInfo} -> do
        placeholder <- deferConstructorCall scope ctorInfo 0 mbExpected
        pure (surfaceVar placeholder)
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
    if name `notElem` collectFreeValues Set.empty body && mbTy == Nothing
      then compileExpr scope mbExpected body
      else do
        let recursive = mentionsFreeValue name rhs
        case (recursive, mbTy, inlineImmediateLetUse name rhs body) of
          (False, Nothing, Just inlined) ->
            compileExpr scope mbExpected inlined
          _ -> do
            runtimeName <- freshRuntimeName name
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
                    Nothing ->
                      case inferKnownExprType selfScope rhs of
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
    (headExpr, [arg])
      | Just expectedTy <- mbExpected -> do
          (expectedHeadTy, argSurface) <-
            case inferKnownExprType scope arg of
              Just argTy -> do
                argSurface <- compileExpr scope (Just argTy) arg
                pure (STArrow argTy expectedTy, argSurface)
              Nothing -> do
                argTy <- freshTypeName
                argSurface <- compileExpr scope Nothing arg
                pure (STArrow argTy expectedTy, argSurface)
          headSurface <- compileExpr scope (Just expectedHeadTy) headExpr
          pure (surfaceApp headSurface argSurface)
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
compileValueApp scope mbExpected ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let expectedArgTys = constructorExpectedArgTypes scope ctorInfo args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgTys args
  placeholder <- deferConstructorCall scope ctorInfo (length args) mbExpected
  pure (foldl surfaceApp (surfaceVar placeholder) argSurfaces)
  where
    compileConstructorArg expectedTy arg = do
      case inferKnownExprType scope arg of
        Just knownTy
          | Set.null (freeTypeVarsSrcType knownTy) ->
              compileExpr scope (Just knownTy) arg
          | otherwise ->
              compileExpr scope (Just expectedTy) arg
        Nothing -> do
          argSurface <- compileExpr scope Nothing arg
          pure $
            if hasLeadingForall expectedTy
              then surfaceAnn argSurface (lowerType scope expectedTy)
              else argSurface

compileValueApp scope mbExpected valueInfo args = do
  argSurfaces <- mapM (compileExpr scope Nothing) args
  evidenceSurfaces <- valueEvidenceArgs scope valueInfo args
  let headSurface =
        case valueInfo of
          OrdinaryValue {valueRuntimeName = runtimeName} -> surfaceVar runtimeName
          OverloadedMethod {} -> error "compileValueApp does not handle overloaded methods"
      headWithEvidence = foldl surfaceApp headSurface evidenceSurfaces
      applied = foldl surfaceApp headWithEvidence argSurfaces
  pure $ case mbExpected of
    Just expectedTy
      | not (isLocalOrdinaryValue valueInfo),
        isRecursiveResultType expectedTy || isRecursiveResultType (lowerType scope expectedTy) ->
          surfaceAnn applied (lowerType scope expectedTy)
    _ -> applied

constructorExpectedArgTypes :: ElaborateScope -> ConstructorInfo -> [P.Expr] -> [SrcType]
constructorExpectedArgTypes scope ctorInfo args =
  reverse (snd (foldl step (Map.empty, []) (zip (ctorArgs ctorInfo) args)))
  where
    step (subst, acc) (templateTy, arg) =
      let expectedTy = specializeSrcType subst templateTy
          subst' =
            case inferKnownExprType scope arg >>= matchTypes subst templateTy of
              Just matched -> matched
              Nothing -> subst
       in (subst', expectedTy : acc)

valueEvidenceArgs :: ElaborateScope -> ValueInfo -> [P.Expr] -> ElaborateM [SurfaceExpr]
valueEvidenceArgs scope OrdinaryValue {valueType = visibleTy, valueConstraints = constraints} args
  | null constraints = pure []
  | otherwise = do
      subst <-
        case inferCallSubst scope visibleTy args of
          Just subst0 -> pure subst0
          Nothing ->
            case constraints of
              constraint : _ -> throwError (ProgramNoMatchingInstance (P.constraintClassName constraint) (P.constraintType constraint))
              [] -> pure Map.empty
      concat <$> mapM (constraintEvidenceArgExprs scope . applyConstraintSubst subst) constraints
valueEvidenceArgs _ _ _ = pure []

constraintEvidenceArgExprs :: ElaborateScope -> P.ClassConstraint -> ElaborateM [SurfaceExpr]
constraintEvidenceArgExprs scope constraint
  | shouldDeferConstraintEvidence scope constraint =
      deferConstraintEvidenceExprs scope constraint
  | otherwise =
      resolveConstraintEvidenceExpr scope Set.empty constraint

shouldDeferConstraintEvidence :: ElaborateScope -> P.ClassConstraint -> Bool
shouldDeferConstraintEvidence scope constraint =
  not (Set.null (freeTypeVarsSrcType (P.constraintType constraint)))
    && not (constraintCoveredByEvidence scope constraint)

constraintCoveredByEvidence :: ElaborateScope -> P.ClassConstraint -> Bool
constraintCoveredByEvidence scope constraint =
  case Map.lookup (P.constraintClassName constraint) (esClasses scope) of
    Nothing -> False
    Just classInfo ->
      all
        ( \methodInfo ->
            case lookupEvidenceMethod scope (P.constraintClassName constraint) (P.constraintType constraint) (methodName methodInfo) of
              Just _ -> True
              Nothing -> False
        )
        (Map.elems (classMethods classInfo))

deferConstraintEvidenceExprs :: ElaborateScope -> P.ClassConstraint -> ElaborateM [SurfaceExpr]
deferConstraintEvidenceExprs scope constraint =
  case Map.lookup (P.constraintClassName constraint) (esClasses scope) of
    Nothing -> throwError (ProgramUnknownClass (P.constraintClassName constraint))
    Just classInfo ->
      mapM (deferMethodEvidenceExpr scope (P.constraintType constraint)) (Map.elems (classMethods classInfo))

deferMethodEvidenceExpr :: ElaborateScope -> SrcType -> MethodInfo -> ElaborateM SurfaceExpr
deferMethodEvidenceExpr scope classArgTy methodInfo = do
  let methodTy =
        stripVacuousSrcForalls $
          specializeMethodType (methodType methodInfo) (methodParamName methodInfo) classArgTy
      fullArity = methodFullArity methodInfo
  placeholder <- deferMethodCall scope methodInfo fullArity methodTy
  expanded <- etaExpandMissingArgs scope methodInfo methodTy 0 fullArity (surfaceVar placeholder)
  pure (surfaceAnn expanded (lowerType scope methodTy))

inferCallSubst :: ElaborateScope -> SrcType -> [P.Expr] -> Maybe (Map String SrcType)
inferCallSubst scope visibleTy args = do
  let (_, bodyTy) = splitForalls visibleTy
      (argTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip argTys args,
            Just actualTy <- [inferKnownExprType scope arg]
        ]
  foldM (\acc (templateTy, actualTy) -> matchTypes acc templateTy actualTy) Map.empty knownPairs

inlineImmediateLetUse :: String -> P.Expr -> P.Expr -> Maybe P.Expr
inlineImmediateLetUse bindingName rhs body =
  let (headExpr, args) = collectApps body
   in case (headExpr, args) of
        (EVar name, _ : _)
          | name == bindingName,
            rhsConsumesAppliedArgs rhs (length args) ->
              Just (foldl EApp rhs args)
        _ -> Nothing

rhsConsumesAppliedArgs :: P.Expr -> Int -> Bool
rhsConsumesAppliedArgs _ 0 = True
rhsConsumesAppliedArgs expr argCount =
  case expr of
    ELam param body ->
      mentionsFreeValue (P.paramName param) body
        && rhsConsumesAppliedArgs body (argCount - 1)
    _ -> True

isLocalOrdinaryValue :: ValueInfo -> Bool
isLocalOrdinaryValue OrdinaryValue {valueOriginModule = "<local>"} = True
isLocalOrdinaryValue _ = False

knownConstructorResultType :: ElaborateScope -> ConstructorInfo -> [P.Expr] -> Maybe SrcType
knownConstructorResultType scope ctorInfo args = do
  argTypes <- traverse (inferKnownExprType scope) args
  subst <- foldM (\acc (templateTy, actualTy) -> matchTypes acc templateTy actualTy) Map.empty (zip (ctorArgs ctorInfo) argTypes)
  pure (Map.foldrWithKey substituteTypeVar (ctorResult ctorInfo) subst)

compileMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileMethodApp scope _mbExpected methodInfo args
  | null args = throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
  | otherwise = do
      let fullArity = methodFullArity methodInfo
          suppliedArity = length args
          knownClassArg = knownMethodClassArg scope methodInfo args
          placeholderTy = placeholderMethodType scope methodInfo args
          knownArgTys =
            case knownClassArg of
              Just _ -> Just (take suppliedArity (methodArgumentTypes placeholderTy))
              Nothing -> Nothing
      argSurfaces <-
        case knownArgTys of
          Just argTys -> zipWithM (compileExpectedMethodArg scope) argTys args
          Nothing -> mapM (compileExpr scope Nothing) args
      case knownClassArg of
        Just classArgTy
          | shouldResolveMethodBeforeInference scope methodInfo classArgTy -> do
              methodHead <- resolveMethodHeadExpr scope Set.empty methodInfo classArgTy
              let applied = foldl surfaceApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy suppliedArity fullArity applied
              pure $
                case peelAppliedType placeholderTy suppliedArity of
                  Just remainingTy
                    | suppliedArity < fullArity -> surfaceAnn expanded (lowerType scope remainingTy)
                  _ -> expanded
        _ -> do
          placeholder <- deferMethodCall scope methodInfo fullArity placeholderTy
          let applied = foldl surfaceApp (surfaceVar placeholder) argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy suppliedArity fullArity applied
          pure $
            case peelAppliedType placeholderTy suppliedArity of
              Just remainingTy
                | suppliedArity < fullArity -> surfaceAnn expanded (lowerType scope remainingTy)
              _ -> expanded

compileExpectedMethodArg :: ElaborateScope -> SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpectedMethodArg scope expectedTy expr =
  case expr of
    EAnn {} ->
      compileExpr scope (Just expectedTy) expr
    EApp (ELam param (EVar bodyName)) actual
      | bodyName == P.paramName param ->
          compileExpr scope (Just expectedTy) actual
    EApp (ELam param body) actual -> do
      runtimeName <- freshRuntimeName (P.paramName param)
      actualExpr <- compileExpr scope (Just expectedTy) actual
      scope' <- extendLocal scope (P.paramName param) runtimeName (Just expectedTy)
      bodyExpr <- compileExpr scope' (Just expectedTy) body
      pure (surfaceLet runtimeName actualExpr (surfaceAnn bodyExpr (lowerType scope expectedTy)))
    EVar name
      | Just ConstructorValue {} <- Map.lookup name (esValues scope) ->
          compileExpr scope (Just expectedTy) expr
    EVar {} ->
      compileExpr scope Nothing expr
    _
      | (EVar name, _) <- collectApps expr,
        Just ConstructorValue {} <- Map.lookup name (esValues scope) ->
          compileExpr scope (Just expectedTy) expr
    _ -> do
      argExpr <- compileExpr scope Nothing expr
      pure (surfaceAnn argExpr (lowerType scope expectedTy))

resolveMethodHeadExpr :: ElaborateScope -> Set (P.ClassName, String) -> MethodInfo -> SrcType -> ElaborateM SurfaceExpr
resolveMethodHeadExpr scope seen methodInfo classArgTy =
  case lookupEvidenceMethod scope (methodClassName methodInfo) classArgTy (methodName methodInfo) of
    Just (runtimeName, _) -> pure (surfaceVar runtimeName)
    Nothing -> do
      (instanceInfo, subst) <- liftEitherElab (resolveInstanceInfoWithSubst scope (methodClassName methodInfo) classArgTy)
      case Map.lookup (methodName methodInfo) (instanceMethods instanceInfo) of
        Just OrdinaryValue {valueRuntimeName = runtimeName, valueConstraints = constraints} -> do
          evidenceArgs <-
            concat
              <$> mapM
                (resolveConstraintEvidenceExpr scope seen . applyConstraintSubst subst)
                constraints
          pure (foldl surfaceApp (surfaceVar runtimeName) evidenceArgs)
        _ -> throwError (ProgramUnknownMethod (methodName methodInfo))

shouldResolveMethodBeforeInference :: ElaborateScope -> MethodInfo -> SrcType -> Bool
shouldResolveMethodBeforeInference scope methodInfo classArgTy =
  case lookupEvidenceMethod scope (methodClassName methodInfo) classArgTy (methodName methodInfo) of
    Just _ -> True
    Nothing -> False

resolveConstraintEvidenceExpr :: ElaborateScope -> Set (P.ClassName, String) -> P.ClassConstraint -> ElaborateM [SurfaceExpr]
resolveConstraintEvidenceExpr scope seen constraint =
  case Map.lookup (P.constraintClassName constraint) (esClasses scope) of
    Nothing -> throwError (ProgramUnknownClass (P.constraintClassName constraint))
    Just classInfo -> do
      let key = (P.constraintClassName constraint, show (P.constraintType constraint))
      whenSeen key
      mapM
        ( \methodInfo ->
            resolveMethodHeadExpr
              scope
              (Set.insert key seen)
              methodInfo
              (P.constraintType constraint)
        )
        (Map.elems (classMethods classInfo))
  where
    whenSeen key =
      when (key `Set.member` seen) $
        throwError (ProgramNoMatchingInstance (P.constraintClassName constraint) (P.constraintType constraint))

lookupEvidenceMethod :: ElaborateScope -> P.ClassName -> SrcType -> P.MethodName -> Maybe (String, SrcType)
lookupEvidenceMethod scope className0 headTy methodName0 =
  case
    [ methodEvidence
      | evidence <- esEvidence scope,
        evidenceClassName evidence == className0,
        lowerType scope (evidenceType evidence) == lowerType scope headTy,
        Just methodEvidence <- [Map.lookup methodName0 (evidenceMethods evidence)]
    ]
  of
    methodEvidence : _ -> Just methodEvidence
    [] -> Nothing

applyConstraintSubst :: Map String SrcType -> P.ClassConstraint -> P.ClassConstraint
applyConstraintSubst subst constraint =
  constraint {P.constraintType = applySrcSubst subst (P.constraintType constraint)}

applySrcSubst :: Map String SrcType -> SrcType -> SrcType
applySrcSubst subst ty =
  Map.foldrWithKey substituteTypeVar ty subst

liftEitherElab :: Either ProgramError a -> ElaborateM a
liftEitherElab = either throwError pure

etaExpandMissingArgs :: ElaborateScope -> MethodInfo -> SrcType -> Int -> Int -> SurfaceExpr -> ElaborateM SurfaceExpr
etaExpandMissingArgs scope methodInfo methodTy suppliedArity fullArity applied = do
  let missingArity = max 0 (fullArity - suppliedArity)
  if missingArity == 0
    then pure applied
    else do
      missingNames <- replicateM missingArity (freshRuntimeName (methodName methodInfo ++ "_arg"))
      let missingTypes = drop suppliedArity (methodArgumentTypes methodTy)
          body = foldl surfaceApp applied (map surfaceVar missingNames)
      pure (foldr wrapMissingArg body (zip missingNames missingTypes))
  where
    wrapMissingArg (name, ty) body
      | Set.null (freeTypeVarsSrcType ty) = surfaceLamAnn name (lowerType scope ty) body
      | otherwise = surfaceLam name body

methodFullArity :: MethodInfo -> Int
methodFullArity methodInfo =
  length (methodArgumentTypes (methodType methodInfo))

methodArgumentTypes :: SrcType -> [SrcType]
methodArgumentTypes ty =
  let (_, bodyTy) = splitForalls ty
      (argTys, _) = splitArrows bodyTy
   in argTys

deferMethodCall :: ElaborateScope -> MethodInfo -> Int -> SrcType -> ElaborateM String
deferMethodCall scope methodInfo fullArity placeholderSourceTy = do
  placeholder <- freshDeferredMethodName (methodName methodInfo)
  let placeholderTy = lowerType scope placeholderSourceTy
      deferred =
        DeferredMethodCall
          { deferredMethodPlaceholder = placeholder,
            deferredMethodInfo = methodInfo,
            deferredMethodArgCount = fullArity,
            deferredMethodFullArity = fullArity,
            deferredMethodName = methodName methodInfo
          }
  registerDeferredObligation placeholder placeholderTy (DeferredMethod deferred)
  pure placeholder

deferConstructorCall :: ElaborateScope -> ConstructorInfo -> Int -> Maybe SrcType -> ElaborateM String
deferConstructorCall scope ctorInfo argCount mbExpected = do
  placeholder <- freshDeferredConstructorName (ctorName ctorInfo)
  let quantifiedTy = quantifyFreeTypeVars (ctorType ctorInfo)
      occurrenceTy = constructorOccurrenceType ctorInfo argCount
      instBinders = map fst (fst (splitForalls quantifiedTy))
      initialSubst =
        case mbExpected >>= matchTypes Map.empty occurrenceTy of
          Just subst -> subst
          Nothing -> Map.empty
      placeholderSourceTy = specializeQuantifiedType initialSubst quantifiedTy
      placeholderTy = lowerType scope placeholderSourceTy
      bindingMode = DeferredBindingMonomorphic
      deferred =
        DeferredConstructorCall
          { deferredConstructorPlaceholder = placeholder,
            deferredConstructorInfo = ctorInfo,
            deferredConstructorArgCount = argCount,
            deferredConstructorSourceType = placeholderSourceTy,
            deferredConstructorOccurrenceType = specializeSrcType initialSubst occurrenceTy,
            deferredConstructorInstBinders = instBinders,
            deferredConstructorInitialSubst = initialSubst,
            deferredConstructorBindingMode = bindingMode
          }
  registerDeferredObligation placeholder placeholderTy (DeferredConstructor deferred)
  pure placeholder

constructorOccurrenceType :: ConstructorInfo -> Int -> SrcType
constructorOccurrenceType ctorInfo argCount =
  foldr STArrow (ctorResult ctorInfo) (drop argCount (ctorArgs ctorInfo))

specializeQuantifiedType :: Map String SrcType -> SrcType -> SrcType
specializeQuantifiedType subst ty =
  let (foralls, body) = splitForalls ty
      kept =
        [ (name, fmap (specializeSrcType subst) mb)
          | (name, mb) <- foralls,
            Map.notMember name subst
        ]
   in foldr
        (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc)
        (specializeSrcType subst body)
        kept

specializeSrcType :: Map String SrcType -> SrcType -> SrcType
specializeSrcType subst ty = case ty of
  STVar name -> Map.findWithDefault ty name subst
  STArrow dom cod -> STArrow (specializeSrcType subst dom) (specializeSrcType subst cod)
  STBase {} -> ty
  STCon name args -> STCon name (fmap (specializeSrcType subst) args)
  STForall name mb body
    | Map.member name subst -> STForall name mb body
    | otherwise ->
        STForall name (fmap (SrcBound . specializeSrcType subst . unSrcBound) mb) (specializeSrcType subst body)
  STMu name body
    | Map.member name subst -> STMu name body
    | otherwise -> STMu name (specializeSrcType subst body)
  STBottom -> STBottom

deferCaseCall :: ElaborateScope -> DataInfo -> SrcType -> ElaborateM String
deferCaseCall scope dataInfo resultTy = do
  placeholder <- freshDeferredCaseName (dataName dataInfo)
  let headTy = dataHeadType dataInfo
      resultTyElab = lowerType scope resultTy
      handlerTys = replicate (length (dataConstructors dataInfo)) STBottom
      placeholderTy = foldr STArrow resultTyElab (lowerType scope headTy : handlerTys)
      deferred =
        DeferredCaseCall
          { deferredCasePlaceholder = placeholder,
            deferredCaseDataInfo = dataInfo,
            deferredCaseResultType = resultTy,
            deferredCaseHandlerNames = [],
            deferredCaseExpectedArgCount = 1 + length handlerTys
          }
  registerDeferredObligation placeholder placeholderTy (DeferredCase deferred)
  pure placeholder

registerDeferredObligation :: String -> SrcType -> DeferredProgramObligation -> ElaborateM ()
registerDeferredObligation placeholder placeholderTy obligation =
  modify
    ( \state ->
        state
          { elaborateDeferredObligations = Map.insert placeholder obligation (elaborateDeferredObligations state),
            elaborateExternalTypes = Map.insert placeholder placeholderTy (elaborateExternalTypes state)
          }
    )

placeholderMethodType :: ElaborateScope -> MethodInfo -> [P.Expr] -> SrcType
placeholderMethodType scope methodInfo args =
  let paramName = methodParamName methodInfo
      methodTy = methodType methodInfo
      quantifiedMethodTy = quantifiedMethodType methodInfo
      knownClassArg = knownMethodClassArg scope methodInfo args
   in case knownClassArg of
        Just classArgTy -> stripVacuousSrcForalls (specializeMethodType methodTy paramName classArgTy)
        Nothing -> quantifiedMethodTy

knownMethodClassArg :: ElaborateScope -> MethodInfo -> [P.Expr] -> Maybe SrcType
knownMethodClassArg scope methodInfo args = do
  let (_, bodyTy) = splitForalls (methodType methodInfo)
      (paramTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip paramTys args,
            Just actualTy <- [inferKnownExprType scope arg]
        ]
  subst <- foldM (\acc (templateTy, actualTy) -> matchTypes acc templateTy actualTy) Map.empty knownPairs
  Map.lookup (methodParamName methodInfo) subst

quantifiedMethodType :: MethodInfo -> SrcType
quantifiedMethodType methodInfo =
  let paramName = methodParamName methodInfo
      methodTy = methodType methodInfo
      (foralls, _) = splitForalls methodTy
   in if any ((== paramName) . fst) foralls || paramName `Set.notMember` freeTypeVarsSrcType methodTy
        then methodTy
        else STForall paramName Nothing methodTy

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
              case valueInfo of
                OrdinaryValue {valueType = ty}
                  | not (null args), hasLeadingForall ty -> Nothing
                ConstructorValue {valueCtorInfo = ctorInfo}
                  | length args == length (ctorArgs ctorInfo) ->
                      knownConstructorResultType scope ctorInfo args
                _ -> appliedValueResultType valueInfo (length args)
        _ -> Nothing
    _ -> Nothing

hasLeadingForall :: SrcType -> Bool
hasLeadingForall ty =
  case ty of
    STForall {} -> True
    _ -> False

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
      let mbInferredScrutineeTy = inferKnownExprType scope scrutinee
          mbAnnotationScrutineeTy = catchAllPatternAnnotationType alts
          mbScrutineeTy =
            case mbInferredScrutineeTy of
              Just knownTy -> Just knownTy
              Nothing -> mbAnnotationScrutineeTy
          annotateScrutinee =
            case (mbInferredScrutineeTy, mbAnnotationScrutineeTy) of
              (Nothing, Just annTy) -> Just annTy
              _ -> Nothing
      mapM_ (\scrutineeTy -> mapM_ (validatePatternType scope scrutineeTy . P.altPattern) alts) mbScrutineeTy
      scrutineeExpr0 <- compileExpr scope mbScrutineeTy scrutinee
      let scrutineeExpr =
            case annotateScrutinee of
              Just annTy -> surfaceAnn scrutineeExpr0 (lowerType scope annTy)
              Nothing -> scrutineeExpr0
      compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts
    owners -> do
      dataInfo <- requireSingleDataOwner scope owners
      let headTy = dataHeadType dataInfo
          scrutineeTy =
            case inferKnownExprType scope scrutinee of
              Just knownTy -> knownTy
              Nothing -> headTy
      validateOrderedPatterns alts
      mapM_ (validatePatternType scope scrutineeTy . P.altPattern) alts
      (resultTy, _quantifyResult) <-
        case mbExpected of
          Just expectedTy -> pure (expectedTy, False)
          Nothing -> do
            resultVar <- freshTypeVarName
            pure (STVar resultVar, True)
      case localIdentityScrutinee scope scrutinee of
        Just inner -> compileCase scope mbExpected inner alts
        Nothing -> do
          scrutineeExpr <- compileExpr scope (Just scrutineeTy) scrutinee
          let forceAnnotateHandlers = any (not . null . ctorForalls) (dataConstructors dataInfo)
          handlers <- mapM (compileHandler scope scrutineeExpr resultTy dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholder <- deferCaseCall scope dataInfo resultTy
          pure (foldl surfaceApp (surfaceVar placeholder) (scrutineeExpr : handlers))

localIdentityScrutinee :: ElaborateScope -> P.Expr -> Maybe P.Expr
localIdentityScrutinee scope expr =
  case collectApps expr of
    (ELam param (EVar bodyName), [arg])
      | bodyName == P.paramName param ->
          Just arg
    (EVar name, [arg])
      | name == "id",
        Just OrdinaryValue {valueOriginModule = "<local>"} <- Map.lookup name (esValues scope) ->
          Just arg
    _ -> Nothing

compileCatchAllOnly :: ElaborateScope -> Maybe SrcType -> Maybe SrcType -> SurfaceExpr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileExpr scope mbExpected body
      case mbScrutineeTy of
        Just _ -> do
          scrutineeName <- freshRuntimeName "case_scrutinee"
          pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        Nothing -> pure bodyExpr
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName name
      scope' <-
        case mbScrutineeTy of
          Just scrutineeTy -> extendLocal scope name runtimeName (Just scrutineeTy)
          Nothing -> extendLocalLowered scope name runtimeName =<< freshTypeName
      bodyExpr <- compileExpr scope' mbExpected body
      pure (surfaceLet runtimeName scrutineeExpr bodyExpr)
    [P.Alt (P.PatAnn inner _) body] -> compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr [P.Alt inner body]
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileHandler :: ElaborateScope -> SurfaceExpr -> SrcType -> DataInfo -> [P.Alt] -> Bool -> ConstructorInfo -> ElaborateM SurfaceExpr
compileHandler scope scrutineeExpr resultTy dataInfo alts forceAnnotateHandlers ctorInfo = do
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
  let topArgs = zip3 (map (const P.PatWildcard) (ctorArgs ctorInfo)) runtimeNames (ctorArgs ctorInfo)
      candidates = matchingCandidates ctorInfo
  bodyExpr <- compileCandidates topArgs candidates
  let handlerBody =
        foldr
          (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
          bodyExpr
          (zip runtimeNames (ctorArgs ctorInfo))
  if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
    then pure handlerBody
    else do
      let handlerTy = handlerSurfaceType scope ctorInfo (lowerType scope resultTy)
      pure (surfaceAnn handlerBody handlerTy)
  where
    matchingCandidates ctor =
      filter (patternCouldMatchConstructor ctor . P.altPattern) alts

    compileCandidates _ [] = throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
    compileCandidates topArgs [alt] =
      compileAltCandidate topArgs alt Nothing
    compileCandidates topArgs (alt : rest) = do
      fallback <- compileCandidates topArgs rest
      compileAltCandidate topArgs alt (Just fallback)

    compileAltCandidate topArgs (P.Alt pattern0 body) mbFallback =
      case stripPatternAnn pattern0 of
        P.PatWildcard -> compileExpr scope (Just resultTy) body
        P.PatVar name -> do
          scrutineeName <- freshRuntimeName name
          scope' <- extendLocalLowered scope name scrutineeName (lowerType scope (dataHeadType dataInfo))
          bodyExpr <- compileExpr scope' (Just resultTy) body
          pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        P.PatCtor ctorName0 patterns
          | ctorName0 == ctorName ctorInfo,
            length patterns == length (ctorArgs ctorInfo) ->
              compilePatternSequence scope (zip3 patterns (map middle topArgs) (ctorArgs ctorInfo)) body mbFallback
          | ctorName0 == ctorName ctorInfo ->
              throwError (ProgramPatternConstructorMismatch ctorName0 (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    middle (_, value, _) = value

    compilePatternSequence scope0 [] body _ =
      compileExpr scope0 (Just resultTy) body
    compilePatternSequence scope0 ((pattern0, runtimeName, argTy) : rest) body mbFallback =
      case stripPatternAnn pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          scope' <- extendLocal scope0 sourceName runtimeName (Just argTy)
          compilePatternSequence scope' rest body mbFallback
        P.PatCtor nestedCtorName nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfo nestedCtorName
          nestedDataInfo <- lookupDataInfo (ctorOwningType nestedCtorInfo)
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch nestedCtorName argTy)
            else do
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure fallback0
                  Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              let forceNestedAnnotations = any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
              matchingBody <- compilePatternSequence scope0 (zip3 nestedPatterns nestedRuntimeNames (ctorArgs nestedCtorInfo) ++ rest) body mbFallback
              handlers <- mapM (nestedHandler forceNestedAnnotations nestedCtorInfo nestedRuntimeNames matchingBody fallback) (dataConstructors nestedDataInfo)
              placeholder <- deferCaseCall scope0 nestedDataInfo resultTy
              pure (foldl surfaceApp (surfaceVar placeholder) (surfaceVar runtimeName : handlers))
        P.PatAnn inner _ -> compilePatternSequence scope0 ((inner, runtimeName, argTy) : rest) body mbFallback

    nestedHandler forceNestedAnnotations targetCtor nestedRuntimeNames matchingBody fallback ctor =
      let argNames = if ctorName ctor == ctorName targetCtor then nestedRuntimeNames else ["unused" ++ show ix | ix <- [1 .. length (ctorArgs ctor)]]
          selectedBody = if ctorName ctor == ctorName targetCtor then matchingBody else fallback
          handlerBody =
            foldr
              (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
              selectedBody
              (zip argNames (ctorArgs ctor))
       in if not forceNestedAnnotations && null (ctorForalls ctor)
            then pure handlerBody
            else do
              let handlerTy = handlerSurfaceType scope ctor (lowerType scope resultTy)
              pure (surfaceAnn handlerBody handlerTy)

    lookupConstructorInfo ctorName0 =
      case Map.lookup ctorName0 (esValues scope) of
        Just ConstructorValue {valueCtorInfo = ctorInfo0} -> pure ctorInfo0
        _ -> throwError (ProgramUnknownConstructor ctorName0)

    lookupDataInfo typeName =
      case Map.lookup typeName (esTypes scope) of
        Just info -> pure info
        Nothing -> throwError (ProgramUnknownType typeName)

ctorOwners :: [P.Alt] -> [String]
ctorOwners = foldr go []
  where
    go alt acc = case P.altPattern alt of
      P.PatCtor ctorName0 _ -> ctorName0 : acc
      P.PatAnn inner _ -> go (P.Alt inner (P.altExpr alt)) acc
      _ -> acc

catchAllPatternAnnotationType :: [P.Alt] -> Maybe SrcType
catchAllPatternAnnotationType alts =
  case alts of
    [P.Alt pattern0 _] -> patternAnnotationType pattern0
    _ -> Nothing

patternAnnotationType :: P.Pattern -> Maybe SrcType
patternAnnotationType pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      case patternAnnotationType inner of
        Just innerTy -> Just innerTy
        Nothing -> Just annTy
    _ -> Nothing

validatePatternType :: ElaborateScope -> SrcType -> P.Pattern -> ElaborateM ()
validatePatternType scope expectedTy pattern0 =
  case pattern0 of
    P.PatWildcard -> pure ()
    P.PatVar {} -> pure ()
    P.PatAnn inner annTy -> do
      validatePatternAnnotation scope expectedTy annTy
      validatePatternType scope expectedTy inner
    P.PatCtor ctorName0 patterns -> do
      ctorInfo <- lookupConstructorInfo ctorName0
      subst <-
        case matchPatternTypes (ctorResult ctorInfo) expectedTy of
          Just subst0 -> pure subst0
          Nothing -> throwError (ProgramPatternConstructorMismatch ctorName0 expectedTy)
      if length patterns /= length (ctorArgs ctorInfo)
        then throwError (ProgramPatternConstructorMismatch ctorName0 expectedTy)
        else
          mapM_
            ( \(nestedPattern, argTy) ->
                validatePatternType scope (specializeSrcType subst argTy) nestedPattern
            )
            (zip patterns (ctorArgs ctorInfo))
  where
    lookupConstructorInfo ctorName0 =
      case Map.lookup ctorName0 (esValues scope) of
        Just ConstructorValue {valueCtorInfo = ctorInfo} -> pure ctorInfo
        _ -> throwError (ProgramUnknownConstructor ctorName0)

    matchPatternTypes template actual =
      case matchTypes Map.empty template actual of
        Just subst -> Just subst
        Nothing ->
          case matchTypes Map.empty actual template of
            Just subst -> Just subst
            Nothing
              | lowerType scope template == lowerType scope actual -> Just Map.empty
            Nothing -> Nothing

validatePatternAnnotation :: ElaborateScope -> SrcType -> SrcType -> ElaborateM ()
validatePatternAnnotation scope expectedTy annTy =
  when (not (patternAnnotationCompatible expectedTy annTy)) $
    throwError (ProgramTypeMismatch annTy expectedTy)
  where
    patternAnnotationCompatible left right =
      lowerType scope left == lowerType scope right
        || matchTypes Map.empty left right /= Nothing
        || matchTypes Map.empty right left /= Nothing

validateOrderedPatterns :: [P.Alt] -> ElaborateM ()
validateOrderedPatterns = go Set.empty False
  where
    go _ _ [] = pure ()
    go seen catchAllSeen (P.Alt pattern0 _ : rest)
      | catchAllSeen =
          throwError (ProgramDuplicateCaseBranch (maybe "_" id (topConstructorName pattern0)))
      | isCatchAllPattern pattern0 =
          go seen True rest
      | Just ctorName0 <- topConstructorName pattern0,
        ctorName0 `Set.member` seen =
          throwError (ProgramDuplicateCaseBranch ctorName0)
      | Just ctorName0 <- flatCatchAllConstructor pattern0 =
          go (Set.insert ctorName0 seen) False rest
      | otherwise = go seen False rest

topConstructorName :: P.Pattern -> Maybe String
topConstructorName pattern0 =
  case stripPatternAnn pattern0 of
    P.PatCtor ctorName0 _ -> Just ctorName0
    _ -> Nothing

flatCatchAllConstructor :: P.Pattern -> Maybe String
flatCatchAllConstructor pattern0 =
  case stripPatternAnn pattern0 of
    P.PatCtor ctorName0 patterns
      | all isCatchAllPattern patterns -> Just ctorName0
    _ -> Nothing

isCatchAllPattern :: P.Pattern -> Bool
isCatchAllPattern pattern0 =
  case stripPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    _ -> False

patternCouldMatchConstructor :: ConstructorInfo -> P.Pattern -> Bool
patternCouldMatchConstructor ctorInfo pattern0 =
  case stripPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    P.PatCtor ctorName0 _ -> ctorName0 == ctorName ctorInfo
    P.PatAnn inner _ -> patternCouldMatchConstructor ctorInfo inner

stripPatternAnn :: P.Pattern -> P.Pattern
stripPatternAnn pattern0 =
  case pattern0 of
    P.PatAnn inner _ -> stripPatternAnn inner
    _ -> pattern0

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
  fst <$> resolveInstanceInfoWithSubst scope className0 headTy

resolveInstanceInfoWithSubst :: ElaborateScope -> P.ClassName -> SrcType -> Either ProgramError (InstanceInfo, Map String SrcType)
resolveInstanceInfoWithSubst scope className0 headTy =
  case matches of
    [match] -> Right match
    [] -> Left (ProgramNoMatchingInstance className0 headTy)
    _ -> Left (ProgramNoMatchingInstance className0 headTy)
  where
    matches =
      [ (info, subst)
        | info <- esInstances scope,
          instanceClassName info == className0,
          Just subst <- [matchInstanceHead info]
      ]

    matchInstanceHead info =
      case matchTypes Map.empty (instanceHeadType info) headTy of
        Just subst -> Just subst
        Nothing ->
          if lowerType scope (instanceHeadType info) == lowerType scope headTy
            then Just Map.empty
            else Nothing

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

quantifyFreeTypeVars :: SrcType -> SrcType
quantifyFreeTypeVars ty =
  foldr (\name acc -> STForall name Nothing acc) ty (sort (Set.toList (freeTypeVarsSrcType ty)))

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

extendConstraintEvidence :: ElaborateScope -> [P.ClassConstraint] -> ElaborateM (ElaborateScope, [(String, SrcType)])
extendConstraintEvidence scope constraints = do
  built <- mapM buildEvidence constraints
  let evidenceInfos = concatMap fst built
      params = concatMap snd built
  let runtimeTypes = Map.fromList params
  pure
    ( scope
        { esEvidence = evidenceInfos ++ esEvidence scope,
          esRuntimeTypes = runtimeTypes `Map.union` esRuntimeTypes scope
        },
      params
    )
  where
    buildEvidence constraint = do
      classInfo <-
        case Map.lookup (P.constraintClassName constraint) (esClasses scope) of
          Just info -> pure info
          Nothing -> throwError (ProgramUnknownClass (P.constraintClassName constraint))
      methodEntries <-
        mapM
          ( \methodInfo -> do
              runtimeName <- freshRuntimeName ("evidence_" ++ P.constraintClassName constraint ++ "_" ++ methodName methodInfo)
              let evidenceTy = lowerType scope (specializeMethodType (methodType methodInfo) (classParamName classInfo) (P.constraintType constraint))
              pure (methodName methodInfo, (runtimeName, evidenceTy))
          )
          (Map.elems (classMethods classInfo))
      let evidenceInfo =
            EvidenceInfo
              { evidenceClassName = P.constraintClassName constraint,
                evidenceType = P.constraintType constraint,
                evidenceMethods = Map.fromList methodEntries
              }
          params = map snd methodEntries
      pure ([evidenceInfo], params)

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
              valueConstraints = [],
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
              valueConstraints = [],
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
      P.PatCtor _ patterns -> concatMap patternBinders patterns
      P.PatVar name -> [name]
      P.PatWildcard -> []
      P.PatAnn inner _ -> patternBinders inner

collectApps :: P.Expr -> (P.Expr, [P.Expr])
collectApps = go []
  where
    go acc (EApp fun arg) = go (arg : acc) fun
    go acc headExpr = (headExpr, acc)


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

freshDeferredConstructorName :: String -> ElaborateM String
freshDeferredConstructorName ctorName0 = do
  n <- freshCounter
  pure ("$deferred_ctor_" ++ ctorName0 ++ "_" ++ show n)

freshDeferredCaseName :: String -> ElaborateM String
freshDeferredCaseName typeName = do
  n <- freshCounter
  pure ("$deferred_case_" ++ typeName ++ "_" ++ show n)

freshCounter :: ElaborateM Int
freshCounter = do
  state <- get
  let n = elaborateFreshCounter state
  modify (\state' -> state' {elaborateFreshCounter = n + 1})
  pure n
