{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    mkElaborateScope,
    elaborateConstructorBinding,
    elaborateExprBinding,
    lowerType,
  )
where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, evalState, get, modify)
import Data.List (find, nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.Pipeline (ExternalEnv, renderPipelineError, runPipelineElabWithEnv)
import MLF.Elab.Types (ElabTerm, ElabType)
import qualified MLF.Elab.Types as X
import MLF.Frontend.Normalize (normalizeExpr, normalizeType, substSrcType)
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax
  ( Expr (..),
    Lit (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    SurfaceExpr,
  )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarsType, freshNameLike)

data ElaborateScope = ElaborateScope
  { esValues :: Map String ValueInfo,
    esRuntimeTypes :: Map String SrcType,
    esTypes :: Map String DataInfo,
    esInstances :: [InstanceInfo]
  }

type ElaborateM a = ExceptT ProgramError (State Int) a

runElaborateM :: ElaborateM a -> Either ProgramError a
runElaborateM action = evalState (runExceptT action) 0

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

{- Note [recoverSourceType]

When the eMLF pipeline infers a type, it returns raw Church-encoded μ forms
with fresh binder names (e.g. STMu "t8" (STForall "t9" Nothing ...)).  The
.mlfp surface layer expects named ADT heads (STBase "Nat", STCon "List" ...).
recoverSourceType walks an inferred SrcType and replaces any sub-tree that is
alpha-equivalent to a known data type's lowered form with the corresponding
named source head type.

This is used in inferSurfaceType so that downstream .mlfp logic (instance
resolution, value-app specialization) sees named types.
-}
recoverSourceType :: ElaborateScope -> SrcType -> SrcType
recoverSourceType scope = recover
  where
    -- Pre-compute the lowered ElabType for each nullary data type head.
    -- For parameterized data types we cannot recover without argument
    -- extraction, so we only handle the nullary case for now.
    nullaryHeads :: [(ElabType, SrcType)]
    nullaryHeads =
      [ (srcTypeToElabType (lowerType scope (STBase name)), STBase name)
        | (name, info) <- Map.toList (esTypes scope),
          null (dataParams info)
      ]

    recover ty =
      -- First check if the whole type matches a known data head
      case lookupHead ty of
        Just headTy -> headTy
        Nothing -> recoverChildren ty

    lookupHead ty =
      let elabTy = srcTypeToElabType ty
       in case find (\(lowered, _) -> alphaEqType elabTy lowered || churchAwareEqType elabTy lowered) nullaryHeads of
            Just (_, headTy) -> Just headTy
            Nothing -> Nothing

    recoverChildren ty = case ty of
      STVar {} -> ty
      STBase {} -> ty
      STBottom -> ty
      STArrow dom cod -> STArrow (recover dom) (recover cod)
      STForall name mb body ->
        STForall name (fmap (SrcBound . recover . unSrcBound) mb) (recover body)
      STMu name body -> STMu name (recover body)
      STCon name args -> STCon name (fmap recover args)

elaborateConstructorBinding :: ElaborateScope -> ConstructorInfo -> Either ProgramError CheckedBinding
elaborateConstructorBinding scope ctorInfo =
  let surfaceExpr = constructorSurfaceExpr scope ctorInfo
      expectedTy = lowerType scope (ctorType ctorInfo)
   in do
        (term, actualTy) <- elaborateSurfaceExprInfo scope surfaceExpr
        let expectedTyElab = srcTypeToElabType expectedTy
            (termNorm, _actualTyNorm) = stripVacuousForallsAndTyAbs term actualTy
        pure
          CheckedBinding
            { checkedBindingName = ctorRuntimeName ctorInfo,
              checkedBindingSourceType = expectedTy,
              checkedBindingSurfaceExpr = surfaceExpr,
              checkedBindingTerm = termNorm,
              checkedBindingType = expectedTyElab,
              checkedBindingExportedAsMain = False
            }

elaborateExprBinding :: ElaborateScope -> String -> SrcType -> Bool -> P.Expr -> Either ProgramError CheckedBinding
elaborateExprBinding scope runtimeName expectedTy exportedAsMain expr = do
  surfaceExpr <- runElaborateM (compileExpr scope (Just expectedTy) expr)
  elaborateSurfaceBinding scope runtimeName expectedTy exportedAsMain surfaceExpr

elaborateSurfaceBinding :: ElaborateScope -> String -> SrcType -> Bool -> SurfaceExpr -> Either ProgramError CheckedBinding
elaborateSurfaceBinding scope runtimeName expectedTy exportedAsMain surfaceExpr = do
  (term, actualTy) <- elaborateSurfaceExprInfo scope surfaceExpr
  let expectedTy' = lowerType scope expectedTy
      (termNorm, actualTyNorm) = stripVacuousForallsAndTyAbs term actualTy
      actualTy' = stripVacuousForalls actualTyNorm
      expectedTyElab = stripVacuousForalls (srcTypeToElabType expectedTy')
      recoveredActualSrcTy = recoverSourceType scope (elabTypeToSrcType actualTy')
      recoveredActualTy = srcTypeToElabType (lowerType scope recoveredActualSrcTy)
  if alphaEqType actualTy' expectedTyElab
    || churchAwareEqType actualTy' expectedTyElab
    || alphaEqType recoveredActualTy expectedTyElab
    || churchAwareEqType recoveredActualTy expectedTyElab
    then
      Right
        CheckedBinding
          { checkedBindingName = runtimeName,
            checkedBindingSourceType = expectedTy',
            checkedBindingSurfaceExpr = surfaceExpr,
            checkedBindingTerm = termNorm,
            checkedBindingType = actualTy',
            checkedBindingExportedAsMain = exportedAsMain
          }
    else Left (ProgramTypeMismatch recoveredActualSrcTy expectedTy')

elaborateSurfaceExprInfo :: ElaborateScope -> SurfaceExpr -> Either ProgramError (ElabTerm, ElabType)
elaborateSurfaceExprInfo scope surfaceExpr = do
  let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
  envBindings <- traverse resolveRuntimeType freeVars
  let extEnv :: ExternalEnv
      extEnv = Map.fromList [(name, normTy) | (name, ty) <- envBindings, let normTy = either (error . show) id (normalizeType ty)]
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  (term, actualTy) <- either (Left . ProgramPipelineError . renderPipelineError) Right (runPipelineElabWithEnv Set.empty extEnv normExpr)
  pure (term, actualTy)
  where
    resolveRuntimeType name =
      case Map.lookup name (esRuntimeTypes scope) of
        Just ty -> Right (name, ty)
        Nothing -> Left (ProgramUnknownValue name)

constructorSurfaceExpr :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExpr scope ctorInfo =
  EAnn (constructorSurfaceExprRaw scope ctorInfo) (lowerType scope (ctorType ctorInfo))

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
          EApp
          (EVar (handlerNames !! ctorIndex ctorInfo))
          (map EVar argNames)
      body = foldr (\(handlerName, handlerTy) acc -> ELamAnn handlerName handlerTy acc) selectedHandler (zip handlerNames handlerTypes)
      lifted =
        foldr
          (\(argName, argTy) acc -> ELamAnn argName (lowerType scope argTy) acc)
          body
          (zip argNames (ctorArgs ctorInfo))
   in lifted
  where
    ctorOrder =
      maybe [] dataConstructors (Map.lookup (ctorOwningType ctorInfo) (esTypes scope))

compileExpr :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpr scope mbExpected expr = case expr of
  P.EVar name ->
    case Map.lookup name (esValues scope) of
      Just OverloadedMethod {} -> throwError (ProgramAmbiguousMethodUse name)
      Just OrdinaryValue {valueRuntimeName = runtimeName} -> pure (EVar runtimeName)
      Just ConstructorValue {valueCtorInfo = ctorInfo} ->
        let ctorExpr = if null (ctorArgs ctorInfo) then constructorSurfaceExprRaw scope ctorInfo else constructorSurfaceExpr scope ctorInfo
         in pure $ case mbExpected of
              Just expectedTy
                | null (ctorArgs ctorInfo), isRecursiveResultType expectedTy -> EAnn ctorExpr (lowerType scope expectedTy)
              _ -> ctorExpr
      Nothing -> throwError (ProgramUnknownValue name)
  P.ELit lit -> pure (ELit lit)
  P.ELam param body -> do
    runtimeName <- freshRuntimeName (P.paramName param)
    let paramTy = case (P.paramType param, mbExpected) of
          (Just ty, _) -> Just ty
          (Nothing, Just (STArrow dom _)) -> Just dom
          _ -> Nothing
    scope' <- extendLocal scope (P.paramName param) runtimeName paramTy
    bodyExpr0 <- compileExpr scope' (expectedCodomain mbExpected) body
    let bodyExpr =
          case expectedCodomain mbExpected of
            Just codTy | isRecursiveResultType codTy -> EAnn bodyExpr0 (lowerType scope codTy)
            _ -> bodyExpr0
    pure $
      case paramTy of
        Just ty -> ELamAnn runtimeName (lowerType scope ty) bodyExpr
        Nothing -> ELam runtimeName bodyExpr
  P.EApp _ _ -> compileApp scope mbExpected expr
  P.ELet name mbTy rhs body -> do
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
      Nothing -> inferSurfaceType scope (ELet runtimeName rhsExpr (EVar runtimeName))
    let rhsExpr' =
          case mbTy of
            Just ty -> EAnn rhsExpr (lowerType scope ty)
            Nothing -> rhsExpr
    bodyScope <- extendLocalLowered scope name runtimeName bindingTy
    bodyExpr <- compileExpr bodyScope mbExpected body
    pure (ELet runtimeName rhsExpr' bodyExpr)
  P.EAnn inner annTy -> do
    innerExpr <- compileExpr scope (Just annTy) inner
    pure (EAnn innerExpr (lowerType scope annTy))
  P.ECase scrutinee alts -> compileCase scope mbExpected scrutinee alts

compileApp :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileApp scope mbExpected expr =
  case collectApps expr of
    (P.EVar name, args)
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope) ->
          compileMethodApp scope mbExpected methodInfo args
      | Just valueInfo <- Map.lookup name (esValues scope) ->
          compileValueApp scope mbExpected valueInfo args
    (headExpr, args) -> do
      headSurface <- compileExpr scope Nothing headExpr
      argSurfaces <- mapM (compileExpr scope Nothing) args
      pure (foldl EApp headSurface argSurfaces)

explicitExprAnnotation :: P.Expr -> Maybe SrcType
explicitExprAnnotation expr =
  case expr of
    P.EAnn _ ty -> Just ty
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
          OrdinaryValue {valueRuntimeName = runtimeName} -> EVar runtimeName
          ConstructorValue {valueCtorInfo = ctorInfo} -> constructorSurfaceExpr scope ctorInfo
          OverloadedMethod {} -> error "compileValueApp does not handle overloaded methods"
      applied = foldl EApp headSurface argSurfaces
      knownResultTy = appliedValueResultType valueInfo (length args)
  pure $ case (valueInfo, mbExpected, knownResultTy) of
    (ConstructorValue {}, Just expectedTy, _)
      | isRecursiveResultType expectedTy -> EAnn applied (lowerType scope expectedTy)
    (ConstructorValue {}, _, Just resultTy)
      | isRecursiveResultType resultTy -> EAnn applied (lowerType scope resultTy)
    _ -> applied

compileMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileMethodApp scope _mbExpected methodInfo args
  | null args = throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
  | otherwise = do
      argTypes <- mapM inferArgType args
      let loweredMethodTy = lowerType scope (methodType methodInfo)
      classArgTy <-
        case inferClassArgument loweredMethodTy (methodParamName methodInfo) argTypes of
          Just ty -> pure ty
          Nothing -> throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
      argSurfaces <- mapM (compileExpr scope Nothing) args
      instanceInfo <- resolveInstance scope (methodClassName methodInfo) classArgTy
      case Map.lookup (methodName methodInfo) (instanceMethods instanceInfo) of
        Just OrdinaryValue {valueRuntimeName = runtimeName} ->
          pure (foldl EApp (EVar runtimeName) argSurfaces)
        _ -> throwError (ProgramUnknownMethod (methodName methodInfo))
  where
    inferArgType expr =
      case inferKnownExprType scope expr of
        Just ty -> pure ty
        Nothing -> inferSurfaceType scope =<< compileExpr scope Nothing expr

inferKnownExprType :: ElaborateScope -> P.Expr -> Maybe SrcType
inferKnownExprType scope expr =
  case expr of
    P.ELit lit -> Just (litSrcType lit)
    P.EVar name ->
      case Map.lookup name (esValues scope) of
        Just OrdinaryValue {valueType = ty} -> Just ty
        Just ConstructorValue {valueType = ty} -> Just ty
        _ -> Nothing
    P.EAnn _ annTy -> Just annTy
    P.EApp _ _ ->
      case collectApps expr of
        (P.EVar name, args)
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
              P.EVar _ -> True
              P.EAnn inner _ -> case inner of
                P.EVar _ -> True
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
              then EAnn scrutineeExpr (lowerType scope headTy)
              else scrutineeExpr
          recursiveVariableScrutinee = variableScrutinee && isRecursiveResultType resultTy
      let scrutineeCaseExpr =
            if shouldAnnotateScrutinee
              then
                if recursiveVariableScrutinee
                  then EAnn scrutineeExpr (lowerType scope headTy)
                  else EAnn scrutineeWithHeadTy (caseScrutineeType scope dataInfo resultTy)
              else scrutineeWithHeadTy
      pure (foldl EApp scrutineeCaseExpr handlers)

compileConstructorScrutineeRaw :: ElaborateScope -> P.Expr -> ElaborateM SurfaceExpr
compileConstructorScrutineeRaw scope expr =
  case collectApps expr of
    (P.EVar name, args)
      | Just ConstructorValue {valueCtorInfo = ctorInfo} <- Map.lookup name (esValues scope) -> do
          argSurfaces <-
            zipWithM
              (\ty arg -> compileExpr scope (Just ty) arg)
              (take (length args) (ctorArgs ctorInfo))
              args
          pure (foldl EApp (constructorSurfaceExprRaw scope ctorInfo) argSurfaces)
    (P.EAnn inner _, []) -> compileConstructorScrutineeRaw scope inner
    _ -> compileExpr scope Nothing expr

compileCatchAllOnly :: ElaborateScope -> Maybe SrcType -> SurfaceExpr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCatchAllOnly scope mbExpected scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> compileExpr scope mbExpected body
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName name
      scope' <- extendLocalLowered scope name runtimeName =<< freshTypeName
      bodyExpr <- compileExpr scope' mbExpected body
      pure (ELet runtimeName scrutineeExpr bodyExpr)
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
              (\(name, argTy) acc -> ELamAnn name (lowerType scope argTy) acc)
              bodyExpr
              (zip runtimeNames (ctorArgs ctorInfo))
      if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
        then pure handlerBody
        else do
          let handlerTy = handlerSurfaceType scope ctorInfo resultTyElab
          pure (EAnn handlerBody handlerTy)
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
              (\(name, argTy) acc -> ELamAnn name (lowerType scope argTy) acc)
              bodyExpr
              lambdaBinders
      if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
        then pure handlerBody
        else do
          let handlerTy = handlerSurfaceType scope ctorInfo resultTyElab
          pure (EAnn handlerBody handlerTy)
    P.PatVar name -> do
      scrutineeName <- freshRuntimeName name
      scope' <- extendLocalLowered scope name scrutineeName (lowerType scope (ctorResult ctorInfo))
      let resultTyElab = lowerType scope resultTy
      bodyExpr <- compileExpr scope' (Just resultTyElab) (P.altExpr alt)
      let handlerBody =
            foldr
              (\(name', argTy) acc -> ELamAnn name' (lowerType scope argTy) acc)
              (ELet scrutineeName scrutineeExpr bodyExpr)
              lambdaBinders
      if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
        then pure handlerBody
        else do
          let handlerTy = handlerSurfaceType scope ctorInfo resultTyElab
          pure (EAnn handlerBody handlerTy)
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
  P.EVar _ -> False
  P.EAnn inner _ -> shouldSpecializeCaseScrutinee inner
  _ -> True

isLocalOrdinaryCall :: ElaborateScope -> P.Expr -> Bool
isLocalOrdinaryCall scope expr =
  case collectApps expr of
    (P.EVar name, _ : _) ->
      case Map.lookup name (esValues scope) of
        Just OrdinaryValue {valueOriginModule = "<local>"} -> True
        _ -> False
    _ -> False

isConstructorCall :: ElaborateScope -> P.Expr -> Bool
isConstructorCall scope expr =
  case collectApps expr of
    (P.EVar name, _ : _) ->
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

resolveInstance :: ElaborateScope -> P.ClassName -> SrcType -> ElaborateM InstanceInfo
resolveInstance scope className0 headTy =
  case filter matches (esInstances scope) of
    [info] -> pure info
    [] -> throwError (ProgramNoMatchingInstance className0 headTy)
    _ -> throwError (ProgramNoMatchingInstance className0 headTy)
  where
    matches info =
      instanceClassName info == className0
        && alphaEqType
          (stripVacuousForalls (srcTypeToElabType (lowerType scope (instanceHeadType info))))
          (stripVacuousForalls (srcTypeToElabType (lowerType scope headTy)))

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

inferSurfaceType :: ElaborateScope -> SurfaceExpr -> ElaborateM SrcType
inferSurfaceType scope expr = do
  let freeVars = sort (Set.toList (surfaceFreeVars expr))
  bindings <- mapM resolveFreeVar freeVars
  let extEnv :: ExternalEnv
      extEnv = Map.fromList [(name, normTy) | (name, ty) <- bindings, let normTy = either (error . show) id (normalizeType ty)]
  normExpr <- either (throwError . ProgramPipelineError . show) pure (normalizeExpr expr)
  (_, inferredTy) <- either (throwError . ProgramPipelineError . renderPipelineError) pure (runPipelineElabWithEnv Set.empty extEnv normExpr)
  pure (stripVacuousSrcForalls (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls inferredTy))))
  where
    resolveFreeVar runtimeName =
      case Map.lookup runtimeName (esRuntimeTypes scope) of
        Just ty -> pure (runtimeName, ty)
        Nothing -> throwError (ProgramUnknownValue runtimeName)

stripVacuousForalls :: ElabType -> ElabType
stripVacuousForalls (X.TForall v _ body)
  | v `Set.notMember` freeTypeVarsType body = stripVacuousForalls body
stripVacuousForalls (X.TForall v mb body) =
  X.TForall v mb (stripVacuousForalls body)
stripVacuousForalls (X.TArrow dom cod) =
  X.TArrow (stripVacuousForalls dom) (stripVacuousForalls cod)
stripVacuousForalls (X.TMu name body) =
  X.TMu name (stripVacuousForalls body)
stripVacuousForalls ty = ty

stripVacuousForallsAndTyAbs :: ElabTerm -> ElabType -> (ElabTerm, ElabType)
stripVacuousForallsAndTyAbs (X.ETyAbs v _ termBody) (X.TForall tv _ tyBody)
  | v == tv,
    tv `Set.notMember` freeTypeVarsType tyBody =
      stripVacuousForallsAndTyAbs termBody tyBody
stripVacuousForallsAndTyAbs (X.ETyAbs v _ termBody) ty
  | v `Set.notMember` freeTypeVarsType ty =
      stripVacuousForallsAndTyAbs termBody ty
stripVacuousForallsAndTyAbs (X.ETyAbs v mb termBody) (X.TForall tv tmb tyBody)
  | v == tv =
      let (termBody', tyBody') = stripVacuousForallsAndTyAbs termBody tyBody
       in (X.ETyAbs v mb termBody', X.TForall tv tmb tyBody')
stripVacuousForallsAndTyAbs (X.ELam name lamTy termBody) (X.TArrow dom cod) =
  let (termBody', cod') = stripVacuousForallsAndTyAbs termBody cod
   in (X.ELam name lamTy termBody', X.TArrow dom cod')
stripVacuousForallsAndTyAbs (X.ERoll rty termBody) ty =
  (X.ERoll rty termBody, ty)
stripVacuousForallsAndTyAbs (X.ELet n sch rhs termBody) ty =
  let (rhs', _) = stripVacuousForallsAndTyAbs rhs (schemeToType sch)
      (termBody', ty') = stripVacuousForallsAndTyAbs termBody ty
   in (X.ELet n sch rhs' termBody', ty')
stripVacuousForallsAndTyAbs term ty = (term, stripVacuousForalls ty)

-- | Strip leading STForall binders that do not appear in the body (SrcType level).
-- Mirrors 'stripVacuousForalls' but operates on the surface type representation.
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

surfaceFreeVars :: SurfaceExpr -> Set String
surfaceFreeVars = go Set.empty
  where
    go bound expr = case expr of
      EVar name
        | name `Set.member` bound -> Set.empty
        | otherwise -> Set.singleton name
      ELit _ -> Set.empty
      ELam name body -> go (Set.insert name bound) body
      ELamAnn name _ body -> go (Set.insert name bound) body
      EApp fun arg -> go bound fun `Set.union` go bound arg
      ELet name rhs body -> go (Set.insert name bound) rhs `Set.union` go (Set.insert name bound) body
      EAnn inner _ -> go bound inner
      ECoerceConst _ -> Set.empty

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
  P.EVar name
    | name `Set.member` bound -> []
    | otherwise -> [name]
  P.ELit _ -> []
  P.ELam param body -> collectFreeValues (Set.insert (P.paramName param) bound) body
  P.EApp fun arg -> collectFreeValues bound fun ++ collectFreeValues bound arg
  P.ELet name _ rhs body -> collectFreeValues bound rhs ++ collectFreeValues (Set.insert name bound) body
  P.EAnn inner _ -> collectFreeValues bound inner
  P.ECase scrutinee alts ->
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
    go acc (P.EApp fun arg) = go (arg : acc) fun
    go acc headExpr = (headExpr, acc)

duplicates :: (Eq a) => [a] -> [a]
duplicates xs = [x | x <- nub xs, length (filter (== x) xs) > 1]

freshRuntimeName :: String -> ElaborateM String
freshRuntimeName base = do
  n <- get
  modify (+ 1)
  pure ("$" ++ base ++ "#" ++ show n)

freshTypeName :: ElaborateM SrcType
freshTypeName = do
  n <- get
  modify (+ 1)
  pure (STVar ("p$" ++ show n))

freshTypeVarName :: ElaborateM String
freshTypeVarName = do
  n <- get
  modify (+ 1)
  pure ("r$" ++ show n)

elabTypeToSrcType :: X.Ty v -> SrcType
elabTypeToSrcType ty = case ty of
  X.TVar name -> STVar name
  X.TArrow dom cod -> STArrow (elabTypeToSrcType dom) (elabTypeToSrcType cod)
  X.TBase (Graph.BaseTy name) -> STBase name
  X.TCon (Graph.BaseTy name) args ->
    case toListNE (fmap elabTypeToSrcType args) of
      x : xs -> STCon name (x :| xs)
      [] -> STBase name
  X.TForall name mb body ->
    STForall name (fmap (SrcBound . elabTypeToSrcType) mb) (elabTypeToSrcType body)
  X.TMu name body -> STMu name (elabTypeToSrcType body)
  X.TBottom -> STBottom

srcTypeToElabType :: SrcTy n v -> ElabType
srcTypeToElabType ty = case ty of
  STVar name -> X.TVar name
  STArrow dom cod -> X.TArrow (srcTypeToElabType dom) (srcTypeToElabType cod)
  STBase name -> X.TBase (Graph.BaseTy name)
  STCon name args -> X.TCon (Graph.BaseTy name) (fmap srcTypeToElabType args)
  STForall name mb body -> X.TForall name (mb >>= srcBoundToElabBound) (srcTypeToElabType body)
  STMu name body -> X.TMu name (srcTypeToElabType body)
  STBottom -> X.TBottom

srcBoundToElabBound :: SrcBound n -> Maybe X.BoundType
srcBoundToElabBound (SrcBound boundTy) =
  case srcTypeToElabType boundTy of
    X.TVar {} -> Nothing
    X.TBottom -> Nothing
    X.TArrow dom cod -> Just (X.TArrow dom cod)
    X.TBase base -> Just (X.TBase base)
    X.TCon con args -> Just (X.TCon con args)
    X.TForall name mb body -> Just (X.TForall name mb body)
    X.TMu name body -> Just (X.TMu name body)
