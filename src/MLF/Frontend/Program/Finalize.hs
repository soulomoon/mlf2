{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize
  ( finalizeBinding,
    finalizeBindingAllowOpaque,
    recoverSourceType,
    sourceForallMatches,
    stripVacuousForallsAndTypeAbs,
  )
where

import Control.Monad (foldM)
import Control.Applicative ((<|>))
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Elab.Pipeline
  ( Env (..),
    renderPipelineError,
    schemeFromType,
    schemeToType,
    typeCheckWithEnv,
  )
import MLF.Elab.Run.Pipeline
  ( PipelineElabDetailedResult (..),
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedUncheckedWithExternalBindings,
  )
import MLF.Elab.Types (ElabTerm, ElabType)
import qualified MLF.Elab.Types as X
import MLF.Frontend.ConstraintGen (ExternalBinding (..), ExternalBindingMode (..), ExternalBindings)
import MLF.Frontend.Normalize (normalizeExpr, normalizeType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeRuntimeTypes,
    classInfoForConstraint,
    inferClassArgument,
    lookupEvidenceMethodByClass,
    lowerType,
    lowerTypeView,
    matchTypes,
    matchTypesInScope,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    sourceTypeViewInScope,
    zeroMethodConstraintCoveredByEvidenceInfo,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    ConstructorInfo (..),
    ConstructorShape (..),
    DataInfo (..),
    DeferredMethodEvidence (..),
    DeferredCaseCall (..),
    DeferredBindingMode (..),
    DeferredConstructorCall (..),
    DeferredMethodCall (..),
    DeferredProgramObligation (..),
    ClassInfo (..),
    EvidenceInfo (..),
    InstanceInfo (..),
    LoweredBinding (..),
    MethodInfo (..),
    ProgramError (..),
    ConstraintInfo (..),
    TypeView (..),
    ValueInfo (..),
    applyConstraintInfoSubst,
    constructorOwnerRuntimeTypeTrackable,
    constructorOwnerShapes,
    constructorShapeFromInfo,
    freeTypeVarsTypeView,
    methodInfoOwnerClassSymbolIdentity,
    SymbolIdentity,
    splitArrows,
    splitForalls,
    specializeMethodType,
    substituteTypeVar,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), SrcBound (..), SrcTy (..), SrcType, SurfaceExpr)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarsType)

finalizeBindingAllowOpaque :: ElaborateScope -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingAllowOpaque scope lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) = do
      placeholderTy <- srcTypeToElabType (loweredBindingExpectedType lowered)
      let placeholder checked =
            checked
              { checkedBindingTerm = X.EVar (loweredBindingName lowered),
                checkedBindingType = placeholderTy
              }
          uncheckedPlaceholder =
            CheckedBinding
              { checkedBindingName = loweredBindingName lowered,
                checkedBindingSourceType = loweredBindingSourceType lowered,
                checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
                checkedBindingTerm = X.EVar (loweredBindingName lowered),
                checkedBindingType = placeholderTy,
                checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
              }
      case finalizeBinding scope lowered of
        Right checked
          | Map.null (loweredBindingDeferredObligations lowered) ->
              -- Successful elaboration can still satisfy an opaque forall by
              -- instantiating the expected type. Re-check no-obligation
              -- surfaces before replacing the checked term with a placeholder.
              case validateOpaqueBindingSurface scope lowered of
                Right () -> Right (placeholder checked)
                Left validationErr -> Left validationErr
          | otherwise -> Right (placeholder checked)
        Left err ->
          case validateOpaqueBindingSurface scope lowered of
            Right () -> Right uncheckedPlaceholder
            Left _ -> Left err
  | otherwise = finalizeBinding scope lowered

validateOpaqueBindingSurface :: ElaborateScope -> LoweredBinding -> Either ProgramError ()
validateOpaqueBindingSurface scope lowered
  | any (not . opaqueSurfaceObligationSupported) (Map.elems (loweredBindingDeferredObligations lowered)) =
      Left (ProgramPipelineError "opaque validation does not support deferred obligations")
  | otherwise =
      case inferOpaqueSurfaceType scope rigidVars runtimeTypes Map.empty (loweredBindingSurfaceExpr lowered) of
        Right actualTy
          | opaqueSourceCompatibleWithRigid rigidVars scope actualTy (loweredBindingExpectedType lowered) -> Right ()
          | otherwise -> Left (ProgramTypeMismatch actualTy (loweredBindingExpectedType lowered))
        Left err -> Left err
  where
    rigidVars = sourceForallBinders (loweredBindingExpectedType lowered)
    runtimeTypes =
      Map.withoutKeys (loweredBindingExternalTypes lowered) Builtins.builtinOpaqueValueNames
        `Map.union` elaborateScopeRuntimeTypes scope

-- Opaque placeholders discard the checked term, so constructor rewrites are
-- harmless after source-level retyping. Method and case obligations still carry
-- evidence or inspection behavior and must not be skipped here.
opaqueSurfaceObligationSupported :: DeferredProgramObligation -> Bool
opaqueSurfaceObligationSupported obligation =
  case obligation of
    DeferredConstructor {} -> True
    _ -> False

sourceForallBinders :: SrcType -> Set String
sourceForallBinders ty =
  case ty of
    STForall name _ body -> Set.insert name (sourceForallBinders body)
    _ -> Set.empty

inferOpaqueSurfaceType :: ElaborateScope -> Set String -> Map String SrcType -> Map String SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceType scope rigidVars runtimeTypes localTypes expr =
  case expr of
    EVar name ->
      case Map.lookup name localTypes <|> Map.lookup name runtimeTypes of
        Just ty -> Right ty
        Nothing -> Left (ProgramUnknownValue name)
    ELit lit -> Right (literalSourceType lit)
    ELamAnn name ty body ->
      STArrow ty <$> inferOpaqueSurfaceType scope rigidVars runtimeTypes (Map.insert name ty localTypes) body
    ELam {} ->
      Left (ProgramPipelineError "opaque validation needs lambda annotations")
    EApp fun arg -> do
      funTy <- inferOpaqueSurfaceType scope rigidVars runtimeTypes localTypes fun
      argTy <- inferOpaqueSurfaceType scope rigidVars runtimeTypes localTypes arg
      applyOpaqueFunctionType scope funTy argTy
    ELet name rhs body -> do
      rhsTy <- inferOpaqueSurfaceType scope rigidVars runtimeTypes localTypes rhs
      inferOpaqueSurfaceType scope rigidVars runtimeTypes (Map.insert name rhsTy localTypes) body
    EAnn inner annTy -> do
      actualTy <- inferOpaqueSurfaceType scope rigidVars runtimeTypes localTypes inner
      if opaqueSourceCompatibleWithRigid rigidVars scope actualTy annTy
        then Right annTy
        else Left (ProgramTypeMismatch actualTy annTy)

applyOpaqueFunctionType :: ElaborateScope -> SrcType -> SrcType -> Either ProgramError SrcType
applyOpaqueFunctionType scope funTy argTy =
  case snd (splitForalls funTy) of
    STArrow paramTy resultTy ->
      case matchTypesInScope scope Map.empty paramTy argTy <|> matchTypesInScope scope Map.empty (lowerType scope paramTy) (lowerType scope argTy) of
        Just subst -> Right (Map.foldrWithKey substituteTypeVar resultTy subst)
        Nothing
          | opaqueSourceCompatible scope argTy paramTy -> Right resultTy
          | otherwise -> Left (ProgramTypeMismatch argTy paramTy)
    other -> Left (ProgramExpectedFunction other)

opaqueSourceCompatible :: ElaborateScope -> SrcType -> SrcType -> Bool
opaqueSourceCompatible = opaqueSourceCompatibleWithRigid Set.empty

opaqueSourceCompatibleWithRigid :: Set String -> ElaborateScope -> SrcType -> SrcType -> Bool
opaqueSourceCompatibleWithRigid rigidVars scope actualTy expectedTy =
  alphaEqSrcType actualTy expectedTy
    || alphaEqSrcType (lowerType scope actualTy) (lowerType scope expectedTy)
    || sourceForallMatchesWithRigidForalls expectedTy actualTy
    || matchTypesInScopePreservingRigid scope rigidVars expectedTy actualTy /= Nothing
    || matchTypesInScopePreservingRigid scope rigidVars actualTy expectedTy /= Nothing

matchTypesInScopePreservingRigid :: ElaborateScope -> Set String -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesInScopePreservingRigid scope rigidVars template actual = do
  subst <- matchTypesInScope scope Map.empty template actual
  if all preservesRigid (Map.toList subst)
    then Just subst
    else Nothing
  where
    preservesRigid (name, ty)
      | name `Set.notMember` rigidVars = True
      | otherwise =
          case ty of
            STVar {} -> True
            STVarApp {} -> True
            _ -> False

literalSourceType :: Lit -> SrcType
literalSourceType lit =
  case lit of
    LInt _ -> STBase "Int"
    LBool _ -> STBase "Bool"
    LString _ -> STBase "String"

finalizeBinding :: ElaborateScope -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBinding scope lowered = do
  PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipeline
      scope
      (constructorBindingNeedsUnchecked scope (loweredBindingName lowered))
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypes lowered)
      (loweredBindingSurfaceExpr lowered)
  (term, actualTy) <-
    finalizeDeferredObligations scope (loweredBindingDeferredObligations lowered) tcEnv term0 actualTy0 (loweredBindingExpectedType lowered)
  let isUncheckedConstructor = constructorBindingNeedsUnchecked scope (loweredBindingName lowered)
      acceptedTerm0 = repairConstructorBindingTerm scope (loweredBindingName lowered) term
  (acceptedTy, acceptedTerm) <-
    if isUncheckedConstructor
      then do
        expectedTy <- srcTypeToElabType (loweredBindingExpectedType lowered)
        Right (expectedTy, acceptedTerm0)
      else Right (stripVacuousForallsAndTypeAbs actualTy acceptedTerm0)
  if isUncheckedConstructor
    then
      Right
        CheckedBinding
          { checkedBindingName = loweredBindingName lowered,
            checkedBindingSourceType = loweredBindingSourceType lowered,
            checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
            checkedBindingTerm = acceptedTerm,
            checkedBindingType = acceptedTy,
            checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
          }
    else do
      let actualTyForCompare = stripVacuousForalls actualTy
          recoveredActualSrcTy = recoverSourceType scope (elabTypeToSrcType actualTyForCompare)
      expectedTyForCompare <- stripVacuousForalls <$> srcTypeToElabType (loweredBindingExpectedType lowered)
      recoveredActualTy <- srcTypeToElabType (lowerType scope recoveredActualSrcTy)
      let sourceForallCompatible =
            if Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered)
              then sourceForallMatchesWithRigidForalls (recoverSourceType scope (loweredBindingExpectedType lowered)) recoveredActualSrcTy
              else sourceForallMatches (recoverSourceType scope (loweredBindingExpectedType lowered)) recoveredActualSrcTy
      if alphaEqType actualTyForCompare expectedTyForCompare
        || churchAwareEqType actualTyForCompare expectedTyForCompare
        || alphaEqType recoveredActualTy expectedTyForCompare
        || churchAwareEqType recoveredActualTy expectedTyForCompare
        || sourceForallCompatible
        then
          Right
            CheckedBinding
              { checkedBindingName = loweredBindingName lowered,
                checkedBindingSourceType = loweredBindingSourceType lowered,
                checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
                checkedBindingTerm = acceptedTerm,
                checkedBindingType = acceptedTy,
                checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
              }
        else Left (ProgramTypeMismatch recoveredActualSrcTy (loweredBindingExpectedType lowered))

constructorBindingNeedsUnchecked :: ElaborateScope -> String -> Bool
constructorBindingNeedsUnchecked scope runtimeName =
  or
    [ ctorRuntimeName ctor == runtimeName
        && (not (null (ctorForalls ctor)) || not (null (dataParams dataInfo)))
      | dataInfo <- Map.elems (elaborateScopeDataTypes scope),
        ctor <- dataConstructors dataInfo
    ]

repairConstructorBindingTerm :: ElaborateScope -> String -> ElabTerm -> ElabTerm
repairConstructorBindingTerm scope runtimeName term =
  case lookupConstructorRuntime scope runtimeName of
    Just (dataInfo, ctor)
      | not (null (dataParams dataInfo)) ->
          moveConstructorResultAbs (dataName dataInfo) (length (ctorArgs ctor)) term
    _ -> term

lookupConstructorRuntime :: ElaborateScope -> String -> Maybe (DataInfo, ConstructorInfo)
lookupConstructorRuntime scope runtimeName =
  case
    [ (dataInfo, ctor)
      | dataInfo <- Map.elems (elaborateScopeDataTypes scope),
        ctor <- dataConstructors dataInfo,
        ctorRuntimeName ctor == runtimeName
    ]
  of
    match : _ -> Just match
    [] -> Nothing

data TypeAbsInfo = TypeAbsInfo
  { typeAbsName :: String,
    typeAbsBound :: Maybe X.BoundType
  }

data TermLamInfo = TermLamInfo String ElabType

data ConstructorSpineItem
  = SpineTypeAbs TypeAbsInfo
  | SpineLam TermLamInfo

moveConstructorResultAbs :: String -> Int -> ElabTerm -> ElabTerm
moveConstructorResultAbs typeName argCount term =
  let (spine, body) = collectConstructorSpine term
      typeAbs = [info | SpineTypeAbs info <- spine]
      lams = [info | SpineLam info <- spine]
      (resultAbs, otherAbs) = partitionResultAbs typeAbs
      (argLams, handlerLams) = splitAt argCount lams
   in wrapTypeAbs otherAbs (wrapLams argLams (wrapTypeAbs resultAbs (wrapLams handlerLams body)))
  where
    resultPrefix = "$" ++ typeName ++ "_result"

    partitionResultAbs =
      foldr
        ( \absInfo (results, others) ->
            if resultPrefix `isPrefixOf` typeAbsName absInfo
              then (absInfo : results, others)
              else (results, absInfo : others)
        )
        ([], [])

collectConstructorSpine :: ElabTerm -> ([ConstructorSpineItem], ElabTerm)
collectConstructorSpine = go []
  where
    go acc = \case
      X.ETyAbs name mb body -> go (acc ++ [SpineTypeAbs (TypeAbsInfo name mb)]) body
      X.ELam name ty body -> go (acc ++ [SpineLam (TermLamInfo name ty)]) body
      other -> (acc, other)

wrapTypeAbs :: [TypeAbsInfo] -> ElabTerm -> ElabTerm
wrapTypeAbs infos body =
  foldr (\TypeAbsInfo {typeAbsName = name, typeAbsBound = mb} acc -> X.ETyAbs name mb acc) body infos

wrapLams :: [TermLamInfo] -> ElabTerm -> ElabTerm
wrapLams infos body =
  foldr (\(TermLamInfo name ty) acc -> X.ELam name ty acc) body infos

runSurfacePipeline :: ElaborateScope -> Bool -> Map String DeferredProgramObligation -> Map String SrcType -> SurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipeline scope forceUnchecked deferredObligations externalTypes surfaceExpr = do
  let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
  envBindings <- traverse resolveRuntimeType freeVars
  extEnvEntries <- traverse externalBindingFor envBindings
  let extEnv :: ExternalBindings
      extEnv = Map.fromList extEnvEntries
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  let runPipeline =
        if not forceUnchecked && Map.null deferredObligations
          then runPipelineElabDetailedWithExternalBindings
          else runPipelineElabDetailedUncheckedWithExternalBindings
  either (Left . ProgramPipelineError . renderPipelineError) Right (runPipeline Set.empty extEnv normExpr)
  where
    runtimeTypes = externalTypes `Map.union` elaborateScopeRuntimeTypes scope

    resolveRuntimeType name =
      case Map.lookup name runtimeTypes of
        Just ty -> Right (name, ty)
        Nothing -> Left (ProgramUnknownValue name)

    externalBindingFor (name, ty) = do
      normTy <- either (Left . ProgramPipelineError . show) Right (normalizeType ty)
      Right
        ( name,
          ExternalBinding
            { externalBindingType = normTy,
              externalBindingMode = externalBindingModeFor name
            }
        )

    externalBindingModeFor name =
      case Map.lookup name deferredObligations of
        Just (DeferredMethod {}) ->
          case Map.lookup name externalTypes of
            Just ty
              | not (Set.null (freeTypeVarsSrcTypeLocal ty)) ->
                  ExternalBindingMonomorphic
            _ -> ExternalBindingScheme
        Just (DeferredConstructor deferred) -> convertDeferredBindingMode (deferredConstructorBindingMode deferred)
        Just (DeferredCase {}) -> ExternalBindingMonomorphic
        _ -> ExternalBindingScheme

    convertDeferredBindingMode mode =
      case mode of
        DeferredBindingScheme -> ExternalBindingScheme
        DeferredBindingMonomorphic -> ExternalBindingMonomorphic

    freeTypeVarsSrcTypeLocal = go Set.empty
      where
        go boundVars ty =
          case ty of
            STVar name
              | name `Set.member` boundVars -> Set.empty
              | otherwise -> Set.singleton name
            STArrow dom cod -> go boundVars dom `Set.union` go boundVars cod
            STBase {} -> Set.empty
            STCon _ args -> foldMap (go boundVars) args
            STVarApp name args ->
              let headVars =
                    if name `Set.member` boundVars
                      then Set.empty
                      else Set.singleton name
               in headVars `Set.union` foldMap (go boundVars) args
            STForall name mb body ->
              maybe Set.empty (go boundVars . unSrcBound) mb
                `Set.union` go (Set.insert name boundVars) body
            STMu name body -> go (Set.insert name boundVars) body
            STBottom -> Set.empty

finalizeDeferredObligations ::
  ElaborateScope ->
  Map String DeferredProgramObligation ->
  Env ->
  ElabTerm ->
  ElabType ->
  SrcType ->
  Either ProgramError (ElabTerm, ElabType)
finalizeDeferredObligations _ deferredObligations _ term inferredTy _
  | Map.null deferredObligations = Right (term, inferredTy)
finalizeDeferredObligations scope deferredObligations tcEnv term _ _ = do
  rewriteEnv <- extendTypeCheckEnvWithRuntimeScope scope tcEnv
  let constructorObligations = Map.mapMaybe onlyConstructor deferredObligations
      caseObligations = Map.mapMaybe onlyCase deferredObligations
      methodObligations = Map.mapMaybe onlyMethod deferredObligations
  constructorsRewritten <- resolveDeferredConstructors scope rewriteEnv constructorObligations term
  (caseRewriteEnv, casesRewritten) <- resolveDeferredCases scope caseObligations rewriteEnv constructorsRewritten
  methodsRewritten <- resolveDeferredMethods scope methodObligations caseRewriteEnv casesRewritten
  rewritten <- refreshLetSchemes caseRewriteEnv methodsRewritten
  rewrittenTy <-
    case typeCheckWithEnv caseRewriteEnv rewritten of
      Right ty -> Right (inlineTypeEnvBounds caseRewriteEnv ty)
      Left err ->
        Left (ProgramPipelineError ("deferred program obligation rewrite failed type check: " ++ show err))
  Right (rewritten, rewrittenTy)
  where
    onlyConstructor = \case
      DeferredConstructor deferred -> Just deferred
      _ -> Nothing

    onlyCase = \case
      DeferredCase deferred -> Just deferred
      _ -> Nothing

    onlyMethod = \case
      DeferredMethod deferred -> Just deferred
      _ -> Nothing

extendTypeCheckEnvWithRuntimeScope :: ElaborateScope -> Env -> Either ProgramError Env
extendTypeCheckEnvWithRuntimeScope scope env = do
  runtimeEnv <- traverse srcTypeToElabType (elaborateScopeRuntimeTypes scope)
  Right
    env
      { termEnv =
          termEnv env
            `Map.union` runtimeEnv
      }

inlineTypeEnvBounds :: Env -> ElabType -> ElabType
inlineTypeEnvBounds env = go Set.empty
  where
    go seen ty = case ty of
      X.TVar name
        | name `Set.member` seen -> ty
        | otherwise ->
            case Map.lookup name (typeEnv env) of
              Just bound
                | bound /= X.TBottom -> go (Set.insert name seen) bound
              _ -> ty
      X.TArrow dom cod -> X.TArrow (go seen dom) (go seen cod)
      X.TCon con args -> X.TCon con (fmap (go seen) args)
      X.TBase {} -> ty
      X.TBottom -> ty
      X.TForall name mb body ->
        let seen' = Set.insert name seen
         in X.TForall name (fmap (goBound seen') mb) (go seen' body)
      X.TMu name body ->
        let seen' = Set.insert name seen
         in X.TMu name (go seen' body)

    goBound seen bound = case bound of
      X.TArrow dom cod -> X.TArrow (go seen dom) (go seen cod)
      X.TCon con args -> X.TCon con (fmap (go seen) args)
      X.TBase {} -> bound
      X.TBottom -> bound
      X.TForall name mb body ->
        let seen' = Set.insert name seen
         in X.TForall name (fmap (goBound seen') mb) (go seen' body)
      X.TMu name body ->
        let seen' = Set.insert name seen
         in X.TMu name (go seen' body)

inferRewrittenLetType :: Env -> ElabTerm -> ElabType -> ElabType
inferRewrittenLetType env rhs fallback =
  case typeCheckWithEnv env rhs of
    Right ty -> inlineTypeEnvBounds env (stripVacuousForalls ty)
    Left _ -> fallback

freeSourceTypeVars :: SrcType -> Set String
freeSourceTypeVars = go Set.empty
  where
    go boundVars ty =
      case ty of
        STVar name
          | name `Set.member` boundVars -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go boundVars dom `Set.union` go boundVars cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go boundVars) args
        STVarApp name args ->
          let headVars =
                if name `Set.member` boundVars
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go boundVars) args
        STForall name mb body ->
          maybe Set.empty (go boundVars . unSrcBound) mb
            `Set.union` go (Set.insert name boundVars) body
        STMu name body -> go (Set.insert name boundVars) body
        STBottom -> Set.empty

sourceForallMatchesWithRigidForalls :: SrcType -> SrcType -> Bool
sourceForallMatchesWithRigidForalls expected actual =
  case sourceForallMatchSubst expected actual of
    Just subst -> all (forallBinderRemainsPolymorphic subst) (usedLeadingForallNames expected)
    Nothing -> False
  where
    forallBinderRemainsPolymorphic subst name =
      case Map.lookup name subst of
        Just STVar {} -> True
        Just STVarApp {} -> True
        Just _ -> False
        Nothing -> True

    usedLeadingForallNames ty =
      case ty of
        STForall name _ body
          | name `Set.member` freeSourceTypeVars body -> name : usedLeadingForallNames body
          | otherwise -> usedLeadingForallNames body
        _ -> []

sourceForallMatches :: SrcType -> SrcType -> Bool
sourceForallMatches expected actual =
  case sourceForallMatchSubst expected actual of
    Just _ -> True
    Nothing -> False

sourceForallMatchSubst :: SrcType -> SrcType -> Maybe (Map String SrcType)
sourceForallMatchSubst expected actual =
  match Set.empty Map.empty expected actual
  where
    match bound subst template actualTy =
      case template of
        STForall name mb body ->
          case actualTy of
            STForall actualName actualMb actualBody -> do
              let bound' = Set.insert name bound
                  subst' = Map.insert name (STVar actualName) (Map.delete name subst)
              subst'' <- matchForallBounds bound' subst' mb actualMb
              match
                bound'
                subst''
                body
                actualBody
            _ -> match (Set.insert name bound) (Map.delete name subst) body actualTy
        STVar name
          | name `Set.member` bound ->
              matchBoundVar subst name actualTy
          | otherwise ->
              case actualTy of
                STVar actualName | actualName == name -> Just subst
                _ -> Nothing
        STArrow dom cod ->
          case actualTy of
            STForall name _ body
              | name `Set.notMember` freeTypeVarsSrcTypeLocal body ->
                  match bound subst template body
            STArrow dom' cod' -> do
              subst' <- match bound subst dom dom'
              match bound subst' cod cod'
            _ -> Nothing
        STBase name ->
          case actualTy of
            STBase actualName | actualName == name -> Just subst
            _ -> Nothing
        STCon name args ->
          case actualTy of
            STCon actualName actualArgs
              | actualName == name && length (toListNE args) == length (toListNE actualArgs) ->
                  foldM
                    (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                    subst
                    (zip (toListNE args) (toListNE actualArgs))
            _ -> Nothing
        STVarApp name args ->
          matchVarApp bound subst name args actualTy
        STMu _ body -> match bound subst body actualTy
        STBottom ->
          case actualTy of
            STBottom -> Just subst
            _ -> Nothing

    matchForallBounds bound subst expectedMb actualMb =
      case (expectedMb, actualMb) of
        (Nothing, Nothing) -> Just subst
        (Just (SrcBound expectedBound), Just (SrcBound actualBound)) ->
          match bound subst expectedBound actualBound
        _ -> Nothing

    matchVarApp bound subst expectedName args actualTy
      | expectedName `Set.member` bound =
          case actualTy of
            STCon actualName actualArgs ->
              matchAppliedHead actualName toConHead (toListNE actualArgs)
            STVarApp actualName actualArgs ->
              matchAppliedHead actualName toVarHead (toListNE actualArgs)
            _ -> Nothing
      | otherwise =
          matchRigidVarAppHead expectedName
      where
        expectedArgs = toListNE args
        expectedArgCount = length expectedArgs

        matchAppliedHead actualName headFromPrefix actualArgs
          | length actualArgs < expectedArgCount = Nothing
          | otherwise = do
              let (headArgs, appliedArgs) = splitAt (length actualArgs - expectedArgCount) actualArgs
              subst' <- matchBoundVar subst expectedName (headFromPrefix actualName headArgs)
              foldM
                (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                subst'
                (zip expectedArgs appliedArgs)

        matchRigidVarAppHead rigidName =
          case actualTy of
            STVarApp actualName actualArgs
              | rigidName == actualName && expectedArgCount == length (toListNE actualArgs) ->
                  foldM
                    (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                    subst
                    (zip expectedArgs (toListNE actualArgs))
            _ -> Nothing

        toConHead actualName [] = STBase actualName
        toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

        toVarHead actualName [] = STVar actualName
        toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

    matchBoundVar subst name actualTy =
      case Map.lookup name subst of
        Nothing -> Just (Map.insert name actualTy subst)
        Just existing
          | alphaEqSrcType existing actualTy -> Just subst
          | otherwise -> Nothing

    freeTypeVarsSrcTypeLocal = freeSourceTypeVars

alphaEqSrcType :: SrcType -> SrcType -> Bool
alphaEqSrcType = go Map.empty Map.empty
  where
    go leftNames rightNames left right =
      case (left, right) of
        (STVar leftName, STVar rightName) ->
          sameTypeVar leftNames rightNames leftName rightName
        (STArrow leftDom leftCod, STArrow rightDom rightCod) ->
          go leftNames rightNames leftDom rightDom
            && go leftNames rightNames leftCod rightCod
        (STBase leftName, STBase rightName) -> leftName == rightName
        (STCon leftName leftArgs, STCon rightName rightArgs) ->
          leftName == rightName
            && length (toListNE leftArgs) == length (toListNE rightArgs)
            && and (zipWith (go leftNames rightNames) (toListNE leftArgs) (toListNE rightArgs))
        (STVarApp leftName leftArgs, STVarApp rightName rightArgs) ->
          sameTypeVar leftNames rightNames leftName rightName
            && length (toListNE leftArgs) == length (toListNE rightArgs)
            && and (zipWith (go leftNames rightNames) (toListNE leftArgs) (toListNE rightArgs))
        (STForall leftName leftMb leftBody, STForall rightName rightMb rightBody) ->
          let leftNames' = Map.insert leftName rightName leftNames
              rightNames' = Map.insert rightName leftName rightNames
           in sameBounds leftNames' rightNames' leftMb rightMb
                && go leftNames' rightNames' leftBody rightBody
        (STMu leftName leftBody, STMu rightName rightBody) ->
          go
            (Map.insert leftName rightName leftNames)
            (Map.insert rightName leftName rightNames)
            leftBody
            rightBody
        (STBottom, STBottom) -> True
        _ -> False
      where
        sameBounds _ _ Nothing Nothing = True
        sameBounds leftNames' rightNames' (Just (SrcBound leftBound)) (Just (SrcBound rightBound)) =
          go leftNames' rightNames' leftBound rightBound
        sameBounds _ _ _ _ = False

    sameTypeVar leftNames rightNames leftName rightName =
      case (Map.lookup leftName leftNames, Map.lookup rightName rightNames) of
        (Just mappedRight, Just mappedLeft) -> mappedRight == rightName && mappedLeft == leftName
        (Nothing, Nothing) -> leftName == rightName
        _ -> False

refreshLetSchemes :: Env -> ElabTerm -> Either ProgramError ElabTerm
refreshLetSchemes = go
  where
    go env term =
      case term of
        X.EVar {} -> Right term
        X.ELit {} -> Right term
        X.ELam name ty body -> do
          let env' = env {termEnv = Map.insert name ty (termEnv env)}
          X.ELam name ty <$> go env' body
        X.EApp fun arg -> X.EApp <$> go env fun <*> go env arg
        X.ELet name scheme rhs body -> do
          let schemeTy = schemeToType scheme
              rhsEnv = env {termEnv = Map.insert name schemeTy (termEnv env)}
          rhs' <- go rhsEnv rhs
          let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
              scheme' = schemeFromType rhsTy
              env' = env {termEnv = Map.insert name rhsTy (termEnv env)}
          X.ELet name scheme' rhs' <$> go env' body
        X.ETyAbs name mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = env {typeEnv = Map.insert name boundTy (typeEnv env)}
          X.ETyAbs name mbBound <$> go env' body
        X.ETyInst inner inst -> (`X.ETyInst` inst) <$> go env inner
        X.ERoll ty body -> X.ERoll ty <$> go env body
        X.EUnroll inner -> X.EUnroll <$> go env inner

resolveDeferredConstructors :: ElaborateScope -> Env -> Map String DeferredConstructorCall -> ElabTerm -> Either ProgramError ElabTerm
resolveDeferredConstructors scope env deferredConstructors = go env
  where
    go env0 term =
      case deferredPlaceholderHeadWithInsts term of
        Just (name, headInsts)
          | Just deferred <- Map.lookup name deferredConstructors,
            deferredConstructorArgCount deferred == 0 ->
              instantiateConstructorOccurrence env0 deferred headInsts [] term
        _ ->
          case term of
            X.EVar {} -> Right term
            X.ELit {} -> Right term
            X.ELam name ty body ->
              let env' = env0 {termEnv = Map.insert name ty (termEnv env0)}
               in X.ELam name ty <$> go env' body
            X.EApp {} -> rewriteApplication env0 term
            X.ELet name scheme rhs body -> do
              let schemeTy = schemeToType scheme
                  rhsEnv = env0 {termEnv = Map.insert name schemeTy (termEnv env0)}
              rhs' <- go rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  env' = env0 {termEnv = Map.insert name rhsTy (termEnv env0)}
              X.ELet name scheme rhs' <$> go env' body
            X.ETyAbs name mbBound body ->
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = env0 {typeEnv = Map.insert name boundTy (typeEnv env0)}
               in X.ETyAbs name mbBound <$> go env' body
            X.ETyInst inner inst -> (`X.ETyInst` inst) <$> go env0 inner
            X.ERoll ty body -> X.ERoll ty <$> go env0 body
            X.EUnroll inner -> X.EUnroll <$> go env0 inner

    rewriteApplication env0 term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHeadWithInsts headTerm of
            Just (name, headInsts)
              | Just deferred <- Map.lookup name deferredConstructors -> do
              args' <- mapM (go env0) args
              instantiateConstructorOccurrence env0 deferred headInsts args' term
            Nothing ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env0 fun <*> go env0 arg
                _ -> Right term
            _ ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env0 fun <*> go env0 arg
                _ -> Right term

    instantiateConstructorOccurrence env0 deferred headInsts args occurrenceTerm = do
      let ctorInfo = deferredConstructorInfo deferred
          runtimeName = ctorRuntimeName ctorInfo
          visibleArgCount = min (deferredConstructorArgCount deferred) (length (ctorArgs ctorInfo))
          visibleArgTemplates = take visibleArgCount (ctorArgs ctorInfo)
          visibleArgs = take visibleArgCount args
          instBinders = deferredConstructorInstBinders deferred
      argTypes <- mapM (inferArgSourceType env0) visibleArgs
      occurrenceTy <- inferOccurrenceSourceType env0 occurrenceTerm
      substFromHead <-
        foldM
          ( \(subst, remainingBinders) instTy ->
              case remainingBinders of
                binder : rest -> do
                  let recoveredInstTy = recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls instTy))
                  subst' <-
                    maybe
                      (Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo)))
                      Right
                      (bindConstructorSubst subst binder recoveredInstTy)
                  Right (subst', rest)
                [] -> Right (subst, [])
          )
          (deferredConstructorInitialSubst deferred, instBinders)
          headInsts
      substFromArgs <-
        Right $
          case
            foldM
              (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy)
              (fst substFromHead)
              (zip visibleArgTemplates argTypes)
          of
            Just subst -> subst
            Nothing ->
              case
                foldM
                  (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy)
                  (deferredConstructorInitialSubst deferred)
                  (zip visibleArgTemplates argTypes)
              of
                Just subst -> subst
                Nothing -> fst substFromHead
      let substFinal =
            case matchTypesInScope scope substFromArgs (deferredConstructorOccurrenceType deferred) occurrenceTy of
              Just subst -> subst
              Nothing -> substFromArgs
      case filter (`Map.notMember` substFinal) instBinders of
        [] -> do
          ctorHead <-
            if constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypes scope) ctorInfo
              then
                foldM
                  ( \headAcc varName ->
                      case Map.lookup varName substFinal of
                        Just ty -> do
                          instTy <- srcTypeToElabType (lowerType scope ty)
                          Right (X.ETyInst headAcc (X.InstApp instTy))
                        Nothing -> Right headAcc
                  )
                  (X.EVar runtimeName)
                  instBinders
              else inlineConstructorHead scope ctorInfo substFinal
          Right (foldl X.EApp ctorHead args)
        _ -> Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo))

    inferArgSourceType env0 arg =
      case typeCheckWithEnv env0 arg of
        Right ty -> Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty)))
        Left err -> Left (ProgramPipelineError ("deferred constructor argument type check failed: " ++ show err))

    inferOccurrenceSourceType env0 occurrenceTerm =
      case typeCheckWithEnv env0 occurrenceTerm of
        Right ty -> Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty)))
        Left err -> Left (ProgramPipelineError ("deferred constructor occurrence type check failed: " ++ show err))

    bindConstructorSubst subst name actual =
      case Map.lookup name subst of
        Nothing -> Just (Map.insert name actual subst)
        Just existing
          | alphaEqSrcType existing actual ->
              Just subst
          | Just existingTy <- srcTypeToElabTypeMaybe (lowerType scope existing),
            Just actualTy <- srcTypeToElabTypeMaybe (lowerType scope actual),
            alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
              Just subst
          | otherwise -> Nothing

inlineConstructorHead :: ElaborateScope -> ConstructorInfo -> Map String SrcType -> Either ProgramError ElabTerm
inlineConstructorHead scope ctorInfo subst = do
  let resultSrcTy = applyConstructorSubst subst (ctorResult ctorInfo)
      argSrcTys = map (applyConstructorSubst subst) (ctorArgs ctorInfo)
      resultVar = "$" ++ ctorOwningType ctorInfo ++ "_result"
      argNames = ["$" ++ ctorName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length argSrcTys]]
      ownerShapes =
        case lookupConstructorRuntime scope (ctorRuntimeName ctorInfo) of
          Just (dataInfo, _) -> map constructorShapeFromInfo (dataConstructors dataInfo)
          Nothing -> constructorOwnerShapes ctorInfo
      handlerShapes = map specializeHandlerShape ownerShapes
      handlerNames =
        [ "$" ++ constructorShapeName shape ++ "_k" ++ show ix
          | (ix, shape) <- zip ([1 :: Int ..]) handlerShapes
        ]
      handlerSrcType shape =
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound mbBound) acc)
          (foldr STArrow (STVar resultVar) (constructorShapeArgs shape))
          (constructorShapeForalls shape)
  selectedHandler <-
    case drop (ctorIndex ctorInfo) handlerNames of
      name : _ -> Right name
      [] -> Left (ProgramPipelineError ("constructor handler order missing `" ++ ctorName ctorInfo ++ "`"))
  resultTy <- srcTypeToElabType (lowerType scope resultSrcTy)
  argTys <- mapM (srcTypeToElabType . lowerType scope) argSrcTys
  handlerTys <- mapM (srcTypeToElabType . lowerType scope . handlerSrcType) handlerShapes
  let selectedBody = foldl X.EApp (X.EVar selectedHandler) (map X.EVar argNames)
      handlerBody =
        foldr
          (\(handlerName, handlerTy) acc -> X.ELam handlerName handlerTy acc)
          selectedBody
          (zip handlerNames handlerTys)
      rolled = X.ERoll resultTy (X.ETyAbs resultVar Nothing handlerBody)
  pure $
    foldr
      (\(argName, argTy) acc -> X.ELam argName argTy acc)
      rolled
      (zip argNames argTys)
  where
    specializeHandlerShape shape =
      case matchTypesInScope scope Map.empty (constructorShapeResult shape) (applyConstructorSubst subst (ctorResult ctorInfo)) of
        Just handlerSubst -> applyConstructorShapeSubst handlerSubst shape
        Nothing -> shape

applyConstructorShapeSubst :: Map String SrcType -> ConstructorShape -> ConstructorShape
applyConstructorShapeSubst subst shape =
  shape
    { constructorShapeForalls =
        [ (name, fmap (applyConstructorSubst subst) mbBound)
          | (name, mbBound) <- constructorShapeForalls shape,
            Map.notMember name subst
        ],
      constructorShapeArgs = map (applyConstructorSubst subst) (constructorShapeArgs shape),
      constructorShapeResult = applyConstructorSubst subst (constructorShapeResult shape)
    }

applyConstructorSubst :: Map String SrcType -> SrcType -> SrcType
applyConstructorSubst subst ty =
  Map.foldrWithKey substituteTypeVar ty subst

resolveDeferredCases :: ElaborateScope -> Map String DeferredCaseCall -> Env -> ElabTerm -> Either ProgramError (Env, ElabTerm)
resolveDeferredCases scope deferredCases = go
  where
    go env term =
      case term of
        X.EVar {} -> Right (env, term)
        X.ELit {} -> Right (env, term)
        X.ELam name ty body -> do
          let env' = env {termEnv = Map.insert name ty (termEnv env)}
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env bodyEnv, X.ELam name ty body')
        X.EApp {} -> rewriteApplication env term
        X.ELet name scheme rhs body -> do
          let schemeTy = schemeToType scheme
              rhsEnv0 = env {termEnv = Map.insert name schemeTy (termEnv env)}
          (rhsEnv, rhs') <- go rhsEnv0 rhs
          let baseBodyEnv = mergeCaseEnv env rhsEnv
              rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
              env' = baseBodyEnv {termEnv = Map.insert name rhsTy (termEnv baseBodyEnv)}
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env (mergeCaseEnv rhsEnv bodyEnv), X.ELet name scheme rhs' body')
        X.ETyAbs name mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = env {typeEnv = Map.insert name boundTy (typeEnv env)}
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env bodyEnv, X.ETyAbs name mbBound body')
        X.ETyInst inner inst -> do
          (innerEnv, inner') <- go env inner
          Right (innerEnv, X.ETyInst inner' inst)
        X.ERoll ty body -> do
          (bodyEnv, body') <- go env body
          Right (bodyEnv, X.ERoll ty body')
        X.EUnroll inner -> do
          (innerEnv, inner') <- go env inner
          Right (innerEnv, X.EUnroll inner')

    rewriteApplication env term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHead headTerm >>= (`Map.lookup` deferredCases) of
            Just deferred -> do
              (argEnv, args') <- mapAccumCaseEnv env args
              resolveDeferredCaseApplication argEnv deferred args'
            Nothing ->
              case term of
                X.EApp fun arg -> do
                  (funEnv, fun') <- go env fun
                  (argEnv, arg') <- go env arg
                  Right (mergeCaseEnv funEnv argEnv, X.EApp fun' arg')
                _ -> Right (env, term)

    resolveDeferredCaseApplication env deferred args =
      case args of
        scrutinee : handlers
          | length args == deferredCaseExpectedArgCount deferred -> do
              (_scrutineeElabTy, scrutineeRawTy, scrutineeRecoveredTy) <- inferDeferredArgType env scrutinee
              validateCaseScrutineeType
                (deferredCaseDataInfo deferred)
                (deferredCaseScrutineeType deferred)
                scrutineeRawTy
                scrutineeRecoveredTy
              resultTy <- srcTypeToElabType (lowerType scope (deferredCaseResultType deferred))
              env' <- extendCaseResultEnv (deferredCaseDataInfo deferred) scrutineeRawTy resultTy env
              let caseHead = caseEliminator resultTy scrutinee
              Right (env', foldl X.EApp caseHead handlers)
        _ -> Left (ProgramCaseOnNonDataType STBottom)

    validateCaseScrutineeType dataInfo expectedScrutineeTy scrutineeRawTy scrutineeTy
      | Just expectedTy <- srcTypeToElabTypeMaybe (lowerType scope expectedScrutineeTy),
        Just actualTy <- srcTypeToElabTypeMaybe scrutineeRawTy,
        alphaEqType actualTy expectedTy || churchAwareEqType actualTy expectedTy =
          Right ()
      | Just _ <- matchDataInfoEncoding scope dataInfo scrutineeRawTy = Right ()
      | otherwise =
          let validHeadNames = Set.fromList (dataInfoHeadNames scope dataInfo)
           in case scrutineeTy of
                STBase name
                  | name `Set.member` validHeadNames -> Right ()
                STCon name _
                  | name `Set.member` validHeadNames -> Right ()
                other -> Left (ProgramCaseOnNonDataType other)

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty ->
          let rawTy = elabTypeToSrcType (stripVacuousForalls ty)
           in Right (ty, rawTy, recoverSourceType scope rawTy)
        Left err ->
          Left (ProgramPipelineError ("deferred case scrutinee type check failed: " ++ show err))

    caseEliminator resultTy scrutinee =
      X.ETyInst (X.EUnroll scrutinee) (X.InstApp resultTy)

    extendCaseResultEnv dataInfo scrutineeRawTy resultTy env =
      case matchDataInfoEncoding scope dataInfo scrutineeRawTy of
        Just (sourceHeadTy, subst) -> do
          headTy <- srcTypeToElabType (lowerType scope sourceHeadTy)
          let resultName = "$" ++ dataName dataInfo ++ "_result"
              resultBinding =
                case Map.lookup resultName subst of
                  Just (STVar resultVar) -> Map.singleton resultVar resultTy
                  _ -> Map.empty
              selfAliasBindings =
                case scrutineeRawTy of
                  STMu actualSelf _ ->
                    Map.fromList
                      [ (alias, headTy)
                        | (alias, STVar actualSelf') <- Map.toList subst,
                          actualSelf' == actualSelf,
                          alias /= actualSelf,
                          alias /= resultName,
                          alias `notElem` dataParams dataInfo
                      ]
                  _ -> Map.empty
          Right env {typeEnv = selfAliasBindings `Map.union` resultBinding `Map.union` typeEnv env}
        Nothing -> Right env

    mapAccumCaseEnv env [] = Right (env, [])
    mapAccumCaseEnv env (arg : rest) = do
      (env1, arg') <- go env arg
      (env2, rest') <- mapAccumCaseEnv env1 rest
      Right (env2, arg' : rest')

    mergeCaseEnv base incoming =
      base {typeEnv = typeEnv incoming `Map.union` typeEnv base}

resolveDeferredMethods :: ElaborateScope -> Map String DeferredMethodCall -> Env -> ElabTerm -> Either ProgramError ElabTerm
resolveDeferredMethods scope deferredMethods = go
  where
    go env term =
      case deferredPlaceholderHeadWithInsts term of
        Just (name, headInsts)
          | Just deferred <- Map.lookup name deferredMethods,
            deferredMethodArgCount deferred == 0 ->
              resolveDeferredNullaryMethod headInsts deferred
        _ ->
          case term of
            X.EVar {} -> Right term
            X.ELit {} -> Right term
            X.ELam name ty body -> do
              let env' = env {termEnv = Map.insert name ty (termEnv env)}
              X.ELam name ty <$> go env' body
            X.EApp {} -> rewriteApplication env term
            X.ELet name scheme rhs body -> do
              let schemeTy = schemeToType scheme
                  rhsEnv = env {termEnv = Map.insert name schemeTy (termEnv env)}
              rhs' <- go rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  env' = env {termEnv = Map.insert name rhsTy (termEnv env)}
              body' <- go env' body
              Right (X.ELet name scheme rhs' body')
            X.ETyAbs name mbBound body -> do
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = env {typeEnv = Map.insert name boundTy (typeEnv env)}
              X.ETyAbs name mbBound <$> go env' body
            X.ETyInst inner inst ->
              (`X.ETyInst` inst) <$> go env inner
            X.ERoll ty body ->
              X.ERoll ty <$> go env body
            X.EUnroll inner ->
              X.EUnroll <$> go env inner

    rewriteApplication env term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHead headTerm >>= (`Map.lookup` deferredMethods) of
            Just deferred -> do
              args' <- mapM (go env) args
              resolveDeferredApplication env deferred args'
            Nothing ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env fun <*> go env arg
                _ -> Right term

    resolveDeferredApplication env deferred args = do
      let methodInfo = deferredMethodInfo deferred
          requiredArgCount = deferredMethodArgCount deferred
      if length args < requiredArgCount
        then Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
        else do
          argViews <- mapM (inferDeferredArgType env) (take requiredArgCount args)
          classArgView <-
            case inferDeferredMethodClassArgument methodInfo argViews (deferredMethodExpectedResult deferred) of
              Just view -> Right view
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          (instanceInfo, subst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
          methodValue <- concreteMethodValue instanceInfo methodInfo
          methodSubst <-
            case inferMethodArgumentSubst methodInfo classArgView subst argViews of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          let eagerConstraints =
                filter
                  constraintGround
                  (map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue))
          evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) Set.empty eagerConstraints
          methodHead <- instantiateMethodValue scope methodSubst methodValue
          Right (foldl X.EApp (foldl X.EApp methodHead evidenceArgs) args)

    resolveDeferredNullaryMethod headInsts deferred = do
      expectedView <-
        case deferredMethodExpectedResult deferred of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      let methodInfo = deferredMethodInfo deferred
      classArgView <-
        case inferNullaryMethodClassArgument methodInfo expectedView of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      case lookupNullaryEvidence deferred methodInfo classArgView of
        Just evidence -> do
          methodSubst <-
            case inferNullaryMethodSubst methodInfo classArgView Map.empty expectedView of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          evidenceHead <- instantiateLocalMethodEvidence scope methodSubst evidence
          evidenceArgs <-
            resolveConstraintEvidenceTerms
              scope
              (deferredMethodLocalEvidence deferred)
              Set.empty
              (nullaryMethodLocalConstraints methodInfo classArgView methodSubst)
          let evidenceTerm = foldl X.EApp evidenceHead evidenceArgs
          Right $
            if nullaryMethodResultIsClassParameter methodInfo
              then reapplyHeadInsts headInsts evidenceTerm
              else evidenceTerm
        Nothing -> do
          (instanceInfo, subst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
          methodValue <- concreteMethodValue instanceInfo methodInfo
          methodSubst <-
            case inferNullaryMethodSubst methodInfo classArgView subst expectedView of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          let eagerConstraints =
                filter
                  constraintGround
                  (map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue))
          evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) Set.empty eagerConstraints
          methodHead <- instantiateMethodValue scope methodSubst methodValue
          Right (reapplyHeadInsts headInsts (foldl X.EApp methodHead evidenceArgs))

    inferDeferredMethodClassArgument methodInfo argViews mbExpectedResult =
      let methodTy = lowerTypeView scope (TypeView (methodType methodInfo) (methodTypeIdentity methodInfo))
          argDisplayTypes = map typeViewDisplay argViews
       in (sourceTypeViewInScope scope <$> inferClassArgument methodTy (methodParamName methodInfo) argDisplayTypes)
            <|> inferDeferredMethodClassArgumentFromExpected methodInfo methodTy argDisplayTypes mbExpectedResult

    inferDeferredMethodClassArgumentFromExpected _ _ _ Nothing = Nothing
    inferDeferredMethodClassArgumentFromExpected methodInfo methodTy argDisplayTypes (Just expectedView) = do
      let (_, bodyTy) = splitForalls methodTy
          (paramTys, resultTy) = splitArrows bodyTy
      substFromArgs <-
        foldM
          (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy)
          Map.empty
          (zip paramTys argDisplayTypes)
      let resultTy' = Map.foldrWithKey substituteTypeVar resultTy substFromArgs
      subst <- matchTypesInScope scope substFromArgs resultTy' (typeViewDisplay expectedView)
      displayTy <- Map.lookup (methodParamName methodInfo) subst
      pure (sourceTypeViewInScope scope displayTy)

    lookupNullaryEvidence deferred methodInfo classArgView =
      case
        lookupEvidenceMethodByClass
          scope
          (methodInfoOwnerClassSymbolIdentity methodInfo)
          (typeViewIdentity classArgView)
          (methodName methodInfo)
          `orElseEvidenceMethod`
          lookupEvidenceMethod
            (deferredMethodLocalEvidence deferred)
            (methodInfoOwnerClassSymbolIdentity methodInfo)
            (typeViewIdentity classArgView)
            (methodName methodInfo)
      of
        Just (runtimeName, evidenceTy) ->
          Just
            DeferredMethodEvidence
              { deferredMethodEvidenceClassArg = classArgView,
                deferredMethodEvidenceRuntimeName = runtimeName,
                deferredMethodEvidenceType = evidenceTy
              }
        Nothing ->
          case deferredMethodEvidence deferred of
            Just evidence -> Just evidence
            _ -> Nothing

    nullaryMethodLocalConstraints methodInfo classArgView methodSubst =
      let headVars = freeTypeVarsTypeView classArgView
          specializedForClass =
            map
              (specializeConstraintInfoType (methodParamName methodInfo) classArgView)
              (methodConstraintInfos methodInfo)
          methodLocal =
            filter
              (not . constraintDeterminedByTypeVars headVars)
              specializedForClass
       in map (applyConstraintInfoSubst methodSubst) methodLocal

    specializeConstraintInfoType paramName headView constraint =
      constraint
        { constraintTypeView =
            TypeView
              { typeViewDisplay = substituteTypeVar paramName (typeViewDisplay headView) (typeViewDisplay (constraintTypeView constraint)),
                typeViewIdentity = substituteTypeVar paramName (typeViewIdentity headView) (typeViewIdentity (constraintTypeView constraint))
              }
        }

    inferNullaryMethodClassArgument methodInfo expectedView
      | deferredMethodFullArityFromInfo methodInfo /= 0 = Nothing
      | otherwise = do
          let (_, bodyTy) = splitForalls (methodType methodInfo)
              (_, resultTy) = splitArrows bodyTy
          subst <- matchTypesInScope scope Map.empty resultTy (typeViewDisplay expectedView)
          classArgTy <- Map.lookup (methodParamName methodInfo) subst
          pure (sourceTypeViewInScope scope classArgTy)

    inferNullaryMethodSubst methodInfo classArgView subst expectedView =
      let specializedMethodTy =
            specializeMethodType
              (methodType methodInfo)
              (methodParamName methodInfo)
              (typeViewDisplay classArgView)
          (_, bodyTy) = splitForalls specializedMethodTy
          (_, resultTy) = splitArrows bodyTy
       in fmap (Map.map (sourceTypeViewInScope scope)) $
            matchTypesInScope
              scope
              (fmap typeViewDisplay subst)
              resultTy
              (typeViewDisplay expectedView)

    nullaryMethodResultIsClassParameter methodInfo =
      let (_, bodyTy) = splitForalls (methodType methodInfo)
          (_, resultTy) = splitArrows bodyTy
       in resultTy == STVar (methodParamName methodInfo)

    deferredMethodFullArityFromInfo methodInfo =
      length (fst (splitArrows (snd (splitForalls (methodType methodInfo)))))

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty ->
          let displayTy = recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty))
           in Right (sourceTypeViewInScope scope displayTy)
        Left err ->
          Left (ProgramPipelineError ("deferred method argument type check failed: " ++ show err))

    concreteMethodValue instanceInfo methodInfo =
      case Map.lookup (methodName methodInfo) (instanceMethods instanceInfo) of
        Just valueInfo@OrdinaryValue {} -> Right valueInfo
        _ -> Left (ProgramUnknownMethod (methodName methodInfo))

    inferMethodArgumentSubst methodInfo classArgView subst argViews =
      let specializedMethodTy = specializeMethodType (methodType methodInfo) (methodParamName methodInfo) (typeViewDisplay classArgView)
          (_, bodyTy) = splitForalls specializedMethodTy
          (paramTys, _) = splitArrows bodyTy
       in fmap (Map.map (sourceTypeViewInScope scope)) $
            foldM
              (\acc (templateTy, actualView) -> matchTypes acc templateTy (typeViewDisplay actualView))
              (fmap typeViewDisplay subst)
              (zip paramTys argViews)

resolveConstraintEvidenceTerms :: ElaborateScope -> [EvidenceInfo] -> Set (SymbolIdentity, String) -> [ConstraintInfo] -> Either ProgramError [ElabTerm]
resolveConstraintEvidenceTerms scope localEvidence seen constraints =
  concat <$> mapM (resolveConstraintEvidenceTerm scope localEvidence seen) constraints

resolveConstraintEvidenceTerm :: ElaborateScope -> [EvidenceInfo] -> Set (SymbolIdentity, String) -> ConstraintInfo -> Either ProgramError [ElabTerm]
resolveConstraintEvidenceTerm scope localEvidence seen constraint = do
  let key = (constraintClassSymbol constraint, show (typeViewIdentity (constraintTypeView constraint)))
  if key `Set.member` seen
    then Left (ProgramNoMatchingInstance (constraintDisplayClass constraint) (typeViewDisplay (constraintTypeView constraint)))
    else do
      mbLocalEvidence <- resolveLocalConstraintEvidenceTerms scope localEvidence constraint
      case mbLocalEvidence of
        Just evidenceTerms -> Right evidenceTerms
        Nothing -> do
          (instanceInfo, subst) <- resolveInstanceInfoByConstraint scope constraint
          let seen' = Set.insert key seen
              methodValues = ordinaryInstanceMethods instanceInfo
          if null methodValues
            then do
              _ <-
                resolveConstraintEvidenceTerms
                  scope
                  localEvidence
                  seen'
                  (map (applyConstraintInfoSubst subst) (instanceConstraintInfos instanceInfo))
              Right []
            else mapM (materializeMethodEvidence (freeTypeVarsTypeView (constraintTypeView constraint)) seen' subst) methodValues
  where
    ordinaryInstanceMethods instanceInfo =
      [valueInfo | valueInfo@OrdinaryValue {} <- Map.elems (instanceMethods instanceInfo)]

    materializeMethodEvidence headVars seen' subst valueInfo = do
      let eagerConstraints =
            filter
              (constraintDeterminedByTypeVars headVars)
              (map (applyConstraintInfoSubst subst) (methodValueConstraints valueInfo))
      nestedEvidence <-
        resolveConstraintEvidenceTerms
          scope
          localEvidence
          seen'
          eagerConstraints
      methodHead <- instantiateMethodValue scope subst valueInfo
      pure (foldl X.EApp methodHead nestedEvidence)

resolveLocalConstraintEvidenceTerms :: ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Either ProgramError (Maybe [ElabTerm])
resolveLocalConstraintEvidenceTerms scope localEvidence constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> Right Nothing
    Just classInfo
      | Map.null (classMethods classInfo) ->
          Right $
            if zeroMethodConstraintCoveredByEvidenceInfo scope constraint
              || zeroMethodConstraintCoveredByEvidence localEvidence constraint
              then Just []
              else Nothing
      | otherwise -> do
          let localMethodEvidence =
                mapM
                  ( \methodInfo -> do
                      (runtimeName, evidenceTy) <-
                        lookupEvidenceMethodByClass
                          scope
                          (constraintClassSymbol constraint)
                          (typeViewIdentity (constraintTypeView constraint))
                          (methodName methodInfo)
                          `orElseEvidenceMethod`
                          lookupEvidenceMethod
                            localEvidence
                            (constraintClassSymbol constraint)
                            (typeViewIdentity (constraintTypeView constraint))
                            (methodName methodInfo)
                      pure
                        DeferredMethodEvidence
                          { deferredMethodEvidenceClassArg = constraintTypeView constraint,
                            deferredMethodEvidenceRuntimeName = runtimeName,
                            deferredMethodEvidenceType = evidenceTy
                          }
                  )
                  (Map.elems (classMethods classInfo))
          case localMethodEvidence of
            Nothing -> Right Nothing
            Just evidence ->
              Just <$> mapM (instantiateLocalMethodEvidence scope Map.empty) evidence

lookupEvidenceMethod :: [EvidenceInfo] -> SymbolIdentity -> SrcType -> String -> Maybe (String, SrcType)
lookupEvidenceMethod evidenceInfos classIdentity headIdentityTy methodName0 =
  case
    [ methodEvidence
      | evidence <- evidenceInfos,
        evidenceClassSymbol evidence == classIdentity,
        evidenceTypeIdentity evidence == headIdentityTy,
        Just methodEvidence <- [Map.lookup methodName0 (evidenceMethods evidence)]
    ]
  of
    methodEvidence : _ -> Just methodEvidence
    [] -> Nothing

orElseEvidenceMethod :: Maybe (String, SrcType) -> Maybe (String, SrcType) -> Maybe (String, SrcType)
orElseEvidenceMethod (Just evidence) _ = Just evidence
orElseEvidenceMethod Nothing fallback = fallback

zeroMethodConstraintCoveredByEvidence :: [EvidenceInfo] -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidence evidenceInfos constraint =
  any
    ( \evidence ->
        evidenceClassSymbol evidence == constraintClassSymbol constraint
          && evidenceTypeIdentity evidence == typeViewIdentity (constraintTypeView constraint)
    )
    evidenceInfos

instantiateLocalMethodEvidence :: ElaborateScope -> Map String TypeView -> DeferredMethodEvidence -> Either ProgramError ElabTerm
instantiateLocalMethodEvidence scope subst DeferredMethodEvidence {deferredMethodEvidenceRuntimeName = runtimeName, deferredMethodEvidenceType = evidenceTy} =
  foldl X.ETyInst (X.EVar runtimeName)
    <$> methodForallInstantiations scope subst (fst (splitForalls evidenceTy))

constraintDeterminedByTypeVars :: Set String -> ConstraintInfo -> Bool
constraintDeterminedByTypeVars typeVars constraint =
  freeTypeVarsTypeView (constraintTypeView constraint) `Set.isSubsetOf` typeVars

constraintGround :: ConstraintInfo -> Bool
constraintGround constraint =
  Set.null (freeTypeVarsTypeView (constraintTypeView constraint))

methodValueConstraints :: ValueInfo -> [ConstraintInfo]
methodValueConstraints OrdinaryValue {valueConstraintInfos = constraints} = constraints
methodValueConstraints _ = []

instantiateMethodValue :: ElaborateScope -> Map String TypeView -> ValueInfo -> Either ProgramError ElabTerm
instantiateMethodValue scope subst OrdinaryValue {valueRuntimeName = runtimeName, valueType = visibleTy} =
  foldl X.ETyInst (X.EVar runtimeName)
    <$> methodForallInstantiations scope subst (fst (splitForalls visibleTy))
instantiateMethodValue _ _ ConstructorValue {valueRuntimeName = runtimeName} =
  Right (X.EVar runtimeName)
instantiateMethodValue _ _ OverloadedMethod {} =
  Right (X.EVar "<overloaded-method>")

methodForallInstantiations :: ElaborateScope -> Map String TypeView -> [(String, Maybe SrcType)] -> Either ProgramError [X.Instantiation]
methodForallInstantiations scope subst = go
  where
    go [] = Right []
    go ((name, _) : rest) =
      case Map.lookup name subst of
        Just ty -> do
          instTy <- srcTypeToElabType (lowerTypeView scope ty)
          (X.InstApp instTy :) <$> go rest
        Nothing
          | any ((`Map.member` subst) . fst) rest -> (X.InstElim :) <$> go rest
          | otherwise -> Right []

collectElabApps :: ElabTerm -> (ElabTerm, [ElabTerm])
collectElabApps = go []
  where
    go args term =
      case term of
        X.EApp fun arg -> go (arg : args) fun
        _ -> (term, args)

deferredPlaceholderHead :: ElabTerm -> Maybe String
deferredPlaceholderHead term =
  case term of
    X.EVar name -> Just name
    X.ETyInst inner _ -> deferredPlaceholderHead inner
    _ -> Nothing

deferredPlaceholderHeadWithInsts :: ElabTerm -> Maybe (String, [ElabType])
deferredPlaceholderHeadWithInsts = go []
  where
    go insts term =
      case term of
        X.EVar name -> Just (name, insts)
        X.ETyInst inner (X.InstApp ty) -> go (ty : insts) inner
        X.ETyInst inner _ -> go insts inner
        _ -> Nothing

reapplyHeadInsts :: [ElabType] -> ElabTerm -> ElabTerm
reapplyHeadInsts insts term =
  foldl X.ETyInst term (map X.InstApp insts)

dataInfoHeadNames :: ElaborateScope -> DataInfo -> [String]
dataInfoHeadNames scope info =
  visibleNames ++ [dataName info | dataName info `notElem` visibleNames]
  where
    visibleNames =
      [ name
        | (name, candidate) <- Map.toList (elaborateScopeDataTypes scope),
          sameDataIdentity candidate info
      ]

    sameDataIdentity left right =
      dataInfoSymbol left == dataInfoSymbol right

{- Note [recoverSourceType]

When the eMLF pipeline infers a type, it returns raw Church-encoded μ forms
with fresh binder names.  The .mlfp layer still needs named source ADT heads
for diagnostics and instance-head comparisons.  This recovery is deliberately
downstream of lowering: `Program.Elaborate` never invokes the pipeline.
-}
recoverSourceType :: ElaborateScope -> SrcType -> SrcType
recoverSourceType scope = recover
  where
    dataInfos = Map.elems (elaborateScopeDataTypes scope)

    recover ty =
      case lookupHead ty of
        Just headTy -> headTy
        Nothing -> recoverChildren ty

    lookupHead ty =
      case mapMaybeDataHead ty dataInfos of
        (headTy : _) -> Just headTy
        [] -> Nothing

    mapMaybeDataHead ty =
      foldr
        ( \info acc ->
            case recoverDataHead ty info of
              Just headTy -> headTy : acc
              Nothing -> acc
        )
        []

    recoverDataHead ty info =
      fst <$> matchDataInfoEncodingWith recover scope info ty

    recoverChildren ty = case ty of
      STVar {} -> ty
      STBase {} -> ty
      STBottom -> ty
      STArrow dom cod -> STArrow (recover dom) (recover cod)
      STForall name mb body ->
        STForall name (fmap (SrcBound . recover . unSrcBound) mb) (recover body)
      STMu name body -> STMu name (recover body)
      STCon name args -> STCon name (fmap recover args)
      STVarApp name args -> STVarApp name (fmap recover args)

matchDataInfoEncoding :: ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncoding = matchDataInfoEncodingWith id

matchDataInfoEncodingWith :: (SrcType -> SrcType) -> ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncodingWith recover scope info ty =
  firstMatch (dataInfoHeadNames scope info)
  where
    params = dataParams info

    firstMatch [] = Nothing
    firstMatch (headName : rest) =
      case matchHeadName headName of
        Just matched -> Just matched
        Nothing -> firstMatch rest

    matchHeadName headName =
      let templateHead =
            case params of
              [] -> STBase headName
              p : ps -> STCon headName (STVar p :| map STVar ps)
          loweredTemplate = lowerType scope templateHead
          matchTemplate template =
            matchRecoverType (Set.fromList params) Map.empty Map.empty template ty
          matched =
            case matchTemplate loweredTemplate of
              Just subst -> Just subst
              Nothing ->
                case loweredTemplate of
                  STMu _ body -> matchTemplate body
                  _ -> Nothing
       in case matched of
            Just subst ->
              let recoveredArgs = map (\param -> recover (Map.findWithDefault (STVar param) param subst)) params
                  recoveredHead =
                    case recoveredArgs of
                      [] -> STBase headName
                      arg : args -> STCon headName (arg :| args)
               in Just (recoveredHead, subst)
            Nothing -> Nothing

matchRecoverType ::
  Set String ->
  Map String SrcType ->
  Map String String ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchRecoverType params subst renames template actual =
  case template of
    STVar name
      | name `Set.member` params ->
          bindRecoverParam name actual subst
      | Just actualName <- Map.lookup name renames ->
          case actual of
            STVar name' | name' == actualName -> Just subst
            _ -> Nothing
      | otherwise ->
          case actual of
            STVar name' | name' == name -> Just subst
            _ -> Nothing
    STArrow dom cod ->
      case actual of
        STArrow dom' cod' -> do
          subst' <- matchRecoverType params subst renames dom dom'
          matchRecoverType params subst' renames cod cod'
        _ -> Nothing
    STBase name ->
      case actual of
        STBase name' | name == name' -> Just subst
        _ -> Nothing
    STCon name args ->
      case actual of
        STCon name' args'
          | name == name' && length (toListNE args) == length (toListNE args') ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType params acc renames leftTy rightTy)
                subst
                (zip (toListNE args) (toListNE args'))
        _ -> Nothing
    STVarApp name args ->
      matchRecoverVarApp params subst renames name args actual
    STForall name _mb body ->
      case actual of
        STForall name' _mb' body' ->
          matchRecoverType params subst (Map.insert name name' renames) body body'
        _ ->
          matchRecoverType (Set.insert name params) subst renames body actual
    STMu name body ->
      case actual of
        STMu name' body' ->
          matchRecoverType params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STBottom ->
      case actual of
        STBottom -> Just subst
        _ -> Nothing

matchRecoverVarApp ::
  Set String ->
  Map String SrcType ->
  Map String String ->
  String ->
  NonEmpty SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchRecoverVarApp params subst renames name args actual
  | name `Set.member` params =
      case actual of
        STCon actualName actualArgs ->
          matchAppliedHead actualName toConHead (toListNE actualArgs)
        STVarApp actualName actualArgs ->
          matchAppliedHead actualName toVarHead (toListNE actualArgs)
        _ -> Nothing
  | Just actualName <- Map.lookup name renames =
      matchRigidVarAppHead actualName
  | otherwise =
      matchRigidVarAppHead name
  where
    expectedArgs = toListNE args
    expectedArgCount = length expectedArgs

    matchAppliedHead actualName headFromPrefix actualArgs
      | length actualArgs < expectedArgCount = Nothing
      | otherwise = do
          let (headArgs, appliedArgs) = splitAt (length actualArgs - expectedArgCount) actualArgs
          subst' <- bindRecoverParam name (headFromPrefix actualName headArgs) subst
          foldM
            (\acc (leftTy, rightTy) -> matchRecoverType params acc renames leftTy rightTy)
            subst'
            (zip expectedArgs appliedArgs)

    matchRigidVarAppHead expectedName =
      case actual of
        STVarApp actualName actualArgs
          | expectedName == actualName && expectedArgCount == length (toListNE actualArgs) ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType params acc renames leftTy rightTy)
                subst
                (zip expectedArgs (toListNE actualArgs))
        _ -> Nothing

    toConHead actualName [] = STBase actualName
    toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

    toVarHead actualName [] = STVar actualName
    toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

bindRecoverParam :: String -> SrcType -> Map String SrcType -> Maybe (Map String SrcType)
bindRecoverParam name actual subst =
  case Map.lookup name subst of
    Nothing -> Just (Map.insert name actual subst)
    Just existing
      | alphaEqSrcType existing actual ->
          Just subst
      | Just existingTy <- srcTypeToElabTypeMaybe existing,
        Just actualTy <- srcTypeToElabTypeMaybe actual,
        alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
          Just subst
      | otherwise -> Nothing

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

stripVacuousForallsAndTypeAbs :: ElabType -> ElabTerm -> (ElabType, ElabTerm)
stripVacuousForallsAndTypeAbs ty term =
  case (ty, term) of
    (X.TForall v _ bodyTy, X.ETyAbs termV _ body)
      | v `Set.notMember` freeTypeVarsType bodyTy,
        termV `Set.notMember` freeTypeVarsTerm body ->
          stripVacuousForallsAndTypeAbs bodyTy body
    (X.TForall typeV mbTy bodyTy, X.ETyAbs termV mbTerm body) ->
      let (bodyTy', body') = stripVacuousForallsAndTypeAbs bodyTy body
       in (X.TForall typeV mbTy bodyTy', X.ETyAbs termV mbTerm body')
    _ -> (ty, term)

freeTypeVarsTerm :: ElabTerm -> Set String
freeTypeVarsTerm term =
  case term of
    X.EVar {} -> Set.empty
    X.ELit {} -> Set.empty
    X.ELam _ ty body ->
      freeTypeVarsType ty `Set.union` freeTypeVarsTerm body
    X.EApp fun arg ->
      freeTypeVarsTerm fun `Set.union` freeTypeVarsTerm arg
    X.ELet _ scheme rhs body ->
      Set.unions
        [ freeTypeVarsType (schemeToType scheme),
          freeTypeVarsTerm rhs,
          freeTypeVarsTerm body
        ]
    X.ETyAbs v mb body ->
      let boundFv = maybe Set.empty freeTypeVarsType mb
          bodyFv = Set.delete v (freeTypeVarsTerm body)
       in boundFv `Set.union` bodyFv
    X.ETyInst inner inst ->
      freeTypeVarsTerm inner `Set.union` freeTypeVarsInstantiation inst
    X.ERoll ty body ->
      freeTypeVarsType ty `Set.union` freeTypeVarsTerm body
    X.EUnroll body ->
      freeTypeVarsTerm body

freeTypeVarsInstantiation :: X.Instantiation -> Set String
freeTypeVarsInstantiation inst =
  case inst of
    X.InstId -> Set.empty
    X.InstApp ty -> freeTypeVarsType ty
    X.InstBot ty -> freeTypeVarsType ty
    X.InstIntro -> Set.empty
    X.InstElim -> Set.empty
    X.InstAbstr v -> Set.singleton v
    X.InstUnder v inner -> Set.delete v (freeTypeVarsInstantiation inner)
    X.InstInside inner -> freeTypeVarsInstantiation inner
    X.InstSeq left right ->
      freeTypeVarsInstantiation left `Set.union` freeTypeVarsInstantiation right

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

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

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

srcTypeToElabType :: SrcTy n v -> Either ProgramError ElabType
srcTypeToElabType ty = case ty of
  STVar name -> Right (X.TVar name)
  STArrow dom cod -> X.TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod
  STBase name -> Right (X.TBase (Graph.BaseTy name))
  STCon name args -> X.TCon (Graph.BaseTy name) <$> traverse srcTypeToElabType args
  STVarApp name _ ->
    Left (unsupportedVariableHeadType name)
  STForall name mb body ->
    X.TForall name
      <$> maybe (Right Nothing) srcBoundToElabBound mb
      <*> srcTypeToElabType body
  STMu name body -> X.TMu name <$> srcTypeToElabType body
  STBottom -> Right X.TBottom

unsupportedVariableHeadType :: String -> ProgramError
unsupportedVariableHeadType name =
  ProgramPipelineError
    ("variable-headed source type application `" ++ name ++ "` is not supported before higher-kinded elaboration")

srcTypeToElabTypeMaybe :: SrcTy n v -> Maybe ElabType
srcTypeToElabTypeMaybe ty = case ty of
  STVar name -> Just (X.TVar name)
  STArrow dom cod -> X.TArrow <$> srcTypeToElabTypeMaybe dom <*> srcTypeToElabTypeMaybe cod
  STBase name -> Just (X.TBase (Graph.BaseTy name))
  STCon name args -> X.TCon (Graph.BaseTy name) <$> traverse srcTypeToElabTypeMaybe args
  STVarApp {} -> Nothing
  STForall name mb body ->
    X.TForall name
      <$> maybe (Just Nothing) srcBoundToElabBoundMaybe mb
      <*> srcTypeToElabTypeMaybe body
  STMu name body -> X.TMu name <$> srcTypeToElabTypeMaybe body
  STBottom -> Just X.TBottom

srcBoundToElabBound :: SrcBound n -> Either ProgramError (Maybe X.BoundType)
srcBoundToElabBound (SrcBound boundTy) =
  case srcTypeToElabType boundTy of
    Left err -> Left err
    Right (X.TVar {}) -> Right Nothing
    Right X.TBottom -> Right Nothing
    Right (X.TArrow dom cod) -> Right (Just (X.TArrow dom cod))
    Right (X.TBase base) -> Right (Just (X.TBase base))
    Right (X.TCon con args) -> Right (Just (X.TCon con args))
    Right (X.TForall name mb body) -> Right (Just (X.TForall name mb body))
    Right (X.TMu name body) -> Right (Just (X.TMu name body))

srcBoundToElabBoundMaybe :: SrcBound n -> Maybe (Maybe X.BoundType)
srcBoundToElabBoundMaybe (SrcBound boundTy) =
  case srcTypeToElabTypeMaybe boundTy of
    Just (X.TVar {}) -> Just Nothing
    Just X.TBottom -> Just Nothing
    Just (X.TArrow dom cod) -> Just (Just (X.TArrow dom cod))
    Just (X.TBase base) -> Just (Just (X.TBase base))
    Just (X.TCon con args) -> Just (Just (X.TCon con args))
    Just (X.TForall name mb body) -> Just (Just (X.TForall name mb body))
    Just (X.TMu name body) -> Just (Just (X.TMu name body))
    Nothing -> Nothing
