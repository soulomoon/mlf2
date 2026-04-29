{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeInstances,
    elaborateScopeRuntimeTypes,
    mkElaborateScope,
    lowerTypeView,
    lowerConstructorBinding,
    lowerConstrainedExprBinding,
    lowerConstrainedResolvedExprBinding,
    lowerResolvedConstrainedExprBinding,
    lowerExprBinding,
    inferClassArgument,
    lowerType,
    sourceTypeViewInScope,
    matchTypes,
    matchTypesInScope,
    freeTypeVarsSrcType,
    resolveInstanceInfo,
    resolveInstanceInfoWithSubst,
    resolveInstanceInfoByIdentityWithSubst,
    resolveInstanceInfoWithIdentityType,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoWithSubst,
    resolveMethodInstanceInfoByTypeView,
    lookupEvidenceMethodByClass,
  )
where

import Control.Monad ((>=>), foldM, replicateM, when, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.List (find, partition, sort)
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
    ResolvedSrcBound (..),
    ResolvedSrcTy (..),
    ResolvedSrcType,
    SrcBound (..),
    SrcTy (..),
    SrcType,
    SurfaceExpr,
    resolvedSrcTypeIdentityType,
  )
import MLF.Frontend.Syntax.Program (ExprF (..))
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Reify.TypeOps (freshNameLike)

data ElaborateScope = ElaborateScope
  { esValues :: Map String ValueInfo,
    esValuesByIdentity :: Map SymbolIdentity [(String, ValueInfo)],
    esRuntimeTypes :: Map String SrcType,
    esTypes :: Map String DataInfo,
    esTypesByIdentity :: Map SymbolIdentity [(String, DataInfo)],
    esClasses :: Map String ClassInfo,
    esClassesByIdentity :: Map SymbolIdentity [(String, ClassInfo)],
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

type ClassIdentity = SymbolIdentity

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
  let values1 = values0 `Map.union` instanceRuntimeValues
   in ElaborateScope
        { esValues = values1,
          esValuesByIdentity = indexByIdentity valueInfoSymbolIdentity values1,
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
          esTypesByIdentity = indexByIdentity dataInfoSymbolIdentity dataTypes,
          esClasses = classes0,
          esClassesByIdentity = indexByIdentity classInfoSymbolIdentity classes0,
          esEvidence = [],
          esInstances = instances0
        }
  where
    shouldTrackRuntimeType ConstructorValue {valueCtorInfo = ctorInfo} =
      constructorOwnerRuntimeTypeTrackable dataTypes ctorInfo
    shouldTrackRuntimeType OverloadedMethod {} = False
    shouldTrackRuntimeType _ = True

    runtimeNameFor OrdinaryValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor ConstructorValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor OverloadedMethod {} = error "overloaded methods do not have runtime names"

    valueTypeFor OrdinaryValue {valueType = ty, valueIdentityType = identityTy, valueConstraints = constraints} =
      lowerTypeViewRaw dataTypes (TypeView (constrainedRuntimeTypeRaw dataTypes classes0 constraints ty) identityTy)
    valueTypeFor ConstructorValue {valueType = ty, valueCtorInfo = ctorInfo} =
      let quantifiedTy = quantifyFreeTypeVars ty
          loweredTy = lowerTypeRaw dataTypes quantifiedTy
       in if constructorOwnerHasVariableHeadApplication dataTypes ctorInfo
            && srcTypeHasVariableHeadApplication loweredTy
            then constructorStructuralPlaceholderTypeFor dataTypes ctorInfo
            else quantifiedTy
    valueTypeFor OverloadedMethod {} = error "overloaded methods do not have concrete runtime types"

    instanceRuntimeValues =
      Map.fromList
        [ (runtimeName, methodValue)
          | instanceInfo <- instances0,
            methodValue@OrdinaryValue {valueRuntimeName = runtimeName} <- Map.elems (instanceMethods instanceInfo)
        ]

indexByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity [(String, a)]
indexByIdentity identityOf =
  Map.fromListWith (++) . map (\(name, info) -> (identityOf info, [(name, info)])) . Map.toList

elaborateScopeRuntimeTypes :: ElaborateScope -> Map String SrcType
elaborateScopeRuntimeTypes = esRuntimeTypes

elaborateScopeDataTypes :: ElaborateScope -> Map String DataInfo
elaborateScopeDataTypes = esTypes

elaborateScopeInstances :: ElaborateScope -> [InstanceInfo]
elaborateScopeInstances = esInstances

lowerType :: ElaborateScope -> SrcType -> SrcType
lowerType scope = lowerTypeRaw (esTypes scope)

lowerTypeView :: ElaborateScope -> TypeView -> SrcType
lowerTypeView scope = lowerTypeViewRaw (esTypes scope)

lowerTypeViewRaw :: Map String DataInfo -> TypeView -> SrcType
lowerTypeViewRaw dataTypes view =
  lowerTypeRaw dataTypes (visibleTypeForIdentity dataTypes (typeViewDisplay view) (typeViewIdentity view))

visibleTypeForIdentity :: Map String DataInfo -> SrcType -> SrcType -> SrcType
visibleTypeForIdentity dataTypes = go
  where
    go display identityTy =
      case (display, identityTy) of
        (STBase displayName, STBase identityName) -> STBase (visibleHeadName identityName displayName)
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          STCon
            (visibleHeadName identityName displayName)
            (zipWithNE go displayArgs identityArgs)
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs)
          | displayName == identityName ->
              STVarApp displayName (zipWithNE go displayArgs identityArgs)
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          STArrow (go displayDom identityDom) (go displayCod identityCod)
        (STForall name displayBound displayBody, STForall _ identityBound identityBody) ->
          STForall
            name
            (zipBound displayBound identityBound)
            (go displayBody identityBody)
        (STMu name displayBody, STMu _ identityBody) -> STMu name (go displayBody identityBody)
        _ -> display

    visibleHeadName identityName displayName =
      case
        [ visibleName
          | (visibleName, info) <- Map.toList dataTypes,
            dataIdentityTypeName info == identityName
        ]
      of
        visibleName : _ -> visibleName
        [] -> displayName

    zipBound (Just (SrcBound displayBound)) (Just (SrcBound identityBound)) =
      Just (SrcBound (go displayBound identityBound))
    zipBound displayBound _ = displayBound

    zipWithNE f (displayHead :| displayTail) (identityHead :| identityTail) =
      f displayHead identityHead :| zipWith f displayTail identityTail

sourceTypeViewInScope :: ElaborateScope -> SrcType -> TypeView
sourceTypeViewInScope scope ty =
  TypeView
    { typeViewDisplay = preferVisibleSourceType scope ty,
      typeViewIdentity = sourceTypeIdentityInScope scope ty
    }

canonicalSourceType :: ElaborateScope -> SrcType -> SrcType
canonicalSourceType = sourceTypeIdentityInScope

sourceTypeIdentityInScope :: ElaborateScope -> SrcType -> SrcType
sourceTypeIdentityInScope scope = canonical
  where
    canonical ty =
      case ty of
        STVar {} -> ty
        STBase name ->
          case Map.lookup name (esTypes scope) of
            Just info -> STBase (qualifiedDataName info)
            Nothing
              | name `Set.member` builtinTypeNames -> STBase ("<builtin>." ++ name)
              | otherwise -> ty
        STCon name args ->
          let args' = fmap canonical args
           in case Map.lookup name (esTypes scope) of
                Just info -> STCon (qualifiedDataName info) args'
                Nothing
                  | name `Set.member` builtinTypeNames -> STCon ("<builtin>." ++ name) args'
                  | otherwise -> STCon name args'
        STVarApp name args -> STVarApp name (fmap canonical args)
        STArrow dom cod -> STArrow (canonical dom) (canonical cod)
        STForall name mb body ->
          STForall name (fmap (SrcBound . canonical . unSrcBound) mb) (canonical body)
        STMu name body -> STMu name (canonical body)
        STBottom -> STBottom

    qualifiedDataName info =
      let identity = dataInfoSymbolIdentity info
       in symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

builtinTypeNames :: Set String
builtinTypeNames = Set.fromList ["Bool", "Int", "String"]

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
          [ lowerTypeRaw dataTypes (methodEvidenceSourceTypeRaw dataTypes classes0 classInfo (P.constraintType constraint) methodInfo)
            | methodInfo <- Map.elems (classMethods classInfo)
          ]

methodEvidenceSourceType :: ElaborateScope -> ClassInfo -> SrcType -> MethodInfo -> SrcType
methodEvidenceSourceType scope =
  methodEvidenceSourceTypeRaw (esTypes scope) (esClasses scope)

methodEvidenceSourceTypeRaw :: Map String DataInfo -> Map String ClassInfo -> ClassInfo -> SrcType -> MethodInfo -> SrcType
methodEvidenceSourceTypeRaw dataTypes classes0 classInfo classArgTy methodInfo =
  let specializedMethodTy =
        specializeMethodType (methodType methodInfo) (classParamName classInfo) classArgTy
      specializedConstraints =
        map
          (specializeConstraintType (classParamName classInfo) classArgTy)
          (methodConstraints methodInfo)
      headVars = freeTypeVarsSrcType classArgTy
      deferredConstraints =
        filter (not . constraintDeterminedByTypeVars headVars) specializedConstraints
      evidenceVisibleTy =
        quantifyMethodLocalVars headVars specializedConstraints specializedMethodTy
   in constrainedRuntimeTypeRaw dataTypes classes0 deferredConstraints evidenceVisibleTy

specializeConstraintType :: String -> SrcType -> P.ClassConstraint -> P.ClassConstraint
specializeConstraintType paramName headTy constraint =
  constraint {P.constraintType = substituteTypeVar paramName headTy (P.constraintType constraint)}

specializeConstraintInfoType :: String -> TypeView -> ConstraintInfo -> ConstraintInfo
specializeConstraintInfoType paramName headView constraint =
  constraint
    { constraintTypeView =
        TypeView
          { typeViewDisplay = substituteTypeVar paramName (typeViewDisplay headView) (typeViewDisplay (constraintTypeView constraint)),
            typeViewIdentity = substituteTypeVar paramName (typeViewIdentity headView) (typeViewIdentity (constraintTypeView constraint))
          }
    }

constraintDeterminedByTypeVars :: Set String -> P.ClassConstraint -> Bool
constraintDeterminedByTypeVars typeVars constraint =
  freeTypeVarsSrcType (P.constraintType constraint) `Set.isSubsetOf` typeVars

constraintInfoDeterminedByTypeVars :: Set String -> ConstraintInfo -> Bool
constraintInfoDeterminedByTypeVars typeVars constraint =
  freeTypeVarsTypeView (constraintTypeView constraint) `Set.isSubsetOf` typeVars

quantifyMethodLocalVars :: Set String -> [P.ClassConstraint] -> SrcType -> SrcType
quantifyMethodLocalVars headVars constraints ty =
  let (foralls, _) = splitForalls ty
      alreadyQuantified = Set.fromList (map fst foralls)
      constraintVars = foldMap (freeTypeVarsSrcType . P.constraintType) constraints
      localVars =
        (freeTypeVarsSrcType ty `Set.union` constraintVars)
          Set.\\ headVars
          Set.\\ alreadyQuantified
   in foldr (\name acc -> STForall name Nothing acc) ty (sort (Set.toList localVars))

lowerTypeRaw :: Map String DataInfo -> SrcType -> SrcType
lowerTypeRaw dataTypes = lower Map.empty Nothing
  where
    lower subst currentData = lowerWith Set.empty subst currentData

    lowerWith seen subst currentData ty = case ty of
      STVar name ->
        case Map.lookup name subst of
          Just replacement
            | replacement /= ty && not (substitutionCycle name seen replacement) ->
                lowerWith (Set.insert name seen) subst currentData replacement
          _ -> ty
      STArrow dom cod -> STArrow (lowerWith seen subst currentData dom) (lowerWith seen subst currentData cod)
      STBase name ->
        case Map.lookup name dataTypes of
          Just info -> encodeDataType subst info []
          Nothing -> STBase name
      STCon name args ->
        case Map.lookup name dataTypes of
          Just info -> encodeDataType subst info (actualArgsForData (lowerWith seen subst currentData) info (toListNE args))
          Nothing -> STCon name (fmap (lowerWith seen subst currentData) args)
      STVarApp name args ->
        let args' = fmap (lowerWith seen subst currentData) args
         in lowerAppliedTypeHead (\seen' -> lowerWith seen' subst currentData) subst seen name args'
      STForall name mb body ->
        let subst' = Map.delete name subst
            seen' = Set.delete name seen
         in STForall name (fmap (SrcBound . lowerWith seen' subst' currentData . unSrcBound) mb) (lowerWith seen' subst' currentData body)
      STMu name body -> STMu name (lowerWith (Set.delete name seen) (Map.delete name subst) currentData body)
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
            ( \(name, mbBound) acc ->
                STForall name (fmap (SrcBound . lowerCtorArg subst ownerIdentity selfTy) mbBound) acc
            )
            (foldr STArrow resultTy (map (lowerCtorArg subst ownerIdentity selfTy) (ctorArgs ctor)))
            (ctorForalls ctor)
          | ctor <- dataConstructors info
          , let ownerIdentity = Just (dataIdentity info)
        ]

    lowerCtorArg subst currentData selfTy = lowerCtorArgWith Set.empty subst currentData selfTy

    lowerCtorArgWith seen subst currentData selfTy ty = case ty of
      STVar name ->
        case Map.lookup name subst of
          Just replacement
            | replacement /= ty && not (substitutionCycle name seen replacement) ->
                lowerCtorArgWith (Set.insert name seen) subst currentData selfTy replacement
          _ -> ty
      STArrow dom cod -> STArrow (lowerCtorArgWith seen subst currentData selfTy dom) (lowerCtorArgWith seen subst currentData selfTy cod)
      STBase name
        | isCurrentDataAlias currentData name -> selfTy
        | otherwise ->
            case Map.lookup name dataTypes of
              Just info -> encodeDataType subst info []
              Nothing -> STBase name
      STCon name args
        | isCurrentDataAlias currentData name -> selfTy
        | otherwise ->
            case Map.lookup name dataTypes of
              Just info -> encodeDataType subst info (actualArgsForData (lowerCtorArgWith seen subst currentData selfTy) info (toListNE args))
              Nothing -> STCon name (fmap (lowerCtorArgWith seen subst currentData selfTy) args)
      STVarApp name args ->
        let args' = fmap (lowerCtorArgWith seen subst currentData selfTy) args
         in lowerAppliedTypeHead (\seen' -> lowerCtorArgWith seen' subst currentData selfTy) subst seen name args'
      STForall name mb body ->
        let subst' = Map.delete name subst
            seen' = Set.delete name seen
         in STForall name (fmap (SrcBound . lowerCtorArgWith seen' subst' currentData selfTy . unSrcBound) mb) (lowerCtorArgWith seen' subst' currentData selfTy body)
      STMu name body -> STMu name (lowerCtorArgWith (Set.delete name seen) (Map.delete name subst) currentData selfTy body)
      STBottom -> STBottom

    isCurrentDataAlias currentData name =
      case currentData of
        Nothing -> False
        Just ownerIdentity ->
          case Map.lookup name dataTypes of
            Just info -> dataIdentity info == ownerIdentity
            Nothing -> name == ownerIdentity

    dataIdentity info =
      let identity = dataInfoSymbolIdentity info
       in symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

    actualArgsForData lowerArg info =
      zipWith
        ( \param arg ->
            if P.typeParamIsFirstOrder param
              then lowerArg arg
              else arg
        )
        (dataTypeParams info)

    substitutionCycle name seen replacement =
      name `Set.member` seen
        || maybe False (\replacementName -> replacementName == name || replacementName `Set.member` seen) (variableHeadName replacement)

    variableHeadName ty =
      case ty of
        STVar replacementName -> Just replacementName
        STVarApp replacementName _ -> Just replacementName
        _ -> Nothing

    lowerAppliedTypeHead continue subst seen name args =
      case Map.lookup name subst >>= \replacement -> applyTypeHead replacement (toListNE args) of
        Just replacementTy
          | replacementTy == STVarApp name args -> replacementTy
          | substitutionCycle name seen replacementTy -> STVarApp name args
          | otherwise -> continue (Set.insert name seen) replacementTy
        Nothing -> STVarApp name args

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

lowerConstructorBinding :: ElaborateScope -> ConstructorInfo -> LoweredBinding
lowerConstructorBinding scope ctorInfo =
  LoweredBinding
    { loweredBindingName = ctorRuntimeName ctorInfo,
      loweredBindingSourceType = canonicalSourceType scope (quantifyFreeTypeVars (ctorType ctorInfo)),
      loweredBindingExpectedType = constructorBindingExpectedType scope ctorInfo,
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
        loweredBindingSourceType = canonicalSourceType scope expectedTy,
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
        loweredBindingSourceType = canonicalSourceType scope visibleTy,
        loweredBindingExpectedType = lowerType scope expectedTy,
        loweredBindingSurfaceExpr = elaborateResultValue result,
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypes = elaborateResultExternalTypes result,
        loweredBindingExportedAsMain = exportedAsMain
      }
  where
    wrapEvidence (runtimeName0, evidenceTy) acc =
      surfaceLamAnn runtimeName0 evidenceTy acc

lowerConstrainedResolvedExprBinding :: ElaborateScope -> String -> [ConstraintInfo] -> TypeView -> Bool -> P.ResolvedExpr -> Either ProgramError LoweredBinding
lowerConstrainedResolvedExprBinding scope runtimeName constraints visibleView exportedAsMain expr = do
  result <- runElaborateM $ do
    (scopeWithEvidence, evidenceParams) <- extendConstraintEvidenceInfo scope constraints
    let (_, bodyExpectedTy) = splitForalls (typeViewDisplay visibleView)
    bodyExpr <- compileResolvedExpr scopeWithEvidence (Just bodyExpectedTy) expr
    pure (foldr wrapEvidence bodyExpr evidenceParams)
  let expectedTy = constrainedRuntimeTypeInfo scope constraints visibleView
  pure
    LoweredBinding
      { loweredBindingName = runtimeName,
        loweredBindingSourceType = canonicalSourceType scope (typeViewDisplay visibleView),
        loweredBindingExpectedType = lowerType scope expectedTy,
        loweredBindingSurfaceExpr = elaborateResultValue result,
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypes = elaborateResultExternalTypes result,
        loweredBindingExportedAsMain = exportedAsMain
      }
  where
    wrapEvidence (runtimeName0, evidenceTy) acc =
      surfaceLamAnn runtimeName0 evidenceTy acc

lowerResolvedConstrainedExprBinding :: ElaborateScope -> String -> P.ResolvedConstrainedType -> Bool -> P.ResolvedExpr -> Either ProgramError LoweredBinding
lowerResolvedConstrainedExprBinding scope runtimeName ty exportedAsMain expr = do
  constraints <- mapM (resolvedConstraintInfoForScope scope) (P.constrainedConstraints ty)
  bodyView <- resolvedTypeViewForScope scope (P.constrainedBody ty)
  let visibleView =
        TypeView
          { typeViewDisplay =
              constrainedVisibleType
                (P.ConstrainedType (map displayConstraint constraints) (typeViewDisplay bodyView)),
            typeViewIdentity =
              constrainedVisibleType
                ( P.ConstrainedType
                    [ P.ClassConstraint (constraintDisplayClass constraint) (typeViewIdentity (constraintTypeView constraint))
                      | constraint <- constraints
                    ]
                    (typeViewIdentity bodyView)
                )
          }
  lowerConstrainedResolvedExprBinding
    scope
    runtimeName
    constraints
    visibleView
    exportedAsMain
    expr

constrainedRuntimeTypeInfo :: ElaborateScope -> [ConstraintInfo] -> TypeView -> SrcType
constrainedRuntimeTypeInfo scope constraints visibleView =
  let (foralls, bodyTy) = splitForalls (typeViewDisplay visibleView)
      evidenceTys = concatMap constraintEvidenceTypes constraints
      runtimeBody = foldr STArrow bodyTy evidenceTys
   in foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) runtimeBody foralls
  where
    constraintEvidenceTypes constraint =
      case classInfoForConstraint scope constraint of
        Nothing -> []
        Just classInfo ->
          [ lowerTypeRaw (esTypes scope) (methodEvidenceSourceType scope classInfo (typeViewDisplay (constraintTypeView constraint)) methodInfo)
            | methodInfo <- Map.elems (classMethods classInfo)
          ]

resolvedConstraintInfoForScope :: ElaborateScope -> P.ResolvedClassConstraint -> Either ProgramError ConstraintInfo
resolvedConstraintInfoForScope scope constraint =
  ConstraintInfo
    <$> displayClassNameForResolved scope (P.constraintClassName constraint)
    <*> pure (resolvedSymbolIdentity (P.constraintClassName constraint))
    <*> resolvedTypeViewForScope scope (P.constraintType constraint)

resolvedTypeViewForScope :: ElaborateScope -> ResolvedSrcType -> Either ProgramError TypeView
resolvedTypeViewForScope scope ty =
  TypeView
    <$> displaySrcTypeForResolved scope ty
    <*> pure (resolvedSrcTypeIdentityType ty)

displayClassNameForResolved :: ElaborateScope -> ResolvedSymbol -> Either ProgramError String
displayClassNameForResolved scope symbol =
  case displayNameForSymbol (esClassesByIdentity scope) symbol of
    Just name -> pure name
    Nothing -> Left (ProgramUnknownClass (P.refDisplayName symbol))

displaySrcTypeForResolved :: ElaborateScope -> ResolvedSrcType -> Either ProgramError SrcType
displaySrcTypeForResolved scope = \case
  RSTVar name -> pure (STVar name)
  RSTArrow dom cod -> STArrow <$> displaySrcTypeForResolved scope dom <*> displaySrcTypeForResolved scope cod
  RSTBase symbol -> STBase <$> displayTypeHeadNameForResolved scope symbol
  RSTCon symbol args -> STCon <$> displayTypeHeadNameForResolved scope symbol <*> traverse (displaySrcTypeForResolved scope) args
  RSTVarApp name args -> STVarApp name <$> traverse (displaySrcTypeForResolved scope) args
  RSTForall name mb body ->
    STForall name
      <$> traverse (fmap SrcBound . displaySrcTypeForResolved scope . unResolvedSrcBound) mb
      <*> displaySrcTypeForResolved scope body
  RSTMu name body -> STMu name <$> displaySrcTypeForResolved scope body
  RSTBottom -> pure STBottom

displayTypeHeadNameForResolved :: ElaborateScope -> ResolvedSymbol -> Either ProgramError String
displayTypeHeadNameForResolved scope symbol =
  case displayNameForSymbol (esTypesByIdentity scope) symbol of
    Just name -> pure name
    Nothing
      | isBuiltinTypeSymbol symbol -> pure (P.refDisplayName symbol)
    Nothing -> Left (ProgramUnknownType (P.refDisplayName symbol))

displayNameForSymbol :: Map SymbolIdentity [(String, a)] -> ResolvedSymbol -> Maybe String
displayNameForSymbol namesByIdentity symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) namesByIdentity of
    Just entries ->
      case [name | (name, _) <- entries, name == P.refDisplayName symbol] of
        name : _ -> Just name
        [] ->
          case entries of
            (name, _) : _ -> Just name
            [] -> Nothing
    Nothing -> Nothing

isBuiltinTypeSymbol :: ResolvedSymbol -> Bool
isBuiltinTypeSymbol symbol =
  let identity = resolvedSymbolIdentity symbol
   in symbolNamespace identity == SymbolType
        && symbolDefiningModule identity == "<builtin>"

constructorSurfaceExpr :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExpr scope ctorInfo =
  surfaceAnn (constructorSurfaceExprRaw scope ctorInfo) (constructorBindingExpectedType scope ctorInfo)

constructorBindingExpectedType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorBindingExpectedType scope ctorInfo =
  let loweredTy = lowerType scope (quantifyFreeTypeVars (ctorType ctorInfo))
   in if constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypes scope) ctorInfo
        && srcTypeHasVariableHeadApplication loweredTy
        then constructorStructuralPlaceholderType scope ctorInfo
        else loweredTy

constructorSurfaceExprRaw :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExprRaw scope ctorInfo =
  let argNames = ["$" ++ ctorName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
      handlerNames = ["$" ++ ctorName ctorInfo ++ "_k" ++ show ix | ix <- [1 .. length handlerCtorOrder]]
      resultVar =
        if any (not . null . ctorForalls) handlerCtorOrder || constructorOwnerHasParams
          then "$" ++ ctorOwningType ctorInfo ++ "_result"
          else "a"
      useStructuralTypes =
        constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypes scope) ctorInfo
          && srcTypeHasVariableHeadApplication (lowerType scope (quantifyFreeTypeVars (ctorType ctorInfo)))
      argTypes =
        if useStructuralTypes
          then constructorStructuralArgs ctorInfo
          else ctorArgs ctorInfo
      handlerTypes =
        if useStructuralTypes
          then map (constructorStructuralHandlerType resultVar . constructorShapeFromInfo) handlerCtorOrder
          else map (\ctor -> handlerSurfaceType scope ctor (STVar resultVar)) handlerCtorOrder
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
          (zip argNames argTypes)
   in lifted
  where
    ownerInfo =
      visibleDataInfo <$> resolveConstructorDataInfo scope ctorInfo
    ctorOrder =
      maybe [] dataConstructors ownerInfo
    handlerCtorOrder =
      map specializeHandlerConstructor ctorOrder
    constructorOwnerHasParams =
      maybe False (not . null . dataParams) ownerInfo
    specializeHandlerConstructor ctor =
      case matchTypes Map.empty (ctorResult ctor) (ctorResult ctorInfo) of
        Just subst -> specializeConstructorInfo subst ctor
        Nothing -> ctor

compileExpr :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpr scope mbExpected expr = case expr of
  EVar name ->
    case Map.lookup name (esValues scope) of
      Just OverloadedMethod {valueMethodInfo = methodInfo} ->
        compileNullaryMethodUse scope mbExpected methodInfo
      Just valueInfo@OrdinaryValue {valueRuntimeName = runtimeName} -> do
        evidenceSurfaces <- valueEvidenceArgs scope valueInfo mbExpected []
        pure (foldl surfaceApp (surfaceVar runtimeName) evidenceSurfaces)
      Just ConstructorValue {valueCtorInfo = ctorInfo} -> do
        compileConstructorHead scope ctorInfo 0 (constructorInitialSubst scope ctorInfo 0 mbExpected)
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
  EAnn inner annTy ->
    case inner of
      EVar name
        | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope),
          methodFullArity methodInfo == 0 ->
            compileExpr scope (Just annTy) inner
      _ -> do
        innerExpr <- compileExpr scope (Just annTy) inner
        pure (surfaceAnn innerExpr (lowerType scope annTy))
  ECase scrutinee alts -> compileCase scope mbExpected scrutinee alts

compileResolvedExpr :: ElaborateScope -> Maybe SrcType -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileResolvedExpr scope mbExpected expr = case expr of
  EVar ref -> do
    valueInfo <- lookupResolvedValueInfo scope ref
    case valueInfo of
      OverloadedMethod {valueMethodInfo = methodInfo} ->
        compileNullaryMethodUse scope mbExpected methodInfo
      OrdinaryValue {valueRuntimeName = runtimeName} -> do
        evidenceSurfaces <- valueResolvedEvidenceArgs scope valueInfo mbExpected []
        pure (foldl surfaceApp (surfaceVar runtimeName) evidenceSurfaces)
      ConstructorValue {valueCtorInfo = ctorInfo} -> do
        compileConstructorHead scope ctorInfo 0 (constructorInitialSubst scope ctorInfo 0 mbExpected)
  ELit lit -> pure (surfaceLit lit)
  ELam param body -> do
    runtimeName <- freshRuntimeName (P.paramName param)
    paramAnn <- traverse (liftEitherElab . displaySrcTypeForResolved scope) (P.paramType param)
    let paramTy = case (paramAnn, mbExpected) of
          (Just ty, _) -> Just ty
          (Nothing, Just (STArrow dom _)) -> Just dom
          _ -> Nothing
    scope' <- extendLocal scope (P.paramName param) runtimeName paramTy
    bodyExpr0 <- compileResolvedExpr scope' (expectedCodomain mbExpected) body
    let bodyExpr =
          case expectedCodomain mbExpected of
            Just codTy | isRecursiveResultType codTy -> surfaceAnn bodyExpr0 (lowerType scope codTy)
            _ -> bodyExpr0
    pure $
      case paramTy of
        Just ty -> surfaceLamAnn runtimeName (lowerType scope ty) bodyExpr
        Nothing -> surfaceLam runtimeName bodyExpr
  EApp _ _ -> compileResolvedApp scope mbExpected expr
  ELet name mbTy rhs body -> do
    mbDisplayTy <- traverse (liftEitherElab . displaySrcTypeForResolved scope) mbTy
    if name `notElem` collectFreeResolvedValues Set.empty body && mbDisplayTy == Nothing
      then compileResolvedExpr scope mbExpected body
      else do
        let recursive = mentionsFreeResolvedValue name rhs
        case (recursive, mbDisplayTy, inlineImmediateResolvedLetUse name rhs body) of
          (False, Nothing, Just inlined) ->
            compileResolvedExpr scope mbExpected inlined
          _ -> do
            runtimeName <- freshRuntimeName name
            provisionalTy <- case (recursive, mbDisplayTy) of
              (True, Nothing) -> Just <$> freshTypeName
              _ -> pure mbDisplayTy
            selfScope <-
              if recursive
                then extendLocal scope name runtimeName provisionalTy
                else pure scope
            rhsExpr <- compileResolvedExpr selfScope provisionalTy rhs
            bindingTy <- case mbDisplayTy of
              Just ty -> pure (lowerType scope ty)
              Nothing
                | Just rhsTy <- explicitResolvedExprAnnotation scope rhs -> pure (lowerType scope rhsTy)
                | Just rhsTy <- inferKnownResolvedExprType selfScope rhs -> pure (lowerType scope rhsTy)
                | Just ty <- provisionalTy -> pure (lowerType scope ty)
              Nothing -> freshTypeName
            let rhsExpr' =
                  case mbDisplayTy of
                    Just ty -> surfaceAnn rhsExpr (lowerType scope ty)
                    Nothing ->
                      case inferKnownResolvedExprType selfScope rhs of
                        Just ty -> surfaceAnn rhsExpr (lowerType scope ty)
                        Nothing -> rhsExpr
            bodyScope <- extendLocalLowered scope name runtimeName bindingTy
            bodyExpr <- compileResolvedExpr bodyScope mbExpected body
            pure (surfaceLet runtimeName rhsExpr' bodyExpr)
  EAnn inner annTy -> do
    annDisplayTy <- liftEitherElab (displaySrcTypeForResolved scope annTy)
    case inner of
      EVar ref
        | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
          methodFullArity methodInfo == 0 ->
            compileResolvedExpr scope (Just annDisplayTy) inner
      _ -> do
        innerExpr <- compileResolvedExpr scope (Just annDisplayTy) inner
        pure (surfaceAnn innerExpr (lowerType scope annDisplayTy))
  ECase scrutinee alts -> compileResolvedCase scope mbExpected scrutinee alts

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

compileResolvedApp :: ElaborateScope -> Maybe SrcType -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileResolvedApp scope mbExpected expr =
  case collectResolvedApps expr of
    (EVar ref, args) -> do
      valueInfo <- lookupResolvedValueInfo scope ref
      case valueInfo of
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          compileResolvedMethodApp scope mbExpected methodInfo args
        _ ->
          compileResolvedValueApp scope mbExpected valueInfo args
    (headExpr, [arg])
      | Just expectedTy <- mbExpected -> do
          (expectedHeadTy, argSurface) <-
            case inferKnownResolvedExprType scope arg of
              Just argTy -> do
                argSurface <- compileResolvedExpr scope (Just argTy) arg
                pure (STArrow argTy expectedTy, argSurface)
              Nothing -> do
                argTy <- freshTypeName
                argSurface <- compileResolvedExpr scope Nothing arg
                pure (STArrow argTy expectedTy, argSurface)
          headSurface <- compileResolvedExpr scope (Just expectedHeadTy) headExpr
          pure (surfaceApp headSurface argSurface)
    (headExpr, args) -> do
      headSurface <- compileResolvedExpr scope Nothing headExpr
      argSurfaces <- mapM (compileResolvedExpr scope Nothing) args
      pure (foldl surfaceApp headSurface argSurfaces)

explicitExprAnnotation :: P.Expr -> Maybe SrcType
explicitExprAnnotation expr =
  case expr of
    EAnn _ ty -> Just ty
    _ -> Nothing

explicitResolvedExprAnnotation :: ElaborateScope -> P.ResolvedExpr -> Maybe SrcType
explicitResolvedExprAnnotation scope expr =
  case expr of
    EAnn _ ty -> either (const Nothing) Just (displaySrcTypeForResolved scope ty)
    _ -> Nothing

lookupResolvedValueInfo :: ElaborateScope -> P.ResolvedValueRef -> ElaborateM ValueInfo
lookupResolvedValueInfo scope ref =
  case ref of
    P.ResolvedLocalValue name ->
      case Map.lookup name (esValues scope) of
        Just valueInfo -> pure valueInfo
        Nothing -> throwError (ProgramUnknownValue name)
    P.ResolvedGlobalValue symbol ->
      case lookupValueInfoBySymbol scope symbol of
        Just valueInfo -> pure valueInfo
        Nothing -> throwError (ProgramUnknownValue (P.refDisplayName symbol))

lookupValueInfoBySymbol :: ElaborateScope -> ResolvedSymbol -> Maybe ValueInfo
lookupValueInfoBySymbol scope symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) (esValuesByIdentity scope) of
    Just entries ->
      case [info | (name, info) <- entries, name == P.refDisplayName symbol] of
        info : _ -> Just info
        [] ->
          case entries of
            (_, info) : _ -> Just info
            [] -> Nothing
    Nothing -> Nothing

compileValueApp :: ElaborateScope -> Maybe SrcType -> ValueInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileValueApp scope mbExpected ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let (constructorSubst, expectedArgTys) = constructorArgPlan scope ctorInfo mbExpected args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgTys args
  constructorHead <- compileConstructorHead scope ctorInfo (length args) constructorSubst
  pure (foldl surfaceApp constructorHead argSurfaces)
  where
    compileConstructorArg expectedTy arg = do
      case inferKnownExprType scope arg of
        Just knownTy -> do
          let specializedKnownTy = specializeKnownTypeForExpected scope expectedTy knownTy
          ensureSourceTypeCompatible scope expectedTy specializedKnownTy
          if Set.null (freeTypeVarsSrcType knownTy)
            then compileExpr scope (Just knownTy) arg
            else compileExpr scope (Just expectedTy) arg
        Nothing -> do
          argSurface <- compileExpr scope Nothing arg
          pure $
            if hasLeadingForall expectedTy
              then surfaceAnn argSurface (lowerType scope expectedTy)
              else argSurface

compileValueApp scope mbExpected valueInfo args = do
  let expectedArgTys = valueExpectedArgTypes scope valueInfo mbExpected args
  argSurfaces <- zipWithM compileValueArg (expectedArgTys ++ repeat Nothing) args
  evidenceSurfaces <- valueEvidenceArgs scope valueInfo mbExpected args
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
  where
    compileValueArg (Just expectedTy) arg
      | isPartialOverloadedMethodApp scope arg =
          compileKnownExpectedArg expectedTy arg
    compileValueArg (Just expectedTy) arg =
      compileKnownExpectedArg expectedTy arg
    compileValueArg _ arg =
      compileExpr scope Nothing arg

    compileKnownExpectedArg expectedTy arg = do
      case inferKnownExprType scope arg of
        Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
        Nothing -> pure ()
      compileExpr scope (Just expectedTy) arg

compileResolvedValueApp :: ElaborateScope -> Maybe SrcType -> ValueInfo -> [P.ResolvedExpr] -> ElaborateM SurfaceExpr
compileResolvedValueApp scope mbExpected ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let (constructorSubst, expectedArgTys) = constructorResolvedArgPlan scope ctorInfo mbExpected args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgTys args
  constructorHead <- compileConstructorHead scope ctorInfo (length args) constructorSubst
  pure (foldl surfaceApp constructorHead argSurfaces)
  where
    compileConstructorArg expectedTy arg = do
      case inferKnownResolvedExprType scope arg of
        Just knownTy -> do
          let specializedKnownTy = specializeKnownTypeForExpected scope expectedTy knownTy
          ensureSourceTypeCompatible scope expectedTy specializedKnownTy
          if Set.null (freeTypeVarsSrcType knownTy)
            then compileResolvedExpr scope (Just knownTy) arg
            else compileResolvedExpr scope (Just expectedTy) arg
        Nothing -> do
          argSurface <- compileResolvedExpr scope Nothing arg
          pure $
            if hasLeadingForall expectedTy
              then surfaceAnn argSurface (lowerType scope expectedTy)
              else argSurface

compileResolvedValueApp scope mbExpected valueInfo args = do
  let expectedArgTys = valueExpectedArgTypes scope valueInfo mbExpected args
  argSurfaces <- zipWithM compileValueArg (expectedArgTys ++ repeat Nothing) args
  evidenceSurfaces <- valueResolvedEvidenceArgs scope valueInfo mbExpected args
  let headSurface =
        case valueInfo of
          OrdinaryValue {valueRuntimeName = runtimeName} -> surfaceVar runtimeName
          OverloadedMethod {} -> error "compileResolvedValueApp does not handle overloaded methods"
      headWithEvidence = foldl surfaceApp headSurface evidenceSurfaces
      applied = foldl surfaceApp headWithEvidence argSurfaces
  pure $ case mbExpected of
    Just expectedTy
      | not (isLocalOrdinaryValue valueInfo),
        isRecursiveResultType expectedTy || isRecursiveResultType (lowerType scope expectedTy) ->
          surfaceAnn applied (lowerType scope expectedTy)
    _ -> applied
  where
    compileValueArg (Just expectedTy) arg
      | isPartialOverloadedResolvedMethodApp scope arg =
          compileKnownExpectedArg expectedTy arg
    compileValueArg (Just expectedTy) arg =
      compileKnownExpectedArg expectedTy arg
    compileValueArg _ arg =
      compileResolvedExpr scope Nothing arg

    compileKnownExpectedArg expectedTy arg = do
      case inferKnownResolvedExprType scope arg of
        Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
        Nothing -> pure ()
      compileResolvedExpr scope (Just expectedTy) arg

compileConstructorHead :: ElaborateScope -> ConstructorInfo -> Int -> Map String SrcType -> ElaborateM SurfaceExpr
compileConstructorHead scope ctorInfo argCount constructorSubst = do
  placeholder <- deferConstructorCall scope ctorInfo argCount constructorSubst
  pure (surfaceVar placeholder)

specializeConstructorInfo :: Map String SrcType -> ConstructorInfo -> ConstructorInfo
specializeConstructorInfo subst ctorInfo =
  let foralls' =
        [ (name, fmap (specializeSrcType subst) mbBound)
          | (name, mbBound) <- ctorForalls ctorInfo,
            Map.notMember name subst
        ]
      args' = map (specializeSrcType subst) (ctorArgs ctorInfo)
      result' = specializeSrcType subst (ctorResult ctorInfo)
      bodyTy = foldr STArrow result' args'
      type' =
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound mbBound) acc)
          bodyTy
          foralls'
   in ctorInfo
        { ctorType = type',
          ctorForalls = foralls',
          ctorArgs = args',
          ctorResult = result'
        }

ordinaryValueTypeInScope :: ElaborateScope -> ValueInfo -> SrcType
ordinaryValueTypeInScope scope OrdinaryValue {valueType = ty, valueIdentityType = identityTy} =
  visibleTypeForIdentity (esTypes scope) ty identityTy
ordinaryValueTypeInScope _ _ =
  STBottom

valueExpectedArgTypes :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [expr] -> [Maybe SrcType]
valueExpectedArgTypes scope valueInfo mbExpected args =
  let (argTys0, resultTy0) =
        case valueInfo of
          OrdinaryValue {} ->
            splitArrows (snd (splitForalls (ordinaryValueTypeInScope scope valueInfo)))
          _ -> ([], STBottom)
      resultTyForArity =
        foldr STArrow resultTy0 (drop (length args) argTys0)
      subst =
        case mbExpected >>= matchTypesInScope scope Map.empty resultTyForArity of
          Just matched -> matched
          Nothing -> Map.empty
      argTys = map (specializeSrcType subst) argTys0
   in map concreteExpectedTy (take (length args) argTys)
  where
    concreteExpectedTy ty
      | Set.null (freeTypeVarsSrcType ty) = Just ty
      | otherwise = Nothing

isPartialOverloadedMethodApp :: ElaborateScope -> P.Expr -> Bool
isPartialOverloadedMethodApp scope expr =
  case collectApps expr of
    (EVar name, args)
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope) ->
          not (null args) && length args < methodFullArity methodInfo
    _ -> False

isPartialOverloadedResolvedMethodApp :: ElaborateScope -> P.ResolvedExpr -> Bool
isPartialOverloadedResolvedMethodApp scope expr =
  case collectResolvedApps expr of
    (EVar ref, args)
      | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          not (null args) && length args < methodFullArity methodInfo
    _ -> False

runElaborateLookup :: ElaborateM a -> Either ProgramError a
runElaborateLookup action = elaborateResultValue <$> runElaborateM action

specializeKnownTypeForExpected :: ElaborateScope -> SrcType -> SrcType -> SrcType
specializeKnownTypeForExpected scope expectedTy knownTy =
  case matchTypesInScope scope Map.empty knownTy expectedTy of
    Just subst -> specializeSrcType subst knownTy
    Nothing ->
      case matchTypesByShape Map.empty knownTy expectedTy of
        Just subst -> specializeSrcType subst knownTy
        Nothing -> knownTy

matchTypesByShape :: Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesByShape subst template actual = case template of
  STVar name ->
    case Map.lookup name subst of
      Nothing -> Just (Map.insert name actual subst)
      Just existing
        | existing == actual -> Just subst
        | otherwise -> Nothing
  STArrow dom cod ->
    case actual of
      STArrow dom' cod' -> do
        subst' <- matchTypesByShape subst dom dom'
        matchTypesByShape subst' cod cod'
      _ -> Nothing
  STBase {} ->
    case actual of
      STBase {} -> Just subst
      _ -> Nothing
  STCon _ args ->
    case actual of
      STCon _ args'
        | length (toListNE args) == length (toListNE args') ->
            foldM
              (\acc (templateTy, actualTy) -> matchTypesByShape acc templateTy actualTy)
              subst
              (zip (toListNE args) (toListNE args'))
      _ -> Nothing
  STVarApp name args ->
    matchTypeHeadApplicationWith matchTypesByShape (==) subst name args actual
  STForall name mb body ->
    case actual of
      STForall name' mb' body'
        | name == name' -> do
            subst' <-
              case (mb, mb') of
                (Nothing, _) -> Just subst
                (Just bound, Just bound') -> matchTypesByShape subst (unSrcBound bound) (unSrcBound bound')
                (Just {}, Nothing) -> Nothing
            matchTypesByShape subst' body body'
      _ -> Nothing
  STMu name body ->
    case actual of
      STMu name' body'
        | name == name' -> matchTypesByShape subst body body'
      _ -> Nothing
  STBottom ->
    case actual of
      STBottom -> Just subst
      _ -> Nothing

matchTypeHeadApplicationWith ::
  (Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)) ->
  (SrcType -> SrcType -> Bool) ->
  Map String SrcType ->
  String ->
  NonEmpty SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchTypeHeadApplicationWith matchChild sameType subst expectedName expectedArgs actual =
  case actual of
    STCon actualName actualArgs ->
      matchAppliedHead (STBase actualName) (toListNE actualArgs)
    STVarApp actualName actualArgs ->
      matchAppliedHead (STVar actualName) (toListNE actualArgs)
    _ -> Nothing
  where
    expectedArgsList = toListNE expectedArgs
    expectedArgCount = length expectedArgsList

    matchAppliedHead headTy actualArgsList
      | length actualArgsList < expectedArgCount = Nothing
      | otherwise = do
          let (headArgs, matchedArgs) = splitAt (length actualArgsList - expectedArgCount) actualArgsList
          appliedHead <- applyTypeHead headTy headArgs
          subst' <- bindTypeHeadVariable sameType subst expectedName appliedHead
          foldM
            (\acc (templateTy, actualTy) -> matchChild acc templateTy actualTy)
            subst'
            (zip expectedArgsList matchedArgs)

bindTypeHeadVariable ::
  (SrcType -> SrcType -> Bool) ->
  Map String SrcType ->
  String ->
  SrcType ->
  Maybe (Map String SrcType)
bindTypeHeadVariable sameType subst name ty =
  case Map.lookup name subst of
    Just existing
      | sameType existing ty -> Just subst
      | otherwise -> Nothing
    Nothing
      | ty == STVar name -> Just subst
      | name `Set.member` freeTypeVarsSrcType ty -> Nothing
      | otherwise -> Just (Map.insert name ty subst)

constructorArgPlan :: ElaborateScope -> ConstructorInfo -> Maybe SrcType -> [P.Expr] -> (Map String SrcType, [SrcType])
constructorArgPlan scope ctorInfo mbExpected args =
  let (subst, argTys) = foldl step (initialSubst, []) (zip (ctorArgs ctorInfo) args)
   in (subst, reverse argTys)
  where
    initialSubst =
      constructorInitialSubst scope ctorInfo (length args) mbExpected

    step (subst, acc) (templateTy, arg) =
      let subst' =
            case inferKnownExprType scope arg >>= matchTypesInScope scope subst templateTy of
              Just matched -> matched
              Nothing -> subst
          expectedTy = specializeSrcType subst' templateTy
       in (subst', expectedTy : acc)

constructorResolvedArgPlan :: ElaborateScope -> ConstructorInfo -> Maybe SrcType -> [P.ResolvedExpr] -> (Map String SrcType, [SrcType])
constructorResolvedArgPlan scope ctorInfo mbExpected args =
  let (subst, argTys) = foldl step (initialSubst, []) (zip (ctorArgs ctorInfo) args)
   in (subst, reverse argTys)
  where
    initialSubst =
      constructorInitialSubst scope ctorInfo (length args) mbExpected

    step (subst, acc) (templateTy, arg) =
      let subst' =
            case inferKnownResolvedExprType scope arg >>= matchTypesInScope scope subst templateTy of
              Just matched -> matched
              Nothing -> subst
          expectedTy = specializeSrcType subst' templateTy
       in (subst', expectedTy : acc)

constructorInitialSubst :: ElaborateScope -> ConstructorInfo -> Int -> Maybe SrcType -> Map String SrcType
constructorInitialSubst scope ctorInfo argCount mbExpected =
  case mbExpected >>= matchTypesInScope scope Map.empty (constructorOccurrenceType ctorInfo argCount) of
    Just subst -> subst
    Nothing -> Map.empty

valueEvidenceArgs :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [P.Expr] -> ElaborateM [SurfaceExpr]
valueEvidenceArgs scope OrdinaryValue {valueDisplayName = displayName, valueType = visibleTy, valueConstraints = displayConstraints, valueConstraintInfos = constraints} mbExpected args
  | null constraints = pure []
  | otherwise = do
      subst <-
        case inferCallSubst scope visibleTy args of
          Just subst0 -> pure (fmap (sourceTypeViewInScope scope) (refineValueEvidenceSubst scope visibleTy mbExpected args subst0))
          Nothing ->
            case displayConstraints of
              constraint : _ -> throwError (ProgramNoMatchingInstance (P.constraintClassName constraint) (P.constraintType constraint))
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      if any usesLocalPolymorphicEvidence specializedConstraints
        then throwError (ProgramAmbiguousConstrainedValueUse displayName)
        else concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      not (Set.null (freeTypeVarsTypeView (constraintTypeView constraint)))
        && constraintCoveredByEvidenceInfo scope constraint
valueEvidenceArgs _ _ _ _ = pure []

valueResolvedEvidenceArgs :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [P.ResolvedExpr] -> ElaborateM [SurfaceExpr]
valueResolvedEvidenceArgs scope OrdinaryValue {valueDisplayName = displayName, valueType = visibleTy, valueConstraints = displayConstraints, valueConstraintInfos = constraints} mbExpected args
  | null constraints = pure []
  | otherwise = do
      subst <-
        case inferResolvedCallSubst scope visibleTy args of
          Just subst0 -> pure (fmap (sourceTypeViewInScope scope) (refineValueEvidenceSubst scope visibleTy mbExpected args subst0))
          Nothing ->
            case displayConstraints of
              constraint : _ -> throwError (ProgramNoMatchingInstance (P.constraintClassName constraint) (P.constraintType constraint))
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      if any usesLocalPolymorphicEvidence specializedConstraints
        then throwError (ProgramAmbiguousConstrainedValueUse displayName)
        else concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      not (Set.null (freeTypeVarsTypeView (constraintTypeView constraint)))
        && constraintCoveredByEvidenceInfo scope constraint
valueResolvedEvidenceArgs _ _ _ _ = pure []

refineValueEvidenceSubst :: ElaborateScope -> SrcType -> Maybe SrcType -> [arg] -> Map String SrcType -> Map String SrcType
refineValueEvidenceSubst scope visibleTy mbExpected args subst =
  case mbExpected >>= matchTypesInScope scope subst resultTyForArity of
    Just subst' -> subst'
    Nothing -> subst
  where
    (argTys, resultTy) = splitArrows (snd (splitForalls visibleTy))
    resultTyForArity = foldr STArrow resultTy (drop (length args) argTys)

constraintEvidenceArgExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [SurfaceExpr]
constraintEvidenceArgExprsInfo scope constraint
  | shouldDeferConstraintEvidenceInfo scope constraint =
      deferConstraintEvidenceExprsInfo scope constraint
  | otherwise =
      resolveConstraintEvidenceExpr scope Set.empty constraint

shouldDeferConstraintEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
shouldDeferConstraintEvidenceInfo scope constraint =
  not (Set.null (freeTypeVarsTypeView (constraintTypeView constraint)))
    && not (constraintCoveredByEvidenceInfo scope constraint)

constraintCoveredByEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
constraintCoveredByEvidenceInfo scope constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> False
    Just classInfo
      | Map.null (classMethods classInfo) ->
          zeroMethodConstraintCoveredByEvidenceInfo scope constraint
      | otherwise ->
          all
            ( \methodInfo ->
                case lookupEvidenceMethodInfo scope constraint (methodName methodInfo) of
                  Just _ -> True
                  Nothing -> False
            )
            (Map.elems (classMethods classInfo))

deferConstraintEvidenceExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [SurfaceExpr]
deferConstraintEvidenceExprsInfo scope constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))
    Just classInfo
      | Map.null (classMethods classInfo) ->
          resolveZeroMethodEvidenceExpr scope Set.empty constraint
      | otherwise ->
          mapM (deferMethodEvidenceExpr scope (constraintTypeView constraint)) (Map.elems (classMethods classInfo))

deferMethodEvidenceExpr :: ElaborateScope -> TypeView -> MethodInfo -> ElaborateM SurfaceExpr
deferMethodEvidenceExpr scope classArgView methodInfo = do
  let methodTy =
        stripVacuousSrcForalls $
          specializeMethodType (methodType methodInfo) (methodParamName methodInfo) (typeViewDisplay classArgView)
      fullArity = methodFullArity methodInfo
  placeholder <- deferMethodCall scope methodInfo fullArity methodTy
  expanded <- etaExpandMissingArgs scope methodInfo methodTy Nothing 0 fullArity (surfaceVar placeholder)
  pure (surfaceAnn expanded (lowerTypeView scope (TypeView methodTy (specializeMethodType (methodTypeIdentity methodInfo) (methodParamName methodInfo) (typeViewIdentity classArgView)))))

inferCallSubst :: ElaborateScope -> SrcType -> [P.Expr] -> Maybe (Map String SrcType)
inferCallSubst scope visibleTy args = do
  let (_, bodyTy) = splitForalls visibleTy
      (argTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip argTys args,
            Just actualTy <- [inferKnownExprType scope arg]
        ]
  foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty knownPairs

inferResolvedCallSubst :: ElaborateScope -> SrcType -> [P.ResolvedExpr] -> Maybe (Map String SrcType)
inferResolvedCallSubst scope visibleTy args = do
  let (_, bodyTy) = splitForalls visibleTy
      (argTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip argTys args,
            Just actualTy <- [inferKnownResolvedExprType scope arg]
        ]
  foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty knownPairs

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

inlineImmediateResolvedLetUse :: String -> P.ResolvedExpr -> P.ResolvedExpr -> Maybe P.ResolvedExpr
inlineImmediateResolvedLetUse bindingName rhs body =
  let (headExpr, args) = collectResolvedApps body
   in case (headExpr, args) of
        (EVar (P.ResolvedLocalValue name), _ : _)
          | name == bindingName,
            resolvedRhsConsumesAppliedArgs rhs (length args) ->
              Just (foldl EApp rhs args)
        _ -> Nothing

resolvedRhsConsumesAppliedArgs :: P.ResolvedExpr -> Int -> Bool
resolvedRhsConsumesAppliedArgs _ 0 = True
resolvedRhsConsumesAppliedArgs expr argCount =
  case expr of
    ELam param body ->
      mentionsFreeResolvedValue (P.paramName param) body
        && resolvedRhsConsumesAppliedArgs body (argCount - 1)
    _ -> True

isLocalOrdinaryValue :: ValueInfo -> Bool
isLocalOrdinaryValue OrdinaryValue {valueOriginModule = "<local>"} = True
isLocalOrdinaryValue _ = False

knownConstructorResultType :: ElaborateScope -> ConstructorInfo -> [P.Expr] -> Maybe SrcType
knownConstructorResultType scope ctorInfo args = do
  argTypes <- traverse (inferKnownExprType scope) args
  subst <- foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty (zip (ctorArgs ctorInfo) argTypes)
  let visibleResult =
        case resolveConstructorDataInfo scope ctorInfo of
          Just resolved -> dataHeadType (visibleDataInfo resolved)
          Nothing -> ctorResult ctorInfo
  pure (Map.foldrWithKey substituteTypeVar visibleResult subst)

knownResolvedConstructorResultType :: ElaborateScope -> ConstructorInfo -> [P.ResolvedExpr] -> Maybe SrcType
knownResolvedConstructorResultType scope ctorInfo args = do
  argTypes <- traverse (inferKnownResolvedExprType scope) args
  subst <- foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty (zip (ctorArgs ctorInfo) argTypes)
  let visibleResult =
        case resolveConstructorDataInfo scope ctorInfo of
          Just resolved -> dataHeadType (visibleDataInfo resolved)
          Nothing -> ctorResult ctorInfo
  pure (Map.foldrWithKey substituteTypeVar visibleResult subst)

compileMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileMethodApp scope mbExpected methodInfo args
  | null args = compileNullaryMethodUse scope mbExpected methodInfo
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
              methodHead <- resolveMethodHeadForCall scope Set.empty methodInfo classArgTy args
              let applied = foldl surfaceApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
              pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
        _ -> do
          placeholder <- deferMethodCall scope methodInfo fullArity placeholderTy
          let applied = foldl surfaceApp (surfaceVar placeholder) argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
          pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
  where
    annotatePartialMethod expanded placeholderTy suppliedArity fullArity
      | suppliedArity < fullArity =
          case mbExpected of
            Just expectedTy -> surfaceAnn expanded (lowerType scope expectedTy)
            Nothing ->
              case peelAppliedType placeholderTy suppliedArity of
                Just remainingTy -> surfaceAnn expanded (lowerType scope remainingTy)
                Nothing -> expanded
      | otherwise = expanded

compileResolvedMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.ResolvedExpr] -> ElaborateM SurfaceExpr
compileResolvedMethodApp scope mbExpected methodInfo args
  | null args = compileNullaryMethodUse scope mbExpected methodInfo
  | otherwise = do
      let fullArity = methodFullArity methodInfo
          suppliedArity = length args
          knownClassArg = knownResolvedMethodClassArg scope methodInfo args
          placeholderTy = placeholderResolvedMethodType scope methodInfo args
          knownArgTys =
            case knownClassArg of
              Just _ -> Just (take suppliedArity (methodArgumentTypes placeholderTy))
              Nothing -> Nothing
      argSurfaces <-
        case knownArgTys of
          Just argTys -> zipWithM (compileExpectedResolvedMethodArg scope) argTys args
          Nothing -> mapM (compileResolvedExpr scope Nothing) args
      case knownClassArg of
        Just classArgTy
          | shouldResolveMethodBeforeInference scope methodInfo classArgTy -> do
              methodHead <- resolveResolvedMethodHeadForCall scope Set.empty methodInfo classArgTy args
              let applied = foldl surfaceApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
              pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
        _ -> do
          placeholder <- deferMethodCall scope methodInfo fullArity placeholderTy
          let applied = foldl surfaceApp (surfaceVar placeholder) argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
          pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
  where
    annotatePartialMethod expanded placeholderTy suppliedArity fullArity
      | suppliedArity < fullArity =
          case mbExpected of
            Just expectedTy -> surfaceAnn expanded (lowerType scope expectedTy)
            Nothing ->
              case peelAppliedType placeholderTy suppliedArity of
                Just remainingTy -> surfaceAnn expanded (lowerType scope remainingTy)
                Nothing -> expanded
      | otherwise = expanded

compileNullaryMethodUse :: ElaborateScope -> Maybe SrcType -> MethodInfo -> ElaborateM SurfaceExpr
compileNullaryMethodUse scope mbExpected methodInfo =
  case nullaryMethodExpectedResultView scope mbExpected methodInfo of
    Just expectedView -> do
      placeholder <- deferNullaryMethodCall scope methodInfo expectedView
      pure (surfaceVar placeholder)
    Nothing -> throwError (ProgramAmbiguousMethodUse (methodName methodInfo))

nullaryMethodExpectedResultView :: ElaborateScope -> Maybe SrcType -> MethodInfo -> Maybe TypeView
nullaryMethodExpectedResultView scope mbExpected methodInfo = do
  expectedTy <- mbExpected
  _ <- inferNullaryMethodClassArg scope methodInfo expectedTy
  pure (sourceTypeViewInScope scope expectedTy)

inferNullaryMethodClassArg :: ElaborateScope -> MethodInfo -> SrcType -> Maybe SrcType
inferNullaryMethodClassArg scope methodInfo expectedTy
  | methodFullArity methodInfo /= 0 = Nothing
  | otherwise = do
      let (_, bodyTy) = splitForalls (methodType methodInfo)
          (_, resultTy) = splitArrows bodyTy
      subst <- matchTypesInScope scope Map.empty resultTy expectedTy
      Map.lookup (methodParamName methodInfo) subst

compileExpectedMethodArg :: ElaborateScope -> SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpectedMethodArg scope expectedTy expr = do
  case inferKnownExprType scope expr of
    Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
    Nothing -> pure ()
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
    EVar name
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope),
        methodFullArity methodInfo == 0 ->
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

compileExpectedResolvedMethodArg :: ElaborateScope -> SrcType -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileExpectedResolvedMethodArg scope expectedTy expr = do
  case inferKnownResolvedExprType scope expr of
    Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
    Nothing -> pure ()
  case expr of
    EAnn {} ->
      compileResolvedExpr scope (Just expectedTy) expr
    EApp (ELam param (EVar (P.ResolvedLocalValue bodyName))) actual
      | bodyName == P.paramName param ->
          compileResolvedExpr scope (Just expectedTy) actual
    EApp (ELam param body) actual -> do
      runtimeName <- freshRuntimeName (P.paramName param)
      actualExpr <- compileResolvedExpr scope (Just expectedTy) actual
      scope' <- extendLocal scope (P.paramName param) runtimeName (Just expectedTy)
      bodyExpr <- compileResolvedExpr scope' (Just expectedTy) body
      pure (surfaceLet runtimeName actualExpr (surfaceAnn bodyExpr (lowerType scope expectedTy)))
    EVar ref
      | Right ConstructorValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          compileResolvedExpr scope (Just expectedTy) expr
    EVar ref
      | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
        methodFullArity methodInfo == 0 ->
          compileResolvedExpr scope (Just expectedTy) expr
    EVar {} ->
      compileResolvedExpr scope Nothing expr
    _
      | (EVar ref, _) <- collectResolvedApps expr,
        Right ConstructorValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          compileResolvedExpr scope (Just expectedTy) expr
    _ -> do
      argExpr <- compileResolvedExpr scope Nothing expr
      pure (surfaceAnn argExpr (lowerType scope expectedTy))

ensureSourceTypeCompatible :: ElaborateScope -> SrcType -> SrcType -> ElaborateM ()
ensureSourceTypeCompatible scope expectedTy actualTy =
  when (sourceTypesNeedNominalRejection scope expectedTy actualTy) $
    throwError (ProgramTypeMismatch actualTy expectedTy)

sourceTypesNeedNominalRejection :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypesNeedNominalRejection scope expectedTy actualTy =
  sourceTypeMentionsVisibleData scope expectedTy
    && sourceTypeMentionsVisibleData scope actualTy
    && lowerType scope expectedTy == lowerType scope actualTy
    && matchTypesInScope scope Map.empty expectedTy actualTy == Nothing
    && matchTypesInScope scope Map.empty actualTy expectedTy == Nothing

sourceTypeMentionsVisibleData :: ElaborateScope -> SrcType -> Bool
sourceTypeMentionsVisibleData scope ty =
  case ty of
    STVar {} -> False
    STBase name -> Map.member name (esTypes scope)
    STCon name args ->
      Map.member name (esTypes scope)
        || any (sourceTypeMentionsVisibleData scope) args
    STVarApp _ args -> any (sourceTypeMentionsVisibleData scope) args
    STArrow dom cod ->
      sourceTypeMentionsVisibleData scope dom
        || sourceTypeMentionsVisibleData scope cod
    STForall _ mb body ->
      maybe False (sourceTypeMentionsVisibleData scope . unSrcBound) mb
        || sourceTypeMentionsVisibleData scope body
    STMu _ body -> sourceTypeMentionsVisibleData scope body
    STBottom -> False

resolveMethodHeadExprInfo :: ElaborateScope -> Set (SymbolIdentity, String) -> MethodInfo -> TypeView -> ElaborateM SurfaceExpr
resolveMethodHeadExprInfo scope seen methodInfo classArgView =
  case lookupEvidenceMethodByClass scope (methodInfoClassIdentity methodInfo) (typeViewIdentity classArgView) (methodName methodInfo) of
    Just (runtimeName, _) -> pure (surfaceVar runtimeName)
    Nothing -> do
      (instanceInfo, subst) <- liftEitherElab (resolveMethodInstanceInfoByTypeView scope methodInfo classArgView)
      case Map.lookup (methodName methodInfo) (instanceMethods instanceInfo) of
        Just OrdinaryValue {valueRuntimeName = runtimeName, valueConstraintInfos = constraints} -> do
          let headVars = freeTypeVarsTypeView classArgView
              eagerConstraints =
                filter
                  (constraintInfoDeterminedByTypeVars headVars)
                  (map (applyConstraintInfoSubst subst) constraints)
          evidenceArgs <-
            concat
              <$> mapM
                (resolveConstraintEvidenceExpr scope seen)
                eagerConstraints
          pure (foldl surfaceApp (surfaceVar runtimeName) evidenceArgs)
        _ -> throwError (ProgramUnknownMethod (methodName methodInfo))

resolveMethodHeadForCall :: ElaborateScope -> Set (SymbolIdentity, String) -> MethodInfo -> SrcType -> [P.Expr] -> ElaborateM SurfaceExpr
resolveMethodHeadForCall scope seen methodInfo classArgTy args =
  let classArgView = sourceTypeViewInScope scope classArgTy
   in case lookupEvidenceMethodByClass scope (methodInfoClassIdentity methodInfo) (typeViewIdentity classArgView) (methodName methodInfo) of
    Just (runtimeName, _) -> do
      evidenceArgs <- methodLocalEvidenceArgsForCall scope methodInfo classArgTy args
      pure (foldl surfaceApp (surfaceVar runtimeName) evidenceArgs)
    Nothing -> resolveMethodHeadExprInfo scope seen methodInfo classArgView

resolveResolvedMethodHeadForCall :: ElaborateScope -> Set (SymbolIdentity, String) -> MethodInfo -> SrcType -> [P.ResolvedExpr] -> ElaborateM SurfaceExpr
resolveResolvedMethodHeadForCall scope seen methodInfo classArgTy args =
  let classArgView = sourceTypeViewInScope scope classArgTy
   in case lookupEvidenceMethodByClass scope (methodInfoClassIdentity methodInfo) (typeViewIdentity classArgView) (methodName methodInfo) of
    Just (runtimeName, _) -> do
      evidenceArgs <- methodLocalEvidenceArgsForResolvedCall scope methodInfo classArgTy args
      pure (foldl surfaceApp (surfaceVar runtimeName) evidenceArgs)
    Nothing -> resolveMethodHeadExprInfo scope seen methodInfo classArgView

methodLocalEvidenceArgsForCall :: ElaborateScope -> MethodInfo -> SrcType -> [P.Expr] -> ElaborateM [SurfaceExpr]
methodLocalEvidenceArgsForCall scope methodInfo classArgTy args = do
  subst <-
    case inferMethodCallSubst scope methodInfo classArgTy args of
      Just subst0 -> pure subst0
      Nothing -> pure Map.empty
  let methodLocalConstraintInfos =
        filter
          (not . constraintInfoDeterminedByTypeVars (freeTypeVarsTypeView (sourceTypeViewInScope scope classArgTy)))
          (map (specializeConstraintInfoType (methodParamName methodInfo) (sourceTypeViewInScope scope classArgTy)) (methodConstraintInfos methodInfo))
      specializedConstraints = map (applyConstraintInfoSubst (fmap (sourceTypeViewInScope scope) subst)) methodLocalConstraintInfos
  concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints

methodLocalEvidenceArgsForResolvedCall :: ElaborateScope -> MethodInfo -> SrcType -> [P.ResolvedExpr] -> ElaborateM [SurfaceExpr]
methodLocalEvidenceArgsForResolvedCall scope methodInfo classArgTy args = do
  subst <-
    case inferResolvedMethodCallSubst scope methodInfo classArgTy args of
      Just subst0 -> pure subst0
      Nothing -> pure Map.empty
  let methodLocalConstraintInfos =
        filter
          (not . constraintInfoDeterminedByTypeVars (freeTypeVarsTypeView (sourceTypeViewInScope scope classArgTy)))
          (map (specializeConstraintInfoType (methodParamName methodInfo) (sourceTypeViewInScope scope classArgTy)) (methodConstraintInfos methodInfo))
      specializedConstraints = map (applyConstraintInfoSubst (fmap (sourceTypeViewInScope scope) subst)) methodLocalConstraintInfos
  concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints

inferMethodCallSubst :: ElaborateScope -> MethodInfo -> SrcType -> [P.Expr] -> Maybe (Map String SrcType)
inferMethodCallSubst scope methodInfo classArgTy args = do
  let specializedMethodTy = specializeMethodType (methodType methodInfo) (methodParamName methodInfo) classArgTy
      (_, bodyTy) = splitForalls specializedMethodTy
      (paramTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip paramTys args,
            Just actualTy <- [inferKnownExprType scope arg]
        ]
  foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty knownPairs

inferResolvedMethodCallSubst :: ElaborateScope -> MethodInfo -> SrcType -> [P.ResolvedExpr] -> Maybe (Map String SrcType)
inferResolvedMethodCallSubst scope methodInfo classArgTy args = do
  let specializedMethodTy = specializeMethodType (methodType methodInfo) (methodParamName methodInfo) classArgTy
      (_, bodyTy) = splitForalls specializedMethodTy
      (paramTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip paramTys args,
            Just actualTy <- [inferKnownResolvedExprType scope arg]
        ]
  foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty knownPairs

shouldResolveMethodBeforeInference :: ElaborateScope -> MethodInfo -> SrcType -> Bool
shouldResolveMethodBeforeInference scope methodInfo classArgTy =
  case lookupEvidenceMethodByClass scope (methodInfoClassIdentity methodInfo) (typeViewIdentity (sourceTypeViewInScope scope classArgTy)) (methodName methodInfo) of
    Just _ -> True
    Nothing -> False

resolveConstraintEvidenceExpr :: ElaborateScope -> Set (SymbolIdentity, String) -> ConstraintInfo -> ElaborateM [SurfaceExpr]
resolveConstraintEvidenceExpr scope seen constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))
    Just classInfo -> do
      let key = constraintEvidenceKey constraint
      whenSeen key
      if Map.null (classMethods classInfo)
        then resolveZeroMethodEvidenceExpr scope seen constraint
        else
          mapM
            ( \methodInfo ->
                resolveMethodHeadExprInfo
                  scope
                  (Set.insert key seen)
                  methodInfo
                  (constraintTypeView constraint)
            )
            (Map.elems (classMethods classInfo))
  where
    whenSeen key =
      when (key `Set.member` seen) $
        throwError (ProgramNoMatchingInstance (constraintDisplayClass constraint) (typeViewDisplay (constraintTypeView constraint)))

resolveZeroMethodEvidenceExpr :: ElaborateScope -> Set (SymbolIdentity, String) -> ConstraintInfo -> ElaborateM [SurfaceExpr]
resolveZeroMethodEvidenceExpr scope seen constraint
  | zeroMethodConstraintCoveredByEvidenceInfo scope constraint = pure []
  | otherwise = do
      let key = constraintEvidenceKey constraint
      (instanceInfo, subst) <- liftEitherElab (resolveInstanceInfoByConstraint scope constraint)
      _ <-
        concat
          <$> mapM
            (resolveConstraintEvidenceExpr scope (Set.insert key seen) . applyConstraintInfoSubst subst)
            (instanceConstraintInfos instanceInfo)
      pure []

zeroMethodConstraintCoveredByEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidenceInfo scope constraint =
  any
    ( \evidence ->
        evidenceClassSymbol evidence == constraintClassSymbol constraint
          && evidenceTypeIdentity evidence == typeViewIdentity (constraintTypeView constraint)
    )
    (esEvidence scope)

lookupEvidenceMethodInfo :: ElaborateScope -> ConstraintInfo -> P.MethodName -> Maybe (String, SrcType)
lookupEvidenceMethodInfo scope constraint =
  lookupEvidenceMethodByClass scope (constraintClassSymbol constraint) (typeViewIdentity (constraintTypeView constraint))

lookupEvidenceMethodByClass :: ElaborateScope -> SymbolIdentity -> SrcType -> P.MethodName -> Maybe (String, SrcType)
lookupEvidenceMethodByClass scope classIdentity0 headIdentityTy methodName0 =
  case
    [ methodEvidence
      | evidence <- esEvidence scope,
        evidenceClassSymbol evidence == classIdentity0,
        evidenceTypeIdentity evidence == headIdentityTy,
        Just methodEvidence <- [Map.lookup methodName0 (evidenceMethods evidence)]
    ]
  of
    methodEvidence : _ -> Just methodEvidence
    [] -> Nothing

constraintClassIdentity :: ElaborateScope -> P.ClassName -> Maybe SymbolIdentity
constraintClassIdentity scope className0 =
  classInfoSymbolIdentity <$> Map.lookup className0 (esClasses scope)

classInfoForConstraint :: ElaborateScope -> ConstraintInfo -> Maybe ClassInfo
classInfoForConstraint scope constraint =
  case Map.lookup (constraintClassSymbol constraint) (esClassesByIdentity scope) of
    Just ((_, classInfo) : _) -> Just classInfo
    _ -> Map.lookup (constraintDisplayClass constraint) (esClasses scope)

constraintEvidenceKey :: ConstraintInfo -> (SymbolIdentity, String)
constraintEvidenceKey constraint =
  (constraintClassSymbol constraint, show (typeViewIdentity (constraintTypeView constraint)))

liftEitherElab :: Either ProgramError a -> ElaborateM a
liftEitherElab = either throwError pure

etaExpandMissingArgs :: ElaborateScope -> MethodInfo -> SrcType -> Maybe SrcType -> Int -> Int -> SurfaceExpr -> ElaborateM SurfaceExpr
etaExpandMissingArgs scope methodInfo methodTy mbExpected suppliedArity fullArity applied = do
  let missingArity = max 0 (fullArity - suppliedArity)
  if missingArity == 0
    then pure applied
    else do
      missingNames <- replicateM missingArity (freshRuntimeName (methodName methodInfo ++ "_arg"))
      let missingTypes = zipWith preferExpectedType methodMissingTypes (expectedMissingTypes ++ repeat Nothing)
          body = foldl surfaceApp applied (map surfaceVar missingNames)
      pure (foldr wrapMissingArg body (zip missingNames missingTypes))
  where
    methodMissingTypes =
      drop suppliedArity (methodArgumentTypes methodTy)

    expectedMissingTypes =
      case mbExpected of
        Just expectedTy ->
          map Just (fst (splitArrows (snd (splitForalls expectedTy))))
        Nothing -> []

    preferExpectedType methodTy0 (Just expectedTy)
      | Set.null (freeTypeVarsSrcType expectedTy) = expectedTy
      | otherwise = methodTy0
    preferExpectedType methodTy0 Nothing = methodTy0

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
            deferredMethodName = methodName methodInfo,
            deferredMethodExpectedResult = Nothing,
            deferredMethodEvidence = Nothing
          }
  registerDeferredObligation placeholder placeholderTy (DeferredMethod deferred)
  pure placeholder

deferNullaryMethodCall :: ElaborateScope -> MethodInfo -> TypeView -> ElaborateM String
deferNullaryMethodCall scope methodInfo expectedView = do
  placeholder <- freshDeferredMethodName (methodName methodInfo)
  let placeholderTy = lowerTypeView scope expectedView
      localEvidence = nullaryMethodEvidence scope methodInfo expectedView
      deferred =
        DeferredMethodCall
          { deferredMethodPlaceholder = placeholder,
            deferredMethodInfo = methodInfo,
            deferredMethodArgCount = 0,
            deferredMethodFullArity = 0,
            deferredMethodName = methodName methodInfo,
            deferredMethodExpectedResult = Just expectedView,
            deferredMethodEvidence = localEvidence
          }
  registerDeferredObligation placeholder placeholderTy (DeferredMethod deferred)
  pure placeholder

nullaryMethodEvidence :: ElaborateScope -> MethodInfo -> TypeView -> Maybe DeferredMethodEvidence
nullaryMethodEvidence scope methodInfo expectedView = do
  classArgTy <- inferNullaryMethodClassArg scope methodInfo (typeViewDisplay expectedView)
  let classArgView = sourceTypeViewInScope scope classArgTy
  (runtimeName, evidenceTy) <-
    lookupEvidenceMethodByClass
      scope
      (methodInfoOwnerClassSymbolIdentity methodInfo)
      (typeViewIdentity classArgView)
      (methodName methodInfo)
  pure
    DeferredMethodEvidence
      { deferredMethodEvidenceClassArg = classArgView,
        deferredMethodEvidenceRuntimeName = runtimeName,
        deferredMethodEvidenceType = evidenceTy
      }

deferConstructorCall :: ElaborateScope -> ConstructorInfo -> Int -> Map String SrcType -> ElaborateM String
deferConstructorCall scope ctorInfo argCount initialSubst = do
  placeholder <- freshDeferredConstructorName (ctorName ctorInfo)
  let quantifiedTy = quantifyFreeTypeVars (ctorType ctorInfo)
      occurrenceTy = constructorOccurrenceType ctorInfo argCount
      instBinders = map fst (fst (splitForalls quantifiedTy))
      placeholderSourceTy = specializeQuantifiedType initialSubst quantifiedTy
      loweredPlaceholderTy = lowerType scope placeholderSourceTy
      placeholderTy =
        if constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypes scope) ctorInfo
          && srcTypeHasVariableHeadApplication loweredPlaceholderTy
          then constructorStructuralPlaceholderType scope ctorInfo
          else loweredPlaceholderTy
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

constructorStructuralPlaceholderType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorStructuralPlaceholderType scope ctorInfo =
  constructorStructuralPlaceholderTypeFor (elaborateScopeDataTypes scope) ctorInfo

constructorStructuralPlaceholderTypeFor :: Map String DataInfo -> ConstructorInfo -> SrcType
constructorStructuralPlaceholderTypeFor dataTypes ctorInfo =
  foldr
    STArrow
    (STVar resultVar)
    (constructorStructuralArgs ctorInfo ++ map handlerType ownerShapes)
  where
    ownerShapes =
      case [dataInfo | dataInfo <- Map.elems dataTypes, dataInfoSymbolIdentity dataInfo == ctorOwningTypeIdentity ctorInfo] of
        dataInfo : _ -> map constructorShapeFromInfo (dataConstructors dataInfo)
        [] -> constructorOwnerShapes ctorInfo

    resultVar = "$" ++ ctorOwningType ctorInfo ++ "_result"

    handlerType shape =
      constructorStructuralHandlerType resultVar shape

constructorStructuralHandlerType :: String -> ConstructorShape -> SrcType
constructorStructuralHandlerType resultVar shape =
  foldr STArrow (STVar resultVar) (constructorStructuralShapeArgs shape)

constructorStructuralArgs :: ConstructorInfo -> [SrcType]
constructorStructuralArgs ctor =
  constructorStructuralArgsFor (ctorName ctor) (length (ctorArgs ctor))

constructorStructuralShapeArgs :: ConstructorShape -> [SrcType]
constructorStructuralShapeArgs shape =
  constructorStructuralArgsFor (constructorShapeName shape) (length (constructorShapeArgs shape))

constructorStructuralArgsFor :: String -> Int -> [SrcType]
constructorStructuralArgsFor name arity =
  [ STVar ("$" ++ name ++ "_arg" ++ show ix ++ "_type")
    | ix <- [1 .. arity]
  ]

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
  STVarApp name args ->
    let args' = fmap (specializeSrcType subst) args
     in case Map.lookup name subst >>= \replacement -> applyTypeHead replacement (toListNE args') of
          Just replacementTy -> replacementTy
          Nothing -> STVarApp name args'
  STForall name mb body
    | Map.member name subst -> STForall name mb body
    | otherwise ->
        STForall name (fmap (SrcBound . specializeSrcType subst . unSrcBound) mb) (specializeSrcType subst body)
  STMu name body
    | Map.member name subst -> STMu name body
    | otherwise -> STMu name (specializeSrcType subst body)
  STBottom -> STBottom

deferCaseCall :: ElaborateScope -> DataInfo -> SrcType -> SrcType -> ElaborateM String
deferCaseCall scope dataInfo scrutineeTy resultTy = do
  placeholder <- freshDeferredCaseName (dataName dataInfo)
  let resultTyElab = lowerType scope resultTy
      handlerTys = replicate (length (dataConstructors dataInfo)) STBottom
      placeholderTy = foldr STArrow resultTyElab (lowerType scope scrutineeTy : handlerTys)
      deferred =
        DeferredCaseCall
          { deferredCasePlaceholder = placeholder,
            deferredCaseDataInfo = dataInfo,
            deferredCaseScrutineeType = scrutineeTy,
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

placeholderResolvedMethodType :: ElaborateScope -> MethodInfo -> [P.ResolvedExpr] -> SrcType
placeholderResolvedMethodType scope methodInfo args =
  let paramName = methodParamName methodInfo
      methodTy = methodType methodInfo
      quantifiedMethodTy = quantifiedMethodType methodInfo
      knownClassArg = knownResolvedMethodClassArg scope methodInfo args
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
  subst <- foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty knownPairs
  Map.lookup (methodParamName methodInfo) subst

knownResolvedMethodClassArg :: ElaborateScope -> MethodInfo -> [P.ResolvedExpr] -> Maybe SrcType
knownResolvedMethodClassArg scope methodInfo args = do
  let (_, bodyTy) = splitForalls (methodType methodInfo)
      (paramTys, _) = splitArrows bodyTy
      knownPairs =
        [ (templateTy, actualTy)
          | (templateTy, arg) <- zip paramTys args,
            Just actualTy <- [inferKnownResolvedExprType scope arg]
        ]
  subst <- foldM (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy) Map.empty knownPairs
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
        Just valueInfo@OrdinaryValue {} -> Just (ordinaryValueTypeInScope scope valueInfo)
        Just ConstructorValue {valueCtorInfo = ctorInfo} -> Just (constructorVisibleType scope ctorInfo)
        _ -> Nothing
    EAnn _ annTy -> Just annTy
    EApp _ _ ->
      case collectApps expr of
        (EVar name, args)
          | Just valueInfo <- Map.lookup name (esValues scope) ->
              case valueInfo of
                OrdinaryValue {}
                  | let ty = ordinaryValueTypeInScope scope valueInfo,
                    not (null args),
                    hasLeadingForall ty ->
                      Nothing
                ConstructorValue {valueCtorInfo = ctorInfo}
                  | length args == length (ctorArgs ctorInfo) ->
                      knownConstructorResultType scope ctorInfo args
                _ -> appliedValueResultType scope valueInfo (length args)
        _ -> Nothing
    _ -> Nothing

inferKnownResolvedExprType :: ElaborateScope -> P.ResolvedExpr -> Maybe SrcType
inferKnownResolvedExprType scope expr =
  case expr of
    ELit lit -> Just (litSrcType lit)
    EVar ref ->
      case runElaborateLookup (lookupResolvedValueInfo scope ref) of
        Right valueInfo@OrdinaryValue {} -> Just (ordinaryValueTypeInScope scope valueInfo)
        Right ConstructorValue {valueCtorInfo = ctorInfo} -> Just (constructorVisibleType scope ctorInfo)
        _ -> Nothing
    EAnn _ annTy -> either (const Nothing) Just (displaySrcTypeForResolved scope annTy)
    EApp _ _ ->
      case collectResolvedApps expr of
        (EVar ref, args)
          | Right valueInfo <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
              case valueInfo of
                OrdinaryValue {}
                  | let ty = ordinaryValueTypeInScope scope valueInfo,
                    not (null args),
                    hasLeadingForall ty ->
                      Nothing
                ConstructorValue {valueCtorInfo = ctorInfo}
                  | length args == length (ctorArgs ctorInfo) ->
                      knownResolvedConstructorResultType scope ctorInfo args
                _ -> appliedValueResultType scope valueInfo (length args)
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

appliedValueResultType :: ElaborateScope -> ValueInfo -> Int -> Maybe SrcType
appliedValueResultType scope valueInfo argCount =
  case valueInfo of
    OrdinaryValue {} -> peelAppliedType (ordinaryValueTypeInScope scope valueInfo) argCount
    ConstructorValue {valueCtorInfo = ctorInfo} ->
      if argCount > length (ctorArgs ctorInfo)
        then Nothing
        else peelAppliedType (constructorVisibleType scope ctorInfo) argCount
    OverloadedMethod {} -> Nothing

constructorVisibleType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorVisibleType scope ctorInfo =
  rewriteSrcTypeOccurrences (ctorResult ctorInfo) visibleResult (ctorType ctorInfo)
  where
    visibleResult =
      case resolveConstructorDataInfo scope ctorInfo of
        Just resolved -> dataHeadType (visibleDataInfo resolved)
        Nothing -> ctorResult ctorInfo

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
      validateOrderedPatterns scope alts
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
          handlers <- mapM (compileHandler scope scrutineeExpr scrutineeTy resultTy dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholder <- deferCaseCall scope dataInfo scrutineeTy resultTy
          pure (foldl surfaceApp (surfaceVar placeholder) (scrutineeExpr : handlers))

compileResolvedCase :: ElaborateScope -> Maybe SrcType -> P.ResolvedExpr -> [P.ResolvedAlt] -> ElaborateM SurfaceExpr
compileResolvedCase scope mbExpected scrutinee alts = do
  case resolvedCtorOwners alts of
    [] -> do
      let mbInferredScrutineeTy = inferKnownResolvedExprType scope scrutinee
      mbAnnotationScrutineeTy <- catchAllResolvedPatternAnnotationType scope alts
      let mbScrutineeTy =
            case mbInferredScrutineeTy of
              Just knownTy -> Just knownTy
              Nothing -> mbAnnotationScrutineeTy
          annotateScrutinee =
            case (mbInferredScrutineeTy, mbAnnotationScrutineeTy) of
              (Nothing, Just annTy) -> Just annTy
              _ -> Nothing
      mapM_ (\scrutineeTy -> mapM_ (validateResolvedPatternType scope scrutineeTy . P.altPattern) alts) mbScrutineeTy
      scrutineeExpr0 <- compileResolvedExpr scope mbScrutineeTy scrutinee
      let scrutineeExpr =
            case annotateScrutinee of
              Just annTy -> surfaceAnn scrutineeExpr0 (lowerType scope annTy)
              Nothing -> scrutineeExpr0
      compileResolvedCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts
    owners -> do
      dataInfo <- requireSingleResolvedDataOwner scope owners
      let headTy = dataHeadType dataInfo
          scrutineeTy =
            case inferKnownResolvedExprType scope scrutinee of
              Just knownTy -> knownTy
              Nothing -> headTy
      validateResolvedOrderedPatterns scope alts
      mapM_ (validateResolvedPatternType scope scrutineeTy . P.altPattern) alts
      (resultTy, _quantifyResult) <-
        case mbExpected of
          Just expectedTy -> pure (expectedTy, False)
          Nothing -> do
            resultVar <- freshTypeVarName
            pure (STVar resultVar, True)
      case localResolvedIdentityScrutinee scope scrutinee of
        Just inner -> compileResolvedCase scope mbExpected inner alts
        Nothing -> do
          scrutineeExpr <- compileResolvedExpr scope (Just scrutineeTy) scrutinee
          let forceAnnotateHandlers = any (not . null . ctorForalls) (dataConstructors dataInfo)
          handlers <- mapM (compileResolvedHandler scope scrutineeExpr scrutineeTy resultTy dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholder <- deferCaseCall scope dataInfo scrutineeTy resultTy
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

localResolvedIdentityScrutinee :: ElaborateScope -> P.ResolvedExpr -> Maybe P.ResolvedExpr
localResolvedIdentityScrutinee scope expr =
  case collectResolvedApps expr of
    (ELam param (EVar (P.ResolvedLocalValue bodyName)), [arg])
      | bodyName == P.paramName param ->
          Just arg
    (EVar (P.ResolvedLocalValue name), [arg])
      | name == "id",
        Just OrdinaryValue {valueOriginModule = "<local>"} <- Map.lookup name (esValues scope) ->
          Just arg
    _ -> Nothing

compileCatchAllOnly :: ElaborateScope -> Maybe SrcType -> Maybe SrcType -> SurfaceExpr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileExpr scope mbExpected body
      scrutineeName <- freshRuntimeName "case_scrutinee"
      case mbScrutineeTy of
        Just _ -> pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        Nothing -> do
          -- Keep the scrutinee binding referenced so eMLF infers its own scheme
          -- while the strict let still preserves evaluation before the body.
          forceName <- freshRuntimeName "case_scrutinee_force"
          pure
            ( surfaceLet
                scrutineeName
                scrutineeExpr
                (surfaceLet forceName (surfaceVar scrutineeName) bodyExpr)
            )
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

compileResolvedCatchAllOnly :: ElaborateScope -> Maybe SrcType -> Maybe SrcType -> SurfaceExpr -> [P.ResolvedAlt] -> ElaborateM SurfaceExpr
compileResolvedCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileResolvedExpr scope mbExpected body
      scrutineeName <- freshRuntimeName "case_scrutinee"
      case mbScrutineeTy of
        Just _ -> pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        Nothing -> do
          forceName <- freshRuntimeName "case_scrutinee_force"
          pure
            ( surfaceLet
                scrutineeName
                scrutineeExpr
                (surfaceLet forceName (surfaceVar scrutineeName) bodyExpr)
            )
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName name
      scope' <-
        case mbScrutineeTy of
          Just scrutineeTy -> extendLocal scope name runtimeName (Just scrutineeTy)
          Nothing -> extendLocalLowered scope name runtimeName =<< freshTypeName
      bodyExpr <- compileResolvedExpr scope' mbExpected body
      pure (surfaceLet runtimeName scrutineeExpr bodyExpr)
    [P.Alt (P.PatAnn inner _) body] -> compileResolvedCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr [P.Alt inner body]
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileHandler :: ElaborateScope -> SurfaceExpr -> SrcType -> SrcType -> DataInfo -> [P.Alt] -> Bool -> ConstructorInfo -> ElaborateM SurfaceExpr
compileHandler scope scrutineeExpr scrutineeTy resultTy dataInfo alts forceAnnotateHandlers ctorInfo = do
  let ctorArgTys = specializeConstructorArgsForScrutinee scrutineeTy ctorInfo
      specializedCtorInfo = ctorInfo {ctorArgs = ctorArgTys}
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length ctorArgTys]]
  let topArgs = zip3 (map (const P.PatWildcard) ctorArgTys) runtimeNames ctorArgTys
      candidates = matchingCandidates ctorInfo
  bodyExpr <- compileCandidates topArgs candidates
  let handlerBody =
        foldr
          (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
          bodyExpr
          (zip runtimeNames ctorArgTys)
  if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
    then pure handlerBody
    else do
      let handlerTy = handlerSurfaceType scope specializedCtorInfo (lowerType scope resultTy)
      pure (surfaceAnn handlerBody handlerTy)
  where
    matchingCandidates ctor =
      filter (patternCouldMatchConstructor scope ctor . P.altPattern) alts

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
          scope' <- extendLocalLowered scope name scrutineeName (lowerType scope scrutineeTy)
          bodyExpr <- compileExpr scope' (Just resultTy) body
          pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        P.PatCtor ctorName0 patterns
          | constructorNameMatches scope ctorName0 ctorInfo ->
              if length patterns == length (ctorArgs ctorInfo)
                then compilePatternSequence scope (zip3 patterns (map middle topArgs) (map third topArgs)) body mbFallback
                else throwError (ProgramPatternConstructorMismatch ctorName0 (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    middle (_, value, _) = value
    third (_, _, value) = value

    compilePatternSequence scope0 [] body _ =
      compileExpr scope0 (Just resultTy) body
    compilePatternSequence scope0 ((pattern0, runtimeName, argTy) : rest) body mbFallback =
      case pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          scope' <- extendLocal scope0 sourceName runtimeName (Just argTy)
          compilePatternSequence scope' rest body mbFallback
        P.PatCtor nestedCtorName nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfo scope nestedCtorName
          nestedDataInfo <- lookupDataInfoForConstructor scope nestedCtorInfo
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch nestedCtorName argTy)
            else do
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              let forceNestedAnnotations = any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
                  nestedArgTys = specializeConstructorArgsForScrutinee argTy nestedCtorInfo
              matchingBody <- compilePatternSequence scope0 (zip3 nestedPatterns nestedRuntimeNames nestedArgTys ++ rest) body mbFallback
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure (Just fallback0)
                  Nothing
                    | nestedPatternNeedsFallback nestedDataInfo nestedCtorInfo ->
                        throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
                  Nothing -> pure Nothing
              handlers <- mapM (nestedHandler forceNestedAnnotations argTy nestedCtorInfo nestedRuntimeNames matchingBody fallback) (dataConstructors nestedDataInfo)
              placeholder <- deferCaseCall scope0 nestedDataInfo argTy resultTy
              pure (foldl surfaceApp (surfaceVar placeholder) (surfaceVar runtimeName : handlers))
        P.PatAnn inner annTy -> compilePatternSequence scope0 ((inner, runtimeName, annTy) : rest) body mbFallback

    nestedPatternNeedsFallback nestedDataInfo targetCtor =
      any (not . sameConstructorInfo targetCtor) (dataConstructors nestedDataInfo)

    nestedHandler forceNestedAnnotations nestedScrutineeTy targetCtor nestedRuntimeNames matchingBody mbFallback ctor =
      let ctorArgTys = specializeConstructorArgsForScrutinee nestedScrutineeTy ctor
          specializedCtor = ctor {ctorArgs = ctorArgTys}
          targetSelected = sameConstructorInfo ctor targetCtor
          argNames = if targetSelected then nestedRuntimeNames else ["unused" ++ show ix | ix <- [1 .. length ctorArgTys]]
          selectedBody =
            case (targetSelected, mbFallback) of
              (True, _) -> matchingBody
              (False, Just fallback) -> fallback
              (False, Nothing) -> matchingBody
          handlerBody =
            foldr
              (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
              selectedBody
              (zip argNames ctorArgTys)
       in if not forceNestedAnnotations && null (ctorForalls ctor)
            then pure handlerBody
            else do
              let handlerTy = handlerSurfaceType scope specializedCtor (lowerType scope resultTy)
              pure (surfaceAnn handlerBody handlerTy)

    specializeConstructorArgsForScrutinee actualScrutineeTy ctor =
      case matchTypesInScope scope Map.empty (ctorResult ctor) actualScrutineeTy of
        Just subst -> map (specializeSrcType subst) (ctorArgs ctor)
        Nothing -> ctorArgs ctor

compileResolvedHandler :: ElaborateScope -> SurfaceExpr -> SrcType -> SrcType -> DataInfo -> [P.ResolvedAlt] -> Bool -> ConstructorInfo -> ElaborateM SurfaceExpr
compileResolvedHandler scope scrutineeExpr scrutineeTy resultTy dataInfo alts forceAnnotateHandlers ctorInfo = do
  let ctorArgTys = specializeConstructorArgsForScrutinee scrutineeTy ctorInfo
      specializedCtorInfo = ctorInfo {ctorArgs = ctorArgTys}
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length ctorArgTys]]
  let topArgs = zip3 (map (const P.PatWildcard) ctorArgTys) runtimeNames ctorArgTys
      candidates = matchingCandidates ctorInfo
  bodyExpr <- compileCandidates topArgs candidates
  let handlerBody =
        foldr
          (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
          bodyExpr
          (zip runtimeNames ctorArgTys)
  if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
    then pure handlerBody
    else do
      let handlerTy = handlerSurfaceType scope specializedCtorInfo (lowerType scope resultTy)
      pure (surfaceAnn handlerBody handlerTy)
  where
    matchingCandidates ctor =
      filter (resolvedPatternCouldMatchConstructor scope ctor . P.altPattern) alts

    compileCandidates _ [] = throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
    compileCandidates topArgs [alt] =
      compileAltCandidate topArgs alt Nothing
    compileCandidates topArgs (alt : rest) = do
      fallback <- compileCandidates topArgs rest
      compileAltCandidate topArgs alt (Just fallback)

    compileAltCandidate topArgs (P.Alt pattern0 body) mbFallback =
      case stripResolvedPatternAnn pattern0 of
        P.PatWildcard -> compileResolvedExpr scope (Just resultTy) body
        P.PatVar name -> do
          scrutineeName <- freshRuntimeName name
          scope' <- extendLocalLowered scope name scrutineeName (lowerType scope scrutineeTy)
          bodyExpr <- compileResolvedExpr scope' (Just resultTy) body
          pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        P.PatCtor ctorSymbol patterns
          | constructorSymbolMatches scope ctorSymbol ctorInfo ->
              if length patterns == length (ctorArgs ctorInfo)
                then compilePatternSequence scope (zip3 patterns (map middle topArgs) (map third topArgs)) body mbFallback
                else throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    middle (_, value, _) = value
    third (_, _, value) = value

    compilePatternSequence scope0 [] body _ =
      compileResolvedExpr scope0 (Just resultTy) body
    compilePatternSequence scope0 ((pattern0, runtimeName, argTy) : rest) body mbFallback =
      case pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          scope' <- extendLocal scope0 sourceName runtimeName (Just argTy)
          compilePatternSequence scope' rest body mbFallback
        P.PatCtor nestedCtorSymbol nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfoBySymbol scope nestedCtorSymbol
          nestedDataInfo <- lookupDataInfoForConstructor scope nestedCtorInfo
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch (P.refDisplayName nestedCtorSymbol) argTy)
            else do
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              let forceNestedAnnotations = any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
                  nestedArgTys = specializeConstructorArgsForScrutinee argTy nestedCtorInfo
              matchingBody <- compilePatternSequence scope0 (zip3 nestedPatterns nestedRuntimeNames nestedArgTys ++ rest) body mbFallback
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure (Just fallback0)
                  Nothing
                    | nestedPatternNeedsFallback nestedDataInfo nestedCtorInfo ->
                        throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
                  Nothing -> pure Nothing
              handlers <- mapM (nestedHandler forceNestedAnnotations argTy nestedCtorInfo nestedRuntimeNames matchingBody fallback) (dataConstructors nestedDataInfo)
              placeholder <- deferCaseCall scope0 nestedDataInfo argTy resultTy
              pure (foldl surfaceApp (surfaceVar placeholder) (surfaceVar runtimeName : handlers))
        P.PatAnn inner annTy -> do
          annDisplayTy <- liftEitherElab (displaySrcTypeForResolved scope annTy)
          compilePatternSequence scope0 ((inner, runtimeName, annDisplayTy) : rest) body mbFallback

    nestedPatternNeedsFallback nestedDataInfo targetCtor =
      any (not . sameConstructorInfo targetCtor) (dataConstructors nestedDataInfo)

    nestedHandler forceNestedAnnotations nestedScrutineeTy targetCtor nestedRuntimeNames matchingBody mbFallback ctor =
      let ctorArgTys = specializeConstructorArgsForScrutinee nestedScrutineeTy ctor
          specializedCtor = ctor {ctorArgs = ctorArgTys}
          targetSelected = sameConstructorInfo ctor targetCtor
          argNames = if targetSelected then nestedRuntimeNames else ["unused" ++ show ix | ix <- [1 .. length ctorArgTys]]
          selectedBody =
            case (targetSelected, mbFallback) of
              (True, _) -> matchingBody
              (False, Just fallback) -> fallback
              (False, Nothing) -> matchingBody
          handlerBody =
            foldr
              (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
              selectedBody
              (zip argNames ctorArgTys)
       in if not forceNestedAnnotations && null (ctorForalls ctor)
            then pure handlerBody
            else do
              let handlerTy = handlerSurfaceType scope specializedCtor (lowerType scope resultTy)
              pure (surfaceAnn handlerBody handlerTy)

    specializeConstructorArgsForScrutinee actualScrutineeTy ctor =
      case matchTypesInScope scope Map.empty (ctorResult ctor) actualScrutineeTy of
        Just subst -> map (specializeSrcType subst) (ctorArgs ctor)
        Nothing -> ctorArgs ctor

ctorOwners :: [P.Alt] -> [String]
ctorOwners = foldr go []
  where
    go alt acc = case P.altPattern alt of
      P.PatCtor ctorName0 _ -> ctorName0 : acc
      P.PatAnn inner _ -> go (P.Alt inner (P.altExpr alt)) acc
      _ -> acc

resolvedCtorOwners :: [P.ResolvedAlt] -> [ResolvedSymbol]
resolvedCtorOwners = foldr go []
  where
    go alt acc = case P.altPattern alt of
      P.PatCtor ctorSymbol _ -> ctorSymbol : acc
      P.PatAnn inner _ -> go (P.Alt inner (P.altExpr alt)) acc
      _ -> acc

catchAllPatternAnnotationType :: [P.Alt] -> Maybe SrcType
catchAllPatternAnnotationType alts =
  case alts of
    [P.Alt pattern0 _] -> patternAnnotationType pattern0
    _ -> Nothing

catchAllResolvedPatternAnnotationType :: ElaborateScope -> [P.ResolvedAlt] -> ElaborateM (Maybe SrcType)
catchAllResolvedPatternAnnotationType scope alts =
  case alts of
    [P.Alt pattern0 _] -> resolvedPatternAnnotationType scope pattern0
    _ -> pure Nothing

patternAnnotationType :: P.Pattern -> Maybe SrcType
patternAnnotationType pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      case patternAnnotationType inner of
        Just innerTy -> Just innerTy
        Nothing -> Just annTy
    _ -> Nothing

resolvedPatternAnnotationType :: ElaborateScope -> P.ResolvedPattern -> ElaborateM (Maybe SrcType)
resolvedPatternAnnotationType scope pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      resolvedPatternAnnotationType scope inner >>= \case
        Just innerTy -> pure (Just innerTy)
        Nothing -> Just <$> liftEitherElab (displaySrcTypeForResolved scope annTy)
    _ -> pure Nothing

validatePatternType :: ElaborateScope -> SrcType -> P.Pattern -> ElaborateM ()
validatePatternType scope expectedTy pattern0 =
  case pattern0 of
    P.PatWildcard -> pure ()
    P.PatVar {} -> pure ()
    P.PatAnn inner annTy -> do
      validatePatternAnnotation scope expectedTy annTy
      validatePatternType scope annTy inner
    P.PatCtor ctorName0 patterns -> do
      ctorInfo <- lookupConstructorInfo scope ctorName0
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
    matchPatternTypes template actual =
      case matchTypesInScope scope Map.empty template actual of
        Just subst -> Just subst
        Nothing ->
          case matchTypesInScope scope Map.empty actual template of
            Just subst -> Just subst
            Nothing
              | lowerType scope template == lowerType scope actual -> Just Map.empty
            Nothing -> Nothing

validateResolvedPatternType :: ElaborateScope -> SrcType -> P.ResolvedPattern -> ElaborateM ()
validateResolvedPatternType scope expectedTy pattern0 =
  case pattern0 of
    P.PatWildcard -> pure ()
    P.PatVar {} -> pure ()
    P.PatAnn inner annTy -> do
      annDisplayTy <- liftEitherElab (displaySrcTypeForResolved scope annTy)
      validatePatternAnnotation scope expectedTy annDisplayTy
      validateResolvedPatternType scope annDisplayTy inner
    P.PatCtor ctorSymbol patterns -> do
      ctorInfo <- lookupConstructorInfoBySymbol scope ctorSymbol
      subst <-
        case matchPatternTypes (ctorResult ctorInfo) expectedTy of
          Just subst0 -> pure subst0
          Nothing -> throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) expectedTy)
      if length patterns /= length (ctorArgs ctorInfo)
        then throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) expectedTy)
        else
          mapM_
            ( \(nestedPattern, argTy) ->
                validateResolvedPatternType scope (specializeSrcType subst argTy) nestedPattern
            )
            (zip patterns (ctorArgs ctorInfo))
  where
    matchPatternTypes template actual =
      case matchTypesInScope scope Map.empty template actual of
        Just subst -> Just subst
        Nothing ->
          case matchTypesInScope scope Map.empty actual template of
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
        || matchTypesInScope scope Map.empty left right /= Nothing
        || matchTypesInScope scope Map.empty right left /= Nothing

validateOrderedPatterns :: ElaborateScope -> [P.Alt] -> ElaborateM ()
validateOrderedPatterns scope = go Set.empty False
  where
    go _ _ [] = pure ()
    go seen catchAllSeen (P.Alt pattern0 _ : rest)
      | catchAllSeen =
          throwError (ProgramDuplicateCaseBranch (maybe "_" id (topConstructorName pattern0)))
      | isCatchAllPattern pattern0 =
          go seen True rest
      | Just (ctorName0, ctorIdentity) <- topConstructorIdentity pattern0,
        ctorIdentity `Set.member` seen =
          throwError (ProgramDuplicateCaseBranch ctorName0)
      | Just (_, ctorIdentity) <- flatCatchAllConstructor scope pattern0 =
          go (Set.insert ctorIdentity seen) False rest
      | otherwise = go seen False rest

    topConstructorIdentity pattern0 =
      case topConstructorName pattern0 of
        Just ctorName0
          | Just ctorInfo <- lookupConstructorInfoMaybe scope ctorName0 ->
              Just (ctorName0, ctorInfoSymbol ctorInfo)
        _ -> Nothing

topConstructorName :: P.Pattern -> Maybe String
topConstructorName pattern0 =
  case stripPatternAnn pattern0 of
    P.PatCtor ctorName0 _ -> Just ctorName0
    _ -> Nothing

validateResolvedOrderedPatterns :: ElaborateScope -> [P.ResolvedAlt] -> ElaborateM ()
validateResolvedOrderedPatterns scope = go Set.empty False
  where
    go _ _ [] = pure ()
    go seen catchAllSeen (P.Alt pattern0 _ : rest)
      | catchAllSeen =
          throwError (ProgramDuplicateCaseBranch (maybe "_" P.refDisplayName (topResolvedConstructorSymbol pattern0)))
      | isResolvedCatchAllPattern pattern0 =
          go seen True rest
      | Just ctorSymbol <- topResolvedConstructorSymbol pattern0,
        resolvedSymbolIdentity ctorSymbol `Set.member` seen =
          throwError (ProgramDuplicateCaseBranch (P.refDisplayName ctorSymbol))
      | Just (_, ctorIdentity) <- flatCatchAllResolvedConstructor scope pattern0 =
          go (Set.insert ctorIdentity seen) False rest
      | otherwise = go seen False rest

topResolvedConstructorSymbol :: P.ResolvedPattern -> Maybe ResolvedSymbol
topResolvedConstructorSymbol pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatCtor ctorSymbol _ -> Just ctorSymbol
    _ -> Nothing

flatCatchAllResolvedConstructor :: ElaborateScope -> P.ResolvedPattern -> Maybe (ResolvedSymbol, SymbolIdentity)
flatCatchAllResolvedConstructor scope pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatCtor ctorSymbol patterns
      | all isResolvedCatchAllPattern patterns,
        Just ctorInfo <- lookupConstructorInfoBySymbolMaybe scope ctorSymbol ->
          Just (ctorSymbol, ctorInfoSymbol ctorInfo)
    _ -> Nothing

isResolvedCatchAllPattern :: P.ResolvedPattern -> Bool
isResolvedCatchAllPattern pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    _ -> False

flatCatchAllConstructor :: ElaborateScope -> P.Pattern -> Maybe (String, SymbolIdentity)
flatCatchAllConstructor scope pattern0 =
  case stripPatternAnn pattern0 of
    P.PatCtor ctorName0 patterns
      | all isCatchAllPattern patterns,
        Just ctorInfo <- lookupConstructorInfoMaybe scope ctorName0 ->
          Just (ctorName0, ctorInfoSymbol ctorInfo)
    _ -> Nothing

isCatchAllPattern :: P.Pattern -> Bool
isCatchAllPattern pattern0 =
  case stripPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    _ -> False

patternCouldMatchConstructor :: ElaborateScope -> ConstructorInfo -> P.Pattern -> Bool
patternCouldMatchConstructor scope ctorInfo pattern0 =
  case stripPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    P.PatCtor ctorName0 _ -> constructorNameMatches scope ctorName0 ctorInfo
    P.PatAnn inner _ -> patternCouldMatchConstructor scope ctorInfo inner

resolvedPatternCouldMatchConstructor :: ElaborateScope -> ConstructorInfo -> P.ResolvedPattern -> Bool
resolvedPatternCouldMatchConstructor scope ctorInfo pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    P.PatCtor ctorSymbol _ -> constructorSymbolMatches scope ctorSymbol ctorInfo
    P.PatAnn inner _ -> resolvedPatternCouldMatchConstructor scope ctorInfo inner

stripPatternAnn :: P.Pattern -> P.Pattern
stripPatternAnn pattern0 =
  case pattern0 of
    P.PatAnn inner _ -> stripPatternAnn inner
    _ -> pattern0

stripResolvedPatternAnn :: P.ResolvedPattern -> P.ResolvedPattern
stripResolvedPatternAnn pattern0 =
  case pattern0 of
    P.PatAnn inner _ -> stripResolvedPatternAnn inner
    _ -> pattern0

requireSingleDataOwner :: ElaborateScope -> [String] -> ElaborateM DataInfo
requireSingleDataOwner scope ctorNames0 = do
  owners <- mapM (lookupConstructorInfo scope >=> lookupResolvedDataInfo) ctorNames0
  case owners of
    [] -> throwError (ProgramCaseOnNonDataType STBottom)
    owner : rest
      | all (sameDataInfo owner) rest -> pure (visibleDataInfo owner)
      | otherwise -> throwError (ProgramCaseOnNonDataType STBottom)
  where
    lookupResolvedDataInfo ctorInfo =
      case resolveConstructorDataInfo scope ctorInfo of
        Just resolved -> pure resolved
        Nothing -> throwError (ProgramUnknownType (ctorOwningType ctorInfo))

requireSingleResolvedDataOwner :: ElaborateScope -> [ResolvedSymbol] -> ElaborateM DataInfo
requireSingleResolvedDataOwner scope ctorSymbols = do
  owners <- mapM (lookupConstructorInfoBySymbol scope >=> lookupResolvedDataInfo) ctorSymbols
  case owners of
    [] -> throwError (ProgramCaseOnNonDataType STBottom)
    owner : rest
      | all (sameDataInfo owner) rest -> pure (visibleDataInfo owner)
      | otherwise -> throwError (ProgramCaseOnNonDataType STBottom)
  where
    lookupResolvedDataInfo ctorInfo =
      case resolveConstructorDataInfo scope ctorInfo of
        Just resolved -> pure resolved
        Nothing -> throwError (ProgramUnknownType (ctorOwningType ctorInfo))

lookupConstructorInfo :: ElaborateScope -> String -> ElaborateM ConstructorInfo
lookupConstructorInfo scope ctorName0 =
  case lookupConstructorInfoMaybe scope ctorName0 of
    Just ctorInfo -> pure ctorInfo
    Nothing -> throwError (ProgramUnknownConstructor ctorName0)

lookupConstructorInfoMaybe :: ElaborateScope -> String -> Maybe ConstructorInfo
lookupConstructorInfoMaybe scope ctorName0 =
  case Map.lookup ctorName0 (esValues scope) of
    Just ConstructorValue {valueCtorInfo = ctorInfo} -> Just ctorInfo
    _ -> Nothing

lookupConstructorInfoBySymbol :: ElaborateScope -> ResolvedSymbol -> ElaborateM ConstructorInfo
lookupConstructorInfoBySymbol scope symbol =
  case lookupConstructorInfoBySymbolMaybe scope symbol of
    Just ctorInfo -> pure ctorInfo
    Nothing -> throwError (ProgramUnknownConstructor (P.refDisplayName symbol))

lookupConstructorInfoBySymbolMaybe :: ElaborateScope -> ResolvedSymbol -> Maybe ConstructorInfo
lookupConstructorInfoBySymbolMaybe scope symbol =
  case lookupValueInfoBySymbol scope symbol of
    Just ConstructorValue {valueCtorInfo = ctorInfo} -> Just ctorInfo
    _ -> Nothing

lookupDataInfoForConstructor :: ElaborateScope -> ConstructorInfo -> ElaborateM DataInfo
lookupDataInfoForConstructor scope ctorInfo =
  case resolveConstructorDataInfo scope ctorInfo of
    Just resolved -> pure (visibleDataInfo resolved)
    Nothing -> throwError (ProgramUnknownType (ctorOwningType ctorInfo))

resolveConstructorDataInfo :: ElaborateScope -> ConstructorInfo -> Maybe (String, DataInfo)
resolveConstructorDataInfo scope ctorInfo =
  case Map.lookup (ctorOwningTypeIdentity ctorInfo) (esTypesByIdentity scope) of
    Just entries ->
      case find (constructorBelongsToDataInfo ctorInfo . snd) entries of
        Just match -> Just match
        Nothing -> fallback
    Nothing -> fallback
  where
    fallback =
      find
        (constructorBelongsToDataInfo ctorInfo . snd)
        (Map.toList (esTypes scope))

visibleDataInfo :: (String, DataInfo) -> DataInfo
visibleDataInfo (visibleName, info) =
  info {dataName = visibleName}

sameDataInfo :: (String, DataInfo) -> (String, DataInfo) -> Bool
sameDataInfo (_, left) (_, right) =
  dataInfoSymbolIdentity left == dataInfoSymbolIdentity right

constructorBelongsToDataInfo :: ConstructorInfo -> DataInfo -> Bool
constructorBelongsToDataInfo ctorInfo =
  any (sameConstructorInfo ctorInfo) . dataConstructors

constructorNameMatches :: ElaborateScope -> String -> ConstructorInfo -> Bool
constructorNameMatches scope ctorName0 ctorInfo =
  case lookupConstructorInfoMaybe scope ctorName0 of
    Just namedCtor -> sameConstructorInfo namedCtor ctorInfo
    Nothing -> False

constructorSymbolMatches :: ElaborateScope -> ResolvedSymbol -> ConstructorInfo -> Bool
constructorSymbolMatches scope ctorSymbol ctorInfo =
  case lookupConstructorInfoBySymbolMaybe scope ctorSymbol of
    Just namedCtor -> sameConstructorInfo namedCtor ctorInfo
    Nothing -> False

sameConstructorInfo :: ConstructorInfo -> ConstructorInfo -> Bool
sameConstructorInfo left right =
  ctorInfoSymbol left == ctorInfoSymbol right

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
  fmap (fmap (fmap typeViewDisplay)) $
    resolveInstanceInfoWithTypeView scope className0 expectedClassIdentity (sourceTypeViewInScope scope headTy)
  where
    expectedClassIdentity = classInfoIdentity <$> Map.lookup className0 (esClasses scope)

resolveInstanceInfoByIdentityWithSubst :: ElaborateScope -> ClassIdentity -> P.ClassName -> SrcType -> Either ProgramError (InstanceInfo, Map String SrcType)
resolveInstanceInfoByIdentityWithSubst scope classIdentity0 className0 =
  fmap (fmap (fmap typeViewDisplay)) . resolveInstanceInfoWithTypeView scope className0 (Just classIdentity0) . sourceTypeViewInScope scope

resolveMethodInstanceInfoWithSubst :: ElaborateScope -> MethodInfo -> SrcType -> Either ProgramError (InstanceInfo, Map String SrcType)
resolveMethodInstanceInfoWithSubst scope methodInfo =
  fmap (fmap (fmap typeViewDisplay)) . resolveMethodInstanceInfoByTypeView scope methodInfo . sourceTypeViewInScope scope

resolveInstanceInfoByConstraint :: ElaborateScope -> ConstraintInfo -> Either ProgramError (InstanceInfo, Map String TypeView)
resolveInstanceInfoByConstraint scope constraint =
  resolveInstanceInfoWithTypeView
    scope
    (constraintDisplayClass constraint)
    (Just (constraintClassSymbol constraint))
    (constraintTypeView constraint)

resolveInstanceInfoWithIdentityType :: ElaborateScope -> ClassIdentity -> P.ClassName -> TypeView -> Either ProgramError (InstanceInfo, Map String TypeView)
resolveInstanceInfoWithIdentityType scope classIdentity0 className0 =
  resolveInstanceInfoWithTypeView scope className0 (Just classIdentity0)

resolveMethodInstanceInfoByTypeView :: ElaborateScope -> MethodInfo -> TypeView -> Either ProgramError (InstanceInfo, Map String TypeView)
resolveMethodInstanceInfoByTypeView scope methodInfo =
  resolveInstanceInfoWithTypeView scope (methodClassName methodInfo) (Just (methodInfoClassIdentity methodInfo))

classInfoIdentity :: ClassInfo -> ClassIdentity
classInfoIdentity = classInfoSymbolIdentity

methodInfoClassIdentity :: MethodInfo -> ClassIdentity
methodInfoClassIdentity = methodInfoOwnerClassSymbolIdentity

resolveInstanceInfoWithTypeView ::
  ElaborateScope ->
  P.ClassName ->
  Maybe ClassIdentity ->
  TypeView ->
  Either ProgramError (InstanceInfo, Map String TypeView)
resolveInstanceInfoWithTypeView scope className0 expectedClassIdentity headView =
  case deduplicatedMatches of
    [(match, subst, _direct)] -> Right (match, subst)
    [] -> Left (ProgramNoMatchingInstance className0 (typeViewDisplay headView))
    _ -> Left (ProgramNoMatchingInstance className0 (typeViewDisplay headView))
  where
    deduplicatedMatches = deduplicateEquivalentMatches matches

    matches =
      [ (info, subst, direct)
        | info <- esInstances scope,
          instanceMatchesClassIdentity info,
          Just (subst, direct) <- [matchInstanceHead info]
      ]

    instanceMatchesClassIdentity info =
      case expectedClassIdentity of
        Just identity -> instanceInfoClassIdentity info == identity
        Nothing -> False

    instanceInfoClassIdentity info =
      instanceInfoClassSymbolIdentity info

    matchInstanceHead info =
      case matchTypeViewAgainstIdentity scope Map.empty (TypeView (instanceHeadType info) (instanceHeadIdentityType info)) headView of
        Just subst -> Just (subst, True)
        Nothing -> Nothing

    deduplicateEquivalentMatches [] = []
    deduplicateEquivalentMatches (match : rest) =
      let (equivalent, different) = partition (equivalentInstanceMatch match) rest
       in foldl preferredInstanceMatch match equivalent : deduplicateEquivalentMatches different

    equivalentInstanceMatch (left, _, _) (right, _, _) =
      instanceOriginModule left == instanceOriginModule right
        && instanceInfoClassIdentity left == instanceInfoClassIdentity right
        && instanceHeadIdentityType left == instanceHeadIdentityType right
        && map canonicalConstraint (instanceConstraints left) == map canonicalConstraint (instanceConstraints right)
        && fmap methodRuntimeIdentity (instanceMethods left) == fmap methodRuntimeIdentity (instanceMethods right)

    canonicalConstraint constraint =
      ( canonicalConstraintClassKey (P.constraintClassName constraint),
        canonicalSourceType scope (P.constraintType constraint)
      )

    canonicalConstraintClassKey classKeyName =
      case constraintClassIdentity scope classKeyName of
        Just identity -> Right identity
        Nothing -> Left classKeyName

    methodRuntimeIdentity valueInfo =
      case valueInfo of
        OrdinaryValue {valueRuntimeName = runtimeName} -> runtimeName
        ConstructorValue {valueRuntimeName = runtimeName} -> runtimeName
        OverloadedMethod {valueMethodInfo = methodInfo} -> methodRuntimeBase methodInfo

    preferredInstanceMatch left@(_, leftSubst, leftDirect) right@(_, rightSubst, rightDirect)
      | rightDirect && not leftDirect = right
      | rightDirect == leftDirect && Map.size rightSubst > Map.size leftSubst = right
      | otherwise = left

matchTypeViewAgainstIdentity :: ElaborateScope -> Map String TypeView -> TypeView -> TypeView -> Maybe (Map String TypeView)
matchTypeViewAgainstIdentity scope subst template actual =
  case typeViewIdentity template of
    STVar name ->
      case Map.lookup name subst of
        Nothing -> Just (Map.insert name actual subst)
        Just existing
          | semanticTypeEqual scope (typeViewIdentity existing) (typeViewIdentity actual) -> Just subst
          | otherwise -> Nothing
    STArrow dom cod ->
      case typeViewIdentity actual of
        STArrow dom' cod' -> do
          subst' <- matchTypeViewAgainstIdentity scope subst (childView dom dom) (childView (displayDom actual) dom')
          matchTypeViewAgainstIdentity scope subst' (childView cod cod) (childView (displayCod actual) cod')
        _ -> Nothing
    STBase expectedName ->
      case typeViewIdentity actual of
        STBase actualName
          | expectedName == actualName -> Just subst
        _ -> Nothing
    STCon expectedName args ->
      case typeViewIdentity actual of
        STCon actualName actualArgs
          | expectedName == actualName,
            length (toListNE args) == length (toListNE actualArgs) ->
              foldM
                (\acc (templateTy, actualTy) -> matchTypeViewAgainstIdentity scope acc templateTy actualTy)
                subst
                (zip (map sameView (toListNE args)) (zipWithActual actualArgs))
        _ -> Nothing
    STVarApp expectedName args ->
      matchTypeViewHeadApplication scope subst expectedName args actual
    STForall name mb body ->
      case typeViewIdentity actual of
        STForall name' mb' body'
          | name == name' -> do
              subst' <-
                case (mb, mb') of
                  (Nothing, _) -> Just subst
                  (Just bound, Just bound') -> matchTypeViewAgainstIdentity scope subst (sameView (unSrcBound bound)) (sameView (unSrcBound bound'))
                  (Just {}, Nothing) -> Nothing
              matchTypeViewAgainstIdentity scope subst' (sameView body) (sameView body')
        _ -> Nothing
    STMu name body ->
      case typeViewIdentity actual of
        STMu name' body'
          | name == name' -> matchTypeViewAgainstIdentity scope subst (sameView body) (sameView body')
        _ -> Nothing
    STBottom ->
      case typeViewIdentity actual of
        STBottom -> Just subst
        _ -> Nothing
  where
    sameView ty = TypeView ty ty

    childView display identityTy = TypeView display identityTy

    displayDom view =
      case typeViewDisplay view of
        STArrow dom _ -> dom
        _ -> typeViewDisplay view

    displayCod view =
      case typeViewDisplay view of
        STArrow _ cod -> cod
        _ -> typeViewDisplay view

    zipWithActual actualArgs =
      case typeViewDisplay actual of
        STCon _ displayArgs -> zipWith childView (toListNE displayArgs) (toListNE actualArgs)
        STVarApp _ displayArgs -> zipWith childView (toListNE displayArgs) (toListNE actualArgs)
        _ -> map sameView (toListNE actualArgs)

matchTypeViewHeadApplication ::
  ElaborateScope ->
  Map String TypeView ->
  String ->
  NonEmpty SrcType ->
  TypeView ->
  Maybe (Map String TypeView)
matchTypeViewHeadApplication scope subst expectedName expectedArgs actual =
  case typeViewIdentity actual of
    STCon actualName actualArgs ->
      matchAppliedHead (STBase actualName) (displayApplicationHead (STBase actualName) actualArgs) (toListNE actualArgs)
    STVarApp actualName actualArgs ->
      matchAppliedHead (STVar actualName) (displayApplicationHead (STVar actualName) actualArgs) (toListNE actualArgs)
    _ -> Nothing
  where
    expectedArgsList = toListNE expectedArgs
    expectedArgCount = length expectedArgsList

    matchAppliedHead identityHead (displayHead, displayArgs) identityArgs
      | length identityArgs < expectedArgCount = Nothing
      | length displayArgs /= length identityArgs = Nothing
      | otherwise = do
          let prefixLength = length identityArgs - expectedArgCount
              (identityHeadArgs, matchedIdentityArgs) = splitAt prefixLength identityArgs
              (displayHeadArgs, matchedDisplayArgs) = splitAt prefixLength displayArgs
          headIdentity <- applyTypeHead identityHead identityHeadArgs
          headDisplay <- applyTypeHead displayHead displayHeadArgs
          subst' <-
            bindTypeViewHeadVariable
              scope
              subst
              expectedName
              TypeView
                { typeViewDisplay = headDisplay,
                  typeViewIdentity = headIdentity
                }
          foldM
            (\acc (templateTy, actualTy) -> matchTypeViewAgainstIdentity scope acc templateTy actualTy)
            subst'
            (zip (map sameView expectedArgsList) (zipWith childView matchedDisplayArgs matchedIdentityArgs))

    displayApplicationHead fallbackHead fallbackArgs =
      case typeViewDisplay actual of
        STCon displayName displayArgs -> (STBase displayName, toListNE displayArgs)
        STVarApp displayName displayArgs -> (STVar displayName, toListNE displayArgs)
        _ -> (fallbackHead, toListNE fallbackArgs)

    sameView ty = TypeView ty ty

    childView display identityTy = TypeView display identityTy

bindTypeViewHeadVariable ::
  ElaborateScope ->
  Map String TypeView ->
  String ->
  TypeView ->
  Maybe (Map String TypeView)
bindTypeViewHeadVariable scope subst name view =
  case Map.lookup name subst of
    Just existing
      | semanticTypeEqual scope (typeViewIdentity existing) (typeViewIdentity view) -> Just subst
      | otherwise -> Nothing
    Nothing
      | typeViewIdentity view == STVar name -> Just subst
      | name `Set.member` freeTypeVarsTypeView view -> Nothing
      | otherwise -> Just (Map.insert name view subst)

preferVisibleSourceType :: ElaborateScope -> SrcType -> SrcType
preferVisibleSourceType scope = go
  where
    go ty =
      case ty of
        STVar {} -> ty
        STBase name -> STBase (preferVisibleTypeHeadName scope name)
        STCon name args -> STCon (preferVisibleTypeHeadName scope name) (fmap go args)
        STVarApp name args -> STVarApp name (fmap go args)
        STArrow dom cod -> STArrow (go dom) (go cod)
        STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu name body -> STMu name (go body)
        STBottom -> STBottom

preferVisibleTypeHeadName :: ElaborateScope -> String -> String
preferVisibleTypeHeadName scope name
  | Map.member name (esTypes scope) = name
  | otherwise =
      case
        [ visibleName
          | (visibleName, info) <- Map.toList (esTypes scope),
            dataName info == name || symbolDefiningName (dataInfoSymbolIdentity info) == unqualifiedSymbolName name
        ]
      of
        visibleName : _ -> visibleName
        [] -> name

rewriteSrcTypeOccurrences :: SrcType -> SrcType -> SrcType -> SrcType
rewriteSrcTypeOccurrences needle replacement = go
  where
    go ty
      | ty == needle = replacement
      | otherwise =
          case ty of
            STVar {} -> ty
            STBase {} -> ty
            STCon name args -> STCon name (fmap go args)
            STVarApp name args -> STVarApp name (fmap go args)
            STArrow dom cod -> STArrow (go dom) (go cod)
            STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
            STMu name body -> STMu name (go body)
            STBottom -> STBottom

inferClassArgument :: SrcType -> String -> [SrcType] -> Maybe SrcType
inferClassArgument methodTy classParam args =
  let (_, bodyTy) = splitForalls methodTy
      (paramTys, _) = splitArrows bodyTy
   in Map.lookup classParam
        =<< foldM (\subst (templateTy, actualTy) -> matchTypes subst templateTy actualTy) Map.empty (zip paramTys args)

matchTypes :: Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypes = matchTypesWith (==) (==)

matchTypesInScope :: ElaborateScope -> Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesInScope scope =
  matchTypesWith (semanticTypeEqual scope) (sameTypeHeadInScope scope)

matchTypesWith ::
  (SrcType -> SrcType -> Bool) ->
  (String -> String -> Bool) ->
  Map String SrcType ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchTypesWith sameType sameTypeHead subst template actual = case template of
  STVar name ->
    case Map.lookup name subst of
      Nothing -> Just (Map.insert name actual subst)
      Just existing
        | sameType existing actual -> Just subst
        | otherwise -> Nothing
  STArrow dom cod ->
    case actual of
      STArrow dom' cod' -> do
        subst' <- matchTypesWith sameType sameTypeHead subst dom dom'
        matchTypesWith sameType sameTypeHead subst' cod cod'
      _ -> Nothing
  STBase {} ->
    case actual of
      STBase {} | sameType template actual -> Just subst
      _ -> Nothing
  STCon name args ->
    case actual of
      STCon name' args'
        | sameTypeHead name name' && length (toListNE args) == length (toListNE args') ->
            foldM
              (\acc (leftTy, rightTy) -> matchTypesWith sameType sameTypeHead acc leftTy rightTy)
              subst
              (zip (toListNE args) (toListNE args'))
      _ -> Nothing
  STVarApp name args ->
    matchTypeHeadApplicationWith
      (matchTypesWith sameType sameTypeHead)
      sameType
      subst
      name
      args
      actual
  STForall name mb body ->
    case actual of
      STForall name' mb' body'
        | maybe True (\bound -> maybe False (sameType (unSrcBound bound) . unSrcBound) mb') mb && name == name' ->
            matchTypesWith sameType sameTypeHead subst body body'
      _ -> Nothing
  STMu name body ->
    case actual of
      STMu name' body'
        | name == name' -> matchTypesWith sameType sameTypeHead subst body body'
      _ -> Nothing
  STBottom ->
    case actual of
      STBottom -> Just subst
      _ -> Nothing

semanticTypeEqual :: ElaborateScope -> SrcType -> SrcType -> Bool
semanticTypeEqual scope left right =
  canonicalSourceType scope left == canonicalSourceType scope right

sameTypeHeadInScope :: ElaborateScope -> String -> String -> Bool
sameTypeHeadInScope scope left right =
  canonicalTypeHeadName scope left == canonicalTypeHeadName scope right

canonicalTypeHeadName :: ElaborateScope -> String -> String
canonicalTypeHeadName scope name =
  case Map.lookup name (esTypes scope) of
    Just info ->
      dataIdentityTypeName info
    Nothing -> name

dataIdentityTypeName :: DataInfo -> String
dataIdentityTypeName info =
  let identity = dataInfoSymbolIdentity info
   in symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

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
    go bound (STVarApp name args) =
      let headVars =
            if name `Set.member` bound
              then Set.empty
              else Set.singleton name
       in headVars `Set.union` foldMap (go bound) args
    go bound (STForall name mb body) =
      let bound' = Set.insert name bound
          mbFvs = maybe Set.empty (go bound . unSrcBound) mb
       in mbFvs `Set.union` go bound' body
    go bound (STMu name body) = go (Set.insert name bound) body

extendConstraintEvidence :: ElaborateScope -> [P.ClassConstraint] -> ElaborateM (ElaborateScope, [(String, SrcType)])
extendConstraintEvidence scope constraints = do
  constraintInfos <- mapM constraintInfoForDisplayConstraint constraints
  extendConstraintEvidenceInfo scope constraintInfos
  where
    constraintInfoForDisplayConstraint constraint = do
      classInfo <-
        case Map.lookup (P.constraintClassName constraint) (esClasses scope) of
          Just info -> pure info
          Nothing -> throwError (ProgramUnknownClass (P.constraintClassName constraint))
      pure
        ConstraintInfo
          { constraintDisplayClass = P.constraintClassName constraint,
            constraintClassSymbol = classInfoSymbolIdentity classInfo,
            constraintTypeView = sourceTypeViewInScope scope (P.constraintType constraint)
          }

extendConstraintEvidenceInfo :: ElaborateScope -> [ConstraintInfo] -> ElaborateM (ElaborateScope, [(String, SrcType)])
extendConstraintEvidenceInfo scope constraints = do
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
        case classInfoForConstraint scope constraint of
          Just info -> pure info
          Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))
      methodEntries <-
        mapM
          ( \methodInfo -> do
              runtimeName <- freshRuntimeName ("evidence_" ++ constraintDisplayClass constraint ++ "_" ++ methodName methodInfo)
              let evidenceTy =
                    lowerTypeView
                      scope
                      ( TypeView
                          (methodEvidenceSourceType scope classInfo (typeViewDisplay (constraintTypeView constraint)) methodInfo)
                          (methodEvidenceSourceType scope classInfo (typeViewIdentity (constraintTypeView constraint)) methodInfo)
                      )
              pure (methodName methodInfo, (runtimeName, evidenceTy))
          )
          (Map.elems (classMethods classInfo))
      let evidenceInfo =
            EvidenceInfo
              { evidenceClassName = constraintDisplayClass constraint,
                evidenceClassSymbol = constraintClassSymbol constraint,
                evidenceType = typeViewDisplay (constraintTypeView constraint),
                evidenceTypeIdentity = typeViewIdentity (constraintTypeView constraint),
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
  insertLocalValue sourceName valueInfo $
    scope
      { esRuntimeTypes = Map.insert runtimeName loweredTy (esRuntimeTypes scope)
      }
  where
    valueInfo =
      OrdinaryValue
        { valueDisplayName = sourceName,
          valueInfoSymbol = localValueSymbol runtimeName,
          valueRuntimeName = runtimeName,
          valueType = loweredTy,
          valueIdentityType = loweredTy,
          valueConstraints = [],
          valueConstraintInfos = [],
          valueOriginModule = "<local>"
        }

extendLocalSourceTypePure :: ElaborateScope -> String -> String -> SrcType -> ElaborateScope
extendLocalSourceTypePure scope sourceName runtimeName sourceTy =
  insertLocalValue sourceName valueInfo $
    scope
      { esRuntimeTypes = Map.insert runtimeName (lowerType scope sourceTy) (esRuntimeTypes scope)
      }
  where
    valueInfo =
      OrdinaryValue
        { valueDisplayName = sourceName,
          valueInfoSymbol = localValueSymbol runtimeName,
          valueRuntimeName = runtimeName,
          valueType = sourceTy,
          valueIdentityType = sourceTypeIdentityInScope scope sourceTy,
          valueConstraints = [],
          valueConstraintInfos = [],
          valueOriginModule = "<local>"
        }

insertLocalValue :: String -> ValueInfo -> ElaborateScope -> ElaborateScope
insertLocalValue sourceName valueInfo scope =
  scope
    { esValues =
        Map.insert
          sourceName
          valueInfo
          (esValues scope),
      esValuesByIdentity =
        Map.insertWith
          (++)
          (valueInfoSymbolIdentity valueInfo)
          [(sourceName, valueInfo)]
          (esValuesByIdentity scope)
    }

localValueSymbol :: String -> SymbolIdentity
localValueSymbol runtimeName =
  SymbolIdentity
    { symbolNamespace = SymbolValue,
      symbolDefiningModule = "<local>",
      symbolDefiningName = runtimeName,
      symbolOwnerIdentity = Nothing
    }

expectedCodomain :: Maybe SrcType -> Maybe SrcType
expectedCodomain = \case
  Just (STArrow _ cod) -> Just cod
  _ -> Nothing

mentionsFreeValue :: String -> P.Expr -> Bool
mentionsFreeValue name = elem name . collectFreeValues Set.empty

mentionsFreeResolvedValue :: String -> P.ResolvedExpr -> Bool
mentionsFreeResolvedValue name = elem name . collectFreeResolvedValues Set.empty

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

collectFreeResolvedValues :: Set String -> P.ResolvedExpr -> [String]
collectFreeResolvedValues bound expr = case expr of
  EVar (P.ResolvedLocalValue name)
    | name `Set.member` bound -> []
    | otherwise -> [name]
  EVar P.ResolvedGlobalValue {} -> []
  ELit _ -> []
  ELam param body -> collectFreeResolvedValues (Set.insert (P.paramName param) bound) body
  EApp fun arg -> collectFreeResolvedValues bound fun ++ collectFreeResolvedValues bound arg
  ELet name _ rhs body -> collectFreeResolvedValues bound rhs ++ collectFreeResolvedValues (Set.insert name bound) body
  EAnn inner _ -> collectFreeResolvedValues bound inner
  ECase scrutinee alts ->
    collectFreeResolvedValues bound scrutinee ++ concatMap collectAlt alts
  where
    collectAlt (P.Alt pattern0 body) =
      collectFreeResolvedValues (Set.union bound (Set.fromList (patternBinders pattern0))) body

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

collectResolvedApps :: P.ResolvedExpr -> (P.ResolvedExpr, [P.ResolvedExpr])
collectResolvedApps = go []
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
