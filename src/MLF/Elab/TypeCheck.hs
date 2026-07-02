{-# LANGUAGE GADTs #-}

module MLF.Elab.TypeCheck
  ( Env (..),
    ResolvedTermEnv,
    emptyEnv,
    emptyResolvedTermEnv,
    mkTypeCheckEnvWithResolvedTerms,
    insertResolvedTermBinding,
    insertTypeBindingRef,
    lookupTypeBindingRef,
    insertResolvedTermEnv,
    lookupResolvedTermEnvEntry,
    resolvedTermEnvFromList,
    resolvedTermEnvEntries,
    restrictResolvedTermBindings,
    unionEnvs,
    typeCheck,
    typeCheckWithEnv,
    typeCheckWithResolvedEnv,
    checkInstantiation,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Inst (InstEvalSpec (..), evalInstantiationWith, renameInstBoundRef, schemeToType)
import MLF.Elab.Types
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol (SymbolIdentity, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity
  ( StructuralTypeBinderRole (..),
    typeBinderIdentityStructural,
  )
import MLF.Reify.TypeOps
  ( alphaEqType,
    churchAwareEqType,
    firstNonContractiveRecursiveType,
    freeTypeVarRefsType,
    matchTypeRefs,
    substTypeCaptureRef,
  )

data Env = Env
  { typeEnv :: Map.Map TypeBinderRef ElabType,
    resolvedTermEnv :: ResolvedTermEnv
  }
  deriving (Eq, Show)

newtype FreeVarCounts = FreeVarCounts [(TypeBinderRef, Int)]

data TypeCheckEnvSummary = TypeCheckEnvSummary
  { tcesTermFreeVars :: FreeVarCounts,
    tcesTypeFreeVars :: FreeVarCounts
  }

newtype ResolvedTermEnv = ResolvedTermEnv (Map.Map ResolvedTermIdentityKey (ResolvedVar, ElabType))
  deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = mkTypeCheckEnvWithResolvedTerms [] Map.empty

mkTypeCheckEnvWithResolvedTerms :: [(ResolvedVar, ElabType)] -> Map.Map TypeBinderRef ElabType -> Env
mkTypeCheckEnvWithResolvedTerms terms types =
  Env types (resolvedTermEnvFromList terms)

insertResolvedTermBinding :: ResolvedVar -> ElabType -> Env -> Env
insertResolvedTermBinding resolved ty env =
  let resolved' = mapResolvedVarType (const ty) resolved
   in env {resolvedTermEnv = insertResolvedTermEnv resolved' ty (resolvedTermEnv env)}

insertTypeBindingRef :: TypeBinderRef -> ElabType -> Env -> Env
insertTypeBindingRef ref ty env =
  env {typeEnv = insertTypeEnvBinding ref ty (typeEnv env)}

lookupTypeBindingRef :: TypeBinderRef -> Env -> Maybe ElabType
lookupTypeBindingRef ref env =
  lookupTypeRefInMap ref (typeEnv env)

restrictResolvedTermBindings :: [ResolvedVar] -> Env -> Env
restrictResolvedTermBindings allowed env =
  env {resolvedTermEnv = restrictResolvedTermEnv allowed (resolvedTermEnv env)}

unionEnvs :: Env -> Env -> Env
unionEnvs preferred fallback =
  Env
    { typeEnv = types,
      resolvedTermEnv = overlayResolvedTermEnv (resolvedTermEnv preferred) (resolvedTermEnv fallback)
    }
  where
    types = mergeTypeEnvs (typeEnv preferred) (typeEnv fallback)

restrictResolvedTermEnv :: [ResolvedVar] -> ResolvedTermEnv -> ResolvedTermEnv
restrictResolvedTermEnv allowed (ResolvedTermEnv resolvedEnv) =
  ResolvedTermEnv (Map.restrictKeys resolvedEnv allowedKeys)
  where
    allowedKeys =
      Set.fromList (map resolvedVarIdentityKey allowed)

insertTypeEnvBinding :: TypeBinderRef -> ElabType -> Map.Map TypeBinderRef ElabType -> Map.Map TypeBinderRef ElabType
insertTypeEnvBinding ref ty =
  Map.insert ref ty . deleteTypeEnvBinding ref

deleteTypeEnvBinding :: TypeBinderRef -> Map.Map TypeBinderRef ElabType -> Map.Map TypeBinderRef ElabType
deleteTypeEnvBinding ref =
  Map.filterWithKey (\existing _ -> not (typeBinderRefsSameIdentity existing ref))

lookupTypeRefInMap :: TypeBinderRef -> Map.Map TypeBinderRef ElabType -> Maybe ElabType
lookupTypeRefInMap ref bindings =
  Map.lookup ref bindings

typeEnvContainsRef :: TypeBinderRef -> Env -> Bool
typeEnvContainsRef ref env =
  case lookupTypeBindingRef ref env of
    Just _ -> True
    Nothing -> False

mergeTypeEnvs :: Map.Map TypeBinderRef ElabType -> Map.Map TypeBinderRef ElabType -> Map.Map TypeBinderRef ElabType
mergeTypeEnvs preferred fallback =
  foldl' (\acc (ref, ty) -> insertTypeEnvBinding ref ty acc) fallback (Map.toList preferred)

typeCheck :: XmlfTerm -> Either TypeCheckError ElabType
typeCheck = typeCheckWithEnv emptyEnv

typeCheckWithEnv :: Env -> XmlfTerm -> Either TypeCheckError ElabType
typeCheckWithEnv env =
  typeCheckWithEnvSummary (summarizeTypeCheckEnv env) (resolvedTermEnv env) env

typeCheckWithResolvedEnv :: ResolvedTermEnv -> Env -> XmlfTerm -> Either TypeCheckError ElabType
typeCheckWithResolvedEnv resolvedEnv env =
  typeCheckWithEnvSummary
    (summarizeTypeCheckEnv env)
    (overlayResolvedTermEnv resolvedEnv (resolvedTermEnv env))
    env

typeCheckWithEnvSummary :: TypeCheckEnvSummary -> ResolvedTermEnv -> Env -> XmlfTerm -> Either TypeCheckError ElabType
typeCheckWithEnvSummary envSummary resolvedEnv env term = case term of
  EVarNode resolved ->
    lookupResolvedTermEnv resolvedEnv resolved
  ELit lit -> Right (litType lit)
  ELam resolved body -> do
    let ty = resolvedVarType resolved
    ensureContractiveType ty
    let envSummary' = insertResolvedTermSummary resolved ty env envSummary
        env' = insertResolvedTermBinding resolved ty env
        resolvedEnv' = insertResolvedTermEnv (mapResolvedVarType (const ty) resolved) ty resolvedEnv
    bodyTy <- typeCheckWithEnvSummary envSummary' resolvedEnv' env' body
    Right (TArrow ty bodyTy)
  EApp f a -> do
    fTy <- typeCheckWithEnvSummary envSummary resolvedEnv env f
    aTy <- typeCheckWithEnvSummary envSummary resolvedEnv env a
    case fTy of
      TArrow argTy resTy ->
        let argTy' = stripVacuousForallsDeep (inlineTypeEnvBounds env argTy)
            aTy' = stripVacuousForallsDeep (inlineTypeEnvBounds env aTy)
            peelLeadingUnboundedForalls ty = case ty of
              TForallRef _ Nothing body -> peelLeadingUnboundedForalls body
              _ -> ty
            muCompatible =
              case (argTy', aTy') of
                (expectedMu@(TMuRef expectedRef expectedBody), actualMu@(TMuRef actualRef actualBody)) ->
                  let expectedBody' = stripVacuousForallsDeep (substTypeCaptureRef expectedRef expectedMu expectedBody)
                      actualBody' = stripVacuousForallsDeep (substTypeCaptureRef actualRef actualMu actualBody)
                      expectedBodyPeeled = peelLeadingUnboundedForalls expectedBody'
                      actualBodyPeeled = peelLeadingUnboundedForalls actualBody'
                      instantiatedActual =
                        case (actualBody', expectedBody') of
                          (TForallRef resultRef Nothing resultBody, TArrow resultTy _) ->
                            Just (stripVacuousForallsDeep (substTypeCaptureRef resultRef (stripVacuousForallsDeep resultTy) resultBody))
                          _ -> Nothing
                      instantiatedExpected =
                        case (expectedBody', actualBody') of
                          (TForallRef resultRef Nothing resultBody, TArrow resultTy _) ->
                            Just (stripVacuousForallsDeep (substTypeCaptureRef resultRef (stripVacuousForallsDeep resultTy) resultBody))
                          _ -> Nothing
                   in expectedBody' == actualBody'
                        || alphaEqType expectedBody' actualBody'
                        || churchAwareEqType expectedBody' actualBody'
                        || expectedBodyPeeled == actualBodyPeeled
                        || alphaEqType expectedBodyPeeled actualBodyPeeled
                        || churchAwareEqType expectedBodyPeeled actualBodyPeeled
                        || maybe False (\ty -> expectedBody' == ty || alphaEqType expectedBody' ty || churchAwareEqType expectedBody' ty) instantiatedActual
                        || maybe False (\ty -> ty == actualBody' || alphaEqType ty actualBody' || churchAwareEqType ty actualBody') instantiatedExpected
                (expectedMu@(TMuRef expectedRef expectedBody), actualTy) ->
                  let expectedBody' = stripVacuousForallsDeep (substTypeCaptureRef expectedRef expectedMu expectedBody)
                      expectedBodyPeeled = peelLeadingUnboundedForalls expectedBody'
                      instantiatedExpected =
                        case (expectedBody', actualTy) of
                          (TForallRef resultRef Nothing resultBody, TArrow resultTy _) ->
                            let resultTy' = stripVacuousForallsDeep resultTy
                             in Just (stripVacuousForallsDeep (substTypeCaptureRef resultRef resultTy' resultBody))
                          _ -> Nothing
                   in expectedBody' == actualTy
                        || alphaEqType expectedBody' actualTy
                        || churchAwareEqType expectedBody' actualTy
                        || expectedBodyPeeled == actualTy
                        || alphaEqType expectedBodyPeeled actualTy
                        || churchAwareEqType expectedBodyPeeled actualTy
                        || maybe False (\ty -> ty == actualTy || alphaEqType ty actualTy || churchAwareEqType ty actualTy) instantiatedExpected
                _ -> False
         in if argTy' == TBottom
              || argTy' == aTy'
              || alphaEqType argTy' aTy'
              || churchAwareEqType argTy' aTy'
              || nominalStructuralMuCompatible argTy' aTy'
              || opaqueIOCompatible argTy' aTy'
              || muCompatible
              then Right resTy
              else
                case specializeFlexibleArgumentResult env argTy' aTy' resTy of
                  Just resTy' -> Right resTy'
                  Nothing -> Left (TCArgumentMismatch argTy' aTy')
      _ -> Left (TCExpectedArrow fTy)
  ELet resolved sch rhs body -> do
    ensureContractiveType (schemeToType sch)
    let schTy = schemeToType sch
        envSummary' = insertResolvedTermSummary resolved schTy env envSummary
        env' = insertResolvedTermBinding resolved schTy env
        resolvedEnv' = insertResolvedTermEnv (mapResolvedVarType (const schTy) resolved) schTy resolvedEnv
    rhsTy <- typeCheckWithEnvSummary envSummary' resolvedEnv' env' rhs
    if resolvedVarIsDiscard resolved || letSchemeAccepts rhsTy schTy
      then do
        typeCheckWithEnvSummary envSummary' resolvedEnv' env' body
      else Left (TCLetTypeMismatch rhsTy schTy)
  ETyAbsRef ref mbBound body -> do
    maybe (Right ()) (ensureContractiveType . tyToElab) mbBound
    let v = typeBinderRefName ref
        boundTy = boundType mbBound
    if any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType boundTy)
      then Left (TCTypeAbsBoundMentionsVar v)
      else
        if any (typeBinderRefsSameIdentity ref) (summaryFreeTypeVarRefs envSummary)
          then Left (TCTypeAbsVarInScope v)
          else do
            let envSummary' = insertTypeSummaryRef ref boundTy env envSummary
                env' = insertTypeBindingRef ref boundTy env
            bodyTy <- typeCheckWithEnvSummary envSummary' resolvedEnv env' body
            Right (TForallRef ref mbBound bodyTy)
  ETyInst e inst -> do
    ensureContractiveInstantiation inst
    ty <- typeCheckWithEnvSummary envSummary resolvedEnv env e
    checkInstantiation env ty inst
  ERoll recursiveTy body -> do
    ensureContractiveType recursiveTy
    case recursiveTy of
      TMuRef ref unfoldedBody -> do
        bodyTy <- typeCheckWithEnvSummary envSummary resolvedEnv env body
        let expectedBodyTy = substTypeCaptureRef ref recursiveTy unfoldedBody
            expectedBodyTyAlias = collapseRecursiveAlias ref recursiveTy expectedBodyTy
            expectedBodyTy' = stripVacuousForallsDeep expectedBodyTy
            expectedBodyTyAlias' = stripVacuousForallsDeep expectedBodyTyAlias
            bodyTy' = stripVacuousForallsDeep bodyTy
        if expectedBodyTy' == bodyTy'
          || expectedBodyTyAlias' == bodyTy'
          || alphaEqType expectedBodyTy' bodyTy'
          || alphaEqType expectedBodyTyAlias' bodyTy'
          || churchAwareEqType expectedBodyTy' bodyTy'
          || churchAwareEqType expectedBodyTyAlias' bodyTy'
          || alphaEqType (TVarRef ref) bodyTy'
          then Right recursiveTy
          else Left (TCRollBodyMismatch expectedBodyTy bodyTy)
      _ -> Left (TCExpectedRecursive recursiveTy)
  EUnroll e -> do
    ty <- typeCheckWithEnvSummary envSummary resolvedEnv env e
    case ty of
      TMuRef ref body -> Right (substTypeCaptureRef ref ty body)
      _ -> Left (TCExpectedRecursive ty)

ensureContractiveType :: ElabType -> Either TypeCheckError ()
ensureContractiveType ty = case firstNonContractiveRecursiveType ty of
  Just badTy -> Left (TCNonContractiveRecursiveType badTy)
  Nothing -> Right ()

ensureContractiveInstantiation :: Instantiation -> Either TypeCheckError ()
ensureContractiveInstantiation inst = case inst of
  InstId -> Right ()
  InstApp ty -> ensureContractiveType ty
  InstBot ty -> ensureContractiveType ty
  InstIntro -> Right ()
  InstElim -> Right ()
  InstAbstrRef _ -> Right ()
  InstUnderRef _ inner -> ensureContractiveInstantiation inner
  InstInside inner -> ensureContractiveInstantiation inner
  InstSeq a b -> ensureContractiveInstantiation a >> ensureContractiveInstantiation b

checkInstantiation :: Env -> ElabType -> Instantiation -> Either TypeCheckError ElabType
checkInstantiation env ty inst =
  let canonicalizeAppLikeInst inst0 = case inst0 of
        InstApp ty' -> InstApp ty'
        InstSeq (InstInside (InstBot ty')) InstElim -> InstApp ty'
        InstSeq (InstInside (InstApp ty')) InstElim -> InstApp ty'
        _ -> inst0
      inst' = canonicalizeAppLikeInst inst
      staleAppLikeInst inst0 = case inst0 of
        InstApp {} -> True
        InstSeq (InstInside (InstBot _)) InstElim -> True
        InstSeq (InstInside (InstApp _)) InstElim -> True
        _ -> False
   in case ty of
        TForallRef {} -> (\(_, _, ty') -> ty') <$> evalInstantiationWith spec inst' (identityGeneratorAfterType ty, env, ty)
        _ | staleAppLikeInst inst' -> Right ty
        _ -> (\(_, _, ty') -> ty') <$> evalInstantiationWith spec inst' (identityGeneratorAfterType ty, env, ty)
  where
    spec :: InstEvalSpec Env TypeCheckError
    spec =
      InstEvalSpec
        { instBot = \tArg (k, env', t) -> case t of
            TBottom -> Right (k, env', tArg)
            _ -> Left (TCInstantiationError (InstBot tArg) t ("InstBot expects TBottom, got " ++ pretty t)),
          instAbstr = \ref (k, env', t) ->
            let v = typeBinderRefName ref
             in case lookupTypeBindingRef ref env' of
                  Nothing -> Left (TCUnboundTypeVar v)
                  Just bound ->
                    if t == bound || alphaEqType t bound
                      then Right (k, env', TVarRef ref)
                      else Left (TCInstantiationError (InstAbstrRef ref) t ("InstAbstr expects bound " ++ pretty bound)),
          instElimError = \inst0 t ->
            TCInstantiationError inst0 t ("InstElim expects forall, got " ++ pretty t),
          instInsideError = \_inst0 t ->
            TCInstantiationError InstId t ("InstInside expects forall, got " ++ pretty t),
          instUnderError = \phiInst t ->
            TCInstantiationError phiInst t ("InstUnder expects forall, got " ++ pretty t),
          instElimEnv = \_ref _replacement env' -> env',
          instUnderEnv = \ref bound env' ->
            insertTypeBindingRef ref bound env',
          renameBound = renameInstBoundRef
        }

litType :: Lit -> ElabType
litType = \case
  LInt _ -> builtinLiteralType "Int"
  LBool _ -> builtinLiteralType "Bool"
  LChar _ -> builtinLiteralType "Char"
  LString _ -> builtinLiteralType "String"

builtinLiteralType :: String -> ElabType
builtinLiteralType name =
  TBaseWithIdentity (Builtins.builtinTypeHeadIdentity name) (BaseTy name)

boundType :: Maybe BoundType -> ElabType
boundType = maybe TBottom tyToElab

summarizeTypeCheckEnv :: Env -> TypeCheckEnvSummary
summarizeTypeCheckEnv env =
  TypeCheckEnvSummary
    { tcesTermFreeVars = freeVarCountsFromTypes (map snd (resolvedTermEnvEntries (resolvedTermEnv env))),
      tcesTypeFreeVars = freeVarCountsFromTypes (Map.elems (typeEnv env))
    }

insertResolvedTermSummary :: ResolvedVar -> ElabType -> Env -> TypeCheckEnvSummary -> TypeCheckEnvSummary
insertResolvedTermSummary resolved ty env summary =
  summary
    { tcesTermFreeVars =
        replaceTypeFreeVars oldTy ty (tcesTermFreeVars summary)
    }
  where
    oldTy = snd <$> lookupResolvedTermEnvEntry (resolvedTermEnv env) resolved

insertTypeSummaryRef :: TypeBinderRef -> ElabType -> Env -> TypeCheckEnvSummary -> TypeCheckEnvSummary
insertTypeSummaryRef ref ty env summary =
  summary
    { tcesTypeFreeVars =
        replaceTypeFreeVars (lookupTypeBindingRef ref env) ty (tcesTypeFreeVars summary)
    }

summaryFreeTypeVarRefs :: TypeCheckEnvSummary -> [TypeBinderRef]
summaryFreeTypeVarRefs summary =
  unionTypeRefs
    (freeVarCountsRefs (tcesTermFreeVars summary))
    (freeVarCountsRefs (tcesTypeFreeVars summary))

freeVarCountsFromTypes :: [ElabType] -> FreeVarCounts
freeVarCountsFromTypes =
  foldl' (\counts ty -> insertFreeVarRefs (freeTypeVarRefsType ty) counts) emptyFreeVarCounts

emptyFreeVarCounts :: FreeVarCounts
emptyFreeVarCounts = FreeVarCounts []

freeVarCountsRefs :: FreeVarCounts -> [TypeBinderRef]
freeVarCountsRefs (FreeVarCounts counts) = map fst counts

replaceTypeFreeVars :: Maybe ElabType -> ElabType -> FreeVarCounts -> FreeVarCounts
replaceTypeFreeVars oldTy newTy =
  insertFreeVarRefs (freeTypeVarRefsType newTy)
    . maybe id (deleteFreeVarRefs . freeTypeVarRefsType) oldTy

insertFreeVarRefs :: [TypeBinderRef] -> FreeVarCounts -> FreeVarCounts
insertFreeVarRefs refs (FreeVarCounts counts) =
  FreeVarCounts (foldl' insertOne counts refs)
  where
    insertOne [] ref = [(ref, 1)]
    insertOne ((existing, count) : rest) ref
      | typeBinderRefsSameIdentity existing ref = (existing, count + 1) : rest
      | otherwise = (existing, count) : insertOne rest ref

deleteFreeVarRefs :: [TypeBinderRef] -> FreeVarCounts -> FreeVarCounts
deleteFreeVarRefs refs (FreeVarCounts counts) =
  FreeVarCounts (foldl' deleteOne counts refs)
  where
    deleteOne [] _ = []
    deleteOne ((existing, count) : rest) ref
      | typeBinderRefsSameIdentity existing ref =
          let count' = count - 1
           in if count' <= 0 then rest else (existing, count') : rest
      | otherwise = (existing, count) : deleteOne rest ref

unionTypeRefs :: [TypeBinderRef] -> [TypeBinderRef] -> [TypeBinderRef]
unionTypeRefs left right =
  foldr insertRef right left
  where
    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

inlineTypeEnvBounds :: Env -> ElabType -> ElabType
inlineTypeEnvBounds env = go []
  where
    go seen ty = case ty of
      TVarRef ref ->
        if any (typeBinderRefsSameIdentity ref) seen
              then TVarRef ref
              else
                case lookupTypeBindingRef ref env of
                  Just bound
                    | bound /= TBottom -> go (ref : seen) bound
                  _ -> TVarRef ref
      TArrow dom cod -> TArrow (go seen dom) (go seen cod)
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap (go seen) args)
      TVarAppRef ref args -> TVarAppRef ref (fmap (go seen) args)
      TBaseWithIdentity _ _ -> ty
      TBottom -> ty
      TForallRef ref mb body ->
        let seen' = ref : seen
         in TForallRef ref (fmap (goBound seen') mb) (go seen' body)
      TMuRef ref body ->
        let seen' = ref : seen
         in TMuRef ref (go seen' body)

    goBound seen bound = case bound of
      TArrow dom cod -> TArrow (go seen dom) (go seen cod)
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap (go seen) args)
      TVarAppRef ref args -> TVarAppRef ref (fmap (go seen) args)
      TBaseWithIdentity _ _ -> bound
      TBottom -> bound
      TForallRef ref mb body ->
        let seen' = ref : seen
         in TForallRef ref (fmap (goBound seen') mb) (go seen' body)
      TMuRef ref body ->
        let seen' = ref : seen
         in TMuRef ref (go seen' body)

lookupResolvedTermEnvEntry :: ResolvedTermEnv -> ResolvedVar -> Maybe (ResolvedVar, ElabType)
lookupResolvedTermEnvEntry (ResolvedTermEnv resolvedEnv) resolved =
  Map.lookup (resolvedVarIdentityKey resolved) resolvedEnv

lookupResolvedTermEnv :: ResolvedTermEnv -> ResolvedVar -> Either TypeCheckError ElabType
lookupResolvedTermEnv resolvedEnv resolved =
  case lookupResolvedTermEnvEntry resolvedEnv resolved of
    Just (_, ty) ->
      checkedResolvedType ty
    Nothing ->
      Left (TCUnboundVar name)
  where
    name = resolvedVarReferenceName resolved

    checkedResolvedType ty
      | not (resolvedVarIsLocal resolved)
          || resolvedVarTypeMatches ty (resolvedVarType resolved) =
          Right ty
      | otherwise =
          Left (TCResolvedVarTypeMismatch name ty (resolvedVarType resolved))

insertResolvedTermEnv :: ResolvedVar -> ElabType -> ResolvedTermEnv -> ResolvedTermEnv
insertResolvedTermEnv resolved ty (ResolvedTermEnv resolvedEnv) =
  ResolvedTermEnv $
    Map.insertWith
      keepExisting
      (resolvedVarIdentityKey resolved)
      (resolved, ty)
      resolvedEnv
  where
    keepExisting _new existing = existing

emptyResolvedTermEnv :: ResolvedTermEnv
emptyResolvedTermEnv = ResolvedTermEnv Map.empty

resolvedTermEnvFromList :: [(ResolvedVar, ElabType)] -> ResolvedTermEnv
resolvedTermEnvFromList entries =
  ResolvedTermEnv
    ( Map.fromList
        [ (identity, entry)
        | (identity, entry : rest) <- Map.toList entriesByIdentity,
          all (== entry) rest
        ]
    )
  where
    entriesByIdentity =
      Map.fromListWith
        (++)
        [ (resolvedVarIdentityKey resolved, [(mapResolvedVarType (const ty) resolved, ty)])
        | (resolved, ty) <- entries
        ]

overlayResolvedTermEnv :: ResolvedTermEnv -> ResolvedTermEnv -> ResolvedTermEnv
overlayResolvedTermEnv (ResolvedTermEnv preferred) (ResolvedTermEnv fallback) =
  ResolvedTermEnv (preferred `Map.union` fallback)

resolvedTermEnvEntries :: ResolvedTermEnv -> [(ResolvedVar, ElabType)]
resolvedTermEnvEntries (ResolvedTermEnv resolvedEnv) =
  Map.elems resolvedEnv

specializeFlexibleArgumentResult :: Env -> ElabType -> ElabType -> ElabType -> Maybe ElabType
specializeFlexibleArgumentResult env expected actual result =
  case expected of
    TVarRef ref
      | not (typeEnvContainsRef ref env),
        not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType actual)) ->
          Just (substTypeCaptureRef ref actual result)
    _ -> Nothing

letSchemeAccepts :: ElabType -> ElabType -> Bool
letSchemeAccepts rhsTy schTy =
  let rhsTy' = stripVacuousForallsDeep rhsTy
      schTy' = stripVacuousForallsDeep schTy
   in rhsTy' == schTy'
        || alphaEqType rhsTy' schTy'
        || churchAwareEqType rhsTy' schTy'
        || rhsIsInstanceOfScheme rhsTy' schTy'

resolvedVarTypeMatches :: ElabType -> ElabType -> Bool
resolvedVarTypeMatches envTy resolvedTy =
  let envTy' = stripVacuousForallsDeep envTy
      resolvedTy' = stripVacuousForallsDeep resolvedTy
   in envTy' == resolvedTy'
        || alphaEqType envTy' resolvedTy'
        || churchAwareEqType envTy' resolvedTy'
        || rhsIsInstanceOfScheme resolvedTy' envTy'
        || rhsIsInstanceOfScheme envTy' resolvedTy'

rhsIsInstanceOfScheme :: ElabType -> ElabType -> Bool
rhsIsInstanceOfScheme rhsTy schTy =
  let (schBinds, schBody) = splitForallsWithRefs schTy
      (rhsBinds, rhsBody) = splitForallsWithRefs rhsTy
      schBinderRefs = map fst schBinds
      (rhsInstanceRef, _) = freshTypeBinderRef "_rhs_instance" (identityGeneratorAfterType (TArrow rhsTy schTy))
      rhsInstanceTy = TVarRef rhsInstanceRef
      sameBinderSpine =
        length schBinds == length rhsBinds
          && alphaEqType
            (rebuildForallsWithRefs schBinds rhsInstanceTy)
            (rebuildForallsWithRefs rhsBinds rhsInstanceTy)
   in case matchTypeRefs schBinderRefs schBody rhsBody of
        Right subst ->
          sameBinderSpine || schemeBindersMapToFreeRhsVars schBinderRefs rhsBody subst
        Left _ -> False

schemeBindersMapToFreeRhsVars :: [TypeBinderRef] -> ElabType -> Map.Map TypeBinderRef ElabType -> Bool
schemeBindersMapToFreeRhsVars binderRefs rhsBody subst =
  case traverse (`Map.lookup` subst) binderRefs of
    Just tys
      | Just refs <- traverse asTypeVarRef tys ->
          distinctTypeBinderRefs refs
            && all (\ref -> any (typeBinderRefsSameIdentity ref) rhsFreeRefs) refs
    _ -> False
  where
    rhsFreeRefs = freeTypeVarRefsType rhsBody

    asTypeVarRef ty =
      case ty of
        TVarRef ref -> Just ref
        _ -> Nothing

distinctTypeBinderRefs :: [TypeBinderRef] -> Bool
distinctTypeBinderRefs refs =
  case refs of
    [] -> True
    ref : rest ->
      not (any (typeBinderRefsSameIdentity ref) rest)
        && distinctTypeBinderRefs rest

splitForallsWithRefs :: ElabType -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
splitForallsWithRefs = go
  where
    go ty =
      case ty of
        TForallRef ref mb body ->
          let (binds, body') = go body
           in ((ref, mb) : binds, body')
        _ -> ([], ty)

rebuildForallsWithRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType
rebuildForallsWithRefs binds body =
  foldr (\(ref, bnd) acc -> TForallRef ref bnd acc) body binds

stripVacuousForallsDeep :: ElabType -> ElabType
stripVacuousForallsDeep ty = case ty of
  TForallRef ref mb body
    | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType body)) ->
        stripVacuousForallsDeep body
    | otherwise ->
        TForallRef ref (fmap stripVacuousForallsDeepBound mb) (stripVacuousForallsDeep body)
  TArrow dom cod -> TArrow (stripVacuousForallsDeep dom) (stripVacuousForallsDeep cod)
  TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripVacuousForallsDeep args)
  TVarAppRef ref args -> TVarAppRef ref (fmap stripVacuousForallsDeep args)
  TMuRef ref body -> TMuRef ref (stripVacuousForallsDeep body)
  _ -> ty

stripVacuousForallsDeepBound :: BoundType -> BoundType
stripVacuousForallsDeepBound bound = case bound of
  TArrow dom cod -> TArrow (stripVacuousForallsDeep dom) (stripVacuousForallsDeep cod)
  TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripVacuousForallsDeep args)
  TVarAppRef ref args -> TVarAppRef ref (fmap stripVacuousForallsDeep args)
  TForallRef ref mb body ->
    TForallRef ref (fmap stripVacuousForallsDeepBound mb) (stripVacuousForallsDeep body)
  TMuRef ref body -> TMuRef ref (stripVacuousForallsDeep body)
  _ -> bound

opaqueIOCompatible :: ElabType -> ElabType -> Bool
opaqueIOCompatible expected actual =
  case (expected, actual) of
    (TConWithIdentity expectedIdentity expectedName (_ :| []), TConWithIdentity actualIdentity actualName (_ :| [])) ->
      isOpaqueIOHead expectedIdentity expectedName && isOpaqueIOHead actualIdentity actualName
    (TArrow expectedDom expectedCod, TArrow actualDom actualCod) ->
      opaqueIODomainCompatible expectedDom actualDom
        && opaqueIOCompatible expectedCod actualCod
    _ -> False
  where
    isOpaqueIOHead (Just identity) _ =
      identity == Builtins.builtinTypeIdentity "IO"
    isOpaqueIOHead Nothing _ =
      False

    opaqueIODomainCompatible expectedDom actualDom =
      expectedDom == actualDom
        || alphaEqType expectedDom actualDom
        || churchAwareEqType expectedDom actualDom
        || case (expectedDom, actualDom) of
          (TVarRef {}, TVarRef {}) -> True
          _ -> False

nominalStructuralMuCompatible :: ElabType -> ElabType -> Bool
nominalStructuralMuCompatible expected actual =
  case (expected, actual) of
    (TBaseWithIdentity expectedIdentity expectedBase, actualMu@TMuRef {}) ->
      nominalHeadMatchesStructuralMu expectedIdentity expectedBase actualMu
    (TConWithIdentity expectedIdentity expectedBase _, actualMu@TMuRef {}) ->
      nominalHeadMatchesStructuralMu expectedIdentity expectedBase actualMu
    (expectedMu@TMuRef {}, TBaseWithIdentity actualIdentity actualBase) ->
      nominalHeadMatchesStructuralMu actualIdentity actualBase expectedMu
    (expectedMu@TMuRef {}, TConWithIdentity actualIdentity actualBase _) ->
      nominalHeadMatchesStructuralMu actualIdentity actualBase expectedMu
    _ -> False

nominalHeadMatchesStructuralMu :: Maybe SymbolIdentity -> BaseTy -> ElabType -> Bool
nominalHeadMatchesStructuralMu nominalIdentity _ (TMuRef selfRef _) =
  structuralSelfMatchesNominalIdentity nominalIdentity selfRef
nominalHeadMatchesStructuralMu _ _ _ =
  False

structuralSelfMatchesNominalIdentity :: Maybe SymbolIdentity -> TypeBinderRef -> Bool
structuralSelfMatchesNominalIdentity (Just identity) selfRef =
  case typeBinderIdentityStructural (typeBinderRefIdentity selfRef) of
    Just (unique, StructuralSelfBinder) -> unique == symbolUniqueIdentity identity
    _ -> False
structuralSelfMatchesNominalIdentity Nothing _ =
  False

collapseRecursiveAlias :: TypeBinderRef -> ElabType -> ElabType -> ElabType
collapseRecursiveAlias muRef recursiveTy = go
  where
    go ty
      | ty == recursiveTy || alphaEqType ty recursiveTy = TVarRef muRef
      | otherwise =
          case ty of
            TArrow dom cod -> TArrow (go dom) (go cod)
            TConWithIdentity identity con args -> TConWithIdentity identity con (fmap go args)
            TVarAppRef ref args -> TVarAppRef ref (fmap go args)
            TForallRef ref mb body -> TForallRef ref (fmap goBound mb) (go body)
            TMuRef ref body -> TMuRef ref (go body)
            _ -> ty

    goBound bound = case bound of
      TArrow dom cod -> TArrow (go dom) (go cod)
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap go args)
      TVarAppRef ref args -> TVarAppRef ref (fmap go args)
      TForallRef ref mb body -> TForallRef ref (fmap goBound mb) (go body)
      TMuRef ref body -> TMuRef ref (go body)
      _ -> bound
