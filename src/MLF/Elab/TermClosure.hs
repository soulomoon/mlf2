{-# LANGUAGE GADTs #-}

module MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstRefsIfNeeded,
    alignTopTyAbsToScheme,
    alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    preserveRetainedChildAuthoritativeResult,
    renameTermTypeVars,
    substInTermRefs,
  )
where

import Data.Functor.Foldable (Recursive (project), cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck (emptyEnv, emptyResolvedTermEnv, insertResolvedTermBinding, insertResolvedTermEnv, insertTypeBindingRef, typeCheck, typeCheckWithResolvedEnv)
import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, freshNameLike, matchTypeRefs, substTypeSimpleRef)

type TypeVarRename = (TypeBinderRef, TypeBinderRef)

closeTermWithSchemeSubstRefsIfNeeded :: IntMap.IntMap TypeBinderRef -> ElabScheme -> XmlfTerm -> XmlfTerm
closeTermWithSchemeSubstRefsIfNeeded subst sch term =
  let (subst', sch', renames) = freshenSchemeAndSubstAgainstTerm term subst sch
      schemeFreshened = not (null renames)
      termSubst = renameTermTypeVars renames (substInTermRefs subst' term)
      schemeTy = schemeToType sch'
      rollIfRecursiveCandidate ty
        | alphaEqType ty schemeTy = termSubst
        | churchAwareEqType ty schemeTy =
            case schemeTy of
              TMuRef {} ->
                let rolled = ERoll schemeTy termSubst
                 in case typeCheck rolled of
                      Right _ -> rolled
                      Left _ -> termSubst
              _ -> termSubst
        | otherwise = termSubst
   in case typeCheck termSubst of
        Right ty | alphaEqType ty schemeTy || churchAwareEqType ty schemeTy -> rollIfRecursiveCandidate ty
        Right _ ->
          case alignTermTypeVarsToScheme sch' termSubst of
            Just termAligned -> termAligned
            Nothing ->
              let termAlignedBody = maybe termSubst id (alignTermTypeVarsToSchemeBody sch' termSubst)
               in wrapTermWithSchemeIfMatchesOrFreshened schemeFreshened sch' termAlignedBody
        _ ->
          let termAlignedBody = maybe termSubst id (alignTermTypeVarsToSchemeBody sch' termSubst)
           in wrapTermWithSchemeIfMatchesOrFreshened schemeFreshened sch' termAlignedBody

preserveRetainedChildAuthoritativeResult :: XmlfTerm -> Maybe XmlfTerm
preserveRetainedChildAuthoritativeResult = go emptyResolvedTermEnv emptyEnv
  where
    go resolvedEnv env term = case term of
      ELet resolved sch rhs body ->
        let key = TermRefResolved resolved
         in case () of
              _
                | isTrivialRetainedChildBody key body,
                  isForallIdentityScheme sch ->
                  case typeCheckWithResolvedEnv resolvedEnv env rhs of
                    Right rhsTy
                      | hasRecursiveComponent rhsTy ->
                          Just rhs
                    _ -> descendResolved resolvedEnv env resolved sch rhs body
                | Just bodyPreserved <- preserveRetainedChildDirectBoundary resolvedEnv env key rhs body ->
                  Just bodyPreserved
                | Just bodyPreserved <- preserveRetainedChildAliasBoundary resolvedEnv env key resolved sch rhs body ->
                  Just bodyPreserved
                | otherwise -> descendResolved resolvedEnv env resolved sch rhs body
      ETyAbsRef ref mbBound body ->
        let env' =
              insertTypeBindingRef ref (maybe TBottom tyToElab mbBound) env
         in fmap (eTyAbsWithRef ref mbBound) (go resolvedEnv env' body)
      _ -> Nothing

    descendResolved resolvedEnv env resolved sch rhs body =
      let schTy = schemeToType sch
          env' = insertResolvedTermBinding resolved schTy env
          resolvedEnv' = insertResolvedTermEnv resolved schTy resolvedEnv
       in fmap (ELet resolved sch rhs) (go resolvedEnv' env' body)

    preserveRetainedChildDirectBoundary resolvedEnv env key rhs body
      | isClearBoundaryRetainedChildRhs key body =
          case typeCheckWithResolvedEnv resolvedEnv env rhs of
            Right rhsTy
              | hasRecursiveComponent rhsTy ->
                  Just rhs
            _ -> Nothing
      | otherwise = Nothing

    preserveRetainedChildAliasBoundary resolvedEnv env key resolved sch rhs body
      | isAliasFrameRhs rhs,
        hasRetainedChildAliasBoundary key body 2 =
          case typeCheckWithResolvedEnv resolvedEnv env (ELet resolved sch rhs body) of
            Left (TCLetTypeMismatch _ _) ->
              case typeCheckWithResolvedEnv resolvedEnv env rhs of
                Right rhsTy
                  | hasRecursiveComponent rhsTy ->
                      Just rhs
                _ -> Nothing
            _ -> Nothing
      | otherwise = Nothing

    hasRetainedChildAliasBoundary :: TermRefKey -> XmlfTerm -> Int -> Bool
    hasRetainedChildAliasBoundary source term remainingAliasFrames = case term of
      ELet resolved childSch childRhs childBody ->
        hasRetainedChildAliasBoundaryLet
          source
          (TermRefResolved resolved)
          childSch
          childRhs
          childBody
          remainingAliasFrames
      _ -> False

    hasRetainedChildAliasBoundaryLet ::
      TermRefKey ->
      TermRefKey ->
      ElabScheme ->
      XmlfTerm ->
      XmlfTerm ->
      Int ->
      Bool
    hasRetainedChildAliasBoundaryLet source child childSch childRhs childBody remainingAliasFrames
      | isClearBoundaryRetainedChildRhs source childRhs
          && isForallIdentityScheme childSch
          && isTrivialRetainedChildBody child childBody =
          True
      | usesTermVar source childRhs
          && remainingAliasFrames == 0
          && isAliasFrameRhs childRhs
          && hasRetainedChildClearBoundary child childBody =
          True
      | usesTermVar source childRhs
          && remainingAliasFrames > 0
          && isAliasFrameRhs childRhs
          && hasRetainedChildAliasBoundary child childBody (remainingAliasFrames - 1) =
          True
      | otherwise = False

    hasRetainedChildClearBoundary :: TermRefKey -> XmlfTerm -> Bool
    hasRetainedChildClearBoundary source term =
      hasRetainedChildClearBoundaryWithAliasBudget source term 5

    hasRetainedChildClearBoundaryWithAliasBudget :: TermRefKey -> XmlfTerm -> Int -> Bool
    hasRetainedChildClearBoundaryWithAliasBudget source term remainingAliasFrames = case term of
      ELet resolved childSch childRhs childBody ->
        hasRetainedChildClearBoundaryWithAliasBudgetLet
          source
          (TermRefResolved resolved)
          childSch
          childRhs
          childBody
          remainingAliasFrames
      _ -> False

    hasRetainedChildClearBoundaryWithAliasBudgetLet ::
      TermRefKey ->
      TermRefKey ->
      ElabScheme ->
      XmlfTerm ->
      XmlfTerm ->
      Int ->
      Bool
    hasRetainedChildClearBoundaryWithAliasBudgetLet source child childSch childRhs childBody remainingAliasFrames
      | isClearBoundaryRetainedChildRhs source childRhs
          && isForallIdentityScheme childSch
          && isTrivialRetainedChildBody child childBody =
          True
      | remainingAliasFrames > 0
          && usesTermVar source childRhs
          && isAliasFrameRhs childRhs
          && hasRetainedChildClearBoundaryWithAliasBudget child childBody (remainingAliasFrames - 1) =
          True
      | otherwise = False

newtype TermRefKey
  = TermRefResolved ResolvedVar

termRefKeyMatches :: TermRefKey -> ResolvedVar -> Bool
termRefKeyMatches key resolved =
  case key of
    TermRefResolved expected ->
      resolvedVarSameIdentity expected resolved

isTrivialRetainedChildBody :: TermRefKey -> XmlfTerm -> Bool
isTrivialRetainedChildBody v body = case body of
  EVarNode resolved -> termRefKeyMatches v resolved
  _ -> False

isForallIdentityScheme :: ElabScheme -> Bool
isForallIdentityScheme sch = case schemeToType sch of
  TForallRef ref Nothing body -> body == TVarRef ref
  _ -> False

hasRecursiveComponent :: ElabType -> Bool
hasRecursiveComponent ty = case ty of
  TMuRef _ _ -> True
  TArrow dom cod -> hasRecursiveComponent dom || hasRecursiveComponent cod
  TConWithIdentity _ _ args -> any hasRecursiveComponent args
  TForallRef _ mb body -> maybe False hasRecursiveBound mb || hasRecursiveComponent body
  _ -> False
  where
    hasRecursiveBound bound = case bound of
      TArrow dom cod -> hasRecursiveComponent dom || hasRecursiveComponent cod
      TBaseWithIdentity _ _ -> False
      TConWithIdentity _ _ args -> any hasRecursiveComponent args
      TVarAppRef _ args -> any hasRecursiveComponent args
      TForallRef _ mb body -> maybe False hasRecursiveBound mb || hasRecursiveComponent body
      TMuRef _ _ -> True
      TBottom -> False

wrapTermWithScheme :: ElabScheme -> XmlfTerm -> XmlfTerm
wrapTermWithScheme scheme term =
  foldr (\(ref, bound) acc -> eTyAbsWithRef ref bound acc) term (schemeBinderRefs scheme)

wrapTermWithSchemeIfMatchesOrFreshened :: Bool -> ElabScheme -> XmlfTerm -> XmlfTerm
wrapTermWithSchemeIfMatchesOrFreshened schemeFreshened scheme term =
  let wrapped = wrapTermWithScheme scheme term
      schemeTy = schemeToType scheme
   in case typeCheck wrapped of
        Right ty
          | alphaEqType ty schemeTy || churchAwareEqType ty schemeTy ->
              wrapped
        _ | schemeFreshened -> wrapped
        _ -> term

freshenSchemeAndSubstAgainstTerm ::
  XmlfTerm ->
  IntMap.IntMap TypeBinderRef ->
  ElabScheme ->
  (IntMap.IntMap TypeBinderRef, ElabScheme, [TypeVarRename])
freshenSchemeAndSubstAgainstTerm term subst sch =
  let reservedNames = Set.union (typeAbsNamesInTerm term) (typeVarNamesInTerm term)
      binds = schemeBinderRefs sch
      body0 = schemeBody sch
      binderNames = map (typeBinderRefName . fst) binds
      binderDomain = Set.fromList binderNames
      renameRefs =
        [ (oldRef, renameTypeBinderRef newName oldRef)
          | (oldRef, newName) <- chooseBinderRenames binderDomain reservedNames (map fst binds),
            typeBinderRefName oldRef /= newName
        ]
   in if null renameRefs
        then (subst, sch, [])
        else
          let subst' =
                IntMap.map (applyRefRenames renameRefs) subst
              binds' = renameSchemeBinds renameRefs binds
              body' = applyTypeRenames renameRefs body0
           in (subst', mkElabSchemeWithRefs binds' body', renameRefs)

chooseBinderRenames ::
  Set.Set String ->
  Set.Set String ->
  [TypeBinderRef] ->
  [(TypeBinderRef, String)]
chooseBinderRenames binderDomain = go
  where
    go _ [] = []
    go used (binderRef : rest) =
      let binder = typeBinderRefName binderRef
          needsRename = Set.member binder used
          usedForFresh = Set.union used binderDomain
          binder' =
            if needsRename
              then freshNameLike binder usedForFresh
              else binder
          used' = Set.insert binder' used
       in (binderRef, binder') : go used' rest

renameSchemeBinds ::
  [TypeVarRename] ->
  [(TypeBinderRef, Maybe BoundType)] ->
  [(TypeBinderRef, Maybe BoundType)]
renameSchemeBinds renames = go []
  where
    go _ [] = []
    go prev ((ref, mbBound) : restBinds) =
      let ref' = applyRefRenames renames ref
          refRenamed = ref /= ref'
          mbBound' = fmap (renameBound prev) mbBound
          prev'
            | refRenamed = prev ++ [(ref, ref')]
            | otherwise = prev
       in (ref', mbBound') : go prev' restBinds

renameBound :: [TypeVarRename] -> BoundType -> BoundType
renameBound renames bound =
  case elabToBound (applyTypeRenames renames (tyToElab bound)) of
    Right bound' -> bound'
    Left _ -> bound

applyTypeRenames :: [TypeVarRename] -> ElabType -> ElabType
applyTypeRenames renames ty0 =
  foldl'
    ( \ty (oldRef, newRef) ->
        substTypeSimpleRef oldRef (TVarRef newRef) ty
    )
    ty0
    renames

renameTermTypeVars :: [TypeVarRename] -> XmlfTerm -> XmlfTerm
renameTermTypeVars renames0 = go renames0
  where
    go renames term = case project term of
      EVarNodeF resolved ->
        EVarNode (mapResolvedVarType (applyTypeRenames renames) resolved)
      ELitF lit -> ELit lit
      ELamF resolved body ->
        ELam
          (mapResolvedVarType (applyTypeRenames renames) resolved)
          (go renames body)
      EAppF f a ->
        EApp (go renames f) (go renames a)
      ELetF resolved sch rhs body ->
        let resolved' = mapResolvedVarType (applyTypeRenames renames) resolved
            sch' = schemeFromType (applyTypeRenames renames (schemeToType sch))
         in ELet resolved' sch' (go renames rhs) (go renames body)
      ETyAbsFRef ref mbBound body ->
        let ref' = applyRefRenames renames ref
            mbBound' = fmap (renameBound renames) mbBound
            renamesBody = filter (not . shadowsTypeBinder ref . fst) renames
         in eTyAbsWithRef ref' mbBound' (go renamesBody body)
      ETyInstF e inst ->
        ETyInst (go renames e) (renameInst renames inst)
      ERollF ty body ->
        ERoll (applyTypeRenames renames ty) (go renames body)
      EUnrollF body ->
        EUnroll (go renames body)

alignTermTypeVarsToScheme :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
alignTermTypeVarsToScheme sch term =
  let binderRefs = map fst (schemeBinderRefs sch)
   in case typeCheck term of
        Right ty ->
          let freeRefs = freeTypeVarRefsType ty
              renames = zip freeRefs binderRefs
              termAligned = renameTermTypeVars renames term
           in case typeCheck termAligned of
                Right tyAligned
                  | alphaEqType tyAligned (schemeToType sch) -> Just termAligned
                _ -> Nothing
        Left _ -> Nothing

alignTopTyAbsToScheme :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
alignTopTyAbsToScheme sch term =
  let binds = schemeBinderRefs sch
      (topBinds, body) = splitTopTyAbs term
   in if length topBinds /= length binds
        then Nothing
        else
          let renames =
                [ (oldRef, newRef)
                  | ((oldRef, _), (newRef, _)) <- zip topBinds binds,
                    oldRef /= newRef
                ]
              body' = renameTermTypeVars renames body
              rebuilt =
                foldr
                  (\(ref, mbBound) acc -> eTyAbsWithRef ref mbBound acc)
                  body'
                  binds
           in case typeCheck rebuilt of
                Right tyAligned
                  | alphaEqType tyAligned (schemeToType sch) -> Just rebuilt
                _ -> Nothing

topTyAbsRefs :: XmlfTerm -> [TypeBinderRef]
topTyAbsRefs term = case term of
  ETyAbsRef ref _ body -> ref : topTyAbsRefs body
  _ -> []

splitTopTyAbs :: XmlfTerm -> ([(TypeBinderRef, Maybe BoundType)], XmlfTerm)
splitTopTyAbs term = case term of
  ETyAbsRef ref mbBound body ->
    let (binds, core) = splitTopTyAbs body
     in ((ref, mbBound) : binds, core)
  _ -> ([], term)

alignTermTypeVarsToTopTyAbs :: XmlfTerm -> Maybe XmlfTerm
alignTermTypeVarsToTopTyAbs term =
  let binders = topTyAbsRefs term
   in case (binders, typeCheck term) of
        ([], _) -> Nothing
        (_, Left _) -> Nothing
        (_, Right ty) ->
          let freeRefs = freeTypeVarRefsType ty
              renames = zip freeRefs binders
              termAligned = renameTermTypeVars renames term
           in case typeCheck termAligned of
                Right tyAligned
                  | null (freeTypeVarRefsType tyAligned) || length freeRefs <= length binders -> Just termAligned
                _ -> Nothing

alignTermTypeVarsToSchemeBody :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
alignTermTypeVarsToSchemeBody scheme term =
  case typeCheck term of
    Right ty ->
      case structuralRenamesToSchemeBody binderRefs body ty of
        Just renames ->
          let termAligned = renameTermTypeVars renames term
           in case typeCheck termAligned of
                Right tyAligned
                  | alphaEqType tyAligned body -> Just termAligned
                _ -> sortedFreeVarAlignment ty
        Nothing -> sortedFreeVarAlignment ty
    Left _ -> Nothing
  where
    binderRefs = map fst (schemeBinderRefs scheme)
    body = schemeBody scheme

    sortedFreeVarAlignment ty =
      let freeRefs = freeTypeVarRefsType ty
          renames = zip freeRefs binderRefs
          termAligned = renameTermTypeVars renames term
       in case typeCheck termAligned of
            Right tyAligned
              | alphaEqType tyAligned body -> Just termAligned
            _ -> Nothing

structuralRenamesToSchemeBody :: [TypeBinderRef] -> ElabType -> ElabType -> Maybe [TypeVarRename]
structuralRenamesToSchemeBody binderRefs body ty =
  case matchTypeRefs binderRefs body ty of
    Right subst ->
      let renames =
            [ (sourceRef, targetRef)
              | targetRef <- binderRefs,
                Just (TVarRef sourceRef) <- [Map.lookup targetRef subst],
                sourceRef /= targetRef
            ]
          sourceRefs = map fst renames
       in if Set.size (Set.fromList sourceRefs) == length sourceRefs
            then Just renames
            else Nothing
    Left _ -> Nothing

renameInst :: [TypeVarRename] -> Instantiation -> Instantiation
renameInst renames inst = case project inst of
  InstIdF -> InstId
  InstAppF ty -> InstApp (applyTypeRenames renames ty)
  InstBotF ty -> InstBot (applyTypeRenames renames ty)
  InstIntroF -> InstIntro
  InstElimF -> InstElim
  InstAbstrFRef ref ->
    instAbstrWithRef (applyRefRenames renames ref)
  InstUnderFRef ref inner ->
    instUnderWithRef
      (applyRefRenames renames ref)
      (renameInst renames inner)
  InstInsideF inner -> InstInside (renameInst renames inner)
  InstSeqF i1 i2 -> InstSeq (renameInst renames i1) (renameInst renames i2)

applyRefRenames :: [TypeVarRename] -> TypeBinderRef -> TypeBinderRef
applyRefRenames [] ref = ref
applyRefRenames ((oldRef, newRef) : rest) ref
  | typeBinderRefsSameIdentity ref oldRef = newRef
  | otherwise = applyRefRenames rest ref

shadowsTypeBinder :: TypeBinderRef -> TypeBinderRef -> Bool
shadowsTypeBinder = typeBinderRefsSameIdentity

isAliasFrameRhs :: XmlfTerm -> Bool
isAliasFrameRhs rhs = case rhs of
  EVarNode {} -> True
  ETyAbsRef _ _ body -> isAliasFrameRhs body
  _ -> False

isClearBoundaryRetainedChildRhs :: TermRefKey -> XmlfTerm -> Bool
isClearBoundaryRetainedChildRhs source rhs = case rhs of
  EApp f arg -> isIdentityBoundaryLambda f && usesTermVar source arg
  ETyAbsRef _ _ body -> isClearBoundaryRetainedChildRhs source body
  ETyInst e _ -> isClearBoundaryRetainedChildRhs source e
  _ -> False

isIdentityBoundaryLambda :: XmlfTerm -> Bool
isIdentityBoundaryLambda term = case term of
  ELam resolved body -> isIdentityBoundaryBody (TermRefResolved resolved) body
  ETyAbsRef _ _ body -> isIdentityBoundaryLambda body
  ETyInst e _ -> isIdentityBoundaryLambda e
  _ -> False

isIdentityBoundaryBody :: TermRefKey -> XmlfTerm -> Bool
isIdentityBoundaryBody v body = case body of
  EVarNode resolved -> termRefKeyMatches v resolved
  ETyAbsRef _ _ inner -> isIdentityBoundaryBody v inner
  ETyInst e _ -> isIdentityBoundaryBody v e
  _ -> False

usesTermVar :: TermRefKey -> XmlfTerm -> Bool
usesTermVar target = go
  where
    go term = case term of
      EVarNode resolved -> termRefKeyMatches target resolved
      ELit _ -> False
      ELam resolved body
        | termRefKeyMatches target resolved -> False
        | otherwise -> go body
      EApp f a -> go f || go a
      ELet resolved _ rhs body ->
        go rhs || (not (termRefKeyMatches target resolved) && go body)
      ETyAbsRef _ _ body -> go body
      ETyInst e _ -> go e
      ERoll _ body -> go body
      EUnroll body -> go body

typeAbsNamesInTerm :: XmlfTerm -> Set.Set String
typeAbsNamesInTerm = cata alg
  where
    alg term = case term of
      EVarNodeF _ -> Set.empty
      ELitF _ -> Set.empty
      ELamF _ body -> body
      EAppF f a -> Set.union f a
      ELetF _ _ rhs body -> Set.union rhs body
      ETyAbsFRef ref _ body -> Set.insert (typeBinderRefName ref) body
      ETyInstF e _ -> e
      ERollF _ body -> body
      EUnrollF body -> body

typeVarNamesInTerm :: XmlfTerm -> Set.Set String
typeVarNamesInTerm =
  Set.fromList . map typeBinderRefName . typeVarRefsInTerm

typeVarRefsInTerm :: XmlfTerm -> [TypeBinderRef]
typeVarRefsInTerm = cata alg
  where
    alg term = case term of
      EVarNodeF resolved -> freeTypeVarRefsType (resolvedVarType resolved)
      ELitF _ -> []
      ELamF resolved body -> freeTypeVarRefsType (resolvedVarType resolved) ++ body
      EAppF f a -> f ++ a
      ELetF resolved sch rhs body ->
        freeTypeVarRefsType (resolvedVarType resolved) ++ freeTypeVarRefsType (schemeToType sch) ++ rhs ++ body
      ETyAbsFRef ref mb body -> ref : (maybe [] freeTypeVarRefsType mb ++ body)
      ETyInstF e inst -> e ++ typeVarRefsInst inst
      ERollF ty body -> freeTypeVarRefsType ty ++ body
      EUnrollF body -> body

typeVarRefsInst :: Instantiation -> [TypeBinderRef]
typeVarRefsInst inst = case project inst of
  InstIdF -> []
  InstAppF ty -> freeTypeVarRefsType ty
  InstIntroF -> []
  InstElimF -> []
  InstInsideF inner -> typeVarRefsInst inner
  InstSeqF a b -> typeVarRefsInst a ++ typeVarRefsInst b
  InstUnderFRef _ inner -> typeVarRefsInst inner
  InstBotF ty -> freeTypeVarRefsType ty
  InstAbstrFRef _ -> []

substInTermRefs :: IntMap.IntMap TypeBinderRef -> XmlfTerm -> XmlfTerm
substInTermRefs subst = cata alg
  where
    alg term = case term of
      EVarNodeF resolved -> EVarNode (mapResolvedVarType (substInTyRefs subst) resolved)
      ELitF l -> ELit l
      ELamF resolved body -> ELam (mapResolvedVarType (substInTyRefs subst) resolved) body
      EAppF f a -> EApp f a
      ELetF resolved sch rhs body ->
        ELet
          (mapResolvedVarType (substInTyRefs subst) resolved)
          (substInSchemeRefs subst sch)
          rhs
          body
      ETyAbsFRef ref b body ->
        eTyAbsWithRef (applySubstRefByMap subst ref) (fmap (substInTyRefs subst) b) body
      ETyInstF e i -> ETyInst e (substInInstRefs subst i)
      ERollF ty body -> ERoll (substInTyRefs subst ty) body
      EUnrollF body -> EUnroll body

substInTyRefs :: IntMap.IntMap TypeBinderRef -> Ty v -> Ty v
substInTyRefs subst = cataIx alg
  where
    alg :: TyIF i Ty -> Ty i
    alg node = case node of
      TVarIFRef ref -> TVarRef (applySubstRef ref)
      TVarAppIFRef ref args -> TVarAppRef (applySubstRef ref) args
      TArrowIF d c -> TArrow d c
      TConIFWithIdentity identity c args -> TConWithIdentity identity c args
      TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
      TForallIFRef ref mb body -> TForallRef (applySubstRef ref) mb body
      TMuIFRef ref body -> TMuRef (applySubstRef ref) body
      TBottomIF -> TBottom

    applySubstRef = applySubstRefByMap subst

applySubstRefByMap :: IntMap.IntMap TypeBinderRef -> TypeBinderRef -> TypeBinderRef
applySubstRefByMap subst ref =
  case typeBinderRefNode ref of
    Just (NodeId nid) ->
      case IntMap.lookup nid subst of
        Just ref' -> ref'
        Nothing -> ref
    Nothing -> ref

substInSchemeRefs :: IntMap.IntMap TypeBinderRef -> ElabScheme -> ElabScheme
substInSchemeRefs subst scheme =
  schemeFromType (substInTyRefs subst (schemeToType scheme))

substInInstRefs :: IntMap.IntMap TypeBinderRef -> Instantiation -> Instantiation
substInInstRefs subst = cata alg
  where
    alg inst = case inst of
      InstIdF -> InstId
      InstAppF t -> InstApp (substInTyRefs subst t)
      InstBotF t -> InstBot (substInTyRefs subst t)
      InstIntroF -> InstIntro
      InstElimF -> InstElim
      InstAbstrFRef ref -> instAbstrWithRef (applySubstRefByMap subst ref)
      InstUnderFRef ref i' -> instUnderWithRef (applySubstRefByMap subst ref) i'
      InstInsideF i' -> InstInside i'
      InstSeqF i1 i2 -> InstSeq i1 i2
