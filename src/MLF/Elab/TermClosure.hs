{-# LANGUAGE GADTs #-}

module MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstRefsIfNeeded,
    constructTermWithSchemeSubstRefs,
    constructTermWithSchemeSubstRefsByBinderRoutes,
    etaExpandTermToSchemeSubstRefs,
    alignTopTyAbsToScheme,
    alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    preserveRetainedChildAuthoritativeResult,
    refreshLocalResolvedVarType,
    renameTermTypeBinderRefPayloads,
    renameTypeBinderRefPayloads,
    renameTermTypeVars,
    substInTermRefs,
  )
where

import Data.Functor.Foldable (Recursive (project), cata)
import Data.Either (isRight)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck (Env, emptyEnv, emptyResolvedTermEnv, insertResolvedTermBinding, insertResolvedTermEnv, insertTypeBindingRef, mkTypeCheckEnvWithResolvedTerms, typeCheckWithEnv, typeCheckWithResolvedEnv)
import MLF.Elab.Types
import MLF.Frontend.Syntax (Lit (LInt))
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, freshNameLike, matchTypeRefs, substTypeCaptureRef, substTypeSimpleRef)

type TypeVarRename = (TypeBinderRef, TypeBinderRef)

binderBoundsAgree :: Maybe BoundType -> Maybe BoundType -> Bool
binderBoundsAgree Nothing Nothing = True
binderBoundsAgree (Just left) (Just right) =
  let leftTy = tyToElab left
      rightTy = tyToElab right
   in alphaEqType leftTy rightTy
        || churchAwareEqType leftTy rightTy
binderBoundsAgree _ _ = False

closeTermWithSchemeSubstRefsIfNeeded :: IntMap.IntMap TypeBinderRef -> ElabScheme -> XmlfTerm -> XmlfTerm
closeTermWithSchemeSubstRefsIfNeeded subst sch term =
  let (subst', sch', renames) = freshenSchemeAndSubstAgainstTerm term subst sch
      termSubst = renameTermTypeVars renames (substInTermRefs subst' term)
      schemeTy = schemeToType sch'
      rollIfRecursiveCandidate ty
        | alphaEqType ty schemeTy = termSubst
        | muTy@(TMuRef muRef muBody) <- schemeTy,
          let unfoldedTy = substTypeCaptureRef muRef muTy muBody,
          alphaEqType ty unfoldedTy =
            let rolled = ERoll muTy termSubst
             in case typeCheckOpenTerm rolled of
                  Right _ -> rolled
                  Left _ -> termSubst
        | otherwise = termSubst
   in case alignTopTyAbsToScheme sch' termSubst of
        Just termAligned -> termAligned
        Nothing ->
          case typeCheckOpenTerm termSubst of
            Right ty
              | null (schemeBinderRefs sch'),
                alphaEqType ty schemeTy || churchAwareEqType ty schemeTy ->
                  rollIfRecursiveCandidate ty
            Right _ ->
              case alignTermTypeVarsToScheme sch' termSubst of
                Just termAligned -> termAligned
                Nothing ->
                  let termAlignedBody = maybe termSubst id (alignTermTypeVarsToSchemeBody sch' termSubst)
                   in wrapTermWithSchemeIfMatches sch' termAlignedBody
            _ ->
              let termAlignedBody = maybe termSubst id (alignTermTypeVarsToSchemeBody sch' termSubst)
               in wrapTermWithSchemeIfMatches sch' termAlignedBody

-- | Close a producer that was already elaborated under the prepared Gamma.
-- The scheme is construction authority here, not a candidate inferred from
-- the finished term, so emit its abstraction spine unconditionally.  Exact
-- roots use this before applying their compiler-owned specialization; the
-- validation at that boundary checks the resulting xMLF construction.
constructTermWithSchemeSubstRefs
  :: IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> XmlfTerm
constructTermWithSchemeSubstRefs subst scheme term =
  let (subst', scheme', renames) =
        freshenSchemeAndSubstAgainstTerm term subst scheme
      termSubst = renameTermTypeVars renames (substInTermRefs subst' term)
   in wrapTermWithScheme scheme' termSubst

-- | Construct a root scheme around an open producer without identifying
-- unrelated leading abstractions by position.  A same-identity abstraction,
-- or one related to a root binder by an explicit construction route, is
-- reused exactly once; every other leading abstraction remains an inner,
-- locally owned Gamma.
--
-- This is the exact-root construction rule.  For example, closing
-- @Lambda a. ...@ with a scheme that also owns @a@ must not duplicate it,
-- while closing @Lambda e >= tau. ... Hyp(e)@ with @forall a. ...@ must
-- produce @Lambda a. Lambda e >= tau. ... Hyp(e)@ rather than rename @e@ to
-- the unbounded @a@.
constructTermWithSchemeSubstRefsByBinderRoutes
  :: [(TypeBinderRef, TypeBinderRef)]
  -> IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> XmlfTerm
constructTermWithSchemeSubstRefsByBinderRoutes binderRoutes subst scheme term =
  let (subst', scheme', renames) =
        freshenSchemeAndSubstAgainstTerm term subst scheme
      termSubst = renameTermTypeVars renames (substInTermRefs subst' term)
      rootBinders = schemeBinderRefs scheme'
      (existingBinders, body) = splitTopTyAbs termSubst
   in case partitionExistingBinders rootBinders existingBinders of
        Just (localBinders, reusedRenames) ->
          let localTerm =
                foldr
                  (\(ref, mbBound) acc -> eTyAbsWithRef ref mbBound acc)
                  body
                  localBinders
              localTerm' = renameTermTypeVars reusedRenames localTerm
           in wrapTermWithScheme scheme' localTerm'
        -- Conflicting or out-of-order same-identity declarations cannot be
        -- repaired by adding another abstraction with that identity.  Keep
        -- the checked producer intact so the exact-boundary validator reports
        -- the authority mismatch without first manufacturing an invalid term.
        Nothing -> termSubst
  where
    partitionExistingBinders rootBinders =
      go rootBinders
      where
        go _ [] = Just ([], [])
        go remaining (binder@(existingRef, existingBound) : rest) =
          case
              break
                (\(rootRef, _) -> existingRef `constructsRootBinder` rootRef)
                remaining
            of
            (_, (rootRef, rootBound) : remainingAfter)
              | binderBoundsAgree existingBound rootBound -> do
                  (localBinders, renames) <- go remainingAfter rest
                  pure
                    ( localBinders
                    , [ (existingRef, rootRef)
                      | existingRef /= rootRef
                      ]
                        ++ renames
                    )
              | otherwise -> Nothing
            _
              | any
                  (\(rootRef, _) ->
                    existingRef `constructsRootBinder` rootRef
                  )
                  rootBinders ->
                  -- The identity was already consumed, so this is either a
                  -- duplicate declaration or an order inversion.
                  Nothing
              | otherwise -> do
                  (localBinders, renames) <- go remaining rest
                  pure (binder : localBinders, renames)

    existingRef `constructsRootBinder` rootRef =
      typeBinderRefsSameIdentity existingRef rootRef
        || any
          (\(sourceRef, targetRef) ->
            typeBinderRefsSameIdentity existingRef sourceRef
              && typeBinderRefsSameIdentity rootRef targetRef
          )
          binderRoutes

-- | Make a polymorphic value explicit at a let/publication boundary.  A term
-- may already have the complete scheme type because an application returns a
-- polymorphic value; xMLF publication still requires the scheme binder to be
-- represented by the matching type abstraction.  Abstract the value to the
-- scheme body (using its own binder identity) and then emit that abstraction.
etaExpandTermToSchemeSubstRefs
  :: IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> XmlfTerm
etaExpandTermToSchemeSubstRefs subst scheme term =
  let (subst', scheme', renames) =
        freshenSchemeAndSubstAgainstTerm term subst scheme
      termSubst = renameTermTypeVars renames (substInTermRefs subst' term)
   in wrapTermWithSchemeIfMatches scheme' termSubst

preserveRetainedChildAuthoritativeResult :: XmlfTerm -> Maybe XmlfTerm
preserveRetainedChildAuthoritativeResult = go emptyResolvedTermEnv emptyEnv
  where
    go resolvedEnv env term = case term of
      ELet resolved sch rhs body ->
        let key = resolved
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
          resolved
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
          resolved
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

type TermRefKey = ResolvedVar

termRefKeyMatches :: TermRefKey -> ResolvedVar -> Bool
termRefKeyMatches = resolvedVarSameIdentity

-- | Change a local binder's carried type together with every occurrence that
-- refers to the same resolved identity.  Keeping this operation here prevents
-- term-shape alignment from producing a binder/occurrence type mismatch.
refreshLocalResolvedVarType :: ResolvedVar -> ElabType -> XmlfTerm -> XmlfTerm
refreshLocalResolvedVarType target ty = go
  where
    matches = resolvedVarSameIdentity target

    go term =
      case term of
        EVarNode resolved
          | matches resolved -> EVarNode (mapResolvedVarType (const ty) resolved)
          | otherwise -> term
        ELit {} -> term
        ELam resolved body
          | matches resolved -> ELam resolved body
          | otherwise -> ELam resolved (go body)
        EApp fun arg -> EApp (go fun) (go arg)
        ELet resolved scheme rhs body
          | matches resolved -> ELet resolved scheme rhs body
          | otherwise -> ELet resolved scheme (go rhs) (go body)
        ETyAbsRef ref mbBound body -> ETyAbsRef ref mbBound (go body)
        ETyInst inner inst -> ETyInst (go inner) inst
        ERoll rollTy body -> ERoll rollTy (go body)
        EUnroll body -> EUnroll (go body)

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

wrapTermWithSchemeIfMatches :: ElabScheme -> XmlfTerm -> XmlfTerm
wrapTermWithSchemeIfMatches scheme term =
  let abstracted = abstractTermToSchemeBody scheme term
      termAbstracted = maybe term id abstracted
      wrapped = wrapTermWithScheme scheme termAbstracted
      schemeTy = schemeToType scheme
   in case typeCheckOpenTerm wrapped of
        Right ty
          | alphaEqType ty schemeTy || churchAwareEqType ty schemeTy ->
              wrapped
        _ -> term

-- | Construct the explicit xMLF coercions required by a generalized scheme.
-- A flexible binder @a >= bound@ is introduced by 'ETyAbsRef'; wherever the
-- inferred term still produces @bound@ but the scheme body requires @a@,
-- 'InstAbstrRef' is the proof that raises that result to @a@. Lambda
-- codomains are constructed earlier by their Figure 15.3.5 body edges; this
-- closure layer must not descend through an 'ELam' and retrofit one.
abstractTermToSchemeBody :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
abstractTermToSchemeBody scheme term = do
  actualTy <- either (const Nothing) Just (typeCheckOpenTerm term)
  abstractTermToType
    (schemeBinderRefs scheme)
    actualTy
    (schemeBody scheme)
    term

abstractTermToType :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType -> XmlfTerm -> Maybe XmlfTerm
abstractTermToType binders actualTy expectedTy term
  | alphaEqType actualTy expectedTy || churchAwareEqType actualTy expectedTy =
      Just term
  | ELet resolved scheme rhs body <- term =
      ELet resolved scheme rhs
        <$> abstractTermToType binders actualTy expectedTy body
  | TForallRef ref (Just bound) bodyTy <- actualTy,
    let boundTy = tyToElab bound
        instantiatedTy = substTypeSimpleRef ref boundTy bodyTy,
    alphaEqType instantiatedTy expectedTy
      || churchAwareEqType instantiatedTy expectedTy =
      -- Closing a polymorphic term against a monomorphic scheme is an
      -- explicit specialization boundary.  Construct the application from
      -- the target type itself; plain InstElim is reserved for witness replay
      -- where OpWeaken selects the already-carried flexible bound.
      Just (ETyInst term (InstApp boundTy))
  | TVarRef expectedRef <- expectedTy,
    Just (_, mbBound) <- find (typeBinderRefsSameIdentity expectedRef . fst) binders,
    let boundTy = maybe TBottom tyToElab mbBound,
    alphaEqType actualTy boundTy || churchAwareEqType actualTy boundTy =
      Just (ETyInst term (instAbstrWithRef expectedRef))
  | TForallRef actualRef actualBound actualBody <- actualTy,
    [(expectedRef, _)] <-
      [ (candidateRef, candidateBound)
      | (candidateRef, candidateBound) <- binders
      , let specializedBody =
              substTypeCaptureRef
                actualRef
                (TVarRef candidateRef)
                actualBody
      , alphaEqType specializedBody expectedTy
          || churchAwareEqType specializedBody expectedTy
      , forallBoundsCanRebind actualBound candidateBound
      ] =
      Just
        ( ETyInst
            term
            (forallRebindingInstantiation actualBound expectedRef)
        )
  | otherwise = Nothing
  where
    forallBoundsCanRebind Nothing _ = True
    forallBoundsCanRebind (Just actual) (Just expected) =
      let actualTy' = tyToElab actual
          expectedTy' = tyToElab expected
       in alphaEqType actualTy' expectedTy'
            || churchAwareEqType actualTy' expectedTy'
    forallBoundsCanRebind (Just _) Nothing = False

    forallRebindingInstantiation Nothing expectedRef =
      InstApp (TVarRef expectedRef)
    forallRebindingInstantiation (Just _) expectedRef =
      InstSeq
        (InstInside (instAbstrWithRef expectedRef))
        InstElim

freshenSchemeAndSubstAgainstTerm ::
  XmlfTerm ->
  IntMap.IntMap TypeBinderRef ->
  ElabScheme ->
  (IntMap.IntMap TypeBinderRef, ElabScheme, [TypeVarRename])
freshenSchemeAndSubstAgainstTerm term subst sch =
  let reservedRefs = typeVarRefsInTerm term
      binds = schemeBinderRefs sch
      body0 = schemeBody sch
      binderNames = map (typeBinderRefName . fst) binds
      binderDomain = Set.fromList binderNames
      renameRefs =
        [ (oldRef, renameTypeBinderRef newName oldRef)
          | (oldRef, newName) <- chooseBinderRenames binderDomain reservedRefs (map fst binds),
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
  [TypeBinderRef] ->
  [TypeBinderRef] ->
  [(TypeBinderRef, String)]
chooseBinderRenames binderDomain reservedRefs = go Set.empty
  where
    reservedNames = Set.fromList (map typeBinderRefName reservedRefs)

    go _ [] = []
    go used (binderRef : rest) =
      let binder = typeBinderRefName binderRef
          conflictsWithReserved ref =
            typeBinderRefName ref == binder
              && not (typeBinderRefsSameIdentity ref binderRef)
          needsRename =
            Set.member binder used
              || any conflictsWithReserved reservedRefs
          usedForFresh = Set.unions [used, binderDomain, reservedNames]
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
          refRenamed = not (typeBinderRefsSameIdentityAndName ref ref')
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

-- | Rename the display payload of selected type-binder identities everywhere
-- in a term.  Unlike substitution, this operation intentionally crosses the
-- matching lexical binder: every replacement preserves its identity, so the
-- declaration, its bound, all scoped occurrences, instantiations, and carried
-- schemes must change together.
renameTermTypeBinderRefPayloads :: [TypeVarRename] -> XmlfTerm -> XmlfTerm
renameTermTypeBinderRefPayloads renames = renameTerm
  where
    renameRef = applyRefRenames renames
    renameType = renameTypeBinderRefPayloads renames

    renameTerm term =
      case term of
        EVarNode resolved ->
          EVarNode (mapResolvedVarType renameType resolved)
        ELit lit -> ELit lit
        ELam resolved body ->
          ELam
            (mapResolvedVarType renameType resolved)
            (renameTerm body)
        EApp fun arg -> EApp (renameTerm fun) (renameTerm arg)
        ELet resolved scheme rhs body ->
          ELet
            (mapResolvedVarType renameType resolved)
            (renameScheme scheme)
            (renameTerm rhs)
            (renameTerm body)
        ETyAbsRef ref mbBound body ->
          ETyAbsRef
            (renameRef ref)
            (fmap (mapBoundType renameType) mbBound)
            (renameTerm body)
        ETyInst body inst ->
          ETyInst (renameTerm body) (renameInstantiation inst)
        ERoll ty body -> ERoll (renameType ty) (renameTerm body)
        EUnroll body -> EUnroll (renameTerm body)

    renameScheme scheme =
      mkElabSchemeWithRefs
        [ (renameRef ref, fmap (mapBoundType renameType) mbBound)
        | (ref, mbBound) <- schemeBinderRefs scheme
        ]
        (renameType (schemeBody scheme))

    renameInstantiation inst =
      case inst of
        InstId -> InstId
        InstApp ty -> InstApp (renameType ty)
        InstBot ty -> InstBot (renameType ty)
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstAbstrRef ref -> InstAbstrRef (renameRef ref)
        InstUnderRef ref inner ->
          InstUnderRef (renameRef ref) (renameInstantiation inner)
        InstInside inner -> InstInside (renameInstantiation inner)
        InstSeq left right ->
          InstSeq
            (renameInstantiation left)
            (renameInstantiation right)

-- | Rename only a type-binder reference's presentation while preserving its
-- identity and lexical structure.
renameTypeBinderRefPayloads :: [TypeVarRename] -> ElabType -> ElabType
renameTypeBinderRefPayloads renames = renameType
  where
    renameRef = applyRefRenames renames

    renameType ty =
      case ty of
        TVarRef ref -> TVarRef (renameRef ref)
        TVarAppRef ref args ->
          TVarAppRef (renameRef ref) (fmap renameType args)
        TArrow domain codomain ->
          TArrow (renameType domain) (renameType codomain)
        TConWithIdentity identity constructor args ->
          TConWithIdentity identity constructor (fmap renameType args)
        TBaseWithIdentity identity base -> TBaseWithIdentity identity base
        TForallRef ref mbBound body ->
          TForallRef
            (renameRef ref)
            (fmap (mapBoundType renameType) mbBound)
            (renameType body)
        TMuRef ref body -> TMuRef (renameRef ref) (renameType body)
        TBottom -> TBottom

alignTermTypeVarsToScheme :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
alignTermTypeVarsToScheme sch term =
  let binderRefs = map fst (schemeBinderRefs sch)
   in case typeCheckOpenTerm term of
        Right ty ->
          let freeRefs = freeTypeVarRefsType ty
              renames = zip freeRefs binderRefs
              termAligned = renameTermTypeVars renames term
           in case typeCheckOpenTerm termAligned of
                Right tyAligned
                  | alphaEqType tyAligned (schemeToType sch)
                  , syntacticallyConstructsSchemeForalls
                      binderRefs
                      termAligned ->
                      Just termAligned
                _ -> Nothing
        Left _ -> Nothing
  where
    syntacticallyConstructsSchemeForalls expectedRefs =
      go expectedRefs
      where
        go [] _ = True
        go (expectedRef : rest) (ETyAbsRef actualRef _ body)
          | typeBinderRefsSameIdentity expectedRef actualRef =
              go rest body
        go _ _ = False

alignTopTyAbsToScheme :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
alignTopTyAbsToScheme sch term =
  let binds = schemeBinderRefs sch
      (topBinds, body) = splitTopTyAbs term
      identityMatches =
        matchExistingBinderSubsequence topBinds binds
      hasSharedIdentity =
        any
          (\(existingRef, _) ->
            any
              (typeBinderRefsSameIdentity existingRef . fst)
              binds
          )
          topBinds
   in case identityMatches of
        Just matched
          | hasSharedIdentity ->
              validateRebuilt
                ( rebuild binds body
                    [ (oldRef, expectedRef)
                    | ((oldRef, _), (expectedRef, _)) <- matched
                    , oldRef /= expectedRef
                    ]
                )
        _
          | hasSharedIdentity -> Nothing
          | otherwise -> alignPositionally binds topBinds body
  where
    rebuild binds body renames =
      foldr
        (\(ref, mbBound) acc -> eTyAbsWithRef ref mbBound acc)
        (renameTermTypeVars renames body)
        binds

    alignPositionally binds topBinds body
      | existingCount > length binds = Nothing
      | otherwise =
          let (expectedPrefix, missingSuffix) =
                splitAt existingCount binds
              renames =
                [ (oldRef, newRef)
                | ((oldRef, _), (newRef, _)) <-
                    zip topBinds expectedPrefix
                , oldRef /= newRef
                ]
              body' = renameTermTypeVars renames body
              completedBody =
                foldr
                  (\(ref, mbBound) acc ->
                    eTyAbsWithRef ref mbBound acc
                  )
                  body'
                  missingSuffix
              -- With no shared identity, the eliminator/scheme contract is
              -- the only authority relating the existing prefix to the
              -- expected binders. Only its unmatched suffix is new.
              rebuilt =
                foldr
                  (\(ref, mbBound) acc ->
                    eTyAbsWithRef ref mbBound acc
                  )
                  completedBody
                  expectedPrefix
           in validateRebuilt rebuilt
      where
        existingCount = length topBinds

    validateRebuilt rebuilt =
      case typeCheckOpenTerm rebuilt of
        Right tyAligned
          | alphaEqType tyAligned (schemeToType sch) -> Just rebuilt
        Right _ -> Nothing
        -- Deferred placeholders can make the body temporarily uncheckable
        -- even though the explicit abstraction spine is already present.
        -- Accept only when every deferred-free sibling and its enclosing
        -- application/let context checks.
        Left _
          | deferredTypecheckFailureIsIsolated rebuilt -> Just rebuilt
          | otherwise -> Nothing

    matchExistingBinderSubsequence [] _ = Just []
    matchExistingBinderSubsequence _ [] = Nothing
    matchExistingBinderSubsequence
      existing@(oldBinder@(oldRef, oldBound) : oldRest)
      (newBinder@(newRef, newBound) : newRest)
        | typeBinderRefsSameIdentity oldRef newRef =
            if binderBoundsAgree oldBound newBound
              then
                ((oldBinder, newBinder) :)
                  <$> matchExistingBinderSubsequence oldRest newRest
              else Nothing
        | otherwise =
            matchExistingBinderSubsequence existing newRest

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
   in case (binders, typeCheckOpenTerm term) of
        ([], _) -> Nothing
        (_, Left _) -> Nothing
        (_, Right ty) ->
          let freeRefs = freeTypeVarRefsType ty
              renames = zip freeRefs binders
              termAligned = renameTermTypeVars renames term
           in case typeCheckOpenTerm termAligned of
                Right tyAligned
                  | null (freeTypeVarRefsType tyAligned) || length freeRefs <= length binders -> Just termAligned
                _ -> Nothing

alignTermTypeVarsToSchemeBody :: ElabScheme -> XmlfTerm -> Maybe XmlfTerm
alignTermTypeVarsToSchemeBody scheme term =
  case typeCheckOpenTerm term of
    Right ty ->
      case structuralRenamesToSchemeBody binderRefs body ty of
        Just renames ->
          let termAligned = renameTermTypeVars renames term
           in case typeCheckOpenTerm termAligned of
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
       in case typeCheckOpenTerm termAligned of
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

typeCheckOpenTerm :: XmlfTerm -> Either TypeCheckError ElabType
typeCheckOpenTerm term =
  typeCheckWithEnv
    ( mkTypeCheckEnvWithResolvedTerms
        [(resolved, resolvedVarType resolved) | resolved <- freeResolvedTermVars term]
        Map.empty
    )
    term

freeResolvedTermVars :: XmlfTerm -> [ResolvedVar]
freeResolvedTermVars = go []
  where
    go bound term = case term of
      EVarNode resolved
        | resolvedVarBoundBy bound resolved -> []
        | otherwise -> [resolved]
      ELit {} -> []
      ELam resolved body -> go (resolved : bound) body
      EApp fun arg -> go bound fun ++ go bound arg
      ELet resolved _ rhs body ->
        let bound' = resolved : bound
         in go bound' rhs ++ go bound' body
      ETyAbsRef _ _ body -> go bound body
      ETyInst inner _ -> go bound inner
      ERoll _ body -> go bound body
      EUnroll body -> go bound body

containsUnresolvedDeferredRef :: XmlfTerm -> Bool
containsUnresolvedDeferredRef = any isDeferred . freeResolvedTermVars
  where
    isDeferred resolved =
      case deferredResolvedVarRef resolved of
        Just _ -> True
        Nothing -> False

-- | Check that a failed typecheck is confined to unresolved deferred holes.
-- Every subtree that does not contain a hole must typecheck in its lexical
-- environment.  For applications, a deferred argument cannot excuse a
-- non-function operator; for lets, a deferred body cannot excuse an invalid
-- ordinary RHS/scheme pair.
deferredTypecheckFailureIsIsolated :: XmlfTerm -> Bool
deferredTypecheckFailureIsIsolated = go emptyEnv
  where
    go :: Env -> XmlfTerm -> Bool
    go env term
      | not (containsUnresolvedDeferredRef term) =
          isRight (typeCheckOpenTermWithBaseEnv env term)
      | otherwise =
          case term of
            EVarNode resolved ->
              case deferredResolvedVarRef resolved of
                Just _ -> True
                Nothing -> False
            ELit {} -> False
            ELam resolved body ->
              binderTypeChecks env resolved
                && go
                  (insertResolvedTermBinding resolved (resolvedVarType resolved) env)
                  body
            EApp fun arg ->
              go env fun
                && go env arg
                && functionPositionCanDependOnDeferred env fun
            ELet resolved scheme rhs body ->
              let schemeTy = schemeToType scheme
                  resolved' = mapResolvedVarType (const schemeTy) resolved
                  env' = insertResolvedTermBinding resolved' schemeTy env
               in binderTypeChecks env resolved'
                    && go env' rhs
                    && go env' body
                    && ordinaryLetRhsMatches env resolved' scheme rhs
            ETyAbsRef ref mbBound body ->
              typeBinderChecks env ref mbBound
                && go
                  (insertTypeBindingRef ref (maybe TBottom tyToElab mbBound) env)
                  body
            ETyInst inner _ ->
              go env inner
                && instantiationFailureCanDependOnDeferred env term inner
            ERoll _ body -> go env body
            EUnroll body -> go env body

    binderTypeChecks env resolved =
      isRight $
        typeCheckOpenTermWithBaseEnv
          env
          (ELam resolved (EVarNode resolved))

    typeBinderChecks env ref mbBound =
      isRight $
        typeCheckOpenTermWithBaseEnv
          env
          (eTyAbsWithRef ref mbBound (ELit (LInt 0)))

    functionPositionCanDependOnDeferred env fun
      | containsUnresolvedDeferredRef fun = True
      | otherwise =
          case typeCheckOpenTermWithBaseEnv env fun of
            Right TArrow {} -> True
            _ -> False

    -- An unresolved term may postpone checking an enclosing instantiation
    -- only when it can still change the instantiated term's type.  A hole in
    -- an application argument cannot change the function result, so once that
    -- application has a type, an invalid InstAbstr is an ordinary construction
    -- error rather than a deferred one.
    instantiationFailureCanDependOnDeferred env term inner =
      case typeCheckOpenTermWithBaseEnv env inner of
        Left _ -> True
        Right _ ->
          resultTypeMayDependOnDeferred inner
            || isRight (typeCheckOpenTermWithBaseEnv env term)

    resultTypeMayDependOnDeferred term =
      case term of
        EVarNode resolved ->
          case deferredResolvedVarRef resolved of
            Just _ -> True
            Nothing -> False
        ELit {} -> False
        ELam _ body -> resultTypeMayDependOnDeferred body
        EApp fun _ -> resultTypeMayDependOnDeferred fun
        ELet _ _ _ body -> resultTypeMayDependOnDeferred body
        ETyAbsRef _ _ body -> resultTypeMayDependOnDeferred body
        ETyInst inner _ -> resultTypeMayDependOnDeferred inner
        ERoll {} -> False
        EUnroll body -> resultTypeMayDependOnDeferred body

    ordinaryLetRhsMatches env resolved scheme rhs
      | containsUnresolvedDeferredRef rhs = True
      | otherwise =
          isRight $
            typeCheckOpenTermWithBaseEnv
              env
              (ELet resolved scheme rhs (EVarNode resolved))

typeCheckOpenTermWithBaseEnv :: Env -> XmlfTerm -> Either TypeCheckError ElabType
typeCheckOpenTermWithBaseEnv baseEnv term =
  typeCheckWithEnv
    ( foldr
        (\resolved -> insertResolvedTermBinding resolved (resolvedVarType resolved))
        baseEnv
        (freeResolvedTermVars term)
    )
    term

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
  ELam resolved body -> isIdentityBoundaryBody resolved body
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
  InstAbstrFRef ref -> [ref]

substInTermRefs :: IntMap.IntMap TypeBinderRef -> XmlfTerm -> XmlfTerm
substInTermRefs = go
  where
    go subst term = case term of
      EVarNode resolved -> EVarNode (mapResolvedVarType (substInTyRefs subst) resolved)
      ELit l -> ELit l
      ELam resolved body ->
        ELam
          (mapResolvedVarType (substInTyRefs subst) resolved)
          (go subst body)
      EApp fun arg -> EApp (go subst fun) (go subst arg)
      ELet resolved scheme rhs body ->
        ELet
          (mapResolvedVarType (substInTyRefs subst) resolved)
          (substInSchemeRefs subst scheme)
          (go subst rhs)
          (go subst body)
      ETyAbsRef ref mbBound body ->
        let (ref', bodySubst) = substituteLexicalBinder subst ref
         in eTyAbsWithRef
              ref'
              (fmap (substInTyRefs subst) mbBound)
              (go bodySubst body)
      ETyInst inner inst ->
        ETyInst
          (go subst inner)
          (substInInstRefs subst inst)
      ERoll ty body -> ERoll (substInTyRefs subst ty) (go subst body)
      EUnroll body -> EUnroll (go subst body)

substInTyRefs :: IntMap.IntMap TypeBinderRef -> Ty v -> Ty v
substInTyRefs = go
  where
    go :: IntMap.IntMap TypeBinderRef -> Ty i -> Ty i
    go subst ty = case ty of
      TVarRef ref -> TVarRef (applySubstRefByMap subst ref)
      TVarAppRef ref args ->
        TVarAppRef
          (applySubstRefByMap subst ref)
          (fmap (go subst) args)
      TArrow domain codomain -> TArrow (go subst domain) (go subst codomain)
      TConWithIdentity identity constructor args ->
        TConWithIdentity identity constructor (fmap (go subst) args)
      TBaseWithIdentity identity base -> TBaseWithIdentity identity base
      TForallRef ref mbBound body ->
        let (ref', bodySubst) = substituteLexicalBinder subst ref
         in TForallRef
              ref'
              (fmap (go subst) mbBound)
              (go bodySubst body)
      TMuRef ref body ->
        let (ref', bodySubst) = substituteLexicalBinder subst ref
         in TMuRef ref' (go bodySubst body)
      TBottom -> TBottom

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
substInInstRefs = go
  where
    go subst inst = case inst of
      InstId -> InstId
      InstApp ty -> InstApp (substInTyRefs subst ty)
      InstBot ty -> InstBot (substInTyRefs subst ty)
      InstIntro -> InstIntro
      InstElim -> InstElim
      InstAbstrRef ref ->
        let substituted = applySubstRefByMap subst ref
         in instAbstrWithRef
              ( if typeBinderRefsSameIdentity ref substituted
                  then substituted
                  else ref
              )
      InstUnderRef ref inner ->
        let (ref', innerSubst) = substituteLexicalBinder subst ref
         in instUnderWithRef ref' (go innerSubst inner)
      InstInside inner -> InstInside (go subst inner)
      InstSeq left right -> InstSeq (go subst left) (go subst right)

-- | A graph substitution may refine a free reference to a source or
-- structural identity, but it cannot capture a lexical binder carrying that
-- same graph node.  Same-identity replacements are presentation changes and
-- remain visible in the body; cross-identity replacements are shadowed for
-- the whole binder scope.
substituteLexicalBinder
  :: IntMap.IntMap TypeBinderRef
  -> TypeBinderRef
  -> (TypeBinderRef, IntMap.IntMap TypeBinderRef)
substituteLexicalBinder subst ref =
  let substituted = applySubstRefByMap subst ref
   in if typeBinderRefsSameIdentity ref substituted
        then (substituted, subst)
        else (ref, shadowTypeBinderSubst ref subst)

shadowTypeBinderSubst
  :: TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
shadowTypeBinderSubst ref subst =
  case typeBinderRefNode ref of
    Just (NodeId nodeKey) -> IntMap.delete nodeKey subst
    Nothing -> subst
