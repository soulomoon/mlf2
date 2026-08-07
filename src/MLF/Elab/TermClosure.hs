{-# LANGUAGE GADTs #-}

module MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstRefsIfNeeded,
    constructTermWithSchemeSubstRefs,
    constructTermWithInterleavedSchemeSubstRefsAtPublication,
    constructTermWithSchemeSubstRefsAtPublication,
    constructTermWithSchemeSubstRefsAtPublicationWithRoutes,
    constructTermWithCertifiedResultSchemeAtPublicationWithRoutes,
    constructTermWithSchemeSubstRefsAtResult,
    constructTermWithSchemeSubstRefsByBinderRoutes,
    etaExpandTermToSchemeSubstRefs,
    alignTopTyAbsToScheme,
    alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    preserveRetainedChildAuthoritativeResult,
    refreshLocalResolvedVarType,
    freshenTypeAbsIdentitiesAgainstEnv,
    freshenTypeAbsIdentitiesAgainstEnvWithRenames,
    renameTypeVarInTermAgainstEnv,
    alphaRenameTermTypeBinderScopes,
    alphaRenameTypeBinderScopes,
    renameTermTypeBinderRefPayloads,
    renameBoundTypeBinderRefPayloads,
    renameTypeBinderRefPayloads,
    renameTermTypeVars,
    substInTermRefs,
    typeCheckConstructedOpenTermWithBaseEnv,
  )
where

import Data.Functor.Foldable (Recursive (project), cata)
import Data.Either (isRight)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Elab.Inst (applyInstantiation, composeInst, schemeToType)
import qualified MLF.Elab.Reduce as Reduce
import MLF.Elab.TypeCheck (Env, emptyEnv, emptyResolvedTermEnv, insertResolvedTermBinding, insertResolvedTermEnv, insertTypeBindingRef, mkTypeCheckEnvWithResolvedTerms, typeCheckWithEnv, typeCheckWithResolvedEnv)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Frontend.Syntax (Lit (LInt))
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, freshNameLike, matchTypeRefs, substTypeCaptureRef, substTypeSimpleRef)

type TypeVarRename = (TypeBinderRef, TypeBinderRef)

-- | Alpha-rename type abstractions that would capture an exact type identity
-- already visible in the checking environment.  This is needed when a child
-- value with a complete forall spine is moved beneath an enclosing
-- construction Gamma that opens the source binder used to specialize that
-- value.  The child retains a fresh local identity; the ambient identity
-- remains available to the explicit Inside(Hyp);N computation.
freshenTypeAbsIdentitiesAgainstEnv
  :: TypeCheck.Env
  -> XmlfTerm
  -> XmlfTerm
freshenTypeAbsIdentitiesAgainstEnv initialEnv term0 =
  fst (freshenTypeAbsIdentitiesAgainstEnvWithRenames initialEnv term0)

-- | Freshen capture-prone type abstractions and return the exact identity
-- renames performed by that construction pass.  Callers that publish
-- construction certificates must transport the same renames rather than
-- recovering them from the subsequently checked type.
freshenTypeAbsIdentitiesAgainstEnvWithRenames
  :: TypeCheck.Env
  -> XmlfTerm
  -> (XmlfTerm, [TypeVarRename])
freshenTypeAbsIdentitiesAgainstEnvWithRenames initialEnv term0 =
  let (term, _, renames) = go generator0 visibleRefs initialEnv term0
   in (term, renames)
  where
    visibleRefs =
      foldr insertRef (Map.keys (TypeCheck.typeEnv initialEnv))
        ( concatMap
            (freeTypeVarRefsType . snd)
            ( TypeCheck.resolvedTermEnvEntries
                (TypeCheck.resolvedTermEnv initialEnv)
            )
            ++ concatMap
              freeTypeVarRefsType
              (Map.elems (TypeCheck.typeEnv initialEnv))
        )
    seedTerm =
      foldr
        (\ref body -> ETyAbsRef ref Nothing body)
        term0
        visibleRefs
    generator0 = identityGeneratorAfterTerm seedTerm

    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    go generator visible tcEnv term =
      case project term of
        EVarNodeF {} -> (term, generator, [])
        ELitF {} -> (term, generator, [])
        ELamF resolved body ->
          let tcEnv' =
                TypeCheck.insertResolvedTermBinding
                  resolved
                  (resolvedVarType resolved)
                  tcEnv
              visible' =
                foldr
                  insertRef
                  visible
                  (freeTypeVarRefsType (resolvedVarType resolved))
              (body', generator', renames) =
                go generator visible' tcEnv' body
           in (ELam resolved body', generator', renames)
        EAppF function argument ->
          let (function', generator', functionRenames) =
                go generator visible tcEnv function
              (argument', generator'', argumentRenames) =
                go generator' visible tcEnv argument
           in ( EApp function' argument'
              , generator''
              , functionRenames ++ argumentRenames
              )
        ELetF resolved scheme rhs body ->
          let (scheme', schemeBinderRenames, generator') =
                freshenLetSchemeBinders generator visible scheme
              schemeTy = schemeToType scheme'
              resolved' = mapResolvedVarType (const schemeTy) resolved
              tcEnv' =
                TypeCheck.insertResolvedTermBinding
                  resolved'
                  schemeTy
                  tcEnv
              visible' =
                foldr
                  insertRef
                  visible
                  (freeTypeVarRefsType schemeTy)
              rhsAtFreshScheme =
                alphaRenameLeadingSchemeAbstractions
                  tcEnv'
                  schemeBinderRenames
                  (refreshLocalResolvedVarType resolved' schemeTy rhs)
              bodyAtFreshScheme =
                refreshLocalResolvedVarType resolved' schemeTy body
              (rhs', generator'', rhsRenames) =
                go generator' visible' tcEnv' rhsAtFreshScheme
              (body', generator''', bodyRenames) =
                go generator'' visible' tcEnv' bodyAtFreshScheme
           in ( ELet resolved' scheme' rhs' body'
              , generator'''
              , schemeBinderRenames ++ rhsRenames ++ bodyRenames
              )
        ETyAbsFRef ref mbBound body ->
          let collision =
                any (typeBinderRefsSameIdentity ref) visible
              (ref', bodyForRef, generator') =
                if collision
                  then
                    let (freshRef, nextGenerator) =
                          freshTypeBinderRef
                            (typeBinderRefName ref)
                            generator
                     in
                      ( freshRef
                      , renameTypeVarInTermAgainstEnv
                          tcEnv
                          ref
                          freshRef
                          body
                      , nextGenerator
                      )
                  else (ref, body, generator)
              visible' = insertRef ref' visible
              tcEnv' =
                TypeCheck.insertTypeBindingRef
                  ref'
                  (maybe TBottom tyToElab mbBound)
                  tcEnv
              (body', generator'', bodyRenames) =
                go generator' visible' tcEnv' bodyForRef
              abstractionRename
                | collision = [(ref, ref')]
                | otherwise = []
           in ( ETyAbsRef ref' mbBound body'
              , generator''
              , abstractionRename ++ bodyRenames
              )
        ETyInstF inner instantiation ->
          let (inner', generator', renames) =
                go generator visible tcEnv inner
           in (ETyInst inner' instantiation, generator', renames)
        ERollF ty body ->
          let (body', generator', renames) =
                go generator visible tcEnv body
           in (ERoll ty body', generator', renames)
        EUnrollF body ->
          let (body', generator', renames) =
                go generator visible tcEnv body
           in (EUnroll body', generator', renames)

    -- A let scheme is a lexical type-binder boundary just like the explicit
    -- abstractions that construct its RHS.  If an enclosing Gamma already
    -- uses the same identity, freshen the scheme and its matching RHS
    -- abstractions together before descending.  Freshening only the
    -- 'ETyAbsRef' would leave the 'ELet' scheme and its occurrences at the
    -- captured ambient identity.
    freshenLetSchemeBinders generator visible scheme =
      ( mkElabSchemeWithRefs
          (renameSchemeBinders [] (schemeBinderRefs scheme))
          (applyIdentityRenames renames (schemeBody scheme))
      , renames
      , generator'
      )
      where
        (renames, generator', _) =
          foldl'
            chooseBinder
            ([], generator, visible)
            (map fst (schemeBinderRefs scheme))

        chooseBinder (chosen, nextGenerator, reserved) ref
          | any (typeBinderRefsSameIdentity ref) reserved =
              let (freshRef, generatorAfterFresh) =
                    freshTypeBinderRef
                      (typeBinderRefName ref)
                      nextGenerator
               in ( chosen ++ [(ref, freshRef)]
                  , generatorAfterFresh
                  , insertRef freshRef reserved
                  )
          | otherwise =
              (chosen, nextGenerator, insertRef ref reserved)

        renameSchemeBinders _ [] = []
        renameSchemeBinders preceding ((ref, mbBound) : rest) =
          let ref' = applyRefRenames renames ref
              mbBound' = fmap (renameBoundAt preceding) mbBound
              preceding'
                | typeBinderRefsSameIdentity ref ref' = preceding
                | otherwise = preceding ++ [(ref, ref')]
           in (ref', mbBound') : renameSchemeBinders preceding' rest

        renameBoundAt activeRenames bound =
          case
              elabToBound
                (applyIdentityRenames activeRenames (tyToElab bound))
            of
            Right renamed -> renamed
            Left _ -> bound

    applyIdentityRenames renames ty0 =
      foldl'
        ( \ty (oldRef, newRef) ->
            substTypeCaptureRef oldRef (TVarRef newRef) ty
        )
        ty0
        renames

    -- The scheme binders correspond to leading RHS abstractions when the
    -- producer constructs its own forall spine.  Rename those declarations
    -- and their scoped occurrences, while leaving ambient resolved-variable
    -- payloads supplied by the checking environment untouched.
    alphaRenameLeadingSchemeAbstractions _ [] term = term
    alphaRenameLeadingSchemeAbstractions tcEnv renames term =
      case term of
        ETyAbsRef ref mbBound body ->
          case
              find
                (\(oldRef, _) -> typeBinderRefsSameIdentity oldRef ref)
                renames
            of
            Just (oldRef, newRef) ->
              let body' =
                    renameTypeVarInTermAgainstEnv
                      tcEnv
                      oldRef
                      newRef
                      body
                  tcEnv' =
                    TypeCheck.insertTypeBindingRef
                      newRef
                      (maybe TBottom tyToElab mbBound)
                      tcEnv
               in ETyAbsRef
                    newRef
                    mbBound
                    ( alphaRenameLeadingSchemeAbstractions
                        tcEnv'
                        ( filter
                            ( not
                                . typeBinderRefsSameIdentity oldRef
                                . fst
                            )
                            renames
                        )
                        body'
                    )
            Nothing ->
              let tcEnv' =
                    TypeCheck.insertTypeBindingRef
                      ref
                      (maybe TBottom tyToElab mbBound)
                      tcEnv
               in ETyAbsRef
                    ref
                    mbBound
                    ( alphaRenameLeadingSchemeAbstractions
                        tcEnv'
                        renames
                        body
                    )
        _ -> term

-- | Rename occurrences of one type binder in a term body while preserving
-- resolved variables supplied by the surrounding checking environment.
-- A nested abstraction of the same identity stops the rename, as required
-- for ordinary alpha-conversion.
renameTypeVarInTermAgainstEnv
  :: TypeCheck.Env
  -> TypeBinderRef
  -> TypeBinderRef
  -> XmlfTerm
  -> XmlfTerm
renameTypeVarInTermAgainstEnv env oldRef newRef = go env
  where
    renameTy = substTypeCaptureRef oldRef (TVarRef newRef)
    -- A bound is a lexical type value, not a collection of independent
    -- embedded types.  In particular, a nested forall or mu can shadow the
    -- binder being alpha-renamed outside the bound.  Mapping 'renameTy' over
    -- its children would forget that scope and rename the nested binder's
    -- occurrences without renaming its declaration.
    renameLocalBound bound =
      case bound of
        TArrow domain codomain ->
          TArrow (renameTy domain) (renameTy codomain)
        TConWithIdentity identity constructor arguments ->
          TConWithIdentity identity constructor (fmap renameTy arguments)
        TVarAppRef ref arguments ->
          TVarAppRef (renameRef ref) (fmap renameTy arguments)
        TBaseWithIdentity identity base ->
          TBaseWithIdentity identity base
        TForallRef ref mbBound body
          | typeBinderRefsSameIdentity ref oldRef ->
              TForallRef ref mbBound body
          | otherwise ->
              TForallRef
                ref
                (fmap renameLocalBound mbBound)
                (renameTy body)
        TMuRef ref body
          | typeBinderRefsSameIdentity ref oldRef ->
              TMuRef ref body
          | otherwise ->
              TMuRef ref (renameTy body)
        TBottom -> TBottom
    renameScheme scheme =
      schemeFromType (renameTy (schemeToType scheme))
    renameRef ref
      | typeBinderRefsSameIdentity ref oldRef = newRef
      | otherwise = ref
    renameLocalInstantiation instantiation =
      case project instantiation of
        InstIdF -> InstId
        InstAppF ty -> InstApp (renameTy ty)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstInsideF inner -> InstInside (renameLocalInstantiation inner)
        InstSeqF first second ->
          InstSeq
            (renameLocalInstantiation first)
            (renameLocalInstantiation second)
        InstUnderFRef ref inner ->
          instUnderWithRef
            (renameRef ref)
            (renameLocalInstantiation inner)
        InstBotF ty -> InstBot (renameTy ty)
        InstAbstrFRef ref -> instAbstrWithRef (renameRef ref)

    go tcEnv term =
      case project term of
        EVarNodeF resolved ->
          case
              TypeCheck.lookupResolvedTermEnvEntry
                (TypeCheck.resolvedTermEnv tcEnv)
                resolved
            of
              Just (_, ty) ->
                EVarNode (mapResolvedVarType (const ty) resolved)
              Nothing ->
                EVarNode (mapResolvedVarType renameTy resolved)
        ELitF lit -> ELit lit
        ELamF resolved body ->
          let resolved' = mapResolvedVarType renameTy resolved
              tcEnv' =
                TypeCheck.insertResolvedTermBinding
                  resolved'
                  (resolvedVarType resolved')
                  tcEnv
           in ELam resolved' (go tcEnv' body)
        EAppF function argument ->
          EApp (go tcEnv function) (go tcEnv argument)
        ELetF resolved scheme rhs body ->
          let scheme' = renameScheme scheme
              schemeTy = schemeToType scheme'
              resolved' = mapResolvedVarType (const schemeTy) resolved
              tcEnv' =
                TypeCheck.insertResolvedTermBinding
                  resolved'
                  schemeTy
                  tcEnv
           in
            ELet
              resolved'
              scheme'
              (go tcEnv' rhs)
              (go tcEnv' body)
        ETyAbsFRef ref mbBound body
          | typeBinderRefsSameIdentity ref oldRef ->
              ETyAbsRef ref (fmap renameLocalBound mbBound) body
          | otherwise ->
              let mbBound' = fmap renameLocalBound mbBound
                  tcEnv' =
                    TypeCheck.insertTypeBindingRef
                      ref
                      (maybe TBottom tyToElab mbBound')
                      tcEnv
               in ETyAbsRef ref mbBound' (go tcEnv' body)
        ETyInstF inner instantiation ->
          ETyInst
            (go tcEnv inner)
            (renameLocalInstantiation instantiation)
        ERollF ty body -> ERoll (renameTy ty) (go tcEnv body)
        EUnrollF body -> EUnroll (go tcEnv body)

-- | Alpha-copy selected lexical type declarations without rewriting a free
-- occurrence that merely carries the same identity.  This differs
-- deliberately from 'renameTermTypeBinderRefPayloads': the latter is an
-- identity-preserving presentation change, while this operation may replace
-- one binder by a genuinely fresh identity.
--
-- The distinction matters for the paper's annotated self-application.  When
-- @g : forall a. a -> a@ is moved under an enclosing @Lambda a@, the forall in
-- @g@ must be copied, but the later computation @g[a]@ must keep referring to
-- the enclosing @a@.  A global payload rewrite would turn both occurrences
-- into the copy and invalidate the terminal @Hyp@.
alphaRenameTermTypeBinderScopes
  :: [TypeVarRename]
  -> XmlfTerm
  -> XmlfTerm
alphaRenameTermTypeBinderScopes selectedRenames = renameTerm []
  where
    renameTerm activeRenames term =
      case term of
        EVarNode resolved ->
          EVarNode (mapResolvedVarType (renameType activeRenames) resolved)
        ELit lit -> ELit lit
        ELam resolved body ->
          ELam
            (mapResolvedVarType (renameType activeRenames) resolved)
            (renameTerm activeRenames body)
        EApp function argument ->
          EApp
            (renameTerm activeRenames function)
            (renameTerm activeRenames argument)
        ELet resolved scheme rhs body ->
          ELet
            (mapResolvedVarType (renameType activeRenames) resolved)
            (renameScheme activeRenames scheme)
            (renameTerm activeRenames rhs)
            (renameTerm activeRenames body)
        ETyAbsRef ref mbBound body ->
          let (ref', bodyRenames) = enterBinder activeRenames ref
           in ETyAbsRef
                ref'
                (fmap (renameScopedBound activeRenames) mbBound)
                (renameTerm bodyRenames body)
        ETyInst body instantiation ->
          ETyInst
            (renameTerm activeRenames body)
            (renameInstantiation activeRenames instantiation)
        ERoll ty body ->
          ERoll
            (renameType activeRenames ty)
            (renameTerm activeRenames body)
        EUnroll body -> EUnroll (renameTerm activeRenames body)

    renameScheme activeRenames =
      schemeFromType . renameType activeRenames . schemeToType

    renameInstantiation activeRenames instantiation =
      case instantiation of
        InstId -> InstId
        InstApp ty -> InstApp (renameType activeRenames ty)
        InstBot ty -> InstBot (renameType activeRenames ty)
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstAbstrRef ref -> InstAbstrRef (renameActiveRef activeRenames ref)
        InstUnderRef ref inner ->
          let (ref', innerRenames) = enterBinder activeRenames ref
           in InstUnderRef
                ref'
                (renameInstantiation innerRenames inner)
        InstInside inner ->
          InstInside (renameInstantiation activeRenames inner)
        InstSeq left right ->
          InstSeq
            (renameInstantiation activeRenames left)
            (renameInstantiation activeRenames right)

    renameType = alphaRenameTypeBinderScopesWith selectedRenames

    renameScopedBound activeRenames bound =
      case elabToBound (renameType activeRenames (tyToElab bound)) of
        Right renamed -> renamed
        Left _ -> bound

    renameActiveRef activeRenames ref =
      fromMaybe ref (lookupRename activeRenames ref)

    enterBinder activeRenames ref =
      case lookupRename activeRenames ref of
        -- A declaration with the same identity nested under an already active
        -- copy shadows that copy.  Keeping the nested declaration at the old
        -- identity makes the two scopes distinct after the outer copy.
        Just _ -> (ref, removeRename ref activeRenames)
        Nothing ->
          case lookupRename selectedRenames ref of
            Just freshRef ->
              ( freshRef
              , (ref, freshRef) : removeRename ref activeRenames
              )
            Nothing -> (ref, removeRename ref activeRenames)

    lookupRename renames ref =
      snd
        <$> find
          (\(sourceRef, _) -> typeBinderRefsSameIdentity sourceRef ref)
          renames

    removeRename ref =
      filter
        (not . (`typeBinderRefsSameIdentity` ref) . fst)

-- | Type-only half of 'alphaRenameTermTypeBinderScopes'.  Selected binders are
-- copied when their declarations are encountered; free occurrences are
-- rewritten only while that copied declaration is lexically active.
alphaRenameTypeBinderScopes
  :: [TypeVarRename]
  -> ElabType
  -> ElabType
alphaRenameTypeBinderScopes selectedRenames =
  alphaRenameTypeBinderScopesWith selectedRenames []

alphaRenameTypeBinderScopesWith
  :: [TypeVarRename]
  -> [TypeVarRename]
  -> ElabType
  -> ElabType
alphaRenameTypeBinderScopesWith selectedRenames = renameType
  where
    renameType activeRenames ty =
      case ty of
        TVarRef ref -> TVarRef (renameActiveRef activeRenames ref)
        TVarAppRef ref arguments ->
          TVarAppRef
            (renameActiveRef activeRenames ref)
            (fmap (renameType activeRenames) arguments)
        TArrow domain codomain ->
          TArrow
            (renameType activeRenames domain)
            (renameType activeRenames codomain)
        TConWithIdentity identity constructor arguments ->
          TConWithIdentity
            identity
            constructor
            (fmap (renameType activeRenames) arguments)
        TBaseWithIdentity identity base -> TBaseWithIdentity identity base
        TForallRef ref mbBound body ->
          let (ref', bodyRenames) = enterBinder activeRenames ref
           in TForallRef
                ref'
                (fmap (renameScopedBound activeRenames) mbBound)
                (renameType bodyRenames body)
        TMuRef ref body ->
          let (ref', bodyRenames) = enterBinder activeRenames ref
           in TMuRef ref' (renameType bodyRenames body)
        TBottom -> TBottom

    renameScopedBound activeRenames bound =
      case elabToBound (renameType activeRenames (tyToElab bound)) of
        Right renamed -> renamed
        Left _ -> bound

    renameActiveRef activeRenames ref =
      fromMaybe ref (lookupRename activeRenames ref)

    enterBinder activeRenames ref =
      case lookupRename activeRenames ref of
        Just _ -> (ref, removeRename ref activeRenames)
        Nothing ->
          case lookupRename selectedRenames ref of
            Just freshRef ->
              ( freshRef
              , (ref, freshRef) : removeRename ref activeRenames
              )
            Nothing -> (ref, removeRename ref activeRenames)

    lookupRename renames ref =
      snd
        <$> find
          (\(sourceRef, _) -> typeBinderRefsSameIdentity sourceRef ref)
          renames

    removeRename ref =
      filter
        (not . (`typeBinderRefsSameIdentity` ref) . fst)

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

-- | Construct a publication whose dependency-ordered forall spine combines
-- binders already constructed by the checked producer with binders owned by
-- the enclosing root.  The locally constructed identities are opened at
-- their exact positions in the target spine before the missing flexible
-- binders are abstracted.  This is the construction required for a result
-- such as @forall a. forall b >= tau[a]. b@: moving @b@ outside the existing
-- @a@ abstraction would leave its bound out of scope.
constructTermWithInterleavedSchemeSubstRefsAtPublication
  :: Env
  -> IntMap.IntMap TypeBinderRef
  -> [TypeBinderRef]
  -> ElabScheme
  -> XmlfTerm
  -> XmlfTerm
constructTermWithInterleavedSchemeSubstRefsAtPublication env subst localRefs scheme term =
  case candidate of
    Just constructed
      | checksAgainstTarget constructed -> constructed
    _ -> constructTermWithSchemeSubstRefsAtPublication env subst scheme term
  where
    schemeScopeEnv =
      foldr
        ( \(ref, mbBound) ->
            insertTypeBindingRef ref (maybe TBottom tyToElab mbBound)
        )
        env
        (schemeBinderRefs scheme)
    producer = freshenTypeAbsIdentitiesAgainstEnv schemeScopeEnv term
    (subst', scheme', renames) =
      freshenSchemeAndSubstAgainstTerm producer subst scheme
    producerSubst =
      renameTermTypeVars renames (substInTermRefs subst' producer)
    targetBinders = schemeBinderRefs scheme'
    targetBody = schemeBody scheme'

    candidate = do
      actualTy <- either (const Nothing) Just (typeCheckWithEnv env producerSubst)
      (openedTy, openedProducer) <-
        openConstructedBinders targetBinders actualTy producerSubst
      constructedBody <-
        constructTermToType
          PreserveLambdaResults
          targetBinders
          openedTy
          targetBody
          openedProducer
      pure (wrapTermWithScheme scheme' constructedBody)

    openConstructedBinders [] actualTy producerTerm =
      Just (actualTy, producerTerm)
    openConstructedBinders ((targetRef, targetBound) : rest) actualTy producerTerm
      | any (typeBinderRefsSameIdentity targetRef) localRefs = do
          (actualRef, actualBound, actualBody) <-
            case actualTy of
              TForallRef ref mbBound body -> Just (ref, mbBound, body)
              _ -> Nothing
          if forallBoundsCanRebind actualBound targetBound
            then
              let openedTy =
                    substTypeCaptureRef
                      actualRef
                      (TVarRef targetRef)
                      actualBody
                  openedTerm =
                    ETyInst
                      producerTerm
                      (forallRebindingInstantiation actualBound targetRef)
               in openConstructedBinders rest openedTy openedTerm
            else Nothing
      | otherwise =
          openConstructedBinders rest actualTy producerTerm

    checksAgainstTarget constructed =
      case typeCheckWithEnv env constructed of
        Right constructedTy ->
          alphaEqType constructedTy (schemeToType scheme')
            || churchAwareEqType constructedTy (schemeToType scheme')
        Left _ -> False

    forallBoundsCanRebind Nothing _ = True
    forallBoundsCanRebind (Just actual) (Just expected) =
      binderBoundsAgree (Just actual) (Just expected)
    forallBoundsCanRebind (Just _) Nothing = False

    forallRebindingInstantiation Nothing expectedRef =
      InstApp (TVarRef expectedRef)
    forallRebindingInstantiation (Just _) expectedRef =
      InstSeq
        (InstInside (instAbstrWithRef expectedRef))
        InstElim

-- | Construct a let/publication boundary from its authoritative scheme.
-- Reuse an exact leading abstraction prefix when the producer has already
-- built it.  Otherwise, when a computation merely /returns/ a forall, first
-- alpha-freshen colliding abstractions inside that computation, eliminate the
-- returned forall at the publication binders, and emit the scheme spine once.
-- This keeps binder ownership explicit without ever nesting two declarations
-- of the same 'TypeBinderIdentity'.
constructTermWithSchemeSubstRefsAtPublication
  :: Env
  -> IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> XmlfTerm
constructTermWithSchemeSubstRefsAtPublication env subst scheme term =
  fst
    ( constructTermWithSchemeSubstRefsAtPublicationWithRoutes
        env
        subst
        scheme
        term
    )

-- | Construct a let/publication boundary and retain the exact forall-binder
-- applications selected while building it.  A route @(source, target)@ means
-- that the checked producer's @forall source@ was opened at the already
-- declared publication binder @target@.  Owner-final certificates must replay
-- the same route; otherwise their completed bounds keep the producer-local
-- identity even though the term has already published the target identity.
--
-- The routes are emitted by 'constructTermToTypeWithRoutes' at the same point
-- as the corresponding 'InstApp'.  They are therefore construction evidence,
-- not a match reconstructed from the finished type.
constructTermWithSchemeSubstRefsAtPublicationWithRoutes
  :: Env
  -> IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> (XmlfTerm, [TypeVarRename])
constructTermWithSchemeSubstRefsAtPublicationWithRoutes env subst scheme term =
  constructTermWithResultModeAtPublicationWithRoutes
    PreserveLambdaResults
    env
    subst
    scheme
    term

-- | Construct a publication from an owner-final result certificate.  The
-- caller has already proved that the checked producer owns the complete
-- Figure 15.3.5 result path, so simplification may consume vacuous foralls
-- beneath transparent identity applications and value lambdas before the
-- publication spine is emitted.  Ordinary publication must use
-- 'constructTermWithSchemeSubstRefsAtPublicationWithRoutes' instead.
constructTermWithCertifiedResultSchemeAtPublicationWithRoutes
  :: Env
  -> IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> (XmlfTerm, [TypeVarRename])
constructTermWithCertifiedResultSchemeAtPublicationWithRoutes env subst scheme term =
  constructTermWithResultModeAtPublicationWithRoutes
    ConstructLambdaResults
    env
    subst
    scheme
    term

constructTermWithResultModeAtPublicationWithRoutes
  :: LambdaResultConstruction
  -> Env
  -> IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> (XmlfTerm, [TypeVarRename])
constructTermWithResultModeAtPublicationWithRoutes resultConstruction env subst scheme term =
  let structuralCandidate =
        constructTermWithSchemeSubstRefsByBinderRoutes
          []
          subst
          scheme
          term
      targetTy = schemeToType scheme
      checksAgainst target candidate =
        case typeCheckWithEnv env candidate of
          Right candidateTy ->
            alphaEqType candidateTy target
              || churchAwareEqType candidateTy target
          Left _ -> False
      constructsSchemeSpine candidate =
        go (schemeBinderRefs scheme) candidate
        where
          go [] _ = True
          go ((expectedRef, _) : rest) (ETyAbsRef actualRef _ body)
            | typeBinderRefsSameIdentity expectedRef actualRef =
                go rest body
          go _ _ = False
      constructionScopeBinders =
        case resultConstruction of
          PreserveLambdaResults -> schemeBinderRefs scheme
          ConstructLambdaResults -> forallDeclarationsInType targetTy
   in if constructsSchemeSpine structuralCandidate
        && checksAgainst targetTy structuralCandidate
        then (structuralCandidate, [])
        else
          let schemeScopeEnv =
                foldr
                  ( \(ref, mbBound) ->
                      insertTypeBindingRef
                        ref
                        (maybe TBottom tyToElab mbBound)
                  )
                  env
                  constructionScopeBinders
              producer =
                freshenTypeAbsIdentitiesAgainstEnv
                  schemeScopeEnv
                  term
              (subst', scheme', renames) =
                freshenSchemeAndSubstAgainstTerm producer subst scheme
              producerSubst =
                renameTermTypeVars renames (substInTermRefs subst' producer)
              constructedBody = do
                actualTy <- either (const Nothing) Just (typeCheckWithEnv env producerSubst)
                constructTermToTypeWithRoutes
                  resultConstruction
                  (schemeBinderRefs scheme')
                  actualTy
                  (schemeBody scheme')
                  producerSubst
              publicationCandidate =
                ( \construction ->
                    ( wrapTermWithScheme
                        scheme'
                        (constructedTerm construction)
                    , constructedBinderRoutes construction
                    )
                )
                  <$> constructedBody
           in case publicationCandidate of
                Just candidate@(candidateTerm, _)
                  | checksAgainst (schemeToType scheme') candidateTerm -> candidate
                _ -> (structuralCandidate, [])

-- | Construct a prepared root packet consumer through the term's result
-- path.  Unlike ordinary closure, this operation may descend through value
-- lambdas: its caller carries positive topology-packet placement provenance
-- proving that the enclosing flexible result was created by the
-- generalization plan.  Without that proof, lambda codomains remain owned by
-- their Figure 15.3.5 constructors and use
-- 'closeTermWithSchemeSubstRefsIfNeeded' instead.
constructTermWithSchemeSubstRefsAtResult
  :: Env
  -> IntMap.IntMap TypeBinderRef
  -> ElabScheme
  -> XmlfTerm
  -> XmlfTerm
constructTermWithSchemeSubstRefsAtResult env subst scheme term =
  let (subst', scheme', renames) =
        freshenSchemeAndSubstAgainstTerm term subst scheme
      termSubst = renameTermTypeVars renames (substInTermRefs subst' term)
      termConstructed =
        case typeCheckWithEnv env termSubst of
          Right actualTy ->
            maybe
              termSubst
              id
              ( constructTermToType
                  ConstructLambdaResults
                  (schemeBinderRefs scheme')
                  actualTy
                  (schemeBody scheme')
                  termSubst
              )
          Left _ -> termSubst
   in wrapTermWithScheme scheme' termConstructed

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
  constructTermToType
    PreserveLambdaResults
    (schemeBinderRefs scheme)
    actualTy
    (schemeBody scheme)
    term

data LambdaResultConstruction
  = PreserveLambdaResults
  | ConstructLambdaResults

forallDeclarationsInType
  :: ElabType
  -> [(TypeBinderRef, Maybe BoundType)]
forallDeclarationsInType ty =
  case ty of
    TVarRef {} -> []
    TVarAppRef _ arguments -> foldMap forallDeclarationsInType arguments
    TArrow domain codomain ->
      forallDeclarationsInType domain
        ++ forallDeclarationsInType codomain
    TConWithIdentity _ _ arguments ->
      foldMap forallDeclarationsInType arguments
    TBaseWithIdentity {} -> []
    TForallRef ref mbBound body ->
      (ref, mbBound)
        : maybe [] (forallDeclarationsInType . tyToElab) mbBound
          ++ forallDeclarationsInType body
    TMuRef _ body -> forallDeclarationsInType body
    TBottom -> []

data ConstructedTerm = ConstructedTerm
  { constructedTerm :: !XmlfTerm,
    constructedBinderRoutes :: ![TypeVarRename]
  }

constructTermToType
  :: LambdaResultConstruction
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> XmlfTerm
  -> Maybe XmlfTerm
constructTermToType lambdaResultConstruction binders actualTy expectedTy term =
  constructedTerm
    <$> constructTermToTypeWithRoutes
      lambdaResultConstruction
      binders
      actualTy
      expectedTy
      term

constructTermToTypeWithRoutes
  :: LambdaResultConstruction
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> XmlfTerm
  -> Maybe ConstructedTerm
constructTermToTypeWithRoutes lambdaResultConstruction binders actualTy expectedTy term
  | alphaEqType actualTy expectedTy || churchAwareEqType actualTy expectedTy =
      Just (ConstructedTerm term [])
  | ELet resolved scheme rhs body <- term =
      mapConstructedTerm (ELet resolved scheme rhs)
        <$> constructTermToTypeWithRoutes
          lambdaResultConstruction
          binders
          actualTy
          expectedTy
          body
  | ConstructLambdaResults <- lambdaResultConstruction
  , EApp
      (ELam parameter (EVarNode occurrence))
      argument <- term
  , resolvedVarSameIdentity parameter occurrence =
      -- A prepared topology-packet closure is positive authority to construct
      -- the result beneath transparent source frames.  A checked syntactic
      -- identity application is one such frame: its argument is the complete
      -- returned value, and rebuilding both resolved occurrences at the
      -- constructed endpoint preserves the xMLF application invariant.
      --
      -- This case is deliberately available only to
      -- 'ConstructLambdaResults'.  Ordinary publication must not infer that an
      -- arbitrary EApp is result-transparent from its final type or retrofit a
      -- coercion after type checking.
      let parameterAtExpected =
            mapResolvedVarType (const expectedTy) parameter
          occurrenceAtExpected =
            mapResolvedVarType (const expectedTy) occurrence
       in mapConstructedTerm
            ( EApp
                (ELam parameterAtExpected (EVarNode occurrenceAtExpected))
            )
            <$> constructTermToTypeWithRoutes
              lambdaResultConstruction
              binders
              actualTy
              expectedTy
              argument
  | ConstructLambdaResults <- lambdaResultConstruction
  , ELam resolved body <- term
  , TArrow actualDomain actualCodomain <- actualTy
  , TArrow expectedDomain expectedCodomain <- expectedTy
  , alphaEqType actualDomain expectedDomain
      || churchAwareEqType actualDomain expectedDomain =
      mapConstructedTerm (ELam resolved)
        <$> constructTermToTypeWithRoutes
          lambdaResultConstruction
          binders
          actualCodomain
          expectedCodomain
          body
  | ConstructLambdaResults <- lambdaResultConstruction
  , ETyAbsRef termRef termBound body <- term
  , TForallRef actualRef actualBound actualBody <- actualTy
  , TForallRef expectedRef expectedBound expectedBody <- expectedTy
  , typeBinderRefsSameIdentity termRef actualRef
  , typeBinderRefsSameIdentity actualRef expectedRef
  , binderBoundsAgree actualBound expectedBound =
      mapConstructedTerm (eTyAbsWithRef termRef termBound)
        <$> constructTermToTypeWithRoutes
          lambdaResultConstruction
          binders
          actualBody
          expectedBody
          body
  | ConstructLambdaResults <- lambdaResultConstruction
  , TForallRef actualRef actualBound actualBody <- actualTy
  , TForallRef expectedRef expectedBound expectedBody <- expectedTy
  , forallBoundsCanRebind actualBound expectedBound = do
      let step =
            forallRebindingInstantiation
              actualBound
              expectedRef
          steppedTerm = ETyInst term step
      steppedTy <- either (const Nothing) Just (applyInstantiation actualTy step)
      construction <-
        constructTermToTypeWithRoutes
          lambdaResultConstruction
          binders
          steppedTy
          expectedBody
          steppedTerm
      pure
        construction
          { constructedTerm =
              eTyAbsWithRef
                expectedRef
                expectedBound
                (constructedTerm construction)
          , constructedBinderRoutes =
              binderApplicationRoute actualRef actualBody expectedRef
                ++ constructedBinderRoutes construction
          }
  | TForallRef ref (Just bound) bodyTy <- actualTy,
    let boundTy = tyToElab bound
        instantiatedTy = substTypeSimpleRef ref boundTy bodyTy,
    alphaEqType instantiatedTy expectedTy
      || churchAwareEqType instantiatedTy expectedTy =
      -- Closing a polymorphic term against a monomorphic scheme is an
      -- explicit specialization boundary.  Construct the application from
      -- the target type itself; plain InstElim is reserved for witness replay
      -- where OpWeaken selects the already-carried flexible bound.
      Just (ConstructedTerm (ETyInst term (InstApp boundTy)) [])
  | TVarRef expectedRef <- expectedTy,
    Just (_, mbBound) <- find (typeBinderRefsSameIdentity expectedRef . fst) binders,
    let boundTy = maybe TBottom tyToElab mbBound,
    alphaEqType actualTy boundTy || churchAwareEqType actualTy boundTy =
      Just
        ( ConstructedTerm
            (ETyInst term (instAbstrWithRef expectedRef))
            []
        )
  | TForallRef actualRef actualBound actualBody <- actualTy
  , length (topTyAbsRefs term) < length binders =
      constructFromLeadingForall actualRef actualBound actualBody
  | TForallRef actualRef actualBound actualBody <- actualTy
  , [(expectedRef, _)] <-
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
        ( ConstructedTerm
            ( ETyInst
                term
                (forallRebindingInstantiation actualBound expectedRef)
            )
            (binderApplicationRoute actualRef actualBody expectedRef)
        )
  | otherwise = Nothing
  where
    -- A publication scheme can introduce binders in a different lexical
    -- order from the producer's principal forall spine.  Consume the source
    -- spine one checked step at a time before wrapping the target binders.
    -- In particular, a vacuous source slot is eliminated with N before a
    -- later target binder is used as an explicit type argument.  Treating the
    -- two alpha-equivalent complete types as already constructed would leave
    -- the target identity free instead of publishing its TAbs slot.
    constructFromLeadingForall actualRef actualBound actualBody =
      firstSuccessful
        ( vacuousElimination
            ++ targetApplications
            ++ boundedElimination
        )
      where
        vacuousElimination =
          [ (InstElim, [])
          | not
              ( any
                  (typeBinderRefsSameIdentity actualRef)
                  (freeTypeVarRefsType actualBody)
              )
          ]

        targetApplications =
          [ ( forallRebindingInstantiation actualBound targetRef
            , binderApplicationRoute actualRef actualBody targetRef
            )
          | (targetRef, targetBound) <- binders
          , forallBoundsCanRebind actualBound targetBound
          ]

        boundedElimination =
          [(InstElim, []) | Just _ <- [actualBound]]

        firstSuccessful [] = Nothing
        firstSuccessful (step : rest) =
          case constructAfter step of
            Just constructed -> Just constructed
            Nothing -> firstSuccessful rest

        constructAfter (step, stepRoutes) = do
          appliedTy <- either (const Nothing) Just (applyInstantiation actualTy step)
          let explicitStep =
                fromMaybe
                  (ETyInst term step)
                  (canonicalVacuousPrefixComposition step)
              steppedTerm =
                fromMaybe
                  explicitStep
                  (Reduce.reduceLeadingTypeInstantiationRedexes explicitStep)
          construction <-
            constructTermToTypeWithRoutes
              lambdaResultConstruction
              binders
              appliedTy
              expectedTy
              steppedTerm
          pure
            construction
              { constructedBinderRoutes =
                  stepRoutes ++ constructedBinderRoutes construction
              }

        -- A producer may first retain a vacuous forall while opening its
        -- next binder at that retained identity.  Applying the resulting
        -- forall at a publication binder has the paper-normal form
        -- @N ; <target>@.  Reconstruct that form only from the exact nested
        -- computation and validate both paths from the same source type;
        -- this is an xMLF construction equation, not a type-shape rewrite.
        canonicalVacuousPrefixComposition step =
          case term of
            ETyInst inner existing@(InstUnderRef retainedRef (InstApp (TVarRef argumentRef)))
              | typeBinderRefsSameIdentity retainedRef argumentRef -> do
                  innerTy <- either (const Nothing) Just (typeCheckOpenTerm inner)
                  case innerTy of
                    TForallRef prefixRef _ prefixBody
                      | typeBinderRefsSameIdentity prefixRef retainedRef
                      , not
                          ( any
                              (typeBinderRefsSameIdentity prefixRef)
                              (freeTypeVarRefsType prefixBody)
                          ) -> do
                          existingTy <- either (const Nothing) Just (applyInstantiation innerTy existing)
                          if typesAgree existingTy actualTy
                            then do
                              let canonical = composeInst InstElim step
                              canonicalTy <- either (const Nothing) Just (applyInstantiation innerTy canonical)
                              if typesAgree canonicalTy expectedTy
                                then Just (ETyInst inner canonical)
                                else Nothing
                            else Nothing
                    _ -> Nothing
            _ -> Nothing

        typesAgree left right =
          alphaEqType left right || churchAwareEqType left right

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

    mapConstructedTerm wrap construction =
      construction {constructedTerm = wrap (constructedTerm construction)}

    binderApplicationRoute sourceRef sourceBody targetRef =
      [ (sourceRef, targetRef)
      | not (typeBinderRefsSameIdentity sourceRef targetRef)
      , any
          (typeBinderRefsSameIdentity sourceRef)
          (freeTypeVarRefsType sourceBody)
      ]

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
            (fmap (renameBoundTypeBinderRefPayloads renames) mbBound)
            (renameTerm body)
        ETyInst body inst ->
          ETyInst (renameTerm body) (renameInstantiation inst)
        ERoll ty body -> ERoll (renameType ty) (renameTerm body)
        EUnroll body -> EUnroll (renameTerm body)

    renameScheme scheme =
      mkElabSchemeWithRefs
        [ ( renameRef ref
          , fmap (renameBoundTypeBinderRefPayloads renames) mbBound
          )
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
            (fmap (renameBoundTypeBinderRefPayloads renames) mbBound)
            (renameType body)
        TMuRef ref body -> TMuRef (renameRef ref) (renameType body)
        TBottom -> TBottom

-- | Rename binder references inside a bound as one lexical value.  In
-- particular, nested forall and mu declarations must move together with
-- their scoped occurrences.  'mapBoundType' intentionally maps only the
-- embedded 'ElabType' positions and therefore cannot implement this atomic
-- identity-preserving operation.
renameBoundTypeBinderRefPayloads
  :: [TypeVarRename]
  -> BoundType
  -> BoundType
renameBoundTypeBinderRefPayloads renames = renameBoundPayload
  where
    renameRef = applyRefRenames renames
    renameType = renameTypeBinderRefPayloads renames

    renameBoundPayload bound =
      case bound of
        TArrow domain codomain ->
          TArrow (renameType domain) (renameType codomain)
        TConWithIdentity identity constructor args ->
          TConWithIdentity identity constructor (fmap renameType args)
        TVarAppRef ref args ->
          TVarAppRef (renameRef ref) (fmap renameType args)
        TBaseWithIdentity identity base ->
          TBaseWithIdentity identity base
        TForallRef ref mbBound body ->
          TForallRef
            (renameRef ref)
            (fmap renameBoundPayload mbBound)
            (renameType body)
        TMuRef ref body ->
          TMuRef (renameRef ref) (renameType body)
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

-- | Check an already constructed open term in the exact occurrence context
-- carried by that term, while retaining the caller's type-binder context.
-- Construction may project a free resolved-variable payload through a
-- certified graph/source quotient.  In that case the recursive result's
-- occurrence context is the preferred term Gamma for this boundary; lexical
-- lambda/let binders remain authoritative inside 'typeCheckWithResolvedEnv'.
--
-- Conflicting free occurrences of one resolved identity are not hidden:
-- 'insertResolvedTermEnv' retains one entry, so the other occurrence still
-- fails with 'TCResolvedVarTypeMismatch'.
typeCheckConstructedOpenTermWithBaseEnv
  :: Env
  -> XmlfTerm
  -> Either TypeCheckError ElabType
typeCheckConstructedOpenTermWithBaseEnv baseEnv term =
  typeCheckWithResolvedEnv
    ( foldr
        (\resolved ->
          insertResolvedTermEnv
            resolved
            (resolvedVarType resolved)
        )
        emptyResolvedTermEnv
        (freeResolvedTermVars term)
    )
    baseEnv
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
