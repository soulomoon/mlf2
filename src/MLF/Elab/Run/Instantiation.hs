{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.Instantiation (
    ExactBinderSpinePlan,
    exactBinderSpineRenames,
    exactBinderSpineInstantiation,
    planExactBinderSpine,
    inferInstAppArgsFromSchemeRefs,
    inferInstAppArgsFromSchemeRefsExact,
    constructExactInstantiation,
    constructExactInstantiationAtSourceArguments,
    resolvedSourceApplicationArgumentEndpoint,
    sourceSchemeConstructsExactEndpoint,
    residualTopologyAgreesExact,
    varRefsInType,
    substTypeSelectiveRefs,
    instInsideFromArgsWithBoundsRefs,
    containsForallType
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard)
import qualified Data.Map.Strict as Map

import Data.List (find)

import MLF.Elab.Inst
    ( applyInstantiation
    , composeInst
    , instForLeadingTypeArgument
    , schemeToType
    )
import MLF.Elab.TermClosure
    ( renameBoundTypeBinderRefPayloads
    , renameTypeBinderRefPayloads
    )
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Reify.TypeOps
    ( alphaEqType
    , churchAwareEqType
    , composeTypeHeadRef
    , freeTypeVarRefsType
    , matchChurchAwareTypeRefs
    , matchTypeRefs
    , stripForallsType
    )
newtype SubstFun (i :: TopVar) =
    SubstFun { runSubstFun :: [TypeBinderRef] -> Ty i }

-- | A checked construction that changes only a leading forall spine.
-- Binder renames retain target quantifiers by exact identity; the
-- instantiation consumes source-only quantifiers, using 'InstUnderRef' when
-- that work occurs below a retained declaration.
data ExactBinderSpinePlan = ExactBinderSpinePlan
    { exactBinderSpineRenames :: [(TypeBinderRef, TypeBinderRef)]
    , exactBinderSpineInstantiation :: Instantiation
    }
    deriving (Eq, Show)

-- | Construct one source forall spine at an exact target.  Retaining the next
-- target binder is attempted first.  If its complete bound/body cannot reach
-- the target, the next source binder must instead be consumed by an inferred
-- type application or by the canonical N computation at its bound.
--
-- Position only proposes a retained binder correspondence.  The returned
-- plan is accepted only after applying every rename and the complete xMLF
-- instantiation reproduces the target under the caller's endpoint equality.
planExactBinderSpine
    :: (ElabType -> ElabType -> Bool)
    -> ElabType
    -> ElabType
    -> Maybe ExactBinderSpinePlan
planExactBinderSpine typesAgree sourceTy targetTy = do
    (renames, instantiation) <- go sourceTy targetTy
    completedTy <-
        either
            (const Nothing)
            Just
            ( applyInstantiation
                (renameTypeBinderRefPayloads renames sourceTy)
                instantiation
            )
    guard (typesAgree completedTy targetTy)
    pure
        ExactBinderSpinePlan
            { exactBinderSpineRenames = renames
            , exactBinderSpineInstantiation = instantiation
            }
  where
    go source target =
        introduceExactFlexibleEndpoint source target
            <|> case source of
                TForallRef sourceRef sourceBound sourceBody ->
                    retainLeadingBinder
                        sourceRef
                        sourceBound
                        sourceBody
                        target
                        <|> consumeAndReintroduceVacuousBinder
                            sourceRef
                            sourceBound
                            source
                            target
                        <|> specializeLeadingBinder
                            sourceRef
                            sourceBound
                            sourceBody
                            source
                            target
                TBottom
                    | not (typesAgree TBottom target) ->
                        Just ([], InstBot target)
                _
                    | typesAgree source target -> Just ([], InstId)
                    | otherwise -> Nothing

    -- Root RaiseMerge may publish the checked source as the exact bound of a
    -- fresh flexible result, @forall (alpha > source). alpha@.  Construct
    -- that endpoint directly with the xMLF Intro/Inside/Hyp rules.  Treating
    -- the target binder as though it corresponded positionally to the
    -- source's first forall loses the complete source scheme when @source@
    -- is itself polymorphic.
    introduceExactFlexibleEndpoint source target =
        case target of
            TForallRef targetRef (Just targetBound) (TVarRef bodyRef)
                | typeBinderRefsSameIdentity targetRef bodyRef
                , let boundTy = tyToElab targetBound
                , typesAgree source boundTy ->
                    Just
                        ( []
                        , composeInst
                            InstIntro
                            ( composeInst
                                (InstInside (InstBot boundTy))
                                ( instUnderWithRef
                                    targetRef
                                    (InstAbstrRef targetRef)
                                )
                            )
                        )
            _ -> Nothing

    -- A bounded source declaration can be consumed at its bound by N before
    -- the exact endpoint reintroduces the same positional identity as a
    -- vacuous unbounded declaration by O.  For example,
    --
    --   forall (b >= Bool). t -> b
    --     <= forall b. t -> Bool.
    --
    -- 'retainLeadingBinder' must reject that bound change.  Requiring the
    -- exact same identity here prevents a general target-vacuous O from
    -- bypassing the ordinary retained-spine plan.  The final application
    -- check above still has to reproduce the complete target, so O cannot
    -- hide a residual body mismatch.
    consumeAndReintroduceVacuousBinder
        sourceRef
        sourceBound
        source
        target = do
            _ <- sourceBound
            TForallRef targetRef Nothing targetBody <- pure target
            guard (typeBinderRefsSameIdentity sourceRef targetRef)
            guard
                ( not
                    ( any
                        (typeBinderRefsSameIdentity targetRef)
                        (freeTypeVarRefsType targetBody)
                    )
                )
            specializedTy <-
                either
                    (const Nothing)
                    Just
                    (applyInstantiation source InstElim)
            (renames, bodyInstantiation) <- go specializedTy targetBody
            pure
                ( renames
                , composeInst
                    InstElim
                    (composeInst bodyInstantiation InstIntro)
                )

    retainLeadingBinder sourceRef sourceBound sourceBody target =
        case target of
            TForallRef targetRef targetBound targetBody -> do
                let binderRename =
                        if typeBinderRefsSameIdentity sourceRef targetRef
                            then []
                            else [(sourceRef, targetRef)]
                    alignType =
                        renameTypeBinderRefPayloads binderRename
                (boundRenames, boundInstantiation) <-
                    alignRetainedBound
                        ( fmap
                            (renameBoundTypeBinderRefPayloads binderRename)
                            sourceBound
                        )
                        targetBound
                (innerRenames, innerInstantiation) <-
                    go (alignType sourceBody) targetBody
                let insideInstantiation =
                        case boundInstantiation of
                            InstId -> InstId
                            _ -> InstInside boundInstantiation
                let underInstantiation =
                        case innerInstantiation of
                            InstId -> InstId
                            _ -> instUnderWithRef targetRef innerInstantiation
                pure
                    ( binderRename ++ boundRenames ++ innerRenames
                    , composeInst insideInstantiation underInstantiation
                    )
            _ -> Nothing

    specializeLeadingBinder sourceRef sourceBound sourceBody source target =
        inferSpecialization <|> eliminateAtBound
      where
        inferSpecialization = do
            [argumentTy] <-
                inferInstAppArgsFromSchemeRefsExact
                    [(sourceRef, sourceBound)]
                    -- Later source-only binders are constructed by the
                    -- recursive spine plan.  They must not hide the current
                    -- binder's structural occurrence while its exact
                    -- argument is inferred.  This is proposal-only: every
                    -- skipped binder is still consumed or retained below,
                    -- and the completed plan is accepted only when applying
                    -- it reproduces the exact target.
                    (stripForallsType sourceBody)
                    target
            -- The binder being eliminated is not in scope at its own type
            -- application site.  A target that merely exposes the source
            -- body can otherwise infer the escaping argument @a@ for
            -- @forall a. ...@ and masquerade as a real specialization.
            guard
                ( not
                    ( any
                        (typeBinderRefsSameIdentity sourceRef)
                        (freeTypeVarRefsType argumentTy)
                    )
                )
            continueWith
                (instForLeadingTypeArgument source argumentTy)

        eliminateAtBound = continueWith InstElim

        continueWith step = do
            specializedTy <-
                either (const Nothing) Just
                    (applyInstantiation source step)
            (renames, rest) <- go specializedTy target
            pure (renames, composeInst step rest)

    alignRetainedBound Nothing Nothing = Just ([], InstId)
    alignRetainedBound Nothing (Just targetBound) =
        go TBottom (tyToElab targetBound)
    alignRetainedBound (Just sourceBound) (Just targetBound) =
        go (tyToElab sourceBound) (tyToElab targetBound)
    alignRetainedBound (Just _) Nothing = Nothing

inferInstAppArgsFromSchemeRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefs binds body targetTy =
    inferInstAppArgsFromSchemeRefsWith
        matchTypeRefs
        alphaEqType
        (stripForallsType targetTy)
        binds
        body
        targetTy

-- | Infer source-scheme applications for an explicit xMLF endpoint.  Unlike
-- the compatibility inference above, this preserves a leading forall in the
-- target: first-class polymorphic arguments such as @id id@ instantiate the
-- function at @forall a. a -> a@, not at that scheme's stripped body.
inferInstAppArgsFromSchemeRefsExact :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefsExact binds body targetTy =
    inferInstAppArgsFromSchemeRefsWith
        matchChurchAwareTypeRefs
        exactTypesAgree
        targetTy
        binds
        body
        targetTy
  where
    exactTypesAgree left right =
        alphaEqType left right || churchAwareEqType left right

-- | Select the exact argument endpoint for an identity-resolved source
-- application.  The function's source domain must already be closed: if it
-- still depends on one of the function scheme's binders, as in the paper's
-- @g g@ construction, root preparation must not invent a specialization
-- before the application constructor has established it.
resolvedSourceApplicationArgumentEndpoint
    :: TypeCheck.Env
    -> SchemeInfo
    -> SchemeInfo
    -> Maybe ElabType
resolvedSourceApplicationArgumentEndpoint typeEnv functionSchemeInfo argumentSchemeInfo = do
    sourceDomain <-
        case schemeBody (siScheme functionSchemeInfo) of
            TArrow domain _ -> Just domain
            _ -> Nothing
    guard (null (freeTypeVarRefsType sourceDomain))
    sourceSchemeConstructsExactEndpoint
        typeEnv
        sourceDomain
        argumentSchemeInfo

-- | Prove that one identity-resolved source occurrence constructs an exact
-- endpoint. The complete source forall spine must be consumed, and every
-- type application is checked before the endpoint is published.
sourceSchemeConstructsExactEndpoint
    :: TypeCheck.Env
    -> ElabType
    -> SchemeInfo
    -> Maybe ElabType
sourceSchemeConstructsExactEndpoint typeEnv endpoint schemeInfo = do
    let sourceScheme = siScheme schemeInfo
        sourceTy = schemeToType sourceScheme
        sourceBinders = schemeBinderRefs sourceScheme
    arguments <-
        inferInstAppArgsFromSchemeRefsExact
            sourceBinders
            (schemeBody sourceScheme)
            endpoint
    guard (length arguments == length sourceBinders)
    appliedTy <- foldM applySourceArgument sourceTy arguments
    guard (alphaEqType appliedTy endpoint || churchAwareEqType appliedTy endpoint)
    pure endpoint
  where
    applySourceArgument currentTy argumentTy =
        let instantiation =
                case currentTy of
                    TForallRef _ (Just bound) _
                        | alphaEqType argumentTy (tyToElab bound)
                            || churchAwareEqType argumentTy (tyToElab bound) ->
                            InstElim
                    _ -> InstApp argumentTy
        in either
            (const Nothing)
            Just
            (TypeCheck.checkInstantiation typeEnv currentTy instantiation)

-- | Construct the complete xMLF computation from one checked source type to
-- an exact endpoint.  Target foralls are built with M/I before source
-- specialization is attempted; bounded applications recursively construct
-- their type argument from the declared bound before applying N.  Every
-- candidate is checked from the original source and must reproduce the exact
-- endpoint, so this constructs the occurrence-boundary computation rather
-- than repairing a mismatched term after type checking.
constructExactInstantiation
    :: TypeCheck.Env
    -> (ElabType -> ElabType -> Bool)
    -> ElabType
    -> ElabType
    -> Maybe Instantiation
constructExactInstantiation typeEnv typesAgree source target = do
    instantiation <- go typeEnv source target
    constructed <-
        either
            (const Nothing)
            Just
            (TypeCheck.checkInstantiation typeEnv source instantiation)
    guard (typesAgree constructed target)
    pure instantiation
  where
    go env sourceTy targetTy
        | typesAgree sourceTy targetTy = Just InstId
        | TVarRef targetRef <- targetTy
        , Just targetBound <- TypeCheck.lookupTypeBindingRef targetRef env = do
            -- Hyp consumes the declaration's bound, not an arbitrary source
            -- endpoint.  Construct that bound first so an occurrence such as
            --
            --   forall alpha. alpha -> alpha  <=  a -> a  <=  b
            --
            -- is emitted as @N a; Hyp b@.  Requiring the source to equal the
            -- bound here would lose the leading specialization and leave a
            -- bare Hyp that cannot type-check at the occurrence boundary.
            boundInstantiation <- go env sourceTy targetBound
            Just
                ( composeInst
                    boundInstantiation
                    (InstAbstrRef targetRef)
                )
        | TBottom <- sourceTy = Just (InstBot targetTy)
        | TForallRef targetRef mbTargetBound targetBody <- targetTy =
            introduceTargetForall
                env
                sourceTy
                targetRef
                mbTargetBound
                targetBody
                <|> specializeSource env sourceTy targetTy
        | otherwise = specializeSource env sourceTy targetTy

    introduceTargetForall
        env
        sourceTy
        targetRef
        mbTargetBound
        targetBody = do
            let targetBound = maybe TBottom tyToElab mbTargetBound
                bodyEnv =
                    TypeCheck.insertTypeBindingRef
                        targetRef
                        targetBound
                        env
                refineIntroducedBound =
                    case mbTargetBound of
                        Nothing -> InstId
                        Just bound -> InstInside (InstBot (tyToElab bound))
            bodyInstantiation <- go bodyEnv sourceTy targetBody
            pure
                ( composeInst
                    InstIntro
                    ( composeInst
                        refineIntroducedBound
                        (InstUnderRef targetRef bodyInstantiation)
                    )
                )

    specializeSource env sourceTy targetTy = do
        arguments <-
            inferExactTransportArguments
                (schemeFromType sourceTy)
                targetTy
        (prefix, applied) <-
            foldM (applyArgument env) (InstId, sourceTy) arguments
        (completed, completedTy) <-
            eliminateVacuousForalls env targetTy prefix applied
        guard (typesAgree completedTy targetTy)
        pure completed

    inferExactTransportArguments sourceScheme endpoint =
        if vacuousPrefixReachesEndpoint (schemeToType sourceScheme)
            then Just []
            else
                inferInstAppArgsFromSchemeRefsExact
                    binders
                    body
                    endpoint
                    <|> inferFromArrowDomain
      where
        binders = schemeBinderRefs sourceScheme
        body = schemeBody sourceScheme
        inferFromArrowDomain =
            case (body, endpoint) of
                (TArrow sourceDomain _, TArrow targetDomain _) ->
                    inferInstAppArgsFromSchemeRefsExact
                        binders
                        sourceDomain
                        targetDomain
                _ -> Nothing
        vacuousPrefixReachesEndpoint current
            | typesAgree current endpoint = True
            | TForallRef ref _ bodyTy <- current
            , not
                ( any
                    (typeBinderRefsSameIdentity ref)
                    (freeTypeVarRefsType bodyTy)
                ) =
                vacuousPrefixReachesEndpoint bodyTy
            | otherwise = False

    applyArgument env (prefix, current) argument = do
        applyExactSourceArgument
            env
            typesAgree
            (go env)
            (prefix, current)
            argument

    eliminateVacuousForalls env endpoint = advance
      where
        advance prefix current
            | typesAgree current endpoint = Just (prefix, current)
            | TForallRef ref _ body <- current
            , not
                ( any
                    (typeBinderRefsSameIdentity ref)
                    (freeTypeVarRefsType body)
                ) = do
                next <-
                    either
                        (const Nothing)
                        Just
                        (TypeCheck.checkInstantiation env current InstElim)
                advance (composeInst prefix InstElim) next
            | otherwise = Just (prefix, current)

-- | Construct a complete source-forall specialization at arguments selected
-- by an external construction certificate.  Unlike
-- 'constructExactInstantiation', this function does not infer arguments from
-- the residual target: every leading source declaration must have one exact
-- supplied argument.  This matters when a declaration is vacuous in the
-- residual but is still a dependency of a later flexible bound.  The
-- certificate owns that positional correspondence; each N/bound computation
-- is nevertheless checked here before the endpoint is returned.
constructExactInstantiationAtSourceArguments
    :: TypeCheck.Env
    -> (ElabType -> ElabType -> Bool)
    -> ElabType
    -> [ElabType]
    -> ElabType
    -> Maybe Instantiation
constructExactInstantiationAtSourceArguments
    typeEnv
    typesAgree
    source
    arguments
    target = do
        guard
            ( length arguments
                == length (schemeBinderRefs (schemeFromType source))
            )
        (instantiation, constructed) <-
            foldM
                ( applyExactSourceArgument
                    typeEnv
                    typesAgree
                    (constructExactInstantiation typeEnv typesAgree)
                )
                (InstId, source)
                arguments
        guard (typesAgree constructed target)
        checked <-
            either
                (const Nothing)
                Just
                (TypeCheck.checkInstantiation typeEnv source instantiation)
        guard (typesAgree checked target)
        pure instantiation

applyExactSourceArgument
    :: TypeCheck.Env
    -> (ElabType -> ElabType -> Bool)
    -> (ElabType -> ElabType -> Maybe Instantiation)
    -> (Instantiation, ElabType)
    -> ElabType
    -> Maybe (Instantiation, ElabType)
applyExactSourceArgument
    env
    typesAgree
    constructBound
    (prefix, current)
    argument = do
        step <-
            case current of
                TForallRef _ Nothing _ -> Just (InstApp argument)
                TForallRef _ (Just bound) _
                    | let boundTy = tyToElab bound
                    , typesAgree argument boundTy ->
                        Just InstElim
                    | TVarRef argumentRef <- argument
                    , Just argumentBound <-
                        TypeCheck.lookupTypeBindingRef argumentRef env -> do
                        -- A flexible binder cannot store a bare type
                        -- variable as its rewritten bound.  Construct its
                        -- declared bound to the ambient variable's declared
                        -- bound first, then perform Hyp inside the flexible
                        -- binder and N-eliminate that binder.  Building the
                        -- bound directly to @argument@ would erase the
                        -- variable-shaped bound to Bottom before N runs.
                        inside <- constructBound (tyToElab bound) argumentBound
                        let refineBound =
                                case inside of
                                    InstId -> InstId
                                    _ -> InstInside inside
                            selectAmbientArgument =
                                composeInst
                                    (InstInside (InstAbstrRef argumentRef))
                                    InstElim
                        pure
                            ( composeInst
                                refineBound
                                selectAmbientArgument
                            )
                    | otherwise -> do
                        inside <- constructBound (tyToElab bound) argument
                        pure (composeInst (InstInside inside) InstElim)
                _ -> Nothing
        current' <-
            either
                (const Nothing)
                Just
                (TypeCheck.checkInstantiation env current step)
        pure (composeInst prefix step, current')

-- | Compare a fully specialized residual function with the application
-- topology that justified its arguments.  A graph endpoint may retain a
-- leading bounded forall where the constructed endpoint has already selected
-- that bound, so equality may eliminate those explicit bounds at any residual
-- position.  Other arrow structure must agree recursively; matching only the
-- terminal result is deliberately insufficient.
residualTopologyAgreesExact :: ElabType -> ElabType -> Bool
residualTopologyAgreesExact left right
    | alphaEqType left right || churchAwareEqType left right = True
    | otherwise =
        case (left, right) of
            (TArrow leftDomain leftCodomain, TArrow rightDomain rightCodomain) ->
                residualTopologyAgreesExact leftDomain rightDomain
                    && residualTopologyAgreesExact leftCodomain rightCodomain
            (TForallRef{}, _) ->
                case applyInstantiation left InstElim of
                    Right left' -> residualTopologyAgreesExact left' right
                    Left _ -> False
            (_, TForallRef{}) ->
                case applyInstantiation right InstElim of
                    Right right' -> residualTopologyAgreesExact left right'
                    Left _ -> False
            _ -> False

inferInstAppArgsFromSchemeRefsWith
    :: ([TypeBinderRef] -> ElabType -> ElabType -> Either ElabError (Map.Map TypeBinderRef ElabType))
    -> (ElabType -> ElabType -> Bool)
    -> ElabType
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ElabType
    -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefsWith matchRefs typesAgree targetCore binds body targetTy =
    let binderRefs = map fst binds
        targetForallRefs =
            let alg ty = case ty of
                    TForallIFRef ref _ body' -> ref : unK body'
                    TConIFWithIdentity _ _ args -> concatMap unK args
                    _ -> []
            in cataIxConst alg targetTy
        argsAreIdentity :: [ElabType] -> Bool
        argsAreIdentity args =
            and
                [ case arg of
                    TVarRef argRef ->
                        any (typeBinderRefsSameIdentity argRef) targetForallRefs
                    _ -> False
                | arg <- args
                ]
        -- Source applications may consume only a leading binder prefix.
        -- Keep that ordering rule and the escaping-identity rejection in one
        -- constructor so body matching and bound matching cannot drift.
        validatedPrefixArguments argsMaybe =
            let (prefixArgs, remainingArgs) = span isPresent argsMaybe
                args = [ty | Just ty <- prefixArgs]
            in if any isPresent remainingArgs || argsAreIdentity args
                then Nothing
                else Just args
        isPresent (Just _) = True
        isPresent Nothing = False
        inferFromBody =
            let argumentsFromSubst subst0 = do
                    subst <- closeDependentBoundSubstitutions subst0
                    validatedPrefixArguments
                        (map (`Map.lookup` subst) binderRefs)
                -- Matching the residual body can determine a later flexible
                -- binder before an earlier binder that occurs only in that
                -- binder's bound.  For example,
                --
                --   forall d. forall (f >= K d). b -> f
                --
                -- at @b -> K t@ first determines @f := K t@.  The declaration
                -- of @f@ then determines @d := t@.  Propagate those exact
                -- bound equations to a fixed point before enforcing the
                -- source-prefix rule.  Every resulting argument is still
                -- checked by the caller's real N computation, so this adds
                -- construction evidence rather than a type-shape fallback.
                closeDependentBoundSubstitutions subst0 = do
                    subst <- foldM extendFromBound subst0 binds
                    if Map.size subst == Map.size subst0
                        then Just subst
                        else closeDependentBoundSubstitutions subst
                extendFromBound subst (binderRef, mbBound) =
                    case (Map.lookup binderRef subst, mbBound) of
                        (Just argumentTy, Just bound) ->
                            let boundTy = tyToElab bound
                                missingDependencies =
                                    [ dependencyRef
                                    | dependencyRef <- binderRefs
                                    , Map.notMember dependencyRef subst
                                    , any
                                        (typeBinderRefsSameIdentity dependencyRef)
                                        (freeTypeVarRefsType boundTy)
                                    ]
                                specializedBound =
                                    substTypeSelectiveRefs [] subst boundTy
                            in if null missingDependencies
                                then Just subst
                                else
                                    case
                                        matchRefs
                                            missingDependencies
                                            specializedBound
                                            argumentTy
                                    of
                                        Left _ -> Just subst
                                        Right inferred ->
                                            foldM mergeExactSubstitution subst (Map.toList inferred)
                        _ -> Just subst
                mergeExactSubstitution subst (ref, inferredTy) =
                    case Map.lookup ref subst of
                        Nothing -> Just (Map.insert ref inferredTy subst)
                        Just existingTy
                            | typesAgree existingTy inferredTy -> Just subst
                            | otherwise -> Nothing
                fromMatch =
                    case matchRefs binderRefs body targetCore of
                        Left _ -> Nothing
                        Right subst -> argumentsFromSubst subst
                fromArrowPrefix =
                    let bindDomain substAcc bodyDom targetDom =
                            case bodyDom of
                                TVarRef ref
                                    | Just binderRef <- matchBinderRef ref ->
                                        case Map.lookup binderRef substAcc of
                                            Nothing -> Just (Map.insert binderRef targetDom substAcc)
                                            Just prev
                                                | typesAgree prev targetDom -> Just substAcc
                                                | otherwise -> Nothing
                                _
                                    | typesAgree bodyDom targetDom -> Just substAcc
                                    | otherwise -> Nothing
                        go substAcc bodyTy targetTy' =
                            case (bodyTy, targetTy') of
                                (TArrow bodyDom bodyCod, TArrow targetDom targetCod) -> do
                                    substNext <- bindDomain substAcc bodyDom targetDom
                                    go substNext bodyCod targetCod
                                _ -> Just substAcc
                    in do
                        subst <- go Map.empty body targetCore
                        argumentsFromSubst subst
            in fromMatch <|> fromArrowPrefix
        inferDefaultEliminations =
            -- A quantified variable may be absent from the body, so matching
            -- the body cannot infer an argument for it.  In that case the
            -- paper's N computation is canonical: eliminate each leading
            -- quantifier at its current lower bound, and accept the arguments
            -- only when the constructed computation reaches the exact
            -- semantic endpoint.
            let sourceTy =
                    foldr
                        (\(ref, mbBound) rest -> tForallWithRef ref mbBound rest)
                        body
                        binds
                go current remaining acc =
                    case remaining of
                        []
                            | alphaEqType current targetTy
                                || churchAwareEqType current targetTy -> Just (reverse acc)
                            | otherwise -> Nothing
                        _ : rest ->
                            case current of
                                TForallRef _ mbBound _ -> do
                                    current' <- either (const Nothing) Just (applyInstantiation current InstElim)
                                    let argument = maybe TBottom tyToElab mbBound
                                    go current' rest (argument : acc)
                                _ -> Nothing
            in go sourceTy binds []
        inferFromBound binderRef bound =
            let boundCore = stripForallsType bound
                matchVars = map canonicalSchemeRef (varRefsInType boundCore)
                canonicalSchemeRef ref =
                    case find (typeBinderRefsSameIdentity ref) binderRefs of
                        Just binderRef' -> binderRef'
                        Nothing -> ref
            in case matchRefs matchVars boundCore targetCore of
                Left _ -> Nothing
                Right subst ->
                    let innerVars =
                            filter
                                (\ref -> not (any (typeBinderRefsSameIdentity ref) binderRefs))
                                matchVars
                        pickInnerArg =
                            case innerVars of
                                [inner] -> Map.lookup inner subst
                                _ -> Nothing
                        argFor ref =
                            if typeBinderRefsSameIdentity ref binderRef
                                then
                                    case pickInnerArg of
                                        Just innerArg -> Just innerArg
                                        Nothing -> Just (substTypeSelectiveRefs binderRefs subst boundCore)
                                else Map.lookup ref subst
                        argsMaybe = map argFor binderRefs
                    in validatedPrefixArguments argsMaybe
    in case body of
        TVarRef ref ->
            case find (typeBinderRefsSameIdentity ref . fst) binds of
                Just (binderRef, Just bound) ->
                    inferFromBound binderRef (tyToElab bound)
                        <|> inferDefaultEliminations
                Just (_, Nothing) -> inferFromBody <|> inferDefaultEliminations
                Nothing -> Nothing
        _ -> inferFromBody <|> inferDefaultEliminations
  where
    matchBinderRef ref =
        find (typeBinderRefsSameIdentity ref) (map fst binds)

varRefsInType :: ElabType -> [TypeBinderRef]
varRefsInType = cataIxConst alg
  where
    alg :: TyIF i (K [TypeBinderRef]) -> [TypeBinderRef]
    alg ty = case ty of
        TVarIFRef ref -> [ref]
        TArrowIF a b -> dedupeRefs (unK a ++ unK b)
        TConIFWithIdentity _ _ args -> dedupeRefs (concatMap unK args)
        TVarAppIFRef ref args -> dedupeRefs (ref : concatMap unK args)
        TBaseIFWithIdentity _ _ -> []
        TBottomIF -> []
        TForallIFRef _ mb body ->
            let varsBound = maybe [] unK mb
            in dedupeRefs (varsBound ++ unK body)
        TMuIFRef _ body -> unK body

dedupeRefs :: [TypeBinderRef] -> [TypeBinderRef]
dedupeRefs =
    foldr insertRef []
  where
    insertRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

substTypeSelectiveRefs :: [TypeBinderRef] -> Map.Map TypeBinderRef ElabType -> ElabType -> ElabType
substTypeSelectiveRefs binderRefs subst ty0 = runSubstFun (cataIx alg ty0) []
  where
    alg :: TyIF i SubstFun -> SubstFun i
    alg ty = case ty of
        TVarIFRef ref ->
            SubstFun $ \bound ->
                if refMember ref bound || refMember ref binderRefs
                    then TVarRef ref
                    else case lookupRef ref subst of
                        Just ty' -> ty'
                        Nothing -> TVarRef ref
        TArrowIF a b ->
            SubstFun $ \bound -> TArrow (runSubstFun a bound) (runSubstFun b bound)
        TConIFWithIdentity identity c args ->
            SubstFun $ \bound -> TConWithIdentity identity c (fmap (\f -> runSubstFun f bound) args)
        TVarAppIFRef ref args ->
            SubstFun $ \bound ->
                let args' = fmap (\f -> runSubstFun f bound) args
                in if refMember ref bound || refMember ref binderRefs
                    then TVarAppRef ref args'
                    else case lookupRef ref subst of
                        Just ty' -> composeTypeHeadRef ref ty' args'
                        Nothing -> TVarAppRef ref args'
        TBaseIFWithIdentity identity b -> SubstFun (const (TBaseWithIdentity identity b))
        TBottomIF -> SubstFun (const TBottom)
        TForallIFRef ref mb body ->
            SubstFun $ \bound ->
                let bound' = insertRef ref bound
                    mb' = fmap (\f -> runSubstFun f bound') mb
                    body' = runSubstFun body bound'
                in TForallRef ref mb' body'
        TMuIFRef ref body ->
            SubstFun $ \bound ->
                let bound' = insertRef ref bound
                in TMuRef ref (runSubstFun body bound')

    insertRef ref refs
        | refMember ref refs = refs
        | otherwise = ref : refs

    refMember ref =
        any (typeBinderRefsSameIdentity ref)

    lookupRef ref substMap =
        snd <$> find (typeBinderRefsSameIdentity ref . fst) (Map.toList substMap)

instInsideFromArgsWithBoundsRefs :: [(TypeBinderRef, Maybe BoundType)] -> [ElabType] -> Maybe Instantiation
instInsideFromArgsWithBoundsRefs binds args = go binds args
  where
    go [] _ = Just InstId
    go _ [] = Just InstId
    go ((ref, mbBound):ns) (t:ts) = do
        rest <- go ns ts
        inst <- instFor mbBound t
        pure $ case (inst, rest) of
            (InstId, InstId) -> InstId
            (InstId, _) -> instUnderWithRef ref rest
            (_, InstId) -> inst
            _ -> InstSeq inst (instUnderWithRef ref rest)

    instFor :: Maybe BoundType -> ElabType -> Maybe Instantiation
    instFor mbBound t = case mbBound of
        Nothing -> Just (InstInside (InstBot t))
        Just bound
            | containsForallTy bound -> Just (InstInside (InstApp t))
            | alphaEqType boundTy TBottom -> Just (InstInside (InstBot t))
            | alphaEqType boundTy t -> Just InstId
            | otherwise -> Nothing
          where
            boundTy = tyToElab bound

containsForallType :: ElabType -> Bool
containsForallType = cataIxConst alg
  where
    alg ty = case ty of
        TForallIFRef _ _ _ -> True
        TMuIFRef _ body -> unK body
        TArrowIF a b -> unK a || unK b
        TConIFWithIdentity _ _ args -> any unK args
        _ -> False
