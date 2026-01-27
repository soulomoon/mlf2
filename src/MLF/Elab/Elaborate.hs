{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module MLF.Elab.Elaborate (
    expansionToInst,
    elaborate,
    elaborateWithGen,
    elaborateWithScope
) where

import Data.Functor.Foldable (cata, para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import MLF.Util.Names (parseNameId)
import MLF.Reify.TypeOps (alphaEqType, inlineBaseBoundsType, matchType)
import MLF.Util.RecursionSchemes (cataM)

import MLF.Frontend.Syntax (VarName)
import MLF.Constraint.Types
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeForBound)
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import qualified MLF.Elab.Inst as Inst
import MLF.Reify.Core
    ( namedNodes
    , reifyBoundWithNames
    , reifyTypeWithNamedSetNoFallback
    , reifyTypeWithNamesNoFallback
    )
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution
    ( EdgeTrace
    , etBinderArgs
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr(..), AnnExprF(..))

type GeneralizeAtWith =
    Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

-- | Convert an Expansion to an Instantiation witness.
-- This translates the presolution expansion recipe into an xMLF instantiation.
--
-- Note on paper alignment:
-- Presolution's expansions (Instantiate, Forall, Compose) map roughly to
-- xMLF's instantiations (App/Elim, Intro, Seq).
-- Specifically:
--   - ExpInstantiate args: In xMLF, instantiation is N (Elim) followed by
--     substitution. Presolution does "forall elimination + substitution" in one step.
--     We map this to a sequence of (N; ⟨τ⟩) or just ⟨τ⟩ depending on context,
--     but since xMLF ⟨τ⟩ usually implies elimination in standard F, we model
--     ExpInstantiate as a sequence of type applications ⟨τ⟩ which implicitly
--     includes the elimination step N where needed, or explicit N if args are empty.
--     Actually, looking at `papers/these-finale-english.txt` (see `papers/xmlf.txt`):
--       (∀(α ⩾ τ) τ') N  --> τ'{α ← τ}  (Eliminate quantifier)
--       (∀(α ⩾ τ) τ') !α --> τ'{α ← τ}  (Abstract bound)
--     Presolution's ExpInstantiate [t1..tn] means "instantiate the outermost
--     quantifiers with t1..tn". This corresponds to N; ⟨t1⟩; ...; N; ⟨tn⟩.
--     However, standard presentation often folds N into the application.
--     For strict adherence, we should emit N for every quantifier eliminated.
--     But ExpInstantiate removes the quantifier *and* substitutes.
--     We map ExpInstantiate [t] to (N; ⟨t⟩) if it replaces a bounded var,
--     or just ⟨t⟩ if it's a standard F app.
--     Given we don't track the *source* type here easily, we generate a sequence
--     of applications ⟨t⟩, assuming the elaboration context or a later pass
--     refines this if explicit N is required.
--     For now: ExpInstantiate [t] -> ⟨t⟩.
--
expansionToInst :: SolveResult -> Expansion -> Either ElabError Instantiation
expansionToInst res = cataM alg
  where
    constraint = srConstraint res
    nodes = cNodes constraint
    canonical = Solve.frWith (srUnionFind res)
    resolveBaseBound start =
        let go visited nid =
                let nidC = canonical nid
                    key = getNodeId nidC
                in if IntSet.member key visited
                    then Nothing
                    else
                        case IntMap.lookup key nodes of
                            Just TyBase{} -> Just nidC
                            Just TyBottom{} -> Just nidC
                            Just TyVar{} ->
                                case VarStore.lookupVarBound constraint nidC of
                                    Just bnd -> go (IntSet.insert key visited) bnd
                                    Nothing -> Nothing
                            _ -> Nothing
        in go IntSet.empty start
    reifyArg arg =
        let argC = canonical arg
        in case resolveBaseBound argC of
            Just baseC -> reifyTypeWithNamesNoFallback res IntMap.empty baseC
            Nothing -> reifyTypeWithNamesNoFallback res IntMap.empty argC
    -- See Note [Instantiation arg sanitization] in
    -- docs/notes/2026-01-27-elab-changes.md.
    sanitizeArg = inlineBoundVarsTypeForBound res
    alg layer = case layer of
        ExpIdentityF -> Right InstId
        ExpInstantiateF args -> do
            tys <- mapM reifyArg args
            let tys' = map sanitizeArg tys
            -- Build a sequence of type applications.
            -- In xMLF, simple application is usually sufficient.
            -- If we needed explicit N, we'd need to know the source type schema.
            if null tys'
                then Right InstId
                else Right $ foldr1 InstSeq (map InstApp tys')
        ExpForallF _ -> Right InstIntro
        ExpComposeF exps -> Right $ foldr1 InstSeq (NE.toList exps)

type Env = Map.Map VarName SchemeInfo

data ElabOut = ElabOut
    { elabTerm :: Env -> Either ElabError ElabTerm
    , elabStripped :: Env -> Either ElabError ElabTerm
    }

schemeBodyTarget :: SolveResult -> NodeId -> NodeId
schemeBodyTarget res target =
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
        targetC = canonical target
    in case IntMap.lookup (getNodeId targetC) (cNodes constraint) of
        Just TyVar{ tnBound = Just bnd } ->
            case IntMap.lookup (getNodeId (canonical bnd)) (cNodes constraint) of
                Just TyForall{ tnBody = body } -> canonical body
                _ -> canonical bnd
        Just TyForall{ tnBody = body } -> canonical body
        _ -> targetC

elaborate
    :: GeneralizeAtWith
    -> SolveResult
    -> SolveResult
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborate generalizeAtWith resPhi resGen edgeWitnesses edgeTraces edgeExpansions ann =
    let constraint = srConstraint resGen
        keys = IntMap.keys (cNodes constraint)
        baseToSolved =
            IntMap.fromList
                [ (k, NodeId k)
                | k <- keys
                ]
        solvedToBase =
            IntMap.fromList
                [ (k, NodeId k)
                | k <- keys
                ]
        gaParents = GaBindParents
            { gaBindParentsBase = cBindParents constraint
            , gaBaseConstraint = constraint
            , gaBaseToSolved = baseToSolved
            , gaSolvedToBase = solvedToBase
            }
    in elaborateWithGen generalizeAtWith resPhi resGen resGen gaParents edgeWitnesses edgeTraces edgeExpansions ann

elaborateWithGen
    :: GeneralizeAtWith
    -> SolveResult
    -> SolveResult
    -> SolveResult
    -> GaBindParents
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborateWithGen generalizeAtWith resPhi resReify resGen gaParents edgeWitnesses edgeTraces edgeExpansions ann =
    elaborateWithScope generalizeAtWith resPhi resReify resGen gaParents edgeWitnesses edgeTraces edgeExpansions IntMap.empty ann

elaborateWithScope
    :: GeneralizeAtWith
    -> SolveResult
    -> SolveResult
    -> SolveResult
    -> GaBindParents
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> IntMap.IntMap NodeRef
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborateWithScope generalizeAtWith resPhi resReify resGen gaParents edgeWitnesses edgeTraces edgeExpansions scopeOverrides ann = do
    namedSetPhi <- namedNodes resPhi
    namedSetReify <- namedNodes resReify
    let ElabOut { elabTerm = runElab } = para (elabAlg namedSetPhi namedSetReify) ann
    runElab Map.empty
  where
    canonical = Solve.frWith (srUnionFind resReify)
    scopeRootFromBase root =
        case IntMap.lookup (getNodeId (canonical root)) (gaSolvedToBase gaParents) of
            Nothing -> typeRef root
            Just baseN ->
                case bindingPathToRootLocal (gaBindParentsBase gaParents) (typeRef baseN) of
                    Left _ -> typeRef root
                    Right path ->
                        case listToMaybe [gid | GenRef gid <- drop 1 path] of
                            Just gid -> GenRef gid
                            Nothing -> typeRef root

    mkOut :: (Env -> Either ElabError ElabTerm) -> ElabOut
    mkOut f = ElabOut f f

    elabAlg :: IntSet.IntSet -> IntSet.IntSet -> AnnExprF (AnnExpr, ElabOut) -> ElabOut
    elabAlg namedSetPhi namedSetReify layer = case layer of
        AVarF v _ -> mkOut $ \env ->
            maybe (Left (EnvLookup v)) (const (Right (EVar v))) (Map.lookup v env)
        ALitF lit _ -> mkOut $ \_ -> Right (ELit lit)
        ALamF v n _ (_bodyAnn, bodyOut) _ ->
            let f env = do
                    ty <- reifyTypeWithNamedSetNoFallback resReify IntMap.empty namedSetReify n
                    -- Add lambda parameter to env as a scheme derived from the annotated type,
                    -- so κσ can reorder/instantiate quantified parameters when present.
                    let paramScheme = SchemeInfo { siScheme = schemeFromType ty, siSubst = IntMap.empty }
                        env' = Map.insert v paramScheme env
                    body' <- elabTerm bodyOut env'
                    pure (ELam v ty body')
            in mkOut f
        AAppF (fAnn, fOut) (aAnn, aOut) funEid argEid _ ->
            let f env = do
                    f' <- elabTerm fOut env
                    a' <- elabTerm aOut env
                    funInst0 <- reifyInst namedSetReify env fAnn funEid
                    let argNode = annNode aAnn
                        argType = case VarStore.lookupVarBound (srConstraint resPhi) argNode of
                            Just bnd -> reifyTypeForInstArg namedSetPhi resPhi bnd
                            Nothing -> reifyTypeForInstArg namedSetPhi resPhi argNode
                    let funInst =
                            let baseInst =
                                    case (funInst0, IntMap.lookup (getEdgeId funEid) edgeExpansions) of
                                        (InstId, Just (ExpInstantiate args))
                                            | not (null args) ->
                                                case argType of
                                                    Right argTy -> InstApp argTy
                                                    Left _ -> funInst0
                                        (InstApp TBottom, Just (ExpInstantiate args))
                                            | not (null args) ->
                                                case argType of
                                                    Right argTy | argTy /= TBottom -> InstApp argTy
                                                    _ -> funInst0
                                        _ -> funInst0
                            in case (argType, baseInst) of
                                (Right argTy, InstApp _) -> InstApp argTy
                                (Right argTy, _)
                                    | countInstApps baseInst == 1 ->
                                        let (inst', _rest) = replaceInstApps [argTy] baseInst
                                        in inst'
                                _ -> baseInst
                    argInst <- reifyInst namedSetReify env aAnn argEid
                    let argIsPoly =
                            case aAnn of
                                AVar v _ ->
                                    case Map.lookup v env of
                                        Just SchemeInfo{ siScheme = Forall binds _ } -> not (null binds)
                                        Nothing -> False
                                _ -> False
                    let argInst' =
                            if argIsPoly
                                then case (argInst, funInst) of
                                    (InstId, InstApp ty) -> InstApp ty
                                    _ -> argInst
                                else argInst
                    let fApp = case funInst of
                            InstId -> f'
                            _      -> ETyInst f' funInst
                        aApp = case argInst' of
                            InstId -> a'
                            _      -> ETyInst a' argInst'
                    pure (EApp fApp aApp)
            in mkOut f
        ALetF v schemeGenId schemeRootId _ _rhsScopeGen (_rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
            let f env = do
                    rhs' <- elabTerm rhsOut env
                    let scopeRoot =
                            case IntMap.lookup (getNodeId (canonical schemeRootId)) scopeOverrides of
                                Just ref -> ref
                                Nothing -> scopeRootFromBase schemeRootId
                    _ <- pure $
                        debugElabGeneralize
                            ("elaborate let: schemeGenId=" ++ show schemeGenId
                                ++ " schemeRootId=" ++ show schemeRootId
                                ++ " scopeRoot=" ++ show scopeRoot
                            )
                            ()
                    let targetC = schemeBodyTarget resGen schemeRootId
                    (sch, subst) <-
                        generalizeAtWith (Just gaParents) resGen scopeRoot targetC
                    case debugElabGeneralize
                        ("elaborate let: scheme=" ++ show sch
                            ++ " subst=" ++ show subst
                        )
                        () of
                        () -> pure ()
                    let rhsSubst = substInTerm subst rhs'
                    let env' = Map.insert v SchemeInfo { siScheme = sch, siSubst = subst } env
                        rhsAbs = case sch of
                            Forall binds _ -> foldr (\(name, bound) e -> ETyAbs name bound e) rhsSubst binds
                    let bodyElab =
                            case bodyAnn of
                                AAnn _ target _ | canonical target == canonical trivialRoot ->
                                    elabStripped bodyOut
                                _ -> elabTerm bodyOut
                    body' <- bodyElab env'
                    pure (ELet v sch rhsAbs body')
            in mkOut f
        AAnnF (exprAnn, exprOut) _ eid ->
            ElabOut
                { elabTerm = \env -> do
                    expr' <- elabTerm exprOut env
                    inst <- reifyInst namedSetReify env exprAnn eid
                    pure $ case inst of
                        InstId -> expr'
                        _      -> ETyInst expr' inst
                , elabStripped = \env -> elabTerm exprOut env
                }

    -- | Convert an edge witness to an xMLF instantiation witness.
    --
    -- If the function position is a variable, we use its generalized scheme
    -- (and the NodeId→name substitution from `generalizeAt`) to support Σ(g)
    -- reordering and targeted instantiation.
    reifyInst :: IntSet.IntSet -> Env -> AnnExpr -> EdgeId -> Either ElabError Instantiation
    reifyInst namedSetReify env funAnn (EdgeId eid) =
        let dbg =
                debugElabGeneralize
                    ("reifyInst: edge=" ++ show eid
                        ++ " witness=" ++ show (IntMap.member eid edgeWitnesses)
                        ++ " trace=" ++ show (IntMap.member eid edgeTraces)
                        ++ " exp=" ++ show (IntMap.member eid edgeExpansions)
                    )
                    ()
        in dbg `seq`
        case IntMap.lookup eid edgeWitnesses of
            Nothing ->
                debugElabGeneralize
                    ("reifyInst: missing witness for edge " ++ show eid)
                    (Right InstId)
            Just ew -> do
                let mTrace = IntMap.lookup eid edgeTraces
                    mExpansion = IntMap.lookup eid edgeExpansions
                let mSchemeInfo = case funAnn of
                        AVar v _ -> Map.lookup v env
                        _ -> Nothing
                    mSchemeInfo' =
                        case mSchemeInfo of
                            Just si
                                | IntMap.null (siSubst si)
                                , Forall binds _ <- siScheme si
                                , null binds ->
                                    Nothing
                            _ -> mSchemeInfo
                phi0 <- phiFromEdgeWitnessWithTrace generalizeAtWith resReify (Just gaParents) mSchemeInfo' mTrace ew
                let phi = patchInstAppsFromTarget namedSetReify ew mSchemeInfo phi0
                instFromTrace <- case (phi, mExpansion) of
                    (InstId, Just (ExpInstantiate args)) -> do
                        let argNodes =
                                case mTrace of
                                    Just tr | not (null (etBinderArgs tr)) ->
                                        map snd (etBinderArgs tr)
                                    _ -> args
                        let targetArgs =
                                case mSchemeInfo of
                                    Just si ->
                                        case reifyTargetType namedSetReify ew si of
                                            Right targetTy ->
                                                case inferInstAppArgs (siScheme si) targetTy of
                                                    Just inferred
                                                        | length inferred == length argNodes ->
                                                            Just inferred
                                                    _ -> Nothing
                                            Left _ -> Nothing
                                    Nothing -> Nothing
                        let substForArgs =
                                case mSchemeInfo of
                                    Just si -> siSubst si
                                    Nothing -> IntMap.empty
                            constraint = srConstraint resReify
                            reifyArg arg =
                                case VarStore.lookupVarBound constraint arg of
                                    Just bnd -> reifyBoundWithNames resReify substForArgs bnd
                                    Nothing -> reifyTypeWithNamedSetNoFallback resReify substForArgs namedSetReify arg
                        argTys <- case targetArgs of
                            Just inferred -> pure inferred
                            Nothing -> mapM reifyArg argNodes
                        let argTys' = map (inlineBoundVarsTypeForBound resReify) argTys
                        pure (Just (instSeqApps argTys'))
                    _ -> pure Nothing
                case instFromTrace of
                    Just inst -> Right inst
                    Nothing -> simplifyInstForScheme namedSetReify mSchemeInfo mExpansion phi

    simplifyInstForScheme
        :: IntSet.IntSet
        -> Maybe SchemeInfo
        -> Maybe Expansion
        -> Instantiation
        -> Either ElabError Instantiation
    simplifyInstForScheme namedSetReify mSchemeInfo mExpansion phi =
        case (mSchemeInfo, mExpansion) of
            (Just si, Just (ExpInstantiate args)) -> do
                let subst = siSubst si
                    schemeTy = schemeToType (siScheme si)
                let constraint = srConstraint resReify
                    reifyArg arg =
                        case VarStore.lookupVarBound constraint arg of
                            Just bnd -> reifyBoundWithNames resReify subst bnd
                            Nothing -> reifyTypeWithNamedSetNoFallback resReify subst namedSetReify arg
                argTys <- mapM reifyArg args
                let argTys' = map (inlineBoundVarsTypeForBound resReify) argTys
                case applyInstantiation schemeTy phi of
                    Left _ -> Right phi
                    Right targetTy -> do
                        let candidates = [instSeqApps (take k argTys') | k <- [0 .. length argTys']]
                            matches inst =
                                case applyInstantiation schemeTy inst of
                                    Right ty -> alphaEqType ty targetTy
                                    Left _ -> False
                        pure $ case filter matches candidates of
                            (inst:_) -> inst
                            [] -> phi
            _ -> Right phi

    patchInstAppsFromTarget :: IntSet.IntSet -> EdgeWitness -> Maybe SchemeInfo -> Instantiation -> Instantiation
    patchInstAppsFromTarget namedSetReify ew mSchemeInfo inst =
        case mSchemeInfo of
            Nothing -> inst
            Just si ->
                case reifyTargetType namedSetReify ew si of
                    Left _ -> inst
                    Right targetTy ->
                        case inferInstAppArgs (siScheme si) targetTy of
                            Nothing -> inst
                            Just args ->
                                let args' = map (inlineBoundVarsTypeForBound resReify) args
                                    (inst', _unused) = replaceInstApps args' inst
                                in inst'

    reifyTargetType :: IntSet.IntSet -> EdgeWitness -> SchemeInfo -> Either ElabError ElabType
    reifyTargetType namedSetReify ew si =
        let subst = siSubst si
        in case VarStore.lookupVarBound (srConstraint resReify) (ewRight ew) of
            Just bnd -> reifyTypeWithNamedSetNoFallback resReify subst namedSetReify bnd
            Nothing -> reifyTypeWithNamedSetNoFallback resReify subst namedSetReify (ewRight ew)

    inferInstAppArgs :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferInstAppArgs scheme targetTy =
        let (binds, body) = Inst.splitForalls (schemeToType scheme)
            binderNames = map fst binds
        in case matchType (Set.fromList binderNames) body targetTy of
            Left _ -> Nothing
            Right subst ->
                if all (`Map.member` subst) binderNames
                    then Just [ty | name <- binderNames, Just ty <- [Map.lookup name subst]]
                    else Nothing

    replaceInstApps :: [ElabType] -> Instantiation -> (Instantiation, [ElabType])
    replaceInstApps args0 inst0 = (cata alg inst0) args0
      where
        alg inst args = case inst of
            InstIdF -> (InstId, args)
            InstSeqF a b ->
                    let (a', args1) = a args
                        (b', args2) = b args1
                    in (InstSeq a' b', args2)
            InstAppF t ->
                case args of
                    (t':rest) -> (InstApp t', rest)
                    [] -> (InstApp t, [])
            InstBotF t -> (InstBot t, args)
            InstAbstrF v -> (InstAbstr v, args)
            InstIntroF -> (InstIntro, args)
            InstElimF -> (InstElim, args)
            InstInsideF phi ->
                    let (phi', rest) = phi args
                    in (InstInside phi', rest)
            InstUnderF v phi ->
                    let (phi', rest) = phi args
                    in (InstUnder v phi', rest)

    countInstApps :: Instantiation -> Int
    countInstApps = cata alg
      where
        alg inst0 = case inst0 of
            InstIdF -> 0
            InstSeqF a b -> a + b
            InstAppF _ -> 1
            InstBotF _ -> 0
            InstAbstrF _ -> 0
            InstIntroF -> 0
            InstElimF -> 0
            InstInsideF phi -> phi
            InstUnderF _ phi -> phi

debugElabGeneralize :: String -> a -> a
debugElabGeneralize msg value =
    if debugElabGeneralizeEnabled
        then trace msg value
        else value

debugElabGeneralizeEnabled :: Bool
debugElabGeneralizeEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_GENERALIZE"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugElabGeneralizeEnabled #-}

-- | Substitute names in a term (and its embedded types)
substInTerm :: IntMap.IntMap String -> ElabTerm -> ElabTerm
substInTerm subst = cata alg
  where
    alg term = case term of
        EVarF v -> EVar v
        ELitF l -> ELit l
        ELamF v ty body -> ELam v (substInTy subst ty) body
        EAppF f a -> EApp f a
        ELetF v sch rhs body -> ELet v (substInScheme subst sch) rhs body
        ETyAbsF v b body -> ETyAbs v (fmap (substInTy subst) b) body
        ETyInstF e i -> ETyInst e (substInInst subst i)

substInTy :: IntMap.IntMap String -> Ty v -> Ty v
substInTy subst = cataIx alg
  where
    alg :: TyIF i Ty -> Ty i
    alg node = case node of
        TVarIF v -> TVar (applySubst v)
        TArrowIF d c -> TArrow d c
        TBaseIF b -> TBase b
        TForallIF v mb body -> TForall v mb body
        TBottomIF -> TBottom

    applySubst name =
        case parseNameId name of
            Just nid -> case IntMap.lookup nid subst of
                Just newName -> newName
                Nothing -> name
            Nothing -> name

substInScheme :: IntMap.IntMap String -> ElabScheme -> ElabScheme
substInScheme subst scheme =
    schemeFromType (substInTy subst (schemeToType scheme))

substInInst :: IntMap.IntMap String -> Instantiation -> Instantiation
substInInst subst = cata alg
  where
    alg inst = case inst of
        InstIdF -> InstId
        InstAppF t -> InstApp (substInTy subst t)
        InstBotF t -> InstBot (substInTy subst t)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrF v -> InstAbstr v
        InstUnderF v i' -> InstUnder v i'
        InstInsideF i' -> InstInside i'
        InstSeqF i1 i2 -> InstSeq i1 i2

instSeqApps :: [ElabType] -> Instantiation
instSeqApps tys = case map InstApp tys of
    [] -> InstId
    [inst] -> inst
    insts -> foldr1 InstSeq insts

reifyTypeForInstArg :: IntSet.IntSet -> SolveResult -> NodeId -> Either ElabError ElabType
reifyTypeForInstArg namedSet res nid = do
    ty <- reifyTypeWithNamedSetNoFallback res IntMap.empty namedSet nid
    let ty' = inlineBaseBounds res ty
    pure (inlineBoundVarsTypeForBound res ty')

inlineBaseBounds :: SolveResult -> ElabType -> ElabType
inlineBaseBounds res =
    inlineBaseBoundsType
        (srConstraint res)
        (Solve.frWith (srUnionFind res))

annNode :: AnnExpr -> NodeId
annNode ann = case ann of
    ALit _ nid -> nid
    AVar _ nid -> nid
    ALam _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid
