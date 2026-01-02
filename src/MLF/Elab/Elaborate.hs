module MLF.Elab.Elaborate (
    expansionToInst,
    elaborate
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)

import MLF.Frontend.Syntax (VarName)
import MLF.Constraint.Types
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Elab.Types
import MLF.Elab.Generalize (generalizeAt)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Reify (reifyTypeWithNames)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution (EdgeTrace)
import MLF.Frontend.ConstraintGen.Types (AnnExpr(..))

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
--     Actually, looking at xmlf.txt:
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
expansionToInst res expn = case expn of
    ExpIdentity -> Right InstId
    ExpInstantiate args -> do
        tys <- mapM (reifyTypeWithNames res IntMap.empty) args
        -- Build a sequence of type applications.
        -- In xMLF, simple application is usually sufficient.
        -- If we needed explicit N, we'd need to know the source type schema.
        if null tys
            then Right InstId
            else Right $ foldr1 InstSeq (map InstApp tys)
    ExpForall _ -> Right InstIntro
    ExpCompose exps -> do
        insts <- mapM (expansionToInst res) (NE.toList exps)
        Right $ foldr1 InstSeq insts

type Env = Map.Map VarName SchemeInfo

elaborate
    :: SolveResult
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborate res edgeWitnesses edgeTraces edgeExpansions ann = go Map.empty ann
  where
    canonical = Solve.frWith (srUnionFind res)

    go :: Env -> AnnExpr -> Either ElabError ElabTerm
    go env ae = case ae of
        AVar v _ -> maybe (Left (EnvLookup v)) (const (Right (EVar v))) (Map.lookup v env)
        ALit lit _ -> Right (ELit lit)
        ALam v n _ body _ -> do
            ty <- reifyTypeWithNames res IntMap.empty n
            -- Add lambda parameter to env with a monomorphic "scheme"
            let paramScheme = SchemeInfo { siScheme = Forall [] ty, siSubst = IntMap.empty }
                env' = Map.insert v paramScheme env
            body' <- go env' body
            pure (ELam v ty body')
        AApp f a funEid argEid _ -> do
            f' <- go env f
            a' <- go env a
            funInst <- reifyInst env f funEid
            argInst <- reifyInst env a argEid
            let fApp = case funInst of
                    InstId -> f'
                    _      -> ETyInst f' funInst
                aApp = case argInst of
                    InstId -> a'
                    _      -> ETyInst a' argInst
            pure (EApp fApp aApp)
        ALet v schemeGen schemeRoot _ rhsScopeGen rhs body _ -> do
            rhs' <- go env rhs
            let scopeRoot =
                    if schemeGen /= rhsScopeGen
                        then typeRef (canonical schemeRoot)
                        else genRef schemeGen
            (sch, subst) <- generalizeAt res scopeRoot schemeRoot
            let rhsSubst = substInTerm subst rhs'
            let env' = Map.insert v SchemeInfo { siScheme = sch, siSubst = subst } env
                rhsAbs = case sch of
                    Forall binds _ -> foldr (\(name, bound) e -> ETyAbs name bound e) rhsSubst binds
            body' <- go env' body
            pure (ELet v sch rhsAbs body')
        AAnn expr _ eid -> do
            expr' <- go env expr
            inst <- reifyInst env expr eid
            pure $ case inst of
                InstId -> expr'
                _      -> ETyInst expr' inst

    -- | Convert an edge witness to an xMLF instantiation witness.
    --
    -- If the function position is a variable, we use its generalized scheme
    -- (and the NodeId→name substitution from `generalizeAt`) to support Σ(g)
    -- reordering and targeted instantiation.
    reifyInst :: Env -> AnnExpr -> EdgeId -> Either ElabError Instantiation
    reifyInst env funAnn (EdgeId eid) =
        case IntMap.lookup eid edgeWitnesses of
            Nothing -> Right InstId
            Just ew -> do
                let mTrace = IntMap.lookup eid edgeTraces
                    mExpansion = IntMap.lookup eid edgeExpansions
                let mSchemeInfo = case funAnn of
                        AVar v _ -> Map.lookup v env
                        _ -> Nothing
                    mSchemeInfo' = case mSchemeInfo of
                        Just si | IntMap.null (siSubst si) -> Nothing
                        _ -> mSchemeInfo
                phi <- phiFromEdgeWitnessWithTrace res mSchemeInfo' mTrace ew
                simplifyInstForScheme mSchemeInfo mExpansion phi

    simplifyInstForScheme
        :: Maybe SchemeInfo
        -> Maybe Expansion
        -> Instantiation
        -> Either ElabError Instantiation
    simplifyInstForScheme mSchemeInfo mExpansion phi =
        case (mSchemeInfo, mExpansion) of
            (Just si, Just (ExpInstantiate args)) -> do
                let subst = siSubst si
                    schemeTy = schemeToType (siScheme si)
                argTys <- mapM (reifyTypeWithNames res subst) args
                case applyInstantiation schemeTy phi of
                    Left _ -> Right phi
                    Right targetTy -> do
                        let candidates = [instSeqApps (take k argTys) | k <- [0 .. length argTys]]
                            matches inst =
                                case applyInstantiation schemeTy inst of
                                    Right ty -> alphaEqType ty targetTy
                                    Left _ -> False
                        pure $ case filter matches candidates of
                            (inst:_) -> inst
                            [] -> phi
            _ -> Right phi

-- | Substitute names in a term (and its embedded types)
substInTerm :: IntMap.IntMap String -> ElabTerm -> ElabTerm
substInTerm subst term = case term of
    EVar v -> EVar v
    ELit l -> ELit l
    ELam v ty body -> ELam v (substInType subst ty) (substInTerm subst body)
    EApp f a -> EApp (substInTerm subst f) (substInTerm subst a)
    ELet v sch rhs body -> ELet v (substInScheme subst sch) (substInTerm subst rhs) (substInTerm subst body)
    ETyAbs v b body -> ETyAbs v (fmap (substInType subst) b) (substInTerm subst body)
    ETyInst e i -> ETyInst (substInTerm subst e) (substInInst subst i)

substInType :: IntMap.IntMap String -> ElabType -> ElabType
substInType subst t = case t of
    TVar v -> TVar (applySubst v)
    TArrow d c -> TArrow (substInType subst d) (substInType subst c)
    TBase b -> TBase b
    TForall v b t' -> TForall v (fmap (substInType subst) b) (substInType subst t')
    TBottom -> TBottom
  where
    applySubst name =
        case parseName name of
            Just nid -> case IntMap.lookup nid subst of
                Just newName -> newName
                Nothing -> name
            Nothing -> name

    parseName ('t':rest) = readMaybe rest
    parseName _ = Nothing

substInScheme :: IntMap.IntMap String -> ElabScheme -> ElabScheme
substInScheme subst (Forall binds ty) =
    Forall (map (\(n, b) -> (n, fmap (substInType subst) b)) binds) (substInType subst ty)

substInInst :: IntMap.IntMap String -> Instantiation -> Instantiation
substInInst subst i = case i of
    InstId -> InstId
    InstApp t -> InstApp (substInType subst t)
    InstBot t -> InstBot (substInType subst t)
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstr v -> InstAbstr v
    InstUnder v i' -> InstUnder v (substInInst subst i')
    InstInside i' -> InstInside (substInInst subst i')
    InstSeq i1 i2 -> InstSeq (substInInst subst i1) (substInInst subst i2)

instSeqApps :: [ElabType] -> Instantiation
instSeqApps tys = case map InstApp tys of
    [] -> InstId
    [inst] -> inst
    insts -> foldr1 InstSeq insts

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = go Map.empty Map.empty
  where
    go envL envR t1 t2 = case (t1, t2) of
        (TVar a, TVar b) ->
            case Map.lookup a envL of
                Just b' -> b == b'
                Nothing -> case Map.lookup b envR of
                    Just a' -> a == a'
                    Nothing -> a == b
        (TArrow a1 b1, TArrow a2 b2) ->
            go envL envR a1 a2 && go envL envR b1 b2
        (TBase b1, TBase b2) -> b1 == b2
        (TBottom, TBottom) -> True
        (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
            let bound1 = maybe TBottom id mb1
                bound2 = maybe TBottom id mb2
                envL' = Map.insert v1 v2 envL
                envR' = Map.insert v2 v1 envR
            in go envL envR bound1 bound2 && go envL' envR' body1 body2
        _ -> False
