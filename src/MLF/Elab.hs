{-# LANGUAGE LambdaCase #-}
module MLF.Elab (
    ElabType(..),
    ElabScheme(..),
    ElabTerm(..),
    Instantiation(..),
    ElabError(..),
    Pretty(..),
    elaborate,
    reifyType,
    reifyTypeWithBound,
    generalizeAt,
    expansionToInst,
    applyInstantiation,
    sigmaReorder,
    -- * Witness translation (for tests/debugging)
    phiFromEdgeWitness,
    runPipelineElab,
    applyRedirectsToAnn,
    -- * Exported for testing/debugging
    chaseRedirects,
    SchemeInfo(..)
) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (elemIndex, nub, sort, sortOn)
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)

import MLF.Syntax (VarName, Lit(..), Expr)
import MLF.Types
import MLF.Solve hiding (MissingNode)
import qualified MLF.Solve as Solve (frWith)
import MLF.ConstraintGen (AnnExpr(..), ConstraintResult(..), generateConstraints)
import MLF.Normalize (normalize)
import MLF.Acyclicity (checkAcyclicity)
import MLF.Presolution (computePresolution, PresolutionResult(..))

-- | Explicitly typed types for elaboration (xMLF).
-- Corresponds to Figure 1 in "A Church-Style Intermediate Language for MLF".
--
-- xMLF extends System F with instance-bounded polymorphism (flexible quantification):
--   ∀(α ⩾ τ). σ
--
-- This restricts the variable α to range only over instances of τ.
--
-- Constructors:
--   * TVar: Type variables (α)
--   * TArrow: Function types (τ -> τ)
--   * TBase: Base types (Int, Bool, etc.) - extension of the paper's calculus
--   * TForall: Flexible quantification ∀(α ⩾ τ). σ.
--       - Nothing bound implies ⩾ ⊥ (standard System F unbounded quantification)
--       - Just bound implies explicit instance bound
--   * TBottom: The bottom type ⊥ (minimal type), used as the default bound.
data ElabType
    = TVar String
    | TArrow ElabType ElabType
    | TBase BaseTy
    | TForall String (Maybe ElabType) ElabType  -- ∀(α ⩾ τ?). σ
    | TBottom                                    -- ⊥ (minimal type)
    deriving (Eq, Show)

-- | Polymorphic schemes (multiple quantifiers).
data ElabScheme = Forall [(String, Maybe ElabType)] ElabType
    deriving (Eq, Show)

-- | Instantiation witnesses (φ) for xMLF.
-- These explicitly record how a polymorphic type is instantiated.
--
-- From the FLOPS 2010 paper:
--   φ ::= 1        -- identity
--       | ⟨τ⟩      -- type application (substitute for outermost var)
--       | τ        -- bottom instantiation (substitute ⊥ with τ)
--       | O        -- introduce ∀ (skip outermost quantifier)
--       | φ; φ'    -- sequential composition
data Instantiation
    = InstId                                -- 1 (identity)
    | InstApp ElabType                      -- ⟨τ⟩ (type application)
    | InstBot ElabType                      -- τ (instantiate ⊥)
    | InstIntro                             -- O (introduce/skip ∀)
    | InstElim                              -- N (eliminate ∀)
    | InstAbstr String                      -- !α (abstract bound)
    | InstUnder String Instantiation        -- ∀(α ⩾) φ (under)
    | InstInside Instantiation              -- ∀(⩾ φ) (inside)
    | InstSeq Instantiation Instantiation   -- φ; φ' (composition)
    deriving (Eq, Show)

-- | Explicitly typed terms with type abstractions and instantiations (xMLF).
data ElabTerm
    = EVar String
    | ELit Lit
    | ELam String ElabType ElabTerm
    | EApp ElabTerm ElabTerm
    | ELet String ElabScheme ElabTerm ElabTerm
    | ETyAbs String (Maybe ElabType) ElabTerm  -- Λ(α ⩾ τ?). e (bounded type abstraction)
    | ETyInst ElabTerm Instantiation           -- e φ (instantiation)
    deriving (Eq, Show)

-- | Simple pretty-printing class for elaborated artifacts.
class Pretty a where
    pretty :: a -> String

instance Pretty ElabType where
    pretty =
        let go t = case t of
                TVar v -> v
                TBase (BaseTy b) -> b
                TArrow a b -> p a ++ " -> " ++ pretty b
                TForall v Nothing body -> "∀" ++ v ++ ". " ++ pretty body
                TForall v (Just bound) body ->
                    "∀(" ++ v ++ " ⩾ " ++ pretty bound ++ "). " ++ pretty body
                TBottom -> "⊥"
            p x@TArrow{} = "(" ++ pretty x ++ ")"
            p x@TForall{} = "(" ++ pretty x ++ ")"
            p x = pretty x
        in go

instance Pretty ElabScheme where
    pretty (Forall [] ty) = pretty ty
    pretty (Forall binds ty) = "∀" ++ unwords (map prettyBind binds) ++ ". " ++ pretty ty
      where
        prettyBind (v, Nothing) = v
        prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ pretty bound ++ ")"

instance Pretty Instantiation where
    pretty inst = case inst of
        InstId -> "1"
        InstApp ty -> "⟨" ++ pretty ty ++ "⟩"
        InstBot ty -> pretty ty
        InstIntro -> "O"
        InstElim -> "N"
        InstAbstr v -> "!" ++ v
        InstUnder v i -> "∀(" ++ v ++ " ⩾) " ++ pretty i
        InstInside i -> "∀(⩾ " ++ pretty i ++ ")"
        InstSeq i1 i2 -> pretty i1 ++ "; " ++ pretty i2

instance Pretty ElabTerm where
    pretty term = case term of
        EVar v -> v
        ELit l -> case l of
            LInt i -> show i
            LBool b -> if b then "true" else "false"
            LString s -> show s
        ELam v ty body -> "λ" ++ v ++ ":" ++ pretty ty ++ ". " ++ pretty body
        EApp f a -> par (pretty f) ++ " " ++ parArg a
          where
            parArg x@EApp{} = par (pretty x)
            parArg x@ELam{} = par (pretty x)
            parArg x = pretty x
        ELet v sch rhs body -> "let " ++ v ++ " : " ++ pretty sch ++ " = " ++ pretty rhs ++ " in " ++ pretty body
        ETyAbs v Nothing body -> "Λ" ++ v ++ ". " ++ pretty body
        ETyAbs v (Just bound) body -> "Λ(" ++ v ++ " ⩾ " ++ pretty bound ++ "). " ++ pretty body
        ETyInst e inst -> pretty e ++ " " ++ prettyInst inst
      where
        par s = "(" ++ s ++ ")"
        prettyInst InstId = "1"
        prettyInst i = "[" ++ pretty i ++ "]"

-- | Errors that can arise during elaboration or reification.
data ElabError
    = ResidualTyExp NodeId
    | MissingNode NodeId
    | MissingGNode GNodeId
    | FreeVarOutOfScope NodeId
    | EnvLookup VarName
    | ValidationFailed [String]
    | NameConflict String
    | InstantiationError String
    deriving (Eq, Show)

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: SolveResult -> NodeId -> Either ElabError ElabType
reifyType res nid = snd <$> go IntMap.empty (canonical nid)
  where
    nodes = cNodes (srConstraint res)
    gnodes = cGNodes (srConstraint res)
    uf = srUnionFind res
    canonical = Solve.frWith uf

    lookupNode k = maybe (Left (MissingNode k)) Right (IntMap.lookup (getNodeId k) nodes)

    go cache n = case IntMap.lookup (getNodeId n) cache of
        Just t -> Right (cache, t)
        Nothing -> do
            node <- lookupNode n
            (cache', t) <- case node of
                TyVar{} -> pure (cache, TVar (nameFor n))
                TyBase{ tnBase = b } -> pure (cache, TBase b)
                TyArrow{ tnDom = d, tnCod = c } -> do
                    (cache1, d') <- go cache (canonical d)
                    (cache2, c') <- go cache1 (canonical c)
                    pure (cache2, TArrow d' c')
                TyForall{ tnQuantLevel = qLvl, tnBody = b } -> do
                    (cache1, body) <- go cache (canonical b)
                    used <- varsAtLevelInNode qLvl (canonical b)
                    let binders =
                            case IntMap.lookup (getGNodeId qLvl) gnodes of
                                Nothing -> []
                                Just gnode -> [ v | (v, _) <- gBinds gnode, IntSet.member (getNodeId (canonical v)) used ]
                        bindersSorted = sort binders
                        wrap v inner = TForall (nameFor v) Nothing inner
                        t' = case bindersSorted of
                                [] -> TForall (nameFor n) Nothing body
                                bs -> foldr wrap body bs
                    pure (cache1, t')
                TyExp{ tnBody = b } -> do
                    go cache (canonical b)
            let cache'' = IntMap.insert (getNodeId n) t cache'
            pure (cache'', t)

    nameFor (NodeId i) = "t" ++ show i

    varsAtLevelInNode :: GNodeId -> NodeId -> Either ElabError IntSet.IntSet
    varsAtLevelInNode lvl start = goV IntSet.empty [start]
      where
        goV seen [] = Right seen
        goV seen (x:xs)
            | IntSet.member (getNodeId x) seen = goV seen xs
            | otherwise =
                case IntMap.lookup (getNodeId x) nodes of
                    Nothing -> Right seen
                    Just node ->
                        let seen' = IntSet.insert (getNodeId x) seen
                            kids = case node of
                                TyArrow{ tnDom = d, tnCod = c } -> [canonical d, canonical c]
                                TyForall{ tnBody = b } -> [canonical b]
                                TyExp{ tnBody = b } -> [canonical b]
                                _ -> []
                            addVar = case node of
                                TyVar{ tnVarLevel = l } | l == lvl -> IntSet.insert (getNodeId x) seen'
                                _ -> seen'
                        in goV addVar (kids ++ xs)

-- | Reify a type with proper instance bounds computation (xMLF style).
-- The bound is computed by examining the tnOwnerLevel of the TyForall:
--   - If tnOwnerLevel == currentScope: rigid (no bound needed)
--   - If tnOwnerLevel < currentScope: flexible (bound = the type constrained by outer scope)
--
-- For simplicity in this version, we compute bounds based on whether the forall
-- is at the "expected" level (rigid) or not (flexible).
reifyTypeWithBound :: SolveResult -> GNodeId -> NodeId -> Either ElabError ElabType
reifyTypeWithBound res currentScope nid = snd <$> go IntMap.empty (canonical nid)
  where
    nodes = cNodes (srConstraint res)
    gnodes = cGNodes (srConstraint res)
    uf = srUnionFind res
    canonical = Solve.frWith uf

    lookupNode k = maybe (Left (MissingNode k)) Right (IntMap.lookup (getNodeId k) nodes)

    go cache n = case IntMap.lookup (getNodeId n) cache of
        Just t -> Right (cache, t)
        Nothing -> do
            node <- lookupNode n
            (cache', t) <- case node of
                TyVar{} -> pure (cache, TVar (nameFor n))
                TyBase{ tnBase = b } -> pure (cache, TBase b)
                TyArrow{ tnDom = d, tnCod = c } -> do
                    (cache1, d') <- go cache (canonical d)
                    (cache2, c') <- go cache1 (canonical c)
                    pure (cache2, TArrow d' c')
                TyForall{ tnOwnerLevel = ownerLvl, tnQuantLevel = quantLvl, tnBody = b } -> do
                    -- Compute bound based on whether this forall is "flexible"
                    -- A forall is flexible if it's owned by an outer scope
                    let isFlexible = ownerLvl /= currentScope && getGNodeId ownerLvl < getGNodeId currentScope
                    bound <- if isFlexible
                             then computeBound quantLvl b
                             else pure Nothing
                    (cache1, body) <- go cache (canonical b)
                    pure (cache1, TForall (nameFor n) bound body)
                TyExp{ tnBody = b } -> do
                    go cache (canonical b)
            let cache'' = IntMap.insert (getNodeId n) t cache'
            pure (cache'', t)

    -- Compute the bound type for a flexible forall
    -- The bound represents the constraint on the quantified variables
    computeBound :: GNodeId -> NodeId -> Either ElabError (Maybe ElabType)
    computeBound quantLvl bodyNode = do
        -- Find variables bound at quantLvl in the body
        case IntMap.lookup (getGNodeId quantLvl) gnodes of
            Nothing -> pure Nothing
            Just gnode -> do
                let boundVars = gBinds gnode
                if null boundVars
                    then pure Nothing
                    else do
                        -- The bound is the type that these variables must satisfy
                        -- For now, we just reify the body type as the bound
                        (_, bodyTy) <- go IntMap.empty (canonical bodyNode)
                        pure (Just bodyTy)

    nameFor (NodeId i) = "t" ++ show i

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> GNodeId -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt res gid nid = do
    let gnodes = cGNodes (srConstraint res)
        nodes = cNodes (srConstraint res)
        uf = srUnionFind res
        canonical = Solve.frWith uf
    g <- maybe (Left (MissingGNode gid)) Right (IntMap.lookup (getGNodeId gid) gnodes)
    let target = case IntMap.lookup (getNodeId (canonical nid)) nodes of
            Just TyExp{ tnBody = b } -> canonical b
            _ -> canonical nid

    -- Optimization: If target is a TyForall that quantifies the target level,
    -- then it handles the generalization itself. Do not infer new binders.
    -- This avoids "double quantification" where both the structural node and
    -- the inferred scheme quantify the same variables.
    isStructuralForall <- case IntMap.lookup (getNodeId target) nodes of
        Just TyForall{ tnQuantLevel = q } -> pure (q == gid)
        _ -> pure False

    if isStructuralForall
        then do
            -- Optimization: The target node IS a TyForall that structurally binds the variables at this level.
            -- We want to return a Scheme that quantifies these variables, but the Body of the scheme
            -- should be the body of the TyForall, not the TyForall itself (to avoid double quantification).
            -- We still need to compute the bindings (names) so that 'elaborate' can insert ETyAbs.

            -- We use the standard logic to find free variables and generate names,
            -- but we reify the BODY of the structural Forall.
            let fv = freeVars res target IntSet.empty

            -- Map bound variables to their canonical representatives and bounds
            let boundVars =
                    [ (v, mb)
                    | (v, mb) <- gBinds g
                    , case IntMap.lookup (getNodeId (canonical v)) nodes of
                        Just TyVar{ tnVarLevel = l } -> l == gid
                        _ -> False
                    ]
                canonicalBinders = [ (v, canonical v, mb) | (v, mb) <- boundVars ]

            -- Filter those whose canonical rep is in free variables
            let activeBinders = filter (\(_, canon, _) -> IntSet.member (getNodeId canon) fv) canonicalBinders

            -- Use canonical IDs for substitution
            let grouped =
                    IntMap.fromListWith
                        (\(canon1, mb1) (_, mb2) ->
                            let mb = case mb1 of
                                    Just{} -> mb1
                                    Nothing -> mb2
                            in (canon1, mb)
                        )
                        [ (getNodeId canon, (canon, mb)) | (_, canon, mb) <- activeBinders ]

            let candidates = IntMap.keys grouped
                names = zipWith alphaName [0..] candidates
                subst = IntMap.fromList (zip candidates names)

            -- Check for instance bounds
            bindings <- mapM (\(name, nidInt) -> do
                    let (_, mbBoundNode) = grouped IntMap.! nidInt
                    boundTy <- case mbBoundNode of
                        Nothing -> pure Nothing
                        Just bNode -> Just <$> reifyTypeWithNames res subst bNode
                    pure (name, boundTy)
                ) (zip names candidates)

            -- CRITICAL FIX: Reify the BODY of the structural Forall
            let bodyNode = case IntMap.lookup (getNodeId target) nodes of
                    Just TyForall{ tnBody = b } -> canonical b
                    _ -> target -- Should not happen given isStructuralForall check

            ty <- reifyTypeWithNames res subst bodyNode
            pure (Forall bindings ty, subst)
        else do
            let fv = freeVars res target IntSet.empty

            -- Map bound variables to their canonical representatives and bounds
            let boundVars =
                    [ (v, mb)
                    | (v, mb) <- gBinds g
                    , case IntMap.lookup (getNodeId (canonical v)) nodes of
                        Just TyVar{ tnVarLevel = l } -> l == gid
                        _ -> False
                    ]
                canonicalBinders = [ (v, canonical v, mb) | (v, mb) <- boundVars ]

            -- Filter those whose canonical rep is in free variables
            let activeBinders = filter (\(_, canon, _) -> IntSet.member (getNodeId canon) fv) canonicalBinders

            -- Use canonical IDs for substitution
            let grouped =
                    IntMap.fromListWith
                        (\(canon1, mb1) (_, mb2) ->
                            let mb = case mb1 of
                                    Just{} -> mb1
                                    Nothing -> mb2
                            in (canon1, mb)
                        )
                        [ (getNodeId canon, (canon, mb)) | (_, canon, mb) <- activeBinders ]

            let candidates = IntMap.keys grouped
                names = zipWith alphaName [0..] candidates
                subst = IntMap.fromList (zip candidates names)

            -- Check for instance bounds
            bindings <- mapM (\(name, nidInt) -> do
                    let (_, mbBoundNode) = grouped IntMap.! nidInt
                    boundTy <- case mbBoundNode of
                        Nothing -> pure Nothing
                        Just bNode -> Just <$> reifyTypeWithNames res subst bNode
                    pure (name, boundTy)
                ) (zip names candidates)

            ty <- reifyTypeWithNames res subst target
            pure (Forall bindings ty, subst)

-- | Reify with an explicit name substitution for vars.
reifyTypeWithNames :: SolveResult -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames res subst nid = snd <$> go IntMap.empty (canonical nid)
  where
    nodes = cNodes (srConstraint res)
    gnodes = cGNodes (srConstraint res)
    uf = srUnionFind res
    canonical = Solve.frWith uf
    lookupNode k = maybe (Left (MissingNode k)) Right (IntMap.lookup (getNodeId k) nodes)

    go cache n = do
        -- trace ("Reify: visiting " ++ show n) $ return ()
        case IntMap.lookup (getNodeId n) cache of
            Just t -> Right (cache, t)
            Nothing -> do
                node <- lookupNode n
                (cache', t) <- case node of
                    TyVar{} -> do
                        let name = maybe (nameFor n) id (IntMap.lookup (getNodeId n) subst)
                        -- trace ("ReifyVar: nid=" ++ show (getNodeId n) ++ " subst=" ++ show (IntMap.lookup (getNodeId n) subst) ++ " res=" ++ name) $ return ()
                        pure (cache, TVar name)
                    TyBase{ tnBase = b } -> pure (cache, TBase b)
                    TyArrow{ tnDom = d, tnCod = c } -> do
                        (cache1, d') <- go cache (canonical d)
                        (cache2, c') <- go cache1 (canonical c)
                        pure (cache2, TArrow d' c')
                    TyForall{ tnQuantLevel = qLvl, tnBody = b } -> do
                        -- Reify the quantifier(s) at this level.
                        --
                        -- A single `TyForall` node only stores the quantification *level*.
                        -- That level may contain multiple `TyVar`s (e.g. the scheme’s binder
                        -- plus monomorphic vars introduced while typechecking the RHS).
                        --
                        -- We therefore bind *exactly the vars at qLvl that are actually used
                        -- in the body*, ordered deterministically.
                        quantBinders <- case IntMap.lookup (getGNodeId qLvl) gnodes of
                             Nothing -> pure []
                             Just gnode -> pure (gBinds gnode)

                        used <- varsAtLevelInNode qLvl (canonical b)
                        let usedBinders =
                                [ (v, mb) | (v, mb) <- quantBinders
                                , IntSet.member (getNodeId (canonical v)) used
                                ]
                            -- deterministic order (smallest NodeId first)
                            usedBindersSorted = sortOnFst usedBinders

                        (cache1, body) <- go cache (canonical b)

                        let wrapOne (v, mBound) inner = do
                                let vName = varNameFor v
                                mbBoundTy <- case mBound of
                                    Nothing -> pure Nothing
                                    Just bn -> Just . snd <$> go cache1 (canonical bn)
                                pure (TForall vName mbBoundTy inner)

                        -- If the quantifier is vacuous w.r.t. qLvl, keep a dummy unbounded ∀.
                        -- This preserves structure and avoids losing information.
                        t' <- case usedBindersSorted of
                            [] -> pure (TForall (nameFor n) Nothing body)
                            bs -> foldrM wrapOne body bs

                        pure (cache1, t')
                    TyExp{ tnBody = b } -> do
                        go cache (canonical b)
                let cache'' = IntMap.insert (getNodeId n) t cache'
                pure (cache'', t)

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in maybe (nameFor cv) id (IntMap.lookup (getNodeId cv) subst)

    sortOnFst :: Ord a => [(a, b)] -> [(a, b)]
    sortOnFst = sortOn (\(a, _) -> a)

    foldrM :: (a -> b -> Either ElabError b) -> b -> [a] -> Either ElabError b
    foldrM _ z [] = Right z
    foldrM f z (x:xs) = do
        z' <- foldrM f z xs
        f x z'

    -- Collect TyVar NodeIds at a given level reachable from a node.
    varsAtLevelInNode :: GNodeId -> NodeId -> Either ElabError IntSet.IntSet
    varsAtLevelInNode lvl start = goV IntSet.empty [start]
      where
        goV seen [] = Right seen
        goV seen (nid0:rest)
            | IntSet.member (getNodeId nid0) seen = goV seen rest
            | otherwise =
                case IntMap.lookup (getNodeId nid0) nodes of
                    Nothing -> Right seen
                    Just node ->
                        let seen' = IntSet.insert (getNodeId nid0) seen
                            kids = case node of
                                TyArrow{ tnDom = d, tnCod = c } -> [canonical d, canonical c]
                                TyForall{ tnBody = b } -> [canonical b]
                                TyExp{ tnBody = b } -> [canonical b]
                                _ -> []
                            addVar = case node of
                                TyVar{ tnVarLevel = l } | l == lvl -> IntSet.insert (getNodeId nid0) seen'
                                _ -> seen'
                        in goV addVar (kids ++ rest)

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
--     We will map ExpInstantiate [t] to (N; ⟨t⟩) if it replaces a bounded var,
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
        tys <- mapM (reifyType res) args
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

-- | Turn a scheme into its corresponding type (nested `∀`).
schemeToType :: ElabScheme -> ElabType
schemeToType (Forall binds body) =
    foldr (\(v, b) t -> TForall v b t) body binds

composeInst :: Instantiation -> Instantiation -> Instantiation
composeInst InstId i = i
composeInst i InstId = i
composeInst i1 i2 = InstSeq i1 i2

instMany :: [Instantiation] -> Instantiation
instMany = foldr composeInst InstId

splitForalls :: ElabType -> ([(String, Maybe ElabType)], ElabType)
splitForalls = \case
    TForall v b t -> let (qs, body) = splitForalls t in ((v, b) : qs, body)
    t -> ([], t)

-- | Apply an xMLF instantiation to an xMLF type (xmlf Fig. 3).
--
-- This is a *partial* function: it fails if the instantiation expects a certain
-- type form (e.g. ∀ for `N`) but the type does not match.
applyInstantiation :: ElabType -> Instantiation -> Either ElabError ElabType
applyInstantiation ty inst = snd <$> go 0 ty inst
  where
    -- counter threads fresh names for `O`.
    go :: Int -> ElabType -> Instantiation -> Either ElabError (Int, ElabType)
    go k t i = case i of
        InstId -> Right (k, t)
        InstSeq i1 i2 -> do
            (k1, t1) <- go k t i1
            go k1 t1 i2

        -- Sugar: ⟨τ⟩ ≡ (∀(⩾ τ); N)
        InstApp argTy ->
            go k t (InstSeq (InstInside (InstBot argTy)) InstElim)

        -- Bottom instantiation: ⊥ τ = τ
        InstBot tArg -> case t of
            TBottom -> Right (k, tArg)
            _ -> Left (InstantiationError ("InstBot expects ⊥, got: " ++ pretty t))

        -- Abstract bound: τ (!α) = α (no environment checking here; see xmlf §1.2)
        InstAbstr v -> Right (k, TVar v)

        -- Quantifier intro: τ O = ∀(α ⩾ ⊥). τ   (α fresh)
        InstIntro -> do
            let used = ftvType t
                (fresh, k') = freshName k used
            Right (k', TForall fresh Nothing t)

        -- Quantifier elim: (∀(α ⩾ τ) τ') N = τ'{α ← τ}
        InstElim -> case t of
            TForall v mbBound body -> do
                let bTy = maybe TBottom id mbBound
                Right (k, substType v bTy body)
            _ -> Left (InstantiationError ("InstElim expects ∀, got: " ++ pretty t))

        -- Inside: (∀(α ⩾ τ) τ') (∀(⩾ φ)) = ∀(α ⩾ (τ φ)) τ'
        InstInside phi -> case t of
            TForall v mbBound body -> do
                let b0 = maybe TBottom id mbBound
                (k1, b1) <- go k b0 phi
                let mb' = if b1 == TBottom then Nothing else Just b1
                Right (k1, TForall v mb' body)
            _ -> Left (InstantiationError ("InstInside expects ∀, got: " ++ pretty t))

        -- Under: (∀(α ⩾ τ) τ') (∀(α ⩾) φ) = ∀(α ⩾ τ) (τ' φ)
        InstUnder vParam phi -> case t of
            TForall v mbBound body -> do
                let phi' = renameInstBound vParam v phi
                (k1, body') <- go k body phi'
                Right (k1, TForall v mbBound body')
            _ -> Left (InstantiationError ("InstUnder expects ∀, got: " ++ pretty t))

    -- free type variables (for freshness)
    ftvType :: ElabType -> [String]
    ftvType = nub . goF
      where
        goF ty0 = case ty0 of
            TVar v -> [v]
            TArrow a b -> goF a ++ goF b
            TBase _ -> []
            TBottom -> []
            TForall v mb b ->
                let fvB = goF b
                    fvBound = maybe [] goF mb
                in filter (/= v) fvB ++ fvBound

    freshName :: Int -> [String] -> (String, Int)
    freshName n used =
        let candidate = "u" ++ show n
        in if candidate `elem` used then freshName (n + 1) used else (candidate, n + 1)

    -- Capture-avoiding substitution [x ↦ s]t (only for types).
    substType :: String -> ElabType -> ElabType -> ElabType
    substType x s t0 = case t0 of
        TVar v | v == x -> s
        TVar v -> TVar v
        TArrow a b -> TArrow (substType x s a) (substType x s b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body
            | v == x -> TForall v (fmap (substType x s) mb) body
            | v `elem` ftvType s ->
                let v' = pickFresh v (ftvType s ++ ftvType body ++ maybe [] ftvType mb)
                    body' = substType v (TVar v') body
                in TForall v' (fmap (substType x s) mb) (substType x s body')
            | otherwise ->
                TForall v (fmap (substType x s) mb) (substType x s body)

    pickFresh :: String -> [String] -> String
    pickFresh base used =
        let cands = base : [base ++ show i | i <- [(1::Int)..]]
        in case filter (`notElem` used) cands of
            (x:_) -> x
            [] -> base  -- unreachable (cands is infinite)

    -- Rename bound variable occurrences inside an instantiation body.
    -- This is α-renaming of the instantiation’s binder: occurrences of `old`
    -- are renamed to `new`, except under a nested `∀(old ⩾)` which re-binds it.
    renameInstBound :: String -> String -> Instantiation -> Instantiation
    renameInstBound old new = goR
      where
        goR inst0 = case inst0 of
            InstId -> InstId
            InstApp t -> InstApp t
            InstBot t -> InstBot t
            InstIntro -> InstIntro
            InstElim -> InstElim
            InstAbstr v -> InstAbstr (if v == old then new else v)
            InstInside i -> InstInside (goR i)
            InstSeq a b -> InstSeq (goR a) (goR b)
            InstUnder v i
                | v == old -> InstUnder v i  -- shadowing: stop renaming under this binder
                | otherwise -> InstUnder v (goR i)

-- | Generate the (paper) commutation instantiation that swaps the first two
-- quantifiers of a type ∀(α ⩾ τα). ∀(β ⩾ τβ). τ.
swapFront :: (String, Maybe ElabType) -> (String, Maybe ElabType) -> Instantiation
swapFront (_a, mbTa) (_b, mbTb) =
    -- xmlf §3.4 “Reordering quantifiers”:
    --   O; ∀(⩾ τα); O; ∀(⩾ τβ); ∀(β ⩾) ∀(α ⩾) h!αi; h!βi
    --
    -- We keep binder names symbolic; `applyInstantiation` α-renames under-binders.
    let ta = maybe TBottom id mbTa
        tb = maybe TBottom id mbTb
        hAbs v = InstSeq (InstInside (InstAbstr v)) InstElim
    in instMany
        [ InstIntro
        , InstInside (InstBot ta)
        , InstIntro
        , InstInside (InstBot tb)
        , InstUnder "β" (InstUnder "α" (InstSeq (hAbs "α") (hAbs "β")))
        ]

-- | Swap quantifiers at depth i and i+1 (0-based) by applying `swapFront`
-- under the first i binders.
swapAt :: Int -> ElabType -> Either ElabError Instantiation
swapAt i ty = case (i, ty) of
    (0, TForall _a ta (TForall _b tb _)) ->
        Right (swapFront (_a, ta) (_b, tb))
    (n, TForall v _ body) | n > 0 ->
        InstUnder v <$> swapAt (n - 1) body
    _ ->
        Left (InstantiationError ("swapAt: cannot swap at depth " ++ show i ++ " in type " ++ pretty ty))

-- | Reorder the leading quantifier spine of `src` so its binder order matches `tgt`.
-- Returns the instantiation Σ that performs the reordering.
sigmaReorder :: ElabType -> ElabType -> Either ElabError Instantiation
sigmaReorder src tgt =
    let (srcQs, _) = splitForalls src
        (tgtQs, _) = splitForalls tgt
        srcIds = map fst srcQs
        desired = map fst tgtQs
    in sigmaReorderTo src srcIds desired

-- | Reorder the leading quantifiers of a type to a desired binder *identity* order
-- using adjacent swaps (bubble-style), producing a Σ instantiation.
--
-- Important: applying the commutation instantiations introduces fresh binder names,
-- so we must *not* use the post-swap binder names for bookkeeping. Instead we track
-- the intended binder identities in a separate list (`ids`) and update it as we swap.
sigmaReorderTo :: ElabType -> [String] -> [String] -> Either ElabError Instantiation
sigmaReorderTo ty0 ids0 desired = go InstId ty0 ids0 0
  where
    go :: Instantiation -> ElabType -> [String] -> Int -> Either ElabError Instantiation
    go acc ty ids idx
        | idx >= length desired = Right acc
        | otherwise = do
            if length ids < length desired
                then Left (InstantiationError ("sigmaReorder: type has only " ++ show (length ids) ++ " binders"))
                else if ids !! idx == desired !! idx
                    then go acc ty ids (idx + 1)
                    else do
                        -- find the desired binder in the suffix
                        case elemIndex (desired !! idx) (drop idx ids) of
                            Nothing ->
                                Left (InstantiationError "sigmaReorder: desired binder not found in source")
                            Just off -> do
                                let k = idx + off
                                (acc', ty', ids') <- bubbleLeft acc ty ids k idx
                                go acc' ty' ids' (idx + 1)

    bubbleLeft :: Instantiation -> ElabType -> [String] -> Int -> Int -> Either ElabError (Instantiation, ElabType, [String])
    bubbleLeft acc ty ids k idx
        | k <= idx = Right (acc, ty, ids)
        | otherwise = do
            sw <- swapAt (k - 1) ty
            ty' <- applyInstantiation ty sw
            let ids' = swapAdjacent (k - 1) ids
            bubbleLeft (composeInst acc sw) ty' ids' (k - 1) idx

    swapAdjacent :: Int -> [a] -> [a]
    swapAdjacent i xs =
        let (pre, rest) = splitAt i xs
        in case rest of
            (a:b:rs) -> pre ++ (b:a:rs)
            _ -> xs

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
phiFromEdgeWitness :: SolveResult -> Maybe SchemeInfo -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitness res mSchemeInfo ew =
    let InstanceWitness ops = ewWitness ew
        introPhi = instMany (replicate (ewForallIntros ew) InstIntro)
    in case mSchemeInfo of
        Nothing -> phiFromType introPhi ops
        Just si -> phiWithScheme si introPhi ops
  where
    phiFromType :: Instantiation -> [InstanceOp] -> Either ElabError Instantiation
    phiFromType introPhi ops = do
        ty0 <- reifyType res (ewRoot ew)
        let ids0 = idsFromType ty0
            lookupBinder nid = Just ("t" ++ show (getNodeId nid))
        (_, _, phi) <- go ty0 ids0 InstId ops lookupBinder
        pure (composeInst phi introPhi)

    phiWithScheme :: SchemeInfo -> Instantiation -> [InstanceOp] -> Either ElabError Instantiation
    phiWithScheme si introPhi ops = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
        (_, _, phi) <- go ty0 ids0 InstId ops lookupBinder
        pure (composeInst phi introPhi)

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{·}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    go :: ElabType -> [Maybe NodeId] -> Instantiation -> [InstanceOp] -> (NodeId -> Maybe String)
       -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    go ty ids phi ops lookupBinder = case ops of
        [] -> Right (ty, ids, phi)

        (OpGraft arg bv : OpWeaken bv' : rest)
            | bv == bv' -> do
                (inst, ids1) <- atBinder ids ty bv $ do
                    argTy <- reifyType res arg
                    pure (InstApp argTy)
                ty' <- applyInstantiation ty inst
                go ty' ids1 (composeInst phi inst) rest lookupBinder
            | otherwise ->
                Left (InstantiationError "witness op mismatch: OpGraft/OpWeaken refer to different nodes")

        (OpGraft arg bv : rest) -> do
            (inst, ids1) <- atBinderKeep ids ty bv $ do
                argTy <- reifyType res arg
                pure (InstInside (InstBot argTy))
            ty' <- applyInstantiation ty inst
            go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpWeaken bv : rest) -> do
            (inst, ids1) <- atBinder ids ty bv (pure InstElim)
            ty' <- applyInstantiation ty inst
            go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaise n : rest) -> do
            -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
            -- bounds it by Tξ(n), then aliases/eliminates the old binder.
            --
            -- For now we approximate the “one level higher / choose m” placement by
            -- raising `n` to the *front* of the current binder spine. This keeps the
            -- witness paper-shaped (it is a real O; ∀(⩾ …); … construction) while we
            -- incrementally improve placement using interior provenance.
            i <- binderIndex ids n
            let (qs, _) = splitForalls ty
            when (length qs /= length ids) $
                Left (InstantiationError "OpRaise: binder spine / identity list length mismatch")
            when (i < 0 || i >= length qs) $
                Left (InstantiationError "OpRaise: binder index out of range")
            let mbBound = snd (qs !! i)
                boundTy = maybe TBottom id mbBound
                hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim

            -- Eliminate the *old* binder `n` in the body, aliasing it to the fresh β.
            (instBody, idsNoN) <- atBinder ids ty n (pure hAbsBeta)

            let inst = instMany
                    [ InstIntro
                    , InstInside (InstBot boundTy)
                    , InstUnder "β" instBody
                    ]
            ty' <- applyInstantiation ty inst

            -- `n` remains a logical binder identity; it just moves to the fresh front binder.
            go ty' (Just n : idsNoN) (composeInst phi inst) rest lookupBinder

        (OpMerge n m : rest) -> do
            mName <- binderNameFor ty ids m lookupBinder
            let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
            (inst, ids1) <- atBinder ids ty n (pure hAbs)
            ty' <- applyInstantiation ty inst
            go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaiseMerge n m : rest) ->
            -- Paper Fig. 10 special-cases RaiseMerge(r, m) at the expansion root
            -- as !αm. We approximate this by using !αm when `n` is not a quantified
            -- binder in the current type.
            case elemIndex (Just n) ids of
                Nothing -> do
                    mName <- binderNameFor ty ids m lookupBinder
                    ty' <- applyInstantiation ty (InstAbstr mName)
                    go ty' [] (composeInst phi (InstAbstr mName)) rest lookupBinder
                Just _ -> do
                    mName <- binderNameFor ty ids m lookupBinder
                    let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                    (inst, ids1) <- atBinder ids ty n (pure hAbs)
                    ty' <- applyInstantiation ty inst
                    go ty' ids1 (composeInst phi inst) rest lookupBinder

    idsForStartType :: SchemeInfo -> ElabType -> [Maybe NodeId]
    idsForStartType si ty =
        let nameToId =
                Map.fromList
                    [ (nm, NodeId k)
                    | (k, nm) <- IntMap.toList (siSubst si)
                    ]
            (qs, _) = splitForalls ty
        in [ case Map.lookup nm nameToId of
                Just nid -> Just nid
                Nothing -> parseBinderId nm
           | (nm, _) <- qs
           ]

    idsFromType :: ElabType -> [Maybe NodeId]
    idsFromType ty =
        let (qs, _) = splitForalls ty
        in map (parseBinderId . fst) qs

    parseBinderId :: String -> Maybe NodeId
    parseBinderId ('t':rest) = NodeId <$> readMaybe rest
    parseBinderId _ = Nothing

    binderNameFor :: ElabType -> [Maybe NodeId] -> NodeId -> (NodeId -> Maybe String) -> Either ElabError String
    binderNameFor ty ids nid lookupBinder =
        case elemIndex (Just nid) ids of
            Just i -> do
                let (qs, _) = splitForalls ty
                    names = map fst qs
                if length names /= length ids
                    then Left (InstantiationError "binderNameFor: binder spine / identity list length mismatch")
                    else if i >= length names
                        then Left (InstantiationError "binderNameFor: index out of range")
                        else Right (names !! i)
            Nothing ->
                case lookupBinder nid of
                    Just nm -> Right nm
                    Nothing -> Right ("t" ++ show (getNodeId nid))

    atBinder :: [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
             -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinder ids ty nid mkInner = do
        i <- binderIndex ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        ids' <- deleteAt i ids
        pure (underContext prefix inner, ids')

    atBinderKeep :: [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
                 -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinderKeep ids ty nid mkInner = do
        i <- binderIndex ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        pure (underContext prefix inner, ids)

    binderIndex :: [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex ids nid =
        case elemIndex (Just nid) ids of
            Just i -> Right i
            Nothing -> Left (InstantiationError ("binder " ++ show nid ++ " not found in identity list"))

    prefixBinderNames :: ElabType -> [Maybe NodeId] -> Int -> Either ElabError [String]
    prefixBinderNames ty ids i = do
        let (qs, _) = splitForalls ty
            names = map fst qs
        if length names /= length ids
            then Left (InstantiationError "prefixBinderNames: binder spine / identity list length mismatch")
            else if i < 0 || i > length names
                then Left (InstantiationError "prefixBinderNames: index out of range")
                else Right (take i names)

    underContext :: [String] -> Instantiation -> Instantiation
    underContext prefix inner = foldr InstUnder inner prefix

    deleteAt :: Int -> [a] -> Either ElabError [a]
    deleteAt i xs
        | i < 0 = Left (InstantiationError "deleteAt: negative index")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in case rest of
                [] -> Left (InstantiationError "deleteAt: index out of range")
                (_:rs) -> Right (pre ++ rs)

-- | Collect free variables by NodeId, skipping vars under TyForall.
freeVars :: SolveResult -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars res nid visited
    | IntSet.member key visited = IntSet.empty
    | otherwise =
        let visited' = IntSet.insert key visited
        in case IntMap.lookup key nodes of
            Nothing -> IntSet.empty
            Just TyVar{} -> IntSet.singleton key
            Just TyBase{} -> IntSet.empty
            Just TyArrow{ tnDom = d, tnCod = c } ->
                freeVars res (canonical d) visited' `IntSet.union`
                freeVars res (canonical c) visited'
            Just TyForall{ tnBody = b } -> freeVars res (canonical b) visited'
            Just TyExp{ tnBody = b } -> freeVars res (canonical b) visited'
  where
    nodes = cNodes (srConstraint res)
    uf = srUnionFind res
    canonical = Solve.frWith uf
    key = getNodeId (canonical nid)
alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)

-- | Environment for elaboration (let-generalized schemes only).
data SchemeInfo = SchemeInfo
    { siScheme :: ElabScheme
    , siSubst :: IntMap.IntMap String
    } deriving (Eq, Show)

type Env = Map.Map VarName SchemeInfo

elaborate :: SolveResult -> IntMap.IntMap EdgeWitness -> AnnExpr -> Either ElabError ElabTerm
elaborate res edgeWitnesses ann = go Map.empty ann
  where
    go :: Env -> AnnExpr -> Either ElabError ElabTerm
    go env ae = case ae of
        AVar v _ -> maybe (Left (EnvLookup v)) (const (Right (EVar v))) (Map.lookup v env)
        ALit lit _ -> Right (ELit lit)
        ALam v n _ body _ -> do
            ty <- reifyType res n
            -- Add lambda parameter to env with a monomorphic "scheme"
            let paramScheme = SchemeInfo { siScheme = Forall [] ty, siSubst = IntMap.empty }
                env' = Map.insert v paramScheme env
            body' <- go env' body
            pure (ELam v ty body')
        AApp f a eid _ -> do
            f' <- go env f
            a' <- go env a
            inst <- reifyInst env f eid
            let fApp = case inst of
                    InstId -> f'
                    _      -> ETyInst f' inst
            pure (EApp fApp a')
        ALet v schemeNode _ childLevel rhs body _ -> do
            rhs' <- go env rhs
            (sch, subst) <- generalizeAt res childLevel schemeNode
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
                let mSchemeInfo = case funAnn of
                        AVar v _ -> Map.lookup v env
                        _ -> Nothing
                phiFromEdgeWitness res mSchemeInfo ew

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

-- | Run the full pipeline (Phases 1–5) then elaborate.
runPipelineElab :: Expr -> Either String (ElabTerm, ElabType)
runPipelineElab expr = do
    ConstraintResult { crConstraint = c0, crRoot = root, crAnnotated = ann } <- firstShow (generateConstraints expr)
    let c1 = normalize c0
    acyc <- firstShow (checkAcyclicity c1)
    pres <- firstShow (computePresolution acyc c1)
    solved <- firstShow (solveUnify (prConstraint pres))
    case validateSolvedGraphStrict solved of
        [] -> do
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            term <- firstShow (elaborate solved (prEdgeWitnesses pres) ann')

            -- Also apply redirects to the root node, as it might have been a TyExp
            let root' = chaseRedirects (prRedirects pres) root

            -- Generalize at root level (GNode 0) to produce a scheme for the result
            (sch, _) <- firstShow (generalizeAt solved (GNodeId 0) root')
            let ty = case sch of
                    Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds

            pure (term, ty)
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)
  where
    firstShow :: Show e => Either e a -> Either String a
    firstShow = either (Left . show) Right

applyRedirectsToAnn :: IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
applyRedirectsToAnn redirects ann = case ann of
    ALit l nid -> ALit l (redir nid)
    AVar v nid -> AVar v (redir nid)
    ALam v pNode x bodyAnn nid -> ALam v (redir pNode) x (applyRedirectsToAnn redirects bodyAnn) (redir nid)
    AApp fAnn argAnn eid nid -> AApp (applyRedirectsToAnn redirects fAnn) (applyRedirectsToAnn redirects argAnn) eid (redir nid)
    ALet v schNode ev childLevel rhsAnn bodyAnn nid ->
        ALet v (redir schNode) ev childLevel (applyRedirectsToAnn redirects rhsAnn) (applyRedirectsToAnn redirects bodyAnn) (redir nid)
    AAnn exprAnn nid eid -> AAnn (applyRedirectsToAnn redirects exprAnn) (redir nid) eid
  where
    redir = chaseRedirects redirects

-- | Chase redirects through the map until stable or missing
chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects redirects nid = case IntMap.lookup (getNodeId nid) redirects of
    Just n' -> if n' == nid then nid else chaseRedirects redirects n'
    Nothing -> nid
