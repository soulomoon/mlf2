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
    phiFromEdgeWitnessWithTrace,
    runPipelineElab,
    applyRedirectsToAnn,
    -- * Exported for testing/debugging
    chaseRedirects,
    SchemeInfo(..),
    -- * Context representation for non-spine Raise (paper Fig. 10)
    ContextStep(..),
    contextToNodeBound,
    selectMinPrecInsertionIndex
) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (elemIndex, nub, sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import qualified MLF.Order as Order
import MLF.Syntax (VarName, Lit(..), Expr)
import MLF.Types
import MLF.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Solve as Solve (frWith)
import MLF.ConstraintGen (AnnExpr(..), ConstraintResult(..), generateConstraints)
import MLF.Normalize (normalize)
import MLF.Acyclicity (checkAcyclicity)
import MLF.Presolution (computePresolution, PresolutionResult(..), EdgeTrace(..))
import MLF.Binding (checkBindingTree, bindingPathToRoot, orderedBinders)
import qualified MLF.VarStore as VarStore

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
    | BindingTreeError BindingError
    | NameConflict String
    | InstantiationError String
    deriving (Eq, Show)

-- | Context steps for reaching a node in the type structure.
--
-- Paper reference: @papers/xmlf.txt@ Figure 10 uses instantiation contexts
-- C{·} to reach interior nodes. A context is a sequence of steps:
--   - StepUnder: go under a quantifier (∀(α ⩾) ·)
--   - StepInside: go inside a bound (∀(⩾ ·))
data ContextStep
    = StepUnder String      -- ^ Go under quantifier with given binder name
    | StepInside            -- ^ Go inside the bound of a quantifier
    deriving (Eq, Show)

-- | Apply a paper-style instantiation context to an instantiation.
--
-- This encodes Figure 10 contexts:
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
applyContext :: [ContextStep] -> Instantiation -> Instantiation
applyContext steps inner = foldr step inner steps
  where
    step cs inst = case cs of
        StepUnder v -> InstUnder v inst
        StepInside -> InstInside inst

-- | Select the insertion index for the paper’s @m = min≺{…}@ choice (Figure 10).
--
-- Given a binder spine @ids@ that is already ordered by the edge-local ≺ ordering,
-- choose the first binder position whose ≺-key is strictly greater than @n@’s,
-- while respecting a minimal insertion index @minIdx@ (from dependency cutoff).
--
-- Returns an index in @[0 .. length ids]@; inserting at @length ids@ appends.
selectMinPrecInsertionIndex
    :: Int
    -> IntMap.IntMap Order.OrderKey
    -> (NodeId -> NodeId)
    -> NodeId
    -> [Maybe NodeId]
    -> Int
selectMinPrecInsertionIndex minIdx orderKeys canonical n ids =
    case IntMap.lookup (getNodeId (canonical n)) orderKeys of
        Nothing -> minIdx'
        Just nk ->
            let keyAt :: Int -> Maybe Order.OrderKey
                keyAt i = do
                    mbNid <- atMaybe i ids
                    nid <- mbNid
                    IntMap.lookup (getNodeId (canonical nid)) orderKeys

                pick =
                    [ i
                    | i <- [minIdx' .. length ids - 1]
                    , Just k <- [keyAt i]
                    , Order.compareOrderKey k nk == GT
                    ]
            in case pick of
                (i : _) -> i
                [] -> length ids
  where
    minIdx' = max 0 (min minIdx (length ids))

    atMaybe :: Int -> [a] -> Maybe a
    atMaybe i xs
        | i < 0 = Nothing
        | otherwise = case drop i xs of
            (x : _) -> Just x
            [] -> Nothing

-- | Compute an instantiation-context path from a root node to a target node.
--
-- Paper reference: @papers/xmlf.txt@ Figure 10 defines instantiation contexts:
--
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
--
-- i.e. contexts navigate *only* through quantifiers (under binders) and their
-- bounds (inside-bounds). This helper computes a conservative context path
-- using the binding tree: if @target@ is transitively bound to @root@, we return
-- the sequence of “under binder” steps for the strict ancestors of @target@ on
-- the binding path.
--
-- Returns 'Nothing' when @target@ is not transitively bound to @root@.
contextToNodeBound :: SolveResult -> NodeId -> NodeId -> Either ElabError (Maybe [ContextStep])
contextToNodeBound res root target = do
    let c = srConstraint res
        uf = srUnionFind res
        canonical = Solve.frWith uf
        rootC = canonical root
        targetC = canonical target

    if rootC == targetC
        then pure (Just [])
        else do
            let keys = Order.orderKeysFromRoot res rootC
            contextToNodeBoundWithOrderKeys canonical keys c rootC targetC

contextToNodeBoundWithOrderKeys
    :: (NodeId -> NodeId)
    -> IntMap.IntMap Order.OrderKey
    -> Constraint
    -> NodeId
    -> NodeId
    -> Either ElabError (Maybe [ContextStep])
contextToNodeBoundWithOrderKeys canonical keys c root target = do
    -- bindingPathToRoot returns target ↦ … ↦ binding-root
    path <- firstBinding (bindingPathToRoot c target)
    let rootId = getNodeId root
        ids = map getNodeId path
    case elemIndex rootId ids of
        Nothing -> pure Nothing
        Just off -> do
            -- Segment is target ↦ … ↦ root; reverse to root ↦ … ↦ target.
            let seg = take (off + 1) path
                chain = reverse seg

            -- Precompute flex-children map (on canonical ids).
            let addChild m (childId, (parent0, flag)) =
                    case flag of
                        BindRigid -> m
                        BindFlex ->
                            let child = canonical (NodeId childId)
                                parent = canonical parent0
                            in if child == parent
                                then m
                                else IntMap.insertWith (++) (getNodeId parent) [child] m

                rawChildren = foldl addChild IntMap.empty (IntMap.toList (cBindParents c))

                childrenOf p =
                    let xs0 = IntMap.findWithDefault [] (getNodeId p) rawChildren
                        -- Deduplicate while preserving ordering.
                        xs =
                            nubSortBy
                                (Order.compareNodesByOrderKey keys)
                                xs0
                    in xs

                nubSortBy :: Eq a => (a -> a -> Ordering) -> [a] -> [a]
                nubSortBy cmp = nub . sortBy cmp

                nameFor nid = "t" ++ show (getNodeId nid)

                go :: [ContextStep] -> [NodeId] -> Either ElabError [ContextStep]
                go acc ns =
                    case ns of
                        (_only : []) -> Right acc
                        (parent : child : rest) -> do
                            let siblings = childrenOf parent
                            idx <- case elemIndex child siblings of
                                Just i -> Right i
                                Nothing ->
                                    Left $
                                        InstantiationError $
                                            "contextToNodeBound: expected child "
                                                ++ show child
                                                ++ " to be flexibly bound to "
                                                ++ show parent
                            let before = take idx siblings
                                underSteps = map (StepUnder . nameFor) before
                                acc' = acc ++ underSteps
                            if null rest
                                then Right acc'
                                else go (acc' ++ [StepInside]) (child : rest)
                        [] -> Right acc

            steps <- go [] chain
            pure (Just steps)
  where
    firstBinding :: Either BindingError a -> Either ElabError a
    firstBinding = either (Left . BindingTreeError) Right

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: SolveResult -> NodeId -> Either ElabError ElabType
reifyType res nid = snd <$> go IntMap.empty (canonical nid)
  where
    constraint = srConstraint res
    nodes = cNodes constraint
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
                    let used = varsAtLevelInNode canonical nodes qLvl (canonical b)
                        binders =
                            [ v
                            | v <- map NodeId (IntSet.toList used)
                            , not (VarStore.isEliminatedVar constraint v || VarStore.isEliminatedVar constraint (canonical v))
                            ]
                        keys = Order.orderKeysFromRoot res (canonical b)
                        bindersSorted = sortBy (\v1 v2 -> Order.compareNodesByOrderKey keys (canonical v1) (canonical v2)) binders
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
        let boundVars = varsAtLevelInNode canonical nodes quantLvl (canonical bodyNode)
        if IntSet.null boundVars
            then pure Nothing
            else do
                -- The bound is the type that these variables must satisfy.
                -- For now, we just reify the body type as the bound.
                (_, bodyTy) <- go IntMap.empty (canonical bodyNode)
                pure (Just bodyTy)

    nameFor (NodeId i) = "t" ++ show i

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> NodeId -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt res scopeRoot nid = do
    let constraint = srConstraint res
        nodes = cNodes constraint
        uf = srUnionFind res
        canonical = Solve.frWith uf
        scopeRootC = canonical scopeRoot
    let target0 = case IntMap.lookup (getNodeId (canonical nid)) nodes of
            Just TyExp{ tnBody = b } -> canonical b
            _ -> canonical nid
        orderRoot = case IntMap.lookup (getNodeId target0) nodes of
            Just TyForall{ tnBody = b } -> canonical b
            _ -> target0

    binders0 <- case orderedBinders canonical constraint scopeRootC of
        Left err -> Left (BindingTreeError err)
        Right bs -> Right bs
    let binders =
            [ v
            | v <- binders0
            , not (VarStore.isEliminatedVar constraint v)
            ]
        grouped =
            IntMap.fromList
                [ (getNodeId v, (v, fmap canonical (VarStore.lookupVarBound constraint v)))
                | v <- binders
                ]

    ordered <- orderBinderCandidates orderRoot grouped
    let names = zipWith alphaName [0..] ordered
        subst = IntMap.fromList (zip ordered names)

    bindings <- mapM (\(name, nidInt) -> do
            let (_, mbBoundNode) = grouped IntMap.! nidInt
            boundTy <- case mbBoundNode of
                Nothing -> pure Nothing
                Just bNode -> Just <$> reifyTypeWithNames res subst bNode
            pure (name, boundTy)
        ) (zip names ordered)

    ty <- reifyTypeWithNames res subst orderRoot
    pure (Forall bindings ty, subst)
  where
    orderBinderCandidates :: NodeId -> IntMap.IntMap (NodeId, Maybe NodeId) -> Either ElabError [Int]
    orderBinderCandidates root grouped = do
        let keys = Order.orderKeysFromRoot res root
            candidates = IntMap.keys grouped
            candidateSet = IntSet.fromList candidates
            depsFor k =
                case snd (grouped IntMap.! k) of
                    Nothing -> []
                    Just bnd ->
                        [ d
                        | d <- IntSet.toList (freeVars res bnd IntSet.empty)
                        , IntSet.member d candidateSet
                        , d /= k
                        ]
            edges = [ (d, k) | k <- candidates, d <- depsFor k ]
            adj = IntMap.fromListWith (++) [ (a, [b]) | (a, b) <- edges ]
            indeg0 =
                IntMap.fromListWith (+)
                    ( [ (k, 0) | k <- candidates ]
                        ++ [ (b, 1) | (_a, b) <- edges ]
                    )
            cmpReady a b =
                case Order.compareNodesByOrderKey keys (NodeId a) (NodeId b) of
                    EQ -> compare a b
                    other -> other
            startQueue =
                sortBy cmpReady
                    [ k
                    | k <- candidates
                    , IntMap.findWithDefault 0 k indeg0 == 0
                    ]

            step :: [Int] -> IntMap.IntMap Int -> [Int] -> Either ElabError [Int]
            step acc indeg queue =
                case queue of
                    [] ->
                        if length acc == length candidates
                            then Right acc
                            else Left (InstantiationError "generalizeAt: cycle in binder bound dependencies")
                    (k:rest) ->
                        let outs = IntMap.findWithDefault [] k adj
                            (indeg', newlyZero) =
                                foldr
                                    (\j (m, zs) ->
                                        case IntMap.lookup j m of
                                            Nothing -> (m, zs)
                                            Just c ->
                                                let c' = c - 1
                                                    m' = IntMap.insert j c' m
                                                in if c' == 0 then (m', j : zs) else (m', zs)
                                    )
                                    (indeg, [])
                                    outs
                            queue' = sortBy cmpReady (rest ++ newlyZero)
                        in step (acc ++ [k]) indeg' queue'

        if length candidates < 2
            then Right candidates
            else step [] indeg0 startQueue

-- | Reify with an explicit name substitution for vars.
reifyTypeWithNames :: SolveResult -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames res subst nid = snd <$> go IntMap.empty (canonical nid)
  where
    nodes = cNodes (srConstraint res)
    constraint = srConstraint res
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
                        let usedBinders =
                                [ (v, fmap canonical (VarStore.lookupVarBound constraint v))
                                | v <- map NodeId (IntSet.toList (varsAtLevelInNode canonical nodes qLvl (canonical b)))
                                , not (VarStore.isEliminatedVar constraint v)
                                ]
                        usedBindersSorted <- orderUsedBinders (canonical b) usedBinders

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

    foldrM :: (a -> b -> Either ElabError b) -> b -> [a] -> Either ElabError b
    foldrM _ z [] = Right z
    foldrM f z (x:xs) = do
        z' <- foldrM f z xs
        f x z'

    orderUsedBinders :: NodeId -> [(NodeId, Maybe NodeId)] -> Either ElabError [(NodeId, Maybe NodeId)]
    orderUsedBinders bodyRoot bs0 = do
        let keys = Order.orderKeysFromRoot res bodyRoot
            canonBinder (v, mb) = (canonical v, fmap canonical mb)
            bs = map canonBinder bs0
            binderKeys = [ getNodeId v | (v, _) <- bs ]
            binderSet = IntSet.fromList binderKeys
            pairsByKey = IntMap.fromList [ (getNodeId v, (v, mb)) | (v, mb) <- bs ]

            depsFor :: (NodeId, Maybe NodeId) -> [Int]
            depsFor (v, mb) =
                case mb of
                    Nothing -> []
                    Just bnd ->
                        [ d
                        | d <- IntSet.toList (freeVars res bnd IntSet.empty)
                        , IntSet.member d binderSet
                        , d /= getNodeId v
                        ]

            edges = [ (d, getNodeId v) | (v, mb) <- bs, d <- depsFor (v, mb) ]
            adj = IntMap.fromListWith (++) [ (a, [b]) | (a, b) <- edges ]
            indeg0 =
                IntMap.fromListWith (+)
                    ( [ (k, 0) | k <- binderKeys ]
                        ++ [ (b, 1) | (_a, b) <- edges ]
                    )

            cmpReady a b =
                case Order.compareNodesByOrderKey keys (NodeId a) (NodeId b) of
                    EQ -> compare a b
                    other -> other

            startQueue =
                sortBy cmpReady
                    [ k
                    | k <- binderKeys
                    , IntMap.findWithDefault 0 k indeg0 == 0
                    ]

            step :: [Int] -> IntMap.IntMap Int -> [Int] -> Either ElabError [Int]
            step acc indeg queue =
                case queue of
                    [] ->
                        if length acc == length binderKeys
                            then Right acc
                            else Left (InstantiationError "reifyTypeWithBound: cycle in binder bound dependencies")
                    (k:rest) ->
                        let outs = IntMap.findWithDefault [] k adj
                            (indeg', newlyZero) =
                                foldr
                                    (\j (m, zs) ->
                                        case IntMap.lookup j m of
                                            Nothing -> (m, zs)
                                            Just c ->
                                                let c' = c - 1
                                                    m' = IntMap.insert j c' m
                                                in if c' == 0 then (m', j : zs) else (m', zs)
                                    )
                                    (indeg, [])
                                    outs
                            queue' = sortBy cmpReady (rest ++ newlyZero)
                        in step (acc ++ [k]) indeg' queue'

        orderedKeys <- step [] indeg0 startQueue
        pure [ p | k <- orderedKeys, Just p <- [IntMap.lookup k pairsByKey] ]

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
    phiFromEdgeWitnessWithTrace res mSchemeInfo Nothing ew

phiFromEdgeWitnessWithTrace :: SolveResult -> Maybe SchemeInfo -> Maybe EdgeTrace -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace res mSchemeInfo mTrace ew = do
    requireValidBindingTree
    let InstanceWitness ops = ewWitness ew
        introPhi = instMany (replicate (ewForallIntros ew) InstIntro)
    case mSchemeInfo of
        Nothing -> phiFromType introPhi ops
        Just si -> phiWithScheme si introPhi ops
  where
    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        case checkBindingTree (srConstraint res) of
            Left err -> Left (BindingTreeError err)
            Right () -> Right ()

    canonicalNode :: NodeId -> NodeId
    canonicalNode = Solve.frWith (srUnionFind res)

    orderRoot :: NodeId
    orderRoot =
        case mTrace of
            Just tr -> etRoot tr
            Nothing -> ewRoot ew

    orderKeys :: IntMap.IntMap Order.OrderKey
    orderKeys =
        case mTrace of
            Just tr | not (IntSet.null (etInterior tr)) ->
                Order.orderKeysFromRootRestricted res orderRoot (etInterior tr)
            _ ->
                Order.orderKeysFromRoot res orderRoot

    phiFromType :: Instantiation -> [InstanceOp] -> Either ElabError Instantiation
    phiFromType introPhi ops = do
        ty0 <- reifyType res (ewRoot ew)
        let ids0 = idsFromType ty0
            lookupBinder nid = Just ("t" ++ show (getNodeId nid))
        (sigma, ty1, ids1) <-
            if needsPrec ops
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- go ty1 ids1 InstId ops lookupBinder
        pure (instMany [sigma, phiOps, introPhi])

    phiWithScheme :: SchemeInfo -> Instantiation -> [InstanceOp] -> Either ElabError Instantiation
    phiWithScheme si introPhi ops = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
        (sigma, ty1, ids1) <-
            if needsPrec ops
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- go ty1 ids1 InstId ops lookupBinder
        pure (instMany [sigma, phiOps, introPhi])

    needsPrec :: [InstanceOp] -> Bool
    needsPrec = any $ \case
        OpRaise{} -> True
        _ -> False

    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
        let (qs, _) = splitForalls ty
        if length qs /= length ids
            then Left (InstantiationError "reorderBindersByPrec: binder spine / identity list length mismatch")
            else if length qs < 2
                then Right (InstId, ty, ids)
                else do
                    let knownKeyCount =
                            length
                                [ ()
                                | Just nid <- ids
                                , IntMap.member (getNodeId (canonicalNode nid)) orderKeys
                                ]
                    if knownKeyCount < 2
                        then Right (InstId, ty, ids)
                        else do
                            desired <- desiredBinderOrder ty ids
                            reorderTo ty ids desired

    desiredBinderOrder :: ElabType -> [Maybe NodeId] -> Either ElabError [Maybe NodeId]
    desiredBinderOrder ty ids = do
        let (qs, _) = splitForalls ty
            names = map fst qs
            bounds = map snd qs
            n = length qs
            nameIndex nm = elemIndex nm names

            -- Bound dependencies: if a occurs free in b's bound, then a must appear before b.
            depsFor :: Int -> [Int]
            depsFor i =
                case bounds !! i of
                    Nothing -> []
                    Just bnd ->
                        [ j
                        | v <- freeTypeVars bnd
                        , v /= names !! i
                        , Just j <- [nameIndex v]
                        ]

            edges :: [(Int, Int)]
            edges =
                [ (d, i)
                | i <- [0 .. n - 1]
                , d <- depsFor i
                ]

            adj :: IntMap.IntMap [Int]
            adj = IntMap.fromListWith (++) [ (a, [b]) | (a, b) <- edges ]

            indeg0 :: IntMap.IntMap Int
            indeg0 =
                IntMap.fromListWith (+)
                    ( [ (i, 0) | i <- [0 .. n - 1] ]
                        ++ [ (b, 1) | (_a, b) <- edges ]
                    )

            cmpIdx :: Int -> Int -> Ordering
            cmpIdx i j =
                case (ids !! i, ids !! j) of
                    (Just a, Just b) ->
                        let ca = canonicalNode a
                            cb = canonicalNode b
                        in Order.compareNodesByOrderKey orderKeys ca cb
                    (Just _, Nothing) -> LT
                    (Nothing, Just _) -> GT
                    (Nothing, Nothing) -> compare i j

            pickQueue :: [Int] -> [Int]
            pickQueue = sortBy cmpIdx

            step :: [Int] -> IntMap.IntMap Int -> [Int] -> Either ElabError [Int]
            step acc indeg queue =
                case queue of
                    [] ->
                        if length acc == n
                            then Right acc
                            else Left (InstantiationError "reorderBindersByPrec: cycle in bound dependencies")
                    (i : rest) ->
                        let outs = IntMap.findWithDefault [] i adj
                            (indeg', newlyZero) =
                                foldr
                                    (\j (m, zs) ->
                                        case IntMap.lookup j m of
                                            Nothing -> (m, zs)
                                            Just k ->
                                                let k' = k - 1
                                                    m' = IntMap.insert j k' m
                                                in if k' == 0 then (m', j : zs) else (m', zs)
                                    )
                                    (indeg, [])
                                    outs
                            queue' = pickQueue (rest ++ newlyZero)
                        in step (acc ++ [i]) indeg' queue'

            startQueue =
                pickQueue
                    [ i
                    | i <- [0 .. n - 1]
                    , IntMap.findWithDefault 0 i indeg0 == 0
                    ]
        idxs <- step [] indeg0 startQueue
        pure [ ids !! i | i <- idxs ]

    reorderTo :: ElabType -> [Maybe NodeId] -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderTo ty0 ids0 desired0 = goReorder InstId ty0 ids0 0
      where
        goReorder :: Instantiation -> ElabType -> [Maybe NodeId] -> Int -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
        goReorder acc ty ids idx
            | idx >= length desired0 = Right (acc, ty, ids)
            | otherwise =
                if ids !! idx == desired0 !! idx
                    then goReorder acc ty ids (idx + 1)
                    else
                        case elemIndex (desired0 !! idx) (drop idx ids) of
                            Nothing ->
                                Left (InstantiationError "reorderBindersByPrec: desired binder not found in source")
                            Just off -> do
                                let k = idx + off
                                (acc', ty', ids') <- bubbleLeftReorder acc ty ids k idx
                                goReorder acc' ty' ids' (idx + 1)

        bubbleLeftReorder :: Instantiation -> ElabType -> [Maybe NodeId] -> Int -> Int -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
        bubbleLeftReorder acc ty ids k idx
            | k <= idx = Right (acc, ty, ids)
            | otherwise = do
                sw <- swapAt (k - 1) ty
                ty' <- applyInstantiation ty sw
                let ids' = swapAdjacent (k - 1) ids
                bubbleLeftReorder (composeInst acc sw) ty' ids' (k - 1) idx

        swapAdjacent :: Int -> [a] -> [a]
        swapAdjacent i xs =
            let (pre, rest) = splitAt i xs
            in case rest of
                (a : b : rs) -> pre ++ (b : a : rs)
                _ -> xs

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{·}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    graftArgFor :: NodeId -> NodeId -> NodeId
    graftArgFor arg bv =
        case mTrace of
            Nothing -> arg
            Just tr ->
                case IntMap.lookup (getNodeId bv) (etCopyMap tr) of
                    Nothing -> arg
                    Just meta -> meta

    go :: ElabType -> [Maybe NodeId] -> Instantiation -> [InstanceOp] -> (NodeId -> Maybe String)
       -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    go ty ids phi ops lookupBinder = case ops of
        [] -> Right (ty, ids, phi)

        (OpGraft arg bv : OpWeaken bv' : rest)
            | bv == bv' -> do
                (inst, ids1) <- atBinder ids ty bv $ do
                    argTy <- reifyType res (graftArgFor arg bv)
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
            -- For spine binders: use the existing logic
            -- For non-spine nodes: use binding edges + ≺ ordering to compute context
            case elemIndex (Just n) ids of
                Just i -> do
                    -- Spine binder case: existing logic
                    let (qs, _) = splitForalls ty
                    when (length qs /= length ids) $
                        Left (InstantiationError "OpRaise: binder spine / identity list length mismatch")
                    when (i < 0 || i >= length qs) $
                        Left (InstantiationError "OpRaise: binder index out of range")

                    let names = map fst qs
                        mbBound = snd (qs !! i)
                        boundTy = maybe TBottom id mbBound
                        boundName = names !! i

                        deps = filter (/= boundName) (freeTypeVars boundTy)
                        depIdxs = mapMaybe (`elemIndex` names) deps
                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                        insertIndex = cutoff + 1

                    when (insertIndex > i) $
                        Left (InstantiationError "OpRaise: computed insertion point is after binder")

                    let prefixBefore = take insertIndex names
                        between = take (i - insertIndex) (drop insertIndex names)
                        hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                        aliasOld = underContext between hAbsBeta

                        local = instMany
                            [ InstIntro
                            , InstInside (InstBot boundTy)
                            , InstUnder "β" aliasOld
                            ]

                        inst = underContext prefixBefore local
                    ty' <- applyInstantiation ty inst

                    idsNoN <- deleteAt i ids
                    ids1 <- insertAt insertIndex (Just n) idsNoN
                    go ty' ids1 (composeInst phi inst) rest lookupBinder

                Nothing -> do
                    -- Non-spine node case: select an insertion point `m = min≺{...}` (Fig. 10)
                    -- using the edge-local ≺ ordering, then insert a fresh quantifier bounded
                    -- by `Tξ(n)` at that point, and then alias/eliminate the original
                    -- (nested) binder for `n` inside the chosen `m`'s bound.
                    --
                    -- Paper Fig. 10:
                    --   Φξ(Raise(n)) = C^r_m { O; ∀(⩾ Tξ(n)); ∀(βn ⩾) C^m_n {h!βn i} }
                    -- where `m = min≺{…}`.

                    nodeTy <-
                        case mSchemeInfo of
                            Just si -> reifyTypeWithNames res (siSubst si) n
                            Nothing -> reifyType res n

                    let (qs, _) = splitForalls ty
                        names = map fst qs

                    when (length qs /= length ids) $
                        Left (InstantiationError "OpRaise (non-spine): binder spine / identity list length mismatch")

                    -- Compute dependency cutoff: the new binder must be inserted after any
                    -- binder that appears free in `Tξ(n)`.
                    let deps = freeTypeVars nodeTy
                        depIdxs = mapMaybe (`elemIndex` names) deps
                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                        minIdx = min (cutoff + 1) (length ids)

                    -- Choose `m = min≺{...}` among the remaining binders:
                    -- insert just before the first binder whose ≺-key is greater than `n`'s.
                    let insertIdx = selectMinPrecInsertionIndex minIdx orderKeys canonicalNode n ids

                        prefixBefore = take insertIdx names
                        mbM =
                            case drop insertIdx ids of
                                (Just m : _) -> Just m
                                _ -> Nothing

                    mNode <- case mbM of
                        Just mNode -> pure mNode
                        Nothing ->
                            Left (InstantiationError "OpRaise (non-spine): could not identify insertion binder m")

                    let mC = canonicalNode mNode
                        nC = canonicalNode n

                    ctxMn0 <- contextToNodeBoundWithOrderKeys canonicalNode orderKeys (srConstraint res) mC nC
                    ctxMn <- case ctxMn0 of
                        Just steps -> pure steps
                        Nothing ->
                            Left (InstantiationError "OpRaise (non-spine): could not compute context C^m_n from binding edges")

                    let hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                        aliasOld = InstInside (applyContext ctxMn hAbsBeta)

                        local =
                            instMany
                                [ InstIntro
                                , InstInside (InstBot nodeTy)
                                , InstUnder "β" aliasOld
                                ]

                        inst = underContext prefixBefore local

                    ty' <- applyInstantiation ty inst
                    ids1 <- insertAt insertIdx (Just n) ids
                    go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpMerge n m : rest) -> do
            mName <- binderNameFor ty ids m lookupBinder
            let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
            (inst, ids1) <- atBinder ids ty n (pure hAbs)
            ty' <- applyInstantiation ty inst
            go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaiseMerge n m : rest) -> do
            -- Paper Fig. 10 special-cases RaiseMerge(r, m) at the (flexible) expansion
            -- root r as !αm. We implement that case precisely: it applies only when
            -- n is the expansion root (up to union-find).
            let nC = canonicalNode n
                rC = canonicalNode orderRoot
            if nC == rC
                then do
                    mName <- binderNameFor ty ids m lookupBinder
                    ty' <- applyInstantiation ty (InstAbstr mName)
                    go ty' [] (composeInst phi (InstAbstr mName)) rest lookupBinder
                else do
                    -- Non-root RaiseMerge behaves like Merge inside the context of n.
                    case elemIndex (Just n) ids of
                        Nothing ->
                            Left (InstantiationError ("OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"))
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

    insertAt :: Int -> a -> [a] -> Either ElabError [a]
    insertAt i x xs
        | i < 0 = Left (InstantiationError "insertAt: negative index")
        | i > length xs = Left (InstantiationError "insertAt: index out of range")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in Right (pre ++ (x : rest))

    freeTypeVars :: ElabType -> [String]
    freeTypeVars = nub . goF []
      where
        goF :: [String] -> ElabType -> [String]
        goF bound ty0 = case ty0 of
            TVar v -> if v `elem` bound then [] else [v]
            TArrow a b -> goF bound a ++ goF bound b
            TBase _ -> []
            TBottom -> []
            TForall v mb body ->
                let fvBound = maybe [] (goF bound) mb
                    fvBody = goF (v : bound) body
                in fvBound ++ fvBody

-- | Collect TyVar NodeIds at a given level reachable from a node.
--
-- This is a legacy helper while we still have `tnVarLevel`-based generalization.
-- It is intentionally independent of `cGNodes`/`gBinds`.
varsAtLevelInNode :: (NodeId -> NodeId) -> IntMap.IntMap TyNode -> GNodeId -> NodeId -> IntSet.IntSet
varsAtLevelInNode canonical nodes lvl start0 = go IntSet.empty IntSet.empty [canonical start0]
  where
    go :: IntSet.IntSet -> IntSet.IntSet -> [NodeId] -> IntSet.IntSet
    go _ vars [] = vars
    go visited vars (nid0:rest)
        | IntSet.member (getNodeId nid0) visited = go visited vars rest
        | otherwise =
            let visited' = IntSet.insert (getNodeId nid0) visited
            in case IntMap.lookup (getNodeId nid0) nodes of
                Nothing -> go visited' vars rest
                Just node ->
                    let vars' = case node of
                            TyVar{ tnVarLevel = l } | l == lvl -> IntSet.insert (getNodeId nid0) vars
                            _ -> vars
                        kids = case node of
                            TyArrow{ tnDom = d, tnCod = c } -> [canonical d, canonical c]
                            TyForall{ tnBody = b } -> [canonical b]
                            TyExp{ tnBody = b } -> [canonical b]
                            _ -> []
                    in go visited' vars' (kids ++ rest)

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

elaborate :: SolveResult -> IntMap.IntMap EdgeWitness -> IntMap.IntMap EdgeTrace -> AnnExpr -> Either ElabError ElabTerm
elaborate res edgeWitnesses edgeTraces ann = go Map.empty ann
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
                let mTrace = IntMap.lookup eid edgeTraces
                let mSchemeInfo = case funAnn of
                        AVar v _ -> Map.lookup v env
                        _ -> Nothing
                phiFromEdgeWitnessWithTrace res mSchemeInfo mTrace ew

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
            term <- firstShow (elaborate solved (prEdgeWitnesses pres) (prEdgeTraces pres) ann')

            -- Also apply redirects to the root node, as it might have been a TyExp
            let root' = chaseRedirects (prRedirects pres) root

            -- Generalize at the root if it is a forall; otherwise treat it as monomorphic.
            let nodes = cNodes (srConstraint solved)
            sch <- case IntMap.lookup (getNodeId root') nodes of
                Just TyForall {} -> fst <$> firstShow (generalizeAt solved root' root')
                _ -> Forall [] <$> firstShow (reifyType solved root')
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
