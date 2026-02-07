{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module MLF.Frontend.Normalize (
    -- * Normalization errors
    NormalizationError (..),
    -- * Type normalization
    normalizeType,
    -- * Expression normalization
    normalizeExpr,
    -- * Utilities (exported for testing)
    freeVarsSrcType,
    substSrcType,
) where

import qualified Data.Set as Set

import MLF.Frontend.Syntax

{- Note [Frontend type normalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normalization inlines alias bounds in forall types. An alias bound is a forall
whose bound is a bare variable:

    ∀(b ⩾ a). body   →   body[b := a]

After normalization, every forall bound is structural (arrow, base, con, forall,
bottom) or absent. This is enforced by the 'NormSrcType'/'StructBound' types
which have no variable constructor for bounds.

Self-bound foralls like ∀(a ⩾ a). body are invalid and produce an explicit
error rather than looping.

Normalization uses capture-avoiding substitution: if inlining a variable would
capture a free variable in the replacement, binders are alpha-renamed.
-}

-- | Errors that can occur during frontend type normalization.
data NormalizationError
    = SelfBoundVariable String SrcType
    -- ^ @SelfBoundVariable v body@: ∀(v ⩾ v). body is invalid.
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Free variables
-- ---------------------------------------------------------------------------

-- | Compute free type variables of a 'SrcType', respecting binder scope.
freeVarsSrcType :: SrcType -> Set.Set String
freeVarsSrcType = go Set.empty
  where
    go bound = \case
        STVar v
            | Set.member v bound -> Set.empty
            | otherwise -> Set.singleton v
        STArrow a b -> Set.union (go bound a) (go bound b)
        STBase _ -> Set.empty
        STCon _ args -> foldMap (go bound) args
        STForall v mb body ->
            let bound' = Set.insert v bound
                freeBound = maybe Set.empty (go bound) mb
            in Set.union freeBound (go bound' body)
        STBottom -> Set.empty

-- ---------------------------------------------------------------------------
-- Capture-avoiding substitution on SrcType
-- ---------------------------------------------------------------------------

-- | @substSrcType x s ty@ substitutes free occurrences of @x@ with @s@ in
-- @ty@, alpha-renaming binders as needed to avoid capture.
substSrcType :: String -> SrcType -> SrcType -> SrcType
substSrcType x s = goSub
  where
    freeS = freeVarsSrcType s

    goSub = \case
        STVar v
            | v == x    -> s
            | otherwise -> STVar v
        STArrow a b -> STArrow (goSub a) (goSub b)
        STBase b -> STBase b
        STCon c args -> STCon c (fmap goSub args)
        STBottom -> STBottom
        STForall v mb body
            | v == x ->
                -- x is shadowed; only substitute in the bound, not the body
                STForall v (fmap goSub mb) body
            | Set.member v freeS ->
                -- v would capture a free variable in s; alpha-rename v
                let used = Set.unions
                        [ freeS
                        , freeVarsSrcType body
                        , maybe Set.empty freeVarsSrcType mb
                        , Set.singleton v
                        ]
                    v' = freshNameLike v used
                    body' = substSrcType v (STVar v') body
                in STForall v' (fmap goSub mb) (goSub body')
            | otherwise ->
                STForall v (fmap goSub mb) (goSub body)

-- ---------------------------------------------------------------------------
-- Fresh name generation
-- ---------------------------------------------------------------------------

freshNameLike :: String -> Set.Set String -> String
freshNameLike base used =
    case filter (`Set.notMember` used) candidates of
        (x:_) -> x
        []    -> base  -- unreachable: infinite list always has a match
  where
    candidates = base : [base ++ show i | i <- [(1::Int)..]]

-- ---------------------------------------------------------------------------
-- Type normalization: SrcType → NormSrcType
-- ---------------------------------------------------------------------------

-- | Normalize a raw 'SrcType' into a 'NormSrcType'.
--
-- Alias bounds (∀(b ⩾ a). body where the bound is a bare variable) are
-- inlined via capture-avoiding substitution. The resulting type uses
-- 'StructBound' for forall bounds, which has no variable constructor.
--
-- Self-bound foralls (∀(a ⩾ a). body) produce a 'SelfBoundVariable' error.
normalizeType :: SrcType -> Either NormalizationError NormSrcType
normalizeType = go
  where
    go :: SrcType -> Either NormalizationError NormSrcType
    go = \case
        STVar v -> Right (NSTVar v)
        STArrow a b -> NSTArrow <$> go a <*> go b
        STBase b -> Right (NSTBase b)
        STCon c args -> NSTCon c <$> traverse go args
        STBottom -> Right NSTBottom
        STForall v Nothing body ->
            NSTForall v Nothing <$> go body
        STForall v (Just bound) body -> case bound of
            STVar alias
                | alias == v ->
                    Left (SelfBoundVariable v body)
                | otherwise ->
                    -- Alias bound: inline by substituting v → alias in body
                    let body' = substSrcType v (STVar alias) body
                    in go body'
            _ -> do
                -- Structural bound: normalize and convert to StructBound
                sb <- normalizeBound bound
                NSTForall v (Just sb) <$> go body

    normalizeBound :: SrcType -> Either NormalizationError StructBound
    normalizeBound = \case
        STVar _ ->
            -- This case is handled above in the STForall branch;
            -- if we reach here it means a variable appeared as a nested
            -- bound component, which is fine — it stays as a NormSrcType var.
            -- But StructBound has no variable constructor, so this is
            -- unreachable from the STForall handler (which peels off STVar).
            error "normalizeBound: unreachable — STVar handled in STForall branch"
        STArrow a b -> SBArrow <$> go a <*> go b
        STBase b -> Right (SBBase b)
        STCon c args -> SBCon c <$> traverse go args
        STBottom -> Right SBBottom
        STForall v Nothing body ->
            SBForall v Nothing <$> go body
        STForall v (Just bound) body -> case bound of
            STVar alias
                | alias == v ->
                    Left (SelfBoundVariable v body)
                | otherwise ->
                    -- Alias in nested bound: inline and re-normalize
                    let body' = substSrcType v (STVar alias) body
                    in normalizeBound body'
            _ -> do
                sb <- normalizeBound bound
                SBForall v (Just sb) <$> go body

-- ---------------------------------------------------------------------------
-- Expression normalization: Expr s SrcType → Expr s NormSrcType
-- ---------------------------------------------------------------------------

-- | Normalize all type annotations in a surface expression.
normalizeExpr :: SurfaceExpr -> Either NormalizationError NormSurfaceExpr
normalizeExpr = \case
    EVar v -> Right (EVar v)
    ELit l -> Right (ELit l)
    ELam v body -> ELam v <$> normalizeExpr body
    EApp f a -> EApp <$> normalizeExpr f <*> normalizeExpr a
    ELet v rhs body -> ELet v <$> normalizeExpr rhs <*> normalizeExpr body
    ELamAnn v ty body -> ELamAnn v <$> normalizeType ty <*> normalizeExpr body
    EAnn e ty -> EAnn <$> normalizeExpr e <*> normalizeType ty