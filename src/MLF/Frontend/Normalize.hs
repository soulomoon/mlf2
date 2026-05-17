{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Frontend.Normalize
  ( -- * Normalization errors
    NormalizationError (..),

    -- * Type normalization
    normalizeType,

    -- * Expression normalization
    normalizeExpr,

    -- * Utilities (exported for testing)
    freeVarsSrcType,
    substSrcType,
  )
where

import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Frontend.Syntax
import MLF.Util.Names (freshNameLike)

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
  = -- | @SelfBoundVariable v body@: ∀(v ⩾ v). body is invalid.
    SelfBoundVariable String SrcType
  | -- | A bound expected to normalize into 'StructBound' still contains a
    -- bare variable subtree (for example after nested alias inlining).
    NonStructuralBoundInStructContext SrcType
  | -- | A type lambda was still present after normalization.
    ResidualTypeLambda SrcType
  | -- | A type application could not be reduced to a core-supported head.
    UnsupportedTypeApplication SrcType
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
      STVarApp name args ->
        let headVars =
              if Set.member name bound
                then Set.empty
                else Set.singleton name
         in headVars `Set.union` foldMap (go bound) args
      STTyLam v body ->
        go (Set.insert v bound) body
      STTyApp fun arg ->
        Set.union (go bound fun) (go bound arg)
      STForall v mb body ->
        let bound' = Set.insert v bound
            freeBound = maybe Set.empty (go bound . unSrcBound) mb
         in Set.union freeBound (go bound' body)
      STMu v body ->
        go (Set.insert v bound) body
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
    mapRawBound f = mkSrcBound . f . unSrcBound

    goSub = \case
      STVar v
        | v == x -> s
        | otherwise -> STVar v
      STArrow a b -> STArrow (goSub a) (goSub b)
      STBase b -> STBase b
      STCon c args -> STCon c (fmap goSub args)
      STVarApp name args ->
        let args' = fmap goSub args
         in case replacementHead name args' of
              Just ty -> ty
              Nothing -> STVarApp name args'
      STTyLam v body
        | v == x ->
            STTyLam v body
        | Set.member v freeS ->
            let used =
                  Set.unions
                    [ freeS,
                      freeVarsSrcType body,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                body' = substSrcType v (STVar v') body
             in STTyLam v' (goSub body')
        | otherwise ->
            STTyLam v (goSub body)
      STTyApp fun arg ->
        STTyApp (goSub fun) (goSub arg)
      STBottom -> STBottom
      STForall v mb body
        | v == x ->
            -- x is shadowed; only substitute in the bound, not the body
            STForall v (fmap (mapRawBound goSub) mb) body
        | Set.member v freeS ->
            -- v would capture a free variable in s; alpha-rename v
            let used =
                  Set.unions
                    [ freeS,
                      freeVarsSrcType body,
                      maybe Set.empty (freeVarsSrcType . unSrcBound) mb,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                body' = substSrcType v (STVar v') body
             in STForall v' (fmap (mapRawBound goSub) mb) (goSub body')
        | otherwise ->
            STForall v (fmap (mapRawBound goSub) mb) (goSub body)
      STMu v body
        | v == x ->
            STMu v body
        | Set.member v freeS ->
            let used =
                  Set.unions
                    [ freeS,
                      freeVarsSrcType body,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                body' = substSrcType v (STVar v') body
             in STMu v' (goSub body')
        | otherwise ->
            STMu v (goSub body)

    replacementHead name args
      | name /= x = Nothing
      | otherwise =
          case s of
            STVar replacementName -> Just (STVarApp replacementName args)
            STBase replacementName -> Just (STCon replacementName args)
            STCon replacementName replacementArgs -> Just (STCon replacementName (replacementArgs <> args))
            STVarApp replacementName replacementArgs -> Just (STVarApp replacementName (replacementArgs <> args))
            _ -> Just (foldl STTyApp s (neToList args))

    neToList (headArg :| restArgs) = headArg : restArgs

-- ---------------------------------------------------------------------------
-- Fresh name generation
-- ---------------------------------------------------------------------------
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
      STVar v -> Right (STVar v)
      STArrow a b -> STArrow <$> go a <*> go b
      STBase b -> Right (STBase b)
      STCon c args -> STCon c <$> traverse go args
      STVarApp v args -> STVarApp v <$> traverse go args
      STTyLam v body ->
        Left (ResidualTypeLambda (STTyLam v body))
      ty@STTyApp {} ->
        normalizeTypeApplication ty
      STBottom -> Right STBottom
      STForall v Nothing body ->
        STForall v Nothing <$> go body
      STForall v (Just bound) body -> case bound of
        SrcBound (STVar alias)
          | alias == v ->
              Left (SelfBoundVariable v body)
          | otherwise ->
              -- Alias bound: inline by substituting v → alias in body
              let body' = substSrcType v (STVar alias) body
               in go body'
        SrcBound boundTy -> do
          -- Structural bound: normalize and convert to StructBound
          sb <- normalizeBound boundTy
          STForall v (Just (mkNormBound sb)) <$> go body
      STMu v body ->
        STMu v <$> go body

    normalizeBound :: SrcType -> Either NormalizationError StructBound
    normalizeBound ty = go ty >>= normToStructBound

    normToStructBound :: NormSrcType -> Either NormalizationError StructBound
    normToStructBound = \case
      STVar v -> Left (NonStructuralBoundInStructContext (STVar v))
      STArrow a b -> Right (STArrow a b)
      STBase b -> Right (STBase b)
      STCon c args -> Right (STCon c args)
      STVarApp v args -> Right (STVarApp v args)
      STForall v mb body -> Right (STForall v mb body)
      STMu v body -> Right (STMu v body)
      STBottom -> Right STBottom
      STTyLam v body -> Left (ResidualTypeLambda (forgetStageAsTop (STTyLam v body)))
      STTyApp fun arg -> Left (UnsupportedTypeApplication (forgetStageAsTop (STTyApp fun arg)))

    normalizeTypeApplication :: SrcType -> Either NormalizationError NormSrcType
    normalizeTypeApplication original =
      reduceHead rawHead rawArgs
      where
        (rawHead, rawArgs) = collectApps original

        reduceHead headTy [] = go headTy
        reduceHead (STTyLam v body) (arg : rest) =
          reduceHead (substSrcType v arg body) rest
        reduceHead headTy args = do
          headNorm <- go headTy
          foldlM (applyNormalized original) headNorm args

    applyNormalized :: SrcType -> NormSrcType -> SrcType -> Either NormalizationError NormSrcType
    applyNormalized original fun rawArg = do
      arg <- go rawArg
      case fun of
        STVar name -> Right (STVarApp name (arg :| []))
        STBase name -> Right (STCon name (arg :| []))
        STCon name args -> Right (STCon name (args <> (arg :| [])))
        STVarApp name args -> Right (STVarApp name (args <> (arg :| [])))
        _ -> Left (UnsupportedTypeApplication original)

    collectApps :: SrcType -> (SrcType, [SrcType])
    collectApps = goApps []
      where
        goApps args (STTyApp fun arg) = goApps (arg : args) fun
        goApps args headTy = (headTy, args)

    foldlM :: (a -> b -> Either e a) -> a -> [b] -> Either e a
    foldlM _ acc [] = Right acc
    foldlM f acc (x : xs) = f acc x >>= \acc' -> foldlM f acc' xs

forgetStageAsTop :: SrcTy n v -> SrcType
forgetStageAsTop = \case
  STVar name -> STVar name
  STArrow dom cod -> STArrow (forgetStageAsTop dom) (forgetStageAsTop cod)
  STBase name -> STBase name
  STCon name args -> STCon name (fmap forgetStageAsTop args)
  STVarApp name args -> STVarApp name (fmap forgetStageAsTop args)
  STTyLam name body -> STTyLam name (forgetStageAsTop body)
  STTyApp fun arg -> STTyApp (forgetStageAsTop fun) (forgetStageAsTop arg)
  STForall name mb body ->
    STForall name (fmap (mkSrcBound . forgetStageAsTop . unSrcBound) mb) (forgetStageAsTop body)
  STMu name body -> STMu name (forgetStageAsTop body)
  STBottom -> STBottom

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
