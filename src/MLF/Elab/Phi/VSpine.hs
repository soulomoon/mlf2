{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Elab.Phi.VSpine
Description : Virtual spine for static-scheme-based Omega translation
Copyright   : (c) 2024
License     : BSD-3-Clause

A VSpine tracks binder names, bounds, and identities symbolically,
mirroring the quantifier spine of an ElabType without requiring
incremental type-state via applyInstantiation.
-}
module MLF.Elab.Phi.VSpine (
    VSpine(..),
    BodyShape(..),
    mkVSpine,
    vSpineNames,
    vSpineBinderRefs,
    vSpineBounds,
    vSpineIds,
    vSpineLength,
    vSpineNull,
    vSpineBinderAt,
    vSpineNameAt,
    vSpineBoundAt,
    vSpineIdAt,
    vsDeleteAt,
    vsUpdateBound,
    vsInsertAt,
    assertSpineSync,
) where

import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Types.Elab
    ( BoundType
    , ElabType
    , Ty(..)
    , TypeBinderRef
    , typeBinderRefName
    )
import MLF.Reify.TypeOps (alphaEqType, splitForallsRefs)
import MLF.Util.ElabError (ElabError(..))

-- | Virtual spine: symbolic representation of a type's quantifier prefix.
-- Each binder carries its name, optional bound, and optional node identity.
data VSpine = VSpine
    { vsBinders :: [(TypeBinderRef, Maybe BoundType, Maybe NodeId)]
      -- ^ (binder ref, bound, identity) per quantifier, outermost first
    , vsBody    :: BodyShape
    } deriving (Show)

-- | Whether the type body (under all quantifiers) is bottom.
data BodyShape = BodyBottom | BodyNonBottom
    deriving (Eq, Show)

-- | Construct a VSpine from the current type and identity list.
mkVSpine :: ElabType -> [Maybe NodeId] -> VSpine
mkVSpine ty ids =
    let (qs, body) = splitForallsRefs ty
        binders = zipWith (\(ref, bound) mid -> (spineRef ref mid, bound, mid)) qs ids
        bodyShape = if alphaEqType body TBottom then BodyBottom else BodyNonBottom
    in VSpine binders bodyShape

spineRef :: TypeBinderRef -> Maybe NodeId -> TypeBinderRef
spineRef ref _mid =
    ref

-- Accessors

vSpineNames :: VSpine -> [String]
vSpineNames = map (\(ref, _, _) -> typeBinderRefName ref) . vsBinders

vSpineBinderRefs :: VSpine -> [TypeBinderRef]
vSpineBinderRefs = map (\(ref, _, _) -> ref) . vsBinders

vSpineBounds :: VSpine -> [Maybe BoundType]
vSpineBounds = map (\(_, b, _) -> b) . vsBinders

vSpineIds :: VSpine -> [Maybe NodeId]
vSpineIds = map (\(_, _, i) -> i) . vsBinders

vSpineLength :: VSpine -> Int
vSpineLength = length . vsBinders

vSpineNull :: VSpine -> Bool
vSpineNull = null . vsBinders

vSpineBinderAt :: VSpine -> Int -> Either ElabError (TypeBinderRef, Maybe BoundType, Maybe NodeId)
vSpineBinderAt vs i
    | i < 0 || i >= len =
        Left $
            PhiInvariantError $
                "VSpine: binder index " ++ show i ++ " out of range for spine length " ++ show len
    | otherwise =
        case drop i (vsBinders vs) of
            entry : _ -> Right entry
            [] ->
                Left $
                    PhiInvariantError $
                        "VSpine: binder index " ++ show i ++ " out of range for spine length " ++ show len
  where
    len = vSpineLength vs

vSpineNameAt :: VSpine -> Int -> Either ElabError String
vSpineNameAt vs i = do
    (ref, _, _) <- vSpineBinderAt vs i
    pure (typeBinderRefName ref)

vSpineBoundAt :: VSpine -> Int -> Either ElabError (Maybe BoundType)
vSpineBoundAt vs i = do
    (_, b, _) <- vSpineBinderAt vs i
    pure b

vSpineIdAt :: VSpine -> Int -> Either ElabError (Maybe NodeId)
vSpineIdAt vs i = do
    (_, _, mid) <- vSpineBinderAt vs i
    pure mid

-- Mutators

-- | Remove the binder at index @i@.
vsDeleteAt :: Int -> VSpine -> VSpine
vsDeleteAt i vs =
    let (pre, rest) = splitAt i (vsBinders vs)
    in vs { vsBinders = pre ++ drop 1 rest }

-- | Update the bound at index @i@.
vsUpdateBound :: Int -> Maybe BoundType -> VSpine -> VSpine
vsUpdateBound i newBound vs =
    let bs = vsBinders vs
        (pre, rest) = splitAt i bs
    in case rest of
        ((ref, _, mid) : rs) -> vs { vsBinders = pre ++ (ref, newBound, mid) : rs }
        [] -> vs  -- out of range: no-op

-- | Insert a binder at index @i@.
vsInsertAt :: Int -> (TypeBinderRef, Maybe BoundType, Maybe NodeId) -> VSpine -> VSpine
vsInsertAt i b vs =
    let (pre, rest) = splitAt i (vsBinders vs)
    in vs { vsBinders = pre ++ b : rest }

-- | Assert that a VSpine is consistent with the given type and identity list.
-- Returns Left with a diagnostic if they disagree.
assertSpineSync :: VSpine -> ElabType -> [Maybe NodeId] -> Either ElabError ()
assertSpineSync vs ty ids = do
    let (qs, _) = splitForallsRefs ty
        vsNames = vSpineNames vs
        tyNames = map (typeBinderRefName . fst) qs
        vsIds = vSpineIds vs
    if vsNames /= tyNames
        then Left $ PhiInvariantError $
            "VSpine desync (names): vs=" ++ show vsNames ++ " ty=" ++ show tyNames
        else if vsIds /= ids
            then Left $ PhiInvariantError $
                "VSpine desync (ids): vs=" ++ show vsIds ++ " ids=" ++ show ids
            else Right ()
