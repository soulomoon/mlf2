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
    vSpineBounds,
    vSpineIds,
    vSpineLength,
    vSpineNull,
    vSpineNameAt,
    vSpineBoundAt,
    vSpineIdAt,
    vsDeleteAt,
    vsPrepend,
    vsUpdateBound,
    vsInsertAt,
    assertSpineSync,
) where

import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Types.Elab (BoundType, ElabType, Ty(..))
import MLF.Reify.TypeOps (alphaEqType, splitForalls)
import MLF.Util.ElabError (ElabError(..))

-- | Virtual spine: symbolic representation of a type's quantifier prefix.
-- Each binder carries its name, optional bound, and optional node identity.
data VSpine = VSpine
    { vsBinders :: [(String, Maybe BoundType, Maybe NodeId)]
      -- ^ (name, bound, identity) per quantifier, outermost first
    , vsBody    :: BodyShape
    } deriving (Show)

-- | Whether the type body (under all quantifiers) is bottom.
data BodyShape = BodyBottom | BodyNonBottom
    deriving (Eq, Show)

-- | Construct a VSpine from the current type and identity list.
mkVSpine :: ElabType -> [Maybe NodeId] -> VSpine
mkVSpine ty ids =
    let (qs, body) = splitForalls ty
        binders = zipWith (\(name, bound) mid -> (name, bound, mid)) qs ids
        bodyShape = if alphaEqType body TBottom then BodyBottom else BodyNonBottom
    in VSpine binders bodyShape

-- Accessors

vSpineNames :: VSpine -> [String]
vSpineNames = map (\(n, _, _) -> n) . vsBinders

vSpineBounds :: VSpine -> [Maybe BoundType]
vSpineBounds = map (\(_, b, _) -> b) . vsBinders

vSpineIds :: VSpine -> [Maybe NodeId]
vSpineIds = map (\(_, _, i) -> i) . vsBinders

vSpineLength :: VSpine -> Int
vSpineLength = length . vsBinders

vSpineNull :: VSpine -> Bool
vSpineNull = null . vsBinders

vSpineNameAt :: VSpine -> Int -> String
vSpineNameAt vs i = let (n, _, _) = vsBinders vs !! i in n

vSpineBoundAt :: VSpine -> Int -> Maybe BoundType
vSpineBoundAt vs i = let (_, b, _) = vsBinders vs !! i in b

vSpineIdAt :: VSpine -> Int -> Maybe NodeId
vSpineIdAt vs i = let (_, _, mid) = vsBinders vs !! i in mid

-- Mutators

-- | Remove the binder at index @i@.
vsDeleteAt :: Int -> VSpine -> VSpine
vsDeleteAt i vs =
    let (pre, rest) = splitAt i (vsBinders vs)
    in vs { vsBinders = pre ++ drop 1 rest }

-- | Prepend a binder at the front.
vsPrepend :: (String, Maybe BoundType, Maybe NodeId) -> VSpine -> VSpine
vsPrepend b vs = vs { vsBinders = b : vsBinders vs }

-- | Update the bound at index @i@.
vsUpdateBound :: Int -> Maybe BoundType -> VSpine -> VSpine
vsUpdateBound i newBound vs =
    let bs = vsBinders vs
        (pre, rest) = splitAt i bs
    in case rest of
        ((n, _, mid) : rs) -> vs { vsBinders = pre ++ (n, newBound, mid) : rs }
        [] -> vs  -- out of range: no-op

-- | Insert a binder at index @i@.
vsInsertAt :: Int -> (String, Maybe BoundType, Maybe NodeId) -> VSpine -> VSpine
vsInsertAt i b vs =
    let (pre, rest) = splitAt i (vsBinders vs)
    in vs { vsBinders = pre ++ b : rest }

-- | Assert that a VSpine is consistent with the given type and identity list.
-- Returns Left with a diagnostic if they disagree.
assertSpineSync :: VSpine -> ElabType -> [Maybe NodeId] -> Either ElabError ()
assertSpineSync vs ty ids = do
    let (qs, _) = splitForalls ty
        vsNames = vSpineNames vs
        tyNames = map fst qs
        vsIds = vSpineIds vs
    if vsNames /= tyNames
        then Left $ PhiInvariantError $
            "VSpine desync (names): vs=" ++ show vsNames ++ " ty=" ++ show tyNames
        else if vsIds /= ids
            then Left $ PhiInvariantError $
                "VSpine desync (ids): vs=" ++ show vsIds ++ " ids=" ++ show ids
            else Right ()
