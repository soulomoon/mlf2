module MLF.Elab.Phi.TestSupport (
    VSpine,
    mkVSpine,
    reorderSpineRefsTo,
    assertSpineSync,
    vSpineBinderAt,
    vSpineBinderRefs,
    vSpineNameAt,
    normalizeInst,
) where

import qualified MLF.Elab.Sigma as Sigma
import MLF.Elab.Types (BoundType, Instantiation, TypeBinderRef)
import MLF.Elab.Phi.Omega.Normalize (normalizeInst)
import MLF.Elab.Phi.VSpine
    ( VSpine
    , assertSpineSync
    , mkVSpine
    , vSpineBinderAt
    , vSpineBinderRefs
    , vSpineNameAt
    )
import MLF.Util.ElabError (ElabError)

reorderSpineRefsTo
    :: Eq a
    => String
    -> [(TypeBinderRef, Maybe BoundType)]
    -> [a]
    -> [a]
    -> Either ElabError (Instantiation, [(TypeBinderRef, Maybe BoundType)], [a])
reorderSpineRefsTo = Sigma.bubbleReorderToFromSpineRefs
