module MLF.Elab.Phi.TestSupport (
    VSpine,
    mkVSpine,
    reorderSpineTo,
    assertSpineSync,
    vSpineBinderAt,
    vSpineNameAt,
) where

import qualified MLF.Elab.Sigma as Sigma
import MLF.Elab.Types (BoundType, Instantiation)
import MLF.Elab.Phi.VSpine
    ( VSpine
    , assertSpineSync
    , mkVSpine
    , vSpineBinderAt
    , vSpineNameAt
    )
import MLF.Util.ElabError (ElabError)

reorderSpineTo
    :: Eq a
    => String
    -> [(String, Maybe BoundType)]
    -> [a]
    -> [a]
    -> Either ElabError (Instantiation, [(String, Maybe BoundType)], [a])
reorderSpineTo = Sigma.bubbleReorderToFromSpine
