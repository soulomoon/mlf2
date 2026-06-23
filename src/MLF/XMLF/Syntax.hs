module MLF.XMLF.Syntax (
    XmlfType (..),
    XmlfComp (..)
) where

import Data.List.NonEmpty (NonEmpty)

-- | Paper-faithful xMLF types (thesis §14.2.1, Fig. 14.2.1).
data XmlfType
    = XTVar String
    | XTArrow XmlfType XmlfType
    | XTBase String
    | XTCon String (NonEmpty XmlfType)
    | XTVarApp String (NonEmpty XmlfType)
    | XTForall String XmlfType XmlfType
    | XTMu String XmlfType
    | XTBottom
    deriving (Eq, Show)

-- | xMLF type computations (thesis §14.2.2, Fig. 14.2.5).
data XmlfComp
    = XCId
    | XCBot XmlfType
    | XCHyp String
    | XCInner XmlfComp
    | XCOuter String XmlfComp
    | XCElim
    | XCIntro
    | XCSeq XmlfComp XmlfComp
    deriving (Eq, Show)
