module MLF.XMLF.Syntax (
    XmlfType (..),
    XmlfComp (..),
    XmlfTerm (..)
) where

import Data.List.NonEmpty (NonEmpty)
import MLF.Frontend.Syntax (Lit)

-- | Paper-faithful xMLF types (thesis §14.2.1, Fig. 14.2.1).
data XmlfType
    = XTVar String
    | XTArrow XmlfType XmlfType
    | XTBase String
    | XTCon String (NonEmpty XmlfType)
    | XTForall String XmlfType XmlfType
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

-- | xMLF terms (thesis §14.2.1, Fig. 14.2.2), plus literals as an implementation extension.
data XmlfTerm
    = XVar String
    | XLit Lit
    | XLam String XmlfType XmlfTerm
    | XApp XmlfTerm XmlfTerm
    | XTyAbs String XmlfType XmlfTerm
    | XTyInst XmlfTerm XmlfComp
    | XLet String XmlfTerm XmlfTerm
    deriving (Eq, Show)
