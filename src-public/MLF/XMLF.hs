{- |
Module      : MLF.XMLF
Description : xMLF diagnostic pretty-printing API

`MLF.XMLF` is the focused downstream entrypoint for xMLF dumps. Terms are
checked `XmlfTerm` values; printed text is diagnostic output, not a source
format.
-}
module MLF.XMLF
    ( XmlfType (..)
    , XmlfComp (..)
    , XmlfTerm (..)
    , prettyXmlfType
    , prettyXmlfComp
    , prettyXmlfTerm
    ) where

import MLF.Types.Elab (XmlfTerm (..))
import MLF.XMLF.Pretty
    ( prettyXmlfComp
    , prettyXmlfTerm
    , prettyXmlfType
    )
import MLF.XMLF.Syntax
    ( XmlfComp (..)
    , XmlfType (..)
    )
