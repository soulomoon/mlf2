{- |
Module      : MLF.XMLF
Description : Stable xMLF syntax, parser, and pretty-printing API

`MLF.XMLF` is the focused downstream entrypoint for explicit xMLF syntax.
Use it when you need to parse, inspect, or pretty-print xMLF types,
computations, and terms without pulling in the eMLF pipeline surface.
-}
module MLF.XMLF
    ( XmlfType (..)
    , XmlfComp (..)
    , XmlfTerm (..)
    , XmlfParseError
    , parseXmlfType
    , parseXmlfComp
    , parseXmlfTerm
    , renderXmlfParseError
    , prettyXmlfType
    , prettyXmlfComp
    , prettyXmlfTerm
    ) where

import MLF.XMLF.Parse
    ( XmlfParseError
    , parseXmlfComp
    , parseXmlfTerm
    , parseXmlfType
    , renderXmlfParseError
    )
import MLF.XMLF.Pretty
    ( prettyXmlfComp
    , prettyXmlfTerm
    , prettyXmlfType
    )
import MLF.XMLF.Syntax
    ( XmlfComp (..)
    , XmlfTerm (..)
    , XmlfType (..)
    )
