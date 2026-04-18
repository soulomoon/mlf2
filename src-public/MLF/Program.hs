-- |
-- Module      : MLF.Program
-- Description : Compatibility shim for the unified `.mlfp` surface
--
-- Prefer `MLF.API` for `.mlfp` parsing/pretty-printing and `MLF.Pipeline` for
-- `.mlfp` elaboration/checking/runtime on the shared eMLF path. This module remains as a thin
-- backwards-compatible re-export.
module MLF.Program
    ( module MLF.Frontend.Syntax.Program
    , ProgramParseError
    , ProgramError (..)
    , CheckedProgram (..)
    , CheckedModule (..)
    , CheckedBinding (..)
    , Value (..)
    , renderProgramParseError
    , parseRawProgram
    , prettyProgram
    , checkProgram
    , runProgram
    , prettyValue
    ) where

import MLF.API
    ( ProgramParseError
    , parseRawProgram
    , prettyProgram
    , renderProgramParseError
    )
import MLF.Frontend.Syntax.Program
import MLF.Pipeline
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    , ProgramError (..)
    , Value (..)
    , checkProgram
    , prettyValue
    , runProgram
    )
