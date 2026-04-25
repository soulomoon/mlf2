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
    , ProgramDiagnostic (..)
    , CheckedProgram (..)
    , CheckedModule (..)
    , CheckedBinding (..)
    , SymbolNamespace (..)
    , SymbolOwnerIdentity (..)
    , SymbolIdentity (..)
    , SymbolOrigin (..)
    , SymbolSpelling (..)
    , ResolvedSymbol (..)
    , ResolvedReferenceKind (..)
    , ResolvedReference (..)
    , ResolvedScope (..)
    , ResolvedModule (..)
    , ResolvedProgram (..)
    , sameResolvedSymbol
    , Value (..)
    , renderProgramParseError
    , renderProgramDiagnostic
    , parseRawProgram
    , parseLocatedProgram
    , parseLocatedProgramWithFile
    , prettyProgram
    , preludeSource
    , preludeProgram
    , preludeLocatedProgram
    , withPrelude
    , withPreludeLocated
    , checkProgram
    , checkLocatedProgram
    , runProgram
    , runLocatedProgram
    , prettyValue
    ) where

import MLF.API
    ( ProgramParseError
    , parseLocatedProgram
    , parseLocatedProgramWithFile
    , parseRawProgram
    , prettyProgram
    , renderProgramParseError
    )
import MLF.Frontend.Syntax.Program
import MLF.Frontend.Program.Prelude
    ( preludeLocatedProgram
    , preludeProgram
    , preludeSource
    , withPrelude
    , withPreludeLocated
    )
import MLF.Pipeline
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    , ProgramDiagnostic (..)
    , ProgramError (..)
    , ResolvedModule (..)
    , ResolvedProgram (..)
    , ResolvedReference (..)
    , ResolvedReferenceKind (..)
    , ResolvedScope (..)
    , ResolvedSymbol (..)
    , sameResolvedSymbol
    , SymbolIdentity (..)
    , SymbolNamespace (..)
    , SymbolOrigin (..)
    , SymbolOwnerIdentity (..)
    , SymbolSpelling (..)
    , Value (..)
    , checkLocatedProgram
    , checkProgram
    , prettyValue
    , renderProgramDiagnostic
    , runLocatedProgram
    , runProgram
    )
