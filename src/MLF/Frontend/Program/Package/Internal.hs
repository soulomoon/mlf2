module MLF.Frontend.Program.Package.Internal
    ( PackageSourceOrigin (..)
    , ProgramSourceUnit (..)
    , LocatedProgramSourceUnit (..)
    , builtinPreludeProgramSourceUnit
    , builtinPreludeLocatedProgramSourceUnit
    )
where

import qualified Data.Map.Strict as Map
import qualified MLF.Frontend.Syntax.Program as P

data PackageSourceOrigin
    = OrdinaryPackageSource
    | BuiltinPreludePackageSource
    deriving (Eq, Show)

data ProgramSourceUnit = ProgramSourceUnitInternal
    { internalProgramSourceUnitPath :: Maybe FilePath
    , internalProgramSourceUnitModules :: [P.Module]
    , internalProgramSourceUnitOrigin :: PackageSourceOrigin
    }
    deriving (Eq, Show)

data LocatedProgramSourceUnit = LocatedProgramSourceUnitInternal
    { internalLocatedProgramSourceUnitPath :: Maybe FilePath
    , internalLocatedProgramSourceUnitModules :: [P.Module]
    , internalLocatedProgramSourceUnitSpans :: P.ProgramSpanIndex
    , internalLocatedProgramSourceUnitOrigin :: PackageSourceOrigin
    }
    deriving (Eq, Show)

-- These constructors live behind an unexposed owner module.  Production code
-- can mark source as builtin only through 'MLF.Frontend.Program.Prelude'; tests
-- use the explicitly named Prelude test-support seam.
builtinPreludeProgramSourceUnit :: FilePath -> P.Program -> ProgramSourceUnit
builtinPreludeProgramSourceUnit sourcePath program =
    ProgramSourceUnitInternal
        { internalProgramSourceUnitPath = Just sourcePath
        , internalProgramSourceUnitModules = P.programModules program
        , internalProgramSourceUnitOrigin = BuiltinPreludePackageSource
        }

builtinPreludeLocatedProgramSourceUnit :: P.LocatedProgram -> LocatedProgramSourceUnit
builtinPreludeLocatedProgramSourceUnit located =
    LocatedProgramSourceUnitInternal
        { internalLocatedProgramSourceUnitPath = locatedProgramSourcePath located
        , internalLocatedProgramSourceUnitModules = P.programModules (P.locatedProgram located)
        , internalLocatedProgramSourceUnitSpans = P.locatedProgramSpans located
        , internalLocatedProgramSourceUnitOrigin = BuiltinPreludePackageSource
        }

locatedProgramSourcePath :: P.LocatedProgram -> Maybe FilePath
locatedProgramSourcePath located =
    case Map.elems (P.spanModules (P.locatedProgramSpans located)) of
        sourceSpan : _ -> Just (P.sourceFile sourceSpan)
        [] -> Nothing
