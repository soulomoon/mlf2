module MLF.Frontend.Program.Prelude.TestSupport
    ( customBuiltinPreludeLocatedProgramSourceUnit
    )
where

import MLF.Frontend.Program.Package (LocatedProgramSourceUnit)
import MLF.Frontend.Program.Package.Internal
    ( builtinPreludeLocatedProgramSourceUnit
    )
import qualified MLF.Frontend.Syntax.Program as P

-- | Test-only seam for proving that the Prelude cache keys resolved content,
-- rather than assuming the one compiled-in Prelude source can never change.
customBuiltinPreludeLocatedProgramSourceUnit ::
    P.LocatedProgram ->
    LocatedProgramSourceUnit
customBuiltinPreludeLocatedProgramSourceUnit =
    builtinPreludeLocatedProgramSourceUnit
