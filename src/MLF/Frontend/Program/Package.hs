module MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    , ProgramSourceUnit (..)
    , LocatedProgramSourceUnit (..)
    , ProgramPackage (..)
    , LocatedProgramPackage (..)
    , trivialPackageId
    , programSourceUnitFromProgram
    , locatedProgramSourceUnitFromLocated
    , trivialProgramPackage
    , trivialLocatedProgramPackage
    , programPackageModuleIds
    , locatedProgramPackageModuleIds
    , prependProgramSourceUnit
    , prependLocatedProgramSourceUnit
    , programPackageProgram
    , locatedProgramPackagePackage
    , locatedProgramPackageProgram
    ) where

import qualified Data.Map.Strict as Map
import qualified MLF.Frontend.Syntax.Program as P

newtype PackageId = PackageId
    { packageIdName :: String
    }
    deriving (Eq, Ord, Show)

data PackageModuleId = PackageModuleId
    { packageModulePackageId :: PackageId
    , packageModuleName :: P.ModuleName
    }
    deriving (Eq, Ord, Show)

data ProgramSourceUnit = ProgramSourceUnit
    { programSourceUnitPath :: Maybe FilePath
    , programSourceUnitModules :: [P.Module]
    }
    deriving (Eq, Show)

data LocatedProgramSourceUnit = LocatedProgramSourceUnit
    { locatedProgramSourceUnitPath :: Maybe FilePath
    , locatedProgramSourceUnitModules :: [P.Module]
    , locatedProgramSourceUnitSpans :: P.ProgramSpanIndex
    }
    deriving (Eq, Show)

data ProgramPackage = ProgramPackage
    { programPackageId :: PackageId
    , programPackageSourceUnits :: [ProgramSourceUnit]
    }
    deriving (Eq, Show)

data LocatedProgramPackage = LocatedProgramPackage
    { locatedProgramPackageId :: PackageId
    , locatedProgramPackageSourceUnits :: [LocatedProgramSourceUnit]
    }
    deriving (Eq, Show)

trivialPackageId :: PackageId
trivialPackageId = PackageId "<trivial-package>"

programSourceUnitFromProgram :: P.Program -> ProgramSourceUnit
programSourceUnitFromProgram program =
    ProgramSourceUnit
        { programSourceUnitPath = Nothing
        , programSourceUnitModules = P.programModules program
        }

locatedProgramSourceUnitFromLocated :: P.LocatedProgram -> LocatedProgramSourceUnit
locatedProgramSourceUnitFromLocated located =
    LocatedProgramSourceUnit
        { locatedProgramSourceUnitPath = locatedProgramSourcePath located
        , locatedProgramSourceUnitModules = P.programModules (P.locatedProgram located)
        , locatedProgramSourceUnitSpans = P.locatedProgramSpans located
        }

trivialProgramPackage :: P.Program -> ProgramPackage
trivialProgramPackage program =
    ProgramPackage
        { programPackageId = trivialPackageId
        , programPackageSourceUnits = [programSourceUnitFromProgram program]
        }

trivialLocatedProgramPackage :: P.LocatedProgram -> LocatedProgramPackage
trivialLocatedProgramPackage located =
    LocatedProgramPackage
        { locatedProgramPackageId = trivialPackageId
        , locatedProgramPackageSourceUnits = [locatedProgramSourceUnitFromLocated located]
        }

programPackageModuleIds :: ProgramPackage -> [PackageModuleId]
programPackageModuleIds package =
    concatMap
        (programSourceUnitModuleIds (programPackageId package))
        (programPackageSourceUnits package)

locatedProgramPackageModuleIds :: LocatedProgramPackage -> [PackageModuleId]
locatedProgramPackageModuleIds package =
    concatMap
        (locatedProgramSourceUnitModuleIds (locatedProgramPackageId package))
        (locatedProgramPackageSourceUnits package)

prependProgramSourceUnit :: ProgramSourceUnit -> ProgramPackage -> ProgramPackage
prependProgramSourceUnit sourceUnit package =
    package {programPackageSourceUnits = sourceUnit : programPackageSourceUnits package}

prependLocatedProgramSourceUnit ::
    LocatedProgramSourceUnit ->
    LocatedProgramPackage ->
    LocatedProgramPackage
prependLocatedProgramSourceUnit sourceUnit package =
    package {locatedProgramPackageSourceUnits = sourceUnit : locatedProgramPackageSourceUnits package}

programPackageProgram :: ProgramPackage -> P.Program
programPackageProgram package =
    P.Program (concatMap programSourceUnitModules (programPackageSourceUnits package))

locatedProgramPackagePackage :: LocatedProgramPackage -> ProgramPackage
locatedProgramPackagePackage package =
    ProgramPackage
        { programPackageId = locatedProgramPackageId package
        , programPackageSourceUnits =
            map locatedProgramSourceUnitProgramSourceUnit
                (locatedProgramPackageSourceUnits package)
        }

locatedProgramPackageProgram :: LocatedProgramPackage -> P.LocatedProgram
locatedProgramPackageProgram package =
    P.LocatedProgram
        { P.locatedProgram =
            P.Program
                (concatMap locatedProgramSourceUnitModules (locatedProgramPackageSourceUnits package))
        , P.locatedProgramSpans =
            foldl
                prependSpans
                P.emptyProgramSpanIndex
                (locatedProgramPackageSourceUnits package)
        }
  where
    prependSpans spans sourceUnit =
        locatedProgramSourceUnitSpans sourceUnit `P.appendProgramSpanIndex` spans

programSourceUnitModuleIds :: PackageId -> ProgramSourceUnit -> [PackageModuleId]
programSourceUnitModuleIds packageId sourceUnit =
    [ PackageModuleId packageId (P.moduleName mod0)
    | mod0 <- programSourceUnitModules sourceUnit
    ]

locatedProgramSourceUnitModuleIds :: PackageId -> LocatedProgramSourceUnit -> [PackageModuleId]
locatedProgramSourceUnitModuleIds packageId sourceUnit =
    [ PackageModuleId packageId (P.moduleName mod0)
    | mod0 <- locatedProgramSourceUnitModules sourceUnit
    ]

locatedProgramSourceUnitProgramSourceUnit :: LocatedProgramSourceUnit -> ProgramSourceUnit
locatedProgramSourceUnitProgramSourceUnit sourceUnit =
    ProgramSourceUnit
        { programSourceUnitPath = locatedProgramSourceUnitPath sourceUnit
        , programSourceUnitModules = locatedProgramSourceUnitModules sourceUnit
        }

locatedProgramSourcePath :: P.LocatedProgram -> Maybe FilePath
locatedProgramSourcePath located =
    case Map.elems (P.spanModules (P.locatedProgramSpans located)) of
        sourceSpan : _ -> Just (P.sourceFile sourceSpan)
        [] -> Nothing
