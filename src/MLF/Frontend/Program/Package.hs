module MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    , ProgramPackageDiscoveryError (..)
    , ProgramSourceUnit (..)
    , LocatedProgramSourceUnit (..)
    , ProgramPackage (..)
    , LocatedProgramPackage (..)
    , PackageModuleGraph (..)
    , PackageModuleGraphNode (..)
    , trivialPackageId
    , discoverLocatedProgramPackage
    , programSourceUnitFromProgram
    , locatedProgramSourceUnitFromLocated
    , trivialProgramPackage
    , trivialLocatedProgramPackage
    , programPackageModuleGraph
    , locatedProgramPackageModuleGraph
    , programPackageModuleIds
    , locatedProgramPackageModuleIds
    , prependProgramSourceUnit
    , prependLocatedProgramSourceUnit
    , programPackageProgram
    , programPackageOrderedProgram
    , locatedProgramPackagePackage
    , locatedProgramPackageProgram
    , locatedProgramPackageOrderedProgram
    ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, forM)
import Data.Bifunctor (first)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Frontend.Parse.Program
    ( ProgramParseError
    , parseLocatedProgramWithFile
    )
import MLF.Frontend.Program.Types (ProgramError (..))
import qualified MLF.Frontend.Syntax.Program as P
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.FilePath ((</>), normalise, takeExtension)

newtype PackageId = PackageId
    { packageIdName :: String
    }
    deriving (Eq, Ord, Show)

data PackageModuleId = PackageModuleId
    { packageModulePackageId :: PackageId
    , packageModuleName :: P.ModuleName
    }
    deriving (Eq, Ord, Show)

data ProgramPackageDiscoveryError
    = ProgramPackageDiscoveryReadError FilePath IOException
    | ProgramPackageDiscoveryParseError FilePath ProgramParseError
    deriving (Show)

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

data PackageModuleGraph = PackageModuleGraph
    { packageModuleGraphNodes :: [PackageModuleGraphNode]
    , packageModuleGraphOrder :: [PackageModuleId]
    }
    deriving (Eq, Show)

data PackageModuleGraphNode = PackageModuleGraphNode
    { packageModuleGraphNodeId :: PackageModuleId
    , packageModuleGraphNodeSourcePath :: Maybe FilePath
    , packageModuleGraphNodeImports :: [PackageModuleId]
    }
    deriving (Eq, Show)

data ModuleEntry = ModuleEntry
    { moduleEntryId :: PackageModuleId
    , moduleEntrySourcePath :: Maybe FilePath
    , moduleEntryModule :: P.Module
    }
    deriving (Eq, Show)

trivialPackageId :: PackageId
trivialPackageId = PackageId "<trivial-package>"

discoverLocatedProgramPackage ::
    PackageId ->
    FilePath ->
    IO (Either ProgramPackageDiscoveryError LocatedProgramPackage)
discoverLocatedProgramPackage packageId root = do
    sourcePathsResult <- try (discoverPackageSourceFiles root) :: IO (Either IOException [FilePath])
    case sourcePathsResult of
        Left err -> pure (Left (ProgramPackageDiscoveryReadError root err))
        Right sourcePaths -> do
            parsed <- mapM parsePackageSourceFile sourcePaths
            pure $ do
                sourceUnits <- sequence parsed
                pure
                    LocatedProgramPackage
                        { locatedProgramPackageId = packageId
                        , locatedProgramPackageSourceUnits = sourceUnits
                        }

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

programPackageModuleGraph :: ProgramPackage -> Either ProgramError PackageModuleGraph
programPackageModuleGraph package =
    packageModuleGraph
        (programPackageId package)
        [ (programSourceUnitPath sourceUnit, programSourceUnitModules sourceUnit)
        | sourceUnit <- programPackageSourceUnits package
        ]

locatedProgramPackageModuleGraph :: LocatedProgramPackage -> Either ProgramError PackageModuleGraph
locatedProgramPackageModuleGraph package =
    packageModuleGraph
        (locatedProgramPackageId package)
        [ (locatedProgramSourceUnitPath sourceUnit, locatedProgramSourceUnitModules sourceUnit)
        | sourceUnit <- locatedProgramPackageSourceUnits package
        ]

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

programPackageOrderedProgram :: ProgramPackage -> Either ProgramError P.Program
programPackageOrderedProgram package =
    P.Program . map moduleEntryModule
        <$> orderedPackageModules
            (programPackageId package)
            [ (programSourceUnitPath sourceUnit, programSourceUnitModules sourceUnit)
            | sourceUnit <- programPackageSourceUnits package
            ]

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
        , P.locatedProgramSpans = locatedProgramPackageSpanIndex package
        }

locatedProgramPackageOrderedProgram :: LocatedProgramPackage -> Either ProgramError P.LocatedProgram
locatedProgramPackageOrderedProgram package = do
    orderedModules <-
        orderedPackageModules
            (locatedProgramPackageId package)
            [ (locatedProgramSourceUnitPath sourceUnit, locatedProgramSourceUnitModules sourceUnit)
            | sourceUnit <- locatedProgramPackageSourceUnits package
            ]
    pure
        P.LocatedProgram
            { P.locatedProgram = P.Program (map moduleEntryModule orderedModules)
            , P.locatedProgramSpans = locatedProgramPackageSpanIndex package
            }

locatedProgramPackageSpanIndex :: LocatedProgramPackage -> P.ProgramSpanIndex
locatedProgramPackageSpanIndex package =
    foldl
        prependSpans
        P.emptyProgramSpanIndex
        (locatedProgramPackageSourceUnits package)
  where
    prependSpans spans sourceUnit =
        locatedProgramSourceUnitSpans sourceUnit `P.appendProgramSpanIndex` spans

discoverPackageSourceFiles :: FilePath -> IO [FilePath]
discoverPackageSourceFiles root = go (normalise root)
  where
    go dir = do
        entries <- sort <$> listDirectory dir
        fmap concat $
            forM entries $ \entry -> do
                let path = dir </> entry
                isDir <- doesDirectoryExist path
                isFile <- doesFileExist path
                if isDir
                    then go path
                    else
                        pure
                            [ normalise path
                            | isFile
                            , takeExtension path == ".mlfp"
                            ]

parsePackageSourceFile :: FilePath -> IO (Either ProgramPackageDiscoveryError LocatedProgramSourceUnit)
parsePackageSourceFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first (ProgramPackageDiscoveryReadError path) fileResult
        first
            (ProgramPackageDiscoveryParseError path)
            (locatedProgramSourceUnitFromLocated <$> parseLocatedProgramWithFile path source)

packageModuleGraph ::
    PackageId ->
    [(Maybe FilePath, [P.Module])] ->
    Either ProgramError PackageModuleGraph
packageModuleGraph packageId sourceUnits = do
    entries <- moduleEntries packageId sourceUnits
    ordered <- topoSortModuleEntries entries
    pure
        PackageModuleGraph
            { packageModuleGraphNodes = map packageModuleGraphNode entries
            , packageModuleGraphOrder = map moduleEntryId ordered
            }

orderedPackageModules ::
    PackageId ->
    [(Maybe FilePath, [P.Module])] ->
    Either ProgramError [ModuleEntry]
orderedPackageModules packageId sourceUnits =
    moduleEntries packageId sourceUnits >>= topoSortModuleEntries

moduleEntries ::
    PackageId ->
    [(Maybe FilePath, [P.Module])] ->
    Either ProgramError [ModuleEntry]
moduleEntries packageId sourceUnits = do
    let entries =
            [ ModuleEntry
                { moduleEntryId = PackageModuleId packageId (P.moduleName mod0)
                , moduleEntrySourcePath = sourcePath
                , moduleEntryModule = mod0
                }
            | (sourcePath, modules0) <- sourceUnits
            , mod0 <- modules0
            ]
    case firstDuplicate (map (P.moduleName . moduleEntryModule) entries) of
        Just duplicate -> Left (ProgramDuplicateModule duplicate)
        Nothing -> do
            let moduleNames = Set.fromList (map (P.moduleName . moduleEntryModule) entries)
            case firstUnknownImport moduleNames entries of
                Just missingModule -> Left (ProgramUnknownImportModule missingModule)
                Nothing -> pure entries

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (item : items)
        | item `Set.member` seen = Just item
        | otherwise = go (Set.insert item seen) items

firstUnknownImport :: Set.Set P.ModuleName -> [ModuleEntry] -> Maybe P.ModuleName
firstUnknownImport moduleNames entries =
    case
        [ importName
        | entry <- entries
        , importName <- moduleEntryImportNames entry
        , importName `Set.notMember` moduleNames
        ] of
        missingModule : _ -> Just missingModule
        [] -> Nothing

topoSortModuleEntries :: [ModuleEntry] -> Either ProgramError [ModuleEntry]
topoSortModuleEntries entries = do
    (_, _, orderedRev) <-
        foldM
            (visit [])
            (Set.empty, Set.empty, [])
            (map (P.moduleName . moduleEntryModule) entries)
    pure (reverse orderedRev)
  where
    moduleMap = Map.fromList [(P.moduleName (moduleEntryModule entry), entry) | entry <- entries]

    visit stack (tempMarks, permMarks, ordered) moduleName0
        | moduleName0 `Set.member` permMarks = pure (tempMarks, permMarks, ordered)
        | moduleName0 `Set.member` tempMarks = Left (ProgramImportCycle (cyclePath moduleName0 stack))
        | otherwise =
            case Map.lookup moduleName0 moduleMap of
                Nothing -> Left (ProgramUnknownImportModule moduleName0)
                Just entry -> do
                    let tempMarks' = Set.insert moduleName0 tempMarks
                    (tempMarksAfterImports, permMarksAfterImports, orderedAfterImports) <-
                        foldM
                            (visit (moduleName0 : stack))
                            (tempMarks', permMarks, ordered)
                            (moduleEntryImportNames entry)
                    pure
                        ( Set.delete moduleName0 tempMarksAfterImports
                        , Set.insert moduleName0 permMarksAfterImports
                        , entry : orderedAfterImports
                        )

cyclePath :: P.ModuleName -> [P.ModuleName] -> [P.ModuleName]
cyclePath moduleName0 stack =
    reverse (moduleName0 : takeUntil moduleName0 stack)

takeUntil :: (Eq a) => a -> [a] -> [a]
takeUntil _ [] = []
takeUntil target (item : items)
    | target == item = [item]
    | otherwise = item : takeUntil target items

packageModuleGraphNode :: ModuleEntry -> PackageModuleGraphNode
packageModuleGraphNode entry =
    PackageModuleGraphNode
        { packageModuleGraphNodeId = moduleEntryId entry
        , packageModuleGraphNodeSourcePath = moduleEntrySourcePath entry
        , packageModuleGraphNodeImports =
            [ PackageModuleId (packageModulePackageId (moduleEntryId entry)) importName
            | importName <- moduleEntryImportNames entry
            ]
        }

moduleEntryImportNames :: ModuleEntry -> [P.ModuleName]
moduleEntryImportNames entry =
    [ P.importModuleName import0
    | import0 <- P.moduleImports (moduleEntryModule entry)
    ]

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
