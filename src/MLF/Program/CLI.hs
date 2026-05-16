module MLF.Program.CLI
    ( programCliUsage
    , checkProgramArgs
    , runProgramArgs
    , emitBackendArgs
    , emitNativeArgs
    , checkProgramFile
    , emitBackendFile
    , emitNativeFile
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.List (intercalate)
import System.Directory (doesDirectoryExist, doesFileExist)

import MLF.Backend.Emission.Prepare
    ( prepareBackendEmissionFromLocatedPackage
    , renderBackendEmissionPreparationError
    )
import MLF.Backend.LLVM
    ( renderBackendLLVMError
    , renderCheckedProgramLLVM
    , renderCheckedProgramNativeLLVM
    )
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check (checkLocatedProgramPackage)
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage (..)
    , PackageId (..)
    , PackageRoot (..)
    , PackageSearchPath (..)
    , ProgramPackageDiscoveryError (..)
    , discoverLocatedProgramPackageFromSearchPath
    , locatedProgramSourceUnitFromLocated
    , trivialLocatedProgramPackage
    , trivialPackageId
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Frontend.Program.Run
    ( programRunOutput
    , runLocatedProgramPackageOutput
    )
import MLF.Frontend.Program.Types (renderProgramDiagnostic)
import MLF.Frontend.Syntax.Program (LocatedProgram)

data ProgramCliInput = ProgramCliInput
    { programCliInputPath :: FilePath
    , programCliInputSearchPaths :: [FilePath]
    }

programCliUsage :: String
programCliUsage =
    unlines
        [ "Usage:"
        , "  mlf2 check-program <file-or-root> [--search-path <root> ...]"
        , "  mlf2 run-program <file-or-root> [--search-path <root> ...]"
        , "  mlf2 emit-backend <file-or-root> [--search-path <root> ...]"
        , "  mlf2 emit-native <file-or-root> [--search-path <root> ...]"
        , ""
        , "A file input is loaded as a trivial local package source unit."
        , "A directory input is loaded as the primary local package root."
        , "--search-path appends ordered local package roots."
        , "check-program prepends the built-in Prelude and reports OK on success."
        , "run-program prepends the built-in Prelude and prints the pure result or IO stdout."
        , "emit-backend prepends the built-in Prelude and prints LLVM IR."
        , "emit-native prepends the built-in Prelude and prints LLVM IR with a native process entrypoint."
        ]

checkProgramArgs :: [String] -> IO (Either String String)
checkProgramArgs args =
    runLocatedPackageCommand args $ \package -> do
        _ <- first renderProgramDiagnostic (checkLocatedProgramPackage package)
        pure "OK\n"

runProgramArgs :: [String] -> IO (Either String String)
runProgramArgs args =
    runLocatedPackageCommand args $ \package ->
        first renderProgramDiagnostic $
            programRunOutput <$> runLocatedProgramPackageOutput package

emitBackendArgs :: [String] -> IO (Either String String)
emitBackendArgs args =
    runLocatedPackageCommand args $ \package -> do
        checked <-
            first
                renderBackendEmissionPreparationError
                (prepareBackendEmissionFromLocatedPackage package)
        first renderBackendLLVMError (renderCheckedProgramLLVM checked)

emitNativeArgs :: [String] -> IO (Either String String)
emitNativeArgs args =
    runLocatedPackageCommand args $ \package -> do
        checked <-
            first
                renderBackendEmissionPreparationError
                (prepareBackendEmissionFromLocatedPackage package)
        first renderBackendLLVMError (renderCheckedProgramNativeLLVM checked)

checkProgramFile :: FilePath -> IO (Either String String)
checkProgramFile path =
    checkProgramArgs [path]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path =
    runProgramArgs [path]

emitBackendFile :: FilePath -> IO (Either String String)
emitBackendFile path =
    emitBackendArgs [path]

emitNativeFile :: FilePath -> IO (Either String String)
emitNativeFile path =
    emitNativeArgs [path]

runLocatedPackageCommand ::
    [String] ->
    (LocatedProgramPackage -> Either String String) ->
    IO (Either String String)
runLocatedPackageCommand args command =
    case parseProgramCliInputArgs args of
        Left err ->
            pure (Left err)
        Right input -> do
            packageResult <- loadLocatedProgramPackageInput input
            pure (packageResult >>= command)

parseProgramCliInputArgs :: [String] -> Either String ProgramCliInput
parseProgramCliInputArgs args =
    case args of
        path : rest ->
            ProgramCliInput path <$> parseSearchPaths [] rest
        [] ->
            Left programCliUsage
  where
    parseSearchPaths acc rest =
        case rest of
            [] ->
                Right (reverse acc)
            "--search-path" : root : remaining ->
                parseSearchPaths (root : acc) remaining
            _ ->
                Left programCliUsage

loadLocatedProgramPackageInput :: ProgramCliInput -> IO (Either String LocatedProgramPackage)
loadLocatedProgramPackageInput input = do
    isFile <- doesFileExist path
    isDirectory <- doesDirectoryExist path
    case (isFile, isDirectory) of
        (True, _) ->
            loadFilePackage path searchPaths
        (_, True) ->
            loadRootPackage path searchPaths
        _ ->
            pure (Left ("program input path is neither a file nor a directory: " ++ path))
  where
    path = programCliInputPath input
    searchPaths = programCliInputSearchPaths input

loadFilePackage :: FilePath -> [FilePath] -> IO (Either String LocatedProgramPackage)
loadFilePackage path searchPaths = do
    locatedResult <- readLocatedProgramFile path
    case locatedResult of
        Left err ->
            pure (Left err)
        Right located
            | null searchPaths ->
                pure (Right (withPreludeLocatedPackage (trivialLocatedProgramPackage located)))
            | otherwise -> do
                searchPackageResult <-
                    discoverLocatedProgramPackageFromSearchPath
                        trivialPackageId
                        (PackageSearchPath (map PackageRoot searchPaths))
                pure $ do
                    searchPackage <- first renderProgramPackageDiscoveryError searchPackageResult
                    pure $
                        withPreludeLocatedPackage
                            LocatedProgramPackage
                                { locatedProgramPackageId = trivialPackageId
                                , locatedProgramPackageSourceUnits =
                                    locatedProgramSourceUnitFromLocated located
                                        : locatedProgramPackageSourceUnits searchPackage
                                }

loadRootPackage :: FilePath -> [FilePath] -> IO (Either String LocatedProgramPackage)
loadRootPackage root searchPaths = do
    packageResult <-
        discoverLocatedProgramPackageFromSearchPath
            cliPackageId
            (PackageSearchPath (map PackageRoot (root : searchPaths)))
    pure (withPreludeLocatedPackage <$> first renderProgramPackageDiscoveryError packageResult)

readLocatedProgramFile :: FilePath -> IO (Either String LocatedProgram)
readLocatedProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first (renderReadError path) fileResult
        first renderProgramParseError (parseLocatedProgramWithFile path source)

renderReadError :: FilePath -> IOException -> String
renderReadError path err =
    "failed to read `" ++ path ++ "`: " ++ show err

renderProgramPackageDiscoveryError :: ProgramPackageDiscoveryError -> String
renderProgramPackageDiscoveryError err =
    case err of
        ProgramPackageDiscoveryReadError path ioErr ->
            "failed to read package root `" ++ path ++ "`: " ++ show ioErr
        ProgramPackageDiscoveryParseError path parseErr ->
            "failed to parse package source `" ++ path ++ "`:\n" ++ renderProgramParseError parseErr
        ProgramPackageDiscoveryDuplicateModule moduleName sourcePaths ->
            "duplicate module `"
                ++ moduleName
                ++ "` discovered in package source files: "
                ++ intercalate ", " sourcePaths

cliPackageId :: PackageId
cliPackageId = PackageId "<cli-package>"
