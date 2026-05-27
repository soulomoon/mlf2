{-# LANGUAGE LambdaCase #-}

module CheckedProgramTestSupport
  ( CheckedProgramArtifact (..),
    checkedProgramArtifactFromLocatedPackage,
    checkedProgramArtifactFromPackageRoots,
    checkedProgramArtifactFromFile,
    checkedProgramArtifactFromSource,
    checkedArtifactCheckOutput,
    checkedArtifactRunOutput,
    checkedArtifactBackendLLVM,
    checkedArtifactNativeLLVM,
    checkProgramFileCached,
    runProgramFileCached,
    emitBackendFileCached,
    emitNativeFileCached,
    emitBackendSourceCached,
    emitNativeSourceCached,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import MLF.Backend.Emission.Prepare (prepareCheckedProgramForBackendEmission)
import MLF.Backend.LLVM
  ( renderBackendLLVMError,
    renderCheckedProgramLLVM,
    renderCheckedProgramNativeLLVM,
  )
import MLF.Frontend.Parse.Program
  ( parseLocatedProgramWithFile,
    renderProgramParseError,
  )
import MLF.Frontend.Program.Check (checkLocatedProgramPackage)
import MLF.Frontend.Program.Package
  ( LocatedProgramPackage,
    PackageId,
    PackageRoot (..),
    PackageSearchPath (..),
    ProgramPackageDiscoveryError (..),
    discoverLocatedProgramPackageFromSearchPath,
    trivialLocatedProgramPackage,
  )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Frontend.Program.Run
  ( programRunOutput,
    runCheckedProgramOutput,
  )
import MLF.Frontend.Program.Types
  ( CheckedProgram,
    ProgramError,
    diagnosticForProgramError,
    renderProgramDiagnostic,
  )
import System.IO.Unsafe (unsafePerformIO)

data CheckedProgramArtifact = CheckedProgramArtifact
  { checkedArtifactChecked :: CheckedProgram,
    checkedArtifactPrepared :: CheckedProgram
  }

type ArtifactResult = Either String CheckedProgramArtifact

data CheckedProgramTestCache = CheckedProgramTestCache
  { checkedProgramArtifacts :: Map.Map String ArtifactResult
  }

emptyCheckedProgramTestCache :: CheckedProgramTestCache
emptyCheckedProgramTestCache =
  CheckedProgramTestCache
    { checkedProgramArtifacts = Map.empty
    }

checkedProgramTestCache :: MVar CheckedProgramTestCache
checkedProgramTestCache =
  unsafePerformIO (newMVar emptyCheckedProgramTestCache)
{-# NOINLINE checkedProgramTestCache #-}

checkedProgramArtifactFromLocatedPackage :: LocatedProgramPackage -> ArtifactResult
checkedProgramArtifactFromLocatedPackage package =
  mkCheckedProgramArtifact
    <$> first renderProgramDiagnostic (checkLocatedProgramPackage package)

checkedProgramArtifactFromPackageRoots :: PackageId -> [FilePath] -> IO ArtifactResult
checkedProgramArtifactFromPackageRoots packageId roots =
  cachedArtifact ("package-roots:" ++ show packageId ++ ":" ++ show roots) $ do
    packageResult <-
      discoverLocatedProgramPackageFromSearchPath
        packageId
        (PackageSearchPath (map PackageRoot roots))
    pure $ do
      package <- first renderProgramPackageDiscoveryError packageResult
      checkedProgramArtifactFromLocatedPackage (withPreludeLocatedPackage package)

checkedProgramArtifactFromFile :: FilePath -> IO ArtifactResult
checkedProgramArtifactFromFile path = do
  source <- readFile path
  checkedProgramArtifactFromSource path source

checkedProgramArtifactFromSource :: FilePath -> String -> IO ArtifactResult
checkedProgramArtifactFromSource path source =
  cachedArtifact ("source:" ++ path ++ ":" ++ source) $
    pure $
      case parseLocatedProgramWithFile path source of
        Left err ->
          Left (renderProgramParseError err)
        Right located ->
          checkedProgramArtifactFromLocatedPackage
            (withPreludeLocatedPackage (trivialLocatedProgramPackage located))

checkedArtifactCheckOutput :: CheckedProgramArtifact -> Either String String
checkedArtifactCheckOutput _artifact =
  Right "OK\n"

checkedArtifactRunOutput :: CheckedProgramArtifact -> Either String String
checkedArtifactRunOutput artifact =
  first renderProgramError $
    programRunOutput <$> runCheckedProgramOutput (checkedArtifactChecked artifact)

checkedArtifactBackendLLVM :: CheckedProgramArtifact -> Either String String
checkedArtifactBackendLLVM artifact =
  first renderBackendLLVMError $
    renderCheckedProgramLLVM (checkedArtifactPrepared artifact)

checkedArtifactNativeLLVM :: CheckedProgramArtifact -> Either String String
checkedArtifactNativeLLVM artifact =
  first renderBackendLLVMError $
    renderCheckedProgramNativeLLVM (checkedArtifactPrepared artifact)

checkProgramFileCached :: FilePath -> IO (Either String String)
checkProgramFileCached path =
  runArtifactCommand checkedArtifactCheckOutput (checkedProgramArtifactFromFile path)

runProgramFileCached :: FilePath -> IO (Either String String)
runProgramFileCached path =
  runArtifactCommand checkedArtifactRunOutput (checkedProgramArtifactFromFile path)

emitBackendFileCached :: FilePath -> IO (Either String String)
emitBackendFileCached path =
  runArtifactCommand checkedArtifactBackendLLVM (checkedProgramArtifactFromFile path)

emitNativeFileCached :: FilePath -> IO (Either String String)
emitNativeFileCached path =
  runArtifactCommand checkedArtifactNativeLLVM (checkedProgramArtifactFromFile path)

emitBackendSourceCached :: FilePath -> String -> IO (Either String String)
emitBackendSourceCached path source =
  runArtifactCommand checkedArtifactBackendLLVM (checkedProgramArtifactFromSource path source)

emitNativeSourceCached :: FilePath -> String -> IO (Either String String)
emitNativeSourceCached path source =
  runArtifactCommand checkedArtifactNativeLLVM (checkedProgramArtifactFromSource path source)

mkCheckedProgramArtifact :: CheckedProgram -> CheckedProgramArtifact
mkCheckedProgramArtifact checked =
  CheckedProgramArtifact
    { checkedArtifactChecked = checked,
      checkedArtifactPrepared = prepareCheckedProgramForBackendEmission checked
    }

renderProgramError :: ProgramError -> String
renderProgramError err =
  renderProgramDiagnostic (diagnosticForProgramError Nothing err)

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

runArtifactCommand ::
  (CheckedProgramArtifact -> Either String String) ->
  IO ArtifactResult ->
  IO (Either String String)
runArtifactCommand command loadArtifact = do
  artifactResult <- loadArtifact
  pure (artifactResult >>= command)

cachedArtifact :: String -> IO ArtifactResult -> IO ArtifactResult
cachedArtifact key build = do
  cache <- readMVar checkedProgramTestCache
  case Map.lookup key (checkedProgramArtifacts cache) of
    Just result ->
      pure result
    Nothing -> do
      result <- build
      modifyMVar_ checkedProgramTestCache $ \cache' ->
        pure
          cache'
            { checkedProgramArtifacts =
                Map.insert key result (checkedProgramArtifacts cache')
            }
      pure result
