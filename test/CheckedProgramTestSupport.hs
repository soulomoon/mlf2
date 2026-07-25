module CheckedProgramTestSupport
  ( CheckedProgramArtifact,
    checkedProgramArtifactFromLocatedPackage,
    checkedProgramArtifactFromFile,
    checkedProgramArtifactFromSource,
    checkedProgramArtifactFromSourceWithCache,
    checkedProgramArtifactFromCheckedWithPreparation,
    checkedArtifactModuleNames,
    checkedArtifactCheckOutput,
    checkedArtifactRunOutput,
    checkedArtifactBackendLLVM,
    checkedArtifactNativeLLVM,
    runCheckedProgramArtifact,
  )
where

import Data.Bifunctor (first)
import MLF.Backend.Convert (convertCheckedProgram)
import MLF.Backend.Emission.Prepare (prepareCheckedProgramForBackendEmission, renderBackendEmissionPreparationError)
import MLF.Backend.IR (ProductionBackendProgram)
import MLF.Backend.LLVM
  ( BackendLLVMError (BackendLLVMConversionFailed),
    renderBackendLLVMError,
    renderBackendProgramLLVM,
    renderBackendProgramNativeLLVM,
  )
import MLF.Frontend.Parse.Program
  ( parseLocatedProgramWithFile,
    renderProgramParseError,
  )
import MLF.Frontend.Program.Check (checkLocatedProgramPackage)
import qualified MLF.Frontend.Program.Check.TestSupport as PreludeCacheTestSupport
import MLF.Frontend.Program.Checked
  ( CheckedProgram,
    checkedProgramModules,
  )
import MLF.Frontend.Program.Package
  ( LocatedProgramPackage,
    trivialLocatedProgramPackage,
  )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackageIfImported)
import MLF.Frontend.Program.Run
  ( programRunOutput,
    runCheckedProgramOutput,
  )
import MLF.Frontend.Program.Types
  ( ProgramDiagnostic,
    ProgramError,
    CheckedModule (checkedModuleName),
    diagnosticForProgramError,
    renderProgramDiagnostic,
  )
import qualified MLF.Frontend.Syntax.Program as P

data CheckedProgramArtifact = CheckedProgramArtifact
  { checkedArtifactChecked :: CheckedProgram,
    checkedArtifactBackendPreparation :: Either String ProductionBackendProgram
  }

type ArtifactResult = Either String CheckedProgramArtifact

checkedProgramArtifactFromLocatedPackage :: LocatedProgramPackage -> ArtifactResult
checkedProgramArtifactFromLocatedPackage package =
  mkCheckedProgramArtifact
    <$> first renderProgramDiagnostic (checkLocatedProgramPackage package)

checkedProgramArtifactFromFile :: FilePath -> IO ArtifactResult
checkedProgramArtifactFromFile path = do
  source <- readFile path
  checkedProgramArtifactFromSource path source

checkedProgramArtifactFromSource :: FilePath -> String -> IO ArtifactResult
checkedProgramArtifactFromSource =
  checkedProgramArtifactFromSourceWith checkLocatedProgramPackage

checkedProgramArtifactFromSourceWithCache ::
  PreludeCacheTestSupport.BuiltinPreludeCheckCacheHandle ->
  FilePath ->
  String ->
  IO ArtifactResult
checkedProgramArtifactFromSourceWithCache cacheHandle path source =
  checkedProgramArtifactFromSourceWith
    (PreludeCacheTestSupport.checkLocatedProgramPackageWithCache cacheHandle)
    path
    source

checkedProgramArtifactFromSourceWith ::
  (LocatedProgramPackage -> Either ProgramDiagnostic CheckedProgram) ->
  FilePath ->
  String ->
  IO ArtifactResult
checkedProgramArtifactFromSourceWith checkPackage path source =
  pure $
    case parseLocatedProgramWithFile path source of
      Left err ->
        Left (renderProgramParseError err)
      Right located ->
        mkCheckedProgramArtifact
          <$> first
            renderProgramDiagnostic
            (checkPackage (runtimeLocatedPackage located))

runtimeLocatedPackage :: P.LocatedProgram -> LocatedProgramPackage
runtimeLocatedPackage =
  withPreludeLocatedPackageIfImported . trivialLocatedProgramPackage

checkedArtifactModuleNames :: CheckedProgramArtifact -> [P.ModuleName]
checkedArtifactModuleNames =
  map checkedModuleName
    . checkedProgramModules
    . checkedArtifactChecked

checkedArtifactCheckOutput :: CheckedProgramArtifact -> Either String String
checkedArtifactCheckOutput _artifact =
  Right "OK\n"

checkedArtifactRunOutput :: CheckedProgramArtifact -> Either String String
checkedArtifactRunOutput artifact =
  first renderProgramError $
    programRunOutput <$> runCheckedProgramOutput (checkedArtifactChecked artifact)

checkedArtifactBackendLLVM :: CheckedProgramArtifact -> Either String String
checkedArtifactBackendLLVM artifact = do
  prepared <- checkedArtifactBackendPreparation artifact
  first renderBackendLLVMError $
    renderBackendProgramLLVM prepared

checkedArtifactNativeLLVM :: CheckedProgramArtifact -> Either String String
checkedArtifactNativeLLVM artifact = do
  prepared <- checkedArtifactBackendPreparation artifact
  first renderBackendLLVMError $
    renderBackendProgramNativeLLVM prepared

mkCheckedProgramArtifact :: CheckedProgram -> CheckedProgramArtifact
mkCheckedProgramArtifact checked =
  checkedProgramArtifactFromCheckedWithPreparation
    (first renderBackendEmissionPreparationError . prepareCheckedProgramForBackendEmission)
    checked

-- | Test-only construction seam for checking the artifact's forcing boundary.
-- Backend preparation and conversion are stored as one shared lazy field:
-- interpreter access observes only the checked program, while both backend
-- renderers force the same production backend program on demand.
checkedProgramArtifactFromCheckedWithPreparation ::
  (CheckedProgram -> Either String CheckedProgram) ->
  CheckedProgram ->
  CheckedProgramArtifact
checkedProgramArtifactFromCheckedWithPreparation prepare checked =
  CheckedProgramArtifact
    { checkedArtifactChecked = checked,
      checkedArtifactBackendPreparation =
        prepare checked
          >>= first
            (renderBackendLLVMError . BackendLLVMConversionFailed)
            . convertCheckedProgram
    }

renderProgramError :: ProgramError -> String
renderProgramError err =
  renderProgramDiagnostic (diagnosticForProgramError Nothing err)

runCheckedProgramArtifact ::
  (CheckedProgramArtifact -> Either String output) ->
  IO ArtifactResult ->
  IO (Either String output)
runCheckedProgramArtifact command loadArtifact = do
  artifactResult <- loadArtifact
  pure (artifactResult >>= command)
