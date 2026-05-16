module MLF.Program.CLI
    ( programCliUsage
    , emitBackendFile
    , emitNativeFile
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)

import MLF.Backend.Emission.Prepare
    ( prepareBackendEmissionFromSource
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
import MLF.Frontend.Program.Run
    ( programRunOutput
    , runLocatedProgramOutput
    )
import MLF.Frontend.Program.Prelude (withPreludeLocated)
import MLF.Frontend.Program.Types (renderProgramDiagnostic)

programCliUsage :: String
programCliUsage =
    unlines
        [ "Usage:"
        , "  mlf2 run-program <file.mlfp>"
        , "  mlf2 emit-backend <file.mlfp>"
        , "  mlf2 emit-native <file.mlfp>"
        , ""
        , "run-program prepends the built-in Prelude and prints the pure result or IO stdout."
        , "emit-backend prepends the built-in Prelude and prints LLVM IR."
        , "emit-native prepends the built-in Prelude and prints LLVM IR with a native process entrypoint."
        ]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        first renderProgramDiagnostic (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated program))

emitBackendFile :: FilePath -> IO (Either String String)
emitBackendFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        checked <-
            first
                renderBackendEmissionPreparationError
                (prepareBackendEmissionFromSource path source)
        first renderBackendLLVMError (renderCheckedProgramLLVM checked)

emitNativeFile :: FilePath -> IO (Either String String)
emitNativeFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        checked <-
            first
                renderBackendEmissionPreparationError
                (prepareBackendEmissionFromSource path source)
        first renderBackendLLVMError (renderCheckedProgramNativeLLVM checked)
