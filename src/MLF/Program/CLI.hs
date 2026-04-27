module MLF.Program.CLI
    ( programCliUsage
    , emitBackendFile
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)

import MLF.Backend.Text
    ( renderBackendTextError
    , renderCheckedProgramBackendText
    )
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check (checkLocatedProgram)
import MLF.Frontend.Program.Run
    ( prettyValue
    , runLocatedProgram
    )
import MLF.Frontend.Program.Prelude (withPreludeLocated)
import MLF.Frontend.Program.Types (renderProgramDiagnostic)

programCliUsage :: String
programCliUsage =
    unlines
        [ "Usage:"
        , "  mlf2 run-program <file.mlfp>"
        , "  mlf2 emit-backend <file.mlfp>"
        , ""
        , "run-program prepends the built-in Prelude and prints the resulting value."
        , "emit-backend checks the file as provided and prints first LLVM-like backend text."
        ]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        first renderProgramDiagnostic (prettyValue <$> runLocatedProgram (withPreludeLocated program))

emitBackendFile :: FilePath -> IO (Either String String)
emitBackendFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        checked <- first renderProgramDiagnostic (checkLocatedProgram program)
        first renderBackendTextError (renderCheckedProgramBackendText checked)
