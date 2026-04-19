module MLF.Program.CLI
    ( programCliUsage
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)

import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
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
        , ""
        , "Runs a .mlfp program file through the unified MLF frontend/pipeline path and"
        , "prints the resulting value."
        ]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        first renderProgramDiagnostic (prettyValue <$> runLocatedProgram (withPreludeLocated program))
