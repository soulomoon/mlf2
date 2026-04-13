module MLF.Program.CLI
    ( programCliUsage
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)

import MLF.Frontend.Program.Parse
    ( parseRawProgram
    , renderProgramParseError
    )
import MLF.Frontend.Program.Run
    ( prettyValue
    , runProgram
    )

programCliUsage :: String
programCliUsage =
    unlines
        [ "Usage:"
        , "  mlf2 run-program <file.mlfp>"
        , ""
        , "Runs a recursive-ADT program file through the MLF.Program surface and"
        , "prints the resulting value."
        ]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseRawProgram source)
        first show (prettyValue <$> runProgram program)
