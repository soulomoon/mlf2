module MLF.Research.URI.R2.C1.Prototype.Entrypoint (
    AppRun(..),
    runApp,
    runAppFromCurrentDirectory,
    runResearchPrototype
) where

import System.Directory (getCurrentDirectory)

import MLF.Elab.Pipeline
    ( Pretty(..)
    , defaultPipelineConfig
    , renderPipelineError
    , runPipelineElabWithConfig
    )
import MLF.Frontend.Normalize (normalizeExpr)
import MLF.Frontend.Syntax (Expr(..))

import MLF.Research.URI.R2.C1.Prototype.Artifact (writeP1Artifact)
import MLF.Research.URI.R2.C1.Prototype.P1 (executeP1)
import MLF.Research.URI.R2.C1.Prototype.Types

data AppRun
    = AppDefaultDemo String
    | AppPrototype PrototypeReport
    deriving (Eq, Show)

runAppFromCurrentDirectory :: [String] -> IO (Either String AppRun)
runAppFromCurrentDirectory args = do
    repoRoot <- getCurrentDirectory
    runApp repoRoot args

runApp :: FilePath -> [String] -> IO (Either String AppRun)
runApp _ [] = do
    output <- runDefaultDemo
    pure (Right (AppDefaultDemo output))
runApp repoRoot args =
    case parsePrototypeRequest repoRoot args of
        Left err -> pure (Left err)
        Right req -> do
            result <- runResearchPrototype req
            pure (either (Left . show) (Right . AppPrototype) result)

runResearchPrototype :: PrototypeRequest -> IO (Either PrototypeError PrototypeReport)
runResearchPrototype req
    | prResearchEntrypointId req /= researchEntrypointId =
        pure (Left (UnsupportedResearchEntrypoint (prResearchEntrypointId req)))
    | prStageSelector req /= stageSelectorP1 =
        pure (Left (UnsupportedStageSelector (prStageSelector req)))
    | prScenarioId req /= scenarioIdUriR2C1OnlyV1 =
        pure (Left (UnsupportedScenario (prScenarioId req)))
    | prAttemptId req < 1 || prAttemptId req > 3 =
        pure (Left (UnsupportedAttemptId (prAttemptId req)))
    | otherwise = do
        report <- executeP1 req
        _ <- writeP1Artifact report
        pure (Right report)

parsePrototypeRequest :: FilePath -> [String] -> Either String PrototypeRequest
parsePrototypeRequest repoRoot args = do
    entrypoint <- lookupFlag "--research-entrypoint"
    stageSelector <- lookupFlag "--stage-selector"
    scenarioId <- lookupFlag "--scenario-id"
    attemptIdText <- lookupFlag "--attempt-id"
    attemptId <- parseAttemptId attemptIdText
    pure
        PrototypeRequest
            { prRepoRoot = repoRoot
            , prResearchEntrypointId = entrypoint
            , prStageSelector = stageSelector
            , prScenarioId = scenarioId
            , prAttemptId = attemptId
            }
  where
    parsedFlags = parsePairs args

    lookupFlag :: String -> Either String String
    lookupFlag key =
        case lookup key parsedFlags of
            Nothing -> Left ("Missing required prototype flag: " ++ key)
            Just value -> Right value

parsePairs :: [String] -> [(String, String)]
parsePairs [] = []
parsePairs (key : value : rest) = (key, value) : parsePairs rest
parsePairs [_] = []

parseAttemptId :: String -> Either String Int
parseAttemptId raw =
    case reads raw of
        [(attemptId, "")] -> Right attemptId
        _ -> Left ("Invalid attempt id: " ++ raw)

runDefaultDemo :: IO String
runDefaultDemo = do
    let expr = ELam "x" (EVar "x")
    case normalizeExpr expr of
        Left err ->
            pure ("Normalization failed: " ++ show err)
        Right normExpr ->
            case runPipelineElabWithConfig defaultPipelineConfig mempty normExpr of
                Left err ->
                    pure ("Pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) ->
                    pure ("Type: " ++ pretty ty)
