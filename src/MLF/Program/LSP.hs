{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MLF.Program.LSP
  ( LspState,
    LspOutput (..),
    initialLspState,
    handleLspMessage,
    diagnosticsForText,
    lspContentLengthFrame,
    runLanguageServer,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Aeson
  ( Value (..),
    decodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isSpace, toLower)
import Data.List (find, isSuffixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import MLF.Frontend.Parse.Program
  ( ProgramParseError,
    parseLocatedProgramWithFile,
    renderProgramParseError,
  )
import MLF.Frontend.Program.Check (checkLocatedProgram)
import MLF.Frontend.Program.Prelude (withPreludeLocated)
import MLF.Frontend.Program.Types (ProgramDiagnostic (..))
import qualified MLF.Frontend.Syntax.Program as P
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    hFlush,
    hIsEOF,
    hSetBinaryMode,
    hSetBuffering,
    stdin,
    stdout,
  )
import Text.Read (readMaybe)

data LspDocument = LspDocument
  { documentUri :: Text,
    documentLanguageId :: Maybe Text,
    documentVersion :: Maybe Int,
    documentText :: Text
  }
  deriving (Eq, Show)

data LspState = LspState
  { lspDocuments :: Map.Map Text LspDocument,
    lspShutdownRequested :: Bool,
    lspExitCode :: Maybe ExitCode
  }
  deriving (Eq, Show)

newtype LspOutput = LspOutput
  { lspOutputValue :: Value
  }
  deriving (Eq, Show)

initialLspState :: LspState
initialLspState =
  LspState
    { lspDocuments = Map.empty,
      lspShutdownRequested = False,
      lspExitCode = Nothing
    }

handleLspMessage :: LspState -> Value -> (LspState, [LspOutput])
handleLspMessage state value =
  case parseMaybe parseIncomingMessage value of
    Just (IncomingRequest requestId method params) ->
      handleRequest state requestId method params
    Just (IncomingNotification method params) ->
      handleNotification state method params
    Nothing ->
      (state, [])

data IncomingMessage
  = IncomingRequest Value Text Value
  | IncomingNotification Text Value
  deriving (Eq, Show)

parseIncomingMessage :: Value -> Parser IncomingMessage
parseIncomingMessage =
  withObject "JSON-RPC message" $ \obj -> do
    method <- obj .: "method"
    params <- obj .:? "params"
    mbId <- obj .:? "id"
    case mbId of
      Just requestId -> pure (IncomingRequest requestId method (fromMaybe (Object mempty) params))
      Nothing -> pure (IncomingNotification method (fromMaybe (Object mempty) params))

handleRequest :: LspState -> Value -> Text -> Value -> (LspState, [LspOutput])
handleRequest state requestId method params =
  case method of
    "initialize" ->
      (state, [response requestId initializeResult])
    "shutdown" ->
      (state {lspShutdownRequested = True}, [response requestId Null])
    "textDocument/diagnostic" ->
      handleDocumentDiagnosticRequest state requestId params
    _ ->
      (state, [errorResponse requestId (-32601) ("method not found: " <> method)])

handleDocumentDiagnosticRequest :: LspState -> Value -> Value -> (LspState, [LspOutput])
handleDocumentDiagnosticRequest state requestId params =
  case parseMaybe parseTextDocumentIdentifierParams params of
    Just uri ->
      let diagnostics =
            case Map.lookup uri (lspDocuments state) of
              Just document -> diagnosticsForDocument document
              Nothing -> []
          result =
            object
              [ "kind" .= ("full" :: Text),
                "items" .= diagnostics
              ]
       in (state, [response requestId result])
    Nothing ->
      (state, [errorResponse requestId (-32602) "invalid textDocument/diagnostic params"])

handleNotification :: LspState -> Text -> Value -> (LspState, [LspOutput])
handleNotification state method params =
  case method of
    "initialized" ->
      (state, [])
    "exit" ->
      ( state
          { lspExitCode =
              Just $
                if lspShutdownRequested state
                  then ExitSuccess
                  else ExitFailure 1
          },
        []
      )
    "textDocument/didOpen" ->
      case parseMaybe parseDidOpen params of
        Just document ->
          let state' = state {lspDocuments = Map.insert (documentUri document) document (lspDocuments state)}
           in (state', [publishDiagnostics document])
        Nothing -> (state, [])
    "textDocument/didChange" ->
      case parseMaybe parseDidChange params of
        Just (uri, mbVersion, changes) ->
          let mbExisting = Map.lookup uri (lspDocuments state)
              nextText = lastText changes <|> fmap documentText mbExisting
              mbDocument =
                ( \text ->
                    LspDocument
                      { documentUri = uri,
                        documentLanguageId = mbExisting >>= documentLanguageId,
                        documentVersion = mbVersion <|> (mbExisting >>= documentVersion),
                        documentText = text
                      }
                )
                  <$> nextText
           in case mbDocument of
                Just document ->
                  let state' = state {lspDocuments = Map.insert uri document (lspDocuments state)}
                   in (state', [publishDiagnostics document])
                Nothing -> (state, [])
        Nothing -> (state, [])
    "textDocument/didClose" ->
      case parseMaybe parseTextDocumentIdentifierParams params of
        Just uri ->
          let state' = state {lspDocuments = Map.delete uri (lspDocuments state)}
           in (state', [publishEmptyDiagnostics uri Nothing])
        Nothing -> (state, [])
    "textDocument/didSave" ->
      case parseMaybe parseDidSave params of
        Just (uri, mbText) ->
          case Map.lookup uri (lspDocuments state) of
            Just existing ->
              let document = existing {documentText = fromMaybe (documentText existing) mbText}
                  state' = state {lspDocuments = Map.insert uri document (lspDocuments state)}
               in (state', [publishDiagnostics document])
            Nothing -> (state, [])
        Nothing -> (state, [])
    _ ->
      (state, [])

initializeResult :: Value
initializeResult =
  object
    [ "capabilities"
        .= object
          [ "textDocumentSync"
              .= object
                [ "openClose" .= True,
                  "change" .= (1 :: Int),
                  "save" .= object ["includeText" .= True]
                ]
          ]
    ]

response :: Value -> Value -> LspOutput
response requestId result =
  LspOutput $
    object
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= requestId,
        "result" .= result
      ]

errorResponse :: Value -> Int -> Text -> LspOutput
errorResponse requestId code message =
  LspOutput $
    object
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= requestId,
        "error"
          .= object
            [ "code" .= code,
              "message" .= message
            ]
      ]

notification :: Text -> Value -> LspOutput
notification method params =
  LspOutput $
    object
      [ "jsonrpc" .= ("2.0" :: Text),
        "method" .= method,
        "params" .= params
      ]

parseTextDocumentIdentifierParams :: Value -> Parser Text
parseTextDocumentIdentifierParams =
  withObject "TextDocumentIdentifier params" $ \obj -> do
    textDocument <- obj .: "textDocument"
    withObject "TextDocumentIdentifier" (\td -> td .: "uri") textDocument

parseDidOpen :: Value -> Parser LspDocument
parseDidOpen =
  withObject "DidOpenTextDocumentParams" $ \obj -> do
    textDocument <- obj .: "textDocument"
    withObject
      "TextDocumentItem"
      ( \td -> do
          uri <- td .: "uri"
          languageId <- td .:? "languageId"
          version <- td .:? "version"
          text <- td .: "text"
          pure
            LspDocument
              { documentUri = uri,
                documentLanguageId = languageId,
                documentVersion = version,
                documentText = text
              }
      )
      textDocument

parseDidChange :: Value -> Parser (Text, Maybe Int, [Text])
parseDidChange =
  withObject "DidChangeTextDocumentParams" $ \obj -> do
    textDocument <- obj .: "textDocument"
    (uri, version) <-
      withObject
        "VersionedTextDocumentIdentifier"
        ( \td -> do
            uri <- td .: "uri"
            version <- td .:? "version"
            pure (uri, version)
        )
        textDocument
    contentChanges <- obj .: "contentChanges"
    texts <- mapM parseTextChange contentChanges
    pure (uri, version, texts)

parseTextChange :: Value -> Parser Text
parseTextChange =
  withObject "TextDocumentContentChangeEvent" $ \obj ->
    obj .: "text"

parseDidSave :: Value -> Parser (Text, Maybe Text)
parseDidSave =
  withObject "DidSaveTextDocumentParams" $ \obj -> do
    uri <- parseTextDocumentIdentifierParams (Object obj)
    text <- obj .:? "text"
    pure (uri, text)

lastText :: [Text] -> Maybe Text
lastText [] = Nothing
lastText texts = Just (last texts)

publishDiagnostics :: LspDocument -> LspOutput
publishDiagnostics document =
  notification
    "textDocument/publishDiagnostics"
    ( object
        [ "uri" .= documentUri document,
          "version" .= documentVersion document,
          "diagnostics" .= diagnosticsForDocument document
        ]
    )

publishEmptyDiagnostics :: Text -> Maybe Int -> LspOutput
publishEmptyDiagnostics uri version =
  notification
    "textDocument/publishDiagnostics"
    ( object
        [ "uri" .= uri,
          "version" .= version,
          "diagnostics" .= ([] :: [Value])
        ]
    )

diagnosticsForDocument :: LspDocument -> [Value]
diagnosticsForDocument document
  | isMlfpDocument document =
      diagnosticsForText
        (documentFilePath (documentUri document))
        (T.unpack (documentText document))
  | otherwise = []

isMlfpDocument :: LspDocument -> Bool
isMlfpDocument document =
  maybe False ((== "mlfp") . T.toLower) (documentLanguageId document)
    || ".mlfp" `isSuffixOf` documentFilePath (documentUri document)

documentFilePath :: Text -> FilePath
documentFilePath uri =
  case T.stripPrefix "file://" uri of
    Just path -> T.unpack (percentDecodeText path)
    Nothing -> T.unpack uri

percentDecodeText :: Text -> Text
percentDecodeText text =
  case TE.decodeUtf8' (BS.pack (go (T.unpack text))) of
    Right decoded -> decoded
    Left _ -> text
  where
    go :: String -> [Word8]
    go [] = []
    go ('%' : a : b : rest)
      | Just byte <- hexByte a b = byte : go rest
    go (c : rest) = fromIntegral (fromEnum c) : go rest

hexByte :: Char -> Char -> Maybe Word8
hexByte high low = do
  hi <- hexDigit high
  lo <- hexDigit low
  pure (fromIntegral (hi * 16 + lo))

hexDigit :: Char -> Maybe Int
hexDigit c
  | '0' <= c && c <= '9' = Just (fromEnum c - fromEnum '0')
  | 'a' <= lower && lower <= 'f' = Just (10 + fromEnum lower - fromEnum 'a')
  | otherwise = Nothing
  where
    lower = toLower c

diagnosticsForText :: FilePath -> String -> [Value]
diagnosticsForText path source =
  case parseLocatedProgramWithFile path source of
    Left err -> [parseErrorDiagnostic err]
    Right located ->
      case checkLocatedProgram (withPreludeLocated located) of
        Left diagnostic -> [programDiagnosticToJson diagnostic]
        Right _ -> []

parseErrorDiagnostic :: ProgramParseError -> Value
parseErrorDiagnostic err =
  let rendered = trimEnd (renderProgramParseError err)
      range = case parseErrorLineColumn rendered of
        Just (line, column) -> lspRange line column line (column + 1)
        Nothing -> lspRange 1 1 1 2
   in diagnosticJson range rendered [] Nothing

parseErrorLineColumn :: String -> Maybe (Int, Int)
parseErrorLineColumn rendered =
  case reverse (filter (not . null) (splitOnColon (takeWhile (/= '\n') rendered))) of
    columnText : lineText : _
      | all isDigit columnText,
        all isDigit lineText ->
          (,) <$> readMaybe lineText <*> readMaybe columnText
    _ -> Nothing

splitOnColon :: String -> [String]
splitOnColon = go []
  where
    go current [] = [reverse current]
    go current (':' : rest) = reverse current : go [] rest
    go current (c : rest) = go (c : current) rest

programDiagnosticToJson :: ProgramDiagnostic -> Value
programDiagnosticToJson diagnostic =
  diagnosticJson
    (maybe (lspRange 1 1 1 2) sourceSpanRange (diagnosticSpan diagnostic))
    (diagnosticMessage diagnostic)
    (diagnosticHints diagnostic)
    (Just (show (diagnosticError diagnostic)))

diagnosticJson :: Value -> String -> [String] -> Maybe String -> Value
diagnosticJson range message hints mbCode =
  object
    [ "range" .= range,
      "severity" .= (1 :: Int),
      "source" .= ("mlf2" :: Text),
      "message" .= diagnosticMessageText message hints,
      "code" .= mbCode
    ]

diagnosticMessageText :: String -> [String] -> Text
diagnosticMessageText message hints =
  T.pack $
    trimEnd $
      unlines $
        message : map ("hint: " ++) hints

sourceSpanRange :: P.SourceSpan -> Value
sourceSpanRange span0 =
  lspRange
    (P.sourceLine (P.sourceStart span0))
    (P.sourceColumn (P.sourceStart span0))
    (P.sourceLine (P.sourceEnd span0))
    (P.sourceColumn (P.sourceEnd span0))

lspRange :: Int -> Int -> Int -> Int -> Value
lspRange startLine startColumn endLine endColumn =
  object
    [ "start" .= lspPosition startLine startColumn,
      "end" .= lspPosition endLine endColumn
    ]

lspPosition :: Int -> Int -> Value
lspPosition line column =
  object
    [ "line" .= max 0 (line - 1),
      "character" .= max 0 (column - 1)
    ]

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

lspContentLengthFrame :: Value -> BL.ByteString
lspContentLengthFrame value =
  let payload = encode value
      header = "Content-Length: " ++ show (BL.length payload) ++ "\r\n\r\n"
   in BL.fromStrict (BSC.pack header) <> payload

runLanguageServer :: IO ()
runLanguageServer = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  loop initialLspState
  where
    loop state = do
      mbMessage <- readLspMessage stdin
      case mbMessage of
        Nothing -> pure ()
        Just message -> do
          let (state', outputs) = handleLspMessage state message
          mapM_ (writeLspOutput stdout) outputs
          case lspExitCode state' of
            Just code -> exitWith code
            Nothing -> loop state'

writeLspOutput :: Handle -> LspOutput -> IO ()
writeLspOutput handle (LspOutput value) = do
  BL.hPut handle (lspContentLengthFrame value)
  hFlush handle

readLspMessage :: Handle -> IO (Maybe Value)
readLspMessage handle = do
  mbHeaders <- readHeaders handle
  case mbHeaders of
    Nothing -> pure Nothing
    Just headers ->
      case contentLength headers of
        Nothing -> pure Nothing
        Just len -> do
          payload <- BS.hGet handle len
          pure (decodeStrict' payload)

readHeaders :: Handle -> IO (Maybe [String])
readHeaders handle = do
  eof <- hIsEOF handle
  if eof
    then pure Nothing
    else go []
  where
    go acc = do
      lineResult <- try (BSC.hGetLine handle) :: IO (Either IOException BSC.ByteString)
      case lineResult of
        Left _ -> pure Nothing
        Right rawLine ->
          let line = stripTrailingCr (BSC.unpack rawLine)
           in if null line
                then pure (Just (reverse acc))
                else go (line : acc)

stripTrailingCr :: String -> String
stripTrailingCr line =
  case reverse line of
    '\r' : rest -> reverse rest
    _ -> line

contentLength :: [String] -> Maybe Int
contentLength headers =
  find (> 0) (mapMaybe parseContentLength headers)

parseContentLength :: String -> Maybe Int
parseContentLength header = do
  value <- stripPrefix "content-length:" (map toLower header)
  readMaybe (dropWhile isSpace value)
