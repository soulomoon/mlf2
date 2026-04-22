{-# LANGUAGE OverloadedStrings #-}

module ProgramLspSpec (spec) where

import Data.Aeson (Value (..), object, parseJSON, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import MLF.Program.LSP
  ( LspOutput (..),
    diagnosticsForText,
    handleLspMessage,
    initialLspState,
    lspContentLengthFrame,
  )
import Test.Hspec
import Text.Read (readMaybe)

spec :: Spec
spec = describe "MLF.Program.LSP" $ do
  it "turns located .mlfp checker failures into LSP diagnostics" $ do
    let source =
          unlines
            [ "module Main export (main) {",
              "  import Missing;",
              "  def main : Bool = true;",
              "}"
            ]
        diagnostics = diagnosticsForText "/tmp/missing-import.mlfp" source
    diagnostics `shouldSatisfy` ((== 1) . length)
    diagnostic <- expectOne "diagnostic" diagnostics
    stringAt ["message"] diagnostic `shouldSatisfy` maybe False (T.isInfixOf "unknown imported module `Missing`")
    intAt ["range", "start", "line"] diagnostic `shouldBe` Just 1
    intAt ["range", "start", "character"] diagnostic `shouldBe` Just 9

  it "turns parse failures into LSP diagnostics with a source range" $ do
    let diagnostics = diagnosticsForText "/tmp/bad.mlfp" "not a program"
    diagnostics `shouldSatisfy` ((== 1) . length)
    diagnostic <- expectOne "diagnostic" diagnostics
    intAt ["range", "start", "line"] diagnostic `shouldBe` Just 0
    stringAt ["message"] diagnostic `shouldSatisfy` maybe False (T.isInfixOf "/tmp/bad.mlfp:1:1")

  it "handles initialize and full text-document synchronization" $ do
    let initialize =
          object
            [ "jsonrpc" .= ("2.0" :: Text),
              "id" .= (1 :: Int),
              "method" .= ("initialize" :: Text),
              "params" .= object []
            ]
        (state0, initializeOutputs) = handleLspMessage initialLspState initialize
    initializeOutputs `shouldSatisfy` ((== 1) . length)
    initializeOutput <- expectOne "initialize output" initializeOutputs
    intAt ["result", "capabilities", "textDocumentSync", "change"] (outputValue initializeOutput)
      `shouldBe` Just 1

    let uri = "file:///tmp/example.mlfp" :: Text
        badSource =
          unlines
            [ "module Main export (main) {",
              "  import Missing;",
              "  def main : Bool = true;",
              "}"
            ]
        didOpen =
          object
            [ "jsonrpc" .= ("2.0" :: Text),
              "method" .= ("textDocument/didOpen" :: Text),
              "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uri,
                          "languageId" .= ("mlfp" :: Text),
                          "version" .= (1 :: Int),
                          "text" .= badSource
                        ]
                  ]
            ]
        (state1, openOutputs) = handleLspMessage state0 didOpen
    openOutputs `shouldSatisfy` ((== 1) . length)
    openOutput <- expectOne "open output" openOutputs
    diagnosticsLength (outputValue openOutput) `shouldBe` Just 1

    let goodSource =
          unlines
            [ "module Main export (main) {",
              "  def main : Bool = true;",
              "}"
            ]
        didChange =
          object
            [ "jsonrpc" .= ("2.0" :: Text),
              "method" .= ("textDocument/didChange" :: Text),
              "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uri,
                          "version" .= (2 :: Int)
                        ],
                    "contentChanges" .= [object ["text" .= goodSource]]
                  ]
            ]
        (_state2, changeOutputs) = handleLspMessage state1 didChange
    changeOutputs `shouldSatisfy` ((== 1) . length)
    changeOutput <- expectOne "change output" changeOutputs
    diagnosticsLength (outputValue changeOutput) `shouldBe` Just 0

  it "frames JSON-RPC payloads with Content-Length" $ do
    let payload =
          object
            [ "jsonrpc" .= ("2.0" :: Text),
              "id" .= (1 :: Int),
              "result" .= Null
            ]
        frame = BL8.unpack (lspContentLengthFrame payload)
        (header, rest) = breakOn "\r\n\r\n" frame
        body = drop 4 rest
    header `shouldSatisfy` isInfixOf "Content-Length: "
    parsedContentLength header `shouldBe` Just (length body)
    body `shouldSatisfy` isInfixOf "\"jsonrpc\":\"2.0\""

outputValue :: LspOutput -> Value
outputValue (LspOutput value) = value

expectOne :: String -> [a] -> IO a
expectOne label values =
  case values of
    [value] -> pure value
    _ -> expectationFailure ("expected one " ++ label ++ ", got " ++ show (length values)) >> fail label

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] value = Just value
lookupPath (key : rest) (Object object0) =
  KeyMap.lookup (Key.fromText key) object0 >>= lookupPath rest
lookupPath _ _ = Nothing

stringAt :: [Text] -> Value -> Maybe Text
stringAt path value =
  case lookupPath path value of
    Just (String text) -> Just text
    _ -> Nothing

intAt :: [Text] -> Value -> Maybe Int
intAt path value =
  lookupPath path value >>= parseMaybe parseJSON

diagnosticsLength :: Value -> Maybe Int
diagnosticsLength value =
  case lookupPath ["params", "diagnostics"] value of
    Just (Array diagnostics) -> Just (length diagnostics)
    _ -> Nothing

breakOn :: String -> String -> (String, String)
breakOn needle haystack = go "" haystack
  where
    go prefix rest
      | needle `isPrefixOf` rest = (reverse prefix, rest)
      | otherwise =
          case rest of
            [] -> (reverse prefix, [])
            c : cs -> go (c : prefix) cs

parsedContentLength :: String -> Maybe Int
parsedContentLength header =
  readMaybe . dropWhile (== ' ') =<< stripPrefix "Content-Length:" header
