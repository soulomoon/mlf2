{-# LANGUAGE GADTs #-}

module ProgramParserParitySpec (spec) where

import Data.List (isInfixOf)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import MLF.API
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Syntax (Lit (..), SrcTy (..), SrcType)
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Program.CLI (runProgramArgs)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
    describe "MLF.Program parser parity" $ do
        it "parser-owned .mlfp parser matches canonical parser for a basic Bool definition and source spans" $ do
            source <- readFile canonicalSourcePath
            expected <- readFile expectedProjectionPath

            canonicalProjection <- renderCanonicalProjection canonicalSourcePath source
            parserParityOutput <- runSharedParserFixture parserParityPackageRoot

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser matches canonical parser for a single import declaration and source spans" $ do
            source <- readFile importCanonicalSourcePath
            expected <- readFile importExpectedProjectionPath

            canonicalProjection <- renderCanonicalProjection importCanonicalSourcePath source
            parserParityOutput <- runSharedParserFixture importParserParityPackageRoot

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint" $ do
            sharedParserExists <- doesFileExist (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedParserExists `shouldBe` True

            basicExpected <- readFile expectedProjectionPath
            importExpected <- readFile importExpectedProjectionPath

            runSharedParserFixture parserParityPackageRoot
                `shouldReturn` Right basicExpected
            runSharedParserFixture importParserParityPackageRoot
                `shouldReturn` Right importExpected

        it "shared parser-owned .mlfp parser composes grammar without fixture-level token streams" $ do
            sharedParserSource <- concat <$> traverse readFile sharedParserAuditFiles
            let bannedMatches =
                    filter (`isInfixOf` sharedParserSource) sharedParserBannedPhrases
            bannedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser consumes tokens through parser-state grammar combinators" $ do
            sharedParserSource <- concat <$> traverse readFile sharedParserAuditFiles
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")

            let fixedOffsetMatches =
                    filter (`isInfixOf` sharedParserSource) sharedParserFixedOffsetPhrases
            fixedOffsetMatches `shouldBe` []

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedCombinatorSource)) sharedParserRequiredCombinators

        it "shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedLexerSource <- readFile (sharedParserLibraryRoot </> "ParserParityLexer.mlfp")

            let earlySuccessMatches =
                    filter (`isInfixOf` sharedParserSource) sharedParserEarlySuccessPhrases
                staticDiagnosticMatches =
                    filter (`isInfixOf` sharedParserSource) sharedParserStaticNegativeEvidencePhrases

            earlySuccessMatches `shouldBe` []
            staticDiagnosticMatches `shouldBe` []
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserCompleteParseRequiredPhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserDynamicEvidenceRequiredPhrases
            sharedLexerSource `shouldSatisfy` isInfixOf "def tokenizeCompleteModule : ParserSourceInput -> LexerResult"
            sharedLexerSource `shouldSatisfy` isInfixOf "validateSourceInput input"

        it "parser-owned .mlfp parser matches canonical parser for multiple value definitions and value-reference spans" $ do
            source <- readFile valueDefListCanonicalSourcePath
            expected <- readFile valueDefListExpectedProjectionPath

            canonicalProjection <- renderCanonicalProjection valueDefListCanonicalSourcePath source
            parserParityOutput <- runSharedParserFixture valueDefListParserParityPackageRoot

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser matches canonical parser for let, lambda, and application expressions" $ do
            source <- readFile letLambdaApplicationCanonicalSourcePath
            expected <- readFile letLambdaApplicationExpectedProjectionPath

            canonicalProjection <- renderCanonicalProjection letLambdaApplicationCanonicalSourcePath source
            parserParityOutput <- runSharedParserFixture letLambdaApplicationParserParityPackageRoot

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser matches canonical parser for typed let, annotated lambda, and expression annotations" $ do
            source <- readFile typedAnnotationTypesCanonicalSourcePath
            expected <- readFile typedAnnotationTypesExpectedProjectionPath

            canonicalProjection <- renderCanonicalProjection typedAnnotationTypesCanonicalSourcePath source
            parserParityOutput <- runSharedParserFixture typedAnnotationTypesParserParityPackageRoot

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser matches canonical parser for data declarations and constructor spans" $ do
            source <- readFile dataDeclarationConstructorSpansCanonicalSourcePath
            expected <- readFile dataDeclarationConstructorSpansExpectedProjectionPath

            canonicalProjection <- renderCanonicalProjection dataDeclarationConstructorSpansCanonicalSourcePath source
            parserParityOutput <- runSharedParserFixture dataDeclarationConstructorSpansParserParityPackageRoot

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser rejects malformed annotation syntax through public run-program" $ do
            evidenceRoot <- writeTypedAnnotationTypesNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right typedAnnotationTypesNegativeEvidenceProjection

        it "parser-owned .mlfp parser rejects malformed data declarations through public run-program" $ do
            evidenceRoot <- writeDataDeclarationConstructorSpansNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right dataDeclarationConstructorSpansNegativeEvidenceProjection

        it "parser-owned .mlfp parser rejects malformed import syntax through public run-program" $ do
            evidenceRoot <- writeImportNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right importNegativeEvidenceProjection

        it "parser-owned .mlfp parser rejects malformed value-definition sequencing through public run-program" $ do
            evidenceRoot <- writeValueDefListNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right valueDefListNegativeEvidenceProjection

        it "parser-owned .mlfp parser rejects malformed let expressions through public run-program" $ do
            evidenceRoot <- writeLetLambdaApplicationNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right letLambdaApplicationNegativeEvidenceProjection

        it "parser-owned .mlfp tokenizer and parser reject discrete token mismatches" $ do
            evidenceRoot <- writeRetryEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right retryEvidenceProjection

canonicalSourcePath :: FilePath
canonicalSourcePath =
    "test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp"

importCanonicalSourcePath :: FilePath
importCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/import-exposing-def-bool/src/Main.mlfp"

valueDefListCanonicalSourcePath :: FilePath
valueDefListCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/value-def-list-int-ref/src/Main.mlfp"

letLambdaApplicationCanonicalSourcePath :: FilePath
letLambdaApplicationCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/let-lambda-application/src/Main.mlfp"

typedAnnotationTypesCanonicalSourcePath :: FilePath
typedAnnotationTypesCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/typed-annotation-types/src/Main.mlfp"

dataDeclarationConstructorSpansCanonicalSourcePath :: FilePath
dataDeclarationConstructorSpansCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/src/Main.mlfp"

expectedProjectionPath :: FilePath
expectedProjectionPath =
    "test/conformance/mlfp/parser-parity/basic-module-def-bool/expected/parser-program.txt"

importExpectedProjectionPath :: FilePath
importExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/import-exposing-def-bool/expected/parser-program.txt"

valueDefListExpectedProjectionPath :: FilePath
valueDefListExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/value-def-list-int-ref/expected/parser-program.txt"

letLambdaApplicationExpectedProjectionPath :: FilePath
letLambdaApplicationExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/let-lambda-application/expected/parser-program.txt"

typedAnnotationTypesExpectedProjectionPath :: FilePath
typedAnnotationTypesExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/typed-annotation-types/expected/parser-program.txt"

dataDeclarationConstructorSpansExpectedProjectionPath :: FilePath
dataDeclarationConstructorSpansExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/expected/parser-program.txt"

parserParityPackageRoot :: FilePath
parserParityPackageRoot =
    "test/programs/compiler-parser-parity/basic-module-def-bool"

importParserParityPackageRoot :: FilePath
importParserParityPackageRoot =
    "test/programs/compiler-parser-parity/import-exposing-def-bool"

valueDefListParserParityPackageRoot :: FilePath
valueDefListParserParityPackageRoot =
    "test/programs/compiler-parser-parity/value-def-list-int-ref"

letLambdaApplicationParserParityPackageRoot :: FilePath
letLambdaApplicationParserParityPackageRoot =
    "test/programs/compiler-parser-parity/let-lambda-application"

typedAnnotationTypesParserParityPackageRoot :: FilePath
typedAnnotationTypesParserParityPackageRoot =
    "test/programs/compiler-parser-parity/typed-annotation-types"

dataDeclarationConstructorSpansParserParityPackageRoot :: FilePath
dataDeclarationConstructorSpansParserParityPackageRoot =
    "test/programs/compiler-parser-parity/data-declaration-constructor-spans"

sharedParserLibraryRoot :: FilePath
sharedParserLibraryRoot =
    "test/programs/compiler-parser-parity/parser-library"

sharedParserAuditFiles :: [FilePath]
sharedParserAuditFiles =
    [ sharedParserLibraryRoot </> "ParserParityToken.mlfp"
    , sharedParserLibraryRoot </> "ParserParityLexer.mlfp"
    , sharedParserLibraryRoot </> "ParserParityParser.mlfp"
    ]

sharedParserBannedPhrases :: [String]
sharedParserBannedPhrases =
    [ "BasicModuleTokens"
    , "ImportBoolTokens"
    , "ValueDefListTokens"
    , "LetLambdaApplicationTokens"
    , "TypedAnnotationTypesTokens"
    , "DataDeclarationTokens"
    , "LexerOk basicModuleTokens"
    , "LexerOk importBoolTokens"
    , "LexerOk valueDefListTokens"
    , "LexerOk letLambdaApplicationTokens"
    , "LexerOk typedAnnotationTypesTokens"
    , "LexerOk dataDeclarationTokens"
    , "case tokens"
    ]

sharedParserFixedOffsetPhrases :: [String]
sharedParserFixedOffsetPhrases =
    map sharedParserFixedSourceProbe
        [ 0
        , 7
        , 12
        , 20
        , 30
        , 46
        , 55
        , 60
        , 61
        , 67
        , 80
        , 87
        , 90
        , 94
        ]

sharedParserFixedSourceProbe :: Int -> String
sharedParserFixedSourceProbe offset =
    "stringSlice " <> "source " <> show offset

sharedParserRequiredCombinators :: [String]
sharedParserRequiredCombinators =
    [ "class Functor"
    , "class Applicative"
    , "class Monad"
    , "data Parser a"
    , "parserBind"
    , "parserMap"
    , "parserChoice"
    , "captureSpan"
    , "diagnosticLabel"
    ]

sharedParserEarlySuccessPhrases :: [String]
sharedParserEarlySuccessPhrases =
    [ "ParserTextMatched -> moduleKey \"data-constructor-spans\""
    , "ParserTextMatched -> moduleKey boolKey"
    , "ParserTextMatched -> moduleKey \"value-int-ref\""
    , "ParserTextMatched -> moduleKey \"typed-annotation\""
    , "ParserTextMismatch -> moduleKey \"let-lambda\""
    ]

sharedParserCompleteParseRequiredPhrases :: [String]
sharedParserCompleteParseRequiredPhrases =
    [ "parserStateAtEnd state"
    , "ParserAtEnd ->"
    , "ParserNotAtEnd ->"
    , "completeModuleKey"
    , "parseDataDeclaration"
    , "parseBoolDefinitionEquals"
    , "parseValueTwoDefinition"
    , "parseLetLambdaTail"
    , "parseTypedAnnotationTail"
    ]

sharedParserStaticNegativeEvidencePhrases :: [String]
sharedParserStaticNegativeEvidencePhrases =
    [ "stringAppend \"import parser negative expected-import-semicolon@\""
    , "stringAppend \"value-def-list parser negative expected-def-semicolon@\""
    , "stringAppend \"let-lambda-application parser negative expected-let-in@\""
    , "stringAppend \"typed-annotation-types parser negative expected-let-annotation-type@\""
    , "stringAppend \"data-declaration parser negative expected-constructor-colon@\""
    ]

sharedParserDynamicEvidenceRequiredPhrases :: [String]
sharedParserDynamicEvidenceRequiredPhrases =
    [ "parseCompleteModule basicParserNegativeSourceInput"
    , "parseCompleteModule importParserNegativeSourceInput"
    , "parseCompleteModule valueDefListParserNegativeSourceInput"
    , "parseCompleteModule letLambdaApplicationParserNegativeSourceInput"
    , "parseCompleteModule typedAnnotationTypesParserNegativeSourceInput"
    , "parseCompleteModule dataDeclarationParserNegativeSourceInput"
    , "tokenizeCompleteModule source"
    , "tokenizeCompleteModule lexerMismatchSource"
    , "renderDiagnosticEvidence"
    ]

runSharedParserFixture :: FilePath -> IO (Either String String)
runSharedParserFixture fixtureRoot =
    runProgramArgs [fixtureRoot, "--search-path", sharedParserLibraryRoot]

retryEvidencePackageRoot :: FilePath
retryEvidencePackageRoot =
    "dist-newstyle/parser-parity-basic-module-def-bool-retry-evidence"

importNegativeEvidencePackageRoot :: FilePath
importNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-import-exposing-def-bool-negative-evidence"

valueDefListNegativeEvidencePackageRoot :: FilePath
valueDefListNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-value-def-list-int-ref-negative-evidence"

letLambdaApplicationNegativeEvidencePackageRoot :: FilePath
letLambdaApplicationNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-let-lambda-application-negative-evidence"

typedAnnotationTypesNegativeEvidencePackageRoot :: FilePath
typedAnnotationTypesNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-typed-annotation-types-negative-evidence"

dataDeclarationConstructorSpansNegativeEvidencePackageRoot :: FilePath
dataDeclarationConstructorSpansNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-data-declaration-constructor-spans-negative-evidence"

writeRetryEvidencePackage :: IO FilePath
writeRetryEvidencePackage = do
    removePathForcibly retryEvidencePackageRoot
    createDirectoryIfMissing True retryEvidencePackageRoot
    writeFile (retryEvidencePackageRoot </> "Main.mlfp") retryEvidenceMainSource
    pure retryEvidencePackageRoot

writeImportNegativeEvidencePackage :: IO FilePath
writeImportNegativeEvidencePackage = do
    removePathForcibly importNegativeEvidencePackageRoot
    createDirectoryIfMissing True importNegativeEvidencePackageRoot
    writeFile (importNegativeEvidencePackageRoot </> "Main.mlfp") importNegativeEvidenceMainSource
    pure importNegativeEvidencePackageRoot

writeValueDefListNegativeEvidencePackage :: IO FilePath
writeValueDefListNegativeEvidencePackage = do
    removePathForcibly valueDefListNegativeEvidencePackageRoot
    createDirectoryIfMissing True valueDefListNegativeEvidencePackageRoot
    writeFile (valueDefListNegativeEvidencePackageRoot </> "Main.mlfp") valueDefListNegativeEvidenceMainSource
    pure valueDefListNegativeEvidencePackageRoot

writeLetLambdaApplicationNegativeEvidencePackage :: IO FilePath
writeLetLambdaApplicationNegativeEvidencePackage = do
    removePathForcibly letLambdaApplicationNegativeEvidencePackageRoot
    createDirectoryIfMissing True letLambdaApplicationNegativeEvidencePackageRoot
    writeFile (letLambdaApplicationNegativeEvidencePackageRoot </> "Main.mlfp") letLambdaApplicationNegativeEvidenceMainSource
    pure letLambdaApplicationNegativeEvidencePackageRoot

writeTypedAnnotationTypesNegativeEvidencePackage :: IO FilePath
writeTypedAnnotationTypesNegativeEvidencePackage = do
    removePathForcibly typedAnnotationTypesNegativeEvidencePackageRoot
    createDirectoryIfMissing True typedAnnotationTypesNegativeEvidencePackageRoot
    writeFile (typedAnnotationTypesNegativeEvidencePackageRoot </> "Main.mlfp") typedAnnotationTypesNegativeEvidenceMainSource
    pure typedAnnotationTypesNegativeEvidencePackageRoot

writeDataDeclarationConstructorSpansNegativeEvidencePackage :: IO FilePath
writeDataDeclarationConstructorSpansNegativeEvidencePackage = do
    removePathForcibly dataDeclarationConstructorSpansNegativeEvidencePackageRoot
    createDirectoryIfMissing True dataDeclarationConstructorSpansNegativeEvidencePackageRoot
    writeFile (dataDeclarationConstructorSpansNegativeEvidencePackageRoot </> "Main.mlfp") dataDeclarationConstructorSpansNegativeEvidenceMainSource
    pure dataDeclarationConstructorSpansNegativeEvidencePackageRoot

retryEvidenceMainSource :: String
retryEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (basicLexerMismatchSourceInput, basicPositiveSourceInput, renderParserParityRetryEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp\";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderParserParityRetryEvidence sourceFile basicPositiveSourceInput basicLexerMismatchSourceInput);"
        , "}"
        ]

importNegativeEvidenceMainSource :: String
importNegativeEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderImportParserNegativeEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/import-exposing-def-bool/src/Main.mlfp\";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderImportParserNegativeEvidence sourceFile);"
        , "}"
        ]

valueDefListNegativeEvidenceMainSource :: String
valueDefListNegativeEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderValueDefListParserNegativeEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/value-def-list-int-ref/src/Main.mlfp\";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderValueDefListParserNegativeEvidence sourceFile);"
        , "}"
        ]

letLambdaApplicationNegativeEvidenceMainSource :: String
letLambdaApplicationNegativeEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderLetLambdaApplicationParserNegativeEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/let-lambda-application/src/Main.mlfp\";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderLetLambdaApplicationParserNegativeEvidence sourceFile);"
        , "}"
        ]

typedAnnotationTypesNegativeEvidenceMainSource :: String
typedAnnotationTypesNegativeEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderTypedAnnotationTypesParserNegativeEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/typed-annotation-types/src/Main.mlfp\";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderTypedAnnotationTypesParserNegativeEvidence sourceFile);"
        , "}"
        ]

dataDeclarationConstructorSpansNegativeEvidenceMainSource :: String
dataDeclarationConstructorSpansNegativeEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderDataDeclarationParserNegativeEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/src/Main.mlfp\";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderDataDeclarationParserNegativeEvidence sourceFile);"
        , "}"
        ]

retryEvidenceProjection :: String
retryEvidenceProjection =
    unlines
        [ "tokens module@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:1-1:7 Main@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:8-1:12 export@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:19"
        , "lexer negative unexpected-source@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:19"
        , "parser negative expected-equals@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:2:21-2:25"
        ]

importNegativeEvidenceProjection :: String
importNegativeEvidenceProjection =
    unlines
        [ "import parser negative expected-import-semicolon@test/conformance/mlfp/parser-parity/import-exposing-def-bool/src/Main.mlfp:2:33-2:34"
        ]

valueDefListNegativeEvidenceProjection :: String
valueDefListNegativeEvidenceProjection =
    unlines
        [ "value-def-list parser negative expected-def-semicolon@test/conformance/mlfp/parser-parity/value-def-list-int-ref/src/Main.mlfp:3:20-3:21"
        ]

letLambdaApplicationNegativeEvidenceProjection :: String
letLambdaApplicationNegativeEvidenceProjection =
    unlines
        [ "let-lambda-application parser negative expected-let-in@test/conformance/mlfp/parser-parity/let-lambda-application/src/Main.mlfp:3:34-3:36"
        ]

typedAnnotationTypesNegativeEvidenceProjection :: String
typedAnnotationTypesNegativeEvidenceProjection =
    unlines
        [ "typed-annotation-types parser negative expected-let-annotation-type@test/conformance/mlfp/parser-parity/typed-annotation-types/src/Main.mlfp:3:29-3:30"
        ]

dataDeclarationConstructorSpansNegativeEvidenceProjection :: String
dataDeclarationConstructorSpansNegativeEvidenceProjection =
    unlines
        [ "data-declaration parser negative expected-constructor-colon@test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/src/Main.mlfp:3:12-3:13"
        ]

renderCanonicalProjection :: FilePath -> String -> IO String
renderCanonicalProjection path source =
    case parseLocatedProgramWithFile path source of
        Left err ->
            expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right located ->
            renderLocatedProjection located

renderLocatedProjection :: P.LocatedProgram -> IO String
renderLocatedProjection located =
    case P.locatedProgram located of
        P.Program [module0] -> renderModuleProjection (P.locatedProgramSpans located) module0
        other ->
            expectationFailure ("expected one parsed module, got: " ++ show other)
                >> fail "unexpected parsed program shape"

renderModuleProjection :: P.ProgramSpanIndex -> P.Module -> IO String
renderModuleProjection spans module0 = do
    renderedExports <- renderExportProjections spans (P.moduleExports module0)
    renderedImports <- renderImportProjections spans (P.moduleImports module0)
    renderedDefs <- renderDefProjections spans (P.moduleDecls module0)
    moduleSpan <- requireMapSpan "module" (P.moduleName module0) (P.spanModules spans)

    pure $
        unlines
            ( [ "module " ++ P.moduleName module0 ++ " span=" ++ renderSpan moduleSpan
              ]
                ++ renderedExports
                ++ renderedImports
                ++ renderedDefs
            )

renderExportProjections :: P.ProgramSpanIndex -> Maybe [P.ExportItem] -> IO [String]
renderExportProjections spans exports =
    case exports of
        Nothing -> pure []
        Just items -> traverse (renderExportProjection spans) items

renderExportProjection :: P.ProgramSpanIndex -> P.ExportItem -> IO String
renderExportProjection spans item = do
    let name = P.exportItemName item
    exportSpan <- requireListSpan "export" name (P.spanExportItems spans)
    pure $
        "export "
            ++ renderExportKind item
            ++ " "
            ++ name
            ++ " span="
            ++ renderSpan exportSpan

renderImportProjections :: P.ProgramSpanIndex -> [P.Import] -> IO [String]
renderImportProjections spans imports =
    case imports of
        [] -> pure []
        [import0] -> do
            importSpan <- requireListSpan "import" (P.importModuleName import0) (P.spanImports spans)
            exposingItem <- requireSingleImportExposing (P.importExposing import0)
            exposingSpan <- requireListSpan "import exposing item" (P.exportItemName exposingItem) (P.spanImportItems spans)
            pure
                [ "import "
                    ++ P.importModuleName import0
                    ++ " span="
                    ++ renderSpan importSpan
                , "import exposing "
                    ++ renderExportKind exposingItem
                    ++ " "
                    ++ P.exportItemName exposingItem
                    ++ " span="
                    ++ renderSpan exposingSpan
                ]
        other ->
            expectationFailure ("expected zero or one import declaration, got: " ++ show other)
                >> fail "unexpected import shape"

requireSingleImportExposing :: Maybe [P.ExportItem] -> IO P.ExportItem
requireSingleImportExposing exposing =
    case exposing of
        Just [item] -> pure item
        other ->
            expectationFailure ("expected one import exposing item, got: " ++ show other)
                >> fail "unexpected import exposing shape"

renderExportKind :: P.ExportItem -> String
renderExportKind item =
    case item of
        P.ExportValue _ -> "value"
        P.ExportType _ -> "type"
        P.ExportTypeWithConstructors _ -> "type-with-constructors"

renderDefProjections :: P.ProgramSpanIndex -> [P.Decl] -> IO [String]
renderDefProjections spans decls =
    concat <$> traverse (renderDefProjection spans) decls

renderDefProjection :: P.ProgramSpanIndex -> P.Decl -> IO [String]
renderDefProjection spans decl =
    case decl of
        P.DeclData data0 -> renderDataProjection spans data0
        P.DeclDef def0 -> do
            defSpan <- requireListSpan "definition" (P.defDeclName def0) (P.spanValues spans)
            pure
                [ "def "
                    ++ P.defDeclName def0
                    ++ " type="
                    ++ renderSrcType (P.constrainedBody (P.defDeclType def0))
                    ++ " expr="
                    ++ renderExpr (P.defDeclExpr def0)
                    ++ " span="
                    ++ renderSpan defSpan
                ]
        other ->
            expectationFailure ("expected data or def declaration, got: " ++ show other)
                >> fail "unexpected declaration shape"

renderDataProjection :: P.ProgramSpanIndex -> P.DataDecl -> IO [String]
renderDataProjection spans data0 = do
    dataSpan <- requireListSpan "data declaration" (P.dataDeclName data0) (P.spanTypes spans)
    renderedConstructors <- traverse (renderConstructorProjection spans) (P.dataDeclConstructors data0)
    pure $
        ( "data "
            ++ P.dataDeclName data0
            ++ " span="
            ++ renderSpan dataSpan
        )
            : renderedConstructors

renderConstructorProjection :: P.ProgramSpanIndex -> P.ConstructorDecl -> IO String
renderConstructorProjection spans ctor = do
    ctorSpan <- requireListSpan "constructor" (P.constructorDeclName ctor) (P.spanConstructors spans)
    pure $
        "constructor "
            ++ P.constructorDeclName ctor
            ++ " type="
            ++ renderSrcType (P.constructorDeclType ctor)
            ++ " span="
            ++ renderSpan ctorSpan

requireMapSpan :: String -> String -> Map.Map String P.SourceSpan -> IO P.SourceSpan
requireMapSpan label name spans =
    case Map.lookup name spans of
        Just span0 -> pure span0
        Nothing ->
            expectationFailure ("missing " ++ label ++ " span for " ++ show name)
                >> fail "missing span"

requireListSpan :: String -> String -> Map.Map String [P.SourceSpan] -> IO P.SourceSpan
requireListSpan label name spans =
    case Map.lookup name spans of
        Just [span0] -> pure span0
        Just other ->
            expectationFailure ("expected one " ++ label ++ " span for " ++ show name ++ ", got: " ++ show other)
                >> fail "unexpected span count"
        Nothing ->
            expectationFailure ("missing " ++ label ++ " span for " ++ show name)
                >> fail "missing span"

renderSrcType :: SrcType -> String
renderSrcType ty =
    renderSrcTypePrec 0 ty

renderSrcTypePrec :: Int -> SrcType -> String
renderSrcTypePrec precedence ty =
    case ty of
        STVar name -> name
        STBase name -> name
        STArrow dom cod ->
            parenthesizeIf (precedence > 1) $
                renderSrcTypePrec 2 dom ++ " -> " ++ renderSrcTypePrec 1 cod
        STForall name Nothing body ->
            parenthesizeIf (precedence > 0) $
                "∀" ++ name ++ ". " ++ renderSrcTypePrec 0 body
        other -> show other

renderExpr :: P.Expr -> String
renderExpr expr =
    renderExprPrec 0 expr

renderExprPrec :: Int -> P.Expr -> String
renderExprPrec precedence expr =
    case expr of
        P.EVar name -> name
        P.ELit (LInt value) -> show value
        P.ELit (LBool True) -> "true"
        P.ELit (LBool False) -> "false"
        P.ELam param body ->
            parenthesizeIf (precedence > 0) $
                "λ" ++ renderParam param ++ " " ++ renderExprPrec 0 body
        P.EApp fun arg ->
            parenthesizeIf (precedence > 1) $
                renderExprPrec 1 fun ++ " " ++ renderExprPrec 2 arg
        P.ELet name mbTy rhs body ->
            parenthesizeIf (precedence > 0) $
                "let "
                    ++ name
                    ++ maybe "" ((" : " ++) . renderSrcType) mbTy
                    ++ " = "
                    ++ renderExprPrec 0 rhs
                    ++ " in "
                    ++ renderExprPrec 0 body
        P.EAnn inner ty ->
            "(" ++ renderExprPrec 0 inner ++ " : " ++ renderSrcType ty ++ ")"
        other -> show other

renderParam :: P.Param -> String
renderParam param =
    case P.paramType param of
        Nothing -> P.paramName param
        Just ty -> "(" ++ P.paramName param ++ " : " ++ renderSrcType ty ++ ")"

parenthesizeIf :: Bool -> String -> String
parenthesizeIf shouldParenthesize rendered =
    if shouldParenthesize
        then "(" ++ rendered ++ ")"
        else rendered

renderSpan :: P.SourceSpan -> String
renderSpan span0 =
    P.sourceFile span0
        ++ ":"
        ++ renderPosition (P.sourceStart span0)
        ++ "-"
        ++ renderPosition (P.sourceEnd span0)

renderPosition :: P.SourcePosition -> String
renderPosition position =
    show (P.sourceLine position) ++ ":" ++ show (P.sourceColumn position)
