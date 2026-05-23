{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ProgramParserParitySpec (spec) where

import Data.List (intercalate, isInfixOf)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import MLF.API
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Syntax (Lit (..), SrcKind (..), SrcTy (..), SrcType)
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
        it "shared parser-owned .mlfp parser lexes carried fixtures from source text before grammar parsing" $ do
            basicExpected <- readFile expectedProjectionPath
            dataExpected <- readFile dataDeclarationConstructorSpansExpectedProjectionPath

            basicRoot <-
                writeSourceTextFixturePackage
                    sourceTextBasicPackageRoot
                    canonicalSourcePath
                    basicModuleSourceText
            dataRoot <-
                writeSourceTextFixturePackage
                    sourceTextDataPackageRoot
                    dataDeclarationConstructorSpansCanonicalSourcePath
                    dataDeclarationConstructorSpansSourceText

            runSharedParserFixture basicRoot
                `shouldReturn` Right basicExpected
            runSharedParserFixture dataRoot
                `shouldReturn` Right dataExpected

        it "shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns" $ do
            constructorSource <- readFile caseExpressionConstructorPatternsCanonicalSourcePath
            constructorExpected <- readFile caseExpressionConstructorPatternsExpectedProjectionPath
            nestedSource <- readFile caseExpressionNestedPatternsCanonicalSourcePath
            nestedExpected <- readFile caseExpressionNestedPatternsExpectedProjectionPath

            constructorCanonicalProjection <-
                renderCanonicalProjection caseExpressionConstructorPatternsCanonicalSourcePath constructorSource
            nestedCanonicalProjection <-
                renderCanonicalProjection caseExpressionNestedPatternsCanonicalSourcePath nestedSource
            constructorParserParityOutput <-
                runSharedParserFixture caseExpressionConstructorPatternsParserParityPackageRoot
            nestedParserParityOutput <-
                runSharedParserFixture caseExpressionNestedPatternsParserParityPackageRoot

            constructorCanonicalProjection `shouldBe` constructorExpected
            nestedCanonicalProjection `shouldBe` nestedExpected
            constructorParserParityOutput `shouldBe` Right constructorExpected
            nestedParserParityOutput `shouldBe` Right nestedExpected

        it "shared parser-owned .mlfp parser extends source-text grammar to typeclass, deriving, and instance declarations" $ do
            derivingSource <- readFile typeclassDerivingMethodCanonicalSourcePath
            derivingExpected <- readFile typeclassDerivingMethodExpectedProjectionPath
            instanceSource <- readFile typeclassInstanceNullaryMethodCanonicalSourcePath
            instanceExpected <- readFile typeclassInstanceNullaryMethodExpectedProjectionPath

            derivingCanonicalProjection <-
                renderCanonicalProjection typeclassDerivingMethodCanonicalSourcePath derivingSource
            instanceCanonicalProjection <-
                renderCanonicalProjection typeclassInstanceNullaryMethodCanonicalSourcePath instanceSource
            derivingParserParityOutput <-
                runSharedParserFixture typeclassDerivingMethodParserParityPackageRoot
            instanceParserParityOutput <-
                runSharedParserFixture typeclassInstanceNullaryMethodParserParityPackageRoot

            derivingCanonicalProjection `shouldBe` derivingExpected
            instanceCanonicalProjection `shouldBe` instanceExpected
            derivingParserParityOutput `shouldBe` Right derivingExpected
            instanceParserParityOutput `shouldBe` Right instanceExpected

        it "shared parser-owned .mlfp parser extends source-text grammar to higher-kinded and constrained class syntax" $ do
            higherKindedSource <- readFile higherKindedClassDataParamsCanonicalSourcePath
            higherKindedExpected <- readFile higherKindedClassDataParamsExpectedProjectionPath
            fundepSource <- readFile multiparamSuperclassFundepCanonicalSourcePath
            fundepExpected <- readFile multiparamSuperclassFundepExpectedProjectionPath

            higherKindedCanonicalProjection <-
                renderCanonicalProjection higherKindedClassDataParamsCanonicalSourcePath higherKindedSource
            fundepCanonicalProjection <-
                renderCanonicalProjection multiparamSuperclassFundepCanonicalSourcePath fundepSource
            higherKindedParserParityOutput <-
                runSharedParserFixture higherKindedClassDataParamsParserParityPackageRoot
            fundepParserParityOutput <-
                runSharedParserFixture multiparamSuperclassFundepParserParityPackageRoot
            negativeEvidenceRoot <- writeHigherKindedFundepNegativeEvidencePackage
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")

            higherKindedCanonicalProjection `shouldBe` higherKindedExpected
            fundepCanonicalProjection `shouldBe` fundepExpected
            higherKindedParserParityOutput `shouldBe` Right higherKindedExpected
            fundepParserParityOutput `shouldBe` Right fundepExpected
            runSharedParserFixture negativeEvidenceRoot
                `shouldReturn` Right higherKindedFundepNegativeEvidenceProjection
            filter (`isInfixOf` sharedParserSource) sharedParserRound314ShortcutPhrases
                `shouldBe` []

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
            sharedLexerSource `shouldSatisfy` isInfixOf "def tokenizeCompleteModule : String -> LexerResult"
            sharedLexerSource `shouldSatisfy` isInfixOf "initialSourceCursor sourceText"

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

        it "parser-owned .mlfp parser rejects malformed case branch arrows through public run-program" $ do
            evidenceRoot <- writeCaseExpressionNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right caseExpressionNegativeEvidenceProjection

        it "parser-owned .mlfp parser rejects malformed instance method definitions through public run-program" $ do
            evidenceRoot <- writeTypeclassInstanceNegativeEvidencePackage
            runSharedParserFixture evidenceRoot
                `shouldReturn` Right typeclassInstanceNegativeEvidenceProjection

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

caseExpressionConstructorPatternsCanonicalSourcePath :: FilePath
caseExpressionConstructorPatternsCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/case-expression-constructor-patterns/src/Main.mlfp"

caseExpressionNestedPatternsCanonicalSourcePath :: FilePath
caseExpressionNestedPatternsCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/case-expression-nested-patterns/src/Main.mlfp"

typeclassDerivingMethodCanonicalSourcePath :: FilePath
typeclassDerivingMethodCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/typeclass-deriving-method/src/Main.mlfp"

typeclassInstanceNullaryMethodCanonicalSourcePath :: FilePath
typeclassInstanceNullaryMethodCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/typeclass-instance-nullary-method/src/Main.mlfp"

higherKindedClassDataParamsCanonicalSourcePath :: FilePath
higherKindedClassDataParamsCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/higher-kinded-class-data-params/src/Main.mlfp"

multiparamSuperclassFundepCanonicalSourcePath :: FilePath
multiparamSuperclassFundepCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/multiparam-superclass-fundep/src/Main.mlfp"

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

caseExpressionConstructorPatternsExpectedProjectionPath :: FilePath
caseExpressionConstructorPatternsExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/case-expression-constructor-patterns/expected/parser-program.txt"

caseExpressionNestedPatternsExpectedProjectionPath :: FilePath
caseExpressionNestedPatternsExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/case-expression-nested-patterns/expected/parser-program.txt"

typeclassDerivingMethodExpectedProjectionPath :: FilePath
typeclassDerivingMethodExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/typeclass-deriving-method/expected/parser-program.txt"

typeclassInstanceNullaryMethodExpectedProjectionPath :: FilePath
typeclassInstanceNullaryMethodExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/typeclass-instance-nullary-method/expected/parser-program.txt"

higherKindedClassDataParamsExpectedProjectionPath :: FilePath
higherKindedClassDataParamsExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/higher-kinded-class-data-params/expected/parser-program.txt"

multiparamSuperclassFundepExpectedProjectionPath :: FilePath
multiparamSuperclassFundepExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/multiparam-superclass-fundep/expected/parser-program.txt"

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

caseExpressionConstructorPatternsParserParityPackageRoot :: FilePath
caseExpressionConstructorPatternsParserParityPackageRoot =
    "test/programs/compiler-parser-parity/case-expression-constructor-patterns"

caseExpressionNestedPatternsParserParityPackageRoot :: FilePath
caseExpressionNestedPatternsParserParityPackageRoot =
    "test/programs/compiler-parser-parity/case-expression-nested-patterns"

typeclassDerivingMethodParserParityPackageRoot :: FilePath
typeclassDerivingMethodParserParityPackageRoot =
    "test/programs/compiler-parser-parity/typeclass-deriving-method"

typeclassInstanceNullaryMethodParserParityPackageRoot :: FilePath
typeclassInstanceNullaryMethodParserParityPackageRoot =
    "test/programs/compiler-parser-parity/typeclass-instance-nullary-method"

higherKindedClassDataParamsParserParityPackageRoot :: FilePath
higherKindedClassDataParamsParserParityPackageRoot =
    "test/programs/compiler-parser-parity/higher-kinded-class-data-params"

multiparamSuperclassFundepParserParityPackageRoot :: FilePath
multiparamSuperclassFundepParserParityPackageRoot =
    "test/programs/compiler-parser-parity/multiparam-superclass-fundep"

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
    [ concat ["Basic", "Module", "Tokens"]
    , concat ["Import", "Bool", "Tokens"]
    , concat ["Value", "Def", "List", "Tokens"]
    , concat ["Let", "Lambda", "Application", "Tokens"]
    , concat ["Typed", "Annotation", "Types", "Tokens"]
    , concat ["Data", "Declaration", "Tokens"]
    , concat ["Typeclass", "Tokens"]
    , concat ["Instance", "Tokens"]
    , concat ["Higher", "Kinded", "Tokens"]
    , concat ["Constraint", "Tokens"]
    , concat ["Fundep", "Tokens"]
    , concat ["LexerOk ", "basic", "Module", "Tokens"]
    , concat ["LexerOk ", "import", "Bool", "Tokens"]
    , concat ["LexerOk ", "value", "Def", "List", "Tokens"]
    , concat ["LexerOk ", "let", "Lambda", "Application", "Tokens"]
    , concat ["LexerOk ", "typed", "Annotation", "Types", "Tokens"]
    , concat ["LexerOk ", "data", "Declaration", "Tokens"]
    , concat ["LexerOk ", "typeclass", "Tokens"]
    , concat ["LexerOk ", "instance", "Tokens"]
    , concat ["LexerOk ", "higher", "Kinded", "Tokens"]
    , concat ["LexerOk ", "constraint", "Tokens"]
    , concat ["LexerOk ", "fundep", "Tokens"]
    , concat ["case", " tokens"]
    , concat ["class", " tokens"]
    , concat ["instance", " tokens"]
    , concat ["higher-kinded", " tokens"]
    , concat ["constraint", " tokens"]
    , concat ["fundep", " tokens"]
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

sharedParserRound314ShortcutPhrases :: [String]
sharedParserRound314ShortcutPhrases =
    [ "parseHigherKindedModule"
    , "parseMultiparam"
    , "completeModuleKey \"higher-kinded-class-data-params\""
    , "completeModuleKey \"multiparam-superclass-fundep\""
    , "moduleKey \"higher-kinded-class-data-params\""
    , "moduleKey \"multiparam-superclass-fundep\""
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
    [ "parseCompleteModule sourceText"
    , "tokenizeCompleteModule sourceText"
    , "tokenizeCompleteModule lexerMismatchSourceText"
    , "renderParserNegativeEvidenceFromSourceText"
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

caseExpressionNegativeEvidencePackageRoot :: FilePath
caseExpressionNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-case-expression-negative-evidence"

typeclassInstanceNegativeEvidencePackageRoot :: FilePath
typeclassInstanceNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-typeclass-instance-negative-evidence"

higherKindedFundepNegativeEvidencePackageRoot :: FilePath
higherKindedFundepNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-higher-kinded-fundep-negative-evidence"

sourceTextBasicPackageRoot :: FilePath
sourceTextBasicPackageRoot =
    "dist-newstyle/parser-parity-basic-module-def-bool-source-text"

sourceTextDataPackageRoot :: FilePath
sourceTextDataPackageRoot =
    "dist-newstyle/parser-parity-data-declaration-constructor-spans-source-text"

writeSourceTextFixturePackage :: FilePath -> FilePath -> String -> IO FilePath
writeSourceTextFixturePackage packageRoot sourceFile sourceText = do
    removePathForcibly packageRoot
    createDirectoryIfMissing True packageRoot
    writeFile
        (packageRoot </> "Main.mlfp")
        (sourceTextFixtureMainSource sourceFile sourceText)
    pure packageRoot

sourceTextFixtureMainSource :: FilePath -> String -> String
sourceTextFixtureMainSource sourceFile sourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderParserParityProjectionFromSourceText);"
        , ""
        , "  def sourceFile : String ="
        , "    " <> show sourceFile <> ";"
        , ""
        , "  def sourceText : String ="
        , "    " <> show sourceText <> ";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderParserParityProjectionFromSourceText sourceFile sourceText);"
        , "}"
        ]

basicModuleSourceText :: String
basicModuleSourceText =
    unlines
        [ "module Main export (main) {"
        , "  def main : Bool = true;"
        , "}"
        ]

dataDeclarationConstructorSpansSourceText :: String
dataDeclarationConstructorSpansSourceText =
    unlines
        [ "module Main export (Nat(..), main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  def main : Nat = Succ Zero;"
        , "}"
        ]

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

writeCaseExpressionNegativeEvidencePackage :: IO FilePath
writeCaseExpressionNegativeEvidencePackage = do
    removePathForcibly caseExpressionNegativeEvidencePackageRoot
    createDirectoryIfMissing True caseExpressionNegativeEvidencePackageRoot
    writeFile (caseExpressionNegativeEvidencePackageRoot </> "Main.mlfp") caseExpressionNegativeEvidenceMainSource
    pure caseExpressionNegativeEvidencePackageRoot

writeTypeclassInstanceNegativeEvidencePackage :: IO FilePath
writeTypeclassInstanceNegativeEvidencePackage = do
    removePathForcibly typeclassInstanceNegativeEvidencePackageRoot
    createDirectoryIfMissing True typeclassInstanceNegativeEvidencePackageRoot
    writeFile (typeclassInstanceNegativeEvidencePackageRoot </> "Main.mlfp") typeclassInstanceNegativeEvidenceMainSource
    pure typeclassInstanceNegativeEvidencePackageRoot

writeHigherKindedFundepNegativeEvidencePackage :: IO FilePath
writeHigherKindedFundepNegativeEvidencePackage = do
    removePathForcibly higherKindedFundepNegativeEvidencePackageRoot
    createDirectoryIfMissing True higherKindedFundepNegativeEvidencePackageRoot
    writeFile (higherKindedFundepNegativeEvidencePackageRoot </> "Main.mlfp") higherKindedFundepNegativeEvidenceMainSource
    pure higherKindedFundepNegativeEvidencePackageRoot

retryEvidenceMainSource :: String
retryEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderParserParityRetryEvidence);"
        , ""
        , "  def sourceFile : String ="
        , "    \"test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp\";"
        , ""
        , "  def sourceText : String ="
        , "    " <> show basicModuleSourceText <> ";"
        , ""
        , "  def lexerMismatchSourceText : String ="
        , "    " <> show lexerMismatchSourceText <> ";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderParserParityRetryEvidence sourceFile sourceText lexerMismatchSourceText);"
        , "}"
        ]

importNegativeEvidenceMainSource :: String
importNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "import parser negative "
        importCanonicalSourcePath
        importNegativeSourceText

valueDefListNegativeEvidenceMainSource :: String
valueDefListNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "value-def-list parser negative "
        valueDefListCanonicalSourcePath
        valueDefListNegativeSourceText

letLambdaApplicationNegativeEvidenceMainSource :: String
letLambdaApplicationNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "let-lambda-application parser negative "
        letLambdaApplicationCanonicalSourcePath
        letLambdaApplicationNegativeSourceText

typedAnnotationTypesNegativeEvidenceMainSource :: String
typedAnnotationTypesNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "typed-annotation-types parser negative "
        typedAnnotationTypesCanonicalSourcePath
        typedAnnotationTypesNegativeSourceText

dataDeclarationConstructorSpansNegativeEvidenceMainSource :: String
dataDeclarationConstructorSpansNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "data-declaration parser negative "
        dataDeclarationConstructorSpansCanonicalSourcePath
        dataDeclarationConstructorSpansNegativeSourceText

caseExpressionNegativeEvidenceMainSource :: String
caseExpressionNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "case-expression parser negative "
        caseExpressionConstructorPatternsCanonicalSourcePath
        caseExpressionNegativeSourceText

typeclassInstanceNegativeEvidenceMainSource :: String
typeclassInstanceNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "typeclass-instance parser negative "
        typeclassInstanceNullaryMethodCanonicalSourcePath
        typeclassInstanceNegativeSourceText

higherKindedFundepNegativeEvidenceMainSource :: String
higherKindedFundepNegativeEvidenceMainSource =
    parserNegativeEvidenceMainSource
        "higher-kinded-fundep parser negative "
        multiparamSuperclassFundepCanonicalSourcePath
        higherKindedFundepNegativeSourceText

parserNegativeEvidenceMainSource :: String -> FilePath -> String -> String
parserNegativeEvidenceMainSource prefix sourceFile sourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderParserNegativeEvidenceFromSourceText);"
        , ""
        , "  def sourceFile : String ="
        , "    " <> show sourceFile <> ";"
        , ""
        , "  def sourceText : String ="
        , "    " <> show sourceText <> ";"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn (renderParserNegativeEvidenceFromSourceText " <> show prefix <> " sourceFile sourceText);"
        , "}"
        ]

lexerMismatchSourceText :: String
lexerMismatchSourceText =
    "module Main ?\n"

importNegativeSourceText :: String
importNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Bool)"
        , "  def main : Bool = true;"
        , "}"
        ]

valueDefListNegativeSourceText :: String
valueDefListNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Int);"
        , "  def two : Int = 2"
        , "  def main : Int = two;"
        , "}"
        ]

letLambdaApplicationNegativeSourceText :: String
letLambdaApplicationNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Int);"
        , "  def main : Int = let id = λx x id 1;"
        , "}"
        ]

typedAnnotationTypesNegativeSourceText :: String
typedAnnotationTypesNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Int);"
        , "  def main : Int = let id : = λ(x : Int) x in (id 1 : Int);"
        , "}"
        ]

dataDeclarationConstructorSpansNegativeSourceText :: String
dataDeclarationConstructorSpansNegativeSourceText =
    unlines
        [ "module Main export (Nat(..), main) {"
        , "  data Nat ="
        , "      Zero Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  def main : Nat = Succ Zero;"
        , "}"
        ]

caseExpressionNegativeSourceText :: String
caseExpressionNegativeSourceText =
    unlines
        [ "module Main export (Nat(..), main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  def main : Int = case Succ Zero of {"
        , "    Zero 0;"
        , "    Succ _ -> 1"
        , "  };"
        , "}"
        ]

typeclassInstanceNegativeSourceText :: String
typeclassInstanceNegativeSourceText =
    unlines
        [ "module Main export (Monoid, Nat(..), mempty, append, main) {"
        , "  class Monoid a {"
        , "    mempty : a;"
        , "    append : a -> a -> a;"
        , "  }"
        , ""
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  instance Monoid Nat {"
        , "    mempty Zero;"
        , "    append = λleft λright left;"
        , "  }"
        , ""
        , "  def main : Nat = append (mempty : Nat) Zero;"
        , "}"
        ]

higherKindedFundepNegativeSourceText :: String
higherKindedFundepNegativeSourceText =
    unlines
        [ "module Main export (Monad) {"
        , "  class Functor f => Monad (m :: * -> *) (f :: * -> *) | m f {"
        , "    bind : ∀ a b. m a -> (a -> m b) -> m b;"
        , "  }"
        , ""
        , "  instance Monad IO IO {"
        , "  }"
        , "}"
        ]

retryEvidenceProjection :: String
retryEvidenceProjection =
    unlines
        [ "tokens module@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:1-1:7 Main@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:8-1:12 export@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:19"
        , "lexer negative unexpected-source@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:13"
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

caseExpressionNegativeEvidenceProjection :: String
caseExpressionNegativeEvidenceProjection =
    unlines
        [ "case-expression parser negative expected-case-branch-arrow@test/conformance/mlfp/parser-parity/case-expression-constructor-patterns/src/Main.mlfp:7:10-7:11"
        ]

typeclassInstanceNegativeEvidenceProjection :: String
typeclassInstanceNegativeEvidenceProjection =
    unlines
        [ "typeclass-instance parser negative expected-instance-method-equals@test/conformance/mlfp/parser-parity/typeclass-instance-nullary-method/src/Main.mlfp:12:12-12:16"
        ]

higherKindedFundepNegativeEvidenceProjection :: String
higherKindedFundepNegativeEvidenceProjection =
    unlines
        [ "higher-kinded-fundep parser negative expected-functional-dependency-arrow@test/conformance/mlfp/parser-parity/multiparam-superclass-fundep/src/Main.mlfp:2:60-2:61"
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
        P.DeclClass class0 -> renderClassProjection spans class0
        P.DeclInstance instance0 -> renderInstanceProjection spans instance0
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
    renderedDeriving <- renderDerivingProjections spans (P.dataDeclDeriving data0)
    pure $
        ( "data "
            ++ P.dataDeclName data0
            ++ renderOptionalTypeParams (P.dataDeclParams data0)
            ++ " span="
            ++ renderSpan dataSpan
        )
            : renderedConstructors
                ++ renderedDeriving

renderClassProjection :: P.ProgramSpanIndex -> P.ClassDecl -> IO [String]
renderClassProjection spans class0 = do
    classSpan <- requireIndexedSpan "class declaration" (P.classDeclName class0) 0 (P.spanClasses spans)
    renderedSuperclasses <- traverse (renderSuperclassProjection spans) (P.classDeclSuperclasses class0)
    let renderedFundeps = map (renderFunctionalDependencyProjection classSpan) (P.classDeclFundeps class0)
    renderedMethods <- traverse (renderMethodSignatureProjection spans) (P.classDeclMethods class0)
    pure $
        ( "class "
            ++ P.classDeclName class0
            ++ " params="
            ++ renderTypeParams (NE.toList (P.classDeclParams class0))
            ++ " span="
            ++ renderSpan classSpan
        )
            : renderedSuperclasses
                ++ renderedFundeps
                ++ renderedMethods

renderSuperclassProjection :: P.ProgramSpanIndex -> P.ClassConstraint -> IO String
renderSuperclassProjection spans superclass0 = do
    classSpan <- requireListSpan "superclass" (P.constraintClassName superclass0) (P.spanClasses spans)
    pure $
        "superclass "
            ++ P.constraintClassName superclass0
            ++ " types="
            ++ intercalate "," (map renderSrcType (NE.toList (P.constraintTypes superclass0)))
            ++ " span="
            ++ renderSpan classSpan

renderFunctionalDependencyProjection :: P.SourceSpan -> P.FunctionalDependency -> String
renderFunctionalDependencyProjection classSpan fundep =
    "fundep "
        ++ intercalate "," (NE.toList (P.fundepDeterminers fundep))
        ++ " -> "
        ++ intercalate "," (NE.toList (P.fundepDetermined fundep))
        ++ " span="
        ++ renderSpan classSpan

renderMethodSignatureProjection :: P.ProgramSpanIndex -> P.MethodSig -> IO String
renderMethodSignatureProjection spans method0 = do
    methodSpan <- requireIndexedSpan "method signature" (P.methodSigName method0) 0 (P.spanValues spans)
    pure $
        "method-signature "
            ++ P.methodSigName method0
            ++ " type="
            ++ renderConstrainedType (P.methodSigType method0)
            ++ " span="
            ++ renderSpan methodSpan

renderInstanceProjection :: P.ProgramSpanIndex -> P.InstanceDecl -> IO [String]
renderInstanceProjection spans instance0 = do
    let className = P.instanceDeclClass instance0 :: String
    classSpan <- requireLastSpan "instance class" className (P.spanClasses spans)
    renderedConstraints <- traverse (renderInstanceConstraintProjection spans) (P.instanceDeclConstraints instance0)
    renderedMethods <- traverse (renderMethodDefProjection spans) (P.instanceDeclMethods instance0)
    pure $
        ( "instance "
            ++ className
            ++ " types="
            ++ intercalate "," (map renderSrcType (NE.toList (P.instanceDeclTypes instance0)))
            ++ " span="
            ++ renderSpan classSpan
        )
            : renderedConstraints
                ++ renderedMethods

renderInstanceConstraintProjection :: P.ProgramSpanIndex -> P.ClassConstraint -> IO String
renderInstanceConstraintProjection spans constraint0 = do
    classSpan <- requireListSpan "instance constraint" (P.constraintClassName constraint0) (P.spanClasses spans)
    pure $
        "instance-constraint "
            ++ P.constraintClassName constraint0
            ++ " types="
            ++ intercalate "," (map renderSrcType (NE.toList (P.constraintTypes constraint0)))
            ++ " span="
            ++ renderSpan classSpan

renderMethodDefProjection :: P.ProgramSpanIndex -> P.MethodDef -> IO String
renderMethodDefProjection spans method0 = do
    methodSpan <- requireLastSpan "method definition" (P.methodDefName method0) (P.spanValues spans)
    pure $
        "method-definition "
            ++ P.methodDefName method0
            ++ " expr="
            ++ renderExpr (P.methodDefExpr method0)
            ++ " span="
            ++ renderSpan methodSpan

renderDerivingProjections :: P.ProgramSpanIndex -> [String] -> IO [String]
renderDerivingProjections spans classes =
    traverse (renderDerivingProjection spans) classes

renderDerivingProjection :: P.ProgramSpanIndex -> String -> IO String
renderDerivingProjection spans className = do
    classSpan <- requireLastSpan "deriving class" className (P.spanClasses spans)
    pure $
        "deriving "
            ++ className
            ++ " span="
            ++ renderSpan classSpan

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

requireIndexedSpan :: String -> String -> Int -> Map.Map String [P.SourceSpan] -> IO P.SourceSpan
requireIndexedSpan label name index spans =
    case Map.lookup name spans of
        Just values
            | index < length values -> pure (values !! index)
            | otherwise ->
                expectationFailure ("missing " ++ label ++ " span index " ++ show index ++ " for " ++ show name ++ ", got: " ++ show values)
                    >> fail "missing indexed span"
        Nothing ->
            expectationFailure ("missing " ++ label ++ " span for " ++ show name)
                >> fail "missing span"

requireLastSpan :: String -> String -> Map.Map String [P.SourceSpan] -> IO P.SourceSpan
requireLastSpan label name spans =
    case Map.lookup name spans of
        Just [] ->
            expectationFailure ("empty " ++ label ++ " span list for " ++ show name)
                >> fail "empty span list"
        Just values -> pure (last values)
        Nothing ->
            expectationFailure ("missing " ++ label ++ " span for " ++ show name)
                >> fail "missing span"

renderConstrainedType :: P.ConstrainedType -> String
renderConstrainedType constrained =
    case P.constrainedConstraints constrained of
        [] -> renderSrcType (P.constrainedBody constrained)
        constraints ->
            intercalate ", " (map renderClassConstraint constraints)
                ++ " => "
                ++ renderSrcType (P.constrainedBody constrained)

renderClassConstraint :: P.ClassConstraint -> String
renderClassConstraint constraint =
    let className = P.constraintClassName constraint :: String
     in className
        ++ " "
        ++ intercalate " " (map renderSrcType (NE.toList (P.constraintTypes constraint)))

renderSrcType :: SrcType -> String
renderSrcType ty =
    renderSrcTypePrec 0 ty

renderSrcTypePrec :: Int -> SrcType -> String
renderSrcTypePrec precedence ty =
    case ty of
        STVar name -> name
        STBase name -> name
        STCon name args ->
            parenthesizeIf (precedence > 2) $
                unwords (name : map (renderSrcTypePrec 3) (NE.toList args))
        STVarApp name args ->
            parenthesizeIf (precedence > 2) $
                unwords (name : map (renderSrcTypePrec 3) (NE.toList args))
        STTyApp fun arg ->
            parenthesizeIf (precedence > 2) $
                renderSrcTypePrec 2 fun ++ " " ++ renderSrcTypePrec 3 arg
        STArrow dom cod ->
            parenthesizeIf (precedence > 1) $
                renderSrcTypePrec 2 dom ++ " -> " ++ renderSrcTypePrec 1 cod
        STForall name Nothing body ->
            let (names, finalBody) = collectForallNames body
             in parenthesizeIf (precedence > 0) $
                    "∀" ++ unwords (name : names) ++ ". " ++ renderSrcTypePrec 0 finalBody
        STForall name (Just _) body ->
            parenthesizeIf (precedence > 0) $
                "∀" ++ name ++ ". " ++ renderSrcTypePrec 0 body
        other -> show other

collectForallNames :: SrcType -> ([String], SrcType)
collectForallNames ty =
    case ty of
        STForall name Nothing body ->
            let (names, finalBody) = collectForallNames body
             in (name : names, finalBody)
        _ -> ([], ty)

renderOptionalTypeParams :: [P.TypeParam] -> String
renderOptionalTypeParams params =
    case params of
        [] -> ""
        _ -> " params=" ++ renderTypeParams params

renderTypeParams :: [P.TypeParam] -> String
renderTypeParams =
    intercalate "," . map renderTypeParam

renderTypeParam :: P.TypeParam -> String
renderTypeParam param =
    P.typeParamName param ++ renderTypeParamKind (P.typeParamKind param)

renderTypeParamKind :: SrcKind -> String
renderTypeParamKind kind =
    case kind of
        KType -> ""
        _ -> "::" ++ renderSrcKind kind

renderSrcKind :: SrcKind -> String
renderSrcKind =
    renderSrcKindPrec 0

renderSrcKindPrec :: Int -> SrcKind -> String
renderSrcKindPrec precedence kind =
    case kind of
        KType -> "*"
        KArrow left right ->
            parenthesizeIf (precedence > 0) $
                renderSrcKindPrec 1 left ++ " -> " ++ renderSrcKindPrec 0 right

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
        P.ECase scrutinee alts ->
            parenthesizeIf (precedence > 0) $
                "case "
                    ++ renderExprPrec 0 scrutinee
                    ++ " of { "
                    ++ intercalate "; " (map renderAlt alts)
                    ++ " }"
        other -> show other

renderParam :: P.Param -> String
renderParam param =
    case P.paramType param of
        Nothing -> P.paramName param
        Just ty -> "(" ++ P.paramName param ++ " : " ++ renderSrcType ty ++ ")"

renderAlt :: P.Alt -> String
renderAlt alt =
    renderPattern (P.altPattern alt) ++ " -> " ++ renderExpr (P.altExpr alt)

renderPattern :: P.Pattern -> String
renderPattern pat =
    case pat of
        P.PatCtor ctor patterns ->
            unwords (ctor : map renderPatternArg patterns)
        P.PatVar name -> name
        P.PatWildcard -> "_"
        P.PatAnn inner ty -> "(" ++ renderPattern inner ++ " : " ++ renderSrcType ty ++ ")"

renderPatternArg :: P.Pattern -> String
renderPatternArg pat =
    case pat of
        P.PatVar {} -> renderPattern pat
        P.PatWildcard -> renderPattern pat
        P.PatCtor _ [] -> renderPattern pat
        _ -> "(" ++ renderPattern pat ++ ")"

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
