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
import MLF.Frontend.TypeLevel
    ( TypeFamilyDecl (..)
    , TypeFamilyEquation (..)
    , TypeLevelKind (..)
    , TypeLevelPattern (..)
    , TypeLevelTy (..)
    )
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
        it "matches canonical parser projections for every batched positive fixture" $
            traverse_ assertCanonicalParserParityProjection parserParityPositiveCases

        it "shared parser-owned .mlfp parser parses Char and String literals" $ do
            source <- readFile textLiteralCharStringCanonicalSourcePath
            expected <- readFile textLiteralCharStringExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection textLiteralCharStringCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch textLiteralCharStringParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses first-class polymorphic source types" $ do
            source <- readFile firstClassPolymorphismSourceTypesCanonicalSourcePath
            expected <- readFile firstClassPolymorphismSourceTypesExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection firstClassPolymorphismSourceTypesCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch firstClassPolymorphismSourceTypesParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        beforeAll loadParserParityBatchFixture $ do
            it "runs all .mlfp parser parity fixtures through one generated public CLI driver" $ \fixture ->
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)

            it "shared parser-owned .mlfp parser parses multi-module source text and export/import surfaces" $ \fixture -> do
                abstractSource <- readFile multiModuleAbstractExportImportCanonicalSourcePath
                abstractExpected <- readFile multiModuleAbstractExportImportExpectedProjectionPath
                recursiveSource <- readFile multiModuleRecursiveAdtExportImportCanonicalSourcePath
                recursiveExpected <- readFile multiModuleRecursiveAdtExportImportExpectedProjectionPath

                abstractCanonicalProjection <-
                    renderCanonicalProjection multiModuleAbstractExportImportCanonicalSourcePath abstractSource
                recursiveCanonicalProjection <-
                    renderCanonicalProjection multiModuleRecursiveAdtExportImportCanonicalSourcePath recursiveSource
                sharedParserSource <- concat <$> traverse readFile sharedParserAuditFiles

                abstractCanonicalProjection `shouldBe` abstractExpected
                recursiveCanonicalProjection `shouldBe` recursiveExpected
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:multi-module-abstract-export-import" abstractExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:multi-module-recursive-adt-export-import" recursiveExpected)
                filter (`isInfixOf` sharedParserSource) sharedParserRound318ShortcutPhrases
                    `shouldBe` []

            it "parser-owned .mlfp parser rejects malformed multi-module import exposing separators through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:multi-module-import-exposing-separator" importExposingSeparatorNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed text literal diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:text-literal-malformed" textLiteralMalformedNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed first-class polymorphic source-type diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:first-class-polymorphism-source-type" firstClassPolymorphismSourceTypeNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser library routes the generated batch through one entrypoint" $ \fixture -> do
                sharedParserExists <- doesFileExist (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
                sharedParserExists `shouldBe` True

                let batchSource = batchMainSource fixture
                batchSource `shouldSatisfy` isInfixOf "import ParserParityParser exposing"
                batchSource `shouldSatisfy` isInfixOf "renderParserParityProjectionFromSourceText"
                batchSource `shouldSatisfy` isInfixOf "renderParserNegativeEvidenceFromSourceText"
                batchSource `shouldSatisfy` isInfixOf "renderParserParityRetryEvidence"

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

        it "shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            let shortcutMatches =
                    filter
                        (`isInfixOf` sharedParserSource)
                        sharedParserShortcutPhrases
            shortcutMatches `shouldBe` []

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

typeFamilyKindLambdaCanonicalSourcePath :: FilePath
typeFamilyKindLambdaCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/type-family-kind-lambda/src/Main.mlfp"

typeFamilyApplyAnnotationCanonicalSourcePath :: FilePath
typeFamilyApplyAnnotationCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/type-family-apply-annotation/src/Main.mlfp"

gadtResultConstructorSpansCanonicalSourcePath :: FilePath
gadtResultConstructorSpansCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/gadt-result-constructor-spans/src/Main.mlfp"

existentialConstructorForallCanonicalSourcePath :: FilePath
existentialConstructorForallCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/existential-constructor-forall/src/Main.mlfp"

qualifiedImportAliasReferencesCanonicalSourcePath :: FilePath
qualifiedImportAliasReferencesCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/qualified-import-alias-references/src/Main.mlfp"

qualifiedImportAliasOnlyCanonicalSourcePath :: FilePath
qualifiedImportAliasOnlyCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/qualified-import-alias-only/src/Main.mlfp"

multiModuleAbstractExportImportCanonicalSourcePath :: FilePath
multiModuleAbstractExportImportCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/multi-module-abstract-export-import/src/Main.mlfp"

multiModuleRecursiveAdtExportImportCanonicalSourcePath :: FilePath
multiModuleRecursiveAdtExportImportCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/multi-module-recursive-adt-export-import/src/Main.mlfp"

textLiteralCharStringCanonicalSourcePath :: FilePath
textLiteralCharStringCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/text-literal-char-string/src/Main.mlfp"

firstClassPolymorphismSourceTypesCanonicalSourcePath :: FilePath
firstClassPolymorphismSourceTypesCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/first-class-polymorphism-source-types/src/Main.mlfp"

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

typeFamilyKindLambdaExpectedProjectionPath :: FilePath
typeFamilyKindLambdaExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/type-family-kind-lambda/expected/parser-program.txt"

typeFamilyApplyAnnotationExpectedProjectionPath :: FilePath
typeFamilyApplyAnnotationExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/type-family-apply-annotation/expected/parser-program.txt"

gadtResultConstructorSpansExpectedProjectionPath :: FilePath
gadtResultConstructorSpansExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/gadt-result-constructor-spans/expected/parser-program.txt"

existentialConstructorForallExpectedProjectionPath :: FilePath
existentialConstructorForallExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/existential-constructor-forall/expected/parser-program.txt"

qualifiedImportAliasReferencesExpectedProjectionPath :: FilePath
qualifiedImportAliasReferencesExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/qualified-import-alias-references/expected/parser-program.txt"

qualifiedImportAliasOnlyExpectedProjectionPath :: FilePath
qualifiedImportAliasOnlyExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/qualified-import-alias-only/expected/parser-program.txt"

multiModuleAbstractExportImportExpectedProjectionPath :: FilePath
multiModuleAbstractExportImportExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/multi-module-abstract-export-import/expected/parser-program.txt"

multiModuleRecursiveAdtExportImportExpectedProjectionPath :: FilePath
multiModuleRecursiveAdtExportImportExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/multi-module-recursive-adt-export-import/expected/parser-program.txt"

textLiteralCharStringExpectedProjectionPath :: FilePath
textLiteralCharStringExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/text-literal-char-string/expected/parser-program.txt"

firstClassPolymorphismSourceTypesExpectedProjectionPath :: FilePath
firstClassPolymorphismSourceTypesExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/first-class-polymorphism-source-types/expected/parser-program.txt"

sharedParserLibraryRoot :: FilePath
sharedParserLibraryRoot =
    "test/programs/compiler-parser-parity/parser-library"

textLiteralCharStringParserProgramRoot :: FilePath
textLiteralCharStringParserProgramRoot =
    "test/programs/compiler-parser-parity/text-literal-char-string"

firstClassPolymorphismSourceTypesParserProgramRoot :: FilePath
firstClassPolymorphismSourceTypesParserProgramRoot =
    "test/programs/compiler-parser-parity/first-class-polymorphism-source-types"

sharedParserAuditFiles :: [FilePath]
sharedParserAuditFiles =
    [ sharedParserLibraryRoot </> "ParserParityToken.mlfp"
    , sharedParserLibraryRoot </> "ParserParityLexer.mlfp"
    , sharedParserLibraryRoot </> "ParserParityParser.mlfp"
    , sharedParserLibraryRoot </> "ParserParityAst.mlfp"
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
    , concat ["Type", "Family", "Tokens"]
    , concat ["Family", "Tokens"]
    , concat ["Gadt", "Tokens"]
    , concat ["Existential", "Tokens"]
    , concat ["Multi", "Module", "Tokens"]
    , concat ["Abstract", "Export", "Tokens"]
    , concat ["Recursive", "Adt", "Tokens"]
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
    , concat ["LexerOk ", "type", "Family", "Tokens"]
    , concat ["LexerOk ", "family", "Tokens"]
    , concat ["LexerOk ", "gadt", "Tokens"]
    , concat ["LexerOk ", "existential", "Tokens"]
    , concat ["LexerOk ", "multi", "Module", "Tokens"]
    , concat ["LexerOk ", "abstract", "Export", "Tokens"]
    , concat ["LexerOk ", "recursive", "Adt", "Tokens"]
    , concat ["First", "Class", "Polymorphism", "Tokens"]
    , concat ["LexerOk ", "first", "Class", "Polymorphism", "Tokens"]
    , concat ["case", " tokens"]
    , concat ["class", " tokens"]
    , concat ["instance", " tokens"]
    , concat ["higher-kinded", " tokens"]
    , concat ["constraint", " tokens"]
    , concat ["fundep", " tokens"]
    , concat ["type-family", " tokens"]
    , concat ["family", " tokens"]
    , concat ["gadt", " tokens"]
    , concat ["existential", " tokens"]
    , concat ["multi-module", " tokens"]
    , concat ["abstract-export", " tokens"]
    , concat ["recursive-adt", " tokens"]
    , concat ["first-class-polymorphism-source-types", " tokens"]
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

sharedParserShortcutPhrases :: [String]
sharedParserShortcutPhrases =
    concat
        [ sharedParserRound314ShortcutPhrases
        , sharedParserRound315ShortcutPhrases
        , sharedParserRound316ShortcutPhrases
        , sharedParserRound317ShortcutPhrases
        , sharedParserRound318ShortcutPhrases
        , sharedParserRound319ShortcutPhrases
        , sharedParserRound320ShortcutPhrases
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

sharedParserRound315ShortcutPhrases :: [String]
sharedParserRound315ShortcutPhrases =
    [ "parseTypeFamilyKindLambdaModule"
    , "parseTypeFamilyApplyAnnotationModule"
    , "completeModuleKey \"type-family-kind-lambda\""
    , "completeModuleKey \"type-family-apply-annotation\""
    , "moduleKey \"type-family-kind-lambda\""
    , "moduleKey \"type-family-apply-annotation\""
    ]

sharedParserRound316ShortcutPhrases :: [String]
sharedParserRound316ShortcutPhrases =
    [ "parseGadtResultModule"
    , "parseExistentialConstructorModule"
    , "completeModuleKey \"gadt-result-constructor-spans\""
    , "completeModuleKey \"existential-constructor-forall\""
    , "moduleKey \"gadt-result-constructor-spans\""
    , "moduleKey \"existential-constructor-forall\""
    ]

sharedParserRound317ShortcutPhrases :: [String]
sharedParserRound317ShortcutPhrases =
    [ "parseQualifiedImportAliasModule"
    , "parseQualifiedAliasOnlyModule"
    , "completeModuleKey \"qualified-import-alias-references\""
    , "completeModuleKey \"qualified-import-alias-only\""
    , "moduleKey \"qualified-import-alias-references\""
    , "moduleKey \"qualified-import-alias-only\""
    ]

sharedParserRound318ShortcutPhrases :: [String]
sharedParserRound318ShortcutPhrases =
    [ "parseMultiModuleAbstractExportImport"
    , "parseMultiModuleRecursiveAdtExportImport"
    , "completeModuleKey \"multi-module-abstract-export-import\""
    , "completeModuleKey \"multi-module-recursive-adt-export-import\""
    , "moduleKey \"multi-module-abstract-export-import\""
    , "moduleKey \"multi-module-recursive-adt-export-import\""
    , "programKey \"multi-module-abstract-export-import\""
    , "programKey \"multi-module-recursive-adt-export-import\""
    , "abstract-core-user-program"
    , "recursive-core-user-program"
    , "programKey"
    , "programKey \"abstract-core-user-program\""
    , "programKey \"recursive-core-user-program\""
    , "renderAbstractCoreUserProgram"
    , "renderRecursiveCoreUserProgram"
    , "parseCoreUserProgram"
    , "parseCoreUserAbstractProgram"
    , "parseCoreUserRecursiveProgram"
    , "parseAbstractCoreModule"
    , "parseAbstractUserModule"
    , "parseRecursiveCoreModule"
    , "parseRecursiveUserModule"
    , "abstractCoreUserProgramValue"
    , "recursiveCoreUserProgramValue"
    , "moduleKey \"round318"
    , "completeModuleKey \"round318"
    , "round318-core-nat-value-module"
    , "round318-core-eq-nat-expr-module"
    , "round318-user-nat-value-module"
    , "round318-user-eq-nat-expr-module"
    , "renderRound318"
    , "renderCoreNatValueModuleProjection"
    , "renderCoreEqNatExprModuleProjection"
    , "renderUserNatValueModuleProjection"
    , "renderUserEqNatExprModuleProjection"
    , "parseRound318"
    , "parseCoreNatValue"
    , "parseCoreEqNatExpr"
    , "parseUserNatValue"
    , "parseUserEqNatExpr"
    , "finishCoreNatValue"
    , "finishCoreEqNatExpr"
    , "finishUserNatValue"
    , "finishUserEqNatExpr"
    , "abstractCoreExports"
    , "recursiveCoreExports"
    , "abstractUserImports"
    , "recursiveUserImports"
    , "abstractNatDeclarations"
    , "recursiveCoreDeclarations"
    , "abstractUserDefinitions"
    , "recursiveUserDefinitions"
    , "coreValueBasisExports"
    , "coreRecursiveBasisExports"
    , "userValueMainExports"
    , "userRecursiveMainExports"
    , "userValueBasisImports"
    , "userRecursiveBasisImports"
    , "natBasisDeclarations"
    , "coreRecursiveDeclarations"
    , "userValueBasisDefinitions"
    , "userRecursiveDefinitions"
    , "finishCoreValueBasisRows"
    , "finishCoreRecursiveBasisRows"
    , "finishUserValueBasisRows"
    , "finishUserRecursiveBasisRows"
    , "parseCoreValueBasis"
    , "parseCoreRecursiveBasis"
    , "parseUserValueBasis"
    , "parseUserRecursiveBasis"
    , "parseCoreModuleName"
    , "parseUserModuleName"
    , "parseCoreProgramModule"
    , "parseUserProgramModule"
    , "parseSelectedProgramModuleName"
    , "parseCoreProjectionExportRows"
    , "parseUserNatImportRows"
    , "parseUserClassImportRows"
    , "parseNatSurfaceExportRows"
    , "parseClassSurfaceExportRows"
    , "parseMainHeaderExportRows"
    , "parseThreeItemImportRows"
    , "parseFourItemImportRows"
    , "parseProjectionExportSecondOrDone"
    , "parseProjectionExportThirdOrDone"
    , "parseProjectionExportFourthOrDone"
    , "parseImportProjectionSecondOrClose"
    , "parseImportProjectionThirdOrClose"
    , "parseImportProjectionFourthOrClose"
    , "ExpectedImportExposingSeparator \"12:29-12:33\""
    , "parseProgramModuleName"
    , "parseKnownModuleName"
    , "parseKnownImportModuleName"
    , "parseFirstPlainTypeImportNamed"
    , "parsePlainTypeExportNamed"
    , "parseValueExportNamed"
    , "parseConstructedTypeExportNamed"
    , "parseConstructedTypeImportNamed"
    , "parsePlainTypeImportNamed"
    , "parseValueImportNamed"
    , "firstImportModuleSpan"
    , "exportSurfaceSpan"
    , "constructedExportSurfaceSpan"
    , "valueExportSurfaceSpan"
    , "importSurfaceSpan"
    , "constructedImportSurfaceSpan"
    , "valueImportSurfaceSpan"
    , "constructedSurfaceSpan"
    , "finishUserNatImportRows"
    , "finishUserClassImportRows"
    , "pending-user-main-export"
    , "stringIndexOf importRows"
    , "finishExactModuleBodyRows sourceFile moduleName exportRows \"1:1-17:1\""
    , "finishExactModuleBodyRows sourceFile moduleName exportRows \"1:1-11:1\""
    , "finishImportedBodyRows sourceFile moduleName exportRows importRows \"17:1-25:1\""
    , "finishImportedBodyRows sourceFile moduleName exportRows importRows \"11:1-16:1\""
    , "dataRows sourceFile \"Nat\" \"6:3-10:3\""
    , "dataRows sourceFile \"Nat\" \"2:3-6:3\""
    , "dataParamRows sourceFile \"Expr\" \"a\" \"10:3-14:3\""
    , "constructorRows sourceFile \"Zero\" \"Nat\" \"7:7-8:5\""
    , "constructorRows sourceFile \"Succ\" \"Nat -> Nat\" \"8:7-8:24\""
    , "constructorRows sourceFile \"DoneNat\" \"Nat -> Expr Nat\" \"11:7-12:5\""
    , "constructorRows sourceFile \"Step\" \"Expr a -> Expr a\" \"12:7-12:30\""
    , "defRows sourceFile \"zero\" \"Nat\" \"Zero\" \"14:3-15:1\""
    , "defRows sourceFile \"succ\" \"Nat -> Nat\" \"λ(n : Nat) Succ n\" \"8:3-9:1\""
    , "classRows sourceFile \"Eq\" \"a\" \"2:3-6:3\""
    , "methodSignatureRows sourceFile \"eq\" \"a -> a -> Bool\" \"3:5-4:3\""
    , "mainDefinitionSpanForType"
    , "zeroDefinitionSpan"
    , "natDataRowsForSpan"
    , "exprDataRowsForSpan"
    ]

sharedParserRound319ShortcutPhrases :: [String]
sharedParserRound319ShortcutPhrases =
    [ "parseTextLiteralCharString"
    , "completeModuleKey \"text-literal-char-string\""
    , "moduleKey \"text-literal-char-string\""
    , "programKey \"text-literal-char-string\""
    , concat ["Text", "Literal", "Char", "String", "Tokens"]
    , concat ["LexerOk ", "text", "Literal", "Char", "String", "Tokens"]
    , concat ["text-literal-char-string", " tokens"]
    , "defRows sourceFile \"sampleChar\""
    , "defRows sourceFile \"sampleString\""
    , "def sampleChar type=Char expr='λ'"
    , "def sampleString type=String expr=\"hello λ\""
    , "text-literal parser negative unexpected-source@"
    ]

sharedParserRound320ShortcutPhrases :: [String]
sharedParserRound320ShortcutPhrases =
    [ concat ["parse", "First", "Class", "Polymorphism"]
    , concat ["completeModuleKey \"", "first-class-polymorphism-source-types", "\""]
    , concat ["moduleKey \"", "first-class-polymorphism-source-types", "\""]
    , concat ["programKey \"", "first-class-polymorphism-source-types", "\""]
    , concat ["First", "Class", "Polymorphism", "Tokens"]
    , concat ["LexerOk ", "first", "Class", "Polymorphism", "Tokens"]
    , concat ["first-class-polymorphism-source-types", " tokens"]
    , concat ["defRows sourceFile \"", "usePoly", "\""]
    , concat ["defRows sourceFile \"", "id", "\""]
    , concat ["def usePoly type=", "(∀a. a -> a) -> Bool"]
    , concat ["def id type=", "∀a. a -> a"]
    , concat ["def main type=Bool expr=", "usePoly id"]
    , concat ["first-class-polymorphism parser negative ", "expected-constructor-forall-dot@"]
    ]

sharedParserCompleteParseRequiredPhrases :: [String]
sharedParserCompleteParseRequiredPhrases =
    [ "parserStateAtEnd state"
    , "ParserAtEnd ->"
    , "ParserNotAtEnd ->"
    , "parseCompleteProgram"
    , "parseSharedProgramModule"
    , "parseImportProjectionList"
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
    , "stringAppend \"multi-module import-exposing parser negative expected-import-exposing-separator@\""
    , "stringAppend \"text-literal parser negative unexpected-source@\""
    ]

sharedParserDynamicEvidenceRequiredPhrases :: [String]
sharedParserDynamicEvidenceRequiredPhrases =
    [ "parseCompleteModule sourceText"
    , "parseCompleteProgram sourceText"
    , "tokenizeCompleteModule sourceText"
    , "tokenizeCompleteModule lexerMismatchSourceText"
    , "renderParserNegativeEvidenceFromSourceText"
    , "renderDiagnosticEvidence"
    ]

runSharedParserBatch :: FilePath -> IO (Either String String)
runSharedParserBatch batchRoot =
    runProgramArgs [batchRoot, "--search-path", sharedParserLibraryRoot]

data ParserParityBatchFixture = ParserParityBatchFixture
    { batchExpectedOutput :: String
    , batchMainSource :: String
    , batchRunResult :: Either String String
    }

loadParserParityBatchFixture :: IO ParserParityBatchFixture
loadParserParityBatchFixture = do
    expected <- expectedParserParityBatchOutput
    batchRoot <- writeParserParityBatchPackage
    source <- readFile (batchRoot </> "Main.mlfp")
    result <- runSharedParserBatch batchRoot
    pure
        ParserParityBatchFixture
            { batchExpectedOutput = expected
            , batchMainSource = source
            , batchRunResult = result
            }

parserParityBatchPackageRoot :: FilePath
parserParityBatchPackageRoot =
    "dist-newstyle/parser-parity-batch"

data ParserParityPositiveCase = ParserParityPositiveCase
    { positiveCaseLabel :: String
    , positiveCaseIdentifier :: String
    , positiveCaseSourcePath :: FilePath
    , positiveCaseExpectedPath :: FilePath
    }

data ParserParityNegativeCase = ParserParityNegativeCase
    { negativeCaseLabel :: String
    , negativeCaseIdentifier :: String
    , negativeCasePrefix :: String
    , negativeCaseSourcePath :: FilePath
    , negativeCaseSourceText :: String
    , negativeCaseExpected :: String
    }

parserParityPositiveCases :: [ParserParityPositiveCase]
parserParityPositiveCases =
    [ ParserParityPositiveCase "positive:basic-module-def-bool" "positiveBasicModuleDefBool" canonicalSourcePath expectedProjectionPath
    , ParserParityPositiveCase "positive:import-exposing-def-bool" "positiveImportExposingDefBool" importCanonicalSourcePath importExpectedProjectionPath
    , ParserParityPositiveCase "positive:value-def-list-int-ref" "positiveValueDefListIntRef" valueDefListCanonicalSourcePath valueDefListExpectedProjectionPath
    , ParserParityPositiveCase "positive:let-lambda-application" "positiveLetLambdaApplication" letLambdaApplicationCanonicalSourcePath letLambdaApplicationExpectedProjectionPath
    , ParserParityPositiveCase "positive:typed-annotation-types" "positiveTypedAnnotationTypes" typedAnnotationTypesCanonicalSourcePath typedAnnotationTypesExpectedProjectionPath
    , ParserParityPositiveCase "positive:data-declaration-constructor-spans" "positiveDataDeclarationConstructorSpans" dataDeclarationConstructorSpansCanonicalSourcePath dataDeclarationConstructorSpansExpectedProjectionPath
    , ParserParityPositiveCase "positive:case-expression-constructor-patterns" "positiveCaseExpressionConstructorPatterns" caseExpressionConstructorPatternsCanonicalSourcePath caseExpressionConstructorPatternsExpectedProjectionPath
    , ParserParityPositiveCase "positive:case-expression-nested-patterns" "positiveCaseExpressionNestedPatterns" caseExpressionNestedPatternsCanonicalSourcePath caseExpressionNestedPatternsExpectedProjectionPath
    , ParserParityPositiveCase "positive:typeclass-deriving-method" "positiveTypeclassDerivingMethod" typeclassDerivingMethodCanonicalSourcePath typeclassDerivingMethodExpectedProjectionPath
    , ParserParityPositiveCase "positive:typeclass-instance-nullary-method" "positiveTypeclassInstanceNullaryMethod" typeclassInstanceNullaryMethodCanonicalSourcePath typeclassInstanceNullaryMethodExpectedProjectionPath
    , ParserParityPositiveCase "positive:higher-kinded-class-data-params" "positiveHigherKindedClassDataParams" higherKindedClassDataParamsCanonicalSourcePath higherKindedClassDataParamsExpectedProjectionPath
    , ParserParityPositiveCase "positive:multiparam-superclass-fundep" "positiveMultiparamSuperclassFundep" multiparamSuperclassFundepCanonicalSourcePath multiparamSuperclassFundepExpectedProjectionPath
    , ParserParityPositiveCase "positive:type-family-kind-lambda" "positiveTypeFamilyKindLambda" typeFamilyKindLambdaCanonicalSourcePath typeFamilyKindLambdaExpectedProjectionPath
    , ParserParityPositiveCase "positive:type-family-apply-annotation" "positiveTypeFamilyApplyAnnotation" typeFamilyApplyAnnotationCanonicalSourcePath typeFamilyApplyAnnotationExpectedProjectionPath
    , ParserParityPositiveCase "positive:gadt-result-constructor-spans" "positiveGadtResultConstructorSpans" gadtResultConstructorSpansCanonicalSourcePath gadtResultConstructorSpansExpectedProjectionPath
    , ParserParityPositiveCase "positive:existential-constructor-forall" "positiveExistentialConstructorForall" existentialConstructorForallCanonicalSourcePath existentialConstructorForallExpectedProjectionPath
    , ParserParityPositiveCase "positive:qualified-import-alias-references" "positiveQualifiedImportAliasReferences" qualifiedImportAliasReferencesCanonicalSourcePath qualifiedImportAliasReferencesExpectedProjectionPath
    , ParserParityPositiveCase "positive:qualified-import-alias-only" "positiveQualifiedImportAliasOnly" qualifiedImportAliasOnlyCanonicalSourcePath qualifiedImportAliasOnlyExpectedProjectionPath
    , ParserParityPositiveCase "positive:multi-module-abstract-export-import" "positiveMultiModuleAbstractExportImport" multiModuleAbstractExportImportCanonicalSourcePath multiModuleAbstractExportImportExpectedProjectionPath
    , ParserParityPositiveCase "positive:multi-module-recursive-adt-export-import" "positiveMultiModuleRecursiveAdtExportImport" multiModuleRecursiveAdtExportImportCanonicalSourcePath multiModuleRecursiveAdtExportImportExpectedProjectionPath
    , ParserParityPositiveCase "positive:text-literal-char-string" "positiveTextLiteralCharString" textLiteralCharStringCanonicalSourcePath textLiteralCharStringExpectedProjectionPath
    , ParserParityPositiveCase "positive:first-class-polymorphism-source-types" "positiveFirstClassPolymorphismSourceTypes" firstClassPolymorphismSourceTypesCanonicalSourcePath firstClassPolymorphismSourceTypesExpectedProjectionPath
    ]

parserParityNegativeCases :: [ParserParityNegativeCase]
parserParityNegativeCases =
    [ ParserParityNegativeCase "negative:import-exposing-def-bool" "negativeImportExposingDefBool" "import parser negative " importCanonicalSourcePath importNegativeSourceText importNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:value-def-list-int-ref" "negativeValueDefListIntRef" "value-def-list parser negative " valueDefListCanonicalSourcePath valueDefListNegativeSourceText valueDefListNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:let-lambda-application" "negativeLetLambdaApplication" "let-lambda-application parser negative " letLambdaApplicationCanonicalSourcePath letLambdaApplicationNegativeSourceText letLambdaApplicationNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:typed-annotation-types" "negativeTypedAnnotationTypes" "typed-annotation-types parser negative " typedAnnotationTypesCanonicalSourcePath typedAnnotationTypesNegativeSourceText typedAnnotationTypesNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:data-declaration-constructor-spans" "negativeDataDeclarationConstructorSpans" "data-declaration parser negative " dataDeclarationConstructorSpansCanonicalSourcePath dataDeclarationConstructorSpansNegativeSourceText dataDeclarationConstructorSpansNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:case-expression-constructor-patterns" "negativeCaseExpressionConstructorPatterns" "case-expression parser negative " caseExpressionConstructorPatternsCanonicalSourcePath caseExpressionNegativeSourceText caseExpressionNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:typeclass-instance-nullary-method" "negativeTypeclassInstanceNullaryMethod" "typeclass-instance parser negative " typeclassInstanceNullaryMethodCanonicalSourcePath typeclassInstanceNegativeSourceText typeclassInstanceNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:multiparam-superclass-fundep" "negativeMultiparamSuperclassFundep" "higher-kinded-fundep parser negative " multiparamSuperclassFundepCanonicalSourcePath higherKindedFundepNegativeSourceText higherKindedFundepNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:type-family-kind-lambda" "negativeTypeFamilyKindLambda" "type-family parser negative " typeFamilyKindLambdaCanonicalSourcePath typeFamilyEquationNegativeSourceText typeFamilyEquationNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:existential-constructor-forall" "negativeExistentialConstructorForall" "constructor-forall parser negative " existentialConstructorForallCanonicalSourcePath constructorForallNegativeSourceText constructorForallNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:qualified-import-alias-references" "negativeQualifiedImportAliasReferences" "qualified-import-alias parser negative " qualifiedImportAliasReferencesCanonicalSourcePath qualifiedImportAliasNegativeSourceText qualifiedImportAliasNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:multi-module-import-exposing-separator" "negativeMultiModuleImportExposingSeparator" "multi-module import-exposing parser negative " multiModuleAbstractExportImportCanonicalSourcePath importExposingSeparatorNegativeSourceText importExposingSeparatorNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:text-literal-malformed" "negativeTextLiteralMalformed" "text-literal parser negative " textLiteralCharStringCanonicalSourcePath textLiteralMalformedNegativeSourceText textLiteralMalformedNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:first-class-polymorphism-source-type" "negativeFirstClassPolymorphismSourceType" "first-class-polymorphism parser negative " firstClassPolymorphismSourceTypesCanonicalSourcePath firstClassPolymorphismSourceTypeNegativeSourceText firstClassPolymorphismSourceTypeNegativeEvidenceProjection
    ]

assertCanonicalParserParityProjection :: ParserParityPositiveCase -> IO ()
assertCanonicalParserParityProjection testCase = do
    source <- readFile (positiveCaseSourcePath testCase)
    expected <- readFile (positiveCaseExpectedPath testCase)
    canonicalProjection <- renderCanonicalProjection (positiveCaseSourcePath testCase) source
    canonicalProjection `shouldBe` expected

writeParserParityBatchPackage :: IO FilePath
writeParserParityBatchPackage = do
    loadedPositiveCases <- traverse loadPositiveParserParityCase parserParityPositiveCases
    removePathForcibly parserParityBatchPackageRoot
    createDirectoryIfMissing True parserParityBatchPackageRoot
    writeFile
        (parserParityBatchPackageRoot </> "Main.mlfp")
        (parserParityBatchMainSource loadedPositiveCases)
    pure parserParityBatchPackageRoot

loadPositiveParserParityCase :: ParserParityPositiveCase -> IO (ParserParityPositiveCase, String)
loadPositiveParserParityCase testCase = do
    sourceText <- readFile (positiveCaseSourcePath testCase)
    pure (testCase, sourceText)

parserParityBatchMainSource :: [(ParserParityPositiveCase, String)] -> String
parserParityBatchMainSource loadedPositiveCases =
    unlines $
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStr, stringAppend);"
        , "  import ParserParityParser exposing (renderParserParityProjectionFromSourceText, renderParserParityRetryEvidence, renderParserNegativeEvidenceFromSourceText);"
        , ""
        , "  def section : String -> String -> String ="
        , "    λ(label : String) λ(output : String)"
        , "      stringAppend \"== \" (stringAppend label (stringAppend \" ==\\n\" (stringAppend output \"\\n\")));"
        , ""
        ]
            ++ concatMap renderPositiveBatchDefinitions loadedPositiveCases
            ++ concatMap renderNegativeBatchDefinitions parserParityNegativeCases
            ++ renderRetryBatchDefinitions
            ++ [ "  def parserParityBatchOutput : String ="
               , "    " <> appendStringExpressions batchSectionNames <> ";"
               , ""
               , "  def main : IO Unit ="
               , "    putStr parserParityBatchOutput;"
               , "}"
               ]
  where
    batchSectionNames =
        map ((<> "Section") . positiveCaseIdentifier . fst) loadedPositiveCases
            ++ map ((<> "Section") . negativeCaseIdentifier) parserParityNegativeCases
            ++ [retryEvidenceIdentifier <> "Section"]

renderPositiveBatchDefinitions :: (ParserParityPositiveCase, String) -> [String]
renderPositiveBatchDefinitions (testCase, sourceText) =
    [ "  def " <> ident <> "SourceFile : String ="
    , "    " <> show (positiveCaseSourcePath testCase) <> ";"
    , ""
    , "  def " <> ident <> "SourceText : String ="
    , "    " <> show sourceText <> ";"
    , ""
    , "  def " <> ident <> "Section : String ="
    , "    section "
        <> show (positiveCaseLabel testCase)
        <> " (renderParserParityProjectionFromSourceText "
        <> ident
        <> "SourceFile "
        <> ident
        <> "SourceText);"
    , ""
    ]
  where
    ident = positiveCaseIdentifier testCase

renderNegativeBatchDefinitions :: ParserParityNegativeCase -> [String]
renderNegativeBatchDefinitions testCase =
    [ "  def " <> ident <> "SourceFile : String ="
    , "    " <> show (negativeCaseSourcePath testCase) <> ";"
    , ""
    , "  def " <> ident <> "SourceText : String ="
    , "    " <> show (negativeCaseSourceText testCase) <> ";"
    , ""
    , "  def " <> ident <> "Section : String ="
    , "    section "
        <> show (negativeCaseLabel testCase)
        <> " (renderParserNegativeEvidenceFromSourceText "
        <> show (negativeCasePrefix testCase)
        <> " "
        <> ident
        <> "SourceFile "
        <> ident
        <> "SourceText);"
    , ""
    ]
  where
    ident = negativeCaseIdentifier testCase

renderRetryBatchDefinitions :: [String]
renderRetryBatchDefinitions =
    [ "  def " <> retryEvidenceIdentifier <> "SourceFile : String ="
    , "    " <> show canonicalSourcePath <> ";"
    , ""
    , "  def " <> retryEvidenceIdentifier <> "SourceText : String ="
    , "    " <> show basicModuleSourceText <> ";"
    , ""
    , "  def " <> retryEvidenceIdentifier <> "LexerMismatchSourceText : String ="
    , "    " <> show lexerMismatchSourceText <> ";"
    , ""
    , "  def " <> retryEvidenceIdentifier <> "Section : String ="
    , "    section "
        <> show retryEvidenceLabel
        <> " (renderParserParityRetryEvidence "
        <> retryEvidenceIdentifier
        <> "SourceFile "
        <> retryEvidenceIdentifier
        <> "SourceText "
        <> retryEvidenceIdentifier
        <> "LexerMismatchSourceText);"
    , ""
    ]

appendStringExpressions :: [String] -> String
appendStringExpressions expressions =
    case expressions of
        [] -> "\"\""
        [expr] -> expr
        expr : rest -> "stringAppend " <> expr <> "\n      (" <> appendStringExpressions rest <> ")"

expectedParserParityBatchOutput :: IO String
expectedParserParityBatchOutput = do
    positiveSections <- traverse expectedPositiveParserParitySection parserParityPositiveCases
    let negativeSections =
            map expectedNegativeParserParitySection parserParityNegativeCases
    pure $
        concat
            ( positiveSections
                ++ negativeSections
                ++ [batchSection retryEvidenceLabel retryEvidenceProjection]
            )

expectedPositiveParserParitySection :: ParserParityPositiveCase -> IO String
expectedPositiveParserParitySection testCase =
    batchSection (positiveCaseLabel testCase)
        <$> readFile (positiveCaseExpectedPath testCase)

expectedNegativeParserParitySection :: ParserParityNegativeCase -> String
expectedNegativeParserParitySection testCase =
    batchSection (negativeCaseLabel testCase) (negativeCaseExpected testCase)

batchSection :: String -> String -> String
batchSection label output =
    "== " <> label <> " ==\n" <> output

retryEvidenceLabel :: String
retryEvidenceLabel =
    "retry:basic-module-def-bool"

retryEvidenceIdentifier :: String
retryEvidenceIdentifier =
    "retryBasicModuleDefBool"

basicModuleSourceText :: String
basicModuleSourceText =
    unlines
        [ "module Main export (main) {"
        , "  def main : Bool = true;"
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

typeFamilyEquationNegativeSourceText :: String
typeFamilyEquationNegativeSourceText =
    unlines
        [ "module Main {"
        , "  type family Normalize (a :: k) :: k where {"
        , "    Normalize Int Int;"
        , "  }"
        , "}"
        ]

constructorForallNegativeSourceText :: String
constructorForallNegativeSourceText =
    unlines
        [ "module Main export (Nat(..), Expr(..), SomeExpr(..), unwrapSome, main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  data Expr a ="
        , "      DoneNat : Nat -> Expr Nat"
        , "    | Step : Expr a -> Expr a;"
        , ""
        , "  data SomeExpr ="
        , "      SomeExpr : ∀ a Expr a -> SomeExpr;"
        , ""
        , "  def unwrapSome : SomeExpr -> Bool = λ(boxed) case boxed of {"
        , "    SomeExpr expr -> case expr of {"
        , "      DoneNat _ -> true;"
        , "      Step next -> unwrapSome (SomeExpr next)"
        , "    }"
        , "  };"
        , ""
        , "  def main : Bool = unwrapSome (SomeExpr (Step (DoneNat Zero)));"
        , "}"
        ]

qualifiedImportAliasNegativeSourceText :: String
qualifiedImportAliasNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Core as exposing (answer);"
        , "  def main : Bool = true;"
        , "}"
        ]

importExposingSeparatorNegativeSourceText :: String
importExposingSeparatorNegativeSourceText =
    unlines
        [ ""
        , ""
        , ""
        , ""
        , ""
        , ""
        , ""
        , ""
        , ""
        , ""
        , "module User export (main) {"
        , "  import Core exposing (Nat zero, succ);"
        , ""
        , "  def main : Nat = succ zero;"
        , "}"
        ]

textLiteralMalformedNegativeSourceText :: String
textLiteralMalformedNegativeSourceText =
    unlines
        [ "module Main export (sampleString) {"
        , "  import Prelude exposing (String);"
        , "  def sampleString : String = \"unterminated λ;"
        , "}"
        ]

firstClassPolymorphismSourceTypeNegativeSourceText :: String
firstClassPolymorphismSourceTypeNegativeSourceText =
    unlines
        [ "module FirstClassPolymorphism export (usePoly, id, main) {"
        , "  def usePoly : (∀ a. a -> a) -> Bool ="
        , "    λ(poly : ∀ a. a -> a) let x = poly 1 in poly true;"
        , ""
        , "  def id : ∀ a a -> a = λx x;"
        , ""
        , "  def main : Bool = usePoly id;"
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
        [ "import parser negative expected-import-semicolon@test/conformance/mlfp/parser-parity/import-exposing-def-bool/src/Main.mlfp:3:3-3:6"
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

typeFamilyEquationNegativeEvidenceProjection :: String
typeFamilyEquationNegativeEvidenceProjection =
    unlines
        [ "type-family parser negative expected-type-family-equation-equals@test/conformance/mlfp/parser-parity/type-family-kind-lambda/src/Main.mlfp:3:19-3:22"
        ]

constructorForallNegativeEvidenceProjection :: String
constructorForallNegativeEvidenceProjection =
    unlines
        [ "constructor-forall parser negative expected-constructor-forall-dot@test/conformance/mlfp/parser-parity/existential-constructor-forall/src/Main.mlfp:11:22-11:26"
        ]

qualifiedImportAliasNegativeEvidenceProjection :: String
qualifiedImportAliasNegativeEvidenceProjection =
    unlines
        [ "qualified-import-alias parser negative expected-import-alias@test/conformance/mlfp/parser-parity/qualified-import-alias-references/src/Main.mlfp:2:18-2:26"
        ]

importExposingSeparatorNegativeEvidenceProjection :: String
importExposingSeparatorNegativeEvidenceProjection =
    unlines
        [ "multi-module import-exposing parser negative expected-import-exposing-separator@test/conformance/mlfp/parser-parity/multi-module-abstract-export-import/src/Main.mlfp:12:29-12:33"
        ]

textLiteralMalformedNegativeEvidenceProjection :: String
textLiteralMalformedNegativeEvidenceProjection =
    unlines
        [ "text-literal parser negative unexpected-source@test/conformance/mlfp/parser-parity/text-literal-char-string/src/Main.mlfp:3:47"
        ]

firstClassPolymorphismSourceTypeNegativeEvidenceProjection :: String
firstClassPolymorphismSourceTypeNegativeEvidenceProjection =
    unlines
        [ concat
            [ "first-class-polymorphism parser negative "
            , "expected-constructor-forall-dot@"
            , "test/conformance/mlfp/parser-parity/first-class-polymorphism-source-types/src/Main.mlfp:5:16-5:17"
            ]
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
        P.Program modules0 ->
            concat <$> traverse (renderModuleProjection (P.locatedProgramSpans located)) modules0

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
    concat <$> traverse (renderImportProjection spans) imports

renderImportProjection :: P.ProgramSpanIndex -> P.Import -> IO [String]
renderImportProjection spans import0 = do
    importSpan <- requireListSpan "import" (P.importModuleName import0) (P.spanImports spans)
    aliasProjection <- renderImportAliasProjection spans (P.importAlias import0)
    exposingProjections <- renderImportExposingProjections spans (P.importExposing import0)
    pure $
        [ "import "
            ++ P.importModuleName import0
            ++ " span="
            ++ renderSpan importSpan
        ]
            ++ aliasProjection
            ++ exposingProjections

renderImportAliasProjection :: P.ProgramSpanIndex -> Maybe String -> IO [String]
renderImportAliasProjection spans alias =
    case alias of
        Nothing -> pure []
        Just aliasName -> do
            aliasSpan <- requireListSpan "import alias" aliasName (P.spanImportAliases spans)
            pure
                [ "import alias "
                    ++ aliasName
                    ++ " span="
                    ++ renderSpan aliasSpan
                ]

renderImportExposingProjections :: P.ProgramSpanIndex -> Maybe [P.ExportItem] -> IO [String]
renderImportExposingProjections spans exposing =
    case exposing of
        Nothing -> pure []
        Just items -> traverse (renderImportExposingProjection spans) items

renderImportExposingProjection :: P.ProgramSpanIndex -> P.ExportItem -> IO String
renderImportExposingProjection spans item = do
    let name = P.exportItemName item
    exposingSpan <- requireListSpan "import exposing item" name (P.spanImportItems spans)
    pure $
        "import exposing "
            ++ renderExportKind item
            ++ " "
            ++ name
            ++ " span="
            ++ renderSpan exposingSpan

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
        P.DeclTypeFamily family0 -> renderTypeFamilyProjection spans family0
        P.DeclData data0 -> renderDataProjection spans data0
        P.DeclDef def0 -> do
            defSpan <- requireListSpan "definition" (P.defDeclName def0) (P.spanValues spans)
            pure
                [ "def "
                    ++ P.defDeclName def0
                    ++ " type="
                    ++ renderConstrainedType (P.defDeclType def0)
                    ++ " expr="
                    ++ renderExpr (P.defDeclExpr def0)
                    ++ " span="
                    ++ renderSpan defSpan
                ]

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

renderTypeFamilyProjection :: P.ProgramSpanIndex -> TypeFamilyDecl -> IO [String]
renderTypeFamilyProjection spans family0 = do
    familySpan <- requireListSpan "type-family declaration" (familyDeclName family0) (P.spanTypes spans)
    pure $
        [ "type-family "
            ++ familyDeclName family0
            ++ renderTypeFamilyParams (familyDeclParams family0)
            ++ " result="
            ++ renderTypeLevelKind (familyDeclResultKind family0)
            ++ " span="
            ++ renderSpan familySpan
        ]
            ++ map (renderTypeFamilyEquationProjection (familyDeclName family0)) (familyDeclEquations family0)

renderTypeFamilyParams :: [(String, TypeLevelKind)] -> String
renderTypeFamilyParams params =
    case params of
        [] -> ""
        _ -> " params=" ++ intercalate "," (map renderTypeFamilyParam params)

renderTypeFamilyParam :: (String, TypeLevelKind) -> String
renderTypeFamilyParam (name, kind)
    | kind == TLKType = name
    | otherwise = name ++ "::" ++ renderTypeLevelKind kind

renderTypeFamilyEquationProjection :: String -> TypeFamilyEquation -> String
renderTypeFamilyEquationProjection familyName equation =
    "family-equation "
        ++ familyName
        ++ " patterns="
        ++ intercalate "," (map renderTypeLevelPattern (familyEquationPatterns equation))
        ++ " rhs="
        ++ renderTypeLevelType (familyEquationRhs equation)

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
        STTyLam name body ->
            parenthesizeIf (precedence > 0) $
                "Λ" ++ name ++ ". " ++ renderSrcTypePrec 0 body
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

renderTypeLevelKind :: TypeLevelKind -> String
renderTypeLevelKind =
    renderTypeLevelKindPrec 0

renderTypeLevelKindPrec :: Int -> TypeLevelKind -> String
renderTypeLevelKindPrec precedence kind =
    case kind of
        TLKType -> "*"
        TLKVar name -> name
        TLKArrow left right ->
            parenthesizeIf (precedence > 0) $
                renderTypeLevelKindPrec 1 left ++ " -> " ++ renderTypeLevelKindPrec 0 right

renderTypeLevelType :: TypeLevelTy -> String
renderTypeLevelType =
    renderTypeLevelTypePrec 0

renderTypeLevelTypePrec :: Int -> TypeLevelTy -> String
renderTypeLevelTypePrec precedence ty =
    case ty of
        TLTVar name -> name
        TLTCon name -> name
        TLTArrow dom cod ->
            parenthesizeIf (precedence > 1) $
                renderTypeLevelTypePrec 2 dom ++ " -> " ++ renderTypeLevelTypePrec 1 cod
        TLTLam name kind body ->
            let (binders, tailBody) = collectTypeLevelLams body
                binderText =
                    unwords (renderTypeLevelLamBinder (name, kind) : map renderTypeLevelLamBinder binders)
             in parenthesizeIf (precedence > 0) $
                    "Λ" ++ binderText ++ ". " ++ renderTypeLevelTypePrec 0 tailBody
        TLTApp fun arg ->
            parenthesizeIf (precedence > 2) $
                renderTypeLevelTypePrec 2 fun ++ " " ++ renderTypeLevelTypeArg arg
        TLTFamilyApp name args ->
            parenthesizeIf (precedence > 2) $
                unwords (name : map renderTypeLevelTypeArg args)

renderTypeLevelTypeArg :: TypeLevelTy -> String
renderTypeLevelTypeArg ty =
    case ty of
        TLTVar {} -> renderTypeLevelTypePrec 3 ty
        TLTCon {} -> renderTypeLevelTypePrec 3 ty
        _ -> "(" ++ renderTypeLevelTypePrec 0 ty ++ ")"

renderTypeLevelLamBinder :: (String, TypeLevelKind) -> String
renderTypeLevelLamBinder (name, kind)
    | kind == TLKType = name
    | otherwise = "(" ++ name ++ " :: " ++ renderTypeLevelKind kind ++ ")"

collectTypeLevelLams :: TypeLevelTy -> ([(String, TypeLevelKind)], TypeLevelTy)
collectTypeLevelLams ty =
    case ty of
        TLTLam name kind body ->
            let (rest, tailBody) = collectTypeLevelLams body
             in ((name, kind) : rest, tailBody)
        _ -> ([], ty)

renderTypeLevelPattern :: TypeLevelPattern -> String
renderTypeLevelPattern pat =
    case pat of
        TLPVar name -> name
        TLPCon name patterns -> unwords (name : map renderTypeLevelPatternArg patterns)

renderTypeLevelPatternArg :: TypeLevelPattern -> String
renderTypeLevelPatternArg pat =
    case pat of
        TLPVar {} -> renderTypeLevelPattern pat
        TLPCon _ [] -> renderTypeLevelPattern pat
        _ -> "(" ++ renderTypeLevelPattern pat ++ ")"

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
        P.ELit (LChar value) -> renderCharLiteral value
        P.ELit (LString value) -> renderStringLiteral value
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

renderCharLiteral :: Char -> String
renderCharLiteral value =
    "'" ++ renderLiteralChar value ++ "'"

renderStringLiteral :: String -> String
renderStringLiteral value =
    "\"" ++ concatMap renderLiteralChar value ++ "\""

renderLiteralChar :: Char -> String
renderLiteralChar value =
    case value of
        '\'' -> "\\'"
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\t' -> "\\t"
        _ -> [value]

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
