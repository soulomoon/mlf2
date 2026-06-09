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

        it "shared parser-owned .mlfp parser parses higher-order partial applications" $ do
            source <- readFile higherOrderPartialApplicationCanonicalSourcePath
            expected <- readFile higherOrderPartialApplicationExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection higherOrderPartialApplicationCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch higherOrderPartialApplicationParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses higher-order local function flow" $ do
            source <- readFile higherOrderLocalFunctionFlowCanonicalSourcePath
            expected <- readFile higherOrderLocalFunctionFlowExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection higherOrderLocalFunctionFlowCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch higherOrderLocalFunctionFlowParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses higher-order returned functions" $ do
            source <- readFile higherOrderReturnedFunctionCanonicalSourcePath
            expected <- readFile higherOrderReturnedFunctionExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection higherOrderReturnedFunctionCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch higherOrderReturnedFunctionParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses higher-order function fields" $ do
            source <- readFile higherOrderFunctionFieldCanonicalSourcePath
            expected <- readFile higherOrderFunctionFieldExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection higherOrderFunctionFieldCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch higherOrderFunctionFieldParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses authoritative recursive let flows" $ do
            source <- readFile authoritativeRecursiveLetCanonicalSourcePath
            expected <- readFile authoritativeRecursiveLetExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection authoritativeRecursiveLetCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch authoritativeRecursiveLetParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses authoritative cross-module let polymorphism" $ do
            source <- readFile authoritativeCrossModuleLetPolymorphismCanonicalSourcePath
            expected <- readFile authoritativeCrossModuleLetPolymorphismExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection authoritativeCrossModuleLetPolymorphismCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch authoritativeCrossModuleLetPolymorphismParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses authoritative unified case analysis" $
            assertSharedParserParityProjection
                authoritativeCaseAnalysisCanonicalSourcePath
                authoritativeCaseAnalysisExpectedProjectionPath
                authoritativeCaseAnalysisParserProgramRoot

        it "shared parser-owned .mlfp parser parses importless authoritative unified let polymorphism" $
            assertSharedParserParityProjection
                authoritativeLetPolymorphismCanonicalSourcePath
                authoritativeLetPolymorphismExpectedProjectionPath
                authoritativeLetPolymorphismParserProgramRoot

        it "shared parser-owned .mlfp parser parses authoritative unified nullary overloaded methods" $
            assertSharedParserParityProjection
                authoritativeNullaryOverloadedMethodCanonicalSourcePath
                authoritativeNullaryOverloadedMethodExpectedProjectionPath
                authoritativeNullaryOverloadedMethodParserProgramRoot

        it "shared parser-owned .mlfp parser parses authoritative unified overloaded methods" $
            assertSharedParserParityProjection
                authoritativeOverloadedMethodCanonicalSourcePath
                authoritativeOverloadedMethodExpectedProjectionPath
                authoritativeOverloadedMethodParserProgramRoot

        it "shared parser-owned .mlfp parser parses recursive ADT plain Nat" $ do
            source <- readFile recursiveAdtPlainNatCanonicalSourcePath
            expected <- readFile recursiveAdtPlainNatExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection recursiveAdtPlainNatCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch recursiveAdtPlainNatParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses recursive list tail" $ do
            source <- readFile recursiveListTailCanonicalSourcePath
            expected <- readFile recursiveListTailExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection recursiveListTailCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch recursiveListTailParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses recursive tree first-order programs" $ do
            source <- readFile recursiveTreeFirstOrderCanonicalSourcePath
            expected <- readFile recursiveTreeFirstOrderExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection recursiveTreeFirstOrderCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch recursiveTreeFirstOrderParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses recursive tree deriving programs" $ do
            source <- readFile recursiveTreeDerivingCanonicalSourcePath
            expected <- readFile recursiveTreeDerivingExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection recursiveTreeDerivingCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch recursiveTreeDerivingParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses recursive ADT typeclass integration" $ do
            source <- readFile typeclassIntegrationCanonicalSourcePath
            expected <- readFile typeclassIntegrationExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection typeclassIntegrationCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch typeclassIntegrationParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses abstract recursive ADT module use" $ do
            source <- readFile abstractRecursiveAdtModuleUseCanonicalSourcePath
            expected <- readFile abstractRecursiveAdtModuleUseExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection abstractRecursiveAdtModuleUseCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch abstractRecursiveAdtModuleUseParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses module-integrated recursive existential programs" $ do
            source <- readFile moduleIntegratedRecursiveExistentialCanonicalSourcePath
            expected <- readFile moduleIntegratedRecursiveExistentialExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection moduleIntegratedRecursiveExistentialCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch moduleIntegratedRecursiveExistentialParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses complex recursive programs" $ do
            source <- readFile complexRecursiveProgramCanonicalSourcePath
            expected <- readFile complexRecursiveProgramExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection complexRecursiveProgramCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch complexRecursiveProgramParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses named deriving Eq recursive ADTs" $ do
            source <- readFile derivingEqCanonicalSourcePath
            expected <- readFile derivingEqExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection derivingEqCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch derivingEqParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses named recursive GADT source modules" $ do
            source <- readFile recursiveGadtCanonicalSourcePath
            expected <- readFile recursiveGadtExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection recursiveGadtCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch recursiveGadtParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses named recursive existential source modules" $ do
            source <- readFile recursiveExistentialCanonicalSourcePath
            expected <- readFile recursiveExistentialExpectedProjectionPath
            canonicalProjection <- renderCanonicalProjection recursiveExistentialCanonicalSourcePath source
            sharedParserProjection <- runSharedParserBatch recursiveExistentialParserProgramRoot

            canonicalProjection `shouldBe` expected
            sharedParserProjection `shouldBe` Right expected

        it "shared parser-owned .mlfp parser parses same-root package source layout" $
            assertSharedPackageParserParityProjection
                packageCrossModuleLetSourcePaths
                packageCrossModuleLetExpectedProjectionPath
                packageCrossModuleLetParserProgramRoot

        it "shared parser-owned .mlfp parser parses ordered search-path package source layout" $
            assertSharedPackageParserParityProjection
                packageSearchPathImportSourcePaths
                packageSearchPathImportExpectedProjectionPath
                packageSearchPathImportParserProgramRoot

        it "shared parser-owned .mlfp parser parses compiler-seed data-model package sources" $
            assertSharedPackageParserParityProjection
                compilerSeedDataModelSourcePaths
                compilerSeedDataModelExpectedProjectionPath
                compilerSeedDataModelParserProgramRoot

        it "shared parser-owned .mlfp parser parses compiler-seed lexer source" $
            assertSharedParserParityProjection
                compilerSeedLexerSourcePath
                compilerSeedLexerExpectedProjectionPath
                compilerSeedLexerParserProgramRoot

        it "compiler-seed data-model parser-parity sources copy selected seed modules" $
            traverse_ assertSourceCopy compilerSeedDataModelSourceCopyPairs

        it "compiler-seed lexer parser-parity source copies the selected seed module" $
            assertSourceCopy (compilerSeedLexerOriginalPath, compilerSeedLexerSourcePath)

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

            it "parser-owned .mlfp parser reports malformed higher-order partial-application diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:higher-order-partial-application" higherOrderPartialApplicationNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed higher-order local function diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:higher-order-local-function-flow" higherOrderLocalFunctionFlowNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed higher-order returned-function diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:higher-order-returned-function" higherOrderReturnedFunctionNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed higher-order function-field diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:higher-order-function-field" higherOrderFunctionFieldNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed authoritative recursive-let diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:authoritative-recursive-let" authoritativeRecursiveLetNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed authoritative cross-module let-polymorphism diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:authoritative-cross-module-let-polymorphism" authoritativeCrossModuleLetPolymorphismNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes authoritative unified exact source fixtures through the generated public CLI driver" $ \fixture -> do
                caseAnalysisExpected <- readFile authoritativeCaseAnalysisExpectedProjectionPath
                letPolymorphismExpected <- readFile authoritativeLetPolymorphismExpectedProjectionPath
                nullaryOverloadedExpected <- readFile authoritativeNullaryOverloadedMethodExpectedProjectionPath
                overloadedExpected <- readFile authoritativeOverloadedMethodExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:authoritative-case-analysis" caseAnalysisExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:authoritative-let-polymorphism" letPolymorphismExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:authoritative-nullary-overloaded-method" nullaryOverloadedExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:authoritative-overloaded-method" overloadedExpected)

            it "parser-owned .mlfp parser reports malformed authoritative unified diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:authoritative-unified-let-polymorphism" authoritativeUnifiedLetPolymorphismNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed recursive ADT plain Nat diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:recursive-adt-plain-nat" recursiveAdtPlainNatNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed recursive list tail diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:recursive-list-tail" recursiveListTailNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes recursive tree fixtures through the generated public CLI driver" $ \fixture -> do
                firstOrderExpected <- readFile recursiveTreeFirstOrderExpectedProjectionPath
                derivingExpected <- readFile recursiveTreeDerivingExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:recursive-tree-first-order" firstOrderExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:recursive-tree-deriving" derivingExpected)

            it "parser-owned .mlfp parser reports malformed recursive tree diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:recursive-tree-branch-arrow" recursiveTreeNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes recursive ADT typeclass integration through the generated public CLI driver" $ \fixture -> do
                expected <- readFile typeclassIntegrationExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:typeclass-integration" expected)

            it "parser-owned .mlfp parser reports malformed recursive ADT typeclass integration diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:typeclass-integration-nested-case" typeclassIntegrationNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes abstract recursive ADT module use through the generated public CLI driver" $ \fixture -> do
                expected <- readFile abstractRecursiveAdtModuleUseExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:abstract-recursive-adt-module-use" expected)

            it "parser-owned .mlfp parser reports malformed abstract recursive ADT module use diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:abstract-recursive-adt-module-use" abstractRecursiveAdtModuleUseNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes module-integrated recursive existential source through the generated public CLI driver" $ \fixture -> do
                expected <- readFile moduleIntegratedRecursiveExistentialExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:module-integrated-recursive-existential" expected)

            it "parser-owned .mlfp parser reports malformed module-integrated recursive existential diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:module-integrated-recursive-existential" moduleIntegratedRecursiveExistentialNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes complex recursive programs through the generated public CLI driver" $ \fixture -> do
                expected <- readFile complexRecursiveProgramExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:complex-recursive-program" expected)

            it "parser-owned .mlfp parser reports malformed complex recursive program diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:complex-recursive-program" complexRecursiveProgramNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes named recursive ADT source modules through the generated public CLI driver" $ \fixture -> do
                derivingExpected <- readFile derivingEqExpectedProjectionPath
                gadtExpected <- readFile recursiveGadtExpectedProjectionPath
                existentialExpected <- readFile recursiveExistentialExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:deriving-eq" derivingExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:recursive-gadt" gadtExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:recursive-existential" existentialExpected)

            it "parser-owned .mlfp parser reports malformed named recursive ADT diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:named-recursive-adt-case-branch" namedRecursiveAdtNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes package source-layout fixtures through the generated public CLI driver" $ \fixture -> do
                crossModuleLetExpected <- readFile packageCrossModuleLetExpectedProjectionPath
                searchPathImportExpected <- readFile packageSearchPathImportExpectedProjectionPath
                compilerSeedDataModelExpected <- readFile compilerSeedDataModelExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:package-cross-module-let" crossModuleLetExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:package-search-path-import" searchPathImportExpected)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:compiler-seed-data-model" compilerSeedDataModelExpected)

            it "parser-owned .mlfp parser reports malformed package-layout import diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:package-cross-module-let-import-semicolon" packageLayoutImportSemicolonNegativeEvidenceProjection)

            it "parser-owned .mlfp parser reports malformed compiler-seed data-model diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:compiler-seed-data-model-case-branch" compilerSeedDataModelCaseBranchNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser routes compiler-seed lexer through the generated public CLI driver" $ \fixture -> do
                compilerSeedLexerExpected <- readFile compilerSeedLexerExpectedProjectionPath

                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "positive:compiler-seed-lexer" compilerSeedLexerExpected)

            it "parser-owned .mlfp parser reports malformed compiler-seed lexer diagnostics through public run-program" $ \fixture -> do
                batchRunResult fixture `shouldBe` Right (batchExpectedOutput fixture)
                batchExpectedOutput fixture
                    `shouldSatisfy` isInfixOf
                        (batchSection "negative:compiler-seed-lexer-case-branch" compilerSeedLexerCaseBranchNegativeEvidenceProjection)

            it "shared parser-owned .mlfp parser library routes the generated batch through one entrypoint" $ \fixture -> do
                sharedParserExists <- doesFileExist (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
                sharedParserExists `shouldBe` True

                let batchSource = batchMainSource fixture
                batchSource `shouldSatisfy` isInfixOf "import ParserParityParser exposing"
                batchSource `shouldSatisfy` isInfixOf "renderParserParityProjectionFromSourceText"
                batchSource `shouldSatisfy` isInfixOf "renderParserParityPackageProjectionFromSourceTexts"
                batchSource `shouldSatisfy` isInfixOf "renderParserParityPackageProjectionFromFourSourceTexts"
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

        it "shared parser-owned .mlfp parser centralizes diagnostic expectations" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedExpectationAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedCombinatorSource)) sharedParserExpectationSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserExpectationUsePhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares bounded projection row lists" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedProjectionListAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedProjectionRowsSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedProjectionRowsUsePhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares bounded case branch rows" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedCaseBranchAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedCaseBranchRowsSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedCaseBranchRowsUsePhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares bounded application arguments" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedApplicationArgumentAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedApplicationArgumentsSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedApplicationArgumentsUsePhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares nested parenthesized application depth handling" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")
            sharedSpecSource <- readFile "test/ProgramParserParitySpec.hs"

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                staticGuardSource = parserLibrarySource <> "\n" <> sharedSpecSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedNestedParenthesizedApplicationAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserNestedParenthesizedApplicationSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserNestedParenthesizedApplicationUsePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` staticGuardSource)) sharedParserNestedParenthesizedApplicationGuardPhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares bounded annotated lambda RHS depth handling" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")
            sharedSpecSource <- readFile "test/ProgramParserParitySpec.hs"

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                staticGuardSource = parserLibrarySource <> "\n" <> sharedSpecSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedAnnotatedLambdaRhsAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserAnnotatedLambdaRhsSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserAnnotatedLambdaRhsUsePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` staticGuardSource)) sharedParserAnnotatedLambdaRhsGuardPhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares bounded source-type arrow-tail text accumulation" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")
            sharedSpecSource <- readFile "test/ProgramParserParitySpec.hs"

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                staticGuardSource = parserLibrarySource <> "\n" <> sharedSpecSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedSourceTypeArrowTailAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserSourceTypeArrowTailSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserSourceTypeArrowTailUsePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` staticGuardSource)) sharedParserSourceTypeArrowTailGuardPhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares constructor row accumulation" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedCombinatorSource <- readFile (sharedParserLibraryRoot </> "ParserParityParserCombinator.mlfp")
            sharedSpecSource <- readFile "test/ProgramParserParitySpec.hs"

            let parserLibrarySource = sharedParserSource <> "\n" <> sharedCombinatorSource
                staticGuardSource = parserLibrarySource <> "\n" <> sharedSpecSource
                removedMatches =
                    filter
                        (`isInfixOf` parserLibrarySource)
                        sharedParserRemovedConstructorRowAccumulatorAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` parserLibrarySource)) sharedParserConstructorRowAccumulatorSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserConstructorRowAccumulatorUsePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` staticGuardSource)) sharedParserConstructorRowAccumulatorGuardPhrases
            removedMatches `shouldBe` []

        it "shared parser-owned .mlfp parser shares bounded source-definition row sequencing" $ do
            sharedParserSource <- readFile (sharedParserLibraryRoot </> "ParserParityParser.mlfp")
            sharedSpecSource <- readFile "test/ProgramParserParitySpec.hs"

            let staticGuardSource = sharedParserSource <> "\n" <> sharedSpecSource
                removedMatches =
                    filter
                        (`isInfixOf` sharedParserSource)
                        sharedParserRemovedSourceDefinitionRowSequenceAliases

            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedSourceDefinitionRowSequenceSubstratePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` sharedParserSource)) sharedParserBoundedSourceDefinitionRowSequenceUsePhrases
            traverse_ (`shouldSatisfy` (`isInfixOf` staticGuardSource)) sharedParserSourceDefinitionRowSequenceGuardPhrases
            removedMatches `shouldBe` []

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

derivingEqCanonicalSourcePath :: FilePath
derivingEqCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/deriving-eq/src/Main.mlfp"

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

recursiveGadtCanonicalSourcePath :: FilePath
recursiveGadtCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/recursive-gadt/src/Main.mlfp"

recursiveExistentialCanonicalSourcePath :: FilePath
recursiveExistentialCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/recursive-existential/src/Main.mlfp"

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

higherOrderPartialApplicationCanonicalSourcePath :: FilePath
higherOrderPartialApplicationCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/higher-order-partial-application/src/Main.mlfp"

higherOrderLocalFunctionFlowCanonicalSourcePath :: FilePath
higherOrderLocalFunctionFlowCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/higher-order-local-function-flow/src/Main.mlfp"

higherOrderReturnedFunctionCanonicalSourcePath :: FilePath
higherOrderReturnedFunctionCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/higher-order-returned-function/src/Main.mlfp"

higherOrderFunctionFieldCanonicalSourcePath :: FilePath
higherOrderFunctionFieldCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/higher-order-function-field/src/Main.mlfp"

authoritativeRecursiveLetCanonicalSourcePath :: FilePath
authoritativeRecursiveLetCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/authoritative-recursive-let/src/Main.mlfp"

authoritativeCrossModuleLetPolymorphismCanonicalSourcePath :: FilePath
authoritativeCrossModuleLetPolymorphismCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/src/Main.mlfp"

authoritativeCaseAnalysisCanonicalSourcePath :: FilePath
authoritativeCaseAnalysisCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/authoritative-case-analysis/src/Main.mlfp"

authoritativeLetPolymorphismCanonicalSourcePath :: FilePath
authoritativeLetPolymorphismCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/authoritative-let-polymorphism/src/Main.mlfp"

authoritativeNullaryOverloadedMethodCanonicalSourcePath :: FilePath
authoritativeNullaryOverloadedMethodCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/authoritative-nullary-overloaded-method/src/Main.mlfp"

authoritativeOverloadedMethodCanonicalSourcePath :: FilePath
authoritativeOverloadedMethodCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/authoritative-overloaded-method/src/Main.mlfp"

recursiveAdtPlainNatCanonicalSourcePath :: FilePath
recursiveAdtPlainNatCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/src/Main.mlfp"

recursiveListTailCanonicalSourcePath :: FilePath
recursiveListTailCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/recursive-list-tail/src/Main.mlfp"

recursiveTreeFirstOrderCanonicalSourcePath :: FilePath
recursiveTreeFirstOrderCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/recursive-tree-first-order/src/Main.mlfp"

recursiveTreeDerivingCanonicalSourcePath :: FilePath
recursiveTreeDerivingCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/recursive-tree-deriving/src/Main.mlfp"

typeclassIntegrationCanonicalSourcePath :: FilePath
typeclassIntegrationCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/typeclass-integration/src/Main.mlfp"

abstractRecursiveAdtModuleUseCanonicalSourcePath :: FilePath
abstractRecursiveAdtModuleUseCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/src/Main.mlfp"

moduleIntegratedRecursiveExistentialCanonicalSourcePath :: FilePath
moduleIntegratedRecursiveExistentialCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/module-integrated-recursive-existential/src/Main.mlfp"

complexRecursiveProgramCanonicalSourcePath :: FilePath
complexRecursiveProgramCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/complex-recursive-program/src/Main.mlfp"

packageCrossModuleLetCoreSourcePath :: FilePath
packageCrossModuleLetCoreSourcePath =
    "test/conformance/mlfp/parser-parity/package-cross-module-let/src/Core.mlfp"

packageCrossModuleLetMainSourcePath :: FilePath
packageCrossModuleLetMainSourcePath =
    "test/conformance/mlfp/parser-parity/package-cross-module-let/src/Main.mlfp"

packageSearchPathImportLibSourcePath :: FilePath
packageSearchPathImportLibSourcePath =
    "test/conformance/mlfp/parser-parity/package-search-path-import/roots/lib/SearchLib.mlfp"

packageSearchPathImportMainSourcePath :: FilePath
packageSearchPathImportMainSourcePath =
    "test/conformance/mlfp/parser-parity/package-search-path-import/roots/main/Main.mlfp"

compilerSeedDataModelSeedSourceOriginalPath :: FilePath
compilerSeedDataModelSeedSourceOriginalPath =
    "test/programs/compiler-seed/frontend-contract/SeedSource.mlfp"

compilerSeedDataModelSeedTokenOriginalPath :: FilePath
compilerSeedDataModelSeedTokenOriginalPath =
    "test/programs/compiler-seed/frontend-contract/SeedToken.mlfp"

compilerSeedDataModelSeedDiagnosticOriginalPath :: FilePath
compilerSeedDataModelSeedDiagnosticOriginalPath =
    "test/programs/compiler-seed/frontend-contract/SeedDiagnostic.mlfp"

compilerSeedDataModelSeedAstOriginalPath :: FilePath
compilerSeedDataModelSeedAstOriginalPath =
    "test/programs/compiler-seed/frontend-contract/SeedAst.mlfp"

compilerSeedLexerOriginalPath :: FilePath
compilerSeedLexerOriginalPath =
    "test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp"

compilerSeedDataModelSeedSourceSourcePath :: FilePath
compilerSeedDataModelSeedSourceSourcePath =
    "test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedSource.mlfp"

compilerSeedDataModelSeedTokenSourcePath :: FilePath
compilerSeedDataModelSeedTokenSourcePath =
    "test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedToken.mlfp"

compilerSeedDataModelSeedDiagnosticSourcePath :: FilePath
compilerSeedDataModelSeedDiagnosticSourcePath =
    "test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedDiagnostic.mlfp"

compilerSeedDataModelSeedAstSourcePath :: FilePath
compilerSeedDataModelSeedAstSourcePath =
    "test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedAst.mlfp"

compilerSeedLexerSourcePath :: FilePath
compilerSeedLexerSourcePath =
    "test/conformance/mlfp/parser-parity/compiler-seed-lexer/src/SeedLexer.mlfp"

packageCrossModuleLetSourcePaths :: [FilePath]
packageCrossModuleLetSourcePaths =
    [ packageCrossModuleLetCoreSourcePath
    , packageCrossModuleLetMainSourcePath
    ]

packageSearchPathImportSourcePaths :: [FilePath]
packageSearchPathImportSourcePaths =
    [ packageSearchPathImportLibSourcePath
    , packageSearchPathImportMainSourcePath
    ]

compilerSeedDataModelSourcePaths :: [FilePath]
compilerSeedDataModelSourcePaths =
    [ compilerSeedDataModelSeedSourceSourcePath
    , compilerSeedDataModelSeedTokenSourcePath
    , compilerSeedDataModelSeedDiagnosticSourcePath
    , compilerSeedDataModelSeedAstSourcePath
    ]

compilerSeedDataModelSourceCopyPairs :: [(FilePath, FilePath)]
compilerSeedDataModelSourceCopyPairs =
    [ (compilerSeedDataModelSeedSourceOriginalPath, compilerSeedDataModelSeedSourceSourcePath)
    , (compilerSeedDataModelSeedTokenOriginalPath, compilerSeedDataModelSeedTokenSourcePath)
    , (compilerSeedDataModelSeedDiagnosticOriginalPath, compilerSeedDataModelSeedDiagnosticSourcePath)
    , (compilerSeedDataModelSeedAstOriginalPath, compilerSeedDataModelSeedAstSourcePath)
    ]

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

derivingEqExpectedProjectionPath :: FilePath
derivingEqExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/deriving-eq/expected/parser-program.txt"

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

recursiveGadtExpectedProjectionPath :: FilePath
recursiveGadtExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/recursive-gadt/expected/parser-program.txt"

recursiveExistentialExpectedProjectionPath :: FilePath
recursiveExistentialExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/recursive-existential/expected/parser-program.txt"

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

higherOrderPartialApplicationExpectedProjectionPath :: FilePath
higherOrderPartialApplicationExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/higher-order-partial-application/expected/parser-program.txt"

higherOrderLocalFunctionFlowExpectedProjectionPath :: FilePath
higherOrderLocalFunctionFlowExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/higher-order-local-function-flow/expected/parser-program.txt"

higherOrderReturnedFunctionExpectedProjectionPath :: FilePath
higherOrderReturnedFunctionExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/higher-order-returned-function/expected/parser-program.txt"

higherOrderFunctionFieldExpectedProjectionPath :: FilePath
higherOrderFunctionFieldExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/higher-order-function-field/expected/parser-program.txt"

authoritativeRecursiveLetExpectedProjectionPath :: FilePath
authoritativeRecursiveLetExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/authoritative-recursive-let/expected/parser-program.txt"

authoritativeCrossModuleLetPolymorphismExpectedProjectionPath :: FilePath
authoritativeCrossModuleLetPolymorphismExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/expected/parser-program.txt"

authoritativeCaseAnalysisExpectedProjectionPath :: FilePath
authoritativeCaseAnalysisExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/authoritative-case-analysis/expected/parser-program.txt"

authoritativeLetPolymorphismExpectedProjectionPath :: FilePath
authoritativeLetPolymorphismExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/authoritative-let-polymorphism/expected/parser-program.txt"

authoritativeNullaryOverloadedMethodExpectedProjectionPath :: FilePath
authoritativeNullaryOverloadedMethodExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/authoritative-nullary-overloaded-method/expected/parser-program.txt"

authoritativeOverloadedMethodExpectedProjectionPath :: FilePath
authoritativeOverloadedMethodExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/authoritative-overloaded-method/expected/parser-program.txt"

recursiveAdtPlainNatExpectedProjectionPath :: FilePath
recursiveAdtPlainNatExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/expected/parser-program.txt"

recursiveListTailExpectedProjectionPath :: FilePath
recursiveListTailExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/recursive-list-tail/expected/parser-program.txt"

recursiveTreeFirstOrderExpectedProjectionPath :: FilePath
recursiveTreeFirstOrderExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/recursive-tree-first-order/expected/parser-program.txt"

recursiveTreeDerivingExpectedProjectionPath :: FilePath
recursiveTreeDerivingExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/recursive-tree-deriving/expected/parser-program.txt"

typeclassIntegrationExpectedProjectionPath :: FilePath
typeclassIntegrationExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/typeclass-integration/expected/parser-program.txt"

abstractRecursiveAdtModuleUseExpectedProjectionPath :: FilePath
abstractRecursiveAdtModuleUseExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/expected/parser-program.txt"

moduleIntegratedRecursiveExistentialExpectedProjectionPath :: FilePath
moduleIntegratedRecursiveExistentialExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/module-integrated-recursive-existential/expected/parser-program.txt"

complexRecursiveProgramExpectedProjectionPath :: FilePath
complexRecursiveProgramExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/complex-recursive-program/expected/parser-program.txt"

packageCrossModuleLetExpectedProjectionPath :: FilePath
packageCrossModuleLetExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/package-cross-module-let/expected/parser-program.txt"

packageSearchPathImportExpectedProjectionPath :: FilePath
packageSearchPathImportExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/package-search-path-import/expected/parser-program.txt"

compilerSeedDataModelExpectedProjectionPath :: FilePath
compilerSeedDataModelExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/compiler-seed-data-model/expected/parser-program.txt"

compilerSeedLexerExpectedProjectionPath :: FilePath
compilerSeedLexerExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/compiler-seed-lexer/expected/parser-program.txt"

sharedParserLibraryRoot :: FilePath
sharedParserLibraryRoot =
    "test/programs/compiler-parser-parity/parser-library"

textLiteralCharStringParserProgramRoot :: FilePath
textLiteralCharStringParserProgramRoot =
    "test/programs/compiler-parser-parity/text-literal-char-string"

firstClassPolymorphismSourceTypesParserProgramRoot :: FilePath
firstClassPolymorphismSourceTypesParserProgramRoot =
    "test/programs/compiler-parser-parity/first-class-polymorphism-source-types"

higherOrderPartialApplicationParserProgramRoot :: FilePath
higherOrderPartialApplicationParserProgramRoot =
    "test/programs/compiler-parser-parity/higher-order-partial-application"

higherOrderLocalFunctionFlowParserProgramRoot :: FilePath
higherOrderLocalFunctionFlowParserProgramRoot =
    "test/programs/compiler-parser-parity/higher-order-local-function-flow"

higherOrderReturnedFunctionParserProgramRoot :: FilePath
higherOrderReturnedFunctionParserProgramRoot =
    "test/programs/compiler-parser-parity/higher-order-returned-function"

higherOrderFunctionFieldParserProgramRoot :: FilePath
higherOrderFunctionFieldParserProgramRoot =
    "test/programs/compiler-parser-parity/higher-order-function-field"

authoritativeRecursiveLetParserProgramRoot :: FilePath
authoritativeRecursiveLetParserProgramRoot =
    "test/programs/compiler-parser-parity/authoritative-recursive-let"

authoritativeCrossModuleLetPolymorphismParserProgramRoot :: FilePath
authoritativeCrossModuleLetPolymorphismParserProgramRoot =
    "test/programs/compiler-parser-parity/authoritative-cross-module-let-polymorphism"

authoritativeCaseAnalysisParserProgramRoot :: FilePath
authoritativeCaseAnalysisParserProgramRoot =
    "test/programs/compiler-parser-parity/authoritative-case-analysis"

authoritativeLetPolymorphismParserProgramRoot :: FilePath
authoritativeLetPolymorphismParserProgramRoot =
    "test/programs/compiler-parser-parity/authoritative-let-polymorphism"

authoritativeNullaryOverloadedMethodParserProgramRoot :: FilePath
authoritativeNullaryOverloadedMethodParserProgramRoot =
    "test/programs/compiler-parser-parity/authoritative-nullary-overloaded-method"

authoritativeOverloadedMethodParserProgramRoot :: FilePath
authoritativeOverloadedMethodParserProgramRoot =
    "test/programs/compiler-parser-parity/authoritative-overloaded-method"

recursiveAdtPlainNatParserProgramRoot :: FilePath
recursiveAdtPlainNatParserProgramRoot =
    "test/programs/compiler-parser-parity/recursive-adt-plain-nat"

recursiveListTailParserProgramRoot :: FilePath
recursiveListTailParserProgramRoot =
    "test/programs/compiler-parser-parity/recursive-list-tail"

recursiveTreeFirstOrderParserProgramRoot :: FilePath
recursiveTreeFirstOrderParserProgramRoot =
    "test/programs/compiler-parser-parity/recursive-tree-first-order"

recursiveTreeDerivingParserProgramRoot :: FilePath
recursiveTreeDerivingParserProgramRoot =
    "test/programs/compiler-parser-parity/recursive-tree-deriving"

typeclassIntegrationParserProgramRoot :: FilePath
typeclassIntegrationParserProgramRoot =
    "test/programs/compiler-parser-parity/typeclass-integration"

abstractRecursiveAdtModuleUseParserProgramRoot :: FilePath
abstractRecursiveAdtModuleUseParserProgramRoot =
    "test/programs/compiler-parser-parity/abstract-recursive-adt-module-use"

moduleIntegratedRecursiveExistentialParserProgramRoot :: FilePath
moduleIntegratedRecursiveExistentialParserProgramRoot =
    "test/programs/compiler-parser-parity/module-integrated-recursive-existential"

complexRecursiveProgramParserProgramRoot :: FilePath
complexRecursiveProgramParserProgramRoot =
    "test/programs/compiler-parser-parity/complex-recursive-program"

derivingEqParserProgramRoot :: FilePath
derivingEqParserProgramRoot =
    "test/programs/compiler-parser-parity/deriving-eq"

recursiveGadtParserProgramRoot :: FilePath
recursiveGadtParserProgramRoot =
    "test/programs/compiler-parser-parity/recursive-gadt"

recursiveExistentialParserProgramRoot :: FilePath
recursiveExistentialParserProgramRoot =
    "test/programs/compiler-parser-parity/recursive-existential"

packageCrossModuleLetParserProgramRoot :: FilePath
packageCrossModuleLetParserProgramRoot =
    "test/programs/compiler-parser-parity/package-cross-module-let"

packageSearchPathImportParserProgramRoot :: FilePath
packageSearchPathImportParserProgramRoot =
    "test/programs/compiler-parser-parity/package-search-path-import"

compilerSeedDataModelParserProgramRoot :: FilePath
compilerSeedDataModelParserProgramRoot =
    "test/programs/compiler-parser-parity/compiler-seed-data-model"

compilerSeedLexerParserProgramRoot :: FilePath
compilerSeedLexerParserProgramRoot =
    "test/programs/compiler-parser-parity/compiler-seed-lexer"

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
    , concat ["Recursive", "Adt", "Plain", "Nat", "Tokens"]
    , concat ["Plain", "Recursive", "Nat", "Tokens"]
    , concat ["Recursive", "List", "Tail", "Tokens"]
    , concat ["Recursive", "Tree", "First", "Order", "Tokens"]
    , concat ["Recursive", "Tree", "Deriving", "Tokens"]
    , concat ["Typeclass", "Integration", "Tokens"]
    , concat ["Abstract", "Recursive", "Adt", "Module", "Use", "Tokens"]
    , concat ["Module", "Integrated", "Recursive", "Existential", "Tokens"]
    , concat ["Complex", "Recursive", "Program", "Tokens"]
    , concat ["Deriving", "Eq", "Tokens"]
    , concat ["Recursive", "Gadt", "Tokens"]
    , concat ["Recursive", "Existential", "Tokens"]
    , concat ["Seed", "Source", "Tokens"]
    , concat ["Seed", "Token", "Tokens"]
    , concat ["Seed", "Diagnostic", "Tokens"]
    , concat ["Seed", "Ast", "Tokens"]
    , concat ["Compiler", "Seed", "Data", "Model", "Tokens"]
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
    , concat ["LexerOk ", "recursive", "Adt", "Plain", "Nat", "Tokens"]
    , concat ["LexerOk ", "plain", "Recursive", "Nat", "Tokens"]
    , concat ["LexerOk ", "recursive", "List", "Tail", "Tokens"]
    , concat ["LexerOk ", "recursive", "Tree", "First", "Order", "Tokens"]
    , concat ["LexerOk ", "recursive", "Tree", "Deriving", "Tokens"]
    , concat ["LexerOk ", "typeclass", "Integration", "Tokens"]
    , concat ["LexerOk ", "abstract", "Recursive", "Adt", "Module", "Use", "Tokens"]
    , concat ["LexerOk ", "module", "Integrated", "Recursive", "Existential", "Tokens"]
    , concat ["LexerOk ", "complex", "Recursive", "Program", "Tokens"]
    , concat ["LexerOk ", "deriving", "Eq", "Tokens"]
    , concat ["LexerOk ", "recursive", "Gadt", "Tokens"]
    , concat ["LexerOk ", "recursive", "Existential", "Tokens"]
    , concat ["LexerOk ", "seed", "Source", "Tokens"]
    , concat ["LexerOk ", "seed", "Token", "Tokens"]
    , concat ["LexerOk ", "seed", "Diagnostic", "Tokens"]
    , concat ["LexerOk ", "seed", "Ast", "Tokens"]
    , concat ["LexerOk ", "compiler", "Seed", "Data", "Model", "Tokens"]
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
    , concat ["recursive-adt-plain-nat", " tokens"]
    , concat ["plain-recursive-nat", " tokens"]
    , concat ["recursive-list-tail", " tokens"]
    , concat ["recursive-tree-first-order", " tokens"]
    , concat ["recursive-tree-deriving", " tokens"]
    , concat ["typeclass-integration", " tokens"]
    , concat ["abstract-recursive-adt-module-use", " tokens"]
    , concat ["module-integrated-recursive-existential", " tokens"]
    , concat ["complex-recursive-program", " tokens"]
    , concat ["deriving-eq", " tokens"]
    , concat ["recursive-gadt", " tokens"]
    , concat ["recursive-existential", " tokens"]
    , concat ["compiler-seed-data-model", " tokens"]
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

sharedParserExpectationSubstratePhrases :: [String]
sharedParserExpectationSubstratePhrases =
    [ "data ParserExpectation"
    , "def parserDiagnosticForExpectation : ParserExpectation -> String -> ParserDiagnostic"
    , "def parserFailExpectedAtCurrent : ParserExpectation -> Parser ParserValue"
    , "def labelExpected : ParserExpectation -> Parser ParserValue -> Parser ParserValue"
    , "UnexpectedSourceText span -> ParserStepError (parserDiagnosticForExpectation expectation span)"
    ]

sharedParserExpectationUsePhrases :: [String]
sharedParserExpectationUsePhrases =
    [ "labelExpected ParserExpectEquals"
    , "labelExpected ParserExpectImportAlias"
    , "labelExpected ParserExpectFunctionalDependencyArrow"
    , "parserFailExpected ParserExpectImportSemicolon"
    , "parserFailExpectedAtCurrent ParserExpectDefSemicolon"
    , "parserFailExpectedAtCurrent ParserExpectImportExposingSeparator"
    , "parserFailExpectedAtCurrent ParserExpectExpressionCloseParen"
    ]

sharedParserRemovedExpectationAliases :: [String]
sharedParserRemovedExpectationAliases =
    [ "parserFailExpectedImportSemicolonAtCurrent"
    , "parserFailExpectedDefSemicolonAtCurrent"
    , "parserFailExpectedImportExposingSeparatorAtCurrent"
    , "parserFailExpectedCaseBranchArrowAtCurrent"
    , "parserFailExpectedConstructorForallDotAtCurrent"
    , "parserFailExpectedExpressionCloseParenAtCurrent"
    , "labelUnexpectedSource"
    , "labelEquals"
    , "labelImportSemicolon"
    , "labelImportAlias"
    , "labelDefSemicolon"
    , "labelLetIn"
    , "labelLetAnnotationType"
    , "labelConstructorColon"
    , "labelCaseBranchArrow"
    , "labelInstanceMethodEquals"
    , "labelFunctionalDependencyArrow"
    , "labelTypeFamilyEquationEquals"
    , "labelConstructorForallDot"
    ]

sharedParserBoundedProjectionRowsSubstratePhrases :: [String]
sharedParserBoundedProjectionRowsSubstratePhrases =
    [ "data ProjectionRowParser"
    , "data ProjectionRowsFinish"
    , "def parseBoundedDelimitedProjectionRows : String -> ProjectionRowParser -> ProjectionRowsFinish -> ProjectionRowsFinish"
    , "def parseBoundedDelimitedProjectionRowsMoreOrDone8"
    , "def parseBoundedDelimitedProjectionRowsMoreOrDone0"
    , "def appendBoundedDelimitedProjectionRowsAndFinish"
    ]

sharedParserBoundedProjectionRowsUsePhrases :: [String]
sharedParserBoundedProjectionRowsUsePhrases =
    [ "parseBoundedDelimitedProjectionRows sourceFile ProjectionExportRows ProjectionRowsReturn ProjectionRowsReturn ValueUnit"
    , "parseBoundedDelimitedProjectionRowsMoreOrDone8 sourceFile ProjectionImportRows ProjectionRowsImportCloseOrSeparator ProjectionRowsImportFinalClose"
    , "def parseImportProjectionCloseOrSeparator : ParserValue -> Parser ParserValue"
    , "parserFailExpectedAtCurrent ParserExpectImportExposingSeparator"
    , "def parseImportProjectionFinalClose : ParserValue -> Parser ParserValue"
    ]

sharedParserRemovedProjectionListAliases :: [String]
sharedParserRemovedProjectionListAliases =
    concat
        [ [ "parseProjectionExportMoreOrDone" <> show n | n <- [(0 :: Int) .. 8] ]
        , [ "parseProjectionExportNextItem" <> show n | n <- [(0 :: Int) .. 8] ]
        , [ "appendExportProjectionRowsAndContinue" <> show n | n <- [(0 :: Int) .. 7] ]
        , [ "parseImportProjectionMoreOrClose" <> show n | n <- [(0 :: Int) .. 8] ]
        , [ "parseImportProjectionNextItem" <> show n | n <- [(0 :: Int) .. 8] ]
        , [ "appendImportProjectionRowsAndContinue" <> show n | n <- [(1 :: Int) .. 7] ]
        , [ "parseImportProjectionMoreOrClose"
          , "appendImportProjectionRowsAndClose"
          , "appendFinalImportProjectionRows"
          , "parseFinalImportProjectionClose"
          ]
        ]

sharedParserBoundedCaseBranchRowsSubstratePhrases :: [String]
sharedParserBoundedCaseBranchRowsSubstratePhrases =
    [ "def parseBoundedCaseBranchRows : (ParserValue -> Parser ParserValue) -> ParserValue -> ParserValue -> Parser ParserValue"
    , "λ(branchParser : ParserValue -> Parser ParserValue)"
    , "parserBind (branchParser ValueUnit)"
    , "def parseBoundedCaseBranchRowsMoreOrClose8"
    , "def parseBoundedCaseBranchRowsMoreOrClose1"
    , "def appendBoundedCaseBranchRowsAndClose"
    ]

sharedParserBoundedCaseBranchRowsUsePhrases :: [String]
sharedParserBoundedCaseBranchRowsUsePhrases =
    [ "parseBoundedCaseBranchRows parseSourceCaseBranch scrutineeValue ValueUnit"
    , "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch scrutineeValue ValueUnit"
    , "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch4 scrutineeValue ValueUnit"
    , "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch3 scrutineeValue ValueUnit"
    , "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch2 scrutineeValue ValueUnit"
    , "parseBoundedCaseBranchRows parseNestedCaseBranchInnerBranch1 scrutineeValue ValueUnit"
    , "parserBind (appendSourceCaseBranchText branchRows nextBranch)"
    , "parseSourceCaseClose scrutineeValue"
    ]

sharedParserRemovedCaseBranchAliases :: [String]
sharedParserRemovedCaseBranchAliases =
    concat
        [ [ "parseSourceCaseMoreOrClose" <> show n | n <- [(1 :: Int) .. 8] ]
        , [ "parseSourceCaseNextBranch" <> show n | n <- [(0 :: Int) .. 7] ]
        , [ "appendSourceCaseBranchAndContinue" <> show n | n <- [(1 :: Int) .. 7] ]
        , [ "appendSourceCaseBranchAndClose" ]
        , [ "parseNestedCaseBranchMoreOrClose" <> show n | n <- [(1 :: Int) .. 8] ]
        , [ "parseNestedCaseBranchNextBranch" <> show n | n <- [(0 :: Int) .. 7] ]
        , [ "appendNestedCaseBranchAndContinue" <> show n | n <- [(1 :: Int) .. 7] ]
        , [ "appendNestedCaseBranchAndClose" ]
        , [ "parseNestedCaseBranchMoreOrClose" <> show n <> "Depth" <> show depth
          | depth <- [(1 :: Int) .. 4]
          , n <- [(1 :: Int) .. 8]
          ]
        , [ "parseNestedCaseBranchNextBranch" <> show n <> "Depth" <> show depth
          | depth <- [(1 :: Int) .. 4]
          , n <- [(0 :: Int) .. 7]
          ]
        , [ "appendNestedCaseBranchAndContinue" <> show n <> "Depth" <> show depth
          | depth <- [(1 :: Int) .. 4]
          , n <- [(0 :: Int) .. 7]
          ]
        ]

sharedParserBoundedApplicationArgumentsSubstratePhrases :: [String]
sharedParserBoundedApplicationArgumentsSubstratePhrases =
    [ "def parseBoundedApplicationArguments : (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    , "def parseBoundedTwoApplicationArguments : (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    , "def parseBoundedSingleApplicationArgument : (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    , "λ(argumentParser : ParserValue -> Parser ParserValue)"
    , "def parseBoundedApplicationArgumentsMoreOrDone6"
    , "def parseBoundedApplicationArgumentsMoreOrDone0"
    , "def appendBoundedApplicationArgumentAndContinue0"
    ]

sharedParserBoundedApplicationArgumentsUsePhrases :: [String]
sharedParserBoundedApplicationArgumentsUsePhrases =
    [ "parseBoundedApplicationArguments parseExpressionAtom"
    , "parseBoundedSingleApplicationArgument parseSimpleExpressionAtom"
    , "parseBoundedTwoApplicationArguments parseSimpleExpressionAtom"
    , "parserBind (argumentParser ValueUnit)"
    , "parserBind (finishApplicationExpression applicationValue argumentValue)"
    ]

sharedParserRemovedApplicationArgumentAliases :: [String]
sharedParserRemovedApplicationArgumentAliases =
    [ "parseApplicationArgumentOrDone"
    , "parseApplicationSecondArgumentOrDone"
    , "parseApplicationThirdArgumentOrDone"
    , "parseApplicationFourthArgumentOrDone"
    , "parseApplicationFifthArgumentOrDone"
    , "parseApplicationSixthArgumentOrDone"
    , "parseApplicationSeventhArgumentOrDone"
    , "parseApplicationEighthArgumentOrDone"
    , "parseApplicationNinthArgumentOrDone"
    , "parseApplicationTenthArgumentOrDone"
    , "parseApplicationEleventhArgumentOrDone"
    , "parseApplicationTwelfthArgumentOrDone"
    , "parseApplicationThirteenthArgumentOrDone"
    , "parseSimpleApplicationArgumentOrDone"
    , "parseTwoSimpleApplicationArgumentOrDone"
    , "parseSimpleApplicationSecondArgumentOrDone"
    , "parseSimpleApplicationThirdArgumentOrDone"
    ]

sharedParserNestedParenthesizedApplicationSubstratePhrases :: [String]
sharedParserNestedParenthesizedApplicationSubstratePhrases =
    [ "def parseNestedParenthesizedApplicationTopLevelOrDone"
    , "def finishNestedParenthesizedApplicationArgumentWithSecondDepth4"
    , "def finishNestedParenthesizedApplicationArgumentWithSecondDepth2"
    , "def finishNestedParenthesizedApplicationArgumentWithSecondDepth1"
    , "def finishNestedParenthesizedApplicationArgumentWithSimpleSecond"
    , "def parseNestedParenthesizedApplicationSecondDepth4OrSimpleDone"
    , "def parseNestedParenthesizedApplicationSecondDepth2OrSimpleDone"
    , "def parseNestedParenthesizedApplicationSecondDepth1OrSimpleDone"
    , "def parseNestedParenthesizedApplicationArgumentDepth4OrDone"
    , "def parseNestedParenthesizedApplicationArgumentDepth3OrDone"
    , "def parseNestedParenthesizedApplicationArgumentDepth2OrDone"
    , "def parseNestedParenthesizedApplicationArgumentDepth1OrDone"
    , "def parseNestedParenthesizedApplicationArgumentDepth0OrDone"
    ]

sharedParserNestedParenthesizedApplicationUsePhrases :: [String]
sharedParserNestedParenthesizedApplicationUsePhrases =
    [ "parserBind (parseSimpleExpressionAtom ValueUnit)\n        parseNestedParenthesizedApplicationTopLevelOrDone"
    , "parserBind (parseSimpleExpressionAtom ValueUnit)\n        parseNestedParenthesizedApplicationArgumentDepth4OrDone"
    , "parserBind (parseSimpleExpressionAtom ValueUnit)\n        parseNestedParenthesizedApplicationArgumentDepth3OrDone"
    , "parserBind (parseSimpleExpressionAtom ValueUnit)\n        parseNestedParenthesizedApplicationArgumentDepth2OrDone"
    , "parserBind (parseSimpleExpressionAtom ValueUnit)\n        parseNestedParenthesizedApplicationArgumentDepth1OrDone"
    , "parserBind (parseSimpleExpressionAtom ValueUnit)\n        parseNestedParenthesizedApplicationArgumentDepth0OrDone"
    , "parseParenthesizedNestedApplicationArgument4 ValueUnit) (finishNestedParenthesizedApplicationArgumentWithSecondDepth4 functionValue)"
    , "parseParenthesizedNestedApplicationArgument2 ValueUnit) (finishNestedParenthesizedApplicationArgumentWithSecondDepth1 functionValue)"
    , "parseParenthesizedNestedApplicationArgument1 ValueUnit) (finishNestedParenthesizedApplicationArgumentWithSecondDepth1 functionValue)"
    , "parseParenthesizedTwoSimpleApplicationArgument ValueUnit) (finishNestedParenthesizedApplicationArgumentWithSimpleSecond functionValue)"
    , "parseBoundedSingleApplicationArgument parseSimpleExpressionAtom applicationValue"
    ]

sharedParserNestedParenthesizedApplicationGuardPhrases :: [String]
sharedParserNestedParenthesizedApplicationGuardPhrases =
    [ "shared parser-owned .mlfp parser shares nested parenthesized application depth handling"
    , "sharedParserNestedParenthesizedApplicationSubstratePhrases"
    , "sharedParserNestedParenthesizedApplicationUsePhrases"
    , "sharedParserRemovedNestedParenthesizedApplicationAliases"
    ]

sharedParserRemovedNestedParenthesizedApplicationAliases :: [String]
sharedParserRemovedNestedParenthesizedApplicationAliases =
    concat
        [ [ concat ["parse", "Parenthesized", "Application", "Argument", "Or", "Done"]
          , concat ["append", "Parenthesized", "Application", "Argument"]
          , concat ["parse", "Parenthesized", "Application", "Simple", "Second", "Or", "Done"]
          , concat ["parse", "Parenthesized", "Application", "Second", "Argument", "Or", "Done"]
          , concat ["parse", "Parenthesized", "Application", "Second", "Argument", "Or", "Simple", "Done"]
          , concat ["append", "Parenthesized", "Application", "Simple", "Argument", "0"]
          ]
        , [ concat ["parse", "Nested", "Parenthesized", "Application", "Argument", "Or", "Done", show n]
          | n <- [(0 :: Int) .. 4]
          ]
        ]

sharedParserAnnotatedLambdaRhsSubstratePhrases :: [String]
sharedParserAnnotatedLambdaRhsSubstratePhrases =
    [ "def parseBoundedAnnotatedLambdaRhsExpressionWithBody : (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    , "def parseBoundedAnnotatedLambdaRhsOpenWithBody : (ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    , "def parseBoundedAnnotatedLambdaRhsParamTypeWithBody : (ParserValue -> Parser ParserValue) -> ParserValue -> ParserValue -> Parser ParserValue"
    , "def parseBoundedAnnotatedLambdaRhsBodyWithBody : (ParserValue -> Parser ParserValue) -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue"
    , "parserBind (bodyParser ValueUnit)"
    , "finishAnnotatedLambdaExpression paramToken typeValue"
    ]

sharedParserAnnotatedLambdaRhsUsePhrases :: [String]
sharedParserAnnotatedLambdaRhsUsePhrases =
    [ "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression5 ValueUnit"
    , "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression4 ValueUnit"
    , "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression3 ValueUnit"
    , "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression2 ValueUnit"
    , "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression1 ValueUnit"
    , "parseBoundedAnnotatedLambdaRhsExpressionWithBody parseBoundedAnnotatedLambdaRhsBodyExpression0 ValueUnit"
    , "def parseBoundedAnnotatedLambdaRhsBodyExpression0 : ParserValue -> Parser ParserValue"
    , "parserChoice (parseSourceCaseExpression ValueUnit) (parseApplicationOrAtomExpression ValueUnit)"
    ]

sharedParserAnnotatedLambdaRhsGuardPhrases :: [String]
sharedParserAnnotatedLambdaRhsGuardPhrases =
    [ "shared parser-owned .mlfp parser shares bounded annotated lambda RHS depth handling"
    , "sharedParserAnnotatedLambdaRhsSubstratePhrases"
    , "sharedParserAnnotatedLambdaRhsUsePhrases"
    , "sharedParserRemovedAnnotatedLambdaRhsAliases"
    ]

sharedParserRemovedAnnotatedLambdaRhsAliases :: [String]
sharedParserRemovedAnnotatedLambdaRhsAliases =
    concat
        [ [ "parseAnnotatedLambdaRhsExpression" <> show n | n <- [(1 :: Int) .. 5] ]
        , [ "parseAnnotatedLambdaRhsOpen" <> show n | n <- [(1 :: Int) .. 5] ]
        , [ "parseAnnotatedLambdaRhsParam" <> show n | n <- [(1 :: Int) .. 5] ]
        , [ "parseAnnotatedLambdaRhsParamColon" <> show n | n <- [(1 :: Int) .. 5] ]
        , [ "parseAnnotatedLambdaRhsParamType" <> show n | n <- [(1 :: Int) .. 5] ]
        , [ "parseAnnotatedLambdaRhsParamClose" <> show n | n <- [(1 :: Int) .. 5] ]
        , [ "parseAnnotatedLambdaRhsBody" <> show n | n <- [(1 :: Int) .. 5] ]
        ]

sharedParserSourceTypeArrowTailSubstratePhrases :: [String]
sharedParserSourceTypeArrowTailSubstratePhrases =
    [ "def parseBoundedSourceTypeArrowTailText : (String -> ParserValue -> Parser ParserValue) -> String -> ParserValue -> Parser ParserValue"
    , "def parseBoundedSourceTypeCodomainText : (String -> ParserValue -> Parser ParserValue) -> String -> ParserValue -> Parser ParserValue"
    , "appendSourceArrowTypeText prefix rightType"
    , "parserBind (parseSourceTypeCodomainAtom ValueUnit)"
    , "def parseBoundedSourceTypeArrowTailTextBudget7"
    , "def parseBoundedSourceTypeArrowTailTextBudget0"
    ]

sharedParserSourceTypeArrowTailUsePhrases :: [String]
sharedParserSourceTypeArrowTailUsePhrases =
    [ "parserBind (parseSourceTypeCodomainAtom ValueUnit)\n        (parseBoundedSourceTypeArrowTailTextBudget7 (parserTextFromValue leftType))"
    , "parseBoundedSourceTypeArrowTailText parseBoundedSourceTypeCodomainTextBudget6"
    , "parseBoundedSourceTypeArrowTailText parseBoundedSourceTypeCodomainTextBudget2"
    , "parseBoundedSourceTypeCodomainText parseBoundedSourceTypeArrowTailTextBudget0"
    , "parserTextValue (appendSourceArrowTypeText prefix rightType)"
    ]

sharedParserSourceTypeArrowTailGuardPhrases :: [String]
sharedParserSourceTypeArrowTailGuardPhrases =
    [ "shared parser-owned .mlfp parser shares bounded source-type arrow-tail text accumulation"
    , "sharedParserSourceTypeArrowTailSubstratePhrases"
    , "sharedParserSourceTypeArrowTailUsePhrases"
    , "sharedParserRemovedSourceTypeArrowTailAliases"
    ]

sharedParserRemovedSourceTypeArrowTailAliases :: [String]
sharedParserRemovedSourceTypeArrowTailAliases =
    concat
        [ [ "parseSourceTypeArrowTailText" <> show n | n <- [(0 :: Int) .. 7] ]
        , [ "parseSourceTypeCodomainText" <> show n | n <- [(0 :: Int) .. 6] ]
        ]

sharedParserConstructorRowAccumulatorSubstratePhrases :: [String]
sharedParserConstructorRowAccumulatorSubstratePhrases =
    [ "ValueConstructorRows : String -> ParserValue"
    , "def emptyConstructorRows : ParserValue"
    , "def constructorRowsFromValue : ParserValue -> String"
    , "def appendConstructorRow : String -> ParserValue -> ParserValue -> ParserValue -> String -> Parser ParserValue"
    , "def dataRowsWithConstructorRows : String -> ParserValue -> String -> ParserValue -> String"
    ]

sharedParserConstructorRowAccumulatorUsePhrases :: [String]
sharedParserConstructorRowAccumulatorUsePhrases =
    [ "parseExactFourConstructorDataRowsConstructor1Name sourceFile dataStart dataNameToken emptyConstructorRows"
    , "parseExactFiveConstructorDataRowsConstructor1Name sourceFile dataStart dataNameToken emptyConstructorRows"
    , "parseExactNineConstructorDataRowsConstructor1Name sourceFile dataStart dataNameToken emptyConstructorRows"
    , "parserBind (appendConstructorRow sourceFile constructorRowsValue c4Token t4"
    , "parserBind (appendConstructorRow sourceFile constructorRowsValue c5Token t5"
    , "parserBind (appendConstructorRow sourceFile constructorRowsValue c9Token t9"
    , "dataRowsWithConstructorRows\n                      sourceFile\n                      dataNameToken"
    ]

sharedParserConstructorRowAccumulatorGuardPhrases :: [String]
sharedParserConstructorRowAccumulatorGuardPhrases =
    [ "shared parser-owned .mlfp parser shares constructor row accumulation"
    , "sharedParserConstructorRowAccumulatorSubstratePhrases"
    , "sharedParserConstructorRowAccumulatorUsePhrases"
    , "sharedParserRemovedConstructorRowAccumulatorAliases"
    ]

sharedParserRemovedConstructorRowAccumulatorAliases :: [String]
sharedParserRemovedConstructorRowAccumulatorAliases =
    concat
        [ [ "parseExactFourConstructorDataRowsConstructor" <> show n <> "Continue" | n <- [(1 :: Int) .. 3] ]
        , [ "parseExactFiveConstructorDataRowsConstructor" <> show n <> "Continue" | n <- [(1 :: Int) .. 4] ]
        , [ "parseExactNineConstructorDataRowsConstructor" <> show n <> "Continue" | n <- [(1 :: Int) .. 8] ]
        , [ "parseExactFourConstructorDataRowsFinish"
          , "parseExactFiveConstructorDataRowsFinish"
          , "parseExactNineConstructorDataRowsFinish"
          ]
        ]

sharedParserBoundedSourceDefinitionRowSequenceSubstratePhrases :: [String]
sharedParserBoundedSourceDefinitionRowSequenceSubstratePhrases =
    [ "def parseBoundedSourceDefinitionRows : String -> (String -> ParserValue -> Parser ParserValue) -> ParserValue -> Parser ParserValue"
    , "def parseBoundedSourceDefinitionNextRows : (String -> ParserValue -> Parser ParserValue) -> String -> ParserValue -> Parser ParserValue"
    , "def appendBoundedSourceDefinitionRowsAndContinue : (String -> ParserValue -> Parser ParserValue) -> String -> ParserValue -> ParserValue -> Parser ParserValue"
    , "def finishBoundedSourceDefinitionRows : String -> ParserValue -> Parser ParserValue"
    , "def parseBoundedSourceDefinitionRowsRemaining15"
    , "def parseBoundedSourceDefinitionRowsRemaining12"
    , "def parseBoundedSourceDefinitionRowsRemaining3"
    , "parserBind (parseSourceDefinitionRows sourceFile ValueUnit)"
    , "parserBind (appendProjectionValues existingRows nextRows)"
    ]

sharedParserBoundedSourceDefinitionRowSequenceUsePhrases :: [String]
sharedParserBoundedSourceDefinitionRowSequenceUsePhrases =
    [ "parseBoundedSourceDefinitionRows sourceFile parseBoundedSourceDefinitionRowsRemaining3 ValueUnit"
    , "parseBoundedSourceDefinitionRows sourceFile parseBoundedSourceDefinitionRowsRemaining12 ValueUnit"
    , "parseBoundedSourceDefinitionRows sourceFile parseBoundedSourceDefinitionRowsRemaining15 ValueUnit"
    , "parserBind (parseFourSourceDefinitionRows sourceFile ValueUnit)\n        (finishSixDataFourDefinitionRows sourceFile moduleStart moduleName exportRows dataRowsValue)"
    , "parserBind (parseSixteenSourceDefinitionRows sourceFile ValueUnit)\n        (appendProjectionValues dataRowsValue)"
    ]

sharedParserSourceDefinitionRowSequenceGuardPhrases :: [String]
sharedParserSourceDefinitionRowSequenceGuardPhrases =
    [ "shared parser-owned .mlfp parser shares bounded source-definition row sequencing"
    , "sharedParserBoundedSourceDefinitionRowSequenceSubstratePhrases"
    , "sharedParserBoundedSourceDefinitionRowSequenceUsePhrases"
    , "sharedParserRemovedSourceDefinitionRowSequenceAliases"
    ]

sharedParserRemovedSourceDefinitionRowSequenceAliases :: [String]
sharedParserRemovedSourceDefinitionRowSequenceAliases =
    [ "parseFourSourceDefinitionSecondRows"
    , "parseFourSourceDefinitionThirdRows"
    , "parseFourSourceDefinitionFourthRows"
    , "finishFourSourceDefinitionRows"
    , "finishFourSourceDefinitionRowsThird"
    , "finishFourSourceDefinitionRowsFourth"
    , "parseThirteenSourceDefinitionRowsSecondBatch"
    , "appendThirteenSourceDefinitionRowsSecondBatch"
    , "parseThirteenSourceDefinitionRowsThirdBatch"
    , "appendThirteenSourceDefinitionRowsThirdBatch"
    , "parseThirteenSourceDefinitionRowsFinal"
    , "parseSixteenSourceDefinitionRowsSecondBatch"
    , "appendSixteenSourceDefinitionRowsSecondBatch"
    , "parseSixteenSourceDefinitionRowsThirdBatch"
    , "appendSixteenSourceDefinitionRowsThirdBatch"
    , "parseSixteenSourceDefinitionRowsFourthBatch"
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
        , sharedParserRound321ShortcutPhrases
        , sharedParserRound322ShortcutPhrases
        , sharedParserRound323ShortcutPhrases
        , sharedParserRound325ShortcutPhrases
        , sharedParserRound326ShortcutPhrases
        , sharedParserRound327ShortcutPhrases
        , sharedParserRound328ShortcutPhrases
        , sharedParserRound329ShortcutPhrases
        , sharedParserRound330ShortcutPhrases
        , sharedParserRound331ShortcutPhrases
        , sharedParserRound332ShortcutPhrases
        , sharedParserRound333ShortcutPhrases
        , sharedParserRound334ShortcutPhrases
        , sharedParserRound335ShortcutPhrases
        , sharedParserRound336ShortcutPhrases
        , sharedParserRound337ShortcutPhrases
        , sharedParserRound338ShortcutPhrases
        , sharedParserRound339ShortcutPhrases
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

sharedParserRound321ShortcutPhrases :: [String]
sharedParserRound321ShortcutPhrases =
    [ concat ["parse", "Higher", "Order", "Partial", "Application"]
    , concat ["completeModuleKey \"", "higher-order-partial-application", "\""]
    , concat ["moduleKey \"", "higher-order-partial-application", "\""]
    , concat ["programKey \"", "higher-order-partial-application", "\""]
    , concat ["Higher", "Order", "Partial", "Application", "Tokens"]
    , concat ["LexerOk ", "higher", "Order", "Partial", "Application", "Tokens"]
    , concat ["higher-order-partial-application", " tokens"]
    , concat ["defRows sourceFile \"", "keepLeft", "\""]
    , concat ["defRows sourceFile \"", "apply", "\""]
    , concat ["def keepLeft type=", "Int -> Int -> Int expr=", "λx λy x"]
    , concat ["def main type=Int expr=", "apply (keepLeft 1)"]
    , concat ["higher-order-partial-application parser negative ", "expected-expression-close-paren@"]
    ]

sharedParserRound322ShortcutPhrases :: [String]
sharedParserRound322ShortcutPhrases =
    [ concat ["parse", "Higher", "Order", "Local", "Function", "Flow"]
    , concat ["completeModuleKey \"", "higher-order-local-function-flow", "\""]
    , concat ["moduleKey \"", "higher-order-local-function-flow", "\""]
    , concat ["programKey \"", "higher-order-local-function-flow", "\""]
    , concat ["Higher", "Order", "Local", "Function", "Flow", "Tokens"]
    , concat ["LexerOk ", "higher", "Order", "Local", "Function", "Flow", "Tokens"]
    , concat ["higher-order-local-function-flow", " tokens"]
    , concat ["defRows sourceFile \"", "use", "\""]
    , concat ["def main type=Int expr=", "let captured : Int = 41 in let f : Int -> Int = λ(x : Int) captured in use f"]
    , concat ["higher-order-local-function-flow parser negative ", "expected-let-in@"]
    ]

sharedParserRound323ShortcutPhrases :: [String]
sharedParserRound323ShortcutPhrases =
    [ concat ["parse", "Higher", "Order", "Returned", "Function"]
    , concat ["completeModuleKey \"", "higher-order-returned-function", "\""]
    , concat ["moduleKey \"", "higher-order-returned-function", "\""]
    , concat ["programKey \"", "higher-order-returned-function", "\""]
    , concat ["Higher", "Order", "Returned", "Function", "Tokens"]
    , concat ["LexerOk ", "higher", "Order", "Returned", "Function", "Tokens"]
    , concat ["higher-order-returned-function", " tokens"]
    , concat ["defRows sourceFile \"", "make", "\""]
    , concat ["def make type=", "Int -> (Int -> Int) expr=", "λ(base : Int) let captured : Int = base in λ(x : Int) captured"]
    , concat ["def make type=", "Int -> Int -> Int expr=", "λ(base : Int) let captured : Int = base in λ(x : Int) captured"]
    , concat ["def main type=Int expr=", "(make 41) 0"]
    , concat ["def main type=Int expr=", "make 41 0"]
    , concat ["higher-order-returned-function parser negative ", "expected-expression-close-paren@"]
    ]

sharedParserRound325ShortcutPhrases :: [String]
sharedParserRound325ShortcutPhrases =
    [ concat ["parse", "Higher", "Order", "Function", "Field"]
    , concat ["completeModuleKey \"", "higher-order-function-field", "\""]
    , concat ["moduleKey \"", "higher-order-function-field", "\""]
    , concat ["programKey \"", "higher-order-function-field", "\""]
    , concat ["Higher", "Order", "Function", "Field", "Tokens"]
    , concat ["LexerOk ", "higher", "Order", "Function", "Field", "Tokens"]
    , concat ["higher-order-function-field", " tokens"]
    , concat ["defRows sourceFile \"", "FnBox", "\""]
    , concat ["constructor FnBox type=", "(Int -> Int) -> FnBox"]
    , concat ["def main type=Int expr=", "let captured : Int = 41 in let f : Int -> Int ="]
    , concat ["higher-order-function-field parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound326ShortcutPhrases :: [String]
sharedParserRound326ShortcutPhrases =
    [ concat ["parse", "Authoritative", "Recursive", "Let"]
    , concat ["completeModuleKey \"", "authoritative-recursive-let", "\""]
    , concat ["moduleKey \"", "authoritative-recursive-let", "\""]
    , concat ["programKey \"", "authoritative-recursive-let", "\""]
    , concat ["Authoritative", "Recursive", "Let", "Tokens"]
    , concat ["LexerOk ", "authoritative", "Recursive", "Let", "Tokens"]
    , concat ["authoritative-recursive-let", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module Main export (Nat(..), main)", "\""]
    , concat ["stringIndexOf \"", "module Main export (Nat(..), main)", "\" sourceText"]
    , concat ["defRows sourceFile \"", "peel", "\""]
    , concat ["defRows sourceFile \"", "main", "\""]
    , concat ["def main type=Bool expr=", "let peel : Nat -> Nat ="]
    , concat ["authoritative-recursive-let parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound327ShortcutPhrases :: [String]
sharedParserRound327ShortcutPhrases =
    [ concat ["parse", "Authoritative", "Cross", "Module", "Let", "Polymorphism"]
    , concat ["completeModuleKey \"", "authoritative-cross-module-let-polymorphism", "\""]
    , concat ["moduleKey \"", "authoritative-cross-module-let-polymorphism", "\""]
    , concat ["programKey \"", "authoritative-cross-module-let-polymorphism", "\""]
    , concat ["Authoritative", "Cross", "Module", "Let", "Polymorphism", "Tokens"]
    , concat ["LexerOk ", "authoritative", "Cross", "Module", "Let", "Polymorphism", "Tokens"]
    , concat ["authoritative-cross-module-let-polymorphism", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module Core export (applyId)", "\""]
    , concat ["stringIndexOf \"", "module Core export (applyId)", "\" sourceText"]
    , concat ["defRows sourceFile \"", "applyId", "\""]
    , concat ["defRows sourceFile \"", "main", "\""]
    , concat ["def applyId type=Int expr=", "let id = λx x in id 1"]
    , concat ["authoritative-cross-module-let-polymorphism parser negative ", "expected-def-semicolon@"]
    ]

sharedParserRound328ShortcutPhrases :: [String]
sharedParserRound328ShortcutPhrases =
    [ concat ["parse", "Recursive", "Adt", "Plain", "Nat"]
    , concat ["parse", "Plain", "Recursive", "Nat"]
    , concat ["completeModuleKey \"", "recursive-adt-plain-nat", "\""]
    , concat ["moduleKey \"", "recursive-adt-plain-nat", "\""]
    , concat ["programKey \"", "recursive-adt-plain-nat", "\""]
    , concat ["Recursive", "Adt", "Plain", "Nat", "Tokens"]
    , concat ["Plain", "Recursive", "Nat", "Tokens"]
    , concat ["LexerOk ", "recursive", "Adt", "Plain", "Nat", "Tokens"]
    , concat ["LexerOk ", "plain", "Recursive", "Nat", "Tokens"]
    , concat ["recursive-adt-plain-nat", " tokens"]
    , concat ["plain-recursive-nat", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module NatPlain export", "\""]
    , concat ["stringIndexOf \"", "module NatPlain export", "\" sourceText"]
    , concat ["defRows sourceFile \"", "isZero", "\""]
    , concat ["defRows sourceFile \"", "peel", "\""]
    , concat ["defRows sourceFile \"", "main", "\""]
    , concat ["def isZero type=Nat -> Bool expr=", "λ(n : Nat) case n of"]
    , concat ["def peel type=Nat -> Nat expr=", "λ(n : Nat) case n of"]
    , concat ["def main type=Bool expr=", "isZero (peel (Succ Zero))"]
    , concat ["recursive-adt-plain-nat parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound329ShortcutPhrases :: [String]
sharedParserRound329ShortcutPhrases =
    [ concat ["parse", "Recursive", "List", "Tail"]
    , concat ["completeModuleKey \"", "recursive-list-tail", "\""]
    , concat ["moduleKey \"", "recursive-list-tail", "\""]
    , concat ["programKey \"", "recursive-list-tail", "\""]
    , concat ["Recursive", "List", "Tail", "Tokens"]
    , concat ["LexerOk ", "recursive", "List", "Tail", "Tokens"]
    , concat ["recursive-list-tail", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module RecursiveList export", "\""]
    , concat ["stringIndexOf \"", "module RecursiveList export", "\" sourceText"]
    , concat ["defRows sourceFile \"", "tailOrNil", "\""]
    , concat ["defRows sourceFile \"", "isNil", "\""]
    , concat ["defRows sourceFile \"", "main", "\""]
    , concat ["dataRows sourceFile \"", "List", "\""]
    , concat ["constructorRows sourceFile \"", "Cons", "\""]
    , concat ["def tailOrNil type=List -> List expr=", "λ(xs : List) case xs of"]
    , concat ["def isNil type=List -> Bool expr=", "λ(xs : List) case xs of"]
    , concat ["def main type=Bool expr=", "isNil (tailOrNil (Cons Zero Nil))"]
    , concat ["recursive-list-tail parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound330ShortcutPhrases :: [String]
sharedParserRound330ShortcutPhrases =
    [ concat ["parse", "Recursive", "Tree"]
    , concat ["completeModuleKey \"", "recursive-tree-first-order", "\""]
    , concat ["completeModuleKey \"", "recursive-tree-deriving", "\""]
    , concat ["moduleKey \"", "recursive-tree-first-order", "\""]
    , concat ["moduleKey \"", "recursive-tree-deriving", "\""]
    , concat ["programKey \"", "recursive-tree-first-order", "\""]
    , concat ["programKey \"", "recursive-tree-deriving", "\""]
    , concat ["Recursive", "Tree", "First", "Order", "Tokens"]
    , concat ["Recursive", "Tree", "Deriving", "Tokens"]
    , concat ["LexerOk ", "recursive", "Tree", "First", "Order", "Tokens"]
    , concat ["LexerOk ", "recursive", "Tree", "Deriving", "Tokens"]
    , concat ["recursive-tree-first-order", " tokens"]
    , concat ["recursive-tree-deriving", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module RecursiveTree", "\""]
    , concat ["stringIndexOf \"", "module RecursiveTree", "\" sourceText"]
    , concat ["defRows sourceFile \"", "mirror", "\""]
    , concat ["defRows sourceFile \"", "isBranch", "\""]
    , concat ["defRows sourceFile \"", "main", "\""]
    , concat ["dataRows sourceFile \"", "Tree", "\""]
    , concat ["constructorRows sourceFile \"", "Branch", "\""]
    , concat ["def main type=Bool expr=", "isBranch (mirror (Branch Leaf Leaf))"]
    , concat ["recursive-tree parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound331ShortcutPhrases :: [String]
sharedParserRound331ShortcutPhrases =
    [ concat ["parse", "Typeclass", "Integration"]
    , concat ["parse", "Recursive", "Adt", "Typeclass", "Integration"]
    , concat ["completeModuleKey \"", "typeclass-integration", "\""]
    , concat ["moduleKey \"", "typeclass-integration", "\""]
    , concat ["programKey \"", "typeclass-integration", "\""]
    , concat ["Typeclass", "Integration", "Tokens"]
    , concat ["LexerOk ", "typeclass", "Integration", "Tokens"]
    , concat ["typeclass-integration", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module TypeclassIntegration export", "\""]
    , concat ["stringIndexOf \"", "module TypeclassIntegration export", "\" sourceText"]
    , concat ["instanceRows sourceFile \"", "Eq", "\" \"", "Nat", "\""]
    , concat ["methodDefinitionRows sourceFile \"", "eq", "\""]
    , concat ["defRows sourceFile \"", "same", "\""]
    , concat ["defRows sourceFile \"", "main", "\" \"", "Bool", "\" \"", "same (Succ (Succ Zero))", "\""]
    , concat ["method-definition eq expr=", "λ(left : Nat) λ(right : Nat) case left"]
    , concat ["def same type=", "Nat -> Nat -> Bool expr=", "λ(left : Nat) λ(right : Nat) eq left right"]
    , concat ["def main type=Bool expr=", "same (Succ (Succ Zero)) (Succ (Succ Zero))"]
    , concat ["typeclass-integration parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound332ShortcutPhrases :: [String]
sharedParserRound332ShortcutPhrases =
    [ concat ["parse", "Abstract", "Recursive", "Adt", "Module", "Use"]
    , concat ["completeModuleKey \"", "abstract-recursive-adt-module-use", "\""]
    , concat ["moduleKey \"", "abstract-recursive-adt-module-use", "\""]
    , concat ["programKey \"", "abstract-recursive-adt-module-use", "\""]
    , concat ["Abstract", "Recursive", "Adt", "Module", "Use", "Tokens"]
    , concat ["LexerOk ", "abstract", "Recursive", "Adt", "Module", "Use", "Tokens"]
    , concat ["abstract-recursive-adt-module-use", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module Core export (Nat, zero, succ, peel, isZero)", "\""]
    , concat ["stringIndexOf \"", "module Core export (Nat, zero, succ, peel, isZero)", "\" sourceText"]
    , concat ["defRows sourceFile \"", "zero", "\" \"", "Nat", "\" \"", "Zero", "\" \"", "6:3-8:3", "\""]
    , concat ["defRows sourceFile \"", "succ", "\" \"", "Nat -> Nat", "\" \"", "λ(n : Nat) Succ n", "\" \"", "8:3-10:3", "\""]
    , concat ["defRows sourceFile \"", "peel", "\""]
    , concat ["defRows sourceFile \"", "isZero", "\""]
    , concat ["defRows sourceFile \"", "main", "\" \"", "Bool", "\" \"", "isZero (peel (succ zero))", "\""]
    , concat ["def main type=Bool expr=", "isZero (peel (succ zero))"]
    , concat ["abstract-recursive-adt-module-use parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound333ShortcutPhrases :: [String]
sharedParserRound333ShortcutPhrases =
    [ concat ["parse", "Module", "Integrated", "Recursive", "Existential"]
    , concat ["completeModuleKey \"", "module-integrated-recursive-existential", "\""]
    , concat ["moduleKey \"", "module-integrated-recursive-existential", "\""]
    , concat ["programKey \"", "module-integrated-recursive-existential", "\""]
    , concat ["Module", "Integrated", "Recursive", "Existential", "Tokens"]
    , concat ["LexerOk ", "module", "Integrated", "Recursive", "Existential", "Tokens"]
    , concat ["module-integrated-recursive-existential", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module Core export (Eq, Nat(..), Expr(..), SomeExpr(..), eq)", "\""]
    , concat ["stringIndexOf \"", "module Core export (Eq, Nat(..), Expr(..), SomeExpr(..), eq)", "\" sourceText"]
    , concat ["defRows sourceFile \"", "peel", "\""]
    , concat ["defRows sourceFile \"", "peelSome", "\""]
    , concat ["defRows sourceFile \"", "main", "\" \"", "Bool", "\" \"", "eq (peelSome"]
    , concat ["def peelSome type=", "SomeExpr -> Nat expr=", "λboxed case boxed of"]
    , concat ["def main type=Bool expr=", "eq (peelSome (SomeExpr (Step (DoneNat (Succ Zero))))) (Succ Zero)"]
    , concat ["module-integrated-recursive-existential parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound334ShortcutPhrases :: [String]
sharedParserRound334ShortcutPhrases =
    [ concat ["parse", "Complex", "Recursive", "Program"]
    , concat ["completeModuleKey \"", "complex-recursive-program", "\""]
    , concat ["moduleKey \"", "complex-recursive-program", "\""]
    , concat ["programKey \"", "complex-recursive-program", "\""]
    , concat ["Complex", "Recursive", "Program", "Tokens"]
    , concat ["LexerOk ", "complex", "Recursive", "Program", "Tokens"]
    , concat ["complex-recursive-program", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module ComplexRecursiveProgram export", "\""]
    , concat ["stringIndexOf \"", "module ComplexRecursiveProgram export", "\" sourceText"]
    , concat ["defRows sourceFile \"", "mirror", "\""]
    , concat ["defRows sourceFile \"", "leftDepth", "\""]
    , concat ["defRows sourceFile \"", "rightDepth", "\""]
    , concat ["defRows sourceFile \"", "main", "\" \"", "Bool", "\" \"", "eq (leftDepth"]
    , concat ["def mirror type=", "Tree -> Tree expr=", "λ(tree : Tree) case tree of"]
    , concat ["def leftDepth type=", "Tree -> Nat expr=", "λ(tree : Tree) case tree of"]
    , concat ["def rightDepth type=", "Tree -> Nat expr=", "λ(tree : Tree) case tree of"]
    , concat ["def main type=Bool expr=", "eq (leftDepth (mirror"]
    , concat ["complex-recursive-program parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound335ShortcutPhrases :: [String]
sharedParserRound335ShortcutPhrases =
    [ concat ["parse", "Deriving", "Eq"]
    , concat ["parse", "Recursive", "Gadt"]
    , concat ["parse", "Recursive", "Existential"]
    , concat ["completeModuleKey \"", "deriving-eq", "\""]
    , concat ["completeModuleKey \"", "recursive-gadt", "\""]
    , concat ["completeModuleKey \"", "recursive-existential", "\""]
    , concat ["moduleKey \"", "deriving-eq", "\""]
    , concat ["moduleKey \"", "recursive-gadt", "\""]
    , concat ["moduleKey \"", "recursive-existential", "\""]
    , concat ["programKey \"", "deriving-eq", "\""]
    , concat ["programKey \"", "recursive-gadt", "\""]
    , concat ["programKey \"", "recursive-existential", "\""]
    , concat ["Deriving", "Eq", "Tokens"]
    , concat ["Recursive", "Gadt", "Tokens"]
    , concat ["Recursive", "Existential", "Tokens"]
    , concat ["LexerOk ", "deriving", "Eq", "Tokens"]
    , concat ["LexerOk ", "recursive", "Gadt", "Tokens"]
    , concat ["LexerOk ", "recursive", "Existential", "Tokens"]
    , concat ["deriving-eq", " tokens"]
    , concat ["recursive-gadt", " tokens"]
    , concat ["recursive-existential", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module DerivingEq export", "\""]
    , concat ["stringIndexOf sourceText \"", "module RecursiveGadt export", "\""]
    , concat ["stringIndexOf sourceText \"", "module RecursiveExistential export", "\""]
    , concat ["stringIndexOf \"", "module DerivingEq export", "\" sourceText"]
    , concat ["stringIndexOf \"", "module RecursiveGadt export", "\" sourceText"]
    , concat ["stringIndexOf \"", "module RecursiveExistential export", "\" sourceText"]
    , concat ["defRows sourceFile \"", "doneNow", "\""]
    , concat ["defRows sourceFile \"", "unwrapSome", "\""]
    , concat ["defRows sourceFile \"", "main", "\" \"", "Bool", "\" \"", "doneNow (Step"]
    , concat ["defRows sourceFile \"", "main", "\" \"", "Bool", "\" \"", "unwrapSome (SomeExpr"]
    , concat ["def doneNow type=", "Expr a -> Bool expr=", "λexpr case expr"]
    , concat ["def unwrapSome type=", "SomeExpr -> Bool expr=", "λboxed case boxed"]
    , concat ["def main type=Bool expr=", "doneNow (Step (DoneNat Zero))"]
    , concat ["def main type=Bool expr=", "unwrapSome (SomeExpr (Step (DoneNat Zero)))"]
    , concat ["named-recursive-adt parser negative ", "expected-case-branch-arrow@"]
    ]

sharedParserRound336ShortcutPhrases :: [String]
sharedParserRound336ShortcutPhrases =
    [ concat ["parse", "Authoritative", "Case", "Analysis"]
    , concat ["parse", "Authoritative", "Let", "Polymorphism"]
    , concat ["parse", "Authoritative", "Nullary", "Overloaded", "Method"]
    , concat ["parse", "Authoritative", "Overloaded", "Method"]
    , concat ["completeModuleKey \"", "authoritative-case-analysis", "\""]
    , concat ["completeModuleKey \"", "authoritative-let-polymorphism", "\""]
    , concat ["completeModuleKey \"", "authoritative-nullary-overloaded-method", "\""]
    , concat ["completeModuleKey \"", "authoritative-overloaded-method", "\""]
    , concat ["moduleKey \"", "authoritative-case-analysis", "\""]
    , concat ["moduleKey \"", "authoritative-let-polymorphism", "\""]
    , concat ["moduleKey \"", "authoritative-nullary-overloaded-method", "\""]
    , concat ["moduleKey \"", "authoritative-overloaded-method", "\""]
    , concat ["programKey \"", "authoritative-case-analysis", "\""]
    , concat ["programKey \"", "authoritative-let-polymorphism", "\""]
    , concat ["programKey \"", "authoritative-nullary-overloaded-method", "\""]
    , concat ["programKey \"", "authoritative-overloaded-method", "\""]
    , concat ["Authoritative", "Case", "Analysis", "Tokens"]
    , concat ["Authoritative", "Let", "Polymorphism", "Tokens"]
    , concat ["Authoritative", "Nullary", "Overloaded", "Method", "Tokens"]
    , concat ["Authoritative", "Overloaded", "Method", "Tokens"]
    , concat ["LexerOk ", "authoritative", "Case", "Analysis", "Tokens"]
    , concat ["LexerOk ", "authoritative", "Let", "Polymorphism", "Tokens"]
    , concat ["LexerOk ", "authoritative", "Nullary", "Overloaded", "Method", "Tokens"]
    , concat ["LexerOk ", "authoritative", "Overloaded", "Method", "Tokens"]
    , concat ["authoritative-case-analysis", " tokens"]
    , concat ["authoritative-let-polymorphism", " tokens"]
    , concat ["authoritative-nullary-overloaded-method", " tokens"]
    , concat ["authoritative-overloaded-method", " tokens"]
    , concat ["stringIndexOf sourceText \"", "test/programs/unified/authoritative-case-analysis", "\""]
    , concat ["stringIndexOf sourceText \"", "test/programs/unified/authoritative-let-polymorphism", "\""]
    , concat ["stringIndexOf sourceText \"", "test/programs/unified/authoritative-nullary-overloaded-method", "\""]
    , concat ["stringIndexOf sourceText \"", "test/programs/unified/authoritative-overloaded-method", "\""]
    , concat ["authoritative-let-polymorphism", " def main type=Int expr=", "let id = λx x in id 1"]
    , concat ["authoritative-case-analysis", " def main type=Int expr=", "case Succ Zero of { Zero -> 0; Succ _ -> 1 }"]
    , concat ["authoritative-nullary-overloaded-method", " def main type=Nat expr=", "append (mempty : Nat) Zero"]
    , concat ["authoritative-overloaded-method", " def main type=Bool expr=", "eq (Succ Zero) (Succ Zero)"]
    , concat ["authoritative-unified parser negative ", "expected-def-semicolon@"]
    ]

sharedParserRound337ShortcutPhrases :: [String]
sharedParserRound337ShortcutPhrases =
    [ concat ["parse", "Package", "Cross", "Module", "Let"]
    , concat ["parse", "Package", "Search", "Path", "Import"]
    , concat ["render", "Package", "Cross", "Module", "Let"]
    , concat ["render", "Package", "Search", "Path", "Import"]
    , concat ["completeModuleKey \"", "package-cross-module-let", "\""]
    , concat ["completeModuleKey \"", "package-search-path-import", "\""]
    , concat ["moduleKey \"", "package-cross-module-let", "\""]
    , concat ["moduleKey \"", "package-search-path-import", "\""]
    , concat ["programKey \"", "package-cross-module-let", "\""]
    , concat ["programKey \"", "package-search-path-import", "\""]
    , concat ["Package", "Cross", "Module", "Let", "Tokens"]
    , concat ["Package", "Search", "Path", "Import", "Tokens"]
    , concat ["LexerOk ", "package", "Cross", "Module", "Let", "Tokens"]
    , concat ["LexerOk ", "package", "Search", "Path", "Import", "Tokens"]
    , concat ["package-cross-module-let", " tokens"]
    , concat ["package-search-path-import", " tokens"]
    , concat ["stringIndexOf sourceText \"", "test/conformance/mlfp/parser-parity/package-cross-module-let", "\""]
    , concat ["stringIndexOf sourceText \"", "test/conformance/mlfp/parser-parity/package-search-path-import", "\""]
    , concat ["stringIndexOf firstSourceText \"", "module Core export", "\""]
    , concat ["stringIndexOf secondSourceText \"", "module Main export", "\""]
    , "parseWholePackage"
    , "renderWholePackage"
    , "concatPackageSourceText"
    , "combinedPackageSourceText"
    , "preRenderedPackageProjection"
    , concat ["package-layout parser negative ", "expected-import-semicolon@"]
    ]

sharedParserRound338ShortcutPhrases :: [String]
sharedParserRound338ShortcutPhrases =
    [ concat ["parse", "Compiler", "Seed", "Data", "Model"]
    , concat ["parse", "Seed", "Source"]
    , concat ["parse", "Seed", "Token"]
    , concat ["parse", "Seed", "Diagnostic"]
    , concat ["parse", "Seed", "Ast"]
    , concat ["render", "Compiler", "Seed", "Data", "Model"]
    , concat ["completeModuleKey \"", "SeedSource", "\""]
    , concat ["completeModuleKey \"", "SeedToken", "\""]
    , concat ["completeModuleKey \"", "SeedDiagnostic", "\""]
    , concat ["completeModuleKey \"", "SeedAst", "\""]
    , concat ["moduleKey \"", "SeedSource", "\""]
    , concat ["moduleKey \"", "SeedToken", "\""]
    , concat ["moduleKey \"", "SeedDiagnostic", "\""]
    , concat ["moduleKey \"", "SeedAst", "\""]
    , concat ["Seed", "Source", "Tokens"]
    , concat ["Seed", "Token", "Tokens"]
    , concat ["Seed", "Diagnostic", "Tokens"]
    , concat ["Seed", "Ast", "Tokens"]
    , concat ["LexerOk ", "seed", "Source", "Tokens"]
    , concat ["LexerOk ", "seed", "Token", "Tokens"]
    , concat ["LexerOk ", "seed", "Diagnostic", "Tokens"]
    , concat ["LexerOk ", "seed", "Ast", "Tokens"]
    , concat ["compiler-seed-data-model", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module SeedSource export", "\""]
    , concat ["stringIndexOf sourceText \"", "module SeedToken export", "\""]
    , concat ["stringIndexOf sourceText \"", "module SeedDiagnostic export", "\""]
    , concat ["stringIndexOf sourceText \"", "module SeedAst export", "\""]
    , concat ["stringIndexOf firstSourceText \"", "module SeedSource export", "\""]
    , concat ["stringIndexOf secondSourceText \"", "module SeedToken export", "\""]
    , concat ["stringIndexOf thirdSourceText \"", "module SeedDiagnostic export", "\""]
    , concat ["stringIndexOf fourthSourceText \"", "module SeedAst export", "\""]
    , concat ["module SeedSource span=", "test/conformance/mlfp/parser-parity/compiler-seed-data-model"]
    , concat ["constructor Line1Column1 type=SourcePosition span=", "test/conformance/mlfp/parser-parity/compiler-seed-data-model"]
    , concat ["def spanStart type=SourceSpan -> SourcePosition expr=", "λ(span : SourceSpan) case span of"]
    , concat ["def positiveSeedInput type=SeedInput expr=", "SeedInputCons"]
    , concat ["compiler-seed-data-model parser negative ", "expected-case-branch-arrow@"]
    , "preRenderedCompilerSeedDataModelProjection"
    , "compilerSeedDataModelProjectionRows"
    ]

sharedParserRound339ShortcutPhrases :: [String]
sharedParserRound339ShortcutPhrases =
    [ concat ["parse", "Compiler", "Seed", "Lexer"]
    , concat ["parse", "Seed", "Lexer"]
    , concat ["render", "Compiler", "Seed", "Lexer"]
    , concat ["completeModuleKey \"", "SeedLexer", "\""]
    , concat ["moduleKey \"", "SeedLexer", "\""]
    , concat ["Seed", "Lexer", "Tokens"]
    , concat ["LexerOk ", "seed", "Lexer", "Tokens"]
    , concat ["compiler-seed-lexer", " tokens"]
    , concat ["stringIndexOf sourceText \"", "module SeedLexer export", "\""]
    , concat ["module SeedLexer span=", "test/conformance/mlfp/parser-parity/compiler-seed-lexer"]
    , concat ["def lexSeedInput type=SeedInput -> LexerResult expr=", "λ(input : SeedInput) case input of"]
    , concat ["def lexAfterLiteral type=SourceSpan -> SourceSpan -> SeedIdentifier", " -> SourceSpan -> SourceSpan"]
    , concat ["lexer-positive:def-main-equals-true", ";lexer-negative:unknown@span-unknown-symbol"]
    , concat ["compiler-seed-lexer parser negative ", "expected-case-branch-arrow@"]
    , "preRenderedCompilerSeedLexerProjection"
    , "compilerSeedLexerProjectionRows"
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
    , concat ["stringAppend \"higher-order-partial-application parser negative ", "expected-expression-close-paren@\""]
    , concat ["stringAppend \"higher-order-local-function-flow parser negative ", "expected-let-in@\""]
    , concat ["stringAppend \"higher-order-returned-function parser negative ", "expected-expression-close-paren@\""]
    , concat ["stringAppend \"higher-order-function-field parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"authoritative-recursive-let parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"authoritative-cross-module-let-polymorphism parser negative ", "expected-def-semicolon@\""]
    , concat ["stringAppend \"authoritative-unified parser negative ", "expected-def-semicolon@\""]
    , concat ["stringAppend \"recursive-adt-plain-nat parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"recursive-list-tail parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"recursive-tree parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"typeclass-integration parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"abstract-recursive-adt-module-use parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"complex-recursive-program parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"named-recursive-adt parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"package-layout parser negative ", "expected-import-semicolon@\""]
    , concat ["stringAppend \"compiler-seed-data-model parser negative ", "expected-case-branch-arrow@\""]
    , concat ["stringAppend \"compiler-seed-lexer parser negative ", "expected-case-branch-arrow@\""]
    ]

sharedParserDynamicEvidenceRequiredPhrases :: [String]
sharedParserDynamicEvidenceRequiredPhrases =
    [ "parseCompleteModule sourceText"
    , "parseCompleteProgram sourceText"
    , "tokenizeCompleteModule sourceText"
    , "tokenizeCompleteModule lexerMismatchSourceText"
    , "renderParserParityPackageProjectionFromSourceTexts"
    , "renderParserParityPackageProjectionFromFourSourceTexts"
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

data ParserParityPackageSource = ParserParityPackageSource
    { packageSourceIdentifier :: String
    , packageSourcePath :: FilePath
    }

data ParserParityPackagePositiveCase = ParserParityPackagePositiveCase
    { packagePositiveCaseLabel :: String
    , packagePositiveCaseIdentifier :: String
    , packagePositiveCaseSources :: [ParserParityPackageSource]
    , packagePositiveCaseExpectedPath :: FilePath
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
    , ParserParityPositiveCase "positive:deriving-eq" "positiveDerivingEq" derivingEqCanonicalSourcePath derivingEqExpectedProjectionPath
    , ParserParityPositiveCase "positive:typeclass-instance-nullary-method" "positiveTypeclassInstanceNullaryMethod" typeclassInstanceNullaryMethodCanonicalSourcePath typeclassInstanceNullaryMethodExpectedProjectionPath
    , ParserParityPositiveCase "positive:higher-kinded-class-data-params" "positiveHigherKindedClassDataParams" higherKindedClassDataParamsCanonicalSourcePath higherKindedClassDataParamsExpectedProjectionPath
    , ParserParityPositiveCase "positive:multiparam-superclass-fundep" "positiveMultiparamSuperclassFundep" multiparamSuperclassFundepCanonicalSourcePath multiparamSuperclassFundepExpectedProjectionPath
    , ParserParityPositiveCase "positive:type-family-kind-lambda" "positiveTypeFamilyKindLambda" typeFamilyKindLambdaCanonicalSourcePath typeFamilyKindLambdaExpectedProjectionPath
    , ParserParityPositiveCase "positive:type-family-apply-annotation" "positiveTypeFamilyApplyAnnotation" typeFamilyApplyAnnotationCanonicalSourcePath typeFamilyApplyAnnotationExpectedProjectionPath
    , ParserParityPositiveCase "positive:gadt-result-constructor-spans" "positiveGadtResultConstructorSpans" gadtResultConstructorSpansCanonicalSourcePath gadtResultConstructorSpansExpectedProjectionPath
    , ParserParityPositiveCase "positive:existential-constructor-forall" "positiveExistentialConstructorForall" existentialConstructorForallCanonicalSourcePath existentialConstructorForallExpectedProjectionPath
    , ParserParityPositiveCase "positive:recursive-gadt" "positiveRecursiveGadt" recursiveGadtCanonicalSourcePath recursiveGadtExpectedProjectionPath
    , ParserParityPositiveCase "positive:recursive-existential" "positiveRecursiveExistential" recursiveExistentialCanonicalSourcePath recursiveExistentialExpectedProjectionPath
    , ParserParityPositiveCase "positive:qualified-import-alias-references" "positiveQualifiedImportAliasReferences" qualifiedImportAliasReferencesCanonicalSourcePath qualifiedImportAliasReferencesExpectedProjectionPath
    , ParserParityPositiveCase "positive:qualified-import-alias-only" "positiveQualifiedImportAliasOnly" qualifiedImportAliasOnlyCanonicalSourcePath qualifiedImportAliasOnlyExpectedProjectionPath
    , ParserParityPositiveCase "positive:multi-module-abstract-export-import" "positiveMultiModuleAbstractExportImport" multiModuleAbstractExportImportCanonicalSourcePath multiModuleAbstractExportImportExpectedProjectionPath
    , ParserParityPositiveCase "positive:multi-module-recursive-adt-export-import" "positiveMultiModuleRecursiveAdtExportImport" multiModuleRecursiveAdtExportImportCanonicalSourcePath multiModuleRecursiveAdtExportImportExpectedProjectionPath
    , ParserParityPositiveCase "positive:text-literal-char-string" "positiveTextLiteralCharString" textLiteralCharStringCanonicalSourcePath textLiteralCharStringExpectedProjectionPath
    , ParserParityPositiveCase "positive:first-class-polymorphism-source-types" "positiveFirstClassPolymorphismSourceTypes" firstClassPolymorphismSourceTypesCanonicalSourcePath firstClassPolymorphismSourceTypesExpectedProjectionPath
    , ParserParityPositiveCase "positive:higher-order-partial-application" "positiveHigherOrderPartialApplication" higherOrderPartialApplicationCanonicalSourcePath higherOrderPartialApplicationExpectedProjectionPath
    , ParserParityPositiveCase "positive:higher-order-local-function-flow" "positiveHigherOrderLocalFunctionFlow" higherOrderLocalFunctionFlowCanonicalSourcePath higherOrderLocalFunctionFlowExpectedProjectionPath
    , ParserParityPositiveCase "positive:higher-order-returned-function" "positiveHigherOrderReturnedFunction" higherOrderReturnedFunctionCanonicalSourcePath higherOrderReturnedFunctionExpectedProjectionPath
    , ParserParityPositiveCase "positive:higher-order-function-field" "positiveHigherOrderFunctionField" higherOrderFunctionFieldCanonicalSourcePath higherOrderFunctionFieldExpectedProjectionPath
    , ParserParityPositiveCase "positive:authoritative-recursive-let" "positiveAuthoritativeRecursiveLet" authoritativeRecursiveLetCanonicalSourcePath authoritativeRecursiveLetExpectedProjectionPath
    , ParserParityPositiveCase "positive:authoritative-cross-module-let-polymorphism" "positiveAuthoritativeCrossModuleLetPolymorphism" authoritativeCrossModuleLetPolymorphismCanonicalSourcePath authoritativeCrossModuleLetPolymorphismExpectedProjectionPath
    , ParserParityPositiveCase "positive:authoritative-case-analysis" "positiveAuthoritativeCaseAnalysis" authoritativeCaseAnalysisCanonicalSourcePath authoritativeCaseAnalysisExpectedProjectionPath
    , ParserParityPositiveCase "positive:authoritative-let-polymorphism" "positiveAuthoritativeLetPolymorphism" authoritativeLetPolymorphismCanonicalSourcePath authoritativeLetPolymorphismExpectedProjectionPath
    , ParserParityPositiveCase "positive:authoritative-nullary-overloaded-method" "positiveAuthoritativeNullaryOverloadedMethod" authoritativeNullaryOverloadedMethodCanonicalSourcePath authoritativeNullaryOverloadedMethodExpectedProjectionPath
    , ParserParityPositiveCase "positive:authoritative-overloaded-method" "positiveAuthoritativeOverloadedMethod" authoritativeOverloadedMethodCanonicalSourcePath authoritativeOverloadedMethodExpectedProjectionPath
    , ParserParityPositiveCase "positive:recursive-adt-plain-nat" "positiveRecursiveAdtPlainNat" recursiveAdtPlainNatCanonicalSourcePath recursiveAdtPlainNatExpectedProjectionPath
    , ParserParityPositiveCase "positive:recursive-list-tail" "positiveRecursiveListTail" recursiveListTailCanonicalSourcePath recursiveListTailExpectedProjectionPath
    , ParserParityPositiveCase "positive:recursive-tree-first-order" "positiveRecursiveTreeFirstOrder" recursiveTreeFirstOrderCanonicalSourcePath recursiveTreeFirstOrderExpectedProjectionPath
    , ParserParityPositiveCase "positive:recursive-tree-deriving" "positiveRecursiveTreeDeriving" recursiveTreeDerivingCanonicalSourcePath recursiveTreeDerivingExpectedProjectionPath
    , ParserParityPositiveCase "positive:typeclass-integration" "positiveTypeclassIntegration" typeclassIntegrationCanonicalSourcePath typeclassIntegrationExpectedProjectionPath
    , ParserParityPositiveCase "positive:abstract-recursive-adt-module-use" "positiveAbstractRecursiveAdtModuleUse" abstractRecursiveAdtModuleUseCanonicalSourcePath abstractRecursiveAdtModuleUseExpectedProjectionPath
    , ParserParityPositiveCase "positive:module-integrated-recursive-existential" "positiveModuleIntegratedRecursiveExistential" moduleIntegratedRecursiveExistentialCanonicalSourcePath moduleIntegratedRecursiveExistentialExpectedProjectionPath
    , ParserParityPositiveCase "positive:complex-recursive-program" "positiveComplexRecursiveProgram" complexRecursiveProgramCanonicalSourcePath complexRecursiveProgramExpectedProjectionPath
    , ParserParityPositiveCase "positive:compiler-seed-lexer" "positiveCompilerSeedLexer" compilerSeedLexerSourcePath compilerSeedLexerExpectedProjectionPath
    ]

parserParityPackagePositiveCases :: [ParserParityPackagePositiveCase]
parserParityPackagePositiveCases =
    [ ParserParityPackagePositiveCase
        "positive:package-cross-module-let"
        "positivePackageCrossModuleLet"
        [ ParserParityPackageSource "Core" packageCrossModuleLetCoreSourcePath
        , ParserParityPackageSource "Main" packageCrossModuleLetMainSourcePath
        ]
        packageCrossModuleLetExpectedProjectionPath
    , ParserParityPackagePositiveCase
        "positive:package-search-path-import"
        "positivePackageSearchPathImport"
        [ ParserParityPackageSource "SearchLib" packageSearchPathImportLibSourcePath
        , ParserParityPackageSource "Main" packageSearchPathImportMainSourcePath
        ]
        packageSearchPathImportExpectedProjectionPath
    , ParserParityPackagePositiveCase
        "positive:compiler-seed-data-model"
        "positiveCompilerSeedDataModel"
        [ ParserParityPackageSource "SeedSource" compilerSeedDataModelSeedSourceSourcePath
        , ParserParityPackageSource "SeedToken" compilerSeedDataModelSeedTokenSourcePath
        , ParserParityPackageSource "SeedDiagnostic" compilerSeedDataModelSeedDiagnosticSourcePath
        , ParserParityPackageSource "SeedAst" compilerSeedDataModelSeedAstSourcePath
        ]
        compilerSeedDataModelExpectedProjectionPath
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
    , ParserParityNegativeCase "negative:higher-order-partial-application" "negativeHigherOrderPartialApplication" "higher-order-partial-application parser negative " higherOrderPartialApplicationCanonicalSourcePath higherOrderPartialApplicationNegativeSourceText higherOrderPartialApplicationNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:higher-order-local-function-flow" "negativeHigherOrderLocalFunctionFlow" "higher-order-local-function-flow parser negative " higherOrderLocalFunctionFlowCanonicalSourcePath higherOrderLocalFunctionFlowNegativeSourceText higherOrderLocalFunctionFlowNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:higher-order-returned-function" "negativeHigherOrderReturnedFunction" "higher-order-returned-function parser negative " higherOrderReturnedFunctionCanonicalSourcePath higherOrderReturnedFunctionNegativeSourceText higherOrderReturnedFunctionNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:higher-order-function-field" "negativeHigherOrderFunctionField" "higher-order-function-field parser negative " higherOrderFunctionFieldCanonicalSourcePath higherOrderFunctionFieldNegativeSourceText higherOrderFunctionFieldNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:authoritative-recursive-let" "negativeAuthoritativeRecursiveLet" "authoritative-recursive-let parser negative " authoritativeRecursiveLetCanonicalSourcePath authoritativeRecursiveLetNegativeSourceText authoritativeRecursiveLetNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:authoritative-cross-module-let-polymorphism" "negativeAuthoritativeCrossModuleLetPolymorphism" "authoritative-cross-module-let-polymorphism parser negative " authoritativeCrossModuleLetPolymorphismCanonicalSourcePath authoritativeCrossModuleLetPolymorphismNegativeSourceText authoritativeCrossModuleLetPolymorphismNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:authoritative-unified-let-polymorphism" "negativeAuthoritativeUnifiedLetPolymorphism" "authoritative-unified parser negative " authoritativeLetPolymorphismCanonicalSourcePath authoritativeUnifiedLetPolymorphismNegativeSourceText authoritativeUnifiedLetPolymorphismNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:recursive-adt-plain-nat" "negativeRecursiveAdtPlainNat" "recursive-adt-plain-nat parser negative " recursiveAdtPlainNatCanonicalSourcePath recursiveAdtPlainNatNegativeSourceText recursiveAdtPlainNatNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:recursive-list-tail" "negativeRecursiveListTail" "recursive-list-tail parser negative " recursiveListTailCanonicalSourcePath recursiveListTailNegativeSourceText recursiveListTailNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:recursive-tree-branch-arrow" "negativeRecursiveTreeBranchArrow" "recursive-tree parser negative " recursiveTreeFirstOrderCanonicalSourcePath recursiveTreeNegativeSourceText recursiveTreeNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:typeclass-integration-nested-case" "negativeTypeclassIntegrationNestedCase" "typeclass-integration parser negative " typeclassIntegrationCanonicalSourcePath typeclassIntegrationNegativeSourceText typeclassIntegrationNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:abstract-recursive-adt-module-use" "negativeAbstractRecursiveAdtModuleUse" "abstract-recursive-adt-module-use parser negative " abstractRecursiveAdtModuleUseCanonicalSourcePath abstractRecursiveAdtModuleUseNegativeSourceText abstractRecursiveAdtModuleUseNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:module-integrated-recursive-existential" "negativeModuleIntegratedRecursiveExistential" "module-integrated-recursive-existential parser negative " moduleIntegratedRecursiveExistentialCanonicalSourcePath moduleIntegratedRecursiveExistentialNegativeSourceText moduleIntegratedRecursiveExistentialNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:complex-recursive-program" "negativeComplexRecursiveProgram" "complex-recursive-program parser negative " complexRecursiveProgramCanonicalSourcePath complexRecursiveProgramNegativeSourceText complexRecursiveProgramNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:named-recursive-adt-case-branch" "negativeNamedRecursiveAdtCaseBranch" "named-recursive-adt parser negative " recursiveGadtCanonicalSourcePath namedRecursiveAdtNegativeSourceText namedRecursiveAdtNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:package-cross-module-let-import-semicolon" "negativePackageCrossModuleLetImportSemicolon" "package-layout parser negative " packageCrossModuleLetMainSourcePath packageLayoutImportSemicolonNegativeSourceText packageLayoutImportSemicolonNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:compiler-seed-data-model-case-branch" "negativeCompilerSeedDataModelCaseBranch" "compiler-seed-data-model parser negative " compilerSeedDataModelSeedSourceSourcePath compilerSeedDataModelCaseBranchNegativeSourceText compilerSeedDataModelCaseBranchNegativeEvidenceProjection
    , ParserParityNegativeCase "negative:compiler-seed-lexer-case-branch" "negativeCompilerSeedLexerCaseBranch" "compiler-seed-lexer parser negative " compilerSeedLexerSourcePath compilerSeedLexerCaseBranchNegativeSourceText compilerSeedLexerCaseBranchNegativeEvidenceProjection
    ]

assertCanonicalParserParityProjection :: ParserParityPositiveCase -> IO ()
assertCanonicalParserParityProjection testCase = do
    source <- readFile (positiveCaseSourcePath testCase)
    expected <- readFile (positiveCaseExpectedPath testCase)
    canonicalProjection <- renderCanonicalProjection (positiveCaseSourcePath testCase) source
    canonicalProjection `shouldBe` expected

assertSharedParserParityProjection :: FilePath -> FilePath -> FilePath -> IO ()
assertSharedParserParityProjection sourcePath expectedPath parserRoot = do
    source <- readFile sourcePath
    expected <- readFile expectedPath
    canonicalProjection <- renderCanonicalProjection sourcePath source
    sharedParserProjection <- runSharedParserBatch parserRoot

    canonicalProjection `shouldBe` expected
    sharedParserProjection `shouldBe` Right expected

assertSharedPackageParserParityProjection :: [FilePath] -> FilePath -> FilePath -> IO ()
assertSharedPackageParserParityProjection sourcePaths expectedPath parserRoot = do
    expected <- readFile expectedPath
    canonicalProjection <- renderCanonicalPackageProjection sourcePaths
    sharedParserProjection <- runSharedParserBatch parserRoot

    canonicalProjection `shouldBe` expected
    sharedParserProjection `shouldBe` Right expected

assertSourceCopy :: (FilePath, FilePath) -> IO ()
assertSourceCopy (sourcePath, copiedPath) = do
    source <- readFile sourcePath
    copied <- readFile copiedPath
    copied `shouldBe` source

writeParserParityBatchPackage :: IO FilePath
writeParserParityBatchPackage = do
    loadedPositiveCases <- traverse loadPositiveParserParityCase parserParityPositiveCases
    loadedPackagePositiveCases <-
        traverse loadPositivePackageParserParityCase parserParityPackagePositiveCases
    removePathForcibly parserParityBatchPackageRoot
    createDirectoryIfMissing True parserParityBatchPackageRoot
    writeFile
        (parserParityBatchPackageRoot </> "Main.mlfp")
        (parserParityBatchMainSource loadedPositiveCases loadedPackagePositiveCases)
    pure parserParityBatchPackageRoot

loadPositiveParserParityCase :: ParserParityPositiveCase -> IO (ParserParityPositiveCase, String)
loadPositiveParserParityCase testCase = do
    sourceText <- readFile (positiveCaseSourcePath testCase)
    pure (testCase, sourceText)

loadPositivePackageParserParityCase ::
    ParserParityPackagePositiveCase ->
    IO (ParserParityPackagePositiveCase, [(ParserParityPackageSource, String)])
loadPositivePackageParserParityCase testCase = do
    loadedSources <- traverse loadPackageSource (packagePositiveCaseSources testCase)
    pure (testCase, loadedSources)
  where
    loadPackageSource sourceCase = do
        sourceText <- readFile (packageSourcePath sourceCase)
        pure (sourceCase, sourceText)

parserParityBatchMainSource ::
    [(ParserParityPositiveCase, String)] ->
    [(ParserParityPackagePositiveCase, [(ParserParityPackageSource, String)])] ->
    String
parserParityBatchMainSource loadedPositiveCases loadedPackagePositiveCases =
    unlines $
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStr, stringAppend);"
        , "  import ParserParityParser exposing (renderParserParityProjectionFromSourceText, renderParserParityPackageProjectionFromSourceTexts, renderParserParityPackageProjectionFromFourSourceTexts, renderParserParityRetryEvidence, renderParserNegativeEvidenceFromSourceText);"
        , ""
        , "  def section : String -> String -> String ="
        , "    λ(label : String) λ(output : String)"
        , "      stringAppend \"== \" (stringAppend label (stringAppend \" ==\\n\" (stringAppend output \"\\n\")));"
        , ""
        ]
            ++ concatMap renderPositiveBatchDefinitions loadedPositiveCases
            ++ concatMap renderPackagePositiveBatchDefinitions loadedPackagePositiveCases
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
            ++ map ((<> "Section") . packagePositiveCaseIdentifier . fst) loadedPackagePositiveCases
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

renderPackagePositiveBatchDefinitions ::
    (ParserParityPackagePositiveCase, [(ParserParityPackageSource, String)]) ->
    [String]
renderPackagePositiveBatchDefinitions (testCase, loadedSources) =
    concatMap (renderPackageSourceBatchDefinitions ident) loadedSources
        ++ [ "  def " <> ident <> "Section : String ="
           , "    section "
                <> show (packagePositiveCaseLabel testCase)
                <> " ("
                <> packageProjectionRendererCall ident loadedSources
                <> ");"
           , ""
           ]
  where
    ident = packagePositiveCaseIdentifier testCase

renderPackageSourceBatchDefinitions :: String -> (ParserParityPackageSource, String) -> [String]
renderPackageSourceBatchDefinitions prefix (sourceCase, sourceText) =
    [ "  def " <> ident <> "SourceFile : String ="
    , "    " <> show (packageSourcePath sourceCase) <> ";"
    , ""
    , "  def " <> ident <> "SourceText : String ="
    , "    " <> show sourceText <> ";"
    , ""
    ]
  where
    ident = packageSourceBatchIdentifierWithPrefix prefix sourceCase

packageProjectionRendererCall ::
    String ->
    [(ParserParityPackageSource, String)] ->
    String
packageProjectionRendererCall ident loadedSources =
    case loadedSources of
        [(firstSource, _), (secondSource, _)] ->
            "renderParserParityPackageProjectionFromSourceTexts "
                <> packageSourceCallPrefix ident firstSource
                <> " "
                <> packageSourceCallPrefix ident secondSource
        [(firstSource, _), (secondSource, _), (thirdSource, _), (fourthSource, _)] ->
            "renderParserParityPackageProjectionFromFourSourceTexts "
                <> packageSourceCallPrefix ident firstSource
                <> " "
                <> packageSourceCallPrefix ident secondSource
                <> " "
                <> packageSourceCallPrefix ident thirdSource
                <> " "
                <> packageSourceCallPrefix ident fourthSource
        _ -> error "package parser parity fixtures require exactly two or four source files"

packageSourceCallPrefix :: String -> ParserParityPackageSource -> String
packageSourceCallPrefix ident sourceCase =
    packageSourceBatchIdentifierWithPrefix ident sourceCase
        <> "SourceFile "
        <> packageSourceBatchIdentifierWithPrefix ident sourceCase
        <> "SourceText"

packageSourceBatchIdentifierWithPrefix :: String -> ParserParityPackageSource -> String
packageSourceBatchIdentifierWithPrefix prefix sourceCase =
    prefix <> packageSourceIdentifier sourceCase

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
    packagePositiveSections <-
        traverse expectedPackagePositiveParserParitySection parserParityPackagePositiveCases
    let negativeSections =
            map expectedNegativeParserParitySection parserParityNegativeCases
    pure $
        concat
            ( positiveSections
                ++ packagePositiveSections
                ++ negativeSections
                ++ [batchSection retryEvidenceLabel retryEvidenceProjection]
            )

expectedPositiveParserParitySection :: ParserParityPositiveCase -> IO String
expectedPositiveParserParitySection testCase =
    batchSection (positiveCaseLabel testCase)
        <$> readFile (positiveCaseExpectedPath testCase)

expectedPackagePositiveParserParitySection :: ParserParityPackagePositiveCase -> IO String
expectedPackagePositiveParserParitySection testCase =
    batchSection (packagePositiveCaseLabel testCase)
        <$> readFile (packagePositiveCaseExpectedPath testCase)

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

higherOrderPartialApplicationNegativeSourceText :: String
higherOrderPartialApplicationNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  def keepLeft : Int -> Int -> Int = λx λy x;"
        , "  def apply : (Int -> Int) -> Int = λf f 2;"
        , ""
        , "  def main : Int = apply (keepLeft 1;"
        , "}"
        ]

higherOrderLocalFunctionFlowNegativeSourceText :: String
higherOrderLocalFunctionFlowNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;"
        , ""
        , "  def main : Int ="
        , "    let captured : Int = 41 in"
        , "    let f : Int -> Int = λ(x : Int) captured;"
        , "}"
        ]

higherOrderReturnedFunctionNegativeSourceText :: String
higherOrderReturnedFunctionNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  def make : Int -> (Int -> Int) ="
        , "    λ(base : Int)"
        , "      let captured : Int = base in"
        , "      λ(x : Int) captured;"
        , ""
        , "  def main : Int = (make 41 0;"
        , "}"
        ]

higherOrderFunctionFieldNegativeSourceText :: String
higherOrderFunctionFieldNegativeSourceText =
    unlines
        [ "module Main export (FnBox(..), main) {"
        , "  data FnBox ="
        , "      FnBox : (Int -> Int) -> FnBox;"
        , ""
        , "  def main : Int ="
        , "    let captured : Int = 41 in"
        , "    let f : Int -> Int = λ(x : Int) captured in"
        , "    case FnBox f of { FnBox g g 0 };"
        , "}"
        ]

authoritativeRecursiveLetNegativeSourceText :: String
authoritativeRecursiveLetNegativeSourceText =
    unlines
        [ "module Main export (Nat(..), main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  def main : Bool ="
        , "    let peel : Nat -> Nat = λ(n : Nat) case n of {"
        , "      Zero Zero;"
        , "      Succ inner -> peel inner"
        , "    } in"
        , "    case peel (Succ Zero) of {"
        , "      Zero -> true;"
        , "      Succ _ -> false"
        , "    };"
        , "}"
        ]

authoritativeCrossModuleLetPolymorphismNegativeSourceText :: String
authoritativeCrossModuleLetPolymorphismNegativeSourceText =
    unlines
        [ "module Core export (applyId) {"
        , "  def applyId : Int = let id = λx x in id 1"
        , "}"
        , ""
        , "module User export (main) {"
        , "  import Core exposing (applyId);"
        , "  def main : Int = applyId;"
        , "}"
        ]

authoritativeUnifiedLetPolymorphismNegativeSourceText :: String
authoritativeUnifiedLetPolymorphismNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  def main : Int = let id = λx x in id 1"
        , "}"
        ]

recursiveAdtPlainNatNegativeSourceText :: String
recursiveAdtPlainNatNegativeSourceText =
    unlines
        [ "module NatPlain export (Nat(..), isZero, peel, main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  def isZero : Nat -> Bool = λ(n : Nat) case n of {"
        , "    Zero -> true;"
        , "    Succ _ -> false"
        , "  };"
        , ""
        , "  def peel : Nat -> Nat = λ(n : Nat) case n of {"
        , "    Zero -> Zero;"
        , "    Succ inner inner"
        , "  };"
        , ""
        , "  def main : Bool = isZero (peel (Succ Zero));"
        , "}"
        ]

recursiveListTailNegativeSourceText :: String
recursiveListTailNegativeSourceText =
    unlines
        [ "module RecursiveList export (Nat(..), List(..), tailOrNil, isNil, main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  data List ="
        , "      Nil : List"
        , "    | Cons : Nat -> List -> List;"
        , ""
        , "  def tailOrNil : List -> List = λ(xs : List) case xs of {"
        , "    Nil -> Nil;"
        , "    Cons _ rest rest"
        , "  };"
        , ""
        , "  def isNil : List -> Bool = λ(xs : List) case xs of {"
        , "    Nil -> true;"
        , "    Cons _ _ -> false"
        , "  };"
        , ""
        , "  def main : Bool = isNil (tailOrNil (Cons Zero Nil));"
        , "}"
        ]

recursiveTreeNegativeSourceText :: String
recursiveTreeNegativeSourceText =
    unlines
        [ "module RecursiveTreeFirstOrder export (Tree(..), mirror, isBranch, main) {"
        , "  data Tree ="
        , "      Leaf : Tree"
        , "    | Branch : Tree -> Tree -> Tree;"
        , ""
        , "  def mirror : Tree -> Tree = λ(tree : Tree) case tree of {"
        , "    Leaf -> Leaf;"
        , "    Branch left right Branch (mirror right) (mirror left)"
        , "  };"
        , ""
        , "  def isBranch : Tree -> Bool = λ(tree : Tree) case tree of {"
        , "    Leaf -> false;"
        , "    Branch _ _ -> true"
        , "  };"
        , ""
        , "  def main : Bool = isBranch (mirror (Branch Leaf Leaf));"
        , "}"
        ]

typeclassIntegrationNegativeSourceText :: String
typeclassIntegrationNegativeSourceText =
    unlines
        [ "module TypeclassIntegration export (Eq, Nat(..), eq, same, main) {"
        , "  class Eq a {"
        , "    eq : a -> a -> Bool;"
        , "  }"
        , ""
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  instance Eq Nat {"
        , "    eq = λ(left : Nat) λ(right : Nat) case left of {"
        , "      Zero -> case right of {"
        , "        Zero true;"
        , "        Succ _ -> false"
        , "      };"
        , "      Succ leftRest -> case right of {"
        , "        Zero -> false;"
        , "        Succ rightRest -> eq leftRest rightRest"
        , "      }"
        , "    };"
        , "  }"
        , ""
        , "  def same : Nat -> Nat -> Bool = λ(left : Nat) λ(right : Nat) eq left right;"
        , ""
        , "  def main : Bool = same (Succ (Succ Zero)) (Succ (Succ Zero));"
        , "}"
        ]

abstractRecursiveAdtModuleUseNegativeSourceText :: String
abstractRecursiveAdtModuleUseNegativeSourceText =
    unlines
        [ "module Core export (Nat, zero, succ, peel, isZero) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  def zero : Nat = Zero;"
        , ""
        , "  def succ : Nat -> Nat = λ(n : Nat) Succ n;"
        , ""
        , "  def peel : Nat -> Nat = λ(n : Nat) case n of {"
        , "    Zero -> Zero;"
        , "    Succ inner inner"
        , "  };"
        , ""
        , "  def isZero : Nat -> Bool = λ(n : Nat) case n of {"
        , "    Zero -> true;"
        , "    Succ _ -> false"
        , "  };"
        , "}"
        , ""
        , "module User export (main) {"
        , "  import Core exposing (Nat, zero, succ, peel, isZero);"
        , ""
        , "  def main : Bool = isZero (peel (succ zero));"
        , "}"
        ]

moduleIntegratedRecursiveExistentialNegativeSourceText :: String
moduleIntegratedRecursiveExistentialNegativeSourceText =
    unlines
        [ "module Core export (Eq, Nat(..), Expr(..), SomeExpr(..), eq) {"
        , "  class Eq a {"
        , "    eq : a -> a -> Bool;"
        , "  }"
        , ""
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat"
        , "    deriving Eq;"
        , ""
        , "  data Expr a ="
        , "      DoneNat : Nat -> Expr Nat"
        , "    | Step : Expr a -> Expr a;"
        , ""
        , "  data SomeExpr ="
        , "      SomeExpr : ∀ a. Expr a -> SomeExpr;"
        , "}"
        , ""
        , "module User export (peelSome, main) {"
        , "  import Core exposing (Nat(..), Expr(..), SomeExpr(..), eq);"
        , ""
        , "  def peel : Expr a -> Nat = λ(expr) case expr of {"
        , "    DoneNat n -> n;"
        , "    Step next -> peel next"
        , "  };"
        , ""
        , "  def peelSome : SomeExpr -> Nat = λ(boxed) case boxed of {"
        , "    SomeExpr expr peel expr"
        , "  };"
        , ""
        , "  def main : Bool = eq (peelSome (SomeExpr (Step (DoneNat (Succ Zero))))) (Succ Zero);"
        , "}"
        ]

complexRecursiveProgramNegativeSourceText :: String
complexRecursiveProgramNegativeSourceText =
    unlines
        [ "module ComplexRecursiveProgram export (Eq, Nat(..), Tree(..), eq, mirror, leftDepth, rightDepth, main) {"
        , "  class Eq a {"
        , "    eq : a -> a -> Bool;"
        , "  }"
        , ""
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat"
        , "    deriving Eq;"
        , ""
        , "  data Tree ="
        , "      Leaf : Tree"
        , "    | Branch : Tree -> Tree -> Tree;"
        , ""
        , "  def mirror : Tree -> Tree = λ(tree : Tree) case tree of {"
        , "    Leaf -> Leaf;"
        , "    Branch left right -> Branch (mirror right) (mirror left)"
        , "  };"
        , ""
        , "  def leftDepth : Tree -> Nat = λ(tree : Tree) case tree of {"
        , "    Leaf -> Zero;"
        , "    Branch left _ Succ (leftDepth left)"
        , "  };"
        , ""
        , "  def rightDepth : Tree -> Nat = λ(tree : Tree) case tree of {"
        , "    Leaf -> Zero;"
        , "    Branch _ right -> Succ (rightDepth right)"
        , "  };"
        , ""
        , "  def main : Bool ="
        , "    eq"
        , "      (leftDepth"
        , "        (mirror"
        , "          (Branch"
        , "            (Branch Leaf Leaf)"
        , "            (Branch"
        , "              Leaf"
        , "              (Branch Leaf Leaf)))))"
        , "      (rightDepth"
        , "        (Branch"
        , "          (Branch Leaf Leaf)"
        , "          (Branch"
        , "            Leaf"
        , "            (Branch Leaf Leaf))));"
        , "}"
        ]

namedRecursiveAdtNegativeSourceText :: String
namedRecursiveAdtNegativeSourceText =
    unlines
        [ "module RecursiveGadt export (Nat(..), Expr(..), doneNow, main) {"
        , "  data Nat ="
        , "      Zero : Nat"
        , "    | Succ : Nat -> Nat;"
        , ""
        , "  data Expr a ="
        , "      DoneNat : Nat -> Expr Nat"
        , "    | Step : Expr a -> Expr a;"
        , ""
        , "  def doneNow : Expr a -> Bool = λ(expr) case expr of {"
        , "    DoneNat _ true;"
        , "    Step next -> doneNow next"
        , "  };"
        , ""
        , "  def main : Bool = doneNow (Step (DoneNat Zero));"
        , "}"
        ]

packageLayoutImportSemicolonNegativeSourceText :: String
packageLayoutImportSemicolonNegativeSourceText =
    unlines
        [ "module Main export (main) {"
        , "  import Core exposing (applyId)"
        , ""
        , "  def main : Int = applyId;"
        , "}"
        ]

compilerSeedDataModelCaseBranchNegativeSourceText :: String
compilerSeedDataModelCaseBranchNegativeSourceText =
    unlines
        [ "module SeedSource export (SourcePosition(..), SourceSpan(..), SeedIdentifier(..), SeedBoolLiteral(..), SeedInputSymbol(..), SeedInput(..), spanStart, spanEnd, positiveSeedInput, negativeSeedInput) {"
        , "  data SourcePosition ="
        , "      Line1Column1 : SourcePosition"
        , "    | Line1Column4 : SourcePosition"
        , "    | Line1Column5 : SourcePosition"
        , "    | Line1Column6 : SourcePosition"
        , "    | Line1Column9 : SourcePosition"
        , "    | Line1Column10 : SourcePosition"
        , "    | Line1Column11 : SourcePosition"
        , "    | Line1Column12 : SourcePosition"
        , "    | Line1Column16 : SourcePosition;"
        , ""
        , "  data SourceSpan ="
        , "      SpanDefKeyword : SourceSpan"
        , "    | SpanIdentifierMain : SourceSpan"
        , "    | SpanEquals : SourceSpan"
        , "    | SpanBoolTrue : SourceSpan"
        , "    | SpanUnknownSymbol : SourceSpan;"
        , ""
        , "  data SeedIdentifier ="
        , "      IdentifierMain : SeedIdentifier;"
        , ""
        , "  data SeedBoolLiteral ="
        , "      BoolLiteralTrue : SeedBoolLiteral;"
        , ""
        , "  data SeedInputSymbol ="
        , "      InputDef : SourceSpan -> SeedInputSymbol"
        , "    | InputIdentifier : SourceSpan -> SeedIdentifier -> SeedInputSymbol"
        , "    | InputEquals : SourceSpan -> SeedInputSymbol"
        , "    | InputBoolLiteral : SourceSpan -> SeedBoolLiteral -> SeedInputSymbol"
        , "    | InputUnknown : SourceSpan -> SeedInputSymbol;"
        , ""
        , "  data SeedInput ="
        , "      SeedInputNil : SeedInput"
        , "    | SeedInputCons : SeedInputSymbol -> SeedInput -> SeedInput;"
        , ""
        , "  def spanStart : SourceSpan -> SourcePosition ="
        , "    λ(span : SourceSpan) case span of {"
        , "      SpanDefKeyword Line1Column1;"
        , "      SpanIdentifierMain -> Line1Column5;"
        , "      SpanEquals -> Line1Column10;"
        , "      SpanBoolTrue -> Line1Column12;"
        , "      SpanUnknownSymbol -> Line1Column5"
        , "    };"
        , ""
        , "  def spanEnd : SourceSpan -> SourcePosition ="
        , "    λ(span : SourceSpan) case span of {"
        , "      SpanDefKeyword -> Line1Column4;"
        , "      SpanIdentifierMain -> Line1Column9;"
        , "      SpanEquals -> Line1Column11;"
        , "      SpanBoolTrue -> Line1Column16;"
        , "      SpanUnknownSymbol -> Line1Column6"
        , "    };"
        , ""
        , "  def positiveSeedInput : SeedInput ="
        , "    SeedInputCons (InputDef SpanDefKeyword)"
        , "      (SeedInputCons (InputIdentifier SpanIdentifierMain IdentifierMain)"
        , "        (SeedInputCons (InputEquals SpanEquals)"
        , "          (SeedInputCons (InputBoolLiteral SpanBoolTrue BoolLiteralTrue)"
        , "            SeedInputNil)));"
        , ""
        , "  def negativeSeedInput : SeedInput ="
        , "    SeedInputCons (InputDef SpanDefKeyword)"
        , "      (SeedInputCons (InputUnknown SpanUnknownSymbol)"
        , "        SeedInputNil);"
        , "}"
        ]

compilerSeedLexerCaseBranchNegativeSourceText :: String
compilerSeedLexerCaseBranchNegativeSourceText =
    unlines
        [ "module SeedLexer export (LexerResult(..), lexSeedInput) {"
        , "  data LexerResult ="
        , "      LexerOk : LexerResult"
        , "    | LexerError : LexerResult;"
        , ""
        , "  def lexSeedInput : LexerResult ="
        , "    case LexerOk of {"
        , "      LexerOk LexerError"
        , "    };"
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

higherOrderPartialApplicationNegativeEvidenceProjection :: String
higherOrderPartialApplicationNegativeEvidenceProjection =
    unlines
        [ concat
            [ "higher-order-partial-application parser negative "
            , "expected-expression-close-paren@"
            , "test/conformance/mlfp/parser-parity/higher-order-partial-application/src/Main.mlfp:5:37-5:38"
            ]
        ]

higherOrderLocalFunctionFlowNegativeEvidenceProjection :: String
higherOrderLocalFunctionFlowNegativeEvidenceProjection =
    unlines
        [ concat
            [ "higher-order-local-function-flow parser negative "
            , "expected-let-in@"
            , "test/conformance/mlfp/parser-parity/higher-order-local-function-flow/src/Main.mlfp:3:34-3:36"
            ]
        ]

higherOrderReturnedFunctionNegativeEvidenceProjection :: String
higherOrderReturnedFunctionNegativeEvidenceProjection =
    unlines
        [ concat
            [ "higher-order-returned-function parser negative "
            , "expected-expression-close-paren@"
            , "test/conformance/mlfp/parser-parity/higher-order-returned-function/src/Main.mlfp:7:29-7:30"
            ]
        ]

higherOrderFunctionFieldNegativeEvidenceProjection :: String
higherOrderFunctionFieldNegativeEvidenceProjection =
    unlines
        [ concat
            [ "higher-order-function-field parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/higher-order-function-field/src/Main.mlfp:8:33-8:34"
            ]
        ]

authoritativeRecursiveLetNegativeEvidenceProjection :: String
authoritativeRecursiveLetNegativeEvidenceProjection =
    unlines
        [ concat
            [ "authoritative-recursive-let parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/authoritative-recursive-let/src/Main.mlfp:8:16-8:17"
            ]
        ]

authoritativeCrossModuleLetPolymorphismNegativeEvidenceProjection :: String
authoritativeCrossModuleLetPolymorphismNegativeEvidenceProjection =
    unlines
        [ concat
            [ "authoritative-cross-module-let-polymorphism parser negative "
            , "expected-def-semicolon@"
            , "test/conformance/mlfp/parser-parity/authoritative-cross-module-let-polymorphism/src/Main.mlfp:3:1-3:2"
            ]
        ]

authoritativeUnifiedLetPolymorphismNegativeEvidenceProjection :: String
authoritativeUnifiedLetPolymorphismNegativeEvidenceProjection =
    unlines
        [ concat
            [ "authoritative-unified parser negative "
            , "expected-def-semicolon@"
            , "test/conformance/mlfp/parser-parity/authoritative-let-polymorphism/src/Main.mlfp:3:1-3:2"
            ]
        ]

recursiveAdtPlainNatNegativeEvidenceProjection :: String
recursiveAdtPlainNatNegativeEvidenceProjection =
    unlines
        [ concat
            [ "recursive-adt-plain-nat parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/src/Main.mlfp:14:3-14:4"
            ]
        ]

recursiveListTailNegativeEvidenceProjection :: String
recursiveListTailNegativeEvidenceProjection =
    unlines
        [ concat
            [ "recursive-list-tail parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/recursive-list-tail/src/Main.mlfp:12:17-12:21"
            ]
        ]

recursiveTreeNegativeEvidenceProjection :: String
recursiveTreeNegativeEvidenceProjection =
    unlines
        [ concat
            [ "recursive-tree parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/recursive-tree-first-order/src/Main.mlfp:8:23-8:29"
            ]
        ]

typeclassIntegrationNegativeEvidenceProjection :: String
typeclassIntegrationNegativeEvidenceProjection =
    unlines
        [ concat
            [ "typeclass-integration parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/typeclass-integration/src/Main.mlfp:13:14-13:18"
            ]
        ]

abstractRecursiveAdtModuleUseNegativeEvidenceProjection :: String
abstractRecursiveAdtModuleUseNegativeEvidenceProjection =
    unlines
        [ concat
            [ "abstract-recursive-adt-module-use parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/src/Main.mlfp:13:3-13:4"
            ]
        ]

moduleIntegratedRecursiveExistentialNegativeEvidenceProjection :: String
moduleIntegratedRecursiveExistentialNegativeEvidenceProjection =
    unlines
        [ concat
            [ "module-integrated-recursive-existential parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/module-integrated-recursive-existential/src/Main.mlfp:28:24-28:28"
            ]
        ]

complexRecursiveProgramNegativeEvidenceProjection :: String
complexRecursiveProgramNegativeEvidenceProjection =
    unlines
        [ concat
            [ "complex-recursive-program parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/complex-recursive-program/src/Main.mlfp:22:19-22:23"
            ]
        ]

namedRecursiveAdtNegativeEvidenceProjection :: String
namedRecursiveAdtNegativeEvidenceProjection =
    unlines
        [ concat
            [ "named-recursive-adt parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/recursive-gadt/src/Main.mlfp:11:15-11:19"
            ]
        ]

packageLayoutImportSemicolonNegativeEvidenceProjection :: String
packageLayoutImportSemicolonNegativeEvidenceProjection =
    unlines
        [ concat
            [ "package-layout parser negative "
            , "expected-import-semicolon@"
            , "test/conformance/mlfp/parser-parity/package-cross-module-let/src/Main.mlfp:4:3-4:6"
            ]
        ]

compilerSeedDataModelCaseBranchNegativeEvidenceProjection :: String
compilerSeedDataModelCaseBranchNegativeEvidenceProjection =
    unlines
        [ concat
            [ "compiler-seed-data-model parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/compiler-seed-data-model/src/SeedSource.mlfp:39:34-39:35"
            ]
        ]

compilerSeedLexerCaseBranchNegativeEvidenceProjection :: String
compilerSeedLexerCaseBranchNegativeEvidenceProjection =
    unlines
        [ concat
            [ "compiler-seed-lexer parser negative "
            , "expected-case-branch-arrow@"
            , "test/conformance/mlfp/parser-parity/compiler-seed-lexer/src/SeedLexer.mlfp:9:5-9:6"
            ]
        ]

renderCanonicalProjection :: FilePath -> String -> IO String
renderCanonicalProjection path source =
    case parseLocatedProgramWithFile path source of
        Left err ->
            expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right located ->
            renderLocatedProjection located

renderCanonicalPackageProjection :: [FilePath] -> IO String
renderCanonicalPackageProjection sourcePaths =
    concat <$> traverse renderSourcePathProjection sourcePaths
  where
    renderSourcePathProjection sourcePath = do
        source <- readFile sourcePath
        renderCanonicalProjection sourcePath source

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
