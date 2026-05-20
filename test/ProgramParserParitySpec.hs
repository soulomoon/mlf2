{-# LANGUAGE GADTs #-}

module ProgramParserParitySpec (spec) where

import qualified Data.Map.Strict as Map
import MLF.API
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Syntax (Lit (..), SrcTy (..), SrcType)
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Program.CLI (runProgramArgs)
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
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
            parserParityOutput <- runProgramArgs [parserParityPackageRoot]

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser matches canonical parser for a single import declaration and source spans" $ do
            source <- readFile importCanonicalSourcePath
            expected <- readFile importExpectedProjectionPath

            canonicalProjection <- renderCanonicalProjection importCanonicalSourcePath source
            parserParityOutput <- runProgramArgs [importParserParityPackageRoot]

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp parser rejects malformed import syntax through public run-program" $ do
            evidenceRoot <- writeImportNegativeEvidencePackage
            runProgramArgs [evidenceRoot]
                `shouldReturn` Right importNegativeEvidenceProjection

        it "parser-owned .mlfp tokenizer and parser reject discrete token mismatches" $ do
            evidenceRoot <- writeRetryEvidencePackage
            runProgramArgs [evidenceRoot]
                `shouldReturn` Right retryEvidenceProjection

canonicalSourcePath :: FilePath
canonicalSourcePath =
    "test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp"

importCanonicalSourcePath :: FilePath
importCanonicalSourcePath =
    "test/conformance/mlfp/parser-parity/import-exposing-def-bool/src/Main.mlfp"

expectedProjectionPath :: FilePath
expectedProjectionPath =
    "test/conformance/mlfp/parser-parity/basic-module-def-bool/expected/parser-program.txt"

importExpectedProjectionPath :: FilePath
importExpectedProjectionPath =
    "test/conformance/mlfp/parser-parity/import-exposing-def-bool/expected/parser-program.txt"

parserParityPackageRoot :: FilePath
parserParityPackageRoot =
    "test/programs/compiler-parser-parity/basic-module-def-bool"

importParserParityPackageRoot :: FilePath
importParserParityPackageRoot =
    "test/programs/compiler-parser-parity/import-exposing-def-bool"

retryEvidencePackageRoot :: FilePath
retryEvidencePackageRoot =
    "dist-newstyle/parser-parity-basic-module-def-bool-retry-evidence"

importNegativeEvidencePackageRoot :: FilePath
importNegativeEvidencePackageRoot =
    "dist-newstyle/parser-parity-import-exposing-def-bool-negative-evidence"

parserParitySupportModules :: [FilePath]
parserParitySupportModules =
    [ "ParserParitySource.mlfp"
    , "ParserParityToken.mlfp"
    , "ParserParityAst.mlfp"
    , "ParserParityParser.mlfp"
    ]

importParserParitySupportModules :: [FilePath]
importParserParitySupportModules =
    parserParitySupportModules

writeRetryEvidencePackage :: IO FilePath
writeRetryEvidencePackage = do
    removePathForcibly retryEvidencePackageRoot
    createDirectoryIfMissing True retryEvidencePackageRoot
    mapM_ copySupportModule parserParitySupportModules
    writeFile (retryEvidencePackageRoot </> "Main.mlfp") retryEvidenceMainSource
    pure retryEvidencePackageRoot
  where
    copySupportModule name =
        copyFile
            (parserParityPackageRoot </> name)
            (retryEvidencePackageRoot </> name)

writeImportNegativeEvidencePackage :: IO FilePath
writeImportNegativeEvidencePackage = do
    removePathForcibly importNegativeEvidencePackageRoot
    createDirectoryIfMissing True importNegativeEvidencePackageRoot
    mapM_ copySupportModule importParserParitySupportModules
    writeFile (importNegativeEvidencePackageRoot </> "Main.mlfp") importNegativeEvidenceMainSource
    pure importNegativeEvidencePackageRoot
  where
    copySupportModule name =
        copyFile
            (importParserParityPackageRoot </> name)
            (importNegativeEvidencePackageRoot </> name)

retryEvidenceMainSource :: String
retryEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderParserParityRetryEvidence);"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn renderParserParityRetryEvidence;"
        , "}"
        ]

importNegativeEvidenceMainSource :: String
importNegativeEvidenceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
        , "  import ParserParityParser exposing (renderImportParserNegativeEvidence);"
        , ""
        , "  def main : IO Unit ="
        , "    putStrLn renderImportParserNegativeEvidence;"
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
    exportName <- requireSingleValueExport (P.moduleExports module0)
    renderedImports <- renderImportProjections spans (P.moduleImports module0)
    def0 <- requireSingleDef (P.moduleDecls module0)
    moduleSpan <- requireMapSpan "module" (P.moduleName module0) (P.spanModules spans)
    exportSpan <- requireListSpan "export" exportName (P.spanExportItems spans)
    defSpan <- requireListSpan "definition" (P.defDeclName def0) (P.spanValues spans)

    pure $
        unlines
            ( [ "module " ++ P.moduleName module0 ++ " span=" ++ renderSpan moduleSpan
              , "export value " ++ exportName ++ " span=" ++ renderSpan exportSpan
              ]
                ++ renderedImports
                ++ [ "def "
                ++ P.defDeclName def0
                ++ " type="
                ++ renderSrcType (P.constrainedBody (P.defDeclType def0))
                ++ " expr="
                ++ renderExpr (P.defDeclExpr def0)
                ++ " span="
                ++ renderSpan defSpan
                   ]
            )

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

requireSingleValueExport :: Maybe [P.ExportItem] -> IO String
requireSingleValueExport exports =
    case exports of
        Just [P.ExportValue name] -> pure name
        other ->
            expectationFailure ("expected one value export, got: " ++ show other)
                >> fail "unexpected export shape"

requireSingleDef :: [P.Decl] -> IO P.DefDecl
requireSingleDef decls =
    case decls of
        [P.DeclDef def0] -> pure def0
        other ->
            expectationFailure ("expected one def declaration, got: " ++ show other)
                >> fail "unexpected declaration shape"

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
    case ty of
        STBase name -> name
        other -> show other

renderExpr :: P.Expr -> String
renderExpr expr =
    case expr of
        P.ELit (LBool True) -> "true"
        P.ELit (LBool False) -> "false"
        other -> show other

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
