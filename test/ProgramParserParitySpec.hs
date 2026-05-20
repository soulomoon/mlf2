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

            canonicalProjection <- renderCanonicalProjection source
            parserParityOutput <- runProgramArgs [parserParityPackageRoot]

            canonicalProjection `shouldBe` expected
            parserParityOutput `shouldBe` Right expected

        it "parser-owned .mlfp tokenizer and parser reject discrete token mismatches" $ do
            evidenceRoot <- writeRetryEvidencePackage
            runProgramArgs [evidenceRoot]
                `shouldReturn` Right retryEvidenceProjection

canonicalSourcePath :: FilePath
canonicalSourcePath =
    "test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp"

expectedProjectionPath :: FilePath
expectedProjectionPath =
    "test/conformance/mlfp/parser-parity/basic-module-def-bool/expected/parser-program.txt"

parserParityPackageRoot :: FilePath
parserParityPackageRoot =
    "test/programs/compiler-parser-parity/basic-module-def-bool"

retryEvidencePackageRoot :: FilePath
retryEvidencePackageRoot =
    "dist-newstyle/parser-parity-basic-module-def-bool-retry-evidence"

parserParitySupportModules :: [FilePath]
parserParitySupportModules =
    [ "ParserParitySource.mlfp"
    , "ParserParityToken.mlfp"
    , "ParserParityAst.mlfp"
    , "ParserParityParser.mlfp"
    ]

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

retryEvidenceProjection :: String
retryEvidenceProjection =
    unlines
        [ "tokens module@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:1-1:7 Main@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:8-1:12 export@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:19"
        , "lexer negative unexpected-source@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:13-1:19"
        , "parser negative expected-equals@test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:2:21-2:25"
        ]

renderCanonicalProjection :: String -> IO String
renderCanonicalProjection source =
    case parseLocatedProgramWithFile canonicalSourcePath source of
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
    def0 <- requireSingleDef (P.moduleDecls module0)
    moduleSpan <- requireMapSpan "module" (P.moduleName module0) (P.spanModules spans)
    exportSpan <- requireListSpan "export" exportName (P.spanExportItems spans)
    defSpan <- requireListSpan "definition" (P.defDeclName def0) (P.spanValues spans)

    pure $
        unlines
            [ "module " ++ P.moduleName module0 ++ " span=" ++ renderSpan moduleSpan
            , "export value " ++ exportName ++ " span=" ++ renderSpan exportSpan
            , "def "
                ++ P.defDeclName def0
                ++ " type="
                ++ renderSrcType (P.constrainedBody (P.defDeclType def0))
                ++ " expr="
                ++ renderExpr (P.defDeclExpr def0)
                ++ " span="
                ++ renderSpan defSpan
            ]

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
