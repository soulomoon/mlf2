{-# LANGUAGE LambdaCase #-}

module ProgramCliPackageSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.List (isInfixOf)
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openTempFile)
import Test.Hspec

import LLVMToolSupport (validateLLVMAssembly)
import MLF.Program.CLI
    ( checkProgramArgs
    , checkProgramFile
    , emitBackendArgs
    , emitNativeArgs
    , programCliUsage
    , runProgramArgs
    , runProgramFile
    )
import Parity.ProgramMatrix
    ( staticCrossModulePackageRoot
    , staticSearchPathLibRoot
    , staticSearchPathMainRoot
    )

spec :: Spec
spec =
    describe "MLF.Program CLI package migration" $ do
        it "keeps file inputs as trivial package entrypoints" $ do
            let path = "test/programs/recursive-adt/plain-recursive-nat.mlfp"

            checkProgramFile path `shouldReturn` Right "OK\n"
            runProgramFile path `shouldReturn` Right "true\n"

        it "checks and runs a directory package with ordered search paths" $
            withTempPackageRoots 2 $ \roots ->
                case roots of
                    [mainRoot, libRoot] -> do
                        _ <- writePackageFile mainRoot "Main.mlfp" mainSource
                        _ <- writePackageFile libRoot "Lib.mlfp" libSource
                        let args = packageArgs mainRoot libRoot

                        checkProgramArgs args `shouldReturn` Right "OK\n"
                        runProgramArgs args `shouldReturn` Right "2\n"
                    _ ->
                        expectationFailure ("expected two roots, got " ++ show roots)

        it "checks, runs, and emits backend LLVM from the static package-root fixture" $ do
            checkProgramArgs [staticCrossModulePackageRoot] `shouldReturn` Right "OK\n"
            runProgramArgs [staticCrossModulePackageRoot] `shouldReturn` Right "1\n"

            output <- requireRight =<< emitBackendArgs [staticCrossModulePackageRoot]

            output `shouldSatisfy` isInfixOf "define i64 @\"Core__applyId\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "checks, runs, and emits backend LLVM from the static search-path fixture" $ do
            let args = [staticSearchPathMainRoot, "--search-path", staticSearchPathLibRoot]
            checkProgramArgs args `shouldReturn` Right "OK\n"
            runProgramArgs args `shouldReturn` Right "2\n"

            output <- requireRight =<< emitBackendArgs args

            output `shouldSatisfy` isInfixOf "define i64 @\"SearchLib__two\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "emits backend LLVM from a directory package entrypoint" $
            withTempPackageRoots 2 $ \roots ->
                case roots of
                    [mainRoot, libRoot] -> do
                        _ <- writePackageFile mainRoot "Main.mlfp" mainSource
                        _ <- writePackageFile libRoot "Lib.mlfp" libSource

                        output <- requireRight =<< emitBackendArgs (packageArgs mainRoot libRoot)

                        output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
                        output `shouldSatisfy` isInfixOf "define i64 @\"Lib__two\"()"
                        output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
                        validateLLVMAssembly output
                    _ ->
                        expectationFailure ("expected two roots, got " ++ show roots)

        it "emits native LLVM from a directory package entrypoint" $
            withTempPackageRoots 2 $ \roots ->
                case roots of
                    [mainRoot, libRoot] -> do
                        _ <- writePackageFile mainRoot "Main.mlfp" mainSource
                        _ <- writePackageFile libRoot "Lib.mlfp" libSource

                        output <- requireRight =<< emitNativeArgs (packageArgs mainRoot libRoot)

                        output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
                        output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
                        validateLLVMAssembly output
                    _ ->
                        expectationFailure ("expected two roots, got " ++ show roots)

        it "reports missing file-or-root inputs with the requested path" $
            withTempPackageRoot $ \root -> do
                let missingPath = root </> "missing"

                result <- runProgramArgs [missingPath]

                result `shouldSatisfy` \case
                    Left err ->
                        missingPath `isInfixOf` err
                            && "neither a file nor a directory" `isInfixOf` err
                    Right _ ->
                        False

        it "rejects malformed search-path arguments before file IO" $
            runProgramArgs ["input.mlfp", "--search-path"]
                `shouldReturn` Left programCliUsage

packageArgs :: FilePath -> FilePath -> [String]
packageArgs mainRoot libRoot =
    [mainRoot, "--search-path", libRoot]

libSource :: String
libSource =
    unlines
        [ "module Lib export (two) {"
        , "  def two : Int = 2;"
        , "}"
        ]

mainSource :: String
mainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (two);"
        , "  def main : Int = two;"
        , "}"
        ]

withTempPackageRoot :: (FilePath -> IO a) -> IO a
withTempPackageRoot action =
    bracket createTempPackageRoot removeDirectoryRecursive action

withTempPackageRoots :: Int -> ([FilePath] -> IO a) -> IO a
withTempPackageRoots count action =
    bracket
        (replicateM count createTempPackageRoot)
        (mapM_ removeDirectoryRecursive)
        action

createTempPackageRoot :: IO FilePath
createTempPackageRoot = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir "mlf2-cli-package-root"
    hClose handle
    removeFile path
    createDirectory path
    pure path

writePackageFile :: FilePath -> FilePath -> String -> IO FilePath
writePackageFile root relativePath contents = do
    let path = root </> relativePath
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path contents
    pure path

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
