module LLVMToolSupport
    ( validateLLVMAssembly
    , validateLLVMObjectCode
    , withTempLLVM
    , withTempProgram
    ) where

import Control.Exception (bracket)
import Control.Monad (filterM)
import System.Directory (doesFileExist, findExecutable, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

validateLLVMAssembly :: String -> Expectation
validateLLVMAssembly output = do
    mbLlvmAs <- findLLVMTool "llvm-as"
    case mbLlvmAs of
        Nothing ->
            pendingWith "required LLVM tool not found: llvm-as"
        Just llvmAs ->
            withTempLLVM output $ \path -> do
                (exitCode, stderr) <- runLLVMTool llvmAs ["-o", "/dev/null", path]
                case exitCode of
                    ExitSuccess -> pure ()
                    ExitFailure _ -> expectationFailure ("llvm-as rejected backend output:\n" ++ stderr)

validateLLVMObjectCode :: String -> Expectation
validateLLVMObjectCode output = do
    mbLlc <- findLLVMTool "llc"
    case mbLlc of
        Nothing ->
            pendingWith "required LLVM tool not found: llc"
        Just llc ->
            withTempLLVM output $ \path -> do
                (exitCode, stderr) <- runLLVMTool llc ["-filetype=obj", "-o", "/dev/null", path]
                case exitCode of
                    ExitSuccess -> pure ()
                    ExitFailure _ -> expectationFailure ("llc rejected backend output:\n" ++ stderr)

withTempLLVM :: String -> (FilePath -> IO a) -> IO a
withTempLLVM output action = do
    tempDir <- getTemporaryDirectory
    bracket (writeTempLLVM tempDir) removeFile action
  where
    writeTempLLVM tempDir = do
        (path, handle) <- openTempFile tempDir "mlf2-backend-llvm.ll"
        hPutStr handle output
        hClose handle
        pure path

withTempProgram :: String -> (FilePath -> IO a) -> IO a
withTempProgram contents action = do
    tempDir <- getTemporaryDirectory
    bracket (writeTempProgram tempDir) removeFile action
  where
    writeTempProgram tempDir = do
        (path, handle) <- openTempFile tempDir "mlf2-backend-llvm.mlfp"
        hPutStr handle contents
        hClose handle
        pure path

runLLVMTool :: FilePath -> [String] -> IO (ExitCode, String)
runLLVMTool tool args = do
    (plainExitCode, _plainStdout, plainStderr) <- readProcessWithExitCode tool args ""
    case plainExitCode of
        ExitSuccess -> pure (ExitSuccess, "")
        ExitFailure _ -> do
            (opaqueExitCode, _opaqueStdout, opaqueStderr) <-
                readProcessWithExitCode tool ("-opaque-pointers" : args) ""
            pure $
                case opaqueExitCode of
                    ExitSuccess -> (ExitSuccess, "")
                    ExitFailure _ ->
                        ( opaqueExitCode
                        , plainStderr
                            ++ "\nWith -opaque-pointers:\n"
                            ++ opaqueStderr
                        )

findLLVMTool :: String -> IO (Maybe FilePath)
findLLVMTool name = do
    envCandidates <- filterM doesFileExist =<< traverseMaybeEnv (toolEnvNames name)
    case envCandidates of
        path : _ -> pure (Just path)
        [] -> do
            result <- findExecutable name
            case result of
                Just path -> pure (Just path)
                Nothing -> findKnownLLVMTool name knownLLVMToolPaths

traverseMaybeEnv :: [String] -> IO [FilePath]
traverseMaybeEnv names = do
    values <- traverse lookupEnv names
    pure [path | Just path <- values]

toolEnvNames :: String -> [String]
toolEnvNames name =
    case name of
        "llvm-as" -> ["LLVM_AS"]
        "llc" -> ["LLC", "LLVM_LLC"]
        _ -> []

findKnownLLVMTool :: String -> [FilePath] -> IO (Maybe FilePath)
findKnownLLVMTool name paths = do
    existing <- filterM doesFileExist [path | path <- paths, takeFileName path == name]
    case existing of
        path : _ -> pure (Just path)
        [] -> pure Nothing

knownLLVMToolPaths :: [FilePath]
knownLLVMToolPaths =
    [ "/opt/homebrew/opt/llvm/bin/llvm-as"
    , "/opt/homebrew/opt/llvm/bin/llc"
    , "/usr/local/opt/llvm/bin/llvm-as"
    , "/usr/local/opt/llvm/bin/llc"
    ]
