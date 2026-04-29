module LLVMToolSupport
    ( LLVMToolchain (..)
    , NativeRunResult (..)
    , discoverNativeLLVMToolchain
    , runLLVMNativeExecutable
    , validateLLVMAssembly
    , validateLLVMObjectCode
    , withTempLLVM
    , withTempProgram
    ) where

import Control.Exception (bracket)
import Control.Monad (filterM)
import System.Directory
    ( createDirectory
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isPathSeparator, takeFileName, (</>))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

data LLVMToolchain = LLVMToolchain
    { llvmToolchainLlc :: FilePath
    , llvmToolchainNativeLinker :: FilePath
    }
    deriving (Eq, Show)

data NativeRunResult = NativeRunResult
    { nativeRunExitCode :: ExitCode
    , nativeRunStdout :: String
    , nativeRunStderr :: String
    }
    deriving (Eq, Show)

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

discoverNativeLLVMToolchain :: IO (Either [String] LLVMToolchain)
discoverNativeLLVMToolchain = do
    mbLlc <- findLLVMTool "llc"
    mbNativeLinker <- findNativeLinker
    pure $
        case (mbLlc, mbNativeLinker) of
            (Just llc, Just nativeLinker) ->
                Right
                    LLVMToolchain
                        { llvmToolchainLlc = llc
                        , llvmToolchainNativeLinker = nativeLinker
                        }
            _ ->
                Left $
                    missingTool "llc" mbLlc
                        ++ missingTool
                            "native C compiler/linker (set CC or install cc, clang, or gcc)"
                            mbNativeLinker

runLLVMNativeExecutable :: String -> IO NativeRunResult
runLLVMNativeExecutable output = do
    discovered <- discoverNativeLLVMToolchain
    case discovered of
        Left missing ->
            skipMissingNativeToolchain missing
        Right toolchain ->
            runLLVMNativeExecutableWith toolchain output

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

runLLVMNativeExecutableWith :: LLVMToolchain -> String -> IO NativeRunResult
runLLVMNativeExecutableWith toolchain output =
    withTempLLVMBuildDirectory $ \buildDir -> do
        let llvmPath = buildDir </> "program.ll"
            objectPath = buildDir </> "program.o"
            executablePath = buildDir </> "program"
            llc = llvmToolchainLlc toolchain
            nativeLinker = llvmToolchainNativeLinker toolchain

        writeFile llvmPath output

        (llcExitCode, llcStderr) <-
            runLLVMTool llc ["-filetype=obj", "-o", objectPath, llvmPath]
        expectProcessSuccess "llc rejected native-runner LLVM input" llcExitCode "" llcStderr

        (linkExitCode, linkStdout, linkStderr) <-
            readProcessWithExitCode nativeLinker [objectPath, "-o", executablePath] ""
        expectProcessSuccess "native linker rejected LLVM object" linkExitCode linkStdout linkStderr

        (runExitCode, runStdout, runStderr) <- readProcessWithExitCode executablePath [] ""
        pure
            NativeRunResult
                { nativeRunExitCode = runExitCode
                , nativeRunStdout = runStdout
                , nativeRunStderr = runStderr
                }

withTempLLVMBuildDirectory :: (FilePath -> IO a) -> IO a
withTempLLVMBuildDirectory action = do
    tempDir <- getTemporaryDirectory
    bracket (createTempLLVMBuildDirectory tempDir) removeDirectoryRecursive action

createTempLLVMBuildDirectory :: FilePath -> IO FilePath
createTempLLVMBuildDirectory tempDir = do
    (path, handle) <- openTempFile tempDir "mlf2-llvm-native"
    hClose handle
    removeFile path
    createDirectory path
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
    mbEnvCandidate <- findEnvExecutable (toolEnvNames name)
    case mbEnvCandidate of
        Just path -> pure (Just path)
        Nothing -> do
            result <- findExecutable name
            case result of
                Just path -> pure (Just path)
                Nothing -> findKnownLLVMTool name knownLLVMToolPaths

findNativeLinker :: IO (Maybe FilePath)
findNativeLinker = do
    mbEnvCandidate <- findEnvExecutable ["CC"]
    case mbEnvCandidate of
        Just path -> pure (Just path)
        Nothing -> firstJustM findExecutable ["cc", "clang", "gcc"]

findEnvExecutable :: [String] -> IO (Maybe FilePath)
findEnvExecutable names = do
    values <- traverse lookupEnv names
    firstJustM resolveExecutableCandidate [path | Just path <- values]

resolveExecutableCandidate :: FilePath -> IO (Maybe FilePath)
resolveExecutableCandidate candidate
    | any isPathSeparator candidate = do
        exists <- doesFileExist candidate
        pure $
            if exists
                then Just candidate
                else Nothing
    | otherwise =
        findExecutable candidate

firstJustM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM action (candidate : candidates) = do
    result <- action candidate
    case result of
        Just _ -> pure result
        Nothing -> firstJustM action candidates

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

missingTool :: String -> Maybe FilePath -> [String]
missingTool name mbPath =
    case mbPath of
        Just _ -> []
        Nothing -> [name]

skipMissingNativeToolchain :: [String] -> IO a
skipMissingNativeToolchain missing = do
    pendingWith ("required native LLVM toolchain pieces not found: " ++ unwords missing)
    fail "native LLVM toolchain unavailable"

expectProcessSuccess :: String -> ExitCode -> String -> String -> IO ()
expectProcessSuccess label exitCode stdout stderr =
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            expectationFailure $
                label
                    ++ ":\nstdout:\n"
                    ++ stdout
                    ++ "\nstderr:\n"
                    ++ stderr

knownLLVMToolPaths :: [FilePath]
knownLLVMToolPaths =
    [ "/opt/homebrew/opt/llvm/bin/llvm-as"
    , "/opt/homebrew/opt/llvm/bin/llc"
    , "/usr/local/opt/llvm/bin/llvm-as"
    , "/usr/local/opt/llvm/bin/llc"
    ]
