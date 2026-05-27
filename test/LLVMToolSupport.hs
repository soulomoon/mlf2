module LLVMToolSupport
    ( LLVMToolchain (..)
    , NativeRunResult (..)
    , ToolCommand (..)
    , discoverNativeLLVMToolchain
    , parseExecutableCommand
    , runLLVMNativeExecutable
    , validateLLVMAssembly
    , validateLLVMObjectCode
    , withTempLLVM
    , withTempProgram
    ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.Bits (xor)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    , removeFile
    )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isPathSeparator, takeFileName, (</>))
import System.IO (hClose, hPutStr, openTempFile, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Test.Hspec

data LLVMToolchain = LLVMToolchain
    { llvmToolchainLlc :: FilePath
    , llvmToolchainNativeLinker :: ToolCommand
    }
    deriving (Eq, Show)

data ToolCommand = ToolCommand
    { toolCommandExecutable :: FilePath
    , toolCommandArguments :: [String]
    }
    deriving (Eq, Ord, Show)

data NativeRunResult = NativeRunResult
    { nativeRunExitCode :: ExitCode
    , nativeRunStdout :: String
    , nativeRunStderr :: String
    }
    deriving (Eq, Show)

data LLVMTestCache = LLVMTestCache
    { llvmCacheRoot :: Maybe FilePath
    , llvmCacheNativeToolchain :: Maybe (Either [String] LLVMToolchain)
    , llvmCacheAssemblyValidation :: Map.Map String (Either String ())
    , llvmCacheObjectCode :: Map.Map String (Either String FilePath)
    , llvmCacheRuntimeObjects :: Map.Map ToolCommand [FilePath]
    , llvmCacheNativeExecutables :: Map.Map String (Either String FilePath)
    }

emptyLLVMTestCache :: LLVMTestCache
emptyLLVMTestCache =
    LLVMTestCache
        { llvmCacheRoot = Nothing
        , llvmCacheNativeToolchain = Nothing
        , llvmCacheAssemblyValidation = Map.empty
        , llvmCacheObjectCode = Map.empty
        , llvmCacheRuntimeObjects = Map.empty
        , llvmCacheNativeExecutables = Map.empty
        }

llvmTestCache :: MVar LLVMTestCache
llvmTestCache =
    unsafePerformIO (newMVar emptyLLVMTestCache)
{-# NOINLINE llvmTestCache #-}

validateLLVMAssembly :: String -> Expectation
validateLLVMAssembly output = do
    mbLlvmAs <- findLLVMTool "llvm-as"
    case mbLlvmAs of
        Nothing ->
            expectationFailure "required LLVM tool not found: llvm-as"
        Just llvmAs -> do
            result <- cachedAssemblyValidation llvmAs output
            case result of
                Right () -> pure ()
                Left errOutput -> expectationFailure errOutput

validateLLVMObjectCode :: String -> Expectation
validateLLVMObjectCode output = do
    mbLlc <- findLLVMTool "llc"
    case mbLlc of
        Nothing ->
            expectationFailure "required LLVM tool not found: llc"
        Just llc -> do
            result <- cachedObjectCode llc output
            case result of
                Right _ -> pure ()
                Left errOutput -> expectationFailure errOutput

discoverNativeLLVMToolchain :: IO (Either [String] LLVMToolchain)
discoverNativeLLVMToolchain = do
    cache <- readMVar llvmTestCache
    case llvmCacheNativeToolchain cache of
        Just result -> pure result
        Nothing -> do
            result <- discoverNativeLLVMToolchainUncached
            modifyMVar_ llvmTestCache $ \cache' ->
                pure cache' { llvmCacheNativeToolchain = Just result }
            pure result

runLLVMNativeExecutable :: String -> IO NativeRunResult
runLLVMNativeExecutable output = do
    discovered <- discoverNativeLLVMToolchain
    case discovered of
        Left missing ->
            failMissingNativeToolchain missing
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
runLLVMNativeExecutableWith toolchain output = do
    mbExecutable <- cachedNativeExecutable toolchain output
    case mbExecutable of
        Left errOutput -> do
            expectationFailure errOutput
            fail "native LLVM executable unavailable"
        Right executablePath -> do
            (runExitCode, runStdout, runStderr) <- readProcessWithExitCode executablePath [] ""
            pure
                NativeRunResult
                    { nativeRunExitCode = runExitCode
                    , nativeRunStdout = runStdout
                    , nativeRunStderr = runStderr
                    }

discoverNativeLLVMToolchainUncached :: IO (Either [String] LLVMToolchain)
discoverNativeLLVMToolchainUncached = do
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

cachedAssemblyValidation :: FilePath -> String -> IO (Either String ())
cachedAssemblyValidation llvmAs output = do
    let key = cacheKey ["assembly", llvmAs, output]
    cache <- readMVar llvmTestCache
    case Map.lookup key (llvmCacheAssemblyValidation cache) of
        Just result -> pure result
        Nothing -> do
            result <- buildAssemblyValidation llvmAs output
            modifyMVar_ llvmTestCache $ \cache' ->
                pure
                    cache'
                        { llvmCacheAssemblyValidation =
                            Map.insert key result (llvmCacheAssemblyValidation cache')
                        }
            pure result

buildAssemblyValidation :: FilePath -> String -> IO (Either String ())
buildAssemblyValidation llvmAs output =
    withTempLLVM output $ \path -> do
        (exitCode, errOutput) <- runLLVMTool llvmAs ["-o", "/dev/null", path]
        pure $
            case exitCode of
                ExitSuccess -> Right ()
                ExitFailure _ -> Left ("llvm-as rejected backend output:\n" ++ errOutput)

cachedObjectCode :: FilePath -> String -> IO (Either String FilePath)
cachedObjectCode llc output = do
    let key = cacheKey ["object", llc, output]
    cache <- readMVar llvmTestCache
    case Map.lookup key (llvmCacheObjectCode cache) of
        Just result -> pure result
        Nothing -> do
            result <- buildObjectCode key llc output
            modifyMVar_ llvmTestCache $ \cache' ->
                pure
                    cache'
                        { llvmCacheObjectCode =
                            Map.insert key result (llvmCacheObjectCode cache')
                        }
            pure result

buildObjectCode :: String -> FilePath -> String -> IO (Either String FilePath)
buildObjectCode key llc output = do
    cacheRoot <- getLLVMCacheRoot
    let objectDir = cacheRoot </> stableCacheName key
        llvmPath = objectDir </> "program.ll"
        objectPath = objectDir </> "program.o"
    createDirectoryIfMissing True objectDir
    writeFile llvmPath output
    (exitCode, errOutput) <-
        runLLVMTool llc ["-relocation-model=pic", "-filetype=obj", "-o", objectPath, llvmPath]
    pure $
        case exitCode of
            ExitSuccess -> Right objectPath
            ExitFailure _ -> Left ("llc rejected backend output:\n" ++ errOutput)

cachedNativeExecutable :: LLVMToolchain -> String -> IO (Either String FilePath)
cachedNativeExecutable toolchain output = do
    extraObjects <- cachedRuntimeObjects (llvmToolchainNativeLinker toolchain)
    let key = cacheKey ["native", show toolchain, show extraObjects, output]
    cache <- readMVar llvmTestCache
    case Map.lookup key (llvmCacheNativeExecutables cache) of
        Just result -> pure result
        Nothing -> do
            result <- buildNativeExecutable key toolchain extraObjects output
            modifyMVar_ llvmTestCache $ \cache' ->
                pure
                    cache'
                        { llvmCacheNativeExecutables =
                            Map.insert key result (llvmCacheNativeExecutables cache')
                        }
            pure result

buildNativeExecutable :: String -> LLVMToolchain -> [FilePath] -> String -> IO (Either String FilePath)
buildNativeExecutable key toolchain extraObjects output = do
    objectResult <- cachedObjectCode (llvmToolchainLlc toolchain) output
    case objectResult of
        Left errOutput -> pure (Left errOutput)
        Right objectPath -> do
            cacheRoot <- getLLVMCacheRoot
            let executableDir = cacheRoot </> stableCacheName key
                executablePath = executableDir </> "program"
                nativeLinker = llvmToolchainNativeLinker toolchain
            createDirectoryIfMissing True executableDir
            (linkExitCode, linkStdout, linkStderr) <-
                readProcessWithExitCode
                    (toolCommandExecutable nativeLinker)
                    (toolCommandArguments nativeLinker ++ [objectPath] ++ extraObjects ++ ["-o", executablePath])
                    ""
            pure $
                case linkExitCode of
                    ExitSuccess -> Right executablePath
                    ExitFailure _ ->
                        Left (processFailureMessage "native linker rejected LLVM object" linkStdout linkStderr)

cachedRuntimeObjects :: ToolCommand -> IO [FilePath]
cachedRuntimeObjects nativeLinker = do
    cache <- readMVar llvmTestCache
    case Map.lookup nativeLinker (llvmCacheRuntimeObjects cache) of
        Just objects -> pure objects
        Nothing -> do
            objects <- compileCRuntimeIfPresent nativeLinker
            modifyMVar_ llvmTestCache $ \cache' ->
                pure
                    cache'
                        { llvmCacheRuntimeObjects =
                            Map.insert nativeLinker objects (llvmCacheRuntimeObjects cache')
                        }
            pure objects

getLLVMCacheRoot :: IO FilePath
getLLVMCacheRoot =
    modifyMVar llvmTestCache $ \cache ->
        case llvmCacheRoot cache of
            Just root -> pure (cache, root)
            Nothing -> do
                tempDir <- getTemporaryDirectory
                (path, handle) <- openTempFile tempDir "mlf2-llvm-cache"
                hClose handle
                removeFile path
                createDirectory path
                pure (cache { llvmCacheRoot = Just path }, path)

-- | Compile the Rust runtime crate if it exists, returning the path to the static library.
compileCRuntimeIfPresent :: ToolCommand -> IO [FilePath]
compileCRuntimeIfPresent _nativeLinker = do
    let cargoTomlPath = "runtime" </> "mlfp_io" </> "Cargo.toml"
    exists <- doesFileExist cargoTomlPath
    if exists
        then do
            mbCargo <- findExecutable "cargo"
            case mbCargo of
                Nothing -> do
                    hPutStr stderr "warning: cargo not found, skipping Rust runtime compilation\n"
                    pure []
                Just cargo -> do
                    (cargoExitCode, _cargoStdout, cargoStderr) <-
                        readProcessWithExitCode cargo
                            ["build", "--release", "--manifest-path", cargoTomlPath]
                            ""
                    case cargoExitCode of
                        ExitSuccess -> do
                            let libPath = "runtime" </> "mlfp_io" </> "target" </> "release" </> "libmlfp_io.a"
                            libExists <- doesFileExist libPath
                            if libExists
                                then pure [libPath]
                                else pure []
                        _ -> do
                            hPutStr stderr ("warning: Rust runtime compilation failed:\n" ++ cargoStderr)
                            pure []
                        else pure []

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

findNativeLinker :: IO (Maybe ToolCommand)
findNativeLinker = do
    mbEnvCandidate <- findEnvToolCommand ["CC"]
    case mbEnvCandidate of
        Just command -> pure (Just command)
        Nothing -> firstJustM findExecutableToolCommand ["cc", "clang", "gcc"]

findEnvExecutable :: [String] -> IO (Maybe FilePath)
findEnvExecutable names = do
    values <- traverse lookupEnv names
    firstJustM resolveExecutableCandidate [path | Just path <- values]

findEnvToolCommand :: [String] -> IO (Maybe ToolCommand)
findEnvToolCommand names = do
    values <- traverse lookupEnv names
    firstJustM resolveExecutableCommandCandidate [path | Just path <- values]

resolveExecutableCommandCandidate :: String -> IO (Maybe ToolCommand)
resolveExecutableCommandCandidate candidate =
    case parseExecutableCommand candidate of
        Nothing -> pure Nothing
        Just (executable, arguments) -> do
            mbPath <- resolveExecutableCandidate executable
            pure $ fmap (`ToolCommand` arguments) mbPath

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

findExecutableToolCommand :: FilePath -> IO (Maybe ToolCommand)
findExecutableToolCommand candidate =
    fmap (`ToolCommand` []) <$> findExecutable candidate

parseExecutableCommand :: String -> Maybe (FilePath, [String])
parseExecutableCommand candidate =
    case parseCommandWords candidate of
        Just (executable : arguments) -> Just (executable, arguments)
        _ -> Nothing

parseCommandWords :: String -> Maybe [String]
parseCommandWords = go False [] [] Unquoted
  where
    go inWord current wordsSoFar mode input =
        case (mode, input) of
            (Unquoted, []) -> Just (reverse (finishWord inWord current wordsSoFar))
            (SingleQuoted, []) -> Nothing
            (DoubleQuoted, []) -> Nothing
            (Unquoted, char : rest)
                | isSpace char ->
                    go False [] (finishWord inWord current wordsSoFar) Unquoted rest
                | char == '\'' ->
                    go True current wordsSoFar SingleQuoted rest
                | char == '"' ->
                    go True current wordsSoFar DoubleQuoted rest
                | char == '\\' ->
                    escaped inWord current wordsSoFar Unquoted rest
                | otherwise ->
                    go True (char : current) wordsSoFar Unquoted rest
            (SingleQuoted, char : rest)
                | char == '\'' ->
                    go True current wordsSoFar Unquoted rest
                | otherwise ->
                    go True (char : current) wordsSoFar SingleQuoted rest
            (DoubleQuoted, char : rest)
                | char == '"' ->
                    go True current wordsSoFar Unquoted rest
                | char == '\\' ->
                    escaped inWord current wordsSoFar DoubleQuoted rest
                | otherwise ->
                    go True (char : current) wordsSoFar DoubleQuoted rest

    escaped _ _ _ _ [] = Nothing
    escaped _ current wordsSoFar mode (char : rest) =
        go True (char : current) wordsSoFar mode rest

    finishWord inWord current wordsSoFar =
        if inWord
            then reverse current : wordsSoFar
            else wordsSoFar

data QuoteMode
    = Unquoted
    | SingleQuoted
    | DoubleQuoted

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

missingTool :: String -> Maybe a -> [String]
missingTool name mbPath =
    case mbPath of
        Just _ -> []
        Nothing -> [name]

failMissingNativeToolchain :: [String] -> IO a
failMissingNativeToolchain missing = do
    expectationFailure ("required native LLVM toolchain pieces not found: " ++ unwords missing)
    fail "native LLVM toolchain unavailable"

processFailureMessage :: String -> String -> String -> String
processFailureMessage label stdoutText stderrText =
    label
        ++ ":\nstdout:\n"
        ++ stdoutText
        ++ "\nstderr:\n"
        ++ stderrText

cacheKey :: [String] -> String
cacheKey = concatMap (++ "\NUL")

stableCacheName :: String -> String
stableCacheName key =
    "mlf2-" ++ showHex (fnv1a64 key) ""

fnv1a64 :: String -> Word64
fnv1a64 =
    foldl' step 14695981039346656037
  where
    step hash char =
        (hash `xor` fromIntegral (fromEnum char)) * 1099511628211

knownLLVMToolPaths :: [FilePath]
knownLLVMToolPaths =
    [ "/opt/homebrew/opt/llvm/bin/llvm-as"
    , "/opt/homebrew/opt/llvm/bin/llc"
    , "/usr/local/opt/llvm/bin/llvm-as"
    , "/usr/local/opt/llvm/bin/llc"
    ]
