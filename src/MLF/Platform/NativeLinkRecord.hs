module MLF.Platform.NativeLinkRecord
    ( SelfBootProofActionId (..)
    , SelfBootStageId (..)
    , StageOwnedOutputDirectory (..)
    , NativeLinkObjectInput (..)
    , ResolvedSelfBootLinkedLibraryIdentity (..)
    , ResolvedLinkedLibraryKind (..)
    , ResolvedLinkedLibraryLinkMode (..)
    , NativeLinkSearchPath (..)
    , NativeLinkRPath (..)
    , NativeLinkInstallName (..)
    , NativeLinkOutputArtifact (..)
    , NativeLinkExitStatus (..)
    , CanonicalSelfBootLinkRecord (..)
    , NativeLinkRecordEvidence (..)
    , NativeLinkRecordViolation (..)
    , validateCanonicalSelfBootLinkRecord
    , renderCanonicalSelfBootLinkRecord
    , renderNativeLinkRecordEvidence
    , renderNativeLinkRecordViolation
    , renderNativeLinkRecordViolations
    ) where

import Data.Char (isSpace)
import Data.List (group, intercalate, isPrefixOf, sort, sortOn)

import MLF.Platform.Contract
    ( TargetTriple (..)
    , ToolchainLinkerMode (..)
    )

newtype SelfBootProofActionId = SelfBootProofActionId
    { unSelfBootProofActionId :: String
    }
    deriving (Eq, Ord, Show)

newtype SelfBootStageId = SelfBootStageId
    { unSelfBootStageId :: String
    }
    deriving (Eq, Ord, Show)

newtype StageOwnedOutputDirectory = StageOwnedOutputDirectory
    { unStageOwnedOutputDirectory :: FilePath
    }
    deriving (Eq, Ord, Show)

data NativeLinkObjectInput = NativeLinkObjectInput
    { nativeLinkObjectInputPath :: FilePath
    , nativeLinkObjectInputHash :: String
    }
    deriving (Eq, Ord, Show)

newtype ResolvedLinkedLibraryKind = ResolvedLinkedLibraryKind
    { unResolvedLinkedLibraryKind :: String
    }
    deriving (Eq, Ord, Show)

newtype ResolvedLinkedLibraryLinkMode = ResolvedLinkedLibraryLinkMode
    { unResolvedLinkedLibraryLinkMode :: String
    }
    deriving (Eq, Ord, Show)

data ResolvedSelfBootLinkedLibraryIdentity = ResolvedSelfBootLinkedLibraryIdentity
    { resolvedLinkedLibraryName :: String
    , resolvedLinkedLibraryKind :: ResolvedLinkedLibraryKind
    , resolvedLinkedLibraryLinkMode :: ResolvedLinkedLibraryLinkMode
    , resolvedLinkedLibraryFilePath :: Maybe FilePath
    , resolvedLinkedLibraryFrameworkIdentity :: Maybe String
    , resolvedLinkedLibraryPlatformPackageIdentity :: Maybe String
    , resolvedLinkedLibraryContentHash :: Maybe String
    }
    deriving (Eq, Ord, Show)

newtype NativeLinkSearchPath = NativeLinkSearchPath
    { unNativeLinkSearchPath :: FilePath
    }
    deriving (Eq, Ord, Show)

newtype NativeLinkRPath = NativeLinkRPath
    { unNativeLinkRPath :: FilePath
    }
    deriving (Eq, Ord, Show)

newtype NativeLinkInstallName = NativeLinkInstallName
    { unNativeLinkInstallName :: String
    }
    deriving (Eq, Ord, Show)

data NativeLinkOutputArtifact = NativeLinkOutputArtifact
    { nativeLinkOutputArtifactPath :: Maybe FilePath
    , nativeLinkOutputArtifactHash :: Maybe String
    }
    deriving (Eq, Ord, Show)

data NativeLinkExitStatus
    = NativeLinkExited Int
    | NativeLinkSignaled String
    | NativeLinkExitStatusUnsupported String
    deriving (Eq, Ord, Show)

data CanonicalSelfBootLinkRecord = CanonicalSelfBootLinkRecord
    { canonicalSelfBootLinkRecordProofActionId :: Maybe SelfBootProofActionId
    , canonicalSelfBootLinkRecordLinkerArgv :: [String]
    , canonicalSelfBootLinkRecordTargetTriple :: Maybe TargetTriple
    , canonicalSelfBootLinkRecordLinkerMode :: Maybe ToolchainLinkerMode
    , canonicalSelfBootLinkRecordObjectInputs :: [NativeLinkObjectInput]
    , canonicalSelfBootLinkRecordResolvedLibraries :: [ResolvedSelfBootLinkedLibraryIdentity]
    , canonicalSelfBootLinkRecordLibrarySearchPaths :: [NativeLinkSearchPath]
    , canonicalSelfBootLinkRecordRPaths :: [NativeLinkRPath]
    , canonicalSelfBootLinkRecordInstallNames :: [NativeLinkInstallName]
    , canonicalSelfBootLinkRecordOutputArtifact :: NativeLinkOutputArtifact
    , canonicalSelfBootLinkRecordOwningStage :: Maybe SelfBootStageId
    , canonicalSelfBootLinkRecordStageOwnedOutputDirectory :: Maybe StageOwnedOutputDirectory
    , canonicalSelfBootLinkRecordExitStatus :: NativeLinkExitStatus
    }
    deriving (Eq, Ord, Show)

data NativeLinkRecordEvidence = NativeLinkRecordEvidence
    { nativeLinkRecordEvidenceProofActionId :: SelfBootProofActionId
    , nativeLinkRecordEvidenceLinkerArgv :: [String]
    , nativeLinkRecordEvidenceTargetTriple :: TargetTriple
    , nativeLinkRecordEvidenceLinkerMode :: ToolchainLinkerMode
    , nativeLinkRecordEvidenceObjectInputs :: [NativeLinkObjectInput]
    , nativeLinkRecordEvidenceResolvedLibraries :: [ResolvedSelfBootLinkedLibraryIdentity]
    , nativeLinkRecordEvidenceLibrarySearchPaths :: [NativeLinkSearchPath]
    , nativeLinkRecordEvidenceRPaths :: [NativeLinkRPath]
    , nativeLinkRecordEvidenceInstallNames :: [NativeLinkInstallName]
    , nativeLinkRecordEvidenceOutputArtifact :: NativeLinkOutputArtifact
    , nativeLinkRecordEvidenceOwningStage :: SelfBootStageId
    , nativeLinkRecordEvidenceStageOwnedOutputDirectory :: StageOwnedOutputDirectory
    , nativeLinkRecordEvidenceExitStatus :: NativeLinkExitStatus
    }
    deriving (Eq, Ord, Show)

data NativeLinkRecordViolation
    = MissingSelfBootProofActionId
    | BlankSelfBootProofActionId
    | MissingSelfBootStageId
    | BlankSelfBootStageId
    | MissingStageOwnedOutputDirectory
    | BlankStageOwnedOutputDirectory
    | EmptyNativeLinkerArgv
    | BlankNativeLinkerExecutable
    | MissingNativeLinkTargetTriple
    | BlankNativeLinkTargetTriple
    | MissingNativeLinkerMode
    | BlankNativeLinkerMode
    | MissingNativeLinkObjectInputs
    | BlankNativeLinkObjectInputPath
    | BlankNativeLinkObjectInputHash FilePath
    | DuplicateNativeLinkObjectInputPath FilePath
    | NativeLinkObjectInputOutsideStageOwnedOutputDirectory FilePath FilePath
    | BlankResolvedLinkedLibraryName
    | BlankResolvedLinkedLibraryKind String
    | BlankResolvedLinkedLibraryLinkMode String
    | UnresolvedSelfBootLinkedLibraryNameOnly String
    | MissingResolvedLinkedLibraryIdentity String
    | BlankResolvedLinkedLibraryFilePath String
    | BlankResolvedLinkedLibraryFrameworkIdentity String
    | BlankResolvedLinkedLibraryPlatformPackageIdentity String
    | MissingResolvedLinkedLibraryContentHash String
    | BlankResolvedLinkedLibraryContentHash String
    | DuplicateResolvedLinkedLibraryIdentity String
    | BlankNativeLinkSearchPath
    | DuplicateNativeLinkSearchPath FilePath
    | BlankNativeLinkRPath
    | DuplicateNativeLinkRPath FilePath
    | BlankNativeLinkInstallName
    | DuplicateNativeLinkInstallName String
    | MissingNativeLinkOutputArtifactPath
    | BlankNativeLinkOutputArtifactPath
    | MissingNativeLinkOutputArtifactHash
    | BlankNativeLinkOutputArtifactHash FilePath
    | NativeLinkOutputArtifactOutsideStageOwnedOutputDirectory FilePath FilePath
    | MalformedNativeLinkExitStatus String
    | UnsupportedNativeLinkExitStatus String
    deriving (Eq, Ord, Show)

validateCanonicalSelfBootLinkRecord ::
    CanonicalSelfBootLinkRecord ->
    Either [NativeLinkRecordViolation] NativeLinkRecordEvidence
validateCanonicalSelfBootLinkRecord record =
    case violations of
        [] -> Right (nativeLinkRecordEvidence record)
        _ -> Left (sort violations)
  where
    violations =
        concat
            [ validateProofActionId (canonicalSelfBootLinkRecordProofActionId record)
            , validateOwningStage (canonicalSelfBootLinkRecordOwningStage record)
            , validateStageOwnedOutputDirectory (canonicalSelfBootLinkRecordStageOwnedOutputDirectory record)
            , validateLinkerArgv (canonicalSelfBootLinkRecordLinkerArgv record)
            , validateTargetTriple (canonicalSelfBootLinkRecordTargetTriple record)
            , validateLinkerMode (canonicalSelfBootLinkRecordLinkerMode record)
            , validateObjectInputs
                (canonicalSelfBootLinkRecordStageOwnedOutputDirectory record)
                (canonicalSelfBootLinkRecordObjectInputs record)
            , validateResolvedLibraries (canonicalSelfBootLinkRecordResolvedLibraries record)
            , validateSearchPaths (canonicalSelfBootLinkRecordLibrarySearchPaths record)
            , validateRPaths (canonicalSelfBootLinkRecordRPaths record)
            , validateInstallNames (canonicalSelfBootLinkRecordInstallNames record)
            , validateOutputArtifact
                (canonicalSelfBootLinkRecordStageOwnedOutputDirectory record)
                (canonicalSelfBootLinkRecordOutputArtifact record)
            , validateExitStatus (canonicalSelfBootLinkRecordExitStatus record)
            ]

renderCanonicalSelfBootLinkRecord ::
    CanonicalSelfBootLinkRecord ->
    Either [NativeLinkRecordViolation] String
renderCanonicalSelfBootLinkRecord record =
    renderNativeLinkRecordEvidence <$> validateCanonicalSelfBootLinkRecord record

renderNativeLinkRecordEvidence :: NativeLinkRecordEvidence -> String
renderNativeLinkRecordEvidence evidence =
    unlines $
        [ "mlf-platform-native-link-record-v1"
        , "proof-action-id: " ++ unSelfBootProofActionId (nativeLinkRecordEvidenceProofActionId evidence)
        , "owning-stage: " ++ unSelfBootStageId (nativeLinkRecordEvidenceOwningStage evidence)
        , "stage-owned-output-directory: " ++ unStageOwnedOutputDirectory (nativeLinkRecordEvidenceStageOwnedOutputDirectory evidence)
        , "target-triple: " ++ unTargetTriple (nativeLinkRecordEvidenceTargetTriple evidence)
        , "linker-mode: " ++ unToolchainLinkerMode (nativeLinkRecordEvidenceLinkerMode evidence)
        , "linker-argv:"
        ]
            ++ renderIndentedItems (nativeLinkRecordEvidenceLinkerArgv evidence)
            ++ ["object-inputs:"]
            ++ renderIndentedItems (map renderObjectInput (nativeLinkRecordEvidenceObjectInputs evidence))
            ++ ["resolved-linked-libraries:"]
            ++ renderIndentedItems (map renderResolvedLibrary (nativeLinkRecordEvidenceResolvedLibraries evidence))
            ++ ["library-search-paths:"]
            ++ renderIndentedItems (map unNativeLinkSearchPath (nativeLinkRecordEvidenceLibrarySearchPaths evidence))
            ++ ["rpaths:"]
            ++ renderIndentedItems (map unNativeLinkRPath (nativeLinkRecordEvidenceRPaths evidence))
            ++ ["install-names:"]
            ++ renderIndentedItems (map unNativeLinkInstallName (nativeLinkRecordEvidenceInstallNames evidence))
            ++ [ "output-artifact: " ++ renderOutputArtifact (nativeLinkRecordEvidenceOutputArtifact evidence)
               , "exit-status: " ++ renderExitStatus (nativeLinkRecordEvidenceExitStatus evidence)
               ]

renderNativeLinkRecordViolation :: NativeLinkRecordViolation -> String
renderNativeLinkRecordViolation violation =
    case violation of
        MissingSelfBootProofActionId ->
            "self-boot proof action id is missing from native link record"
        BlankSelfBootProofActionId ->
            "self-boot proof action id is blank in native link record"
        MissingSelfBootStageId ->
            "owning stage is missing from native link record"
        BlankSelfBootStageId ->
            "owning stage is blank in native link record"
        MissingStageOwnedOutputDirectory ->
            "stage-owned output directory is missing from native link record"
        BlankStageOwnedOutputDirectory ->
            "stage-owned output directory is blank in native link record"
        EmptyNativeLinkerArgv ->
            "linker argv vector is empty in native link record"
        BlankNativeLinkerExecutable ->
            "linker argv executable is blank in native link record"
        MissingNativeLinkTargetTriple ->
            "target triple is missing from native link record"
        BlankNativeLinkTargetTriple ->
            "target triple is blank in native link record"
        MissingNativeLinkerMode ->
            "linker mode is missing from native link record"
        BlankNativeLinkerMode ->
            "linker mode is blank in native link record"
        MissingNativeLinkObjectInputs ->
            "native link record has no object inputs"
        BlankNativeLinkObjectInputPath ->
            "object input path is blank in native link record"
        BlankNativeLinkObjectInputHash path ->
            "object input " ++ renderNamedValue path ++ " has blank content hash"
        DuplicateNativeLinkObjectInputPath path ->
            "duplicate object input path in native link record: " ++ renderNamedValue path
        NativeLinkObjectInputOutsideStageOwnedOutputDirectory root path ->
            "object input path outside the declared stage-owned output directory: root=" ++ root ++ " path=" ++ path
        BlankResolvedLinkedLibraryName ->
            "resolved linked library identity has blank library name"
        BlankResolvedLinkedLibraryKind name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has blank kind"
        BlankResolvedLinkedLibraryLinkMode name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has blank link mode"
        UnresolvedSelfBootLinkedLibraryNameOnly name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " uses only an unresolved -l-style name"
        MissingResolvedLinkedLibraryIdentity name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has no resolved file, framework, or platform package identity"
        BlankResolvedLinkedLibraryFilePath name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has blank resolved file path"
        BlankResolvedLinkedLibraryFrameworkIdentity name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has blank framework identity"
        BlankResolvedLinkedLibraryPlatformPackageIdentity name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has blank platform package identity"
        MissingResolvedLinkedLibraryContentHash name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " is file/framework-backed but missing content hash"
        BlankResolvedLinkedLibraryContentHash name ->
            "resolved linked library identity " ++ renderNamedValue name ++ " has blank content hash"
        DuplicateResolvedLinkedLibraryIdentity key ->
            "duplicate resolved linked library identity key: " ++ renderNamedValue key
        BlankNativeLinkSearchPath ->
            "library search path is blank in native link record"
        DuplicateNativeLinkSearchPath path ->
            "duplicate library search path in native link record: " ++ renderNamedValue path
        BlankNativeLinkRPath ->
            "rpath entry is blank in native link record"
        DuplicateNativeLinkRPath path ->
            "duplicate rpath entry in native link record: " ++ renderNamedValue path
        BlankNativeLinkInstallName ->
            "install-name entry is blank in native link record"
        DuplicateNativeLinkInstallName name ->
            "duplicate install-name entry in native link record: " ++ renderNamedValue name
        MissingNativeLinkOutputArtifactPath ->
            "output artifact path is missing from native link record"
        BlankNativeLinkOutputArtifactPath ->
            "output artifact path is blank in native link record"
        MissingNativeLinkOutputArtifactHash ->
            "output artifact hash is missing from native link record"
        BlankNativeLinkOutputArtifactHash path ->
            "output artifact " ++ renderNamedValue path ++ " has blank content hash"
        NativeLinkOutputArtifactOutsideStageOwnedOutputDirectory root path ->
            "output artifact path outside the declared stage-owned output directory: root=" ++ root ++ " path=" ++ path
        MalformedNativeLinkExitStatus status ->
            "native link exit status is malformed: " ++ renderNamedValue status
        UnsupportedNativeLinkExitStatus status ->
            "native link exit status is unsupported: " ++ renderNamedValue status

renderNativeLinkRecordViolations :: [NativeLinkRecordViolation] -> String
renderNativeLinkRecordViolations =
    unlines . map renderNativeLinkRecordViolation . sort

validateProofActionId :: Maybe SelfBootProofActionId -> [NativeLinkRecordViolation]
validateProofActionId value =
    case value of
        Nothing -> [MissingSelfBootProofActionId]
        Just actionId
            | isBlank (unSelfBootProofActionId actionId) -> [BlankSelfBootProofActionId]
            | otherwise -> []

validateOwningStage :: Maybe SelfBootStageId -> [NativeLinkRecordViolation]
validateOwningStage value =
    case value of
        Nothing -> [MissingSelfBootStageId]
        Just stage
            | isBlank (unSelfBootStageId stage) -> [BlankSelfBootStageId]
            | otherwise -> []

validateStageOwnedOutputDirectory ::
    Maybe StageOwnedOutputDirectory ->
    [NativeLinkRecordViolation]
validateStageOwnedOutputDirectory value =
    case value of
        Nothing -> [MissingStageOwnedOutputDirectory]
        Just root
            | isBlank (unStageOwnedOutputDirectory root) -> [BlankStageOwnedOutputDirectory]
            | otherwise -> []

validateLinkerArgv :: [String] -> [NativeLinkRecordViolation]
validateLinkerArgv argv =
    case argv of
        [] -> [EmptyNativeLinkerArgv]
        executable : _
            | isBlank executable -> [BlankNativeLinkerExecutable]
            | otherwise -> []

validateTargetTriple :: Maybe TargetTriple -> [NativeLinkRecordViolation]
validateTargetTriple value =
    case value of
        Nothing -> [MissingNativeLinkTargetTriple]
        Just target
            | isBlank (unTargetTriple target) -> [BlankNativeLinkTargetTriple]
            | otherwise -> []

validateLinkerMode :: Maybe ToolchainLinkerMode -> [NativeLinkRecordViolation]
validateLinkerMode value =
    case value of
        Nothing -> [MissingNativeLinkerMode]
        Just mode
            | isBlank (unToolchainLinkerMode mode) -> [BlankNativeLinkerMode]
            | otherwise -> []

validateObjectInputs ::
    Maybe StageOwnedOutputDirectory ->
    [NativeLinkObjectInput] ->
    [NativeLinkRecordViolation]
validateObjectInputs mbRoot inputs =
    [MissingNativeLinkObjectInputs | null inputs]
        ++ concatMap (validateObjectInput mbRoot) inputs
        ++ map DuplicateNativeLinkObjectInputPath (duplicates (filter (not . isBlank) (map nativeLinkObjectInputPath inputs)))

validateObjectInput ::
    Maybe StageOwnedOutputDirectory ->
    NativeLinkObjectInput ->
    [NativeLinkRecordViolation]
validateObjectInput mbRoot input =
    [BlankNativeLinkObjectInputPath | isBlank path]
        ++ [BlankNativeLinkObjectInputHash path | isBlank (nativeLinkObjectInputHash input)]
        ++ validateStageOwnedPath NativeLinkObjectInputOutsideStageOwnedOutputDirectory mbRoot path
  where
    path =
        nativeLinkObjectInputPath input

validateResolvedLibraries ::
    [ResolvedSelfBootLinkedLibraryIdentity] ->
    [NativeLinkRecordViolation]
validateResolvedLibraries libraries =
    concatMap validateResolvedLibrary libraries
        ++ map DuplicateResolvedLinkedLibraryIdentity (duplicates (map resolvedLinkedLibraryKey libraries))

validateResolvedLibrary ::
    ResolvedSelfBootLinkedLibraryIdentity ->
    [NativeLinkRecordViolation]
validateResolvedLibrary library =
    [BlankResolvedLinkedLibraryName | isBlank name]
        ++ [BlankResolvedLinkedLibraryKind name | isBlank kind]
        ++ [BlankResolvedLinkedLibraryLinkMode name | isBlank mode]
        ++ selectedTextViolation BlankResolvedLinkedLibraryFilePath name (resolvedLinkedLibraryFilePath library)
        ++ selectedTextViolation BlankResolvedLinkedLibraryFrameworkIdentity name (resolvedLinkedLibraryFrameworkIdentity library)
        ++ selectedTextViolation BlankResolvedLinkedLibraryPlatformPackageIdentity name (resolvedLinkedLibraryPlatformPackageIdentity library)
        ++ resolvedIdentityViolation library
        ++ fileBackedHashViolations library
  where
    name =
        resolvedLinkedLibraryName library
    kind =
        unResolvedLinkedLibraryKind (resolvedLinkedLibraryKind library)
    mode =
        unResolvedLinkedLibraryLinkMode (resolvedLinkedLibraryLinkMode library)

resolvedIdentityViolation ::
    ResolvedSelfBootLinkedLibraryIdentity ->
    [NativeLinkRecordViolation]
resolvedIdentityViolation library
    | hasResolvedLibraryIdentity library = []
    | "-l" `isPrefixOf` resolvedLinkedLibraryName library =
        [UnresolvedSelfBootLinkedLibraryNameOnly (resolvedLinkedLibraryName library)]
    | otherwise =
        [MissingResolvedLinkedLibraryIdentity (resolvedLinkedLibraryName library)]

fileBackedHashViolations ::
    ResolvedSelfBootLinkedLibraryIdentity ->
    [NativeLinkRecordViolation]
fileBackedHashViolations library
    | not (hasNonBlankMaybeText (resolvedLinkedLibraryFilePath library) || hasNonBlankMaybeText (resolvedLinkedLibraryFrameworkIdentity library)) = []
    | otherwise =
        case resolvedLinkedLibraryContentHash library of
            Nothing -> [MissingResolvedLinkedLibraryContentHash name]
            Just contentHash
                | isBlank contentHash -> [BlankResolvedLinkedLibraryContentHash name]
                | otherwise -> []
  where
    name =
        resolvedLinkedLibraryName library

selectedTextViolation ::
    (String -> NativeLinkRecordViolation) ->
    String ->
    Maybe String ->
    [NativeLinkRecordViolation]
selectedTextViolation violation name value =
    case value of
        Nothing -> []
        Just text
            | isBlank text -> [violation name]
            | otherwise -> []

validateSearchPaths :: [NativeLinkSearchPath] -> [NativeLinkRecordViolation]
validateSearchPaths paths =
    concatMap validateSearchPath paths
        ++ map DuplicateNativeLinkSearchPath (duplicates (filter (not . isBlank) (map unNativeLinkSearchPath paths)))

validateSearchPath :: NativeLinkSearchPath -> [NativeLinkRecordViolation]
validateSearchPath path =
    [BlankNativeLinkSearchPath | isBlank (unNativeLinkSearchPath path)]

validateRPaths :: [NativeLinkRPath] -> [NativeLinkRecordViolation]
validateRPaths paths =
    concatMap validateRPath paths
        ++ map DuplicateNativeLinkRPath (duplicates (filter (not . isBlank) (map unNativeLinkRPath paths)))

validateRPath :: NativeLinkRPath -> [NativeLinkRecordViolation]
validateRPath path =
    [BlankNativeLinkRPath | isBlank (unNativeLinkRPath path)]

validateInstallNames :: [NativeLinkInstallName] -> [NativeLinkRecordViolation]
validateInstallNames names =
    concatMap validateInstallName names
        ++ map DuplicateNativeLinkInstallName (duplicates (filter (not . isBlank) (map unNativeLinkInstallName names)))

validateInstallName :: NativeLinkInstallName -> [NativeLinkRecordViolation]
validateInstallName name =
    [BlankNativeLinkInstallName | isBlank (unNativeLinkInstallName name)]

validateOutputArtifact ::
    Maybe StageOwnedOutputDirectory ->
    NativeLinkOutputArtifact ->
    [NativeLinkRecordViolation]
validateOutputArtifact mbRoot artifact =
    validateOutputArtifactPath mbRoot (nativeLinkOutputArtifactPath artifact)
        ++ validateOutputArtifactHash artifact

validateOutputArtifactPath ::
    Maybe StageOwnedOutputDirectory ->
    Maybe FilePath ->
    [NativeLinkRecordViolation]
validateOutputArtifactPath mbRoot value =
    case value of
        Nothing -> [MissingNativeLinkOutputArtifactPath]
        Just path
            | isBlank path -> [BlankNativeLinkOutputArtifactPath]
            | otherwise -> validateStageOwnedPath NativeLinkOutputArtifactOutsideStageOwnedOutputDirectory mbRoot path

validateOutputArtifactHash ::
    NativeLinkOutputArtifact ->
    [NativeLinkRecordViolation]
validateOutputArtifactHash artifact =
    case nativeLinkOutputArtifactHash artifact of
        Nothing -> [MissingNativeLinkOutputArtifactHash]
        Just contentHash
            | isBlank contentHash -> [BlankNativeLinkOutputArtifactHash (renderMaybeText (nativeLinkOutputArtifactPath artifact))]
            | otherwise -> []

validateExitStatus :: NativeLinkExitStatus -> [NativeLinkRecordViolation]
validateExitStatus status =
    case status of
        NativeLinkExited code
            | code < 0 -> [MalformedNativeLinkExitStatus ("exited code=" ++ show code)]
            | otherwise -> []
        NativeLinkSignaled signalName
            | isBlank signalName -> [MalformedNativeLinkExitStatus "signal=<blank>"]
            | otherwise -> []
        NativeLinkExitStatusUnsupported representation ->
            [UnsupportedNativeLinkExitStatus representation]

nativeLinkRecordEvidence :: CanonicalSelfBootLinkRecord -> NativeLinkRecordEvidence
nativeLinkRecordEvidence record =
    NativeLinkRecordEvidence
        { nativeLinkRecordEvidenceProofActionId =
            requireEvidenceValue (canonicalSelfBootLinkRecordProofActionId record)
        , nativeLinkRecordEvidenceLinkerArgv =
            canonicalSelfBootLinkRecordLinkerArgv record
        , nativeLinkRecordEvidenceTargetTriple =
            requireEvidenceValue (canonicalSelfBootLinkRecordTargetTriple record)
        , nativeLinkRecordEvidenceLinkerMode =
            requireEvidenceValue (canonicalSelfBootLinkRecordLinkerMode record)
        , nativeLinkRecordEvidenceObjectInputs =
            sortOn nativeLinkObjectInputPath (canonicalSelfBootLinkRecordObjectInputs record)
        , nativeLinkRecordEvidenceResolvedLibraries =
            sortOn resolvedLinkedLibraryKey (canonicalSelfBootLinkRecordResolvedLibraries record)
        , nativeLinkRecordEvidenceLibrarySearchPaths =
            sortOn unNativeLinkSearchPath (canonicalSelfBootLinkRecordLibrarySearchPaths record)
        , nativeLinkRecordEvidenceRPaths =
            sortOn unNativeLinkRPath (canonicalSelfBootLinkRecordRPaths record)
        , nativeLinkRecordEvidenceInstallNames =
            sortOn unNativeLinkInstallName (canonicalSelfBootLinkRecordInstallNames record)
        , nativeLinkRecordEvidenceOutputArtifact =
            canonicalSelfBootLinkRecordOutputArtifact record
        , nativeLinkRecordEvidenceOwningStage =
            requireEvidenceValue (canonicalSelfBootLinkRecordOwningStage record)
        , nativeLinkRecordEvidenceStageOwnedOutputDirectory =
            requireEvidenceValue (canonicalSelfBootLinkRecordStageOwnedOutputDirectory record)
        , nativeLinkRecordEvidenceExitStatus =
            canonicalSelfBootLinkRecordExitStatus record
        }

renderObjectInput :: NativeLinkObjectInput -> String
renderObjectInput input =
    "path="
        ++ nativeLinkObjectInputPath input
        ++ " hash="
        ++ nativeLinkObjectInputHash input

renderResolvedLibrary :: ResolvedSelfBootLinkedLibraryIdentity -> String
renderResolvedLibrary library =
    concat
        [ "name="
        , resolvedLinkedLibraryName library
        , " kind="
        , unResolvedLinkedLibraryKind (resolvedLinkedLibraryKind library)
        , " link-mode="
        , unResolvedLinkedLibraryLinkMode (resolvedLinkedLibraryLinkMode library)
        , " path="
        , renderMaybeText (resolvedLinkedLibraryFilePath library)
        , " framework="
        , renderMaybeText (resolvedLinkedLibraryFrameworkIdentity library)
        , " platform-package="
        , renderMaybeText (resolvedLinkedLibraryPlatformPackageIdentity library)
        , " hash="
        , renderMaybeText (resolvedLinkedLibraryContentHash library)
        ]

renderOutputArtifact :: NativeLinkOutputArtifact -> String
renderOutputArtifact artifact =
    "path="
        ++ renderMaybeText (nativeLinkOutputArtifactPath artifact)
        ++ " hash="
        ++ renderMaybeText (nativeLinkOutputArtifactHash artifact)

renderExitStatus :: NativeLinkExitStatus -> String
renderExitStatus status =
    case status of
        NativeLinkExited code ->
            "exited code=" ++ show code
        NativeLinkSignaled signalName ->
            "signaled signal=" ++ signalName
        NativeLinkExitStatusUnsupported representation ->
            "unsupported representation=" ++ representation

renderIndentedItems :: [String] -> [String]
renderIndentedItems values =
    case values of
        [] -> ["  - <none>"]
        _ -> map ("  - " ++) values

renderNamedValue :: String -> String
renderNamedValue value
    | isBlank value = "<blank>"
    | otherwise = value

renderMaybeText :: Maybe String -> String
renderMaybeText value =
    case value of
        Nothing -> "<none>"
        Just text
            | isBlank text -> "<blank>"
            | otherwise -> text

validateStageOwnedPath ::
    (FilePath -> FilePath -> NativeLinkRecordViolation) ->
    Maybe StageOwnedOutputDirectory ->
    FilePath ->
    [NativeLinkRecordViolation]
validateStageOwnedPath violation mbRoot path =
    case mbRoot of
        Nothing -> []
        Just root
            | isBlank (unStageOwnedOutputDirectory root) || isBlank path -> []
            | isPathInsideRoot (unStageOwnedOutputDirectory root) path -> []
            | otherwise -> [violation (unStageOwnedOutputDirectory root) path]

isPathInsideRoot :: FilePath -> FilePath -> Bool
isPathInsideRoot root path =
    normalizedPath == normalizedRoot
        || rootWithSlash `isPrefixOf` normalizedPath
  where
    normalizedRoot =
        normalizePath root
    normalizedPath =
        normalizePath path
    rootWithSlash =
        case normalizedRoot of
            "/" -> "/"
            _ -> normalizedRoot ++ "/"

normalizePath :: FilePath -> FilePath
normalizePath path =
    renderPath absolute (foldl normalizeSegment [] (pathSegments path))
  where
    absolute =
        "/" `isPrefixOf` path

normalizeSegment :: [String] -> String -> [String]
normalizeSegment stack segment
    | segment == "" || segment == "." = stack
    | segment == ".." =
        case stack of
            [] -> [".."]
            _ -> init stack
    | otherwise = stack ++ [segment]

renderPath :: Bool -> [String] -> FilePath
renderPath absolute segments =
    case (absolute, segments) of
        (True, []) -> "/"
        (True, _) -> "/" ++ intercalate "/" segments
        (False, []) -> "."
        (False, _) -> intercalate "/" segments

pathSegments :: FilePath -> [String]
pathSegments [] =
    []
pathSegments value =
    let (segment, rest) = break (== '/') value
     in case rest of
            [] -> [segment]
            _ : remaining -> segment : pathSegments remaining

hasResolvedLibraryIdentity :: ResolvedSelfBootLinkedLibraryIdentity -> Bool
hasResolvedLibraryIdentity library =
    hasNonBlankMaybeText (resolvedLinkedLibraryFilePath library)
        || hasNonBlankMaybeText (resolvedLinkedLibraryFrameworkIdentity library)
        || hasNonBlankMaybeText (resolvedLinkedLibraryPlatformPackageIdentity library)

hasNonBlankMaybeText :: Maybe String -> Bool
hasNonBlankMaybeText =
    maybe False (not . isBlank)

resolvedLinkedLibraryKey :: ResolvedSelfBootLinkedLibraryIdentity -> String
resolvedLinkedLibraryKey library =
    intercalate
        ":"
        [ resolvedLinkedLibraryName library
        , unResolvedLinkedLibraryKind (resolvedLinkedLibraryKind library)
        , unResolvedLinkedLibraryLinkMode (resolvedLinkedLibraryLinkMode library)
        ]

duplicates :: [String] -> [String]
duplicates values =
    map duplicateKey (filter hasDuplicate (group (sort values)))
  where
    hasDuplicate group0 =
        case group0 of
            _ : _ : _ -> True
            _ -> False

    duplicateKey group0 =
        case group0 of
            key : _ -> key
            [] -> ""

requireEvidenceValue :: Maybe a -> a
requireEvidenceValue value =
    case value of
        Just present -> present
        Nothing -> error "validated native link record evidence is missing a required value"

isBlank :: String -> Bool
isBlank =
    all isSpace
