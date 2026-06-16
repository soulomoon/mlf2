module MLF.Platform.ToolchainIdentity
  ( ObservedToolIdentity (..),
    ObservedToolchainSystemLibrary (..),
    ObservedToolchainIdentity (..),
    ToolchainIdentitySnapshot (..),
    ToolchainIdentityEvidence (..),
    ToolchainIdentityViolation (..),
    validatePlatformToolchainIdentity,
    renderPlatformToolchainIdentityEvidence,
    renderPlatformToolchainIdentityViolation,
    renderPlatformToolchainIdentityViolations,
  )
where

import Data.Char (isSpace)
import Data.List (find, group, sort, sortOn)
import MLF.Platform.Contract
  ( HostToolchainContract (..),
    ResolvedToolIdentity (..),
    TargetTriple (..),
    ToolchainCodegenSetting (..),
    ToolchainLinkerMode (..),
    ToolchainSysrootIdentity (..),
    ToolchainSystemLibraryIdentity (..),
    ToolchainToolRole (..),
  )

data ObservedToolIdentity = ObservedToolIdentity
  { observedToolRole :: ToolchainToolRole,
    observedToolPath :: Maybe FilePath,
    observedToolDigest :: Maybe String,
    observedToolUnavailableReason :: Maybe String,
    observedToolVersion :: Maybe String,
    observedToolAvailable :: Bool
  }
  deriving (Eq, Ord, Show)

data ObservedToolchainSystemLibrary = ObservedToolchainSystemLibrary
  { observedToolchainSystemLibraryName :: String,
    observedToolchainSystemLibraryIdentity :: String
  }
  deriving (Eq, Ord, Show)

data ObservedToolchainIdentity = ObservedToolchainIdentity
  { observedToolchainTargetTriple :: TargetTriple,
    observedToolchainTools :: [ObservedToolIdentity],
    observedToolchainSysrootIdentity :: Maybe ToolchainSysrootIdentity,
    observedToolchainSystemLibraries :: [ObservedToolchainSystemLibrary],
    observedToolchainCodegenSettings :: [ToolchainCodegenSetting],
    observedToolchainLinkerMode :: Maybe ToolchainLinkerMode
  }
  deriving (Eq, Ord, Show)

data ToolchainIdentitySnapshot = ToolchainIdentitySnapshot
  { toolchainIdentitySnapshotTargetTriple :: TargetTriple,
    toolchainIdentitySnapshotContract :: HostToolchainContract,
    toolchainIdentitySnapshotObservation :: ObservedToolchainIdentity
  }
  deriving (Eq, Ord, Show)

data ToolchainIdentityEvidence = ToolchainIdentityEvidence
  { toolchainIdentityEvidenceTargetTriple :: TargetTriple,
    toolchainIdentityEvidenceTools :: [ObservedToolIdentity],
    toolchainIdentityEvidenceSysrootIdentity :: Maybe ToolchainSysrootIdentity,
    toolchainIdentityEvidenceSystemLibraries :: [ObservedToolchainSystemLibrary],
    toolchainIdentityEvidenceCodegenSettings :: [ToolchainCodegenSetting],
    toolchainIdentityEvidenceLinkerMode :: Maybe ToolchainLinkerMode
  }
  deriving (Eq, Ord, Show)

data ToolchainIdentityViolation
  = MissingDeclaredToolchainLinkerMode
  | BlankDeclaredToolchainLinkerMode
  | BlankDeclaredSysrootIdentity
  | BlankDeclaredSysrootUnavailableReason
  | DuplicateDeclaredToolRole String
  | DuplicateDeclaredSystemLibraryIdentity String
  | DuplicateDeclaredCodegenSetting String
  | DuplicateObservedToolRole String
  | DuplicateObservedSystemLibraryIdentity String
  | DuplicateObservedCodegenSetting String
  | TargetTripleMismatch String String
  | MissingObservedToolRole String
  | UndeclaredObservedToolRole String
  | ToolAvailabilityMismatch String String String
  | ToolPathMismatch String String String
  | ToolDigestMismatch String String String
  | ToolUnavailableReasonMismatch String String String
  | ToolVersionMismatch String String String
  | DeclaredToolVersionOnly String
  | ObservedToolVersionOnly String
  | DeclaredToolIdentityIncomplete String
  | ObservedToolIdentityIncomplete String
  | MissingObservedSysrootIdentity
  | SysrootIdentityMismatch String String
  | MissingObservedSystemLibraryIdentity String
  | UndeclaredObservedSystemLibraryIdentity String
  | SystemLibraryIdentityMismatch String String String
  | MissingObservedCodegenSetting String
  | UndeclaredObservedCodegenSetting String
  | CodegenSettingMismatch String String String
  | MissingObservedLinkerMode
  | LinkerModeMismatch String String
  deriving (Eq, Ord, Show)

validatePlatformToolchainIdentity :: ToolchainIdentitySnapshot -> Either [ToolchainIdentityViolation] ToolchainIdentityEvidence
validatePlatformToolchainIdentity snapshot =
  case violations of
    [] -> Right (toolchainIdentityEvidence snapshot)
    _ -> Left (sort violations)
  where
    contract =
      toolchainIdentitySnapshotContract snapshot
    observation =
      toolchainIdentitySnapshotObservation snapshot
    violations =
      concat
        [ validateDeclaredToolchainContract contract,
          validateObservedToolchainIdentity observation,
          validateTargetTriple
            (toolchainIdentitySnapshotTargetTriple snapshot)
            (observedToolchainTargetTriple observation),
          validateTools (hostToolchainTools contract) (observedToolchainTools observation),
          validateSysrootIdentity (hostToolchainSysrootIdentity contract) (observedToolchainSysrootIdentity observation),
          validateSystemLibraries
            (hostToolchainSystemLibraries contract)
            (observedToolchainSystemLibraries observation),
          validateCodegenSettings
            (hostToolchainCodegenSettings contract)
            (observedToolchainCodegenSettings observation),
          validateLinkerMode (hostToolchainLinkerMode contract) (observedToolchainLinkerMode observation)
        ]

renderPlatformToolchainIdentityEvidence :: ToolchainIdentityEvidence -> String
renderPlatformToolchainIdentityEvidence evidence =
  unlines $
    [ "mlf-platform-toolchain-identity-evidence-v1",
      "target-triple: " ++ unTargetTriple (toolchainIdentityEvidenceTargetTriple evidence),
      "tools:"
    ]
      ++ renderIndentedItems (map renderObservedToolIdentity (sortOn observedToolKey (toolchainIdentityEvidenceTools evidence)))
      ++ [ "sysroot: " ++ renderToolchainSysrootIdentity (toolchainIdentityEvidenceSysrootIdentity evidence),
           "system-libraries:"
         ]
      ++ renderIndentedItems
        ( map
            renderObservedToolchainSystemLibrary
            (sortOn observedToolchainSystemLibraryName (toolchainIdentityEvidenceSystemLibraries evidence))
        )
      ++ ["codegen-settings:"]
      ++ renderIndentedItems
        (map renderToolchainCodegenSetting (sortOn toolchainCodegenSettingKey (toolchainIdentityEvidenceCodegenSettings evidence)))
      ++ ["linker-mode: " ++ renderToolchainLinkerMode (toolchainIdentityEvidenceLinkerMode evidence)]

renderPlatformToolchainIdentityViolation :: ToolchainIdentityViolation -> String
renderPlatformToolchainIdentityViolation violation =
  case violation of
    MissingDeclaredToolchainLinkerMode ->
      "declared host toolchain linker mode is missing"
    BlankDeclaredToolchainLinkerMode ->
      "declared host toolchain linker mode is blank"
    BlankDeclaredSysrootIdentity ->
      "declared host toolchain sysroot identity is blank"
    BlankDeclaredSysrootUnavailableReason ->
      "declared host toolchain sysroot unavailable reason is blank"
    DuplicateDeclaredToolRole role ->
      "duplicate declared tool role: " ++ renderNamedValue role
    DuplicateDeclaredSystemLibraryIdentity name ->
      "duplicate declared system library identity: " ++ renderNamedValue name
    DuplicateDeclaredCodegenSetting key ->
      "duplicate declared codegen setting: " ++ renderNamedValue key
    DuplicateObservedToolRole role ->
      "duplicate observed tool role: " ++ renderNamedValue role
    DuplicateObservedSystemLibraryIdentity name ->
      "duplicate observed system library identity: " ++ renderNamedValue name
    DuplicateObservedCodegenSetting key ->
      "duplicate observed codegen setting: " ++ renderNamedValue key
    TargetTripleMismatch expected observed ->
      "target triple mismatch: expected " ++ expected ++ " observed " ++ observed
    MissingObservedToolRole role ->
      "declared required tool role missing from observation: " ++ renderNamedValue role
    UndeclaredObservedToolRole role ->
      "observed tool role has no declared tool contract: " ++ renderNamedValue role
    ToolAvailabilityMismatch role expected observed ->
      "tool " ++ renderNamedValue role ++ " availability mismatch: expected " ++ expected ++ " observed " ++ observed
    ToolPathMismatch role expected observed ->
      "tool " ++ renderNamedValue role ++ " path mismatch: expected " ++ expected ++ " observed " ++ observed
    ToolDigestMismatch role expected observed ->
      "tool " ++ renderNamedValue role ++ " digest mismatch: expected " ++ expected ++ " observed " ++ observed
    ToolUnavailableReasonMismatch role expected observed ->
      "tool " ++ renderNamedValue role ++ " unavailable reason mismatch: expected " ++ expected ++ " observed " ++ observed
    ToolVersionMismatch role expected observed ->
      "tool " ++ renderNamedValue role ++ " version mismatch: expected " ++ expected ++ " observed " ++ observed
    DeclaredToolVersionOnly role ->
      "declared tool " ++ renderNamedValue role ++ " uses a version string alone as proof identity"
    ObservedToolVersionOnly role ->
      "observed tool " ++ renderNamedValue role ++ " uses a version string alone as proof identity"
    DeclaredToolIdentityIncomplete role ->
      "declared tool " ++ renderNamedValue role ++ " requires resolved path plus digest or explicit unavailable reason"
    ObservedToolIdentityIncomplete role ->
      "observed tool " ++ renderNamedValue role ++ " requires resolved path plus digest or explicit unavailable reason"
    MissingObservedSysrootIdentity ->
      "declared sysroot identity is missing from observations"
    SysrootIdentityMismatch expected observed ->
      "sysroot identity mismatch: expected " ++ expected ++ " observed " ++ observed
    MissingObservedSystemLibraryIdentity name ->
      "declared system library identity is missing from observations: " ++ renderNamedValue name
    UndeclaredObservedSystemLibraryIdentity name ->
      "observed system library identity has no declaration: " ++ renderNamedValue name
    SystemLibraryIdentityMismatch name expected observed ->
      "system library " ++ renderNamedValue name ++ " identity mismatch: expected " ++ expected ++ " observed " ++ observed
    MissingObservedCodegenSetting key ->
      "declared codegen setting is missing from observations: " ++ renderNamedValue key
    UndeclaredObservedCodegenSetting key ->
      "observed codegen setting has no declaration: " ++ renderNamedValue key
    CodegenSettingMismatch key expected observed ->
      "codegen setting " ++ renderNamedValue key ++ " mismatch: expected " ++ expected ++ " observed " ++ observed
    MissingObservedLinkerMode ->
      "declared linker mode is missing from observations"
    LinkerModeMismatch expected observed ->
      "linker mode mismatch: expected " ++ expected ++ " observed " ++ observed

renderPlatformToolchainIdentityViolations :: [ToolchainIdentityViolation] -> String
renderPlatformToolchainIdentityViolations =
  unlines . map renderPlatformToolchainIdentityViolation . sort

validateDeclaredToolchainContract :: HostToolchainContract -> [ToolchainIdentityViolation]
validateDeclaredToolchainContract contract =
  concat
    [ map DuplicateDeclaredToolRole (duplicates (map toolRoleKey tools)),
      concatMap validateDeclaredTool tools,
      validateDeclaredSysroot (hostToolchainSysrootIdentity contract),
      map DuplicateDeclaredSystemLibraryIdentity (duplicates (map toolchainSystemLibraryName libraries)),
      map DuplicateDeclaredCodegenSetting (duplicates (map toolchainCodegenSettingKey settings)),
      validateDeclaredLinkerMode (hostToolchainLinkerMode contract)
    ]
  where
    tools =
      hostToolchainTools contract
    libraries =
      hostToolchainSystemLibraries contract
    settings =
      hostToolchainCodegenSettings contract

validateObservedToolchainIdentity :: ObservedToolchainIdentity -> [ToolchainIdentityViolation]
validateObservedToolchainIdentity observation =
  concat
    [ map DuplicateObservedToolRole (duplicates (map observedToolKey tools)),
      concatMap validateObservedTool tools,
      map DuplicateObservedSystemLibraryIdentity (duplicates (map observedToolchainSystemLibraryName libraries)),
      map DuplicateObservedCodegenSetting (duplicates (map toolchainCodegenSettingKey settings))
    ]
  where
    tools =
      observedToolchainTools observation
    libraries =
      observedToolchainSystemLibraries observation
    settings =
      observedToolchainCodegenSettings observation

validateDeclaredTool :: ResolvedToolIdentity -> [ToolchainIdentityViolation]
validateDeclaredTool tool =
  [DeclaredToolVersionOnly role | resolvedToolVersionOnly tool]
    ++ [DeclaredToolIdentityIncomplete role | not (resolvedToolHasProofIdentity tool)]
  where
    role =
      toolRoleKey tool

validateObservedTool :: ObservedToolIdentity -> [ToolchainIdentityViolation]
validateObservedTool tool =
  [ObservedToolVersionOnly role | observedToolVersionOnly tool]
    ++ [ObservedToolIdentityIncomplete role | not (observedToolHasProofIdentity tool)]
  where
    role =
      observedToolKey tool

validateDeclaredSysroot :: Maybe ToolchainSysrootIdentity -> [ToolchainIdentityViolation]
validateDeclaredSysroot value =
  case value of
    Nothing -> []
    Just sysroot ->
      case sysroot of
        ToolchainSysrootAvailable identity
          | isBlank identity -> [BlankDeclaredSysrootIdentity]
        ToolchainSysrootUnavailable reason
          | isBlank reason -> [BlankDeclaredSysrootUnavailableReason]
        _ -> []

validateDeclaredLinkerMode :: Maybe ToolchainLinkerMode -> [ToolchainIdentityViolation]
validateDeclaredLinkerMode value =
  case value of
    Nothing -> [MissingDeclaredToolchainLinkerMode]
    Just mode
      | isBlank (unToolchainLinkerMode mode) -> [BlankDeclaredToolchainLinkerMode]
      | otherwise -> []

validateTargetTriple :: TargetTriple -> TargetTriple -> [ToolchainIdentityViolation]
validateTargetTriple expected observed =
  [ TargetTripleMismatch (unTargetTriple expected) (unTargetTriple observed)
    | expected /= observed
  ]

validateTools :: [ResolvedToolIdentity] -> [ObservedToolIdentity] -> [ToolchainIdentityViolation]
validateTools declared observed =
  concat
    [ map (MissingObservedToolRole . toolRoleKey) (filter (not . hasObservedToolRole observed . resolvedToolRole) declared),
      map (UndeclaredObservedToolRole . observedToolKey) (filter (not . hasDeclaredToolRole declared . observedToolRole) observed),
      concatMap (validateObservedToolMatch observed) declared
    ]

validateObservedToolMatch :: [ObservedToolIdentity] -> ResolvedToolIdentity -> [ToolchainIdentityViolation]
validateObservedToolMatch observed declared =
  case findObservedTool (resolvedToolRole declared) observed of
    Nothing -> []
    Just matched ->
      concat
        [ compareAvailability role (resolvedToolIsAvailable declared) (observedToolAvailable matched),
          compareMaybeText (ToolPathMismatch role) (resolvedToolPath declared) (observedToolPath matched),
          compareMaybeText (ToolDigestMismatch role) (resolvedToolDigest declared) (observedToolDigest matched),
          compareMaybeText
            (ToolUnavailableReasonMismatch role)
            (resolvedToolUnavailableReason declared)
            (observedToolUnavailableReason matched),
          compareMaybeText (ToolVersionMismatch role) (resolvedToolVersion declared) (observedToolVersion matched)
        ]
  where
    role =
      toolRoleKey declared

validateSysrootIdentity :: Maybe ToolchainSysrootIdentity -> Maybe ToolchainSysrootIdentity -> [ToolchainIdentityViolation]
validateSysrootIdentity declared observed =
  case declared of
    Nothing -> []
    Just expected ->
      case observed of
        Nothing -> [MissingObservedSysrootIdentity]
        Just actual
          | expected == actual -> []
          | otherwise -> [SysrootIdentityMismatch (renderToolchainSysrootIdentity (Just expected)) (renderToolchainSysrootIdentity (Just actual))]

validateSystemLibraries ::
  [ToolchainSystemLibraryIdentity] ->
  [ObservedToolchainSystemLibrary] ->
  [ToolchainIdentityViolation]
validateSystemLibraries declared observed =
  concat
    [ map
        (MissingObservedSystemLibraryIdentity . toolchainSystemLibraryName)
        (filter (not . hasObservedSystemLibrary observed . toolchainSystemLibraryName) declared),
      map
        (UndeclaredObservedSystemLibraryIdentity . observedToolchainSystemLibraryName)
        (filter (not . hasDeclaredSystemLibrary declared . observedToolchainSystemLibraryName) observed),
      concatMap (validateObservedSystemLibraryMatch observed) declared
    ]

validateObservedSystemLibraryMatch ::
  [ObservedToolchainSystemLibrary] ->
  ToolchainSystemLibraryIdentity ->
  [ToolchainIdentityViolation]
validateObservedSystemLibraryMatch observed declared =
  case findObservedSystemLibrary (toolchainSystemLibraryName declared) observed of
    Nothing -> []
    Just matched
      | toolchainSystemLibraryIdentity declared == observedToolchainSystemLibraryIdentity matched -> []
      | otherwise ->
          [ SystemLibraryIdentityMismatch
              (toolchainSystemLibraryName declared)
              (toolchainSystemLibraryIdentity declared)
              (observedToolchainSystemLibraryIdentity matched)
          ]

validateCodegenSettings :: [ToolchainCodegenSetting] -> [ToolchainCodegenSetting] -> [ToolchainIdentityViolation]
validateCodegenSettings declared observed =
  concat
    [ map
        (MissingObservedCodegenSetting . toolchainCodegenSettingKey)
        (filter (not . hasObservedCodegenSetting observed . toolchainCodegenSettingKey) declared),
      map
        (UndeclaredObservedCodegenSetting . toolchainCodegenSettingKey)
        (filter (not . hasDeclaredCodegenSetting declared . toolchainCodegenSettingKey) observed),
      concatMap (validateObservedCodegenSettingMatch observed) declared
    ]

validateObservedCodegenSettingMatch :: [ToolchainCodegenSetting] -> ToolchainCodegenSetting -> [ToolchainIdentityViolation]
validateObservedCodegenSettingMatch observed declared =
  case findObservedCodegenSetting (toolchainCodegenSettingKey declared) observed of
    Nothing -> []
    Just matched
      | toolchainCodegenSettingValue declared == toolchainCodegenSettingValue matched -> []
      | otherwise ->
          [ CodegenSettingMismatch
              (toolchainCodegenSettingKey declared)
              (toolchainCodegenSettingValue declared)
              (toolchainCodegenSettingValue matched)
          ]

validateLinkerMode :: Maybe ToolchainLinkerMode -> Maybe ToolchainLinkerMode -> [ToolchainIdentityViolation]
validateLinkerMode declared observed =
  case declared of
    Nothing -> []
    Just expected ->
      case observed of
        Nothing -> [MissingObservedLinkerMode]
        Just actual
          | expected == actual -> []
          | otherwise -> [LinkerModeMismatch (unToolchainLinkerMode expected) (unToolchainLinkerMode actual)]

toolchainIdentityEvidence :: ToolchainIdentitySnapshot -> ToolchainIdentityEvidence
toolchainIdentityEvidence snapshot =
  ToolchainIdentityEvidence
    { toolchainIdentityEvidenceTargetTriple =
        toolchainIdentitySnapshotTargetTriple snapshot,
      toolchainIdentityEvidenceTools =
        observedToolchainTools observation,
      toolchainIdentityEvidenceSysrootIdentity =
        observedToolchainSysrootIdentity observation,
      toolchainIdentityEvidenceSystemLibraries =
        observedToolchainSystemLibraries observation,
      toolchainIdentityEvidenceCodegenSettings =
        observedToolchainCodegenSettings observation,
      toolchainIdentityEvidenceLinkerMode =
        observedToolchainLinkerMode observation
    }
  where
    observation =
      toolchainIdentitySnapshotObservation snapshot

compareAvailability :: String -> Bool -> Bool -> [ToolchainIdentityViolation]
compareAvailability role expected observed =
  [ ToolAvailabilityMismatch role (renderAvailability expected) (renderAvailability observed)
    | expected /= observed
  ]

compareMaybeText :: (String -> String -> ToolchainIdentityViolation) -> Maybe String -> Maybe String -> [ToolchainIdentityViolation]
compareMaybeText mismatch expected observed =
  [mismatch (renderMaybeText expected) (renderMaybeText observed) | normalizeMaybeText expected /= normalizeMaybeText observed]

resolvedToolHasProofIdentity :: ResolvedToolIdentity -> Bool
resolvedToolHasProofIdentity tool =
  (hasNonBlankMaybeText (resolvedToolPath tool) && hasNonBlankMaybeText (resolvedToolDigest tool))
    || hasNonBlankMaybeText (resolvedToolUnavailableReason tool)

observedToolHasProofIdentity :: ObservedToolIdentity -> Bool
observedToolHasProofIdentity tool =
  (hasNonBlankMaybeText (observedToolPath tool) && hasNonBlankMaybeText (observedToolDigest tool))
    || hasNonBlankMaybeText (observedToolUnavailableReason tool)

resolvedToolVersionOnly :: ResolvedToolIdentity -> Bool
resolvedToolVersionOnly tool =
  hasNonBlankMaybeText (resolvedToolVersion tool)
    && not (hasNonBlankMaybeText (resolvedToolPath tool))
    && not (hasNonBlankMaybeText (resolvedToolDigest tool))
    && not (hasNonBlankMaybeText (resolvedToolUnavailableReason tool))

observedToolVersionOnly :: ObservedToolIdentity -> Bool
observedToolVersionOnly tool =
  hasNonBlankMaybeText (observedToolVersion tool)
    && not (hasNonBlankMaybeText (observedToolPath tool))
    && not (hasNonBlankMaybeText (observedToolDigest tool))
    && not (hasNonBlankMaybeText (observedToolUnavailableReason tool))

resolvedToolIsAvailable :: ResolvedToolIdentity -> Bool
resolvedToolIsAvailable =
  not . hasNonBlankMaybeText . resolvedToolUnavailableReason

renderObservedToolIdentity :: ObservedToolIdentity -> String
renderObservedToolIdentity tool =
  concat
    [ "role=",
      observedToolKey tool,
      " availability=",
      renderAvailability (observedToolAvailable tool),
      " path=",
      renderMaybeText (observedToolPath tool),
      " digest=",
      renderMaybeText (observedToolDigest tool),
      " unavailable-reason=",
      renderMaybeText (observedToolUnavailableReason tool),
      " version=",
      renderMaybeText (observedToolVersion tool)
    ]

renderToolchainSysrootIdentity :: Maybe ToolchainSysrootIdentity -> String
renderToolchainSysrootIdentity value =
  case value of
    Nothing ->
      "availability=unavailable reason=<none>"
    Just sysroot ->
      case sysroot of
        ToolchainSysrootAvailable identity ->
          "availability=available identity=" ++ identity
        ToolchainSysrootUnavailable reason ->
          "availability=unavailable reason=" ++ reason

renderObservedToolchainSystemLibrary :: ObservedToolchainSystemLibrary -> String
renderObservedToolchainSystemLibrary library =
  concat
    [ "name=",
      observedToolchainSystemLibraryName library,
      " identity=",
      observedToolchainSystemLibraryIdentity library
    ]

renderToolchainCodegenSetting :: ToolchainCodegenSetting -> String
renderToolchainCodegenSetting setting =
  concat
    [ "key=",
      toolchainCodegenSettingKey setting,
      " value=",
      toolchainCodegenSettingValue setting
    ]

renderToolchainLinkerMode :: Maybe ToolchainLinkerMode -> String
renderToolchainLinkerMode value =
  case value of
    Nothing ->
      "<missing>"
    Just mode ->
      unToolchainLinkerMode mode

renderAvailability :: Bool -> String
renderAvailability available =
  if available
    then "available"
    else "unavailable"

renderNamedValue :: String -> String
renderNamedValue value
  | isBlank value = "<blank>"
  | otherwise = value

renderMaybeText :: Maybe String -> String
renderMaybeText value =
  case normalizeMaybeText value of
    Nothing -> "<none>"
    Just text -> text

renderIndentedItems :: [String] -> [String]
renderIndentedItems values =
  case values of
    [] -> ["  - <none>"]
    _ -> map ("  - " ++) values

normalizeMaybeText :: Maybe String -> Maybe String
normalizeMaybeText value =
  case value of
    Nothing -> Nothing
    Just text
      | isBlank text -> Nothing
      | otherwise -> Just text

hasNonBlankMaybeText :: Maybe String -> Bool
hasNonBlankMaybeText =
  maybe False (not . isBlank)

findObservedTool :: ToolchainToolRole -> [ObservedToolIdentity] -> Maybe ObservedToolIdentity
findObservedTool role =
  find ((== role) . observedToolRole)

findObservedSystemLibrary :: String -> [ObservedToolchainSystemLibrary] -> Maybe ObservedToolchainSystemLibrary
findObservedSystemLibrary name =
  find ((== name) . observedToolchainSystemLibraryName)

findObservedCodegenSetting :: String -> [ToolchainCodegenSetting] -> Maybe ToolchainCodegenSetting
findObservedCodegenSetting key =
  find ((== key) . toolchainCodegenSettingKey)

hasDeclaredToolRole :: [ResolvedToolIdentity] -> ToolchainToolRole -> Bool
hasDeclaredToolRole tools role =
  any ((== role) . resolvedToolRole) tools

hasObservedToolRole :: [ObservedToolIdentity] -> ToolchainToolRole -> Bool
hasObservedToolRole tools role =
  any ((== role) . observedToolRole) tools

hasDeclaredSystemLibrary :: [ToolchainSystemLibraryIdentity] -> String -> Bool
hasDeclaredSystemLibrary libraries name =
  any ((== name) . toolchainSystemLibraryName) libraries

hasObservedSystemLibrary :: [ObservedToolchainSystemLibrary] -> String -> Bool
hasObservedSystemLibrary libraries name =
  any ((== name) . observedToolchainSystemLibraryName) libraries

hasDeclaredCodegenSetting :: [ToolchainCodegenSetting] -> String -> Bool
hasDeclaredCodegenSetting settings key =
  any ((== key) . toolchainCodegenSettingKey) settings

hasObservedCodegenSetting :: [ToolchainCodegenSetting] -> String -> Bool
hasObservedCodegenSetting settings key =
  any ((== key) . toolchainCodegenSettingKey) settings

toolRoleKey :: ResolvedToolIdentity -> String
toolRoleKey tool =
  unToolchainToolRole (resolvedToolRole tool)

observedToolKey :: ObservedToolIdentity -> String
observedToolKey tool =
  unToolchainToolRole (observedToolRole tool)

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

isBlank :: String -> Bool
isBlank =
  all isSpace
