module MLF.Platform.Contract
  ( PlatformAbiVersion (..),
    PlatformSubstrateContractPackageId (..),
    PlatformSubstrateContractPackageVersion (..),
    TargetTriple (..),
    SubstrateComponentKind (..),
    SubstrateComponentName (..),
    SubstrateComponentDigest (..),
    SubstrateComponent (..),
    ToolchainToolRole (..),
    ResolvedToolIdentity (..),
    HostToolchainContract (..),
    AmbientInputName (..),
    AmbientInputDisposition (..),
    AmbientInputRule (..),
    AmbientInputPolicy (..),
    LoaderEnvironmentVariable (..),
    LoaderEnvironmentDisposition (..),
    LoaderEnvironmentRule (..),
    LoaderEnvironmentPolicy (..),
    PlatformSubstrateContract (..),
    PlatformContractError (..),
    validatePlatformSubstrateContract,
    renderPlatformContractError,
    renderPlatformContractErrors,
    renderPlatformSubstrateContract,
    renderSubstrateFingerprintMaterial,
  )
where

import Data.Char (isSpace)
import Data.List (group, intercalate, sort, sortOn)

newtype PlatformAbiVersion = PlatformAbiVersion
  { unPlatformAbiVersion :: String
  }
  deriving (Eq, Ord, Show)

newtype PlatformSubstrateContractPackageId = PlatformSubstrateContractPackageId
  { unPlatformSubstrateContractPackageId :: String
  }
  deriving (Eq, Ord, Show)

newtype PlatformSubstrateContractPackageVersion = PlatformSubstrateContractPackageVersion
  { unPlatformSubstrateContractPackageVersion :: String
  }
  deriving (Eq, Ord, Show)

newtype TargetTriple = TargetTriple
  { unTargetTriple :: String
  }
  deriving (Eq, Ord, Show)

newtype SubstrateComponentKind = SubstrateComponentKind
  { unSubstrateComponentKind :: String
  }
  deriving (Eq, Ord, Show)

newtype SubstrateComponentName = SubstrateComponentName
  { unSubstrateComponentName :: String
  }
  deriving (Eq, Ord, Show)

newtype SubstrateComponentDigest = SubstrateComponentDigest
  { unSubstrateComponentDigest :: String
  }
  deriving (Eq, Ord, Show)

data SubstrateComponent = SubstrateComponent
  { substrateComponentKind :: SubstrateComponentKind,
    substrateComponentName :: SubstrateComponentName,
    substrateComponentDigest :: SubstrateComponentDigest
  }
  deriving (Eq, Ord, Show)

newtype ToolchainToolRole = ToolchainToolRole
  { unToolchainToolRole :: String
  }
  deriving (Eq, Ord, Show)

data ResolvedToolIdentity = ResolvedToolIdentity
  { resolvedToolRole :: ToolchainToolRole,
    resolvedToolPath :: Maybe FilePath,
    resolvedToolDigest :: Maybe String,
    resolvedToolUnavailableReason :: Maybe String,
    resolvedToolVersion :: Maybe String
  }
  deriving (Eq, Ord, Show)

newtype HostToolchainContract = HostToolchainContract
  { hostToolchainTools :: [ResolvedToolIdentity]
  }
  deriving (Eq, Ord, Show)

newtype AmbientInputName = AmbientInputName
  { unAmbientInputName :: String
  }
  deriving (Eq, Ord, Show)

data AmbientInputDisposition
  = AmbientInputScrubbed
  | AmbientInputDeclared String
  | AmbientInputNormalized String
  deriving (Eq, Ord, Show)

data AmbientInputRule = AmbientInputRule
  { ambientInputRuleName :: AmbientInputName,
    ambientInputRuleDisposition :: AmbientInputDisposition
  }
  deriving (Eq, Ord, Show)

data AmbientInputPolicy = AmbientInputPolicy
  { ambientInputPolicyName :: String,
    ambientInputPolicyRules :: [AmbientInputRule]
  }
  deriving (Eq, Ord, Show)

newtype LoaderEnvironmentVariable = LoaderEnvironmentVariable
  { unLoaderEnvironmentVariable :: String
  }
  deriving (Eq, Ord, Show)

data LoaderEnvironmentDisposition
  = LoaderEnvironmentScrubbed
  | LoaderEnvironmentDeclared String
  | LoaderEnvironmentNormalized String
  deriving (Eq, Ord, Show)

data LoaderEnvironmentRule = LoaderEnvironmentRule
  { loaderEnvironmentRuleVariable :: LoaderEnvironmentVariable,
    loaderEnvironmentRuleDisposition :: LoaderEnvironmentDisposition
  }
  deriving (Eq, Ord, Show)

data LoaderEnvironmentPolicy = LoaderEnvironmentPolicy
  { loaderEnvironmentPolicyName :: String,
    loaderEnvironmentPolicyRules :: [LoaderEnvironmentRule]
  }
  deriving (Eq, Ord, Show)

data PlatformSubstrateContract = PlatformSubstrateContract
  { platformContractAbiVersion :: Maybe PlatformAbiVersion,
    platformContractPackageId :: Maybe PlatformSubstrateContractPackageId,
    platformContractPackageVersion :: Maybe PlatformSubstrateContractPackageVersion,
    platformContractTargetTriple :: Maybe TargetTriple,
    platformContractSubstrateComponents :: [SubstrateComponent],
    platformContractHostToolchain :: HostToolchainContract,
    platformContractAmbientInputPolicy :: Maybe AmbientInputPolicy,
    platformContractLoaderEnvironmentPolicy :: Maybe LoaderEnvironmentPolicy
  }
  deriving (Eq, Ord, Show)

data PlatformContractError
  = MissingPlatformAbiVersion
  | EmptyPlatformAbiVersion
  | MissingPlatformSubstrateContractPackageId
  | EmptyPlatformSubstrateContractPackageId
  | MissingPlatformSubstrateContractPackageVersion
  | EmptyPlatformSubstrateContractPackageVersion
  | MissingTargetTriple
  | EmptyTargetTriple
  | EmptySubstrateComponentKind
  | EmptySubstrateComponentName
  | EmptySubstrateComponentDigest
  | DuplicateSubstrateComponentKey String
  | EmptyToolchainToolRole
  | DuplicateHostToolchainRole String
  | IncompleteResolvedToolIdentity String
  | MissingAmbientInputPolicy
  | EmptyAmbientInputPolicyName
  | EmptyAmbientInputRuleName
  | EmptyAmbientInputNormalizedValue String
  | DuplicateAmbientInputRule String
  | MissingLoaderEnvironmentPolicy
  | EmptyLoaderEnvironmentPolicyName
  | EmptyLoaderEnvironmentVariable
  | EmptyLoaderEnvironmentNormalizedValue String
  | DuplicateLoaderEnvironmentRule String
  deriving (Eq, Ord, Show)

validatePlatformSubstrateContract :: PlatformSubstrateContract -> [PlatformContractError]
validatePlatformSubstrateContract contract =
  concat
    [ validateRequiredMaybe
        MissingPlatformAbiVersion
        EmptyPlatformAbiVersion
        unPlatformAbiVersion
        (platformContractAbiVersion contract),
      validateRequiredMaybe
        MissingPlatformSubstrateContractPackageId
        EmptyPlatformSubstrateContractPackageId
        unPlatformSubstrateContractPackageId
        (platformContractPackageId contract),
      validateRequiredMaybe
        MissingPlatformSubstrateContractPackageVersion
        EmptyPlatformSubstrateContractPackageVersion
        unPlatformSubstrateContractPackageVersion
        (platformContractPackageVersion contract),
      validateRequiredMaybe
        MissingTargetTriple
        EmptyTargetTriple
        unTargetTriple
        (platformContractTargetTriple contract),
      validateSubstrateComponents (platformContractSubstrateComponents contract),
      validateHostToolchainContract (platformContractHostToolchain contract),
      validateAmbientInputPolicy (platformContractAmbientInputPolicy contract),
      validateLoaderEnvironmentPolicy (platformContractLoaderEnvironmentPolicy contract)
    ]

renderPlatformContractError :: PlatformContractError -> String
renderPlatformContractError err =
  case err of
    MissingPlatformAbiVersion ->
      "platform ABI version is missing"
    EmptyPlatformAbiVersion ->
      "platform ABI version is empty"
    MissingPlatformSubstrateContractPackageId ->
      "platform substrate contract package id is missing"
    EmptyPlatformSubstrateContractPackageId ->
      "platform substrate contract package id is empty"
    MissingPlatformSubstrateContractPackageVersion ->
      "platform substrate contract package version is missing"
    EmptyPlatformSubstrateContractPackageVersion ->
      "platform substrate contract package version is empty"
    MissingTargetTriple ->
      "target triple is missing"
    EmptyTargetTriple ->
      "target triple is empty"
    EmptySubstrateComponentKind ->
      "substrate component kind is empty"
    EmptySubstrateComponentName ->
      "substrate component name is empty"
    EmptySubstrateComponentDigest ->
      "substrate component digest is empty"
    DuplicateSubstrateComponentKey key ->
      "duplicate substrate component key: " ++ key
    EmptyToolchainToolRole ->
      "host toolchain role is empty"
    DuplicateHostToolchainRole role ->
      "duplicate host toolchain role: " ++ role
    IncompleteResolvedToolIdentity role ->
      "host toolchain identity for role " ++ role ++ " requires resolved path plus digest or explicit unavailable reason; version string alone is not accepted"
    MissingAmbientInputPolicy ->
      "ambient-input policy is missing"
    EmptyAmbientInputPolicyName ->
      "ambient-input policy name is empty"
    EmptyAmbientInputRuleName ->
      "ambient-input rule name is empty"
    EmptyAmbientInputNormalizedValue name ->
      "ambient-input rule " ++ name ++ " has empty normalized value"
    DuplicateAmbientInputRule name ->
      "duplicate ambient-input rule: " ++ name
    MissingLoaderEnvironmentPolicy ->
      "loader-environment policy is missing"
    EmptyLoaderEnvironmentPolicyName ->
      "loader-environment policy name is empty"
    EmptyLoaderEnvironmentVariable ->
      "loader-environment rule variable is empty"
    EmptyLoaderEnvironmentNormalizedValue variable ->
      "loader-environment rule " ++ variable ++ " has empty normalized value"
    DuplicateLoaderEnvironmentRule variable ->
      "duplicate loader-environment rule: " ++ variable

renderPlatformContractErrors :: [PlatformContractError] -> String
renderPlatformContractErrors =
  unlines . map renderPlatformContractError

renderPlatformSubstrateContract :: PlatformSubstrateContract -> Either [PlatformContractError] String
renderPlatformSubstrateContract =
  renderValidatedContract "mlf-platform-substrate-contract-v1"

renderSubstrateFingerprintMaterial :: PlatformSubstrateContract -> Either [PlatformContractError] String
renderSubstrateFingerprintMaterial =
  renderValidatedContract "mlf-platform-substrate-fingerprint-material-v1"

validateRequiredMaybe :: PlatformContractError -> PlatformContractError -> (a -> String) -> Maybe a -> [PlatformContractError]
validateRequiredMaybe missingErr emptyErr project value =
  case value of
    Nothing -> [missingErr]
    Just wrapped
      | isBlank (project wrapped) -> [emptyErr]
      | otherwise -> []

validateSubstrateComponents :: [SubstrateComponent] -> [PlatformContractError]
validateSubstrateComponents components =
  concatMap validateSubstrateComponent components
    ++ map DuplicateSubstrateComponentKey (duplicates (map substrateComponentKey components))

validateSubstrateComponent :: SubstrateComponent -> [PlatformContractError]
validateSubstrateComponent component =
  concat
    [ [EmptySubstrateComponentKind | isBlank (unSubstrateComponentKind (substrateComponentKind component))],
      [EmptySubstrateComponentName | isBlank (unSubstrateComponentName (substrateComponentName component))],
      [EmptySubstrateComponentDigest | isBlank (unSubstrateComponentDigest (substrateComponentDigest component))]
    ]

validateHostToolchainContract :: HostToolchainContract -> [PlatformContractError]
validateHostToolchainContract (HostToolchainContract tools) =
  concatMap validateResolvedToolIdentity tools
    ++ map DuplicateHostToolchainRole (duplicates (map toolRoleKey tools))

validateResolvedToolIdentity :: ResolvedToolIdentity -> [PlatformContractError]
validateResolvedToolIdentity identity =
  [EmptyToolchainToolRole | isBlank role]
    ++ [IncompleteResolvedToolIdentity role | missingPath || missingDigestAndUnavailableReason]
  where
    role = unToolchainToolRole (resolvedToolRole identity)
    missingPath = maybe True isBlank (resolvedToolPath identity)
    missingDigestAndUnavailableReason =
      maybe True isBlank (resolvedToolDigest identity)
        && maybe True isBlank (resolvedToolUnavailableReason identity)

validateAmbientInputPolicy :: Maybe AmbientInputPolicy -> [PlatformContractError]
validateAmbientInputPolicy value =
  case value of
    Nothing -> [MissingAmbientInputPolicy]
    Just policy ->
      [EmptyAmbientInputPolicyName | isBlank (ambientInputPolicyName policy)]
        ++ concatMap validateAmbientInputRule (ambientInputPolicyRules policy)
        ++ map DuplicateAmbientInputRule (duplicates (map ambientInputRuleKey (ambientInputPolicyRules policy)))

validateAmbientInputRule :: AmbientInputRule -> [PlatformContractError]
validateAmbientInputRule rule =
  [EmptyAmbientInputRuleName | isBlank name]
    ++ case ambientInputRuleDisposition rule of
      AmbientInputNormalized value
        | isBlank value -> [EmptyAmbientInputNormalizedValue name]
      _ -> []
  where
    name = ambientInputRuleKey rule

validateLoaderEnvironmentPolicy :: Maybe LoaderEnvironmentPolicy -> [PlatformContractError]
validateLoaderEnvironmentPolicy value =
  case value of
    Nothing -> [MissingLoaderEnvironmentPolicy]
    Just policy ->
      [EmptyLoaderEnvironmentPolicyName | isBlank (loaderEnvironmentPolicyName policy)]
        ++ concatMap validateLoaderEnvironmentRule (loaderEnvironmentPolicyRules policy)
        ++ map DuplicateLoaderEnvironmentRule (duplicates (map loaderEnvironmentRuleKey (loaderEnvironmentPolicyRules policy)))

validateLoaderEnvironmentRule :: LoaderEnvironmentRule -> [PlatformContractError]
validateLoaderEnvironmentRule rule =
  [EmptyLoaderEnvironmentVariable | isBlank variable]
    ++ case loaderEnvironmentRuleDisposition rule of
      LoaderEnvironmentNormalized value
        | isBlank value -> [EmptyLoaderEnvironmentNormalizedValue variable]
      _ -> []
  where
    variable = loaderEnvironmentRuleKey rule

renderValidatedContract :: String -> PlatformSubstrateContract -> Either [PlatformContractError] String
renderValidatedContract header contract =
  case validatePlatformSubstrateContract contract of
    [] -> unlines <$> renderContractLines header contract
    errors -> Left errors

renderContractLines :: String -> PlatformSubstrateContract -> Either [PlatformContractError] [String]
renderContractLines header contract = do
  abiVersion <- requirePresent MissingPlatformAbiVersion (platformContractAbiVersion contract)
  packageId <- requirePresent MissingPlatformSubstrateContractPackageId (platformContractPackageId contract)
  packageVersion <- requirePresent MissingPlatformSubstrateContractPackageVersion (platformContractPackageVersion contract)
  targetTriple <- requirePresent MissingTargetTriple (platformContractTargetTriple contract)
  ambientPolicy <- requirePresent MissingAmbientInputPolicy (platformContractAmbientInputPolicy contract)
  loaderPolicy <- requirePresent MissingLoaderEnvironmentPolicy (platformContractLoaderEnvironmentPolicy contract)
  Right $
    [ header,
      "abi-version: " ++ unPlatformAbiVersion abiVersion,
      "contract-package-id: " ++ unPlatformSubstrateContractPackageId packageId,
      "contract-package-version: " ++ unPlatformSubstrateContractPackageVersion packageVersion,
      "target-triple: " ++ unTargetTriple targetTriple,
      "substrate-components:"
    ]
      ++ map renderSubstrateComponent (sortOn substrateComponentKey (platformContractSubstrateComponents contract))
      ++ ["host-toolchain:"]
      ++ map renderResolvedToolIdentity (sortOn toolRoleKey (hostToolchainTools (platformContractHostToolchain contract)))
      ++ renderAmbientInputPolicy ambientPolicy
      ++ renderLoaderEnvironmentPolicy loaderPolicy

renderSubstrateComponent :: SubstrateComponent -> String
renderSubstrateComponent component =
  concat
    [ "- kind=",
      unSubstrateComponentKind (substrateComponentKind component),
      " name=",
      unSubstrateComponentName (substrateComponentName component),
      " digest=",
      unSubstrateComponentDigest (substrateComponentDigest component)
    ]

renderResolvedToolIdentity :: ResolvedToolIdentity -> String
renderResolvedToolIdentity identity =
  concat
    [ "- role=",
      unToolchainToolRole (resolvedToolRole identity),
      " path=",
      renderMaybeText (resolvedToolPath identity),
      " digest=",
      renderMaybeText (resolvedToolDigest identity),
      " unavailable-reason=",
      renderMaybeText (resolvedToolUnavailableReason identity),
      " version=",
      renderMaybeText (resolvedToolVersion identity)
    ]

renderAmbientInputPolicy :: AmbientInputPolicy -> [String]
renderAmbientInputPolicy policy =
  [ "ambient-input-policy:",
    "  name: " ++ ambientInputPolicyName policy,
    "  rules:"
  ]
    ++ renderIndentedItems (map renderAmbientInputRule (sortOn ambientInputRuleKey (ambientInputPolicyRules policy)))

renderAmbientInputRule :: AmbientInputRule -> String
renderAmbientInputRule rule =
  concat
    [ "name=",
      ambientInputRuleKey rule,
      " ",
      renderAmbientInputDisposition (ambientInputRuleDisposition rule)
    ]

renderAmbientInputDisposition :: AmbientInputDisposition -> String
renderAmbientInputDisposition disposition =
  case disposition of
    AmbientInputScrubbed ->
      "disposition=scrubbed"
    AmbientInputDeclared value ->
      "disposition=declared value=" ++ value
    AmbientInputNormalized value ->
      "disposition=normalized value=" ++ value

renderLoaderEnvironmentPolicy :: LoaderEnvironmentPolicy -> [String]
renderLoaderEnvironmentPolicy policy =
  [ "loader-environment-policy:",
    "  name: " ++ loaderEnvironmentPolicyName policy,
    "  rules:"
  ]
    ++ renderIndentedItems (map renderLoaderEnvironmentRule (sortOn loaderEnvironmentRuleKey (loaderEnvironmentPolicyRules policy)))

renderLoaderEnvironmentRule :: LoaderEnvironmentRule -> String
renderLoaderEnvironmentRule rule =
  concat
    [ "variable=",
      loaderEnvironmentRuleKey rule,
      " ",
      renderLoaderEnvironmentDisposition (loaderEnvironmentRuleDisposition rule)
    ]

renderLoaderEnvironmentDisposition :: LoaderEnvironmentDisposition -> String
renderLoaderEnvironmentDisposition disposition =
  case disposition of
    LoaderEnvironmentScrubbed ->
      "disposition=scrubbed"
    LoaderEnvironmentDeclared value ->
      "disposition=declared value=" ++ value
    LoaderEnvironmentNormalized value ->
      "disposition=normalized value=" ++ value

renderIndentedItems :: [String] -> [String]
renderIndentedItems values =
  case values of
    [] -> ["  - <none>"]
    _ -> map ("  - " ++) values

renderMaybeText :: Maybe String -> String
renderMaybeText value =
  case value of
    Nothing -> "<none>"
    Just text
      | isBlank text -> "<none>"
      | otherwise -> text

requirePresent :: PlatformContractError -> Maybe a -> Either [PlatformContractError] a
requirePresent err value =
  case value of
    Nothing -> Left [err]
    Just present -> Right present

substrateComponentKey :: SubstrateComponent -> String
substrateComponentKey component =
  intercalate
    ":"
    [ unSubstrateComponentKind (substrateComponentKind component),
      unSubstrateComponentName (substrateComponentName component)
    ]

toolRoleKey :: ResolvedToolIdentity -> String
toolRoleKey identity =
  unToolchainToolRole (resolvedToolRole identity)

ambientInputRuleKey :: AmbientInputRule -> String
ambientInputRuleKey rule =
  unAmbientInputName (ambientInputRuleName rule)

loaderEnvironmentRuleKey :: LoaderEnvironmentRule -> String
loaderEnvironmentRuleKey rule =
  unLoaderEnvironmentVariable (loaderEnvironmentRuleVariable rule)

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
