module MLF.Platform.EnvironmentPolicy
  ( ObservedAmbientInput (..),
    ObservedLoaderEnvironmentVariable (..),
    EnvironmentPolicySnapshot (..),
    EnvironmentPolicyEvidence (..),
    EnvironmentPolicyViolation (..),
    validatePlatformEnvironmentPolicies,
    renderPlatformEnvironmentPolicyEvidence,
    renderPlatformEnvironmentPolicyViolation,
    renderPlatformEnvironmentPolicyViolations,
  )
where

import Data.Char (isSpace)
import Data.List (find, group, sort, sortOn)
import MLF.Platform.Contract
  ( AmbientInputDisposition (..),
    AmbientInputName (..),
    AmbientInputPolicy (..),
    AmbientInputRule (..),
    LoaderEnvironmentDisposition (..),
    LoaderEnvironmentPolicy (..),
    LoaderEnvironmentRule (..),
    LoaderEnvironmentVariable (..),
  )

data ObservedAmbientInput = ObservedAmbientInput
  { observedAmbientInputName :: AmbientInputName,
    observedAmbientInputValue :: Maybe String
  }
  deriving (Eq, Ord, Show)

data ObservedLoaderEnvironmentVariable = ObservedLoaderEnvironmentVariable
  { observedLoaderEnvironmentVariable :: LoaderEnvironmentVariable,
    observedLoaderEnvironmentValue :: Maybe String
  }
  deriving (Eq, Ord, Show)

data EnvironmentPolicySnapshot = EnvironmentPolicySnapshot
  { environmentPolicySnapshotAmbientPolicy :: AmbientInputPolicy,
    environmentPolicySnapshotLoaderPolicy :: LoaderEnvironmentPolicy,
    environmentPolicySnapshotAmbientInputs :: [ObservedAmbientInput],
    environmentPolicySnapshotLoaderEnvironment :: [ObservedLoaderEnvironmentVariable]
  }
  deriving (Eq, Ord, Show)

data EnvironmentPolicyEvidence = EnvironmentPolicyEvidence
  { environmentPolicyEvidenceAmbientPolicyName :: String,
    environmentPolicyEvidenceAmbientInputs :: [(AmbientInputName, AmbientInputDisposition, Maybe String)],
    environmentPolicyEvidenceLoaderPolicyName :: String,
    environmentPolicyEvidenceLoaderVariables :: [(LoaderEnvironmentVariable, LoaderEnvironmentDisposition, Maybe String)]
  }
  deriving (Eq, Ord, Show)

data EnvironmentPolicyViolation
  = DuplicateAmbientInputRule String
  | DuplicateLoaderEnvironmentRule String
  | UndeclaredAmbientInput String
  | UndeclaredLoaderEnvironmentVariable String
  | ScrubbedAmbientInputObserved String
  | ScrubbedLoaderEnvironmentVariableObserved String
  | AmbientInputDeclaredValueMismatch String String String
  | LoaderEnvironmentDeclaredValueMismatch String String String
  | AmbientInputNormalizedValueMismatch String String String
  | LoaderEnvironmentNormalizedValueMismatch String String String
  | MissingDeclaredAmbientInput String
  | MissingDeclaredLoaderEnvironmentVariable String
  | MissingNormalizedAmbientInput String
  | MissingNormalizedLoaderEnvironmentVariable String
  | BlankAmbientInputName
  | BlankLoaderEnvironmentVariable
  | BlankAmbientInputNormalizedValue String
  | BlankLoaderEnvironmentNormalizedValue String
  deriving (Eq, Ord, Show)

validatePlatformEnvironmentPolicies :: EnvironmentPolicySnapshot -> Either [EnvironmentPolicyViolation] EnvironmentPolicyEvidence
validatePlatformEnvironmentPolicies snapshot =
  case violations of
    [] -> Right (environmentPolicyEvidence snapshot)
    _ -> Left (sort violations)
  where
    ambientRules =
      ambientInputPolicyRules (environmentPolicySnapshotAmbientPolicy snapshot)
    loaderRules =
      loaderEnvironmentPolicyRules (environmentPolicySnapshotLoaderPolicy snapshot)
    ambientObserved =
      environmentPolicySnapshotAmbientInputs snapshot
    loaderObserved =
      environmentPolicySnapshotLoaderEnvironment snapshot
    violations =
      concat
        [ validateAmbientRules ambientRules,
          validateLoaderRules loaderRules,
          validateObservedAmbientInputNames ambientObserved,
          validateObservedLoaderEnvironmentNames loaderObserved,
          validateUndeclaredAmbientInputs ambientRules ambientObserved,
          validateUndeclaredLoaderEnvironment loaderRules loaderObserved,
          concatMap (validateAmbientRuleObservation ambientObserved) ambientRules,
          concatMap (validateLoaderRuleObservation loaderObserved) loaderRules
        ]

renderPlatformEnvironmentPolicyEvidence :: EnvironmentPolicyEvidence -> String
renderPlatformEnvironmentPolicyEvidence evidence =
  unlines $
    [ "mlf-platform-environment-policy-evidence-v1",
      "ambient-input-policy: " ++ environmentPolicyEvidenceAmbientPolicyName evidence,
      "ambient-inputs:"
    ]
      ++ renderIndentedItems (map renderAmbientInputEvidence (sortOn ambientEvidenceKey (environmentPolicyEvidenceAmbientInputs evidence)))
      ++ [ "loader-environment-policy: " ++ environmentPolicyEvidenceLoaderPolicyName evidence,
           "loader-environment:"
         ]
      ++ renderIndentedItems (map renderLoaderEnvironmentEvidence (sortOn loaderEvidenceKey (environmentPolicyEvidenceLoaderVariables evidence)))

renderPlatformEnvironmentPolicyViolation :: EnvironmentPolicyViolation -> String
renderPlatformEnvironmentPolicyViolation violation =
  case violation of
    DuplicateAmbientInputRule name ->
      "duplicate ambient-input rule: " ++ renderNamedValue name
    DuplicateLoaderEnvironmentRule variable ->
      "duplicate loader-environment rule: " ++ renderNamedValue variable
    UndeclaredAmbientInput name ->
      "observed proof-affecting ambient input has no rule: " ++ renderNamedValue name
    UndeclaredLoaderEnvironmentVariable variable ->
      "observed loader-affecting environment variable has no rule: " ++ renderNamedValue variable
    ScrubbedAmbientInputObserved name ->
      "ambient input " ++ renderNamedValue name ++ " is scrubbed but observed as present"
    ScrubbedLoaderEnvironmentVariableObserved variable ->
      "loader environment variable " ++ renderNamedValue variable ++ " is scrubbed but observed as present"
    AmbientInputDeclaredValueMismatch name expected observed ->
      "ambient input " ++ renderNamedValue name ++ " declared value mismatch: expected " ++ expected ++ " observed " ++ observed
    LoaderEnvironmentDeclaredValueMismatch variable expected observed ->
      "loader environment variable " ++ renderNamedValue variable ++ " declared value mismatch: expected " ++ expected ++ " observed " ++ observed
    AmbientInputNormalizedValueMismatch name expected observed ->
      "ambient input " ++ renderNamedValue name ++ " normalized value mismatch: expected " ++ expected ++ " observed " ++ observed
    LoaderEnvironmentNormalizedValueMismatch variable expected observed ->
      "loader environment variable " ++ renderNamedValue variable ++ " normalized value mismatch: expected " ++ expected ++ " observed " ++ observed
    MissingDeclaredAmbientInput name ->
      "ambient input " ++ renderNamedValue name ++ " is declared but missing from the snapshot"
    MissingDeclaredLoaderEnvironmentVariable variable ->
      "loader environment variable " ++ renderNamedValue variable ++ " is declared but missing from the snapshot"
    MissingNormalizedAmbientInput name ->
      "ambient input " ++ renderNamedValue name ++ " is normalized but missing from the snapshot"
    MissingNormalizedLoaderEnvironmentVariable variable ->
      "loader environment variable " ++ renderNamedValue variable ++ " is normalized but missing from the snapshot"
    BlankAmbientInputName ->
      "ambient input name is blank"
    BlankLoaderEnvironmentVariable ->
      "loader environment variable name is blank"
    BlankAmbientInputNormalizedValue name ->
      "ambient input " ++ renderNamedValue name ++ " has blank normalized value"
    BlankLoaderEnvironmentNormalizedValue variable ->
      "loader environment variable " ++ renderNamedValue variable ++ " has blank normalized value"

renderPlatformEnvironmentPolicyViolations :: [EnvironmentPolicyViolation] -> String
renderPlatformEnvironmentPolicyViolations =
  unlines . map renderPlatformEnvironmentPolicyViolation . sort

environmentPolicyEvidence :: EnvironmentPolicySnapshot -> EnvironmentPolicyEvidence
environmentPolicyEvidence snapshot =
  EnvironmentPolicyEvidence
    { environmentPolicyEvidenceAmbientPolicyName =
        ambientInputPolicyName (environmentPolicySnapshotAmbientPolicy snapshot),
      environmentPolicyEvidenceAmbientInputs =
        map (ambientRuleEvidence (environmentPolicySnapshotAmbientInputs snapshot)) $
          ambientInputPolicyRules (environmentPolicySnapshotAmbientPolicy snapshot),
      environmentPolicyEvidenceLoaderPolicyName =
        loaderEnvironmentPolicyName (environmentPolicySnapshotLoaderPolicy snapshot),
      environmentPolicyEvidenceLoaderVariables =
        map (loaderRuleEvidence (environmentPolicySnapshotLoaderEnvironment snapshot)) $
          loaderEnvironmentPolicyRules (environmentPolicySnapshotLoaderPolicy snapshot)
    }

ambientRuleEvidence :: [ObservedAmbientInput] -> AmbientInputRule -> (AmbientInputName, AmbientInputDisposition, Maybe String)
ambientRuleEvidence observed rule =
  ( ambientInputRuleName rule,
    ambientInputRuleDisposition rule,
    observedAmbientInputValue =<< findObservedAmbientInput (ambientInputRuleName rule) observed
  )

loaderRuleEvidence ::
  [ObservedLoaderEnvironmentVariable] ->
  LoaderEnvironmentRule ->
  (LoaderEnvironmentVariable, LoaderEnvironmentDisposition, Maybe String)
loaderRuleEvidence observed rule =
  ( loaderEnvironmentRuleVariable rule,
    loaderEnvironmentRuleDisposition rule,
    observedLoaderEnvironmentValue =<< findObservedLoaderEnvironmentVariable (loaderEnvironmentRuleVariable rule) observed
  )

validateAmbientRules :: [AmbientInputRule] -> [EnvironmentPolicyViolation]
validateAmbientRules rules =
  concatMap validateAmbientRule rules
    ++ map DuplicateAmbientInputRule (duplicates (map ambientInputRuleKey rules))

validateAmbientRule :: AmbientInputRule -> [EnvironmentPolicyViolation]
validateAmbientRule rule =
  [BlankAmbientInputName | isBlank name]
    ++ case ambientInputRuleDisposition rule of
      AmbientInputNormalized value
        | isBlank value -> [BlankAmbientInputNormalizedValue name]
      _ -> []
  where
    name = ambientInputRuleKey rule

validateLoaderRules :: [LoaderEnvironmentRule] -> [EnvironmentPolicyViolation]
validateLoaderRules rules =
  concatMap validateLoaderRule rules
    ++ map DuplicateLoaderEnvironmentRule (duplicates (map loaderEnvironmentRuleKey rules))

validateLoaderRule :: LoaderEnvironmentRule -> [EnvironmentPolicyViolation]
validateLoaderRule rule =
  [BlankLoaderEnvironmentVariable | isBlank variable]
    ++ case loaderEnvironmentRuleDisposition rule of
      LoaderEnvironmentNormalized value
        | isBlank value -> [BlankLoaderEnvironmentNormalizedValue variable]
      _ -> []
  where
    variable = loaderEnvironmentRuleKey rule

validateObservedAmbientInputNames :: [ObservedAmbientInput] -> [EnvironmentPolicyViolation]
validateObservedAmbientInputNames =
  concatMap $ \observed ->
    [BlankAmbientInputName | isBlank (observedAmbientInputKey observed)]

validateObservedLoaderEnvironmentNames :: [ObservedLoaderEnvironmentVariable] -> [EnvironmentPolicyViolation]
validateObservedLoaderEnvironmentNames =
  concatMap $ \observed ->
    [BlankLoaderEnvironmentVariable | isBlank (observedLoaderEnvironmentKey observed)]

validateUndeclaredAmbientInputs :: [AmbientInputRule] -> [ObservedAmbientInput] -> [EnvironmentPolicyViolation]
validateUndeclaredAmbientInputs rules =
  map (UndeclaredAmbientInput . observedAmbientInputKey)
    . filter (not . hasAmbientInputRule rules . observedAmbientInputName)

validateUndeclaredLoaderEnvironment ::
  [LoaderEnvironmentRule] ->
  [ObservedLoaderEnvironmentVariable] ->
  [EnvironmentPolicyViolation]
validateUndeclaredLoaderEnvironment rules =
  map (UndeclaredLoaderEnvironmentVariable . observedLoaderEnvironmentKey)
    . filter (not . hasLoaderEnvironmentRule rules . observedLoaderEnvironmentVariable)

validateAmbientRuleObservation :: [ObservedAmbientInput] -> AmbientInputRule -> [EnvironmentPolicyViolation]
validateAmbientRuleObservation observed rule =
  case ambientInputRuleDisposition rule of
    AmbientInputScrubbed ->
      case presentValue of
        Just _ -> [ScrubbedAmbientInputObserved name]
        Nothing -> []
    AmbientInputDeclared expected ->
      validateDeclaredValue MissingDeclaredAmbientInput AmbientInputDeclaredValueMismatch name expected presentValue
    AmbientInputNormalized expected ->
      validateDeclaredValue MissingNormalizedAmbientInput AmbientInputNormalizedValueMismatch name expected presentValue
  where
    name = ambientInputRuleKey rule
    presentValue =
      observedAmbientInputValue =<< findObservedAmbientInput (ambientInputRuleName rule) observed

validateLoaderRuleObservation :: [ObservedLoaderEnvironmentVariable] -> LoaderEnvironmentRule -> [EnvironmentPolicyViolation]
validateLoaderRuleObservation observed rule =
  case loaderEnvironmentRuleDisposition rule of
    LoaderEnvironmentScrubbed ->
      case presentValue of
        Just _ -> [ScrubbedLoaderEnvironmentVariableObserved variable]
        Nothing -> []
    LoaderEnvironmentDeclared expected ->
      validateDeclaredValue MissingDeclaredLoaderEnvironmentVariable LoaderEnvironmentDeclaredValueMismatch variable expected presentValue
    LoaderEnvironmentNormalized expected ->
      validateDeclaredValue MissingNormalizedLoaderEnvironmentVariable LoaderEnvironmentNormalizedValueMismatch variable expected presentValue
  where
    variable = loaderEnvironmentRuleKey rule
    presentValue =
      observedLoaderEnvironmentValue =<< findObservedLoaderEnvironmentVariable (loaderEnvironmentRuleVariable rule) observed

validateDeclaredValue :: (String -> EnvironmentPolicyViolation) -> (String -> String -> String -> EnvironmentPolicyViolation) -> String -> String -> Maybe String -> [EnvironmentPolicyViolation]
validateDeclaredValue missingViolation mismatchViolation name expected presentValue =
  case presentValue of
    Nothing -> [missingViolation name]
    Just observed
      | observed == expected -> []
      | otherwise -> [mismatchViolation name expected observed]

findObservedAmbientInput :: AmbientInputName -> [ObservedAmbientInput] -> Maybe ObservedAmbientInput
findObservedAmbientInput name =
  find ((== name) . observedAmbientInputName)

findObservedLoaderEnvironmentVariable ::
  LoaderEnvironmentVariable ->
  [ObservedLoaderEnvironmentVariable] ->
  Maybe ObservedLoaderEnvironmentVariable
findObservedLoaderEnvironmentVariable variable =
  find ((== variable) . observedLoaderEnvironmentVariable)

hasAmbientInputRule :: [AmbientInputRule] -> AmbientInputName -> Bool
hasAmbientInputRule rules name =
  any ((== name) . ambientInputRuleName) rules

hasLoaderEnvironmentRule :: [LoaderEnvironmentRule] -> LoaderEnvironmentVariable -> Bool
hasLoaderEnvironmentRule rules variable =
  any ((== variable) . loaderEnvironmentRuleVariable) rules

renderAmbientInputEvidence :: (AmbientInputName, AmbientInputDisposition, Maybe String) -> String
renderAmbientInputEvidence (name, disposition, observedValue) =
  concat
    [ "name=",
      unAmbientInputName name,
      " ",
      renderAmbientInputDisposition disposition,
      " observed=",
      renderObservedValue observedValue
    ]

renderLoaderEnvironmentEvidence :: (LoaderEnvironmentVariable, LoaderEnvironmentDisposition, Maybe String) -> String
renderLoaderEnvironmentEvidence (variable, disposition, observedValue) =
  concat
    [ "variable=",
      unLoaderEnvironmentVariable variable,
      " ",
      renderLoaderEnvironmentDisposition disposition,
      " observed=",
      renderObservedValue observedValue
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

renderLoaderEnvironmentDisposition :: LoaderEnvironmentDisposition -> String
renderLoaderEnvironmentDisposition disposition =
  case disposition of
    LoaderEnvironmentScrubbed ->
      "disposition=scrubbed"
    LoaderEnvironmentDeclared value ->
      "disposition=declared value=" ++ value
    LoaderEnvironmentNormalized value ->
      "disposition=normalized value=" ++ value

renderObservedValue :: Maybe String -> String
renderObservedValue value =
  case value of
    Nothing -> "<absent>"
    Just present -> present

renderNamedValue :: String -> String
renderNamedValue value
  | isBlank value = "<blank>"
  | otherwise = value

renderIndentedItems :: [String] -> [String]
renderIndentedItems values =
  case values of
    [] -> ["  - <none>"]
    _ -> map ("  - " ++) values

ambientEvidenceKey :: (AmbientInputName, AmbientInputDisposition, Maybe String) -> String
ambientEvidenceKey (name, _, _) =
  unAmbientInputName name

loaderEvidenceKey :: (LoaderEnvironmentVariable, LoaderEnvironmentDisposition, Maybe String) -> String
loaderEvidenceKey (variable, _, _) =
  unLoaderEnvironmentVariable variable

ambientInputRuleKey :: AmbientInputRule -> String
ambientInputRuleKey rule =
  unAmbientInputName (ambientInputRuleName rule)

loaderEnvironmentRuleKey :: LoaderEnvironmentRule -> String
loaderEnvironmentRuleKey rule =
  unLoaderEnvironmentVariable (loaderEnvironmentRuleVariable rule)

observedAmbientInputKey :: ObservedAmbientInput -> String
observedAmbientInputKey observed =
  unAmbientInputName (observedAmbientInputName observed)

observedLoaderEnvironmentKey :: ObservedLoaderEnvironmentVariable -> String
observedLoaderEnvironmentKey observed =
  unLoaderEnvironmentVariable (observedLoaderEnvironmentVariable observed)

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
