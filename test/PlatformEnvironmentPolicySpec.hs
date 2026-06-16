module PlatformEnvironmentPolicySpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
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
import MLF.Platform.EnvironmentPolicy
import Test.Hspec

spec :: Spec
spec = describe "MLF.Platform environment policy" $ do
  it "renders normalized ambient and loader policy evidence deterministically" $ do
    expected <- readFile "test/golden/platform-contract/normalized-environment-policy.txt"
    evidence <- expectEvidence validSnapshot
    reorderedEvidence <- expectEvidence reorderedValidSnapshot
    renderPlatformEnvironmentPolicyEvidence evidence `shouldBe` expected
    renderPlatformEnvironmentPolicyEvidence reorderedEvidence `shouldBe` expected

  it "rejects undeclared ambient and loader inputs with distinct diagnostics" $ do
    let snapshot =
          validSnapshot
            { environmentPolicySnapshotAmbientInputs =
                ObservedAmbientInput (AmbientInputName "wall-clock-time") (Just "fixed-example")
                  : environmentPolicySnapshotAmbientInputs validSnapshot,
              environmentPolicySnapshotLoaderEnvironment =
                ObservedLoaderEnvironmentVariable (LoaderEnvironmentVariable "DYLD_FALLBACK_LIBRARY_PATH") (Just "/tmp/lib")
                  : environmentPolicySnapshotLoaderEnvironment validSnapshot
            }
    diagnostics <- expectViolations snapshot
    diagnostics `shouldSatisfy` isInfixOf "observed proof-affecting ambient input has no rule: wall-clock-time"
    diagnostics `shouldSatisfy` isInfixOf "observed loader-affecting environment variable has no rule: DYLD_FALLBACK_LIBRARY_PATH"

  it "rejects duplicate rules, scrubbed observations, normalized mismatches, and blank policy fields" $ do
    forM_ requiredViolationCases $ \(_, snapshot, expectedDiagnostic) -> do
      diagnostics <- expectViolations snapshot
      diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

  it "keeps environment policy validation pure over explicit snapshots" $ do
    let first = validatePlatformEnvironmentPolicies validSnapshot
        second = validatePlatformEnvironmentPolicies validSnapshot
    first `shouldBe` second

validSnapshot :: EnvironmentPolicySnapshot
validSnapshot =
  EnvironmentPolicySnapshot
    { environmentPolicySnapshotAmbientPolicy = ambientPolicy,
      environmentPolicySnapshotLoaderPolicy = loaderPolicy,
      environmentPolicySnapshotAmbientInputs =
        [ ObservedAmbientInput (AmbientInputName "timezone") (Just "UTC"),
          ObservedAmbientInput (AmbientInputName "locale") (Just "C.UTF-8")
        ],
      environmentPolicySnapshotLoaderEnvironment =
        [ ObservedLoaderEnvironmentVariable (LoaderEnvironmentVariable "MLF_RUNTIME_LIBRARY") (Just "/opt/mlf/runtime/lib/libmlfp_io.so"),
          ObservedLoaderEnvironmentVariable (LoaderEnvironmentVariable "LD_LIBRARY_PATH") (Just "/opt/mlf/runtime/lib")
        ]
    }

reorderedValidSnapshot :: EnvironmentPolicySnapshot
reorderedValidSnapshot =
  validSnapshot
    { environmentPolicySnapshotAmbientPolicy =
        ambientPolicy {ambientInputPolicyRules = reverse (ambientInputPolicyRules ambientPolicy)},
      environmentPolicySnapshotLoaderPolicy =
        loaderPolicy {loaderEnvironmentPolicyRules = reverse (loaderEnvironmentPolicyRules loaderPolicy)},
      environmentPolicySnapshotAmbientInputs = reverse (environmentPolicySnapshotAmbientInputs validSnapshot),
      environmentPolicySnapshotLoaderEnvironment = reverse (environmentPolicySnapshotLoaderEnvironment validSnapshot)
    }

ambientPolicy :: AmbientInputPolicy
ambientPolicy =
  AmbientInputPolicy
    { ambientInputPolicyName = "explicit-proof-ambient-inputs",
      ambientInputPolicyRules =
        [ AmbientInputRule (AmbientInputName "timezone") (AmbientInputDeclared "UTC"),
          AmbientInputRule (AmbientInputName "argv") AmbientInputScrubbed,
          AmbientInputRule (AmbientInputName "locale") (AmbientInputNormalized "C.UTF-8")
        ]
    }

loaderPolicy :: LoaderEnvironmentPolicy
loaderPolicy =
  LoaderEnvironmentPolicy
    { loaderEnvironmentPolicyName = "explicit-loader-environment",
      loaderEnvironmentPolicyRules =
        [ LoaderEnvironmentRule (LoaderEnvironmentVariable "MLF_RUNTIME_LIBRARY") (LoaderEnvironmentDeclared "/opt/mlf/runtime/lib/libmlfp_io.so"),
          LoaderEnvironmentRule (LoaderEnvironmentVariable "DYLD_LIBRARY_PATH") LoaderEnvironmentScrubbed,
          LoaderEnvironmentRule (LoaderEnvironmentVariable "LD_LIBRARY_PATH") (LoaderEnvironmentNormalized "/opt/mlf/runtime/lib")
        ]
    }

requiredViolationCases :: [(String, EnvironmentPolicySnapshot, String)]
requiredViolationCases =
  [ ( "duplicate ambient-input rule",
      validSnapshot
        { environmentPolicySnapshotAmbientPolicy =
            ambientPolicy
              { ambientInputPolicyRules =
                  AmbientInputRule (AmbientInputName "locale") (AmbientInputNormalized "C.UTF-8")
                    : ambientInputPolicyRules ambientPolicy
              }
        },
      "duplicate ambient-input rule: locale"
    ),
    ( "duplicate loader-environment rule",
      validSnapshot
        { environmentPolicySnapshotLoaderPolicy =
            loaderPolicy
              { loaderEnvironmentPolicyRules =
                  LoaderEnvironmentRule (LoaderEnvironmentVariable "LD_LIBRARY_PATH") (LoaderEnvironmentNormalized "/opt/mlf/runtime/lib")
                    : loaderEnvironmentPolicyRules loaderPolicy
              }
        },
      "duplicate loader-environment rule: LD_LIBRARY_PATH"
    ),
    ( "scrubbed ambient input",
      validSnapshot
        { environmentPolicySnapshotAmbientInputs =
            ObservedAmbientInput (AmbientInputName "argv") (Just "--build")
              : environmentPolicySnapshotAmbientInputs validSnapshot
        },
      "ambient input argv is scrubbed but observed as present"
    ),
    ( "scrubbed loader environment variable",
      validSnapshot
        { environmentPolicySnapshotLoaderEnvironment =
            ObservedLoaderEnvironmentVariable (LoaderEnvironmentVariable "DYLD_LIBRARY_PATH") (Just "/tmp/dylib")
              : environmentPolicySnapshotLoaderEnvironment validSnapshot
        },
      "loader environment variable DYLD_LIBRARY_PATH is scrubbed but observed as present"
    ),
    ( "normalized ambient input",
      validSnapshot
        { environmentPolicySnapshotAmbientInputs =
            replaceAmbientObservation (AmbientInputName "locale") (Just "en_US.UTF-8") (environmentPolicySnapshotAmbientInputs validSnapshot)
        },
      "ambient input locale normalized value mismatch: expected C.UTF-8 observed en_US.UTF-8"
    ),
    ( "normalized loader environment variable",
      validSnapshot
        { environmentPolicySnapshotLoaderEnvironment =
            replaceLoaderObservation (LoaderEnvironmentVariable "LD_LIBRARY_PATH") (Just "/tmp/lib") (environmentPolicySnapshotLoaderEnvironment validSnapshot)
        },
      "loader environment variable LD_LIBRARY_PATH normalized value mismatch: expected /opt/mlf/runtime/lib observed /tmp/lib"
    ),
    ( "blank ambient input",
      validSnapshot
        { environmentPolicySnapshotAmbientPolicy =
            ambientPolicy {ambientInputPolicyRules = [AmbientInputRule (AmbientInputName "") AmbientInputScrubbed]}
        },
      "ambient input name is blank"
    ),
    ( "blank loader environment variable",
      validSnapshot
        { environmentPolicySnapshotLoaderPolicy =
            loaderPolicy {loaderEnvironmentPolicyRules = [LoaderEnvironmentRule (LoaderEnvironmentVariable "") LoaderEnvironmentScrubbed]}
        },
      "loader environment variable name is blank"
    ),
    ( "blank normalized ambient input",
      validSnapshot
        { environmentPolicySnapshotAmbientPolicy =
            ambientPolicy {ambientInputPolicyRules = [AmbientInputRule (AmbientInputName "locale") (AmbientInputNormalized "")]}
        },
      "ambient input locale has blank normalized value"
    ),
    ( "blank normalized loader environment variable",
      validSnapshot
        { environmentPolicySnapshotLoaderPolicy =
            loaderPolicy {loaderEnvironmentPolicyRules = [LoaderEnvironmentRule (LoaderEnvironmentVariable "LD_LIBRARY_PATH") (LoaderEnvironmentNormalized "")]}
        },
      "loader environment variable LD_LIBRARY_PATH has blank normalized value"
    )
  ]

replaceAmbientObservation :: AmbientInputName -> Maybe String -> [ObservedAmbientInput] -> [ObservedAmbientInput]
replaceAmbientObservation name value =
  map $ \observed ->
    if observedAmbientInputName observed == name
      then observed {observedAmbientInputValue = value}
      else observed

replaceLoaderObservation ::
  LoaderEnvironmentVariable ->
  Maybe String ->
  [ObservedLoaderEnvironmentVariable] ->
  [ObservedLoaderEnvironmentVariable]
replaceLoaderObservation variable value =
  map $ \observed ->
    if observedLoaderEnvironmentVariable observed == variable
      then observed {observedLoaderEnvironmentValue = value}
      else observed

expectEvidence :: EnvironmentPolicySnapshot -> IO EnvironmentPolicyEvidence
expectEvidence snapshot =
  case validatePlatformEnvironmentPolicies snapshot of
    Right evidence -> pure evidence
    Left violations -> do
      expectationFailure ("expected valid environment policy snapshot, got: " ++ renderPlatformEnvironmentPolicyViolations violations)
      pure (EnvironmentPolicyEvidence "" [] "" [])

expectViolations :: EnvironmentPolicySnapshot -> IO String
expectViolations snapshot =
  case validatePlatformEnvironmentPolicies snapshot of
    Left violations -> pure (renderPlatformEnvironmentPolicyViolations violations)
    Right evidence -> do
      expectationFailure ("expected environment policy violations, got: " ++ renderPlatformEnvironmentPolicyEvidence evidence)
      pure ""
