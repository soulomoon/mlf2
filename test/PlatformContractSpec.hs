module PlatformContractSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import MLF.Platform.Contract
import Test.Hspec

spec :: Spec
spec = describe "MLF.Platform contract substrate" $ do
  it "renders deterministic substrate contract declarations" $ do
    expected <- readFile "test/golden/platform-contract/minimal-substrate-contract.txt"
    renderPlatformSubstrateContract minimalContract `shouldBe` Right expected
    renderSubstrateFingerprintMaterial minimalContract
      `shouldBe` renderSubstrateFingerprintMaterial reorderedMinimalContract

  it "changes substrate fingerprint material when declared platform identity changes" $ do
    base <- expectFingerprint minimalContract
    forM_ fingerprintDriftCases $ \(name, contract) -> do
      changed <- expectFingerprint contract
      changed `shouldNotBe` base
      changed `shouldSatisfy` isInfixOf name

  it "rejects invalid platform substrate contract declarations with named diagnostics" $ do
    forM_ invalidContractCases $ \(name, contract, expectedDiagnostic) -> do
      let renderedDiagnostics =
            renderPlatformContractErrors (validatePlatformSubstrateContract contract)
      renderedDiagnostics `shouldSatisfy` isInfixOf expectedDiagnostic
      renderedDiagnostics `shouldSatisfy` isInfixOf name

  it "keeps substrate rendering pure over explicit contract declarations" $ do
    let first = renderSubstrateFingerprintMaterial minimalContract
        second = renderSubstrateFingerprintMaterial minimalContract
    first `shouldBe` second

minimalContract :: PlatformSubstrateContract
minimalContract =
  PlatformSubstrateContract
    { platformContractAbiVersion = Just (PlatformAbiVersion "mlf-platform-abi-1"),
      platformContractPackageId = Just (PlatformSubstrateContractPackageId "mlf.platform.substrate.minimal"),
      platformContractPackageVersion = Just (PlatformSubstrateContractPackageVersion "0.1.0"),
      platformContractTargetTriple = Just (TargetTriple "x86_64-unknown-linux-gnu"),
      platformContractSubstrateComponents =
        [ SubstrateComponent
            { substrateComponentKind = SubstrateComponentKind "runtime",
              substrateComponentName = SubstrateComponentName "mlfp-io",
              substrateComponentDigest = SubstrateComponentDigest "sha256:1111111111111111111111111111111111111111111111111111111111111111"
            },
          SubstrateComponent
            { substrateComponentKind = SubstrateComponentKind "builtin-source",
              substrateComponentName = SubstrateComponentName "prelude",
              substrateComponentDigest = SubstrateComponentDigest "sha256:2222222222222222222222222222222222222222222222222222222222222222"
            }
        ],
      platformContractHostToolchain =
        HostToolchainContract
          [ ResolvedToolIdentity
              { resolvedToolRole = ToolchainToolRole "c-compiler",
                resolvedToolPath = Just "/opt/mlf/toolchains/clang-18/bin/clang",
                resolvedToolDigest = Just "sha256:3333333333333333333333333333333333333333333333333333333333333333",
                resolvedToolUnavailableReason = Nothing,
                resolvedToolVersion = Just "clang-18.1.8"
              },
            ResolvedToolIdentity
              { resolvedToolRole = ToolchainToolRole "archive-tool",
                resolvedToolPath = Just "/opt/mlf/toolchains/llvm-18/bin/llvm-ar",
                resolvedToolDigest = Just "sha256:4444444444444444444444444444444444444444444444444444444444444444",
                resolvedToolUnavailableReason = Nothing,
                resolvedToolVersion = Just "llvm-ar-18.1.8"
              }
          ],
      platformContractAmbientInputPolicy =
        Just
          AmbientInputPolicy
            { ambientInputPolicyName = "explicit-empty-ambient-inputs",
              ambientInputPolicyInputs = ["argv", "stdin"]
            },
      platformContractLoaderEnvironmentPolicy =
        Just
          LoaderEnvironmentPolicy
            { loaderEnvironmentPolicyName = "scrubbed-loader-environment",
              loaderEnvironmentPolicyVariables =
                [ "DYLD_LIBRARY_PATH=cleared",
                  "LD_LIBRARY_PATH=cleared"
                ]
            }
    }

reorderedMinimalContract :: PlatformSubstrateContract
reorderedMinimalContract =
  minimalContract
    { platformContractSubstrateComponents = reverse (platformContractSubstrateComponents minimalContract),
      platformContractHostToolchain =
        HostToolchainContract (reverse (hostToolchainTools (platformContractHostToolchain minimalContract)))
    }

fingerprintDriftCases :: [(String, PlatformSubstrateContract)]
fingerprintDriftCases =
  [ ("mlf-platform-abi-2", minimalContract {platformContractAbiVersion = Just (PlatformAbiVersion "mlf-platform-abi-2")}),
    ("aarch64-apple-darwin", minimalContract {platformContractTargetTriple = Just (TargetTriple "aarch64-apple-darwin")}),
    ( "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      minimalContract
        { platformContractSubstrateComponents =
            [ SubstrateComponent
                { substrateComponentKind = SubstrateComponentKind "runtime",
                  substrateComponentName = SubstrateComponentName "mlfp-io",
                  substrateComponentDigest = SubstrateComponentDigest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                },
              SubstrateComponent
                { substrateComponentKind = SubstrateComponentKind "builtin-source",
                  substrateComponentName = SubstrateComponentName "prelude",
                  substrateComponentDigest = SubstrateComponentDigest "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                }
            ]
        }
    ),
    ( "/nix/store/mlf-clang/bin/clang",
      minimalContract
        { platformContractHostToolchain =
            HostToolchainContract
              [ ResolvedToolIdentity
                  { resolvedToolRole = ToolchainToolRole "c-compiler",
                    resolvedToolPath = Just "/nix/store/mlf-clang/bin/clang",
                    resolvedToolDigest = Just "sha256:3333333333333333333333333333333333333333333333333333333333333333",
                    resolvedToolUnavailableReason = Nothing,
                    resolvedToolVersion = Just "clang-18.1.8"
                  },
                ResolvedToolIdentity
                  { resolvedToolRole = ToolchainToolRole "archive-tool",
                    resolvedToolPath = Just "/opt/mlf/toolchains/llvm-18/bin/llvm-ar",
                    resolvedToolDigest = Just "sha256:4444444444444444444444444444444444444444444444444444444444444444",
                    resolvedToolUnavailableReason = Nothing,
                    resolvedToolVersion = Just "llvm-ar-18.1.8"
                  }
              ]
        }
    ),
    ( "explicit-file-inputs",
      minimalContract
        { platformContractAmbientInputPolicy =
            Just
              AmbientInputPolicy
                { ambientInputPolicyName = "explicit-file-inputs",
                  ambientInputPolicyInputs = ["argv", "source-root", "stdin"]
                }
        }
    ),
    ( "preserved-loader-environment",
      minimalContract
        { platformContractLoaderEnvironmentPolicy =
            Just
              LoaderEnvironmentPolicy
                { loaderEnvironmentPolicyName = "preserved-loader-environment",
                  loaderEnvironmentPolicyVariables = ["LD_LIBRARY_PATH=declared"]
                }
        }
    )
  ]

invalidContractCases :: [(String, PlatformSubstrateContract, String)]
invalidContractCases =
  [ ("platform ABI version", minimalContract {platformContractAbiVersion = Nothing}, "platform ABI version is missing"),
    ("platform ABI version", minimalContract {platformContractAbiVersion = Just (PlatformAbiVersion "")}, "platform ABI version is empty"),
    ("platform substrate contract package id", minimalContract {platformContractPackageId = Nothing}, "platform substrate contract package id is missing"),
    ("platform substrate contract package id", minimalContract {platformContractPackageId = Just (PlatformSubstrateContractPackageId "")}, "platform substrate contract package id is empty"),
    ("platform substrate contract package version", minimalContract {platformContractPackageVersion = Nothing}, "platform substrate contract package version is missing"),
    ( "platform substrate contract package version",
      minimalContract {platformContractPackageVersion = Just (PlatformSubstrateContractPackageVersion "")},
      "platform substrate contract package version is empty"
    ),
    ("target triple", minimalContract {platformContractTargetTriple = Nothing}, "target triple is missing"),
    ("target triple", minimalContract {platformContractTargetTriple = Just (TargetTriple "")}, "target triple is empty"),
    ( "substrate component key",
      minimalContract
        { platformContractSubstrateComponents =
            [ SubstrateComponent (SubstrateComponentKind "runtime") (SubstrateComponentName "mlfp-io") (SubstrateComponentDigest "sha256:1"),
              SubstrateComponent (SubstrateComponentKind "runtime") (SubstrateComponentName "mlfp-io") (SubstrateComponentDigest "sha256:2")
            ]
        },
      "duplicate substrate component key"
    ),
    ( "host toolchain role",
      minimalContract
        { platformContractHostToolchain =
            HostToolchainContract
              [ ResolvedToolIdentity (ToolchainToolRole "c-compiler") (Just "/tool/clang") (Just "sha256:1") Nothing (Just "clang"),
                ResolvedToolIdentity (ToolchainToolRole "c-compiler") (Just "/other/clang") (Just "sha256:2") Nothing (Just "clang")
              ]
        },
      "duplicate host toolchain role"
    ),
    ( "host toolchain identity",
      minimalContract
        { platformContractHostToolchain =
            HostToolchainContract
              [ ResolvedToolIdentity (ToolchainToolRole "c-compiler") Nothing Nothing Nothing (Just "clang-18.1.8")
              ]
        },
      "version string alone is not accepted"
    ),
    ("ambient-input policy", minimalContract {platformContractAmbientInputPolicy = Nothing}, "ambient-input policy is missing"),
    ("loader-environment policy", minimalContract {platformContractLoaderEnvironmentPolicy = Nothing}, "loader-environment policy is missing")
  ]

expectFingerprint :: PlatformSubstrateContract -> IO String
expectFingerprint contract =
  case renderSubstrateFingerprintMaterial contract of
    Right material -> pure material
    Left errors -> do
      expectationFailure ("expected valid platform contract, got: " ++ renderPlatformContractErrors errors)
      pure ""
