module PlatformToolchainIdentitySpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import MLF.Platform.Contract
import MLF.Platform.ToolchainIdentity
import Test.Hspec

spec :: Spec
spec = describe "MLF.Platform toolchain identity" $ do
  it "renders host toolchain identity evidence deterministically" $ do
    expected <- readFile "test/golden/platform-contract/host-toolchain-identity.txt"
    evidence <- expectEvidence validSnapshot
    reorderedEvidence <- expectEvidence reorderedValidSnapshot
    renderPlatformToolchainIdentityEvidence evidence `shouldBe` expected
    renderPlatformToolchainIdentityEvidence reorderedEvidence `shouldBe` expected

  it "accepts matched unavailable host toolchain identities" $ do
    evidence <- expectEvidence unavailableToolSnapshot
    renderPlatformToolchainIdentityEvidence evidence
      `shouldSatisfy` isInfixOf "role=optional-assembler availability=unavailable"
    renderPlatformToolchainIdentityEvidence evidence
      `shouldSatisfy` isInfixOf "unavailable-reason=not-required-for-this-target"

  it "rejects toolchain identity drift with named diagnostics" $ do
    forM_ driftViolationCases $ \(_, snapshot, expectedDiagnostic) -> do
      diagnostics <- expectViolations snapshot
      diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

  it "rejects duplicate toolchain identity declarations and observations" $ do
    forM_ duplicateViolationCases $ \(_, snapshot, expectedDiagnostic) -> do
      diagnostics <- expectViolations snapshot
      diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

  it "keeps toolchain identity validation pure over explicit snapshots" $ do
    let first = validatePlatformToolchainIdentity validSnapshot
        second = validatePlatformToolchainIdentity validSnapshot
    first `shouldBe` second

validSnapshot :: ToolchainIdentitySnapshot
validSnapshot =
  ToolchainIdentitySnapshot
    { toolchainIdentitySnapshotTargetTriple = targetTriple,
      toolchainIdentitySnapshotContract = validContract,
      toolchainIdentitySnapshotObservation = validObservation
    }

reorderedValidSnapshot :: ToolchainIdentitySnapshot
reorderedValidSnapshot =
  validSnapshot
    { toolchainIdentitySnapshotContract =
        validContract
          { hostToolchainTools = reverse (hostToolchainTools validContract),
            hostToolchainSystemLibraries = reverse (hostToolchainSystemLibraries validContract),
            hostToolchainCodegenSettings = reverse (hostToolchainCodegenSettings validContract)
          },
      toolchainIdentitySnapshotObservation =
        validObservation
          { observedToolchainTools = reverse (observedToolchainTools validObservation),
            observedToolchainSystemLibraries = reverse (observedToolchainSystemLibraries validObservation),
            observedToolchainCodegenSettings = reverse (observedToolchainCodegenSettings validObservation)
          }
    }

unavailableToolSnapshot :: ToolchainIdentitySnapshot
unavailableToolSnapshot =
  validSnapshot
    { toolchainIdentitySnapshotContract =
        validContract
          { hostToolchainTools =
              [ ResolvedToolIdentity
                  (ToolchainToolRole "optional-assembler")
                  Nothing
                  Nothing
                  (Just "not-required-for-this-target")
                  Nothing
              ]
          },
      toolchainIdentitySnapshotObservation =
        validObservation
          { observedToolchainTools =
              [ ObservedToolIdentity
                  (ToolchainToolRole "optional-assembler")
                  Nothing
                  Nothing
                  (Just "not-required-for-this-target")
                  Nothing
                  False
              ]
          }
    }

targetTriple :: TargetTriple
targetTriple =
  TargetTriple "x86_64-unknown-linux-gnu"

validContract :: HostToolchainContract
validContract =
  HostToolchainContract
    { hostToolchainTools =
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
      hostToolchainSysrootIdentity =
        Just (ToolchainSysrootAvailable "sysroot:glibc-2.39-x86_64"),
      hostToolchainSystemLibraries =
        [ ToolchainSystemLibraryIdentity "libc" "sha256:5555555555555555555555555555555555555555555555555555555555555555",
          ToolchainSystemLibraryIdentity "libm" "sha256:6666666666666666666666666666666666666666666666666666666666666666"
        ],
      hostToolchainCodegenSettings =
        [ ToolchainCodegenSetting "cpu" "x86-64-v2",
          ToolchainCodegenSetting "opt-level" "2"
        ],
      hostToolchainLinkerMode =
        Just (ToolchainLinkerMode "dynamic")
    }

validObservation :: ObservedToolchainIdentity
validObservation =
  ObservedToolchainIdentity
    { observedToolchainTargetTriple = targetTriple,
      observedToolchainTools =
        [ ObservedToolIdentity
            { observedToolRole = ToolchainToolRole "c-compiler",
              observedToolPath = Just "/opt/mlf/toolchains/clang-18/bin/clang",
              observedToolDigest = Just "sha256:3333333333333333333333333333333333333333333333333333333333333333",
              observedToolUnavailableReason = Nothing,
              observedToolVersion = Just "clang-18.1.8",
              observedToolAvailable = True
            },
          ObservedToolIdentity
            { observedToolRole = ToolchainToolRole "archive-tool",
              observedToolPath = Just "/opt/mlf/toolchains/llvm-18/bin/llvm-ar",
              observedToolDigest = Just "sha256:4444444444444444444444444444444444444444444444444444444444444444",
              observedToolUnavailableReason = Nothing,
              observedToolVersion = Just "llvm-ar-18.1.8",
              observedToolAvailable = True
            }
        ],
      observedToolchainSysrootIdentity =
        Just (ToolchainSysrootAvailable "sysroot:glibc-2.39-x86_64"),
      observedToolchainSystemLibraries =
        [ ObservedToolchainSystemLibrary "libc" "sha256:5555555555555555555555555555555555555555555555555555555555555555",
          ObservedToolchainSystemLibrary "libm" "sha256:6666666666666666666666666666666666666666666666666666666666666666"
        ],
      observedToolchainCodegenSettings =
        [ ToolchainCodegenSetting "cpu" "x86-64-v2",
          ToolchainCodegenSetting "opt-level" "2"
        ],
      observedToolchainLinkerMode =
        Just (ToolchainLinkerMode "dynamic")
    }

driftViolationCases :: [(String, ToolchainIdentitySnapshot, String)]
driftViolationCases =
  [ ( "target triple",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainTargetTriple = TargetTriple "aarch64-apple-darwin"}
        },
      "target triple mismatch"
    ),
    ( "required tool",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainTools = [archiveToolObservation]}
        },
      "declared required tool role missing from observation: c-compiler"
    ),
    ( "undeclared observed tool",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainTools =
                  ObservedToolIdentity (ToolchainToolRole "assembler") (Just "/tool/as") (Just "sha256:1") Nothing (Just "as-1") True
                    : observedToolchainTools validObservation
              }
        },
      "observed tool role has no declared tool contract: assembler"
    ),
    ( "tool path",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainTools =
                  replaceToolObservation (ToolchainToolRole "c-compiler") cCompilerObservation {observedToolPath = Just "/other/clang"}
              }
        },
      "tool c-compiler path mismatch"
    ),
    ( "tool digest",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainTools =
                  replaceToolObservation (ToolchainToolRole "c-compiler") cCompilerObservation {observedToolDigest = Just "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
              }
        },
      "tool c-compiler digest mismatch"
    ),
    ( "tool unavailable reason",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract
              { hostToolchainTools =
                  [ResolvedToolIdentity (ToolchainToolRole "optional-assembler") Nothing Nothing (Just "not-required") Nothing]
              },
          toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainTools =
                  [ObservedToolIdentity (ToolchainToolRole "optional-assembler") Nothing Nothing (Just "not-installed") Nothing False]
              }
        },
      "tool optional-assembler unavailable reason mismatch"
    ),
    ( "tool version",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainTools =
                  replaceToolObservation (ToolchainToolRole "c-compiler") cCompilerObservation {observedToolVersion = Just "clang-18.1.9"}
              }
        },
      "tool c-compiler version mismatch"
    ),
    ( "version string alone",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract
              { hostToolchainTools =
                  [ResolvedToolIdentity (ToolchainToolRole "c-compiler") Nothing Nothing Nothing (Just "clang-18.1.8")]
              }
        },
      "declared tool c-compiler uses a version string alone as proof identity"
    ),
    ( "observed version string alone",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainTools =
                  replaceToolObservation (ToolchainToolRole "c-compiler") cCompilerObservation {observedToolPath = Nothing, observedToolDigest = Nothing}
              }
        },
      "observed tool c-compiler uses a version string alone as proof identity"
    ),
    ( "sysroot",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainSysrootIdentity = Nothing}
        },
      "declared sysroot identity is missing from observations"
    ),
    ( "sysroot",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainSysrootIdentity = Just (ToolchainSysrootAvailable "sysroot:macosx-15.4")}
        },
      "sysroot identity mismatch"
    ),
    ( "system library",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainSystemLibraries = [ObservedToolchainSystemLibrary "libm" "sha256:6666666666666666666666666666666666666666666666666666666666666666"]}
        },
      "declared system library identity is missing from observations: libc"
    ),
    ( "system library",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainSystemLibraries =
                  [ ObservedToolchainSystemLibrary "libc" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                    ObservedToolchainSystemLibrary "libm" "sha256:6666666666666666666666666666666666666666666666666666666666666666"
                  ]
              }
        },
      "system library libc identity mismatch"
    ),
    ( "codegen setting",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainCodegenSettings = [ToolchainCodegenSetting "cpu" "x86-64-v2"]}
        },
      "declared codegen setting is missing from observations: opt-level"
    ),
    ( "codegen setting",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainCodegenSettings =
                  [ ToolchainCodegenSetting "cpu" "x86-64-v2",
                    ToolchainCodegenSetting "opt-level" "3"
                  ]
              }
        },
      "codegen setting opt-level mismatch"
    ),
    ( "linker mode",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainLinkerMode = Nothing}
        },
      "declared linker mode is missing from observations"
    ),
    ( "linker mode",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainLinkerMode = Just (ToolchainLinkerMode "static")}
        },
      "linker mode mismatch"
    )
  ]

duplicateViolationCases :: [(String, ToolchainIdentitySnapshot, String)]
duplicateViolationCases =
  [ ( "duplicate declared tool role",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract {hostToolchainTools = cCompilerTool : hostToolchainTools validContract}
        },
      "duplicate declared tool role: c-compiler"
    ),
    ( "duplicate declared system library",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract
              { hostToolchainSystemLibraries =
                  ToolchainSystemLibraryIdentity "libc" "sha256:1" : hostToolchainSystemLibraries validContract
              }
        },
      "duplicate declared system library identity: libc"
    ),
    ( "duplicate declared codegen setting",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract {hostToolchainCodegenSettings = ToolchainCodegenSetting "cpu" "generic" : hostToolchainCodegenSettings validContract}
        },
      "duplicate declared codegen setting: cpu"
    ),
    ( "duplicate observed tool role",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainTools = cCompilerObservation : observedToolchainTools validObservation}
        },
      "duplicate observed tool role: c-compiler"
    ),
    ( "duplicate observed system library",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation
              { observedToolchainSystemLibraries =
                  ObservedToolchainSystemLibrary "libc" "sha256:1" : observedToolchainSystemLibraries validObservation
              }
        },
      "duplicate observed system library identity: libc"
    ),
    ( "duplicate observed codegen setting",
      validSnapshot
        { toolchainIdentitySnapshotObservation =
            validObservation {observedToolchainCodegenSettings = ToolchainCodegenSetting "cpu" "generic" : observedToolchainCodegenSettings validObservation}
        },
      "duplicate observed codegen setting: cpu"
    ),
    ( "missing linker mode",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract {hostToolchainLinkerMode = Nothing}
        },
      "declared host toolchain linker mode is missing"
    ),
    ( "blank linker mode",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract {hostToolchainLinkerMode = Just (ToolchainLinkerMode "")}
        },
      "declared host toolchain linker mode is blank"
    ),
    ( "blank sysroot",
      validSnapshot
        { toolchainIdentitySnapshotContract =
            validContract {hostToolchainSysrootIdentity = Just (ToolchainSysrootAvailable "")}
        },
      "declared host toolchain sysroot identity is blank"
    )
  ]

cCompilerTool :: ResolvedToolIdentity
cCompilerTool =
  ResolvedToolIdentity
    { resolvedToolRole = ToolchainToolRole "c-compiler",
      resolvedToolPath = Just "/opt/mlf/toolchains/clang-18/bin/clang",
      resolvedToolDigest = Just "sha256:3333333333333333333333333333333333333333333333333333333333333333",
      resolvedToolUnavailableReason = Nothing,
      resolvedToolVersion = Just "clang-18.1.8"
    }

cCompilerObservation :: ObservedToolIdentity
cCompilerObservation =
  ObservedToolIdentity
    { observedToolRole = ToolchainToolRole "c-compiler",
      observedToolPath = Just "/opt/mlf/toolchains/clang-18/bin/clang",
      observedToolDigest = Just "sha256:3333333333333333333333333333333333333333333333333333333333333333",
      observedToolUnavailableReason = Nothing,
      observedToolVersion = Just "clang-18.1.8",
      observedToolAvailable = True
    }

archiveToolObservation :: ObservedToolIdentity
archiveToolObservation =
  ObservedToolIdentity
    { observedToolRole = ToolchainToolRole "archive-tool",
      observedToolPath = Just "/opt/mlf/toolchains/llvm-18/bin/llvm-ar",
      observedToolDigest = Just "sha256:4444444444444444444444444444444444444444444444444444444444444444",
      observedToolUnavailableReason = Nothing,
      observedToolVersion = Just "llvm-ar-18.1.8",
      observedToolAvailable = True
    }

replaceToolObservation :: ToolchainToolRole -> ObservedToolIdentity -> [ObservedToolIdentity]
replaceToolObservation role replacement =
  map
    ( \observed ->
        if observedToolRole observed == role
          then replacement
          else observed
    )
    (observedToolchainTools validObservation)

expectEvidence :: ToolchainIdentitySnapshot -> IO ToolchainIdentityEvidence
expectEvidence snapshot =
  case validatePlatformToolchainIdentity snapshot of
    Right evidence -> pure evidence
    Left violations -> do
      expectationFailure ("expected valid toolchain identity snapshot, got: " ++ renderPlatformToolchainIdentityViolations violations)
      pure (ToolchainIdentityEvidence (TargetTriple "") [] Nothing [] [] Nothing)

expectViolations :: ToolchainIdentitySnapshot -> IO String
expectViolations snapshot =
  case validatePlatformToolchainIdentity snapshot of
    Left violations -> pure (renderPlatformToolchainIdentityViolations violations)
    Right evidence -> do
      expectationFailure ("expected toolchain identity violations, got: " ++ renderPlatformToolchainIdentityEvidence evidence)
      pure ""
