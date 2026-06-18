module PlatformNativeLinkRecordSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)

import MLF.Platform.Contract
    ( TargetTriple (..)
    , ToolchainLinkerMode (..)
    )
import MLF.Platform.NativeLinkRecord
import Test.Hspec

spec :: Spec
spec = describe "MLF.Platform native link record" $ do
    it "renders canonical native link records deterministically" $ do
        expected <- readFile "test/golden/platform-contract/native-link-record.txt"
        renderCanonicalSelfBootLinkRecord validRecord `shouldBe` Right expected
        renderCanonicalSelfBootLinkRecord reorderedValidRecord `shouldBe` Right expected

    it "rejects incomplete native link records with named diagnostics" $ do
        forM_ incompleteRecordCases $ \(_, record, expectedDiagnostic) -> do
            diagnostics <- expectViolations record
            diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

    it "rejects native link object and output paths outside the stage root" $ do
        diagnostics <-
            expectViolations
                validRecord
                    { canonicalSelfBootLinkRecordObjectInputs =
                        [NativeLinkObjectInput ".self-boot/stage1/obj/compiler-main.o" "sha256:object-stage1"]
                    , canonicalSelfBootLinkRecordOutputArtifact =
                        NativeLinkOutputArtifact (Just ".self-boot/stage1/bin/mlfp-stage1") (Just "sha256:linked-stage1")
                    }
        diagnostics
            `shouldSatisfy` isInfixOf "object input path outside the declared stage-owned output directory"
        diagnostics
            `shouldSatisfy` isInfixOf "output artifact path outside the declared stage-owned output directory"

    it "rejects resolved linked library identity drift and shape errors" $ do
        forM_ linkedLibraryViolationCases $ \(_, libraries, expectedDiagnostic) -> do
            diagnostics <-
                expectViolations
                    validRecord
                        { canonicalSelfBootLinkRecordResolvedLibraries = libraries
                        }
            diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

    it "keeps native link record validation pure over explicit records" $ do
        let first = validateCanonicalSelfBootLinkRecord validRecord
            second = validateCanonicalSelfBootLinkRecord validRecord
        first `shouldBe` second

validRecord :: CanonicalSelfBootLinkRecord
validRecord =
    CanonicalSelfBootLinkRecord
        { canonicalSelfBootLinkRecordProofActionId =
            Just (SelfBootProofActionId "stage0.link.compiler")
        , canonicalSelfBootLinkRecordLinkerArgv =
            [ "/opt/mlf/toolchains/clang-18/bin/clang"
            , "-target"
            , "x86_64-unknown-linux-gnu"
            , ".self-boot/stage0/obj/compiler-main.o"
            , ".self-boot/stage0/obj/compiler-runtime.o"
            , "-o"
            , ".self-boot/stage0/bin/mlfp-stage1"
            ]
        , canonicalSelfBootLinkRecordTargetTriple =
            Just targetTriple
        , canonicalSelfBootLinkRecordLinkerMode =
            Just linkerMode
        , canonicalSelfBootLinkRecordObjectInputs =
            objectInputs
        , canonicalSelfBootLinkRecordResolvedLibraries =
            resolvedLibraries
        , canonicalSelfBootLinkRecordLibrarySearchPaths =
            librarySearchPaths
        , canonicalSelfBootLinkRecordRPaths =
            rpaths
        , canonicalSelfBootLinkRecordInstallNames =
            installNames
        , canonicalSelfBootLinkRecordOutputArtifact =
            outputArtifact
        , canonicalSelfBootLinkRecordOwningStage =
            Just (SelfBootStageId "stage0")
        , canonicalSelfBootLinkRecordStageOwnedOutputDirectory =
            Just stageRoot
        , canonicalSelfBootLinkRecordExitStatus =
            NativeLinkExited 0
        }

reorderedValidRecord :: CanonicalSelfBootLinkRecord
reorderedValidRecord =
    validRecord
        { canonicalSelfBootLinkRecordObjectInputs =
            reverse objectInputs
        , canonicalSelfBootLinkRecordResolvedLibraries =
            reverse resolvedLibraries
        , canonicalSelfBootLinkRecordLibrarySearchPaths =
            reverse librarySearchPaths
        , canonicalSelfBootLinkRecordRPaths =
            reverse rpaths
        , canonicalSelfBootLinkRecordInstallNames =
            reverse installNames
        }

targetTriple :: TargetTriple
targetTriple =
    TargetTriple "x86_64-unknown-linux-gnu"

linkerMode :: ToolchainLinkerMode
linkerMode =
    ToolchainLinkerMode "dynamic"

stageRoot :: StageOwnedOutputDirectory
stageRoot =
    StageOwnedOutputDirectory ".self-boot/stage0"

objectInputs :: [NativeLinkObjectInput]
objectInputs =
    [ NativeLinkObjectInput
        { nativeLinkObjectInputPath = ".self-boot/stage0/obj/compiler-runtime.o"
        , nativeLinkObjectInputHash = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        }
    , NativeLinkObjectInput
        { nativeLinkObjectInputPath = ".self-boot/stage0/obj/compiler-main.o"
        , nativeLinkObjectInputHash = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        }
    ]

resolvedLibraries :: [ResolvedSelfBootLinkedLibraryIdentity]
resolvedLibraries =
    [ mlfpIoLibrary
    , libSystemLibrary
    , preludePackageLibrary
    ]

librarySearchPaths :: [NativeLinkSearchPath]
librarySearchPaths =
    [ NativeLinkSearchPath "/opt/mlf/toolchains/clang-18/lib"
    , NativeLinkSearchPath ".self-boot/stage0/lib"
    ]

rpaths :: [NativeLinkRPath]
rpaths =
    [ NativeLinkRPath ".self-boot/stage0/lib"
    , NativeLinkRPath "$ORIGIN/../lib"
    ]

installNames :: [NativeLinkInstallName]
installNames =
    [ NativeLinkInstallName "libmlfp_io.so"
    , NativeLinkInstallName "@rpath/libSystem.B.dylib"
    ]

outputArtifact :: NativeLinkOutputArtifact
outputArtifact =
    NativeLinkOutputArtifact
        { nativeLinkOutputArtifactPath = Just ".self-boot/stage0/bin/mlfp-stage1"
        , nativeLinkOutputArtifactHash = Just "sha256:6666666666666666666666666666666666666666666666666666666666666666"
        }

incompleteRecordCases :: [(String, CanonicalSelfBootLinkRecord, String)]
incompleteRecordCases =
    [ ( "missing proof action id"
      , validRecord {canonicalSelfBootLinkRecordProofActionId = Nothing}
      , "self-boot proof action id is missing"
      )
    , ( "blank proof action id"
      , validRecord {canonicalSelfBootLinkRecordProofActionId = Just (SelfBootProofActionId " ")}
      , "self-boot proof action id is blank"
      )
    , ( "missing owning stage"
      , validRecord {canonicalSelfBootLinkRecordOwningStage = Nothing}
      , "owning stage is missing"
      )
    , ( "blank owning stage"
      , validRecord {canonicalSelfBootLinkRecordOwningStage = Just (SelfBootStageId "")}
      , "owning stage is blank"
      )
    , ( "missing stage output directory"
      , validRecord {canonicalSelfBootLinkRecordStageOwnedOutputDirectory = Nothing}
      , "stage-owned output directory is missing"
      )
    , ( "blank stage output directory"
      , validRecord {canonicalSelfBootLinkRecordStageOwnedOutputDirectory = Just (StageOwnedOutputDirectory " ")}
      , "stage-owned output directory is blank"
      )
    , ( "empty linker argv"
      , validRecord {canonicalSelfBootLinkRecordLinkerArgv = []}
      , "linker argv vector is empty"
      )
    , ( "blank linker argv executable"
      , validRecord {canonicalSelfBootLinkRecordLinkerArgv = [" ", "-o", "program"]}
      , "linker argv executable is blank"
      )
    , ( "missing target triple"
      , validRecord {canonicalSelfBootLinkRecordTargetTriple = Nothing}
      , "target triple is missing"
      )
    , ( "blank target triple"
      , validRecord {canonicalSelfBootLinkRecordTargetTriple = Just (TargetTriple " ")}
      , "target triple is blank"
      )
    , ( "missing linker mode"
      , validRecord {canonicalSelfBootLinkRecordLinkerMode = Nothing}
      , "linker mode is missing"
      )
    , ( "blank linker mode"
      , validRecord {canonicalSelfBootLinkRecordLinkerMode = Just (ToolchainLinkerMode "")}
      , "linker mode is blank"
      )
    , ( "missing object inputs"
      , validRecord {canonicalSelfBootLinkRecordObjectInputs = []}
      , "native link record has no object inputs"
      )
    , ( "blank object input path"
      , validRecord {canonicalSelfBootLinkRecordObjectInputs = [NativeLinkObjectInput "" "sha256:object"]}
      , "object input path is blank"
      )
    , ( "blank object input hash"
      , validRecord {canonicalSelfBootLinkRecordObjectInputs = [NativeLinkObjectInput ".self-boot/stage0/obj/compiler-main.o" ""]}
      , "object input .self-boot/stage0/obj/compiler-main.o has blank content hash"
      )
    , ( "duplicate object input paths"
      , validRecord {canonicalSelfBootLinkRecordObjectInputs = [objectInputMain, objectInputMain]}
      , "duplicate object input path"
      )
    , ( "blank library search path"
      , validRecord {canonicalSelfBootLinkRecordLibrarySearchPaths = [NativeLinkSearchPath ""]}
      , "library search path is blank"
      )
    , ( "duplicate library search paths"
      , validRecord {canonicalSelfBootLinkRecordLibrarySearchPaths = [NativeLinkSearchPath ".self-boot/stage0/lib", NativeLinkSearchPath ".self-boot/stage0/lib"]}
      , "duplicate library search path"
      )
    , ( "blank rpath"
      , validRecord {canonicalSelfBootLinkRecordRPaths = [NativeLinkRPath " "]}
      , "rpath entry is blank"
      )
    , ( "duplicate rpaths"
      , validRecord {canonicalSelfBootLinkRecordRPaths = [NativeLinkRPath "$ORIGIN/../lib", NativeLinkRPath "$ORIGIN/../lib"]}
      , "duplicate rpath entry"
      )
    , ( "blank install name"
      , validRecord {canonicalSelfBootLinkRecordInstallNames = [NativeLinkInstallName ""]}
      , "install-name entry is blank"
      )
    , ( "duplicate install names"
      , validRecord {canonicalSelfBootLinkRecordInstallNames = [NativeLinkInstallName "libmlfp_io.so", NativeLinkInstallName "libmlfp_io.so"]}
      , "duplicate install-name entry"
      )
    , ( "missing output artifact path"
      , validRecord {canonicalSelfBootLinkRecordOutputArtifact = outputArtifact {nativeLinkOutputArtifactPath = Nothing}}
      , "output artifact path is missing"
      )
    , ( "blank output artifact path"
      , validRecord {canonicalSelfBootLinkRecordOutputArtifact = outputArtifact {nativeLinkOutputArtifactPath = Just " "}}
      , "output artifact path is blank"
      )
    , ( "missing output artifact hash"
      , validRecord {canonicalSelfBootLinkRecordOutputArtifact = outputArtifact {nativeLinkOutputArtifactHash = Nothing}}
      , "output artifact hash is missing"
      )
    , ( "blank output artifact hash"
      , validRecord {canonicalSelfBootLinkRecordOutputArtifact = outputArtifact {nativeLinkOutputArtifactHash = Just " "}}
      , "output artifact .self-boot/stage0/bin/mlfp-stage1 has blank content hash"
      )
    , ( "negative exit status"
      , validRecord {canonicalSelfBootLinkRecordExitStatus = NativeLinkExited (-1)}
      , "native link exit status is malformed"
      )
    , ( "blank signal exit status"
      , validRecord {canonicalSelfBootLinkRecordExitStatus = NativeLinkSignaled ""}
      , "native link exit status is malformed"
      )
    , ( "unsupported exit status"
      , validRecord {canonicalSelfBootLinkRecordExitStatus = NativeLinkExitStatusUnsupported "core-dumped=true"}
      , "native link exit status is unsupported"
      )
    ]

linkedLibraryViolationCases :: [(String, [ResolvedSelfBootLinkedLibraryIdentity], String)]
linkedLibraryViolationCases =
    [ ( "blank library name"
      , [mlfpIoLibrary {resolvedLinkedLibraryName = ""}]
      , "resolved linked library identity has blank library name"
      )
    , ( "blank library kind"
      , [mlfpIoLibrary {resolvedLinkedLibraryKind = ResolvedLinkedLibraryKind ""}]
      , "resolved linked library identity mlfp-io has blank kind"
      )
    , ( "blank library link mode"
      , [mlfpIoLibrary {resolvedLinkedLibraryLinkMode = ResolvedLinkedLibraryLinkMode " "}]
      , "resolved linked library identity mlfp-io has blank link mode"
      )
    , ( "unresolved -l library"
      , [ mlfpIoLibrary
            { resolvedLinkedLibraryName = "-lmlfp_io"
            , resolvedLinkedLibraryFilePath = Nothing
            , resolvedLinkedLibraryContentHash = Nothing
            }
        ]
      , "resolved linked library identity -lmlfp_io uses only an unresolved -l-style name"
      )
    , ( "missing resolved identity"
      , [ mlfpIoLibrary
            { resolvedLinkedLibraryFilePath = Nothing
            , resolvedLinkedLibraryContentHash = Nothing
            }
        ]
      , "resolved linked library identity mlfp-io has no resolved file, framework, or platform package identity"
      )
    , ( "blank resolved file path"
      , [mlfpIoLibrary {resolvedLinkedLibraryFilePath = Just " "}]
      , "resolved linked library identity mlfp-io has blank resolved file path"
      )
    , ( "blank framework identity"
      , [libSystemLibrary {resolvedLinkedLibraryFrameworkIdentity = Just ""}]
      , "resolved linked library identity libSystem has blank framework identity"
      )
    , ( "blank package identity"
      , [preludePackageLibrary {resolvedLinkedLibraryPlatformPackageIdentity = Just ""}]
      , "resolved linked library identity mlf.prelude has blank platform package identity"
      )
    , ( "missing content hash"
      , [mlfpIoLibrary {resolvedLinkedLibraryContentHash = Nothing}]
      , "resolved linked library identity mlfp-io is file/framework-backed but missing content hash"
      )
    , ( "blank content hash"
      , [mlfpIoLibrary {resolvedLinkedLibraryContentHash = Just ""}]
      , "resolved linked library identity mlfp-io has blank content hash"
      )
    , ( "duplicate library identity"
      , [mlfpIoLibrary, mlfpIoLibrary]
      , "duplicate resolved linked library identity key"
      )
    ]

objectInputMain :: NativeLinkObjectInput
objectInputMain =
    NativeLinkObjectInput
        { nativeLinkObjectInputPath = ".self-boot/stage0/obj/compiler-main.o"
        , nativeLinkObjectInputHash = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        }

mlfpIoLibrary :: ResolvedSelfBootLinkedLibraryIdentity
mlfpIoLibrary =
    ResolvedSelfBootLinkedLibraryIdentity
        { resolvedLinkedLibraryName = "mlfp-io"
        , resolvedLinkedLibraryKind = ResolvedLinkedLibraryKind "runtime"
        , resolvedLinkedLibraryLinkMode = ResolvedLinkedLibraryLinkMode "dynamic"
        , resolvedLinkedLibraryFilePath = Just ".self-boot/stage0/lib/libmlfp_io.so"
        , resolvedLinkedLibraryFrameworkIdentity = Nothing
        , resolvedLinkedLibraryPlatformPackageIdentity = Nothing
        , resolvedLinkedLibraryContentHash = Just "sha256:3333333333333333333333333333333333333333333333333333333333333333"
        }

libSystemLibrary :: ResolvedSelfBootLinkedLibraryIdentity
libSystemLibrary =
    ResolvedSelfBootLinkedLibraryIdentity
        { resolvedLinkedLibraryName = "libSystem"
        , resolvedLinkedLibraryKind = ResolvedLinkedLibraryKind "framework"
        , resolvedLinkedLibraryLinkMode = ResolvedLinkedLibraryLinkMode "dynamic"
        , resolvedLinkedLibraryFilePath = Nothing
        , resolvedLinkedLibraryFrameworkIdentity = Just "framework:libSystem.B"
        , resolvedLinkedLibraryPlatformPackageIdentity = Nothing
        , resolvedLinkedLibraryContentHash = Just "sha256:4444444444444444444444444444444444444444444444444444444444444444"
        }

preludePackageLibrary :: ResolvedSelfBootLinkedLibraryIdentity
preludePackageLibrary =
    ResolvedSelfBootLinkedLibraryIdentity
        { resolvedLinkedLibraryName = "mlf.prelude"
        , resolvedLinkedLibraryKind = ResolvedLinkedLibraryKind "platform-package"
        , resolvedLinkedLibraryLinkMode = ResolvedLinkedLibraryLinkMode "static"
        , resolvedLinkedLibraryFilePath = Nothing
        , resolvedLinkedLibraryFrameworkIdentity = Nothing
        , resolvedLinkedLibraryPlatformPackageIdentity = Just "package:mlf.prelude@lock-sha256:5555"
        , resolvedLinkedLibraryContentHash = Nothing
        }

expectViolations :: CanonicalSelfBootLinkRecord -> IO String
expectViolations record =
    case validateCanonicalSelfBootLinkRecord record of
        Left violations -> pure (renderNativeLinkRecordViolations violations)
        Right evidence -> do
            expectationFailure ("expected native link record violations, got: " ++ renderNativeLinkRecordEvidence evidence)
            pure ""
