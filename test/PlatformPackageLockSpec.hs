module PlatformPackageLockSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import MLF.Frontend.Program.BuildGraph
    ( InterfaceSummaryMetadata (..)
    , PackageSourceMetadata (..)
    )
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    )
import MLF.Platform.Contract (PlatformAbiVersion (..))
import MLF.Platform.PackageLock
import Test.Hspec

spec :: Spec
spec = describe "MLF.Platform package lock" $ do
    it "renders checked local package lock evidence deterministically" $ do
        expected <- readFile "test/golden/platform-contract/checked-package-lock.txt"
        evidence <- expectEvidence validLock validCurrentSnapshot
        reorderedEvidence <- expectEvidence reorderedValidLock reorderedValidCurrentSnapshot
        renderPackageLockEvidence evidence `shouldBe` expected
        renderPackageLockEvidence reorderedEvidence `shouldBe` expected
        renderSelfBootPackageLock validLock `shouldSatisfy` rendersCheckedLockHeader

    it "rejects checked package lock drift with named diagnostics" $ do
        forM_ driftViolationCases $ \(_, lock, snapshot, expectedDiagnostic) -> do
            diagnostics <- expectViolations lock snapshot
            diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

    it "rejects duplicate and blank checked package lock fields with named diagnostics" $ do
        forM_ declarationViolationCases $ \(_, lock, expectedDiagnostic) -> do
            diagnostics <- expectViolations lock validCurrentSnapshot
            diagnostics `shouldSatisfy` isInfixOf expectedDiagnostic

    it "keeps package lock validation pure over explicit snapshots" $ do
        let first = validateSelfBootPackageLock validLock validCurrentSnapshot
            second = validateSelfBootPackageLock validLock validCurrentSnapshot
        first `shouldBe` second

validLock :: SelfBootPackageLock
validLock =
    SelfBootPackageLock
        [ compilerPackage
        , preludePackage
        ]

reorderedValidLock :: SelfBootPackageLock
reorderedValidLock =
    SelfBootPackageLock
        [ preludePackage
            { lockedPackageEntryModules =
                reverse (lockedPackageEntryModules preludePackage)
            }
        , compilerPackage
            { lockedPackageEntryModules =
                map reorderLockedModuleEntry (reverse (lockedPackageEntryModules compilerPackage))
            }
        ]

validCurrentSnapshot :: CurrentPackageLockSnapshot
validCurrentSnapshot =
    CurrentPackageLockSnapshot
        [ currentCompilerPackage
        , currentPreludePackage
        ]

reorderedValidCurrentSnapshot :: CurrentPackageLockSnapshot
reorderedValidCurrentSnapshot =
    CurrentPackageLockSnapshot
        [ currentPreludePackage
            { currentPackageEntryModules =
                reverse (currentPackageEntryModules currentPreludePackage)
            }
        , currentCompilerPackage
            { currentPackageEntryModules =
                map reorderLockedModuleEntry (reverse (currentPackageEntryModules currentCompilerPackage))
            }
        ]

compilerPackage :: LockedPackageEntry
compilerPackage =
    LockedPackageEntry
        { lockedPackageEntryIdentity = Just compilerPackageIdentity
        , lockedPackageEntryRoot = Just (LockedPackageRoot "packages/compiler")
        , lockedPackageEntryRequiredAbiVersion = abiVersion
        , lockedPackageEntryRequiredSubstrateFingerprint = substrateFingerprint
        , lockedPackageEntryModules =
            [ compilerMainModule
            , compilerResolveModule
            ]
        }

preludePackage :: LockedPackageEntry
preludePackage =
    LockedPackageEntry
        { lockedPackageEntryIdentity = Just preludePackageIdentity
        , lockedPackageEntryRoot = Just (LockedPackageRoot "packages/prelude")
        , lockedPackageEntryRequiredAbiVersion = abiVersion
        , lockedPackageEntryRequiredSubstrateFingerprint = substrateFingerprint
        , lockedPackageEntryModules = [preludeModule]
        }

currentCompilerPackage :: CurrentPackageEntry
currentCompilerPackage =
    CurrentPackageEntry
        { currentPackageEntryIdentity = compilerPackageIdentity
        , currentPackageEntryRoot = LockedPackageRoot "packages/compiler"
        , currentPackageEntryAbiVersion = abiVersion
        , currentPackageEntrySubstrateFingerprint = substrateFingerprint
        , currentPackageEntryModules =
            [ compilerMainModule
            , compilerResolveModule
            ]
        }

currentPreludePackage :: CurrentPackageEntry
currentPreludePackage =
    CurrentPackageEntry
        { currentPackageEntryIdentity = preludePackageIdentity
        , currentPackageEntryRoot = LockedPackageRoot "packages/prelude"
        , currentPackageEntryAbiVersion = abiVersion
        , currentPackageEntrySubstrateFingerprint = substrateFingerprint
        , currentPackageEntryModules = [preludeModule]
        }

compilerPackageIdentity :: LockedPackageIdentity
compilerPackageIdentity =
    LockedPackageIdentity compilerPackageId

preludePackageIdentity :: LockedPackageIdentity
preludePackageIdentity =
    LockedPackageIdentity preludePackageId

compilerPackageId :: PackageId
compilerPackageId =
    PackageId "mlf.compiler"

preludePackageId :: PackageId
preludePackageId =
    PackageId "mlf.prelude"

abiVersion :: PlatformAbiVersion
abiVersion =
    PlatformAbiVersion "mlf-platform-abi-1"

substrateFingerprint :: LockedSubstrateFingerprintMaterial
substrateFingerprint =
    LockedSubstrateFingerprintMaterial "substrate-fingerprint:round-360"

compilerMainId :: PackageModuleId
compilerMainId =
    PackageModuleId compilerPackageId "Compiler.Main"

compilerResolveId :: PackageModuleId
compilerResolveId =
    PackageModuleId compilerPackageId "Compiler.Resolve"

preludeId :: PackageModuleId
preludeId =
    PackageModuleId preludePackageId "Prelude"

extraPackageId :: PackageId
extraPackageId =
    PackageId "mlf.extra"

extraPackageIdentity :: LockedPackageIdentity
extraPackageIdentity =
    LockedPackageIdentity extraPackageId

extraModuleId :: PackageModuleId
extraModuleId =
    PackageModuleId compilerPackageId "Compiler.Extra"

compilerMainModule :: LockedModuleEntry
compilerMainModule =
    LockedModuleEntry
        { lockedModuleEntryId = compilerMainId
        , lockedModuleEntrySourceMetadata =
            PackageSourceMetadata "source-sha256:compiler-main"
        , lockedModuleEntryDirectDependencies =
            [ compilerResolveId
            , preludeId
            ]
        , lockedModuleEntryDependencyInterfaces =
            [ dependencyInterface compilerResolveId "interface-sha256:compiler-resolve"
            , dependencyInterface preludeId "interface-sha256:prelude"
            ]
        , lockedModuleEntryInterfaceMetadata =
            InterfaceSummaryMetadata "interface-sha256:compiler-main"
        }

compilerResolveModule :: LockedModuleEntry
compilerResolveModule =
    LockedModuleEntry
        { lockedModuleEntryId = compilerResolveId
        , lockedModuleEntrySourceMetadata =
            PackageSourceMetadata "source-sha256:compiler-resolve"
        , lockedModuleEntryDirectDependencies = [preludeId]
        , lockedModuleEntryDependencyInterfaces =
            [dependencyInterface preludeId "interface-sha256:prelude"]
        , lockedModuleEntryInterfaceMetadata =
            InterfaceSummaryMetadata "interface-sha256:compiler-resolve"
        }

preludeModule :: LockedModuleEntry
preludeModule =
    LockedModuleEntry
        { lockedModuleEntryId = preludeId
        , lockedModuleEntrySourceMetadata =
            PackageSourceMetadata "source-sha256:prelude"
        , lockedModuleEntryDirectDependencies = []
        , lockedModuleEntryDependencyInterfaces = []
        , lockedModuleEntryInterfaceMetadata =
            InterfaceSummaryMetadata "interface-sha256:prelude"
        }

extraModule :: LockedModuleEntry
extraModule =
    LockedModuleEntry
        { lockedModuleEntryId = extraModuleId
        , lockedModuleEntrySourceMetadata =
            PackageSourceMetadata "source-sha256:compiler-extra"
        , lockedModuleEntryDirectDependencies = []
        , lockedModuleEntryDependencyInterfaces = []
        , lockedModuleEntryInterfaceMetadata =
            InterfaceSummaryMetadata "interface-sha256:compiler-extra"
        }

extraCurrentPackage :: CurrentPackageEntry
extraCurrentPackage =
    CurrentPackageEntry
        { currentPackageEntryIdentity = extraPackageIdentity
        , currentPackageEntryRoot = LockedPackageRoot "packages/extra"
        , currentPackageEntryAbiVersion = abiVersion
        , currentPackageEntrySubstrateFingerprint = substrateFingerprint
        , currentPackageEntryModules = []
        }

dependencyInterface :: PackageModuleId -> String -> LockedDependencyInterface
dependencyInterface moduleId fingerprint =
    LockedDependencyInterface
        { lockedDependencyInterfaceModuleId = moduleId
        , lockedDependencyInterfaceMetadata = InterfaceSummaryMetadata fingerprint
        }

driftViolationCases :: [(String, SelfBootPackageLock, CurrentPackageLockSnapshot, String)]
driftViolationCases =
    [ ( "current package identity",
        validLock,
        validCurrentSnapshot
            { currentPackageLockSnapshotEntries =
                extraCurrentPackage : currentPackageLockSnapshotEntries validCurrentSnapshot
            },
        "current package identity not declared by checked lock: mlf.extra"
      )
    , ( "locked package identity",
        validLock,
        CurrentPackageLockSnapshot [currentCompilerPackage],
        "locked package identity missing from current package snapshot: mlf.prelude"
      )
    , ( "normalized local root",
        validLock,
        validCurrentSnapshot
            { currentPackageLockSnapshotEntries =
                [ currentCompilerPackage
                    { currentPackageEntryRoot =
                        LockedPackageRoot "packages/compiler-renamed"
                    }
                , currentPreludePackage
                ]
            },
        "normalized local root drift for package mlf.compiler"
      )
    , ( "ABI version",
        validLock,
        validCurrentSnapshot
            { currentPackageLockSnapshotEntries =
                [ currentCompilerPackage
                    { currentPackageEntryAbiVersion =
                        PlatformAbiVersion "mlf-platform-abi-2"
                    }
                , currentPreludePackage
                ]
            },
        "required ABI version drift for package mlf.compiler"
      )
    , ( "substrate fingerprint",
        validLock,
        validCurrentSnapshot
            { currentPackageLockSnapshotEntries =
                [ currentCompilerPackage
                    { currentPackageEntrySubstrateFingerprint =
                        LockedSubstrateFingerprintMaterial "substrate-fingerprint:changed"
                    }
                , currentPreludePackage
                ]
            },
        "required substrate fingerprint material drift for package mlf.compiler"
      )
    , ( "source metadata",
        validLock,
        currentCompilerWithMain
            compilerMainModule
                { lockedModuleEntrySourceMetadata =
                    PackageSourceMetadata "source-sha256:compiler-main-changed"
                },
        "source metadata drift for module mlf.compiler:Compiler.Main"
      )
    , ( "direct dependency id",
        validLock,
        currentCompilerWithMain
            compilerMainModule
                { lockedModuleEntryDirectDependencies = [preludeId]
                },
        "direct dependency id drift for module mlf.compiler:Compiler.Main"
      )
    , ( "dependency interface metadata",
        validLock,
        currentCompilerWithMain
            compilerMainModule
                { lockedModuleEntryDependencyInterfaces =
                    [ dependencyInterface compilerResolveId "interface-sha256:compiler-resolve"
                    , dependencyInterface preludeId "interface-sha256:prelude-changed"
                    ]
                },
        "dependency interface metadata drift for module mlf.compiler:Compiler.Main importing mlf.prelude:Prelude"
      )
    , ( "interface metadata",
        validLock,
        currentCompilerWithMain
            compilerMainModule
                { lockedModuleEntryInterfaceMetadata =
                    InterfaceSummaryMetadata "interface-sha256:compiler-main-changed"
                },
        "interface metadata drift for module mlf.compiler:Compiler.Main"
      )
    , ( "current module identity",
        validLock,
        validCurrentSnapshot
            { currentPackageLockSnapshotEntries =
                [ currentCompilerPackage
                    { currentPackageEntryModules =
                        extraModule : currentPackageEntryModules currentCompilerPackage
                    }
                , currentPreludePackage
                ]
            },
        "current module identity missing from checked lock for package mlf.compiler: mlf.compiler:Compiler.Extra"
      )
    , ( "locked module identity",
        validLock,
        validCurrentSnapshot
            { currentPackageLockSnapshotEntries =
                [ currentCompilerPackage
                    { currentPackageEntryModules = [compilerResolveModule]
                    }
                , currentPreludePackage
                ]
            },
        "locked module identity missing from current package snapshot for package mlf.compiler: mlf.compiler:Compiler.Main"
      )
    ]

declarationViolationCases :: [(String, SelfBootPackageLock, String)]
declarationViolationCases =
    [ ( "missing package identity",
        SelfBootPackageLock
            [compilerPackage {lockedPackageEntryIdentity = Nothing}],
        "package identity is missing"
      )
    , ( "blank package identity",
        SelfBootPackageLock
            [ compilerPackage
                { lockedPackageEntryIdentity =
                    Just (LockedPackageIdentity (PackageId ""))
                }
            ],
        "package identity is blank"
      )
    , ( "missing normalized local root",
        SelfBootPackageLock
            [compilerPackage {lockedPackageEntryRoot = Nothing}],
        "normalized local root is missing"
      )
    , ( "blank normalized local root",
        SelfBootPackageLock
            [ compilerPackage
                { lockedPackageEntryRoot =
                    Just (LockedPackageRoot "")
                }
            ],
        "normalized local root is blank"
      )
    , ( "duplicate locked package identities",
        SelfBootPackageLock [compilerPackage, compilerPackage],
        "duplicate locked package identity: mlf.compiler"
      )
    , ( "duplicate locked module identities",
        SelfBootPackageLock
            [ compilerPackage
                { lockedPackageEntryModules =
                    compilerMainModule : lockedPackageEntryModules compilerPackage
                }
            , preludePackage
            ],
        "duplicate locked module identity in package mlf.compiler: mlf.compiler:Compiler.Main"
      )
    , ( "duplicate dependency interface entries",
        SelfBootPackageLock
            [ compilerPackage
                { lockedPackageEntryModules =
                    [ compilerMainModule
                        { lockedModuleEntryDependencyInterfaces =
                            dependencyInterface preludeId "interface-sha256:prelude"
                                : lockedModuleEntryDependencyInterfaces compilerMainModule
                        }
                    , compilerResolveModule
                    ]
                }
            , preludePackage
            ],
        "duplicate dependency interface entry in package mlf.compiler module mlf.compiler:Compiler.Main: mlf.prelude:Prelude"
      )
    ]

currentCompilerWithMain :: LockedModuleEntry -> CurrentPackageLockSnapshot
currentCompilerWithMain mainModule =
    validCurrentSnapshot
        { currentPackageLockSnapshotEntries =
            [ currentCompilerPackage
                { currentPackageEntryModules =
                    [ mainModule
                    , compilerResolveModule
                    ]
                }
            , currentPreludePackage
            ]
        }

reorderLockedModuleEntry :: LockedModuleEntry -> LockedModuleEntry
reorderLockedModuleEntry moduleEntry =
    moduleEntry
        { lockedModuleEntryDirectDependencies =
            reverse (lockedModuleEntryDirectDependencies moduleEntry)
        , lockedModuleEntryDependencyInterfaces =
            reverse (lockedModuleEntryDependencyInterfaces moduleEntry)
        }

rendersCheckedLockHeader :: Either [PackageLockViolation] String -> Bool
rendersCheckedLockHeader result =
    case result of
        Right rendered ->
            "mlf-platform-checked-package-lock-v1" `isInfixOf` rendered
        Left _ ->
            False

expectEvidence :: SelfBootPackageLock -> CurrentPackageLockSnapshot -> IO PackageLockEvidence
expectEvidence lock snapshot =
    case validateSelfBootPackageLock lock snapshot of
        Right evidence -> pure evidence
        Left violations -> do
            expectationFailure ("expected valid package lock, got: " ++ renderPackageLockViolations violations)
            pure (PackageLockEvidence [])

expectViolations :: SelfBootPackageLock -> CurrentPackageLockSnapshot -> IO String
expectViolations lock snapshot =
    case validateSelfBootPackageLock lock snapshot of
        Left violations -> pure (renderPackageLockViolations violations)
        Right evidence -> do
            expectationFailure ("expected package lock violations, got: " ++ renderPackageLockEvidence evidence)
            pure ""
