module MLF.Platform.PackageLock
    ( SelfBootPackageLock (..)
    , LockedPackageIdentity (..)
    , LockedPackageRoot (..)
    , LockedSubstrateFingerprintMaterial (..)
    , LockedPackageEntry (..)
    , LockedModuleEntry (..)
    , LockedDependencyInterface (..)
    , CurrentPackageEntry (..)
    , CurrentPackageLockSnapshot (..)
    , PackageLockEvidence (..)
    , PackageLockViolation (..)
    , validateSelfBootPackageLock
    , renderSelfBootPackageLock
    , renderPackageLockEvidence
    , renderPackageLockViolation
    , renderPackageLockViolations
    ) where

import Data.Char (isSpace)
import Data.List (find, group, sort, sortOn)

import MLF.Frontend.Program.BuildGraph
    ( InterfaceSummaryMetadata (..)
    , PackageSourceMetadata (..)
    )
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    )
import MLF.Platform.Contract (PlatformAbiVersion (..))

newtype LockedPackageIdentity = LockedPackageIdentity
    { lockedPackageIdentityPackageId :: PackageId
    }
    deriving (Eq, Ord, Show)

newtype LockedPackageRoot = LockedPackageRoot
    { lockedPackageRootPath :: FilePath
    }
    deriving (Eq, Ord, Show)

newtype LockedSubstrateFingerprintMaterial = LockedSubstrateFingerprintMaterial
    { unLockedSubstrateFingerprintMaterial :: String
    }
    deriving (Eq, Ord, Show)

data SelfBootPackageLock = SelfBootPackageLock
    { selfBootPackageLockEntries :: [LockedPackageEntry]
    }
    deriving (Eq, Ord, Show)

data LockedPackageEntry = LockedPackageEntry
    { lockedPackageEntryIdentity :: Maybe LockedPackageIdentity
    , lockedPackageEntryRoot :: Maybe LockedPackageRoot
    , lockedPackageEntryRequiredAbiVersion :: PlatformAbiVersion
    , lockedPackageEntryRequiredSubstrateFingerprint :: LockedSubstrateFingerprintMaterial
    , lockedPackageEntryModules :: [LockedModuleEntry]
    }
    deriving (Eq, Ord, Show)

data LockedModuleEntry = LockedModuleEntry
    { lockedModuleEntryId :: PackageModuleId
    , lockedModuleEntrySourceMetadata :: PackageSourceMetadata
    , lockedModuleEntryDirectDependencies :: [PackageModuleId]
    , lockedModuleEntryDependencyInterfaces :: [LockedDependencyInterface]
    , lockedModuleEntryInterfaceMetadata :: InterfaceSummaryMetadata
    }
    deriving (Eq, Ord, Show)

data LockedDependencyInterface = LockedDependencyInterface
    { lockedDependencyInterfaceModuleId :: PackageModuleId
    , lockedDependencyInterfaceMetadata :: InterfaceSummaryMetadata
    }
    deriving (Eq, Ord, Show)

data CurrentPackageEntry = CurrentPackageEntry
    { currentPackageEntryIdentity :: LockedPackageIdentity
    , currentPackageEntryRoot :: LockedPackageRoot
    , currentPackageEntryAbiVersion :: PlatformAbiVersion
    , currentPackageEntrySubstrateFingerprint :: LockedSubstrateFingerprintMaterial
    , currentPackageEntryModules :: [LockedModuleEntry]
    }
    deriving (Eq, Ord, Show)

data CurrentPackageLockSnapshot = CurrentPackageLockSnapshot
    { currentPackageLockSnapshotEntries :: [CurrentPackageEntry]
    }
    deriving (Eq, Ord, Show)

data PackageLockEvidence = PackageLockEvidence
    { packageLockEvidenceEntries :: [LockedPackageEntry]
    }
    deriving (Eq, Ord, Show)

data PackageLockViolation
    = MissingLockedPackageIdentity
    | BlankLockedPackageIdentity
    | MissingLockedPackageRoot String
    | BlankLockedPackageRoot String
    | DuplicateLockedPackageIdentity String
    | DuplicateLockedModuleIdentity String String
    | DuplicateLockedDependencyInterface String String String
    | CurrentPackageIdentityNotDeclaredByCheckedLock String
    | LockedPackageIdentityMissingFromCurrentSnapshot String
    | NormalizedLocalRootDrift String String String
    | RequiredAbiVersionDrift String String String
    | RequiredSubstrateFingerprintDrift String String String
    | SourceMetadataDrift String String String
    | DirectDependencyIdsDrift String [PackageModuleId] [PackageModuleId]
    | DependencyInterfaceMetadataDrift String String (Maybe InterfaceSummaryMetadata) (Maybe InterfaceSummaryMetadata)
    | InterfaceMetadataDrift String InterfaceSummaryMetadata InterfaceSummaryMetadata
    | CurrentModuleIdentityMissingFromCheckedLock String String
    | LockedModuleIdentityMissingFromCurrentSnapshot String String
    deriving (Eq, Ord, Show)

validateSelfBootPackageLock ::
    SelfBootPackageLock ->
    CurrentPackageLockSnapshot ->
    Either [PackageLockViolation] PackageLockEvidence
validateSelfBootPackageLock lock snapshot =
    case violations of
        [] -> Right (PackageLockEvidence (selfBootPackageLockEntries lock))
        _ -> Left (sort violations)
  where
    violations =
        validateLockedPackageEntries (selfBootPackageLockEntries lock)
            ++ validatePackageClosure (selfBootPackageLockEntries lock) (currentPackageLockSnapshotEntries snapshot)

renderSelfBootPackageLock :: SelfBootPackageLock -> Either [PackageLockViolation] String
renderSelfBootPackageLock lock =
    case validateLockedPackageEntries (selfBootPackageLockEntries lock) of
        [] -> Right (renderPackageEntries "mlf-platform-checked-package-lock-v1" (selfBootPackageLockEntries lock))
        violations -> Left (sort violations)

renderPackageLockEvidence :: PackageLockEvidence -> String
renderPackageLockEvidence evidence =
    renderPackageEntries "mlf-platform-checked-package-lock-evidence-v1" (packageLockEvidenceEntries evidence)

renderPackageLockViolation :: PackageLockViolation -> String
renderPackageLockViolation violation =
    case violation of
        MissingLockedPackageIdentity ->
            "package identity is missing from checked package lock entry"
        BlankLockedPackageIdentity ->
            "package identity is blank in checked package lock entry"
        MissingLockedPackageRoot packageName ->
            "normalized local root is missing for checked package " ++ renderNamedValue packageName
        BlankLockedPackageRoot packageName ->
            "normalized local root is blank for checked package " ++ renderNamedValue packageName
        DuplicateLockedPackageIdentity packageName ->
            "duplicate locked package identity: " ++ renderNamedValue packageName
        DuplicateLockedModuleIdentity packageName moduleName ->
            "duplicate locked module identity in package " ++ renderNamedValue packageName ++ ": " ++ renderNamedValue moduleName
        DuplicateLockedDependencyInterface packageName moduleName dependencyName ->
            "duplicate dependency interface entry in package "
                ++ renderNamedValue packageName
                ++ " module "
                ++ renderNamedValue moduleName
                ++ ": "
                ++ renderNamedValue dependencyName
        CurrentPackageIdentityNotDeclaredByCheckedLock packageName ->
            "current package identity missing from checked lock; current package identity not declared by checked lock: "
                ++ renderNamedValue packageName
        LockedPackageIdentityMissingFromCurrentSnapshot packageName ->
            "locked package identity missing from current package snapshot: " ++ renderNamedValue packageName
        NormalizedLocalRootDrift packageName expected actual ->
            "normalized local root drift for package "
                ++ renderNamedValue packageName
                ++ ": expected "
                ++ expected
                ++ " current "
                ++ actual
        RequiredAbiVersionDrift packageName expected actual ->
            "required ABI version drift for package "
                ++ renderNamedValue packageName
                ++ ": expected "
                ++ expected
                ++ " current "
                ++ actual
        RequiredSubstrateFingerprintDrift packageName expected actual ->
            "required substrate fingerprint material drift for package "
                ++ renderNamedValue packageName
                ++ ": expected "
                ++ expected
                ++ " current "
                ++ actual
        SourceMetadataDrift moduleName expected actual ->
            "source metadata drift for module "
                ++ renderNamedValue moduleName
                ++ ": expected "
                ++ expected
                ++ " current "
                ++ actual
        DirectDependencyIdsDrift moduleName expected actual ->
            "direct dependency id drift for module "
                ++ renderNamedValue moduleName
                ++ ": expected "
                ++ renderPackageModuleIds expected
                ++ " current "
                ++ renderPackageModuleIds actual
        DependencyInterfaceMetadataDrift moduleName dependencyName expected actual ->
            "dependency interface metadata drift for module "
                ++ renderNamedValue moduleName
                ++ " importing "
                ++ renderNamedValue dependencyName
                ++ ": expected "
                ++ renderMaybeInterfaceMetadata expected
                ++ " current "
                ++ renderMaybeInterfaceMetadata actual
        InterfaceMetadataDrift moduleName expected actual ->
            "interface metadata drift for module "
                ++ renderNamedValue moduleName
                ++ ": expected "
                ++ interfaceSummaryMetadataFingerprint expected
                ++ " current "
                ++ interfaceSummaryMetadataFingerprint actual
        CurrentModuleIdentityMissingFromCheckedLock packageName moduleName ->
            "current module identity missing from checked lock for package "
                ++ renderNamedValue packageName
                ++ ": "
                ++ renderNamedValue moduleName
        LockedModuleIdentityMissingFromCurrentSnapshot packageName moduleName ->
            "locked module identity missing from current package snapshot for package "
                ++ renderNamedValue packageName
                ++ ": "
                ++ renderNamedValue moduleName

renderPackageLockViolations :: [PackageLockViolation] -> String
renderPackageLockViolations =
    unlines . map renderPackageLockViolation . sort

validateLockedPackageEntries :: [LockedPackageEntry] -> [PackageLockViolation]
validateLockedPackageEntries entries =
    concatMap validateLockedPackageEntry entries
        ++ map DuplicateLockedPackageIdentity (duplicates (lockedPackageKeys entries))

validateLockedPackageEntry :: LockedPackageEntry -> [PackageLockViolation]
validateLockedPackageEntry entry =
    validateLockedPackageIdentity entry
        ++ validateLockedPackageRoot entry
        ++ map (DuplicateLockedModuleIdentity packageName) duplicateModuleKeys
        ++ concatMap (validateLockedModuleEntry packageName) (lockedPackageEntryModules entry)
  where
    packageName =
        lockedPackageEntryLabel entry
    duplicateModuleKeys =
        duplicates (map renderPackageModuleId (lockedPackageEntryModules entry >>= pure . lockedModuleEntryId))

validateLockedPackageIdentity :: LockedPackageEntry -> [PackageLockViolation]
validateLockedPackageIdentity entry =
    case lockedPackageEntryIdentity entry of
        Nothing -> [MissingLockedPackageIdentity]
        Just identity
            | isBlank (lockedPackageIdentityKey identity) -> [BlankLockedPackageIdentity]
            | otherwise -> []

validateLockedPackageRoot :: LockedPackageEntry -> [PackageLockViolation]
validateLockedPackageRoot entry =
    case lockedPackageEntryRoot entry of
        Nothing -> [MissingLockedPackageRoot (lockedPackageEntryLabel entry)]
        Just root
            | isBlank (lockedPackageRootPath root) -> [BlankLockedPackageRoot (lockedPackageEntryLabel entry)]
            | otherwise -> []

validateLockedModuleEntry :: String -> LockedModuleEntry -> [PackageLockViolation]
validateLockedModuleEntry packageName moduleEntry =
    map
        (DuplicateLockedDependencyInterface packageName (renderPackageModuleId (lockedModuleEntryId moduleEntry)))
        (duplicates (map (renderPackageModuleId . lockedDependencyInterfaceModuleId) (lockedModuleEntryDependencyInterfaces moduleEntry)))

validatePackageClosure :: [LockedPackageEntry] -> [CurrentPackageEntry] -> [PackageLockViolation]
validatePackageClosure locked current =
    concat
        [ map
            (CurrentPackageIdentityNotDeclaredByCheckedLock . currentPackageKey)
            (filter (not . hasLockedPackage locked . currentPackageEntryIdentity) current)
        , map
            (LockedPackageIdentityMissingFromCurrentSnapshot . lockedPackageEntryLabel)
            (filter (not . hasCurrentPackage current) validLocked)
        , concatMap (validateMatchingPackage current) validLocked
        ]
  where
    validLocked =
        filter hasComparableLockedPackageIdentity locked

validateMatchingPackage :: [CurrentPackageEntry] -> LockedPackageEntry -> [PackageLockViolation]
validateMatchingPackage current locked =
    case lockedPackageEntryIdentity locked >>= findCurrentPackage current of
        Nothing -> []
        Just currentEntry ->
            concat
                [ compareText
                    (NormalizedLocalRootDrift packageName)
                    (maybe "<missing>" lockedPackageRootPath (lockedPackageEntryRoot locked))
                    (lockedPackageRootPath (currentPackageEntryRoot currentEntry))
                , compareText
                    (RequiredAbiVersionDrift packageName)
                    (unPlatformAbiVersion (lockedPackageEntryRequiredAbiVersion locked))
                    (unPlatformAbiVersion (currentPackageEntryAbiVersion currentEntry))
                , compareText
                    (RequiredSubstrateFingerprintDrift packageName)
                    (unLockedSubstrateFingerprintMaterial (lockedPackageEntryRequiredSubstrateFingerprint locked))
                    (unLockedSubstrateFingerprintMaterial (currentPackageEntrySubstrateFingerprint currentEntry))
                , validateModuleClosure packageName (lockedPackageEntryModules locked) (currentPackageEntryModules currentEntry)
                ]
  where
    packageName =
        lockedPackageEntryLabel locked

validateModuleClosure :: String -> [LockedModuleEntry] -> [LockedModuleEntry] -> [PackageLockViolation]
validateModuleClosure packageName locked current =
    concat
        [ map
            (CurrentModuleIdentityMissingFromCheckedLock packageName . renderPackageModuleId . lockedModuleEntryId)
            (filter (not . hasLockedModule locked . lockedModuleEntryId) current)
        , map
            (LockedModuleIdentityMissingFromCurrentSnapshot packageName . renderPackageModuleId . lockedModuleEntryId)
            (filter (not . hasCurrentModule current . lockedModuleEntryId) locked)
        , concatMap (validateMatchingModule current) locked
        ]

validateMatchingModule :: [LockedModuleEntry] -> LockedModuleEntry -> [PackageLockViolation]
validateMatchingModule current locked =
    case findCurrentModule current (lockedModuleEntryId locked) of
        Nothing -> []
        Just currentModule ->
            let lockedDependencies =
                    sort (lockedModuleEntryDirectDependencies locked)
                currentDependencies =
                    sort (lockedModuleEntryDirectDependencies currentModule)
             in concat
                    [ compareText
                        (SourceMetadataDrift moduleName)
                        (packageSourceMetadataFingerprint (lockedModuleEntrySourceMetadata locked))
                        (packageSourceMetadataFingerprint (lockedModuleEntrySourceMetadata currentModule))
                    , [ DirectDependencyIdsDrift moduleName lockedDependencies currentDependencies
                        | lockedDependencies /= currentDependencies
                      ]
                    , validateDependencyInterfaceMetadata moduleName locked currentModule
                    , [ InterfaceMetadataDrift
                        moduleName
                        (lockedModuleEntryInterfaceMetadata locked)
                        (lockedModuleEntryInterfaceMetadata currentModule)
                        | lockedModuleEntryInterfaceMetadata locked /= lockedModuleEntryInterfaceMetadata currentModule
                      ]
                    ]
  where
    moduleName =
        renderPackageModuleId (lockedModuleEntryId locked)

validateDependencyInterfaceMetadata ::
    String ->
    LockedModuleEntry ->
    LockedModuleEntry ->
    [PackageLockViolation]
validateDependencyInterfaceMetadata moduleName locked current =
    [ DependencyInterfaceMetadataDrift moduleName dependency expected actual
    | dependency <- allDependencyInterfaceKeys
    , let expected = findDependencyInterfaceMetadata dependency (lockedModuleEntryDependencyInterfaces locked)
    , let actual = findDependencyInterfaceMetadata dependency (lockedModuleEntryDependencyInterfaces current)
    , expected /= actual
    ]
  where
    allDependencyInterfaceKeys =
        sort
            ( unique
                ( map (renderPackageModuleId . lockedDependencyInterfaceModuleId) (lockedModuleEntryDependencyInterfaces locked)
                    ++ map (renderPackageModuleId . lockedDependencyInterfaceModuleId) (lockedModuleEntryDependencyInterfaces current)
                )
            )

renderPackageEntries :: String -> [LockedPackageEntry] -> String
renderPackageEntries header entries =
    unlines $
        [ header
        , "packages:"
        ]
            ++ concatMap renderLockedPackageEntry (sortOn lockedPackageEntryLabel entries)

renderLockedPackageEntry :: LockedPackageEntry -> [String]
renderLockedPackageEntry entry =
    [ "  - package="
        ++ lockedPackageEntryLabel entry
        ++ " root="
        ++ maybe "<missing>" lockedPackageRootPath (lockedPackageEntryRoot entry)
        ++ " abi-version="
        ++ unPlatformAbiVersion (lockedPackageEntryRequiredAbiVersion entry)
        ++ " substrate-fingerprint="
        ++ unLockedSubstrateFingerprintMaterial (lockedPackageEntryRequiredSubstrateFingerprint entry)
    , "    modules:"
    ]
        ++ renderModuleItems (map renderLockedModuleEntry (sortOn (renderPackageModuleId . lockedModuleEntryId) (lockedPackageEntryModules entry)))

renderLockedModuleEntry :: LockedModuleEntry -> [String]
renderLockedModuleEntry moduleEntry =
    [ "module="
        ++ renderPackageModuleId (lockedModuleEntryId moduleEntry)
        ++ " source-metadata="
        ++ packageSourceMetadataFingerprint (lockedModuleEntrySourceMetadata moduleEntry)
        ++ " interface-metadata="
        ++ interfaceSummaryMetadataFingerprint (lockedModuleEntryInterfaceMetadata moduleEntry)
    , "direct-dependencies:"
    ]
        ++ renderDependencyItems (map renderPackageModuleId (sort (lockedModuleEntryDirectDependencies moduleEntry)))
        ++ ["dependency-interfaces:"]
        ++ renderDependencyItems
            ( map
                renderLockedDependencyInterface
                (sortOn (renderPackageModuleId . lockedDependencyInterfaceModuleId) (lockedModuleEntryDependencyInterfaces moduleEntry))
            )

renderLockedDependencyInterface :: LockedDependencyInterface -> String
renderLockedDependencyInterface dependency =
    "module="
        ++ renderPackageModuleId (lockedDependencyInterfaceModuleId dependency)
        ++ " metadata="
        ++ interfaceSummaryMetadataFingerprint (lockedDependencyInterfaceMetadata dependency)

renderModuleItems :: [[String]] -> [String]
renderModuleItems renderedModules =
    case renderedModules of
        [] -> ["    - <none>"]
        _ -> concatMap renderModuleItem renderedModules

renderModuleItem :: [String] -> [String]
renderModuleItem rendered =
    case rendered of
        [] -> []
        firstLine : rest ->
            ("    - " ++ firstLine) : map ("      " ++) rest

renderDependencyItems :: [String] -> [String]
renderDependencyItems values =
    case values of
        [] -> ["  - <none>"]
        _ -> map ("  - " ++) values

findDependencyInterfaceMetadata :: String -> [LockedDependencyInterface] -> Maybe InterfaceSummaryMetadata
findDependencyInterfaceMetadata dependencyName =
    fmap lockedDependencyInterfaceMetadata
        . find ((== dependencyName) . renderPackageModuleId . lockedDependencyInterfaceModuleId)

findCurrentPackage :: [CurrentPackageEntry] -> LockedPackageIdentity -> Maybe CurrentPackageEntry
findCurrentPackage entries identity =
    find ((== identity) . currentPackageEntryIdentity) entries

findCurrentModule :: [LockedModuleEntry] -> PackageModuleId -> Maybe LockedModuleEntry
findCurrentModule entries moduleId =
    find ((== moduleId) . lockedModuleEntryId) entries

hasLockedPackage :: [LockedPackageEntry] -> LockedPackageIdentity -> Bool
hasLockedPackage entries identity =
    any ((== Just identity) . lockedPackageEntryIdentity) entries

hasCurrentPackage :: [CurrentPackageEntry] -> LockedPackageEntry -> Bool
hasCurrentPackage entries entry =
    case lockedPackageEntryIdentity entry of
        Nothing -> False
        Just identity -> any ((== identity) . currentPackageEntryIdentity) entries

hasLockedModule :: [LockedModuleEntry] -> PackageModuleId -> Bool
hasLockedModule entries moduleId =
    any ((== moduleId) . lockedModuleEntryId) entries

hasCurrentModule :: [LockedModuleEntry] -> PackageModuleId -> Bool
hasCurrentModule =
    hasLockedModule

hasComparableLockedPackageIdentity :: LockedPackageEntry -> Bool
hasComparableLockedPackageIdentity entry =
    case lockedPackageEntryIdentity entry of
        Nothing -> False
        Just identity -> not (isBlank (lockedPackageIdentityKey identity))

lockedPackageKeys :: [LockedPackageEntry] -> [String]
lockedPackageKeys entries =
    [ lockedPackageIdentityKey identity
    | entry <- entries
    , Just identity <- [lockedPackageEntryIdentity entry]
    , not (isBlank (lockedPackageIdentityKey identity))
    ]

lockedPackageEntryLabel :: LockedPackageEntry -> String
lockedPackageEntryLabel entry =
    case lockedPackageEntryIdentity entry of
        Nothing -> "<missing>"
        Just identity -> lockedPackageIdentityKey identity

lockedPackageIdentityKey :: LockedPackageIdentity -> String
lockedPackageIdentityKey =
    packageIdName . lockedPackageIdentityPackageId

currentPackageKey :: CurrentPackageEntry -> String
currentPackageKey =
    lockedPackageIdentityKey . currentPackageEntryIdentity

renderPackageModuleId :: PackageModuleId -> String
renderPackageModuleId moduleId =
    packageIdName (packageModulePackageId moduleId) ++ ":" ++ packageModuleName moduleId

renderPackageModuleIds :: [PackageModuleId] -> String
renderPackageModuleIds =
    show . map renderPackageModuleId . sort

renderMaybeInterfaceMetadata :: Maybe InterfaceSummaryMetadata -> String
renderMaybeInterfaceMetadata value =
    case value of
        Nothing -> "<missing>"
        Just metadata -> interfaceSummaryMetadataFingerprint metadata

compareText :: (String -> String -> PackageLockViolation) -> String -> String -> [PackageLockViolation]
compareText mismatch expected actual =
    [mismatch expected actual | expected /= actual]

renderNamedValue :: String -> String
renderNamedValue value
    | isBlank value = "<blank>"
    | otherwise = value

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

unique :: [String] -> [String]
unique =
    map groupKey . group . sort
  where
    groupKey group0 =
        case group0 of
            key : _ -> key
            [] -> ""

isBlank :: String -> Bool
isBlank =
    all isSpace
