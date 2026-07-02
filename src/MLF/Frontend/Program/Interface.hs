module MLF.Frontend.Program.Interface
    ( ModuleInterface (..)
    , PackageInterface (..)
    , ProgramInterfaceError (..)
    , moduleInterfaceFromCheckedModule
    , packageInterfaceFromCheckedProgram
    , packageInterfaceModuleById
    , validatePackageInterface
    , renderProgramInterfaceError
    ) where

import Control.Monad (forM_, unless, when)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Frontend.Symbol (symbolIdentityStableName)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleGraph (..)
    , PackageModuleGraphNode (..)
    , PackageModuleId (..)
    )
import MLF.Frontend.Program.Types
    ( CheckedModule (..)
    , CheckedProgram (..)
    , ClassInfo (..)
    , ConstructorInfo (..)
    , DataInfo (..)
    , ExportedTypeInfo (..)
    , InstanceInfo (..)
    , ModuleExports (..)
    , SymbolIdentity
    , SymbolNamespace (..)
    , SymbolOwnerIdentity (..)
    , classInfoSymbolIdentity
    , constructorInfoSymbolIdentity
    , dataInfoSymbolIdentity
    , instanceInfoClassSymbolIdentity
    , methodInfoOwnerClassSymbolIdentity
    , methodInfoSymbolIdentity
    , symbolDefiningModule
    , symbolDefiningName
    , symbolNamespace
    , symbolOwnerIdentity
    , valueInfoSymbolIdentity
    )
import qualified MLF.Frontend.Syntax.Program as P

data ModuleInterface = ModuleInterface
    { moduleInterfaceId :: PackageModuleId
    , moduleInterfaceIdentity :: SymbolIdentity
    , moduleInterfaceSourcePath :: Maybe FilePath
    , moduleInterfaceDependencies :: [PackageModuleId]
    , moduleInterfaceExports :: ModuleExports
    , moduleInterfaceDataByIdentity :: Map SymbolIdentity DataInfo
    , moduleInterfaceClassesByIdentity :: Map SymbolIdentity ClassInfo
    , moduleInterfaceInstances :: [InstanceInfo]
    }
    deriving (Eq, Show)

newtype PackageInterface = PackageInterface
    { packageInterfaceModules :: [ModuleInterface]
    }
    deriving (Eq, Show)

data ProgramInterfaceError
    = ProgramInterfaceModuleMissing PackageModuleId
    | ProgramInterfaceUnexpectedModule PackageModuleId
    | ProgramInterfaceModuleOrderMismatch [PackageModuleId] [PackageModuleId]
    | ProgramInterfaceCheckedModuleMismatch PackageModuleId SymbolIdentity
    | ProgramInterfaceSourcePathMismatch PackageModuleId (Maybe FilePath) (Maybe FilePath)
    | ProgramInterfaceDependenciesMismatch PackageModuleId [PackageModuleId] [PackageModuleId]
    | ProgramInterfaceExportOwnerMismatch PackageModuleId SymbolIdentity
    | ProgramInterfaceIdentityKeyMismatch PackageModuleId SymbolIdentity SymbolIdentity
    | ProgramInterfaceIdentityKeySetMismatch PackageModuleId [SymbolIdentity] [SymbolIdentity]
    | ProgramInterfaceDuplicateDisplayName PackageModuleId String [SymbolIdentity]
    | ProgramInterfaceDuplicateMetadataIdentity PackageModuleId String SymbolIdentity
    | ProgramInterfaceExportConstructorOwnerMismatch PackageModuleId SymbolIdentity SymbolIdentity
    | ProgramInterfaceClassMethodOwnerMismatch PackageModuleId SymbolIdentity SymbolIdentity
    | ProgramInterfaceInstanceOriginMismatch PackageModuleId SymbolIdentity
    deriving (Eq, Show)

moduleInterfaceFromCheckedModule ::
    PackageModuleGraphNode ->
    CheckedModule ->
    Either ProgramInterfaceError ModuleInterface
moduleInterfaceFromCheckedModule node checked = do
    let moduleId = packageModuleGraphNodeId node
    unless (checkedModuleMatchesId moduleId checked) $
        Left (ProgramInterfaceCheckedModuleMismatch moduleId (checkedModuleIdentity checked))
    let interface =
            ModuleInterface
                { moduleInterfaceId = moduleId
                , moduleInterfaceIdentity = checkedModuleIdentity checked
                , moduleInterfaceSourcePath = packageModuleGraphNodeSourcePath node
                , moduleInterfaceDependencies = packageModuleGraphNodeImports node
                , moduleInterfaceExports = checkedModuleExports checked
                , moduleInterfaceDataByIdentity = checkedModuleData checked
                , moduleInterfaceClassesByIdentity = checkedModuleClasses checked
                , moduleInterfaceInstances = checkedModuleInstances checked
                }
    validateModuleInterface interface
    pure interface

packageInterfaceFromCheckedProgram ::
    PackageModuleGraph ->
    CheckedProgram ->
    Either ProgramInterfaceError PackageInterface
packageInterfaceFromCheckedProgram graph checked = do
    interfaces <-
        mapM
            moduleInterfaceForGraphId
            (packageModuleGraphOrder graph)
    let packageInterface = PackageInterface interfaces
    validatePackageInterface graph packageInterface
    pure packageInterface
  where
    nodesById =
        Map.fromList
            [ (packageModuleGraphNodeId node, node)
            | node <- packageModuleGraphNodes graph
            ]
    moduleInterfaceForGraphId moduleId = do
        node <-
            maybe
                (Left (ProgramInterfaceModuleMissing moduleId))
                Right
                (Map.lookup moduleId nodesById)
        checkedModule <-
            maybe
                (Left (ProgramInterfaceModuleMissing moduleId))
                Right
                (find (checkedModuleMatchesId moduleId) (checkedProgramModules checked))
        moduleInterfaceFromCheckedModule node checkedModule

checkedModuleMatchesId :: PackageModuleId -> CheckedModule -> Bool
checkedModuleMatchesId moduleId checked =
    symbolNamespace identity == SymbolModule
        && symbolDefiningModule identity == moduleName0
        && symbolDefiningName identity == moduleName0
  where
    identity = checkedModuleIdentity checked
    moduleName0 = packageModuleName moduleId

packageInterfaceModuleById :: PackageModuleId -> PackageInterface -> Maybe ModuleInterface
packageInterfaceModuleById moduleId =
    find ((== moduleId) . moduleInterfaceId) . packageInterfaceModules

validatePackageInterface ::
    PackageModuleGraph ->
    PackageInterface ->
    Either ProgramInterfaceError ()
validatePackageInterface graph packageInterface = do
    forM_ expectedIds $ \moduleId ->
        when (moduleId `Set.notMember` actualIdSet) $
            Left (ProgramInterfaceModuleMissing moduleId)
    forM_ actualIds $ \moduleId ->
        when (moduleId `Set.notMember` expectedIdSet) $
            Left (ProgramInterfaceUnexpectedModule moduleId)
    unless (actualIds == expectedIds) $
        Left (ProgramInterfaceModuleOrderMismatch expectedIds actualIds)
    requireUniqueModuleIdentities (packageInterfaceModules packageInterface)
    forM_ (packageInterfaceModules packageInterface) $ \interface -> do
        node <-
            maybe
                (Left (ProgramInterfaceUnexpectedModule (moduleInterfaceId interface)))
                Right
                (Map.lookup (moduleInterfaceId interface) expectedNodesById)
        validateModuleAgainstGraph node interface
        validateModuleInterface interface
        forM_ (moduleInterfaceDependencies interface) $ \dependency ->
            when (dependency `Set.notMember` actualIdSet) $
                Left (ProgramInterfaceModuleMissing dependency)
  where
    expectedIds = packageModuleGraphOrder graph
    actualIds = map moduleInterfaceId (packageInterfaceModules packageInterface)
    expectedIdSet = Set.fromList expectedIds
    actualIdSet = Set.fromList actualIds
    expectedNodesById =
        Map.fromList
            [ (packageModuleGraphNodeId node, node)
            | node <- packageModuleGraphNodes graph
            ]

requireUniqueModuleIdentities :: [ModuleInterface] -> Either ProgramInterfaceError ()
requireUniqueModuleIdentities =
    go Set.empty
  where
    go _ [] = Right ()
    go seen (interface : rest)
        | identity `Set.member` seen =
            Left (ProgramInterfaceDuplicateMetadataIdentity (moduleInterfaceId interface) "module" identity)
        | otherwise =
            go (Set.insert identity seen) rest
      where
        identity = moduleInterfaceIdentity interface

validateModuleAgainstGraph ::
    PackageModuleGraphNode ->
    ModuleInterface ->
    Either ProgramInterfaceError ()
validateModuleAgainstGraph node interface = do
    let moduleId = moduleInterfaceId interface
        expectedSourcePath = packageModuleGraphNodeSourcePath node
        actualSourcePath = moduleInterfaceSourcePath interface
        expectedDependencies = packageModuleGraphNodeImports node
        actualDependencies = moduleInterfaceDependencies interface
    unless (actualSourcePath == expectedSourcePath) $
        Left (ProgramInterfaceSourcePathMismatch moduleId expectedSourcePath actualSourcePath)
    unless (actualDependencies == expectedDependencies) $
        Left (ProgramInterfaceDependenciesMismatch moduleId expectedDependencies actualDependencies)

validateModuleInterface :: ModuleInterface -> Either ProgramInterfaceError ()
validateModuleInterface interface = do
    requireIdentityDisplayMap moduleId (exportedValuesByIdentity exports0) (exportedValueDisplaysByIdentity exports0)
    requireIdentityDisplayMap moduleId (exportedTypesByIdentity exports0) (exportedTypeDisplaysByIdentity exports0)
    requireIdentityDisplayMap moduleId (exportedClassesByIdentity exports0) (exportedClassDisplaysByIdentity exports0)
    validateModuleIdentity (moduleInterfaceIdentity interface)
    forM_ (Map.toList (moduleInterfaceDataByIdentity interface)) $ \(identity, dataInfo) -> do
        requireIdentityKey moduleId identity (dataInfoSymbolIdentity dataInfo)
        validateData dataInfo
    forM_ (Map.toList (moduleInterfaceClassesByIdentity interface)) $ \(identity, classInfo) -> do
        requireIdentityKey moduleId identity (classInfoSymbolIdentity classInfo)
        validateClass classInfo
    forM_ (Map.toList (exportedValuesByIdentity exports0)) $ \(identity, valueInfo) -> do
        requireIdentityKey moduleId identity (valueInfoSymbolIdentity valueInfo)
        validateValue valueInfo
    forM_ (Map.toList (exportedTypesByIdentity exports0)) $ \(identity, typeInfo) -> do
        requireIdentityKey moduleId identity (dataInfoSymbolIdentity (exportedTypeData typeInfo))
        validateExportedType typeInfo
    forM_ (Map.toList (exportedClassesByIdentity exports0)) $ \(identity, classInfo) -> do
        requireIdentityKey moduleId identity (classInfoSymbolIdentity classInfo)
        validateClass classInfo
    forM_ (moduleInterfaceInstances interface) validateInstance
  where
    moduleId = moduleInterfaceId interface
    moduleName0 = packageModuleName moduleId
    exports0 = moduleInterfaceExports interface

    validateModuleIdentity identity =
        unless
            (symbolNamespace identity == SymbolModule && symbolDefiningModule identity == moduleName0 && symbolDefiningName identity == moduleName0)
            (Left (ProgramInterfaceExportOwnerMismatch moduleId identity))

    validateValue valueInfo =
        requireIdentityDefinedHere moduleId moduleName0 (valueInfoSymbolIdentity valueInfo)

    validateData dataInfo = do
        let dataIdentity = dataInfoSymbolIdentity dataInfo
        requireUniqueIdentities moduleId "constructor" (map ctorInfoSymbol (dataConstructors dataInfo))
        requireIdentityDefinedHere moduleId moduleName0 dataIdentity
        forM_ (dataConstructors dataInfo) $ \ctorInfo -> do
            let ctorIdentity = constructorInfoSymbolIdentity dataInfo ctorInfo
            requireIdentityDefinedHere moduleId moduleName0 ctorIdentity
            unless (ctorOwningTypeIdentity ctorInfo == dataIdentity) $
                Left
                    ( ProgramInterfaceExportConstructorOwnerMismatch
                        moduleId
                        dataIdentity
                        (ctorOwningTypeIdentity ctorInfo)
                    )

    validateExportedType typeInfo = do
        let dataInfo = exportedTypeData typeInfo
            dataIdentity = dataInfoSymbolIdentity dataInfo
        requireIdentityDisplayMap
            moduleId
            (exportedTypeConstructorsByIdentity typeInfo)
            (exportedTypeConstructorDisplaysByIdentity typeInfo)
        requireIdentityDefinedHere moduleId moduleName0 dataIdentity
        forM_ (Map.toList (exportedTypeConstructorsByIdentity typeInfo)) $ \(identity, ctorInfo) -> do
            let ctorIdentity = constructorInfoSymbolIdentity dataInfo ctorInfo
            requireIdentityKey moduleId identity ctorIdentity
            requireIdentityDefinedHere moduleId moduleName0 ctorIdentity
            unless (ctorOwningTypeIdentity ctorInfo == dataIdentity) $
                Left
                    ( ProgramInterfaceExportConstructorOwnerMismatch
                        moduleId
                        dataIdentity
                        (ctorOwningTypeIdentity ctorInfo)
                    )
            unless (symbolOwnerIdentity ctorIdentity == Just (SymbolOwnerType dataIdentity)) $
                Left
                    ( ProgramInterfaceExportConstructorOwnerMismatch
                        moduleId
                        dataIdentity
                        ctorIdentity
                    )

    validateClass classInfo = do
        let classIdentity = classInfoSymbolIdentity classInfo
        requireIdentityDefinedHere moduleId moduleName0 classIdentity
        forM_ (Map.toList (classMethodsByIdentity classInfo)) $ \(identity, methodInfo) -> do
            let methodIdentity = methodInfoSymbolIdentity methodInfo
                ownerIdentity = methodInfoOwnerClassSymbolIdentity methodInfo
            requireIdentityKey moduleId identity methodIdentity
            requireIdentityDefinedHere moduleId moduleName0 methodIdentity
            unless (ownerIdentity == classIdentity) $
                Left
                    ( ProgramInterfaceClassMethodOwnerMismatch
                        moduleId
                        classIdentity
                        ownerIdentity
                    )

    validateInstance instanceInfo = do
        unless (instanceOriginModuleIdentity instanceInfo == moduleInterfaceIdentity interface) $
            Left (ProgramInterfaceInstanceOriginMismatch moduleId (instanceOriginModuleIdentity instanceInfo))
        requireClassIdentity (instanceInfoClassSymbolIdentity instanceInfo)

    requireClassIdentity identity =
        unless (symbolNamespace identity == SymbolClass) $
            Left (ProgramInterfaceExportOwnerMismatch moduleId identity)

requireIdentityKey :: PackageModuleId -> SymbolIdentity -> SymbolIdentity -> Either ProgramInterfaceError ()
requireIdentityKey moduleId key payload =
    unless (key == payload) $
        Left (ProgramInterfaceIdentityKeyMismatch moduleId key payload)

requireIdentityKeySet :: PackageModuleId -> Set.Set SymbolIdentity -> Set.Set SymbolIdentity -> Either ProgramInterfaceError ()
requireIdentityKeySet moduleId expected actual =
    unless (expected == actual) $
        Left (ProgramInterfaceIdentityKeySetMismatch moduleId (Set.toList expected) (Set.toList actual))

requireIdentityDisplayMap :: PackageModuleId -> Map SymbolIdentity a -> Map SymbolIdentity String -> Either ProgramInterfaceError ()
requireIdentityDisplayMap moduleId values displays = do
    requireIdentityKeySet moduleId (Map.keysSet values) (Map.keysSet displays)
    requireDistinctDisplayNames moduleId displays

requireDistinctDisplayNames :: PackageModuleId -> Map SymbolIdentity String -> Either ProgramInterfaceError ()
requireDistinctDisplayNames moduleId displays =
    case find ((> 1) . Set.size . snd) (Map.toList identitiesByDisplay) of
        Just (displayName, identities) ->
            Left (ProgramInterfaceDuplicateDisplayName moduleId displayName (Set.toList identities))
        Nothing -> Right ()
  where
    identitiesByDisplay =
        Map.fromListWith
            Set.union
            [ (displayName, Set.singleton identity)
            | (identity, displayName) <- Map.toList displays
            ]

requireUniqueIdentities :: PackageModuleId -> String -> [SymbolIdentity] -> Either ProgramInterfaceError ()
requireUniqueIdentities moduleId label identities =
    case find ((> 1) . snd) (Map.toList countsByIdentity) of
        Just (identity, _) ->
            Left (ProgramInterfaceDuplicateMetadataIdentity moduleId label identity)
        Nothing -> Right ()
  where
    countsByIdentity =
        Map.fromListWith
            (+)
            [(identity, 1 :: Int) | identity <- identities]

requireIdentityDefinedHere ::
    PackageModuleId ->
    P.ModuleName ->
    SymbolIdentity ->
    Either ProgramInterfaceError ()
requireIdentityDefinedHere moduleId moduleName0 identity =
    unless
        (identityDefinedHere || preludeBuiltinIdentity)
        (Left (ProgramInterfaceExportOwnerMismatch moduleId identity))
  where
    identityDefinedHere = symbolDefiningModule identity == moduleName0
    preludeBuiltinIdentity =
        moduleName0 == "Prelude"
            && symbolDefiningModule identity == Builtins.builtinModuleName

renderProgramInterfaceError :: ProgramInterfaceError -> String
renderProgramInterfaceError err =
    case err of
        ProgramInterfaceModuleMissing moduleId ->
            "missing interface for " ++ renderPackageModuleId moduleId
        ProgramInterfaceUnexpectedModule moduleId ->
            "unexpected interface for " ++ renderPackageModuleId moduleId
        ProgramInterfaceModuleOrderMismatch expected actual ->
            "interface module order mismatch: expected "
                ++ show (map renderPackageModuleId expected)
                ++ ", got "
                ++ show (map renderPackageModuleId actual)
        ProgramInterfaceCheckedModuleMismatch moduleId actualModule ->
            "checked module mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": got "
                ++ symbolDefiningName actualModule
        ProgramInterfaceSourcePathMismatch moduleId expected actual ->
            "source path mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": expected "
                ++ show expected
                ++ ", got "
                ++ show actual
        ProgramInterfaceDependenciesMismatch moduleId expected actual ->
            "dependency mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": expected "
                ++ show (map renderPackageModuleId expected)
                ++ ", got "
                ++ show (map renderPackageModuleId actual)
        ProgramInterfaceExportOwnerMismatch moduleId identity ->
            "export owner mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": "
                ++ show identity
        ProgramInterfaceIdentityKeyMismatch moduleId key payload ->
            "identity key mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": key "
                ++ show key
                ++ ", payload "
                ++ show payload
        ProgramInterfaceIdentityKeySetMismatch moduleId expected actual ->
            "identity key set mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": expected "
                ++ show expected
                ++ ", got "
                ++ show actual
        ProgramInterfaceDuplicateDisplayName moduleId displayName identities ->
            "duplicate interface display name for "
                ++ renderPackageModuleId moduleId
                ++ ": "
                ++ show displayName
                ++ " maps to "
                ++ show identities
        ProgramInterfaceDuplicateMetadataIdentity moduleId label identity ->
            "duplicate interface "
                ++ label
                ++ " identity for "
                ++ renderPackageModuleId moduleId
                ++ ": "
                ++ symbolIdentityStableName identity
        ProgramInterfaceExportConstructorOwnerMismatch moduleId expected actual ->
            "constructor owner mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": expected "
                ++ show expected
                ++ ", got "
                ++ show actual
        ProgramInterfaceClassMethodOwnerMismatch moduleId expected actual ->
            "class method owner mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": expected "
                ++ show expected
                ++ ", got "
                ++ show actual
        ProgramInterfaceInstanceOriginMismatch moduleId actualIdentity ->
            "instance origin mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": got "
                ++ show actualIdentity

renderPackageModuleId :: PackageModuleId -> String
renderPackageModuleId moduleId =
    packageIdName (packageModulePackageId moduleId) ++ ":" ++ packageModuleName moduleId
