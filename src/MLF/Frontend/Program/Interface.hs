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
    , SymbolIdentity (..)
    , SymbolNamespace (..)
    , SymbolOwnerIdentity (..)
    , classInfoSymbolIdentity
    , constructorInfoSymbolIdentity
    , dataInfoSymbolIdentity
    , instanceInfoClassSymbolIdentity
    , methodInfoOwnerClassSymbolIdentity
    , methodInfoSymbolIdentity
    , valueInfoSymbolIdentity
    )
import qualified MLF.Frontend.Syntax.Program as P

data ModuleInterface = ModuleInterface
    { moduleInterfaceId :: PackageModuleId
    , moduleInterfaceSourcePath :: Maybe FilePath
    , moduleInterfaceDependencies :: [PackageModuleId]
    , moduleInterfaceExports :: ModuleExports
    , moduleInterfaceData :: Map String DataInfo
    , moduleInterfaceClasses :: Map String ClassInfo
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
    | ProgramInterfaceCheckedModuleMismatch PackageModuleId P.ModuleName
    | ProgramInterfaceSourcePathMismatch PackageModuleId (Maybe FilePath) (Maybe FilePath)
    | ProgramInterfaceDependenciesMismatch PackageModuleId [PackageModuleId] [PackageModuleId]
    | ProgramInterfaceExportOwnerMismatch PackageModuleId SymbolIdentity
    | ProgramInterfaceExportConstructorOwnerMismatch PackageModuleId SymbolIdentity SymbolIdentity
    | ProgramInterfaceClassMethodOwnerMismatch PackageModuleId SymbolIdentity SymbolIdentity
    | ProgramInterfaceInstanceOriginMismatch PackageModuleId P.ModuleName
    deriving (Eq, Show)

moduleInterfaceFromCheckedModule ::
    PackageModuleGraphNode ->
    CheckedModule ->
    Either ProgramInterfaceError ModuleInterface
moduleInterfaceFromCheckedModule node checked = do
    let moduleId = packageModuleGraphNodeId node
        moduleName0 = packageModuleName moduleId
    unless (checkedModuleName checked == moduleName0) $
        Left (ProgramInterfaceCheckedModuleMismatch moduleId (checkedModuleName checked))
    let interface =
            ModuleInterface
                { moduleInterfaceId = moduleId
                , moduleInterfaceSourcePath = packageModuleGraphNodeSourcePath node
                , moduleInterfaceDependencies = packageModuleGraphNodeImports node
                , moduleInterfaceExports = checkedModuleExports checked
                , moduleInterfaceData = checkedModuleData checked
                , moduleInterfaceClasses = checkedModuleClasses checked
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
    checkedByModule =
        Map.fromList
            [ (checkedModuleName checkedModule, checkedModule)
            | checkedModule <- checkedProgramModules checked
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
                (Map.lookup (packageModuleName moduleId) checkedByModule)
        moduleInterfaceFromCheckedModule node checkedModule

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
    forM_ (Map.elems (exportedValues exports0)) validateValue
    forM_ (Map.elems (exportedTypes exports0)) validateExportedType
    forM_ (Map.elems (exportedClasses exports0)) validateClass
    forM_ (moduleInterfaceInstances interface) validateInstance
  where
    moduleId = moduleInterfaceId interface
    moduleName0 = packageModuleName moduleId
    exports0 = moduleInterfaceExports interface

    validateValue valueInfo =
        requireIdentityDefinedHere moduleId moduleName0 (valueInfoSymbolIdentity valueInfo)

    validateExportedType typeInfo = do
        let dataInfo = exportedTypeData typeInfo
            dataIdentity = dataInfoSymbolIdentity dataInfo
        requireIdentityDefinedHere moduleId moduleName0 dataIdentity
        forM_ (Map.elems (exportedTypeConstructors typeInfo)) $ \ctorInfo -> do
            let ctorIdentity = constructorInfoSymbolIdentity dataInfo ctorInfo
            requireIdentityDefinedHere moduleId moduleName0 ctorIdentity
            unless (ctorOwningTypeIdentity ctorInfo == dataIdentity) $
                Left
                    ( ProgramInterfaceExportConstructorOwnerMismatch
                        moduleId
                        dataIdentity
                        (ctorOwningTypeIdentity ctorInfo)
                    )
            unless (symbolOwnerIdentity ctorIdentity == Just (SymbolOwnerType moduleName0 (symbolDefiningName dataIdentity))) $
                Left
                    ( ProgramInterfaceExportConstructorOwnerMismatch
                        moduleId
                        dataIdentity
                        ctorIdentity
                    )

    validateClass classInfo = do
        let classIdentity = classInfoSymbolIdentity classInfo
        requireIdentityDefinedHere moduleId moduleName0 classIdentity
        forM_ (Map.elems (classMethods classInfo)) $ \methodInfo -> do
            let methodIdentity = methodInfoSymbolIdentity methodInfo
                ownerIdentity = methodInfoOwnerClassSymbolIdentity methodInfo
            requireIdentityDefinedHere moduleId moduleName0 methodIdentity
            unless (ownerIdentity == classIdentity) $
                Left
                    ( ProgramInterfaceClassMethodOwnerMismatch
                        moduleId
                        classIdentity
                        ownerIdentity
                    )

    validateInstance instanceInfo = do
        unless (instanceOriginModule instanceInfo == moduleName0) $
            Left (ProgramInterfaceInstanceOriginMismatch moduleId (instanceOriginModule instanceInfo))
        requireClassIdentity (instanceInfoClassSymbolIdentity instanceInfo)

    requireClassIdentity identity =
        unless (symbolNamespace identity == SymbolClass) $
            Left (ProgramInterfaceExportOwnerMismatch moduleId identity)

requireIdentityDefinedHere ::
    PackageModuleId ->
    P.ModuleName ->
    SymbolIdentity ->
    Either ProgramInterfaceError ()
requireIdentityDefinedHere moduleId moduleName0 identity =
    unless (identityDefinedHere || preludeBuiltinIdentity) $
        Left (ProgramInterfaceExportOwnerMismatch moduleId identity)
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
                ++ actualModule
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
        ProgramInterfaceInstanceOriginMismatch moduleId actualModule ->
            "instance origin mismatch for "
                ++ renderPackageModuleId moduleId
                ++ ": got "
                ++ actualModule

renderPackageModuleId :: PackageModuleId -> String
renderPackageModuleId moduleId =
    packageIdName (packageModulePackageId moduleId) ++ ":" ++ packageModuleName moduleId
