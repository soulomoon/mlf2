module MLF.Frontend.Program.BuildGraph
    ( PackageBuildGraph (..)
    , PackageBuildGraphNode (..)
    , PackageSourceMetadata (..)
    , InterfaceSummaryMetadata (..)
    , PackageBuildGraphError (..)
    , PackageBuildCacheEntry (..)
    , PackageBuildCacheValidationError (..)
    , programPackageBuildGraph
    , locatedProgramPackageBuildGraph
    , programPackageBuildGraphWithInterfaces
    , locatedProgramPackageBuildGraphWithInterfaces
    , packageBuildGraphNodeById
    , moduleInterfaceSummaryMetadata
    , packageBuildCacheEntryFromInterface
    , packageBuildCacheEntries
    , validatePackageBuildCacheEntry
    , validatePackageBuildCacheEntryFor
    , renderPackageBuildGraphError
    , renderPackageBuildCacheValidationError
    ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Frontend.Program.Interface
    ( ModuleInterface (..)
    , PackageInterface (..)
    , ProgramInterfaceError
    , packageInterfaceModuleById
    , renderProgramInterfaceError
    , validatePackageInterface
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage
    , PackageId (..)
    , PackageModuleGraph (..)
    , PackageModuleGraphNode (..)
    , PackageModuleId (..)
    , ProgramPackage (..)
    , ProgramSourceUnit (..)
    , locatedProgramPackagePackage
    , programPackageModuleGraph
    )
import MLF.Frontend.Program.Types (ProgramError (..))
import qualified MLF.Frontend.Syntax.Program as P

data PackageBuildGraph = PackageBuildGraph
    { packageBuildGraphNodes :: [PackageBuildGraphNode]
    , packageBuildGraphOrder :: [PackageModuleId]
    }
    deriving (Eq, Show)

data PackageBuildGraphNode = PackageBuildGraphNode
    { packageBuildGraphNodeId :: PackageModuleId
    , packageBuildGraphNodeSourcePath :: Maybe FilePath
    , packageBuildGraphNodeDirectImports :: [PackageModuleId]
    , packageBuildGraphNodeSourceMetadata :: PackageSourceMetadata
    , packageBuildGraphNodeDependencyInterfaceMetadata ::
        [(PackageModuleId, InterfaceSummaryMetadata)]
    }
    deriving (Eq, Show)

newtype PackageSourceMetadata = PackageSourceMetadata
    { packageSourceMetadataFingerprint :: String
    }
    deriving (Eq, Ord, Show)

newtype InterfaceSummaryMetadata = InterfaceSummaryMetadata
    { interfaceSummaryMetadataFingerprint :: String
    }
    deriving (Eq, Ord, Show)

data PackageBuildGraphError
    = PackageBuildGraphProgramError ProgramError
    | PackageBuildGraphInterfaceError ProgramInterfaceError
    deriving (Eq, Show)

data PackageBuildCacheEntry = PackageBuildCacheEntry
    { packageBuildCacheEntryModuleId :: PackageModuleId
    , packageBuildCacheEntrySourceMetadata :: PackageSourceMetadata
    , packageBuildCacheEntryDirectDependencies :: [PackageModuleId]
    , packageBuildCacheEntryDependencyInterfaceMetadata ::
        [(PackageModuleId, InterfaceSummaryMetadata)]
    , packageBuildCacheEntryInterfaceMetadata :: InterfaceSummaryMetadata
    }
    deriving (Eq, Show)

data PackageBuildCacheValidationError
    = PackageBuildCacheEntryModuleIdMismatch PackageModuleId PackageModuleId
    | PackageBuildCacheGraphNodeMissing PackageModuleId
    | PackageBuildCacheInterfaceMissing PackageModuleId
    | PackageBuildCacheSourceMetadataMismatch PackageModuleId PackageSourceMetadata PackageSourceMetadata
    | PackageBuildCacheDependenciesMismatch PackageModuleId [PackageModuleId] [PackageModuleId]
    | PackageBuildCacheDependencyInterfaceMetadataMismatch
        PackageModuleId
        PackageModuleId
        (Maybe InterfaceSummaryMetadata)
        (Maybe InterfaceSummaryMetadata)
    | PackageBuildCacheInterfaceMetadataMismatch
        PackageModuleId
        InterfaceSummaryMetadata
        InterfaceSummaryMetadata
    deriving (Eq, Show)

programPackageBuildGraph :: ProgramPackage -> Either ProgramError PackageBuildGraph
programPackageBuildGraph package = do
    moduleGraph <- programPackageModuleGraph package
    buildPackageBuildGraph package moduleGraph Nothing

locatedProgramPackageBuildGraph ::
    LocatedProgramPackage ->
    Either ProgramError PackageBuildGraph
locatedProgramPackageBuildGraph =
    programPackageBuildGraph . locatedProgramPackagePackage

programPackageBuildGraphWithInterfaces ::
    ProgramPackage ->
    PackageInterface ->
    Either PackageBuildGraphError PackageBuildGraph
programPackageBuildGraphWithInterfaces package packageInterface = do
    moduleGraph <-
        first PackageBuildGraphProgramError
            (programPackageModuleGraph package)
    first PackageBuildGraphInterfaceError $
        validatePackageInterface moduleGraph packageInterface
    first PackageBuildGraphProgramError $
        buildPackageBuildGraph package moduleGraph (Just packageInterface)

locatedProgramPackageBuildGraphWithInterfaces ::
    LocatedProgramPackage ->
    PackageInterface ->
    Either PackageBuildGraphError PackageBuildGraph
locatedProgramPackageBuildGraphWithInterfaces package =
    programPackageBuildGraphWithInterfaces (locatedProgramPackagePackage package)

packageBuildGraphNodeById ::
    PackageModuleId ->
    PackageBuildGraph ->
    Maybe PackageBuildGraphNode
packageBuildGraphNodeById moduleId graph =
    Map.lookup moduleId $
        Map.fromList
            [ (packageBuildGraphNodeId node, node)
            | node <- packageBuildGraphNodes graph
            ]

moduleInterfaceSummaryMetadata :: ModuleInterface -> InterfaceSummaryMetadata
moduleInterfaceSummaryMetadata interface =
    InterfaceSummaryMetadata
        ( show
            ( moduleInterfaceId interface
            , moduleInterfaceSourcePath interface
            , moduleInterfaceDependencies interface
            , moduleInterfaceExports interface
            , moduleInterfaceData interface
            , moduleInterfaceClasses interface
            , moduleInterfaceInstances interface
            )
        )

packageBuildCacheEntryFromInterface ::
    PackageBuildGraphNode ->
    ModuleInterface ->
    Either PackageBuildCacheValidationError PackageBuildCacheEntry
packageBuildCacheEntryFromInterface node interface = do
    unless (moduleInterfaceId interface == packageBuildGraphNodeId node) $
        Left
            ( PackageBuildCacheEntryModuleIdMismatch
                (packageBuildGraphNodeId node)
                (moduleInterfaceId interface)
            )
    pure
        PackageBuildCacheEntry
            { packageBuildCacheEntryModuleId = packageBuildGraphNodeId node
            , packageBuildCacheEntrySourceMetadata = packageBuildGraphNodeSourceMetadata node
            , packageBuildCacheEntryDirectDependencies = packageBuildGraphNodeDirectImports node
            , packageBuildCacheEntryDependencyInterfaceMetadata =
                packageBuildGraphNodeDependencyInterfaceMetadata node
            , packageBuildCacheEntryInterfaceMetadata =
                moduleInterfaceSummaryMetadata interface
            }

packageBuildCacheEntries ::
    PackageBuildGraph ->
    PackageInterface ->
    Either PackageBuildCacheValidationError [PackageBuildCacheEntry]
packageBuildCacheEntries graph packageInterface =
    traverse cacheEntryForNode (packageBuildGraphNodes graph)
  where
    cacheEntryForNode node = do
        interface <-
            maybe
                (Left (PackageBuildCacheInterfaceMissing (packageBuildGraphNodeId node)))
                Right
                (packageInterfaceModuleById (packageBuildGraphNodeId node) packageInterface)
        packageBuildCacheEntryFromInterface node interface

validatePackageBuildCacheEntry ::
    PackageBuildGraph ->
    PackageInterface ->
    PackageBuildCacheEntry ->
    Either PackageBuildCacheValidationError ()
validatePackageBuildCacheEntry graph packageInterface entry =
    validatePackageBuildCacheEntryFor
        (packageBuildCacheEntryModuleId entry)
        graph
        packageInterface
        entry

validatePackageBuildCacheEntryFor ::
    PackageModuleId ->
    PackageBuildGraph ->
    PackageInterface ->
    PackageBuildCacheEntry ->
    Either PackageBuildCacheValidationError ()
validatePackageBuildCacheEntryFor expectedModuleId graph packageInterface entry = do
    unless (packageBuildCacheEntryModuleId entry == expectedModuleId) $
        Left
            ( PackageBuildCacheEntryModuleIdMismatch
                expectedModuleId
                (packageBuildCacheEntryModuleId entry)
            )
    node <-
        maybe
            (Left (PackageBuildCacheGraphNodeMissing expectedModuleId))
            Right
            (packageBuildGraphNodeById expectedModuleId graph)
    interface <-
        maybe
            (Left (PackageBuildCacheInterfaceMissing expectedModuleId))
            Right
            (packageInterfaceModuleById expectedModuleId packageInterface)
    let currentSourceMetadata = packageBuildGraphNodeSourceMetadata node
        cachedSourceMetadata = packageBuildCacheEntrySourceMetadata entry
        currentDependencies = packageBuildGraphNodeDirectImports node
        cachedDependencies = packageBuildCacheEntryDirectDependencies entry
        currentDependencyMetadata =
            packageBuildGraphNodeDependencyInterfaceMetadata node
        cachedDependencyMetadata =
            packageBuildCacheEntryDependencyInterfaceMetadata entry
        currentInterfaceMetadata =
            moduleInterfaceSummaryMetadata interface
        cachedInterfaceMetadata =
            packageBuildCacheEntryInterfaceMetadata entry
    unless (cachedSourceMetadata == currentSourceMetadata) $
        Left
            ( PackageBuildCacheSourceMetadataMismatch
                expectedModuleId
                currentSourceMetadata
                cachedSourceMetadata
            )
    unless (cachedDependencies == currentDependencies) $
        Left
            ( PackageBuildCacheDependenciesMismatch
                expectedModuleId
                currentDependencies
                cachedDependencies
            )
    unless (cachedDependencyMetadata == currentDependencyMetadata) $
        let (dependency, current, cached) =
                firstDependencyMetadataMismatch
                    currentDependencyMetadata
                    cachedDependencyMetadata
         in Left
                ( PackageBuildCacheDependencyInterfaceMetadataMismatch
                    expectedModuleId
                    dependency
                    current
                    cached
                )
    unless (cachedInterfaceMetadata == currentInterfaceMetadata) $
        Left
            ( PackageBuildCacheInterfaceMetadataMismatch
                expectedModuleId
                currentInterfaceMetadata
                cachedInterfaceMetadata
            )

renderPackageBuildGraphError :: PackageBuildGraphError -> String
renderPackageBuildGraphError err =
    case err of
        PackageBuildGraphProgramError programErr ->
            show programErr
        PackageBuildGraphInterfaceError interfaceErr ->
            "invalid .mlfp interface artifact at package build graph boundary: "
                ++ renderProgramInterfaceError interfaceErr

renderPackageBuildCacheValidationError ::
    PackageBuildCacheValidationError ->
    String
renderPackageBuildCacheValidationError err =
    case err of
        PackageBuildCacheEntryModuleIdMismatch expected actual ->
            "package build cache entry module mismatch: expected "
                ++ renderPackageModuleId expected
                ++ ", got "
                ++ renderPackageModuleId actual
        PackageBuildCacheGraphNodeMissing moduleId ->
            "package build graph is missing module " ++ renderPackageModuleId moduleId
        PackageBuildCacheInterfaceMissing moduleId ->
            "package interface is missing module " ++ renderPackageModuleId moduleId
        PackageBuildCacheSourceMetadataMismatch moduleId _ _ ->
            "stale source metadata for " ++ renderPackageModuleId moduleId
        PackageBuildCacheDependenciesMismatch moduleId expected actual ->
            "stale direct dependencies for "
                ++ renderPackageModuleId moduleId
                ++ ": expected "
                ++ renderModuleIds expected
                ++ ", got "
                ++ renderModuleIds actual
        PackageBuildCacheDependencyInterfaceMetadataMismatch moduleId dependency _ _ ->
            "stale dependency interface metadata for "
                ++ renderPackageModuleId moduleId
                ++ " importing "
                ++ renderPackageModuleId dependency
        PackageBuildCacheInterfaceMetadataMismatch moduleId _ _ ->
            "stale interface summary metadata for " ++ renderPackageModuleId moduleId

buildPackageBuildGraph ::
    ProgramPackage ->
    PackageModuleGraph ->
    Maybe PackageInterface ->
    Either ProgramError PackageBuildGraph
buildPackageBuildGraph package moduleGraph mbInterface = do
    nodes <-
        traverse
            (buildNode nodesById sourceMetadataById)
            (packageModuleGraphOrder moduleGraph)
    pure
        PackageBuildGraph
            { packageBuildGraphNodes = nodes
            , packageBuildGraphOrder = packageModuleGraphOrder moduleGraph
            }
  where
    nodesById =
        Map.fromList
            [ (packageModuleGraphNodeId node, node)
            | node <- packageModuleGraphNodes moduleGraph
            ]
    sourceMetadataById =
        packageSourceMetadataById package

    buildNode nodesById0 sourceMetadataById0 moduleId = do
        node <-
            maybe
                (Left (ProgramPipelineError ("missing package graph node for " ++ renderPackageModuleId moduleId)))
                Right
                (Map.lookup moduleId nodesById0)
        sourceMetadata <-
            maybe
                (Left (ProgramPipelineError ("missing package source metadata for " ++ renderPackageModuleId moduleId)))
                Right
                (Map.lookup moduleId sourceMetadataById0)
        pure
            PackageBuildGraphNode
                { packageBuildGraphNodeId = moduleId
                , packageBuildGraphNodeSourcePath = packageModuleGraphNodeSourcePath node
                , packageBuildGraphNodeDirectImports = packageModuleGraphNodeImports node
                , packageBuildGraphNodeSourceMetadata = sourceMetadata
                , packageBuildGraphNodeDependencyInterfaceMetadata =
                    dependencyInterfaceMetadata node
                }

    dependencyInterfaceMetadata node =
        case mbInterface of
            Nothing -> []
            Just packageInterface ->
                [ (dependency, moduleInterfaceSummaryMetadata interface)
                | dependency <- packageModuleGraphOrder moduleGraph
                , dependency `Set.member` directDependencySet
                , Just interface <- [packageInterfaceModuleById dependency packageInterface]
                ]
      where
        directDependencySet = Set.fromList (packageModuleGraphNodeImports node)

packageSourceMetadataById :: ProgramPackage -> Map PackageModuleId PackageSourceMetadata
packageSourceMetadataById package =
    Map.fromList
        [ ( PackageModuleId (programPackageId package) (P.moduleName mod0)
          , sourceMetadataForModule mod0
          )
        | sourceUnit <- programPackageSourceUnits package
        , mod0 <- programSourceUnitModules sourceUnit
        ]

sourceMetadataForModule :: P.Module -> PackageSourceMetadata
sourceMetadataForModule =
    PackageSourceMetadata . show

firstDependencyMetadataMismatch ::
    [(PackageModuleId, InterfaceSummaryMetadata)] ->
    [(PackageModuleId, InterfaceSummaryMetadata)] ->
    (PackageModuleId, Maybe InterfaceSummaryMetadata, Maybe InterfaceSummaryMetadata)
firstDependencyMetadataMismatch current cached =
    case dropWhile samePair (zip current cached) of
        ((currentPair, cachedPair) : _) ->
            mismatchFromPairs currentMap cachedMap currentPair cachedPair
        [] ->
            case current of
                (dependency, metadata) : _ | dependency `Map.notMember` cachedMap ->
                    (dependency, Just metadata, Nothing)
                [] ->
                    case cached of
                        (dependency, metadata) : _ | dependency `Map.notMember` currentMap ->
                            (dependency, Nothing, Just metadata)
                        (dependency, metadata) : _ ->
                            (dependency, Map.lookup dependency currentMap, Just metadata)
                        [] ->
                            error "firstDependencyMetadataMismatch called with matching metadata"
                (dependency, metadata) : _ ->
                    (dependency, Just metadata, Map.lookup dependency cachedMap)
  where
    samePair (left, right) = left == right
    currentMap = Map.fromList current
    cachedMap = Map.fromList cached

    mismatchFromPairs currentMap0 cachedMap0 (currentDependency, currentMetadata) (cachedDependency, cachedMetadata)
        | currentDependency == cachedDependency =
            (currentDependency, Just currentMetadata, Just cachedMetadata)
        | otherwise =
            ( currentDependency
            , Just currentMetadata
            , Map.lookup currentDependency cachedMap0
            )
              `preferNamedDependency`
                ( cachedDependency
                , Map.lookup cachedDependency currentMap0
                , Just cachedMetadata
                )

    preferNamedDependency left@(_, Just _, Just _) _ = left
    preferNamedDependency _ right = right

renderModuleIds :: [PackageModuleId] -> String
renderModuleIds =
    ("[" ++) . (++ "]") . intercalate ", " . map renderPackageModuleId

renderPackageModuleId :: PackageModuleId -> String
renderPackageModuleId moduleId =
    packageIdName (packageModulePackageId moduleId) ++ ":" ++ packageModuleName moduleId
