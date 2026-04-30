{-# LANGUAGE GADTs #-}

module MLF.Program.CLI
    ( programCliUsage
    , emitBackendFile
    , emitNativeFile
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MLF.Backend.LLVM
    ( renderBackendLLVMError
    , renderCheckedProgramLLVM
    , renderCheckedProgramNativeLLVM
    )
import MLF.Elab.Types (ElabTerm (..))
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check (checkLocatedProgram)
import MLF.Frontend.Program.Run
    ( programRunOutput
    , runLocatedProgramOutput
    )
import MLF.Frontend.Program.Prelude (withPreludeLocated)
import MLF.Frontend.Program.Types
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    , ConstructorInfo (..)
    , DataInfo (..)
    , ResolvedModule (..)
    , ResolvedProgram (..)
    , ResolvedReference (..)
    , ResolvedReferenceKind (..)
    , ResolvedSymbol (..)
    , SymbolIdentity (..)
    , SymbolNamespace (..)
    , SymbolOwnerIdentity (..)
    , renderProgramDiagnostic
    )
import MLF.Frontend.Syntax (SrcBound (..), SrcType, SrcTy (..))

programCliUsage :: String
programCliUsage =
    unlines
        [ "Usage:"
        , "  mlf2 run-program <file.mlfp>"
        , "  mlf2 emit-backend <file.mlfp>"
        , "  mlf2 emit-native <file.mlfp>"
        , ""
        , "run-program prepends the built-in Prelude and prints the pure result or IO stdout."
        , "emit-backend prepends the built-in Prelude and prints LLVM IR."
        , "emit-native prepends the built-in Prelude and prints LLVM IR with a native process entrypoint."
        ]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        first renderProgramDiagnostic (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated program))

emitBackendFile :: FilePath -> IO (Either String String)
emitBackendFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        checked <- first renderProgramDiagnostic (checkLocatedProgram (withPreludeLocated program))
        first renderBackendLLVMError (renderCheckedProgramLLVM (emitBackendCheckedProgram checked))

emitNativeFile :: FilePath -> IO (Either String String)
emitNativeFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        checked <- first renderProgramDiagnostic (checkLocatedProgram (withPreludeLocated program))
        first renderBackendLLVMError (renderCheckedProgramNativeLLVM (emitBackendCheckedProgram checked))

emitBackendCheckedProgram :: CheckedProgram -> CheckedProgram
emitBackendCheckedProgram checked =
    checked {checkedProgramModules = map (emitBackendModule retainedPreludeBindings retainedPreludeData) modules0}
  where
    modules0 = checkedProgramModules checked
    retainedPreludeBindings = preludeBindingDependencyClosure modules0
    retainedPreludeData = preludeDataDependencyClosure checked retainedPreludeBindings

emitBackendModule :: Set String -> Set SymbolIdentity -> CheckedModule -> CheckedModule
emitBackendModule retainedPreludeBindings retainedPreludeData checkedModule
    | checkedModuleName checkedModule == "Prelude" =
        checkedModule
            { checkedModuleBindings =
                filter
                    ((`Set.member` retainedPreludeBindings) . checkedBindingName)
                    (checkedModuleBindings checkedModule)
            , checkedModuleData =
                Map.filter
                    ((`Set.member` retainedPreludeData) . dataInfoSymbol)
                    (checkedModuleData checkedModule)
            }
    | otherwise = checkedModule

preludeBindingDependencyClosure :: [CheckedModule] -> Set String
preludeBindingDependencyClosure modules0 =
    close (referencedBindingNames nonPreludeBindings) Set.empty
  where
    preludeBindingsByName =
        Map.fromList
            [ (checkedBindingName binding, binding)
            | binding <- preludeBindings
            ]

    preludeBindings =
        [ binding
        | checkedModule <- modules0
        , checkedModuleName checkedModule == "Prelude"
        , binding <- checkedModuleBindings checkedModule
        ]

    nonPreludeBindings =
        [ binding
        | checkedModule <- modules0
        , checkedModuleName checkedModule /= "Prelude"
        , binding <- checkedModuleBindings checkedModule
        ]

    close pending retained =
        case Set.minView (pendingPreludeBindings pending retained) of
            Nothing -> retained
            Just (name, pendingRest) ->
                case Map.lookup name preludeBindingsByName of
                    Nothing -> close pendingRest retained
                    Just binding ->
                        close
                            (Set.union pendingRest (referencedBindingNames [binding]))
                            (Set.insert name retained)

    pendingPreludeBindings pending retained =
        (pending `Set.intersection` Map.keysSet preludeBindingsByName)
            `Set.difference` retained

preludeDataDependencyClosure :: CheckedProgram -> Set String -> Set SymbolIdentity
preludeDataDependencyClosure checked retainedPreludeBindings =
    close initialData Set.empty
  where
    modules0 = checkedProgramModules checked
    preludeData =
        [ dataInfo
        | checkedModule <- modules0
        , checkedModuleName checkedModule == "Prelude"
        , dataInfo <- Map.elems (checkedModuleData checkedModule)
        ]

    preludeBindings =
        [ binding
        | checkedModule <- modules0
        , checkedModuleName checkedModule == "Prelude"
        , binding <- checkedModuleBindings checkedModule
        ]

    preludeDataByIdentity =
        Map.fromList [(dataInfoSymbol dataInfo, dataInfo) | dataInfo <- preludeData]

    preludeDataByName =
        Map.fromList [(dataName dataInfo, dataInfoSymbol dataInfo) | dataInfo <- preludeData]

    preludeDataByConstructorBinding =
        Map.fromList
            [ (ctorRuntimeName constructorInfo, dataInfoSymbol dataInfo)
            | dataInfo <- preludeData
            , constructorInfo <- dataConstructors dataInfo
            ]

    initialData =
        Set.unions
            [ referencedPreludeData (checkedProgramResolved checked) preludeDataByName
            , retainedPreludeBindingData preludeDataByName preludeBindings retainedPreludeBindings
            , Set.fromList
                [ dataIdentity
                | bindingName <- Set.toList retainedPreludeBindings
                , Just dataIdentity <- [Map.lookup bindingName preludeDataByConstructorBinding]
                ]
            ]

    close pending retained =
        case Set.minView (pendingPreludeData pending retained) of
            Nothing -> retained
            Just (dataIdentity, pendingRest) ->
                case Map.lookup dataIdentity preludeDataByIdentity of
                    Nothing -> close pendingRest retained
                    Just dataInfo ->
                        close
                            (Set.union pendingRest (preludeDataDependencies preludeDataByName dataInfo))
                            (Set.insert dataIdentity retained)

    pendingPreludeData pending retained =
        (pending `Set.intersection` Map.keysSet preludeDataByIdentity)
            `Set.difference` retained

referencedPreludeData :: ResolvedProgram -> Map.Map String SymbolIdentity -> Set SymbolIdentity
referencedPreludeData resolvedProgram preludeDataByName =
    Set.fromList
        [ dataIdentity
        | resolvedModule <- resolvedProgramModules resolvedProgram
        , resolvedModuleName resolvedModule /= "Prelude"
        , reference <- resolvedModuleReferences resolvedModule
        , Just dataIdentity <- [preludeDataReference preludeDataByName reference]
        ]

preludeDataReference :: Map.Map String SymbolIdentity -> ResolvedReference -> Maybe SymbolIdentity
preludeDataReference preludeDataByName reference =
    case resolvedReferenceKind reference of
        ResolvedTypeReference
            | symbolNamespace symbolIdentity == SymbolType
            , symbolDefiningModule symbolIdentity == "Prelude" ->
                Just symbolIdentity
        ResolvedConstructorReference ->
            case symbolOwnerIdentity symbolIdentity of
                Just (SymbolOwnerType "Prelude" typeName) ->
                    Map.lookup typeName preludeDataByName
                _ -> Nothing
        _ -> Nothing
  where
    symbolIdentity = resolvedSymbolIdentity (resolvedReferenceSymbol reference)

retainedPreludeBindingData ::
    Map.Map String SymbolIdentity -> [CheckedBinding] -> Set String -> Set SymbolIdentity
retainedPreludeBindingData preludeDataByName preludeBindings retainedPreludeBindings =
    Set.unions
        [ sourceTypePreludeData preludeDataByName (checkedBindingSourceType binding)
        | binding <- preludeBindings
        , checkedBindingName binding `Set.member` retainedPreludeBindings
        ]

preludeDataDependencies :: Map.Map String SymbolIdentity -> DataInfo -> Set SymbolIdentity
preludeDataDependencies preludeDataByName dataInfo =
    Set.unions
        [ sourceTypePreludeData preludeDataByName sourceType
        | constructorInfo <- dataConstructors dataInfo
        , sourceType <- constructorTypes constructorInfo
        ]
  where
    constructorTypes constructorInfo =
        ctorResult constructorInfo
            : ctorArgs constructorInfo
            ++ [bound | (_, Just bound) <- ctorForalls constructorInfo]

sourceTypePreludeData :: Map.Map String SymbolIdentity -> SrcType -> Set SymbolIdentity
sourceTypePreludeData preludeDataByName =
    Set.fromList . mapMaybe (`Map.lookup` preludeDataByName) . Set.toList . sourceTypeHeads

sourceTypeHeads :: SrcType -> Set String
sourceTypeHeads =
    go
  where
    go sourceType =
        case sourceType of
            STVar {} ->
                Set.empty
            STArrow dom cod ->
                Set.union (go dom) (go cod)
            STBase name ->
                Set.singleton name
            STCon name args ->
                Set.insert name (foldMap go args)
            STVarApp _ args ->
                foldMap go args
            STForall _ mbBound body ->
                maybe Set.empty (go . unSrcBound) mbBound `Set.union` go body
            STMu _ body ->
                go body
            STBottom ->
                Set.empty

referencedBindingNames :: [CheckedBinding] -> Set String
referencedBindingNames bindings =
    Set.unions (map (freeElabTermVars . checkedBindingTerm) bindings)

freeElabTermVars :: ElabTerm -> Set String
freeElabTermVars =
    go
  where
    go term =
        case term of
            EVar name ->
                Set.singleton name
            ELit {} ->
                Set.empty
            ELam name _ body ->
                Set.delete name (go body)
            EApp fun arg ->
                Set.union (go fun) (go arg)
            ELet name _ rhs body ->
                Set.union (go rhs) (Set.delete name (go body))
            ETyAbs _ _ body ->
                go body
            ETyInst body _ ->
                go body
            ERoll _ body ->
                go body
            EUnroll body ->
                go body
