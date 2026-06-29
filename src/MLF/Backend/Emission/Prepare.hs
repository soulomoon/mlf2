{-# LANGUAGE GADTs #-}

module MLF.Backend.Emission.Prepare
    ( BackendEmissionPreparationError (..)
    , renderBackendEmissionPreparationError
    , prepareBackendEmissionFromSource
    , prepareBackendEmissionFromProgramPackage
    , prepareBackendEmissionFromLocatedPackage
    , prepareCheckedProgramForBackendEmission
    ) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MLF.Elab.Types (ElabType, ResolvedVar (..), Ty (..), XmlfTerm (..), resolvedVarBoundBy)
import MLF.Frontend.Parse.Program
    ( ProgramParseError
    , parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check
    ( checkLocatedProgramPackage
    , checkProgramPackage
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage
    , ProgramPackage
    , trivialLocatedProgramPackage
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Frontend.Program.Types
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    , ConstructorInfo (..)
    , DataInfo (..)
    , IdDetails (..)
    , ProgramDiagnostic
    , ProgramError
    , ResolvedProgram (..)
    , ResolvedReference
    , ResolvedReferenceKind (..)
    , SymbolIdentity
    , SymbolNamespace (..)
    , TypeView (..)
    , symbolDefiningName
    , symbolNamespace
    , constructorRefSymbol
    , resolvedModuleIdentity
    , resolvedModuleReferences
    , resolvedReferenceKind
    , resolvedReferenceSymbol
    , resolvedSymbolIdentity
    , diagnosticForProgramError
    , renderProgramDiagnostic
    )

data BackendEmissionPreparationError
    = BackendEmissionProgramParseError ProgramParseError
    | BackendEmissionProgramDiagnostic ProgramDiagnostic
    | BackendEmissionProgramError ProgramError
    deriving (Eq, Show)

renderBackendEmissionPreparationError :: BackendEmissionPreparationError -> String
renderBackendEmissionPreparationError err =
    case err of
        BackendEmissionProgramParseError parseErr ->
            renderProgramParseError parseErr
        BackendEmissionProgramDiagnostic diagnostic ->
            renderProgramDiagnostic diagnostic
        BackendEmissionProgramError programErr ->
            renderProgramDiagnostic (diagnosticForProgramError Nothing programErr)

prepareBackendEmissionFromSource ::
    FilePath -> String -> Either BackendEmissionPreparationError CheckedProgram
prepareBackendEmissionFromSource path source = do
    program <-
        first BackendEmissionProgramParseError
            (parseLocatedProgramWithFile path source)
    prepareBackendEmissionFromLocatedPackage
        (withPreludeLocatedPackage (trivialLocatedProgramPackage program))

prepareBackendEmissionFromProgramPackage ::
    ProgramPackage -> Either BackendEmissionPreparationError CheckedProgram
prepareBackendEmissionFromProgramPackage package =
    prepareCheckedProgramForBackendEmission
        <$> first BackendEmissionProgramError (checkProgramPackage package)

prepareBackendEmissionFromLocatedPackage ::
    LocatedProgramPackage -> Either BackendEmissionPreparationError CheckedProgram
prepareBackendEmissionFromLocatedPackage package =
    prepareCheckedProgramForBackendEmission
        <$> first BackendEmissionProgramDiagnostic (checkLocatedProgramPackage package)

prepareCheckedProgramForBackendEmission :: CheckedProgram -> CheckedProgram
prepareCheckedProgramForBackendEmission checked =
    checked {checkedProgramModules = map (prepareModule preludeIdentity retainedPreludeBindings retainedPreludeData) modules0}
  where
    modules0 = checkedProgramModules checked
    preludeIdentity = preludeModuleIdentity modules0
    retainedPreludeBindings = preludeBindingDependencyClosure preludeIdentity modules0
    retainedPreludeData = preludeDataDependencyClosure preludeIdentity checked retainedPreludeBindings

preludeModuleIdentity :: [CheckedModule] -> Maybe SymbolIdentity
preludeModuleIdentity modules0 =
    case [checkedModuleIdentity checkedModule | checkedModule <- modules0, isPreludeModuleIdentity (checkedModuleIdentity checkedModule)] of
        identity : _ -> Just identity
        [] -> Nothing

isPreludeModuleIdentity :: SymbolIdentity -> Bool
isPreludeModuleIdentity identity =
    symbolNamespace identity == SymbolModule
        && symbolDefiningName identity == "Prelude"

isPreludeModule :: Maybe SymbolIdentity -> CheckedModule -> Bool
isPreludeModule preludeIdentity checkedModule =
    Just (checkedModuleIdentity checkedModule) == preludeIdentity

prepareModule :: Maybe SymbolIdentity -> Set SymbolIdentity -> Set SymbolIdentity -> CheckedModule -> CheckedModule
prepareModule preludeIdentity retainedPreludeBindings retainedPreludeData checkedModule
    | isPreludeModule preludeIdentity checkedModule =
        checkedModule
            { checkedModuleBindings =
                filter
                    (retainedPreludeBinding retainedPreludeBindings)
                    (checkedModuleBindings checkedModule)
            , checkedModuleData =
                Map.filter
                    ((`Set.member` retainedPreludeData) . dataInfoSymbol)
                    (checkedModuleData checkedModule)
            }
    | otherwise = checkedModule

retainedPreludeBinding :: Set SymbolIdentity -> CheckedBinding -> Bool
retainedPreludeBinding retainedPreludeBindings binding =
    case checkedBindingSymbolIdentity binding of
        Just symbol -> symbol `Set.member` retainedPreludeBindings
        Nothing -> False

checkedBindingSymbolIdentity :: CheckedBinding -> Maybe SymbolIdentity
checkedBindingSymbolIdentity =
    resolvedVarSymbolIdentity . checkedBindingResolvedVar

resolvedVarSymbolIdentity :: ResolvedVar -> Maybe SymbolIdentity
resolvedVarSymbolIdentity resolved =
    case resolvedVarDetails resolved of
        TopLevelId symbol -> Just symbol
        ConstructorId ref -> Just (constructorRefSymbol ref)
        MethodId symbol -> Just symbol
        _ -> Nothing

preludeBindingDependencyClosure :: Maybe SymbolIdentity -> [CheckedModule] -> Set SymbolIdentity
preludeBindingDependencyClosure preludeIdentity modules0 =
    close (referencedBindingSymbols nonPreludeBindings) Set.empty
  where
    preludeBindingsByIdentity =
        Map.fromList
            [ (symbol, binding)
            | binding <- preludeBindings
            , Just symbol <- [checkedBindingSymbolIdentity binding]
            ]

    preludeBindings =
        [ binding
        | checkedModule <- modules0
        , isPreludeModule preludeIdentity checkedModule
        , binding <- checkedModuleBindings checkedModule
        ]

    nonPreludeBindings =
        [ binding
        | checkedModule <- modules0
        , not (isPreludeModule preludeIdentity checkedModule)
        , binding <- checkedModuleBindings checkedModule
        ]

    close pending retained =
        case Set.minView (pendingPreludeBindings pending retained) of
            Nothing -> retained
            Just (symbol, pendingRest) ->
                case Map.lookup symbol preludeBindingsByIdentity of
                    Nothing -> close pendingRest retained
                    Just binding ->
                        close
                            (Set.union pendingRest (referencedBindingSymbols [binding]))
                            (Set.insert symbol retained)

    pendingPreludeBindings pending retained =
        (pending `Set.intersection` Map.keysSet preludeBindingsByIdentity)
            `Set.difference` retained

preludeDataDependencyClosure :: Maybe SymbolIdentity -> CheckedProgram -> Set SymbolIdentity -> Set SymbolIdentity
preludeDataDependencyClosure preludeIdentity checked retainedPreludeBindings =
    close initialData Set.empty
  where
    modules0 = checkedProgramModules checked
    preludeData =
        [ dataInfo
        | checkedModule <- modules0
        , isPreludeModule preludeIdentity checkedModule
        , dataInfo <- Map.elems (checkedModuleData checkedModule)
        ]

    preludeBindings =
        [ binding
        | checkedModule <- modules0
        , isPreludeModule preludeIdentity checkedModule
        , binding <- checkedModuleBindings checkedModule
        ]

    preludeDataByIdentity =
        Map.fromList [(dataInfoSymbol dataInfo, dataInfo) | dataInfo <- preludeData]

    preludeDataByConstructorBinding =
        Map.fromList
            [ (ctorInfoSymbol constructorInfo, dataInfoSymbol dataInfo)
            | dataInfo <- preludeData
            , constructorInfo <- dataConstructors dataInfo
            ]

    initialData =
        Set.unions
            [ referencedPreludeData preludeIdentity preludeDataByIdentity preludeDataByConstructorBinding (checkedProgramResolved checked)
            , retainedPreludeBindingData (Map.keysSet preludeDataByIdentity) preludeBindings retainedPreludeBindings
            , Set.fromList
                [ dataIdentity
                | bindingSymbol <- Set.toList retainedPreludeBindings
                , Just dataIdentity <- [Map.lookup bindingSymbol preludeDataByConstructorBinding]
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
                            (Set.union pendingRest (preludeDataDependencies (Map.keysSet preludeDataByIdentity) dataInfo))
                            (Set.insert dataIdentity retained)

    pendingPreludeData pending retained =
        (pending `Set.intersection` Map.keysSet preludeDataByIdentity)
            `Set.difference` retained

referencedPreludeData ::
    Maybe SymbolIdentity ->
    Map.Map SymbolIdentity DataInfo ->
    Map.Map SymbolIdentity SymbolIdentity ->
    ResolvedProgram ->
    Set SymbolIdentity
referencedPreludeData preludeIdentity preludeDataByIdentity preludeDataByConstructorBinding resolvedProgram =
    Set.fromList
        [ dataIdentity
        | resolvedModule <- resolvedProgramModules resolvedProgram
        , Just (resolvedModuleIdentity resolvedModule) /= preludeIdentity
        , reference <- resolvedModuleReferences resolvedModule
        , Just dataIdentity <- [preludeDataReference preludeDataByIdentity preludeDataByConstructorBinding reference]
        ]

preludeDataReference ::
    Map.Map SymbolIdentity DataInfo ->
    Map.Map SymbolIdentity SymbolIdentity ->
    ResolvedReference ->
    Maybe SymbolIdentity
preludeDataReference preludeDataByIdentity preludeDataByConstructorBinding reference =
    case resolvedReferenceKind reference of
        ResolvedTypeReference
            | symbolIdentity `Map.member` preludeDataByIdentity ->
                Just symbolIdentity
        ResolvedConstructorReference ->
            Map.lookup symbolIdentity preludeDataByConstructorBinding
        _ -> Nothing
  where
    symbolIdentity = resolvedSymbolIdentity (resolvedReferenceSymbol reference)

retainedPreludeBindingData ::
    Set SymbolIdentity ->
    [CheckedBinding] -> Set SymbolIdentity -> Set SymbolIdentity
retainedPreludeBindingData preludeDataIdentities preludeBindings retainedPreludeBindings =
    Set.unions
        [ elabTypePreludeData preludeDataIdentities (checkedBindingType binding)
        | binding <- preludeBindings
        , Just bindingSymbol <- [checkedBindingSymbolIdentity binding]
        , bindingSymbol `Set.member` retainedPreludeBindings
        ]

elabTypePreludeData :: Set SymbolIdentity -> ElabType -> Set SymbolIdentity
elabTypePreludeData preludeDataIdentities ty =
    Set.filter (`Set.member` preludeDataIdentities) (elabTypeHeadIdentities ty)

elabTypeHeadIdentities :: Ty v -> Set SymbolIdentity
elabTypeHeadIdentities =
    \case
        TVarRef {} ->
            Set.empty
        TArrow dom cod ->
            Set.union (elabTypeHeadIdentities dom) (elabTypeHeadIdentities cod)
        TConWithIdentity identity _ args ->
            maybe Set.empty Set.singleton identity
                `Set.union` foldMap elabTypeHeadIdentities args
        TVarAppRef _ args ->
            foldMap elabTypeHeadIdentities args
        TBaseWithIdentity identity _ ->
            maybe Set.empty Set.singleton identity
        TForallRef _ mbBound body ->
            maybe Set.empty elabTypeHeadIdentities mbBound
                `Set.union` elabTypeHeadIdentities body
        TMuRef _ body ->
            elabTypeHeadIdentities body
        TBottom ->
            Set.empty

preludeDataDependencies :: Set SymbolIdentity -> DataInfo -> Set SymbolIdentity
preludeDataDependencies preludeDataIdentities dataInfo =
    Set.unions
        [ Set.filter (`Set.member` preludeDataIdentities) (Set.fromList (Map.elems (typeViewHeadIdentities (ctorTypeView constructorInfo))))
        | constructorInfo <- dataConstructors dataInfo
        ]

referencedBindingSymbols :: [CheckedBinding] -> Set SymbolIdentity
referencedBindingSymbols bindings =
    Set.unions (map (freeXmlfTermVarSymbols . checkedBindingTerm) bindings)

freeXmlfTermVarSymbols :: XmlfTerm -> Set SymbolIdentity
freeXmlfTermVarSymbols =
    go []
  where
    go bound term =
        case term of
            EVarNode resolved ->
                freeResolvedVar bound resolved
            ELit {} ->
                Set.empty
            ELam resolved body ->
                go (resolved : bound) body
            EApp fun arg ->
                Set.union (go bound fun) (go bound arg)
            ELet resolved _ rhs body ->
                Set.union
                    (go bound rhs)
                    (go (resolved : bound) body)
            ETyAbsRef _ _ body ->
                go bound body
            ETyInst body _ ->
                go bound body
            ERoll _ body ->
                go bound body
            EUnroll body ->
                go bound body

    freeResolvedVar bound resolved =
        if resolvedVarBoundBy bound resolved
            then Set.empty
            else maybe Set.empty Set.singleton (resolvedVarSymbolIdentity resolved)
