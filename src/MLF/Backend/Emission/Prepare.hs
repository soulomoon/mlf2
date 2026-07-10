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
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MLF.Elab.Types (ElabType, Ty (..), XmlfTerm (..), resolvedVarBoundBy, resolvedVarSymbolIdentity)
import MLF.Frontend.Parse.Program
    ( ProgramParseError
    , parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check
    ( checkLocatedProgramPackage
    , checkProgramPackage
    , validateCheckedProgramTypeViews
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
    , resolvedModuleIdentity
    , resolvedModuleReferences
    , resolvedReferenceKind
    , resolvedReferenceSymbol
    , resolvedSymbolIdentity
    , uniqueInfoEntriesByIdentity
    , diagnosticForProgramError
    , renderProgramDiagnostic
    )
import MLF.Frontend.Symbol (lookupSymbolIdentityExact, memberSymbolIdentityExact, sameSymbolIdentity)

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
    first BackendEmissionProgramError (checkProgramPackage package)
        >>= prepareCheckedProgramForBackendEmission

prepareBackendEmissionFromLocatedPackage ::
    LocatedProgramPackage -> Either BackendEmissionPreparationError CheckedProgram
prepareBackendEmissionFromLocatedPackage package =
    first BackendEmissionProgramDiagnostic (checkLocatedProgramPackage package)
        >>= prepareCheckedProgramForBackendEmission

prepareCheckedProgramForBackendEmission :: CheckedProgram -> Either BackendEmissionPreparationError CheckedProgram
prepareCheckedProgramForBackendEmission checked = do
    first BackendEmissionProgramError (validateCheckedProgramTypeViews checked)
    pure checked {checkedProgramModules = map (prepareModule preludeIdentity retainedPreludeBindings retainedPreludeData) modules0}
  where
    modules0 = checkedProgramModules checked
    preludeIdentity = preludeModuleIdentity modules0
    retainedPreludeBindings = preludeBindingDependencyClosure preludeIdentity modules0
    retainedPreludeData = preludeDataDependencyClosure preludeIdentity checked retainedPreludeBindings

preludeModuleIdentity :: [CheckedModule] -> Maybe SymbolIdentity
preludeModuleIdentity modules0 =
    case nubBy sameSymbolIdentity preludeIdentities of
        [identity] -> Just identity
        _ -> Nothing
  where
    preludeIdentities =
        [ checkedModuleIdentity checkedModule
        | checkedModule <- modules0
        , isPreludeModuleIdentity (checkedModuleIdentity checkedModule)
        ]

isPreludeModuleIdentity :: SymbolIdentity -> Bool
isPreludeModuleIdentity identity =
    symbolNamespace identity == SymbolModule
        && symbolDefiningName identity == "Prelude"

isPreludeModule :: Maybe SymbolIdentity -> CheckedModule -> Bool
isPreludeModule preludeIdentity checkedModule =
    maybe False (\identity -> sameSymbolIdentity identity (checkedModuleIdentity checkedModule)) preludeIdentity

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
                    ((`memberSymbolIdentityExact` retainedPreludeData) . dataInfoSymbol)
                    (checkedModuleData checkedModule)
            }
    | otherwise = checkedModule

retainedPreludeBinding :: Set SymbolIdentity -> CheckedBinding -> Bool
retainedPreludeBinding retainedPreludeBindings binding =
    case checkedBindingSymbolIdentity binding of
        Just symbol -> symbol `memberSymbolIdentityExact` retainedPreludeBindings
        Nothing -> False

checkedBindingSymbolIdentity :: CheckedBinding -> Maybe SymbolIdentity
checkedBindingSymbolIdentity =
    resolvedVarSymbolIdentity . checkedBindingResolvedVar

preludeBindingDependencyClosure :: Maybe SymbolIdentity -> [CheckedModule] -> Set SymbolIdentity
preludeBindingDependencyClosure preludeIdentity modules0 =
    close (referencedBindingSymbols nonPreludeBindings) Set.empty
  where
    preludeBindingsByIdentity =
        uniqueInfoEntriesByIdentity
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
                case lookupSymbolIdentityExact symbol preludeBindingsByIdentity of
                    Nothing -> close pendingRest retained
                    Just binding ->
                        close
                            (Set.union pendingRest (referencedBindingSymbols [binding]))
                            (Set.insert symbol retained)

    pendingPreludeBindings pending retained =
        exactSetDifference
            (Set.filter (`identityInMap` preludeBindingsByIdentity) pending)
            retained

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
        uniqueInfoEntriesByIdentity [(dataInfoSymbol dataInfo, dataInfo) | dataInfo <- preludeData]

    preludeDataByConstructorBinding =
        uniqueInfoEntriesByIdentity
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
                , Just dataIdentity <- [lookupSymbolIdentityExact bindingSymbol preludeDataByConstructorBinding]
                ]
            ]

    close pending retained =
        case Set.minView (pendingPreludeData pending retained) of
            Nothing -> retained
            Just (dataIdentity, pendingRest) ->
                case lookupSymbolIdentityExact dataIdentity preludeDataByIdentity of
                    Nothing -> close pendingRest retained
                    Just dataInfo ->
                        close
                            (Set.union pendingRest (preludeDataDependencies (Map.keysSet preludeDataByIdentity) dataInfo))
                            (Set.insert dataIdentity retained)

    pendingPreludeData pending retained =
        exactSetDifference
            (Set.filter (`identityInMap` preludeDataByIdentity) pending)
            retained

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
            | symbolIdentity `identityInMap` preludeDataByIdentity ->
                Just symbolIdentity
        ResolvedConstructorReference ->
            lookupSymbolIdentityExact symbolIdentity preludeDataByConstructorBinding
        _ -> Nothing
  where
    symbolIdentity = resolvedSymbolIdentity (resolvedReferenceSymbol reference)

retainedPreludeBindingData ::
    Set SymbolIdentity ->
    [CheckedBinding] -> Set SymbolIdentity -> Set SymbolIdentity
retainedPreludeBindingData preludeDataIdentities preludeBindings retainedPreludeBindings =
    Set.unions
        [ Set.union
            (elabTypePreludeData preludeDataIdentities (checkedBindingType binding))
            (typeViewPreludeData preludeDataIdentities (checkedBindingSourceTypeView binding))
        | binding <- preludeBindings
        , Just bindingSymbol <- [checkedBindingSymbolIdentity binding]
        , bindingSymbol `memberSymbolIdentityExact` retainedPreludeBindings
        ]

typeViewPreludeData :: Set SymbolIdentity -> TypeView -> Set SymbolIdentity
typeViewPreludeData preludeDataIdentities view =
    Set.filter
        (`memberSymbolIdentityExact` preludeDataIdentities)
        (Set.fromList (Map.elems (typeViewHeadIdentities view)))

elabTypePreludeData :: Set SymbolIdentity -> ElabType -> Set SymbolIdentity
elabTypePreludeData preludeDataIdentities ty =
    Set.filter (`memberSymbolIdentityExact` preludeDataIdentities) (elabTypeHeadIdentities ty)

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
        [ Set.filter (`memberSymbolIdentityExact` preludeDataIdentities) (Set.fromList (Map.elems (typeViewHeadIdentities (ctorTypeView constructorInfo))))
        | constructorInfo <- dataConstructors dataInfo
        ]

identityInMap :: SymbolIdentity -> Map.Map SymbolIdentity a -> Bool
identityInMap identity =
    maybe False (const True) . lookupSymbolIdentityExact identity

exactSetDifference :: Set SymbolIdentity -> Set SymbolIdentity -> Set SymbolIdentity
exactSetDifference values removed =
    Set.filter (not . (`memberSymbolIdentityExact` removed)) values

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
