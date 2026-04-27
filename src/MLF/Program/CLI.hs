module MLF.Program.CLI
    ( programCliUsage
    , emitBackendFile
    , runProgramFile
    ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MLF.Backend.Text
    ( renderBackendTextError
    , renderCheckedProgramBackendText
    )
import MLF.Elab.Types (ElabTerm (..))
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check (checkLocatedProgram)
import MLF.Frontend.Program.Run
    ( prettyValue
    , runLocatedProgram
    )
import MLF.Frontend.Program.Prelude (withPreludeLocated)
import MLF.Frontend.Program.Types
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    , renderProgramDiagnostic
    )

programCliUsage :: String
programCliUsage =
    unlines
        [ "Usage:"
        , "  mlf2 run-program <file.mlfp>"
        , "  mlf2 emit-backend <file.mlfp>"
        , ""
        , "run-program prepends the built-in Prelude and prints the resulting value."
        , "emit-backend prepends the built-in Prelude and prints first LLVM-like backend text."
        ]

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        first renderProgramDiagnostic (prettyValue <$> runLocatedProgram (withPreludeLocated program))

emitBackendFile :: FilePath -> IO (Either String String)
emitBackendFile path = do
    fileResult <- try (readFile path) :: IO (Either IOException String)
    pure $ do
        source <- first show fileResult
        program <- first renderProgramParseError (parseLocatedProgramWithFile path source)
        checked <- first renderProgramDiagnostic (checkLocatedProgram (withPreludeLocated program))
        first renderBackendTextError (renderCheckedProgramBackendText (emitBackendCheckedProgram checked))

emitBackendCheckedProgram :: CheckedProgram -> CheckedProgram
emitBackendCheckedProgram checked =
    checked {checkedProgramModules = map (emitBackendModule retainedPreludeBindings) modules0}
  where
    modules0 = checkedProgramModules checked
    retainedPreludeBindings = preludeBindingDependencyClosure modules0

emitBackendModule :: Set String -> CheckedModule -> CheckedModule
emitBackendModule retainedPreludeBindings checkedModule
    | checkedModuleName checkedModule == "Prelude" =
        checkedModule
            { checkedModuleBindings =
                filter
                    ((`Set.member` retainedPreludeBindings) . checkedBindingName)
                    (checkedModuleBindings checkedModule)
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
