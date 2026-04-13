module MLF.Frontend.Program.Run
    ( Value (..)
    , runProgram
    , prettyValue
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified MLF.Frontend.Program.Syntax as ProgramSyntax
import MLF.Frontend.Program.Check
    ( CheckedProgram (..)
    , CheckedModule (..)
    , ConstructorInfo (..)
    , DataInfo (..)
    , ProgramError (..)
    , ResolvedAlt (..)
    , ResolvedBinding (..)
    , ResolvedExpr (..)
    , checkProgram
    )
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Frontend.Syntax as Frontend

data Value
    = VLit Lit
    | VClosure RuntimeEnv String ResolvedExpr
    | VConstructor ConstructorInfo [Value]
    | VData ConstructorInfo [Value]
    | VThunk RuntimeEnv ResolvedExpr

instance Eq Value where
    VLit a == VLit b = a == b
    VData ctorA argsA == VData ctorB argsB = ctorRuntimeName ctorA == ctorRuntimeName ctorB && argsA == argsB
    VConstructor ctorA argsA == VConstructor ctorB argsB = ctorRuntimeName ctorA == ctorRuntimeName ctorB && argsA == argsB
    _ == _ = False

instance Show Value where
    show = prettyValue

type RuntimeEnv = Map String Value

runProgram :: ProgramSyntax.Program -> Either ProgramError Value
runProgram program = do
    checked <- checkProgram program
    evalMain checked

evalMain :: CheckedProgram -> Either ProgramError Value
evalMain checked = force (runtimeEnv Map.! checkedProgramMain checked)
  where
    runtimeEnv = buildRuntimeEnv checked

buildRuntimeEnv :: CheckedProgram -> RuntimeEnv
buildRuntimeEnv checked = env
  where
    constructors =
        Map.fromList
            [ ( ctorRuntimeName ctor
              , if null (ctorArgs ctor)
                    then VData ctor []
                    else VConstructor ctor []
              )
            | checkedModule <- checkedProgramModules checked
            , dataInfo <- Map.elems (checkedModuleData checkedModule)
            , ctor <- dataConstructors dataInfo
            ]
    bindings =
        Map.fromList
            [ (resolvedBindingName binding, VThunk env (resolvedBindingExpr binding))
            | checkedModule <- checkedProgramModules checked
            , binding <- checkedModuleBindings checkedModule
            ]
    env = bindings `Map.union` constructors

force :: Value -> Either ProgramError Value
force value = case value of
    VThunk env expr -> eval env expr
    other -> Right other

eval :: RuntimeEnv -> ResolvedExpr -> Either ProgramError Value
eval env expr = case expr of
    RVar name ->
        case Map.lookup name env of
            Just value -> force value
            Nothing -> Left (ProgramUnknownValue name)
    RLit lit -> Right (VLit lit)
    RLam name body -> Right (VClosure env name body)
    RApp fun arg -> do
        funValue <- eval env fun >>= force
        argValue <- eval env arg >>= force
        applyValue funValue argValue
    RLet name rhs body ->
        let env' = Map.insert name (VThunk env' rhs) env
        in eval env' body
    RCase scrutinee alts -> do
        scrutineeValue <- eval env scrutinee >>= force
        matchAlts env scrutineeValue alts

applyValue :: Value -> Value -> Either ProgramError Value
applyValue funValue argValue = case funValue of
    VClosure env name body -> eval (Map.insert name argValue env) body
    VConstructor ctor applied ->
        let applied' = applied ++ [argValue]
        in if length applied' < length (ctorArgs ctor)
            then Right (VConstructor ctor applied')
            else Right (VData ctor applied')
    VData _ _ -> Left (ProgramExpectedFunction (Frontend.STBase "<data>"))
    VLit _ -> Left (ProgramExpectedFunction (Frontend.STBase "<literal>"))
    VThunk env expr -> eval env expr >>= \forced -> applyValue forced argValue

matchAlts :: RuntimeEnv -> Value -> [ResolvedAlt] -> Either ProgramError Value
matchAlts env value alts =
    case alts of
        [] -> Left (ProgramNonExhaustiveCase [])
        alt : rest ->
            case alt of
                RAltCtor ctor binders body ->
                    case value of
                        VData actualCtor args
                            | ctorRuntimeName actualCtor == ctorRuntimeName ctor && length binders == length args ->
                                let env' = foldl bindValue env (zip binders args)
                                in eval env' body
                        _ -> matchAlts env value rest
                RAltVar name body -> eval (Map.insert name value env) body
                RAltWildcard body -> eval env body
  where
    bindValue acc (name, argValue) = Map.insert name argValue acc

prettyValue :: Value -> String
prettyValue value = case value of
    VLit (LInt i) -> show i
    VLit (LBool b) -> if b then "true" else "false"
    VLit (LString s) -> show s
    VClosure {} -> "<closure>"
    VConstructor ctor args -> "<constructor " ++ ctorName ctor ++ suffix args ++ ">"
    VData ctor args -> ctorName ctor ++ suffix args
    VThunk {} -> "<thunk>"
  where
    suffix [] = ""
    suffix args = "(" ++ unwords (map prettyValue args) ++ ")"
