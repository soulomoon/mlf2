{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.LLVM.Lower
Description : Lower typed backend IR into real LLVM IR syntax
-}
module MLF.Backend.LLVM.Lower
  ( BackendLLVMError (..),
    lowerBackendProgram,
    renderBackendLLVMError,
  )
where

import Control.Monad (foldM, unless, when, zipWithM, zipWithM_)
import Control.Monad.State.Strict (StateT (StateT), evalStateT, get, gets, modify)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, ord)
import Data.List (intercalate, nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import MLF.Backend.IR
import MLF.Backend.LLVM.Syntax
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))

data BackendLLVMError
  = BackendLLVMValidationFailed BackendValidationError
  | BackendLLVMUnsupportedType String BackendType
  | BackendLLVMUnsupportedExpression String String
  | BackendLLVMUnsupportedCall String
  | BackendLLVMUnknownFunction String
  | BackendLLVMUnknownConstructor String
  | BackendLLVMArityMismatch String Int Int
  | BackendLLVMUnsupportedString String
  | BackendLLVMInternalError String
  deriving (Eq, Show)

data ProgramBase = ProgramBase
  { pbBindings :: Map String BindingInfo,
    pbBindingOrder :: [String],
    pbConstructors :: Map String ConstructorRuntime,
    pbDataNames :: Set String
  }

data ProgramEnv = ProgramEnv
  { peBase :: ProgramBase,
    peSpecializations :: Map String Specialization,
    peStringGlobals :: Map String String
  }

data BindingInfo = BindingInfo
  { biName :: String,
    biForm :: FunctionForm,
    biExportedAsMain :: Bool
  }
  deriving (Eq, Show)

data FunctionForm = FunctionForm
  { ffTypeBinders :: [(String, Maybe BackendType)],
    ffParams :: [(String, BackendType)],
    ffBody :: BackendExpr,
    ffReturnType :: BackendType
  }
  deriving (Eq, Show)

data ConstructorRuntime = ConstructorRuntime
  { crConstructor :: BackendConstructor,
    crDataParameters :: [String],
    crTag :: Integer
  }
  deriving (Eq, Show)

data SpecRequest = SpecRequest
  { srBindingName :: String,
    srTypeArgs :: [BackendType]
  }
  deriving (Eq, Show)

data Specialization = Specialization
  { spRequest :: SpecRequest,
    spFunctionName :: String,
    spForm :: FunctionForm
  }
  deriving (Eq, Show)

data LowerValue = LowerValue
  { lvBackendType :: BackendType,
    lvLLVMType :: LLVMType,
    lvOperand :: LLVMOperand
  }
  deriving (Eq, Show)

data LocalFunction = LocalFunction
  { lfForm :: FunctionForm,
    lfCapturedEnv :: ExprEnv
  }
  deriving (Eq, Show)

data ExprEnv = ExprEnv
  { eeValues :: Map String LowerValue,
    eeLocalFunctions :: Map String LocalFunction
  }
  deriving (Eq, Show)

data FunctionState = FunctionState
  { fsNextLocal :: Int,
    fsNextBlock :: Int,
    fsCurrentLabel :: String,
    fsCurrentInstructions :: [LLVMInstruction],
    fsCompletedBlocks :: [LLVMBasicBlock]
  }
  deriving (Eq, Show)

type LowerM = StateT FunctionState (Either BackendLLVMError)

lowerBackendProgram :: BackendProgram -> Either BackendLLVMError LLVMModule
lowerBackendProgram program = do
  first BackendLLVMValidationFailed (validateBackendProgram program)
  base <- buildProgramBase program
  reachable <- reachableBindings base (backendProgramMain program)
  specializations <- collectRequiredSpecializations base reachable
  let stringGlobals = assignStringGlobals (collectProgramStrings reachable specializations)
      env =
        ProgramEnv
          { peBase = base,
            peSpecializations = Map.fromList [(specializationKey (spRequest spec), spec) | spec <- specializations],
            peStringGlobals = stringGlobals
          }
  functions <-
    (++)
      <$> traverse (lowerMonomorphicBinding env) (filter (null . ffTypeBinders . biForm) reachable)
      <*> traverse (lowerSpecialization env) specializations
  mainBinding <- requireBinding base (backendProgramMain program)
  when (not (null (ffTypeBinders (biForm mainBinding)))) $
    Left (BackendLLVMUnsupportedExpression "program main" "polymorphic main binding")
  pure
    LLVMModule
      { llvmModuleGlobals =
          [LLVMStringGlobal globalName value | (value, globalName) <- Map.toAscList stringGlobals],
        llvmModuleDeclarations = runtimeDeclarations,
        llvmModuleFunctions = functions
      }

runtimeDeclarations :: [LLVMDeclaration]
runtimeDeclarations =
  [ LLVMDeclaration "malloc" LLVMPtr [LLVMInt 64],
    LLVMDeclaration "__mlfp_and" (LLVMInt 1) [LLVMInt 1, LLVMInt 1]
  ]

buildProgramBase :: BackendProgram -> Either BackendLLVMError ProgramBase
buildProgramBase program = do
  let modules0 = backendProgramModules program
      bindings =
        [ binding
        | backendModule <- modules0,
          binding <- backendModuleBindings backendModule
        ]
      dataDecls =
        [ dataDecl
        | backendModule <- modules0,
          dataDecl <- backendModuleData backendModule
        ]
      bindingInfos = map bindingInfo bindings
      constructors =
        concatMap constructorRuntimes dataDecls
  pure
        ProgramBase
          { pbBindings = Map.fromList [(biName info, info) | info <- bindingInfos],
            pbBindingOrder = map biName bindingInfos,
            pbConstructors = Map.fromList [(backendConstructorName (crConstructor ctor), ctor) | ctor <- constructors],
            pbDataNames = Set.fromList (concatMap dataNameAliases (map backendDataName dataDecls))
          }

bindingInfo :: BackendBinding -> BindingInfo
bindingInfo binding =
  BindingInfo
    { biName = backendBindingName binding,
      biForm = functionFormFromExpr (backendBindingExpr binding),
      biExportedAsMain = backendBindingExportedAsMain binding
    }

constructorRuntimes :: BackendData -> [ConstructorRuntime]
constructorRuntimes dataDecl =
  [ ConstructorRuntime
      { crConstructor = constructor,
        crDataParameters = backendDataParameters dataDecl,
        crTag = tag
      }
  | (tag, constructor) <- zip [0 ..] (backendDataConstructors dataDecl)
  ]

dataNameAliases :: String -> [String]
dataNameAliases name =
  nub [name, suffixAfter '.' name, suffixAfterRuntimeModule name]

suffixAfter :: Char -> String -> String
suffixAfter needle value =
  reverse (takeWhile (/= needle) (reverse value))

suffixAfterRuntimeModule :: String -> String
suffixAfterRuntimeModule value =
  case splitRuntimeModule value of
    Just suffix -> suffix
    Nothing -> value

splitRuntimeModule :: String -> Maybe String
splitRuntimeModule value =
  go value
  where
    go [] = Nothing
    go ('_' : '_' : rest) = Just rest
    go (_ : rest) = go rest

functionFormFromExpr :: BackendExpr -> FunctionForm
functionFormFromExpr expr =
  FunctionForm
    { ffTypeBinders = typeBinders,
      ffParams = params,
      ffBody = body,
      ffReturnType = backendExprType body
    }
  where
    (typeBinders, afterTypes) = collectTypeAbs expr
    (params, body) = collectLams afterTypes

collectTypeAbs :: BackendExpr -> ([(String, Maybe BackendType)], BackendExpr)
collectTypeAbs =
  \case
    BackendTyAbs _ name mbBound body ->
      let (params, core) = collectTypeAbs body
       in ((name, mbBound) : params, core)
    expr -> ([], expr)

collectLams :: BackendExpr -> ([(String, BackendType)], BackendExpr)
collectLams =
  \case
    BackendLam _ name paramTy body ->
      let (params, core) = collectLams body
       in ((name, paramTy) : params, core)
    expr -> ([], expr)

reachableBindings :: ProgramBase -> String -> Either BackendLLVMError [BindingInfo]
reachableBindings base mainName =
  traverse (requireBinding base) orderedReachable
  where
    reachableNames = reachableBindingNames base mainName
    orderedReachable =
      filter (`Set.member` reachableNames) (pbBindingOrder base)

reachableBindingNames :: ProgramBase -> String -> Set String
reachableBindingNames base mainName =
  close (Set.singleton mainName) Set.empty
  where
    close pending seen =
      case Set.minView (pending `Set.difference` seen) of
        Nothing -> seen
        Just (name, pendingRest) ->
          case Map.lookup name (pbBindings base) of
            Nothing -> close pendingRest seen
            Just binding ->
              close
                (pendingRest `Set.union` freeGlobalBindingRefs base binding)
                (Set.insert name seen)

requireBinding :: ProgramBase -> String -> Either BackendLLVMError BindingInfo
requireBinding base name =
  case Map.lookup name (pbBindings base) of
    Just binding -> Right binding
    Nothing -> Left (BackendLLVMUnknownFunction name)

collectRequiredSpecializations :: ProgramBase -> [BindingInfo] -> Either BackendLLVMError [Specialization]
collectRequiredSpecializations base reachable =
  go Map.empty initialRequests
  where
    initialRequests =
      concatMap
        (collectSpecializationRequests base Map.empty . ffBody . biForm)
        (filter (null . ffTypeBinders . biForm) reachable)

    go seen [] =
      Right (map snd (sortOn fst (Map.toList seen)))
    go seen (request : rest)
      | Map.member key seen = go seen rest
      | otherwise = do
          binding <- requireBinding base (srBindingName request)
          form <- instantiateFunctionForm ("specialization " ++ srBindingName request) (biForm binding) (srTypeArgs request) []
          let spec =
                Specialization
                  { spRequest = request,
                    spFunctionName = specializedFunctionName request,
                    spForm = form
                  }
              nestedRequests = collectSpecializationRequests base Map.empty (ffBody form)
          go (Map.insert key spec seen) (rest ++ nestedRequests)
      where
        key = specializationKey request

collectSpecializationRequests :: ProgramBase -> Map String BackendType -> BackendExpr -> [SpecRequest]
collectSpecializationRequests base substitution expr =
  requestHere ++ childRequests
  where
    requestHere =
      case expr of
        BackendApp {} ->
          case collectCall expr of
            Just (BackendVar _ name, typeArgs, _)
              | Just binding <- Map.lookup name (pbBindings base),
                not (null (ffTypeBinders (biForm binding))),
                length typeArgs == length (ffTypeBinders (biForm binding)) ->
                  [SpecRequest name (map (substituteBackendTypes substitution) typeArgs)]
            _ -> []
        _ -> []

    childRequests =
      case expr of
        BackendVar {} -> []
        BackendLit {} -> []
        BackendLam _ _ _ body ->
          collectSpecializationRequests base substitution body
        BackendApp _ fun arg ->
          collectSpecializationRequests base substitution fun
            ++ collectSpecializationRequests base substitution arg
        BackendLet _ _ _ rhs body ->
          collectSpecializationRequests base substitution rhs
            ++ collectSpecializationRequests base substitution body
        BackendTyAbs _ name _ body ->
          collectSpecializationRequests base (Map.delete name substitution) body
        BackendTyApp _ fun ty ->
          collectSpecializationRequests base substitution fun
            ++ concatMap (const []) [ty]
        BackendConstruct _ _ args ->
          concatMap (collectSpecializationRequests base substitution) args
        BackendCase _ scrutinee alternatives ->
          collectSpecializationRequests base substitution scrutinee
            ++ concatMap (collectSpecializationRequests base substitution . backendAltBody) (NE.toList alternatives)
        BackendRoll _ payload ->
          collectSpecializationRequests base substitution payload
        BackendUnroll _ payload ->
          collectSpecializationRequests base substitution payload

freeGlobalBindingRefs :: ProgramBase -> BindingInfo -> Set String
freeGlobalBindingRefs base binding =
  freeGlobalRefs base Set.empty (ffBody (biForm binding))

freeGlobalRefs :: ProgramBase -> Set String -> BackendExpr -> Set String
freeGlobalRefs base bound expr =
  case expr of
    BackendVar _ name
      | Set.member name bound -> Set.empty
      | Map.member name (pbBindings base) -> Set.singleton name
      | otherwise -> Set.empty
    BackendLit {} ->
      Set.empty
    BackendLam _ name _ body ->
      freeGlobalRefs base (Set.insert name bound) body
    BackendApp _ fun arg ->
      freeGlobalRefs base bound fun `Set.union` freeGlobalRefs base bound arg
    BackendLet _ name _ rhs body ->
      freeGlobalRefs base bound rhs `Set.union` freeGlobalRefs base (Set.insert name bound) body
    BackendTyAbs _ _ _ body ->
      freeGlobalRefs base bound body
    BackendTyApp _ fun _ ->
      freeGlobalRefs base bound fun
    BackendConstruct _ _ args ->
      Set.unions (map (freeGlobalRefs base bound) args)
    BackendCase _ scrutinee alternatives ->
      freeGlobalRefs base bound scrutinee
        `Set.union` Set.unions (map (freeAlternativeRefs bound) (NE.toList alternatives))
    BackendRoll _ payload ->
      freeGlobalRefs base bound payload
    BackendUnroll _ payload ->
      freeGlobalRefs base bound payload
  where
    freeAlternativeRefs bound0 alternative =
      freeGlobalRefs base (Set.union (patternBinders (backendAltPattern alternative)) bound0) (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

collectProgramStrings :: [BindingInfo] -> [Specialization] -> [String]
collectProgramStrings reachable specializations =
  sort $
    nub $
      concatMap (collectStringLiterals . ffBody . biForm) (filter (null . ffTypeBinders . biForm) reachable)
        ++ concatMap (collectStringLiterals . ffBody . spForm) specializations

collectStringLiterals :: BackendExpr -> [String]
collectStringLiterals =
  \case
    BackendVar {} -> []
    BackendLit _ (LString value) -> [value]
    BackendLit {} -> []
    BackendLam _ _ _ body -> collectStringLiterals body
    BackendApp _ fun arg -> collectStringLiterals fun ++ collectStringLiterals arg
    BackendLet _ _ _ rhs body -> collectStringLiterals rhs ++ collectStringLiterals body
    BackendTyAbs _ _ _ body -> collectStringLiterals body
    BackendTyApp _ fun _ -> collectStringLiterals fun
    BackendConstruct _ _ args -> concatMap collectStringLiterals args
    BackendCase _ scrutinee alternatives ->
      collectStringLiterals scrutinee ++ concatMap (collectStringLiterals . backendAltBody) (NE.toList alternatives)
    BackendRoll _ payload -> collectStringLiterals payload
    BackendUnroll _ payload -> collectStringLiterals payload

assignStringGlobals :: [String] -> Map String String
assignStringGlobals values =
  Map.fromList [(value, "__mlfp_str." ++ show index0) | (index0, value) <- zip [(0 :: Int) ..] values]

asciiString :: String -> Bool
asciiString =
  all (\char -> ord char >= 0 && ord char <= 127)

specializationKey :: SpecRequest -> String
specializationKey request =
  srBindingName request ++ "\0" ++ intercalate "\0" (map backendTypeKey (srTypeArgs request))

specializedFunctionName :: SpecRequest -> String
specializedFunctionName request =
  srBindingName request ++ "$" ++ intercalate "$" (map backendTypeKey (srTypeArgs request))

backendTypeKey :: BackendType -> String
backendTypeKey =
  sanitizeKey . show

sanitizeKey :: String -> String
sanitizeKey =
  map sanitizeChar
  where
    sanitizeChar char
      | isAlphaNum char = char
      | otherwise = '_'

lowerMonomorphicBinding :: ProgramEnv -> BindingInfo -> Either BackendLLVMError LLVMFunction
lowerMonomorphicBinding env binding =
  lowerFunction env (biName binding) False (biForm binding)

lowerSpecialization :: ProgramEnv -> Specialization -> Either BackendLLVMError LLVMFunction
lowerSpecialization env specialization =
  lowerFunction env (spFunctionName specialization) True (spForm specialization)

lowerFunction :: ProgramEnv -> String -> Bool -> FunctionForm -> Either BackendLLVMError LLVMFunction
lowerFunction env name private form = do
  unless (null (ffTypeBinders form)) $
    Left (BackendLLVMUnsupportedExpression ("binding " ++ show name) "unspecialized polymorphic binding")
  returnTy <- lowerBackendType env ("return type of " ++ name) (ffReturnType form)
  params <- traverse lowerParam (ffParams form)
  let initialExprEnv = initialFunctionEnv form params
  result <-
    evalStateT
      ( do
          bodyValue <- lowerExpr env initialExprEnv ("binding " ++ show name) (ffBody form)
          unless (lvLLVMType bodyValue == returnTy) $
            liftEither (BackendLLVMInternalError ("LLVM return type mismatch in " ++ name))
          finishCurrentBlock (LLVMRet returnTy (lvOperand bodyValue))
          gets (reverse . fsCompletedBlocks)
      )
      initialFunctionState
  pure
    LLVMFunction
      { llvmFunctionName = name,
        llvmFunctionPrivate = private,
        llvmFunctionReturnType = returnTy,
        llvmFunctionParameters = params,
        llvmFunctionBlocks = result
      }
  where
    lowerParam (paramName, paramTy) = do
      llvmTy <- lowerBackendType env ("parameter " ++ show paramName ++ " of " ++ name) paramTy
      pure (LLVMParameter llvmTy paramName)

initialFunctionState :: FunctionState
initialFunctionState =
  FunctionState
    { fsNextLocal = 0,
      fsNextBlock = 0,
      fsCurrentLabel = "entry",
      fsCurrentInstructions = [],
      fsCompletedBlocks = []
    }

initialFunctionEnv :: FunctionForm -> [LLVMParameter] -> ExprEnv
initialFunctionEnv form params =
  ExprEnv
    { eeValues =
        Map.fromList
          [ (paramName, LowerValue paramTy (llvmParameterType param) (LLVMLocal (llvmParameterType param) paramName))
          | ((paramName, paramTy), param) <- zip (ffParams form) params
          ],
      eeLocalFunctions = Map.empty
    }

liftEither :: BackendLLVMError -> LowerM a
liftEither =
  StateT . const . Left

lowerExpr :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerExpr env exprEnv context expr =
  case expr of
    BackendVar ty name ->
      lowerVar env exprEnv context ty name
    BackendLit ty lit ->
      lowerLit env context ty lit
    BackendLam {} ->
      liftEither (BackendLLVMUnsupportedExpression context "escaping lambda")
    BackendApp {} ->
      lowerCall env exprEnv context expr
    BackendLet resultTy name _ rhs body -> do
      exprEnv' <- bindLet env exprEnv context name rhs
      bodyValue <- lowerExpr env exprEnv' context body
      expectedTy <- lowerBackendTypeM env context resultTy
      unless (lvLLVMType bodyValue == expectedTy) $
        liftEither (BackendLLVMInternalError ("let result type mismatch at " ++ context))
      pure bodyValue
    BackendTyAbs {} ->
      liftEither (BackendLLVMUnsupportedExpression context "escaping type abstraction")
    BackendTyApp _ fun _ ->
      lowerExpr env exprEnv context fun
    BackendConstruct resultTy name args ->
      lowerConstruct env exprEnv context resultTy name args
    BackendCase resultTy scrutinee alternatives ->
      lowerCase env exprEnv context resultTy scrutinee alternatives
    BackendRoll resultTy payload ->
      lowerRollLike env exprEnv context resultTy payload "roll"
    BackendUnroll resultTy payload ->
      lowerRollLike env exprEnv context resultTy payload "unroll"

bindLet :: ProgramEnv -> ExprEnv -> String -> String -> BackendExpr -> LowerM ExprEnv
bindLet env exprEnv context name rhs =
  case functionFormFromExpr rhs of
    form
      | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
          pure
            exprEnv
              { eeLocalFunctions =
                  Map.insert
                    name
                    LocalFunction {lfForm = form, lfCapturedEnv = exprEnv}
                    (eeLocalFunctions exprEnv)
              }
    _ -> do
      value <- lowerExpr env exprEnv (context ++ ", let " ++ show name) rhs
      pure exprEnv {eeValues = Map.insert name value (eeValues exprEnv)}

lowerVar :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> LowerM LowerValue
lowerVar env exprEnv context ty name =
  case Map.lookup name (eeValues exprEnv) of
    Just value -> pure value
    Nothing ->
      case Map.lookup name (pbBindings (peBase env)) of
        Just binding
          | not (null (ffTypeBinders (biForm binding))) ->
              liftEither (BackendLLVMUnsupportedExpression context ("escaping polymorphic binding " ++ show name))
          | null (ffParams (biForm binding)) -> do
              resultTy <- lowerBackendTypeM env context ty
              result <- emitAssign "call" resultTy (LLVMCall name [])
              pure (LowerValue ty resultTy result)
          | otherwise ->
              liftEither (BackendLLVMUnsupportedExpression context ("escaping function " ++ show name))
        Nothing ->
          liftEither (BackendLLVMUnknownFunction name)

lowerLit :: ProgramEnv -> String -> BackendType -> Lit -> LowerM LowerValue
lowerLit env context ty lit = do
  llvmTy <- lowerBackendTypeM env context ty
  case lit of
    LInt value ->
      pure (LowerValue ty llvmTy (LLVMIntLiteral 64 value))
    LBool value ->
      pure (LowerValue ty llvmTy (LLVMIntLiteral 1 (if value then 1 else 0)))
    LString value ->
      case Map.lookup value (peStringGlobals env) of
        Just globalName
          | asciiString value ->
              pure (LowerValue ty llvmTy (LLVMGlobalRef LLVMPtr globalName))
        Just _ ->
          liftEither (BackendLLVMUnsupportedString value)
        Nothing ->
          liftEither (BackendLLVMInternalError ("missing string global at " ++ context))

lowerCall :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerCall env exprEnv context expr =
  case collectCall expr of
    Nothing ->
      liftEither (BackendLLVMUnsupportedCall context)
    Just (headExpr, typeArgs, args) ->
      case headExpr of
        BackendVar _ name ->
          case Map.lookup name (eeLocalFunctions exprEnv) of
            Just localFunction ->
              lowerLocalFunctionCall env exprEnv context name localFunction typeArgs args
            Nothing ->
              lowerGlobalCall env exprEnv context name typeArgs args
        BackendLam {} ->
          lowerDirectFunctionCall env exprEnv context (functionFormFromExpr headExpr) typeArgs args
        BackendTyAbs {} ->
          lowerDirectFunctionCall env exprEnv context (functionFormFromExpr headExpr) typeArgs args
        _ ->
          liftEither (BackendLLVMUnsupportedCall ("unsupported call head at " ++ context))

lowerLocalFunctionCall :: ProgramEnv -> ExprEnv -> String -> String -> LocalFunction -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerLocalFunctionCall env callEnv context name localFunction typeArgs args = do
  form <- instantiateFunctionFormM context (lfForm localFunction) typeArgs args
  callArgs <- traverse (lowerExpr env callEnv context) args
  bindFunctionArguments env context name form callArgs
  let bodyEnv = extendExprEnvWithArguments (lfCapturedEnv localFunction) form callArgs
  lowerExpr env bodyEnv context (ffBody form)

lowerDirectFunctionCall :: ProgramEnv -> ExprEnv -> String -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerDirectFunctionCall env exprEnv context form0 typeArgs args = do
  form <- instantiateFunctionFormM context form0 typeArgs args
  callArgs <- traverse (lowerExpr env exprEnv context) args
  bindFunctionArguments env context "lambda" form callArgs
  lowerExpr env (extendExprEnvWithArguments exprEnv form callArgs) context (ffBody form)

lowerGlobalCall :: ProgramEnv -> ExprEnv -> String -> String -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerGlobalCall env exprEnv context name typeArgs args
  | name == "__mlfp_and" = do
      unless (length args == 2) $
        liftEither (BackendLLVMArityMismatch name 2 (length args))
      callArgs <- traverse (lowerExpr env exprEnv context) args
      let expectedTypes = [LLVMInt 1, LLVMInt 1]
      zipWithM_ (requireLLVMType context name) expectedTypes callArgs
      result <- emitAssign "call" (LLVMInt 1) (LLVMCall "__mlfp_and" [(LLVMInt 1, lvOperand arg) | arg <- callArgs])
      pure (LowerValue (BTBase (BaseTy "Bool")) (LLVMInt 1) result)
  | otherwise =
      case Map.lookup name (pbBindings (peBase env)) of
        Nothing ->
          liftEither (BackendLLVMUnknownFunction name)
        Just binding -> do
          form <- instantiateFunctionFormM context (biForm binding) typeArgs args
          unless (length args == length (ffParams form)) $
            liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          bindFunctionArguments env context name form callArgs
          resultTy <- lowerBackendTypeM env context (ffReturnType form)
          functionName <- globalFunctionName env context binding typeArgs
          result <- emitAssign "call" resultTy (LLVMCall functionName [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
          pure (LowerValue (ffReturnType form) resultTy result)

globalFunctionName :: ProgramEnv -> String -> BindingInfo -> [BackendType] -> LowerM String
globalFunctionName env context binding typeArgs
  | null (ffTypeBinders (biForm binding)) =
      pure (biName binding)
  | otherwise =
      case Map.lookup (specializationKey request) (peSpecializations env) of
        Just specialization -> pure (spFunctionName specialization)
        Nothing ->
          liftEither (BackendLLVMInternalError ("missing specialization for " ++ biName binding ++ " at " ++ context))
  where
    request = SpecRequest (biName binding) typeArgs

bindFunctionArguments :: ProgramEnv -> String -> String -> FunctionForm -> [LowerValue] -> LowerM ()
bindFunctionArguments env context name form args = do
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  expectedTypes <- traverse (lowerBackendTypeM env context . snd) (ffParams form)
  zipWithM_ (requireLLVMType context name) expectedTypes args

requireLLVMType :: String -> String -> LLVMType -> LowerValue -> LowerM ()
requireLLVMType context name expected actual =
  unless (lvLLVMType actual == expected) $
    liftEither
      ( BackendLLVMInternalError
          ( "argument type mismatch in "
              ++ name
              ++ " at "
              ++ context
              ++ ": expected "
              ++ show expected
              ++ ", got "
              ++ show (lvLLVMType actual)
          )
      )

extendExprEnvWithArguments :: ExprEnv -> FunctionForm -> [LowerValue] -> ExprEnv
extendExprEnvWithArguments exprEnv form args =
  exprEnv
    { eeValues =
        Map.union
          (Map.fromList [(name, value) | ((name, _), value) <- zip (ffParams form) args])
          (eeValues exprEnv)
    }

instantiateFunctionFormM :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM FunctionForm
instantiateFunctionFormM context form typeArgs args =
  case instantiateFunctionForm context form typeArgs args of
    Right instantiated -> pure instantiated
    Left err -> liftEither err

instantiateFunctionForm :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError FunctionForm
instantiateFunctionForm context form typeArgs args = do
  substitution <- resolveTypeArguments context form typeArgs args
  let substituteTy = substituteBackendTypes substitution
  pure
    FunctionForm
      { ffTypeBinders = [],
        ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
        ffBody = substituteExprTypes substitution (ffBody form),
        ffReturnType = substituteTy (ffReturnType form)
      }

resolveTypeArguments :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError (Map String BackendType)
resolveTypeArguments context form explicitArgs valueArgs
  | null binders =
      if null explicitArgs
        then Right Map.empty
        else Left (BackendLLVMUnsupportedCall ("unexpected type arguments at " ++ context))
  | length explicitArgs == length binders =
      Right (Map.fromList (zip binderNames explicitArgs))
  | null explicitArgs =
      inferTypeArguments context binderNames (ffParams form) valueArgs
  | otherwise =
      Left (BackendLLVMUnsupportedCall ("partial type application at " ++ context))
  where
    binders = ffTypeBinders form
    binderNames = map fst binders

inferTypeArguments :: String -> [String] -> [(String, BackendType)] -> [BackendExpr] -> Either BackendLLVMError (Map String BackendType)
inferTypeArguments context binderNames params args = do
  substitution <-
    foldM
      (\acc ((_, expectedTy), actualExpr) -> matchTypeParams binderSet acc expectedTy (backendExprType actualExpr))
      Map.empty
      (zip params args)
  case filter (`Map.notMember` substitution) binderNames of
    [] -> Right substitution
    missing -> Left (BackendLLVMUnsupportedCall ("could not infer type arguments " ++ show missing ++ " at " ++ context))
  where
    binderSet = Set.fromList binderNames

matchTypeParams :: Set String -> Map String BackendType -> BackendType -> BackendType -> Either BackendLLVMError (Map String BackendType)
matchTypeParams binderSet substitution expected actual =
  case expected of
    BTVar name
      | Set.member name binderSet ->
          case Map.lookup name substitution of
            Nothing -> Right (Map.insert name actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Right substitution
              | otherwise -> Left (BackendLLVMUnsupportedCall ("conflicting inferred type argument for " ++ name))
    _ ->
      case (expected, actual) of
        (BTArrow leftA rightA, BTArrow leftB rightB) ->
          matchTypeParams binderSet substitution leftA leftB >>= \subst -> matchTypeParams binderSet subst rightA rightB
        (BTCon conA argsA, BTCon conB argsB)
          | conA == conB && length argsA == length argsB ->
              foldM
                (\subst (tyA, tyB) -> matchTypeParams binderSet subst tyA tyB)
                substitution
                (zip (NE.toList argsA) (NE.toList argsB))
        (BTBase baseA, BTBase baseB)
          | baseA == baseB -> Right substitution
        (BTForall nameA boundA bodyA, BTForall nameB boundB bodyB) -> do
          substA <-
            case (boundA, boundB) of
              (Nothing, Nothing) -> Right substitution
              (Just tyA, Just tyB) -> matchTypeParams binderSet substitution tyA tyB
              _ -> Left (BackendLLVMUnsupportedCall "mismatched forall bounds during type argument inference")
          matchTypeParams binderSet substA bodyA (substituteBackendType nameB (BTVar nameA) bodyB)
        (BTMu nameA bodyA, BTMu nameB bodyB) ->
          matchTypeParams binderSet substitution bodyA (substituteBackendType nameB (BTVar nameA) bodyB)
        (BTBottom, BTBottom) -> Right substitution
        _ -> Right substitution

collectCall :: BackendExpr -> Maybe (BackendExpr, [BackendType], [BackendExpr])
collectCall expr =
  case collectApps expr of
    (_, []) -> Nothing
    (headExpr, args) ->
      let (typedHead, typeArgs) = collectTyApps headExpr
       in Just (typedHead, typeArgs, args)

collectApps :: BackendExpr -> (BackendExpr, [BackendExpr])
collectApps =
  go []
  where
    go args =
      \case
        BackendApp _ fun arg -> go (arg : args) fun
        expr -> (expr, args)

collectTyApps :: BackendExpr -> (BackendExpr, [BackendType])
collectTyApps =
  go []
  where
    go args =
      \case
        BackendTyApp _ fun ty -> go (ty : args) fun
        expr -> (expr, args)

lowerConstruct :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> [BackendExpr] -> LowerM LowerValue
lowerConstruct env exprEnv context resultTy name args =
  case Map.lookup name (pbConstructors (peBase env)) of
    Nothing ->
      liftEither (BackendLLVMUnknownConstructor name)
    Just constructorRuntime -> do
      let constructor = crConstructor constructorRuntime
      unless (length args == length (backendConstructorFields constructor)) $
        liftEither (BackendLLVMArityMismatch name (length (backendConstructorFields constructor)) (length args))
      argValues <- traverse (lowerExpr env exprEnv context) args
      object <- emitMalloc (8 * (1 + length args))
      tagPtr <- emitGep "tag.ptr" object 0
      emitStore (LLVMInt 64) (LLVMIntLiteral 64 (crTag constructorRuntime)) tagPtr
      zipWithM_ (storeField object) [0 ..] argValues
      resultLLVMType <- lowerBackendTypeM env context resultTy
      unless (resultLLVMType == LLVMPtr) $
        liftEither (BackendLLVMUnsupportedType context resultTy)
      pure (LowerValue resultTy LLVMPtr object)
  where
    storeField object index0 value = do
      fieldPtr <- emitGep "field.ptr" object (8 * (index0 + 1))
      emitStore (lvLLVMType value) (lvOperand value) fieldPtr

lowerCase :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> NonEmpty BackendAlternative -> LowerM LowerValue
lowerCase env exprEnv context resultTy scrutinee alternatives = do
  resultLLVMType <- lowerBackendTypeM env context resultTy
  scrutineeValue <- lowerExpr env exprEnv context scrutinee
  unless (lvLLVMType scrutineeValue == LLVMPtr) $
    liftEither (BackendLLVMUnsupportedType (context ++ " case scrutinee") (lvBackendType scrutineeValue))
  tagPtr <- emitGep "case.tag.ptr" (lvOperand scrutineeValue) 0
  tagValue <- emitAssign "case.tag" (LLVMInt 64) (LLVMLoad (LLVMInt 64) tagPtr)
  altLabels <- traverse (const (freshBlock "case.alt")) (NE.toList alternatives)
  defaultLabel <- maybe (freshBlock "case.default") pure (lookupDefaultLabel altLabels)
  joinLabel <- freshBlock "case.join"
  let switchTargets = mapMaybe constructorSwitchTarget (zip (NE.toList alternatives) altLabels)
  finishCurrentBlock (LLVMSwitch (LLVMInt 64) tagValue defaultLabel switchTargets)
  incoming <- concat <$> zipWithM (lowerAlternative resultLLVMType joinLabel scrutineeValue) (NE.toList alternatives) altLabels
  when (lookupDefaultLabel altLabels == Nothing) $ do
    startBlock defaultLabel
    finishCurrentBlock LLVMUnreachable
  startBlock joinLabel
  result <- emitAssign "case.result" resultLLVMType (LLVMPhi resultLLVMType incoming)
  pure (LowerValue resultTy resultLLVMType result)
  where
    alternativesList = NE.toList alternatives

    lookupDefaultLabel labels =
      case [label | (BackendAlternative BackendDefaultPattern _, label) <- zip alternativesList labels] of
        label : _ -> Just label
        [] -> Nothing

    constructorSwitchTarget (BackendAlternative pattern0 _, label) =
      case pattern0 of
        BackendDefaultPattern -> Nothing
        BackendConstructorPattern name _ ->
          case Map.lookup name (pbConstructors (peBase env)) of
            Just constructorRuntime -> Just (crTag constructorRuntime, label)
            Nothing -> Nothing

    lowerAlternative resultLLVMType joinLabel scrutineeValue alternative label = do
      startBlock label
      exprEnv' <- bindAlternativePattern scrutineeValue alternative
      bodyValue <- lowerExpr env exprEnv' context (backendAltBody alternative)
      unless (lvLLVMType bodyValue == resultLLVMType) $
        liftEither (BackendLLVMInternalError ("case alternative type mismatch at " ++ context))
      sourceLabel <- gets fsCurrentLabel
      finishCurrentBlock (LLVMBr joinLabel)
      pure [(lvOperand bodyValue, sourceLabel)]

    bindAlternativePattern scrutineeValue (BackendAlternative pattern0 _) =
      case pattern0 of
        BackendDefaultPattern ->
          pure exprEnv
        BackendConstructorPattern name binders ->
          case Map.lookup name (pbConstructors (peBase env)) of
            Nothing ->
              liftEither (BackendLLVMUnknownConstructor name)
            Just constructorRuntime -> do
              fieldTys <- constructorFieldTypesForScrutinee env context constructorRuntime (lvBackendType scrutineeValue)
              unless (length fieldTys == length binders) $
                liftEither (BackendLLVMArityMismatch name (length fieldTys) (length binders))
              loadedFields <- zipWithM (loadField scrutineeValue) [0 ..] fieldTys
              pure
                exprEnv
                  { eeValues =
                      Map.union
                        (Map.fromList (zip binders loadedFields))
                        (eeValues exprEnv)
                  }

    loadField scrutineeValue index0 fieldTy = do
      llvmTy <- lowerBackendTypeM env context fieldTy
      fieldPtr <- emitGep "case.field.ptr" (lvOperand scrutineeValue) (8 * (index0 + 1))
      loaded <- emitAssign "case.field" llvmTy (LLVMLoad llvmTy fieldPtr)
      pure (LowerValue fieldTy llvmTy loaded)

constructorFieldTypesForScrutinee :: ProgramEnv -> String -> ConstructorRuntime -> BackendType -> LowerM [BackendType]
constructorFieldTypesForScrutinee _ context constructorRuntime scrutineeTy =
  case matchConstructorResult parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
    Just substitution ->
      pure (map (substituteBackendTypes substitution) (backendConstructorFields constructor))
    Nothing ->
      liftEither
        ( BackendLLVMUnsupportedExpression
            context
            ("could not match constructor result for " ++ backendConstructorName constructor)
        )
  where
    constructor = crConstructor constructorRuntime
    parameters =
      Set.fromList
        ( crDataParameters constructorRuntime
            ++ map backendTypeBinderName (backendConstructorForalls constructor)
        )

matchConstructorResult :: Set String -> Map String BackendType -> BackendType -> BackendType -> Maybe (Map String BackendType)
matchConstructorResult parameters substitution expected actual =
  case expected of
    BTVar name
      | Set.member name parameters ->
          case Map.lookup name substitution of
            Nothing -> Just (Map.insert name actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Just substitution
              | otherwise -> Nothing
    _ ->
      case (expected, actual) of
        (BTVar expectedName, BTVar actualName)
          | expectedName == actualName -> Just substitution
        (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
          matchConstructorResult parameters substitution expectedDom actualDom
            >>= \subst -> matchConstructorResult parameters subst expectedCod actualCod
        (BTBase expectedBase, BTBase actualBase)
          | expectedBase == actualBase -> Just substitution
        (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
          | expectedCon == actualCon && length expectedArgs == length actualArgs ->
              foldM
                (\subst (expectedArg, actualArg) -> matchConstructorResult parameters subst expectedArg actualArg)
                substitution
                (zip (NE.toList expectedArgs) (NE.toList actualArgs))
        (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
          subst <-
            case (expectedBound, actualBound) of
              (Nothing, Nothing) -> Just substitution
              (Just expectedBoundTy, Just actualBoundTy) -> matchConstructorResult parameters substitution expectedBoundTy actualBoundTy
              _ -> Nothing
          matchConstructorResult parameters subst expectedBody (substituteBackendType actualName (BTVar expectedName) actualBody)
        (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
          matchConstructorResult parameters substitution expectedBody (substituteBackendType actualName (BTVar expectedName) actualBody)
        (BTBottom, BTBottom) ->
          Just substitution
        _ ->
          Nothing

lowerRollLike :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> String -> LowerM LowerValue
lowerRollLike env exprEnv context resultTy payload nodeName = do
  payloadValue <- lowerExpr env exprEnv context payload
  resultLLVMType <- lowerBackendTypeM env context resultTy
  if resultLLVMType == lvLLVMType payloadValue
    then pure (LowerValue resultTy resultLLVMType (lvOperand payloadValue))
    else liftEither (BackendLLVMUnsupportedExpression context ("representation-changing " ++ nodeName))

lowerBackendTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerBackendTypeM env context ty =
  case lowerBackendType env context ty of
    Right llvmTy -> pure llvmTy
    Left err -> liftEither err

lowerBackendType :: ProgramEnv -> String -> BackendType -> Either BackendLLVMError LLVMType
lowerBackendType env context ty =
  case ty of
    BTBase (BaseTy "Int") -> Right (LLVMInt 64)
    BTBase (BaseTy "Bool") -> Right (LLVMInt 1)
    BTBase (BaseTy "String") -> Right LLVMPtr
    BTBase (BaseTy name)
      | Set.member name (pbDataNames (peBase env)) -> Right LLVMPtr
      | otherwise -> Left (BackendLLVMUnsupportedType context ty)
    BTCon (BaseTy name) _
      | Set.member name (pbDataNames (peBase env)) -> Right LLVMPtr
      | otherwise -> Left (BackendLLVMUnsupportedType context ty)
    BTMu {} -> Right LLVMPtr
    BTVar {} -> Left (BackendLLVMUnsupportedType context ty)
    BTArrow {} -> Left (BackendLLVMUnsupportedType context ty)
    BTForall {} -> Left (BackendLLVMUnsupportedType context ty)
    BTBottom -> Left (BackendLLVMUnsupportedType context ty)

emitMalloc :: Int -> LowerM LLVMOperand
emitMalloc size =
  emitAssign "malloc" LLVMPtr (LLVMCall "malloc" [(LLVMInt 64, LLVMIntLiteral 64 (toInteger size))])

emitGep :: String -> LLVMOperand -> Int -> LowerM LLVMOperand
emitGep prefix base offset =
  emitAssign prefix LLVMPtr (LLVMGetElementPtr (LLVMInt 8) base [(LLVMInt 64, LLVMIntLiteral 64 (toInteger offset))])

emitStore :: LLVMType -> LLVMOperand -> LLVMOperand -> LowerM ()
emitStore ty value pointer =
  emitInstruction (LLVMStore ty value pointer)

emitAssign :: String -> LLVMType -> LLVMExpression -> LowerM LLVMOperand
emitAssign prefix ty expr = do
  name <- freshLocal prefix
  emitInstruction (LLVMAssign name ty expr)
  pure (LLVMLocal ty name)

emitInstruction :: LLVMInstruction -> LowerM ()
emitInstruction instruction =
  modify $ \state0 ->
    state0 {fsCurrentInstructions = fsCurrentInstructions state0 ++ [instruction]}

freshLocal :: String -> LowerM String
freshLocal prefix = do
  index0 <- gets fsNextLocal
  modify $ \state0 -> state0 {fsNextLocal = index0 + 1}
  pure ("__llvm." ++ prefix ++ "." ++ show index0)

freshBlock :: String -> LowerM String
freshBlock prefix = do
  index0 <- gets fsNextBlock
  modify $ \state0 -> state0 {fsNextBlock = index0 + 1}
  pure (sanitizeBlockLabel prefix ++ "." ++ show index0)

sanitizeBlockLabel :: String -> String
sanitizeBlockLabel =
  map sanitizeChar
  where
    sanitizeChar char
      | isAlphaNum char = char
      | otherwise = '.'

finishCurrentBlock :: LLVMTerminator -> LowerM ()
finishCurrentBlock terminator = do
  state0 <- get
  let block =
        LLVMBasicBlock
          { llvmBlockLabel = fsCurrentLabel state0,
            llvmBlockInstructions = fsCurrentInstructions state0,
            llvmBlockTerminator = terminator
          }
  modify $ \state1 ->
    state1
      { fsCurrentInstructions = [],
        fsCompletedBlocks = block : fsCompletedBlocks state1
      }

startBlock :: String -> LowerM ()
startBlock label =
  modify $ \state0 ->
    state0
      { fsCurrentLabel = label,
        fsCurrentInstructions = []
      }

substituteExprTypes :: Map String BackendType -> BackendExpr -> BackendExpr
substituteExprTypes substitution =
  go
  where
    substituteTy = substituteBackendTypes substitution

    go =
      \case
        BackendVar resultTy name ->
          BackendVar (substituteTy resultTy) name
        BackendLit resultTy lit ->
          BackendLit (substituteTy resultTy) lit
        BackendLam resultTy name paramTy body ->
          BackendLam (substituteTy resultTy) name (substituteTy paramTy) (go body)
        BackendApp resultTy fun arg ->
          BackendApp (substituteTy resultTy) (go fun) (go arg)
        BackendLet resultTy name bindingTy rhs body ->
          BackendLet (substituteTy resultTy) name (substituteTy bindingTy) (go rhs) (go body)
        BackendTyAbs resultTy name mbBound body ->
          BackendTyAbs (substituteTy resultTy) name (fmap substituteTy mbBound) (substituteExprTypes (Map.delete name substitution) body)
        BackendTyApp resultTy fun argTy ->
          BackendTyApp (substituteTy resultTy) (go fun) (substituteTy argTy)
        BackendConstruct resultTy name args ->
          BackendConstruct (substituteTy resultTy) name (map go args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase (substituteTy resultTy) (go scrutinee) (fmap substituteAlternative alternatives)
        BackendRoll resultTy payload ->
          BackendRoll (substituteTy resultTy) (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll (substituteTy resultTy) (go payload)

    substituteAlternative alternative =
      alternative {backendAltBody = go (backendAltBody alternative)}

renderBackendLLVMError :: BackendLLVMError -> String
renderBackendLLVMError =
  \case
    BackendLLVMValidationFailed err ->
      "Backend LLVM validation failed: " ++ show err
    BackendLLVMUnsupportedType context ty ->
      "Unsupported backend LLVM type at " ++ context ++ ": " ++ show ty
    BackendLLVMUnsupportedExpression context detail ->
      "Unsupported backend LLVM expression at " ++ context ++ ": " ++ detail
    BackendLLVMUnsupportedCall detail ->
      "Unsupported backend LLVM call: " ++ detail
    BackendLLVMUnknownFunction name ->
      "Unknown backend LLVM function: " ++ name
    BackendLLVMUnknownConstructor name ->
      "Unknown backend LLVM constructor: " ++ name
    BackendLLVMArityMismatch name expected actual ->
      "Backend LLVM arity mismatch for " ++ name ++ ": expected " ++ show expected ++ ", got " ++ show actual
    BackendLLVMUnsupportedString value ->
      "Unsupported backend LLVM string literal: " ++ show value
    BackendLLVMInternalError detail ->
      "Internal backend LLVM error: " ++ detail
