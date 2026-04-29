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
import Data.List (intercalate, isPrefixOf, nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showHex)

import MLF.Backend.IR
import MLF.Backend.LLVM.Syntax
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))
import MLF.Util.Names (freshNameLike)

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
    peEvidenceWrappers :: Map String EvidenceWrapper,
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

data EvidenceWrapper = EvidenceWrapper
  { ewKey :: String,
    ewFunctionName :: String,
    ewExpectedType :: BackendType,
    ewExpr :: BackendExpr
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
  let evidenceWrappers = collectEvidenceWrappers base reachable specializations
      referencedFunctionNames = collectReferencedFunctionNames base reachable specializations evidenceWrappers
      stringGlobals = assignStringGlobals (collectProgramStrings reachable specializations)
      env =
        ProgramEnv
          { peBase = base,
            peSpecializations = Map.fromList [(specializationKey (spRequest spec), spec) | spec <- specializations],
            peEvidenceWrappers = Map.fromList [(ewKey wrapper, wrapper) | wrapper <- evidenceWrappers],
            peStringGlobals = stringGlobals
          }
  functions <-
    concat
      <$> sequence
        [ traverse (lowerMonomorphicBinding env) (filter (shouldLowerReachableBinding referencedFunctionNames) reachable),
          traverse (lowerSpecialization env) (filter (shouldLowerSpecialization referencedFunctionNames) specializations),
          traverse (lowerEvidenceWrapper env) evidenceWrappers
        ]
  mainBinding <- requireBinding base (backendProgramMain program)
  when (not (null (ffTypeBinders (biForm mainBinding)))) $
    Left (BackendLLVMUnsupportedExpression "program main" "polymorphic main binding")
  pure
    LLVMModule
      { llvmModuleGlobals =
          [LLVMStringGlobal globalName value | (value, globalName) <- Map.toAscList stringGlobals],
        llvmModuleDeclarations = runtimeDeclarations base,
        llvmModuleFunctions = functions
      }

shouldLowerReachableBinding :: Set String -> BindingInfo -> Bool
shouldLowerReachableBinding referencedFunctionNames binding =
  null (ffTypeBinders form)
    && ( biExportedAsMain binding
           || canEmitFunctionForm form
           || (Set.member (biName binding) referencedFunctionNames && canEmitReferencedFunctionForm form)
       )
  where
    form = biForm binding

shouldLowerSpecialization :: Set String -> Specialization -> Bool
shouldLowerSpecialization referencedFunctionNames specialization =
  canEmitFunctionForm form
    || (Set.member (spFunctionName specialization) referencedFunctionNames && canEmitReferencedFunctionForm form)
  where
    form = spForm specialization

canEmitFunctionForm :: FunctionForm -> Bool
canEmitFunctionForm form =
  not (requiresInlineCall form) || canEmitInlineOnlyFunctionParameters form

canEmitInlineOnlyFunctionParameters :: FunctionForm -> Bool
canEmitInlineOnlyFunctionParameters form =
  not (containsInlineOnlyEvidenceParameterCall form)
    && canEmitReferencedFunctionForm form

canEmitReferencedFunctionForm :: FunctionForm -> Bool
canEmitReferencedFunctionForm form =
  all (uncurry canEmitFunctionParameter) (ffParams form)

canEmitFunctionParameter :: String -> BackendType -> Bool
canEmitFunctionParameter paramName paramTy
  | isFunctionLikeBackendType paramTy =
      isEvidenceParameter paramName paramTy || isFirstOrderFunctionPointerType paramTy
  | otherwise = True

runtimeAndName :: String
runtimeAndName =
  "__mlfp_and"

runtimeMallocName :: String
runtimeMallocName =
  "malloc"

runtimeDeclarations :: ProgramBase -> [LLVMDeclaration]
runtimeDeclarations base =
  [ LLVMDeclaration runtimeMallocName LLVMPtr [LLVMInt 64]
    | Map.notMember runtimeMallocName (pbBindings base)
  ]
    ++ [ LLVMDeclaration runtimeAndName (LLVMInt 1) [LLVMInt 1, LLVMInt 1]
         | Map.notMember runtimeAndName (pbBindings base)
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
      biForm = functionFormFromExpected (backendBindingType binding) (backendBindingExpr binding),
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

functionFormFromExpected :: BackendType -> BackendExpr -> FunctionForm
functionFormFromExpected expectedTy expr =
  case functionFormFromExpr expr of
    form
      | Just completed <- completeAliasFunctionForm form ->
          completed
      | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
          form
      | otherwise ->
          case aliasFunctionForm expectedTy expr of
            Just aliasForm -> aliasForm
            Nothing -> form

completeAliasFunctionForm :: FunctionForm -> Maybe FunctionForm
completeAliasFunctionForm form
  | not (isAliasExpr (ffBody form)) = Nothing
  | null params = Nothing
  | otherwise = do
      body <- applyAliasArguments (ffBody form) (ffReturnType form) (zip argNames params)
      pure
        form
          { ffParams = ffParams form ++ zip argNames params,
            ffBody = body,
            ffReturnType = returnTy
          }
  where
    (params, returnTy) = collectArrowsType (ffReturnType form)
    argNames = ["__mlfp_alias_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]

aliasFunctionForm :: BackendType -> BackendExpr -> Maybe FunctionForm
aliasFunctionForm expectedTy expr
  | not (isAliasExpr expr) = Nothing
  | null typeBinders && null params = Nothing
  | otherwise = do
      headExpr <- either (const Nothing) Just (applyTypeApplicationsToExpr "function alias" afterForalls expr typeArgs)
      body <- applyAliasArguments headExpr afterForalls (zip argNames params)
      pure
        FunctionForm
          { ffTypeBinders = typeBinders,
            ffParams = zip argNames params,
            ffBody = body,
            ffReturnType = returnTy
          }
  where
    (typeBinders, afterForalls) = collectForallsType expectedTy
    (params, returnTy) = collectArrowsType afterForalls
    typeArgs = [BTVar name | (name, _) <- typeBinders]
    argNames = ["__mlfp_alias_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]

isAliasExpr :: BackendExpr -> Bool
isAliasExpr =
  \case
    BackendVar {} -> True
    BackendTyApp _ fun _ -> isAliasExpr fun
    BackendApp _ fun arg -> isAliasExpr fun && isAliasArgument arg
    BackendLet _ _ _ rhs body -> isTransparentAliasLetRhs rhs && isAliasExpr body
    _ -> False

isAliasLetRhs :: BackendExpr -> Bool
isAliasLetRhs =
  \case
    BackendVar {} -> True
    BackendTyApp _ fun _ -> isAliasLetRhs fun
    BackendApp _ fun arg -> isAliasLetRhs fun && isAliasArgument arg
    BackendLet _ _ _ rhs body -> isAliasLetRhs rhs && isAliasLetRhs body
    _ -> False

isAliasArgument :: BackendExpr -> Bool
isAliasArgument =
  \case
    BackendVar ty _ -> isFunctionLikeBackendType ty
    BackendTyApp ty fun _ -> isFunctionLikeBackendType ty && isAliasArgument fun
    BackendApp ty fun arg -> isFunctionLikeBackendType ty && isAliasExpr fun && isAliasArgument arg
    BackendLet ty _ _ rhs body -> isFunctionLikeBackendType ty && isTransparentAliasLetRhs rhs && isAliasArgument body
    _ -> False

isTransparentAliasLetRhs :: BackendExpr -> Bool
isTransparentAliasLetRhs rhs =
  isAliasLetRhs rhs || hasTopLevelTypeAbs rhs

hasTopLevelTypeAbs :: BackendExpr -> Bool
hasTopLevelTypeAbs expr =
  not (null typeBinders)
  where
    (typeBinders, _) = collectTypeAbs expr

collectForallsType :: BackendType -> ([(String, Maybe BackendType)], BackendType)
collectForallsType =
  \case
    BTForall name mbBound body ->
      let (binders, core) = collectForallsType body
       in ((name, mbBound) : binders, core)
    ty -> ([], ty)

collectArrowsType :: BackendType -> ([BackendType], BackendType)
collectArrowsType =
  \case
    BTArrow paramTy resultTy ->
      let (params, returnTy) = collectArrowsType resultTy
       in (paramTy : params, returnTy)
    ty -> ([], ty)

applyAliasArguments :: BackendExpr -> BackendType -> [(String, BackendType)] -> Maybe BackendExpr
applyAliasArguments expr _ [] =
  Just expr
applyAliasArguments expr ty ((name, paramTy) : rest) =
  case ty of
    BTArrow expectedParamTy resultTy
      | alphaEqBackendType expectedParamTy paramTy ->
          applyAliasArguments (BackendApp resultTy expr (BackendVar paramTy name)) resultTy rest
    _ ->
      Nothing

collectTypeAbs :: BackendExpr -> ([(String, Maybe BackendType)], BackendExpr)
collectTypeAbs =
  \case
    BackendTyAbs _ name mbBound body ->
      let (params, core) = collectTypeAbs body
       in ((name, mbBound) : params, core)
    expr -> ([], expr)

collectLams :: BackendExpr -> ([(String, BackendType)], BackendExpr)
collectLams expr =
  let (params, core) = collectRawLams expr
      paramNames = Set.fromList (map fst params)
      reserved = freeBackendExprVars core `Set.difference` paramNames
      (params', renaming) = freshenLambdaParams reserved params
   in (params', renameBackendVars renaming core)

collectRawLams :: BackendExpr -> ([(String, BackendType)], BackendExpr)
collectRawLams =
  \case
    BackendLam _ name paramTy body ->
      let (params, core) = collectRawLams body
       in ((name, paramTy) : params, core)
    expr -> ([], expr)

freshenLambdaParams :: Set String -> [(String, BackendType)] -> ([(String, BackendType)], Map String String)
freshenLambdaParams =
  go Map.empty
  where
    go renaming used =
      \case
        [] -> ([], renaming)
        (name, ty) : rest ->
          let name' = freshNameLike name used
              used' = Set.insert name' used
              renaming' = Map.insert name name' renaming
              (rest', finalRenaming) = go renaming' used' rest
           in ((name', ty) : rest', finalRenaming)

renameBackendVars :: Map String String -> BackendExpr -> BackendExpr
renameBackendVars renaming0 =
  go renaming0
  where
    renameName renaming name =
      Map.findWithDefault name name renaming

    go renaming =
      \case
        BackendVar resultTy name ->
          BackendVar resultTy (renameName renaming name)
        BackendLit resultTy lit ->
          BackendLit resultTy lit
        BackendLam resultTy name paramTy body ->
          BackendLam resultTy name paramTy (go (Map.delete name renaming) body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go renaming fun) (go renaming arg)
        BackendLet resultTy name bindingTy rhs body ->
          BackendLet resultTy name bindingTy (go renaming rhs) (go (Map.delete name renaming) body)
        BackendTyAbs resultTy name mbBound body ->
          BackendTyAbs resultTy name mbBound (go renaming body)
        BackendTyApp resultTy fun ty ->
          BackendTyApp resultTy (go renaming fun) ty
        BackendConstruct resultTy name args ->
          BackendConstruct resultTy name (map (go renaming) args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go renaming scrutinee) (fmap (renameAlternative renaming) alternatives)
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go renaming payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go renaming payload)

    renameAlternative renaming (BackendAlternative pattern0 body) =
      BackendAlternative pattern0 (go (withoutPatternBinders pattern0 renaming) body)

    withoutPatternBinders pattern0 renaming =
      case pattern0 of
        BackendDefaultPattern ->
          renaming
        BackendConstructorPattern _ binders ->
          foldr Map.delete renaming binders

freeBackendExprVars :: BackendExpr -> Set String
freeBackendExprVars =
  go Set.empty
  where
    go bound =
      \case
        BackendVar _ name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        BackendLit {} ->
          Set.empty
        BackendLam _ name _ body ->
          go (Set.insert name bound) body
        BackendApp _ fun arg ->
          go bound fun `Set.union` go bound arg
        BackendLet _ name _ rhs body ->
          go bound rhs `Set.union` go (Set.insert name bound) body
        BackendTyAbs _ _ _ body ->
          go bound body
        BackendTyApp _ fun _ ->
          go bound fun
        BackendConstruct _ _ args ->
          Set.unions (map (go bound) args)
        BackendCase _ scrutinee alternatives ->
          go bound scrutinee `Set.union` Set.unions (map (freeAlternative bound) (NE.toList alternatives))
        BackendRoll _ payload ->
          go bound payload
        BackendUnroll _ payload ->
          go bound payload

    freeAlternative bound (BackendAlternative pattern0 body) =
      go (Set.union (patternBinders pattern0) bound) body

    patternBinders =
      \case
        BackendDefaultPattern ->
          Set.empty
        BackendConstructorPattern _ binders ->
          Set.fromList binders

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
        (collectSpecializationRequestsInForm base Map.empty . biForm)
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
              nestedRequests = collectSpecializationRequestsInForm base Map.empty form
          go (Map.insert key spec seen) (rest ++ nestedRequests)
      where
        key = specializationKey request

collectEvidenceWrappers :: ProgramBase -> [BindingInfo] -> [Specialization] -> [EvidenceWrapper]
collectEvidenceWrappers base reachable specializations =
  zipWith assignName [(0 :: Int) ..] uniqueRequests
  where
    requests =
      concatMap (collectEvidenceWrappersInForm base Map.empty Set.empty . biForm) monomorphicReachable
        ++ concatMap (collectEvidenceWrappersInForm base Map.empty Set.empty . spForm) specializations
    monomorphicReachable =
      filter (null . ffTypeBinders . biForm) reachable
    uniqueRequests =
      map snd (Map.toAscList (Map.fromList [(evidenceWrapperKey expected expr, (expected, expr)) | (expected, expr) <- requests]))
    assignName index0 (expected, expr) =
      EvidenceWrapper
        { ewKey = evidenceWrapperKey expected expr,
          ewFunctionName = "__mlfp_evidence_wrapper$" ++ show index0,
          ewExpectedType = expected,
          ewExpr = expr
        }

collectReferencedFunctionNames :: ProgramBase -> [BindingInfo] -> [Specialization] -> [EvidenceWrapper] -> Set String
collectReferencedFunctionNames base reachable specializations evidenceWrappers =
  Set.unions
    ( map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . biForm) reachable
        ++ map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . spForm) specializations
        ++ map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . evidenceWrapperForm) evidenceWrappers
    )

collectReferencedFunctionNamesInForm :: ProgramBase -> Map String BackendType -> Set String -> FunctionForm -> Set String
collectReferencedFunctionNamesInForm base substitution bound form =
  collectReferencedFunctionNamesInExpr
    base
    substitution
    (Set.union (Set.fromList (map fst (ffParams form))) bound)
    (ffBody form)

collectReferencedFunctionNamesInExpr :: ProgramBase -> Map String BackendType -> Set String -> BackendExpr -> Set String
collectReferencedFunctionNamesInExpr base substitution bound expr =
  referencedHere `Set.union` childReferences
  where
    referencedHere =
      case collectCall expr of
        Just (callee, typeArgs, args) ->
          let typeArgs' = map (substituteBackendTypes substitution) typeArgs
              args' = map (substituteExprTypes substitution) args
           in case instantiateFunctionFormWithTypeArgs "referenced function argument" (callableForm callee) typeArgs' args' of
                Right (_, form) ->
                  Set.fromList
                    [ functionName
                    | ((_, paramTy), arg) <- zip (ffParams form) args',
                      isFunctionLikeBackendType paramTy,
                      Just functionName <- [referencedFunctionArgumentName arg]
                    ]
                Left _ -> Set.empty
        Nothing -> Set.empty

    callableForm callee =
      case callee of
        BackendVar calleeTy _ -> functionFormFromType calleeTy
        _ -> functionFormFromExpr callee

    referencedFunctionArgumentName arg =
      case collectTyApps arg of
        (BackendVar _ name, typeArgs)
          | Set.notMember name bound,
            Just binding <- Map.lookup name (pbBindings base) ->
              case instantiateFunctionFormWithTypeArgs ("referenced function argument " ++ name) (biForm binding) typeArgs [] of
                Right (resolvedTypeArgs, _)
                  | null (ffTypeBinders (biForm binding)) -> Just (biName binding)
                  | otherwise -> Just (specializedFunctionName (SpecRequest name resolvedTypeArgs))
                Left _ -> Nothing
        _ -> Nothing

    childReferences =
      case expr of
        BackendVar {} -> Set.empty
        BackendLit {} -> Set.empty
        BackendLam _ name _ body ->
          collectReferencedFunctionNamesInExpr base substitution (Set.insert name bound) body
        BackendApp _ fun arg ->
          collectReferencedFunctionNamesInExpr base substitution bound fun
            `Set.union` collectReferencedFunctionNamesInExpr base substitution bound arg
        BackendLet _ name bindingTy rhs body ->
          collectLetRhsReferences bindingTy rhs
            `Set.union` collectReferencedFunctionNamesInExpr base substitution (Set.insert name bound) body
        BackendTyAbs _ name _ body ->
          collectReferencedFunctionNamesInExpr base (Map.delete name substitution) bound body
        BackendTyApp _ fun _ ->
          collectReferencedFunctionNamesInExpr base substitution bound fun
        BackendConstruct _ _ args ->
          Set.unions (map (collectReferencedFunctionNamesInExpr base substitution bound) args)
        BackendCase _ scrutinee alternatives ->
          collectReferencedFunctionNamesInExpr base substitution bound scrutinee
            `Set.union` Set.unions (map collectAlternativeReferences (NE.toList alternatives))
        BackendRoll _ payload ->
          collectReferencedFunctionNamesInExpr base substitution bound payload
        BackendUnroll _ payload ->
          collectReferencedFunctionNamesInExpr base substitution bound payload

    collectLetRhsReferences bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              collectReferencedFunctionNamesInForm base substitution bound form
        _ ->
          collectReferencedFunctionNamesInExpr base substitution bound rhs

    collectAlternativeReferences alternative =
      collectReferencedFunctionNamesInExpr
        base
        substitution
        (Set.union (patternBinders (backendAltPattern alternative)) bound)
        (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

collectEvidenceWrappersInForm :: ProgramBase -> Map String BackendType -> Set String -> FunctionForm -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInForm base substitution bound form =
  collectEvidenceWrappersInExpr
    base
    substitution
    (Set.union (Set.fromList (map fst (ffParams form))) bound)
    (ffBody form)

collectEvidenceWrappersInExpr :: ProgramBase -> Map String BackendType -> Set String -> BackendExpr -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInExpr base substitution bound expr =
  wrappersHere ++ childWrappers
  where
    wrappersHere =
      case collectCall expr of
        Just (BackendVar _ name, typeArgs, args)
          | Set.notMember name bound,
            Just binding <- Map.lookup name (pbBindings base) ->
              let typeArgs' = map (substituteBackendTypes substitution) typeArgs
                  args' = map (substituteExprTypes substitution) args
               in case instantiateFunctionFormWithTypeArgs ("evidence wrapper request " ++ name) (biForm binding) typeArgs' args' of
                    Right (_, form) ->
                      [ (paramTy, arg)
                      | ((paramName, paramTy), arg) <- zip (ffParams form) args',
                        isEvidenceArgument False paramName paramTy,
                        not (isSimpleFunctionReference arg),
                        evidenceWrapperArgumentClosed bound arg
                      ]
                    Left _ -> []
        _ -> []

    childWrappers =
      case expr of
        BackendVar {} -> []
        BackendLit {} -> []
        BackendLam _ name _ body ->
          collectEvidenceWrappersInExpr base substitution (Set.insert name bound) body
        BackendApp _ fun arg ->
          collectEvidenceWrappersInExpr base substitution bound fun
            ++ collectEvidenceWrappersInExpr base substitution bound arg
        BackendLet _ name bindingTy rhs body ->
          collectLetRhsWrappers bindingTy rhs
            ++ collectEvidenceWrappersInExpr base substitution (Set.insert name bound) body
        BackendTyAbs _ name _ body ->
          collectEvidenceWrappersInExpr base (Map.delete name substitution) bound body
        BackendTyApp _ fun _ ->
          collectEvidenceWrappersInExpr base substitution bound fun
        BackendConstruct _ _ args ->
          concatMap (collectEvidenceWrappersInExpr base substitution bound) args
        BackendCase _ scrutinee alternatives ->
          collectEvidenceWrappersInExpr base substitution bound scrutinee
            ++ concatMap collectAlternativeWrappers (NE.toList alternatives)
        BackendRoll _ payload ->
          collectEvidenceWrappersInExpr base substitution bound payload
        BackendUnroll _ payload ->
          collectEvidenceWrappersInExpr base substitution bound payload

    collectLetRhsWrappers bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              collectEvidenceWrappersInForm base substitution bound form
        _ ->
          collectEvidenceWrappersInExpr base substitution bound rhs

    collectAlternativeWrappers alternative =
      collectEvidenceWrappersInExpr
        base
        substitution
        (Set.union (patternBinders (backendAltPattern alternative)) bound)
        (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

evidenceWrapperKey :: BackendType -> BackendExpr -> String
evidenceWrapperKey expected expr =
  backendTypeKey expected ++ "\0" ++ show expr

isSimpleFunctionReference :: BackendExpr -> Bool
isSimpleFunctionReference arg =
  case collectTyApps arg of
    (BackendVar {}, _) -> True
    _ -> False

evidenceWrapperArgumentClosed :: Set String -> BackendExpr -> Bool
evidenceWrapperArgumentClosed bound expr =
  Set.null (freeTermVars expr `Set.intersection` bound)

freeTermVars :: BackendExpr -> Set String
freeTermVars =
  go Set.empty
  where
    go bound =
      \case
        BackendVar _ name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        BackendLit {} ->
          Set.empty
        BackendLam _ name _ body ->
          go (Set.insert name bound) body
        BackendApp _ fun arg ->
          Set.union (go bound fun) (go bound arg)
        BackendLet _ name _ rhs body ->
          Set.union (go bound rhs) (go (Set.insert name bound) body)
        BackendTyAbs _ _ _ body ->
          go bound body
        BackendTyApp _ fun _ ->
          go bound fun
        BackendConstruct _ _ args ->
          foldMap (go bound) args
        BackendCase _ scrutinee alternatives ->
          Set.union
            (go bound scrutinee)
            (foldMap (goAlternative bound) (NE.toList alternatives))
        BackendRoll _ payload ->
          go bound payload
        BackendUnroll _ payload ->
          go bound payload

    goAlternative bound (BackendAlternative pattern0 body) =
      go (Set.union (patternBinders pattern0) bound) body

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

collectSpecializationRequestsInForm :: ProgramBase -> Map String BackendType -> FunctionForm -> [SpecRequest]
collectSpecializationRequestsInForm base substitution form =
  collectSpecializationRequestsInFormWithBound base substitution Set.empty form

collectSpecializationRequestsInFormWithBound ::
  ProgramBase ->
  Map String BackendType ->
  Set String ->
  FunctionForm ->
  [SpecRequest]
collectSpecializationRequestsInFormWithBound base substitution bound form =
  collectSpecializationRequestsWithBound
    base
    substitution
    (Set.union (Set.fromList (map fst (ffParams form))) bound)
    (ffBody form)

collectSpecializationRequestsWithBound ::
  ProgramBase ->
  Map String BackendType ->
  Set String ->
  BackendExpr ->
  [SpecRequest]
collectSpecializationRequestsWithBound base substitution bound expr =
  requestHere ++ childRequests
  where
    requestHere =
      case expr of
        BackendApp {} ->
          case collectCall expr of
            Just (BackendVar _ name, typeArgs, _)
              | Set.notMember name bound,
                Just binding <- Map.lookup name (pbBindings base),
                not (null (ffTypeBinders (biForm binding))),
                length typeArgs == length (ffTypeBinders (biForm binding)) ->
                  [SpecRequest name (map (substituteBackendTypes substitution) typeArgs)]
            Just (BackendVar _ name, typeArgs, args)
              | Set.notMember name bound,
                Just binding <- Map.lookup name (pbBindings base),
                not (null (ffTypeBinders (biForm binding))) ->
                  let typeArgs' = map (substituteBackendTypes substitution) typeArgs
                      args' = map (substituteExprTypes substitution) args
                   in case instantiateFunctionFormWithTypeArgs ("specialization request " ++ name) (biForm binding) typeArgs' args' of
                        Right (resolvedTypeArgs, _) -> [SpecRequest name resolvedTypeArgs]
                        Left _ -> []
            Just (fun, typeArgs, args) ->
              collectAdministrativeCallRequests fun typeArgs args
            _ -> []
        BackendTyApp {} ->
          case collectTyApps expr of
            (BackendVar _ name, typeArgs)
              | Set.notMember name bound,
                Just binding <- Map.lookup name (pbBindings base),
                not (null (ffTypeBinders (biForm binding))) ->
                  let typeArgs' = map (substituteBackendTypes substitution) typeArgs
                   in case instantiateFunctionFormWithTypeArgs ("specialization request " ++ name) (biForm binding) typeArgs' [] of
                        Right (resolvedTypeArgs, _) -> [SpecRequest name resolvedTypeArgs]
                        _ -> []
            (fun, typeArgs) ->
              collectAdministrativeTypeAppRequests fun typeArgs
        _ -> []

    childRequests =
      case expr of
        BackendVar {} -> []
        BackendLit {} -> []
        BackendLam _ name _ body ->
          collectSpecializationRequestsWithBound base substitution (Set.insert name bound) body
        BackendApp _ fun arg ->
          collectSpecializationRequestsWithBound base substitution bound fun
            ++ collectSpecializationRequestsWithBound base substitution bound arg
        BackendLet _ name bindingTy rhs body ->
          collectLetRhsSpecializationRequests bindingTy rhs
            ++ collectSpecializationRequestsWithBound base substitution (Set.insert name bound) body
        BackendTyAbs _ name _ body ->
          collectSpecializationRequestsWithBound base (Map.delete name substitution) bound body
        BackendTyApp {} ->
          []
        BackendConstruct _ _ args ->
          concatMap (collectSpecializationRequestsWithBound base substitution bound) args
        BackendCase _ scrutinee alternatives ->
          collectSpecializationRequestsWithBound base substitution bound scrutinee
            ++ concatMap collectAlternativeRequests (NE.toList alternatives)
        BackendRoll _ payload ->
          collectSpecializationRequestsWithBound base substitution bound payload
        BackendUnroll _ payload ->
          collectSpecializationRequestsWithBound base substitution bound payload

    collectAlternativeRequests alternative =
      collectSpecializationRequestsWithBound
        base
        substitution
        (Set.union (patternBinders (backendAltPattern alternative)) bound)
        (backendAltBody alternative)

    collectLetRhsSpecializationRequests bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              collectSpecializationRequestsInFormWithBound base substitution bound form
        _ ->
          collectSpecializationRequestsWithBound base substitution bound rhs

    collectAdministrativeTypeAppRequests fun typeArgs =
      case pushTypeApplicationsIntoExpression context resultTy fun' typeArgs' of
        Right (Just applied) ->
          collectSpecializationRequestsWithBound base Map.empty bound applied
        Right Nothing ->
          case instantiateFunctionFormWithTypeArgs context (functionFormFromExpr fun') typeArgs' [] of
            Right (_, form) ->
              collectSpecializationRequestsWithBound
                base
                Map.empty
                (Set.union (Set.fromList (map fst (ffParams form))) bound)
                (ffBody form)
            Left _ ->
              []
        Left _ ->
          []
      where
        context = "specialization request"
        resultTy = substituteBackendTypes substitution (backendExprType expr)
        fun' = substituteExprTypes substitution fun
        typeArgs' = map (substituteBackendTypes substitution) typeArgs

    collectAdministrativeCallRequests fun typeArgs args =
      case pushCallIntoExpression context resultTy fun' typeArgs' args' of
        Right (Just applied) ->
          collectSpecializationRequestsWithBound base Map.empty bound applied
        Right Nothing ->
          []
        Left _ ->
          []
      where
        context = "specialization request"
        resultTy = substituteBackendTypes substitution (backendExprType expr)
        fun' = substituteExprTypes substitution fun
        typeArgs' = map (substituteBackendTypes substitution) typeArgs
        args' = map (substituteExprTypes substitution) args

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

freeGlobalBindingRefs :: ProgramBase -> BindingInfo -> Set String
freeGlobalBindingRefs base binding =
  freeGlobalRefs
    base
    (Set.fromList (map fst (ffParams (biForm binding))))
    (ffBody (biForm binding))

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

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate =
  go Set.empty
  where
    go _ [] = Nothing
    go seen (value : rest)
      | Set.member value seen = Just value
      | otherwise = go (Set.insert value seen) rest

specializationKey :: SpecRequest -> String
specializationKey request =
  srBindingName request ++ "\0" ++ intercalate "\0" (map backendTypeKey (srTypeArgs request))

specializedFunctionName :: SpecRequest -> String
specializedFunctionName request =
  srBindingName request ++ "$" ++ intercalate "$" (map backendTypeKey (srTypeArgs request))

backendTypeKey :: BackendType -> String
backendTypeKey =
  ("t" ++) . intercalate "_" . map (flip showHex "" . ord) . show

lowerMonomorphicBinding :: ProgramEnv -> BindingInfo -> Either BackendLLVMError LLVMFunction
lowerMonomorphicBinding env binding =
  lowerFunction env (biName binding) False (biForm binding)

lowerSpecialization :: ProgramEnv -> Specialization -> Either BackendLLVMError LLVMFunction
lowerSpecialization env specialization =
  lowerFunction env (spFunctionName specialization) True (spForm specialization)

lowerEvidenceWrapper :: ProgramEnv -> EvidenceWrapper -> Either BackendLLVMError LLVMFunction
lowerEvidenceWrapper env wrapper =
  lowerFunction env (ewFunctionName wrapper) True (evidenceWrapperForm wrapper)

evidenceWrapperForm :: EvidenceWrapper -> FunctionForm
evidenceWrapperForm wrapper =
  FunctionForm
    { ffTypeBinders = [],
      ffParams = zip paramNames params,
      ffBody = body,
      ffReturnType = returnTy
    }
  where
    (params, returnTy) = collectArrowsType (ewExpectedType wrapper)
    paramNames = ["__mlfp_evidence_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]
    paramExprs = [BackendVar paramTy name | (name, paramTy) <- zip paramNames params]
    body = applyEvidenceWrapperArgs (ewExpr wrapper) (ewExpectedType wrapper) paramExprs

applyEvidenceWrapperArgs :: BackendExpr -> BackendType -> [BackendExpr] -> BackendExpr
applyEvidenceWrapperArgs expr _ [] =
  expr
applyEvidenceWrapperArgs expr ty (arg : rest) =
  case ty of
    BTArrow _ resultTy ->
      applyEvidenceWrapperArgs (BackendApp resultTy expr arg) resultTy rest
    _ ->
      expr

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
      llvmTy <- lowerFunctionParameterType env ("parameter " ++ show paramName ++ " of " ++ name) paramName paramTy
      pure (LLVMParameter llvmTy paramName)

lowerFunctionParameterType :: ProgramEnv -> String -> String -> BackendType -> Either BackendLLVMError LLVMType
lowerFunctionParameterType env context paramName paramTy
  | isEvidenceParameter paramName paramTy || isFirstOrderFunctionPointerType paramTy = Right LLVMPtr
  | otherwise = lowerBackendType env context paramTy

lowerFunctionParameterTypeM :: ProgramEnv -> String -> Bool -> String -> BackendType -> LowerM LLVMType
lowerFunctionParameterTypeM env context allowNestedEvidence paramName paramTy =
  case lowerArgumentType env context allowNestedEvidence paramName paramTy of
    Right llvmTy -> pure llvmTy
    Left err -> liftEither err

lowerArgumentType :: ProgramEnv -> String -> Bool -> String -> BackendType -> Either BackendLLVMError LLVMType
lowerArgumentType env context allowNestedEvidence paramName paramTy
  | isEvidenceArgument allowNestedEvidence paramName paramTy = Right LLVMPtr
  | otherwise = lowerBackendType env context paramTy

isEvidenceArgument :: Bool -> String -> BackendType -> Bool
isEvidenceArgument allowNestedEvidence paramName paramTy =
  (isEvidenceName paramName || (allowNestedEvidence && isNestedEvidenceName paramName))
    && isFunctionLikeBackendType paramTy

isEvidenceParameter :: String -> BackendType -> Bool
isEvidenceParameter =
  isEvidenceArgument True

isEvidenceName :: String -> Bool
isEvidenceName =
  isPrefixOf "$evidence_"

isEvidenceCallableName :: String -> Bool
isEvidenceCallableName name =
  isEvidenceName name || isNestedEvidenceName name

isNestedEvidenceName :: String -> Bool
isNestedEvidenceName name =
  "__mlfp_alias_arg" `isPrefixOf` name
    || "__mlfp_evidence_arg" `isPrefixOf` name

isFunctionLikeBackendType :: BackendType -> Bool
isFunctionLikeBackendType =
  \case
    BTForall _ _ body -> isFunctionLikeBackendType body
    BTArrow {} -> True
    _ -> False

isFirstOrderFunctionPointerType :: BackendType -> Bool
isFirstOrderFunctionPointerType ty =
  case ty of
    BTArrow {} ->
      let (params, returnTy) = collectArrowsType ty
       in all isFirstOrderPointerValueType (returnTy : params)
    _ ->
      False

isFirstOrderPointerValueType :: BackendType -> Bool
isFirstOrderPointerValueType =
  \case
    BTVar {} ->
      False
    BTArrow {} ->
      False
    BTBase {} ->
      True
    BTCon _ args ->
      all isFirstOrderPointerValueType args
    BTForall {} ->
      False
    BTMu {} ->
      True
    BTBottom ->
      False

requiresInlineCall :: FunctionForm -> Bool
requiresInlineCall form =
  any (uncurry (isInlineOnlyFunctionParameter False)) (ffParams form)
    || containsInlineOnlyEvidenceParameterCall form

containsInlineOnlyEvidenceParameterCall :: FunctionForm -> Bool
containsInlineOnlyEvidenceParameterCall form =
  go (evidenceParameterNames form) Set.empty (ffBody form)
  where
    go evidenceParams localFunctions expr =
      callRequiresInline evidenceParams localFunctions expr
        || case expr of
          BackendVar {} -> False
          BackendLit {} -> False
          BackendLam _ name _ body ->
            go evidenceParams (Set.delete name localFunctions) body
          BackendApp _ fun arg ->
            go evidenceParams localFunctions fun || go evidenceParams localFunctions arg
          BackendLet _ name bindingTy rhs body ->
            let rhsForm = functionFormFromExpected bindingTy rhs
                rhsIsLocalFunction = not (null (ffTypeBinders rhsForm)) || not (null (ffParams rhsForm))
                localFunctions' =
                  if rhsIsLocalFunction
                    then Set.insert name localFunctions
                    else Set.delete name localFunctions
             in go evidenceParams localFunctions rhs || go evidenceParams localFunctions' body
          BackendTyAbs _ _ _ body ->
            go evidenceParams localFunctions body
          BackendTyApp _ fun _ ->
            go evidenceParams localFunctions fun
          BackendConstruct _ _ args ->
            any (go evidenceParams localFunctions) args
          BackendCase _ scrutinee alternatives ->
            go evidenceParams localFunctions scrutinee
              || any (goAlternative evidenceParams localFunctions) (NE.toList alternatives)
          BackendRoll _ payload ->
            go evidenceParams localFunctions payload
          BackendUnroll _ payload ->
            go evidenceParams localFunctions payload

    goAlternative evidenceParams localFunctions (BackendAlternative pattern0 body) =
      go evidenceParams (localFunctions `Set.difference` patternBinders pattern0) body

    callRequiresInline evidenceParams localFunctions expr =
      case collectCall expr of
        Just (BackendVar calleeTy name, typeArgs, args)
          | Set.member name evidenceParams ->
              case instantiateFunctionFormWithTypeArgs "inline evidence parameter call" (functionFormFromType calleeTy) typeArgs args of
                Right (_, callForm) ->
                  any (uncurry (argumentRequiresInline localFunctions)) (zip (ffParams callForm) args)
                Left _ ->
                  False
        _ ->
          False

    argumentRequiresInline localFunctions (_, paramTy) arg =
      isFunctionLikeBackendType paramTy && functionExpressionRequiresInline localFunctions arg

    functionExpressionRequiresInline localFunctions arg =
      case collectTyApps arg of
        (BackendVar _ name, _) ->
          Set.member name localFunctions
        _ ->
          case arg of
            BackendLam {} -> True
            BackendTyAbs {} -> True
            BackendLet _ name bindingTy rhs body ->
              let rhsForm = functionFormFromExpected bindingTy rhs
                  localFunctions' =
                    if not (null (ffTypeBinders rhsForm)) || not (null (ffParams rhsForm))
                      then Set.insert name localFunctions
                      else Set.delete name localFunctions
               in functionExpressionRequiresInline localFunctions' body
            _ -> False

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

evidenceParameterNames :: FunctionForm -> Set String
evidenceParameterNames form =
  Set.fromList
    [ name
    | (name, ty) <- ffParams form,
      isEvidenceArgument False name ty
    ]

hasTypeBinders :: BackendType -> Bool
hasTypeBinders =
  \case
    BTForall {} -> True
    _ -> False

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
    BackendTyApp {} ->
      lowerTyApp env exprEnv context expr
    BackendConstruct resultTy name args ->
      lowerConstruct env exprEnv context resultTy name args
    BackendCase resultTy scrutinee alternatives ->
      lowerCase env exprEnv context resultTy scrutinee alternatives
    BackendRoll resultTy payload ->
      lowerRollLike env exprEnv context resultTy payload "roll"
    BackendUnroll resultTy payload ->
      lowerRollLike env exprEnv context resultTy payload "unroll"

lowerTyApp :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerTyApp env exprEnv context expr =
  case collectTyApps expr of
    (BackendVar _ name, typeArgs)
      | Just localFunction <- Map.lookup name (eeLocalFunctions exprEnv) ->
          lowerLocalFunctionValue env context (backendExprType expr) name localFunction typeArgs
      | Just binding <- Map.lookup name (pbBindings (peBase env)),
        not (null (ffTypeBinders (biForm binding))) ->
          lowerGlobalValue env context (backendExprType expr) name binding typeArgs
    (fun, typeArgs) ->
      lowerDirectFunctionValue env exprEnv context (backendExprType expr) fun typeArgs

lowerDirectFunctionValue :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> [BackendType] -> LowerM LowerValue
lowerDirectFunctionValue env exprEnv context resultTy fun typeArgs = do
  case pushTypeApplicationsIntoExpression context resultTy fun typeArgs of
    Right (Just applied) ->
      lowerExpr env exprEnv context applied
    Right Nothing -> do
      form <- instantiateFunctionFormM context (functionFormFromExpr fun) typeArgs []
      lowerInstantiatedFunctionValue env exprEnv context "type-applied expression" resultTy form
    Left err ->
      liftEither err

pushTypeApplicationsIntoExpression :: String -> BackendType -> BackendExpr -> [BackendType] -> Either BackendLLVMError (Maybe BackendExpr)
pushTypeApplicationsIntoExpression context resultTy fun typeArgs =
  case fun of
    BackendLet _ name bindingTy rhs body -> do
      appliedBody <- applyTypeApplicationsToExpr context resultTy body typeArgs
      pure (Just (BackendLet resultTy name bindingTy rhs appliedBody))
    BackendCase _ scrutinee alternatives -> do
      appliedAlternatives <- traverse applyAlternative alternatives
      pure (Just (BackendCase resultTy scrutinee appliedAlternatives))
    _ ->
      pure Nothing
  where
    applyAlternative (BackendAlternative pattern0 body) =
      BackendAlternative pattern0 <$> applyTypeApplicationsToExpr context resultTy body typeArgs

applyTypeApplicationsToExpr :: String -> BackendType -> BackendExpr -> [BackendType] -> Either BackendLLVMError BackendExpr
applyTypeApplicationsToExpr context expectedTy expr typeArgs = do
  (applied, actualTy) <- applyTypeApplicationsToExprWithType context expr typeArgs
  unless (alphaEqBackendType expectedTy actualTy) $
    Left (BackendLLVMInternalError ("type application result mismatch at " ++ context))
  pure applied

applyTypeApplicationsToExprWithType :: String -> BackendExpr -> [BackendType] -> Either BackendLLVMError (BackendExpr, BackendType)
applyTypeApplicationsToExprWithType context expr typeArgs =
  foldM applyOne (expr, backendExprType expr) typeArgs
  where
    applyOne (current, currentTy) typeArg =
      case currentTy of
        BTForall name _ bodyTy ->
          let resultTy = substituteBackendType name typeArg bodyTy
           in Right (BackendTyApp resultTy current typeArg, resultTy)
        _ ->
          Left (BackendLLVMUnsupportedCall ("unexpected type arguments at " ++ context))

pushCallIntoExpression :: String -> BackendType -> BackendExpr -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError (Maybe BackendExpr)
pushCallIntoExpression context resultTy fun typeArgs args =
  case fun of
    BackendLet _ name bindingTy rhs body -> do
      appliedBody <- applyCallToExpr context resultTy body typeArgs args
      pure (Just (BackendLet resultTy name bindingTy rhs appliedBody))
    BackendCase _ scrutinee alternatives -> do
      appliedAlternatives <- traverse applyAlternative alternatives
      pure (Just (BackendCase resultTy scrutinee appliedAlternatives))
    _ ->
      pure Nothing
  where
    applyAlternative (BackendAlternative pattern0 body) =
      BackendAlternative pattern0 <$> applyCallToExpr context resultTy body typeArgs args

applyCallToExpr :: String -> BackendType -> BackendExpr -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError BackendExpr
applyCallToExpr context expectedTy expr typeArgs args = do
  (typedExpr, typedExprTy) <- applyTypeApplicationsToExprWithType context expr typeArgs
  (applied, actualTy) <- foldM applyOne (typedExpr, typedExprTy) args
  unless (alphaEqBackendType expectedTy actualTy) $
    Left (BackendLLVMInternalError ("call result mismatch at " ++ context))
  pure applied
  where
    applyOne (current, currentTy) arg =
      case currentTy of
        BTArrow expectedArgTy resultTy
          | alphaEqBackendType expectedArgTy (backendExprType arg) ->
              Right (BackendApp resultTy current arg, resultTy)
          | otherwise ->
              Left (BackendLLVMUnsupportedCall ("argument type mismatch at " ++ context))
        _ ->
          Left (BackendLLVMUnsupportedCall ("too many call arguments at " ++ context))

lowerLocalFunctionValue :: ProgramEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> LowerM LowerValue
lowerLocalFunctionValue env context resultTy name localFunction typeArgs = do
  form <- instantiateFunctionFormM context (lfForm localFunction) typeArgs []
  lowerInstantiatedFunctionValue env (lfCapturedEnv localFunction) context name resultTy form

lowerInstantiatedFunctionValue :: ProgramEnv -> ExprEnv -> String -> String -> BackendType -> FunctionForm -> LowerM LowerValue
lowerInstantiatedFunctionValue env exprEnv context name resultTy form = do
  unless (null (ffParams form)) $
    liftEither (BackendLLVMUnsupportedExpression context ("escaping function " ++ show name))
  unless (alphaEqBackendType resultTy (ffReturnType form)) $
    liftEither (BackendLLVMInternalError ("value type mismatch for " ++ name ++ " at " ++ context))
  value <- lowerExpr env exprEnv context (ffBody form)
  expectedTy <- lowerBackendTypeM env context resultTy
  unless (lvLLVMType value == expectedTy) $
    liftEither (BackendLLVMInternalError ("value LLVM type mismatch for " ++ name ++ " at " ++ context))
  pure value

bindLet :: ProgramEnv -> ExprEnv -> String -> String -> BackendExpr -> LowerM ExprEnv
bindLet env exprEnv context name rhs =
  case functionFormFromExpected (backendExprType rhs) rhs of
    form
      | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
          pure
            exprEnv
              { eeLocalFunctions =
                  Map.insert
                    name
                    LocalFunction {lfForm = form, lfCapturedEnv = exprEnv}
                    (eeLocalFunctions exprEnv),
                eeValues = Map.delete name (eeValues exprEnv)
              }
    _ -> do
      value <- lowerExpr env exprEnv (context ++ ", let " ++ show name) rhs
      pure
        exprEnv
          { eeValues = Map.insert name value (eeValues exprEnv),
            eeLocalFunctions = Map.delete name (eeLocalFunctions exprEnv)
          }

lowerVar :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> LowerM LowerValue
lowerVar env exprEnv context ty name =
  case Map.lookup name (eeValues exprEnv) of
    Just value -> pure value
    Nothing ->
      case Map.lookup name (eeLocalFunctions exprEnv) of
        Just localFunction ->
          lowerLocalFunctionValue env context ty name localFunction []
        Nothing ->
          case Map.lookup name (pbBindings (peBase env)) of
            Just binding ->
              lowerGlobalValue env context ty name binding []
            Nothing ->
              liftEither (BackendLLVMUnknownFunction name)

lowerGlobalValue :: ProgramEnv -> String -> BackendType -> String -> BindingInfo -> [BackendType] -> LowerM LowerValue
lowerGlobalValue env context resultTy name binding typeArgs =
  case (ffTypeBinders form, typeArgs) of
    ([], []) ->
      lowerInstantiatedGlobalValue resultTy name binding [] form
    ([], _ : _) ->
      liftEither (BackendLLVMUnsupportedCall ("unexpected type arguments at " ++ context))
    (_ : _, []) ->
      liftEither (BackendLLVMUnsupportedExpression context ("escaping polymorphic binding " ++ show name))
    (_ : _, _) -> do
      (resolvedTypeArgs, instantiated) <- instantiateFunctionFormWithTypeArgsM context form typeArgs []
      lowerInstantiatedGlobalValue resultTy name binding resolvedTypeArgs instantiated
  where
    form = biForm binding

    lowerInstantiatedGlobalValue expectedTy functionContext binding0 resolvedTypeArgs instantiated = do
      unless (null (ffParams instantiated)) $
        liftEither (BackendLLVMUnsupportedExpression context ("escaping function " ++ show functionContext))
      unless (alphaEqBackendType expectedTy (ffReturnType instantiated)) $
        liftEither (BackendLLVMInternalError ("global value type mismatch for " ++ functionContext ++ " at " ++ context))
      resultLLVMType <- lowerBackendTypeM env context expectedTy
      functionName <- globalFunctionName env context binding0 resolvedTypeArgs
      result <- emitAssign "call" resultLLVMType (LLVMCall functionName [])
      pure (LowerValue expectedTy resultLLVMType result)

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
              case Map.lookup name (eeValues exprEnv) of
                Just value
                  | isFunctionLikeBackendType (lvBackendType value) ->
                      lowerIndirectValueCall env exprEnv context name value typeArgs args
                _ ->
                  lowerGlobalCall env exprEnv context name typeArgs args
        BackendLam {} ->
          lowerDirectFunctionCall env exprEnv context (functionFormFromExpr headExpr) typeArgs args
        BackendTyAbs {} ->
          lowerDirectFunctionCall env exprEnv context (functionFormFromExpr headExpr) typeArgs args
        _ ->
          case pushCallIntoExpression context (backendExprType expr) headExpr typeArgs args of
            Right (Just applied) ->
              lowerExpr env exprEnv context applied
            Right Nothing ->
              liftEither (BackendLLVMUnsupportedCall ("unsupported call head at " ++ context))
            Left err ->
              liftEither err

lowerIndirectValueCall :: ProgramEnv -> ExprEnv -> String -> String -> LowerValue -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerIndirectValueCall env exprEnv context name callee typeArgs args = do
  unless (isEvidenceCallableName name || isFirstOrderFunctionPointerType (lvBackendType callee)) $
    liftEither (BackendLLVMUnsupportedExpression context ("escaping function value " ++ show name))
  form <- instantiateFunctionFormM context (functionFormFromType (lvBackendType callee)) typeArgs args
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  case indirectCalleeFunctionForm env callee of
    Just calleeForm0 -> do
      calleeForm <- instantiateFunctionFormM context calleeForm0 typeArgs args
      if requiresInlineCall calleeForm
        then do
          bodyEnv <- bindCallArguments env exprEnv exprEnv context False name calleeForm args
          lowerExpr env bodyEnv context (ffBody calleeForm)
        else lowerIndirectPointerCall form
    Nothing ->
      lowerIndirectPointerCall form
  where
    lowerIndirectPointerCall form = do
      callArgs <- zipWithM (lowerExprForIndirectArgument env exprEnv context) (ffParams form) args
      bindIndirectFunctionArguments env context name form callArgs
      resultTy <- lowerBackendTypeM env context (ffReturnType form)
      result <- emitAssign "call" resultTy (LLVMCallOperand (lvOperand callee) [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
      pure (LowerValue (ffReturnType form) resultTy result)

indirectCalleeFunctionForm :: ProgramEnv -> LowerValue -> Maybe FunctionForm
indirectCalleeFunctionForm env callee =
  case lvOperand callee of
    LLVMGlobalRef _ functionName ->
      lookupFunctionFormByName env functionName
    _ ->
      Nothing

lookupFunctionFormByName :: ProgramEnv -> String -> Maybe FunctionForm
lookupFunctionFormByName env functionName =
  case Map.lookup functionName (pbBindings (peBase env)) of
    Just binding -> Just (biForm binding)
    Nothing ->
      case [spForm specialization | specialization <- Map.elems (peSpecializations env), spFunctionName specialization == functionName] of
        form : _ -> Just form
        [] ->
          case [evidenceWrapperForm wrapper | wrapper <- Map.elems (peEvidenceWrappers env), ewFunctionName wrapper == functionName] of
            form : _ -> Just form
            [] -> Nothing

functionFormFromType :: BackendType -> FunctionForm
functionFormFromType ty =
  FunctionForm
    { ffTypeBinders = typeBinders,
      ffParams = zip paramNames params,
      ffBody = BackendVar returnTy "__mlfp_callable_result",
      ffReturnType = returnTy
    }
  where
    (typeBinders, afterForalls) = collectForallsType ty
    (params, returnTy) = collectArrowsType afterForalls
    paramNames = ["__mlfp_callable_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]

lowerExprForArgument :: ProgramEnv -> ExprEnv -> String -> Bool -> (String, BackendType) -> BackendExpr -> LowerM LowerValue
lowerExprForArgument env exprEnv context allowNestedEvidence (paramName, paramTy) arg
  | isEvidenceArgument allowNestedEvidence paramName paramTy =
      lowerEvidenceArgument env exprEnv context paramTy arg
  | otherwise =
      lowerExpr env exprEnv context arg

lowerExprForIndirectArgument :: ProgramEnv -> ExprEnv -> String -> (String, BackendType) -> BackendExpr -> LowerM LowerValue
lowerExprForIndirectArgument env exprEnv context (paramName, paramTy) arg
  | isEvidenceArgument False paramName paramTy =
      lowerEvidenceArgument env exprEnv context paramTy arg
  | isFunctionLikeBackendType paramTy =
      case Map.lookup (evidenceWrapperKey paramTy arg) (peEvidenceWrappers env) of
        Just wrapper ->
          pure (LowerValue paramTy LLVMPtr (LLVMGlobalRef LLVMPtr (ewFunctionName wrapper)))
        Nothing ->
          lowerFunctionArgument env exprEnv context paramTy arg
  | otherwise =
      lowerExpr env exprEnv context arg

lowerFunctionArgument :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerFunctionArgument env exprEnv context expectedTy arg =
  case collectTyApps arg of
    (BackendVar _ name, typeArgs) ->
      lowerFunctionReference env exprEnv context expectedTy name typeArgs
    _ ->
      liftEither (BackendLLVMUnsupportedExpression context "unsupported function argument")

lowerEvidenceArgument :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerEvidenceArgument env exprEnv context expectedTy arg =
  case collectTyApps arg of
    (BackendVar _ name, typeArgs) ->
      lowerFunctionReference env exprEnv context expectedTy name typeArgs
    _ ->
      case Map.lookup (evidenceWrapperKey expectedTy arg) (peEvidenceWrappers env) of
        Just wrapper ->
          pure (LowerValue expectedTy LLVMPtr (LLVMGlobalRef LLVMPtr (ewFunctionName wrapper)))
        Nothing ->
          liftEither (BackendLLVMUnsupportedExpression context "unsupported evidence function argument")

lowerFunctionReference :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> [BackendType] -> LowerM LowerValue
lowerFunctionReference env exprEnv context expectedTy name typeArgs =
  case Map.lookup name (eeLocalFunctions exprEnv) of
    Just localFunction ->
      lowerLocalFunctionReference env context expectedTy name localFunction typeArgs
    Nothing ->
      lowerNonLocalFunctionReference env exprEnv context expectedTy name typeArgs

lowerLocalFunctionReference :: ProgramEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> LowerM LowerValue
lowerLocalFunctionReference env context expectedTy name localFunction typeArgs = do
  form <-
    if null typeArgs
      then pure (lfForm localFunction)
      else instantiateFunctionFormM context (lfForm localFunction) typeArgs []
  let actualTy = functionTypeFromFormWithBinders form
  requireEvidenceFunctionType context name expectedTy actualTy
  case etaAliasTarget form of
    Just (targetName, targetTypeArgs) ->
      lowerFunctionReference env (lfCapturedEnv localFunction) context expectedTy targetName targetTypeArgs
    Nothing ->
      liftEither (BackendLLVMUnsupportedExpression context ("unsupported function argument " ++ show name))

etaAliasTarget :: FunctionForm -> Maybe (String, [BackendType])
etaAliasTarget form =
  case collectValueApps (ffBody form) of
    (headExpr, args)
      | mapMaybe backendVarExprName args == map fst (ffParams form),
        length args == length (ffParams form) ->
          case collectTyApps headExpr of
            (BackendVar _ targetName, targetTypeArgs) ->
              Just (targetName, eraseAliasBinderTypeArgs targetTypeArgs)
            _ ->
              Nothing
    _ ->
      Nothing
  where
    binderTypeArgs =
      [BTVar name | (name, _) <- ffTypeBinders form]
    eraseAliasBinderTypeArgs targetTypeArgs
      | targetTypeArgs == binderTypeArgs = []
      | otherwise = targetTypeArgs

collectValueApps :: BackendExpr -> (BackendExpr, [BackendExpr])
collectValueApps =
  go []
  where
    go args =
      \case
        BackendApp _ fun arg -> go (arg : args) fun
        expr -> (expr, args)

backendVarExprName :: BackendExpr -> Maybe String
backendVarExprName =
  \case
    BackendVar _ name -> Just name
    _ -> Nothing

lowerNonLocalFunctionReference :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> [BackendType] -> LowerM LowerValue
lowerNonLocalFunctionReference env exprEnv context expectedTy name typeArgs =
  case Map.lookup name (eeValues exprEnv) of
    Just value
      | isFunctionLikeBackendType (lvBackendType value) ->
          lowerValueFunctionReference context expectedTy name value typeArgs
    _ ->
      case Map.lookup name (pbBindings (peBase env)) of
        Just binding -> do
          (resolvedTypeArgs, form) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs []
          let actualTy = functionTypeFromForm form
          requireEvidenceFunctionType context name expectedTy actualTy
          functionName <- globalFunctionName env context binding resolvedTypeArgs
          pure (LowerValue expectedTy LLVMPtr (LLVMGlobalRef LLVMPtr functionName))
        Nothing ->
          liftEither (BackendLLVMUnknownFunction name)

lowerValueFunctionReference :: String -> BackendType -> String -> LowerValue -> [BackendType] -> LowerM LowerValue
lowerValueFunctionReference context expectedTy name value typeArgs = do
  actualTy <-
    if null typeArgs
      then pure (lvBackendType value)
      else instantiateCallableTypeM context (lvBackendType value) typeArgs []
  requireEvidenceFunctionType context name expectedTy actualTy
  pure (LowerValue expectedTy LLVMPtr (lvOperand value))

instantiateCallableTypeM :: String -> BackendType -> [BackendType] -> [BackendExpr] -> LowerM BackendType
instantiateCallableTypeM context ty typeArgs args = do
  form <- instantiateFunctionFormM context (functionFormFromType ty) typeArgs args
  pure (functionTypeFromForm form)

functionTypeFromForm :: FunctionForm -> BackendType
functionTypeFromForm form =
  foldr BTArrow (ffReturnType form) (map snd (ffParams form))

functionTypeFromFormWithBinders :: FunctionForm -> BackendType
functionTypeFromFormWithBinders form =
  foldr
    (\(name, mbBound) body -> BTForall name mbBound body)
    (functionTypeFromForm form)
    (ffTypeBinders form)

requireEvidenceFunctionType :: String -> String -> BackendType -> BackendType -> LowerM ()
requireEvidenceFunctionType context name expected actual =
  unless (evidenceFunctionTypesCompatible expected actual) $
    liftEither
      ( BackendLLVMInternalError
          ( "evidence function type mismatch for "
              ++ name
              ++ " at "
              ++ context
              ++ ": expected "
              ++ show expected
              ++ ", got "
              ++ show actual
          )
      )

evidenceFunctionTypesCompatible :: BackendType -> BackendType -> Bool
evidenceFunctionTypesCompatible expected actual =
  alphaEqBackendType expected actual || sameFunctionShape expected actual
  where
    sameFunctionShape left right =
      case (left, right) of
        (BTForall _ _ leftBody, BTForall _ _ rightBody) ->
          sameFunctionShape leftBody rightBody
        (BTArrow leftParam leftResult, BTArrow rightParam rightResult) ->
          runtimeCompatibleValueType leftParam rightParam && sameFunctionShape leftResult rightResult
        _ ->
          runtimeCompatibleValueType left right

runtimeCompatibleValueType :: BackendType -> BackendType -> Bool
runtimeCompatibleValueType left right =
  alphaEqBackendType left right
    || case (left, right) of
      (BTMu {}, BTMu {}) -> True
      (BTArrow {}, BTArrow {}) -> True
      (BTForall {}, BTForall {}) -> True
      (BTBase leftBase, BTBase rightBase) -> leftBase == rightBase
      (BTCon leftCon leftArgs, BTCon rightCon rightArgs) ->
        leftCon == rightCon
          && length leftArgs == length rightArgs
          && and (zipWith runtimeCompatibleValueType (NE.toList leftArgs) (NE.toList rightArgs))
      (BTBottom, BTBottom) -> True
      _ -> False

lowerLocalFunctionCall :: ProgramEnv -> ExprEnv -> String -> String -> LocalFunction -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerLocalFunctionCall env callEnv context name localFunction typeArgs args = do
  let allowNestedEvidence = isEvidenceName name
  form <- instantiateFunctionFormM context (lfForm localFunction) typeArgs args
  bodyEnv <- bindCallArguments env callEnv (lfCapturedEnv localFunction) context allowNestedEvidence name form args
  lowerExpr env bodyEnv context (ffBody form)

lowerDirectFunctionCall :: ProgramEnv -> ExprEnv -> String -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerDirectFunctionCall env exprEnv context form0 typeArgs args = do
  form <- instantiateFunctionFormM context form0 typeArgs args
  bodyEnv <- bindCallArguments env exprEnv exprEnv context False "lambda" form args
  lowerExpr env bodyEnv context (ffBody form)

lowerGlobalCall :: ProgramEnv -> ExprEnv -> String -> String -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerGlobalCall env exprEnv context name typeArgs args =
  case Map.lookup name (pbBindings (peBase env)) of
    Just binding -> do
      (resolvedTypeArgs, form) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs args
      unless (length args == length (ffParams form)) $
        liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
      if shouldInlineGlobalCall env exprEnv binding resolvedTypeArgs form args
        then do
          bodyEnv <- bindCallArguments env exprEnv exprEnv context False name form args
          lowerExpr env bodyEnv context (ffBody form)
        else do
          callArgs <- zipWithM (lowerExprForArgument env exprEnv context False) (ffParams form) args
          bindFunctionArguments env context False name form callArgs
          resultTy <- lowerBackendTypeM env context (ffReturnType form)
          functionName <- globalFunctionName env context binding resolvedTypeArgs
          result <- emitAssign "call" resultTy (LLVMCall functionName [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
          pure (LowerValue (ffReturnType form) resultTy result)
    Nothing
      | name == runtimeAndName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          let expectedTypes = [LLVMInt 1, LLVMInt 1]
          zipWithM_ (requireLLVMType context name) expectedTypes callArgs
          result <- emitAssign "call" (LLVMInt 1) (LLVMCall runtimeAndName [(LLVMInt 1, lvOperand arg) | arg <- callArgs])
          pure (LowerValue (BTBase (BaseTy "Bool")) (LLVMInt 1) result)
    Nothing ->
      liftEither (BackendLLVMUnknownFunction name)

shouldInlineGlobalCall :: ProgramEnv -> ExprEnv -> BindingInfo -> [BackendType] -> FunctionForm -> [BackendExpr] -> Bool
shouldInlineGlobalCall env exprEnv binding resolvedTypeArgs form args =
  requiresInlineCall form
    || missingPolymorphicSpecialization env binding resolvedTypeArgs
    || any (evidenceArgumentRequiresInline env exprEnv) (zip (ffParams form) args)

missingPolymorphicSpecialization :: ProgramEnv -> BindingInfo -> [BackendType] -> Bool
missingPolymorphicSpecialization env binding resolvedTypeArgs =
  not (null (ffTypeBinders (biForm binding)))
    && Map.notMember (specializationKey (SpecRequest (biName binding) resolvedTypeArgs)) (peSpecializations env)

evidenceArgumentRequiresInline :: ProgramEnv -> ExprEnv -> ((String, BackendType), BackendExpr) -> Bool
evidenceArgumentRequiresInline env exprEnv ((paramName, paramTy), arg) =
  isEvidenceArgument False paramName paramTy && functionArgumentRequiresInline env exprEnv arg

functionArgumentRequiresInline :: ProgramEnv -> ExprEnv -> BackendExpr -> Bool
functionArgumentRequiresInline env exprEnv arg =
  case collectTyApps arg of
    (BackendVar _ name, typeArgs)
      | Just localFunction <- Map.lookup name (eeLocalFunctions exprEnv) ->
          requiresInlineCall (lfForm localFunction)
      | Just binding <- Map.lookup name (pbBindings (peBase env)) ->
          case instantiateFunctionFormWithTypeArgs "inline evidence argument" (biForm binding) typeArgs [] of
            Right (_, form) -> requiresInlineCall form
            Left _ -> False
    _ -> False

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

bindFunctionArguments :: ProgramEnv -> String -> Bool -> String -> FunctionForm -> [LowerValue] -> LowerM ()
bindFunctionArguments env context allowNestedEvidence name form args = do
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  expectedTypes <- traverse (\(paramName, paramTy) -> lowerFunctionParameterTypeM env context allowNestedEvidence paramName paramTy) (ffParams form)
  zipWithM_ (requireLLVMType context name) expectedTypes args

bindIndirectFunctionArguments :: ProgramEnv -> String -> String -> FunctionForm -> [LowerValue] -> LowerM ()
bindIndirectFunctionArguments env context name form args = do
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  expectedTypes <- traverse (lowerIndirectFunctionParameterTypeM env context . snd) (ffParams form)
  zipWithM_ (requireLLVMType context name) expectedTypes args

lowerIndirectFunctionParameterTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerIndirectFunctionParameterTypeM env context paramTy
  | isFunctionLikeBackendType paramTy = pure LLVMPtr
  | otherwise =
      case lowerBackendType env context paramTy of
        Right llvmTy -> pure llvmTy
        Left err -> liftEither err

bindCallArguments ::
  ProgramEnv ->
  ExprEnv ->
  ExprEnv ->
  String ->
  Bool ->
  String ->
  FunctionForm ->
  [BackendExpr] ->
  LowerM ExprEnv
bindCallArguments env callEnv bodyEnv0 context allowNestedEvidence name form args = do
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  foldM bindOne bodyEnv0 (zip (ffParams form) args)
  where
    bindOne bodyEnv ((paramName, paramTy), arg)
      | isInlineFunctionArgument allowNestedEvidence paramName paramTy = do
          let argForm = functionFormFromExpected paramTy arg
          pure
            bodyEnv
              { eeLocalFunctions =
                  Map.insert
                    paramName
                    LocalFunction {lfForm = argForm, lfCapturedEnv = callEnv}
                    (eeLocalFunctions bodyEnv),
                eeValues = Map.delete paramName (eeValues bodyEnv)
              }
      | otherwise = do
          value <- lowerExprForArgument env callEnv context allowNestedEvidence (paramName, paramTy) arg
          pure
            bodyEnv
              { eeValues = Map.insert paramName value (eeValues bodyEnv),
                eeLocalFunctions = Map.delete paramName (eeLocalFunctions bodyEnv)
              }

isInlineFunctionArgument :: Bool -> String -> BackendType -> Bool
isInlineFunctionArgument allowNestedEvidence paramName paramTy =
  isInlineOnlyFunctionParameter allowNestedEvidence paramName paramTy

isInlineOnlyFunctionParameter :: Bool -> String -> BackendType -> Bool
isInlineOnlyFunctionParameter allowNestedEvidence paramName paramTy =
  isFunctionLikeBackendType paramTy
    && (evidenceNeedsInlining || firstOrderFunctionNeedsInlining)
  where
    evidenceLike = isEvidenceArgument allowNestedEvidence paramName paramTy
    polymorphicFunction = hasTypeBinders paramTy
    evidenceNeedsInlining = evidenceLike && polymorphicFunction
    firstOrderFunctionNeedsInlining = not evidenceLike && not polymorphicFunction

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

instantiateFunctionFormM :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM FunctionForm
instantiateFunctionFormM context form typeArgs args =
  case instantiateFunctionFormWithTypeArgs context form typeArgs args of
    Right (_, instantiated) -> pure instantiated
    Left err -> liftEither err

instantiateFunctionFormWithTypeArgsM :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM ([BackendType], FunctionForm)
instantiateFunctionFormWithTypeArgsM context form typeArgs args =
  case instantiateFunctionFormWithTypeArgs context form typeArgs args of
    Right instantiated -> pure instantiated
    Left err -> liftEither err

instantiateFunctionForm :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError FunctionForm
instantiateFunctionForm context form typeArgs args =
  snd <$> instantiateFunctionFormWithTypeArgs context form typeArgs args

instantiateFunctionFormWithTypeArgs :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError ([BackendType], FunctionForm)
instantiateFunctionFormWithTypeArgs context form typeArgs args = do
  substitution <- resolveTypeArguments context form typeArgs args
  resolvedTypeArgs <- resolvedTypeArguments context (map fst (ffTypeBinders form)) substitution
  let substituteTy = substituteBackendTypes substitution
      instantiated =
        FunctionForm
          { ffTypeBinders = [],
            ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
            ffBody = substituteExprTypes substitution (ffBody form),
            ffReturnType = substituteTy (ffReturnType form)
          }
  pure (resolvedTypeArgs, instantiated)

resolvedTypeArguments :: String -> [String] -> Map String BackendType -> Either BackendLLVMError [BackendType]
resolvedTypeArguments context binderNames substitution =
  traverse lookupResolved binderNames
  where
    lookupResolved name =
      case Map.lookup name substitution of
        Just ty -> Right ty
        Nothing -> Left (BackendLLVMInternalError ("missing resolved type argument " ++ show name ++ " at " ++ context))

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
      object <- emitMalloc env context (8 * (1 + length args))
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
  rejectNonTailDefaultAlternative
  resultLLVMType <- lowerBackendTypeM env context resultTy
  scrutineeValue <- lowerExpr env exprEnv context scrutinee
  unless (lvLLVMType scrutineeValue == LLVMPtr) $
    liftEither (BackendLLVMUnsupportedType (context ++ " case scrutinee") (lvBackendType scrutineeValue))
  tagPtr <- emitGep "case.tag.ptr" (lvOperand scrutineeValue) 0
  tagValue <- emitAssign "case.tag" (LLVMInt 64) (LLVMLoad (LLVMInt 64) tagPtr)
  altLabels <- traverse (const (freshBlock "case.alt")) (NE.toList alternatives)
  defaultLabel <- maybe (freshBlock "case.default") pure (lookupDefaultLabel altLabels)
  joinLabel <- freshBlock "case.join"
  let constructorTargets = mapMaybe constructorSwitchTarget (zip (NE.toList alternatives) altLabels)
      switchTargets = [(tag, label) | (tag, label, _) <- constructorTargets]
  rejectDuplicateSwitchTargets constructorTargets
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

    rejectNonTailDefaultAlternative =
      case break isDefaultAlternative alternativesList of
        (_, []) -> pure ()
        (_, [_]) -> pure ()
        (_, _ : _ : _) ->
          liftEither (BackendLLVMUnsupportedExpression context "default case alternative must be last")

    isDefaultAlternative (BackendAlternative BackendDefaultPattern _) =
      True
    isDefaultAlternative _ =
      False

    lookupDefaultLabel labels =
      case [label | (BackendAlternative BackendDefaultPattern _, label) <- zip alternativesList labels] of
        label : _ -> Just label
        [] -> Nothing

    constructorSwitchTarget (BackendAlternative pattern0 _, label) =
      case pattern0 of
        BackendDefaultPattern -> Nothing
        BackendConstructorPattern name _ ->
          case Map.lookup name (pbConstructors (peBase env)) of
            Just constructorRuntime -> Just (crTag constructorRuntime, label, name)
            Nothing -> Nothing

    rejectDuplicateSwitchTargets targets =
      case firstDuplicate (map (\(tag, _, _) -> tag) targets) of
        Just tag ->
          liftEither (BackendLLVMUnsupportedExpression context ("duplicate constructor case tag " ++ show tag))
        Nothing ->
          pure ()

    lowerAlternative resultLLVMType joinLabel scrutineeValue alternative label = do
      startBlock label
      exprEnv' <- bindAlternativePattern scrutineeValue alternative
      bodyValue <- lowerExpr env exprEnv' context (backendAltBody alternative)
      unless (lvLLVMType bodyValue == resultLLVMType) $
        liftEither (BackendLLVMInternalError ("case alternative type mismatch at " ++ context))
      sourceLabel <- gets fsCurrentLabel
      finishCurrentBlock (LLVMBr joinLabel)
      pure [(lvOperand bodyValue, sourceLabel)]

    bindAlternativePattern scrutineeValue (BackendAlternative pattern0 body) =
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
              let usedBinders = freeBackendExprVars body
              loadedFields <- mapMaybe id <$> traverse (loadUsedField usedBinders scrutineeValue) (zip3 [0 :: Int ..] binders fieldTys)
              pure
                exprEnv
                  { eeValues =
                      Map.union
                        (Map.fromList loadedFields)
                        (eeValues exprEnv)
                  }

    loadUsedField usedBinders scrutineeValue (index0, binder, fieldTy)
      | Set.member binder usedBinders = do
          loaded <- loadField scrutineeValue index0 fieldTy
          pure (Just (binder, loaded))
      | otherwise =
          pure Nothing

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

emitMalloc :: ProgramEnv -> String -> Int -> LowerM LLVMOperand
emitMalloc env context size
  | Map.member runtimeMallocName (pbBindings (peBase env)) =
      liftEither (BackendLLVMUnsupportedExpression context ("reserved runtime binding " ++ show runtimeMallocName))
  | otherwise =
      emitAssign "malloc" LLVMPtr (LLVMCall runtimeMallocName [(LLVMInt 64, LLVMIntLiteral 64 (toInteger size))])

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
