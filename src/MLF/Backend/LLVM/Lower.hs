{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.LLVM.Lower
Description : Lower typed backend IR into real LLVM IR syntax
-}

{- Note [Closure ABI]
~~~~~~~~~~~~~~~~~~~~~
Backend closure values are heap pointers to a two-word record:

* word 0 stores the closure entry code pointer;
* word 1 stores the environment pointer, or null when the closure has no
  captures.

The environment object is owned by the closure value and contains one machine
word per captured runtime value in the order written in the backend IR. Closure
entry functions are private LLVM functions with debug-friendly names supplied
by `BackendClosure`; they take a hidden `ptr env` parameter before the erased
monomorphic runtime arguments. Direct first-order backend calls keep using
their existing first-order function symbols; indirect closure calls must use
the explicit `BackendClosureCall` node.
-}
module MLF.Backend.LLVM.Lower
  ( BackendLLVMError (..),
    evidenceFunctionTypesCompatible,
    inferTypeArgumentsForTest,
    lowerBackendProgram,
    lowerBackendProgramNative,
    renderBackendLLVMError,
  )
where

import Control.Monad (foldM, unless, when, zipWithM, zipWithM_)
import Control.Monad.State.Strict (StateT (StateT), evalStateT, get, gets, modify)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, ord)
import Data.List (intercalate, isPrefixOf, nub, sort, sortOn, stripPrefix)
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
  | BackendLLVMDuplicateSymbol String
  | BackendLLVMInternalError String
  deriving (Eq, Show)

data ProgramBase = ProgramBase
  { pbBindings :: Map String BindingInfo,
    pbBindingOrder :: [String],
    pbConstructors :: Map String ConstructorRuntime,
    pbData :: Map String DataRuntime,
    pbDataNames :: Set String
  }

data DataRuntime = DataRuntime
  { drData :: BackendData,
    drConstructors :: [ConstructorRuntime]
  }
  deriving (Eq, Show)

data ProgramEnv = ProgramEnv
  { peBase :: ProgramBase,
    peSpecializations :: Map String Specialization,
    peEvidenceWrappers :: Map String EvidenceWrapper,
    peFunctionWrappers :: Map String FunctionWrapper,
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

data FunctionWrapper = FunctionWrapper
  { fwKey :: String,
    fwFunctionName :: String,
    fwExpectedType :: BackendType,
    fwExpr :: BackendExpr
  }
  deriving (Eq, Show)

data ClosureEntry = ClosureEntry
  { ceFunctionType :: BackendType,
    ceEntryName :: String,
    ceCaptures :: [(String, BackendType)],
    ceParams :: [(String, BackendType)],
    ceBody :: BackendExpr
  }
  deriving (Eq, Show)

data LoweredProgram = LoweredProgram
  { lpBase :: ProgramBase,
    lpEnv :: ProgramEnv,
    lpMainBinding :: BindingInfo,
    lpFunctions :: [LLVMFunction]
  }

data NativeRenderSpec = NativeRenderSpec
  { nrsType :: BackendType,
    nrsFunctionName :: String
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
    lfCapturedEnv :: ExprEnv,
    lfStoredReference :: Maybe (BackendType, BackendExpr)
  }
  deriving (Eq, Show)

data ExprEnv = ExprEnv
  { eeValues :: Map String LowerValue,
    eeLocalFunctions :: Map String LocalFunction,
    eeActiveGlobalInlines :: Set String
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
  lowered <- lowerBackendProgramCore program
  validateLLVMModuleSymbols
    LLVMModule
      { llvmModuleGlobals = rawLLVMGlobals lowered,
        llvmModuleDeclarations = runtimeDeclarations (lpBase lowered),
        llvmModuleFunctions = lpFunctions lowered
      }

lowerBackendProgramNative :: BackendProgram -> Either BackendLLVMError LLVMModule
lowerBackendProgramNative program = do
  lowered <- lowerBackendProgramCore program
  lowerNativeProgram lowered

lowerBackendProgramCore :: BackendProgram -> Either BackendLLVMError LoweredProgram
lowerBackendProgramCore program = do
  first BackendLLVMValidationFailed (validateBackendProgram program)
  base <- buildProgramBase program
  reachable <- reachableBindings base (backendProgramMain program)
  specializations <- collectRequiredSpecializations base reachable
  let evidenceWrappers = collectEvidenceWrappers base reachable specializations
      functionWrappers = collectFunctionWrappers base reachable specializations
      closureEntries0 = collectClosureEntries reachable specializations evidenceWrappers functionWrappers
      referencedFunctionNames = collectReferencedFunctionNames base reachable specializations evidenceWrappers functionWrappers
      stringGlobals = assignStringGlobals (collectProgramStrings reachable specializations evidenceWrappers functionWrappers)
      env =
        ProgramEnv
          { peBase = base,
            peSpecializations = Map.fromList [(specializationKey (spRequest spec), spec) | spec <- specializations],
            peEvidenceWrappers = Map.fromList [(ewKey wrapper, wrapper) | wrapper <- evidenceWrappers],
            peFunctionWrappers = Map.fromList [(fwKey wrapper, wrapper) | wrapper <- functionWrappers],
            peStringGlobals = stringGlobals
          }
  closureEntries <- requireUniqueClosureEntries closureEntries0
  functions <-
    concat
      <$> sequence
        [ traverse (lowerMonomorphicBinding env) (filter (shouldLowerReachableBinding referencedFunctionNames) reachable),
          traverse (lowerSpecialization env) (filter (shouldLowerSpecialization referencedFunctionNames) specializations),
          traverse (lowerEvidenceWrapper env) evidenceWrappers,
          traverse (lowerFunctionWrapper env) functionWrappers,
          traverse (lowerClosureEntry env) closureEntries
        ]
  mainBinding <- requireBinding base (backendProgramMain program)
  when (not (null (ffTypeBinders (biForm mainBinding)))) $
    Left (BackendLLVMUnsupportedExpression "program main" "polymorphic main binding")
  pure
    LoweredProgram
      { lpBase = base,
        lpEnv = env,
        lpMainBinding = mainBinding,
        lpFunctions = functions
      }

rawLLVMGlobals :: LoweredProgram -> [LLVMGlobal]
rawLLVMGlobals lowered =
  [LLVMStringGlobal globalName value | (value, globalName) <- Map.toAscList (peStringGlobals (lpEnv lowered))]

lowerNativeProgram :: LoweredProgram -> Either BackendLLVMError LLVMModule
lowerNativeProgram lowered = do
  let base = lpBase lowered
      env = lpEnv lowered
      mainBinding = lpMainBinding lowered
      mainForm = biForm mainBinding
  unless (null (ffParams mainForm)) $
    Left (BackendLLVMUnsupportedExpression "native process main" "main must be a zero-argument pure value")
  renderSpecs <- collectNativeRenderSpecs base (ffReturnType mainForm)
  rejectNativeSymbolConflicts base renderSpecs
  let renderMap = Map.fromList [(backendTypeKey (nrsType spec), nrsFunctionName spec) | spec <- renderSpecs]
  renderers <- traverse (lowerNativeRenderer env renderMap) renderSpecs
  entrypoint <- lowerNativeEntrypoint env mainBinding renderMap
  validateLLVMModuleSymbols
    LLVMModule
      { llvmModuleGlobals = rawLLVMGlobals lowered ++ nativeGlobals base renderSpecs,
        llvmModuleDeclarations = nativeRuntimeDeclarations base,
        llvmModuleFunctions =
          lpFunctions lowered
            ++ nativeRuntimeFunctions base
            ++ renderers
            ++ [entrypoint]
      }

validateLLVMModuleSymbols :: LLVMModule -> Either BackendLLVMError LLVMModule
validateLLVMModuleSymbols module0 =
  case duplicateSymbols symbolNames of
    name : _ -> Left (BackendLLVMDuplicateSymbol name)
    [] -> Right module0
  where
    symbolNames =
      map llvmGlobalName (llvmModuleGlobals module0)
        ++ map llvmDeclarationName (llvmModuleDeclarations module0)
        ++ map llvmFunctionName (llvmModuleFunctions module0)

    duplicateSymbols =
      go . sort

    go [] = []
    go [_] = []
    go (x : y : rest)
      | x == y = x : go (dropWhile (== x) rest)
      | otherwise = go (y : rest)

nativeRuntimeDeclarations :: ProgramBase -> [LLVMDeclaration]
nativeRuntimeDeclarations base =
  [ LLVMDeclaration runtimeMallocName LLVMPtr [LLVMInt 64] False
    | Map.notMember runtimeMallocName (pbBindings base)
  ]
    ++ [LLVMDeclaration nativePrintfName (LLVMInt 32) [LLVMPtr] True]

nativeRuntimeFunctions :: ProgramBase -> [LLVMFunction]
nativeRuntimeFunctions base =
  [nativeAndFunction | Map.notMember runtimeAndName (pbBindings base)]

nativeCMainName :: String
nativeCMainName =
  "main"

nativePrintfName :: String
nativePrintfName =
  "printf"

nativeRenderPrefix :: String
nativeRenderPrefix =
  "__mlfp_native_render$"

nativeFmtIntName :: String
nativeFmtIntName =
  "__mlfp_native_fmt_i64"

nativeFmtStringName :: String
nativeFmtStringName =
  "__mlfp_native_fmt_str"

nativeStrTrueName :: String
nativeStrTrueName =
  "__mlfp_native_str_true"

nativeStrFalseName :: String
nativeStrFalseName =
  "__mlfp_native_str_false"

nativeStrNewlineName :: String
nativeStrNewlineName =
  "__mlfp_native_str_newline"

nativeStrSpaceName :: String
nativeStrSpaceName =
  "__mlfp_native_str_space"

nativeStrOpenParenName :: String
nativeStrOpenParenName =
  "__mlfp_native_str_open_paren"

nativeStrCloseParenName :: String
nativeStrCloseParenName =
  "__mlfp_native_str_close_paren"

nativeGlobals :: ProgramBase -> [NativeRenderSpec] -> [LLVMGlobal]
nativeGlobals base renderSpecs =
  [ LLVMStringGlobal nativeFmtIntName "%ld",
    LLVMStringGlobal nativeFmtStringName "%s",
    LLVMStringGlobal nativeStrTrueName "true",
    LLVMStringGlobal nativeStrFalseName "false",
    LLVMStringGlobal nativeStrNewlineName "\n",
    LLVMStringGlobal nativeStrSpaceName " ",
    LLVMStringGlobal nativeStrOpenParenName "(",
    LLVMStringGlobal nativeStrCloseParenName ")"
  ]
    ++ concatMap (constructorNameGlobals base) renderSpecs

constructorNameGlobals :: ProgramBase -> NativeRenderSpec -> [LLVMGlobal]
constructorNameGlobals base spec =
  case nativeDataRuntimeForTypeName (nrsType spec) of
    Nothing -> []
    Just dataName ->
      case Map.lookup dataName (pbData base) of
        Nothing -> []
        Just dataRuntime0 ->
          [ LLVMStringGlobal (nativeConstructorGlobalName spec constructorRuntime) (displayConstructorName dataRuntime0 constructorRuntime)
          | constructorRuntime <- drConstructors dataRuntime0
          ]

nativeDataRuntimeForTypeName :: BackendType -> Maybe String
nativeDataRuntimeForTypeName =
  \case
    BTBase (BaseTy name) -> Just name
    BTCon (BaseTy name) _ -> Just name
    BTMu name _ -> structuralMuDataName name
    _ -> Nothing

nativeConstructorGlobalName :: NativeRenderSpec -> ConstructorRuntime -> String
nativeConstructorGlobalName spec constructorRuntime =
  "__mlfp_native_ctor$" ++ backendTypeKey (nrsType spec) ++ "$" ++ show (crTag constructorRuntime)

nativeRendererName :: BackendType -> String
nativeRendererName ty =
  nativeRenderPrefix ++ backendTypeKey ty

rejectNativeSymbolConflicts :: ProgramBase -> [NativeRenderSpec] -> Either BackendLLVMError ()
rejectNativeSymbolConflicts base renderSpecs =
  case [name | name <- Map.keys (pbBindings base), nativeNameConflicts name] of
    name : _ ->
      Left (BackendLLVMUnsupportedExpression "native process" ("reserved native LLVM symbol " ++ show name))
    [] -> Right ()
  where
    reservedNames =
      Set.fromList
        ( [ nativeCMainName,
            nativePrintfName,
            nativeFmtIntName,
            nativeFmtStringName,
            nativeStrTrueName,
            nativeStrFalseName,
            nativeStrNewlineName,
            nativeStrSpaceName,
            nativeStrOpenParenName,
            nativeStrCloseParenName
          ]
            ++ map nrsFunctionName renderSpecs
        )

    nativeNameConflicts name =
      Set.member name reservedNames
        || nativeRenderPrefix `isPrefixOf` name
        || "__mlfp_native_" `isPrefixOf` name

collectNativeRenderSpecs :: ProgramBase -> BackendType -> Either BackendLLVMError [NativeRenderSpec]
collectNativeRenderSpecs base rootTy =
  reverse . fst <$> go Set.empty [] rootTy
  where
    go seen specs ty
      | Set.member key seen = Right (specs, seen)
      | otherwise =
          case nativeRenderableKind base ty of
            NativeScalar ->
              Right (NativeRenderSpec ty (nativeRendererName ty) : specs, Set.insert key seen)
            NativeData dataRuntime0 -> do
              let seen' = Set.insert key seen
                  spec = NativeRenderSpec ty (nativeRendererName ty)
              foldM (collectConstructorFields ty) (spec : specs, seen') (drConstructors dataRuntime0)
            NativeUnsupported detail ->
              Left (BackendLLVMUnsupportedExpression "native result rendering" detail)
      where
        key = backendTypeKey ty

    collectConstructorFields resultTy (specs, seen) constructorRuntime = do
      fieldTys <-
        case constructorRuntimeFieldTypes constructorRuntime resultTy of
          Just tys -> Right tys
          Nothing ->
            Left
              ( BackendLLVMUnsupportedExpression
                  "native result rendering"
                  ("could not match constructor result for " ++ backendConstructorName (crConstructor constructorRuntime))
              )
      foldM
        ( \(specsAcc, seenAcc) fieldTy -> do
            go seenAcc specsAcc fieldTy
        )
        (specs, seen)
        fieldTys

data NativeRenderableKind
  = NativeScalar
  | NativeData DataRuntime
  | NativeUnsupported String

nativeRenderableKind :: ProgramBase -> BackendType -> NativeRenderableKind
nativeRenderableKind base ty =
  case ty of
    BTBase (BaseTy "Int") -> NativeScalar
    BTBase (BaseTy "Bool") -> NativeScalar
    BTBase (BaseTy "String") -> NativeUnsupported "String main values are not supported by native result rendering yet"
    BTBase (BaseTy name) ->
      maybe (NativeUnsupported ("unknown native result type " ++ show name)) NativeData (Map.lookup name (pbData base))
    BTCon (BaseTy name) _ ->
      maybe (NativeUnsupported ("unknown native result type " ++ show name)) NativeData (Map.lookup name (pbData base))
    BTArrow {} -> NativeUnsupported "function main values are not native-renderable"
    BTForall {} -> NativeUnsupported "polymorphic main values are not native-renderable"
    BTVar {} -> NativeUnsupported "type-variable main values are not native-renderable"
    BTVarApp {} -> NativeUnsupported "variable-headed main values are not native-renderable"
    BTMu name _ ->
      case structuralMuDataName name >>= (`Map.lookup` pbData base) of
        Just dataRuntime0 -> NativeData dataRuntime0
        Nothing -> NativeUnsupported "structural recursive main values are not native-renderable"
    BTBottom -> NativeUnsupported "bottom main values are not native-renderable"

lowerNativeRenderer :: ProgramEnv -> Map String String -> NativeRenderSpec -> Either BackendLLVMError LLVMFunction
lowerNativeRenderer env renderMap spec =
  case nativeRenderableKind (peBase env) (nrsType spec) of
    NativeScalar ->
      lowerNativeScalarRenderer spec
    NativeData dataRuntime0 ->
      lowerNativeDataRenderer env renderMap spec dataRuntime0
    NativeUnsupported detail ->
      Left (BackendLLVMUnsupportedExpression "native result rendering" detail)

lowerNativeScalarRenderer :: NativeRenderSpec -> Either BackendLLVMError LLVMFunction
lowerNativeScalarRenderer spec =
  case nrsType spec of
    BTBase (BaseTy "Int") ->
      lowerNativeFunction
        (nrsFunctionName spec)
        (LLVMInt 32)
        [(LLVMInt 64, "value"), (LLVMInt 1, "parenthesize")]
        $ \params -> do
          let value = requireNativeParam "value" params
          _ <- emitPrintf nativeFmtIntName [(LLVMInt 64, value)]
          finishNativeSuccess
    BTBase (BaseTy "Bool") ->
      lowerNativeFunction
        (nrsFunctionName spec)
        (LLVMInt 32)
        [(LLVMInt 1, "value"), (LLVMInt 1, "parenthesize")]
        $ \params -> do
          let value = requireNativeParam "value" params
          trueLabel <- freshBlock "bool.true"
          falseLabel <- freshBlock "bool.false"
          finishCurrentBlock (LLVMSwitch (LLVMInt 1) value falseLabel [(1, trueLabel)])
          startBlock trueLabel
          _ <- emitPrintStringGlobal nativeStrTrueName
          finishNativeSuccess
          startBlock falseLabel
          _ <- emitPrintStringGlobal nativeStrFalseName
          finishNativeSuccess
    _ ->
      Left (BackendLLVMUnsupportedExpression "native result rendering" ("unsupported scalar renderer " ++ show (nrsType spec)))

lowerNativeDataRenderer :: ProgramEnv -> Map String String -> NativeRenderSpec -> DataRuntime -> Either BackendLLVMError LLVMFunction
lowerNativeDataRenderer env renderMap spec dataRuntime0 =
  lowerNativeFunction
    (nrsFunctionName spec)
    (LLVMInt 32)
    [(LLVMPtr, "value"), (LLVMInt 1, "parenthesize")]
    $ \params -> do
      let value = requireNativeParam "value" params
          parenthesize = requireNativeParam "parenthesize" params
      tagPtr <- emitGep "native.tag.ptr" value 0
      tagValue <- emitAssign "native.tag" (LLVMInt 64) (LLVMLoad (LLVMInt 64) tagPtr)
      altLabels <- traverse (const (freshBlock "native.ctor")) (drConstructors dataRuntime0)
      defaultLabel <- freshBlock "native.unknown"
      let switchTargets = [(crTag constructorRuntime, label) | (constructorRuntime, label) <- zip (drConstructors dataRuntime0) altLabels]
      finishCurrentBlock (LLVMSwitch (LLVMInt 64) tagValue defaultLabel switchTargets)
      zipWithM_ (lowerNativeConstructorRenderer env renderMap spec value parenthesize) (drConstructors dataRuntime0) altLabels
      startBlock defaultLabel
      finishCurrentBlock LLVMUnreachable

lowerNativeConstructorRenderer ::
  ProgramEnv ->
  Map String String ->
  NativeRenderSpec ->
  LLVMOperand ->
  LLVMOperand ->
  ConstructorRuntime ->
  String ->
  LowerM ()
lowerNativeConstructorRenderer env renderMap spec value parenthesize constructorRuntime label = do
  fieldTys <-
    case constructorRuntimeFieldTypes constructorRuntime (nrsType spec) of
      Just tys -> pure tys
      Nothing ->
        liftEither
          ( BackendLLVMUnsupportedExpression
              "native result rendering"
              ("could not match constructor result for " ++ backendConstructorName (crConstructor constructorRuntime))
          )
  startBlock label
  if null fieldTys
    then do
      _ <- emitPrintStringGlobal (nativeConstructorGlobalName spec constructorRuntime)
      finishNativeSuccess
    else do
      openLabel <- freshBlock "native.open"
      bodyLabel <- freshBlock "native.body"
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) parenthesize bodyLabel [(1, openLabel)])
      startBlock openLabel
      _ <- emitPrintStringGlobal nativeStrOpenParenName
      finishCurrentBlock (LLVMBr bodyLabel)
      startBlock bodyLabel
      _ <- emitPrintStringGlobal (nativeConstructorGlobalName spec constructorRuntime)
      zipWithM_ (printField fieldTys) [0 :: Int ..] fieldTys
      closeLabel <- freshBlock "native.close"
      doneLabel <- freshBlock "native.done"
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) parenthesize doneLabel [(1, closeLabel)])
      startBlock closeLabel
      _ <- emitPrintStringGlobal nativeStrCloseParenName
      finishNativeSuccess
      startBlock doneLabel
      finishNativeSuccess
  where
    printField _ index0 fieldTy = do
      _ <- emitPrintStringGlobal nativeStrSpaceName
      fieldLLVMType <- lowerBackendTypeM env "native result field" fieldTy
      fieldPtr <- emitGep "native.field.ptr" value (8 * (index0 + 1))
      fieldValue <- emitAssign "native.field" fieldLLVMType (LLVMLoad fieldLLVMType fieldPtr)
      callNativeRenderer renderMap fieldTy fieldLLVMType fieldValue (nativeFieldParenthesize (peBase env) fieldTy)

nativeFieldParenthesize :: ProgramBase -> BackendType -> Bool
nativeFieldParenthesize base ty =
  case nativeRenderableKind base ty of
    NativeData {} -> True
    _ -> False

lowerNativeEntrypoint :: ProgramEnv -> BindingInfo -> Map String String -> Either BackendLLVMError LLVMFunction
lowerNativeEntrypoint env mainBinding renderMap =
  lowerNativeFunction nativeCMainName (LLVMInt 32) [] $ \_ -> do
    let mainForm = biForm mainBinding
    mainLLVMType <- lowerBackendTypeM env "native process main result" (ffReturnType mainForm)
    mainValue <- emitAssign "native.main" mainLLVMType (LLVMCall (biName mainBinding) [])
    callNativeRenderer renderMap (ffReturnType mainForm) mainLLVMType mainValue False
    _ <- emitPrintStringGlobal nativeStrNewlineName
    finishNativeSuccess

nativeAndFunction :: LLVMFunction
nativeAndFunction =
  case
    lowerNativeFunction runtimeAndName (LLVMInt 1) [(LLVMInt 1, "left"), (LLVMInt 1, "right")] $ \params -> do
      result <- emitAssign "and" (LLVMInt 1) (LLVMAnd (requireNativeParam "left" params) (requireNativeParam "right" params))
      finishCurrentBlock (LLVMRet (LLVMInt 1) result)
  of
    Right function -> function
    Left err -> error ("internal native __mlfp_and lowering failed: " ++ renderBackendLLVMError err)

lowerNativeFunction ::
  String ->
  LLVMType ->
  [(LLVMType, String)] ->
  (Map String LLVMOperand -> LowerM ()) ->
  Either BackendLLVMError LLVMFunction
lowerNativeFunction name returnTy params buildBody = do
  blocks <- evalStateT (buildBody paramOperands >> gets (reverse . fsCompletedBlocks)) initialFunctionState
  pure
    LLVMFunction
      { llvmFunctionName = name,
        llvmFunctionPrivate = name /= nativeCMainName && name /= runtimeAndName,
        llvmFunctionReturnType = returnTy,
        llvmFunctionParameters = [LLVMParameter ty paramName | (ty, paramName) <- params],
        llvmFunctionBlocks = blocks
      }
  where
    paramOperands =
      Map.fromList [(paramName, LLVMLocal ty paramName) | (ty, paramName) <- params]

finishNativeSuccess :: LowerM ()
finishNativeSuccess =
  finishCurrentBlock (LLVMRet (LLVMInt 32) (LLVMIntLiteral 32 0))

requireNativeParam :: String -> Map String LLVMOperand -> LLVMOperand
requireNativeParam name params =
  case Map.lookup name params of
    Just operand -> operand
    Nothing -> error ("internal native parameter missing: " ++ name)

emitPrintf :: String -> [(LLVMType, LLVMOperand)] -> LowerM LLVMOperand
emitPrintf formatGlobal args =
  emitAssign
    "printf"
    (LLVMInt 32)
    (LLVMCall nativePrintfName ((LLVMPtr, LLVMGlobalRef LLVMPtr formatGlobal) : args))

emitPrintStringGlobal :: String -> LowerM LLVMOperand
emitPrintStringGlobal globalName =
  emitPrintf nativeFmtStringName [(LLVMPtr, LLVMGlobalRef LLVMPtr globalName)]

callNativeRenderer :: Map String String -> BackendType -> LLVMType -> LLVMOperand -> Bool -> LowerM ()
callNativeRenderer renderMap ty llvmTy value parenthesize =
  case Map.lookup (backendTypeKey ty) renderMap of
    Just renderName -> do
      _ <-
        emitAssign
          "render"
          (LLVMInt 32)
          ( LLVMCall
              renderName
              [ (llvmTy, value),
                (LLVMInt 1, LLVMIntLiteral 1 (if parenthesize then 1 else 0))
              ]
          )
      pure ()
    Nothing ->
      liftEither (BackendLLVMUnsupportedExpression "native result rendering" ("missing renderer for " ++ show ty))

displayConstructorName :: DataRuntime -> ConstructorRuntime -> String
displayConstructorName dataRuntime0 constructorRuntime =
  case runtimeModulePrefix (backendDataName (drData dataRuntime0)) >>= (`stripPrefix` runtimeName) of
    Just displayName
      | not (null displayName) -> displayName
    _ -> runtimeName
  where
    runtimeName = backendConstructorName (crConstructor constructorRuntime)

runtimeModulePrefix :: String -> Maybe String
runtimeModulePrefix qualifiedDataName0 =
  case break (== '.') (reverse qualifiedDataName0) of
    (_, []) -> Nothing
    (_, _ : reversedModuleName) -> Just (reverse reversedModuleName ++ "__")

shouldLowerReachableBinding :: Set String -> BindingInfo -> Bool
shouldLowerReachableBinding referencedFunctionNames binding =
  null (ffTypeBinders form)
    && ( biExportedAsMain binding
           || canEmitDirectReachableFunction (biName binding) form
           || (Set.member (biName binding) referencedFunctionNames && canEmitReferencedFunctionForm form)
       )
  where
    form = biForm binding

canEmitDirectReachableFunction :: String -> FunctionForm -> Bool
canEmitDirectReachableFunction name form
  | not (requiresInlineCall form) =
      canEmitFunctionForm form
  | otherwise =
      canEmitReferencedFunctionForm form && functionFormCallsGlobal name form

functionFormCallsGlobal :: String -> FunctionForm -> Bool
functionFormCallsGlobal name form =
  go (Set.fromList (map fst (ffParams form))) (ffBody form)
  where
    go bound expr =
      case collectCall expr of
        Just (BackendVar _ calleeName, _, _)
          | calleeName == name && Set.notMember calleeName bound -> True
        _ -> childCalls bound expr

    childCalls bound =
      \case
        BackendVar {} -> False
        BackendLit {} -> False
        BackendLam _ paramName _ body ->
          go (Set.insert paramName bound) body
        BackendApp _ fun arg ->
          go bound fun || go bound arg
        BackendLet _ localName _ rhs body ->
          go bound rhs || go (Set.insert localName bound) body
        BackendTyAbs _ _ _ body ->
          go bound body
        BackendTyApp _ fun _ ->
          go bound fun
        BackendConstruct _ _ args ->
          any (go bound) args
        BackendCase _ scrutinee alternatives ->
          go bound scrutinee || any (alternativeCalls bound) (NE.toList alternatives)
        BackendRoll _ payload ->
          go bound payload
        BackendUnroll _ payload ->
          go bound payload
        BackendClosure _ _ captures params body ->
          any (go bound . backendClosureCaptureExpr) captures
            || go (Set.unions [bound, Set.fromList (map backendClosureCaptureName captures), Set.fromList (map fst params)]) body
        BackendClosureCall _ fun args ->
          go bound fun || any (go bound) args

    alternativeCalls bound (BackendAlternative pattern0 body) =
      go (Set.union bound (patternBinders pattern0)) body

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

shouldLowerSpecialization :: Set String -> Specialization -> Bool
shouldLowerSpecialization referencedFunctionNames specialization =
  (not (requiresInlineCall form) && canEmitFunctionForm form)
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
  [ LLVMDeclaration runtimeMallocName LLVMPtr [LLVMInt 64] False
    | Map.notMember runtimeMallocName (pbBindings base)
  ]
    ++ [ LLVMDeclaration runtimeAndName (LLVMInt 1) [LLVMInt 1, LLVMInt 1] False
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
      dataRuntimes =
        map dataRuntime dataDecls
  pure
        ProgramBase
          { pbBindings = Map.fromList [(biName info, info) | info <- bindingInfos],
            pbBindingOrder = map biName bindingInfos,
            pbConstructors = Map.fromList [(backendConstructorName (crConstructor ctor), ctor) | ctor <- constructors],
            pbData = Map.fromList [(backendDataName (drData dataRuntime0), dataRuntime0) | dataRuntime0 <- dataRuntimes],
            pbDataNames = Set.fromList (map backendDataName dataDecls)
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

dataRuntime :: BackendData -> DataRuntime
dataRuntime dataDecl =
  DataRuntime
    { drData = dataDecl,
      drConstructors = constructorRuntimes dataDecl
    }

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

substituteFunctionFormTypes :: Map String BackendType -> FunctionForm -> FunctionForm
substituteFunctionFormTypes substitution0 form =
  FunctionForm
    { ffTypeBinders = [(name, fmap substituteTy mbBound) | (name, mbBound) <- ffTypeBinders form],
      ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
      ffBody = substituteExprTypes substitution (ffBody form),
      ffReturnType = substituteTy (ffReturnType form)
    }
  where
    binderNames = Set.fromList (map fst (ffTypeBinders form))
    substitution = Map.withoutKeys substitution0 binderNames
    substituteTy = substituteBackendTypes substitution

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

functionFormType :: FunctionForm -> BackendType
functionFormType form =
  foldr
    (\(name, mbBound) body -> BTForall name mbBound body)
    (foldr (\(_, paramTy) body -> BTArrow paramTy body) (ffReturnType form) (ffParams form))
    (ffTypeBinders form)

backendTypeHasRuntimeRepresentation :: ProgramEnv -> BackendType -> Bool
backendTypeHasRuntimeRepresentation env ty =
  isClosureRuntimeValueType ty
    || case lowerBackendType env "runtime representation check" ty of
      Right _ -> True
      Left _ -> False

backendTypeRequiresStaticSpecialization :: BackendType -> Bool
backendTypeRequiresStaticSpecialization =
  \case
    BTVar {} -> False
    BTArrow {} -> False
    BTBase {} -> False
    BTCon {} -> False
    BTVarApp {} -> True
    BTForall {} -> True
    BTMu {} -> False
    BTBottom -> False

emptyExprEnv :: ExprEnv
emptyExprEnv =
  ExprEnv
    { eeValues = Map.empty,
      eeLocalFunctions = Map.empty,
      eeActiveGlobalInlines = Set.empty
    }

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
        BackendClosure resultTy entryName captures params body ->
          BackendClosure
            resultTy
            entryName
            (map (renameCapture renaming) captures)
            params
            (go (withoutNames (map backendClosureCaptureName captures ++ map fst params) renaming) body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall resultTy (go renaming fun) (map (go renaming) args)

    renameAlternative renaming (BackendAlternative pattern0 body) =
      BackendAlternative pattern0 (go (withoutPatternBinders pattern0 renaming) body)

    renameCapture renaming capture =
      capture {backendClosureCaptureExpr = go renaming (backendClosureCaptureExpr capture)}

    withoutNames names renaming =
      foldr Map.delete renaming names

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
        BackendClosure _ _ captures params body ->
          Set.unions (map (go bound . backendClosureCaptureExpr) captures)
            `Set.union` go (Set.unions [bound, Set.fromList (map backendClosureCaptureName captures), Set.fromList (map fst params)]) body
        BackendClosureCall _ fun args ->
          go bound fun `Set.union` Set.unions (map (go bound) args)

    freeAlternative bound (BackendAlternative pattern0 body) =
      go (Set.union (patternBinders pattern0) bound) body

    patternBinders =
      \case
        BackendDefaultPattern ->
          Set.empty
        BackendConstructorPattern _ binders ->
          Set.fromList binders

backendExprVarTypesFor :: Set String -> BackendExpr -> Map String BackendType
backendExprVarTypesFor targets =
  go Set.empty
  where
    go shadowed =
      \case
        BackendVar ty name
          | Set.member name targets && Set.notMember name shadowed -> Map.singleton name ty
          | otherwise -> Map.empty
        BackendLit {} ->
          Map.empty
        BackendLam _ name _ body ->
          go (Set.insert name shadowed) body
        BackendApp _ fun arg ->
          go shadowed fun `Map.union` go shadowed arg
        BackendLet _ name _ rhs body ->
          go shadowed rhs `Map.union` go (Set.insert name shadowed) body
        BackendTyAbs _ _ _ body ->
          go shadowed body
        BackendTyApp _ fun _ ->
          go shadowed fun
        BackendConstruct _ _ args ->
          Map.unions (map (go shadowed) args)
        BackendCase _ scrutinee alternatives ->
          go shadowed scrutinee `Map.union` Map.unions (map (goAlternative shadowed) (NE.toList alternatives))
        BackendRoll _ payload ->
          go shadowed payload
        BackendUnroll _ payload ->
          go shadowed payload
        BackendClosure _ _ captures params body ->
          Map.unions (map (go shadowed . backendClosureCaptureExpr) captures)
            `Map.union` go (Set.unions [shadowed, Set.fromList (map backendClosureCaptureName captures), Set.fromList (map fst params)]) body
        BackendClosureCall _ fun args ->
          go shadowed fun `Map.union` Map.unions (map (go shadowed) args)

    goAlternative shadowed (BackendAlternative pattern0 body) =
      go (Set.union shadowed (patternBinders pattern0)) body

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
        ++ concatMap (collectEvidenceWrappersInForm base Map.empty Set.empty . qualifiedSpecializationForm) specializations
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

collectFunctionWrappers :: ProgramBase -> [BindingInfo] -> [Specialization] -> [FunctionWrapper]
collectFunctionWrappers base reachable specializations =
  zipWith assignName [(0 :: Int) ..] uniqueRequests
  where
    requests =
      concatMap (collectFunctionWrappersInForm base Map.empty Set.empty . biForm) monomorphicReachable
        ++ concatMap (collectFunctionWrappersInForm base Map.empty Set.empty . qualifiedSpecializationForm) specializations
    monomorphicReachable =
      filter (null . ffTypeBinders . biForm) reachable
    uniqueRequests =
      map snd (Map.toAscList (Map.fromList [(functionWrapperKey expected expr, (expected, expr)) | (expected, expr) <- requests]))
    assignName index0 (expected, expr) =
      FunctionWrapper
        { fwKey = functionWrapperKey expected expr,
          fwFunctionName = "__mlfp_function_wrapper$" ++ show index0,
          fwExpectedType = expected,
          fwExpr = expr
        }

collectReferencedFunctionNames :: ProgramBase -> [BindingInfo] -> [Specialization] -> [EvidenceWrapper] -> [FunctionWrapper] -> Set String
collectReferencedFunctionNames base reachable specializations evidenceWrappers functionWrappers =
  Set.unions
    ( map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . biForm) reachable
        ++ map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . spForm) specializations
        ++ map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . evidenceWrapperForm) evidenceWrappers
        ++ map (collectReferencedFunctionNamesInForm base Map.empty Set.empty . functionWrapperForm) functionWrappers
    )

type LocalFunctionForms = Map String FunctionForm

data LocalStoredFunction = LocalStoredFunction
  { lsfForm :: FunctionForm,
    lsfSourceExpr :: BackendExpr
  }

type LocalStoredFunctions = Map String LocalStoredFunction

shadowLocalFunctionForms :: Set String -> LocalFunctionForms -> LocalFunctionForms
shadowLocalFunctionForms names forms =
  Map.withoutKeys forms names

collectReferencedFunctionNamesInForm :: ProgramBase -> Map String BackendType -> Set String -> FunctionForm -> Set String
collectReferencedFunctionNamesInForm base substitution bound form =
  collectReferencedFunctionNamesInFormWithLocals base substitution Map.empty bound form

collectReferencedFunctionNamesInFormWithLocals :: ProgramBase -> Map String BackendType -> LocalFunctionForms -> Set String -> FunctionForm -> Set String
collectReferencedFunctionNamesInFormWithLocals base substitution localForms bound form =
  let paramNames = Set.fromList (map fst (ffParams form))
   in collectReferencedFunctionNamesInExpr
        base
        substitution
        (shadowLocalFunctionForms paramNames localForms)
        (Set.union paramNames bound)
        (ffBody form)

collectReferencedFunctionNamesInExpr :: ProgramBase -> Map String BackendType -> LocalFunctionForms -> Set String -> BackendExpr -> Set String
collectReferencedFunctionNamesInExpr base substitution localForms bound expr =
  referencedHere `Set.union` childReferences
  where
    referencedHere =
      case collectCall expr of
        Just (callee, typeArgs, args) ->
          let typeArgs' = map (substituteBackendTypes substitution) typeArgs
              args' = map (substituteExprTypes substitution) args
           in case instantiateFunctionFormWithTypeArgs "referenced function argument" (callableForm callee) typeArgs' args' of
                Right (_, form) ->
                  localCallReferences callee typeArgs' args'
                    `Set.union` Set.fromList
                      [ functionName
                      | ((_, paramTy), arg) <- zip (ffParams form) args',
                        isFunctionLikeBackendType paramTy,
                        Just functionName <- [referencedFunctionArgumentName arg]
                      ]
                Left _ -> Set.empty
        Nothing ->
          localTypeApplicationReferences

    callableForm callee =
      case callee of
        BackendVar calleeTy name ->
          case Map.lookup name localForms of
            Just form -> form
            Nothing -> functionFormFromType calleeTy
        _ -> functionFormFromExpr callee

    referencedFunctionArgumentName arg =
      case collectTyApps arg of
        (BackendVar _ name, typeArgs) ->
          referencedFunctionNameByName Set.empty name typeArgs
        _ -> Nothing

    referencedFunctionNameByName seen name typeArgs
      | Set.member name seen = Nothing
      | Just form <- Map.lookup name localForms =
          case instantiateFunctionFormWithTypeArgs ("referenced local function argument " ++ name) form typeArgs [] of
            Right (_, instantiated) ->
              case etaAliasTarget instantiated of
                Just (targetName, targetTypeArgs) ->
                  referencedFunctionNameByName (Set.insert name seen) targetName targetTypeArgs
                Nothing -> Nothing
            Left _ -> Nothing
      | Set.notMember name bound,
        Just binding <- Map.lookup name (pbBindings base) =
          case instantiateFunctionFormWithTypeArgs ("referenced function argument " ++ name) (biForm binding) typeArgs [] of
            Right (resolvedTypeArgs, _)
              | null (ffTypeBinders (biForm binding)) -> Just (biName binding)
              | otherwise -> Just (specializedFunctionName (SpecRequest name resolvedTypeArgs))
            Left _ -> Nothing
      | otherwise = Nothing

    localCallReferences callee typeArgs args =
      case callee of
        BackendVar _ name
          | Just form <- Map.lookup name localForms ->
              collectInstantiatedLocalReferences name form typeArgs args
        _ -> Set.empty

    localTypeApplicationReferences =
      case collectTyApps expr of
        (BackendVar _ name, typeArgs)
          | Just form <- Map.lookup name localForms ->
              collectInstantiatedLocalReferences name form (map (substituteBackendTypes substitution) typeArgs) []
        _ -> Set.empty

    collectInstantiatedLocalReferences name form typeArgs args =
      case instantiateFunctionFormWithTypeArgs ("referenced local function " ++ name) form typeArgs args of
        Right (_, instantiated) ->
          collectReferencedFunctionNamesInFormWithLocals base Map.empty localForms bound instantiated
        Left _ -> Set.empty

    childReferences =
      case expr of
        BackendVar {} -> Set.empty
        BackendLit {} -> Set.empty
        BackendLam _ name _ body ->
          collectReferencedFunctionNamesInExpr base substitution (Map.delete name localForms) (Set.insert name bound) body
        BackendApp _ fun arg ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound fun
            `Set.union` collectReferencedFunctionNamesInExpr base substitution localForms bound arg
        BackendLet _ name bindingTy rhs body ->
          collectLetRhsReferences bindingTy rhs
            `Set.union` collectReferencedFunctionNamesInExpr base substitution (collectLetLocalForms name bindingTy rhs) (Set.insert name bound) body
        BackendTyAbs _ name _ body ->
          collectReferencedFunctionNamesInExpr base (Map.delete name substitution) localForms bound body
        BackendTyApp _ fun _ ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound fun
        BackendConstruct _ _ args ->
          Set.unions (map (collectReferencedFunctionNamesInExpr base substitution localForms bound) args)
        BackendCase _ scrutinee alternatives ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound scrutinee
            `Set.union` Set.unions (map collectAlternativeReferences (NE.toList alternatives))
        BackendRoll _ payload ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound payload
        BackendUnroll _ payload ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound payload
        BackendClosure _ _ captures params body ->
          Set.unions (map (collectReferencedFunctionNamesInExpr base substitution localForms bound . backendClosureCaptureExpr) captures)
            `Set.union` collectReferencedFunctionNamesInExpr
              base
              substitution
              (shadowLocalFunctionForms closureBound localForms)
              (Set.union closureBound bound)
              body
          where
            closureBound = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
        BackendClosureCall _ fun args ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound fun
            `Set.union` Set.unions (map (collectReferencedFunctionNamesInExpr base substitution localForms bound) args)

    collectLetRhsReferences bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form0
          | not (null (ffTypeBinders form0)) || not (null (ffParams form0)) ->
              let form = substituteFunctionFormTypes substitution form0
               in if null (ffTypeBinders form)
                    then collectReferencedFunctionNamesInFormWithLocals base Map.empty localForms bound form
                    else Set.empty
        _ ->
          collectReferencedFunctionNamesInExpr base substitution localForms bound rhs

    collectLetLocalForms name bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              Map.insert name (substituteFunctionFormTypes substitution form) localForms
        _ ->
          Map.delete name localForms

    collectAlternativeReferences alternative =
      let binders = patternBinders (backendAltPattern alternative)
       in
      collectReferencedFunctionNamesInExpr
        base
        substitution
        (shadowLocalFunctionForms binders localForms)
        (Set.union binders bound)
        (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

collectEvidenceWrappersInForm :: ProgramBase -> Map String BackendType -> Set String -> FunctionForm -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInForm base substitution bound form =
  collectEvidenceWrappersInFormWithLocals base substitution Map.empty bound form

collectEvidenceWrappersInFormWithLocals :: ProgramBase -> Map String BackendType -> LocalFunctionForms -> Set String -> FunctionForm -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInFormWithLocals base substitution localForms bound form =
  let paramNames = Set.fromList (map fst (ffParams form))
   in collectEvidenceWrappersInExpr
        base
        substitution
        (shadowLocalFunctionForms paramNames localForms)
        (Set.union paramNames bound)
        (ffBody form)

collectEvidenceWrappersInExpr :: ProgramBase -> Map String BackendType -> LocalFunctionForms -> Set String -> BackendExpr -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInExpr base substitution localForms bound expr =
  wrappersHere ++ childWrappers
  where
    wrappersHere =
      case collectCall expr of
        Just (BackendVar _ name, typeArgs, args)
          | Just form <- Map.lookup name localForms ->
              let typeArgs' = map (substituteBackendTypes substitution) typeArgs
                  args' = map (substituteExprTypes substitution) args
               in collectInstantiatedLocalWrappers name form typeArgs' args'
          | Set.notMember name bound,
            Just binding <- Map.lookup name (pbBindings base) ->
              let typeArgs' = map (substituteBackendTypes substitution) typeArgs
                  args' = map (substituteExprTypes substitution) args
               in case instantiateFunctionFormWithTypeArgs ("evidence wrapper request " ++ name) (biForm binding) typeArgs' args' of
                    Right (_, form) -> evidenceArgumentWrappers form args'
                    Left _ -> []
        _ -> localTypeApplicationWrappers

    childWrappers =
      case expr of
        BackendVar {} -> []
        BackendLit {} -> []
        BackendLam _ name _ body ->
          collectEvidenceWrappersInExpr base substitution (Map.delete name localForms) (Set.insert name bound) body
        BackendApp _ fun arg ->
          collectEvidenceWrappersInExpr base substitution localForms bound fun
            ++ collectEvidenceWrappersInExpr base substitution localForms bound arg
        BackendLet _ name bindingTy rhs body ->
          collectLetRhsWrappers bindingTy rhs
            ++ collectEvidenceWrappersInExpr base substitution (collectLetLocalForms name bindingTy rhs) (Set.insert name bound) body
        BackendTyAbs _ name _ body ->
          collectEvidenceWrappersInExpr base (Map.delete name substitution) localForms bound body
        BackendTyApp _ fun _ ->
          collectEvidenceWrappersInExpr base substitution localForms bound fun
        BackendConstruct _ _ args ->
          concatMap (collectEvidenceWrappersInExpr base substitution localForms bound) args
        BackendCase _ scrutinee alternatives ->
          collectEvidenceWrappersInExpr base substitution localForms bound scrutinee
            ++ concatMap collectAlternativeWrappers (NE.toList alternatives)
        BackendRoll _ payload ->
          collectEvidenceWrappersInExpr base substitution localForms bound payload
        BackendUnroll _ payload ->
          collectEvidenceWrappersInExpr base substitution localForms bound payload
        BackendClosure _ _ captures params body ->
          concatMap (collectEvidenceWrappersInExpr base substitution localForms bound . backendClosureCaptureExpr) captures
            ++ collectEvidenceWrappersInExpr
              base
              substitution
              (shadowLocalFunctionForms closureBound localForms)
              (Set.union closureBound bound)
              body
          where
            closureBound = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
        BackendClosureCall _ fun args ->
          collectEvidenceWrappersInExpr base substitution localForms bound fun
            ++ concatMap (collectEvidenceWrappersInExpr base substitution localForms bound) args

    evidenceArgumentWrappers form args =
      [ (paramTy, arg)
      | ((paramName, paramTy), arg) <- zip (ffParams form) args,
        isEvidenceArgument False paramName paramTy,
        not (isSimpleFunctionReference arg),
        evidenceWrapperArgumentClosed bound arg
      ]

    localTypeApplicationWrappers =
      case collectTyApps expr of
        (BackendVar _ name, typeArgs)
          | Just form <- Map.lookup name localForms ->
              collectInstantiatedLocalWrappers name form (map (substituteBackendTypes substitution) typeArgs) []
        _ -> []

    collectInstantiatedLocalWrappers name form typeArgs args =
      case instantiateFunctionFormWithTypeArgs ("evidence wrapper request " ++ name) form typeArgs args of
        Right (_, instantiated) ->
          evidenceArgumentWrappers instantiated args
            ++ collectEvidenceWrappersInFormWithLocals base Map.empty localForms bound instantiated
        Left _ -> []

    collectLetRhsWrappers bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form0
          | not (null (ffTypeBinders form0)) || not (null (ffParams form0)) ->
              let form = substituteFunctionFormTypes substitution form0
               in if null (ffTypeBinders form)
                    then collectEvidenceWrappersInFormWithLocals base Map.empty localForms bound form
                    else []
        _ ->
          collectEvidenceWrappersInExpr base substitution localForms bound rhs

    collectLetLocalForms name bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              Map.insert name (substituteFunctionFormTypes substitution form) localForms
        _ ->
          Map.delete name localForms

    collectAlternativeWrappers alternative =
      let binders = patternBinders (backendAltPattern alternative)
       in
      collectEvidenceWrappersInExpr
        base
        substitution
        (shadowLocalFunctionForms binders localForms)
        (Set.union binders bound)
        (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

evidenceWrapperKey :: BackendType -> BackendExpr -> String
evidenceWrapperKey expected expr =
  backendTypeKey expected ++ "\0" ++ show expr

collectFunctionWrappersInForm :: ProgramBase -> Map String BackendType -> Set String -> FunctionForm -> [(BackendType, BackendExpr)]
collectFunctionWrappersInForm base substitution bound form =
  collectFunctionWrappersInFormWithLocals base substitution Map.empty bound form

collectFunctionWrappersInFormWithLocals :: ProgramBase -> Map String BackendType -> LocalStoredFunctions -> Set String -> FunctionForm -> [(BackendType, BackendExpr)]
collectFunctionWrappersInFormWithLocals base substitution localFunctions bound form =
  let paramNames = Set.fromList (map fst (ffParams form))
   in collectFunctionWrappersInExpr
        base
        substitution
        (Map.withoutKeys localFunctions paramNames)
        (Set.union paramNames bound)
        (ffBody form)

collectFunctionWrappersInExpr :: ProgramBase -> Map String BackendType -> LocalStoredFunctions -> Set String -> BackendExpr -> [(BackendType, BackendExpr)]
collectFunctionWrappersInExpr base substitution localFunctions bound expr =
  wrapperRequests ++ childRequests
  where
    wrapperRequests =
      case expr of
        BackendConstruct resultTy name args ->
          case Map.lookup name (pbConstructors base) >>= \constructorRuntime -> constructorRuntimeFieldTypes constructorRuntime resultTy of
            Just fieldTys ->
              [ request
              | (fieldTy, arg) <- zip fieldTys args,
                let fieldTy' = substituteBackendTypes substitution fieldTy,
                let arg' = substituteExprTypes substitution arg,
                isFirstOrderFunctionPointerType fieldTy',
                not (isClosureRuntimeValueType fieldTy'),
                Just request <- [functionWrapperRequest fieldTy' arg']
              ]
            Nothing -> []
        _ -> []

    functionWrapperRequest fieldTy arg =
      case localStoredFunctionWrapperSource arg of
        Just sourceExpr
          | evidenceWrapperArgumentClosed bound sourceExpr ->
              Just (fieldTy, sourceExpr)
        _ | not (isSimpleFunctionReference arg),
            evidenceWrapperArgumentClosed bound arg ->
              Just (fieldTy, arg)
        _ ->
          Nothing

    localStoredFunctionWrapperSource arg =
      case collectTyApps arg of
        (BackendVar _ name, typeArgs) ->
          localStoredFunctionSourceByName Set.empty name typeArgs
        _ -> Nothing

    localStoredFunctionSourceByName seen name typeArgs
      | Set.member name seen = Nothing
      | Just localFunction <- Map.lookup name localFunctions =
          case instantiateFunctionFormWithTypeArgs ("function wrapper request " ++ name) (lsfForm localFunction) typeArgs [] of
            Right (_, instantiated) ->
              case etaAliasTarget instantiated of
                Just (targetName, targetTypeArgs) ->
                  localStoredFunctionSourceByName (Set.insert name seen) targetName targetTypeArgs
                Nothing ->
                  applyStoredSourceTypeArgs name (lsfSourceExpr localFunction) typeArgs
            Left _ ->
              Nothing
      | otherwise =
          Nothing

    applyStoredSourceTypeArgs _ sourceExpr [] =
      Just sourceExpr
    applyStoredSourceTypeArgs name sourceExpr typeArgs =
      case applyTypeApplicationsToExprWithType ("function wrapper request " ++ name) sourceExpr typeArgs of
        Right (applied, _) -> Just applied
        Left _ -> Nothing

    childRequests =
      case expr of
        BackendVar {} ->
          []
        BackendLit {} ->
          []
        BackendLam _ name _ body ->
          collectFunctionWrappersInExpr base substitution (Map.delete name localFunctions) (Set.insert name bound) body
        BackendApp _ fun arg ->
          collectFunctionWrappersInExpr base substitution localFunctions bound fun
            ++ collectFunctionWrappersInExpr base substitution localFunctions bound arg
        BackendLet _ name _ rhs body ->
          collectFunctionWrappersInExpr base substitution localFunctions bound rhs
            ++ collectFunctionWrappersInExpr base substitution (collectLetLocalFunction name rhs) (Set.insert name bound) body
        BackendTyAbs _ name _ body ->
          collectFunctionWrappersInExpr base (Map.delete name substitution) localFunctions bound body
        BackendTyApp _ fun _ ->
          collectFunctionWrappersInExpr base substitution localFunctions bound fun
        BackendConstruct _ _ args ->
          concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound) args
        BackendCase _ scrutinee alternatives ->
          collectFunctionWrappersInExpr base substitution localFunctions bound scrutinee
            ++ concatMap collectAlternativeWrappers (NE.toList alternatives)
        BackendRoll _ payload ->
          collectFunctionWrappersInExpr base substitution localFunctions bound payload
        BackendUnroll _ payload ->
          collectFunctionWrappersInExpr base substitution localFunctions bound payload
        BackendClosure _ _ captures params body ->
          concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound . backendClosureCaptureExpr) captures
            ++ collectFunctionWrappersInExpr
              base
              substitution
              (Map.withoutKeys localFunctions closureBound)
              (Set.union closureBound bound)
              body
          where
            closureBound = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
        BackendClosureCall _ fun args ->
          collectFunctionWrappersInExpr base substitution localFunctions bound fun
            ++ concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound) args

    collectLetLocalFunction name rhs =
      case functionFormFromExpected (backendExprType rhs) rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              Map.insert
                name
                LocalStoredFunction
                  { lsfForm = substituteFunctionFormTypes substitution form,
                    lsfSourceExpr = substituteExprTypes substitution rhs
                  }
                localFunctions
        _ ->
          Map.delete name localFunctions

    collectAlternativeWrappers alternative =
      let binders = patternBinders (backendAltPattern alternative)
       in collectFunctionWrappersInExpr
            base
            substitution
            (Map.withoutKeys localFunctions binders)
            (Set.union binders bound)
            (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

functionWrapperKey :: BackendType -> BackendExpr -> String
functionWrapperKey expected expr =
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
        BackendClosure _ _ captures params body ->
          foldMap (go bound . backendClosureCaptureExpr) captures
            `Set.union` go (Set.unions [bound, Set.fromList (map backendClosureCaptureName captures), Set.fromList (map fst params)]) body
        BackendClosureCall _ fun args ->
          Set.union (go bound fun) (foldMap (go bound) args)

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
        BackendClosure _ _ captures params body ->
          concatMap (collectSpecializationRequestsWithBound base substitution bound . backendClosureCaptureExpr) captures
            ++ collectSpecializationRequestsWithBound
              base
              substitution
              (Set.union (Set.fromList (map backendClosureCaptureName captures ++ map fst params)) bound)
              body
        BackendClosureCall _ fun args ->
          collectSpecializationRequestsWithBound base substitution bound fun
            ++ concatMap (collectSpecializationRequestsWithBound base substitution bound) args

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
    BackendClosure _ _ captures params body ->
      Set.unions (map (freeGlobalRefs base bound . backendClosureCaptureExpr) captures)
        `Set.union` freeGlobalRefs base (Set.unions [bound, Set.fromList (map backendClosureCaptureName captures), Set.fromList (map fst params)]) body
    BackendClosureCall _ fun args ->
      freeGlobalRefs base bound fun `Set.union` Set.unions (map (freeGlobalRefs base bound) args)
  where
    freeAlternativeRefs bound0 alternative =
      freeGlobalRefs base (Set.union (patternBinders (backendAltPattern alternative)) bound0) (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

collectProgramStrings :: [BindingInfo] -> [Specialization] -> [EvidenceWrapper] -> [FunctionWrapper] -> [String]
collectProgramStrings reachable specializations evidenceWrappers functionWrappers =
  sort $
    nub $
      concatMap (collectStringLiterals . ffBody . biForm) (filter (null . ffTypeBinders . biForm) reachable)
        ++ concatMap (collectStringLiterals . ffBody . spForm) specializations
        ++ concatMap (collectStringLiterals . ffBody . evidenceWrapperForm) evidenceWrappers
        ++ concatMap (collectStringLiterals . ffBody . functionWrapperForm) functionWrappers

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
    BackendClosure _ _ captures _ body ->
      concatMap (collectStringLiterals . backendClosureCaptureExpr) captures ++ collectStringLiterals body
    BackendClosureCall _ fun args ->
      collectStringLiterals fun ++ concatMap collectStringLiterals args

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
  lowerFunction env (spFunctionName specialization) True (qualifiedSpecializationForm specialization)

lowerEvidenceWrapper :: ProgramEnv -> EvidenceWrapper -> Either BackendLLVMError LLVMFunction
lowerEvidenceWrapper env wrapper =
  lowerFunction env (ewFunctionName wrapper) True (qualifiedEvidenceWrapperForm wrapper)

lowerFunctionWrapper :: ProgramEnv -> FunctionWrapper -> Either BackendLLVMError LLVMFunction
lowerFunctionWrapper env wrapper =
  lowerFunction env (fwFunctionName wrapper) True (qualifiedFunctionWrapperForm wrapper)

collectClosureEntries :: [BindingInfo] -> [Specialization] -> [EvidenceWrapper] -> [FunctionWrapper] -> [ClosureEntry]
collectClosureEntries reachable specializations evidenceWrappers functionWrappers =
  concatMap (collectClosureEntriesInForm . biForm) (filter (null . ffTypeBinders . biForm) reachable)
    ++ concatMap (collectClosureEntriesInForm . qualifiedSpecializationForm) specializations
    ++ concatMap (collectClosureEntriesInForm . qualifiedEvidenceWrapperForm) evidenceWrappers
    ++ concatMap (collectClosureEntriesInForm . qualifiedFunctionWrapperForm) functionWrappers

requireUniqueClosureEntries :: [ClosureEntry] -> Either BackendLLVMError [ClosureEntry]
requireUniqueClosureEntries entries =
  reverse <$> foldM includeEntry [] entries
  where
    includeEntry kept entry =
      case filter ((== ceEntryName entry) . ceEntryName) kept of
        [] ->
          Right (entry : kept)
        existing : _
          | existing == entry ->
              Right kept
          | otherwise ->
              Left (BackendLLVMInternalError ("duplicate closure entry after specialization: " ++ ceEntryName entry))

qualifiedSpecializationForm :: Specialization -> FunctionForm
qualifiedSpecializationForm specialization =
  qualifyClosureEntriesInForm (spFunctionName specialization) (spForm specialization)

qualifiedEvidenceWrapperForm :: EvidenceWrapper -> FunctionForm
qualifiedEvidenceWrapperForm wrapper =
  qualifyClosureEntriesInForm (ewFunctionName wrapper) (evidenceWrapperForm wrapper)

qualifiedFunctionWrapperForm :: FunctionWrapper -> FunctionForm
qualifiedFunctionWrapperForm wrapper =
  qualifyClosureEntriesInForm (fwFunctionName wrapper) (functionWrapperForm wrapper)

qualifyClosureEntriesInForm :: String -> FunctionForm -> FunctionForm
qualifyClosureEntriesInForm ownerName form =
  form {ffBody = qualifyClosureEntriesInExpr ownerName (ffBody form)}

qualifyInstantiatedClosureEntries :: String -> [BackendType] -> FunctionForm -> FunctionForm
qualifyInstantiatedClosureEntries ownerName resolvedTypeArgs form
  | null resolvedTypeArgs = form
  | otherwise = qualifyClosureEntriesInForm (closureEntryOwnerName ownerName resolvedTypeArgs) form

qualifyClosureEntriesInExpr :: String -> BackendExpr -> BackendExpr
qualifyClosureEntriesInExpr ownerName =
  go
  where
    go =
      \case
        BackendVar resultTy name ->
          BackendVar resultTy name
        BackendLit resultTy lit ->
          BackendLit resultTy lit
        BackendLam resultTy name paramTy body ->
          BackendLam resultTy name paramTy (go body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go fun) (go arg)
        BackendLet resultTy name bindingTy rhs body ->
          BackendLet resultTy name bindingTy (go rhs) (go body)
        BackendTyAbs resultTy name mbBound body ->
          BackendTyAbs resultTy name mbBound (go body)
        BackendTyApp resultTy fun ty ->
          BackendTyApp resultTy (go fun) ty
        BackendConstruct resultTy name args ->
          BackendConstruct resultTy name (map go args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go scrutinee) (fmap qualifyAlternative alternatives)
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go payload)
        BackendClosure resultTy entryName captures params body ->
          BackendClosure
            resultTy
            (qualifiedClosureEntryName ownerName entryName)
            (map qualifyCapture captures)
            params
            (go body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall resultTy (go fun) (map go args)

    qualifyAlternative alternative =
      alternative {backendAltBody = go (backendAltBody alternative)}

    qualifyCapture capture =
      capture {backendClosureCaptureExpr = go (backendClosureCaptureExpr capture)}

qualifiedClosureEntryName :: String -> String -> String
qualifiedClosureEntryName ownerName entryName =
  ownerName ++ "$" ++ entryName

collectClosureEntriesInForm :: FunctionForm -> [ClosureEntry]
collectClosureEntriesInForm =
  collectClosureEntriesInFormWithLocals Map.empty

collectClosureEntriesInFormWithLocals :: LocalFunctionForms -> FunctionForm -> [ClosureEntry]
collectClosureEntriesInFormWithLocals localForms form =
  collectClosureEntriesInExpr (shadowLocalFunctionForms paramNames localForms) (ffBody form)
  where
    paramNames = Set.fromList (map fst (ffParams form))

collectClosureEntriesInExpr :: LocalFunctionForms -> BackendExpr -> [ClosureEntry]
collectClosureEntriesInExpr localForms expr =
  case expr of
    BackendVar {} -> []
    BackendLit {} -> []
    BackendLam _ name _ body ->
      collectClosureEntriesInExpr (Map.delete name localForms) body
    BackendApp _ fun arg ->
      case collectAdministrativeCallEntries of
        Just entries -> entries
        Nothing ->
          collectAppliedLocalClosureEntries
            ++ collectClosureEntriesInExpr localForms fun
            ++ collectClosureEntriesInExpr localForms arg
    BackendLet _ name bindingTy rhs body ->
      collectClosureEntriesInExpr localForms rhs
        ++ collectClosureEntriesInExpr (collectLetLocalForm name bindingTy rhs) body
    BackendTyAbs {} -> []
    BackendTyApp {} ->
      case collectAdministrativeTypeAppEntries of
        Just entries -> entries
        Nothing -> collectTypeAppliedClosureEntries
    BackendConstruct _ _ args -> concatMap (collectClosureEntriesInExpr localForms) args
    BackendCase _ scrutinee alternatives ->
      collectClosureEntriesInExpr localForms scrutinee ++ concatMap collectAlternativeEntries (NE.toList alternatives)
    BackendRoll _ payload -> collectClosureEntriesInExpr localForms payload
    BackendUnroll _ payload -> collectClosureEntriesInExpr localForms payload
    BackendClosure resultTy entryName captures params body ->
      ClosureEntry
        { ceFunctionType = resultTy,
          ceEntryName = entryName,
          ceCaptures = [(backendClosureCaptureName capture, backendClosureCaptureType capture) | capture <- captures],
          ceParams = params,
          ceBody = body
        }
        : concatMap (collectClosureEntriesInExpr localForms . backendClosureCaptureExpr) captures
          ++ collectClosureEntriesInExpr (shadowLocalFunctionForms closureBound localForms) body
      where
        closureBound = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
    BackendClosureCall _ fun args ->
      collectClosureEntriesInExpr localForms fun ++ concatMap (collectClosureEntriesInExpr localForms) args
  where
    collectAppliedLocalClosureEntries =
      case collectCall expr of
        Just (BackendVar _ name, typeArgs, args)
          | Just form <- Map.lookup name localForms ->
              collectInstantiatedLocalClosureEntries name form typeArgs args
        _ -> []

    collectAdministrativeCallEntries =
      case collectCall expr of
        Just (headExpr, typeArgs, args) ->
          case pushCallIntoExpression "closure entry collection" (backendExprType expr) headExpr typeArgs args of
            Right (Just applied) -> Just (collectClosureEntriesInExpr localForms applied)
            _ -> Nothing
        Nothing -> Nothing

    collectTypeAppliedClosureEntries =
      case collectTyApps expr of
        (BackendVar _ name, typeArgs)
          | Just form <- Map.lookup name localForms ->
              collectInstantiatedLocalClosureEntries name form typeArgs []
        (fun@BackendTyAbs {}, typeArgs) ->
          collectInstantiatedClosureEntries
            "__mlfp_direct_typeapp"
            (functionFormFromExpr fun)
            typeArgs
            []
        (fun, _) ->
          collectClosureEntriesInExpr localForms fun

    collectAdministrativeTypeAppEntries =
      case collectTyApps expr of
        (headExpr, typeArgs) ->
          case pushTypeApplicationsIntoExpression "closure entry collection" (backendExprType expr) headExpr typeArgs of
            Right (Just applied) -> Just (collectClosureEntriesInExpr localForms applied)
            _ -> Nothing

    collectInstantiatedLocalClosureEntries name form typeArgs args =
      collectInstantiatedClosureEntries name form typeArgs args

    collectInstantiatedClosureEntries ownerName form typeArgs args =
      case instantiateFunctionFormWithTypeArgs ("closure entry collection " ++ ownerName) form typeArgs args of
        Right (resolvedTypeArgs, instantiated) ->
          collectClosureEntriesInFormWithLocals
            localForms
            (qualifyInstantiatedClosureEntries ownerName resolvedTypeArgs instantiated)
        Left _ ->
          []

    collectLetLocalForm name bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) ->
              Map.insert name form localForms
        _ ->
          Map.delete name localForms

    collectAlternativeEntries alternative =
      let binders = patternBinders (backendAltPattern alternative)
       in collectClosureEntriesInExpr
            (shadowLocalFunctionForms binders localForms)
            (backendAltBody alternative)

    patternBinders =
      \case
        BackendDefaultPattern -> Set.empty
        BackendConstructorPattern _ binders -> Set.fromList binders

closureEntryOwnerName :: String -> [BackendType] -> String
closureEntryOwnerName name typeArgs =
  name ++ concatMap (("$" ++) . backendTypeKey) typeArgs

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

functionWrapperForm :: FunctionWrapper -> FunctionForm
functionWrapperForm wrapper =
  FunctionForm
    { ffTypeBinders = [],
      ffParams = zip paramNames params,
      ffBody = body,
      ffReturnType = returnTy
    }
  where
    (params, returnTy) = collectArrowsType (fwExpectedType wrapper)
    paramNames = ["__mlfp_function_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]
    paramExprs = [BackendVar paramTy name | (name, paramTy) <- zip paramNames params]
    body = applyEvidenceWrapperArgs (fwExpr wrapper) (fwExpectedType wrapper) paramExprs

applyEvidenceWrapperArgs :: BackendExpr -> BackendType -> [BackendExpr] -> BackendExpr
applyEvidenceWrapperArgs expr _ [] =
  expr
applyEvidenceWrapperArgs expr ty args
  | backendExprIsExplicitClosure expr =
      BackendClosureCall
        { backendExprType = returnTy,
          backendClosureFunction = expr,
          backendClosureArguments = args
        }
  where
    (_, returnTy) = collectArrowsType ty
applyEvidenceWrapperArgs expr ty (arg : rest) =
  case ty of
    BTArrow _ resultTy ->
      applyEvidenceWrapperArgs (BackendApp resultTy expr arg) resultTy rest
    _ ->
      expr

backendExprIsExplicitClosure :: BackendExpr -> Bool
backendExprIsExplicitClosure =
  \case
    BackendClosure {} -> True
    BackendLet _ _ _ _ body -> backendExprIsExplicitClosure body
    _ -> False

lowerFunction :: ProgramEnv -> String -> Bool -> FunctionForm -> Either BackendLLVMError LLVMFunction
lowerFunction env name private form = do
  unless (null (ffTypeBinders form)) $
    Left (BackendLLVMUnsupportedExpression ("binding " ++ show name) "unspecialized polymorphic binding")
  returnTy <- lowerRuntimeValueType env ("return type of " ++ name) (ffReturnType form)
  params <- traverse lowerParam (ffParams form)
  let initialExprEnv = initialFunctionEnv name form params
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

lowerClosureEntry :: ProgramEnv -> ClosureEntry -> Either BackendLLVMError LLVMFunction
lowerClosureEntry env entry = do
  returnTy <- lowerBackendType env ("return type of closure " ++ ceEntryName entry) (closureReturnType entry)
  params <- traverse lowerParam (ceParams entry)
  let envParameter = LLVMParameter LLVMPtr "__mlfp_env"
      initialExprEnv =
        ExprEnv
          { eeValues =
              Map.fromList
                [ (paramName, LowerValue paramTy (llvmParameterType param) (LLVMLocal (llvmParameterType param) paramName))
                | ((paramName, paramTy), param) <- zip (ceParams entry) params
                ],
            eeLocalFunctions = Map.empty,
            eeActiveGlobalInlines = Set.empty
          }
  result <-
    evalStateT
      ( do
          bodyEnv <- loadClosureCaptures initialExprEnv
          bodyValue <- lowerExpr env bodyEnv ("closure " ++ show (ceEntryName entry)) (ceBody entry)
          unless (lvLLVMType bodyValue == returnTy) $
            liftEither (BackendLLVMInternalError ("closure return type mismatch in " ++ ceEntryName entry))
          finishCurrentBlock (LLVMRet returnTy (lvOperand bodyValue))
          gets (reverse . fsCompletedBlocks)
      )
      initialFunctionState
  pure
    LLVMFunction
      { llvmFunctionName = ceEntryName entry,
        llvmFunctionPrivate = True,
        llvmFunctionReturnType = returnTy,
        llvmFunctionParameters = envParameter : params,
        llvmFunctionBlocks = result
      }
  where
    closureReturnType closureEntry =
      case collectArrowsType (ceFunctionType closureEntry) of
        (_, returnTy) -> returnTy

    lowerParam (paramName, paramTy) = do
      llvmTy <- lowerArgumentType env ("closure parameter " ++ show paramName ++ " of " ++ ceEntryName entry) False paramName paramTy
      pure (LLVMParameter llvmTy paramName)

    loadClosureCaptures exprEnv0 =
      foldM loadOne exprEnv0 (zip [0 :: Int ..] (ceCaptures entry))

    loadOne exprEnv0 (index0, (captureName, captureTy)) = do
      llvmTy <- lowerClosureStoredTypeM env ("closure capture " ++ show captureName ++ " of " ++ ceEntryName entry) captureTy
      fieldPtr <- emitGep "closure.env.field.ptr" (LLVMLocal LLVMPtr "__mlfp_env") (8 * index0)
      loaded <- emitAssign "closure.env.field" llvmTy (LLVMLoad llvmTy fieldPtr)
      pure
        exprEnv0
          { eeValues =
              Map.insert
                captureName
                (LowerValue captureTy llvmTy loaded)
                (eeValues exprEnv0)
          }

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
  | isFirstOrderFunctionPointerType paramTy = Right LLVMPtr
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
    BTVarApp {} ->
      False
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
          BackendClosure _ _ captures params body ->
            any (go evidenceParams localFunctions . backendClosureCaptureExpr) captures
              || go
                evidenceParams
                (localFunctions `Set.difference` Set.fromList (map backendClosureCaptureName captures ++ map fst params))
                body
          BackendClosureCall _ fun args ->
            go evidenceParams localFunctions fun || any (go evidenceParams localFunctions) args

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

initialFunctionEnv :: String -> FunctionForm -> [LLVMParameter] -> ExprEnv
initialFunctionEnv name form params =
  ExprEnv
    { eeValues =
        Map.fromList
          [ (paramName, LowerValue paramTy (llvmParameterType param) (LLVMLocal (llvmParameterType param) paramName))
          | ((paramName, paramTy), param) <- zip (ffParams form) params
          ],
      eeLocalFunctions = Map.empty,
      eeActiveGlobalInlines = Set.singleton name
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
      expectedTy <- lowerRuntimeValueTypeM env context resultTy
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
    BackendClosure resultTy entryName captures _ _ ->
      lowerClosureValue env exprEnv context resultTy entryName captures
    BackendClosureCall resultTy fun args ->
      lowerClosureCall env exprEnv context resultTy fun args

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
      (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (functionFormFromExpr fun) typeArgs []
      let form = qualifyInstantiatedClosureEntries "__mlfp_direct_typeapp" resolvedTypeArgs form0
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
  (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (lfForm localFunction) typeArgs []
  let form = qualifyInstantiatedClosureEntries name resolvedTypeArgs form0
  lowerInstantiatedFunctionValue env (lfCapturedEnv localFunction) context name resultTy form

lowerInstantiatedFunctionValue :: ProgramEnv -> ExprEnv -> String -> String -> BackendType -> FunctionForm -> LowerM LowerValue
lowerInstantiatedFunctionValue env exprEnv context name resultTy form = do
  unless (null (ffParams form)) $
    liftEither (BackendLLVMUnsupportedExpression context ("escaping function " ++ show name))
  unless (alphaEqBackendType resultTy (ffReturnType form)) $
    liftEither (BackendLLVMInternalError ("value type mismatch for " ++ name ++ " at " ++ context))
  value <- lowerExpr env exprEnv context (ffBody form)
  expectedTy <- lowerRuntimeValueTypeM env context resultTy
  unless (lvLLVMType value == expectedTy) $
    liftEither (BackendLLVMInternalError ("value LLVM type mismatch for " ++ name ++ " at " ++ context))
  pure value

bindLet :: ProgramEnv -> ExprEnv -> String -> String -> BackendExpr -> LowerM ExprEnv
bindLet env exprEnv context name rhs =
  case closurePointerAliasValue exprEnv rhs of
    Just value ->
      pure
        exprEnv
          { eeValues = Map.insert name value (eeValues exprEnv),
            eeLocalFunctions = Map.delete name (eeLocalFunctions exprEnv)
          }
    Nothing ->
      case functionFormFromExpected (backendExprType rhs) rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              pure
                exprEnv
                  { eeLocalFunctions =
                      Map.insert
                        name
                        LocalFunction
                          { lfForm = form,
                            lfCapturedEnv = exprEnv,
                            lfStoredReference = Just (backendExprType rhs, rhs)
                          }
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

closurePointerAliasValue :: ExprEnv -> BackendExpr -> Maybe LowerValue
closurePointerAliasValue exprEnv =
  \case
    BackendVar ty name
      | isFunctionLikeBackendType ty,
        Just value <- Map.lookup name (eeValues exprEnv),
        lvLLVMType value == LLVMPtr ->
          Just value {lvBackendType = ty}
    BackendTyApp ty fun _
      | isFunctionLikeBackendType ty,
        Just value <- closurePointerAliasValue exprEnv fun ->
          Just value {lvBackendType = ty}
    BackendLet ty name bindingTy rhs body
      | isFunctionLikeBackendType ty ->
          let exprEnvForBody =
                case closurePointerAliasValue exprEnv rhs of
                  Just value ->
                    exprEnv
                      { eeValues = Map.insert name value {lvBackendType = bindingTy} (eeValues exprEnv),
                        eeLocalFunctions = Map.delete name (eeLocalFunctions exprEnv)
                      }
                  Nothing ->
                    exprEnv
                      { eeValues = Map.delete name (eeValues exprEnv),
                        eeLocalFunctions = Map.delete name (eeLocalFunctions exprEnv)
                      }
           in closurePointerAliasValue exprEnvForBody body
    _ ->
      Nothing

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
      resultLLVMType <- lowerRuntimeValueTypeM env context expectedTy
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

lowerClosureValue :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> [BackendClosureCapture] -> LowerM LowerValue
lowerClosureValue env exprEnv context resultTy entryName captures = do
  captureValues <- traverse lowerCapture captures
  envPointer <- lowerClosureEnvironment captureValues
  closurePointer <- emitMalloc env context 16
  codePtrField <- emitGep "closure.code.ptr" closurePointer 0
  emitStore LLVMPtr (LLVMGlobalRef LLVMPtr entryName) codePtrField
  envPtrField <- emitGep "closure.env.ptr" closurePointer 8
  emitStore LLVMPtr envPointer envPtrField
  pure (LowerValue resultTy LLVMPtr closurePointer)
  where
    lowerCapture capture = do
      value <-
        if shouldLowerStoredFunctionCapture capture
          then
            lowerStoredFunctionArgument
              env
              exprEnv
              (context ++ ", closure capture " ++ show (backendClosureCaptureName capture))
              (backendClosureCaptureType capture)
              (backendClosureCaptureExpr capture)
          else
            lowerExpr env exprEnv (context ++ ", closure capture " ++ show (backendClosureCaptureName capture)) (backendClosureCaptureExpr capture)
      expectedTy <- lowerClosureStoredTypeM env context (backendClosureCaptureType capture)
      requireLLVMType context (backendClosureCaptureName capture) expectedTy value
      pure (expectedTy, value)

    shouldLowerStoredFunctionCapture capture =
      isFirstOrderFunctionPointerType (backendClosureCaptureType capture)
        && not (isEvidenceArgument True (backendClosureCaptureName capture) (backendClosureCaptureType capture))
        && not (captureExprIsRuntimeClosureValue capture)
        && case collectTyApps (backendClosureCaptureExpr capture) of
          (BackendVar {}, _) -> True
          _ -> False

    captureExprIsRuntimeClosureValue capture =
      case closurePointerAliasValue exprEnv (backendClosureCaptureExpr capture) of
        Just _ ->
          True
        Nothing ->
          captureExprNamesGlobalClosureValue capture

    captureExprNamesGlobalClosureValue capture =
      case collectTyApps (backendClosureCaptureExpr capture) of
        (BackendVar _ name, typeArgs)
          | Just binding <- Map.lookup name (pbBindings (peBase env)),
            Right (_, form) <- instantiateFunctionFormWithTypeArgs context (biForm binding) typeArgs [],
            null (ffParams form),
            alphaEqBackendType (backendClosureCaptureType capture) (ffReturnType form),
            isClosureRuntimeValueType (ffReturnType form) ->
              True
        _ ->
          False

    lowerClosureEnvironment [] =
      pure LLVMNull
    lowerClosureEnvironment captureValues = do
      envPointer <- emitMalloc env context (8 * length captureValues)
      zipWithM_ (storeCapture envPointer) [0 :: Int ..] captureValues
      pure envPointer

    storeCapture envPointer index0 (captureTy, value) = do
      fieldPtr <- emitGep "closure.env.field.ptr" envPointer (8 * index0)
      emitStore captureTy (lvOperand value) fieldPtr

lowerClosureCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> [BackendExpr] -> LowerM LowerValue
lowerClosureCall env exprEnv context resultTy fun args = do
  case collectTyApps fun of
    (BackendVar _ name, typeArgs)
      | Just localFunction <- Map.lookup name (eeLocalFunctions exprEnv) ->
          lowerLocalFunctionCall env exprEnv context name localFunction typeArgs args
    _ ->
      lowerClosurePointerCall
  where
    lowerClosurePointerCall = do
      callee <- lowerClosureCallee env exprEnv context fun
      lowerClosurePointerValueCall env exprEnv context resultTy callee args

lowerClosurePointerValueCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> LowerValue -> [BackendExpr] -> LowerM LowerValue
lowerClosurePointerValueCall env exprEnv context resultTy callee args = do
  unless (lvLLVMType callee == LLVMPtr) $
    liftEither (BackendLLVMUnsupportedExpression context ("closure callee is not a pointer: " ++ show (lvBackendType callee)))
  let (paramTys, returnTy) = collectArrowsType (lvBackendType callee)
  when (null paramTys) $
    liftEither (BackendLLVMUnsupportedExpression context ("closure callee is not a function: " ++ show (lvBackendType callee)))
  unless (length paramTys == length args) $
    liftEither (BackendLLVMArityMismatch "closure" (length paramTys) (length args))
  resultLLVMType <- lowerBackendTypeM env context resultTy
  returnLLVMType <- lowerBackendTypeM env context returnTy
  unless (resultLLVMType == returnLLVMType) $
    liftEither (BackendLLVMInternalError ("closure call result mismatch at " ++ context))
  callArgs <- zipWithM lowerClosureArg (zip [0 :: Int ..] paramTys) args
  codePtrField <- emitGep "closure.code.ptr" (lvOperand callee) 0
  codePtr <- emitAssign "closure.code" LLVMPtr (LLVMLoad LLVMPtr codePtrField)
  envPtrField <- emitGep "closure.env.ptr" (lvOperand callee) 8
  closureEnv <- emitAssign "closure.env" LLVMPtr (LLVMLoad LLVMPtr envPtrField)
  result <-
    emitAssign
      "closure.call"
      resultLLVMType
      ( LLVMCallOperand
          codePtr
          ((LLVMPtr, closureEnv) : [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
      )
  pure (LowerValue resultTy resultLLVMType result)
  where
    lowerClosureArg (index0, paramTy) arg =
      lowerExprForIndirectArgument env exprEnv context ("__mlfp_closure_arg" ++ show index0, paramTy) arg

lowerClosureCallee :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerClosureCallee env exprEnv context =
  \case
    BackendLet _ name _ rhs body -> do
      exprEnv' <- bindLet env exprEnv context name rhs
      lowerClosureCallee env exprEnv' context body
    expr ->
      lowerExpr env exprEnv context expr

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
                  lowerGlobalCall env exprEnv context (backendExprType expr) name typeArgs args
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
      resultTy <- lowerRuntimeValueTypeM env context (ffReturnType form)
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
      case [qualifiedSpecializationForm specialization | specialization <- Map.elems (peSpecializations env), spFunctionName specialization == functionName] of
        form : _ -> Just form
        [] ->
          case [qualifiedEvidenceWrapperForm wrapper | wrapper <- Map.elems (peEvidenceWrappers env), ewFunctionName wrapper == functionName] of
            form : _ -> Just form
            [] ->
              case [qualifiedFunctionWrapperForm wrapper | wrapper <- Map.elems (peFunctionWrappers env), fwFunctionName wrapper == functionName] of
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
  | isFirstOrderFunctionPointerType paramTy =
      lowerFunctionArgument env exprEnv context paramTy arg
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

lowerStoredFunctionArgument :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerStoredFunctionArgument env exprEnv context expectedTy arg =
  case collectTyApps arg of
    (BackendVar _ name, typeArgs) ->
      lowerStoredFunctionReference env exprEnv context expectedTy name typeArgs
    _ ->
      case Map.lookup (functionWrapperKey expectedTy arg) (peFunctionWrappers env) of
        Just wrapper ->
          pure (LowerValue expectedTy LLVMPtr (LLVMGlobalRef LLVMPtr (fwFunctionName wrapper)))
        Nothing ->
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

lowerStoredFunctionReference :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> [BackendType] -> LowerM LowerValue
lowerStoredFunctionReference env exprEnv context expectedTy name typeArgs =
  case Map.lookup name (eeLocalFunctions exprEnv) of
    Just localFunction ->
      lowerLocalFunctionReferenceWith True env context expectedTy name localFunction typeArgs
    Nothing ->
      lowerNonLocalFunctionReference env exprEnv context expectedTy name typeArgs

lowerLocalFunctionReference :: ProgramEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> LowerM LowerValue
lowerLocalFunctionReference =
  lowerLocalFunctionReferenceWith False

lowerLocalFunctionReferenceWith :: Bool -> ProgramEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> LowerM LowerValue
lowerLocalFunctionReferenceWith allowStoredReference env context expectedTy name localFunction typeArgs = do
  (resolvedTypeArgs, form0) <-
    if null typeArgs
      then pure ([], lfForm localFunction)
      else instantiateFunctionFormWithTypeArgsM context (lfForm localFunction) typeArgs []
  let form = qualifyInstantiatedClosureEntries name resolvedTypeArgs form0
  let actualTy = functionTypeFromFormWithBinders form
  requireEvidenceFunctionType context name expectedTy actualTy
  case etaAliasTarget form of
    Just (targetName, targetTypeArgs) ->
      if allowStoredReference
        then lowerStoredFunctionReference env (lfCapturedEnv localFunction) context expectedTy targetName targetTypeArgs
        else lowerFunctionReference env (lfCapturedEnv localFunction) context expectedTy targetName targetTypeArgs
    Nothing ->
      lowerLocalFunctionStoredReference env context expectedTy localFunction typeArgs >>= \case
        Just value ->
          if allowStoredReference
            then pure value
            else unsupportedFunctionArgument
        Nothing ->
          unsupportedFunctionArgument
  where
    unsupportedFunctionArgument =
      liftEither (BackendLLVMUnsupportedExpression context ("unsupported function argument " ++ show name))

lowerLocalFunctionStoredReference :: ProgramEnv -> String -> BackendType -> LocalFunction -> [BackendType] -> LowerM (Maybe LowerValue)
lowerLocalFunctionStoredReference env context expectedTy localFunction typeArgs =
  case lfStoredReference localFunction of
    Just (_, sourceExpr0) -> do
      sourceExpr <- storedReferenceSourceExpr context sourceExpr0 typeArgs
      case Map.lookup (functionWrapperKey expectedTy sourceExpr) (peFunctionWrappers env) of
        Just wrapper ->
          pure (Just (LowerValue expectedTy LLVMPtr (LLVMGlobalRef LLVMPtr (fwFunctionName wrapper))))
        Nothing ->
          pure Nothing
    Nothing ->
      pure Nothing

storedReferenceSourceExpr :: String -> BackendExpr -> [BackendType] -> LowerM BackendExpr
storedReferenceSourceExpr _ sourceExpr [] =
  pure sourceExpr
storedReferenceSourceExpr context sourceExpr typeArgs =
  case applyTypeApplicationsToExprWithType context sourceExpr typeArgs of
    Right (applied, _) -> pure applied
    Left err -> liftEither err

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
      (BTArrow leftParam leftResult, BTArrow rightParam rightResult) ->
        runtimeCompatibleValueType leftParam rightParam
          && runtimeCompatibleValueType leftResult rightResult
      (BTForall leftName leftBound leftBody, BTForall rightName rightBound rightBody) ->
        runtimeCompatibleMaybeType leftBound rightBound
          && runtimeCompatibleValueType
            (substituteBackendType leftName (BTVar freshName) leftBody)
            (substituteBackendType rightName (BTVar freshName) rightBody)
        where
          freshName =
            freshNameLike
              leftName
              ( Set.unions
                  [ backendTypeVariableNames leftBody,
                    backendTypeVariableNames rightBody,
                    maybe Set.empty backendTypeVariableNames leftBound,
                    maybe Set.empty backendTypeVariableNames rightBound,
                    Set.fromList [leftName, rightName]
                  ]
              )
      (BTBase leftBase, BTBase rightBase) -> leftBase == rightBase
      (BTCon leftCon leftArgs, BTCon rightCon rightArgs) ->
        leftCon == rightCon
          && length leftArgs == length rightArgs
          && and (zipWith runtimeCompatibleValueType (NE.toList leftArgs) (NE.toList rightArgs))
      (BTBottom, BTBottom) -> True
      _ -> False

runtimeCompatibleMaybeType :: Maybe BackendType -> Maybe BackendType -> Bool
runtimeCompatibleMaybeType Nothing Nothing =
  True
runtimeCompatibleMaybeType (Just left) (Just right) =
  runtimeCompatibleValueType left right
runtimeCompatibleMaybeType _ _ =
  False

backendTypeVariableNames :: BackendType -> Set String
backendTypeVariableNames =
  \case
    BTVar name ->
      Set.singleton name
    BTArrow param result ->
      backendTypeVariableNames param `Set.union` backendTypeVariableNames result
    BTBase {} ->
      Set.empty
    BTCon _ args ->
      Set.unions (map backendTypeVariableNames (NE.toList args))
    BTVarApp name args ->
      Set.insert name (Set.unions (map backendTypeVariableNames (NE.toList args)))
    BTForall name mbBound body ->
      Set.insert name $
        maybe Set.empty backendTypeVariableNames mbBound `Set.union` backendTypeVariableNames body
    BTMu name body ->
      Set.insert name (backendTypeVariableNames body)
    BTBottom ->
      Set.empty

lowerLocalFunctionCall :: ProgramEnv -> ExprEnv -> String -> String -> LocalFunction -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerLocalFunctionCall env callEnv context name localFunction typeArgs args = do
  let allowNestedEvidence = isEvidenceName name
  (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (lfForm localFunction) typeArgs args
  let form = qualifyInstantiatedClosureEntries name resolvedTypeArgs form0
  bodyEnv <- bindCallArguments env callEnv (lfCapturedEnv localFunction) context allowNestedEvidence name form args
  lowerExpr env bodyEnv context (ffBody form)

lowerDirectFunctionCall :: ProgramEnv -> ExprEnv -> String -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerDirectFunctionCall env exprEnv context form0 typeArgs args = do
  (resolvedTypeArgs, form1) <- instantiateFunctionFormWithTypeArgsM context form0 typeArgs args
  let form = qualifyInstantiatedClosureEntries "__mlfp_direct_typeapp" resolvedTypeArgs form1
  bodyEnv <- bindCallArguments env exprEnv exprEnv context False "lambda" form args
  lowerExpr env bodyEnv context (ffBody form)

lowerGlobalCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerGlobalCall env exprEnv context resultTy name typeArgs args =
  case Map.lookup name (pbBindings (peBase env)) of
    Just binding -> do
      (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs args
      let form = qualifyInstantiatedClosureEntries name resolvedTypeArgs form0
          arity = length (ffParams form)
      case compare (length args) arity of
        GT -> do
          unless (isClosureRuntimeValueType (ffReturnType form)) $
            liftEither (BackendLLVMArityMismatch name arity (length args))
          let (directArgs, closureArgs) = splitAt arity args
          callee <- lowerGlobalCall env exprEnv context (ffReturnType form) name typeArgs directArgs
          lowerClosurePointerValueCall env exprEnv context resultTy callee closureArgs
        LT ->
          liftEither (BackendLLVMArityMismatch name arity (length args))
        EQ ->
          lowerSaturatedGlobalCall binding resolvedTypeArgs form
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
  where
    lowerSaturatedGlobalCall binding resolvedTypeArgs form =
      if requiresInlineCall form
        && Set.member (biName binding) (eeActiveGlobalInlines exprEnv)
        && not (canEmitFunctionForm form)
        then liftEither (BackendLLVMUnsupportedExpression context ("recursive static global " ++ show name))
        else if shouldInlineGlobalCall env exprEnv binding resolvedTypeArgs form args
        then do
          let bodyEnv0 =
                exprEnv
                  { eeActiveGlobalInlines = Set.insert (biName binding) (eeActiveGlobalInlines exprEnv)
                  }
          bodyEnv <- bindCallArguments env exprEnv bodyEnv0 context False name form args
          lowerExpr env bodyEnv context (ffBody form)
        else do
          callArgs <- zipWithM (lowerExprForArgument env exprEnv context False) (ffParams form) args
          bindFunctionArguments env context False name form callArgs
          llvmResultTy <- lowerRuntimeValueTypeM env context (ffReturnType form)
          functionName <- globalFunctionName env context binding resolvedTypeArgs
          result <- emitAssign "call" llvmResultTy (LLVMCall functionName [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
          pure (LowerValue (ffReturnType form) llvmResultTy result)

shouldInlineGlobalCall :: ProgramEnv -> ExprEnv -> BindingInfo -> [BackendType] -> FunctionForm -> [BackendExpr] -> Bool
shouldInlineGlobalCall env exprEnv binding resolvedTypeArgs form args =
  (requiresInlineCall form && Set.notMember (biName binding) (eeActiveGlobalInlines exprEnv))
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

lowerStaticFunctionArgument :: ProgramEnv -> ExprEnv -> String -> String -> BackendType -> BackendExpr -> LowerM LocalFunction
lowerStaticFunctionArgument env callEnv context paramName expectedTy arg =
  case collectTyApps arg of
    (BackendVar _ name, typeArgs) ->
      case Map.lookup name (eeLocalFunctions callEnv) of
        Just localFunction -> do
          form <- instantiateStaticFunctionForm context expectedTy (lfForm localFunction) typeArgs
          requireStaticFunctionType context paramName expectedTy (localFunction {lfForm = form})
        Nothing ->
          case Map.lookup name (pbBindings (peBase env)) of
            Just binding -> do
              form <- instantiateStaticFunctionForm context expectedTy (biForm binding) typeArgs
              if canEmitFunctionForm form
                then lowerDirectStaticFunctionArgument callEnv context paramName expectedTy arg
                else
                  requireStaticFunctionType
                    context
                    paramName
                    expectedTy
                    ( LocalFunction
                        { lfForm = form,
                          lfCapturedEnv = emptyExprEnv,
                          lfStoredReference = Nothing
                        }
                    )
            Nothing ->
              lowerDirectStaticFunctionArgument callEnv context paramName expectedTy arg
    _ ->
      lowerDirectStaticFunctionArgument callEnv context paramName expectedTy arg

instantiateStaticFunctionForm :: String -> BackendType -> FunctionForm -> [BackendType] -> LowerM FunctionForm
instantiateStaticFunctionForm context expectedTy form typeArgs
  | null typeArgs && alphaEqBackendType expectedTy (functionFormType form) =
      pure form
  | not (null typeArgs) =
      instantiateFunctionFormM context form typeArgs []
  | null (ffTypeBinders form) =
      pure form
  | otherwise = do
      inferredTypeArgs <- inferStaticFunctionTypeArgs context expectedTy form
      instantiateFunctionFormM context form inferredTypeArgs []

inferStaticFunctionTypeArgs :: String -> BackendType -> FunctionForm -> LowerM [BackendType]
inferStaticFunctionTypeArgs context expectedTy form =
  case matchTypeParams binderSet Map.empty sourceTy expectedTy >>= resolvedTypeArguments context binderNames of
    Right typeArgs -> pure typeArgs
    Left err -> liftEither err
  where
    binderNames = map fst (ffTypeBinders form)
    binderSet = Set.fromList binderNames
    sourceTy = foldr BTArrow (ffReturnType form) (map snd (ffParams form))

lowerDirectStaticFunctionArgument :: ExprEnv -> String -> String -> BackendType -> BackendExpr -> LowerM LocalFunction
lowerDirectStaticFunctionArgument callEnv context paramName expectedTy arg = do
  let form = functionFormFromExpected expectedTy arg
  when (null (ffTypeBinders form) && null (ffParams form)) $
    liftEither
      ( BackendLLVMUnsupportedExpression
          context
          ("unsupported static function argument " ++ show paramName)
      )
  requireStaticFunctionType
    context
    paramName
    expectedTy
    ( LocalFunction
        { lfForm = form,
          lfCapturedEnv = callEnv,
          lfStoredReference = Just (expectedTy, arg)
        }
    )

requireStaticFunctionType :: String -> String -> BackendType -> LocalFunction -> LowerM LocalFunction
requireStaticFunctionType context paramName expectedTy localFunction = do
  unless (alphaEqBackendType expectedTy (functionFormType (lfForm localFunction))) $
    liftEither
      ( BackendLLVMUnsupportedExpression
          context
          ( "static argument "
              ++ show paramName
              ++ " has type "
              ++ show (functionFormType (lfForm localFunction))
              ++ ", expected "
              ++ show expectedTy
          )
      )
  pure localFunction

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
      | isFirstOrderFunctionPointerType paramTy = do
          mbClosureValue <- lowerClosureRuntimeArgumentMaybe env callEnv context paramTy arg
          case mbClosureValue of
            Just value ->
              bindValue bodyEnv paramName value
            Nothing ->
              bindInlineOrValue bodyEnv paramName paramTy arg
      | isInlineFunctionArgument allowNestedEvidence paramName paramTy = do
          localFunction <- lowerStaticFunctionArgument env callEnv context paramName paramTy arg
          pure
            bodyEnv
              { eeLocalFunctions =
                  Map.insert
                    paramName
                    localFunction
                    (eeLocalFunctions bodyEnv),
                eeValues = Map.delete paramName (eeValues bodyEnv)
              }
      | otherwise = do
          bindValueFromExpr bodyEnv paramName paramTy arg

    bindInlineOrValue bodyEnv paramName paramTy arg
      | isInlineFunctionArgument allowNestedEvidence paramName paramTy = do
          localFunction <- lowerStaticFunctionArgument env callEnv context paramName paramTy arg
          pure
            bodyEnv
              { eeLocalFunctions =
                  Map.insert
                    paramName
                    localFunction
                    (eeLocalFunctions bodyEnv),
                eeValues = Map.delete paramName (eeValues bodyEnv)
              }
      | otherwise =
          bindValueFromExpr bodyEnv paramName paramTy arg

    bindValueFromExpr bodyEnv paramName paramTy arg = do
      value <- lowerExprForArgument env callEnv context allowNestedEvidence (paramName, paramTy) arg
      bindValue bodyEnv paramName value

    bindValue bodyEnv paramName value =
      pure
        bodyEnv
          { eeValues = Map.insert paramName value (eeValues bodyEnv),
            eeLocalFunctions = Map.delete paramName (eeLocalFunctions bodyEnv)
          }

lowerClosureRuntimeArgumentMaybe :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM (Maybe LowerValue)
lowerClosureRuntimeArgumentMaybe env exprEnv context expectedTy arg =
  case closurePointerAliasValue exprEnv arg of
    Just value ->
      pure (Just value {lvBackendType = expectedTy})
    Nothing ->
      case arg of
        BackendClosure {}
          | alphaEqBackendType expectedTy (backendExprType arg) ->
              Just <$> lowerExpr env exprEnv context arg
        _ ->
          case collectTyApps arg of
            (BackendVar _ name, typeArgs)
              | Just binding <- Map.lookup name (pbBindings (peBase env)) -> do
                  (_, form) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs []
                  if null (ffParams form)
                    && alphaEqBackendType expectedTy (ffReturnType form)
                    && isClosureRuntimeValueType (ffReturnType form)
                    then Just <$> lowerGlobalValue env context expectedTy name binding typeArgs
                    else pure Nothing
            _ ->
              pure Nothing

isInlineFunctionArgument :: Bool -> String -> BackendType -> Bool
isInlineFunctionArgument allowNestedEvidence paramName paramTy =
  isInlineOnlyFunctionParameter allowNestedEvidence paramName paramTy

isInlineOnlyFunctionParameter :: Bool -> String -> BackendType -> Bool
isInlineOnlyFunctionParameter allowNestedEvidence paramName paramTy =
  isFunctionLikeBackendType paramTy
    && (evidenceNeedsInlining || nonEvidenceNeedsInlining)
  where
    evidenceLike = isEvidenceArgument allowNestedEvidence paramName paramTy
    polymorphicFunction = hasTypeBinders paramTy
    evidenceNeedsInlining = evidenceLike && polymorphicFunction
    nonEvidenceNeedsInlining = not evidenceLike

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

data TypeParamMatchStrictness
  = AllowResidualTypeMismatch
  | RejectResidualTypeMismatch
  deriving (Eq, Show)

matchTypeParams :: Set String -> Map String BackendType -> BackendType -> BackendType -> Either BackendLLVMError (Map String BackendType)
matchTypeParams binderSet substitution expected actual =
  matchTypeParamsWith AllowResidualTypeMismatch binderSet substitution expected actual

matchTypeParamsWith ::
  TypeParamMatchStrictness ->
  Set String ->
  Map String BackendType ->
  BackendType ->
  BackendType ->
  Either BackendLLVMError (Map String BackendType)
matchTypeParamsWith strictness binderSet substitution expected actual =
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
          matchTypeParamsWith strictness binderSet substitution leftA leftB >>= \subst ->
            matchTypeParamsWith strictness binderSet subst rightA rightB
        (BTCon conA argsA, BTCon conB argsB)
          | conA == conB && length argsA == length argsB ->
              foldM
                (\subst (tyA, tyB) -> matchTypeParamsWith strictness binderSet subst tyA tyB)
                substitution
                (zip (NE.toList argsA) (NE.toList argsB))
        (BTVarApp name args, _) ->
          matchTypeParamApplication binderSet substitution name (NE.toList args) actual
        (BTBase baseA, BTBase baseB)
          | baseA == baseB -> Right substitution
        (BTForall nameA boundA bodyA, BTForall nameB boundB bodyB) -> do
          substA <-
            case (boundA, boundB) of
              (Nothing, Nothing) -> Right substitution
              (Just tyA, Just tyB) -> matchTypeParamsWith strictness binderSet substitution tyA tyB
              _ -> Left (BackendLLVMUnsupportedCall "mismatched forall bounds during type argument inference")
          matchTypeParamsWith strictness binderSet substA bodyA (substituteBackendType nameB (BTVar nameA) bodyB)
        (BTMu nameA bodyA, BTMu nameB bodyB) ->
          matchTypeParamsWith strictness binderSet substitution bodyA (substituteBackendType nameB (BTVar nameA) bodyB)
        (BTBottom, BTBottom) -> Right substitution
        _
          | alphaEqBackendType expected actual ->
              Right substitution
          | strictness == RejectResidualTypeMismatch ->
              Left
                ( BackendLLVMUnsupportedCall
                    ( "type application argument mismatch during type argument inference: expected "
                        ++ show expected
                        ++ ", got "
                        ++ show actual
                    )
                )
          | otherwise ->
              Right substitution

matchTypeParamApplication ::
  Set String ->
  Map String BackendType ->
  String ->
  [BackendType] ->
  BackendType ->
  Either BackendLLVMError (Map String BackendType)
matchTypeParamApplication binderSet substitution name expectedArgs actual =
  case decomposeBackendTypeHead actual of
    Just (actualHead, actualArgs)
      | length expectedArgs == length actualArgs -> do
          substitution' <-
            if Set.member name binderSet
              then bindTypeParam name actualHead
              else matchRigidHead name actualHead
          foldM
            (\subst (expectedArg, actualArg) -> matchTypeParamsWith RejectResidualTypeMismatch binderSet subst expectedArg actualArg)
            substitution'
            (zip expectedArgs actualArgs)
      | otherwise ->
          Left (BackendLLVMUnsupportedCall ("type application arity mismatch during type argument inference for " ++ name))
    _ ->
      Left (BackendLLVMUnsupportedCall ("expected applied type while inferring type argument for " ++ name))
  where
    matchRigidHead expectedName actualHead
      | alphaEqBackendType (BTVar expectedName) actualHead = Right substitution
      | otherwise =
          Left
            ( BackendLLVMUnsupportedCall
                ( "rigid type application head mismatch during type argument inference: expected "
                    ++ show (BTVar expectedName)
                    ++ ", got "
                    ++ show actualHead
                )
            )

    bindTypeParam paramName actualHead =
      case Map.lookup paramName substitution of
        Nothing -> Right (Map.insert paramName actualHead substitution)
        Just previous
          | alphaEqBackendType previous actualHead -> Right substitution
          | otherwise -> Left (BackendLLVMUnsupportedCall ("conflicting inferred type argument for " ++ paramName))

inferTypeArgumentsForTest :: String -> [String] -> [(String, BackendType)] -> [BackendExpr] -> Either BackendLLVMError (Map String BackendType)
inferTypeArgumentsForTest =
  inferTypeArguments

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
      fieldTys <-
        case constructorRuntimeFieldTypes constructorRuntime resultTy of
          Just resolvedFieldTys -> pure resolvedFieldTys
          Nothing ->
            liftEither
              ( BackendLLVMUnsupportedExpression
                  context
                  ("could not match constructor result for " ++ backendConstructorName constructor)
              )
      unless (length args == length fieldTys) $
        liftEither (BackendLLVMArityMismatch name (length fieldTys) (length args))
      argValues <- zipWithM (lowerConstructField env exprEnv context) fieldTys args
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

lowerConstructField :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerConstructField env exprEnv context fieldTy arg
  | isClosureRuntimeValueType fieldTy = do
      value <- lowerExpr env exprEnv context arg
      expectedTy <- lowerRuntimeValueTypeM env context fieldTy
      requireLLVMType context "constructor field" expectedTy value
      pure value {lvBackendType = fieldTy}
  | isFirstOrderFunctionPointerType fieldTy =
      lowerStoredFunctionArgument env exprEnv context fieldTy arg
  | otherwise =
      lowerExpr env exprEnv context arg

lowerCase :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> NonEmpty BackendAlternative -> LowerM LowerValue
lowerCase env exprEnv context resultTy scrutinee alternatives =
  case scrutinee of
    BackendConstruct scrutineeTy name args ->
      case Map.lookup name (pbConstructors (peBase env)) of
        Just constructorRuntime -> do
          fieldTys <- constructorFieldTypesForScrutinee env context constructorRuntime scrutineeTy
          if any backendTypeRequiresStaticSpecialization fieldTys
            then lowerImmediateConstructCase env exprEnv context resultTy name args fieldTys alternatives
            else lowerHeapCase env exprEnv context resultTy scrutinee alternatives
        Nothing ->
          lowerHeapCase env exprEnv context resultTy scrutinee alternatives
    _ ->
      lowerHeapCase env exprEnv context resultTy scrutinee alternatives

lowerHeapCase :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> NonEmpty BackendAlternative -> LowerM LowerValue
lowerHeapCase env exprEnv context resultTy scrutinee alternatives = do
  rejectNonTailDefaultAlternative
  resultLLVMType <- lowerCaseResultTypeM env context resultTy
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
                  bodyBinderTypes = backendExprVarTypesFor (Set.fromList binders) body
                  effectiveFieldTys =
                    [ Map.findWithDefault fieldTy binder bodyBinderTypes
                      | (binder, fieldTy) <- zip binders fieldTys
                    ]
              loadedFields <- mapMaybe id <$> traverse (loadUsedField usedBinders scrutineeValue) (zip3 [0 :: Int ..] binders effectiveFieldTys)
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
      llvmTy <- lowerStoredFieldTypeM env context fieldTy
      fieldPtr <- emitGep "case.field.ptr" (lvOperand scrutineeValue) (8 * (index0 + 1))
      loaded <- emitAssign "case.field" llvmTy (LLVMLoad llvmTy fieldPtr)
      pure (LowerValue fieldTy llvmTy loaded)

lowerStoredFieldTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerStoredFieldTypeM env context fieldTy
  | isClosureRuntimeValueType fieldTy = pure LLVMPtr
  | isFirstOrderFunctionPointerType fieldTy = pure LLVMPtr
  | otherwise = lowerBackendTypeM env context fieldTy

lowerClosureStoredTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerClosureStoredTypeM env context fieldTy
  | isClosureRuntimeValueType fieldTy = pure LLVMPtr
  | isFirstOrderFunctionPointerType fieldTy = pure LLVMPtr
  | otherwise = lowerBackendTypeM env context fieldTy

lowerRuntimeValueTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerRuntimeValueTypeM env context resultTy =
  case lowerRuntimeValueType env context resultTy of
    Right llvmTy -> pure llvmTy
    Left err -> liftEither err

lowerRuntimeValueType :: ProgramEnv -> String -> BackendType -> Either BackendLLVMError LLVMType
lowerRuntimeValueType env context resultTy
  | isClosureRuntimeValueType resultTy = Right LLVMPtr
  | otherwise = lowerBackendType env context resultTy

lowerCaseResultTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerCaseResultTypeM env context resultTy
  | isClosureRuntimeValueType resultTy = pure LLVMPtr
  | otherwise = lowerBackendTypeM env context resultTy

isClosureRuntimeValueType :: BackendType -> Bool
isClosureRuntimeValueType =
  \case
    BTArrow {} -> True
    _ -> False

lowerImmediateConstructCase ::
  ProgramEnv ->
  ExprEnv ->
  String ->
  BackendType ->
  String ->
  [BackendExpr] ->
  [BackendType] ->
  NonEmpty BackendAlternative ->
  LowerM LowerValue
lowerImmediateConstructCase env exprEnv context resultTy constructorName args fieldTys alternatives = do
  rejectNonTailDefaultAlternative
  rejectDuplicateConstructorAlternatives
  unless (length args == length fieldTys) $
    liftEither (BackendLLVMArityMismatch constructorName (length fieldTys) (length args))
  case selectedAlternative of
    Just alternative -> do
      exprEnv' <- bindImmediateAlternativePattern alternative
      bodyValue <- lowerExpr env exprEnv' context (backendAltBody alternative)
      expectedTy <- lowerCaseResultTypeM env context resultTy
      unless (lvLLVMType bodyValue == expectedTy) $
        liftEither (BackendLLVMInternalError ("immediate case alternative type mismatch at " ++ context))
      pure bodyValue
    Nothing ->
      lowerUnmatchedImmediateCase
  where
    alternativesList = NE.toList alternatives

    selectedAlternative =
      case [alternative | alternative@(BackendAlternative (BackendConstructorPattern name _) _) <- alternativesList, name == constructorName] of
        alternative : _ -> Just alternative
        [] ->
          case [alternative | alternative@(BackendAlternative BackendDefaultPattern _) <- alternativesList] of
            alternative : _ -> Just alternative
            [] -> Nothing

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

    rejectDuplicateConstructorAlternatives =
      case firstDuplicate [name | BackendAlternative (BackendConstructorPattern name _) _ <- alternativesList] of
        Just name ->
          liftEither (BackendLLVMUnsupportedExpression context ("duplicate constructor case alternative " ++ show name))
        Nothing ->
          pure ()

    lowerUnmatchedImmediateCase = do
      zipWithM_ evaluateUnusedField fieldTys args
      expectedTy <- lowerCaseResultTypeM env context resultTy
      finishCurrentBlock LLVMUnreachable
      continuationLabel <- freshBlock "case.unreachable.cont"
      startBlock continuationLabel
      operand <- dummyOperandAfterUnreachable context expectedTy
      pure (LowerValue resultTy expectedTy operand)

    bindImmediateAlternativePattern (BackendAlternative pattern0 body) =
      case pattern0 of
        BackendDefaultPattern -> do
          zipWithM_ evaluateUnusedField fieldTys args
          pure exprEnv
        BackendConstructorPattern name binders -> do
          unless (name == constructorName) $
            liftEither
              ( BackendLLVMUnsupportedExpression
                  context
                  ("selected immediate constructor mismatch " ++ show name ++ " for " ++ show constructorName)
              )
          unless (length binders == length fieldTys) $
            liftEither (BackendLLVMArityMismatch name (length fieldTys) (length binders))
          foldM
            (bindUsedField (freeBackendExprVars body))
            exprEnv
            (zip3 binders fieldTys args)

    bindUsedField usedBinders acc (binder, fieldTy, arg)
      | backendTypeRequiresStaticSpecialization fieldTy = do
          localFunction <- lowerStaticFunctionArgument env exprEnv context binder fieldTy arg
          if Set.member binder usedBinders
            then pure acc {eeLocalFunctions = Map.insert binder localFunction (eeLocalFunctions acc)}
            else pure acc
      | backendTypeHasRuntimeRepresentation env fieldTy = do
          value <- lowerConstructField env exprEnv context fieldTy arg
          expectedTy <- lowerRuntimeValueTypeM env context fieldTy
          requireLLVMType context constructorName expectedTy value
          if Set.member binder usedBinders
            then pure acc {eeValues = Map.insert binder value (eeValues acc)}
            else pure acc
      | otherwise = do
          liftEither (BackendLLVMUnsupportedType ("field " ++ show binder ++ " at " ++ context) fieldTy)

    evaluateUnusedField fieldTy arg
      | backendTypeRequiresStaticSpecialization fieldTy = do
          _ <- lowerStaticFunctionArgument env exprEnv context "_" fieldTy arg
          pure ()
      | backendTypeHasRuntimeRepresentation env fieldTy = do
          value <- lowerConstructField env exprEnv context fieldTy arg
          expectedTy <- lowerRuntimeValueTypeM env context fieldTy
          requireLLVMType context constructorName expectedTy value
      | otherwise =
          liftEither (BackendLLVMUnsupportedType ("field at " ++ context) fieldTy)

dummyOperandAfterUnreachable :: String -> LLVMType -> LowerM LLVMOperand
dummyOperandAfterUnreachable _ (LLVMInt width) =
  pure (LLVMIntLiteral width 0)
dummyOperandAfterUnreachable _ LLVMPtr =
  pure LLVMNull
dummyOperandAfterUnreachable context ty =
  liftEither (BackendLLVMInternalError ("cannot synthesize unreachable value of type " ++ show ty ++ " at " ++ context))

constructorFieldTypesForScrutinee :: ProgramEnv -> String -> ConstructorRuntime -> BackendType -> LowerM [BackendType]
constructorFieldTypesForScrutinee _ context constructorRuntime scrutineeTy =
  case constructorRuntimeFieldTypes constructorRuntime scrutineeTy of
    Just fieldTys -> pure fieldTys
    Nothing ->
      liftEither
        ( BackendLLVMUnsupportedExpression
            context
            ("could not match constructor result for " ++ backendConstructorName (crConstructor constructorRuntime))
        )

constructorRuntimeFieldTypes :: ConstructorRuntime -> BackendType -> Maybe [BackendType]
constructorRuntimeFieldTypes constructorRuntime scrutineeTy =
  case structuralConstructorFieldTypes constructorRuntime scrutineeTy of
    Just fieldTys ->
      Just fieldTys
    Nothing ->
      case matchConstructorResult (crDataParameters constructorRuntime) parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
        Just substitution ->
          Just (map (substituteBackendTypes substitution) (backendConstructorFields constructor))
        Nothing ->
          Nothing
  where
    constructor = crConstructor constructorRuntime
    parameters =
      Set.fromList
        ( crDataParameters constructorRuntime
            ++ map backendTypeBinderName (backendConstructorForalls constructor)
        )

structuralConstructorFieldTypes :: ConstructorRuntime -> BackendType -> Maybe [BackendType]
structuralConstructorFieldTypes constructorRuntime scrutineeTy = do
  (muName, body) <-
    case scrutineeTy of
      BTMu name muBody -> Just (name, muBody)
      _ -> Nothing
  dataName <- constructorResultDataName (backendConstructorResult constructor)
  if structuralMuNameMatches dataName muName
    then do
      handlerFields <- structuralBackendHandlerFields body
      fieldTys <- atMay handlerFields (fromInteger (crTag constructorRuntime))
      if length fieldTys == length (backendConstructorFields constructor)
        then Just (map (substituteBackendType muName scrutineeTy) fieldTys)
        else Nothing
    else Nothing
  where
    constructor = crConstructor constructorRuntime

constructorResultDataName :: BackendType -> Maybe String
constructorResultDataName =
  \case
    BTBase (BaseTy name) -> Just name
    BTCon (BaseTy name) _ -> Just name
    BTMu name _ -> structuralMuDataName name
    _ -> Nothing

structuralBackendHandlerFields :: BackendType -> Maybe [[BackendType]]
structuralBackendHandlerFields =
  \case
    BTForall resultName _ handlerTy -> collectHandlers resultName handlerTy
    _ -> Nothing
  where
    collectHandlers resultName =
      go []
      where
        go handlers ty
          | alphaEqBackendType ty (BTVar resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> do
                  fields <- collectHandlerFields resultName handlerTy
                  go (handlers ++ [fields]) rest
                _ -> Nothing

    collectHandlerFields resultName =
      go []
      where
        go fields ty
          | alphaEqBackendType ty (BTVar resultName) = Just fields
          | otherwise =
              case ty of
                BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
                _ -> Nothing

atMay :: [a] -> Int -> Maybe a
atMay xs index0
  | index0 < 0 = Nothing
  | otherwise =
      case drop index0 xs of
        value : _ -> Just value
        [] -> Nothing

matchConstructorResult :: [String] -> Set String -> Map String BackendType -> BackendType -> BackendType -> Maybe (Map String BackendType)
matchConstructorResult dataParameterOrder parameters substitution expected actual =
  case expected of
    BTVar name
      | Set.member name parameters ->
          case Map.lookup name substitution of
            Nothing -> Just (Map.insert name actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Just substitution
              | otherwise -> Nothing
    _ ->
      if alphaEqBackendType expected actual
        then Just substitution
        else
          ( case (expected, actual) of
              (BTVar expectedName, BTVar actualName)
                | expectedName == actualName -> Just substitution
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedDom actualDom
                  >>= \subst -> matchConstructorResult dataParameterOrder parameters subst expectedCod actualCod
              (BTBase expectedBase, BTBase actualBase)
                | expectedBase == actualBase -> Just substitution
              (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
                | expectedCon == actualCon && length expectedArgs == length actualArgs ->
                    foldM
                      (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
                      substitution
                      (zip (NE.toList expectedArgs) (NE.toList actualArgs))
              (BTMu expectedName expectedBody, actualTy@(BTBase {})) ->
                matchStructuralMuExpected expectedName expectedBody actualTy
              (BTMu expectedName expectedBody, actualTy@(BTCon {})) ->
                matchStructuralMuExpected expectedName expectedBody actualTy
              (expectedTy@(BTBase {}), BTMu actualName actualBody) ->
                matchStructuralMuActual expectedTy actualName actualBody
              (expectedTy@(BTCon {}), BTMu actualName actualBody) ->
                matchStructuralMuActual expectedTy actualName actualBody
              (BTVarApp expectedName expectedArgs, _) ->
                matchConstructorResultApplication dataParameterOrder parameters substitution expectedName (NE.toList expectedArgs) actual
              (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
                subst <-
                  case (expectedBound, actualBound) of
                    (Nothing, Nothing) -> Just substitution
                    (Just expectedBoundTy, Just actualBoundTy) -> matchConstructorResult dataParameterOrder parameters substitution expectedBoundTy actualBoundTy
                    _ -> Nothing
                matchConstructorResult dataParameterOrder parameters subst expectedBody (substituteBackendType actualName (BTVar expectedName) actualBody)
              (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedBody (substituteBackendType actualName (BTVar expectedName) actualBody)
              (BTBottom, BTBottom) ->
                Just substitution
              _ ->
                Nothing
          )
  where
    matchStructuralMuExpected muName _body actualTy =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
          structuralMuAsActualDataType muName actualTy
            >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
        ]

    matchStructuralMuActual expectedTy muName _body =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
          structuralMuAsActualDataType muName expectedTy
            >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
        ]

    firstJust =
      \case
        [] -> Nothing
        candidate : rest ->
          case candidate of
            Just value -> Just value
            Nothing -> firstJust rest

structuralMuAsDataType :: [String] -> String -> Maybe BackendType
structuralMuAsDataType dataParameterOrder muName = do
  dataName <- structuralMuDataName muName
  let parameterArgs = map BTVar dataParameterOrder
  Just $
    case parameterArgs of
      [] -> BTBase (BaseTy dataName)
      arg : rest -> BTCon (BaseTy dataName) (arg NE.:| rest)

structuralMuAsActualDataType :: String -> BackendType -> Maybe BackendType
structuralMuAsActualDataType muName actual =
  case actual of
    BTBase (BaseTy actualName)
      | structuralMuNameMatches actualName muName -> Just actual
    BTCon (BaseTy actualName) _
      | structuralMuNameMatches actualName muName -> Just actual
    _ -> Nothing

structuralMuNameMatches :: String -> String -> Bool
structuralMuNameMatches dataName muName =
  case structuralMuDataName muName of
    Just muDataName -> dataName == muDataName
    Nothing -> False

structuralMuDataName :: String -> Maybe String
structuralMuDataName name =
  stripSuffixSimple "_self" (dropWhile (== '$') name)

stripSuffixSimple :: String -> String -> Maybe String
stripSuffixSimple suffix value =
  reverse <$> stripPrefixSimple (reverse suffix) (reverse value)

stripPrefixSimple :: String -> String -> Maybe String
stripPrefixSimple [] value =
  Just value
stripPrefixSimple _ [] =
  Nothing
stripPrefixSimple (expected : expectedRest) (actual : actualRest)
  | expected == actual = stripPrefixSimple expectedRest actualRest
  | otherwise = Nothing

matchConstructorResultApplication ::
  [String] ->
  Set String ->
  Map String BackendType ->
  String ->
  [BackendType] ->
  BackendType ->
  Maybe (Map String BackendType)
matchConstructorResultApplication dataParameterOrder parameters substitution name expectedArgs actual =
  case decomposeBackendTypeHead actual of
    Just (actualHead, actualArgs)
      | length expectedArgs == length actualArgs -> do
          substitution' <-
            if Set.member name parameters
              then insertParameterSubstitution name actualHead substitution
              else matchConstructorResult dataParameterOrder parameters substitution (BTVar name) actualHead
          foldM
            (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
            substitution'
            (zip expectedArgs actualArgs)
    _ -> Nothing
  where
    insertParameterSubstitution paramName actualHead substitution0 =
      case Map.lookup paramName substitution0 of
        Nothing -> Just (Map.insert paramName actualHead substitution0)
        Just previous
          | alphaEqBackendType previous actualHead -> Just substitution0
          | otherwise -> Nothing

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
    BTVarApp {} -> Left (BackendLLVMUnsupportedType context ty)
    BTArrow {} -> Left (BackendLLVMUnsupportedType context ty)
    BTForall {} -> Left (BackendLLVMUnsupportedType context ty)
    BTBottom -> Left (BackendLLVMUnsupportedType context ty)

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVar name -> Just (BTVar name, [])
    BTBase name -> Just (BTBase name, [])
    BTCon name args -> Just (BTBase name, NE.toList args)
    BTVarApp name args -> Just (BTVar name, NE.toList args)
    _ -> Nothing

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
        BackendClosure resultTy entryName captures params body ->
          BackendClosure
            (substituteTy resultTy)
            entryName
            (map substituteCapture captures)
            [(paramName, substituteTy paramTy) | (paramName, paramTy) <- params]
            (go body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall (substituteTy resultTy) (go fun) (map go args)

    substituteAlternative alternative =
      alternative {backendAltBody = go (backendAltBody alternative)}

    substituteCapture capture =
      capture
        { backendClosureCaptureType = substituteTy (backendClosureCaptureType capture),
          backendClosureCaptureExpr = go (backendClosureCaptureExpr capture)
        }

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
    BackendLLVMDuplicateSymbol name ->
      "Duplicate backend LLVM symbol: " ++ show name
    BackendLLVMInternalError detail ->
      "Internal backend LLVM error: " ++ detail
