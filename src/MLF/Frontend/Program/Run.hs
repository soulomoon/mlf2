{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Run
  ( Value (..),
    ProgramRunResult (..),
    runProgram,
    runProgramPackage,
    runLocatedProgram,
    runLocatedProgramPackage,
    runProgramOutput,
    runProgramPackageOutput,
    runLocatedProgramOutput,
    runLocatedProgramPackageOutput,
    runLocatedProgramPackageOutputWithTiming,
    runCheckedProgramOutput,
    programRunOutput,
    prettyValue,
  )
where

import Control.Exception (evaluate)
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM)
import Data.Foldable (toList)
import Data.List (elemIndex, find, findIndex, intercalate, isInfixOf, isPrefixOf, isSuffixOf, stripPrefix, tails)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Elab.Pipeline (XmlfTerm (..), Pretty (..), Ty (TForallRef), normalize, schemeFromType, typeCheck)
import MLF.Elab.Types (ElabType, ResolvedTermIdentityKey, ResolvedVar (..), deferredResolvedVarRef, resolvedVarBindingSymbolIdentity, resolvedVarBoundBy, resolvedVarConstructorRef, resolvedVarIdentityKey, resolvedVarReferenceName, resolvedVarSameIdentity)
import qualified MLF.Elab.Types as X
import MLF.Frontend.Program.Check (checkLocatedProgram, checkLocatedProgramPackage, checkLocatedProgramPackageWithTiming, checkProgram, checkProgramPackage)
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    classInfoForConstraint,
    diagnosticTypeViewDisplay,
    elaborateScopeDataTypesByIdentity,
    lookupEvidenceMethodByClass,
    lookupEvidenceMethodByClassTypes,
    matchMethodTypeViews,
    matchTypeViewsAgainstIdentity,
    mkElaborateScope,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    sourceTypeViewInScope,
    zeroMethodConstraintCoveredByEvidenceInfo,
  )
import MLF.Frontend.Program.Finalize (recoverSourceType, typeViewToElabType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Package
  ( LocatedProgramPackage,
    ProgramPackage,
    locatedProgramPackageProgram,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    ClassInfo (..),
    CheckedModule (..),
    CheckedProgram (..),
    ConstraintInfo (..),
    ConstructorForallBinder (..),
    ConstructorInfo (..),
    DataInfo (..),
    DeferredCaseCall (..),
    DeferredConstructorCall (..),
    DeferredMethodCall (..),
    DeferredMethodEvidence (..),
    DeferredProgramObligation (..),
    EvidenceMethod (..),
    EvidenceInfo (..),
    InstanceInfo (..),
    MethodInfo (..),
    ProgramDiagnostic,
    ProgramError (..),
    SymbolIdentity,
    SymbolNamespace (..),
    symbolDefiningName,
    symbolNamespace,
    TypeBinderSubst,
    TypeView (..),
    TypeViewSubst,
    ValueInfo (..),
    applyConstraintInfoSubst,
    applyTypeViewSubst,
    checkedBindingSourceType,
    ctorName,
    ctorArgs,
    classInfoIdentityQualifiedName,
    dataParamBinders,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    deferredMethodName,
    diagnosticForProgramError,
    emptyTypeBinderSubst,
    filterHeadIdentitiesByNames,
    freeTypeBinderIdentitiesTypeViews,
    freeTypeVarsTypeView,
    lookupInstanceMethod,
    lookupMethodParamViewSubst,
    methodType,
    methodTypeView,
    methodResultTypeView,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    methodParamBinderIdentities,
    mergeSymbolIdentityMaps,
    mergeTypeBinderIdentityMaps,
    specializeMethodTypeView,
    splitArrows,
    splitForalls,
    typeBinderAliasIdentityMap,
    typeHeadNamesSrcType,
    typeViewVarPairs,
    typeViewBinderIdentityForAlias,
    typeViewHeadPairs,
    typeViewHeadIdentityForAlias,
    typeViewSubstFromParamIdentities,
    typeViewsIdentity,
    uniqueEvidenceMethod,
    uniqueEvidenceMethodMatch,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToTypeViewSubstWith,
    insertTypeBinderSubstWithIdentity,
    ordinaryValueTypeView,
    resolvedVarFromValueInfo,
  )
import MLF.Frontend.Symbol (symbolIdentityStableName)
import MLF.Frontend.Syntax (Lit (..), SrcBound (..), SrcTy (..), SrcType)
import qualified MLF.Frontend.Syntax.Program as ProgramSyntax
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType)
import MLF.Types.Identity (constructorRefSymbol, DeferredRef, EnvRef, IdDetails (..), LocalRef, PrimitiveRef, primitiveRefSymbol, TypeBinderIdentity)
import MLF.Util.Timing (TimingConfig, timeProgramIO)

data Value
  = VLit Lit
  | VData String [Value]
  | VTerm XmlfTerm
  deriving (Eq, Show)

data ProgramRunResult = ProgramRunResult
  { programRunStdout :: String,
    programRunValue :: Maybe Value
  }
  deriving (Eq, Show)

runProgram :: ProgramSyntax.Program -> Either ProgramError Value
runProgram program = do
  checked <- checkProgram program
  runCheckedPureProgram checked

runProgramPackage :: ProgramPackage -> Either ProgramError Value
runProgramPackage package = do
  checked <- checkProgramPackage package
  runCheckedPureProgram checked

runLocatedProgram :: ProgramSyntax.LocatedProgram -> Either ProgramDiagnostic Value
runLocatedProgram located = do
  checked <- checkLocatedProgram located
  case runCheckedPureProgram checked of
    Left err -> Left (diagnosticForProgramError (Just located) err)
    Right value -> pure value

runLocatedProgramPackage :: LocatedProgramPackage -> Either ProgramDiagnostic Value
runLocatedProgramPackage package = do
  checked <- checkLocatedProgramPackage package
  case runCheckedPureProgram checked of
    Left err -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
    Right value -> pure value

runProgramOutput :: ProgramSyntax.Program -> Either ProgramError ProgramRunResult
runProgramOutput program = do
  checked <- checkProgram program
  runCheckedProgramOutput checked

runProgramPackageOutput :: ProgramPackage -> Either ProgramError ProgramRunResult
runProgramPackageOutput package = do
  checked <- checkProgramPackage package
  runCheckedProgramOutput checked

runLocatedProgramOutput :: ProgramSyntax.LocatedProgram -> Either ProgramDiagnostic ProgramRunResult
runLocatedProgramOutput located = do
  checked <- checkLocatedProgram located
  case runCheckedProgramOutput checked of
    Left err -> Left (diagnosticForProgramError (Just located) err)
    Right result -> pure result

runLocatedProgramPackageOutput :: LocatedProgramPackage -> Either ProgramDiagnostic ProgramRunResult
runLocatedProgramPackageOutput package = do
  checked <- checkLocatedProgramPackage package
  case runCheckedProgramOutput checked of
    Left err -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
    Right result -> pure result

runLocatedProgramPackageOutputWithTiming :: TimingConfig -> LocatedProgramPackage -> IO (Either ProgramDiagnostic ProgramRunResult)
runLocatedProgramPackageOutputWithTiming timing package = do
  checkedResult <- checkLocatedProgramPackageWithTiming timing package
  case checkedResult of
    Left diagnostic ->
      pure (Left diagnostic)
    Right checked -> do
      outputResult <-
        timeProgramIO
          timing
          "program.run.output"
          (evaluate (runCheckedProgramOutput checked))
      pure $
        case outputResult of
          Left err -> Left (diagnosticForProgramError (Just (locatedProgramPackageProgram package)) err)
          Right result -> Right result

programRunOutput :: ProgramRunResult -> String
programRunOutput result =
  programRunStdout result
    ++ maybe "" ((++ "\n") . prettyValue) (programRunValue result)

runCheckedPureProgram :: CheckedProgram -> Either ProgramError Value
runCheckedPureProgram checked = do
  context <- mkRuntimeContext checked
  case classifyMainMode context checked of
    MainPure -> do
      runCheckedPureMain context checked
    MainIOUnit ->
      Left (ProgramPipelineError "runProgram value API does not return IO main output")
    MainIOOther ty ->
      Left (ProgramPipelineError ("runProgram value API does not return IO main output: " ++ show ty))
    MainUnsupportedIO ty ->
      Left (unsupportedIOMainError ty)

runCheckedPureMain :: RuntimeContext -> CheckedProgram -> Either ProgramError Value
runCheckedPureMain context checked = do
  rejectOpaqueDependencies checked
  let normalizedTerm = normalizeProgramTerm (programMainTerm checked)
  if termMentionsRuntimePurePrimitive normalizedTerm
    then runtimeValueToValue <$> mainRuntimeValue context checked
    else pure (toValueWithProgram context checked normalizedTerm)

runCheckedProgramOutput :: CheckedProgram -> Either ProgramError ProgramRunResult
runCheckedProgramOutput checked = do
  context <- mkRuntimeContext checked
  case classifyMainMode context checked of
    MainPure -> do
      value <- runCheckedPureMain context checked
      pure
        ProgramRunResult
          { programRunStdout = "",
            programRunValue = Just value
          }
    MainIOUnit -> do
      action <- mainIOAction context checked
      (stdoutText, result) <- executeIOAction context action
      if isRuntimeUnit context result
        then
          pure
            ProgramRunResult
              { programRunStdout = stdoutText,
                programRunValue = Nothing
              }
        else Left (ProgramPipelineError "run-program IO main did not finish with Unit")
    MainIOOther _ty -> do
      action <- mainIOAction context checked
      (stdoutText, result) <- executeIOAction context action
      pure
        ProgramRunResult
          { programRunStdout = stdoutText,
            programRunValue = Just (runtimeValueToValue result)
          }
    MainUnsupportedIO ty ->
      Left (unsupportedIOMainError ty)

data MainMode
  = MainPure
  | MainIOUnit
  | MainIOOther SrcType
  | MainUnsupportedIO SrcType
  deriving (Eq, Show)

classifyMainMode :: RuntimeContext -> CheckedProgram -> MainMode
classifyMainMode context checked =
  case mainBinding checked of
    Just binding
      | isIOUnitElabType context (checkedBindingType binding) -> MainIOUnit
      | isIOElabType (checkedBindingType binding) -> MainIOOther (recoverMainSourceType checked displaySourceTy)
      | checkedBindingMentionsOpaqueBuiltin binding -> MainUnsupportedIO (recoverMainSourceType checked displaySourceTy)
      where
        displaySourceTy = checkedBindingSourceType binding
    _ -> MainPure

isIOElabType :: ElabType -> Bool
isIOElabType ty =
  case ty of
    X.TConWithIdentity (Just identity) _ _ ->
      identity == Builtins.builtinTypeIdentity "IO"
    _ -> False

isIOUnitElabType :: RuntimeContext -> ElabType -> Bool
isIOUnitElabType context ty =
  case ty of
    X.TConWithIdentity (Just identity) _ args
      | identity == Builtins.builtinTypeIdentity "IO" ->
          case toList args of
            [arg] -> isPreludeUnitElabType context arg
            _ -> False
    _ -> False

isPreludeUnitElabType :: RuntimeContext -> ElabType -> Bool
isPreludeUnitElabType context ty =
  hasPreludeUnitIdentityHead
    || maybe False matchesExpandedPreludeUnit (preludeUnitElabType context)
  where
    hasPreludeUnitIdentityHead =
      case ty of
        X.TBaseWithIdentity (Just identity) _ ->
          isPreludeUnitIdentity context identity
        X.TConWithIdentity (Just identity) _ args ->
          null (toList args) && isPreludeUnitIdentity context identity
        _ -> False

    matchesExpandedPreludeUnit unitTy =
      ty == unitTy || alphaEqType ty unitTy || churchAwareEqType ty unitTy

isPreludeUnitIdentity :: RuntimeContext -> SymbolIdentity -> Bool
isPreludeUnitIdentity context identity =
  maybe False ((== identity) . dataInfoSymbol) (preludeUnitDataInfo context)

preludeUnitElabType :: RuntimeContext -> Maybe ElabType
preludeUnitElabType context = do
  view <- preludeUnitTypeView context
  either
    (const Nothing)
    Just
    (typeViewToElabType (runtimeElaborateScope context) view)

preludeUnitTypeView :: RuntimeContext -> Maybe TypeView
preludeUnitTypeView context = do
  dataInfo <- preludeUnitDataInfo context
  let displayHeadName = dataInfoIdentityName dataInfo
      identityHeadName = symbolIdentityStableName (dataInfoSymbol dataInfo)
  pure
    TypeView
      { typeViewDisplay = STBase displayHeadName,
        typeViewIdentity = STBase identityHeadName,
        typeViewHeadIdentities =
          Map.fromList
            [ (displayHeadName, dataInfoSymbol dataInfo),
              (identityHeadName, dataInfoSymbol dataInfo)
            ],
        typeViewBinderIdentities = Map.empty
      }

preludeUnitDataInfo :: RuntimeContext -> Maybe DataInfo
preludeUnitDataInfo context = do
  ctor <- lookupPreludeUnitConstructor context
  Map.lookup (ctorOwningTypeIdentity ctor) (runtimeDataByIdentity context)

unsupportedIOMainError :: SrcType -> ProgramError
unsupportedIOMainError ty =
  ProgramPipelineError ("run-program supports only main : IO Unit, got " ++ show ty)

programMainTerm :: CheckedProgram -> XmlfTerm
programMainTerm checked =
  foldr bindAll (EVarNode (checkedProgramMainResolvedVar checked)) (reachableRuntimeBindings checked)
  where
    bindAll binding body =
      ELet
        (checkedBindingResolvedVar binding)
        (schemeFromType (checkedBindingType binding))
        (checkedBindingTerm binding)
        body

rejectOpaqueDependencies :: CheckedProgram -> Either ProgramError ()
rejectOpaqueDependencies checked =
  case reachableOpaqueRuntimeDependencies checked of
    [] ->
      Right ()
    dependencies ->
      Left
        ( ProgramPipelineError
            ( "run-program does not support IO dependencies yet: "
                ++ intercalate ", " dependencies
            )
        )

data RuntimeContext = RuntimeContext
  { runtimeBindingsByIdentity :: Map.Map SymbolIdentity CheckedBinding,
    runtimeDataByIdentity :: Map.Map SymbolIdentity DataInfo,
    runtimeDataInfos :: [DataInfo],
    runtimeConstructorsByIdentity :: Map.Map SymbolIdentity ConstructorInfo,
    runtimePreludeConstructorsByKey :: Map.Map PreludeConstructorKey ConstructorInfo,
    runtimePreludeBindingsByKey :: Map.Map PreludeBindingKey CheckedBinding,
    runtimeElaborateScope :: ElaborateScope
  }

data PreludeBindingKey
  = PreludeStringFromList
  deriving (Eq, Ord, Show)

preludeBindingLabel :: PreludeBindingKey -> String
preludeBindingLabel key =
  case key of
    PreludeStringFromList -> "stringFromList"

data PreludeConstructorKey
  = PreludeUnitUnit
  | PreludeNatZero
  | PreludeNatSucc
  | PreludeOptionNone
  | PreludeOptionSome
  | PreludeListNil
  | PreludeListCons
  deriving (Eq, Ord, Show)

type RuntimeDeferredValues = Map.Map DeferredRef RuntimeDeferredValue

type RuntimeEnv = Map.Map RuntimeVarKey RuntimeValue

type RuntimeLookupStack = [RuntimeLookupFrame]

data RuntimeLookupFrame = RuntimeLookupFrame
  { runtimeLookupFrameKey :: RuntimeVarKey,
    runtimeLookupFrameName :: String
  }

data RuntimeVarKey
  = RuntimeLocalKey LocalRef
  | RuntimeEnvKey EnvRef
  | RuntimeTopLevelKey SymbolIdentity
  | RuntimeConstructorKey SymbolIdentity
  | RuntimeMethodKey SymbolIdentity
  | RuntimePrimitiveKey PrimitiveRef
  | RuntimeDeferredKey DeferredRef
  deriving (Eq, Ord, Show)

data RuntimeDeferredValue
  = RuntimeDeferredConstructor DeferredConstructorCall
  | RuntimeDeferredCase DeferredCaseCall
  | RuntimeDeferredMethod DeferredMethodCall

data RuntimeConstructorSpec = RuntimeConstructorSpec
  { runtimeConstructorInfo :: ConstructorInfo,
    runtimeConstructorDeferred :: Maybe DeferredConstructorCall
  }

data RuntimeValue
  = RuntimeLit Lit
  | RuntimeUnit
  | RuntimeData ConstructorInfo TypeView [RuntimeValue]
  | RuntimeClosure ResolvedVar XmlfTerm RuntimeEnv RuntimeLookupStack RuntimeDeferredValues
  | RuntimeUnrolled RuntimeValue
  | RuntimeDataEliminator ConstructorInfo [RuntimeValue] [RuntimeValue]
  | RuntimeConstructor RuntimeConstructorSpec [RuntimeValue]
  | RuntimeCase DeferredCaseCall [RuntimeValue]
  | RuntimeMethod RuntimeLookupStack RuntimeDeferredValues RuntimeEnv DeferredMethodCall [RuntimeValue]
  | RuntimePrimitive RuntimePrimitive [RuntimeValue]
  | RuntimeIO RuntimeIOAction

data RuntimePrimitive
  = RuntimeIOPure
  | RuntimeIOBind
  | RuntimeIOMap
  | RuntimeIOAp
  | RuntimeIOPutStrLn
  | RuntimeIOGetLine
  | RuntimeIOPutStr
  | RuntimeIOReadFile
  | RuntimeIOWriteFile
  | RuntimeIOAppendFile
  | RuntimeIOExitWith
  | RuntimeIONewIORef
  | RuntimeIOReadIORef
  | RuntimeIOWriteIORef
  | RuntimeIOGetArgs
  | RuntimeAnd
  | RuntimeStringLength
  | RuntimeStringIsEmpty
  | RuntimeStringContainsChar
  | RuntimeStringContains
  | RuntimeStringEquals
  | RuntimeStringStartsWith
  | RuntimeStringEndsWith
  | RuntimeStringAppend
  | RuntimeStringReplaceChar
  | RuntimeStringReplace
  | RuntimeStringIndexOfChar
  | RuntimeStringIndexOf
  | RuntimeStringSplit
  | RuntimeStringJoin
  | RuntimeStringSplitChar
  | RuntimeStringCompare
  | RuntimeStringFromChar
  | RuntimeStringFromInt
  | RuntimeStringFromBool
  | RuntimeStringFromNat
  | RuntimePreludeStringFromList
  | RuntimeStringToList
  | RuntimeStringDrop
  | RuntimeStringTake
  | RuntimeStringSlice
  | RuntimeStringCharAt
  | RuntimeStringCharAtOption
  | RuntimeCharIsDigit
  | RuntimeCharIsAsciiLower
  | RuntimeCharIsAsciiUpper
  | RuntimeCharIsAsciiAlpha
  | RuntimeCharIsAsciiAlphaNum
  | RuntimeCharIsAsciiIdentifierStart
  | RuntimeCharIsAsciiIdentifierContinue
  | RuntimeCharIsAsciiWhitespace
  | RuntimeCharIsAsciiPunctuation
  | RuntimeCharIsAsciiPrintable
  | RuntimeCharIsAsciiHexDigit
  | RuntimeCharIsAsciiLineBreak
  | RuntimeCharIsAsciiControl
  | RuntimeCharToAsciiLower
  | RuntimeCharToAsciiUpper
  | RuntimeStringToAsciiLower
  | RuntimeStringToAsciiUpper
  deriving (Eq, Show)

data RuntimeIOAction
  = RuntimePure RuntimeValue
  | RuntimeBind RuntimeIOAction RuntimeValue
  | RuntimeMap RuntimeValue RuntimeIOAction
  | RuntimeAp RuntimeIOAction RuntimeIOAction
  | RuntimePutStrLn String
  | RuntimeGetLine
  | RuntimePutStr String
  | RuntimeReadFile String
  | RuntimeWriteFile String String
  | RuntimeAppendFile String String
  | RuntimeExitWith Integer
  | RuntimeNewIORef RuntimeValue
  | RuntimeReadIORef RuntimeValue
  | RuntimeWriteIORef RuntimeValue RuntimeValue
  | RuntimeGetArgs

mainIOAction :: RuntimeContext -> CheckedProgram -> Either ProgramError RuntimeIOAction
mainIOAction context checked = do
  binding <-
    case lookupRuntimeBindingResolved context (checkedProgramMainResolvedVar checked) of
      Just found -> Right found
      Nothing -> Left ProgramMainNotFound
  value <- evalRuntimeBinding context [] binding
  case value of
    RuntimeIO action -> Right action
    _ -> Left (ProgramPipelineError "run-program IO main did not evaluate to an IO action")

mainRuntimeValue :: RuntimeContext -> CheckedProgram -> Either ProgramError RuntimeValue
mainRuntimeValue context checked = do
  binding <-
    case lookupRuntimeBindingResolved context (checkedProgramMainResolvedVar checked) of
      Just found -> Right found
      Nothing -> Left ProgramMainNotFound
  evalRuntimeBinding context [] binding

mkRuntimeContext :: CheckedProgram -> Either ProgramError RuntimeContext
mkRuntimeContext checked = do
  let dataInfos = allDataInfos checked
  _modulesByIdentity <-
    uniqueRuntimeInfoByIdentity
      "module"
      [(checkedModuleIdentity checkedModule, checkedModule) | checkedModule <- checkedProgramModules checked]
  bindingsByIdentity <-
    uniqueRuntimeInfoByIdentity
      "binding"
      [ (symbol, binding)
      | binding <- allCheckedBindings checked,
        Just symbol <- [resolvedVarBindingSymbolIdentity (checkedBindingResolvedVar binding)]
      ]
  dataByIdentity <-
    uniqueRuntimeInfoByIdentity
      "data"
      [(dataInfoSymbol dataInfo, dataInfo) | dataInfo <- dataInfos]
  constructorsByIdentity <-
    uniqueRuntimeInfoByIdentity
      "constructor"
      [(ctorInfoSymbol ctor, ctor) | dataInfo <- dataInfos, ctor <- dataConstructors dataInfo]
  preludeConstructorsByKey <-
    uniqueRuntimeInfoByKey
      "Prelude constructor key"
      preludeConstructorLabel
      [ (key, ctor)
      | checkedModule <- checkedProgramModules checked,
        isRuntimePreludeModule checkedModule,
        dataInfo <- Map.elems (checkedModuleData checkedModule),
        ctor <- dataConstructors dataInfo,
        Just key <- [preludeConstructorKey checkedModule dataInfo ctor]
      ]
  preludeBindingsByKey <-
    uniqueRuntimeInfoByKey
      "Prelude binding key"
      preludeBindingLabel
      [ (key, binding)
      | checkedModule <- checkedProgramModules checked,
        isRuntimePreludeModule checkedModule,
        binding <- checkedModuleBindings checkedModule,
        Just key <- [preludeBindingKey binding]
      ]
  Right
    RuntimeContext
      { runtimeBindingsByIdentity = bindingsByIdentity,
        runtimeDataByIdentity = dataByIdentity,
        runtimeDataInfos = dataInfos,
        runtimeConstructorsByIdentity = constructorsByIdentity,
        runtimePreludeConstructorsByKey = preludeConstructorsByKey,
        runtimePreludeBindingsByKey = preludeBindingsByKey,
        runtimeElaborateScope = programElaborateScope checked
      }

uniqueRuntimeInfoByIdentity :: String -> [(SymbolIdentity, a)] -> Either ProgramError (Map.Map SymbolIdentity a)
uniqueRuntimeInfoByIdentity label =
  go Map.empty
  where
    go entries [] = Right entries
    go entries ((identity, info) : rest)
      | Map.member identity entries =
          Left (ProgramPipelineError ("run-program duplicate checked " ++ label ++ " identity: " ++ symbolIdentityStableName identity))
      | otherwise =
          go (Map.insert identity info entries) rest

uniqueRuntimeInfoByKey :: (Ord key) => String -> (key -> String) -> [(key, a)] -> Either ProgramError (Map.Map key a)
uniqueRuntimeInfoByKey label keyName =
  go Map.empty
  where
    go entries [] = Right entries
    go entries ((key, info) : rest)
      | Map.member key entries =
          Left (ProgramPipelineError ("run-program duplicate checked " ++ label ++ ": " ++ keyName key))
      | otherwise =
          go (Map.insert key info entries) rest

isRuntimePreludeModule :: CheckedModule -> Bool
isRuntimePreludeModule checkedModule =
  isRuntimePreludeModuleIdentity (checkedModuleIdentity checkedModule)

isRuntimePreludeModuleIdentity :: SymbolIdentity -> Bool
isRuntimePreludeModuleIdentity identity =
  symbolNamespace identity == SymbolModule
    && symbolDefiningName identity == "Prelude"

preludeBindingKey :: CheckedBinding -> Maybe PreludeBindingKey
preludeBindingKey binding =
  case resolvedVarBindingSymbolIdentity (checkedBindingResolvedVar binding) of
    Just symbol
      | symbol == Builtins.builtinValueIdentity PrimitiveInventory.stringFromListPrimitiveName -> Just PreludeStringFromList
    _ -> Nothing

preludeConstructorKey :: CheckedModule -> DataInfo -> ConstructorInfo -> Maybe PreludeConstructorKey
preludeConstructorKey checkedModule dataInfo ctor =
  if ctorOwningTypeIdentity ctor == dataInfoSymbol dataInfo
    then case (preludeDataNameByIdentity checkedModule (dataInfoSymbol dataInfo), ctorIndex ctor) of
      (Just "Unit", 0) -> Just PreludeUnitUnit
      (Just "Nat", 0) -> Just PreludeNatZero
      (Just "Nat", 1) -> Just PreludeNatSucc
      (Just "Option", 0) -> Just PreludeOptionNone
      (Just "Option", 1) -> Just PreludeOptionSome
      (Just "List", 0) -> Just PreludeListNil
      (Just "List", 1) -> Just PreludeListCons
      _ -> Nothing
    else Nothing

preludeDataNameByIdentity :: CheckedModule -> SymbolIdentity -> Maybe String
preludeDataNameByIdentity checkedModule identity =
  case [symbolDefiningName storedIdentity | storedIdentity <- Map.keys (checkedModuleData checkedModule), storedIdentity == identity] of
    name : _ -> Just name
    [] -> Nothing

preludeConstructorLabel :: PreludeConstructorKey -> String
preludeConstructorLabel key =
  case key of
    PreludeUnitUnit -> "Unit.Unit"
    PreludeNatZero -> "Nat.Zero"
    PreludeNatSucc -> "Nat.Succ"
    PreludeOptionNone -> "Option.None"
    PreludeOptionSome -> "Option.Some"
    PreludeListNil -> "List.Nil"
    PreludeListCons -> "List.Cons"

evalRuntimeBinding :: RuntimeContext -> RuntimeLookupStack -> CheckedBinding -> Either ProgramError RuntimeValue
evalRuntimeBinding context stack binding =
  evalRuntimeTermWithStack
    context
    stack
    (bindingDeferredValues binding)
    Map.empty
    (checkedBindingTerm binding)

bindingDeferredValues :: CheckedBinding -> RuntimeDeferredValues
bindingDeferredValues binding =
  Map.fromList
    [ (runtimeDeferredValueRef value, value)
    | obligation <- Map.elems (checkedBindingDeferredObligations binding),
      let value = runtimeDeferredValue obligation
    ]

runtimeDeferredValue :: DeferredProgramObligation -> RuntimeDeferredValue
runtimeDeferredValue obligation =
  case obligation of
    DeferredConstructor deferred -> RuntimeDeferredConstructor deferred
    DeferredCase deferred -> RuntimeDeferredCase deferred
    DeferredMethod deferred -> RuntimeDeferredMethod deferred

runtimeDeferredValueRef :: RuntimeDeferredValue -> DeferredRef
runtimeDeferredValueRef value =
  case value of
    RuntimeDeferredConstructor deferred -> deferredConstructorRef deferred
    RuntimeDeferredCase deferred -> deferredCaseRef deferred
    RuntimeDeferredMethod deferred -> deferredMethodRef deferred

evalRuntimeTermWithStack ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  XmlfTerm ->
  Either ProgramError RuntimeValue
evalRuntimeTermWithStack context stack deferredValues env term =
  case term of
    EVarNode resolved -> lookupRuntimeResolvedValue context stack deferredValues env resolved
    ELit lit -> Right (RuntimeLit lit)
    -- Top-level recursion through a lambda body is a delayed call, not a
    -- recursive value lookup while the binding RHS is being forced.
    ELam resolved body ->
      Right (RuntimeClosure resolved body env [] deferredValues)
    EApp fun arg -> do
      funValue <- evalRuntimeTermWithStack context stack deferredValues env fun
      argValue <- evalRuntimeTermWithStack context stack deferredValues env arg
      applyRuntimeValue context funValue argValue
    ELet resolved _ rhs body -> do
      rhsValue <- evalRuntimeTermWithStack context stack deferredValues env rhs
      evalRuntimeTermWithStack context stack deferredValues (insertRuntimeEnv resolved rhsValue env) body
    ETyAbsRef _ _ body ->
      evalRuntimeTermWithStack context stack deferredValues env body
    ETyInst inner _ ->
      evalRuntimeTermWithStack context stack deferredValues env inner
    ERoll _ body ->
      evalRuntimeTermWithStack context stack deferredValues env body
    EUnroll body -> do
      value <- evalRuntimeTermWithStack context stack deferredValues env body
      case value of
        RuntimeData {} -> Right (RuntimeUnrolled value)
        RuntimeUnit -> Right (RuntimeUnrolled value)
        _ -> Right value

lookupRuntimeResolvedValue ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  ResolvedVar ->
  Either ProgramError RuntimeValue
lookupRuntimeResolvedValue context stack deferredValues env resolved =
  case lookupRuntimeEnvResolved resolved env of
    Just value -> Right value
    Nothing ->
      case runtimePrimitiveResolved resolved of
        Just prim -> Right (RuntimePrimitive prim [])
        Nothing
          | resolvedVarMatchesPreludeStringFromList context resolved -> Right (RuntimePrimitive RuntimePreludeStringFromList [])
        Nothing
          | Just deferred <- lookupRuntimeDeferredResolved resolved deferredValues ->
              lookupRuntimeDeferredValue context stack deferredValues env deferred
          | Just ctor <- lookupRuntimeConstructorResolved context resolved ->
              runtimeConstructorValue context (RuntimeConstructorSpec ctor Nothing) []
          | Just binding <- lookupRuntimeBindingResolved context resolved ->
              evalRuntimeBindingByIdentity context stack binding
          | otherwise -> Left (ProgramUnknownValue name)
  where
    name = resolvedVarReferenceName resolved

lookupRuntimeEnvResolved :: ResolvedVar -> RuntimeEnv -> Maybe RuntimeValue
lookupRuntimeEnvResolved resolved env =
  Map.lookup (runtimeVarKey resolved) env

lookupRuntimeDeferredResolved :: ResolvedVar -> RuntimeDeferredValues -> Maybe RuntimeDeferredValue
lookupRuntimeDeferredResolved resolved deferredValues =
  deferredResolvedVarRef resolved >>= (`Map.lookup` deferredValues)

insertRuntimeEnv :: ResolvedVar -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
insertRuntimeEnv resolved value =
  Map.insert (runtimeVarKey resolved) value

runtimeVarKey :: ResolvedVar -> RuntimeVarKey
runtimeVarKey resolved =
  case resolvedVarDetails resolved of
    LocalId ref -> RuntimeLocalKey ref
    EvidenceId ref -> RuntimeLocalKey ref
    EnvId ref -> RuntimeEnvKey ref
    TopLevelId identity -> RuntimeTopLevelKey identity
    ConstructorId ref -> RuntimeConstructorKey (constructorRefSymbol ref)
    MethodId identity -> RuntimeMethodKey identity
    PrimitiveId ref -> RuntimePrimitiveKey ref
    DeferredId ref -> RuntimeDeferredKey ref

lookupRuntimeConstructorResolved :: RuntimeContext -> ResolvedVar -> Maybe ConstructorInfo
lookupRuntimeConstructorResolved context resolved =
  case resolvedVarConstructorRef resolved of
    Just ref -> Map.lookup (constructorRefSymbol ref) (runtimeConstructorsByIdentity context)
    Nothing -> Nothing

lookupRuntimeBindingResolved :: RuntimeContext -> ResolvedVar -> Maybe CheckedBinding
lookupRuntimeBindingResolved context resolved =
  resolvedVarBindingSymbolIdentity resolved >>= (`Map.lookup` runtimeBindingsByIdentity context)

evalRuntimeBindingByIdentity :: RuntimeContext -> RuntimeLookupStack -> CheckedBinding -> Either ProgramError RuntimeValue
evalRuntimeBindingByIdentity context stack binding
  | any ((== runtimeLookupFrameKey frame) . runtimeLookupFrameKey) stack =
      Left (recursiveRuntimeBindingError frame stack)
  | otherwise = evalRuntimeBinding context (frame : stack) binding
  where
    frame =
      RuntimeLookupFrame
        { runtimeLookupFrameKey = runtimeVarKey (checkedBindingResolvedVar binding),
          runtimeLookupFrameName = checkedBindingRuntimeName binding
        }

checkedBindingRuntimeName :: CheckedBinding -> String
checkedBindingRuntimeName =
  resolvedVarRuntimeName . checkedBindingResolvedVar

lookupRuntimeDeferredValue ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  RuntimeDeferredValue ->
  Either ProgramError RuntimeValue
lookupRuntimeDeferredValue context stack deferredValues env deferred =
  case deferred of
    RuntimeDeferredConstructor deferredConstructor ->
      runtimeConstructorValue
        context
        (RuntimeConstructorSpec (deferredConstructorInfo deferredConstructor) (Just deferredConstructor))
        []
    RuntimeDeferredCase deferredCase ->
      Right (RuntimeCase deferredCase [])
    RuntimeDeferredMethod deferredMethod ->
      resolveRuntimeMethod context stack deferredValues env deferredMethod []

recursiveRuntimeBindingError :: RuntimeLookupFrame -> RuntimeLookupStack -> ProgramError
recursiveRuntimeBindingError frame stack =
  ProgramPipelineError
    ( "run-program IO runtime encountered recursive top-level binding lookup: "
        ++ intercalate " -> " cyclePath
    )
  where
    cycleFrames =
      dropWhile
        ((/= runtimeLookupFrameKey frame) . runtimeLookupFrameKey)
        (reverse stack)
        ++ [frame]
    cyclePath = map runtimeLookupFrameName cycleFrames

runtimePrimitiveEntries :: [(String, RuntimePrimitive)]
runtimePrimitiveEntries =
  [ ("__io_pure", RuntimeIOPure),
    ("__io_bind", RuntimeIOBind),
    ("__io_map", RuntimeIOMap),
    ("__io_ap", RuntimeIOAp),
    ("__io_putStrLn", RuntimeIOPutStrLn),
    ("__io_getLine", RuntimeIOGetLine),
    ("__io_putStr", RuntimeIOPutStr),
    ("__io_readFile", RuntimeIOReadFile),
    ("__io_writeFile", RuntimeIOWriteFile),
    ("__io_appendFile", RuntimeIOAppendFile),
    ("__io_exitWith", RuntimeIOExitWith),
    ("__io_newIORef", RuntimeIONewIORef),
    ("__io_readIORef", RuntimeIOReadIORef),
    ("__io_writeIORef", RuntimeIOWriteIORef),
    ("__io_getArgs", RuntimeIOGetArgs),
    ("__mlfp_and", RuntimeAnd),
    (PrimitiveInventory.stringLengthPrimitiveName, RuntimeStringLength),
    (PrimitiveInventory.stringIsEmptyPrimitiveName, RuntimeStringIsEmpty),
    (PrimitiveInventory.stringContainsCharPrimitiveName, RuntimeStringContainsChar),
    (PrimitiveInventory.stringContainsPrimitiveName, RuntimeStringContains),
    (PrimitiveInventory.stringEqualsPrimitiveName, RuntimeStringEquals),
    (PrimitiveInventory.stringStartsWithPrimitiveName, RuntimeStringStartsWith),
    (PrimitiveInventory.stringEndsWithPrimitiveName, RuntimeStringEndsWith),
    (PrimitiveInventory.stringAppendPrimitiveName, RuntimeStringAppend),
    (PrimitiveInventory.stringReplaceCharPrimitiveName, RuntimeStringReplaceChar),
    (PrimitiveInventory.stringReplacePrimitiveName, RuntimeStringReplace),
    (PrimitiveInventory.stringIndexOfCharPrimitiveName, RuntimeStringIndexOfChar),
    (PrimitiveInventory.stringIndexOfPrimitiveName, RuntimeStringIndexOf),
    (PrimitiveInventory.stringSplitPrimitiveName, RuntimeStringSplit),
    (PrimitiveInventory.stringJoinPrimitiveName, RuntimeStringJoin),
    (PrimitiveInventory.stringSplitCharPrimitiveName, RuntimeStringSplitChar),
    (PrimitiveInventory.stringComparePrimitiveName, RuntimeStringCompare),
    (PrimitiveInventory.stringFromCharPrimitiveName, RuntimeStringFromChar),
    (PrimitiveInventory.stringFromIntPrimitiveName, RuntimeStringFromInt),
    (PrimitiveInventory.stringFromBoolPrimitiveName, RuntimeStringFromBool),
    (PrimitiveInventory.stringFromNatPrimitiveName, RuntimeStringFromNat),
    (PrimitiveInventory.stringFromListPrimitiveName, RuntimePreludeStringFromList),
    (PrimitiveInventory.stringToListPrimitiveName, RuntimeStringToList),
    (PrimitiveInventory.stringDropPrimitiveName, RuntimeStringDrop),
    (PrimitiveInventory.stringTakePrimitiveName, RuntimeStringTake),
    (PrimitiveInventory.stringSlicePrimitiveName, RuntimeStringSlice),
    (PrimitiveInventory.stringCharAtPrimitiveName, RuntimeStringCharAt),
    (PrimitiveInventory.stringCharAtOptionPrimitiveName, RuntimeStringCharAtOption),
    (PrimitiveInventory.charIsDigitPrimitiveName, RuntimeCharIsDigit),
    (PrimitiveInventory.charIsAsciiLowerPrimitiveName, RuntimeCharIsAsciiLower),
    (PrimitiveInventory.charIsAsciiUpperPrimitiveName, RuntimeCharIsAsciiUpper),
    (PrimitiveInventory.charIsAsciiAlphaPrimitiveName, RuntimeCharIsAsciiAlpha),
    (PrimitiveInventory.charIsAsciiAlphaNumPrimitiveName, RuntimeCharIsAsciiAlphaNum),
    (PrimitiveInventory.charIsAsciiIdentifierStartPrimitiveName, RuntimeCharIsAsciiIdentifierStart),
    (PrimitiveInventory.charIsAsciiIdentifierContinuePrimitiveName, RuntimeCharIsAsciiIdentifierContinue),
    (PrimitiveInventory.charIsAsciiWhitespacePrimitiveName, RuntimeCharIsAsciiWhitespace),
    (PrimitiveInventory.charIsAsciiPunctuationPrimitiveName, RuntimeCharIsAsciiPunctuation),
    (PrimitiveInventory.charIsAsciiPrintablePrimitiveName, RuntimeCharIsAsciiPrintable),
    (PrimitiveInventory.charIsAsciiHexDigitPrimitiveName, RuntimeCharIsAsciiHexDigit),
    (PrimitiveInventory.charIsAsciiLineBreakPrimitiveName, RuntimeCharIsAsciiLineBreak),
    (PrimitiveInventory.charIsAsciiControlPrimitiveName, RuntimeCharIsAsciiControl),
    (PrimitiveInventory.charToAsciiLowerPrimitiveName, RuntimeCharToAsciiLower),
    (PrimitiveInventory.charToAsciiUpperPrimitiveName, RuntimeCharToAsciiUpper),
    (PrimitiveInventory.stringToAsciiLowerPrimitiveName, RuntimeStringToAsciiLower),
    (PrimitiveInventory.stringToAsciiUpperPrimitiveName, RuntimeStringToAsciiUpper)
  ]

runtimePrimitiveResolved :: ResolvedVar -> Maybe RuntimePrimitive
runtimePrimitiveResolved resolved =
  runtimePrimitiveByIdentity =<< resolvedVarPrimitiveSymbol resolved

resolvedVarPrimitiveSymbol :: ResolvedVar -> Maybe SymbolIdentity
resolvedVarPrimitiveSymbol resolved =
  case resolvedVarDetails resolved of
    PrimitiveId ref ->
      Just (primitiveRefSymbol ref)
    TopLevelId symbol
      | Map.member symbol runtimePrimitivesByIdentity ->
          Just symbol
    _ ->
      Nothing

runtimePrimitiveByIdentity :: SymbolIdentity -> Maybe RuntimePrimitive
runtimePrimitiveByIdentity symbol =
  Map.lookup symbol runtimePrimitivesByIdentity

runtimePrimitivesByIdentity :: Map.Map SymbolIdentity RuntimePrimitive
runtimePrimitivesByIdentity =
  Map.fromList
    [ (Builtins.builtinValueIdentity name, primitive)
    | (name, primitive) <- runtimePrimitiveEntries
    ]

runtimePrimitiveNamesByIdentity :: Map.Map SymbolIdentity String
runtimePrimitiveNamesByIdentity =
  Map.fromList
    [ (Builtins.builtinValueIdentity name, name)
    | name <- Map.keys PrimitiveInventory.primitiveValueSpecs
    ]

runtimeConstructorValue :: RuntimeContext -> RuntimeConstructorSpec -> [RuntimeValue] -> Either ProgramError RuntimeValue
runtimeConstructorValue context spec args
  | isPreludeUnitConstructor context ctor && null args = Right RuntimeUnit
  | length args == length argViews = do
      resultView <- runtimeConstructorResultView context spec args
      Right (RuntimeData ctor resultView args)
  | length args < length argViews = Right (RuntimeConstructor spec args)
  | otherwise =
      Left (ProgramPipelineError ("run-program constructor over-applied: " ++ ctorName ctor))
  where
    ctor = runtimeConstructorInfo spec
    argViews = constructorInfoArgViews ctor

runtimeConstructorResultView :: RuntimeContext -> RuntimeConstructorSpec -> [RuntimeValue] -> Either ProgramError TypeView
runtimeConstructorResultView context spec args = do
  (substBinders, startSubst) <- runtimeConstructorSubstSeed context spec
  Right (applyRuntimeConstructorSubstView scope (subst substBinders startSubst) resultView)
  where
    scope = runtimeElaborateScope context
    ctor = runtimeConstructorInfo spec
    argViews = constructorInfoArgViews ctor
    resultView = runtimeConstructorOccurrenceResultView context spec
    subst substBinders startSubst
      | Set.null (freeTypeVarsTypeView resultViewFromDeferred) = startSubst
      | otherwise = foldl (refineFromRuntimeArg substBinders) startSubst (zip argViews args)
      where
        resultViewFromDeferred = applyRuntimeConstructorSubstView scope startSubst resultView

    refineFromRuntimeArg substBinders acc (templateView, arg) =
      case runtimeValueTypeView context arg of
        Left _ -> acc
        Right actualView ->
          case matchRuntimeTypeBinderSubstInScope scope substBinders acc templateView actualView of
            Just acc' -> acc'
            Nothing -> acc

runtimeConstructorSubstSeed :: RuntimeContext -> RuntimeConstructorSpec -> Either ProgramError ([(String, TypeBinderIdentity)], TypeBinderSubst)
runtimeConstructorSubstSeed context spec =
  case runtimeConstructorDeferred spec of
    Just deferred ->
      Right (deferredConstructorInstBinders deferred, deferredConstructorInitialSubst deferred)
    Nothing ->
      (\binders -> (binders, emptyTypeBinderSubst)) <$> runtimeConstructorBinders context ctor
  where
    ctor = runtimeConstructorInfo spec

runtimeConstructorBinders :: RuntimeContext -> ConstructorInfo -> Either ProgramError [(String, TypeBinderIdentity)]
runtimeConstructorBinders context ctor
  | null missingFreeBinders = Right binders
  | missing : _ <- missingFreeBinders =
      Left (ProgramPipelineError ("run-program constructor binder `" ++ missing ++ "` is missing identity"))
  | otherwise = Right binders
  where
    binders = explicitBinders ++ viewBinders ++ ownerBinders ++ pairedViewBinders
    explicitBinders =
      Map.toList $
        typeBinderAliasIdentityMap
          [ (constructorForallDisplayName binder, constructorForallIdentity binder)
          | binder <- ctorForallBinderInfo ctor
          ]
    viewBinders =
      Map.toList (typeViewBinderIdentities (ctorTypeView ctor))
    pairedViewBinders =
      [ (identityName, identity)
      | (identityName, _) <- Map.toList (typeViewVarPairs (ctorTypeView ctor)),
        Just identity <- [typeViewBinderIdentityForAlias ctorView identityName]
      ]
    ctorView =
      ctorTypeView ctor
    ownerBinders =
      case Map.lookup (ctorOwningTypeIdentity ctor) (elaborateScopeDataTypesByIdentity (runtimeElaborateScope context)) of
        Just dataInfo ->
          Map.toList (typeBinderAliasIdentityMap (dataParamBinders dataInfo))
        Nothing -> []
    binderNames = Set.fromList (map fst binders)
    missingFreeBinders =
      [ name
      | name <- Set.toList (freeTypeVarsTypeViewDisplayAndIdentity (ctorTypeView ctor)),
        name `Set.notMember` binderNames
      ]

matchRuntimeTypeBinderSubstInScope ::
  ElaborateScope ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  TypeView ->
  TypeView ->
  Maybe TypeBinderSubst
matchRuntimeTypeBinderSubstInScope scope binders subst templateView actualView =
  typeBinderSubstFromTypeViewSubst binders
    <$> matchTypeViewsAgainstIdentity
      scope
      (typeBinderSubstToTypeViewSubstWith (sourceTypeViewInScope scope) subst)
      (NE.singleton (typeBinderTemplateView binders templateView))
      (NE.singleton actualView)

typeBinderTemplateView :: [(String, TypeBinderIdentity)] -> TypeView -> TypeView
typeBinderTemplateView binders view =
  view
    { typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities view,
            typeBinderAliasIdentityMap binders
          ]
    }

applyRuntimeConstructorSubstView :: ElaborateScope -> TypeBinderSubst -> TypeView -> TypeView
applyRuntimeConstructorSubstView scope subst =
  applyTypeViewSubst (typeBinderSubstToTypeViewSubstWith (sourceTypeViewInScope scope) subst)

runtimeConstructorOccurrenceResultView :: RuntimeContext -> RuntimeConstructorSpec -> TypeView
runtimeConstructorOccurrenceResultView context spec =
  case runtimeConstructorDeferred spec of
    Just deferred ->
      occurrenceView
        { typeViewBinderIdentities =
            mergeTypeBinderIdentityMaps
              [ typeViewBinderIdentities occurrenceView,
                typeBinderAliasIdentityMap (deferredConstructorInstBinders deferred)
              ]
        }
      where
        occurrenceView =
          sourceTypeViewInScope (runtimeElaborateScope context) occurrenceType
        occurrenceType =
          dropSourceArrows
            (length (constructorInfoArgViews ctor) - deferredConstructorArgCount deferred)
            (deferredConstructorOccurrenceType deferred)
    Nothing -> constructorInfoResultView ctor
  where
    ctor = runtimeConstructorInfo spec

freeTypeVarsTypeViewDisplayAndIdentity :: TypeView -> Set.Set String
freeTypeVarsTypeViewDisplayAndIdentity view =
  freeTypeVarsRuntimeSrcType (typeViewDisplay view)
    <> freeTypeVarsRuntimeSrcType (typeViewIdentity view)

dropSourceArrows :: Int -> SrcType -> SrcType
dropSourceArrows count ty
  | count <= 0 = ty
dropSourceArrows count ty =
  case ty of
    STArrow _ resultTy -> dropSourceArrows (count - 1) resultTy
    _ -> ty

freeTypeVarsRuntimeSrcType :: SrcType -> Set.Set String
freeTypeVarsRuntimeSrcType =
  freeTypeVarsRuntimeSrcTy

freeTypeVarsRuntimeSrcTy :: SrcTy n v -> Set.Set String
freeTypeVarsRuntimeSrcTy = go Set.empty
  where
    go :: Set.Set String -> SrcTy n0 v0 -> Set.Set String
    go bound ty =
      case ty of
        STVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go bound dom `Set.union` go bound cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go bound) args
        STVarApp name args ->
          let headVars =
                if name `Set.member` bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STTyLam name body -> go (Set.insert name bound) body
        STTyApp fun arg -> go bound fun `Set.union` go bound arg
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body -> go (Set.insert name bound) body
        STBottom -> Set.empty

isPreludeUnitConstructor :: RuntimeContext -> ConstructorInfo -> Bool
isPreludeUnitConstructor context ctor =
  case lookupPreludeConstructor context PreludeUnitUnit of
    Just unitCtor -> sameRuntimeConstructor ctor unitCtor
    Nothing -> False

applyRuntimeValue :: RuntimeContext -> RuntimeValue -> RuntimeValue -> Either ProgramError RuntimeValue
applyRuntimeValue context funValue argValue =
  case funValue of
    RuntimeClosure resolved body closureEnv closureStack closureDeferredValues ->
      evalRuntimeTermWithStack context closureStack closureDeferredValues (insertRuntimeEnv resolved argValue closureEnv) body
    RuntimeUnrolled value ->
      applyRuntimeUnrolledData context value [argValue]
    RuntimeDataEliminator ctor fields handlers ->
      applyRuntimeDataEliminator context ctor fields (handlers ++ [argValue])
    RuntimePrimitive prim args ->
      applyRuntimePrimitive context prim (args ++ [argValue])
    RuntimeConstructor spec args
      | length args < length (ctorArgs (runtimeConstructorInfo spec)) ->
          runtimeConstructorValue context spec (args ++ [argValue])
    RuntimeCase deferred args ->
      applyRuntimeCase context deferred (args ++ [argValue])
    RuntimeMethod stack deferredValues env deferred args ->
      resolveRuntimeMethod context stack deferredValues env deferred (args ++ [argValue])
    _ -> Left (ProgramPipelineError "run-program IO interpreter expected a function")

applyRuntimeUnrolledData :: RuntimeContext -> RuntimeValue -> [RuntimeValue] -> Either ProgramError RuntimeValue
applyRuntimeUnrolledData context value handlers =
  case value of
    RuntimeData ctor _ fields ->
      applyRuntimeDataEliminator context ctor fields handlers
    RuntimeUnit ->
      case lookupPreludeUnitConstructor context of
        Just ctor -> applyRuntimeDataEliminator context ctor [] handlers
        Nothing -> Left (ProgramPipelineError "run-program could not recover Unit constructor metadata")
    _ -> Left (ProgramPipelineError "run-program expected an unrolled data value")

applyRuntimeDataEliminator :: RuntimeContext -> ConstructorInfo -> [RuntimeValue] -> [RuntimeValue] -> Either ProgramError RuntimeValue
applyRuntimeDataEliminator context ctor fields handlers
  | length handlers < expectedHandlers =
      Right (RuntimeDataEliminator ctor fields handlers)
  | otherwise =
      case drop (ctorIndex ctor) handlers of
        handler : _ -> foldM (applyRuntimeValue context) handler fields
        [] -> Left (ProgramPipelineError ("run-program constructor handler order missing `" ++ ctorName ctor ++ "`"))
  where
    expectedHandlers = length (ctorOwnerConstructors ctor)

applyRuntimeCase :: RuntimeContext -> DeferredCaseCall -> [RuntimeValue] -> Either ProgramError RuntimeValue
applyRuntimeCase context deferred args
  | length args < expectedArgCount = Right (RuntimeCase deferred args)
  | otherwise = do
      let (caseArgs, extraArgs) = splitAt expectedArgCount args
      result <- evaluateRuntimeCase context deferred caseArgs
      foldM (applyRuntimeValue context) result extraArgs
  where
    expectedArgCount = deferredCaseExpectedArgCount deferred

evaluateRuntimeCase :: RuntimeContext -> DeferredCaseCall -> [RuntimeValue] -> Either ProgramError RuntimeValue
evaluateRuntimeCase context deferred args =
  case args of
    scrutinee : handlers
      | length args == deferredCaseExpectedArgCount deferred -> do
          (ctor, fields) <- runtimeCaseScrutinee context deferred scrutinee
          handler <- runtimeCaseHandler deferred ctor handlers
          foldM (applyRuntimeValue context) handler fields
    _ ->
      Left (ProgramPipelineError "run-program deferred case received malformed arguments")

runtimeCaseScrutinee :: RuntimeContext -> DeferredCaseCall -> RuntimeValue -> Either ProgramError (ConstructorInfo, [RuntimeValue])
runtimeCaseScrutinee context deferred value =
  case value of
    RuntimeData ctor _ fields
      | constructorBelongsToCase deferred ctor -> Right (ctor, fields)
    RuntimeUnit ->
      case find (isPreludeUnitConstructor context) (dataConstructors (deferredCaseDataInfo deferred)) of
        Just ctor -> Right (ctor, [])
        Nothing -> Left (ProgramPipelineError "run-program deferred case expected a data constructor scrutinee")
    _ -> Left (ProgramPipelineError "run-program deferred case expected a data constructor scrutinee")

constructorBelongsToCase :: DeferredCaseCall -> ConstructorInfo -> Bool
constructorBelongsToCase deferred ctor =
  any (sameRuntimeConstructor ctor) (dataConstructors (deferredCaseDataInfo deferred))

sameRuntimeConstructor :: ConstructorInfo -> ConstructorInfo -> Bool
sameRuntimeConstructor left right =
  ctorInfoSymbol left == ctorInfoSymbol right

runtimeCaseHandler :: DeferredCaseCall -> ConstructorInfo -> [RuntimeValue] -> Either ProgramError RuntimeValue
runtimeCaseHandler deferred ctor handlers
  | length handlers /= length constructors =
      Left (ProgramPipelineError "run-program deferred case received the wrong number of handlers")
  | otherwise =
      case [handler | (ctorInfo, handler) <- zip constructors handlers, sameRuntimeConstructor ctor ctorInfo] of
        handler : _ -> Right handler
        [] -> Left (ProgramPipelineError "run-program deferred case constructor does not belong to its data type")
  where
    constructors = dataConstructors (deferredCaseDataInfo deferred)

resolveRuntimeMethod ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  [RuntimeValue] ->
  Either ProgramError RuntimeValue
resolveRuntimeMethod context stack deferredValues env deferred args
  | length args < requiredArgCount =
      Right (RuntimeMethod stack deferredValues env deferred args)
  | otherwise = do
      let (methodArgs, extraArgs) = splitAt requiredArgCount args
      methodValue <- resolveRuntimeMethodReady context stack deferredValues env deferred methodArgs
      foldM (applyRuntimeValue context) methodValue extraArgs
  where
    requiredArgCount = deferredMethodArgCount deferred

resolveRuntimeMethodReady ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  [RuntimeValue] ->
  Either ProgramError RuntimeValue
resolveRuntimeMethodReady context stack deferredValues env deferred args =
  if null args && deferredMethodArgCount deferred == 0
    then resolveRuntimeNullaryMethod context stack deferredValues env deferred
    else do
      argViews <- mapM (runtimeValueTypeView context) args
      classArgView <-
        case inferRuntimeMethodClassArgument context (deferredMethodInfo deferred) argViews (deferredMethodExpectedResult deferred) of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      case lookupRuntimeMethodEvidence context deferred classArgView of
        Just (evidence, evidenceSubst) ->
          resolveRuntimeEvidenceMethod context stack deferredValues env deferred classArgView argViews args evidence
            evidenceSubst
        Nothing ->
          resolveRuntimeInstanceMethod context stack deferredValues env deferred classArgView argViews args

resolveRuntimeEvidenceMethod ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  TypeView ->
  [TypeView] ->
  [RuntimeValue] ->
  DeferredMethodEvidence ->
  TypeViewSubst ->
  Either ProgramError RuntimeValue
resolveRuntimeEvidenceMethod context stack deferredValues env deferred classArgView argViews args evidence evidenceSubst = do
  methodSubst <-
    case inferRuntimeMethodArgumentSubst context (deferredMethodInfo deferred) classArgView Map.empty argViews of
      Just subst -> Right subst
      Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
  let methodSubst' = methodSubst `Map.union` evidenceSubst
  methodLocalConstraints <- runtimeMethodLocalConstraints (deferredMethodInfo deferred) classArgView methodSubst'
  evidenceArgs <-
    resolveRuntimeConstraintEvidenceValues
      context
      stack
      deferredValues
      env
      (deferredMethodLocalEvidence deferred)
      Set.empty
      methodLocalConstraints
  methodHead <- lookupRuntimeEvidenceMethodValue context stack deferredValues env (deferredMethodEvidenceMethod evidence)
  foldM (applyRuntimeValue context) methodHead (evidenceArgs ++ args)

resolveRuntimeInstanceMethod ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  TypeView ->
  [TypeView] ->
  [RuntimeValue] ->
  Either ProgramError RuntimeValue
resolveRuntimeInstanceMethod context stack deferredValues env deferred classArgView argViews args = do
  (instanceInfo, instanceSubst) <- resolveMethodInstanceInfoByTypeView (runtimeElaborateScope context) (deferredMethodInfo deferred) classArgView
  methodValueInfo <-
    case lookupInstanceMethod (deferredMethodInfo deferred) instanceInfo of
      Just valueInfo@OrdinaryValue {} -> Right valueInfo
      _ -> Left (ProgramUnknownMethod (deferredMethodName deferred))
  methodSubst <-
    case inferRuntimeMethodArgumentSubst context (deferredMethodInfo deferred) classArgView instanceSubst argViews of
      Just subst -> Right subst
      Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
  eagerConstraints <-
    filterRuntimeConstraintGround
      (map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValueInfo))
  evidenceArgs <-
    resolveRuntimeConstraintEvidenceValues
      context
      stack
      deferredValues
      env
      (deferredMethodLocalEvidence deferred)
      Set.empty
      eagerConstraints
  methodResolved <- runtimeMethodValueResolved context (deferredMethodName deferred) methodValueInfo
  methodHead <- lookupRuntimeResolvedValue context stack deferredValues env methodResolved
  foldM (applyRuntimeValue context) methodHead (evidenceArgs ++ args)

resolveRuntimeNullaryMethod ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  Either ProgramError RuntimeValue
resolveRuntimeNullaryMethod context stack deferredValues env deferred = do
  expectedView <-
    case deferredMethodExpectedResult deferred of
      Just view -> Right view
      Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
  classArgView <-
    case inferRuntimeNullaryMethodClassArgument context (deferredMethodInfo deferred) expectedView of
      Just view -> Right view
      Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
  case lookupRuntimeMethodEvidence context deferred classArgView of
    Just (evidence, evidenceSubst) ->
      resolveRuntimeNullaryEvidenceMethod context stack deferredValues env deferred classArgView expectedView evidence evidenceSubst
    Nothing ->
      resolveRuntimeNullaryInstanceMethod context stack deferredValues env deferred classArgView expectedView

resolveRuntimeNullaryEvidenceMethod ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  TypeView ->
  TypeView ->
  DeferredMethodEvidence ->
  TypeViewSubst ->
  Either ProgramError RuntimeValue
resolveRuntimeNullaryEvidenceMethod context stack deferredValues env deferred classArgView expectedView evidence evidenceSubst = do
  methodSubst <-
    case inferRuntimeNullaryMethodSubst context (deferredMethodInfo deferred) classArgView Map.empty expectedView of
      Just subst -> Right subst
      Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
  let methodSubst' = methodSubst `Map.union` evidenceSubst
  methodLocalConstraints <- runtimeMethodLocalConstraints (deferredMethodInfo deferred) classArgView methodSubst'
  evidenceArgs <-
    resolveRuntimeConstraintEvidenceValues
      context
      stack
      deferredValues
      env
      (deferredMethodLocalEvidence deferred)
      Set.empty
      methodLocalConstraints
  methodHead <- lookupRuntimeEvidenceMethodValue context stack deferredValues env (deferredMethodEvidenceMethod evidence)
  foldM (applyRuntimeValue context) methodHead evidenceArgs

resolveRuntimeNullaryInstanceMethod ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  DeferredMethodCall ->
  TypeView ->
  TypeView ->
  Either ProgramError RuntimeValue
resolveRuntimeNullaryInstanceMethod context stack deferredValues env deferred classArgView expectedView = do
  (instanceInfo, instanceSubst) <- resolveMethodInstanceInfoByTypeView (runtimeElaborateScope context) (deferredMethodInfo deferred) classArgView
  methodValueInfo <-
    case lookupInstanceMethod (deferredMethodInfo deferred) instanceInfo of
      Just valueInfo@OrdinaryValue {} -> Right valueInfo
      _ -> Left (ProgramUnknownMethod (deferredMethodName deferred))
  methodSubst <-
    case inferRuntimeNullaryMethodSubst context (deferredMethodInfo deferred) classArgView instanceSubst expectedView of
      Just subst -> Right subst
      Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
  eagerConstraints <-
    filterRuntimeConstraintGround
      (map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValueInfo))
  evidenceArgs <-
    resolveRuntimeConstraintEvidenceValues
      context
      stack
      deferredValues
      env
      (deferredMethodLocalEvidence deferred)
      Set.empty
      eagerConstraints
  methodResolved <- runtimeMethodValueResolved context (deferredMethodName deferred) methodValueInfo
  methodHead <- lookupRuntimeResolvedValue context stack deferredValues env methodResolved
  foldM (applyRuntimeValue context) methodHead evidenceArgs

lookupRuntimeMethodEvidence :: RuntimeContext -> DeferredMethodCall -> TypeView -> Maybe (DeferredMethodEvidence, TypeViewSubst)
lookupRuntimeMethodEvidence context deferred classArgView =
  case uniqueEvidenceMethodMatch localMatches of
    Just (methodEvidence, subst) ->
      Just (mkEvidence methodEvidence, subst)
    Nothing ->
      case globalEvidence of
        Just methodEvidence -> Just (mkEvidence methodEvidence, Map.empty)
        Nothing -> fallbackEvidence
  where
    methodInfo = deferredMethodInfo deferred
    scope = runtimeElaborateScope context
    targetViews = classArgView NE.:| []
    mkEvidence methodEvidence =
      DeferredMethodEvidence
        { deferredMethodEvidenceClassArg = classArgView,
          deferredMethodEvidenceClassArgs = targetViews,
          deferredMethodEvidenceMethod = methodEvidence
        }
    globalEvidence =
      lookupEvidenceMethodByClass
        scope
        (methodInfoOwnerClassSymbolIdentity methodInfo)
        (typeViewIdentity classArgView)
        (methodInfoSymbolIdentity methodInfo)
    localMatches =
      [ (methodEvidence, subst)
      | evidence <- deferredMethodLocalEvidence deferred,
        evidenceClassSymbol evidence == methodInfoOwnerClassSymbolIdentity methodInfo,
        Just subst <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) targetViews],
        methodEvidence <- maybe [] (: []) (Map.lookup (methodInfoSymbolIdentity methodInfo) (evidenceMethodsByIdentity evidence))
      ]
    fallbackEvidence = do
      evidence <- deferredMethodEvidence deferred
      subst <- matchMethodTypeViews scope Map.empty (deferredMethodEvidenceClassArgs evidence) targetViews
      pure (evidence {deferredMethodEvidenceClassArg = classArgView, deferredMethodEvidenceClassArgs = targetViews}, subst)

runtimeMethodLocalConstraints :: MethodInfo -> TypeView -> TypeViewSubst -> Either ProgramError [ConstraintInfo]
runtimeMethodLocalConstraints methodInfo classArgView methodSubst = do
  headVars <- freeTypeBinderIdentitiesTypeViewsOrError (NE.singleton classArgView)
  methodLocal <-
    filterM
      (fmap not . constraintDeterminedByTypeBinderIdentities headVars)
      specializedForClass
  pure (map (applyConstraintInfoSubst methodSubst) methodLocal)
  where
    classArgSubst =
      typeViewSubstFromParamIdentities
        (methodParamBinderIdentities methodInfo)
        (NE.singleton classArgView)
    specializedForClass =
      map
        (applyConstraintInfoSubst classArgSubst)
        (methodConstraintInfos methodInfo)

runtimeMethodValueResolved :: RuntimeContext -> String -> ValueInfo -> Either ProgramError ResolvedVar
runtimeMethodValueResolved context _ valueInfo@OrdinaryValue {} = do
  resolvedTy <-
    typeViewToElabType
      (runtimeElaborateScope context)
      (ordinaryValueTypeView valueInfo)
  Right (resolvedVarFromValueInfo valueInfo resolvedTy)
runtimeMethodValueResolved _ methodName0 _ = Left (ProgramUnknownMethod methodName0)

methodValueConstraints :: ValueInfo -> [ConstraintInfo]
methodValueConstraints OrdinaryValue {valueConstraintInfos = constraints} = constraints
methodValueConstraints _ = []

constraintGround :: ConstraintInfo -> Either ProgramError Bool
constraintGround constraint =
  Set.null <$> freeTypeBinderIdentitiesTypeViewsOrError (constraintTypeViews constraint)

filterRuntimeConstraintGround :: [ConstraintInfo] -> Either ProgramError [ConstraintInfo]
filterRuntimeConstraintGround =
  filterM constraintGround

resolveRuntimeConstraintEvidenceValues ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  [EvidenceInfo] ->
  Set.Set (SymbolIdentity, [SrcType]) ->
  [ConstraintInfo] ->
  Either ProgramError [RuntimeValue]
resolveRuntimeConstraintEvidenceValues context stack deferredValues env localEvidence seen constraints =
  concat <$> mapM (resolveRuntimeConstraintEvidenceValue context stack deferredValues env localEvidence seen) constraints

resolveRuntimeConstraintEvidenceValue ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  [EvidenceInfo] ->
  Set.Set (SymbolIdentity, [SrcType]) ->
  ConstraintInfo ->
  Either ProgramError [RuntimeValue]
resolveRuntimeConstraintEvidenceValue context stack deferredValues env localEvidence seen constraint = do
  let key = constraintEvidenceKey constraint
  if key `Set.member` seen
    then Left (noMatchingInstanceError (runtimeElaborateScope context) constraint)
    else do
      mbLocalEvidence <- resolveRuntimeLocalConstraintEvidenceValues context stack deferredValues env localEvidence constraint
      case mbLocalEvidence of
        Just evidenceValues -> Right evidenceValues
        Nothing -> do
          (instanceInfo, subst) <- resolveInstanceInfoByConstraint (runtimeElaborateScope context) constraint
          let seen' = Set.insert key seen
              methodValues = [valueInfo | valueInfo@OrdinaryValue {} <- Map.elems (instanceMethodsByIdentity instanceInfo)]
          if null methodValues
            then do
              _ <-
                resolveRuntimeConstraintEvidenceValues
                  context
                  stack
                  deferredValues
                  env
                  localEvidence
                  seen'
                  (map (applyConstraintInfoSubst subst) (instanceConstraintInfos instanceInfo))
              Right []
            else mapM (materializeRuntimeMethodEvidence context stack deferredValues env localEvidence seen' subst constraint) methodValues

materializeRuntimeMethodEvidence ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  [EvidenceInfo] ->
  Set.Set (SymbolIdentity, [SrcType]) ->
  TypeViewSubst ->
  ConstraintInfo ->
  ValueInfo ->
  Either ProgramError RuntimeValue
materializeRuntimeMethodEvidence context stack deferredValues env localEvidence seen subst constraint valueInfo = do
  eagerConstraints <-
    filterRuntimeConstraintGround
      (map (applyConstraintInfoSubst subst) (methodValueConstraints valueInfo))
  nestedEvidence <-
    resolveRuntimeConstraintEvidenceValues
      context
      stack
      deferredValues
      env
      localEvidence
      seen
      eagerConstraints
  methodResolved <- runtimeMethodValueResolved context (constraintDisplayClass constraint) valueInfo
  methodHead <- lookupRuntimeResolvedValue context stack deferredValues env methodResolved
  foldM (applyRuntimeValue context) methodHead nestedEvidence

resolveRuntimeLocalConstraintEvidenceValues ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  [EvidenceInfo] ->
  ConstraintInfo ->
  Either ProgramError (Maybe [RuntimeValue])
resolveRuntimeLocalConstraintEvidenceValues context stack deferredValues env localEvidence constraint =
  case classInfoForConstraint (runtimeElaborateScope context) constraint of
    Nothing -> Right Nothing
    Just classInfo
      | Map.null (classMethodsByIdentity classInfo) ->
          Right $
            if zeroMethodConstraintCoveredByEvidenceInfo (runtimeElaborateScope context) constraint
              || zeroMethodConstraintCoveredByRuntimeEvidence (runtimeElaborateScope context) localEvidence constraint
              then Just []
              else Nothing
      | otherwise -> do
          let localMethodEvidence =
                mapM
                  ( \methodInfo -> do
                      methodEvidence <-
                        lookupEvidenceMethodByClassTypes
                          (runtimeElaborateScope context)
                          (constraintClassSymbol constraint)
                          (typeViewsIdentity (constraintTypeViews constraint))
                          (methodInfoSymbolIdentity methodInfo)
                          `orElseRuntimeEvidenceMethod`
                          lookupRuntimeEvidenceMethod
                            (runtimeElaborateScope context)
                            localEvidence
                            (constraintClassSymbol constraint)
                            (constraintTypeViews constraint)
                            (methodInfoSymbolIdentity methodInfo)
                      pure methodEvidence
                  )
                  (Map.elems (classMethodsByIdentity classInfo))
          case localMethodEvidence of
            Nothing -> Right Nothing
            Just methodEvidence ->
              Just <$> mapM (lookupRuntimeEvidenceMethodValue context stack deferredValues env) methodEvidence

lookupRuntimeEvidenceMethod :: ElaborateScope -> [EvidenceInfo] -> SymbolIdentity -> NE.NonEmpty TypeView -> SymbolIdentity -> Maybe EvidenceMethod
lookupRuntimeEvidenceMethod scope evidenceInfos classIdentity headViews methodIdentity =
  uniqueEvidenceMethod
    [ methodEvidence
      | evidence <- evidenceInfos,
        evidenceClassSymbol evidence == classIdentity,
        Just _ <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) headViews],
        methodEvidence <- maybe [] (: []) (Map.lookup methodIdentity (evidenceMethodsByIdentity evidence))
    ]

lookupRuntimeEvidenceMethodValue ::
  RuntimeContext ->
  RuntimeLookupStack ->
  RuntimeDeferredValues ->
  RuntimeEnv ->
  EvidenceMethod ->
  Either ProgramError RuntimeValue
lookupRuntimeEvidenceMethodValue context stack deferredValues env methodEvidence =
  case evidenceMethodResolvedVar methodEvidence of
    Just resolved ->
      lookupRuntimeResolvedValue context stack deferredValues env resolved
    Nothing ->
      Left
        ( ProgramPipelineError
            ("run-program evidence method lacks resolved identity: " ++ evidenceMethodRuntimeName methodEvidence)
        )

orElseRuntimeEvidenceMethod :: Maybe EvidenceMethod -> Maybe EvidenceMethod -> Maybe EvidenceMethod
orElseRuntimeEvidenceMethod (Just evidence) _ = Just evidence
orElseRuntimeEvidenceMethod Nothing fallback = fallback

constraintEvidenceKey :: ConstraintInfo -> (SymbolIdentity, [SrcType])
constraintEvidenceKey constraint =
  (constraintClassSymbol constraint, NE.toList (typeViewsIdentity (constraintTypeViews constraint)))

noMatchingInstanceError :: ElaborateScope -> ConstraintInfo -> ProgramError
noMatchingInstanceError scope constraint =
  case NE.toList (fmap (diagnosticTypeViewDisplay scope) (constraintTypeViews constraint)) of
    [ty] -> ProgramNoMatchingInstance (constraintDisplayClass constraint) ty
    tys -> ProgramNoMatchingInstanceHead (constraintDisplayClass constraint) tys

zeroMethodConstraintCoveredByRuntimeEvidence :: ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByRuntimeEvidence scope evidenceInfos constraint =
  any
    ( \evidence ->
        evidenceClassSymbol evidence == constraintClassSymbol constraint
          && case matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) (constraintTypeViews constraint) of
            Just _ -> True
            Nothing -> False
    )
    evidenceInfos

constraintDeterminedByTypeBinderIdentities :: Set.Set TypeBinderIdentity -> ConstraintInfo -> Either ProgramError Bool
constraintDeterminedByTypeBinderIdentities typeVars constraint =
  (`Set.isSubsetOf` typeVars) <$> freeTypeBinderIdentitiesTypeViewsOrError (constraintTypeViews constraint)

freeTypeBinderIdentitiesTypeViewsOrError :: NE.NonEmpty TypeView -> Either ProgramError (Set.Set TypeBinderIdentity)
freeTypeBinderIdentitiesTypeViewsOrError views =
  case freeTypeBinderIdentitiesTypeViews views of
    Right identities -> Right identities
    Left name ->
      Left $
        ProgramPipelineError
          ("run-program resolved type variable `" ++ name ++ "` is missing binder identity")

runtimeValueTypeView :: RuntimeContext -> RuntimeValue -> Either ProgramError TypeView
runtimeValueTypeView context value =
  case value of
    RuntimeLit (LInt _) -> Right (sourceTypeViewInScope scope (STBase "Int"))
    RuntimeLit (LBool _) -> Right (sourceTypeViewInScope scope (STBase "Bool"))
    RuntimeLit (LChar _) -> Right (sourceTypeViewInScope scope (STBase "Char"))
    RuntimeLit (LString _) -> Right (sourceTypeViewInScope scope (STBase "String"))
    RuntimeUnit ->
      case preludeUnitTypeView context of
        Just view -> Right view
        Nothing -> Left (ProgramPipelineError "run-program could not recover Unit type metadata")
    RuntimeData _ resultView _ -> Right resultView
    _ -> Left (ProgramPipelineError "run-program IO runtime cannot infer deferred method argument type")
  where
    scope = runtimeElaborateScope context

inferRuntimeMethodClassArgument :: RuntimeContext -> MethodInfo -> [TypeView] -> Maybe TypeView -> Maybe TypeView
inferRuntimeMethodClassArgument context methodInfo argViews mbExpectedResult =
  inferRuntimeMethodClassArgumentFromArgs context methodInfo argViews
    <|> inferRuntimeMethodClassArgumentFromExpected context methodInfo argViews mbExpectedResult

inferRuntimeMethodClassArgumentFromArgs :: RuntimeContext -> MethodInfo -> [TypeView] -> Maybe TypeView
inferRuntimeMethodClassArgumentFromArgs context methodInfo argViews = do
  subst <-
    foldM
      (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (NE.singleton templateView) (NE.singleton actualView))
      Map.empty
      (zip (methodParamViews methodView) argViews)
  NE.head <$> lookupMethodParamViewSubst methodInfo subst
  where
    scope = runtimeElaborateScope context
    methodView = methodTypeView methodInfo

inferRuntimeMethodClassArgumentFromExpected :: RuntimeContext -> MethodInfo -> [TypeView] -> Maybe TypeView -> Maybe TypeView
inferRuntimeMethodClassArgumentFromExpected _ _ _ Nothing = Nothing
inferRuntimeMethodClassArgumentFromExpected context methodInfo argViews (Just expectedView) = do
  let scope = runtimeElaborateScope context
      methodView = methodTypeView methodInfo
  substFromArgs <-
    foldM
      (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (NE.singleton templateView) (NE.singleton actualView))
      Map.empty
      (zip (methodParamViews methodView) argViews)
  subst <- matchMethodTypeViews scope substFromArgs (NE.singleton (methodResultTypeView methodInfo)) (NE.singleton expectedView)
  NE.head <$> lookupMethodParamViewSubst methodInfo subst

inferRuntimeNullaryMethodClassArgument :: RuntimeContext -> MethodInfo -> TypeView -> Maybe TypeView
inferRuntimeNullaryMethodClassArgument context methodInfo expectedView
  | methodFullArityFromInfo methodInfo /= 0 = Nothing
  | otherwise = do
      let scope = runtimeElaborateScope context
      subst <- matchMethodTypeViews scope Map.empty (NE.singleton (methodResultTypeView methodInfo)) (NE.singleton expectedView)
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

inferRuntimeNullaryMethodSubst :: RuntimeContext -> MethodInfo -> TypeView -> TypeViewSubst -> TypeView -> Maybe TypeViewSubst
inferRuntimeNullaryMethodSubst context methodInfo classArgView subst expectedView =
  matchMethodTypeViews scope subst (NE.singleton (methodResultView specializedMethodView)) (NE.singleton expectedView)
  where
    scope = runtimeElaborateScope context
    specializedMethodView = specializeMethodTypeView methodInfo (NE.singleton classArgView)

inferRuntimeMethodArgumentSubst :: RuntimeContext -> MethodInfo -> TypeView -> TypeViewSubst -> [TypeView] -> Maybe TypeViewSubst
inferRuntimeMethodArgumentSubst context methodInfo classArgView subst argViews =
  foldM
    (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (NE.singleton templateView) (NE.singleton actualView))
    subst
    (zip (methodParamViews specializedMethodView) argViews)
  where
    scope = runtimeElaborateScope context
    specializedMethodView = specializeMethodTypeView methodInfo (NE.singleton classArgView)

methodResultView :: TypeView -> TypeView
methodResultView view =
  view
    { typeViewDisplay = displayResult,
      typeViewIdentity = identityResult
    }
  where
    (_, displayBodyTy) = splitForalls (typeViewDisplay view)
    (_, displayResult) = splitArrows displayBodyTy
    (_, identityBodyTy) = splitForalls (typeViewIdentity view)
    (_, identityResult) = splitArrows identityBodyTy

methodParamViews :: TypeView -> [TypeView]
methodParamViews view =
  zipWith paramView displayParamTys identityParamTys
  where
    (_, displayBodyTy) = splitForalls (typeViewDisplay view)
    (displayParamTys, _) = splitArrows displayBodyTy
    (_, identityBodyTy) = splitForalls (typeViewIdentity view)
    (identityParamTys, _) = splitArrows identityBodyTy
    paramView displayTy identityTy =
      view
        { typeViewDisplay = displayTy,
          typeViewIdentity = identityTy
        }

methodFullArityFromInfo :: MethodInfo -> Int
methodFullArityFromInfo methodInfo =
  length (fst (splitArrows (snd (splitForalls (methodType methodInfo)))))

applyRuntimePrimitive :: RuntimeContext -> RuntimePrimitive -> [RuntimeValue] -> Either ProgramError RuntimeValue
applyRuntimePrimitive context prim args
  | length args < runtimePrimitiveArity prim = Right (RuntimePrimitive prim args)
  | length args > runtimePrimitiveArity prim =
      Left (ProgramPipelineError ("run-program IO primitive over-applied: " ++ show prim))
  | otherwise =
      case (prim, args) of
        (RuntimeIOPure, [value]) ->
          Right (RuntimeIO (RuntimePure value))
        (RuntimeIOBind, [RuntimeIO action, continuation]) ->
          Right (RuntimeIO (RuntimeBind action continuation))
        (RuntimeIOBind, _) ->
          Left (ProgramPipelineError "run-program __io_bind expected an IO action and continuation")
        (RuntimeIOMap, [mapper, RuntimeIO action]) ->
          Right (RuntimeIO (RuntimeMap mapper action))
        (RuntimeIOMap, _) ->
          Left (ProgramPipelineError "run-program __io_map expected a mapper and an IO action")
        (RuntimeIOAp, [RuntimeIO wrappedFunction, RuntimeIO wrappedValue]) ->
          Right (RuntimeIO (RuntimeAp wrappedFunction wrappedValue))
        (RuntimeIOAp, _) ->
          Left (ProgramPipelineError "run-program __io_ap expected two IO actions")
        (RuntimeIOPutStrLn, [RuntimeLit (LString msg)]) ->
          Right (RuntimeIO (RuntimePutStrLn msg))
        (RuntimeIOPutStrLn, [_]) ->
          Left (ProgramPipelineError "run-program __io_putStrLn expected a String argument")
        (RuntimeIOGetLine, []) ->
          Right (RuntimeIO RuntimeGetLine)
        (RuntimeIOPutStr, [RuntimeLit (LString msg)]) ->
          Right (RuntimeIO (RuntimePutStr msg))
        (RuntimeIOPutStr, [_]) ->
          Left (ProgramPipelineError "run-program __io_putStr expected a String argument")
        (RuntimeIOReadFile, [RuntimeLit (LString path)]) ->
          Right (RuntimeIO (RuntimeReadFile path))
        (RuntimeIOReadFile, [_]) ->
          Left (ProgramPipelineError "run-program __io_readFile expected a String argument")
        (RuntimeIOWriteFile, [RuntimeLit (LString path), RuntimeLit (LString contents)]) ->
          Right (RuntimeIO (RuntimeWriteFile path contents))
        (RuntimeIOWriteFile, _) ->
          Left (ProgramPipelineError "run-program __io_writeFile expected two String arguments")
        (RuntimeIOAppendFile, [RuntimeLit (LString path), RuntimeLit (LString contents)]) ->
          Right (RuntimeIO (RuntimeAppendFile path contents))
        (RuntimeIOAppendFile, _) ->
          Left (ProgramPipelineError "run-program __io_appendFile expected two String arguments")
        (RuntimeIOExitWith, [RuntimeLit (LInt status)]) ->
          Right (RuntimeIO (RuntimeExitWith status))
        (RuntimeIOExitWith, [_]) ->
          Left (ProgramPipelineError "run-program __io_exitWith expected an Int argument")
        (RuntimeIONewIORef, [value]) ->
          Right (RuntimeIO (RuntimeNewIORef value))
        (RuntimeIOReadIORef, [ref]) ->
          Right (RuntimeIO (RuntimeReadIORef ref))
        (RuntimeIOWriteIORef, [ref, value]) ->
          Right (RuntimeIO (RuntimeWriteIORef ref value))
        (RuntimeIOWriteIORef, _) ->
          Left (ProgramPipelineError "run-program __io_writeIORef expected two arguments")
        (RuntimeIOGetArgs, []) ->
          Right (RuntimeIO RuntimeGetArgs)
        (RuntimeAnd, [RuntimeLit (LBool left), RuntimeLit (LBool right)]) ->
          Right (RuntimeLit (LBool (left && right)))
        (RuntimeAnd, _) ->
          Left (ProgramPipelineError "run-program __mlfp_and expected Bool arguments")
        (RuntimeStringLength, [RuntimeLit (LString value)]) ->
          Right (RuntimeLit (LInt (toInteger (length value))))
        (RuntimeStringLength, _) ->
          Left (ProgramPipelineError "run-program __string_length expected a String argument")
        (RuntimeStringIsEmpty, [RuntimeLit (LString value)]) ->
          Right (RuntimeLit (LBool (null value)))
        (RuntimeStringIsEmpty, _) ->
          Left (ProgramPipelineError "run-program __string_is_empty expected a String argument")
        (RuntimeStringContainsChar, [RuntimeLit (LString value), RuntimeLit (LChar needle)]) ->
          Right (RuntimeLit (LBool (needle `elem` value)))
        (RuntimeStringContainsChar, _) ->
          Left (ProgramPipelineError "run-program __string_contains_char expected String and Char arguments")
        (RuntimeStringContains, [RuntimeLit (LString haystack), RuntimeLit (LString needle)]) ->
          Right (RuntimeLit (LBool (needle `isInfixOf` haystack)))
        (RuntimeStringContains, _) ->
          Left (ProgramPipelineError "run-program __string_contains expected String arguments")
        (RuntimeStringEquals, [RuntimeLit (LString left), RuntimeLit (LString right)]) ->
          Right (RuntimeLit (LBool (left == right)))
        (RuntimeStringEquals, _) ->
          Left (ProgramPipelineError "run-program __string_equals expected String arguments")
        (RuntimeStringStartsWith, [RuntimeLit (LString haystack), RuntimeLit (LString prefix)]) ->
          Right (RuntimeLit (LBool (prefix `isPrefixOf` haystack)))
        (RuntimeStringStartsWith, _) ->
          Left (ProgramPipelineError "run-program __string_starts_with expected String arguments")
        (RuntimeStringEndsWith, [RuntimeLit (LString haystack), RuntimeLit (LString suffix)]) ->
          Right (RuntimeLit (LBool (suffix `isSuffixOf` haystack)))
        (RuntimeStringEndsWith, _) ->
          Left (ProgramPipelineError "run-program __string_ends_with expected String arguments")
        (RuntimeStringAppend, [RuntimeLit (LString left), RuntimeLit (LString right)]) ->
          Right (RuntimeLit (LString (left ++ right)))
        (RuntimeStringAppend, _) ->
          Left (ProgramPipelineError "run-program __string_append expected String arguments")
        (RuntimeStringReplaceChar, [RuntimeLit (LString value), RuntimeLit (LChar needle), RuntimeLit (LChar replacement)]) ->
          Right (RuntimeLit (LString (map (\char -> if char == needle then replacement else char) value)))
        (RuntimeStringReplaceChar, _) ->
          Left (ProgramPipelineError "run-program __string_replace_char expected String and Char arguments")
        (RuntimeStringReplace, [RuntimeLit (LString haystack), RuntimeLit (LString needle), RuntimeLit (LString replacement)]) ->
          Right (RuntimeLit (LString (replaceString haystack needle replacement)))
        (RuntimeStringReplace, _) ->
          Left (ProgramPipelineError "run-program __string_replace expected String arguments")
        (RuntimeStringIndexOfChar, [RuntimeLit (LString value), RuntimeLit (LChar needle)]) ->
          runtimeStringIndexOfChar context value needle
        (RuntimeStringIndexOfChar, _) ->
          Left (ProgramPipelineError "run-program __string_index_of_char expected String and Char arguments")
        (RuntimeStringIndexOf, [RuntimeLit (LString haystack), RuntimeLit (LString needle)]) ->
          runtimeStringIndexOf context haystack needle
        (RuntimeStringIndexOf, _) ->
          Left (ProgramPipelineError "run-program __string_index_of expected String arguments")
        (RuntimeStringSplit, [RuntimeLit (LString haystack), RuntimeLit (LString delimiter)]) ->
          runtimeStringSplit context haystack delimiter
        (RuntimeStringSplit, _) ->
          Left (ProgramPipelineError "run-program __string_split expected String arguments")
        (RuntimeStringJoin, [RuntimeLit (LString separator), values]) ->
          RuntimeLit . LString . intercalate separator <$> runtimeStringListToStrings context values
        (RuntimeStringJoin, _) ->
          Left (ProgramPipelineError "run-program __string_join expected String and List String arguments")
        (RuntimeStringSplitChar, [RuntimeLit (LString haystack), RuntimeLit (LChar delimiter)]) ->
          runtimeStringSplit context haystack [delimiter]
        (RuntimeStringSplitChar, _) ->
          Left (ProgramPipelineError "run-program __string_split_char expected String and Char arguments")
        (RuntimeStringCompare, [RuntimeLit (LString left), RuntimeLit (LString right)]) ->
          Right (RuntimeLit (LInt (orderingToInt (compare left right))))
        (RuntimeStringCompare, _) ->
          Left (ProgramPipelineError "run-program __string_compare expected String arguments")
        (RuntimeStringFromChar, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LString [value]))
        (RuntimeStringFromChar, _) ->
          Left (ProgramPipelineError "run-program __string_from_char expected a Char argument")
        (RuntimeStringFromInt, [RuntimeLit (LInt value)]) ->
          Right (RuntimeLit (LString (show value)))
        (RuntimeStringFromInt, _) ->
          Left (ProgramPipelineError "run-program __string_from_int expected an Int argument")
        (RuntimeStringFromBool, [RuntimeLit (LBool value)]) ->
          Right (RuntimeLit (LString (if value then "true" else "false")))
        (RuntimeStringFromBool, _) ->
          Left (ProgramPipelineError "run-program __string_from_bool expected a Bool argument")
        (RuntimeStringFromNat, [value]) ->
          RuntimeLit . LString . show <$> runtimeNatToInteger context value
        (RuntimeStringFromNat, _) ->
          Left (ProgramPipelineError "run-program __string_from_nat expected a Nat argument")
        (RuntimePreludeStringFromList, [value]) ->
          RuntimeLit . LString <$> runtimeListCharToString context value
        (RuntimePreludeStringFromList, _) ->
          Left (ProgramPipelineError "run-program stringFromList expected a List Char argument")
        (RuntimeStringToList, [RuntimeLit (LString value)]) ->
          runtimeStringToList context value
        (RuntimeStringToList, _) ->
          Left (ProgramPipelineError "run-program __string_to_list expected a String argument")
        (RuntimeStringDrop, [RuntimeLit (LString value), RuntimeLit (LInt count)]) ->
          Right (RuntimeLit (LString (dropUnicodeScalars count value)))
        (RuntimeStringDrop, _) ->
          Left (ProgramPipelineError "run-program __string_drop expected String and Int arguments")
        (RuntimeStringTake, [RuntimeLit (LString value), RuntimeLit (LInt count)]) ->
          Right (RuntimeLit (LString (takeUnicodeScalars count value)))
        (RuntimeStringTake, _) ->
          Left (ProgramPipelineError "run-program __string_take expected String and Int arguments")
        (RuntimeStringSlice, [RuntimeLit (LString value), RuntimeLit (LInt start), RuntimeLit (LInt count)]) ->
          Right (RuntimeLit (LString (sliceUnicodeScalars start count value)))
        (RuntimeStringSlice, _) ->
          Left (ProgramPipelineError "run-program __string_slice expected String and two Int arguments")
        (RuntimeStringCharAt, [RuntimeLit (LString value), RuntimeLit (LInt index)]) ->
          case charAtUnicodeScalar index value of
            Just char -> Right (RuntimeLit (LChar char))
            Nothing -> Left (ProgramPipelineError "run-program __string_char_at index out of range")
        (RuntimeStringCharAt, _) ->
          Left (ProgramPipelineError "run-program __string_char_at expected String and Int arguments")
        (RuntimeStringCharAtOption, [RuntimeLit (LString value), RuntimeLit (LInt index)]) ->
          runtimeStringCharAtOption context value index
        (RuntimeStringCharAtOption, _) ->
          Left (ProgramPipelineError "run-program __string_char_at_option expected String and Int arguments")
        (RuntimeCharIsDigit, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiDecimalDigit value)))
        (RuntimeCharIsDigit, _) ->
          Left (ProgramPipelineError "run-program __char_is_digit expected a Char argument")
        (RuntimeCharIsAsciiLower, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiLower value)))
        (RuntimeCharIsAsciiLower, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_lower expected a Char argument")
        (RuntimeCharIsAsciiUpper, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiUpper value)))
        (RuntimeCharIsAsciiUpper, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_upper expected a Char argument")
        (RuntimeCharIsAsciiAlpha, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiAlpha value)))
        (RuntimeCharIsAsciiAlpha, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_alpha expected a Char argument")
        (RuntimeCharIsAsciiAlphaNum, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiAlphaNum value)))
        (RuntimeCharIsAsciiAlphaNum, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_alpha_num expected a Char argument")
        (RuntimeCharIsAsciiIdentifierStart, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiIdentifierStart value)))
        (RuntimeCharIsAsciiIdentifierStart, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_identifier_start expected a Char argument")
        (RuntimeCharIsAsciiIdentifierContinue, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiIdentifierContinue value)))
        (RuntimeCharIsAsciiIdentifierContinue, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_identifier_continue expected a Char argument")
        (RuntimeCharIsAsciiWhitespace, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiWhitespace value)))
        (RuntimeCharIsAsciiWhitespace, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_whitespace expected a Char argument")
        (RuntimeCharIsAsciiPunctuation, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiPunctuation value)))
        (RuntimeCharIsAsciiPunctuation, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_punctuation expected a Char argument")
        (RuntimeCharIsAsciiPrintable, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiPrintable value)))
        (RuntimeCharIsAsciiPrintable, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_printable expected a Char argument")
        (RuntimeCharIsAsciiHexDigit, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiHexDigit value)))
        (RuntimeCharIsAsciiHexDigit, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_hex_digit expected a Char argument")
        (RuntimeCharIsAsciiLineBreak, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiLineBreak value)))
        (RuntimeCharIsAsciiLineBreak, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_line_break expected a Char argument")
        (RuntimeCharIsAsciiControl, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LBool (isAsciiControl value)))
        (RuntimeCharIsAsciiControl, _) ->
          Left (ProgramPipelineError "run-program __char_is_ascii_control expected a Char argument")
        (RuntimeCharToAsciiLower, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LChar (toAsciiLower value)))
        (RuntimeCharToAsciiLower, _) ->
          Left (ProgramPipelineError "run-program __char_to_ascii_lower expected a Char argument")
        (RuntimeCharToAsciiUpper, [RuntimeLit (LChar value)]) ->
          Right (RuntimeLit (LChar (toAsciiUpper value)))
        (RuntimeCharToAsciiUpper, _) ->
          Left (ProgramPipelineError "run-program __char_to_ascii_upper expected a Char argument")
        (RuntimeStringToAsciiLower, [RuntimeLit (LString value)]) ->
          Right (RuntimeLit (LString (map toAsciiLower value)))
        (RuntimeStringToAsciiLower, _) ->
          Left (ProgramPipelineError "run-program __string_to_ascii_lower expected a String argument")
        (RuntimeStringToAsciiUpper, [RuntimeLit (LString value)]) ->
          Right (RuntimeLit (LString (map toAsciiUpper value)))
        (RuntimeStringToAsciiUpper, _) ->
          Left (ProgramPipelineError "run-program __string_to_ascii_upper expected a String argument")
        _ ->
          Left (ProgramPipelineError ("run-program malformed IO primitive call: " ++ show prim))

isAsciiDecimalDigit :: Char -> Bool
isAsciiDecimalDigit value =
  value >= '0' && value <= '9'

resolvedVarMatchesPreludeStringFromList :: RuntimeContext -> ResolvedVar -> Bool
resolvedVarMatchesPreludeStringFromList context resolved =
  case lookupPreludeBinding context PreludeStringFromList of
    Just binding -> resolvedVarMatchesCheckedBinding resolved binding
    Nothing -> False

lookupPreludeBinding :: RuntimeContext -> PreludeBindingKey -> Maybe CheckedBinding
lookupPreludeBinding context key =
  Map.lookup key (runtimePreludeBindingsByKey context)

runtimeStringIndexOfChar :: RuntimeContext -> String -> Char -> Either ProgramError RuntimeValue
runtimeStringIndexOfChar context value needle = do
  case elemIndex needle value of
    Just index -> runtimeSomeInt context (toInteger index)
    Nothing -> runtimeNoneInt context

runtimeStringIndexOf :: RuntimeContext -> String -> String -> Either ProgramError RuntimeValue
runtimeStringIndexOf context haystack needle =
  case findIndex (needle `isPrefixOf`) (tails haystack) of
    Just index -> runtimeSomeInt context (toInteger index)
    Nothing -> runtimeNoneInt context

replaceString :: String -> String -> String -> String
replaceString haystack needle replacement
  | null needle = haystack
  | otherwise = go haystack
  where
    go [] = []
    go rest@(char : afterChar) =
      case stripPrefix needle rest of
        Just afterMatch -> replacement ++ go afterMatch
        Nothing -> char : go afterChar

runtimeStringSplit :: RuntimeContext -> String -> String -> Either ProgramError RuntimeValue
runtimeStringSplit context haystack delimiter =
  runtimeStringList context (splitString haystack delimiter)

splitString :: String -> String -> [String]
splitString haystack delimiter
  | null delimiter = [haystack]
  | otherwise = go haystack
  where
    go rest =
      case splitFirst rest of
        Just (segment, afterMatch) -> segment : go afterMatch
        Nothing -> [rest]

    splitFirst =
      scan []

    scan prefix rest =
      case stripPrefix delimiter rest of
        Just afterMatch -> Just (reverse prefix, afterMatch)
        Nothing ->
          case rest of
            [] -> Nothing
            char : afterChar -> scan (char : prefix) afterChar

runtimeSomeInt :: RuntimeContext -> Integer -> Either ProgramError RuntimeValue
runtimeSomeInt context value = do
  someCtor <- requirePreludeConstructor context PreludeOptionSome
  runtimeConstructorValue context (RuntimeConstructorSpec someCtor Nothing) [RuntimeLit (LInt value)]

runtimeNoneInt :: RuntimeContext -> Either ProgramError RuntimeValue
runtimeNoneInt =
  runtimeNone

runtimeSomeChar :: RuntimeContext -> Char -> Either ProgramError RuntimeValue
runtimeSomeChar context value =
  runtimeSome context (RuntimeLit (LChar value))

runtimeNone :: RuntimeContext -> Either ProgramError RuntimeValue
runtimeNone context = do
  noneCtor <- requirePreludeConstructor context PreludeOptionNone
  runtimeConstructorValue context (RuntimeConstructorSpec noneCtor Nothing) []

runtimeSome :: RuntimeContext -> RuntimeValue -> Either ProgramError RuntimeValue
runtimeSome context value = do
  someCtor <- requirePreludeConstructor context PreludeOptionSome
  runtimeConstructorValue context (RuntimeConstructorSpec someCtor Nothing) [value]

runtimeStringCharAtOption :: RuntimeContext -> String -> Integer -> Either ProgramError RuntimeValue
runtimeStringCharAtOption context value index =
  case charAtUnicodeScalar index value of
    Just char -> runtimeSomeChar context char
    Nothing -> runtimeNone context

runtimeStringToList :: RuntimeContext -> String -> Either ProgramError RuntimeValue
runtimeStringToList context value = do
  runtimeList context [RuntimeLit (LChar char) | char <- value]

runtimeStringList :: RuntimeContext -> [String] -> Either ProgramError RuntimeValue
runtimeStringList context values =
  runtimeList context [RuntimeLit (LString value) | value <- values]

runtimeList :: RuntimeContext -> [RuntimeValue] -> Either ProgramError RuntimeValue
runtimeList context values = do
  nilCtor <- requirePreludeConstructor context PreludeListNil
  consCtor <- requirePreludeConstructor context PreludeListCons
  nilValue <- runtimeConstructorValue context (RuntimeConstructorSpec nilCtor Nothing) []
  foldM
    ( \tailValue value ->
        runtimeConstructorValue
          context
          (RuntimeConstructorSpec consCtor Nothing)
          [value, tailValue]
    )
    nilValue
    (reverse values)

runtimeListCharToString :: RuntimeContext -> RuntimeValue -> Either ProgramError String
runtimeListCharToString context =
  fmap reverse . go []
  where
    go chars value =
      case value of
        RuntimeData ctor _ []
          | isPreludeConstructor context PreludeListNil ctor -> Right chars
        RuntimeData ctor _ [RuntimeLit (LChar char), rest]
          | isPreludeConstructor context PreludeListCons ctor -> go (char : chars) rest
        _ -> Left (ProgramPipelineError "run-program stringFromList expected a List Char argument")

runtimeStringListToStrings :: RuntimeContext -> RuntimeValue -> Either ProgramError [String]
runtimeStringListToStrings context =
  fmap reverse . go []
  where
    go strings value =
      case value of
        RuntimeData ctor _ []
          | isPreludeConstructor context PreludeListNil ctor -> Right strings
        RuntimeData ctor _ [RuntimeLit (LString string), rest]
          | isPreludeConstructor context PreludeListCons ctor -> go (string : strings) rest
        _ -> Left (ProgramPipelineError "run-program __string_join expected a List String argument")

runtimeNatToInteger :: RuntimeContext -> RuntimeValue -> Either ProgramError Integer
runtimeNatToInteger context =
  go 0
  where
    go count value =
      case value of
        RuntimeData ctor _ []
          | isPreludeConstructor context PreludeNatZero ctor -> Right count
        RuntimeData ctor _ [rest]
          | isPreludeConstructor context PreludeNatSucc ctor -> go (count + 1) rest
        _ -> Left (ProgramPipelineError "run-program __string_from_nat expected a Nat argument")

requirePreludeConstructor :: RuntimeContext -> PreludeConstructorKey -> Either ProgramError ConstructorInfo
requirePreludeConstructor context key =
  case lookupPreludeConstructor context key of
    Just ctor -> Right ctor
    Nothing -> Left (ProgramPipelineError ("run-program missing Prelude constructor " ++ preludeConstructorLabel key))

lookupPreludeUnitConstructor :: RuntimeContext -> Maybe ConstructorInfo
lookupPreludeUnitConstructor context =
  lookupPreludeConstructor context PreludeUnitUnit

lookupPreludeConstructor :: RuntimeContext -> PreludeConstructorKey -> Maybe ConstructorInfo
lookupPreludeConstructor context key =
  Map.lookup key (runtimePreludeConstructorsByKey context)

isPreludeConstructor :: RuntimeContext -> PreludeConstructorKey -> ConstructorInfo -> Bool
isPreludeConstructor context key ctor =
  case lookupPreludeConstructor context key of
    Just expected -> sameRuntimeConstructor ctor expected
    Nothing -> False

isAsciiLower :: Char -> Bool
isAsciiLower value =
  value >= 'a' && value <= 'z'

isAsciiUpper :: Char -> Bool
isAsciiUpper value =
  value >= 'A' && value <= 'Z'

isAsciiAlpha :: Char -> Bool
isAsciiAlpha value =
  isAsciiLower value || isAsciiUpper value

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum value =
  isAsciiAlpha value || isAsciiDecimalDigit value

isAsciiIdentifierStart :: Char -> Bool
isAsciiIdentifierStart value =
  value == '_' || isAsciiAlpha value

isAsciiIdentifierContinue :: Char -> Bool
isAsciiIdentifierContinue value =
  value == '_' || value == '\'' || isAsciiAlphaNum value

isAsciiWhitespace :: Char -> Bool
isAsciiWhitespace value =
  value == ' '
    || value == '\t'
    || value == '\n'
    || value == '\r'
    || value == '\f'
    || value == '\v'

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation value =
  (value >= '!' && value <= '/')
    || (value >= ':' && value <= '@')
    || (value >= '[' && value <= '`')
    || (value >= '{' && value <= '~')

isAsciiPrintable :: Char -> Bool
isAsciiPrintable value =
  value >= ' ' && value <= '~'

isAsciiHexDigit :: Char -> Bool
isAsciiHexDigit value =
  isAsciiDecimalDigit value
    || (value >= 'a' && value <= 'f')
    || (value >= 'A' && value <= 'F')

isAsciiLineBreak :: Char -> Bool
isAsciiLineBreak value =
  value == '\n' || value == '\r'

isAsciiControl :: Char -> Bool
isAsciiControl value =
  value < ' ' || value == '\DEL'

toAsciiLower :: Char -> Char
toAsciiLower value
  | isAsciiUpper value = toEnum (fromEnum value + 32)
  | otherwise = value

toAsciiUpper :: Char -> Char
toAsciiUpper value
  | isAsciiLower value = toEnum (fromEnum value - 32)
  | otherwise = value

orderingToInt :: Ordering -> Integer
orderingToInt ordering =
  case ordering of
    LT -> -1
    EQ -> 0
    GT -> 1

dropUnicodeScalars :: Integer -> String -> String
dropUnicodeScalars count value
  | count <= 0 = value
  | otherwise = go count value
  where
    go remaining rest
      | remaining <= 0 = rest
    go _ [] = []
    go remaining (_ : rest) = go (remaining - 1) rest

takeUnicodeScalars :: Integer -> String -> String
takeUnicodeScalars count value
  | count <= 0 = []
  | otherwise = go count value
  where
    go remaining _
      | remaining <= 0 = []
    go _ [] = []
    go remaining (char : rest) = char : go (remaining - 1) rest

sliceUnicodeScalars :: Integer -> Integer -> String -> String
sliceUnicodeScalars start count value =
  takeUnicodeScalars count (dropUnicodeScalars start value)

charAtUnicodeScalar :: Integer -> String -> Maybe Char
charAtUnicodeScalar index value
  | index < 0 = Nothing
  | otherwise = go index value
  where
    go _ [] = Nothing
    go remaining (char : rest)
      | remaining == 0 = Just char
      | otherwise = go (remaining - 1) rest

runtimePrimitiveArity :: RuntimePrimitive -> Int
runtimePrimitiveArity prim =
  case prim of
    RuntimeIOPure -> 1
    RuntimeIOBind -> 2
    RuntimeIOMap -> 2
    RuntimeIOAp -> 2
    RuntimeIOPutStrLn -> 1
    RuntimeIOGetLine -> 0
    RuntimeIOPutStr -> 1
    RuntimeIOReadFile -> 1
    RuntimeIOWriteFile -> 2
    RuntimeIOAppendFile -> 2
    RuntimeIOExitWith -> 1
    RuntimeIONewIORef -> 1
    RuntimeIOReadIORef -> 1
    RuntimeIOWriteIORef -> 2
    RuntimeIOGetArgs -> 0
    RuntimeAnd -> 2
    RuntimeStringLength -> 1
    RuntimeStringIsEmpty -> 1
    RuntimeStringContainsChar -> 2
    RuntimeStringContains -> 2
    RuntimeStringEquals -> 2
    RuntimeStringStartsWith -> 2
    RuntimeStringEndsWith -> 2
    RuntimeStringAppend -> 2
    RuntimeStringReplaceChar -> 3
    RuntimeStringReplace -> 3
    RuntimeStringIndexOfChar -> 2
    RuntimeStringIndexOf -> 2
    RuntimeStringSplit -> 2
    RuntimeStringJoin -> 2
    RuntimeStringSplitChar -> 2
    RuntimeStringCompare -> 2
    RuntimeStringFromChar -> 1
    RuntimeStringFromInt -> 1
    RuntimeStringFromBool -> 1
    RuntimeStringFromNat -> 1
    RuntimePreludeStringFromList -> 1
    RuntimeStringToList -> 1
    RuntimeStringDrop -> 2
    RuntimeStringTake -> 2
    RuntimeStringSlice -> 3
    RuntimeStringCharAt -> 2
    RuntimeStringCharAtOption -> 2
    RuntimeCharIsDigit -> 1
    RuntimeCharIsAsciiLower -> 1
    RuntimeCharIsAsciiUpper -> 1
    RuntimeCharIsAsciiAlpha -> 1
    RuntimeCharIsAsciiAlphaNum -> 1
    RuntimeCharIsAsciiIdentifierStart -> 1
    RuntimeCharIsAsciiIdentifierContinue -> 1
    RuntimeCharIsAsciiWhitespace -> 1
    RuntimeCharIsAsciiPunctuation -> 1
    RuntimeCharIsAsciiPrintable -> 1
    RuntimeCharIsAsciiHexDigit -> 1
    RuntimeCharIsAsciiLineBreak -> 1
    RuntimeCharIsAsciiControl -> 1
    RuntimeCharToAsciiLower -> 1
    RuntimeCharToAsciiUpper -> 1
    RuntimeStringToAsciiLower -> 1
    RuntimeStringToAsciiUpper -> 1

executeIOAction :: RuntimeContext -> RuntimeIOAction -> Either ProgramError (String, RuntimeValue)
executeIOAction context action =
  case action of
    RuntimePure value ->
      Right ("", value)
    RuntimePutStrLn msg ->
      Right (msg ++ "\n", RuntimeUnit)
    RuntimeGetLine ->
      Left (ProgramPipelineError "run-program __io_getLine requires native execution (use --native)")
    RuntimePutStr msg ->
      Right (msg, RuntimeUnit)
    RuntimeReadFile _ ->
      Left (ProgramPipelineError "run-program __io_readFile requires native execution (use --native)")
    RuntimeWriteFile _ _ ->
      Left (ProgramPipelineError "run-program __io_writeFile requires native execution (use --native)")
    RuntimeAppendFile _ _ ->
      Left (ProgramPipelineError "run-program __io_appendFile requires native execution (use --native)")
    RuntimeExitWith _ ->
      Left (ProgramPipelineError "run-program __io_exitWith requires native execution (use --native)")
    RuntimeNewIORef _ ->
      Left (ProgramPipelineError "run-program __io_newIORef requires native execution (use --native)")
    RuntimeReadIORef _ ->
      Left (ProgramPipelineError "run-program __io_readIORef requires native execution (use --native)")
    RuntimeWriteIORef _ _ ->
      Left (ProgramPipelineError "run-program __io_writeIORef requires native execution (use --native)")
    RuntimeGetArgs ->
      Left (ProgramPipelineError "run-program __io_getArgs requires native execution (use --native)")
    RuntimeBind first continuation -> do
      (firstStdout, firstValue) <- executeIOAction context first
      nextValue <- applyRuntimeValue context continuation firstValue
      nextAction <-
        case nextValue of
          RuntimeIO action' -> Right action'
          _ -> Left (ProgramPipelineError "run-program __io_bind continuation did not return an IO action")
      (nextStdout, resultValue) <- executeIOAction context nextAction
      Right (firstStdout ++ nextStdout, resultValue)
    RuntimeMap mapper action0 -> do
      (stdout0, value) <- executeIOAction context action0
      mapped <- applyRuntimeValue context mapper value
      Right (stdout0, mapped)
    RuntimeAp wrappedFunction wrappedValue -> do
      (functionStdout, functionValue) <- executeIOAction context wrappedFunction
      (valueStdout, value) <- executeIOAction context wrappedValue
      applied <- applyRuntimeValue context functionValue value
      Right (functionStdout ++ valueStdout, applied)

isRuntimeUnit :: RuntimeContext -> RuntimeValue -> Bool
isRuntimeUnit context value =
  case value of
    RuntimeUnit -> True
    RuntimeData ctor _ [] -> isPreludeUnitConstructor context ctor
    _ -> False

runtimeValueToValue :: RuntimeValue -> Value
runtimeValueToValue rv =
  case rv of
    RuntimeLit lit -> VLit lit
    RuntimeUnit -> VData "Unit" []
    RuntimeData ctor _ args -> VData (ctorName ctor) (map runtimeValueToValue args)
    RuntimeConstructor spec args -> VData (ctorName (runtimeConstructorInfo spec)) (map runtimeValueToValue args)
    _ -> VData "<closure>" []

reachableOpaqueRuntimeDependencies :: CheckedProgram -> [String]
reachableOpaqueRuntimeDependencies checked =
  map checkedBindingRuntimeName (reachableOpaqueRuntimeBindings checked)
    ++ Set.toAscList (reachableOpaquePrimitiveNames checked)

reachableRuntimeBindings :: CheckedProgram -> [CheckedBinding]
reachableRuntimeBindings checked =
  [ binding
    | binding <- reachableCheckedBindings checked,
      not (checkedBindingMentionsOpaqueBuiltin binding)
  ]

reachableOpaqueRuntimeBindings :: CheckedProgram -> [CheckedBinding]
reachableOpaqueRuntimeBindings checked =
  [ binding
    | binding <- reachableCheckedBindings checked,
      checkedBindingMentionsOpaqueBuiltin binding
  ]

reachableOpaquePrimitiveNames :: CheckedProgram -> Set.Set String
reachableOpaquePrimitiveNames checked =
  Set.fromList
    [ name
    | identity <- Set.toList (reachableOpaquePrimitiveIdentities checked),
      Just name <- [Map.lookup identity runtimePrimitiveNamesByIdentity]
    ]

reachableOpaquePrimitiveIdentities :: CheckedProgram -> Set.Set SymbolIdentity
reachableOpaquePrimitiveIdentities checked =
  reachableFreePrimitiveIdentities checked `Set.intersection` builtinOpaqueValueIdentities

reachableFreePrimitiveIdentities :: CheckedProgram -> Set.Set SymbolIdentity
reachableFreePrimitiveIdentities checked =
  Set.unions
    [ freeResolvedTermPrimitiveIdentities (checkedBindingTerm binding)
    | binding <- reachableCheckedBindings checked
    ]

reachableCheckedBindings :: CheckedProgram -> [CheckedBinding]
reachableCheckedBindings checked =
  [ binding
    | binding <- bindings,
      Set.member (checkedBindingIdentityKey binding) reachableKeys
  ]
  where
    bindings = allCheckedBindings checked
    bindingByKey =
      Map.fromList [(checkedBindingIdentityKey binding, binding) | binding <- bindings]
    roots =
      [ binding
        | binding <- bindings,
          resolvedVarMatchesCheckedBinding (checkedProgramMainResolvedVar checked) binding
      ]
    reachableKeys = collectReachableCheckedBindingKeys bindingByKey Set.empty roots

collectReachableCheckedBindingKeys :: Map.Map ResolvedTermIdentityKey CheckedBinding -> Set.Set ResolvedTermIdentityKey -> [CheckedBinding] -> Set.Set ResolvedTermIdentityKey
collectReachableCheckedBindingKeys bindingByKey visited pending =
  case pending of
    [] -> visited
    binding : rest
      | Set.member key visited ->
          collectReachableCheckedBindingKeys bindingByKey visited rest
      | otherwise ->
          let deps = checkedBindingDependencyBindings bindingByKey binding
           in collectReachableCheckedBindingKeys bindingByKey (Set.insert key visited) (deps ++ rest)
      where
        key = checkedBindingIdentityKey binding

checkedBindingDependencyBindings :: Map.Map ResolvedTermIdentityKey CheckedBinding -> CheckedBinding -> [CheckedBinding]
checkedBindingDependencyBindings bindingByKey binding =
  [ candidate
    | freeResolved <- freeResolvedTermVars (checkedBindingTerm binding),
      Just candidate <- [Map.lookup (resolvedVarIdentityKey freeResolved) bindingByKey]
  ]

checkedBindingIdentityKey :: CheckedBinding -> ResolvedTermIdentityKey
checkedBindingIdentityKey =
  resolvedVarIdentityKey . checkedBindingResolvedVar

resolvedVarMatchesCheckedBinding :: ResolvedVar -> CheckedBinding -> Bool
resolvedVarMatchesCheckedBinding resolved binding =
  resolvedVarIdentityKey resolved == checkedBindingIdentityKey binding

allCheckedBindings :: CheckedProgram -> [CheckedBinding]
allCheckedBindings checked =
  [ binding
    | checkedModule <- checkedProgramModules checked,
      binding <- checkedModuleBindings checkedModule
  ]

checkedBindingMentionsOpaqueBuiltin :: CheckedBinding -> Bool
checkedBindingMentionsOpaqueBuiltin binding =
  Builtins.srcTypeMentionsOpaqueBuiltin (checkedBindingSourceType binding)
    || any (`Set.member` builtinOpaqueTypeIdentities) (Map.elems (typeViewHeadIdentities (checkedBindingSourceTypeView binding)))

freeResolvedTermVars :: XmlfTerm -> [ResolvedVar]
freeResolvedTermVars =
  go []
  where
    go bound term =
      case term of
        EVarNode resolved ->
          if resolvedVarBoundBy bound resolved
            then []
            else [resolved]
        ELit {} -> []
        ELam resolved body ->
          go (resolved : bound) body
        EApp fun arg -> go bound fun ++ go bound arg
        ELet resolved _ rhs body ->
          go bound rhs ++ go (resolved : bound) body
        ETyAbsRef _ _ body -> go bound body
        ETyInst inner _ -> go bound inner
        ERoll _ body -> go bound body
        EUnroll body -> go bound body

freeResolvedTermPrimitiveIdentities :: XmlfTerm -> Set.Set SymbolIdentity
freeResolvedTermPrimitiveIdentities term =
  Set.fromList
    [ symbol
    | resolved <- freeResolvedTermVars term,
      Just symbol <- [resolvedVarPrimitiveSymbol resolved]
    ]

termMentionsRuntimePurePrimitive :: XmlfTerm -> Bool
termMentionsRuntimePurePrimitive =
  any resolvedVarIsRuntimePurePrimitive . freeResolvedTermVars

resolvedVarIsRuntimePurePrimitive :: ResolvedVar -> Bool
resolvedVarIsRuntimePurePrimitive resolved =
  case resolvedVarPrimitiveSymbol resolved of
    Just symbol ->
      Set.member symbol runtimePurePrimitiveIdentities
    Nothing ->
      False

runtimePurePrimitiveIdentities :: Set.Set SymbolIdentity
runtimePurePrimitiveIdentities =
  Set.fromList (map Builtins.builtinValueIdentity runtimePurePrimitiveNames)

builtinOpaqueValueIdentities :: Set.Set SymbolIdentity
builtinOpaqueValueIdentities =
  Set.fromList (map Builtins.builtinValueIdentity (Set.toList Builtins.builtinOpaqueValueNames))

builtinOpaqueTypeIdentities :: Set.Set SymbolIdentity
builtinOpaqueTypeIdentities =
  Set.fromList (map Builtins.builtinTypeIdentity (Set.toList Builtins.builtinOpaqueTypeNames))

runtimePurePrimitiveNames :: [String]
runtimePurePrimitiveNames =
  [ PrimitiveInventory.stringLengthPrimitiveName,
    PrimitiveInventory.stringIsEmptyPrimitiveName,
    PrimitiveInventory.stringContainsCharPrimitiveName,
    PrimitiveInventory.stringContainsPrimitiveName,
    PrimitiveInventory.stringEqualsPrimitiveName,
    PrimitiveInventory.stringStartsWithPrimitiveName,
    PrimitiveInventory.stringEndsWithPrimitiveName,
    PrimitiveInventory.stringAppendPrimitiveName,
    PrimitiveInventory.stringReplaceCharPrimitiveName,
    PrimitiveInventory.stringReplacePrimitiveName,
    PrimitiveInventory.stringIndexOfCharPrimitiveName,
    PrimitiveInventory.stringIndexOfPrimitiveName,
    PrimitiveInventory.stringSplitPrimitiveName,
    PrimitiveInventory.stringJoinPrimitiveName,
    PrimitiveInventory.stringSplitCharPrimitiveName,
    PrimitiveInventory.stringComparePrimitiveName,
    PrimitiveInventory.stringFromCharPrimitiveName,
    PrimitiveInventory.stringFromIntPrimitiveName,
    PrimitiveInventory.stringFromBoolPrimitiveName,
    PrimitiveInventory.stringFromNatPrimitiveName,
    PrimitiveInventory.stringFromListPrimitiveName,
    PrimitiveInventory.stringToListPrimitiveName,
    PrimitiveInventory.stringDropPrimitiveName,
    PrimitiveInventory.stringTakePrimitiveName,
    PrimitiveInventory.stringSlicePrimitiveName,
    PrimitiveInventory.stringCharAtPrimitiveName,
    PrimitiveInventory.stringCharAtOptionPrimitiveName,
    PrimitiveInventory.charIsDigitPrimitiveName,
    PrimitiveInventory.charIsAsciiLowerPrimitiveName,
    PrimitiveInventory.charIsAsciiUpperPrimitiveName,
    PrimitiveInventory.charIsAsciiAlphaPrimitiveName,
    PrimitiveInventory.charIsAsciiAlphaNumPrimitiveName,
    PrimitiveInventory.charIsAsciiIdentifierStartPrimitiveName,
    PrimitiveInventory.charIsAsciiIdentifierContinuePrimitiveName,
    PrimitiveInventory.charIsAsciiWhitespacePrimitiveName,
    PrimitiveInventory.charIsAsciiPunctuationPrimitiveName,
    PrimitiveInventory.charIsAsciiPrintablePrimitiveName,
    PrimitiveInventory.charIsAsciiHexDigitPrimitiveName,
    PrimitiveInventory.charIsAsciiLineBreakPrimitiveName,
    PrimitiveInventory.charIsAsciiControlPrimitiveName,
    PrimitiveInventory.charToAsciiLowerPrimitiveName,
    PrimitiveInventory.charToAsciiUpperPrimitiveName,
    PrimitiveInventory.stringToAsciiLowerPrimitiveName,
    PrimitiveInventory.stringToAsciiUpperPrimitiveName
  ]

normalizeProgramTerm :: XmlfTerm -> XmlfTerm
normalizeProgramTerm term =
  let termNorm = normalize term
      termSimplified = case termNorm of
        ELet resolved _ rhs (EVarNode bodyResolved)
          | resolvedVarSameIdentity resolved bodyResolved ->
              rhs
        _ -> termNorm
      termUnderTyAbs =
        case termSimplified of
          ETyAbsRef ref mbBound body ->
            let body' = normalizeProgramTerm body
                rebuilt = ETyAbsRef ref mbBound body'
             in case typeCheck rebuilt of
                  Right (TForallRef _ _ bodyTy)
                    | not (typeBinderRefOccursInType ref bodyTy) -> body'
                  _ -> rebuilt
          _ -> termSimplified
      termStripped = stripUnusedTopTyAbs termUnderTyAbs
   in if termStripped == term
        then termStripped
        else normalizeProgramTerm termStripped

stripUnusedTopTyAbs :: XmlfTerm -> XmlfTerm
stripUnusedTopTyAbs term = case term of
  ETyAbsRef ref mbBound body ->
    let body' = stripUnusedTopTyAbs body
        term' = ETyAbsRef ref mbBound body'
     in case typeCheck term' of
          Right (TForallRef _ _ bodyTy)
            | not (typeBinderRefOccursInType ref bodyTy) -> body'
          _ -> term'
  ELam resolved body -> ELam resolved (stripUnusedTopTyAbs body)
  EApp f a -> EApp (stripUnusedTopTyAbs f) (stripUnusedTopTyAbs a)
  ELet resolved sch rhs body ->
    ELet resolved sch (stripUnusedTopTyAbs rhs) (stripUnusedTopTyAbs body)
  ETyInst e inst -> ETyInst (stripUnusedTopTyAbs e) inst
  ERoll ty body -> ERoll ty (stripUnusedTopTyAbs body)
  EUnroll body -> EUnroll (stripUnusedTopTyAbs body)
  _ -> term

typeBinderRefOccursInType :: X.TypeBinderRef -> ElabType -> Bool
typeBinderRefOccursInType ref ty =
  any (X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsType ty)

toValueWithProgram :: RuntimeContext -> CheckedProgram -> XmlfTerm -> Value
toValueWithProgram context checked term =
  case mainBinding checked of
    Just binding ->
      let mbDataInfo = lookupDataInfoForBinding context binding
       in case decodeSourceValueWithDataInfo context (checkedBindingSourceTypeView binding) mbDataInfo term of
            Just value -> value
            Nothing ->
              case mbDataInfo of
                Just {} ->
                  case decodeAnyData context term of
                    Just value -> value
                    Nothing -> toValue term
                Nothing -> toValue term
    Nothing ->
      case decodeAnyData context term of
        Just value -> value
        Nothing -> toValue term

toValue :: XmlfTerm -> Value
toValue term = case stripRuntimeWrappers term of
  ELit lit -> VLit lit
  other -> VTerm other

prettyValue :: Value -> String
prettyValue value = case value of
  VLit (LInt i) -> show i
  VLit (LBool b) -> if b then "true" else "false"
  VLit (LChar c) -> show c
  VLit (LString s) -> show s
  VData ctor [] -> ctor
  VData ctor args -> unwords (ctor : map prettyValueArg args)
  VTerm term -> pretty term

prettyValueArg :: Value -> String
prettyValueArg value = case value of
  VData _ (_ : _) -> "(" ++ prettyValue value ++ ")"
  _ -> prettyValue value

mainBinding :: CheckedProgram -> Maybe CheckedBinding
mainBinding checked =
  case
    [ binding
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        resolvedVarMatchesCheckedBinding (checkedProgramMainResolvedVar checked) binding
    ]
  of
    binding : _ -> Just binding
    [] -> Nothing

recoverMainSourceType :: CheckedProgram -> SrcType -> SrcType
recoverMainSourceType checked ty =
  case ty of
    STArrow {} -> ty
    _ -> recoverSourceType (programElaborateScope checked) ty

programElaborateScope :: CheckedProgram -> ElaborateScope
programElaborateScope checked =
  mkElaborateScope
    Map.empty
    (Map.fromList [(qualifiedDataName info, info) | info <- allDataInfos checked])
    (Map.fromList [(qualifiedClassName info, info) | info <- allClassInfos checked])
    (allInstanceInfos checked)

decodeSourceValueWithDataInfo :: RuntimeContext -> TypeView -> Maybe DataInfo -> XmlfTerm -> Maybe Value
decodeSourceValueWithDataInfo context view mbDataInfo term =
  case mbDataInfo of
    Nothing ->
      case stripRuntimeWrappers term of
        ELit lit -> Just (VLit lit)
        _ -> Nothing
    Just dataInfo ->
      decodeChurchData context view dataInfo (dataTypeSubst dataInfo view) term

lookupDataInfoForBinding :: RuntimeContext -> CheckedBinding -> Maybe DataInfo
lookupDataInfoForBinding context binding =
  lookupDataInfoByElabTypeIdentity context (checkedBindingType binding)
    <|> sourceTypeDataInfo context binding

lookupDataInfoByElabTypeIdentity :: RuntimeContext -> ElabType -> Maybe DataInfo
lookupDataInfoByElabTypeIdentity context ty =
  case ty of
    X.TBaseWithIdentity (Just identity) _ ->
      Map.lookup identity (runtimeDataByIdentity context)
    X.TConWithIdentity (Just identity) _ _ ->
      Map.lookup identity (runtimeDataByIdentity context)
    X.TForallRef _ _ body ->
      lookupDataInfoByElabTypeIdentity context body
    _ ->
      Nothing

sourceTypeDataInfo :: RuntimeContext -> CheckedBinding -> Maybe DataInfo
sourceTypeDataInfo context binding =
  lookupDataInfoForTypeView context (checkedBindingSourceTypeView binding)

lookupDataInfoForTypeView :: RuntimeContext -> TypeView -> Maybe DataInfo
lookupDataInfoForTypeView context view = do
  identity <- sourceTypeDataHeadIdentity view
  Map.lookup identity (runtimeDataByIdentity context)

sourceTypeDataHeadIdentity :: TypeView -> Maybe SymbolIdentity
sourceTypeDataHeadIdentity view =
  (sourceTypeDataHeadName (typeViewIdentity view) >>= typeViewHeadIdentityForAlias view)
    <|> (sourceTypeDataHeadName (typeViewDisplay view) >>= typeViewHeadIdentityForAlias view)

sourceTypeDataHeadName :: SrcType -> Maybe String
sourceTypeDataHeadName =
  \case
    STBase name -> Just name
    STCon name _ -> Just name
    _ -> Nothing

sourceTypeIsDataView :: RuntimeContext -> TypeView -> Bool
sourceTypeIsDataView context view =
  case lookupDataInfoForTypeView context view of
    Just {} -> True
    Nothing -> False

qualifiedDataName :: DataInfo -> String
qualifiedDataName =
  dataInfoIdentityQualifiedName

qualifiedClassName :: ClassInfo -> String
qualifiedClassName =
  classInfoIdentityQualifiedName

allDataInfos :: CheckedProgram -> [DataInfo]
allDataInfos checked =
  [ dataInfo
    | checkedModule <- checkedProgramModules checked,
      dataInfo <- Map.elems (checkedModuleData checkedModule)
  ]

allClassInfos :: CheckedProgram -> [ClassInfo]
allClassInfos checked =
  [ classInfo
    | checkedModule <- checkedProgramModules checked,
      classInfo <- Map.elems (checkedModuleClasses checkedModule)
  ]

allInstanceInfos :: CheckedProgram -> [InstanceInfo]
allInstanceInfos checked =
  concatMap checkedModuleInstances (checkedProgramModules checked)

decodeAnyData :: RuntimeContext -> XmlfTerm -> Maybe Value
decodeAnyData context term =
  case [value | dataInfo <- runtimeDataInfos context, Just value <- [decodeChurchData context emptyView dataInfo emptyTypeBinderSubst term]] of
    [value] -> Just value
    _ -> Nothing
  where
    emptyView =
      TypeView
        { typeViewDisplay = STBottom,
          typeViewIdentity = STBottom,
          typeViewHeadIdentities = Map.empty,
          typeViewBinderIdentities = Map.empty
        }

decodeChurchData :: RuntimeContext -> TypeView -> DataInfo -> TypeBinderSubst -> XmlfTerm -> Maybe Value
decodeChurchData context sourceView dataInfo subst term = do
  let stripped = stripRuntimeWrappers term
      (handlerNames, body) = collectLeadingLams stripped
      constructors = dataConstructors dataInfo
  if length handlerNames < length constructors
    then Nothing
    else do
      let activeHandlers = take (length constructors) handlerNames
          (headTerm, args) = collectElabApps (stripRuntimeWrappers body)
      selectedHandler <- case headTerm of
        EVarNode resolved -> Just resolved
        _ -> Nothing
      ctorInfo <- lookupByHandler activeHandlers constructors selectedHandler
      let ctorArgViews = constructorInfoArgViews ctorInfo
      if length args /= length ctorArgViews
        then Nothing
        else
          let argViews = map (canonicalFieldTypeView context dataInfo . substDataParamView sourceView subst) ctorArgViews
           in Just (VData (ctorName ctorInfo) (zipWith (decodeArg context) argViews args))

dataTypeSubst :: DataInfo -> TypeView -> TypeBinderSubst
dataTypeSubst dataInfo view =
  if sourceTypeDataHeadIdentity view == Just (dataInfoSymbol dataInfo)
    then
      case (dataParamBinders dataInfo, typeViewIdentity view) of
        ([], STBase {}) -> emptyTypeBinderSubst
        (binders, STCon _ args)
          | length binders == length args ->
              foldr (uncurry insertDataParam) emptyTypeBinderSubst (zip binders (toList args))
        _ -> emptyTypeBinderSubst
    else emptyTypeBinderSubst
  where
    insertDataParam (displayName, identity) ty =
      insertTypeBinderSubstWithIdentity identity displayName ty

constructorInfoArgViews :: ConstructorInfo -> [TypeView]
constructorInfoArgViews ctorInfo =
  zipWith argView displayArgs identityArgs
  where
    view = ctorTypeView ctorInfo
    (_, displayBody) = splitForalls (typeViewDisplay view)
    (_, identityBody) = splitForalls (typeViewIdentity view)
    (displayArgs, _) = splitArrows displayBody
    (identityArgs, _) = splitArrows identityBody

    argView displayTy identityTy =
      TypeView
        { typeViewDisplay = displayTy,
          typeViewIdentity = identityTy,
          typeViewHeadIdentities =
            filterHeadIdentitiesByNames
              (typeHeadNamesSrcType identityTy <> typeHeadNamesSrcType displayTy)
              (typeViewHeadIdentities view),
          typeViewBinderIdentities = typeViewBinderIdentities view
        }

constructorInfoResultView :: ConstructorInfo -> TypeView
constructorInfoResultView ctorInfo =
  view
    { typeViewDisplay = displayResult,
      typeViewIdentity = identityResult,
      typeViewHeadIdentities =
        filterHeadIdentitiesByNames
          (typeHeadNamesSrcType identityResult <> typeHeadNamesSrcType displayResult)
          (typeViewHeadIdentities view)
    }
  where
    view = ctorTypeView ctorInfo
    (_, displayBody) = splitForalls (typeViewDisplay view)
    (_, identityBody) = splitForalls (typeViewIdentity view)
    (_, displayResult) = splitArrows displayBody
    (_, identityResult) = splitArrows identityBody

substDataParamView :: TypeView -> TypeBinderSubst -> TypeView -> TypeView
substDataParamView sourceView subst view =
  substitutedView
    { typeViewIdentity = identityTy,
      typeViewHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities substitutedView,
            sourceHeadIdentitiesFor identityHeadNames
          ]
    }
  where
    substitutedView =
      applyTypeViewSubst (typeBinderSubstToTypeViewSubstWith substView subst) view

    substView ty =
      TypeView
        { typeViewDisplay = displayTypeFromRuntimeHeadPairs sourceHeadPairs ty,
          typeViewIdentity = ty,
          typeViewHeadIdentities =
            sourceHeadIdentitiesFor (typeHeadNamesSrcType ty),
          typeViewBinderIdentities =
            sourceBinderIdentitiesFor (freeTypeVarsRuntimeSrcType ty)
        }

    identityTy = typeViewIdentity substitutedView
    identityHeadNames = typeHeadNamesSrcType identityTy
    sourceHeadIdentitiesFor names =
      filterHeadIdentitiesByNames
        (names <> pairedSourceHeadNames names)
        sourceHeadIdentities
    pairedSourceHeadNames names =
      Set.fromList
        [ displayName
        | identityName <- Set.toList names,
          Just displayName <- [Map.lookup identityName sourceHeadPairs]
        ]
    sourceHeadPairs =
      typeViewHeadPairs sourceView
    sourceHeadIdentities = typeViewHeadIdentities sourceView
    sourceBinderIdentitiesFor names =
      typeBinderAliasIdentityMap
        [ (name, identity)
        | name <- Set.toList (names <> pairedSourceBinderNames names),
          Just identity <- [typeViewBinderIdentityForAlias sourceView name]
        ]
    pairedSourceBinderNames names =
      Set.fromList
        [ displayName
        | identityName <- Set.toList names,
          Just displayName <- [Map.lookup identityName sourceBinderPairs]
        ]
    sourceBinderPairs =
      typeViewVarPairs sourceView

displayTypeFromRuntimeHeadPairs :: Map.Map String String -> SrcType -> SrcType
displayTypeFromRuntimeHeadPairs pairs =
  go
  where
    displayHead name =
      Map.findWithDefault name name pairs

    go ty =
      case ty of
        STVar {} -> ty
        STBase name -> STBase (displayHead name)
        STCon name args -> STCon (displayHead name) (fmap go args)
        STVarApp name args -> STVarApp name (fmap go args)
        STTyLam name body -> STTyLam name (go body)
        STTyApp fun arg -> STTyApp (go fun) (go arg)
        STArrow dom cod -> STArrow (go dom) (go cod)
        STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu name body -> STMu name (go body)
        STBottom -> STBottom

canonicalFieldTypeView :: RuntimeContext -> DataInfo -> TypeView -> TypeView
canonicalFieldTypeView context _ownerInfo view =
  view
    { typeViewIdentity = identityTy,
      typeViewHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities view,
            canonicalHeadIdentities
          ]
    }
  where
    (identityTy, canonicalHeadIdentities) = canonical (typeViewIdentity view)

    canonical ty =
      case ty of
        STVar {} -> (ty, Map.empty)
        STBase name ->
          case lookupDataInfoByViewHead name of
            Just info ->
              let canonicalName = qualifiedDataName info
               in (STBase canonicalName, Map.singleton canonicalName (dataInfoSymbol info))
            Nothing -> (ty, Map.empty)
        STCon name args ->
          let (args', argIdentities) = canonicalArgs args
           in case lookupDataInfoByViewHead name of
                Just info ->
                  let canonicalName = qualifiedDataName info
                   in (STCon canonicalName args', Map.insert canonicalName (dataInfoSymbol info) argIdentities)
                Nothing -> (STCon name args', argIdentities)
        STVarApp name args ->
          let (args', identities) = canonicalArgs args
           in (STVarApp name args', identities)
        STTyLam name body ->
          let (body', identities) = canonical body
           in (STTyLam name body', identities)
        STTyApp fun arg ->
          let (fun', funIdentities) = canonical fun
              (arg', argIdentities) = canonical arg
           in (STTyApp fun' arg', mergeSymbolIdentityMaps [funIdentities, argIdentities])
        STArrow dom cod ->
          let (dom', domIdentities) = canonical dom
              (cod', codIdentities) = canonical cod
           in (STArrow dom' cod', mergeSymbolIdentityMaps [domIdentities, codIdentities])
        STForall name mb body ->
          let (mb', mbIdentities) =
                case mb of
                  Just (SrcBound bound) ->
                    let (bound', boundIdentities) = canonical bound
                     in (Just (SrcBound bound'), boundIdentities)
                  Nothing -> (Nothing, Map.empty)
              (body', bodyIdentities) = canonical body
           in (STForall name mb' body', mergeSymbolIdentityMaps [mbIdentities, bodyIdentities])
        STMu name body ->
          let (body', identities) = canonical body
           in (STMu name body', identities)
        STBottom -> (STBottom, Map.empty)

    canonicalArgs (arg NE.:| args) =
      let (arg', argIdentities) = canonical arg
          (argsRev, identities) =
            foldl
              ( \(accArgs, accIdentities) next ->
                  let (next', nextIdentities) = canonical next
                   in (next' : accArgs, mergeSymbolIdentityMaps [accIdentities, nextIdentities])
              )
              ([], argIdentities)
              args
       in (arg' NE.:| reverse argsRev, identities)

    lookupDataInfoByViewHead name =
      case typeViewHeadIdentityForAlias view name >>= (`Map.lookup` runtimeDataByIdentity context) of
        Just info -> Just info
        Nothing -> Nothing

decodeArg :: RuntimeContext -> TypeView -> XmlfTerm -> Value
decodeArg context view term =
  case decodeSourceValueWithDataInfo context view (lookupDataInfoForTypeView context view) term of
    Just value -> value
    Nothing
      | sourceTypeIsDataView context view ->
          case decodeAnyData context term of
            Just value -> value
            Nothing -> toValue term
      | otherwise -> toValue term

lookupByHandler :: [ResolvedVar] -> [ConstructorInfo] -> ResolvedVar -> Maybe ConstructorInfo
lookupByHandler handlerNames constructors selected =
  case [ctor | (handlerName, ctor) <- zip handlerNames constructors, resolvedVarSameIdentity handlerName selected] of
    [ctor] -> Just ctor
    _ -> Nothing

collectLeadingLams :: XmlfTerm -> ([ResolvedVar], XmlfTerm)
collectLeadingLams = go []
  where
    go acc term =
      case stripRuntimeWrappers term of
        ELam resolved body -> go (acc ++ [resolved]) body
        other -> (acc, other)

collectElabApps :: XmlfTerm -> (XmlfTerm, [XmlfTerm])
collectElabApps = go []
  where
    go acc term =
      case stripRuntimeWrappers term of
        EApp fun arg -> go (stripRuntimeWrappers arg : acc) fun
        other -> (other, acc)

stripRuntimeWrappers :: XmlfTerm -> XmlfTerm
stripRuntimeWrappers term =
  case term of
    ETyAbsRef _ _ body -> stripRuntimeWrappers body
    ETyInst inner _ -> stripRuntimeWrappers inner
    ERoll _ body -> stripRuntimeWrappers body
    EUnroll body -> stripRuntimeWrappers body
    _ -> term
